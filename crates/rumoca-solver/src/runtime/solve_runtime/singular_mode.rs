//! Naming a singular active mode (EX004).
//!
//! The structural matching of an algebraic block holds over the union of every
//! branch of its runtime `if` equations. A branch combination can still leave
//! unknowns of the block undetermined: two closed tank ports both state
//! `m_flow = 0` and nothing fixes the pressure of the pipe between them. The
//! combination is a fact of the run, not of construction (it may be unreachable
//! in another model), so the runtime reports it when it reaches it.

use super::*;

impl ScalarExecutionView<'_> {
    /// The unknowns of `block` and the branch combination its rows select at
    /// parameters `p`: every discrete coordinate and relation memory a row of
    /// the block reads, with its current value, and the rows that select a
    /// branch. `None` for a block whose rows select no branch.
    pub(crate) fn singular_active_mode(
        &self,
        block: &solve::AlgebraicProjectionBlock,
        p: &[f64],
    ) -> Option<(String, String)> {
        let unknowns = block
            .y_indices
            .iter()
            .map(|&index| {
                self.model
                    .problem
                    .solver_name(index)
                    .unwrap_or_else(|| format!("y[{index}]"))
            })
            .collect::<Vec<_>>()
            .join(", ");
        let mode_slots = self.mode_defining_parameter_slots();
        let source = self.scalar.implicit_scalar_rhs.source();
        let mut read = BTreeSet::new();
        let mut conditional = BTreeSet::new();
        for program in 0..source.row_count() {
            let outputs = source.row_output_count(program)?;
            let in_block = (0..outputs)
                .filter_map(|offset| source.row_output_index(program, offset))
                .filter(|row| block.rows.contains(row))
                .collect::<Vec<_>>();
            if in_block.is_empty() {
                continue;
            }
            if source.program_selects_branch(program)? {
                conditional.extend(in_block);
            }
            read.extend(
                source
                    .mode_parameter_reads(program)?
                    .into_iter()
                    .filter(|index| mode_slots.contains_key(index)),
            );
        }
        // A block whose rows select no branch has one mode; its singular
        // sensitivity (an infinite slope such as `root^3 = x` at the origin) is
        // not a branch combination and keeps its own report.
        if read.is_empty() && conditional.is_empty() {
            return None;
        }
        let selectors = read
            .into_iter()
            .map(|index| {
                let value = p.get(index).copied().unwrap_or(f64::NAN);
                format!("{} = {}", mode_slots[&index], branch_value(value))
            })
            .collect::<Vec<_>>();
        // A branch condition evaluated inline from continuous coordinates
        // names no stored selector; the rows that select are named instead.
        let conditional = conditional
            .into_iter()
            .map(|row| self.row_target_name(row))
            .collect::<Vec<_>>();
        let mode = format!(
            "{{selectors: [{}]; conditional rows solving: [{}]}}",
            selectors.join(", "),
            conditional.join(", ")
        );
        Some((format!("[{unknowns}]"), mode))
    }

    /// The unknown a row of the implicit system is matched to, by name.
    pub(super) fn row_target_name(&self, row: usize) -> String {
        match self
            .model
            .problem
            .continuous
            .implicit_row_targets
            .target_at(row)
        {
            Some(solve::ScalarSlot::Y { index, .. }) => self
                .model
                .problem
                .solver_name(index)
                .unwrap_or_else(|| format!("y[{index}]")),
            _ => format!("row {row}"),
        }
    }

    /// Parameter-storage slots whose values select branches: discrete update
    /// and runtime-assignment targets, and relation memories, by name.
    fn mode_defining_parameter_slots(&self) -> BTreeMap<usize, String> {
        let problem = &self.model.problem;
        let mut slots = BTreeMap::new();
        let discrete = problem
            .discrete
            .update_targets
            .iter()
            .chain(&problem.discrete.runtime_assignment_targets);
        for slot in discrete {
            if let solve::ScalarSlot::P { index, .. } = slot {
                let name = problem
                    .layout
                    .scalar_name_for_slot(*slot)
                    .unwrap_or_else(|| format!("p[{index}]"));
                slots.insert(*index, name);
            }
        }
        for (root, slot) in problem
            .events
            .root_relation_memory_targets
            .iter()
            .enumerate()
        {
            if let Some(solve::ScalarSlot::P { index, .. }) = slot {
                slots
                    .entry(*index)
                    .or_insert_with(|| format!("relation of root {root}"));
            }
        }
        slots
    }
}

fn branch_value(value: f64) -> String {
    if value == 0.0 {
        "false".to_string()
    } else if value == 1.0 {
        "true".to_string()
    } else {
        value.to_string()
    }
}
