//! Naming a singular active mode (EX004).
//!
//! The structural matching of an algebraic block holds over the union of every
//! branch of its runtime `if` equations. A branch combination can still leave
//! unknowns of the block undetermined: two closed tank ports both state
//! `m_flow = 0` and nothing fixes the pressure of the pipe between them. The
//! combination is a fact of the run, not of construction (it may be unreachable
//! in another model), so the runtime reports it when it reaches it.

use super::*;

impl SolveRuntime {
    /// The unknowns of `block` and the branch combination its rows select at
    /// parameters `p`: every discrete coordinate and relation memory a row of
    /// the block reads, with its current value, and the rows that select a
    /// branch. `None` for a block whose rows select no branch.
    pub(crate) fn singular_active_mode(
        &self,
        block: &solve::AlgebraicProjectionBlock,
        p: &[f64],
    ) -> Option<(String, String)> {
        let solver_names = &self.model.problem.solve_layout.solver_maps.names;
        let unknowns = block
            .y_indices
            .iter()
            .map(|&index| {
                solver_names
                    .get(index)
                    .map_or_else(|| format!("y[{index}]"), Clone::clone)
            })
            .collect::<Vec<_>>()
            .join(", ");
        let mode_slots = self.mode_defining_parameter_slots();
        let source = self.implicit_scalar_rhs.block();
        let mut read = BTreeSet::new();
        let mut conditional = BTreeSet::new();
        let mut output = 0;
        for program in source.programs() {
            let outputs = solve::ScalarProgramBlock::program_output_count(program);
            let rows = &source.output_indices()[output..output + outputs];
            output += outputs;
            let in_block = rows
                .iter()
                .filter(|row| block.rows.contains(row))
                .copied()
                .collect::<Vec<_>>();
            if in_block.is_empty() {
                continue;
            }
            if selects_branch(program) {
                conditional.extend(in_block);
            }
            let mut reads = Vec::new();
            collect_parameter_reads(program, &mut |index| reads.push(index));
            read.extend(
                reads
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
    fn row_target_name(&self, row: usize) -> String {
        match self
            .model
            .problem
            .continuous
            .implicit_row_targets
            .get(row)
            .copied()
            .flatten()
        {
            Some(solve::ScalarSlot::Y { index, .. }) => self
                .model
                .problem
                .solve_layout
                .solver_maps
                .names
                .get(index)
                .map_or_else(|| format!("y[{index}]"), Clone::clone),
            _ => format!("row {row}"),
        }
    }

    /// Parameter-storage slots whose values select branches: discrete update
    /// and runtime-assignment targets, and relation memories, by name.
    fn mode_defining_parameter_slots(&self) -> BTreeMap<usize, String> {
        let problem = &self.model.problem;
        let mut names = BTreeMap::new();
        for (name, slot) in problem.layout.bindings() {
            if let solve::ScalarSlot::P { index, .. } = slot {
                names
                    .entry(*index)
                    .or_insert_with(|| name.as_str().to_string());
            }
        }
        let mut slots = BTreeMap::new();
        let discrete = problem
            .discrete
            .update_targets
            .iter()
            .chain(&problem.discrete.runtime_assignment_targets);
        for slot in discrete {
            if let solve::ScalarSlot::P { index, .. } = slot {
                let name = names
                    .get(index)
                    .cloned()
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

/// Whether `program` selects between branches at run time.
fn selects_branch(program: &[solve::LinearOp]) -> bool {
    program.iter().any(|op| {
        matches!(
            op,
            solve::LinearOp::Select { .. } | solve::LinearOp::FunctionConditional { .. }
        )
    })
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

/// Every parameter-storage slot `program` loads.
fn collect_parameter_reads(program: &[solve::LinearOp], visit: &mut dyn FnMut(usize)) {
    for op in program {
        match op {
            solve::LinearOp::LoadP { index, .. } => visit(*index),
            solve::LinearOp::LoadIndexedP { base, count, .. } => {
                (*base..*base + *count).for_each(&mut *visit)
            }
            solve::LinearOp::TensorLoad {
                input: solve::TensorInputKind::P,
                input_start,
                count,
                ..
            } => (*input_start..*input_start + *count).for_each(&mut *visit),
            solve::LinearOp::FunctionConditional { program, .. } => {
                for arm in &program.arms {
                    collect_parameter_reads(&arm.condition, visit);
                    collect_parameter_reads(&arm.result, visit);
                }
                collect_parameter_reads(&program.fallback, visit);
            }
            _ => {}
        }
    }
}
