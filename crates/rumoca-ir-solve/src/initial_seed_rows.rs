//! Discrete equation rows that initialization seeds before its first algebraic
//! refresh.
//!
//! MLS 3.7 §8.6: every equation holds at the initial instant. A discrete
//! equation whose value the initial parameters and inputs fix (`k2 = k`) must
//! therefore hold before a continuous row reads its target (`y = p[k2]`);
//! otherwise that row sees the declaration default. Solve IR issues the set
//! here, in dependency order, so the solver and the generated C apply the same
//! rows before the first algebraic settle. A row that reads a continuous
//! coordinate, a `pre` or relation memory, or a discrete value no seeded row
//! defines keeps its ordinary place in the event iteration.
use std::collections::BTreeSet;

use serde::Serialize;

use crate::{
    DiscreteRowRole, LinearOp, PeriodicClockId, PreParamBinding, PreParamSource,
    ScalarProgramBlock, ScalarSlot, SolveProblem, SolveVariableStorageRole,
    SolveVariableStorageRun,
};

/// One discrete row the initialization seeds, with the program and output that
/// produce it, in the order the rows must be applied.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub struct InitialSeedRow {
    pub row: usize,
    pub program: usize,
    pub output: usize,
    /// The row's parameter target.
    pub target: usize,
    /// The `pre` slot of the target, which initialization keeps at the
    /// declaration start: the initial event pass publishes the row.
    pub pre_slot: Option<usize>,
}

impl SolveProblem {
    /// Unclocked discrete equation rows whose programs read only time and the
    /// parameter, constant and input slots, or the targets of rows seeded before
    /// them, through plain scalar operations.
    pub fn initial_seed_discrete_rows(&self) -> Vec<InitialSeedRow> {
        let discrete = &self.discrete;
        initial_seed_rows(
            &discrete.rhs,
            &discrete.update_targets,
            &discrete.row_roles,
            &discrete.clock_owners,
            &fixed_parameter_slots(
                &self.solve_layout.variable_storage_runs,
                self.initialization
                    .update_targets()
                    .iter()
                    .chain(self.initialization.projection_unknowns())
                    .chain(self.initialization.row_targets().iter().flatten()),
            ),
            &self.solve_layout.pre_param_bindings,
        )
    }
}

/// The parameter slots whose values are known before initialization runs:
/// declared parameters, constants and external inputs, less any slot the
/// initialization system itself solves or updates (a `fixed = false`
/// parameter, or one with an initialization update). `pre` slots, relation
/// and condition memories and discrete values have other storage roles.
fn fixed_parameter_slots<'a>(
    runs: &[SolveVariableStorageRun],
    solved: impl Iterator<Item = &'a ScalarSlot>,
) -> BTreeSet<usize> {
    let mut fixed = BTreeSet::new();
    for run in runs {
        let ScalarSlot::P { index, .. } = run.base else {
            continue;
        };
        if matches!(
            run.role,
            SolveVariableStorageRole::Parameter
                | SolveVariableStorageRole::Constant
                | SolveVariableStorageRole::ExternalInput
        ) {
            fixed.extend(index..index.saturating_add(run.scalar_count));
        }
    }
    for slot in solved {
        if let ScalarSlot::P { index, .. } = slot {
            fixed.remove(index);
        }
    }
    fixed
}

/// The parameter slots a program reads, or `None` when it reads anything else
/// (a continuous coordinate, a fold, a call, a table) or uses an operation other
/// than plain scalar arithmetic.
fn parameter_reads(program: &[LinearOp]) -> Option<BTreeSet<usize>> {
    let mut reads = BTreeSet::new();
    for op in program {
        match op {
            LinearOp::LoadP { index, .. } => {
                reads.insert(*index);
            }
            LinearOp::LoadIndexedP { base, count, .. } => {
                reads.extend(*base..base.saturating_add(*count));
            }
            LinearOp::Const { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::Move { .. }
            | LinearOp::Unary { .. }
            | LinearOp::Binary { .. }
            | LinearOp::Compare { .. }
            | LinearOp::Select { .. }
            | LinearOp::StoreOutput { .. } => {}
            _ => return None,
        }
    }
    Some(reads)
}

fn initial_seed_rows(
    rhs: &ScalarProgramBlock,
    targets: &[ScalarSlot],
    roles: &[DiscreteRowRole],
    clock_owners: &[Option<PeriodicClockId>],
    fixed: &BTreeSet<usize>,
    pre_bindings: &[PreParamBinding],
) -> Vec<InitialSeedRow> {
    struct Candidate {
        row: usize,
        program: usize,
        output: usize,
        target: usize,
        reads: BTreeSet<usize>,
    }
    let mut candidates = Vec::new();
    for (row, target) in targets.iter().enumerate() {
        let ScalarSlot::P { index: target, .. } = *target else {
            continue;
        };
        if !matches!(roles.get(row), Some(DiscreteRowRole::Equation))
            || !matches!(clock_owners.get(row), Some(None))
        {
            continue;
        }
        let Some((program, output)) = rhs.output_position(row) else {
            continue;
        };
        let Some(reads) = rhs.program(program).and_then(parameter_reads) else {
            continue;
        };
        if reads.contains(&target) {
            continue;
        }
        candidates.push(Candidate {
            row,
            program,
            output,
            target,
            reads,
        });
    }
    // A row joins once every slot it reads is fixed or seeded by an earlier
    // row, so the order of admission is a dependency order; a cycle never
    // joins and stays in the event iteration.
    let mut known = fixed.clone();
    let mut seeded = Vec::new();
    loop {
        let mut admitted = false;
        candidates.retain(|candidate| {
            if !candidate.reads.is_subset(&known) {
                return true;
            }
            seeded.push(InitialSeedRow {
                row: candidate.row,
                program: candidate.program,
                output: candidate.output,
                target: candidate.target,
                pre_slot: pre_bindings
                    .iter()
                    .find(|binding| {
                        matches!(binding.source, PreParamSource::P { index } if index == candidate.target)
                    })
                    .map(|binding| binding.dest_p_index),
            });
            admitted |= known.insert(candidate.target);
            false
        });
        if !admitted {
            return seeded;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{BinaryOp, scalar_slot_p, scalar_slot_y};

    fn block(programs: Vec<Vec<LinearOp>>) -> ScalarProgramBlock {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("initial_seed_rows_test.mo"),
            0,
            1,
        );
        let provenance = rumoca_core::ProvenanceSpan::new(span, "initial seed rows test").unwrap();
        ScalarProgramBlock::with_source_span(programs, provenance).unwrap()
    }

    fn copy_p(index: usize) -> Vec<LinearOp> {
        vec![
            LinearOp::LoadP { dst: 0, index },
            LinearOp::StoreOutput { src: 0 },
        ]
    }

    fn clock(index: u32) -> PeriodicClockId {
        serde_json::from_value(serde_json::json!(index)).unwrap()
    }

    fn run(index: usize, count: usize, role: SolveVariableStorageRole) -> SolveVariableStorageRun {
        SolveVariableStorageRun {
            base: scalar_slot_p(index),
            scalar_count: count,
            role,
            value_kind: crate::SolveVariableValueKind::Real,
        }
    }

    #[test]
    fn only_declared_parameters_constants_and_inputs_are_fixed_slots() {
        use SolveVariableStorageRole as Role;
        let runs = [
            run(0, 2, Role::Parameter),
            run(2, 1, Role::Constant),
            run(3, 1, Role::ExternalInput),
            run(4, 1, Role::DiscreteValue),
            run(5, 1, Role::DiscreteReal),
            SolveVariableStorageRun {
                base: scalar_slot_y(0),
                scalar_count: 1,
                role: Role::State,
                value_kind: crate::SolveVariableValueKind::Real,
            },
        ];
        // slot 6 would be a pre or memory slot: it belongs to no declared run
        assert_eq!(
            fixed_parameter_slots(&runs, [].iter()),
            BTreeSet::from([0, 1, 2, 3])
        );
    }

    #[test]
    fn a_parameter_the_initialization_system_solves_is_not_a_fixed_slot() {
        let runs = [run(0, 2, SolveVariableStorageRole::Parameter)];
        let solved = [scalar_slot_p(1), scalar_slot_y(0)];
        assert_eq!(
            fixed_parameter_slots(&runs, solved.iter()),
            BTreeSet::from([0])
        );
    }

    fn rows(seeded: &[InitialSeedRow]) -> Vec<usize> {
        seeded.iter().map(|seed| seed.row).collect()
    }

    #[test]
    fn a_row_reading_only_fixed_slots_is_seeded() {
        let rhs = block(vec![copy_p(0)]);
        let seeded = initial_seed_rows(
            &rhs,
            &[scalar_slot_p(1)],
            &[DiscreteRowRole::Equation],
            &[None],
            &BTreeSet::from([0]),
            &[],
        );
        assert_eq!(
            seeded,
            vec![InitialSeedRow {
                row: 0,
                program: 0,
                output: 0,
                target: 1,
                pre_slot: None,
            }]
        );
    }

    #[test]
    fn a_chain_of_discrete_aliases_is_seeded_in_dependency_order() {
        // row 0: p[2] = p[1] + 1 reads row 1's target; row 1: p[1] = p[0]
        let rhs = block(vec![
            vec![
                LinearOp::LoadP { dst: 0, index: 1 },
                LinearOp::Const { dst: 1, value: 1.0 },
                LinearOp::Binary {
                    dst: 2,
                    op: BinaryOp::Add,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ],
            copy_p(0),
        ]);
        let seeded = initial_seed_rows(
            &rhs,
            &[scalar_slot_p(2), scalar_slot_p(1)],
            &[DiscreteRowRole::Equation, DiscreteRowRole::Equation],
            &[None, None],
            &BTreeSet::from([0]),
            &[],
        );
        assert_eq!(rows(&seeded), vec![1, 0]);
    }

    #[test]
    fn rows_reading_a_coordinate_an_unseeded_slot_a_cycle_or_under_a_clock_stay_in_the_event_pass()
    {
        let rhs = block(vec![
            vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            copy_p(7),
            copy_p(4),
            copy_p(3),
            copy_p(0),
        ]);
        let seeded = initial_seed_rows(
            &rhs,
            &[
                scalar_slot_p(1),
                scalar_slot_p(2),
                scalar_slot_p(3),
                scalar_slot_p(4),
                scalar_slot_p(5),
            ],
            &[DiscreteRowRole::Equation; 5],
            &[None, None, None, None, Some(clock(0))],
            &BTreeSet::from([0]),
            &[],
        );
        assert!(seeded.is_empty(), "{seeded:?}");
    }

    #[test]
    fn a_row_writing_a_coordinate_or_reading_its_own_target_is_not_seeded() {
        let rhs = block(vec![copy_p(0), copy_p(1)]);
        let seeded = initial_seed_rows(
            &rhs,
            &[scalar_slot_y(0), scalar_slot_p(1)],
            &[DiscreteRowRole::Equation, DiscreteRowRole::Equation],
            &[None, None],
            &BTreeSet::from([0]),
            &[],
        );
        assert!(seeded.is_empty(), "{seeded:?}");
    }
}
