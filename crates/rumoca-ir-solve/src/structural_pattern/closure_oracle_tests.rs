//! The algebraic closure on guarded multi-output programs: what it
//! materialises, and that its answers are the scalar closure's.

use std::collections::BTreeSet;

use rumoca_core::{SourceId, Span};

use super::state_jacobian::take_closure_work;
use super::{
    KinematicStates, NonKinematicReason, PatternDerivation, PatternProvenance, StructuralPattern,
};
use crate::{
    AlgebraicProjectionBlock, AlgebraicProjectionPlan, BinaryOp, CompareOp, LinearOp, Reg,
    ScalarProgramBlock,
};

fn block(programs: Vec<Vec<LinearOp>>) -> ScalarProgramBlock {
    let spans = vec![
        Span::from_offsets(SourceId::from_source_name("closure_oracle.mo"), 0, 1);
        programs.len()
    ];
    ScalarProgramBlock::with_program_spans(programs, spans).expect("valid test programs")
}

fn reg(index: usize) -> Reg {
    Reg::try_from(index).expect("test register fits")
}

/// One program with an output per cell `k`: `y[unknown + k] - f(y[read + k])`,
/// where the checked call `f` guards that its input is finite. The program
/// executes every cell's guard, so each output's execution reads every read
/// cell, as one call over a whole tensor does.
fn guarded_cells(cells: usize, read: usize, unknown: usize) -> Vec<LinearOp> {
    let site = super::execution_reads_tests::guarded_site();
    (0..cells)
        .flat_map(|cell| {
            let base = 4 * cell;
            [
                LinearOp::LoadY {
                    dst: reg(base),
                    index: read + cell,
                },
                LinearOp::PureCall {
                    dst_start: reg(base + 1),
                    input_starts: vec![reg(base), reg(base)].into(),
                    site: site.clone(),
                },
                LinearOp::LoadY {
                    dst: reg(base + 2),
                    index: unknown + cell,
                },
                LinearOp::Binary {
                    dst: reg(base + 3),
                    op: BinaryOp::Sub,
                    lhs: reg(base + 2),
                    rhs: reg(base + 1),
                },
                LinearOp::StoreOutput { src: reg(base + 3) },
            ]
        })
        .collect()
}

fn copy(index: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index },
        LinearOp::StoreOutput { src: 0 },
    ]
}

/// `y[index] > 0`, a relation that seeds the non-kinematic set.
fn relation(index: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index },
        LinearOp::Const { dst: 1, value: 0.0 },
        LinearOp::Compare {
            dst: 2,
            op: CompareOp::Gt,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

/// The derivative rows, the implicit rows, the projection solving them, and
/// the state and solver counts.
pub(super) struct System {
    pub(super) derivative: ScalarProgramBlock,
    pub(super) implicit: ScalarProgramBlock,
    pub(super) plan: AlgebraicProjectionPlan,
    pub(super) states: usize,
    pub(super) solver: usize,
}

impl System {
    pub(super) fn partition(&self) -> KinematicStates {
        KinematicStates::derive(
            &self.derivative,
            &self.implicit,
            &self.plan,
            self.states,
            self.solver,
        )
    }
}

fn singletons(
    rows_and_unknowns: impl IntoIterator<Item = (usize, usize)>,
) -> Vec<AlgebraicProjectionBlock> {
    rows_and_unknowns
        .into_iter()
        .map(|(row, unknown)| AlgebraicProjectionBlock {
            rows: vec![row],
            y_indices: vec![unknown],
            ..AlgebraicProjectionBlock::default()
        })
        .collect()
}

/// `n` states, `a = f(x)` and `b = f(a)` solved cell by cell from one guarded
/// program each, and `der(x[k]) = b[k]`, except that the derivative of state
/// 0 evaluates a relation on `b[0]`.
pub(super) fn guarded_chain(n: usize) -> System {
    let derivative = std::iter::once(relation(2 * n))
        .chain((1..n).map(|k| copy(2 * n + k)))
        .collect();
    let blocks = singletons(
        (0..n)
            .map(|k| (k, n + k))
            .chain((0..n).map(|k| (n + k, 2 * n + k))),
    );
    System {
        derivative: block(derivative),
        implicit: block(vec![guarded_cells(n, 0, n), guarded_cells(n, n, 2 * n)]),
        plan: AlgebraicProjectionPlan {
            blocks: blocks.into(),
        },
        states: n,
        solver: 3 * n,
    }
}

/// The closure materialises intervals, not states: a guarded program over
/// `n` cells whose every output reads every cell costs a bounded number of
/// entries per row, and the guard still carries state 0's relation to every
/// state.
#[test]
fn a_guarded_multi_output_program_closes_over_intervals_not_states() {
    let n = 24;
    let system = guarded_chain(n);
    take_closure_work();
    let partition = system.partition();
    let work = take_closure_work();
    let rows = system.implicit.output_count();
    eprintln!("closure materialised {work} entries for {rows} rows");
    assert!(
        work <= 8 * rows,
        "the closure materialised {work} entries for {rows} rows"
    );
    assert!(partition.states().is_empty());
    assert_eq!(partition.reason(0), Some(NonKinematicReason::Relation));
    for state in 1..n {
        assert_eq!(
            partition.reason(state),
            Some(NonKinematicReason::Closure { through: 0 }),
            "state {state}"
        );
    }
}

/// [`guarded_chain`] with its blocks listed consumers first, so the closure
/// needs more than one pass to settle.
fn guarded_chain_consumers_first(n: usize) -> System {
    let mut system = guarded_chain(n);
    let mut blocks = system.plan.blocks.scalar_blocks().to_vec();
    blocks.rotate_left(n);
    system.plan = AlgebraicProjectionPlan {
        blocks: blocks.into(),
    };
    system
}

/// `z0 - z1 - x0` and `z1 - z0*x1` solved together (a cycle), `w` solved
/// from a guarded call on `z0` whose guard also reads `z1`, and
/// `der(x0) = w`, `der(x1) = x2`, `der(x2) = z0`.
fn coupled_cycle() -> System {
    let site = super::execution_reads_tests::guarded_site();
    let coupled = vec![
        LinearOp::LoadY { dst: 0, index: 3 },
        LinearOp::LoadY { dst: 1, index: 4 },
        LinearOp::LoadY { dst: 2, index: 0 },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 3,
            rhs: 2,
        },
        LinearOp::StoreOutput { src: 4 },
        LinearOp::LoadY { dst: 5, index: 1 },
        LinearOp::Binary {
            dst: 6,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 5,
        },
        LinearOp::Binary {
            dst: 7,
            op: BinaryOp::Sub,
            lhs: 1,
            rhs: 6,
        },
        LinearOp::StoreOutput { src: 7 },
    ];
    let guarded = vec![
        LinearOp::LoadY { dst: 0, index: 3 },
        LinearOp::LoadY { dst: 1, index: 4 },
        LinearOp::PureCall {
            dst_start: 2,
            input_starts: vec![0, 1].into(),
            site,
        },
        LinearOp::LoadY { dst: 3, index: 5 },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 3,
            rhs: 2,
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    System {
        derivative: block(vec![copy(5), copy(2), copy(3)]),
        implicit: block(vec![coupled, guarded]),
        plan: AlgebraicProjectionPlan {
            blocks: vec![
                AlgebraicProjectionBlock {
                    rows: vec![0, 1],
                    y_indices: vec![3, 4],
                    ..AlgebraicProjectionBlock::default()
                },
                AlgebraicProjectionBlock {
                    rows: vec![2],
                    y_indices: vec![5],
                    ..AlgebraicProjectionBlock::default()
                },
            ]
            .into(),
        },
        states: 3,
        solver: 6,
    }
}

/// The fixtures every comparison with the scalar oracle runs on.
pub(super) fn oracle_fixtures() -> Vec<(&'static str, System)> {
    vec![
        ("guarded chain", guarded_chain(6)),
        (
            "guarded chain, consumers first",
            guarded_chain_consumers_first(6),
        ),
        ("coupled cycle", coupled_cycle()),
    ]
}

/// The execution reads of every output of `programs`, in output order.
pub(super) fn execution_rows(programs: &ScalarProgramBlock) -> Vec<Vec<usize>> {
    programs
        .programs()
        .iter()
        .flat_map(|program| {
            StructuralPattern::derive_output_execution_y_dependency_ranges(program, None)
                .expect("fixture reads are derivable")
        })
        .map(|columns| columns.iter().collect())
        .collect()
}

/// The closure as it was computed before it held intervals: one scalar state
/// set per block, every column visited on its own. It is the oracle the
/// interval closure must answer exactly as.
pub(super) struct ScalarClosure {
    state_count: usize,
    solver_count: usize,
    owner: Vec<Option<usize>>,
    pub(super) block_states: Vec<BTreeSet<usize>>,
}

impl ScalarClosure {
    pub(super) fn derive(
        implicit: &[Vec<usize>],
        plan: &AlgebraicProjectionPlan,
        (state_count, solver_count): (usize, usize),
    ) -> Self {
        let mut owner = vec![None; solver_count - state_count];
        for (block, projection) in plan.blocks.scalar_blocks().iter().enumerate() {
            for &y in &projection.y_indices {
                owner[y - state_count] = Some(block);
            }
        }
        let mut closure = Self {
            state_count,
            solver_count,
            owner,
            block_states: vec![BTreeSet::new(); plan.blocks.len()],
        };
        let mut changed = true;
        while changed {
            changed = false;
            for (block, projection) in plan.blocks.scalar_blocks().iter().enumerate() {
                let read = projection
                    .rows
                    .iter()
                    .flat_map(|&row| closure.row_states(&implicit[row], Some(block)))
                    .collect::<Vec<_>>();
                let before = closure.block_states[block].len();
                closure.block_states[block].extend(read);
                changed |= closure.block_states[block].len() != before;
            }
        }
        closure
    }

    pub(super) fn row_states(&self, columns: &[usize], reader: Option<usize>) -> Vec<usize> {
        let mut states = BTreeSet::new();
        for &column in columns {
            states.extend(self.column_states(column, reader));
        }
        states.into_iter().collect()
    }

    pub(super) fn owner_block(&self, column: usize) -> Option<usize> {
        let algebraic = column.checked_sub(self.state_count)?;
        self.owner.get(algebraic).copied().flatten()
    }

    fn column_states(&self, column: usize, reader: Option<usize>) -> Vec<usize> {
        if column < self.state_count {
            return vec![column];
        }
        if column >= self.solver_count {
            return Vec::new();
        }
        match self.owner[column - self.state_count] {
            Some(block) if Some(block) == reader => Vec::new(),
            Some(block) => self.block_states[block].iter().copied().collect(),
            None => panic!("fixture algebraic {column} has no block"),
        }
    }
}

fn provenance() -> PatternProvenance {
    PatternProvenance::derived(
        PatternDerivation::DependencyPropagation,
        Span::from_offsets(SourceId::from_source_name("closure_oracle.mo"), 0, 1),
    )
    .expect("fixture provenance")
}

/// The state Jacobian, which closes over a certified relation through the
/// same closure, answers row for row as the scalar closure does.
#[test]
fn the_state_jacobian_matches_the_scalar_closure() {
    for (name, system) in oracle_fixtures() {
        let derivative = execution_rows(&system.derivative);
        let implicit = execution_rows(&system.implicit);
        let relation = |rows: &[Vec<usize>]| {
            StructuralPattern::from_checked_row_dependencies(
                rows.len(),
                system.solver,
                rows,
                provenance(),
            )
            .expect("fixture relation")
        };
        let jacobian = StructuralPattern::derive_state_jacobian(
            &relation(&derivative),
            Some(&relation(&implicit)),
            &system.plan,
            system.states,
            system.solver,
        )
        .expect("state Jacobian");
        let oracle = ScalarClosure::derive(&implicit, &system.plan, (system.states, system.solver));
        for (row, columns) in derivative.iter().enumerate() {
            let mut actual = Vec::new();
            jacobian.visit_row_columns(row, &mut |column| actual.push(column));
            actual.sort_unstable();
            assert_eq!(
                actual,
                oracle.row_states(columns, None),
                "{name}: row {row}"
            );
        }
    }
}
