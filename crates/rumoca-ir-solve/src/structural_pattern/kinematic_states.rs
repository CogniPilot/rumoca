//! The kinematic state partition a partitioned (semi-implicit) step reads.
//!
//! A state `q` is kinematic when the derivative row of `q` stays exact after
//! the other states are updated in place without a new algebraic refresh: the
//! row reads every other state only as a state slot, and each algebraic it
//! reads depends, through the algebraic projection, on kinematic states only.
//! Rewriting the remaining states and re-running the derivative program over
//! the algebraics refreshed at the evaluated point then gives the kinematic
//! entries a full evaluation at the rewritten point would give.
//!
//! Those conditions hold vacuously when every state is kinematic, so they
//! cannot choose a partition alone. The remaining states `R` are the least set
//! that contains two seeds and is closed under the conditions:
//!
//! - a state whose derivative closure solves for derivatives
//!   (`LinearSolveComponent`), evaluates a relation (`Compare`, an event owner
//!   or a `noEvent` condition alike), runs an effect (tables, random state),
//!   reads a slot outside the solver vector, or reaches a simultaneous
//!   projection block (more than one unknown, or a tearing);
//! - a velocity coordinate: a state whose slot is, unchanged, the derivative of
//!   another state (`der(q) = v`).
//!
//! The kinematic states are the rest. A system with no seed has every state
//! kinematic and an empty `R`, which a partitioned step treats as one explicit
//! step. Reads are the primal execution read relation of each output: its
//! own reads and its program's obligations, which every output of the
//! program shares
//! ([`StructuralPattern::derive_output_execution_y_dependency_parts`]), not the
//! sensitivity relation the state Jacobian closes over: a state read only
//! inside a condition has no tangent but still changes the value.
//!
//! The relations are read per output row from the scalar programs the
//! runtime executes: a tensor compute node enters as its per-cell rows, so a
//! cell is judged by what that cell reads, and the partition describes the
//! very programs the kinematic evaluation runs. The derivation fails closed:
//! an algebraic no projection block solves gives an empty kinematic set; an
//! output without a program or whose read set cannot be derived keeps its
//! state, or the algebraics of its projection block, out of it.

use std::collections::{BTreeMap, BTreeSet};
use std::ops::RangeInclusive;
use std::sync::Arc;

use super::StructuralPattern;
use super::state_jacobian::{AlgebraicClosure, RowColumns};
use crate::{
    AlgebraicProjectionBlock, AlgebraicProjectionPlan, IndexIntervals, LinearOp, Reg,
    ScalarProgramBlock,
};

/// Why a state is not kinematic: the seed or closure step that placed it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NonKinematicReason {
    /// Its derivative program, or an algebraic it reads, solves a linear
    /// system (a derivative solve).
    DerivativeSolve,
    /// Its derivative program, or an algebraic it reads, evaluates a relation.
    Relation,
    /// Its derivative program, or an algebraic it reads, runs an effect, reads
    /// a slot outside the solver vector, or has no derivable read set.
    Unanalysable,
    /// It reads an algebraic of a simultaneous projection block.
    SimultaneousBlock,
    /// The derivative of state `of` is a copy of its slot.
    Velocity { of: usize },
    /// It reads an algebraic whose closure reaches the non-kinematic state
    /// `through`.
    Closure { through: usize },
}

/// The compiler-issued kinematic continuous states, ascending, and the reason
/// each other state is not kinematic.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct KinematicStates {
    states: Box<[usize]>,
    reasons: Box<[Option<NonKinematicReason>]>,
}

impl KinematicStates {
    /// Derive the partition from the derivative rows (one output per state)
    /// and the implicit rows the algebraic projection `plan` solves, over a
    /// solver vector of `state_count` states followed by its algebraics up to
    /// `solver_count`.
    #[must_use]
    pub fn derive(
        derivative: &ScalarProgramBlock,
        implicit: &ScalarProgramBlock,
        plan: &AlgebraicProjectionPlan,
        state_count: usize,
        solver_count: usize,
    ) -> Self {
        derive_reasons(derivative, implicit, plan, (state_count, solver_count))
            .map(|reasons| Self {
                states: (0..reasons.len())
                    .filter(|&state| reasons[state].is_none())
                    .collect(),
                reasons: reasons.into_boxed_slice(),
            })
            .unwrap_or_default()
    }

    /// The kinematic state indices, ascending.
    #[must_use]
    pub fn states(&self) -> &[usize] {
        &self.states
    }

    /// Why `state` is not kinematic; `None` for a kinematic state and for
    /// every state of a partition that was not issued.
    #[must_use]
    pub fn reason(&self, state: usize) -> Option<NonKinematicReason> {
        self.reasons.get(state).copied().flatten()
    }
}

/// What one output row reads.
#[derive(Clone, Default)]
struct OutputReads {
    /// Solver-Y columns the output itself reads.
    columns: Arc<IndexIntervals>,
    /// Solver-Y columns its program's execution obligations read, shared by
    /// every output of the program rather than copied into each.
    obligations: Arc<IndexIntervals>,
    /// What makes the program unusable for a kinematic row, if anything.
    taint: Option<NonKinematicReason>,
    /// The slot the output copies unchanged, when it is exactly one load.
    copy_of: Option<usize>,
}

impl OutputReads {
    /// Every column the output's execution reads: its own and its program's
    /// obligations, interval by interval in ascending order of their starts.
    fn intervals(&self) -> impl Iterator<Item = RangeInclusive<usize>> + '_ {
        let mut own = self.columns.intervals().peekable();
        let mut shared = self.obligations.intervals().peekable();
        std::iter::from_fn(move || match (own.peek(), shared.peek()) {
            (Some(own_next), Some(shared_next)) if own_next.start() <= shared_next.start() => {
                own.next()
            }
            (_, Some(_)) => shared.next(),
            _ => own.next(),
        })
    }

    /// Whether the output reads a slot past the solver vector.
    fn reads_outside(&self, solver_count: usize) -> bool {
        [&self.columns, &self.obligations]
            .into_iter()
            .any(|columns| {
                columns
                    .intervals()
                    .last()
                    .is_some_and(|interval| *interval.end() >= solver_count)
            })
    }

    /// The algebraic columns the output's execution reads.
    fn algebraic_columns(
        &self,
        (state_count, solver_count): (usize, usize),
    ) -> impl Iterator<Item = usize> + '_ {
        let solver_last = solver_count.checked_sub(1);
        self.intervals().flat_map(move |interval| {
            let (start, last) = interval.into_inner();
            solver_last
                .map(|solver_last| start.max(state_count)..=last.min(solver_last))
                .into_iter()
                .flatten()
        })
    }
}

/// The implicit rows as the closure reads them.
struct ImplicitRelation<'a>(&'a [OutputReads]);

impl RowColumns for ImplicitRelation<'_> {
    fn row_count(&self) -> usize {
        self.0.len()
    }

    fn visit_row_intervals(&self, row: usize, visitor: &mut dyn FnMut(RangeInclusive<usize>)) {
        self.0[row].intervals().for_each(visitor);
    }
}

fn derive_reasons(
    derivative: &ScalarProgramBlock,
    implicit: &ScalarProgramBlock,
    plan: &AlgebraicProjectionPlan,
    (state_count, solver_count): (usize, usize),
) -> Option<Vec<Option<NonKinematicReason>>> {
    let derivative = output_reads(derivative, state_count)?;
    let implicit = output_reads(implicit, implicit.output_count())?;
    let closure = AlgebraicClosure::derive(
        Some(&ImplicitRelation(&implicit)),
        plan,
        (state_count, solver_count),
        None,
    )
    .ok()?;
    let blocks = plan.blocks.scalar_blocks();
    if blocks.len() != plan.blocks.len() {
        return None;
    }
    let block_taint = block_taint(blocks, &implicit, &closure, (state_count, solver_count))?;
    let mut reasons = vec![None; state_count];
    let mut read_blocks = Vec::with_capacity(state_count);
    for (state, row) in derivative.iter().enumerate() {
        let owners = algebraic_owners(row, &closure, (state_count, solver_count))?;
        let outside = row
            .reads_outside(solver_count)
            .then_some(NonKinematicReason::Unanalysable);
        reasons[state] = reasons[state]
            .or(row.taint)
            .or(outside)
            .or_else(|| owners.iter().find_map(|&block| block_taint[block]));
        if let Some(velocity) = row
            .copy_of
            .filter(|&slot| slot < state_count && slot != state)
        {
            reasons[velocity] =
                reasons[velocity].or(Some(NonKinematicReason::Velocity { of: state }));
        }
        read_blocks.push(owners);
    }
    // The non-kinematic states as intervals, kept equal to `reasons`.
    let mut non_kinematic = IndexIntervals::of(
        reasons
            .iter()
            .enumerate()
            .filter(|(_, reason)| reason.is_some())
            .map(|(state, _)| state),
    );
    let mut changed = true;
    while changed {
        changed = false;
        for state in 0..state_count {
            if reasons[state].is_some() {
                continue;
            }
            if let Some(through) = reached(&read_blocks[state], &closure, &non_kinematic) {
                reasons[state] = Some(NonKinematicReason::Closure { through });
                non_kinematic.insert(state);
                changed = true;
            }
        }
    }
    Some(reasons)
}

/// The least non-kinematic state an algebraic of one of `blocks` depends on,
/// in block order.
fn reached(
    blocks: &BTreeSet<usize>,
    closure: &AlgebraicClosure,
    non_kinematic: &IndexIntervals,
) -> Option<usize> {
    blocks.iter().find_map(|&block| {
        let reached = closure.block_states(block)?.intersection(non_kinematic);
        reached.intervals().next().map(|interval| *interval.start())
    })
}

/// The projection blocks solving the algebraic columns `row` reads; `None`
/// when one has no block.
fn algebraic_owners(
    row: &OutputReads,
    closure: &AlgebraicClosure,
    counts: (usize, usize),
) -> Option<BTreeSet<usize>> {
    row.algebraic_columns(counts)
        .map(|column| closure.owner_block(column))
        .collect()
}

/// Each projection block's taint: simultaneous, a tainted row or a slot
/// outside the solver vector, or an algebraic of a tainted block it reads.
fn block_taint(
    blocks: &[AlgebraicProjectionBlock],
    implicit: &[OutputReads],
    closure: &AlgebraicClosure,
    counts: (usize, usize),
) -> Option<Vec<Option<NonKinematicReason>>> {
    let mut taint = blocks
        .iter()
        .map(|block| own_block_taint(block, implicit, counts.1))
        .collect::<Option<Vec<_>>>()?;
    let mut changed = true;
    while changed {
        changed = false;
        for (index, block) in blocks.iter().enumerate() {
            if taint[index].is_some() {
                continue;
            }
            if let Some(reason) =
                reads_tainted_block(block, index, implicit, (closure, counts), &taint)
            {
                taint[index] = Some(reason);
                changed = true;
            }
        }
    }
    Some(taint)
}

/// A block's own taint; `None` when one of its rows has no program.
fn own_block_taint(
    block: &AlgebraicProjectionBlock,
    implicit: &[OutputReads],
    solver_count: usize,
) -> Option<Option<NonKinematicReason>> {
    let mut taint = (block.y_indices.len() > 1 || block.tearing.is_some())
        .then_some(NonKinematicReason::SimultaneousBlock);
    for &row in &block.rows {
        let output = implicit.get(row)?;
        let outside = output
            .reads_outside(solver_count)
            .then_some(NonKinematicReason::Unanalysable);
        taint = taint.or(output.taint).or(outside);
    }
    Some(taint)
}

fn reads_tainted_block(
    block: &AlgebraicProjectionBlock,
    index: usize,
    implicit: &[OutputReads],
    (closure, counts): (&AlgebraicClosure, (usize, usize)),
    taint: &[Option<NonKinematicReason>],
) -> Option<NonKinematicReason> {
    block
        .rows
        .iter()
        .flat_map(|&row| implicit[row].algebraic_columns(counts))
        .find_map(|column| {
            closure
                .owner_block(column)
                .filter(|&owner| owner != index)
                .and_then(|owner| taint[owner])
        })
}

/// The read set of each of the first `rows` outputs of `programs`. An
/// output without a program reads nothing and is unanalysable: an implicit
/// row the derivative rows replaced is never read, and any other one taints
/// what reads it.
fn output_reads(programs: &ScalarProgramBlock, rows: usize) -> Option<Vec<OutputReads>> {
    let mut analyzed: Vec<Option<Vec<OutputReads>>> = vec![None; programs.row_count()];
    let unanalysable = OutputReads {
        taint: Some(NonKinematicReason::Unanalysable),
        ..OutputReads::default()
    };
    (0..rows)
        .map(|row| {
            let Some((program, position)) = programs.output_position(row) else {
                return Some(unanalysable.clone());
            };
            let outputs = analyzed.get_mut(program)?.get_or_insert_with(|| {
                program_reads(
                    programs.programs()[program].as_slice(),
                    programs.program_span(program),
                )
            });
            Some(
                outputs
                    .get(position)
                    .cloned()
                    .unwrap_or(unanalysable.clone()),
            )
        })
        .collect()
}

/// Per-output reads of one scalar program. A program whose read set cannot
/// be derived yields no outputs, so each of its rows is unanalysable.
fn program_reads(program: &[LinearOp], span: Option<rumoca_core::Span>) -> Vec<OutputReads> {
    let (Ok((reads, obligations)), Ok(effects)) = (
        StructuralPattern::derive_output_execution_y_dependency_parts(program, span),
        StructuralPattern::execution_has_effect(program, span),
    ) else {
        return Vec::new();
    };
    let program_taint = relation_or_solve(program);
    let copies = output_copies(program);
    reads
        .into_iter()
        .zip(std::iter::repeat(effects))
        .enumerate()
        .map(|(position, (columns, effect))| OutputReads {
            columns,
            obligations: Arc::clone(&obligations),
            taint: program_taint.or(effect.then_some(NonKinematicReason::Unanalysable)),
            copy_of: copies.get(position).copied().flatten(),
        })
        .collect()
}

/// Whether `program`, or a region nested in it, solves a linear system or
/// evaluates a relation; a solve is reported first.
fn relation_or_solve(program: &[LinearOp]) -> Option<NonKinematicReason> {
    let mut found = None;
    for op in program {
        match op_relation_or_solve(op) {
            Some(NonKinematicReason::DerivativeSolve) => {
                return Some(NonKinematicReason::DerivativeSolve);
            }
            other => found = found.or(other),
        }
    }
    found
}

fn op_relation_or_solve(op: &LinearOp) -> Option<NonKinematicReason> {
    match op {
        LinearOp::LinearSolveComponent { .. } => Some(NonKinematicReason::DerivativeSolve),
        LinearOp::Compare { .. } => Some(NonKinematicReason::Relation),
        LinearOp::FunctionFold { program, .. }
        | LinearOp::GuardedFunctionFold { program, .. }
        | LinearOp::StoreOutputFunctionFold { program, .. } => {
            program.regions().find_map(relation_or_solve)
        }
        LinearOp::FunctionConditional { program, .. } => program
            .arms
            .iter()
            .find_map(|arm| relation_or_solve(&arm.condition).or(relation_or_solve(&arm.result)))
            .or_else(|| relation_or_solve(&program.fallback)),
        _ => None,
    }
}

/// For each `StoreOutput` in order, the Y slot it stores unchanged when its
/// source register was last written by a single `LoadY`. Empty when the
/// program stores outputs any other way, so positions stay exact.
fn output_copies(program: &[LinearOp]) -> Vec<Option<usize>> {
    let mut loaded = BTreeMap::new();
    let mut copies = Vec::new();
    for op in program {
        match op {
            LinearOp::StoreOutput { src } => copies.push(loaded.get(src).copied().flatten()),
            LinearOp::StoreOutputRange { .. }
            | LinearOp::StoreOutputFunctionFold { .. }
            | LinearOp::StoreOutputFoldTensorUpdate { .. } => return Vec::new(),
            LinearOp::LoadY { dst, index } => {
                loaded.insert(*dst, Some(*index));
            }
            LinearOp::Move { dst, src } => {
                let source = loaded.get(src).copied().flatten();
                loaded.insert(*dst, source);
            }
            _ => forget_written(op, &mut loaded),
        }
    }
    copies
}

/// Forget the loads of the registers `op` writes.
fn forget_written(op: &LinearOp, loaded: &mut BTreeMap<Reg, Option<usize>>) {
    let Some(dst) = op.dst_register() else {
        return;
    };
    for offset in 0..op.dst_register_count().max(1) {
        let Some(register) = Reg::try_from(offset)
            .ok()
            .and_then(|offset| dst.checked_add(offset))
        else {
            break;
        };
        loaded.insert(register, None);
    }
}

#[cfg(test)]
mod scalar_oracle_tests;
