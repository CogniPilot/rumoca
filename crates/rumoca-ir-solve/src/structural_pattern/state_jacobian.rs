//! Relations composed from certified relations (SPEC_0039): the state
//! Jacobian of a continuous system, and the union of two relations over one
//! shape. Every dependency fact is read out of an already-certified pattern;
//! callers choose only which certified relations and which projection blocks
//! are composed. A composition whose facts are incomplete is a construction
//! error, never a conservative guess.

use std::ops::RangeInclusive;

use rumoca_core::Span;

use super::{StructuralPattern, StructuralPatternError, dependency_error};
use crate::{AlgebraicProjectionPlan, IndexIntervals};

impl StructuralPattern {
    /// The structural pattern of the state Jacobian `d(der)/d(states)`.
    ///
    /// The state Jacobian-vector product seeds the states, fills the algebraic
    /// seeds from the algebraic projection's forward sensitivity, and applies
    /// the derivative JVP. `derivative` is the derivative JVP's certified
    /// relation over solver columns (states first, then algebraics; columns
    /// past `solver_count` carry no state seed). Each algebraic column is
    /// replaced by the states it depends on: the columns the `implicit` rows
    /// of the projection block solving it read, closed over the algebraics
    /// those rows read in turn. A block solves all its unknowns together, so
    /// each unknown depends on every column the block reads. The result is a
    /// superset of the nonzeros of every state Jacobian-vector product.
    ///
    /// An algebraic column read without a projection block solving it, a
    /// projection block without an implicit relation, or a block row or
    /// unknown outside the relation's shape is refused.
    pub fn derive_state_jacobian(
        derivative: &Self,
        implicit: Option<&Self>,
        plan: &AlgebraicProjectionPlan,
        state_count: usize,
        solver_count: usize,
    ) -> Result<Self, StructuralPatternError> {
        let span = Some(derivative.owner.provenance.span());
        if derivative.owner.rows as usize != state_count || solver_count < state_count {
            return Err(dependency_error(
                "the derivative relation does not have one row per state",
                span,
            ));
        }
        let closure = AlgebraicClosure::derive(
            implicit.map(|implicit| implicit as &dyn RowColumns),
            plan,
            (state_count, solver_count),
            span,
        )?;
        let rows = (0..state_count)
            .map(|row| closure.row_states(derivative, row, None))
            .collect::<Result<Vec<_>, _>>()?;
        Self::from_checked_row_dependencies(
            state_count,
            state_count,
            &rows,
            derivative.owner.provenance,
        )
    }

    /// The union of two certified relations over one shape: a superset of
    /// each, as a Jacobian colored for either of two systems needs.
    pub fn union(&self, other: &Self) -> Result<Self, StructuralPatternError> {
        if (self.owner.rows, self.owner.columns) != (other.owner.rows, other.owner.columns) {
            return Err(dependency_error(
                "a structural union needs two relations of one shape",
                Some(self.owner.provenance.span()),
            ));
        }
        let rows = (0..self.owner.rows as usize)
            .map(|row| {
                let mut columns = Vec::new();
                self.visit_row_columns(row, &mut |column| columns.push(column));
                other.visit_row_columns(row, &mut |column| columns.push(column));
                columns
            })
            .collect::<Vec<_>>();
        Self::from_checked_row_dependencies(
            self.owner.rows as usize,
            self.owner.columns as usize,
            &rows,
            self.owner.provenance,
        )
    }
}

/// A relation the algebraic closure reads: the solver columns each row
/// depends on. The state Jacobian closes over the certified sensitivity
/// relation; the kinematic state partition closes over the primal read
/// relation, which also holds the columns a row reads only in a condition.
pub(super) trait RowColumns {
    fn row_count(&self) -> usize;
    /// The row's columns as intervals in ascending order of their starts.
    fn visit_row_intervals(&self, row: usize, visitor: &mut dyn FnMut(RangeInclusive<usize>));
}

impl RowColumns for StructuralPattern {
    fn row_count(&self) -> usize {
        self.rows() as usize
    }

    /// Runs of consecutive columns, in the order the pattern visits them.
    fn visit_row_intervals(&self, row: usize, visitor: &mut dyn FnMut(RangeInclusive<usize>)) {
        let mut runs: Vec<(usize, usize)> = Vec::new();
        Self::visit_row_columns(self, row, &mut |column| match runs.last_mut() {
            Some((_, last)) if last.checked_add(1) == Some(column) => *last = column,
            _ => runs.push((column, column)),
        });
        for (start, last) in runs {
            visitor(start..=last);
        }
    }
}

/// The states every solver column depends on through the algebraic
/// projection, held as intervals: a row that reads a whole tensor of states,
/// or a block whose unknowns depend on one, costs an interval, not a state
/// each.
pub(super) struct AlgebraicClosure {
    state_count: usize,
    solver_count: usize,
    /// The projection block solving each algebraic (solver `state_count + k`).
    owner: Vec<Option<usize>>,
    /// The states each projection block's unknowns depend on.
    block_states: Vec<IndexIntervals>,
    span: Option<Span>,
}

impl AlgebraicClosure {
    pub(super) fn derive(
        implicit: Option<&dyn RowColumns>,
        plan: &AlgebraicProjectionPlan,
        (state_count, solver_count): (usize, usize),
        span: Option<Span>,
    ) -> Result<Self, StructuralPatternError> {
        let mut owner = vec![None; solver_count - state_count];
        for (block, projection) in plan.blocks.scalar_blocks().iter().enumerate() {
            for &y in &projection.y_indices {
                *owner_slot(&mut owner, y, state_count, span)? = Some(block);
            }
        }
        let mut closure = Self {
            state_count,
            solver_count,
            owner,
            block_states: vec![IndexIntervals::default(); plan.blocks.len()],
            span,
        };
        match implicit {
            Some(implicit) => closure.close_over(plan, implicit)?,
            None if !plan.blocks.is_empty() => {
                return Err(dependency_error(
                    "algebraic projection blocks have no implicit relation to read",
                    span,
                ));
            }
            None => {}
        }
        Ok(closure)
    }

    /// Grow each block's states from the columns its rows read until no block
    /// changes; the plan's dependency order makes this one or two passes.
    fn close_over(
        &mut self,
        plan: &AlgebraicProjectionPlan,
        implicit: &dyn RowColumns,
    ) -> Result<(), StructuralPatternError> {
        let mut changed = true;
        while changed {
            changed = false;
            for (block, projection) in plan.blocks.scalar_blocks().iter().enumerate() {
                let grown = self.block_states[block].union(&self.block_reads(
                    implicit,
                    &projection.rows,
                    block,
                )?);
                changed |= grown != self.block_states[block];
                self.block_states[block] = grown;
            }
        }
        Ok(())
    }

    /// The states the rows of projection block `block` read.
    fn block_reads(
        &self,
        implicit: &dyn RowColumns,
        rows: &[usize],
        block: usize,
    ) -> Result<IndexIntervals, StructuralPatternError> {
        let mut read = Vec::new();
        for &row in rows {
            self.extend_row_states(implicit, row, Some(block), &mut read)?;
        }
        Ok(IndexIntervals::of_intervals(read))
    }

    /// The states row `row` of `relation` depends on when read by projection
    /// block `reader` (`None` for the derivative), ascending.
    pub(super) fn row_states(
        &self,
        relation: &dyn RowColumns,
        row: usize,
        reader: Option<usize>,
    ) -> Result<Vec<usize>, StructuralPatternError> {
        let mut states = Vec::new();
        self.extend_row_states(relation, row, reader, &mut states)?;
        Ok(IndexIntervals::of_intervals(states).iter().collect())
    }

    /// The states the unknowns of projection block `block` depend on.
    pub(super) fn block_states(&self, block: usize) -> Option<&IndexIntervals> {
        self.block_states.get(block)
    }

    /// The projection block solving algebraic solver column `column`.
    pub(super) fn owner_block(&self, column: usize) -> Option<usize> {
        let algebraic = column.checked_sub(self.state_count)?;
        self.owner.get(algebraic).copied().flatten()
    }

    /// Append the state intervals row `row` of `relation` stands for when
    /// read by projection block `reader`.
    fn extend_row_states(
        &self,
        relation: &dyn RowColumns,
        row: usize,
        reader: Option<usize>,
        states: &mut Vec<RangeInclusive<usize>>,
    ) -> Result<(), StructuralPatternError> {
        if row >= relation.row_count() {
            return Err(dependency_error(
                format!("relation row {row} is outside the relation"),
                self.span,
            ));
        }
        let mut columns = Vec::new();
        relation.visit_row_intervals(row, &mut |interval| columns.push(interval));
        for interval in columns {
            self.extend_interval_states(interval, reader, states)?;
        }
        Ok(())
    }

    /// Append the states a column interval stands for when read by projection
    /// block `reader`: its states themselves, nothing for parameter columns or
    /// the reader's own unknowns, and the states of the block solving any
    /// other algebraic. An interval repeating the last one is not appended.
    fn extend_interval_states(
        &self,
        columns: RangeInclusive<usize>,
        reader: Option<usize>,
        states: &mut Vec<RangeInclusive<usize>>,
    ) -> Result<(), StructuralPatternError> {
        let (start, last) = columns.into_inner();
        if let Some(state_last) = self.state_count.checked_sub(1)
            && start <= state_last
        {
            push_new(states, start..=last.min(state_last));
        }
        let Some(solver_last) = self.solver_count.checked_sub(1) else {
            return Ok(());
        };
        for column in start.max(self.state_count)..=last.min(solver_last) {
            if let Some(block) = self.read_block(column, reader)? {
                self.block_states[block]
                    .intervals()
                    .for_each(|interval| push_new(states, interval));
            }
        }
        Ok(())
    }

    /// The block whose states algebraic column `column` stands for when read
    /// by projection block `reader`; `None` for the reader's own unknowns.
    fn read_block(
        &self,
        column: usize,
        reader: Option<usize>,
    ) -> Result<Option<usize>, StructuralPatternError> {
        match self.owner_block(column) {
            Some(block) => Ok((Some(block) != reader).then_some(block)),
            None => Err(dependency_error(
                format!("solver column {column} is an algebraic no projection block solves"),
                self.span,
            )),
        }
    }
}

/// Append `interval` unless it repeats the last appended one, counting what
/// the closure materialises.
fn push_new(states: &mut Vec<RangeInclusive<usize>>, interval: RangeInclusive<usize>) {
    if states.last() != Some(&interval) {
        record_closure_work(1);
        states.push(interval);
    }
}

#[cfg(test)]
thread_local! {
    static CLOSURE_WORK: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// The entries the closure has materialised on this thread since the last
/// call, which a test bounds instead of timing the closure.
#[cfg(test)]
pub(super) fn take_closure_work() -> usize {
    CLOSURE_WORK.with(|work| work.replace(0))
}

#[cfg(test)]
fn record_closure_work(entries: usize) {
    CLOSURE_WORK.with(|work| work.set(work.get().saturating_add(entries)));
}

#[cfg(not(test))]
fn record_closure_work(_entries: usize) {}

/// The owner slot of algebraic solver column `y`.
fn owner_slot(
    owner: &mut [Option<usize>],
    y: usize,
    state_count: usize,
    span: Option<Span>,
) -> Result<&mut Option<usize>, StructuralPatternError> {
    y.checked_sub(state_count)
        .and_then(|k| owner.get_mut(k))
        .ok_or_else(|| {
            dependency_error(
                format!("a projection block solves solver column {y}, not an algebraic"),
                span,
            )
        })
}
