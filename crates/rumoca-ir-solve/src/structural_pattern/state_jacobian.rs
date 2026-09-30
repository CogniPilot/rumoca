//! Relations composed from certified relations (SPEC_0039): the state
//! Jacobian of a continuous system, and the union of two relations over one
//! shape. Every dependency fact is read out of an already-certified pattern;
//! callers choose only which certified relations and which projection blocks
//! are composed. A composition whose facts are incomplete is a construction
//! error, never a conservative guess.

use std::collections::BTreeSet;

use rumoca_core::Span;

use super::{StructuralPattern, StructuralPatternError, dependency_error};
use crate::AlgebraicProjectionPlan;

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
        let span = Some(derivative.provenance.span());
        if derivative.rows as usize != state_count || solver_count < state_count {
            return Err(dependency_error(
                "the derivative relation does not have one row per state",
                span,
            ));
        }
        let closure = AlgebraicClosure::derive(implicit, plan, (state_count, solver_count), span)?;
        let rows = (0..state_count)
            .map(|row| closure.row_states(derivative, row, None))
            .collect::<Result<Vec<_>, _>>()?;
        Self::from_checked_row_dependencies(state_count, state_count, &rows, derivative.provenance)
    }

    /// The union of two certified relations over one shape: a superset of
    /// each, as a Jacobian colored for either of two systems needs.
    pub fn union(&self, other: &Self) -> Result<Self, StructuralPatternError> {
        if (self.rows, self.columns) != (other.rows, other.columns) {
            return Err(dependency_error(
                "a structural union needs two relations of one shape",
                Some(self.provenance.span()),
            ));
        }
        let rows = (0..self.rows as usize)
            .map(|row| {
                let mut columns = Vec::new();
                self.visit_row_columns(row, &mut |column| columns.push(column));
                other.visit_row_columns(row, &mut |column| columns.push(column));
                columns
            })
            .collect::<Vec<_>>();
        Self::from_checked_row_dependencies(
            self.rows as usize,
            self.columns as usize,
            &rows,
            self.provenance,
        )
    }
}

/// The states every solver column depends on through the algebraic
/// projection.
struct AlgebraicClosure {
    state_count: usize,
    solver_count: usize,
    /// The projection block solving each algebraic (solver `state_count + k`).
    owner: Vec<Option<usize>>,
    /// The states each projection block's unknowns depend on.
    block_states: Vec<BTreeSet<usize>>,
    span: Option<Span>,
}

impl AlgebraicClosure {
    fn derive(
        implicit: Option<&StructuralPattern>,
        plan: &AlgebraicProjectionPlan,
        (state_count, solver_count): (usize, usize),
        span: Option<Span>,
    ) -> Result<Self, StructuralPatternError> {
        let mut owner = vec![None; solver_count - state_count];
        for (block, projection) in plan.blocks.iter().enumerate() {
            for &y in &projection.y_indices {
                *owner_slot(&mut owner, y, state_count, span)? = Some(block);
            }
        }
        let mut closure = Self {
            state_count,
            solver_count,
            owner,
            block_states: vec![BTreeSet::new(); plan.blocks.len()],
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
        implicit: &StructuralPattern,
    ) -> Result<(), StructuralPatternError> {
        let mut changed = true;
        while changed {
            changed = false;
            for (block, projection) in plan.blocks.iter().enumerate() {
                let read = projection
                    .rows
                    .iter()
                    .map(|&row| self.row_states(implicit, row, Some(block)))
                    .collect::<Result<Vec<_>, _>>()?;
                let before = self.block_states[block].len();
                self.block_states[block].extend(read.into_iter().flatten());
                changed |= self.block_states[block].len() != before;
            }
        }
        Ok(())
    }

    /// The states row `row` of `relation` depends on when read by projection
    /// block `reader` (`None` for the derivative).
    fn row_states(
        &self,
        relation: &StructuralPattern,
        row: usize,
        reader: Option<usize>,
    ) -> Result<Vec<usize>, StructuralPatternError> {
        if row >= relation.rows as usize {
            return Err(dependency_error(
                format!("relation row {row} is outside the relation"),
                self.span,
            ));
        }
        let mut columns = Vec::new();
        relation.visit_row_columns(row, &mut |column| columns.push(column));
        let mut states = BTreeSet::new();
        for column in columns {
            states.extend(self.column_states(column, reader)?);
        }
        Ok(states.into_iter().collect())
    }

    /// The states solver column `column` stands for when read by projection
    /// block `reader`: a state itself, nothing for a parameter column or the
    /// reader's own unknowns, and the states of the block solving any other
    /// algebraic.
    fn column_states(
        &self,
        column: usize,
        reader: Option<usize>,
    ) -> Result<Vec<usize>, StructuralPatternError> {
        if column < self.state_count {
            return Ok(vec![column]);
        }
        if column >= self.solver_count {
            return Ok(Vec::new());
        }
        match self.owner[column - self.state_count] {
            Some(block) if Some(block) == reader => Ok(Vec::new()),
            Some(block) => Ok(self.block_states[block].iter().copied().collect()),
            None => Err(dependency_error(
                format!("solver column {column} is an algebraic no projection block solves"),
                self.span,
            )),
        }
    }
}

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
