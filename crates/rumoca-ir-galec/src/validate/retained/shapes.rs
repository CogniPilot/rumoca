//! Monotone construction-time fixed-shape finalization.
//!
//! The successful retained carrier stores only the four closed summaries. The
//! per-subject pending states exist solely inside the private builder and are
//! consumed exactly once before root close.

use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FinalizationState {
    Pending,
    Proven,
    Failure,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FixedShapeFamily {
    family: &'static str,
    states: Vec<FinalizationState>,
    outstanding: usize,
    first_outstanding: usize,
    failure_count: usize,
    first_failure: Option<(usize, UnprovenValueShape)>,
}

#[must_use = "a prepared fixed-shape finalization must be committed exactly once"]
pub(super) struct PreparedFixedShapeFinalization<'a> {
    state: &'a mut FinalizationState,
    outstanding: &'a mut usize,
    first_outstanding: &'a mut usize,
    failure_count: &'a mut usize,
    first_failure: &'a mut Option<(usize, UnprovenValueShape)>,
    next_state: FinalizationState,
    next_outstanding: usize,
    next_first_outstanding: usize,
    next_failure_count: usize,
    next_first_failure: Option<(usize, UnprovenValueShape)>,
}

impl FixedShapeFamily {
    fn pending(family: &'static str, count: usize) -> Result<Self, RetainedValidationError> {
        if count > 0 {
            to_u32(count - 1)?;
        }
        let mut states = Vec::new();
        states
            .try_reserve(count)
            .map_err(|_| RetainedValidationError::AllocationFailed { family })?;
        states.resize(count, FinalizationState::Pending);
        Ok(Self {
            family,
            states,
            outstanding: count,
            first_outstanding: 0,
            failure_count: 0,
            first_failure: None,
        })
    }

    fn prepare(
        &mut self,
        index: u32,
        failure: Option<UnprovenValueShape>,
    ) -> Result<PreparedFixedShapeFinalization<'_>, RetainedValidationError> {
        let position = index as usize;
        match self.states.get(position) {
            Some(FinalizationState::Pending) => {}
            Some(FinalizationState::Proven | FinalizationState::Failure) => {
                return Err(RetainedValidationError::DuplicateFact {
                    family: self.family,
                    index,
                });
            }
            None => return Err(RetainedValidationError::MissingResolvedSubject),
        }

        let next_outstanding =
            self.outstanding
                .checked_sub(1)
                .ok_or(RetainedValidationError::InconsistentFact {
                    family: self.family,
                    index,
                })?;
        let mut next_first_outstanding = self.first_outstanding;
        if position == next_first_outstanding {
            while next_first_outstanding < self.states.len()
                && (next_first_outstanding == position
                    || !matches!(
                        self.states.get(next_first_outstanding),
                        Some(FinalizationState::Pending)
                    ))
            {
                next_first_outstanding += 1;
            }
        }
        let (next_state, next_failure_count, next_first_failure) = match failure {
            None => (
                FinalizationState::Proven,
                self.failure_count,
                self.first_failure,
            ),
            Some(failure) => {
                let count = self
                    .failure_count
                    .checked_add(1)
                    .ok_or(RetainedValidationError::LocatorOverflow)?;
                let first = match self.first_failure {
                    Some((first_index, first)) if first_index <= position => {
                        Some((first_index, first))
                    }
                    _ => Some((position, failure)),
                };
                (FinalizationState::Failure, count, first)
            }
        };
        let state = self
            .states
            .get_mut(position)
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        Ok(PreparedFixedShapeFinalization {
            state,
            outstanding: &mut self.outstanding,
            first_outstanding: &mut self.first_outstanding,
            failure_count: &mut self.failure_count,
            first_failure: &mut self.first_failure,
            next_state,
            next_outstanding,
            next_first_outstanding,
            next_failure_count,
            next_first_failure,
        })
    }

    fn close(&self) -> Result<FixedShapeFamilyState, RetainedValidationError> {
        if self.outstanding != 0 {
            return Err(RetainedValidationError::MissingFact {
                family: self.family,
                index: to_u32(self.first_outstanding)?,
            });
        }
        let Some(count) = NonZeroUsize::new(self.failure_count) else {
            return Ok(FixedShapeFamilyState::AllProven);
        };
        let Some((_, first)) = self.first_failure else {
            return inconsistent(self.family, self.failure_count);
        };
        Ok(FixedShapeFamilyState::Failures { count, first })
    }
}

impl PreparedFixedShapeFinalization<'_> {
    pub(super) fn commit(self) {
        *self.state = self.next_state;
        *self.outstanding = self.next_outstanding;
        *self.first_outstanding = self.next_first_outstanding;
        *self.failure_count = self.next_failure_count;
        *self.first_failure = self.next_first_failure;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct FixedShapeCapabilities {
    call_results: FixedShapeFamily,
    declarations: FixedShapeFamily,
    expressions: FixedShapeFamily,
    references: FixedShapeFamily,
}

impl FixedShapeCapabilities {
    pub(super) fn from_topology(
        retained: &RetainedValidation,
    ) -> Result<Self, RetainedValidationError> {
        let primitive_count = retained
            .declarations
            .iter()
            .filter(|subject| subject.primitive)
            .count();
        let mut result = Self {
            call_results: FixedShapeFamily::pending(
                "call-result-fixed-shape-finalization",
                retained.calls.len(),
            )?,
            declarations: FixedShapeFamily::pending(
                "declaration-fixed-shape-finalization",
                primitive_count,
            )?,
            expressions: FixedShapeFamily::pending(
                "expression-fixed-shape-finalization",
                retained.expressions.len(),
            )?,
            references: FixedShapeFamily::pending(
                "reference-fixed-shape-finalization",
                retained.references.len(),
            )?,
        };

        for (primitive_index, declaration) in retained
            .declarations
            .iter()
            .filter(|subject| subject.primitive)
            .enumerate()
        {
            let failure = declaration
                .fixed_shape
                .is_none()
                .then_some(UnprovenValueShape {
                    subject: "declaration",
                    provenance: declaration.provenance,
                });
            result
                .declarations
                .prepare(to_u32(primitive_index)?, failure)?
                .commit();
        }
        Ok(result)
    }

    pub(super) fn prepare_expression(
        &mut self,
        index: u32,
        shape: &Option<FixedValueShape>,
        provenance: SubjectProvenance,
    ) -> Result<PreparedFixedShapeFinalization<'_>, RetainedValidationError> {
        self.expressions.prepare(
            index,
            shape.is_none().then_some(UnprovenValueShape {
                subject: "expression",
                provenance,
            }),
        )
    }

    pub(super) fn prepare_reference(
        &mut self,
        index: u32,
        shape: &Option<FixedValueShape>,
        provenance: SubjectProvenance,
    ) -> Result<PreparedFixedShapeFinalization<'_>, RetainedValidationError> {
        self.references.prepare(
            index,
            shape.is_none().then_some(UnprovenValueShape {
                subject: "reference",
                provenance,
            }),
        )
    }

    pub(super) fn prepare_call_results(
        &mut self,
        index: u32,
        failure: Option<UnprovenValueShape>,
    ) -> Result<PreparedFixedShapeFinalization<'_>, RetainedValidationError> {
        self.call_results.prepare(index, failure)
    }

    pub(super) fn finish(&self) -> Result<FixedShapeClosure, RetainedValidationError> {
        Ok(FixedShapeClosure {
            call_results: self.call_results.close()?,
            declarations: self.declarations.close()?,
            expressions: self.expressions.close()?,
            references: self.references.close()?,
        })
    }

    #[cfg(test)]
    pub(super) fn fault_finalize_expression(
        &mut self,
        index: u32,
    ) -> Result<(), RetainedValidationError> {
        self.expressions.prepare(index, None)?.commit();
        Ok(())
    }
}
