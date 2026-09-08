//! Private affine construction capabilities for retained semantic relations.
//!
//! A prepared capability borrows every state cell and counter that its commit
//! changes. It therefore cannot be copied, retained across another mutation,
//! or committed twice. Preparation performs every fallible check; commit only
//! assigns through already-borrowed locations.

use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FactState {
    Pending,
    NotChecked,
    Consumed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FactFamily {
    family: &'static str,
    states: Vec<FactState>,
    outstanding: usize,
    first_outstanding: usize,
}

#[must_use = "a prepared fact consumption must be committed exactly once"]
struct PreparedFactConsumption<'a> {
    state: &'a mut FactState,
    outstanding: &'a mut usize,
    first_outstanding: &'a mut usize,
    next_outstanding: usize,
    next_first_outstanding: usize,
}

impl FactFamily {
    const fn new(family: &'static str) -> Self {
        Self {
            family,
            states: Vec::new(),
            outstanding: 0,
            first_outstanding: 0,
        }
    }

    fn reserve(&mut self, additional: usize) -> Result<(), RetainedValidationError> {
        self.states
            .try_reserve(additional)
            .map_err(|_| RetainedValidationError::AllocationFailed {
                family: self.family,
            })
    }

    fn issue<T>(
        &mut self,
        index: u32,
        fact: &RequiredFact<T>,
    ) -> Result<(), RetainedValidationError> {
        if index as usize != self.states.len() {
            return Err(RetainedValidationError::DuplicateSubject);
        }
        let state = match fact {
            RequiredFact::Pending => FactState::Pending,
            RequiredFact::NotChecked(_) => FactState::NotChecked,
            RequiredFact::Checked(_) => FactState::Consumed,
        };
        let next_outstanding = if matches!(state, FactState::Consumed) {
            self.outstanding
        } else {
            self.outstanding
                .checked_add(1)
                .ok_or(RetainedValidationError::LocatorOverflow)?
        };

        // Capacity was reserved for the complete topology before any issue.
        self.states.push(state);
        self.outstanding = next_outstanding;
        self.advance_first();
        Ok(())
    }

    fn prepare_consumption(
        &mut self,
        index: u32,
    ) -> Result<PreparedFactConsumption<'_>, RetainedValidationError> {
        let position = index as usize;
        match self.states.get(position) {
            Some(FactState::Pending) => {}
            Some(FactState::NotChecked | FactState::Consumed) => {
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
                    || matches!(
                        self.states.get(next_first_outstanding),
                        Some(FactState::Consumed)
                    ))
            {
                next_first_outstanding += 1;
            }
        }
        let state = self
            .states
            .get_mut(position)
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        Ok(PreparedFactConsumption {
            state,
            outstanding: &mut self.outstanding,
            first_outstanding: &mut self.first_outstanding,
            next_outstanding,
            next_first_outstanding,
        })
    }

    #[cfg(test)]
    fn consume(&mut self, index: u32) -> Result<(), RetainedValidationError> {
        self.prepare_consumption(index)?.commit();
        Ok(())
    }

    fn finish(&self) -> Result<(), RetainedValidationError> {
        if self.outstanding == 0 {
            return Ok(());
        }
        let index = to_u32(self.first_outstanding)?;
        match self.states.get(self.first_outstanding) {
            Some(FactState::Pending) => Err(RetainedValidationError::MissingFact {
                family: self.family,
                index,
            }),
            Some(FactState::NotChecked) => Err(RetainedValidationError::UncheckedFact {
                family: self.family,
                index,
            }),
            Some(FactState::Consumed) | None => inconsistent(self.family, self.first_outstanding),
        }
    }

    fn advance_first(&mut self) {
        while self
            .states
            .get(self.first_outstanding)
            .is_some_and(|state| matches!(state, FactState::Consumed))
        {
            self.first_outstanding += 1;
        }
    }
}

impl PreparedFactConsumption<'_> {
    fn commit(self) {
        *self.state = FactState::Consumed;
        *self.outstanding = self.next_outstanding;
        *self.first_outstanding = self.next_first_outstanding;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CallResultState {
    AwaitingResolution,
    NotChecked,
    Known { remaining: u32 },
    Unknown,
    Closed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CallResultCapabilities {
    states: Vec<CallResultState>,
    outstanding: u64,
    first_outstanding: usize,
}

#[must_use = "a prepared call resolution must be committed exactly once"]
struct PreparedCallResultResolution<'a> {
    state: &'a mut CallResultState,
    outstanding: &'a mut u64,
    first_outstanding: &'a mut usize,
    next_state: CallResultState,
    next_outstanding: u64,
    next_first_outstanding: usize,
}

#[must_use = "prepared call results must be closed exactly once"]
struct PreparedCallResultClose<'a> {
    state: &'a mut CallResultState,
    outstanding: &'a mut u64,
    first_outstanding: &'a mut usize,
    next_outstanding: u64,
    next_first_outstanding: usize,
}

impl CallResultCapabilities {
    const fn new() -> Self {
        Self {
            states: Vec::new(),
            outstanding: 0,
            first_outstanding: 0,
        }
    }

    fn reserve(&mut self, additional: usize) -> Result<(), RetainedValidationError> {
        self.states
            .try_reserve(additional)
            .map_err(|_| RetainedValidationError::AllocationFailed {
                family: "call-result-capability",
            })
    }

    fn issue<T>(
        &mut self,
        index: u32,
        resolution: &RequiredFact<T>,
    ) -> Result<(), RetainedValidationError> {
        if index as usize != self.states.len() {
            return Err(RetainedValidationError::DuplicateSubject);
        }
        self.states.push(match resolution {
            RequiredFact::Pending => CallResultState::AwaitingResolution,
            RequiredFact::NotChecked(_) => CallResultState::NotChecked,
            RequiredFact::Checked(_) => CallResultState::Closed,
        });
        Ok(())
    }

    fn prepare_resolution(
        &mut self,
        index: u32,
        result_count: usize,
        known: bool,
    ) -> Result<PreparedCallResultResolution<'_>, RetainedValidationError> {
        let position = index as usize;
        if !matches!(
            self.states.get(position),
            Some(CallResultState::AwaitingResolution)
        ) {
            return Err(RetainedValidationError::DuplicateFact {
                family: "call-resolution",
                index,
            });
        }
        let (next_state, next_outstanding, next_first_outstanding) = if known {
            let result_count = u32::try_from(result_count)
                .map_err(|_| RetainedValidationError::LocatorOverflow)?;
            let next_outstanding = self
                .outstanding
                .checked_add(u64::from(result_count))
                .ok_or(RetainedValidationError::LocatorOverflow)?;
            let next_first = match (result_count, self.outstanding) {
                (0, _) => self.first_outstanding,
                (_, 0) => position,
                _ => self.first_outstanding.min(position),
            };
            (
                CallResultState::Known {
                    remaining: result_count,
                },
                next_outstanding,
                next_first,
            )
        } else {
            (
                CallResultState::Unknown,
                self.outstanding,
                self.first_outstanding,
            )
        };
        let state = self
            .states
            .get_mut(position)
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        Ok(PreparedCallResultResolution {
            state,
            outstanding: &mut self.outstanding,
            first_outstanding: &mut self.first_outstanding,
            next_state,
            next_outstanding,
            next_first_outstanding,
        })
    }

    fn prepare_close(
        &mut self,
        index: u32,
        result_count: usize,
    ) -> Result<PreparedCallResultClose<'_>, RetainedValidationError> {
        let position = index as usize;
        let remaining = match self.states.get(position) {
            Some(CallResultState::Known { remaining })
                if usize::try_from(*remaining).ok() == Some(result_count) =>
            {
                *remaining
            }
            Some(CallResultState::Known { .. }) => {
                return inconsistent("call-result-capability", position);
            }
            Some(CallResultState::AwaitingResolution) => {
                return Err(RetainedValidationError::MissingFact {
                    family: "call-resolution",
                    index,
                });
            }
            Some(CallResultState::NotChecked) => {
                return Err(RetainedValidationError::UncheckedFact {
                    family: "call-resolution",
                    index,
                });
            }
            Some(CallResultState::Unknown | CallResultState::Closed) => {
                return inconsistent("call-result-capability", position);
            }
            None => return Err(RetainedValidationError::MissingResolvedSubject),
        };
        let next_outstanding = self.outstanding.checked_sub(u64::from(remaining)).ok_or(
            RetainedValidationError::InconsistentFact {
                family: "call-result-capability",
                index,
            },
        )?;
        let mut next_first = self.first_outstanding;
        if position == next_first {
            while next_first < self.states.len()
                && (next_first == position
                    || !matches!(
                        self.states.get(next_first),
                        Some(CallResultState::Known { remaining }) if *remaining > 0
                    ))
            {
                next_first += 1;
            }
        }
        let state = self
            .states
            .get_mut(position)
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        Ok(PreparedCallResultClose {
            state,
            outstanding: &mut self.outstanding,
            first_outstanding: &mut self.first_outstanding,
            next_outstanding,
            next_first_outstanding: next_first,
        })
    }

    fn finish(&self) -> Result<(), RetainedValidationError> {
        if self.outstanding == 0 {
            Ok(())
        } else {
            inconsistent("call-result-capability", self.first_outstanding)
        }
    }
}

impl PreparedCallResultResolution<'_> {
    fn commit(self) {
        *self.state = self.next_state;
        *self.outstanding = self.next_outstanding;
        *self.first_outstanding = self.next_first_outstanding;
    }
}

impl PreparedCallResultClose<'_> {
    fn commit(self) {
        *self.state = CallResultState::Closed;
        *self.outstanding = self.next_outstanding;
        *self.first_outstanding = self.next_first_outstanding;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CapabilityBalance {
    family: &'static str,
    issued: u64,
    consumed: u64,
}

#[must_use = "a prepared capability balance must be committed exactly once"]
struct PreparedBalance<'a> {
    issued: &'a mut u64,
    consumed: &'a mut u64,
    next_issued: u64,
    next_consumed: u64,
}

impl CapabilityBalance {
    const fn new(family: &'static str) -> Self {
        Self {
            family,
            issued: 0,
            consumed: 0,
        }
    }

    fn prepare_pair(
        &mut self,
        count: usize,
    ) -> Result<PreparedBalance<'_>, RetainedValidationError> {
        let count = u64::try_from(count).map_err(|_| RetainedValidationError::LocatorOverflow)?;
        let next_issued = self
            .issued
            .checked_add(count)
            .ok_or(RetainedValidationError::LocatorOverflow)?;
        let next_consumed = self
            .consumed
            .checked_add(count)
            .ok_or(RetainedValidationError::LocatorOverflow)?;
        Ok(PreparedBalance {
            issued: &mut self.issued,
            consumed: &mut self.consumed,
            next_issued,
            next_consumed,
        })
    }

    #[cfg(test)]
    fn issue_unconsumed(&mut self, count: u64) -> Result<(), RetainedValidationError> {
        self.issued = self
            .issued
            .checked_add(count)
            .ok_or(RetainedValidationError::LocatorOverflow)?;
        Ok(())
    }

    fn finish(&self) -> Result<(), RetainedValidationError> {
        if self.issued == self.consumed {
            Ok(())
        } else {
            inconsistent(
                self.family,
                usize::try_from(self.consumed)
                    .map_err(|_| RetainedValidationError::LocatorOverflow)?,
            )
        }
    }
}

impl PreparedBalance<'_> {
    fn commit(self) {
        *self.issued = self.next_issued;
        *self.consumed = self.next_consumed;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FactCapabilities {
    user_call_graph: FactFamily,
    declaration_start: FactFamily,
    expression_type: FactFamily,
    expression_shape: FactFamily,
    expression_literal: FactFamily,
    reference_resolution: FactFamily,
    reference_shape: FactFamily,
    call_resolution: FactFamily,
    call_result_set: FactFamily,
}

impl FactCapabilities {
    const fn new() -> Self {
        Self {
            user_call_graph: FactFamily::new("user-call-graph"),
            declaration_start: FactFamily::new("declaration-start"),
            expression_type: FactFamily::new("expression-type"),
            expression_shape: FactFamily::new("expression-fixed-shape"),
            expression_literal: FactFamily::new("expression-evaluated-literal"),
            reference_resolution: FactFamily::new("reference-resolution"),
            reference_shape: FactFamily::new("reference-fixed-shape"),
            call_resolution: FactFamily::new("call-resolution"),
            call_result_set: FactFamily::new("call-result-set"),
        }
    }

    fn finish(&self) -> Result<(), RetainedValidationError> {
        self.user_call_graph.finish()?;
        self.declaration_start.finish()?;
        self.expression_type.finish()?;
        self.expression_shape.finish()?;
        self.expression_literal.finish()?;
        self.reference_resolution.finish()?;
        self.reference_shape.finish()?;
        self.call_resolution.finish()?;
        self.call_result_set.finish()
    }
}

#[must_use = "prepared expression facts must be committed exactly once"]
pub(super) struct PreparedExpressionCapabilities<'a> {
    ty: PreparedFactConsumption<'a>,
    shape: PreparedFactConsumption<'a>,
    literal: PreparedFactConsumption<'a>,
}

#[must_use = "a prepared declaration-start fact must be committed exactly once"]
pub(super) struct PreparedDeclarationStartCapability<'a>(PreparedFactConsumption<'a>);

impl PreparedDeclarationStartCapability<'_> {
    pub(super) fn commit(self) {
        self.0.commit();
    }
}

#[must_use = "prepared reference facts must be committed exactly once"]
pub(super) struct PreparedReferenceCapabilities<'a> {
    resolution: PreparedFactConsumption<'a>,
    shape: PreparedFactConsumption<'a>,
}

#[must_use = "a prepared call resolution must be committed exactly once"]
pub(super) struct PreparedCallResolutionCapabilities<'a> {
    builtin_results: PreparedBalance<'a>,
    fact: PreparedFactConsumption<'a>,
    call_results: PreparedCallResultResolution<'a>,
}

#[must_use = "prepared call results must be committed exactly once"]
pub(super) struct PreparedCallResultsCapabilities<'a> {
    fact: PreparedFactConsumption<'a>,
    call_results: PreparedCallResultClose<'a>,
}

#[must_use = "the prepared call graph must be committed exactly once"]
pub(super) struct PreparedUserCallGraph<'a>(PreparedFactConsumption<'a>);

impl PreparedExpressionCapabilities<'_> {
    pub(super) fn commit(self) {
        self.ty.commit();
        self.shape.commit();
        self.literal.commit();
    }
}

impl PreparedReferenceCapabilities<'_> {
    pub(super) fn commit(self) {
        self.resolution.commit();
        self.shape.commit();
    }
}

impl PreparedCallResolutionCapabilities<'_> {
    pub(super) fn commit(self) {
        self.builtin_results.commit();
        self.fact.commit();
        self.call_results.commit();
    }
}

impl PreparedCallResultsCapabilities<'_> {
    pub(super) fn commit(self) {
        self.fact.commit();
        self.call_results.commit();
    }
}

impl PreparedUserCallGraph<'_> {
    pub(super) fn commit(self) {
        self.0.commit();
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct ConstructionCapabilities {
    facts: FactCapabilities,
    call_results: CallResultCapabilities,
    builtin_results: CapabilityBalance,
}

impl ConstructionCapabilities {
    pub(super) fn from_topology(
        retained: &RetainedValidation,
    ) -> Result<Self, RetainedValidationError> {
        let mut facts = FactCapabilities::new();
        facts.user_call_graph.reserve(1)?;
        facts
            .declaration_start
            .reserve(retained.declarations.len())?;
        facts.expression_type.reserve(retained.expressions.len())?;
        facts.expression_shape.reserve(retained.expressions.len())?;
        facts
            .expression_literal
            .reserve(retained.expressions.len())?;
        facts
            .reference_resolution
            .reserve(retained.references.len())?;
        facts.reference_shape.reserve(retained.references.len())?;
        facts.call_resolution.reserve(retained.calls.len())?;
        facts.call_result_set.reserve(retained.calls.len())?;
        let mut call_results = CallResultCapabilities::new();
        call_results.reserve(retained.calls.len())?;

        facts.user_call_graph.issue(0, &retained.user_call_graph)?;
        for (index, declaration) in retained.declarations.iter().enumerate() {
            facts
                .declaration_start
                .issue(to_u32(index)?, &declaration.start)?;
        }
        for (index, expression) in retained.expressions.iter().enumerate() {
            let index = to_u32(index)?;
            facts.expression_type.issue(index, &expression.ty)?;
            facts
                .expression_shape
                .issue(index, &expression.fixed_shape)?;
            facts
                .expression_literal
                .issue(index, &expression.evaluated_literal)?;
        }
        for (index, reference) in retained.references.iter().enumerate() {
            let index = to_u32(index)?;
            facts
                .reference_resolution
                .issue(index, &reference.resolution)?;
            facts.reference_shape.issue(index, &reference.fixed_shape)?;
        }
        for (index, call) in retained.calls.iter().enumerate() {
            let index = to_u32(index)?;
            facts.call_resolution.issue(index, &call.resolution)?;
            facts.call_result_set.issue(index, &call.result_set)?;
            call_results.issue(index, &call.resolution)?;
        }
        Ok(Self {
            facts,
            call_results,
            builtin_results: CapabilityBalance::new("builtin-result-capability"),
        })
    }

    pub(super) fn finish(&self) -> Result<(), RetainedValidationError> {
        self.facts.finish()?;
        self.call_results.finish()?;
        self.builtin_results.finish()
    }

    pub(super) fn prepare_expression(
        &mut self,
        index: u32,
    ) -> Result<PreparedExpressionCapabilities<'_>, RetainedValidationError> {
        Ok(PreparedExpressionCapabilities {
            ty: self.facts.expression_type.prepare_consumption(index)?,
            shape: self.facts.expression_shape.prepare_consumption(index)?,
            literal: self.facts.expression_literal.prepare_consumption(index)?,
        })
    }

    pub(super) fn prepare_declaration_start(
        &mut self,
        index: u32,
    ) -> Result<PreparedDeclarationStartCapability<'_>, RetainedValidationError> {
        Ok(PreparedDeclarationStartCapability(
            self.facts.declaration_start.prepare_consumption(index)?,
        ))
    }

    pub(super) fn prepare_reference(
        &mut self,
        index: u32,
    ) -> Result<PreparedReferenceCapabilities<'_>, RetainedValidationError> {
        Ok(PreparedReferenceCapabilities {
            resolution: self.facts.reference_resolution.prepare_consumption(index)?,
            shape: self.facts.reference_shape.prepare_consumption(index)?,
        })
    }

    pub(super) fn prepare_call_resolution(
        &mut self,
        index: u32,
        builtin_result_count: usize,
        result_count: usize,
        known: bool,
    ) -> Result<PreparedCallResolutionCapabilities<'_>, RetainedValidationError> {
        Ok(PreparedCallResolutionCapabilities {
            builtin_results: self.builtin_results.prepare_pair(builtin_result_count)?,
            fact: self.facts.call_resolution.prepare_consumption(index)?,
            call_results: self
                .call_results
                .prepare_resolution(index, result_count, known)?,
        })
    }

    pub(super) fn prepare_call_results(
        &mut self,
        index: u32,
        result_count: usize,
    ) -> Result<PreparedCallResultsCapabilities<'_>, RetainedValidationError> {
        Ok(PreparedCallResultsCapabilities {
            fact: self.facts.call_result_set.prepare_consumption(index)?,
            call_results: self.call_results.prepare_close(index, result_count)?,
        })
    }

    pub(super) fn prepare_user_call_graph(
        &mut self,
    ) -> Result<PreparedUserCallGraph<'_>, RetainedValidationError> {
        Ok(PreparedUserCallGraph(
            self.facts.user_call_graph.prepare_consumption(0)?,
        ))
    }

    #[cfg(test)]
    pub(super) fn issue_unconsumed_builtin_result(
        &mut self,
    ) -> Result<(), RetainedValidationError> {
        self.builtin_results.issue_unconsumed(1)
    }

    #[cfg(test)]
    pub(super) fn consume_call_result_fact_only(
        &mut self,
        index: u32,
    ) -> Result<(), RetainedValidationError> {
        self.facts.call_result_set.consume(index)
    }
}
