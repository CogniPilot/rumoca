//! Owned source inputs and one-time checker-issued observation bindings.

use super::*;
use crate::{ScalarProgramBlock, SolveModel, SolvePureCallOwnerId};

mod native_schedule;
pub use native_schedule::{
    RetainedNativePreparation, RetainedNativeSchedule, RetainedNativeScheduleError,
};

#[cfg(test)]
mod tests;

/// Only the component's actual event-source blocks can issue selectors.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RetainedAssertionSource {
    RootConditions,
    ActionConditions,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, thiserror::Error)]
pub enum RetainedAssertionInputError {
    #[error("canonical start-time resolution must precede assertion issuance")]
    UnresolvedStartTime,
    #[error("the retained root has no selected source program")]
    MissingProgram,
    #[error("the retained source program has no selected operation")]
    MissingOperation,
    #[error("nested scalar-region selection has no retained cursor capability")]
    NestedScalarRegionUnsupported,
    #[error("the exact source invocation has no checked action binding")]
    UncheckedInvocation,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, thiserror::Error)]
pub enum RetainedAssertionResolutionError {
    #[error("a resolved assertion root cannot be resolved for another instance")]
    AlreadyResolved,
    #[error("{}", .0.message())]
    Clock(rumoca_core::ClockLatticeErrorKind),
}

/// An immutable root shared only from the correlated FMI runtime view.
/// Clone shares this allocation; it does not copy tables, programs or actions.
#[derive(Clone, Debug)]
pub struct RetainedAssertionRoot {
    model: Arc<SolveModel>,
    metadata: Arc<crate::fmi::FmiMetadata>,
    resolved_start_time: Option<f64>,
}

impl RetainedAssertionRoot {
    pub(crate) fn from_fmi_root(
        model: Arc<SolveModel>,
        metadata: Arc<crate::fmi::FmiMetadata>,
    ) -> Self {
        Self {
            model,
            metadata,
            resolved_start_time: None,
        }
    }

    #[must_use]
    pub fn execution_source(&self) -> SolveExecutionSource {
        SolveExecutionSource::from_shared(self.model.clone())
    }

    pub fn model(&self) -> &SolveModel {
        &self.model
    }

    /// Borrow the original inventory that was checked with this model's root.
    #[must_use]
    pub fn metadata(&self) -> &crate::fmi::FmiMetadata {
        &self.metadata
    }

    #[must_use]
    pub fn shares_root_owner(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.model, &other.model)
    }

    /// Unchecked source ingredients projected from this same checked inventory.
    /// Component-derived locals are reconstructed by the FMI constructor.
    pub fn fmi_declaration_inputs(
        &self,
    ) -> impl Iterator<Item = crate::fmi::FmiVariableInput> + '_ {
        self.metadata
            .variables()
            .iter()
            .filter_map(|variable| variable.source_input())
    }

    /// The original finite coordinate issued by canonical instance resolution.
    #[must_use]
    pub fn resolved_start_time(&self) -> Option<f64> {
        self.resolved_start_time
    }

    /// Use the existing instance-boundary resolver. Unchanged roots share the
    /// original allocation; changed schedules receive one new immutable root.
    pub fn resolved_periodic_schedules_at(
        &self,
        start_time: f64,
    ) -> Result<Self, RetainedAssertionResolutionError> {
        if self.resolved_start_time.is_some() {
            return Err(RetainedAssertionResolutionError::AlreadyResolved);
        }
        if !start_time.is_finite() {
            return Err(RetainedAssertionResolutionError::Clock(
                rumoca_core::ClockLatticeErrorKind::NonFiniteSeconds,
            ));
        }
        let model = match self
            .model
            .resolved_periodic_schedules_at(start_time)
            .map_err(RetainedAssertionResolutionError::Clock)?
        {
            std::borrow::Cow::Borrowed(_) => self.model.clone(),
            std::borrow::Cow::Owned(resolved) => Arc::new(resolved),
        };
        Ok(Self {
            model,
            metadata: self.metadata.clone(),
            resolved_start_time: Some(start_time),
        })
    }

    /// Select a top-level source operation only. There is no public detached
    /// program, action, region cursor or projection-metadata argument.
    pub fn checked_invocation(
        &self,
        source: RetainedAssertionSource,
        program_index: usize,
        operation: usize,
    ) -> Result<RetainedAssertionInvocation, RetainedAssertionInputError> {
        if self.resolved_start_time.is_none() {
            return Err(RetainedAssertionInputError::UnresolvedStartTime);
        }
        let program = self
            .source_block(source)
            .programs()
            .get(program_index)
            .ok_or(RetainedAssertionInputError::MissingProgram)?;
        let selected = program
            .get(operation)
            .ok_or(RetainedAssertionInputError::MissingOperation)?;
        if matches!(
            selected,
            LinearOp::FunctionFold { .. }
                | LinearOp::GuardedFunctionFold { .. }
                | LinearOp::StoreOutputFunctionFold { .. }
                | LinearOp::FunctionConditional { .. }
        ) {
            return Err(RetainedAssertionInputError::NestedScalarRegionUnsupported);
        }
        let checked = CheckedAssertionInvocation::new(
            &self.model.pure_calls,
            program,
            operation,
            &self.model.problem.events.actions,
        )
        .ok_or(RetainedAssertionInputError::UncheckedInvocation)?;
        let owner = checked.owner.id();
        let bindings = checked
            .projections
            .into_vec()
            .into_iter()
            .map(RetainedActionBinding::from_checked)
            .collect();
        Ok(RetainedAssertionInvocation {
            root: self.clone(),
            source,
            program_index,
            operation,
            owner,
            bindings,
        })
    }

    fn source_block(&self, source: RetainedAssertionSource) -> &ScalarProgramBlock {
        match source {
            RetainedAssertionSource::RootConditions => &self.model.problem.events.root_conditions,
            RetainedAssertionSource::ActionConditions => {
                &self.model.problem.events.action_conditions
            }
        }
    }
}

/// The existing invocation checker issues these recipes; the retained product
/// moves them once and never reruns the checker during observation access.
struct RetainedActionBinding {
    action_index: usize,
    predicate_output: usize,
    message: Box<[RetainedMessageBinding]>,
}

enum RetainedMessageBinding {
    Text { part_index: usize },
    Conversion(CheckedAssertionConversion),
}

impl RetainedActionBinding {
    fn from_checked(action: CheckedAssertionAction<'_>) -> Self {
        let message = action
            .message
            .into_vec()
            .into_iter()
            .enumerate()
            .map(|(part_index, part)| match part {
                CheckedAssertionMessagePart::Text(_) => RetainedMessageBinding::Text { part_index },
                CheckedAssertionMessagePart::Conversion(conversion) => {
                    RetainedMessageBinding::Conversion(conversion)
                }
            })
            .collect();
        Self {
            action_index: action.action_index,
            predicate_output: action.predicate_output,
            message,
        }
    }
}

/// Sealed retained authority. No Clone, serialization or public constructor.
pub struct RetainedAssertionInvocation {
    root: RetainedAssertionRoot,
    source: RetainedAssertionSource,
    program_index: usize,
    operation: usize,
    owner: SolvePureCallOwnerId,
    bindings: Box<[RetainedActionBinding]>,
}

impl RetainedAssertionInvocation {
    pub fn root(&self) -> &RetainedAssertionRoot {
        &self.root
    }
    pub fn table(&self) -> &SolvePureCallTable {
        &self.root.model.pure_calls
    }
    pub fn program(&self) -> &[LinearOp] {
        &self.root.source_block(self.source).programs()[self.program_index]
    }
    pub const fn operation(&self) -> usize {
        self.operation
    }
    pub fn owner(&self) -> &SolvePureCallOwner {
        self.table()
            .owner(self.owner)
            .expect("issued owner belongs to retained immutable table")
    }
    pub fn actions(&self) -> &[SolveEventAction] {
        &self.root.model.problem.events.actions
    }
    pub fn projections(&self) -> impl ExactSizeIterator<Item = RetainedAssertionAction<'_>> {
        self.bindings.iter().map(|binding| {
            RetainedAssertionAction::retained(binding, &self.actions()[binding.action_index])
        })
    }
    pub fn owns_projection(&self, action: &RetainedAssertionAction<'_>) -> bool {
        let RetainedActionView::Retained {
            binding: issuing,
            action,
        } = action.view
        else {
            return false;
        };
        self.bindings.iter().any(|binding| {
            std::ptr::eq(binding, issuing)
                && std::ptr::eq(&self.actions()[binding.action_index], action)
        })
    }
}

/// A view of the same original source action and its private issuing binding.
pub struct RetainedAssertionAction<'invocation> {
    view: RetainedActionView<'invocation>,
}

enum RetainedActionView<'invocation> {
    Retained {
        binding: &'invocation RetainedActionBinding,
        action: &'invocation SolveEventAction,
    },
    Checked(&'invocation CheckedAssertionAction<'invocation>),
}

pub enum RetainedAssertionMessagePart<'invocation> {
    Text(&'invocation str),
    Conversion(&'invocation CheckedAssertionConversion),
}

impl<'invocation> RetainedAssertionAction<'invocation> {
    fn retained(
        binding: &'invocation RetainedActionBinding,
        action: &'invocation SolveEventAction,
    ) -> Self {
        Self {
            view: RetainedActionView::Retained { binding, action },
        }
    }
    pub(super) fn checked(action: &'invocation CheckedAssertionAction<'invocation>) -> Self {
        Self {
            view: RetainedActionView::Checked(action),
        }
    }
    pub const fn action(&self) -> &'invocation SolveEventAction {
        match self.view {
            RetainedActionView::Retained { action, .. } => action,
            RetainedActionView::Checked(action) => action.action,
        }
    }
    pub const fn action_index(&self) -> usize {
        match self.view {
            RetainedActionView::Retained { binding, .. } => binding.action_index,
            RetainedActionView::Checked(action) => action.action_index,
        }
    }
    pub const fn predicate_output(&self) -> usize {
        match self.view {
            RetainedActionView::Retained { binding, .. } => binding.predicate_output,
            RetainedActionView::Checked(action) => action.predicate_output,
        }
    }
    pub fn message(
        &self,
    ) -> impl ExactSizeIterator<Item = RetainedAssertionMessagePart<'invocation>> {
        match self.view {
            RetainedActionView::Retained { binding, action } => RetainedMessageIterator::Retained {
                bindings: binding.message.iter(),
                action,
            },
            RetainedActionView::Checked(action) => {
                RetainedMessageIterator::Checked(action.message.iter())
            }
        }
    }
}

enum RetainedMessageIterator<'invocation> {
    Retained {
        bindings: std::slice::Iter<'invocation, RetainedMessageBinding>,
        action: &'invocation SolveEventAction,
    },
    Checked(std::slice::Iter<'invocation, CheckedAssertionMessagePart<'invocation>>),
}

impl<'invocation> Iterator for RetainedMessageIterator<'invocation> {
    type Item = RetainedAssertionMessagePart<'invocation>;
    fn next(&mut self) -> Option<Self::Item> {
        Some(match self {
            Self::Checked(parts) => match parts.next()? {
                CheckedAssertionMessagePart::Text(text) => RetainedAssertionMessagePart::Text(text),
                CheckedAssertionMessagePart::Conversion(conversion) => {
                    RetainedAssertionMessagePart::Conversion(conversion)
                }
            },
            Self::Retained { bindings, action } => match bindings.next()? {
                RetainedMessageBinding::Text { part_index } => {
                    let SolveEventMessagePart::Text(text) = &action.message.parts[*part_index]
                    else {
                        unreachable!(
                            "issued text binding retains its original immutable message part"
                        )
                    };
                    RetainedAssertionMessagePart::Text(text)
                }
                RetainedMessageBinding::Conversion(conversion) => {
                    RetainedAssertionMessagePart::Conversion(conversion)
                }
            },
        })
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::Retained { bindings, .. } => bindings.size_hint(),
            Self::Checked(parts) => parts.size_hint(),
        }
    }
}
impl ExactSizeIterator for RetainedMessageIterator<'_> {}
