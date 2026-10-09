//! Private ordered observations; only complete invocations publish values.

use super::*;
use rumoca_ir_solve::{SolveAssertionLevel, SolveAssertionMessage};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct AssertionObservation {
    pub(super) owner: SolvePureCallOwnerId,
    pub(super) predicate_output: usize,
    pub(super) provenance: Span,
    pub(super) captures: Box<[(usize, TypedValue)]>,
}

// Exact owner and actual input cells retained only when observations exist.
#[allow(dead_code)]
#[derive(Debug)]
pub(super) struct AssertionInvocation<'model> {
    pub(super) owner: &'model SolvePureCallOwner,
    pub(super) arguments: std::sync::Arc<[TypedValue]>,
}

// Retained for the checked C25 projection; ordinary diagnostics confer no membership.
#[allow(dead_code)]
#[derive(Debug)]
pub(super) struct AssertionFrame<'model> {
    pub(super) owner: SolvePureCallOwnerId,
    pub(super) program: &'model TypedProgram,
    pub(super) operation: usize,
    pub(super) domain_point: Option<std::sync::Arc<[i64]>>,
    pub(super) invocation: Option<AssertionInvocation<'model>>,
}

#[derive(Debug)]
pub(super) struct ObservedAssertion<'model> {
    pub(super) observation: AssertionObservation,
    pub(super) invocation_path: Vec<AssertionFrame<'model>>,
}

#[derive(Debug)]
pub(super) struct AssertionStop<'model> {
    pub(super) observations: Vec<ObservedAssertion<'model>>,
}

/// Diagnostic data from an ordinary failed typed call; grants no observation authority.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypedAssertionFailure {
    observation: AssertionObservation,
}

impl TypedAssertionFailure {
    #[must_use]
    pub const fn owner(&self) -> SolvePureCallOwnerId {
        self.observation.owner
    }

    #[must_use]
    pub const fn predicate_output(&self) -> usize {
        self.observation.predicate_output
    }

    #[must_use]
    pub const fn source_span(&self) -> Span {
        self.observation.provenance
    }

    #[must_use]
    pub fn message_captures(&self) -> &[(usize, TypedValue)] {
        &self.observation.captures
    }
}

pub(super) enum InvocationCompletion<'model, T> {
    Complete {
        values: T,
        observations: Vec<ObservedAssertion<'model>>,
    },
    Stopped(AssertionStop<'model>),
    Fault {
        error: TypedProgramEvalError,
        observations: Vec<ObservedAssertion<'model>>,
    },
}

impl<T> InvocationCompletion<'_, T> {
    pub(super) fn into_complete(self) -> Result<T, TypedProgramEvalError> {
        match self {
            Self::Complete { values, .. } => Ok(values),
            Self::Stopped(mut stop) => Err(TypedProgramEvalError::AssertionFailed {
                failure: TypedAssertionFailure {
                    observation: stop
                        .observations
                        .pop()
                        .expect("a stop owns its failed observation")
                        .observation,
                },
            }),
            Self::Fault { error, .. } => Err(error),
        }
    }
}

impl<'model> EvalFrame<'model, '_> {
    pub(super) fn retain_invocation(
        &mut self,
        owner: &'model SolvePureCallOwner,
        arguments: &[TypedValue],
    ) {
        let observations = match &mut self.stopped {
            Some(stop) => &mut stop.observations,
            None => &mut self.observations,
        };
        if observations.is_empty() {
            return;
        }
        let arguments: std::sync::Arc<[TypedValue]> = std::sync::Arc::from(arguments);
        for observed in observations {
            let frame = observed
                .invocation_path
                .last_mut()
                .expect("an observation owns its origin frame");
            debug_assert!(std::ptr::eq(frame.program, self.program));
            debug_assert_eq!(frame.owner, owner.id());
            frame.invocation = Some(AssertionInvocation {
                owner,
                arguments: arguments.clone(),
            });
        }
    }

    pub(super) fn accept_completion<T>(
        &mut self,
        completion: InvocationCompletion<'model, T>,
    ) -> Option<T> {
        self.accept_completion_at(completion, None)
    }

    pub(super) fn accept_completion_at<T>(
        &mut self,
        completion: InvocationCompletion<'model, T>,
        point: Option<&[i64]>,
    ) -> Option<T> {
        let (values, mut observations, stopped) = match completion {
            InvocationCompletion::Complete {
                values,
                observations,
            } => (Some(values), observations, false),
            InvocationCompletion::Stopped(stop) => (None, stop.observations, true),
            InvocationCompletion::Fault {
                error,
                observations,
            } => {
                self.fault = Some(error);
                (None, observations, false)
            }
        };
        let point = if observations.is_empty() {
            None
        } else {
            point.map(std::sync::Arc::from)
        };
        for observed in &mut observations {
            observed.invocation_path.push(AssertionFrame {
                owner: self.owner,
                program: self.program,
                operation: self.operation,
                domain_point: point.clone(),
                invocation: None,
            });
        }
        self.observations.append(&mut observations);
        if stopped {
            self.stopped = Some(AssertionStop {
                observations: std::mem::take(&mut self.observations),
            });
        }
        values
    }

    pub(super) fn eval_assertion(
        &mut self,
        (predicate_output, message_outputs): (usize, &[usize]),
        condition: SolveRegisterId,
        captures: &[SolveRegisterId],
        destinations: &[SolveRegisterId],
        message: &'model SolveAssertionMessage,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        if scalar_boolean(self.read(condition, provenance)?, provenance)? {
            for destination in destinations {
                let value_type = self.destination_type(*destination, provenance)?;
                let value = TypedValue::scalar(&SolveValue::inactive_assertion_message(
                    value_type.element_type(),
                ));
                self.write(*destination, value, provenance)?;
            }
            return Ok(());
        }
        let values = match message {
            SolveAssertionMessage::NoCaptures => Vec::new(),
            SolveAssertionMessage::Captures { program } => {
                let arguments = captures
                    .iter()
                    .map(|capture| self.read_owned(*capture, provenance))
                    .collect::<Result<Vec<_>, _>>()?;
                let completed = eval_region(
                    self.table,
                    program,
                    arguments,
                    &mut FrameStorage::default(),
                    self.mode,
                    self.chain,
                    self.owner,
                )?;
                let Some(values) = self.accept_completion(completed) else {
                    return Ok(());
                };
                values
            }
        };
        let owner = self
            .table
            .owner(self.owner)
            .ok_or(TypedProgramEvalError::UnknownOwner { owner: self.owner })?;
        let (outputs, source_predicate) = match self.mode {
            EvaluationMode::Primal => (owner.outputs(), predicate_output),
            EvaluationMode::Directional => {
                let directional = owner.directional().ok_or(invalid_error(
                    "assertion has no directional owner",
                    provenance,
                ))?;
                let source = owner
                    .directional_primal_output_index(predicate_output)
                    .ok_or(invalid_error(
                        "assertion has no source predicate output",
                        provenance,
                    ))?;
                (directional.outputs(), source)
            }
        };
        // Construction binds every region result to this exact output index.
        let captures = message_outputs
            .iter()
            .copied()
            .enumerate()
            .map(|(offset, output)| (output, values[offset].clone()))
            .collect();
        self.observations.push(ObservedAssertion {
            observation: AssertionObservation {
                owner: self.owner,
                predicate_output: source_predicate,
                provenance,
                captures,
            },
            invocation_path: vec![AssertionFrame {
                owner: self.owner,
                program: self.program,
                operation: self.operation,
                domain_point: None,
                invocation: None,
            }],
        });
        if outputs[predicate_output].assertion_level() == Some(SolveAssertionLevel::Error) {
            self.stopped = Some(AssertionStop {
                observations: std::mem::take(&mut self.observations),
            });
            return Ok(());
        }
        self.transfer_call_outputs(values, destinations, provenance)
    }
}
