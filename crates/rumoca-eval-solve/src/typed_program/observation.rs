//! Consume private observations through one checked borrowed invocation.

mod formatting;

use super::*;
use crate::EvalSolveError;
use rumoca_ir_solve::{CheckedAssertionInvocation, SolveEventActionKind};

/// Detached diagnostic data. It grants no invocation or action membership.
#[derive(Debug, PartialEq)]
pub struct AssertionReport {
    pub action_index: usize,
    pub kind: SolveEventActionKind,
    pub message: String,
    pub span: Span,
}

pub enum AssertionInvocationEvaluation {
    Complete {
        values: Vec<TypedValue>,
        reports: Vec<AssertionReport>,
    },
    Failed {
        reports: Vec<AssertionReport>,
    },
    Fault {
        reports: Vec<AssertionReport>,
        error: EvalSolveError,
    },
}

/// The only observation adapter accepts the checked immutable construction,
/// invokes its exact owner once and consumes that invocation's private stop.
pub fn eval_assertion_invocation(
    invocation: &CheckedAssertionInvocation<'_>,
    arguments: &[TypedValue],
) -> Result<AssertionInvocationEvaluation, EvalSolveError> {
    let owner = invocation.owner();
    let chain = RecursionChain::ROOT
        .enter(invocation.table(), owner.id(), owner.provenance())
        .map_err(evaluation_error)?;
    let completion =
        eval_owner(invocation.table(), owner, arguments, chain).map_err(evaluation_error)?;
    let (values, observations, fault) = match completion {
        InvocationCompletion::Complete {
            values,
            observations,
        } => (Some(values), observations, None),
        InvocationCompletion::Stopped(stop) => (None, stop.observations, None),
        InvocationCompletion::Fault {
            error,
            observations,
        } => (None, observations, Some(error)),
    };
    let mut reports = Vec::new();
    for observed in observations {
        let mapped = map_observation(invocation, &observed, arguments)?;
        for projection in invocation
            .projections()
            .iter()
            .filter(|projection| projection.predicate_output() == mapped.predicate_output)
        {
            if projection.action().span != mapped.provenance {
                return Err(binding_error(
                    "assertion action changed its source provenance",
                ));
            }
            reports.push(AssertionReport {
                action_index: projection.action_index(),
                kind: projection.action().kind,
                message: formatting::render_message(projection, &mapped)?,
                span: mapped.provenance,
            });
        }
    }
    if let Some(error) = fault {
        return Ok(AssertionInvocationEvaluation::Fault {
            reports,
            error: evaluation_error(error),
        });
    }
    match values {
        Some(values) => Ok(AssertionInvocationEvaluation::Complete { values, reports }),
        None if reports
            .last()
            .is_some_and(|report| report.kind == SolveEventActionKind::Assert) =>
        {
            Ok(AssertionInvocationEvaluation::Failed { reports })
        }
        None => Err(binding_error(
            "fatal assertion has no checked action projection",
        )),
    }
}

fn evaluation_error(error: TypedProgramEvalError) -> EvalSolveError {
    EvalSolveError::ShapeContract {
        message: error.to_string(),
        span: error.source_span(),
    }
}

fn binding_error(message: &'static str) -> EvalSolveError {
    EvalSolveError::ShapeContract {
        message: message.to_owned(),
        span: None,
    }
}

fn map_observation(
    invocation: &CheckedAssertionInvocation<'_>,
    observed: &ObservedAssertion<'_>,
    arguments: &[TypedValue],
) -> Result<assertions::AssertionObservation, EvalSolveError> {
    let outer = observed
        .invocation_path
        .last()
        .ok_or_else(|| binding_error("assertion observation has no source invocation"))?;
    let called = outer
        .invocation
        .as_ref()
        .ok_or_else(|| binding_error("assertion observation lost its invocation arguments"))?;
    if !std::ptr::eq(called.owner, invocation.owner())
        || !std::ptr::eq(outer.program, invocation.owner().body())
        || called.arguments.as_ref() != arguments
    {
        return Err(binding_error(
            "assertion observation belongs to a different invocation",
        ));
    }
    let mut mapped = observed.observation.clone();
    for frame in observed.invocation_path.iter().skip(1) {
        let op = frame
            .program
            .operations()
            .get(frame.operation)
            .ok_or_else(|| binding_error("assertion invocation path has no issuing operation"))?;
        let SolveOperation::Call {
            owner,
            assertion_forwarding,
            ..
        } = op.operation()
        else {
            if frame.owner == mapped.owner {
                continue;
            }
            return Err(binding_error(
                "assertion owner changed without an issued call",
            ));
        };
        if *owner != mapped.owner {
            return Err(binding_error(
                "assertion forwarding changed its child invocation",
            ));
        }
        let forwarding = assertion_forwarding
            .iter()
            .find(|forwarding| forwarding.child_predicate() == mapped.predicate_output)
            .ok_or_else(|| binding_error("assertion call has no checked forwarding"))?;
        remap_captures(
            invocation.table(),
            &mut mapped,
            frame.owner,
            forwarding.parent_predicate(),
        )?;
    }
    if mapped.owner != invocation.owner().id() {
        return Err(binding_error(
            "assertion path did not reach its checked source owner",
        ));
    }
    Ok(mapped)
}

fn remap_captures(
    table: &SolvePureCallTable,
    observation: &mut assertions::AssertionObservation,
    owner: SolvePureCallOwnerId,
    predicate_output: usize,
) -> Result<(), EvalSolveError> {
    let outputs = table
        .owner(owner)
        .ok_or_else(|| binding_error("assertion forwarding lost its parent owner"))?
        .outputs();
    let destinations: Vec<_> = outputs
        .iter()
        .enumerate()
        .filter_map(|(index, output)| {
            (output.message_predicate_output(index) == Some(predicate_output)).then_some(index)
        })
        .collect();
    if destinations.len() != observation.captures.len() {
        return Err(binding_error(
            "assertion forwarding changed its capture tuple",
        ));
    }
    for ((output, value), destination) in observation.captures.iter_mut().zip(destinations) {
        if value.value_type() != outputs[destination].value_type() {
            return Err(binding_error("assertion forwarding changed a capture type"));
        }
        *output = destination;
    }
    observation.owner = owner;
    observation.predicate_output = predicate_output;
    Ok(())
}
