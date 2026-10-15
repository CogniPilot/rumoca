//! Source-bound numeric programs of ordinary event messages.

use super::*;
use solve_eval::{EventMessageProgramExecution, EventMessageProgramInput, EventMessageProgramKind};

type PartPrograms = BTreeMap<EventMessageProgramKind, Rc<dyn CompiledSolveExpression>>;

#[derive(Clone)]
pub(super) struct CompiledMessagePrograms {
    execute: Rc<EventMessageProgramExecution>,
}

impl CompiledMessagePrograms {
    pub(super) fn new(
        owner: SolveRuntimeModel,
        backend: &dyn SolveExecutionBackend,
    ) -> Result<Self, EvalSolveError> {
        let programs = owner
            .problem
            .events
            .actions
            .iter()
            .enumerate()
            .map(|(index, action)| prepare_action(index, action, backend))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self {
            execute: Rc::new(move |input| evaluate(&owner, &programs, input)),
        })
    }

    pub(super) fn execution(&self) -> &EventMessageProgramExecution {
        self.execute.as_ref()
    }
}

fn prepare_action(
    index: usize,
    action: &solve::SolveEventAction,
    backend: &dyn SolveExecutionBackend,
) -> Result<Vec<PartPrograms>, EvalSolveError> {
    // Projected messages come from the checked invocation's compiled outputs.
    if action.assertion_projection.is_some() {
        return Ok(Vec::new());
    }
    action
        .message
        .parts
        .iter()
        .enumerate()
        .map(|(part, source)| prepare_part(index, part, action, source, backend))
        .collect()
}

fn prepare_part(
    index: usize,
    part: usize,
    action: &solve::SolveEventAction,
    source: &solve::SolveEventMessagePart,
    backend: &dyn SolveExecutionBackend,
) -> Result<PartPrograms, EvalSolveError> {
    let solve::SolveEventMessagePart::Conversion { value, format, .. } = source else {
        return Ok(BTreeMap::new());
    };
    let solve::SolveStringConversionFormat::Options {
        minimum_length,
        left_justified,
        significant_digits,
    } = format;
    let mut programs = BTreeMap::new();
    for (kind, row) in [
        (EventMessageProgramKind::Value, Some(value)),
        (
            EventMessageProgramKind::MinimumLength,
            minimum_length.as_ref(),
        ),
        (
            EventMessageProgramKind::LeftJustified,
            left_justified.as_ref(),
        ),
        (
            EventMessageProgramKind::SignificantDigits,
            significant_digits.as_ref(),
        ),
    ] {
        // Absent authored options use the language's formatting defaults.
        let Some(row) = row else {
            continue;
        };
        let label = format!("event_messages[{index}].parts[{part}].{kind:?}");
        programs.insert(kind, compile_program(row, action.span, &label, backend)?);
    }
    Ok(programs)
}

fn compile_program(
    row: &[solve::LinearOp],
    span: rumoca_core::Span,
    label: &str,
    backend: &dyn SolveExecutionBackend,
) -> Result<Rc<dyn CompiledSolveExpression>, EvalSolveError> {
    let source = solve::ScalarProgramBlock::with_program_spans(vec![row.to_vec()], vec![span])
        .map_err(|error| EvalSolveError::CompiledExecution {
            message: error.to_string(),
            span: Some(span),
        })?;
    if source.output_count() != 1 {
        return Err(EvalSolveError::CompiledExecution {
            message: "numeric message program must own exactly one output".into(),
            span: Some(span),
        });
    }
    backend
        .compile_expression(&source)
        .map_err(|error| EvalSolveError::CompiledExecution {
            message: format!("required compiled program `{label}` could not be lowered: {error}"),
            span: Some(span),
        })
}

fn evaluate(
    owner: &SolveRuntimeModel,
    programs: &[Vec<PartPrograms>],
    input: EventMessageProgramInput<'_>,
) -> Result<f64, EvalSolveError> {
    let source_error = || EvalSolveError::CompiledExecution {
        message: "numeric message request does not belong to its prepared source".into(),
        span: Some(input.action.span),
    };
    let action = owner
        .problem
        .events
        .actions
        .iter()
        .position(|action| std::ptr::eq(action, input.action))
        .ok_or_else(source_error)?;
    let code = programs
        .get(action)
        .and_then(|parts| parts.get(input.part))
        .and_then(|part| part.get(&input.kind))
        .ok_or_else(source_error)?;
    let mut output = [0.0];
    code.call(
        input.y,
        input.p,
        input.t,
        owner.external_tables.as_slice(),
        &mut output,
    )
    .map_err(|error| match error {
        RuntimeSolveError::SourceFault { message, span } => {
            EvalSolveError::SourceOperationFault { message, span }
        }
        error => EvalSolveError::CompiledExecution {
            span: error.source_span().or(Some(input.action.span)),
            message: error.to_string(),
        },
    })?;
    Ok(output[0])
}

impl SolveRuntime {
    pub(super) fn message_program_execution(&self) -> Option<&EventMessageProgramExecution> {
        match &self.execution {
            RuntimeExecutionInventory::Scalar(scalar) => match &scalar.expressions {
                PreparedExpressions::Compiled(code) => Some(code.messages.execution()),
                PreparedExpressions::Reference => None,
            },
            #[cfg(feature = "native-wasm")]
            RuntimeExecutionInventory::Native(_) => Some(&native_message_requires_observation),
        }
    }
}

#[cfg(feature = "native-wasm")]
fn native_message_requires_observation(
    input: EventMessageProgramInput<'_>,
) -> Result<f64, EvalSolveError> {
    Err(EvalSolveError::CompiledExecution {
        message: "retained native messages require their checked invocation observation".into(),
        span: Some(input.action.span),
    })
}
