//! Source-bound directional programs prepared before runtime admission.

use super::super::*;
use solve_eval::JacobianEvalInputs;

#[derive(Clone)]
pub(in super::super) struct CoupledEventJvps {
    pub(super) discrete: Option<PreparedJvp>,
    pub(super) runtime: Option<PreparedJvp>,
    pub(super) guarded: Option<PreparedJvp>,
    pub(super) structured: Option<PreparedJvp>,
}

impl CoupledEventJvps {
    pub(in super::super) fn prepare(
        artifacts: &solve::DiscreteSolveArtifacts,
        backend: Option<&dyn SolveExecutionBackend>,
    ) -> Result<Self, EvalSolveError> {
        let prepare = |label, source: &Option<solve::ScalarProgramBlock>| {
            source
                .as_ref()
                .map(|source| PreparedJvp::new(label, source, backend))
                .transpose()
        };
        Ok(Self {
            discrete: prepare("discrete_rhs_jvp", &artifacts.rhs_jacobian_v)?,
            runtime: prepare(
                "runtime_assignment_jvp",
                &artifacts.runtime_assignment_jacobian_v,
            )?,
            guarded: prepare("guarded_assignment_jvp", &artifacts.guarded_jacobian_v)?,
            structured: prepare("structured_discrete_jvp", &artifacts.structured_jacobian_v)?,
        })
    }
}

pub(super) fn required<'a>(
    block: &'a Option<PreparedJvp>,
    rows: &str,
) -> Result<&'a PreparedJvp, RuntimeSolveError> {
    block
        .as_ref()
        .ok_or_else(|| RuntimeSolveError::DirectionalDerivativeUnavailable {
            reason: format!("the {rows} have no directional derivative"),
        })
}

#[derive(Clone)]
pub(super) enum PreparedJvp {
    Reference(PreparedScalarProgramBlock),
    Compiled {
        source: PreparedScalarProgramBlock,
        code: Rc<dyn CompiledSolveJacobianExpression>,
    },
}

impl PreparedJvp {
    fn new(
        label: &str,
        source: &solve::ScalarProgramBlock,
        backend: Option<&dyn SolveExecutionBackend>,
    ) -> Result<Self, EvalSolveError> {
        let prepared = PreparedScalarProgramBlock::new(source.clone())?;
        let Some(backend) = backend else {
            return Ok(Self::Reference(prepared));
        };
        let whole = backend
            .compile_jacobian_expression(source)
            .map_err(|message| {
                let spans = source.program_spans();
                let span = spans
                    .first()
                    .copied()
                    .filter(|first| spans.iter().all(|span| span == first));
                EvalSolveError::CompiledExecution {
                    message: format!(
                        "required compiled program `{label}` could not be lowered: {message}"
                    ),
                    span,
                }
            })?;
        let code = selected_programs::jacobian(backend, label, source, whole)?;
        Ok(Self::Compiled {
            source: prepared,
            code,
        })
    }

    pub(super) fn source(&self) -> &PreparedScalarProgramBlock {
        match self {
            Self::Reference(source) | Self::Compiled { source, .. } => source,
        }
    }

    pub(super) fn eval(
        &self,
        inputs: JacobianEvalInputs<'_>,
        context: RowEvalContext<'_>,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        match self {
            Self::Compiled { source, code } => code
                .call(inputs.y, inputs.p, inputs.t, inputs.seed, tables, out)
                .map_err(|error| {
                    admitted_compiled_error(
                        "coupled event JVP",
                        source.block().program_spans(),
                        error,
                    )
                }),
            Self::Reference(source) => source
                .eval_with_context(
                    inputs.y,
                    inputs.p,
                    inputs.t,
                    RowEvalContext {
                        seed: Some(inputs.seed),
                        ..context
                    },
                    out,
                )
                .map_err(Into::into),
        }
    }

    pub(super) fn eval_program_outputs(
        &self,
        program: usize,
        inputs: JacobianEvalInputs<'_>,
        context: RowEvalContext<'_>,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        match self {
            Self::Compiled { code, .. } => {
                if !code.call_program_outputs(program, inputs, tables, out)? {
                    return Err(RuntimeSolveError::solve_ir(
                        "required coupled event JVP program is absent",
                    ));
                }
                Ok(())
            }
            Self::Reference(source) => source
                .eval_row_outputs_unchecked_with_context(
                    program,
                    inputs.y,
                    inputs.p,
                    inputs.t,
                    RowEvalContext {
                        seed: Some(inputs.seed),
                        ..context
                    },
                    out,
                )
                .map_err(Into::into),
        }
    }

    pub(super) fn eval_output(
        &self,
        coordinate: (usize, usize),
        inputs: JacobianEvalInputs<'_>,
        context: RowEvalContext<'_>,
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<f64, RuntimeSolveError> {
        match self {
            Self::Compiled { code, .. } => code
                .call_program_output(
                    coordinate,
                    inputs.y,
                    inputs.p,
                    inputs.t,
                    inputs.seed,
                    tables,
                )?
                .ok_or_else(|| {
                    RuntimeSolveError::solve_ir("required coupled event JVP output is absent")
                }),
            Self::Reference(source) => source
                .eval_row_output_unchecked_with_context(
                    coordinate.0,
                    coordinate.1,
                    inputs.y,
                    inputs.p,
                    inputs.t,
                    RowEvalContext {
                        seed: Some(inputs.seed),
                        ..context
                    },
                )
                .map_err(Into::into),
        }
    }
}
