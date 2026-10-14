//! Evaluation-only owners retain no assignment certificates or queries.
use super::*;
use rumoca_eval_solve::reverse::{ReverseCotangents, ReverseInputs, ReverseScratch};

#[derive(Clone, Copy)]
pub(in crate::runtime::solve_runtime) struct EvaluationAnalysis<'source> {
    source: &'source PreparedEvaluationBlock,
}

impl EvaluationAnalysis<'_> {
    pub(in crate::runtime::solve_runtime) fn requirements(
        &self,
    ) -> solve_eval::RowInputRequirements {
        self.source.requirements()
    }
}

#[derive(Clone)]
pub(in crate::runtime::solve_runtime) struct EvaluationPrimal {
    source: PreparedEvaluationBlock,
    execution: PrimalExecution,
}

impl EvaluationPrimal {
    pub(in crate::runtime::solve_runtime) fn prepare(
        label: &'static str,
        canonical: solve::ScalarProgramBlock,
        compute: Option<&solve::ComputeBlock>,
        backend: Option<&Rc<dyn SolveExecutionBackend>>,
        primary: Option<&Self>,
    ) -> Result<Self, EvalSolveError> {
        let primary = primary.filter(|owner| same_primal_backend(&owner.execution, backend));
        let base = primary.map(|owner| &owner.source);
        let reuse = BlockReuse::of_programs(base.map(PreparedEvaluationBlock::block), &canonical);
        let execution = match backend {
            None => PrimalExecution::Reference {
                compute: compute
                    .map(|compute| {
                        PreparedComputeBlock::new_with_label(compute, label).map(Box::new)
                    })
                    .transpose()?,
            },
            Some(backend) => {
                let code = primary.and_then(|owner| match &owner.execution {
                    PrimalExecution::Reference { .. } => None,
                    PrimalExecution::Compiled { code, .. } => Some(code),
                });
                let prepared = reuse.expression_with_fresh(
                    code,
                    &canonical,
                    &mut || match compute {
                        Some(compute) => selected_programs::compute_expression(
                            backend.as_ref(),
                            label,
                            compute,
                            &canonical,
                        ),
                        None => selected_programs::expression(backend.as_ref(), label, &canonical),
                    },
                    &mut |source| selected_programs::expression(backend.as_ref(), label, source),
                )?;
                PrimalExecution::Compiled {
                    backend: Rc::clone(backend),
                    code: prepared,
                }
            }
        };
        Ok(Self {
            source: reuse.prepared_evaluation(base, canonical)?,
            execution,
        })
    }

    pub(in crate::runtime::solve_runtime) fn eval(
        &self,
        point: RowEvalPoint<'_>,
        context: RowEvalContext<'_>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        match &self.execution {
            PrimalExecution::Reference {
                compute: Some(compute),
            } => compute
                .eval_with_context(point.y, point.p, point.t, context, out)
                .map_err(Into::into),
            PrimalExecution::Reference { compute: None } => self
                .source
                .eval_with_context(point.y, point.p, point.t, context, out)
                .map_err(Into::into),
            PrimalExecution::Compiled { code, .. } => code.call(
                point.y,
                point.p,
                point.t,
                context.external_tables.unwrap_or(&[]),
                out,
            ),
        }
    }

    pub(in crate::runtime::solve_runtime) fn reference(&self) -> Option<ReferenceEvaluation<'_>> {
        match &self.execution {
            PrimalExecution::Reference { .. } => Some(ReferenceEvaluation {
                source: &self.source,
            }),
            PrimalExecution::Compiled { .. } => None,
        }
    }
}

fn same_primal_backend(
    execution: &PrimalExecution,
    requested: Option<&Rc<dyn SolveExecutionBackend>>,
) -> bool {
    match (execution, requested) {
        (PrimalExecution::Reference { .. }, None) => true,
        (PrimalExecution::Compiled { backend, .. }, Some(requested)) => {
            Rc::ptr_eq(backend, requested)
        }
        _ => false,
    }
}

pub(in crate::runtime::solve_runtime) struct ReferenceEvaluation<'source> {
    source: &'source PreparedEvaluationBlock,
}

impl ReferenceEvaluation<'_> {
    pub(in crate::runtime::solve_runtime) fn reverse_vjp(
        &self,
        inputs: &ReverseInputs<'_>,
        output_cotangents: &[f64],
        cotangents: &mut ReverseCotangents<'_>,
        scratch: &mut ReverseScratch,
    ) -> Result<(), RuntimeSolveError> {
        self.source
            .reverse_vjp(inputs, output_cotangents, cotangents, scratch)
            .map_err(Into::into)
    }
}

#[derive(Clone)]
pub(in crate::runtime::solve_runtime) struct EvaluationDirectional {
    source: PreparedEvaluationBlock,
    execution: DirectionalExecution,
}

impl EvaluationDirectional {
    pub(in crate::runtime::solve_runtime) fn prepare(
        label: &str,
        canonical: solve::ScalarProgramBlock,
        compute: Option<&solve::ComputeBlock>,
        backend: Option<&Rc<dyn SolveExecutionBackend>>,
        primary: Option<&Self>,
    ) -> Result<Self, EvalSolveError> {
        let primary = primary.filter(|owner| same_directional_backend(&owner.execution, backend));
        let base = primary.map(|owner| &owner.source);
        let reuse = BlockReuse::of_programs(base.map(PreparedEvaluationBlock::block), &canonical);
        let execution = match backend {
            None => DirectionalExecution::Reference,
            Some(backend) => {
                let code = primary.and_then(|owner| match &owner.execution {
                    DirectionalExecution::Reference => None,
                    DirectionalExecution::Compiled { code, .. } => Some(code),
                });
                let prepared = reuse.jacobian_with_fresh(
                    code,
                    &canonical,
                    &mut || match compute {
                        Some(compute) => selected_programs::compute_jacobian(
                            backend.as_ref(),
                            label,
                            compute,
                            &canonical,
                        ),
                        None => selected_programs::jacobian(backend.as_ref(), label, &canonical),
                    },
                    &mut |source| selected_programs::jacobian(backend.as_ref(), label, source),
                )?;
                DirectionalExecution::Compiled {
                    backend: Rc::clone(backend),
                    code: prepared,
                }
            }
        };
        Ok(Self {
            source: reuse.prepared_evaluation(base, canonical)?,
            execution,
        })
    }

    pub(in crate::runtime::solve_runtime) fn source(&self) -> EvaluationAnalysis<'_> {
        EvaluationAnalysis {
            source: &self.source,
        }
    }

    pub(in crate::runtime::solve_runtime) fn eval(
        &self,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        context: RowEvalContext<'_>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        match &self.execution {
            DirectionalExecution::Reference => self
                .source
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
            DirectionalExecution::Compiled { code, .. } => code.call(
                inputs.y,
                inputs.p,
                inputs.t,
                inputs.seed,
                context.external_tables.unwrap_or(&[]),
                out,
            ),
        }
    }
}

fn same_directional_backend(
    execution: &DirectionalExecution,
    requested: Option<&Rc<dyn SolveExecutionBackend>>,
) -> bool {
    match (execution, requested) {
        (DirectionalExecution::Reference, None) => true,
        (DirectionalExecution::Compiled { backend, .. }, Some(requested)) => {
            Rc::ptr_eq(backend, requested)
        }
        _ => false,
    }
}
