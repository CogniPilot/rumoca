//! Source-bound scalar execution with an explicit reference engine.
use super::*;
mod analysis;
mod evaluation;
mod reference;
pub(super) use analysis::ProgramAnalysis;
pub(super) use evaluation::{EvaluationDirectional, EvaluationPrimal};
use selected_programs::{CompleteExpression, CompleteJacobian};

#[derive(Clone)]
pub(super) struct PrimalProgram {
    source: PreparedScalarProgramBlock,
    execution: PrimalExecution,
}

#[derive(Clone)]
enum PrimalExecution {
    Reference {
        compute: Option<Box<PreparedComputeBlock>>,
    },
    Compiled {
        backend: Rc<dyn SolveExecutionBackend>,
        code: Rc<CompleteExpression>,
    },
}

impl PrimalProgram {
    fn same_backend(&self, requested: Option<&Rc<dyn SolveExecutionBackend>>) -> bool {
        match (&self.execution, requested) {
            (PrimalExecution::Reference { .. }, None) => true,
            (PrimalExecution::Compiled { backend, .. }, Some(requested)) => {
                Rc::ptr_eq(backend, requested)
            }
            _ => false,
        }
    }

    pub(super) fn prepare(
        label: &str,
        source: solve::ScalarProgramBlock,
        backend: Option<&Rc<dyn SolveExecutionBackend>>,
        primary: Option<&Self>,
    ) -> Result<Self, EvalSolveError> {
        let primary = primary.filter(|owner| owner.same_backend(backend));
        let base = primary.map(|owner| &owner.source);
        let reuse = BlockReuse::of(base, &source);
        let execution = match backend {
            None => PrimalExecution::Reference { compute: None },
            Some(backend) => {
                let code = primary.and_then(|owner| match &owner.execution {
                    PrimalExecution::Reference { .. } => None,
                    PrimalExecution::Compiled { code, .. } => Some(code),
                });
                let prepared = reuse.expression(code, &source, &mut |source| {
                    selected_programs::expression(backend.as_ref(), label, source)
                })?;
                PrimalExecution::Compiled {
                    backend: Rc::clone(backend),
                    code: prepared,
                }
            }
        };
        Ok(Self {
            source: reuse.prepared(base, source)?,
            execution,
        })
    }

    pub(super) fn prepare_compact(
        label: &'static str,
        canonical: solve::ScalarProgramBlock,
        compute: &solve::ComputeBlock,
        backend: Option<&Rc<dyn SolveExecutionBackend>>,
        primary: Option<&Self>,
    ) -> Result<Self, EvalSolveError> {
        let primary = primary.filter(|owner| owner.same_backend(backend));
        let base = primary.map(|owner| &owner.source);
        let reuse = BlockReuse::of(base, &canonical);
        let execution = match backend {
            None => PrimalExecution::Reference {
                compute: Some(Box::new(PreparedComputeBlock::new_with_label(
                    compute, label,
                )?)),
            },
            Some(backend) => {
                let code = primary.and_then(|owner| match &owner.execution {
                    PrimalExecution::Reference { .. } => None,
                    PrimalExecution::Compiled { code, .. } => Some(code),
                });
                let code = reuse.expression_with_fresh(
                    code,
                    &canonical,
                    &mut || {
                        selected_programs::compute_expression(
                            backend.as_ref(),
                            label,
                            compute,
                            &canonical,
                        )
                    },
                    &mut |source| selected_programs::expression(backend.as_ref(), label, source),
                )?;
                PrimalExecution::Compiled {
                    backend: Rc::clone(backend),
                    code,
                }
            }
        };
        Ok(Self {
            source: reuse.prepared(base, canonical)?,
            execution,
        })
    }

    pub(super) fn prepare_with_compute(
        label: &'static str,
        canonical: solve::ScalarProgramBlock,
        compute: &solve::ComputeBlock,
        backend: Option<&Rc<dyn SolveExecutionBackend>>,
        primary: Option<&Self>,
    ) -> Result<Self, EvalSolveError> {
        let mut owner = Self::prepare(label, canonical, backend, primary)?;
        if let PrimalExecution::Reference { compute: prepared } = &mut owner.execution {
            let base = primary.and_then(|primary| match &primary.execution {
                PrimalExecution::Reference { compute } => compute.as_deref(),
                PrimalExecution::Compiled { .. } => None,
            });
            *prepared = Some(Box::new(match base {
                Some(base) => PreparedComputeBlock::with_shared_programs(base, compute, label)?,
                None => PreparedComputeBlock::new_with_label(compute, label)?,
            }));
        }
        Ok(owner)
    }

    pub(super) fn replaced_outputs(
        primary: Option<&Self>,
        canonical: &solve::ScalarProgramBlock,
    ) -> Result<BTreeSet<usize>, EvalSolveError> {
        BlockReuse::of(primary.map(|owner| &owner.source), canonical).replaced_outputs(canonical)
    }

    pub(super) fn source(&self) -> ProgramAnalysis<'_> {
        ProgramAnalysis::new(&self.source)
    }

    pub(super) fn is_compiled(&self) -> bool {
        matches!(self.execution, PrimalExecution::Compiled { .. })
    }

    pub(super) fn eval(
        &self,
        point: RowEvalPoint<'_>,
        context: RowEvalContext<'_>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        match &self.execution {
            PrimalExecution::Reference { .. } => self
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

    pub(super) fn eval_compact(
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
            _ => self.eval(point, context, out),
        }
    }

    pub(super) fn eval_output(
        &self,
        coordinate: (usize, usize),
        point: RowEvalPoint<'_>,
        context: RowEvalContext<'_>,
    ) -> Result<f64, RuntimeSolveError> {
        match &self.execution {
            PrimalExecution::Reference { .. } => self
                .source
                .eval_row_output_unchecked_with_context(
                    coordinate.0,
                    coordinate.1,
                    point.y,
                    point.p,
                    point.t,
                    context,
                )
                .map_err(Into::into),
            PrimalExecution::Compiled { code, .. } => code.call_program_output(
                coordinate,
                point.y,
                point.p,
                point.t,
                context.external_tables.unwrap_or(&[]),
            ),
        }
    }

    pub(super) fn eval_program_outputs(
        &self,
        program: usize,
        point: RowEvalPoint<'_>,
        context: RowEvalContext<'_>,
        out: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        match &self.execution {
            PrimalExecution::Reference { .. } => self
                .source
                .eval_row_outputs_unchecked_with_context(
                    program, point.y, point.p, point.t, context, out,
                )
                .map_err(Into::into),
            PrimalExecution::Compiled { code, .. } => code.call_program_outputs(
                program,
                point.y,
                point.p,
                point.t,
                context.external_tables.unwrap_or(&[]),
                out,
            ),
        }
    }

    pub(super) fn eval_outputs_at(
        &self,
        coordinates: &[(usize, usize)],
        point: RowEvalPoint<'_>,
        context: RowEvalContext<'_>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        match &self.execution {
            PrimalExecution::Reference { .. } => {
                reference_bulk(&self.source, coordinates, point, context, out)
            }
            PrimalExecution::Compiled { code, .. } => code.call_program_outputs_at(
                coordinates,
                (point.y, point.p, point.t),
                context.external_tables.unwrap_or(&[]),
                out,
            ),
        }
    }
}

#[derive(Clone)]
pub(super) struct DirectionalProgram {
    source: PreparedScalarProgramBlock,
    execution: DirectionalExecution,
}

#[derive(Clone)]
enum DirectionalExecution {
    Reference,
    Compiled {
        backend: Rc<dyn SolveExecutionBackend>,
        code: Rc<CompleteJacobian>,
    },
}

impl DirectionalProgram {
    fn same_backend(&self, requested: Option<&Rc<dyn SolveExecutionBackend>>) -> bool {
        match (&self.execution, requested) {
            (DirectionalExecution::Reference, None) => true,
            (DirectionalExecution::Compiled { backend, .. }, Some(requested)) => {
                Rc::ptr_eq(backend, requested)
            }
            _ => false,
        }
    }

    pub(super) fn prepare(
        label: &str,
        source: solve::ScalarProgramBlock,
        backend: Option<&Rc<dyn SolveExecutionBackend>>,
        primary: Option<&Self>,
    ) -> Result<Self, EvalSolveError> {
        let primary = primary.filter(|owner| owner.same_backend(backend));
        let base = primary.map(|owner| &owner.source);
        let reuse = BlockReuse::of(base, &source);
        let execution = match backend {
            None => DirectionalExecution::Reference,
            Some(backend) => {
                let code = primary.and_then(|owner| match &owner.execution {
                    DirectionalExecution::Reference => None,
                    DirectionalExecution::Compiled { code, .. } => Some(code),
                });
                let prepared = reuse.jacobian(code, &source, &mut |source| {
                    selected_programs::jacobian(backend.as_ref(), label, source)
                })?;
                DirectionalExecution::Compiled {
                    backend: Rc::clone(backend),
                    code: prepared,
                }
            }
        };
        Ok(Self {
            source: reuse.prepared(base, source)?,
            execution,
        })
    }

    pub(super) fn replaced_outputs(
        primary: Option<&Self>,
        canonical: &solve::ScalarProgramBlock,
    ) -> Result<BTreeSet<usize>, EvalSolveError> {
        BlockReuse::of(primary.map(|owner| &owner.source), canonical).replaced_outputs(canonical)
    }

    pub(super) fn source(&self) -> ProgramAnalysis<'_> {
        ProgramAnalysis::new(&self.source)
    }

    pub(super) fn is_compiled(&self) -> bool {
        matches!(self.execution, DirectionalExecution::Compiled { .. })
    }

    pub(super) fn eval(
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

    pub(super) fn eval_output(
        &self,
        coordinate: (usize, usize),
        inputs: solve_eval::JacobianEvalInputs<'_>,
        context: RowEvalContext<'_>,
    ) -> Result<f64, RuntimeSolveError> {
        match &self.execution {
            DirectionalExecution::Reference => self
                .source
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
            DirectionalExecution::Compiled { code, .. } => code.call_program_output(
                coordinate,
                inputs.y,
                inputs.p,
                inputs.t,
                inputs.seed,
                context.external_tables.unwrap_or(&[]),
            ),
        }
    }

    pub(super) fn eval_program_outputs(
        &self,
        program: usize,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        context: RowEvalContext<'_>,
        out: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        match &self.execution {
            DirectionalExecution::Reference => self
                .source
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
            DirectionalExecution::Compiled { code, .. } => code.call_program_outputs(
                program,
                inputs,
                context.external_tables.unwrap_or(&[]),
                out,
            ),
        }
    }

    pub(super) fn prepare_projection_entries(
        &self,
        applications: &[&solve::ProjectionJacobianApplication],
    ) -> Result<Vec<Option<Rc<dyn CompiledSolveProjectionJacobian>>>, String> {
        match &self.execution {
            DirectionalExecution::Reference => Ok(vec![None; applications.len()]),
            DirectionalExecution::Compiled { code, .. } => code.prepare_projections(applications),
        }
    }
}

fn reference_bulk(
    source: &PreparedScalarProgramBlock,
    coordinates: &[(usize, usize)],
    point: RowEvalPoint<'_>,
    context: RowEvalContext<'_>,
    out: &mut [f64],
) -> Result<(), RuntimeSolveError> {
    if coordinates.len() != out.len() {
        return Err(RuntimeSolveError::solve_ir(
            "bulk reference output width does not match coordinates",
        ));
    }
    let mut groups = BTreeMap::<usize, Vec<(usize, usize)>>::new();
    for (destination, &(program, offset)) in coordinates.iter().enumerate() {
        if source
            .row_output_count(program)
            .is_none_or(|count| offset >= count)
        {
            return Err(RuntimeSolveError::solve_ir(
                "bulk reference coordinate is outside its source",
            ));
        }
        groups
            .entry(program)
            .or_default()
            .push((destination, offset));
    }
    let mut staged = Vec::new();
    resize_runtime_values(&mut staged, out.len(), 0.0, "reference bulk transaction")?;
    let mut values = Vec::new();
    for (program, placements) in groups {
        source.eval_row_outputs_unchecked_with_context(
            program,
            point.y,
            point.p,
            point.t,
            context,
            &mut values,
        )?;
        for (destination, offset) in placements {
            staged[destination] = values[offset];
        }
    }
    out.copy_from_slice(&staged);
    Ok(())
}
