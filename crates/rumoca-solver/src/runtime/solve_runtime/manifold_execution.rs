use super::*;
use selected_programs::{CompleteExpression, CompleteJacobian};

/// Both execution forms retain the same constraint programs and output order.
#[derive(Clone)]
pub(super) struct PreparedManifoldProjection {
    residual: PreparedComputeBlock,
    directional: PreparedComputeBlock,
    directional_scalar: PreparedScalarProgramBlock,
    output_scratch: RefCell<Vec<f64>>,
    compiled_residual: Option<Rc<CompleteExpression>>,
    compiled_directional: Option<Rc<CompleteJacobian>>,
}

impl PreparedManifoldProjection {
    pub(super) fn new(
        model: &solve::SolveModel,
        backend: Option<&dyn SolveExecutionBackend>,
    ) -> Result<Self, EvalSolveError> {
        let residual = &model.problem.continuous.manifold_residual;
        let directional = &model.artifacts.continuous.manifold_jacobian_v;
        let compiled = backend.filter(|_| !residual.is_empty());
        let compiled_residual = compiled
            .map(|backend| {
                let source = to_scalar_program_block(residual)?;
                let whole = required_compiled(
                    "manifold_residual",
                    source.program_spans(),
                    backend.compile_expression(&source),
                )?;
                selected_programs::expression_from_whole(
                    backend,
                    "manifold_residual",
                    &source,
                    whole,
                )
            })
            .transpose()?;
        let compiled_directional = compiled
            .map(|backend| {
                let source = to_scalar_program_block(directional)?;
                let whole = required_compiled(
                    "manifold_jacobian_v",
                    source.program_spans(),
                    backend.compile_jacobian_expression(&source),
                )?;
                selected_programs::jacobian_from_whole(
                    backend,
                    "manifold_jacobian_v",
                    &source,
                    whole,
                )
            })
            .transpose()?;
        Ok(Self {
            residual: PreparedComputeBlock::new_with_label(residual, "runtime_manifold_residual")?,
            directional: PreparedComputeBlock::new_with_label(
                directional,
                "runtime_manifold_jacobian_v",
            )?,
            directional_scalar: PreparedScalarProgramBlock::new(to_scalar_program_block(
                directional,
            )?)?,
            output_scratch: RefCell::default(),
            compiled_residual,
            compiled_directional,
        })
    }

    pub(super) fn len(&self) -> usize {
        self.residual.len()
    }

    pub(super) fn eval_selected_directional(
        &self,
        selection: &solve::ProjectionOutputSelection,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        context: RowEvalContext<'_>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if out.len() != selection.output_len() {
            return Err(RuntimeSolveError::solve_ir(
                "manifold output extent mismatch",
            ));
        }
        out.fill(0.0);
        let mut values = self.output_scratch.borrow_mut();
        for program in selection.programs() {
            self.eval_program_outputs(program.program(), inputs, context, &mut values)?;
            if values.len() != program.output_count() {
                return Err(RuntimeSolveError::solve_ir(
                    "manifold program output count mismatch",
                ));
            }
            for &(offset, row) in program.placements() {
                out[row] = values[offset];
            }
        }
        Ok(())
    }

    fn eval_program_outputs(
        &self,
        program: usize,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        context: RowEvalContext<'_>,
        out: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        if let Some(compiled) = &self.compiled_directional {
            return compiled.call_program_outputs(
                program,
                inputs,
                context.external_tables.unwrap_or(&[]),
                out,
            );
        }
        self.directional_scalar
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
            .map_err(Into::into)
    }

    pub(super) fn eval_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if let Some(compiled) = &self.compiled_residual {
            return compiled.call(y, p, t, context.external_tables.unwrap_or(&[]), out);
        }
        self.residual
            .eval_with_context(y, p, t, context, out)
            .map_err(Into::into)
    }

    pub(super) fn eval_directional(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if let Some(compiled) = &self.compiled_directional {
            return compiled.call(y, p, t, seed, context.external_tables.unwrap_or(&[]), out);
        }
        self.directional
            .eval_with_context(
                y,
                p,
                t,
                RowEvalContext {
                    seed: Some(seed),
                    ..context
                },
                out,
            )
            .map_err(Into::into)
    }
}
