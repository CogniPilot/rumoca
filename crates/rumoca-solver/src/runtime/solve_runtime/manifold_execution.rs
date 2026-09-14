use super::*;

/// Both execution forms retain the same constraint programs and output order.
#[derive(Clone)]
pub(super) struct PreparedManifoldProjection {
    residual: PreparedComputeBlock,
    directional: PreparedComputeBlock,
    directional_scalar: PreparedScalarProgramBlock,
    output_scratch: RefCell<Vec<f64>>,
    compiled_residual: Option<Rc<dyn CompiledSolveExpression>>,
    compiled_directional: Option<Rc<dyn CompiledSolveJacobianExpression>>,
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
                Ok::<_, EvalSolveError>(optional_compiled(
                    "manifold_residual",
                    backend.compile_expression(&to_scalar_program_block(residual)?),
                ))
            })
            .transpose()?
            .flatten();
        let compiled_directional = compiled
            .map(|backend| {
                Ok::<_, EvalSolveError>(optional_compiled(
                    "manifold_jacobian_v",
                    backend.compile_jacobian_expression(&to_scalar_program_block(directional)?),
                ))
            })
            .transpose()?
            .flatten();
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
        selection: &solve::JacobianOutputSelection,
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
        if let Some(compiled) = &self.compiled_directional
            && compiled
                .call_program_outputs(program, inputs, context.external_tables.unwrap_or(&[]), out)
                .map_err(RuntimeSolveError::solve_ir)?
        {
            return Ok(());
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
            return compiled
                .call(y, p, t, context.external_tables.unwrap_or(&[]), out)
                .map_err(RuntimeSolveError::solve_ir);
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
            return compiled
                .call(y, p, t, seed, context.external_tables.unwrap_or(&[]), out)
                .map_err(RuntimeSolveError::solve_ir);
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
