use super::*;
use rumoca_ir_solve::ProjectionJacobianApplication;

#[cfg(test)]
mod reuse_tests;

/// A source-bound colored Jacobian with exclusively leased numeric storage.
pub struct CompiledProjectionJacobian {
    jit: Rc<CompiledJacobianRows>,
    batch: super::projection_batch::ProjectionBatch,
    application: ProjectionJacobianApplication,
    required_y_len: usize,
    validate: bool,
    scratch: RefCell<ProjectionScratch>,
}

struct ProjectionScratch {
    seed: Vec<f64>,
    program_output: Vec<f64>,
    matrix: Vec<f64>,
}

impl CompiledProjectionJacobian {
    pub(crate) fn new(
        jit: Rc<CompiledJacobianRows>,
        application: ProjectionJacobianApplication,
    ) -> Result<Self, CompileError> {
        let output_count = program_output_capacity(&jit, &application)?;
        let required_y_len = application
            .y_indices()
            .iter()
            .copied()
            .max()
            .map_or(Some(0), |index| index.checked_add(1))
            .ok_or_else(|| CompileError::Input("projection seed extent overflows".into()))?;
        let seed_len = required_y_len.max(jit.input_requirements.seed_len);
        let batch = super::projection_batch::ProjectionBatch::compile(
            &application,
            jit._pure_calls.as_deref(),
        )?;
        let validate = application.colors().iter().any(|color| {
            color.outputs().programs().iter().any(|program| {
                let row = &jit.rows[program.program()];
                row.interpreter_supported && should_validate_jit_row(row.validate_with_interpreter)
            })
        });
        let scratch = RefCell::new(ProjectionScratch {
            seed: vec![0.0; seed_len],
            program_output: vec![0.0; output_count],
            matrix: vec![0.0; application.output_len()],
        });
        Ok(Self {
            jit,
            batch,
            application,
            required_y_len,
            validate,
            scratch,
        })
    }

    pub fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        if out.len() != self.application.output_len() || y.len() < self.required_y_len {
            return Err(CompileError::Input(
                "projection Jacobian extent mismatch".into(),
            ));
        }
        let mut scratch = self.scratch.try_borrow_mut().map_err(|_| {
            CompileError::Input("projection Jacobian workspace is already in use".into())
        })?;
        validate_input_requirements(self.jit.input_requirements, y, p, Some(&scratch.seed))?;
        let mut registers = self.jit.regs_scratch.try_borrow_mut().map_err(|_| {
            CompileError::Input("Jacobian register workspace is already in use".into())
        })?;
        scratch.seed.fill(0.0);
        with_active_external_tables(external_tables, || {
            let expected = if self.validate {
                self.evaluate(&mut scratch, &mut registers, y, p, t, external_tables)?;
                Some(scratch.matrix.clone())
            } else {
                None
            };
            let ProjectionScratch { seed, matrix, .. } = &mut *scratch;
            self.batch.call(y, p, t, seed, matrix)?;
            self.jit.record_jit_call();
            if let Some(expected) = expected {
                validate_matrix(matrix, &expected)?;
            }
            Ok::<_, CompileError>(())
        })?;
        out.copy_from_slice(&scratch.matrix);
        Ok(())
    }

    fn evaluate(
        &self,
        scratch: &mut ProjectionScratch,
        registers: &mut Vec<f64>,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[ExternalTableData],
    ) -> Result<(), CompileError> {
        for color in self.application.colors() {
            for &index in color.seed_indices() {
                scratch.seed[index] = 1.0;
            }
            let context = JacobianCallContext {
                y,
                p,
                t,
                v: &scratch.seed,
                external_tables,
            };
            self.evaluate_color(
                color.outputs(),
                &mut scratch.program_output,
                &mut scratch.matrix,
                registers,
                &context,
            )?;
            for &index in color.seed_indices() {
                scratch.seed[index] = 0.0;
            }
        }
        Ok(())
    }
    fn evaluate_color(
        &self,
        selection: &rumoca_ir_solve::ProjectionOutputSelection,
        program_output: &mut [f64],
        matrix: &mut [f64],
        registers: &mut Vec<f64>,
        context: &JacobianCallContext<'_>,
    ) -> Result<(), CompileError> {
        for program in selection.programs() {
            let output = &mut program_output[..program.output_count()];
            self.jit.call_jacobian_row(
                &self.jit.rows[program.program()],
                registers,
                context,
                output,
            )?;
            for &(offset, target) in program.placements() {
                matrix[target] = output[offset];
            }
        }
        Ok(())
    }
}

fn validate_matrix(actual: &[f64], expected: &[f64]) -> Result<(), CompileError> {
    for (&actual, &expected) in actual.iter().zip(expected) {
        validate_jit_matches_interpreter("projection application", actual, expected)?;
    }
    Ok(())
}

fn program_output_capacity(
    jit: &CompiledJacobianRows,
    application: &ProjectionJacobianApplication,
) -> Result<usize, CompileError> {
    let mut count = 0;
    for color in application.colors() {
        for program in color.outputs().programs() {
            let row = jit.rows.get(program.program()).ok_or_else(|| {
                CompileError::Input("projection program is outside compiled rows".into())
            })?;
            if row.plan.output_count() != program.output_count() {
                return Err(CompileError::Input(
                    "projection program output extent mismatch".into(),
                ));
            }
            count = count.max(program.output_count());
        }
    }
    Ok(count)
}

impl CompiledJacobianRows {
    pub(crate) fn compile_projection_rows(
        &self,
        rows: &[Vec<LinearOp>],
        block: usize,
    ) -> Result<Self, CompileError> {
        compile_jacobian_rows_attached(rows, self._pure_calls.clone(), Some(block))
    }
}
