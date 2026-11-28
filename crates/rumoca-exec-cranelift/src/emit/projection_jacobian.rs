use super::*;
use rumoca_ir_solve::ProjectionJacobianApplication;

#[cfg(test)]
mod reuse_tests;

/// A source-bound colored Jacobian with exclusively leased numeric storage.
pub struct CompiledProjectionJacobian {
    #[cfg(test)]
    jit: Rc<CompiledJacobianRows>,
    inputs: InputRequirements,
    batch: super::projection_batch::ProjectionBatch,
    application: ProjectionJacobianApplication,
    required_y_len: usize,
    #[cfg(test)]
    validate: bool,
    scratch: RefCell<ProjectionScratch>,
}

struct ProjectionScratch {
    seed: Vec<f64>,
    #[cfg(test)]
    program_output: Vec<f64>,
    matrix: Vec<f64>,
}

impl CompiledProjectionJacobian {
    /// Prepare every application into the shared projection module, which is
    /// finalized once for all of them.
    pub(crate) fn new_all(
        applications: Vec<ProjectionJacobianApplication>,
        source: &ScalarProgramBlock,
        jit: &Rc<CompiledJacobianRows>,
        shared: &super::SharedProjectionModule,
    ) -> Result<Vec<Self>, CompileError> {
        let mut prepared = Vec::with_capacity(applications.len());
        for application in &applications {
            let same_source = source.shares_program_owner(application.source());
            let inputs = if same_source {
                jit.input_requirements
            } else {
                let rows = application.source().programs();
                for row in rows {
                    validate_row_supported_by_jit(row, RowKind::JacobianV)?;
                }
                input_requirements_for_plans(&plan_rows(rows)?)
            };
            prepared.push((inputs, program_output_capacity(application)?));
        }
        let refs = applications.iter().collect::<Vec<_>>();
        let batches = super::projection_batch::ProjectionBatch::compile_all(
            &refs,
            jit._pure_calls.as_ref(),
            shared,
        )?;
        applications
            .into_iter()
            .zip(batches)
            .zip(prepared)
            .map(|((application, batch), (inputs, capacity))| {
                #[cfg(test)]
                let reference = if source.shares_program_owner(application.source()) {
                    jit.clone()
                } else {
                    Rc::new(compile_jacobian_rows_attached(
                        application.source().programs(),
                        jit._pure_calls.clone(),
                        Some(application.block_index()),
                    )?)
                };
                Self::with_batch(
                    application,
                    batch,
                    inputs,
                    capacity,
                    #[cfg(test)]
                    reference,
                )
            })
            .collect()
    }

    fn with_batch(
        application: ProjectionJacobianApplication,
        batch: super::projection_batch::ProjectionBatch,
        inputs: InputRequirements,
        _output_count: usize,
        #[cfg(test)] jit: Rc<CompiledJacobianRows>,
    ) -> Result<Self, CompileError> {
        let required_y_len = application
            .y_indices()
            .iter()
            .copied()
            .max()
            .map_or(Some(0), |index| index.checked_add(1))
            .ok_or_else(|| CompileError::Input("projection seed extent overflows".into()))?;
        let seed_len = required_y_len.max(inputs.seed_len);
        #[cfg(test)]
        let validate = application.colors().iter().any(|color| {
            color.outputs().programs().iter().any(|program| {
                let row = &jit.rows[program.program()];
                row.interpreter_supported
            })
        });
        let scratch = RefCell::new(ProjectionScratch {
            seed: vec![0.0; seed_len],
            #[cfg(test)]
            program_output: vec![0.0; _output_count],
            matrix: vec![0.0; application.compact_layout().len()],
        });
        Ok(Self {
            inputs,
            #[cfg(test)]
            jit,
            batch,
            application,
            required_y_len,
            #[cfg(test)]
            validate,
            scratch,
        })
    }

    /// Write the block Jacobian at `(y, p, t)` into `out` in the application's
    /// compact pattern layout, one value per pattern entry.
    pub fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        if out.len() != self.application.compact_layout().len() || y.len() < self.required_y_len {
            return Err(CompileError::Input(
                "projection Jacobian extent mismatch".into(),
            ));
        }
        let mut scratch = self.scratch.try_borrow_mut().map_err(|_| {
            CompileError::Input("projection Jacobian workspace is already in use".into())
        })?;
        validate_input_requirements(self.inputs, y, p, Some(&scratch.seed))?;
        #[cfg(test)]
        let mut registers = self.jit.regs_scratch.try_borrow_mut().map_err(|_| {
            CompileError::Input("Jacobian register workspace is already in use".into())
        })?;
        // The seed is all zero between calls: every color sets and clears its
        // own seed entries, and a failed call clears the whole seed below.
        let called = with_active_external_tables(external_tables, || {
            let ProjectionScratch { seed, matrix, .. } = &mut *scratch;
            self.batch.call(y, p, t, seed, matrix)?;
            #[cfg(test)]
            self.jit.record_jit_call();
            #[cfg(test)]
            if self.validate {
                let actual = scratch.matrix.clone();
                self.evaluate(&mut scratch, &mut registers, y, p, t, external_tables)?;
                validate_matrix(&actual, &scratch.matrix)?;
                scratch.matrix.copy_from_slice(&actual);
            }
            Ok::<_, CompileError>(())
        });
        if let Err(error) = called {
            scratch.seed.fill(0.0);
            return Err(error);
        }
        out.copy_from_slice(&scratch.matrix);
        Ok(())
    }

    #[cfg(test)]
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
    #[cfg(test)]
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
            let placements = super::projection_batch::compact_placements(
                &self.application,
                program.placements(),
            )?;
            for (offset, slot) in placements {
                matrix[slot] = output[offset];
            }
        }
        Ok(())
    }
}

#[cfg(test)]
fn validate_matrix(actual: &[f64], expected: &[f64]) -> Result<(), CompileError> {
    for (&actual, &expected) in actual.iter().zip(expected) {
        validate_jit_matches_interpreter("projection application", actual, expected)?;
    }
    Ok(())
}

fn program_output_capacity(
    application: &ProjectionJacobianApplication,
) -> Result<usize, CompileError> {
    let mut count = 0;
    for color in application.colors() {
        for program in color.outputs().programs() {
            let row = application
                .source()
                .program(program.program())
                .ok_or_else(|| {
                    CompileError::Input("projection program is outside compiled rows".into())
                })?;
            if ScalarProgramBlock::program_output_count(row) != program.output_count() {
                return Err(CompileError::Input(
                    "projection program output extent mismatch".into(),
                ));
            }
            count = count.max(program.output_count());
        }
    }
    Ok(count)
}
