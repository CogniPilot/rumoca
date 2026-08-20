//! Native specialization of single-owner scalar rows.
//!
//! Every row an event pass evaluates more than a handful of times is compiled
//! once through the execution backend and replayed; the reference interpreter
//! stays the fallback and the oracle, never the plan. The cache and the failure
//! set are passed in, so one mechanism serves the discrete rows, the root
//! conditions, the visible-value rows, and the SOLVE-C57 clock-partition
//! intermediates without any of them re-deriving compilation policy.

use super::*;

impl SolveRuntime {
    pub(super) fn compile_discrete_specialization(&self, program: usize) {
        let Some(backend) = &self.execution_backend else {
            return;
        };
        if self.failed_discrete_rows.borrow().contains(&program) {
            return;
        }
        let (program_ops, output_count, guard_expectations) =
            if let Some(specialization) = self.discrete_rhs.specialized_row_program(program) {
                (
                    specialization.program,
                    specialization.output_count,
                    specialization.guard_expectations,
                )
            } else {
                let Some(program_ops) = self.discrete_rhs.block().programs().get(program).cloned()
                else {
                    return;
                };
                let Some(output_count) = self.discrete_rhs.row_output_count(program) else {
                    return;
                };
                (program_ops, output_count, Vec::new().into_boxed_slice())
            };
        let Some(total_outputs) = output_count.checked_add(guard_expectations.len()) else {
            return;
        };
        let Some(span) = self.discrete_rhs.block().program_span(program) else {
            return;
        };
        let block = match solve::ScalarProgramBlock::with_output_indices(
            vec![program_ops],
            vec![span],
            (0..total_outputs).collect(),
        ) {
            Ok(block) => block,
            Err(error) => {
                trace_native_execution_failure(program, &error.to_string());
                self.failed_discrete_rows.borrow_mut().insert(program);
                return;
            }
        };
        let expression = match backend.compile_expression(&block) {
            Ok(expression) => expression,
            Err(error) => {
                trace_native_execution_failure(program, &error);
                self.failed_discrete_rows.borrow_mut().insert(program);
                return;
            }
        };
        tracing::debug!(
            target: "rumoca_solver::native_execution",
            program,
            ops = block.programs()[0].len(),
            outputs = output_count,
            guards = guard_expectations.len(),
            "compiled discrete specialization"
        );
        self.compiled_discrete_rows
            .borrow_mut()
            .entry(program)
            .or_default()
            .push(CompiledDiscreteSpecialization {
                expression,
                output_count,
                guard_expectations,
            });
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn eval_single_output_rows_with_native(
        &self,
        block: &PreparedScalarProgramBlock,
        cache: &RefCell<FxHashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
        failed: &RefCell<BTreeSet<usize>>,
        row_indices: &[usize],
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        for &row in row_indices {
            if self.try_compiled_single_output(cache, row, y, p, t, out)? {
                continue;
            }
            // Retained tensor/fold programs can be dramatically more expensive
            // in the reference interpreter than in the native loop backend.
            // Try the complete checked program before paying for a profiling /
            // specialization pass. If that eager form is unsupported, clear
            // only this provisional failure so the interpreter can learn a
            // guarded specialization and compile it below.
            failed.borrow_mut().remove(&row);
            self.compile_cached_row(block, cache, failed, row);
            if self.try_compiled_single_output(cache, row, y, p, t, out)? {
                continue;
            }
            let value =
                block.eval_row_unchecked_with_context(row, y, p, t, self.row_eval_context())?;
            let out_len = out.len();
            let slot = out.get_mut(row).ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "selected expression row {row} is outside output length {out_len}"
                ))
            })?;
            *slot = value;
            self.compile_cached_row(block, cache, failed, row);
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn eval_selected_outputs_with_native(
        &self,
        block: &PreparedScalarProgramBlock,
        cache: &RefCell<FxHashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
        failed: &RefCell<BTreeSet<usize>>,
        output_indices: &[usize],
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let mut programs = BTreeMap::<usize, Vec<(usize, usize)>>::new();
        for &output in output_indices {
            let (program, offset) = block.row_output_position(output).ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "selected expression output {output} has no owning program"
                ))
            })?;
            programs.entry(program).or_default().push((output, offset));
        }
        for (program, selected) in programs {
            if self.try_compiled_program_outputs(cache, program, &selected, y, p, t, out)? {
                continue;
            }
            failed.borrow_mut().remove(&program);
            self.compile_cached_row(block, cache, failed, program);
            if self.try_compiled_program_outputs(cache, program, &selected, y, p, t, out)? {
                continue;
            }
            let mut values = Vec::new();
            block.eval_row_outputs_unchecked_with_context(
                program,
                y,
                p,
                t,
                self.row_eval_context(),
                &mut values,
            )?;
            copy_interpreted_program_outputs(&values, program, &selected, out)?;
            self.compile_cached_row(block, cache, failed, program);
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn try_compiled_program_outputs(
        &self,
        cache: &RefCell<FxHashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
        program: usize,
        selected: &[(usize, usize)],
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        let mut cache = cache.borrow_mut();
        let Some(compiled_variants) = cache.get_mut(&program) else {
            return Ok(false);
        };
        for variant_index in (0..compiled_variants.len()).rev() {
            let compiled = &compiled_variants[variant_index];
            let total_outputs = compiled
                .output_count
                .checked_add(compiled.guard_expectations.len())
                .ok_or_else(|| RuntimeSolveError::solve_ir("compiled output count overflow"))?;
            let mut scratch = self.compiled_output_scratch.borrow_mut();
            scratch.resize(total_outputs, 0.0);
            let called = compiled.expression.call(
                y,
                p,
                t,
                self.model.external_tables.as_slice(),
                &mut scratch,
            );
            if let Err(error) = &called {
                tracing::debug!(
                    target: "rumoca_solver::native_execution",
                    program,
                    %error,
                    "compiled specialization call failed"
                );
            }
            let valid = called.is_ok()
                && scratch[compiled.output_count..]
                    .iter()
                    .zip(&compiled.guard_expectations)
                    .all(|(actual, expected)| (*actual != 0.0) == *expected);
            if !valid {
                continue;
            }
            copy_compiled_program_outputs(&scratch, program, selected, out)?;
            drop(scratch);
            if variant_index + 1 != compiled_variants.len() {
                let compiled = compiled_variants.remove(variant_index);
                compiled_variants.push(compiled);
            }
            return Ok(true);
        }
        Ok(false)
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn try_compiled_single_output(
        &self,
        cache: &RefCell<FxHashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
        row: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        let mut cache = cache.borrow_mut();
        let Some(compiled_variants) = cache.get_mut(&row) else {
            return Ok(false);
        };
        for variant_index in (0..compiled_variants.len()).rev() {
            let compiled = &compiled_variants[variant_index];
            let total_outputs = compiled
                .output_count
                .checked_add(compiled.guard_expectations.len())
                .ok_or_else(|| RuntimeSolveError::solve_ir("compiled output count overflow"))?;
            let mut scratch = self.compiled_output_scratch.borrow_mut();
            scratch.resize(total_outputs, 0.0);
            let valid = compiled
                .expression
                .call(y, p, t, self.model.external_tables.as_slice(), &mut scratch)
                .is_ok()
                && scratch[compiled.output_count..]
                    .iter()
                    .zip(&compiled.guard_expectations)
                    .all(|(actual, expected)| (*actual != 0.0) == *expected);
            if !valid {
                continue;
            }
            let out_len = out.len();
            let slot = out.get_mut(row).ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "selected expression row {row} is outside output length {out_len}"
                ))
            })?;
            *slot = scratch[0];
            drop(scratch);
            if variant_index + 1 != compiled_variants.len() {
                let compiled = compiled_variants.remove(variant_index);
                compiled_variants.push(compiled);
            }
            return Ok(true);
        }
        Ok(false)
    }

    pub(super) fn compile_cached_row(
        &self,
        prepared: &PreparedScalarProgramBlock,
        cache: &RefCell<FxHashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
        failed: &RefCell<BTreeSet<usize>>,
        row: usize,
    ) {
        let Some(backend) = &self.execution_backend else {
            return;
        };
        if failed.borrow().contains(&row) {
            return;
        }
        let Some((program, output_count, guard_expectations)) = cached_row_program(prepared, row)
        else {
            failed.borrow_mut().insert(row);
            return;
        };
        let Some(total_outputs) = output_count.checked_add(guard_expectations.len()) else {
            failed.borrow_mut().insert(row);
            return;
        };
        let Some(span) = prepared.block().program_span(row) else {
            failed.borrow_mut().insert(row);
            return;
        };
        let block = match solve::ScalarProgramBlock::with_output_indices(
            vec![program],
            vec![span],
            (0..total_outputs).collect(),
        ) {
            Ok(block) => block,
            Err(error) => {
                trace_native_execution_failure(row, &error.to_string());
                failed.borrow_mut().insert(row);
                return;
            }
        };
        let expression = match backend.compile_expression(&block) {
            Ok(expression) => expression,
            Err(error) => {
                trace_native_execution_failure(row, &error);
                failed.borrow_mut().insert(row);
                return;
            }
        };
        cache
            .borrow_mut()
            .entry(row)
            .or_default()
            .push(CompiledDiscreteSpecialization {
                expression,
                output_count,
                guard_expectations,
            });
    }
}

fn copy_interpreted_program_outputs(
    values: &[f64],
    program: usize,
    selected: &[(usize, usize)],
    out: &mut [f64],
) -> Result<(), RuntimeSolveError> {
    for &(output, offset) in selected {
        let value = values.get(offset).copied().ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "selected expression output {output} offset {offset} is outside program {program}"
            ))
        })?;
        let out_len = out.len();
        let slot = out.get_mut(output).ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "selected expression output {output} is outside output length {out_len}"
            ))
        })?;
        *slot = value;
    }
    Ok(())
}

fn copy_compiled_program_outputs(
    values: &[f64],
    program: usize,
    selected: &[(usize, usize)],
    out: &mut [f64],
) -> Result<(), RuntimeSolveError> {
    for &(output, offset) in selected {
        let value = values.get(offset).copied().ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "compiled expression output {offset} is outside program {program}"
            ))
        })?;
        let out_len = out.len();
        let slot = out.get_mut(output).ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "selected expression output {output} is outside output length {out_len}"
            ))
        })?;
        *slot = value;
    }
    Ok(())
}
