//! Evaluate all outputs of one retained JVP program in one invocation.

use super::*;

impl CompiledJacobianRows {
    pub(crate) fn call_program_outputs(
        &self,
        program: usize,
        inputs: rumoca_eval_solve::JacobianEvalInputs<'_>,
        external_tables: &[ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<(), CompileError> {
        let row = self.rows.get(program).ok_or_else(|| {
            CompileError::Input(format!(
                "Jacobian program {program} is outside compiled rows"
            ))
        })?;
        validate_input_requirements(
            self.input_requirements,
            inputs.y,
            inputs.p,
            Some(inputs.seed),
        )?;
        out.resize(row.plan.output_count(), 0.0);
        with_active_external_tables(external_tables, || {
            self.call_jacobian_row(
                row,
                &mut self.regs_scratch.borrow_mut(),
                &JacobianCallContext {
                    y: inputs.y,
                    p: inputs.p,
                    t: inputs.t,
                    v: inputs.seed,
                    external_tables,
                },
                out,
            )
        })
    }
}
