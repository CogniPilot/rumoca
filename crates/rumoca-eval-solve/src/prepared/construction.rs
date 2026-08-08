use super::*;

impl Clone for PreparedScalarProgramBlock {
    fn clone(&self) -> Self {
        Self {
            block: self.block.clone(),
            output_count: self.output_count,
            row_outputs: self.row_outputs.clone(),
            row_registers: self.row_registers.clone(),
            row_requirements: self.row_requirements.clone(),
            row_seed_loads: self.row_seed_loads.clone(),
            row_assignment_shapes: self.row_assignment_shapes.clone(),
            row_parameter_static_y_gradient_params: self
                .row_parameter_static_y_gradient_params
                .clone(),
            requirements: self.requirements,
            scratch: RefCell::new(RowEvalScratch::default()),
            row_output_scratch: RefCell::new(Vec::new()),
        }
    }
}

impl PreparedScalarProgramBlock {
    pub fn new(block: ScalarProgramBlock) -> Result<Self, EvalSolveError> {
        let row_count = block.programs().len();
        let block_span = block.program_span(0);
        let output_count = checked_prepared_output_count(&block)?;
        let row_outputs = Box::new(prepare_row_output_metadata(&block, output_count)?);
        let mut row_registers =
            prepared_vec_with_capacity(row_count, "prepared row register count", block_span)?;
        let mut row_requirements =
            prepared_vec_with_capacity(row_count, "prepared row requirement count", block_span)?;
        let mut row_seed_loads =
            prepared_vec_with_capacity(row_count, "prepared row seed load count", block_span)?;
        let mut row_assignment_shapes = prepared_vec_with_capacity(
            row_count,
            "prepared row assignment shape count",
            block_span,
        )?;
        let mut row_parameter_static_y_gradient_params = prepared_vec_with_capacity(
            row_count,
            "prepared parameter-static gradient count",
            block_span,
        )?;
        let mut requirements = RowInputRequirements::default();
        for (row_idx, row) in block.programs().iter().enumerate() {
            let span = block.program_span(row_idx);
            let row_requirement =
                row_input_requirements(row).map_err(|error| error.with_source_span(span))?;
            row_registers
                .push(required_registers(row).map_err(|error| error.with_source_span(span))?);
            row_requirements.push(row_requirement);
            row_seed_loads.push(prepared_seed_loads(row, span)?);
            row_assignment_shapes.push(
                target_assignment_shapes(row)
                    .map_err(|error| error.with_source_span(span))?
                    .into_boxed_slice(),
            );
            row_parameter_static_y_gradient_params.push(
                parameter_static_y_gradient(row)
                    .then(|| row_parameter_indices(row).into_boxed_slice()),
            );
            requirements = requirements.merge(row_requirement);
        }
        Ok(Self {
            block,
            output_count,
            row_outputs,
            row_registers,
            row_requirements,
            row_seed_loads,
            row_assignment_shapes,
            row_parameter_static_y_gradient_params,
            requirements,
            scratch: RefCell::new(RowEvalScratch::default()),
            row_output_scratch: RefCell::new(Vec::new()),
        })
    }

    pub fn from_compute_block(block: &ComputeBlock) -> Result<Self, EvalSolveError> {
        Self::new(crate::to_scalar_program_block(block)?)
    }
}
