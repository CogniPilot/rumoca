use super::*;
use crate::invalid_row;

impl Clone for PreparedScalarProgramBlock {
    fn clone(&self) -> Self {
        Self {
            block: self.block.clone(),
            output_count: self.output_count,
            row_outputs: self.row_outputs.clone(),
            row_registers: self.row_registers.clone(),
            row_lazy_plans: self.row_lazy_plans.clone(),
            row_requirements: self.row_requirements.clone(),
            row_reverse_y_gradient_supported: self.row_reverse_y_gradient_supported.clone(),
            row_is_causal: self.row_is_causal.clone(),
            row_assignment_shapes: self.row_assignment_shapes.clone(),
            row_tensor_affine_assignments: self.row_tensor_affine_assignments.clone(),
            row_parameter_indices: self.row_parameter_indices.clone(),
            row_parameter_static_y_gradient_params: self
                .row_parameter_static_y_gradient_params
                .clone(),
            requirements: self.requirements,
            scratch: RefCell::new(RowEvalScratch::default()),
            row_output_scratch: RefCell::new(Vec::new()),
        }
    }
}

/// Every per-program fact the prepared block records for one program.
struct PreparedRow {
    register_count: usize,
    lazy_plan: Option<PreparedLazyRowPlan>,
    requirement: RowInputRequirements,
    reverse_y_gradient_supported: bool,
    is_causal: bool,
    assignment_shapes: Box<[(usize, TargetAssignmentShape)]>,
    tensor_affine_assignments: tensor_affine_assignment::PreparedTensorAffineAssignments,
    parameter_indices: Box<[usize]>,
    parameter_static_y_gradient_params: Option<Box<[usize]>>,
}

fn prepare_row(block: &ScalarProgramBlock, row_idx: usize) -> Result<PreparedRow, EvalSolveError> {
    let span = block.program_span(row_idx);
    let row = block.program(row_idx).ok_or_else(|| {
        invalid_row("prepared program is outside its block").with_source_span(span)
    })?;
    let requirement = row_input_requirements(row).map_err(|error| error.with_source_span(span))?;
    let register_count = block.program_register_count(row_idx).ok_or_else(|| {
        invalid_row("checked block has no register certificate for its program")
            .with_source_span(span)
    })?;
    let assignment_shapes = target_assignment_shapes_with_output_offsets(row)
        .map_err(|error| error.with_source_span(span))?
        .into_boxed_slice();
    let tensor_affine_assignments =
        tensor_affine_assignment::prepare(row, &assignment_shapes, span)?;
    let parameter_indices = row_parameter_indices(row).into_boxed_slice();
    Ok(PreparedRow {
        register_count,
        lazy_plan: PreparedLazyRowPlan::new(row, register_count),
        requirement,
        reverse_y_gradient_supported: reverse_y_gradient_supported(row),
        is_causal: !row.iter().any(non_causal_linear_op),
        assignment_shapes,
        tensor_affine_assignments,
        parameter_static_y_gradient_params: parameter_static_y_gradient(row)
            .then(|| parameter_indices.clone()),
        parameter_indices,
    })
}

/// The programs at which `target` differs from `base` when both blocks share
/// one output layout: the same program count, output indices, and output count
/// of every program. `None` when the layouts differ.
pub fn replaced_programs(
    base: &ScalarProgramBlock,
    target: &ScalarProgramBlock,
) -> Option<Vec<usize>> {
    if base.programs().len() != target.programs().len()
        || base.output_indices() != target.output_indices()
    {
        return None;
    }
    let mut replaced = Vec::new();
    for (index, (base, target)) in base.programs().iter().zip(target.programs()).enumerate() {
        if base == target {
            continue;
        }
        if ScalarProgramBlock::program_output_count(base)
            != ScalarProgramBlock::program_output_count(target)
        {
            return None;
        }
        replaced.push(index);
    }
    Some(replaced)
}

impl PreparedScalarProgramBlock {
    pub fn new(block: ScalarProgramBlock) -> Result<Self, EvalSolveError> {
        let row_count = block.programs().len();
        let block_span = block.program_span(0);
        let output_count = checked_prepared_output_count(&block)?;
        let row_outputs = Box::new(prepare_row_output_metadata(&block, output_count)?);
        let mut prepared = Self {
            output_count,
            row_outputs,
            row_registers: prepared_vec_with_capacity(
                row_count,
                "prepared row register count",
                block_span,
            )?,
            row_lazy_plans: prepared_vec_with_capacity(
                row_count,
                "prepared lazy row plan count",
                block_span,
            )?,
            row_requirements: prepared_vec_with_capacity(
                row_count,
                "prepared row requirement count",
                block_span,
            )?,
            row_reverse_y_gradient_supported: prepared_vec_with_capacity(
                row_count,
                "prepared reverse gradient capability count",
                block_span,
            )?,
            row_is_causal: prepared_vec_with_capacity(
                row_count,
                "prepared row causality count",
                block_span,
            )?,
            row_assignment_shapes: prepared_vec_with_capacity(
                row_count,
                "prepared row assignment shape count",
                block_span,
            )?,
            row_tensor_affine_assignments: Vec::with_capacity(row_count),
            row_parameter_indices: prepared_vec_with_capacity(
                row_count,
                "prepared row parameter index count",
                block_span,
            )?,
            row_parameter_static_y_gradient_params: prepared_vec_with_capacity(
                row_count,
                "prepared parameter-static gradient count",
                block_span,
            )?,
            requirements: RowInputRequirements::default(),
            scratch: RefCell::new(RowEvalScratch::default()),
            row_output_scratch: RefCell::new(Vec::new()),
            block,
        };
        for row_idx in 0..row_count {
            let row = prepare_row(&prepared.block, row_idx)?;
            prepared.requirements = prepared.requirements.merge(row.requirement);
            prepared.push_row(row);
        }
        Ok(prepared)
    }

    /// The prepared form of `block`, a block whose programs equal `base`'s
    /// except at `replaced` under one output layout (see [`replaced_programs`]):
    /// only the replaced programs are prepared, every other program keeps the
    /// facts `base` recorded for it.
    pub fn with_replaced_programs(
        base: &Self,
        block: ScalarProgramBlock,
        replaced: &[usize],
    ) -> Result<Self, EvalSolveError> {
        let same_owner = replaced.is_empty() && base.block.shares_program_owner(&block);
        if !same_owner && replaced_programs(&base.block, &block).as_deref() != Some(replaced) {
            return Err(invalid_row(
                "replaced programs do not describe the difference from the base block",
            )
            .with_source_span(block.first_source_span()));
        }
        let mut prepared = base.clone();
        prepared.block = block;
        for &row_idx in replaced {
            let row = prepare_row(&prepared.block, row_idx)?;
            prepared.set_row(row_idx, row);
        }
        prepared.requirements = prepared
            .row_requirements
            .iter()
            .fold(RowInputRequirements::default(), |merged, row| {
                merged.merge(*row)
            });
        Ok(prepared)
    }

    fn push_row(&mut self, row: PreparedRow) {
        self.row_registers.push(row.register_count);
        self.row_lazy_plans.push(row.lazy_plan);
        self.row_requirements.push(row.requirement);
        self.row_reverse_y_gradient_supported
            .push(row.reverse_y_gradient_supported);
        self.row_is_causal.push(row.is_causal);
        self.row_assignment_shapes.push(row.assignment_shapes);
        self.row_tensor_affine_assignments
            .push(row.tensor_affine_assignments);
        self.row_parameter_indices.push(row.parameter_indices);
        self.row_parameter_static_y_gradient_params
            .push(row.parameter_static_y_gradient_params);
    }

    fn set_row(&mut self, row_idx: usize, row: PreparedRow) {
        self.row_registers[row_idx] = row.register_count;
        self.row_lazy_plans[row_idx] = row.lazy_plan;
        self.row_requirements[row_idx] = row.requirement;
        self.row_reverse_y_gradient_supported[row_idx] = row.reverse_y_gradient_supported;
        self.row_is_causal[row_idx] = row.is_causal;
        self.row_assignment_shapes[row_idx] = row.assignment_shapes;
        self.row_tensor_affine_assignments[row_idx] = row.tensor_affine_assignments;
        self.row_parameter_indices[row_idx] = row.parameter_indices;
        self.row_parameter_static_y_gradient_params[row_idx] =
            row.parameter_static_y_gradient_params;
    }

    pub fn from_compute_block(block: &ComputeBlock) -> Result<Self, EvalSolveError> {
        Self::new(crate::to_scalar_program_block(block)?)
    }
}

impl PreparedComputeBlock {
    /// The prepared form of `block` given `base`, the prepared block of the
    /// same role in another system: when both are one scalar-program node
    /// under one output layout, only the programs that differ are prepared
    /// (see [`PreparedScalarProgramBlock::with_replaced_programs`]); otherwise
    /// `block` is prepared anew.
    pub fn with_shared_programs(
        base: &Self,
        block: &ComputeBlock,
        label: &'static str,
    ) -> Result<Self, EvalSolveError> {
        let (
            [PreparedComputeNode::ScalarPrograms(prepared)],
            [ComputeNode::ScalarPrograms(source)],
        ) = (base.nodes.as_slice(), block.nodes.as_slice())
        else {
            return Self::new_with_label(block, label);
        };
        let placed = ScalarProgramBlock::with_output_indices(
            source.programs().to_vec(),
            source.program_spans().to_vec(),
            scalar_program_output_indices(source, 0, "prepared scalar programs")?,
        )?;
        let Some(replaced) = replaced_programs(prepared.block(), &placed) else {
            return Self::new_with_label(block, label);
        };
        let len = block.len().map_err(EvalSolveError::from)?;
        if len != base.len {
            return Self::new_with_label(block, label);
        }
        let node = PreparedScalarProgramBlock::with_replaced_programs(prepared, placed, &replaced)?;
        Ok(Self {
            label,
            requirements: node.requirements(),
            nodes: vec![PreparedComputeNode::ScalarPrograms(Box::new(node))],
            len,
            scratch: RefCell::new(RowEvalScratch::default()),
        })
    }
}

fn reverse_y_gradient_supported(row: &[LinearOp]) -> bool {
    row.iter()
        .filter(|op| matches!(op, LinearOp::StoreOutput { .. }))
        .count()
        == 1
        && row.iter().all(crate::reverse::reverse_row_op_supported)
}
