use super::*;

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum TensorElementType {
    #[default]
    Real64,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum TensorLayout {
    #[default]
    RowMajorDense,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum ScalarFallback {
    #[default]
    Exact,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct TensorNodeMetadata {
    pub element_type: TensorElementType,
    pub layout: TensorLayout,
    pub scalar_fallback: ScalarFallback,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AffineStencilIndexStrideTerm {
    pub dimension: usize,
    pub stride: isize,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AffineStencilConstStrideTerm {
    pub dimension: usize,
    pub stride: f64,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AffineStencilLoadStride {
    pub op_position: usize,
    pub terms: Vec<AffineStencilIndexStrideTerm>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AffineStencilConstStride {
    pub op_position: usize,
    pub terms: Vec<AffineStencilConstStrideTerm>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TensorOutputMap {
    pub start: usize,
    pub strides: Vec<AffineStencilIndexStrideTerm>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TensorOutputMapError {
    Dimension {
        output_dimension: usize,
        domain_rank: usize,
    },
    StructuredIndexDomain {
        error: StructuredIndexDomainError,
    },
    NegativeIndex {
        value: isize,
    },
    OutputIndexOverflow,
}

impl TensorOutputMap {
    pub fn dense_contiguous(
        start: usize,
        domain: &StructuredIndexDomain,
    ) -> Result<Self, TensorOutputMapError> {
        Ok(Self {
            start,
            strides: dense_domain_output_strides(domain)?,
        })
    }

    pub fn output_indices(
        &self,
        domain: &StructuredIndexDomain,
    ) -> Result<Vec<usize>, TensorOutputMapError> {
        let index_tuples = domain
            .index_tuples()
            .map_err(|error| TensorOutputMapError::StructuredIndexDomain { error })?;
        let Some(base_tuple) = index_tuples.first().cloned() else {
            return Ok(Vec::new());
        };
        index_tuples
            .iter()
            .map(|index_tuple| self.output_index(domain, &base_tuple, index_tuple))
            .collect()
    }

    pub fn output_count(
        &self,
        domain: &StructuredIndexDomain,
    ) -> Result<usize, TensorOutputMapError> {
        let extents = domain
            .extents()
            .map_err(|error| TensorOutputMapError::StructuredIndexDomain { error })?;
        if extents.contains(&0) {
            return Ok(0);
        }
        let (minimum, maximum) = self.output_bounds(&extents, domain.binders.len())?;
        if minimum < 0 {
            let value =
                isize::try_from(minimum).map_err(|_| TensorOutputMapError::OutputIndexOverflow)?;
            return Err(TensorOutputMapError::NegativeIndex { value });
        }
        let max_index =
            usize::try_from(maximum).map_err(|_| TensorOutputMapError::OutputIndexOverflow)?;
        max_index
            .checked_add(1)
            .ok_or(TensorOutputMapError::OutputIndexOverflow)
    }

    fn output_bounds(
        &self,
        extents: &[usize],
        domain_rank: usize,
    ) -> Result<(i128, i128), TensorOutputMapError> {
        let start =
            i128::try_from(self.start).map_err(|_| TensorOutputMapError::OutputIndexOverflow)?;
        let mut minimum = start;
        let mut maximum = start;
        let dimension_strides = aggregate_affine_index_strides(&self.strides, domain_rank)?;
        for (extent, stride) in extents.iter().copied().zip(dimension_strides) {
            let last_position = i128::try_from(extent - 1)
                .map_err(|_| TensorOutputMapError::OutputIndexOverflow)?;
            let offset = last_position
                .checked_mul(stride)
                .ok_or(TensorOutputMapError::OutputIndexOverflow)?;
            if offset < 0 {
                minimum = minimum
                    .checked_add(offset)
                    .ok_or(TensorOutputMapError::OutputIndexOverflow)?;
            } else {
                maximum = maximum
                    .checked_add(offset)
                    .ok_or(TensorOutputMapError::OutputIndexOverflow)?;
            }
        }
        Ok((minimum, maximum))
    }

    fn output_index(
        &self,
        domain: &StructuredIndexDomain,
        base_tuple: &[i64],
        index_tuple: &[i64],
    ) -> Result<usize, TensorOutputMapError> {
        let mut value =
            i128::try_from(self.start).map_err(|_| TensorOutputMapError::OutputIndexOverflow)?;
        let dimension_strides =
            aggregate_affine_index_strides(&self.strides, domain.binders.len())?;
        for (dimension, stride) in dimension_strides.into_iter().enumerate() {
            let delta = i128::from(output_ordinal_delta(
                dimension,
                domain,
                base_tuple,
                index_tuple,
            ));
            let offset = delta
                .checked_mul(stride)
                .ok_or(TensorOutputMapError::OutputIndexOverflow)?;
            value = value
                .checked_add(offset)
                .ok_or(TensorOutputMapError::OutputIndexOverflow)?;
        }
        if value < 0 {
            let value =
                isize::try_from(value).map_err(|_| TensorOutputMapError::OutputIndexOverflow)?;
            return Err(TensorOutputMapError::NegativeIndex { value });
        }
        usize::try_from(value).map_err(|_| TensorOutputMapError::OutputIndexOverflow)
    }
}

fn aggregate_affine_index_strides(
    terms: &[AffineStencilIndexStrideTerm],
    domain_rank: usize,
) -> Result<Vec<i128>, TensorOutputMapError> {
    let mut dimension_strides = vec![0i128; domain_rank];
    for term in terms {
        let Some(stride) = dimension_strides.get_mut(term.dimension) else {
            return Err(TensorOutputMapError::Dimension {
                output_dimension: term.dimension,
                domain_rank,
            });
        };
        *stride = stride
            .checked_add(term.stride as i128)
            .ok_or(TensorOutputMapError::OutputIndexOverflow)?;
    }
    Ok(dimension_strides)
}

fn dense_domain_output_strides(
    domain: &StructuredIndexDomain,
) -> Result<Vec<AffineStencilIndexStrideTerm>, TensorOutputMapError> {
    let mut later_count = 1usize;
    let mut terms = Vec::new();
    for (dimension, binder) in domain.binders.iter().enumerate().rev() {
        let value_count =
            binder_value_count(binder).ok_or(TensorOutputMapError::OutputIndexOverflow)?;
        if value_count > 1 {
            terms.push(AffineStencilIndexStrideTerm {
                dimension,
                stride: isize::try_from(later_count)
                    .map_err(|_| TensorOutputMapError::OutputIndexOverflow)?,
            });
        }
        later_count = later_count
            .checked_mul(value_count)
            .ok_or(TensorOutputMapError::OutputIndexOverflow)?;
    }
    terms.reverse();
    Ok(terms)
}

fn output_ordinal_delta(
    dimension: usize,
    domain: &StructuredIndexDomain,
    base_tuple: &[i64],
    index_tuple: &[i64],
) -> i64 {
    let step = domain.binders[dimension].step;
    (index_tuple[dimension] - base_tuple[dimension]) / step
}

fn binder_value_count(binder: &rumoca_core::StructuredIndexBinder) -> Option<usize> {
    if binder.step == 0 {
        return Some(0);
    }
    if binder.step > 0 {
        if binder.lower > binder.upper {
            return Some(0);
        }
        usize::try_from(
            ((i128::from(binder.upper) - i128::from(binder.lower)) / i128::from(binder.step)) + 1,
        )
        .ok()
    } else {
        if binder.lower < binder.upper {
            return Some(0);
        }
        usize::try_from(
            ((i128::from(binder.lower) - i128::from(binder.upper)) / -i128::from(binder.step)) + 1,
        )
        .ok()
    }
}

/// A single tensor-level compute node.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ComputeNode {
    /// Existing scalar op rows — all current behavior lives here.
    ScalarPrograms(ScalarProgramBlock),

    /// Dense matrix multiply: C (m×n) = A (m×k) * B (k×n).
    ///
    /// `lhs_ops` evaluates to m*k values (regs `lhs_start..lhs_start+m*k`, row-major).
    /// `rhs_ops` evaluates to k*n values (regs `rhs_start..rhs_start+k*n`, row-major).
    /// Writes m*n consecutive output values (one per output slot).
    MatMul {
        lhs_ops: Vec<LinearOp>,
        lhs_start: Reg,
        rhs_ops: Vec<LinearOp>,
        rhs_start: Reg,
        m: usize,
        k: usize,
        n: usize,
        /// Sparsity of the lhs (A) operand.  `Dense` unless the lowering phase
        /// can statically prove a sparser structure.
        lhs_pattern: StructuralPattern,
        /// Sparsity of the rhs (B) operand.  `Dense` unless statically known.
        rhs_pattern: StructuralPattern,
        metadata: TensorNodeMetadata,
        span: Span,
    },

    /// Dense linear solve: A (n×n) * x = b, writes n consecutive output values.
    ///
    /// `setup_ops` evaluates to n*n + n values:
    ///   regs `matrix_start..matrix_start+n*n` = A (row-major)
    ///   regs `rhs_start..rhs_start+n` = b
    /// `next_reg` is the first free register after setup (used for scalarization).
    LinSolve {
        setup_ops: Vec<LinearOp>,
        matrix_start: Reg,
        rhs_start: Reg,
        n: usize,
        next_reg: Reg,
        /// Constructor-derived structural pattern of the square coefficient
        /// matrix. Runtime values never redefine this relation.
        matrix_pattern: StructuralPattern,
        metadata: TensorNodeMetadata,
        span: Span,
    },

    /// Elementwise tensor map over a compact index domain.
    ///
    /// Expands to one scalar row per domain point by cloning `base_ops` and
    /// applying affine register-independent strides to loads and constants.
    Map {
        domain: StructuredIndexDomain,
        output_map: TensorOutputMap,
        base_ops: Vec<LinearOp>,
        load_strides: Vec<AffineStencilLoadStride>,
        const_strides: Vec<AffineStencilConstStride>,
        metadata: TensorNodeMetadata,
        span: Span,
    },

    /// Consecutive scalar rows whose load indices advance affinely over a compact domain.
    ///
    /// Expands to one scalar row per compact domain point by cloning `base_ops`
    /// and applying each load/constant stride term to the corresponding domain
    /// coordinate offset.
    AffineStencil {
        domain: StructuredIndexDomain,
        output_map: TensorOutputMap,
        base_ops: Vec<LinearOp>,
        load_strides: Vec<AffineStencilLoadStride>,
        const_strides: Vec<AffineStencilConstStride>,
        metadata: TensorNodeMetadata,
        span: Span,
    },
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct ComputeNodeCounts {
    pub scalar_programs: usize,
    pub matmul: usize,
    pub linsolve: usize,
    pub map: usize,
    pub affine_stencil: usize,
}

impl ComputeNodeCounts {
    pub fn tensor_nodes(self) -> usize {
        self.matmul + self.linsolve + self.map + self.affine_stencil
    }

    pub fn add_assign(&mut self, rhs: Self) {
        self.scalar_programs += rhs.scalar_programs;
        self.matmul += rhs.matmul;
        self.linsolve += rhs.linsolve;
        self.map += rhs.map;
        self.affine_stencil += rhs.affine_stencil;
    }
}

/// A sequence of compute nodes in a `SolveProblem`.
///
/// Serializes as `{"nodes": [...]}` where each node is a tagged enum variant
/// (`ScalarPrograms`, `MatMul`, `LinSolve`, `AffineStencil`). Tensor structure is preserved through
/// the serde round-trip so backends can choose scalar fallback or native tensor ops.
#[derive(Clone, Debug, Default)]
pub struct ComputeBlock {
    pub nodes: Vec<ComputeNode>,
}

impl ComputeBlock {
    /// Wrap a `ScalarProgramBlock` in a single `ScalarPrograms` node.
    pub fn from_scalar_program_block(block: ScalarProgramBlock) -> Self {
        if block.is_empty() {
            Self { nodes: vec![] }
        } else {
            Self {
                nodes: vec![ComputeNode::ScalarPrograms(block)],
            }
        }
    }

    /// Total output slot count across all nodes.
    pub fn len(&self) -> Result<usize, SolveProblemShapeContractError> {
        self.output_count("ComputeBlock::len")
    }

    pub fn output_count(
        &self,
        context: &'static str,
    ) -> Result<usize, SolveProblemShapeContractError> {
        let mut output_cursor = 0usize;
        for (node_index, node) in self.nodes.iter().enumerate() {
            match node {
                ComputeNode::ScalarPrograms(block) => {
                    output_cursor = block.advance_compute_block_output_cursor(
                        context,
                        node_index,
                        output_cursor,
                    )?;
                }
                ComputeNode::Map {
                    domain, output_map, ..
                }
                | ComputeNode::AffineStencil {
                    domain, output_map, ..
                } => {
                    output_cursor = output_cursor.max(tensor_output_count_for_node(
                        context, node_index, node, domain, output_map,
                    )?);
                }
                ComputeNode::MatMul { m, n, span, .. } => {
                    let output_count = m
                        .checked_mul(*n)
                        .ok_or_else(|| output_index_overflow(context, node_index, Some(*span)))?;
                    output_cursor = output_cursor
                        .checked_add(output_count)
                        .ok_or_else(|| output_index_overflow(context, node_index, Some(*span)))?;
                }
                ComputeNode::LinSolve { n, span, .. } => {
                    output_cursor = output_cursor
                        .checked_add(*n)
                        .ok_or_else(|| output_index_overflow(context, node_index, Some(*span)))?;
                }
            }
        }
        Ok(output_cursor)
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.iter().all(|node| match node {
            ComputeNode::ScalarPrograms(block) => block.is_empty(),
            ComputeNode::Map { domain, .. } | ComputeNode::AffineStencil { domain, .. } => {
                domain.scalar_count().is_ok_and(|count| count == 0)
            }
            ComputeNode::MatMul { m, n, .. } => *m == 0 || *n == 0,
            ComputeNode::LinSolve { n, .. } => *n == 0,
        })
    }

    pub fn compute_node_counts(&self) -> ComputeNodeCounts {
        let mut counts = ComputeNodeCounts::default();
        for node in &self.nodes {
            match node {
                ComputeNode::ScalarPrograms(_) => counts.scalar_programs += 1,
                ComputeNode::MatMul { .. } => counts.matmul += 1,
                ComputeNode::LinSolve { .. } => counts.linsolve += 1,
                ComputeNode::Map { .. } => counts.map += 1,
                ComputeNode::AffineStencil { .. } => counts.affine_stencil += 1,
            }
        }
        counts
    }

    pub fn uses_linear_solve_component(&self) -> bool {
        self.nodes.iter().any(|node| match node {
            ComputeNode::ScalarPrograms(block) => block.uses_linear_solve_component(),
            ComputeNode::LinSolve { .. } => true,
            ComputeNode::Map { base_ops, .. } | ComputeNode::AffineStencil { base_ops, .. } => {
                linear_ops_use_linear_solve_component(base_ops)
            }
            ComputeNode::MatMul { .. } => false,
        })
    }

    pub fn tensor_node_count(&self) -> usize {
        self.compute_node_counts().tensor_nodes()
    }

    pub fn validate_shape_contract(
        &self,
        context: impl Into<String>,
    ) -> Result<(), SolveProblemShapeContractError> {
        let context = context.into();
        for (index, node) in self.nodes.iter().enumerate() {
            node.validate_shape_contract(&context, index)?;
        }
        Ok(())
    }
}

fn tensor_output_count_for_node(
    context: &'static str,
    node_index: usize,
    node: &ComputeNode,
    domain: &StructuredIndexDomain,
    output_map: &TensorOutputMap,
) -> Result<usize, SolveProblemShapeContractError> {
    let (dimension, span) = match node {
        ComputeNode::Map { span, .. } => ("Map", *span),
        ComputeNode::AffineStencil { span, .. } => ("AffineStencil", *span),
        ComputeNode::ScalarPrograms(_)
        | ComputeNode::MatMul { .. }
        | ComputeNode::LinSolve { .. } => unreachable!("tensor output count requires tensor node"),
    };
    output_map
        .output_count(domain)
        .map_err(|error| tensor_output_map_error(context, node_index, dimension, error, span))
}

fn tensor_output_map_error(
    context: &str,
    node_index: usize,
    dimension: &'static str,
    error: TensorOutputMapError,
    span: Span,
) -> SolveProblemShapeContractError {
    match error {
        TensorOutputMapError::Dimension {
            output_dimension,
            domain_rank,
        } => SolveProblemShapeContractError::TensorOutputMapDimension {
            context: context.to_string(),
            node_index,
            dimension,
            output_dimension,
            domain_rank,
            span,
        },
        TensorOutputMapError::StructuredIndexDomain { error } => {
            SolveProblemShapeContractError::StructuredIndexDomain {
                context: context.to_string(),
                node_index,
                dimension,
                error,
                span,
            }
        }
        TensorOutputMapError::NegativeIndex { value } => {
            SolveProblemShapeContractError::TensorOutputMapNegativeIndex {
                context: context.to_string(),
                node_index,
                dimension,
                value,
                span,
            }
        }
        TensorOutputMapError::OutputIndexOverflow => {
            output_index_overflow(context, node_index, Some(span))
        }
    }
}

pub(super) fn output_index_overflow(
    context: impl Into<String>,
    node_index: usize,
    span: Option<Span>,
) -> SolveProblemShapeContractError {
    SolveProblemShapeContractError::OutputIndexOverflow {
        context: context.into(),
        node_index,
        span,
    }
}

impl ComputeNode {
    pub fn validate_shape_contract(
        &self,
        context: &str,
        node_index: usize,
    ) -> Result<(), SolveProblemShapeContractError> {
        match self {
            ComputeNode::ScalarPrograms(block) => {
                block
                    .validate_shape_contract(context)
                    .map_err(|err| match err {
                        SolveProblemShapeContractError::ScalarProgramSpanMismatch {
                            programs,
                            spans,
                            ..
                        } => SolveProblemShapeContractError::ScalarProgramSpanMismatch {
                            context: context.to_string(),
                            node_index,
                            programs,
                            spans,
                            span: block.first_program_span(),
                        },
                        SolveProblemShapeContractError::ScalarProgramOutputIndexMismatch {
                            programs,
                            output_indices,
                            ..
                        } => SolveProblemShapeContractError::ScalarProgramOutputIndexMismatch {
                            context: context.to_string(),
                            node_index,
                            programs,
                            output_indices,
                            span: block.first_program_span(),
                        },
                        SolveProblemShapeContractError::ScalarProgramMissingOutput {
                            program_index,
                            span,
                            ..
                        } => SolveProblemShapeContractError::ScalarProgramMissingOutput {
                            context: context.to_string(),
                            node_index,
                            program_index,
                            span,
                        },
                        other => other,
                    })?;
            }
            ComputeNode::MatMul { m, k, n, span, .. } => {
                if *m == 0 || *k == 0 || *n == 0 {
                    return Err(SolveProblemShapeContractError::ZeroTensorDimension {
                        context: context.to_string(),
                        node_index,
                        dimension: "MatMul",
                        span: *span,
                    });
                }
            }
            ComputeNode::LinSolve { n, span, .. } => {
                if *n == 0 {
                    return Err(SolveProblemShapeContractError::ZeroTensorDimension {
                        context: context.to_string(),
                        node_index,
                        dimension: "LinSolve",
                        span: *span,
                    });
                }
            }
            ComputeNode::Map { .. } | ComputeNode::AffineStencil { .. } => {
                validate_affine_compute_node(self, context, node_index)?;
            }
        }
        Ok(())
    }
}

fn validate_affine_compute_node(
    node: &ComputeNode,
    context: &str,
    node_index: usize,
) -> Result<(), SolveProblemShapeContractError> {
    let (node_kind, dimension, domain, output_map, base_ops, load_strides, const_strides, span) =
        match node {
            ComputeNode::Map {
                domain,
                output_map,
                base_ops,
                load_strides,
                const_strides,
                span,
                ..
            } => (
                AffineTensorNodeKind::Map,
                "Map",
                domain,
                output_map,
                base_ops,
                load_strides,
                const_strides,
                *span,
            ),
            ComputeNode::AffineStencil {
                domain,
                output_map,
                base_ops,
                load_strides,
                const_strides,
                span,
                ..
            } => (
                AffineTensorNodeKind::AffineStencil,
                "AffineStencil",
                domain,
                output_map,
                base_ops,
                load_strides,
                const_strides,
                *span,
            ),
            _ => return Ok(()),
        };
    let validation = AffineValidationContext {
        context,
        node_index,
        node_kind,
        dimension,
        domain,
        base_ops,
        span,
    };
    validate_tensor_domain(context, node_index, dimension, domain, span)?;
    validate_tensor_output_map(context, node_index, dimension, domain, output_map, span)?;
    validate_affine_stride_metadata(&validation, load_strides, const_strides)
}

struct AffineValidationContext<'a> {
    context: &'a str,
    node_index: usize,
    node_kind: AffineTensorNodeKind,
    dimension: &'static str,
    domain: &'a StructuredIndexDomain,
    base_ops: &'a [LinearOp],
    span: Span,
}

fn validate_affine_stride_metadata(
    validation: &AffineValidationContext<'_>,
    load_strides: &[AffineStencilLoadStride],
    const_strides: &[AffineStencilConstStride],
) -> Result<(), SolveProblemShapeContractError> {
    validate_affine_load_strides(validation, load_strides)?;
    validate_affine_constant_strides(validation, const_strides)?;
    validate_affine_load_index_ranges(validation, load_strides)
}

fn validate_affine_load_strides(
    validation: &AffineValidationContext<'_>,
    load_strides: &[AffineStencilLoadStride],
) -> Result<(), SolveProblemShapeContractError> {
    for stride in load_strides {
        validate_affine_stride_dimensions(validation, "load", stride.op_position, &stride.terms)?;
        match validation.base_ops.get(stride.op_position) {
            Some(LinearOp::LoadY { .. } | LinearOp::LoadP { .. } | LinearOp::LoadSeed { .. }) => {}
            actual => {
                return Err(invalid_affine_stride_operation(
                    validation,
                    "load",
                    stride.op_position,
                    "LoadY, LoadP, or LoadSeed",
                    actual,
                ));
            }
        }
    }
    Ok(())
}

fn validate_affine_constant_strides(
    validation: &AffineValidationContext<'_>,
    const_strides: &[AffineStencilConstStride],
) -> Result<(), SolveProblemShapeContractError> {
    let mut constant_strides_by_op = vec![None::<Vec<f64>>; validation.base_ops.len()];
    for stride in const_strides {
        for term in &stride.terms {
            if !term.stride.is_finite() {
                return Err(
                    SolveProblemShapeContractError::NonFiniteAffineConstantStride {
                        context: validation.context.to_string(),
                        node_index: validation.node_index,
                        dimension: validation.dimension,
                        op_position: stride.op_position,
                        stride_dimension: term.dimension,
                        span: validation.span,
                    },
                );
            }
        }
        validate_affine_stride_dimensions(
            validation,
            "constant",
            stride.op_position,
            &stride.terms,
        )?;
        match validation.base_ops.get(stride.op_position) {
            Some(LinearOp::Const { .. }) => {}
            actual => {
                return Err(invalid_affine_stride_operation(
                    validation,
                    "constant",
                    stride.op_position,
                    "Const",
                    actual,
                ));
            }
        }
        let Some(dimension_strides) = constant_strides_by_op.get_mut(stride.op_position) else {
            continue;
        };
        let dimension_strides =
            dimension_strides.get_or_insert_with(|| vec![0.0; validation.domain.binders.len()]);
        for term in &stride.terms {
            let Some(combined) = dimension_strides.get_mut(term.dimension) else {
                continue;
            };
            *combined += term.stride;
            if !combined.is_finite() {
                return Err(
                    SolveProblemShapeContractError::NonFiniteAffineConstantStride {
                        context: validation.context.to_string(),
                        node_index: validation.node_index,
                        dimension: validation.dimension,
                        op_position: stride.op_position,
                        stride_dimension: term.dimension,
                        span: validation.span,
                    },
                );
            }
        }
    }
    Ok(())
}

trait AffineStrideDimension {
    fn dimension(&self) -> usize;
}

impl AffineStrideDimension for AffineStencilIndexStrideTerm {
    fn dimension(&self) -> usize {
        self.dimension
    }
}

impl AffineStrideDimension for AffineStencilConstStrideTerm {
    fn dimension(&self) -> usize {
        self.dimension
    }
}

fn validate_affine_stride_dimensions<T: AffineStrideDimension>(
    validation: &AffineValidationContext<'_>,
    stride_kind: &'static str,
    op_position: usize,
    terms: &[T],
) -> Result<(), SolveProblemShapeContractError> {
    let domain_rank = validation.domain.binders.len();
    for term in terms {
        if term.dimension() >= domain_rank {
            return Err(SolveProblemShapeContractError::AffineStrideDimension {
                context: validation.context.to_string(),
                node_index: validation.node_index,
                dimension: validation.dimension,
                stride_kind,
                op_position,
                stride_dimension: term.dimension(),
                domain_rank,
                span: validation.span,
            });
        }
    }
    Ok(())
}

fn invalid_affine_stride_operation(
    validation: &AffineValidationContext<'_>,
    stride_kind: &'static str,
    op_position: usize,
    expected: &'static str,
    actual: Option<&LinearOp>,
) -> SolveProblemShapeContractError {
    SolveProblemShapeContractError::AffineStrideOperation {
        context: validation.context.to_string().into_boxed_str(),
        node_index: validation.node_index,
        node_kind: validation.node_kind,
        stride_kind,
        op_position,
        op_count: validation.base_ops.len(),
        expected,
        actual: actual.map(LinearOp::kind_name),
        span: validation.span,
    }
}

fn validate_affine_load_index_ranges(
    validation: &AffineValidationContext<'_>,
    load_strides: &[AffineStencilLoadStride],
) -> Result<(), SolveProblemShapeContractError> {
    let extents = validation.domain.extents().map_err(|error| {
        SolveProblemShapeContractError::StructuredIndexDomain {
            context: validation.context.to_string(),
            node_index: validation.node_index,
            dimension: validation.dimension,
            error,
            span: validation.span,
        }
    })?;
    let mut by_op = vec![None::<Vec<i128>>; validation.base_ops.len()];
    for load_stride in load_strides {
        let Some(strides) = by_op.get_mut(load_stride.op_position) else {
            continue;
        };
        let strides = strides.get_or_insert_with(|| vec![0i128; validation.domain.binders.len()]);
        for term in &load_stride.terms {
            let Some(stride) = strides.get_mut(term.dimension) else {
                continue;
            };
            *stride = stride.checked_add(term.stride as i128).ok_or_else(|| {
                affine_load_index_overflow(
                    validation.context,
                    validation.node_index,
                    validation.dimension,
                    load_stride.op_position,
                    validation.span,
                )
            })?;
        }
    }
    for (op_position, strides) in by_op.into_iter().enumerate() {
        let Some(strides) = strides else {
            continue;
        };
        let base = match validation.base_ops[op_position] {
            LinearOp::LoadY { index, .. }
            | LinearOp::LoadP { index, .. }
            | LinearOp::LoadSeed { index, .. } => index,
            _ => continue,
        };
        let (minimum, maximum) =
            affine_index_bounds(base, &strides, &extents).ok_or_else(|| {
                affine_load_index_overflow(
                    validation.context,
                    validation.node_index,
                    validation.dimension,
                    op_position,
                    validation.span,
                )
            })?;
        if minimum < 0 || usize::try_from(maximum).is_err() {
            return Err(SolveProblemShapeContractError::AffineLoadIndexRange {
                context: validation.context.to_string(),
                node_index: validation.node_index,
                dimension: validation.dimension,
                op_position,
                minimum,
                maximum,
                span: validation.span,
            });
        }
    }
    Ok(())
}

fn affine_index_bounds(base: usize, strides: &[i128], extents: &[usize]) -> Option<(i128, i128)> {
    let mut minimum = i128::try_from(base).ok()?;
    let mut maximum = minimum;
    for (&stride, &extent) in strides.iter().zip(extents) {
        let last = i128::try_from(extent.saturating_sub(1)).ok()?;
        let offset = last.checked_mul(stride)?;
        if offset < 0 {
            minimum = minimum.checked_add(offset)?;
        } else {
            maximum = maximum.checked_add(offset)?;
        }
    }
    Some((minimum, maximum))
}

fn affine_load_index_overflow(
    context: &str,
    node_index: usize,
    dimension: &'static str,
    op_position: usize,
    span: Span,
) -> SolveProblemShapeContractError {
    SolveProblemShapeContractError::AffineLoadIndexOverflow {
        context: context.to_string(),
        node_index,
        dimension,
        op_position,
        span,
    }
}

fn validate_tensor_domain(
    context: &str,
    node_index: usize,
    dimension: &'static str,
    domain: &StructuredIndexDomain,
    span: Span,
) -> Result<usize, SolveProblemShapeContractError> {
    domain.validate().map_err(
        |err| SolveProblemShapeContractError::StructuredIndexDomain {
            context: context.to_string(),
            node_index,
            dimension,
            error: err,
            span,
        },
    )
}

fn validate_tensor_output_map(
    context: &str,
    node_index: usize,
    dimension: &'static str,
    domain: &StructuredIndexDomain,
    output_map: &TensorOutputMap,
    span: Span,
) -> Result<(), SolveProblemShapeContractError> {
    for term in &output_map.strides {
        if term.dimension >= domain.binders.len() {
            return Err(SolveProblemShapeContractError::TensorOutputMapDimension {
                context: context.to_string(),
                node_index,
                dimension,
                output_dimension: term.dimension,
                domain_rank: domain.binders.len(),
                span,
            });
        }
    }
    // Validate compactly from the affine bounds. Materializing every domain
    // point here would make the Solve-IR contract itself scalar in tensor
    // cardinality, defeating the stage's compact-domain guarantee.
    output_map
        .output_count(domain)
        .map_err(|error| tensor_output_map_error(context, node_index, dimension, error, span))?;
    Ok(())
}

impl Serialize for ComputeBlock {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        #[derive(Serialize)]
        struct Ser<'a> {
            nodes: &'a Vec<ComputeNode>,
        }
        Ser { nodes: &self.nodes }.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for ComputeBlock {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(Deserialize)]
        struct Wire {
            nodes: Vec<ComputeNode>,
        }

        let wire = Wire::deserialize(deserializer)?;
        Ok(Self { nodes: wire.nodes })
    }
}

/// Register range for a tensor operand in a `ComputeNode`.
///
/// Shapes follow Modelica's row-major convention. Used in Phase 2 tensor ops.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum TensorSource {
    /// Contiguous virtual registers `start..start+product(shape)`, row-major.
    Regs { start: Reg, shape: Vec<usize> },
    /// Contiguous slice of the `y[]` state/algebraic vector.
    ///
    /// Shapes must agree with `VarLayout::shapes` for the same variable name.
    /// Construct via `VarLayout::y_slice` to guarantee shape presence.
    YSlice { start: usize, shape: Vec<usize> },
    /// Contiguous slice of the `p[]` parameter vector.
    ///
    /// Construct via `VarLayout::p_slice` to guarantee shape presence.
    PSlice { start: usize, shape: Vec<usize> },
}

impl VarLayout {
    /// Construct a `TensorSource::YSlice` for a named Y-slot array variable.
    ///
    /// Returns `None` if the variable is not in Y-storage, is scalar, or its
    /// shape is not recorded in the layout (e.g., truncated by `solver_len`).
    /// Prefer this over constructing `YSlice` directly to avoid shape-mismatch
    /// errors when the slice is consumed by a backend.
    pub fn y_slice(&self, name: &str) -> Option<TensorSource> {
        let shape = self.shape(name)?.to_vec();
        let start = match self.binding(name)? {
            ScalarSlot::Y { index, .. } => index,
            _ => return None,
        };
        Some(TensorSource::YSlice { start, shape })
    }

    /// Construct a `TensorSource::PSlice` for a named P-slot array variable.
    ///
    /// Returns `None` if the variable is not in P-storage, is scalar, or its
    /// shape is not recorded in the layout.
    pub fn p_slice(&self, name: &str) -> Option<TensorSource> {
        let shape = self.shape(name)?.to_vec();
        let start = match self.binding(name)? {
            ScalarSlot::P { index, .. } => index,
            _ => return None,
        };
        Some(TensorSource::PSlice { start, shape })
    }
}
