//! Solver-facing Solve IR.
//!
//! This crate contains data consumed by simulation backends after DAE-level
//! structural/lowering phases. It must stay free of DAE evaluation and phase
//! logic.
//!
//! SPEC_0021 file-size exception: Solve IR still defines scalar rows, tensor
//! nodes, validation, and visitor contracts in one facade. split plan: move
//! tensor contracts, validation errors, and visitors into focused modules.

#[cfg(test)]
mod compute_block_tests;
mod layout;
mod linear_op;
pub mod visitor;

use indexmap::IndexMap;
use rumoca_core::{
    ExternalTableData, SourceId, Span, StructuredIndexDomain, StructuredIndexDomainError,
};
use serde::{Deserialize, Serialize};

pub use layout::{
    ComponentReferenceKey, ComponentReferenceKeyError, ComponentReferenceKeyErrorKind,
    ComponentReferenceKeyPart, ComponentReferenceSubscriptKey, IndexedScalarSlot, ScalarSlot,
    VarLayout, VarLayoutShapeContractError, scalar_slot_p, scalar_slot_y,
};
pub use linear_op::{
    BinaryOp, CompareOp, LinearOp, RandomGenerator, Reg, UnaryOp, resolve_indexed_slot,
};
pub use visitor::{
    LinearOpSliceKind, SolveVisitor, VisitScope, walk_compute_block, walk_compute_node,
    walk_scalar_program_block, walk_solve_artifacts, walk_solve_model, walk_solve_problem,
};

pub const SOLVE_SCHEMA_VERSION: u16 = 14;

pub fn source_span_from_offsets(source: u64, start: usize, end: usize) -> Span {
    Span::from_offsets(SourceId(source), start, end)
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ExternalTables {
    tables: Vec<ExternalTableData>,
}

impl ExternalTables {
    pub fn new(tables: Vec<ExternalTableData>) -> Self {
        Self { tables }
    }

    pub fn as_slice(&self) -> &[ExternalTableData] {
        &self.tables
    }

    pub fn is_empty(&self) -> bool {
        self.tables.is_empty()
    }

    pub fn len(&self) -> usize {
        self.tables.len()
    }

    pub fn push_table(
        &mut self,
        id: u64,
        data: Vec<Vec<f64>>,
        columns: Vec<usize>,
        smoothness: i64,
        extrapolation: i64,
    ) {
        self.tables.push(ExternalTableData {
            id,
            data,
            columns,
            smoothness,
            extrapolation,
        });
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ScalarProgramBlock {
    pub programs: Vec<Vec<LinearOp>>,
    pub program_spans: Vec<Span>,
    pub output_indices: Vec<usize>,
}

impl ScalarProgramBlock {
    pub fn with_program_spans(
        programs: Vec<Vec<LinearOp>>,
        program_spans: Vec<Span>,
    ) -> Result<Self, SolveProblemShapeContractError> {
        let output_indices = (0..stored_output_count(&programs)).collect();
        Self::with_output_indices(programs, program_spans, output_indices)
    }

    pub fn with_output_indices(
        programs: Vec<Vec<LinearOp>>,
        program_spans: Vec<Span>,
        output_indices: Vec<usize>,
    ) -> Result<Self, SolveProblemShapeContractError> {
        validate_scalar_program_metadata_lengths(
            "ScalarProgramBlock",
            0,
            programs.len(),
            program_spans.len(),
            stored_output_count(&programs),
            output_indices.len(),
            first_span(&program_spans),
        )?;
        Ok(Self::from_valid_parts(
            programs,
            program_spans,
            output_indices,
        ))
    }

    fn from_valid_parts(
        programs: Vec<Vec<LinearOp>>,
        program_spans: Vec<Span>,
        output_indices: Vec<usize>,
    ) -> Self {
        Self {
            programs,
            program_spans,
            output_indices,
        }
    }

    pub fn with_contiguous_output_indices(
        programs: Vec<Vec<LinearOp>>,
        program_spans: Vec<Span>,
        start: usize,
    ) -> Result<Self, SolveProblemShapeContractError> {
        let end = start
            .checked_add(stored_output_count(&programs))
            .ok_or_else(|| {
                output_index_overflow("ScalarProgramBlock", 0, first_span(&program_spans))
            })?;
        let output_indices = (start..end).collect();
        Self::with_output_indices(programs, program_spans, output_indices)
    }

    pub fn with_source_span(programs: Vec<Vec<LinearOp>>, span: Span) -> Self {
        let program_spans = vec![span; programs.len()];
        let output_indices = (0..stored_output_count(&programs)).collect();
        Self::from_valid_parts(programs, program_spans, output_indices)
    }

    pub fn program_span(&self, row: usize) -> Option<Span> {
        self.program_spans
            .get(row)
            .copied()
            .filter(|span| !span.is_dummy())
    }

    pub fn first_source_span(&self) -> Option<Span> {
        self.program_spans
            .iter()
            .copied()
            .find(|span| !span.is_dummy())
    }

    /// Number of `StoreOutput` ops in a single program.
    ///
    /// A program may emit more than one output: matmul/linsolve nodes lower to
    /// one self-contained program that computes its operands once and stores
    /// every result via consecutive `StoreOutput` ops.
    pub fn program_output_count(program: &[LinearOp]) -> usize {
        program
            .iter()
            .filter(|op| matches!(op, LinearOp::StoreOutput { .. }))
            .count()
    }

    /// Total number of `StoreOutput` ops produced by this block.
    pub fn stored_output_count(&self) -> usize {
        self.programs
            .iter()
            .map(|program| Self::program_output_count(program))
            .sum()
    }

    /// Map a dense output slot to the program that produces it.
    ///
    /// `output_indices` may be sparse, so this first maps the output slot to
    /// its stored-output ordinal and then finds the owning program.
    pub fn program_index_for_output(&self, output: usize) -> Option<usize> {
        let mut remaining = self
            .output_indices
            .iter()
            .position(|output_index| *output_index == output)?;
        for (idx, program) in self.programs.iter().enumerate() {
            let count = Self::program_output_count(program);
            if remaining < count {
                return Some(idx);
            }
            remaining -= count;
        }
        None
    }

    /// Source span for a dense output slot, looked up via its owning program.
    ///
    /// All outputs of a matmul/linsolve program share the node's span, matching
    /// the pre-existing per-node span attribution.
    pub fn span_for_output(&self, output: usize) -> Option<Span> {
        let program_index = self.program_index_for_output(output)?;
        self.program_span(program_index)
    }

    pub fn len(&self) -> usize {
        self.output_count()
    }

    pub fn row_count(&self) -> usize {
        self.programs.len()
    }

    pub fn output_count(&self) -> usize {
        self.output_indices
            .iter()
            .copied()
            .max()
            .map_or(0, |index| index + 1)
    }

    pub fn uses_local_contiguous_output_indices(&self) -> bool {
        self.output_indices
            .iter()
            .copied()
            .eq(0..self.stored_output_count())
    }

    pub fn compute_block_output_indices(
        &self,
        context: &str,
        node_index: usize,
        output_cursor: usize,
    ) -> Result<Vec<usize>, SolveProblemShapeContractError> {
        if self.uses_local_contiguous_output_indices() {
            let end = output_cursor
                .checked_add(self.stored_output_count())
                .ok_or_else(|| {
                    output_index_overflow(context, node_index, self.first_program_span())
                })?;
            Ok((output_cursor..end).collect())
        } else {
            Ok(self.output_indices.clone())
        }
    }

    pub fn placed_in_compute_block(
        &self,
        context: &str,
        node_index: usize,
        output_cursor: usize,
    ) -> Result<Self, SolveProblemShapeContractError> {
        Self::with_output_indices(
            self.programs.clone(),
            self.program_spans.clone(),
            self.compute_block_output_indices(context, node_index, output_cursor)?,
        )
    }

    pub fn advance_compute_block_output_cursor(
        &self,
        context: &str,
        node_index: usize,
        output_cursor: usize,
    ) -> Result<usize, SolveProblemShapeContractError> {
        let Some(max_index) = self
            .compute_block_output_indices(context, node_index, output_cursor)?
            .into_iter()
            .max()
        else {
            return Ok(output_cursor);
        };
        let next = max_index
            .checked_add(1)
            .ok_or_else(|| output_index_overflow(context, node_index, self.first_program_span()))?;
        Ok(output_cursor.max(next))
    }

    pub fn is_empty(&self) -> bool {
        self.programs.is_empty()
    }

    fn first_program_span(&self) -> Option<Span> {
        self.first_source_span()
    }

    pub fn validate_shape_contract(
        &self,
        context: impl Into<String>,
    ) -> Result<(), SolveProblemShapeContractError> {
        let context = context.into();
        if self.program_spans.len() != self.programs.len() {
            return Err(SolveProblemShapeContractError::ScalarProgramSpanMismatch {
                context,
                node_index: 0,
                programs: self.programs.len(),
                spans: self.program_spans.len(),
                span: self.first_program_span(),
            });
        }
        if self.output_indices.len() != self.stored_output_count() {
            return Err(
                SolveProblemShapeContractError::ScalarProgramOutputIndexMismatch {
                    context,
                    node_index: 0,
                    programs: self.stored_output_count(),
                    output_indices: self.output_indices.len(),
                    span: self.first_program_span(),
                },
            );
        }
        Ok(())
    }
}

fn first_span(spans: &[Span]) -> Option<Span> {
    spans.iter().copied().find(|span| !span.is_dummy())
}

fn stored_output_count(programs: &[Vec<LinearOp>]) -> usize {
    programs
        .iter()
        .map(|program| ScalarProgramBlock::program_output_count(program))
        .sum()
}

fn validate_scalar_program_metadata_lengths(
    context: impl Into<String>,
    node_index: usize,
    programs: usize,
    spans: usize,
    stored_outputs: usize,
    output_indices: usize,
    span: Option<Span>,
) -> Result<(), SolveProblemShapeContractError> {
    let context = context.into();
    if spans != programs {
        return Err(SolveProblemShapeContractError::ScalarProgramSpanMismatch {
            context,
            node_index,
            programs,
            spans,
            span,
        });
    }
    if output_indices != stored_outputs {
        return Err(
            SolveProblemShapeContractError::ScalarProgramOutputIndexMismatch {
                context,
                node_index,
                programs: stored_outputs,
                output_indices,
                span,
            },
        );
    }
    Ok(())
}

/// Sparsity annotation for a tensor operand in a `ComputeNode`.
///
/// Used by backends to emit optimized kernels (e.g., diagonal multiply instead of GEMM)
/// and by sparsity-aware Jacobian builders to skip probing for known-zero entries.
/// `Dense` is always a conservative fallback — it never causes incorrect results.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum SparsityPattern {
    /// All entries may be nonzero (conservative default).
    #[default]
    Dense,
    /// Square matrix with nonzero entries only on the main diagonal.
    Diagonal,
    /// Explicit set of (row, col) nonzero positions in row-major order.
    Explicit { nnz: Vec<(usize, usize)> },
}

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
        let Some(max_index) = self.output_indices(domain)?.into_iter().max() else {
            return Ok(0);
        };
        max_index
            .checked_add(1)
            .ok_or(TensorOutputMapError::OutputIndexOverflow)
    }

    fn output_index(
        &self,
        domain: &StructuredIndexDomain,
        base_tuple: &[i64],
        index_tuple: &[i64],
    ) -> Result<usize, TensorOutputMapError> {
        let mut value =
            isize::try_from(self.start).map_err(|_| TensorOutputMapError::OutputIndexOverflow)?;
        for term in &self.strides {
            if term.dimension >= domain.binders.len() {
                return Err(TensorOutputMapError::Dimension {
                    output_dimension: term.dimension,
                    domain_rank: domain.binders.len(),
                });
            }
            let delta = isize::try_from(output_ordinal_delta(
                term.dimension,
                domain,
                base_tuple,
                index_tuple,
            ))
            .map_err(|_| TensorOutputMapError::OutputIndexOverflow)?;
            let offset = delta
                .checked_mul(term.stride)
                .ok_or(TensorOutputMapError::OutputIndexOverflow)?;
            value = value
                .checked_add(offset)
                .ok_or(TensorOutputMapError::OutputIndexOverflow)?;
        }
        usize::try_from(value).map_err(|_| TensorOutputMapError::NegativeIndex { value })
    }
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
        #[serde(default)]
        lhs_sparsity: SparsityPattern,
        /// Sparsity of the rhs (B) operand.  `Dense` unless statically known.
        #[serde(default)]
        rhs_sparsity: SparsityPattern,
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
    context: &'static str,
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

fn output_index_overflow(
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
            ComputeNode::Map {
                domain,
                output_map,
                span,
                ..
            } => {
                let count = validate_tensor_domain(context, node_index, "Map", domain, *span)?;
                if count == 0 {
                    return Err(SolveProblemShapeContractError::ZeroTensorDimension {
                        context: context.to_string(),
                        node_index,
                        dimension: "Map",
                        span: *span,
                    });
                }
                validate_tensor_output_map(context, node_index, "Map", domain, output_map, *span)?;
            }
            ComputeNode::AffineStencil {
                domain,
                output_map,
                span,
                ..
            } => {
                let count =
                    validate_tensor_domain(context, node_index, "AffineStencil", domain, *span)?;
                if count == 0 {
                    return Err(SolveProblemShapeContractError::ZeroTensorDimension {
                        context: context.to_string(),
                        node_index,
                        dimension: "AffineStencil",
                        span: *span,
                    });
                }
                validate_tensor_output_map(
                    context,
                    node_index,
                    "AffineStencil",
                    domain,
                    output_map,
                    *span,
                )?;
            }
        }
        Ok(())
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
    if domain
        .index_tuples()
        .map_err(
            |error| SolveProblemShapeContractError::StructuredIndexDomain {
                context: context.to_string(),
                node_index,
                dimension,
                error,
                span,
            },
        )?
        .is_empty()
    {
        return Ok(());
    }
    output_map.output_indices(domain).map_err(|err| match err {
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
    })?;
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

#[cfg(test)]
mod tests;

#[derive(Clone, Debug, Serialize)]
pub struct SolveProblem {
    pub schema_version: u16,
    pub layout: VarLayout,
    pub solve_layout: SolveLayout,
    pub continuous: ContinuousSolveSystem,
    pub initialization: InitializationSolveSystem,
    pub discrete: DiscreteSolveSystem,
    pub events: SolveEventPartition,
    pub clocks: SolveClockPartition,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct SolveProblemWire {
    schema_version: u16,
    layout: VarLayout,
    solve_layout: SolveLayout,
    continuous: ContinuousSolveSystem,
    initialization: InitializationSolveSystem,
    discrete: DiscreteSolveSystem,
    events: SolveEventPartition,
    clocks: SolveClockPartition,
}

impl Default for SolveProblem {
    fn default() -> Self {
        Self {
            schema_version: SOLVE_SCHEMA_VERSION,
            layout: VarLayout::default(),
            solve_layout: SolveLayout::default(),
            continuous: ContinuousSolveSystem::default(),
            initialization: InitializationSolveSystem::default(),
            discrete: DiscreteSolveSystem::default(),
            events: SolveEventPartition::default(),
            clocks: SolveClockPartition::default(),
        }
    }
}

impl<'de> Deserialize<'de> for SolveProblem {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let wire = SolveProblemWire::deserialize(deserializer)?;
        if wire.schema_version != SOLVE_SCHEMA_VERSION {
            return Err(serde::de::Error::custom(format!(
                "unsupported Solve schema_version {}; expected {}",
                wire.schema_version, SOLVE_SCHEMA_VERSION
            )));
        }

        Ok(Self {
            schema_version: wire.schema_version,
            layout: wire.layout,
            solve_layout: wire.solve_layout,
            continuous: wire.continuous,
            initialization: wire.initialization,
            discrete: wire.discrete,
            events: wire.events,
            clocks: wire.clocks,
        })
    }
}

impl SolveProblem {
    pub fn with_derivative_rhs(derivative_rhs: ComputeBlock) -> Self {
        Self {
            continuous: ContinuousSolveSystem {
                derivative_rhs,
                ..ContinuousSolveSystem::default()
            },
            ..Self::default()
        }
    }

    pub fn compute_node_counts(&self) -> ComputeNodeCounts {
        let mut counts = self.continuous.implicit_rhs.compute_node_counts();
        counts.add_assign(self.continuous.residual.compute_node_counts());
        counts.add_assign(self.continuous.derivative_rhs.compute_node_counts());
        counts
    }

    pub fn validate_shape_contract(&self) -> Result<(), SolveProblemShapeContractError> {
        if self.schema_version != SOLVE_SCHEMA_VERSION {
            return Err(SolveProblemShapeContractError::SchemaVersion {
                actual: self.schema_version,
                expected: SOLVE_SCHEMA_VERSION,
            });
        }
        self.layout
            .validate_shape_contract()
            .map_err(SolveProblemShapeContractError::Layout)?;
        self.continuous
            .implicit_rhs
            .validate_shape_contract("continuous.implicit_rhs")?;
        self.continuous
            .residual
            .validate_shape_contract("continuous.residual")?;
        self.continuous
            .derivative_rhs
            .validate_shape_contract("continuous.derivative_rhs")?;
        validate_count(
            "continuous.implicit_row_targets",
            self.continuous
                .implicit_rhs
                .output_count("continuous.implicit_rhs")?,
            self.continuous.implicit_row_targets.len(),
        )?;
        self.initialization
            .residual
            .validate_shape_contract("initialization.residual")?;
        self.initialization
            .update_rhs
            .validate_shape_contract("initialization.update_rhs")?;
        validate_count(
            "initialization.row_targets",
            self.initialization.residual.len()?,
            self.initialization.row_targets.len(),
        )?;
        validate_count(
            "initialization.update_targets",
            self.initialization.update_rhs.len(),
            self.initialization.update_targets.len(),
        )?;
        validate_indices(
            "initialization.projection_indices",
            &self.initialization.projection_indices,
            self.solve_layout.solver_scalar_count(),
        )?;
        validate_projection_plan(
            "initialization.projection_plan",
            &self.initialization.projection_plan,
            self.initialization.residual.len()?,
            self.solve_layout.solver_scalar_count(),
        )?;
        self.discrete
            .runtime_assignment_rhs
            .validate_shape_contract("discrete.runtime_assignment_rhs")?;
        self.discrete.rhs.validate_shape_contract("discrete.rhs")?;
        validate_count(
            "discrete.runtime_assignment_targets",
            self.discrete.runtime_assignment_rhs.len(),
            self.discrete.runtime_assignment_targets.len(),
        )?;
        validate_count(
            "discrete.update_targets",
            self.discrete.rhs.len(),
            self.discrete.update_targets.len(),
        )?;
        self.events
            .root_conditions
            .validate_shape_contract("events.root_conditions")?;
        self.events
            .dynamic_time_event_rhs
            .validate_shape_contract("events.dynamic_time_event_rhs")?;
        self.events
            .action_conditions
            .validate_shape_contract("events.action_conditions")?;
        validate_count(
            "events.action_conditions",
            self.events.actions.len(),
            self.events.action_conditions.len(),
        )?;
        Ok(())
    }
}

fn validate_count(
    context: &'static str,
    expected: usize,
    actual: usize,
) -> Result<(), SolveProblemShapeContractError> {
    if expected == actual {
        return Ok(());
    }
    Err(SolveProblemShapeContractError::ScalarProgramCountMismatch {
        context,
        expected,
        actual,
        span: None,
    })
}

fn validate_indices(
    context: &'static str,
    indices: &[usize],
    upper_bound: usize,
) -> Result<(), SolveProblemShapeContractError> {
    for &index in indices {
        if index < upper_bound {
            continue;
        }
        return Err(SolveProblemShapeContractError::SolverIndexOutOfBounds {
            context,
            index,
            upper_bound,
            span: None,
        });
    }
    Ok(())
}

fn validate_projection_plan(
    context: &'static str,
    plan: &AlgebraicProjectionPlan,
    row_upper_bound: usize,
    y_upper_bound: usize,
) -> Result<(), SolveProblemShapeContractError> {
    for block in &plan.blocks {
        validate_indices(context, &block.rows, row_upper_bound)?;
        validate_indices(context, &block.y_indices, y_upper_bound)?;
        for step in &block.causal_steps {
            validate_indices(context, &[step.row], row_upper_bound)?;
            validate_indices(context, &[step.y_index], y_upper_bound)?;
        }
    }
    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SolveProblemShapeContractError {
    SchemaVersion {
        actual: u16,
        expected: u16,
    },
    Layout(VarLayoutShapeContractError),
    ScalarProgramSpanMismatch {
        context: String,
        node_index: usize,
        programs: usize,
        spans: usize,
        span: Option<Span>,
    },
    ScalarProgramOutputIndexMismatch {
        context: String,
        node_index: usize,
        programs: usize,
        output_indices: usize,
        span: Option<Span>,
    },
    ScalarProgramCountMismatch {
        context: &'static str,
        expected: usize,
        actual: usize,
        span: Option<Span>,
    },
    ZeroTensorDimension {
        context: String,
        node_index: usize,
        dimension: &'static str,
        span: Span,
    },
    StructuredIndexDomain {
        context: String,
        node_index: usize,
        dimension: &'static str,
        error: StructuredIndexDomainError,
        span: Span,
    },
    TensorOutputMapDimension {
        context: String,
        node_index: usize,
        dimension: &'static str,
        output_dimension: usize,
        domain_rank: usize,
        span: Span,
    },
    TensorOutputMapNegativeIndex {
        context: String,
        node_index: usize,
        dimension: &'static str,
        value: isize,
        span: Span,
    },
    OutputIndexOverflow {
        context: String,
        node_index: usize,
        span: Option<Span>,
    },
    SolverIndexOutOfBounds {
        context: &'static str,
        index: usize,
        upper_bound: usize,
        span: Option<Span>,
    },
}

impl SolveProblemShapeContractError {
    pub fn source_span(&self) -> Option<Span> {
        match self {
            Self::SchemaVersion { .. } => None,
            Self::Layout(err) => err.source_span(),
            Self::ScalarProgramSpanMismatch { span, .. }
            | Self::ScalarProgramOutputIndexMismatch { span, .. }
            | Self::ScalarProgramCountMismatch { span, .. }
            | Self::OutputIndexOverflow { span, .. }
            | Self::SolverIndexOutOfBounds { span, .. } => *span,
            Self::ZeroTensorDimension { span, .. }
            | Self::StructuredIndexDomain { span, .. }
            | Self::TensorOutputMapDimension { span, .. }
            | Self::TensorOutputMapNegativeIndex { span, .. } => Some(*span),
        }
    }
}

impl std::fmt::Display for SolveProblemShapeContractError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SchemaVersion { actual, expected } => {
                write!(
                    f,
                    "Solve schema version {actual} does not match expected {expected}"
                )
            }
            Self::Layout(err) => write!(f, "Solve layout shape contract failed: {err}"),
            Self::ScalarProgramSpanMismatch {
                context,
                node_index,
                programs,
                spans,
                ..
            } => write!(
                f,
                "{context} node {node_index} has {programs} scalar programs but {spans} spans"
            ),
            Self::ScalarProgramOutputIndexMismatch {
                context,
                node_index,
                programs,
                output_indices,
                ..
            } => write!(
                f,
                "{context} node {node_index} has {programs} scalar programs but \
                 {output_indices} output indices"
            ),
            Self::ScalarProgramCountMismatch {
                context,
                expected,
                actual,
                ..
            } => write!(f, "{context} expected {expected} rows, got {actual}"),
            Self::ZeroTensorDimension {
                context,
                node_index,
                dimension,
                ..
            } => write!(
                f,
                "{context} node {node_index} has zero {dimension} tensor dimension"
            ),
            Self::StructuredIndexDomain {
                context,
                node_index,
                dimension,
                error,
                ..
            } => write!(
                f,
                "{context} node {node_index} {dimension} domain is invalid: {error}"
            ),
            Self::TensorOutputMapDimension {
                context,
                node_index,
                dimension,
                output_dimension,
                domain_rank,
                ..
            } => write!(
                f,
                "{context} node {node_index} {dimension} output map references dimension \
                 {output_dimension}, but domain rank is {domain_rank}"
            ),
            Self::TensorOutputMapNegativeIndex {
                context,
                node_index,
                dimension,
                value,
                ..
            } => write!(
                f,
                "{context} node {node_index} {dimension} output map produced negative output index {value}"
            ),
            Self::OutputIndexOverflow {
                context,
                node_index,
                ..
            } => write!(
                f,
                "{context} node {node_index} output index arithmetic overflowed"
            ),
            Self::SolverIndexOutOfBounds {
                context,
                index,
                upper_bound,
                ..
            } => write!(
                f,
                "{context} references solver index {index}, but upper bound is {upper_bound}"
            ),
        }
    }
}

impl std::error::Error for SolveProblemShapeContractError {}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ContinuousSolveSystem {
    pub implicit_rhs: ComputeBlock,
    pub implicit_row_targets: Vec<Option<ScalarSlot>>,
    pub algebraic_projection_plan: AlgebraicProjectionPlan,
    pub residual: ComputeBlock,
    pub derivative_rhs: ComputeBlock,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct AlgebraicProjectionPlan {
    pub blocks: Vec<AlgebraicProjectionBlock>,
}

impl AlgebraicProjectionPlan {
    pub fn is_empty(&self) -> bool {
        self.blocks.is_empty()
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct AlgebraicProjectionBlock {
    pub rows: Vec<usize>,
    pub y_indices: Vec<usize>,
    #[serde(default)]
    pub causal_steps: Vec<AlgebraicProjectionStep>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct AlgebraicProjectionStep {
    pub row: usize,
    pub y_index: usize,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveArtifacts {
    pub continuous: ContinuousSolveArtifacts,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ContinuousSolveArtifacts {
    #[serde(default)]
    pub mass_matrix: Vec<Vec<f64>>,
    pub implicit_jacobian_v: ComputeBlock,
    /// Per-row forward-mode AD JVP of the *scalarized* `implicit_rhs`, row-aligned
    /// with successful `to_scalar_program_block(implicit_rhs)` output (and hence
    /// with the algebraic refresh plan's `row_idx`). Used by the state-only path
    /// to propagate the state seed through the algebraic projection
    /// (`d(alg)/d(state)`). Distinct from the tensor `implicit_jacobian_v`, whose
    /// scalarization is not row-aligned when the system has linear
    /// (`LinSolve`/`MatMul`) blocks.
    #[serde(default)]
    pub implicit_jacobian_v_scalar: ScalarProgramBlock,
    pub full_jacobian_v: ScalarProgramBlock,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InitializationSolveSystem {
    pub residual: ComputeBlock,
    pub row_targets: Vec<Option<ScalarSlot>>,
    pub projection_indices: Vec<usize>,
    #[serde(default)]
    pub projection_plan: AlgebraicProjectionPlan,
    #[serde(default)]
    pub update_rhs: ScalarProgramBlock,
    #[serde(default)]
    pub update_targets: Vec<ScalarSlot>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct DiscreteSolveSystem {
    pub runtime_assignment_rhs: ScalarProgramBlock,
    pub runtime_assignment_targets: Vec<ScalarSlot>,
    pub rhs: ScalarProgramBlock,
    pub update_targets: Vec<ScalarSlot>,
    pub pre_modes: Vec<DiscreteEventPreMode>,
    pub observation_refresh: Vec<bool>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveEventPartition {
    pub root_conditions: ScalarProgramBlock,
    pub root_relation_memory_targets: Vec<Option<ScalarSlot>>,
    pub scheduled_time_events: Vec<f64>,
    pub dynamic_time_event_names: Vec<String>,
    pub dynamic_time_event_rhs: ScalarProgramBlock,
    pub action_conditions: ScalarProgramBlock,
    pub actions: Vec<SolveEventAction>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct SolveEventAction {
    pub kind: SolveEventActionKind,
    pub message: SolveEventMessage,
    pub span: rumoca_core::Span,
    pub origin: String,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveEventMessage {
    pub parts: Vec<SolveEventMessagePart>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum SolveEventMessagePart {
    Text(String),
    Number(Vec<LinearOp>),
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub enum SolveEventActionKind {
    Assert,
    Terminate,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveClockPartition {
    pub periodic_event_schedules: Vec<PeriodicEventSchedule>,
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub enum DiscreteEventPreMode {
    /// Use the value from the start of the current clock/event tick.
    EventEntry,
    /// Hold `pre(..)` fixed for one event-iteration pass.
    Fixed,
    /// Read the current event-iteration fixed-point state.
    #[default]
    FollowCurrent,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct PeriodicEventSchedule {
    pub period_seconds: f64,
    pub phase_seconds: f64,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolverNameIndexMaps {
    pub names: Vec<String>,
    pub name_to_idx: IndexMap<String, usize>,
    pub base_to_indices: IndexMap<String, Vec<usize>>,
}

/// Source slot for a `__pre__.*` parameter binding.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum PreParamSource {
    /// Copy from `y[index]` at event entry.
    Y { index: usize },
    /// Copy from `p[index]` (snapshot) at event entry.
    P { index: usize },
}

/// Maps a `__pre__.*` parameter's P-slot to the source slot it should be
/// snapshot-copied from at event entry. Built by phase-solve-lower from the
/// VarLayout after DAE-IR pre_lowering has run.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PreParamBinding {
    pub dest_p_index: usize,
    pub source: PreParamSource,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveLayout {
    pub solver_maps: SolverNameIndexMaps,
    pub state_scalar_count: usize,
    pub algebraic_scalar_count: usize,
    pub output_scalar_count: usize,
    pub parameter_count: usize,
    pub compiled_parameter_len: usize,
    pub input_scalar_names: Vec<String>,
    pub discrete_real_scalar_names: Vec<String>,
    pub discrete_valued_scalar_names: Vec<String>,
    #[serde(default)]
    pub relation_memory_parameter_indices: Vec<usize>,
    #[serde(default)]
    pub initial_event_parameter_index: Option<usize>,
    /// Snapshot bindings for `__pre__.*` parameters created by DAE-IR
    /// pre_lowering. At event entry the runtime copies each source slot into
    /// the corresponding dest P-slot before the event equations evaluate.
    #[serde(default)]
    pub pre_param_bindings: Vec<PreParamBinding>,
}

impl SolveLayout {
    pub fn solver_maps(&self) -> &SolverNameIndexMaps {
        &self.solver_maps
    }

    pub fn state_scalar_count(&self) -> usize {
        self.state_scalar_count
    }

    pub fn algebraic_scalar_count(&self) -> usize {
        self.algebraic_scalar_count
    }

    pub fn output_scalar_count(&self) -> usize {
        self.output_scalar_count
    }

    pub fn solver_scalar_count(&self) -> usize {
        self.solver_maps.names.len()
    }

    pub fn input_scalar_names(&self) -> &[String] {
        &self.input_scalar_names
    }

    pub fn input_parameter_index(&self, name: &str) -> Option<usize> {
        self.input_scalar_names
            .iter()
            .position(|candidate| candidate == name)
            .map(|offset| self.parameter_count + offset)
    }

    pub fn discrete_real_parameter_index(&self, name: &str) -> Option<usize> {
        self.discrete_real_scalar_names
            .iter()
            .position(|candidate| candidate == name)
            .map(|offset| self.parameter_count + self.input_scalar_names.len() + offset)
    }

    pub fn discrete_valued_parameter_index(&self, name: &str) -> Option<usize> {
        self.discrete_valued_scalar_names
            .iter()
            .position(|candidate| candidate == name)
            .map(|offset| {
                self.parameter_count
                    + self.input_scalar_names.len()
                    + self.discrete_real_scalar_names.len()
                    + offset
            })
    }

    pub fn has_runtime_parameter_tail(&self) -> bool {
        !self.input_scalar_names.is_empty()
            || !self.discrete_real_scalar_names.is_empty()
            || !self.discrete_valued_scalar_names.is_empty()
    }

    pub fn solver_idx_for_target(&self, target: &str) -> Option<usize> {
        solver_idx_for_target(target, &self.solver_maps.name_to_idx)
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct SolveVariableMeta {
    pub name: String,
    pub source_span: Span,
    pub role: String,
    pub is_state: bool,
    pub value_type: Option<String>,
    pub variability: Option<String>,
    pub time_domain: Option<String>,
    pub unit: Option<String>,
    pub start: Option<String>,
    pub min: Option<String>,
    pub max: Option<String>,
    pub nominal: Option<String>,
    pub fixed: Option<bool>,
    pub description: Option<String>,
}

impl SolveVariableMeta {
    pub fn empty_with_span(source_span: Span) -> Self {
        Self {
            name: String::new(),
            source_span,
            role: String::new(),
            is_state: bool::default(),
            value_type: None,
            variability: None,
            time_domain: None,
            unit: None,
            start: None,
            min: None,
            max: None,
            nominal: None,
            fixed: None,
            description: None,
        }
    }
}

/// Solver-facing Solve IR package.
///
/// This is pure data. DAE inspection, scalarization, start evaluation, and
/// mass-matrix extraction happen before this value is constructed.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveModel {
    pub problem: SolveProblem,
    pub artifacts: SolveArtifacts,
    pub initial_y: Vec<f64>,
    pub parameters: Vec<f64>,
    #[serde(default)]
    pub external_tables: ExternalTables,
    pub visible_names: Vec<String>,
    #[serde(default)]
    pub visible_value_rows: ScalarProgramBlock,
    pub variable_meta: Vec<SolveVariableMeta>,
}

impl SolveModel {
    pub fn state_scalar_count(&self) -> usize {
        self.problem.solve_layout.state_scalar_count()
    }

    pub fn solver_scalar_count(&self) -> usize {
        self.problem.solve_layout.solver_scalar_count()
    }

    pub fn initialization_projection_indices(&self) -> &[usize] {
        &self.problem.initialization.projection_indices
    }
}

pub fn solver_idx_for_target(target: &str, name_to_idx: &IndexMap<String, usize>) -> Option<usize> {
    if let Some(&idx) = name_to_idx.get(target) {
        return Some(idx);
    }
    if let Some(scalar) = rumoca_core::parse_scalar_name(target)
        && scalar.indices.iter().all(|index| *index == 1)
    {
        return name_to_idx.get(scalar.base).copied();
    }
    None
}
