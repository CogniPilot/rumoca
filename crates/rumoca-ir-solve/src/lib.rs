//! Solver-facing Solve IR.
//!
//! This crate contains data consumed by simulation backends after DAE-level
//! structural/lowering phases. It must stay free of DAE evaluation and phase
//! logic.

mod layout;
mod linear_op;
pub mod visitor;

use indexmap::IndexMap;
use rumoca_core::{ExternalTableData, Span};
use serde::{Deserialize, Serialize};

pub use layout::{
    IndexedScalarSlot, ScalarSlot, VarLayout, VarLayoutShapeContractError, scalar_slot_p,
    scalar_slot_y,
};
pub use linear_op::{BinaryOp, CompareOp, LinearOp, RandomGenerator, Reg, UnaryOp};
pub use visitor::{
    LinearOpSliceKind, SolveVisitor, VisitScope, walk_compute_block, walk_compute_node,
    walk_scalar_program_block, walk_solve_artifacts, walk_solve_model, walk_solve_problem,
};

pub const SOLVE_SCHEMA_VERSION: u16 = 6;

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
}

impl ScalarProgramBlock {
    pub fn new(programs: Vec<Vec<LinearOp>>) -> Self {
        Self::with_program_spans(programs, Vec::new())
    }

    pub fn with_program_spans(programs: Vec<Vec<LinearOp>>, mut program_spans: Vec<Span>) -> Self {
        program_spans.truncate(programs.len());
        program_spans.resize(programs.len(), Span::DUMMY);
        Self {
            programs,
            program_spans,
        }
    }

    pub fn with_source_span(programs: Vec<Vec<LinearOp>>, span: Span) -> Self {
        let program_spans = vec![span; programs.len()];
        Self::with_program_spans(programs, program_spans)
    }

    pub fn program_span(&self, row: usize) -> Option<Span> {
        self.program_spans
            .get(row)
            .copied()
            .filter(|span| !span.is_dummy())
    }

    pub fn len(&self) -> usize {
        self.programs.len()
    }

    pub fn is_empty(&self) -> bool {
        self.programs.is_empty()
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
                span: self.program_span(0).unwrap_or(Span::DUMMY),
            });
        }
        Ok(())
    }
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
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct ComputeNodeCounts {
    pub scalar_programs: usize,
    pub matmul: usize,
    pub linsolve: usize,
}

impl ComputeNodeCounts {
    pub fn tensor_nodes(self) -> usize {
        self.matmul + self.linsolve
    }

    pub fn add_assign(&mut self, rhs: Self) {
        self.scalar_programs += rhs.scalar_programs;
        self.matmul += rhs.matmul;
        self.linsolve += rhs.linsolve;
    }
}

/// A sequence of compute nodes in a `SolveProblem`.
///
/// Serializes as `{"nodes": [...]}` where each node is a tagged enum variant
/// (`ScalarPrograms`, `MatMul`, `LinSolve`). Tensor structure is preserved through
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

    /// Total scalar row count across all nodes.
    pub fn len(&self) -> usize {
        self.nodes
            .iter()
            .map(|n| match n {
                ComputeNode::ScalarPrograms(b) => b.len(),
                ComputeNode::MatMul { m, n, .. } => m * n,
                ComputeNode::LinSolve { n, .. } => *n,
            })
            .sum()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn compute_node_counts(&self) -> ComputeNodeCounts {
        let mut counts = ComputeNodeCounts::default();
        for node in &self.nodes {
            match node {
                ComputeNode::ScalarPrograms(_) => counts.scalar_programs += 1,
                ComputeNode::MatMul { .. } => counts.matmul += 1,
                ComputeNode::LinSolve { .. } => counts.linsolve += 1,
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
                            span: block.program_span(0).unwrap_or(Span::DUMMY),
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
        }
        Ok(())
    }
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
mod tests {
    use super::*;
    use indexmap::IndexMap;

    const REPRESENTATIVE_SOLVE_PROBLEM_GOLDEN: &str =
        include_str!("../tests/golden/representative_solve_problem.solve.json");

    fn make_layout(y_shapes: &[(&str, Vec<usize>)], p_shapes: &[(&str, Vec<usize>)]) -> VarLayout {
        let mut bindings = IndexMap::new();
        let mut shapes = IndexMap::new();
        let mut y_offset = 0usize;
        let mut p_offset = 0usize;
        for (name, shape) in y_shapes {
            let size: usize = shape.iter().product();
            bindings.insert(name.to_string(), scalar_slot_y(y_offset));
            shapes.insert(name.to_string(), shape.clone());
            y_offset += size;
        }
        for (name, shape) in p_shapes {
            let size: usize = shape.iter().product();
            bindings.insert(name.to_string(), scalar_slot_p(p_offset));
            shapes.insert(name.to_string(), shape.clone());
            p_offset += size;
        }
        VarLayout::from_parts_with_shapes(bindings, shapes, y_offset, p_offset)
    }

    fn representative_solve_problem_fixture() -> SolveProblem {
        SolveProblem {
            schema_version: SOLVE_SCHEMA_VERSION,
            layout: make_layout(
                &[("x", vec![1]), ("y", vec![1]), ("hold.y", vec![1])],
                &[("p", vec![1]), ("__pre__.hold.y", vec![1])],
            ),
            solve_layout: representative_solve_layout(),
            continuous: representative_continuous_system(),
            initialization: representative_initialization_system(),
            discrete: representative_discrete_system(),
            events: representative_event_partition(),
            clocks: representative_clock_partition(),
        }
    }

    fn representative_solver_maps() -> SolverNameIndexMaps {
        let mut name_to_idx = IndexMap::new();
        name_to_idx.insert("x".to_string(), 0);
        name_to_idx.insert("y".to_string(), 1);
        name_to_idx.insert("hold.y".to_string(), 2);

        let mut base_to_indices = IndexMap::new();
        base_to_indices.insert("x".to_string(), vec![0]);
        base_to_indices.insert("y".to_string(), vec![1]);
        base_to_indices.insert("hold.y".to_string(), vec![2]);

        SolverNameIndexMaps {
            names: vec!["x".to_string(), "y".to_string(), "hold.y".to_string()],
            name_to_idx,
            base_to_indices,
        }
    }

    fn representative_solve_layout() -> SolveLayout {
        SolveLayout {
            solver_maps: representative_solver_maps(),
            state_scalar_count: 1,
            algebraic_scalar_count: 1,
            output_scalar_count: 1,
            parameter_count: 1,
            compiled_parameter_len: 2,
            discrete_real_scalar_names: vec!["hold.y".to_string()],
            relation_memory_parameter_indices: vec![1],
            initial_event_parameter_index: Some(1),
            pre_param_bindings: vec![PreParamBinding {
                dest_p_index: 1,
                source: PreParamSource::Y { index: 2 },
            }],
            ..SolveLayout::default()
        }
    }

    fn representative_continuous_system() -> ContinuousSolveSystem {
        ContinuousSolveSystem {
            implicit_rhs: ComputeBlock {
                nodes: vec![ComputeNode::ScalarPrograms(ScalarProgramBlock::new(vec![
                    vec![
                        LinearOp::LoadY { dst: 0, index: 0 },
                        LinearOp::LoadP { dst: 1, index: 0 },
                        LinearOp::Binary {
                            dst: 2,
                            op: BinaryOp::Sub,
                            lhs: 0,
                            rhs: 1,
                        },
                        LinearOp::StoreOutput { src: 2 },
                    ],
                ]))],
            },
            implicit_row_targets: vec![Some(scalar_slot_y(1))],
            algebraic_projection_plan: AlgebraicProjectionPlan {
                blocks: vec![AlgebraicProjectionBlock {
                    rows: vec![1],
                    y_indices: vec![1],
                    causal_steps: Vec::new(),
                }],
            },
            residual: ScalarProgramBlock::new(vec![vec![
                LinearOp::LoadY { dst: 0, index: 1 },
                LinearOp::StoreOutput { src: 0 },
            ]]),
            derivative_rhs: representative_derivative_rhs(),
        }
    }

    fn representative_derivative_rhs() -> ComputeBlock {
        ComputeBlock {
            nodes: vec![ComputeNode::MatMul {
                lhs_ops: vec![LinearOp::LoadP { dst: 0, index: 0 }],
                lhs_start: 0,
                rhs_ops: vec![LinearOp::LoadY { dst: 1, index: 0 }],
                rhs_start: 1,
                m: 1,
                k: 1,
                n: 1,
                lhs_sparsity: SparsityPattern::Diagonal,
                rhs_sparsity: SparsityPattern::Dense,
                metadata: TensorNodeMetadata::default(),
                span: Span::DUMMY,
            }],
        }
    }

    fn representative_initialization_system() -> InitializationSolveSystem {
        InitializationSolveSystem {
            row_targets: vec![Some(scalar_slot_y(1))],
            residual: ScalarProgramBlock::new(vec![vec![
                LinearOp::Const { dst: 0, value: 0.0 },
                LinearOp::StoreOutput { src: 0 },
            ]]),
            projection_plan: AlgebraicProjectionPlan::default(),
            update_rhs: ScalarProgramBlock::default(),
            update_targets: Vec::new(),
        }
    }

    fn representative_discrete_system() -> DiscreteSolveSystem {
        DiscreteSolveSystem {
            runtime_assignment_rhs: ScalarProgramBlock::new(vec![vec![
                LinearOp::LoadY { dst: 0, index: 2 },
                LinearOp::StoreOutput { src: 0 },
            ]]),
            runtime_assignment_targets: vec![scalar_slot_p(1)],
            rhs: ScalarProgramBlock::new(vec![vec![
                LinearOp::LoadY { dst: 0, index: 1 },
                LinearOp::Const { dst: 1, value: 0.0 },
                LinearOp::Compare {
                    dst: 2,
                    op: CompareOp::Gt,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ]]),
            update_targets: vec![scalar_slot_y(2)],
            pre_modes: vec![DiscreteEventPreMode::Fixed],
            observation_refresh: vec![true],
        }
    }

    fn representative_event_partition() -> SolveEventPartition {
        SolveEventPartition {
            root_conditions: ScalarProgramBlock::new(vec![vec![
                LinearOp::LoadTime { dst: 0 },
                LinearOp::LoadP { dst: 1, index: 0 },
                LinearOp::Compare {
                    dst: 2,
                    op: CompareOp::Ge,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ]]),
            scheduled_time_events: vec![0.1],
            ..SolveEventPartition::default()
        }
    }

    fn representative_clock_partition() -> SolveClockPartition {
        SolveClockPartition {
            periodic_event_schedules: vec![PeriodicEventSchedule {
                period_seconds: 0.1,
                phase_seconds: 0.0,
            }],
        }
    }

    fn assert_same_json_shape<T: serde::Serialize>(actual: &T, expected: &T) {
        assert_eq!(
            serde_json::to_value(actual).expect("serialize actual"),
            serde_json::to_value(expected).expect("serialize expected")
        );
    }

    #[test]
    fn y_slice_returns_some_for_y_array_variable() {
        let layout = make_layout(&[("x", vec![3, 3])], &[]);
        let src = layout
            .y_slice("x")
            .expect("3×3 Y-slot variable should yield YSlice");
        assert!(matches!(src, TensorSource::YSlice { start: 0, shape } if shape == [3, 3]));
    }

    #[test]
    fn p_slice_returns_some_for_p_array_variable() {
        let layout = make_layout(&[], &[("A", vec![2, 4])]);
        let src = layout
            .p_slice("A")
            .expect("2×4 P-slot variable should yield PSlice");
        assert!(matches!(src, TensorSource::PSlice { start: 0, shape } if shape == [2, 4]));
    }

    #[test]
    fn indexed_bindings_are_derived_from_shape_metadata() {
        let layout = make_layout(&[("body.frame.R.T", vec![3, 3])], &[]);
        let entries = layout
            .indexed_bindings()
            .get(&rumoca_core::ComponentPath::from_flat_path(
                "body.frame.R.T",
            ))
            .expect("array layout should expose structured scalar slots");

        assert_eq!(entries.len(), 9);
        assert_eq!(entries[0].indices, vec![1, 1]);
        assert!(matches!(entries[0].slot, ScalarSlot::Y { index: 0, .. }));
        assert_eq!(entries[8].indices, vec![3, 3]);
        assert!(matches!(entries[8].slot, ScalarSlot::Y { index: 8, .. }));
    }

    #[test]
    fn y_slice_returns_none_for_p_slot_variable() {
        let layout = make_layout(&[], &[("p", vec![2])]);
        assert!(
            layout.y_slice("p").is_none(),
            "P-slot variable must not yield YSlice"
        );
    }

    #[test]
    fn p_slice_returns_none_for_y_slot_variable() {
        let layout = make_layout(&[("x", vec![2])], &[]);
        assert!(
            layout.p_slice("x").is_none(),
            "Y-slot variable must not yield PSlice"
        );
    }

    #[test]
    fn y_slice_returns_none_for_scalar_variable_without_shape() {
        let mut bindings = IndexMap::new();
        bindings.insert("s".to_string(), scalar_slot_y(0));
        let layout = VarLayout::from_parts_with_shapes(bindings, IndexMap::new(), 1, 0);
        assert!(
            layout.y_slice("s").is_none(),
            "scalar variable with no recorded shape must not yield YSlice"
        );
    }

    #[test]
    fn y_slice_returns_none_for_unknown_variable() {
        let layout = make_layout(&[("x", vec![2])], &[]);
        assert!(layout.y_slice("unknown").is_none());
    }

    #[test]
    fn compute_block_tensor_nodes_survive_serde_roundtrip() {
        let block = ComputeBlock {
            nodes: vec![
                ComputeNode::ScalarPrograms(ScalarProgramBlock::new(vec![vec![
                    LinearOp::Const { dst: 0, value: 1.0 },
                    LinearOp::StoreOutput { src: 0 },
                ]])),
                ComputeNode::MatMul {
                    lhs_ops: vec![
                        LinearOp::Const { dst: 0, value: 2.0 },
                        LinearOp::Move { dst: 1, src: 0 },
                    ],
                    lhs_start: 1,
                    rhs_ops: vec![
                        LinearOp::LoadSeed { dst: 2, index: 0 },
                        LinearOp::Move { dst: 3, src: 2 },
                    ],
                    rhs_start: 3,
                    m: 1,
                    k: 1,
                    n: 1,
                    lhs_sparsity: SparsityPattern::Diagonal,
                    rhs_sparsity: SparsityPattern::Dense,
                    metadata: TensorNodeMetadata::default(),
                    span: Span::DUMMY,
                },
                ComputeNode::LinSolve {
                    setup_ops: vec![
                        LinearOp::LoadP { dst: 0, index: 0 },
                        LinearOp::LoadP { dst: 1, index: 1 },
                        LinearOp::LoadP { dst: 2, index: 2 },
                        LinearOp::LoadY { dst: 3, index: 0 },
                    ],
                    matrix_start: 0,
                    rhs_start: 3,
                    n: 2,
                    next_reg: 4,
                    metadata: TensorNodeMetadata::default(),
                    span: Span::DUMMY,
                },
            ],
        };

        let json = serde_json::to_string(&block).expect("serialize ComputeBlock");
        // Verify the JSON preserves tensor node tags, not a flat rows list
        assert!(
            json.contains("\"MatMul\""),
            "MatMul node must appear in JSON: {json}"
        );
        assert!(
            json.contains("\"LinSolve\""),
            "LinSolve node must appear in JSON: {json}"
        );
        assert!(
            json.contains("\"lhs_sparsity\""),
            "sparsity annotation must survive: {json}"
        );
        assert!(
            json.contains("\"metadata\""),
            "tensor metadata must survive: {json}"
        );

        let back: ComputeBlock = serde_json::from_str(&json).expect("deserialize ComputeBlock");
        assert_eq!(
            back.nodes.len(),
            3,
            "all three tensor nodes must survive round-trip"
        );
        assert!(matches!(&back.nodes[0], ComputeNode::ScalarPrograms(_)));
        assert!(matches!(
            &back.nodes[1],
            ComputeNode::MatMul {
                m: 1,
                k: 1,
                n: 1,
                lhs_sparsity: SparsityPattern::Diagonal,
                metadata: TensorNodeMetadata {
                    element_type: TensorElementType::Real64,
                    layout: TensorLayout::RowMajorDense,
                    scalar_fallback: ScalarFallback::Exact,
                },
                ..
            }
        ));
        assert!(matches!(&back.nodes[2], ComputeNode::LinSolve { n: 2, .. }));
    }

    #[test]
    fn compute_node_counts_cover_blocks_and_problem() {
        let scalar = ComputeNode::ScalarPrograms(ScalarProgramBlock::new(vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ]]));
        let matmul = ComputeNode::MatMul {
            lhs_ops: vec![LinearOp::Const { dst: 0, value: 1.0 }],
            lhs_start: 0,
            rhs_ops: vec![LinearOp::Const { dst: 1, value: 2.0 }],
            rhs_start: 1,
            m: 1,
            k: 1,
            n: 1,
            lhs_sparsity: SparsityPattern::Dense,
            rhs_sparsity: SparsityPattern::Dense,
            metadata: TensorNodeMetadata::default(),
            span: Span::DUMMY,
        };
        let linsolve = ComputeNode::LinSolve {
            setup_ops: vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::Const { dst: 1, value: 2.0 },
            ],
            matrix_start: 0,
            rhs_start: 1,
            n: 1,
            next_reg: 2,
            metadata: TensorNodeMetadata::default(),
            span: Span::DUMMY,
        };
        let block = ComputeBlock {
            nodes: vec![scalar, matmul.clone(), linsolve.clone()],
        };

        let counts = block.compute_node_counts();
        assert_eq!(counts.scalar_programs, 1);
        assert_eq!(counts.matmul, 1);
        assert_eq!(counts.linsolve, 1);
        assert_eq!(block.tensor_node_count(), 2);

        let mut problem = SolveProblem::default();
        problem.continuous.implicit_rhs = block;
        problem.continuous.derivative_rhs = ComputeBlock {
            nodes: vec![matmul, linsolve],
        };
        let problem_counts = problem.compute_node_counts();
        assert_eq!(problem_counts.scalar_programs, 1);
        assert_eq!(problem_counts.matmul, 2);
        assert_eq!(problem_counts.linsolve, 2);
        assert_eq!(problem_counts.tensor_nodes(), 4);
    }

    #[test]
    fn solve_problem_json_has_supported_schema_version() {
        let value = serde_json::to_value(SolveProblem::default()).expect("serialize SolveProblem");
        assert_eq!(
            value
                .get("schema_version")
                .and_then(serde_json::Value::as_u64),
            Some(u64::from(SOLVE_SCHEMA_VERSION))
        );

        let mut missing = value.clone();
        missing
            .as_object_mut()
            .expect("SolveProblem JSON should be object")
            .remove("schema_version");
        assert!(
            serde_json::from_value::<SolveProblem>(missing).is_err(),
            "SolveProblem JSON must carry an explicit schema_version"
        );

        let mut unsupported = value;
        unsupported["schema_version"] = serde_json::json!(SOLVE_SCHEMA_VERSION + 1);
        let err = serde_json::from_value::<SolveProblem>(unsupported)
            .expect_err("unsupported SolveProblem schema version must fail");
        assert!(err.to_string().contains("unsupported Solve schema_version"));
    }

    #[test]
    fn representative_solve_problem_json_roundtrip_preserves_schema_shape() {
        let problem = representative_solve_problem_fixture();
        let json = serde_json::to_string_pretty(&problem).expect("serialize SolveProblem");
        let decoded: SolveProblem = serde_json::from_str(&json).expect("deserialize SolveProblem");
        assert_same_json_shape(&decoded, &problem);
    }

    #[test]
    fn representative_solve_problem_json_matches_committed_golden() {
        let problem = representative_solve_problem_fixture();
        let actual = serde_json::to_value(&problem).expect("serialize representative SolveProblem");
        let expected: serde_json::Value = serde_json::from_str(REPRESENTATIVE_SOLVE_PROBLEM_GOLDEN)
            .expect("valid SolveProblem golden JSON");

        serde_json::from_value::<SolveProblem>(expected.clone())
            .expect("golden uses supported Solve schema");
        assert_eq!(actual, expected);
    }

    #[test]
    fn representative_solve_problem_bincode_roundtrip_preserves_schema_shape() {
        let problem = representative_solve_problem_fixture();
        let bytes = bincode::serialize(&problem).expect("serialize SolveProblem as bincode");
        let decoded: SolveProblem =
            bincode::deserialize(&bytes).expect("deserialize SolveProblem from bincode");
        assert_same_json_shape(&decoded, &problem);
    }

    #[test]
    fn solve_problem_shape_contract_rejects_bad_schema_version() {
        let mut problem = representative_solve_problem_fixture();
        problem.schema_version = SOLVE_SCHEMA_VERSION + 1;

        assert_eq!(
            problem.validate_shape_contract(),
            Err(SolveProblemShapeContractError::SchemaVersion {
                actual: SOLVE_SCHEMA_VERSION + 1,
                expected: SOLVE_SCHEMA_VERSION,
            })
        );
    }

    #[test]
    fn solve_problem_shape_contract_rejects_zero_tensor_dimension() {
        let mut problem = representative_solve_problem_fixture();
        problem.continuous.derivative_rhs = ComputeBlock {
            nodes: vec![ComputeNode::LinSolve {
                setup_ops: Vec::new(),
                matrix_start: 0,
                rhs_start: 0,
                n: 0,
                next_reg: 0,
                metadata: TensorNodeMetadata::default(),
                span: Span::DUMMY,
            }],
        };

        assert_eq!(
            problem.validate_shape_contract(),
            Err(SolveProblemShapeContractError::ZeroTensorDimension {
                context: "continuous.derivative_rhs".to_string(),
                node_index: 0,
                dimension: "LinSolve",
                span: Span::DUMMY,
            })
        );
    }
}

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
            self.continuous.implicit_rhs.len(),
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
            self.initialization.residual.len(),
            self.initialization.row_targets.len(),
        )?;
        validate_count(
            "initialization.update_targets",
            self.initialization.update_rhs.len(),
            self.initialization.update_targets.len(),
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
        span: Span::DUMMY,
    })
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
        span: Span,
    },
    ScalarProgramCountMismatch {
        context: &'static str,
        expected: usize,
        actual: usize,
        span: Span,
    },
    ZeroTensorDimension {
        context: String,
        node_index: usize,
        dimension: &'static str,
        span: Span,
    },
}

impl SolveProblemShapeContractError {
    pub fn span(&self) -> Span {
        match self {
            Self::SchemaVersion { .. } => Span::DUMMY,
            Self::Layout(err) => err.span(),
            Self::ScalarProgramSpanMismatch { span, .. }
            | Self::ScalarProgramCountMismatch { span, .. }
            | Self::ZeroTensorDimension { span, .. } => *span,
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ContinuousSolveSystem {
    pub implicit_rhs: ComputeBlock,
    pub implicit_row_targets: Vec<Option<ScalarSlot>>,
    pub algebraic_projection_plan: AlgebraicProjectionPlan,
    pub residual: ScalarProgramBlock,
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
    /// with `to_scalar_program_block(implicit_rhs)` (and hence with the algebraic
    /// refresh plan's `row_idx`). Used by the state-only path to propagate the
    /// state seed through the algebraic projection (`d(alg)/d(state)`). Distinct
    /// from the tensor `implicit_jacobian_v`, whose scalarization is not
    /// row-aligned when the system has linear (`LinSolve`/`MatMul`) blocks.
    #[serde(default)]
    pub implicit_jacobian_v_scalar: ScalarProgramBlock,
    pub full_jacobian_v: ScalarProgramBlock,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InitializationSolveSystem {
    pub residual: ScalarProgramBlock,
    pub row_targets: Vec<Option<ScalarSlot>>,
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
    pub message: Option<String>,
    pub span: rumoca_core::Span,
    pub origin: String,
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
    Fixed,
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

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
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
