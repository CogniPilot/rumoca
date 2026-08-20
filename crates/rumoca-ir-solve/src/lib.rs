//! Solver-facing Solve IR.
//!
//! This crate contains data consumed by simulation backends after DAE-level
//! structural/lowering phases. It must stay free of DAE evaluation and phase
//! logic.

// SPEC_0021 file-size exception - split plan: extract the Solve program validation and invariant checks into ir-solve/src/program_checks.rs, leaving this file as the module facade and re-exports; tracked as RDD2/GALEC cleanup debt (SPEC_0021 follow-up).

mod certificate;
#[cfg(test)]
mod certificate_tests;
#[cfg(test)]
mod compute_block_tests;
mod feature_query;
pub mod fmi;
mod layout;
mod linear_op;
mod model;
mod refresh;
#[cfg(test)]
mod scalar_program_tests;
mod shape_error;
mod typed_program;
mod variable_bounds;
pub mod visitor;

use indexmap::IndexMap;
use rumoca_core::{
    ExternalTableData, ProvenanceSpan, SourceId, Span, StructuredIndexDomain,
    StructuredIndexDomainError,
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeSet, HashMap};

pub use certificate::{
    derive_root_reachable_runtime_rows, derive_root_relation_refresh_roles,
    derive_runtime_assignment_roles,
};
pub use feature_query::{
    SolveEventClass, solve_event_class, solve_has_clocks, solve_has_events,
    solve_has_initialization, solve_has_runtime_events,
};
pub use layout::{
    ComponentReferenceKey, ComponentReferenceKeyError, ComponentReferenceKeyErrorKind,
    ComponentReferenceKeyPart, ComponentReferenceSubscriptKey, IndexedScalarSlot, ScalarSlot,
    VarLayout, VarLayoutShapeContractError, scalar_slot_p, scalar_slot_y,
};
pub use linear_op::{
    BinaryOp, CompareOp, FoldInitialSource, FoldTensorNode, FoldTensorUpdate,
    FunctionConditionalArmProgram, FunctionConditionalOwnerId, FunctionConditionalProgram,
    FunctionFoldProgram, LinearOp, RandomGenerator, Reg, ScalarProgramRegisterError,
    ScalarProgramRegisterFlow, TargetAssignmentShape, TensorConcatenateSource, TensorIndex,
    TensorInputKind, TensorSubscript, TensorUpdateSubscript, UnaryOp, resolve_indexed_slot,
};
pub use model::*;
pub use refresh::*;
pub use shape_error::{AffineTensorNodeKind, SolveProblemShapeContractError};
pub use typed_program::*;
pub use visitor::{
    LinearOpSliceKind, SolveVisitor, VisitScope, walk_compute_block, walk_compute_node,
    walk_scalar_program_block, walk_solve_artifacts, walk_solve_model, walk_solve_problem,
};

pub const SOLVE_SCHEMA_VERSION: u16 = 61;

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

/// A checked block of scalar programs with exact row provenance and output identity.
///
/// Invariant-bearing columns cannot be mutated after construction:
///
/// ```compile_fail
/// use rumoca_ir_solve::{LinearOp, ScalarProgramBlock};
///
/// let mut block = ScalarProgramBlock::default();
/// block.programs.push(vec![LinearOp::StoreOutput { src: 0 }]);
/// ```
#[derive(Clone, Debug, Default)]
pub struct ScalarProgramBlock {
    programs: Vec<Vec<LinearOp>>,
    program_spans: Vec<Span>,
    output_indices: Vec<usize>,
    /// Construction-owned execution capacity for each stored program.
    ///
    /// This proof is rebuilt on wire replay and deliberately is not
    /// serialized as a second source of truth.
    program_register_counts: Box<[usize]>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ScalarProgramBlockWire {
    programs: Vec<Vec<LinearOp>>,
    program_spans: Vec<Span>,
    output_indices: Vec<usize>,
}

#[derive(Serialize)]
struct ScalarProgramBlockWireRef<'a> {
    programs: &'a [Vec<LinearOp>],
    program_spans: &'a [Span],
    output_indices: &'a [usize],
}

impl Serialize for ScalarProgramBlock {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        ScalarProgramBlockWireRef {
            programs: &self.programs,
            program_spans: &self.program_spans,
            output_indices: &self.output_indices,
        }
        .serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for ScalarProgramBlock {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let wire = ScalarProgramBlockWire::deserialize(deserializer)?;
        Self::with_output_indices(wire.programs, wire.program_spans, wire.output_indices)
            .map_err(serde::de::Error::custom)
    }
}

impl ScalarProgramBlock {
    /// Constructs programs whose stored outputs use dense local indices.
    ///
    /// Provenance is mandatory at the API boundary:
    ///
    /// ```compile_fail
    /// use rumoca_ir_solve::{LinearOp, ScalarProgramBlock};
    ///
    /// let _ = ScalarProgramBlock::with_program_spans(vec![vec![
    ///     LinearOp::Const { dst: 0, value: 1.0 },
    ///     LinearOp::StoreOutput { src: 0 },
    /// ]]);
    /// ```
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
        validate_scalar_program_provenance("ScalarProgramBlock", 0, &program_spans)?;
        validate_scalar_program_outputs("ScalarProgramBlock", 0, &programs, &program_spans)?;
        validate_function_conditional_owners("ScalarProgramBlock", 0, &programs, &program_spans)?;
        let program_register_counts = derive_scalar_program_register_counts(
            "ScalarProgramBlock",
            0,
            &programs,
            &program_spans,
        )?;
        Ok(Self::from_valid_parts(
            programs,
            program_spans,
            output_indices,
            program_register_counts,
        ))
    }

    fn from_valid_parts(
        programs: Vec<Vec<LinearOp>>,
        program_spans: Vec<Span>,
        output_indices: Vec<usize>,
        program_register_counts: Box<[usize]>,
    ) -> Self {
        Self {
            programs,
            program_spans,
            output_indices,
            program_register_counts,
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

    /// Constructs dense-output programs owned by one exact source occurrence.
    ///
    /// A raw or dummy [`Span`] cannot cross this boundary:
    ///
    /// ```compile_fail
    /// use rumoca_core::{SourceId, Span};
    /// use rumoca_ir_solve::{LinearOp, ScalarProgramBlock};
    ///
    /// let raw_span = Span::from_offsets(SourceId::from_source_name("fixture.mo"), 0, 1);
    /// let _ = ScalarProgramBlock::with_source_span(
    ///     vec![vec![
    ///         LinearOp::Const { dst: 0, value: 1.0 },
    ///         LinearOp::StoreOutput { src: 0 },
    ///     ]],
    ///     raw_span,
    /// );
    /// ```
    pub fn with_source_span(
        programs: Vec<Vec<LinearOp>>,
        provenance: ProvenanceSpan,
    ) -> Result<Self, SolveProblemShapeContractError> {
        let span = provenance.span();
        let program_spans = vec![span; programs.len()];
        let output_indices = (0..stored_output_count(&programs)).collect();
        Self::with_output_indices(programs, program_spans, output_indices)
    }

    pub fn program_span(&self, row: usize) -> Option<Span> {
        self.program_spans.get(row).copied()
    }

    pub fn programs(&self) -> &[Vec<LinearOp>] {
        &self.programs
    }

    pub fn program(&self, index: usize) -> Option<&[LinearOp]> {
        self.programs.get(index).map(Vec::as_slice)
    }

    pub fn program_spans(&self) -> &[Span] {
        &self.program_spans
    }

    /// Exact register capacity proved when this program entered the block.
    pub fn program_register_count(&self, index: usize) -> Option<usize> {
        self.program_register_counts.get(index).copied()
    }

    pub fn output_indices(&self) -> &[usize] {
        &self.output_indices
    }

    pub fn first_source_span(&self) -> Option<Span> {
        self.program_spans.first().copied()
    }

    /// Number of `StoreOutput` ops in a single program.
    ///
    /// A program may emit more than one output: matmul/linsolve nodes lower to
    /// one self-contained program that computes its operands once and stores
    /// every result via consecutive `StoreOutput` ops.
    pub fn program_output_count(program: &[LinearOp]) -> usize {
        program
            .iter()
            .map(|op| match op {
                LinearOp::StoreOutput { .. } => 1,
                LinearOp::StoreOutputRange { count, .. } => *count,
                _ => 0,
            })
            .sum()
    }

    /// Total number of `StoreOutput` ops produced by this block.
    pub fn stored_output_count(&self) -> usize {
        self.programs
            .iter()
            .map(|program| Self::program_output_count(program))
            .sum()
    }

    pub fn uses_linear_solve_component(&self) -> bool {
        self.programs
            .iter()
            .any(|program| linear_ops_use_linear_solve_component(program))
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
}

fn first_span(spans: &[Span]) -> Option<Span> {
    spans.first().copied()
}

fn stored_output_count(programs: &[Vec<LinearOp>]) -> usize {
    programs
        .iter()
        .map(|program| ScalarProgramBlock::program_output_count(program))
        .sum()
}

fn validate_scalar_program_provenance(
    context: &str,
    node_index: usize,
    program_spans: &[Span],
) -> Result<(), SolveProblemShapeContractError> {
    let Some(program_index) = program_spans.iter().position(Span::is_dummy) else {
        return Ok(());
    };
    Err(
        SolveProblemShapeContractError::ScalarProgramMissingProvenance {
            context: context.to_string(),
            node_index,
            program_index,
        },
    )
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

fn validate_scalar_program_outputs(
    context: &str,
    node_index: usize,
    programs: &[Vec<LinearOp>],
    program_spans: &[Span],
) -> Result<(), SolveProblemShapeContractError> {
    let Some(program_index) = programs
        .iter()
        .position(|program| ScalarProgramBlock::program_output_count(program) == 0)
    else {
        return Ok(());
    };
    let span = program_spans.get(program_index).copied();
    Err(SolveProblemShapeContractError::ScalarProgramMissingOutput {
        context: context.to_string(),
        node_index,
        program_index,
        span,
    })
}

pub(crate) fn derive_scalar_program_register_counts(
    context: &str,
    node_index: usize,
    programs: &[Vec<LinearOp>],
    program_spans: &[Span],
) -> Result<Box<[usize]>, SolveProblemShapeContractError> {
    // The enclosing constructor has already proved that each issued owner id
    // denotes one exact body, so replay that body once even when wire decoding
    // reconstructed several equal `Arc` allocations for its references.
    let mut validation = linear_op::ScalarProgramValidationCache::for_checked_owner_table();
    let mut register_counts = Vec::with_capacity(programs.len());
    for (program_index, program) in programs.iter().enumerate() {
        let flow = match ScalarProgramRegisterFlow::derive_with_cache(program, &mut validation) {
            Ok(flow) => flow,
            Err(error) => {
                let span = program_spans.get(program_index).copied();
                return Err(SolveProblemShapeContractError::ScalarProgramRegisterFlow {
                    context: context.to_string(),
                    node_index,
                    program_index,
                    error,
                    span,
                });
            }
        };
        register_counts.push(flow.register_count());
    }
    Ok(register_counts.into_boxed_slice())
}

fn validate_function_conditional_owners(
    context: &str,
    node_index: usize,
    programs: &[Vec<LinearOp>],
    program_spans: &[Span],
) -> Result<(), SolveProblemShapeContractError> {
    fn visit<'a>(
        operations: &'a [LinearOp],
        owners: &mut HashMap<FunctionConditionalOwnerId, &'a FunctionConditionalProgram>,
    ) -> Option<u64> {
        for operation in operations {
            let mismatched = match operation {
                LinearOp::FunctionConditional { program, .. } => visit_conditional(program, owners),
                LinearOp::FunctionFold { program, .. }
                | LinearOp::GuardedFunctionFold { program, .. }
                | LinearOp::StoreOutputFunctionFold { program, .. } => {
                    visit(&program.update, owners)
                }
                _ => None,
            };
            if mismatched.is_some() {
                return mismatched;
            }
        }
        None
    }

    /// Claims the conditional body for its owner id, then walks its arms and
    /// fallback in construction order.
    fn visit_conditional<'a>(
        program: &'a FunctionConditionalProgram,
        owners: &mut HashMap<FunctionConditionalOwnerId, &'a FunctionConditionalProgram>,
    ) -> Option<u64> {
        if let Some(owner) = claim_conditional_owner(program, owners) {
            return Some(owner);
        }
        for arm in &program.arms {
            if let Some(owner) =
                visit(&arm.condition, owners).or_else(|| visit(&arm.result, owners))
            {
                return Some(owner);
            }
        }
        visit(&program.fallback, owners)
    }

    /// Records the first checked body seen for an owner id, reporting the owner
    /// when a later body claims the same id with different contents.
    fn claim_conditional_owner<'a>(
        program: &'a FunctionConditionalProgram,
        owners: &mut HashMap<FunctionConditionalOwnerId, &'a FunctionConditionalProgram>,
    ) -> Option<u64> {
        let owner = program.owner?;
        let Some(previous) = owners.get(&owner) else {
            owners.insert(owner, program);
            return None;
        };
        if !std::ptr::eq(*previous, program) && *previous != program {
            return Some(owner.get());
        }
        None
    }

    let mut owners = HashMap::new();
    for (program_index, program) in programs.iter().enumerate() {
        if let Some(owner) = visit(program, &mut owners) {
            return Err(
                SolveProblemShapeContractError::FunctionConditionalOwnerMismatch {
                    context: context.to_string(),
                    node_index,
                    owner,
                    program_index,
                    span: program_spans.get(program_index).copied(),
                },
            );
        }
    }
    Ok(())
}

mod structural_pattern;
pub use structural_pattern::{
    ColumnColoring, PatternDerivation, PatternProvenance, StructuralPattern,
    StructuralPatternError, StructuralPatternView,
};

#[cfg(test)]
pub(crate) fn fixture_pattern(rows: usize, columns: usize, diagonal: bool) -> StructuralPattern {
    let dependencies = (0..rows)
        .map(|row| {
            if diagonal {
                (row < columns).then_some(row).into_iter().collect()
            } else {
                (0..columns).collect()
            }
        })
        .collect::<Vec<_>>();
    let provenance = PatternProvenance::derived(
        PatternDerivation::TensorOperand,
        Span::from_offsets(
            SourceId::from_source_name("solve_ir_pattern_fixture.mo"),
            0,
            1,
        ),
    )
    .expect("fixture provenance");
    StructuralPattern::from_row_dependencies(rows, columns, &dependencies, provenance)
        .expect("fixture pattern")
}

mod tensor;
use tensor::output_index_overflow;
pub use tensor::{
    AffineStencilConstStride, AffineStencilConstStrideTerm, AffineStencilIndexStrideTerm,
    AffineStencilLoadStride, ComputeBlock, ComputeNode, ComputeNodeCounts, ScalarFallback,
    TensorElementType, TensorLayout, TensorNodeMetadata, TensorOutputMap, TensorOutputMapError,
    TensorSource,
};

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

        let mut problem = Self {
            schema_version: wire.schema_version,
            layout: wire.layout,
            solve_layout: wire.solve_layout,
            continuous: wire.continuous,
            initialization: wire.initialization,
            discrete: wire.discrete,
            events: wire.events,
            clocks: wire.clocks,
        };
        problem
            .continuous
            .refresh_owners
            .rebuild_exact_assignment_programs(&problem.continuous.implicit_rhs)
            .map_err(serde::de::Error::custom)?;
        problem.validate().map_err(serde::de::Error::custom)?;
        Ok(problem)
    }
}

impl SolveProblem {
    /// Build a continuous-only problem from one checked derivative program and
    /// the variable layout that program addresses.
    ///
    /// The layout is a required input rather than a default: the derivative
    /// seed space is `y_scalars + p_scalars` wide and parameter seeds start at
    /// `y_scalars`, so a layout that does not own the program's own `Y`/`P`
    /// loads silently aliases derivative columns. The state extent is taken
    /// from the program's checked output count, and the finished problem is
    /// validated before it is returned.
    pub fn with_derivative_rhs(
        derivative_rhs: ComputeBlock,
        layout: VarLayout,
    ) -> Result<Self, SolveProblemShapeContractError> {
        let state_scalar_count = derivative_rhs.output_count("continuous.derivative_rhs")?;
        let problem = Self {
            layout,
            solve_layout: SolveLayout {
                state_scalar_count,
                ..SolveLayout::default()
            },
            continuous: ContinuousSolveSystem {
                derivative_rhs,
                ..ContinuousSolveSystem::default()
            },
            ..Self::default()
        };
        problem.validate()?;
        Ok(problem)
    }

    pub fn compute_node_counts(&self) -> ComputeNodeCounts {
        let mut counts = self.continuous.implicit_rhs.compute_node_counts();
        counts.add_assign(self.continuous.residual.compute_node_counts());
        counts.add_assign(self.continuous.manifold_residual.compute_node_counts());
        counts.add_assign(self.continuous.derivative_rhs.compute_node_counts());
        counts
    }

    pub fn uses_linear_solve_component(&self) -> bool {
        self.continuous.implicit_rhs.uses_linear_solve_component()
            || self.continuous.residual.uses_linear_solve_component()
            || self
                .continuous
                .manifold_residual
                .uses_linear_solve_component()
            || self.continuous.derivative_rhs.uses_linear_solve_component()
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
        validate_variable_storage_runs(self)?;
        validate_event_iteration_plan(self)?;
        validate_continuous_system_shape(self)?;
        validate_initialization_system_shape(self)?;
        validate_discrete_system_shape(self)?;
        validate_event_partition_shape(self)?;
        Ok(())
    }

    /// Validate the complete finalized Solve-IR stage contract.
    pub fn validate(&self) -> Result<(), SolveProblemShapeContractError> {
        self.validate_shape_contract()
    }
}

impl SolveModel {
    /// Validate the finalized model, including every issued pure-call
    /// reference against this model's sole checked owner table.
    pub fn validate(&self) -> Result<(), SolveProblemShapeContractError> {
        self.problem.validate()?;
        let mut validator = ModelPureCallSiteValidator {
            table: &self.pure_calls,
        };
        validator.visit_solve_model(self)
    }
}

/// Prove that every pure-call reference already present in a finalized Solve
/// problem belongs to, and exactly matches, its sole model-level owner table.
pub fn validate_problem_pure_call_sites(
    problem: &SolveProblem,
    table: &SolvePureCallTable,
) -> Result<(), SolveProblemShapeContractError> {
    problem.validate()?;
    let mut validator = ModelPureCallSiteValidator { table };
    validator.visit_solve_problem(problem)
}

struct ModelPureCallSiteValidator<'model> {
    table: &'model SolvePureCallTable,
}

impl SolveVisitor for ModelPureCallSiteValidator<'_> {
    type Error = SolveProblemShapeContractError;

    fn visit_event_transaction_program(
        &mut self,
        _index: usize,
        program: &EventTransactionProgram,
    ) -> Result<(), Self::Error> {
        self.require_site(
            "discrete.event_transactions",
            program.site(),
            Some(program.span()),
        )
    }

    fn visit_linear_op(
        &mut self,
        kind: LinearOpSliceKind,
        _op_index: usize,
        op: &LinearOp,
    ) -> Result<(), Self::Error> {
        if let LinearOp::PureCall { site, .. } = op {
            self.require_site(
                "SolveModel scalar program",
                site,
                linear_op_slice_span(kind),
            )?;
        }
        Ok(())
    }
}

impl ModelPureCallSiteValidator<'_> {
    fn require_site(
        &self,
        context: &'static str,
        site: &SolvePureCallSite,
        span: Option<Span>,
    ) -> Result<(), SolveProblemShapeContractError> {
        if self.table.matches_site(site) {
            return Ok(());
        }
        Err(SolveProblemShapeContractError::PureCallSiteMismatch {
            context,
            owner: site.owner().index(),
            span,
        })
    }
}

const fn linear_op_slice_span(kind: LinearOpSliceKind) -> Option<Span> {
    match kind {
        LinearOpSliceKind::ScalarProgram { span, .. } => span,
        LinearOpSliceKind::GuardedAssignmentProgram { span, .. }
        | LinearOpSliceKind::MatMulLhs { span, .. }
        | LinearOpSliceKind::MatMulRhs { span, .. }
        | LinearOpSliceKind::LinSolveSetup { span, .. }
        | LinearOpSliceKind::MapBase { span, .. }
        | LinearOpSliceKind::AffineStencilBase { span, .. } => Some(span),
    }
}

fn validate_variable_storage_runs(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    if problem.solve_layout.variable_storage_runs.len()
        != problem.solve_layout.variable_declarations.len()
    {
        return Err(variable_storage_contract(
            0,
            "storage and declaration catalogs have different lengths",
        ));
    }
    for (variable, storage) in problem
        .solve_layout
        .variable_storage_runs
        .iter()
        .copied()
        .enumerate()
    {
        let declaration = problem.solve_layout.variable_declarations[variable];
        if storage.role != declaration.role() || storage.value_kind != declaration.value_kind() {
            return Err(variable_storage_contract(
                variable,
                "storage role or kind disagrees with its immutable declaration",
            ));
        }
        let role_uses_p = matches!(
            storage.role,
            SolveVariableStorageRole::Parameter
                | SolveVariableStorageRole::Constant
                | SolveVariableStorageRole::ExternalInput
                | SolveVariableStorageRole::DiscreteReal
                | SolveVariableStorageRole::DiscreteValue
        );
        let (base, extent, base_uses_p) = match storage.base {
            ScalarSlot::P { index, .. } => (index, problem.layout.p_scalars(), true),
            ScalarSlot::Y { index, .. } => (index, problem.layout.y_scalars(), false),
            ScalarSlot::Time | ScalarSlot::Constant(_) => {
                return Err(variable_storage_contract(
                    variable,
                    "storage base is not a mutable Y/P coordinate",
                ));
            }
        };
        if role_uses_p != base_uses_p {
            return Err(variable_storage_contract(
                variable,
                "storage column disagrees with its typed variable role",
            ));
        }
        let kind_matches_role = match storage.role {
            SolveVariableStorageRole::State
            | SolveVariableStorageRole::Algebraic
            | SolveVariableStorageRole::Output
            | SolveVariableStorageRole::DiscreteReal => {
                storage.value_kind == SolveVariableValueKind::Real
            }
            SolveVariableStorageRole::DiscreteValue => matches!(
                storage.value_kind,
                SolveVariableValueKind::Integer
                    | SolveVariableValueKind::Boolean
                    | SolveVariableValueKind::Enumeration
            ),
            SolveVariableStorageRole::Parameter
            | SolveVariableStorageRole::Constant
            | SolveVariableStorageRole::ExternalInput => true,
        };
        if !kind_matches_role {
            return Err(variable_storage_contract(
                variable,
                "value kind disagrees with its typed variable role",
            ));
        }
        if !variable_time_domain_matches_role(declaration, storage.role) {
            return Err(variable_storage_contract(
                variable,
                "effective time domain disagrees with its typed variable role",
            ));
        }
        let end = base.checked_add(storage.scalar_count).ok_or_else(|| {
            variable_storage_contract(variable, "storage scalar range overflowed")
        })?;
        if end > extent {
            return Err(variable_storage_contract(
                variable,
                "storage scalar range exceeds its Y/P column",
            ));
        }
    }
    Ok(())
}

fn variable_time_domain_matches_role(
    declaration: SolveVariableDeclaration,
    role: SolveVariableStorageRole,
) -> bool {
    match role {
        SolveVariableStorageRole::Parameter | SolveVariableStorageRole::Constant => {
            declaration.time_domain() == SolveVariableTimeDomain::Static
        }
        SolveVariableStorageRole::DiscreteReal | SolveVariableStorageRole::DiscreteValue => {
            declaration.time_domain() == SolveVariableTimeDomain::EventDiscrete
        }
        SolveVariableStorageRole::Algebraic | SolveVariableStorageRole::Output => matches!(
            declaration.time_domain(),
            SolveVariableTimeDomain::ContinuousTime | SolveVariableTimeDomain::EventDiscontinuous
        ),
        SolveVariableStorageRole::ExternalInput | SolveVariableStorageRole::State => {
            declaration.time_domain() == SolveVariableTimeDomain::ContinuousTime
        }
    }
}

fn variable_storage_contract(
    variable: usize,
    detail: &'static str,
) -> SolveProblemShapeContractError {
    SolveProblemShapeContractError::DiscreteCertificate {
        context: "solve_layout.variable_storage_runs",
        row: variable,
        detail,
        span: None,
    }
}

#[derive(Default)]
struct EventIterationClaims {
    covered_variables: BTreeSet<usize>,
    covered_bindings: BTreeSet<usize>,
    current_indices: BTreeSet<usize>,
    pre_indices: BTreeSet<usize>,
    scalar_rows: BTreeSet<usize>,
    structured_updates: BTreeSet<usize>,
    guarded_ranges: BTreeSet<(usize, usize)>,
    transaction_targets: BTreeSet<(usize, usize)>,
}

fn validate_event_iteration_plan(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    let layout = &problem.solve_layout;
    let expected_variables = layout
        .variable_storage_runs
        .iter()
        .enumerate()
        .filter_map(|(variable, storage)| {
            (storage.scalar_count != 0 && storage.event_iteration_kind().is_some())
                .then_some(variable)
        })
        .collect::<BTreeSet<_>>();
    let mut claims = EventIterationClaims::default();
    validate_event_transaction_producer_coverage(problem, &mut claims)?;
    for (row, run) in problem
        .discrete
        .event_iteration_plan
        .runs
        .iter()
        .enumerate()
    {
        validate_event_iteration_run(problem, row, run, &mut claims)?;
    }
    if claims.covered_variables != expected_variables {
        return Err(event_iteration_contract(
            0,
            "plan is not a reverse bijection over typed discrete variable owners",
        ));
    }
    validate_scalar_event_producers(problem, &claims.scalar_rows)?;
    validate_structured_event_producers(problem, &claims.structured_updates)?;
    validate_guarded_event_producers(problem, &claims.guarded_ranges)?;
    validate_event_transaction_target_claims(problem, &claims.transaction_targets)
}

fn validate_event_iteration_run(
    problem: &SolveProblem,
    row: usize,
    run: &EventIterationRun,
    claims: &mut EventIterationClaims,
) -> Result<(), SolveProblemShapeContractError> {
    let layout = &problem.solve_layout;
    let storage = layout
        .variable_storage_runs
        .get(run.variable)
        .ok_or_else(|| event_iteration_contract(row, "variable owner is out of bounds"))?;
    if storage.event_iteration_kind().is_none() {
        return Err(event_iteration_contract(
            row,
            "variable owner is not a typed discrete coordinate",
        ));
    }
    if !claims.covered_variables.insert(run.variable) {
        return Err(event_iteration_contract(
            row,
            "variable owner is duplicated",
        ));
    }
    if storage.scalar_count == 0 {
        return Err(event_iteration_contract(row, "run is empty"));
    }
    let current_base = validate_event_iteration_bindings(problem, row, run, *storage, claims)?;
    if claims
        .current_indices
        .iter()
        .any(|index| claims.pre_indices.contains(index))
    {
        return Err(event_iteration_contract(
            row,
            "current and pre lanes overlap",
        ));
    }
    validate_event_iteration_owner(problem, row, run, *storage, current_base, claims)
}

fn validate_event_iteration_bindings(
    problem: &SolveProblem,
    row: usize,
    run: &EventIterationRun,
    storage: SolveVariableStorageRun,
    claims: &mut EventIterationClaims,
) -> Result<usize, SolveProblemShapeContractError> {
    let layout = &problem.solve_layout;
    let end = run
        .pre_binding_start
        .checked_add(storage.scalar_count)
        .ok_or_else(|| event_iteration_contract(row, "binding range overflowed"))?;
    let bindings = layout
        .pre_param_bindings
        .get(run.pre_binding_start..end)
        .ok_or_else(|| event_iteration_contract(row, "binding range is out of bounds"))?;
    let ScalarSlot::P {
        index: current_base,
        ..
    } = storage.base
    else {
        return Err(event_iteration_contract(
            row,
            "typed discrete coordinate is not P-backed",
        ));
    };
    let mut pre_base = None;
    for (offset, binding) in bindings.iter().enumerate() {
        let binding_index = run.pre_binding_start + offset;
        if !claims.covered_bindings.insert(binding_index) {
            return Err(event_iteration_contract(row, "binding ranges overlap"));
        }
        if binding.clock_schedule.is_some() {
            return Err(event_iteration_contract(
                row,
                "run contains a clocked previous binding",
            ));
        }
        let PreParamSource::P { index: current } = binding.source else {
            return Err(event_iteration_contract(
                row,
                "run source is not a discrete P slot",
            ));
        };
        let expected_pre = pre_base
            .get_or_insert(binding.dest_p_index)
            .checked_add(offset);
        if current_base.checked_add(offset) != Some(current)
            || expected_pre != Some(binding.dest_p_index)
        {
            return Err(event_iteration_contract(
                row,
                "run bindings are not contiguous",
            ));
        }
        validate_indices(
            "discrete.event_iteration_plan.current",
            &[current],
            problem.layout.p_scalars(),
        )?;
        validate_indices(
            "discrete.event_iteration_plan.pre",
            &[binding.dest_p_index],
            problem.layout.p_scalars(),
        )?;
        if !claims.current_indices.insert(current)
            || !claims.pre_indices.insert(binding.dest_p_index)
        {
            return Err(event_iteration_contract(
                row,
                "current or pre lanes are duplicated",
            ));
        }
    }
    Ok(current_base)
}

fn validate_event_iteration_owner(
    problem: &SolveProblem,
    row: usize,
    run: &EventIterationRun,
    storage: SolveVariableStorageRun,
    current_base: usize,
    claims: &mut EventIterationClaims,
) -> Result<(), SolveProblemShapeContractError> {
    match run.owner {
        EventIterationOwner::Hold => {
            if storage.role != SolveVariableStorageRole::DiscreteReal {
                return Err(event_iteration_contract(
                    row,
                    "only ordinary discrete Real runs may hold",
                ));
            }
        }
        EventIterationOwner::ScalarRows { start_row } => {
            validate_scalar_event_owner(
                problem,
                row,
                start_row,
                storage.scalar_count,
                current_base,
                &mut claims.scalar_rows,
            )?;
        }
        EventIterationOwner::StructuredUpdate { update_index } => {
            validate_structured_event_owner(
                problem,
                row,
                update_index,
                storage,
                &mut claims.structured_updates,
            )?;
        }
        EventIterationOwner::GuardedAssignment {
            program_index,
            target_range_index,
        } => {
            validate_guarded_event_owner(
                problem,
                row,
                program_index,
                target_range_index,
                storage,
                &mut claims.guarded_ranges,
            )?;
        }
        EventIterationOwner::EventTransaction {
            program_index,
            target_index,
        } => validate_event_transaction_owner(
            problem,
            row,
            program_index,
            target_index,
            storage,
            &mut claims.transaction_targets,
        )?,
    }
    Ok(())
}

fn validate_event_transaction_owner(
    problem: &SolveProblem,
    row: usize,
    program_index: usize,
    target_index: usize,
    storage: SolveVariableStorageRun,
    claimed_targets: &mut BTreeSet<(usize, usize)>,
) -> Result<(), SolveProblemShapeContractError> {
    let transaction = problem
        .discrete
        .event_transactions
        .get(program_index)
        .ok_or_else(|| event_iteration_contract(row, "transaction owner is out of bounds"))?;
    let target = transaction
        .targets()
        .get(target_index)
        .ok_or_else(|| event_iteration_contract(row, "transaction target is out of bounds"))?;
    if target.base() != storage.base
        || target.value_type().scalar_count() as usize != storage.scalar_count
    {
        return Err(event_iteration_contract(
            row,
            "transaction target does not define the run",
        ));
    }
    if !claimed_targets.insert((program_index, target_index)) {
        return Err(event_iteration_contract(
            row,
            "transaction target is claimed more than once",
        ));
    }
    Ok(())
}

fn validate_event_transaction_target_claims(
    problem: &SolveProblem,
    claimed_targets: &BTreeSet<(usize, usize)>,
) -> Result<(), SolveProblemShapeContractError> {
    let expected = problem
        .discrete
        .event_transactions
        .iter()
        .enumerate()
        .flat_map(|(program_index, program)| {
            (0..program.targets().len()).map(move |target_index| (program_index, target_index))
        })
        .collect::<BTreeSet<_>>();
    if &expected != claimed_targets {
        return Err(event_iteration_contract(
            0,
            "event-plan runs do not exactly cover transaction targets",
        ));
    }
    Ok(())
}

fn validate_event_transaction_producer_coverage(
    problem: &SolveProblem,
    claims: &mut EventIterationClaims,
) -> Result<(), SolveProblemShapeContractError> {
    let mut scalar_transaction = HashMap::new();
    let mut guarded_transaction = HashMap::new();
    let mut producer_claims = EventTransactionProducerClaims {
        event_iteration: claims,
        scalar_transaction: &mut scalar_transaction,
        guarded_transaction: &mut guarded_transaction,
    };
    for (program_index, transaction) in problem.discrete.event_transactions.iter().enumerate() {
        for (target, owner) in transaction
            .targets()
            .iter()
            .zip(transaction.producer_owners())
        {
            producer_claims.claim(problem, program_index, transaction, target, *owner)?;
        }
    }
    validate_complete_scalar_program_coverage(problem, &scalar_transaction)?;
    validate_complete_guarded_program_coverage(problem, &guarded_transaction)
}

struct EventTransactionProducerClaims<'a> {
    event_iteration: &'a mut EventIterationClaims,
    scalar_transaction: &'a mut HashMap<usize, usize>,
    guarded_transaction: &'a mut HashMap<(usize, usize), usize>,
}

impl EventTransactionProducerClaims<'_> {
    fn claim(
        &mut self,
        problem: &SolveProblem,
        transaction_index: usize,
        transaction: &EventTransactionProgram,
        target: &EventTransactionTarget,
        owner: EventTransactionProducerOwner,
    ) -> Result<(), SolveProblemShapeContractError> {
        let storage = exact_transaction_target_storage(problem, target).ok_or_else(|| {
            event_transaction_contract(
                transaction_index,
                "transaction target is not one complete canonical variable-storage run",
                transaction.span(),
            )
        })?;
        match owner {
            EventTransactionProducerOwner::ScalarRows { start_row } => {
                self.claim_scalar(problem, transaction_index, transaction, target, start_row)
            }
            EventTransactionProducerOwner::StructuredUpdate { update_index } => self
                .claim_structured(
                    problem,
                    transaction_index,
                    transaction,
                    storage,
                    target.clock_owner(),
                    update_index,
                ),
            EventTransactionProducerOwner::GuardedAssignment {
                program_index,
                target_range_index,
            } => self.claim_guarded(
                problem,
                transaction_index,
                transaction,
                storage,
                target.clock_owner(),
                (program_index, target_range_index),
            ),
        }
    }

    fn claim_scalar(
        &mut self,
        problem: &SolveProblem,
        transaction_index: usize,
        transaction: &EventTransactionProgram,
        target: &EventTransactionTarget,
        start_row: usize,
    ) -> Result<(), SolveProblemShapeContractError> {
        let width = target.value_type().scalar_count() as usize;
        let current_base = scalar_slot_index(target.base()).ok_or_else(|| {
            event_transaction_contract(
                transaction_index,
                "transaction scalar target is not P-backed",
                transaction.span(),
            )
        })?;
        validate_scalar_event_owner(
            problem,
            transaction_index,
            start_row,
            width,
            current_base,
            &mut self.event_iteration.scalar_rows,
        )?;
        for row in start_row..start_row + width {
            require_unique_transaction_scalar_row(
                self.scalar_transaction,
                row,
                transaction_index,
                transaction.span(),
            )?;
        }
        require_transaction_clock(
            problem.discrete.clock_owners[start_row],
            target.clock_owner(),
            transaction_index,
            transaction.span(),
        )
    }

    fn claim_structured(
        &mut self,
        problem: &SolveProblem,
        transaction_index: usize,
        transaction: &EventTransactionProgram,
        storage: SolveVariableStorageRun,
        target_clock: Option<PeriodicClockId>,
        update_index: usize,
    ) -> Result<(), SolveProblemShapeContractError> {
        validate_structured_event_owner(
            problem,
            transaction_index,
            update_index,
            storage,
            &mut self.event_iteration.structured_updates,
        )?;
        require_transaction_clock(
            problem.discrete.structured_updates[update_index].clock_owner,
            target_clock,
            transaction_index,
            transaction.span(),
        )
    }

    fn claim_guarded(
        &mut self,
        problem: &SolveProblem,
        transaction_index: usize,
        transaction: &EventTransactionProgram,
        storage: SolveVariableStorageRun,
        target_clock: Option<PeriodicClockId>,
        guarded: (usize, usize),
    ) -> Result<(), SolveProblemShapeContractError> {
        let (program_index, target_range_index) = guarded;
        validate_guarded_event_owner(
            problem,
            transaction_index,
            program_index,
            target_range_index,
            storage,
            &mut self.event_iteration.guarded_ranges,
        )?;
        if self
            .guarded_transaction
            .insert(guarded, transaction_index)
            .is_some()
        {
            return Err(event_transaction_contract(
                transaction_index,
                "guarded producer target is covered more than once",
                transaction.span(),
            ));
        }
        require_transaction_clock(
            problem.discrete.guarded_assignments[program_index].clock_owner(),
            target_clock,
            transaction_index,
            transaction.span(),
        )
    }
}

fn require_unique_transaction_scalar_row(
    owners: &mut HashMap<usize, usize>,
    row: usize,
    transaction_index: usize,
    span: Span,
) -> Result<(), SolveProblemShapeContractError> {
    if owners.insert(row, transaction_index).is_none() {
        return Ok(());
    }
    Err(event_transaction_contract(
        transaction_index,
        "scalar producer row is covered more than once",
        span,
    ))
}

fn exact_transaction_target_storage(
    problem: &SolveProblem,
    target: &EventTransactionTarget,
) -> Option<SolveVariableStorageRun> {
    problem
        .solve_layout
        .variable_storage_runs
        .iter()
        .copied()
        .find(|storage| {
            storage.base == target.base()
                && storage.scalar_count == target.value_type().scalar_count() as usize
                && storage.event_iteration_kind().is_some()
        })
}

const fn scalar_slot_index(slot: ScalarSlot) -> Option<usize> {
    match slot {
        ScalarSlot::P { index, .. } => Some(index),
        ScalarSlot::Y { .. } | ScalarSlot::Time | ScalarSlot::Constant(_) => None,
    }
}

fn require_transaction_clock(
    producer: Option<PeriodicClockId>,
    transaction: Option<PeriodicClockId>,
    program_index: usize,
    span: Span,
) -> Result<(), SolveProblemShapeContractError> {
    if producer == transaction {
        return Ok(());
    }
    Err(event_transaction_contract(
        program_index,
        "producer and transaction clocks disagree",
        span,
    ))
}

fn validate_complete_scalar_program_coverage(
    problem: &SolveProblem,
    owners: &HashMap<usize, usize>,
) -> Result<(), SolveProblemShapeContractError> {
    let mut output_cursor = 0usize;
    for program in problem.discrete.rhs.programs() {
        let count = ScalarProgramBlock::program_output_count(program);
        let outputs = &problem.discrete.rhs.output_indices()[output_cursor..output_cursor + count];
        output_cursor += count;
        let Some(transaction) = outputs
            .iter()
            .find_map(|output| owners.get(output))
            .copied()
        else {
            continue;
        };
        if outputs
            .iter()
            .any(|output| owners.get(output) != Some(&transaction))
        {
            return Err(event_iteration_contract(
                transaction,
                "a transaction does not cover one complete scalar producer program",
            ));
        }
    }
    Ok(())
}

fn validate_complete_guarded_program_coverage(
    problem: &SolveProblem,
    owners: &HashMap<(usize, usize), usize>,
) -> Result<(), SolveProblemShapeContractError> {
    for (program_index, program) in problem.discrete.guarded_assignments.iter().enumerate() {
        let Some(transaction) = (0..program.target_ranges().len())
            .find_map(|target| owners.get(&(program_index, target)).copied())
        else {
            continue;
        };
        if (0..program.target_ranges().len())
            .any(|target| owners.get(&(program_index, target)) != Some(&transaction))
        {
            return Err(event_iteration_contract(
                transaction,
                "a transaction does not cover one complete guarded producer program",
            ));
        }
    }
    Ok(())
}

fn event_transaction_contract(
    program_index: usize,
    detail: &'static str,
    span: Span,
) -> SolveProblemShapeContractError {
    SolveProblemShapeContractError::EventTransactionProgram {
        program_index,
        detail,
        span: Some(span),
    }
}

fn validate_guarded_event_owner(
    problem: &SolveProblem,
    row: usize,
    program_index: usize,
    target_range_index: usize,
    storage: SolveVariableStorageRun,
    claimed_ranges: &mut BTreeSet<(usize, usize)>,
) -> Result<(), SolveProblemShapeContractError> {
    let program = problem
        .discrete
        .guarded_assignments
        .get(program_index)
        .ok_or_else(|| event_iteration_contract(row, "guarded owner is out of bounds"))?;
    let range = program
        .target_ranges()
        .get(target_range_index)
        .ok_or_else(|| event_iteration_contract(row, "guarded target range is out of bounds"))?;
    if range.base() != storage.base || range.count() != storage.scalar_count {
        return Err(event_iteration_contract(
            row,
            "guarded target range does not define the run",
        ));
    }
    if program.role() != DiscreteRowRole::Equation {
        return Err(event_iteration_contract(
            row,
            "guarded event-plan owner is not an equation",
        ));
    }
    if !claimed_ranges.insert((program_index, target_range_index)) {
        return Err(event_iteration_contract(
            row,
            "guarded target range is claimed more than once",
        ));
    }
    Ok(())
}

fn validate_scalar_event_owner(
    problem: &SolveProblem,
    row: usize,
    start_row: usize,
    scalar_count: usize,
    current_base: usize,
    claimed_rows: &mut BTreeSet<usize>,
) -> Result<(), SolveProblemShapeContractError> {
    let row_end = start_row
        .checked_add(scalar_count)
        .ok_or_else(|| event_iteration_contract(row, "owner row range overflowed"))?;
    let targets = problem
        .discrete
        .update_targets
        .get(start_row..row_end)
        .ok_or_else(|| event_iteration_contract(row, "owner row range is out of bounds"))?;
    let clocks = problem
        .discrete
        .clock_owners
        .get(start_row..row_end)
        .ok_or_else(|| event_iteration_contract(row, "owner clock range is out of bounds"))?;
    let owner_clock = clocks.first().copied().flatten();
    for (offset, target) in targets.iter().enumerate() {
        let expected_target = current_base
            .checked_add(offset)
            .ok_or_else(|| event_iteration_contract(row, "owner target range overflowed"))?;
        if *target != scalar_slot_p(expected_target) {
            return Err(event_iteration_contract(
                row,
                "owner rows do not define the run",
            ));
        }
        if clocks[offset] != owner_clock {
            return Err(event_iteration_contract(
                row,
                "owner rows disagree on their typed clock",
            ));
        }
        claimed_rows.insert(start_row + offset);
    }
    Ok(())
}

fn validate_structured_event_owner(
    problem: &SolveProblem,
    row: usize,
    update_index: usize,
    storage: SolveVariableStorageRun,
    claimed_updates: &mut BTreeSet<usize>,
) -> Result<(), SolveProblemShapeContractError> {
    let update = problem
        .discrete
        .structured_updates
        .get(update_index)
        .ok_or_else(|| event_iteration_contract(row, "structured owner is out of bounds"))?;
    let Some(ComputeNode::Map { domain, .. }) =
        problem.discrete.structured_rhs.nodes.get(update.node_index)
    else {
        return Err(event_iteration_contract(
            row,
            "structured owner is not a compact Map",
        ));
    };
    let dense = TensorOutputMap::dense_contiguous(0, domain)
        .map_err(|_| event_iteration_contract(row, "structured owner domain is invalid"))?;
    if update.target.base != storage.base
        || update.target.map != dense
        || domain.scalar_count().ok() != Some(storage.scalar_count)
    {
        return Err(event_iteration_contract(
            row,
            "structured owner does not define the run",
        ));
    }
    claimed_updates.insert(update_index);
    Ok(())
}

fn validate_scalar_event_producers(
    problem: &SolveProblem,
    claimed_rows: &BTreeSet<usize>,
) -> Result<(), SolveProblemShapeContractError> {
    let layout = &problem.solve_layout;
    for (row, target) in problem.discrete.update_targets.iter().copied().enumerate() {
        let storage_variable = storage_variable_for_slot(layout, target)
            .map_err(|detail| event_iteration_contract(row, detail))?;
        let is_discrete = storage_variable.is_some_and(|variable| {
            layout.variable_storage_runs[variable]
                .event_iteration_kind()
                .is_some()
        });
        if storage_variable.is_some() && !is_discrete {
            return Err(event_iteration_contract(
                row,
                "a producer targets a canonical non-discrete variable",
            ));
        }
        if problem.discrete.row_roles.get(row) == Some(&DiscreteRowRole::Equation)
            && (!is_discrete || !claimed_rows.contains(&row))
        {
            return Err(event_iteration_contract(
                row,
                "an equation producer is not owned by exactly one typed event-plan variable",
            ));
        }
        if is_discrete && !claimed_rows.contains(&row) {
            return Err(event_iteration_contract(
                row,
                "a scalar discrete producer is not owned by its plan run",
            ));
        }
        if external_input_storage_contains(layout, target) {
            return Err(event_iteration_contract(
                row,
                "an external input cannot have a discrete producer",
            ));
        }
    }
    Ok(())
}

fn validate_structured_event_producers(
    problem: &SolveProblem,
    claimed_updates: &BTreeSet<usize>,
) -> Result<(), SolveProblemShapeContractError> {
    let layout = &problem.solve_layout;
    for (update_index, update) in problem.discrete.structured_updates.iter().enumerate() {
        let storage_variable = storage_variable_for_slot(layout, update.target.base)
            .map_err(|detail| event_iteration_contract(update_index, detail))?;
        let is_discrete = storage_variable.is_some_and(|variable| {
            layout.variable_storage_runs[variable]
                .event_iteration_kind()
                .is_some()
        });
        if storage_variable.is_some() && !is_discrete {
            return Err(event_iteration_contract(
                update_index,
                "a structured producer targets a canonical non-discrete variable",
            ));
        }
        if update.role == DiscreteRowRole::Equation
            && (!is_discrete || !claimed_updates.contains(&update_index))
        {
            return Err(event_iteration_contract(
                update_index,
                "a structured equation producer is not owned by exactly one typed event-plan variable",
            ));
        }
        if is_discrete && !claimed_updates.contains(&update_index) {
            return Err(event_iteration_contract(
                update_index,
                "a structured discrete producer is not owned by its plan run",
            ));
        }
        if external_input_storage_contains(layout, update.target.base) {
            return Err(event_iteration_contract(
                update_index,
                "an external input cannot have a structured producer",
            ));
        }
    }
    Ok(())
}

fn validate_guarded_event_producers(
    problem: &SolveProblem,
    claimed_ranges: &BTreeSet<(usize, usize)>,
) -> Result<(), SolveProblemShapeContractError> {
    let layout = &problem.solve_layout;
    for (program_index, program) in problem.discrete.guarded_assignments.iter().enumerate() {
        for (target_range_index, range) in program.target_ranges().iter().enumerate() {
            let storage_variable = storage_variable_for_slot(layout, range.base())
                .map_err(|detail| event_iteration_contract(program_index, detail))?;
            let is_discrete = storage_variable.is_some_and(|variable| {
                layout.variable_storage_runs[variable]
                    .event_iteration_kind()
                    .is_some()
            });
            let claimed = claimed_ranges.contains(&(program_index, target_range_index));
            if program.role() == DiscreteRowRole::Equation && (!is_discrete || !claimed) {
                return Err(event_iteration_contract(
                    program_index,
                    "a guarded equation target is not owned by exactly one typed event-plan variable",
                ));
            }
            if is_discrete && !claimed {
                return Err(event_iteration_contract(
                    program_index,
                    "a guarded discrete target is not owned by its plan run",
                ));
            }
            if external_input_storage_contains(layout, range.base()) {
                return Err(event_iteration_contract(
                    program_index,
                    "an external input cannot have a guarded producer",
                ));
            }
        }
    }
    Ok(())
}
fn storage_variable_for_slot(
    layout: &SolveLayout,
    slot: ScalarSlot,
) -> Result<Option<usize>, &'static str> {
    let mut owners = layout
        .variable_storage_runs
        .iter()
        .enumerate()
        .filter_map(|(variable, storage)| storage_run_contains(*storage, slot).then_some(variable));
    let first = owners.next();
    if owners.next().is_some() {
        return Err("one producer slot belongs to multiple variable-storage runs");
    }
    Ok(first)
}

fn external_input_storage_contains(layout: &SolveLayout, slot: ScalarSlot) -> bool {
    layout.variable_storage_runs.iter().any(|storage| {
        storage.role == SolveVariableStorageRole::ExternalInput
            && storage_run_contains(*storage, slot)
    })
}

fn storage_run_contains(storage: SolveVariableStorageRun, slot: ScalarSlot) -> bool {
    match (storage.base, slot) {
        (ScalarSlot::P { index: base, .. }, ScalarSlot::P { index, .. })
        | (ScalarSlot::Y { index: base, .. }, ScalarSlot::Y { index, .. }) => base
            .checked_add(storage.scalar_count)
            .is_some_and(|end| (base..end).contains(&index)),
        _ => false,
    }
}

fn event_iteration_contract(row: usize, detail: &'static str) -> SolveProblemShapeContractError {
    SolveProblemShapeContractError::DiscreteCertificate {
        context: "discrete.event_iteration_plan",
        row,
        detail,
        span: None,
    }
}

fn validate_continuous_system_shape(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    let system = &problem.continuous;
    system
        .implicit_rhs
        .validate_shape_contract("continuous.implicit_rhs")?;
    system
        .residual
        .validate_shape_contract("continuous.residual")?;
    system
        .manifold_residual
        .validate_shape_contract("continuous.manifold_residual")?;
    system
        .derivative_rhs
        .validate_shape_contract("continuous.derivative_rhs")?;
    system
        .refresh_owners
        .validate_against(&system.implicit_rhs)
        .map_err(
            |error| SolveProblemShapeContractError::ContinuousRefreshOwner {
                detail: error.to_string(),
            },
        )?;
    for (context, block) in [
        ("continuous.implicit_rhs", &system.implicit_rhs),
        ("continuous.residual", &system.residual),
        ("continuous.manifold_residual", &system.manifold_residual),
        ("continuous.derivative_rhs", &system.derivative_rhs),
    ] {
        variable_bounds::validate_compute_block_variable_bounds(block, context, &problem.layout)?;
    }
    let implicit_count = system
        .implicit_rhs
        .output_count("continuous.implicit_rhs")?;
    validate_count(
        "continuous.implicit_row_targets",
        implicit_count,
        system.implicit_row_targets.len(),
    )?;
    validate_projection_plan(
        "continuous.algebraic_projection_plan",
        &system.algebraic_projection_plan,
        implicit_count,
        problem.solve_layout.solver_scalar_count(),
    )?;
    validate_implicit_output_ownership(problem)?;
    let manifold_count = system
        .manifold_residual
        .output_count("continuous.manifold_residual")?;
    validate_manifold_projection_plan(
        "continuous.manifold_projection_plan",
        &system.manifold_projection_plan,
        manifold_count,
        problem.solve_layout.state_scalar_count(),
    )
}

fn validate_implicit_output_ownership(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    let system = &problem.continuous;
    let produced = system
        .implicit_rhs
        .produced_output_indices("continuous.implicit_rhs")?
        .into_iter()
        .collect::<BTreeSet<_>>();
    let projection_pairs = system
        .algebraic_projection_plan
        .blocks
        .iter()
        .flat_map(|block| {
            block
                .rows
                .iter()
                .copied()
                .zip(block.y_indices.iter().copied())
        })
        .collect::<BTreeSet<_>>();
    let state_count = problem.solve_layout.state_scalar_count();
    let required_algebraic_end = state_count
        .checked_add(problem.solve_layout.algebraic_scalar_count())
        .ok_or_else(|| SolveProblemShapeContractError::ContinuousRefreshOwner {
            detail: "continuous algebraic Y inventory overflows host range".to_string(),
        })?;
    let mut target_pairs = BTreeSet::new();
    for (row, target) in system
        .implicit_row_targets
        .iter()
        .enumerate()
        .filter_map(|(row, target)| target.map(|target| (row, target)))
    {
        let ScalarSlot::Y { index, .. } = target else {
            return implicit_output_ownership_error(
                &produced,
                &projection_pairs,
                &target_pairs,
                state_count,
                required_algebraic_end,
            );
        };
        if !(state_count..problem.solve_layout.solver_scalar_count()).contains(&index)
            || !target_pairs.insert((row, index))
        {
            return implicit_output_ownership_error(
                &produced,
                &projection_pairs,
                &target_pairs,
                state_count,
                required_algebraic_end,
            );
        }
    }
    let target_rows = target_pairs
        .iter()
        .map(|(row, _)| *row)
        .collect::<BTreeSet<_>>();
    let target_indices = target_pairs
        .iter()
        .map(|(_, index)| *index)
        .collect::<BTreeSet<_>>();
    let required_indices = (state_count..required_algebraic_end).collect::<BTreeSet<_>>();
    if produced == target_rows
        && projection_pairs == target_pairs
        && required_indices.is_subset(&target_indices)
    {
        Ok(())
    } else {
        implicit_output_ownership_error(
            &produced,
            &projection_pairs,
            &target_pairs,
            state_count,
            required_algebraic_end,
        )
    }
}

fn implicit_output_ownership_error<T>(
    produced: &BTreeSet<usize>,
    projection_pairs: &BTreeSet<(usize, usize)>,
    target_pairs: &BTreeSet<(usize, usize)>,
    state_count: usize,
    required_algebraic_end: usize,
) -> Result<T, SolveProblemShapeContractError> {
    Err(SolveProblemShapeContractError::ContinuousRefreshOwner {
        detail: format!(
            "continuous implicit produced outputs {produced:?}, projection row/target pairs {projection_pairs:?}, and explicit row/target pairs {target_pairs:?} do not cover the required algebraic Y inventory {state_count}..{required_algebraic_end}"
        ),
    })
}

fn validate_initialization_system_shape(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    let system = &problem.initialization;
    system
        .residual
        .validate_shape_contract("initialization.residual")?;
    let residual_count = system.residual.len()?;
    validate_count(
        "initialization.row_targets",
        residual_count,
        system.row_targets.len(),
    )?;
    validate_count(
        "initialization.row_roles",
        residual_count,
        system.row_roles.len(),
    )?;
    validate_count(
        "initialization.update_targets",
        system.update_rhs.len(),
        system.update_targets.len(),
    )?;
    validate_initial_projection_unknowns(
        "initialization.projection_unknowns",
        &system.projection_unknowns,
        problem.solve_layout.solver_scalar_count(),
        problem.layout.p_scalars(),
    )?;
    validate_initial_projection_plan(
        "initialization.projection_plan",
        &system.projection_plan,
        residual_count,
        problem.solve_layout.solver_scalar_count(),
        problem.layout.p_scalars(),
    )
}

fn validate_discrete_system_shape(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    let system = &problem.discrete;
    certificate::validate_discrete_certificate_shape(problem)?;
    variable_bounds::validate_scalar_program_block_variable_bounds(
        &system.rhs,
        "discrete.rhs",
        &problem.layout,
    )?;
    system
        .structured_rhs
        .validate_shape_contract("discrete.structured_rhs")?;
    validate_count(
        "discrete.update_targets",
        system.rhs.len(),
        system.update_targets.len(),
    )?;
    validate_count(
        "discrete.row_roles",
        system.rhs.len(),
        system.row_roles.len(),
    )?;
    validate_count(
        "discrete.pre_modes",
        system.rhs.len(),
        system.pre_modes.len(),
    )?;
    validate_count(
        "discrete.observation_refresh",
        system.rhs.len(),
        system.observation_refresh.len(),
    )?;
    validate_observation_refresh_coupling(system)?;
    validate_count(
        "discrete.integrator_history_effects",
        system.rhs.len(),
        system.integrator_history_effects.len(),
    )?;
    validate_count(
        "discrete.clock_owners",
        system.rhs.len(),
        system.clock_owners.len(),
    )?;
    let clock_count = problem.clocks.periodic_event_schedules.len();
    for clock in system.clock_owners.iter().flatten().copied() {
        validate_indices("discrete.clock_owners", &[clock.index()], clock_count)?;
    }
    for clocks in &system.clock_partition_intermediate_clocks {
        for clock in clocks {
            validate_indices(
                "discrete.clock_partition_intermediate_clocks",
                &[clock.index()],
                clock_count,
            )?;
        }
    }
    validate_structured_discrete_shape(problem, clock_count)?;
    validate_guarded_assignment_shape(problem, clock_count)?;
    validate_event_transaction_shape(problem, clock_count)?;
    validate_count(
        "clocks.activation_parameter_indices",
        clock_count,
        problem.clocks.activation_parameter_indices.len(),
    )?;
    validate_indices(
        "clocks.activation_parameter_indices",
        &problem.clocks.activation_parameter_indices,
        problem.layout.p_scalars(),
    )?;
    validate_unique_indices(
        "clocks.activation_parameter_indices",
        &problem.clocks.activation_parameter_indices,
    )?;
    Ok(())
}

fn validate_observation_refresh_coupling(
    system: &DiscreteSolveSystem,
) -> Result<(), SolveProblemShapeContractError> {
    for (row, selected) in system.observation_refresh.iter().copied().enumerate() {
        if !selected {
            continue;
        }
        if system.clock_owners.get(row) != Some(&None)
            || system.pre_modes.get(row) != Some(&DiscreteEventPreMode::FollowCurrent)
        {
            return Err(SolveProblemShapeContractError::DiscreteCertificate {
                context: "discrete.observation_refresh",
                row,
                detail: "selected observation row is not unclocked FollowCurrent",
                span: system.rhs.span_for_output(row),
            });
        }
    }
    if system.observation_refresh_reads_y {
        return Ok(());
    }
    let mut output_ordinal = 0usize;
    for (program_index, program) in system.rhs.programs().iter().enumerate() {
        let span = system.rhs.program_span(program_index);
        let dependencies =
            StructuralPattern::derive_output_y_dependencies(program, span).map_err(|_| {
                SolveProblemShapeContractError::DiscreteCertificate {
                    context: "discrete.observation_refresh_reads_y",
                    row: program_index,
                    detail: "observation dependency program is not certifiable",
                    span,
                }
            })?;
        for y_dependencies in dependencies {
            let row = system
                .rhs
                .output_indices()
                .get(output_ordinal)
                .copied()
                .ok_or(SolveProblemShapeContractError::DiscreteCertificate {
                    context: "discrete.observation_refresh_reads_y",
                    row: program_index,
                    detail: "observation output has no row identity",
                    span,
                })?;
            if system.observation_refresh.get(row) == Some(&true) && !y_dependencies.is_empty() {
                return Err(SolveProblemShapeContractError::DiscreteCertificate {
                    context: "discrete.observation_refresh_reads_y",
                    row,
                    detail: "false certificate omits a selected row Y dependency",
                    span: system.rhs.span_for_output(row),
                });
            }
            output_ordinal = output_ordinal.checked_add(1).ok_or(
                SolveProblemShapeContractError::DiscreteCertificate {
                    context: "discrete.observation_refresh_reads_y",
                    row,
                    detail: "observation output ordinal overflow",
                    span,
                },
            )?;
        }
    }
    Ok(())
}

fn validate_event_transaction_shape(
    problem: &SolveProblem,
    clock_count: usize,
) -> Result<(), SolveProblemShapeContractError> {
    let mut claimed_actions = BTreeSet::new();
    for (program_index, program) in problem.discrete.event_transactions.iter().enumerate() {
        for &clock in program.clock_owners() {
            validate_indices(
                "discrete.event_transactions.clock_owners",
                &[clock.index()],
                clock_count,
            )?;
        }
        for input in program.inputs() {
            validate_event_transaction_storage_range(
                "discrete.event_transactions.input",
                input.source(),
                input.value_type().scalar_count() as usize,
                &problem.layout,
                Some(program.span()),
            )?;
        }
        for target in program.targets() {
            validate_event_transaction_storage_range(
                "discrete.event_transactions.target",
                target.base(),
                target.value_type().scalar_count() as usize,
                &problem.layout,
                Some(program.span()),
            )?;
        }
        if program.assertions().iter().any(|action| {
            action
                .clock_owner
                .is_some_and(|clock| clock.index() >= clock_count)
        }) {
            return Err(SolveProblemShapeContractError::EventTransactionProgram {
                program_index,
                detail: "assertion clock owner is out of bounds",
                span: Some(program.span()),
            });
        }
        validate_event_transaction_action_projections(
            problem,
            program_index,
            program,
            &mut claimed_actions,
        )?;
    }
    Ok(())
}

fn validate_event_transaction_action_projections(
    problem: &SolveProblem,
    program_index: usize,
    program: &EventTransactionProgram,
    claimed_actions: &mut BTreeSet<usize>,
) -> Result<(), SolveProblemShapeContractError> {
    for (assertion, action_indices) in program
        .assertions()
        .iter()
        .zip(program.assertion_action_indices())
    {
        validate_event_transaction_action_group(
            problem,
            program_index,
            program.span(),
            assertion,
            action_indices,
            claimed_actions,
        )?;
    }
    Ok(())
}

fn validate_event_transaction_action_group(
    problem: &SolveProblem,
    program_index: usize,
    span: Span,
    assertion: &SolveEventAction,
    action_indices: &[usize],
    claimed_actions: &mut BTreeSet<usize>,
) -> Result<(), SolveProblemShapeContractError> {
    for &action_index in action_indices {
        let action = problem.events.actions.get(action_index).ok_or_else(|| {
            event_transaction_contract(
                program_index,
                "assertion action projection is out of bounds",
                span,
            )
        })?;
        if action != assertion {
            return Err(event_transaction_contract(
                program_index,
                "assertion action projection does not match its predicate action",
                span,
            ));
        }
        if !claimed_actions.insert(action_index) {
            return Err(event_transaction_contract(
                program_index,
                "assertion action projection is covered more than once",
                span,
            ));
        }
    }
    Ok(())
}

fn validate_event_transaction_storage_range(
    context: &'static str,
    slot: ScalarSlot,
    count: usize,
    layout: &VarLayout,
    span: Option<Span>,
) -> Result<(), SolveProblemShapeContractError> {
    let (storage, start, extent) = match slot {
        ScalarSlot::Y { index, .. } => ("Y", index, layout.y_scalars()),
        ScalarSlot::P { index, .. } => ("P", index, layout.p_scalars()),
        ScalarSlot::Time | ScalarSlot::Constant(_) => return Ok(()),
    };
    let end = start.checked_add(count).ok_or(
        SolveProblemShapeContractError::VariableIndexOutOfBounds {
            context,
            storage,
            index: usize::MAX,
            extent,
            span,
        },
    )?;
    if end > extent {
        return Err(SolveProblemShapeContractError::VariableIndexOutOfBounds {
            context,
            storage,
            index: end - 1,
            extent,
            span,
        });
    }
    Ok(())
}

fn validate_guarded_assignment_shape(
    problem: &SolveProblem,
    clock_count: usize,
) -> Result<(), SolveProblemShapeContractError> {
    for (program_index, program) in problem.discrete.guarded_assignments.iter().enumerate() {
        variable_bounds::validate_guarded_assignment_variable_bounds(
            program,
            program_index,
            &problem.layout,
        )?;
        if let Some(clock) = program.clock_owner() {
            validate_indices(
                "discrete.guarded_assignments.clock_owner",
                &[clock.index()],
                clock_count,
            )?;
        }
        for range in program.target_ranges() {
            let (storage, base, extent) = match range.base() {
                ScalarSlot::Y { index, .. } => ("Y", index, problem.layout.y_scalars()),
                ScalarSlot::P { index, .. } => ("P", index, problem.layout.p_scalars()),
                ScalarSlot::Time | ScalarSlot::Constant(_) => {
                    unreachable!("checked guarded target range uses only Y/P storage")
                }
            };
            let end = base.checked_add(range.count()).ok_or(
                SolveProblemShapeContractError::GuardedAssignmentProgram {
                    program_index,
                    detail: "target range overflows",
                    span: Some(program.span()),
                },
            )?;
            if end > extent {
                return Err(SolveProblemShapeContractError::VariableIndexOutOfBounds {
                    context: "discrete.guarded_assignments.target",
                    storage,
                    index: end - 1,
                    extent,
                    span: Some(program.span()),
                });
            }
        }
    }
    Ok(())
}

fn validate_structured_discrete_shape(
    problem: &SolveProblem,
    clock_count: usize,
) -> Result<(), SolveProblemShapeContractError> {
    let system = &problem.discrete;
    validate_count(
        "discrete.structured_updates",
        system.structured_rhs.nodes.len(),
        system.structured_updates.len(),
    )?;
    let scalar_targets = system
        .update_targets
        .iter()
        .filter_map(|target| match target {
            ScalarSlot::Y { index, .. } => Some(("Y", *index)),
            ScalarSlot::P { index, .. } => Some(("P", *index)),
            ScalarSlot::Time | ScalarSlot::Constant(_) => None,
        })
        .collect::<BTreeSet<_>>();
    let mut structured_nodes = BTreeSet::new();
    let mut structured_targets = BTreeSet::new();
    for (update_index, update) in system.structured_updates.iter().enumerate() {
        if !structured_nodes.insert(update.node_index) {
            return Err(SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: update.node_index,
                detail: "compute node is claimed by more than one update",
                span: None,
            });
        }
        if let Some(clock) = update.clock_owner {
            validate_indices(
                "discrete.structured_updates.clock_owner",
                &[clock.index()],
                clock_count,
            )?;
        }
        for (target, _) in system.structured_assignments(update_index)? {
            let (storage, index, extent) = match target {
                ScalarSlot::Y { index, .. } => ("Y", index, problem.layout.y_scalars()),
                ScalarSlot::P { index, .. } => ("P", index, problem.layout.p_scalars()),
                ScalarSlot::Time | ScalarSlot::Constant(_) => {
                    unreachable!("structured_assignments admits only Y/P target bases")
                }
            };
            if index >= extent {
                return Err(SolveProblemShapeContractError::VariableIndexOutOfBounds {
                    context: "discrete.structured_updates.target",
                    storage,
                    index,
                    extent,
                    span: None,
                });
            }
            if scalar_targets.contains(&(storage, index)) {
                return Err(SolveProblemShapeContractError::StructuredDiscreteUpdate {
                    update_index,
                    node_index: update.node_index,
                    detail: "target is also owned by a scalar discrete update",
                    span: None,
                });
            }
            if !structured_targets.insert((storage, index)) {
                return Err(SolveProblemShapeContractError::DuplicateIndex {
                    context: "discrete.structured_updates.target",
                    index,
                    span: None,
                });
            }
        }
    }
    Ok(())
}

fn validate_event_partition_shape(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    let events = &problem.events;
    certificate::validate_root_certificate_shape(problem)?;
    validate_count(
        "events.root_relation_memory_targets",
        events.root_conditions.len(),
        events.root_relation_memory_targets.len(),
    )?;
    validate_count(
        "events.root_zero_domains",
        events.root_conditions.len(),
        events.root_zero_domains.len(),
    )?;
    validate_scheduled_root_conditions(
        "events.scheduled_root_conditions",
        &events.scheduled_root_conditions,
        events.root_conditions.len(),
    )?;
    validate_count(
        "events.action_conditions",
        events.actions.len(),
        events.action_conditions.len(),
    )?;
    let clock_count = problem.clocks.periodic_event_schedules.len();
    for owner in events
        .actions
        .iter()
        .filter_map(|action| action.clock_owner)
    {
        validate_indices("events.actions.clock_owner", &[owner.index()], clock_count)?;
    }
    validate_terminal_event_shape(problem)?;
    validate_delay_partition_shape(problem)
}

fn validate_terminal_event_shape(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    match (
        problem.events.has_terminal_event,
        problem.solve_layout.terminal_event_parameter_index,
    ) {
        (true, Some(index)) => validate_indices(
            "solve_layout.terminal_event_parameter_index",
            &[index],
            problem.layout.p_scalars(),
        ),
        (true, None) => validate_count("solve_layout.terminal_event_parameter_index", 1, 0),
        (false, Some(_)) => validate_count("solve_layout.terminal_event_parameter_index", 0, 1),
        (false, None) => Ok(()),
    }
}

fn validate_delay_partition_shape(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    let delays = &problem.events.delays;
    let delay_count = delays.source_rhs.len();
    validate_count(
        "events.delays.delay_time_rhs",
        delay_count,
        delays.delay_time_rhs.len(),
    )?;
    validate_count(
        "events.delays.delay_max_rhs",
        delay_count,
        delays.delay_max_rhs.len(),
    )?;
    validate_count(
        "events.delays.value_parameter_indices",
        delay_count,
        delays.value_parameter_indices.len(),
    )?;
    validate_count(
        "events.delays.source_is_discrete",
        delay_count,
        delays.source_is_discrete.len(),
    )?;
    validate_indices(
        "events.delays.value_parameter_indices",
        &delays.value_parameter_indices,
        problem.layout.p_scalars(),
    )?;
    validate_unique_indices(
        "events.delays.value_parameter_indices",
        &delays.value_parameter_indices,
    )
}

fn linear_ops_use_linear_solve_component(ops: &[LinearOp]) -> bool {
    ops.iter()
        .any(|op| matches!(op, LinearOp::LinearSolveComponent { .. }))
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

fn validate_unique_indices(
    context: &'static str,
    indices: &[usize],
) -> Result<(), SolveProblemShapeContractError> {
    let mut seen = BTreeSet::new();
    for &index in indices {
        if seen.insert(index) {
            continue;
        }
        return Err(SolveProblemShapeContractError::DuplicateIndex {
            context,
            index,
            span: None,
        });
    }
    Ok(())
}

fn validate_scheduled_root_conditions(
    context: &'static str,
    roots: &[ScheduledRootCondition],
    upper_bound: usize,
) -> Result<(), SolveProblemShapeContractError> {
    for root in roots {
        validate_indices(context, &[root.root_index], upper_bound)?;
        if root.period_seconds.is_finite()
            && root.period_seconds > 0.0
            && root.phase_seconds.is_finite()
        {
            continue;
        }
        return Err(SolveProblemShapeContractError::InvalidScheduledRootTiming {
            context,
            root_index: root.root_index,
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
    let mut rows_seen = BTreeSet::new();
    let mut unknowns_seen = BTreeSet::new();
    for block in &plan.blocks {
        validate_projection_block_shape(context, block.rows.len(), block.y_indices.len())?;
        validate_indices(context, &block.rows, row_upper_bound)?;
        validate_indices(context, &block.y_indices, y_upper_bound)?;
        validate_unique_projection_indices(context, &block.rows, &mut rows_seen)?;
        validate_unique_projection_indices(context, &block.y_indices, &mut unknowns_seen)?;
    }
    Ok(())
}

fn validate_manifold_projection_plan(
    context: &'static str,
    plan: &AlgebraicProjectionPlan,
    row_upper_bound: usize,
    state_upper_bound: usize,
) -> Result<(), SolveProblemShapeContractError> {
    let mut rows_seen = BTreeSet::new();
    let mut states_seen = BTreeSet::new();
    for block in &plan.blocks {
        if block.rows.is_empty()
            || block.y_indices.is_empty()
            || block.rows.len() > block.y_indices.len()
        {
            return Err(
                SolveProblemShapeContractError::ProjectionBlockShapeMismatch {
                    context,
                    row_count: block.rows.len(),
                    unknown_count: block.y_indices.len(),
                    span: None,
                },
            );
        }
        validate_indices(context, &block.rows, row_upper_bound)?;
        validate_indices(context, &block.y_indices, state_upper_bound)?;
        validate_unique_projection_indices(context, &block.rows, &mut rows_seen)?;
        validate_unique_projection_indices(context, &block.y_indices, &mut states_seen)?;
    }
    validate_count(context, row_upper_bound, rows_seen.len())
}

fn validate_initial_projection_plan(
    context: &'static str,
    plan: &InitializationProjectionPlan,
    row_upper_bound: usize,
    y_upper_bound: usize,
    p_upper_bound: usize,
) -> Result<(), SolveProblemShapeContractError> {
    let mut rows_seen = BTreeSet::new();
    let mut unknowns_seen = BTreeSet::new();
    for block in &plan.blocks {
        validate_projection_block_shape(context, block.rows.len(), block.unknowns.len())?;
        validate_indices(context, &block.rows, row_upper_bound)?;
        validate_initial_projection_unknowns(
            context,
            &block.unknowns,
            y_upper_bound,
            p_upper_bound,
        )?;
        validate_unique_projection_indices(context, &block.rows, &mut rows_seen)?;
        for unknown in &block.unknowns {
            let Some(key) = projection_unknown_key(*unknown) else {
                return Err(SolveProblemShapeContractError::InvalidProjectionUnknown {
                    context,
                    unknown: format!("{unknown:?}"),
                    y_upper_bound,
                    p_upper_bound,
                    span: None,
                });
            };
            if unknowns_seen.insert(key) {
                continue;
            }
            return Err(SolveProblemShapeContractError::DuplicateProjectionUnknown {
                context,
                unknown: format!("{unknown:?}"),
                span: None,
            });
        }
    }
    Ok(())
}

fn validate_projection_block_shape(
    context: &'static str,
    row_count: usize,
    unknown_count: usize,
) -> Result<(), SolveProblemShapeContractError> {
    if row_count == unknown_count {
        return Ok(());
    }
    Err(
        SolveProblemShapeContractError::ProjectionBlockShapeMismatch {
            context,
            row_count,
            unknown_count,
            span: None,
        },
    )
}

fn validate_unique_projection_indices(
    context: &'static str,
    indices: &[usize],
    seen: &mut BTreeSet<usize>,
) -> Result<(), SolveProblemShapeContractError> {
    for &index in indices {
        if seen.insert(index) {
            continue;
        }
        return Err(SolveProblemShapeContractError::DuplicateIndex {
            context,
            index,
            span: None,
        });
    }
    Ok(())
}

fn projection_unknown_key(slot: ScalarSlot) -> Option<(bool, usize)> {
    match slot {
        ScalarSlot::Y { index, .. } => Some((false, index)),
        ScalarSlot::P { index, .. } => Some((true, index)),
        ScalarSlot::Time | ScalarSlot::Constant(_) => None,
    }
}

fn validate_initial_projection_unknowns(
    context: &'static str,
    unknowns: &[ScalarSlot],
    y_upper_bound: usize,
    p_upper_bound: usize,
) -> Result<(), SolveProblemShapeContractError> {
    let mut seen = BTreeSet::new();
    for unknown in unknowns {
        let key = match *unknown {
            ScalarSlot::Y { index, .. } if index < y_upper_bound => Some((false, index)),
            ScalarSlot::P { index, .. } if index < p_upper_bound => Some((true, index)),
            _ => None,
        };
        let Some(key) = key else {
            return Err(SolveProblemShapeContractError::InvalidProjectionUnknown {
                context,
                unknown: format!("{unknown:?}"),
                y_upper_bound,
                p_upper_bound,
                span: None,
            });
        };
        if !seen.insert(key) {
            return Err(SolveProblemShapeContractError::DuplicateProjectionUnknown {
                context,
                unknown: format!("{unknown:?}"),
                span: None,
            });
        }
    }
    Ok(())
}
