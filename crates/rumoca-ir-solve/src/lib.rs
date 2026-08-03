//! Solver-facing Solve IR.
//!
//! This crate contains data consumed by simulation backends after DAE-level
//! structural/lowering phases. It must stay free of DAE evaluation and phase
//! logic.
//!
//! SPEC_0021 file-size exception: the Solve IR root still owns its checked wire
//! forms and the aggregate `SolveProblem` validation boundary. split plan: move
//! scalar-program wire validation into `scalar_program.rs` and `SolveProblem`
//! wire/shape validation into `solve_problem.rs`, retaining facade re-exports.

mod certificate;
#[cfg(test)]
mod certificate_tests;
#[cfg(test)]
mod compute_block_tests;
mod layout;
mod linear_op;
#[cfg(test)]
mod scalar_program_tests;
mod shape_error;
mod variable_bounds;
pub mod visitor;

use indexmap::IndexMap;
use rumoca_core::{
    ExternalTableData, ProvenanceSpan, SourceId, Span, StructuredIndexDomain,
    StructuredIndexDomainError,
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;

pub use certificate::{
    derive_root_reachable_runtime_rows, derive_root_relation_refresh_roles,
    derive_runtime_assignment_roles,
};
pub use layout::{
    ComponentReferenceKey, ComponentReferenceKeyError, ComponentReferenceKeyErrorKind,
    ComponentReferenceKeyPart, ComponentReferenceSubscriptKey, IndexedScalarSlot, ScalarSlot,
    VarLayout, VarLayoutShapeContractError, scalar_slot_p, scalar_slot_y,
};
pub use linear_op::{
    BinaryOp, CompareOp, LinearOp, RandomGenerator, Reg, ScalarProgramRegisterError,
    ScalarProgramRegisterFlow, UnaryOp, resolve_indexed_slot,
};
pub use shape_error::{AffineTensorNodeKind, SolveProblemShapeContractError};
pub use visitor::{
    LinearOpSliceKind, SolveVisitor, VisitScope, walk_compute_block, walk_compute_node,
    walk_scalar_program_block, walk_solve_artifacts, walk_solve_model, walk_solve_problem,
};

pub const SOLVE_SCHEMA_VERSION: u16 = 31;

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
        validate_scalar_program_register_flows("ScalarProgramBlock", 0, &programs, &program_spans)?;
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

fn validate_scalar_program_register_flows(
    context: &str,
    node_index: usize,
    programs: &[Vec<LinearOp>],
    program_spans: &[Span],
) -> Result<(), SolveProblemShapeContractError> {
    for (program_index, program) in programs.iter().enumerate() {
        let error = match ScalarProgramRegisterFlow::derive(program) {
            Ok(_) => continue,
            Err(error) => error,
        };
        let span = program_spans.get(program_index).copied();
        return Err(SolveProblemShapeContractError::ScalarProgramRegisterFlow {
            context: context.to_string(),
            node_index,
            program_index,
            error,
            span,
        });
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

        let problem = Self {
            schema_version: wire.schema_version,
            layout: wire.layout,
            solve_layout: wire.solve_layout,
            continuous: wire.continuous,
            initialization: wire.initialization,
            discrete: wire.discrete,
            events: wire.events,
            clocks: wire.clocks,
        };
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

fn validate_event_iteration_plan(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    let layout = &problem.solve_layout;
    let plan = &problem.discrete.event_iteration_plan;
    let expected_variables = layout
        .variable_storage_runs
        .iter()
        .enumerate()
        .filter_map(|(variable, storage)| {
            (storage.scalar_count != 0 && storage.event_iteration_kind().is_some())
                .then_some(variable)
        })
        .collect::<BTreeSet<_>>();
    let mut covered_variables = BTreeSet::new();
    let mut covered_bindings = BTreeSet::new();
    let mut current_indices = BTreeSet::new();
    let mut pre_indices = BTreeSet::new();
    let mut claimed_scalar_rows = BTreeSet::new();
    let mut claimed_structured_updates = BTreeSet::new();
    for (row, run) in plan.runs.iter().enumerate() {
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
        if !covered_variables.insert(run.variable) {
            return Err(event_iteration_contract(
                row,
                "variable owner is duplicated",
            ));
        }
        if storage.scalar_count == 0 {
            return Err(event_iteration_contract(row, "run is empty"));
        }
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
            if !covered_bindings.insert(binding_index) {
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
            if !current_indices.insert(current) || !pre_indices.insert(binding.dest_p_index) {
                return Err(event_iteration_contract(
                    row,
                    "current or pre lanes are duplicated",
                ));
            }
        }
        if current_indices
            .iter()
            .any(|index| pre_indices.contains(index))
        {
            return Err(event_iteration_contract(
                row,
                "current and pre lanes overlap",
            ));
        }
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
                let row_end = start_row
                    .checked_add(storage.scalar_count)
                    .ok_or_else(|| event_iteration_contract(row, "owner row range overflowed"))?;
                let targets = problem
                    .discrete
                    .update_targets
                    .get(start_row..row_end)
                    .ok_or_else(|| {
                        event_iteration_contract(row, "owner row range is out of bounds")
                    })?;
                let clocks = problem
                    .discrete
                    .clock_owners
                    .get(start_row..row_end)
                    .ok_or_else(|| {
                        event_iteration_contract(row, "owner clock range is out of bounds")
                    })?;
                let owner_clock = clocks.first().copied().flatten();
                for (offset, target) in targets.iter().enumerate() {
                    let expected_target = current_base.checked_add(offset).ok_or_else(|| {
                        event_iteration_contract(row, "owner target range overflowed")
                    })?;
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
                    claimed_scalar_rows.insert(start_row + offset);
                }
            }
            EventIterationOwner::StructuredUpdate { update_index } => {
                let update = problem
                    .discrete
                    .structured_updates
                    .get(update_index)
                    .ok_or_else(|| {
                        event_iteration_contract(row, "structured owner is out of bounds")
                    })?;
                let Some(ComputeNode::Map { domain, .. }) =
                    problem.discrete.structured_rhs.nodes.get(update.node_index)
                else {
                    return Err(event_iteration_contract(
                        row,
                        "structured owner is not a compact Map",
                    ));
                };
                let dense = TensorOutputMap::dense_contiguous(0, domain).map_err(|_| {
                    event_iteration_contract(row, "structured owner domain is invalid")
                })?;
                if update.target.base != storage.base
                    || update.target.map != dense
                    || domain.scalar_count().ok() != Some(storage.scalar_count)
                {
                    return Err(event_iteration_contract(
                        row,
                        "structured owner does not define the run",
                    ));
                }
                claimed_structured_updates.insert(update_index);
            }
        }
    }
    if covered_variables != expected_variables {
        return Err(event_iteration_contract(
            0,
            "plan is not a reverse bijection over typed discrete variable owners",
        ));
    }
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
            && (!is_discrete || !claimed_scalar_rows.contains(&row))
        {
            return Err(event_iteration_contract(
                row,
                "an equation producer is not owned by exactly one typed event-plan variable",
            ));
        }
        if is_discrete && !claimed_scalar_rows.contains(&row) {
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
            && (!is_discrete || !claimed_structured_updates.contains(&update_index))
        {
            return Err(event_iteration_contract(
                update_index,
                "a structured equation producer is not owned by exactly one typed event-plan variable",
            ));
        }
        if is_discrete && !claimed_structured_updates.contains(&update_index) {
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
    validate_structured_discrete_shape(problem, clock_count)?;
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

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ContinuousSolveSystem {
    pub implicit_rhs: ComputeBlock,
    pub implicit_row_targets: Vec<Option<ScalarSlot>>,
    pub algebraic_projection_plan: AlgebraicProjectionPlan,
    pub residual: ComputeBlock,
    /// Lower-order holonomic and velocity residuals retained when structural
    /// index reduction replaces them with acceleration-level equations.
    pub manifold_residual: ComputeBlock,
    /// Connected state-coordinate blocks used to project accepted numerical
    /// steps onto `manifold_residual = 0`. Blocks may have more state
    /// coordinates than residual rows; runtimes use a minimum-norm correction.
    pub manifold_projection_plan: AlgebraicProjectionPlan,
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
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InitializationProjectionPlan {
    pub blocks: Vec<InitializationProjectionBlock>,
}

impl InitializationProjectionPlan {
    pub fn is_empty(&self) -> bool {
        self.blocks.is_empty()
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InitializationProjectionBlock {
    pub rows: Vec<usize>,
    /// Initialization unknowns may reside in either solver Y storage or
    /// parameter P storage.  Time and constant slots are invalid here.
    pub unknowns: Vec<ScalarSlot>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveArtifacts {
    pub continuous: ContinuousSolveArtifacts,
    pub initialization: InitializationSolveArtifacts,
}

#[derive(Clone, Debug)]
pub struct JacobianStructure {
    pattern: StructuralPattern,
    coloring: ColumnColoring,
}

impl JacobianStructure {
    pub fn derived(pattern: StructuralPattern) -> Self {
        let coloring = pattern.column_coloring();
        Self { pattern, coloring }
    }

    pub const fn pattern(&self) -> &StructuralPattern {
        &self.pattern
    }

    pub const fn coloring(&self) -> &ColumnColoring {
        &self.coloring
    }
}

#[derive(Clone, Debug, Default)]
pub struct ContinuousStructuralArtifacts {
    implicit: Option<JacobianStructure>,
    manifold: Option<JacobianStructure>,
    derivative: Option<JacobianStructure>,
}

impl ContinuousStructuralArtifacts {
    pub fn derived(
        implicit: Option<StructuralPattern>,
        manifold: Option<StructuralPattern>,
        derivative: Option<StructuralPattern>,
    ) -> Self {
        Self {
            implicit: implicit.map(JacobianStructure::derived),
            manifold: manifold.map(JacobianStructure::derived),
            derivative: derivative.map(JacobianStructure::derived),
        }
    }

    pub const fn implicit(&self) -> Option<&JacobianStructure> {
        self.implicit.as_ref()
    }

    pub const fn manifold(&self) -> Option<&JacobianStructure> {
        self.manifold.as_ref()
    }

    pub const fn derivative(&self) -> Option<&JacobianStructure> {
        self.derivative.as_ref()
    }
}

#[derive(Clone, Debug, Default)]
pub struct InitializationStructuralArtifacts {
    residual: Option<JacobianStructure>,
}

impl InitializationStructuralArtifacts {
    pub fn derived(residual: Option<StructuralPattern>) -> Self {
        Self {
            residual: residual.map(JacobianStructure::derived),
        }
    }

    pub const fn residual(&self) -> Option<&JacobianStructure> {
        self.residual.as_ref()
    }
}

/// Compact solver-facing mass-matrix representation.
///
/// The matrix dimension is the state scalar count in the accompanying
/// [`SolveLayout`]. Identity therefore needs no payload, while general sparse
/// matrices retain only their nonzero entries.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum MassMatrix {
    #[default]
    Identity,
    Diagonal {
        values: Vec<f64>,
    },
    Sparse {
        entries: Vec<MassMatrixEntry>,
    },
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Serialize)]
pub struct MassMatrixEntry {
    pub row: usize,
    pub column: usize,
    pub value: f64,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ContinuousSolveArtifacts {
    /// Constructor-derived metadata; canonical Solve wire reconstructs it.
    #[serde(skip)]
    pub structural: ContinuousStructuralArtifacts,
    pub mass_matrix: MassMatrix,
    pub implicit_jacobian_v: ComputeBlock,
    /// Per-row forward-mode AD JVP of the *scalarized* `implicit_rhs`, row-aligned
    /// with successful `to_scalar_program_block(implicit_rhs)` output (and hence
    /// with the algebraic refresh plan's `row_idx`). Used by the state-only path
    /// to propagate the state seed through the algebraic projection
    /// (`d(alg)/d(state)`). Distinct from the tensor `implicit_jacobian_v`, whose
    /// scalarization is not row-aligned when the system has linear
    /// (`LinSolve`/`MatMul`) blocks.
    pub implicit_jacobian_v_scalar: ScalarProgramBlock,
    /// Forward-mode state Jacobian-vector product for
    /// [`ContinuousSolveSystem::manifold_residual`].
    pub manifold_jacobian_v: ComputeBlock,
    pub full_jacobian_v: ScalarProgramBlock,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InitializationSolveArtifacts {
    /// Constructor-derived metadata; canonical Solve wire reconstructs it.
    #[serde(skip)]
    pub structural: InitializationStructuralArtifacts,
    pub residual_jacobian_v: ComputeBlock,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InitializationSolveSystem {
    pub residual: ComputeBlock,
    pub row_targets: Vec<Option<ScalarSlot>>,
    /// What the initialization projection does with each residual row, indexed by
    /// equation index alongside `row_targets`.
    ///
    /// A runtime that only knows "this row has no target" cannot tell a row the
    /// rest of the system already determined — a legal MLS §8.6 consistency check
    /// — from a row nothing solved because it reads a coordinate outside the
    /// planned unknown space. Reporting the first when it is the second names the
    /// wrong defect, so the planner records which it is.
    pub row_roles: Vec<InitializationRowRole>,
    pub projection_unknowns: Vec<ScalarSlot>,
    pub projection_plan: InitializationProjectionPlan,
    pub update_rhs: ScalarProgramBlock,
    pub update_targets: Vec<ScalarSlot>,
}

/// What the MLS §8.6 initialization projection does with one residual row.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
pub enum InitializationRowRole {
    /// A projection block solves this row for the coordinate `row_targets` names.
    Solved,
    /// Every coordinate the row reads is determined by something other than this
    /// row — a `fixed = true` start, a pin, a binding, or another block — so the
    /// row is a consistency check between values the rest of the system fixed.
    /// MLS §8.6 permits such a row; a failure of one is a contradiction between
    /// declarations, not an unsolved coordinate.
    #[default]
    SurplusCheck,
    /// The row reads a coordinate the projection does not own, so nothing solved
    /// it and the residual is a check over a value the row cannot control.
    UnownedCoordinate(InitializationCoordinateKind),
}

/// Which coordinate kind kept a residual row out of the planned unknown space.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum InitializationCoordinateKind {
    /// A continuous algebraic or output coordinate. The initialization residual
    /// is certified against a reconstructed value, but the reduced
    /// initialization projection does not yet own the coordinate or its total
    /// derivative through the continuous system.
    Algebraic,
    /// A discrete-time coordinate or its `pre` value.
    Discrete,
    /// A coordinate the lowering cannot read per scalar: an array state, a
    /// multi-scalar row, or a structured family point.
    Unreadable,
    /// A coordinate the projection could own, but whose component the planner
    /// could not make square.
    Unmatched,
    /// Any other coordinate outside the planned space — an input, a delay, a
    /// `previous`, a relation memory, a terminal.
    Other,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum EventIterationValueKind {
    Real,
    Integer,
    Boolean,
    /// Positive integral ordinal. The DAE currently erases the declared upper
    /// literal bound; restoring that bound is a tracked upstream obligation.
    Enumeration,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum EventIterationOwner {
    Hold,
    ScalarRows { start_row: usize },
    StructuredUpdate { update_index: usize },
}

#[derive(Clone, Copy, Debug, PartialEq, Deserialize, Serialize)]
pub struct EventIterationRun {
    /// Canonical typed variable-storage owner.
    pub variable: usize,
    pub pre_binding_start: usize,
    pub owner: EventIterationOwner,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct EventIterationPlan {
    pub runs: Vec<EventIterationRun>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct DiscreteSolveSystem {
    /// Compact compiler-owned Appendix-B iteration catalog.
    pub event_iteration_plan: EventIterationPlan,
    pub runtime_assignment_rhs: ScalarProgramBlock,
    pub runtime_assignment_targets: Vec<ScalarSlot>,
    /// Compiler certificate for whether each runtime assignment evaluates a
    /// relation (directly or through another runtime assignment).
    pub runtime_assignment_roles: Vec<RuntimeAssignmentRole>,
    /// Relation-free root-driven assignments that remain valid after event
    /// `pre` history has committed.
    pub post_commit_assignment_rhs: ScalarProgramBlock,
    pub post_commit_assignment_targets: Vec<ScalarSlot>,
    /// Runtime-row owner copied by each post-commit row. Shape validation
    /// proves the copy is exact and the owner is relation-free.
    pub post_commit_assignment_runtime_rows: Vec<usize>,
    pub rhs: ScalarProgramBlock,
    pub update_targets: Vec<ScalarSlot>,
    pub row_roles: Vec<DiscreteRowRole>,
    pub pre_modes: Vec<DiscreteEventPreMode>,
    pub observation_refresh: Vec<bool>,
    /// Compiler-derived effect of changing each scalar update target on an
    /// integrator's continuous multistep history.
    ///
    /// This vector is row-aligned with `rhs`. A runtime may join the effect
    /// with exact update changes, but must not recover it from row position or
    /// model identity.
    pub integrator_history_effects: Vec<IntegratorHistoryEffect>,
    /// Periodic activation owner for each discrete row.
    ///
    /// `None` denotes an ordinary event-iteration row. A clock-owned row is
    /// evaluated only when the referenced exact lattice ticks.
    pub clock_owners: Vec<Option<PeriodicClockId>>,
    /// Compact B.1c maps. Scalar owners remain in `rhs`; a structured owner is
    /// represented exactly once here and is scalarized only by evaluation or
    /// backend adapter APIs.
    #[serde(default)]
    pub structured_rhs: ComputeBlock,
    #[serde(default)]
    pub structured_updates: Vec<StructuredDiscreteUpdate>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
pub enum RuntimeAssignmentRole {
    /// The row consumes already-selected values without evaluating a relation.
    RelationFree,
    /// The row evaluates a relation, depends on such a row, or writes relation memory.
    #[default]
    RelationEvaluating,
}

/// Compact target projection and row policy for one structured B.1c map node.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct StructuredDiscreteUpdate {
    /// Absolute index into [`DiscreteSolveSystem::structured_rhs`] nodes.
    pub node_index: usize,
    pub target: StructuredDiscreteTargetMap,
    pub role: DiscreteRowRole,
    pub pre_mode: DiscreteEventPreMode,
    pub observation_refresh: bool,
    pub integrator_history_effect: IntegratorHistoryEffect,
    pub clock_owner: Option<PeriodicClockId>,
}

/// One compact affine projection from map points to consecutive Y/P storage.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct StructuredDiscreteTargetMap {
    pub base: ScalarSlot,
    pub map: TensorOutputMap,
}

impl DiscreteSolveSystem {
    /// Checked scalar adapter view for one compact structured update.
    ///
    /// Each pair is `(target slot, structured_rhs output lane)`. Backends use
    /// this at their scalar boundary; the Solve IR retains only the compact map.
    pub fn structured_assignments(
        &self,
        update_index: usize,
    ) -> Result<Vec<(ScalarSlot, usize)>, SolveProblemShapeContractError> {
        let update = self.structured_updates.get(update_index).ok_or(
            SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: usize::MAX,
                detail: "update index is out of bounds",
                span: None,
            },
        )?;
        let node = self.structured_rhs.nodes.get(update.node_index).ok_or(
            SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: update.node_index,
                detail: "compute node index is out of bounds",
                span: None,
            },
        )?;
        let ComputeNode::Map {
            domain,
            output_map,
            span,
            ..
        } = node
        else {
            return Err(SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: update.node_index,
                detail: "compute node is not a Map",
                span: None,
            });
        };
        let sources = output_map.output_indices(domain).map_err(|_| {
            SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: update.node_index,
                detail: "compute output projection is invalid",
                span: Some(*span),
            }
        })?;
        let targets = update.target.map.output_indices(domain).map_err(|_| {
            SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: update.node_index,
                detail: "target projection is invalid",
                span: Some(*span),
            }
        })?;
        if sources.len() != targets.len() {
            return Err(SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: update.node_index,
                detail: "compute and target projections have different cardinality",
                span: Some(*span),
            });
        }
        targets
            .into_iter()
            .zip(sources)
            .map(|(offset, source)| {
                offset_scalar_slot(update.target.base, offset)
                    .map(|target| (target, source))
                    .ok_or(SolveProblemShapeContractError::StructuredDiscreteUpdate {
                        update_index,
                        node_index: update.node_index,
                        detail: "target base is not Y/P storage or its offset overflows",
                        span: Some(*span),
                    })
            })
            .collect()
    }
}

fn offset_scalar_slot(base: ScalarSlot, offset: usize) -> Option<ScalarSlot> {
    match base {
        ScalarSlot::Y { index, .. } => index.checked_add(offset).map(scalar_slot_y),
        ScalarSlot::P { index, .. } => index.checked_add(offset).map(scalar_slot_p),
        ScalarSlot::Time | ScalarSlot::Constant(_) => None,
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveEventPartition {
    pub root_conditions: ScalarProgramBlock,
    pub root_relation_memory_targets: Vec<Option<ScalarSlot>>,
    pub root_zero_domains: Vec<RootZeroDomain>,
    /// Compiler certificate describing which root memories may participate in
    /// post-commit algebraic coupling.
    pub root_relation_refresh_roles: Vec<RootRelationRefreshRole>,
    /// Hidden P slots that retain the previous value of each DAE condition.
    ///
    /// Event-action programs read these slots to distinguish a rising edge
    /// from a condition that merely remains true across an unrelated event.
    pub condition_memory_parameter_indices: Vec<usize>,
    pub scheduled_root_conditions: Vec<ScheduledRootCondition>,
    pub scheduled_time_events: Vec<f64>,
    pub dynamic_time_event_names: Vec<String>,
    pub dynamic_time_event_rhs: ScalarProgramBlock,
    pub action_conditions: ScalarProgramBlock,
    pub actions: Vec<SolveEventAction>,
    pub has_terminal_event: bool,
    pub delays: SolveDelayPartition,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
pub enum RootRelationRefreshRole {
    #[default]
    Frozen,
    AlgebraicDependent,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveDelayPartition {
    /// Current-value expressions whose accepted values populate history.
    pub source_rhs: ScalarProgramBlock,
    /// Delay amount evaluated at the query time.
    pub delay_time_rhs: ScalarProgramBlock,
    /// Maximum retained history horizon. When source `delayMax` is omitted,
    /// this row is identical to the corresponding delay-time row.
    pub delay_max_rhs: ScalarProgramBlock,
    /// Runtime-managed P slot receiving the delayed value for each row.
    pub value_parameter_indices: Vec<usize>,
    /// Whether the source uses piecewise-constant history rather than linear
    /// interpolation between accepted points.
    pub source_is_discrete: Vec<bool>,
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub enum RootZeroDomain {
    Positive,
    NonPositive,
    #[default]
    Previous,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ScheduledRootCondition {
    pub root_index: usize,
    pub period_seconds: f64,
    pub phase_seconds: f64,
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
    Conversion {
        value: Vec<LinearOp>,
        source: SolveStringConversionSource,
        format: SolveStringConversionFormat,
    },
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum SolveStringConversionSource {
    Real,
    Integer,
    Boolean,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum SolveStringConversionFormat {
    Options {
        minimum_length: Option<Vec<LinearOp>>,
        left_justified: Option<Vec<LinearOp>>,
        significant_digits: Option<Vec<LinearOp>>,
    },
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub enum SolveEventActionKind {
    Assert,
    Terminate,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveClockPartition {
    pub periodic_event_schedules: Vec<PeriodicEventSchedule>,
    /// Hidden Boolean-as-Real P slot for each typed periodic clock.
    ///
    /// The runtime derives each value from the schedule at the current event
    /// instant. These lanes make clock leaves computable inside mixed
    /// condition DAGs without creating another clock or row owner.
    pub activation_parameter_indices: Vec<usize>,
}

impl SolveClockPartition {
    pub fn periodic_clock_id(&self, index: usize) -> Option<PeriodicClockId> {
        self.periodic_event_schedules
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(PeriodicClockId)
    }

    pub fn periodic_schedule(&self, clock: PeriodicClockId) -> Option<&PeriodicEventSchedule> {
        self.periodic_event_schedules.get(clock.index())
    }
}

/// Typed identity of one periodic schedule in a [`SolveClockPartition`].
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(transparent)]
pub struct PeriodicClockId(u32);

impl PeriodicClockId {
    pub const fn index(self) -> usize {
        self.0 as usize
    }
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

/// Whether changing one typed discrete owner can invalidate continuous
/// integrator history.
///
/// `Preserve` is positive compiler evidence. The fail-closed default is
/// `Restart`, used whenever lowering cannot prove the dependency absent.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum IntegratorHistoryEffect {
    Preserve,
    #[default]
    Restart,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum DiscreteRowRole {
    /// A B.1c equation that participates in initialization and event iteration.
    Equation,
    /// An assignment that executes only on its owning event edge.
    EventAction,
    /// Runtime memory for detecting a condition edge.
    ConditionMemory,
}

#[derive(Clone, Debug, Serialize)]
pub struct PeriodicEventSchedule {
    lattice: rumoca_core::ClockLattice,
    anchor: rumoca_core::ClockPhaseAnchor,
}

impl PeriodicEventSchedule {
    /// Construct an exact rational schedule from finite second values.
    ///
    /// This is the external-boundary constructor for solver fixtures and
    /// decoded configuration. Compiler lowering should pass its already proven
    /// [`rumoca_core::ClockLattice`] through [`Self::new`].
    pub fn from_seconds(
        period: f64,
        phase: f64,
    ) -> Result<Self, rumoca_core::ClockLatticeErrorKind> {
        Self::new(rumoca_core::ClockLattice::from_seconds(period, phase)?)
    }

    pub fn new(
        lattice: rumoca_core::ClockLattice,
    ) -> Result<Self, rumoca_core::ClockLatticeErrorKind> {
        Self::from_schedule(rumoca_core::PeriodicClockSchedule::absolute(lattice)?)
    }

    pub fn from_schedule(
        schedule: rumoca_core::PeriodicClockSchedule,
    ) -> Result<Self, rumoca_core::ClockLatticeErrorKind> {
        let schedule = match schedule.anchor() {
            rumoca_core::ClockPhaseAnchor::Absolute => {
                rumoca_core::PeriodicClockSchedule::absolute(schedule.lattice())?
            }
            rumoca_core::ClockPhaseAnchor::SimulationStart => {
                rumoca_core::PeriodicClockSchedule::simulation_start_relative(schedule.lattice())?
            }
        };
        Ok(Self {
            lattice: schedule.lattice(),
            anchor: schedule.anchor(),
        })
    }

    /// The authoritative exact rational lattice (MLS §16.3/§16.5).
    pub const fn lattice(&self) -> rumoca_core::ClockLattice {
        self.lattice
    }

    pub const fn anchor(&self) -> rumoca_core::ClockPhaseAnchor {
        self.anchor
    }

    /// Resolve a simulation-start-relative phase for one ME instance.
    pub fn resolved_at(&self, start_time: f64) -> Result<Self, rumoca_core::ClockLatticeErrorKind> {
        let schedule = match self.anchor {
            rumoca_core::ClockPhaseAnchor::Absolute => {
                rumoca_core::PeriodicClockSchedule::absolute(self.lattice)?
            }
            rumoca_core::ClockPhaseAnchor::SimulationStart => {
                rumoca_core::PeriodicClockSchedule::simulation_start_relative(self.lattice)?
            }
        };
        Self::from_schedule(schedule.resolve_at(start_time)?)
    }

    pub fn period_seconds(&self) -> f64 {
        self.lattice.period_seconds()
    }

    pub fn phase_seconds(&self) -> f64 {
        self.lattice.phase_seconds()
    }

    /// Instant of tick `index` in seconds, computed exactly then rounded once.
    ///
    /// A schedule with no rational form or a tick outside the exact integer
    /// representation reports the original lattice error. Authoritative
    /// schedulers must not replace that failure with floating-point arithmetic.
    pub fn exact_tick_time_seconds(
        &self,
        index: impl Into<i128>,
    ) -> Result<f64, rumoca_core::ClockLatticeErrorKind> {
        self.lattice.tick_time_seconds(index)
    }
}

impl Default for PeriodicEventSchedule {
    fn default() -> Self {
        Self::new(
            rumoca_core::ClockLattice::from_interval_counter(1, 1)
                .expect("one-second clock lattice is valid"),
        )
        .expect("one-second periodic schedule is valid")
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct PeriodicEventScheduleWire {
    lattice: rumoca_core::ClockLattice,
    #[serde(default)]
    anchor: rumoca_core::ClockPhaseAnchor,
}

impl<'de> Deserialize<'de> for PeriodicEventSchedule {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let wire = PeriodicEventScheduleWire::deserialize(deserializer)?;
        let schedule = match wire.anchor {
            rumoca_core::ClockPhaseAnchor::Absolute => {
                rumoca_core::PeriodicClockSchedule::absolute(wire.lattice)
            }
            rumoca_core::ClockPhaseAnchor::SimulationStart => {
                rumoca_core::PeriodicClockSchedule::simulation_start_relative(wire.lattice)
            }
        }
        .map_err(serde::de::Error::custom)?;
        Self::from_schedule(schedule).map_err(serde::de::Error::custom)
    }
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
    /// Owning periodic clock for MLS §16 `previous()` history.
    ///
    /// `None` denotes ordinary Modelica `pre()` history and is committed after
    /// every event. A scheduled binding is committed only when this clock
    /// ticks, so unrelated roots and other clocks cannot advance its history.
    pub clock_schedule: Option<PeriodicEventSchedule>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum SolveVariableStorageRole {
    Parameter,
    Constant,
    ExternalInput,
    State,
    Algebraic,
    Output,
    DiscreteReal,
    DiscreteValue,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum SolveVariableValueKind {
    Real,
    Integer,
    Boolean,
    Enumeration,
    String,
}

#[derive(Clone, Copy, Debug, PartialEq, Deserialize, Serialize)]
pub struct SolveVariableStorageRun {
    pub base: ScalarSlot,
    pub scalar_count: usize,
    pub role: SolveVariableStorageRole,
    pub value_kind: SolveVariableValueKind,
}

/// Immutable typed declaration replayed independently of storage projection.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct SolveVariableDeclaration {
    role: SolveVariableStorageRole,
    value_kind: SolveVariableValueKind,
}

impl SolveVariableDeclaration {
    pub const fn new(role: SolveVariableStorageRole, value_kind: SolveVariableValueKind) -> Self {
        Self { role, value_kind }
    }

    pub const fn role(self) -> SolveVariableStorageRole {
        self.role
    }

    pub const fn value_kind(self) -> SolveVariableValueKind {
        self.value_kind
    }
}

impl SolveVariableStorageRun {
    pub fn event_iteration_kind(self) -> Option<EventIterationValueKind> {
        match (self.role, self.value_kind) {
            (SolveVariableStorageRole::DiscreteReal, SolveVariableValueKind::Real) => {
                Some(EventIterationValueKind::Real)
            }
            (SolveVariableStorageRole::DiscreteValue, SolveVariableValueKind::Integer) => {
                Some(EventIterationValueKind::Integer)
            }
            (SolveVariableStorageRole::DiscreteValue, SolveVariableValueKind::Boolean) => {
                Some(EventIterationValueKind::Boolean)
            }
            (SolveVariableStorageRole::DiscreteValue, SolveVariableValueKind::Enumeration) => {
                Some(EventIterationValueKind::Enumeration)
            }
            _ => None,
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveLayout {
    pub solver_maps: SolverNameIndexMaps,
    /// Dense DAE variable ordinal to its first Solve storage slot.
    ///
    /// Scalar `k` of a variable is stored at `base + k` in the same column.
    /// This is the canonical cross-phase coordinate map; display names are not
    /// used to recover compiler identity.
    pub variable_storage_runs: Vec<SolveVariableStorageRun>,
    /// Canonical typed DAE declarations in the same dense identity order.
    pub variable_declarations: Vec<SolveVariableDeclaration>,
    pub state_scalar_count: usize,
    pub algebraic_scalar_count: usize,
    pub output_scalar_count: usize,
    pub parameter_count: usize,
    pub compiled_parameter_len: usize,
    pub input_scalar_names: Vec<String>,
    pub discrete_real_scalar_names: Vec<String>,
    pub discrete_valued_scalar_names: Vec<String>,
    pub relation_memory_parameter_indices: Vec<usize>,
    pub initial_event_parameter_index: Option<usize>,
    /// P-slot that is true only while applying the final simulation event.
    pub terminal_event_parameter_index: Option<usize>,
    /// Hidden P-slot used by initialization residuals that contain
    /// `homotopy(actual, simplified)`.
    ///
    /// The initialization driver advances this value from zero to one. Models
    /// without homotopy expressions omit the slot entirely.
    pub initial_homotopy_parameter_index: Option<usize>,
    /// Snapshot bindings for `__pre__.*` parameters created by DAE-IR
    /// pre_lowering. At event entry the runtime copies each source slot into
    /// the corresponding dest P-slot before the event equations evaluate.
    pub pre_param_bindings: Vec<PreParamBinding>,
}

impl SolveLayout {
    pub fn solver_maps(&self) -> &SolverNameIndexMaps {
        &self.solver_maps
    }

    pub fn variable_scalar_slot(&self, variable: usize, scalar: usize) -> Option<ScalarSlot> {
        let run = self.variable_storage_runs.get(variable)?;
        if scalar >= run.scalar_count {
            return None;
        }
        match run.base {
            ScalarSlot::Y { index, .. } => index.checked_add(scalar).map(scalar_slot_y),
            ScalarSlot::P { index, .. } => index.checked_add(scalar).map(scalar_slot_p),
            ScalarSlot::Time | ScalarSlot::Constant(_) => None,
        }
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
    /// Positive nominal values aligned with solver `y` slots.
    pub solver_nominals: Vec<f64>,
    pub parameters: Vec<f64>,
    pub external_tables: ExternalTables,
    pub visible_names: Vec<String>,
    pub visible_value_rows: ScalarProgramBlock,
    pub variable_meta: Vec<SolveVariableMeta>,
}

impl SolveModel {
    /// Clone this compile-time model and resolve every periodic schedule at
    /// the FMI instance's simulation start instant.
    pub fn resolved_periodic_schedules_at(
        &self,
        start_time: f64,
    ) -> Result<Self, rumoca_core::ClockLatticeErrorKind> {
        let mut resolved = self.clone();
        for schedule in &mut resolved.problem.clocks.periodic_event_schedules {
            *schedule = schedule.resolved_at(start_time)?;
        }
        for binding in &mut resolved.problem.solve_layout.pre_param_bindings {
            if let Some(schedule) = &mut binding.clock_schedule {
                *schedule = schedule.resolved_at(start_time)?;
            }
        }
        Ok(resolved)
    }

    pub fn state_scalar_count(&self) -> usize {
        self.problem.solve_layout.state_scalar_count()
    }

    pub fn solver_scalar_count(&self) -> usize {
        self.problem.solve_layout.solver_scalar_count()
    }

    pub fn initialization_projection_unknowns(&self) -> &[ScalarSlot] {
        &self.problem.initialization.projection_unknowns
    }

    /// Return a finite positive scale for one solver variable.
    ///
    /// The declared nominal is the baseline. A larger start magnitude expands
    /// the scale so solver tolerances remain meaningful for large initial
    /// values and runtime start overrides.
    pub fn solver_variable_scale(&self, index: usize) -> f64 {
        let nominal = self
            .solver_nominals
            .get(index)
            .copied()
            .filter(|value| value.is_finite() && *value > 0.0)
            .unwrap_or(1.0);
        let start_magnitude = self
            .initial_y
            .get(index)
            .copied()
            .filter(|value| value.is_finite())
            .map_or(0.0, f64::abs);
        nominal.max(start_magnitude)
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
