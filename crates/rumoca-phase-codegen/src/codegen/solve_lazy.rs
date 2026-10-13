//! Lazy minijinja [`Object`] wrappers over the Solve IR.
//!
//! `Value::from_serialize` of the `SolveProblem` materializes every `LinearOp`
//! into ~2 `IndexMap`s each (~64x the JSON size) — gigabytes for a 150k-op
//! model. These wrappers expose the IR to templates *lazily*: structural fields
//! are produced on demand and op lists materialize one op at a time during
//! iteration, so peak memory is O(one program) instead of O(whole problem).
//! Targets that never access a field (e.g. c-ode never touches
//! `solve.continuous`) pay nothing for it. The Rust render functions can also
//! `downcast_object_ref` to [`SolveProgramsObject`] / [`SolveOpListObject`] to
//! iterate the typed ops directly with zero materialization.

use std::fmt;
use std::sync::Arc;

use indexmap::IndexMap;
use minijinja::Value;
use minijinja::value::{Enumerator, Object, ObjectRepr};
use rumoca_ir_solve as solve;

use crate::errors::CodegenError;

// ── Kernel handles ───────────────────────────────────────────────────────────

/// One correlated, read-only owner for everything a lazy Solve renderer reads.
///
/// Keeping the problem and artifacts in one handle makes it impossible for an
/// internal caller to pair an FMI problem with artifacts from another model.
/// Standalone renderers retain their existing pair of separately lowered
/// values, while the FMI variant retains the complete correlated codegen view
/// in its proved C event profile.
#[derive(Clone)]
pub(super) enum SolveRenderHandle {
    Standalone {
        problem: Arc<solve::SolveProblem>,
        artifacts: Arc<solve::SolveArtifacts>,
        /// The model's pure-call owners, when the caller lowered them with
        /// the problem, so scalar targets can inline `PureCall` ops.
        pure_calls: Option<Arc<solve::SolvePureCallTable>>,
    },
    Fmi(Arc<solve::fmi::FmiCCodegenView>),
    Model(Arc<solve::SolveModel>),
}

impl SolveRenderHandle {
    pub(super) fn standalone(
        problem: Arc<solve::SolveProblem>,
        artifacts: Arc<solve::SolveArtifacts>,
    ) -> Self {
        Self::Standalone {
            problem,
            artifacts,
            pure_calls: None,
        }
    }

    pub(super) fn standalone_with_calls(
        problem: Arc<solve::SolveProblem>,
        artifacts: Arc<solve::SolveArtifacts>,
        pure_calls: Arc<solve::SolvePureCallTable>,
    ) -> Self {
        Self::Standalone {
            problem,
            artifacts,
            pure_calls: Some(pure_calls),
        }
    }

    /// The pure-call owners a standalone or retained-model scalar target inlines; an FMI
    /// component renders its owners as typed functions instead.
    pub(super) fn inlined_pure_calls(&self) -> Option<&solve::SolvePureCallTable> {
        match self {
            Self::Standalone { pure_calls, .. } => pure_calls.as_deref(),
            Self::Model(model) => Some(&model.pure_calls),
            Self::Fmi(_) => None,
        }
    }

    pub(super) fn fmi(component: solve::fmi::FmiCCodegenView) -> Self {
        Self::Fmi(Arc::new(component))
    }

    pub(super) fn problem(&self) -> &solve::SolveProblem {
        match self {
            Self::Standalone { problem, .. } => problem,
            Self::Fmi(component) => component.problem(),
            Self::Model(model) => &model.problem,
        }
    }

    pub(super) fn artifacts(&self) -> &solve::SolveArtifacts {
        match self {
            Self::Standalone { artifacts, .. } => artifacts,
            Self::Fmi(component) => component.artifacts(),
            Self::Model(model) => &model.artifacts,
        }
    }

    /// The `fmi` render entry: nothing for a standalone kernel, and for a
    /// component the C profile it was already proved to satisfy.
    ///
    /// That view is the only whole-inventory encoding, so a template cannot be
    /// handed a component outside its checked event profile. Component
    /// construction already guarantees the inventory shape; the renderer
    /// constructor checks its remaining capability domain before this handle
    /// exists, so there is no case to reject here.
    pub(super) fn fmi_value(&self) -> Value {
        match self {
            Self::Standalone { .. } | Self::Model(_) => Value::default(),
            Self::Fmi(component) => Value::from_serialize(component.as_ref()),
        }
    }
}

/// Runtime values of a complete lowered model. Large program inventories keep
/// the same lazy program views as the canonical Solve problem.
pub(super) fn model_value(model: Arc<solve::SolveModel>) -> Result<Value, CodegenError> {
    let visible_rows = Arc::new(model.visible_value_rows.clone());
    let visible = scalar_program_block_value(visible_rows.clone());
    let visible_plan = Value::from_object(super::scalar_program_plan::ScalarProgramPlan::new(
        visible_rows,
    )?);
    Ok(lazy_map(
        &[
            "initial_y",
            "parameters",
            "solver_nominals",
            "visible_names",
            "visible_value_rows",
            "visible_value_plan",
            "variable_meta",
            "external_tables",
            "pure_calls",
            "pure_call_directional_owners",
        ],
        move |key| match key {
            "initial_y" => Some(Value::from_serialize(&model.initial_y)),
            "parameters" => Some(Value::from_serialize(&model.parameters)),
            "solver_nominals" => Some(Value::from_serialize(&model.solver_nominals)),
            "visible_names" => Some(Value::from_serialize(&model.visible_names)),
            "visible_value_rows" => Some(visible.clone()),
            "visible_value_plan" => Some(visible_plan.clone()),
            "variable_meta" => Some(Value::from_serialize(&model.variable_meta)),
            "external_tables" => Some(Value::from_serialize(&model.external_tables)),
            "pure_calls" => Some(Value::from_serialize(&model.pure_calls)),
            "pure_call_directional_owners" => Some(
                super::pure_call_families::directional_owners_value(&model.pure_calls),
            ),
            _ => None,
        },
    ))
}

// ── Generic lazy Map / Seq ───────────────────────────────────────────────────

type MapGet = Arc<dyn Fn(&str) -> Option<Value> + Send + Sync>;

struct LazyMap {
    keys: &'static [&'static str],
    get: MapGet,
}

impl fmt::Debug for LazyMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("LazyMap")
    }
}

impl Object for LazyMap {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Map
    }
    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Iter(Box::new(self.keys.iter().copied().map(Value::from)))
    }
    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        (self.get)(key.as_str()?)
    }
}

fn lazy_map(
    keys: &'static [&'static str],
    get: impl Fn(&str) -> Option<Value> + Send + Sync + 'static,
) -> Value {
    Value::from_object(LazyMap {
        keys,
        get: Arc::new(get),
    })
}

type SeqGet = Arc<dyn Fn(usize) -> Value + Send + Sync>;

struct LazySeq {
    len: usize,
    get: SeqGet,
}

impl fmt::Debug for LazySeq {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("LazySeq")
    }
}

impl Object for LazySeq {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Seq
    }
    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Seq(self.len)
    }
    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let idx = key.as_usize()?;
        (idx < self.len).then(|| (self.get)(idx))
    }
}

fn lazy_seq(len: usize, get: impl Fn(usize) -> Value + Send + Sync + 'static) -> Value {
    Value::from_object(LazySeq {
        len,
        get: Arc::new(get),
    })
}

// ── Op-list leaves (downcast-able by the render functions) ───────────────────

/// The `programs` list of a `ScalarProgramBlock` (a Seq of op-lists). Render
/// functions downcast this to iterate the typed programs/ops directly.
#[derive(Debug)]
pub(super) struct SolveProgramsObject {
    pub(super) block: Arc<solve::ScalarProgramBlock>,
}

impl Object for SolveProgramsObject {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Seq
    }
    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Seq(self.block.programs().len())
    }
    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let idx = key.as_usize()?;
        (idx < self.block.programs().len()).then(|| {
            Value::from_object(SolveOpListObject {
                ops: OpListSource::Program(self.block.clone(), idx),
            })
        })
    }
}

/// A single op list — either a program inside a `ScalarProgramBlock` or a raw
/// `Vec<LinearOp>` (matmul/linsolve operand ops). Materializes one op at a time.
#[derive(Debug)]
pub(super) struct SolveOpListObject {
    ops: OpListSource,
}

#[derive(Debug)]
enum OpListSource {
    Program(Arc<solve::ScalarProgramBlock>, usize),
    Raw(Arc<Vec<solve::LinearOp>>),
}

impl SolveOpListObject {
    fn ops(&self) -> &[solve::LinearOp] {
        match &self.ops {
            OpListSource::Program(block, idx) => &block.programs()[*idx],
            OpListSource::Raw(ops) => ops,
        }
    }
}

impl Object for SolveOpListObject {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Seq
    }
    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Seq(self.ops().len())
    }
    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let idx = key.as_usize()?;
        self.ops().get(idx).map(Value::from_serialize)
    }
}

fn raw_op_list_value(ops: Arc<Vec<solve::LinearOp>>) -> Value {
    Value::from_object(SolveOpListObject {
        ops: OpListSource::Raw(ops),
    })
}

// ── IR hierarchy ─────────────────────────────────────────────────────────────

fn scalar_program_block_value(block: Arc<solve::ScalarProgramBlock>) -> Value {
    lazy_map(
        &["programs", "program_spans", "output_indices"],
        move |k| match k {
            "programs" => Some(Value::from_object(SolveProgramsObject {
                block: block.clone(),
            })),
            "program_spans" => Some(Value::from_serialize(block.program_spans())),
            "output_indices" => Some(Value::from_serialize(block.output_indices())),
            _ => None,
        },
    )
}

pub(in crate::codegen) fn compute_node_value(
    node: Arc<solve::ComputeNode>,
) -> Result<Value, CodegenError> {
    // Serialized as a tagged enum: { "MatMul": {...} } / { "ScalarPrograms": ... }
    // / { "LinSolve": {...} }. Only the active variant key is present.
    Ok(match node.as_ref() {
        solve::ComputeNode::ScalarPrograms(block) => {
            let block = Arc::new(block.clone());
            lazy_map(&["ScalarPrograms"], move |k| {
                (k == "ScalarPrograms").then(|| scalar_program_block_value(block.clone()))
            })
        }
        solve::ComputeNode::MatMul { .. } => lazy_map(&["MatMul"], move |k| {
            (k == "MatMul").then(|| matmul_value(node.clone()))
        }),
        solve::ComputeNode::LinSolve { .. } => lazy_map(&["LinSolve"], move |k| {
            (k == "LinSolve").then(|| linsolve_value(node.clone()))
        }),
        // An affine stencil renders as its scalarized expansion — matching how
        // `c_renderable_derivative_nodes` lowers stencils for the C templates.
        solve::ComputeNode::Map { .. } | solve::ComputeNode::AffineStencil { .. } => {
            let scalar = rumoca_eval_solve::to_scalar_program_block(&solve::ComputeBlock {
                nodes: vec![node.as_ref().clone()],
            })?;
            let scalar = Arc::new(scalar);
            lazy_map(&["ScalarPrograms"], move |k| {
                (k == "ScalarPrograms").then(|| scalar_program_block_value(scalar.clone()))
            })
        }
    })
}

fn matmul_value(node: Arc<solve::ComputeNode>) -> Value {
    lazy_map(
        &[
            "lhs_ops",
            "lhs_start",
            "rhs_ops",
            "rhs_start",
            "m",
            "k",
            "n",
            "lhs_pattern",
            "rhs_pattern",
            "lhs_pattern_kind",
            "lhs_pattern_nonzeros",
            "metadata",
            "span",
        ],
        move |k| {
            let solve::ComputeNode::MatMul {
                lhs_ops,
                lhs_start,
                rhs_ops,
                rhs_start,
                m,
                k: kk,
                n,
                lhs_pattern,
                rhs_pattern,
                metadata,
                span,
            } = node.as_ref()
            else {
                return None;
            };
            match k {
                "lhs_ops" => Some(raw_op_list_value(Arc::new(lhs_ops.clone()))),
                "rhs_ops" => Some(raw_op_list_value(Arc::new(rhs_ops.clone()))),
                "lhs_start" => Some(Value::from(*lhs_start)),
                "rhs_start" => Some(Value::from(*rhs_start)),
                "m" => Some(Value::from(*m)),
                "k" => Some(Value::from(*kk)),
                "n" => Some(Value::from(*n)),
                "lhs_pattern" => Some(Value::from_serialize(lhs_pattern)),
                "rhs_pattern" => Some(Value::from_serialize(rhs_pattern)),
                "lhs_pattern_kind" => Some(Value::from(pattern_kind(lhs_pattern))),
                "lhs_pattern_nonzeros" => {
                    Some(Value::from_serialize(pattern_nonzeros(lhs_pattern)))
                }
                "metadata" => Some(Value::from_serialize(metadata)),
                "span" => Some(Value::from_serialize(span)),
                _ => None,
            }
        },
    )
}

fn pattern_kind(pattern: &solve::StructuralPattern) -> &'static str {
    match pattern.view() {
        solve::StructuralPatternView::Empty => "empty",
        solve::StructuralPatternView::Full => "full",
        solve::StructuralPatternView::Diagonal => "diagonal",
        solve::StructuralPatternView::Banded { .. } => "banded",
        solve::StructuralPatternView::Csr { .. } => "csr",
        solve::StructuralPatternView::Affine { .. } => "affine",
        solve::StructuralPatternView::Stacked { .. } => "stacked",
    }
}

fn pattern_nonzeros(pattern: &solve::StructuralPattern) -> Vec<(u32, u32)> {
    let mut entries = Vec::with_capacity(pattern.nonzero_upper_bound().unwrap_or(0));
    for (column, rows) in pattern.column_rows().into_iter().enumerate() {
        entries.extend(rows.into_iter().map(|row| (row as u32, column as u32)));
    }
    entries.sort_unstable();
    entries
}

fn linsolve_value(node: Arc<solve::ComputeNode>) -> Value {
    lazy_map(
        &[
            "setup_ops",
            "matrix_start",
            "rhs_start",
            "n",
            "next_reg",
            "matrix_pattern",
            "metadata",
            "span",
        ],
        move |k| {
            let solve::ComputeNode::LinSolve {
                setup_ops,
                matrix_start,
                rhs_start,
                n,
                next_reg,
                matrix_pattern,
                metadata,
                span,
            } = node.as_ref()
            else {
                return None;
            };
            match k {
                "setup_ops" => Some(raw_op_list_value(Arc::new(setup_ops.clone()))),
                "matrix_start" => Some(Value::from(*matrix_start)),
                "rhs_start" => Some(Value::from(*rhs_start)),
                "n" => Some(Value::from(*n)),
                "next_reg" => Some(Value::from(*next_reg)),
                "matrix_pattern" => Some(Value::from_serialize(matrix_pattern)),
                "metadata" => Some(Value::from_serialize(metadata)),
                "span" => Some(Value::from_serialize(span)),
                _ => None,
            }
        },
    )
}

/// Lazy view of a `ComputeBlock` exposing `nodes` (structured) plus the derived
/// `scalar_programs` fallback and counts — matching `solve_template_blocks_value`.
pub(super) fn compute_block_value(block: Arc<solve::ComputeBlock>) -> Result<Value, CodegenError> {
    super::lazy_scalar_projection::validate(&block)?;
    let (scalar_programs, scalar_plan) = super::lazy_scalar_projection::views(block.clone());
    let output_count = block.len()?;
    let uses_linear_solve = block.uses_linear_solve_component();
    let nodes = nodes_value(block.clone())?;
    Ok(lazy_map(
        &[
            "nodes",
            "scalar_plan",
            "scalar_programs",
            "output_count",
            "tensor_node_count",
            "scalar_programs_use_linear_solve_component",
        ],
        move |k| match k {
            "nodes" => Some(nodes.clone()),
            "scalar_plan" => Some(scalar_plan.clone()),
            "scalar_programs" => Some(scalar_programs.clone()),
            "output_count" => Some(Value::from(output_count)),
            "tensor_node_count" => Some(Value::from(block.tensor_node_count())),
            "scalar_programs_use_linear_solve_component" => Some(Value::from(uses_linear_solve)),
            _ => None,
        },
    ))
}

fn continuous_value(handle: SolveRenderHandle) -> Result<Value, CodegenError> {
    let problem = handle.problem();
    let exact_assignments = super::me_projection::exact_algebraic_assignments_value(problem)?;
    let implicit_rhs = compute_block_value(Arc::new(problem.continuous.implicit_rhs.clone()))?;
    let derivative_rhs = compute_block_value(Arc::new(problem.continuous.derivative_rhs.clone()))?;
    let residual = compute_block_value(Arc::new(problem.continuous.residual.clone()))?;
    let manifold = compute_block_value(Arc::new(problem.continuous.manifold_residual.clone()))?;
    let algebraic_tearing = algebraic_tearing_value(problem)?;
    Ok(lazy_map(
        &[
            "implicit_rhs",
            "implicit_row_targets",
            "exact_algebraic_assignments",
            "algebraic_projection_plan",
            "algebraic_tearing",
            "residual",
            "derivative_rhs",
            "manifold_residual",
            "manifold_projection_plan",
            "reduced_chart_set",
            "refresh_owners",
        ],
        move |k| {
            let c = &handle.problem().continuous;
            match k {
                "algebraic_tearing" => Some(algebraic_tearing.clone()),
                "implicit_rhs" => Some(implicit_rhs.clone()),
                "exact_algebraic_assignments" => Some(exact_assignments.clone()),
                "derivative_rhs" => Some(derivative_rhs.clone()),
                "residual" => Some(residual.clone()),
                "manifold_residual" => Some(manifold.clone()),
                "manifold_projection_plan" => {
                    Some(Value::from_serialize(&c.manifold_projection_plan))
                }
                "reduced_chart_set" => Some(Value::from_serialize(&c.reduced_chart_set)),
                "refresh_owners" => Some(Value::from_serialize(&c.refresh_owners)),
                "implicit_row_targets" => Some(Value::from_serialize(&c.implicit_row_targets)),
                "algebraic_projection_plan" => {
                    Some(Value::from_serialize(&c.algebraic_projection_plan))
                }
                _ => None,
            }
        },
    ))
}

/// Reuse the evaluator's certified isolators and causal order; no target infers assignments.
fn algebraic_tearing_value(problem: &solve::SolveProblem) -> Result<Value, CodegenError> {
    let prepared = rumoca_eval_solve::PreparedScalarProgramBlock::from_compute_block(
        &problem.continuous.implicit_rhs,
    )
    .map_err(|error| CodegenError::template(error.to_string()))?;
    let blocks = problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .iter()
        .map(|block| torn_block_value(&prepared, block))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Value::from(blocks))
}

fn torn_block_value(
    prepared: &rumoca_eval_solve::PreparedScalarProgramBlock,
    block: &solve::AlgebraicProjectionBlock,
) -> Result<Value, CodegenError> {
    let Some(tearing) = &block.tearing else {
        return Ok(Value::from(()));
    };
    let causal = tearing
        .causal_steps
        .iter()
        .map(|step| (step.row, step.y_index))
        .collect::<Vec<_>>();
    let Some(composite) = prepared
        .prepare_torn_sweep(&causal, &tearing.residual_rows)
        .and_then(|sweep| prepared.torn_sweep_composite(&sweep))
    else {
        return Ok(Value::from(()));
    };
    let Some(outputs) = composite
        .residual_outputs
        .iter()
        .copied()
        .collect::<Option<Vec<_>>>()
    else {
        return Ok(Value::from(()));
    };
    let spans = causal
        .iter()
        .map(|(row, _)| {
            let (program, _) = prepared.row_output_position(*row)?;
            prepared.block().program_span(program)
        })
        .collect::<Option<Vec<_>>>()
        .ok_or_else(|| CodegenError::template("Torn causal row has no source provenance"))?;
    let assignments =
        solve::ScalarProgramBlock::with_program_spans(composite.assignment_rows, spans)
            .map_err(|error| CodegenError::template(error.to_string()))?;
    let assignments = super::scalar_program_plan::ScalarProgramPlan::new(Arc::new(assignments))?;
    let residuals =
        super::scalar_program_plan::ScalarProgramPlan::new(Arc::new(composite.residual_block))?;
    Ok(minijinja::context! {
        tear_y_indices => tearing.tear_y_indices,
        assignment_targets => composite.assignment_targets,
        residual_outputs => outputs,
        assignment_plan => Value::from_object(assignments),
        residual_plan => Value::from_object(residuals),
    })
}

/// Target-local explicit algebraic execution profile.
///
/// The profile consumes only checked Solve inventories and issued assignment
/// programs. Assignment-shape derivation and dependency discovery already ran
/// when the Solve owner was constructed or replayed from wire.
#[must_use]
pub fn explicit_algebraic_assignment_complete(problem: &solve::SolveProblem) -> bool {
    explicit_algebraic_assignment_complete_with_calls(problem, None)
}

/// [`explicit_algebraic_assignment_complete`] for a target that inlines the
/// model's pure calls (`pure_calls`, from the same Solve lowering).
#[must_use]
pub fn explicit_algebraic_assignment_complete_with_calls(
    problem: &solve::SolveProblem,
    pure_calls: Option<&solve::SolvePureCallTable>,
) -> bool {
    algebraic_assignment_ownership_complete(problem)
        && materialized_assignment_programs(problem, pure_calls).is_some()
}

fn algebraic_assignment_ownership_complete(problem: &solve::SolveProblem) -> bool {
    if problem.validate().is_err() {
        return false;
    }
    let continuous = &problem.continuous;
    let state_count = problem.solve_layout.state_scalar_count();
    let Some(required_algebraic_end) =
        state_count.checked_add(problem.solve_layout.algebraic_scalar_count())
    else {
        return false;
    };
    let expected_targets = continuous
        .algebraic_projection_plan
        .blocks
        .iter()
        .flat_map(|block| block.y_indices.iter().copied())
        .collect::<std::collections::BTreeSet<_>>();
    if !(state_count..required_algebraic_end).all(|target| expected_targets.contains(&target)) {
        return false;
    }
    if expected_targets.is_empty() {
        return continuous.implicit_rhs.is_empty();
    }
    let owners = &continuous.refresh_owners;
    if owners.algebraic().simultaneous_plan != continuous.algebraic_projection_plan
        || owners.algebraic().simultaneous_block_indices
            != (0..continuous.algebraic_projection_plan.blocks.len()).collect::<Vec<_>>()
        || !owners.algebraic_exact_assignment_stages_cover()
    {
        return false;
    }
    let Some(assignments) = ordered_exact_algebraic_assignments(owners) else {
        return false;
    };
    let mut assigned = std::collections::BTreeSet::new();
    let mut settled = solve::SettledIntervals::default();
    let mut rows = std::collections::BTreeSet::new();
    for (program, position) in assignments {
        let Some(&target) = program.target_indices().get(position) else {
            return false;
        };
        let Some(shape) = program.assignment_shapes().get(position) else {
            return false;
        };
        if target != shape.target_y_index()
            || !expected_targets.contains(&target)
            || !supported_explicit_assignment_shape(shape)
            || program
                .assignment_y_dependencies(position)
                .is_none_or(|dependencies| {
                    dependencies.reads_unresolved(
                        target,
                        &expected_targets,
                        |dependency| assigned.contains(&dependency),
                        &mut settled,
                    )
                })
            || !assigned.insert(target)
        {
            return false;
        }
        rows.extend(program.row_owners().iter().copied());
    }
    assigned == expected_targets && rows.len() == continuous.algebraic_projection_plan.blocks.len()
}

/// Every ordered exact assignment program, materialized and lowered to scalar
/// ops, with the `y` slots it assigns. `None` when a program cannot be
/// materialized or contains an op no scalar template renders.
/// One materialized assignment program: its scalar ops, its provenance, and
/// the `y` slots it assigns (in its store order).
type MaterializedAssignment = (Vec<solve::LinearOp>, rumoca_core::Span, Vec<usize>);

fn materialized_assignment_programs(
    problem: &solve::SolveProblem,
    pure_calls: Option<&solve::SolvePureCallTable>,
) -> Option<Vec<MaterializedAssignment>> {
    let assignments = ordered_exact_algebraic_assignments(&problem.continuous.refresh_owners)?;
    let mut programs = Vec::new();
    let mut previous: Option<&solve::ExactRefreshAssignmentProgram> = None;
    for (program, _) in assignments {
        // One owner contributes one consecutive entry per assigned target;
        // materialize it once.
        if previous.is_some_and(|seen| std::ptr::eq(seen, program)) {
            continue;
        }
        previous = Some(program);
        let block = program
            .final_scalar_program(&problem.continuous.implicit_rhs)
            .ok()?;
        let [ops] = block.programs() else {
            return None;
        };
        let span = block.program_span(0)?;
        let lowered = rumoca_eval_solve::lower_scalar_ops(ops, pure_calls)?;
        programs.push((lowered, span, program.target_indices().to_vec()));
    }
    Some(programs)
}

/// `block` with every scalar-program node's tensor register ops lowered to
/// scalar ops; other nodes (MatMul, LinSolve, maps) are left for the
/// template's scalar view. `None` when a program cannot be lowered.
pub(super) fn scalar_lowered_block(
    block: &solve::ComputeBlock,
    pure_calls: Option<&solve::SolvePureCallTable>,
) -> Result<Option<solve::ComputeBlock>, CodegenError> {
    block
        .nodes
        .iter()
        .map(|node| scalar_lowered_node(node, pure_calls))
        .collect::<Option<Vec<_>>>()
        .map(|nodes| {
            nodes
                .into_iter()
                .collect::<Result<Vec<_>, _>>()
                .map(|nodes| solve::ComputeBlock { nodes })
        })
        .transpose()
}

/// One node with its scalar programs lowered; `None` when a program cannot be.
fn scalar_lowered_node(
    node: &solve::ComputeNode,
    pure_calls: Option<&solve::SolvePureCallTable>,
) -> Option<Result<solve::ComputeNode, CodegenError>> {
    let solve::ComputeNode::ScalarPrograms(programs) = node else {
        return Some(Ok(node.clone()));
    };
    let lowered = programs
        .programs()
        .iter()
        .map(|program| rumoca_eval_solve::lower_scalar_ops(program, pure_calls))
        .collect::<Option<Vec<_>>>()?;
    Some(
        solve::ScalarProgramBlock::with_output_indices(
            lowered,
            programs.program_spans().to_vec(),
            programs.output_indices().to_vec(),
        )
        .map(solve::ComputeNode::ScalarPrograms)
        .map_err(|error| CodegenError::template(error.to_string())),
    )
}

/// Whether the explicit initialization's ordered parameter and state updates
/// each assign one output and admit the final scalar projection, including
/// the model's pure-call owners.
#[must_use]
pub fn explicit_initialization_renderable_with_calls(
    problem: &solve::SolveProblem,
    pure_calls: Option<&solve::SolvePureCallTable>,
) -> bool {
    solve::solve_initialization_is_explicit(problem)
        && problem
            .initialization
            .update_rhs()
            .programs()
            .iter()
            .all(|program| {
                solve::ScalarProgramBlock::program_output_count(program) == 1
                    && rumoca_eval_solve::lower_scalar_ops(program, pure_calls).is_some()
            })
}

/// The algebraic refresh as one scalar block a pure-RHS template can execute.
///
/// When every algebraic slot is owned by an issued exact assignment
/// ([`explicit_algebraic_assignment_complete`]), the assignments are
/// materialized in their checked construction order. Each block output is
/// paired with the `y` slot it assigns, so a template that renders the block
/// before `derivative_rhs` fills every algebraic slot the derivative rows read.
/// `None` when the refresh is not a complete explicit assignment schedule;
/// such a model is refused at the target capability gate.
pub(super) fn explicit_algebraic_assignment_block(
    problem: &solve::SolveProblem,
    pure_calls: Option<&solve::SolvePureCallTable>,
) -> Result<Option<solve::ComputeBlock>, CodegenError> {
    if problem.solve_layout.algebraic_scalar_count() == 0
        || !algebraic_assignment_ownership_complete(problem)
    {
        return Ok(unavailable_algebraic_assignment_block());
    }
    materialized_assignment_programs(problem, pure_calls)
        .map(materialize_output_assignments)
        .transpose()
}

fn unavailable_algebraic_assignment_block() -> Option<solve::ComputeBlock> {
    None
}

pub(super) struct ScalarInitializationBlocks {
    pub parameters: Option<solve::ComputeBlock>,
    pub states: Option<solve::ComputeBlock>,
}

/// Prepare each explicit update once, then partition its checked program into
/// parameter or state outputs while retaining construction order and provenance.
/// An unsupported update leaves both scalar views absent.
pub(super) fn initialization_blocks(
    problem: &solve::SolveProblem,
    pure_calls: Option<&solve::SolvePureCallTable>,
) -> Result<ScalarInitializationBlocks, CodegenError> {
    let absent = || ScalarInitializationBlocks {
        parameters: None,
        states: None,
    };
    if !solve::solve_initialization_is_explicit(problem) {
        return Ok(absent());
    }
    let initialization = &problem.initialization;
    let update = initialization.update_rhs();
    let mut parameters = Vec::new();
    let mut states = Vec::new();
    for (index, (program, target)) in update
        .programs()
        .iter()
        .zip(initialization.update_targets())
        .enumerate()
    {
        if solve::ScalarProgramBlock::program_output_count(program) != 1 {
            return Ok(absent());
        }
        let Some(lowered) = rumoca_eval_solve::lower_scalar_ops(program, pure_calls) else {
            return Ok(absent());
        };
        let span = update.program_span(index).ok_or_else(|| {
            CodegenError::template("initialization program has no provenance".to_string())
        })?;
        match target {
            solve::ScalarSlot::P { index, .. } => parameters.push((lowered, span, vec![*index])),
            solve::ScalarSlot::Y { index, .. } => states.push((lowered, span, vec![*index])),
            _ => return Ok(absent()),
        }
    }
    Ok(ScalarInitializationBlocks {
        parameters: (!parameters.is_empty())
            .then(|| materialize_output_assignments(parameters))
            .transpose()?,
        states: (!states.is_empty())
            .then(|| materialize_output_assignments(states))
            .transpose()?,
    })
}

/// Build one scalar block whose outputs are the assigned slot indices.
fn materialize_output_assignments(
    programs: Vec<MaterializedAssignment>,
) -> Result<solve::ComputeBlock, CodegenError> {
    let mut ops = Vec::new();
    let mut spans = Vec::new();
    let mut output_indices = Vec::new();
    for (program, span, targets) in programs {
        ops.push(program);
        spans.push(span);
        output_indices.extend(targets);
    }
    let block = solve::ScalarProgramBlock::with_output_indices(ops, spans, output_indices)
        .map_err(|error| CodegenError::template(error.to_string()))?;
    Ok(solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(block)],
    })
}

/// Whether an issued exact assignment can run as straight-line scalar code.
///
/// Shapes materialize through the checked exact-assignment builder
/// (`ExactRefreshAssignmentProgram::final_scalar_program`). Shapes that divide
/// by a runtime coefficient (`Affine` with a coefficient register,
/// `Reciprocal`) are materialized guarded: a zero or non-finite coefficient
/// poisons the value to NaN, so a degenerate point fails loudly in the host
/// integrator instead of producing a silently wrong derivative. Only a
/// construction-constant coefficient must be a usable divisor.
///
/// `TensorAffine` materializes into tensor register ops (`TensorFill`, ...);
/// those are lowered to scalar ops (`rumoca_eval_solve::lower_scalar_ops`), and a program that
/// cannot be lowered fails `materialized_assignment_programs`.
fn supported_explicit_assignment_shape(shape: &solve::TargetAssignmentShape) -> bool {
    match shape {
        solve::TargetAssignmentShape::Zero { .. }
        | solve::TargetAssignmentShape::Direct { .. }
        | solve::TargetAssignmentShape::Reciprocal { .. }
        | solve::TargetAssignmentShape::TensorAffine { .. } => true,
        solve::TargetAssignmentShape::Affine {
            coefficient_reg,
            coefficient_scale,
            ..
        } => {
            coefficient_reg.is_some()
                || (coefficient_scale.is_finite() && *coefficient_scale != 0.0)
        }
        solve::TargetAssignmentShape::Additive { coefficient, .. } => {
            coefficient.is_finite() && *coefficient != 0.0
        }
    }
}

fn ordered_exact_algebraic_assignments(
    owners: &solve::ContinuousRefreshOwners,
) -> Option<Vec<(&solve::ExactRefreshAssignmentProgram, usize)>> {
    let mut assignments = Vec::new();
    for stage in &owners.algebraic().value_stages {
        let (static_sequence, dynamic_sequence) = match stage {
            solve::RefreshStage::CausalSeedSweep { .. } => continue,
            solve::RefreshStage::ExactAssignments {
                static_sequence,
                dynamic_sequence,
                ..
            } => (*static_sequence, *dynamic_sequence),
            solve::RefreshStage::ProjectionBlock { .. } => return None,
        };
        for sequence in [static_sequence, dynamic_sequence] {
            let Some(schedule) = owners.exact_assignment_schedule(sequence) else {
                continue;
            };
            for program_id in schedule.program_ids() {
                let program = owners.exact_assignment_program(*program_id)?;
                assignments
                    .extend((0..program.target_indices().len()).map(|index| (program, index)));
            }
        }
    }
    Some(assignments)
}

// SPEC_0021: Exception - exhaustive lazy routing of the discrete render fields.
#[allow(clippy::too_many_lines)]
fn discrete_value(handle: SolveRenderHandle) -> Result<Value, CodegenError> {
    let scalar =
        super::discrete_render_view::DiscreteRenderView::checked(&handle.problem().discrete)?;
    let rhs = scalar_program_block_value(Arc::new(scalar.rhs));
    let rhs_plan = Value::from_object(super::scalar_program_plan::ScalarProgramPlan::new(
        Arc::new(handle.problem().discrete.rhs.clone()),
    )?);
    let runtime_assignment_plan =
        Value::from_object(super::scalar_program_plan::ScalarProgramPlan::new(
            Arc::new(handle.problem().discrete.runtime_assignment_rhs.clone()),
        )?);
    let post_commit_assignment_plan =
        Value::from_object(super::scalar_program_plan::ScalarProgramPlan::new(
            Arc::new(handle.problem().discrete.post_commit_assignment_rhs.clone()),
        )?);
    let guarded_assignment_plan = guarded_assignment_plan(&handle.problem().discrete)?;
    let structured_rhs =
        compute_block_value(Arc::new(handle.problem().discrete.structured_rhs.clone()))?;
    let structured_assignments = handle
        .problem()
        .discrete
        .structured_updates
        .iter()
        .enumerate()
        .map(|(index, _)| {
            handle
                .problem()
                .discrete
                .structured_assignments(index)
                .map_err(|error| CodegenError::template(error.to_string()))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let structured_assignments = Value::from_serialize(&structured_assignments);
    let clock_intermediate_plan = Value::from_object(
        super::scalar_program_plan::ScalarProgramPlan::new(Arc::new(
            handle
                .problem()
                .discrete
                .clock_partition_intermediates
                .clone(),
        ))?,
    );
    let targets = scalar.targets;
    let pre_modes = scalar.pre_modes;
    let observation_refresh = scalar.observation_refresh;
    Ok(lazy_map(
        &[
            "runtime_assignment_rhs",
            "runtime_assignment_plan",
            "runtime_assignment_targets",
            "runtime_assignment_roles",
            "post_commit_assignment_rhs",
            "post_commit_assignment_plan",
            "guarded_assignment_plan",
            "guarded_assignment_targets",
            "event_iteration_plan",
            "event_transactions",
            "clock_partition_order",
            "clock_owners",
            "row_roles",
            "structured_row_roles",
            "guarded_assignment_clock_owners",
            "structured_update_count",
            "structured_rhs",
            "structured_assignments",
            "structured_clock_owners",
            "clock_partition_intermediate_count",
            "clock_partition_intermediate_plan",
            "clock_partition_intermediate_targets",
            "clock_partition_intermediate_clocks",
            "post_commit_assignment_targets",
            "post_commit_assignment_runtime_rows",
            "rhs",
            "rhs_plan",
            "update_targets",
            "pre_modes",
            "observation_refresh",
        ],
        move |k| {
            let d = &handle.problem().discrete;
            match k {
                "rhs" => Some(rhs.clone()),
                "rhs_plan" => Some(rhs_plan.clone()),
                "runtime_assignment_plan" => Some(runtime_assignment_plan.clone()),
                "post_commit_assignment_plan" => Some(post_commit_assignment_plan.clone()),
                "guarded_assignment_plan" => Some(guarded_assignment_plan.clone()),
                "guarded_assignment_targets" => Some(Value::from_serialize(
                    d.guarded_assignments
                        .iter()
                        .map(|program| program.target_ranges())
                        .collect::<Vec<_>>(),
                )),
                "event_iteration_plan" => Some(Value::from_serialize(&d.event_iteration_plan)),
                "event_transactions" => Some(Value::from_serialize(&d.event_transactions)),
                "clock_partition_order" => Some(Value::from_serialize(&d.clock_partition_order)),
                "clock_owners" => Some(Value::from_serialize(&d.clock_owners)),
                "row_roles" => Some(Value::from_serialize(&d.row_roles)),
                "structured_row_roles" => Some(Value::from_serialize(
                    d.structured_updates
                        .iter()
                        .map(|update| update.role)
                        .collect::<Vec<_>>(),
                )),
                "guarded_assignment_clock_owners" => Some(Value::from_serialize(
                    d.guarded_assignments
                        .iter()
                        .map(|program| program.clock_owner())
                        .collect::<Vec<_>>(),
                )),
                "clock_partition_intermediate_plan" => Some(clock_intermediate_plan.clone()),
                "clock_partition_intermediate_targets" => Some(Value::from_serialize(
                    &d.clock_partition_intermediate_targets,
                )),
                "clock_partition_intermediate_clocks" => Some(Value::from_serialize(
                    &d.clock_partition_intermediate_clocks,
                )),
                "structured_rhs" => Some(structured_rhs.clone()),
                "structured_assignments" => Some(structured_assignments.clone()),
                "structured_clock_owners" => Some(Value::from_serialize(
                    d.structured_updates
                        .iter()
                        .map(|update| update.clock_owner)
                        .collect::<Vec<_>>(),
                )),
                "structured_update_count" => Some(Value::from(d.structured_updates.len())),
                "clock_partition_intermediate_count" => {
                    Some(Value::from(d.clock_partition_intermediates.len()))
                }
                "runtime_assignment_rhs" => Some(scalar_program_block_value(Arc::new(
                    d.runtime_assignment_rhs.clone(),
                ))),
                "runtime_assignment_targets" => {
                    Some(Value::from_serialize(&d.runtime_assignment_targets))
                }
                "runtime_assignment_roles" => {
                    Some(Value::from_serialize(&d.runtime_assignment_roles))
                }
                "post_commit_assignment_rhs" => Some(scalar_program_block_value(Arc::new(
                    d.post_commit_assignment_rhs.clone(),
                ))),
                "post_commit_assignment_targets" => {
                    Some(Value::from_serialize(&d.post_commit_assignment_targets))
                }
                "post_commit_assignment_runtime_rows" => Some(Value::from_serialize(
                    &d.post_commit_assignment_runtime_rows,
                )),
                "update_targets" => Some(Value::from_serialize(&targets)),
                "pre_modes" => Some(Value::from_serialize(&pre_modes)),
                "observation_refresh" => Some(Value::from_serialize(&observation_refresh)),
                _ => None,
            }
        },
    ))
}

fn delay_value(partition: &solve::SolveDelayPartition) -> Result<Value, CodegenError> {
    let plans = [
        &partition.source_rhs,
        &partition.delay_time_rhs,
        &partition.delay_max_rhs,
    ]
    .into_iter()
    .map(|block| {
        super::scalar_program_plan::ScalarProgramPlan::new(Arc::new(block.clone()))
            .map(Value::from_object)
    })
    .collect::<Result<Vec<_>, _>>()?;
    let partition = Arc::new(partition.clone());
    Ok(lazy_map(
        &[
            "source_rhs",
            "delay_time_rhs",
            "delay_max_rhs",
            "source_plan",
            "delay_time_plan",
            "delay_max_plan",
            "value_parameter_indices",
            "source_is_discrete",
        ],
        move |key| match key {
            "source_plan" => Some(plans[0].clone()),
            "delay_time_plan" => Some(plans[1].clone()),
            "delay_max_plan" => Some(plans[2].clone()),
            "source_rhs" => Some(scalar_program_block_value(Arc::new(
                partition.source_rhs.clone(),
            ))),
            "delay_time_rhs" => Some(scalar_program_block_value(Arc::new(
                partition.delay_time_rhs.clone(),
            ))),
            "delay_max_rhs" => Some(scalar_program_block_value(Arc::new(
                partition.delay_max_rhs.clone(),
            ))),
            "value_parameter_indices" => {
                Some(Value::from_serialize(&partition.value_parameter_indices))
            }
            "source_is_discrete" => Some(Value::from_serialize(&partition.source_is_discrete)),
            _ => None,
        },
    ))
}

fn events_value(handle: SolveRenderHandle) -> Result<Value, CodegenError> {
    let problem = handle.problem();
    let delays = delay_value(&problem.events.delays)?;
    let root_neighborhoods = Value::from_serialize(
        solve::root_neighborhoods(&problem.events.root_conditions)
            .map_err(|error| CodegenError::template(error.to_string()))?,
    );
    let root_plan = Value::from_object(super::scalar_program_plan::ScalarProgramPlan::new(
        Arc::new(problem.events.root_conditions.clone()),
    )?);
    let action_plan = Value::from_object(super::scalar_program_plan::ScalarProgramPlan::new(
        Arc::new(problem.events.action_conditions.clone()),
    )?);
    let dynamic_time_event_plan =
        Value::from_object(super::scalar_program_plan::ScalarProgramPlan::new(
            Arc::new(problem.events.dynamic_time_event_rhs.clone()),
        )?);
    let messages = super::fmi_c_assertions::messages(problem)?;
    let message_plan = messages.rows_value()?;
    let message_parts = Value::from_serialize(&messages.parts);
    Ok(lazy_map(
        &[
            "action_message_plan",
            "action_message_parts",
            "root_conditions",
            "condition_memory_parameter_indices",
            "has_terminal_event",
            "delays",
            "root_plan",
            "root_relation_memory_targets",
            "root_zero_domains",
            "root_relation_refresh_roles",
            "root_neighborhoods",
            "scheduled_root_conditions",
            "scheduled_time_events",
            "dynamic_time_event_names",
            "dynamic_time_event_rhs",
            "dynamic_time_event_plan",
            "action_conditions",
            "action_plan",
            "actions",
        ],
        move |k| {
            let e = &handle.problem().events;
            match k {
                "condition_memory_parameter_indices" => {
                    Some(Value::from_serialize(&e.condition_memory_parameter_indices))
                }
                "has_terminal_event" => Some(Value::from(e.has_terminal_event)),
                "delays" => Some(delays.clone()),
                "root_conditions" => Some(scalar_program_block_value(Arc::new(
                    e.root_conditions.clone(),
                ))),
                "root_plan" => Some(root_plan.clone()),
                "root_neighborhoods" => Some(root_neighborhoods.clone()),
                "dynamic_time_event_rhs" => Some(scalar_program_block_value(Arc::new(
                    e.dynamic_time_event_rhs.clone(),
                ))),
                "action_conditions" => Some(scalar_program_block_value(Arc::new(
                    e.action_conditions.clone(),
                ))),
                "dynamic_time_event_plan" => Some(dynamic_time_event_plan.clone()),
                "action_plan" => Some(action_plan.clone()),
                "action_message_plan" => Some(message_plan.clone()),
                "action_message_parts" => Some(message_parts.clone()),
                "root_relation_memory_targets" => {
                    Some(Value::from_serialize(&e.root_relation_memory_targets))
                }
                "root_zero_domains" => Some(Value::from_serialize(&e.root_zero_domains)),
                "root_relation_refresh_roles" => {
                    Some(Value::from_serialize(&e.root_relation_refresh_roles))
                }
                "scheduled_root_conditions" => {
                    Some(Value::from_serialize(&e.scheduled_root_conditions))
                }
                "scheduled_time_events" => Some(Value::from_serialize(&e.scheduled_time_events)),
                "dynamic_time_event_names" => {
                    Some(Value::from_serialize(&e.dynamic_time_event_names))
                }
                "actions" => Some(Value::from_serialize(&e.actions)),
                _ => None,
            }
        },
    ))
}

pub(super) fn artifacts_value(handle: SolveRenderHandle) -> Result<Value, CodegenError> {
    let continuous = continuous_artifacts_value(handle.clone())?;
    let initial = &handle.artifacts().initialization;
    let initialization = resolved_artifact_map([
        (
            "residual_jacobian_v",
            Some(compute_block_value(Arc::new(
                initial.residual_jacobian_v.clone(),
            ))?),
        ),
        (
            "update_jacobian_v",
            optional_scalar_plan(initial.update_jacobian_v.as_ref())?,
        ),
    ]);
    let discrete = &handle.artifacts().discrete;
    let discrete = resolved_artifact_map([
        (
            "rhs_jacobian_v",
            optional_scalar_plan(discrete.rhs_jacobian_v.as_ref())?,
        ),
        (
            "runtime_assignment_jacobian_v",
            optional_scalar_plan(discrete.runtime_assignment_jacobian_v.as_ref())?,
        ),
        (
            "guarded_jacobian_v",
            optional_scalar_plan(discrete.guarded_jacobian_v.as_ref())?,
        ),
        (
            "structured_jacobian_v",
            optional_scalar_plan(discrete.structured_jacobian_v.as_ref())?,
        ),
    ]);
    Ok(lazy_map(
        &["continuous", "initialization", "discrete"],
        move |k| match k {
            "continuous" => Some(continuous.clone()),
            "initialization" => Some(initialization.clone()),
            "discrete" => Some(discrete.clone()),
            _ => None,
        },
    ))
}

/// Artifact maps enumerate only the entries that are present, so a template
/// that walks the context never observes an unresolved optional plan.
fn resolved_artifact_map(
    entries: impl IntoIterator<Item = (&'static str, Option<Value>)>,
) -> Value {
    let present = entries
        .into_iter()
        .filter_map(|(key, value)| value.map(|value| (key, value)))
        .collect::<IndexMap<_, _>>();
    Value::from_serialize(present)
}

/// The lazy plan object for a scalar program block that is always present.
fn scalar_plan_value(block: &solve::ScalarProgramBlock) -> Result<Value, CodegenError> {
    super::scalar_program_plan::ScalarProgramPlan::new(Arc::new(block.clone()))
        .map(Value::from_object)
}

/// The lazy plan object for an optional scalar program block, or `None` when
/// the artifact is absent.
fn optional_scalar_plan(
    block: Option<&solve::ScalarProgramBlock>,
) -> Result<Option<Value>, CodegenError> {
    block.map(scalar_plan_value).transpose()
}

/// Lazy `solve.artifacts.continuous` map: the continuous Jacobian / mass-matrix
/// artifacts, each produced on demand so a target that never reads them pays
/// nothing for materializing op-heavy blocks.
fn continuous_artifacts_value(handle: SolveRenderHandle) -> Result<Value, CodegenError> {
    let implicit_jacobian_v = compute_block_value(Arc::new(
        handle.artifacts().continuous.implicit_jacobian_v.clone(),
    ))?;
    let manifold_jacobian_v = compute_block_value(Arc::new(
        handle.artifacts().continuous.manifold_jacobian_v.clone(),
    ))?;
    let full_jacobian_plan = scalar_plan_value(&handle.artifacts().continuous.full_jacobian_v)?;
    let implicit_full_jacobian_plan =
        scalar_plan_value(&handle.artifacts().continuous.implicit_jacobian_v_scalar)?;
    Ok(lazy_map(
        &[
            "mass_matrix",
            "manifold_jacobian_v",
            "implicit_jacobian_v",
            "full_jacobian_v",
            "full_jacobian_plan",
            "implicit_full_jacobian_plan",
        ],
        move |k| {
            let c = &handle.artifacts().continuous;
            match k {
                "manifold_jacobian_v" => Some(manifold_jacobian_v.clone()),
                "implicit_jacobian_v" => Some(implicit_jacobian_v.clone()),
                "full_jacobian_plan" => Some(full_jacobian_plan.clone()),
                "implicit_full_jacobian_plan" => Some(implicit_full_jacobian_plan.clone()),
                "full_jacobian_v" => Some(scalar_program_block_value(Arc::new(
                    c.full_jacobian_v.clone(),
                ))),
                "mass_matrix" => Some(Value::from_serialize(&c.mass_matrix)),
                _ => None,
            }
        },
    ))
}

/// Lazy `solve` context object: the `SolveProblem` fields plus an embedded
/// `artifacts` field (templates access `solve.artifacts.*`). Structural fields
/// (`layout`, `solve_layout`, targets) serialize eagerly (small); op-heavy
/// sub-systems are produced lazily.
pub(super) fn solve_value(handle: SolveRenderHandle) -> Result<Value, CodegenError> {
    let initialization = minijinja::context! {
        residual => compute_block_value(Arc::new(handle.problem().initialization.residual().clone()))?,
        update_plan => Value::from_object(super::scalar_program_plan::ScalarProgramPlan::new(
            Arc::new(handle.problem().initialization.update_rhs().clone()),
        )?),
        ..Value::from_serialize(&handle.problem().initialization)
    };
    let continuous = continuous_value(handle.clone())?;
    let discrete = discrete_value(handle.clone())?;
    let events = events_value(handle.clone())?;
    let artifacts_value = artifacts_value(handle.clone())?;
    Ok(lazy_map(
        &[
            "schema_version",
            "layout",
            "solve_layout",
            "continuous",
            "initialization",
            "discrete",
            "events",
            "clocks",
            "artifacts",
        ],
        move |k| match k {
            "schema_version" => Some(Value::from(handle.problem().schema_version)),
            "layout" => Some(Value::from_serialize(&handle.problem().layout)),
            "solve_layout" => Some(Value::from_serialize(&handle.problem().solve_layout)),
            "continuous" => Some(continuous.clone()),
            "discrete" => Some(discrete.clone()),
            "events" => Some(events.clone()),
            "initialization" => Some(initialization.clone()),
            "clocks" => Some(minijinja::context! {
                render_schedules => handle.problem().clocks.periodic_event_schedules.iter().map(clock_render_schedule).collect::<Vec<_>>(),
                pre_binding_schedules => handle.problem().solve_layout.pre_param_bindings.iter().map(|binding| binding.clock_schedule.as_ref().map(clock_render_schedule)).collect::<Vec<_>>(),
                ..Value::from_serialize(&handle.problem().clocks)
            }),
            "artifacts" => Some(artifacts_value.clone()),
            _ => None,
        },
    ))
}

fn clock_render_schedule(schedule: &solve::PeriodicEventSchedule) -> Value {
    minijinja::context! {
        period_num => schedule.lattice().period().numerator().to_string(),
        period_den => schedule.lattice().period().denominator().to_string(),
        phase_num => schedule.lattice().phase().numerator().to_string(),
        phase_den => schedule.lattice().phase().denominator().to_string(),
        anchor => Value::from_serialize(schedule.anchor()),
    }
}

/// Lazy `nodes` Seq of a `ComputeBlock` (each `ComputeNode` materialized on
/// demand, with its op lists lazy underneath).
pub(super) fn nodes_value(block: Arc<solve::ComputeBlock>) -> Result<Value, CodegenError> {
    Ok(lazy_seq(block.nodes.len(), move |i| {
        compute_node_value(Arc::new(block.nodes[i].clone())).unwrap_or_else(|error| {
            Value::from(crate::errors::render_err(error.to_string()).with_source(error))
        })
    }))
}

/// The guarded assignments as one renderable plan, their outputs numbered in
/// program order (the order `fmi.scalar_events.guarded_targets` lists).
fn guarded_assignment_plan(discrete: &solve::DiscreteSolveSystem) -> Result<Value, CodegenError> {
    let (programs, spans) = discrete
        .guarded_assignments
        .iter()
        .map(|program| (program.program().to_vec(), program.span()))
        .unzip();
    let block = solve::ScalarProgramBlock::with_program_spans(programs, spans)
        .map_err(|error| CodegenError::template(error.to_string()))?;
    Ok(Value::from_object(
        super::scalar_program_plan::ScalarProgramPlan::new(Arc::new(block))?,
    ))
}
