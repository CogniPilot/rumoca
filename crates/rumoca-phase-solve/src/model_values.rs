use std::collections::HashMap;

use rumoca_core::Span;
use rumoca_eval_dae::{NumericEvaluationError, NumericEvaluationErrorKind, NumericEvaluator};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::{LowerError, lower_selection, lower_solve_artifacts};

/// Failure while constructing the complete executable Solve root from one DAE.
#[derive(Debug)]
pub enum SolveModelLoweringError {
    Lower(LowerError),
    RuntimeValues { message: String, span: Option<Span> },
    InvalidOverride { message: String },
}

impl SolveModelLoweringError {
    #[must_use]
    pub fn source_span(&self) -> Option<Span> {
        match self {
            Self::Lower(error) => error.source_span(),
            Self::RuntimeValues { span, .. } => *span,
            Self::InvalidOverride { .. } => None,
        }
    }
}

impl std::fmt::Display for SolveModelLoweringError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lower(error) => write!(formatter, "{error}"),
            Self::RuntimeValues { message, .. } | Self::InvalidOverride { message } => {
                write!(formatter, "{message}")
            }
        }
    }
}

impl std::error::Error for SolveModelLoweringError {}

impl From<LowerError> for SolveModelLoweringError {
    fn from(error: LowerError) -> Self {
        Self::Lower(error)
    }
}

/// Complete checked Solve construction plus its phase-owned timing split.
pub struct LoweredSolveModel<'source> {
    model: solve::SolveModel,
    prepared: rumoca_phase_structural::PreparedDae<'source>,
    program_seconds: f64,
    runtime_value_seconds: f64,
}

/// Phase-owned milestones in complete Solve-model construction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveModelLoweringStage {
    Programs,
    RuntimeValues,
}

impl LoweredSolveModel<'_> {
    /// The exact structurally prepared DAE this executable root was lowered
    /// from. FMI metadata must inspect this value, never the pre-transform
    /// source root beside it.
    #[must_use]
    pub fn prepared_dae(&self) -> &dae::Dae {
        self.prepared.as_dae()
    }

    #[must_use]
    pub fn program_seconds(&self) -> f64 {
        self.program_seconds
    }

    #[must_use]
    pub fn runtime_value_seconds(&self) -> f64 {
        self.runtime_value_seconds
    }

    /// Borrow the executable root while it remains correlated with the exact
    /// prepared DAE that produced it.
    ///
    /// This supports observers and execution-backend preparation before the
    /// pair is consumed into an FMI component; it provides no mutable or owned
    /// escape.
    #[must_use]
    pub fn model(&self) -> &solve::SolveModel {
        &self.model
    }

    /// Apply one checked experiment-level initial-state override without
    /// exposing mutable Solve IR.
    pub fn set_initial_state(
        &mut self,
        name: &str,
        value: f64,
    ) -> Result<(), SolveModelLoweringError> {
        if !value.is_finite() {
            return Err(SolveModelLoweringError::InvalidOverride {
                message: format!("start override for `{name}` must be finite"),
            });
        }
        let state_count = self.model.state_scalar_count();
        let state_names = self
            .model
            .problem
            .solve_layout
            .solver_maps
            .names
            .get(..state_count)
            .ok_or_else(|| SolveModelLoweringError::InvalidOverride {
                message: "state count exceeds the checked Solve name layout".to_owned(),
            })?;
        let index = state_names
            .iter()
            .position(|candidate| candidate == name)
            .ok_or_else(|| SolveModelLoweringError::InvalidOverride {
                message: format!("`{name}` is not a state of this model"),
            })?;
        let target = self.model.initial_y.get_mut(index).ok_or_else(|| {
            SolveModelLoweringError::InvalidOverride {
                message: format!(
                    "state `{name}` has no initial-value slot in the checked Solve model"
                ),
            }
        })?;
        *target = value;
        Ok(())
    }

    #[must_use]
    pub fn into_model(self) -> solve::SolveModel {
        self.model
    }
}

/// Construct one complete executable Solve root from one checked DAE.
pub fn lower_solve_model<'source>(
    model: &'source dae::Dae,
    overrides: &HashMap<String, f64>,
    mut begin_stage: impl FnMut(SolveModelLoweringStage),
) -> Result<LoweredSolveModel<'source>, SolveModelLoweringError> {
    begin_stage(SolveModelLoweringStage::Programs);
    let program_start = rumoca_core::maybe_start_timer();
    let selection = crate::state_selection::prepare(model, overrides).map_err(|error| {
        LowerError::Structural {
            reason: error.to_string(),
            span: error.source_span(),
        }
    })?;
    let package = lower_selection(&selection, overrides)?;
    let prepared = selection.primary;
    let problem = package.problem;
    let artifacts = lower_solve_artifacts(&problem)?;
    let program_seconds = rumoca_core::maybe_elapsed_seconds(program_start);

    begin_stage(SolveModelLoweringStage::RuntimeValues);
    let runtime_value_start = rumoca_core::maybe_start_timer();
    let vectors = runtime_vectors(prepared.as_dae(), &problem, overrides)?;
    let solve_model = solve::SolveModel {
        problem,
        pure_calls: package.pure_calls,
        artifacts,
        initial_y: vectors.initial_y,
        solver_nominals: vectors.solver_nominals,
        parameters: vectors.parameters,
        external_tables: solve::ExternalTables::new(model.external_tables().to_vec()),
        visible_names: vectors.visible_names,
        visible_value_rows: vectors.visible_value_rows,
        variable_meta: vectors.variable_meta,
    };
    solve_model.validate().map_err(LowerError::from)?;
    let runtime_value_seconds = rumoca_core::maybe_elapsed_seconds(runtime_value_start);
    Ok(LoweredSolveModel {
        model: solve_model,
        prepared,
        program_seconds,
        runtime_value_seconds,
    })
}

struct RuntimeVectors {
    initial_y: Vec<f64>,
    solver_nominals: Vec<f64>,
    parameters: Vec<f64>,
    visible_names: Vec<String>,
    visible_value_rows: solve::ScalarProgramBlock,
    variable_meta: Vec<solve::SolveVariableMeta>,
}

fn runtime_vectors(
    model: &dae::Dae,
    problem: &solve::SolveProblem,
    overrides: &HashMap<String, f64>,
) -> Result<RuntimeVectors, SolveModelLoweringError> {
    model.inspect(|view| {
        let evaluator = NumericEvaluator::with_overrides(view, |variable, scalar| {
            variable
                .scalar_name(scalar)
                .and_then(|name| overrides.get(&name).copied())
        });
        RuntimeVectorBuilder {
            model,
            view,
            problem,
            evaluator,
        }
        .build()
    })
}

struct RuntimeVectorBuilder<'model, 'dae, F> {
    model: &'model dae::Dae,
    view: dae::DaeView<'dae>,
    problem: &'model solve::SolveProblem,
    evaluator: NumericEvaluator<'dae, F>,
}

impl<'dae, F> RuntimeVectorBuilder<'_, 'dae, F>
where
    F: FnMut(dae::VariableView<'dae>, usize) -> Option<f64>,
{
    fn build(mut self) -> Result<RuntimeVectors, SolveModelLoweringError> {
        let mut columns = RuntimeColumns {
            initial_y: vec![0.0; self.problem.layout.y_scalars()],
            solver_nominals: vec![1.0; self.problem.layout.y_scalars()],
            parameters: vec![0.0; self.problem.layout.p_scalars()],
        };
        self.seed_homotopy_continuation(&mut columns)?;

        for (id, variable) in self.view.variables() {
            if is_non_numeric(variable) {
                continue;
            }
            let values = self.evaluator.initial_value(id).map_err(evaluation_error)?;
            let nominals = self.variable_nominals(variable)?;
            self.write_variable(variable, &values, &nominals, &mut columns)?;
        }
        self.seed_alias_class_starts(&mut columns)?;
        let (visible_names, visible_value_rows, variable_meta) = self.visible_projections()?;
        Ok(RuntimeVectors {
            initial_y: columns.initial_y,
            solver_nominals: columns.solver_nominals,
            parameters: columns.parameters,
            visible_names,
            visible_value_rows,
            variable_meta,
        })
    }

    /// Seed the hidden homotopy continuation slot (λ) to `1.0`.
    ///
    /// MLS 3.6 §3.7.4.3 defines `homotopy(actual, simplified)` through the blend
    /// `lambda*actual + (1 - lambda)*simplified` and explicitly permits the
    /// trivial implementation `homotopy(actual, simplified) = actual`. λ = 1
    /// *is* that trivial implementation for the blend the Solve lowering emits,
    /// so seeding `1.0` makes `actual` the default reading of every homotopy
    /// expression: simulation-time rows, rows in systems the initialization
    /// continuation does not steer, and every backend that has no continuation
    /// at all agree on it. Seeding `0.0` would instead make `simplified` the
    /// default — a system the model author supplied only as a starting guess —
    /// for anything the continuation failed to reach.
    ///
    /// `SolveRuntime::project_initial_variables` is the only writer that moves
    /// λ: it drives `0 -> 1` around the solves its coverage certificate names
    /// and leaves `1.0` behind.
    fn seed_homotopy_continuation(
        &self,
        columns: &mut RuntimeColumns,
    ) -> Result<(), SolveModelLoweringError> {
        let Some(index) = self.problem.solve_layout.initial_homotopy_parameter_index else {
            return Ok(());
        };
        let len = columns.parameters.len();
        let slot = columns.parameters.get_mut(index).ok_or_else(|| {
            runtime_error(
                format!(
                    "initial homotopy parameter index {index} is outside the {len} runtime \
                     parameters"
                ),
                first_span(self.view),
            )
        })?;
        *slot = 1.0;
        Ok(())
    }

    fn visible_projections(
        &self,
    ) -> Result<
        (
            Vec<String>,
            solve::ScalarProgramBlock,
            Vec<solve::SolveVariableMeta>,
        ),
        SolveModelLoweringError,
    > {
        let mut names = Vec::new();
        let mut programs = Vec::new();
        let mut spans = Vec::new();
        let mut metadata = Vec::new();
        for (id, variable) in self
            .view
            .variables()
            .filter(|(_, variable)| is_visible_role(variable.role()) && !is_non_numeric(*variable))
        {
            for scalar in 0..variable.scalar_count() {
                let name = scalar_name(variable, scalar)?;
                let slot = visible_variable_slot(self.problem, id, variable, scalar, &name)?;
                programs.push(slot_projection(slot, variable.declaration().span())?);
                spans.push(variable.declaration().span());
                metadata.push(self.variable_meta(id, variable, name.clone()));
                names.push(name);
            }
        }
        let rows = solve::ScalarProgramBlock::with_program_spans(programs, spans)
            .map_err(|error| runtime_error(error.to_string(), first_span(self.view)))?;
        Ok((names, rows, metadata))
    }

    fn write_variable(
        &self,
        variable: dae::VariableView<'dae>,
        values: &[f64],
        nominals: &[f64],
        columns: &mut RuntimeColumns,
    ) -> Result<(), SolveModelLoweringError> {
        for scalar in 0..variable.scalar_count() {
            let name = scalar_name(variable, scalar)?;
            let slot = variable_slot(self.problem, variable, &name)?;
            match slot {
                solve::ScalarSlot::Y { index, .. } => {
                    columns.initial_y[index] = values[scalar];
                    columns.solver_nominals[index] = nominals[scalar];
                }
                solve::ScalarSlot::P { index, .. } => columns.parameters[index] = values[scalar],
                solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => {
                    return Err(runtime_error(
                        format!("checked variable `{name}` was assigned a non-storage slot"),
                        variable.declaration().span(),
                    ));
                }
            }
        }
        Ok(())
    }

    fn variable_nominals(
        &mut self,
        variable: dae::VariableView<'dae>,
    ) -> Result<Vec<f64>, SolveModelLoweringError> {
        let Some(expression) = variable.nominal() else {
            return Ok(vec![1.0; variable.scalar_count()]);
        };
        let mut values = self
            .evaluator
            .expression(expression)
            .map_err(evaluation_error)?;
        if values.len() == 1 && variable.scalar_count() > 1 {
            values.resize(variable.scalar_count(), values[0]);
        }
        if values.len() != variable.scalar_count()
            || values
                .iter()
                .any(|value| !value.is_finite() || *value <= 0.0)
        {
            return Err(runtime_error(
                format!(
                    "nominal for `{}` must contain {} finite positive values",
                    variable.name(),
                    variable.scalar_count()
                ),
                expression_span(self.view, expression),
            ));
        }
        Ok(values)
    }

    /// Seed the initialization guess of every aliased coordinate from the class
    /// member that carries the `fixed = true` start.
    ///
    /// MLS 3.6 §8.6 turns a `fixed = true` start into the initialization
    /// equation `vc = vc.start`, and a chain of exact equalities `vc = a`,
    /// `a = b` states that value about `a` and `b` as well. Index reduction
    /// keeps the pinned member as the defined alias and selects one of the
    /// unpinned members as the coordinate it integrates, and that integrated
    /// coordinate has no start of its own, so its guess defaults to zero. An
    /// algebraic law singular at the origin, such as point gravity
    /// `-(mu/(r*r)) * r/|r|`, then evaluates a non-finite residual at that guess
    /// and no initialization solve can leave it. Carrying the pinned start
    /// across the whole equivalence class makes the first residual evaluation
    /// finite; the value seeded is a guess the initialization system still
    /// recomputes, never a substitute for the stated equation. When two members
    /// of one class carry different `fixed = true` starts, the class is left
    /// untouched: structural preparation reports that contradiction, and this
    /// seeding never resolves it by silently choosing one.
    fn seed_alias_class_starts(
        &self,
        columns: &mut RuntimeColumns,
    ) -> Result<(), SolveModelLoweringError> {
        let classes = self.collect_alias_classes(columns.initial_y.len());
        let anchors = self.alias_class_anchors(&classes, &columns.initial_y);
        for index in 0..columns.initial_y.len() {
            let (root, sign) = classes.find(index);
            if let Some(anchor) = anchors[root].filter(|anchor| !anchor.conflicting) {
                columns.initial_y[index] = sign * anchor.value;
            }
        }
        Ok(())
    }

    /// Union solver Y slots proven equal by an exact continuous alias equality.
    fn collect_alias_classes(&self, slot_count: usize) -> SignedUnionFind {
        let mut classes = SignedUnionFind::new(slot_count);
        for owner in self.view.continuous_owners() {
            let residuals = match owner {
                dae::ContinuousOwnerView::Residual { equation, .. } => {
                    vec![equation.residual()]
                }
                dae::ContinuousOwnerView::Structured { family, .. } => {
                    family.bodies().iter().collect()
                }
            };
            for residual in residuals {
                union_alias_equation(self.view, self.problem, residual, &mut classes);
            }
        }
        classes
    }

    /// The pinned value of each alias class root, flagged when two members carry
    /// contradicting `fixed = true` starts.
    fn alias_class_anchors(
        &self,
        classes: &SignedUnionFind,
        initial_y: &[f64],
    ) -> Vec<Option<AliasAnchor>> {
        let mut anchors = vec![None; initial_y.len()];
        for (_, variable) in self.view.variables() {
            for scalar in 0..variable.scalar_count() {
                self.record_scalar_anchor(classes, variable, scalar, initial_y, &mut anchors);
            }
        }
        anchors
    }

    fn record_scalar_anchor(
        &self,
        classes: &SignedUnionFind,
        variable: dae::VariableView<'dae>,
        scalar: usize,
        initial_y: &[f64],
        anchors: &mut [Option<AliasAnchor>],
    ) {
        if variable.fixed_scalar(scalar) != Some(true) {
            return;
        }
        let Some(index) = y_slot_index(self.problem, variable, scalar) else {
            return;
        };
        let value = initial_y[index];
        if !value.is_finite() {
            return;
        }
        let (root, sign) = classes.find(index);
        merge_alias_anchor(&mut anchors[root], value * sign);
    }

    fn variable_meta(
        &self,
        id: dae::VariableId<'dae>,
        variable: dae::VariableView<'dae>,
        name: String,
    ) -> solve::SolveVariableMeta {
        let time_domain = self
            .problem
            .solve_layout
            .variable_declarations
            .get(id.index() as usize)
            .expect("checked Solve declaration catalog follows dense DAE identity")
            .time_domain();
        solve::SolveVariableMeta {
            name,
            source_span: variable.declaration().span(),
            role: role_name(variable.role()).to_string(),
            is_state: variable.role() == dae::VariableRole::State,
            value_type: Some(format!("{:?}", variable.value_type().scalar_type())),
            variability: Some(format!("{:?}", variable.variability())),
            time_domain: Some(time_domain.as_str().to_string()),
            unit: variable.unit().map(str::to_string),
            start: variable
                .start()
                .and_then(|expression| self.expression_source(expression)),
            min: variable
                .minimum()
                .and_then(|expression| self.expression_source(expression)),
            max: variable
                .maximum()
                .and_then(|expression| self.expression_source(expression)),
            nominal: variable
                .nominal()
                .and_then(|expression| self.expression_source(expression)),
            // Reporting metadata only: initialization reads `fixed` per scalar
            // (initial_pins), so a non-uniform continuous `fixed` reducing to
            // None here does not affect the solve.
            fixed: variable.fixed_uniform(),
            description: variable.description().map(str::to_string),
        }
    }

    fn expression_source(&self, expression: dae::ExprId<'dae>) -> Option<String> {
        let provenance = self.view.expression(expression)?.provenance();
        self.model.source_text(provenance).map(str::to_string)
    }
}

fn visible_variable_slot(
    problem: &solve::SolveProblem,
    id: dae::VariableId<'_>,
    variable: dae::VariableView<'_>,
    scalar: usize,
    name: &str,
) -> Result<solve::ScalarSlot, SolveModelLoweringError> {
    problem
        .solve_layout
        .variable_scalar_slot(id.index() as usize, scalar)
        .ok_or_else(|| {
            runtime_error(
                format!("checked variable `{name}` has no typed Solve slot"),
                variable.declaration().span(),
            )
        })
}

struct RuntimeColumns {
    initial_y: Vec<f64>,
    solver_nominals: Vec<f64>,
    parameters: Vec<f64>,
}

/// True when a checked declaration carries no numeric value at all.
///
/// The runtime vectors are `f64` columns; MLS §3.8.4 gives `String` no numeric value, so
/// a `String` declaration (for example `Modelica.Clocked.Types.SolverMethod solverMethod`
/// on every clocked partition) has nothing to initialize and nothing to trace. Skipping
/// it substitutes no stand-in value: the checked DAE rejects a `String` operand in any
/// numeric expression or residual at construction, so no lowered program can read the
/// storage column the layout reserves for it.
fn is_non_numeric(variable: dae::VariableView<'_>) -> bool {
    variable.value_type().scalar_type() == dae::ScalarType::String
}

fn is_visible_role(role: dae::VariableRole) -> bool {
    matches!(
        role,
        dae::VariableRole::State
            | dae::VariableRole::Algebraic
            | dae::VariableRole::Output
            | dae::VariableRole::Input
            | dae::VariableRole::DiscreteReal
            | dae::VariableRole::DiscreteValue
    )
}

fn slot_projection(
    slot: solve::ScalarSlot,
    span: Span,
) -> Result<Vec<solve::LinearOp>, SolveModelLoweringError> {
    let load = match slot {
        solve::ScalarSlot::Y { index, .. } => solve::LinearOp::LoadY { dst: 0, index },
        solve::ScalarSlot::P { index, .. } => solve::LinearOp::LoadP { dst: 0, index },
        solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => {
            return Err(runtime_error(
                "visible DAE coordinate does not map to writable Solve storage",
                span,
            ));
        }
    };
    Ok(vec![load, solve::LinearOp::StoreOutput { src: 0 }])
}

/// Resolve a causal definition that is a whole-coordinate alias, returning the
/// sign the equality carries and the referenced algebraic coordinate.
///
/// Only an exact `+other` or `-other` over a whole algebraic coordinate counts:
/// the class members are then element-aligned, so the start of one seeds the
/// other scalar for scalar without resolving subscripts. A definition that adds
/// a displacement or reads more than one coordinate is not a plain alias and is
/// left for the initialization solve.
/// Tolerance on agreement between two `fixed = true` starts in one alias class.
const ALIAS_START_EPS: f64 = 1.0e-9;

/// The value one alias equivalence class is pinned to, plus whether two of its
/// `fixed = true` members disagree on it.
#[derive(Clone, Copy)]
struct AliasAnchor {
    value: f64,
    conflicting: bool,
}

/// Disjoint-set forest over solver Y slots that records the sign relating each
/// member to its root: `y[i] = sign(i) * y[root(i)]`.
///
/// Union by size bounds tree height to `log2(n)`, so `find` stays cheap without
/// path compression and needs no interior mutability.
struct SignedUnionFind {
    parent: Vec<usize>,
    sign: Vec<f64>,
    size: Vec<usize>,
}

impl SignedUnionFind {
    fn new(count: usize) -> Self {
        Self {
            parent: (0..count).collect(),
            sign: vec![1.0; count],
            size: vec![1; count],
        }
    }

    fn find(&self, mut node: usize) -> (usize, f64) {
        let mut sign = 1.0;
        while self.parent[node] != node {
            sign *= self.sign[node];
            node = self.parent[node];
        }
        (node, sign)
    }

    /// Record `y[a] = relation * y[b]` with `relation` either `+1` or `-1`.
    fn union(&mut self, a: usize, b: usize, relation: f64) {
        let (root_a, sign_a) = self.find(a);
        let (root_b, sign_b) = self.find(b);
        if root_a == root_b {
            return;
        }
        // y[a] = relation * y[b], y[a] = sign_a * y[root_a], y[b] = sign_b * y[root_b]
        // give y[root_a] = relation * sign_a * sign_b * y[root_b] (signs are +-1).
        let root_relation = relation * sign_a * sign_b;
        if self.size[root_a] < self.size[root_b] {
            self.parent[root_a] = root_b;
            self.sign[root_a] = root_relation;
            self.size[root_b] += self.size[root_a];
        } else {
            self.parent[root_b] = root_a;
            self.sign[root_b] = root_relation;
            self.size[root_a] += self.size[root_b];
        }
    }
}

/// Fold one `fixed = true` member's value into its class anchor, marking the
/// anchor conflicting when a second member states a different value.
fn merge_alias_anchor(anchor: &mut Option<AliasAnchor>, root_value: f64) {
    match anchor {
        None => {
            *anchor = Some(AliasAnchor {
                value: root_value,
                conflicting: false,
            });
        }
        Some(existing) => {
            if (existing.value - root_value).abs() > ALIAS_START_EPS {
                existing.conflicting = true;
            }
        }
    }
}

/// The solver Y slot of one scalar of a variable, when it has one.
fn y_slot_index(
    problem: &solve::SolveProblem,
    variable: dae::VariableView<'_>,
    scalar: usize,
) -> Option<usize> {
    let name = variable.scalar_name(scalar)?;
    match problem.layout.binding(&name)? {
        solve::ScalarSlot::Y { index, .. } => Some(index),
        _ => None,
    }
}

/// Union the aligned scalar members of an exact equality residual `a - b = 0`.
///
/// Only exact `+-` coordinate equalities count: both sides must expand to the
/// same number of scalar coordinate reads. A residual that reads a coordinate
/// through arithmetic, a displacement, or a call is not a plain alias and joins
/// no class here.
fn union_alias_equation<'dae>(
    view: dae::DaeView<'dae>,
    problem: &solve::SolveProblem,
    residual: dae::ExprId<'dae>,
    classes: &mut SignedUnionFind,
) {
    let Some((lhs, rhs)) = alias_equation_sides(view, residual) else {
        return;
    };
    let (Some(lhs_terms), Some(rhs_terms)) = (
        alias_scalar_terms(view, problem, lhs, 1.0),
        alias_scalar_terms(view, problem, rhs, 1.0),
    ) else {
        return;
    };
    if lhs_terms.len() != rhs_terms.len() {
        return;
    }
    for ((lhs_sign, lhs_index), (rhs_sign, rhs_index)) in lhs_terms.into_iter().zip(rhs_terms) {
        if let (Some(lhs_index), Some(rhs_index)) = (lhs_index, rhs_index) {
            classes.union(lhs_index, rhs_index, lhs_sign * rhs_sign);
        }
    }
}

/// View a residual root as the two sides of the equality it states, folding the
/// sign wrappers and literal-zero terms an exact `a = b` can carry.
fn alias_equation_sides<'dae>(
    view: dae::DaeView<'dae>,
    mut expression: dae::ExprId<'dae>,
) -> Option<(dae::ExprId<'dae>, dae::ExprId<'dae>)> {
    loop {
        match view.expression(expression)?.operation() {
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
                operand,
            } => expression = operand,
            dae::ExpressionOperation::Binary {
                operator: operator @ (dae::BinaryOperator::Add | dae::BinaryOperator::Subtract),
                lhs,
                rhs,
            } => {
                if is_literal_zero(view, lhs) {
                    expression = rhs;
                } else if is_literal_zero(view, rhs) {
                    expression = lhs;
                } else if operator == dae::BinaryOperator::Subtract {
                    return Some((lhs, rhs));
                } else {
                    return None;
                }
            }
            _ => return None,
        }
    }
}

fn is_literal_zero<'dae>(view: dae::DaeView<'dae>, expression: dae::ExprId<'dae>) -> bool {
    matches!(
        view.expression(expression).map(|node| node.operation()),
        Some(dae::ExpressionOperation::Literal(
            dae::DaeLiteral::Integer(0) | dae::DaeLiteral::Real(0.0)
        ))
    )
}

/// Expand one side of an alias equality into its aligned scalar coordinate
/// reads, each as `(sign, solver Y slot)`. A scalar with no Y slot keeps its
/// position with `None` so the two sides stay aligned. Any operand that is not
/// a signed coordinate, a single constant-index element, or an array of those
/// disqualifies the whole side.
fn alias_scalar_terms<'dae>(
    view: dae::DaeView<'dae>,
    problem: &solve::SolveProblem,
    expression: dae::ExprId<'dae>,
    sign: f64,
) -> Option<Vec<(f64, Option<usize>)>> {
    match view.expression(expression)?.operation() {
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus,
            operand,
        } => alias_scalar_terms(view, problem, operand, sign),
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Negate,
            operand,
        } => alias_scalar_terms(view, problem, operand, -sign),
        dae::ExpressionOperation::Coordinate(coordinate) => {
            let variable_id = value_coordinate_variable(view, expression, coordinate)?;
            let variable = view.variable(variable_id)?;
            Some(
                (0..variable.scalar_count())
                    .map(|scalar| (sign, y_slot_index(problem, variable, scalar)))
                    .collect(),
            )
        }
        dae::ExpressionOperation::Index { base, subscripts } => {
            let dae::ExpressionOperation::Coordinate(coordinate) =
                view.expression(base)?.operation()
            else {
                return None;
            };
            let variable_id = value_coordinate_variable(view, base, coordinate)?;
            let variable = view.variable(variable_id)?;
            let scalar = single_index_scalar(view, subscripts, variable.scalar_count())?;
            Some(vec![(sign, y_slot_index(problem, variable, scalar))])
        }
        dae::ExpressionOperation::Array(operands) => {
            let mut terms = Vec::new();
            for operand in operands.iter() {
                terms.extend(alias_scalar_terms(view, problem, operand, sign)?);
            }
            Some(terms)
        }
        _ => None,
    }
}

/// The variable behind a coordinate read that stores its own value, or `None`
/// for reads that do not: a `der(x)` coordinate shares the raw index of `x` yet
/// holds the derivative, not the value, so treating it as an alias of `x` would
/// merge a coordinate with its own rate. Only state and algebraic value reads
/// name the slot that carries the value an alias equality equates.
fn value_coordinate_variable<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    coordinate: dae::CoordinateView<'dae>,
) -> Option<dae::VariableId<'dae>> {
    match coordinate {
        dae::CoordinateView::State(_) | dae::CoordinateView::Algebraic(_) => {
            view.expression(expression)?.variable_coordinate()
        }
        _ => None,
    }
}

/// Resolve a single one-based constant subscript into a zero-based scalar of a
/// one-dimensional array coordinate.
fn single_index_scalar<'dae>(
    view: dae::DaeView<'dae>,
    subscripts: dae::SubscriptsView<'dae>,
    scalar_count: usize,
) -> Option<usize> {
    if subscripts.len() != 1 {
        return None;
    }
    let dae::SubscriptView::Index { expression, .. } = subscripts.get(0)? else {
        return None;
    };
    let dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(index)) =
        view.expression(expression)?.operation()
    else {
        return None;
    };
    let scalar = usize::try_from(index.checked_sub(1)?).ok()?;
    (scalar < scalar_count).then_some(scalar)
}

fn scalar_name(
    variable: dae::VariableView<'_>,
    scalar: usize,
) -> Result<String, SolveModelLoweringError> {
    variable.scalar_name(scalar).ok_or_else(|| {
        runtime_error(
            format!(
                "checked variable `{}` has no scalar name at ordinal {scalar}",
                variable.name()
            ),
            variable.declaration().span(),
        )
    })
}

fn variable_slot(
    problem: &solve::SolveProblem,
    variable: dae::VariableView<'_>,
    name: &str,
) -> Result<solve::ScalarSlot, SolveModelLoweringError> {
    problem.layout.binding(name).ok_or_else(|| {
        runtime_error(
            format!("checked variable `{name}` has no Solve storage slot"),
            variable.declaration().span(),
        )
    })
}

fn evaluation_error(error: NumericEvaluationError) -> SolveModelLoweringError {
    if error.kind() == NumericEvaluationErrorKind::InvalidOverride {
        SolveModelLoweringError::InvalidOverride {
            message: error.to_string(),
        }
    } else {
        runtime_error(error.to_string(), error.span())
    }
}

fn expression_span<'dae>(view: dae::DaeView<'dae>, expression: dae::ExprId<'dae>) -> Span {
    view.expression(expression)
        .expect("finalized expression identity resolves")
        .provenance()
        .span()
}

const fn role_name(role: dae::VariableRole) -> &'static str {
    match role {
        dae::VariableRole::Parameter => "parameter",
        dae::VariableRole::Constant => "constant",
        dae::VariableRole::Input => "input",
        dae::VariableRole::State => "state",
        dae::VariableRole::Algebraic => "algebraic",
        dae::VariableRole::Output => "output",
        dae::VariableRole::DiscreteReal => "discrete-real",
        dae::VariableRole::DiscreteValue => "discrete-valued",
    }
}

fn first_span(view: dae::DaeView<'_>) -> Span {
    view.responsible_span()
        .expect("runtime layout with visible variables has responsible provenance")
}

fn runtime_error(message: impl Into<String>, span: Span) -> SolveModelLoweringError {
    SolveModelLoweringError::RuntimeValues {
        message: message.into(),
        span: (!span.is_dummy()).then_some(span),
    }
}
