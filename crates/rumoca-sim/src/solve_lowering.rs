use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_solver::SimOptions;

// Re-exported through the sim facade so the root stays a curated same-crate
// facade (see `architecture_hardening_test::test_sim_facade_cross_crate_exports_are_curated`).
pub use rumoca_eval_solve::{EvalAtReport, EvalAtSlot, JacobianReport};
pub use rumoca_phase_structural::{BlockReport, StructuralReport, TearingReport};
#[cfg(not(target_arch = "wasm32"))]
use std::time::Instant;

#[derive(Debug)]
pub enum SimulationDiagnosticError {
    SolveLowering(rumoca_phase_solve::SolveModelLowerError),
    Solver(String),
    RuntimePreparation {
        message: String,
        span: Option<rumoca_core::Span>,
    },
}

impl SimulationDiagnosticError {
    pub fn diagnostic_code(&self) -> &'static str {
        match self {
            Self::SolveLowering(_) => "lowering",
            Self::Solver(_) | Self::RuntimePreparation { .. } => "simulation",
        }
    }

    pub fn diagnostic_label(&self) -> String {
        match self {
            Self::SolveLowering(error) => error.diagnostic_label(),
            Self::Solver(_) | Self::RuntimePreparation { .. } => {
                "simulation failure originates here".to_string()
            }
        }
    }

    pub fn source_span(&self) -> Option<rumoca_core::Span> {
        match self {
            Self::SolveLowering(error) => error.source_span(),
            Self::Solver(_) => None,
            Self::RuntimePreparation { span, .. } => *span,
        }
    }
}

impl std::fmt::Display for SimulationDiagnosticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SolveLowering(error) => write!(f, "{error}"),
            Self::Solver(error) => write!(f, "{error}"),
            Self::RuntimePreparation { message, .. } => write!(f, "{message}"),
        }
    }
}

impl std::error::Error for SimulationDiagnosticError {}

impl From<rumoca_eval_solve::EvalSolveError> for SimulationDiagnosticError {
    fn from(value: rumoca_eval_solve::EvalSolveError) -> Self {
        Self::RuntimePreparation {
            message: value.to_string(),
            span: value.source_span(),
        }
    }
}

pub fn lower_dae_for_simulation(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<solve::SolveModel, rumoca_phase_solve::SolveModelLowerError> {
    lower_dae_for_simulation_with_stage_timing(dae_model, opts, |_| {}).map(|(model, _)| model)
}

pub fn lower_dae_for_gpu_preparation(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<solve::SolveModel, rumoca_phase_solve::SolveModelLowerError> {
    let structurally_lowered = structurally_lower_dae_for_simulation(dae_model, opts)?;
    rumoca_phase_solve::lower_dae_to_solve_model_owned_for_gpu_preparation_with_metadata(
        structurally_lowered.dae,
        &structurally_lowered.metadata_dae,
    )
}

/// Tolerance / iteration budget for the `--inspect eval` algebraic refresh, matching
/// the rk45 backend's runtime settings so the probe sees the same algebraic
/// solution the solver would at that point.
const EVAL_AT_REFRESH_TOL: f64 = 1.0e-10;
const EVAL_AT_REFRESH_MAX_ITERS: usize = 32;

/// Result of a [`eval_dae_at`] probe: the named per-variable report plus the
/// state vector and state names actually evaluated.
#[derive(Debug, Clone)]
pub struct EvalAtProbe {
    /// Named solver values and state derivatives, with non-finite entries flagged.
    pub report: rumoca_eval_solve::EvalAtReport,
    /// State vector used for the evaluation, in model state order.
    pub state_used: Vec<f64>,
    /// State names in model order, aligned with `state_used` — the authoritative
    /// answer to "what states does this model have?".
    pub state_names: Vec<String>,
}

/// Lower `dae_model` to the solver runtime and evaluate it at `(state, t)`,
/// naming every solver value and state derivative and flagging non-finite ones.
///
/// States are addressed by name (`state_overrides`), never by position, so the
/// probe is robust to state reordering during lowering: any state not listed
/// keeps its model initial value, and an unknown name is an error that lists the
/// valid state names. This is a one-shot, scriptable NaN trace — it attributes a
/// `NaN`/`inf` to a specific model variable in one command instead of repeated
/// instrumented rebuilds.
pub fn eval_dae_at(
    dae_model: &dae::Dae,
    opts: &SimOptions,
    state_overrides: &[(String, f64)],
    t: f64,
) -> Result<EvalAtProbe, SimulationDiagnosticError> {
    let solve_model = lower_dae_for_simulation(dae_model, opts)
        .map_err(SimulationDiagnosticError::SolveLowering)?;
    let (state_used, state_names) =
        resolve_probe_state(&solve_model, state_overrides, "--inspect eval --at")?;

    let runtime = rumoca_eval_solve::SolveRuntime::new(&solve_model)
        .map_err(SimulationDiagnosticError::from)?;
    let report = runtime.eval_at(
        t,
        &state_used,
        &solve_model.parameters,
        EVAL_AT_REFRESH_TOL,
        EVAL_AT_REFRESH_MAX_ITERS,
    );
    Ok(EvalAtProbe {
        report,
        state_used,
        state_names,
    })
}

/// Resolve a probe's state vector from named overrides: seed the model's initial
/// state, then apply each `name=value` override by name. Returns
/// `(state_used, state_names)`. An unknown name is an error (tagged with `label`,
/// e.g. `--inspect eval --at` / `--inspect jacobian --at`) that lists the valid
/// state names.
fn resolve_probe_state(
    solve_model: &solve::SolveModel,
    state_overrides: &[(String, f64)],
    label: &str,
) -> Result<(Vec<f64>, Vec<String>), SimulationDiagnosticError> {
    let state_count = solve_model.state_scalar_count();
    let state_names = solve_model.problem.solve_layout.solver_maps.names[..state_count].to_vec();

    let mut state_used = vec![0.0; state_count];
    for (dst, src) in state_used
        .iter_mut()
        .zip(solve_model.initial_y.iter().copied())
    {
        *dst = src;
    }
    for (name, value) in state_overrides {
        let index = state_names
            .iter()
            .position(|state| state == name)
            .ok_or_else(|| {
                SimulationDiagnosticError::Solver(format!(
                    "{label}: `{name}` is not a state of this model; valid states: [{}]",
                    state_names.join(", ")
                ))
            })?;
        state_used[index] = *value;
    }
    Ok((state_used, state_names))
}

/// Result of a [`jacobian_for_dae`] probe: the named dense state Jacobian plus
/// the state vector it was evaluated at.
#[derive(Debug, Clone)]
pub struct JacobianProbe {
    /// Named dense state Jacobian with singular-column / zero-pivot flags.
    pub report: rumoca_eval_solve::JacobianReport,
    /// State vector used, in model state order.
    pub state_used: Vec<f64>,
    /// State names in model order, aligned with `state_used`.
    pub state_names: Vec<String>,
}

/// Lower `dae_model` and assemble the dense state Jacobian
/// `∂(der(state))/∂(state)` at `(state, t)` by finite difference, naming every
/// row/column (by qualified name) and flagging structurally-singular columns and zero
/// pivots. States are addressed by name (`state_overrides`); unset states keep
/// their model initial value. Backs the `rumoca sim --inspect jacobian` debug dump.
pub fn jacobian_for_dae(
    dae_model: &dae::Dae,
    opts: &SimOptions,
    state_overrides: &[(String, f64)],
    t: f64,
) -> Result<JacobianProbe, SimulationDiagnosticError> {
    let solve_model = lower_dae_for_simulation(dae_model, opts)
        .map_err(SimulationDiagnosticError::SolveLowering)?;
    let (state_used, state_names) =
        resolve_probe_state(&solve_model, state_overrides, "--inspect jacobian --at")?;

    let runtime = rumoca_eval_solve::SolveRuntime::new(&solve_model)
        .map_err(SimulationDiagnosticError::from)?;
    let report = runtime.eval_state_jacobian(
        t,
        &state_used,
        &solve_model.parameters,
        rumoca_eval_solve::AlgebraicSettle {
            tol: EVAL_AT_REFRESH_TOL,
            max_iters: EVAL_AT_REFRESH_MAX_ITERS,
        },
    );
    Ok(JacobianProbe {
        report,
        state_used,
        state_names,
    })
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct SolveLoweringTimings {
    pub structural_dae_seconds: f64,
    pub solve_ir_seconds: f64,
}

impl SolveLoweringTimings {
    #[cfg(feature = "solver-diffsol")]
    pub(crate) fn total_seconds(self) -> f64 {
        self.structural_dae_seconds + self.solve_ir_seconds
    }
}

pub(crate) fn lower_dae_for_simulation_with_stage_timing(
    dae_model: &dae::Dae,
    opts: &SimOptions,
    mut begin_stage: impl FnMut(&'static str),
) -> Result<(solve::SolveModel, SolveLoweringTimings), rumoca_phase_solve::SolveModelLowerError> {
    let mut timings = SolveLoweringTimings::default();

    begin_stage("ir_solve_structural_dae");
    let structural_start = stage_timer_start();
    let structurally_lowered = structurally_lower_dae_for_simulation(dae_model, opts)?;
    timings.structural_dae_seconds = stage_timer_elapsed_seconds(structural_start);

    begin_stage("ir_solve");
    log_solve_lowering_start("solve_ir.lower_dae_to_solve_model");
    let solve_ir_start = stage_timer_start();
    let solve_model =
        rumoca_phase_solve::lower_dae_to_solve_model_owned_with_visible_expressions_and_metadata(
            structurally_lowered.dae,
            structurally_lowered.visible_expressions,
            &structurally_lowered.metadata_dae,
        )?;
    timings.solve_ir_seconds = stage_timer_elapsed_seconds(solve_ir_start);
    log_solve_lowering_done("solve_ir.lower_dae_to_solve_model", solve_ir_start);
    if tracing::enabled!(target: "rumoca_phase_structural", tracing::Level::DEBUG) {
        let layout = &solve_model.problem.layout;
        let mut names_by_y: std::collections::HashMap<usize, &str> =
            std::collections::HashMap::new();
        for (name, slot) in layout.bindings() {
            if let solve::ScalarSlot::Y { index, .. } = slot {
                names_by_y.insert(*index, name.as_str());
            }
        }
        for (row, target) in solve_model
            .problem
            .continuous
            .implicit_row_targets
            .iter()
            .enumerate()
        {
            let label = match target {
                Some(solve::ScalarSlot::Y { index, .. }) => format!(
                    "Y[{index}] {}",
                    names_by_y.get(&{ *index }).copied().unwrap_or("?")
                ),
                Some(other) => format!("{other:?}"),
                None => "RESIDUAL-ONLY".to_string(),
            };
            tracing::debug!(
                target: "rumoca_phase_structural",
                "[sim-trace] solve row {row} -> {label}"
            );
        }
        for (block_idx, block) in solve_model
            .problem
            .continuous
            .algebraic_projection_plan
            .blocks
            .iter()
            .enumerate()
        {
            tracing::debug!(
                target: "rumoca_phase_structural",
                "[sim-trace] projection block {block_idx}: rows={:?} y_indices={:?} ({}) causal_steps={}",
                block.rows,
                block.y_indices,
                block
                    .y_indices
                    .iter()
                    .map(|idx| names_by_y.get(idx).copied().unwrap_or("?"))
                    .collect::<Vec<_>>()
                    .join(", "),
                block.causal_steps.len()
            );
        }
    }
    Ok((solve_model, timings))
}

pub fn structurally_lowered_dae_for_simulation_artifact(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<dae::Dae, rumoca_phase_solve::SolveModelLowerError> {
    structurally_lower_dae_for_simulation(dae_model, opts).map(|lowered| lowered.dae)
}

/// Structurally analyze the model and return a named report of the matching,
/// BLT blocks, coupled SCCs, and tearing. The analysis runs on the flattened
/// DAE *before* scalar-block elimination/tearing collapses the system, so the
/// coupled algebraic loops (e.g. "3 derivatives form one coupled block") and how
/// the compiler would tear them are visible — that elimination is exactly what
/// hides them in the final lowered artifact. Uses the same matching / BLT /
/// tearing algorithms the compiler uses. Backs the `rumoca sim --structure`
/// debug dump.
pub fn structural_report_for_dae(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<rumoca_phase_structural::StructuralReport, SimulationDiagnosticError> {
    // Report on the same prepared system the simulator matches, so the analysis
    // reflects reality (e.g. `der(x)` references in non-ODE rows are resolved).
    let mut prepared = dae_model.clone();
    prepare_dae_for_structural_analysis(&mut prepared, opts)
        .map_err(SimulationDiagnosticError::SolveLowering)?;
    rumoca_phase_structural::build_structural_report(&prepared).map_err(|error| {
        SimulationDiagnosticError::Solver(format!("structural analysis failed: {error}"))
    })
}

/// Shared structural preparation run before both the simulation lowering and the
/// `--inspect structure` report: scalarize (when requested), demote pseudo-states
/// and reduce index, eliminate derivative aliases, and rewrite standalone
/// `der(state)` references in non-ODE rows (`y = der(x)` → `y = <x's ODE rhs>`).
///
/// Keeping this in one place ensures the structural report and the simulator
/// agree on the matched system, and that fixes apply to both paths at once.
pub(crate) fn prepare_dae_for_structural_analysis(
    lowered: &mut dae::Dae,
    opts: &SimOptions,
) -> Result<(), rumoca_phase_solve::SolveModelLowerError> {
    if opts.scalarize {
        log_solve_lowering_start("prepare.scalarize_equations");
        let timer = stage_timer_start();
        rumoca_phase_structural::scalarize::scalarize_equations(lowered)
            .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
        log_solve_lowering_done("prepare.scalarize_equations", timer);
    }
    log_solve_lowering_start("prepare.demote_exact_alias_component_states");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::demote_exact_alias_component_states(lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("prepare.demote_exact_alias_component_states", timer);
    log_solve_lowering_start("prepare.demote_direct_assigned_states");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::demote_direct_assigned_states(lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("prepare.demote_direct_assigned_states", timer);
    log_solve_lowering_start("prepare.reduce_constrained_dummy_derivatives");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::reduce_constrained_dummy_derivatives(lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("prepare.reduce_constrained_dummy_derivatives", timer);
    log_solve_lowering_start("prepare.index_reduce_missing_state_derivatives");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::index_reduce_missing_state_derivatives(lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("prepare.index_reduce_missing_state_derivatives", timer);
    log_solve_lowering_start("prepare.demote_states_without_assignable_derivative_rows");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::demote_states_without_assignable_derivative_rows(lowered);
    log_solve_lowering_done(
        "prepare.demote_states_without_assignable_derivative_rows",
        timer,
    );
    log_solve_lowering_start("prepare.eliminate_derivative_aliases");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::eliminate_derivative_aliases(lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("prepare.eliminate_derivative_aliases", timer);
    log_solve_lowering_start("prepare.demote_states_without_retained_derivative_rows");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::demote_states_without_retained_derivative_rows(lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done(
        "prepare.demote_states_without_retained_derivative_rows",
        timer,
    );
    // After demotion, any `der(<algebraic>)` (a differentiated algebraic such as
    // `a_rel = der(w_rel)`, or successive `Der` blocks) is expanded symbolically
    // via the chain rule, leaving only `der(state)`. Running this after demotion
    // is essential: a `der`'d algebraic with its own defining equation is first
    // demoted from a spurious state, then its derivative is expanded here rather
    // than left as an orphan column (which the matcher reports as singular).
    log_solve_lowering_start("prepare.expand_compound_derivatives");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::expand_compound_derivatives(lowered);
    log_solve_lowering_done("prepare.expand_compound_derivatives", timer);
    // Rewrite `y = der(x)` (e.g. a `Modelica.Blocks.Continuous.Der` block reading
    // a state derivative) into `y = <x's ODE rhs>` so `y` is matchable. Without
    // this, the standalone `der(x)` reference in a non-ODE row has no column to
    // match and the system reports a spurious structural singularity.
    log_solve_lowering_start("prepare.substitute_standalone_state_derivatives_in_non_ode_rows");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::substitute_standalone_state_derivatives_in_non_ode_rows(
        lowered,
    );
    log_solve_lowering_done(
        "prepare.substitute_standalone_state_derivatives_in_non_ode_rows",
        timer,
    );
    if tracing::enabled!(target: "rumoca_phase_structural", tracing::Level::DEBUG) {
        for (index, eq) in lowered.continuous.equations.iter().enumerate() {
            let summary = format!("{}{}", equation_lhs_prefix(eq), debug_render_expr(&eq.rhs));
            tracing::debug!(
                target: "rumoca_phase_structural",
                "[sim-trace] prepared f_x[{index}] origin='{}' {}",
                eq.origin,
                summary
            );
        }
    }
    Ok(())
}

/// Structured triage of a structural singularity, so failures can be classified
/// without hand-reading the DAE: for each unmatched unknown it records a likely
/// root-cause category and the `f_x` rows that reference it.
#[derive(Debug, Clone)]
pub struct SingularityDiagnosis {
    pub n_equations: usize,
    pub n_unknowns: usize,
    pub n_matched: usize,
    pub unknowns: Vec<UnmatchedUnknownDiagnosis>,
    pub equations: Vec<UnmatchedEquationDiagnosis>,
}

/// One unmatched equation from a [`SingularityDiagnosis`].
#[derive(Debug, Clone)]
pub struct UnmatchedEquationDiagnosis {
    pub name: String,
    pub origin: String,
    pub summary: String,
}

/// One unmatched unknown from a [`SingularityDiagnosis`].
#[derive(Debug, Clone)]
pub struct UnmatchedUnknownDiagnosis {
    pub name: String,
    pub category: String,
    pub referencing_rows: Vec<usize>,
}

/// Run the shared structural prep + matching; if the prepared system is
/// singular, return a per-unmatched-unknown diagnosis (`None` when it matches).
/// This powers `--inspect structure`'s automated failure explanation so the
/// development cycle does not require hand-reading DAE dumps.
pub fn diagnose_structural_singularity(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<Option<SingularityDiagnosis>, SimulationDiagnosticError> {
    let mut prepared = dae_model.clone();
    prepare_dae_for_structural_analysis(&mut prepared, opts)
        .map_err(SimulationDiagnosticError::SolveLowering)?;

    let error = match rumoca_phase_structural::build_structural_report(&prepared) {
        Ok(_) => return Ok(None),
        Err(error) => error,
    };
    if tracing::enabled!(target: "rumoca_phase_structural", tracing::Level::DEBUG) {
        for (index, eq) in prepared.continuous.equations.iter().enumerate() {
            let mut summary = format!("{}{:?}", equation_lhs_prefix(eq), eq.rhs);
            summary.truncate(200);
            tracing::debug!(
                target: "rumoca_phase_structural",
                "[sim-trace] prepared f_x[{index}] origin='{}' {}",
                eq.origin,
                summary
            );
        }
    }
    let rumoca_phase_structural::StructuralError::Singular {
        n_equations,
        n_unknowns,
        n_matched,
        unmatched_unknowns,
        unmatched_equations,
        ..
    } = error
    else {
        return Ok(None);
    };

    let unknowns = unmatched_unknowns
        .iter()
        .map(|name| {
            let (category, der_inner) = classify_unmatched_unknown(name, &prepared);
            let referencing_rows = unknown_referencing_rows(&prepared, name, der_inner.as_deref());
            UnmatchedUnknownDiagnosis {
                name: name.clone(),
                category,
                referencing_rows,
            }
        })
        .collect();
    let equations = unmatched_equations
        .iter()
        .map(|name| unmatched_equation_diagnosis(&prepared, name))
        .collect();

    Ok(Some(SingularityDiagnosis {
        n_equations,
        n_unknowns,
        n_matched,
        unknowns,
        equations,
    }))
}

/// Resolve one `f_x[N]` unmatched-equation name to its origin and a truncated
/// expression summary from the prepared system.
fn unmatched_equation_diagnosis(dae: &dae::Dae, name: &str) -> UnmatchedEquationDiagnosis {
    let index = name
        .strip_prefix("f_x[")
        .and_then(|rest| rest.strip_suffix(']'))
        .and_then(|digits| digits.parse::<usize>().ok());
    let equation = index.and_then(|index| dae.continuous.equations.get(index));
    let (origin, summary) = equation.map_or_else(
        || ("<not an f_x row>".to_string(), String::new()),
        |eq| {
            let mut summary = format!("{}{:?}", equation_lhs_prefix(eq), eq.rhs);
            summary.truncate(220);
            (eq.origin.clone(), summary)
        },
    );
    UnmatchedEquationDiagnosis {
        name: name.to_string(),
        origin,
        summary,
    }
}

fn unmatched_base_name(name: &str) -> &str {
    rumoca_core::parse_scalar_name(name)
        .map(|scalar| scalar.base)
        .unwrap_or(name)
}

/// Classify an unmatched unknown by likely root cause. For `der(X)` it also
/// returns the inner name `X` so callers can find the rows that reference it.
fn classify_unmatched_unknown(name: &str, dae: &dae::Dae) -> (String, Option<String>) {
    if let Some(inner) = name
        .strip_prefix("der(")
        .and_then(|rest| rest.strip_suffix(')'))
    {
        let key = rumoca_core::VarName::new(unmatched_base_name(inner));
        let category = if dae.variables.states.contains_key(&key) {
            "der of state (its ODE row should match — check der-equation form)"
        } else if dae.variables.algebraics.contains_key(&key) {
            "der of an ALGEBRAIC (not a state): der-of-der / index reduction needed"
        } else {
            "der of a non-state variable"
        };
        return (category.to_string(), Some(inner.to_string()));
    }
    let category = if name.ends_with(".v") {
        "node potential (electrical/magnetic topology / missing reference)"
    } else if name.ends_with(".tau") || name.ends_with(".f") {
        "connector flow (mechanical: gear/constraint torque or force)"
    } else if name.ends_with(".y2") {
        "inverse-block output (`y2`)"
    } else {
        "algebraic unknown"
    };
    (category.to_string(), None)
}

/// `f_x` row indices that reference the unmatched unknown (the `der(X)` inner
/// name for derivatives, else the plain variable).
fn unknown_referencing_rows(dae: &dae::Dae, name: &str, der_inner: Option<&str>) -> Vec<usize> {
    let key = rumoca_core::VarName::new(der_inner.map_or(name, unmatched_base_name));
    dae.continuous
        .equations
        .iter()
        .enumerate()
        .filter(|(_, eq)| {
            // `lhs` is an optional assigned variable name; `rhs` is the expression.
            if der_inner.is_some() {
                dae::expr_contains_der_of(&eq.rhs, &key)
            } else {
                eq.lhs.as_ref().map(rumoca_core::Reference::var_name) == Some(&key)
                    || dae::expr_contains_var(&eq.rhs, &key)
            }
        })
        .map(|(index, _)| index)
        .collect()
}

#[cfg(not(target_arch = "wasm32"))]
type StageTimer = Instant;

#[cfg(target_arch = "wasm32")]
type StageTimer = ();

#[cfg(not(target_arch = "wasm32"))]
fn stage_timer_start() -> StageTimer {
    Instant::now()
}

#[cfg(target_arch = "wasm32")]
fn stage_timer_start() -> StageTimer {}

#[cfg(not(target_arch = "wasm32"))]
fn stage_timer_elapsed_seconds(start: StageTimer) -> f64 {
    start.elapsed().as_secs_f64()
}

#[cfg(target_arch = "wasm32")]
fn stage_timer_elapsed_seconds(_start: StageTimer) -> f64 {
    0.0
}

fn log_solve_lowering_start(_label: &str) {}

fn log_solve_lowering_done(_label: &str, _start: StageTimer) {}

pub(crate) struct StructurallyLoweredDae {
    dae: dae::Dae,
    metadata_dae: dae::Dae,
    visible_expressions: Vec<rumoca_phase_solve::VisibleExpression>,
}

struct PreparedStructuralDaes {
    source_dae: dae::Dae,
    lowered: dae::Dae,
    metadata_dae: dae::Dae,
}

pub(crate) fn structurally_lower_dae_for_simulation(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<StructurallyLoweredDae, rumoca_phase_solve::SolveModelLowerError> {
    let PreparedStructuralDaes {
        source_dae,
        mut lowered,
        mut metadata_dae,
    } = prepare_structural_daes(dae_model, opts)?;

    log_solve_lowering_start("structural.eliminate_trivial");
    let timer = stage_timer_start();
    let elimination = rumoca_phase_structural::eliminate::eliminate_trivial(&mut lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("structural.eliminate_trivial", timer);
    if let Some(source) = elimination.blt_error {
        if dae_model.variables.states.is_empty() {
            validate_residual_shapes_for_simulation(dae_model)?;
        }
        return Err(rumoca_phase_solve::SolveModelLowerError::Structural { source });
    }

    apply_simulation_elimination(&mut lowered, &elimination.substitutions)?;
    trace_simulation_elimination(&lowered, &elimination.substitutions);
    mark_state_selection_metadata(&mut metadata_dae, &elimination.substitutions)?;
    let visible_expressions =
        visible_expressions_after_elimination(&source_dae, &elimination.substitutions, opts)?;

    Ok(StructurallyLoweredDae {
        dae: lowered,
        metadata_dae,
        visible_expressions,
    })
}

fn prepare_structural_daes(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<PreparedStructuralDaes, rumoca_phase_solve::SolveModelLowerError> {
    log_solve_lowering_start("structural.attach_dae_reference_metadata");
    let timer = stage_timer_start();
    let mut source_dae = dae_model.clone();
    rumoca_phase_dae::attach_dae_reference_metadata(&mut source_dae)
        .map_err(metadata_attachment_lower_error)?;
    log_solve_lowering_done("structural.attach_dae_reference_metadata", timer);
    log_solve_lowering_start("structural.clone_source_for_lowered");
    let timer = stage_timer_start();
    let mut lowered = source_dae.clone();
    log_solve_lowering_done("structural.clone_source_for_lowered", timer);
    prepare_dae_for_structural_analysis(&mut lowered, opts)?;
    log_solve_lowering_start("structural.remove_duplicate_continuous_equations");
    let timer = stage_timer_start();
    remove_duplicate_continuous_equations(&mut lowered);
    log_solve_lowering_done("structural.remove_duplicate_continuous_equations", timer);
    log_solve_lowering_start("structural.clone_metadata_dae");
    let timer = stage_timer_start();
    let metadata_dae = lowered.clone();
    log_solve_lowering_done("structural.clone_metadata_dae", timer);

    Ok(PreparedStructuralDaes {
        source_dae,
        lowered,
        metadata_dae,
    })
}

fn apply_simulation_elimination(
    lowered: &mut dae::Dae,
    substitutions: &[rumoca_phase_structural::eliminate::Substitution],
) -> Result<(), rumoca_phase_solve::SolveModelLowerError> {
    log_solve_lowering_start("structural.demote_states_without_retained_derivative_rows");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::demote_states_without_retained_derivative_rows(lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done(
        "structural.demote_states_without_retained_derivative_rows",
        timer,
    );
    log_solve_lowering_start("structural.apply_elimination_substitutions_to_dae");
    let timer = stage_timer_start();
    rumoca_phase_structural::eliminate::apply_elimination_substitutions_to_dae(
        lowered,
        substitutions,
    )
    .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("structural.apply_elimination_substitutions_to_dae", timer);
    Ok(())
}

fn trace_simulation_elimination(
    lowered: &dae::Dae,
    substitutions: &[rumoca_phase_structural::eliminate::Substitution],
) {
    if tracing::enabled!(target: "rumoca_phase_structural", tracing::Level::DEBUG) {
        for sub in substitutions {
            tracing::debug!(
                target: "rumoca_phase_structural",
                "[sim-trace] substitution {} := {}",
                sub.var_name.as_str(),
                debug_render_expr(&sub.expr)
            );
        }
        for (index, eq) in lowered.continuous.equations.iter().enumerate() {
            tracing::debug!(
                target: "rumoca_phase_structural",
                "[sim-trace] post-elim f_x[{index}] origin='{}' {}{}",
                eq.origin,
                equation_lhs_prefix(eq),
                debug_render_expr(&eq.rhs)
            );
        }
    }
}

fn mark_state_selection_metadata(
    metadata_dae: &mut dae::Dae,
    substitutions: &[rumoca_phase_structural::eliminate::Substitution],
) -> Result<(), rumoca_phase_solve::SolveModelLowerError> {
    log_solve_lowering_start("structural.clone_state_selection_dae");
    let timer = stage_timer_start();
    let mut state_selection_dae = metadata_dae.clone();
    log_solve_lowering_done("structural.clone_state_selection_dae", timer);
    log_solve_lowering_start("structural.apply_state_selection_substitutions");
    let timer = stage_timer_start();
    rumoca_phase_structural::eliminate::apply_elimination_substitutions_to_dae(
        &mut state_selection_dae,
        substitutions,
    )
    .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("structural.apply_state_selection_substitutions", timer);
    log_solve_lowering_start("structural.demote_state_selection_dae");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::demote_states_without_retained_derivative_rows(
        &mut state_selection_dae,
    )
    .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("structural.demote_state_selection_dae", timer);
    log_solve_lowering_start("structural.mark_constrained_dummy_states_in_metadata");
    let timer = stage_timer_start();
    mark_constrained_dummy_states_in_metadata(&state_selection_dae, metadata_dae);
    log_solve_lowering_done(
        "structural.mark_constrained_dummy_states_in_metadata",
        timer,
    );
    Ok(())
}

fn visible_expressions_after_elimination(
    source_dae: &dae::Dae,
    substitutions: &[rumoca_phase_structural::eliminate::Substitution],
    opts: &SimOptions,
) -> Result<Vec<rumoca_phase_solve::VisibleExpression>, rumoca_phase_solve::SolveModelLowerError> {
    log_solve_lowering_start("structural.clone_observation_dae");
    let timer = stage_timer_start();
    let mut observation_dae = source_dae.clone();
    log_solve_lowering_done("structural.clone_observation_dae", timer);
    if opts.scalarize {
        log_solve_lowering_start("structural.scalarize_observation_dae");
        let timer = stage_timer_start();
        rumoca_phase_structural::scalarize::scalarize_equations(&mut observation_dae)
            .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
        log_solve_lowering_done("structural.scalarize_observation_dae", timer);
    }
    if !substitutions.is_empty() {
        log_solve_lowering_start("structural.resolve_observation_substitutions");
        let timer = stage_timer_start();
        for eq in &mut observation_dae.continuous.equations {
            eq.rhs = rumoca_phase_structural::eliminate::resolve_substitutions_in_expr(
                &eq.rhs,
                substitutions,
            )
            .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
        }
        log_solve_lowering_done("structural.resolve_observation_substitutions", timer);
    }
    log_solve_lowering_start("structural.visible_expressions_for_dae");
    let timer = stage_timer_start();
    let mut visible_expressions = rumoca_phase_solve::visible_expressions_for_dae(&observation_dae)
        .map_err(rumoca_phase_solve::SolveModelLowerError::Lower)?;
    log_solve_lowering_done("structural.visible_expressions_for_dae", timer);
    if !substitutions.is_empty() {
        log_solve_lowering_start("structural.resolve_visible_expression_substitutions");
        let timer = stage_timer_start();
        for visible in &mut visible_expressions {
            visible.expr = rumoca_phase_structural::eliminate::resolve_substitutions_in_expr(
                &visible.expr,
                substitutions,
            )
            .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
        }
        log_solve_lowering_done("structural.resolve_visible_expression_substitutions", timer);
    }
    Ok(visible_expressions)
}

fn metadata_attachment_lower_error(
    err: rumoca_phase_dae::ToDaeError,
) -> rumoca_phase_solve::SolveModelLowerError {
    let reason = format!("DAE reference metadata attachment failed: {err}");
    rumoca_phase_solve::SolveModelLowerError::Lower(lower_contract_error_from_optional_span(
        reason,
        err.source_span(),
    ))
}

fn lower_contract_error_from_optional_span(
    reason: String,
    span: Option<rumoca_core::Span>,
) -> rumoca_phase_solve::lower::LowerError {
    match span {
        Some(span) if !span.is_dummy() => {
            rumoca_phase_solve::lower::LowerError::ContractViolation { reason, span }
        }
        Some(_) | None => {
            rumoca_phase_solve::lower::LowerError::UnspannedContractViolation { reason }
        }
    }
}

fn remove_duplicate_continuous_equations(dae: &mut dae::Dae) {
    let mut unique = Vec::with_capacity(dae.continuous.equations.len());
    let mut keys = Vec::with_capacity(dae.continuous.equations.len());
    for eq in dae.continuous.equations.drain(..) {
        let key = duplicate_equation_key(&eq);
        if keys.contains(&key) {
            continue;
        }
        keys.push(key);
        unique.push(eq);
    }
    dae.continuous.equations = unique;
}

fn duplicate_equation_key(eq: &dae::Equation) -> String {
    if let Some((state_name, rhs)) = derivative_residual_signature(&eq.rhs) {
        return format!("der:{state_name}:{}", expression_key(rhs));
    }
    format!("lhs:{:?}:rhs:{}", eq.lhs, expression_key(&eq.rhs))
}

fn derivative_residual_signature(
    expr: &rumoca_core::Expression,
) -> Option<(String, &rumoca_core::Expression)> {
    let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = expr else {
        return None;
    };
    if !matches!(op, rumoca_core::OpBinary::Sub) {
        return None;
    }
    derivative_target_name(lhs).map(|target| (target, rhs.as_ref()))
}

fn derivative_target_name(expr: &rumoca_core::Expression) -> Option<String> {
    let rumoca_core::Expression::BuiltinCall { function, args, .. } = expr else {
        return None;
    };
    if *function != rumoca_core::BuiltinFunction::Der {
        return None;
    }
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = args.first()?
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    Some(name.as_str().to_string())
}

fn expression_key(expr: &rumoca_core::Expression) -> String {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => format!("var:{}:{subscripts:?}", name.as_str()),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            format!("bin:{op:?}:{}:{}", expression_key(lhs), expression_key(rhs))
        }
        rumoca_core::Expression::Unary { op, rhs, .. } => {
            format!("un:{op:?}:{}", expression_key(rhs))
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. } => format!(
            "builtin:{function:?}:{}",
            args.iter()
                .map(expression_key)
                .collect::<Vec<_>>()
                .join(",")
        ),
        _ => format!("{expr:?}"),
    }
}

fn validate_residual_shapes_for_simulation(
    dae_model: &dae::Dae,
) -> Result<(), rumoca_phase_solve::SolveModelLowerError> {
    let layout = rumoca_phase_solve::build_var_layout(dae_model)?;
    rumoca_phase_solve::lower::lower_residual(dae_model, &layout)?;
    Ok(())
}

fn mark_constrained_dummy_states_in_metadata(
    structural_dae: &dae::Dae,
    metadata_dae: &mut dae::Dae,
) {
    for state_name in
        rumoca_phase_structural::dae_prepare::constrained_dummy_state_names(structural_dae)
    {
        let name = rumoca_core::VarName::new(state_name);
        if let Some(var) = metadata_dae.variables.states.shift_remove(&name) {
            metadata_dae.variables.algebraics.insert(name, var);
        }
    }
}

fn equation_lhs_prefix(eq: &dae::Equation) -> String {
    match eq.lhs.as_ref() {
        Some(lhs) => format!("{} = ", lhs.as_str()),
        None => String::new(),
    }
}

/// Compact Modelica-ish rendering for `--trace` diagnostics only.
pub(crate) fn debug_render_expr(expr: &rumoca_core::Expression) -> String {
    use rumoca_core::Expression as E;
    match expr {
        E::Literal { value, .. } => format!("{value:?}"),
        E::VarRef {
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                name.as_str().to_string()
            } else {
                format!("{}[{} subs]", name.as_str(), subscripts.len())
            }
        }
        E::Binary { op, lhs, rhs, .. } => format!(
            "({} {op:?} {})",
            debug_render_expr(lhs),
            debug_render_expr(rhs)
        ),
        E::Unary { op, rhs, .. } => format!("({op:?} {})", debug_render_expr(rhs)),
        E::BuiltinCall { function, args, .. } => format!(
            "{function:?}({})",
            args.iter()
                .map(debug_render_expr)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        E::FunctionCall { name, args, .. } => format!(
            "{}({})",
            name.as_str(),
            args.iter()
                .map(debug_render_expr)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        E::If {
            branches,
            else_branch,
            ..
        } => format!(
            "if({} branches, else {})",
            branches.len(),
            debug_render_expr(else_branch)
        ),
        other => format!("<{}>", std::any::type_name_of_val(other)),
    }
}

#[cfg(test)]
mod tests {
    use rumoca_core::{BuiltinFunction, Expression, OpBinary, SourceId, Span, Subscript, VarName};

    use super::*;

    #[test]
    fn simulation_structural_lowering_keeps_observations_for_torn_variables() {
        let dae = symbolic_loop_dae();
        let model = lower_dae_for_simulation(&dae, &SimOptions::default())
            .expect("torn loop should lower to solve IR");

        assert_eq!(model.visible_names, ["a", "b", "c"]);
        assert_eq!(model.visible_value_rows.len(), model.visible_names.len());
        assert_eq!(model.problem.solve_layout.solver_maps.names.len(), 1);
    }

    #[test]
    fn simulation_structural_lowering_reports_blt_singularity() {
        let mut dae = dae::Dae::new();
        dae.variables.algebraics.insert(
            VarName::new("a"),
            dae::Variable::new(
                VarName::new("a"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
        dae.variables.algebraics.insert(
            VarName::new("b"),
            dae::Variable::new(
                VarName::new("b"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
        dae.continuous.equations.push(dae::Equation {
            lhs: None,
            rhs: Expression::Binary {
                op: OpBinary::Add,
                lhs: Box::new(var("a")),
                rhs: Box::new(var("b")),
                span: fixture_span(),
            },
            span: fixture_span(),
            origin: "singular test".to_string(),
            scalar_count: 1,
        });

        let mut dae = dae;
        rumoca_phase_dae::attach_dae_reference_metadata(&mut dae)
            .expect("fixture DAE reference metadata should normalize");
        let err = lower_dae_for_simulation(&dae, &SimOptions::default())
            .expect_err("BLT singularity must not be silently skipped");

        assert!(
            err.to_string().contains("structural lowering failed"),
            "got: {err}"
        );
        assert!(err.to_string().contains("structurally singular system"));
    }

    #[test]
    fn metadata_attachment_lower_error_preserves_dae_source_span() {
        let span = Span::from_offsets(SourceId(9), 21, 34);
        let err = metadata_attachment_lower_error(
            rumoca_phase_dae::ToDaeError::runtime_metadata_violation_at(
                "missing reference metadata",
                span,
            ),
        );

        assert_eq!(err.source_span(), Some(span));
        assert!(
            matches!(
                err,
                rumoca_phase_solve::SolveModelLowerError::Lower(
                    rumoca_phase_solve::lower::LowerError::ContractViolation {
                        span: actual,
                        ..
                    }
                ) if actual == span
            ),
            "metadata attachment error should preserve the DAE error span"
        );
    }

    #[test]
    fn metadata_attachment_lower_error_keeps_unspanned_dae_error_unspanned() {
        let err = metadata_attachment_lower_error(
            rumoca_phase_dae::ToDaeError::runtime_metadata_violation("metadata-only corruption"),
        );

        assert_eq!(err.source_span(), None);
        assert!(
            matches!(
                err,
                rumoca_phase_solve::SolveModelLowerError::Lower(
                    rumoca_phase_solve::lower::LowerError::UnspannedContractViolation { .. }
                )
            ),
            "metadata-only error must not receive fabricated provenance"
        );
    }

    #[test]
    fn simulation_structural_singularity_carries_unmatched_variable_span() {
        let span = Span::from_offsets(rumoca_core::SourceId(7), 100, 110);
        let mut dae = dae::Dae::new();
        for name in ["a", "b"] {
            dae.variables.algebraics.insert(
                VarName::new(name),
                dae::Variable {
                    source_span: span,
                    ..dae::Variable::new(
                        VarName::new(name),
                        rumoca_core::Span::from_offsets(
                            rumoca_core::SourceId::from_source_name(file!()),
                            1,
                            2,
                        ),
                    )
                },
            );
        }
        // One equation (`0 = a + b`), two unknowns -> structurally singular
        // (an additive constraint cannot be alias-eliminated).
        dae.continuous.equations.push(eq(Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(var("a")),
            rhs: Box::new(var("b")),
            span: fixture_span(),
        }));

        let mut dae = dae;
        rumoca_phase_dae::attach_dae_reference_metadata(&mut dae)
            .expect("fixture DAE reference metadata should normalize");
        let err = lower_dae_for_simulation(&dae, &SimOptions::default())
            .expect_err("singular system should error");
        assert_eq!(
            err.source_span(),
            Some(span),
            "structural singularity should carry the unmatched variable span: {err:?}"
        );
    }

    #[test]
    fn simulation_structural_lowering_demotes_unresolved_derivative_alias_state() {
        let mut dae = derivative_alias_state_dae();
        rumoca_phase_dae::attach_dae_reference_metadata(&mut dae)
            .expect("fixture DAE reference metadata should normalize");
        let model = lower_dae_for_simulation(&dae, &SimOptions::default())
            .expect("derivative alias state should lower without an underdetermined solver slot");

        assert_eq!(model.state_scalar_count(), 0);
        assert!(
            !model
                .problem
                .solve_layout
                .solver_maps
                .names
                .contains(&"dx".to_string())
        );
    }

    #[test]
    fn simulation_structural_lowering_keeps_cross_coupled_ode_states() {
        let dae = oscillator_dae();
        let model = lower_dae_for_simulation(&dae, &SimOptions::default())
            .expect("cross-coupled ODE states should lower");

        assert_eq!(model.state_scalar_count(), 2);
        assert_eq!(model.problem.solve_layout.solver_maps.names, ["x", "v"]);
    }

    #[test]
    fn simulation_structural_lowering_demotes_vector_state_with_only_alias_rows() {
        let dae = vector_alias_state_dae();
        let lowered = structurally_lower_dae_for_simulation(&dae, &SimOptions::default())
            .expect("vector alias state should structurally lower");

        assert!(
            !lowered
                .dae
                .variables
                .states
                .contains_key(&VarName::new("imc.is")),
            "vector alias state without retained derivative rows should be demoted"
        );
        assert!(
            lowered
                .dae
                .variables
                .algebraics
                .contains_key(&VarName::new("imc.is"))
        );
    }

    #[test]
    fn simulation_structural_lowering_differentiates_vector_function_constraint_for_coupled_state()
    {
        let dae = quaternion_constraint_dae();
        let model = lower_dae_for_simulation(&dae, &SimOptions::default())
            .expect("vector function constraint should provide the missing coupled state row");

        assert_eq!(model.state_scalar_count(), 4);
        assert!(["Q[1]", "Q[2]", "Q[3]", "Q[4]"].iter().all(|name| {
            model
                .problem
                .solve_layout
                .solver_maps
                .names
                .contains(&name.to_string())
        }));
    }

    #[test]
    fn simulation_structural_lowering_reports_state_metadata_before_elimination() {
        let dae = exact_alias_state_dae();
        let model = lower_dae_for_simulation(&dae, &SimOptions::default())
            .expect("exact alias state model should lower");

        let x_meta = model
            .variable_meta
            .iter()
            .find(|meta| meta.name == "x")
            .expect("x should remain visible");
        let y_meta = model
            .variable_meta
            .iter()
            .find(|meta| meta.name == "y")
            .expect("y should remain visible");

        let selected_count = usize::from(x_meta.is_state) + usize::from(y_meta.is_state);
        assert_eq!(
            selected_count, 1,
            "exact alias component should report one selected state"
        );
    }

    #[test]
    fn simulation_metadata_reports_constrained_state_as_unselected() {
        let dae = constrained_state_dae();
        let model = lower_dae_for_simulation(&dae, &SimOptions::default())
            .expect("direct constrained state model should lower");

        let x1_meta = model
            .variable_meta
            .iter()
            .find(|meta| meta.name == "x1")
            .expect("x1 should remain visible");
        let x2_meta = model
            .variable_meta
            .iter()
            .find(|meta| meta.name == "x2")
            .expect("x2 should remain visible");
        let x3_meta = model
            .variable_meta
            .iter()
            .find(|meta| meta.name == "x3")
            .expect("x3 should remain visible");

        assert_eq!(model.state_scalar_count(), 1);
        assert!(!x1_meta.is_state);
        assert_eq!(x1_meta.role, "algebraic");
        assert!(x2_meta.is_state);
        assert!(!x3_meta.is_state);
        assert_eq!(x3_meta.role, "algebraic");
    }

    #[test]
    fn simulation_lowering_preserves_source_span_for_shape_errors() {
        let mut model = dae::Dae::new();
        model.variables.algebraics.insert(
            VarName::new("A"),
            dae::Variable {
                dims: vec![3, 3],
                ..dae::Variable::new(
                    VarName::new("A"),
                    rumoca_core::Span::from_offsets(
                        rumoca_core::SourceId::from_source_name(file!()),
                        1,
                        2,
                    ),
                )
            },
        );
        model.variables.algebraics.insert(
            VarName::new("b"),
            dae::Variable {
                dims: vec![2],
                ..dae::Variable::new(
                    VarName::new("b"),
                    rumoca_core::Span::from_offsets(
                        rumoca_core::SourceId::from_source_name(file!()),
                        1,
                        2,
                    ),
                )
            },
        );
        let span = Span::from_offsets(rumoca_core::SourceId(4), 40, 45);
        let rhs = sub(var("A").with_span(span), var("b").with_span(span));
        model.continuous.equations.push(dae::Equation {
            lhs: None,
            rhs,
            span,
            origin: "shape mismatch".to_string(),
            scalar_count: 9,
        });

        let mut model = model;
        rumoca_phase_dae::attach_dae_reference_metadata(&mut model)
            .expect("fixture DAE reference metadata should normalize");
        let err = lower_dae_for_simulation(&model, &SimOptions::default())
            .expect_err("shape mismatch should fail during simulation lowering");
        assert_eq!(err.source_span(), Some(span), "unexpected error: {err:?}");
        let diagnostic = SimulationDiagnosticError::SolveLowering(err);
        assert_eq!(diagnostic.diagnostic_code(), "lowering");
        assert_eq!(
            diagnostic.diagnostic_label(),
            "array operands have incompatible shapes [3, 3] and [2]"
        );
    }

    #[test]
    fn simulation_diagnostic_preserves_runtime_preparation_span() {
        let span = Span::from_offsets(rumoca_core::SourceId(8), 12, 18);
        let error = rumoca_eval_solve::EvalSolveError::Scalarization {
            message: "invalid native map metadata".to_string(),
            span: Some(span),
        };
        let diagnostic = SimulationDiagnosticError::from(error);

        assert_eq!(diagnostic.diagnostic_code(), "simulation");
        assert_eq!(diagnostic.source_span(), Some(span));
        assert_eq!(
            diagnostic.to_string(),
            "Solve-IR scalarization failed: invalid native map metadata"
        );
    }

    fn symbolic_loop_dae() -> dae::Dae {
        let mut model = dae::Dae::new();
        for name in ["a", "b", "c"] {
            model.variables.algebraics.insert(
                VarName::new(name),
                dae::Variable::new(
                    VarName::new(name),
                    rumoca_core::Span::from_offsets(
                        rumoca_core::SourceId::from_source_name(file!()),
                        1,
                        2,
                    ),
                ),
            );
        }
        model.continuous.equations.push(eq(sub(var("a"), var("b"))));
        model.continuous.equations.push(eq(sub(var("b"), var("c"))));
        model.continuous.equations.push(eq(sub(
            var("c"),
            Expression::BuiltinCall {
                function: BuiltinFunction::Sin,
                args: vec![var("a")],
                span: fixture_span(),
            },
        )));
        model
    }

    fn derivative_alias_state_dae() -> dae::Dae {
        let mut model = dae::Dae::new();
        model.variables.states.insert(
            VarName::new("x"),
            dae::Variable::new(
                VarName::new("x"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
        model.variables.algebraics.insert(
            VarName::new("dx"),
            dae::Variable::new(
                VarName::new("dx"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
        model.continuous.equations.push(eq(sub(var("x"), time())));
        model
            .continuous
            .equations
            .push(eq(sub(var("dx"), der(var("x")))));
        model
    }

    fn oscillator_dae() -> dae::Dae {
        let mut model = dae::Dae::new();
        for name in ["x", "v"] {
            model.variables.states.insert(
                VarName::new(name),
                dae::Variable::new(
                    VarName::new(name),
                    rumoca_core::Span::from_offsets(
                        rumoca_core::SourceId::from_source_name(file!()),
                        1,
                        2,
                    ),
                ),
            );
        }
        model
            .continuous
            .equations
            .push(eq(sub(der(var("x")), var("v"))));
        model.continuous.equations.push(eq(sub(
            der(var("v")),
            Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs: Box::new(var("x")),
                span: fixture_span(),
            },
        )));
        model
    }

    fn exact_alias_state_dae() -> dae::Dae {
        let mut model = dae::Dae::new();
        for name in ["x", "y"] {
            model.variables.states.insert(
                VarName::new(name),
                dae::Variable::new(
                    VarName::new(name),
                    rumoca_core::Span::from_offsets(
                        rumoca_core::SourceId::from_source_name(file!()),
                        1,
                        2,
                    ),
                ),
            );
        }
        model.variables.algebraics.insert(
            VarName::new("a"),
            dae::Variable::new(
                VarName::new("a"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
        model.continuous.equations.push(eq(sub(var("x"), var("a"))));
        model.continuous.equations.push(eq(sub(var("y"), var("a"))));
        model
            .continuous
            .equations
            .push(eq(sub(der(var("x")), time())));
        model
            .continuous
            .equations
            .push(eq(sub(der(var("y")), time())));
        model
    }

    fn vector_alias_state_dae() -> dae::Dae {
        let mut model = dae::Dae::new();
        model.variables.states.insert(
            VarName::new("imc.is"),
            dae::Variable {
                dims: vec![3],
                ..dae::Variable::new(
                    VarName::new("imc.is"),
                    rumoca_core::Span::from_offsets(
                        rumoca_core::SourceId::from_source_name(file!()),
                        1,
                        2,
                    ),
                )
            },
        );
        model.variables.states.insert(
            VarName::new("x"),
            dae::Variable::new(
                VarName::new("x"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
        for idx in 1..=3 {
            model.variables.algebraics.insert(
                VarName::new(format!("imc.plug_sp.pin[{idx}].i")),
                dae::Variable::new(
                    VarName::new(format!("imc.plug_sp.pin[{idx}].i")),
                    rumoca_core::Span::from_offsets(
                        rumoca_core::SourceId::from_source_name(file!()),
                        1,
                        2,
                    ),
                ),
            );
            model.continuous.equations.push(eq(sub(
                var_idx("imc.is", idx),
                var(&format!("imc.plug_sp.pin[{idx}].i")),
            )));
        }
        model
            .continuous
            .equations
            .push(eq(sub(der(var("x")), time())));
        model
    }

    fn constrained_state_dae() -> dae::Dae {
        let mut model = dae::Dae::new();
        for name in ["x1", "x2", "x3"] {
            model.variables.states.insert(
                VarName::new(name),
                dae::Variable::new(
                    VarName::new(name),
                    rumoca_core::Span::from_offsets(
                        rumoca_core::SourceId::from_source_name(file!()),
                        1,
                        2,
                    ),
                ),
            );
        }
        model.variables.algebraics.insert(
            VarName::new("a"),
            dae::Variable::new(
                VarName::new("a"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );

        model
            .continuous
            .equations
            .push(eq(sub(var("a"), sub(neg(var("x2")), var("x3")))));
        model
            .continuous
            .equations
            .push(eq(sub(var("x1"), var("a"))));
        model
            .continuous
            .equations
            .push(eq(sub(der(var("x1")), time())));
        model
            .continuous
            .equations
            .push(eq(sub(der(var("x2")), time())));
        model
            .continuous
            .equations
            .push(eq(sub(der(var("x3")), time())));
        model
    }

    fn quaternion_constraint_dae() -> dae::Dae {
        let mut model = dae::Dae::new();
        model.variables.states.insert(
            VarName::new("Q"),
            dae::Variable {
                dims: vec![4],
                ..dae::Variable::new(
                    VarName::new("Q"),
                    rumoca_core::Span::from_offsets(
                        rumoca_core::SourceId::from_source_name(file!()),
                        1,
                        2,
                    ),
                )
            },
        );
        model.symbols.functions.insert(
            VarName::new("orientationConstraint"),
            orientation_constraint_function(),
        );
        for idx in 1..=3 {
            model
                .continuous
                .equations
                .push(eq(sub(der(var_idx("Q", idx)), time())));
        }
        model.continuous.equations.push(eq(sub(
            array(vec![int(0)]),
            call("orientationConstraint", vec![var("Q")]),
        )));
        model
    }

    fn orientation_constraint_function() -> rumoca_core::Function {
        let span = fixture_span();
        let mut function = rumoca_core::Function::new("orientationConstraint", span);
        function
            .inputs
            .push(rumoca_core::FunctionParam::new("Q", "Orientation", span));
        let mut output = rumoca_core::FunctionParam::new("residue", "Real", span);
        output.dims = vec![1];
        function.outputs.push(output);
        function.body.push(rumoca_core::Statement::Assignment {
            comp: rumoca_core::ComponentReference {
                local: false,
                span,
                parts: vec![rumoca_core::ComponentRefPart {
                    ident: "residue".to_string(),
                    span,
                    subs: Vec::new(),
                }],
                def_id: None,
            },
            value: array(vec![sub(mul(var("Q"), var("Q")), int(1))]),
            span,
        });
        function
    }

    fn fixture_span() -> Span {
        Span::from_offsets(SourceId(10_001), 1, 2)
    }

    fn eq(rhs: Expression) -> dae::Equation {
        eq_with_scalar_count(rhs, 1)
    }

    fn eq_with_scalar_count(rhs: Expression, scalar_count: usize) -> dae::Equation {
        dae::Equation {
            lhs: None,
            rhs,
            span: fixture_span(),
            origin: "test".to_string(),
            scalar_count,
        }
    }

    fn sub(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: fixture_span(),
        }
    }

    fn mul(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: OpBinary::Mul,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: fixture_span(),
        }
    }

    fn neg(rhs: Expression) -> Expression {
        Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs: Box::new(rhs),
            span: fixture_span(),
        }
    }

    fn array(elements: Vec<Expression>) -> Expression {
        Expression::Array {
            elements,
            is_matrix: false,
            span: fixture_span(),
        }
    }

    fn call(name: &str, args: Vec<Expression>) -> Expression {
        Expression::FunctionCall {
            name: reference(name),
            args,
            is_constructor: false,
            span: fixture_span(),
        }
    }

    fn component_ref(name: &str) -> rumoca_core::ComponentReference {
        let span = fixture_span();
        rumoca_core::ComponentReference {
            local: false,
            span,
            parts: vec![rumoca_core::ComponentRefPart {
                ident: name.to_string(),
                span,
                subs: Vec::new(),
            }],
            def_id: None,
        }
    }

    fn reference(name: &str) -> rumoca_core::Reference {
        rumoca_core::Reference::with_component_reference(name, component_ref(name))
    }

    fn int(value: i64) -> Expression {
        Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            span: fixture_span(),
        }
    }

    fn var(name: &str) -> Expression {
        let span = fixture_span();
        Expression::VarRef {
            name: reference(name),
            subscripts: Vec::new(),
            span,
        }
    }

    fn var_idx(name: &str, idx: i64) -> Expression {
        let span = fixture_span();
        Expression::VarRef {
            name: reference(name),
            subscripts: vec![Subscript::generated_index(idx, span)],
            span,
        }
    }

    fn time() -> Expression {
        var("time")
    }

    fn der(arg: Expression) -> Expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![arg],
            span: fixture_span(),
        }
    }

    fn real(value: f64) -> Expression {
        Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span: fixture_span(),
        }
    }

    fn div(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: OpBinary::Div,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: fixture_span(),
        }
    }

    #[test]
    fn eval_dae_at_names_nonfinite_state_derivative() {
        // der(x) = 1 / y ; der(y) = -1. At y = 0 the first derivative is inf,
        // and the probe must name it so a NaN/inf is one command away.
        let mut dae = dae::Dae::new();
        for name in ["x", "y"] {
            dae.variables.states.insert(
                VarName::new(name),
                dae::Variable::new(
                    VarName::new(name),
                    rumoca_core::Span::from_offsets(
                        rumoca_core::SourceId::from_source_name(file!()),
                        1,
                        2,
                    ),
                ),
            );
        }
        dae.continuous
            .equations
            .push(eq(sub(der(var("x")), div(real(1.0), var("y")))));
        dae.continuous
            .equations
            .push(eq(sub(der(var("y")), real(-1.0))));

        // Override the state `y` by name (positional ordering is never used).
        let probe = eval_dae_at(&dae, &SimOptions::default(), &[("y".to_string(), 0.0)], 0.5)
            .expect("model should lower and evaluate");
        let report = &probe.report;

        assert_eq!(report.state_count, 2);
        assert_eq!(probe.state_names, vec!["x".to_string(), "y".to_string()]);
        let y_index = probe.state_names.iter().position(|s| s == "y").unwrap();
        assert_eq!(probe.state_used[y_index], 0.0);

        let der_x = report
            .derivatives
            .iter()
            .find(|slot| slot.name == "der(x)")
            .expect("der(x) present");
        assert!(!der_x.is_finite(), "der(x)=1/0 should be non-finite");

        assert!(report.has_nonfinite());
        let nonfinite_names: Vec<_> = report
            .nonfinite()
            .map(|(_, slot)| slot.name.clone())
            .collect();
        assert!(
            nonfinite_names.iter().any(|name| name == "der(x)"),
            "non-finite report should name der(x): {nonfinite_names:?}"
        );

        let der_y = report
            .derivatives
            .iter()
            .find(|slot| slot.name == "der(y)")
            .expect("der(y) present");
        assert!(der_y.is_finite(), "der(y) should stay finite");
    }

    #[test]
    fn jacobian_for_dae_assembles_named_matrix_and_flags_zero_pivots() {
        // Oscillator der(x)=v, der(v)=-x -> J = [[0,1],[-1,0]]: both diagonal
        // pivots are zero, no structurally-singular columns.
        let probe = jacobian_for_dae(&oscillator_dae(), &SimOptions::default(), &[], 0.0)
            .expect("oscillator jacobian should assemble");
        let report = &probe.report;

        assert_eq!(report.dim(), 2);
        assert_eq!(probe.state_names, vec!["x".to_string(), "v".to_string()]);
        assert!(
            report.singular_columns().is_empty(),
            "both states affect a derivative"
        );
        assert_eq!(
            report.zero_pivots(),
            vec![0, 1],
            "d(der(x))/dx and d(der(v))/dv are both zero"
        );
        // Off-diagonal structure: d(der(x))/dv = 1, d(der(v))/dx = -1.
        let entries: std::collections::HashMap<(usize, usize), f64> = report
            .nonzero_entries()
            .map(|(r, c, v)| ((r, c), v))
            .collect();
        assert!((entries[&(0, 1)] - 1.0).abs() < 1e-4, "{entries:?}");
        assert!((entries[&(1, 0)] + 1.0).abs() < 1e-4, "{entries:?}");
        assert!(report.error.is_none());
    }

    #[test]
    fn eval_dae_at_rejects_unknown_state_name() {
        let err = eval_dae_at(
            &oscillator_dae(),
            &SimOptions::default(),
            &[("nope".to_string(), 1.0)],
            0.0,
        )
        .expect_err("unknown state name should error");
        let message = err.to_string();
        assert!(message.contains("`nope` is not a state"), "{message}");
        assert!(message.contains('x') && message.contains('v'), "{message}");
    }

    #[test]
    fn eval_dae_at_reports_finite_values_from_initial_state() {
        // No overrides: states keep their model initial value (here 0).
        let probe = eval_dae_at(&oscillator_dae(), &SimOptions::default(), &[], 0.0)
            .expect("oscillator should lower and evaluate");
        let report = &probe.report;

        assert_eq!(report.state_count, 2);
        assert!(!report.has_nonfinite());
        assert!(report.error.is_none());
        // der(x) = v, der(v) = -x; at the zero initial state both are 0.
        let der_x = report
            .derivatives
            .iter()
            .find(|s| s.name == "der(x)")
            .unwrap();
        let der_v = report
            .derivatives
            .iter()
            .find(|s| s.name == "der(v)")
            .unwrap();
        assert_eq!(der_x.value, 0.0);
        assert_eq!(der_v.value, 0.0);
    }
}
