use std::time::Instant;

use crate::equation_scalarize::scalarize_equations;
use crate::runtime::timeout::{TimeoutBudget, run_timeout_step, run_timeout_step_result};
use crate::sim_types::SimError;
use crate::simulation::diagnostics::{dump_missing_state_equation_diagnostics, sim_trace_enabled};
use crate::simulation::equation_reorder::{EquationReorderError, reorder_equations_for_solver};
use crate::simulation::mass_matrix_form::{
    MassMatrixFormError, validate_constant_mass_matrix_form,
};
use crate::simulation::parameter_values::{
    ParameterValueError, default_parameter_values_with_budget,
};
use crate::simulation::pipeline::{MassMatrix, PreparedSimulation, run_logged_phase};
use crate::simulation::runtime_alias_normalization::{
    RuntimeAliasSubstitution,
    apply_runtime_alias_substitutions_to_elimination as core_apply_runtime_alias_substitutions_to_elimination,
    normalize_runtime_aliases_collect as core_normalize_runtime_aliases_collect,
};
use crate::simulation::runtime_prep::{compute_mass_matrix, pin_orphaned_variables};
use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_phase_structural::eliminate;

use crate::simulation::dae_prepare::{
    demote_alias_states_without_der, demote_coupled_derivative_states,
    demote_direct_assigned_states, demote_orphan_states_without_equation_refs,
    demote_states_without_assignable_derivative_rows, demote_states_without_derivative_refs,
    eliminate_derivative_aliases, expand_compound_derivatives,
    index_reduce_missing_state_derivatives, normalize_ode_equation_signs,
    promote_der_algebraics_to_states,
};
use crate::simulation::introspection::trace_flow_array_alias_watch;

type Dae = dae::Dae;
type Literal = dae::Literal;
type VarName = dae::VarName;

const DUMMY_STATE_NAME: &str = "_rumoca_dummy_state";

#[inline]
fn trace_timer_start_if(enabled: bool) -> Option<Instant> {
    if !enabled {
        return None;
    }
    #[cfg(target_arch = "wasm32")]
    {
        None
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        Some(Instant::now())
    }
}

#[inline]
fn trace_timer_elapsed_seconds(start: Option<Instant>) -> f64 {
    start.map_or(0.0, |t0| t0.elapsed().as_secs_f64())
}

fn build_parameter_values(dae_model: &Dae, budget: &TimeoutBudget) -> Result<Vec<f64>, SimError> {
    default_parameter_values_with_budget(dae_model, budget).map_err(|err| match err {
        ParameterValueError::Timeout(timeout) => timeout.into(),
        ParameterValueError::Invalid(message) => SimError::SolverError(message),
    })
}

fn run_timeout_step_sim<F>(budget: &TimeoutBudget, step: F) -> Result<(), SimError>
where
    F: FnOnce(),
{
    run_timeout_step::<SimError, _>(budget, step)
}

fn run_timeout_step_result_sim<F>(budget: &TimeoutBudget, step: F) -> Result<(), SimError>
where
    F: FnOnce() -> Result<(), SimError>,
{
    run_timeout_step_result::<SimError, _>(budget, step)
}

fn inject_dummy_state(dae_model: &mut Dae) {
    let var_name = VarName::new(DUMMY_STATE_NAME);
    let mut var = dae::Variable::new(var_name.clone());
    var.start = Some(dae::Expression::Literal(Literal::Real(0.0)));
    var.fixed = Some(true);
    dae_model.states.insert(var_name, var);

    let der_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Der,
        args: vec![dae::Expression::VarRef {
            name: VarName::new(DUMMY_STATE_NAME),
            subscripts: vec![],
        }],
    };
    dae_model.f_x.push(dae::Equation {
        lhs: None,
        rhs: der_expr,
        span: Span::DUMMY,
        scalar_count: 1,
        origin: "dummy_state_injection".to_string(),
    });
}

fn debug_print_after_expand(dae_model: &Dae) {
    if std::env::var("RUMOCA_DEBUG").is_err() {
        return;
    }
    eprintln!("[after expand_compound_derivatives] equations:");
    for (i, eq) in dae_model.f_x.iter().enumerate() {
        eprintln!("  eq[{i}]: {:?}", eq.rhs);
    }
    eprintln!(
        "[after expand_compound_derivatives] algebraics: {:?}",
        dae_model
            .algebraics
            .keys()
            .map(|n| n.as_str())
            .collect::<Vec<_>>()
    );
    eprintln!(
        "[after expand_compound_derivatives] states: {:?}",
        dae_model
            .states
            .keys()
            .map(|n| n.as_str())
            .collect::<Vec<_>>()
    );
}

fn debug_print_prepare_counts(dae_model: &Dae) {
    if std::env::var("RUMOCA_DEBUG").is_err() {
        return;
    }
    eprintln!(
        "[prepare_dae] states={}, algebraics={}, eqs={}",
        dae_model.states.len(),
        dae_model.algebraics.len(),
        dae_model.f_x.len()
    );
}

fn debug_print_mass_matrix(dae_model: &Dae, mass_matrix: &MassMatrix) {
    if std::env::var("RUMOCA_DEBUG").is_err() {
        return;
    }
    let state_names: Vec<_> = dae_model.states.keys().map(|n| n.as_str()).collect();
    for (i, name) in state_names.iter().enumerate() {
        let diag = mass_matrix
            .get(i)
            .and_then(|row| row.get(i))
            .copied()
            .unwrap_or(1.0);
        if (diag - 1.0).abs() > 1.0e-10 {
            eprintln!("[mass_matrix] state[{i}] {name:?} diag={diag}");
        }
        if let Some(row) = mass_matrix.get(i) {
            for (j, coeff) in row
                .iter()
                .copied()
                .enumerate()
                .filter(|(j, coeff)| *j != i && coeff.abs() > 1.0e-10)
            {
                let other = state_names.get(j).copied().unwrap_or("<unknown>");
                eprintln!("[mass_matrix] state[{i}] {name:?} offdiag[{j}] {other:?}={coeff}");
            }
        }
    }
}

fn apply_runtime_alias_substitutions_to_elimination(
    elim: &mut eliminate::EliminationResult,
    substitutions: &[RuntimeAliasSubstitution],
) {
    core_apply_runtime_alias_substitutions_to_elimination(elim, substitutions);
}

fn normalize_runtime_aliases_collect(dae: &mut Dae) -> (usize, Vec<RuntimeAliasSubstitution>) {
    core_normalize_runtime_aliases_collect(dae)
}

pub(super) fn run_orphan_and_direct_state_demotion_phases(
    dae: &mut Dae,
    budget: &TimeoutBudget,
    trace: bool,
) -> Result<(), SimError> {
    let mut n_demoted_orphan_states = 0usize;
    run_logged_phase(
        trace,
        "demote_orphan_states_without_equation_refs(phase1h)",
        || {
            run_timeout_step_sim(budget, || {
                n_demoted_orphan_states = demote_orphan_states_without_equation_refs(dae);
            })
        },
    )?;
    if n_demoted_orphan_states > 0 {
        eprintln!(
            "[prepare_dae] demoted {} orphan states without equation references",
            n_demoted_orphan_states
        );
    }

    let mut n_demoted_no_der_states = 0usize;
    run_logged_phase(
        trace,
        "demote_states_without_derivative_refs(phase1h2)",
        || {
            run_timeout_step_sim(budget, || {
                n_demoted_no_der_states = demote_states_without_derivative_refs(dae);
            })
        },
    )?;
    if n_demoted_no_der_states > 0 {
        eprintln!(
            "[prepare_dae] demoted {} states without derivative references",
            n_demoted_no_der_states
        );
    }

    let mut n_demoted_unassignable_states = 0usize;
    run_logged_phase(
        trace,
        "demote_states_without_assignable_derivative_rows(phase1h3)",
        || {
            run_timeout_step_sim(budget, || {
                n_demoted_unassignable_states =
                    demote_states_without_assignable_derivative_rows(dae);
            })
        },
    )?;
    if n_demoted_unassignable_states > 0 {
        eprintln!(
            "[prepare_dae] demoted {} states without assignable derivative rows",
            n_demoted_unassignable_states
        );
    }

    let n_states_before_repromote: usize = dae.states.values().map(|v| v.size()).sum();
    run_logged_phase(trace, "promote_der_algebraics_to_states(phase1h4)", || {
        run_timeout_step_sim(budget, || promote_der_algebraics_to_states(dae))
    })?;
    let n_states_after_repromote: usize = dae.states.values().map(|v| v.size()).sum();
    let n_repromoted = n_states_after_repromote.saturating_sub(n_states_before_repromote);
    if n_repromoted > 0 {
        eprintln!(
            "[prepare_dae] re-promoted {} algebraics to states after demotion passes",
            n_repromoted
        );
    }

    let mut n_redemoted_unassignable_states = 0usize;
    run_logged_phase(
        trace,
        "demote_states_without_assignable_derivative_rows(phase1h5)",
        || {
            run_timeout_step_sim(budget, || {
                n_redemoted_unassignable_states =
                    demote_states_without_assignable_derivative_rows(dae);
            })
        },
    )?;
    if n_redemoted_unassignable_states > 0 {
        eprintln!(
            "[prepare_dae] re-demoted {} states without assignable derivative rows after re-promotion",
            n_redemoted_unassignable_states
        );
    }

    let mut n_demoted_direct_assigned_states = 0usize;
    run_logged_phase(trace, "demote_direct_assigned_states(phase1i)", || {
        run_timeout_step_sim(budget, || {
            n_demoted_direct_assigned_states = demote_direct_assigned_states(dae);
        })
    })?;
    if n_demoted_direct_assigned_states > 0 {
        eprintln!(
            "[prepare_dae] demoted {} direct-assigned trajectory states",
            n_demoted_direct_assigned_states
        );
        run_logged_phase(trace, "expand_compound_derivatives(phase1i-post)", || {
            run_timeout_step_sim(budget, || expand_compound_derivatives(dae))
        })?;
    }
    Ok(())
}

pub(super) fn run_prepare_structure_passes(
    dae: &mut Dae,
    budget: &TimeoutBudget,
) -> Result<(), SimError> {
    let trace = sim_trace_enabled();

    run_logged_phase(trace, "expand_compound_derivatives(phase1a2)", || {
        run_timeout_step_sim(budget, || expand_compound_derivatives(dae))
    })?;
    debug_print_after_expand(dae);

    run_logged_phase(trace, "eliminate_derivative_aliases(phase1b)", || {
        run_timeout_step_sim(budget, || eliminate_derivative_aliases(dae))
    })?;

    let mut n_index_reduced = 0usize;
    run_logged_phase(
        trace,
        "index_reduce_missing_state_derivatives(phase1c)",
        || {
            run_timeout_step_sim(budget, || {
                n_index_reduced = index_reduce_missing_state_derivatives(dae);
            })
        },
    )?;
    if n_index_reduced > 0 {
        eprintln!(
            "[prepare_dae] index-reduced {} missing state derivatives",
            n_index_reduced
        );
    }

    let mut n_demoted_coupled_states = 0usize;
    run_logged_phase(trace, "demote_coupled_derivative_states(phase1d)", || {
        run_timeout_step_sim(budget, || {
            n_demoted_coupled_states = demote_coupled_derivative_states(dae);
        })
    })?;
    if n_demoted_coupled_states > 0 {
        eprintln!(
            "[prepare_dae] demoted {} coupled-derivative states",
            n_demoted_coupled_states
        );
    }

    let mut n_demoted_alias_states = 0usize;
    run_logged_phase(trace, "demote_alias_states_without_der(phase1e)", || {
        run_timeout_step_sim(budget, || {
            n_demoted_alias_states = demote_alias_states_without_der(dae);
        })
    })?;
    if n_demoted_alias_states > 0 || n_demoted_coupled_states > 0 {
        eprintln!(
            "[prepare_dae] demoted {} alias-states without derivative rows",
            n_demoted_alias_states
        );
        run_logged_phase(trace, "expand_compound_derivatives(phase1e-post)", || {
            run_timeout_step_sim(budget, || expand_compound_derivatives(dae))
        })?;
    }

    let n_states_before_promote: usize = dae.states.values().map(|v| v.size()).sum();
    run_logged_phase(trace, "promote_der_algebraics_to_states(phase1f)", || {
        run_timeout_step_sim(budget, || promote_der_algebraics_to_states(dae))
    })?;
    let n_x_after: usize = dae.states.values().map(|v| v.size()).sum();
    let n_promoted = n_x_after - n_states_before_promote;
    if n_promoted > 0 {
        eprintln!(
            "[prepare_dae] promoted {} algebraics to states ({} → {} states)",
            n_promoted, n_states_before_promote, n_x_after
        );
    }

    run_orphan_and_direct_state_demotion_phases(dae, budget, trace)?;
    Ok(())
}

pub(super) fn reorder_equations_for_prepare(
    dae: &mut Dae,
    budget: &TimeoutBudget,
) -> Result<(), SimError> {
    match run_timeout_step_result_sim(budget, || {
        reorder_equations_for_solver(dae).map_err(|err| match err {
            EquationReorderError::EquationMismatch {
                n_equations,
                n_states,
                n_algebraics,
            } => SimError::EquationMismatch {
                n_equations,
                n_states,
                n_algebraics,
            },
            EquationReorderError::MissingStateEquation(name) => {
                SimError::MissingStateEquation(name)
            }
        })
    }) {
        Ok(()) => Ok(()),
        Err(SimError::MissingStateEquation(name)) => {
            dump_missing_state_equation_diagnostics(dae, &name);
            Err(SimError::MissingStateEquation(name))
        }
        Err(e) => Err(e),
    }
}

pub fn build_ic_plan_or_empty(
    dae: &Dae,
    n_x: usize,
    budget: &TimeoutBudget,
) -> Result<Vec<rumoca_phase_structural::IcBlock>, SimError> {
    budget.check()?;
    if let Some(hint) = rumoca_phase_structural::build_ic_relaxation_hint(dae, n_x)
        && relaxed_ic_hint_has_disjoint_drop_row(dae, &hint)
    {
        if sim_trace_enabled() {
            eprintln!(
                "[sim-trace] IC plan build: skipping relaxed BLT hint with disjoint dropped rows/unknowns; using full-Newton IC solve"
            );
        }
        budget.check()?;
        return Ok(Vec::new());
    }
    let ic_blocks = match rumoca_phase_structural::build_ic_plan(dae, n_x) {
        Ok(blocks) => blocks,
        Err(err) => {
            if sim_trace_enabled() {
                eprintln!(
                    "[sim-trace] IC plan build failed, using empty plan: {:?}",
                    err
                );
            }
            Vec::new()
        }
    };
    budget.check()?;
    Ok(ic_blocks)
}

pub fn relaxed_ic_hint_has_disjoint_drop_row(
    dae: &Dae,
    hint: &rumoca_phase_structural::IcRelaxationHint,
) -> bool {
    if hint.dropped_eq_global.is_empty() || hint.dropped_unknown_names.is_empty() {
        return false;
    }
    let dropped_unknowns: Vec<dae::VarName> = hint
        .dropped_unknown_names
        .iter()
        .map(dae::VarName::new)
        .collect();
    hint.dropped_eq_global.iter().copied().any(|eq_idx| {
        let Some(eq) = dae.f_x.get(eq_idx) else {
            return true;
        };
        !dropped_unknowns
            .iter()
            .any(|name| rumoca_phase_structural::eliminate::expr_contains_var(&eq.rhs, name))
    })
}

fn validate_constant_solver_mass_matrix_form(
    dae: &Dae,
    n_x: usize,
    mass_matrix: &MassMatrix,
    budget: &TimeoutBudget,
) -> Result<(), SimError> {
    match validate_constant_mass_matrix_form(dae, n_x, mass_matrix, budget) {
        Ok(()) => Ok(()),
        Err(MassMatrixFormError::Timeout(timeout)) => Err(timeout.into()),
        Err(MassMatrixFormError::Invalid(message)) => Err(SimError::SolverError(format!(
            "constant mass-matrix form check failed {message}"
        ))),
    }
}

pub(super) fn build_prepare_mass_matrix(
    dae: &Dae,
    n_x: usize,
    has_dummy: bool,
    budget: &TimeoutBudget,
) -> Result<MassMatrix, SimError> {
    if has_dummy {
        return Ok(Vec::new());
    }
    budget.check()?;
    let trace = sim_trace_enabled();
    if trace {
        eprintln!("[sim-trace] mass_diag start: build_parameter_values");
    }
    let t_params = trace_timer_start_if(trace);
    let param_values = build_parameter_values(dae, budget)?;
    if trace {
        eprintln!(
            "[sim-trace] mass_diag done: build_parameter_values elapsed={:.3}s",
            trace_timer_elapsed_seconds(t_params)
        );
    }
    if trace {
        eprintln!("[sim-trace] mass_matrix start: compute_mass_matrix");
    }
    let t_compute = trace_timer_start_if(trace);
    let mass_matrix = compute_mass_matrix(dae, n_x, &param_values, budget)?;
    if trace {
        eprintln!(
            "[sim-trace] mass_matrix done: compute_mass_matrix elapsed={:.3}s",
            trace_timer_elapsed_seconds(t_compute)
        );
    }
    if trace {
        eprintln!("[sim-trace] mass_matrix start: validate_mass_matrix_form");
    }
    let t_validate = trace_timer_start_if(trace);
    validate_constant_solver_mass_matrix_form(dae, n_x, &mass_matrix, budget)?;
    if trace {
        eprintln!(
            "[sim-trace] mass_matrix done: validate_mass_matrix_form elapsed={:.3}s",
            trace_timer_elapsed_seconds(t_validate)
        );
    }
    budget.check()?;
    Ok(mass_matrix)
}

pub(super) fn eliminate_trivial_with_trace(
    dae: &mut Dae,
    trace: bool,
    phase_name: &str,
    watch_name: &str,
) -> eliminate::EliminationResult {
    if trace {
        eprintln!("[sim-trace] prepare phase start: {phase_name}");
    }
    let t_elim = trace_timer_start_if(trace);
    let elim = eliminate::eliminate_trivial(dae);
    trace_flow_array_alias_watch(watch_name, dae, trace);
    if trace {
        eprintln!(
            "[sim-trace] prepare phase done: {phase_name} elapsed={:.3}s eliminated_eqs={} substitutions={}",
            trace_timer_elapsed_seconds(t_elim),
            elim.n_eliminated,
            elim.substitutions.len()
        );
    }
    elim
}

fn run_trivial_elimination_phase(
    dae: &mut Dae,
    trace: bool,
    disable_trivial_elim: bool,
) -> eliminate::EliminationResult {
    if disable_trivial_elim {
        if trace {
            eprintln!("[sim-trace] prepare phase skipped: eliminate_trivial (disabled)");
        }
        return eliminate::EliminationResult::default();
    }
    eliminate_trivial_with_trace(dae, trace, "eliminate_trivial", "after_eliminate_trivial")
}

fn run_post_structure_elimination_phase(
    dae: &mut Dae,
    trace: bool,
    disable_trivial_elim: bool,
    elim: &mut eliminate::EliminationResult,
) {
    if disable_trivial_elim {
        if trace {
            eprintln!(
                "[sim-trace] prepare phase skipped: eliminate_trivial(post-structure) (disabled)"
            );
        }
        return;
    }
    let elim_post = eliminate_trivial_with_trace(
        dae,
        trace,
        "eliminate_trivial(post-structure)",
        "after_eliminate_trivial_post",
    );
    elim.substitutions.extend(elim_post.substitutions);
    elim.n_eliminated += elim_post.n_eliminated;
}

fn run_post_scalarize_elimination_phase(
    dae: &mut Dae,
    trace: bool,
    scalarize: bool,
    disable_trivial_elim: bool,
    elim: &mut eliminate::EliminationResult,
) {
    if scalarize || disable_trivial_elim {
        trace_flow_array_alias_watch("after_eliminate_trivial_post_scalarize", dae, trace);
        if trace {
            eprintln!(
                "[sim-trace] prepare phase done: eliminate_trivial(post-scalarize) elapsed=0.000s eliminated_eqs=0 substitutions=0 (skipped: scalarized equations)"
            );
        }
        return;
    }
    let elim_post_scalar = eliminate_trivial_with_trace(
        dae,
        trace,
        "eliminate_trivial(post-scalarize)",
        "after_eliminate_trivial_post_scalarize",
    );
    elim.substitutions.extend(elim_post_scalar.substitutions);
    elim.n_eliminated += elim_post_scalar.n_eliminated;
}

pub(super) fn build_ic_plan_with_trace(
    dae: &Dae,
    n_x: usize,
    budget: &TimeoutBudget,
    trace: bool,
) -> Result<Vec<rumoca_phase_structural::IcBlock>, SimError> {
    if trace {
        eprintln!("[sim-trace] prepare step start: build_ic_plan");
    }
    let t_ic = trace_timer_start_if(trace);
    let ic_blocks = build_ic_plan_or_empty(dae, n_x, budget)?;
    if trace {
        eprintln!(
            "[sim-trace] prepare step done: build_ic_plan elapsed={:.3}s",
            trace_timer_elapsed_seconds(t_ic)
        );
    }
    Ok(ic_blocks)
}

pub(super) fn build_mass_matrix_with_trace(
    dae: &Dae,
    n_x: usize,
    has_dummy: bool,
    budget: &TimeoutBudget,
    trace: bool,
) -> Result<MassMatrix, SimError> {
    if trace {
        eprintln!("[sim-trace] prepare step start: build_mass_matrix");
    }
    let t_mass = trace_timer_start_if(trace);
    let mass_matrix = build_prepare_mass_matrix(dae, n_x, has_dummy, budget)?;
    if trace {
        eprintln!(
            "[sim-trace] prepare step done: build_mass_matrix elapsed={:.3}s",
            trace_timer_elapsed_seconds(t_mass)
        );
    }
    Ok(mass_matrix)
}

pub(super) fn log_prepare_substitutions_if_introspect(
    elim: &eliminate::EliminationResult,
    trace: bool,
) {
    if !(trace && std::env::var("RUMOCA_SIM_INTROSPECT").is_ok()) {
        return;
    }
    let expr_limit = std::env::var("RUMOCA_SIM_INTROSPECT_EXPR_CHARS")
        .ok()
        .and_then(|raw| raw.parse::<usize>().ok())
        .filter(|n| *n > 0)
        .unwrap_or(160);
    let truncate = |s: String| -> String {
        if s.len() <= expr_limit {
            s
        } else {
            format!("{}...", &s[..expr_limit])
        }
    };
    let limit = std::env::var("RUMOCA_SIM_INTROSPECT_SUB_LIMIT")
        .ok()
        .and_then(|raw| raw.parse::<usize>().ok())
        .filter(|n| *n > 0)
        .unwrap_or(40);
    eprintln!(
        "[sim-introspect] substitutions count={} (showing {})",
        elim.substitutions.len(),
        limit.min(elim.substitutions.len())
    );
    for (i, sub) in elim.substitutions.iter().take(limit).enumerate() {
        let expr = truncate(format!("{:?}", sub.expr));
        eprintln!(
            "[sim-introspect] sub[{i}] var={} env_keys={:?} expr={}",
            sub.var_name.as_str(),
            sub.env_keys,
            expr
        );
    }
    if elim.substitutions.len() > limit {
        eprintln!(
            "[sim-introspect] ... omitted {} substitutions (set RUMOCA_SIM_INTROSPECT_SUB_LIMIT to increase)",
            elim.substitutions.len() - limit
        );
    }
}

/// Lightweight preparation path for template/runtime code generation.
///
/// Keeps structural preparation/index-reduction passes, but intentionally skips
/// solver-only products (IC block plan + mass matrix extraction) and runtime
/// alias normalization to preserve template-visible channels.
pub fn prepare_dae_for_template_codegen_only(
    dae: &Dae,
    scalarize: bool,
    budget: &TimeoutBudget,
) -> Result<Dae, SimError> {
    let trace = sim_trace_enabled();
    let trace_step = |name: &str, f: &mut dyn FnMut() -> Result<(), SimError>| {
        if trace {
            eprintln!("[sim-trace] prepare(template) step start: {name}");
        }
        let t0 = trace_timer_start_if(trace);
        let res = f();
        if trace {
            eprintln!(
                "[sim-trace] prepare(template) step done: {name} elapsed={:.3}s",
                trace_timer_elapsed_seconds(t0)
            );
        }
        res
    };

    budget.check()?;
    let n_x_orig: usize = dae.states.values().map(|v| v.size()).sum();
    let n_z_declared: usize = dae.algebraics.values().map(|v| v.size()).sum::<usize>()
        + dae.outputs.values().map(|v| v.size()).sum::<usize>();
    let n_discrete_declared: usize = dae.discrete_reals.values().map(|v| v.size()).sum::<usize>()
        + dae
            .discrete_valued
            .values()
            .map(|v| v.size())
            .sum::<usize>();

    if n_x_orig + n_z_declared + n_discrete_declared == 0 {
        return Err(SimError::EmptySystem);
    }

    let mut dae = dae.clone();
    let disable_trivial_elim = std::env::var("RUMOCA_SIM_DISABLE_TRIVIAL_ELIM").is_ok();

    budget.check()?;
    let mut elim = run_trivial_elimination_phase(&mut dae, trace, disable_trivial_elim);
    budget.check()?;

    run_prepare_structure_passes(&mut dae, budget)?;
    trace_flow_array_alias_watch("after_structure_passes(template)", &dae, trace);

    budget.check()?;
    run_post_structure_elimination_phase(&mut dae, trace, disable_trivial_elim, &mut elim);
    budget.check()?;

    debug_print_prepare_counts(&dae);
    let has_dummy = dae.states.values().map(|v| v.size()).sum::<usize>() == 0;
    if has_dummy {
        trace_step("inject_dummy_state", &mut || {
            run_timeout_step_sim(budget, || inject_dummy_state(&mut dae))
        })?;
    }

    if scalarize {
        trace_step("scalarize_equations", &mut || {
            run_timeout_step_sim(budget, || scalarize_equations(&mut dae))
        })?;
        trace_flow_array_alias_watch("after_scalarize(template)", &dae, trace);
    }

    trace_step("reorder_equations_for_solver", &mut || {
        reorder_equations_for_prepare(&mut dae, budget)
    })?;

    trace_step("normalize_ode_equation_signs", &mut || {
        run_timeout_step_sim(budget, || normalize_ode_equation_signs(&mut dae))
    })?;

    budget.check()?;
    run_post_scalarize_elimination_phase(
        &mut dae,
        trace,
        scalarize,
        disable_trivial_elim,
        &mut elim,
    );
    budget.check()?;

    log_prepare_substitutions_if_introspect(&elim, trace);

    trace_step("pin_orphaned_variables", &mut || {
        run_timeout_step_sim(budget, || pin_orphaned_variables(&mut dae, &elim))
    })?;

    Ok(dae)
}

pub fn prepare_dae(
    dae: &Dae,
    scalarize: bool,
    budget: &TimeoutBudget,
) -> Result<PreparedSimulation, SimError> {
    let trace = sim_trace_enabled();
    let trace_step = |name: &str, f: &mut dyn FnMut() -> Result<(), SimError>| {
        if trace {
            eprintln!("[sim-trace] prepare step start: {name}");
        }
        let t0 = trace_timer_start_if(trace);
        let res = f();
        if trace {
            eprintln!(
                "[sim-trace] prepare step done: {name} elapsed={:.3}s",
                trace_timer_elapsed_seconds(t0)
            );
        }
        res
    };
    budget.check()?;
    let n_x_orig: usize = dae.states.values().map(|v| v.size()).sum();
    let n_z_declared: usize = dae.algebraics.values().map(|v| v.size()).sum::<usize>()
        + dae.outputs.values().map(|v| v.size()).sum::<usize>();
    let n_discrete_declared: usize = dae.discrete_reals.values().map(|v| v.size()).sum::<usize>()
        + dae
            .discrete_valued
            .values()
            .map(|v| v.size())
            .sum::<usize>();

    if n_x_orig + n_z_declared + n_discrete_declared == 0 {
        return Err(SimError::EmptySystem);
    }

    let mut dae = dae.clone();
    let disable_trivial_elim = std::env::var("RUMOCA_SIM_DISABLE_TRIVIAL_ELIM").is_ok();

    budget.check()?;
    let mut elim = run_trivial_elimination_phase(&mut dae, trace, disable_trivial_elim);
    budget.check()?;

    run_prepare_structure_passes(&mut dae, budget)?;
    trace_flow_array_alias_watch("after_structure_passes", &dae, trace);

    run_logged_phase(trace, "normalize_runtime_aliases", || {
        run_timeout_step_sim(budget, || {
            let (n_normalized, runtime_alias_substitutions) =
                normalize_runtime_aliases_collect(&mut dae);
            apply_runtime_alias_substitutions_to_elimination(
                &mut elim,
                &runtime_alias_substitutions,
            );
            if trace && n_normalized > 0 {
                eprintln!(
                    "[sim-trace] normalized {} runtime alias variables into core-state equations/events",
                    n_normalized
                );
            }
        })
    })?;

    budget.check()?;
    run_post_structure_elimination_phase(&mut dae, trace, disable_trivial_elim, &mut elim);
    budget.check()?;

    debug_print_prepare_counts(&dae);
    let has_dummy = dae.states.values().map(|v| v.size()).sum::<usize>() == 0;
    if has_dummy {
        trace_step("inject_dummy_state", &mut || {
            run_timeout_step_sim(budget, || inject_dummy_state(&mut dae))
        })?;
    }

    if scalarize {
        trace_step("scalarize_equations", &mut || {
            run_timeout_step_sim(budget, || scalarize_equations(&mut dae))
        })?;
        trace_flow_array_alias_watch("after_scalarize", &dae, trace);
    }

    trace_step("reorder_equations_for_solver", &mut || {
        reorder_equations_for_prepare(&mut dae, budget)
    })?;

    trace_step("normalize_ode_equation_signs", &mut || {
        run_timeout_step_sim(budget, || normalize_ode_equation_signs(&mut dae))
    })?;

    budget.check()?;
    run_post_scalarize_elimination_phase(
        &mut dae,
        trace,
        scalarize,
        disable_trivial_elim,
        &mut elim,
    );
    budget.check()?;

    log_prepare_substitutions_if_introspect(&elim, trace);

    trace_step("pin_orphaned_variables", &mut || {
        run_timeout_step_sim(budget, || pin_orphaned_variables(&mut dae, &elim))
    })?;

    let n_x: usize = dae.states.values().map(|v| v.size()).sum();
    let ic_blocks = build_ic_plan_with_trace(&dae, n_x, budget, trace)?;

    let mass_matrix = build_mass_matrix_with_trace(&dae, n_x, has_dummy, budget, trace)?;

    debug_print_mass_matrix(&dae, &mass_matrix);

    Ok(PreparedSimulation {
        dae,
        has_dummy_state: has_dummy,
        elimination: elim,
        ic_blocks,
        mass_matrix,
    })
}
