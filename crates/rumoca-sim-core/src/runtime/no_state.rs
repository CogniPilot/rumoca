use std::collections::{HashMap, HashSet};

use rumoca_eval_runtime::eval;
use rumoca_ir_dae as dae;
use rumoca_phase_structural::EliminationResult;

use crate::{reconstruct, timeline};

/// Shared inputs for solver-agnostic no-state runtime sampling.
pub struct NoStateSampleContext<'a> {
    pub dae: &'a dae::Dae,
    pub elim: &'a EliminationResult,
    pub param_values: &'a [f64],
    pub all_names: &'a [String],
    pub solver_name_to_idx: &'a HashMap<String, usize>,
    pub n_x: usize,
    pub t_start: f64,
    pub requires_projection: bool,
}

/// Errors from no-state runtime sampling.
#[derive(Debug)]
pub enum NoStateSampleError<E> {
    Callback(E),
    SampleScheduleMismatch { captured: usize, expected: usize },
}

/// No-state sampling output tuple: final solver vector + per-output channel samples.
pub type NoStateSampleData = (Vec<f64>, Vec<Vec<f64>>);

/// Result type for no-state runtime sampling.
pub type NoStateSampleResult<E> = Result<NoStateSampleData, NoStateSampleError<E>>;

fn settle_runtime_env(ctx: &NoStateSampleContext<'_>, y: &mut [f64], t: f64) -> eval::VarEnv<f64> {
    let mut env = crate::runtime::event::settle_runtime_event_updates_default(
        crate::runtime::event::EventSettleInput {
            dae: ctx.dae,
            y,
            p: ctx.param_values,
            n_x: ctx.n_x,
            t_eval: t,
        },
    );
    reconstruct::apply_eliminated_substitutions_to_env(ctx.elim, &mut env);
    env
}

fn project_algebraics_if_changed<E, FProjectOrSeed>(
    ctx: &NoStateSampleContext<'_>,
    y: &mut Vec<f64>,
    t: f64,
    project_or_seed: &mut FProjectOrSeed,
) -> Result<bool, NoStateSampleError<E>>
where
    FProjectOrSeed: FnMut(&mut Vec<f64>, f64, bool) -> Result<(), E>,
{
    if !ctx.requires_projection {
        return Ok(false);
    }

    let before = y.clone();
    project_or_seed(y, t, true).map_err(NoStateSampleError::Callback)?;
    let changed = before
        .iter()
        .zip(y.iter())
        .any(|(old, new)| (old - new).abs() > 1.0e-12);
    Ok(changed)
}

fn runtime_output_value(
    ctx: &NoStateSampleContext<'_>,
    env: &eval::VarEnv<f64>,
    y: &[f64],
    name: &str,
) -> f64 {
    env.vars
        .get(name)
        .copied()
        .or_else(|| {
            ctx.solver_name_to_idx
                .get(name)
                .and_then(|idx| y.get(*idx).copied())
        })
        .unwrap_or(0.0)
}

fn append_output_samples_at_time(
    ctx: &NoStateSampleContext<'_>,
    output_times: &[f64],
    t: f64,
    out_idx: &mut usize,
    data: &mut [Vec<f64>],
    env: &eval::VarEnv<f64>,
    y: &[f64],
) {
    while *out_idx < output_times.len()
        && timeline::sample_time_match_with_tol(output_times[*out_idx], t)
    {
        for (name, series) in ctx.all_names.iter().zip(data.iter_mut()) {
            if *out_idx != series.len() {
                continue;
            }
            series.push(runtime_output_value(ctx, env, y, name));
        }
        *out_idx += 1;
    }
}

/// Collect sampled outputs for a no-state system.
///
/// `check_deadline` is called before each evaluation time.
/// `project_or_seed` performs backend-specific projection and/or direct-assignment seeding.
pub fn collect_algebraic_samples<E, FCheck, FProjectOrSeed>(
    ctx: &NoStateSampleContext<'_>,
    output_times: &[f64],
    evaluation_times: &[f64],
    mut y: Vec<f64>,
    mut check_deadline: FCheck,
    mut project_or_seed: FProjectOrSeed,
) -> NoStateSampleResult<E>
where
    FCheck: FnMut() -> Result<(), E>,
    FProjectOrSeed: FnMut(&mut Vec<f64>, f64, bool) -> Result<(), E>,
{
    let mut data: Vec<Vec<f64>> = vec![Vec::with_capacity(output_times.len()); ctx.all_names.len()];

    eval::clear_pre_values();
    project_or_seed(&mut y, ctx.t_start, ctx.requires_projection)
        .map_err(NoStateSampleError::Callback)?;
    crate::runtime::startup::refresh_pre_values_from_state_with_initial_assignments(
        ctx.dae,
        &y,
        ctx.param_values,
        ctx.t_start,
    );
    let mut t0_env =
        crate::runtime::event::build_runtime_env(ctx.dae, &mut y, ctx.param_values, ctx.t_start);
    reconstruct::apply_eliminated_substitutions_to_env(ctx.elim, &mut t0_env);

    let mut out_idx = 0usize;
    for &t in evaluation_times {
        check_deadline().map_err(NoStateSampleError::Callback)?;
        project_or_seed(&mut y, t, ctx.requires_projection)
            .map_err(NoStateSampleError::Callback)?;

        let mut env = settle_runtime_env(ctx, &mut y, t);
        let pre_snapshot = eval::snapshot_pre_values();
        eval::seed_pre_values_from_env(&env);
        if project_algebraics_if_changed(ctx, &mut y, t, &mut project_or_seed)? {
            env = settle_runtime_env(ctx, &mut y, t);
        }
        eval::restore_pre_values(pre_snapshot);

        eval::seed_pre_values_from_env(&env);
        append_output_samples_at_time(ctx, output_times, t, &mut out_idx, &mut data, &env, &y);
    }

    if out_idx != output_times.len() {
        return Err(NoStateSampleError::SampleScheduleMismatch {
            captured: out_idx,
            expected: output_times.len(),
        });
    }

    Ok((y, data))
}

/// Remove backend-internal dummy state channels from no-state outputs.
pub fn finalize_algebraic_outputs(
    all_names: Vec<String>,
    data: Vec<Vec<f64>>,
    n_x: usize,
    dummy_state_name: &str,
) -> (Vec<String>, Vec<Vec<f64>>, usize) {
    let (mut final_names, mut final_data, mut final_n_states) = (all_names, data, n_x);
    if let Some(dummy_idx) = final_names.iter().position(|name| name == dummy_state_name) {
        final_names.remove(dummy_idx);
        if dummy_idx < final_data.len() {
            final_data.remove(dummy_idx);
        }
        final_n_states = final_n_states.saturating_sub(1);
    }
    (final_names, final_data, final_n_states)
}

/// Collect additional discrete names needed for reconstruction context.
pub fn collect_reconstruction_discrete_context_names(
    dae_model: &dae::Dae,
    elim: &EliminationResult,
    existing_names: &[String],
) -> Vec<String> {
    let existing: HashSet<String> = existing_names.iter().cloned().collect();
    let mut extras: indexmap::IndexSet<String> = indexmap::IndexSet::new();

    for sub in &elim.substitutions {
        let mut refs = HashSet::new();
        sub.expr.collect_var_refs(&mut refs);
        for name in refs {
            let raw = name.as_str();
            if existing.contains(raw) {
                continue;
            }
            let key = dae::VarName::new(raw);
            if dae_model.discrete_reals.contains_key(&key)
                || dae_model.discrete_valued.contains_key(&key)
            {
                extras.insert(raw.to_string());
            }
        }
    }

    extras.into_iter().collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    use rumoca_ir_dae as dae;

    #[test]
    fn finalize_algebraic_outputs_removes_dummy_channel() {
        let all_names = vec!["__dummy_time_state".to_string(), "y".to_string()];
        let data = vec![vec![0.0, 0.0], vec![1.0, 2.0]];
        let (names, values, n_states) =
            finalize_algebraic_outputs(all_names, data, 1, "__dummy_time_state");
        assert_eq!(names, vec!["y".to_string()]);
        assert_eq!(values, vec![vec![1.0, 2.0]]);
        assert_eq!(n_states, 0);
    }

    #[test]
    fn collect_reconstruction_discrete_context_names_collects_discrete_refs() {
        let mut dae_model = dae::Dae::default();
        dae_model.discrete_reals.insert(
            dae::VarName::new("d"),
            dae::Variable::new(dae::VarName::new("d")),
        );
        let mut elim = EliminationResult::default();
        elim.substitutions
            .push(rumoca_phase_structural::Substitution {
                var_name: dae::VarName::new("y"),
                expr: dae::Expression::VarRef {
                    name: dae::VarName::new("d"),
                    subscripts: vec![],
                },
                env_keys: vec![],
            });
        let extras = collect_reconstruction_discrete_context_names(&dae_model, &elim, &[]);
        assert_eq!(extras, vec!["d".to_string()]);
    }

    #[test]
    fn collect_algebraic_samples_reports_schedule_mismatch() {
        let dae_model = dae::Dae::default();
        let elim = EliminationResult::default();
        let all_names: Vec<String> = Vec::new();
        let solver_name_to_idx: HashMap<String, usize> = HashMap::new();
        let ctx = NoStateSampleContext {
            dae: &dae_model,
            elim: &elim,
            param_values: &[],
            all_names: &all_names,
            solver_name_to_idx: &solver_name_to_idx,
            n_x: 0,
            t_start: 0.0,
            requires_projection: false,
        };

        let result = collect_algebraic_samples(
            &ctx,
            &[0.0],
            &[],
            vec![],
            || Ok::<(), ()>(()),
            |_y, _t, _requires_projection| Ok::<(), ()>(()),
        );

        assert!(matches!(
            result,
            Err(NoStateSampleError::SampleScheduleMismatch { .. })
        ));
    }
}
