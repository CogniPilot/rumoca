use super::*;

pub(crate) fn extract_direct_assignment(rhs: &Expression) -> Option<(String, &Expression)> {
    rumoca_sim::runtime::assignment::extract_direct_assignment(rhs)
}

pub(crate) fn direct_assignment_from_equation(eq: &Equation) -> Option<(String, &Expression)> {
    rumoca_sim::runtime::assignment::direct_assignment_from_equation(eq)
}

pub(crate) fn direct_seed_var_ref_key(expr: &Expression) -> Option<String> {
    let Expression::VarRef { name, subscripts } = expr else {
        return None;
    };
    rumoca_sim::runtime::assignment::canonical_var_ref_key(name, subscripts)
}

pub(crate) fn direct_seed_is_zero_literal(expr: &Expression) -> bool {
    match expr {
        Expression::Literal(dae::Literal::Integer(0)) => true,
        Expression::Literal(dae::Literal::Real(v)) => v.abs() <= f64::EPSILON,
        _ => false,
    }
}

pub(crate) fn direct_seed_target_key(expr: &Expression) -> Option<String> {
    if let Some(key) = direct_seed_var_ref_key(expr) {
        return Some(key);
    }
    let Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(_),
        lhs,
        rhs,
    } = expr
    else {
        return None;
    };
    direct_seed_is_zero_literal(rhs.as_ref())
        .then(|| direct_seed_var_ref_key(lhs.as_ref()))
        .flatten()
}

pub(crate) fn normalize_direct_seed_solver_solution(
    solution: &Expression,
    name_to_idx: &HashMap<String, usize>,
) -> Expression {
    let Expression::VarRef { name, subscripts } = solution else {
        return solution.clone();
    };
    if !subscripts.is_empty() {
        return solution.clone();
    }
    let raw = name.as_str();
    let Some((base, raw_indices)) = raw.split_once('[') else {
        return solution.clone();
    };
    let Some(indices) = raw_indices.strip_suffix(']') else {
        return solution.clone();
    };
    let all_one = indices
        .split(',')
        .all(|part| part.trim().is_empty() || part.trim() == "1");
    if !all_one || !name_to_idx.contains_key(base) {
        return solution.clone();
    }
    Expression::VarRef {
        name: dae::VarName::new(base),
        subscripts: vec![],
    }
}

pub(crate) fn direct_seed_assignment_from_equation<'a>(
    dae: &Dae,
    eq: &'a Equation,
    name_to_idx: &HashMap<String, usize>,
) -> Option<(String, &'a Expression)> {
    let Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(_),
        lhs,
        rhs,
    } = &eq.rhs
    else {
        return direct_assignment_from_equation(eq);
    };

    let lhs_direct_key = direct_seed_var_ref_key(lhs.as_ref());
    let rhs_direct_key = direct_seed_var_ref_key(rhs.as_ref());
    let lhs_key = lhs_direct_key
        .clone()
        .or_else(|| direct_seed_target_key(lhs.as_ref()));
    let rhs_key = rhs_direct_key
        .clone()
        .or_else(|| direct_seed_target_key(rhs.as_ref()));
    let lhs_wrapped_target = lhs_direct_key.is_none() && lhs_key.is_some();
    let rhs_wrapped_target = rhs_direct_key.is_none() && rhs_key.is_some();
    let lhs_solver = lhs_key
        .as_ref()
        .and_then(|name| solver_idx_for_target(name, name_to_idx))
        .is_some();
    let rhs_solver = rhs_key
        .as_ref()
        .and_then(|name| solver_idx_for_target(name, name_to_idx))
        .is_some();

    match (lhs_key, rhs_key, lhs_solver, rhs_solver) {
        (Some(_lhs_name), Some(rhs_name), true, false)
            if rumoca_sim::runtime::assignment::is_runtime_unknown_name(dae, rhs_name.as_str()) =>
        {
            Some((rhs_name, lhs.as_ref()))
        }
        (Some(lhs_name), Some(_rhs_name), false, true)
            if rumoca_sim::runtime::assignment::is_runtime_unknown_name(dae, lhs_name.as_str()) =>
        {
            Some((lhs_name, rhs.as_ref()))
        }
        (Some(lhs_name), None, true, false) if lhs_wrapped_target => Some((lhs_name, rhs.as_ref())),
        (None, Some(rhs_name), false, true) if rhs_wrapped_target => Some((rhs_name, lhs.as_ref())),
        _ => direct_assignment_from_equation(eq),
    }
}

pub(crate) fn direct_seed_solution_is_env_only_alias_varref(
    dae: &Dae,
    solution: &Expression,
    name_to_idx: &HashMap<String, usize>,
) -> bool {
    let Expression::VarRef { name, subscripts } = solution else {
        return false;
    };
    let Some(source_key) = rumoca_sim::runtime::assignment::canonical_var_ref_key(name, subscripts)
    else {
        return false;
    };
    rumoca_sim::runtime::assignment::is_runtime_unknown_name(dae, source_key.as_str())
        && solver_idx_for_target(source_key.as_str(), name_to_idx).is_none()
}

pub(crate) fn direct_seed_solution_is_redundant_alias_varref(
    dae: &Dae,
    solution: &Expression,
    name_to_idx: &HashMap<String, usize>,
    target_stats: rumoca_sim::runtime::assignment::DirectAssignmentTargetStats,
) -> bool {
    if direct_seed_solution_is_env_only_alias_varref(dae, solution, name_to_idx) {
        return true;
    }

    // MLS Appendix B: when a target has one real defining equation plus plain
    // alias/connection equations, direct seeding must keep the defining RHS and
    // ignore the alias propagation edges. Otherwise the runtime seed pass can
    // oscillate between the physical equation and a stale solver alias value.
    target_stats.total > 1
        && target_stats.non_alias == 1
        && rumoca_sim::runtime::assignment::assignment_solution_is_alias_varref(dae, solution)
}

#[cfg(test)]
pub(crate) fn apply_initial_section_assignments(
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    t_eval: f64,
) -> usize {
    rumoca_sim::apply_initial_section_assignments(dae, y, p, t_eval)
}

pub(crate) fn apply_initial_section_assignments_strict(
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    t_eval: f64,
) -> Result<usize, crate::SimError> {
    rumoca_sim::runtime::startup::apply_initial_section_assignments_strict(dae, y, p, t_eval)
        .map_err(crate::SimError::CompiledEval)
}

pub(crate) fn solver_idx_for_target(
    target: &str,
    name_to_idx: &HashMap<String, usize>,
) -> Option<usize> {
    rumoca_sim::runtime::layout::solver_idx_for_target(target, name_to_idx)
}

pub(crate) fn log_ic_direct_seed(name: &str, value: f64) {
    if sim_introspect_enabled() {
        eprintln!("[sim-introspect] IC direct seed {} = {}", name, value);
    }
}

#[cfg(test)]
pub(crate) fn apply_discrete_partition_updates(dae: &Dae, env: &mut VarEnv<f64>) -> bool {
    rumoca_sim::runtime::discrete::apply_discrete_partition_updates(dae, env)
}

pub(crate) fn apply_seeded_values_to_indices(
    y: &mut [f64],
    env: &mut VarEnv<f64>,
    names: &[String],
    indices: &[usize],
    values: &[f64],
    n_x: usize,
) -> (bool, usize) {
    rumoca_sim::runtime::assignment::apply_seeded_values_to_indices(
        y,
        env,
        names,
        indices,
        values,
        n_x,
        log_ic_direct_seed,
    )
}

#[cfg(test)]
pub(crate) fn apply_runtime_values_to_indices(
    y: &mut [f64],
    env: &mut VarEnv<f64>,
    names: &[String],
    indices: &[usize],
    values: &[f64],
    n_x: usize,
) -> (bool, usize) {
    rumoca_sim::runtime::assignment::apply_runtime_values_to_indices(
        y, env, names, indices, values, n_x,
    )
}

#[cfg(test)]
pub(crate) fn build_runtime_alias_adjacency(dae: &Dae, n_x: usize) -> HashMap<String, Vec<String>> {
    rumoca_sim::runtime::alias::build_runtime_alias_adjacency_with_known_assignments(dae, n_x)
}

#[cfg(test)]
pub(crate) fn collect_runtime_alias_anchor_names(dae: &Dae, n_x: usize) -> HashSet<String> {
    rumoca_sim::runtime::alias::collect_runtime_alias_anchor_names(dae, n_x)
}

#[cfg(test)]
pub(crate) fn propagate_runtime_alias_components_from_env(
    dae: &Dae,
    y: &mut [f64],
    n_x: usize,
    env: &mut VarEnv<f64>,
) -> usize {
    rumoca_sim::runtime::alias::propagate_runtime_alias_components_from_env(dae, y, n_x, env)
}

pub(crate) fn seed_direct_assignment_initial_values(
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    n_x: usize,
    use_initial: bool,
    t_eval: f64,
) -> usize {
    seed_direct_assignment_initial_values_with_overrides(
        dae,
        y,
        p,
        n_x,
        None,
        DirectAssignmentSeedOptions {
            use_initial,
            t_eval,
            skip_unknown_alias_pairs: false,
            allow_unsolved_solver_sources: false,
            bootstrap_initial_section: true,
        },
    )
}

pub(crate) fn seed_runtime_direct_assignment_values(
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    n_x: usize,
    t_eval: f64,
) -> usize {
    let ctx = build_runtime_direct_seed_context(dae, y.len(), n_x);
    seed_runtime_direct_assignment_values_with_context(&ctx, dae, y, p, t_eval)
}

pub(crate) struct RuntimeDirectSeedContext {
    n_x: usize,
    names: Vec<String>,
    candidates: Vec<RuntimeDirectSeedCandidate>,
    compiled_rows: Option<CompiledRuntimeExpressionContext>,
}

pub(crate) enum RuntimeDirectSeedApply {
    SolverScalar {
        solver_idx: usize,
        solver_name: String,
    },
    SolverArray {
        indices: Vec<usize>,
    },
    EnvScalar,
    EnvArray {
        dims: Vec<i64>,
    },
}

pub(crate) struct RuntimeDirectSeedCandidate {
    target: String,
    solution: Expression,
    value_count: usize,
    row_range: Option<std::ops::Range<usize>>,
    trace_target: bool,
    apply: RuntimeDirectSeedApply,
}

pub(crate) struct RuntimeDirectSeedCandidateBuildContext<'a> {
    dae: &'a Dae,
    y_len: usize,
    n_x: usize,
    names: &'a [String],
    name_to_idx: &'a HashMap<String, usize>,
    base_to_indices: &'a HashMap<String, Vec<usize>>,
    target_dependencies: &'a HashMap<String, Vec<String>>,
    target_assignment_stats:
        &'a HashMap<String, rumoca_sim::runtime::assignment::DirectAssignmentTargetStats>,
}

pub(crate) fn collect_runtime_direct_seed_target_dependencies(
    dae: &Dae,
    n_x: usize,
    name_to_idx: &HashMap<String, usize>,
) -> HashMap<String, Vec<String>> {
    let mut deps: HashMap<String, std::collections::HashSet<String>> = HashMap::new();
    for eq in dae.f_x.iter().skip(n_x) {
        if eq.origin == "orphaned_variable_pin" {
            continue;
        }
        let Some((target, solution)) = direct_seed_assignment_from_equation(dae, eq, name_to_idx)
        else {
            continue;
        };
        if rumoca_sim::runtime::assignment::assignment_solution_is_alias_varref(dae, solution) {
            continue;
        }
        let mut refs = std::collections::HashSet::new();
        solution.collect_var_refs(&mut refs);
        let target_name = target.clone();
        let target_deps = deps.entry(target).or_default();
        for name in refs {
            let source = name.as_str();
            if source == target_name.as_str() {
                continue;
            }
            if rumoca_sim::runtime::assignment::is_known_assignment_name(dae, source) {
                target_deps.insert(source.to_string());
            }
        }
    }
    deps.into_iter()
        .map(|(target, deps)| {
            let mut deps = deps.into_iter().collect::<Vec<_>>();
            deps.sort();
            (target, deps)
        })
        .collect()
}

pub(crate) fn collect_runtime_direct_seed_target_stats(
    dae: &Dae,
    n_x: usize,
    name_to_idx: &HashMap<String, usize>,
) -> HashMap<String, rumoca_sim::runtime::assignment::DirectAssignmentTargetStats> {
    let mut stats: HashMap<String, rumoca_sim::runtime::assignment::DirectAssignmentTargetStats> =
        HashMap::new();
    for eq in dae.f_x.iter().skip(n_x) {
        if eq.origin == "orphaned_variable_pin" {
            continue;
        }
        let Some((target, solution)) = direct_seed_assignment_from_equation(dae, eq, name_to_idx)
        else {
            continue;
        };
        let normalized_solution = normalize_direct_seed_solver_solution(solution, name_to_idx);
        let entry = stats.entry(target).or_default();
        entry.total += 1;
        if !rumoca_sim::runtime::assignment::assignment_solution_is_alias_varref(
            dae,
            &normalized_solution,
        ) {
            entry.non_alias += 1;
        }
    }
    stats
}

pub(crate) fn build_runtime_direct_seed_context(
    dae: &Dae,
    y_len: usize,
    n_x: usize,
) -> RuntimeDirectSeedContext {
    let SolverNameIndexMaps {
        names,
        name_to_idx,
        base_to_indices,
    } = build_solver_name_index_maps(dae, y_len);
    let target_assignment_stats = collect_runtime_direct_seed_target_stats(dae, n_x, &name_to_idx);
    let target_dependencies =
        collect_runtime_direct_seed_target_dependencies(dae, n_x, &name_to_idx);
    let build_ctx = RuntimeDirectSeedCandidateBuildContext {
        dae,
        y_len,
        n_x,
        names: &names,
        name_to_idx: &name_to_idx,
        base_to_indices: &base_to_indices,
        target_dependencies: &target_dependencies,
        target_assignment_stats: &target_assignment_stats,
    };
    let runtime_candidates = build_runtime_direct_seed_candidates(&build_ctx, None);
    let (compiled_rows, candidates) = if runtime_candidates.is_empty() {
        (None, runtime_candidates)
    } else {
        let compiled_ctx = build_compiled_direct_seed_context(
            dae,
            y_len,
            DirectAssignmentSeedOptions {
                use_initial: false,
                t_eval: 0.0,
                skip_unknown_alias_pairs: true,
                allow_unsolved_solver_sources: true,
                bootstrap_initial_section: false,
            },
            &base_to_indices,
        );
        match compiled_ctx {
            Ok(Some(compiled_ctx)) => {
                let candidates = build_runtime_direct_seed_candidates(
                    &build_ctx,
                    Some(&compiled_ctx.rows_by_eq),
                );
                let compiled_rows = (!candidates.is_empty()).then_some(compiled_ctx.compiled_rows);
                (compiled_rows, candidates)
            }
            Ok(None) | Err(crate::SimError::CompiledEval(_)) => (None, runtime_candidates),
            Err(err) => panic!("runtime direct-seed context failed: {err}"),
        }
    };
    RuntimeDirectSeedContext {
        n_x,
        names,
        candidates,
        compiled_rows,
    }
}

pub(crate) fn seed_runtime_direct_assignment_values_with_context(
    ctx: &RuntimeDirectSeedContext,
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    t_eval: f64,
) -> usize {
    seed_runtime_direct_assignment_values_with_context_and_env(ctx, dae, y, p, t_eval, None)
}

pub(crate) fn seed_runtime_direct_assignment_values_with_context_and_env(
    ctx: &RuntimeDirectSeedContext,
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    t_eval: f64,
    reusable_env: Option<&mut Option<VarEnv<f64>>>,
) -> usize {
    seed_direct_assignment_initial_values_with_runtime_context(
        ctx,
        RuntimeSeedRun {
            dae,
            y,
            p,
            seed_env: None,
            reusable_env,
            options: DirectAssignmentSeedOptions {
                use_initial: false,
                t_eval,
                skip_unknown_alias_pairs: true,
                allow_unsolved_solver_sources: true,
                bootstrap_initial_section: false,
            },
            blocked_solver_cols: None,
        },
    )
}

pub(crate) fn seed_runtime_direct_assignment_values_with_context_and_env_and_blocked_solver_cols(
    ctx: &RuntimeDirectSeedContext,
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    t_eval: f64,
    reusable_env: Option<&mut Option<VarEnv<f64>>>,
    blocked_solver_cols: &[bool],
) -> usize {
    seed_direct_assignment_initial_values_with_runtime_context(
        ctx,
        RuntimeSeedRun {
            dae,
            y,
            p,
            seed_env: None,
            reusable_env,
            options: DirectAssignmentSeedOptions {
                use_initial: false,
                t_eval,
                skip_unknown_alias_pairs: true,
                allow_unsolved_solver_sources: true,
                bootstrap_initial_section: false,
            },
            blocked_solver_cols: Some(blocked_solver_cols),
        },
    )
}

pub(crate) struct RuntimeSeedRun<'a> {
    dae: &'a Dae,
    y: &'a mut [f64],
    p: &'a [f64],
    seed_env: Option<&'a VarEnv<f64>>,
    reusable_env: Option<&'a mut Option<VarEnv<f64>>>,
    options: DirectAssignmentSeedOptions,
    blocked_solver_cols: Option<&'a [bool]>,
}

pub(crate) fn seed_direct_assignment_initial_values_with_runtime_context(
    runtime_ctx: &RuntimeDirectSeedContext,
    run: RuntimeSeedRun<'_>,
) -> usize {
    if run.dae.f_x.len() <= runtime_ctx.n_x || run.y.is_empty() || runtime_ctx.candidates.is_empty()
    {
        return 0;
    }

    let mut updates = 0usize;
    let max_passes = run.y.len().max(4);
    let store_solver_values_in_env = run.seed_env.is_some();
    let mut owned_env;
    let env = if let Some(env_slot) = run.reusable_env {
        if env_slot.is_none() {
            *env_slot = Some(if store_solver_values_in_env {
                build_direct_seed_base_env(run.dae, run.y, run.p, run.options, run.seed_env)
            } else {
                build_runtime_direct_seed_base_env(run.dae, run.y, run.p, run.options, run.seed_env)
            });
        }
        let env = env_slot
            .as_mut()
            .expect("direct-seed reusable env slot must be populated");
        if store_solver_values_in_env {
            refresh_direct_seed_env(env, run.dae, run.y, run.p, run.options, run.seed_env);
        } else {
            refresh_runtime_direct_seed_env(env, run.dae, run.p, run.options, run.seed_env);
            rumoca_sim::runtime::alias::propagate_runtime_alias_components_from_env(
                run.dae,
                run.y,
                runtime_ctx.n_x,
                env,
            );
        }
        env
    } else {
        owned_env = if store_solver_values_in_env {
            build_direct_seed_base_env(run.dae, run.y, run.p, run.options, run.seed_env)
        } else {
            build_runtime_direct_seed_base_env(run.dae, run.y, run.p, run.options, run.seed_env)
        };
        &mut owned_env
    };
    let mut y_scratch = Vec::with_capacity(run.y.len());
    let mut compiled_scalar_out = Vec::new();
    for _ in 0..max_passes {
        let mut changed = false;
        let compiled_values = runtime_ctx.compiled_rows.as_ref().map(|compiled_rows| {
            eval_compiled_runtime_expressions_from_env(
                compiled_rows,
                run.y,
                env,
                run.p,
                run.options.t_eval,
                &mut y_scratch,
                &mut compiled_scalar_out,
            )
        });

        for candidate in &runtime_ctx.candidates {
            let (eq_changed, eq_updates) = apply_runtime_seed_candidate(
                runtime_ctx,
                candidate,
                run.y,
                env,
                compiled_values,
                store_solver_values_in_env,
                run.blocked_solver_cols,
            );
            changed |= eq_changed;
            updates += eq_updates;
        }

        if !changed {
            break;
        }

        if store_solver_values_in_env {
            refresh_direct_seed_env(env, run.dae, run.y, run.p, run.options, run.seed_env);
        } else {
            refresh_runtime_direct_seed_env(env, run.dae, run.p, run.options, run.seed_env);
        }
    }
    updates
}

pub(crate) fn build_runtime_direct_seed_candidates(
    ctx: &RuntimeDirectSeedCandidateBuildContext<'_>,
    rows_by_eq: Option<&HashMap<usize, std::ops::Range<usize>>>,
) -> Vec<RuntimeDirectSeedCandidate> {
    let mut candidates = Vec::new();
    for eq in ctx.dae.f_x.iter().skip(ctx.n_x) {
        if eq.origin == "orphaned_variable_pin" {
            continue;
        }
        let Some((target, solution)) =
            direct_seed_assignment_from_equation(ctx.dae, eq, ctx.name_to_idx)
        else {
            continue;
        };
        let normalized_solution = normalize_direct_seed_solver_solution(solution, ctx.name_to_idx);
        if direct_seed_solution_is_env_only_alias_varref(
            ctx.dae,
            &normalized_solution,
            ctx.name_to_idx,
        ) {
            continue;
        }
        let target_stats = ctx
            .target_assignment_stats
            .get(target.as_str())
            .copied()
            .unwrap_or_default();
        if direct_seed_solution_is_redundant_alias_varref(
            ctx.dae,
            &normalized_solution,
            ctx.name_to_idx,
            target_stats,
        ) {
            continue;
        }
        if target_stats.total > 1 && target_stats.non_alias != 1 {
            continue;
        }
        if direct_seed_solution_is_runtime_clock_signal(ctx.dae, &normalized_solution) {
            continue;
        }
        let branch_ctx = BranchConditionContext {
            name_to_idx: ctx.name_to_idx,
            n_x: ctx.n_x,
            y_len: ctx.y_len,
            target_dependencies: ctx.target_dependencies,
        };
        if solution_has_solver_dependent_branch_condition(&normalized_solution, &branch_ctx) {
            continue;
        }
        let target_size = direct_seed_target_size(ctx.dae, target.as_str(), ctx.base_to_indices);
        let row_range = rows_by_eq.and_then(|rows| rows.get(&equation_key(eq)).cloned());
        let apply = if !target.contains('[') && target_size > 1 {
            if let Some(indices) = ctx
                .base_to_indices
                .get(target.as_str())
                .filter(|indices| !indices.is_empty())
            {
                RuntimeDirectSeedApply::SolverArray {
                    indices: indices.clone(),
                }
            } else {
                RuntimeDirectSeedApply::EnvArray {
                    dims: direct_seed_target_dims(ctx.dae, target.as_str(), target_size),
                }
            }
        } else if let Some(solver_idx) = solver_idx_for_target(target.as_str(), ctx.name_to_idx) {
            if solver_idx < ctx.n_x || solver_idx >= ctx.y_len {
                continue;
            }
            RuntimeDirectSeedApply::SolverScalar {
                solver_idx,
                solver_name: ctx
                    .names
                    .get(solver_idx)
                    .cloned()
                    .unwrap_or_else(|| target.clone()),
            }
        } else {
            RuntimeDirectSeedApply::EnvScalar
        };
        candidates.push(RuntimeDirectSeedCandidate {
            trace_target: should_trace_direct_seed_target(target.as_str()),
            target,
            solution: normalized_solution,
            value_count: target_size.max(1),
            row_range,
            apply,
        });
    }
    candidates
}

#[derive(Clone, Copy)]
pub(crate) struct DirectAssignmentSeedOptions {
    pub(super) use_initial: bool,
    pub(super) t_eval: f64,
    pub(super) skip_unknown_alias_pairs: bool,
    pub(super) allow_unsolved_solver_sources: bool,
    pub(super) bootstrap_initial_section: bool,
}

pub(crate) type SolverNameIndexMaps = rumoca_sim::runtime::layout::SolverNameIndexMaps;

pub(crate) fn build_solver_name_index_maps(dae: &Dae, y_len: usize) -> SolverNameIndexMaps {
    rumoca_sim::runtime::layout::build_solver_name_index_maps(dae, y_len)
}

pub(crate) fn apply_seed_env_overrides(env: &mut VarEnv<f64>, seed_env: Option<&VarEnv<f64>>) {
    let Some(seed_env) = seed_env else {
        return;
    };
    for (name, value) in &seed_env.vars {
        env.set(name, *value);
    }
}

pub(crate) fn copy_runtime_discrete_binding(dst: &mut VarEnv<f64>, src: &VarEnv<f64>, name: &str) {
    if let Some(value) = src.vars.get(name) {
        dst.set(name, *value);
    }
    let prefix = format!("{name}[");
    for (key, value) in src
        .vars
        .iter()
        .filter(|(key, _)| key.starts_with(prefix.as_str()))
    {
        dst.set(key, *value);
    }
}

pub(crate) fn merge_initial_section_discrete_values(
    dae: &Dae,
    dst: &mut VarEnv<f64>,
    y: &[f64],
    p: &[f64],
    t_eval: f64,
) {
    let n_x = dae.states.values().map(|var| var.size()).sum::<usize>();
    let direct_assignment_ctx =
        rumoca_sim::runtime::assignment::build_runtime_direct_assignment_context(dae, y.len(), n_x);
    let alias_ctx =
        rumoca_sim::runtime::alias::build_runtime_alias_propagation_context(dae, y.len(), n_x);
    let solver_name_to_idx = build_solver_name_index_maps(dae, y.len()).name_to_idx;
    let all_names: Vec<String> = Vec::new();
    let clock_event_times: Vec<f64> = Vec::new();
    let dynamic_time_event_names: Vec<String> = Vec::new();
    let elim = rumoca_phase_structural::EliminationResult::default();
    let sample_ctx = rumoca_sim::NoStateSampleContext {
        dae,
        elim: &elim,
        param_values: p,
        all_names: &all_names,
        clock_event_times: &clock_event_times,
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &dynamic_time_event_names,
        solver_name_to_idx: &solver_name_to_idx,
        n_x,
        t_start: t_eval,
        requires_projection: false,
        projection_needs_event_refresh: false,
        requires_live_pre_values: rumoca_sim::runtime::no_state::no_state_requires_live_pre_values(
            dae,
        ),
    };
    let mut startup_y = y.to_vec();
    let initial_env = rumoca_sim::runtime::no_state::build_initial_settled_runtime_env(
        &sample_ctx,
        &mut startup_y,
        t_eval,
    );
    for (name, _) in dae.discrete_reals.iter().chain(dae.discrete_valued.iter()) {
        copy_runtime_discrete_binding(dst, &initial_env, name.as_str());
    }
}

pub(crate) fn build_runtime_direct_seed_base_env(
    dae: &Dae,
    y: &[f64],
    p: &[f64],
    options: DirectAssignmentSeedOptions,
    seed_env: Option<&VarEnv<f64>>,
) -> VarEnv<f64> {
    let mut env =
        rumoca_eval_dae::runtime::build_runtime_parameter_tail_env(dae, p, options.t_eval);
    if options.bootstrap_initial_section {
        // MLS §8.6 / Appendix B: initial direct-assignment seeding must observe
        // the initialization section's current discrete values before the first
        // ordinary runtime projection/newton pass.
        merge_initial_section_discrete_values(dae, &mut env, y, p, options.t_eval);
    }
    env.is_initial = options.use_initial;
    apply_seed_env_overrides(&mut env, seed_env);
    env
}

pub(crate) fn build_direct_seed_base_env(
    dae: &Dae,
    y: &[f64],
    p: &[f64],
    options: DirectAssignmentSeedOptions,
    seed_env: Option<&VarEnv<f64>>,
) -> VarEnv<f64> {
    let mut env = if options.bootstrap_initial_section {
        let mut startup_y = y.to_vec();
        rumoca_sim::runtime::startup::build_initial_section_env(
            dae,
            &mut startup_y,
            p,
            options.t_eval,
        )
    } else {
        rumoca_eval_dae::runtime::build_runtime_parameter_tail_env(dae, p, options.t_eval)
    };
    if !options.bootstrap_initial_section {
        let mut idx = 0usize;
        for (name, var) in dae
            .states
            .iter()
            .chain(dae.algebraics.iter())
            .chain(dae.outputs.iter())
        {
            rumoca_eval_dae::runtime::map_var_to_env(&mut env, name.as_str(), var, y, &mut idx);
        }
    } else {
        rumoca_eval_dae::runtime::refresh_env_solver_and_parameter_values(
            &mut env,
            dae,
            y,
            p,
            options.t_eval,
        );
    }
    env.is_initial = options.use_initial;
    apply_seed_env_overrides(&mut env, seed_env);
    env
}

pub(crate) fn refresh_runtime_direct_seed_env(
    env: &mut VarEnv<f64>,
    dae: &Dae,
    p: &[f64],
    options: DirectAssignmentSeedOptions,
    seed_env: Option<&VarEnv<f64>>,
) {
    env.set("time", options.t_eval);
    let mut pidx = 0usize;
    for (name, var) in &dae.parameters {
        rumoca_eval_dae::runtime::map_var_to_env(env, name.as_str(), var, p, &mut pidx);
    }
    env.is_initial = options.use_initial;
    apply_seed_env_overrides(env, seed_env);
}

pub(crate) fn refresh_direct_seed_env(
    env: &mut VarEnv<f64>,
    dae: &Dae,
    y: &[f64],
    p: &[f64],
    options: DirectAssignmentSeedOptions,
    seed_env: Option<&VarEnv<f64>>,
) {
    rumoca_eval_dae::runtime::refresh_env_solver_and_parameter_values(
        env,
        dae,
        y,
        p,
        options.t_eval,
    );
    env.is_initial = options.use_initial;
    apply_seed_env_overrides(env, seed_env);
}

pub(crate) fn apply_seeded_values_to_indices_without_env(
    y: &mut [f64],
    names: &[String],
    indices: &[usize],
    values: &[f64],
    n_x: usize,
) -> (bool, usize) {
    let mut changed = false;
    let mut updates = 0usize;
    for (slot, idx_ref) in indices.iter().enumerate() {
        let var_idx = *idx_ref;
        if var_idx < n_x || var_idx >= y.len() {
            continue;
        }
        let value = clamp_finite(*values.get(slot).unwrap_or(&0.0));
        if (y[var_idx] - value).abs() <= 1.0e-12 {
            continue;
        }
        y[var_idx] = value;
        if let Some(name) = names.get(var_idx) {
            log_ic_direct_seed(name, value);
        }
        changed = true;
        updates += 1;
    }
    (changed, updates)
}

pub(crate) struct CompiledDirectSeedContext {
    compiled_rows: CompiledRuntimeExpressionContext,
    rows_by_eq: HashMap<usize, std::ops::Range<usize>>,
}

pub(crate) struct DirectSeedPass<'a> {
    rows_by_eq: &'a HashMap<usize, std::ops::Range<usize>>,
    values: &'a [f64],
}

impl<'a> DirectSeedPass<'a> {
    fn values_for_eq(&self, eq: &Equation) -> Option<&'a [f64]> {
        let range = self.rows_by_eq.get(&equation_key(eq))?;
        self.values.get(range.clone())
    }
}

pub(crate) fn build_compiled_direct_seed_context(
    dae: &Dae,
    y_len: usize,
    options: DirectAssignmentSeedOptions,
    base_to_indices: &HashMap<String, Vec<usize>>,
) -> Result<Option<CompiledDirectSeedContext>, crate::SimError> {
    let scalarization = rumoca_phase_structural::scalarize::build_expression_scalarization_context(dae);
    let SolverNameIndexMaps { name_to_idx, .. } = build_solver_name_index_maps(dae, y_len);
    let n_x = count_states(dae);
    let target_dependencies =
        collect_runtime_direct_seed_target_dependencies(dae, n_x, &name_to_idx);
    let mut expressions = Vec::new();
    let mut rows_by_eq = HashMap::new();

    for eq in &dae.f_x {
        if eq.origin == "orphaned_variable_pin" {
            continue;
        }
        let Some((target, solution)) = direct_seed_assignment_from_equation(dae, eq, &name_to_idx)
        else {
            continue;
        };
        let normalized_solution = normalize_direct_seed_solver_solution(solution, &name_to_idx);
        if direct_seed_solution_is_runtime_clock_signal(dae, &normalized_solution) {
            continue;
        }
        let branch_ctx = BranchConditionContext {
            name_to_idx: &name_to_idx,
            n_x,
            y_len,
            target_dependencies: &target_dependencies,
        };
        if solution_has_solver_dependent_branch_condition(&normalized_solution, &branch_ctx) {
            continue;
        }
        if direct_seed_solution_requires_runtime_eval(&normalized_solution) {
            continue;
        }
        let target_size = direct_seed_target_size(dae, target.as_str(), base_to_indices);
        let range_start = expressions.len();
        expressions.extend(rumoca_phase_structural::scalarize::scalarize_expression_rows(
            &normalized_solution,
            target_size,
            &scalarization,
        ));
        rows_by_eq.insert(equation_key(eq), range_start..expressions.len());
    }

    if expressions.is_empty() {
        return Ok(None);
    }

    build_compiled_runtime_expression_context(dae, y_len, &expressions, options.use_initial, true)
        .map(|compiled_rows| {
            Some(CompiledDirectSeedContext {
                compiled_rows,
                rows_by_eq,
            })
        })
}

pub(crate) fn direct_seed_target_size(
    dae: &Dae,
    target: &str,
    base_to_indices: &HashMap<String, Vec<usize>>,
) -> usize {
    rumoca_sim::runtime::assignment::variable_size_for_assignment_name(dae, target)
        .or_else(|| {
            (!target.contains('['))
                .then(|| base_to_indices.get(target).map(Vec::len))
                .flatten()
        })
        .unwrap_or(1)
}

pub(crate) fn direct_seed_solution_is_runtime_clock_signal(
    dae: &Dae,
    solution: &Expression,
) -> bool {
    rumoca_sim::runtime::clock::sample_clock_arg_is_explicit_clock(dae, solution, &VarEnv::new())
}

pub(crate) fn direct_seed_solution_requires_runtime_eval(expr: &Expression) -> bool {
    match expr {
        // MLS §3.7.2 / §8.6 / §16.5.1: event and clock operators depend on
        // left-limit or tick semantics, which the current compiled PR2 rows do
        // not model directly. Keep those direct-assignment seeds on the
        // runtime evaluator instead of failing the whole compiled batch.
        Expression::BuiltinCall { function, args } => {
            matches!(
                function,
                dae::BuiltinFunction::Pre
                    | dae::BuiltinFunction::Sample
                    | dae::BuiltinFunction::Edge
                    | dae::BuiltinFunction::Change
                    | dae::BuiltinFunction::Reinit
            ) || args.iter().any(direct_seed_solution_requires_runtime_eval)
        }
        Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            matches!(
                short,
                "previous"
                    | "hold"
                    | "Clock"
                    | "subSample"
                    | "superSample"
                    | "shiftSample"
                    | "backSample"
                    | "firstTick"
            ) || args.iter().any(direct_seed_solution_requires_runtime_eval)
        }
        Expression::Binary { lhs, rhs, .. } => {
            direct_seed_solution_requires_runtime_eval(lhs)
                || direct_seed_solution_requires_runtime_eval(rhs)
        }
        Expression::Unary { rhs, .. } => direct_seed_solution_requires_runtime_eval(rhs),
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                direct_seed_solution_requires_runtime_eval(cond)
                    || direct_seed_solution_requires_runtime_eval(value)
            }) || direct_seed_solution_requires_runtime_eval(else_branch)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => elements
            .iter()
            .any(direct_seed_solution_requires_runtime_eval),
        Expression::Range { start, step, end } => {
            direct_seed_solution_requires_runtime_eval(start)
                || step
                    .as_deref()
                    .is_some_and(direct_seed_solution_requires_runtime_eval)
                || direct_seed_solution_requires_runtime_eval(end)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            direct_seed_solution_requires_runtime_eval(expr)
                || indices
                    .iter()
                    .any(|idx| direct_seed_solution_requires_runtime_eval(&idx.range))
                || filter
                    .as_deref()
                    .is_some_and(direct_seed_solution_requires_runtime_eval)
        }
        Expression::Index { base, subscripts } => {
            direct_seed_solution_requires_runtime_eval(base)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(expr) => direct_seed_solution_requires_runtime_eval(expr),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        Expression::FieldAccess { base, .. } => direct_seed_solution_requires_runtime_eval(base),
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => false,
    }
}

pub(crate) fn condition_depends_on_unsolved_solver_unknown(
    condition: &Expression,
    name_to_idx: &HashMap<String, usize>,
    n_x: usize,
    y_len: usize,
    target_dependencies: &HashMap<String, Vec<String>>,
) -> bool {
    let mut refs = std::collections::HashSet::new();
    condition.collect_var_refs(&mut refs);
    let mut visiting = std::collections::HashSet::new();
    refs.into_iter().any(|name| {
        name_or_dependency_is_unsolved_solver_unknown(
            name.as_str(),
            name_to_idx,
            n_x,
            y_len,
            target_dependencies,
            &mut visiting,
        )
    })
}

pub(crate) fn name_or_dependency_is_unsolved_solver_unknown(
    name: &str,
    name_to_idx: &HashMap<String, usize>,
    n_x: usize,
    y_len: usize,
    target_dependencies: &HashMap<String, Vec<String>>,
    visiting: &mut std::collections::HashSet<String>,
) -> bool {
    if solver_idx_for_target(name, name_to_idx).is_some_and(|idx| idx >= n_x && idx < y_len) {
        return true;
    }
    if let Some(base) = dae::component_base_name(name)
        && base != name
        && solver_idx_for_target(base.as_str(), name_to_idx)
            .is_some_and(|idx| idx >= n_x && idx < y_len)
    {
        return true;
    }
    name_dependency_is_unsolved_solver_unknown(
        name,
        name_to_idx,
        n_x,
        y_len,
        target_dependencies,
        visiting,
    ) || dae::component_base_name(name).is_some_and(|base| {
        base != name
            && name_dependency_is_unsolved_solver_unknown(
                base.as_str(),
                name_to_idx,
                n_x,
                y_len,
                target_dependencies,
                visiting,
            )
    })
}

pub(crate) fn name_dependency_is_unsolved_solver_unknown(
    name: &str,
    name_to_idx: &HashMap<String, usize>,
    n_x: usize,
    y_len: usize,
    target_dependencies: &HashMap<String, Vec<String>>,
    visiting: &mut std::collections::HashSet<String>,
) -> bool {
    if !visiting.insert(name.to_string()) {
        return false;
    }
    let result = target_dependencies.get(name).is_some_and(|deps| {
        deps.iter().any(|dep| {
            name_or_dependency_is_unsolved_solver_unknown(
                dep.as_str(),
                name_to_idx,
                n_x,
                y_len,
                target_dependencies,
                visiting,
            )
        })
    });
    visiting.remove(name);
    result
}

pub(crate) struct BranchConditionContext<'a> {
    name_to_idx: &'a HashMap<String, usize>,
    n_x: usize,
    y_len: usize,
    target_dependencies: &'a HashMap<String, Vec<String>>,
}

pub(crate) fn solution_has_solver_dependent_branch_condition(
    expr: &Expression,
    ctx: &BranchConditionContext<'_>,
) -> bool {
    match expr {
        Expression::If {
            branches,
            else_branch,
        } => {
            // MLS §3.3 / §3.7.5 / Appendix B: noEvent/smooth may suppress event
            // generation, but branch selection still depends on the current
            // continuous solve state. Runtime direct seeding must not pin those
            // branch-valued equations from stale algebraic guesses.
            solution_if_branches_have_solver_dependent_condition(branches, else_branch, ctx)
        }
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            solution_branch_condition_in_exprs(args, ctx)
        }
        Expression::Binary { lhs, rhs, .. } => {
            solution_has_solver_dependent_branch_condition(lhs, ctx)
                || solution_has_solver_dependent_branch_condition(rhs, ctx)
        }
        Expression::Unary { rhs, .. } | Expression::FieldAccess { base: rhs, .. } => {
            solution_has_solver_dependent_branch_condition(rhs, ctx)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            solution_branch_condition_in_exprs(elements, ctx)
        }
        Expression::Range { start, step, end } => {
            solution_has_solver_dependent_branch_condition(start, ctx)
                || step
                    .as_deref()
                    .is_some_and(|expr| solution_has_solver_dependent_branch_condition(expr, ctx))
                || solution_has_solver_dependent_branch_condition(end, ctx)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            solution_has_solver_dependent_branch_condition(expr, ctx)
                || indices
                    .iter()
                    .any(|index| solution_has_solver_dependent_branch_condition(&index.range, ctx))
                || filter
                    .as_deref()
                    .is_some_and(|expr| solution_has_solver_dependent_branch_condition(expr, ctx))
        }
        Expression::Index { base, subscripts } => {
            solution_has_solver_dependent_branch_condition(base, ctx)
                || solution_branch_condition_in_subscripts(subscripts, ctx)
        }
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => false,
    }
}

pub(crate) fn solution_if_branches_have_solver_dependent_condition(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    ctx: &BranchConditionContext<'_>,
) -> bool {
    branches.iter().any(|(condition, value)| {
        condition_depends_on_unsolved_solver_unknown(
            condition,
            ctx.name_to_idx,
            ctx.n_x,
            ctx.y_len,
            ctx.target_dependencies,
        ) || solution_has_solver_dependent_branch_condition(value, ctx)
    }) || solution_has_solver_dependent_branch_condition(else_branch, ctx)
}

pub(crate) fn solution_branch_condition_in_exprs(
    exprs: &[Expression],
    ctx: &BranchConditionContext<'_>,
) -> bool {
    exprs
        .iter()
        .any(|expr| solution_has_solver_dependent_branch_condition(expr, ctx))
}

pub(crate) fn solution_branch_condition_in_subscripts(
    subscripts: &[dae::Subscript],
    ctx: &BranchConditionContext<'_>,
) -> bool {
    subscripts.iter().any(|subscript| match subscript {
        dae::Subscript::Expr(expr) => solution_has_solver_dependent_branch_condition(expr, ctx),
        dae::Subscript::Index(_) | dae::Subscript::Colon => false,
    })
}

pub(crate) fn direct_seed_target_dims(dae: &Dae, target: &str, width: usize) -> Vec<i64> {
    let lookup = |name: &str| {
        dae.states
            .get(&dae::VarName::new(name))
            .or_else(|| dae.algebraics.get(&dae::VarName::new(name)))
            .or_else(|| dae.outputs.get(&dae::VarName::new(name)))
            .or_else(|| dae.inputs.get(&dae::VarName::new(name)))
            .or_else(|| dae.parameters.get(&dae::VarName::new(name)))
            .or_else(|| dae.constants.get(&dae::VarName::new(name)))
            .or_else(|| dae.discrete_reals.get(&dae::VarName::new(name)))
            .or_else(|| dae.discrete_valued.get(&dae::VarName::new(name)))
            .or_else(|| dae.derivative_aliases.get(&dae::VarName::new(name)))
            .map(|var| var.dims.clone())
    };

    lookup(target)
        .or_else(|| dae::component_base_name(target).and_then(|base| lookup(&base)))
        .unwrap_or_else(|| vec![width as i64])
}

pub(crate) fn apply_seed_values_to_env_only_with_dims(
    env: &mut VarEnv<f64>,
    target: &str,
    dims: &[i64],
    values: &[f64],
) -> bool {
    let mut staged = VarEnv::new();
    rumoca_eval_dae::runtime::set_array_entries(&mut staged, target, dims, values);
    let mut changed = false;
    for (name, value) in staged.vars {
        if env
            .vars
            .get(name.as_str())
            .is_none_or(|existing| (existing - value).abs() > 1.0e-12)
        {
            env.set(name.as_str(), value);
            changed = true;
        }
    }
    changed
}

pub(crate) fn apply_seed_values_to_env_only(
    dae: &Dae,
    env: &mut VarEnv<f64>,
    target: &str,
    values: &[f64],
) -> bool {
    let dims = direct_seed_target_dims(dae, target, values.len());
    apply_seed_values_to_env_only_with_dims(env, target, &dims, values)
}

pub(crate) struct DirectSeedPassContext<'a> {
    dae: &'a Dae,
    n_x: usize,
    y_len: usize,
    options: DirectAssignmentSeedOptions,
    names: &'a [String],
    name_to_idx: &'a HashMap<String, usize>,
    base_to_indices: &'a HashMap<String, Vec<usize>>,
    target_assignment_stats:
        &'a HashMap<String, rumoca_sim::runtime::assignment::DirectAssignmentTargetStats>,
}

pub(crate) fn compiled_direct_seed_values<'a>(
    eq: &Equation,
    compiled_pass: Option<&'a DirectSeedPass<'a>>,
) -> Option<&'a [f64]> {
    compiled_pass.and_then(|pass| pass.values_for_eq(eq))
}

pub(crate) fn skip_seed_direct_assignment_candidate(
    ctx: &DirectSeedPassContext<'_>,
    target: &str,
    solution: &Expression,
    is_alias_solution: bool,
    source_known: bool,
    trace_target: bool,
) -> bool {
    let target_stats = ctx
        .target_assignment_stats
        .get(target)
        .copied()
        .unwrap_or_default();
    if target_stats.total > 1 && target_stats.non_alias != 1 {
        log_runtime_direct_seed_skip_multiple_assignments(trace_target, target, target_stats.total);
        return true;
    }
    direct_seed_solution_is_redundant_alias_varref(ctx.dae, solution, ctx.name_to_idx, target_stats)
        || (target_stats.total > 1 && target_stats.non_alias == 1 && is_alias_solution)
        || (!source_known && !ctx.options.allow_unsolved_solver_sources)
        || direct_seed_solution_is_runtime_clock_signal(ctx.dae, solution)
}

pub(crate) fn apply_seed_direct_assignment_equation(
    ctx: &DirectSeedPassContext<'_>,
    eq: &Equation,
    y: &mut [f64],
    env: &mut VarEnv<f64>,
    compiled_pass: Option<&DirectSeedPass<'_>>,
) -> (bool, usize) {
    if eq.origin == "orphaned_variable_pin" {
        return (false, 0);
    }
    let Some((target, solution)) =
        direct_seed_assignment_from_equation(ctx.dae, eq, ctx.name_to_idx)
    else {
        return (false, 0);
    };
    let normalized_solution = normalize_direct_seed_solver_solution(solution, ctx.name_to_idx);
    let is_alias_solution = direct_seed_solution_is_env_only_alias_varref(
        ctx.dae,
        &normalized_solution,
        ctx.name_to_idx,
    );
    if ctx.options.skip_unknown_alias_pairs && is_alias_solution {
        return (false, 0);
    }
    let trace_target = should_trace_direct_seed_target(target.as_str());
    let source_known = if trace_target || !ctx.options.allow_unsolved_solver_sources {
        rumoca_sim::runtime::assignment::direct_assignment_source_is_known(
            ctx.dae,
            &normalized_solution,
            ctx.n_x,
            ctx.y_len,
            |target| solver_idx_for_target(target, ctx.name_to_idx),
        )
    } else {
        true
    };
    if trace_target {
        eprintln!(
            "[sim-introspect] runtime direct seed candidate target={} source_known={} allow_unsolved={}",
            target, source_known, ctx.options.allow_unsolved_solver_sources
        );
    }
    if skip_seed_direct_assignment_candidate(
        ctx,
        target.as_str(),
        &normalized_solution,
        is_alias_solution,
        source_known,
        trace_target,
    ) {
        return (false, 0);
    }

    let target_size = direct_seed_target_size(ctx.dae, target.as_str(), ctx.base_to_indices);
    if let Some(result) = apply_seed_direct_assignment_array_target(
        ctx,
        eq,
        y,
        env,
        compiled_pass,
        SeedArrayTarget {
            name: target.as_str(),
            solution: &normalized_solution,
            size: target_size,
        },
    ) {
        return result;
    }

    let value = compiled_direct_seed_values(eq, compiled_pass)
        .and_then(|values| values.first().copied())
        .map(clamp_finite)
        .unwrap_or_else(|| {
            clamp_finite(rumoca_eval_dae::runtime::eval_expr::<f64>(
                &normalized_solution,
                env,
            ))
        });
    if trace_target {
        eprintln!(
            "[sim-introspect] runtime direct seed eval target={} solver_idx={:?} value={}",
            target,
            solver_idx_for_target(target.as_str(), ctx.name_to_idx),
            value
        );
    }

    if let Some(var_idx) = solver_idx_for_target(target.as_str(), ctx.name_to_idx) {
        if var_idx < ctx.n_x || var_idx >= y.len() {
            return (false, 0);
        }
        if (y[var_idx] - value).abs() <= 1e-12 {
            return (false, 0);
        }

        y[var_idx] = value;
        if let Some(name) = ctx.names.get(var_idx) {
            env.set(name, value);
            log_ic_direct_seed(name, value);
        }
        return (true, 1);
    }

    if env
        .vars
        .get(target.as_str())
        .is_none_or(|existing| (existing - value).abs() > 1.0e-12)
    {
        env.set(target.as_str(), value);
        return (true, 0);
    }

    (false, 0)
}

pub(crate) fn apply_seed_direct_assignment_array_target(
    ctx: &DirectSeedPassContext<'_>,
    eq: &Equation,
    y: &mut [f64],
    env: &mut VarEnv<f64>,
    compiled_pass: Option<&DirectSeedPass<'_>>,
    target: SeedArrayTarget<'_>,
) -> Option<(bool, usize)> {
    if target.name.contains('[') || target.size <= 1 {
        return None;
    }
    let values = compiled_direct_seed_values(eq, compiled_pass)
        .map(|values| values.iter().copied().map(clamp_finite).collect::<Vec<_>>())
        .unwrap_or_else(|| evaluate_direct_assignment_values(target.solution, env, target.size));
    Some(
        if let Some(indices) = ctx
            .base_to_indices
            .get(target.name)
            .filter(|indices| !indices.is_empty())
        {
            apply_seeded_values_to_indices(y, env, ctx.names, indices, &values, ctx.n_x)
        } else {
            (
                apply_seed_values_to_env_only(ctx.dae, env, target.name, &values),
                0,
            )
        },
    )
}

#[derive(Clone, Copy)]
pub(crate) struct SeedArrayTarget<'a> {
    name: &'a str,
    solution: &'a Expression,
    size: usize,
}

pub(crate) fn runtime_seed_values<'a>(
    candidate: &RuntimeDirectSeedCandidate,
    env: &mut VarEnv<f64>,
    compiled_values: Option<&'a [f64]>,
    owned_values: &'a mut Option<Vec<f64>>,
) -> &'a [f64] {
    if let Some(values) = compiled_values.and_then(|values| {
        candidate
            .row_range
            .as_ref()
            .and_then(|range| values.get(range.clone()))
    }) {
        return values;
    }
    owned_values.get_or_insert_with(|| {
        evaluate_direct_assignment_values(&candidate.solution, env, candidate.value_count)
    })
}

pub(crate) fn apply_runtime_seed_array_candidate(
    runtime_ctx: &RuntimeDirectSeedContext,
    candidate: &RuntimeDirectSeedCandidate,
    values: &[f64],
    y: &mut [f64],
    env: &mut VarEnv<f64>,
    store_solver_values_in_env: bool,
    blocked_solver_cols: Option<&[bool]>,
) -> (bool, usize) {
    match &candidate.apply {
        RuntimeDirectSeedApply::SolverArray { indices } => {
            if let Some(blocked_solver_cols) = blocked_solver_cols {
                let mut blocked_apply = BlockedRuntimeSeedApply {
                    y,
                    env,
                    names: &runtime_ctx.names,
                    n_x: runtime_ctx.n_x,
                    blocked_solver_cols,
                    store_solver_values_in_env,
                };
                return apply_runtime_seeded_values_to_indices_with_blocked_cols(
                    indices,
                    values,
                    &mut blocked_apply,
                );
            }
            let values = values.iter().copied().map(clamp_finite).collect::<Vec<_>>();
            if store_solver_values_in_env {
                apply_seeded_values_to_indices(
                    y,
                    env,
                    &runtime_ctx.names,
                    indices,
                    &values,
                    runtime_ctx.n_x,
                )
            } else {
                apply_seeded_values_to_indices_without_env(
                    y,
                    &runtime_ctx.names,
                    indices,
                    &values,
                    runtime_ctx.n_x,
                )
            }
        }
        RuntimeDirectSeedApply::EnvArray { dims } => {
            let values = values.iter().copied().map(clamp_finite).collect::<Vec<_>>();
            (
                apply_seed_values_to_env_only_with_dims(
                    env,
                    candidate.target.as_str(),
                    dims,
                    &values,
                ),
                0,
            )
        }
        _ => unreachable!("array seed helper requires an array candidate"),
    }
}

pub(crate) fn apply_runtime_seed_scalar_candidate(
    candidate: &RuntimeDirectSeedCandidate,
    values: &[f64],
    y: &mut [f64],
    env: &mut VarEnv<f64>,
    store_solver_values_in_env: bool,
    blocked_solver_cols: Option<&[bool]>,
) -> (bool, usize) {
    let value = values.first().copied().map(clamp_finite).unwrap_or(0.0);
    match &candidate.apply {
        RuntimeDirectSeedApply::SolverScalar {
            solver_idx,
            solver_name,
        } => {
            if blocked_solver_cols
                .and_then(|blocked| blocked.get(*solver_idx))
                .copied()
                .unwrap_or(false)
            {
                if candidate.trace_target {
                    eprintln!(
                        "[sim-introspect] runtime direct seed skipped target={} solver_idx={} reason=fixed_runtime_projection_col",
                        candidate.target, solver_idx
                    );
                }
                return (false, 0);
            }
            if candidate.trace_target {
                eprintln!(
                    "[sim-introspect] runtime direct seed eval target={} solver_idx={:?} value={}",
                    candidate.target, solver_idx, value
                );
            }
            if (y[*solver_idx] - value).abs() <= 1.0e-12 {
                return (false, 0);
            }
            y[*solver_idx] = value;
            if store_solver_values_in_env {
                env.set(solver_name, value);
            }
            log_ic_direct_seed(solver_name, value);
            (true, 1)
        }
        RuntimeDirectSeedApply::EnvScalar => {
            if candidate.trace_target {
                eprintln!(
                    "[sim-introspect] runtime direct seed eval target={} solver_idx=None value={}",
                    candidate.target, value
                );
            }
            if env
                .vars
                .get(candidate.target.as_str())
                .is_none_or(|existing| (existing - value).abs() > 1.0e-12)
            {
                env.set(candidate.target.as_str(), value);
                return (true, 0);
            }
            (false, 0)
        }
        _ => unreachable!("scalar seed helper requires a scalar candidate"),
    }
}

pub(crate) fn apply_runtime_seed_candidate(
    runtime_ctx: &RuntimeDirectSeedContext,
    candidate: &RuntimeDirectSeedCandidate,
    y: &mut [f64],
    env: &mut VarEnv<f64>,
    compiled_values: Option<&[f64]>,
    store_solver_values_in_env: bool,
    blocked_solver_cols: Option<&[bool]>,
) -> (bool, usize) {
    let mut owned_values = None;
    let values = runtime_seed_values(candidate, env, compiled_values, &mut owned_values);
    match &candidate.apply {
        RuntimeDirectSeedApply::SolverArray { .. } | RuntimeDirectSeedApply::EnvArray { .. } => {
            apply_runtime_seed_array_candidate(
                runtime_ctx,
                candidate,
                values,
                y,
                env,
                store_solver_values_in_env,
                blocked_solver_cols,
            )
        }
        RuntimeDirectSeedApply::SolverScalar { .. } | RuntimeDirectSeedApply::EnvScalar => {
            apply_runtime_seed_scalar_candidate(
                candidate,
                values,
                y,
                env,
                store_solver_values_in_env,
                blocked_solver_cols,
            )
        }
    }
}

pub(crate) struct BlockedRuntimeSeedApply<'a> {
    y: &'a mut [f64],
    env: &'a mut VarEnv<f64>,
    names: &'a [String],
    n_x: usize,
    blocked_solver_cols: &'a [bool],
    store_solver_values_in_env: bool,
}

pub(crate) fn apply_runtime_seeded_values_to_indices_with_blocked_cols(
    indices: &[usize],
    values: &[f64],
    ctx: &mut BlockedRuntimeSeedApply<'_>,
) -> (bool, usize) {
    let mut changed = false;
    let mut updates = 0usize;
    for (slot, &idx) in indices.iter().enumerate() {
        if idx < ctx.n_x
            || idx >= ctx.y.len()
            || ctx.blocked_solver_cols.get(idx).copied().unwrap_or(false)
        {
            continue;
        }
        let value = clamp_finite(*values.get(slot).unwrap_or(&0.0));
        if (ctx.y[idx] - value).abs() <= 1.0e-12 {
            continue;
        }
        ctx.y[idx] = value;
        if ctx.store_solver_values_in_env
            && let Some(name) = ctx.names.get(idx)
        {
            ctx.env.set(name, value);
        }
        log_ic_direct_seed(
            ctx.names
                .get(idx)
                .map(String::as_str)
                .unwrap_or("<blocked-runtime-seed>"),
            value,
        );
        changed = true;
        updates += 1;
    }
    (changed, updates)
}

pub(crate) fn seed_direct_assignment_initial_values_with_overrides(
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    n_x: usize,
    seed_env: Option<&VarEnv<f64>>,
    options: DirectAssignmentSeedOptions,
) -> usize {
    if dae.f_x.len() <= n_x || y.is_empty() {
        return 0;
    }

    let SolverNameIndexMaps {
        names,
        name_to_idx,
        base_to_indices,
    } = build_solver_name_index_maps(dae, y.len());
    let target_assignment_stats =
        rumoca_sim::runtime::assignment::collect_direct_assignment_target_stats(dae, n_x, false);
    let pass_ctx = DirectSeedPassContext {
        dae,
        n_x,
        y_len: y.len(),
        options,
        names: &names,
        name_to_idx: &name_to_idx,
        base_to_indices: &base_to_indices,
        target_assignment_stats: &target_assignment_stats,
    };
    let compiled_ctx = build_compiled_direct_seed_context(dae, y.len(), options, &base_to_indices)
        .expect("compiled direct-seed rows required on the live diffsol path");

    let mut updates = 0usize;
    let max_passes = y.len().max(4);
    let mut env = build_direct_seed_base_env(dae, y, p, options, seed_env);
    let mut y_scratch = Vec::with_capacity(y.len());
    let mut compiled_scalar_out = Vec::new();
    for _ in 0..max_passes {
        let mut changed = false;
        let compiled_pass = compiled_ctx.as_ref().map(|compiled| DirectSeedPass {
            rows_by_eq: &compiled.rows_by_eq,
            values: eval_compiled_runtime_expressions_from_env(
                &compiled.compiled_rows,
                y,
                &env,
                p,
                options.t_eval,
                &mut y_scratch,
                &mut compiled_scalar_out,
            ),
        });

        for eq in dae.f_x.iter().skip(n_x) {
            let (eq_changed, eq_updates) = apply_seed_direct_assignment_equation(
                &pass_ctx,
                eq,
                y,
                &mut env,
                compiled_pass.as_ref(),
            );
            changed |= eq_changed;
            updates += eq_updates;
        }

        if !changed {
            break;
        }

        rumoca_eval_dae::runtime::refresh_env_solver_and_parameter_values(
            &mut env,
            dae,
            y,
            p,
            options.t_eval,
        );
        env.is_initial = options.use_initial;
        apply_seed_env_overrides(&mut env, seed_env);
    }
    updates
}

#[cfg(test)]
pub(crate) struct RuntimeDirectPropagationContext<'a> {
    dae: &'a Dae,
    n_x: usize,
    names: &'a [String],
    name_to_idx: &'a HashMap<String, usize>,
    base_to_indices: &'a HashMap<String, Vec<usize>>,
    target_assignment_stats:
        &'a HashMap<String, rumoca_sim::runtime::assignment::DirectAssignmentTargetStats>,
}

#[cfg(test)]
pub(crate) fn apply_runtime_direct_propagation_equation(
    ctx: &RuntimeDirectPropagationContext<'_>,
    eq: &Equation,
    y: &mut [f64],
    env: &mut VarEnv<f64>,
    compiled_pass: Option<&DirectSeedPass<'_>>,
) -> (bool, usize) {
    if eq.origin == "orphaned_variable_pin" {
        return (false, 0);
    }
    let Some((target, solution)) =
        direct_seed_assignment_from_equation(ctx.dae, eq, ctx.name_to_idx)
    else {
        return (false, 0);
    };
    let target_stats = ctx
        .target_assignment_stats
        .get(target.as_str())
        .copied()
        .unwrap_or_default();
    if direct_seed_solution_is_redundant_alias_varref(
        ctx.dae,
        solution,
        ctx.name_to_idx,
        target_stats,
    ) {
        return (false, 0);
    }
    if target_stats.total > 1 && target_stats.non_alias != 1 {
        maybe_log_runtime_direct_propagation_skip(target.as_str(), target_stats.total);
        return (false, 0);
    }

    if !target.contains('[')
        && let Some(indices) = ctx.base_to_indices.get(target.as_str())
        && indices.len() > 1
    {
        let values = compiled_pass
            .and_then(|pass| pass.values_for_eq(eq))
            .map(|values| values.iter().copied().map(clamp_finite).collect())
            .unwrap_or_else(|| evaluate_direct_assignment_values(solution, env, indices.len()));
        return apply_runtime_values_to_indices(y, env, ctx.names, indices, &values, ctx.n_x);
    }

    let value = compiled_pass
        .and_then(|pass| pass.values_for_eq(eq))
        .and_then(|values| values.first().copied())
        .map(clamp_finite)
        .unwrap_or_else(|| clamp_finite(eval_expr::<f64>(solution, env)));
    let mut changed = false;
    let mut updates = 0usize;
    if let Some(var_idx) = solver_idx_for_target(target.as_str(), ctx.name_to_idx)
        && var_idx >= ctx.n_x
        && var_idx < y.len()
        && (y[var_idx] - value).abs() > 1.0e-12
    {
        y[var_idx] = value;
        changed = true;
        updates += 1;
    }
    if env
        .vars
        .get(target.as_str())
        .is_none_or(|existing| (existing - value).abs() > 1.0e-12)
    {
        env.set(target.as_str(), value);
        changed = true;
        updates += 1;
    }
    (changed, updates)
}

#[cfg(test)]
pub(crate) fn propagate_runtime_direct_assignments_from_env(
    dae: &Dae,
    y: &mut [f64],
    n_x: usize,
    env: &mut VarEnv<f64>,
) -> usize {
    if dae.f_x.len() <= n_x || y.is_empty() {
        return 0;
    }

    let SolverNameIndexMaps {
        names,
        name_to_idx,
        base_to_indices,
    } = build_solver_name_index_maps(dae, y.len());
    let target_assignment_stats =
        rumoca_sim::runtime::assignment::collect_direct_assignment_target_stats(dae, n_x, false);
    let pass_ctx = RuntimeDirectPropagationContext {
        dae,
        n_x,
        names: &names,
        name_to_idx: &name_to_idx,
        base_to_indices: &base_to_indices,
        target_assignment_stats: &target_assignment_stats,
    };
    let params = default_params(dae);
    let t_eval = env.get("time");
    let compiled_ctx = build_compiled_direct_seed_context(
        dae,
        y.len(),
        DirectAssignmentSeedOptions {
            use_initial: false,
            t_eval,
            skip_unknown_alias_pairs: true,
            allow_unsolved_solver_sources: true,
            bootstrap_initial_section: false,
        },
        &base_to_indices,
    )
    .ok()
    .flatten();

    let mut updates = 0usize;
    let max_passes = y.len().max(4);
    let mut y_scratch = Vec::with_capacity(y.len());
    let mut compiled_scalar_out = Vec::new();
    for _ in 0..max_passes {
        let mut changed = false;
        let compiled_pass = compiled_ctx.as_ref().map(|compiled| DirectSeedPass {
            rows_by_eq: &compiled.rows_by_eq,
            values: eval_compiled_runtime_expressions_from_env(
                &compiled.compiled_rows,
                y,
                env,
                &params,
                t_eval,
                &mut y_scratch,
                &mut compiled_scalar_out,
            ),
        });
        for eq in dae.f_x.iter().skip(n_x) {
            let (eq_changed, eq_updates) = apply_runtime_direct_propagation_equation(
                &pass_ctx,
                eq,
                y,
                env,
                compiled_pass.as_ref(),
            );
            changed |= eq_changed;
            updates += eq_updates;
        }
        if !changed {
            break;
        }
    }

    updates
}
