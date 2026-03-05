use std::collections::{HashMap, HashSet};

use rumoca_eval_flat::eval;
use rumoca_ir_dae as dae;

use crate::equation_scalarize::build_output_names;
use crate::runtime::event::{EventSettleInput, settle_runtime_event_updates_default};
use crate::sim_types::SimVariableMeta;
use crate::simulation::diagnostics::truncate_debug;

struct VariableSource<'a> {
    var: &'a dae::Variable,
    role: &'static str,
    is_state: bool,
}

fn scalar_channel_names_from_vars<'a>(
    vars: impl Iterator<Item = (&'a dae::VarName, &'a dae::Variable)>,
) -> Vec<String> {
    let mut names = Vec::new();
    for (name, var) in vars {
        let size = var.size();
        if size <= 1 {
            names.push(name.as_str().to_string());
        } else {
            for i in 1..=size {
                names.push(format!("{}[{}]", name.as_str(), i));
            }
        }
    }
    names
}

pub fn build_visible_result_names(dae_model: &dae::Dae) -> Vec<String> {
    let mut names = build_output_names(dae_model);
    names.extend(scalar_channel_names_from_vars(
        dae_model.discrete_reals.iter(),
    ));
    names.extend(scalar_channel_names_from_vars(
        dae_model.discrete_valued.iter(),
    ));
    names
}

pub fn collect_discrete_channel_names(dae_model: &dae::Dae) -> Vec<String> {
    scalar_channel_names_from_vars(
        dae_model
            .discrete_reals
            .iter()
            .chain(dae_model.discrete_valued.iter()),
    )
}

fn expr_uses_event_dependent_discrete(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            matches!(
                function,
                dae::BuiltinFunction::Pre
                    | dae::BuiltinFunction::Sample
                    | dae::BuiltinFunction::Edge
                    | dae::BuiltinFunction::Change
                    | dae::BuiltinFunction::Reinit
                    | dae::BuiltinFunction::Initial
            ) || args.iter().any(expr_uses_event_dependent_discrete)
        }
        dae::Expression::FunctionCall { name, args, .. } => {
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
            ) || args.iter().any(expr_uses_event_dependent_discrete)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_uses_event_dependent_discrete(lhs) || expr_uses_event_dependent_discrete(rhs)
        }
        dae::Expression::Unary { rhs, .. } => expr_uses_event_dependent_discrete(rhs),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expr_uses_event_dependent_discrete(cond)
                    || expr_uses_event_dependent_discrete(value)
            }) || expr_uses_event_dependent_discrete(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(expr_uses_event_dependent_discrete)
        }
        dae::Expression::Range { start, step, end } => {
            expr_uses_event_dependent_discrete(start)
                || step
                    .as_ref()
                    .is_some_and(|value| expr_uses_event_dependent_discrete(value))
                || expr_uses_event_dependent_discrete(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_uses_event_dependent_discrete(expr)
                || indices
                    .iter()
                    .any(|index| expr_uses_event_dependent_discrete(&index.range))
                || filter
                    .as_ref()
                    .is_some_and(|value| expr_uses_event_dependent_discrete(value))
        }
        dae::Expression::Index { base, subscripts } => {
            expr_uses_event_dependent_discrete(base)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(value) => expr_uses_event_dependent_discrete(value),
                    _ => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_uses_event_dependent_discrete(base),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn collect_recomputable_discrete_targets(dae_model: &dae::Dae) -> HashSet<String> {
    let mut targets = HashSet::new();
    for eq in dae_model.f_z.iter().chain(dae_model.f_m.iter()) {
        let Some(lhs) = eq.lhs.as_ref() else {
            continue;
        };
        if expr_uses_event_dependent_discrete(&eq.rhs) {
            continue;
        }
        targets.insert(lhs.as_str().to_string());
    }
    targets
}

pub fn evaluate_runtime_discrete_channels(
    dae_model: &dae::Dae,
    n_x: usize,
    param_values: &[f64],
    times: &[f64],
    solver_names: &[String],
    solver_data: &[Vec<f64>],
) -> (Vec<String>, Vec<Vec<f64>>) {
    let recomputable_targets = collect_recomputable_discrete_targets(dae_model);
    let discrete_names: Vec<String> = collect_discrete_channel_names(dae_model)
        .into_iter()
        .filter(|name| {
            let base = dae::component_base_name(name).unwrap_or_else(|| name.to_string());
            recomputable_targets.contains(&base)
        })
        .collect();
    if discrete_names.is_empty() || times.is_empty() {
        return (Vec::new(), Vec::new());
    }

    let solver_name_to_idx: HashMap<&str, usize> = solver_names
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.as_str(), idx))
        .collect();
    let solver_len = solver_data.len().min(solver_names.len());
    let mut discrete_data: Vec<Vec<f64>> = discrete_names
        .iter()
        .map(|_| Vec::with_capacity(times.len()))
        .collect();
    eval::clear_pre_values();

    for (sample_idx, &t_eval) in times.iter().enumerate() {
        let mut y = vec![0.0; solver_len];
        for (col_idx, series) in solver_data.iter().enumerate().take(solver_len) {
            if let Some(value) = series.get(sample_idx).copied() {
                y[col_idx] = value;
            }
        }
        let env = settle_runtime_event_updates_default(EventSettleInput {
            dae: dae_model,
            y: &mut y,
            p: param_values,
            n_x,
            t_eval,
        });
        for (channel_idx, name) in discrete_names.iter().enumerate() {
            let value = env
                .vars
                .get(name.as_str())
                .copied()
                .or_else(|| {
                    solver_name_to_idx
                        .get(name.as_str())
                        .and_then(|idx| y.get(*idx).copied())
                })
                .unwrap_or(0.0);
            discrete_data[channel_idx].push(value);
        }
        eval::seed_pre_values_from_env(&env);
    }

    (discrete_names, discrete_data)
}

pub fn merge_runtime_discrete_channels(
    final_names: &mut Vec<String>,
    final_data: &mut Vec<Vec<f64>>,
    discrete_names: Vec<String>,
    discrete_data: Vec<Vec<f64>>,
) {
    if discrete_names.is_empty() {
        return;
    }
    let mut existing_idx: HashMap<String, usize> = final_names
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.clone(), idx))
        .collect();

    for (name, series) in discrete_names.into_iter().zip(discrete_data) {
        if let Some(idx) = existing_idx.get(&name).copied() {
            if idx < final_data.len() {
                final_data[idx] = series;
            }
            continue;
        }
        let next = final_names.len();
        existing_idx.insert(name.clone(), next);
        final_names.push(name);
        final_data.push(series);
    }
}

fn lookup_variable_exact<'a>(dae_model: &'a dae::Dae, name: &str) -> Option<VariableSource<'a>> {
    let key = dae::VarName::new(name.to_string());
    if let Some(var) = dae_model.states.get(&key) {
        return Some(VariableSource {
            var,
            role: "state",
            is_state: true,
        });
    }
    if let Some(var) = dae_model.algebraics.get(&key) {
        return Some(VariableSource {
            var,
            role: "algebraic",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.outputs.get(&key) {
        return Some(VariableSource {
            var,
            role: "output",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.inputs.get(&key) {
        return Some(VariableSource {
            var,
            role: "input",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.parameters.get(&key) {
        return Some(VariableSource {
            var,
            role: "parameter",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.constants.get(&key) {
        return Some(VariableSource {
            var,
            role: "constant",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.discrete_reals.get(&key) {
        return Some(VariableSource {
            var,
            role: "discrete-real",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.discrete_valued.get(&key) {
        return Some(VariableSource {
            var,
            role: "discrete-valued",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.derivative_aliases.get(&key) {
        return Some(VariableSource {
            var,
            role: "derivative-alias",
            is_state: false,
        });
    }
    None
}

fn trim_trailing_scalar_indices(name: &str) -> &str {
    let mut trimmed = name;
    loop {
        if !trimmed.ends_with(']') {
            break;
        }
        let Some(open_idx) = trimmed.rfind('[') else {
            break;
        };
        let index_text = &trimmed[(open_idx + 1)..(trimmed.len() - 1)];
        if index_text.is_empty() || !index_text.chars().all(|c| c.is_ascii_digit()) {
            break;
        }
        trimmed = &trimmed[..open_idx];
    }
    trimmed
}

fn lookup_variable_source<'a>(dae_model: &'a dae::Dae, name: &str) -> Option<VariableSource<'a>> {
    lookup_variable_exact(dae_model, name).or_else(|| {
        let base = trim_trailing_scalar_indices(name);
        if base != name {
            lookup_variable_exact(dae_model, base)
        } else {
            None
        }
    })
}

fn format_meta_expr(expr: &dae::Expression) -> String {
    truncate_debug(&format!("{expr:?}"), 160)
}

fn classify_role(role: &str, is_state: bool) -> (Option<String>, Option<String>, Option<String>) {
    if is_state {
        return (
            Some("Real".to_string()),
            Some("continuous".to_string()),
            Some("continuous-time".to_string()),
        );
    }

    match role {
        "algebraic" | "output" | "input" | "derivative-alias" => (
            Some("Real".to_string()),
            Some("continuous".to_string()),
            Some("continuous-time".to_string()),
        ),
        "parameter" => (
            Some("Real".to_string()),
            Some("parameter".to_string()),
            Some("static".to_string()),
        ),
        "constant" => (
            Some("Real".to_string()),
            Some("constant".to_string()),
            Some("static".to_string()),
        ),
        "discrete-real" => (
            Some("Real".to_string()),
            Some("discrete".to_string()),
            Some("event-discrete".to_string()),
        ),
        "discrete-valued" => (
            Some("Boolean/Integer/Enum".to_string()),
            Some("discrete".to_string()),
            Some("event-discrete".to_string()),
        ),
        _ => (None, None, None),
    }
}

pub fn build_variable_meta(
    dae_model: &dae::Dae,
    names: &[String],
    n_states: usize,
) -> Vec<SimVariableMeta> {
    names
        .iter()
        .enumerate()
        .map(|(idx, name)| {
            if let Some(source) = lookup_variable_source(dae_model, name) {
                let (value_type, variability, time_domain) =
                    classify_role(source.role, source.is_state);
                SimVariableMeta {
                    name: name.clone(),
                    role: source.role.to_string(),
                    is_state: source.is_state,
                    value_type,
                    variability,
                    time_domain,
                    unit: source.var.unit.clone(),
                    start: source.var.start.as_ref().map(format_meta_expr),
                    min: source.var.min.as_ref().map(format_meta_expr),
                    max: source.var.max.as_ref().map(format_meta_expr),
                    nominal: source.var.nominal.as_ref().map(format_meta_expr),
                    fixed: source.var.fixed,
                    description: source.var.description.clone(),
                }
            } else {
                let inferred_is_state = idx < n_states;
                let inferred_role = if inferred_is_state {
                    "state"
                } else {
                    "unknown"
                };
                let (value_type, variability, time_domain) =
                    classify_role(inferred_role, inferred_is_state);
                SimVariableMeta {
                    name: name.clone(),
                    role: inferred_role.to_string(),
                    is_state: inferred_is_state,
                    value_type,
                    variability,
                    time_domain,
                    unit: None,
                    start: None,
                    min: None,
                    max: None,
                    nominal: None,
                    fixed: None,
                    description: None,
                }
            }
        })
        .collect()
}
