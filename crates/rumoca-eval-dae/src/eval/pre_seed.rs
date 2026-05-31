use super::*;

fn pre_seed_array_fill_default() -> f64 {
    0.0
}

pub(super) fn try_seed_var_from_pre_store(
    env: &mut VarEnv<f64>,
    name: &str,
    var: &rumoca_ir_dae::Variable,
) -> bool {
    let trace_pre = crate::trace::introspect_enabled();
    let sz = var.size();
    if sz <= 1 {
        if let Some(value) = lookup_pre_value_in(&env.runtime, name) {
            if trace_pre && name.contains("signalSource") {
                tracing::debug!(target: "rumoca_eval_dae::introspect", "pre-seed scalar {} = {}", name, value);
            }
            env.set(name, value);
            return true;
        }
        return false;
    }

    let mut values = Vec::with_capacity(sz);
    let mut found_any = false;
    for flat_idx in 0..sz {
        let key = dae::scalar_name_text_for_flat_index(name, &var.dims, flat_idx);
        if let Some(value) = lookup_pre_value_in(&env.runtime, &key) {
            values.push(value);
            found_any = true;
        } else {
            values.push(f64::NAN);
        }
    }

    let fallback = lookup_pre_value_in(&env.runtime, name)
        .or_else(|| values.iter().copied().find(|v| v.is_finite()));
    if !found_any && fallback.is_none() {
        if trace_pre && name.contains("signalSource") {
            tracing::debug!(target: "rumoca_eval_dae::introspect", "pre-seed array {} missing", name);
        }
        return false;
    }

    let fill = fallback.unwrap_or_else(pre_seed_array_fill_default);
    for value in &mut values {
        if !value.is_finite() {
            *value = fill;
        }
    }
    set_array_entries(env, name, &var.dims, &values);
    if trace_pre && name.contains("signalSource") {
        tracing::debug!(
            target: "rumoca_eval_dae::introspect",
            "pre-seed array {} size={}",
            name,
            values.len()
        );
    }
    true
}
