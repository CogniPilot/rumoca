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
            values.push(Some(value));
            found_any = true;
        } else {
            values.push(None);
        }
    }

    let fallback =
        lookup_pre_value_in(&env.runtime, name).or_else(|| values.iter().copied().flatten().next());
    if !found_any && fallback.is_none() {
        if trace_pre && name.contains("signalSource") {
            tracing::debug!(target: "rumoca_eval_dae::introspect", "pre-seed array {} missing", name);
        }
        return false;
    }

    let fill = fallback.unwrap_or_else(pre_seed_array_fill_default);
    let values = values
        .into_iter()
        .map(|value| value.unwrap_or(fill))
        .collect::<Vec<_>>();
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
