use super::*;

pub(super) fn try_seed_var_from_pre_store(
    env: &mut VarEnv<f64>,
    name: &str,
    var: &rumoca_ir_dae::Variable,
) -> bool {
    let trace_pre = std::env::var("RUMOCA_SIM_INTROSPECT").is_ok();
    let sz = var.size();
    if sz <= 1 {
        if let Some(value) = get_pre_value(name) {
            if trace_pre && name.contains("signalSource") {
                eprintln!("[sim-introspect] pre-seed scalar {} = {}", name, value);
            }
            env.set(name, value);
            return true;
        }
        return false;
    }

    let mut values = Vec::with_capacity(sz);
    let mut found_any = false;
    for flat_idx in 0..sz {
        let key = flat_index_to_subscripts(flat_idx, &var.dims)
            .map(|subs| format_multi_subscript_key(name, &subs))
            .unwrap_or_else(|| format!("{name}[{}]", flat_idx + 1));
        if let Some(value) = get_pre_value(&key) {
            values.push(value);
            found_any = true;
        } else {
            values.push(f64::NAN);
        }
    }

    let fallback = get_pre_value(name).or_else(|| values.iter().copied().find(|v| v.is_finite()));
    if !found_any && fallback.is_none() {
        if trace_pre && name.contains("signalSource") {
            eprintln!("[sim-introspect] pre-seed array {} missing", name);
        }
        return false;
    }

    let fill = fallback.unwrap_or(0.0);
    for value in &mut values {
        if !value.is_finite() {
            *value = fill;
        }
    }
    set_array_entries(env, name, &var.dims, &values);
    if trace_pre && name.contains("signalSource") {
        eprintln!(
            "[sim-introspect] pre-seed array {} size={}",
            name,
            values.len()
        );
    }
    true
}
