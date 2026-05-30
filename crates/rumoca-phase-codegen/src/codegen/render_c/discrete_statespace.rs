use super::super::render_expr::get_field;
use super::{ExprConfig, var_name_to_symbol};
use minijinja::Value;

pub(super) fn synthesize_discrete_statespace_rhs(
    var_name: &str,
    dae: &Value,
    cfg: &ExprConfig,
) -> Option<String> {
    if let Some(prefix) = state_space_member_prefix(var_name, "e") {
        let setpoint = format!("{prefix}.setpoint");
        let measurement = format!("{prefix}.measurement");
        if has_var_in_dae_map(dae, "y", &setpoint) && has_var_in_dae_map(dae, "y", &measurement) {
            return Some(format!(
                "({}) - ({})",
                var_name_to_symbol(&setpoint, cfg),
                var_name_to_symbol(&measurement, cfg)
            ));
        }
        return None;
    }

    if let Some(prefix) = state_space_member_prefix(var_name, "u_k") {
        let x_name = format!("{prefix}.x");
        let e_name = format!("{prefix}.e");
        let c_name = format!("{prefix}.C_d");
        let d_name = format!("{prefix}.D_d");
        let e_expr = current_error_expr(prefix, dae, cfg);
        let n = get_first_dim_for_var_in_dae(dae, &x_name)?;
        if !has_var_in_dae_map(dae, "z", &x_name)
            || !has_var_in_dae_map(dae, "z", &e_name)
            || !has_var_in_dae_map(dae, "p", &c_name)
            || !has_var_in_dae_map(dae, "p", &d_name)
        {
            return None;
        }

        let mut terms = Vec::new();
        for j in 1..=n {
            terms.push(format!(
                "({} * pre_{})",
                indexed_alias(&c_name, j, cfg),
                indexed_alias(&x_name, j, cfg)
            ));
        }
        terms.push(format!(
            "({} * {})",
            var_name_to_symbol(&d_name, cfg),
            e_expr
        ));
        return Some(terms.join(" + "));
    }

    if let Some((prefix, i)) = parse_indexed_suffix(var_name, ".x") {
        let x_name = format!("{prefix}.x");
        let e_name = format!("{prefix}.e");
        let a_name = format!("{prefix}.A_d");
        let b_name = format!("{prefix}.B_d");
        let e_expr = current_error_expr(prefix, dae, cfg);
        let n = get_first_dim_for_var_in_dae(dae, &x_name)?;
        if i < 1 || i > n {
            return None;
        }
        if !has_var_in_dae_map(dae, "z", &x_name)
            || !has_var_in_dae_map(dae, "z", &e_name)
            || !has_var_in_dae_map(dae, "p", &a_name)
            || !has_var_in_dae_map(dae, "p", &b_name)
        {
            return None;
        }

        let mut terms = Vec::new();
        for j in 1..=n {
            let flat_idx = (i - 1) * n + j;
            terms.push(format!(
                "({} * pre_{})",
                indexed_alias(&a_name, flat_idx, cfg),
                indexed_alias(&x_name, j, cfg)
            ));
        }
        terms.push(format!("({} * {})", indexed_alias(&b_name, i, cfg), e_expr));
        return Some(terms.join(" + "));
    }

    None
}

fn state_space_member_prefix<'a>(name: &'a str, member: &str) -> Option<&'a str> {
    let (prefix, leaf) = rumoca_core::split_last_top_level(name)?;
    (leaf == member).then_some(prefix)
}

fn parse_indexed_suffix<'a>(name: &'a str, suffix: &str) -> Option<(&'a str, usize)> {
    let (prefix, leaf) = rumoca_core::split_last_top_level(name)?;
    let member = suffix.strip_prefix('.').unwrap_or(suffix);
    let scalar = rumoca_core::parse_scalar_name(leaf)?;
    if scalar.base != member {
        return None;
    }
    let [idx] = scalar.indices.as_slice() else {
        return None;
    };
    let idx = usize::try_from(*idx).ok().filter(|idx| *idx > 0)?;
    Some((prefix, idx))
}

fn indexed_alias(base_name: &str, idx: usize, cfg: &ExprConfig) -> String {
    var_name_to_symbol(&format!("{base_name}[{idx}]"), cfg)
}

fn has_var_in_dae_map(dae: &Value, map_name: &str, var_name: &str) -> bool {
    let Ok(map) = dae.get_attr(map_name) else {
        return false;
    };
    map.get_item(&Value::from(var_name)).is_ok()
}

fn get_first_dim_for_var_in_dae(dae: &Value, var_name: &str) -> Option<usize> {
    for map_name in ["z", "m", "x"] {
        let Ok(map) = dae.get_attr(map_name) else {
            continue;
        };
        let Ok(var) = map.get_item(&Value::from(var_name)) else {
            continue;
        };
        let Ok(dims) = get_field(&var, "dims") else {
            return None;
        };
        let len = dims.len()?;
        if len == 0 {
            return None;
        }
        let first = dims.get_item(&Value::from(0)).ok()?;
        if let Some(v) = first.as_usize() {
            return Some(v);
        }
        if let Some(v) = first.as_i64() {
            return usize::try_from(v).ok();
        }
    }
    None
}

fn current_error_expr(prefix: &str, dae: &Value, cfg: &ExprConfig) -> String {
    let setpoint = format!("{prefix}.setpoint");
    let measurement = format!("{prefix}.measurement");
    if has_var_in_dae_map(dae, "y", &setpoint) && has_var_in_dae_map(dae, "y", &measurement) {
        return format!(
            "({} - {})",
            var_name_to_symbol(&setpoint, cfg),
            var_name_to_symbol(&measurement, cfg)
        );
    }
    var_name_to_symbol(&format!("{prefix}.e"), cfg)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn state_space_member_prefix_uses_top_level_segments() {
        assert_eq!(state_space_member_prefix("ctrl.e", "e"), Some("ctrl"));
        assert_eq!(state_space_member_prefix("ctrl.u_k", "u_k"), Some("ctrl"));
        assert_eq!(state_space_member_prefix("ctrl.my_e", "e"), None);
        assert_eq!(state_space_member_prefix("ctrl[index.e]", "e"), None);
    }

    #[test]
    fn indexed_state_space_member_uses_top_level_segments() {
        assert_eq!(parse_indexed_suffix("ctrl.x[2]", ".x"), Some(("ctrl", 2)));
        assert_eq!(
            parse_indexed_suffix("plant.ctrl.x[12]", ".x"),
            Some(("plant.ctrl", 12))
        );
        assert_eq!(parse_indexed_suffix("ctrl.x_alias[2]", ".x"), None);
        assert_eq!(parse_indexed_suffix("ctrl.x[0]", ".x"), None);
        assert_eq!(parse_indexed_suffix("ctrl.x[1,2]", ".x"), None);
        assert_eq!(parse_indexed_suffix("ctrl[index.x].y", ".x"), None);
    }
}
