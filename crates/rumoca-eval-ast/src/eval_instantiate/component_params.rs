use super::{
    ConditionEvalEnv, InstantiateEvalCtx, ast, eval_scoped_string_condition_with_depth,
    get_enum_value_with_depth, resolve_component_ref_expr, try_eval_bool_literal,
    try_eval_integer_expr_with_depth,
};
use rumoca_ir_ast::AstIndexMap as IndexMap;
use rustc_hash::FxHashMap;

pub(super) fn component_condition_value_expr(comp: &ast::Component) -> &ast::Expression {
    comp.binding.as_ref().unwrap_or(&comp.start)
}

/// Try to extract a string from an expression.
pub fn expr_to_string(expr: &ast::Expression) -> Option<String> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::String,
            token,
            ..
        } => {
            // Remove surrounding quotes if present
            let s = token.text.trim().trim_matches('"').trim();
            (!s.is_empty()).then(|| s.to_string())
        }
        ast::Expression::ComponentReference(comp_ref) => {
            let value = ast::QualifiedName::from_component_reference(comp_ref).to_flat_string();
            (!value.is_empty()).then_some(value)
        }
        ast::Expression::Parenthesized { inner, .. } => expr_to_string(inner),
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem,
            lhs,
            rhs,
            ..
        } => Some(format!("{}{}", expr_to_string(lhs)?, expr_to_string(rhs)?)),
        _ => None,
    }
}

/// Try to evaluate an expression to a string/enum value without falling back to
/// unresolved component references.
pub fn try_eval_string_expr(ctx: &InstantiateEvalCtx, expr: &ast::Expression) -> Option<String> {
    let InstantiateEvalCtx {
        tree,
        mod_env,
        effective_components,
        resolve_class_components,
    } = ctx;
    let value = get_enum_value_with_depth(
        expr,
        mod_env,
        effective_components,
        tree,
        *resolve_class_components,
        None,
        0,
    )?;
    if let ast::Expression::ComponentReference(comp_ref) = expr
        && value == comp_ref.to_string()
    {
        return None;
    }
    Some(value)
}

/// Evaluate an MLS predefined `StateSelect` attribute expression.
pub fn eval_state_select_expr(
    ctx: &InstantiateEvalCtx,
    expr: &ast::Expression,
) -> Option<rumoca_core::StateSelect> {
    let env = ConditionEvalEnv {
        mod_env: ctx.mod_env,
        effective_components: ctx.effective_components,
        tree: ctx.tree,
        resolve_class_components: ctx.resolve_class_components,
    };
    eval_state_select_expr_with_depth(expr, env, None, 0)
}

fn eval_state_select_expr_with_depth(
    expr: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<rumoca_core::StateSelect> {
    if depth > super::MAX_CONDITION_DEPTH {
        return None;
    }
    if let Some(value) = parse_state_select(expr) {
        return Some(value);
    }

    match expr {
        ast::Expression::ComponentReference(comp_ref) => {
            let (resolved_expr, next_scope) = resolve_component_ref_expr(
                comp_ref,
                env.mod_env,
                env.effective_components,
                env.tree,
                scope_prefix,
            )?;
            eval_state_select_expr_with_depth(&resolved_expr, env, next_scope.as_deref(), depth + 1)
        }
        ast::Expression::Parenthesized { inner, .. } => {
            eval_state_select_expr_with_depth(inner, env, scope_prefix, depth + 1)
        }
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => eval_if_state_select(branches, else_branch, env, scope_prefix, depth + 1),
        ast::Expression::Array { elements, .. } => {
            let first = elements.first()?;
            if !elements.iter().all(|element| element == first) {
                return None;
            }
            eval_state_select_expr_with_depth(first, env, scope_prefix, depth + 1)
        }
        _ => None,
    }
}

fn eval_if_state_select(
    branches: &[(ast::Expression, ast::Expression)],
    else_branch: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<rumoca_core::StateSelect> {
    for (cond, branch_expr) in branches {
        match eval_scoped_string_condition_with_depth(cond, env, scope_prefix, depth) {
            Some(true) => {
                return eval_state_select_expr_with_depth(branch_expr, env, scope_prefix, depth);
            }
            Some(false) => continue,
            None => return None,
        }
    }
    eval_state_select_expr_with_depth(else_branch, env, scope_prefix, depth)
}

/// Parse a rumoca_core::StateSelect value from an expression.
pub fn parse_state_select(expr: &ast::Expression) -> Option<rumoca_core::StateSelect> {
    if let ast::Expression::ComponentReference(comp_ref) = expr
        && comp_ref.parts.len() == 2
        && comp_ref.parts.first()?.ident.text.as_ref() == "StateSelect"
    {
        return state_select_from_modelica_literal(&comp_ref.parts.last()?.ident.text);
    }
    None
}

/// Parse an MLS predefined `StateSelect` enumeration literal name.
fn state_select_from_modelica_literal(value: &str) -> Option<rumoca_core::StateSelect> {
    match value {
        "never" => Some(rumoca_core::StateSelect::Never),
        "avoid" => Some(rumoca_core::StateSelect::Avoid),
        "default" => Some(rumoca_core::StateSelect::Default),
        "prefer" => Some(rumoca_core::StateSelect::Prefer),
        "always" => Some(rumoca_core::StateSelect::Always),
        _ => None,
    }
}

/// Extract binding from declaration or modification.
///
/// MLS §7.2: Modifications from outer scopes override inner bindings.
/// A modification like `p(k = 5.0)` provides a binding value for component k.
pub fn extract_binding(
    comp: &ast::Component,
    mod_env: &ast::ModificationEnvironment,
) -> (Option<ast::Expression>, bool, Option<ast::QualifiedName>) {
    // Check mod_env for binding override (outer modification takes precedence)
    // The binding modification is stored under just the component name
    let binding_path = ast::QualifiedName::from_ident(&comp.name);
    if let Some(mod_value) = mod_env.get(&binding_path) {
        return (
            Some(mod_value.value.clone()),
            true,
            mod_value.source_scope.clone(),
        );
    }

    // Check if the component has an explicit binding from declaration
    // Use the dedicated `binding` field which preserves the binding even when
    // there's a separate start= modifier (e.g., `Real v(start=V0) = p.v - n.v`)
    if let Some(binding) = &comp.binding {
        return (Some(binding.clone()), false, None);
    }

    // Fallback: check start when has_explicit_binding is true
    // Handles code paths where binding may not be populated (e.g. modification-only declarations)
    if comp.has_explicit_binding
        && !comp.start_is_modification
        && !matches!(comp.start, ast::Expression::Empty { .. })
    {
        return (Some(comp.start.clone()), false, None);
    }

    (None, false, None)
}

/// Extract boolean parameter values from components for conditional equation evaluation.
///
/// This enables proper handling of patterns like:
/// ```modelica
/// if use_numberPort then connect(numberPort, showNumber); else ... end if;
/// ```
///
/// Returns a map of component names to their boolean values.
/// Takes the modification environment to check for parameter overrides.
pub fn extract_bool_params_with_mods(
    effective_components: &IndexMap<String, ast::Component>,
    mod_env: &ast::ModificationEnvironment,
) -> FxHashMap<String, bool> {
    extract_params_with_mods(effective_components, mod_env, |comp, expr| {
        if !matches!(comp.variability, rumoca_core::Variability::Parameter(_)) {
            return None;
        }
        let type_name = comp.type_name.to_string();
        if !rumoca_core::qualified_type_name_matches(&type_name, "Boolean") {
            return None;
        }
        try_eval_bool_literal(expr)
    })
}

/// Extract integer parameter values from components for for-loop range evaluation.
///
/// This enables proper handling of patterns like:
/// ```modelica
/// for k in 1:m loop connect(plug_p.pin[k], resistor[k].p); end for;
/// ```
///
/// Returns a map of component names to their integer values.
/// Takes the modification environment to check for parameter overrides.
pub fn extract_int_params_with_mods(ctx: &InstantiateEvalCtx) -> FxHashMap<String, i64> {
    let InstantiateEvalCtx {
        tree,
        mod_env,
        effective_components,
        resolve_class_components,
    } = ctx;
    let mut int_params = extract_params_with_mods(effective_components, mod_env, |comp, expr| {
        if !matches!(
            comp.variability,
            rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
        ) {
            return None;
        }
        try_eval_integer_expr_with_depth(
            expr,
            mod_env,
            effective_components,
            tree,
            *resolve_class_components,
            0,
        )
    });

    // Also add dotted keys from multi-part modifications in mod_env.
    // This handles record field references like cellData.nRC used in for-loop ranges.
    for (qn, mod_value) in &mod_env.active {
        if qn.parts.len() > 1 {
            let dotted_key: String = qn
                .parts
                .iter()
                .map(|(name, _)| name.as_str())
                .collect::<Vec<_>>()
                .join(".");
            if int_params.contains_key(&dotted_key) {
                continue;
            }
            if let Some(value) = try_eval_integer_expr_with_depth(
                &mod_value.value,
                mod_env,
                effective_components,
                tree,
                *resolve_class_components,
                0,
            ) {
                int_params.insert(dotted_key, value);
            }
        }
    }

    // If a record parameter is rebound by reference (e.g., `cellData = cellData2`),
    // propagate integer fields from the target record (`cellData2.nRC`) to the
    // bound name (`cellData.nRC`) so for-loop ranges use the overridden values.
    propagate_record_alias_integer_params(&mut int_params, mod_env);

    int_params
}

fn extract_params_with_mods<T, F>(
    effective_components: &IndexMap<String, ast::Component>,
    mod_env: &ast::ModificationEnvironment,
    mut eval: F,
) -> FxHashMap<String, T>
where
    F: FnMut(&ast::Component, &ast::Expression) -> Option<T>,
{
    let mut params = FxHashMap::default();

    for (name, comp) in effective_components {
        let mod_path = ast::QualifiedName::from_ident(name);
        if let Some(mod_value) = mod_env.get(&mod_path)
            && let Some(value) = eval(comp, &mod_value.value)
        {
            params.insert(name.clone(), value);
            continue;
        }

        if let Some(value_expr) = component_expr_for_structural_eval(comp)
            && let Some(value) = eval(comp, value_expr)
        {
            params.insert(name.clone(), value);
        }
    }

    params
}

/// Return the declaration-side value expression for structural evaluation.
///
/// MLS §4.4.4 / §7.2: structural parameter values come from declaration bindings
/// (`x = expr`) and applied modifications; `start` is only an initialization
/// attribute for simulation variables and should be secondary for compile-time shape.
pub(crate) fn component_expr_for_structural_eval(
    comp: &ast::Component,
) -> Option<&ast::Expression> {
    if let Some(binding) = comp.binding.as_ref() {
        return Some(binding);
    }
    if comp.has_explicit_binding
        && !comp.start_is_modification
        && !matches!(comp.start, ast::Expression::Empty { .. })
    {
        return Some(&comp.start);
    }
    (!comp.start_is_modification && !matches!(comp.start, ast::Expression::Empty { .. }))
        .then_some(&comp.start)
}

/// Generate enclosing-scope lookup paths for a dotted reference.
///
/// Example: `pipe2.flowModel.nFM` -> [`pipe2.nFM`, `nFM`]
/// This models lexical scope climbing for nested component members.
pub(crate) fn enclosing_scope_candidates(dotted: &str) -> Vec<String> {
    let mut parts: Vec<&str> = rumoca_core::split_path_with_indices(dotted);
    let mut candidates = Vec::new();
    while parts.len() > 1 {
        let remove_idx = parts.len() - 2;
        parts.remove(remove_idx);
        candidates.push(parts.join("."));
    }
    candidates
}

pub fn propagate_record_alias_integer_params(
    int_params: &mut FxHashMap<String, i64>,
    mod_env: &ast::ModificationEnvironment,
) {
    let aliases = collect_record_aliases(mod_env);
    if aliases.is_empty() {
        return;
    }

    // Resolve simple alias chains (a=b, b=c) without unbounded growth.
    const MAX_ALIAS_PROPAGATION_PASSES: usize = 8;
    for _ in 0..MAX_ALIAS_PROPAGATION_PASSES {
        let mut changed = false;

        for (alias_name, target_name) in &aliases {
            if alias_name == target_name {
                continue;
            }

            if let Some(value) = int_params.get(target_name).copied() {
                let prev = int_params.insert(alias_name.clone(), value);
                changed |= prev != Some(value);
            }

            let target_prefix = format!("{target_name}.");
            let alias_prefix = format!("{alias_name}.");
            let propagated: Vec<_> = int_params
                .iter()
                .filter_map(|(key, value)| {
                    key.strip_prefix(&target_prefix)
                        .map(|suffix| (format!("{alias_prefix}{suffix}"), *value))
                })
                .collect();

            for (key, value) in propagated {
                let prev = int_params.insert(key, value);
                changed |= prev != Some(value);
            }
        }

        if !changed {
            break;
        }
    }
}

fn collect_record_aliases(mod_env: &ast::ModificationEnvironment) -> Vec<(String, String)> {
    let mut aliases = Vec::new();
    for (target_qn, mod_value) in &mod_env.active {
        if target_qn.parts.len() != 1 {
            continue;
        }
        let Some(alias_name) = target_qn.first_name() else {
            continue;
        };
        let ast::Expression::ComponentReference(comp_ref) = &mod_value.value else {
            continue;
        };
        let Some(target_name) = component_ref_to_dotted_no_subscripts(comp_ref) else {
            continue;
        };
        aliases.push((alias_name.to_string(), target_name));
    }
    aliases
}

pub(crate) fn component_ref_to_dotted_no_subscripts(
    comp_ref: &ast::ComponentReference,
) -> Option<String> {
    if comp_ref.parts.is_empty() || comp_ref.parts.iter().any(|part| part.subs.is_some()) {
        return None;
    }
    Some(
        comp_ref
            .parts
            .iter()
            .map(|part| part.ident.text.as_ref())
            .collect::<Vec<_>>()
            .join("."),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    fn bool_literal(value: bool) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token: rumoca_core::Token {
                text: Arc::from(if value { "true" } else { "false" }),
                ..Default::default()
            },
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn bool_param_with_start(name: &str, type_name: &str, value: bool) -> ast::Component {
        ast::Component {
            name: name.to_string(),
            type_name: ast::Name::from_string(type_name),
            variability: rumoca_core::Variability::Parameter(Default::default()),
            start: bool_literal(value),
            ..Default::default()
        }
    }

    fn bool_param_with_binding(name: &str, type_name: &str, value: bool) -> ast::Component {
        ast::Component {
            name: name.to_string(),
            type_name: ast::Name::from_string(type_name),
            variability: rumoca_core::Variability::Parameter(Default::default()),
            binding: Some(bool_literal(value)),
            has_explicit_binding: true,
            ..Default::default()
        }
    }

    #[test]
    fn bool_param_extraction_requires_boolean_type_segment() {
        let mut components = IndexMap::default();
        components.insert(
            "plain".to_string(),
            bool_param_with_start("plain", "Boolean", true),
        );
        components.insert(
            "qualified".to_string(),
            bool_param_with_start("qualified", "Modelica.Boolean", false),
        );
        components.insert(
            "nested".to_string(),
            bool_param_with_start("nested", "Pkg.Types.Boolean", true),
        );
        components.insert(
            "prefix_lookalike".to_string(),
            bool_param_with_start("prefix_lookalike", "MyBoolean", true),
        );
        components.insert(
            "suffix_lookalike".to_string(),
            bool_param_with_start("suffix_lookalike", "Pkg.BooleanAlias", true),
        );

        let bool_params =
            extract_bool_params_with_mods(&components, &ast::ModificationEnvironment::new());

        assert_eq!(bool_params.get("plain"), Some(&true));
        assert_eq!(bool_params.get("qualified"), Some(&false));
        assert_eq!(bool_params.get("nested"), Some(&true));
        assert!(!bool_params.contains_key("prefix_lookalike"));
        assert!(!bool_params.contains_key("suffix_lookalike"));
    }

    #[test]
    fn bool_param_extraction_uses_declaration_binding() {
        let mut components = IndexMap::default();
        components.insert(
            "use_numberPort".to_string(),
            bool_param_with_binding("use_numberPort", "Boolean", true),
        );

        let bool_params =
            extract_bool_params_with_mods(&components, &ast::ModificationEnvironment::new());

        assert_eq!(bool_params.get("use_numberPort"), Some(&true));
    }

    #[test]
    fn bool_param_extraction_ignores_start_attribute_modification() {
        let mut components = IndexMap::default();
        let mut comp = bool_param_with_start("useHeatPort", "Boolean", false);
        comp.start_is_modification = true;
        components.insert("useHeatPort".to_string(), comp);

        let bool_params =
            extract_bool_params_with_mods(&components, &ast::ModificationEnvironment::new());

        assert!(!bool_params.contains_key("useHeatPort"));
    }
}
