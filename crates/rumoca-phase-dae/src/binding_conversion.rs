//! Conversion of declaration bindings into continuous DAE equations.

use std::collections::HashSet;

use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use rustc_hash::FxHashMap;

use crate::{
    classification, flat_to_dae_expression, flat_to_dae_var_name, name_resolution,
    path_utils::split_path_with_indices,
};

type Dae = dae::Dae;
type VariableKind = dae::VariableKind;
type EquationOrigin = flat::EquationOrigin;
type Expression = flat::Expression;
type Literal = flat::Literal;
type Model = flat::Model;
type VarName = flat::VarName;

/// Convert variable bindings to equations (MLS §4.4.1).
/// A declaration `Real y = expr` is equivalent to `Real y; equation y = expr;`
///
/// For input variables with bindings that are NOT connected, the binding also
/// becomes an equation. This handles cases like `input Integer expr[:] = {4, 6}`
/// where the binding provides the default value for the unconnected input.
///
/// IMPORTANT: Bindings are default values that can be overridden by explicit equations.
/// However, if a binding relates the variable to OTHER unknowns (not just constants/parameters),
/// it provides a multi-variable constraint that must be preserved even if the variable has
/// an explicit equation.
///
/// Example: `v = p.v - n.v` relates v, p.v, and n.v. Even if there's an explicit equation
/// for v, the binding provides the only constraint on p.v and n.v, so it must be kept.
pub(super) fn convert_bindings_to_equations(
    dae: &mut Dae,
    flat: &Model,
    prefix_children: &FxHashMap<String, Vec<VarName>>,
    state_vars: &HashSet<VarName>,
    connected_inputs: &HashSet<VarName>,
    algorithm_defined_vars: &HashSet<VarName>,
    record_eq_defined_vars: &HashSet<VarName>,
) {
    let (_directly_defined, all_defined) = super::collect_continuous_equation_lhs(flat);
    let unknowns = collect_unknowns(flat, state_vars);
    let known_var_names: HashSet<String> = flat
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();
    let unknown_prefix_children = build_unknown_prefix_children(&unknowns);
    let connected_inputs_only_connected_to_inputs =
        find_connected_inputs_only_connected_to_inputs(flat);

    // Build a map from flat equation LHS to the binding expression it matches.
    // When a flat equation has the SAME LHS variable as a binding AND the same
    // expression, skip the binding to avoid generating a duplicate equation.
    let flat_eq_bindings: HashSet<VarName> = build_duplicate_binding_set(flat);

    // Variables whose explicit equations reference other unknowns in the RHS.
    // When an explicit equation relates a variable to unknowns (e.g., `x = y`),
    // a constant binding (e.g., `x = 0`) provides additional information (the value)
    // and must NOT be skipped.
    let defined_by_unknown_rhs = collect_vars_with_unknown_rhs(flat, &unknowns);

    for (name, var) in &flat.variables {
        if !var.is_primitive && prefix_children.contains_key(name.as_str()) {
            continue;
        }

        let kind = if dae.discrete_reals.contains_key(&flat_to_dae_var_name(name))
            || dae
                .discrete_valued
                .contains_key(&flat_to_dae_var_name(name))
        {
            VariableKind::Discrete
        } else {
            classification::classify_variable(var, state_vars)
        };

        // Connected input-only alias sets (MLS §9.1) must preserve their value anchor
        // from declaration bindings; otherwise the system can become underdetermined.
        let keep_connected_input_binding = should_keep_connected_input_binding(
            &kind,
            name,
            var,
            &connected_inputs_only_connected_to_inputs,
        );

        if should_skip_variable_binding(&kind, name, connected_inputs)
            && !keep_connected_input_binding
            && !binding_defines_underdefined_unknown(var, name, &unknowns, &all_defined, flat)
        {
            continue;
        }

        // Skip when explicit equation overrides (unless binding provides multi-var
        // constraint OR the explicit equation relates the variable to other unknowns).
        // When the explicit equation has unknowns in its RHS (e.g., `suspend = subport.suspend`),
        // a constant binding (e.g., `suspend = false`) provides the VALUE and must be kept.
        if all_defined.contains(name)
            && !defined_by_unknown_rhs.contains(name)
            && should_skip_binding_for_explicit_var(name, var, &unknowns, &unknown_prefix_children)
        {
            continue;
        }

        // MLS §4.4.1: If the flat model already has an explicit equation with
        // the same LHS variable AND the same RHS expression as the binding,
        // skip the binding to avoid generating a duplicate equation.
        if flat_eq_bindings.contains(name) {
            continue;
        }

        // Skip variables that are defined by algorithm sections (MLS §11.1)
        // Algorithm outputs already contribute equations; adding binding equations
        // would cause double-counting and incorrect balance.
        if algorithm_defined_vars.contains(name) {
            continue;
        }

        // Skip variables that are record fields already defined by a record equation.
        // E.g., if `cc = func(...)` defines all fields of cc, don't also generate
        // a binding equation for `cc.m_capgd` (would double-count that field).
        if record_eq_defined_vars.contains(name) {
            continue;
        }

        if let Some(binding) = &var.binding {
            add_binding_equation(
                dae,
                name,
                binding,
                &var.dims,
                &kind,
                var.is_discrete_type,
                &known_var_names,
            );
        }
    }
}

/// Keep declaration bindings for connected input-only alias sets.
///
/// For connected inputs that only connect to other inputs, the declaration binding
/// provides the value anchor (MLS §4.4.1, §9.1). Dropping it can underdetermine
/// the alias component.
pub(super) fn should_keep_connected_input_binding(
    kind: &VariableKind,
    name: &VarName,
    var: &flat::Variable,
    connected_inputs_only_connected_to_inputs: &HashSet<VarName>,
) -> bool {
    *kind == VariableKind::Input
        && var.binding.is_some()
        && connected_inputs_only_connected_to_inputs.contains(name)
}

/// Build a prefix index for unknown variables.
///
/// For each unknown `a.b.c`, this stores:
/// - `a` -> `a.b.c`
/// - `a.b` -> `a.b.c`
///
/// This lets binding analysis detect when a reference like `a.b` points to
/// expanded unknown fields (`a.b.re`, `a.b.im`) instead of a scalar variable.
pub(super) fn build_unknown_prefix_children(
    unknowns: &HashSet<VarName>,
) -> FxHashMap<String, Vec<VarName>> {
    let mut prefix_children: FxHashMap<String, Vec<VarName>> = FxHashMap::default();
    for unknown in unknowns {
        let parts = split_path_with_indices(unknown.as_str());
        if parts.len() < 2 {
            continue;
        }
        let mut prefix = String::new();
        for (idx, segment) in parts[..parts.len() - 1].iter().enumerate() {
            if idx > 0 {
                prefix.push('.');
            }
            prefix.push_str(segment);
            prefix_children
                .entry(prefix.clone())
                .or_default()
                .push(unknown.clone());
        }
    }
    prefix_children
}

/// Check if a binding should be skipped because an explicit equation takes precedence.
///
/// Returns true if the binding should be skipped (explicit equation overrides).
/// Returns false if the binding provides a multi-variable constraint that must be preserved.
///
/// MLS §4.4.1: Bindings that only reference constants/parameters can be overridden
/// by explicit equations. However, bindings that reference other unknowns provide
/// constraints among multiple variables and must be kept.
pub(super) fn should_skip_binding_for_explicit_var(
    name: &VarName,
    var: &flat::Variable,
    unknowns: &HashSet<VarName>,
    unknown_prefix_children: &FxHashMap<String, Vec<VarName>>,
) -> bool {
    let Some(binding) = &var.binding else {
        // No binding - nothing to skip, but also nothing to keep
        return true;
    };

    // Check if binding references any OTHER unknowns (not just `name` itself)
    let binding_refs = super::collect_var_refs(binding);
    let resolved_name =
        super::resolve_name_against_set(name, unknowns).unwrap_or_else(|| name.clone());
    let references_other_unknowns = binding_refs.iter().any(|r| {
        if let Some(resolved_ref) = super::resolve_name_against_set(r, unknowns) {
            return resolved_ref != resolved_name;
        }

        // A record-level ref (e.g., `port_p.V_m`) can map to expanded unknown
        // fields (e.g., `port_p.V_m.re`, `port_p.V_m.im`).
        unknown_prefix_children
            .get(r.as_str())
            .is_some_and(|children| children.iter().any(|child| *child != resolved_name))
    });

    // Skip binding only if it doesn't reference other unknowns
    !references_other_unknowns
}

/// Collect variables whose explicit equations reference other unknowns in the RHS.
///
/// MLS §8.4: For a residual `lhs - rhs = 0`, if the RHS contains VarRefs that
/// are unknowns (other than the LHS variable itself), then the equation is
/// "relational" — it relates the LHS to other unknowns rather than just
/// assigning a constant.
///
/// This is used to decide whether a constant binding should be kept: when the
/// explicit equation relates the variable to unknowns, the binding provides
/// the VALUE and the explicit equation provides the RELATION between variables.
pub(super) fn collect_vars_with_unknown_rhs(
    flat: &Model,
    unknowns: &HashSet<VarName>,
) -> HashSet<VarName> {
    let unknown_subscriptless_index = build_unknown_subscriptless_index(unknowns);
    let mut result = HashSet::default();

    for eq in &flat.equations {
        if eq.origin.is_connection() {
            continue;
        }
        // Flow-sum equations relate multiple unknowns.
        if matches!(eq.origin, EquationOrigin::FlowSum { .. }) {
            let refs = super::collect_var_refs(&eq.residual);
            let unknown_refs: HashSet<_> = refs
                .iter()
                .filter_map(|r| {
                    resolve_unknown_ref_by_shape(r, unknowns, &unknown_subscriptless_index)
                })
                .collect();
            if unknown_refs.len() > 1 {
                result.extend(unknown_refs);
            }
            continue;
        }
        if matches!(eq.origin, EquationOrigin::UnconnectedFlow { .. }) {
            continue;
        }

        // For standard equations: extract LHS, check if RHS references unknowns.
        let Expression::Binary { op, lhs, rhs } = &eq.residual else {
            continue;
        };
        if !matches!(op, rumoca_ir_core::OpBinary::Sub(_)) {
            continue;
        }
        let Expression::VarRef { name: lhs_name, .. } = lhs.as_ref() else {
            continue;
        };
        let rhs_refs = super::collect_var_refs(rhs);
        let lhs_resolved =
            resolve_unknown_ref_by_shape(lhs_name, unknowns, &unknown_subscriptless_index)
                .unwrap_or_else(|| lhs_name.clone());
        if rhs_refs
            .iter()
            .filter_map(|r| resolve_unknown_ref_by_shape(r, unknowns, &unknown_subscriptless_index))
            .any(|r| r != lhs_resolved)
        {
            result.insert(lhs_name.clone());
        }
    }

    result
}

/// Find variables whose bindings would duplicate an explicit equation in the flat model.
///
/// When the flat model has an equation `v = expr` (in residual form `v - expr = 0`)
/// AND the variable `v` also has a binding `= expr` with the same expression,
/// the binding should be skipped to avoid generating a duplicate equation.
fn build_duplicate_binding_set(flat: &Model) -> HashSet<VarName> {
    let mut duplicates = HashSet::default();

    for eq in &flat.equations {
        if eq.origin.is_connection() {
            continue;
        }
        if matches!(
            eq.origin,
            EquationOrigin::FlowSum { .. } | EquationOrigin::UnconnectedFlow { .. }
        ) {
            continue;
        }

        // Extract LHS name and RHS expression from residual: lhs - rhs = 0.
        let Expression::Binary { op, lhs, rhs } = &eq.residual else {
            continue;
        };
        if !matches!(op, rumoca_ir_core::OpBinary::Sub(_)) {
            continue;
        }
        let Expression::VarRef { name, .. } = lhs.as_ref() else {
            continue;
        };

        let is_dup = flat
            .variables
            .get(name)
            .and_then(|var| var.binding.as_ref())
            .is_some_and(|binding| expressions_equivalent(binding, rhs));
        if is_dup {
            duplicates.insert(name.clone());
        }
    }

    duplicates
}

/// Check if two flat expressions are structurally equivalent.
///
/// This is conservative and may return false for semantically equivalent
/// expressions with different syntax.
fn expressions_equivalent(a: &Expression, b: &Expression) -> bool {
    match (a, b) {
        (
            Expression::VarRef {
                name: n1,
                subscripts: s1,
            },
            Expression::VarRef {
                name: n2,
                subscripts: s2,
            },
        ) => n1 == n2 && s1.len() == s2.len(),
        (Expression::Literal(l1), Expression::Literal(l2)) => match (l1, l2) {
            (Literal::Integer(a), Literal::Integer(b)) => a == b,
            (Literal::Real(a), Literal::Real(b)) => (a - b).abs() < 1e-15,
            (Literal::Boolean(a), Literal::Boolean(b)) => a == b,
            (Literal::String(a), Literal::String(b)) => a == b,
            _ => false,
        },
        (
            Expression::Binary {
                op: op1,
                lhs: l1,
                rhs: r1,
            },
            Expression::Binary {
                op: op2,
                lhs: l2,
                rhs: r2,
            },
        ) => {
            std::mem::discriminant(op1) == std::mem::discriminant(op2)
                && expressions_equivalent(l1, l2)
                && expressions_equivalent(r1, r2)
        }
        (Expression::Unary { op: op1, rhs: o1 }, Expression::Unary { op: op2, rhs: o2 }) => {
            std::mem::discriminant(op1) == std::mem::discriminant(op2)
                && expressions_equivalent(o1, o2)
        }
        (
            Expression::BuiltinCall {
                function: n1,
                args: a1,
                ..
            },
            Expression::BuiltinCall {
                function: n2,
                args: a2,
                ..
            },
        ) => {
            n1 == n2
                && a1.len() == a2.len()
                && a1
                    .iter()
                    .zip(a2.iter())
                    .all(|(x, y)| expressions_equivalent(x, y))
        }
        (
            Expression::FunctionCall {
                name: n1, args: a1, ..
            },
            Expression::FunctionCall {
                name: n2, args: a2, ..
            },
        ) => {
            n1 == n2
                && a1.len() == a2.len()
                && a1
                    .iter()
                    .zip(a2.iter())
                    .all(|(x, y)| expressions_equivalent(x, y))
        }
        _ => false,
    }
}

/// Remove all array subscripts from a flattened variable name.
///
/// Example: `a.b[1,:].c[2]` -> `a.b.c`
fn strip_all_subscripts(name: &str) -> String {
    let mut out = String::with_capacity(name.len());
    let mut depth = 0usize;
    for ch in name.chars() {
        match ch {
            '[' => depth += 1,
            ']' => depth = depth.saturating_sub(1),
            _ if depth == 0 => out.push(ch),
            _ => {}
        }
    }
    out
}

fn build_unknown_subscriptless_index(
    unknowns: &HashSet<VarName>,
) -> FxHashMap<String, Vec<VarName>> {
    let mut index: FxHashMap<String, Vec<VarName>> = FxHashMap::default();
    for unknown in unknowns {
        let key = strip_all_subscripts(unknown.as_str());
        index.entry(key).or_default().push(unknown.clone());
    }
    index
}

/// Resolve unknown references, including collapsed array-member forms.
///
/// Flattened equations can reference connector/member arrays without explicit
/// singleton subscripts (e.g. `heatPorts.T`) while unknowns are scalarized
/// members (`heatPorts[1].T`). When this subscript-less match is unique, treat
/// it as the same unknown relation (MLS §9.1 connection expansion semantics).
fn resolve_unknown_ref_by_shape(
    name: &VarName,
    unknowns: &HashSet<VarName>,
    unknown_subscriptless_index: &FxHashMap<String, Vec<VarName>>,
) -> Option<VarName> {
    if let Some(resolved) = super::resolve_name_against_set(name, unknowns) {
        return Some(resolved);
    }
    let key = strip_all_subscripts(name.as_str());
    let matches = unknown_subscriptless_index.get(&key)?;
    if matches.len() == 1 {
        return Some(matches[0].clone());
    }
    None
}

/// Check if a variable has its own binding equation.
fn has_binding(flat: &Model, name: &VarName) -> bool {
    flat.variables
        .get(name)
        .is_some_and(|var| var.binding.is_some())
}

/// Check if a binding references an unknown that has no other defining equation.
/// Returns true if the binding should be kept to define the underdefined unknown.
fn binding_defines_underdefined_unknown(
    var: &flat::Variable,
    name: &VarName,
    unknowns: &HashSet<VarName>,
    explicitly_defined: &HashSet<VarName>,
    flat: &Model,
) -> bool {
    let Some(binding) = &var.binding else {
        return false;
    };
    let binding_refs = super::collect_var_refs(binding);
    let resolved_name =
        super::resolve_name_against_set(name, unknowns).unwrap_or_else(|| name.clone());
    binding_refs.iter().any(|r| {
        let Some(resolved_ref) = super::resolve_name_against_set(r, unknowns) else {
            return false;
        };
        resolved_ref != resolved_name
            && !explicitly_defined.contains(&resolved_ref)
            && !has_binding(flat, &resolved_ref)
    })
}

/// Find internal inputs that only connect to other internal inputs in connect equations.
///
/// For these input-only alias sets, declaration bindings provide the value anchor
/// (MLS §4.4.1), so skipping bindings can underdetermine the system.
fn find_connected_inputs_only_connected_to_inputs(flat: &Model) -> HashSet<VarName> {
    let mut connected_inputs: HashSet<VarName> = HashSet::default();
    let mut connected_to_non_input_peer: HashSet<VarName> = HashSet::default();

    for eq in flat.equations.iter().filter(|eq| eq.origin.is_connection()) {
        let Expression::Binary { op, lhs, rhs } = &eq.residual else {
            continue;
        };
        if !matches!(op, rumoca_ir_core::OpBinary::Sub(_)) {
            continue;
        }

        let lhs_name = name_resolution::extract_varref_name(lhs);
        let rhs_name = name_resolution::extract_varref_name(rhs);

        let lhs_input = lhs_name
            .as_ref()
            .and_then(|name| super::resolve_internal_input(name, flat));
        let rhs_input = rhs_name
            .as_ref()
            .and_then(|name| super::resolve_internal_input(name, flat));

        match (lhs_input, rhs_input) {
            (Some(lhs_var), Some(rhs_var)) => {
                connected_inputs.insert(lhs_var);
                connected_inputs.insert(rhs_var);
            }
            (Some(lhs_var), None) => {
                connected_inputs.insert(lhs_var.clone());
                connected_to_non_input_peer.insert(lhs_var);
            }
            (None, Some(rhs_var)) => {
                connected_inputs.insert(rhs_var.clone());
                connected_to_non_input_peer.insert(rhs_var);
            }
            (None, None) => {}
        }
    }

    connected_inputs
        .into_iter()
        .filter(|name| !connected_to_non_input_peer.contains(name))
        .collect()
}

/// Collect all unknowns (state/algebraic/output variables).
fn collect_unknowns(flat: &Model, state_vars: &HashSet<VarName>) -> HashSet<VarName> {
    flat.variables
        .iter()
        .filter(|(_, var)| {
            let kind = classification::classify_variable(var, state_vars);
            matches!(
                kind,
                VariableKind::State | VariableKind::Algebraic | VariableKind::Output
            )
        })
        .map(|(name, _)| name.clone())
        .collect()
}

/// Check if a variable's binding should be skipped entirely.
fn should_skip_variable_binding(
    kind: &VariableKind,
    name: &VarName,
    connected_inputs: &HashSet<VarName>,
) -> bool {
    // Parameters/constants are fixed values and do not produce residual equations.
    if matches!(kind, VariableKind::Parameter | VariableKind::Constant) {
        return true;
    }
    // Connected inputs get their value from connection equations
    kind == &VariableKind::Input && connected_inputs.contains(name)
}

/// Add a binding equation to the appropriate DAE equation list.
fn add_binding_equation(
    dae: &mut Dae,
    name: &VarName,
    binding: &Expression,
    dims: &[i64],
    kind: &VariableKind,
    is_discrete_type: bool,
    known_var_names: &HashSet<String>,
) {
    let projected_binding =
        project_scalar_binding_record_field_alias(name, binding, known_var_names);
    let scalar_count = super::compute_var_size(dims);
    let origin = rumoca_ir_flat::EquationOrigin::Binding {
        variable: name.as_str().to_string(),
    }
    .to_string();

    if *kind == VariableKind::Discrete {
        let explicit = dae::Equation::explicit_with_scalar_count(
            flat_to_dae_var_name(name),
            flat_to_dae_expression(&projected_binding),
            Span::DUMMY,
            origin,
            scalar_count,
        );
        if is_discrete_type {
            dae.f_m.push(explicit);
        } else {
            dae.f_z.push(explicit);
        }
        return;
    }

    let residual = create_binding_residual(name, &projected_binding);
    let dae_eq = dae::Equation::residual_array(
        flat_to_dae_expression(&residual),
        Span::DUMMY,
        origin,
        scalar_count,
    );

    // All continuous equations go into f_x (MLS B.1a) — no ODE/algebraic/output split.
    dae.f_x.push(dae_eq);
}

fn project_scalar_binding_record_field_alias(
    lhs_name: &VarName,
    binding: &Expression,
    known_var_names: &HashSet<String>,
) -> Expression {
    let Expression::VarRef {
        name: rhs_name,
        subscripts,
    } = binding
    else {
        return binding.clone();
    };
    if !subscripts.is_empty() {
        return binding.clone();
    }

    let lhs_parts = split_path_with_indices(lhs_name.as_str());
    if lhs_parts.len() < 2 {
        return binding.clone();
    }
    let field_suffix = format!(".{}", lhs_parts[lhs_parts.len() - 1]);

    let projected = format!("{}{}", rhs_name.as_str(), field_suffix);
    if !known_var_names.contains(projected.as_str()) {
        return binding.clone();
    }

    Expression::VarRef {
        name: VarName::new(projected),
        subscripts: vec![],
    }
}

/// Create a residual expression for a binding equation: `var - binding`.
///
/// For a declaration like `Real y = expr`, this creates the residual `y - expr`
/// which represents the equation `y - expr = 0` (i.e., `y = expr`).
fn create_binding_residual(name: &VarName, binding: &Expression) -> Expression {
    let var_ref = Expression::VarRef {
        name: name.clone(),
        subscripts: vec![],
    };

    Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(rumoca_ir_core::Token::default()),
        lhs: Box::new(var_ref),
        rhs: Box::new(binding.clone()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_unknown_prefix_children_indexes_intermediate_paths() {
        let unknowns = HashSet::from([
            VarName::new("root.a.b"),
            VarName::new("root.c"),
            VarName::new("other.x"),
        ]);

        let prefix_children = build_unknown_prefix_children(&unknowns);

        let root_children = prefix_children
            .get("root")
            .expect("root prefix should be indexed");
        assert!(root_children.contains(&VarName::new("root.a.b")));
        assert!(root_children.contains(&VarName::new("root.c")));

        let root_a_children = prefix_children
            .get("root.a")
            .expect("nested prefix should be indexed");
        assert_eq!(root_a_children, &vec![VarName::new("root.a.b")]);
    }

    #[test]
    fn test_build_unknown_prefix_children_ignores_dot_inside_subscript_expression() {
        let unknowns = HashSet::from([VarName::new("bus[data.medium].pin.i")]);
        let prefix_children = build_unknown_prefix_children(&unknowns);

        assert_eq!(
            prefix_children.get("bus[data.medium]"),
            Some(&vec![VarName::new("bus[data.medium].pin.i")])
        );
        assert_eq!(
            prefix_children.get("bus[data.medium].pin"),
            Some(&vec![VarName::new("bus[data.medium].pin.i")])
        );
        assert!(
            !prefix_children.contains_key("bus[data"),
            "dot inside bracketed subscripts must not become a path separator"
        );
    }

    #[test]
    fn test_should_skip_binding_for_explicit_var_keeps_prefix_unknown_binding() {
        let unknowns = HashSet::from([
            VarName::new("port_p.v.re"),
            VarName::new("port_n.v.re"),
            VarName::new("self"),
        ]);
        let unknown_prefix_children = build_unknown_prefix_children(&unknowns);

        let var = flat::Variable {
            name: VarName::new("self"),
            variability: rumoca_ir_core::Variability::Empty,
            is_primitive: true,
            binding: Some(Expression::VarRef {
                name: VarName::new("port_p.v"),
                subscripts: vec![],
            }),
            ..Default::default()
        };
        assert!(
            !should_skip_binding_for_explicit_var(
                &VarName::new("self"),
                &var,
                &unknowns,
                &unknown_prefix_children
            ),
            "record-prefix unknown binding must be preserved"
        );
    }

    #[test]
    fn test_should_skip_binding_for_explicit_var_skips_constant_binding() {
        let unknowns = HashSet::from([VarName::new("x")]);
        let unknown_prefix_children = build_unknown_prefix_children(&unknowns);

        let var = flat::Variable {
            name: VarName::new("x"),
            variability: rumoca_ir_core::Variability::Empty,
            is_primitive: true,
            binding: Some(Expression::Literal(flat::Literal::Real(1.0))),
            ..Default::default()
        };
        assert!(should_skip_binding_for_explicit_var(
            &VarName::new("x"),
            &var,
            &unknowns,
            &unknown_prefix_children
        ));
    }

    #[test]
    fn test_build_duplicate_binding_set_matches_identical_rhs() {
        let mut flat = Model::new();
        flat.add_variable(
            VarName::new("x"),
            flat::Variable {
                name: VarName::new("x"),
                is_primitive: true,
                binding: Some(Expression::Literal(flat::Literal::Real(1.0))),
                ..Default::default()
            },
        );
        flat.add_equation(rumoca_ir_flat::Equation {
            residual: Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(rumoca_ir_core::Token::default()),
                lhs: Box::new(Expression::VarRef {
                    name: VarName::new("x"),
                    subscripts: vec![],
                }),
                rhs: Box::new(Expression::Literal(flat::Literal::Real(1.0))),
            },
            span: Span::DUMMY,
            origin: EquationOrigin::ComponentEquation {
                component: "M".to_string(),
            },
            scalar_count: 1,
        });

        let duplicates = build_duplicate_binding_set(&flat);
        assert!(duplicates.contains(&VarName::new("x")));
    }

    #[test]
    fn test_project_scalar_binding_record_field_alias_projects_known_field() {
        let known_var_names = HashSet::from([
            "mach.friction.frictionParameters.wRef".to_string(),
            "data.frictionParameters.wRef".to_string(),
        ]);
        let lhs = VarName::new("mach.friction.frictionParameters.wRef");
        let binding = Expression::VarRef {
            name: VarName::new("data.frictionParameters"),
            subscripts: vec![],
        };

        let projected = project_scalar_binding_record_field_alias(&lhs, &binding, &known_var_names);
        assert_eq!(
            format!("{projected:?}"),
            format!(
                "{:?}",
                Expression::VarRef {
                    name: VarName::new("data.frictionParameters.wRef"),
                    subscripts: vec![],
                }
            )
        );
    }
}
