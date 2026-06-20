//! Conversion of declaration bindings into continuous DAE equations.

use indexmap::IndexSet;
use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use rustc_hash::FxHashMap;
use std::collections::HashSet;

use crate::{
    ToDaeError, classification, flat_to_dae_expression_with_refs, flat_to_dae_var_name,
    path_utils::strip_all_subscripts,
};

type Dae = dae::Dae;
type VariableKind = dae::VariableKind;
type EquationOrigin = flat::EquationOrigin;
type Expression = rumoca_core::Expression;
type Literal = rumoca_core::Literal;
type Model = flat::Model;
type VarName = rumoca_core::VarName;

struct BindingEquationContext<'a> {
    dae: &'a mut Dae,
    flat: &'a Model,
    known_var_names: &'a HashSet<String>,
}

struct BindingEquationSpec<'a> {
    name: &'a VarName,
    binding: &'a Expression,
    source_span: Span,
    dims: &'a [i64],
    kind: &'a VariableKind,
    is_discrete_type: bool,
}

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
    state_vars: &IndexSet<VarName>,
    connected_inputs: &IndexSet<VarName>,
    algorithm_defined_vars: &HashSet<VarName>,
    record_eq_defined_vars: &HashSet<VarName>,
) -> Result<(), ToDaeError> {
    let (_directly_defined, all_defined) = super::collect_continuous_equation_lhs(flat);
    let unknowns = collect_unknowns(flat, state_vars);
    let known_var_names: HashSet<String> = flat
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();
    let unknown_prefix_children = build_unknown_prefix_children(&unknowns)?;
    let internal_inputs = super::InternalInputIndex::new(flat)?;
    let connected_inputs_only_connected_to_inputs =
        super::find_connected_inputs_only_connected_to_inputs(flat, &internal_inputs);

    // Build a map from flat equation LHS to the binding expression it exactly
    // shadows. This is an origin-selection rule at the conversion boundary, not
    // a generic DAE residual suppression pass.
    let explicitly_shadowed_bindings: HashSet<VarName> =
        collect_bindings_shadowed_by_identical_explicit_equations(flat);

    // Variables whose explicit equations reference other unknowns in the RHS.
    // When an explicit equation relates a variable to unknowns (e.g., `x = y`),
    // a constant binding (e.g., `x = 0`) provides additional information (the value)
    // and must NOT be skipped.
    let defined_by_unknown_rhs = collect_vars_with_unknown_rhs(flat, &unknowns);

    for (name, var) in &flat.variables {
        if !var.is_primitive && prefix_children.contains_key(name.as_str()) {
            continue;
        }

        let kind = if dae
            .variables
            .discrete_reals
            .contains_key(&flat_to_dae_var_name(name))
            || dae
                .variables
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

        // MLS §4.4.1: A declaration binding is converted to an equation only
        // once. If flattening has already materialized an identical explicit
        // equation for the same variable, use that source equation as the
        // canonical origin.
        if explicitly_shadowed_bindings.contains(name) {
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
            let mut binding_context = BindingEquationContext {
                dae,
                flat,
                known_var_names: &known_var_names,
            };
            add_binding_equation(
                &mut binding_context,
                BindingEquationSpec {
                    name,
                    binding,
                    source_span: var.source_span,
                    dims: &var.dims,
                    kind: &kind,
                    is_discrete_type: var.is_discrete_type,
                },
            )?;
        }
    }
    Ok(())
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
) -> Result<FxHashMap<String, Vec<VarName>>, ToDaeError> {
    let mut prefix_children: FxHashMap<String, Vec<VarName>> = FxHashMap::default();
    for unknown in unknowns {
        let path = rumoca_core::ComponentPath::from_flat_path(unknown.as_str());
        for idx in 1..path.len() {
            let Some(prefix) = path.prefix(idx) else {
                return Err(ToDaeError::runtime_contract_violation(format!(
                    "unknown `{unknown}` could not produce prefix {idx} from {} path segments",
                    path.len()
                )));
            };
            let prefix = prefix.to_flat_string();
            prefix_children
                .entry(prefix)
                .or_default()
                .push(unknown.clone());
        }
    }
    Ok(prefix_children)
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
        let Expression::Binary { op, lhs, rhs, .. } = &eq.residual else {
            continue;
        };
        if !matches!(op, rumoca_core::OpBinary::Sub) {
            continue;
        }
        let Expression::VarRef { name: lhs_name, .. } = lhs.as_ref() else {
            continue;
        };
        let rhs_refs = super::collect_var_refs(rhs);
        let lhs_resolved = resolve_unknown_ref_by_shape(
            lhs_name.var_name(),
            unknowns,
            &unknown_subscriptless_index,
        )
        .unwrap_or_else(|| lhs_name.var_name().clone());
        if rhs_refs
            .iter()
            .filter_map(|r| resolve_unknown_ref_by_shape(r, unknowns, &unknown_subscriptless_index))
            .any(|r| r != lhs_resolved)
        {
            result.insert(lhs_name.var_name().clone());
        }
    }

    result
}

/// Find bindings already materialized as explicit equations in the flat model.
///
/// When the flat model has an equation `v = expr` (in residual form `v - expr = 0`)
/// AND the variable `v` also has a binding `= expr` with the same expression,
/// the explicit equation carries the canonical source origin into DAE.
fn collect_bindings_shadowed_by_identical_explicit_equations(flat: &Model) -> HashSet<VarName> {
    let mut shadowed = HashSet::default();

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
        let Expression::Binary { op, lhs, rhs, .. } = &eq.residual else {
            continue;
        };
        if !matches!(op, rumoca_core::OpBinary::Sub) {
            continue;
        }
        let Expression::VarRef { name, .. } = lhs.as_ref() else {
            continue;
        };

        let is_dup = flat
            .variables
            .get(name.var_name())
            .and_then(|var| var.binding.as_ref())
            .is_some_and(|binding| expressions_equivalent(binding, rhs));
        if is_dup {
            shadowed.insert(name.var_name().clone());
        }
    }

    shadowed
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
                span: _,
            },
            Expression::VarRef {
                name: n2,
                subscripts: s2,
                span: _,
            },
        ) => n1.var_name() == n2.var_name() && subscript_slices_equivalent(s1, s2),
        (Expression::Literal { value: l1, .. }, Expression::Literal { value: l2, .. }) => {
            match (l1, l2) {
                (Literal::Integer(a), Literal::Integer(b)) => a == b,
                (Literal::Real(a), Literal::Real(b)) => (a - b).abs() < 1e-15,
                (Literal::Boolean(a), Literal::Boolean(b)) => a == b,
                (Literal::String(a), Literal::String(b)) => a == b,
                _ => false,
            }
        }
        (
            Expression::Binary {
                op: op1,
                lhs: l1,
                rhs: r1,
                span: _,
            },
            Expression::Binary {
                op: op2,
                lhs: l2,
                rhs: r2,
                span: _,
            },
        ) => {
            std::mem::discriminant(op1) == std::mem::discriminant(op2)
                && expressions_equivalent(l1, l2)
                && expressions_equivalent(r1, r2)
        }
        (
            Expression::Unary {
                op: op1,
                rhs: o1,
                span: _,
            },
            Expression::Unary {
                op: op2,
                rhs: o2,
                span: _,
            },
        ) => {
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

fn subscript_slices_equivalent(
    lhs: &[rumoca_core::Subscript],
    rhs: &[rumoca_core::Subscript],
) -> bool {
    lhs.len() == rhs.len()
        && lhs
            .iter()
            .zip(rhs)
            .all(|(lhs, rhs)| subscripts_equivalent(lhs, rhs))
}

fn subscripts_equivalent(lhs: &rumoca_core::Subscript, rhs: &rumoca_core::Subscript) -> bool {
    match (lhs, rhs) {
        (
            rumoca_core::Subscript::Index { value: lhs, .. },
            rumoca_core::Subscript::Index { value: rhs, .. },
        ) => lhs == rhs,
        (rumoca_core::Subscript::Colon { .. }, rumoca_core::Subscript::Colon { .. }) => true,
        (
            rumoca_core::Subscript::Expr { expr: lhs, .. },
            rumoca_core::Subscript::Expr { expr: rhs, .. },
        ) => expressions_equivalent(lhs, rhs),
        _ => false,
    }
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

/// Collect all unknowns (state/algebraic/output variables).
fn collect_unknowns(flat: &Model, state_vars: &IndexSet<VarName>) -> HashSet<VarName> {
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
    connected_inputs: &IndexSet<VarName>,
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
    ctx: &mut BindingEquationContext<'_>,
    spec: BindingEquationSpec<'_>,
) -> Result<(), ToDaeError> {
    let scalar_count = super::compute_var_size(spec.dims);
    let span = binding_equation_span(spec.name, spec.binding, spec.source_span)?;
    let selected_binding = select_scalar_binding_record_field_alias(
        spec.name,
        spec.binding,
        ctx.known_var_names,
        span,
    );
    let origin = rumoca_ir_flat::EquationOrigin::Binding {
        variable: spec.name.as_str().to_string(),
    }
    .to_string();

    if *spec.kind == VariableKind::Discrete {
        let explicit = dae::Equation::explicit_with_scalar_count(
            flat_to_dae_var_name(spec.name),
            flat_to_dae_expression_with_refs(&selected_binding, ctx.flat)?,
            span,
            origin,
            scalar_count,
        );
        if spec.is_discrete_type {
            ctx.dae.discrete.valued_updates.push(explicit);
        } else {
            ctx.dae.discrete.real_updates.push(explicit);
        }
        return Ok(());
    }

    let residual = create_binding_residual(spec.name, &selected_binding, span);
    let dae_eq = dae::Equation::residual_array(
        flat_to_dae_expression_with_refs(&residual, ctx.flat)?,
        span,
        origin,
        scalar_count,
    );

    // All continuous equations go into f_x (MLS B.1a) — no ODE/algebraic/output split.
    ctx.dae.continuous.equations.push(dae_eq);
    Ok(())
}

fn binding_equation_span(
    name: &VarName,
    binding: &Expression,
    source_span: Span,
) -> Result<Span, ToDaeError> {
    binding
        .span()
        .filter(|span| !span.is_dummy())
        .or_else(|| (!source_span.is_dummy()).then_some(source_span))
        .ok_or_else(|| {
            ToDaeError::runtime_metadata_violation(format!(
                "binding equation for `{}` is missing source provenance",
                name.as_str()
            ))
        })
}

fn select_scalar_binding_record_field_alias(
    lhs_name: &VarName,
    binding: &Expression,
    known_var_names: &HashSet<String>,
    owner_span: Span,
) -> Expression {
    let Expression::VarRef {
        name: rhs_name,
        subscripts,
        ..
    } = binding
    else {
        return binding.clone();
    };
    if !subscripts.is_empty() {
        return binding.clone();
    }

    let lhs_path = rumoca_core::ComponentPath::from_flat_path(lhs_name.as_str());
    if lhs_path.len() < 2 {
        return binding.clone();
    }
    let Some(field_name) = lhs_path.parts().last() else {
        return binding.clone();
    };

    let selected = rumoca_core::ComponentPath::from_reference(rhs_name)
        .join(&rumoca_core::ComponentPath::from_flat_path(field_name))
        .to_flat_string();
    let Some(selected) = crate::path_utils::resolve_known_path_suffix(&selected, known_var_names)
    else {
        return binding.clone();
    };

    Expression::VarRef {
        name: VarName::new(selected).into(),
        subscripts: vec![],
        span: owner_span,
    }
}

/// Create a residual expression for a binding equation: `var - binding`.
///
/// For a declaration like `Real y = expr`, this creates the residual `y - expr`
/// which represents the equation `y - expr = 0` (i.e., `y = expr`).
fn create_binding_residual(name: &VarName, binding: &Expression, span: Span) -> Expression {
    let var_ref = Expression::VarRef {
        name: name.clone().into(),
        subscripts: vec![],
        span,
    };

    Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(var_ref),
        rhs: Box::new(binding.clone()),
        span,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("binding_conversion_test.mo"),
            4,
            9,
        )
    }

    #[test]
    fn test_build_unknown_prefix_children_indexes_intermediate_paths() {
        let unknowns = HashSet::from([
            VarName::new("root.a.b"),
            VarName::new("root.c"),
            VarName::new("other.x"),
        ]);

        let prefix_children = build_unknown_prefix_children(&unknowns)
            .unwrap_or_else(|err| panic!("unknown prefix index should build: {err}"));

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
        let prefix_children = build_unknown_prefix_children(&unknowns)
            .unwrap_or_else(|err| panic!("unknown prefix index should build: {err}"));

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
    fn binding_equation_span_prefers_binding_expression_span() {
        let declaration_span = Span::from_offsets(
            rumoca_core::SourceId::from_source_name("binding_decl.mo"),
            4,
            9,
        );
        let binding_span = Span::from_offsets(
            rumoca_core::SourceId::from_source_name("binding_decl.mo"),
            13,
            21,
        );
        let binding = Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: binding_span,
        };

        assert_eq!(
            binding_equation_span(&VarName::new("x"), &binding, declaration_span)
                .unwrap_or_else(|err| panic!("binding span should resolve: {err}")),
            binding_span
        );
    }

    #[test]
    fn binding_equation_span_uses_declaration_when_binding_is_unspanned() {
        let declaration_span = Span::from_offsets(
            rumoca_core::SourceId::from_source_name("binding_decl_fallback.mo"),
            4,
            9,
        );
        let binding = Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: Span::DUMMY,
        };

        assert_eq!(
            binding_equation_span(&VarName::new("x"), &binding, declaration_span)
                .unwrap_or_else(|err| panic!("binding span should resolve: {err}")),
            declaration_span
        );
    }

    #[test]
    fn binding_equation_span_rejects_unspanned_binding_and_declaration() {
        let binding = Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: Span::DUMMY,
        };

        let err = binding_equation_span(&VarName::new("x"), &binding, Span::DUMMY)
            .expect_err("unspanned binding equation should fail");

        assert!(matches!(err, ToDaeError::RuntimeMetadataViolation { .. }));
    }

    #[test]
    fn test_should_skip_binding_for_explicit_var_keeps_prefix_unknown_binding() {
        let unknowns = HashSet::from([
            VarName::new("port_p.v.re"),
            VarName::new("port_n.v.re"),
            VarName::new("self"),
        ]);
        let unknown_prefix_children = build_unknown_prefix_children(&unknowns)
            .unwrap_or_else(|err| panic!("unknown prefix index should build: {err}"));

        let var = flat::Variable {
            name: VarName::new("self"),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            binding: Some(Expression::VarRef {
                name: VarName::new("port_p.v").into(),
                subscripts: vec![],
                span: test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
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
        let unknown_prefix_children = build_unknown_prefix_children(&unknowns)
            .unwrap_or_else(|err| panic!("unknown prefix index should build: {err}"));

        let var = flat::Variable {
            name: VarName::new("x"),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            binding: Some(Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span: test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        };
        assert!(should_skip_binding_for_explicit_var(
            &VarName::new("x"),
            &var,
            &unknowns,
            &unknown_prefix_children
        ));
    }

    #[test]
    fn test_collect_shadowed_bindings_matches_identical_rhs() {
        let mut flat = Model::new();
        flat.add_variable(
            VarName::new("x"),
            flat::Variable {
                name: VarName::new("x"),
                is_primitive: true,
                binding: Some(Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span: test_span(),
                }),
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );
        flat.add_equation(rumoca_ir_flat::Equation {
            residual: Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(Expression::VarRef {
                    name: VarName::new("x").into(),
                    subscripts: vec![],
                    span: test_span(),
                }),
                rhs: Box::new(Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span: test_span(),
                }),
                span: test_span(),
            },
            span: test_span(),
            origin: EquationOrigin::ComponentEquation {
                component: "M".to_string(),
            },
            scalar_count: 1,
        });

        let shadowed = collect_bindings_shadowed_by_identical_explicit_equations(&flat);
        assert!(shadowed.contains(&VarName::new("x")));
    }

    #[test]
    fn test_select_scalar_binding_record_field_alias_selects_known_field() {
        let known_var_names = HashSet::from([
            "mach.friction.frictionParameters.wRef".to_string(),
            "data.frictionParameters.wRef".to_string(),
        ]);
        let lhs = VarName::new("mach.friction.frictionParameters.wRef");
        let binding = Expression::VarRef {
            name: VarName::new("data.frictionParameters").into(),
            subscripts: vec![],
            span: test_span(),
        };
        let owner_span = test_span();

        let selected =
            select_scalar_binding_record_field_alias(&lhs, &binding, &known_var_names, owner_span);
        assert_eq!(
            format!("{selected:?}"),
            format!(
                "{:?}",
                Expression::VarRef {
                    name: VarName::new("data.frictionParameters.wRef").into(),
                    subscripts: vec![],
                    span: owner_span,
                }
            )
        );
    }

    #[test]
    fn test_select_scalar_binding_record_field_alias_accepts_source_spanned_binding() {
        let known_var_names = HashSet::from(["data.frictionParameters.wRef".to_string()]);
        let lhs = VarName::new("mach.friction.frictionParameters.wRef");
        let binding = Expression::VarRef {
            name: VarName::new("data.frictionParameters").into(),
            subscripts: vec![],
            span: Span::from_offsets(
                rumoca_core::SourceId::from_source_name("record_alias.mo"),
                21,
                44,
            ),
        };
        let owner_span = test_span();

        let selected =
            select_scalar_binding_record_field_alias(&lhs, &binding, &known_var_names, owner_span);

        assert_eq!(
            format!("{selected:?}"),
            format!(
                "{:?}",
                Expression::VarRef {
                    name: VarName::new("data.frictionParameters.wRef").into(),
                    subscripts: vec![],
                    span: owner_span,
                }
            )
        );
    }

    #[test]
    fn test_select_scalar_binding_record_field_alias_resolves_misqualified_prefix() {
        let known_var_names = HashSet::from(["aimcData.statorCoreParameters.wRef".to_string()]);
        let lhs = VarName::new("aimc.statorCoreParameters.wRef");
        let binding = Expression::VarRef {
            name: VarName::new("aimc.aimcData.statorCoreParameters").into(),
            subscripts: vec![],
            span: test_span(),
        };
        let owner_span = test_span();

        let selected =
            select_scalar_binding_record_field_alias(&lhs, &binding, &known_var_names, owner_span);
        assert_eq!(
            format!("{selected:?}"),
            format!(
                "{:?}",
                Expression::VarRef {
                    name: VarName::new("aimcData.statorCoreParameters.wRef").into(),
                    subscripts: vec![],
                    span: owner_span,
                }
            )
        );
    }
}
