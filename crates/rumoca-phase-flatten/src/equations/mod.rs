//! Equation flattening for the flatten phase.
//!
//! SPEC_0021 file-size exception: split plan is to move focused equation
//! lowering helpers into owned submodules after BOPTEST parity stabilization.
//!
//! This module converts instance equations to flat equations in
//! residual form (0 = residual).

use std::sync::Arc;

// Conditional tracing support (SPEC_0024)
use rumoca_eval_flat::constant::{EvalContext, Value};
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;
#[cfg(feature = "tracing")]
use tracing::{debug, warn};

use crate::boolean_eval::{
    is_structural_expression, try_eval_boolean_with_ctx_inner, try_eval_structural_boolean,
    try_resolve_enum_value,
};
use crate::errors::FlattenError;
use crate::pipeline::try_eval_const_boolean_with_scope;
use crate::static_subscripts::try_constant_integer;
use crate::{Context, qualify_expression_imports_with_def_map_ctx};

mod conditional_and_eval;
mod connections_graph;
mod flattened_equations;
mod structured_domain;
mod zero_sized_reductions;
pub(crate) use conditional_and_eval::build_eval_context;
use conditional_and_eval::*;
pub(crate) use conditional_and_eval::{
    expand_range_indices, substitute_index_in_equation, substitute_index_in_expression,
};
use connections_graph::{extract_vcg_data_from_function_call, is_side_effect_only_function};
pub(crate) use flattened_equations::FlattenedEquations;
use structured_domain::{
    SourceStructuredIteration, compact_domain_from_iterations, lift_full_iteration_child_family,
};
use zero_sized_reductions::{expand_reduction_over_array_ref, simplify_zero_sized_reductions};

type ClassTree = ast::ClassTree;
type ComponentRefPart = ast::ComponentRefPart;
type ComponentReference = ast::ComponentReference;
type EquationBlock = ast::EquationBlock;
type ForIndex = ast::ForIndex;
type OpBinary = rumoca_core::OpBinary;
type QualifiedName = ast::QualifiedName;
type TerminalType = ast::TerminalType;
type Token = rumoca_core::Token;
#[cfg(test)]
type InstanceEquation = ast::InstanceEquation;
type AssertEquation = flat::AssertEquation;

#[derive(Debug, Clone)]
struct ArrayRefExpansion {
    path: String,
    dims: Vec<i64>,
    component_part_index: Option<usize>,
}

/// Build a qualified name string from a prefix and component reference.
///
/// Combines the prefix parts (from the current scope) with the component
/// reference parts to create a fully qualified name like "model.sub.var".
///
/// MLS §10.1: Array subscripts are part of the variable identity.
/// For `filter[1].n`, the qualified name includes the subscript: "prefix.filter[1].n".
pub(crate) fn build_qualified_name(
    prefix: &ast::QualifiedName,
    cr: &ast::ComponentReference,
) -> String {
    let mut parts: Vec<String> = prefix
        .parts
        .iter()
        .map(|(name, subs)| {
            if subs.is_empty() {
                name.clone()
            } else {
                format!(
                    "{}[{}]",
                    name,
                    subs.iter()
                        .map(|s| s.to_string())
                        .collect::<Vec<_>>()
                        .join(",")
                )
            }
        })
        .collect();
    parts.extend(cr.parts.iter().map(format_component_ref_part));
    parts.join(".")
}

/// Format a component reference part, including subscripts if present.
///
/// MLS §10.1: Array subscripts are part of the variable identity.
/// - `filter` → "filter"
/// - `filter[1]` → "filter[1]"
/// - `matrix[1,2]` → "matrix[1,2]"
fn format_component_ref_part(part: &ast::ComponentRefPart) -> String {
    let name = part.ident.text.to_string();
    match &part.subs {
        Some(subs) if !subs.is_empty() => {
            let sub_strs: Vec<String> = subs.iter().map(format_subscript_for_lookup).collect();
            format!("{}[{}]", name, sub_strs.join(","))
        }
        _ => name,
    }
}

/// Format a subscript for parameter lookup.
///
/// Converts subscript expressions to string form for building qualified names.
/// Only handles concrete integer subscripts (after index substitution).
fn format_subscript_for_lookup(sub: &ast::Subscript) -> String {
    match sub {
        ast::Subscript::Expression(expr) => format_subscript_expr(expr),
        ast::Subscript::Range { .. } | ast::Subscript::Empty => ":".to_string(),
    }
}

/// Format a subscript expression to string.
fn format_subscript_expr(expr: &ast::Expression) -> String {
    if let Some(value) = try_constant_integer(expr) {
        return value.to_string();
    }

    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
            ..
        } => token.text.to_string(),
        ast::Expression::ComponentReference(cr) => {
            // Simple variable reference (should have been substituted)
            cr.parts
                .iter()
                .map(|p| p.ident.text.to_string())
                .collect::<Vec<_>>()
                .join(".")
        }
        ast::Expression::Unary { op, rhs, .. } => {
            format!("{}{}", op, format_subscript_expr(rhs))
        }
        ast::Expression::Binary { op, lhs, rhs, .. } => {
            format!(
                "({} {} {})",
                format_subscript_expr(lhs),
                op,
                format_subscript_expr(rhs)
            )
        }
        ast::Expression::FunctionCall { comp, args, .. } => {
            let func_name: String = comp
                .parts
                .iter()
                .map(|p| p.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");
            let arg_strs: Vec<String> = args.iter().map(format_subscript_expr).collect();
            format!("{}({})", func_name, arg_strs.join(", "))
        }
        ast::Expression::Range {
            start, step, end, ..
        } => match step {
            Some(s) => format!(
                "{}:{}:{}",
                format_subscript_expr(start),
                format_subscript_expr(s),
                format_subscript_expr(end)
            ),
            None => format!(
                "{}:{}",
                format_subscript_expr(start),
                format_subscript_expr(end)
            ),
        },
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::End,
            ..
        } => "end".to_string(),
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
            ..
        } => token.text.to_string(),
        _ => "?".to_string(),
    }
}

/// Get the parent scope of a qualified prefix.
///
/// "Model.sub.comp" -> Some("Model.sub")
/// "Model" -> Some("")
/// "" -> None
fn get_parent_prefix(prefix: &ast::QualifiedName) -> Option<ast::QualifiedName> {
    if prefix.parts.is_empty() {
        None
    } else {
        Some(ast::QualifiedName {
            parts: prefix.parts[..prefix.parts.len() - 1].to_vec(),
        })
    }
}

fn lookup_integer_param_with_unindexed_scope(ctx: &Context, key: &str) -> Option<i64> {
    if let Some(value) = ctx.get_integer_param(key) {
        return Some(value);
    }
    for candidate in crate::path_utils::unindexed_lookup_variants(key) {
        if let Some(value) = ctx.get_integer_param(&candidate) {
            return Some(value);
        }
    }
    None
}

fn lookup_array_dims_with_unindexed_scope(ctx: &Context, key: &str) -> Option<Vec<i64>> {
    if let Some(dims) = ctx.get_array_dims(key) {
        return Some(dims);
    }
    for candidate in crate::path_utils::unindexed_lookup_variants(key) {
        if let Some(dims) = ctx.get_array_dims(&candidate) {
            return Some(dims);
        }
    }
    None
}

/// Try to look up a parameter value in the current scope or any parent scope.
///
/// Per MLS scope resolution rules, a reference like `lines` in a nested component
/// `Model.s` should first try `Model.s.lines`, then `Model.lines`, then `lines`.
fn lookup_parameter_in_scope(
    ctx: &Context,
    cr: &ast::ComponentReference,
    prefix: &ast::QualifiedName,
) -> Option<i64> {
    let first_ident = cr
        .parts
        .first()
        .map(|p| p.ident.text.as_ref())
        .unwrap_or("");
    let is_type_ref = first_ident.starts_with(char::is_uppercase);
    let is_instance_ref = first_ident.starts_with(char::is_lowercase);

    // MLS §7.3: Replaceable type references like `Medium.nXi` map to instance
    // names like `medium.nXi` in the flat model. Try lowercasing the first part
    // of the component reference when it looks like a type name (starts uppercase).
    if is_type_ref && let Some(val) = try_lowercase_type_ref(ctx, cr, prefix) {
        return Some(val);
    }

    // Try fully qualified after active package aliases so an instance redeclare
    // can override stale defaults injected under the package alias spelling.
    let qualified = build_qualified_name(prefix, cr);
    if let Some(val) = lookup_integer_param_with_unindexed_scope(ctx, &qualified) {
        return Some(val);
    }

    // Flattened component references like `medium.nXi` should also resolve
    // against injected package constants like `Medium.nXi`.
    if is_instance_ref && let Some(val) = try_uppercase_instance_ref(ctx, cr, prefix) {
        return Some(val);
    }

    // Try parent scopes (with alias resolution)
    let mut current_prefix = prefix.clone();
    while let Some(parent) = get_parent_prefix(&current_prefix) {
        if is_type_ref && let Some(val) = try_lowercase_type_ref(ctx, cr, &parent) {
            return Some(val);
        }
        let parent_qualified = build_qualified_name(&parent, cr);
        if let Some(val) = lookup_integer_param_with_unindexed_scope(ctx, &parent_qualified) {
            #[cfg(feature = "tracing")]
            debug!(
                original = %qualified,
                found = %parent_qualified,
                "parameter resolved in parent scope"
            );
            return Some(val);
        }
        // Also try uppercase package alias form in parent scope
        if is_instance_ref && let Some(val) = try_uppercase_instance_ref(ctx, cr, &parent) {
            return Some(val);
        }
        current_prefix = parent;
    }

    // Try unqualified (root scope) - just the component reference itself
    // MLS §10.1: Include subscripts in the lookup (e.g., "filter[1].n")
    let unqualified = cr
        .parts
        .iter()
        .map(format_component_ref_part)
        .collect::<Vec<_>>()
        .join(".");
    if let Some(val) = lookup_integer_param_with_unindexed_scope(ctx, &unqualified) {
        return Some(val);
    }

    // Try lowercase type ref at root scope
    if is_type_ref && let Some(val) = try_lowercase_type_ref(ctx, cr, &ast::QualifiedName::new()) {
        return Some(val);
    }
    if is_instance_ref
        && let Some(val) = try_uppercase_instance_ref(ctx, cr, &ast::QualifiedName::new())
    {
        return Some(val);
    }

    // Common medium size constants (nX, nXi, nC, nS):
    // infer from already-known array dimensions in the current scope chain.
    if cr.parts.len() == 1 {
        let simple_name = cr.parts[0].ident.text.as_ref();
        if let Some(val) = infer_size_constant_from_dims(ctx, simple_name, prefix) {
            return Some(val);
        }
    }

    // MLS §7.3: `Medium.nXi` in conservation-balance scopes can be recovered from
    // already-instantiated medium-sized arrays when package constants were not
    // materialized as scalar parameters during constant injection.
    if is_type_ref
        && cr.parts.len() >= 2
        && let Some(val) = infer_medium_size_constant_from_type_ref(ctx, cr, prefix)
    {
        return Some(val);
    }

    None
}

/// Infer package size constants referenced through replaceable type aliases.
///
/// Example: in `ConservationEquation`, `Medium.nXi` can be recovered from the
/// first dimension of `mXi`, `XiOut`, or similar arrays already sized during
/// instantiation even when `medium.nXi` was not injected as a scalar parameter.
fn infer_medium_size_constant_from_type_ref(
    ctx: &Context,
    cr: &ast::ComponentReference,
    prefix: &ast::QualifiedName,
) -> Option<i64> {
    let constant_name = cr.parts.last()?.ident.text.as_ref();
    let array_candidates: &[&str] = match constant_name {
        "nXi" => &[
            "Xi",
            "mXi",
            "XiOut",
            "mXiOut",
            "XiOut_internal",
            "mbXi_flow",
        ],
        "nX" => &["X"],
        "nC" => &[
            "C",
            "COut",
            "mC",
            "COut_internal",
            "C_flow_internal",
            "s",
            "extraPropertiesNames",
            "C_nominal",
        ],
        "nS" => &["substanceNames"],
        _ => return None,
    };

    let first = cr.parts.first()?.ident.text.as_ref();
    if !first.starts_with(char::is_uppercase) {
        return None;
    }
    let lowered = {
        let mut chars = first.chars();
        let head = chars.next()?;
        head.to_lowercase().collect::<String>() + chars.as_str()
    };

    let mut current = Some(prefix.clone());
    while let Some(scope_qn) = current {
        let scope = scope_qn.to_flat_string();
        for candidate in array_candidates {
            for key in medium_size_array_lookup_keys(&scope, &lowered, candidate) {
                if let Some(dims) = lookup_array_dims_with_unindexed_scope(ctx, &key)
                    && let Some(size) = medium_size_from_array_dims(constant_name, &dims)
                {
                    return Some(size);
                }
            }
        }
        current = get_parent_prefix(&scope_qn);
    }

    None
}

fn medium_size_array_lookup_keys(
    scope: &str,
    lowered_alias: &str,
    array_name: &str,
) -> [String; 2] {
    [
        if scope.is_empty() {
            array_name.to_string()
        } else {
            format!("{scope}.{array_name}")
        },
        if scope.is_empty() {
            format!("{lowered_alias}.{array_name}")
        } else {
            format!("{scope}.{lowered_alias}.{array_name}")
        },
    ]
}

fn medium_size_from_array_dims(constant_name: &str, dims: &[i64]) -> Option<i64> {
    match constant_name {
        "nXi" | "nX" | "nC" | "nS" => dims.last().copied(),
        _ => None,
    }
}

/// Try looking up a type reference with the first part lowercased.
///
/// In Modelica, replaceable types like `Medium` map to instance components
/// like `medium` in the flat model. `Medium.nXi` should resolve to `medium.nXi`.
fn try_lowercase_type_ref(
    ctx: &Context,
    cr: &ast::ComponentReference,
    prefix: &ast::QualifiedName,
) -> Option<i64> {
    let mut parts: Vec<String> = prefix.parts.iter().map(|(name, _)| name.clone()).collect();
    // Lowercase the first part of the component reference
    let first = cr.parts[0].ident.text.to_string();
    let lowered = first[..1].to_lowercase() + &first[1..];
    parts.push(lowered);
    // Add remaining parts as-is
    parts.extend(cr.parts[1..].iter().map(format_component_ref_part));
    let lowered_name = parts.join(".");
    lookup_integer_param_with_unindexed_scope(ctx, &lowered_name)
}

/// Try looking up an instance-qualified reference with the first part uppercased.
///
/// Example: `medium.nXi` -> `Medium.nXi`.
fn try_uppercase_instance_ref(
    ctx: &Context,
    cr: &ast::ComponentReference,
    prefix: &ast::QualifiedName,
) -> Option<i64> {
    let mut parts: Vec<String> = prefix.parts.iter().map(|(name, _)| name.clone()).collect();
    let first = cr.parts[0].ident.text.to_string();
    let uppered = first[..1].to_uppercase() + &first[1..];
    parts.push(uppered);
    parts.extend(cr.parts[1..].iter().map(format_component_ref_part));
    let uppered_name = parts.join(".");
    lookup_integer_param_with_unindexed_scope(ctx, &uppered_name)
}

/// Infer structural size constants from known array dimensions in scope.
///
/// Examples:
/// - `nX`  from `X[:]`
/// - `nXi` from `Xi[:]`
/// - `nC`  from `C[:]`
/// - `nS`  from `substanceNames[:]`
/// - `nr`  from `r[:]`
/// - `na`  from `a[:]`
/// - `ncr` from `cr[:]`
/// - `nc0` from `c0[:]`
/// - `nout` from `columns[:]`
fn infer_size_constant_from_dims(
    ctx: &Context,
    constant_name: &str,
    prefix: &ast::QualifiedName,
) -> Option<i64> {
    let candidates: &[&str] = match constant_name {
        "nX" => &["X"],
        "nXi" => &["Xi"],
        "nC" => &["C"],
        "nS" => &["substanceNames"],
        "nr" => &["r"],
        "na" => &["a", "b", "ku"],
        "ncr" => &["cr"],
        "nc0" => &["c0", "c1"],
        "nout" => &["columns"],
        _ => return None,
    };

    let mut current = Some(prefix.clone());
    while let Some(scope_qn) = current {
        let scope = scope_qn.to_flat_string();
        for candidate in candidates {
            let qualified = if scope.is_empty() {
                (*candidate).to_string()
            } else {
                format!("{scope}.{candidate}")
            };
            if let Some(dims) = lookup_array_dims_with_unindexed_scope(ctx, &qualified)
                && let Some(&first) = dims.first()
            {
                return Some(first);
            }
        }
        current = get_parent_prefix(&scope_qn);
    }

    None
}

/// Flatten an equation with optional def-map canonicalization for function references.
pub(crate) fn flatten_equation_with_def_map(
    ctx: &Context,
    inst_eq: &ast::InstanceEquation,
    prefix: &ast::QualifiedName,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<FlattenedEquations, FlattenError> {
    let span = inst_eq.span;
    let origin = rumoca_ir_flat::EquationOrigin::ComponentEquation {
        component: inst_eq.origin.to_flat_string(),
    };

    match &inst_eq.equation {
        ast::Equation::Empty => Ok(FlattenedEquations::default()),

        ast::Equation::Simple { lhs, rhs } => {
            // MLS Appendix B allows edge()/change() in discrete equations:
            // "The discrete equation: d_i = f_i(d, pre(d), p, t) at events
            //  may contain edge and change function calls."

            // Debug output for array references (MLS §10.5).
            // Array equations are preserved; expansion is deferred.
            #[cfg(feature = "tracing")]
            {
                let lhs_refs = find_array_refs_needing_expansion(lhs, prefix, ctx);
                let rhs_refs = find_array_refs_needing_expansion(rhs, prefix, ctx);
                if !lhs_refs.is_empty() || !rhs_refs.is_empty() {
                    debug!(
                        lhs_refs = ?lhs_refs,
                        rhs_refs = ?rhs_refs,
                        origin = %origin,
                        "found array refs in equation"
                    );
                }
            }

            // MLS §10.5: Check for range subscripts that evaluate to empty ranges.
            // For example, `der(x_scaled[2:nx])` with nx=1 produces range 2:1
            // which is empty, so the equation should be skipped entirely.
            let lhs_empty = has_empty_range_subscript(ctx, lhs, prefix);
            let rhs_empty = has_empty_range_subscript(ctx, rhs, prefix);
            if lhs_empty || rhs_empty {
                return Ok(FlattenedEquations::default());
            }

            // MLS §10.4.1: Preserve array-comprehension equations by expanding
            // structural ranges before AST->Flat conversion.
            let lhs = expand_array_comprehensions_in_expression(ctx, lhs, prefix, span)?;
            let rhs = expand_array_comprehensions_in_expression(ctx, rhs, prefix, span)?;
            let lhs = simplify_zero_sized_reductions(ctx, &lhs, prefix);
            let rhs = simplify_zero_sized_reductions(ctx, &rhs, prefix);

            // Preserve simple equations as a single residual equation.
            // Array scalarization and counting are handled downstream.
            let residual = make_residual(ctx, &lhs, &rhs, prefix, def_map)?;
            let scalar_count = infer_simple_equation_scalar_count(&lhs, &rhs, prefix, ctx);
            if scalar_count == 0 {
                return Ok(FlattenedEquations::default());
            }

            let equation = if scalar_count == 1 {
                flat::Equation::new(residual, span, origin)
            } else {
                flat::Equation::new_array(residual, span, origin, scalar_count)
            };
            Ok(FlattenedEquations {
                equations: vec![equation],
                structured_equations: vec![],
                assert_equations: vec![],
                when_clauses: vec![],
                definite_roots: vec![],
                branches: vec![],
                potential_roots: vec![],
            })
        }

        ast::Equation::Connect { .. } => {
            // Connections are handled separately in the connections module
            Ok(FlattenedEquations::default())
        }

        ast::Equation::For { indices, equations } => {
            // Expand for-equations by iterating over indices (MLS §8.3.3)
            // This now also handles when-equations inside for-loops (MLS §8.3.5)
            expand_for_equation(ctx, indices, equations, prefix, span, &origin, def_map)
        }

        ast::Equation::When(_blocks) => {
            // When-equations are handled separately by flatten_when_equation()
            // Return empty here since they don't produce regular flat equations
            Ok(FlattenedEquations::default())
        }

        ast::Equation::If {
            cond_blocks,
            else_block,
        } => {
            // Convert if-equations to conditional expressions (MLS §8.3.4)
            expand_if_equation(ctx, cond_blocks, else_block, prefix, span, &origin, def_map)
        }

        ast::Equation::FunctionCall { comp, args } => {
            flatten_function_call_equation(ctx, comp, args, prefix, span, def_map)
        }

        ast::Equation::Assert {
            condition,
            message,
            level,
        } => flatten_assert_equation(
            ctx,
            condition,
            message,
            level.as_ref(),
            prefix,
            span,
            def_map,
        ),
    }
}

fn flatten_assert_equation(
    ctx: &Context,
    condition: &ast::Expression,
    message: &ast::Expression,
    level: Option<&ast::Expression>,
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<FlattenedEquations, FlattenError> {
    // MLS §8.3.7: preserve assert-equations for runtime checks in flat output.
    // They do not contribute to the DAE residual equation system.
    let imports = &ctx.current_imports;
    let assert_eq = flat::AssertEquation::new(
        qualify_expression_imports_with_def_map_ctx(condition, prefix, imports, def_map, ctx)?,
        qualify_expression_imports_with_def_map_ctx(message, prefix, imports, def_map, ctx)?,
        level
            .map(|expr| {
                qualify_expression_imports_with_def_map_ctx(expr, prefix, imports, def_map, ctx)
            })
            .transpose()?,
        span,
    );
    Ok(FlattenedEquations {
        equations: vec![],
        structured_equations: vec![],
        assert_equations: vec![assert_eq],
        when_clauses: vec![],
        definite_roots: vec![],
        branches: vec![],
        potential_roots: vec![],
    })
}

fn flatten_function_call_equation(
    ctx: &Context,
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<FlattenedEquations, FlattenError> {
    if is_assert_function_call(comp) {
        return flatten_assert_function_call(ctx, args, prefix, span, def_map);
    }
    extract_vcg_data_from_function_call(comp, args, prefix)
}

fn is_assert_function_call(comp: &ast::ComponentReference) -> bool {
    comp.parts
        .last()
        .map(|part| part.ident.text.as_ref() == "assert")
        .unwrap_or(false)
}

fn flatten_assert_function_call(
    ctx: &Context,
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<FlattenedEquations, FlattenError> {
    let positional: Vec<&ast::Expression> = args
        .iter()
        .filter(|arg| !matches!(arg, ast::Expression::NamedArgument { .. }))
        .collect();
    let condition = named_call_arg(args, "condition").or_else(|| positional.first().copied());
    let message = named_call_arg(args, "message").or_else(|| positional.get(1).copied());
    let level = named_call_arg(args, "level").or_else(|| positional.get(2).copied());

    let (condition, message) = match (condition, message) {
        (Some(condition), Some(message)) => (condition, message),
        _ => {
            return Err(FlattenError::unsupported_equation(
                "assert() equation requires at least condition and message arguments",
                span,
            ));
        }
    };

    let imports = &ctx.current_imports;
    let assert_eq = flat::AssertEquation::new(
        qualify_expression_imports_with_def_map_ctx(condition, prefix, imports, def_map, ctx)?,
        qualify_expression_imports_with_def_map_ctx(message, prefix, imports, def_map, ctx)?,
        level
            .map(|expr| {
                qualify_expression_imports_with_def_map_ctx(expr, prefix, imports, def_map, ctx)
            })
            .transpose()?,
        span,
    );

    Ok(FlattenedEquations {
        equations: vec![],
        structured_equations: vec![],
        assert_equations: vec![assert_eq],
        when_clauses: vec![],
        definite_roots: vec![],
        branches: vec![],
        potential_roots: vec![],
    })
}

fn named_call_arg<'a>(args: &'a [ast::Expression], name: &str) -> Option<&'a ast::Expression> {
    args.iter().find_map(|arg| {
        if let ast::Expression::NamedArgument {
            name: arg_name,
            value,
            ..
        } = arg
            && arg_name.text.as_ref() == name
        {
            Some(value.as_ref())
        } else {
            None
        }
    })
}

/// Create a residual expression: lhs - rhs
fn make_residual(
    ctx: &Context,
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    prefix: &ast::QualifiedName,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<rumoca_core::Expression, FlattenError> {
    // Create: lhs - rhs
    let residual = ast::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Arc::new(lhs.clone()),
        rhs: Arc::new(rhs.clone()),
        span: lhs.span(),
    };

    qualify_expression_imports_with_def_map_ctx(
        &residual,
        prefix,
        &ctx.current_imports,
        def_map,
        ctx,
    )
}

/// Expand array comprehensions in equation expressions when index ranges are structural.
///
/// This preserves current scalar-count and ToDAE expectations for equation residuals
/// while `ast::Expression::ArrayComprehension` support is rolled through downstream passes.
// SPEC_0021: Exception - exhaustive expression-tree rewrite over AST variants.
#[allow(clippy::too_many_lines)]
fn expand_array_comprehensions_in_expression(
    ctx: &Context,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
) -> Result<ast::Expression, FlattenError> {
    match expr {
        ast::Expression::ArrayComprehension {
            expr: body,
            indices,
            filter,
            ..
        } => expand_array_comprehension_expression(
            ctx,
            body,
            indices,
            filter.as_deref(),
            prefix,
            span,
        ),
        ast::Expression::Binary { op, lhs, rhs, span } => Ok(ast::Expression::Binary {
            op: op.clone(),
            lhs: Arc::new(expand_array_comprehensions_in_expression(
                ctx, lhs, prefix, *span,
            )?),
            rhs: Arc::new(expand_array_comprehensions_in_expression(
                ctx, rhs, prefix, *span,
            )?),
            span: *span,
        }),
        ast::Expression::Unary { op, rhs, span } => Ok(ast::Expression::Unary {
            op: op.clone(),
            rhs: Arc::new(expand_array_comprehensions_in_expression(
                ctx, rhs, prefix, *span,
            )?),
            span: *span,
        }),
        ast::Expression::FunctionCall { comp, args, span } => {
            if let Some(expanded) = expand_reduction_over_array_ref(ctx, comp, args, prefix, *span)?
            {
                return Ok(expanded);
            }
            Ok(ast::Expression::FunctionCall {
                comp: comp.clone(),
                args: expand_expression_list(ctx, args, prefix, *span)?,
                span: *span,
            })
        }
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => expand_if_expression(ctx, branches, else_branch, prefix, span),
        ast::Expression::Array {
            elements,
            is_matrix,
            span,
        } => Ok(ast::Expression::Array {
            elements: expand_expression_list(ctx, elements, prefix, *span)?,
            is_matrix: *is_matrix,
            span: *span,
        }),
        ast::Expression::Tuple { elements, span } => Ok(ast::Expression::Tuple {
            elements: expand_expression_list(ctx, elements, prefix, *span)?,
            span: *span,
        }),
        ast::Expression::Range {
            start,
            step,
            end,
            span,
        } => Ok(ast::Expression::Range {
            start: Arc::new(expand_array_comprehensions_in_expression(
                ctx, start, prefix, *span,
            )?),
            step: step
                .as_ref()
                .map(|s| expand_array_comprehensions_in_expression(ctx, s, prefix, *span))
                .transpose()?
                .map(Arc::new),
            end: Arc::new(expand_array_comprehensions_in_expression(
                ctx, end, prefix, *span,
            )?),
            span: *span,
        }),
        ast::Expression::Parenthesized { inner, span } => Ok(ast::Expression::Parenthesized {
            inner: Arc::new(expand_array_comprehensions_in_expression(
                ctx, inner, prefix, *span,
            )?),
            span: *span,
        }),
        ast::Expression::ArrayIndex {
            base,
            subscripts,
            span,
        } => Ok(ast::Expression::ArrayIndex {
            base: Arc::new(expand_array_comprehensions_in_expression(
                ctx, base, prefix, *span,
            )?),
            subscripts: subscripts.clone(),
            span: *span,
        }),
        ast::Expression::FieldAccess { base, field, span } => Ok(ast::Expression::FieldAccess {
            base: Arc::new(expand_array_comprehensions_in_expression(
                ctx, base, prefix, *span,
            )?),
            field: field.clone(),
            span: *span,
        }),
        _ => Ok(expr.clone()),
    }
}

fn expand_expression_list(
    ctx: &Context,
    exprs: &[ast::Expression],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
) -> Result<Vec<ast::Expression>, FlattenError> {
    exprs
        .iter()
        .map(|expr| expand_array_comprehensions_in_expression(ctx, expr, prefix, span))
        .collect()
}

fn expand_if_expression(
    ctx: &Context,
    branches: &[(ast::Expression, ast::Expression)],
    else_branch: &ast::Expression,
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
) -> Result<ast::Expression, FlattenError> {
    let branches = branches
        .iter()
        .map(|(cond, then_expr)| {
            Ok::<_, FlattenError>((
                expand_array_comprehensions_in_expression(ctx, cond, prefix, span)?,
                expand_array_comprehensions_in_expression(ctx, then_expr, prefix, span)?,
            ))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let else_branch = Arc::new(expand_array_comprehensions_in_expression(
        ctx,
        else_branch,
        prefix,
        span,
    )?);
    Ok(ast::Expression::If {
        branches,
        else_branch,
        span,
    })
}

fn expand_array_comprehension_expression(
    ctx: &Context,
    body: &ast::Expression,
    indices: &[ast::ForIndex],
    filter: Option<&ast::Expression>,
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
) -> Result<ast::Expression, FlattenError> {
    let mut index_ranges: Vec<(String, Vec<i64>)> = Vec::new();
    for idx in indices {
        let values = expand_range_indices(ctx, &idx.range, prefix, span)?;
        index_ranges.push((idx.ident.text.to_string(), values));
    }

    let env = ArrayComprehensionExpansionEnv {
        ctx,
        prefix,
        span,
        index_ranges: &index_ranges,
    };
    let mut expanded_elements = Vec::new();
    expand_array_comprehension_recursive(
        &env,
        body.clone(),
        filter.cloned(),
        0,
        &mut expanded_elements,
    )?;

    Ok(ast::Expression::Array {
        elements: expanded_elements,
        is_matrix: matches!(body, ast::Expression::Array { .. }),
        span,
    })
}

struct ArrayComprehensionExpansionEnv<'a> {
    ctx: &'a Context,
    prefix: &'a ast::QualifiedName,
    span: rumoca_core::Span,
    index_ranges: &'a [(String, Vec<i64>)],
}

fn expand_array_comprehension_recursive(
    env: &ArrayComprehensionExpansionEnv<'_>,
    body: ast::Expression,
    filter: Option<ast::Expression>,
    depth: usize,
    out: &mut Vec<ast::Expression>,
) -> Result<(), FlattenError> {
    if depth == env.index_ranges.len() {
        if let Some(filter_expr) = &filter {
            match try_eval_boolean_with_ctx_inner(filter_expr, Some(env.ctx), env.prefix) {
                Some(true) => {}
                Some(false) => return Ok(()),
                None => {
                    return Err(FlattenError::unsupported_equation(
                        "array-comprehension filter must be structurally evaluable in equations",
                        env.span,
                    ));
                }
            }
        }
        out.push(expand_array_comprehensions_in_expression(
            env.ctx, &body, env.prefix, env.span,
        )?);
        return Ok(());
    }

    let (idx_name, values) = &env.index_ranges[depth];
    for &val in values {
        let substituted_body = substitute_index_in_expression(&body, idx_name, val);
        let substituted_filter = filter
            .as_ref()
            .map(|f| substitute_index_in_expression(f, idx_name, val));
        expand_array_comprehension_recursive(
            env,
            substituted_body,
            substituted_filter,
            depth + 1,
            out,
        )?;
    }
    Ok(())
}

// ============================================================================
// Array flat::Equation Expansion (MLS §10.5)
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExpressionShape {
    Scalar,
    Vector(i64),
    Matrix(i64, i64),
    Other,
}

fn expression_shape_from_dims(dims: &[i64]) -> ExpressionShape {
    match dims {
        [] => ExpressionShape::Scalar,
        [n] => ExpressionShape::Vector((*n).max(0)),
        [r, c] => ExpressionShape::Matrix((*r).max(0), (*c).max(0)),
        _ => ExpressionShape::Other,
    }
}

fn infer_component_ref_shape(
    cr: &ast::ComponentReference,
    prefix: &ast::QualifiedName,
    ctx: &Context,
) -> ExpressionShape {
    let qualified = build_qualified_name(prefix, cr);
    // Exact lookup already includes any expanded parent indices in the key
    // (e.g. `medium_T[2].state.X`), so dims are already projected.
    if let Some(dims) = ctx.get_array_dimensions(&qualified) {
        return expression_shape_from_dims(dims);
    }

    // Fall back to the unscripted path and project by subscripts in the reference.
    // This handles references like `A[i]` when only `A` has known dimensions.
    let unscripted = strip_subscripts_from_component_ref(cr);
    let qualified_unscripted = build_qualified_name(prefix, &unscripted);
    let Some(dims) = ctx.get_array_dimensions(&qualified_unscripted) else {
        return ExpressionShape::Scalar;
    };
    expression_shape_from_dims(&project_component_dims_by_subscripts(dims, cr))
}

fn strip_subscripts_from_component_ref(cr: &ast::ComponentReference) -> ast::ComponentReference {
    let mut stripped = cr.clone();
    for part in &mut stripped.parts {
        part.subs = None;
    }
    stripped
}

fn project_component_dims_by_subscripts(dims: &[i64], cr: &ast::ComponentReference) -> Vec<i64> {
    if dims.is_empty() {
        return Vec::new();
    }

    let mut remaining_dims = Vec::new();
    let mut dim_idx = 0usize;

    for part in &cr.parts {
        let Some(subs) = &part.subs else {
            continue;
        };
        for sub in subs {
            if dim_idx >= dims.len() {
                break;
            }
            match sub {
                ast::Subscript::Expression(_) => {
                    dim_idx += 1;
                }
                ast::Subscript::Range { .. } => {
                    remaining_dims.push(dims[dim_idx]);
                    dim_idx += 1;
                }
                ast::Subscript::Empty => {}
            }
        }
    }

    remaining_dims.extend_from_slice(&dims[dim_idx..]);
    remaining_dims
}

fn combine_additive_shapes(lhs: ExpressionShape, rhs: ExpressionShape) -> ExpressionShape {
    match (lhs, rhs) {
        (ExpressionShape::Scalar, ExpressionShape::Scalar) => ExpressionShape::Scalar,
        (ExpressionShape::Vector(n), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Vector(n)) => ExpressionShape::Vector(n),
        (ExpressionShape::Matrix(r, c), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Matrix(r, c)) => ExpressionShape::Matrix(r, c),
        (ExpressionShape::Vector(a), ExpressionShape::Vector(b)) if a == b => {
            ExpressionShape::Vector(a)
        }
        (ExpressionShape::Matrix(r1, c1), ExpressionShape::Matrix(r2, c2))
            if r1 == r2 && c1 == c2 =>
        {
            ExpressionShape::Matrix(r1, c1)
        }
        _ => ExpressionShape::Other,
    }
}

fn combine_mul_shapes(lhs: ExpressionShape, rhs: ExpressionShape) -> ExpressionShape {
    match (lhs, rhs) {
        (ExpressionShape::Scalar, ExpressionShape::Scalar) => ExpressionShape::Scalar,
        (ExpressionShape::Vector(n), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Vector(n)) => ExpressionShape::Vector(n),
        (ExpressionShape::Matrix(r, c), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Matrix(r, c)) => ExpressionShape::Matrix(r, c),
        // Modelica vector * vector is dot-product.
        (ExpressionShape::Vector(a), ExpressionShape::Vector(b)) if a == b => {
            ExpressionShape::Scalar
        }
        (ExpressionShape::Vector(v), ExpressionShape::Matrix(r, c)) if v == r => {
            ExpressionShape::Vector(c)
        }
        (ExpressionShape::Matrix(r, c), ExpressionShape::Vector(v)) if c == v => {
            ExpressionShape::Vector(r)
        }
        (ExpressionShape::Matrix(r1, c1), ExpressionShape::Matrix(r2, c2)) if c1 == r2 => {
            ExpressionShape::Matrix(r1, c2)
        }
        _ => ExpressionShape::Other,
    }
}

fn combine_elementwise_shapes(lhs: ExpressionShape, rhs: ExpressionShape) -> ExpressionShape {
    match (lhs, rhs) {
        (ExpressionShape::Scalar, ExpressionShape::Scalar) => ExpressionShape::Scalar,
        (ExpressionShape::Vector(n), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Vector(n)) => ExpressionShape::Vector(n),
        (ExpressionShape::Matrix(r, c), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Matrix(r, c)) => ExpressionShape::Matrix(r, c),
        (ExpressionShape::Vector(a), ExpressionShape::Vector(b)) if a == b => {
            ExpressionShape::Vector(a)
        }
        (ExpressionShape::Matrix(r1, c1), ExpressionShape::Matrix(r2, c2))
            if r1 == r2 && c1 == c2 =>
        {
            ExpressionShape::Matrix(r1, c1)
        }
        _ => ExpressionShape::Other,
    }
}

fn infer_scalar_rhs_shape(
    lhs_shape: ExpressionShape,
    rhs_shape: ExpressionShape,
) -> ExpressionShape {
    match (lhs_shape, rhs_shape) {
        (ExpressionShape::Scalar, ExpressionShape::Scalar) => ExpressionShape::Scalar,
        (ExpressionShape::Vector(n), ExpressionShape::Scalar) => ExpressionShape::Vector(n),
        (ExpressionShape::Matrix(r, c), ExpressionShape::Scalar) => ExpressionShape::Matrix(r, c),
        _ => ExpressionShape::Other,
    }
}

fn infer_binary_shape(
    op: &rumoca_core::OpBinary,
    lhs_shape: ExpressionShape,
    rhs_shape: ExpressionShape,
) -> ExpressionShape {
    match op {
        rumoca_core::OpBinary::Add
        | rumoca_core::OpBinary::Sub
        | rumoca_core::OpBinary::AddElem
        | rumoca_core::OpBinary::SubElem => combine_additive_shapes(lhs_shape, rhs_shape),
        rumoca_core::OpBinary::Mul => combine_mul_shapes(lhs_shape, rhs_shape),
        rumoca_core::OpBinary::MulElem
        | rumoca_core::OpBinary::DivElem
        | rumoca_core::OpBinary::ExpElem => combine_elementwise_shapes(lhs_shape, rhs_shape),
        rumoca_core::OpBinary::Div | rumoca_core::OpBinary::Exp => {
            infer_scalar_rhs_shape(lhs_shape, rhs_shape)
        }
        rumoca_core::OpBinary::Eq
        | rumoca_core::OpBinary::Neq
        | rumoca_core::OpBinary::Lt
        | rumoca_core::OpBinary::Le
        | rumoca_core::OpBinary::Gt
        | rumoca_core::OpBinary::Ge
        | rumoca_core::OpBinary::And
        | rumoca_core::OpBinary::Or
        | rumoca_core::OpBinary::Assign
        | rumoca_core::OpBinary::Empty => ExpressionShape::Scalar,
    }
}

fn infer_expression_shape(
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
    ctx: &Context,
) -> ExpressionShape {
    match expr {
        ast::Expression::Terminal { .. } => ExpressionShape::Scalar,
        ast::Expression::ComponentReference(cr) => infer_component_ref_shape(cr, prefix, ctx),
        ast::Expression::Unary { rhs, .. } | ast::Expression::Parenthesized { inner: rhs, .. } => {
            infer_expression_shape(rhs, prefix, ctx)
        }
        ast::Expression::Binary { op, lhs, rhs, .. } => {
            let lhs_shape = infer_expression_shape(lhs, prefix, ctx);
            let rhs_shape = infer_expression_shape(rhs, prefix, ctx);
            infer_binary_shape(op, lhs_shape, rhs_shape)
        }
        ast::Expression::FunctionCall { comp, .. } => {
            if is_reduction_operator(comp) {
                ExpressionShape::Scalar
            } else {
                ExpressionShape::Other
            }
        }
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let else_shape = infer_expression_shape(else_branch, prefix, ctx);
            if branches
                .iter()
                .all(|(_, expr)| infer_expression_shape(expr, prefix, ctx) == else_shape)
            {
                else_shape
            } else {
                ExpressionShape::Other
            }
        }
        ast::Expression::Array {
            elements,
            is_matrix,
            ..
        } => {
            if *is_matrix {
                ExpressionShape::Other
            } else if elements
                .iter()
                .all(|e| infer_expression_shape(e, prefix, ctx) == ExpressionShape::Scalar)
            {
                ExpressionShape::Vector(elements.len() as i64)
            } else {
                ExpressionShape::Other
            }
        }
        ast::Expression::Range { .. }
        | ast::Expression::FieldAccess { .. }
        | ast::Expression::Tuple { .. }
        | ast::Expression::ArrayComprehension { .. }
        | ast::Expression::ArrayIndex { .. }
        | ast::Expression::NamedArgument { .. }
        | ast::Expression::Modification { .. }
        | ast::Expression::ClassModification { .. }
        | ast::Expression::Empty { .. } => ExpressionShape::Other,
    }
}

fn shape_scalar_size(shape: ExpressionShape) -> Option<usize> {
    match shape {
        ExpressionShape::Scalar => Some(1),
        ExpressionShape::Vector(n) => Some(n.max(0) as usize),
        ExpressionShape::Matrix(r, c) => {
            Some((r.max(0) as usize).saturating_mul(c.max(0) as usize))
        }
        ExpressionShape::Other => None,
    }
}

fn dims_scalar_size(dims: &[i64]) -> usize {
    if dims.is_empty() {
        1
    } else {
        dims.iter()
            .fold(1usize, |acc, d| acc.saturating_mul((*d).max(0) as usize))
    }
}

/// Infer scalar equation count for a simple equation without expanding it.
///
/// This preserves array equations as single residuals while still carrying scalar
/// size information for balance checking.
fn infer_simple_equation_scalar_count(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    prefix: &ast::QualifiedName,
    ctx: &Context,
) -> usize {
    let lhs_shape = infer_expression_shape(lhs, prefix, ctx);
    let rhs_shape = infer_expression_shape(rhs, prefix, ctx);

    match (lhs_shape, rhs_shape) {
        (ExpressionShape::Scalar, ExpressionShape::Scalar) => return 1,
        (ExpressionShape::Vector(n), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Vector(n)) => return n.max(0) as usize,
        (ExpressionShape::Matrix(r, c), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Matrix(r, c)) => {
            return (r.max(0) as usize).saturating_mul(c.max(0) as usize);
        }
        (ExpressionShape::Vector(a), ExpressionShape::Vector(b)) if a == b => {
            return a.max(0) as usize;
        }
        (ExpressionShape::Matrix(r1, c1), ExpressionShape::Matrix(r2, c2))
            if r1 == r2 && c1 == c2 =>
        {
            return (r1.max(0) as usize).saturating_mul(c1.max(0) as usize);
        }
        _ => {}
    }

    // Prefer LHS shape when available (equation result shape is usually driven by LHS).
    if let Some(size) = shape_scalar_size(lhs_shape)
        && size != 1
    {
        return size;
    }
    if let Some(size) = shape_scalar_size(rhs_shape)
        && size != 1
    {
        return size;
    }

    // Fallback to array-reference dimensional metadata.
    if let Some(array_ref) = find_array_refs_needing_expansion(lhs, prefix, ctx).first() {
        return dims_scalar_size(&array_ref.dims);
    }
    1
}

/// Find array variables in an expression that need index expansion.
///
/// Returns (qualified_path, dimensions) for each array variable found that:
/// 1. Is referenced without subscripts (e.g., `plug_p.pin.v` not `plug_p.pin[1].v`)
/// 2. Has known array dimensions in the context
///
/// This enables determining equation scalar size while preserving array equations
/// as a single residual.
fn find_array_refs_needing_expansion(
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
    ctx: &Context,
) -> Vec<ArrayRefExpansion> {
    let mut results = Vec::new();
    find_array_refs_recursive(expr, prefix, ctx, &mut results);
    results
}

/// Check if a path segment already has subscripts in the component reference.
fn has_subscript_at_index(cr: &ast::ComponentReference, cr_part_index: usize) -> bool {
    cr_part_index < cr.parts.len() && cr.parts[cr_part_index].subs.is_some()
}

/// Try to find array dimensions for a component reference and add to results.
fn find_array_ref_in_component(
    cr: &ast::ComponentReference,
    prefix: &ast::QualifiedName,
    ctx: &Context,
    results: &mut Vec<ArrayRefExpansion>,
) {
    let mut path = String::new();
    let prefix_parts = prefix.parts.iter().map(|(name, subs)| {
        let formatted = if subs.is_empty() {
            name.clone()
        } else {
            format!(
                "{}[{}]",
                name,
                subs.iter()
                    .map(|sub| sub.to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            )
        };
        (
            formatted,
            None,
            !subs.is_empty() || rumoca_core::split_trailing_subscript_suffix(name).is_some(),
        )
    });
    let component_parts = cr.parts.iter().enumerate().map(|(index, part)| {
        (
            format_component_ref_part(part),
            Some(index),
            has_subscript_at_index(cr, index),
        )
    });

    for (segment, component_part_index, already_subscripted) in prefix_parts.chain(component_parts)
    {
        if !path.is_empty() {
            path.push('.');
        }
        path.push_str(&segment);
        let Some(dims) = ctx.get_array_dimensions(&path) else {
            continue;
        };

        if dims.is_empty() {
            continue;
        }

        if already_subscripted {
            continue;
        }

        // Add if not already present
        if !results.iter().any(|array_ref| array_ref.path == path) {
            results.push(ArrayRefExpansion {
                path: path.clone(),
                dims: dims.clone(),
                component_part_index,
            });
        }
        break; // Found array, stop looking (innermost first)
    }
}

/// Check if a function call is a Modelica reduction operator (MLS §10.3.4).
/// Reduction operators (sum, product, min, max) take arrays and return scalars,
/// so array arguments inside them should NOT trigger equation expansion.
fn is_reduction_operator(comp: &ast::ComponentReference) -> bool {
    if comp.parts.len() == 1 {
        let name = comp.parts[0].ident.text.as_ref();
        matches!(name, "sum" | "product" | "min" | "max")
    } else {
        false
    }
}

/// Recursively walk an expression tree to find array references.
fn find_array_refs_recursive(
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
    ctx: &Context,
    results: &mut Vec<ArrayRefExpansion>,
) {
    match expr {
        ast::Expression::ComponentReference(cr) => {
            find_array_ref_in_component(cr, prefix, ctx, results);
        }

        ast::Expression::Binary { lhs, rhs, .. } => {
            find_array_refs_recursive(lhs, prefix, ctx, results);
            find_array_refs_recursive(rhs, prefix, ctx, results);
        }

        ast::Expression::Unary { rhs, .. } => {
            find_array_refs_recursive(rhs, prefix, ctx, results);
        }

        ast::Expression::FunctionCall { comp, args, .. } => {
            // MLS §10.3.4: Reduction operators (sum, product, min, max) take arrays
            // and return scalars. Array refs inside them should NOT trigger expansion.
            if !is_reduction_operator(comp) {
                for arg in args {
                    find_array_refs_recursive(arg, prefix, ctx, results);
                }
            }
        }

        ast::Expression::Parenthesized { inner, .. } => {
            find_array_refs_recursive(inner, prefix, ctx, results);
        }

        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                find_array_refs_recursive(cond, prefix, ctx, results);
                find_array_refs_recursive(then_expr, prefix, ctx, results);
            }
            find_array_refs_recursive(else_branch, prefix, ctx, results);
        }

        ast::Expression::Array { elements, .. } => {
            for elem in elements {
                find_array_refs_recursive(elem, prefix, ctx, results);
            }
        }

        ast::Expression::Range {
            start, step, end, ..
        } => {
            find_array_refs_recursive(start, prefix, ctx, results);
            if let Some(s) = step {
                find_array_refs_recursive(s, prefix, ctx, results);
            }
            find_array_refs_recursive(end, prefix, ctx, results);
        }

        ast::Expression::FieldAccess { base, .. } => {
            find_array_refs_recursive(base, prefix, ctx, results);
        }

        // Terminal expressions and others don't contain array references needing expansion
        ast::Expression::Terminal { .. }
        | ast::Expression::Tuple { .. }
        | ast::Expression::Empty { .. }
        | ast::Expression::NamedArgument { .. }
        | ast::Expression::Modification { .. }
        | ast::Expression::ClassModification { .. }
        | ast::Expression::ArrayComprehension { .. }
        | ast::Expression::ArrayIndex { .. } => {}
    }
}

/// Expand a for-equation by iterating over all index combinations.
///
/// MLS §8.3.3: "The for-equation construct allows iteration over a set of equations."
fn expand_for_equation(
    ctx: &Context,
    indices: &[ast::ForIndex],
    equations: &[ast::Equation],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    origin: &rumoca_ir_flat::EquationOrigin,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<FlattenedEquations, FlattenError> {
    // If no indices, just process the equations directly
    if indices.is_empty() {
        return flatten_equations_list(ctx, equations, prefix, span, origin, def_map);
    }

    let mut iterations = Vec::new();
    let mut result = FlattenedEquations::default();
    let mut index_values = Vec::with_capacity(indices.len());
    let env = ForIterationEnv {
        ctx,
        prefix,
        span,
        origin,
        def_map,
    };
    collect_for_iterations(
        &env,
        indices,
        equations,
        &mut index_values,
        &mut iterations,
        &mut result,
    )?;
    if iterations.is_empty() {
        return Ok(result);
    }
    let domain = compact_domain_from_iterations(indices, &iterations, span)?;
    let nested_family_lifted =
        lift_full_iteration_child_family(&mut result.structured_equations, &domain, &iterations);
    if nested_family_lifted {
        return Ok(result);
    }
    result
        .structured_equations
        .push(flat::StructuredEquationFamily {
            domain,
            first_equation_index: 0,
            equation_counts: iterations
                .iter()
                .map(|iteration| iteration.equation_count)
                .collect(),
            span,
            origin: origin.clone(),
        });

    Ok(result)
}

struct ForIterationEnv<'a> {
    ctx: &'a Context,
    prefix: &'a ast::QualifiedName,
    span: rumoca_core::Span,
    origin: &'a rumoca_ir_flat::EquationOrigin,
    def_map: Option<&'a crate::ResolveDefMap>,
}

fn collect_for_iterations(
    env: &ForIterationEnv<'_>,
    indices: &[ast::ForIndex],
    equations: &[ast::Equation],
    index_values: &mut Vec<i64>,
    iterations: &mut Vec<SourceStructuredIteration>,
    out: &mut FlattenedEquations,
) -> Result<(), FlattenError> {
    if indices.is_empty() {
        let flattened = flatten_equations_list(
            env.ctx,
            equations,
            env.prefix,
            env.span,
            env.origin,
            env.def_map,
        )?;
        iterations.push(SourceStructuredIteration {
            index_values: index_values.clone(),
            equation_count: flattened.equations.len(),
        });
        out.append(flattened);
        return Ok(());
    }

    let first_index = &indices[0];
    let remaining_indices = &indices[1..];
    let range_values = expand_range_indices(env.ctx, &first_index.range, env.prefix, env.span)?;
    let index_name = &first_index.ident.text;

    for value in range_values {
        let substituted: Vec<ast::Equation> = equations
            .iter()
            .map(|eq| substitute_index_in_equation(eq, index_name, value))
            .collect();
        index_values.push(value);
        collect_for_iterations(
            env,
            remaining_indices,
            &substituted,
            index_values,
            iterations,
            out,
        )?;
        index_values.pop();
    }

    Ok(())
}

#[derive(Clone)]
struct SimpleEquation {
    lhs: ast::Expression,
    rhs: ast::Expression,
}

/// Expand an if-equation by converting to conditional expressions.
///
/// MLS §8.3.4: "An if-equation creates a conditional set of equations."
fn expand_if_equation(
    ctx: &Context,
    cond_blocks: &[ast::EquationBlock],
    else_block: &Option<Vec<ast::Equation>>,
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    origin: &rumoca_ir_flat::EquationOrigin,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<FlattenedEquations, FlattenError> {
    if cond_blocks.is_empty() {
        return Ok(FlattenedEquations::default());
    }

    // First, try to evaluate constant conditions at compile time
    // This handles cases like "if false then ... else ... end if"
    // and parameter-dependent conditions that can be resolved
    if let Some(selected_branch) = try_select_constant_branch(ctx, cond_blocks, else_block, prefix)
    {
        return flatten_equations_list(ctx, &selected_branch, prefix, span, origin, def_map);
    }

    // Non-constant conditions: expand each branch to simple equations first
    // This handles for-equations, nested constant if-equations, etc.
    let mut expanded_branches: Vec<(ast::Expression, Vec<SimpleEquation>)> = Vec::new();

    for block in cond_blocks {
        let simple_eqs = expand_to_simple_equations(ctx, &block.eqs, prefix, span)?;
        expanded_branches.push((block.cond.clone(), simple_eqs));
    }

    let else_simple_eqs = if let Some(else_eqs) = else_block {
        expand_to_simple_equations(ctx, else_eqs, prefix, span)?
    } else {
        vec![]
    };

    // Check if all branches have the same number of equations
    let num_equations = expanded_branches[0].1.len();
    let all_same_count = expanded_branches
        .iter()
        .all(|(_, eqs)| eqs.len() == num_equations)
        && (else_simple_eqs.is_empty() || else_simple_eqs.len() == num_equations);

    if all_same_count {
        // Fast path: position-based matching (original behavior)
        let mut result = FlattenedEquations::default();
        let eq_context = ConditionalEquationContext {
            ctx,
            prefix,
            span,
            origin,
            imports: &ctx.current_imports,
            def_map,
        };
        for eq_idx in 0..num_equations {
            let flat_eq = create_conditional_equation_from_simple(
                &expanded_branches,
                &else_simple_eqs,
                eq_idx,
                &eq_context,
            )?;
            result.equations.push(flat_eq);
        }
        Ok(result)
    } else {
        // MLS §8.3.4: Branches with different equation counts require parameter conditions.
        // Try harder to evaluate conditions using all parameter values (not just structural).
        try_select_branch_for_mismatched_if(
            ctx,
            cond_blocks,
            else_block,
            prefix,
            span,
            origin,
            def_map,
        )
    }
}

/// Try to select a branch based on constant condition evaluation.
///
/// Returns Some(equations) if a branch can be selected at compile time,
/// None if conditions are non-constant.
///
/// Note: Parameter-aware evaluation is available (`try_eval_boolean_with_ctx`) but
/// Evaluates conditions using structural parameters only (MLS §18.3).
/// Parameters with annotation(Evaluate=true) or declared final are considered
/// structural and safe to evaluate at compile time for branch selection.
fn try_select_constant_branch(
    ctx: &Context,
    cond_blocks: &[ast::EquationBlock],
    else_block: &Option<Vec<ast::Equation>>,
    prefix: &ast::QualifiedName,
) -> Option<Vec<ast::Equation>> {
    // Try to evaluate conditions using structural parameters only
    // This respects Evaluate=true annotation per MLS §18.3
    for (i, block) in cond_blocks.iter().enumerate() {
        let is_struct = is_structural_expression(ctx, &block.cond, prefix);
        let eval_result = try_eval_structural_boolean(ctx, &block.cond, prefix);
        let _ = (i, is_struct); // suppress unused warnings
        match eval_result {
            Some(true) => {
                // Condition is true - select this branch
                return Some(block.eqs.clone());
            }
            Some(false) => {
                // Condition is false - continue to next branch
                continue;
            }
            None => {
                // Non-constant or non-structural condition - can't select at compile time
                return None;
            }
        }
    }

    // All conditions were constant false - use else branch if present
    // If no else branch, return empty equations (valid per MLS §8.3.4)
    Some(equations_from_optional_else(else_block))
}

fn equations_from_optional_else(else_block: &Option<Vec<ast::Equation>>) -> Vec<ast::Equation> {
    match else_block {
        Some(equations) => equations.clone(),
        None => Vec::new(),
    }
}

/// Fallback branch selection for if-equations with mismatched equation counts.
///
/// Per MLS §8.3.4, if-equations with different equation counts in branches
/// require parameter-dependent conditions. This function tries to evaluate
/// conditions using ALL parameter values (not just structural ones) to select
/// a branch at compile time. If evaluation fails, returns an error.
fn try_select_branch_for_mismatched_if(
    ctx: &Context,
    cond_blocks: &[ast::EquationBlock],
    else_block: &Option<Vec<ast::Equation>>,
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    origin: &rumoca_ir_flat::EquationOrigin,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<FlattenedEquations, FlattenError> {
    let scope = prefix.to_flat_string();
    for block in cond_blocks {
        if let Some(true) = try_eval_const_boolean_with_scope(&block.cond, ctx, &scope) {
            return flatten_equations_list(ctx, &block.eqs, prefix, span, origin, def_map);
        }
        if let Some(false) = try_eval_const_boolean_with_scope(&block.cond, ctx, &scope) {
            continue;
        }
        // Can't evaluate this condition at all
        let description =
            mismatched_if_equation_description(ctx, cond_blocks, else_block, prefix, span)?;
        return Err(FlattenError::unsupported_equation(description, span));
    }
    // All conditions false, use else branch
    let else_eqs = equations_from_optional_else(else_block);
    flatten_equations_list(ctx, &else_eqs, prefix, span, origin, def_map)
}

fn mismatched_if_equation_description(
    ctx: &Context,
    cond_blocks: &[ast::EquationBlock],
    else_block: &Option<Vec<ast::Equation>>,
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
) -> Result<String, FlattenError> {
    let mut counts = cond_blocks
        .iter()
        .map(|block| expand_to_simple_equations(ctx, &block.eqs, prefix, span).map(|eqs| eqs.len()))
        .collect::<Result<Vec<_>, _>>()?;
    let else_count = else_block
        .as_ref()
        .map(|eqs| expand_to_simple_equations(ctx, eqs, prefix, span).map(|eqs| eqs.len()))
        .transpose()?;
    if let Some(count) = else_count {
        counts.push(count);
    }
    let conditions = cond_blocks
        .iter()
        .map(|block| format_subscript_expr(&block.cond))
        .collect::<Vec<_>>()
        .join("; ");
    let references = cond_blocks
        .iter()
        .flat_map(|block| condition_reference_debug(ctx, &block.cond, prefix))
        .collect::<Vec<_>>()
        .join(", ");
    let enum_candidates = cond_blocks
        .iter()
        .flat_map(|block| condition_enum_candidates(ctx, &block.cond))
        .collect::<Vec<_>>()
        .join(", ");
    Ok(format!(
        "if-equation branches have mismatched equation counts in scope `{}`: conditions [{}], refs [{}], enum candidates [{}], branch counts {:?}, else count {}",
        prefix.to_flat_string(),
        conditions,
        references,
        enum_candidates,
        counts,
        else_count
            .map(|count| count.to_string())
            .unwrap_or_else(|| "none".to_string())
    ))
}

fn condition_enum_candidates(ctx: &Context, expr: &ast::Expression) -> Vec<String> {
    match expr {
        ast::Expression::ComponentReference(cr) => {
            let name = cr.to_string();
            let suffix = format!(".{name}");
            ctx.enum_parameter_values
                .iter()
                .filter(|(key, _)| key.as_str() == name || key.ends_with(&suffix))
                .take(12)
                .map(|(key, value)| format!("{key}={value}"))
                .collect()
        }
        ast::Expression::Binary { lhs, rhs, .. } => {
            let mut refs = condition_enum_candidates(ctx, lhs);
            refs.extend(condition_enum_candidates(ctx, rhs));
            refs
        }
        ast::Expression::Unary { rhs, .. } => condition_enum_candidates(ctx, rhs),
        ast::Expression::Parenthesized { inner, .. } => condition_enum_candidates(ctx, inner),
        ast::Expression::FunctionCall { args, .. } => args
            .iter()
            .flat_map(|arg| condition_enum_candidates(ctx, arg))
            .collect(),
        _ => Vec::new(),
    }
}

fn condition_reference_debug(
    ctx: &Context,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
) -> Vec<String> {
    match expr {
        ast::Expression::ComponentReference(cr) => {
            let name = cr.to_string();
            let enum_value =
                try_resolve_enum_value(Some(ctx), expr, prefix).unwrap_or_else(|| "-".to_string());
            let bool_value = try_eval_boolean_with_ctx_inner(expr, Some(ctx), prefix)
                .map(|value| value.to_string())
                .unwrap_or_else(|| "-".to_string());
            let int_value = lookup_parameter_in_scope(ctx, cr, prefix)
                .map(|value| value.to_string())
                .unwrap_or_else(|| "-".to_string());
            vec![format!(
                "{name}:int={int_value}:enum={enum_value}:bool={bool_value}"
            )]
        }
        ast::Expression::Binary { lhs, rhs, .. } => {
            let mut refs = condition_reference_debug(ctx, lhs, prefix);
            refs.extend(condition_reference_debug(ctx, rhs, prefix));
            refs
        }
        ast::Expression::Unary { rhs, .. } => condition_reference_debug(ctx, rhs, prefix),
        ast::Expression::Parenthesized { inner, .. } => {
            condition_reference_debug(ctx, inner, prefix)
        }
        ast::Expression::FunctionCall { args, .. } => args
            .iter()
            .flat_map(|arg| condition_reference_debug(ctx, arg, prefix))
            .collect(),
        _ => Vec::new(),
    }
}

/// Expand equations to simple equations (lhs = rhs form).
///
/// Expands for-equations and constant if-equations, returning a flat list
/// of simple equations that can be matched across branches.
fn expand_to_simple_equations(
    ctx: &Context,
    equations: &[ast::Equation],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
) -> Result<Vec<SimpleEquation>, FlattenError> {
    let mut result = Vec::new();

    for eq in equations {
        match eq {
            ast::Equation::Simple { lhs, rhs } => {
                let expanded = expand_array_assignment(lhs, rhs);
                result.extend(expanded);
            }

            ast::Equation::For { indices, equations } => {
                // Expand for-equation to simple equations
                let expanded = expand_for_to_simple(ctx, indices, equations, prefix, span)?;
                result.extend(expanded);
            }

            ast::Equation::If {
                cond_blocks,
                else_block,
            } => {
                // For nested if-equations, try constant condition evaluation first
                if let Some(selected) =
                    try_select_constant_branch(ctx, cond_blocks, else_block, prefix)
                {
                    let expanded = expand_to_simple_equations(ctx, &selected, prefix, span)?;
                    result.extend(expanded);
                } else {
                    // Non-constant nested if-equation - expand recursively
                    let nested =
                        expand_nested_if_to_simple(ctx, cond_blocks, else_block, prefix, span)?;
                    result.extend(nested);
                }
            }

            ast::Equation::Empty
            | ast::Equation::Connect { .. }
            | ast::Equation::Assert { .. }
            | ast::Equation::When(_)
            | ast::Equation::FunctionCall { .. } => {
                // Skip these - they don't contribute to regular flat equations:
                // - Connect: handled separately in connections module
                // - Assert: runtime checks, not equation system
                // - When: handled separately by flatten_when_equation
                // - FunctionCall: typically assert(), Modelica.Utilities.*, etc.
            }
        }
    }

    Ok(result)
}

/// Expand a simple equation with an array RHS into per-element equations.
///
/// For `x = {e1, e2, e3}` where x is a ast::ComponentReference, produces:
/// `x[1] = e1, x[2] = e2, x[3] = e3`
///
/// Handles nested arrays recursively for multi-dimensional cases.
/// Falls back to a single equation when the RHS is not an array.
fn expand_array_assignment(lhs: &ast::Expression, rhs: &ast::Expression) -> Vec<SimpleEquation> {
    let rhs_elements = match rhs {
        ast::Expression::Array { elements, .. } if !elements.is_empty() => elements,
        _ => {
            return vec![SimpleEquation {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }];
        }
    };
    expand_array_lhs_elements(lhs, rhs, rhs_elements)
}

/// Expand array assignment given the RHS elements extracted from an Array expression.
fn expand_array_lhs_elements(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    rhs_elements: &[ast::Expression],
) -> Vec<SimpleEquation> {
    match lhs {
        ast::Expression::ComponentReference(cr) => expand_array_cr(cr, rhs_elements),
        ast::Expression::Array {
            elements: lhs_elements,
            ..
        } => lhs_elements
            .iter()
            .zip(rhs_elements.iter())
            .flat_map(|(l, r)| expand_array_assignment(l, r))
            .collect(),
        _ => vec![SimpleEquation {
            lhs: lhs.clone(),
            rhs: rhs.clone(),
        }],
    }
}

/// Expand a ast::ComponentReference LHS with per-element subscripts for each RHS element.
fn expand_array_cr(
    cr: &ast::ComponentReference,
    rhs_elements: &[ast::Expression],
) -> Vec<SimpleEquation> {
    rhs_elements
        .iter()
        .enumerate()
        .flat_map(|(i, elem)| {
            let new_cr = add_subscript_to_component_ref(cr, (i as i64) + 1);
            let new_lhs = ast::Expression::ComponentReference(new_cr);
            expand_array_assignment(&new_lhs, elem)
        })
        .collect()
}

/// Add an integer subscript to the last part of a ast::ComponentReference.
///
/// `x` becomes `x[index]`, `x[1]` becomes `x[1, index]`.
fn add_subscript_to_component_ref(
    cr: &ast::ComponentReference,
    index: i64,
) -> ast::ComponentReference {
    let mut new_cr = cr.clone();
    if let Some(last) = new_cr.parts.last_mut() {
        let sub = ast::Subscript::Expression(ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: rumoca_core::Token {
                text: std::sync::Arc::from(index.to_string()),
                ..Default::default()
            },
            span: cr.span,
        });
        match &mut last.subs {
            Some(subs) => subs.push(sub),
            None => last.subs = Some(vec![sub]),
        }
    }
    new_cr
}

/// Expand a for-equation to simple equations.
fn expand_for_to_simple(
    ctx: &Context,
    indices: &[ast::ForIndex],
    equations: &[ast::Equation],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
) -> Result<Vec<SimpleEquation>, FlattenError> {
    if indices.is_empty() {
        return expand_to_simple_equations(ctx, equations, prefix, span);
    }

    let first_index = &indices[0];
    let remaining_indices = &indices[1..];

    let index_values = expand_range_indices(ctx, &first_index.range, prefix, span)?;
    let index_name = &first_index.ident.text;

    let mut result = Vec::new();
    for value in index_values {
        // Substitute index variable in all equations
        let substituted: Vec<ast::Equation> = equations
            .iter()
            .map(|eq| substitute_index_in_equation(eq, index_name, value))
            .collect();

        // Recursively expand remaining indices
        let expanded = expand_for_to_simple(ctx, remaining_indices, &substituted, prefix, span)?;
        result.extend(expanded);
    }

    Ok(result)
}
