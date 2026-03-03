//! Equation flattening for the flatten phase.
//!
//! This module converts instance equations to flat equations in
//! residual form (0 = residual).

use std::sync::Arc;

// Conditional tracing support (SPEC_0024)
use rumoca_eval_const::{EvalContext, Value};
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;
#[cfg(feature = "tracing")]
use tracing::{debug, warn};

use crate::boolean_eval::{
    is_structural_expression, try_eval_boolean_with_ctx_inner, try_eval_structural_boolean,
};
use crate::errors::FlattenError;
use crate::{Context, qualify_expression_imports_with_def_map};

mod conditional_and_eval;
mod connections_graph;
pub(crate) use conditional_and_eval::build_eval_context;
use conditional_and_eval::*;
pub(crate) use conditional_and_eval::{
    expand_range_indices, substitute_index_in_equation, substitute_index_in_expression,
};
use connections_graph::{extract_vcg_data_from_function_call, is_side_effect_only_function};

type ClassTree = ast::ClassTree;
type ComponentRefPart = ast::ComponentRefPart;
type ComponentReference = ast::ComponentReference;
type EquationBlock = ast::EquationBlock;
type ForIndex = ast::ForIndex;
type OpBinary = ast::OpBinary;
type QualifiedName = ast::QualifiedName;
type TerminalType = ast::TerminalType;
type Token = ast::Token;
#[cfg(test)]
type InstanceEquation = ast::InstanceEquation;
type AssertEquation = flat::AssertEquation;

/// Result of flattening equations, containing both regular equations and when-clauses.
/// This is needed because for/if-equations can contain when-equations inside them.
#[derive(Default)]
pub struct FlattenedEquations {
    /// Regular flat equations (continuous, discrete)
    pub equations: Vec<flat::Equation>,
    /// Preserved `for`-equation grouping metadata (MLS §8.3.3).
    pub for_equations: Vec<flat::ForEquation>,
    /// Assertion equations preserved from equation sections (MLS §8.3.7).
    pub assert_equations: Vec<flat::AssertEquation>,
    /// When-clauses extracted from nested when-equations
    pub when_clauses: Vec<flat::WhenClause>,
    /// Definite roots from Connections.root() calls (MLS §9.4.1).
    /// Stores the qualified path to the overconstrained record.
    pub definite_roots: Vec<String>,
    /// Branches from Connections.branch(a, b) calls (MLS §9.4).
    /// Each entry is (from_path, to_path) forming a required edge in the VCG.
    pub branches: Vec<(String, String)>,
    /// Potential roots from Connections.potentialRoot(a, priority) calls (MLS §9.4).
    /// Each entry is (path, priority) where lower priority means more likely to be root.
    pub potential_roots: Vec<(String, i64)>,
}

impl FlattenedEquations {
    /// Merge another flattened bundle into this one.
    ///
    /// Keeping merge logic centralized avoids accidentally dropping side-channel
    /// data when adding new flattened outputs.
    fn append(&mut self, mut other: FlattenedEquations) {
        self.equations.append(&mut other.equations);
        self.for_equations.append(&mut other.for_equations);
        self.assert_equations.append(&mut other.assert_equations);
        self.when_clauses.append(&mut other.when_clauses);
        self.definite_roots.append(&mut other.definite_roots);
        self.branches.append(&mut other.branches);
        self.potential_roots.append(&mut other.potential_roots);
    }

    fn is_empty(&self) -> bool {
        self.equations.is_empty()
            && self.for_equations.is_empty()
            && self.assert_equations.is_empty()
            && self.when_clauses.is_empty()
            && self.definite_roots.is_empty()
            && self.branches.is_empty()
            && self.potential_roots.is_empty()
    }
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
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
        } => token.text.to_string(),
        ast::Expression::ComponentReference(cr) => {
            // Simple variable reference (should have been substituted)
            cr.parts
                .iter()
                .map(|p| p.ident.text.to_string())
                .collect::<Vec<_>>()
                .join(".")
        }
        ast::Expression::Unary { op, rhs } => {
            format!("{}{}", op, format_subscript_expr(rhs))
        }
        ast::Expression::Binary { op, lhs, rhs } => {
            format!(
                "({} {} {})",
                format_subscript_expr(lhs),
                op,
                format_subscript_expr(rhs)
            )
        }
        ast::Expression::FunctionCall { comp, args } => {
            let func_name: String = comp
                .parts
                .iter()
                .map(|p| p.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");
            let arg_strs: Vec<String> = args.iter().map(format_subscript_expr).collect();
            format!("{}({})", func_name, arg_strs.join(", "))
        }
        ast::Expression::Range { start, step, end } => match step {
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

    // First try fully qualified (with alias resolution)
    let qualified = build_qualified_name(prefix, cr);
    if let Some(val) = ctx.get_integer_param(&qualified) {
        return Some(val);
    }

    // MLS §7.3: Replaceable type references like `Medium.nXi` map to instance
    // names like `medium.nXi` in the flat model. Try lowercasing the first part
    // of the component reference when it looks like a type name (starts uppercase).
    if is_type_ref && let Some(val) = try_lowercase_type_ref(ctx, cr, prefix) {
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
        let parent_qualified = build_qualified_name(&parent, cr);
        if let Some(val) = ctx.get_integer_param(&parent_qualified) {
            #[cfg(feature = "tracing")]
            debug!(
                original = %qualified,
                found = %parent_qualified,
                "parameter resolved in parent scope"
            );
            return Some(val);
        }
        // Also try lowercase type ref in parent scope
        if is_type_ref && let Some(val) = try_lowercase_type_ref(ctx, cr, &parent) {
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
    if let Some(val) = ctx.get_integer_param(&unqualified) {
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

    // MLS §7.3: Last resort for type references like `Medium.nXi` when the type alias
    // is declared inside a component class (not at root). Search for any parameter
    // with a matching lowercased suffix (e.g., find `source.medium.nXi` for `Medium.nXi`).
    if is_type_ref && let Some(val) = try_lowercase_type_ref_suffix(ctx, cr) {
        return Some(val);
    }
    if is_instance_ref && let Some(val) = try_uppercase_instance_ref_suffix(ctx, cr) {
        return Some(val);
    }

    // Final fallback for common medium size constants (nX, nXi, nC, nS):
    // infer from already-known array dimensions in the current scope chain.
    if cr.parts.len() == 1 {
        let simple_name = cr.parts[0].ident.text.as_ref();
        if let Some(val) = infer_size_constant_from_dims(ctx, simple_name, prefix) {
            return Some(val);
        }
    }

    None
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
    ctx.get_integer_param(&lowered_name)
}

/// Last-resort lookup for type references like `Medium.nXi` by suffix matching.
///
/// When a component class declares `replaceable package Medium = ...` but the
/// root model doesn't, `Medium.nXi` won't exist at root scope. However, instance
/// variables like `source.medium.nXi` will exist. This function finds the first
/// parameter whose name ends with the lowercased suffix (e.g., `.medium.nXi`).
fn try_lowercase_type_ref_suffix(ctx: &Context, cr: &ast::ComponentReference) -> Option<i64> {
    let first = cr.parts[0].ident.text.to_string();
    let lowered = first[..1].to_lowercase() + &first[1..];
    let mut suffix_parts = vec![lowered];
    suffix_parts.extend(cr.parts[1..].iter().map(format_component_ref_part));
    let suffix = format!(".{}", suffix_parts.join("."));
    lookup_unique_suffix_integer_param(ctx, &suffix)
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
    ctx.get_integer_param(&uppered_name)
}

/// Last-resort lookup for instance-qualified refs by uppercased suffix matching.
///
/// Example: `medium.nXi` -> suffix `.Medium.nXi`.
fn try_uppercase_instance_ref_suffix(ctx: &Context, cr: &ast::ComponentReference) -> Option<i64> {
    let first = cr.parts[0].ident.text.to_string();
    let uppered = first[..1].to_uppercase() + &first[1..];
    let mut suffix_parts = vec![uppered];
    suffix_parts.extend(cr.parts[1..].iter().map(format_component_ref_part));
    let suffix = format!(".{}", suffix_parts.join("."));
    lookup_unique_suffix_integer_param(ctx, &suffix)
}

/// Resolve an integer parameter by suffix only when all matches agree.
///
/// This constrains suffix matching to deterministic, non-ambiguous cases.
/// We require exactly one matching key to avoid cross-scope accidental matches.
fn lookup_unique_suffix_integer_param(ctx: &Context, suffix: &str) -> Option<i64> {
    let mut value: Option<i64> = None;
    let mut matches = 0usize;
    for (key, candidate) in &ctx.parameter_values {
        if !key.ends_with(suffix) {
            continue;
        }
        matches += 1;
        if matches > 1 {
            return None;
        }
        value = Some(*candidate);
    }
    value
}

/// Infer medium-style size constants from known array dimensions in scope.
///
/// Examples:
/// - `nX`  from `X[:]`
/// - `nXi` from `Xi[:]`
/// - `nC`  from `C[:]`
/// - `nS`  from `substanceNames[:]`
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
            if let Some(dims) = ctx.get_array_dims(&qualified)
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
pub fn flatten_equation_with_def_map(
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

            // Preserve simple equations as a single residual equation.
            // Array scalarization and counting are handled downstream.
            let residual = make_residual(&lhs, &rhs, prefix, &ctx.current_imports, def_map);
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
                for_equations: vec![],
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
            flatten_function_call_equation(comp, args, prefix, span, &ctx.current_imports, def_map)
        }

        ast::Equation::Assert {
            condition,
            message,
            level,
        } => {
            // MLS §8.3.7: preserve assert-equations for runtime checks in flat output.
            // They do not contribute to the DAE residual equation system.
            let imports = &ctx.current_imports;
            let assert_eq = flat::AssertEquation::new(
                qualify_expression_imports_with_def_map(condition, prefix, imports, def_map),
                qualify_expression_imports_with_def_map(message, prefix, imports, def_map),
                level.as_ref().map(|expr| {
                    qualify_expression_imports_with_def_map(expr, prefix, imports, def_map)
                }),
                span,
            );
            Ok(FlattenedEquations {
                equations: vec![],
                for_equations: vec![],
                assert_equations: vec![assert_eq],
                when_clauses: vec![],
                definite_roots: vec![],
                branches: vec![],
                potential_roots: vec![],
            })
        }
    }
}

fn flatten_function_call_equation(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    imports: &crate::qualify::ImportMap,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<FlattenedEquations, FlattenError> {
    if is_assert_function_call(comp) {
        return flatten_assert_function_call(args, prefix, span, imports, def_map);
    }
    Ok(extract_vcg_data_from_function_call(comp, args, prefix))
}

fn is_assert_function_call(comp: &ast::ComponentReference) -> bool {
    comp.parts
        .last()
        .map(|part| part.ident.text.as_ref() == "assert")
        .unwrap_or(false)
}

fn flatten_assert_function_call(
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    imports: &crate::qualify::ImportMap,
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

    let assert_eq = flat::AssertEquation::new(
        qualify_expression_imports_with_def_map(condition, prefix, imports, def_map),
        qualify_expression_imports_with_def_map(message, prefix, imports, def_map),
        level.map(|expr| qualify_expression_imports_with_def_map(expr, prefix, imports, def_map)),
        span,
    );

    Ok(FlattenedEquations {
        equations: vec![],
        for_equations: vec![],
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
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    prefix: &ast::QualifiedName,
    imports: &crate::qualify::ImportMap,
    def_map: Option<&crate::ResolveDefMap>,
) -> flat::Expression {
    // Create: lhs - rhs
    let residual = ast::Expression::Binary {
        op: ast::OpBinary::Sub(rumoca_ir_ast::Token::default()),
        lhs: Arc::new(lhs.clone()),
        rhs: Arc::new(rhs.clone()),
    };

    qualify_expression_imports_with_def_map(&residual, prefix, imports, def_map)
}

/// Expand array comprehensions in equation expressions when index ranges are structural.
///
/// This preserves current scalar-count and ToDAE expectations for equation residuals
/// while `ast::Expression::ArrayComprehension` support is rolled through downstream passes.
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
        } => expand_array_comprehension_expression(
            ctx,
            body,
            indices,
            filter.as_deref(),
            prefix,
            span,
        ),
        ast::Expression::Binary { op, lhs, rhs } => Ok(ast::Expression::Binary {
            op: op.clone(),
            lhs: Arc::new(expand_array_comprehensions_in_expression(
                ctx, lhs, prefix, span,
            )?),
            rhs: Arc::new(expand_array_comprehensions_in_expression(
                ctx, rhs, prefix, span,
            )?),
        }),
        ast::Expression::Unary { op, rhs } => Ok(ast::Expression::Unary {
            op: op.clone(),
            rhs: Arc::new(expand_array_comprehensions_in_expression(
                ctx, rhs, prefix, span,
            )?),
        }),
        ast::Expression::FunctionCall { comp, args } => Ok(ast::Expression::FunctionCall {
            comp: comp.clone(),
            args: expand_expression_list(ctx, args, prefix, span)?,
        }),
        ast::Expression::If {
            branches,
            else_branch,
        } => expand_if_expression(ctx, branches, else_branch, prefix, span),
        ast::Expression::Array {
            elements,
            is_matrix,
        } => Ok(ast::Expression::Array {
            elements: expand_expression_list(ctx, elements, prefix, span)?,
            is_matrix: *is_matrix,
        }),
        ast::Expression::Tuple { elements } => Ok(ast::Expression::Tuple {
            elements: expand_expression_list(ctx, elements, prefix, span)?,
        }),
        ast::Expression::Range { start, step, end } => Ok(ast::Expression::Range {
            start: Arc::new(expand_array_comprehensions_in_expression(
                ctx, start, prefix, span,
            )?),
            step: step
                .as_ref()
                .map(|s| expand_array_comprehensions_in_expression(ctx, s, prefix, span))
                .transpose()?
                .map(Arc::new),
            end: Arc::new(expand_array_comprehensions_in_expression(
                ctx, end, prefix, span,
            )?),
        }),
        ast::Expression::Parenthesized { inner } => Ok(ast::Expression::Parenthesized {
            inner: Arc::new(expand_array_comprehensions_in_expression(
                ctx, inner, prefix, span,
            )?),
        }),
        ast::Expression::ArrayIndex { base, subscripts } => Ok(ast::Expression::ArrayIndex {
            base: Arc::new(expand_array_comprehensions_in_expression(
                ctx, base, prefix, span,
            )?),
            subscripts: subscripts.clone(),
        }),
        ast::Expression::FieldAccess { base, field } => Ok(ast::Expression::FieldAccess {
            base: Arc::new(expand_array_comprehensions_in_expression(
                ctx, base, prefix, span,
            )?),
            field: field.clone(),
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

fn infer_expression_shape(
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
    ctx: &Context,
) -> ExpressionShape {
    match expr {
        ast::Expression::Terminal { .. } => ExpressionShape::Scalar,
        ast::Expression::ComponentReference(cr) => infer_component_ref_shape(cr, prefix, ctx),
        ast::Expression::Unary { rhs, .. } | ast::Expression::Parenthesized { inner: rhs } => {
            infer_expression_shape(rhs, prefix, ctx)
        }
        ast::Expression::Binary { op, lhs, rhs } => {
            let lhs_shape = infer_expression_shape(lhs, prefix, ctx);
            let rhs_shape = infer_expression_shape(rhs, prefix, ctx);
            match op {
                ast::OpBinary::Add(_)
                | ast::OpBinary::Sub(_)
                | ast::OpBinary::AddElem(_)
                | ast::OpBinary::SubElem(_) => combine_additive_shapes(lhs_shape, rhs_shape),
                ast::OpBinary::Mul(_) => combine_mul_shapes(lhs_shape, rhs_shape),
                ast::OpBinary::MulElem(_) => combine_elementwise_shapes(lhs_shape, rhs_shape),
                ast::OpBinary::Div(_) => match (lhs_shape, rhs_shape) {
                    (ExpressionShape::Scalar, ExpressionShape::Scalar) => ExpressionShape::Scalar,
                    (ExpressionShape::Vector(n), ExpressionShape::Scalar) => {
                        ExpressionShape::Vector(n)
                    }
                    (ExpressionShape::Matrix(r, c), ExpressionShape::Scalar) => {
                        ExpressionShape::Matrix(r, c)
                    }
                    _ => ExpressionShape::Other,
                },
                ast::OpBinary::DivElem(_) | ast::OpBinary::ExpElem(_) => {
                    combine_elementwise_shapes(lhs_shape, rhs_shape)
                }
                ast::OpBinary::Exp(_) => match (lhs_shape, rhs_shape) {
                    (ExpressionShape::Scalar, ExpressionShape::Scalar) => ExpressionShape::Scalar,
                    (ExpressionShape::Vector(n), ExpressionShape::Scalar) => {
                        ExpressionShape::Vector(n)
                    }
                    (ExpressionShape::Matrix(r, c), ExpressionShape::Scalar) => {
                        ExpressionShape::Matrix(r, c)
                    }
                    _ => ExpressionShape::Other,
                },
                ast::OpBinary::Eq(_)
                | ast::OpBinary::Neq(_)
                | ast::OpBinary::Lt(_)
                | ast::OpBinary::Le(_)
                | ast::OpBinary::Gt(_)
                | ast::OpBinary::Ge(_)
                | ast::OpBinary::And(_)
                | ast::OpBinary::Or(_)
                | ast::OpBinary::Assign(_)
                | ast::OpBinary::Empty => ExpressionShape::Scalar,
            }
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
        | ast::Expression::Empty => ExpressionShape::Other,
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
    if let Some((_, dims)) = find_array_refs_needing_expansion(lhs, prefix, ctx).first() {
        return dims_scalar_size(dims);
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
) -> Vec<(String, Vec<i64>)> {
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
    results: &mut Vec<(String, Vec<i64>)>,
) {
    let qualified = build_qualified_name(prefix, cr);
    let parts = crate::path_utils::parse_path_with_indices(&qualified);
    let prefix_part_count = prefix.parts.len();

    for i in 1..=parts.len() {
        let path = parts[..i].join(".");
        let Some(dims) = ctx.get_array_dimensions(&path) else {
            continue;
        };

        if dims.is_empty() {
            continue;
        }

        // Check if this path segment already has subscripts (already indexed).
        // For prefix parts (i <= prefix_part_count): check both the subscripts vector
        // and embedded brackets in the name string (from array component expansion,
        // which embeds subscripts like "inductor[1]" in the name).
        // For CR parts (i > prefix_part_count): check the component reference subscripts.
        let already_subscripted = if i <= prefix_part_count {
            !prefix.parts[i - 1].1.is_empty() || prefix.parts[i - 1].0.contains('[')
        } else {
            has_subscript_at_index(cr, i - prefix_part_count - 1)
        };

        if already_subscripted {
            continue;
        }

        // Add if not already present
        if !results.iter().any(|(p, _)| p == &path) {
            results.push((path, dims.clone()));
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
    results: &mut Vec<(String, Vec<i64>)>,
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

        ast::Expression::Parenthesized { inner } => {
            find_array_refs_recursive(inner, prefix, ctx, results);
        }

        ast::Expression::If {
            branches,
            else_branch,
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

        ast::Expression::Range { start, step, end } => {
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
        | ast::Expression::Empty
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
    result.for_equations.push(flat::ForEquation {
        index_names: indices
            .iter()
            .map(|idx| idx.ident.text.to_string())
            .collect(),
        first_equation_index: 0,
        iterations,
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
    iterations: &mut Vec<flat::ForEquationIteration>,
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
        iterations.push(flat::ForEquationIteration {
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

/// A simple equation extracted from complex equation structures.
#[derive(Clone)]
struct SimpleEquation {
    lhs: ast::Expression,
    rhs: ast::Expression,
}

/// Expand an if-equation by converting to conditional expressions.
///
/// MLS §8.3.4: "An if-equation creates a conditional set of equations."
///
/// Converts:
/// ```text
/// if cond then
///   x = a;
/// else
///   x = b;
/// end if;
/// ```
/// To:
/// ```text
/// x = if cond then a else b;
/// ```
///
/// Supports:
/// - Constant condition evaluation (compile-time branch selection)
/// - For-equations in branches (expanded before matching)
/// - Nested if-equations with constant conditions
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
    Some(else_block.clone().unwrap_or_default())
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
    for block in cond_blocks {
        if let Some(true) = try_eval_boolean_with_ctx_inner(&block.cond, Some(ctx), prefix) {
            return flatten_equations_list(ctx, &block.eqs, prefix, span, origin, def_map);
        }
        if let Some(false) = try_eval_boolean_with_ctx_inner(&block.cond, Some(ctx), prefix) {
            continue;
        }
        // Can't evaluate this condition at all
        return Err(FlattenError::unsupported_equation(
            "if-equation branches have mismatched equation counts",
            span,
        ));
    }
    // All conditions false, use else branch
    let else_eqs = else_block.clone().unwrap_or_default();
    flatten_equations_list(ctx, &else_eqs, prefix, span, origin, def_map)
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
            token: ast::Token {
                text: std::sync::Arc::from(index.to_string()),
                ..Default::default()
            },
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

    let index_values = match expand_range_indices(ctx, &first_index.range, prefix, span) {
        Ok(values) => values,
        Err(_) => return Ok(Vec::new()), // Skip unresolvable for-equations
    };
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
