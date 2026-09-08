//! Boolean expression evaluation for the flatten phase.
//!
//! This module handles evaluation of boolean expressions during flattening:
//! - Literal boolean evaluation
//! - Parameter-based boolean evaluation
//! - Structural parameter checking (MLS §18.3)
//! - Enum value comparison

use rumoca_ir_ast as ast;

use crate::Context;
use crate::equations::build_qualified_name;

struct FlattenScalarAdapter<'a> {
    ctx: Option<&'a Context>,
    structural_only: bool,
    vcg_queries: VcgQueryMode<'a>,
}

enum VcgQueryMode<'a> {
    Disabled,
    Checked {
        operators: &'a ast::ConnectionOperatorCatalog,
        missing: std::cell::RefCell<Option<MissingVcgProof>>,
    },
}

#[derive(Debug)]
struct MissingVcgProof {
    query: VcgBooleanQuery,
    path: Option<String>,
    reason: &'static str,
    span: rumoca_core::Span,
}

#[derive(Clone, Copy, Debug)]
enum VcgBooleanQuery {
    IsRoot,
    Rooted,
}

impl VcgBooleanQuery {
    fn function_name(self) -> &'static str {
        match self {
            Self::IsRoot => "Connections.isRoot",
            Self::Rooted => "Connections.rooted",
        }
    }
}

impl FlattenScalarAdapter<'_> {
    fn without_vcg_queries(
        ctx: Option<&Context>,
        structural_only: bool,
    ) -> FlattenScalarAdapter<'_> {
        FlattenScalarAdapter {
            ctx,
            structural_only,
            vcg_queries: VcgQueryMode::Disabled,
        }
    }

    fn with_checked_vcg_queries<'a>(
        ctx: Option<&'a Context>,
        structural_only: bool,
        operators: &'a ast::ConnectionOperatorCatalog,
    ) -> FlattenScalarAdapter<'a> {
        FlattenScalarAdapter {
            ctx,
            structural_only,
            vcg_queries: VcgQueryMode::Checked {
                operators,
                missing: std::cell::RefCell::new(None),
            },
        }
    }

    fn finish_boolean(&self, value: Option<bool>) -> Result<Option<bool>, crate::FlattenError> {
        let VcgQueryMode::Checked { missing, .. } = &self.vcg_queries else {
            return Ok(value);
        };
        let Some(missing) = missing.borrow_mut().take() else {
            return Ok(value);
        };
        let target = missing
            .path
            .as_deref()
            .map_or_else(|| "<invalid argument>".to_string(), str::to_string);
        Err(crate::FlattenError::invalid_connection_evidence(
            format!(
                "{}({target}) has no exact finalized VCG proof: {}",
                missing.query.function_name(),
                missing.reason
            ),
            missing.span,
        ))
    }

    fn lookup_vcg_boolean(
        &self,
        query: VcgBooleanQuery,
        args: &[ast::Expression],
        prefix: &ast::QualifiedName,
        span: rumoca_core::Span,
    ) -> Option<bool> {
        let VcgQueryMode::Checked {
            missing: failure, ..
        } = &self.vcg_queries
        else {
            return None;
        };
        let result = lookup_vcg_boolean(self.ctx, query, args, prefix, span);
        match result {
            Ok(value) => Some(value),
            Err(missing) => {
                *failure.borrow_mut() = Some(missing);
                None
            }
        }
    }

    /// True when any operand names a parameter this fold must not read.
    fn refuses_non_structural(
        &self,
        operands: [&ast::Expression; 2],
        prefix: &ast::QualifiedName,
    ) -> bool {
        let Some(ctx) = self.ctx else {
            return false;
        };
        operands.into_iter().any(|operand| {
            ast::expression_component_path(operand).is_some_and(|path| {
                scoped_set_contains(&ctx.non_structural_params, &path.to_flat_string(), prefix)
            })
        })
    }
}

impl rumoca_eval_ast::ast_scalar::AstScalarContext for FlattenScalarAdapter<'_> {
    fn lookup_integer(&self, expr: &ast::Expression, scope: &str, _depth: usize) -> Option<i64> {
        let ctx = self.ctx?;
        let name = ast::expression_component_path(expr)?.to_flat_string();
        let prefix = ast::QualifiedName::from_dotted(scope);
        if self.structural_only && scoped_set_contains(&ctx.non_structural_params, &name, &prefix) {
            return None;
        }
        scoped_lookup_integer_param(ctx, &name, &prefix)
    }

    fn lookup_real(&self, expr: &ast::Expression, scope: &str, _depth: usize) -> Option<f64> {
        let ctx = self.ctx?;
        let name = ast::expression_component_path(expr)?.to_flat_string();
        let prefix = ast::QualifiedName::from_dotted(scope);
        if self.structural_only && scoped_set_contains(&ctx.non_structural_params, &name, &prefix) {
            return None;
        }
        scoped_lookup_real_param(ctx, &name, &prefix)
            .or_else(|| scoped_lookup_integer_param(ctx, &name, &prefix).map(|value| value as f64))
    }

    fn lookup_boolean(&self, expr: &ast::Expression, scope: &str, _depth: usize) -> Option<bool> {
        let ctx = self.ctx?;
        let name = ast::expression_component_path(expr)?.to_flat_string();
        let prefix = ast::QualifiedName::from_dotted(scope);
        if self.structural_only && scoped_set_contains(&ctx.non_structural_params, &name, &prefix) {
            return None;
        }
        scoped_lookup_map(&ctx.boolean_parameter_values, &name, &prefix)
    }

    fn call_integer(
        &self,
        function: &ast::ComponentReference,
        args: &[ast::Expression],
        scope: &str,
        _depth: usize,
        _span: rumoca_core::Span,
    ) -> Option<i64> {
        let ctx = self.ctx?;
        if function.to_string() == "cardinality" {
            return lookup_cardinality(Some(ctx), args, &ast::QualifiedName::from_dotted(scope));
        }
        if self.structural_only {
            return None;
        }
        crate::eval_const_integer_function_with_scope(function, args, ctx, scope)
    }

    fn call_boolean(
        &self,
        function: &ast::ComponentReference,
        args: &[ast::Expression],
        scope: &str,
        _depth: usize,
        span: rumoca_core::Span,
    ) -> Option<bool> {
        let prefix = ast::QualifiedName::from_dotted(scope);
        let VcgQueryMode::Checked { operators, .. } = &self.vcg_queries else {
            return None;
        };
        match function
            .target_def_id()
            .and_then(|declaration| operators.role(declaration))
        {
            Some(rumoca_core::ConnectionGraphOperatorRole::IsRoot) => {
                self.lookup_vcg_boolean(VcgBooleanQuery::IsRoot, args, &prefix, span)
            }
            Some(rumoca_core::ConnectionGraphOperatorRole::Rooted) => {
                self.lookup_vcg_boolean(VcgBooleanQuery::Rooted, args, &prefix, span)
            }
            Some(
                rumoca_core::ConnectionGraphOperatorRole::Branch
                | rumoca_core::ConnectionGraphOperatorRole::Root
                | rumoca_core::ConnectionGraphOperatorRole::PotentialRoot,
            )
            | None => None,
        }
    }

    fn call_real(
        &self,
        function: &ast::ComponentReference,
        args: &[ast::Expression],
        scope: &str,
        _depth: usize,
        _span: rumoca_core::Span,
    ) -> Option<f64> {
        if self.structural_only {
            return None;
        }
        crate::eval_const_real_function_with_scope(function, args, self.ctx?, scope)
    }

    fn enum_equal(
        &self,
        lhs: &ast::Expression,
        rhs: &ast::Expression,
        scope: &str,
        _depth: usize,
    ) -> Option<bool> {
        let prefix = ast::QualifiedName::from_dotted(scope);
        // Same refusal as the scalar lookups: under `structural_only` a
        // non-structural parameter must not decide a structural fold, or a
        // conditional that has to survive to runtime is eliminated here.
        if self.structural_only && self.refuses_non_structural([lhs, rhs], &prefix) {
            return None;
        }
        let lhs = try_resolve_enum_identity(self.ctx, lhs, &prefix)?;
        let rhs = try_resolve_enum_identity(self.ctx, rhs, &prefix)?;
        Some(lhs.declaration() == rhs.declaration() && lhs.ordinal() == rhs.ordinal())
    }

    fn coerce_integral_real(&self, value: f64, _span: rumoca_core::Span) -> Option<i64> {
        (value >= i64::MIN as f64 && value < -(i64::MIN as f64)).then_some(value as i64)
    }

    fn integer_binary(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: i64,
        rhs: i64,
        _span: rumoca_core::Span,
    ) -> Option<i64> {
        rumoca_core::eval_ast_integer_binary(op, lhs, rhs)
    }
}

pub(crate) fn try_eval_integer_with_scope(
    expr: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<i64> {
    rumoca_eval_ast::ast_scalar::eval_integer(
        expr,
        &FlattenScalarAdapter::without_vcg_queries(Some(ctx), false),
        scope,
        0,
    )
}

pub(crate) fn try_eval_real_with_scope(
    expr: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<f64> {
    rumoca_eval_ast::ast_scalar::eval_real(
        expr,
        &FlattenScalarAdapter::without_vcg_queries(Some(ctx), false),
        scope,
        0,
    )
}

pub(crate) fn try_eval_boolean_with_scope(
    expr: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<bool> {
    rumoca_eval_ast::ast_scalar::eval_boolean(
        expr,
        &FlattenScalarAdapter::without_vcg_queries(Some(ctx), false),
        scope,
        0,
    )
}

/// Try to evaluate an expression to a constant boolean for structural branch selection.
///
/// Per MLS §18.3, compile-time branch selection is only safe for structural
/// quantities (e.g., parameters marked `Evaluate=true`, `final` parameters, and
/// constants). Non-structural parameters must not be folded here.
///
/// Check if an expression only references structural parameters (Evaluate=true or final).
///
/// Returns true if:
/// - The expression is a literal (no variable references)
/// - All variable references are to structural parameters
///
/// This is used to determine if an expression is safe for compile-time evaluation
/// per MLS §18.3.
pub(crate) fn is_structural_expression(
    ctx: &Context,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
    operators: &ast::ConnectionOperatorCatalog,
) -> bool {
    match expr {
        // Literals are always safe
        ast::Expression::Terminal { .. } => true,

        // Check if component reference is a parameter with a known value
        ast::Expression::ComponentReference(cr) => {
            let cref_name = cr.to_string();

            // Parameters explicitly declared with `fixed=false` are
            // non-structural and must not be folded.
            if scoped_set_contains(&ctx.non_structural_params, &cref_name, prefix) {
                return false;
            }

            // Check if it's a structural parameter (Evaluate=true or final)
            if scoped_set_contains(&ctx.structural_params, &cref_name, prefix) {
                return true;
            }

            // Compile-time constants injected from package/class scopes are
            // structural by definition.
            if scoped_lookup_map(&ctx.constant_values, &cref_name, prefix).is_some() {
                return true;
            }

            // Fallback for constants/parameters injected from class/package scopes
            // that are tracked in scalar lookup maps.
            if scoped_lookup_integer_param(ctx, &cref_name, prefix).is_some()
                || scoped_lookup_map(&ctx.boolean_parameter_values, &cref_name, prefix).is_some()
                || scoped_lookup_map(&ctx.enum_parameter_values, &cref_name, prefix).is_some()
            {
                return true;
            }

            // Check if it's likely an enumeration literal (not a known parameter)
            // Enumeration literals like Types.FilterType.LowPass are constants
            // They have multiple parts and don't exist as parameters
            if cr.parts.len() > 1 {
                // If it has multiple parts and isn't a known parameter,
                // treat as constant (likely an enumeration literal)
                return true;
            }

            false
        }

        // Recursively check sub-expressions
        ast::Expression::Unary { rhs, .. } => is_structural_expression(ctx, rhs, prefix, operators),

        ast::Expression::Parenthesized { inner, .. } => {
            is_structural_expression(ctx, inner, prefix, operators)
        }

        ast::Expression::Binary { lhs, rhs, .. } => {
            is_structural_expression(ctx, lhs, prefix, operators)
                && is_structural_expression(ctx, rhs, prefix, operators)
        }

        // Connection graph functions and cardinality are structural (MLS §9.4, §3.7.2.3)
        // They determine connectivity which is known at compile time
        ast::Expression::FunctionCall { comp, .. } => {
            comp.target_def_id()
                .and_then(|declaration| operators.role(declaration))
                .is_some()
                || comp.to_string() == "cardinality"
        }

        // Other expressions are not safe (other function calls, arrays, etc.)
        _ => false,
    }
}

/// Try to evaluate a boolean expression, but only if it uses structural parameters.
///
/// This is the safe version that respects the Evaluate=true annotation (MLS §18.3).
/// Lookup of a non-structural value refuses to fold at the point where evaluation
/// reaches it. This preserves Modelica short-circuit semantics: `false and p`
/// does not read a non-structural `p`, while `true and p` remains unknown.
///
/// Returns Some(value) if:
/// - ast::Expression is a literal
/// - ast::Expression only uses structural parameters and can be evaluated
///
/// Returns None if:
/// - ast::Expression uses non-structural parameters
/// - ast::Expression cannot be evaluated
pub(crate) fn try_eval_structural_boolean(
    ctx: &Context,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
    operators: &ast::ConnectionOperatorCatalog,
) -> Result<Option<bool>, crate::FlattenError> {
    let adapter = FlattenScalarAdapter::with_checked_vcg_queries(Some(ctx), true, operators);
    let value =
        rumoca_eval_ast::ast_scalar::eval_boolean(expr, &adapter, &prefix.to_flat_string(), 0);
    adapter.finish_boolean(value)
}

/// Inner implementation for boolean evaluation.
pub(crate) fn try_eval_boolean_with_ctx_inner(
    expr: &ast::Expression,
    ctx: Option<&Context>,
    prefix: &ast::QualifiedName,
    operators: &ast::ConnectionOperatorCatalog,
) -> Result<Option<bool>, crate::FlattenError> {
    let adapter = FlattenScalarAdapter::with_checked_vcg_queries(ctx, false, operators);
    let value =
        rumoca_eval_ast::ast_scalar::eval_boolean(expr, &adapter, &prefix.to_flat_string(), 0);
    adapter.finish_boolean(value)
}

pub(crate) fn try_resolve_enum_display(
    ctx: Option<&Context>,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
) -> Option<String> {
    try_resolve_enum_identity(ctx, expr, prefix).map(|value| resolved_enum_display_name(&value))
}

fn try_resolve_enum_identity(
    ctx: Option<&Context>,
    expr: &ast::Expression,
    _prefix: &ast::QualifiedName,
) -> Option<rumoca_eval_flat::constant::ResolvedEnumValue> {
    match expr {
        ast::Expression::ComponentReference(cr) => {
            let ctx = ctx?;
            if let Some((instance_id, root_def_id)) =
                ctx.current_class_instance_id.zip(cr.root_def_id())
            {
                let identity = rumoca_eval_flat::constant::ResolvedOccurrenceKey {
                    instance_id,
                    root_def_id,
                };
                if let Some(rumoca_eval_flat::constant::Value::ResolvedEnum(value)) =
                    ctx.parameter_values_by_identity.get(&identity)
                {
                    return Some(value.clone());
                }
            }

            let literal = cr.parts.last()?.ident.text.as_ref();
            let mut matches = cr.parts.iter().rev().skip(1).filter_map(|part| {
                ctx.resolved_enum_catalog
                    .get(part.def_id?, literal)
                    .cloned()
            });
            let selected = matches.next()?;
            if matches.next().is_none() {
                return Some(selected);
            }
            None
        }
        _ => None,
    }
}

pub(crate) fn resolved_enum_display_name(
    value: &rumoca_eval_flat::constant::ResolvedEnumValue,
) -> String {
    if value.display_type().is_empty() {
        value.literal().to_string()
    } else {
        format!("{}.{}", value.display_type(), value.literal())
    }
}

fn scoped_lookup_real_param(ctx: &Context, name: &str, prefix: &ast::QualifiedName) -> Option<f64> {
    let name_path = rumoca_core::ComponentPath::from_flat_path(name);
    let scope_path = prefix.to_component_path();
    for candidate in rumoca_core::scoped_component_path_candidates(&name_path, &scope_path) {
        if let Some(value) = lookup_real_exact_or_unindexed(ctx, &candidate) {
            return Some(value);
        }
    }
    lookup_real_exact_or_unindexed(ctx, name)
}

fn lookup_real_exact_or_unindexed(ctx: &Context, key: &str) -> Option<f64> {
    if let Some(value) = ctx.real_parameter_values.get(key).copied() {
        return Some(value);
    }
    for candidate in crate::path_utils::unindexed_lookup_variants(key) {
        if let Some(value) = ctx.real_parameter_values.get(&candidate).copied() {
            return Some(value);
        }
    }
    None
}

/// Evaluate an integer expression for comparison through the shared AST scalar
/// interpreter, retaining flatten's scoped lookup and cardinality policy.
pub(crate) fn try_eval_integer_for_comparison(
    ctx: Option<&Context>,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
) -> Option<i64> {
    rumoca_eval_ast::ast_scalar::eval_integer(
        expr,
        &FlattenScalarAdapter::without_vcg_queries(ctx, false),
        &prefix.to_flat_string(),
        0,
    )
}

/// Look up `cardinality(c)` in the pre-computed cardinality counts (MLS §3.7.2.3).
///
/// Qualifies the connector argument using the current prefix and looks up the count
/// in `ctx.cardinality_counts`. Returns 0 for connectors with no connections.
fn lookup_cardinality(
    ctx: Option<&Context>,
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
) -> Option<i64> {
    let ctx = ctx?;
    let path = extract_vcg_arg_path(args, prefix)?;
    // MLS §9.4.2: cardinality() returns 0 for unconnected connectors; missing
    // map entry means the connector has no connections recorded, which is 0.
    Some(*ctx.cardinality_counts.get(&path).unwrap_or(&0))
}

fn scoped_lookup_map<T: Clone>(
    map: &rustc_hash::FxHashMap<String, T>,
    name: &str,
    prefix: &ast::QualifiedName,
) -> Option<T> {
    let name_path = rumoca_core::ComponentPath::from_flat_path(name);
    let scope_path = prefix.to_component_path();
    for candidate in rumoca_core::scoped_component_path_candidates(&name_path, &scope_path) {
        if let Some(value) = lookup_map_exact_or_unindexed(map, &candidate) {
            return Some(value.clone());
        }
    }
    lookup_map_exact_or_unindexed(map, name).cloned()
}

fn scoped_lookup_integer_param(
    ctx: &Context,
    name: &str,
    prefix: &ast::QualifiedName,
) -> Option<i64> {
    let name_path = rumoca_core::ComponentPath::from_flat_path(name);
    let scope_path = prefix.to_component_path();
    for candidate in rumoca_core::scoped_component_path_candidates(&name_path, &scope_path) {
        if let Some(value) = lookup_integer_exact_or_unindexed(ctx, &candidate) {
            return Some(value);
        }
    }
    lookup_integer_exact_or_unindexed(ctx, name)
}

fn lookup_integer_exact_or_unindexed(ctx: &Context, key: &str) -> Option<i64> {
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

fn lookup_map_exact_or_unindexed<'a, T>(
    map: &'a rustc_hash::FxHashMap<String, T>,
    key: &str,
) -> Option<&'a T> {
    if let Some(value) = map.get(key) {
        return Some(value);
    }
    for candidate in crate::path_utils::unindexed_lookup_variants(key) {
        if let Some(value) = map.get(&candidate) {
            return Some(value);
        }
    }
    None
}

fn scoped_set_contains(
    set: &std::collections::HashSet<String>,
    name: &str,
    prefix: &ast::QualifiedName,
) -> bool {
    let name_path = rumoca_core::ComponentPath::from_flat_path(name);
    let scope_path = prefix.to_component_path();
    for candidate in rumoca_core::scoped_component_path_candidates(&name_path, &scope_path) {
        if set_contains_exact_or_unindexed(set, &candidate) {
            return true;
        }
    }
    set_contains_exact_or_unindexed(set, name)
}

fn set_contains_exact_or_unindexed(set: &std::collections::HashSet<String>, key: &str) -> bool {
    if set.contains(key) {
        return true;
    }
    crate::path_utils::unindexed_lookup_variants(key)
        .iter()
        .any(|candidate| set.contains(candidate))
}

/// Look up one graph query in the exact finalized VCG maps (MLS §9.4).
fn lookup_vcg_boolean(
    ctx: Option<&Context>,
    query: VcgBooleanQuery,
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
) -> Result<bool, MissingVcgProof> {
    let Some(ctx) = ctx else {
        return Err(MissingVcgProof {
            query,
            path: None,
            reason: "the flatten context is absent",
            span,
        });
    };
    let Some(path) = extract_vcg_arg_path(args, prefix) else {
        return Err(MissingVcgProof {
            query,
            path: None,
            reason: "the graph query argument is not a component reference",
            span,
        });
    };
    let map = match query {
        VcgBooleanQuery::IsRoot => &ctx.vcg_is_root,
        VcgBooleanQuery::Rooted => &ctx.vcg_rooted,
    };
    map.get(&path).copied().ok_or(MissingVcgProof {
        query,
        path: Some(path),
        reason: "the graph query target is absent from the finalized VCG catalog",
        span,
    })
}

/// Extract the qualified path from the first argument of a VCG function call.
///
/// `Connections.isRoot(frame_a.R)` with prefix "body" → "body.frame_a.R"
fn extract_vcg_arg_path(args: &[ast::Expression], prefix: &ast::QualifiedName) -> Option<String> {
    match args.first()? {
        ast::Expression::ComponentReference(cr) => Some(build_qualified_name(prefix, cr)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::sync::Arc;

    use super::*;
    use rumoca_ir_ast as ast;
    use rustc_hash::FxHashMap;

    fn prefix_with_subscript_dot() -> ast::QualifiedName {
        let mut prefix = ast::QualifiedName::new();
        prefix.push("pkg".to_string(), vec![]);
        prefix.push("arr[data.medium]".to_string(), vec![]);
        prefix
    }

    #[test]
    fn scoped_lookup_map_ignores_dot_inside_subscript_scope() {
        let mut map = FxHashMap::default();
        map.insert("pkg.arr[data.value".to_string(), 99);
        map.insert("pkg.value".to_string(), 7);

        let resolved = scoped_lookup_map(&map, "value", &prefix_with_subscript_dot());
        assert_eq!(resolved, Some(7));
    }

    #[test]
    fn scoped_lookup_map_uses_unindexed_array_element_scope() {
        let mut map = FxHashMap::default();
        map.insert("adaptor.filter.transferFunction.nx".to_string(), 1);

        let prefix = ast::QualifiedName::from_dotted("adaptor.filter[1].transferFunction[1]");
        let resolved = scoped_lookup_map(&map, "nx", &prefix);
        assert_eq!(resolved, Some(1));
    }

    #[test]
    fn scoped_lookup_map_prefers_indexed_scope_override() {
        let mut map = FxHashMap::default();
        map.insert("adaptor.filter.transferFunction.nx".to_string(), 1);
        map.insert("adaptor.filter[1].transferFunction[1].nx".to_string(), 0);

        let prefix = ast::QualifiedName::from_dotted("adaptor.filter[1].transferFunction[1]");
        let resolved = scoped_lookup_map(&map, "nx", &prefix);
        assert_eq!(resolved, Some(0));
    }

    #[test]
    fn scoped_set_contains_ignores_dot_inside_subscript_scope() {
        let set = HashSet::from(["pkg.arr[data.flag".to_string()]);
        assert!(
            !scoped_set_contains(&set, "flag", &prefix_with_subscript_dot()),
            "dot inside bracketed subscripts must not create a valid parent scope"
        );
    }

    fn token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: Arc::from(text.to_string()),
            ..rumoca_core::Token::default()
        }
    }

    fn comp_ref(path: &str) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: crate::path_utils::segments(path)
                .into_iter()
                .enumerate()
                .map(|(index, part)| ast::ComponentRefPart {
                    ident: token(part),
                    subs: None,
                    def_id: Some(rumoca_core::DefId::new(11_001 + index as u32)),
                })
                .collect(),
            span: rumoca_core::Span::DUMMY,
            qualified_display_name: None,
        }
    }

    fn cref_expr(path: &str) -> ast::Expression {
        ast::Expression::ComponentReference(comp_ref(path))
    }

    fn cref_expr_with_target(path: &str, target: rumoca_core::DefId) -> ast::Expression {
        let mut reference = comp_ref(path);
        reference
            .parts
            .last_mut()
            .expect("fixture reference has one path segment")
            .def_id = Some(target);
        ast::Expression::ComponentReference(reference)
    }

    fn eq_expr(lhs: ast::Expression, rhs: ast::Expression) -> ast::Expression {
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Eq,
            lhs: Arc::new(lhs),
            rhs: Arc::new(rhs),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn int_expr(value: i64) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: token(&value.to_string()),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn bool_expr(value: bool) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token: token(if value { "true" } else { "false" }),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn vcg_query_with_target(
        function: &str,
        target: rumoca_core::DefId,
        argument: ast::Expression,
    ) -> ast::Expression {
        let mut comp = comp_ref(function);
        comp.parts
            .last_mut()
            .expect("fixture function reference is nonempty")
            .def_id = Some(target);
        ast::Expression::FunctionCall {
            comp,
            args: vec![argument],
            is_partial_application: false,
            span: rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name("missing_vcg_query.mo"),
                12,
                35,
            ),
        }
    }

    #[test]
    fn missing_is_root_catalog_entry_is_a_typed_refusal() {
        let ctx = Context::new();
        let operators = crate::test_support::connection_operators();
        let expression = vcg_query_with_target(
            "Connections.isRoot",
            operators.declaration(rumoca_core::ConnectionGraphOperatorRole::IsRoot),
            cref_expr("frame.R"),
        );

        let error = try_eval_boolean_with_ctx_inner(
            &expression,
            Some(&ctx),
            &ast::QualifiedName::new(),
            &operators,
        )
        .expect_err("a graph query cannot invent a result for an absent VCG node");

        let crate::FlattenError::InvalidConnectionEvidence { description, .. } = error else {
            panic!("expected typed missing-VCG evidence, got {error:?}");
        };
        assert!(description.contains("Connections.isRoot(frame.R)"));
        assert!(description.contains("absent from the finalized VCG catalog"));
    }

    #[test]
    fn rooted_query_requires_a_component_reference_argument() {
        let ctx = Context::new();
        let operators = crate::test_support::connection_operators();
        let expression = vcg_query_with_target(
            "Connections.rooted",
            operators.declaration(rumoca_core::ConnectionGraphOperatorRole::Rooted),
            bool_expr(true),
        );

        let error =
            try_eval_structural_boolean(&ctx, &expression, &ast::QualifiedName::new(), &operators)
                .expect_err("a graph query without an exact component path must be refused");

        let crate::FlattenError::InvalidConnectionEvidence { description, .. } = error else {
            panic!("expected typed invalid-VCG-argument evidence, got {error:?}");
        };
        assert!(description.contains("Connections.rooted(<invalid argument>)"));
        assert!(description.contains("not a component reference"));
    }

    #[test]
    fn integer_comparison_uses_unindexed_integral_real_scope() {
        let mut ctx = Context::new();
        ctx.real_parameter_values
            .insert("adaptor.filter.transferFunction[1].nx".to_string(), 1.0);

        let prefix = ast::QualifiedName::from_dotted("adaptor.filter[1].transferFunction[1]");
        let expr = eq_expr(cref_expr("nx"), int_expr(0));
        let value = try_eval_boolean_with_ctx_inner(
            &expr,
            Some(&ctx),
            &prefix,
            &crate::test_support::connection_operators(),
        )
        .expect("fixture contains no VCG query");
        assert_eq!(value, Some(false));
    }

    fn resolved_enum_fixture(
        owner: rumoca_core::DefId,
        display_type: &str,
        literal: &str,
    ) -> (
        rumoca_eval_flat::constant::ResolvedEnumCatalog,
        rumoca_eval_flat::constant::ResolvedEnumValue,
    ) {
        let catalog = rumoca_eval_flat::constant::ResolvedEnumCatalog::try_from_declarations(vec![
            rumoca_eval_flat::constant::ResolvedEnumDeclaration {
                declaration: owner,
                type_name: display_type.to_string(),
                literals: vec![literal.to_string()],
            },
        ])
        .unwrap();
        let value = catalog.get(owner, literal).unwrap().clone();
        (catalog, value)
    }

    fn resolved_enum_literal(
        display_type: &str,
        owner: rumoca_core::DefId,
        literal: &str,
    ) -> ast::Expression {
        let mut parts = crate::path_utils::segments(display_type)
            .into_iter()
            .enumerate()
            .map(|(index, part)| ast::ComponentRefPart {
                ident: token(part),
                subs: None,
                def_id: Some(rumoca_core::DefId::new(13_000 + index as u32)),
            })
            .collect::<Vec<_>>();
        parts
            .last_mut()
            .expect("fixture enum type has at least one segment")
            .def_id = Some(owner);
        parts.push(ast::ComponentRefPart {
            ident: token(literal),
            subs: None,
            def_id: Some(owner),
        });
        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts,
            span: rumoca_core::Span::DUMMY,
            qualified_display_name: None,
        })
    }

    #[test]
    fn enum_equality_accepts_one_resolved_owner_and_ordinal() {
        let owner = rumoca_core::DefId::new(12_001);
        let parameter = rumoca_core::DefId::new(12_101);
        let instance = rumoca_core::InstanceId::new(12_201);
        let (catalog, value) = resolved_enum_fixture(owner, "Pkg.Mode", "on");
        let mut ctx = Context::new();
        ctx.resolved_enum_catalog = catalog;
        ctx.current_class_instance_id = Some(instance);
        ctx.parameter_values_by_identity.insert(
            rumoca_eval_flat::constant::ResolvedOccurrenceKey {
                instance_id: instance,
                root_def_id: parameter,
            },
            rumoca_eval_flat::constant::Value::ResolvedEnum(value),
        );

        let expr = eq_expr(
            cref_expr_with_target("mode", parameter),
            resolved_enum_literal("Pkg.Mode", owner, "on"),
        );
        let value = try_eval_boolean_with_ctx_inner(
            &expr,
            Some(&ctx),
            &ast::QualifiedName::new(),
            &crate::test_support::connection_operators(),
        )
        .expect("fixture contains no VCG query");
        assert_eq!(value, Some(true));
    }

    #[test]
    fn enum_equality_rejects_same_spelling_from_distinct_resolved_owners() {
        let first_owner = rumoca_core::DefId::new(12_002);
        let second_owner = rumoca_core::DefId::new(12_003);
        let parameter = rumoca_core::DefId::new(12_102);
        let instance = rumoca_core::InstanceId::new(12_202);
        let first_catalog =
            rumoca_eval_flat::constant::ResolvedEnumCatalog::try_from_declarations(vec![
                rumoca_eval_flat::constant::ResolvedEnumDeclaration {
                    declaration: first_owner,
                    type_name: "Pkg.Mode".to_string(),
                    literals: vec!["on".to_string()],
                },
                rumoca_eval_flat::constant::ResolvedEnumDeclaration {
                    declaration: second_owner,
                    type_name: "Pkg.Mode".to_string(),
                    literals: vec!["on".to_string()],
                },
            ])
            .unwrap();
        let first_value = first_catalog.get(first_owner, "on").unwrap().clone();
        let mut ctx = Context::new();
        ctx.resolved_enum_catalog = first_catalog;
        ctx.current_class_instance_id = Some(instance);
        ctx.parameter_values_by_identity.insert(
            rumoca_eval_flat::constant::ResolvedOccurrenceKey {
                instance_id: instance,
                root_def_id: parameter,
            },
            rumoca_eval_flat::constant::Value::ResolvedEnum(first_value),
        );

        let expr = eq_expr(
            cref_expr_with_target("mode", parameter),
            resolved_enum_literal("Pkg.Mode", second_owner, "on"),
        );
        let value = try_eval_boolean_with_ctx_inner(
            &expr,
            Some(&ctx),
            &ast::QualifiedName::new(),
            &crate::test_support::connection_operators(),
        )
        .expect("fixture contains no VCG query");
        assert_eq!(value, Some(false));
    }

    #[test]
    fn enum_parameter_lookup_refuses_foreign_same_spelling_declaration() {
        let owner = rumoca_core::DefId::new(12_005);
        let selected_parameter = rumoca_core::DefId::new(12_105);
        let foreign_parameter = rumoca_core::DefId::new(12_106);
        let instance = rumoca_core::InstanceId::new(12_205);
        let (catalog, value) = resolved_enum_fixture(owner, "Pkg.Mode", "on");
        let mut ctx = Context::new();
        ctx.resolved_enum_catalog = catalog;
        ctx.current_class_instance_id = Some(instance);
        ctx.parameter_values_by_identity.insert(
            rumoca_eval_flat::constant::ResolvedOccurrenceKey {
                instance_id: instance,
                root_def_id: selected_parameter,
            },
            rumoca_eval_flat::constant::Value::ResolvedEnum(value),
        );

        let expr = eq_expr(
            cref_expr_with_target("mode", foreign_parameter),
            resolved_enum_literal("Pkg.Mode", owner, "on"),
        );
        let value = try_eval_boolean_with_ctx_inner(
            &expr,
            Some(&ctx),
            &ast::QualifiedName::new(),
            &crate::test_support::connection_operators(),
        )
        .expect("fixture contains no VCG query");
        assert_eq!(value, None);
    }

    #[test]
    fn enum_equality_refuses_an_identity_free_literal() {
        let owner = rumoca_core::DefId::new(12_004);
        let parameter = rumoca_core::DefId::new(12_104);
        let instance = rumoca_core::InstanceId::new(12_204);
        let (catalog, value) = resolved_enum_fixture(owner, "Pkg.Mode", "on");
        let mut ctx = Context::new();
        ctx.resolved_enum_catalog = catalog;
        ctx.current_class_instance_id = Some(instance);
        ctx.parameter_values_by_identity.insert(
            rumoca_eval_flat::constant::ResolvedOccurrenceKey {
                instance_id: instance,
                root_def_id: parameter,
            },
            rumoca_eval_flat::constant::Value::ResolvedEnum(value),
        );

        let mut identity_free = comp_ref("Pkg.Mode.on");
        for part in &mut identity_free.parts {
            part.def_id = None;
        }
        let expr = eq_expr(
            cref_expr_with_target("mode", parameter),
            ast::Expression::ComponentReference(identity_free),
        );
        let value = try_eval_boolean_with_ctx_inner(
            &expr,
            Some(&ctx),
            &ast::QualifiedName::new(),
            &crate::test_support::connection_operators(),
        )
        .expect("fixture contains no VCG query");
        assert_eq!(value, None);
    }

    #[test]
    fn non_structural_boolean_parameter_is_not_structural() {
        let mut ctx = Context::new();
        ctx.boolean_parameter_values.insert("cp".to_string(), false);
        ctx.non_structural_params.insert("cp".to_string());

        let expr = cref_expr("cp");
        assert!(
            !is_structural_expression(
                &ctx,
                &expr,
                &ast::QualifiedName::new(),
                &crate::test_support::connection_operators(),
            ),
            "plain known boolean parameter must not be treated as structural"
        );
        assert_eq!(
            try_eval_structural_boolean(
                &ctx,
                &expr,
                &ast::QualifiedName::new(),
                &crate::test_support::connection_operators(),
            )
            .expect("fixture contains no VCG query"),
            None
        );
    }

    #[test]
    fn structural_short_circuit_does_not_read_non_structural_rhs() {
        let mut ctx = Context::new();
        ctx.boolean_parameter_values.insert("p".to_string(), true);
        ctx.non_structural_params.insert("p".to_string());
        let and = |lhs, rhs| ast::Expression::Binary {
            op: rumoca_core::OpBinary::And,
            lhs: Arc::new(lhs),
            rhs: Arc::new(rhs),
            span: rumoca_core::Span::DUMMY,
        };

        assert_eq!(
            try_eval_structural_boolean(
                &ctx,
                &and(bool_expr(false), cref_expr("p")),
                &ast::QualifiedName::new(),
                &crate::test_support::connection_operators(),
            )
            .expect("fixture contains no VCG query"),
            Some(false),
        );
        assert_eq!(
            try_eval_structural_boolean(
                &ctx,
                &and(bool_expr(true), cref_expr("p")),
                &ast::QualifiedName::new(),
                &crate::test_support::connection_operators(),
            )
            .expect("fixture contains no VCG query"),
            None,
        );
    }

    #[test]
    fn structural_parameter_can_be_evaluated_for_branch_selection() {
        let mut ctx = Context::new();
        ctx.boolean_parameter_values.insert("cp".to_string(), false);
        ctx.structural_params.insert("cp".to_string());

        let expr = cref_expr("cp");
        assert!(is_structural_expression(
            &ctx,
            &expr,
            &ast::QualifiedName::new(),
            &crate::test_support::connection_operators(),
        ));
        assert_eq!(
            try_eval_structural_boolean(
                &ctx,
                &expr,
                &ast::QualifiedName::new(),
                &crate::test_support::connection_operators(),
            )
            .expect("fixture contains no VCG query"),
            Some(false)
        );
    }

    #[test]
    fn known_constant_boolean_from_lookup_is_structural() {
        let mut ctx = Context::new();
        ctx.boolean_parameter_values
            .insert("fixedX".to_string(), false);

        let expr = cref_expr("fixedX");
        assert!(is_structural_expression(
            &ctx,
            &expr,
            &ast::QualifiedName::new(),
            &crate::test_support::connection_operators(),
        ));
        assert_eq!(
            try_eval_structural_boolean(
                &ctx,
                &expr,
                &ast::QualifiedName::new(),
                &crate::test_support::connection_operators(),
            )
            .expect("fixture contains no VCG query"),
            Some(false)
        );
    }
}
