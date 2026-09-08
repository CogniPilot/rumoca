//! Expression qualification entry points used by the flatten pipeline:
//! prefixing local names, resolving imported short names (MLS §13.2), and
//! canonicalizing resolved declarations through the def map.

use super::*;

/// Convert a QualifiedName to a flat VarName string.
pub(crate) fn qualified_to_var_name(qn: &QualifiedName) -> VarName {
    VarName::new(qn.to_flat_string())
}

/// Qualify an expression with a prefix (convert local names to global names).
///
/// This walks the expression tree and prefixes all component references
/// with the given prefix. For example, if prefix is "sub" and the expression
/// contains "x", it becomes "sub.x".
///
/// Uses default options: does not skip local refs, resets def_id.
/// Does NOT resolve imports — use `qualify_expression_imports` for that.
pub(crate) fn qualify_expression(
    expr: &ast::Expression,
    prefix: &QualifiedName,
) -> Result<rumoca_core::Expression, FlattenError> {
    qualify_expression_imports(expr, prefix, &qualify::ImportMap::default())
}

/// Qualify an expression with import-aware resolution (MLS §13.2).
///
/// Like `qualify_expression`, but also resolves imported short names to their
/// fully-qualified forms using the provided import map. For example, if imports
/// contain `("pi", "Modelica.Constants.pi")`, then `pi` becomes
/// `Modelica.Constants.pi` instead of being prefixed with the component path.
pub(crate) fn qualify_expression_imports(
    expr: &ast::Expression,
    prefix: &QualifiedName,
    imports: &qualify::ImportMap,
) -> Result<rumoca_core::Expression, FlattenError> {
    qualify_expression_with_effective_imports(
        expr,
        EffectiveExpressionContext {
            prefix,
            imports,
            options: qualify::QualifyOptions::default(),
            instance_name: None,
            locals: None,
            predefined_string_declaration: None,
            predefined_intrinsics: ast_lower::PredefinedIntrinsicIds::default(),
            equation_residual: false,
        },
    )
}

/// Qualify with flatten-context semantic metadata for class-reference canonicalization.
pub(crate) fn qualify_expression_imports_ctx(
    expr: &ast::Expression,
    prefix: &QualifiedName,
    imports: &qualify::ImportMap,
    ctx: &Context,
    locals: Option<&std::collections::HashSet<String>>,
) -> Result<rumoca_core::Expression, FlattenError> {
    qualify_expression_imports_ctx_mode(expr, prefix, imports, ctx, locals, false)
}

pub(crate) fn qualify_equation_residual_imports_ctx(
    expr: &ast::Expression,
    prefix: &QualifiedName,
    imports: &qualify::ImportMap,
    ctx: &Context,
    locals: Option<&std::collections::HashSet<String>>,
) -> Result<rumoca_core::Expression, FlattenError> {
    qualify_expression_imports_ctx_mode(expr, prefix, imports, ctx, locals, true)
}

fn qualify_expression_imports_ctx_mode(
    expr: &ast::Expression,
    prefix: &QualifiedName,
    imports: &qualify::ImportMap,
    ctx: &Context,
    locals: Option<&std::collections::HashSet<String>>,
    equation_residual: bool,
) -> Result<rumoca_core::Expression, FlattenError> {
    // MLS §5.3.1: a name the lookup authority refused to bind through the
    // current scope's imports is a typed error at its use site.
    import_scopes::refuse_ambiguous_import_uses(expr, &ctx.current_import_refusals, locals)?;
    let opts = qualify::QualifyOptions::default();
    let scoped_imports =
        component_member_scope::imports_without_instance_member_aliases(expr, prefix, imports, ctx);
    let instance_name = ctx.instance_name_for_prefix(prefix);
    qualify_expression_with_effective_imports(
        expr,
        EffectiveExpressionContext {
            prefix,
            imports: &scoped_imports,
            options: opts,
            instance_name: instance_name.as_deref(),
            locals,
            predefined_string_declaration: ctx.predefined_string_declaration,
            predefined_intrinsics: ctx.predefined_intrinsics,
            equation_residual,
        },
    )
}

struct EffectiveExpressionContext<'a> {
    prefix: &'a QualifiedName,
    imports: &'a qualify::ImportMap,
    options: qualify::QualifyOptions,
    instance_name: Option<&'a str>,
    locals: Option<&'a std::collections::HashSet<String>>,
    predefined_string_declaration: Option<rumoca_core::DefId>,
    predefined_intrinsics: crate::ast_lower::PredefinedIntrinsicIds,
    equation_residual: bool,
}

fn qualify_expression_with_effective_imports(
    expr: &ast::Expression,
    context: EffectiveExpressionContext<'_>,
) -> Result<rumoca_core::Expression, FlattenError> {
    let qualified = context.locals.map_or_else(
        || {
            qualify::qualify_expression_with_imports(
                expr,
                context.prefix,
                context.options,
                context.imports,
            )
        },
        |locals| {
            qualify::qualify_expression_with_imports_and_locals(
                expr,
                context.prefix,
                context.options,
                locals,
                context.imports,
            )
        },
    );
    let lowering = crate::ast_lower::LoweringContext {
        instance_name: context.instance_name,
        predefined_string_declaration: context.predefined_string_declaration,
        predefined_intrinsics: context.predefined_intrinsics,
    };
    if context.equation_residual {
        crate::ast_lower::equation_residual_from_ast_with_context(&qualified, lowering)
    } else {
        crate::ast_lower::expression_from_ast_with_context(&qualified, lowering)
    }
}
