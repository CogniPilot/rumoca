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
    qualify_expression_imports_with_def_map(expr, prefix, imports, None)
}

/// Qualify an expression with import-aware resolution and optional def-map canonicalization.
///
/// When a component reference carries a resolved `def_id` (notably function calls),
/// `def_map` canonicalizes it to the fully-qualified declaration name.
pub(crate) fn qualify_expression_imports_with_def_map(
    expr: &ast::Expression,
    prefix: &QualifiedName,
    imports: &qualify::ImportMap,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<rumoca_core::Expression, FlattenError> {
    // Use default options for equation qualification
    let opts = qualify::QualifyOptions::default();
    let filtered_imports;
    let imports = if let Some(def_map) = def_map {
        filtered_imports = imports_without_shadowed_aliases(expr, imports, def_map);
        &filtered_imports
    } else {
        imports
    };
    qualify_expression_with_effective_imports(
        expr,
        EffectiveExpressionContext {
            prefix,
            imports,
            options: opts,
            instance_name: None,
            locals: None,
            predefined_string_declaration: None,
            predefined_intrinsics: ast_lower::PredefinedIntrinsicIds::default(),
        },
    )
}

/// Qualify with flatten-context semantic metadata for class-reference canonicalization.
pub(crate) fn qualify_expression_imports_with_def_map_ctx(
    expr: &ast::Expression,
    prefix: &QualifiedName,
    imports: &qualify::ImportMap,
    def_map: Option<&crate::ResolveDefMap>,
    ctx: &Context,
    locals: Option<&std::collections::HashSet<String>>,
) -> Result<rumoca_core::Expression, FlattenError> {
    let opts = qualify::QualifyOptions::default();
    let def_filtered_imports;
    let imports = if let Some(def_map) = def_map {
        def_filtered_imports = imports_without_shadowed_aliases(expr, imports, def_map);
        &def_filtered_imports
    } else {
        imports
    };
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
        },
    )
}
