use super::*;

pub(super) fn qualify_expression_with_effective_imports(
    expr: &ast::Expression,
    prefix: &QualifiedName,
    imports: &qualify::ImportMap,
    def_map: Option<&crate::ResolveDefMap>,
    opts: qualify::QualifyOptions,
    instance_name: Option<&str>,
    locals: Option<&std::collections::HashSet<String>>,
) -> Result<rumoca_core::Expression, FlattenError> {
    let qualified = locals.map_or_else(
        || qualify::qualify_expression_with_imports(expr, prefix, opts, imports),
        |locals| {
            qualify::qualify_expression_with_imports_and_locals(expr, prefix, opts, locals, imports)
        },
    );
    crate::ast_lower::expression_from_ast_with_context(
        &qualified,
        crate::ast_lower::LoweringContext {
            def_map,
            instance_name,
        },
    )
}

pub(super) fn imports_without_shadowed_aliases(
    expr: &ast::Expression,
    imports: &qualify::ImportMap,
    def_map: &crate::ResolveDefMap,
) -> qualify::ImportMap {
    let mut shadowed = std::collections::HashSet::new();
    collect_shadowed_import_aliases(expr, imports, def_map, &mut shadowed);
    if shadowed.is_empty() {
        return imports.clone();
    }

    imports
        .iter()
        .filter(|(alias, _)| !shadowed.contains(alias.as_str()))
        .map(|(alias, target)| (alias.clone(), target.clone()))
        .collect()
}

fn collect_shadowed_import_aliases(
    expr: &ast::Expression,
    imports: &qualify::ImportMap,
    def_map: &crate::ResolveDefMap,
    shadowed: &mut std::collections::HashSet<String>,
) {
    match expr {
        ast::Expression::ComponentReference(cr) => {
            collect_component_shadowed_import_alias(cr, imports, def_map, shadowed);
        }
        ast::Expression::Binary { lhs, rhs, .. } => {
            collect_shadowed_import_aliases(lhs, imports, def_map, shadowed);
            collect_shadowed_import_aliases(rhs, imports, def_map, shadowed);
        }
        ast::Expression::Unary { rhs, .. } | ast::Expression::Parenthesized { inner: rhs, .. } => {
            collect_shadowed_import_aliases(rhs, imports, def_map, shadowed);
        }
        ast::Expression::FunctionCall { comp, args, .. } => {
            collect_component_shadowed_import_alias(comp, imports, def_map, shadowed);
            for arg in args {
                collect_shadowed_import_aliases(arg, imports, def_map, shadowed);
            }
        }
        ast::Expression::ClassModification {
            target,
            modifications,
            ..
        } => {
            collect_component_shadowed_import_alias(target, imports, def_map, shadowed);
            for modification in modifications {
                collect_shadowed_import_aliases(modification, imports, def_map, shadowed);
            }
        }
        ast::Expression::NamedArgument { value, .. } => {
            collect_shadowed_import_aliases(value, imports, def_map, shadowed);
        }
        ast::Expression::Modification { target, value, .. } => {
            collect_component_shadowed_import_alias(target, imports, def_map, shadowed);
            collect_shadowed_import_aliases(value, imports, def_map, shadowed);
        }
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                collect_shadowed_import_aliases(condition, imports, def_map, shadowed);
                collect_shadowed_import_aliases(value, imports, def_map, shadowed);
            }
            collect_shadowed_import_aliases(else_branch, imports, def_map, shadowed);
        }
        ast::Expression::Array { elements, .. } | ast::Expression::Tuple { elements, .. } => {
            for element in elements {
                collect_shadowed_import_aliases(element, imports, def_map, shadowed);
            }
        }
        ast::Expression::Range {
            start, step, end, ..
        } => {
            collect_shadowed_import_aliases(start, imports, def_map, shadowed);
            if let Some(step) = step {
                collect_shadowed_import_aliases(step, imports, def_map, shadowed);
            }
            collect_shadowed_import_aliases(end, imports, def_map, shadowed);
        }
        ast::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            collect_shadowed_import_aliases(expr, imports, def_map, shadowed);
            for index in indices {
                collect_shadowed_import_aliases(&index.range, imports, def_map, shadowed);
            }
            if let Some(filter) = filter {
                collect_shadowed_import_aliases(filter, imports, def_map, shadowed);
            }
        }
        ast::Expression::ArrayIndex {
            base, subscripts, ..
        } => {
            collect_shadowed_import_aliases(base, imports, def_map, shadowed);
            for subscript in subscripts {
                collect_subscript_shadowed_import_aliases(subscript, imports, def_map, shadowed);
            }
        }
        ast::Expression::FieldAccess { base, .. } => {
            collect_shadowed_import_aliases(base, imports, def_map, shadowed);
        }
        ast::Expression::Terminal { .. } | ast::Expression::Empty { .. } => {}
    }
}

fn collect_component_shadowed_import_alias(
    cr: &ast::ComponentReference,
    imports: &qualify::ImportMap,
    def_map: &crate::ResolveDefMap,
    shadowed: &mut std::collections::HashSet<String>,
) {
    for part in &cr.parts {
        if let Some(subscripts) = &part.subs {
            for subscript in subscripts {
                collect_subscript_shadowed_import_aliases(subscript, imports, def_map, shadowed);
            }
        }
    }

    let Some(first) = cr.parts.first() else {
        return;
    };
    let alias = first.ident.text.as_ref();
    let Some(imported_path) = imports.get(alias) else {
        return;
    };
    let Some(resolved_path) = cr.def_id.and_then(|def_id| def_map.get(&def_id)) else {
        return;
    };
    if !super::context_and_tests::resolved_path_has_import_alias(resolved_path, alias) {
        return;
    }
    if resolved_path != imported_path {
        shadowed.insert(alias.to_string());
    }
}

fn collect_subscript_shadowed_import_aliases(
    subscript: &ast::Subscript,
    imports: &qualify::ImportMap,
    def_map: &crate::ResolveDefMap,
    shadowed: &mut std::collections::HashSet<String>,
) {
    if let ast::Subscript::Expression(expr) = subscript {
        collect_shadowed_import_aliases(expr, imports, def_map, shadowed);
    }
}

#[cfg(test)]
#[path = "context_and_tests/import_shadow_tests.rs"]
mod import_shadow_tests;
