use rumoca_ir_ast as ast;

use crate::pipeline::{
    collect_function_calls_from_equation, collect_function_calls_from_expression,
};
use crate::{Context, FlattenError, connections, functions};

pub(crate) fn compute_cardinality_counts(ctx: &mut Context, overlay: &ast::InstanceOverlay) {
    for (_def_id, class_data) in &overlay.classes {
        for conn in &class_data.connections {
            if connections::connection_involves_disabled(conn, &overlay.disabled_components) {
                continue;
            }
            let a_path = conn.a.to_flat_string();
            let b_path = conn.b.to_flat_string();
            *ctx.cardinality_counts.entry(a_path).or_insert(0) += 1;
            *ctx.cardinality_counts.entry(b_path).or_insert(0) += 1;
        }
    }
}

pub(crate) fn pre_collect_functions(
    ctx: &mut Context,
    overlay: &ast::InstanceOverlay,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
) -> Result<(), FlattenError> {
    let mut function_names = functions::FunctionRequests::default();

    for (_def_id, class_data) in &overlay.classes {
        for eq in &class_data.equations {
            collect_function_calls_from_equation(
                &eq.equation,
                &mut function_names,
                tree,
                class_index,
            );
        }
    }

    for (_def_id, instance_data) in &overlay.components {
        if let Some(binding) = &instance_data.binding {
            collect_function_calls_from_expression(binding, &mut function_names, tree, class_index);
        }
        if let Some(start) = &instance_data.start {
            collect_function_calls_from_expression(start, &mut function_names, tree, class_index);
        }
        if let Some(min) = &instance_data.min {
            collect_function_calls_from_expression(min, &mut function_names, tree, class_index);
        }
        if let Some(max) = &instance_data.max {
            collect_function_calls_from_expression(max, &mut function_names, tree, class_index);
        }
        for sub in &instance_data.dims_expr {
            if let ast::Subscript::Expression(expr) = sub {
                collect_function_calls_from_expression(
                    expr,
                    &mut function_names,
                    tree,
                    class_index,
                );
            }
        }
    }

    let mut pending = function_names.into_entries();
    let mut visited = functions::FunctionRequests::default();
    while let Some(func_request) = pending.pop() {
        if !visited.insert_if_new(func_request.clone()) {
            continue;
        }
        let Some(func) = add_function_to_context(&func_request, ctx, tree, class_index)? else {
            continue;
        };
        for dep in functions::collect_function_dep_requests(&func) {
            if !visited.contains(&dep) {
                pending.push(dep);
            }
        }
    }
    Ok(())
}

fn add_function_to_context(
    request: &functions::FunctionRequest,
    ctx: &mut Context,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
) -> Result<Option<rumoca_core::Function>, FlattenError> {
    let Some((resolved_name, func)) =
        functions::lookup_function_request(tree, class_index, request)?
    else {
        return Ok(None);
    };
    if !functions::is_executable_flat_function(&func) {
        return Ok(None);
    }
    let qualified_name = func.name.to_string();

    if !ctx.functions.contains_key(&qualified_name) {
        ctx.functions.insert(qualified_name.clone(), func.clone());
    }
    if !ctx.functions.contains_key(&resolved_name) {
        ctx.functions.insert(resolved_name.clone(), func.clone());
    }
    if !ctx.functions.contains_key(&request.name) {
        ctx.functions.insert(request.name.clone(), func.clone());
    }
    add_function_short_name(&request.name, &func, ctx);
    add_function_short_name(&resolved_name, &func, ctx);
    add_function_short_name(&qualified_name, &func, ctx);
    Ok(Some(func))
}

fn add_function_short_name(func_name: &str, func: &rumoca_core::Function, ctx: &mut Context) {
    let short_name = crate::path_utils::top_level_last_segment(func_name);
    if short_name != func_name && !ctx.functions.contains_key(short_name) {
        ctx.functions.insert(short_name.to_string(), func.clone());
    }
}

pub(crate) fn extract_simple_path(expr: &ast::Expression) -> Option<String> {
    match expr {
        ast::Expression::ComponentReference(cr) => {
            let has_subscripts = cr
                .parts
                .iter()
                .any(|p| p.subs.as_ref().is_some_and(|s| !s.is_empty()));
            if has_subscripts || cr.parts.is_empty() {
                return None;
            }
            Some(
                cr.parts
                    .iter()
                    .map(|p| p.ident.text.as_ref())
                    .collect::<Vec<_>>()
                    .join("."),
            )
        }
        ast::Expression::FieldAccess { base, field, .. } => {
            let base_path = extract_simple_path(base)?;
            Some(format!("{}.{}", base_path, field))
        }
        _ => None,
    }
}
