use super::*;
use crate::record_constant_arrays::try_extract_record_array_constructor_constant;
use crate::source_spans::required_location_span;

mod component_binding_values;
mod context;
mod function_resolution;
mod structural_asserts;

pub(crate) use component_binding_values::collect_component_binding_values;
pub(crate) use context::{ConstantOccurrenceId, Context};
pub(crate) use function_resolution::resolve_function_name;
pub(crate) use structural_asserts::fold_structural_initial_asserts;

fn required_owner_span(
    span: rumoca_core::Span,
    context: &'static str,
) -> Option<rumoca_core::Span> {
    span.require_provenance(context)
        .ok()
        .map(rumoca_core::ProvenanceSpan::span)
}

pub(crate) fn inject_class_extends_constants(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    scope: &str,
    class_def: &ClassDef,
    resolve_context: &str,
    ctx: &mut Context,
) -> Result<(), FlattenError> {
    extract_constants_from_class_with_prefix_and_imports(
        tree,
        class_index,
        scope,
        class_def,
        resolve_context,
        ctx,
    )?;
    for ext in &class_def.extends {
        apply_extends_constants_for_scope(tree, class_index, scope, ext, resolve_context, ctx)?;
    }
    Ok(())
}

pub(crate) fn apply_extends_constants_for_scope(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    scope: &str,
    ext: &rumoca_ir_ast::Extend,
    resolve_context: &str,
    ctx: &mut Context,
) -> Result<(), FlattenError> {
    extract_extends_modification_constants(tree, class_index, scope, ext, resolve_context, ctx);
    if let Some(base_qname) =
        resolve_extends_base_qname(class_index, &ext.base_name.to_string(), resolve_context)
        && base_qname != scope
    {
        extract_extends_modification_constants(
            tree,
            class_index,
            &base_qname,
            ext,
            &base_qname,
            ctx,
        );
    }
    extract_extends_redeclare_package_constants(
        tree,
        class_index,
        scope,
        ext,
        resolve_context,
        ctx,
    )?;
    extract_extends_chain_constants(
        tree,
        class_index,
        scope,
        &ext.base_name.to_string(),
        resolve_context,
        ctx,
    )?;
    Ok(())
}

pub(crate) fn inject_nested_class_constants(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    comp_scope: &str,
    nested_scope: &str,
    nested_class: &ClassDef,
    resolve_context: &str,
    ctx: &mut Context,
) -> Result<(), FlattenError> {
    // `<comp>.<alias>.<const>`
    extract_constants_from_class_with_prefix_and_imports(
        tree,
        class_index,
        nested_scope,
        nested_class,
        resolve_context,
        ctx,
    )?;
    // `<comp>.<const>`
    extract_constants_from_class_with_prefix_and_imports(
        tree,
        class_index,
        comp_scope,
        nested_class,
        resolve_context,
        ctx,
    )?;

    for ext in &nested_class.extends {
        apply_extends_constants_for_scope(
            tree,
            class_index,
            nested_scope,
            ext,
            resolve_context,
            ctx,
        )?;
        apply_extends_constants_for_scope(
            tree,
            class_index,
            comp_scope,
            ext,
            resolve_context,
            ctx,
        )?;
    }
    Ok(())
}

pub(crate) fn inject_alias_component_package_constants(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    comp_scope: &str,
    alias_name: &str,
    alias_comp: &rumoca_ir_ast::Component,
    resolve_context: &str,
    ctx: &mut Context,
) -> Result<(), FlattenError> {
    if !alias_name.starts_with(char::is_uppercase) {
        return Ok(());
    }

    let Some((alias_class, alias_context)) =
        resolve_alias_component_class(tree, class_index, alias_comp, resolve_context)
    else {
        return Ok(());
    };
    if !matches!(
        alias_class.class_type,
        rumoca_core::ClassType::Package
            | rumoca_core::ClassType::Record
            | rumoca_core::ClassType::Class
    ) {
        return Ok(());
    }

    let alias_scope = format!("{comp_scope}.{alias_name}");
    extract_constants_from_class_with_prefix_and_imports(
        tree,
        class_index,
        &alias_scope,
        alias_class,
        &alias_context,
        ctx,
    )?;
    let type_alias = crate::path_utils::leaf_segment(&alias_context);
    let type_alias_scope = (!type_alias.is_empty() && type_alias != alias_name)
        .then(|| format!("{comp_scope}.{type_alias}"));
    if let Some(type_alias_scope) = &type_alias_scope {
        extract_constants_from_class_with_prefix_and_imports(
            tree,
            class_index,
            type_alias_scope,
            alias_class,
            &alias_context,
            ctx,
        )?;
    }
    if matches!(alias_class.class_type, rumoca_core::ClassType::Package) {
        extract_constants_from_class_with_prefix_and_imports(
            tree,
            class_index,
            comp_scope,
            alias_class,
            &alias_context,
            ctx,
        )?;
    }
    for ext in &alias_class.extends {
        apply_extends_constants_for_scope(
            tree,
            class_index,
            &alias_scope,
            ext,
            &alias_context,
            ctx,
        )?;
        if let Some(type_alias_scope) = &type_alias_scope {
            apply_extends_constants_for_scope(
                tree,
                class_index,
                type_alias_scope,
                ext,
                &alias_context,
                ctx,
            )?;
        }
        if matches!(alias_class.class_type, rumoca_core::ClassType::Package) {
            apply_extends_constants_for_scope(
                tree,
                class_index,
                comp_scope,
                ext,
                &alias_context,
                ctx,
            )?;
        }
    }
    Ok(())
}

fn resolve_alias_component_class<'tree>(
    tree: &ClassTree,
    class_index: &'tree rumoca_ir_ast::ClassDefIndex<'tree>,
    alias_comp: &rumoca_ir_ast::Component,
    resolve_context: &str,
) -> Option<(&'tree ClassDef, String)> {
    let alias_type_name = alias_comp.type_name.to_string();
    alias_comp
        .type_def_id
        .or(alias_comp.type_name.def_id)
        .and_then(|def_id| {
            let class = class_index.get(def_id)?;
            let context = tree
                .def_map
                .get(&def_id)
                .cloned()
                .unwrap_or_else(|| alias_type_name.clone());
            Some((class, context))
        })
        .or_else(|| {
            let (class, resolved_name) =
                resolve_class_in_scope_indexed(class_index, &alias_type_name, resolve_context);
            class.map(|class| {
                (
                    class,
                    resolved_name.unwrap_or_else(|| alias_type_name.clone()),
                )
            })
        })
        .or_else(|| {
            class_index
                .get_by_qualified_name(&alias_type_name)
                .map(|class| (class, alias_type_name))
        })
}

/// Collect a class and all ancestors, using resolved DefIds for class-body
/// access whenever the tree already carries them.
pub(crate) fn collect_ancestor_classes_with_index<'a>(
    tree: &'a ClassTree,
    class_index: &'a rumoca_ir_ast::ClassDefIndex<'a>,
    class_name: &str,
) -> Vec<&'a ClassDef> {
    let mut result = Vec::new();
    let mut queue: Vec<(String, String, Option<rumoca_core::DefId>)> =
        vec![(class_name.to_string(), class_name.to_string(), None)];
    let mut visited = std::collections::HashSet::new();
    while let Some((name, context, def_id)) = queue.pop() {
        let (class_def, qname) = if let Some(def_id) = def_id {
            let Some(class_def) = class_index.get(def_id) else {
                continue;
            };
            let Some(qname) = tree.def_map.get(&def_id).cloned() else {
                continue;
            };
            (class_def, qname)
        } else {
            let (class_def, resolved_qname) =
                resolve_class_in_scope_indexed(class_index, &name, &context);
            let Some(class_def) = class_def else { continue };
            (class_def, resolved_qname.unwrap_or_else(|| name.clone()))
        };
        if !visited.insert(qname.clone()) {
            continue;
        }
        for ext in &class_def.extends {
            queue.push((ext.base_name.to_string(), qname.clone(), ext.base_def_id));
        }
        result.push(class_def);
    }
    result
}

/// Resolve a potentially relative class name using a prebuilt class index.
pub(crate) fn resolve_class_in_scope_indexed<'a>(
    class_index: &'a rumoca_ir_ast::ClassDefIndex<'a>,
    name: &str,
    context: &str,
) -> (Option<&'a ClassDef>, Option<String>) {
    if let Some(cls) = class_index.get_by_qualified_name(name) {
        return (Some(cls), Some(name.to_string()));
    }
    if !context.is_empty() {
        let qualified = format!("{}.{}", context, name);
        if let Some(cls) = class_index.get_by_qualified_name(&qualified) {
            return (Some(cls), Some(qualified));
        }
    }
    for scope in crate::path_utils::enclosing_scopes(context) {
        let qualified = format!("{scope}.{name}");
        if let Some(cls) = class_index.get_by_qualified_name(&qualified) {
            return (Some(cls), Some(qualified));
        }
    }
    (None, None)
}

pub(crate) fn resolve_extends_base_qname(
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    base_name: &str,
    resolve_context: &str,
) -> Option<String> {
    if class_index.get_by_qualified_name(base_name).is_some() {
        return Some(base_name.to_string());
    }
    resolve_class_in_scope_indexed(class_index, base_name, resolve_context).1
}

/// Multi-pass extraction of integer constants and array dimensions from ancestor classes
/// (MLS §4.5, §7.1).
///
/// Each ancestor contributes its inherited constants under its *own* scope, not
/// only under the base class that declared them. MLS §7.2.5 makes an extends
/// modification part of the inheriting class's environment, so a base binding
/// that reads another inherited constant (`constant Integer nX = nS;` with
/// `extends PartialMedium(nS = 2)`) has to be re-evaluated there: the derived
/// class sees `nX = 2` while the base class still sees `nX = 1`. Recording only
/// the base-scope value leaves every dependent constant — the whole
/// `substanceNames`/`nS`/`nX`/`nXi` chain that sizes the Media arrays — at the
/// unmodified default.
pub(crate) fn extract_ancestor_constants_multi_pass(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    _resolve_context: &str,
    ancestors: &[&ClassDef],
    ctx: &mut Context,
) -> Result<(), FlattenError> {
    const MAX_PASSES: usize = 5;
    for _pass in 0..MAX_PASSES {
        let prev = ctx.parameter_values.len()
            + ctx.array_dimensions.len()
            + ctx.boolean_parameter_values.len()
            + ctx.real_parameter_values.len()
            + ctx.enum_parameter_values.len()
            + ctx.constant_values.len();
        for ancestor in ancestors {
            let ancestor_scope = class_scope_name(tree, ancestor)?;
            for ext in &ancestor.extends {
                apply_extends_constants_for_scope(
                    tree,
                    class_index,
                    &ancestor_scope,
                    ext,
                    &ancestor_scope,
                    ctx,
                )?;
            }
            extract_constants_from_class(class_index, ancestor, ctx);
        }
        let new = ctx.parameter_values.len()
            + ctx.array_dimensions.len()
            + ctx.boolean_parameter_values.len()
            + ctx.real_parameter_values.len()
            + ctx.enum_parameter_values.len()
            + ctx.constant_values.len();
        if new == prev {
            break;
        }
    }
    Ok(())
}

fn class_scope_name(tree: &ClassTree, class_def: &ClassDef) -> Result<String, FlattenError> {
    let span = required_location_span(
        &tree.source_map,
        &class_def.location,
        "ancestor constant injection",
    )?;
    let def_id = class_def.def_id.ok_or_else(|| {
        FlattenError::missing_resolved_class_metadata(
            class_def.name.text.to_string(),
            "ancestor constant injection",
            span,
        )
    })?;
    tree.def_map.get(&def_id).cloned().ok_or_else(|| {
        FlattenError::missing_resolved_class_metadata(
            class_def.name.text.to_string(),
            "ancestor constant injection",
            span,
        )
    })
}

/// Extract integer constants and array dimensions from a class definition (MLS §4.5).
pub(crate) fn extract_constants_from_class(
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    class_def: &ClassDef,
    ctx: &mut Context,
) {
    for (name, comp) in &class_def.components {
        if !matches!(
            comp.variability,
            rumoca_core::Variability::Constant(_) | rumoca_core::Variability::Parameter(_)
        ) {
            continue;
        }
        // MLS §4.4.4: a constant/parameter's value is its declaration binding.
        // `start` is an initial guess (MLS §4.9) that the parser seeds with the
        // declared type's default, so an unbound declaration contributes no
        // constant here rather than an invented `0`/`0.0`/`false` (SPEC_0008).
        let Some(expr) = comp.binding.as_ref() else {
            continue;
        };
        if !ctx.constant_values.contains_key(name)
            && let Some(val) =
                try_extract_record_array_constructor_constant(expr, class_index, ctx, "", name)
        {
            ctx.constant_values.insert(name.clone(), val);
        }
        if !ctx.constant_values.contains_key(name)
            && let Some(val) =
                try_extract_named_record_constructor_constant(expr, class_index, ctx, "", name)
        {
            ctx.constant_values.insert(name.clone(), val);
        }
        if !ctx.constant_values.contains_key(name)
            && let Some(val) = try_eval_const_flat_expr_with_scope(expr, ctx, "")
        {
            ctx.constant_values.insert(name.clone(), val);
        }
        match crate::functions::effective_component_constant_kind(class_index, comp) {
            Some(crate::functions::ComponentConstantKind::Integer)
                if !ctx.parameter_values.contains_key(name) =>
            {
                if let Some(val) = try_eval_const_integer_with_scope(expr, ctx, "") {
                    ctx.parameter_values.insert(name.clone(), val);
                }
            }
            Some(crate::functions::ComponentConstantKind::Real)
                if !ctx.real_parameter_values.contains_key(name) =>
            {
                if let Some(val) = try_eval_const_real_with_scope(expr, ctx, "")
                    && val.is_finite()
                {
                    ctx.real_parameter_values.insert(name.clone(), val);
                }
            }
            Some(crate::functions::ComponentConstantKind::Boolean)
                if !ctx.boolean_parameter_values.contains_key(name) =>
            {
                if let Some(val) = try_eval_const_boolean_with_scope(expr, ctx, "") {
                    ctx.boolean_parameter_values.insert(name.clone(), val);
                }
            }
            Some(crate::functions::ComponentConstantKind::Enumeration)
                if !ctx.enum_parameter_values.contains_key(name) =>
            {
                if let Some(val) = try_eval_const_enum_identity_with_scope(expr, ctx, "") {
                    ctx.enum_parameter_values.insert(name.clone(), val);
                }
            }
            _ => {}
        }
        // Array dimensions from shape
        if !ctx.array_dimensions.contains_key(name) && !comp.shape.is_empty() {
            let dims: Vec<i64> = comp.shape.iter().map(|&d| d as i64).collect();
            ctx.array_dimensions.insert(name.clone(), dims);
        }
        // Array dimensions from binding (array literal length)
        if !ctx.array_dimensions.contains_key(name)
            && let Some(dims) = infer_dims_from_expr(expr, ctx, "")
        {
            ctx.array_dimensions.insert(name.clone(), dims);
        }
        if let Some(def_id) = comp.def_id {
            let value = ctx.constant_values.get(name).cloned().or_else(|| {
                ctx.parameter_values
                    .get(name)
                    .map(|value| rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(*value),
                        span: expr.span(),
                    })
            });
            if let Some(value) = value {
                ctx.constant_values_by_def_id.insert(def_id, value);
            }
        }
    }
}

/// Lookup helper with lexical scope traversal.
///
/// Tries `scope.name`, then progressively shorter scopes, then bare `name`.
/// Aliases and class scopes must be explicit before this point; this lookup
/// does not recover structure from suffix matches.
pub(crate) fn lookup_with_scope<V: Clone + PartialEq>(
    name: &str,
    scope: &str,
    map: &rustc_hash::FxHashMap<String, V>,
) -> Option<V> {
    let name_path = QualifiedName::from_dotted(name);
    let scope_path = QualifiedName::from_dotted(scope);
    lookup_with_qualified_scope(&name_path, &scope_path, map)
}

pub(crate) fn lookup_with_qualified_scope<V: Clone + PartialEq>(
    name: &QualifiedName,
    scope: &QualifiedName,
    map: &rustc_hash::FxHashMap<String, V>,
) -> Option<V> {
    let mut current_scope = Some(scope.clone());
    loop {
        let qualified = match &current_scope {
            Some(scope) if !scope.is_empty() => scope.join(name).to_flat_string(),
            _ => name.to_flat_string(),
        };
        if let Some(val) = map.get(&qualified) {
            return Some(val.clone());
        }
        current_scope = match current_scope {
            Some(scope) if !scope.is_empty() => scope.parent(),
            _ => break,
        };
    }
    let bare_name = name.to_flat_string();
    if let Some(val) = map.get(&bare_name) {
        return Some(val.clone());
    }
    None
}

/// Scope-aware constant integer evaluation.
pub(crate) fn try_eval_const_integer_with_scope(
    expr: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<i64> {
    crate::boolean_eval::try_eval_integer_with_scope(expr, ctx, scope)
}

/// Scope-aware constant real evaluation.
pub(crate) fn try_eval_const_real_with_scope(
    expr: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<f64> {
    crate::boolean_eval::try_eval_real_with_scope(expr, ctx, scope)
}

pub(crate) fn eval_const_real_function_with_scope(
    comp: &rumoca_ir_ast::ComponentReference,
    args: &[ast::Expression],
    ctx: &Context,
    scope: &str,
) -> Option<f64> {
    let fn_name = comp
        .parts
        .last()
        .map(|p| p.ident.text.as_ref())
        .unwrap_or("");
    let eval = |e: &ast::Expression| try_eval_const_real_with_scope(e, ctx, scope);
    if args.len() == 1
        && let Some(function) = rumoca_core::BuiltinFunction::from_name(fn_name)
        && let Some(arg) = eval(&args[0])
        && let Some(value) = rumoca_core::apply_scalar_unary_math(function, arg)
    {
        return Some(value);
    }
    if args.len() == 2
        && let Some(function) = rumoca_core::BuiltinFunction::from_name(fn_name)
        && let Some(lhs) = eval(&args[0])
        && let Some(rhs) = eval(&args[1])
        && let Some(value) = rumoca_core::apply_scalar_binary_math(function, lhs, rhs)
    {
        return Some(value);
    }
    match fn_name {
        "size" if args.len() == 2 => {
            eval_size_call_with_scope(&args[0], &args[1], ctx, scope).map(|v| v as f64)
        }
        "abs" if args.len() == 1 => eval(&args[0]).map(f64::abs),
        "sign" if args.len() == 1 => eval(&args[0]).map(rumoca_core::modelica_sign),
        "sqrt" if args.len() == 1 => eval(&args[0]).map(f64::sqrt),
        "integer" if args.len() == 1 => Some(rumoca_core::modelica_integer_value(eval(&args[0])?)),
        _ => None,
    }
}

pub(crate) fn try_eval_const_flat_expr_with_scope(
    expr: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<rumoca_core::Expression> {
    if let Some(value) = try_eval_const_terminal_expr(expr) {
        return Some(value);
    }

    match expr {
        ast::Expression::ComponentReference(cr) => {
            try_eval_const_component_ref_expr(cr, ctx, scope)
        }
        ast::Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => match try_eval_const_flat_expr_with_scope(rhs, ctx, scope)? {
            rumoca_core::Expression::Literal {
                value: Literal::Real(v),
                ..
            } => Some(rumoca_core::Expression::Literal {
                value: Literal::Real(-v),
                span: expr.span(),
            }),
            rumoca_core::Expression::Literal {
                value: Literal::Integer(v),
                ..
            } => Some(rumoca_core::Expression::Literal {
                value: Literal::Integer(-v),
                span: expr.span(),
            }),
            _ => None,
        },
        ast::Expression::Parenthesized { inner, .. } => {
            try_eval_const_flat_expr_with_scope(inner, ctx, scope)
        }
        ast::Expression::Binary { lhs, rhs, op, .. } => {
            let lhs = try_eval_const_flat_expr_with_scope(lhs, ctx, scope)?;
            let rhs = try_eval_const_flat_expr_with_scope(rhs, ctx, scope)?;
            Some(rumoca_core::Expression::Binary {
                op: op.clone(),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span: expr.span(),
            })
        }
        ast::Expression::Array {
            elements,
            is_matrix,
            ..
        } => try_eval_const_array_expr(elements, *is_matrix, expr.span(), ctx, scope),
        ast::Expression::Tuple { elements, .. } => {
            try_eval_const_tuple_expr(elements, expr.span(), ctx, scope)
        }
        ast::Expression::Range {
            start, step, end, ..
        } => Some(rumoca_core::Expression::Range {
            start: Box::new(try_eval_const_flat_expr_with_scope(start, ctx, scope)?),
            step: if let Some(step_expr) = step {
                Some(Box::new(try_eval_const_flat_expr_with_scope(
                    step_expr, ctx, scope,
                )?))
            } else {
                None
            },
            end: Box::new(try_eval_const_flat_expr_with_scope(end, ctx, scope)?),
            span: expr.span(),
        }),
        ast::Expression::FunctionCall { comp, args, .. } => {
            try_eval_const_function_call_expr(comp, args, expr.span(), ctx, scope)
        }
        ast::Expression::FieldAccess {
            base,
            field,
            field_def_id,
            ..
        } => {
            if let Some(value) =
                try_eval_const_field_access_expr(base, field, expr.span(), ctx, scope)
            {
                return Some(value);
            }
            Some(rumoca_core::Expression::FieldAccess {
                base: Box::new(try_eval_const_flat_expr_with_scope(base, ctx, scope)?),
                field: field.clone(),
                field_def_id: (*field_def_id)?,
                span: expr.span(),
            })
        }
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => try_eval_const_if_expr(branches, else_branch, ctx, scope),
        _ => None,
    }
}

fn try_eval_const_field_access_expr(
    base: &ast::Expression,
    field: &str,
    span: rumoca_core::Span,
    ctx: &Context,
    scope: &str,
) -> Option<rumoca_core::Expression> {
    let ast::Expression::ComponentReference(base_ref) = base else {
        return None;
    };
    let name = QualifiedName::from_component_reference(base_ref).child(field);
    let name_text = name.to_flat_string();
    let scope_path = QualifiedName::from_dotted(scope);

    if let Some(value) = lookup_constant_expr_with_scope(&name_text, scope, &ctx.constant_values) {
        return Some(value.with_span(span));
    }
    if let Some(value) = lookup_with_qualified_scope(&name, &scope_path, &ctx.real_parameter_values)
        && value.is_finite()
    {
        return Some(rumoca_core::Expression::Literal {
            value: Literal::Real(value),
            span,
        });
    }
    if let Some(value) = lookup_with_qualified_scope(&name, &scope_path, &ctx.parameter_values) {
        return Some(rumoca_core::Expression::Literal {
            value: Literal::Integer(value),
            span,
        });
    }
    if let Some(value) =
        lookup_with_qualified_scope(&name, &scope_path, &ctx.boolean_parameter_values)
    {
        return Some(rumoca_core::Expression::Literal {
            value: Literal::Boolean(value),
            span,
        });
    }
    None
}

pub(crate) fn try_extract_named_record_constructor_constant(
    expr: &ast::Expression,
    class_index: &ast::ClassDefIndex<'_>,
    ctx: &mut Context,
    scope: &str,
    full_name: &str,
) -> Option<rumoca_core::Expression> {
    let (constructor, named_fields) = extract_named_record_constructor_fields(expr)?;

    if named_fields.is_empty() {
        return None;
    }

    // Evaluate named fields with bounded fixed-point passes so references like
    // `R_s = R_NASA_2002 / H2O.MM` resolve once `H2O.MM` is available.
    let mut pending = named_fields;
    let mut resolved: Vec<(String, rumoca_core::Expression)> = Vec::new();
    let max_passes = pending.len().max(1);
    for _ in 0..max_passes {
        let mut next_pending = Vec::new();
        let mut progress = false;
        for (field_name, field_expr) in std::mem::take(&mut pending) {
            if let Some(value) = try_eval_const_flat_expr_with_scope(&field_expr, ctx, scope) {
                let field_full_name = format!("{full_name}.{field_name}");
                register_named_record_field_constant(
                    ctx,
                    named_record_field_kind(class_index, &constructor, &field_name),
                    full_name,
                    &field_name,
                    &field_full_name,
                    &value,
                );
                resolved.push((field_name, value));
                progress = true;
            } else {
                next_pending.push((field_name, field_expr));
            }
        }
        if !progress {
            return None;
        }
        if next_pending.is_empty() {
            break;
        }
        pending = next_pending;
    }
    if !pending.is_empty() {
        return None;
    }

    let ctor_args = resolved
        .into_iter()
        .map(|field| named_record_constructor_arg(field, expr.span()))
        .collect::<Option<Vec<_>>>()?;
    Some(rumoca_core::Expression::FunctionCall {
        name: constructor,
        args: ctor_args,
        is_constructor: true,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: expr.span(),
    })
}

fn named_record_constructor_arg(
    (field_name, value): (String, rumoca_core::Expression),
    owner_span: rumoca_core::Span,
) -> Option<rumoca_core::Expression> {
    let span = value
        .span()
        .or_else(|| (!owner_span.is_dummy()).then_some(owner_span))?;
    Some(rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::generated(format!(
            "{}{field_name}",
            rumoca_core::NAMED_FUNCTION_ARG_PREFIX
        )),
        args: vec![value],
        is_constructor: true,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span,
    })
}

fn extract_named_record_constructor_fields(
    expr: &ast::Expression,
) -> Option<(rumoca_core::Reference, Vec<(String, ast::Expression)>)> {
    match expr {
        ast::Expression::FunctionCall { comp, args, .. } => {
            if args.is_empty() {
                return None;
            }
            let ctor_name = QualifiedName::from_component_reference(comp).to_flat_string();
            let constructor = rumoca_core::Reference::with_component_reference(
                ctor_name,
                core_component_reference_from_ast(comp)?,
            );
            let mut named_fields = Vec::new();
            for arg in args {
                let ast::Expression::NamedArgument { name, value, .. } = arg else {
                    return None;
                };
                named_fields.push((name.text.to_string(), value.as_ref().clone()));
            }
            Some((constructor, named_fields))
        }
        ast::Expression::ClassModification {
            target,
            modifications,
            ..
        } => {
            if modifications.is_empty() {
                return None;
            }
            let ctor_name = target.to_string();
            let constructor = rumoca_core::Reference::with_component_reference(
                ctor_name,
                core_component_reference_from_ast(target)?,
            );
            let mut named_fields = Vec::new();
            for modification in modifications {
                match modification {
                    ast::Expression::NamedArgument { name, value, .. } => {
                        named_fields.push((name.text.to_string(), value.as_ref().clone()));
                    }
                    ast::Expression::Modification {
                        target,
                        value: Some(value),
                        ..
                    } => {
                        let field_name = single_target_field_name(target)?;
                        named_fields.push((field_name, value.as_ref().clone()));
                    }
                    _ => return None,
                }
            }
            Some((constructor, named_fields))
        }
        _ => None,
    }
}

fn single_target_field_name(target: &ast::ComponentReference) -> Option<String> {
    let [single_part] = target.parts.as_slice() else {
        return None;
    };
    Some(single_part.ident.text.to_string())
}

fn register_named_record_field_constant(
    ctx: &mut Context,
    kind: Option<crate::functions::ComponentConstantKind>,
    record_prefix: &str,
    field_name: &str,
    field_full_name: &str,
    value: &rumoca_core::Expression,
) {
    insert_with_prefix(
        &mut ctx.constant_values,
        record_prefix,
        field_name,
        field_full_name,
        value.clone(),
    );
    match (kind, value) {
        (
            Some(crate::functions::ComponentConstantKind::Integer),
            rumoca_core::Expression::Literal {
                value: Literal::Integer(v),
                ..
            },
        ) => {
            insert_with_prefix(
                &mut ctx.parameter_values,
                record_prefix,
                field_name,
                field_full_name,
                *v,
            );
        }
        (
            Some(crate::functions::ComponentConstantKind::Real),
            rumoca_core::Expression::Literal {
                value: Literal::Real(v),
                ..
            },
        ) if v.is_finite() => {
            insert_with_prefix(
                &mut ctx.real_parameter_values,
                record_prefix,
                field_name,
                field_full_name,
                *v,
            );
        }
        (
            Some(crate::functions::ComponentConstantKind::Real),
            rumoca_core::Expression::Literal {
                value: Literal::Integer(v),
                ..
            },
        ) => {
            let value = *v as f64;
            if value.is_finite() {
                insert_with_prefix(
                    &mut ctx.real_parameter_values,
                    record_prefix,
                    field_name,
                    field_full_name,
                    value,
                );
            }
        }
        (
            Some(crate::functions::ComponentConstantKind::Boolean),
            rumoca_core::Expression::Literal {
                value: Literal::Boolean(v),
                ..
            },
        ) => {
            insert_with_prefix(
                &mut ctx.boolean_parameter_values,
                record_prefix,
                field_name,
                field_full_name,
                *v,
            );
        }
        (
            Some(crate::functions::ComponentConstantKind::Enumeration),
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            },
        ) if subscripts.is_empty() => {
            let Some(value) = ctx.resolved_enum_catalog.get_reference(name).cloned() else {
                return;
            };
            insert_with_prefix(
                &mut ctx.enum_parameter_values,
                record_prefix,
                field_name,
                field_full_name,
                value,
            );
        }
        _ => {}
    }
}

fn named_record_field_kind(
    class_index: &ast::ClassDefIndex<'_>,
    constructor: &rumoca_core::Reference,
    field_name: &str,
) -> Option<crate::functions::ComponentConstantKind> {
    let record = class_index.get(constructor.target_def_id()?)?;
    let field = crate::functions::component_in_class_scope(class_index, record, field_name)?;
    crate::functions::effective_component_constant_kind(class_index, field)
}

pub(crate) fn try_eval_const_component_ref_expr(
    cr: &rumoca_ir_ast::ComponentReference,
    ctx: &Context,
    scope: &str,
) -> Option<rumoca_core::Expression> {
    let owner_span = required_owner_span(cr.span, "constant component reference evaluation")?;
    if cr.parts.iter().any(|part| {
        part.subs
            .as_ref()
            .is_some_and(|subscripts| !subscripts.is_empty())
    }) {
        let Ok(lowered) = crate::ast_lower::expression_from_ast_with_context(
            &ast::Expression::ComponentReference(cr.clone()),
            crate::ast_lower::LoweringContext {
                predefined_intrinsics: ctx.predefined_intrinsics,
                ..crate::ast_lower::LoweringContext::default()
            },
        ) else {
            return None;
        };
        let Ok(substituted) = crate::postprocess::substitute_known_constants_expr(
            lowered.clone(),
            ctx,
            &rustc_hash::FxHashSet::default(),
            &std::collections::HashSet::new(),
            scope,
        ) else {
            return None;
        };
        if substituted != lowered {
            return Some(substituted);
        }
    }

    let name = QualifiedName::from_component_reference(cr);
    let name_text = name.to_flat_string();
    let scope_path = QualifiedName::from_dotted(scope);
    if let Some(v) = lookup_constant_expr_with_scope(&name_text, scope, &ctx.constant_values) {
        if component_ref_has_array_shape(&name, ctx, scope)
            && !constant_expr_preserves_array_shape(&v)
        {
            return None;
        }
        return Some(v);
    }
    if component_ref_has_array_shape(&name, ctx, scope) {
        return None;
    }
    if let Some(v) = lookup_with_qualified_scope(&name, &scope_path, &ctx.real_parameter_values) {
        return Some(rumoca_core::Expression::Literal {
            value: Literal::Real(v),
            span: owner_span,
        });
    }
    if let Some(v) = lookup_with_qualified_scope(&name, &scope_path, &ctx.parameter_values) {
        return Some(rumoca_core::Expression::Literal {
            value: Literal::Integer(v),
            span: owner_span,
        });
    }
    if let Some(v) = lookup_with_qualified_scope(&name, &scope_path, &ctx.boolean_parameter_values)
    {
        return Some(rumoca_core::Expression::Literal {
            value: Literal::Boolean(v),
            span: owner_span,
        });
    }
    let resolved = resolve_component_ref_through_constant_aliases(&name, ctx, scope);
    let resolved_text = resolved.as_ref().unwrap_or(&name).to_flat_string();
    try_eval_resolved_const_ref(&resolved_text, ctx, owner_span).or_else(|| {
        if !looks_like_enum_literal_path(&resolved_text) {
            return None;
        }
        let component = core_component_reference_from_ast(cr)?;
        Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(&resolved_text, component),
            subscripts: vec![],
            span: owner_span,
        })
    })
}

fn try_eval_resolved_const_ref(
    name: &str,
    ctx: &Context,
    owner_span: rumoca_core::Span,
) -> Option<rumoca_core::Expression> {
    if let Some(v) = lookup_constant_expr_with_scope(name, "", &ctx.constant_values) {
        if lookup_with_scope(name, "", &ctx.array_dimensions).is_some_and(|dims| !dims.is_empty())
            && !constant_expr_preserves_array_shape(&v)
        {
            return None;
        }
        return Some(v);
    }
    if lookup_with_scope(name, "", &ctx.array_dimensions).is_some_and(|dims| !dims.is_empty()) {
        return None;
    }
    if let Some(v) = lookup_with_scope(name, "", &ctx.real_parameter_values) {
        return Some(rumoca_core::Expression::Literal {
            value: Literal::Real(v),
            span: owner_span,
        });
    }
    if let Some(v) = lookup_with_scope(name, "", &ctx.parameter_values) {
        return Some(rumoca_core::Expression::Literal {
            value: Literal::Integer(v),
            span: owner_span,
        });
    }
    if let Some(v) = lookup_with_scope(name, "", &ctx.boolean_parameter_values) {
        return Some(rumoca_core::Expression::Literal {
            value: Literal::Boolean(v),
            span: owner_span,
        });
    }
    None
}

fn component_ref_has_array_shape(name: &QualifiedName, ctx: &Context, scope: &str) -> bool {
    lookup_with_qualified_scope(
        name,
        &QualifiedName::from_dotted(scope),
        &ctx.array_dimensions,
    )
    .is_some_and(|dims| !dims.is_empty())
}

fn resolve_component_ref_through_constant_aliases(
    name: &QualifiedName,
    ctx: &Context,
    scope: &str,
) -> Option<QualifiedName> {
    let mut current = name.clone();
    let mut visited = rustc_hash::FxHashSet::default();
    loop {
        if !visited.insert(current.to_flat_string()) {
            return None;
        }
        let mut replaced = false;
        for split_idx in (1..current.parts.len()).rev() {
            let prefix = QualifiedName {
                parts: current.parts[..split_idx].to_vec(),
            };
            let suffix = QualifiedName {
                parts: current.parts[split_idx..].to_vec(),
            };
            let Some(alias_expr) = lookup_constant_expr_with_scope(
                &prefix.to_flat_string(),
                scope,
                &ctx.constant_values,
            ) else {
                continue;
            };
            let rumoca_core::Expression::VarRef {
                name: alias_name,
                subscripts,
                ..
            } = alias_expr
            else {
                continue;
            };
            if !subscripts.is_empty() {
                continue;
            }
            current = QualifiedName::from_dotted(alias_name.as_str()).join(&suffix);
            replaced = true;
            break;
        }
        if !replaced {
            return (current != *name).then_some(current);
        }
    }
}

fn try_eval_const_function_call_expr(
    comp: &rumoca_ir_ast::ComponentReference,
    args: &[ast::Expression],
    owner_span: rumoca_core::Span,
    ctx: &Context,
    scope: &str,
) -> Option<rumoca_core::Expression> {
    let owner_span = required_owner_span(owner_span, "constant function call evaluation")?;
    let evaluated_args: Vec<_> = args
        .iter()
        .map(|arg| try_eval_const_flat_expr_with_scope(arg, ctx, scope))
        .collect::<Option<Vec<_>>>()?;

    let textual_name = QualifiedName::from_component_reference(comp).to_flat_string();
    let short_name = comp
        .parts
        .last()
        .map(|part| part.ident.text.as_ref())
        .unwrap_or(textual_name.as_str());

    if short_name == "array" {
        return Some(rumoca_core::Expression::Array {
            elements: evaluated_args,
            is_matrix: false,
            span: owner_span,
        });
    }

    if let Some(function) = rumoca_core::BuiltinFunction::from_name(short_name) {
        return Some(rumoca_core::Expression::BuiltinCall {
            function,
            args: evaluated_args,
            span: owner_span,
        });
    }

    if let Some(function) =
        rumoca_core::BuiltinFunction::from_name(&short_name.to_ascii_lowercase())
    {
        return Some(rumoca_core::Expression::BuiltinCall {
            function,
            args: evaluated_args,
            span: owner_span,
        });
    }

    Some(rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            textual_name,
            core_component_reference_from_ast(comp)?,
        ),
        args: evaluated_args,
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: owner_span,
    })
}

fn core_component_reference_from_ast(
    comp: &rumoca_ir_ast::ComponentReference,
) -> Option<rumoca_core::ComponentReference> {
    let parts = comp
        .parts
        .iter()
        .map(|part| {
            Some(rumoca_core::ComponentRefPart {
                ident: part.ident.text.to_string(),
                span: comp.span,
                subs: Vec::new(),
                def_id: part.def_id?,
            })
        })
        .collect::<Option<Vec<_>>>()?;
    rumoca_core::ComponentReference::construct(comp.local, comp.span, parts).ok()
}

pub(crate) fn try_eval_const_array_expr(
    elements: &[ast::Expression],
    is_matrix: bool,
    owner_span: rumoca_core::Span,
    ctx: &Context,
    scope: &str,
) -> Option<rumoca_core::Expression> {
    let owner_span = required_owner_span(owner_span, "constant array evaluation")?;
    let mut out = Vec::with_capacity(elements.len());
    for el in elements {
        out.push(try_eval_const_flat_expr_with_scope(el, ctx, scope)?);
    }
    Some(rumoca_core::Expression::Array {
        elements: out,
        is_matrix,
        span: owner_span,
    })
}

pub(crate) fn try_eval_const_tuple_expr(
    elements: &[ast::Expression],
    owner_span: rumoca_core::Span,
    ctx: &Context,
    scope: &str,
) -> Option<rumoca_core::Expression> {
    let owner_span = required_owner_span(owner_span, "constant tuple evaluation")?;
    let mut out = Vec::with_capacity(elements.len());
    for el in elements {
        out.push(try_eval_const_flat_expr_with_scope(el, ctx, scope)?);
    }
    Some(rumoca_core::Expression::Tuple {
        elements: out,
        span: owner_span,
    })
}

pub(crate) fn try_eval_const_if_expr(
    branches: &[(ast::Expression, ast::Expression)],
    else_branch: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<rumoca_core::Expression> {
    let selected = select_const_if_branch(branches, else_branch, ctx, scope)?;
    try_eval_const_flat_expr_with_scope(selected, ctx, scope)
}

fn select_const_if_branch<'a>(
    branches: &'a [(ast::Expression, ast::Expression)],
    else_branch: &'a ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<&'a ast::Expression> {
    for (cond, then_expr) in branches {
        match try_eval_const_boolean_with_scope(cond, ctx, scope) {
            Some(true) => return Some(then_expr),
            Some(false) => continue,
            None => return None,
        }
    }
    Some(else_branch)
}

pub(crate) fn lookup_constant_expr_with_scope(
    name: &str,
    scope: &str,
    map: &rustc_hash::FxHashMap<String, rumoca_core::Expression>,
) -> Option<rumoca_core::Expression> {
    let name_path = QualifiedName::from_dotted(name);
    let scope_path = QualifiedName::from_dotted(scope);
    lookup_constant_expr_with_qualified_scope(&name_path, &scope_path, map)
}

fn lookup_constant_expr_with_qualified_scope(
    name: &QualifiedName,
    scope: &QualifiedName,
    map: &rustc_hash::FxHashMap<String, rumoca_core::Expression>,
) -> Option<rumoca_core::Expression> {
    let mut current_scope = Some(scope.clone());
    loop {
        let qualified = match &current_scope {
            Some(scope) if !scope.is_empty() => scope.join(name).to_flat_string(),
            _ => name.to_flat_string(),
        };
        if let Some(val) = map.get(&qualified) {
            return Some(val.clone());
        }
        current_scope = match current_scope {
            Some(scope) if !scope.is_empty() => scope.parent(),
            _ => break,
        };
    }
    let bare_name = name.to_flat_string();
    if let Some(val) = map.get(&bare_name) {
        return Some(val.clone());
    }
    None
}

/// Scope-aware builtin integer function evaluation.
pub(crate) fn eval_const_integer_function_with_scope(
    comp: &rumoca_ir_ast::ComponentReference,
    args: &[ast::Expression],
    ctx: &Context,
    scope: &str,
) -> Option<i64> {
    let fn_name = comp
        .parts
        .last()
        .map(|p| p.ident.text.as_ref())
        .unwrap_or("");
    let eval = |e: &ast::Expression| try_eval_const_integer_with_scope(e, ctx, scope);
    match fn_name {
        "size" if args.len() == 2 => eval_size_call_with_scope(&args[0], &args[1], ctx, scope),
        "abs" if args.len() == 1 => eval(&args[0]).map(|v| v.abs()),
        "sign" if args.len() == 1 => eval(&args[0]).map(|v| v.signum()),
        "integer" if args.len() == 1 => eval(&args[0]),
        "max" if args.len() == 2 => Some(eval(&args[0])?.max(eval(&args[1])?)),
        "min" if args.len() == 2 => Some(eval(&args[0])?.min(eval(&args[1])?)),
        "div" if args.len() == 2 => {
            let (x, y) = (eval(&args[0])?, eval(&args[1])?);
            rumoca_core::eval_integer_div_builtin(x, y)
        }
        "mod" if args.len() == 2 => {
            let (x, y) = (eval(&args[0])?, eval(&args[1])?);
            rumoca_core::eval_integer_mod_builtin(x, y)
        }
        "rem" if args.len() == 2 => {
            let (x, y) = (eval(&args[0])?, eval(&args[1])?);
            rumoca_core::eval_integer_rem_builtin(x, y)
        }
        _ => None,
    }
}

/// Scope-aware constant boolean evaluation.
pub(crate) fn try_eval_const_boolean_with_scope(
    expr: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<bool> {
    crate::boolean_eval::try_eval_boolean_with_scope(expr, ctx, scope)
}

pub(crate) fn try_eval_const_enum_identity_with_scope(
    expr: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<rumoca_eval_flat::constant::ResolvedEnumValue> {
    match expr {
        ast::Expression::ComponentReference(cr) => {
            let name = QualifiedName::from_component_reference(cr).to_flat_string();
            lookup_with_scope(&name, scope, &ctx.enum_parameter_values).or_else(|| {
                let component = core_component_reference_from_ast(cr)?;
                let reference = rumoca_core::Reference::from_component_reference(component);
                ctx.resolved_enum_catalog.get_reference(&reference).cloned()
            })
        }
        ast::Expression::Parenthesized { inner, .. } => {
            try_eval_const_enum_identity_with_scope(inner, ctx, scope)
        }
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let selected = select_const_if_branch(branches, else_branch, ctx, scope)?;
            try_eval_const_enum_identity_with_scope(selected, ctx, scope)
        }
        _ => None,
    }
}

/// Scope-aware `size(array, dim)` evaluation.
pub(crate) fn eval_size_call_with_scope(
    array_expr: &ast::Expression,
    dim_expr: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<i64> {
    let dim = try_eval_const_integer_with_scope(dim_expr, ctx, scope)?;
    if dim < 1 {
        return None;
    }
    let dims = match array_expr {
        ast::Expression::ComponentReference(cr) => {
            let arr_name = QualifiedName::from_component_reference(cr).to_flat_string();
            lookup_size_array_dims_with_scope(&arr_name, scope, ctx)
        }
        // MLS §10.3.1: size() depends on the argument's type shape, not its
        // element values. In particular, string/Boolean/enum matrix literals
        // must not be evaluated merely to answer size(A, i).
        _ => infer_dims_from_expr(array_expr, ctx, scope),
    }?;
    dims.get((dim - 1) as usize).copied()
}

fn lookup_size_array_dims_with_scope(
    arr_name: &str,
    scope: &str,
    ctx: &Context,
) -> Option<Vec<i64>> {
    lookup_with_scope(arr_name, scope, &ctx.array_dimensions)
        .or_else(|| lookup_scoped_class_member_array_dims(arr_name, scope, ctx))
}

fn lookup_scoped_class_member_array_dims(
    arr_name: &str,
    scope: &str,
    ctx: &Context,
) -> Option<Vec<i64>> {
    let arr_path = QualifiedName::from_dotted(arr_name);
    if scope.is_empty() || !arr_path.is_dotted() {
        return None;
    }
    let last_segment = arr_path.last_name()?;
    let scoped_member = format!("{scope}.{last_segment}");
    if let Some(dims) = ctx.get_array_dims(&scoped_member) {
        return Some(dims);
    }
    for candidate in crate::path_utils::unindexed_lookup_variants(&scoped_member) {
        if let Some(dims) = ctx.get_array_dims(&candidate) {
            return Some(dims);
        }
    }
    None
}

/// Infer array dimensions from an expression (array literal length).
pub(crate) fn infer_dims_from_expr(
    expr: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<Vec<i64>> {
    match expr {
        ast::Expression::Array { .. }
        | ast::Expression::Range { .. }
        | ast::Expression::DerivativeCall { .. } => infer_dims_via_eval_ast_i64(expr, ctx, scope),
        ast::Expression::FunctionCall { comp, args, .. } => {
            let fn_name = comp
                .parts
                .last()
                .map(|p| p.ident.text.as_ref())
                .unwrap_or("");
            if fn_name == "fill" && args.len() >= 2 {
                let mut dims: Vec<i64> = args[1..]
                    .iter()
                    .map(|a| try_eval_const_integer_with_scope(a, ctx, scope))
                    .collect::<Option<Vec<_>>>()?;
                let mut seed_dims = infer_dims_via_eval_ast_i64(&args[0], ctx, scope)?;
                dims.append(&mut seed_dims);
                Some(dims)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn infer_dims_via_eval_ast_i64(
    expr: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<Vec<i64>> {
    infer_dims_via_eval_ast(expr, ctx, scope).and_then(|dims| {
        dims.into_iter()
            .map(|dim| i64::try_from(dim).ok())
            .collect::<Option<Vec<_>>>()
    })
}

/// Borrowed semantic facts for the shared AST dimension walk.
///
/// Flatten owns these maps and updates them while constants stabilize. Keeping
/// the adapter borrowed makes every inference observe the current facts without
/// rebuilding a parallel evaluator or recovering structure from rendered text.
struct FlattenDimensionContext<'a> {
    context: &'a Context,
}

impl rumoca_eval_ast::eval::DimensionInferenceContext for FlattenDimensionContext<'_> {
    fn lookup_dimensions(&self, name: &str, scope: &str) -> Option<Vec<usize>> {
        lookup_size_array_dims_with_scope(name, scope, self.context)?
            .into_iter()
            .map(|dimension| usize::try_from(dimension).ok())
            .collect()
    }

    fn scalar_value_known(&self, name: &str, scope: &str) -> bool {
        lookup_with_scope(name, scope, &self.context.parameter_values).is_some()
            || lookup_with_scope(name, scope, &self.context.real_parameter_values).is_some()
            || lookup_with_scope(name, scope, &self.context.boolean_parameter_values).is_some()
            || lookup_with_scope(name, scope, &self.context.enum_parameter_values).is_some()
    }

    fn eval_integer(&self, expression: &ast::Expression, scope: &str) -> Option<i64> {
        try_eval_const_integer_with_scope(expression, self.context, scope)
    }

    fn eval_real(&self, expression: &ast::Expression, scope: &str) -> Option<f64> {
        try_eval_const_real_with_scope(expression, self.context, scope)
    }

    fn eval_boolean(&self, expression: &ast::Expression, scope: &str) -> Option<bool> {
        try_eval_const_boolean_with_scope(expression, self.context, scope)
    }
}

fn infer_dims_via_eval_ast(
    expr: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<Vec<usize>> {
    rumoca_eval_ast::eval::infer_dimensions_from_binding_with_scope(
        expr,
        &FlattenDimensionContext { context: ctx },
        scope,
    )
}

/// Pre-evaluate structural equations (MLS §4.4.4).
///
/// Scans equations for simple assignments `var = expr` where `var` is a discrete
/// Boolean variable and `expr` depends only on parameters. If we can evaluate
/// the RHS at compile time, we add the result to the context for if-equation
/// branch selection.
///
/// This handles cases like LossyGear's `ideal = isEqual(lossTable, [0,1,1,0,0], eps)`
/// where `lossTable` is a parameter with a default value.
pub(crate) fn pre_evaluate_structural_equations(
    ctx: &mut Context,
    overlay: &InstanceOverlay,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> Result<(), FlattenError> {
    let eval_ctx = build_structural_eval_context(ctx, overlay, tree, class_index)?;
    let mut call_canonicalizer = crate::functions::StructuralFoldCallCanonicalizer::new(
        ctx.functions.values(),
        tree,
        class_index,
    )?;

    // Scan all class instances for structural Boolean equations
    for (_def_id, class_data) in &overlay.classes {
        let prefix = &class_data.qualified_name;
        for eq_entry in &class_data.equations {
            if !equation_targets_boolean_instance(
                &eq_entry.equation,
                class_data.instance_id,
                overlay,
                tree.type_table.boolean(),
            ) {
                continue;
            }
            if let Some((var_name, bool_val)) = try_eval_structural_equation(
                &eq_entry.equation,
                prefix,
                ctx,
                &eval_ctx,
                &mut call_canonicalizer,
            )? {
                #[cfg(feature = "tracing")]
                tracing::debug!(
                    var = %var_name,
                    value = bool_val,
                    "pre-evaluated structural Boolean equation"
                );
                ctx.boolean_parameter_values.insert(var_name, bool_val);
            }
        }
    }
    Ok(())
}

fn equation_targets_boolean_instance(
    equation: &rumoca_ir_ast::Equation,
    owner_class_id: rumoca_core::InstanceId,
    overlay: &InstanceOverlay,
    boolean_type: rumoca_core::TypeId,
) -> bool {
    let ast::Equation::Simple {
        lhs: ast::Expression::ComponentReference(reference),
        ..
    } = equation
    else {
        return false;
    };
    if reference.parts.len() != 1 || reference.parts[0].subs.is_some() {
        return false;
    }
    let Some(target_def_id) = reference.target_def_id() else {
        return false;
    };
    let Some(owner) = overlay.classes.get(&owner_class_id) else {
        return false;
    };
    let target_path = owner
        .qualified_name
        .join(&ast::QualifiedName::from_component_reference(reference))
        .to_component_path();
    if overlay.array_parent_dims.contains_key(&target_path) {
        return false;
    }

    overlay.components.values().any(|instance| {
        instance.owner_class_id == Some(owner_class_id)
            && instance.dims.is_empty()
            && instance.qualified_name.to_component_path() == target_path
            && overlay
                .type_roots
                .get(&instance.type_id)
                .copied()
                .unwrap_or(instance.type_id)
                == boolean_type
            && instance
                .component_ref
                .as_ref()
                .is_some_and(|component_ref| component_ref.target_def_id() == target_def_id)
    })
}

/// Build an evaluation context populated with known parameters, functions, constants,
/// and component bindings from the overlay.
pub(crate) fn build_structural_eval_context(
    ctx: &Context,
    overlay: &InstanceOverlay,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> Result<rumoca_eval_flat::constant::EvalContext, FlattenError> {
    use rumoca_eval_flat::constant::{EvalContext, Value};

    reject_overlapping_typed_parameter_caches(ctx)?;

    let parameter_capacity = ctx.parameter_values.len()
        + ctx.real_parameter_values.len()
        + ctx.boolean_parameter_values.len();
    let enum_catalog = resolved_tree_enum_catalog(tree)?;
    let mut eval_ctx = EvalContext::structural_preidentity_with_catalog(
        parameter_capacity,
        ctx.functions.len() * 2,
        enum_catalog,
    );
    for (name, value) in &ctx.parameter_values {
        if parameter_is_scalar(ctx, name) {
            eval_ctx.add_parameter(name.clone(), Value::Integer(*value));
        }
    }
    for (name, value) in &ctx.real_parameter_values {
        if parameter_is_scalar(ctx, name) {
            eval_ctx.add_parameter(name.clone(), Value::Real(*value));
        }
    }
    for (name, value) in &ctx.boolean_parameter_values {
        if parameter_is_scalar(ctx, name) {
            eval_ctx.add_parameter(name.clone(), Value::Bool(*value));
        }
    }
    for (name, dimensions) in &ctx.array_dimensions {
        eval_ctx.add_array_dimensions(name.clone(), dimensions.clone());
    }
    crate::equations::try_issue_eval_function_facts(&mut eval_ctx, ctx.functions.values())
        .map_err(|error| FlattenError::internal(error.to_string()))?;

    resolve_constants_from_tree(tree, &mut eval_ctx)?;
    let mut call_canonicalizer = crate::functions::StructuralFoldCallCanonicalizer::new(
        ctx.functions.values(),
        tree,
        class_index,
    )?;
    collect_component_binding_values(ctx, overlay, &mut eval_ctx, &mut call_canonicalizer)?;

    Ok(eval_ctx)
}

fn reject_overlapping_typed_parameter_caches(ctx: &Context) -> Result<(), FlattenError> {
    let mut conflicts = ctx
        .parameter_values
        .keys()
        .chain(ctx.real_parameter_values.keys())
        .chain(ctx.boolean_parameter_values.keys())
        .chain(ctx.enum_parameter_values.keys())
        .filter(|name| {
            usize::from(ctx.parameter_values.contains_key(*name))
                + usize::from(ctx.real_parameter_values.contains_key(*name))
                + usize::from(ctx.boolean_parameter_values.contains_key(*name))
                + usize::from(ctx.enum_parameter_values.contains_key(*name))
                > 1
        })
        .cloned()
        .collect::<Vec<_>>();
    conflicts.sort();
    conflicts.dedup();
    if conflicts.is_empty() {
        return Ok(());
    }
    Err(FlattenError::internal(format!(
        "typed structural parameter cache ownership overlaps for: {}",
        conflicts.join(", ")
    )))
}

pub(in crate::pipeline) fn resolved_tree_enum_catalog(
    tree: &ClassTree,
) -> Result<rumoca_eval_flat::constant::ResolvedEnumCatalog, FlattenError> {
    let mut declarations = Vec::new();
    for (def_id, qualified_name) in &tree.def_map {
        let Some(class_def) = tree.get_class_by_def_id(*def_id) else {
            continue;
        };
        if !class_def.enum_literals.is_empty() {
            declarations.push(rumoca_eval_flat::constant::ResolvedEnumDeclaration {
                declaration: *def_id,
                type_name: qualified_name.clone(),
                literals: class_def
                    .enum_literals
                    .iter()
                    .map(|literal| literal.ident.text.to_string())
                    .collect(),
            });
        }
    }
    rumoca_eval_flat::constant::ResolvedEnumCatalog::try_from_declarations(declarations)
        .map_err(|error| FlattenError::internal(error.to_string()))
}

fn parameter_is_scalar(ctx: &Context, name: &str) -> bool {
    ctx.array_dimensions.get(name).is_none_or(Vec::is_empty)
}

/// Try to evaluate a simple equation as a structural Boolean assignment.
/// Returns `Some((qualified_var_name, bool_value))` if successful.
pub(crate) fn try_eval_structural_equation(
    equation: &rumoca_ir_ast::Equation,
    prefix: &QualifiedName,
    ctx: &Context,
    eval_ctx: &rumoca_eval_flat::constant::EvalContext,
    call_canonicalizer: &mut crate::functions::StructuralFoldCallCanonicalizer<'_>,
) -> Result<Option<(String, bool)>, FlattenError> {
    let ast::Equation::Simple { lhs, rhs } = equation else {
        return Ok(None);
    };

    let ast::Expression::ComponentReference(cr) = lhs else {
        return Ok(None);
    };
    if cr.parts.len() != 1 || cr.parts[0].subs.is_some() {
        return Ok(None);
    }

    let var_name_part = cr.parts[0].ident.text.as_ref();
    let qualified_var = if prefix.is_empty() {
        var_name_part.to_string()
    } else {
        format!("{}.{}", prefix.to_flat_string(), var_name_part)
    };

    if ctx.boolean_parameter_values.contains_key(&qualified_var) {
        return Ok(None);
    }

    let mut flat_rhs = qualify_expression(rhs, prefix)?;
    call_canonicalizer.canonicalize(&mut flat_rhs)?;
    let Some(bool_val) = crate::constant_eval::evaluate_optional_boolean(
        &flat_rhs,
        eval_ctx,
        "evaluating a structural Boolean equation",
        rhs.span(),
    )?
    else {
        return Ok(None);
    };

    Ok(Some((qualified_var, bool_val)))
}

/// Collect function call names from an equation recursively.
/// Uses the ClassTree to resolve def_ids to fully qualified names.
pub(crate) fn collect_function_calls_from_equation(
    eq: &rumoca_ir_ast::Equation,
    owner_span: rumoca_core::Span,
    calls: &mut crate::functions::FunctionRequests,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> Result<(), FlattenError> {
    match eq {
        ast::Equation::Simple { lhs, rhs } => {
            collect_function_calls_from_expression(lhs, calls, tree, class_index);
            collect_function_calls_from_expression(rhs, calls, tree, class_index);
        }
        ast::Equation::For { indices, equations } => {
            for idx in indices {
                collect_function_calls_from_expression(&idx.range, calls, tree, class_index);
            }
            for inner_eq in equations {
                collect_function_calls_from_equation(
                    inner_eq,
                    owner_span,
                    calls,
                    tree,
                    class_index,
                )?;
            }
        }
        ast::Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_function_calls_from_expression(&block.cond, calls, tree, class_index);
                for inner_eq in &block.eqs {
                    collect_function_calls_from_equation(
                        inner_eq,
                        owner_span,
                        calls,
                        tree,
                        class_index,
                    )?;
                }
            }
            if let Some(else_eqs) = else_block {
                for inner_eq in else_eqs {
                    collect_function_calls_from_equation(
                        inner_eq,
                        owner_span,
                        calls,
                        tree,
                        class_index,
                    )?;
                }
            }
        }
        ast::Equation::When(blocks) => {
            for block in blocks {
                collect_function_calls_from_expression(&block.cond, calls, tree, class_index);
                for inner_eq in &block.eqs {
                    collect_function_calls_from_equation(
                        inner_eq,
                        owner_span,
                        calls,
                        tree,
                        class_index,
                    )?;
                }
            }
        }
        ast::Equation::Connect { lhs, rhs } => {
            // Component references in connect don't contain function calls
            let _ = (lhs, rhs);
        }
        ast::Equation::FunctionCall { comp, args, .. } => {
            calls.insert(resolve_function_request(comp, tree, class_index));
            for arg in args {
                collect_function_calls_from_expression(arg, calls, tree, class_index);
            }
        }
        ast::Equation::Assert {
            condition,
            message,
            level,
        } => {
            collect_function_calls_from_expression(condition, calls, tree, class_index);
            collect_function_calls_from_expression(message, calls, tree, class_index);
            if let Some(level) = level {
                collect_function_calls_from_expression(level, calls, tree, class_index);
            }
        }
        ast::Equation::Empty => {
            return Err(FlattenError::invalid_ast_recovery(
                "Equation::Empty is a parser-recovery node",
                owner_span,
            ));
        }
    }
    Ok(())
}

fn resolve_function_request(
    comp: &rumoca_ir_ast::ComponentReference,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> crate::functions::FunctionRequest {
    crate::functions::FunctionRequest::from_resolved_ast_reference(
        resolve_function_name(comp, tree, class_index),
        comp,
    )
}

/// Collect function call names from an expression recursively.
/// Uses the ClassTree to resolve def_ids to fully qualified names.
pub(crate) fn collect_function_calls_from_expression(
    expr: &ast::Expression,
    calls: &mut crate::functions::FunctionRequests,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) {
    use rumoca_ir_ast::visitor::Visitor;
    use std::ops::ControlFlow;

    struct FunctionCallCollector<'a, 'tree> {
        calls: &'a mut crate::functions::FunctionRequests,
        tree: &'tree ClassTree,
        class_index: &'tree rumoca_ir_ast::ClassDefIndex<'tree>,
    }

    impl Visitor for FunctionCallCollector<'_, '_> {
        fn visit_expr_function_call(
            &mut self,
            comp: &ast::ComponentReference,
            args: &[ast::Expression],
        ) -> ControlFlow<()> {
            self.calls
                .insert(resolve_function_request(comp, self.tree, self.class_index));
            self.visit_each(args, Self::visit_expression)
        }
    }

    let mut collector = FunctionCallCollector {
        calls,
        tree,
        class_index,
    };
    let _visit_outcome = collector.visit_expression(expr);
}

#[cfg(test)]
mod constant_folding_tests;

#[cfg(test)]
mod lookup_scope_tests;
