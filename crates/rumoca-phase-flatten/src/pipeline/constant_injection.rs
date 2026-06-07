use super::*;
use crate::record_constant_arrays::try_extract_record_array_constructor_constant;

mod component_binding_values;

pub(crate) use component_binding_values::collect_component_binding_values;

const NAMED_CONSTRUCTOR_ARG_PREFIX: &str = "__rumoca_named_arg__.";

pub(crate) fn inject_class_extends_constants(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    scope: &str,
    class_def: &ClassDef,
    resolve_context: &str,
    ctx: &mut Context,
) {
    extract_constants_from_class_with_prefix_and_imports(
        tree,
        class_index,
        scope,
        class_def,
        resolve_context,
        ctx,
    );
    for ext in &class_def.extends {
        apply_extends_constants_for_scope(tree, class_index, scope, ext, resolve_context, ctx);
    }
}

pub(crate) fn apply_extends_constants_for_scope(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    scope: &str,
    ext: &rumoca_ir_ast::Extend,
    resolve_context: &str,
    ctx: &mut Context,
) {
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
    );
    extract_extends_chain_constants(
        tree,
        class_index,
        scope,
        &ext.base_name.to_string(),
        resolve_context,
        ctx,
    );
}

pub(crate) fn inject_nested_class_constants(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    comp_scope: &str,
    nested_scope: &str,
    nested_class: &ClassDef,
    resolve_context: &str,
    ctx: &mut Context,
) {
    // `<comp>.<alias>.<const>`
    extract_constants_from_class_with_prefix_and_imports(
        tree,
        class_index,
        nested_scope,
        nested_class,
        resolve_context,
        ctx,
    );
    // `<comp>.<const>`
    extract_constants_from_class_with_prefix_and_imports(
        tree,
        class_index,
        comp_scope,
        nested_class,
        resolve_context,
        ctx,
    );

    for ext in &nested_class.extends {
        apply_extends_constants_for_scope(
            tree,
            class_index,
            nested_scope,
            ext,
            resolve_context,
            ctx,
        );
        apply_extends_constants_for_scope(tree, class_index, comp_scope, ext, resolve_context, ctx);
    }
}

pub(crate) fn inject_alias_component_package_constants(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    comp_scope: &str,
    alias_name: &str,
    alias_comp: &rumoca_ir_ast::Component,
    resolve_context: &str,
    ctx: &mut Context,
) {
    if !alias_name.starts_with(char::is_uppercase) {
        return;
    }

    let Some((alias_class, alias_context)) =
        resolve_alias_component_class(tree, class_index, alias_comp, resolve_context)
    else {
        return;
    };
    if !matches!(
        alias_class.class_type,
        rumoca_core::ClassType::Package
            | rumoca_core::ClassType::Record
            | rumoca_core::ClassType::Class
    ) {
        return;
    }

    let alias_scope = format!("{comp_scope}.{alias_name}");
    extract_constants_from_class_with_prefix_and_imports(
        tree,
        class_index,
        &alias_scope,
        alias_class,
        &alias_context,
        ctx,
    );
    let type_alias = rumoca_core::top_level_last_segment(&alias_context);
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
        );
    }
    if matches!(alias_class.class_type, rumoca_core::ClassType::Package) {
        extract_constants_from_class_with_prefix_and_imports(
            tree,
            class_index,
            comp_scope,
            alias_class,
            &alias_context,
            ctx,
        );
    }
    for ext in &alias_class.extends {
        apply_extends_constants_for_scope(
            tree,
            class_index,
            &alias_scope,
            ext,
            &alias_context,
            ctx,
        );
        if let Some(type_alias_scope) = &type_alias_scope {
            apply_extends_constants_for_scope(
                tree,
                class_index,
                type_alias_scope,
                ext,
                &alias_context,
                ctx,
            );
        }
        if matches!(alias_class.class_type, rumoca_core::ClassType::Package) {
            apply_extends_constants_for_scope(
                tree,
                class_index,
                comp_scope,
                ext,
                &alias_context,
                ctx,
            );
        }
    }
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
    let mut scope = context;
    while let Some(parent_scope) = crate::path_utils::parent_scope(scope) {
        scope = parent_scope;
        let qualified = format!("{}.{}", scope, name);
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
pub(crate) fn extract_ancestor_constants_multi_pass(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    _resolve_context: &str,
    ancestors: &[&ClassDef],
    ctx: &mut Context,
) {
    const MAX_PASSES: usize = 5;
    for _pass in 0..MAX_PASSES {
        let prev = ctx.parameter_values.len()
            + ctx.array_dimensions.len()
            + ctx.boolean_parameter_values.len()
            + ctx.real_parameter_values.len()
            + ctx.enum_parameter_values.len()
            + ctx.constant_values.len();
        for ancestor in ancestors {
            let ancestor_scope = class_scope_name(tree, ancestor);
            for ext in &ancestor.extends {
                extract_extends_modification_constants(
                    tree,
                    class_index,
                    &ancestor_scope,
                    ext,
                    &ancestor_scope,
                    ctx,
                );
            }
            extract_constants_from_class(ancestor, ctx);
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
}

fn class_scope_name(tree: &ClassTree, class_def: &ClassDef) -> String {
    class_def
        .def_id
        .and_then(|def_id| tree.def_map.get(&def_id))
        .cloned()
        .unwrap_or_else(|| class_def.name.text.to_string())
}

/// Extract integer constants and array dimensions from a class definition (MLS §4.5).
pub(crate) fn extract_constants_from_class(class_def: &ClassDef, ctx: &mut Context) {
    for (name, comp) in &class_def.components {
        if !matches!(
            comp.variability,
            rumoca_core::Variability::Constant(_) | rumoca_core::Variability::Parameter(_)
        ) {
            continue;
        }
        let binding =
            comp.binding
                .as_ref()
                .or(if !matches!(comp.start, ast::Expression::Empty { .. }) {
                    Some(&comp.start)
                } else {
                    None
                });
        let Some(expr) = binding else { continue };
        let type_name = comp.type_name.to_string();
        if !ctx.constant_values.contains_key(name)
            && let Some(val) = try_extract_record_array_constructor_constant(expr, ctx, "", name)
        {
            ctx.constant_values.insert(name.clone(), val);
        }
        if !ctx.constant_values.contains_key(name)
            && let Some(val) = try_extract_named_record_constructor_constant(expr, ctx, "", name)
        {
            ctx.constant_values.insert(name.clone(), val);
        }
        if !ctx.constant_values.contains_key(name)
            && let Some(val) = try_eval_const_flat_expr_with_scope(expr, ctx, "")
        {
            ctx.constant_values.insert(name.clone(), val);
        }
        // Integer constants
        if type_name == "Integer"
            && !ctx.parameter_values.contains_key(name)
            && let Some(val) = try_eval_const_integer_with_scope(expr, ctx, "")
        {
            ctx.parameter_values.insert(name.clone(), val);
        }
        // Boolean constants
        if type_name == "Boolean"
            && !ctx.boolean_parameter_values.contains_key(name)
            && let Some(val) = try_eval_const_boolean_with_scope(expr, ctx, "")
        {
            ctx.boolean_parameter_values.insert(name.clone(), val);
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

pub(crate) fn lookup_component_ref_with_scope<V: Clone + PartialEq>(
    reference: &rumoca_ir_ast::ComponentReference,
    scope: &QualifiedName,
    map: &rustc_hash::FxHashMap<String, V>,
) -> Option<V> {
    let name = QualifiedName::from_component_reference(reference);
    lookup_with_qualified_scope(&name, scope, map)
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
    match expr {
        ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token,
            ..
        } => token.text.as_ref().parse().ok(),
        ast::Expression::ComponentReference(cr) => {
            let scope_path = QualifiedName::from_dotted(scope);
            lookup_component_ref_with_scope(cr, &scope_path, &ctx.parameter_values)
        }
        ast::Expression::Unary {
            rhs,
            op: OpUnary::Minus,
            ..
        } => try_eval_const_integer_with_scope(rhs, ctx, scope).map(|v| -v),
        ast::Expression::Parenthesized { inner, .. } => {
            try_eval_const_integer_with_scope(inner, ctx, scope)
        }
        ast::Expression::Binary { lhs, rhs, op, .. } => {
            let l = try_eval_const_integer_with_scope(lhs, ctx, scope)?;
            let r = try_eval_const_integer_with_scope(rhs, ctx, scope)?;
            rumoca_core::eval_ast_integer_binary(op, l, r)
        }
        ast::Expression::FunctionCall { comp, args, .. } => {
            eval_const_integer_function_with_scope(comp, args, ctx, scope)
        }
        // MLS §3.6: if-expressions for conditional constant evaluation
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let selected = select_const_if_branch(branches, else_branch, ctx, scope)?;
            try_eval_const_integer_with_scope(selected, ctx, scope)
        }
        _ => None,
    }
}

/// Scope-aware constant real evaluation.
pub(crate) fn try_eval_const_real_with_scope(
    expr: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<f64> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedReal,
            token,
            ..
        } => token.text.as_ref().parse().ok(),
        ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token,
            ..
        } => token.text.as_ref().parse::<i64>().ok().map(|v| v as f64),
        ast::Expression::ComponentReference(cr) => {
            let scope_path = QualifiedName::from_dotted(scope);
            lookup_component_ref_with_scope(cr, &scope_path, &ctx.real_parameter_values).or_else(
                || {
                    lookup_component_ref_with_scope(cr, &scope_path, &ctx.parameter_values)
                        .map(|v| v as f64)
                },
            )
        }
        ast::Expression::Unary {
            rhs,
            op: OpUnary::Minus,
            ..
        } => try_eval_const_real_with_scope(rhs, ctx, scope).map(|v| -v),
        ast::Expression::Parenthesized { inner, .. } => {
            try_eval_const_real_with_scope(inner, ctx, scope)
        }
        ast::Expression::Binary { lhs, rhs, op, .. } => {
            let l = try_eval_const_real_with_scope(lhs, ctx, scope)?;
            let r = try_eval_const_real_with_scope(rhs, ctx, scope)?;
            match op {
                OpBinary::Add => Some(l + r),
                OpBinary::Sub => Some(l - r),
                OpBinary::Mul => Some(l * r),
                OpBinary::Div => (r.abs() > f64::EPSILON).then_some(l / r),
                OpBinary::Exp => Some(l.powf(r)),
                _ => None,
            }
        }
        ast::Expression::FunctionCall { comp, args, .. } => {
            eval_const_real_function_with_scope(comp, args, ctx, scope)
        }
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let selected = select_const_if_branch(branches, else_branch, ctx, scope)?;
            try_eval_const_real_with_scope(selected, ctx, scope)
        }
        _ => None,
    }
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
        "sign" if args.len() == 1 => eval(&args[0]).map(f64::signum),
        "sqrt" if args.len() == 1 => eval(&args[0]).map(f64::sqrt),
        "integer" if args.len() == 1 => Some(eval(&args[0])?.trunc()),
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
        } => try_eval_const_array_expr(elements, *is_matrix, ctx, scope),
        ast::Expression::Tuple { elements, .. } => try_eval_const_tuple_expr(elements, ctx, scope),
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
            try_eval_const_function_call_expr(comp, args, ctx, scope)
        }
        ast::Expression::FieldAccess { base, field, .. } => {
            if let Some(value) =
                try_eval_const_field_access_expr(base, field, expr.span(), ctx, scope)
            {
                return Some(value);
            }
            Some(rumoca_core::Expression::FieldAccess {
                base: Box::new(try_eval_const_flat_expr_with_scope(base, ctx, scope)?),
                field: field.clone(),
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
    if let Some(value) = lookup_with_qualified_scope(&name, &scope_path, &ctx.enum_parameter_values)
    {
        return Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(value),
            subscripts: vec![],
            span,
        });
    }
    None
}

pub(crate) fn try_extract_named_record_constructor_constant(
    expr: &ast::Expression,
    ctx: &mut Context,
    scope: &str,
    full_name: &str,
) -> Option<rumoca_core::Expression> {
    let (ctor_name, named_fields) = extract_named_record_constructor_fields(expr)?;

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
        .map(named_record_constructor_arg)
        .collect();
    Some(rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new(ctor_name),
        args: ctor_args,
        is_constructor: true,
        span: expr.span(),
    })
}

fn named_record_constructor_arg(
    (field_name, value): (String, rumoca_core::Expression),
) -> rumoca_core::Expression {
    let span = value.span().unwrap_or(rumoca_core::Span::DUMMY);
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new(format!("{NAMED_CONSTRUCTOR_ARG_PREFIX}{field_name}")),
        args: vec![value],
        is_constructor: true,
        span,
    }
}

fn extract_named_record_constructor_fields(
    expr: &ast::Expression,
) -> Option<(String, Vec<(String, ast::Expression)>)> {
    match expr {
        ast::Expression::FunctionCall { comp, args, .. } => {
            if args.is_empty() {
                return None;
            }
            let ctor_name = QualifiedName::from_component_reference(comp).to_flat_string();
            let mut named_fields = Vec::new();
            for arg in args {
                let ast::Expression::NamedArgument { name, value, .. } = arg else {
                    return None;
                };
                named_fields.push((name.text.to_string(), value.as_ref().clone()));
            }
            Some((ctor_name, named_fields))
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
            let mut named_fields = Vec::new();
            for modification in modifications {
                match modification {
                    ast::Expression::NamedArgument { name, value, .. } => {
                        named_fields.push((name.text.to_string(), value.as_ref().clone()));
                    }
                    ast::Expression::Modification { target, value, .. } => {
                        let field_name = single_target_field_name(target)?;
                        named_fields.push((field_name, value.as_ref().clone()));
                    }
                    _ => return None,
                }
            }
            Some((ctor_name, named_fields))
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
    match value {
        rumoca_core::Expression::Literal {
            value: Literal::Integer(v),
            ..
        } => {
            insert_with_prefix(
                &mut ctx.parameter_values,
                record_prefix,
                field_name,
                field_full_name,
                *v,
            );
        }
        rumoca_core::Expression::Literal {
            value: Literal::Real(v),
            ..
        } if v.is_finite() => {
            insert_with_prefix(
                &mut ctx.real_parameter_values,
                record_prefix,
                field_name,
                field_full_name,
                *v,
            );
        }
        rumoca_core::Expression::Literal {
            value: Literal::Boolean(v),
            ..
        } => {
            insert_with_prefix(
                &mut ctx.boolean_parameter_values,
                record_prefix,
                field_name,
                field_full_name,
                *v,
            );
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            insert_with_prefix(
                &mut ctx.enum_parameter_values,
                record_prefix,
                field_name,
                field_full_name,
                name.as_str().to_string(),
            );
        }
        _ => {}
    }
}

pub(crate) fn try_eval_const_component_ref_expr(
    cr: &rumoca_ir_ast::ComponentReference,
    ctx: &Context,
    scope: &str,
) -> Option<rumoca_core::Expression> {
    if cr.parts.iter().any(|part| {
        part.subs
            .as_ref()
            .is_some_and(|subscripts| !subscripts.is_empty())
    }) {
        let Ok(lowered) =
            crate::ast_lower::expression_from_ast(&ast::Expression::ComponentReference(cr.clone()))
        else {
            return None;
        };
        let substituted = crate::postprocess::substitute_known_constants_expr(
            lowered.clone(),
            ctx,
            &rustc_hash::FxHashSet::default(),
            &std::collections::HashSet::new(),
            scope,
        );
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
            span: rumoca_core::Span::DUMMY,
        });
    }
    if let Some(v) = lookup_with_qualified_scope(&name, &scope_path, &ctx.parameter_values) {
        return Some(rumoca_core::Expression::Literal {
            value: Literal::Integer(v),
            span: rumoca_core::Span::DUMMY,
        });
    }
    if let Some(v) = lookup_with_qualified_scope(&name, &scope_path, &ctx.boolean_parameter_values)
    {
        return Some(rumoca_core::Expression::Literal {
            value: Literal::Boolean(v),
            span: rumoca_core::Span::DUMMY,
        });
    }
    if let Some(enum_name) =
        lookup_with_qualified_scope(&name, &scope_path, &ctx.enum_parameter_values)
    {
        return Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(enum_name),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        });
    }
    let resolved =
        resolve_component_ref_through_constant_aliases(&name, ctx, scope).unwrap_or(name);
    let resolved_text = resolved.to_flat_string();
    try_eval_resolved_const_ref(&resolved_text, ctx).or_else(|| {
        looks_like_enum_literal_path(&resolved_text).then(|| rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(resolved_text),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        })
    })
}

fn try_eval_resolved_const_ref(name: &str, ctx: &Context) -> Option<rumoca_core::Expression> {
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
            span: rumoca_core::Span::DUMMY,
        });
    }
    if let Some(v) = lookup_with_scope(name, "", &ctx.parameter_values) {
        return Some(rumoca_core::Expression::Literal {
            value: Literal::Integer(v),
            span: rumoca_core::Span::DUMMY,
        });
    }
    if let Some(v) = lookup_with_scope(name, "", &ctx.boolean_parameter_values) {
        return Some(rumoca_core::Expression::Literal {
            value: Literal::Boolean(v),
            span: rumoca_core::Span::DUMMY,
        });
    }
    lookup_with_scope(name, "", &ctx.enum_parameter_values).map(|enum_name| {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(enum_name),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }
    })
}

fn constant_expr_preserves_array_shape(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::Array { .. }
            | rumoca_core::Expression::Tuple { .. }
            | rumoca_core::Expression::Range { .. }
            | rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Fill
                    | rumoca_core::BuiltinFunction::Zeros
                    | rumoca_core::BuiltinFunction::Ones,
                ..
            }
    )
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
    ctx: &Context,
    scope: &str,
) -> Option<rumoca_core::Expression> {
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
            span: rumoca_core::Span::DUMMY,
        });
    }

    if let Some(function) = rumoca_core::BuiltinFunction::from_name(short_name) {
        return Some(rumoca_core::Expression::BuiltinCall {
            function,
            args: evaluated_args,
            span: rumoca_core::Span::DUMMY,
        });
    }

    if let Some(function) =
        rumoca_core::BuiltinFunction::from_name(&short_name.to_ascii_lowercase())
    {
        return Some(rumoca_core::Expression::BuiltinCall {
            function,
            args: evaluated_args,
            span: rumoca_core::Span::DUMMY,
        });
    }

    Some(rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            textual_name,
            core_component_reference_from_ast(comp),
        ),
        args: evaluated_args,
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    })
}

fn core_component_reference_from_ast(
    comp: &rumoca_ir_ast::ComponentReference,
) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: comp.local,
        span: comp.span,
        parts: comp
            .parts
            .iter()
            .map(|part| rumoca_core::ComponentRefPart {
                ident: part.ident.text.to_string(),
                span: comp.span,
                subs: Vec::new(),
            })
            .collect(),
        def_id: comp.def_id,
    }
}

pub(crate) fn try_eval_const_array_expr(
    elements: &[ast::Expression],
    is_matrix: bool,
    ctx: &Context,
    scope: &str,
) -> Option<rumoca_core::Expression> {
    let mut out = Vec::with_capacity(elements.len());
    for el in elements {
        out.push(try_eval_const_flat_expr_with_scope(el, ctx, scope)?);
    }
    Some(rumoca_core::Expression::Array {
        elements: out,
        is_matrix,
        span: rumoca_core::Span::DUMMY,
    })
}

pub(crate) fn try_eval_const_tuple_expr(
    elements: &[ast::Expression],
    ctx: &Context,
    scope: &str,
) -> Option<rumoca_core::Expression> {
    let mut out = Vec::with_capacity(elements.len());
    for el in elements {
        out.push(try_eval_const_flat_expr_with_scope(el, ctx, scope)?);
    }
    Some(rumoca_core::Expression::Tuple {
        elements: out,
        span: rumoca_core::Span::DUMMY,
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
            if y != 0 {
                Some(((x % y) + y) % y)
            } else {
                None
            }
        }
        "rem" if args.len() == 2 => {
            let (x, y) = (eval(&args[0])?, eval(&args[1])?);
            if y != 0 { Some(x % y) } else { None }
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
    match expr {
        ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::Bool,
            token,
            ..
        } => match token.text.as_ref() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },
        ast::Expression::ComponentReference(cr) => {
            let name = QualifiedName::from_component_reference(cr).to_flat_string();
            lookup_with_scope(&name, scope, &ctx.boolean_parameter_values)
        }
        ast::Expression::Unary {
            op: OpUnary::Not,
            rhs,
            ..
        } => try_eval_const_boolean_with_scope(rhs, ctx, scope).map(|v| !v),
        ast::Expression::Parenthesized { inner, .. } => {
            try_eval_const_boolean_with_scope(inner, ctx, scope)
        }
        ast::Expression::Binary { op, lhs, rhs, .. } => match op {
            OpBinary::And => eval_const_boolean_and(lhs, rhs, ctx, scope),
            OpBinary::Or => eval_const_boolean_or(lhs, rhs, ctx, scope),
            // Integer/Real comparisons for conditional parameters (MLS §3.5)
            OpBinary::Eq => eval_const_equality_with_scope(lhs, rhs, ctx, scope, true),
            OpBinary::Neq => eval_const_equality_with_scope(lhs, rhs, ctx, scope, false),
            OpBinary::Lt => eval_const_ordering_with_scope(lhs, rhs, ctx, scope, |l, r| l < r),
            OpBinary::Le => eval_const_ordering_with_scope(lhs, rhs, ctx, scope, |l, r| l <= r),
            OpBinary::Gt => eval_const_ordering_with_scope(lhs, rhs, ctx, scope, |l, r| l > r),
            OpBinary::Ge => eval_const_ordering_with_scope(lhs, rhs, ctx, scope, |l, r| l >= r),
            _ => None,
        },
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let selected = select_const_if_branch(branches, else_branch, ctx, scope)?;
            try_eval_const_boolean_with_scope(selected, ctx, scope)
        }
        _ => None,
    }
}

fn eval_const_ordering_with_scope(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    ctx: &Context,
    scope: &str,
    cmp: impl FnOnce(f64, f64) -> bool,
) -> Option<bool> {
    if let (Some(l), Some(r)) = (
        try_eval_const_integer_with_scope(lhs, ctx, scope),
        try_eval_const_integer_with_scope(rhs, ctx, scope),
    ) {
        return Some(cmp(l as f64, r as f64));
    }
    let l = try_eval_const_real_with_scope(lhs, ctx, scope)?;
    let r = try_eval_const_real_with_scope(rhs, ctx, scope)?;
    Some(cmp(l, r))
}

fn eval_const_boolean_and(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<bool> {
    let lhs_value = try_eval_const_boolean_with_scope(lhs, ctx, scope);
    if lhs_value == Some(false) {
        return Some(false);
    }
    let rhs_value = try_eval_const_boolean_with_scope(rhs, ctx, scope);
    if rhs_value == Some(false) {
        return Some(false);
    }
    lhs_value.zip(rhs_value).map(|(lhs, rhs)| lhs && rhs)
}

fn eval_const_boolean_or(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<bool> {
    let lhs_value = try_eval_const_boolean_with_scope(lhs, ctx, scope);
    if lhs_value == Some(true) {
        return Some(true);
    }
    let rhs_value = try_eval_const_boolean_with_scope(rhs, ctx, scope);
    if rhs_value == Some(true) {
        return Some(true);
    }
    lhs_value.zip(rhs_value).map(|(lhs, rhs)| lhs || rhs)
}

pub(crate) fn eval_const_equality_with_scope(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    ctx: &Context,
    scope: &str,
    is_eq: bool,
) -> Option<bool> {
    if let (Some(l), Some(r)) = (
        try_eval_const_integer_with_scope(lhs, ctx, scope),
        try_eval_const_integer_with_scope(rhs, ctx, scope),
    ) {
        return Some(if is_eq { l == r } else { l != r });
    }

    let lhs_enum = try_eval_const_enum_with_scope(lhs, ctx, scope)?;
    let rhs_enum = try_eval_const_enum_with_scope(rhs, ctx, scope)?;
    let equal = rumoca_core::enum_values_equal(&lhs_enum, &rhs_enum);
    Some(if is_eq { equal } else { !equal })
}

/// Scope-aware constant enum evaluation.
///
/// Supports direct enum literals (`Type.Literal`), references to enum-valued
/// constants/parameters in scope, and if-expressions with constant conditions.
pub(crate) fn try_eval_const_enum_with_scope(
    expr: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<String> {
    match expr {
        ast::Expression::ComponentReference(cr) => {
            let name = QualifiedName::from_component_reference(cr).to_flat_string();

            lookup_with_scope(&name, scope, &ctx.enum_parameter_values).or_else(|| {
                // Enum literals are valid constant references even when no
                // enum-valued parameter exists at the same path.
                looks_like_enum_literal_path(&name).then_some(name)
            })
        }
        ast::Expression::Parenthesized { inner, .. } => {
            try_eval_const_enum_with_scope(inner, ctx, scope)
        }
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let selected = select_const_if_branch(branches, else_branch, ctx, scope)?;
            try_eval_const_enum_with_scope(selected, ctx, scope)
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
    let arr_name = match array_expr {
        ast::Expression::ComponentReference(cr) => {
            QualifiedName::from_component_reference(cr).to_flat_string()
        }
        _ => return None,
    };
    let dims = lookup_size_array_dims_with_scope(&arr_name, scope, ctx)?;
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
        ast::Expression::Array { elements, .. } => Some(vec![elements.len() as i64]),
        ast::Expression::Range { .. } => {
            infer_dims_via_eval_ast(expr, ctx, scope).and_then(|dims| {
                dims.into_iter()
                    .map(|dim| i64::try_from(dim).ok())
                    .collect::<Option<Vec<_>>>()
            })
        }
        ast::Expression::FunctionCall { comp, args, .. } => {
            let fn_name = comp
                .parts
                .last()
                .map(|p| p.ident.text.as_ref())
                .unwrap_or("");
            if fn_name == "fill" && args.len() >= 2 {
                let dims: Option<Vec<i64>> = args[1..]
                    .iter()
                    .map(|a| try_eval_const_integer_with_scope(a, ctx, scope))
                    .collect();
                dims
            } else {
                None
            }
        }
        _ => None,
    }
}

fn infer_dims_via_eval_ast(
    expr: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<Vec<usize>> {
    let mut eval_ctx = rumoca_eval_ast::eval::TypeCheckEvalContext::new();
    for (name, value) in &ctx.parameter_values {
        eval_ctx.add_integer(name, *value);
    }
    for (name, value) in &ctx.real_parameter_values {
        eval_ctx.add_real(name, *value);
    }
    eval_ctx
        .booleans
        .extend(ctx.boolean_parameter_values.clone());
    eval_ctx.enums.extend(ctx.enum_parameter_values.clone());
    for (name, dims) in &ctx.array_dimensions {
        let dims = dims
            .iter()
            .map(|dim| usize::try_from(*dim).ok())
            .collect::<Option<Vec<_>>>()?;
        eval_ctx.add_dimensions(name, dims);
    }
    rumoca_eval_ast::eval::infer_dimensions_from_binding_with_scope(expr, &eval_ctx, scope)
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
) -> Result<(), FlattenError> {
    let eval_ctx = build_structural_eval_context(ctx, overlay, tree)?;

    // Scan all class instances for structural Boolean equations
    for (_def_id, class_data) in &overlay.classes {
        let prefix = &class_data.qualified_name;
        for eq_entry in &class_data.equations {
            if let Some((var_name, bool_val)) =
                try_eval_structural_equation(&eq_entry.equation, prefix, ctx, &eval_ctx)?
            {
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

/// Build an evaluation context populated with known parameters, functions, constants,
/// and component bindings from the overlay.
pub(crate) fn build_structural_eval_context(
    ctx: &Context,
    overlay: &InstanceOverlay,
    tree: &ClassTree,
) -> Result<rumoca_eval_flat::constant::EvalContext, FlattenError> {
    use rumoca_eval_flat::constant::{EvalContext, Value};

    let parameter_capacity = ctx.parameter_values.len()
        + ctx.real_parameter_values.len()
        + ctx.boolean_parameter_values.len();
    let mut eval_ctx = EvalContext::with_capacity(parameter_capacity, 0, ctx.functions.len() * 2);
    for (name, value) in &ctx.parameter_values {
        eval_ctx.add_parameter(name.clone(), Value::Integer(*value));
    }
    for (name, value) in &ctx.real_parameter_values {
        eval_ctx.add_parameter(name.clone(), Value::Real(*value));
    }
    for (name, value) in &ctx.boolean_parameter_values {
        eval_ctx.add_parameter(name.clone(), Value::Bool(*value));
    }
    for func in ctx.functions.values() {
        eval_ctx.add_function(func.clone());
    }

    resolve_constants_from_tree(tree, &mut eval_ctx)?;
    collect_component_binding_values(overlay, &mut eval_ctx)?;

    Ok(eval_ctx)
}

/// Try to evaluate a simple equation as a structural Boolean assignment.
/// Returns `Some((qualified_var_name, bool_value))` if successful.
pub(crate) fn try_eval_structural_equation(
    equation: &rumoca_ir_ast::Equation,
    prefix: &QualifiedName,
    ctx: &Context,
    eval_ctx: &rumoca_eval_flat::constant::EvalContext,
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

    let flat_rhs = qualify_expression(rhs, prefix)?;
    let val = match rumoca_eval_flat::constant::eval_expr(&flat_rhs, eval_ctx) {
        Ok(v) => v,
        Err(_e) => {
            return Ok(None);
        }
    };
    let Some(bool_val) = val.as_bool() else {
        return Ok(None);
    };

    Ok(Some((qualified_var, bool_val)))
}

/// Collect function call names from an equation recursively.
/// Uses the ClassTree to resolve def_ids to fully qualified names.
pub(crate) fn collect_function_calls_from_equation(
    eq: &rumoca_ir_ast::Equation,
    calls: &mut crate::functions::FunctionRequests,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) {
    match eq {
        ast::Equation::Simple { lhs, rhs } => {
            collect_function_calls_from_expression(lhs, calls, tree, class_index);
            collect_function_calls_from_expression(rhs, calls, tree, class_index);
        }
        ast::Equation::For { indices, equations } => {
            // Check the range expressions for function calls
            for idx in indices {
                collect_function_calls_from_expression(&idx.range, calls, tree, class_index);
            }
            for inner_eq in equations {
                collect_function_calls_from_equation(inner_eq, calls, tree, class_index);
            }
        }
        ast::Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_function_calls_from_expression(&block.cond, calls, tree, class_index);
                for inner_eq in &block.eqs {
                    collect_function_calls_from_equation(inner_eq, calls, tree, class_index);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner_eq in else_eqs {
                    collect_function_calls_from_equation(inner_eq, calls, tree, class_index);
                }
            }
        }
        ast::Equation::When(blocks) => {
            for block in blocks {
                collect_function_calls_from_expression(&block.cond, calls, tree, class_index);
                for inner_eq in &block.eqs {
                    collect_function_calls_from_equation(inner_eq, calls, tree, class_index);
                }
            }
        }
        ast::Equation::Connect { lhs, rhs } => {
            // Component references in connect don't contain function calls
            let _ = (lhs, rhs);
        }
        ast::Equation::FunctionCall { comp, args } => {
            calls.insert(resolve_function_request(comp, tree, class_index));
            for arg in args {
                collect_function_calls_from_expression(arg, calls, tree, class_index);
            }
        }
        ast::Equation::Assert {
            condition, message, ..
        } => {
            collect_function_calls_from_expression(condition, calls, tree, class_index);
            collect_function_calls_from_expression(message, calls, tree, class_index);
        }
        ast::Equation::Empty => {}
    }
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

/// Resolve a function call's component reference to its fully qualified name.
/// Uses def_id from import resolution (contract from resolve phase).
pub(crate) fn resolve_function_name(
    comp: &rumoca_ir_ast::ComponentReference,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> String {
    // First get the textual name from parts - this is the full reference as written
    let textual_name = QualifiedName::from_component_reference(comp).to_flat_string();

    // Use def_id for resolved qualified name (from resolve phase)
    // If def_id resolves to a package/class, append remaining path parts and
    // check whether that yields a function (needed for aliases like Medium.foo()).
    if let Some(def_id) = comp.def_id
        && let Some(base_name) = tree.def_map.get(&def_id)
    {
        if comp.parts.len() > 1 {
            let suffix = comp.parts[1..]
                .iter()
                .map(|part| part.ident.text.to_string())
                .collect::<Vec<_>>()
                .join(".");
            let candidate = format!("{base_name}.{suffix}");
            if let Some(class_def) = class_index.get_by_qualified_name(&candidate)
                && class_def.class_type == rumoca_core::ClassType::Function
            {
                return candidate;
            }
        }

        // Also allow direct function def_id resolution.
        if let Some(class_def) = class_index.get_by_qualified_name(base_name)
            && class_def.class_type == rumoca_core::ClassType::Function
        {
            return base_name.clone();
        }
    }

    #[cfg(feature = "tracing")]
    if comp.def_id.is_some() {
        tracing::warn!(
            "Function call has def_id {:?} but not found in def_map: {}",
            comp.def_id,
            comp
        );
    } else {
        tracing::debug!("Function call without def_id: {}", textual_name);
    }
    textual_name
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
    let _ = collector.visit_expression(expr);
}

/// Context for flattening.
pub(crate) struct Context {
    /// Parameter values for evaluating for-equation ranges (name -> integer value).
    pub parameter_values: rustc_hash::FxHashMap<String, i64>,
    /// Real parameter values for evaluating function arguments (name -> real value).
    pub real_parameter_values: rustc_hash::FxHashMap<String, f64>,
    /// Boolean parameter values for evaluating if-equation conditions.
    pub boolean_parameter_values: rustc_hash::FxHashMap<String, bool>,
    /// Enumeration parameter values (name -> qualified enum literal string).
    pub enum_parameter_values: rustc_hash::FxHashMap<String, String>,
    /// General constant expression values (scalars/arrays) extracted from
    /// class/package constants and redeclare/extends modifications.
    pub constant_values: rustc_hash::FxHashMap<String, rumoca_core::Expression>,
    /// Qualified declaration names keyed by semantic target DefId.
    pub target_def_names: rustc_hash::FxHashMap<rumoca_core::DefId, String>,
    /// Fully qualified constant names explicitly modified by extends clauses.
    /// These must not be overwritten by inherited declaration defaults.
    pub(crate) modified_constant_keys: rustc_hash::FxHashSet<String>,
    /// Parameter/constant variable keys materialized from the instantiated flat model.
    /// Injected class defaults must not overwrite these effective instance values.
    pub flat_parameter_constant_keys: rustc_hash::FxHashSet<String>,
    /// Array dimensions for evaluating size() calls (name -> dims).
    pub array_dimensions: rustc_hash::FxHashMap<String, Vec<i64>>,
    /// Parameters marked with annotation(Evaluate=true) or declared final (MLS §18.3).
    /// Only these structural parameters can be used for compile-time branch selection.
    pub structural_params: std::collections::HashSet<String>,
    /// Parameters explicitly declared with `fixed = false` and without
    /// `Evaluate=true`. These must not be folded for structural branch
    /// selection, even when a provisional value is available.
    pub non_structural_params: std::collections::HashSet<String>,
    /// User-defined function definitions for compile-time evaluation (MLS §12.3).
    /// Functions are looked up by qualified name during constant expression evaluation.
    pub functions: rustc_hash::FxHashMap<String, Function>,
    /// Record aliases for resolving field access through record parameter bindings.
    /// Maps record parameter component path -> alias target component path (MLS §7.2.3).
    /// Example: "battery2.cellData" -> "cellData2" allows resolving
    /// "battery2.cellData.nRC" to "cellData2.nRC".
    pub record_aliases:
        rustc_hash::FxHashMap<rumoca_core::ComponentPath, rumoca_core::ComponentPath>,
    /// VCG isRoot results: path -> true if this node is the root of its component (MLS §9.4).
    pub vcg_is_root: rustc_hash::FxHashMap<String, bool>,
    /// VCG rooted results: path -> true if this node is on the "rooted" side (MLS §9.4).
    pub vcg_rooted: rustc_hash::FxHashMap<String, bool>,
    /// Cardinality counts: connector path -> number of connect() statements referencing it (MLS §3.7.2.3).
    pub cardinality_counts: rustc_hash::FxHashMap<String, i64>,
    /// Lazy base evaluator for flatten expression fallback evaluation.
    /// This is built once per flatten context after structural lookup stabilizes.
    pub(crate) eval_fallback_context: std::cell::OnceCell<rumoca_eval_flat::constant::EvalContext>,
    /// Current import map for the class instance being processed (MLS §13.2).
    /// Set before processing each class instance's equations, cleared after.
    pub current_imports: crate::qualify::ImportMap,
    /// Set of DefIds that correspond to class definitions in the current tree.
    /// Used by qualification to distinguish class/type references from components.
    pub class_def_ids: std::sync::Arc<rustc_hash::FxHashSet<rumoca_core::DefId>>,
    /// Canonical class scope path for the class instance currently being flattened.
    /// Derived from `def_map` via the owning class DefId.
    pub current_class_scope_path: Option<String>,
}

#[cfg(test)]
mod constant_folding_tests;

#[cfg(test)]
mod lookup_scope_tests;
