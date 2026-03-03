use super::*;

pub(crate) fn inject_class_extends_constants(
    tree: &ClassTree,
    scope: &str,
    class_def: &ClassDef,
    resolve_context: &str,
    ctx: &mut Context,
) {
    extract_constants_from_class_with_prefix(scope, class_def, ctx);
    for ext in &class_def.extends {
        apply_extends_constants_for_scope(tree, scope, ext, resolve_context, ctx);
    }
}

pub(crate) fn apply_extends_constants_for_scope(
    tree: &ClassTree,
    scope: &str,
    ext: &rumoca_ir_ast::Extend,
    resolve_context: &str,
    ctx: &mut Context,
) {
    extract_extends_modification_constants(tree, scope, ext, resolve_context, ctx);
    if let Some(base_qname) =
        resolve_extends_base_qname(tree, &ext.base_name.to_string(), resolve_context)
        && base_qname != scope
    {
        extract_extends_modification_constants(tree, &base_qname, ext, &base_qname, ctx);
    }
    extract_extends_redeclare_package_constants(tree, scope, ext, resolve_context, ctx);
    extract_extends_chain_constants(
        tree,
        scope,
        &ext.base_name.to_string(),
        resolve_context,
        ctx,
    );
}

pub(crate) fn inject_nested_class_constants(
    tree: &ClassTree,
    comp_scope: &str,
    nested_scope: &str,
    nested_class: &ClassDef,
    resolve_context: &str,
    ctx: &mut Context,
) {
    // `<comp>.<alias>.<const>`
    extract_constants_from_class_with_prefix(nested_scope, nested_class, ctx);
    // `<comp>.<const>`
    extract_constants_from_class_with_prefix(comp_scope, nested_class, ctx);

    for ext in &nested_class.extends {
        apply_extends_constants_for_scope(tree, nested_scope, ext, resolve_context, ctx);
        apply_extends_constants_for_scope(tree, comp_scope, ext, resolve_context, ctx);
    }
}

pub(crate) fn inject_alias_component_package_constants(
    tree: &ClassTree,
    comp_scope: &str,
    alias_name: &str,
    alias_comp: &rumoca_ir_ast::Component,
    resolve_context: &str,
    ctx: &mut Context,
) {
    if !alias_name.starts_with(char::is_uppercase) {
        return;
    }

    let alias_type_name = alias_comp.type_name.to_string();
    let alias_class_with_context = alias_comp
        .type_def_id
        .or(alias_comp.type_name.def_id)
        .and_then(|def_id| {
            let class = tree.get_class_by_def_id(def_id)?;
            let context = tree
                .def_map
                .get(&def_id)
                .cloned()
                .unwrap_or_else(|| alias_type_name.clone());
            Some((class, context))
        })
        .or_else(|| {
            let (class, resolved_name) =
                resolve_class_in_scope(tree, &alias_type_name, resolve_context);
            class.map(|class| {
                (
                    class,
                    resolved_name.unwrap_or_else(|| alias_type_name.clone()),
                )
            })
        })
        .or_else(|| {
            tree.get_class_by_qualified_name(&alias_type_name)
                .map(|class| (class, alias_type_name.clone()))
        });
    let Some((alias_class, alias_context)) = alias_class_with_context else {
        return;
    };
    if !matches!(alias_class.class_type, rumoca_ir_ast::ClassType::Package) {
        return;
    }

    let alias_scope = format!("{comp_scope}.{alias_name}");
    extract_constants_from_class_with_prefix(&alias_scope, alias_class, ctx);
    extract_constants_from_class_with_prefix(comp_scope, alias_class, ctx);
    for ext in &alias_class.extends {
        apply_extends_constants_for_scope(tree, &alias_scope, ext, &alias_context, ctx);
        apply_extends_constants_for_scope(tree, comp_scope, ext, &alias_context, ctx);
    }
}

/// Collect a class and all its ancestors via extends chains.
pub(crate) fn collect_ancestor_classes<'a>(
    tree: &'a ClassTree,
    class_name: &str,
) -> Vec<&'a ClassDef> {
    let mut result = Vec::new();
    let mut queue: Vec<(String, String)> = vec![(class_name.to_string(), class_name.to_string())];
    let mut visited = std::collections::HashSet::new();
    while let Some((name, context)) = queue.pop() {
        if !visited.insert(name.clone()) {
            continue;
        }
        let (class_def, resolved_qname) = resolve_class_in_scope(tree, &name, &context);
        let Some(class_def) = class_def else { continue };
        let qname = resolved_qname.unwrap_or_else(|| name.clone());
        for ext in &class_def.extends {
            queue.push((ext.base_name.to_string(), qname.clone()));
        }
        result.push(class_def);
    }
    result
}

/// Resolve a potentially relative class name using scope-based lookup.
pub(crate) fn resolve_class_in_scope<'a>(
    tree: &'a ClassTree,
    name: &str,
    context: &str,
) -> (Option<&'a ClassDef>, Option<String>) {
    if let Some(cls) = tree.get_class_by_qualified_name(name) {
        return (Some(cls), Some(name.to_string()));
    }
    if !context.is_empty() {
        let qualified = format!("{}.{}", context, name);
        if let Some(cls) = tree.get_class_by_qualified_name(&qualified) {
            return (Some(cls), Some(qualified));
        }
    }
    let mut scope = context;
    while let Some(parent_scope) = crate::path_utils::parent_scope(scope) {
        scope = parent_scope;
        let qualified = format!("{}.{}", scope, name);
        if let Some(cls) = tree.get_class_by_qualified_name(&qualified) {
            return (Some(cls), Some(qualified));
        }
    }
    (None, None)
}

pub(crate) fn resolve_extends_base_qname(
    tree: &ClassTree,
    base_name: &str,
    resolve_context: &str,
) -> Option<String> {
    if tree.get_class_by_qualified_name(base_name).is_some() {
        return Some(base_name.to_string());
    }

    let mut imports = crate::qualify::ImportMap::default();
    crate::qualify::collect_lexical_package_aliases(tree, resolve_context, &mut imports);
    if let Some(expanded_name) = expand_import_alias_prefix(base_name, &imports)
        && tree.get_class_by_qualified_name(&expanded_name).is_some()
    {
        return Some(expanded_name);
    }

    resolve_class_in_scope(tree, base_name, resolve_context).1
}

fn expand_import_alias_prefix(name: &str, imports: &crate::qualify::ImportMap) -> Option<String> {
    let (head, tail) = name.split_once('.').unwrap_or((name, ""));
    let replacement = imports.get(head)?;
    if tail.is_empty() {
        Some(replacement.clone())
    } else {
        Some(format!("{replacement}.{tail}"))
    }
}

/// Multi-pass extraction of integer constants and array dimensions from ancestor classes
/// (MLS §4.5, §7.1).
pub(crate) fn extract_ancestor_constants_multi_pass(
    tree: &ClassTree,
    resolve_context: &str,
    ancestors: &[&ClassDef],
    ctx: &mut Context,
) {
    const MAX_PASSES: usize = 5;
    for _pass in 0..MAX_PASSES {
        let prev = ctx.parameter_values.len()
            + ctx.array_dimensions.len()
            + ctx.boolean_parameter_values.len();
        for ancestor in ancestors {
            for ext in &ancestor.extends {
                extract_extends_modification_constants(tree, "", ext, resolve_context, ctx);
            }
            extract_constants_from_class(ancestor, ctx);
        }
        let new = ctx.parameter_values.len()
            + ctx.array_dimensions.len()
            + ctx.boolean_parameter_values.len();
        if new == prev {
            break;
        }
    }
}

/// Extract integer constants and array dimensions from a class definition (MLS §4.5).
pub(crate) fn extract_constants_from_class(class_def: &ClassDef, ctx: &mut Context) {
    for (name, comp) in &class_def.components {
        if !matches!(
            comp.variability,
            ast::Variability::Constant(_) | ast::Variability::Parameter(_)
        ) {
            continue;
        }
        let binding = comp
            .binding
            .as_ref()
            .or(if !matches!(comp.start, ast::Expression::Empty) {
                Some(&comp.start)
            } else {
                None
            });
        let Some(expr) = binding else { continue };
        let type_name = comp.type_name.to_string();
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
/// For dotted names only, a final unique full-suffix lookup is allowed
/// (e.g. `medium.nXi` can match `source.medium.nXi` when unique).
pub(crate) fn lookup_with_scope<V: Clone + PartialEq>(
    name: &str,
    scope: &str,
    map: &rustc_hash::FxHashMap<String, V>,
) -> Option<V> {
    let mut current_scope = scope;
    loop {
        let qualified = if current_scope.is_empty() {
            name.to_string()
        } else {
            format!("{current_scope}.{name}")
        };
        if let Some(val) = map.get(&qualified) {
            return Some(val.clone());
        }
        if let Some(parent_scope) = crate::path_utils::parent_scope(current_scope) {
            current_scope = parent_scope;
        } else if !current_scope.is_empty() {
            current_scope = "";
        } else {
            break;
        }
    }
    if let Some(val) = map.get(name) {
        return Some(val.clone());
    }
    if crate::path_utils::has_top_level_dot(name) {
        return lookup_unique_dotted_suffix(name, map);
    }
    None
}

/// Fallback lookup for dotted `*.suffix` keys.
///
/// Returns `None` when matches are ambiguous with different values.
pub(crate) fn lookup_unique_dotted_suffix<V: Clone + PartialEq>(
    dotted_name: &str,
    map: &rustc_hash::FxHashMap<String, V>,
) -> Option<V> {
    let suffix = format!(".{dotted_name}");
    let mut found: Option<&V> = None;
    for (key, val) in map {
        if !key.ends_with(&suffix) {
            continue;
        }
        if found.is_some_and(|prev| prev != val) {
            return None;
        }
        found = Some(val);
    }
    found.cloned()
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
        } => token.text.as_ref().parse().ok(),
        ast::Expression::ComponentReference(cr) => {
            let name = cr
                .parts
                .iter()
                .map(|p| p.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");
            lookup_with_scope(&name, scope, &ctx.parameter_values)
        }
        ast::Expression::Unary {
            rhs,
            op: OpUnary::Minus(_),
        } => try_eval_const_integer_with_scope(rhs, ctx, scope).map(|v| -v),
        ast::Expression::Parenthesized { inner } => {
            try_eval_const_integer_with_scope(inner, ctx, scope)
        }
        ast::Expression::Binary { lhs, rhs, op } => {
            let l = try_eval_const_integer_with_scope(lhs, ctx, scope)?;
            let r = try_eval_const_integer_with_scope(rhs, ctx, scope)?;
            eval_ast_integer_binary(op, l, r)
        }
        ast::Expression::FunctionCall { comp, args } => {
            eval_const_integer_function_with_scope(comp, args, ctx, scope)
        }
        // MLS §3.6: if-expressions for conditional constant evaluation
        ast::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                match try_eval_const_boolean_with_scope(cond, ctx, scope) {
                    Some(true) => return try_eval_const_integer_with_scope(then_expr, ctx, scope),
                    Some(false) => continue,
                    None => return None,
                }
            }
            try_eval_const_integer_with_scope(else_branch, ctx, scope)
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
        } => token.text.as_ref().parse().ok(),
        ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token,
        } => token.text.as_ref().parse::<i64>().ok().map(|v| v as f64),
        ast::Expression::ComponentReference(cr) => {
            let name = cr
                .parts
                .iter()
                .map(|p| p.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");
            lookup_with_scope(&name, scope, &ctx.real_parameter_values).or_else(|| {
                lookup_with_scope(&name, scope, &ctx.parameter_values).map(|v| v as f64)
            })
        }
        ast::Expression::Unary {
            rhs,
            op: OpUnary::Minus(_),
        } => try_eval_const_real_with_scope(rhs, ctx, scope).map(|v| -v),
        ast::Expression::Parenthesized { inner } => {
            try_eval_const_real_with_scope(inner, ctx, scope)
        }
        ast::Expression::Binary { lhs, rhs, op } => {
            let l = try_eval_const_real_with_scope(lhs, ctx, scope)?;
            let r = try_eval_const_real_with_scope(rhs, ctx, scope)?;
            match op {
                OpBinary::Add(_) => Some(l + r),
                OpBinary::Sub(_) => Some(l - r),
                OpBinary::Mul(_) => Some(l * r),
                OpBinary::Div(_) => (r.abs() > f64::EPSILON).then_some(l / r),
                OpBinary::Exp(_) => Some(l.powf(r)),
                _ => None,
            }
        }
        ast::Expression::FunctionCall { comp, args } => {
            eval_const_real_function_with_scope(comp, args, ctx, scope)
        }
        ast::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                match try_eval_const_boolean_with_scope(cond, ctx, scope) {
                    Some(true) => return try_eval_const_real_with_scope(then_expr, ctx, scope),
                    Some(false) => continue,
                    None => return None,
                }
            }
            try_eval_const_real_with_scope(else_branch, ctx, scope)
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
    match fn_name {
        "size" if args.len() == 2 => {
            eval_size_call_with_scope(&args[0], &args[1], ctx, scope).map(|v| v as f64)
        }
        "abs" if args.len() == 1 => eval(&args[0]).map(f64::abs),
        "sign" if args.len() == 1 => eval(&args[0]).map(f64::signum),
        "sqrt" if args.len() == 1 => eval(&args[0]).map(f64::sqrt),
        "exp" if args.len() == 1 => eval(&args[0]).map(f64::exp),
        "log" if args.len() == 1 => eval(&args[0]).map(f64::ln),
        "log10" if args.len() == 1 => eval(&args[0]).map(f64::log10),
        "sin" if args.len() == 1 => eval(&args[0]).map(f64::sin),
        "cos" if args.len() == 1 => eval(&args[0]).map(f64::cos),
        "tan" if args.len() == 1 => eval(&args[0]).map(f64::tan),
        "asin" if args.len() == 1 => eval(&args[0]).map(f64::asin),
        "acos" if args.len() == 1 => eval(&args[0]).map(f64::acos),
        "atan" if args.len() == 1 => eval(&args[0]).map(f64::atan),
        "sinh" if args.len() == 1 => eval(&args[0]).map(f64::sinh),
        "cosh" if args.len() == 1 => eval(&args[0]).map(f64::cosh),
        "tanh" if args.len() == 1 => eval(&args[0]).map(f64::tanh),
        "max" if args.len() == 2 => Some(eval(&args[0])?.max(eval(&args[1])?)),
        "min" if args.len() == 2 => Some(eval(&args[0])?.min(eval(&args[1])?)),
        "integer" if args.len() == 1 => Some(eval(&args[0])?.trunc()),
        _ => None,
    }
}

pub(crate) fn try_eval_const_flat_expr_with_scope(
    expr: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<rumoca_ir_flat::Expression> {
    if let Some(value) = try_eval_const_terminal_expr(expr) {
        return Some(value);
    }

    match expr {
        ast::Expression::ComponentReference(cr) => {
            try_eval_const_component_ref_expr(cr, ctx, scope)
        }
        ast::Expression::Unary {
            op: OpUnary::Minus(_),
            rhs,
        } => match try_eval_const_flat_expr_with_scope(rhs, ctx, scope)? {
            rumoca_ir_flat::Expression::Literal(Literal::Real(v)) => {
                Some(rumoca_ir_flat::Expression::Literal(Literal::Real(-v)))
            }
            rumoca_ir_flat::Expression::Literal(Literal::Integer(v)) => {
                Some(rumoca_ir_flat::Expression::Literal(Literal::Integer(-v)))
            }
            _ => None,
        },
        ast::Expression::Parenthesized { inner } => {
            try_eval_const_flat_expr_with_scope(inner, ctx, scope)
        }
        ast::Expression::Binary { lhs, rhs, op } => {
            let lhs = try_eval_const_flat_expr_with_scope(lhs, ctx, scope)?;
            let rhs = try_eval_const_flat_expr_with_scope(rhs, ctx, scope)?;
            Some(rumoca_ir_flat::Expression::Binary {
                op: op.clone(),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        }
        ast::Expression::Array {
            elements,
            is_matrix,
        } => try_eval_const_array_expr(elements, *is_matrix, ctx, scope),
        ast::Expression::Tuple { elements } => try_eval_const_tuple_expr(elements, ctx, scope),
        ast::Expression::Range { start, step, end } => Some(rumoca_ir_flat::Expression::Range {
            start: Box::new(try_eval_const_flat_expr_with_scope(start, ctx, scope)?),
            step: if let Some(step_expr) = step {
                Some(Box::new(try_eval_const_flat_expr_with_scope(
                    step_expr, ctx, scope,
                )?))
            } else {
                None
            },
            end: Box::new(try_eval_const_flat_expr_with_scope(end, ctx, scope)?),
        }),
        ast::Expression::FunctionCall { comp, args } => {
            try_eval_const_function_call_expr(comp, args, ctx, scope)
        }
        ast::Expression::FieldAccess { base, field } => {
            Some(rumoca_ir_flat::Expression::FieldAccess {
                base: Box::new(try_eval_const_flat_expr_with_scope(base, ctx, scope)?),
                field: field.clone(),
            })
        }
        ast::Expression::If {
            branches,
            else_branch,
        } => try_eval_const_if_expr(branches, else_branch, ctx, scope),
        _ => None,
    }
}

pub(crate) fn try_extract_named_record_constructor_constant(
    expr: &ast::Expression,
    ctx: &mut Context,
    scope: &str,
    full_name: &str,
) -> Option<rumoca_ir_flat::Expression> {
    let (ctor_name, named_fields) = extract_named_record_constructor_fields(expr)?;

    if named_fields.is_empty() {
        return None;
    }

    // Evaluate named fields with bounded fixed-point passes so references like
    // `R_s = R_NASA_2002 / H2O.MM` resolve once `H2O.MM` is available.
    let mut pending = named_fields;
    let mut resolved: Vec<(String, rumoca_ir_flat::Expression)> = Vec::new();
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

    let ctor_args = resolved.into_iter().map(|(_, value)| value).collect();
    Some(rumoca_ir_flat::Expression::FunctionCall {
        name: rumoca_ir_flat::VarName::new(ctor_name),
        args: ctor_args,
        is_constructor: true,
    })
}

fn extract_named_record_constructor_fields(
    expr: &ast::Expression,
) -> Option<(String, Vec<(String, ast::Expression)>)> {
    match expr {
        ast::Expression::FunctionCall { comp, args } => {
            if args.is_empty() {
                return None;
            }
            let ctor_name = comp
                .parts
                .iter()
                .map(|part| part.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");
            let mut named_fields = Vec::new();
            for arg in args {
                let ast::Expression::NamedArgument { name, value } = arg else {
                    return None;
                };
                named_fields.push((name.text.to_string(), value.as_ref().clone()));
            }
            Some((ctor_name, named_fields))
        }
        ast::Expression::ClassModification {
            target,
            modifications,
        } => {
            if modifications.is_empty() {
                return None;
            }
            let ctor_name = target.to_string();
            let mut named_fields = Vec::new();
            for modification in modifications {
                match modification {
                    ast::Expression::NamedArgument { name, value } => {
                        named_fields.push((name.text.to_string(), value.as_ref().clone()));
                    }
                    ast::Expression::Modification { target, value } => {
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
    value: &rumoca_ir_flat::Expression,
) {
    insert_with_prefix(
        &mut ctx.constant_values,
        record_prefix,
        field_name,
        field_full_name,
        value.clone(),
    );
    match value {
        rumoca_ir_flat::Expression::Literal(Literal::Integer(v)) => {
            insert_with_prefix(
                &mut ctx.parameter_values,
                record_prefix,
                field_name,
                field_full_name,
                *v,
            );
        }
        rumoca_ir_flat::Expression::Literal(Literal::Real(v)) if v.is_finite() => {
            insert_with_prefix(
                &mut ctx.real_parameter_values,
                record_prefix,
                field_name,
                field_full_name,
                *v,
            );
        }
        rumoca_ir_flat::Expression::Literal(Literal::Boolean(v)) => {
            insert_with_prefix(
                &mut ctx.boolean_parameter_values,
                record_prefix,
                field_name,
                field_full_name,
                *v,
            );
        }
        rumoca_ir_flat::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
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

pub(crate) fn try_eval_const_terminal_expr(
    expr: &ast::Expression,
) -> Option<rumoca_ir_flat::Expression> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedReal,
            token,
        } => token
            .text
            .as_ref()
            .parse::<f64>()
            .ok()
            .map(Literal::Real)
            .map(rumoca_ir_flat::Expression::Literal),
        ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token,
        } => token
            .text
            .as_ref()
            .parse::<i64>()
            .ok()
            .map(Literal::Integer)
            .map(rumoca_ir_flat::Expression::Literal),
        ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::Bool,
            token,
        } => match token.text.as_ref() {
            "true" => Some(rumoca_ir_flat::Expression::Literal(Literal::Boolean(true))),
            "false" => Some(rumoca_ir_flat::Expression::Literal(Literal::Boolean(false))),
            _ => None,
        },
        ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::String,
            token,
        } => Some(rumoca_ir_flat::Expression::Literal(Literal::String(
            token.text.as_ref().to_string(),
        ))),
        _ => None,
    }
}

pub(crate) fn try_eval_const_component_ref_expr(
    cr: &rumoca_ir_ast::ComponentReference,
    ctx: &Context,
    scope: &str,
) -> Option<rumoca_ir_flat::Expression> {
    let name = cr
        .parts
        .iter()
        .map(|p| p.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");
    if let Some(v) = lookup_constant_expr_with_scope(&name, scope, &ctx.constant_values) {
        return Some(v);
    }
    if let Some(v) = lookup_with_scope(&name, scope, &ctx.real_parameter_values) {
        return Some(rumoca_ir_flat::Expression::Literal(Literal::Real(v)));
    }
    if let Some(v) = lookup_with_scope(&name, scope, &ctx.parameter_values) {
        return Some(rumoca_ir_flat::Expression::Literal(Literal::Integer(v)));
    }
    if let Some(v) = lookup_with_scope(&name, scope, &ctx.boolean_parameter_values) {
        return Some(rumoca_ir_flat::Expression::Literal(Literal::Boolean(v)));
    }
    lookup_with_scope(&name, scope, &ctx.enum_parameter_values).map(|enum_name| {
        rumoca_ir_flat::Expression::VarRef {
            name: rumoca_ir_flat::VarName::new(enum_name),
            subscripts: vec![],
        }
    })
}

fn try_eval_const_function_call_expr(
    comp: &rumoca_ir_ast::ComponentReference,
    args: &[ast::Expression],
    ctx: &Context,
    scope: &str,
) -> Option<rumoca_ir_flat::Expression> {
    let evaluated_args: Vec<_> = args
        .iter()
        .map(|arg| try_eval_const_flat_expr_with_scope(arg, ctx, scope))
        .collect::<Option<Vec<_>>>()?;

    let textual_name = comp
        .parts
        .iter()
        .map(|p| p.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");
    let short_name = comp
        .parts
        .last()
        .map(|part| part.ident.text.as_ref())
        .unwrap_or(textual_name.as_str());

    if let Some(function) = rumoca_ir_flat::BuiltinFunction::from_name(short_name) {
        return Some(rumoca_ir_flat::Expression::BuiltinCall {
            function,
            args: evaluated_args,
        });
    }

    if let Some(function) =
        rumoca_ir_flat::BuiltinFunction::from_name(&short_name.to_ascii_lowercase())
    {
        return Some(rumoca_ir_flat::Expression::BuiltinCall {
            function,
            args: evaluated_args,
        });
    }

    Some(rumoca_ir_flat::Expression::FunctionCall {
        name: rumoca_ir_flat::VarName::new(textual_name),
        args: evaluated_args,
        is_constructor: false,
    })
}

pub(crate) fn try_eval_const_array_expr(
    elements: &[ast::Expression],
    is_matrix: bool,
    ctx: &Context,
    scope: &str,
) -> Option<rumoca_ir_flat::Expression> {
    let mut out = Vec::with_capacity(elements.len());
    for el in elements {
        out.push(try_eval_const_flat_expr_with_scope(el, ctx, scope)?);
    }
    Some(rumoca_ir_flat::Expression::Array {
        elements: out,
        is_matrix,
    })
}

pub(crate) fn try_eval_const_tuple_expr(
    elements: &[ast::Expression],
    ctx: &Context,
    scope: &str,
) -> Option<rumoca_ir_flat::Expression> {
    let mut out = Vec::with_capacity(elements.len());
    for el in elements {
        out.push(try_eval_const_flat_expr_with_scope(el, ctx, scope)?);
    }
    Some(rumoca_ir_flat::Expression::Tuple { elements: out })
}

pub(crate) fn try_eval_const_if_expr(
    branches: &[(ast::Expression, ast::Expression)],
    else_branch: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<rumoca_ir_flat::Expression> {
    for (cond, then_expr) in branches {
        match try_eval_const_boolean_with_scope(cond, ctx, scope) {
            Some(true) => return try_eval_const_flat_expr_with_scope(then_expr, ctx, scope),
            Some(false) => continue,
            None => return None,
        }
    }
    try_eval_const_flat_expr_with_scope(else_branch, ctx, scope)
}

pub(crate) fn lookup_constant_expr_with_scope(
    name: &str,
    scope: &str,
    map: &rustc_hash::FxHashMap<String, rumoca_ir_flat::Expression>,
) -> Option<rumoca_ir_flat::Expression> {
    let mut current_scope = scope;
    loop {
        let qualified = if current_scope.is_empty() {
            name.to_string()
        } else {
            format!("{current_scope}.{name}")
        };
        if let Some(val) = map.get(&qualified) {
            return Some(val.clone());
        }
        if let Some(parent_scope) = crate::path_utils::parent_scope(current_scope) {
            current_scope = parent_scope;
        } else if !current_scope.is_empty() {
            current_scope = "";
        } else {
            break;
        }
    }
    if let Some(val) = map.get(name) {
        return Some(val.clone());
    }
    if crate::path_utils::has_top_level_dot(name) {
        let suffix = format!(".{name}");
        let mut matched: Option<rumoca_ir_flat::Expression> = None;
        for (key, val) in map {
            if !key.ends_with(&suffix) {
                continue;
            }
            if matched.replace(val.clone()).is_some() {
                return None;
            }
        }
        return matched;
    }
    None
}

pub(crate) fn eval_ast_integer_binary(op: &OpBinary, lhs: i64, rhs: i64) -> Option<i64> {
    let operator = match op {
        OpBinary::Add(_) => rumoca_core::IntegerBinaryOperator::Add,
        OpBinary::Sub(_) => rumoca_core::IntegerBinaryOperator::Sub,
        OpBinary::Mul(_) => rumoca_core::IntegerBinaryOperator::Mul,
        OpBinary::Div(_) => rumoca_core::IntegerBinaryOperator::Div,
        _ => return None,
    };
    rumoca_core::eval_integer_binary(operator, lhs, rhs)
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
        } => match token.text.as_ref() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },
        ast::Expression::ComponentReference(cr) => {
            let name = cr
                .parts
                .iter()
                .map(|p| p.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");
            lookup_with_scope(&name, scope, &ctx.boolean_parameter_values)
        }
        ast::Expression::Unary {
            op: OpUnary::Not(_),
            rhs,
        } => try_eval_const_boolean_with_scope(rhs, ctx, scope).map(|v| !v),
        ast::Expression::Parenthesized { inner } => {
            try_eval_const_boolean_with_scope(inner, ctx, scope)
        }
        ast::Expression::Binary { op, lhs, rhs } => match op {
            OpBinary::And(_) => Some(
                try_eval_const_boolean_with_scope(lhs, ctx, scope)?
                    && try_eval_const_boolean_with_scope(rhs, ctx, scope)?,
            ),
            OpBinary::Or(_) => Some(
                try_eval_const_boolean_with_scope(lhs, ctx, scope)?
                    || try_eval_const_boolean_with_scope(rhs, ctx, scope)?,
            ),
            // Integer/Real comparisons for conditional parameters (MLS §3.5)
            OpBinary::Eq(_) => eval_const_equality_with_scope(lhs, rhs, ctx, scope, true),
            OpBinary::Neq(_) => eval_const_equality_with_scope(lhs, rhs, ctx, scope, false),
            OpBinary::Lt(_) => {
                if let (Some(l), Some(r)) = (
                    try_eval_const_integer_with_scope(lhs, ctx, scope),
                    try_eval_const_integer_with_scope(rhs, ctx, scope),
                ) {
                    Some(l < r)
                } else {
                    None
                }
            }
            OpBinary::Le(_) => {
                if let (Some(l), Some(r)) = (
                    try_eval_const_integer_with_scope(lhs, ctx, scope),
                    try_eval_const_integer_with_scope(rhs, ctx, scope),
                ) {
                    Some(l <= r)
                } else {
                    None
                }
            }
            OpBinary::Gt(_) => {
                if let (Some(l), Some(r)) = (
                    try_eval_const_integer_with_scope(lhs, ctx, scope),
                    try_eval_const_integer_with_scope(rhs, ctx, scope),
                ) {
                    Some(l > r)
                } else {
                    None
                }
            }
            OpBinary::Ge(_) => {
                if let (Some(l), Some(r)) = (
                    try_eval_const_integer_with_scope(lhs, ctx, scope),
                    try_eval_const_integer_with_scope(rhs, ctx, scope),
                ) {
                    Some(l >= r)
                } else {
                    None
                }
            }
            _ => None,
        },
        _ => None,
    }
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
            let name = cr
                .parts
                .iter()
                .map(|p| p.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");

            lookup_with_scope(&name, scope, &ctx.enum_parameter_values).or_else(|| {
                // Fallback: treat only enum-literal-like dotted refs as literals
                // when not found as scoped enum-valued parameters.
                looks_like_enum_literal_path(&name).then_some(name)
            })
        }
        ast::Expression::Parenthesized { inner } => {
            try_eval_const_enum_with_scope(inner, ctx, scope)
        }
        ast::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                match try_eval_const_boolean_with_scope(cond, ctx, scope) {
                    Some(true) => return try_eval_const_enum_with_scope(then_expr, ctx, scope),
                    Some(false) => continue,
                    None => return None,
                }
            }
            try_eval_const_enum_with_scope(else_branch, ctx, scope)
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
        ast::Expression::ComponentReference(cr) => cr
            .parts
            .iter()
            .map(|p| p.ident.text.as_ref())
            .collect::<Vec<_>>()
            .join("."),
        _ => return None,
    };
    let dims = lookup_with_scope(&arr_name, scope, &ctx.array_dimensions)?;
    dims.get((dim - 1) as usize).copied()
}

/// Infer array dimensions from an expression (array literal length).
pub(crate) fn infer_dims_from_expr(
    expr: &ast::Expression,
    ctx: &Context,
    scope: &str,
) -> Option<Vec<i64>> {
    match expr {
        ast::Expression::Array { elements, .. } => Some(vec![elements.len() as i64]),
        // MLS §10.3.3: fill(s, n1, n2, ...) creates an array of size n1 x n2 x ...
        ast::Expression::FunctionCall { comp, args } => {
            let fn_name = comp
                .parts
                .last()
                .map(|p| p.ident.text.as_ref())
                .unwrap_or("");
            if fn_name == "fill" && args.len() >= 2 {
                // fill(value, n1, n2, ...) - try to evaluate dimension arguments
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
) {
    let eval_ctx = build_structural_eval_context(ctx, overlay, tree);

    // Scan all class instances for structural Boolean equations
    for (_def_id, class_data) in &overlay.classes {
        let prefix = &class_data.qualified_name;
        for eq_entry in &class_data.equations {
            if let Some((var_name, bool_val)) =
                try_eval_structural_equation(&eq_entry.equation, prefix, ctx, &eval_ctx)
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
}

/// Build an evaluation context populated with known parameters, functions, constants,
/// and component bindings from the overlay.
pub(crate) fn build_structural_eval_context(
    ctx: &Context,
    overlay: &InstanceOverlay,
    tree: &ClassTree,
) -> rumoca_eval_const::EvalContext {
    use rumoca_eval_const::{EvalContext, Value};

    let mut eval_ctx = EvalContext::new();
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

    resolve_constants_from_tree(tree, &mut eval_ctx);
    collect_component_binding_values(overlay, &mut eval_ctx);

    eval_ctx
}

/// Evaluate component bindings and start values, adding resolved values to the eval context.
///
/// Only uses start values as fallback for parameters and constants.
/// Non-parameter variables' start values are initial conditions, not compile-time
/// constants, and must not be used for structural equation evaluation (MLS §8.6).
pub(crate) fn collect_component_binding_values(
    overlay: &InstanceOverlay,
    eval_ctx: &mut rumoca_eval_const::EvalContext,
) {
    for (_def_id, instance_data) in &overlay.components {
        let qualified_name = instance_data.qualified_name.to_flat_string();

        if eval_ctx.get(&qualified_name).is_some() {
            continue;
        }

        if let Some(binding) = &instance_data.binding {
            let flat_binding = qualify_expression(binding, &QualifiedName::new());
            if let Ok(val) = rumoca_eval_const::eval_expr(&flat_binding, eval_ctx) {
                eval_ctx.add_parameter(qualified_name.clone(), val);
                continue;
            }
        }

        // Only use start values for parameters/constants as default values.
        // Non-parameter variables' start values are initial conditions that
        // must not be treated as compile-time constants.
        let is_param_or_const = matches!(
            instance_data.variability,
            ast::Variability::Parameter(_) | ast::Variability::Constant(_)
        );
        if is_param_or_const && let Some(start) = &instance_data.start {
            let flat_start = qualify_expression(start, &QualifiedName::new());
            if let Ok(val) = rumoca_eval_const::eval_expr(&flat_start, eval_ctx) {
                eval_ctx.add_parameter(qualified_name, val);
            }
        }
    }
}

/// Try to evaluate a simple equation as a structural Boolean assignment.
/// Returns `Some((qualified_var_name, bool_value))` if successful.
pub(crate) fn try_eval_structural_equation(
    equation: &rumoca_ir_ast::Equation,
    prefix: &QualifiedName,
    ctx: &Context,
    eval_ctx: &rumoca_eval_const::EvalContext,
) -> Option<(String, bool)> {
    let ast::Equation::Simple { lhs, rhs } = equation else {
        return None;
    };

    let ast::Expression::ComponentReference(cr) = lhs else {
        return None;
    };
    if cr.parts.len() != 1 || cr.parts[0].subs.is_some() {
        return None;
    }

    let var_name_part = cr.parts[0].ident.text.as_ref();
    let qualified_var = if prefix.is_empty() {
        var_name_part.to_string()
    } else {
        format!("{}.{}", prefix.to_flat_string(), var_name_part)
    };

    if ctx.boolean_parameter_values.contains_key(&qualified_var) {
        return None;
    }

    let flat_rhs = qualify_expression(rhs, prefix);
    let val = match rumoca_eval_const::eval_expr(&flat_rhs, eval_ctx) {
        Ok(v) => v,
        Err(_e) => {
            return None;
        }
    };
    let bool_val = val.as_bool()?;

    Some((qualified_var, bool_val))
}

/// Collect function call names from an equation recursively.
/// Uses the ClassTree to resolve def_ids to fully qualified names.
pub(crate) fn collect_function_calls_from_equation(
    eq: &rumoca_ir_ast::Equation,
    calls: &mut std::collections::HashSet<String>,
    tree: &ClassTree,
) {
    match eq {
        ast::Equation::Simple { lhs, rhs } => {
            collect_function_calls_from_expression(lhs, calls, tree);
            collect_function_calls_from_expression(rhs, calls, tree);
        }
        ast::Equation::For { indices, equations } => {
            // Check the range expressions for function calls
            for idx in indices {
                collect_function_calls_from_expression(&idx.range, calls, tree);
            }
            for inner_eq in equations {
                collect_function_calls_from_equation(inner_eq, calls, tree);
            }
        }
        ast::Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_function_calls_from_expression(&block.cond, calls, tree);
                for inner_eq in &block.eqs {
                    collect_function_calls_from_equation(inner_eq, calls, tree);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner_eq in else_eqs {
                    collect_function_calls_from_equation(inner_eq, calls, tree);
                }
            }
        }
        ast::Equation::When(blocks) => {
            for block in blocks {
                collect_function_calls_from_expression(&block.cond, calls, tree);
                for inner_eq in &block.eqs {
                    collect_function_calls_from_equation(inner_eq, calls, tree);
                }
            }
        }
        ast::Equation::Connect { lhs, rhs } => {
            // Component references in connect don't contain function calls
            let _ = (lhs, rhs);
        }
        ast::Equation::FunctionCall { comp, args } => {
            // The function call itself - use def_id if available for qualified name
            let func_name = resolve_function_name(comp, tree);
            calls.insert(func_name);
            // Check arguments
            for arg in args {
                collect_function_calls_from_expression(arg, calls, tree);
            }
        }
        ast::Equation::Assert {
            condition, message, ..
        } => {
            collect_function_calls_from_expression(condition, calls, tree);
            collect_function_calls_from_expression(message, calls, tree);
        }
        ast::Equation::Empty => {}
    }
}

/// Resolve a function call's component reference to its fully qualified name.
/// Uses def_id from import resolution (contract from resolve phase).
pub(crate) fn resolve_function_name(
    comp: &rumoca_ir_ast::ComponentReference,
    tree: &ClassTree,
) -> String {
    // First get the textual name from parts - this is the full reference as written
    let textual_name = comp
        .parts
        .iter()
        .map(|p| p.ident.text.to_string())
        .collect::<Vec<_>>()
        .join(".");

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
            if let Some(class_def) = tree.get_class_by_qualified_name(&candidate)
                && class_def.class_type == rumoca_ir_ast::ClassType::Function
            {
                return candidate;
            }
        }

        // Also allow direct function def_id resolution.
        if let Some(class_def) = tree.get_class_by_qualified_name(base_name)
            && class_def.class_type == rumoca_ir_ast::ClassType::Function
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
    calls: &mut std::collections::HashSet<String>,
    tree: &ClassTree,
) {
    match expr {
        ast::Expression::FunctionCall { comp, args, .. } => {
            // Record the function name - use def_id if available for qualified name
            let func_name = resolve_function_name(comp, tree);
            calls.insert(func_name);
            // Check arguments recursively
            for arg in args {
                collect_function_calls_from_expression(arg, calls, tree);
            }
        }
        ast::Expression::Binary { lhs, rhs, .. } => {
            collect_function_calls_from_expression(lhs, calls, tree);
            collect_function_calls_from_expression(rhs, calls, tree);
        }
        ast::Expression::Unary { rhs, .. } => {
            collect_function_calls_from_expression(rhs, calls, tree);
        }
        ast::Expression::Array { elements, .. } => {
            for elem in elements {
                collect_function_calls_from_expression(elem, calls, tree);
            }
        }
        ast::Expression::Range { start, step, end } => {
            collect_function_calls_from_expression(start, calls, tree);
            if let Some(s) = step {
                collect_function_calls_from_expression(s, calls, tree);
            }
            collect_function_calls_from_expression(end, calls, tree);
        }
        ast::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                collect_function_calls_from_expression(cond, calls, tree);
                collect_function_calls_from_expression(then_expr, calls, tree);
            }
            collect_function_calls_from_expression(else_branch, calls, tree);
        }
        ast::Expression::ArrayIndex { base, subscripts } => {
            collect_function_calls_from_expression(base, calls, tree);
            for sub in subscripts {
                if let rumoca_ir_ast::Subscript::Expression(e) = sub {
                    collect_function_calls_from_expression(e, calls, tree);
                }
            }
        }
        ast::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            collect_function_calls_from_expression(expr, calls, tree);
            for idx in indices {
                collect_function_calls_from_expression(&idx.range, calls, tree);
            }
            if let Some(f) = filter {
                collect_function_calls_from_expression(f, calls, tree);
            }
        }
        ast::Expression::Parenthesized { inner } => {
            collect_function_calls_from_expression(inner, calls, tree);
        }
        ast::Expression::Tuple { elements } => {
            for elem in elements {
                collect_function_calls_from_expression(elem, calls, tree);
            }
        }
        ast::Expression::FieldAccess { base, .. } => {
            collect_function_calls_from_expression(base, calls, tree);
        }
        // Terminal expressions don't contain function calls
        ast::Expression::Empty
        | ast::Expression::Terminal { .. }
        | ast::Expression::ComponentReference(_)
        | ast::Expression::NamedArgument { .. }
        | ast::Expression::Modification { .. }
        | ast::Expression::ClassModification { .. } => {}
    }
}

/// Context for flattening.
pub struct Context {
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
    pub constant_values: rustc_hash::FxHashMap<String, rumoca_ir_flat::Expression>,
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
    /// Maps record parameter name -> alias target (MLS §7.2.3).
    /// Example: "battery2.cellData" -> "cellData2" allows resolving
    /// "battery2.cellData.nRC" to "cellData2.nRC".
    pub record_aliases: rustc_hash::FxHashMap<String, String>,
    /// VCG isRoot results: path -> true if this node is the root of its component (MLS §9.4).
    pub vcg_is_root: rustc_hash::FxHashMap<String, bool>,
    /// VCG rooted results: path -> true if this node is on the "rooted" side (MLS §9.4).
    pub vcg_rooted: rustc_hash::FxHashMap<String, bool>,
    /// Cardinality counts: connector path -> number of connect() statements referencing it (MLS §3.7.2.3).
    pub cardinality_counts: rustc_hash::FxHashMap<String, i64>,
    /// Lazy base evaluator for flatten expression fallback evaluation.
    /// This is built once per flatten context after structural lookup stabilizes.
    pub(crate) eval_fallback_context: std::cell::OnceCell<rumoca_eval_const::EvalContext>,
    /// Current import map for the class instance being processed (MLS §13.2).
    /// Set before processing each class instance's equations, cleared after.
    pub current_imports: crate::qualify::ImportMap,
}

#[cfg(test)]
mod lookup_scope_tests {
    use std::sync::Arc;

    use super::{
        lookup_with_scope, try_eval_const_boolean_with_scope, try_eval_const_integer_with_scope,
    };
    use crate::Context;
    use rumoca_ir_ast as ast;
    use rumoca_ir_ast::{ComponentRefPart, ComponentReference, OpBinary, Token};

    #[test]
    fn subscript_dot_name_does_not_trigger_dotted_suffix_lookup() {
        let mut values: rustc_hash::FxHashMap<String, i64> = rustc_hash::FxHashMap::default();
        values.insert("sys.arr[data.medium]".to_string(), 7);

        assert_eq!(lookup_with_scope("arr[data.medium]", "", &values), None);
    }

    #[test]
    fn subscript_dot_name_still_resolves_with_parent_scope() {
        let mut values: rustc_hash::FxHashMap<String, i64> = rustc_hash::FxHashMap::default();
        values.insert("sys.arr[data.medium]".to_string(), 7);

        assert_eq!(
            lookup_with_scope("arr[data.medium]", "sys", &values),
            Some(7)
        );
    }

    fn token(text: &str) -> Token {
        Token {
            text: Arc::from(text.to_string()),
            ..Token::default()
        }
    }

    fn int_expr(value: i64) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token: token(&value.to_string()),
        }
    }

    fn comp_ref(path: &str) -> ComponentReference {
        ComponentReference {
            local: false,
            parts: path
                .split('.')
                .map(|part| ComponentRefPart {
                    ident: token(part),
                    subs: None,
                })
                .collect(),
            def_id: None,
        }
    }

    fn call_expr(name: &str, args: Vec<ast::Expression>) -> ast::Expression {
        ast::Expression::FunctionCall {
            comp: comp_ref(name),
            args,
        }
    }

    fn eq_expr(lhs: ast::Expression, rhs: ast::Expression) -> ast::Expression {
        ast::Expression::Binary {
            op: OpBinary::Eq(Token::default()),
            lhs: Arc::new(lhs),
            rhs: Arc::new(rhs),
        }
    }

    #[test]
    fn const_integer_div_operator_requires_exact_quotient() {
        let expr = ast::Expression::Binary {
            op: OpBinary::Div(Token::default()),
            lhs: Arc::new(int_expr(7)),
            rhs: Arc::new(int_expr(2)),
        };

        assert_eq!(
            try_eval_const_integer_with_scope(&expr, &Context::new(), ""),
            None
        );
    }

    #[test]
    fn const_integer_div_builtin_remains_truncating() {
        let expr = call_expr("div", vec![int_expr(7), int_expr(2)]);

        assert_eq!(
            try_eval_const_integer_with_scope(&expr, &Context::new(), ""),
            Some(3)
        );
    }

    #[test]
    fn const_boolean_enum_eq_accepts_suffix_qualification() {
        let mut ctx = Context::new();
        ctx.enum_parameter_values.insert(
            "controllerType".to_string(),
            "Modelica.Blocks.Types.SimpleController.PI".to_string(),
        );

        let expr = eq_expr(
            ast::Expression::ComponentReference(comp_ref("controllerType")),
            ast::Expression::ComponentReference(comp_ref("SimpleController.PI")),
        );
        assert_eq!(
            try_eval_const_boolean_with_scope(&expr, &ctx, ""),
            Some(true)
        );
    }

    #[test]
    fn const_boolean_enum_eq_accepts_shared_type_literal_tail() {
        let mut ctx = Context::new();
        ctx.enum_parameter_values.insert(
            "frameResolve".to_string(),
            "sensor_frame_a2.MultiBody.Types.ResolveInFrameA.frame_resolve".to_string(),
        );

        let expr = eq_expr(
            ast::Expression::ComponentReference(comp_ref("frameResolve")),
            ast::Expression::ComponentReference(comp_ref(
                "Modelica.Mechanics.MultiBody.Types.ResolveInFrameA.frame_resolve",
            )),
        );
        assert_eq!(
            try_eval_const_boolean_with_scope(&expr, &ctx, ""),
            Some(true)
        );
    }

    #[test]
    fn const_boolean_enum_eq_rejects_different_enum_type() {
        let mut ctx = Context::new();
        ctx.enum_parameter_values.insert(
            "mode".to_string(),
            "Modelica.Blocks.Types.Init.PI".to_string(),
        );

        let expr = eq_expr(
            ast::Expression::ComponentReference(comp_ref("mode")),
            ast::Expression::ComponentReference(comp_ref(
                "Modelica.Blocks.Types.SimpleController.PI",
            )),
        );
        assert_eq!(
            try_eval_const_boolean_with_scope(&expr, &ctx, ""),
            Some(false)
        );
    }
}
