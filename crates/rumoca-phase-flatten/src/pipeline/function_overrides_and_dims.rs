use super::*;

pub(crate) type ComponentOverrideMap =
    rustc_hash::FxHashMap<String, rustc_hash::FxHashMap<String, String>>;
type OverrideFunctionMap = rustc_hash::FxHashMap<String, String>;

pub(crate) fn is_callable_component_type(class_type: &rumoca_ir_ast::ClassType) -> bool {
    !matches!(
        class_type,
        rumoca_ir_ast::ClassType::Package
            | rumoca_ir_ast::ClassType::Connector
            | rumoca_ir_ast::ClassType::Operator
    )
}

pub(crate) fn resolve_component_type_name(
    component: &rumoca_ir_ast::Component,
    tree: &ClassTree,
    class_scope: &str,
) -> Option<String> {
    if let Some(target_def_id) = component.type_def_id.or(component.type_name.def_id)
        && let Some(target_name) = tree.def_map.get(&target_def_id)
    {
        return Some(target_name.clone());
    }

    let raw_type_name = component.type_name.to_string();
    if raw_type_name.is_empty() {
        return None;
    }

    if tree.get_class_by_qualified_name(&raw_type_name).is_some() {
        return Some(raw_type_name);
    }

    resolve_class_in_scope(tree, &raw_type_name, class_scope).1
}

pub(crate) fn collect_component_constructor_aliases_for_class(
    tree: &ClassTree,
    class_def: &rumoca_ir_ast::ClassDef,
    class_scope: &str,
    visited_classes: &mut std::collections::HashSet<usize>,
    overrides: &mut rustc_hash::FxHashMap<String, String>,
) {
    let class_ptr = class_def as *const rumoca_ir_ast::ClassDef as usize;
    if !visited_classes.insert(class_ptr) {
        return;
    }

    for ext in &class_def.extends {
        let base_name = ext.base_name.to_string();
        let (base_class, resolved_base_name) = if let Some(base_def_id) = ext.base_def_id {
            (
                tree.get_class_by_def_id(base_def_id),
                tree.def_map.get(&base_def_id).cloned(),
            )
        } else {
            resolve_class_in_scope(tree, &base_name, class_scope)
        };

        let Some(base_class) = base_class else {
            continue;
        };
        let base_scope = resolved_base_name.unwrap_or(base_name);
        collect_component_constructor_aliases_for_class(
            tree,
            base_class,
            &base_scope,
            visited_classes,
            overrides,
        );
    }

    for (component_name, component) in &class_def.components {
        let Some(target_name) = resolve_component_type_name(component, tree, class_scope) else {
            continue;
        };
        let Some(target_class) = tree.get_class_by_qualified_name(&target_name) else {
            continue;
        };
        if !is_callable_component_type(&target_class.class_type) {
            continue;
        }
        // Derived classes should override inherited aliases with the same name.
        overrides.insert(component_name.clone(), target_name);
    }
}

pub(crate) fn collect_component_constructor_aliases(
    instance: &rumoca_ir_ast::InstanceData,
    tree: &ClassTree,
    overrides: &mut rustc_hash::FxHashMap<String, String>,
) {
    let Some(type_def_id) = instance.type_def_id else {
        return;
    };
    let Some(class_def) = tree.get_class_by_def_id(type_def_id) else {
        return;
    };
    let class_scope = tree
        .def_map
        .get(&type_def_id)
        .map(String::as_str)
        .unwrap_or(class_def.name.text.as_ref());
    let mut visited_classes = std::collections::HashSet::new();
    collect_component_constructor_aliases_for_class(
        tree,
        class_def,
        class_scope,
        &mut visited_classes,
        overrides,
    );
}

pub(crate) fn component_overrides(
    instance: &rumoca_ir_ast::InstanceData,
    tree: &ClassTree,
) -> rustc_hash::FxHashMap<String, String> {
    let mut overrides = rustc_hash::FxHashMap::default();
    collect_component_constructor_aliases(instance, tree, &mut overrides);
    for (alias, def_id) in &instance.class_overrides {
        if let Some(target_name) = tree.def_map.get(def_id) {
            overrides.insert(alias.clone(), target_name.clone());
        }
    }
    overrides
}

pub(crate) fn build_component_override_map(
    overlay: &InstanceOverlay,
    tree: &ClassTree,
) -> ComponentOverrideMap {
    overlay
        .components
        .values()
        .map(|instance| {
            (
                instance.qualified_name.to_flat_string(),
                component_overrides(instance, tree),
            )
        })
        .collect()
}

pub(crate) fn override_context_for_scope(
    scope: &str,
    component_override_map: &ComponentOverrideMap,
    tree: &ClassTree,
) -> (Vec<String>, OverrideFunctionMap) {
    fn apply_scope_override(
        alias: &str,
        target_name: &str,
        tree: &ClassTree,
        packages: &mut std::collections::BTreeSet<String>,
        function_overrides: &mut OverrideFunctionMap,
    ) {
        let Some(target_class) = tree.get_class_by_qualified_name(target_name) else {
            return;
        };
        match target_class.class_type {
            rumoca_ir_ast::ClassType::Package => {
                packages.insert(target_name.to_string());
            }
            _ => {
                function_overrides
                    .entry(alias.to_string())
                    .or_insert_with(|| target_name.to_string());
            }
        }
    }

    let mut packages = std::collections::BTreeSet::new();
    let mut function_overrides = OverrideFunctionMap::default();
    let mut current = Some(scope.to_string());
    while let Some(path) = current {
        if let Some(path_overrides) = component_override_map.get(&path) {
            for (alias, target_name) in path_overrides {
                apply_scope_override(
                    alias,
                    target_name,
                    tree,
                    &mut packages,
                    &mut function_overrides,
                );
            }
        }
        current = crate::path_utils::parent_scope(&path).map(str::to_string);
    }
    (packages.into_iter().collect(), function_overrides)
}

pub(crate) fn collect_package_chain(
    tree: &ClassTree,
    package_name: &str,
    chain: &mut Vec<String>,
    visited: &mut std::collections::HashSet<String>,
) {
    if !visited.insert(package_name.to_string()) {
        return;
    }
    chain.push(package_name.to_string());

    let Some(class_def) = tree.get_class_by_qualified_name(package_name) else {
        return;
    };

    for ext in &class_def.extends {
        let base_name = ext.base_name.to_string();
        let resolved_base = resolve_class_in_scope(tree, &base_name, package_name)
            .1
            .or_else(|| {
                tree.get_class_by_qualified_name(&base_name)
                    .map(|_| base_name.clone())
            });
        if let Some(base) = resolved_base {
            collect_package_chain(tree, &base, chain, visited);
        }
    }
}

pub(crate) fn package_chain_contains(
    tree: &ClassTree,
    package_name: &str,
    query_prefix: &str,
) -> bool {
    let mut chain = Vec::new();
    let mut visited = std::collections::HashSet::new();
    collect_package_chain(tree, package_name, &mut chain, &mut visited);
    chain.iter().any(|name| name == query_prefix)
}

pub(crate) fn resolve_function_in_package_chain(
    tree: &ClassTree,
    package_name: &str,
    function_leaf: &str,
) -> Option<String> {
    fn resolve_inner(
        tree: &ClassTree,
        package_name: &str,
        function_leaf: &str,
        visited: &mut std::collections::HashSet<String>,
    ) -> Option<String> {
        if !visited.insert(package_name.to_string()) {
            return None;
        }

        let direct = format!("{package_name}.{function_leaf}");
        if let Some(class_def) = tree.get_class_by_qualified_name(&direct)
            && class_def.class_type == rumoca_ir_ast::ClassType::Function
        {
            return Some(direct);
        }

        let class_def = tree.get_class_by_qualified_name(package_name)?;
        for ext in &class_def.extends {
            let base_name = ext.base_name.to_string();
            let resolved_base = resolve_class_in_scope(tree, &base_name, package_name)
                .1
                .or_else(|| {
                    tree.get_class_by_qualified_name(&base_name)
                        .map(|_| base_name.clone())
                });
            if let Some(base) = resolved_base
                && let Some(found) = resolve_inner(tree, &base, function_leaf, visited)
            {
                return Some(found);
            }
        }

        None
    }

    let mut visited = std::collections::HashSet::new();
    resolve_inner(tree, package_name, function_leaf, &mut visited)
}

pub(crate) fn resolve_override_function_name(
    current_name: &str,
    tree: &ClassTree,
    override_packages: &[String],
) -> Option<String> {
    let (current_prefix, leaf) = current_name.rsplit_once('.')?;
    let mut matches = std::collections::BTreeSet::new();

    for package_name in override_packages {
        if !package_chain_contains(tree, package_name, current_prefix) {
            continue;
        }
        if let Some(resolved) = resolve_function_in_package_chain(tree, package_name, leaf) {
            matches.insert(resolved);
        }
    }

    if matches.len() == 1 {
        return matches.into_iter().next();
    }

    let non_current: Vec<String> = matches
        .into_iter()
        .filter(|candidate| candidate.as_str() != current_name)
        .collect();
    if non_current.len() == 1 {
        non_current.into_iter().next()
    } else {
        None
    }
}

pub(crate) fn resolve_function_extends_fallback(
    tree: &ClassTree,
    current_name: &str,
) -> Option<String> {
    let mut current = current_name.to_string();
    let mut visited = std::collections::HashSet::new();

    loop {
        if !visited.insert(current.clone()) {
            return None;
        }
        let class_def = tree.get_class_by_qualified_name(&current)?;
        if class_def.class_type != rumoca_ir_ast::ClassType::Function {
            return None;
        }
        if !class_def.algorithms.is_empty() || class_def.external.is_some() {
            return (current != current_name).then_some(current);
        }

        let mut next_function: Option<String> = None;
        for ext in &class_def.extends {
            let base_name = ext.base_name.to_string();
            let resolved_base = resolve_class_in_scope(tree, &base_name, &current)
                .1
                .or_else(|| {
                    tree.get_class_by_qualified_name(&base_name)
                        .map(|_| base_name.clone())
                });
            let Some(candidate) = resolved_base else {
                continue;
            };
            let Some(candidate_class) = tree.get_class_by_qualified_name(&candidate) else {
                continue;
            };
            if candidate_class.class_type != rumoca_ir_ast::ClassType::Function {
                continue;
            }
            match &next_function {
                Some(existing) if existing != &candidate => return None,
                Some(_) => {}
                None => next_function = Some(candidate),
            }
        }
        current = next_function?;
    }
}

pub(crate) struct FunctionOverrideRewriteContext<'a> {
    tree: &'a ClassTree,
    override_packages: &'a [String],
    override_functions: &'a OverrideFunctionMap,
}

pub(crate) fn resolve_function_override_name(
    current_name: &str,
    is_constructor: bool,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Option<String> {
    let leaf = current_name.rsplit('.').next().unwrap_or(current_name);
    let mut resolved = ctx.override_functions.get(leaf).and_then(|candidate| {
        let class_def = ctx.tree.get_class_by_qualified_name(candidate)?;
        let can_rewrite = if is_constructor {
            !matches!(class_def.class_type, rumoca_ir_ast::ClassType::Package)
        } else {
            class_def.class_type == rumoca_ir_ast::ClassType::Function
        };
        can_rewrite.then(|| candidate.clone())
    });
    if resolved.as_deref() == Some(current_name) {
        resolved = None;
    }
    if !is_constructor {
        if resolved.is_none()
            && let Some(candidate) =
                resolve_override_function_name(current_name, ctx.tree, ctx.override_packages)
            && candidate != current_name
        {
            resolved = Some(candidate);
        }
        if resolved.is_none() {
            resolved = resolve_function_extends_fallback(ctx.tree, current_name);
        }
    }
    resolved
}

pub(crate) fn rewrite_function_overrides_in_expression_list(
    expressions: &mut [Expression],
    ctx: &FunctionOverrideRewriteContext<'_>,
) {
    for expression in expressions {
        rewrite_function_overrides_in_expression_with_ctx(expression, ctx);
    }
}

pub(crate) fn rewrite_function_overrides_in_statement_list(
    statements: &mut [rumoca_ir_flat::Statement],
    ctx: &FunctionOverrideRewriteContext<'_>,
) {
    for statement in statements {
        rewrite_function_overrides_in_statement_with_ctx(statement, ctx);
    }
}

pub(crate) fn rewrite_function_overrides_in_expression_with_ctx(
    expr: &mut Expression,
    ctx: &FunctionOverrideRewriteContext<'_>,
) {
    match expr {
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => {
            if let Some(resolved_name) =
                resolve_function_override_name(name.as_str(), *is_constructor, ctx)
            {
                *name = VarName::new(resolved_name);
            }
            rewrite_function_overrides_in_expression_list(args, ctx);
        }
        Expression::BuiltinCall { args, .. } => {
            rewrite_function_overrides_in_expression_list(args, ctx);
        }
        Expression::Binary { lhs, rhs, .. } => {
            rewrite_function_overrides_in_expression_with_ctx(lhs, ctx);
            rewrite_function_overrides_in_expression_with_ctx(rhs, ctx);
        }
        Expression::Unary { rhs, .. } => {
            rewrite_function_overrides_in_expression_with_ctx(rhs, ctx);
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (condition, value) in branches {
                rewrite_function_overrides_in_expression_with_ctx(condition, ctx);
                rewrite_function_overrides_in_expression_with_ctx(value, ctx);
            }
            rewrite_function_overrides_in_expression_with_ctx(else_branch, ctx);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            rewrite_function_overrides_in_expression_list(elements, ctx);
        }
        Expression::Range { start, step, end } => {
            rewrite_function_overrides_in_expression_with_ctx(start, ctx);
            if let Some(step_expression) = step {
                rewrite_function_overrides_in_expression_with_ctx(step_expression, ctx);
            }
            rewrite_function_overrides_in_expression_with_ctx(end, ctx);
        }
        Expression::Index { base, subscripts } => {
            rewrite_function_overrides_in_expression_with_ctx(base, ctx);
            for subscript in subscripts {
                if let Subscript::Expr(expression) = subscript {
                    rewrite_function_overrides_in_expression_with_ctx(expression, ctx);
                }
            }
        }
        Expression::FieldAccess { base, .. } => {
            rewrite_function_overrides_in_expression_with_ctx(base, ctx);
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            rewrite_function_overrides_in_expression_with_ctx(expr, ctx);
            for index in indices {
                rewrite_function_overrides_in_expression_with_ctx(&mut index.range, ctx);
            }
            if let Some(filter_expression) = filter {
                rewrite_function_overrides_in_expression_with_ctx(filter_expression, ctx);
            }
        }
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => {}
    }
}

pub(crate) fn rewrite_function_overrides_in_when_equation_with_ctx(
    eq: &mut rumoca_ir_flat::WhenEquation,
    ctx: &FunctionOverrideRewriteContext<'_>,
) {
    match eq {
        flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
            rewrite_function_overrides_in_expression_with_ctx(value, ctx);
        }
        flat::WhenEquation::Assert { condition, .. } => {
            rewrite_function_overrides_in_expression_with_ctx(condition, ctx);
        }
        flat::WhenEquation::Terminate { .. } => {}
        flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (condition, equations) in branches {
                rewrite_function_overrides_in_expression_with_ctx(condition, ctx);
                for nested_equation in equations {
                    rewrite_function_overrides_in_when_equation_with_ctx(nested_equation, ctx);
                }
            }
            for nested_equation in else_branch {
                rewrite_function_overrides_in_when_equation_with_ctx(nested_equation, ctx);
            }
        }
        flat::WhenEquation::FunctionCallOutputs { function, .. } => {
            rewrite_function_overrides_in_expression_with_ctx(function, ctx);
        }
    }
}

pub(crate) fn rewrite_function_overrides_in_when_clause_with_ctx(
    clause: &mut rumoca_ir_flat::WhenClause,
    ctx: &FunctionOverrideRewriteContext<'_>,
) {
    rewrite_function_overrides_in_expression_with_ctx(&mut clause.condition, ctx);
    for equation in &mut clause.equations {
        rewrite_function_overrides_in_when_equation_with_ctx(equation, ctx);
    }
}

pub(crate) fn rewrite_function_overrides_in_statement_with_ctx(
    stmt: &mut rumoca_ir_flat::Statement,
    ctx: &FunctionOverrideRewriteContext<'_>,
) {
    match stmt {
        flat::Statement::Empty | flat::Statement::Return | flat::Statement::Break => {}
        flat::Statement::Assignment { value, .. } | flat::Statement::Reinit { value, .. } => {
            rewrite_function_overrides_in_expression_with_ctx(value, ctx);
        }
        flat::Statement::For { indices, equations } => {
            for index in indices {
                rewrite_function_overrides_in_expression_with_ctx(&mut index.range, ctx);
            }
            rewrite_function_overrides_in_statement_list(equations, ctx);
        }
        flat::Statement::While(block) => {
            rewrite_function_overrides_in_expression_with_ctx(&mut block.cond, ctx);
            rewrite_function_overrides_in_statement_list(&mut block.stmts, ctx);
        }
        flat::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                rewrite_function_overrides_in_expression_with_ctx(&mut block.cond, ctx);
                rewrite_function_overrides_in_statement_list(&mut block.stmts, ctx);
            }
            if let Some(statements) = else_block {
                rewrite_function_overrides_in_statement_list(statements, ctx);
            }
        }
        flat::Statement::When(blocks) => {
            for block in blocks {
                rewrite_function_overrides_in_expression_with_ctx(&mut block.cond, ctx);
                rewrite_function_overrides_in_statement_list(&mut block.stmts, ctx);
            }
        }
        flat::Statement::FunctionCall { args, outputs, .. } => {
            rewrite_function_overrides_in_expression_list(args, ctx);
            rewrite_function_overrides_in_expression_list(outputs, ctx);
        }
        flat::Statement::Assert {
            condition,
            message,
            level,
        } => {
            rewrite_function_overrides_in_expression_with_ctx(condition, ctx);
            rewrite_function_overrides_in_expression_with_ctx(message, ctx);
            if let Some(level_expression) = level {
                rewrite_function_overrides_in_expression_with_ctx(level_expression, ctx);
            }
        }
    }
}

pub(crate) fn rewrite_function_overrides_in_expression(
    expr: &mut Expression,
    tree: &ClassTree,
    override_packages: &[String],
    override_functions: &OverrideFunctionMap,
) {
    let ctx = FunctionOverrideRewriteContext {
        tree,
        override_packages,
        override_functions,
    };
    rewrite_function_overrides_in_expression_with_ctx(expr, &ctx);
}

pub(crate) fn rewrite_function_overrides_in_when_clause(
    clause: &mut rumoca_ir_flat::WhenClause,
    tree: &ClassTree,
    override_packages: &[String],
    override_functions: &OverrideFunctionMap,
) {
    let ctx = FunctionOverrideRewriteContext {
        tree,
        override_packages,
        override_functions,
    };
    rewrite_function_overrides_in_when_clause_with_ctx(clause, &ctx);
}

pub(crate) fn rewrite_function_overrides_in_statement(
    stmt: &mut rumoca_ir_flat::Statement,
    tree: &ClassTree,
    override_packages: &[String],
    override_functions: &OverrideFunctionMap,
) {
    let ctx = FunctionOverrideRewriteContext {
        tree,
        override_packages,
        override_functions,
    };
    rewrite_function_overrides_in_statement_with_ctx(stmt, &ctx);
}

pub(crate) fn rewrite_function_overrides_in_algorithm(
    algorithm: &mut Algorithm,
    tree: &ClassTree,
    override_packages: &[String],
    override_functions: &OverrideFunctionMap,
) {
    for stmt in &mut algorithm.statements {
        rewrite_function_overrides_in_statement(stmt, tree, override_packages, override_functions);
    }
}

pub(crate) fn rewrite_function_overrides_in_flattened(
    flattened: &mut equations::FlattenedEquations,
    tree: &ClassTree,
    override_packages: &[String],
    override_functions: &OverrideFunctionMap,
) {
    for equation in &mut flattened.equations {
        rewrite_function_overrides_in_expression(
            &mut equation.residual,
            tree,
            override_packages,
            override_functions,
        );
    }
    for assert_eq in &mut flattened.assert_equations {
        rewrite_function_overrides_in_expression(
            &mut assert_eq.condition,
            tree,
            override_packages,
            override_functions,
        );
        rewrite_function_overrides_in_expression(
            &mut assert_eq.message,
            tree,
            override_packages,
            override_functions,
        );
        if let Some(level_expr) = &mut assert_eq.level {
            rewrite_function_overrides_in_expression(
                level_expr,
                tree,
                override_packages,
                override_functions,
            );
        }
    }
    for clause in &mut flattened.when_clauses {
        rewrite_function_overrides_in_when_clause(
            clause,
            tree,
            override_packages,
            override_functions,
        );
    }
}

pub(crate) fn rewrite_function_overrides_in_flat_variable(
    variable: &mut rumoca_ir_flat::Variable,
    tree: &ClassTree,
    override_packages: &[String],
    override_functions: &OverrideFunctionMap,
) {
    if let Some(binding) = &mut variable.binding {
        rewrite_function_overrides_in_expression(
            binding,
            tree,
            override_packages,
            override_functions,
        );
    }
    if let Some(start) = &mut variable.start {
        rewrite_function_overrides_in_expression(
            start,
            tree,
            override_packages,
            override_functions,
        );
    }
    if let Some(min) = &mut variable.min {
        rewrite_function_overrides_in_expression(min, tree, override_packages, override_functions);
    }
    if let Some(max) = &mut variable.max {
        rewrite_function_overrides_in_expression(max, tree, override_packages, override_functions);
    }
    if let Some(nominal) = &mut variable.nominal {
        rewrite_function_overrides_in_expression(
            nominal,
            tree,
            override_packages,
            override_functions,
        );
    }
}

pub(crate) fn rewrite_function_extends_aliases_in_flat_functions(
    flat: &mut Model,
    tree: &ClassTree,
) {
    let override_packages: Vec<String> = Vec::new();
    let override_functions = OverrideFunctionMap::default();
    for function in flat.functions.values_mut() {
        for param in function
            .inputs
            .iter_mut()
            .chain(function.outputs.iter_mut())
            .chain(function.locals.iter_mut())
        {
            if let Some(default_expr) = &mut param.default {
                rewrite_function_overrides_in_expression(
                    default_expr,
                    tree,
                    &override_packages,
                    &override_functions,
                );
            }
        }
        for stmt in &mut function.body {
            rewrite_function_overrides_in_statement(
                stmt,
                tree,
                &override_packages,
                &override_functions,
            );
        }
    }
}

/// Propagate array dims from unexpanded record array parents to scalar field variables.
///
/// When `Complex[3] aw` isn't array-expanded (dims not evaluable at instantiation),
/// fields like `aw.re` are scalar (dims=[]). This propagates parent dims=[3] so
/// that `aw.re` correctly counts as 3 scalars in todae balance checking.
///
/// Only propagates when the parent's per-element variables DON'T exist (i.e.,
/// `aw[1].re` is absent), meaning the expansion didn't happen.
type ParentDims = Vec<(String, Vec<i64>)>;
pub(crate) type DimMap = std::collections::HashMap<String, Vec<i64>>;

pub(crate) fn dims_have_prefix(dims: &[i64], prefix: &[i64]) -> bool {
    dims.len() >= prefix.len() && dims.iter().zip(prefix.iter()).all(|(a, b)| a == b)
}

pub(crate) fn join_parent_child_dims(parent: &[i64], child: &[i64]) -> Vec<i64> {
    let mut dims = Vec::with_capacity(parent.len() + child.len());
    dims.extend(parent.iter().copied());
    dims.extend(child.iter().copied());
    dims
}

pub(crate) fn normalize_inferred_dims_for_parent(inferred: &[i64], parent: &[i64]) -> Vec<i64> {
    if inferred.is_empty() {
        return parent.to_vec();
    }
    if dims_have_prefix(inferred, parent) {
        return inferred.to_vec();
    }
    join_parent_child_dims(parent, inferred)
}

pub(crate) fn matching_parent<'a>(
    name: &str,
    parent_dims: &'a ParentDims,
) -> Option<(&'a str, &'a [i64])> {
    for (prefix, dims) in parent_dims {
        let has_prefix = name.starts_with(prefix.as_str());
        let is_field = name.as_bytes().get(prefix.len()) == Some(&b'.');
        if has_prefix && is_field {
            return Some((prefix.as_str(), dims.as_slice()));
        }
    }
    None
}

pub(crate) fn infer_function_call_dims(
    name: &str,
    function_output_dims: &DimMap,
) -> Option<Vec<i64>> {
    if let Some(dims) = function_output_dims.get(name) {
        return Some(dims.clone());
    }

    let leaf = crate::path_utils::split_path_with_indices(name)
        .into_iter()
        .last()?;
    let mut matched_dims: Option<Vec<i64>> = None;
    for (fn_name, dims) in function_output_dims {
        let same_leaf = crate::path_utils::split_path_with_indices(fn_name)
            .into_iter()
            .last()
            .is_some_and(|candidate| candidate == leaf);
        if !same_leaf {
            continue;
        }
        match &matched_dims {
            Some(existing) if existing != dims => return None,
            Some(_) => {}
            None => matched_dims = Some(dims.clone()),
        }
    }
    matched_dims
}

pub(crate) fn infer_array_dims_from_expression(
    elements: &[Expression],
    is_matrix: bool,
    var_dims: &DimMap,
    function_output_dims: &DimMap,
) -> Option<Vec<i64>> {
    if elements.is_empty() {
        return Some(vec![0]);
    }
    if is_matrix {
        return match elements.first() {
            Some(Expression::Array { elements: row, .. }) => {
                Some(vec![elements.len() as i64, row.len() as i64])
            }
            _ => Some(vec![1, elements.len() as i64]),
        };
    }

    let mut dims = vec![elements.len() as i64];
    let inner_dims = elements
        .iter()
        .find_map(|element| infer_expr_dims(element, var_dims, function_output_dims));
    if let Some(inner) = inner_dims {
        dims.extend(inner);
    }
    Some(dims)
}

pub(crate) fn infer_array_comprehension_dims(
    expr: &Expression,
    indices: &[rumoca_ir_flat::ComprehensionIndex],
    filter: Option<&Expression>,
    var_dims: &DimMap,
    function_output_dims: &DimMap,
) -> Option<Vec<i64>> {
    if filter.is_some() {
        return None;
    }

    let mut dims = Vec::with_capacity(indices.len().saturating_add(1));
    for index in indices {
        let range_dims = infer_expr_dims(&index.range, var_dims, function_output_dims)
            .or_else(|| infer_array_dimensions(&index.range))?;
        if range_dims.is_empty() {
            return None;
        }
        let range_size = range_dims
            .iter()
            .copied()
            .fold(1i64, |acc, dim| acc.saturating_mul(dim.max(0)));
        dims.push(range_size);
    }

    if let Some(mut body_dims) = infer_expr_dims(expr, var_dims, function_output_dims)
        .or_else(|| infer_array_dimensions(expr))
    {
        dims.append(&mut body_dims);
    }
    Some(dims)
}

pub(crate) fn infer_expr_dims(
    expr: &Expression,
    var_dims: &DimMap,
    function_output_dims: &DimMap,
) -> Option<Vec<i64>> {
    match expr {
        Expression::Array {
            elements,
            is_matrix,
        } => infer_array_dims_from_expression(elements, *is_matrix, var_dims, function_output_dims),
        Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            var_dims.get(name.as_str()).cloned()
        }
        Expression::VarRef { .. } | Expression::BuiltinCall { .. } => None,
        Expression::FunctionCall { name, .. } => {
            infer_function_call_dims(name.as_str(), function_output_dims)
        }
        Expression::Binary { lhs, rhs, .. } => {
            let lhs_dims = infer_expr_dims(lhs, var_dims, function_output_dims);
            let rhs_dims = infer_expr_dims(rhs, var_dims, function_output_dims);
            match (lhs_dims, rhs_dims) {
                (Some(l), Some(r)) if l == r => Some(l),
                (Some(l), Some(r)) if l.is_empty() && !r.is_empty() => Some(r),
                (Some(l), Some(r)) if r.is_empty() && !l.is_empty() => Some(l),
                (Some(l), None) if !l.is_empty() => Some(l),
                (None, Some(r)) if !r.is_empty() => Some(r),
                _ => None,
            }
        }
        Expression::Unary { rhs, .. } => infer_expr_dims(rhs, var_dims, function_output_dims),
        Expression::Literal(_) => Some(Vec::new()),
        Expression::If {
            branches,
            else_branch,
        } => branches
            .iter()
            .find_map(|(_cond, branch_expr)| {
                infer_expr_dims(branch_expr, var_dims, function_output_dims)
            })
            .or_else(|| infer_expr_dims(else_branch, var_dims, function_output_dims)),
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => infer_array_comprehension_dims(
            expr,
            indices,
            filter.as_deref(),
            var_dims,
            function_output_dims,
        ),
        Expression::Tuple { .. }
        | Expression::Range { .. }
        | Expression::Index { .. }
        | Expression::FieldAccess { .. }
        | Expression::Empty => None,
    }
}

pub(crate) fn collect_parent_dims(flat: &Model, overlay: &InstanceOverlay) -> ParentDims {
    overlay
        .components
        .values()
        .filter(|inst| !inst.is_primitive && !inst.dims.is_empty())
        .filter_map(|inst| {
            let path = inst.qualified_name.to_flat_string();
            let first_elem = format!("{path}[1].");
            let expanded = flat
                .variables
                .keys()
                .any(|k| k.as_str().starts_with(&first_elem));
            if expanded {
                None
            } else {
                Some((path, inst.dims.clone()))
            }
        })
        .collect()
}

pub(crate) fn collect_function_output_dims(flat: &Model) -> DimMap {
    flat.functions
        .iter()
        .filter_map(|(name, function)| {
            function
                .outputs
                .first()
                .map(|output| (name.as_str().to_string(), output.dims.clone()))
        })
        .collect()
}

pub(crate) fn infer_and_normalize_dims(
    expr: &Expression,
    parent: &[i64],
    var_dims: &DimMap,
    function_output_dims: &DimMap,
) -> Option<Vec<i64>> {
    infer_expr_dims(expr, var_dims, function_output_dims)
        .or_else(|| infer_array_dimensions(expr))
        .map(|dims| normalize_inferred_dims_for_parent(&dims, parent))
}

pub(crate) fn choose_more_specific_dims(first: Vec<i64>, second: Vec<i64>) -> Vec<i64> {
    if second.len() > first.len() {
        return second;
    }
    if first.len() > second.len() {
        return first;
    }

    let first_size = first.iter().product::<i64>();
    let second_size = second.iter().product::<i64>();
    if second_size > first_size {
        second
    } else {
        first
    }
}

pub(crate) fn infer_best_dims_for_var(
    var: &rumoca_ir_flat::Variable,
    parent: &[i64],
    var_dims: &DimMap,
    function_output_dims: &DimMap,
) -> Option<Vec<i64>> {
    let inferred_from_binding = var.binding.as_ref().and_then(|binding| {
        infer_and_normalize_dims(binding, parent, var_dims, function_output_dims)
    });
    let inferred_from_start = var
        .start
        .as_ref()
        .and_then(|start| infer_and_normalize_dims(start, parent, var_dims, function_output_dims));

    match (inferred_from_binding, inferred_from_start) {
        (Some(binding), Some(start)) => Some(choose_more_specific_dims(binding, start)),
        (Some(binding), None) => Some(binding),
        (None, Some(start)) => Some(start),
        (None, None) => {
            if var.binding.is_none() {
                Some(parent.to_vec())
            } else {
                None
            }
        }
    }
}

pub(crate) fn recover_nested_dims_from_bindings(
    flat: &mut Model,
    parent_dims: &ParentDims,
    function_output_dims: &DimMap,
) {
    // Recover nested dimensions from bindings/start values, e.g.:
    // `cylinders.lengthDirection = {v1, v2}` -> `[n, 3]`.
    for _ in 0..2 {
        let var_dims_lookup: DimMap = flat
            .variables
            .iter()
            .map(|(name, var)| (name.as_str().to_string(), var.dims.clone()))
            .collect();

        let mut changed = false;
        for var in flat.variables.values_mut() {
            let Some((_, parent)) = matching_parent(var.name.as_str(), parent_dims) else {
                continue;
            };
            let Some(inferred) =
                infer_best_dims_for_var(var, parent, &var_dims_lookup, function_output_dims)
            else {
                continue;
            };
            if inferred.len() > var.dims.len() {
                var.dims = inferred;
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
}

pub(crate) fn prepend_missing_parent_dims(flat: &mut Model, parent_dims: &ParentDims) {
    for var in flat.variables.values_mut() {
        let Some((_, parent)) = matching_parent(var.name.as_str(), parent_dims) else {
            continue;
        };
        if dims_have_prefix(&var.dims, parent) {
            continue;
        }
        var.dims = join_parent_child_dims(parent, &var.dims);
    }
}

pub(crate) fn build_child_dim_hints(flat: &Model, parent_dims: &ParentDims) -> DimMap {
    let mut child_dim_hints = DimMap::new();
    for var in flat.variables.values() {
        let Some((prefix, parent)) = matching_parent(var.name.as_str(), parent_dims) else {
            continue;
        };
        if !dims_have_prefix(&var.dims, parent) || var.dims.len() <= parent.len() {
            continue;
        }
        let Some(suffix) = var
            .name
            .as_str()
            .strip_prefix(prefix)
            .and_then(|rest| rest.strip_prefix('.'))
        else {
            continue;
        };
        let child_dims = var.dims[parent.len()..].to_vec();
        let replace = child_dim_hints
            .get(suffix)
            .is_none_or(|existing| child_dims.len() > existing.len());
        if replace {
            child_dim_hints.insert(suffix.to_string(), child_dims);
        }
    }
    child_dim_hints
}

pub(crate) fn complete_child_dims_from_hints(
    flat: &mut Model,
    parent_dims: &ParentDims,
    hints: &DimMap,
) {
    for var in flat.variables.values_mut() {
        let Some((prefix, parent)) = matching_parent(var.name.as_str(), parent_dims) else {
            continue;
        };
        if !dims_have_prefix(&var.dims, parent) {
            continue;
        }
        let Some(suffix) = var
            .name
            .as_str()
            .strip_prefix(prefix)
            .and_then(|rest| rest.strip_prefix('.'))
        else {
            continue;
        };
        let Some(hinted_child_dims) = hints.get(suffix) else {
            continue;
        };
        let current_child_len = var.dims.len().saturating_sub(parent.len());
        if current_child_len < hinted_child_dims.len() {
            var.dims = join_parent_child_dims(parent, hinted_child_dims);
        }
    }
}

pub(crate) fn propagate_unexpanded_record_array_dims(flat: &mut Model, overlay: &InstanceOverlay) {
    let mut parent_dims = collect_parent_dims(flat, overlay);
    if parent_dims.is_empty() {
        return;
    }
    // Longest prefix wins when parents are nested.
    parent_dims.sort_by_key(|entry| std::cmp::Reverse(entry.0.len()));

    let function_output_dims = collect_function_output_dims(flat);
    recover_nested_dims_from_bindings(flat, &parent_dims, &function_output_dims);
    prepend_missing_parent_dims(flat, &parent_dims);
    let child_dim_hints = build_child_dim_hints(flat, &parent_dims);
    complete_child_dims_from_hints(flat, &parent_dims, &child_dim_hints);
}
