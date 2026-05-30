use super::*;

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
    stmt: &mut rumoca_core::Statement,
    ctx: &FunctionOverrideRewriteContext<'_>,
) {
    *stmt = FunctionOverrideExpressionRewriter { ctx }.rewrite_statement(stmt);
}

pub(crate) fn rewrite_function_overrides_in_expression(
    expr: &mut Expression,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_packages: &[OverrideTarget],
    override_functions: &OverrideFunctionMap,
) {
    let ctx = FunctionOverrideRewriteContext::new(
        tree,
        class_index,
        override_packages,
        override_functions,
    );
    rewrite_function_overrides_in_expression_with_ctx(expr, &ctx);
}

pub(super) fn rewrite_function_overrides_in_expression_for_scope(
    expr: &mut Expression,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_packages: &[OverrideTarget],
    override_functions: &OverrideFunctionMap,
    active_scope: ComponentPath,
) {
    let ctx = FunctionOverrideRewriteContext::new(
        tree,
        class_index,
        override_packages,
        override_functions,
    )
    .with_active_scope(active_scope);
    rewrite_function_overrides_in_expression_with_ctx(expr, &ctx);
}

pub(crate) fn rewrite_function_overrides_in_when_clause(
    clause: &mut rumoca_ir_flat::WhenClause,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_packages: &[OverrideTarget],
    override_functions: &OverrideFunctionMap,
) {
    let ctx = FunctionOverrideRewriteContext::new(
        tree,
        class_index,
        override_packages,
        override_functions,
    );
    rewrite_function_overrides_in_when_clause_with_ctx(clause, &ctx);
}

pub(crate) fn rewrite_function_overrides_in_statement(
    stmt: &mut rumoca_core::Statement,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_packages: &[OverrideTarget],
    override_functions: &OverrideFunctionMap,
) {
    let ctx = FunctionOverrideRewriteContext::new(
        tree,
        class_index,
        override_packages,
        override_functions,
    );
    rewrite_function_overrides_in_statement_with_ctx(stmt, &ctx);
}

pub(crate) fn rewrite_function_overrides_in_algorithm(
    algorithm: &mut Algorithm,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_packages: &[OverrideTarget],
    override_functions: &OverrideFunctionMap,
) {
    for stmt in &mut algorithm.statements {
        rewrite_function_overrides_in_statement(
            stmt,
            tree,
            class_index,
            override_packages,
            override_functions,
        );
    }
}

pub(crate) fn rewrite_function_overrides_in_flattened(
    flattened: &mut equations::FlattenedEquations,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_packages: &[OverrideTarget],
    override_functions: &OverrideFunctionMap,
) {
    for equation in &mut flattened.equations {
        rewrite_function_overrides_in_expression(
            &mut equation.residual,
            tree,
            class_index,
            override_packages,
            override_functions,
        );
    }
    for assert_eq in &mut flattened.assert_equations {
        rewrite_function_overrides_in_expression(
            &mut assert_eq.condition,
            tree,
            class_index,
            override_packages,
            override_functions,
        );
        rewrite_function_overrides_in_expression(
            &mut assert_eq.message,
            tree,
            class_index,
            override_packages,
            override_functions,
        );
        if let Some(level_expr) = &mut assert_eq.level {
            rewrite_function_overrides_in_expression(
                level_expr,
                tree,
                class_index,
                override_packages,
                override_functions,
            );
        }
    }
    for clause in &mut flattened.when_clauses {
        rewrite_function_overrides_in_when_clause(
            clause,
            tree,
            class_index,
            override_packages,
            override_functions,
        );
    }
}

pub(crate) fn rewrite_function_overrides_in_flat_variable(
    variable: &mut rumoca_ir_flat::Variable,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_packages: &[OverrideTarget],
    override_functions: &OverrideFunctionMap,
    active_scope: &ComponentPath,
) {
    if let Some(binding) = &mut variable.binding {
        rewrite_function_overrides_in_expression_for_scope(
            binding,
            tree,
            class_index,
            override_packages,
            override_functions,
            active_scope.clone(),
        );
    }
    if let Some(start) = &mut variable.start {
        rewrite_function_overrides_in_expression_for_scope(
            start,
            tree,
            class_index,
            override_packages,
            override_functions,
            active_scope.clone(),
        );
    }
    if let Some(min) = &mut variable.min {
        rewrite_function_overrides_in_expression_for_scope(
            min,
            tree,
            class_index,
            override_packages,
            override_functions,
            active_scope.clone(),
        );
    }
    if let Some(max) = &mut variable.max {
        rewrite_function_overrides_in_expression_for_scope(
            max,
            tree,
            class_index,
            override_packages,
            override_functions,
            active_scope.clone(),
        );
    }
    if let Some(nominal) = &mut variable.nominal {
        rewrite_function_overrides_in_expression_for_scope(
            nominal,
            tree,
            class_index,
            override_packages,
            override_functions,
            active_scope.clone(),
        );
    }
}

pub(super) fn flat_variable_needs_override_rewrite(variable: &rumoca_ir_flat::Variable) -> bool {
    variable.binding.is_some()
        || variable.start.is_some()
        || variable.min.is_some()
        || variable.max.is_some()
        || variable.nominal.is_some()
}

pub(crate) fn rewrite_function_overrides_in_flat_model(
    flat: &mut Model,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    component_override_map: &ComponentOverrideMap,
) {
    rewrite_function_overrides_in_flat_variables(flat, tree, class_index, component_override_map);
    rewrite_function_overrides_in_equations(flat, tree, class_index, component_override_map);
    let (override_packages, override_functions) =
        override_context_for_scope("", component_override_map);
    for equation in &mut flat.initial_equations {
        rewrite_function_overrides_in_expression(
            &mut equation.residual,
            tree,
            class_index,
            &override_packages,
            &override_functions,
        );
    }
    for assert_eq in &mut flat.assert_equations {
        rewrite_function_overrides_in_assert_equation(
            assert_eq,
            tree,
            class_index,
            &override_packages,
            &override_functions,
        );
    }
    for assert_eq in &mut flat.initial_assert_equations {
        rewrite_function_overrides_in_assert_equation(
            assert_eq,
            tree,
            class_index,
            &override_packages,
            &override_functions,
        );
    }
    for algorithm in &mut flat.algorithms {
        rewrite_function_overrides_in_algorithm(
            algorithm,
            tree,
            class_index,
            &override_packages,
            &override_functions,
        );
    }
    for algorithm in &mut flat.initial_algorithms {
        rewrite_function_overrides_in_algorithm(
            algorithm,
            tree,
            class_index,
            &override_packages,
            &override_functions,
        );
    }
    for clause in &mut flat.when_clauses {
        rewrite_function_overrides_in_when_clause(
            clause,
            tree,
            class_index,
            &override_packages,
            &override_functions,
        );
    }
    rewrite_function_overrides_in_flat_functions(
        flat,
        tree,
        class_index,
        &override_packages,
        &override_functions,
    );
}

fn rewrite_function_overrides_in_flat_variables(
    flat: &mut Model,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    component_override_map: &ComponentOverrideMap,
) {
    let mut contexts = rustc_hash::FxHashMap::<ComponentPath, OverrideContext>::default();
    for (name, variable) in &mut flat.variables {
        if !flat_variable_needs_override_rewrite(variable) {
            continue;
        }
        let scope_path = ComponentPath::from_flat_path(name.as_str());
        let cache_key = override_context_cache_key(&scope_path, component_override_map);
        let (override_packages, override_functions) = contexts
            .entry(cache_key.clone())
            .or_insert_with_key(|scope| {
                override_context_for_component_path(scope, component_override_map)
            });
        let active_scope = scope_path.parent().unwrap_or_else(|| cache_key.clone());
        rewrite_function_overrides_in_flat_variable(
            variable,
            tree,
            class_index,
            override_packages,
            override_functions,
            &active_scope,
        );
    }
}

fn rewrite_function_overrides_in_equations(
    flat: &mut Model,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    component_override_map: &ComponentOverrideMap,
) {
    let mut contexts = rustc_hash::FxHashMap::<ComponentPath, OverrideContext>::default();
    for equation in &mut flat.equations {
        let scope = equation
            .origin
            .binding_variable()
            .or_else(|| equation.origin.component_name())
            .unwrap_or("");
        let scope_path = ComponentPath::from_flat_path(scope);
        let cache_key = override_context_cache_key(&scope_path, component_override_map);
        let (override_packages, override_functions) = contexts
            .entry(cache_key.clone())
            .or_insert_with_key(|scope| {
                override_context_for_component_path(scope, component_override_map)
            });
        rewrite_function_overrides_in_expression_for_scope(
            &mut equation.residual,
            tree,
            class_index,
            override_packages,
            override_functions,
            cache_key,
        );
    }
}

pub(super) fn rewrite_function_overrides_in_assert_equation(
    assert_eq: &mut rumoca_ir_flat::AssertEquation,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_packages: &[OverrideTarget],
    override_functions: &OverrideFunctionMap,
) {
    rewrite_function_overrides_in_expression(
        &mut assert_eq.condition,
        tree,
        class_index,
        override_packages,
        override_functions,
    );
    rewrite_function_overrides_in_expression(
        &mut assert_eq.message,
        tree,
        class_index,
        override_packages,
        override_functions,
    );
    if let Some(level) = &mut assert_eq.level {
        rewrite_function_overrides_in_expression(
            level,
            tree,
            class_index,
            override_packages,
            override_functions,
        );
    }
}

pub(crate) fn rewrite_function_extends_aliases_in_flat_functions(
    flat: &mut Model,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) {
    for function in flat.functions.values_mut() {
        rewrite_function_extends_aliases_in_function(function, tree, class_index);
    }
}

pub(super) fn rewrite_function_overrides_in_flat_functions(
    flat: &mut Model,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    active_override_packages: &[OverrideTarget],
    active_override_functions: &OverrideFunctionMap,
) {
    for function in flat.functions.values_mut() {
        let mut override_packages = active_override_packages.to_vec();
        override_packages.extend(function_package_override_chain(
            function.name.as_str(),
            tree,
            class_index,
        ));
        rewrite_function_overrides_in_function(
            function,
            tree,
            class_index,
            &override_packages,
            active_override_functions,
        );
    }
}

pub(crate) fn rewrite_function_extends_aliases_in_function(
    function: &mut rumoca_core::Function,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) {
    let override_functions = OverrideFunctionMap::default();
    let override_packages =
        function_package_override_chain(function.name.as_str(), tree, class_index);
    rewrite_function_overrides_in_function(
        function,
        tree,
        class_index,
        &override_packages,
        &override_functions,
    );
}

pub(super) fn rewrite_function_overrides_in_function(
    function: &mut rumoca_core::Function,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_packages: &[OverrideTarget],
    override_functions: &OverrideFunctionMap,
) {
    let lexical_package_def_id =
        function_lexical_package_def_id(function.name.as_str(), class_index);
    let ctx = FunctionOverrideRewriteContext::new(
        tree,
        class_index,
        override_packages,
        override_functions,
    )
    .with_lexical_package_def_id(lexical_package_def_id)
    .with_local_def_ids(function_local_def_ids(function));
    for param in function
        .inputs
        .iter_mut()
        .chain(function.outputs.iter_mut())
        .chain(function.locals.iter_mut())
    {
        if let Some(default_expr) = &mut param.default {
            rewrite_function_overrides_in_expression_with_ctx(default_expr, &ctx);
        }
    }
    for stmt in &mut function.body {
        rewrite_function_overrides_in_statement_with_ctx(stmt, &ctx);
    }
    rewrite_function_self_package_calls(function, tree, class_index);
}

pub(super) fn rewrite_function_self_package_calls(
    function: &mut rumoca_core::Function,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) {
    let Some(package) = function_package_override_chain(function.name.as_str(), tree, class_index)
        .into_iter()
        .next()
    else {
        return;
    };
    let ctx = FunctionSelfPackageRewriteContext {
        tree,
        class_index,
        package,
    };
    for param in function
        .inputs
        .iter_mut()
        .chain(function.outputs.iter_mut())
        .chain(function.locals.iter_mut())
    {
        if let Some(default_expr) = &mut param.default {
            *default_expr =
                FunctionSelfPackageRewriter { ctx: &ctx }.rewrite_expression(default_expr);
        }
    }
    for stmt in &mut function.body {
        *stmt = FunctionSelfPackageRewriter { ctx: &ctx }.rewrite_statement(stmt);
    }
}

pub(super) struct FunctionSelfPackageRewriteContext<'a> {
    tree: &'a ClassTree,
    class_index: &'a rumoca_ir_ast::ClassDefIndex<'a>,
    package: OverrideTarget,
}

pub(super) struct FunctionSelfPackageRewriter<'a> {
    ctx: &'a FunctionSelfPackageRewriteContext<'a>,
}

impl ExpressionRewriter for FunctionSelfPackageRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        let Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } = expr
        else {
            return self.walk_expression(expr);
        };
        let rewritten_args = self.rewrite_expressions(args);
        if *is_constructor {
            return Expression::FunctionCall {
                name: name.clone(),
                args: rewritten_args,
                is_constructor: *is_constructor,
                span: *span,
            };
        }
        let Some(source_package_def_id) =
            reference_source_package_def_id_from_index(name, self.ctx.class_index)
        else {
            return Expression::FunctionCall {
                name: name.clone(),
                args: rewritten_args,
                is_constructor: *is_constructor,
                span: *span,
            };
        };
        if !package_chain_contains_def_id(
            self.ctx.tree,
            self.ctx.class_index,
            &self.ctx.package,
            source_package_def_id,
        ) {
            return Expression::FunctionCall {
                name: name.clone(),
                args: rewritten_args,
                is_constructor: *is_constructor,
                span: *span,
            };
        }
        let Some(resolved_name) = resolve_function_in_package_chain_exposed(
            self.ctx.tree,
            self.ctx.class_index,
            &self.ctx.package,
            name.last_segment(),
        )
        .filter(|resolved| resolved != name.as_str()) else {
            return Expression::FunctionCall {
                name: name.clone(),
                args: rewritten_args,
                is_constructor: *is_constructor,
                span: *span,
            };
        };
        Expression::FunctionCall {
            name: rewritten_function_reference(
                name,
                resolved_name,
                self.ctx.tree,
                self.ctx.class_index,
            ),
            args: rewritten_args,
            is_constructor: *is_constructor,
            span: *span,
        }
    }
}

impl StatementRewriter for FunctionSelfPackageRewriter<'_> {}

pub(super) fn function_lexical_package_def_id(
    function_name: &str,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> Option<rumoca_core::DefId> {
    let package_name = parent_scope(function_name)?;
    class_index
        .get_by_qualified_name(package_name)
        .and_then(|class_def| class_def.def_id)
}

pub(super) fn function_package_override_chain(
    function_name: &str,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> Vec<OverrideTarget> {
    let Some(package_name) = parent_scope(function_name) else {
        return Vec::new();
    };
    let Some(class_def) = class_index.get_by_qualified_name(package_name) else {
        return Vec::new();
    };
    let Some(def_id) = class_def.def_id else {
        return Vec::new();
    };
    vec![OverrideTarget {
        alias: top_level_last_segment(package_name).to_string(),
        name: tree
            .def_map
            .get(&def_id)
            .cloned()
            .unwrap_or_else(|| package_name.to_string()),
        def_id,
        class_type: class_def.class_type.clone(),
        active: false,
        modifier_args: Vec::new(),
    }]
}
