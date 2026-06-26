use super::*;
use crate::source_spans::required_location_span;

pub(crate) fn rewrite_function_overrides_in_when_equation_with_ctx(
    eq: &mut rumoca_ir_flat::WhenEquation,
    ctx: &FunctionOverrideRewriteContext<'_>,
) {
    match eq {
        flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
            rewrite_function_overrides_in_expression_with_ctx(value, ctx);
        }
        flat::WhenEquation::Assert {
            condition, message, ..
        } => {
            rewrite_function_overrides_in_expression_with_ctx(condition, ctx);
            rewrite_function_overrides_in_expression_with_ctx(message, ctx);
        }
        flat::WhenEquation::Terminate { message, .. } => {
            rewrite_function_overrides_in_expression_with_ctx(message, ctx);
        }
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

#[cfg(test)]
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

pub(crate) fn rewrite_function_overrides_in_when_clause_scoped(
    clause: &mut rumoca_ir_flat::WhenClause,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_packages: &[OverrideTarget],
    override_functions: &OverrideFunctionMap,
    active_scope: &ComponentPath,
    component_members: &component_member_scope::ComponentMemberScopes,
) {
    let ctx = FunctionOverrideRewriteContext::new(
        tree,
        class_index,
        override_packages,
        override_functions,
    )
    .with_active_scope(active_scope.clone())
    .with_component_member_scope(component_members);
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
    active_scope: &ComponentPath,
    component_members: &component_member_scope::ComponentMemberScopes,
) {
    let ctx = FunctionOverrideRewriteContext::new(
        tree,
        class_index,
        override_packages,
        override_functions,
    )
    .with_active_scope(active_scope.clone())
    .with_component_member_scope(component_members);
    for equation in &mut flattened.equations {
        rewrite_function_overrides_in_expression_with_ctx(&mut equation.residual, &ctx);
    }
    for assert_eq in &mut flattened.assert_equations {
        rewrite_function_overrides_in_expression_with_ctx(&mut assert_eq.condition, &ctx);
        rewrite_function_overrides_in_expression_with_ctx(&mut assert_eq.message, &ctx);
        if let Some(level_expr) = &mut assert_eq.level {
            rewrite_function_overrides_in_expression_with_ctx(level_expr, &ctx);
        }
    }
    for clause in &mut flattened.when_clauses {
        rewrite_function_overrides_in_when_clause_with_ctx(clause, &ctx);
    }
}

pub(crate) fn rewrite_function_overrides_in_flat_variable(
    variable: &mut rumoca_ir_flat::Variable,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_packages: &[OverrideTarget],
    override_functions: &OverrideFunctionMap,
    active_scope: &ComponentPath,
    component_members: &component_member_scope::ComponentMemberScopes,
) {
    let expression_ctx = || {
        FunctionOverrideRewriteContext::new(
            tree,
            class_index,
            override_packages,
            override_functions,
        )
        .with_active_scope(active_scope.clone())
        .with_component_member_scope(component_members)
    };
    if let Some(binding) = &mut variable.binding {
        rewrite_function_overrides_in_expression_with_ctx(binding, &expression_ctx());
    }
    if let Some(start) = &mut variable.start {
        rewrite_function_overrides_in_expression_with_ctx(start, &expression_ctx());
    }
    if let Some(min) = &mut variable.min {
        rewrite_function_overrides_in_expression_with_ctx(min, &expression_ctx());
    }
    if let Some(max) = &mut variable.max {
        rewrite_function_overrides_in_expression_with_ctx(max, &expression_ctx());
    }
    if let Some(nominal) = &mut variable.nominal {
        rewrite_function_overrides_in_expression_with_ctx(nominal, &expression_ctx());
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
    component_members: &component_member_scope::ComponentMemberScopes,
) -> Result<(), FlattenError> {
    rewrite_function_overrides_in_flat_variables(
        flat,
        tree,
        class_index,
        component_override_map,
        component_members,
    );
    rewrite_function_overrides_in_equations(
        flat,
        tree,
        class_index,
        component_override_map,
        component_members,
    );
    let (override_packages, override_functions) =
        override_context_for_scope("", component_override_map);
    let root_ctx = FunctionOverrideRewriteContext::new(
        tree,
        class_index,
        &override_packages,
        &override_functions,
    )
    .with_component_member_scope(component_members);
    for equation in &mut flat.initial_equations {
        rewrite_function_overrides_in_expression_with_ctx(&mut equation.residual, &root_ctx);
    }
    for assert_eq in &mut flat.assert_equations {
        rewrite_function_overrides_in_expression_with_ctx(&mut assert_eq.condition, &root_ctx);
        rewrite_function_overrides_in_expression_with_ctx(&mut assert_eq.message, &root_ctx);
        if let Some(level) = &mut assert_eq.level {
            rewrite_function_overrides_in_expression_with_ctx(level, &root_ctx);
        }
    }
    for assert_eq in &mut flat.initial_assert_equations {
        rewrite_function_overrides_in_expression_with_ctx(&mut assert_eq.condition, &root_ctx);
        rewrite_function_overrides_in_expression_with_ctx(&mut assert_eq.message, &root_ctx);
        if let Some(level) = &mut assert_eq.level {
            rewrite_function_overrides_in_expression_with_ctx(level, &root_ctx);
        }
    }
    for algorithm in &mut flat.algorithms {
        for stmt in &mut algorithm.statements {
            rewrite_function_overrides_in_statement_with_ctx(stmt, &root_ctx);
        }
    }
    for algorithm in &mut flat.initial_algorithms {
        for stmt in &mut algorithm.statements {
            rewrite_function_overrides_in_statement_with_ctx(stmt, &root_ctx);
        }
    }
    for clause in &mut flat.when_clauses {
        rewrite_function_overrides_in_when_clause_with_ctx(clause, &root_ctx);
    }
    rewrite_function_overrides_in_flat_functions(
        flat,
        tree,
        class_index,
        &override_packages,
        &override_functions,
    )?;
    Ok(())
}

fn rewrite_function_overrides_in_flat_variables(
    flat: &mut Model,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    component_override_map: &ComponentOverrideMap,
    component_members: &component_member_scope::ComponentMemberScopes,
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
            component_members,
        );
    }
}

fn rewrite_function_overrides_in_equations(
    flat: &mut Model,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    component_override_map: &ComponentOverrideMap,
    component_members: &component_member_scope::ComponentMemberScopes,
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
        let ctx = FunctionOverrideRewriteContext::new(
            tree,
            class_index,
            override_packages,
            override_functions,
        )
        .with_active_scope(cache_key)
        .with_component_member_scope(component_members);
        rewrite_function_overrides_in_expression_with_ctx(&mut equation.residual, &ctx);
    }
}

pub(crate) fn rewrite_function_extends_aliases_in_flat_functions(
    flat: &mut Model,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> Result<(), FlattenError> {
    for function in flat.functions.values_mut() {
        rewrite_function_extends_aliases_in_function(function, tree, class_index)?;
    }
    Ok(())
}

pub(super) fn rewrite_function_overrides_in_flat_functions(
    flat: &mut Model,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    active_override_packages: &[OverrideTarget],
    active_override_functions: &OverrideFunctionMap,
) -> Result<(), FlattenError> {
    for function in flat.functions.values_mut() {
        let mut override_packages = active_override_packages.to_vec();
        override_packages.extend(function_package_override_chain(
            function.name.as_str(),
            tree,
            class_index,
        )?);
        rewrite_function_overrides_in_function(
            function,
            tree,
            class_index,
            &override_packages,
            active_override_functions,
        )?;
    }
    Ok(())
}

pub(crate) fn rewrite_function_extends_aliases_in_function(
    function: &mut rumoca_core::Function,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> Result<(), FlattenError> {
    let override_functions = OverrideFunctionMap::default();
    let override_packages =
        function_package_override_chain(function.name.as_str(), tree, class_index)?;
    rewrite_function_overrides_in_function(
        function,
        tree,
        class_index,
        &override_packages,
        &override_functions,
    )
}

pub(super) fn rewrite_function_overrides_in_function(
    function: &mut rumoca_core::Function,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    override_packages: &[OverrideTarget],
    override_functions: &OverrideFunctionMap,
) -> Result<(), FlattenError> {
    let lexical_package_def_id =
        function_lexical_package_def_id(function.name.as_str(), tree, class_index)?;
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
    rewrite_function_self_package_calls(function, tree, class_index)?;
    Ok(())
}

pub(super) fn rewrite_function_self_package_calls(
    function: &mut rumoca_core::Function,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> Result<(), FlattenError> {
    let Some(package) = function_package_override_chain(function.name.as_str(), tree, class_index)?
        .into_iter()
        .next()
    else {
        return Ok(());
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
    Ok(())
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
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> Result<Option<rumoca_core::DefId>, FlattenError> {
    let Some(package_name) = enclosing_scope(function_name) else {
        return Ok(None);
    };
    let Some(class_def) = class_index.get_by_qualified_name(package_name) else {
        return Ok(None);
    };
    class_def.def_id.map(Some).ok_or_else(|| {
        missing_resolved_class_metadata_for_class(
            package_name,
            "function lexical package lookup",
            tree,
            class_def,
        )
    })
}

pub(super) fn function_package_override_chain(
    function_name: &str,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> Result<Vec<OverrideTarget>, FlattenError> {
    let Some(package_name) = enclosing_scope(function_name) else {
        return Ok(Vec::new());
    };
    let Some(class_def) = class_index.get_by_qualified_name(package_name) else {
        return Ok(Vec::new());
    };
    let def_id = class_def.def_id.ok_or_else(|| {
        missing_resolved_class_metadata_for_class(
            package_name,
            "function package override lookup",
            tree,
            class_def,
        )
    })?;
    let name = class_index.qualified_name(def_id).ok_or_else(|| {
        missing_resolved_class_metadata_for_class(
            package_name,
            "function package override lookup",
            tree,
            class_def,
        )
    })?;
    Ok(vec![OverrideTarget {
        alias: leaf_segment(package_name).to_string(),
        name: name.to_string(),
        def_id,
        class_type: class_def.class_type.clone(),
        active: false,
        modifier_args: Vec::new(),
    }])
}

fn missing_resolved_class_metadata_for_class(
    name: &str,
    context: &str,
    tree: &ClassTree,
    class_def: &rumoca_ir_ast::ClassDef,
) -> FlattenError {
    match required_location_span(&tree.source_map, &class_def.location, context) {
        Ok(span) => FlattenError::missing_resolved_class_metadata(name.to_string(), context, span),
        Err(error) => error,
    }
}
