use super::*;

pub(super) fn mark_record_constructor_calls(flat: &mut flat::Model, tree: &ast::ClassTree) {
    let constructor_names: HashSet<String> = tree
        .def_map
        .iter()
        .filter_map(|(def_id, qualified_name)| {
            tree.get_class_by_def_id(*def_id)
                .filter(|class_def| class_def.class_type == rumoca_ir_ast::ClassType::Record)
                .map(|_| qualified_name.clone())
        })
        .collect();
    if constructor_names.is_empty() {
        return;
    }

    for var in flat.variables.values_mut() {
        mark_constructor_opt_expr(&mut var.binding, &constructor_names);
        mark_constructor_opt_expr(&mut var.start, &constructor_names);
        mark_constructor_opt_expr(&mut var.min, &constructor_names);
        mark_constructor_opt_expr(&mut var.max, &constructor_names);
        mark_constructor_opt_expr(&mut var.nominal, &constructor_names);
    }
    for eq in &mut flat.equations {
        mark_constructor_expr(&mut eq.residual, &constructor_names);
    }
    for eq in &mut flat.initial_equations {
        mark_constructor_expr(&mut eq.residual, &constructor_names);
    }
    for assert_eq in &mut flat.assert_equations {
        mark_constructor_expr(&mut assert_eq.condition, &constructor_names);
        mark_constructor_expr(&mut assert_eq.message, &constructor_names);
        mark_constructor_opt_expr(&mut assert_eq.level, &constructor_names);
    }
    for assert_eq in &mut flat.initial_assert_equations {
        mark_constructor_expr(&mut assert_eq.condition, &constructor_names);
        mark_constructor_expr(&mut assert_eq.message, &constructor_names);
        mark_constructor_opt_expr(&mut assert_eq.level, &constructor_names);
    }
    for algorithm in &mut flat.algorithms {
        mark_constructor_statements(&mut algorithm.statements, &constructor_names);
    }
    for algorithm in &mut flat.initial_algorithms {
        mark_constructor_statements(&mut algorithm.statements, &constructor_names);
    }
    for when_clause in &mut flat.when_clauses {
        mark_constructor_expr(&mut when_clause.condition, &constructor_names);
        mark_constructor_when_equations(&mut when_clause.equations, &constructor_names);
    }
    for function in flat.functions.values_mut() {
        for input in &mut function.inputs {
            mark_constructor_opt_expr(&mut input.default, &constructor_names);
        }
        for output in &mut function.outputs {
            mark_constructor_opt_expr(&mut output.default, &constructor_names);
        }
        for local in &mut function.locals {
            mark_constructor_opt_expr(&mut local.default, &constructor_names);
        }
        mark_constructor_statements(&mut function.body, &constructor_names);
    }
}

fn mark_constructor_opt_expr(
    expr: &mut Option<flat::Expression>,
    constructor_names: &HashSet<String>,
) {
    if let Some(expr) = expr {
        mark_constructor_expr(expr, constructor_names);
    }
}

fn mark_constructor_expr(expr: &mut flat::Expression, constructor_names: &HashSet<String>) {
    match expr {
        flat::Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => {
            if constructor_names.contains(name.as_str()) {
                *is_constructor = true;
            }
            for arg in args {
                mark_constructor_expr(arg, constructor_names);
            }
        }
        flat::Expression::BuiltinCall { args, .. } => {
            for arg in args {
                mark_constructor_expr(arg, constructor_names);
            }
        }
        flat::Expression::Binary { lhs, rhs, .. } => {
            mark_constructor_expr(lhs, constructor_names);
            mark_constructor_expr(rhs, constructor_names);
        }
        flat::Expression::Unary { rhs, .. } => mark_constructor_expr(rhs, constructor_names),
        flat::Expression::If {
            branches,
            else_branch,
        } => {
            for (condition, value) in branches {
                mark_constructor_expr(condition, constructor_names);
                mark_constructor_expr(value, constructor_names);
            }
            mark_constructor_expr(else_branch, constructor_names);
        }
        flat::Expression::Array { elements, .. } | flat::Expression::Tuple { elements } => {
            for element in elements {
                mark_constructor_expr(element, constructor_names);
            }
        }
        flat::Expression::Range { start, step, end } => {
            mark_constructor_expr(start, constructor_names);
            if let Some(step_expr) = step {
                mark_constructor_expr(step_expr, constructor_names);
            }
            mark_constructor_expr(end, constructor_names);
        }
        flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            mark_constructor_expr(expr, constructor_names);
            for index in indices {
                mark_constructor_expr(&mut index.range, constructor_names);
            }
            if let Some(filter_expr) = filter {
                mark_constructor_expr(filter_expr, constructor_names);
            }
        }
        flat::Expression::Index { base, subscripts } => {
            mark_constructor_expr(base, constructor_names);
            for subscript in subscripts {
                if let flat::Subscript::Expr(inner_expr) = subscript {
                    mark_constructor_expr(inner_expr, constructor_names);
                }
            }
        }
        flat::Expression::FieldAccess { base, .. } => {
            mark_constructor_expr(base, constructor_names)
        }
        flat::Expression::Literal(_)
        | flat::Expression::VarRef { .. }
        | flat::Expression::Empty => {}
    }
}

fn mark_constructor_statements(
    statements: &mut [rumoca_ir_flat::Statement],
    constructor_names: &HashSet<String>,
) {
    for statement in statements {
        match statement {
            rumoca_ir_flat::Statement::Assignment { value, .. } => {
                mark_constructor_expr(value, constructor_names);
            }
            rumoca_ir_flat::Statement::FunctionCall { args, outputs, .. } => {
                for arg in args {
                    mark_constructor_expr(arg, constructor_names);
                }
                for output in outputs {
                    mark_constructor_expr(output, constructor_names);
                }
            }
            rumoca_ir_flat::Statement::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    mark_constructor_expr(&mut block.cond, constructor_names);
                    mark_constructor_statements(&mut block.stmts, constructor_names);
                }
                if let Some(else_stmts) = else_block {
                    mark_constructor_statements(else_stmts, constructor_names);
                }
            }
            rumoca_ir_flat::Statement::For { indices, equations } => {
                for index in indices {
                    mark_constructor_expr(&mut index.range, constructor_names);
                }
                mark_constructor_statements(equations, constructor_names);
            }
            rumoca_ir_flat::Statement::While(block) => {
                mark_constructor_expr(&mut block.cond, constructor_names);
                mark_constructor_statements(&mut block.stmts, constructor_names);
            }
            rumoca_ir_flat::Statement::When(blocks) => {
                for block in blocks {
                    mark_constructor_expr(&mut block.cond, constructor_names);
                    mark_constructor_statements(&mut block.stmts, constructor_names);
                }
            }
            rumoca_ir_flat::Statement::Reinit { value, .. } => {
                mark_constructor_expr(value, constructor_names);
            }
            rumoca_ir_flat::Statement::Assert {
                condition,
                message,
                level,
            } => {
                mark_constructor_expr(condition, constructor_names);
                mark_constructor_expr(message, constructor_names);
                if let Some(level_expr) = level {
                    mark_constructor_expr(level_expr, constructor_names);
                }
            }
            rumoca_ir_flat::Statement::Return
            | rumoca_ir_flat::Statement::Break
            | rumoca_ir_flat::Statement::Empty => {}
        }
    }
}

fn mark_constructor_when_equations(
    equations: &mut [rumoca_ir_flat::WhenEquation],
    constructor_names: &HashSet<String>,
) {
    for equation in equations {
        match equation {
            rumoca_ir_flat::WhenEquation::Assign { value, .. }
            | rumoca_ir_flat::WhenEquation::Reinit { value, .. } => {
                mark_constructor_expr(value, constructor_names);
            }
            rumoca_ir_flat::WhenEquation::Assert { condition, .. } => {
                mark_constructor_expr(condition, constructor_names);
            }
            rumoca_ir_flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, branch_equations) in branches {
                    mark_constructor_expr(condition, constructor_names);
                    mark_constructor_when_equations(branch_equations, constructor_names);
                }
                mark_constructor_when_equations(else_branch, constructor_names);
            }
            rumoca_ir_flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                mark_constructor_expr(function, constructor_names);
            }
            rumoca_ir_flat::WhenEquation::Terminate { .. } => {}
        }
    }
}

pub(super) fn collapse_index_refs_to_known_varrefs(flat: &mut flat::Model) {
    let known_flat_vars: HashSet<String> = flat
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();

    for eq in &mut flat.equations {
        collapse_index_expr(&mut eq.residual, &known_flat_vars);
    }
    for eq in &mut flat.initial_equations {
        collapse_index_expr(&mut eq.residual, &known_flat_vars);
    }
    for assert_eq in &mut flat.assert_equations {
        collapse_index_expr(&mut assert_eq.condition, &known_flat_vars);
        collapse_index_expr(&mut assert_eq.message, &known_flat_vars);
        if let Some(level) = &mut assert_eq.level {
            collapse_index_expr(level, &known_flat_vars);
        }
    }
    for assert_eq in &mut flat.initial_assert_equations {
        collapse_index_expr(&mut assert_eq.condition, &known_flat_vars);
        collapse_index_expr(&mut assert_eq.message, &known_flat_vars);
        if let Some(level) = &mut assert_eq.level {
            collapse_index_expr(level, &known_flat_vars);
        }
    }

    for var in flat.variables.values_mut() {
        if let Some(binding) = &mut var.binding {
            collapse_index_expr(binding, &known_flat_vars);
        }
        if let Some(start) = &mut var.start {
            collapse_index_expr(start, &known_flat_vars);
        }
        if let Some(min) = &mut var.min {
            collapse_index_expr(min, &known_flat_vars);
        }
        if let Some(max) = &mut var.max {
            collapse_index_expr(max, &known_flat_vars);
        }
        if let Some(nominal) = &mut var.nominal {
            collapse_index_expr(nominal, &known_flat_vars);
        }
    }

    for when_clause in &mut flat.when_clauses {
        collapse_index_expr(&mut when_clause.condition, &known_flat_vars);
        collapse_index_when_equations(&mut when_clause.equations, &known_flat_vars);
    }

    for algorithm in &mut flat.algorithms {
        collapse_index_statements(&mut algorithm.statements, &known_flat_vars);
    }
    for algorithm in &mut flat.initial_algorithms {
        collapse_index_statements(&mut algorithm.statements, &known_flat_vars);
    }

    for function in flat.functions.values_mut() {
        for input in &mut function.inputs {
            if let Some(default) = &mut input.default {
                collapse_index_expr(default, &known_flat_vars);
            }
        }
        for output in &mut function.outputs {
            if let Some(default) = &mut output.default {
                collapse_index_expr(default, &known_flat_vars);
            }
        }
        for local in &mut function.locals {
            if let Some(default) = &mut local.default {
                collapse_index_expr(default, &known_flat_vars);
            }
        }
        collapse_index_statements(&mut function.body, &known_flat_vars);
    }
}

fn collapse_index_when_equations(
    equations: &mut [rumoca_ir_flat::WhenEquation],
    known_flat_vars: &HashSet<String>,
) {
    for equation in equations {
        match equation {
            rumoca_ir_flat::WhenEquation::Assign { value, .. }
            | rumoca_ir_flat::WhenEquation::Reinit { value, .. } => {
                collapse_index_expr(value, known_flat_vars);
            }
            rumoca_ir_flat::WhenEquation::Assert { condition, .. } => {
                collapse_index_expr(condition, known_flat_vars);
            }
            rumoca_ir_flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (cond, branch_equations) in branches {
                    collapse_index_expr(cond, known_flat_vars);
                    collapse_index_when_equations(branch_equations, known_flat_vars);
                }
                collapse_index_when_equations(else_branch, known_flat_vars);
            }
            rumoca_ir_flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                collapse_index_expr(function, known_flat_vars);
            }
            rumoca_ir_flat::WhenEquation::Terminate { .. } => {}
        }
    }
}

fn collapse_index_statements(
    statements: &mut [rumoca_ir_flat::Statement],
    known_flat_vars: &HashSet<String>,
) {
    for statement in statements {
        match statement {
            rumoca_ir_flat::Statement::Assignment { value, .. } => {
                collapse_index_expr(value, known_flat_vars);
            }
            rumoca_ir_flat::Statement::For { indices, equations } => {
                for index in indices {
                    collapse_index_expr(&mut index.range, known_flat_vars);
                }
                collapse_index_statements(equations, known_flat_vars);
            }
            rumoca_ir_flat::Statement::While(block) => {
                collapse_index_expr(&mut block.cond, known_flat_vars);
                collapse_index_statements(&mut block.stmts, known_flat_vars);
            }
            rumoca_ir_flat::Statement::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    collapse_index_expr(&mut block.cond, known_flat_vars);
                    collapse_index_statements(&mut block.stmts, known_flat_vars);
                }
                if let Some(else_block) = else_block {
                    collapse_index_statements(else_block, known_flat_vars);
                }
            }
            rumoca_ir_flat::Statement::When(blocks) => {
                for block in blocks {
                    collapse_index_expr(&mut block.cond, known_flat_vars);
                    collapse_index_statements(&mut block.stmts, known_flat_vars);
                }
            }
            rumoca_ir_flat::Statement::FunctionCall { args, outputs, .. } => {
                for arg in args {
                    collapse_index_expr(arg, known_flat_vars);
                }
                for output in outputs {
                    collapse_index_expr(output, known_flat_vars);
                }
            }
            rumoca_ir_flat::Statement::Reinit { value, .. } => {
                collapse_index_expr(value, known_flat_vars);
            }
            rumoca_ir_flat::Statement::Assert {
                condition,
                message,
                level,
            } => {
                collapse_index_expr(condition, known_flat_vars);
                collapse_index_expr(message, known_flat_vars);
                if let Some(level) = level {
                    collapse_index_expr(level, known_flat_vars);
                }
            }
            rumoca_ir_flat::Statement::Return
            | rumoca_ir_flat::Statement::Break
            | rumoca_ir_flat::Statement::Empty => {}
        }
    }
}

fn collapse_index_subscripts(
    subscripts: &mut [flat::Subscript],
    known_flat_vars: &HashSet<String>,
) {
    for subscript in subscripts {
        if let flat::Subscript::Expr(expr) = subscript {
            collapse_index_expr(expr, known_flat_vars);
        }
    }
}

fn collapse_index_expr(expr: &mut flat::Expression, known_flat_vars: &HashSet<String>) {
    match expr {
        flat::Expression::Binary { lhs, rhs, .. } => {
            collapse_index_expr(lhs, known_flat_vars);
            collapse_index_expr(rhs, known_flat_vars);
        }
        flat::Expression::Unary { rhs, .. } => collapse_index_expr(rhs, known_flat_vars),
        flat::Expression::BuiltinCall { args, .. }
        | flat::Expression::FunctionCall { args, .. } => {
            for arg in args {
                collapse_index_expr(arg, known_flat_vars);
            }
        }
        flat::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, branch) in branches {
                collapse_index_expr(cond, known_flat_vars);
                collapse_index_expr(branch, known_flat_vars);
            }
            collapse_index_expr(else_branch, known_flat_vars);
        }
        flat::Expression::Array { elements, .. } | flat::Expression::Tuple { elements } => {
            for element in elements {
                collapse_index_expr(element, known_flat_vars);
            }
        }
        flat::Expression::Range { start, step, end } => {
            collapse_index_expr(start, known_flat_vars);
            if let Some(step_expr) = step {
                collapse_index_expr(step_expr, known_flat_vars);
            }
            collapse_index_expr(end, known_flat_vars);
        }
        flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            collapse_index_expr(expr, known_flat_vars);
            for index in indices {
                collapse_index_expr(&mut index.range, known_flat_vars);
            }
            if let Some(filter_expr) = filter {
                collapse_index_expr(filter_expr, known_flat_vars);
            }
        }
        flat::Expression::Index { base, subscripts } => {
            collapse_index_expr(base, known_flat_vars);
            collapse_index_subscripts(subscripts, known_flat_vars);

            let folded = match base.as_ref() {
                flat::Expression::VarRef {
                    name,
                    subscripts: base_subscripts,
                } if known_flat_vars.contains(name.as_str()) => {
                    let mut merged = base_subscripts.clone();
                    merged.extend(subscripts.clone());
                    Some(flat::Expression::VarRef {
                        name: name.clone(),
                        subscripts: merged,
                    })
                }
                _ => None,
            };

            if let Some(folded) = folded {
                *expr = folded;
            }
        }
        flat::Expression::FieldAccess { base, .. } => {
            collapse_index_expr(base, known_flat_vars);
        }
        flat::Expression::VarRef { subscripts, .. } => {
            collapse_index_subscripts(subscripts, known_flat_vars);
        }
        flat::Expression::Literal(_) | flat::Expression::Empty => {}
    }
}

pub(super) fn substitute_known_constants_in_flat(flat: &mut flat::Model, ctx: &Context) {
    let live_vars: rustc_hash::FxHashSet<String> = flat
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();
    let no_locals: HashSet<String> = HashSet::new();

    for eq in &mut flat.equations {
        eq.residual =
            substitute_known_constants_expr(eq.residual.clone(), ctx, &live_vars, &no_locals);
    }
    for eq in &mut flat.initial_equations {
        eq.residual =
            substitute_known_constants_expr(eq.residual.clone(), ctx, &live_vars, &no_locals);
    }
    substitute_assert_equations(&mut flat.assert_equations, ctx, &live_vars, &no_locals);
    substitute_assert_equations(
        &mut flat.initial_assert_equations,
        ctx,
        &live_vars,
        &no_locals,
    );
    for when_clause in &mut flat.when_clauses {
        when_clause.condition = substitute_known_constants_expr(
            when_clause.condition.clone(),
            ctx,
            &live_vars,
            &no_locals,
        );
        for equation in &mut when_clause.equations {
            substitute_known_constants_when_equation(equation, ctx, &live_vars, &no_locals);
        }
    }
    substitute_algorithms(&mut flat.algorithms, ctx, &live_vars, &no_locals);
    substitute_algorithms(&mut flat.initial_algorithms, ctx, &live_vars, &no_locals);
    substitute_variable_annotations(&mut flat.variables, ctx, &live_vars, &no_locals);
    substitute_function_bodies(&mut flat.functions, ctx, &live_vars);
}

fn substitute_assert_equations(
    equations: &mut [flat::AssertEquation],
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) {
    for assert_eq in equations {
        assert_eq.condition =
            substitute_known_constants_expr(assert_eq.condition.clone(), ctx, live_vars, locals);
        assert_eq.message =
            substitute_known_constants_expr(assert_eq.message.clone(), ctx, live_vars, locals);
        substitute_opt_expr(&mut assert_eq.level, ctx, live_vars, locals);
    }
}

fn substitute_algorithms(
    algorithms: &mut [flat::Algorithm],
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) {
    for algorithm in algorithms {
        for statement in &mut algorithm.statements {
            substitute_known_constants_statement(statement, ctx, live_vars, locals);
        }
    }
}

fn substitute_variable_annotations(
    variables: &mut indexmap::IndexMap<flat::VarName, flat::Variable>,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) {
    for var in variables.values_mut() {
        substitute_opt_expr(&mut var.binding, ctx, live_vars, locals);
        substitute_opt_expr(&mut var.start, ctx, live_vars, locals);
        substitute_opt_expr(&mut var.min, ctx, live_vars, locals);
        substitute_opt_expr(&mut var.max, ctx, live_vars, locals);
        substitute_opt_expr(&mut var.nominal, ctx, live_vars, locals);
    }
}

fn substitute_function_bodies(
    functions: &mut indexmap::IndexMap<flat::VarName, flat::Function>,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
) {
    for function in functions.values_mut() {
        let function_locals: HashSet<String> = function
            .inputs
            .iter()
            .chain(function.outputs.iter())
            .chain(function.locals.iter())
            .map(|param| param.name.clone())
            .collect();

        for param in function
            .inputs
            .iter_mut()
            .chain(function.outputs.iter_mut())
            .chain(function.locals.iter_mut())
        {
            substitute_opt_expr(&mut param.default, ctx, live_vars, &function_locals);
        }
        for statement in &mut function.body {
            substitute_known_constants_statement(statement, ctx, live_vars, &function_locals);
        }
    }
}

fn substitute_opt_expr(
    expr: &mut Option<flat::Expression>,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) {
    if let Some(expr) = expr {
        *expr = substitute_known_constants_expr(expr.clone(), ctx, live_vars, locals);
    }
}

fn substitute_known_constants_expr(
    expr: flat::Expression,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> flat::Expression {
    match expr {
        flat::Expression::VarRef { name, subscripts } => {
            substitute_known_constants_var_ref(name, subscripts, ctx, live_vars, locals)
        }
        other => substitute_known_constants_non_var_expr(other, ctx, live_vars, locals),
    }
}

fn substitute_known_constants_var_ref(
    name: flat::VarName,
    subscripts: Vec<flat::Subscript>,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> flat::Expression {
    if subscripts.is_empty() {
        if let Some(replaced) = substitute_scalar_var_ref(&name, ctx, live_vars, locals) {
            return replaced;
        }
        return flat::Expression::VarRef { name, subscripts };
    }

    let rewritten_subscripts =
        substitute_known_constants_subscripts(subscripts, ctx, live_vars, locals);
    if locals.contains(name.as_str()) {
        return flat::Expression::VarRef {
            name,
            subscripts: rewritten_subscripts,
        };
    }
    if let Some(replaced) =
        substitute_indexed_constant_var_ref(&name, rewritten_subscripts.clone(), ctx, live_vars)
    {
        return replaced;
    }

    flat::Expression::VarRef {
        name,
        subscripts: rewritten_subscripts,
    }
}

fn substitute_indexed_constant_var_ref(
    name: &flat::VarName,
    subscripts: Vec<flat::Subscript>,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
) -> Option<flat::Expression> {
    if live_vars.contains(name.as_str()) {
        return None;
    }

    let constant_expr = ctx.constant_values.get(name.as_str())?.clone();
    Some(flat::Expression::Index {
        base: Box::new(constant_expr),
        subscripts,
    })
}

fn substitute_scalar_var_ref(
    name: &flat::VarName,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> Option<flat::Expression> {
    let key = name.as_str();
    if live_vars.contains(key) || locals.contains(key) {
        return None;
    }
    if inline_index_base_is_live_or_local(key, live_vars, locals) {
        return None;
    }
    if let Some(v) = resolve_constant_value_expr(key, ctx) {
        return Some(v.clone());
    }
    if let Some(v) = ctx.real_parameter_values.get(key)
        && v.is_finite()
    {
        return Some(flat::Expression::Literal(flat::Literal::Real(*v)));
    }
    if let Some(v) = ctx.parameter_values.get(key) {
        return Some(flat::Expression::Literal(flat::Literal::Integer(*v)));
    }
    if let Some(v) = ctx.boolean_parameter_values.get(key) {
        return Some(flat::Expression::Literal(flat::Literal::Boolean(*v)));
    }
    if let Some(v) = ctx.enum_parameter_values.get(key) {
        return Some(flat::Expression::VarRef {
            name: flat::VarName::new(v.clone()),
            subscripts: vec![],
        });
    }
    if let Some(expr) = resolve_inline_indexed_constant(key, ctx) {
        return Some(expr);
    }

    if let Some(resolved_key) = resolve_varref_through_constant_aliases(key, ctx)
        && resolved_key != key
    {
        if let Some(v) = resolve_constant_value_expr(&resolved_key, ctx) {
            return Some(v.clone());
        }
        if let Some(v) = ctx.real_parameter_values.get(&resolved_key)
            && v.is_finite()
        {
            return Some(flat::Expression::Literal(flat::Literal::Real(*v)));
        }
        if let Some(v) = ctx.parameter_values.get(&resolved_key) {
            return Some(flat::Expression::Literal(flat::Literal::Integer(*v)));
        }
        if let Some(v) = ctx.boolean_parameter_values.get(&resolved_key) {
            return Some(flat::Expression::Literal(flat::Literal::Boolean(*v)));
        }
        if let Some(v) = ctx.enum_parameter_values.get(&resolved_key) {
            return Some(flat::Expression::VarRef {
                name: flat::VarName::new(v.clone()),
                subscripts: vec![],
            });
        }
        if let Some(expr) = resolve_inline_indexed_constant(&resolved_key, ctx) {
            return Some(expr);
        }
        return Some(flat::Expression::VarRef {
            name: flat::VarName::new(resolved_key),
            subscripts: vec![],
        });
    }

    None
}

fn inline_index_base_is_live_or_local(
    name: &str,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> bool {
    let Some((base, _indices)) = split_inline_indexed_name(name) else {
        return false;
    };
    live_vars.contains(base) || locals.contains(base)
}

fn resolve_constant_value_expr<'a>(name: &str, ctx: &'a Context) -> Option<&'a flat::Expression> {
    let mut current = name.to_string();
    let mut visited = rustc_hash::FxHashSet::default();
    loop {
        if !visited.insert(current.clone()) {
            return None;
        }
        let expr = ctx.constant_values.get(&current)?;
        let flat::Expression::VarRef {
            name: alias_name,
            subscripts,
        } = expr
        else {
            return Some(expr);
        };
        if !subscripts.is_empty() || alias_name.as_str() == current {
            return Some(expr);
        }
        current = alias_name.as_str().to_string();
    }
}

fn resolve_varref_through_constant_aliases(name: &str, ctx: &Context) -> Option<String> {
    let mut current = name.to_string();
    let mut visited = rustc_hash::FxHashSet::default();
    loop {
        if !visited.insert(current.clone()) {
            return None;
        }

        let mut replaced = false;
        for (idx, ch) in current.char_indices().rev() {
            if ch != '.' {
                continue;
            }
            let prefix = &current[..idx];
            let suffix = &current[idx..];
            let Some(alias_expr) = ctx.constant_values.get(prefix) else {
                continue;
            };
            let flat::Expression::VarRef {
                name: alias_name,
                subscripts,
            } = alias_expr
            else {
                continue;
            };
            if !subscripts.is_empty() {
                continue;
            }
            current = format!("{}{}", alias_name.as_str(), suffix);
            replaced = true;
            break;
        }

        if !replaced {
            return if current == name { None } else { Some(current) };
        }
    }
}

fn resolve_inline_indexed_constant(name: &str, ctx: &Context) -> Option<flat::Expression> {
    let (base, indices) = split_inline_indexed_name(name)?;
    let index_text = indices.trim();
    let indices: Vec<i64> = index_text
        .split(',')
        .map(str::trim)
        .filter(|part| !part.is_empty())
        .map(|part| part.parse::<i64>())
        .collect::<Result<Vec<_>, _>>()
        .ok()?;
    if indices.is_empty() {
        return None;
    }

    let base_expr = resolve_constant_value_expr(base, ctx)?.clone();
    Some(flat::Expression::Index {
        base: Box::new(base_expr),
        subscripts: indices
            .into_iter()
            .map(|index| {
                flat::Subscript::Expr(Box::new(flat::Expression::Literal(flat::Literal::Integer(
                    index,
                ))))
            })
            .collect(),
    })
}

fn split_inline_indexed_name(name: &str) -> Option<(&str, &str)> {
    let start = name.rfind('[')?;
    let end = name.rfind(']')?;
    if end <= start + 1 || end != name.len() - 1 {
        return None;
    }
    Some((&name[..start], &name[start + 1..end]))
}

fn substitute_known_constants_subscripts(
    subscripts: Vec<flat::Subscript>,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> Vec<flat::Subscript> {
    subscripts
        .into_iter()
        .map(|sub| match sub {
            flat::Subscript::Expr(inner) => flat::Subscript::Expr(Box::new(
                substitute_known_constants_expr(*inner, ctx, live_vars, locals),
            )),
            other => other,
        })
        .collect()
}

fn substitute_known_constants_non_var_expr(
    expr: flat::Expression,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> flat::Expression {
    if let flat::Expression::FieldAccess { base, field } = expr {
        return rewrite_field_access_expr(*base, field, ctx, live_vars, locals);
    }

    substitute_known_constants_non_field_expr(expr, ctx, live_vars, locals)
}

fn substitute_known_constants_non_field_expr(
    expr: flat::Expression,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> flat::Expression {
    let rewrite = |expr| substitute_known_constants_expr(expr, ctx, live_vars, locals);
    match expr {
        flat::Expression::Binary { op, lhs, rhs } => flat::Expression::Binary {
            op,
            lhs: Box::new(rewrite(*lhs)),
            rhs: Box::new(rewrite(*rhs)),
        },
        flat::Expression::Unary { op, rhs } => flat::Expression::Unary {
            op,
            rhs: Box::new(rewrite(*rhs)),
        },
        flat::Expression::BuiltinCall { function, args } => flat::Expression::BuiltinCall {
            function,
            args: args.into_iter().map(rewrite).collect(),
        },
        flat::Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => flat::Expression::FunctionCall {
            name,
            args: args.into_iter().map(rewrite).collect(),
            is_constructor,
        },
        flat::Expression::If {
            branches,
            else_branch,
        } => flat::Expression::If {
            branches: branches
                .into_iter()
                .map(|(cond, value)| (rewrite(cond), rewrite(value)))
                .collect(),
            else_branch: Box::new(rewrite(*else_branch)),
        },
        flat::Expression::Array {
            elements,
            is_matrix,
        } => flat::Expression::Array {
            elements: elements.into_iter().map(rewrite).collect(),
            is_matrix,
        },
        flat::Expression::Tuple { elements } => flat::Expression::Tuple {
            elements: elements.into_iter().map(rewrite).collect(),
        },
        flat::Expression::Range { start, step, end } => flat::Expression::Range {
            start: Box::new(rewrite(*start)),
            step: step.map(|s| Box::new(rewrite(*s))),
            end: Box::new(rewrite(*end)),
        },
        flat::Expression::Index { base, subscripts } => flat::Expression::Index {
            base: Box::new(rewrite(*base)),
            subscripts: substitute_known_constants_subscripts(subscripts, ctx, live_vars, locals),
        },
        flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => flat::Expression::ArrayComprehension {
            expr: Box::new(rewrite(*expr)),
            indices: indices
                .into_iter()
                .map(|mut idx| {
                    idx.range = rewrite(idx.range);
                    idx
                })
                .collect(),
            filter: filter.map(|f| Box::new(rewrite(*f))),
        },
        other => other,
    }
}

fn rewrite_field_access_expr(
    base: flat::Expression,
    field: String,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> flat::Expression {
    let rewritten_base = substitute_known_constants_expr(base, ctx, live_vars, locals);
    if let flat::Expression::FunctionCall {
        name,
        args,
        is_constructor: true,
    } = &rewritten_base
        && args.is_empty()
        && let Some(resolved) = resolve_constant_field_access(name.as_str(), &field, ctx)
    {
        return resolved;
    }
    if let flat::Expression::VarRef { name, subscripts } = &rewritten_base
        && subscripts.is_empty()
        && !live_vars.contains(name.as_str())
        && let Some(resolved) = resolve_constant_field_access(name.as_str(), &field, ctx)
    {
        return resolved;
    }
    flat::Expression::FieldAccess {
        base: Box::new(rewritten_base),
        field,
    }
}

fn resolve_constant_field_access(
    base_name: &str,
    field: &str,
    ctx: &Context,
) -> Option<flat::Expression> {
    let mut current = base_name.to_string();
    let mut visited = rustc_hash::FxHashSet::default();
    loop {
        if !visited.insert(current.clone()) {
            return None;
        }
        let key = format!("{}.{}", current, field);
        if let Some(value) = ctx.constant_values.get(&key) {
            return Some(value.clone());
        }
        if let Some(value) = ctx.real_parameter_values.get(&key)
            && value.is_finite()
        {
            return Some(flat::Expression::Literal(flat::Literal::Real(*value)));
        }
        if let Some(value) = ctx.parameter_values.get(&key) {
            return Some(flat::Expression::Literal(flat::Literal::Integer(*value)));
        }
        if let Some(value) = ctx.boolean_parameter_values.get(&key) {
            return Some(flat::Expression::Literal(flat::Literal::Boolean(*value)));
        }
        if let Some(value) = ctx.enum_parameter_values.get(&key) {
            return Some(flat::Expression::VarRef {
                name: flat::VarName::new(value.clone()),
                subscripts: vec![],
            });
        }

        let alias_expr = ctx.constant_values.get(&current)?;
        let flat::Expression::VarRef { name, subscripts } = alias_expr else {
            return None;
        };
        if !subscripts.is_empty() {
            return None;
        }
        current = name.as_str().to_string();
    }
}

fn substitute_known_constants_statement(
    statement: &mut flat::Statement,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) {
    match statement {
        flat::Statement::Assignment { value, .. } => {
            *value = substitute_known_constants_expr(value.clone(), ctx, live_vars, locals);
        }
        flat::Statement::For { indices, equations } => {
            for index in indices {
                index.range =
                    substitute_known_constants_expr(index.range.clone(), ctx, live_vars, locals);
            }
            for nested in equations {
                substitute_known_constants_statement(nested, ctx, live_vars, locals);
            }
        }
        flat::Statement::While(block) => {
            block.cond =
                substitute_known_constants_expr(block.cond.clone(), ctx, live_vars, locals);
            for nested in &mut block.stmts {
                substitute_known_constants_statement(nested, ctx, live_vars, locals);
            }
        }
        flat::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                block.cond =
                    substitute_known_constants_expr(block.cond.clone(), ctx, live_vars, locals);
                for nested in &mut block.stmts {
                    substitute_known_constants_statement(nested, ctx, live_vars, locals);
                }
            }
            if let Some(else_stmts) = else_block {
                for nested in else_stmts {
                    substitute_known_constants_statement(nested, ctx, live_vars, locals);
                }
            }
        }
        flat::Statement::When(blocks) => {
            for block in blocks {
                block.cond =
                    substitute_known_constants_expr(block.cond.clone(), ctx, live_vars, locals);
                for nested in &mut block.stmts {
                    substitute_known_constants_statement(nested, ctx, live_vars, locals);
                }
            }
        }
        flat::Statement::FunctionCall { args, outputs, .. } => {
            for arg in args {
                *arg = substitute_known_constants_expr(arg.clone(), ctx, live_vars, locals);
            }
            for output in outputs {
                *output = substitute_known_constants_expr(output.clone(), ctx, live_vars, locals);
            }
        }
        flat::Statement::Reinit { value, .. } => {
            *value = substitute_known_constants_expr(value.clone(), ctx, live_vars, locals);
        }
        flat::Statement::Assert {
            condition,
            message,
            level,
        } => {
            *condition = substitute_known_constants_expr(condition.clone(), ctx, live_vars, locals);
            *message = substitute_known_constants_expr(message.clone(), ctx, live_vars, locals);
            if let Some(level_expr) = level {
                *level_expr =
                    substitute_known_constants_expr(level_expr.clone(), ctx, live_vars, locals);
            }
        }
        flat::Statement::Empty | flat::Statement::Return | flat::Statement::Break => {}
    }
}

fn substitute_known_constants_when_equation(
    equation: &mut flat::WhenEquation,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) {
    match equation {
        flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
            *value = substitute_known_constants_expr(value.clone(), ctx, live_vars, locals);
        }
        flat::WhenEquation::Assert { condition, .. } => {
            *condition = substitute_known_constants_expr(condition.clone(), ctx, live_vars, locals);
        }
        flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (condition, equations) in branches {
                *condition =
                    substitute_known_constants_expr(condition.clone(), ctx, live_vars, locals);
                for nested in equations {
                    substitute_known_constants_when_equation(nested, ctx, live_vars, locals);
                }
            }
            for nested in else_branch {
                substitute_known_constants_when_equation(nested, ctx, live_vars, locals);
            }
        }
        flat::WhenEquation::FunctionCallOutputs { function, .. } => {
            *function = substitute_known_constants_expr(function.clone(), ctx, live_vars, locals);
        }
        flat::WhenEquation::Terminate { .. } => {}
    }
}

/// Drop FieldAccess bindings whose targets don't exist in the flat model.
///
/// During modifier propagation, record bindings like `x = someRecord.field` may reference
/// internal component structure that was eliminated during flattening. These dangling
/// FieldAccess bindings would cause incorrect equation generation in todae if kept.
pub(super) fn drop_invalid_field_access_bindings(flat: &mut flat::Model) {
    use std::collections::HashSet;

    // Collect the set of all variable names for lookup
    let all_names: HashSet<flat::VarName> = flat.variables.keys().cloned().collect();

    // Find variables with FieldAccess bindings pointing to non-existent targets
    let to_clear: Vec<flat::VarName> = flat
        .variables
        .iter()
        .filter_map(|(name, var)| {
            let binding = var.binding.as_ref()?;
            let flat::Expression::FieldAccess { base, field } = binding else {
                return None;
            };
            let flat::Expression::VarRef {
                name: base_name, ..
            } = base.as_ref()
            else {
                return None;
            };
            let full_name = flat::VarName::new(format!("{}.{}", base_name.as_str(), field));
            if all_names.contains(&full_name) {
                None
            } else {
                Some(name.clone())
            }
        })
        .collect();

    for name in &to_clear {
        if let Some(var) = flat.variables.get_mut(name) {
            var.binding = None;
        }
    }
}

#[cfg(test)]
mod substitute_constant_tests {
    use super::*;
    use rumoca_core::Span;

    fn simple_assignment(value: flat::Expression) -> flat::Statement {
        flat::Statement::Assignment {
            comp: flat::ComponentReference {
                local: false,
                parts: vec![flat::ComponentRefPart {
                    ident: "y".to_string(),
                    subs: vec![],
                }],
                def_id: None,
            },
            value,
        }
    }

    #[test]
    fn substitutes_known_constants_inside_function_defaults_and_body() {
        let mut model = flat::Model::new();
        let mut function = flat::Function::new("Pkg.f", Span::DUMMY);
        function.add_input(flat::FunctionParam::new("u", "Real").with_default(
            flat::Expression::VarRef {
                name: flat::VarName::new("Pkg.Constants.k"),
                subscripts: vec![],
            },
        ));
        function
            .body
            .push(simple_assignment(flat::Expression::VarRef {
                name: flat::VarName::new("Pkg.Constants.k"),
                subscripts: vec![],
            }));
        model.add_function(function);

        let mut ctx = Context::new();
        ctx.constant_values.insert(
            "Pkg.Constants.k".to_string(),
            flat::Expression::Literal(flat::Literal::Real(42.0)),
        );

        substitute_known_constants_in_flat(&mut model, &ctx);

        let function = model
            .functions
            .get(&flat::VarName::new("Pkg.f"))
            .expect("function should exist");
        assert!(matches!(
            function.inputs[0].default,
            Some(flat::Expression::Literal(flat::Literal::Real(v))) if (v - 42.0).abs() < f64::EPSILON
        ));
        match &function.body[0] {
            flat::Statement::Assignment { value, .. } => assert!(matches!(
                value,
                flat::Expression::Literal(flat::Literal::Real(v)) if (*v - 42.0).abs() < f64::EPSILON
            )),
            other => panic!("expected assignment statement, got {other:?}"),
        }
    }

    #[test]
    fn does_not_substitute_function_local_names() {
        let mut model = flat::Model::new();
        let mut function = flat::Function::new("Pkg.g", Span::DUMMY);
        function.add_input(flat::FunctionParam::new("k", "Real"));
        function
            .body
            .push(simple_assignment(flat::Expression::VarRef {
                name: flat::VarName::new("k"),
                subscripts: vec![],
            }));
        model.add_function(function);

        let mut ctx = Context::new();
        ctx.constant_values.insert(
            "k".to_string(),
            flat::Expression::Literal(flat::Literal::Real(7.0)),
        );

        substitute_known_constants_in_flat(&mut model, &ctx);

        let function = model
            .functions
            .get(&flat::VarName::new("Pkg.g"))
            .expect("function should exist");
        match &function.body[0] {
            flat::Statement::Assignment { value, .. } => assert!(matches!(
                value,
                flat::Expression::VarRef { name, .. } if name.as_str() == "k"
            )),
            other => panic!("expected assignment statement, got {other:?}"),
        }
    }

    #[test]
    fn does_not_substitute_indexed_function_local_names() {
        let mut model = flat::Model::new();
        let mut function = flat::Function::new("Pkg.g_indexed", Span::DUMMY);
        function.add_input(flat::FunctionParam::new("table", "Real").with_dims(vec![7, 2]));
        function
            .body
            .push(simple_assignment(flat::Expression::VarRef {
                name: flat::VarName::new("table"),
                subscripts: vec![
                    flat::Subscript::Expr(Box::new(flat::Expression::VarRef {
                        name: flat::VarName::new("next"),
                        subscripts: vec![],
                    })),
                    flat::Subscript::Index(1),
                ],
            }));
        model.add_function(function);

        let mut ctx = Context::new();
        ctx.constant_values.insert(
            "table".to_string(),
            flat::Expression::BuiltinCall {
                function: flat::BuiltinFunction::Fill,
                args: vec![
                    flat::Expression::Literal(flat::Literal::Real(0.0)),
                    flat::Expression::Literal(flat::Literal::Integer(0)),
                    flat::Expression::Literal(flat::Literal::Integer(2)),
                ],
            },
        );

        substitute_known_constants_in_flat(&mut model, &ctx);

        let function = model
            .functions
            .get(&flat::VarName::new("Pkg.g_indexed"))
            .expect("function should exist");
        match &function.body[0] {
            flat::Statement::Assignment { value, .. } => match value {
                flat::Expression::VarRef { name, subscripts } => {
                    assert_eq!(name.as_str(), "table");
                    assert_eq!(subscripts.len(), 2);
                }
                other => panic!("expected table varref, got {other:?}"),
            },
            other => panic!("expected assignment statement, got {other:?}"),
        }
    }

    #[test]
    fn substitutes_inline_multi_indexed_constant_varref_names() {
        let mut model = flat::Model::new();
        let mut function = flat::Function::new("Pkg.h", Span::DUMMY);
        function
            .body
            .push(simple_assignment(flat::Expression::VarRef {
                name: flat::VarName::new("Modelica.Blocks.Sources.IntegerTable.table[1,1]"),
                subscripts: vec![],
            }));
        model.add_function(function);

        let mut ctx = Context::new();
        ctx.constant_values.insert(
            "Modelica.Blocks.Sources.IntegerTable.table".to_string(),
            flat::Expression::Array {
                elements: vec![
                    flat::Expression::Literal(flat::Literal::Integer(0)),
                    flat::Expression::Literal(flat::Literal::Integer(1)),
                ],
                is_matrix: false,
            },
        );

        substitute_known_constants_in_flat(&mut model, &ctx);

        let function = model
            .functions
            .get(&flat::VarName::new("Pkg.h"))
            .expect("function should exist");
        match &function.body[0] {
            flat::Statement::Assignment { value, .. } => match value {
                flat::Expression::Index { base, subscripts } => {
                    assert!(matches!(
                        base.as_ref(),
                        flat::Expression::Array { elements, is_matrix }
                            if !*is_matrix && elements.len() == 2
                    ));
                    assert_eq!(subscripts.len(), 2);
                    assert!(matches!(
                        &subscripts[0],
                        flat::Subscript::Expr(expr)
                            if matches!(expr.as_ref(), flat::Expression::Literal(flat::Literal::Integer(1)))
                    ));
                    assert!(matches!(
                        &subscripts[1],
                        flat::Subscript::Expr(expr)
                            if matches!(expr.as_ref(), flat::Expression::Literal(flat::Literal::Integer(1)))
                    ));
                }
                other => panic!("expected indexed expression, got {other:?}"),
            },
            other => panic!("expected assignment statement, got {other:?}"),
        }
    }

    #[test]
    fn does_not_substitute_inline_indexed_varref_when_base_is_local() {
        let mut model = flat::Model::new();
        let mut function = flat::Function::new("Pkg.inline_local", Span::DUMMY);
        function.add_input(flat::FunctionParam::new("table", "Real").with_dims(vec![7, 2]));
        function
            .body
            .push(simple_assignment(flat::Expression::VarRef {
                name: flat::VarName::new("table[1,1]"),
                subscripts: vec![],
            }));
        model.add_function(function);

        let mut ctx = Context::new();
        ctx.constant_values.insert(
            "table".to_string(),
            flat::Expression::BuiltinCall {
                function: flat::BuiltinFunction::Fill,
                args: vec![
                    flat::Expression::Literal(flat::Literal::Real(0.0)),
                    flat::Expression::Literal(flat::Literal::Integer(0)),
                    flat::Expression::Literal(flat::Literal::Integer(2)),
                ],
            },
        );

        substitute_known_constants_in_flat(&mut model, &ctx);

        let function = model
            .functions
            .get(&flat::VarName::new("Pkg.inline_local"))
            .expect("function should exist");
        match &function.body[0] {
            flat::Statement::Assignment { value, .. } => assert!(matches!(
                value,
                flat::Expression::VarRef { name, subscripts }
                    if name.as_str() == "table[1,1]" && subscripts.is_empty()
            )),
            other => panic!("expected assignment statement, got {other:?}"),
        }
    }

    #[test]
    fn substitutes_field_access_on_zero_arg_constructor_constants() {
        let mut model = flat::Model::new();
        let mut function = flat::Function::new("Pkg.k", Span::DUMMY);
        function
            .body
            .push(simple_assignment(flat::Expression::FieldAccess {
                base: Box::new(flat::Expression::FunctionCall {
                    name: flat::VarName::new(
                        "Modelica.Electrical.Batteries.ParameterRecords.ExampleData",
                    ),
                    args: vec![],
                    is_constructor: true,
                }),
                field: "useLinearSOCDependency".to_string(),
            }));
        model.add_function(function);

        let mut ctx = Context::new();
        ctx.boolean_parameter_values.insert(
            "Modelica.Electrical.Batteries.ParameterRecords.ExampleData.useLinearSOCDependency"
                .to_string(),
            false,
        );

        substitute_known_constants_in_flat(&mut model, &ctx);

        let function = model
            .functions
            .get(&flat::VarName::new("Pkg.k"))
            .expect("function should exist");
        match &function.body[0] {
            flat::Statement::Assignment { value, .. } => assert!(matches!(
                value,
                flat::Expression::Literal(flat::Literal::Boolean(false))
            )),
            other => panic!("expected assignment statement, got {other:?}"),
        }
    }
}
