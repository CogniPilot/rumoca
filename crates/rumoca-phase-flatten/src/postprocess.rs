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

/// Canonicalize VarRef names through record aliases when the resolved target
/// exists in flattened variables.
///
/// This keeps equation references aligned with flattened declaration paths
/// (MLS §7.2.3 alias semantics), instead of carrying intermediate component
/// implementation paths that do not exist as flat variables.
pub(super) fn canonicalize_varrefs_via_record_aliases(flat: &mut flat::Model, ctx: &Context) {
    let known_vars: std::collections::HashSet<flat::VarName> =
        flat.variables.keys().cloned().collect();
    if known_vars.is_empty() || ctx.record_aliases.is_empty() {
        return;
    }

    let rewrite_name = |name: &flat::VarName| -> flat::VarName {
        let resolved = ctx.resolve_alias(name.as_str());
        if resolved == name.as_str() {
            return name.clone();
        }
        let resolved_name = flat::VarName::new(resolved);
        if known_vars.contains(&resolved_name) {
            resolved_name
        } else {
            name.clone()
        }
    };

    rewrite_varrefs_in_opt_exprs(
        flat.variables.values_mut().flat_map(|v| {
            [
                &mut v.binding,
                &mut v.start,
                &mut v.min,
                &mut v.max,
                &mut v.nominal,
            ]
        }),
        &rewrite_name,
    );
    rewrite_varrefs_in_equations(&mut flat.equations, &rewrite_name);
    rewrite_varrefs_in_equations(&mut flat.initial_equations, &rewrite_name);
    rewrite_varrefs_in_asserts(&mut flat.assert_equations, &rewrite_name);
    rewrite_varrefs_in_asserts(&mut flat.initial_assert_equations, &rewrite_name);
    for algo in flat
        .algorithms
        .iter_mut()
        .chain(flat.initial_algorithms.iter_mut())
    {
        rewrite_varrefs_in_statements(&mut algo.statements, &rewrite_name);
    }
    for when in &mut flat.when_clauses {
        rewrite_varrefs_in_expr(&mut when.condition, &rewrite_name);
        rewrite_varrefs_in_when_equations(&mut when.equations, &rewrite_name);
    }
    for function in flat.functions.values_mut() {
        for param in function
            .inputs
            .iter_mut()
            .chain(function.outputs.iter_mut())
            .chain(function.locals.iter_mut())
        {
            if let Some(default) = &mut param.default {
                rewrite_varrefs_in_expr(default, &rewrite_name);
            }
        }
        rewrite_varrefs_in_statements(&mut function.body, &rewrite_name);
    }
}

fn rewrite_varrefs_in_opt_exprs<'a>(
    exprs: impl Iterator<Item = &'a mut Option<flat::Expression>>,
    rewrite_name: &impl Fn(&flat::VarName) -> flat::VarName,
) {
    for value in exprs.flatten() {
        rewrite_varrefs_in_expr(value, rewrite_name);
    }
}

fn rewrite_varrefs_in_equations(
    equations: &mut [flat::Equation],
    rewrite_name: &impl Fn(&flat::VarName) -> flat::VarName,
) {
    for equation in equations {
        rewrite_varrefs_in_expr(&mut equation.residual, rewrite_name);
    }
}

fn rewrite_varrefs_in_asserts(
    asserts: &mut [flat::AssertEquation],
    rewrite_name: &impl Fn(&flat::VarName) -> flat::VarName,
) {
    for assertion in asserts {
        rewrite_varrefs_in_expr(&mut assertion.condition, rewrite_name);
        rewrite_varrefs_in_expr(&mut assertion.message, rewrite_name);
        if let Some(level) = &mut assertion.level {
            rewrite_varrefs_in_expr(level, rewrite_name);
        }
    }
}

fn rewrite_varrefs_in_statements(
    statements: &mut [flat::Statement],
    rewrite_name: &impl Fn(&flat::VarName) -> flat::VarName,
) {
    for statement in statements {
        match statement {
            flat::Statement::Assignment { value, .. } | flat::Statement::Reinit { value, .. } => {
                rewrite_varrefs_in_expr(value, rewrite_name);
            }
            flat::Statement::FunctionCall { args, outputs, .. } => {
                for arg in args {
                    rewrite_varrefs_in_expr(arg, rewrite_name);
                }
                for out in outputs {
                    rewrite_varrefs_in_expr(out, rewrite_name);
                }
            }
            flat::Statement::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    rewrite_varrefs_in_expr(&mut block.cond, rewrite_name);
                    rewrite_varrefs_in_statements(&mut block.stmts, rewrite_name);
                }
                if let Some(stmts) = else_block {
                    rewrite_varrefs_in_statements(stmts, rewrite_name);
                }
            }
            flat::Statement::For { indices, equations } => {
                for idx in indices {
                    rewrite_varrefs_in_expr(&mut idx.range, rewrite_name);
                }
                rewrite_varrefs_in_statements(equations, rewrite_name);
            }
            flat::Statement::While(block) => {
                rewrite_varrefs_in_expr(&mut block.cond, rewrite_name);
                rewrite_varrefs_in_statements(&mut block.stmts, rewrite_name);
            }
            flat::Statement::When(blocks) => {
                for block in blocks {
                    rewrite_varrefs_in_expr(&mut block.cond, rewrite_name);
                    rewrite_varrefs_in_statements(&mut block.stmts, rewrite_name);
                }
            }
            flat::Statement::Assert {
                condition,
                message,
                level,
            } => {
                rewrite_varrefs_in_expr(condition, rewrite_name);
                rewrite_varrefs_in_expr(message, rewrite_name);
                if let Some(level) = level {
                    rewrite_varrefs_in_expr(level, rewrite_name);
                }
            }
            flat::Statement::Empty | flat::Statement::Return | flat::Statement::Break => {}
        }
    }
}

fn rewrite_varrefs_in_when_equations(
    equations: &mut [flat::WhenEquation],
    rewrite_name: &impl Fn(&flat::VarName) -> flat::VarName,
) {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                rewrite_varrefs_in_expr(value, rewrite_name);
            }
            flat::WhenEquation::Assert { condition, .. } => {
                rewrite_varrefs_in_expr(condition, rewrite_name);
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, nested) in branches {
                    rewrite_varrefs_in_expr(condition, rewrite_name);
                    rewrite_varrefs_in_when_equations(nested, rewrite_name);
                }
                rewrite_varrefs_in_when_equations(else_branch, rewrite_name);
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                rewrite_varrefs_in_expr(function, rewrite_name);
            }
            flat::WhenEquation::Terminate { .. } => {}
        }
    }
}

fn rewrite_varrefs_in_expr(
    expr: &mut flat::Expression,
    rewrite_name: &impl Fn(&flat::VarName) -> flat::VarName,
) {
    match expr {
        flat::Expression::VarRef { name, .. } => {
            *name = rewrite_name(name);
        }
        flat::Expression::FunctionCall { args, .. }
        | flat::Expression::BuiltinCall { args, .. } => {
            for arg in args {
                rewrite_varrefs_in_expr(arg, rewrite_name);
            }
        }
        flat::Expression::Binary { lhs, rhs, .. } => {
            rewrite_varrefs_in_expr(lhs, rewrite_name);
            rewrite_varrefs_in_expr(rhs, rewrite_name);
        }
        flat::Expression::Unary { rhs, .. } => rewrite_varrefs_in_expr(rhs, rewrite_name),
        flat::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, value) in branches {
                rewrite_varrefs_in_expr(cond, rewrite_name);
                rewrite_varrefs_in_expr(value, rewrite_name);
            }
            rewrite_varrefs_in_expr(else_branch, rewrite_name);
        }
        flat::Expression::Array { elements, .. } | flat::Expression::Tuple { elements } => {
            for element in elements {
                rewrite_varrefs_in_expr(element, rewrite_name);
            }
        }
        flat::Expression::Range { start, step, end } => {
            rewrite_varrefs_in_expr(start, rewrite_name);
            if let Some(step_expr) = step {
                rewrite_varrefs_in_expr(step_expr, rewrite_name);
            }
            rewrite_varrefs_in_expr(end, rewrite_name);
        }
        flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            rewrite_varrefs_in_expr(expr, rewrite_name);
            for idx in indices {
                rewrite_varrefs_in_expr(&mut idx.range, rewrite_name);
            }
            if let Some(filter_expr) = filter {
                rewrite_varrefs_in_expr(filter_expr, rewrite_name);
            }
        }
        flat::Expression::Index { base, subscripts } => {
            rewrite_varrefs_in_expr(base, rewrite_name);
            for subscript in subscripts {
                if let flat::Subscript::Expr(inner) = subscript {
                    rewrite_varrefs_in_expr(inner, rewrite_name);
                }
            }
        }
        flat::Expression::FieldAccess { base, .. } => rewrite_varrefs_in_expr(base, rewrite_name),
        flat::Expression::Literal(_) | flat::Expression::Empty => {}
    }
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
        let scope = crate::path_utils::parent_scope(var.name.as_str()).unwrap_or("");
        substitute_unqualified_scope_constants_opt(&mut var.binding, scope, ctx, live_vars, locals);
        substitute_unqualified_scope_constants_opt(&mut var.start, scope, ctx, live_vars, locals);
        substitute_unqualified_scope_constants_opt(&mut var.min, scope, ctx, live_vars, locals);
        substitute_unqualified_scope_constants_opt(&mut var.max, scope, ctx, live_vars, locals);
        substitute_unqualified_scope_constants_opt(&mut var.nominal, scope, ctx, live_vars, locals);
    }
}

fn substitute_unqualified_scope_constants_opt(
    expr: &mut Option<flat::Expression>,
    scope: &str,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) {
    if let Some(expr) = expr {
        *expr = substitute_unqualified_scope_constants_expr(
            expr.clone(),
            scope,
            ctx,
            live_vars,
            locals,
        );
    }
}

fn substitute_unqualified_scope_constants_expr(
    expr: flat::Expression,
    scope: &str,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> flat::Expression {
    if let flat::Expression::VarRef { name, subscripts } = &expr
        && subscripts.is_empty()
        && !crate::path_utils::has_top_level_dot(name.as_str())
        && !live_vars.contains(name.as_str())
        && !locals.contains(name.as_str())
    {
        return substitute_scoped_simple_var_ref(name.clone(), subscripts.clone(), scope, ctx);
    }
    substitute_unqualified_scope_constants_non_var_expr(expr, scope, ctx, live_vars, locals)
}

fn substitute_unqualified_scope_constants_non_var_expr(
    expr: flat::Expression,
    scope: &str,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> flat::Expression {
    match expr {
        flat::Expression::Binary { lhs, rhs, op } => flat::Expression::Binary {
            lhs: Box::new(substitute_unqualified_scope_constants_expr(
                *lhs, scope, ctx, live_vars, locals,
            )),
            rhs: Box::new(substitute_unqualified_scope_constants_expr(
                *rhs, scope, ctx, live_vars, locals,
            )),
            op,
        },
        flat::Expression::Unary { rhs, op } => flat::Expression::Unary {
            rhs: Box::new(substitute_unqualified_scope_constants_expr(
                *rhs, scope, ctx, live_vars, locals,
            )),
            op,
        },
        flat::Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => flat::Expression::FunctionCall {
            name,
            args: substitute_scoped_expr_vec(args, scope, ctx, live_vars, locals),
            is_constructor,
        },
        flat::Expression::BuiltinCall { function, args } => flat::Expression::BuiltinCall {
            function,
            args: substitute_scoped_expr_vec(args, scope, ctx, live_vars, locals),
        },
        flat::Expression::Array {
            elements,
            is_matrix,
        } => flat::Expression::Array {
            elements: substitute_scoped_expr_vec(elements, scope, ctx, live_vars, locals),
            is_matrix,
        },
        flat::Expression::Tuple { elements } => flat::Expression::Tuple {
            elements: substitute_scoped_expr_vec(elements, scope, ctx, live_vars, locals),
        },
        flat::Expression::Range { start, step, end } => flat::Expression::Range {
            start: Box::new(substitute_unqualified_scope_constants_expr(
                *start, scope, ctx, live_vars, locals,
            )),
            step: step.map(|s| {
                Box::new(substitute_unqualified_scope_constants_expr(
                    *s, scope, ctx, live_vars, locals,
                ))
            }),
            end: Box::new(substitute_unqualified_scope_constants_expr(
                *end, scope, ctx, live_vars, locals,
            )),
        },
        flat::Expression::FieldAccess { base, field } => flat::Expression::FieldAccess {
            base: Box::new(substitute_unqualified_scope_constants_expr(
                *base, scope, ctx, live_vars, locals,
            )),
            field,
        },
        flat::Expression::Index { base, subscripts } => flat::Expression::Index {
            base: Box::new(substitute_unqualified_scope_constants_expr(
                *base, scope, ctx, live_vars, locals,
            )),
            subscripts: substitute_scoped_subscripts(subscripts, scope, ctx, live_vars, locals),
        },
        flat::Expression::If {
            branches,
            else_branch,
        } => substitute_scoped_if_expr(branches, *else_branch, scope, ctx, live_vars, locals),
        flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => substitute_scoped_array_comprehension_expr(
            *expr, indices, filter, scope, ctx, live_vars, locals,
        ),
        flat::Expression::VarRef { name, subscripts } => {
            flat::Expression::VarRef { name, subscripts }
        }
        other => other,
    }
}

fn substitute_scoped_if_expr(
    branches: Vec<(flat::Expression, flat::Expression)>,
    else_branch: flat::Expression,
    scope: &str,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> flat::Expression {
    flat::Expression::If {
        branches: branches
            .into_iter()
            .map(|(cond, value)| {
                (
                    substitute_unqualified_scope_constants_expr(
                        cond, scope, ctx, live_vars, locals,
                    ),
                    substitute_unqualified_scope_constants_expr(
                        value, scope, ctx, live_vars, locals,
                    ),
                )
            })
            .collect(),
        else_branch: Box::new(substitute_unqualified_scope_constants_expr(
            else_branch,
            scope,
            ctx,
            live_vars,
            locals,
        )),
    }
}

fn substitute_scoped_array_comprehension_expr(
    expr: flat::Expression,
    indices: Vec<flat::ComprehensionIndex>,
    filter: Option<Box<flat::Expression>>,
    scope: &str,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> flat::Expression {
    flat::Expression::ArrayComprehension {
        expr: Box::new(substitute_unqualified_scope_constants_expr(
            expr, scope, ctx, live_vars, locals,
        )),
        indices: indices
            .into_iter()
            .map(|mut idx| {
                idx.range = substitute_unqualified_scope_constants_expr(
                    idx.range, scope, ctx, live_vars, locals,
                );
                idx
            })
            .collect(),
        filter: filter.map(|f| {
            Box::new(substitute_unqualified_scope_constants_expr(
                *f, scope, ctx, live_vars, locals,
            ))
        }),
    }
}

fn substitute_scoped_simple_var_ref(
    name: flat::VarName,
    subscripts: Vec<flat::Subscript>,
    scope: &str,
    ctx: &Context,
) -> flat::Expression {
    if let Some(v) =
        lookup_constant_expr_with_scope_local(name.as_str(), scope, &ctx.constant_values)
    {
        return v;
    }
    if let Some(v) = lookup_with_scope_local(name.as_str(), scope, &ctx.real_parameter_values)
        && v.is_finite()
    {
        return flat::Expression::Literal(flat::Literal::Real(v));
    }
    if let Some(v) = lookup_with_scope_local(name.as_str(), scope, &ctx.parameter_values) {
        return flat::Expression::Literal(flat::Literal::Integer(v));
    }
    if let Some(v) = lookup_with_scope_local(name.as_str(), scope, &ctx.boolean_parameter_values) {
        return flat::Expression::Literal(flat::Literal::Boolean(v));
    }
    if let Some(v) = lookup_with_scope_local(name.as_str(), scope, &ctx.enum_parameter_values) {
        return flat::Expression::VarRef {
            name: flat::VarName::new(v),
            subscripts: vec![],
        };
    }
    flat::Expression::VarRef { name, subscripts }
}

fn substitute_scoped_expr_vec(
    exprs: Vec<flat::Expression>,
    scope: &str,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> Vec<flat::Expression> {
    exprs
        .into_iter()
        .map(|expr| {
            substitute_unqualified_scope_constants_expr(expr, scope, ctx, live_vars, locals)
        })
        .collect()
}

fn substitute_scoped_subscripts(
    subscripts: Vec<flat::Subscript>,
    scope: &str,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
) -> Vec<flat::Subscript> {
    subscripts
        .into_iter()
        .map(|subscript| match subscript {
            flat::Subscript::Expr(expr) => flat::Subscript::Expr(Box::new(
                substitute_unqualified_scope_constants_expr(*expr, scope, ctx, live_vars, locals),
            )),
            other => other,
        })
        .collect()
}

fn lookup_with_scope_local<V: Clone + PartialEq>(
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
        return lookup_unique_dotted_suffix_value(name, map).cloned();
    }
    None
}

fn lookup_constant_expr_with_scope_local(
    name: &str,
    scope: &str,
    map: &rustc_hash::FxHashMap<String, flat::Expression>,
) -> Option<flat::Expression> {
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
        let mut found: Option<flat::Expression> = None;
        for (key, value) in map {
            if !key.ends_with(&suffix) {
                continue;
            }
            if found.is_some() {
                return None;
            }
            found = Some(value.clone());
        }
        return found;
    }
    None
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
        let function_scope = crate::path_utils::parent_scope(function.name.as_str()).unwrap_or("");

        for param in function
            .inputs
            .iter_mut()
            .chain(function.outputs.iter_mut())
            .chain(function.locals.iter_mut())
        {
            substitute_opt_expr(&mut param.default, ctx, live_vars, &function_locals);
            substitute_unqualified_scope_constants_opt(
                &mut param.default,
                function_scope,
                ctx,
                live_vars,
                &function_locals,
            );
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
    if let Some(expr) = lookup_scalar_key_value(key, ctx) {
        return Some(expr);
    }
    if let Some(case_variant_key) = resolve_case_variant_symbol_key(key, ctx)
        && let Some(expr) = lookup_scalar_key_value(&case_variant_key, ctx)
    {
        return Some(expr);
    }
    if !crate::path_utils::has_top_level_dot(key)
        && let Some(expr) = lookup_scalar_suffix_value(key, ctx)
    {
        return Some(expr);
    }
    if let Some(expr) = resolve_inline_indexed_constant(key, ctx) {
        return Some(expr);
    }

    if let Some(resolved_key) = resolve_varref_through_constant_aliases(key, ctx)
        && resolved_key != key
    {
        if let Some(expr) = lookup_scalar_key_value(&resolved_key, ctx) {
            return Some(expr);
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

fn lookup_scalar_key_value(key: &str, ctx: &Context) -> Option<flat::Expression> {
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
    None
}

fn lookup_scalar_suffix_value(key: &str, ctx: &Context) -> Option<flat::Expression> {
    if let Some(expr) = lookup_unique_dotted_suffix_expr(key, &ctx.constant_values) {
        return Some(expr.clone());
    }
    if let Some(v) = lookup_unique_dotted_suffix_value(key, &ctx.real_parameter_values)
        && v.is_finite()
    {
        return Some(flat::Expression::Literal(flat::Literal::Real(*v)));
    }
    if let Some(v) = lookup_unique_dotted_suffix_value(key, &ctx.parameter_values) {
        return Some(flat::Expression::Literal(flat::Literal::Integer(*v)));
    }
    if let Some(v) = lookup_unique_dotted_suffix_value(key, &ctx.boolean_parameter_values) {
        return Some(flat::Expression::Literal(flat::Literal::Boolean(*v)));
    }
    if let Some(v) = lookup_unique_dotted_suffix_value(key, &ctx.enum_parameter_values) {
        return Some(flat::Expression::VarRef {
            name: flat::VarName::new(v.clone()),
            subscripts: vec![],
        });
    }
    None
}

fn resolve_case_variant_symbol_key(key: &str, ctx: &Context) -> Option<String> {
    if !crate::path_utils::has_top_level_dot(key) {
        return None;
    }
    let mut parts: Vec<String> = key.split('.').map(|s| s.to_string()).collect();
    if parts.len() < 2 {
        return None;
    }

    let mut candidates = Vec::new();
    for idx in 1..parts.len() {
        let segment = &parts[idx];
        let mut chars = segment.chars();
        let Some(first) = chars.next() else {
            continue;
        };
        if !first.is_ascii_alphabetic() {
            continue;
        }
        let rest = chars.as_str();
        let flipped = if first.is_ascii_lowercase() {
            format!("{}{}", first.to_ascii_uppercase(), rest)
        } else {
            format!("{}{}", first.to_ascii_lowercase(), rest)
        };

        let original = parts[idx].clone();
        parts[idx] = flipped;
        let candidate = parts.join(".");
        parts[idx] = original;

        if symbol_key_exists_in_any_context_map(&candidate, ctx) {
            candidates.push(candidate);
        }
    }

    if candidates.len() == 1 {
        return candidates.into_iter().next();
    }
    None
}

fn symbol_key_exists_in_any_context_map(key: &str, ctx: &Context) -> bool {
    ctx.constant_values.contains_key(key)
        || ctx.real_parameter_values.contains_key(key)
        || ctx.parameter_values.contains_key(key)
        || ctx.boolean_parameter_values.contains_key(key)
        || ctx.enum_parameter_values.contains_key(key)
}

fn lookup_unique_dotted_suffix_value<'a, T: PartialEq>(
    simple_name: &str,
    map: &'a rustc_hash::FxHashMap<String, T>,
) -> Option<&'a T> {
    let suffix = format!(".{simple_name}");
    let mut found: Option<&T> = None;
    for (key, value) in map {
        if !key.ends_with(&suffix) {
            continue;
        }
        if found.is_some_and(|prev| prev != value) {
            return None;
        }
        found = Some(value);
    }
    found
}

fn lookup_unique_dotted_suffix_expr<'a>(
    simple_name: &str,
    map: &'a rustc_hash::FxHashMap<String, flat::Expression>,
) -> Option<&'a flat::Expression> {
    let suffix = format!(".{simple_name}");
    let mut found: Option<&flat::Expression> = None;
    for (key, value) in map {
        if !key.ends_with(&suffix) {
            continue;
        }
        if found.is_some() {
            return None;
        }
        found = Some(value);
    }
    found
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
#[path = "postprocess_tests.rs"]
mod postprocess_tests;
