use super::*;
use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

type ComponentReference = dae::ComponentReference;
type ComprehensionIndex = dae::ComprehensionIndex;
type Dae = dae::Dae;
type Expression = dae::Expression;
type ForIndex = dae::ForIndex;
type Function = dae::Function;
type Statement = dae::Statement;
type StatementBlock = dae::StatementBlock;
type Subscript = dae::Subscript;
type VarName = dae::VarName;

fn short_leaf_matches(candidate: &str, short: &str) -> bool {
    candidate
        .rsplit('.')
        .next()
        .is_some_and(|leaf| leaf == short)
}

fn validate_constructor_field_projection(
    base: &Expression,
    field: &str,
    functions: &IndexMap<VarName, Function>,
    span: Span,
) -> Result<(), ToDaeError> {
    if let Expression::FunctionCall {
        name,
        args,
        is_constructor: true,
    } = base
    {
        for arg in args {
            validate_expression_constructor_projections(arg, functions, span)?;
        }

        if matches!(field, "re" | "im") {
            return Ok(());
        }

        let projected_name = format!("{}.{}", name.as_str(), field);
        let Some(constructor) = functions.get(name) else {
            if std::env::var("RUMOCA_DEBUG_TODAE").is_ok() {
                let mut candidates: Vec<String> =
                    functions.keys().map(|f| f.as_str().to_string()).collect();
                candidates.sort();
                let short = name
                    .as_str()
                    .rsplit('.')
                    .next()
                    .unwrap_or(name.as_str())
                    .to_string();
                let mut short_matches: Vec<String> = candidates
                    .iter()
                    .filter(|candidate| short_leaf_matches(candidate, &short))
                    .cloned()
                    .collect();
                short_matches.sort();
                eprintln!(
                    "DEBUG TODAE missing constructor projection={} args_len={} short_matches={short_matches:?} total_functions={}",
                    projected_name,
                    args.len(),
                    candidates.len()
                );
            }
            return Err(ToDaeError::constructor_field_projection_unresolved(
                projected_name,
                span,
            ));
        };

        let field_known = constructor.inputs.iter().any(|param| param.name == field)
            || constructor.outputs.iter().any(|param| param.name == field);
        if !field_known {
            if std::env::var("RUMOCA_DEBUG_TODAE").is_ok() {
                let mut available_fields: Vec<String> = constructor
                    .inputs
                    .iter()
                    .map(|param| format!("in:{}", param.name))
                    .chain(
                        constructor
                            .outputs
                            .iter()
                            .map(|param| format!("out:{}", param.name)),
                    )
                    .collect();
                available_fields.sort();
                eprintln!(
                    "DEBUG TODAE constructor field missing projection={} available={available_fields:?}",
                    projected_name
                );
            }
            return Err(ToDaeError::constructor_field_projection_unresolved(
                projected_name,
                span,
            ));
        }
        return Ok(());
    }

    validate_expression_constructor_projections(base, functions, span)
}

fn validate_expression_constructor_projections(
    expr: &Expression,
    functions: &IndexMap<VarName, Function>,
    span: Span,
) -> Result<(), ToDaeError> {
    match expr {
        Expression::FunctionCall { args, .. } | Expression::BuiltinCall { args, .. } => {
            for arg in args {
                validate_expression_constructor_projections(arg, functions, span)?;
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            validate_expression_constructor_projections(lhs, functions, span)?;
            validate_expression_constructor_projections(rhs, functions, span)?;
        }
        Expression::Unary { rhs, .. } => {
            validate_expression_constructor_projections(rhs, functions, span)?;
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, value) in branches {
                validate_expression_constructor_projections(cond, functions, span)?;
                validate_expression_constructor_projections(value, functions, span)?;
            }
            validate_expression_constructor_projections(else_branch, functions, span)?;
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            for element in elements {
                validate_expression_constructor_projections(element, functions, span)?;
            }
        }
        Expression::Range { start, step, end } => {
            validate_expression_constructor_projections(start, functions, span)?;
            if let Some(step) = step {
                validate_expression_constructor_projections(step, functions, span)?;
            }
            validate_expression_constructor_projections(end, functions, span)?;
        }
        Expression::Index { base, subscripts } => {
            validate_expression_constructor_projections(base, functions, span)?;
            for subscript in subscripts {
                if let Subscript::Expr(expr) = subscript {
                    validate_expression_constructor_projections(expr, functions, span)?;
                }
            }
        }
        Expression::FieldAccess { base, field } => {
            validate_constructor_field_projection(base, field, functions, span)?;
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            validate_expression_constructor_projections(expr, functions, span)?;
            for index in indices {
                validate_expression_constructor_projections(&index.range, functions, span)?;
            }
            if let Some(filter_expr) = filter {
                validate_expression_constructor_projections(filter_expr, functions, span)?;
            }
        }
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => {}
    }
    Ok(())
}

fn validate_statement_constructor_projections(
    stmt: &Statement,
    functions: &IndexMap<VarName, Function>,
    span: Span,
) -> Result<(), ToDaeError> {
    match stmt {
        Statement::Assignment { value, .. } | Statement::Reinit { value, .. } => {
            validate_expression_constructor_projections(value, functions, span)?
        }
        Statement::For { indices, equations } => {
            for index in indices {
                validate_expression_constructor_projections(&index.range, functions, span)?;
            }
            for nested in equations {
                validate_statement_constructor_projections(nested, functions, span)?;
            }
        }
        Statement::While(block) => {
            validate_expression_constructor_projections(&block.cond, functions, span)?;
            for nested in &block.stmts {
                validate_statement_constructor_projections(nested, functions, span)?;
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                validate_expression_constructor_projections(&block.cond, functions, span)?;
                for nested in &block.stmts {
                    validate_statement_constructor_projections(nested, functions, span)?;
                }
            }
            if let Some(else_block) = else_block {
                for nested in else_block {
                    validate_statement_constructor_projections(nested, functions, span)?;
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                validate_expression_constructor_projections(&block.cond, functions, span)?;
                for nested in &block.stmts {
                    validate_statement_constructor_projections(nested, functions, span)?;
                }
            }
        }
        Statement::FunctionCall { args, outputs, .. } => {
            for arg in args {
                validate_expression_constructor_projections(arg, functions, span)?;
            }
            for output in outputs {
                validate_expression_constructor_projections(output, functions, span)?;
            }
        }
        Statement::Assert {
            condition,
            message,
            level,
        } => {
            validate_expression_constructor_projections(condition, functions, span)?;
            validate_expression_constructor_projections(message, functions, span)?;
            if let Some(level) = level {
                validate_expression_constructor_projections(level, functions, span)?;
            }
        }
        Statement::Empty | Statement::Return | Statement::Break => {}
    }
    Ok(())
}

pub(super) fn validate_dae_constructor_field_projections(dae: &Dae) -> Result<(), ToDaeError> {
    for variable in dae
        .states
        .values()
        .chain(dae.algebraics.values())
        .chain(dae.inputs.values())
        .chain(dae.outputs.values())
        .chain(dae.parameters.values())
        .chain(dae.constants.values())
        .chain(dae.discrete_reals.values())
        .chain(dae.discrete_valued.values())
        .chain(dae.derivative_aliases.values())
    {
        for expr in [
            variable.start.as_ref(),
            variable.min.as_ref(),
            variable.max.as_ref(),
            variable.nominal.as_ref(),
        ]
        .into_iter()
        .flatten()
        {
            validate_expression_constructor_projections(expr, &dae.functions, Span::DUMMY)?;
        }
    }

    for equation in dae
        .f_x
        .iter()
        .chain(dae.f_z.iter())
        .chain(dae.f_m.iter())
        .chain(dae.f_c.iter())
        .chain(dae.initial_equations.iter())
    {
        validate_expression_constructor_projections(&equation.rhs, &dae.functions, equation.span)?;
    }

    for function in dae.functions.values() {
        for param in function
            .inputs
            .iter()
            .chain(function.outputs.iter())
            .chain(function.locals.iter())
        {
            if let Some(default_expr) = &param.default {
                validate_expression_constructor_projections(
                    default_expr,
                    &dae.functions,
                    function.span,
                )?;
            }
        }
        for statement in &function.body {
            validate_statement_constructor_projections(statement, &dae.functions, function.span)?;
        }
    }

    Ok(())
}

pub(super) fn validate_dae_references(
    dae: &Dae,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    validate_variable_reference_attributes(dae, known_flat_var_names)?;
    validate_equation_rhs_references(dae, known_flat_var_names)?;
    validate_relation_references(dae, known_flat_var_names)?;
    validate_function_references(dae, known_flat_var_names)?;
    Ok(())
}

fn validate_variable_reference_attributes(
    dae: &Dae,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    for variable in dae
        .states
        .values()
        .chain(dae.algebraics.values())
        .chain(dae.inputs.values())
        .chain(dae.outputs.values())
        .chain(dae.parameters.values())
        .chain(dae.constants.values())
        .chain(dae.discrete_reals.values())
        .chain(dae.discrete_valued.values())
        .chain(dae.derivative_aliases.values())
    {
        for expr in [
            variable.start.as_ref(),
            variable.min.as_ref(),
            variable.max.as_ref(),
            variable.nominal.as_ref(),
        ]
        .into_iter()
        .flatten()
        {
            validate_expression_references(expr, dae, Span::DUMMY, None, known_flat_var_names)?;
        }
    }
    Ok(())
}

fn validate_equation_rhs_references(
    dae: &Dae,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    for equation in dae
        .f_x
        .iter()
        .chain(dae.f_z.iter())
        .chain(dae.f_m.iter())
        .chain(dae.f_c.iter())
        .chain(dae.initial_equations.iter())
    {
        validate_expression_references(
            &equation.rhs,
            dae,
            equation.span,
            None,
            known_flat_var_names,
        )?;
    }
    Ok(())
}

fn validate_relation_references(
    dae: &Dae,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    for relation in &dae.relation {
        validate_expression_references(relation, dae, Span::DUMMY, None, known_flat_var_names)?;
    }
    Ok(())
}

fn validate_function_references(
    dae: &Dae,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    for function in dae.functions.values() {
        let function_scope: HashSet<&str> = function
            .inputs
            .iter()
            .chain(function.outputs.iter())
            .chain(function.locals.iter())
            .map(|param| param.name.as_str())
            .collect();

        for param in function
            .inputs
            .iter()
            .chain(function.outputs.iter())
            .chain(function.locals.iter())
        {
            if let Some(default_expr) = &param.default {
                validate_expression_references(
                    default_expr,
                    dae,
                    function.span,
                    Some(&function_scope),
                    known_flat_var_names,
                )?;
            }
        }

        validate_statement_slice_references(
            &function.body,
            dae,
            function.span,
            Some(&function_scope),
            known_flat_var_names,
        )?;
    }
    Ok(())
}

fn validate_statement_slice_references(
    statements: &[Statement],
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    for statement in statements {
        validate_statement_references(statement, dae, span, function_scope, known_flat_var_names)?;
    }
    Ok(())
}

fn validate_statement_references(
    stmt: &Statement,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    match stmt {
        Statement::Empty | Statement::Return | Statement::Break => Ok(()),
        Statement::Assignment { comp, value } => validate_assignment_statement_references(
            comp,
            value,
            dae,
            span,
            function_scope,
            known_flat_var_names,
        ),
        Statement::For { indices, equations } => validate_for_statement_references(
            indices,
            equations,
            dae,
            span,
            function_scope,
            known_flat_var_names,
        ),
        Statement::While(block) => validate_block_statement_references(
            block,
            dae,
            span,
            function_scope,
            known_flat_var_names,
        ),
        Statement::If {
            cond_blocks,
            else_block,
        } => validate_if_statement_references(
            cond_blocks,
            else_block.as_deref(),
            dae,
            span,
            function_scope,
            known_flat_var_names,
        ),
        Statement::When(blocks) => validate_when_statement_references(
            blocks,
            dae,
            span,
            function_scope,
            known_flat_var_names,
        ),
        Statement::FunctionCall { args, outputs, .. } => {
            validate_function_call_statement_references(
                args,
                outputs,
                dae,
                span,
                function_scope,
                known_flat_var_names,
            )
        }
        Statement::Reinit { variable, value } => validate_reinit_statement_references(
            variable,
            value,
            dae,
            span,
            function_scope,
            known_flat_var_names,
        ),
        Statement::Assert {
            condition,
            message,
            level,
        } => validate_assert_statement_references(
            condition,
            message,
            level.as_ref(),
            dae,
            span,
            function_scope,
            known_flat_var_names,
        ),
    }
}

fn validate_assignment_statement_references(
    comp: &ComponentReference,
    value: &Expression,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    validate_component_reference_target(comp, dae, span, function_scope, known_flat_var_names)?;
    validate_expression_references(value, dae, span, function_scope, known_flat_var_names)
}

fn validate_for_statement_references(
    indices: &[ForIndex],
    equations: &[Statement],
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    let mut loop_scope: HashSet<&str> = function_scope
        .map(|scope| scope.iter().copied().collect())
        .unwrap_or_default();
    for index in indices {
        validate_expression_references(
            &index.range,
            dae,
            span,
            Some(&loop_scope),
            known_flat_var_names,
        )?;
        loop_scope.insert(index.ident.as_str());
    }
    validate_statement_slice_references(
        equations,
        dae,
        span,
        Some(&loop_scope),
        known_flat_var_names,
    )
}

fn validate_block_statement_references(
    block: &StatementBlock,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    validate_expression_references(&block.cond, dae, span, function_scope, known_flat_var_names)?;
    validate_statement_slice_references(
        &block.stmts,
        dae,
        span,
        function_scope,
        known_flat_var_names,
    )
}

fn validate_if_statement_references(
    cond_blocks: &[StatementBlock],
    else_block: Option<&[Statement]>,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    for block in cond_blocks {
        validate_block_statement_references(
            block,
            dae,
            span,
            function_scope,
            known_flat_var_names,
        )?;
    }
    if let Some(else_block) = else_block {
        validate_statement_slice_references(
            else_block,
            dae,
            span,
            function_scope,
            known_flat_var_names,
        )?;
    }
    Ok(())
}

fn validate_when_statement_references(
    blocks: &[StatementBlock],
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    for block in blocks {
        validate_block_statement_references(
            block,
            dae,
            span,
            function_scope,
            known_flat_var_names,
        )?;
    }
    Ok(())
}

fn validate_function_call_statement_references(
    args: &[Expression],
    outputs: &[Expression],
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    for arg in args {
        validate_expression_references(arg, dae, span, function_scope, known_flat_var_names)?;
    }
    for output in outputs {
        validate_expression_references(output, dae, span, function_scope, known_flat_var_names)?;
    }
    Ok(())
}

fn validate_reinit_statement_references(
    variable: &ComponentReference,
    value: &Expression,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    validate_component_reference_target(variable, dae, span, function_scope, known_flat_var_names)?;
    validate_expression_references(value, dae, span, function_scope, known_flat_var_names)
}

fn validate_assert_statement_references(
    condition: &Expression,
    message: &Expression,
    level: Option<&Expression>,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    validate_expression_references(condition, dae, span, function_scope, known_flat_var_names)?;
    validate_expression_references(message, dae, span, function_scope, known_flat_var_names)?;
    if let Some(level) = level {
        validate_expression_references(level, dae, span, function_scope, known_flat_var_names)?;
    }
    Ok(())
}

fn validate_component_reference_target(
    comp: &ComponentReference,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    if let Some(scope) = function_scope {
        let target_name = comp.to_var_name();
        if function_scope_contains_reference(&target_name, scope) {
            return Ok(());
        }
    }
    let expr = Expression::VarRef {
        name: comp.to_var_name(),
        subscripts: vec![],
    };
    validate_expression_references(&expr, dae, span, function_scope, known_flat_var_names)
}

fn validate_expression_references(
    expr: &Expression,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    match expr {
        Expression::VarRef { name, subscripts } => validate_var_ref_expression_references(
            name,
            subscripts,
            dae,
            span,
            function_scope,
            known_flat_var_names,
        ),
        Expression::Binary { lhs, rhs, .. } => {
            validate_expression_references(lhs, dae, span, function_scope, known_flat_var_names)?;
            validate_expression_references(rhs, dae, span, function_scope, known_flat_var_names)
        }
        Expression::Unary { rhs, .. } => {
            validate_expression_references(rhs, dae, span, function_scope, known_flat_var_names)
        }
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            validate_expression_slice_references(
                args,
                dae,
                span,
                function_scope,
                known_flat_var_names,
            )
        }
        Expression::If {
            branches,
            else_branch,
        } => validate_if_expression_references(
            branches,
            else_branch,
            dae,
            span,
            function_scope,
            known_flat_var_names,
        ),
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            validate_expression_slice_references(
                elements,
                dae,
                span,
                function_scope,
                known_flat_var_names,
            )
        }
        Expression::Range { start, step, end } => validate_range_expression_references(
            start,
            step.as_deref(),
            end,
            dae,
            span,
            function_scope,
            known_flat_var_names,
        ),
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => validate_array_comprehension_references(
            expr,
            indices,
            filter.as_deref(),
            dae,
            span,
            function_scope,
            known_flat_var_names,
        ),
        Expression::Index { base, subscripts } => {
            validate_expression_references(base, dae, span, function_scope, known_flat_var_names)?;
            validate_subscript_expr_references(
                subscripts,
                dae,
                span,
                function_scope,
                known_flat_var_names,
            )?;
            Ok(())
        }
        Expression::FieldAccess { base, .. } => {
            validate_expression_references(base, dae, span, function_scope, known_flat_var_names)
        }
        Expression::Literal(_) | Expression::Empty => Ok(()),
    }
}

fn validate_var_ref_expression_references(
    name: &VarName,
    subscripts: &[Subscript],
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    if let Some(scope) = function_scope
        && function_scope_contains_reference(name, scope)
    {
        return validate_subscript_expr_references(
            subscripts,
            dae,
            span,
            function_scope,
            known_flat_var_names,
        );
    }

    if !is_known_dae_reference(dae, name, known_flat_var_names) {
        return Err(ToDaeError::unresolved_reference(name.as_str(), span));
    }

    validate_subscript_expr_references(subscripts, dae, span, function_scope, known_flat_var_names)
}

fn function_scope_contains_reference(name: &VarName, scope: &HashSet<&str>) -> bool {
    if scope.contains(name.as_str()) {
        return true;
    }
    path_utils::get_top_level_prefix(name.as_str())
        .is_some_and(|prefix| scope.contains(prefix.as_str()))
}

fn validate_expression_slice_references(
    expressions: &[Expression],
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    for expression in expressions {
        validate_expression_references(
            expression,
            dae,
            span,
            function_scope,
            known_flat_var_names,
        )?;
    }
    Ok(())
}

fn validate_if_expression_references(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    for (cond, branch) in branches {
        validate_expression_references(cond, dae, span, function_scope, known_flat_var_names)?;
        validate_expression_references(branch, dae, span, function_scope, known_flat_var_names)?;
    }
    validate_expression_references(else_branch, dae, span, function_scope, known_flat_var_names)
}

fn validate_range_expression_references(
    start: &Expression,
    step: Option<&Expression>,
    end: &Expression,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    validate_expression_references(start, dae, span, function_scope, known_flat_var_names)?;
    if let Some(step) = step {
        validate_expression_references(step, dae, span, function_scope, known_flat_var_names)?;
    }
    validate_expression_references(end, dae, span, function_scope, known_flat_var_names)
}

fn validate_array_comprehension_references(
    expr: &Expression,
    indices: &[ComprehensionIndex],
    filter: Option<&Expression>,
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    let mut comprehension_scope: HashSet<&str> = function_scope
        .map(|scope| scope.iter().copied().collect())
        .unwrap_or_default();
    for index in indices {
        validate_expression_references(
            &index.range,
            dae,
            span,
            Some(&comprehension_scope),
            known_flat_var_names,
        )?;
        comprehension_scope.insert(index.name.as_str());
    }

    validate_expression_references(
        expr,
        dae,
        span,
        Some(&comprehension_scope),
        known_flat_var_names,
    )?;
    if let Some(filter) = filter {
        validate_expression_references(
            filter,
            dae,
            span,
            Some(&comprehension_scope),
            known_flat_var_names,
        )?;
    }
    Ok(())
}

fn validate_subscript_expr_references(
    subscripts: &[Subscript],
    dae: &Dae,
    span: Span,
    function_scope: Option<&HashSet<&str>>,
    known_flat_var_names: &HashSet<String>,
) -> Result<(), ToDaeError> {
    for subscript in subscripts {
        if let Subscript::Expr(expr) = subscript {
            validate_expression_references(expr, dae, span, function_scope, known_flat_var_names)?;
        }
    }
    Ok(())
}

fn is_known_dae_reference(
    dae: &Dae,
    name: &VarName,
    known_flat_var_names: &HashSet<String>,
) -> bool {
    let raw = name.as_str();
    if raw == "time" {
        return true;
    }

    if is_known_enum_literal_name(raw, &dae.enum_literal_ordinals) {
        return true;
    }
    if is_known_flat_reference_name(raw, known_flat_var_names) || has_known_dae_descendant(dae, raw)
    {
        return true;
    }

    if known_flat_var_names.contains(raw)
        || dae.states.contains_key(name)
        || dae.algebraics.contains_key(name)
        || dae.inputs.contains_key(name)
        || dae.outputs.contains_key(name)
        || dae.parameters.contains_key(name)
        || dae.constants.contains_key(name)
        || dae.discrete_reals.contains_key(name)
        || dae.discrete_valued.contains_key(name)
        || dae.derivative_aliases.contains_key(name)
    {
        return true;
    }

    path_utils::subscript_fallback_chain(&dae_to_flat_var_name(name))
        .into_iter()
        .any(|candidate| {
            let dae_candidate = flat_to_dae_var_name(&candidate);
            known_flat_var_names.contains(candidate.as_str())
                || dae.states.contains_key(&dae_candidate)
                || dae.algebraics.contains_key(&dae_candidate)
                || dae.inputs.contains_key(&dae_candidate)
                || dae.outputs.contains_key(&dae_candidate)
                || dae.parameters.contains_key(&dae_candidate)
                || dae.constants.contains_key(&dae_candidate)
                || dae.discrete_reals.contains_key(&dae_candidate)
                || dae.discrete_valued.contains_key(&dae_candidate)
                || dae.derivative_aliases.contains_key(&dae_candidate)
        })
}

fn is_known_flat_reference_name(name: &str, known_flat_var_names: &HashSet<String>) -> bool {
    if known_flat_var_names.contains(name)
        || has_known_descendant_in_names(name, known_flat_var_names)
    {
        return true;
    }

    if has_known_normalized_reference_name(name, known_flat_var_names) {
        return true;
    }

    let fallback_name = flat::VarName::new(name);
    path_utils::subscript_fallback_chain(&fallback_name)
        .iter()
        .map(flat::VarName::as_str)
        .any(|candidate| {
            known_flat_var_names.contains(candidate)
                || has_known_descendant_in_names(candidate, known_flat_var_names)
                || has_known_normalized_reference_name(candidate, known_flat_var_names)
        })
}

fn has_known_normalized_reference_name(name: &str, known_names: &HashSet<String>) -> bool {
    let normalized = path_utils::strip_all_subscripts(name);
    if normalized.is_empty() {
        return false;
    }
    let dot_prefix = format!("{normalized}.");
    known_names.iter().any(|candidate| {
        let candidate_normalized = path_utils::strip_all_subscripts(candidate);
        candidate_normalized == normalized || candidate_normalized.starts_with(&dot_prefix)
    })
}

fn has_known_descendant_in_names(name: &str, known_names: &HashSet<String>) -> bool {
    let dot_prefix = format!("{name}.");
    let array_prefix = format!("{name}[");
    known_names
        .iter()
        .any(|candidate| candidate.starts_with(&dot_prefix) || candidate.starts_with(&array_prefix))
}

fn has_known_dae_descendant(dae: &Dae, name: &str) -> bool {
    let dot_prefix = format!("{name}.");
    let array_prefix = format!("{name}[");
    dae.states
        .keys()
        .chain(dae.algebraics.keys())
        .chain(dae.inputs.keys())
        .chain(dae.outputs.keys())
        .chain(dae.parameters.keys())
        .chain(dae.constants.keys())
        .chain(dae.discrete_reals.keys())
        .chain(dae.discrete_valued.keys())
        .chain(dae.derivative_aliases.keys())
        .map(VarName::as_str)
        .any(|candidate| candidate.starts_with(&dot_prefix) || candidate.starts_with(&array_prefix))
}

fn is_known_enum_literal_name(name: &str, ordinals: &IndexMap<String, i64>) -> bool {
    if ordinals.contains_key(name) {
        return true;
    }
    let Some((prefix, literal)) = name.rsplit_once('.') else {
        return false;
    };
    if literal.len() >= 2 && literal.starts_with('\'') && literal.ends_with('\'') {
        let unquoted = &literal[1..literal.len() - 1];
        return ordinals.contains_key(&format!("{prefix}.{unquoted}"));
    }
    ordinals.contains_key(&format!("{prefix}.'{literal}'"))
}
