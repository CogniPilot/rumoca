use std::collections::HashSet;

use rumoca_phase_structural::projection_maps::output_is_complex_record;
use rumoca_eval_dae::runtime as eval;
use rumoca_ir_dae as dae;

type BuiltinFunction = dae::BuiltinFunction;
type ComponentReference = dae::ComponentReference;
type Dae = dae::Dae;
type Expression = dae::Expression;
type ForIndex = dae::ForIndex;
type Statement = dae::Statement;
type StatementBlock = dae::StatementBlock;
type Subscript = dae::Subscript;
type VarName = dae::VarName;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionValidationError {
    pub name: String,
    pub reason: String,
}

fn resolve_dae_function<'a>(dae: &'a Dae, name: &VarName) -> Option<&'a rumoca_ir_dae::Function> {
    if let Some(function) = dae.functions.get(name) {
        return Some(function);
    }

    fn parse_output_projection_suffix(suffix: &str) -> Option<(&str, Vec<usize>)> {
        if suffix.is_empty() {
            return None;
        }
        if let Some(open) = suffix.find('[') {
            if !suffix.ends_with(']') || open == 0 {
                return None;
            }
            let output_name = &suffix[..open];
            let inner = &suffix[open + 1..suffix.len() - 1];
            let indices = inner
                .split(',')
                .map(str::trim)
                .map(|token| token.parse::<usize>().ok())
                .collect::<Option<Vec<_>>>()?;
            return Some((output_name, indices));
        }
        Some((suffix, Vec::new()))
    }

    fn projection_matches_output(function: &rumoca_ir_dae::Function, suffix: &str) -> bool {
        let Some((output_name, indices)) = parse_output_projection_suffix(suffix) else {
            return false;
        };

        let (base_output_name, projected_field) =
            if let Some((base, field)) = output_name.split_once('.') {
                (base, Some(field))
            } else {
                (output_name, None)
            };

        let Some(output) = function
            .outputs
            .iter()
            .find(|out| out.name == base_output_name)
        else {
            return false;
        };

        if let Some(field) = projected_field {
            if !output_is_complex_record(output) {
                return false;
            }
            if !matches!(field, "re" | "im") {
                return false;
            }
        }

        if output.dims.is_empty() {
            return indices.is_empty();
        }

        if indices.is_empty() {
            return false;
        }

        let total = output
            .dims
            .iter()
            .try_fold(1usize, |acc, dim| {
                if *dim <= 0 {
                    None
                } else {
                    acc.checked_mul(*dim as usize)
                }
            })
            .unwrap_or(0);
        if total == 0 {
            return false;
        }

        if indices.len() == 1 {
            let idx = indices[0];
            return idx >= 1 && idx <= total;
        }

        if indices.len() != output.dims.len() {
            return false;
        }

        indices
            .iter()
            .zip(output.dims.iter())
            .all(|(idx, dim)| *dim > 0 && *idx >= 1 && *idx <= *dim as usize)
    }

    let requested = name.as_str();
    let mut split_positions: Vec<usize> =
        requested.match_indices('.').map(|(idx, _)| idx).collect();
    split_positions.reverse();
    for split_idx in split_positions {
        let base_name = &requested[..split_idx];
        let suffix = &requested[split_idx + 1..];
        let base_var = VarName::new(base_name);
        let Some(function) = dae.functions.get(&base_var) else {
            continue;
        };
        if projection_matches_output(function, suffix) {
            return Some(function);
        }
    }

    None
}

fn short_function_name(name: &VarName) -> &str {
    name.as_str().rsplit('.').next().unwrap_or(name.as_str())
}

fn is_runtime_intrinsic_short_name(short: &str) -> bool {
    matches!(
        short,
        "assert"
            | "terminate"
            | "cardinality"
            | "String"
            | "array"
            | "getInstanceName"
            | "fullPathName"
            | "loadResource"
            | "isValidTable"
    )
}

fn is_builtin_or_runtime_special(name: &VarName) -> bool {
    let short = short_function_name(name);
    BuiltinFunction::from_name(short).is_some()
        || BuiltinFunction::from_name(&short.to_ascii_lowercase()).is_some()
        || is_runtime_intrinsic_short_name(short)
        // MLS §6.7.1: Complex is the built-in operator-record constructor.
        || short == "Complex"
        || eval::is_runtime_special_function_name(name.as_str())
}

fn collect_function_parameter_call_aliases(dae: &Dae) -> HashSet<VarName> {
    let mut aliases = HashSet::new();
    for (function_name, function_def) in &dae.functions {
        for param in &function_def.inputs {
            if param.type_name.to_ascii_lowercase().contains("function") {
                aliases.insert(VarName::new(format!(
                    "{}.{}",
                    function_name.as_str(),
                    param.name
                )));
            }
        }
    }
    aliases
}

pub(super) fn validate_sim_function_call_name(
    dae: &Dae,
    name: &VarName,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    if is_builtin_or_runtime_special(name) {
        return Ok(());
    }
    if function_param_aliases.contains(name) {
        return Ok(());
    }

    let Some(func) = resolve_dae_function(dae, name) else {
        return Err(FunctionValidationError {
            name: name.as_str().to_string(),
            reason: "unresolved function call".to_string(),
        });
    };

    if func.external.is_some() && !eval::is_runtime_special_function_name(func.name.as_str()) {
        return Err(FunctionValidationError {
            name: func.name.as_str().to_string(),
            reason: "external function is not supported by this simulator".to_string(),
        });
    }

    if func.external.is_none()
        && func.body.is_empty()
        && !eval::is_runtime_special_function_name(func.name.as_str())
    {
        return Err(FunctionValidationError {
            name: func.name.as_str().to_string(),
            reason: "function has no executable body".to_string(),
        });
    }

    Ok(())
}

pub(super) fn validate_called_function_body(
    dae: &Dae,
    name: &VarName,
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    if !active_stack.insert(name.clone()) {
        // Recursive functions are allowed; stop at the cycle boundary.
        return Ok(());
    }
    if validated_functions.contains(name) {
        active_stack.remove(name);
        return Ok(());
    }

    let Some(func) = resolve_dae_function(dae, name) else {
        active_stack.remove(name);
        return Err(FunctionValidationError {
            name: name.as_str().to_string(),
            reason: "unresolved function call".to_string(),
        });
    };

    for param in func
        .inputs
        .iter()
        .chain(func.outputs.iter())
        .chain(func.locals.iter())
    {
        if let Some(default_expr) = &param.default {
            validate_flat_expr_function_calls(
                dae,
                default_expr,
                validated_functions,
                active_stack,
                function_param_aliases,
            )?;
        }
    }

    for statement in &func.body {
        validate_statement_function_calls(
            dae,
            statement,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?;
    }

    validated_functions.insert(name.clone());
    active_stack.remove(name);
    Ok(())
}

pub(super) fn validate_flat_expr_function_calls(
    dae: &Dae,
    expr: &Expression,
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    if let Some(result) = validate_access_like_expression(
        dae,
        expr,
        validated_functions,
        active_stack,
        function_param_aliases,
    ) {
        return result;
    }

    match expr {
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => {
            validate_nested_function_call(
                dae,
                name,
                args,
                *is_constructor,
                validated_functions,
                active_stack,
                function_param_aliases,
            )?;
        }
        Expression::BuiltinCall { args, .. } => validate_expression_list(
            dae,
            args.iter(),
            validated_functions,
            active_stack,
            function_param_aliases,
        )?,
        Expression::Binary { lhs, rhs, .. } => validate_expression_list(
            dae,
            [lhs.as_ref(), rhs.as_ref()],
            validated_functions,
            active_stack,
            function_param_aliases,
        )?,
        Expression::Unary { rhs, .. } => validate_flat_expr_function_calls(
            dae,
            rhs,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?,
        Expression::If {
            branches,
            else_branch,
        } => validate_if_expression(
            dae,
            branches,
            else_branch,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?,
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            validate_expression_list(
                dae,
                elements.iter(),
                validated_functions,
                active_stack,
                function_param_aliases,
            )?;
        }
        Expression::Range { .. }
        | Expression::Index { .. }
        | Expression::FieldAccess { .. }
        | Expression::ArrayComprehension { .. } => unreachable!(),
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => {}
    }
    Ok(())
}

pub(super) fn validate_access_like_expression(
    dae: &Dae,
    expr: &Expression,
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Option<Result<(), FunctionValidationError>> {
    match expr {
        Expression::Range { start, step, end } => Some(validate_range_expression(
            dae,
            start,
            step.as_deref(),
            end,
            validated_functions,
            active_stack,
            function_param_aliases,
        )),
        Expression::Index { base, subscripts } => Some((|| {
            validate_flat_expr_function_calls(
                dae,
                base,
                validated_functions,
                active_stack,
                function_param_aliases,
            )?;
            validate_index_subscripts(
                dae,
                subscripts,
                validated_functions,
                active_stack,
                function_param_aliases,
            )
        })()),
        Expression::FieldAccess { .. } => Some(validate_field_access_expression(
            dae,
            expr,
            validated_functions,
            active_stack,
            function_param_aliases,
        )),
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => Some(validate_array_comprehension_expression(
            dae,
            expr,
            indices,
            filter.as_deref(),
            validated_functions,
            active_stack,
            function_param_aliases,
        )),
        _ => None,
    }
}

pub(super) fn validate_field_access_expression(
    dae: &Dae,
    expr: &Expression,
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    let Expression::FieldAccess { base, field } = expr else {
        return validate_flat_expr_function_calls(
            dae,
            expr,
            validated_functions,
            active_stack,
            function_param_aliases,
        );
    };

    if let Expression::FunctionCall {
        name,
        args,
        is_constructor: true,
    } = base.as_ref()
    {
        validate_expression_list(
            dae,
            args.iter(),
            validated_functions,
            active_stack,
            function_param_aliases,
        )?;

        if matches!(field.as_str(), "re" | "im") {
            return Ok(());
        }

        let projected_name = format!("{}.{}", name.as_str(), field);
        let Some(constructor) = resolve_dae_function(dae, name) else {
            return Err(FunctionValidationError {
                name: projected_name,
                reason: "constructor field projection requires constructor function definition"
                    .to_string(),
            });
        };

        let field_known = constructor.inputs.iter().any(|param| param.name == *field)
            || constructor.outputs.iter().any(|param| param.name == *field);
        if !field_known {
            return Err(FunctionValidationError {
                name: projected_name,
                reason: "constructor field projection cannot be resolved".to_string(),
            });
        }

        return Ok(());
    }

    validate_flat_expr_function_calls(
        dae,
        base,
        validated_functions,
        active_stack,
        function_param_aliases,
    )
}

pub(super) fn validate_expression_list<'a, I>(
    dae: &Dae,
    exprs: I,
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError>
where
    I: IntoIterator<Item = &'a Expression>,
{
    for expr in exprs {
        validate_flat_expr_function_calls(
            dae,
            expr,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?;
    }
    Ok(())
}

pub(super) fn validate_nested_function_call(
    dae: &Dae,
    name: &VarName,
    args: &[Expression],
    is_constructor: bool,
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    if !is_constructor {
        validate_sim_function_call_name(dae, name, function_param_aliases)?;
        if !is_builtin_or_runtime_special(name) && !function_param_aliases.contains(name) {
            validate_called_function_body(
                dae,
                name,
                validated_functions,
                active_stack,
                function_param_aliases,
            )?;
        }
    }
    validate_expression_list(
        dae,
        args.iter(),
        validated_functions,
        active_stack,
        function_param_aliases,
    )
}

pub(super) fn validate_if_expression(
    dae: &Dae,
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    for (cond, value) in branches {
        validate_expression_list(
            dae,
            [cond, value],
            validated_functions,
            active_stack,
            function_param_aliases,
        )?;
    }
    validate_flat_expr_function_calls(
        dae,
        else_branch,
        validated_functions,
        active_stack,
        function_param_aliases,
    )
}

pub(super) fn validate_range_expression(
    dae: &Dae,
    start: &Expression,
    step: Option<&Expression>,
    end: &Expression,
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    validate_flat_expr_function_calls(
        dae,
        start,
        validated_functions,
        active_stack,
        function_param_aliases,
    )?;
    if let Some(step) = step {
        validate_flat_expr_function_calls(
            dae,
            step,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?;
    }
    validate_flat_expr_function_calls(
        dae,
        end,
        validated_functions,
        active_stack,
        function_param_aliases,
    )
}

pub(super) fn validate_index_subscripts(
    dae: &Dae,
    subscripts: &[Subscript],
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    for subscript in subscripts {
        if let Subscript::Expr(expr) = subscript {
            validate_flat_expr_function_calls(
                dae,
                expr,
                validated_functions,
                active_stack,
                function_param_aliases,
            )?;
        }
    }
    Ok(())
}

pub(super) fn validate_array_comprehension_expression(
    dae: &Dae,
    expr: &Expression,
    indices: &[rumoca_ir_dae::ComprehensionIndex],
    filter: Option<&Expression>,
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    validate_flat_expr_function_calls(
        dae,
        expr,
        validated_functions,
        active_stack,
        function_param_aliases,
    )?;
    for index in indices {
        validate_flat_expr_function_calls(
            dae,
            &index.range,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?;
    }
    if let Some(filter_expr) = filter {
        validate_flat_expr_function_calls(
            dae,
            filter_expr,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?;
    }
    Ok(())
}

pub(super) fn component_ref_to_var_name(comp: &ComponentReference) -> VarName {
    comp.to_var_name()
}

pub(super) fn validate_statement_list(
    dae: &Dae,
    statements: &[Statement],
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    for stmt in statements {
        validate_statement_function_calls(
            dae,
            stmt,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?;
    }
    Ok(())
}

pub(super) fn validate_guarded_statement_block(
    dae: &Dae,
    cond: &Expression,
    statements: &[Statement],
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    validate_flat_expr_function_calls(
        dae,
        cond,
        validated_functions,
        active_stack,
        function_param_aliases,
    )?;
    validate_statement_list(
        dae,
        statements,
        validated_functions,
        active_stack,
        function_param_aliases,
    )
}

pub(super) fn validate_statement_function_call(
    dae: &Dae,
    comp: &ComponentReference,
    args: &[Expression],
    outputs: &[Expression],
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    let name = component_ref_to_var_name(comp);
    let short_name = short_function_name(&name);

    if matches!(short_name, "assert" | "terminate") {
        // Assertion-style calls frequently contain string helper calls in
        // their message arguments. These are non-numeric diagnostics and
        // should not block simulation preflight.
        if let Some(condition) = args.first() {
            validate_flat_expr_function_calls(
                dae,
                condition,
                validated_functions,
                active_stack,
                function_param_aliases,
            )?;
        }
        if args.len() >= 3 {
            validate_flat_expr_function_calls(
                dae,
                &args[2],
                validated_functions,
                active_stack,
                function_param_aliases,
            )?;
        }
        for output in outputs {
            validate_flat_expr_function_calls(
                dae,
                output,
                validated_functions,
                active_stack,
                function_param_aliases,
            )?;
        }
        return Ok(());
    }

    validate_sim_function_call_name(dae, &name, function_param_aliases)?;
    if !is_builtin_or_runtime_special(&name) && !function_param_aliases.contains(&name) {
        validate_called_function_body(
            dae,
            &name,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?;
    }
    for arg in args {
        validate_flat_expr_function_calls(
            dae,
            arg,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?;
    }
    for output in outputs {
        validate_flat_expr_function_calls(
            dae,
            output,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?;
    }
    Ok(())
}

pub(super) fn validate_assert_statement(
    dae: &Dae,
    condition: &Expression,
    level: Option<&Expression>,
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    // Messages are informational and may include unsupported string helper
    // functions that do not affect numeric simulation semantics.
    validate_flat_expr_function_calls(
        dae,
        condition,
        validated_functions,
        active_stack,
        function_param_aliases,
    )?;
    if let Some(level) = level {
        validate_flat_expr_function_calls(
            dae,
            level,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?;
    }
    Ok(())
}

pub(super) fn validate_for_statement(
    dae: &Dae,
    indices: &[ForIndex],
    equations: &[Statement],
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    for index in indices {
        validate_flat_expr_function_calls(
            dae,
            &index.range,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?;
    }
    validate_statement_list(
        dae,
        equations,
        validated_functions,
        active_stack,
        function_param_aliases,
    )
}

pub(super) fn validate_if_statement(
    dae: &Dae,
    cond_blocks: &[StatementBlock],
    else_block: Option<&[Statement]>,
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    for block in cond_blocks {
        validate_guarded_statement_block(
            dae,
            &block.cond,
            &block.stmts,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?;
    }
    if let Some(else_block) = else_block {
        validate_statement_list(
            dae,
            else_block,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?;
    }
    Ok(())
}

pub(super) fn validate_when_statement(
    dae: &Dae,
    blocks: &[StatementBlock],
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    for block in blocks {
        validate_guarded_statement_block(
            dae,
            &block.cond,
            &block.stmts,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?;
    }
    Ok(())
}

pub(super) fn validate_statement_function_calls(
    dae: &Dae,
    stmt: &Statement,
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    match stmt {
        Statement::Assignment { value, .. } => validate_flat_expr_function_calls(
            dae,
            value,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?,
        Statement::For { indices, equations } => validate_for_statement(
            dae,
            indices,
            equations,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?,
        Statement::While(block) => validate_guarded_statement_block(
            dae,
            &block.cond,
            &block.stmts,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?,
        Statement::If {
            cond_blocks,
            else_block,
        } => validate_if_statement(
            dae,
            cond_blocks,
            else_block.as_deref(),
            validated_functions,
            active_stack,
            function_param_aliases,
        )?,
        Statement::When(blocks) => validate_when_statement(
            dae,
            blocks,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?,
        Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => validate_statement_function_call(
            dae,
            comp,
            args,
            outputs,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?,
        Statement::Reinit { value, .. } => validate_flat_expr_function_calls(
            dae,
            value,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?,
        Statement::Assert {
            condition,
            message: _,
            level,
        } => validate_assert_statement(
            dae,
            condition,
            level.as_ref(),
            validated_functions,
            active_stack,
            function_param_aliases,
        )?,
        Statement::Empty | Statement::Return | Statement::Break => {}
    }
    Ok(())
}

pub fn validate_simulation_function_support(dae: &Dae) -> Result<(), FunctionValidationError> {
    let mut validated_functions: HashSet<VarName> = HashSet::new();
    let mut active_stack: HashSet<VarName> = HashSet::new();
    let function_param_aliases = collect_function_parameter_call_aliases(dae);

    for variable in dae
        .states
        .values()
        .chain(dae.algebraics.values())
        .chain(dae.outputs.values())
        .chain(dae.parameters.values())
        .chain(dae.constants.values())
        .chain(dae.inputs.values())
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
            validate_flat_expr_function_calls(
                dae,
                expr,
                &mut validated_functions,
                &mut active_stack,
                &function_param_aliases,
            )?;
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
        validate_flat_expr_function_calls(
            dae,
            &equation.rhs,
            &mut validated_functions,
            &mut active_stack,
            &function_param_aliases,
        )?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn validate_simulation_function_support_allows_complex_constructor_without_body() {
        let mut dae = Dae::default();
        dae.functions.insert(
            VarName::new("Complex"),
            dae::Function::new("Complex", Default::default()),
        );
        dae.outputs.insert(
            VarName::new("y"),
            dae::Variable {
                name: VarName::new("y"),
                start: Some(Expression::FunctionCall {
                    name: VarName::new("Complex"),
                    args: vec![
                        Expression::Literal(dae::Literal::Real(1.0)),
                        Expression::Literal(dae::Literal::Real(2.0)),
                    ],
                    is_constructor: true,
                }),
                ..Default::default()
            },
        );

        validate_simulation_function_support(&dae)
            .expect("Complex constructor should be accepted during function validation");
    }

    #[test]
    fn validate_simulation_function_support_allows_runtime_special_projection_names() {
        let mut dae = Dae::default();
        dae.outputs.insert(
            VarName::new("x"),
            dae::Variable {
                name: VarName::new("x"),
                start: Some(Expression::FunctionCall {
                    name: VarName::new(
                        "Modelica.Math.Random.Generators.Xorshift64star.random.stateOut[1]",
                    ),
                    args: vec![Expression::VarRef {
                        name: VarName::new("state"),
                        subscripts: vec![],
                    }],
                    is_constructor: false,
                }),
                ..Default::default()
            },
        );

        validate_simulation_function_support(&dae).expect(
            "projected runtime-special random outputs should be accepted during function validation",
        );
    }
}
