use std::collections::HashSet;

use rumoca_eval_dae as eval;
use rumoca_ir_dae as dae;

use crate::lower::NAMED_FUNCTION_ARG_PREFIX;
use crate::projection_suffix::{
    OutputProjectionSuffix, output_projection_suffix, record_output_field_param,
    resolve_function_reference,
};

type BuiltinFunction = rumoca_core::BuiltinFunction;
type ComponentReference = rumoca_core::ComponentReference;
type Dae = dae::Dae;
type Expression = rumoca_core::Expression;
type ForIndex = rumoca_core::ForIndex;
type Statement = rumoca_core::Statement;
type StatementBlock = rumoca_core::StatementBlock;
type Subscript = rumoca_core::Subscript;
type VarName = rumoca_core::VarName;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionValidationError {
    pub name: String,
    pub reason: String,
}

fn resolve_dae_function_by_key<'a>(
    dae: &'a Dae,
    requested: &VarName,
) -> Option<&'a rumoca_core::Function> {
    dae.symbols.functions.get(requested)
}

fn projection_matches_output(
    functions: &indexmap::IndexMap<VarName, rumoca_core::Function>,
    function: &rumoca_core::Function,
    projection_suffix: OutputProjectionSuffix,
) -> bool {
    let Some(output) = function
        .outputs
        .iter()
        .find(|out| out.name == projection_suffix.output_name)
    else {
        return false;
    };

    let projected_output = match projection_suffix.output_fields.as_slice() {
        [field] => {
            match record_output_field_param(functions, output, &projection_suffix.output_fields) {
                Some(field_output) => field_output,
                None if output_is_complex_record(output)
                    && matches!(field.as_str(), "re" | "im") =>
                {
                    output
                }
                None => return false,
            }
        }
        [] => output,
        _ => match record_output_field_param(functions, output, &projection_suffix.output_fields) {
            Some(field_output) => field_output,
            None => return false,
        },
    };

    let indices = projection_suffix.indices;
    if projected_output.dims.is_empty() {
        return indices.is_empty();
    }

    if indices.is_empty() {
        return true;
    }

    if projected_output.dims.iter().any(|dim| *dim < 0)
        || (!projected_output.shape_expr.is_empty()
            && projected_output.dims.iter().any(|dim| *dim <= 0))
    {
        return true;
    }

    let Some(total) = projected_output.dims.iter().try_fold(1usize, |acc, dim| {
        usize::try_from(*dim)
            .ok()
            .and_then(|dim| acc.checked_mul(dim))
    }) else {
        return false;
    };

    if indices.len() == 1 {
        let idx = indices[0];
        return idx >= 1 && idx <= total;
    }

    if indices.len() != projected_output.dims.len() {
        return false;
    }

    indices
        .iter()
        .zip(projected_output.dims.iter())
        .all(|(idx, dim)| dimension_index_in_bounds(*idx, *dim))
}

fn resolve_projected_dae_function<'a>(
    dae: &'a Dae,
    name: &rumoca_core::Reference,
) -> Option<&'a rumoca_core::Function> {
    let (_, function) = resolve_function_reference(&dae.symbols.functions, name)?;
    if let Some(resolved) = name.resolved_function() {
        if name.component_ref()?.parts.len() == resolved.base_part_count {
            return Some(function);
        }
    } else {
        return None;
    }
    let suffix = output_projection_suffix(function, name)?;
    projection_matches_output(&dae.symbols.functions, function, suffix).then_some(function)
}

fn dimension_index_in_bounds(index: usize, dim: i64) -> bool {
    let Ok(dim) = usize::try_from(dim) else {
        return false;
    };
    index >= 1 && index <= dim
}

fn resolve_dae_function<'a>(
    dae: &'a Dae,
    name: &rumoca_core::Reference,
) -> Option<&'a rumoca_core::Function> {
    resolve_projected_dae_function(dae, name)
}

fn resolve_dae_component_function<'a>(
    dae: &'a Dae,
    comp: &ComponentReference,
) -> Option<&'a rumoca_core::Function> {
    resolve_projected_dae_function(
        dae,
        &rumoca_core::Reference::from_component_reference(comp.clone()),
    )
}

fn is_runtime_intrinsic_short_name(short: &str) -> bool {
    short == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME
        || eval::is_runtime_special_function_short_name(short)
}

fn is_builtin_or_runtime_special_key(name: &VarName) -> bool {
    let short = name.last_segment();
    BuiltinFunction::from_name(short).is_some()
        || BuiltinFunction::from_name(&short.to_ascii_lowercase()).is_some()
        || is_runtime_intrinsic_short_name(short)
        // MLS §6.7.1: Complex is the built-in operator-record constructor.
        || short == "Complex"
        || eval::is_runtime_special_function_name(name)
}

fn is_builtin_or_runtime_special(name: &rumoca_core::Reference) -> bool {
    is_builtin_or_runtime_special_key(name.var_name())
}

pub(super) fn collect_function_parameter_call_aliases(dae: &Dae) -> HashSet<VarName> {
    let mut aliases = HashSet::new();
    for (function_name, function_def) in &dae.symbols.functions {
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
    name: &rumoca_core::Reference,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    if is_builtin_or_runtime_special(name) {
        return Ok(());
    }
    if function_param_aliases.contains(name.var_name()) {
        return Ok(());
    }

    let Some(func) = resolve_dae_function(dae, name) else {
        return Err(FunctionValidationError {
            name: name.as_str().to_string(),
            reason: "unresolved function call".to_string(),
        });
    };

    if func.external.is_some() && !eval::is_runtime_special_function_name(&func.name) {
        return Err(FunctionValidationError {
            name: func.name.as_str().to_string(),
            reason: "external function is not supported by this simulator".to_string(),
        });
    }

    if func.external.is_none()
        && func.body.is_empty()
        && !eval::is_runtime_special_function_name(&func.name)
    {
        return Err(FunctionValidationError {
            name: func.name.as_str().to_string(),
            reason: "function has no executable body".to_string(),
        });
    }

    Ok(())
}

#[cfg(test)]
mod dynamic_projection_tests {
    use super::*;

    fn fixture_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("dynamic_projection_fixture.mo"),
            1,
            2,
        )
    }

    fn function_with_dynamic_array_output() -> rumoca_core::Function {
        let span = fixture_span();
        let mut function = rumoca_core::Function::new("Pkg.f", span);
        function.outputs.push(
            rumoca_core::FunctionParam::new("y", "Real", span)
                .with_dims(vec![0])
                .with_shape_expr(vec![rumoca_core::Subscript::Expr {
                    expr: Box::new(rumoca_core::Expression::VarRef {
                        name: VarName::new("n").into(),
                        subscripts: vec![],
                        span,
                    }),
                    span,
                }]),
        );
        function.body.push(rumoca_core::Statement::Return { span });
        function.instance_id = Some(rumoca_core::FunctionInstanceId::new(700));
        function
    }

    #[test]
    fn validation_accepts_dynamic_array_output_projection() {
        let mut dae = Dae::default();
        dae.symbols
            .functions
            .insert(VarName::new("Pkg.f"), function_with_dynamic_array_output());
        let aliases = HashSet::new();

        let projected = |name: &str| {
            rumoca_core::Reference::from_component_reference(
                rumoca_core::component_reference_from_flat_name(
                    &VarName::new(name),
                    fixture_span(),
                )
                .expect("structured projected function reference"),
            )
            .with_resolved_function(rumoca_core::ResolvedFunctionReference {
                instance_id: rumoca_core::FunctionInstanceId::new(700),
                base_part_count: 2,
            })
        };
        validate_sim_function_call_name(&dae, &projected("Pkg.f.y"), &aliases)
            .expect("aggregate dynamic output projection should validate");
        validate_sim_function_call_name(&dae, &projected("Pkg.f.y[1]"), &aliases)
            .expect("indexed dynamic output projection should validate");
    }
}

pub(super) fn validate_sim_component_function_call_name(
    dae: &Dae,
    comp: &ComponentReference,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    let name = comp.to_var_name();
    if is_builtin_or_runtime_special_key(&name) {
        return Ok(());
    }
    if function_param_aliases.contains(&name) {
        return Ok(());
    }

    let Some(func) = resolve_dae_component_function(dae, comp) else {
        return Err(FunctionValidationError {
            name: name.as_str().to_string(),
            reason: "unresolved function call".to_string(),
        });
    };

    if func.external.is_some() && !eval::is_runtime_special_function_name(&func.name) {
        return Err(FunctionValidationError {
            name: func.name.as_str().to_string(),
            reason: "external function is not supported by this simulator".to_string(),
        });
    }

    if func.external.is_none()
        && func.body.is_empty()
        && !eval::is_runtime_special_function_name(&func.name)
    {
        return Err(FunctionValidationError {
            name: func.name.as_str().to_string(),
            reason: "function has no executable body".to_string(),
        });
    }

    Ok(())
}

pub(super) fn is_named_function_arg_marker(name: &rumoca_core::Reference) -> bool {
    name.as_str().starts_with(NAMED_FUNCTION_ARG_PREFIX)
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

    let Some(func) = resolve_dae_function_by_key(dae, name) else {
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
            ..
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
            ..
        } => validate_if_expression(
            dae,
            branches,
            else_branch,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?,
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
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
        | Expression::ArrayComprehension { .. } => {
            return Err(FunctionValidationError {
                name: "<expression>".to_string(),
                reason: "access-like expression reached function-call validation dispatcher"
                    .to_string(),
            });
        }
        Expression::VarRef { .. }
        | Expression::Literal { value: _, .. }
        | Expression::Empty { .. } => {}
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
        Expression::Range {
            start, step, end, ..
        } => Some(validate_range_expression(
            dae,
            start,
            step.as_deref(),
            end,
            validated_functions,
            active_stack,
            function_param_aliases,
        )),
        Expression::Index {
            base, subscripts, ..
        } => Some((|| {
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
            ..
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
    let Expression::FieldAccess { base, field, .. } = expr else {
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
        ..
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
    name: &rumoca_core::Reference,
    args: &[Expression],
    is_constructor: bool,
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    if is_named_function_arg_marker(name) {
        return validate_expression_list(
            dae,
            args.iter(),
            validated_functions,
            active_stack,
            function_param_aliases,
        );
    }

    if !is_constructor {
        validate_sim_function_call_name(dae, name, function_param_aliases)?;
        if !is_builtin_or_runtime_special(name) && !function_param_aliases.contains(name.var_name())
        {
            let name = name.var_name();
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
        if let Subscript::Expr { expr, .. } = subscript {
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
    indices: &[rumoca_core::ComprehensionIndex],
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
    _outputs: &[ComponentReference],
    validated_functions: &mut HashSet<VarName>,
    active_stack: &mut HashSet<VarName>,
    function_param_aliases: &HashSet<VarName>,
) -> Result<(), FunctionValidationError> {
    let name = comp.to_var_name();
    let short_name = comp.last_ident().ok_or_else(|| FunctionValidationError {
        name: name.to_string(),
        reason: "function call has no callee identifier".to_string(),
    })?;

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
        return Ok(());
    }

    validate_sim_component_function_call_name(dae, comp, function_param_aliases)?;
    if !is_builtin_or_runtime_special_key(&name) && !function_param_aliases.contains(&name) {
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
        Statement::For {
            indices, equations, ..
        } => validate_for_statement(
            dae,
            indices,
            equations,
            validated_functions,
            active_stack,
            function_param_aliases,
        )?,
        Statement::While { block, .. } => validate_guarded_statement_block(
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
            ..
        } => validate_if_statement(
            dae,
            cond_blocks,
            else_block.as_deref(),
            validated_functions,
            active_stack,
            function_param_aliases,
        )?,
        Statement::When { blocks, .. } => validate_when_statement(
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
            ..
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
            ..
        } => validate_assert_statement(
            dae,
            condition,
            level.as_deref(),
            validated_functions,
            active_stack,
            function_param_aliases,
        )?,
        Statement::Empty { .. } | Statement::Return { .. } | Statement::Break { .. } => {}
    }
    Ok(())
}

pub fn validate_simulation_function_support(dae: &Dae) -> Result<(), FunctionValidationError> {
    let mut validated_functions: HashSet<VarName> = HashSet::new();
    let mut active_stack: HashSet<VarName> = HashSet::new();
    let function_param_aliases = collect_function_parameter_call_aliases(dae);

    for variable in dae
        .variables
        .states
        .values()
        .chain(dae.variables.algebraics.values())
        .chain(dae.variables.outputs.values())
        .chain(dae.variables.parameters.values())
        .chain(dae.variables.constants.values())
        .chain(dae.variables.inputs.values())
        .chain(dae.variables.discrete_reals.values())
        .chain(dae.variables.discrete_valued.values())
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
        .continuous
        .equations
        .iter()
        .chain(dae.discrete.real_updates.iter())
        .chain(dae.discrete.valued_updates.iter())
        .chain(dae.conditions.equations.iter())
        .chain(dae.initialization.equations.iter())
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

fn output_is_complex_record(output: &rumoca_core::FunctionParam) -> bool {
    rumoca_core::qualified_type_name_matches(&output.type_name, "Complex")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("function_validation_fixture.mo"),
            1,
            2,
        )
    }

    #[test]
    fn validate_simulation_function_support_allows_complex_constructor_without_body() {
        let span = fixture_span();
        let mut dae = Dae::default();
        dae.symbols.functions.insert(
            VarName::new("Complex"),
            rumoca_core::Function::new("Complex", span),
        );
        dae.variables.outputs.insert(
            VarName::new("y"),
            dae::Variable {
                name: rumoca_core::VarName::new("y"),
                start: Some(Expression::FunctionCall {
                    name: rumoca_core::VarName::new("Complex").into(),
                    args: vec![
                        Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span,
                        },
                        Expression::Literal {
                            value: rumoca_core::Literal::Real(2.0),
                            span,
                        },
                    ],
                    is_constructor: true,
                    span,
                }),
                ..rumoca_ir_dae::Variable::empty_with_span(span)
            },
        );

        validate_simulation_function_support(&dae)
            .expect("Complex constructor should be accepted during function validation");
    }

    #[test]
    fn validate_field_access_rejects_unknown_spanned_constructor_field() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_function_validation_source_81.mo"),
            3,
            17,
        );
        let mut dae = Dae::default();
        let mut constructor = rumoca_core::Function::new("Pkg.Record", span);
        constructor.instance_id = Some(rumoca_core::FunctionInstanceId::new(701));
        constructor
            .inputs
            .push(rumoca_core::FunctionParam::new("known", "Real", span));
        dae.symbols
            .functions
            .insert(VarName::new("Pkg.Record"), constructor);
        let expr = Expression::FieldAccess {
            base: Box::new(Expression::FunctionCall {
                name: rumoca_core::Reference::from_component_reference(
                    rumoca_core::component_reference_from_flat_name(
                        &VarName::new("Pkg.Record"),
                        span,
                    )
                    .expect("structured constructor reference"),
                )
                .with_resolved_function(rumoca_core::ResolvedFunctionReference {
                    instance_id: rumoca_core::FunctionInstanceId::new(701),
                    base_part_count: 2,
                }),
                args: Vec::new(),
                is_constructor: true,
                span,
            }),
            field: "missing".to_string(),
            span,
        };
        let aliases = HashSet::new();
        let mut validated_functions = HashSet::new();
        let mut active_stack = HashSet::new();

        let err = validate_field_access_expression(
            &dae,
            &expr,
            &mut validated_functions,
            &mut active_stack,
            &aliases,
        )
        .expect_err("unknown constructor field should fail validation");

        assert_eq!(err.name, "Pkg.Record.missing");
        assert_eq!(
            err.reason,
            "constructor field projection cannot be resolved"
        );
    }

    #[test]
    fn validate_simulation_function_support_allows_runtime_special_projection_names() {
        let span = fixture_span();
        let mut dae = Dae::default();
        dae.variables.outputs.insert(
            VarName::new("x"),
            dae::Variable {
                name: rumoca_core::VarName::new("x"),
                start: Some(Expression::Index {
                    base: Box::new(Expression::FunctionCall {
                        name: rumoca_core::VarName::new(
                            "Modelica.Math.Random.Generators.Xorshift64star.random.stateOut",
                        )
                        .into(),
                        args: vec![Expression::VarRef {
                            name: rumoca_core::VarName::new("state").into(),
                            subscripts: vec![],
                            span,
                        }],
                        is_constructor: false,
                        span,
                    }),
                    subscripts: vec![rumoca_core::Subscript::generated_index(1, span)],
                    span,
                }),
                ..rumoca_ir_dae::Variable::empty_with_span(span)
            },
        );

        validate_simulation_function_support(&dae).expect(
            "projected runtime-special random outputs should be accepted during function validation",
        );
    }

    #[test]
    fn validate_simulation_function_support_allows_named_argument_markers() {
        let span = fixture_span();
        let dae = Dae::default();
        let aliases = HashSet::new();
        let mut validated_functions = HashSet::new();
        let mut active_stack = HashSet::new();

        validate_nested_function_call(
            &dae,
            &rumoca_core::Reference::from("__rumoca_named_arg__.id"),
            &[Expression::Literal {
                value: rumoca_core::Literal::Integer(7),
                span,
            }],
            false,
            &mut validated_functions,
            &mut active_stack,
            &aliases,
        )
        .expect("named-argument markers are call metadata, not runtime functions");
    }

    #[test]
    fn dimension_index_in_bounds_rejects_out_of_range_index() {
        assert!(!dimension_index_in_bounds(3, 2));
    }
}
