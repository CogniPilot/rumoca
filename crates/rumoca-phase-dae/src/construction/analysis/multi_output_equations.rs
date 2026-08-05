use super::*;

/// Typed owner for one MLS §12.4.3 multi-result equation.
pub(in crate::construction) struct MultiOutputEquationPlan {
    /// One receiving slot per function result ordinal; `None` is an omitted
    /// receiver in the source tuple.
    pub(in crate::construction) outputs: Vec<Option<VarName>>,
}

pub(super) fn analyze_multi_output_equations(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    shapes: &FunctionShapeAnalysis,
) -> Result<HashMap<usize, MultiOutputEquationPlan>, ToDaeError> {
    let mut plans = HashMap::new();
    for (row, equation) in flat.equations.iter().enumerate() {
        let Some(source) = multi_output_equation(equation) else {
            continue;
        };
        plans.insert(
            row,
            validate_multi_output_equation(flat, roles, states, shapes, source)?,
        );
    }
    Ok(plans)
}

struct MultiOutputEquationSource<'source> {
    receivers: &'source [Expression],
    function: &'source rumoca_core::Reference,
    arguments: &'source [Expression],
    span: Span,
}

fn multi_output_equation(equation: &flat::Equation) -> Option<MultiOutputEquationSource<'_>> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &equation.residual
    else {
        return None;
    };
    let Expression::Tuple { elements, .. } = lhs.as_ref() else {
        return None;
    };
    let Expression::FunctionCall {
        name,
        args,
        is_constructor: false,
        ..
    } = rhs.as_ref()
    else {
        return None;
    };
    Some(MultiOutputEquationSource {
        receivers: elements,
        function: name,
        arguments: args,
        span: equation.span,
    })
}

fn validate_multi_output_equation(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    shapes: &FunctionShapeAnalysis,
    source: MultiOutputEquationSource<'_>,
) -> Result<MultiOutputEquationPlan, ToDaeError> {
    require_span(source.span, "multi-output equation")?;
    let call = shapes.call_certificate(
        source.function,
        source.arguments,
        shapes.model_values(),
        source.span,
    )?;
    let certificate = shapes
        .certificate(&call.specialization)
        .expect("a call-shape certificate names a function certificate");
    if source.receivers.len() != certificate.results.len() {
        return Err(ToDaeError::unsupported_flat(
            "multi-output equation",
            format!(
                "`{}` returns {} values but the receiving tuple has {} slots",
                source.function.as_str(),
                certificate.results.len(),
                source.receivers.len()
            ),
            source.span,
        ));
    }
    let function = &flat.functions[&certificate.key.function];
    // MLS §12.4.3 evaluates the call once. The canonical DAE currently owns
    // one ordinal projection per retained result, which can repeat evaluation;
    // that representation is equivalent only for a side-effect-free body.
    if !function.body_is_pure() {
        return Err(ToDaeError::unsupported_flat(
            "multi-output equation",
            format!(
                "MLS §12.4.3 evaluates a multi-result call once, but the canonical DAE reads \
                 each result as its own call: `{}` is impure, so repeated evaluation would not \
                 preserve the source equation",
                source.function.as_str()
            ),
            source.span,
        ));
    }
    let mut outputs = Vec::with_capacity(source.receivers.len());
    let mut claimed = HashSet::new();
    let receiver_context = ReceiverValidation {
        flat,
        roles,
        function,
        call_prefix: &call.prefix,
        equation_span: source.span,
    };
    for (ordinal, (receiver, result_shape)) in source
        .receivers
        .iter()
        .zip(&certificate.results)
        .enumerate()
    {
        outputs.push(validate_receiver(
            receiver_context,
            receiver,
            result_shape,
            ordinal,
            &mut claimed,
        )?);
    }
    if outputs.iter().all(Option::is_none) {
        return Err(ToDaeError::unsupported_flat(
            "multi-output equation",
            "a receiving tuple must retain at least one function result",
            source.span,
        ));
    }
    for argument in source.arguments {
        validate_model_expression_with_record_array_fields(
            argument,
            roles,
            states,
            shapes.record_array_fields(),
            shapes.model_values(),
        )?;
    }
    Ok(MultiOutputEquationPlan { outputs })
}

#[derive(Clone, Copy)]
struct ReceiverValidation<'scope> {
    flat: &'scope flat::Model,
    roles: &'scope HashMap<VarName, PlannedRole>,
    function: &'scope rumoca_core::Function,
    call_prefix: &'scope [u32],
    equation_span: Span,
}

fn validate_receiver(
    context: ReceiverValidation<'_>,
    receiver: &Expression,
    result_shape: &[u32],
    ordinal: usize,
    claimed: &mut HashSet<VarName>,
) -> Result<Option<VarName>, ToDaeError> {
    if matches!(receiver, Expression::Empty { .. }) {
        return Ok(None);
    }
    let Expression::VarRef {
        name,
        subscripts,
        span,
        ..
    } = receiver
    else {
        return Err(invalid_receiver(
            receiver,
            context.equation_span,
            "must be a direct variable reference",
        ));
    };
    if !subscripts.is_empty() {
        return Err(invalid_receiver(
            receiver,
            context.equation_span,
            "must receive one whole function result",
        ));
    }
    let target = name.var_name().clone();
    if !claimed.insert(target.clone()) {
        return Err(ToDaeError::unsupported_flat(
            "multi-output equation",
            format!("receiving variable `{target}` occurs more than once"),
            *span,
        ));
    }
    if !matches!(
        context.roles.get(&target),
        Some(PlannedRole::State | PlannedRole::Algebraic | PlannedRole::Output)
    ) {
        return Err(ToDaeError::unsupported_flat(
            "multi-output equation",
            format!("receiving variable `{target}` is not a continuous equation coordinate"),
            *span,
        ));
    }
    validate_receiver_type_and_shape(context, receiver, result_shape, ordinal, &target, *span)?;
    Ok(Some(target))
}

fn validate_receiver_type_and_shape(
    context: ReceiverValidation<'_>,
    receiver: &Expression,
    result_shape: &[u32],
    ordinal: usize,
    target: &VarName,
    span: Span,
) -> Result<(), ToDaeError> {
    let variable = &context.flat.variables[target];
    let declared_shape = variable
        .dims
        .iter()
        .map(|extent| u32::try_from(*extent))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_| {
            invalid_receiver(
                receiver,
                context.equation_span,
                "has a non-concrete declared shape",
            )
        })?;
    let actual_shape = context
        .call_prefix
        .iter()
        .copied()
        .chain(result_shape.iter().copied())
        .collect::<Vec<_>>();
    if declared_shape != actual_shape {
        return Err(ToDaeError::unsupported_flat(
            "multi-output equation",
            format!(
                "receiver `{target}` has shape {declared_shape:?}, but result {} has shape {actual_shape:?}",
                ordinal + 1
            ),
            span,
        ));
    }
    let result = &context.function.outputs[ordinal];
    if effective_variable_scalar_type(context.flat, variable)
        != effective_function_scalar_type(context.flat, result)
    {
        return Err(ToDaeError::unsupported_flat(
            "multi-output equation",
            format!(
                "receiver `{target}` does not have the scalar type of result {}",
                ordinal + 1
            ),
            span,
        ));
    }
    Ok(())
}

fn invalid_receiver(expression: &Expression, fallback: Span, detail: &str) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "multi-output equation",
        format!("a receiving tuple slot {detail}"),
        expression_span(expression).unwrap_or(fallback),
    )
}
