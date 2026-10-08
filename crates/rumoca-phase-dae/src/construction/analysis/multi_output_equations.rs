use super::*;

/// Typed owner for one MLS §12.4.3 multi-result equation.
pub(in crate::construction) struct MultiOutputEquationPlan {
    /// One receiving slot per function result ordinal; `None` is an omitted
    /// receiver in the source tuple, or a whole record that `records` owns.
    pub(in crate::construction) outputs: Vec<Option<VarName>>,
    /// The whole records that receive one record-valued result: each leaf
    /// coordinate reads its field projection of the result.
    pub(in crate::construction) records: Vec<MultiOutputRecord>,
}

/// One whole record receiving the record-valued result at `ordinal`.
pub(in crate::construction) struct MultiOutputRecord {
    pub(in crate::construction) ordinal: usize,
    pub(in crate::construction) plan: RecordEquationPlan,
}

pub(super) fn analyze_multi_output_equations(
    flat: &flat::Model,
    equations: &[flat::Equation],
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    shapes: &FunctionShapeAnalysis,
    initialization: bool,
) -> Result<HashMap<usize, MultiOutputEquationPlan>, ToDaeError> {
    let mut plans = HashMap::new();
    for (row, equation) in equations.iter().enumerate() {
        let Some(source) = multi_output_equation(equation) else {
            continue;
        };
        plans.insert(
            row,
            validate_multi_output_equation(flat, roles, states, shapes, source, initialization)?,
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
    initialization: bool,
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
    let mut records = Vec::new();
    let mut claimed = HashSet::new();
    let receiver_context = ReceiverValidation {
        flat,
        roles,
        function,
        call_prefix: &call.prefix,
        equation_span: source.span,
        initialization,
    };
    for (ordinal, (receiver, result_shape)) in source
        .receivers
        .iter()
        .zip(&certificate.results)
        .enumerate()
    {
        if let Some(record) = record_receiver(flat, receiver) {
            let plan = validate_record_receiver(
                receiver_context,
                record,
                (result_shape, ordinal),
                &mut claimed,
            )?;
            records.push(MultiOutputRecord { ordinal, plan });
            outputs.push(None);
            continue;
        }
        outputs.push(validate_receiver(
            receiver_context,
            receiver,
            result_shape,
            ordinal,
            &mut claimed,
        )?);
    }
    if outputs.iter().all(Option::is_none) && records.is_empty() {
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
    Ok(MultiOutputEquationPlan { outputs, records })
}

#[derive(Clone, Copy)]
struct ReceiverValidation<'scope> {
    flat: &'scope flat::Model,
    roles: &'scope HashMap<VarName, PlannedRole>,
    function: &'scope rumoca_core::Function,
    call_prefix: &'scope [u32],
    equation_span: Span,
    initialization: bool,
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
    require_owned_receiver(context, &target, *span)?;
    validate_receiver_type_and_shape(context, receiver, result_shape, ordinal, &target, *span)?;
    Ok(Some(target))
}

/// A receiver is a coordinate this equation may own: continuous, discrete (a
/// discrete receiver is defined by its result ordinal the same way a
/// continuous one is; its owner is the discrete system, MLS Appendix B), or an
/// initial parameter of an initialization equation.
fn require_owned_receiver(
    context: ReceiverValidation<'_>,
    target: &VarName,
    span: Span,
) -> Result<(), ToDaeError> {
    let role = context.roles.get(target);
    let continuous = matches!(
        role,
        Some(PlannedRole::State | PlannedRole::Algebraic | PlannedRole::Output)
    );
    let discrete = !context.initialization
        && matches!(
            role,
            Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
        );
    let initial_parameter = context.initialization && matches!(role, Some(PlannedRole::Parameter));
    if continuous || discrete || initial_parameter {
        return Ok(());
    }
    Err(ToDaeError::unsupported_flat(
        "multi-output equation",
        format!(
            "receiving variable `{target}` is not a coordinate owned by this {} equation",
            if context.initialization {
                "initial"
            } else {
                "continuous"
            }
        ),
        span,
    ))
}

/// A receiving slot that names a whole record: a variable reference that
/// names no Flat variable.
struct RecordReceiver<'flat> {
    record: &'flat flat::RecordInstance,
    expression: &'flat Expression,
    name: &'flat rumoca_core::Reference,
    subscripts: &'flat [rumoca_core::Subscript],
    span: Span,
}

/// The whole record a receiving slot names, when it names no Flat variable.
fn record_receiver<'flat>(
    flat: &'flat flat::Model,
    receiver: &'flat Expression,
) -> Option<RecordReceiver<'flat>> {
    let Expression::VarRef {
        name,
        subscripts,
        span,
    } = receiver
    else {
        return None;
    };
    let record = (!flat.variables.contains_key(name.var_name()))
        .then(|| flat.record_instances.get(name.var_name()))
        .flatten()?;
    Some(RecordReceiver {
        record,
        expression: receiver,
        name,
        subscripts,
        span: *span,
    })
}

/// A whole record receiving one record-valued result: every leaf coordinate
/// of the record is an owned receiver and reads its field projection of the
/// result.
fn validate_record_receiver(
    context: ReceiverValidation<'_>,
    receiver: RecordReceiver<'_>,
    (result_shape, ordinal): (&[u32], usize),
    claimed: &mut HashSet<VarName>,
) -> Result<RecordEquationPlan, ToDaeError> {
    let RecordReceiver {
        record,
        expression,
        name,
        subscripts,
        span,
    } = receiver;
    if !subscripts.is_empty() || !context.call_prefix.is_empty() || !result_shape.is_empty() {
        return Err(invalid_receiver(
            expression,
            context.equation_span,
            "must receive one whole scalar record result",
        ));
    }
    let fields = record_result_fields(
        context.flat,
        record,
        name.var_name(),
        &context.function.outputs[ordinal],
        span,
    )?;
    for field in &fields {
        if !claimed.insert(field.target.clone()) {
            return Err(ToDaeError::unsupported_flat(
                "multi-output equation",
                format!(
                    "receiving variable `{}` occurs more than once",
                    field.target
                ),
                span,
            ));
        }
        require_owned_receiver(context, &field.target, span)?;
    }
    Ok(RecordEquationPlan { fields })
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
