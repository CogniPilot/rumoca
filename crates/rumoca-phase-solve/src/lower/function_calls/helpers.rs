use super::*;

pub(super) struct ComplexProjectionComprehensionCtx<'a> {
    pub(super) indices: &'a [rumoca_core::ComprehensionIndex],
    pub(super) filter: Option<&'a rumoca_core::Expression>,
    pub(super) field: &'a str,
    pub(super) scope: &'a mut Scope,
    pub(super) const_scope: &'a mut IndexMap<String, f64>,
    pub(super) call_depth: usize,
    pub(super) fallback_span: rumoca_core::Span,
}

pub(super) struct FlattenedRecordInputRequest<'a, 'b> {
    pub(super) input: &'a rumoca_core::FunctionParam,
    pub(super) fields: &'a [rumoca_core::FunctionParam],
    pub(super) positional_args: &'a [&'a rumoca_core::Expression],
    pub(super) positional_idx: &'b mut usize,
    pub(super) caller_scope: &'a Scope,
    pub(super) call_depth: usize,
}

pub(super) struct FlattenedRecordPositionalInputRequest<'a, 'b> {
    pub(super) function_name: &'a str,
    pub(super) input: &'a rumoca_core::FunctionParam,
    pub(super) inputs: &'a [rumoca_core::FunctionParam],
    pub(super) input_idx: usize,
    pub(super) positional_args: &'a [&'a rumoca_core::Expression],
    pub(super) positional_idx: &'b mut usize,
    pub(super) caller_scope: &'a Scope,
    pub(super) call_depth: usize,
}

pub(super) struct NamedOrPositionalArg<'a> {
    pub(super) name: &'a str,
    pub(super) idx: usize,
    pub(super) default: f64,
}

pub(super) fn split_flattened_record_input_name(name: &str) -> Option<(&str, &str)> {
    let (prefix, field) = name.split_once('_')?;
    (!prefix.is_empty() && !field.is_empty()).then_some((prefix, field))
}

pub(super) fn flattened_input_has_prefix(name: &str, prefix: &str) -> bool {
    split_flattened_record_input_name(name).is_some_and(|(candidate, _)| candidate == prefix)
}

pub(super) fn missing_required_function_input<T>(
    function_name: &str,
    input: &rumoca_core::FunctionParam,
) -> Result<T, LowerError> {
    Err(LowerError::MissingActualArgument {
        function: function_name.to_string(),
        what: "required input",
        input: input.name.clone(),
        span: input.span,
    })
}

pub(super) fn missing_intrinsic_argument(
    function_name: &str,
    argument: &'static str,
    span: rumoca_core::Span,
) -> LowerError {
    LowerError::contract_violation(format!("{function_name} requires {argument}"), span)
}

pub(super) fn complex_projection_vec_with_capacity(
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<Reg>, LowerError> {
    crate::lower_vec_with_capacity(capacity, context, span)
}

pub(super) fn append_complex_projection_values(
    values: &mut Vec<Reg>,
    additional: Vec<Reg>,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    values.try_reserve_exact(additional.len()).map_err(|_| {
        LowerError::contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })?;
    values.extend(additional);
    Ok(())
}

pub(super) fn record_constructor_field(
    function_name: &str,
    fields: &[rumoca_core::FunctionParam],
    field_name: &str,
    span: rumoca_core::Span,
) -> Result<rumoca_core::FunctionParam, LowerError> {
    fields
        .iter()
        .find(|field| field.name == field_name)
        .cloned()
        .ok_or_else(|| {
            LowerError::contract_violation(
                format!(
                    "record constructor `{function_name}` does not define field `{field_name}`"
                ),
                span,
            )
        })
}

pub(super) fn validate_complex_component_width(
    function_name: &str,
    input: &rumoca_core::FunctionParam,
    dims: &[i64],
    expected: usize,
    actual: usize,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    if actual == expected {
        return Ok(());
    }
    Err(LowerError::contract_violation(
        format!(
            "function `{function_name}` Complex input `{}` expected {expected} scalar value(s) for shape {}, got {actual}",
            input.name,
            format_i64_dims(dims)
        ),
        span,
    ))
}

pub(super) fn function_input_actual_dim(
    function_name: &str,
    input: &rumoca_core::FunctionParam,
    declared: i64,
    actual: usize,
    span: rumoca_core::Span,
) -> Result<i64, LowerError> {
    let actual = i64::try_from(actual).map_err(|_| {
        LowerError::contract_violation(
            format!(
                "function `{function_name}` input `{}` actual dimension {actual} exceeds supported range",
                input.name
            ),
            span,
        )
    })?;
    if declared == 0 || declared == actual {
        return Ok(actual);
    }
    let shape = format_i64_dims(&input.dims);
    Err(LowerError::contract_violation(
        format!(
            "function `{function_name}` input `{}` expected dimension {declared} in declared shape {shape}, got {actual}",
            input.name
        ),
        span,
    ))
}

pub(super) fn checked_usize_dims_to_i64(
    dims: &[usize],
    context: &str,
    span: rumoca_core::Span,
) -> Result<Vec<i64>, LowerError> {
    dims.iter()
        .copied()
        .map(|dim| {
            i64::try_from(dim).map_err(|_| {
                LowerError::contract_violation(
                    format!("{context} dimension {dim} exceeds i64 range"),
                    span,
                )
            })
        })
        .collect()
}
