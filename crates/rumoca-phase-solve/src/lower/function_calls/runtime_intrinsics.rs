use super::*;
use rumoca_ir_solve::RandomGenerator;

#[derive(Debug, Clone, Copy)]
pub(in crate::lower) enum RandomIntrinsicKind {
    InitialState,
    Random,
}

pub(in crate::lower) fn random_intrinsic_kind(
    name: &str,
) -> Option<(RandomGenerator, RandomIntrinsicKind)> {
    match name {
        "Modelica.Math.Random.Utilities.initialStateWithXorshift64star"
        | "Modelica.Math.Random.Generators.Xorshift64star.initialState" => Some((
            RandomGenerator::Xorshift64Star,
            RandomIntrinsicKind::InitialState,
        )),
        "Modelica.Math.Random.Generators.Xorshift128plus.initialState" => Some((
            RandomGenerator::Xorshift128Plus,
            RandomIntrinsicKind::InitialState,
        )),
        "Modelica.Math.Random.Generators.Xorshift1024star.initialState" => Some((
            RandomGenerator::Xorshift1024Star,
            RandomIntrinsicKind::InitialState,
        )),
        "Modelica.Math.Random.Generators.Xorshift64star.random" => {
            Some((RandomGenerator::Xorshift64Star, RandomIntrinsicKind::Random))
        }
        "Modelica.Math.Random.Generators.Xorshift128plus.random" => Some((
            RandomGenerator::Xorshift128Plus,
            RandomIntrinsicKind::Random,
        )),
        "Modelica.Math.Random.Generators.Xorshift1024star.random" => Some((
            RandomGenerator::Xorshift1024Star,
            RandomIntrinsicKind::Random,
        )),
        _ => None,
    }
}

pub(in crate::lower) fn next_positional_function_input_arg<'a>(
    input: &rumoca_core::FunctionParam,
    positional_args: &[&'a rumoca_core::Expression],
    positional_idx: &mut usize,
) -> Option<&'a rumoca_core::Expression> {
    let arg = *positional_args.get(*positional_idx)?;
    *positional_idx += 1;
    if !input.dims.is_empty() {
        skip_array_size_actuals(arg, input.dims.len(), positional_args, positional_idx);
    }
    Some(arg)
}

pub(in crate::lower) fn skip_array_size_actuals(
    array_arg: &rumoca_core::Expression,
    rank: usize,
    positional_args: &[&rumoca_core::Expression],
    positional_idx: &mut usize,
) {
    for dim in 1..=rank {
        let Some(candidate) = positional_args.get(*positional_idx) else {
            return;
        };
        if !is_size_actual_for_array_dim(candidate, array_arg, dim) {
            return;
        }
        *positional_idx += 1;
    }
}

pub(in crate::lower) fn is_size_actual_for_array_dim(
    candidate: &rumoca_core::Expression,
    array_arg: &rumoca_core::Expression,
    dim: usize,
) -> bool {
    let rumoca_core::Expression::BuiltinCall { function, args, .. } = candidate else {
        return false;
    };
    if *function != rumoca_core::BuiltinFunction::Size {
        return false;
    }
    let Some(base) = args.first() else {
        return false;
    };
    if base != array_arg {
        return false;
    }
    args.get(1).is_some_and(|expr| match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => usize::try_from(*value).ok() == Some(dim),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        } => value.is_finite() && value.fract() == 0.0 && *value > 0.0 && *value as usize == dim,
        _ => false,
    })
}

pub(in crate::lower) fn random_projection_state_len(
    generator: RandomGenerator,
    projection: &FunctionOutputProjection,
    args: &[rumoca_core::Expression],
) -> Result<usize, LowerError> {
    let declared_len = match random_state_len_arg(args)? {
        Some(len) => len,
        None => random_generator_state_len(generator),
    };
    Ok(match projection.indices.first().copied() {
        Some(projected_idx) => projected_idx.max(declared_len),
        None => declared_len,
    })
}

pub(in crate::lower) fn random_state_len_arg(
    args: &[rumoca_core::Expression],
) -> Result<Option<usize>, LowerError> {
    let Some(expr) = args.get(2) else {
        return Ok(None);
    };
    let rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        ..
    } = expr
    else {
        return Err(unsupported_at(
            "random state length argument must be an Integer literal",
            expr.span().unwrap_or(rumoca_core::Span::DUMMY),
        ));
    };
    let len = usize::try_from((*value).max(1)).map_err(|_| {
        unsupported_at(
            "random state length argument is outside the supported range",
            expr.span().unwrap_or(rumoca_core::Span::DUMMY),
        )
    })?;
    Ok(Some(len))
}

pub(in crate::lower) fn random_generator_state_len(generator: RandomGenerator) -> usize {
    match generator {
        RandomGenerator::Xorshift64Star => 2,
        RandomGenerator::Xorshift128Plus => 4,
        RandomGenerator::Xorshift1024Star => 33,
    }
}

pub(in crate::lower) fn is_complex_param(param: &rumoca_core::FunctionParam) -> bool {
    rumoca_core::top_level_last_segment(&param.type_name) == "Complex"
}

pub(in crate::lower) fn parse_complex_sum_projection_field(call_name: &str) -> Option<&str> {
    let suffix = call_name.strip_prefix("Modelica.ComplexMath.sum.")?;
    match suffix {
        "re" | "im" => Some(suffix),
        "result.re" => Some("re"),
        "result.im" => Some("im"),
        _ => None,
    }
}

pub(in crate::lower) fn parse_complex_operator_projection(
    call_name: &str,
) -> Option<(BinaryOp, &str)> {
    let (base, field) = rumoca_core::split_last_top_level(call_name)?;
    let op = match base {
        "Complex.'+'" => BinaryOp::Add,
        "Complex.'-'" => BinaryOp::Sub,
        "Complex.'*'" => BinaryOp::Mul,
        "Complex.'/'" => BinaryOp::Div,
        _ => return None,
    };
    if matches!(field, "re" | "im") {
        Some((op, field))
    } else {
        None
    }
}

pub(in crate::lower) fn lower_runtime_string_special_value(
    call_name: &str,
    args: &[rumoca_core::Expression],
    call_span: rumoca_core::Span,
) -> Result<Option<f64>, LowerError> {
    let short_name = intrinsic_short_name(call_name);
    match rumoca_core::modelica_string_intrinsic_short_name(short_name) {
        Some(rumoca_core::ModelicaStringIntrinsic::RequiresLowering) => Err(unsupported_at(
            format!(
                "{} must be lowered to a typed literal or runtime operation before solve lowering",
                short_name
            ),
            call_span,
        )),
        Some(rumoca_core::ModelicaStringIntrinsic::IsEmpty) => Ok(Some(
            if required_literal_string(args.first(), call_span)?
                .trim()
                .is_empty()
            {
                1.0
            } else {
                0.0
            },
        )),
        Some(rumoca_core::ModelicaStringIntrinsic::HashString) => Ok(Some(
            rumoca_eval_dae::modelica_strings_hash_string(required_literal_string(
                args.first(),
                call_span,
            )?) as f64,
        )),
        Some(rumoca_core::ModelicaStringIntrinsic::Length) => Ok(Some(
            required_literal_string(args.first(), call_span)?
                .chars()
                .count() as f64,
        )),
        Some(
            intrinsic @ (rumoca_core::ModelicaStringIntrinsic::Find
            | rumoca_core::ModelicaStringIntrinsic::FindLast),
        ) => Ok(Some(find_string_special_value(
            intrinsic == rumoca_core::ModelicaStringIntrinsic::FindLast,
            required_literal_string(args.first(), call_span)?,
            required_literal_string(args.get(1), call_span)?,
        ))),
        None => match short_name {
            "isValidTable" => Ok(Some(1.0)),
            "writeRealMatrix" => Ok(Some(1.0)),
            _ => Ok(None),
        },
    }
}

fn required_literal_string(
    expr: Option<&rumoca_core::Expression>,
    call_span: rumoca_core::Span,
) -> Result<&str, LowerError> {
    literal_string(expr).ok_or_else(|| {
        let span = expr
            .and_then(rumoca_core::Expression::span)
            .unwrap_or(call_span);
        unsupported_at("string intrinsic requires a literal String argument", span)
    })
}

pub(in crate::lower) fn literal_string(expr: Option<&rumoca_core::Expression>) -> Option<&str> {
    match expr {
        Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String(value),
            ..
        }) => Some(value.as_str()),
        _ => None,
    }
}

pub(in crate::lower) fn non_numeric_record_metadata(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String(_),
            ..
        }
    )
}

pub(in crate::lower) fn find_string_special_value(
    find_last: bool,
    haystack: &str,
    needle: &str,
) -> f64 {
    let idx = if find_last {
        haystack.rfind(needle)
    } else {
        haystack.find(needle)
    };
    idx.map(|i| i.saturating_add(1) as f64).unwrap_or(0.0)
}

pub(in crate::lower) fn exact_dim_value_count(
    dims: &[i64],
    context: impl Into<String>,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    dims_scalar_count(dims, context, span)
}

pub(in crate::lower) fn bind_singleton_array_actual_to_scalar_formal(
    scope: &mut Scope,
    name: &str,
    inferred_dims: &[usize],
    values: &[Reg],
) -> bool {
    if inferred_dims.iter().product::<usize>() != 1 || values.len() != 1 {
        return false;
    }
    scope.insert(generated_scope_key(name), values[0]);
    true
}

pub(in crate::lower) fn decode_named_function_arg(
    expr: &rumoca_core::Expression,
) -> Option<(&str, &rumoca_core::Expression)> {
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor: _,
        span: _,
    } = expr
    else {
        return None;
    };
    let named = name.as_str().strip_prefix(NAMED_FUNCTION_ARG_PREFIX)?;
    let value = args.first()?;
    Some((named, value))
}

pub(in crate::lower) fn split_named_and_positional_call_args<'a>(
    function_name: &str,
    args: &'a [rumoca_core::Expression],
) -> Result<
    (
        IndexMap<String, &'a rumoca_core::Expression>,
        Vec<&'a rumoca_core::Expression>,
    ),
    LowerError,
> {
    let mut named_args = IndexMap::new();
    let mut positional_args = Vec::new();

    for arg in args {
        if let Some((name, value_expr)) = decode_named_function_arg(arg) {
            if named_args.insert(name.to_string(), value_expr).is_some() {
                return Err(LowerError::InvalidFunction {
                    name: function_name.to_string(),
                    reason: format!("named argument slot `{name}` filled more than once"),
                });
            }
        } else {
            positional_args.push(arg);
        }
    }

    Ok((named_args, positional_args))
}
