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
) -> usize {
    projection
        .indices
        .first()
        .copied()
        .unwrap_or(0)
        .max(random_state_len_arg(args).unwrap_or_else(|| random_generator_state_len(generator)))
}

pub(in crate::lower) fn random_state_len_arg(args: &[rumoca_core::Expression]) -> Option<usize> {
    let rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: rumoca_core::Span::DUMMY,
    } = args.get(2)?
    else {
        return None;
    };
    usize::try_from((*value).max(1)).ok()
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
) -> Option<f64> {
    match intrinsic_short_name(call_name) {
        "getInstanceName" | "fullPathName" | "loadResource" | "readLine" | "substring"
        | "scanBoolean" | "scanDelimiter" | "scanIdentifier" | "scanInteger" | "scanNoToken"
        | "scanReal" | "scanString" | "scanToken" | "skipWhiteSpace" => Some(0.0),
        "isValidTable" => Some(1.0),
        "writeRealMatrix" => Some(1.0),
        "isEmpty" => Some(
            literal_string(args.first())
                .map_or(0.0, |s| if s.trim().is_empty() { 1.0 } else { 0.0 }),
        ),
        "hashString" => Some(literal_string(args.first()).map_or(0.0, |value| {
            rumoca_eval_dae::modelica_strings_hash_string(value) as f64
        })),
        "length" => literal_string(args.first()).map(|s| s.chars().count() as f64),
        "find" | "findLast" => Some(find_string_special_value(
            intrinsic_short_name(call_name),
            literal_string(args.first()),
            literal_string(args.get(1)),
        )),
        _ => None,
    }
}

pub(in crate::lower) fn literal_string(expr: Option<&rumoca_core::Expression>) -> Option<&str> {
    match expr {
        Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String(value),
            span: rumoca_core::Span::DUMMY,
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
    short_name: &str,
    haystack: Option<&str>,
    needle: Option<&str>,
) -> f64 {
    let (Some(haystack), Some(needle)) = (haystack, needle) else {
        return 0.0;
    };
    let idx = match short_name {
        "find" => haystack.find(needle),
        "findLast" => haystack.rfind(needle),
        _ => None,
    };
    idx.map(|i| i.saturating_add(1) as f64).unwrap_or(0.0)
}

pub(in crate::lower) fn exact_dim_value_count(dims: &[i64]) -> usize {
    dims.iter()
        .try_fold(1usize, |acc, dim| {
            usize::try_from(*dim)
                .ok()
                .and_then(|dim| acc.checked_mul(dim))
        })
        .unwrap_or_else(|| dims_scalar_count(dims))
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
    scope.insert(ComponentPath::from_flat_path(name), values[0]);
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
