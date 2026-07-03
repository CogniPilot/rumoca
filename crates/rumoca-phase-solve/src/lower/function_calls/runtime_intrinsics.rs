use super::*;
use rumoca_ir_solve::RandomGenerator;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::lower) enum ExternalTableIntrinsicKind {
    Bounds {
        table: ExternalTableRecordKind,
        upper: bool,
    },
    Lookup {
        table: ExternalTableRecordKind,
    },
    NextEvent {
        table: ExternalTableRecordKind,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::lower) enum ExternalTableRecordKind {
    CombiTimeTable,
    CombiTable1D,
}

impl ExternalTableRecordKind {
    pub(in crate::lower) fn constructor_name(self) -> &'static str {
        match self {
            Self::CombiTimeTable => "Modelica.Blocks.Types.ExternalCombiTimeTable",
            Self::CombiTable1D => "Modelica.Blocks.Types.ExternalCombiTable1D",
        }
    }

    pub(in crate::lower) fn flattened_field_count(self) -> usize {
        self.flattened_field_names().len()
    }

    pub(in crate::lower) fn flattened_field_names(self) -> &'static [&'static str] {
        match self {
            Self::CombiTimeTable => &[
                "tableName",
                "fileName",
                "table",
                "startTime",
                "columns",
                "smoothness",
                "extrapolation",
                "shiftTime",
                "timeEvents",
                "verboseRead",
                "delimiter",
                "nHeaderLines",
            ],
            Self::CombiTable1D => &[
                "tableName",
                "fileName",
                "table",
                "columns",
                "smoothness",
                "extrapolation",
                "verboseRead",
                "delimiter",
                "nHeaderLines",
            ],
        }
    }
}

pub(in crate::lower) fn external_table_intrinsic_kind(
    call_name: &str,
) -> Option<ExternalTableIntrinsicKind> {
    match intrinsic_short_name(call_name) {
        "getTimeTableTmin" => Some(ExternalTableIntrinsicKind::Bounds {
            table: ExternalTableRecordKind::CombiTimeTable,
            upper: false,
        }),
        "getTable1DAbscissaUmin" => Some(ExternalTableIntrinsicKind::Bounds {
            table: ExternalTableRecordKind::CombiTable1D,
            upper: false,
        }),
        "getTimeTableTmax" => Some(ExternalTableIntrinsicKind::Bounds {
            table: ExternalTableRecordKind::CombiTimeTable,
            upper: true,
        }),
        "getTable1DAbscissaUmax" => Some(ExternalTableIntrinsicKind::Bounds {
            table: ExternalTableRecordKind::CombiTable1D,
            upper: true,
        }),
        "getTimeTableValueNoDer" | "getTimeTableValueNoDer2" | "getTimeTableValue" => {
            Some(ExternalTableIntrinsicKind::Lookup {
                table: ExternalTableRecordKind::CombiTimeTable,
            })
        }
        "getTable1DValueNoDer" | "getTable1DValueNoDer2" | "getTable1DValue" => {
            Some(ExternalTableIntrinsicKind::Lookup {
                table: ExternalTableRecordKind::CombiTable1D,
            })
        }
        "getNextTimeEvent" => Some(ExternalTableIntrinsicKind::NextEvent {
            table: ExternalTableRecordKind::CombiTimeTable,
        }),
        _ => None,
    }
}

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
        } => positive_integer_real_equals_usize(*value, dim),
        _ => false,
    })
}

fn positive_integer_real_equals_usize(value: f64, expected: usize) -> bool {
    if !value.is_finite() || value.fract() != 0.0 || value <= 0.0 || value >= usize::MAX as f64 {
        return false;
    }
    value == expected as f64
}

pub(in crate::lower) fn random_projection_state_len(
    generator: RandomGenerator,
    projection: &FunctionOutputProjection,
    args: &[rumoca_core::Expression],
) -> Result<usize, LowerError> {
    let declared_len = match random_state_len_arg(args, projection.span)? {
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
    owner_span: rumoca_core::Span,
) -> Result<Option<usize>, LowerError> {
    let Some(expr) = args.get(2) else {
        return Ok(None);
    };
    let span = expr
        .span()
        .or_else(|| (!owner_span.is_dummy()).then_some(owner_span))
        .ok_or_else(|| LowerError::UnspannedContractViolation {
            reason: "random state length argument requires source provenance".to_string(),
        })?;
    let rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        ..
    } = expr
    else {
        return Err(unsupported_at(
            "random state length argument must be an Integer literal",
            span,
        ));
    };
    let len = usize::try_from((*value).max(1)).map_err(|_| {
        unsupported_at(
            "random state length argument is outside the supported range",
            span,
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
    rumoca_core::qualified_type_name_matches(&param.type_name, "Complex")
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
    let (base, field) = crate::path_utils::scope_split(call_name)?;
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
        ) => find_string_special_value(
            intrinsic == rumoca_core::ModelicaStringIntrinsic::FindLast,
            required_literal_string(args.first(), call_span)?,
            required_literal_string(args.get(1), call_span)?,
            call_span,
        )
        .map(Some),
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
    span: rumoca_core::Span,
) -> Result<f64, LowerError> {
    let idx = if find_last {
        haystack.rfind(needle)
    } else {
        haystack.find(needle)
    };
    idx.map(|i| {
        i.checked_add(1).map(|index| index as f64).ok_or_else(|| {
            LowerError::contract_violation("Modelica string find result index overflows", span)
        })
    })
    .transpose()
    .map(|idx| idx.unwrap_or(0.0))
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
    if checked_dim_product(inferred_dims) != Some(1) || values.len() != 1 {
        return false;
    }
    scope.insert(generated_scope_key(name), values[0]);
    true
}

fn checked_dim_product(dims: &[usize]) -> Option<usize> {
    dims.iter()
        .try_fold(1usize, |total, dim| total.checked_mul(*dim))
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

#[cfg(test)]
mod tests {
    use super::*;

    fn unspanned_runtime_intrinsics_test_span() -> rumoca_core::Span {
        rumoca_core::Span::DUMMY
    }

    #[test]
    fn find_string_special_value_uses_modelica_one_based_index() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("runtime_intrinsics_string_find.mo"),
            3,
            20,
        );
        let value = find_string_special_value(false, "abcdef", "cd", span)
            .expect("literal string find should evaluate");

        assert_eq!(value, 3.0);
    }

    #[test]
    fn find_string_special_value_returns_zero_for_missing_match() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("runtime_intrinsics_string_find_last.mo"),
            5,
            26,
        );
        let value = find_string_special_value(true, "abcdef", "xy", span)
            .expect("literal string findLast should evaluate");

        assert_eq!(value, 0.0);
    }

    #[test]
    fn positive_integer_real_equals_usize_rejects_host_overflow() {
        assert!(!positive_integer_real_equals_usize(usize::MAX as f64, 1));
    }

    #[test]
    fn positive_integer_real_equals_usize_matches_expected_dimension() {
        assert!(positive_integer_real_equals_usize(2.0, 2));
        assert!(!positive_integer_real_equals_usize(2.0, 3));
    }

    #[test]
    fn random_state_len_arg_rejects_unspanned_non_literal() {
        let args = vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: unspanned_runtime_intrinsics_test_span(),
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                span: unspanned_runtime_intrinsics_test_span(),
            },
            rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("n").into(),
                subscripts: Vec::new(),
                span: unspanned_runtime_intrinsics_test_span(),
            },
        ];

        let err = random_state_len_arg(&args, unspanned_runtime_intrinsics_test_span())
            .expect_err("unspanned random state length must fail");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason()
                .contains("random state length argument requires source provenance")
        );
    }

    #[test]
    fn random_state_len_arg_uses_owner_span_for_non_literal() {
        let owner_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_function_calls_runtime_intrinsics_source_901.mo",
            ),
            4,
            19,
        );
        let args = vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: owner_span,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                span: owner_span,
            },
            rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("n").into(),
                subscripts: Vec::new(),
                span: unspanned_runtime_intrinsics_test_span(),
            },
        ];

        let err = random_state_len_arg(&args, owner_span)
            .expect_err("non-literal random state length must fail");

        assert_eq!(err.source_span(), Some(owner_span));
        assert!(err.reason().contains("must be an Integer literal"));
    }

    #[test]
    fn bind_singleton_array_actual_declines_overflowing_dims() {
        let mut scope = Scope::new();

        assert!(!bind_singleton_array_actual_to_scalar_formal(
            &mut scope,
            "x",
            &[usize::MAX, 2],
            &[0],
        ));
        assert!(scope.get(&generated_scope_key("x")).is_none());
    }
}
