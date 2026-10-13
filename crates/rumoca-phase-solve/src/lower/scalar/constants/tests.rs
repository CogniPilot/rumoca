use super::*;
use rumoca_core::{SourceMap, TypeId, VarName};

#[derive(Clone, Copy)]
enum Probe {
    Scalar,
    Dot,
    Indexed,
    LargeInteger,
    Tunable,
    FalseAndTunable,
    TrueOrTunable,
    TunableConditional,
}

fn model(probe: Probe) -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "translation scalar guard";
    let id = sources.add("guard.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(id, 0, text.len())).unwrap();
    dae::Dae::construct(sources, |model| {
        let literal = model
            .expressions(|expressions| expressions.at(at).literal(dae::DaeLiteral::Integer(1)))?;
        let integer = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Integer),
                at,
            )
        })?;
        let parameter = model.variables(|variables| {
            variables.parameter(
                VarName::new("p"),
                integer,
                at,
                dae::VariableAttributes {
                    binding: Some(literal),
                    is_tunable: true,
                    ..Default::default()
                },
            )
        })?;
        model.expressions(|expressions| {
            let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
            let zero = expressions.at(at).literal(dae::DaeLiteral::Integer(0))?;
            let lhs = match probe {
                Probe::Scalar => one,
                Probe::Dot => {
                    let minus = expressions.at(at).literal(dae::DaeLiteral::Integer(-1))?;
                    let a = expressions.at(at).array([one, one])?;
                    let b = expressions.at(at).array([one, minus])?;
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Multiply, a, b)?
                }
                Probe::Indexed => {
                    let a = expressions.at(at).array([one, zero])?;
                    expressions.at(at).index(
                        a,
                        [dae::Subscript::Index {
                            expression: one,
                            provenance: at,
                        }],
                    )?
                }
                Probe::LargeInteger => expressions
                    .at(at)
                    .literal(dae::DaeLiteral::Integer((1_i64 << 53) + 1))?,
                Probe::Tunable
                | Probe::FalseAndTunable
                | Probe::TrueOrTunable
                | Probe::TunableConditional => expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::Parameter(parameter))?,
            };
            let comparison = expressions
                .at(at)
                .binary(dae::BinaryOperator::Equal, lhs, zero)?;
            if matches!(
                probe,
                Probe::FalseAndTunable | Probe::TrueOrTunable | Probe::TunableConditional
            ) {
                let yes = expressions.at(at).literal(dae::DaeLiteral::Boolean(true))?;
                let no = expressions
                    .at(at)
                    .literal(dae::DaeLiteral::Boolean(false))?;
                let (condition, selected, fallback) = match probe {
                    Probe::FalseAndTunable => (no, comparison, no),
                    Probe::TrueOrTunable => (yes, yes, comparison),
                    _ => (comparison, yes, no),
                };
                expressions
                    .at(at)
                    .conditional([(condition, selected)], fallback)?;
            }
            Ok(())
        })
    })
    .unwrap()
}

fn result(probe: Probe) -> Option<bool> {
    model(probe).inspect(|view| {
        let guard = view.expression_id(view.expression_count() - 1).unwrap();
        ScalarSelector::new(view, None)
            .translation_guard(guard)
            .unwrap()
    })
}

#[test]
fn translation_scalar_guard_is_exact_and_rejects_other_projection_rules() {
    assert_eq!(result(Probe::Scalar), Some(false));
    // The canonical dot is zero, not its first scalar product. This probe
    // declines it rather than manufacturing a false branch selection.
    assert_eq!(result(Probe::Dot), None);
    assert_eq!(result(Probe::Indexed), None);
    assert_eq!(result(Probe::LargeInteger), None);
    assert_eq!(result(Probe::Tunable), None);
    assert_eq!(result(Probe::FalseAndTunable), Some(false));
    assert_eq!(result(Probe::TrueOrTunable), Some(true));
    assert_eq!(result(Probe::TunableConditional), None);
}

#[test]
fn constant_real_power_probe_keeps_the_exact_integer_exponent() {
    for exponent in [
        9_007_199_254_740_993,
        -9_007_199_254_740_993,
        i64::MIN,
        i64::MAX,
    ] {
        let text = format!("(-1.0)^({exponent})");
        let mut sources = SourceMap::new();
        let source = sources.add("power_probe.mo", &text);
        let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
        let model = dae::Dae::construct(sources, |model| {
            model.expressions(|expressions| {
                let base = expressions.at(at).literal(dae::DaeLiteral::Real(-1.0))?;
                let exponent = expressions
                    .at(at)
                    .literal(dae::DaeLiteral::Integer(exponent))?;
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Power, base, exponent)?;
                Ok(())
            })
        })
        .unwrap();
        let actual = model.inspect(|view| {
            let power = view.expression_id(view.expression_count() - 1).unwrap();
            assert_eq!(
                view.expression(power).unwrap().value_type().scalar_type(),
                dae::ScalarType::Real
            );
            ScalarSelector::new(view, None)
                .constant_real(power, 0)
                .unwrap()
        });
        assert_eq!(
            actual,
            if exponent % 2 == 0 { 1.0 } else { -1.0 },
            "exact exponent {exponent}"
        );
    }
}

#[test]
fn constant_integer_dot_power_probe_computes_the_contraction_or_declines() {
    let text = "(-1.0)^({1,1}*{1,1})";
    let mut sources = SourceMap::new();
    let source = sources.add("dot_power_probe.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        model.expressions(|expressions| {
            let base = expressions.at(at).literal(dae::DaeLiteral::Real(-1.0))?;
            let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
            let vector = expressions.at(at).array([one, one])?;
            let dot = expressions
                .at(at)
                .binary(dae::BinaryOperator::Multiply, vector, vector)?;
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Power, base, dot)?;
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let power = view.expression_id(view.expression_count() - 1).unwrap();
        let dae::ExpressionOperation::Binary { rhs: dot, .. } =
            view.expression(power).unwrap().operation()
        else {
            panic!("source power remains a checked binary expression");
        };
        let dot_type = view.expression(dot).unwrap().value_type();
        assert_eq!(dot_type.scalar_type(), dae::ScalarType::Integer);
        assert!(
            dot_type.is_scalar(),
            "vector multiplication is a contraction"
        );
        let selector = ScalarSelector::new(view, None);
        match selector.constant_real(power, 0) {
            Ok(actual) => assert_eq!(actual, 1.0, "the Integer dot-product exponent is 2"),
            Err(LowerError::NonComputable { span, .. }) => assert_eq!(span, at.span()),
            Err(error) => panic!("unexpected constant probe failure: {error:?}"),
        }
        match selector.integer(dot, 0) {
            Ok(actual) => assert_eq!(actual, 2, "the exact selector must contract both leaves"),
            Err(LowerError::NonComputable { span, .. }) => assert_eq!(span, at.span()),
            Err(error) => panic!("unexpected Integer selector failure: {error:?}"),
        }
        match selector.constant_real(dot, 0) {
            Ok(actual) => assert_eq!(
                actual, 2.0,
                "the coefficient probe must contract both leaves"
            ),
            Err(LowerError::NonComputable { span, .. }) => assert_eq!(span, at.span()),
            Err(error) => panic!("unexpected coefficient probe failure: {error:?}"),
        }
    });
}

#[test]
fn constant_integer_matrix_power_probe_computes_matrix_product_or_declines() {
    let text = "{{1,1},{1,1}}^2";
    let mut sources = SourceMap::new();
    let source = sources.add("matrix_power_probe.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        model.expressions(|expressions| {
            let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
            let row = expressions.at(at).array([one, one])?;
            let matrix = expressions.at(at).array([row, row])?;
            let two = expressions.at(at).literal(dae::DaeLiteral::Integer(2))?;
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Power, matrix, two)?;
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let power = view.expression_id(view.expression_count() - 1).unwrap();
        let result_type = view.expression(power).unwrap().value_type();
        assert_eq!(result_type.scalar_type(), dae::ScalarType::Integer);
        assert_eq!(result_type.dimensions(), &[2, 2]);
        let selector = ScalarSelector::new(view, None);
        for scalar in 0..4 {
            match selector.constant_real(power, scalar) {
                Ok(actual) => assert_eq!(actual, 2.0, "matrix product leaf {scalar}"),
                Err(LowerError::NonComputable { span, .. }) => assert_eq!(span, at.span()),
                Err(error) => panic!("unexpected matrix coefficient failure: {error:?}"),
            }
            match selector.integer(power, scalar) {
                Ok(actual) => assert_eq!(actual, 2, "exact matrix product leaf {scalar}"),
                Err(LowerError::NonComputable { span, .. }) => assert_eq!(span, at.span()),
                Err(error) => panic!("unexpected matrix Integer selector failure: {error:?}"),
            }
        }
    });
}
