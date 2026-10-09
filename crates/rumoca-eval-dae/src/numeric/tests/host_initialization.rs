use super::*;
use crate::{
    InputInitializationPolicy as Policy, NumericEvaluationErrorKind as ErrorKind, NumericInitialRun,
};

fn input_model(start: Option<f64>, binding: Option<f64>, width: u32) -> Dae {
    input_model_with_start(start, binding, width, false)
}

fn input_model_with_start(
    start: Option<f64>,
    binding: Option<f64>,
    width: u32,
    exponential_start: bool,
) -> Dae {
    let text = if exponential_start {
        "input Real u[2,width](each start=exp(1000));"
    } else {
        "input Real u[2,width](each start=start)=fill(binding,2,width);"
    };
    let mut sources = SourceMap::new();
    let source = sources.add("HostInput.mo", text);
    let at = DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    Dae::construct(sources, |model| {
        let ty = model.types(|t| t.derived(ValueType::array(ScalarType::Real, [2, width]), at))?;
        let start = start
            .map(|value| {
                let value = model.expressions(|e| e.at(at).literal(DaeLiteral::Real(value)))?;
                if exponential_start {
                    model.expressions(|e| e.at(at).builtin(PureBuiltin::Exp, [value]))
                } else {
                    Ok(value)
                }
            })
            .transpose()?;
        let binding = if let Some(value) = binding {
            Some(model.expressions(|e| {
                let value = e.at(at).literal(DaeLiteral::Real(value))?;
                let two = e.at(at).literal(DaeLiteral::Integer(2))?;
                let width = e.at(at).literal(DaeLiteral::Integer(i64::from(width)))?;
                e.at(at).builtin(PureBuiltin::Fill, [value, two, width])
            })?)
        } else {
            None
        };
        model.variables(|v| {
            v.input(
                VarName::new("u"),
                ty,
                rumoca_ir_dae::InputVariability::Continuous,
                at,
                rumoca_ir_dae::VariableAttributes {
                    start,
                    binding,
                    causality: rumoca_ir_dae::VariableCausality::Input,
                    declared_causality: rumoca_ir_dae::DeclaredCausality::Input,
                    ..Default::default()
                },
            )
            .map(|_| ())
        })
    })
    .unwrap()
}

#[test]
fn host_input_policy_preserves_source_repeat_and_strict_missing_driver_refusal() {
    assert_eq!(Policy::default(), Policy::RequireRuntimeDriver);
    let model = input_model(Some(-0.0), None, 100_000);
    model.inspect(|view| {
        let input = view.variable_id(0).unwrap();
        assert_eq!(NumericEvaluator::new(view).initial_values(input).unwrap_err().kind(), ErrorKind::MissingValue);
        let values = NumericEvaluator::with_input_policy(view, |_,_| None, Policy::HostDrivenStart).initial_values(input).unwrap();
        assert_eq!(values.len(), 200_000);
        assert_eq!(values.runs().count(), 1);
        assert!(matches!(values.runs().next(), Some(NumericInitialRun::Repeat { value,count }) if count==200_000 && value.to_bits()==(-0.0f64).to_bits()));
    });
}

#[test]
fn host_input_policy_keeps_binding_precedence_and_partial_override_partitions() {
    for binding in [None, Some(2.5)] {
        let model = input_model(Some(-0.0), binding, 3);
        model.inspect(|view| {
            let input = view.variable_id(0).unwrap();
            let values = NumericEvaluator::with_input_policy(
                view,
                |_, i| (i == 2).then_some(7.0),
                Policy::HostDrivenStart,
            )
            .initial_values(input)
            .unwrap();
            let expected = binding.unwrap_or(-0.0);
            assert_eq!(values.runs().count(), 3);
            assert_overridden_values(&values, expected);
            if binding.is_some() {
                assert_eq!(
                    NumericEvaluator::new(view)
                        .initial_values(input)
                        .unwrap()
                        .materialize(),
                    vec![2.5; 6]
                );
            }
        });
    }
}

#[test]
fn host_input_policy_refuses_nonfinite_computed_source_starts() {
    let model = input_model_with_start(Some(1000.0), None, 3, true);
    model.inspect(|view| {
        let input = view.variable_id(0).unwrap();
        assert_eq!(
            NumericEvaluator::with_input_policy(view, |_, _| None, Policy::HostDrivenStart)
                .initial_values(input)
                .unwrap_err()
                .kind(),
            ErrorKind::InvalidValue
        );
    });
}

#[test]
fn host_input_policy_refuses_missing_source_and_nonfinite_actual_overrides() {
    for policy in [Policy::RequireRuntimeDriver, Policy::HostDrivenStart] {
        for start in [None, Some(0.25)] {
            let model = input_model(start, None, 3);
            model.inspect(|view| {
                assert_missing_and_nonfinite(view, policy, start.is_some());
            });
        }
    }
}

fn assert_overridden_values(values: &crate::NumericInitialValues, expected: f64) {
    for (i, value) in values.materialize().into_iter().enumerate() {
        assert_eq!(
            value.to_bits(),
            if i == 2 {
                7.0f64.to_bits()
            } else {
                expected.to_bits()
            }
        );
    }
}

fn assert_missing_and_nonfinite(view: rumoca_ir_dae::DaeView<'_>, policy: Policy, has_start: bool) {
    let input = view.variable_id(0).unwrap();
    if !has_start {
        assert_eq!(
            NumericEvaluator::with_input_policy(view, |_, _| None, policy)
                .initial_values(input)
                .unwrap_err()
                .kind(),
            ErrorKind::MissingValue
        );
        assert_eq!(
            NumericEvaluator::with_input_policy(view, |_, _| Some(3.0), policy)
                .initial_values(input)
                .unwrap()
                .materialize(),
            vec![3.0; 6]
        );
    }
    for value in [
        f64::INFINITY,
        f64::NEG_INFINITY,
        f64::from_bits(0x7ff8_0000_0000_1234),
    ] {
        assert_eq!(
            NumericEvaluator::with_input_policy(view, |_, _| Some(value), policy)
                .initial_values(input)
                .unwrap_err()
                .kind(),
            ErrorKind::InvalidOverride
        );
    }
}
