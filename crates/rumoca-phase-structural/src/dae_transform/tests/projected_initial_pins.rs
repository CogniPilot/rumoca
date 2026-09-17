use super::*;

#[derive(Clone, Copy, PartialEq)]
enum PinProfile {
    Distinct,
    FixedTarget,
    Dynamic,
    Nonlinear,
}

#[test]
fn component_pins_keep_distinct_indices_and_constant_offsets() {
    let model = component_pin_model(PinProfile::Distinct);
    model.inspect(|view| {
        let pins = super::super::initial_pins::transferred_initial_values(view).unwrap();
        assert_eq!(pins.len(), 2);
        for (pin, (source, scalar, expected)) in pins.iter().zip([(1, 0, 1.0), (2, 1, -8.0)]) {
            assert_eq!(
                (pin.source, pin.coordinate, pin.scalar, pin.role),
                (source, 0, scalar, InitialValueRole::Definition)
            );
            assert_eq!(pin_numeric_value(view, pin), expected);
        }
    });
}

fn pin_numeric_value(view: dae::DaeView<'_>, pin: &InitialValuePin) -> f64 {
    let mut evaluator = rumoca_eval_dae::NumericEvaluator::new(view);
    pin.value
        .iter()
        .map(|term| {
            let value = evaluator
                .expression(view.expression_id(term.expression as usize).unwrap())
                .unwrap()[term.scalar as usize];
            if term.negated { -value } else { value }
        })
        .sum()
}

#[test]
fn a_fixed_array_component_is_checked_and_never_overwritten() {
    let model = component_pin_model(PinProfile::FixedTarget);
    model.inspect(|view| {
        let pins = super::super::initial_pins::transferred_initial_values(view).unwrap();
        assert!(pins.iter().all(|p| p.role == InitialValueRole::Check));
        assert!(
            pins.iter()
                .any(|p| p.source == 1 && p.coordinate == 0 && p.scalar == 0)
        );
    });
}

#[test]
fn dynamic_indices_and_nonlinear_reads_cannot_prove_value_aliases() {
    for profile in [PinProfile::Dynamic, PinProfile::Nonlinear] {
        let model = component_pin_model(profile);
        model.inspect(|view| {
            let pins = super::super::initial_pins::transferred_initial_values(view).unwrap();
            let pin = pins.iter().find(|p| p.source == 1).unwrap();
            assert_eq!((pin.coordinate, pin.role), (1, InitialValueRole::Check));
        });
    }
}

#[test]
fn fixed_output_survives_state_demotion_onto_an_array_element() {
    for opposite in [false, true] {
        let counts = [
            (&[1][..], &[1][..], 0),
            (&[3][..], &[2][..], 1),
            (&[4096][..], &[2049][..], 2048),
            (&[2, 3][..], &[2, 2][..], 4),
        ]
        .map(|(shape, indices, scalar)| {
            let model = projected_pin_model(shape, indices, opposite);
            assert_projected_pin(&model, scalar, opposite)
        });
        assert_eq!(
            counts[1], counts[2],
            "projection proof must not enumerate a tensor basis"
        );
    }
}

fn assert_projected_pin(model: &dae::Dae, scalar: u32, opposite: bool) -> usize {
    let prepared = prepare_for_solve(model).expect("retain the exact projected initial equation");
    prepared.inspect(|system| {
        let states = system
            .view
            .variables()
            .filter(|(_, v)| v.role() == dae::VariableRole::State)
            .map(|(id, _)| id.index())
            .collect::<Vec<_>>();
        assert_eq!(states, [0]);
        let pin = system
            .pins
            .iter()
            .find(|p| p.source == 2)
            .expect("fixed output pin");
        assert_eq!(
            (pin.coordinate, pin.scalar, pin.role),
            (0, scalar, InitialValueRole::Definition)
        );
        assert_eq!(pin.value.len(), 1);
        assert_eq!(pin.value[0].negated, opposite);
        let value = system
            .view
            .expression_id(pin.value[0].expression as usize)
            .unwrap();
        assert_eq!(constraints::numeric_literal(system.view, value), Some(1.0));
        // The four original owners plus the reconstructed derivative definition.
        assert_eq!(system.view.continuous_owners().count(), 5);
        system.view.expression_count()
    })
}

fn projected_pin_model(shape: &[u32], indices: &[i64], opposite: bool) -> dae::Dae {
    let axes = shape
        .iter()
        .map(u32::to_string)
        .collect::<Vec<_>>()
        .join(",");
    let subscripts = indices
        .iter()
        .map(i64::to_string)
        .collect::<Vec<_>>()
        .join(",");
    let sign = if opposite { "-" } else { "" };
    let text = format!(
        "model ProjectedPin Real x[{axes}](each stateSelect=StateSelect.always);
        Real u; output Real q(start=1,fixed=true); Real rate;
        equation der(x)=-x; u=x[{subscripts}]; q={sign}u; der(u)=rate; end ProjectedPin;"
    );
    let mut sources = SourceMap::new();
    let source = sources.add("projected_pin.mo", &text);
    let at = source_provenance(source, &text, &text);
    dae::Dae::construct(sources, |model| {
        let (real, tensor) = model.types(|t| {
            Ok((
                t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                t.derived(
                    dae::ValueType::array(dae::ScalarType::Real, shape.to_vec()),
                    at,
                )?,
            ))
        })?;
        let one = model.expressions(|e| e.at(at).literal(dae::DaeLiteral::Real(1.0)))?;
        let (x, u, q, rate) = model.variables(|v| {
            Ok((
                v.state(
                    VarName::new("x"),
                    tensor,
                    at,
                    dae::VariableAttributes {
                        state_select: rumoca_core::StateSelect::Always,
                        ..Default::default()
                    },
                )?,
                v.state(VarName::new("u"), real, at, Default::default())?,
                v.algebraic(
                    VarName::new("q"),
                    real,
                    at,
                    dae::VariableAttributes {
                        start: Some(one),
                        fixed: Some(vec![true]),
                        causality: dae::VariableCausality::Output,
                        ..Default::default()
                    },
                )?,
                v.algebraic(VarName::new("rate"), real, at, Default::default())?,
            ))
        })?;
        let rows = model.expressions(|e| {
            let dx = e.at(at).coordinate(dae::CoordinateInput::Derivative(x))?;
            let du = e.at(at).coordinate(dae::CoordinateInput::Derivative(u))?;
            let x = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
            let u = e.at(at).coordinate(dae::CoordinateInput::State(u))?;
            let q = e.at(at).coordinate(dae::CoordinateInput::Algebraic(q))?;
            let rate = e.at(at).coordinate(dae::CoordinateInput::Algebraic(rate))?;
            let signed_u = if opposite {
                e.at(at).unary(dae::UnaryOperator::Negate, u)?
            } else {
                u
            };
            let subscripts = indices
                .iter()
                .map(|&i| {
                    e.at(at)
                        .literal(dae::DaeLiteral::Integer(i))
                        .map(|expression| dae::Subscript::Index {
                            expression,
                            provenance: at,
                        })
                })
                .collect::<Result<Vec<_>, _>>()?;
            let selected = e.at(at).index(x, subscripts)?;
            Ok([
                e.at(at).binary(dae::BinaryOperator::Add, dx, x)?,
                e.at(at)
                    .binary(dae::BinaryOperator::Subtract, u, selected)?,
                e.at(at)
                    .binary(dae::BinaryOperator::Subtract, q, signed_u)?,
                e.at(at).binary(dae::BinaryOperator::Subtract, du, rate)?,
            ])
        })?;
        model.continuous(|c| {
            for row in rows {
                c.value_equation(at, row)?;
            }
            Ok(())
        })
    })
    .unwrap()
}

fn component_pin_model(profile: PinProfile) -> dae::Dae {
    let q_rhs = match profile {
        PinProfile::Dynamic => "x[i]",
        PinProfile::Nonlinear => "x[1]*x[1]",
        _ => "x[1]",
    };
    let fixed = if profile == PinProfile::FixedTarget {
        "true"
    } else {
        "false"
    };
    let text = format!(
        "model ComponentPins Real x[3](start={{0,0,0}},each fixed={fixed});
        Real q(start=1,fixed=true), y(start=2,fixed=true); parameter Integer i=1;
        equation der(x)=-x; q={q_rhs}; y=x[2]+10; end ComponentPins;"
    );
    let mut sources = SourceMap::new();
    let source = sources.add("component_pins.mo", &text);
    let at = source_provenance(source, &text, &text);
    dae::Dae::construct(sources, |m| {
        let (real, array, integer) = m.types(|t| {
            Ok((
                t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                t.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at)?,
                t.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)?,
            ))
        })?;
        let (zero, one, two, integer_one) = m.expressions(|e| {
            let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            Ok((
                e.at(at).array([zero, zero, zero])?,
                e.at(at).literal(dae::DaeLiteral::Real(1.0))?,
                e.at(at).literal(dae::DaeLiteral::Real(2.0))?,
                e.at(at).literal(dae::DaeLiteral::Integer(1))?,
            ))
        })?;
        let (x, q, y, i) = m.variables(|v| {
            Ok((
                v.state(
                    VarName::new("x"),
                    array,
                    at,
                    dae::VariableAttributes {
                        start: Some(zero),
                        fixed: Some(vec![profile == PinProfile::FixedTarget]),
                        ..Default::default()
                    },
                )?,
                v.algebraic(
                    VarName::new("q"),
                    real,
                    at,
                    dae::VariableAttributes {
                        start: Some(one),
                        fixed: Some(vec![true]),
                        ..Default::default()
                    },
                )?,
                v.algebraic(
                    VarName::new("y"),
                    real,
                    at,
                    dae::VariableAttributes {
                        start: Some(two),
                        fixed: Some(vec![true]),
                        ..Default::default()
                    },
                )?,
                v.parameter(
                    VarName::new("i"),
                    integer,
                    at,
                    dae::VariableAttributes {
                        binding: Some(integer_one),
                        ..Default::default()
                    },
                )?,
            ))
        })?;
        let rows = m.expressions(|e| component_pin_rows(e, profile, (x, q, y, i), at))?;
        m.continuous(|c| {
            for row in rows {
                c.value_equation(at, row)?;
            }
            Ok(())
        })
    })
    .unwrap()
}

fn component_pin_rows<'dae>(
    e: &mut dae::Expressions<'_, 'dae>,
    profile: PinProfile,
    variables: (
        dae::StateId<'dae>,
        dae::AlgebraicId<'dae>,
        dae::AlgebraicId<'dae>,
        dae::ParameterId<'dae>,
    ),
    at: dae::DaeProvenance,
) -> Result<[dae::ExprId<'dae>; 3], dae::DaeConstructionError> {
    let (x, q, y, i) = variables;
    let dx = e.at(at).coordinate(dae::CoordinateInput::Derivative(x))?;
    let x = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
    let q = e.at(at).coordinate(dae::CoordinateInput::Algebraic(q))?;
    let y = e.at(at).coordinate(dae::CoordinateInput::Algebraic(y))?;
    let index = if profile == PinProfile::Dynamic {
        e.at(at).coordinate(dae::CoordinateInput::Parameter(i))?
    } else {
        e.at(at).literal(dae::DaeLiteral::Integer(1))?
    };
    let first = e.at(at).index(
        x,
        [dae::Subscript::Index {
            expression: index,
            provenance: at,
        }],
    )?;
    let rhs = if profile == PinProfile::Nonlinear {
        e.at(at)
            .binary(dae::BinaryOperator::Multiply, first, first)?
    } else {
        first
    };
    let two = e.at(at).literal(dae::DaeLiteral::Integer(2))?;
    let second = e.at(at).index(
        x,
        [dae::Subscript::Index {
            expression: two,
            provenance: at,
        }],
    )?;
    let ten = e.at(at).literal(dae::DaeLiteral::Real(10.0))?;
    let shifted = e.at(at).binary(dae::BinaryOperator::Add, second, ten)?;
    Ok([
        e.at(at).binary(dae::BinaryOperator::Add, dx, x)?,
        e.at(at).binary(dae::BinaryOperator::Subtract, q, rhs)?,
        e.at(at).binary(dae::BinaryOperator::Subtract, y, shifted)?,
    ])
}
