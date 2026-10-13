//! Exact exponent parity must reach the canonical typed executor.

use super::*;

#[test]
fn typed_real_power_preserves_signed_wide_integer_exponents() {
    for (base_shape, exponent_shape) in [(vec![], vec![]), (vec![2], vec![]), (vec![2], vec![2])] {
        let model = power_fixture(
            dae::BinaryOperator::ElementwisePower,
            &base_shape,
            &exponent_shape,
        );
        let table = lower_root_call(&model);
        let [owner] = table.owners() else {
            panic!("one source power owner");
        };
        for exponent in [
            9_007_199_254_740_993,
            -9_007_199_254_740_993,
            i64::MIN,
            i64::MAX,
            0,
            -1,
            63,
        ] {
            let base = rumoca_eval_solve::TypedValue::construct(
                owner.inputs()[0].clone(),
                vec![
                    solve::SolveValueKind::Real64((-1.0_f64).to_bits());
                    owner.inputs()[0].scalar_count() as usize
                ],
            )
            .unwrap();
            let exponent_value = rumoca_eval_solve::TypedValue::construct(
                owner.inputs()[1].clone(),
                vec![
                    solve::SolveValueKind::Integer(exponent);
                    owner.inputs()[1].scalar_count() as usize
                ],
            )
            .unwrap();
            let results =
                rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[base, exponent_value])
                    .unwrap();
            let expected: f64 = if exponent % 2 == 0 { 1.0 } else { -1.0 };
            for value in results[0].elements() {
                assert_eq!(
                    *value,
                    solve::SolveValueKind::Real64(expected.to_bits()),
                    "exponent {exponent}"
                );
            }
        }
    }
}

#[test]
fn typed_integer_exponent_power_preserves_zero_and_large_base_controls() {
    for shape in [vec![], vec![2]] {
        let model = power_fixture(dae::BinaryOperator::ElementwisePower, &shape, &[]);
        let table = lower_root_call(&model);
        let [owner] = table.owners() else {
            panic!("one source power owner");
        };
        for (base, exponent, expected) in [
            (0.0_f64, 0_i64, 1.0_f64),
            (0.0, 2, 0.0),
            (1e200, 1, 1e200),
            (2.0, -1, 0.5),
            (-2.0, -3, -0.125),
            (-0.0, 3, -0.0),
        ] {
            let base = rumoca_eval_solve::TypedValue::construct(
                owner.inputs()[0].clone(),
                vec![
                    solve::SolveValueKind::Real64(base.to_bits());
                    owner.inputs()[0].scalar_count() as usize
                ],
            )
            .unwrap();
            let exponent_value = rumoca_eval_solve::TypedValue::construct(
                owner.inputs()[1].clone(),
                vec![solve::SolveValueKind::Integer(exponent)],
            )
            .unwrap();
            let result =
                rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[base, exponent_value])
                    .unwrap();
            for value in result[0].elements() {
                assert_eq!(
                    *value,
                    solve::SolveValueKind::Real64(expected.to_bits()),
                    "exponent {exponent}"
                );
            }
        }
    }
}

#[test]
fn typed_matrix_power_computes_a_matrix_product_or_refuses() {
    let model = power_fixture(dae::BinaryOperator::Power, &[2, 2], &[]);
    let lowered = model.inspect(|view| {
        let call = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|expression| {
                view.expression(*expression).is_some_and(|node| {
                    node.function_scope().is_none()
                        && matches!(node.operation(), dae::ExpressionOperation::Call { .. })
                })
            })
            .unwrap();
        lower_exact_call(
            view,
            call,
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
        )
    });
    let table = match lowered {
        Ok(table) => table,
        Err(solve::SolveProgramConstructionError::InvalidTensorAlgebra { .. }) => return,
        Err(error) => panic!("matrix power must give its explicit typed refusal: {error}"),
    };
    let [owner] = table.owners() else {
        panic!("one source matrix power owner");
    };
    let base = rumoca_eval_solve::TypedValue::construct(
        owner.inputs()[0].clone(),
        vec![solve::SolveValueKind::Real64(1.0_f64.to_bits()); 4],
    )
    .unwrap();
    let exponent = rumoca_eval_solve::TypedValue::construct(
        owner.inputs()[1].clone(),
        vec![solve::SolveValueKind::Integer(2)],
    )
    .unwrap();
    let results = rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[base, exponent]).unwrap();
    assert_eq!(
        results[0].elements(),
        &[solve::SolveValueKind::Real64(2.0_f64.to_bits()); 4],
        "matrix square multiplies matrices rather than squaring each element"
    );
}

fn power_fixture(
    operator: dae::BinaryOperator,
    base_shape: &[u32],
    exponent_shape: &[u32],
) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("typed_integer_power.mo", "y := a .^ n;");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 12)).unwrap();
    dae::Dae::construct(sources, |model| {
        let (base_type, exponent_type) = model.types(|types| {
            let shape_type = |scalar, shape: &[u32]| dae::ValueType::array(scalar, shape.to_vec());
            Ok((
                types.derived(shape_type(dae::ScalarType::Real, base_shape), at)?,
                types.derived(shape_type(dae::ScalarType::Integer, exponent_shape), at)?,
            ))
        })?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(
                VarName::new("integerExponent"),
                [base_type, exponent_type],
                [base_type],
                at,
            ),
            |model, reservation| {
                let (a, n, y) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("a"), 0, at)?,
                        functions.parameter(&reservation, VarName::new("n"), 1, at)?,
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                    ))
                })?;
                let power = model.expressions(|expressions| {
                    let a = expressions.at(at).function_parameter(a)?;
                    let n = expressions.at(at).function_parameter(n)?;
                    expressions.at(at).binary(operator, a, n)
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| functions.assign(&mut body, y, power, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        let (a, n) = model.variables(|variables| {
            Ok((
                variables.input(
                    VarName::new("a"),
                    base_type,
                    dae::InputVariability::Continuous,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.input(
                    VarName::new("n"),
                    exponent_type,
                    dae::InputVariability::Discrete,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        model.expressions(|expressions| {
            let a = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Input(a))?;
            let n = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Input(n))?;
            expressions.at(at).call(function, 0, [a, n])?;
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn typed_array_base_integer_power_has_a_directional_owner() {
    for exponent_shape in [vec![], vec![2]] {
        let model = power_fixture(dae::BinaryOperator::ElementwisePower, &[2], &exponent_shape);
        let table = lower_root_call(&model);
        let [owner] = table.owners() else {
            panic!("one source power owner");
        };
        assert!(
            owner.directional().is_some(),
            "array-base Real .^ Integer must keep its directional owner"
        );
    }
}
