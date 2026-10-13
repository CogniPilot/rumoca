//! A Real power with an Integer exponent whose base reads a comprehension
//! binder lowers through the exact-power owner, with the binder passed as an
//! owner input.

use super::*;

/// `x[i] = (i * time) ^ exponent` over `i in 1:3`.
fn binder_power_model(exponent: i64) -> dae::Dae {
    let source = TestSource::new("Real x[3]; equation x = {(i * time) ^ 4 for i in 1:3};");
    let declaration = source.at(0, 10);
    let owner = source.at(21, 54);
    let range = source.at(50, 53);
    dae::Dae::construct(source.map, |model| {
        let real_array = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [3]),
                declaration,
            )
        })?;
        let x = model.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                real_array,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let domain = model.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![StructuredIndexBinder {
                        provenance: rumoca_core::IndexBinderProvenance::Generated,
                        id: 0,
                        display_name: "i".to_string(),
                        lower: 1,
                        upper: 3,
                        step: 1,
                    }],
                },
                range,
            )
        })?;
        let i = model.domains(|domains| domains.binder(domain, 0, owner))?;
        let residual = model.expressions(|expressions| {
            let x = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            let i = expressions.at(owner).binder(i)?;
            let time = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Time)?;
            let scaled = expressions
                .at(owner)
                .binary(dae::BinaryOperator::Multiply, i, time)?;
            let exponent = expressions
                .at(owner)
                .literal(dae::DaeLiteral::Integer(exponent))?;
            let power =
                expressions
                    .at(owner)
                    .binary(dae::BinaryOperator::Power, scaled, exponent)?;
            let family = expressions.at(owner).comprehension(domain, power)?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, x, family)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap()
}

fn evaluate_binder_power(exponent: i64, time: f64) -> (usize, [f64; 3]) {
    let model = binder_power_model(exponent);
    let package = lower_solve_package(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = package.problem.continuous.residual.nodes.as_slice()
    else {
        panic!("one compact multi-output residual block expected");
    };
    let mut actual = [0.0; 3];
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        rows,
        &[0.0; 3],
        &[],
        time,
        rumoca_eval_solve::RowEvalContext {
            pure_calls: Some(&package.pure_calls),
            ..Default::default()
        },
        &mut actual,
    )
    .unwrap();
    (package.pure_calls.owners().len(), actual)
}

/// Equal bit patterns, with every NaN equal to every other.
fn same_bits(actual: f64, expected: f64) -> bool {
    (actual.is_nan() && expected.is_nan()) || actual.to_bits() == expected.to_bits()
}

/// SOLVE-C86: every literal Integer exponent lowers as the one plan of
/// `modelica_integer_power`, so the scalar rows are bit-identical to the
/// canonical rule for negative, zero, unit, small, and large exponents on
/// random and edge bases, and no pure-call owner is issued.
#[test]
fn every_literal_integer_exponent_rounds_as_the_canonical_rule_without_an_owner() {
    let times = [
        2.0,
        0.7310585786300049,
        -1.3333333333333333,
        1.0000000000000002,
        0.0,
        -0.0,
        f64::NAN,
        f64::INFINITY,
        f64::NEG_INFINITY,
        f64::MIN_POSITIVE,
        5e-324,
        1e154,
    ];
    for exponent in [-17, -1, 0, 1, 2, 3, 16, 17, 31, 63] {
        for time in times {
            let (owners, actual) = evaluate_binder_power(exponent, time);
            assert_eq!(owners, 0, "exponent {exponent}");
            for (index, value) in actual.into_iter().enumerate() {
                let base = (index + 1) as f64 * time;
                let expected = 0.0 - rumoca_core::modelica_integer_power(base, exponent);
                assert!(
                    same_bits(value, expected),
                    "({base:e}) ^ {exponent}: {value:e} vs {expected:e}"
                );
            }
        }
    }
}
