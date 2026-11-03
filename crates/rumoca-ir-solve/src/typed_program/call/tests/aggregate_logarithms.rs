use super::*;

fn logarithm_table(
    arithmetic: SolveArithmeticProfile,
    tensor: &SolveValueType,
    operator: crate::SolveUnaryOperator,
) -> SolvePureCallTable {
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(731),
            vec![tensor.clone()],
            vec![SolvePureCallOutput::result(tensor.clone())],
            span(1200),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1201))?;
                let output = builder.unary(operator, input, span(1202))?;
                builder.store(outputs[0], output, span(1203))
            },
        )?;
        Ok(())
    })
    .expect("derivative profile refusal must retain the admitted primal owner")
}

#[test]
fn aggregate_logarithm_directional_keeps_checked_index_profile_and_compact_map() {
    for (minimum, maximum, extent, admitted) in [
        (-128, 127, 4, true),
        (-128, 127, 128, false),
        (2, 127, 4, false),
    ] {
        let arithmetic = SolveArithmeticProfile::construct(
            SolveRealFormat::Binary64,
            SolveIntegerDomain::construct(minimum, maximum).unwrap(),
        );
        let tensor =
            SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![extent]).unwrap();
        for operator in [
            crate::SolveUnaryOperator::Log,
            crate::SolveUnaryOperator::Log10,
        ] {
            let table = logarithm_table(arithmetic, &tensor, operator);
            let owner = &table.owners()[0];
            assert_eq!(owner.inputs(), std::slice::from_ref(&tensor));
            assert_eq!(owner.outputs()[0].value_type(), &tensor);
            assert_eq!(owner.directional().is_some(), admitted);
            if let Some(directional) = owner.directional() {
                assert_eq!(
                    directional
                        .body()
                        .operations()
                        .iter()
                        .filter(|op| matches!(op.operation(), crate::SolveOperation::Map { .. }))
                        .count(),
                    1,
                    "one compact tangent owner regardless of aggregate extent"
                );
            }
        }
    }
}

/// Inspect the issued primal and derivative programs without executing them.
#[test]
fn external_builtin_exp_uses_the_existing_exponential_derivative() {
    use rumoca_core::native_body::NativeBody;
    let arithmetic =
        SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL);
    let scalar = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(732),
            vec![scalar.clone()],
            vec![SolvePureCallOutput::result(scalar.clone())],
            span(1210),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1211))?;
                let result = builder.native(NativeBody::BuiltinExp, &[input], span(1212))?;
                builder.store(outputs[0], result[0], span(1213))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let owner = &table.owners()[0];
    let derivative = owner
        .directional()
        .expect("exp has a directional derivative");
    assert!(
        derivative
            .body()
            .operations()
            .iter()
            .any(|operation| matches!(
                operation.operation(),
                crate::SolveOperation::Unary {
                    operator: crate::SolveUnaryOperator::Exp,
                    ..
                }
            ))
    );
    assert!(
        derivative
            .body()
            .operations()
            .iter()
            .any(|operation| matches!(
                operation.operation(),
                crate::SolveOperation::Binary {
                    operator: crate::SolveBinaryOperator::Multiply,
                    ..
                }
            )),
        "the tangent multiplies exp(x) by the incoming tangent"
    );
    assert!(
        !derivative
            .body()
            .operations()
            .iter()
            .any(|operation| matches!(operation.operation(), crate::SolveOperation::Native { .. }))
    );
    let wire = serde_json::to_value(&table).unwrap();
    let replayed: SolvePureCallTable = serde_json::from_value(wire.clone()).unwrap();
    assert_eq!(serde_json::to_value(replayed).unwrap(), wire);
}

#[test]
fn external_builtin_logarithms_and_acos_keep_nonzero_directional_programs() {
    use rumoca_core::native_body::NativeBody;
    let arithmetic =
        SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL);
    let scalar = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    for (body, operator) in [
        (NativeBody::BuiltinLog10, crate::SolveUnaryOperator::Log10),
        (NativeBody::BuiltinLog, crate::SolveUnaryOperator::Log),
        (NativeBody::BuiltinAcos, crate::SolveUnaryOperator::Acos),
    ] {
        let table = SolvePureCallTable::construct(arithmetic, |table| {
            table.add_owner(
                identity(733),
                vec![scalar.clone()],
                vec![SolvePureCallOutput::result(scalar.clone())],
                span(1220),
                |builder, inputs, outputs| {
                    let input = builder.load(inputs[0], span(1221))?;
                    let result = builder.native(body, &[input], span(1222))?;
                    builder.store(outputs[0], result[0], span(1223))
                },
            )?;
            Ok(())
        })
        .unwrap();
        let derivative = table.owners()[0]
            .directional()
            .expect("the mathematical body has a directional derivative");
        let operations = derivative.body().operations();
        assert!(operations.iter().any(|op| matches!(
            op.operation(),
            crate::SolveOperation::Unary { operator: actual, .. } if *actual == operator
        )));
        assert!(
            operations.iter().any(|op| matches!(
                op.operation(),
                crate::SolveOperation::Binary {
                    operator: crate::SolveBinaryOperator::Divide,
                    ..
                }
            )),
            "the derivative retains its input-dependent divisor"
        );
        assert!(
            !operations
                .iter()
                .any(|op| matches!(op.operation(), crate::SolveOperation::Native { .. }))
        );
    }
}
