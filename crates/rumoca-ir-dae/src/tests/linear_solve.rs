use super::*;

fn linear_solve_model(
    matrix: ValueType,
    rhs: ValueType,
    arity: usize,
) -> Result<Dae, DaeConstructionError> {
    let source = TestSource::new("A*x=b");
    let at = source.source("A*x=b", 0);
    Dae::construct(source.map, |model| {
        let (matrix_type, rhs_type) =
            model.types(|types| Ok((types.derived(matrix, at)?, types.derived(rhs, at)?)))?;
        let (matrix, rhs) = model.variables(|variables| {
            Ok((
                variables.input(
                    VarName::new("A"),
                    matrix_type,
                    InputVariability::Discrete,
                    at,
                    VariableAttributes::default(),
                )?,
                variables.input(
                    VarName::new("b"),
                    rhs_type,
                    InputVariability::Discrete,
                    at,
                    VariableAttributes::default(),
                )?,
            ))
        })?;
        model.expressions(|expressions| {
            let matrix = expressions
                .at(at)
                .coordinate(CoordinateInput::Input(matrix))?;
            let rhs = expressions.at(at).coordinate(CoordinateInput::Input(rhs))?;
            let arguments = [matrix, rhs, rhs];
            expressions
                .at(at)
                .builtin(PureBuiltin::LinearSolve, arguments[..arity].iter().copied())?;
            Ok(())
        })
    })
}

#[test]
fn linear_solve_constructs_and_replays_one_tensor_expression() {
    for extent in [1, 3, 4096] {
        let model = linear_solve_model(
            ValueType::array(ScalarType::Real, [extent, extent]),
            ValueType::array(ScalarType::Real, [extent]),
            2,
        )
        .unwrap();
        let check = |view: DaeView<'_>| {
            assert_eq!(view.expression_count(), 3);
            let result = view.expression(view.expression_id(2).unwrap()).unwrap();
            assert_eq!(result.value_type().dimensions(), [extent]);
            assert_eq!(result.value_type().scalar_type(), ScalarType::Real);
            assert!(matches!(result.operation(), ExpressionOperation::Builtin {
                builtin: PureBuiltin::LinearSolve, arguments,
            } if arguments.len() == 2));
        };
        model.inspect(check);
        let decoded: Dae = serde_json::from_str(&serde_json::to_string(&model).unwrap()).unwrap();
        decoded.inspect(check);
        let decoded: Dae = bincode::deserialize(&bincode::serialize(&model).unwrap()).unwrap();
        decoded.inspect(check);
    }
}

#[test]
fn linear_solve_rejects_invalid_arity_shape_and_element_type() {
    for arity in [0, 1, 3] {
        assert!(matches!(linear_solve_model(
            ValueType::array(ScalarType::Real, [2, 2]),
            ValueType::array(ScalarType::Real, [2]), arity,
        ), Err(DaeConstructionError::InvalidArity { expected: 2, found, .. }) if found == arity));
    }
    for (matrix, rhs) in [
        (vec![2, 3], vec![2]),
        (vec![2, 2], vec![3]),
        (vec![2, 2], vec![2, 1]),
        (vec![4], vec![2]),
        (vec![0, 0], vec![0]),
    ] {
        assert!(
            linear_solve_model(
                ValueType::array(ScalarType::Real, matrix),
                ValueType::array(ScalarType::Real, rhs),
                2
            )
            .is_err()
        );
    }
    for (matrix, rhs) in [
        (ScalarType::Integer, ScalarType::Real),
        (ScalarType::Real, ScalarType::Integer),
        (ScalarType::Boolean, ScalarType::Real),
    ] {
        let result = linear_solve_model(
            ValueType::array(matrix, [2, 2]),
            ValueType::array(rhs, [2]),
            2,
        );
        assert!(
            matches!(result, Err(DaeConstructionError::TypeMismatch { .. })),
            "{result:?}"
        );
    }
}
