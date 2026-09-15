use super::*;

#[expect(
    clippy::too_many_lines,
    reason = "checked DAE fixture retains both whole-tensor and binder-dependent equation owners"
)]
fn derivative_tensor_model(domain_dependent: bool) -> dae::Dae {
    let source =
        TestSource::new("parameter Real A[2,2]; Real x[2]; Real z; der(x) = A*x; z = sum(der(x));");
    let at = source.at(0, 68);
    dae::Dae::construct(source.map, |model| {
        let (real, vector, matrix) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [2]), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [2, 2]), at)?,
            ))
        })?;
        let (matrix, state, output) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("A"),
                    matrix,
                    at,
                    dae::VariableAttributes {
                        is_tunable: true,
                        ..Default::default()
                    },
                )?,
                variables.state(VarName::new("x"), vector, at, Default::default())?,
                variables.algebraic(VarName::new("z"), real, at, Default::default())?,
            ))
        })?;
        let domain = if domain_dependent {
            Some(model.domains(|domains| {
                domains.structured(
                    StructuredIndexDomain {
                        binders: vec![StructuredIndexBinder {
                            id: 0,
                            display_name: "i".into(),
                            lower: 1,
                            upper: 2,
                            step: 1,
                        }],
                    },
                    at,
                )
            })?)
        } else {
            None
        };
        let binder = domain
            .map(|domain| model.domains(|domains| domains.binder(domain, 0, at)))
            .transpose()?;
        let (definition, observation) = model.expressions(|expressions| {
            let derivative = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let state = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::State(state))?;
            let matrix = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(matrix))?;
            let product =
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Multiply, matrix, state)?;
            let (target, value) = if let Some(binder) = binder {
                let index = expressions.at(at).binder(binder)?;
                let subscript = dae::Subscript::Index {
                    expression: index,
                    provenance: at,
                };
                let target = expressions.at(at).index(derivative, [subscript])?;
                let value = expressions.at(at).index(product, [subscript])?;
                let value =
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Multiply, index, value)?;
                (target, value)
            } else {
                (derivative, product)
            };
            let definition =
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, target, value)?;
            let sum = expressions
                .at(at)
                .builtin(dae::PureBuiltin::Sum, [derivative])?;
            let output = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(output))?;
            let observation =
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, output, sum)?;
            Ok((definition, observation))
        })?;
        model.continuous(|continuous| {
            if let Some(domain) = domain {
                continuous.structured_family(
                    at,
                    domain,
                    rumoca_core::ComprehensionScalarView::BinderSubstitution,
                    |family| family.body(definition),
                )?;
            } else {
                continuous.value_equation(at, definition)?;
            }
            continuous.value_equation(at, observation)
        })
    })
    .unwrap()
}

#[test]
fn derivative_components_share_their_tensor_definition() {
    check_derivative_tensor(false, 1);
}

#[test]
fn derivative_components_retain_domain_dependent_definitions() {
    check_derivative_tensor(true, 2);
}

fn check_derivative_tensor(domain_dependent: bool, expected_products: usize) {
    let model = derivative_tensor_model(domain_dependent);
    let lowered = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = lowered.continuous.implicit_rhs.nodes.as_slice()
    else {
        panic!("one implicit observation program expected");
    };
    let products = rows
        .programs()
        .iter()
        .flatten()
        .filter(|op| matches!(op, LinearOp::MatrixMultiply { .. }))
        .count();
    assert_eq!(
        products, expected_products,
        "only components with the same definition environment may share a product"
    );
    for (state, matrix) in [
        ([2.0, -3.0], [2.0, 3.0, -5.0, 7.0]),
        ([-1.0, 4.0], [0.0, -2.0, 3.0, 1.0]),
    ] {
        let expected = (matrix[0] * state[0] + matrix[1] * state[1])
            + (if domain_dependent { 2.0 } else { 1.0 })
                * (matrix[2] * state[0] + matrix[3] * state[1]);
        let actual = eval_residual_rows(rows, &[state[0], state[1], 0.0], &matrix);
        assert_eq!(rows.output_indices().len(), 1);
        assert_eq!(actual[rows.output_indices()[0]], -expected);
    }
}
