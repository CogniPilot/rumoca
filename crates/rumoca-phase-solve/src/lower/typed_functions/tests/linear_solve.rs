use super::*;

fn linear_solve_function(extent: u32) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("auxiliary_block.mo", "A*x=b");
    let at = dae::DaeProvenance::generated(
        dae::DaeGeneration::IndexReduction,
        Span::from_offsets(source, 0, 5),
    )
    .unwrap();
    dae::Dae::construct(sources, |model| {
        let (matrix_type, vector_type) = model.types(|types| {
            Ok((
                types.derived(
                    dae::ValueType::array(dae::ScalarType::Real, [extent, extent]),
                    at,
                )?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [extent]), at)?,
            ))
        })?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(
                VarName::new("auxiliary_block"),
                [matrix_type, vector_type],
                [vector_type],
                at,
            ),
            |model, reservation| {
                let (matrix, rhs, output) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("A"), 0, at)?,
                        functions.parameter(&reservation, VarName::new("b"), 1, at)?,
                        functions.output(&reservation, VarName::new("x"), 0, at)?,
                    ))
                })?;
                let result = model.expressions(|expressions| {
                    let matrix = expressions.at(at).function_parameter(matrix)?;
                    let rhs = expressions.at(at).function_parameter(rhs)?;
                    expressions
                        .at(at)
                        .builtin(dae::PureBuiltin::LinearSolve, [matrix, rhs])
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| functions.assign(&mut body, output, result, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        let (matrix, rhs) = model.variables(|variables| {
            Ok((
                variables.input(
                    VarName::new("A"),
                    matrix_type,
                    dae::InputVariability::Continuous,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.input(
                    VarName::new("b"),
                    vector_type,
                    dae::InputVariability::Continuous,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        model.expressions(|expressions| {
            let matrix = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Input(matrix))?;
            let rhs = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Input(rhs))?;
            expressions.at(at).call(function, 0, [matrix, rhs])?;
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn auxiliary_linear_solve_lowers_one_checked_tensor_kernel_at_every_extent() {
    for extent in [1, 3, 4096] {
        let table = lower_root_call(&linear_solve_function(extent));
        let [owner] = table.owners() else {
            panic!("one auxiliary call owner")
        };
        assert_eq!(owner.inputs().len(), 2);
        assert_eq!(owner.outputs().len(), 1);
        assert_eq!(owner.body().operations().len(), 4);
        assert_eq!(owner.body().register_types().len(), 3);
        assert!(matches!(
            owner.body().operations()[2].operation(),
            solve::SolveOperation::LinearSolve { .. }
        ));
    }
}

#[test]
fn auxiliary_linear_solve_executes_through_checked_call_and_directional_program() {
    let model = linear_solve_function(2);
    let table = lower_root_call(&model);
    let tensor = |dimensions, values: &[f64]| {
        rumoca_eval_solve::TypedValue::construct(
            solve::SolveValueType::tensor(
                solve::SolveScalarType::real(arithmetic_profile()),
                dimensions,
            )
            .unwrap(),
            values
                .iter()
                .map(|x| solve::SolveValueKind::Real64(x.to_bits()))
                .collect(),
        )
        .unwrap()
    };
    let result = rumoca_eval_solve::eval_pure_call_directional(
        &table,
        table.owners()[0].id(),
        &[
            tensor(vec![2, 2], &[0.0, 2.0, 3.0, 4.0]),
            tensor(vec![2, 2], &[1.0, 0.0, 0.0, -1.0]),
            tensor(vec![2], &[-4.0, -5.0]),
            tensor(vec![2], &[2.0, 3.0]),
        ],
    )
    .unwrap();
    let values = result
        .iter()
        .flat_map(|value| value.elements())
        .map(|value| {
            let solve::SolveValueKind::Real64(bits) = *value else {
                panic!("Real output")
            };
            f64::from_bits(bits)
        })
        .collect::<Vec<_>>();
    for (actual, expected) in values.iter().zip([1.0, -2.0, -1.0 / 3.0, 0.5]) {
        assert!((actual - expected).abs() < 1e-12, "{values:?}");
    }
}
