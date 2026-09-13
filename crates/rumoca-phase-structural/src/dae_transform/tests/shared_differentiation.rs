use super::*;

#[test]
fn index_reduction_preserves_shared_expression_growth() {
    for dimensions in [vec![], vec![3]] {
        let counts = [4, 8].map(|depth| {
            let model = shared_constraint_model(depth, dimensions.clone());
            let prepared = prepare_for_solve(&model).expect("shared smooth constraint reduces");
            prepared.as_dae().inspect(|view| {
                assert!(sort(view).is_ok());
                assert_eq!(view.variable_count(), 3);
                assert_eq!(view.continuous_owners().count(), 3);
                view.expression_count()
            })
        });
        assert!(
            counts[1] <= 3 * counts[0],
            "doubling shared expression depth must grow linearly: {counts:?}"
        );
    }
}

fn shared_constraint_model(depth: usize, dimensions: Vec<u32>) -> dae::Dae {
    let text = "Real x; Real y; Real a; equation x=f(y); der(y)=a; der(x)=1;";
    let mut sources = SourceMap::new();
    let source = sources.add("shared_constraint.mo", text);
    let at = source_provenance(source, text, text);
    dae::Dae::construct(sources, |model| {
        let ty = model.types(|types| {
            types.derived(
                dae::ValueType::array(dae::ScalarType::Real, dimensions.clone()),
                at,
            )
        })?;
        let (x, y, a) = model.variables(|variables| {
            Ok((
                variables.state(VarName::new("x"), ty, at, Default::default())?,
                variables.state(VarName::new("y"), ty, at, Default::default())?,
                variables.algebraic(VarName::new("a"), ty, at, Default::default())?,
            ))
        })?;
        let residuals = model.expressions(|expressions| {
            let x_value = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::State(x))?;
            let y_value = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::State(y))?;
            let mut rhs = y_value;
            for _ in 0..depth {
                let sum = expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Add, rhs, rhs)?;
                rhs = expressions.at(at).builtin(dae::PureBuiltin::Sin, [sum])?;
            }
            let constraint =
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, x_value, rhs)?;
            let dx = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Derivative(x))?;
            let dy = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Derivative(y))?;
            let a = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(a))?;
            let one = fixture_ones(expressions, at, &dimensions)?;
            Ok([
                constraint,
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, dy, a)?,
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, dx, one)?,
            ])
        })?;
        model.continuous(|continuous| {
            for residual in residuals {
                continuous.value_equation(at, residual)?;
            }
            Ok(())
        })
    })
    .unwrap()
}

fn fixture_ones<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    at: dae::DaeProvenance,
    dimensions: &[u32],
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if dimensions.is_empty() {
        return expressions.at(at).literal(dae::DaeLiteral::Real(1.0));
    }
    let sizes = dimensions
        .iter()
        .map(|&d| {
            expressions
                .at(at)
                .literal(dae::DaeLiteral::Integer(i64::from(d)))
        })
        .collect::<Result<Vec<_>, _>>()?;
    expressions.at(at).builtin(dae::PureBuiltin::Ones, sizes)
}
