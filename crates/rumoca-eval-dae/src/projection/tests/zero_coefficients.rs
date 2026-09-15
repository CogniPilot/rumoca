use super::*;

#[derive(Clone, Copy)]
enum Coefficient {
    Fixed(f64),
    Tunable,
    ChangeableParent,
    Initialization,
}

fn coefficient<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
    kind: Coefficient,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let value = if let Coefficient::Fixed(value) = kind {
        value
    } else {
        0.0
    };
    let mut binding = model
        .expressions(|expressions| expressions.at(at).literal(dae::DaeLiteral::Real(value)))?;
    if matches!(kind, Coefficient::ChangeableParent) {
        let parent = model.variables(|variables| {
            variables.parameter(
                VarName::new("parent"),
                real,
                at,
                dae::VariableAttributes {
                    binding: Some(binding),
                    is_tunable: true,
                    ..Default::default()
                },
            )
        })?;
        binding = model.expressions(|expressions| {
            expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(parent))
        })?;
    }
    let parameter = model.variables(|variables| {
        variables.parameter(
            VarName::new("offDiagonal"),
            real,
            at,
            dae::VariableAttributes {
                binding: Some(binding),
                is_tunable: matches!(kind, Coefficient::Tunable),
                fixed: Some(!matches!(kind, Coefficient::Initialization)),
                ..Default::default()
            },
        )
    })?;
    model.expressions(|expressions| {
        expressions
            .at(at)
            .coordinate(dae::CoordinateInput::Parameter(parameter))
    })
}

fn model(kind: Coefficient, guarded_operand: bool) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("zero_coefficient.mo", "I*x");
    let at = provenance(source, 0, 3);
    dae::Dae::construct(sources, |model| {
        let (real, vector, matrix) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [2]), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [2, 2]), at)?,
            ))
        })?;
        let off_diagonal = coefficient(model, real, at, kind)?;
        let two = model
            .expressions(|expressions| expressions.at(at).literal(dae::DaeLiteral::Real(2.0)))?;
        let gain = model.variables(|variables| {
            variables.parameter(
                VarName::new("gain"),
                real,
                at,
                dae::VariableAttributes {
                    binding: Some(two),
                    is_tunable: true,
                    ..Default::default()
                },
            )
        })?;
        let binding = model.expressions(|expressions| {
            let gain = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(gain))?;
            let first = expressions
                .at(at)
                .builtin(dae::PureBuiltin::PromotedCat2, [gain, off_diagonal])?;
            let second = expressions
                .at(at)
                .builtin(dae::PureBuiltin::PromotedCat2, [off_diagonal, gain])?;
            expressions
                .at(at)
                .builtin(dae::PureBuiltin::PromotedCat1, [first, second])
        })?;
        let (matrix, x) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("I"),
                    matrix,
                    at,
                    dae::VariableAttributes {
                        binding: Some(binding),
                        ..Default::default()
                    },
                )?,
                variables.algebraic(VarName::new("x"), vector, at, Default::default())?,
            ))
        })?;
        model.expressions(|expressions| {
            let matrix = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(matrix))?;
            let mut x = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            if guarded_operand {
                x = expressions.at(at).builtin(dae::PureBuiltin::Sqrt, [x])?;
            }
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Multiply, matrix, x)?;
            Ok(())
        })
    })
    .unwrap()
}

fn unknowns(model: &dae::Dae, scalar: usize) -> Vec<usize> {
    model.inspect(|view| {
        let product = view.expression_id(view.expression_count() - 1).unwrap();
        let mut found = Vec::new();
        for_each_scalar_coordinate(view, product, scalar, None, |coordinate, index| {
            if matches!(coordinate, dae::CoordinateView::Algebraic(_)) {
                found.push(index);
            }
        })
        .unwrap();
        found.sort_unstable();
        found.dedup();
        found
    })
}

#[test]
fn fixed_zero_tensor_coefficients_do_not_couple_independent_unknowns() {
    for zero in [0.0, -0.0] {
        let model = model(Coefficient::Fixed(zero), false);
        assert_eq!(unknowns(&model, 0), [0]);
        assert_eq!(unknowns(&model, 1), [1]);
    }
}

#[test]
fn default_zero_and_initialization_values_cannot_remove_incidence() {
    for kind in [
        Coefficient::Tunable,
        Coefficient::ChangeableParent,
        Coefficient::Initialization,
        Coefficient::Fixed(1.0),
    ] {
        let model = model(kind, false);
        for scalar in 0..2 {
            assert_eq!(unknowns(&model, scalar), [0, 1]);
        }
    }
}

#[test]
fn zero_products_retain_domain_dependencies_of_noncoordinate_operands() {
    let model = model(Coefficient::Fixed(0.0), true);
    assert_eq!(unknowns(&model, 0), [0, 1]);
    assert_eq!(unknowns(&model, 1), [0, 1]);
}
