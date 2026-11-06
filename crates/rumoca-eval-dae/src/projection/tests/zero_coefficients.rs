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
                fixed: Some(vec![!matches!(kind, Coefficient::Initialization)]),
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

/// Bounded test materialization only; production retains the proof DAG.
fn pattern_values(pattern: &ZeroCoefficientPattern) -> Vec<bool> {
    use ZeroCoefficientPatternView as P;
    let dimensions = pattern.dimensions();
    let count: usize = dimensions.iter().map(|&n| n as usize).product();
    assert!(count <= 4, "test materialization is deliberately bounded");
    match pattern.view() {
        P::Uniform(zero) => vec![zero; count],
        P::Array(elements) => elements.iter().flat_map(pattern_values).collect(),
        P::Concatenate(axis, elements) => (0..count)
            .map(|scalar| {
                let mut coords = row_major_coordinates(dimensions, scalar).unwrap();
                let mut selected = coords[axis as usize];
                for element in elements {
                    let shape = element.dimensions();
                    let extent = shape.get(axis as usize).copied().unwrap_or(1);
                    if selected < extent {
                        coords[axis as usize] = selected;
                        let scalar = flatten_coordinates(shape, &coords[..shape.len()]).unwrap();
                        return pattern_values(element)[scalar];
                    }
                    selected -= extent;
                }
                panic!("checked proof concatenation covers its shape")
            })
            .collect(),
        P::Transpose(base) => {
            let values = pattern_values(base);
            let rows = base.dimensions()[0] as usize;
            let columns = base.dimensions()[1] as usize;
            (0..count)
                .map(|s| values[(s % rows) * columns + s / rows])
                .collect()
        }
        P::Index(base, indices) => (0..count)
            .map(|scalar| {
                let result = row_major_coordinates(dimensions, scalar).unwrap();
                let mut result_axis = 0;
                let coordinates: Vec<_> = base
                    .dimensions()
                    .iter()
                    .enumerate()
                    .map(|(axis, _)| {
                        if let Some(Some(index)) = indices.get(axis) {
                            u32::try_from(index - 1).unwrap()
                        } else {
                            let value = result[result_axis];
                            result_axis += 1;
                            value
                        }
                    })
                    .collect();
                let source = flatten_coordinates(base.dimensions(), &coordinates).unwrap();
                pattern_values(base)[source]
            })
            .collect(),
        P::Union(lhs, rhs) => pattern_values(lhs)
            .into_iter()
            .zip(pattern_values(rhs))
            .map(|(a, b)| a || b)
            .collect(),
    }
}

#[test]
fn compact_zero_proof_refines_each_original_indexed_tensor_query() {
    for kind in [
        Coefficient::Fixed(0.0),
        Coefficient::Fixed(-0.0),
        Coefficient::Fixed(1.0),
        Coefficient::Tunable,
        Coefficient::ChangeableParent,
        Coefficient::Initialization,
    ] {
        let model = model(kind, false);
        model.inspect(|view| {
            let product = view
                .expression(view.expression_id(view.expression_count() - 1).unwrap())
                .unwrap();
            let dae::ExpressionOperation::Binary { lhs, rhs, .. } = product.operation() else {
                panic!()
            };
            let mut proof = ZeroCoefficients::default();
            let pattern = proof.omission_pattern(view, rhs, lhs).unwrap();
            let expected: Vec<_> = (0..4)
                .map(|scalar| proof.omits_coordinate(view, rhs, lhs, scalar))
                .collect();
            assert_eq!(pattern.dimensions(), &[2, 2]);
            assert_eq!(pattern_values(&pattern), expected);
        });
    }
}

#[test]
fn compact_zero_proof_refuses_the_same_domain_factor_as_indexed_incidence() {
    let model = model(Coefficient::Fixed(0.0), true);
    model.inspect(|view| {
        let product = view
            .expression(view.expression_id(view.expression_count() - 1).unwrap())
            .unwrap();
        let dae::ExpressionOperation::Binary { lhs, rhs, .. } = product.operation() else {
            panic!()
        };
        assert!(
            ZeroCoefficients::default()
                .omission_pattern(view, rhs, lhs)
                .is_none()
        );
    });
}

fn compact_mixed_coefficient(extent: u32) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("mixed_coefficient.mo", "[0; p]*x");
    let at = provenance(source, 0, 8);
    dae::Dae::construct(sources, |model| {
        let (wide, one) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [extent]), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [1]), at)?,
            ))
        })?;
        let (p, x) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    wide,
                    at,
                    dae::VariableAttributes {
                        is_tunable: true,
                        ..Default::default()
                    },
                )?,
                variables.algebraic(VarName::new("x"), one, at, Default::default())?,
            ))
        })?;
        model.expressions(|expressions| {
            let zero = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            let p = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(p))?;
            let coefficient = expressions
                .at(at)
                .builtin(dae::PureBuiltin::PromotedCat1, [zero, p])?;
            let x = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Multiply, coefficient, x)?;
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn mixed_zero_proof_metadata_follows_source_structure_not_tensor_extent() {
    for extent in [2, 1_000_000] {
        let model = compact_mixed_coefficient(extent);
        model.inspect(|view| {
            let product = view
                .expression(view.expression_id(view.expression_count() - 1).unwrap())
                .unwrap();
            let dae::ExpressionOperation::Binary { lhs, rhs, .. } = product.operation() else {
                panic!()
            };
            let pattern = ZeroCoefficients::default()
                .omission_pattern(view, rhs, lhs)
                .unwrap();
            assert_eq!(pattern.dimensions(), &[extent + 1, 1]);
            assert!(pattern.may_prove_zero());
            assert!(!pattern.proves_all_zero());
            let ZeroCoefficientPatternView::Concatenate(0, children) = pattern.view() else {
                panic!()
            };
            assert_eq!(children.len(), 2);
            assert!(matches!(
                children[0].view(),
                ZeroCoefficientPatternView::Uniform(true)
            ));
            assert!(matches!(
                children[1].view(),
                ZeroCoefficientPatternView::Uniform(false)
            ));
            assert_eq!(children[1].dimensions(), &[extent]);
        });
    }
}
