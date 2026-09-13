use super::*;

enum Selection {
    Index(i64),
    Whole,
    Slice(Vec<i64>),
}

impl Selection {
    fn subscript<'dae>(
        &self,
        expressions: &mut dae::Expressions<'_, 'dae>,
        at: dae::DaeProvenance,
    ) -> Result<dae::Subscript<'dae>, dae::DaeConstructionError> {
        match self {
            Self::Whole => Ok(dae::Subscript::Whole { provenance: at }),
            Self::Index(index) => {
                let expression = expressions
                    .at(at)
                    .literal(dae::DaeLiteral::Integer(*index))?;
                Ok(dae::Subscript::Index {
                    expression,
                    provenance: at,
                })
            }
            Self::Slice(indices) => {
                let elements = indices
                    .iter()
                    .map(|&index| expressions.at(at).literal(dae::DaeLiteral::Integer(index)))
                    .collect::<Result<Vec<_>, _>>()?;
                let expression = expressions.at(at).array(elements)?;
                Ok(dae::Subscript::Slice {
                    expression,
                    provenance: at,
                })
            }
        }
    }
}

fn updated_model(
    dimensions: &[u32],
    value_dimensions: &[u32],
    selection: &[Selection],
) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("array_update_projection.mo", "Real x; Real y; x = y;");
    let at = provenance(source, 0, 21);
    dae::Dae::construct(sources, |model| {
        let x_type = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, dimensions.to_vec()),
                at,
            )
        })?;
        let y_type = model.types(|types| {
            types.intern(
                TypeId::new(1),
                dae::ValueType::array(dae::ScalarType::Real, value_dimensions.to_vec()),
                at,
            )
        })?;
        let x = model.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                x_type,
                at,
                dae::VariableAttributes::default(),
            )
        })?;
        let y = model.variables(|variables| {
            variables.algebraic(
                VarName::new("y"),
                y_type,
                at,
                dae::VariableAttributes::default(),
            )
        })?;
        model.expressions(|expressions| {
            let x = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            let y = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(y))?;
            let subscripts = selection
                .iter()
                .map(|selection| selection.subscript(expressions, at))
                .collect::<Result<Vec<_>, dae::DaeConstructionError>>()?;
            expressions.at(at).array_update(x, y, subscripts)?;
            Ok(())
        })
    })
    .unwrap()
}

fn assert_dependencies(model: &dae::Dae, expected: &[(u32, usize)]) {
    model.inspect(|view| {
        let root = view.expression_id(view.expression_count() - 1).unwrap();
        for (scalar, &expected) in expected.iter().enumerate() {
            let mut dependencies = Vec::new();
            for_each_scalar_coordinate(view, root, scalar, None, |coordinate, index| {
                dependencies.push(algebraic_index(coordinate, index));
            })
            .unwrap();
            assert_eq!(
                dependencies,
                [expected],
                "updated tensor component {scalar}"
            );
        }
    });
}

fn algebraic_index(coordinate: dae::CoordinateView<'_>, index: usize) -> (u32, usize) {
    let dae::CoordinateView::Algebraic(variable) = coordinate else {
        panic!("unexpected coordinate");
    };
    (variable.index(), index)
}

#[test]
fn overwritten_scalar_has_no_dependency_on_the_old_value() {
    let model = updated_model(&[3], &[], &[Selection::Index(3)]);
    assert_dependencies(&model, &[(0, 0), (0, 1), (1, 0)]);
}

#[test]
fn whole_axis_update_keeps_only_the_selected_replacement_component() {
    let model = updated_model(&[2, 3], &[2], &[Selection::Whole, Selection::Index(2)]);
    assert_dependencies(&model, &[(0, 0), (1, 0), (0, 2), (0, 3), (1, 1), (0, 5)]);
}

#[test]
fn permuted_slice_update_preserves_replacement_coordinates() {
    let model = updated_model(
        &[2, 3],
        &[2],
        &[Selection::Slice(vec![2, 1]), Selection::Index(3)],
    );
    assert_dependencies(&model, &[(0, 0), (0, 1), (1, 1), (0, 3), (0, 4), (1, 0)]);
}

#[test]
fn updated_record_field_preserves_outer_and_field_coordinates() {
    let mut sources = SourceMap::new();
    let source = sources.add("record_update_projection.mo", "Pair x[3]; Pair y; x[2]=y;");
    let at = provenance(source, 0, 25);
    let model = dae::Dae::construct(sources, |model| {
        let (x_type, y_type, pair) = model.types(|types| {
            let vector = types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [2]),
                at,
            )?;
            let fields = [
                (VarName::new("first"), vector),
                (VarName::new("second"), vector),
            ];
            Ok((
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::array(dae::ScalarType::Real, [12]),
                    at,
                )?,
                types.intern(
                    TypeId::new(2),
                    dae::ValueType::array(dae::ScalarType::Real, [4]),
                    at,
                )?,
                types.record(VarName::new("Pair"), fields, at)?,
            ))
        })?;
        let x = model.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                x_type,
                at,
                dae::VariableAttributes::default(),
            )
        })?;
        let y = model.variables(|variables| {
            variables.algebraic(
                VarName::new("y"),
                y_type,
                at,
                dae::VariableAttributes::default(),
            )
        })?;
        model.expressions(|expressions| {
            let x = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            let y = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(y))?;
            let records = (0..3)
                .map(|index| record_from_flat(expressions, x, pair, index * 4, at))
                .collect::<Result<Vec<_>, _>>()?;
            let x = expressions.at(at).array(records)?;
            let y = record_from_flat(expressions, y, pair, 0, at)?;
            let two = expressions.at(at).literal(dae::DaeLiteral::Integer(2))?;
            let updated = expressions.at(at).array_update(
                x,
                y,
                [dae::Subscript::Index {
                    expression: two,
                    provenance: at,
                }],
            )?;
            expressions.at(at).field(updated, 1)?;
            Ok(())
        })
    })
    .unwrap();
    assert_dependencies(&model, &[(0, 2), (0, 3), (1, 2), (1, 3), (0, 10), (0, 11)]);
}

fn record_from_flat<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    source: dae::ExprId<'dae>,
    pair: dae::ValueTypeId<'dae>,
    start: i64,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let mut fields = Vec::new();
    for field in 0..2 {
        let mut elements = Vec::new();
        for element in 0..2 {
            let index = expressions
                .at(at)
                .literal(dae::DaeLiteral::Integer(start + 2 * field + element + 1))?;
            elements.push(expressions.at(at).index(
                source,
                [dae::Subscript::Index {
                    expression: index,
                    provenance: at,
                }],
            )?);
        }
        fields.push(expressions.at(at).array(elements)?);
    }
    expressions.at(at).record(pair, fields)
}

#[test]
fn tunable_update_index_keeps_both_possible_coordinate_dependencies() {
    let mut sources = SourceMap::new();
    let text = "Real x[3]; Real y; parameter Integer i=1; x[i]=y;";
    let source = sources.add("tunable_update_projection.mo", text);
    let at = provenance(source, 0, text.len());
    let model = dae::Dae::construct(sources, |model| {
        let (vector, real, integer) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)?,
            ))
        })?;
        let binding = model
            .expressions(|expressions| expressions.at(at).literal(dae::DaeLiteral::Integer(1)))?;
        let (x, y, i) = model.variables(|variables| {
            Ok((
                variables.algebraic(
                    VarName::new("x"),
                    vector,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("y"),
                    real,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.parameter(
                    VarName::new("i"),
                    integer,
                    at,
                    dae::VariableAttributes {
                        binding: Some(binding),
                        ..Default::default()
                    },
                )?,
            ))
        })?;
        model.expressions(|expressions| {
            let x = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            let y = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(y))?;
            let i = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(i))?;
            expressions.at(at).array_update(
                x,
                y,
                [dae::Subscript::Index {
                    expression: i,
                    provenance: at,
                }],
            )?;
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let root = view.expression_id(view.expression_count() - 1).unwrap();
        for scalar in 0..3 {
            let mut dependencies = Vec::new();
            for_each_scalar_coordinate(view, root, scalar, None, |coordinate, index| {
                dependencies.push(typed_index(coordinate, index));
            })
            .unwrap();
            assert_eq!(
                dependencies,
                [
                    ("algebraic", 0, scalar),
                    ("algebraic", 1, 0),
                    ("parameter", 2, 0)
                ]
            );
        }
    });
}

fn typed_index(coordinate: dae::CoordinateView<'_>, index: usize) -> (&'static str, u32, usize) {
    match coordinate {
        dae::CoordinateView::Algebraic(id) => ("algebraic", id.index(), index),
        dae::CoordinateView::Parameter(id) => ("parameter", id.index(), index),
        _ => panic!("unexpected coordinate"),
    }
}
