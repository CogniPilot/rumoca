use rumoca_core::{SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain, TypeId, VarName};

use super::*;

fn provenance(source: rumoca_core::SourceId, start: usize, end: usize) -> dae::DaeProvenance {
    dae::DaeProvenance::source(Span::from_offsets(source, start, end)).unwrap()
}

fn construct_second_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    scalar: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let signature = dae::FunctionSignature::new(VarName::new("second"), [vector], [scalar], at);
    model
        .function(signature, |model, reservation| {
            let input = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, at)
            })?;
            let output = model
                .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
            let input =
                model.expressions(|expressions| expressions.at(at).function_parameter(input))?;
            let two = model.expressions(|expressions| {
                expressions.at(at).literal(dae::DaeLiteral::Integer(2))
            })?;
            let selected = model.expressions(|expressions| {
                expressions.at(at).index(
                    input,
                    [dae::Subscript::Index {
                        expression: two,
                        provenance: at,
                    }],
                )
            })?;
            let shared = model.expressions(|expressions| {
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Add, selected, selected)
            })?;
            let result = model.expressions(|expressions| {
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Multiply, shared, shared)
            })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, output, result, at))?;
            model.functions(|functions| functions.define(body, at))
        })
        .map(|(function, ())| function)
}

fn construct_select_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    integer: dae::ValueTypeId<'dae>,
    real: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let signature =
        dae::FunctionSignature::new(VarName::new("select"), [vector, integer], [real], at);
    model
        .function(signature, |model, reservation| {
            let values = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, at)
            })?;
            let index = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("i"), 1, at)
            })?;
            let output = model
                .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
            let values =
                model.expressions(|expressions| expressions.at(at).function_parameter(values))?;
            let index =
                model.expressions(|expressions| expressions.at(at).function_parameter(index))?;
            let selected = model.expressions(|expressions| {
                expressions.at(at).index(
                    values,
                    [dae::Subscript::Index {
                        expression: index,
                        provenance: at,
                    }],
                )
            })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, output, selected, at))?;
            model.functions(|functions| functions.define(body, at))
        })
        .map(|(function, ())| function)
}

#[test]
fn initial_condition_coordinate_projects_its_checked_runtime_owner() {
    let mut sources = SourceMap::new();
    let source = sources.add("initial_projection.mo", "initial()");
    let owner = provenance(source, 0, 9);
    let model = dae::Dae::construct(sources, |model| {
        let condition = model.conditions(|conditions| conditions.reserve(owner))?;
        model.conditions(|conditions| {
            conditions.define(condition, dae::ConditionInput::Initial, owner)
        })?;
        model.expressions(|expressions| {
            expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Condition(condition))?;
            Ok(())
        })
    })
    .unwrap();

    model.inspect(|view| {
        let expression = view.expression_id(0).expect("the coordinate exists");
        let condition = view.condition_id(0).expect("the condition exists");
        assert!(matches!(
            view.condition(condition).unwrap().operation(),
            dae::ConditionOperation::Initial
        ));
        let mut dependencies = Vec::new();
        for_each_scalar_coordinate(view, expression, 0, None, |coordinate, scalar| {
            dependencies.push((coordinate, scalar));
        })
        .unwrap();
        assert!(matches!(
            dependencies.as_slice(),
            [(dae::CoordinateView::Condition(found), 0)] if *found == condition
        ));
    });
}

#[test]
fn literal_and_slice_indices_select_exact_coordinate_scalars() {
    let mut sources = SourceMap::new();
    let source = sources.add("projection.mo", "Real x[3]; x[2]; x[{3,1}];");
    let declaration = provenance(source, 0, 10);
    let first_use = provenance(source, 11, 15);
    let second_use = provenance(source, 17, 25);
    let model = dae::Dae::construct(sources, |model| {
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
        model.expressions(|expressions| {
            let x_first = expressions
                .at(first_use)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            let two = expressions
                .at(first_use)
                .literal(dae::DaeLiteral::Integer(2))?;
            expressions.at(first_use).index(
                x_first,
                [dae::Subscript::Index {
                    expression: two,
                    provenance: first_use,
                }],
            )?;
            let x_second = expressions
                .at(second_use)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            let three = expressions
                .at(second_use)
                .literal(dae::DaeLiteral::Integer(3))?;
            let one = expressions
                .at(second_use)
                .literal(dae::DaeLiteral::Integer(1))?;
            let selection = expressions.at(second_use).array([three, one])?;
            expressions.at(second_use).index(
                x_second,
                [dae::Subscript::Value {
                    expression: selection,
                    provenance: second_use,
                }],
            )?;
            Ok(())
        })
    })
    .unwrap();

    model.inspect(|view| {
        let scalar = view.expression_id(2).unwrap();
        let mut selected = Vec::new();
        for_each_scalar_coordinate(view, scalar, 0, None, |coordinate, index| {
            assert!(matches!(coordinate, dae::CoordinateView::Algebraic(_)));
            selected.push(index);
        })
        .unwrap();
        assert_eq!(selected, [1]);

        let slice = view.expression_id(7).unwrap();
        selected.clear();
        for_each_scalar_coordinate(view, slice, 0, None, |_, scalar| selected.push(scalar))
            .unwrap();
        for_each_scalar_coordinate(view, slice, 1, None, |_, scalar| selected.push(scalar))
            .unwrap();
        assert_eq!(selected, [2, 0]);
    });
}

#[test]
fn omitted_and_explicit_unit_ranges_project_identically() {
    let mut sources = SourceMap::new();
    let source = sources.add("range_projection.mo", "Real x[3]; x[1:3]; x[1:1:3];");
    let declaration = provenance(source, 0, 10);
    let omitted_at = provenance(source, 11, 17);
    let explicit_at = provenance(source, 19, 27);
    let model = dae::Dae::construct(sources, |model| {
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
        model.expressions(|expressions| {
            let base = expressions
                .at(omitted_at)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            let start = expressions
                .at(omitted_at)
                .literal(dae::DaeLiteral::Integer(1))?;
            let stop = expressions
                .at(omitted_at)
                .literal(dae::DaeLiteral::Integer(3))?;
            let range = expressions.at(omitted_at).range(start, None, stop)?;
            expressions.at(omitted_at).index(
                base,
                [dae::Subscript::Slice {
                    expression: range,
                    provenance: omitted_at,
                }],
            )?;

            let base = expressions
                .at(explicit_at)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            let start = expressions
                .at(explicit_at)
                .literal(dae::DaeLiteral::Integer(1))?;
            let step = expressions
                .at(explicit_at)
                .literal(dae::DaeLiteral::Integer(1))?;
            let stop = expressions
                .at(explicit_at)
                .literal(dae::DaeLiteral::Integer(3))?;
            let range = expressions.at(explicit_at).range(start, Some(step), stop)?;
            expressions.at(explicit_at).index(
                base,
                [dae::Subscript::Slice {
                    expression: range,
                    provenance: explicit_at,
                }],
            )?;
            Ok(())
        })
    })
    .unwrap();

    model.inspect(|view| {
        let indices = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter(|id| {
                view.expression(*id)
                    .is_some_and(|expression| expression.kind() == dae::ExpressionKind::Index)
            })
            .collect::<Vec<_>>();
        assert_eq!(indices.len(), 2);
        for expression in indices {
            assert_eq!(projected_scalars(view, expression), [0, 1, 2]);
        }
    });
}

fn projected_scalars<'dae>(view: dae::DaeView<'dae>, expression: dae::ExprId<'dae>) -> Vec<usize> {
    let mut selected = Vec::new();
    let scalar_count = view
        .expression(expression)
        .expect("checked projection expression resolves")
        .value_type()
        .scalar_count()
        .expect("primitive projection has a scalar count");
    for scalar in 0..scalar_count {
        for_each_scalar_coordinate(view, expression, scalar, None, |_, selected_scalar| {
            selected.push(selected_scalar);
        })
        .unwrap();
    }
    selected
}

#[test]
fn vector_projects_each_result_scalar_to_the_same_compact_operand_scalar() {
    let mut sources = SourceMap::new();
    let source = sources.add("vector_projection.mo", "Real x[1,3,1]; vector(x);");
    let declaration = provenance(source, 0, 14);
    let use_site = provenance(source, 16, 25);
    let model = dae::Dae::construct(sources, |model| {
        let tensor = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [1, 3, 1]),
                declaration,
            )
        })?;
        let x = model.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                tensor,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        model.expressions(|expressions| {
            let x = expressions
                .at(use_site)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            expressions
                .at(use_site)
                .builtin(dae::PureBuiltin::Vector, [x])?;
            Ok(())
        })
    })
    .unwrap();

    model.inspect(|view| {
        let vector = view.expression_id(1).unwrap();
        assert_eq!(projected_scalars(view, vector), [0, 1, 2]);
    });
}

#[test]
fn transpose_projects_rank_three_scalars_through_the_first_two_axes() {
    let mut sources = SourceMap::new();
    let source = sources.add("transpose_projection.mo", "Real x[2,3,2]; transpose(x);");
    let declaration = provenance(source, 0, 14);
    let use_site = provenance(source, 16, 28);
    let model = dae::Dae::construct(sources, |model| {
        let tensor = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [2, 3, 2]),
                declaration,
            )
        })?;
        let x = model.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                tensor,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        model.expressions(|expressions| {
            let x = expressions
                .at(use_site)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            expressions
                .at(use_site)
                .builtin(dae::PureBuiltin::Transpose, [x])?;
            Ok(())
        })
    })
    .unwrap();

    model.inspect(|view| {
        let transpose = view.expression_id(1).unwrap();
        assert_eq!(
            projected_scalars(view, transpose),
            [0, 1, 6, 7, 2, 3, 8, 9, 4, 5, 10, 11]
        );
    });
}

#[test]
fn diagonal_and_outer_product_project_only_exact_operand_scalars() {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "matrix_products_projection.mo",
        "Real d[2]; Real lhs[2]; Real rhs[3]; diagonal(d); outerProduct(lhs,rhs);",
    );
    let at = provenance(source, 0, 1);
    let model = dae::Dae::construct(sources, |model| {
        let (vector2, vector3) = model.types(|types| {
            Ok((
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::array(dae::ScalarType::Real, [2]),
                    at,
                )?,
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::array(dae::ScalarType::Real, [3]),
                    at,
                )?,
            ))
        })?;
        let (d, lhs, rhs) = model.variables(|variables| {
            Ok((
                variables.algebraic(
                    VarName::new("d"),
                    vector2,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("lhs"),
                    vector2,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("rhs"),
                    vector3,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        model.expressions(|expressions| {
            let d = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(d))?;
            expressions
                .at(at)
                .builtin(dae::PureBuiltin::Diagonal, [d])?;
            let lhs = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(lhs))?;
            let rhs = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(rhs))?;
            expressions
                .at(at)
                .builtin(dae::PureBuiltin::OuterProduct, [lhs, rhs])?;
            Ok(())
        })
    })
    .unwrap();

    model.inspect(|view| {
        let diagonal = view.expression_id(1).unwrap();
        assert_eq!(projected_scalars(view, diagonal), [0, 1]);
        let outer = view.expression_id(4).unwrap();
        assert_eq!(
            projected_scalars(view, outer),
            [0, 0, 0, 1, 0, 2, 1, 0, 1, 1, 1, 2]
        );
    });
}

#[test]
fn skew_projects_only_off_diagonal_operand_scalars() {
    let mut sources = SourceMap::new();
    let source = sources.add("skew_projection.mo", "Real x[3]; skew(x);");
    let at = provenance(source, 0, 1);
    let model = dae::Dae::construct(sources, |model| {
        let vector = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [3]),
                at,
            )
        })?;
        let x = model.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                vector,
                at,
                dae::VariableAttributes::default(),
            )
        })?;
        model.expressions(|expressions| {
            let x = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            expressions.at(at).builtin(dae::PureBuiltin::Skew, [x])?;
            Ok(())
        })
    })
    .unwrap();

    model.inspect(|view| {
        let skew = view.expression_id(1).unwrap();
        assert_eq!(projected_scalars(view, skew), [2, 1, 2, 0, 1, 0]);
    });
}

#[test]
fn binder_substitution_uses_the_exact_domain_value() {
    let mut sources = SourceMap::new();
    let source = sources.add("binder.mo", "Real x[3]; for i in 1:3 loop x[i]; end for;");
    let declaration = provenance(source, 0, 10);
    let owner = provenance(source, 11, 43);
    let use_site = provenance(source, 29, 33);
    let model = dae::Dae::construct(sources, |model| {
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
                        id: 0,
                        display_name: "i".to_string(),
                        lower: 1,
                        upper: 3,
                        step: 1,
                    }],
                },
                owner,
            )
        })?;
        let binder = model.domains(|domains| domains.binder(domain, 0, use_site))?;
        model.expressions(|expressions| {
            let x = expressions
                .at(use_site)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            let i = expressions.at(use_site).binder(binder)?;
            expressions.at(use_site).index(
                x,
                [dae::Subscript::Index {
                    expression: i,
                    provenance: use_site,
                }],
            )?;
            Ok(())
        })
    })
    .unwrap();

    model.inspect(|view| {
        let domain = view.domain_id(0).unwrap();
        let expression = view.expression_id(2).unwrap();
        let mut selected = Vec::new();
        for_each_scalar_coordinate(view, expression, 0, Some((domain, &[2])), |_, index| {
            selected.push(index);
        })
        .unwrap();
        assert_eq!(selected, [1]);
    });
}

#[test]
fn dynamic_index_reports_all_potential_values_and_its_index_dependency() {
    let mut sources = SourceMap::new();
    let source = sources.add("dynamic.mo", "Real x[3]; input Integer i; x[i];");
    let x_at = provenance(source, 0, 10);
    let i_at = provenance(source, 12, 26);
    let use_at = provenance(source, 28, 32);
    let model = dae::Dae::construct(sources, |model| {
        let real_array = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [3]),
                x_at,
            )
        })?;
        let integer = model.types(|types| {
            types.intern(
                TypeId::new(1),
                dae::ValueType::scalar(dae::ScalarType::Integer),
                i_at,
            )
        })?;
        let (x, i) = model.variables(|variables| {
            Ok((
                variables.algebraic(
                    VarName::new("x"),
                    real_array,
                    x_at,
                    dae::VariableAttributes::default(),
                )?,
                variables.input(
                    VarName::new("i"),
                    integer,
                    dae::InputVariability::Discrete,
                    i_at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        model.expressions(|expressions| {
            let x = expressions
                .at(use_at)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            let i = expressions
                .at(use_at)
                .coordinate(dae::CoordinateInput::Input(i))?;
            expressions.at(use_at).index(
                x,
                [dae::Subscript::Index {
                    expression: i,
                    provenance: use_at,
                }],
            )?;
            Ok(())
        })
    })
    .unwrap();

    model.inspect(|view| {
        let mut selected = Vec::new();
        for_each_scalar_coordinate(
            view,
            view.expression_id(2).unwrap(),
            0,
            None,
            |coordinate, scalar| selected.push((coordinate, scalar)),
        )
        .unwrap();
        assert_eq!(selected.len(), 4);
        assert_eq!(selected[0].1, 0);
        assert_eq!(selected[1].1, 1);
        assert_eq!(selected[2].1, 2);
        assert_eq!(selected[3].1, 0);
        assert!(matches!(selected[3].0, dae::CoordinateView::Input(_)));
    });
}

#[test]
fn function_summary_substitutes_one_exact_argument_scalar() {
    let mut sources = SourceMap::new();
    let source = sources.add("function_projection.mo", "function second end second;");
    let at = provenance(source, 0, 27);
    let model = dae::Dae::construct(sources, |model| {
        let vector = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [3]),
                at,
            )
        })?;
        let scalar = model.types(|types| {
            types.intern(
                TypeId::new(1),
                dae::ValueType::scalar(dae::ScalarType::Real),
                at,
            )
        })?;
        let x = model.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                vector,
                at,
                dae::VariableAttributes::default(),
            )
        })?;
        let second = construct_second_function(model, vector, scalar, at)?;
        let argument = model.expressions(|expressions| {
            expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(x))
        })?;
        model.expressions(|expressions| expressions.at(at).call(second, 0, [argument]))?;
        Ok(())
    })
    .unwrap();

    model.inspect(|view| {
        let call = view
            .expression_id(view.expression_count() - 1)
            .expect("call expression exists");
        let mut dependencies = Vec::new();
        for_each_scalar_coordinate(view, call, 0, None, |coordinate, scalar| {
            dependencies.push((coordinate, scalar));
        })
        .unwrap();
        assert!(matches!(
            dependencies.as_slice(),
            [(dae::CoordinateView::Algebraic(_), 1)]
        ));
    });
}

#[test]
fn function_index_parameter_preserves_call_site_specialization() {
    let mut sources = SourceMap::new();
    let source = sources.add("function_index.mo", "function select end select;");
    let at = provenance(source, 0, 27);
    let model = dae::Dae::construct(sources, |model| {
        let vector = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [3]),
                at,
            )
        })?;
        let real = model.types(|types| {
            types.intern(
                TypeId::new(1),
                dae::ValueType::scalar(dae::ScalarType::Real),
                at,
            )
        })?;
        let integer = model.types(|types| {
            types.intern(
                TypeId::new(2),
                dae::ValueType::scalar(dae::ScalarType::Integer),
                at,
            )
        })?;
        let x = model.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                vector,
                at,
                dae::VariableAttributes::default(),
            )
        })?;
        let select = construct_select_function(model, vector, integer, real, at)?;
        let values = model.expressions(|expressions| {
            expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(x))
        })?;
        let two = model
            .expressions(|expressions| expressions.at(at).literal(dae::DaeLiteral::Integer(2)))?;
        model.expressions(|expressions| expressions.at(at).call(select, 0, [values, two]))?;
        Ok(())
    })
    .unwrap();

    model.inspect(|view| {
        let call = view
            .expression_id(view.expression_count() - 1)
            .expect("call expression exists");
        let mut dependencies = Vec::new();
        for_each_scalar_coordinate(view, call, 0, None, |coordinate, scalar| {
            dependencies.push((coordinate, scalar));
        })
        .unwrap();
        assert!(matches!(
            dependencies.as_slice(),
            [(dae::CoordinateView::Algebraic(_), 1)]
        ));
    });
}
