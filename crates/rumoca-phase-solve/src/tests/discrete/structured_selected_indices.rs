//! A structured B.1c binder remains symbolic inside selected-arm captures.
use super::*;

#[derive(Clone, Copy)]
enum Projection {
    Binder,
    Columns,
    RepeatedColumn,
}

fn model() -> dae::Dae {
    projected_model(Projection::Binder)
}

fn projected_model(projection: Projection) -> dae::Dae {
    let text = match projection {
        Projection::Columns => {
            "parameter Boolean choose; input Integer row; parameter Real table[2,3]; discrete Boolean flags[3]; equation flags=if choose then {table[row,1]>0,table[row,2]>0,table[row,3]>0} else {false,false,false};"
        }
        Projection::RepeatedColumn => {
            "parameter Boolean choose; input Integer row; parameter Real table[2,3]; discrete Boolean flags[3]; equation flags=if choose then {table[row,1]>0,table[row,2]>0,table[row,2]>0} else {false,false,false};"
        }
        Projection::Binder => {
            "parameter Boolean choose; input Integer row; parameter Real table[2,3]; discrete Boolean flags[3]; equation for i in 1:3 loop flags[i]=if choose then table[row,i]>0 else false; end for;"
        }
    };
    let source = TestSource::new(text);
    let at = source.at(0, text.len());
    dae::Dae::construct(source.map, |model| {
        let (boolean, integer, matrix, targets) = types(model, at)?;
        let (choose, row, table, flags) = model.variables(|variables| {
            Ok((
                variables.parameter(VarName::new("choose"), boolean, at, Default::default())?,
                variables.discrete_value(
                    VarName::new("row"),
                    integer,
                    at,
                    dae::VariableAttributes {
                        causality: dae::VariableCausality::Input,
                        declared_causality: dae::DeclaredCausality::Input,
                        ..Default::default()
                    },
                )?,
                variables.parameter(VarName::new("table"), matrix, at, Default::default())?,
                variables.discrete_value(VarName::new("flags"), targets, at, Default::default())?,
            ))
        })?;
        let domain = model.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![StructuredIndexBinder {
                        provenance: rumoca_core::IndexBinderProvenance::Generated,
                        id: 0,
                        display_name: "i".into(),
                        lower: 1,
                        upper: 3,
                        step: 1,
                    }],
                },
                at,
            )
        })?;
        let binder = model.domains(|domains| domains.binder(domain, 0, at))?;
        let guard = model.expressions(|expressions| {
            expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(choose))
        })?;
        let owner = model.begin_model_conditional(guard, at)?;
        let selected = model.model_conditional_arm(&owner, true, |model| {
            model.expressions(|expressions| {
                let table = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::Parameter(table))?;
                let row = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::DiscreteValue(row))?;
                selected_values(expressions, table, row, binder, projection, at)
            })
        })?;
        let fallback = model.model_conditional_arm(&owner, false, |model| {
            model.expressions(|expressions| fallback_value(expressions, projection, at))
        })?;
        let value = model.finish_model_conditional(owner, [selected, fallback])?;
        model.b1c([flags], |topology| {
            topology.structured_owner(
                at,
                domain,
                match projection {
                    Projection::Binder => rumoca_core::ComprehensionScalarView::BinderSubstitution,
                    Projection::Columns | Projection::RepeatedColumn => {
                        rumoca_core::ComprehensionScalarView::RowMajorProjection
                    }
                },
                [flags],
                |owner| owner.always(at, [(value, at)]),
            )?;
            Ok(())
        })
    })
    .unwrap()
}

fn fallback_value<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    projection: Projection,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let value = expressions
        .at(at)
        .literal(dae::DaeLiteral::Boolean(false))?;
    match projection {
        Projection::Binder => Ok(value),
        Projection::Columns | Projection::RepeatedColumn => expressions.at(at).array([value; 3]),
    }
}

fn selected_values<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    table: dae::ExprId<'dae>,
    row: dae::ExprId<'dae>,
    binder: dae::DomainBinderId<'dae>,
    projection: Projection,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let columns = match projection {
        Projection::Binder => vec![expressions.at(at).binder(binder)?],
        Projection::Columns | Projection::RepeatedColumn => {
            let columns = match projection {
                Projection::RepeatedColumn => [1, 2, 2],
                _ => [1, 2, 3],
            };
            columns
                .into_iter()
                .map(|value| expressions.at(at).literal(dae::DaeLiteral::Integer(value)))
                .collect::<Result<Vec<_>, _>>()?
        }
    };
    let values = columns
        .into_iter()
        .map(|column| {
            let value = expressions.at(at).index(
                table,
                [
                    dae::Subscript::Index {
                        expression: row,
                        provenance: at,
                    },
                    dae::Subscript::Index {
                        expression: column,
                        provenance: at,
                    },
                ],
            )?;
            let zero = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Greater, value, zero)
        })
        .collect::<Result<Vec<_>, _>>()?;
    if values.len() == 1 {
        Ok(values[0])
    } else {
        expressions.at(at).array(values)
    }
}

fn types<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    at: dae::DaeProvenance,
) -> Result<
    (
        dae::ValueTypeId<'dae>,
        dae::ValueTypeId<'dae>,
        dae::ValueTypeId<'dae>,
        dae::ValueTypeId<'dae>,
    ),
    dae::DaeConstructionError,
> {
    model.types(|types| {
        Ok((
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Boolean),
                at,
            )?,
            types.intern(
                TypeId::new(1),
                dae::ValueType::scalar(dae::ScalarType::Integer),
                at,
            )?,
            types.intern(
                TypeId::new(2),
                dae::ValueType::array(dae::ScalarType::Real, [2, 3]),
                at,
            )?,
            types.intern(
                TypeId::new(3),
                dae::ValueType::array(dae::ScalarType::Boolean, [3]),
                at,
            )?,
        ))
    })
}

fn parameters(problem: &rumoca_ir_solve::SolveProblem, choose: bool, row: f64) -> Vec<f64> {
    let mut p = vec![0.0; problem.layout.p_scalars()];
    let values: [&[f64]; 3] = [
        &[f64::from(choose)],
        &[row],
        &[-2.0, 3.0, -4.0, 5.0, -6.0, 7.0],
    ];
    for (ordinal, values) in values.into_iter().enumerate() {
        let ScalarSlot::P { index, .. } = problem.solve_layout.variable_storage_runs[ordinal].base
        else {
            panic!("parameter owns P storage")
        };
        p[index..index + values.len()].copy_from_slice(values);
    }
    p
}

fn evaluate(
    problem: &rumoca_ir_solve::SolveProblem,
    choose: bool,
    row: f64,
) -> Result<Vec<f64>, rumoca_eval_solve::EvalSolveError> {
    let block =
        rumoca_eval_solve::to_scalar_program_block(&problem.discrete.structured_rhs).unwrap();
    let mut output = vec![f64::NAN; 3];
    rumoca_eval_solve::eval_scalar_program_block(
        &block,
        &[],
        &parameters(problem, choose, row),
        0.0,
        None,
        &mut output,
    )?;
    Ok(output)
}

#[test]
fn structured_selected_indices_preserve_one_compact_owner_and_each_column() {
    let problem = lower_solve_problem(&model())
        .expect("nested runtime row and symbolic column have one affine program");
    problem.validate_shape_contract().unwrap();
    assert!(problem.discrete.rhs.is_empty());
    assert_eq!(problem.discrete.structured_rhs.nodes.len(), 1);
    assert_eq!(problem.discrete.structured_updates.len(), 1);
    assert!(matches!(
        problem.discrete.structured_rhs.nodes[0],
        ComputeNode::Map { .. }
    ));
    assert_eq!(evaluate(&problem, true, 1.0).unwrap(), [0.0, 1.0, 0.0]);
    assert_eq!(evaluate(&problem, true, 2.0).unwrap(), [1.0, 0.0, 1.0]);
}

#[test]
fn structured_selected_indices_keep_invalid_active_rows_faulting_and_inactive_rows_lazy() {
    let problem = lower_solve_problem(&model()).expect("selected regions retain runtime indexing");
    assert_eq!(evaluate(&problem, false, 99.0).unwrap(), [0.0; 3]);
    assert!(matches!(
        evaluate(&problem, true, 99.0),
        Err(rumoca_eval_solve::EvalSolveError::TensorIndexOutOfBounds {
            axis: 1,
            index: 99,
            extent: 2,
            ..
        })
    ));
}

#[test]
fn structured_selected_indices_row_major_projection_preserves_each_column() {
    let problem = lower_solve_problem(&projected_model(Projection::Columns)).unwrap();
    assert_eq!(problem.discrete.structured_rhs.nodes.len(), 1);
    assert_eq!(evaluate(&problem, true, 1.0).unwrap(), [0.0, 1.0, 0.0]);
    assert_eq!(evaluate(&problem, true, 2.0).unwrap(), [1.0, 0.0, 1.0]);
}

#[test]
fn structured_selected_indices_row_major_projection_keeps_lazy_faults() {
    let problem = lower_solve_problem(&projected_model(Projection::Columns)).unwrap();
    assert_eq!(evaluate(&problem, false, 99.0).unwrap(), [0.0; 3]);
    assert!(matches!(
        evaluate(&problem, true, 99.0),
        Err(rumoca_eval_solve::EvalSolveError::TensorIndexOutOfBounds {
            axis: 1,
            index: 99,
            extent: 2,
            ..
        })
    ));
}

#[test]
fn structured_selected_indices_two_point_agreement_cannot_admit_a_non_affine_family() {
    let error = lower_solve_problem(&projected_model(Projection::RepeatedColumn)).unwrap_err();
    assert!(matches!(error, LowerError::NonComputable { reason, .. } if reason.contains("affine")));
}
