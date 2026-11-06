//! The incidence zero proof controls omission at the shared constructor.
use super::*;

#[derive(Clone, Copy)]
pub(super) enum Coefficient {
    Fixed(f64),
    Tunable,
    TunableParent,
    Initialization,
}

fn product(kind: Coefficient, guarded: bool) -> dae::Dae {
    shaped_product(kind, guarded, &[])
}

fn shaped_product(kind: Coefficient, guarded: bool, dimensions: &[u32]) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("shared_zero.mo", "p*x");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 3)).unwrap();
    dae::Dae::construct(sources, |model| {
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let factor_type = model.types(|types| {
            types.derived(
                dae::ValueType::array(dae::ScalarType::Real, dimensions.to_vec()),
                at,
            )
        })?;
        let zero = model.expressions(|e| {
            e.at(at).literal(dae::DaeLiteral::Real(
                if let Coefficient::Fixed(value) = kind {
                    value
                } else {
                    0.0
                },
            ))
        })?;
        let binding = if matches!(kind, Coefficient::TunableParent) {
            let parent = model.variables(|v| {
                v.parameter(
                    VarName::new("parent"),
                    real,
                    at,
                    dae::VariableAttributes {
                        binding: Some(zero),
                        is_tunable: true,
                        ..Default::default()
                    },
                )
            })?;
            model.expressions(|e| e.at(at).coordinate(dae::CoordinateInput::Parameter(parent)))?
        } else {
            zero
        };
        let (p, x) = model.variables(|v| {
            Ok((
                v.parameter(
                    VarName::new("p"),
                    real,
                    at,
                    dae::VariableAttributes {
                        binding: Some(binding),
                        is_tunable: matches!(kind, Coefficient::Tunable),
                        fixed: Some(vec![!matches!(kind, Coefficient::Initialization)]),
                        ..Default::default()
                    },
                )?,
                v.algebraic(VarName::new("x"), factor_type, at, Default::default())?,
            ))
        })?;
        model.expressions(|e| {
            let p = e.at(at).coordinate(dae::CoordinateInput::Parameter(p))?;
            let mut x = e.at(at).coordinate(dae::CoordinateInput::Algebraic(x))?;
            if guarded {
                x = e.at(at).builtin(dae::PureBuiltin::Sqrt, [x])?;
            }
            e.at(at).binary(dae::BinaryOperator::Multiply, p, x)?;
            Ok(())
        })
    })
    .unwrap()
}

fn lower(model: &dae::Dae, provide_operands: bool) -> solve::TypedProgram {
    try_lower(model, provide_operands).unwrap()
}

fn try_lower(
    model: &dae::Dae,
    provide_operands: bool,
) -> Result<solve::TypedProgram, solve::SolveProgramConstructionError> {
    try_lower_root(model, provide_operands, false)
}

fn try_lower_root(
    model: &dae::Dae,
    provide_operands: bool,
    reference: bool,
) -> Result<solve::TypedProgram, solve::SolveProgramConstructionError> {
    model.inspect(|view| {
        solve::TypedProgram::construct(arithmetic_profile(), |builder| {
            let product = expression_root(view, reference);
            let node = view.expression(product).unwrap();
            let coordinates = if provide_operands {
                coordinate_inputs(view, builder, node.provenance().span())?
            } else {
                HashMap::new()
            };
            let mut lowerer = ExpressionLowerer::for_model_coordinates(view, builder, coordinates);
            lowerer.expression(product)?;
            Ok(())
        })
    })
}

#[test]
fn fixed_zero_omits_both_operand_demands_before_lowering() {
    for zero in [0.0, -0.0] {
        let program = lower(&product(Coefficient::Fixed(zero), false), false);
        assert_eq!(
            program.operations().len(),
            1,
            "neither operand needs an input binding"
        );
        assert!(
            matches!(program.operations()[0].operation(), solve::SolveOperation::Constant { value, .. }
            if value.kind() == solve::SolveValueKind::Real64(0))
        );
    }
}

#[test]
fn tunable_default_and_initialization_zero_do_not_omit_operands() {
    for kind in [
        Coefficient::Tunable,
        Coefficient::TunableParent,
        Coefficient::Initialization,
        Coefficient::Fixed(1.0),
    ] {
        let program = lower(&product(kind, false), true);
        assert!(program.operations().iter().any(|op| matches!(
            op.operation(),
            solve::SolveOperation::Binary {
                operator: solve::SolveBinaryOperator::Multiply,
                ..
            }
        )));
    }
}

#[test]
fn zero_coefficient_keeps_a_factor_with_domain_dependencies() {
    let program = lower(&product(Coefficient::Fixed(0.0), true), true);
    assert!(program.operations().iter().any(|op| matches!(
        op.operation(),
        solve::SolveOperation::Unary {
            operator: solve::SolveUnaryOperator::Sqrt,
            ..
        }
    )));
    assert!(program.operations().iter().any(|op| matches!(
        op.operation(),
        solve::SolveOperation::Binary {
            operator: solve::SolveBinaryOperator::Multiply,
            ..
        }
    )));
}

#[test]
fn a_uniform_zero_product_stays_compact_for_a_million_element_factor() {
    for extent in [2, 1_000_000] {
        let program = lower(
            &shaped_product(Coefficient::Fixed(0.0), false, &[extent]),
            false,
        );
        assert_eq!(program.operations().len(), 2);
        assert!(matches!(
            program.operations()[1].operation(),
            solve::SolveOperation::Fill { .. }
        ));
        assert_eq!(
            program.register_types().last().unwrap().dimensions(),
            &[extent]
        );
    }
}

fn coordinate_inputs<'program, 'dae>(
    view: dae::DaeView<'dae>,
    builder: &mut solve::TypedProgramBuilder<'program>,
    at: Span,
) -> Result<
    HashMap<ModelCoordinateKey<'dae>, LoweredValue<'program, 'dae>>,
    solve::SolveProgramConstructionError,
> {
    let mut coordinates = HashMap::new();
    for index in 0..view.expression_count() {
        let id = view.expression_id(index).unwrap();
        let coordinate = view.expression(id).unwrap();
        let dae::ExpressionOperation::Coordinate(key) = coordinate.operation() else {
            continue;
        };
        let scalar = solve::SolveScalarType::real(arithmetic_profile());
        let dimensions = coordinate.value_type().dimensions();
        let value_type = if dimensions.is_empty() {
            solve::SolveValueType::scalar(scalar)
        } else {
            solve::SolveValueType::tensor(scalar, dimensions.to_vec()).unwrap()
        };
        let input = builder.declare_slot(
            value_type,
            solve::SolveStorageClass::Input,
            solve::SolveSlotAccess::ReadOnly,
            at,
        )?;
        let value = builder.load(input, at)?;
        coordinates.insert(
            ModelCoordinateKey::from_view(key).unwrap(),
            LoweredValue::scalar(coordinate.value_type_id(), value),
        );
    }
    Ok(coordinates)
}

fn indexed_factor(index: i64) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("indexed_zero.mo", "0*x[i]");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 6)).unwrap();
    dae::Dae::construct(sources, |model| {
        let vector = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [2]), at))?;
        let x = model.variables(|variables| {
            variables.algebraic(VarName::new("x"), vector, at, Default::default())
        })?;
        model.expressions(|e| {
            let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            let x = e.at(at).coordinate(dae::CoordinateInput::Algebraic(x))?;
            let index = e.at(at).literal(dae::DaeLiteral::Integer(index))?;
            let selected = e.at(at).index(
                x,
                [dae::Subscript::Index {
                    expression: index,
                    provenance: at,
                }],
            )?;
            e.at(at)
                .binary(dae::BinaryOperator::Multiply, zero, selected)?;
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn zero_omission_keeps_literal_factor_index_refusals_and_in_range_omission() {
    let model = indexed_factor(3);
    let candidate = try_lower_root(&model, true, false).unwrap_err();
    let reference = try_lower_root(&model, true, true).unwrap_err();
    assert!(matches!(
        candidate,
        solve::SolveProgramConstructionError::InvalidProjection { .. }
    ));
    assert_eq!(
        candidate, reference,
        "construction refusal and source provenance must match retained evaluation"
    );
    for index in [1, 2] {
        let program = lower(&indexed_factor(index), false);
        assert_eq!(
            program.operations().len(),
            1,
            "proved in-range omission needs no factor input"
        );
    }
}

pub(super) fn expression_call(model: &dae::Dae, reference: bool) -> solve::SolvePureCallTable {
    model.inspect(|view| {
        let root = expression_root(view, reference);
        let node = view.expression(root).unwrap();
        let at = node.provenance().span();
        let coordinates =
            model_coordinates::collect_model_coordinate_types(view, [root], [], at).unwrap();
        let inputs = coordinates
            .iter()
            .map(|(_, ty)| lower_primitive_type(view, *ty, arithmetic_profile()).unwrap())
            .collect();
        let output =
            lower_primitive_type(view, node.value_type_id(), arithmetic_profile()).unwrap();
        let identity = solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap());
        let mut registry = PureCallRegistry::new();
        registry.identities = CallRegistration::new(Some(identity));
        let selected_calls = crate::lower::typed_functions::zero_products::register_selections(
            &mut registry.table,
            view,
            [root],
            &mut registry.identities,
        )
        .unwrap();
        registry
            .table
            .add_owner(
                identity,
                inputs,
                vec![solve::SolvePureCallOutput::result(output)],
                at,
                |builder, inputs, outputs| {
                    let mut values = HashMap::new();
                    for ((key, ty), slot) in coordinates.iter().zip(inputs) {
                        let value = builder.load(*slot, at)?;
                        values.insert(*key, LoweredValue::scalar(*ty, value));
                    }
                    let mut lowerer =
                        ExpressionLowerer::for_model_coordinates(view, builder, values);
                    lowerer.selected_calls = selected_calls.clone();
                    let result = lowerer.expression(root)?.only_register(at)?;
                    lowerer.builder.store(outputs[0], result, at)
                },
            )
            .unwrap();
        registry.finish()
    })
}

#[test]
fn zero_omission_preserves_runtime_nonpositive_index_faults() {
    for index in [-1, 0, 1, 2] {
        let model = indexed_factor(index);
        let table = expression_call(&model, false);
        let owner = &table.owners().last().unwrap();
        let input = rumoca_eval_solve::TypedValue::construct(
            owner.inputs()[0].clone(),
            vec![
                solve::SolveValue::real(arithmetic_profile(), 1.0).kind(),
                solve::SolveValue::real(arithmetic_profile(), 2.0).kind(),
            ],
        )
        .unwrap();
        let result = rumoca_eval_solve::eval_pure_call_checked(&table, owner.id(), &[input]);
        if index <= 0 {
            let reference = expression_call(&model, true);
            let reference_owner = &reference.owners().last().unwrap();
            let input = rumoca_eval_solve::TypedValue::construct(
                reference_owner.inputs()[0].clone(),
                vec![
                    solve::SolveValueKind::Real64(1.0f64.to_bits()),
                    solve::SolveValueKind::Real64(2.0f64.to_bits()),
                ],
            )
            .unwrap();
            let retained = rumoca_eval_solve::eval_pure_call_checked(
                &reference,
                reference_owner.id(),
                &[input],
            )
            .unwrap_err();
            assert!(
                matches!(retained,rumoca_eval_solve::TypedProgramEvalError::TensorIndexOutOfBounds { axis:1,index:bad,extent:2,.. } if bad==index)
            );
            assert_eq!(
                result.unwrap_err(),
                retained,
                "exact fault and provenance must match retained evaluation"
            );
        } else {
            assert_eq!(
                result.unwrap()[0].elements()[0],
                solve::SolveValueKind::Real64(0)
            );
        }
    }
}

fn mixed_integer_factor(negate: bool, literal: bool, value: i64) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("mixed_integer_zero.mo", "0 * (1 * (i + 1))               ");
    let at = |offset| {
        dae::DaeProvenance::source(Span::from_offsets(source, offset, offset + 1)).unwrap()
    };
    dae::Dae::construct(sources, |model| {
        let integer = model.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at(8))
        })?;
        let coordinate = if literal {
            None
        } else {
            Some(model.variables(|variables| {
                variables.parameter(
                    VarName::new("i"),
                    integer,
                    at(8),
                    dae::VariableAttributes {
                        is_tunable: true,
                        ..Default::default()
                    },
                )
            })?)
        };
        model.expressions(|e| {
            let i = match coordinate {
                Some(id) => e
                    .at(at(8))
                    .coordinate(dae::CoordinateInput::Parameter(id))?,
                None => e.at(at(8)).literal(dae::DaeLiteral::Integer(value))?,
            };
            let value = if negate {
                e.at(at(10)).unary(dae::UnaryOperator::Negate, i)?
            } else {
                let one = e.at(at(12)).literal(dae::DaeLiteral::Integer(1))?;
                e.at(at(10)).binary(dae::BinaryOperator::Add, i, one)?
            };
            let one = e.at(at(4)).literal(dae::DaeLiteral::Real(1.0))?;
            let factor = e
                .at(at(2))
                .binary(dae::BinaryOperator::Multiply, one, value)?;
            let zero = e.at(at(0)).literal(dae::DaeLiteral::Real(0.0))?;
            e.at(at(0))
                .binary(dae::BinaryOperator::Multiply, zero, factor)?;
            Ok(())
        })
    })
    .unwrap()
}

fn integer_input(
    table: &solve::SolvePureCallTable,
    value: i64,
) -> Vec<rumoca_eval_solve::TypedValue> {
    table
        .owners()
        .last()
        .unwrap()
        .inputs()
        .iter()
        .map(|ty| {
            rumoca_eval_solve::TypedValue::construct(
                ty.clone(),
                vec![solve::SolveValueKind::Integer(value)],
            )
            .unwrap()
        })
        .collect()
}

#[test]
fn a_real_factor_cannot_hide_checked_integer_child_faults() {
    for (negate, literal, value) in [
        (false, false, i64::MAX),
        (false, true, i64::MAX),
        (true, false, i64::MIN),
        (true, true, i64::MIN),
    ] {
        let model = mixed_integer_factor(negate, literal, value);
        let candidate = expression_call(&model, false);
        let reference = expression_call(&model, true);
        let retained = rumoca_eval_solve::eval_pure_call_checked(
            &reference,
            reference.owners().last().unwrap().id(),
            &integer_input(&reference, value),
        )
        .unwrap_err();
        assert!(
            matches!(retained,rumoca_eval_solve::TypedProgramEvalError::IntegerArithmetic { operation,.. }
            if operation==if negate { "unary operation" } else { "binary operation" })
        );
        let actual = rumoca_eval_solve::eval_pure_call_checked(
            &candidate,
            candidate.owners().last().unwrap().id(),
            &integer_input(&candidate, value),
        )
        .unwrap_err();
        assert_eq!(
            actual, retained,
            "the checked Integer fault and original provenance must survive Real promotion and zero"
        );
    }
}

fn expression_root<'dae>(view: dae::DaeView<'dae>, reference: bool) -> dae::ExprId<'dae> {
    let product = view.expression_id(view.expression_count() - 1).unwrap();
    if !reference {
        return product;
    }
    match view.expression(product).unwrap().operation() {
        dae::ExpressionOperation::Binary { rhs, .. } => rhs,
        _ => panic!("the product fixture must have a retained right-hand factor"),
    }
}
