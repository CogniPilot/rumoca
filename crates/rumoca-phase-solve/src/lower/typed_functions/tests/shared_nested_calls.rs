use super::*;

fn guarded_square<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    model
        .function(
            dae::FunctionSignature::new(VarName::new("square"), [real], [real], at),
            |model, reservation| {
                let input =
                    model.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
                let output =
                    model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
                let (value, condition, message) = model.expressions(|e| {
                    let input = e.at(at).function_parameter(input)?;
                    let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
                    Ok((
                        e.at(at)
                            .binary(dae::BinaryOperator::Multiply, input, input)?,
                        e.at(at).binary(dae::BinaryOperator::Greater, input, zero)?,
                        e.at(at)
                            .literal(dae::DaeLiteral::String("positive".into()))?,
                    ))
                })?;
                let mut body = model.functions(|f| f.begin(reservation, at))?;
                model.functions(|f| {
                    f.assertion(&mut body, condition, message, at)?;
                    f.assign(&mut body, output, value, at)?;
                    f.define(body, at)
                })
            },
        )
        .map(|(function, ())| function)
}

fn compose_calls<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
    child: dae::FunctionId<'dae>,
    scale: bool,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let name = VarName::new(format!("parent_{}", child.index()));
    model
        .function(
            dae::FunctionSignature::new(name, [real], [real], at),
            |model, reservation| {
                let input =
                    model.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
                let output =
                    model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
                let value = model.expressions(|e| {
                    let input = e.at(at).function_parameter(input)?;
                    let constant =
                        e.at(at)
                            .literal(dae::DaeLiteral::Real(if scale { 2.0 } else { 1.0 }))?;
                    let operator = if scale {
                        dae::BinaryOperator::Multiply
                    } else {
                        dae::BinaryOperator::Add
                    };
                    let other = e.at(at).binary(operator, input, constant)?;
                    let first = e.at(at).call(child, 0, [input])?;
                    let second = e.at(at).call(child, 0, [other])?;
                    e.at(at).binary(dae::BinaryOperator::Add, first, second)
                })?;
                let mut body = model.functions(|f| f.begin(reservation, at))?;
                model.functions(|f| {
                    f.assign(&mut body, output, value, at)?;
                    f.define(body, at)
                })
            },
        )
        .map(|(function, ())| function)
}

fn call_dag(depth: usize) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("shared_nested_calls.mo", "function square parent root");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 8)).unwrap();
    dae::Dae::construct(sources, |model| {
        let real = model.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let mut function = guarded_square(model, real, at)?;
        for level in 0..depth {
            function = compose_calls(model, real, at, function, level != 0)?;
        }
        model.expressions(|e| {
            let input = e.at(at).literal(dae::DaeLiteral::Real(3.0))?;
            e.at(at).call(function, 0, [input])
        })?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn exact_nested_call_owners_grow_with_source_dag_not_invocation_tree() {
    for depth in [1, 2, 4] {
        let table = lower_root_call(&call_dag(depth));
        assert_eq!(table.owners().len(), 1 + 2 * depth, "depth {depth}");
        let root = table.owners().last().unwrap();
        assert_eq!(
            root.outputs().len(),
            1 + (1 << depth),
            "every invocation retains its predicate"
        );
    }
}

fn real_value(value: f64) -> rumoca_eval_solve::TypedValue {
    rumoca_eval_solve::TypedValue::construct(
        solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic_profile())),
        vec![solve::SolveValueKind::Real64(value.to_bits())],
    )
    .unwrap()
}

#[test]
fn shared_nested_calls_preserve_distinct_arguments_directions_and_assertions() {
    let table = lower_root_call(&call_dag(2));
    let root = table.owners().last().unwrap();
    for x in [-0.75_f64, -0.25, 2.0] {
        let values =
            rumoca_eval_solve::eval_pure_call(&table, root.id(), &[real_value(x)]).unwrap();
        assert_eq!(
            values[0].elements(),
            &[solve::SolveValueKind::Real64(
                (10.0 * x * x + 6.0 * x + 2.0).to_bits()
            )]
        );
        let arguments = [x, x + 1.0, 2.0 * x, 2.0 * x + 1.0];
        for (predicate, argument) in values[1..].iter().zip(arguments) {
            assert_eq!(
                predicate.elements(),
                &[solve::SolveValueKind::Boolean(argument > 0.0)]
            );
        }
        for seed in [1.0, -0.5] {
            let values = rumoca_eval_solve::eval_pure_call_directional(
                &table,
                root.id(),
                &[real_value(x), real_value(seed)],
            )
            .unwrap();
            assert_eq!(
                values[1].elements(),
                &[solve::SolveValueKind::Real64(
                    ((20.0 * x + 6.0) * seed).to_bits()
                )]
            );
            for (predicate, argument) in values[2..].iter().zip(arguments) {
                assert_eq!(
                    predicate.elements(),
                    &[solve::SolveValueKind::Boolean(argument > 0.0)]
                );
            }
        }
    }
}

#[test]
fn finishing_a_call_table_discards_its_source_registration_scope() {
    call_dag(2).inspect(|view| {
        let call = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|expression| {
                view.expression(*expression).is_some_and(|node| {
                    node.function_scope().is_none()
                        && matches!(node.operation(), dae::ExpressionOperation::Call { .. })
                })
            })
            .unwrap();
        let mut registry = PureCallRegistry::new();
        for _ in 0..2 {
            let registered = registry.register_root(view, call).unwrap();
            assert_eq!(
                registry.register_root(view, call).unwrap().owner,
                registered.owner
            );
            let table = registry.finish();
            assert_eq!(table.owners().len(), 5);
            let values =
                rumoca_eval_solve::eval_pure_call(&table, registered.owner, &[real_value(2.0)])
                    .unwrap();
            assert_eq!(
                values[0].elements(),
                &[solve::SolveValueKind::Real64(54.0_f64.to_bits())]
            );
        }
    });
}
