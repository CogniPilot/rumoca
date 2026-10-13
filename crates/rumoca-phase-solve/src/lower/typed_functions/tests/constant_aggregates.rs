//! A constant table read by a loop body is built once, outside the loop.

use super::*;
use rumoca_eval_solve::{TypedValue, eval_pure_call};

#[test]
fn a_constant_table_read_in_a_comprehension_is_built_once_outside_the_map() {
    let model = weighted_comprehension();
    let calls = lower_root_call(&model);
    let owner = calls.owners().last().unwrap();
    let body = owner.body();
    let maps = body
        .operations()
        .iter()
        .filter_map(|op| match op.operation() {
            solve::SolveOperation::Map { body, .. } => Some(body.body()),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(maps.len(), 1, "one comprehension map expected");
    assert_eq!(aggregate_constructions(body), 1);
    assert_eq!(
        aggregate_constructions(maps[0]),
        0,
        "the map body must capture the table instead of rebuilding it"
    );
    let input = TypedValue::construct(
        owner.inputs()[0].clone(),
        (1..=3)
            .map(|i| solve::SolveValueKind::Real64(f64::from(i).to_bits()))
            .collect(),
    )
    .unwrap();
    let expected = TypedValue::construct(
        owner.outputs()[0].value_type().clone(),
        [2.0_f64, 6.0, 15.0]
            .into_iter()
            .map(|value| solve::SolveValueKind::Real64(value.to_bits()))
            .collect(),
    )
    .unwrap();
    assert_eq!(
        eval_pure_call(&calls, owner.id(), std::slice::from_ref(&input)).unwrap(),
        std::slice::from_ref(&expected)
    );
}

#[test]
fn a_literal_initialising_a_carried_array_stays_a_private_value_the_loop_updates_in_place() {
    let model = scaled_carried_vector();
    let calls = lower_root_call(&model);
    let owner = calls.owners().last().unwrap();
    let body = owner.body();
    let (fold_index, initial, transition) = body
        .operations()
        .iter()
        .enumerate()
        .find_map(|(index, op)| match op.operation() {
            solve::SolveOperation::Fold {
                initial,
                transition,
                ..
            } => Some((index, initial.clone(), transition.body())),
            _ => None,
        })
        .expect("one fold expected");
    assert_eq!(initial.len(), 1);
    // The fold is the only reader of its initialiser and takes it over, so the
    // passes update it without a copy.
    assert!(
        body.register_moves_at(initial[0], fold_index),
        "the carried initialiser must be consumed by the fold"
    );
    // The indexed table is issued once in this scope beside the initialiser.
    assert_eq!(aggregate_constructions(body), 2);
    assert_eq!(aggregate_constructions(transition), 0);
}

/// Aggregate constructions held directly by `program` (not by nested regions).
fn aggregate_constructions(program: &solve::TypedProgram) -> usize {
    program
        .operations()
        .iter()
        .filter(|op| {
            matches!(
                op.operation(),
                solve::SolveOperation::ConstructAggregate { .. }
            )
        })
        .count()
}

/// `y := {table[i] * u[i] for i in 1:3}` with `table = {2, 3, 5}`.
fn weighted_comprehension() -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "weights.mo",
        "function weigh input Real u[3]; output Real y[3]; algorithm y := {{2.0,3.0,5.0}[i]*u[i] for i in 1:3}; end weigh;",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 10)).unwrap();
    dae::Dae::construct(sources, |model| {
        let vector = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at))?;
        let domain = model.domains(|domains| domains.structured(structured_range("i", 3), at))?;
        let binder = model.domains(|domains| domains.binder(domain, 0, at))?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("weigh"), [vector], [vector], at),
            |model, reservation| {
                let input =
                    model.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
                let output =
                    model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
                let weighted = model.expressions(|e| {
                    let literals = [2.0, 3.0, 5.0]
                        .into_iter()
                        .map(|value| e.at(at).literal(dae::DaeLiteral::Real(value)))
                        .collect::<Result<Vec<_>, _>>()?;
                    let table = e.at(at).array(literals)?;
                    let position = e.at(at).binder(binder)?;
                    let at_binder = || dae::Subscript::Index {
                        expression: position,
                        provenance: at,
                    };
                    let weight = e.at(at).index(table, [at_binder()])?;
                    let parameter = e.at(at).function_parameter(input)?;
                    let sample = e.at(at).index(parameter, [at_binder()])?;
                    let product = e
                        .at(at)
                        .binary(dae::BinaryOperator::Multiply, weight, sample)?;
                    e.at(at).comprehension(domain, product)
                })?;
                let mut body = model.functions(|f| f.begin(reservation, at))?;
                model.functions(|f| f.assign(&mut body, output, weighted, at))?;
                model.functions(|f| f.define(body, at))
            },
        )?;
        let variable = model.variables(|v| {
            v.algebraic(
                VarName::new("x"),
                vector,
                at,
                dae::VariableAttributes::default(),
            )
        })?;
        model.expressions(|e| {
            let value = e
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(variable))?;
            e.at(at).call(function, 0, [value])
        })?;
        Ok(())
    })
    .unwrap()
}

/// `y := {0, 0, 0}; for i in 1:3 loop y := y + {2, 3, 5}[i] * y; end for`.
fn scaled_carried_vector() -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "carried.mo",
        "function scale output Real y[3]; algorithm y := {0.0,0.0,0.0}; for i in 1:3 loop y := y + {2.0,3.0,5.0}[i]*y; end for; end scale;",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 10)).unwrap();
    dae::Dae::construct(sources, |model| {
        let vector = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at))?;
        let domain = model.domains(|domains| domains.structured(structured_range("i", 3), at))?;
        let binder = model.domains(|domains| domains.binder(domain, 0, at))?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("scale"), [], [vector], at),
            |model, reservation| {
                let output =
                    model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
                let initial = model.expressions(|e| {
                    let zeros = (0..3)
                        .map(|_| e.at(at).literal(dae::DaeLiteral::Real(0.0)))
                        .collect::<Result<Vec<_>, _>>()?;
                    e.at(at).array(zeros)
                })?;
                let mut body = model.functions(|f| f.begin(reservation, at))?;
                model.functions(|f| f.assign(&mut body, output, initial, at))?;
                let mut looped = model.functions(|f| f.begin_loop(body, domain, [output], at))?;
                let carried = model.functions(|f| f.read(looped.body(), output, at))?;
                let updated = model.expressions(|e| {
                    let table = [2.0, 3.0, 5.0]
                        .into_iter()
                        .map(|value| e.at(at).literal(dae::DaeLiteral::Real(value)))
                        .collect::<Result<Vec<_>, _>>()?;
                    let table = e.at(at).array(table)?;
                    let position = e.at(at).binder(binder)?;
                    let weight = e.at(at).index(
                        table,
                        [dae::Subscript::Index {
                            expression: position,
                            provenance: at,
                        }],
                    )?;
                    let scaled = e
                        .at(at)
                        .binary(dae::BinaryOperator::Multiply, weight, carried)?;
                    e.at(at).binary(dae::BinaryOperator::Add, carried, scaled)
                })?;
                model.functions(|f| f.assign_loop(&mut looped, output, updated, at))?;
                let body = model.functions(|f| f.finish_loop(looped, at))?;
                model.functions(|f| f.define(body, at))
            },
        )?;
        model.expressions(|e| e.at(at).call(function, 0, []))?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn one_literal_read_by_the_loop_and_entering_it_as_the_carried_array_stays_private() {
    let model = shared_literal_carried_vector();
    let calls = lower_root_call(&model);
    let owner = calls.owners().last().unwrap();
    let body = owner.body();
    let (fold_index, initial, transition) = body
        .operations()
        .iter()
        .enumerate()
        .find_map(|(index, op)| match op.operation() {
            solve::SolveOperation::Fold {
                initial,
                transition,
                ..
            } => Some((index, initial.clone(), transition.body())),
            _ => None,
        })
        .expect("one fold expected");
    // The literal is the entry value of the carried array, so the fold takes
    // it over and updates it in place instead of copying a shared capture.
    assert!(body.register_moves_at(initial[0], fold_index));
    assert_eq!(aggregate_constructions(body), 1);
    // The loop's own reads of the same literal are built where they are read.
    assert_eq!(aggregate_constructions(transition), 1);
}

/// `T = {2, 3, 5}; y := T; for i in 1:3 loop y := y + T[i] * y; end for`, with
/// the one literal `T` read by the loop and entering it as the carried array.
fn shared_literal_carried_vector() -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "shared.mo",
        "function scale output Real y[3]; algorithm y := {2.0,3.0,5.0}; for i in 1:3 loop y := y + {2.0,3.0,5.0}[i]*y; end for; end scale;",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 10)).unwrap();
    dae::Dae::construct(sources, |model| {
        let vector = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at))?;
        let domain = model.domains(|domains| domains.structured(structured_range("i", 3), at))?;
        let binder = model.domains(|domains| domains.binder(domain, 0, at))?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("scale"), [], [vector], at),
            |model, reservation| {
                let output =
                    model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
                let table = model.expressions(|e| {
                    let values = [2.0, 3.0, 5.0]
                        .into_iter()
                        .map(|value| e.at(at).literal(dae::DaeLiteral::Real(value)))
                        .collect::<Result<Vec<_>, _>>()?;
                    e.at(at).array(values)
                })?;
                let mut body = model.functions(|f| f.begin(reservation, at))?;
                model.functions(|f| f.assign(&mut body, output, table, at))?;
                let mut looped = model.functions(|f| f.begin_loop(body, domain, [output], at))?;
                let carried = model.functions(|f| f.read(looped.body(), output, at))?;
                let updated = model.expressions(|e| {
                    let position = e.at(at).binder(binder)?;
                    let weight = e.at(at).index(
                        table,
                        [dae::Subscript::Index {
                            expression: position,
                            provenance: at,
                        }],
                    )?;
                    let scaled = e
                        .at(at)
                        .binary(dae::BinaryOperator::Multiply, weight, carried)?;
                    e.at(at).binary(dae::BinaryOperator::Add, carried, scaled)
                })?;
                model.functions(|f| f.assign_loop(&mut looped, output, updated, at))?;
                let body = model.functions(|f| f.finish_loop(looped, at))?;
                model.functions(|f| f.define(body, at))
            },
        )?;
        model.expressions(|e| e.at(at).call(function, 0, []))?;
        Ok(())
    })
    .unwrap()
}
