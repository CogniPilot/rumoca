//! `integer(x)` floors a Real; `Integer(e)` is an enumeration's ordinal.
//! Both reach the typed lowerer as one builtin, so only a Real source may
//! convert.

use super::*;

#[test]
fn typed_integer_builtin_converts_only_real_sources() {
    let model = integer_conversion_fixture();
    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one source conversion owner");
    };
    let converts = owner
        .body()
        .operations()
        .iter()
        .filter(|op| matches!(op.operation(), solve::SolveOperation::Convert { .. }))
        .count();
    assert_eq!(converts, 1, "only the Real source floors");
    for (x, n, floored) in [(-1.5_f64, 7_i64, -2_i64), (2.0, -3, 2), (-0.0, 0, 0)] {
        let x_value = rumoca_eval_solve::TypedValue::construct(
            owner.inputs()[0].clone(),
            vec![solve::SolveValueKind::Real64(x.to_bits())],
        )
        .unwrap();
        let n_value = rumoca_eval_solve::TypedValue::construct(
            owner.inputs()[1].clone(),
            vec![solve::SolveValueKind::Integer(n)],
        )
        .unwrap();
        let results =
            rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[x_value, n_value]).unwrap();
        let outputs: Vec<_> = results
            .iter()
            .map(|result| result.elements().to_vec())
            .collect();
        assert_eq!(
            outputs,
            [
                vec![solve::SolveValueKind::Integer(floored)],
                vec![solve::SolveValueKind::Integer(n)],
                vec![solve::SolveValueKind::Integer(2)],
            ],
            "x {x}, n {n}"
        );
    }
}

fn integer_conversion_fixture() -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "typed_integer_conversion.mo",
        "yr := integer(x); yn := Integer(n); ye := Integer(E.b);",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 16)).unwrap();
    dae::Dae::construct(sources, |model| {
        let (real, integer) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)?,
            ))
        })?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(
                VarName::new("integerConversion"),
                [real, integer],
                [integer, integer, integer],
                at,
            ),
            |model, reservation| {
                let (x, n, yr, yn, ye) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("x"), 0, at)?,
                        functions.parameter(&reservation, VarName::new("n"), 1, at)?,
                        functions.output(&reservation, VarName::new("yr"), 0, at)?,
                        functions.output(&reservation, VarName::new("yn"), 1, at)?,
                        functions.output(&reservation, VarName::new("ye"), 2, at)?,
                    ))
                })?;
                let (floored, ordinal, literal) = model.expressions(|e| {
                    let x = e.at(at).function_parameter(x)?;
                    let n = e.at(at).function_parameter(n)?;
                    let b = e.at(at).enumeration_literal(2)?;
                    Ok((
                        e.at(at).builtin(dae::PureBuiltin::Integer, [x])?,
                        e.at(at).builtin(dae::PureBuiltin::Integer, [n])?,
                        e.at(at).builtin(dae::PureBuiltin::Integer, [b])?,
                    ))
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| {
                    functions.assign(&mut body, yr, floored, at)?;
                    functions.assign(&mut body, yn, ordinal, at)?;
                    functions.assign(&mut body, ye, literal, at)
                })?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        let (x, n) = model.variables(|variables| {
            Ok((
                variables.input(
                    VarName::new("x"),
                    real,
                    dae::InputVariability::Continuous,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.input(
                    VarName::new("n"),
                    integer,
                    dae::InputVariability::Discrete,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        model.expressions(|expressions| {
            let x = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Input(x))?;
            let n = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Input(n))?;
            expressions.at(at).call(function, 0, [x, n])?;
            Ok(())
        })
    })
    .unwrap()
}
