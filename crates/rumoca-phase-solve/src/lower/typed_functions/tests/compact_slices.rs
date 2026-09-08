//! End-to-end DAE-to-typed-Solve evidence for compact slice concatenation.

use super::fixtures::lower_root_call;
use super::*;

#[test]
fn checked_dae_column_slices_lower_without_scalar_reconstruction() {
    let model = compact_column_function();
    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one exact call owner expected")
    };
    let operations = owner.body().operations();
    let origins = operations
        .iter()
        .filter_map(|operation| match operation.operation() {
            solve::SolveOperation::ProjectSlice { origin, .. } => Some(origin.as_ref()),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(origins, [&[3][..], &[0][..]]);
    assert_eq!(
        operations
            .iter()
            .filter(|operation| matches!(
                operation.operation(),
                solve::SolveOperation::Concatenate {
                    axis: 1,
                    operands,
                    ..
                } if operands.len() == 2
            ))
            .count(),
        1
    );
    assert!(operations.iter().all(|operation| !matches!(
        operation.operation(),
        solve::SolveOperation::ProjectElement { .. }
            | solve::SolveOperation::ConstructAggregate { .. }
    )));

    let argument_type =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(arithmetic_profile()), vec![10])
            .unwrap();
    let argument = rumoca_eval_solve::TypedValue::construct(
        argument_type,
        [
            10.0_f64, 11.0, 12.0, 20.0, 21.0, 22.0, -7.0, 31.0, 0.25, -0.5,
        ]
        .map(|value| solve::SolveValueKind::Real64(value.to_bits()))
        .to_vec(),
    )
    .unwrap();
    let output = rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[argument]).unwrap();
    let values = output[0]
        .elements()
        .iter()
        .map(|value| match value {
            solve::SolveValueKind::Real64(bits) => f64::from_bits(*bits),
            _ => panic!("the compact result is Real"),
        })
        .collect::<Vec<_>>();
    assert_eq!(values, [20.0, 10.0, 21.0, 11.0, 22.0, 12.0]);
}

fn compact_column_function() -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "typed_compact_columns.mo",
        "function compact input Real x[10]; output Real p[3,2]; end compact;",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 8)).unwrap();
    dae::Dae::construct(sources, |model| {
        let (vector, matrix) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [10]), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [3, 2]), at)?,
            ))
        })?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("compact"), [vector], [matrix], at),
            |model, reservation| {
                let (input, output) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("x"), 0, at)?,
                        functions.output(&reservation, VarName::new("p"), 0, at)?,
                    ))
                })?;
                let input = model
                    .expressions(|expressions| expressions.at(at).function_parameter(input))?;
                let columns = model.expressions(|expressions| {
                    let (four, six, one, three) = (
                        expressions.at(at).literal(dae::DaeLiteral::Integer(4))?,
                        expressions.at(at).literal(dae::DaeLiteral::Integer(6))?,
                        expressions.at(at).literal(dae::DaeLiteral::Integer(1))?,
                        expressions.at(at).literal(dae::DaeLiteral::Integer(3))?,
                    );
                    let upper_range = expressions.at(at).range(four, None, six)?;
                    let lower_range = expressions.at(at).range(one, None, three)?;
                    let upper = expressions.at(at).index(
                        input,
                        [dae::Subscript::Slice {
                            expression: upper_range,
                            provenance: at,
                        }],
                    )?;
                    let lower = expressions.at(at).index(
                        input,
                        [dae::Subscript::Slice {
                            expression: lower_range,
                            provenance: at,
                        }],
                    )?;
                    expressions
                        .at(at)
                        .builtin(dae::PureBuiltin::PromotedCat2, [upper, lower])
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| {
                    functions.assign(&mut body, output, columns, at)?;
                    functions.define(body, at)
                })
            },
        )?;
        let argument = model.expressions(|expressions| {
            let values = [10.0, 11.0, 12.0, 20.0, 21.0, 22.0, -7.0, 31.0, 0.25, -0.5]
                .into_iter()
                .map(|value| expressions.at(at).literal(dae::DaeLiteral::Real(value)))
                .collect::<Result<Vec<_>, _>>()?;
            expressions.at(at).array(values)
        })?;
        model.expressions(|expressions| expressions.at(at).call(function, 0, [argument]))?;
        Ok(())
    })
    .unwrap()
}
