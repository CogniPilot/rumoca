//! A conditional that writes one element of an array selects the element value.

use super::*;
use rumoca_eval_solve::{TypedValue, eval_pure_call};

#[test]
fn conditional_element_writes_select_the_value_and_write_the_array_once() {
    let model = conditional_element_writes();
    let calls = lower_root_call(&model);
    let owner = calls.owners().last().unwrap();
    assert_eq!(
        conditional_count(owner.body()),
        0,
        "arms writing the same element lower as a selection, not a region"
    );
    for (guard, expected) in [(1.0_f64, [1.0_f64; 3]), (-1.0, [2.0; 3])] {
        let input = TypedValue::construct(
            owner.inputs()[0].clone(),
            vec![solve::SolveValueKind::Real64(guard.to_bits())],
        )
        .unwrap();
        let output = eval_pure_call(&calls, owner.id(), std::slice::from_ref(&input)).unwrap();
        let expected = TypedValue::construct(
            owner.outputs()[0].value_type().clone(),
            expected
                .into_iter()
                .map(|value| solve::SolveValueKind::Real64(value.to_bits()))
                .collect(),
        )
        .unwrap();
        assert_eq!(output, std::slice::from_ref(&expected));
    }
}

/// `y := {0, 0, 0}; for i in 1:3 loop if u > 0 then y[i] := 1 else y[i] := 2; end for`.
fn conditional_element_writes() -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "writes.mo",
        "function pick input Real u; output Real y[3]; algorithm y := {0.0,0.0,0.0}; for i in 1:3 loop if u > 0 then y[i] := 1.0; else y[i] := 2.0; end if; end for; end pick;",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 10)).unwrap();
    dae::Dae::construct(sources, |model| {
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let vector = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at))?;
        let domain = model.domains(|domains| domains.structured(structured_range("i", 3), at))?;
        let binder = model.domains(|domains| domains.binder(domain, 0, at))?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("pick"), [real], [vector], at),
            |model, reservation| {
                let input =
                    model.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
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
                let (condition, then_write, else_write) = model.expressions(|e| {
                    let parameter = e.at(at).function_parameter(input)?;
                    let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
                    let condition =
                        e.at(at)
                            .binary(dae::BinaryOperator::Greater, parameter, zero)?;
                    let position = e.at(at).binder(binder)?;
                    let at_binder = || dae::Subscript::Index {
                        expression: position,
                        provenance: at,
                    };
                    let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
                    let two = e.at(at).literal(dae::DaeLiteral::Real(2.0))?;
                    Ok((
                        condition,
                        e.at(at).array_update(carried, one, [at_binder()])?,
                        e.at(at).array_update(carried, two, [at_binder()])?,
                    ))
                })?;
                looped = model.functions(|f| {
                    crate::tests::scoped_assignment_loop(
                        f,
                        looped,
                        &[output],
                        &[condition],
                        &[vec![then_write]],
                        &[else_write],
                        at,
                    )
                })?;
                let body = model.functions(|f| f.finish_loop(looped, at))?;
                model.functions(|f| f.define(body, at))
            },
        )?;
        let argument = model.expressions(|e| e.at(at).literal(dae::DaeLiteral::Real(1.0)))?;
        model.expressions(|e| e.at(at).call(function, 0, [argument]))?;
        Ok(())
    })
    .unwrap()
}
