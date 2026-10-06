//! Function-body capabilities must brand quotients even without parameter reads.
use super::*;

fn quotient_body(builtin: PureBuiltin, in_loop: bool) -> Dae {
    let source = TestSource::new("function f output Integer y; algorithm y := div(k, 2); end f;");
    let at = source.source("div(k, 2)", 0);
    Dae::construct(source.map, |dae| {
        let integer =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Integer), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [integer], at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                let (zero, two) = dae.expressions(|expressions| {
                    Ok((
                        expressions.at(at).literal(DaeLiteral::Integer(0))?,
                        expressions.at(at).literal(DaeLiteral::Integer(2))?,
                    ))
                })?;
                if !in_loop {
                    let quotient =
                        dae.function_runtime_quotient(&body, builtin, [zero, two], at)?;
                    dae.functions(|functions| functions.assign(&mut body, output, quotient, at))?;
                    return dae.functions(|functions| functions.define(body, at));
                }
                dae.functions(|functions| functions.assign(&mut body, output, zero, at))?;
                let domain = dae.domains(|domains| {
                    domains.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                id: 0,
                                display_name: "k".into(),
                                lower: -3,
                                upper: 3,
                                step: 1,
                            }],
                        },
                        at,
                    )
                })?;
                let mut loop_body =
                    dae.functions(|functions| functions.begin_loop(body, domain, [output], at))?;
                let index = dae.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binder(DomainBinderId::from_raw(domain.index(), 0))
                })?;
                let quotient =
                    dae.function_runtime_quotient(loop_body.body(), builtin, [index, two], at)?;
                dae.functions(|functions| {
                    functions.assign_loop(&mut loop_body, output, quotient, at)
                })?;
                let body = dae.functions(|functions| functions.finish_loop(loop_body, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .expect("the body capability admits its exact operands")
}

fn check_roundtrip(dae: Dae) {
    dae.inspect(|view| assert_eq!(view.root_count(), 0));
    let encoded = serde_json::to_string(&dae).expect("owned function quotient must serialize");
    let decoded: Dae = serde_json::from_str(&encoded).expect("checked owner must replay");
    assert_eq!(serde_json::to_string(&decoded).unwrap(), encoded);
}

#[test]
fn function_quotient_binder_only_owners_roundtrip() {
    for builtin in [PureBuiltin::Div, PureBuiltin::Mod, PureBuiltin::Rem] {
        check_roundtrip(quotient_body(builtin, true));
    }
}

#[test]
fn function_quotient_literal_only_owners_roundtrip() {
    for builtin in [PureBuiltin::Div, PureBuiltin::Mod, PureBuiltin::Rem] {
        check_roundtrip(quotient_body(builtin, false));
    }
}
