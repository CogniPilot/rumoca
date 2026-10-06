//! Predicate interfaces must not masquerade as a successful Boolean result.
use super::*;

fn output_owner(assertion: bool, predicate: bool) -> solve::SolvePureCallTable {
    let p = profile();
    let mut table = solve::SolvePureCallTable::builder(p);
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    table
        .add_owner(
            identity(30),
            vec![],
            vec![
                solve::SolvePureCallOutput::result(integer),
                if assertion {
                    solve::SolvePureCallOutput::assertion_predicate()
                } else {
                    solve::SolvePureCallOutput::result(boolean)
                },
            ],
            span(320),
            |b, _, outputs| {
                let value = b.constant(solve::SolveValue::integer(p, 7).unwrap(), span(321))?;
                let predicate = b.constant(solve::SolveValue::boolean(predicate), span(322))?;
                b.store(outputs[0], value, span(323))?;
                b.store(outputs[1], predicate, span(324))
            },
        )
        .unwrap();
    table.finish()
}

#[test]
fn source_assertion_output_kind_is_refused_at_exact_interface_provenance() {
    for predicate in [false, true] {
        let table = output_owner(true, predicate);
        let owner = &table.owners()[0];
        let site = owner.call_site();
        // Raw canonical payload success does not execute a source assertion.
        assert_eq!(
            oracle(&table, &site, &[]).unwrap(),
            cells([
                solve::SolveValueKind::Integer(7),
                solve::SolveValueKind::Boolean(predicate)
            ])
        );
        assert_eq!(
            compile_pure_call_wasm(&table, &site).unwrap_err(),
            TypedCallCompileError::UnsupportedOutputKind {
                owner: owner.id(),
                output: 1,
                kind: solve::SolvePureCallOutputKind::AssertionPredicate,
                provenance: span(320),
            }
        );
    }
}

#[test]
fn ordinary_false_boolean_result_still_executes_as_a_complete_tuple() {
    let table = output_owner(false, false);
    let site = table.owners()[0].call_site();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let actual = Runner::new(&compiled).run(&[]);
    assert_eq!(actual, (0, oracle(&table, &site, &[]).unwrap()));
}
