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

#[test]
fn issued_fatal_forwarding_remains_refused_by_raw_native_interface() {
    let p = profile();
    let outputs = vec![solve::SolvePureCallOutput::assertion_predicate()];
    let mut builder = solve::SolvePureCallTable::builder(p);
    let child = builder
        .add_owner(
            identity(31),
            vec![],
            outputs.clone(),
            span(330),
            |b, _, outputs| {
                let condition = b.constant(solve::SolveValue::boolean(false), span(331))?;
                let assertion = b.assertion_output(0, span(332))?;
                b.check_assertion(assertion, condition, &[], span(333), |_, _, _| Ok(()))?;
                b.store(outputs[0], condition, span(334))
            },
        )
        .unwrap();
    let parent = builder
        .add_owner(identity(32), vec![], outputs, span(335), |b, _, outputs| {
            let call = b.emit_call(child, &[], span(336))?;
            let assertion = b.assertion_output(0, span(337))?;
            b.forward_assertion(&call, 0, assertion, span(338))?;
            b.store(outputs[0], call.registers()[0], span(339))
        })
        .unwrap();
    let table = builder.finish();
    let owner = table.owner(parent).unwrap();
    assert!(owner.assertion_flow().is_some());
    assert!(
        matches!(rumoca_eval_solve::eval_pure_call(&table, parent, &[]),
        Err(rumoca_eval_solve::TypedProgramEvalError::AssertionFailed { failure })
        if failure.owner() == child && failure.source_span() == span(333))
    );
    assert_eq!(
        compile_pure_call_wasm(&table, &owner.call_site()).unwrap_err(),
        TypedCallCompileError::UnsupportedOutputKind {
            owner: parent,
            output: 0,
            kind: solve::SolvePureCallOutputKind::AssertionPredicate,
            provenance: span(335),
        }
    );
}
