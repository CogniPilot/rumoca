use super::*;

fn flow_actions(site: solve::SolvePureCallSite) -> [solve::SolveEventAction; 2] {
    let first = action(site.clone(), solve::SolveAssertionLevel::Error);
    let mut second = action(site.clone(), solve::SolveAssertionLevel::Error);
    second.span = span(14);
    second.assertion_projection = solve::SolveAssertionActionProjection::new(site, 2);
    [first, second]
}

#[test]
fn zero_result_flow_preserves_selected_first_fault_and_repeated_call_authority() {
    let table = zero_result_flow_table(true, true);
    let site = table.owners()[0].call_site();
    let program = invocation_program(site.clone());
    let actions = flow_actions(site);
    let checked = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let compiled = compile_checked_assertion_wasm(checked).unwrap();
    let module = compiled.artifact().module_bytes().as_ptr();
    let mut execution = IssuedExecution::new(&compiled);
    for (argument, selected, source) in [(1, 0, 4), (0, 1, 14), (1, 0, 4)] {
        let CheckedAssertionWasmOutcome::Stopped { observations } =
            execution.call(&compiled, &[argument])
        else {
            panic!("selected assertion was erased or its later numerical fault won");
        };
        assert_eq!(observations.len(), 1);
        assert_eq!(observations[0].source_span(), span(source));
        assert!(std::ptr::eq(
            observations[0].action().action(),
            &actions[selected]
        ));
        assert_eq!(
            observations[0].frames()[0].arguments()[0].bits(),
            &[argument]
        );
        assert_eq!(compiled.artifact().module_bytes().as_ptr(), module);
    }
}

#[test]
fn zero_result_flow_does_not_execute_the_unselected_faulting_arm() {
    for (fail_true, fail_false, argument) in [(true, false, 0), (false, true, 1)] {
        let table = zero_result_flow_table(fail_true, fail_false);
        let site = table.owners()[0].call_site();
        let program = invocation_program(site.clone());
        let actions = flow_actions(site);
        let checked =
            solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
        let compiled = compile_checked_assertion_wasm(checked).unwrap();
        let mut execution = IssuedExecution::new(&compiled);
        let CheckedAssertionWasmOutcome::Complete {
            values,
            observations,
        } = execution.call(&compiled, &[argument])
        else {
            panic!("unselected source region executed its assertion or numerical fault");
        };
        assert_eq!(values[0].bits(), &[42]);
        assert!(observations.is_empty());
    }
}
