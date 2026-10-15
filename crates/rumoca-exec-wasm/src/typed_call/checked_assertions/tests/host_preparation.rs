use super::*;

#[test]
fn prepared_host_retains_exact_authority_and_first_fatal_capture() {
    let table = simple_table(solve::SolveAssertionLevel::Error, false, true);
    let site = table.owners()[0].call_site();
    let program = invocation_program(site.clone());
    let actions = [action(site, solve::SolveAssertionLevel::Error)];
    let checked = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let compiled = compile_checked_assertion_wasm(checked).unwrap();
    assert!(matches!(
        compiled.call(&[0]),
        Err(WasmCompileError::Input(_))
    ));
    let action_pointer = &compiled.invocation().projections()[0] as *const _;
    let bytes_pointer = compiled.artifact().module_bytes().as_ptr();
    let prepared = compiled.prepare_host().unwrap();
    assert!(std::ptr::eq(prepared.invocation().table(), &table));
    assert!(std::ptr::eq(
        prepared.invocation().program(),
        program.as_slice()
    ));
    assert!(std::ptr::eq(
        prepared.invocation().actions(),
        actions.as_slice()
    ));
    assert_eq!(prepared.artifact().module_bytes().as_ptr(), bytes_pointer);
    for _ in 0..2 {
        let CheckedAssertionWasmOutcome::Stopped { observations } = prepared.call(&[0]).unwrap()
        else {
            panic!("the first fatal assertion must stop before the later numerical fault");
        };
        assert_eq!(observations.len(), 1);
        assert_eq!(observations[0].source_span(), span(4));
        assert_eq!(
            observations[0].captures()[0].1.bits(),
            &[9_007_199_254_740_993]
        );
        assert_eq!(observations[0].frames()[0].arguments()[0].bits(), &[0]);
        assert!(std::ptr::eq(observations[0].action(), action_pointer));
        assert!(std::ptr::eq(observations[0].action().action(), &actions[0]));
    }
    let CheckedAssertionWasmOutcome::Fault {
        fault,
        observations,
    } = prepared.call(&[1]).unwrap()
    else {
        panic!("the inactive assertion must not replace a later numerical fault");
    };
    assert_eq!(fault.provenance, span(10));
    assert!(
        observations.is_empty(),
        "previous records must not be replayed"
    );
}

#[test]
fn prepared_host_loop_warning_captures_each_actual_child_frame() {
    let table = loop_table(solve::SolveAssertionLevel::Warning);
    let site = table.owners()[1].call_site();
    let program = invocation_program(site.clone());
    let actions = [action(site, solve::SolveAssertionLevel::Warning)];
    let invocation = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let prepared = compile_checked_assertion_wasm(invocation)
        .unwrap()
        .prepare_host()
        .unwrap();
    let CheckedAssertionWasmOutcome::Complete { observations, .. } = prepared.call(&[]).unwrap()
    else {
        panic!("warning loop must complete");
    };
    assert_eq!(observations.len(), 3);
    for (index, observation) in observations.iter().enumerate() {
        assert_eq!(observation.frames().len(), 2);
        assert_eq!(
            observation.frames()[0].domain_points(),
            &[vec![index as i64 + 1]]
        );
        assert_eq!(
            observation.frames()[1].arguments()[0].bits(),
            &[index as u64 + 1]
        );
        assert_eq!(observation.captures()[0].1.bits(), &[index as u64 + 1]);
    }
}

#[test]
fn prepared_host_forwarded_fatal_loop_stops_at_first_actual_iteration() {
    let table = loop_table(solve::SolveAssertionLevel::Error);
    let site = table.owners()[1].call_site();
    let program = invocation_program(site.clone());
    let actions = [action(site, solve::SolveAssertionLevel::Error)];
    let invocation = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let prepared = compile_checked_assertion_wasm(invocation)
        .unwrap()
        .prepare_host()
        .unwrap();
    let CheckedAssertionWasmOutcome::Stopped { observations } = prepared.call(&[]).unwrap() else {
        panic!("fatal child must stop its parent loop");
    };
    assert_eq!(observations.len(), 1);
    assert_eq!(observations[0].source_span(), span(24));
    assert_eq!(observations[0].frames()[0].domain_points(), &[vec![1]]);
    assert_eq!(observations[0].frames()[1].arguments()[0].bits(), &[1]);
}

#[test]
fn prepared_host_nested_conditional_preserves_selected_fatal_action() {
    let table = zero_result_flow_table(true, true);
    let site = table.owners()[0].call_site();
    let program = invocation_program(site.clone());
    let first = action(site.clone(), solve::SolveAssertionLevel::Error);
    let mut second = action(site.clone(), solve::SolveAssertionLevel::Error);
    second.span = span(14);
    second.assertion_projection = solve::SolveAssertionActionProjection::new(site, 2);
    let actions = [first, second];
    let invocation = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let prepared = compile_checked_assertion_wasm(invocation)
        .unwrap()
        .prepare_host()
        .unwrap();
    for (argument, selected, source) in [(1, 0, 4), (0, 1, 14), (1, 0, 4)] {
        let CheckedAssertionWasmOutcome::Stopped { observations } =
            prepared.call(&[argument]).unwrap()
        else {
            panic!("selected assertion must preserve first-fault order");
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
    }
}

#[test]
fn prepared_host_rejects_malformed_inputs_and_never_replays_stale_observations() {
    let table = simple_table(solve::SolveAssertionLevel::Error, false, false);
    let site = table.owners()[0].call_site();
    let program = invocation_program(site.clone());
    let actions = [action(site, solve::SolveAssertionLevel::Error)];
    let invocation = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let prepared = compile_checked_assertion_wasm(invocation)
        .unwrap()
        .prepare_host()
        .unwrap();
    assert!(
        matches!(prepared.call(&[]), Err(WasmCompileError::Input(message)) if message == "checked invocation input footprint differs")
    );
    assert!(matches!(
        prepared.call(&[0]).unwrap(),
        CheckedAssertionWasmOutcome::Stopped { .. }
    ));
    let CheckedAssertionWasmOutcome::Fault {
        fault,
        observations,
    } = prepared.call(&[2]).unwrap()
    else {
        panic!("invalid Boolean bits must not execute the assertion body");
    };
    assert_eq!(fault.kind, crate::TypedCallFaultKind::InvalidInput);
    assert!(observations.is_empty());
    let CheckedAssertionWasmOutcome::Complete {
        values,
        observations,
    } = prepared.call(&[1]).unwrap()
    else {
        panic!("a later valid call must execute normally");
    };
    assert_eq!(values[0].bits(), &[42]);
    assert!(observations.is_empty());
}

#[test]
fn prepared_host_fault_keeps_caller_output_cells_unpublished() {
    let table = simple_table(solve::SolveAssertionLevel::Warning, false, true);
    let site = table.owners()[0].call_site();
    let program = invocation_program(site.clone());
    let actions = [action(site, solve::SolveAssertionLevel::Warning)];
    let invocation = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let compiled = compile_checked_assertion_wasm(invocation).unwrap();
    let runtime =
        crate::host_typed_call_runtime::HostTypedCallRuntime::new(compiled.artifact()).unwrap();
    let layout = compiled.artifact().layout();
    let sentinel = vec![0x5a5a_5a5a_5a5a_5a5a; (layout.output_bytes / 8) as usize];
    let mut output = sentinel.clone();
    let mut scratch = vec![0; (layout.scratch_bytes / 8) as usize];
    let status = runtime.call_typed(&[0], &mut output, &mut scratch).unwrap();
    assert_ne!(status, 0);
    assert_eq!(output, sentinel);
    let CheckedAssertionWasmOutcome::Fault {
        fault,
        observations,
    } = compiled
        .decode_execution(&[0], &output, &scratch, status)
        .unwrap()
    else {
        panic!("warning must preserve the following numerical fault");
    };
    assert_eq!(fault.provenance, span(10));
    assert_eq!(observations.len(), 1);
}

#[test]
fn prepared_host_uses_shared_math_imports_for_failed_only_messages() {
    let table = math_message_table();
    let site = table.owners()[0].call_site();
    let program = invocation_program(site.clone());
    let actions = [action(site, solve::SolveAssertionLevel::Error)];
    let invocation = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let prepared = compile_checked_assertion_wasm(invocation)
        .unwrap()
        .prepare_host()
        .unwrap();
    let CheckedAssertionWasmOutcome::Stopped { observations } = prepared.call(&[0]).unwrap() else {
        panic!("failed-only math capture must execute before the fatal stop");
    };
    let captured = f64::from_bits(observations[0].captures()[0].1.bits()[0]);
    assert!((captured - 0.8414709848078965).abs() < 1e-15);
    let CheckedAssertionWasmOutcome::Complete {
        observations,
        values,
    } = prepared.call(&[1]).unwrap()
    else {
        panic!("passed assertion must skip its failed-only message");
    };
    assert!(observations.is_empty());
    assert_eq!(values[2].bits(), &[0.0f64.to_bits()]);
}
