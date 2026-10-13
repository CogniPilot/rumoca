use super::*;

fn assigned_child(
    table: &mut solve::SolvePureCallTableBuilder,
    later_fault: bool,
) -> solve::SolvePureCallOwnerId {
    let p = profile();
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    table
        .add_owner(
            identity(230),
            vec![solve::SolveValueType::scalar(
                solve::SolveScalarType::Boolean,
            )],
            vec![
                solve::SolvePureCallOutput::result(real.clone()),
                solve::SolvePureCallOutput::assertion_predicate(),
            ],
            span(1000),
            |builder, inputs, outputs| {
                let assigned = builder.load(inputs[0], span(1001))?;
                let (_, states) = builder.conditional_assigned(
                    assigned,
                    (&[], &[]),
                    (vec![], vec![real.clone()]),
                    span(1002),
                    |branch, _, _, _| {
                        let value = branch.constant(solve::SolveValue::real(p, 7.0), span(1003))?;
                        let state = branch.present_assigned_state(value, span(1004))?;
                        branch.store_assigned_state(0, state, span(1005))
                    },
                    |branch, _, _, _| {
                        let state = branch.absent_assigned_state(real.clone(), span(1006))?;
                        branch.store_assigned_state(0, state, span(1007))
                    },
                )?;
                let assertion = builder.assertion_output(1, span(1008))?;
                let (condition, values) =
                    builder.require_assigned_states(assertion, &states, span(1009))?;
                if later_fault {
                    let one =
                        builder.constant(solve::SolveValue::integer(p, 1).unwrap(), span(1010))?;
                    let zero =
                        builder.constant(solve::SolveValue::integer(p, 0).unwrap(), span(1011))?;
                    builder.binary(
                        solve::SolveBinaryOperator::IntegerQuotient,
                        one,
                        zero,
                        span(1012),
                    )?;
                }
                builder.store(outputs[0], values[0], span(1013))?;
                builder.store(outputs[1], condition, span(1014))
            },
        )
        .unwrap()
}

fn assigned_table(
    parent: bool,
    later_fault: bool,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let mut builder = solve::SolvePureCallTable::builder(profile());
    let child = assigned_child(&mut builder, later_fault);
    let owner = if parent {
        assigned_parent(&mut builder, child)
    } else {
        child
    };
    let site = builder.call_site(owner).unwrap();
    (builder.finish(), site)
}

fn assigned_parent(
    table: &mut solve::SolvePureCallTableBuilder,
    child: solve::SolvePureCallOwnerId,
) -> solve::SolvePureCallOwnerId {
    let p = profile();
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    table
        .add_owner(
            identity(231),
            vec![boolean.clone(), boolean],
            table.call_site(child).unwrap().outputs().to_vec(),
            span(1020),
            |builder, inputs, outputs| {
                let selected = builder.load(inputs[0], span(1021))?;
                let assigned = builder.load(inputs[1], span(1022))?;
                let values = builder.conditional(
                    selected,
                    &[assigned],
                    vec![real],
                    span(1023),
                    |branch, captures, outputs| {
                        let assigned = branch.load(captures[0], span(1024))?;
                        let call = branch.emit_call(child, &[assigned], span(1025))?;
                        let assertion = branch.assertion_output(1, span(1026))?;
                        branch.forward_assertion(&call, 1, assertion, span(1027))?;
                        branch.store(outputs[0], call.registers()[0], span(1028))
                    },
                    |branch, _, outputs| {
                        let value = branch.constant(solve::SolveValue::real(p, 0.0), span(1029))?;
                        branch.store(outputs[0], value, span(1030))
                    },
                )?;
                builder.store(outputs[0], values[0], span(1031))?;
                let inactive = builder.constant(solve::SolveValue::boolean(true), span(1032))?;
                builder.store(outputs[1], inactive, span(1033))
            },
        )
        .unwrap()
}

fn expect_fault(
    compiled: &CompiledTypedCallWasm,
    status: i32,
    owner: solve::SolvePureCallOwnerId,
    provenance: Span,
) {
    assert!(status > 0);
    let fault = compiled
        .faults()
        .iter()
        .find(|fault| fault.status == status as u32)
        .unwrap();
    assert_eq!(fault.owner, owner);
    assert_eq!(fault.provenance, provenance);
    assert_eq!(fault.kind, TypedCallFaultKind::AssertionFailed);
}

#[test]
fn normal_assigned_read_stops_before_projection_and_reuses_clean_input_state() {
    let (table, site) = assigned_table(false, false);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let original_module = compiled.module_bytes().as_ptr();
    let mut runner = Runner::new(&compiled);
    for assigned in [true, false, true, false, true] {
        let input = solve::SolveValueKind::Boolean(assigned);
        let (status, actual) = runner.run(&cells([input]));
        if assigned {
            assert_eq!(status, 0);
            assert_eq!(actual, oracle(&table, &site, &[vec![input]]).unwrap());
        } else {
            assert!(matches!(
                oracle(&table, &site, &[vec![input]]),
                Err(rumoca_eval_solve::TypedProgramEvalError::AssertionFailed { failure })
                if failure.owner() == site.owner() && failure.source_span() == span(1009)
            ));
            expect_fault(&compiled, status, site.owner(), span(1009));
            assert!(actual.iter().all(|byte| *byte == 0xa5));
        }
        assert_eq!(compiled.module_bytes().as_ptr(), original_module);
    }
}

#[test]
fn normal_assigned_read_child_retains_source_fault_and_unselected_arm_is_inactive() {
    let (table, site) = assigned_table(true, false);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    for (selected, assigned) in [
        (false, false),
        (true, true),
        (true, false),
        (false, false),
        (true, true),
    ] {
        let inputs = [
            solve::SolveValueKind::Boolean(selected),
            solve::SolveValueKind::Boolean(assigned),
        ];
        let (status, actual) = runner.run(&cells(inputs));
        if selected && !assigned {
            expect_fault(&compiled, status, table.owners()[0].id(), span(1009));
            assert!(actual.iter().all(|byte| *byte == 0xa5));
        } else {
            assert_eq!(status, 0);
            assert_eq!(
                actual,
                oracle(&table, &site, &inputs.map(|value| vec![value])).unwrap()
            );
        }
    }
}

#[test]
fn normal_assigned_read_failure_precedes_later_arithmetic_fault() {
    let (table, site) = assigned_table(false, true);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    for assigned in [false, true, false] {
        let (status, actual) = runner.run(&cells([solve::SolveValueKind::Boolean(assigned)]));
        assert!(actual.iter().all(|byte| *byte == 0xa5));
        if !assigned {
            expect_fault(&compiled, status, site.owner(), span(1009));
        } else {
            let fault = compiled
                .faults()
                .iter()
                .find(|fault| fault.status == status as u32)
                .unwrap();
            assert_eq!(fault.provenance, span(1012));
            assert_eq!(fault.kind, TypedCallFaultKind::IntegerArithmetic);
        }
    }
}
