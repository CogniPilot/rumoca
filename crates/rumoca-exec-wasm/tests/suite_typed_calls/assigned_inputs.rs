//! A region copies an assigned-state capture into its frame only when one of
//! its operations reads that state.
use super::*;

/// The sum of `real(i)` for `i in 1:last`, plus one for each pass when the
/// body reads the presence of the captured state. The fold always captures
/// the state when `capture` is set; only `read` decides whether it uses it.
fn state_fold(
    capture: bool,
    read: bool,
    last: i64,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(95),
            vec![],
            vec![solve::SolvePureCallOutput::result(real.clone())],
            span(1100),
            |b, _, outputs| {
                let seven = b.constant(solve::SolveValue::real(p, 7.0), span(1101))?;
                let state = b.present_assigned_state(seven, span(1102))?;
                let zero = b.constant(solve::SolveValue::real(p, 0.0), span(1103))?;
                let captured = [state];
                let states: &[_] = if capture { &captured } else { &[] };
                let result = b.fold_with_assigned(
                    domain(1, last, 1),
                    &[zero],
                    &[],
                    states,
                    span(1104),
                    |r, inputs, carried, _, binders, outputs| {
                        let old = r.load(carried[0], span(1105))?;
                        let index = r.load(binders[0], span(1106))?;
                        let index = r.convert(
                            solve::SolveConversionOperator::IntegerToReal,
                            index,
                            span(1107),
                        )?;
                        let mut sum =
                            r.binary(solve::SolveBinaryOperator::Add, old, index, span(1108))?;
                        if read {
                            let present = r.assigned_state_is_present(inputs[0], span(1109))?;
                            let one = r.constant(solve::SolveValue::real(p, 1.0), span(1110))?;
                            let none = r.constant(solve::SolveValue::real(p, 0.0), span(1111))?;
                            let bonus = r.select(present, one, none, span(1112))?;
                            sum =
                                r.binary(solve::SolveBinaryOperator::Add, sum, bonus, span(1113))?;
                        }
                        r.store(outputs[0], sum, span(1114))
                    },
                )?;
                b.store(outputs[0], result[0], span(1115))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

/// Loads, stores and bulk memory operations of every function of `module`.
fn memory_accesses(module: &[u8]) -> usize {
    use wasmparser::Operator as O;
    let mut count = 0;
    for payload in wasmparser::Parser::new(0).parse_all(module) {
        let wasmparser::Payload::CodeSectionEntry(body) = payload.unwrap() else {
            continue;
        };
        for operator in body.get_operators_reader().unwrap() {
            count += usize::from(matches!(
                operator.unwrap(),
                O::I32Load { .. }
                    | O::I64Load { .. }
                    | O::F64Load { .. }
                    | O::I32Store { .. }
                    | O::I64Store { .. }
                    | O::F64Store { .. }
                    | O::MemoryCopy { .. }
                    | O::MemoryFill { .. }
            ));
        }
    }
    count
}

#[test]
fn a_captured_state_the_loop_never_reads_costs_no_memory_access() {
    let (plain, plain_site) = state_fold(false, false, 5);
    let (captured, captured_site) = state_fold(true, false, 5);
    let plain_wasm = compile_pure_call_wasm(&plain, &plain_site).unwrap();
    let captured_wasm = compile_pure_call_wasm(&captured, &captured_site).unwrap();
    for (table, site, wasm) in [
        (&plain, &plain_site, &plain_wasm),
        (&captured, &captured_site, &captured_wasm),
    ] {
        let (status, actual) = Runner::new(wasm).run(&[]);
        assert_eq!(status, 0);
        assert_eq!(actual, oracle(table, site, &[]).unwrap());
        assert_eq!(actual, cells([real(15.0)]));
    }
    assert_eq!(
        memory_accesses(captured_wasm.module_bytes()),
        memory_accesses(plain_wasm.module_bytes())
    );
}

#[test]
fn a_captured_state_the_loop_reads_is_copied_and_matches_the_oracle() {
    let (unread, unread_site) = state_fold(true, false, 5);
    let (read, read_site) = state_fold(true, true, 5);
    let unread_wasm = compile_pure_call_wasm(&unread, &unread_site).unwrap();
    let read_wasm = compile_pure_call_wasm(&read, &read_site).unwrap();
    let (status, actual) = Runner::new(&read_wasm).run(&[]);
    assert_eq!(status, 0);
    assert_eq!(actual, oracle(&read, &read_site, &[]).unwrap());
    assert_eq!(actual, cells([real(20.0)]));
    assert!(
        memory_accesses(read_wasm.module_bytes()) > memory_accesses(unread_wasm.module_bytes())
    );
}
