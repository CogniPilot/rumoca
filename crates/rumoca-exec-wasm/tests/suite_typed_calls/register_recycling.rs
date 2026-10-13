//! Last-read private tensor reuse preserves late reads, captures and faults.
use super::*;

fn fixture(repeated: bool) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let scalar = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(92),
            vec![scalar.clone(), integer, boolean],
            vec![
                solve::SolvePureCallOutput::result(scalar.clone()),
                solve::SolvePureCallOutput::result(scalar.clone()),
            ],
            span(9200),
            |b, input, output| {
                let x = b.load(input[0], span(9201))?;
                let index = b.load(input[1], span(9202))?;
                let enabled = b.load(input[2], span(9203))?;
                let first = b.fill(x, vec![100], span(9204))?;
                let one = b.constant(solve::SolveValue::real(p, 1.0), span(9205))?;
                let mut value = x;
                for step in 0..16 {
                    let next = b.binary(
                        solve::SolveBinaryOperator::Add,
                        value,
                        one,
                        span(9210 + step * 3),
                    )?;
                    let tensor = b.fill(next, vec![100], span(9211 + step * 3))?;
                    value = b.project_element(tensor, vec![99], span(9212 + step * 3))?;
                }
                // Duplicate immutable capture slots cannot create owned storage.
                let selected = b.conditional(
                    enabled,
                    &[first, first, index, value],
                    vec![scalar.clone()],
                    span(9270),
                    |r, captures, outputs| {
                        let old = r.load(captures[0], span(9271))?;
                        let duplicate = r.load(captures[1], span(9272))?;
                        let index = r.load(captures[2], span(9273))?;
                        let late = r.project_element_dynamic(old, &[index], span(9274))?;
                        let same = r.project_element(duplicate, vec![99], span(9275))?;
                        let sum =
                            r.binary(solve::SolveBinaryOperator::Add, late, same, span(9276))?;
                        r.store(outputs[0], sum, span(9277))
                    },
                    |r, captures, outputs| {
                        let value = r.load(captures[3], span(9278))?;
                        r.store(outputs[0], value, span(9279))
                    },
                )?;
                let parent = b.project_element(first, vec![99], span(9280))?;
                b.store(output[0], selected[0], span(9281))?;
                b.store(output[1], parent, span(9282))
            },
        )
        .unwrap();
    let owner = if repeated {
        let inputs = table.call_site(owner).unwrap().inputs().to_vec();
        table
            .add_owner(
                identity(93),
                inputs,
                vec![solve::SolvePureCallOutput::result(scalar.clone()); 4],
                span(9300),
                |b, input, output| {
                    let x = b.load(input[0], span(9301))?;
                    let index = b.load(input[1], span(9302))?;
                    let enabled = b.load(input[2], span(9303))?;
                    let first = b.call(owner, &[x, index, enabled], span(9304))?;
                    let second = b.call(owner, &[x, index, enabled], span(9305))?;
                    for (slot, value) in output.iter().zip(first.iter().chain(&second)) {
                        b.store(*slot, *value, span(9306))?;
                    }
                    Ok(())
                },
            )
            .unwrap()
    } else {
        owner
    };
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn recycled_intermediates_keep_late_parent_and_duplicate_capture_reads_exact() {
    let (table, site) = fixture(false);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    assert!(compiled.layout().scratch_bytes < 8 * 100 * 4);
    let mut runner = Runner::new(&compiled);
    for (x, index, enabled, expected) in [
        (2.0, 1, true, 4.0),
        (2.0, i64::MAX, false, 18.0),
        (-3.0, 99, true, -6.0),
        (-0.0, 1, true, -0.0),
        (2.0, 1, true, 4.0),
    ] {
        let inputs = vec![
            vec![real(x)],
            vec![solve::SolveValueKind::Integer(index)],
            vec![solve::SolveValueKind::Boolean(enabled)],
        ];
        let expected_oracle = oracle(&table, &site, &inputs)
            .expect("positive fixture uses one-based coordinates within its 100-cell source");
        let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
        assert_eq!(
            status,
            0,
            "inputs={inputs:?}, faults={:?}",
            compiled.faults()
        );
        assert_eq!(actual, expected_oracle);
        assert_eq!(actual, cells([real(expected), real(x)]));
    }
}

#[test]
fn a_reached_late_index_fault_preserves_outputs_and_next_invocation() {
    let (table, site) = fixture(false);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    for index in [1, 0, 100, 101, 1] {
        let inputs = vec![
            vec![real(7.0)],
            vec![solve::SolveValueKind::Integer(index)],
            vec![solve::SolveValueKind::Boolean(true)],
        ];
        let expected = oracle(&table, &site, &inputs);
        let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
        match expected {
            Ok(expected) => {
                assert_eq!(status, 0);
                assert_eq!(actual, expected);
            }
            Err(_) => {
                let fault = compiled
                    .faults()
                    .iter()
                    .find(|fault| fault.status == status as u32)
                    .expect("original checked fault status");
                assert_eq!(fault.kind, TypedCallFaultKind::IndexBounds);
                assert_eq!(fault.provenance, span(9274));
                assert_eq!(actual, vec![0xa5; compiled.layout().output_bytes as usize]);
            }
        }
    }
}

#[test]
fn sequential_callees_preserve_the_first_live_result_tuple() {
    let (table, site) = fixture(true);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    for (x, index, enabled) in [
        (3.0, 1, true),
        (9.0, i64::MAX, false),
        (-0.0, 99, true),
        (3.0, 1, true),
    ] {
        let inputs = vec![
            vec![real(x)],
            vec![solve::SolveValueKind::Integer(index)],
            vec![solve::SolveValueKind::Boolean(enabled)],
        ];
        let expected_oracle = oracle(&table, &site, &inputs)
            .expect("positive fixture uses one-based coordinates within its 100-cell source");
        let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
        assert_eq!(
            status,
            0,
            "inputs={inputs:?}, faults={:?}",
            compiled.faults()
        );
        assert_eq!(actual, expected_oracle);
    }
}
