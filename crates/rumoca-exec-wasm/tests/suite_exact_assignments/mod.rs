//! Exact schedule execution preserves issued tuple commits, order and faults.
mod fixture;
mod private_targets;
mod runner;

use rumoca_ir_solve as solve;

#[test]
fn sparse_tuple_and_later_dependency_preserve_canonical_bits_and_source_order() {
    let fixture = fixture::plain(5);
    assert_eq!(fixture.schedule().program_ids().len(), 2);
    let first = fixture
        .owners
        .exact_assignment_program(fixture.schedule().program_ids()[0])
        .unwrap();
    assert_eq!(first.target_indices(), &[1, 3]);
    let compiled = fixture.compile().unwrap();
    let mut runner = runner::Runner::new(&compiled, &fixture.layout);
    for value in [0., -0., 2.5, -5., f64::MIN_POSITIVE] {
        let y = [value, 9., -0., 11., 13., 17.];
        let p = [0.5];
        let mut expected = y;
        fixture.canonical(&mut expected, &p).unwrap();
        let (status, actual) = runner.run(&y, &p);
        assert_eq!(status, 0);
        assert_eq!(runner::bytes(&actual), runner::bytes(&expected));
        assert_eq!(actual[1].to_bits(), (value * p[0]).to_bits());
        assert_eq!(actual[3].to_bits(), (value + p[0]).to_bits());
        assert_eq!(actual[4].to_bits(), (actual[1] - actual[3]).to_bits());
    }
}

#[test]
fn a_schedule_over_the_chunk_budget_runs_as_several_functions_with_canonical_bits() {
    let fixture = fixture::long_chain(solve::EXACT_ASSIGNMENT_CHUNK_OPS * 3 / 4);
    assert_eq!(
        fixture.schedule().chunks().collect::<Vec<_>>(),
        vec![0..1, 1..2]
    );
    let compiled = fixture.compile().unwrap();
    // Encoded body bytes of each defined function, ending with the two chunks and the entry.
    let bodies = wasmparser::Parser::new(0)
        .parse_all(compiled.module_bytes())
        .filter_map(|payload| match payload.ok()? {
            wasmparser::Payload::CodeSectionEntry(body) => Some(body.range().len()),
            _ => None,
        })
        .collect::<Vec<_>>();
    eprintln!("exact chunk body bytes: {bodies:?}");
    let [.., first, second, entry] = bodies[..] else {
        panic!("two chunk functions plus the entry: {bodies:?}");
    };
    // Each chunk holds one of two equal programs, so neither carries the schedule.
    let total = first + second;
    assert!(
        first * 10 <= total * 6 && second * 10 <= total * 6,
        "{bodies:?}"
    );
    assert!(entry < first.min(second) / 10, "{bodies:?}");
    let mut runner = runner::Runner::new(&compiled, &fixture.layout);
    for value in [
        0.,
        -0.,
        2.5,
        -5.,
        f64::MIN_POSITIVE,
        f64::from_bits(1),
        -f64::from_bits(1),
    ] {
        let y = [value, 9., -0., 11., 13., 17.];
        let p = [0.25];
        let mut expected = y;
        fixture.canonical(&mut expected, &p).unwrap();
        let (status, actual) = runner.run(&y, &p);
        assert_eq!(status, 0);
        assert_eq!(runner::bytes(&actual), runner::bytes(&expected));
    }
}

#[test]
fn a_fault_in_a_later_chunk_keeps_earlier_commits_and_its_issued_status() {
    let fixture = fixture::late_fault_after_long_chain(solve::EXACT_ASSIGNMENT_CHUNK_OPS * 3 / 2);
    assert_eq!(
        fixture.schedule().chunks().collect::<Vec<_>>(),
        vec![0..1, 1..2]
    );
    let compiled = fixture.compile().unwrap();
    let fault = compiled
        .faults()
        .iter()
        .find(|fault| fault.kind == rumoca_exec_wasm::TypedCallFaultKind::IntegerConversion)
        .unwrap();
    let mut runner = runner::Runner::new(&compiled, &fixture.layout);
    let y = [2., 9., -0., 11., 13., 17.];
    let p = [0.5, f64::NAN];
    let mut expected = y;
    fixture.canonical(&mut expected, &p).unwrap_err();
    let (status, actual) = runner.run(&y, &p);
    assert_eq!(status as u32, fault.status);
    assert_eq!(runner::bytes(&actual), runner::bytes(&expected));
    assert_ne!(
        actual[1].to_bits(),
        y[1].to_bits(),
        "the first chunk committed"
    );
    assert_eq!(actual[4], y[4], "the failing program publishes nothing");
}

#[test]
fn gathers_in_different_chunks_report_distinct_issued_statuses() {
    let fixture = fixture::gathers_across_chunks(solve::EXACT_ASSIGNMENT_CHUNK_OPS * 3 / 2);
    assert_eq!(
        fixture.schedule().chunks().collect::<Vec<_>>(),
        vec![0..1, 1..2, 2..3]
    );
    let compiled = fixture.compile().unwrap();
    // Statuses run on across chunks; a chunk restarting at its own base would
    // reuse the first gather's statuses for the last one.
    let mut statuses = compiled
        .gather_faults()
        .iter()
        .map(|fault| fault.status)
        .collect::<Vec<_>>();
    statuses.sort_unstable();
    statuses.dedup();
    assert_eq!(statuses.len(), compiled.gather_faults().len());
    let mut runner = runner::Runner::new(&compiled, &fixture.layout);
    let y = [2., 9., -0., 11., 13., 17.];
    let mut run = |p: [f64; 2]| {
        let mut expected = y;
        assert!(fixture.canonical(&mut expected, &p).is_err());
        let (status, actual) = runner.run(&y, &p);
        assert_eq!(runner::bytes(&actual), runner::bytes(&expected));
        let fault = compiled
            .gather_faults()
            .iter()
            .find(|fault| fault.status == status as u32)
            .expect("a failing gather reports one of its issued statuses");
        assert_eq!(fault.kind, gather_fault_kind(3.));
        (fault.status, actual)
    };
    let (early, unchanged) = run([3., 1.]);
    assert_eq!(runner::bytes(&unchanged), runner::bytes(&y));
    let (late, committed) = run([1., 3.]);
    assert!(
        late > early,
        "the later chunk's status follows the earlier one"
    );
    assert_eq!(committed[1], 42.);
    assert_ne!(
        committed[2].to_bits(),
        y[2].to_bits(),
        "the chain committed"
    );
    assert_eq!(committed[4], y[4], "the failing gather publishes nothing");
}

#[test]
fn late_source_fault_keeps_prior_tuple_commits_then_recovers_without_input_mutation() {
    let fixture = fixture::late_fault();
    let compiled = fixture.compile().unwrap();
    let fault = compiled
        .faults()
        .iter()
        .find(|fault| fault.kind == rumoca_exec_wasm::TypedCallFaultKind::IntegerConversion)
        .unwrap();
    assert_eq!(fault.provenance, fixture::span(22));
    let mut runner = runner::Runner::new(&compiled, &fixture.layout);
    let y = [2., 9., -0., 11., 13., 17.];
    for input in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY, 1e100] {
        let p = [0.5, input];
        let mut expected = y;
        let error = fixture.canonical(&mut expected, &p).unwrap_err();
        assert!(error.to_string().contains("Real-to-Integer conversion"));
        // Scalar and WASM execution retain the exact issued conversion provenance.
        assert_eq!(error.source_span(), Some(fixture::span(22)));
        let owner = &fixture.calls.owners()[0];
        let argument = rumoca_eval_solve::TypedValue::construct(
            owner.inputs()[0].clone(),
            vec![solve::SolveValueKind::Real64(input.to_bits())],
        )
        .unwrap();
        assert_eq!(
            rumoca_eval_solve::eval_pure_call(&fixture.calls, owner.id(), &[argument]).unwrap_err(),
            rumoca_eval_solve::TypedProgramEvalError::InvalidIntegerConversion {
                provenance: fixture::span(22)
            }
        );
        let (status, actual) = runner.run(&y, &p);
        assert_eq!(status as u32, fault.status);
        assert_eq!(runner::bytes(&actual), runner::bytes(&expected));
        assert_eq!(actual[1], 1.);
        assert_eq!(actual[3], 2.5);
        assert_eq!(actual[4], y[4], "failing tuple cannot publish its output");
        let valid = [0.5, -3.75];
        fixture.canonical(&mut expected, &valid).unwrap();
        let (status, actual) = runner.run(&y, &valid);
        assert_eq!(status, 0);
        assert_eq!(runner::bytes(&actual), runner::bytes(&expected));
    }
}

#[test]
fn invalid_entry_spans_fail_before_any_source_or_public_writes() {
    let fixture = fixture::plain(5);
    let compiled = fixture.compile().unwrap();
    let mut runner = runner::Runner::new(&compiled, &fixture.layout);
    assert_eq!(runner.run(&[2., 9., -0., 11., 13., 17.], &[0.5]).0, 0);
    for address in [
        0,
        runner.p as i32,
        runner.scratch as i32 + 1,
        runner.memory.data(&runner.store).len() as i32,
    ] {
        assert_eq!(runner.invalid_scratch(address), 1);
    }
}

#[test]
fn an_unused_original_prefix_load_still_requires_its_exact_layout_bound() {
    let fixture = fixture::plain(600);
    let error = fixture
        .compile()
        .err()
        .expect("an unused invalid load must refuse admission");
    assert!(error.to_string().contains("outside 6 slots"));
}

/// The refusal of a schedule whose prefix holds `unread`, after checking that
/// its final program holds none of those operations and that the same prefix
/// with an admitted unread load compiles.
fn unread_prefix_refusal(unread: Vec<solve::LinearOp>) -> String {
    let admitted = fixture::unread_prefix(vec![solve::LinearOp::LoadY { dst: 100, index: 5 }]);
    admitted.compile().unwrap();
    let fixture = fixture::unread_prefix(unread.clone());
    let emitted = fixture.first_final_program();
    assert_eq!(emitted, admitted.first_final_program());
    assert!(unread.iter().all(|operation| !emitted.contains(operation)));
    fixture
        .compile()
        .err()
        .expect("an unread source operation outside the profile must refuse admission")
        .to_string()
}

#[test]
fn an_unread_indexed_parameter_load_still_declines_the_primal_profile() {
    let refusal = unread_prefix_refusal(vec![
        solve::LinearOp::Const {
            dst: 101,
            value: 0.,
        },
        solve::LinearOp::LoadIndexedP {
            dst: 100,
            base: 0,
            count: 1,
            index: 101,
        },
    ]);
    assert!(
        refusal.contains("declines directional/seed operations"),
        "{refusal}"
    );
}

#[test]
fn an_unread_two_lane_tensor_load_still_declines_the_primal_arena_profile() {
    let refusal = unread_prefix_refusal(vec![solve::LinearOp::TensorLoad {
        dst_start: 100,
        input: solve::TensorInputKind::Y,
        input_start: 0,
        count: 2,
        seed_start: None,
        lanes: 2,
    }]);
    assert!(refusal.contains("requires one primal lane"), "{refusal}");
}

#[test]
fn a_late_fault_in_a_complete_tuple_publishes_none_of_its_targets() {
    let fixture = fixture::tuple_fault();
    assert_eq!(fixture.schedule().program_ids().len(), 2);
    let first = fixture
        .owners
        .exact_assignment_program(fixture.schedule().program_ids()[0])
        .unwrap();
    assert_eq!(first.target_indices(), &[1, 3]);
    let compiled = fixture.compile().unwrap();
    let fault = compiled
        .faults()
        .iter()
        .find(|f| f.kind == rumoca_exec_wasm::TypedCallFaultKind::IntegerConversion)
        .unwrap();
    let mut runner = runner::Runner::new(&compiled, &fixture.layout);
    let y = [2., 9., -0., 11., 13., 17.];
    let mut expected = y;
    fixture
        .canonical(&mut expected, &[0.5, f64::NAN])
        .unwrap_err();
    assert_eq!(runner::bytes(&expected), runner::bytes(&y));
    let (status, actual) = runner.run(&y, &[0.5, f64::NAN]);
    assert_eq!(status as u32, fault.status);
    assert_eq!(runner::bytes(&actual), runner::bytes(&y));
    fixture.canonical(&mut expected, &[0.5, 3.]).unwrap();
    let (status, actual) = runner.run(&y, &[0.5, 3.]);
    assert_eq!(status, 0);
    assert_eq!(runner::bytes(&actual), runner::bytes(&expected));
}

/// The kind a checked gather index fault reports: non-integral and
/// non-finite indices fail conversion, integral ones outside `1..=2` the bound.
pub(super) fn gather_fault_kind(index: f64) -> rumoca_exec_wasm::TypedCallFaultKind {
    if index.is_finite() && index.fract() == 0. {
        rumoca_exec_wasm::TypedCallFaultKind::IndexBounds
    } else {
        rumoca_exec_wasm::TypedCallFaultKind::IntegerConversion
    }
}

#[test]
fn a_model_gather_fault_reports_its_issued_gather_status_and_publishes_nothing() {
    let fixture = fixture::gather();
    let compiled = fixture.compile().unwrap();
    assert!(compiled.faults().is_empty());
    let mut runner = runner::Runner::new(&compiled, &fixture.layout);
    let y = [2., 9., -0., 11., 13., 17.];
    for (index, value) in [(1., 42.), (2., 64.)] {
        let mut expected = y;
        fixture.canonical(&mut expected, &[index]).unwrap();
        let (status, actual) = runner.run(&y, &[index]);
        assert_eq!(status, 0);
        assert_eq!(runner::bytes(&actual), runner::bytes(&expected));
        assert_eq!(actual[1], value);
    }
    for index in [0., 3., -1., 1.5, f64::NAN, f64::INFINITY] {
        let mut expected = y;
        fixture.canonical(&mut expected, &[index]).unwrap_err();
        let (status, actual) = runner.run(&y, &[index]);
        let fault = compiled
            .gather_faults()
            .iter()
            .find(|fault| fault.status == status as u32)
            .expect("a failing gather reports one of its issued statuses");
        assert_eq!(fault.kind, gather_fault_kind(index));
        assert_eq!(fault.provenance, fixture::span(1));
        assert_eq!(runner::bytes(&actual), runner::bytes(&y));
    }
}

#[test]
fn host_calls_check_the_issued_layout_before_execution() {
    let fixture = fixture::plain(5);
    let compiled = fixture.compile().unwrap();
    let cells = (compiled.scratch_bytes() as usize).div_ceil(8);
    assert!(cells > 0, "the plain schedule owns private scratch");
    let y = [2., 9., -0., 11., 13., 17.];
    for (y_len, p, scratch_len) in [
        (5, &[0.5][..], cells),
        (6, &[][..], cells),
        (6, &[0.5][..], cells - 1),
    ] {
        let mut y = y[..y_len].to_vec();
        let error = compiled
            .call(&mut y, p, 0., &mut vec![0.; scratch_len])
            .unwrap_err();
        assert!(error.to_string().contains("differ from issued layout"));
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let mut scratch = vec![0.; cells];
        let mut y = y;
        let error = compiled.call(&mut y, &[0.5], 0., &mut scratch).unwrap_err();
        assert!(
            error
                .to_string()
                .contains("no admitted engine on this target")
        );
        assert_eq!(y, [2., 9., -0., 11., 13., 17.]);
    }
}

#[test]
fn a_missing_checked_call_table_refuses_before_emission() {
    let mut fixture = fixture::late_fault();
    fixture.calls = solve::SolvePureCallTable::builder(fixture::arithmetic()).finish();
    assert!(fixture.compile().is_err());
}
