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
        // The legacy scalar wrapper retains the message but erases the typed span.
        assert_eq!(error.source_span(), None);
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
        assert!(error.to_string().contains("execute only on wasm32"));
        assert_eq!(y, [2., 9., -0., 11., 13., 17.]);
    }
}

#[test]
fn a_missing_checked_call_table_refuses_before_emission() {
    let mut fixture = fixture::late_fault();
    fixture.calls = solve::SolvePureCallTable::builder(fixture::arithmetic()).finish();
    assert!(fixture.compile().is_err());
}
