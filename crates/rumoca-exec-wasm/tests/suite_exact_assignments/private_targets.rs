//! Actual emitted private tuple entries versus canonical prepared target queries.
use super::*;

fn prepared(fixture: &fixture::Fixture) -> rumoca_eval_solve::PreparedScalarProgramBlock {
    let solve::ComputeNode::ScalarPrograms(block) = &fixture.source.nodes[0] else {
        panic!("scalar source");
    };
    rumoca_eval_solve::PreparedScalarProgramBlock::new(block.clone()).unwrap()
}

#[test]
fn private_target_tuple_preserves_old_y_discarded_outputs_and_signed_zero() {
    let fixture = fixture::plain(5);
    let prepared = prepared(&fixture);
    for (output, target) in [(0, 1), (1, 3)] {
        let plan = prepared
            .portable_target_value_plan(0, output, target)
            .unwrap()
            .unwrap();
        let compiled = rumoca_exec_wasm::compile_private_program_wasm(
            plan.program(),
            &fixture.layout,
            &fixture.calls,
        )
        .unwrap();
        let mut runner = runner::Runner::new_private(&compiled, &fixture.layout);
        for value in [
            0.,
            -0.,
            2.5,
            -5.,
            f64::MIN_POSITIVE,
            f64::INFINITY,
            f64::NAN,
        ] {
            let y = [value, 9., -0., 11., 13., 17.];
            let p = [0.5];
            let expected = prepared
                .eval_target_assignment_output_unchecked_with_context(
                    rumoca_eval_solve::TargetAssignmentOutputRequest {
                        row_idx: 0,
                        output_offset: output,
                        target_y_index: target,
                        y: &y,
                        p: &p,
                        t: 0.,
                        context: rumoca_eval_solve::RowEvalContext {
                            pure_calls: Some(&fixture.calls),
                            ..Default::default()
                        },
                    },
                )
                .unwrap()
                .unwrap();
            let (status, tuple) = runner.run_private(&y, &p, compiled.output_count());
            assert_eq!(status, 0);
            let actual = tuple[plan.private_result_offset()];
            if expected.is_nan() {
                assert!(actual.is_nan());
            } else {
                assert_eq!(actual.to_bits(), expected.to_bits());
            }
        }
        assert_ne!(runner.invalid_scratch(0), 0);
    }
}

#[test]
fn private_target_original_prefix_faults_but_canonical_suffix_is_not_executed() {
    let fixture = fixture::tuple_fault();
    let prepared = prepared(&fixture);
    let y = [2., 9., -0., 11., 13., 17.];
    for (output, target, fault_expected) in [(0, 1, false), (1, 3, true)] {
        let plan = prepared
            .portable_target_value_plan(0, output, target)
            .unwrap()
            .unwrap();
        let compiled = rumoca_exec_wasm::compile_private_program_wasm(
            plan.program(),
            &fixture.layout,
            &fixture.calls,
        )
        .unwrap();
        let mut runner = runner::Runner::new_private(&compiled, &fixture.layout);
        for invalid in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY, 1e100] {
            let (status, _) = runner.run_private(&y, &[0.5, invalid], compiled.output_count());
            assert_eq!(status != 0, fault_expected);
            if fault_expected {
                assert!(
                    compiled
                        .faults()
                        .iter()
                        .any(|fault| fault.status == status as u32
                            && fault.provenance == fixture::span(22))
                );
            }
        }
        assert_eq!(
            runner
                .run_private(&y, &[0.5, 3.], compiled.output_count())
                .0,
            0
        );
    }
}

#[test]
fn private_target_invalid_unused_load_and_overlapping_versions_decline_before_execution() {
    let fixture = fixture::plain(99);
    let prepared = prepared(&fixture);
    let plan = prepared
        .portable_target_value_plan(0, 0, 1)
        .unwrap()
        .unwrap();
    assert!(
        rumoca_exec_wasm::compile_private_program_wasm(
            plan.program(),
            &fixture.layout,
            &fixture.calls
        )
        .is_err()
    );
    let mut operations = plan.program().programs()[0].clone();
    operations.insert(
        0,
        solve::LinearOp::Const {
            dst: 100,
            value: 1.,
        },
    );
    let block =
        solve::ScalarProgramBlock::with_program_spans(vec![operations], vec![fixture::span(1)])
            .unwrap();
    assert!(
        rumoca_exec_wasm::compile_private_program_wasm(&block, &fixture.layout, &fixture.calls)
            .is_err()
    );
}

#[test]
fn private_target_shared_prefix_retains_complete_original_typed_tuple() {
    let fixture = fixture::typed_tuple();
    let prepared = prepared(&fixture);
    let first = prepared
        .portable_target_value_plan(0, 0, 1)
        .unwrap()
        .unwrap();
    let second = prepared
        .portable_target_value_plan(0, 1, 3)
        .unwrap()
        .unwrap();
    assert_eq!(first.program().programs(), second.program().programs());
    assert_ne!(
        first.private_result_offset(),
        second.private_result_offset()
    );
    assert_eq!(first.canonical_prefix_len(), 4);
    assert!(
        matches!(&first.program().programs()[0][3],solve::LinearOp::PureCall {site,..} if site.outputs().len()==2)
    );
    let compiled = rumoca_exec_wasm::compile_private_program_wasm(
        first.program(),
        &fixture.layout,
        &fixture.calls,
    )
    .unwrap();
    let mut runner = runner::Runner::new_private(&compiled, &fixture.layout);
    for input in [0., -0., 1.25, -3.5] {
        let y = [7., 900., -0., -600., 1., 2.];
        let (status, tuple) = runner.run_private(&y, &[input], compiled.output_count());
        assert_eq!(status, 0);
        assert_eq!(
            tuple[first.private_result_offset()].to_bits(),
            input.to_bits()
        );
        assert_eq!(
            tuple[second.private_result_offset()].to_bits(),
            (input + input).to_bits()
        );
    }
}

#[test]
fn private_target_gather_fault_reports_its_issued_gather_status() {
    let fixture = fixture::gather();
    let prepared = prepared(&fixture);
    let plan = prepared
        .portable_target_value_plan(0, 0, 1)
        .unwrap()
        .unwrap();
    let compiled = rumoca_exec_wasm::compile_private_program_wasm(
        plan.program(),
        &fixture.layout,
        &fixture.calls,
    )
    .unwrap();
    assert!(compiled.faults().is_empty());
    let mut runner = runner::Runner::new_private(&compiled, &fixture.layout);
    let y = [2., 9., -0., 11., 13., 17.];
    for (index, value) in [(1., 42.), (2., 64.)] {
        let (status, tuple) = runner.run_private(&y, &[index], compiled.output_count());
        assert_eq!(status, 0);
        assert_eq!(tuple[plan.private_result_offset()], value);
    }
    for index in [0., 3., 1.5, f64::NAN] {
        let (status, _) = runner.run_private(&y, &[index], compiled.output_count());
        let fault = compiled
            .gather_faults()
            .iter()
            .find(|fault| fault.status == status as u32)
            .expect("a failing gather reports one of its issued statuses");
        assert_eq!(fault.kind, gather_fault_kind(index));
    }
}

#[test]
fn private_host_calls_check_the_issued_layout_before_execution() {
    let fixture = fixture::plain(5);
    let plan = prepared(&fixture)
        .portable_target_value_plan(0, 0, 1)
        .unwrap()
        .unwrap();
    let compiled = rumoca_exec_wasm::compile_private_program_wasm(
        plan.program(),
        &fixture.layout,
        &fixture.calls,
    )
    .unwrap();
    assert!(plan.private_result_offset() < compiled.output_count());
    let cells = (compiled.scratch_bytes() as usize).div_ceil(8);
    assert!(cells > 0, "the private tuple lives in scratch");
    let y = [2., 9., -0., 11., 13., 17.];
    for (y_len, p, scratch_len) in [
        (5, &[0.5][..], cells),
        (6, &[][..], cells),
        (6, &[0.5][..], cells - 1),
    ] {
        let y = &y[..y_len];
        let error = compiled
            .call(y, p, 0., &mut vec![0.; scratch_len])
            .unwrap_err();
        assert!(error.to_string().contains("differ from issued layout"));
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let mut scratch = vec![0.; cells];
        let error = compiled.call(&y, &[0.5], 0., &mut scratch).unwrap_err();
        assert!(error.to_string().contains("execute only on wasm32"));
    }
}
