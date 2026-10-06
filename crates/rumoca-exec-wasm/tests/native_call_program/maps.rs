use super::*;

fn fixture() -> (
    solve::NativeRefreshAssignmentSchedule,
    solve::VarLayout,
    solve::SolvePureCallTable,
    solve::SolvePureCallSite,
) {
    let (table, site) = suite_typed_calls::maps::faults::table(3);
    let first = vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::LoadP { dst: 1, index: 0 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ];
    let second = vec![
        solve::LinearOp::TensorLoad {
            dst_start: 0,
            input: solve::TensorInputKind::Y,
            input_start: 1,
            count: 4,
            seed_start: None,
            lanes: 1,
        },
        solve::LinearOp::TensorLoad {
            dst_start: 4,
            input: solve::TensorInputKind::P,
            input_start: 0,
            count: 4,
            seed_start: None,
            lanes: 1,
        },
        solve::LinearOp::LoadP { dst: 8, index: 4 },
        solve::LinearOp::LoadP { dst: 9, index: 5 },
        solve::LinearOp::PureCall {
            dst_start: 10,
            input_starts: vec![4, 8, 9].into_boxed_slice(),
            site: site.clone(),
        },
        solve::LinearOp::TensorBinary {
            dst_start: 14,
            op: solve::BinaryOp::Sub,
            lhs_start: 0,
            rhs_start: 10,
            count: 4,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        solve::LinearOp::StoreOutputRange {
            start: 14,
            count: 4,
            stride: 1,
        },
    ];
    let source = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::with_program_spans(
                vec![first, second],
                vec![span(16160); 2],
            )
            .unwrap(),
        )],
    };
    let layout = solve::VarLayout::from_parts(Default::default(), 5, 6);
    let targets = (0..5)
        .map(|i| Some(solve::scalar_slot_y(i)))
        .collect::<Vec<_>>();
    let owners =
        solve::NativeRefreshAssignmentSchedule::from_continuous_block(&source, &targets, &layout)
            .unwrap();
    (owners, layout, table, site)
}

#[test]
fn typed_map_native_schedule_late_fault_preserves_complete_public_y_and_p() {
    let (schedule, layout, table, site) = fixture();
    let compiled =
        compile_native_assignment_schedule_with_calls_wasm(&schedule, &layout, &table).unwrap();
    let mut runner = ProgramRunner::new(&compiled, &layout);
    for (target, enabled) in [(3., 1.), (5., 0.), (5., 1.), (3., 1.)] {
        let parameters = [-0., 2., 3., 4., target, enabled];
        let inputs = vec![
            parameters[..4].iter().copied().map(real).collect(),
            vec![solve::SolveValueKind::Integer(target as i64)],
            vec![solve::SolveValueKind::Boolean(enabled == 1.)],
        ];
        let actual = runner.run(&parameters);
        match oracle(&table, &site, &inputs) {
            Ok(expected) => {
                assert_eq!(actual.0, 0);
                assert_eq!(actual.1[0].to_bits(), parameters[0].to_bits());
                let expected = expected
                    .chunks_exact(8)
                    .map(|b| f64::from_le_bytes(b.try_into().unwrap()));
                assert_eq!(
                    actual.1[1..]
                        .iter()
                        .map(|v| v.to_bits())
                        .collect::<Vec<_>>(),
                    expected.map(|v| v.to_bits()).collect::<Vec<_>>()
                );
            }
            Err(error) => {
                assert!(actual.0 > 0);
                assert_eq!(
                    actual.1.iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
                    vec![0xa5a5a5a5a5a5a5a5; 5]
                );
                assert_eq!(error.source_span(), Some(span(16057)));
            }
        }
    }
}
