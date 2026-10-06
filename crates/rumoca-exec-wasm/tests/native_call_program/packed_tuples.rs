use super::*;

fn program(
    count: usize,
    target: usize,
    from_y: bool,
    site: &solve::SolvePureCallSite,
    unused: bool,
) -> Vec<solve::LinearOp> {
    let mut ops = Vec::new();
    for i in 0..count {
        let r = (i * 5) as u32;
        ops.push(solve::LinearOp::LoadY {
            dst: r,
            index: target + i,
        });
        ops.push(if from_y {
            solve::LinearOp::LoadY {
                dst: r + 1,
                index: i,
            }
        } else {
            solve::LinearOp::LoadP {
                dst: r + 1,
                index: i,
            }
        });
        ops.push(if unused && !from_y {
            // The first stage commits to private Y before the consumer's late
            // fault. It performs no potentially failing typed call itself.
            solve::LinearOp::Move {
                dst: r + 2,
                src: r + 1,
            }
        } else {
            solve::LinearOp::PureCall {
                dst_start: r + 2,
                input_starts: vec![r + 1].into_boxed_slice(),
                site: site.clone(),
            }
        });
        ops.push(solve::LinearOp::Binary {
            dst: r + 4,
            op: solve::BinaryOp::Sub,
            lhs: r,
            rhs: if unused { r + 1 } else { r + 2 },
        });
    }
    let packed = (5 * count) as u32;
    ops.extend((0..count).map(|i| solve::LinearOp::Move {
        dst: packed + i as u32,
        src: (5 * i + 4) as u32,
    }));
    ops.push(solve::LinearOp::StoreOutputRange {
        start: packed,
        count,
        stride: 1,
    });
    ops
}

fn fixture(
    count: usize,
    failing: bool,
) -> (
    solve::NativeRefreshAssignmentSchedule,
    solve::VarLayout,
    solve::SolvePureCallTable,
    solve::SolvePureCallSite,
) {
    let (table, site) = affine::table(failing);
    let source = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::with_program_spans(
                vec![
                    program(count, count, true, &site, failing),
                    program(count, 0, false, &site, failing),
                ],
                vec![span(15000), span(15001)],
            )
            .unwrap(),
        )],
    };
    let layout = solve::VarLayout::from_parts(Default::default(), count * 2, count);
    let targets = (count..count * 2)
        .chain(0..count)
        .map(|i| Some(solve::scalar_slot_y(i)))
        .collect::<Vec<_>>();
    let owners =
        solve::NativeRefreshAssignmentSchedule::from_continuous_block(&source, &targets, &layout)
            .unwrap();
    let schedule = owners;
    assert_eq!(schedule.stages().len(), 2);
    assert_eq!(schedule.stages()[0].target_range(), Some(0..count));
    assert_eq!(schedule.stages()[1].target_range(), Some(count..count * 2));
    (schedule, layout, table, site)
}

#[test]
fn packed_tuple_calls_preserve_canonical_ieee_bytes_and_whole_stage_order() {
    let count = 160;
    let (schedule, layout, table, site) = fixture(count, false);
    let compiled =
        compile_native_assignment_schedule_with_calls_wasm(&schedule, &layout, &table).unwrap();
    let mut runner = ProgramRunner::new(&compiled, &layout);
    let samples = [
        0.,
        -0.,
        f64::from_bits(1),
        2.75,
        -3.,
        f64::INFINITY,
        f64::NEG_INFINITY,
        f64::from_bits(0x7ff8_dead_beef_1234),
    ];
    for frame in 0..3 {
        let input = (0..count)
            .map(|i| samples[(i + frame) % samples.len()])
            .collect::<Vec<_>>();
        let (status, output) = runner.run(&input);
        assert_eq!(status, 0);
        for (i, value) in input.into_iter().enumerate() {
            let canonical = oracle(&table, &site, &[vec![real(value)]]).unwrap();
            let first = f64::from_le_bytes(canonical[..8].try_into().unwrap());
            let canonical = oracle(&table, &site, &[vec![real(first)]]).unwrap();
            let second = f64::from_le_bytes(canonical[..8].try_into().unwrap());
            assert_eq!(output[i].to_bits(), first.to_bits());
            assert_eq!(output[count + i].to_bits(), second.to_bits());
        }
    }
}

#[test]
fn packed_tuple_unused_late_call_fault_is_atomic_and_recovers_without_input_mutation() {
    let count = 160;
    let (schedule, layout, table, site) = fixture(count, true);
    let compiled =
        compile_native_assignment_schedule_with_calls_wasm(&schedule, &layout, &table).unwrap();
    let fault = compiled
        .faults()
        .iter()
        .find(|fault| fault.kind == TypedCallFaultKind::IntegerConversion)
        .unwrap();
    assert_eq!(fault.provenance, span(12002));
    let mut runner = ProgramRunner::new(&compiled, &layout);
    let valid = (0..count).map(|i| i as f64 + 0.25).collect::<Vec<_>>();
    let mut invalid = valid.clone();
    invalid[count - 1] = f64::INFINITY;
    let (status, _) = runner.run(&invalid);
    assert_eq!(status as u32, fault.status);
    assert!(oracle(&table, &site, &[vec![real(invalid[count - 1])]]).is_err());
    let (status, output) = runner.run(&valid);
    assert_eq!(status, 0);
    for (i, value) in valid.into_iter().enumerate() {
        assert_eq!(output[i].to_bits(), value.to_bits());
        assert_eq!(output[count + i].to_bits(), value.to_bits());
    }
}
