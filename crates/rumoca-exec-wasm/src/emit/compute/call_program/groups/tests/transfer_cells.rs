//! Complete typed scalar transfers, with independent ABI bit expectations.
use super::*;

fn typed_fixture() -> (
    solve::NativeRefreshAssignmentSchedule,
    VarLayout,
    solve::SolvePureCallTable,
) {
    let profile = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::construct(-3, 3).unwrap(),
    );
    let types = vec![
        solve::SolveValueType::scalar(solve::SolveScalarType::real(profile)),
        solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
        solve::SolveValueType::scalar(solve::SolveScalarType::integer(profile)),
    ];
    let mut table = solve::SolvePureCallTable::builder(profile);
    let owner = table
        .add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(77).unwrap()),
            types.clone(),
            types
                .into_iter()
                .map(solve::SolvePureCallOutput::result)
                .collect(),
            span(70),
            |body, inputs, outputs| {
                for (&input, &output) in inputs.iter().zip(outputs) {
                    let value = body.load(input, span(71))?;
                    body.store(output, value, span(72))?;
                }
                Ok(())
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let mut ops = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::LoadY { dst: 2, index: 2 },
        LinearOp::LoadP { dst: 3, index: 0 },
        LinearOp::LoadP { dst: 4, index: 1 },
        LinearOp::LoadP { dst: 5, index: 2 },
        LinearOp::PureCall {
            dst_start: 6,
            input_starts: vec![3, 4, 5].into_boxed_slice(),
            site,
        },
    ];
    for index in 0..3u32 {
        ops.push(LinearOp::Binary {
            dst: 9 + index,
            op: solve::BinaryOp::Sub,
            lhs: index,
            rhs: 6 + index,
        });
    }
    for index in 0..3u32 {
        ops.push(LinearOp::Move {
            dst: 12 + index,
            src: 9 + index,
        });
    }
    ops.push(LinearOp::StoreOutputRange {
        start: 12,
        count: 3,
        stride: 1,
    });
    let source = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::with_program_spans(vec![ops], vec![span(73)]).unwrap(),
        )],
    };
    let layout = VarLayout::from_parts(Default::default(), 3, 3);
    let targets = (0..3)
        .map(|index| Some(solve::scalar_slot_y(index)))
        .collect::<Vec<_>>();
    let owners =
        solve::NativeRefreshAssignmentSchedule::from_continuous_block(&source, &targets, &layout)
            .unwrap();
    (owners, layout, table.finish())
}

fn run_cells(runner: &mut Runner, inputs: [f64; 3]) -> (i32, Vec<u64>) {
    runner
        .memory
        .write(&mut runner.store, 0, &[0xa5; 24])
        .unwrap();
    let parameters = inputs
        .into_iter()
        .flat_map(f64::to_le_bytes)
        .collect::<Vec<_>>();
    runner
        .memory
        .write(&mut runner.store, runner.p, &parameters)
        .unwrap();
    let total = runner.memory.data(&runner.store).len();
    runner
        .memory
        .write(
            &mut runner.store,
            runner.scratch,
            &vec![0x7f; total - runner.scratch],
        )
        .unwrap();
    let status = runner
        .call
        .call(
            &mut runner.store,
            (0, runner.p as i32, 0., runner.scratch as i32, 0),
        )
        .unwrap();
    let memory = runner.memory.data(&runner.store);
    assert_eq!(&memory[runner.p..runner.p + parameters.len()], parameters);
    if status != 0 {
        assert_eq!(&memory[..24], [0xa5; 24]);
    }
    (
        status,
        memory[..24]
            .chunks_exact(8)
            .map(|cell| u64::from_le_bytes(cell.try_into().unwrap()))
            .collect(),
    )
}

#[test]
fn native_scalar_transfers_preserve_real_bits_boolean_abi_and_checked_integer_domain() {
    let (schedule, layout, table) = typed_fixture();
    let artifact = emit_with_budget(&schedule, &layout, &table, usize::MAX).unwrap();
    let mut runner = Runner::new(&artifact, &layout);
    for bits in [
        0u64,
        0x8000_0000_0000_0000,
        1,
        0x7ff0_0000_0000_0000,
        0xfff0_0000_0000_0000,
        0x7ff8_dead_beef_1234,
        0x7ff0_0000_0000_0001,
    ] {
        for boolean in [
            0.,
            -0.,
            1.,
            -2.,
            f64::INFINITY,
            f64::from_bits(0x7ff8_1234_5678_9abc),
        ] {
            for integer in [-3., -0., 0., 3.] {
                check_transfer(&mut runner, bits, boolean, integer);
            }
        }
    }
    for invalid in [
        -4.,
        4.,
        0.5,
        f64::NAN,
        f64::INFINITY,
        f64::NEG_INFINITY,
        1e100,
    ] {
        assert_eq!(run_cells(&mut runner, [-0., 1., invalid]).0, 2);
        assert_eq!(
            run_cells(&mut runner, [-0., 1., 3.]),
            (0, vec![(-0f64).to_bits(), 1f64.to_bits(), 3f64.to_bits()])
        );
    }
}

fn check_transfer(runner: &mut Runner, bits: u64, boolean: f64, integer: f64) {
    let result = run_cells(runner, [f64::from_bits(bits), boolean, integer]);
    assert_eq!(result.0, 0);
    let expected_integer = if integer == 0. {
        0f64.to_bits()
    } else {
        integer.to_bits()
    };
    let expected_boolean = if boolean != 0. { 1f64 } else { 0f64 };
    assert_eq!(
        result.1,
        [bits, expected_boolean.to_bits(), expected_integer]
    );
}

#[test]
fn scalar_transfer_late_fault_remains_atomic_with_original_owner_and_next_call_recovery() {
    // Reuse the existing checked 96-stage fixture: P calls succeed first, then
    // the final Y-backed call's checked conversion fails in a later group.
    let (schedule, layout, table, site) = fixture();
    let bounded = emit_with_budget(&schedule, &layout, &table, BUDGET).unwrap();
    let mut runner = Runner::new(&bounded, &layout);
    let failure = runner.run(1e10);
    assert_ne!(failure.0, 0);
    assert_eq!(failure.1, vec![u64::from_le_bytes([0xa5; 8]); COUNT]);
    assert_eq!(failure.2, 1);
    let fault = bounded
        .faults()
        .iter()
        .find(|fault| fault.status == failure.0 as u32)
        .unwrap();
    assert_eq!(fault.provenance, span(42));
    assert_eq!(fault.kind, crate::TypedCallFaultKind::IntegerConversion);
    let recovered = runner.run(2.);
    assert_eq!(recovered.0, 0);
    assert_eq!(recovered.2, 2);
    let first = oracle(&table, &site, 2.);
    let final_value = oracle(&table, &site, f64::from_bits(first));
    assert_eq!(recovered.1[..COUNT - 1], vec![first; COUNT - 1]);
    assert_eq!(recovered.1[COUNT - 1], final_value);
}
