//! A helper is built for the results its caller reads (SOLVE-C95).
use super::*;

/// Elements of the tensors: 4800 bytes, above the by-reference threshold.
const N: usize = 600;

/// `reader(x, factor)` calls `split(x, factor) = (x + factor, x ^ factor)`
/// and returns the first result, and the second as well when `reads_both`.
fn table(reads_both: bool) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let tensor =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![N as u32]).unwrap();
    let scalar = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let split = table
        .add_owner(
            identity(9200),
            vec![tensor.clone(), scalar.clone()],
            vec![
                solve::SolvePureCallOutput::result(tensor.clone()),
                solve::SolvePureCallOutput::result(tensor.clone()),
            ],
            span(9200),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span(9201))?;
                let factor = b.load(inputs[1], span(9202))?;
                let sum = b.broadcast_binary(
                    solve::SolveBinaryOperator::Add,
                    x,
                    factor,
                    false,
                    span(9203),
                )?;
                let power = b.broadcast_binary(
                    solve::SolveBinaryOperator::Power,
                    x,
                    factor,
                    false,
                    span(9204),
                )?;
                b.store(outputs[0], sum, span(9205))?;
                b.store(outputs[1], power, span(9206))
            },
        )
        .unwrap();
    let mut outputs = vec![solve::SolvePureCallOutput::result(tensor.clone())];
    if reads_both {
        outputs.push(solve::SolvePureCallOutput::result(tensor.clone()));
    }
    let reader = table
        .add_owner(
            identity(9201),
            vec![tensor, scalar],
            outputs,
            span(9210),
            move |b, inputs, outputs| {
                let x = b.load(inputs[0], span(9211))?;
                let factor = b.load(inputs[1], span(9212))?;
                let both = b.call(split, &[x, factor], span(9213))?;
                b.store(outputs[0], both[0], span(9214))?;
                if reads_both {
                    b.store(outputs[1], both[1], span(9215))?;
                }
                Ok(())
            },
        )
        .unwrap();
    let site = table.call_site(reader).unwrap();
    (table.finish(), site)
}

/// One stage assigning `Y[0..N] = reader(P)[0]`, written as a residual, and
/// the run of it: the published Y and the number of `pow` evaluations.
fn run(reads_both: bool, stage_reads_second: bool) -> (Vec<f64>, Vec<f64>, usize) {
    let (table, site) = table(reads_both);
    let n = N as u32;
    let mut ops = vec![
        solve::LinearOp::TensorLoad {
            dst_start: 0,
            input: solve::TensorInputKind::Y,
            input_start: 0,
            count: N,
            seed_start: None,
            lanes: 1,
        },
        solve::LinearOp::TensorLoad {
            dst_start: n,
            input: solve::TensorInputKind::P,
            input_start: 0,
            count: N,
            seed_start: None,
            lanes: 1,
        },
        solve::LinearOp::LoadP {
            dst: 2 * n,
            index: N,
        },
        solve::LinearOp::PureCall {
            dst_start: 2 * n + 1,
            input_starts: vec![n, 2 * n].into_boxed_slice(),
            site,
        },
        solve::LinearOp::TensorBinary {
            dst_start: 5 * n + 1,
            op: solve::BinaryOp::Sub,
            lhs_start: 0,
            rhs_start: 2 * n + 1,
            count: N,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        solve::LinearOp::StoreOutputRange {
            start: 5 * n + 1,
            count: N,
            stride: 1,
        },
    ];
    if stage_reads_second {
        ops.extend([
            solve::LinearOp::TensorLoad {
                dst_start: 6 * n + 1,
                input: solve::TensorInputKind::Y,
                input_start: N,
                count: N,
                seed_start: None,
                lanes: 1,
            },
            solve::LinearOp::TensorBinary {
                dst_start: 7 * n + 1,
                op: solve::BinaryOp::Sub,
                lhs_start: 6 * n + 1,
                rhs_start: 3 * n + 1,
                count: N,
                lhs_stride: 1,
                rhs_stride: 1,
                lanes: 1,
            },
            solve::LinearOp::StoreOutputRange {
                start: 7 * n + 1,
                count: N,
                stride: 1,
            },
        ]);
    }
    let targets_count = if stage_reads_second { 2 * N } else { N };
    let layout = solve::VarLayout::from_parts(Default::default(), targets_count, targets_count + 1);
    let source = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::with_program_spans(vec![ops], vec![span(900)]).unwrap(),
        )],
    };
    let targets = (0..targets_count)
        .map(|index| Some(solve::scalar_slot_y(index)))
        .collect::<Vec<_>>();
    let schedule =
        solve::NativeRefreshAssignmentSchedule::from_continuous_block(&source, &targets, &layout)
            .unwrap();
    let compiled =
        compile_native_assignment_schedule_with_calls_wasm(&schedule, &layout, &table).unwrap();
    let mut runner = ProgramRunner::new(&compiled, &layout);
    let mut parameters = (0..N).map(|i| 0.5 + i as f64 * 0.01).collect::<Vec<_>>();
    parameters.push(2.0);
    let (status, y) = runner.run(&parameters);
    assert_eq!(status, 0);
    (parameters, y, *runner.store.data())
}

#[test]
fn a_result_the_caller_never_reads_is_not_computed_in_its_helper() {
    let (p, y, evaluations) = run(false, false);
    for i in 0..N {
        assert_eq!(y[i].to_bits(), (p[i] + 2.0).to_bits(), "read result {i}");
    }
    assert_eq!(evaluations, 0, "the unread power is not evaluated");
}

#[test]
fn a_result_no_stage_reads_is_not_computed_even_when_the_root_returns_it() {
    let (p, y, evaluations) = run(true, false);
    for i in 0..N {
        assert_eq!(y[i].to_bits(), (p[i] + 2.0).to_bits(), "read result {i}");
    }
    assert_eq!(evaluations, 0, "the unread power is not evaluated");
}

#[test]
fn a_result_a_stage_reads_is_computed_once() {
    let (p, y, evaluations) = run(true, true);
    for i in 0..N {
        assert_eq!(y[i].to_bits(), (p[i] + 2.0).to_bits(), "first result {i}");
        assert_eq!(
            y[N + i].to_bits(),
            p[i].powf(2.0).to_bits(),
            "second result {i}"
        );
    }
    assert_eq!(evaluations, N, "the power is evaluated once per element");
}
