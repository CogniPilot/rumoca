//! Large Binary64 Real values between a stage's registers and a root call:
//! read in place from P, built in the stored Y cells, or staged.
use super::*;

/// Elements of the tensors: 4800 bytes, above the by-reference threshold.
const N: usize = 600;

/// `scale(x, factor)`: `(x ^ factor, x)`. The first output is built in the
/// caller's buffer; the second is the input returned unchanged.
pub(super) fn scale_table() -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let tensor =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![N as u32]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(9100),
            vec![
                tensor.clone(),
                solve::SolveValueType::scalar(solve::SolveScalarType::real(p)),
            ],
            vec![
                solve::SolvePureCallOutput::result(tensor.clone()),
                solve::SolvePureCallOutput::result(tensor),
            ],
            span(9100),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span(9101))?;
                let factor = b.load(inputs[1], span(9102))?;
                let doubled = b.broadcast_binary(
                    solve::SolveBinaryOperator::Power,
                    x,
                    factor,
                    false,
                    span(9103),
                )?;
                b.store(outputs[0], doubled, span(9104))?;
                b.store(outputs[1], x, span(9105))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

/// How the program feeds and reads the call.
#[derive(Clone, Copy)]
pub(super) struct Shape {
    /// The call input is the sum of two P tensors, not a bare load of P.
    pub(super) computed_input: bool,
    /// The first output is tripled before it is stored, not stored itself.
    pub(super) transformed_output: bool,
    /// Every input is a P cell or a constant, so the call is memoized.
    pub(super) memoized: bool,
}

fn tensor_load(dst: u32, input: solve::TensorInputKind, start: usize) -> solve::LinearOp {
    solve::LinearOp::TensorLoad {
        dst_start: dst,
        input,
        input_start: start,
        count: N,
        seed_start: None,
        lanes: 1,
    }
}

fn tensor_binary(
    dst: u32,
    op: solve::BinaryOp,
    (lhs, rhs): (u32, u32),
    rhs_stride: usize,
) -> solve::LinearOp {
    solve::LinearOp::TensorBinary {
        dst_start: dst,
        op,
        lhs_start: lhs,
        rhs_start: rhs,
        count: N,
        lhs_stride: 1,
        rhs_stride,
        lanes: 1,
    }
}

/// The loads, the factor and the call, with the call results at `5N`.
fn call_ops(shape: Shape, site: &solve::SolvePureCallSite) -> Vec<solve::LinearOp> {
    let n = N as u32;
    // Registers: Y targets at 0 and n, input at 2n (and 3n, 4n), results at 5n.
    let mut ops = vec![
        tensor_load(0, solve::TensorInputKind::Y, 0),
        tensor_load(n, solve::TensorInputKind::Y, N),
        tensor_load(2 * n, solve::TensorInputKind::P, 0),
    ];
    let input = if shape.computed_input {
        ops.push(tensor_load(3 * n, solve::TensorInputKind::P, N));
        ops.push(tensor_binary(
            4 * n,
            solve::BinaryOp::Add,
            (2 * n, 3 * n),
            1,
        ));
        4 * n
    } else {
        2 * n
    };
    ops.push(solve::LinearOp::LoadP {
        dst: 10 * n + 2,
        index: 2 * N,
    });
    let factor = if shape.memoized {
        10 * n + 2
    } else {
        // Arithmetic on the factor defeats the immutable-coordinate memo.
        ops.push(solve::LinearOp::Const {
            dst: 10 * n + 3,
            value: 0.0,
        });
        ops.push(solve::LinearOp::Binary {
            dst: 10 * n + 4,
            op: solve::BinaryOp::Add,
            lhs: 10 * n + 2,
            rhs: 10 * n + 3,
        });
        10 * n + 4
    };
    ops.push(solve::LinearOp::PureCall {
        dst_start: 5 * n,
        input_starts: vec![input, factor].into_boxed_slice(),
        site: site.clone(),
    });
    ops
}

/// The residuals of the two outputs and their stores.
fn residual_ops(shape: Shape) -> Vec<solve::LinearOp> {
    let n = N as u32;
    let call = 5 * n;
    let mut ops = Vec::new();
    let first = if shape.transformed_output {
        ops.push(solve::LinearOp::LoadP {
            dst: 7 * n,
            index: 2 * N + 1,
        });
        ops.push(tensor_binary(
            7 * n + 1,
            solve::BinaryOp::Mul,
            (call, 7 * n),
            0,
        ));
        7 * n + 1
    } else {
        call
    };
    ops.push(tensor_binary(
        8 * n + 1,
        solve::BinaryOp::Sub,
        (0, first),
        1,
    ));
    ops.push(tensor_binary(
        9 * n + 1,
        solve::BinaryOp::Sub,
        (n, call + n),
        1,
    ));
    for start in [8 * n + 1, 9 * n + 1] {
        ops.push(solve::LinearOp::StoreOutputRange {
            start,
            count: N,
            stride: 1,
        });
    }
    ops
}

/// One stage assigning `Y[0..N] = 3 * scale(P)[0]` or `scale(P)[0]` and
/// `Y[N..2N] = scale(P)[1]`, written as residuals like the model's own rows.
pub(super) fn schedule(
    shape: Shape,
    site: &solve::SolvePureCallSite,
) -> (solve::NativeRefreshAssignmentSchedule, solve::VarLayout) {
    let layout = solve::VarLayout::from_parts(Default::default(), 2 * N, 2 * N + 2);
    let mut ops = call_ops(shape, site);
    ops.extend(residual_ops(shape));
    let source = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::with_program_spans(vec![ops], vec![span(900)]).unwrap(),
        )],
    };
    let targets = (0..2 * N)
        .map(|index| Some(solve::scalar_slot_y(index)))
        .collect::<Vec<_>>();
    let schedule =
        solve::NativeRefreshAssignmentSchedule::from_continuous_block(&source, &targets, &layout)
            .unwrap();
    (schedule, layout)
}

fn run(shape: Shape) -> (Vec<f64>, Vec<f64>) {
    let (table, site) = scale_table();
    let (schedule, layout) = schedule(shape, &site);
    let compiled =
        compile_native_assignment_schedule_with_calls_wasm(&schedule, &layout, &table).unwrap();
    let mut runner = ProgramRunner::new(&compiled, &layout);
    let mut parameters = (0..2 * N)
        .map(|i| (i as f64 - 311.5) * 0.375)
        .collect::<Vec<_>>();
    parameters.push(2.0);
    parameters.push(3.0);
    let (status, y) = runner.run(&parameters);
    assert_eq!(status, 0);
    // A second call on the same instance sees the same host P and publishes
    // the same Y.
    let (again, y_again) = runner.run(&parameters);
    assert_eq!(again, 0);
    assert_eq!(y, y_again);
    (parameters, y)
}

#[test]
fn a_bare_load_input_and_a_stored_output_are_read_and_built_in_place() {
    let (p, y) = run(Shape {
        computed_input: false,
        transformed_output: false,
        memoized: false,
    });
    for i in 0..N {
        assert_eq!(y[i].to_bits(), p[i].powf(2.0).to_bits(), "built output {i}");
        assert_eq!(y[N + i].to_bits(), p[i].to_bits(), "returned input {i}");
    }
}

#[test]
fn a_computed_input_is_staged_and_the_outputs_are_unchanged() {
    let (p, y) = run(Shape {
        computed_input: true,
        transformed_output: false,
        memoized: false,
    });
    for i in 0..N {
        let sum = p[i] + p[N + i];
        assert_eq!(y[i].to_bits(), sum.powf(2.0).to_bits(), "built output {i}");
        assert_eq!(y[N + i].to_bits(), sum.to_bits(), "returned input {i}");
    }
}

#[test]
fn an_output_a_register_computation_reads_reaches_its_registers() {
    let (p, y) = run(Shape {
        computed_input: false,
        transformed_output: true,
        memoized: false,
    });
    for i in 0..N {
        assert_eq!(
            y[i].to_bits(),
            (p[i].powf(2.0) * 3.0).to_bits(),
            "output {i}"
        );
        assert_eq!(y[N + i].to_bits(), p[i].to_bits(), "returned input {i}");
    }
}

/// The two-stage program of the memoized call, compiled.
fn memoized_two_stage_program() -> (
    rumoca_exec_wasm::CompiledNativeCallProgramWasm,
    solve::VarLayout,
) {
    let (table, site) = scale_table();
    let n = N as u32;
    let program = |output: u32, target: usize| {
        vec![
            solve::LinearOp::TensorLoad {
                dst_start: 0,
                input: solve::TensorInputKind::Y,
                input_start: target,
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
                site: site.clone(),
            },
            solve::LinearOp::TensorBinary {
                dst_start: 4 * n + 1,
                op: solve::BinaryOp::Sub,
                lhs_start: 0,
                rhs_start: 2 * n + 1 + output * n,
                count: N,
                lhs_stride: 1,
                rhs_stride: 1,
                lanes: 1,
            },
            solve::LinearOp::StoreOutputRange {
                start: 4 * n + 1,
                count: N,
                stride: 1,
            },
        ]
    };
    let layout = solve::VarLayout::from_parts(Default::default(), 2 * N, N + 1);
    let source = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::with_program_spans(
                vec![program(0, 0), program(1, N)],
                vec![span(900), span(901)],
            )
            .unwrap(),
        )],
    };
    let targets = (0..2 * N)
        .map(|index| Some(solve::scalar_slot_y(index)))
        .collect::<Vec<_>>();
    let schedule =
        solve::NativeRefreshAssignmentSchedule::from_continuous_block(&source, &targets, &layout)
            .unwrap();
    let compiled =
        compile_native_assignment_schedule_with_calls_wasm(&schedule, &layout, &table).unwrap();
    (compiled, layout)
}

/// Two stages call the same site with the same P cells. The callee runs once
/// for the invocation; the second stage reads the first's results intact from
/// the call's memo, including the output built by reference and the input
/// returned unchanged.
#[test]
fn a_memoized_call_with_large_outputs_runs_once_and_both_stages_read_its_results() {
    let (compiled, layout) = memoized_two_stage_program();
    let mut runner = ProgramRunner::new(&compiled, &layout);
    let mut parameters = (0..N).map(|i| 0.5 + i as f64 * 0.01).collect::<Vec<_>>();
    parameters.push(2.0);
    for _ in 0..2 {
        *runner.store.data_mut() = 0;
        let (status, y) = runner.run(&parameters);
        assert_eq!(status, 0);
        for i in 0..N {
            assert_eq!(y[i].to_bits(), parameters[i].powf(2.0).to_bits());
            assert_eq!(y[N + i].to_bits(), parameters[i].to_bits());
        }
        assert_eq!(*runner.store.data(), N, "one evaluation of the callee");
    }
}

/// How many `memory.copy` instructions of each constant length a module holds.
fn constant_copy_lengths(module: &[u8]) -> std::collections::BTreeMap<i32, usize> {
    use wasmparser::{Operator, Parser, Payload};
    let mut counts = std::collections::BTreeMap::new();
    for payload in Parser::new(0).parse_all(module) {
        let Payload::CodeSectionEntry(body) = payload.unwrap() else {
            continue;
        };
        let mut length = None;
        for operator in body.get_operators_reader().unwrap() {
            let operator = operator.unwrap();
            if let (Operator::MemoryCopy { .. }, Some(length)) = (&operator, length) {
                *counts.entry(length).or_default() += 1;
            }
            length = match operator {
                Operator::I32Const { value } => Some(value),
                _ => None,
            };
        }
    }
    counts
}

/// A memoized call leaves its results in the memo tuple and every use reads
/// them there: the call is issued once, at its first occurrence in execution
/// order (no flag or branch decides it), the tuple is copied once, and nothing
/// copies it back to the callee's scratch.
#[test]
fn a_memoized_call_is_issued_once_and_copies_its_results_to_the_memo_once() {
    let (compiled, _) = memoized_two_stage_program();
    let lengths = constant_copy_lengths(compiled.module_bytes());
    let results = lengths
        .iter()
        .filter(|(length, _)| **length as usize >= 2 * N * 8)
        .collect::<Vec<_>>();
    assert!(!results.is_empty(), "the memo tuple is copied: {lengths:?}");
    // One copy in all, though two stages use the call.
    assert!(results.iter().all(|(_, count)| **count == 1), "{lengths:?}");
}
