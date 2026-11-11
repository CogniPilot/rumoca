//! The issued rows of a torn sweep run as one module call and write exactly
//! the per-row isolators' bits, in the per-row order.
use super::*;
use rumoca_eval_solve::{
    PreparedScalarProgramBlock, RowEvalContext, TargetAssignmentOutputRequest, TornSweepComposite,
    TornSweepStatus,
};
use solve::{BinaryOp, LinearOp as L};

fn binary(dst: u32, op: BinaryOp, lhs: u32, rhs: u32) -> L {
    L::Binary { dst, op, lhs, rhs }
}

/// Causal rows `y1 := y0 * p0`, `y2 := max(0, y1)`, `y3 := y2 / 2` (a
/// literal coefficient) and `y4 := y3 / p1` (non-finite at `p1 = 0`), then
/// the reduced row `y4 - y0`, one program each.
fn implicit_block() -> PreparedScalarProgramBlock {
    let programs = vec![
        vec![
            L::LoadY { dst: 0, index: 1 },
            L::LoadY { dst: 1, index: 0 },
            L::LoadP { dst: 2, index: 0 },
            binary(3, BinaryOp::Mul, 1, 2),
            binary(4, BinaryOp::Sub, 0, 3),
            L::StoreOutput { src: 4 },
        ],
        vec![
            L::LoadY { dst: 0, index: 2 },
            L::Const { dst: 1, value: 0.0 },
            L::LoadY { dst: 2, index: 1 },
            binary(3, BinaryOp::Max, 1, 2),
            binary(4, BinaryOp::Sub, 0, 3),
            L::StoreOutput { src: 4 },
        ],
        vec![
            L::Const { dst: 0, value: 2.0 },
            L::LoadY { dst: 1, index: 3 },
            binary(2, BinaryOp::Mul, 0, 1),
            L::LoadY { dst: 3, index: 2 },
            binary(4, BinaryOp::Sub, 2, 3),
            L::StoreOutput { src: 4 },
        ],
        vec![
            L::LoadY { dst: 0, index: 4 },
            L::LoadY { dst: 1, index: 3 },
            L::LoadP { dst: 2, index: 1 },
            binary(3, BinaryOp::Div, 1, 2),
            binary(4, BinaryOp::Sub, 0, 3),
            L::StoreOutput { src: 4 },
        ],
        vec![
            L::LoadY { dst: 0, index: 4 },
            L::LoadY { dst: 1, index: 0 },
            binary(2, BinaryOp::Sub, 0, 1),
            L::StoreOutput { src: 2 },
        ],
    ];
    let spans = (0..programs.len()).map(fixture::span).collect();
    PreparedScalarProgramBlock::new(
        solve::ScalarProgramBlock::with_program_spans(programs, spans).unwrap(),
    )
    .unwrap()
}

const STEPS: [(usize, usize); 4] = [(0, 1), (1, 2), (2, 3), (3, 4)];

fn one(chunk: std::ops::Range<usize>) -> Vec<std::ops::Range<usize>> {
    std::iter::once(chunk).collect()
}

/// The one chunk of all four causal rows.
fn whole() -> Vec<std::ops::Range<usize>> {
    one(0..STEPS.len())
}

fn layout() -> solve::VarLayout {
    solve::VarLayout::from_parts(Default::default(), 5, 2)
}

fn composite(block: &PreparedScalarProgramBlock) -> TornSweepComposite {
    let sweep = block.prepare_torn_sweep(&STEPS, &[4]).unwrap();
    block.torn_sweep_composite(&sweep).unwrap()
}

fn rows(composite: &TornSweepComposite) -> solve::ScalarProgramBlock {
    solve::ScalarProgramBlock::with_program_spans(
        composite.assignment_rows.clone(),
        composite.assignment_spans.clone(),
    )
    .unwrap()
}

fn compile(
    composite: &TornSweepComposite,
    chunks: &[std::ops::Range<usize>],
) -> Result<rumoca_exec_wasm::CompiledExactAssignmentWasm, rumoca_exec_wasm::WasmCompileError> {
    rumoca_exec_wasm::compile_torn_assignment_rows_wasm(
        &rows(composite),
        &composite.assignment_targets,
        chunks,
        &layout(),
        &solve::SolvePureCallTable::default(),
    )
}

/// The per-row path: each step's certified isolator at the targets written
/// before it, its raw value committed so later rows read it.
fn per_row(block: &PreparedScalarProgramBlock, y: &mut [f64], p: &[f64]) {
    for (row, target) in STEPS {
        let value = block
            .eval_target_assignment_output_unchecked_with_context(TargetAssignmentOutputRequest {
                row_idx: row,
                output_offset: 0,
                target_y_index: target,
                y,
                p,
                t: 0.,
                context: RowEvalContext::default(),
            })
            .unwrap()
            .unwrap();
        y[target] = value;
    }
}

fn inputs() -> Vec<([f64; 5], [f64; 2])> {
    let mut cases = Vec::new();
    for y0 in [
        0.,
        -0.,
        2.5,
        -3.,
        f64::MIN_POSITIVE,
        -f64::from_bits(1),
        f64::INFINITY,
        f64::NAN,
    ] {
        for p in [[0.5, 0.25], [1.5, 0.], [-0.75, -0.]] {
            cases.push(([y0, 9., -0., 11., 13.], p));
        }
    }
    cases
}

#[test]
fn torn_rows_run_as_one_call_with_the_per_row_bits_and_decline() {
    let block = implicit_block();
    let composite = composite(&block);
    assert_eq!(composite.assignment_targets, [1, 2, 3, 4]);
    let chunks = composite.assignment_chunks().collect::<Vec<_>>();
    assert_eq!(chunks, whole(), "four short rows share one chunk");
    let compiled = compile(&composite, &chunks).unwrap();
    let mut runner = runner::Runner::new(&compiled, &layout());
    let sweep = block.prepare_torn_sweep(&STEPS, &[4]).unwrap();
    let mut declined = 0;
    for (y, p) in inputs() {
        let mut expected = y;
        per_row(&block, &mut expected, &p);
        let (status, actual) = runner.run(&y, &p);
        assert_eq!(status, 0);
        assert_eq!(
            runner::bytes(&actual),
            runner::bytes(&expected),
            "y0 = {:?}, p = {p:?}",
            y[0]
        );
        // The interpreted sweep declines exactly when the host's check of
        // the batched targets finds a non-finite value, and otherwise writes
        // the same bits.
        let mut interpreted = y;
        let mut residual = Vec::new();
        let outcome = block
            .eval_torn_sweep_unchecked_with_context(
                &sweep,
                &mut interpreted,
                &p,
                0.,
                RowEvalContext::default(),
                &mut residual,
            )
            .unwrap();
        let finite = STEPS.iter().all(|&(_, target)| actual[target].is_finite());
        assert_eq!(outcome == TornSweepStatus::Completed, finite);
        if finite {
            assert_eq!(runner::bytes(&interpreted), runner::bytes(&actual));
        } else {
            declined += 1;
        }
    }
    assert!(declined > 0, "the non-finite row declines some inputs");
}

#[test]
fn issued_chunks_split_the_rows_into_functions_with_the_same_bits() {
    let block = implicit_block();
    let composite = composite(&block);
    let single = compile(&composite, &whole()).unwrap();
    let split = compile(&composite, &[0..1, 1..3, 3..4]).unwrap();
    let functions = |compiled: &rumoca_exec_wasm::CompiledExactAssignmentWasm| {
        wasmparser::Parser::new(0)
            .parse_all(compiled.module_bytes())
            .filter(|payload| matches!(payload, Ok(wasmparser::Payload::CodeSectionEntry(_))))
            .count()
    };
    assert_eq!(functions(&split), functions(&single) + 3);
    let mut single = runner::Runner::new(&single, &layout());
    let mut split = runner::Runner::new(&split, &layout());
    for (y, p) in inputs() {
        let ((one, whole), (other, parts)) = (single.run(&y, &p), split.run(&y, &p));
        assert_eq!((one, runner::bytes(&whole)), (other, runner::bytes(&parts)));
    }
}

#[test]
fn torn_rows_refuse_plans_that_differ_from_their_issue() {
    let block = implicit_block();
    let composite = composite(&block);
    let refused =
        |rows: &solve::ScalarProgramBlock, targets: &[usize], chunks: &[std::ops::Range<usize>]| {
            rumoca_exec_wasm::compile_torn_assignment_rows_wasm(
                rows,
                targets,
                chunks,
                &layout(),
                &solve::SolvePureCallTable::default(),
            )
            .err()
            .map(|error| error.to_string())
            .unwrap()
        };
    let rows = rows(&composite);
    let targets = composite.assignment_targets.as_slice();
    for (chunks, what) in [
        (one(0..3), "uncovered row"),
        (vec![0..2, 3..4], "gap"),
        (vec![0..2, 1..4], "overlap"),
    ] {
        assert!(
            refused(&rows, targets, &chunks).contains("chunks differ"),
            "{what}"
        );
    }
    assert!(refused(&rows, &targets[..3], &whole()).contains("target list"));
    assert!(refused(&rows, &[1, 2, 3, 5], &whole()).contains("Y layout"));
    assert!(refused(&solve::ScalarProgramBlock::default(), &[], &[]).contains("no row"));
}
