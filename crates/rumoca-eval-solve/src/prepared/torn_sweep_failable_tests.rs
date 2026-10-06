//! A causal step whose prefix holds an operation that can fail on an earlier
//! step's target starts a new run, so the grouped sweep fails exactly where
//! the per-step isolators fail.

use rumoca_ir_solve::{BinaryOp, LinearOp, ScalarProgramBlock};

use super::{PreparedScalarProgramBlock, TargetAssignmentOutputRequest, TornSweepStatus};
use crate::{EvalSolveError, RowEvalContext};

/// The 1x1 dense solve `c * z = 1` over register `c`, writing register 10.
fn dense_solve(c: u32) -> Vec<LinearOp> {
    vec![LinearOp::LinearSolveComponent {
        dst: 10,
        matrix_start: c,
        rhs_start: 6,
        n: 1,
        component: 0,
    }]
}

/// A fold carrying a constant (its first destination, register 10) and the
/// dense solve over its capture `c` (register 11): only the later carried
/// slot reads the failing solve.
fn fold_with_dense_solve(c: u32) -> Vec<LinearOp> {
    use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
    let update = vec![
        LinearOp::LoadFoldCarried { dst: 0, index: 0 },
        LinearOp::LoadFoldCapture { dst: 1, index: 0 },
        LinearOp::Const { dst: 2, value: 1.0 },
        LinearOp::LinearSolveComponent {
            dst: 3,
            matrix_start: 1,
            rhs_start: 2,
            n: 1,
            component: 0,
        },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::StoreOutput { src: 3 },
    ];
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: 2,
            step: 1,
        }],
    };
    let program = rumoca_ir_solve::FunctionFoldProgram::checked(domain, 2, 1, update)
        .expect("a checked fold");
    vec![
        LinearOp::Const {
            dst: 12,
            value: 3.0,
        },
        LinearOp::Const {
            dst: 13,
            value: 0.0,
        },
        LinearOp::FunctionFold {
            dst_start: 10,
            initial_start: 12,
            capture_start: c,
            program: std::sync::Arc::new(program),
        },
    ]
}

/// A conditional whose selected region writes a constant (its first
/// destination, register 10) and the dense solve over its capture `c`
/// (register 11).
fn conditional_with_dense_solve(c: u32) -> Vec<LinearOp> {
    let condition = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let selected = vec![
        LinearOp::Const { dst: 0, value: 3.0 },
        LinearOp::LoadFunctionConditionalCapture { dst: 1, index: 0 },
        LinearOp::Const { dst: 2, value: 1.0 },
        LinearOp::LinearSolveComponent {
            dst: 3,
            matrix_start: 1,
            rhs_start: 2,
            n: 1,
            component: 0,
        },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::StoreOutput { src: 3 },
    ];
    let fallback = vec![
        LinearOp::Const { dst: 0, value: 3.0 },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let program = rumoca_ir_solve::FunctionConditionalProgram::checked(
        1,
        [1, 1],
        [(condition, selected)],
        fallback,
    )
    .expect("a checked conditional");
    vec![LinearOp::FunctionConditional {
        dst_start: 10,
        capture_start: c,
        program: std::sync::Arc::new(program),
    }]
}

/// `y0 - 2*y2` and `y1 - (y2 + 1)`; the second prefix also runs `dead` over
/// `c = y[source] - 0.9`, dead to the second isolated value, whose dense
/// solve `c * z = 1` is singular exactly at the first step's new value
/// `y0 = 2 * 0.45 = 0.9` when `source = 0`, and never reads a target when
/// `source = 2`.
fn two_outputs_with_dead_solve(source: usize) -> PreparedScalarProgramBlock {
    two_outputs_with(source, dense_solve)
}

fn two_outputs_with(source: usize, dead: fn(u32) -> Vec<LinearOp>) -> PreparedScalarProgramBlock {
    let program = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 2 },
        LinearOp::Const { dst: 2, value: 2.0 },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 1,
            rhs: 2,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
        LinearOp::LoadY { dst: 5, index: 1 },
        LinearOp::Const { dst: 6, value: 1.0 },
        LinearOp::LoadY {
            dst: 7,
            index: source,
        },
        LinearOp::Const { dst: 8, value: 0.9 },
        LinearOp::Binary {
            dst: 9,
            op: BinaryOp::Sub,
            lhs: 7,
            rhs: 8,
        },
    ];
    let mut program = program;
    program.extend(dead(9));
    program.extend([
        LinearOp::Binary {
            dst: 20,
            op: BinaryOp::Add,
            lhs: 1,
            rhs: 6,
        },
        LinearOp::Binary {
            dst: 21,
            op: BinaryOp::Sub,
            lhs: 5,
            rhs: 20,
        },
        LinearOp::StoreOutput { src: 21 },
    ]);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("torn_sweep_failable_tests.mo"),
        0,
        1,
    );
    PreparedScalarProgramBlock::new(
        ScalarProgramBlock::with_program_spans(vec![program], vec![span]).expect("a checked block"),
    )
    .expect("a prepared block")
}

const STEPS: [(usize, usize); 2] = [(0, 0), (1, 1)];
const START: [f64; 3] = [0.3, -1.7, 0.45];

fn grouped_sweep(
    block: &PreparedScalarProgramBlock,
) -> Result<(TornSweepStatus, [f64; 3]), EvalSolveError> {
    let sweep = block
        .prepare_torn_sweep(&STEPS, &[])
        .expect("the sweep prepares");
    let mut y = START;
    let mut residual = Vec::new();
    let status = block.eval_torn_sweep_unchecked_with_context(
        &sweep,
        &mut y,
        &[],
        0.0,
        RowEvalContext::default(),
        &mut residual,
    )?;
    Ok((status, y))
}

/// Each step's isolator on the targets written before it.
fn per_step_isolators(block: &PreparedScalarProgramBlock) -> Result<[f64; 3], EvalSolveError> {
    let mut y = START;
    for (output, target) in STEPS {
        let value = block.eval_target_assignment_output_unchecked_with_context(
            TargetAssignmentOutputRequest {
                row_idx: 0,
                output_offset: output,
                target_y_index: target,
                y: &y,
                p: &[],
                t: 0.0,
                context: RowEvalContext::default(),
            },
        )?;
        y[target] = value.expect("the isolator answers");
    }
    Ok(y)
}

#[test]
fn a_failable_operation_reading_an_earlier_target_splits_the_run() {
    let block = two_outputs_with_dead_solve(0);
    let runs = block.torn_sweep_runs(&STEPS).expect("both steps isolate");
    assert_eq!(runs.len(), 2, "the dead solve reads the first target");
    let reference = per_step_isolators(&block);
    assert!(
        matches!(reference, Err(EvalSolveError::LinearSolve { .. })),
        "the per-step reference fails at the second step: {reference:?}"
    );
    let grouped = grouped_sweep(&block);
    assert!(
        matches!(grouped, Err(EvalSolveError::LinearSolve { .. })),
        "the grouped sweep fails at the same step: {grouped:?}"
    );
}

#[test]
fn a_failable_operation_reading_no_earlier_target_keeps_the_run() {
    let block = two_outputs_with_dead_solve(2);
    let runs = block.torn_sweep_runs(&STEPS).expect("both steps isolate");
    assert_eq!(runs.len(), 1, "the dead solve reads no target");
    let reference = per_step_isolators(&block).expect("the isolators evaluate");
    let (status, grouped) = grouped_sweep(&block).expect("the sweep evaluates");
    assert_eq!(status, TornSweepStatus::Completed);
    assert_eq!(grouped.map(f64::to_bits), reference.map(f64::to_bits));
}

/// A fold or conditional whose first destination is a constant but whose
/// later slot holds a failing solve over an earlier target splits the run:
/// its failure depends on every operand, whatever its first destination
/// depends on.
#[test]
fn a_fold_or_conditional_holding_a_failable_operation_splits_the_run() {
    for (kind, dead) in [
        ("fold", fold_with_dense_solve as fn(u32) -> Vec<LinearOp>),
        ("conditional", conditional_with_dense_solve),
    ] {
        let block = two_outputs_with(0, dead);
        let runs = block.torn_sweep_runs(&STEPS).expect("both steps isolate");
        assert_eq!(
            runs.len(),
            2,
            "{kind}: the nested solve reads the first target"
        );
        let reference = per_step_isolators(&block);
        let grouped = grouped_sweep(&block);
        assert!(
            reference.is_err(),
            "{kind}: the per-step reference fails: {reference:?}"
        );
        assert_eq!(
            grouped.map(|_| ()),
            reference.map(|_| ()),
            "{kind}: the grouped sweep fails as the per-step isolators do"
        );
        let block = two_outputs_with(2, dead);
        let runs = block.torn_sweep_runs(&STEPS).expect("both steps isolate");
        assert_eq!(runs.len(), 1, "{kind}: reading no target keeps the run");
    }
}

/// At START, floor(3*(y[source]-0.9))+3 is 1; after the first target
/// commits y0=0.9 it is 3. The gathered value never reaches the residual.
fn gather_selector(c: u32) -> Vec<LinearOp> {
    vec![
        LinearOp::Const { dst: 12, value: 3. },
        LinearOp::Binary {
            dst: 13,
            op: BinaryOp::Mul,
            lhs: c,
            rhs: 12,
        },
        LinearOp::Unary {
            dst: 14,
            op: rumoca_ir_solve::UnaryOp::Floor,
            arg: 13,
        },
        LinearOp::Binary {
            dst: 15,
            op: BinaryOp::Add,
            lhs: 14,
            rhs: 12,
        },
        LinearOp::Const { dst: 16, value: 5. },
    ]
}

fn register_gather(c: u32) -> Vec<LinearOp> {
    let mut operations = gather_selector(c);
    operations.push(LinearOp::LoadIndexedRegister {
        dst: 10,
        base: 16,
        stride: 1,
        dimensions: vec![1].into_boxed_slice(),
        indices: vec![rumoca_ir_solve::TensorIndex::Runtime(15)].into_boxed_slice(),
    });
    operations
}

fn indexed_fold(capture: bool) -> std::sync::Arc<rumoca_ir_solve::FunctionFoldProgram> {
    use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
    use rumoca_ir_solve::{FunctionFoldProgram, TensorIndex};
    let read = if capture {
        LinearOp::LoadIndexedFoldCapture {
            dst: 1,
            base: 1,
            stride: 1,
            dimensions: vec![1].into_boxed_slice(),
            indices: vec![TensorIndex::Runtime(0)].into_boxed_slice(),
        }
    } else {
        LinearOp::LoadIndexedFoldCarried {
            dst: 1,
            base: 0,
            stride: 1,
            dimensions: vec![1].into_boxed_slice(),
            indices: vec![TensorIndex::Runtime(0)].into_boxed_slice(),
        }
    };
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: 2,
            step: 1,
        }],
    };
    std::sync::Arc::new(
        FunctionFoldProgram::checked(
            domain,
            2,
            if capture { 2 } else { 1 },
            vec![
                LinearOp::LoadFoldCapture { dst: 0, index: 0 },
                read,
                LinearOp::Const { dst: 2, value: 3. },
                LinearOp::StoreOutput { src: 2 },
                LinearOp::StoreOutput { src: 1 },
            ],
        )
        .unwrap(),
    )
}

fn fold_gather(c: u32, capture: bool) -> Vec<LinearOp> {
    let mut operations = gather_selector(c);
    operations.extend([
        LinearOp::Const { dst: 17, value: 3. },
        LinearOp::Const { dst: 18, value: 0. },
        LinearOp::FunctionFold {
            dst_start: 10,
            initial_start: 17,
            capture_start: 15,
            program: indexed_fold(capture),
        },
    ]);
    operations
}

fn carried_gather(c: u32) -> Vec<LinearOp> {
    fold_gather(c, false)
}
fn captured_gather(c: u32) -> Vec<LinearOp> {
    fold_gather(c, true)
}

fn conditional_gather(c: u32) -> Vec<LinearOp> {
    use rumoca_ir_solve::{FunctionConditionalProgram, TensorIndex};
    let mut operations = gather_selector(c);
    let program = FunctionConditionalProgram::checked(
        1,
        [1, 1],
        [(
            vec![
                LinearOp::Const { dst: 0, value: 1. },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                LinearOp::Const { dst: 1, value: 5. },
                LinearOp::LoadIndexedRegister {
                    dst: 2,
                    base: 1,
                    stride: 1,
                    dimensions: vec![1].into_boxed_slice(),
                    indices: vec![TensorIndex::Runtime(0)].into_boxed_slice(),
                },
                LinearOp::Const { dst: 3, value: 3. },
                LinearOp::StoreOutput { src: 3 },
                LinearOp::StoreOutput { src: 2 },
            ],
        )],
        vec![
            LinearOp::Const { dst: 0, value: 3. },
            LinearOp::StoreOutput { src: 0 },
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .unwrap();
    operations.push(LinearOp::FunctionConditional {
        dst_start: 10,
        capture_start: 15,
        program: std::sync::Arc::new(program),
    });
    operations
}

fn conditional_with_fold_gather(c: u32) -> Vec<LinearOp> {
    use rumoca_ir_solve::FunctionConditionalProgram;
    let mut operations = gather_selector(c);
    let program = FunctionConditionalProgram::checked(
        1,
        [1, 1],
        [(
            vec![
                LinearOp::Const { dst: 0, value: 1. },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                LinearOp::Const { dst: 1, value: 3. },
                LinearOp::Const { dst: 2, value: 0. },
                LinearOp::FunctionFold {
                    dst_start: 3,
                    initial_start: 1,
                    capture_start: 0,
                    program: indexed_fold(false),
                },
                LinearOp::StoreOutput { src: 3 },
                LinearOp::StoreOutput { src: 4 },
            ],
        )],
        vec![
            LinearOp::Const { dst: 0, value: 3. },
            LinearOp::StoreOutput { src: 0 },
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .unwrap();
    operations.push(LinearOp::FunctionConditional {
        dst_start: 10,
        capture_start: 15,
        program: std::sync::Arc::new(program),
    });
    operations
}

#[test]
fn checked_gather_fault_dependence_on_an_earlier_target_splits_chains_through_nested_bodies() {
    for (kind, dead) in [
        ("register", register_gather as fn(u32) -> Vec<LinearOp>),
        ("carried", carried_gather),
        ("capture", captured_gather),
        ("conditional", conditional_gather),
        ("conditional/fold", conditional_with_fold_gather),
    ] {
        let block = two_outputs_with(0, dead);
        assert!(
            block.target_isolation_chain_program(0, &STEPS).is_none(),
            "{kind}: a reused old selector cannot certify later address success"
        );
        assert_eq!(block.torn_sweep_runs(&STEPS).unwrap().len(), 2, "{kind}");
        let reference = per_step_isolators(&block).unwrap_err();
        assert!(
            matches!(
                reference,
                EvalSolveError::TensorIndexOutOfBounds {
                    axis: 1,
                    index: 3,
                    extent: 1,
                    ..
                }
            ),
            "{kind}: {reference:?}"
        );
        let grouped = grouped_sweep(&block).unwrap_err();
        assert_eq!(
            grouped, reference,
            "{kind}: grouped execution preserves the late source fault"
        );
        assert!(reference.source_span().is_some(), "{kind}");
        let independent = two_outputs_with(2, dead);
        assert!(
            independent
                .target_isolation_chain_program(0, &STEPS)
                .is_some(),
            "{kind}: independent selectors retain chaining"
        );
        assert_eq!(
            independent.torn_sweep_runs(&STEPS).unwrap().len(),
            1,
            "{kind}"
        );
        let expected = per_step_isolators(&independent).unwrap();
        let (status, actual) = grouped_sweep(&independent).unwrap();
        assert_eq!(status, TornSweepStatus::Completed);
        assert_eq!(
            actual.map(f64::to_bits),
            expected.map(f64::to_bits),
            "{kind}"
        );
    }
}

#[test]
fn only_runtime_gather_coordinates_add_operand_dependent_failure_effects() {
    use rumoca_ir_solve::TensorIndex;
    let gather = |index| LinearOp::LoadIndexedRegister {
        dst: 3,
        base: 1,
        stride: 1,
        dimensions: vec![1].into_boxed_slice(),
        indices: vec![index].into_boxed_slice(),
    };
    assert!(super::dependency::op_can_fail(&gather(
        TensorIndex::Runtime(0)
    )));
    assert!(!super::dependency::op_can_fail(&gather(
        TensorIndex::Constant(0)
    )));
}
