use rumoca_core::{SourceId, Span};
use rumoca_ir_solve::{
    BinaryOp, ComputeBlock, DiscreteSolveSystem, InitializationSolveSystem, LinearOp,
    ScalarProgramBlock, SolveClockPartition, SolveEventPartition, UnaryOp,
};

use super::support::{derivative, fixture};

fn render_solve_template_with_name(
    solve: std::sync::Arc<rumoca_ir_solve::SolveModel>,
    model_name: &str,
) -> Result<String, rumoca_phase_codegen::CodegenError> {
    rumoca_phase_codegen::render_mlir_execution_model(solve, model_name)
}

/// The storage each fixture program addresses.
///
/// Forward-mode AD seeds the parameters after the states, so the derivative
/// Jacobian column space is `y_scalars + p_scalars` wide. Every fixture
/// declares exactly the storage its own `LoadY`/`LoadP` ops read.
fn derivative_problem(
    derivative_rhs: ComputeBlock,
    scalars: Vec<fixture::FixtureScalar>,
) -> fixture::ContinuousProblem {
    derivative::derivative_problem(fixture::ContinuousInventory::new(scalars), derivative_rhs)
}

fn scalar_program_block(rows: Vec<Vec<LinearOp>>, label: &str) -> ScalarProgramBlock {
    ScalarProgramBlock::with_source_span(
        rows,
        Span::from_offsets(SourceId::from_source_name(label), 0, label.len())
            .require_provenance("MLIR renderer fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("fixture program is computable")
}

fn decay_solve() -> std::sync::Arc<rumoca_ir_solve::SolveModel> {
    decay_solve_with_events(SolveEventPartition::default())
}

fn decay_solve_with_events(
    events: SolveEventPartition,
) -> std::sync::Arc<rumoca_ir_solve::SolveModel> {
    let row: Vec<LinearOp> = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Unary {
            dst: 1,
            op: UnaryOp::Neg,
            arg: 0,
        },
        LinearOp::LoadP { dst: 2, index: 0 },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Add,
            lhs: 1,
            rhs: 2,
        },
        LinearOp::StoreOutput { src: 3 },
    ];
    let problem = derivative::derivative_problem_with_partitions(
        fixture::ContinuousInventory::new(vec![
            fixture::FixtureScalar::state("x", 0.0, 1.0),
            fixture::FixtureScalar::parameter("p", 0.0),
        ]),
        ComputeBlock::from_scalar_program_block(scalar_program_block(
            vec![row],
            "render_mlir_decay.mo",
        )),
        InitializationSolveSystem::empty(),
        DiscreteSolveSystem::default(),
        events,
        SolveClockPartition::default(),
    );
    fixture::complete_model(problem)
}

#[test]
fn mlir_template_renders_eval_derivative() {
    let solve = decay_solve();
    let mlir = render_solve_template_with_name(solve, "decay").expect("template should render");

    assert!(
        mlir.contains("func.func @eval_derivative"),
        "missing eval_derivative function"
    );
    assert!(
        !mlir.contains("%drv_zero") && !mlir.contains("scf.for %drv_zero_i"),
        "exact-once derivative ownership must not be repaired by a backend zero-prefill"
    );
    assert!(mlir.contains("memref<?xf64>"), "missing memref type");
    assert!(mlir.contains("arith.negf"), "missing negf for Unary::Neg");
    assert!(mlir.contains("arith.addf"), "missing addf for Binary::Add");
    assert!(
        mlir.contains("memref.load"),
        "missing memref.load for LoadY"
    );
    assert!(
        mlir.contains("memref.store"),
        "missing memref.store for StoreOutput"
    );
    assert!(mlir.contains("module @decay"), "missing module name");
}

#[test]
fn mlir_template_renders_loadtime() {
    let row: Vec<LinearOp> = vec![
        LinearOp::LoadTime { dst: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    // xdot = t reads neither state nor parameter storage.
    let solve = derivative_problem(
        ComputeBlock::from_scalar_program_block(scalar_program_block(
            vec![row],
            "render_mlir_time.mo",
        )),
        vec![fixture::FixtureScalar::state("x", 0.0, 1.0)],
    );
    let solve = fixture::complete_model(solve);

    let mlir = render_solve_template_with_name(solve, "time_dep").expect("template should render");

    // LoadTime uses a distinct SSA definition while preserving signed zero.
    assert!(
        mlir.contains("arith.mulf %t"),
        "LoadTime should multiply %t by one"
    );
}

#[test]
fn mlir_template_no_root_conditions_when_empty() {
    let solve = decay_solve();
    let mlir = render_solve_template_with_name(solve, "decay").expect("template should render");

    assert!(
        !mlir.contains("eval_root_conditions"),
        "should not emit eval_root_conditions when empty"
    );
}

#[test]
fn mlir_template_emits_root_conditions_when_present() {
    let root_row: Vec<LinearOp> = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let solve = decay_solve_with_events(SolveEventPartition {
        root_conditions: scalar_program_block(vec![root_row], "render_mlir_root.mo"),
        root_relation_memory_targets: vec![None],
        root_zero_domains: vec![rumoca_ir_solve::RootZeroDomain::Previous],
        root_relation_refresh_roles: vec![rumoca_ir_solve::RootRelationRefreshRole::Frozen],
        ..SolveEventPartition::default()
    });

    let mlir = render_solve_template_with_name(solve, "decay").expect("template should render");

    assert!(
        mlir.contains("eval_root_conditions"),
        "should emit eval_root_conditions when non-empty"
    );
}

/// `xdot = A * x` as one `MatMul` derivative node over two states.
///
/// `diagonal` selects the checked pattern the MLIR emitter dispatches on:
/// a rectangular-diagonal `A` lowers to element-wise multiplies, anything
/// else lowers to `linalg.matmul`.
fn matmul_solve(diagonal: bool) -> std::sync::Arc<rumoca_ir_solve::SolveModel> {
    let label = "render_mlir_matmul.mo";
    let span = Span::from_offsets(SourceId::from_source_name(label), 0, label.len());
    let provenance = rumoca_ir_solve::PatternProvenance::derived(
        rumoca_ir_solve::PatternDerivation::TensorOperand,
        span,
    )
    .expect("fixture provenance is source-backed");
    let dependencies: Vec<Vec<usize>> = if diagonal {
        vec![vec![0], vec![1]]
    } else {
        vec![vec![0, 1], vec![0, 1]]
    };
    let lhs_pattern =
        rumoca_ir_solve::StructuralPattern::from_row_dependencies(2, 2, &dependencies, provenance)
            .expect("fixture lhs pattern");
    let rhs_pattern = rumoca_ir_solve::StructuralPattern::from_row_dependencies(
        2,
        1,
        &[vec![0], vec![0]],
        provenance,
    )
    .expect("fixture rhs pattern");
    let block = ComputeBlock {
        nodes: vec![rumoca_ir_solve::ComputeNode::MatMul {
            lhs_ops: vec![
                LinearOp::Const {
                    dst: 0,
                    value: -1.0,
                },
                LinearOp::Const { dst: 1, value: 0.0 },
                LinearOp::Const { dst: 2, value: 0.0 },
                LinearOp::Const {
                    dst: 3,
                    value: -2.0,
                },
            ],
            lhs_start: 0,
            rhs_ops: vec![
                LinearOp::LoadY { dst: 4, index: 0 },
                LinearOp::LoadY { dst: 5, index: 1 },
            ],
            rhs_start: 4,
            m: 2,
            k: 2,
            n: 1,
            lhs_pattern,
            rhs_pattern,
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span,
        }],
    };
    let problem = derivative_problem(
        block,
        vec![
            fixture::FixtureScalar::state("x[1]", 0.0, 1.0),
            fixture::FixtureScalar::state("x[2]", 0.0, 1.0),
        ],
    );
    fixture::complete_model(problem)
}

#[test]
fn mlir_template_renders_dense_matmul_node() {
    let mlir =
        render_solve_template_with_name(matmul_solve(false), "matmul_dense").expect("renders");

    assert!(
        mlir.contains("linalg.matmul"),
        "a dense MatMul node must reach the native GEMM emitter:\n{mlir}"
    );
}

#[test]
fn mlir_template_renders_diagonal_matmul_as_elementwise_multiplies() {
    let mlir =
        render_solve_template_with_name(matmul_solve(true), "matmul_diagonal").expect("renders");

    // The emitter selects the fast path from the node's derived pattern kind;
    // losing that projection silently demoted every MatMul to the dense path.
    assert!(
        !mlir.contains("linalg.matmul"),
        "a diagonal MatMul node must not emit a GEMM:\n{mlir}"
    );
    assert!(
        mlir.contains("_diag0 = arith.mulf"),
        "a diagonal MatMul node must emit element-wise multiplies:\n{mlir}"
    );
}
