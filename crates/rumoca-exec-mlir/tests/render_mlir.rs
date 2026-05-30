use rumoca_ir_solve::{
    BinaryOp, ComputeBlock, LinearOp, ScalarProgramBlock, SolveProblem, UnaryOp,
};
use rumoca_phase_codegen::{
    render_solve_template_with_name as render_solve_template_with_artifacts, templates,
};

fn render_solve_template_with_name(
    solve: &SolveProblem,
    model_name: &str,
) -> Result<String, rumoca_phase_codegen::CodegenError> {
    let artifacts =
        rumoca_phase_solve::lower_solve_artifacts(solve).expect("test solve artifacts lower");
    render_solve_template_with_artifacts(solve, &artifacts, mlir_template(), model_name)
}

fn mlir_template() -> &'static str {
    templates::builtin_target("mlir")
        .and_then(|target| target.template_source("mlir.mlir.jinja"))
        .expect("built-in mlir target must provide mlir.mlir.jinja")
}

fn decay_solve() -> SolveProblem {
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
    SolveProblem::with_derivative_rhs(ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::new(vec![row]),
    ))
}

#[test]
fn mlir_template_renders_eval_derivative() {
    let solve = decay_solve();
    let mlir = render_solve_template_with_name(&solve, "decay").expect("template should render");

    assert!(
        mlir.contains("func.func @eval_derivative"),
        "missing eval_derivative function"
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
    let solve = SolveProblem::with_derivative_rhs(ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::new(vec![row]),
    ));

    let mlir = render_solve_template_with_name(&solve, "time_dep").expect("template should render");

    // LoadTime: adds %t to 0.0
    assert!(mlir.contains("arith.addf %t"), "LoadTime should add to %t");
}

#[test]
fn mlir_template_no_root_conditions_when_empty() {
    let solve = decay_solve();
    let mlir = render_solve_template_with_name(&solve, "decay").expect("template should render");

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
    let mut solve = decay_solve();
    solve.events.root_conditions = ScalarProgramBlock::new(vec![root_row]);

    let mlir = render_solve_template_with_name(&solve, "decay").expect("template should render");

    assert!(
        mlir.contains("eval_root_conditions"),
        "should emit eval_root_conditions when non-empty"
    );
}
