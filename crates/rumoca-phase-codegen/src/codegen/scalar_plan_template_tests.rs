use super::codegen_test_support::{
    builtin_template, continuous_system_with_derivative, explicit_ode_layout,
    render_solve_fixture_template as render_solve_template_with_name, solve_artifacts,
    solve_c_fixture_header, solve_c_fixture_source,
};
use rumoca_ir_solve as solve;

fn fixture_provenance() -> rumoca_core::ProvenanceSpan {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("scalar_plan_fixture.mo"),
        1,
        2,
    )
    .require_provenance("scalar plan fixture")
    .expect("fixture span is source-backed")
}

fn derivative_problem(
    program: Vec<solve::LinearOp>,
    parameter_scalars: usize,
) -> solve::SolveProblem {
    let block = solve::ScalarProgramBlock::with_source_span(vec![program], fixture_provenance())
        .expect("fixture scalar program is checked");
    let state_scalars = block.output_count();
    let solve_layout = explicit_ode_layout(state_scalars, parameter_scalars);
    let continuous = continuous_system_with_derivative(
        &solve_layout,
        solve::ComputeBlock::from_scalar_program_block(block),
    );
    let layout =
        solve::VarLayout::from_parts(indexmap::IndexMap::new(), state_scalars, parameter_scalars);
    let initialization = solve::InitializationSolveSystem::empty();
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    solve::SolveProblem::construct(
        layout,
        solve_layout,
        continuous,
        initialization,
        discrete,
        events,
        clocks,
    )
    .expect("Solve fixture aggregates satisfy the checked root contract")
}

#[test]
fn multi_output_program_emits_every_store_and_computes_shared_register_once() {
    let problem = derivative_problem(
        vec![
            solve::LinearOp::LoadP { dst: 0, index: 0 },
            solve::LinearOp::Const {
                dst: 1,
                value: -2.0,
            },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::Binary {
                dst: 3,
                op: solve::BinaryOp::Add,
                lhs: 0,
                rhs: 2,
            },
            solve::LinearOp::StoreOutput { src: 2 },
            solve::LinearOp::StoreOutput { src: 3 },
        ],
        1,
    );
    let artifacts = solve_artifacts(&problem);
    let c =
        render_solve_template_with_name(&problem, &artifacts, solve_c_fixture_source(), "Shared")
            .expect("C multi-output program should render");
    assert_eq!(c.matches("const double __r0 = p[0];").count(), 1);
    assert!(c.contains("const double __r1 = -2.0;"));
    assert!(c.contains("__out[0] = __r2;"));
    assert!(c.contains("__out[1] = __r3;"));

    let rust = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("rust-ode", "model_ode.rs.jinja"),
        "Shared",
    )
    .expect("Rust multi-output program should render");
    assert_eq!(rust.matches("let __r0: f64 = p[0];").count(), 1);
    assert!(rust.contains("let __r1: f64 = -2.0;"));
    assert!(rust.contains("__out[0] = __r2;"));
    assert!(rust.contains("__out[1] = __r3;"));
}

#[test]
fn compact_output_range_expands_only_in_final_textual_rendering() {
    let problem = derivative_problem(
        vec![
            solve::LinearOp::Const { dst: 0, value: 1.0 },
            solve::LinearOp::Const { dst: 1, value: 2.0 },
            solve::LinearOp::Const { dst: 2, value: 3.0 },
            solve::LinearOp::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 2,
            },
        ],
        0,
    );
    let artifacts = solve_artifacts(&problem);
    let plan = render_solve_template_with_name(
        &problem,
        &artifacts,
        r#"{% for program in solve_blocks.continuous.derivative_rhs.scalar_plan.programs %}{% for op in program.ops %}{% if op.kind == "StoreOutputRange" %}{{ op.start }}:{{ op.count }}:{{ op.stride }}:{% for output_index in op.output_indices %}{{ output_index }},{% endfor %}{% endif %}{% endfor %}{% endfor %}"#,
        "CompactRange",
    )
    .expect("checked compact output range should reach the target-neutral plan");
    assert!(plan.contains("0:2:2:0,1,"));

    let c = render_solve_template_with_name(
        &problem,
        &artifacts,
        solve_c_fixture_source(),
        "CompactRange",
    )
    .expect("C should render the checked range at its final boundary");
    assert!(c.contains("__out[0] = __r0;"));
    assert!(c.contains("__out[1] = __r2;"));

    let rust = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("rust-ode", "model_ode.rs.jinja"),
        "CompactRange",
    )
    .expect("Rust should render the checked range at its final boundary");
    assert!(rust.contains("__out[0] = __r0;"));
    assert!(rust.contains("__out[1] = __r2;"));

    let mlir = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("mlir", "mlir.mlir.jinja"),
        "CompactRange",
    )
    .expect("MLIR should render the checked range at its final boundary");
    assert!(mlir.contains("memref.store %r0_0, %out[%outi0]"));
    assert!(mlir.contains("memref.store %r1_2, %out[%outi1]"));

    let wgsl = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("wgsl-ode", "model_ode.wgsl.jinja"),
        "CompactRange",
    )
    .expect("WGSL should render each checked range projection at its final boundary");
    assert!(wgsl.contains("out[0] = 1.0;"));
    assert!(wgsl.contains("out[1] = 3.0;"));
}

#[test]
fn linear_solve_targets_expose_explicit_failure_abis() {
    let problem = derivative_problem(
        vec![
            solve::LinearOp::Const { dst: 0, value: 0.0 },
            solve::LinearOp::Const { dst: 1, value: 1.0 },
            solve::LinearOp::LinearSolveComponent {
                dst: 2,
                matrix_start: 0,
                rhs_start: 1,
                n: 1,
                component: 0,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ],
        0,
    );
    let artifacts = solve_artifacts(&problem);
    let c =
        render_solve_template_with_name(&problem, &artifacts, solve_c_fixture_source(), "Singular")
            .expect("C target should render an explicit status ABI");
    assert!(c.contains("int Singular_derivative_rhs("));
    assert!(c.contains("double __out["));
    assert!(c.contains("return 1;"));
    assert!(c.contains("out[i] = __out[i];"));
    assert!(!c.contains("return 0.0;"));

    let rust = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("rust-ode", "model_ode.rs.jinja"),
        "Singular",
    )
    .expect("Rust target should render a Result ABI");
    assert!(rust.contains(") -> Result<(), SolveError>"));
    assert!(rust.contains("Err(SolveError::SingularLinearSystem)"));
    assert!(rust.contains("copy_from_slice(&__out)"));
    assert!(!rust.contains("return 0.0;"));

    let directory = tempfile::tempdir().expect("temporary generated-kernel directory");
    compile_and_run_c_failure_abi(&problem, &artifacts, &c, directory.path());
    compile_and_run_rust_failure_abi(&rust, directory.path());
}

fn compile_and_run_c_failure_abi(
    problem: &solve::SolveProblem,
    artifacts: &solve::SolveArtifactInputs,
    source: &str,
    directory: &std::path::Path,
) {
    let header =
        render_solve_template_with_name(problem, artifacts, solve_c_fixture_header(), "Singular")
            .expect("C target header should render");
    std::fs::write(directory.join("Singular_ode.c"), source).expect("write generated C source");
    std::fs::write(directory.join("Singular_ode.h"), header).expect("write generated C header");
    std::fs::write(
        directory.join("c_driver.c"),
        r#"#include "Singular_ode.h"
int main(void) {
    double y = 0.0;
    double out = 42.0;
    int status = Singular_derivative_rhs(0.0, &y, 0, &out);
    return status == 1 && out == 42.0 ? 0 : 1;
}
"#,
    )
    .expect("write C failure-ABI driver");
    let binary = directory.join("c_failure_abi");
    let c_compile = std::process::Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror"])
        .arg(directory.join("Singular_ode.c"))
        .arg(directory.join("c_driver.c"))
        .args(["-lm", "-o"])
        .arg(&binary)
        .output()
        .expect("start C compiler");
    assert!(
        c_compile.status.success(),
        "generated C failure ABI did not compile:\n{}",
        String::from_utf8_lossy(&c_compile.stderr)
    );
    assert!(
        std::process::Command::new(&binary)
            .status()
            .expect("run generated C failure-ABI check")
            .success(),
        "singular generated C kernel committed output or returned success"
    );
}

fn compile_and_run_rust_failure_abi(source: &str, directory: &std::path::Path) {
    std::fs::write(directory.join("generated.rs"), source).expect("write generated Rust source");
    std::fs::write(
        directory.join("rust_driver.rs"),
        r#"mod generated;
fn main() {
    let mut out = [42.0];
    let result = generated::derivative_rhs(0.0, &[0.0], &[], &mut out);
    assert_eq!(result, Err(generated::SolveError::SingularLinearSystem));
    assert_eq!(out, [42.0]);
}
"#,
    )
    .expect("write Rust failure-ABI driver");
    let binary = directory.join("rust_failure_abi");
    let rust_compile = std::process::Command::new("rustc")
        .arg("--edition=2024")
        .arg(directory.join("rust_driver.rs"))
        .arg("-o")
        .arg(&binary)
        .output()
        .expect("start Rust compiler");
    assert!(
        rust_compile.status.success(),
        "generated Rust failure ABI did not compile:\n{}",
        String::from_utf8_lossy(&rust_compile.stderr)
    );
    assert!(
        std::process::Command::new(&binary)
            .status()
            .expect("run generated Rust failure-ABI check")
            .success(),
        "singular generated Rust kernel committed output or returned success"
    );
}
