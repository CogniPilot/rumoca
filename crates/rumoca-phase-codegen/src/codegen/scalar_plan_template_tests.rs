use super::codegen_test_support::builtin_template;
use super::*;
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

fn derivative_problem(program: Vec<solve::LinearOp>) -> solve::SolveProblem {
    let block = solve::ScalarProgramBlock::with_source_span(vec![program], fixture_provenance())
        .expect("fixture scalar program is checked");
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs = solve::ComputeBlock::from_scalar_program_block(block);
    problem
}

#[test]
fn scalar_plan_preserves_indexed_load_semantics_output_mapping_and_provenance() {
    let program = vec![
        solve::LinearOp::Const { dst: 0, value: 1.6 },
        solve::LinearOp::LoadIndexedP {
            dst: 1,
            base: 1,
            count: 3,
            index: 0,
        },
        solve::LinearOp::StoreOutput { src: 1 },
    ];
    let problem = derivative_problem(program.clone());
    let artifacts = solve::SolveArtifacts::default();
    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        r#"{% for program in solve_blocks.continuous.derivative_rhs.scalar_plan.programs %}span={{ program.span.start }}:{{ program.span.end }};temps={{ program.temporary_count }};{% for op in program.ops %}{{ op.kind }}:{% if op.kind == "LoadIndexedP" %}{{ op.base }},{{ op.count }},r{{ op.index_ref }}{% elif op.kind == "StoreOutput" %}{{ op.output_index }}{% endif %};{% endfor %}{% endfor %}"#,
        "Indexed",
    )
    .expect("target-neutral scalar plan should render");

    assert!(rendered.contains("temps=2"));
    assert!(rendered.contains("LoadIndexedP:1,3,r0"));
    assert!(rendered.contains("StoreOutput:0"));
    assert!(rendered.contains("span=1:2"));

    let block = solve::ScalarProgramBlock::with_source_span(vec![program], fixture_provenance())
        .expect("fixture scalar program is checked");
    let mut out = [0.0];
    rumoca_eval_solve::eval_scalar_program_block(
        &block,
        &[],
        &[5.0, 10.0, 20.0, 30.0],
        0.0,
        None,
        &mut out,
    )
    .expect("reference evaluator accepts indexed program");
    assert_eq!(out, [30.0]);
}

#[test]
fn multi_output_program_emits_every_store_and_computes_shared_register_once() {
    let problem = derivative_problem(vec![
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
    ]);
    let artifacts = solve::SolveArtifacts::default();
    let c = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("c-solve", "model_solve.c.jinja"),
        "Shared",
    )
    .expect("C multi-output program should render");
    assert_eq!(c.matches("const double __r0 = p[0];").count(), 1);
    assert!(c.contains("const double __r1 = -2.0;"));
    assert!(c.contains("__out[0] = __r2;"));
    assert!(c.contains("__out[1] = __r3;"));

    let rust = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("rust-solve", "model_solve.rs.jinja"),
        "Shared",
    )
    .expect("Rust multi-output program should render");
    assert_eq!(rust.matches("let __r0: f64 = p[0];").count(), 1);
    assert!(rust.contains("let __r1: f64 = -2.0;"));
    assert!(rust.contains("__out[0] = __r2;"));
    assert!(rust.contains("__out[1] = __r3;"));
}

#[test]
fn textual_targets_fail_closed_on_an_unsupported_semantic_op() {
    let problem = derivative_problem(vec![
        solve::LinearOp::Const { dst: 0, value: 0.0 },
        solve::LinearOp::TableNextEvent {
            dst: 1,
            table_id: 0,
            time: 0,
        },
        solve::LinearOp::StoreOutput { src: 1 },
    ]);
    let error = render_solve_template_with_name(
        &problem,
        &solve::SolveArtifacts::default(),
        builtin_template("c-solve", "model_solve.c.jinja"),
        "Unsupported",
    )
    .expect_err("unsupported operations must not be skipped")
    .to_string();

    assert!(error.contains("unsupported-feature:scalar.op.TableNextEvent"));
}

#[test]
fn linear_solve_targets_expose_explicit_failure_abis() {
    let problem = derivative_problem(vec![
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
    ]);
    let artifacts = solve::SolveArtifacts::default();
    let c = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("c-solve", "model_solve.c.jinja"),
        "Singular",
    )
    .expect("C target should render an explicit status ABI");
    assert!(c.contains("int Singular_derivative_rhs("));
    assert!(c.contains("double __out["));
    assert!(c.contains("return 1;"));
    assert!(c.contains("out[i] = __out[i];"));
    assert!(!c.contains("return 0.0;"));

    let rust = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("rust-solve", "model_solve.rs.jinja"),
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
    artifacts: &solve::SolveArtifacts,
    source: &str,
    directory: &std::path::Path,
) {
    let header = render_solve_template_with_name(
        problem,
        artifacts,
        builtin_template("c-solve", "model_solve.h.jinja"),
        "Singular",
    )
    .expect("C target header should render");
    std::fs::write(directory.join("Singular_solve.c"), source).expect("write generated C source");
    std::fs::write(directory.join("Singular_solve.h"), header).expect("write generated C header");
    std::fs::write(
        directory.join("c_driver.c"),
        r#"#include "Singular_solve.h"
int main(void) {
    double out = 42.0;
    int status = Singular_derivative_rhs(0.0, 0, 0, &out);
    return status == 1 && out == 42.0 ? 0 : 1;
}
"#,
    )
    .expect("write C failure-ABI driver");
    let binary = directory.join("c_failure_abi");
    let c_compile = std::process::Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror"])
        .arg(directory.join("Singular_solve.c"))
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
    std::fs::write(directory.join("singular_solve.rs"), source)
        .expect("write generated Rust source");
    std::fs::write(
        directory.join("rust_driver.rs"),
        r#"#[path = "singular_solve.rs"]
mod generated;
fn main() {
    let mut out = [42.0];
    let result = generated::derivative_rhs(0.0, &[], &[], &mut out);
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

#[test]
fn indexed_python_targets_keep_indexing_inside_symbolic_array_dialects() {
    let problem = derivative_problem(vec![
        solve::LinearOp::Const { dst: 0, value: 1.6 },
        solve::LinearOp::LoadIndexedP {
            dst: 1,
            base: 0,
            count: 3,
            index: 0,
        },
        solve::LinearOp::StoreOutput { src: 1 },
    ]);
    let artifacts = solve::SolveArtifacts::default();
    let dae = dae::Dae::construct(rumoca_core::SourceMap::new(), |_| Ok(()))
        .expect("empty checked DAE is valid");
    let renderer = SolveTemplateRenderer::new_with_dae(&problem, &artifacts, &dae)
        .expect("checked Solve context constructs");
    let jax = renderer
        .render(builtin_template("jax-solve", "jax_solve.py.jinja"))
        .expect("JAX indexed target should render");
    assert!(jax.contains("jnp.where"));
    assert!(jax.contains(".astype(jnp.int64)"));
    assert!(!jax.contains("int(min(max(round("));

    let casadi = renderer
        .render(builtin_template("casadi-solve", "casadi_solve.py.jinja"))
        .expect("CasADi indexed target should render");
    assert!(casadi.contains("__index1 = fmin(fmax(if_else("));
    assert!(casadi.contains("if_else(__index1 == 0.0, P[0]"));
    assert!(!casadi.contains("int(min(max(round("));
}
