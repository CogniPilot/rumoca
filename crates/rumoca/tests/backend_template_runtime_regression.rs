use rumoca::Compiler;
use rumoca_compile::codegen::targets::RenderedTargetFile;
use std::fs;
use std::path::Path;
use std::process::Command;

fn checked_decay() -> rumoca::CompilationResult {
    Compiler::new()
        .model("Decay")
        .compile_str(
            "model Decay Real x(start=1); equation der(x)=-2*x; end Decay;",
            "decay.mo",
        )
        .expect("checked compiler pipeline succeeds")
}

fn rendered_target(target: &str) -> (tempfile::TempDir, Vec<RenderedTargetFile>) {
    let result = checked_decay();
    let files = rumoca::render_target_files(&result, "Decay", target, None)
        .expect("checked target renders");
    let directory = tempfile::tempdir().expect("temporary target directory");
    for file in &files {
        let path = directory.path().join(&file.path);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("create rendered target directory");
        }
        fs::write(path, &file.content).expect("write rendered target file");
    }
    (directory, files)
}

fn run_python(module: &Path, script: &str) {
    let output = Command::new("python")
        .args(["-c", script])
        .arg(module)
        .output()
        .expect("start Python target runtime");
    assert!(
        output.status.success(),
        "Python target runtime failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn dae_template_context_exposes_checked_semantic_schema() {
    let result = Compiler::new()
        .model("M")
        .compile_str("model M Real x; equation x=1; end M;", "m.mo")
        .expect("checked compiler pipeline succeeds");
    let rendered = result
        .render_template_str("{{ dae.schema.name }}:{{ dae.schema.version }}")
        .expect("checked DAE template renders");

    // Pinned to `dae_backend::TEMPLATE_SCHEMA_VERSION`: every change to the
    // projected template shape bumps that constant, and this literal must be
    // bumped with it so template consumers see the break loudly. Version 5 is
    // the shape carrying checked function owners, checked discrete ownership,
    // and the proved-projection gate.
    assert_eq!(rendered, "rumoca.checked-dae-template:5");
}

#[test]
fn c_solve_checked_target_compiles() {
    let (directory, files) = rendered_target("c-solve");
    let source = files
        .iter()
        .find(|file| file.path.ends_with(".c"))
        .expect("C Solve target emits a C source");
    let output = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-c"])
        .arg(directory.path().join(&source.path))
        .arg("-I")
        .arg(directory.path())
        .arg("-o")
        .arg(directory.path().join("model.o"))
        .output()
        .expect("start C compiler");
    assert!(
        output.status.success(),
        "checked C Solve target failed to compile:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn casadi_solve_target_imports_evaluates_and_differentiates() {
    let (directory, files) = rendered_target("casadi-solve");
    let module = files
        .iter()
        .find(|file| file.path.ends_with(".py"))
        .expect("CasADi target emits Python");
    run_python(
        &directory.path().join(&module.path),
        r#"
import importlib.util, sys
import casadi as ca
spec = importlib.util.spec_from_file_location("generated", sys.argv[1])
generated = importlib.util.module_from_spec(spec)
spec.loader.exec_module(generated)
value = float(generated.rhs(0.0, ca.DM([1.0]), ca.DM.zeros(0), ca.DM.zeros(0)))
assert value == -2.0
x = ca.SX.sym("x", 1)
derivative = ca.Function("d", [x], [ca.jacobian(generated.rhs(0.0, x, ca.SX.zeros(0), ca.SX.zeros(0)), x)])
assert float(derivative(ca.DM([1.0]))) == -2.0
"#,
    );
}

#[test]
fn jax_solve_target_imports_jits_evaluates_and_differentiates() {
    let (directory, files) = rendered_target("jax-solve");
    let module = files
        .iter()
        .find(|file| file.path.ends_with(".py"))
        .expect("JAX target emits Python");
    run_python(
        &directory.path().join(&module.path),
        r#"
import importlib.util, sys
import jax
import jax.numpy as jnp
spec = importlib.util.spec_from_file_location("generated", sys.argv[1])
generated = importlib.util.module_from_spec(spec)
spec.loader.exec_module(generated)
value = jax.jit(generated.rhs)(0.0, jnp.array([1.0]), jnp.zeros(0), jnp.zeros(0))
assert float(value[0]) == -2.0
derivative = jax.jacfwd(generated.rhs, argnums=1)(0.0, jnp.array([1.0]), jnp.zeros(0), jnp.zeros(0))
assert float(derivative[0, 0]) == -2.0
"#,
    );
}
