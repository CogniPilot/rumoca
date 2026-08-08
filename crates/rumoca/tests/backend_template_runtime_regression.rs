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

#[test]
fn explicit_rhs_targets_reject_implicit_algebraic_models() {
    let compiled = Compiler::new()
        .model("ImplicitAlgebraic")
        .compile_str(
            r#"
model ImplicitAlgebraic
  Real x(start = 1);
  Real algebraic(start = 1);
equation
  der(x) = algebraic;
  algebraic * algebraic = x;
end ImplicitAlgebraic;
"#,
            "ImplicitAlgebraic.mo",
        )
        .expect("the compiler accepts the implicit algebraic model");

    for target in [
        "c-ode",
        "rust-ode",
        "rust-fixed-ode",
        "casadi-ode",
        "jax-ode",
        "cuda-ode",
        "wgsl-ode",
    ] {
        let error = rumoca::render_target_files(&compiled, "ImplicitAlgebraic", target, None)
            .expect_err("an explicit RHS target cannot omit algebraic projection");
        assert!(
            error
                .to_string()
                .contains("unsupported-feature:residual_equations"),
            "target {target} returned the wrong diagnostic: {error:#}"
        );
    }
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

fn run_checked(command: &mut Command, context: &str) {
    let output = command.output().unwrap_or_else(|error| {
        panic!("failed to start {context}: {error}");
    });
    assert!(
        output.status.success(),
        "{context} failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
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
fn c_ode_checked_target_compiles_and_executes() {
    let (directory, files) = rendered_target("c-ode");
    let source = files
        .iter()
        .find(|file| file.path.ends_with(".c"))
        .expect("C ODE target emits a C source");
    let harness = directory.path().join("main.c");
    fs::write(
        &harness,
        r#"#include "Decay_ode.h"
#include <math.h>

int main(void) {
    const double y[1] = {1.0};
    const double p[1] = {0.0};
    double out[1] = {123.0};
    if (Decay_derivative_rhs(0.0, y, p, out) != 0) return 1;
    return fabs(out[0] + 2.0) < 1e-12 ? 0 : 2;
}
"#,
    )
    .expect("write C ODE runtime harness");
    let executable = directory.path().join("c-ode-runtime");
    run_checked(
        Command::new("cc")
            .args(["-std=c11", "-Wall", "-Wextra", "-Werror"])
            .arg(directory.path().join(&source.path))
            .arg(&harness)
            .arg("-I")
            .arg(directory.path())
            .arg("-lm")
            .arg("-o")
            .arg(&executable),
        "compile checked C ODE target",
    );
    run_checked(
        &mut Command::new(executable),
        "execute checked C ODE target",
    );
}

#[test]
fn rust_ode_checked_target_compiles_and_executes() {
    let (directory, files) = rendered_target("rust-ode");
    let module = files
        .iter()
        .find(|file| file.path.ends_with(".rs"))
        .expect("Rust ODE target emits a Rust module");
    let harness = directory.path().join("main.rs");
    fs::write(
        &harness,
        format!(
            r#"#[path = {:?}]
mod generated;

fn main() {{
    let mut out = [123.0];
    generated::derivative_rhs(0.0, &[1.0], &[], &mut out).unwrap();
    assert!((out[0] + 2.0).abs() < 1e-12);
}}
"#,
            module.path,
        ),
    )
    .expect("write Rust ODE runtime harness");
    let executable = directory.path().join("rust-ode-runtime");
    run_checked(
        Command::new("rustc")
            .args(["--edition=2024", "-Dwarnings"])
            .arg(&harness)
            .arg("-o")
            .arg(&executable)
            .current_dir(directory.path()),
        "compile checked Rust ODE target",
    );
    run_checked(
        &mut Command::new(executable),
        "execute checked Rust ODE target",
    );
}

#[test]
fn rust_fixed_ode_checked_target_executes_without_heap_allocation() {
    let (directory, files) = rendered_target("rust-fixed-ode");
    let module = files
        .iter()
        .find(|file| file.path.ends_with(".rs"))
        .expect("fixed Rust ODE target emits a Rust module");
    let harness = directory.path().join("main.rs");
    fs::write(
        &harness,
        format!(
            r#"use std::alloc::{{GlobalAlloc, Layout, System}};
use std::sync::atomic::{{AtomicUsize, Ordering}};

struct CountingAllocator;
static ALLOCATIONS: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for CountingAllocator {{
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {{
        ALLOCATIONS.fetch_add(1, Ordering::SeqCst);
        {unsafe_block} {{ System.alloc(layout) }}
    }}

    unsafe fn dealloc(&self, pointer: *mut u8, layout: Layout) {{
        {unsafe_block} {{ System.dealloc(pointer, layout) }}
    }}
}}

#[global_allocator]
static ALLOCATOR: CountingAllocator = CountingAllocator;

#[path = {:?}]
mod generated;

fn main() {{
    let before = ALLOCATIONS.load(Ordering::SeqCst);
    let out = generated::derivative_rhs(0.0, &[1.0], &[]).unwrap();
    let after = ALLOCATIONS.load(Ordering::SeqCst);
    assert_eq!(before, after, "fixed ODE evaluation allocated");
    assert!((out[0] + 2.0).abs() < 1e-12);
}}
"#,
            module.path,
            unsafe_block = concat!("un", "safe"),
        ),
    )
    .expect("write fixed Rust ODE runtime harness");
    let executable = directory.path().join("rust-fixed-ode-runtime");
    run_checked(
        Command::new("rustc")
            .args(["--edition=2024", "-Dwarnings"])
            .arg(&harness)
            .arg("-o")
            .arg(&executable)
            .current_dir(directory.path()),
        "compile checked fixed Rust ODE target",
    );
    run_checked(
        &mut Command::new(executable),
        "execute checked fixed Rust ODE target",
    );
}

#[test]
fn cuda_ode_generated_kernel_compiles_and_executes_cpu_emulation() {
    let (directory, files) = rendered_target("cuda-ode");
    let source = files
        .iter()
        .find(|file| file.path.ends_with(".cu"))
        .expect("CUDA ODE target emits CUDA source");
    let harness = directory.path().join("main.cpp");
    fs::write(
        &harness,
        format!(
            r#"struct Dim3 {{ int x; }};
static Dim3 blockIdx, blockDim, threadIdx;
#define __global__
#include {:?}

int main() {{
    const double y[2] = {{1.0, 2.0}};
    const double p[2] = {{0.0, 0.0}};
    double out[2] = {{123.0, 123.0}};
    blockIdx.x = 0;
    blockDim.x = 2;
    for (threadIdx.x = 0; threadIdx.x < 2; ++threadIdx.x) {{
        Decay_derivative_rhs_batch(0.0, y, p, out, 1, 1, 1, 2);
    }}
    return out[0] == -2.0 && out[1] == -4.0 ? 0 : 1;
}}
"#,
            source.path,
        ),
    )
    .expect("write CUDA CPU-emulation harness");
    let executable = directory.path().join("cuda-ode-emulation");
    run_checked(
        Command::new("c++")
            .args(["-std=c++17", "-Wall", "-Wextra", "-Werror"])
            .arg(&harness)
            .arg("-o")
            .arg(&executable)
            .current_dir(directory.path()),
        "compile CUDA ODE CPU emulation",
    );
    run_checked(
        &mut Command::new(executable),
        "execute CUDA ODE CPU emulation",
    );
}

#[test]
fn cuda_ode_generated_kernel_compiles_with_required_nvcc() {
    let (directory, files) = rendered_target("cuda-ode");
    let source = files
        .iter()
        .find(|file| file.path.ends_with(".cu"))
        .expect("CUDA ODE target emits CUDA source");
    let available = Command::new("nvcc").arg("--version").output().is_ok();
    if !super::template_runtime_policy::prerequisites_are_available(
        "NVCC compile check",
        &[("NVCC", available)],
    ) {
        return;
    }
    run_checked(
        Command::new("nvcc")
            .args(["-std=c++17", "-c"])
            .arg(directory.path().join(&source.path))
            .arg("-o")
            .arg(directory.path().join("cuda-ode.o")),
        "compile CUDA ODE kernel with NVCC",
    );
}

#[test]
fn casadi_ode_target_imports_evaluates_and_differentiates() {
    let (directory, files) = rendered_target("casadi-ode");
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
fn jax_ode_target_imports_jits_evaluates_and_differentiates() {
    let (directory, files) = rendered_target("jax-ode");
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
adjoint = jax.jacrev(generated.rhs, argnums=1)(0.0, jnp.array([1.0]), jnp.zeros(0), jnp.zeros(0))
assert float(adjoint[0, 0]) == -2.0
"#,
    );
}
