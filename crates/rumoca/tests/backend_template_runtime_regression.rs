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

#[test]
fn only_fmi3_consumes_an_exact_isolable_algebraic_schedule() {
    let compiled = Compiler::new()
        .model("ExactAlgebraic")
        .compile_str(
            r#"
model ExactAlgebraic
  Real x(start = 1);
  Real y(start = 2);
equation
  der(x) = -y;
  0 = y - 2 * x;
end ExactAlgebraic;
"#,
            "ExactAlgebraic.mo",
        )
        .expect("the compiler accepts an exactly isolable algebraic model");

    let fmi3 = rumoca::render_target_files(&compiled, "ExactAlgebraic", "fmi3", None)
        .expect("FMI3 consumes the checked exact-assignment schedule");
    let model_c = fmi3
        .iter()
        .find(|file| file.path == "sources/model.c")
        .expect("FMI3 emits its C kernel");
    assert!(
        model_c.content.contains("refresh_algebraics"),
        "the FMI3 kernel must emit the exact algebraic refresh"
    );
    assert!(
        model_c.content.contains("m->y[1] = r["),
        "the FMI3 kernel must commit the checked algebraic target"
    );
    assert!(
        model_c.content.contains("if (!isfinite(m->y[1]))"),
        "the FMI3 kernel must reject a non-finite refreshed algebraic value"
    );
    execute_emitted_algebraic_refresh(&model_c.content);

    for target in [
        "c-ode",
        "rust-ode",
        "rust-fixed-ode",
        "casadi-ode",
        "jax-ode",
        "cuda-ode",
        "wgsl-ode",
        "fmi2",
        "fmi-ls-wasm",
    ] {
        let error = rumoca::render_target_files(&compiled, "ExactAlgebraic", target, None)
            .expect_err("a target without an exact-assignment consumer must fail closed");
        assert!(
            error
                .to_string()
                .contains("unsupported-feature:residual_equations"),
            "target {target} returned the wrong diagnostic: {error:#}"
        );
    }
}

#[test]
fn fmi3_rejects_a_tunable_algebraic_coefficient() {
    let compiled = Compiler::new()
        .model("TunableAlgebraicCoefficient")
        .compile_str(
            r#"
model TunableAlgebraicCoefficient
  parameter Real a = 2;
  Real x(start = 1);
  Real y(start = 2);
equation
  der(x) = -y;
  0 = a * y - x;
end TunableAlgebraicCoefficient;
"#,
            "TunableAlgebraicCoefficient.mo",
        )
        .expect("the compiler accepts an algebraic model with a tunable coefficient");

    let error = rumoca::render_target_files(&compiled, "TunableAlgebraicCoefficient", "fmi3", None)
        .expect_err("FMI3 must retain a residual solver for a tunable algebraic coefficient");
    assert!(
        error
            .to_string()
            .contains("unsupported-feature:residual_equations"),
        "FMI3 returned the wrong diagnostic: {error:#}"
    );
}

#[test]
fn fmi3_exact_runtime_refreshes_the_final_rk4_state() {
    let rendered = render_fmi3_model(
        "FinalRk4Algebraic",
        r#"
model FinalRk4Algebraic
  Real x(start = 1);
  output Real y(start = 2);
equation
  0 = y - 2*x;
  der(x) = -x;
end FinalRk4Algebraic;
"#,
    );
    execute_emitted_fmi3_kernel(
        &rendered.model_c,
        2,
        1,
        r#"
    ModelInstance model = {0};
    model.y[0] = 1.0; model.y[1] = 2.0;
    model.state = MODEL_STEP; model.type = INTERFACE_CS;
    fmi3Boolean event_needed, terminate, early_return;
    fmi3Float64 last_time;
    if (fmi3DoStep(&model, 0.0, 1.0, fmi3True, &event_needed, &terminate,
                   &early_return, &last_time) != fmi3OK) return 1;
    if (fabs(model.y[0] - 0.375) > 1.0e-12) return 2;
    if (fabs(model.y[1] - 0.75) > 1.0e-12) return 3;
    if (fabs(last_time - 1.0) > 1.0e-12) return 4;
"#,
    );
}

#[test]
fn fmi3_exact_runtime_rolls_back_a_nonfinite_final_algebraic() {
    let rendered = render_fmi3_model(
        "NonfiniteFinalAlgebraic",
        r#"
model NonfiniteFinalAlgebraic
  input Real u(start = 0);
  Real x(start = 1);
  output Real y(start = 1);
equation
  der(x) = -x;
  0 = y - 1/(x-u);
end NonfiniteFinalAlgebraic;
"#,
    );
    let input_vr = rendered.value_reference("u");
    let output_vr = rendered.value_reference("y");
    let body = format!(
        r#"
    ModelInstance model = {{0}};
    model.y[0] = 1.0; model.y[1] = 1.0;
    model.state = MODEL_STEP; model.type = INTERFACE_CS;
    const fmi3ValueReference input_vr = {input_vr};
    const fmi3ValueReference output_vr = {output_vr};
    fmi3Float64 input = 0.375;
    if (fmi3SetFloat64(&model, &input_vr, 1, &input, 1) != fmi3OK) return 1;
    fmi3Float64 output;
    if (fmi3GetFloat64(&model, &output_vr, 1, &output, 1) != fmi3OK) return 2;
    if (fabs(output - 1.6) > 1.0e-12) return 3;
    fmi3Boolean event_needed, terminate, early_return;
    fmi3Float64 last_time;
    if (fmi3DoStep(&model, 0.0, 1.0, fmi3True, &event_needed, &terminate,
                   &early_return, &last_time) != fmi3Error) return 4;
    if (model.time != 0.0 || model.y[0] != 1.0 || model.y[1] != 1.6) return 5;
    fmi3Float64 retained;
    if (fmi3GetFloat64(&model, &input_vr, 1, &retained, 1) != fmi3OK) return 6;
    if (retained != 0.375) return 7;
    input = 0.0;
    if (fmi3SetFloat64(&model, &input_vr, 1, &input, 1) != fmi3OK) return 8;
    if (fmi3GetFloat64(&model, &output_vr, 1, &output, 1) != fmi3OK) return 9;
    if (fabs(output - 1.0) > 1.0e-12) return 10;
    if (fmi3DoStep(&model, 0.0, 0.1, fmi3True, &event_needed, &terminate,
                   &early_return, &last_time) != fmi3OK) return 11;
    if (fabs(last_time - 0.1) > 1.0e-12) return 12;
"#
    );
    execute_emitted_fmi3_kernel(&rendered.model_c, 2, 1, &body);
}

#[test]
fn fmi3_exact_runtime_executes_chained_singletons_in_blt_order() {
    let rendered = render_fmi3_model(
        "ChainedSingletons",
        r#"
model ChainedSingletons
  Real x(start = 3);
  Real a(start = 6);
  output Real b(start = 7);
equation
  der(x) = -b;
  0 = b - (a + 1);
  0 = a - 2*x;
end ChainedSingletons;
"#,
    );
    execute_emitted_fmi3_kernel(
        &rendered.model_c,
        3,
        1,
        r#"
    ModelInstance model = {0};
    model.y[0] = 4.0; model.y[1] = 6.0; model.y[2] = 7.0;
    if (evaluate_derivatives(&model) != fmi3OK) return 1;
    if (fabs(model.y[1] - 8.0) > 1.0e-12) return 2;
    if (fabs(model.y[2] - 9.0) > 1.0e-12) return 3;
    if (fabs(model.derivative[0] + 9.0) > 1.0e-12) return 4;
"#,
    );
}

struct RenderedFmi3 {
    model_c: String,
    model_description: String,
}

impl RenderedFmi3 {
    fn value_reference(&self, name: &str) -> usize {
        let marker = format!("<Float64 name=\"{name}\" valueReference=\"");
        let tail = self
            .model_description
            .split_once(&marker)
            .unwrap_or_else(|| panic!("FMI3 model description declares `{name}`"))
            .1;
        tail.split_once('"')
            .expect("FMI3 value reference terminates")
            .0
            .parse()
            .expect("FMI3 value reference is numeric")
    }
}

fn render_fmi3_model(model: &str, source: &str) -> RenderedFmi3 {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("compile exact FMI3 runtime fixture {model}: {error:#}"));
    let files = rumoca::render_target_files(&compiled, model, "fmi3", None)
        .unwrap_or_else(|error| panic!("render exact FMI3 runtime fixture {model}: {error:#}"));
    let content = |path: &str| {
        files
            .iter()
            .find(|file| file.path == path)
            .unwrap_or_else(|| panic!("FMI3 runtime fixture emits {path}"))
            .content
            .clone()
    };
    RenderedFmi3 {
        model_c: content("sources/model.c"),
        model_description: files
            .iter()
            .find(|file| file.path == "modelDescription.xml")
            .expect("FMI3 runtime fixture emits modelDescription.xml")
            .content
            .clone(),
    }
}

fn execute_emitted_fmi3_kernel(model_c: &str, y_len: usize, state_len: usize, body: &str) {
    let kernel_start = model_c
        .find("static fmi3Status refresh_algebraics")
        .expect("rendered FMI3 C owns an algebraic refresh kernel");
    let kernel_end = model_c[kernel_start..]
        .find("static size_t value_count")
        .map(|offset| kernel_start + offset)
        .expect("rendered FMI3 C terminates its integration kernel");
    let do_step_start = model_c
        .find("FMI_EXPORT fmi3Status fmi3DoStep")
        .expect("rendered FMI3 C owns Co-Simulation stepping");
    let do_step_end = model_c[do_step_start..]
        .find("FMI_EXPORT fmi3Status fmi3ActivateModelPartition")
        .map(|offset| do_step_start + offset)
        .expect("rendered FMI3 C terminates Co-Simulation stepping");
    let kernel = &model_c[kernel_start..kernel_end];
    let value_helpers_start = kernel_end;
    let value_helpers_end = model_c[value_helpers_start..]
        .find("FMI_EXPORT const char* fmi3GetVersion")
        .map(|offset| value_helpers_start + offset)
        .expect("rendered FMI3 C terminates its value helpers");
    let value_helpers = &model_c[value_helpers_start..value_helpers_end];
    let public_values_start = model_c
        .find("FMI_EXPORT fmi3Status fmi3GetFloat64")
        .expect("rendered FMI3 C owns Float64 accessors");
    let public_values_end = model_c[public_values_start..]
        .find("FMI_EXPORT fmi3Status fmi3SetTime")
        .map(|offset| public_values_start + offset)
        .expect("rendered FMI3 C terminates Float64 accessors");
    let public_values = &model_c[public_values_start..public_values_end];
    let do_step = &model_c[do_step_start..do_step_end];
    let driver = format!(
        r#"
#include <math.h>
#include <stddef.h>
#define FMI_EXPORT
typedef void* fmi3Instance;
typedef double fmi3Float64;
typedef int fmi3Boolean;
typedef unsigned int fmi3ValueReference;
typedef enum {{ fmi3OK = 0, fmi3Error = 3 }} fmi3Status;
enum {{ fmi3False = 0, fmi3True = 1 }};
enum ModelState {{ MODEL_INSTANTIATED, MODEL_INITIALIZATION, MODEL_EVENT, MODEL_CONTINUOUS, MODEL_STEP, MODEL_TERMINATED }};
enum InterfaceType {{ INTERFACE_ME, INTERFACE_CS }};
#define Y_LEN {y_len}
#define P_LEN 1
#define STATE_LEN {state_len}
typedef struct {{ double time; double y[Y_LEN]; double p[P_LEN]; double derivative[STATE_LEN]; enum ModelState state; enum InterfaceType type; }} ModelInstance;
{kernel}
{value_helpers}
{public_values}
{do_step}
int main(void) {{
{body}
    return 0;
}}
"#
    );
    compile_and_run_c(&driver, "exact FMI3 runtime kernel");
}

fn compile_and_run_c(driver: &str, label: &str) {
    let work = tempfile::tempdir().expect("create emitted C test directory");
    let source = work.path().join("driver.c");
    let executable = work.path().join("driver");
    fs::write(&source, driver).expect("write emitted C test driver");
    let output = Command::new("cc")
        .args(["-std=c99", "-Wall", "-Wextra", "-Wpedantic", "-Werror"])
        .arg(&source)
        .args(["-lm", "-o"])
        .arg(&executable)
        .output()
        .unwrap_or_else(|error| panic!("start {label} compiler: {error}"));
    assert!(
        output.status.success(),
        "compile {label}: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let output = Command::new(executable)
        .output()
        .unwrap_or_else(|error| panic!("execute {label}: {error}"));
    assert!(
        output.status.success(),
        "execute {label}: status {:?}, stderr: {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn execute_emitted_algebraic_refresh(model_c: &str) {
    let start = model_c
        .find("static fmi3Status refresh_algebraics")
        .expect("rendered FMI3 C owns an algebraic refresh kernel");
    let tail = &model_c[start..];
    let end = tail
        .find("static fmi3Status evaluate_derivatives")
        .expect("rendered FMI3 C terminates the refresh kernel");
    let refresh = &tail[..end];
    let driver = format!(
        r#"
#include <math.h>
#include <stddef.h>
typedef enum {{ fmi3OK = 0, fmi3Error = 3 }} fmi3Status;
typedef struct {{ double time; double y[2]; double p[1]; }} ModelInstance;
{refresh}
int main(void) {{
    ModelInstance model = {{0}};
    model.y[0] = 3.0;
    if (refresh_algebraics(&model) != fmi3OK) return 1;
    if (fabs(model.y[1] - 6.0) > 1.0e-12) return 2;
    model.y[0] = NAN;
    if (refresh_algebraics(&model) != fmi3Error) return 3;
    return 0;
}}
"#
    );
    let work = tempfile::tempdir().expect("create exact algebraic C test directory");
    let source = work.path().join("exact_algebraic.c");
    let executable = work.path().join("exact_algebraic");
    fs::write(&source, driver).expect("write exact algebraic C test driver");
    let output = Command::new("cc")
        .args(["-std=c99", "-Wall", "-Wextra", "-Wpedantic", "-Werror"])
        .arg(&source)
        .args(["-lm", "-o"])
        .arg(&executable)
        .output()
        .expect("compile emitted exact algebraic C kernel");
    assert!(
        output.status.success(),
        "compile emitted exact algebraic C kernel: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let output = Command::new(executable)
        .output()
        .expect("execute emitted exact algebraic C kernel");
    assert!(
        output.status.success(),
        "execute emitted exact algebraic C kernel: status {:?}, stderr: {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stderr)
    );
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
