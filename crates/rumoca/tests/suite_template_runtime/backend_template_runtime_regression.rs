mod jax_lazy_conditionals;
mod selected_regions;

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
    rendered_model_target(&result, "Decay", target)
}

fn rendered_model_target(
    result: &rumoca::CompilationResult,
    model: &str,
    target: &str,
) -> (tempfile::TempDir, Vec<RenderedTargetFile>) {
    let files =
        rumoca::render_target_files(result, model, target, None).expect("checked target renders");
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

    for (target, model_c_path) in [
        ("fmi2", "sources/model.c"),
        ("fmi3", "sources/model.c"),
        ("fmi-ls-wasm", "csrc/model.c"),
    ] {
        let files = rumoca::render_target_files(&compiled, "ImplicitAlgebraic", target, None)
            .expect("FMI projects the implicit algebraic block with the shared ME kernel");
        let model_c = files
            .iter()
            .find(|file| file.path == model_c_path)
            .expect("FMI emits its C kernel");
        assert!(
            model_c.content.contains("rmc_project_stage"),
            "the FMI kernel must project the coupled block"
        );
    }
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
fn fmi2_and_fmi3_consume_an_exact_isolable_algebraic_schedule() {
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

    for target in ["fmi2", "fmi3"] {
        let files = rumoca::render_target_files(&compiled, "ExactAlgebraic", target, None)
            .expect("FMI consumes the checked exact-assignment schedule");
        let model_c = source_unit(&files, "sources/model.c");
        let assign_c = source_unit(&files, "sources/rmc_assign.c");
        assert!(
            model_c.contains("refresh_algebraics"),
            "the FMI kernel must emit the exact algebraic refresh"
        );
        assert!(
            assign_c.contains("m->y[1] = r["),
            "the FMI kernel must commit the checked algebraic target"
        );
        assert!(
            assign_c.contains("if (!isfinite(m->y[1]))"),
            "the FMI kernel must reject a non-finite refreshed algebraic value"
        );
        execute_emitted_algebraic_refresh(&model_c, &assign_c, target);
    }

    // fmi-ls-wasm renders the same C kernel as fmi3, so it consumes the exact
    // schedule through the identical translation units under csrc/.
    {
        let files = rumoca::render_target_files(&compiled, "ExactAlgebraic", "fmi-ls-wasm", None)
            .expect("fmi-ls-wasm consumes the checked exact-assignment schedule");
        let model_c = source_unit(&files, "csrc/model.c");
        let assign_c = source_unit(&files, "csrc/rmc_assign.c");
        assert!(model_c.contains("refresh_algebraics"));
        assert!(assign_c.contains("m->y[1] = r["));
        assert!(assign_c.contains("if (!isfinite(m->y[1]))"));
    }

    // The scalar ODE right-hand-side targets consume the same schedule: they
    // assign every algebraic slot in-function before the derivative rows.
    for target in [
        "c-ode",
        "rust-ode",
        "rust-fixed-ode",
        "casadi-ode",
        "jax-ode",
        "cuda-ode",
    ] {
        let files = rumoca::render_target_files(&compiled, "ExactAlgebraic", target, None)
            .unwrap_or_else(|error| panic!("{target} consumes the exact schedule: {error:#}"));
        assert!(
            files
                .iter()
                .any(|file| file.content.contains("Algebraic refresh")),
            "{target} must emit the in-function algebraic refresh"
        );
    }
    execute_c_ode_algebraic_refresh(&compiled);

    // wgsl-ode renders rows as expressions over a read-only `y` buffer and has
    // no exact-assignment consumer, so it still fails closed.
    let error = rumoca::render_target_files(&compiled, "ExactAlgebraic", "wgsl-ode", None)
        .expect_err("a target without an exact-assignment consumer must fail closed");
    assert!(
        error
            .to_string()
            .contains("unsupported-feature:residual_equations"),
        "wgsl-ode returned the wrong diagnostic: {error:#}"
    );
}

/// Compile and call the emitted c-ode RHS with a deliberately stale algebraic
/// slot: `y = 2*x` must be recomputed in-function, so `der(x) = -y = -3` at
/// `x = 1.5` whatever the caller left in `y[1]`.
fn execute_c_ode_algebraic_refresh(compiled: &rumoca::CompilationResult) {
    let files = rumoca::render_target_files(compiled, "ExactAlgebraic", "c-ode", None)
        .expect("c-ode renders the exact algebraic model");
    let header = files
        .iter()
        .find(|file| file.path == "ExactAlgebraic_ode.h")
        .expect("c-ode emits its header")
        .content
        .clone();
    let source = files
        .iter()
        .find(|file| file.path == "ExactAlgebraic_ode.c")
        .expect("c-ode emits its source")
        .content
        .replace("#include \"ExactAlgebraic_ode.h\"", "");
    let driver = format!(
        r#"{header}
{source}
int main(void) {{
    double y[RUMOCA_EXACTALGEBRAIC_Y_LEN] = {{0}};
    double p[RUMOCA_EXACTALGEBRAIC_P_LEN + 1] = {{0}};
    double out[RUMOCA_EXACTALGEBRAIC_DERIVATIVE_LEN] = {{0}};
    y[0] = 1.5;
    y[1] = 999.0;
    if (ExactAlgebraic_derivative_rhs(0.0, y, p, out) != 0) return 1;
    return fabs(out[0] + 3.0) < 1e-12 ? 0 : 2;
}}
"#
    );
    compile_and_run_c(&driver, "c-ode exact algebraic refresh");
}

/// `der(x) = -y` with the exact algebraic assignment `y = 2*x`.
fn exact_algebraic() -> rumoca::CompilationResult {
    Compiler::new()
        .model("ExactAlgebraic")
        .compile_str(
            "model ExactAlgebraic Real x(start = 1); Real y(start = 2); \
             equation der(x) = -y; 0 = y - 2 * x; end ExactAlgebraic;",
            "ExactAlgebraic.mo",
        )
        .expect("the compiler accepts an exactly isolable algebraic model")
}

/// The fixed Rust ODE target refreshes algebraic slots for the host and names
/// every slot, so a host reads `y = 2*x = 3` at `x = 1.5` by name.
#[test]
fn rust_fixed_ode_refreshes_algebraics_into_named_slots() {
    let (directory, files) =
        rendered_model_target(&exact_algebraic(), "ExactAlgebraic", "rust-fixed-ode");
    let module = files
        .iter()
        .find(|file| file.path.ends_with(".rs"))
        .expect("fixed Rust ODE target emits a Rust module");
    fs::copy(
        directory.path().join(&module.path),
        directory.path().join("generated.rs"),
    )
    .expect("copy fixed Rust ODE module beside its normal module root");
    let harness = directory.path().join("main.rs");
    fs::write(
        &harness,
        r#"mod generated;

fn main() {
    let x = generated::slot(generated::Y_SLOTS, "x").expect("state slot is named");
    let y_slot = generated::slot(generated::Y_SLOTS, "y").expect("algebraic slot is named");
    assert_eq!(generated::slot(generated::Y_SLOTS, "missing"), None);
    assert_eq!(generated::slot(generated::P_SLOTS, "x"), None);
    let mut y = [0.0; generated::Y_LEN];
    y[x] = 1.5;
    y[y_slot] = 999.0;
    generated::refresh_algebraics(0.0, &mut y, &generated::DEFAULT_P).unwrap();
    assert!((y[y_slot] - 3.0).abs() < 1e-12);
    assert_eq!(y[x], 1.5);
}
"#,
    )
    .expect("write fixed Rust ODE refresh harness");
    let executable = directory.path().join("rust-fixed-ode-refresh");
    run_checked(
        Command::new("rustc")
            .args(["--edition=2024", "-Dwarnings"])
            .arg(&harness)
            .arg("-o")
            .arg(&executable)
            .current_dir(directory.path()),
        "compile fixed Rust ODE refresh harness",
    );
    run_checked(
        &mut Command::new(executable),
        "execute fixed Rust ODE refresh harness",
    );
}

/// The c-ode target refreshes algebraic slots for the host and resolves slot
/// names, so a host reads `y = 2*x = 3` at `x = 1.5` by name.
#[test]
fn c_ode_refreshes_algebraics_into_named_slots() {
    let files = rumoca::render_target_files(&exact_algebraic(), "ExactAlgebraic", "c-ode", None)
        .expect("c-ode renders the exact algebraic model");
    let header = source_unit(&files, "ExactAlgebraic_ode.h");
    let source = source_unit(&files, "ExactAlgebraic_ode.c")
        .replace("#include \"ExactAlgebraic_ode.h\"", "");
    let driver = format!(
        r#"{header}
{source}
int main(void) {{
    double y[RUMOCA_EXACTALGEBRAIC_Y_LEN] = {{0}};
    double p[RUMOCA_EXACTALGEBRAIC_P_LEN + 1] = {{0}};
    int x = ExactAlgebraic_y_slot("x");
    int y_slot = ExactAlgebraic_y_slot("y");
    if (x < 0 || y_slot < 0) return 1;
    if (ExactAlgebraic_y_slot("missing") != -1 || ExactAlgebraic_p_slot("x") != -1) return 2;
    y[x] = 1.5;
    y[y_slot] = 999.0;
    if (ExactAlgebraic_refresh_algebraics(0.0, y, p) != 0) return 3;
    return fabs(y[y_slot] - 3.0) < 1e-12 && y[x] == 1.5 ? 0 : 4;
}}
"#
    );
    compile_and_run_c(&driver, "c-ode algebraic refresh into named slots");
}

/// `der(x) = -b` through the chain `m = a*x`, `b = m + 1` with a tunable `a`.
fn chained_tunable_algebraic() -> rumoca::CompilationResult {
    Compiler::new()
        .model("ChainedTunable")
        .compile_str(
            "model ChainedTunable parameter Real a = 2; Real x(start = 1); \
             Real m(start = 2); output Real b(start = 3); \
             equation der(x) = -b; 0 = m - a * x; 0 = b - (m + 1); end ChainedTunable;",
            "ChainedTunable.mo",
        )
        .expect("the compiler accepts a chained algebraic model with a tunable coefficient")
}

/// Writes the fixed Rust ODE module beside `main.rs` and runs `main_body`
/// against it as `mod generated`.
fn run_fixed_rust_ode_harness(
    result: &rumoca::CompilationResult,
    model: &str,
    main_body: &str,
    label: &str,
) {
    let (directory, files) = rendered_model_target(result, model, "rust-fixed-ode");
    let module = files
        .iter()
        .find(|file| file.path.ends_with(".rs"))
        .expect("fixed Rust ODE target emits a Rust module");
    fs::copy(
        directory.path().join(&module.path),
        directory.path().join("generated.rs"),
    )
    .expect("copy fixed Rust ODE module beside its normal module root");
    let harness = directory.path().join("main.rs");
    fs::write(
        &harness,
        format!("mod generated;\n\nfn main() {{\n{main_body}\n}}\n"),
    )
    .expect("write fixed Rust ODE harness");
    let executable = directory.path().join("rust-fixed-ode-harness");
    run_checked(
        Command::new("rustc")
            .args(["--edition=2024", "-Dwarnings"])
            .arg(&harness)
            .arg("-o")
            .arg(&executable)
            .current_dir(directory.path()),
        &format!("compile {label}"),
    );
    run_checked(&mut Command::new(executable), &format!("execute {label}"));
}

/// A chained algebraic refresh runs in dependency order, reads the tunable
/// parameter the host passes in `p`, and exposes the output by name.
#[test]
fn rust_fixed_ode_refresh_follows_chain_and_tunable_parameter() {
    run_fixed_rust_ode_harness(
        &chained_tunable_algebraic(),
        "ChainedTunable",
        r#"
    let x = generated::slot(generated::Y_SLOTS, "x").expect("state slot is named");
    let m = generated::slot(generated::Y_SLOTS, "m").expect("algebraic slot is named");
    let b = generated::slot(generated::Y_SLOTS, "b").expect("output slot is named");
    let a = generated::slot(generated::P_SLOTS, "a").expect("parameter slot is named");
    assert_eq!(generated::slot(generated::Y_SLOTS, "a"), None);
    let mut p = generated::DEFAULT_P;
    let mut y = [0.0; generated::Y_LEN];
    y[x] = 1.5;
    generated::refresh_algebraics(0.0, &mut y, &p).unwrap();
    assert!((y[m] - 3.0).abs() < 1e-12 && (y[b] - 4.0).abs() < 1e-12);
    p[a] = 4.0;
    generated::refresh_algebraics(0.0, &mut y, &p).unwrap();
    assert!((y[m] - 6.0).abs() < 1e-12 && (y[b] - 7.0).abs() < 1e-12);
    assert_eq!(y[x], 1.5);
"#,
        "fixed Rust ODE chained refresh harness",
    );
}

/// Every slot name maps to a distinct in-bounds slot, so a host never aliases
/// two variables or indexes past `State`.
#[test]
fn rust_fixed_ode_slot_tables_are_unique_and_in_bounds() {
    run_fixed_rust_ode_harness(
        &chained_tunable_algebraic(),
        "ChainedTunable",
        r#"
    for (table, len) in [
        (generated::Y_SLOTS, generated::Y_LEN),
        (generated::P_SLOTS, generated::P_LEN),
    ] {
        let mut seen = std::collections::BTreeSet::new();
        for (name, slot) in table {
            assert!(*slot < len, "{name} slot {slot} is outside 0..{len}");
            assert!(seen.insert(*slot), "{name} aliases slot {slot}");
        }
    }
    assert_eq!(generated::Y_SLOTS.len(), generated::Y_LEN);
"#,
        "fixed Rust ODE slot table harness",
    );
}

/// A model with no algebraics refreshes as a no-op and names only its state.
#[test]
fn rust_fixed_ode_refresh_is_a_no_op_without_algebraics() {
    run_fixed_rust_ode_harness(
        &checked_decay(),
        "Decay",
        r#"
    assert_eq!(generated::Y_SLOTS, &[("x", 0)]);
    let mut y = [1.25; generated::Y_LEN];
    generated::refresh_algebraics(0.0, &mut y, &generated::DEFAULT_P).unwrap();
    assert_eq!(y, [1.25; generated::Y_LEN]);
"#,
        "fixed Rust ODE no-algebraic refresh harness",
    );
}

/// The c-ode refresh follows the same chain, honours a host-supplied
/// parameter, and names the output; unknown names resolve to -1.
#[test]
fn c_ode_refresh_follows_chain_and_tunable_parameter() {
    let files = rumoca::render_target_files(
        &chained_tunable_algebraic(),
        "ChainedTunable",
        "c-ode",
        None,
    )
    .expect("c-ode renders the chained algebraic model");
    let header = source_unit(&files, "ChainedTunable_ode.h");
    let source = source_unit(&files, "ChainedTunable_ode.c")
        .replace("#include \"ChainedTunable_ode.h\"", "");
    let driver = format!(
        r#"{header}
{source}
int main(void) {{
    double y[RUMOCA_CHAINEDTUNABLE_Y_LEN] = {{0}};
    double p[RUMOCA_CHAINEDTUNABLE_P_LEN + 1] = {{0}};
    int x = ChainedTunable_y_slot("x");
    int m = ChainedTunable_y_slot("m");
    int b = ChainedTunable_y_slot("b");
    int a = ChainedTunable_p_slot("a");
    if (x < 0 || m < 0 || b < 0 || a < 0) return 1;
    if (ChainedTunable_y_slot("a") != -1 || ChainedTunable_p_slot("b") != -1) return 2;
    y[x] = 1.5;
    p[a] = 2.0;
    if (ChainedTunable_refresh_algebraics(0.0, y, p) != 0) return 3;
    if (fabs(y[m] - 3.0) > 1e-12 || fabs(y[b] - 4.0) > 1e-12) return 4;
    p[a] = 4.0;
    if (ChainedTunable_refresh_algebraics(0.0, y, p) != 0) return 5;
    if (fabs(y[m] - 6.0) > 1e-12 || fabs(y[b] - 7.0) > 1e-12) return 6;
    return y[x] == 1.5 ? 0 : 7;
}}
"#
    );
    compile_and_run_c(&driver, "c-ode chained algebraic refresh");
}

/// A model with no algebraics refreshes as a no-op and names only its state.
#[test]
fn c_ode_refresh_is_a_no_op_without_algebraics() {
    let files = rumoca::render_target_files(&checked_decay(), "Decay", "c-ode", None)
        .expect("c-ode renders the decay model");
    let header = source_unit(&files, "Decay_ode.h");
    let source = source_unit(&files, "Decay_ode.c").replace("#include \"Decay_ode.h\"", "");
    let driver = format!(
        r#"{header}
{source}
int main(void) {{
    double y[RUMOCA_DECAY_Y_LEN] = {{1.25}};
    double p[RUMOCA_DECAY_P_LEN + 1] = {{0}};
    if (Decay_y_slot("x") != 0 || Decay_y_slot("y") != -1) return 1;
    if (Decay_refresh_algebraics(0.0, y, p) != 0) return 2;
    return y[0] == 1.25 ? 0 : 3;
}}
"#
    );
    compile_and_run_c(&driver, "c-ode no-algebraic refresh");
}

/// Solve lowering compacts an algebraic `for` family into one pure call
/// (SPEC_0043 §6c). The scalar ODE targets inline its body, so the family
/// stays renderable; c-ode evaluates it to the reference values.
#[test]
fn ode_targets_inline_a_compacted_algebraic_family_call() {
    let compiled = Compiler::new()
        .model("FamilyCall")
        .compile_str(
            r#"
model FamilyCall
  parameter Real b[3] = {0.1, 0.2, 0.3};
  Real x[3](start = {1, 2, 3}, each fixed = true);
  Real a[3];
equation
  for i in 1:3 loop
    a[i] = tanh(x[i] + b[i]);
  end for;
  der(x) = -a;
end FamilyCall;
"#,
            "FamilyCall.mo",
        )
        .expect("the compiler accepts an algebraic family");
    for target in [
        "c-ode",
        "rust-ode",
        "rust-fixed-ode",
        "casadi-ode",
        "jax-ode",
        "cuda-ode",
    ] {
        rumoca::render_target_files(&compiled, "FamilyCall", target, None)
            .unwrap_or_else(|error| panic!("{target} inlines the family call: {error:#}"));
    }
    let files = rumoca::render_target_files(&compiled, "FamilyCall", "c-ode", None)
        .expect("c-ode renders the family call");
    let header = source_unit(&files, "FamilyCall_ode.h");
    let source =
        source_unit(&files, "FamilyCall_ode.c").replace("#include \"FamilyCall_ode.h\"", "");
    let driver = format!(
        r#"{header}
{source}
int main(void) {{
    double y[RUMOCA_FAMILYCALL_Y_LEN] = {{0}};
    double out[RUMOCA_FAMILYCALL_DERIVATIVE_LEN] = {{0}};
    const double b[3] = {{0.1, 0.2, 0.3}};
    for (int i = 0; i < 3; ++i) y[i] = 1.0 + i;
    if (FamilyCall_derivative_rhs(0.0, y, FamilyCall_default_p, out) != 0) return 1;
    for (int i = 0; i < 3; ++i) {{
        if (fabs(out[i] + tanh(y[i] + b[i])) > 1e-14) return 2;
    }}
    return 0;
}}
"#
    );
    compile_and_run_c(&driver, "c-ode inlined family call");
}

#[test]
fn fmi2_and_fmi3_refresh_through_a_tunable_algebraic_coefficient() {
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

    for target in ["fmi2", "fmi3"] {
        let files =
            rumoca::render_target_files(&compiled, "TunableAlgebraicCoefficient", target, None)
                .expect("FMI refreshes an algebraic through a tunable coefficient");
        let refresh = source_unit(&files, "sources/rmc_assign.c");
        assert!(
            refresh.contains("m->p[0]"),
            "the refresh reads the tunable coefficient at run time instead of a folded value"
        );
    }
}

#[test]
fn fmi3_exact_runtime_refreshes_the_final_step_state() {
    let rendered = render_fmi3_model(
        "FinalStepAlgebraic",
        r#"
model FinalStepAlgebraic
  Real x(start = 1);
  output Real y(start = 2);
equation
  0 = y - 2*x;
  der(x) = -x;
end FinalStepAlgebraic;
"#,
    );
    execute_emitted_fmi3_kernel(
        &rendered,
        r#"
    model.y[0] = 1.0; model.y[1] = 2.0;
    model.state = MODEL_STEP; model.type = INTERFACE_CS;
    fmi3Boolean event_needed, terminate, early_return;
    fmi3Float64 last_time;
    if (fmi3DoStep(&model, 0.0, 1.0, fmi3True, &event_needed, &terminate,
                   &early_return, &last_time) != fmi3OK) return 1;
    if (fabs(model.y[0] - exp(-1.0)) > 1.0e-5) return 2;
    if (fabs(model.y[1] - 2.0 * model.y[0]) > 1.0e-12) return 3;
    if (fabs(last_time - 1.0) > 1.0e-12) return 4;
"#,
    );
}

/// A communication step the error controller cannot complete, here across the
/// finite-time blow-up of `der(x) = x^2` at `t = 1`, is discarded and rolled
/// back instead of returning a finite but meaningless state (issue #361).
#[test]
fn fmi3_exact_runtime_discards_a_step_its_controller_cannot_complete() {
    let rendered = render_fmi3_model(
        "FiniteTimeBlowUp",
        r#"
model FiniteTimeBlowUp
  Real x(start = 1);
  output Real y(start = 1);
equation
  der(x) = x^2;
  0 = y - x;
end FiniteTimeBlowUp;
"#,
    );
    execute_emitted_fmi3_kernel(
        &rendered,
        r#"
    model.y[0] = 1.0; model.y[1] = 1.0;
    model.state = MODEL_STEP; model.type = INTERFACE_CS;
    fmi3Boolean event_needed, terminate, early_return;
    fmi3Float64 last_time = -1.0;
    if (fmi3DoStep(&model, 0.0, 0.5, fmi3True, &event_needed, &terminate,
                   &early_return, &last_time) != fmi3OK) return 1;
    if (fabs(model.y[0] - 2.0) > 1.0e-5) return 2;
    if (fmi3DoStep(&model, 0.5, 1.0, fmi3True, &event_needed, &terminate,
                   &early_return, &last_time) != fmi3Discard) return 3;
    if (model.time != 0.5 || last_time != 0.5 || fabs(model.y[0] - 2.0) > 1.0e-5) return 4;
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
  0 = y - sqrt(x-u);
end NonfiniteFinalAlgebraic;
"#,
    );
    let input_vr = rendered.value_reference("u");
    let output_vr = rendered.value_reference("y");
    let body = format!(
        r#"
    model.y[0] = 1.0; model.y[1] = 1.0;
    model.state = MODEL_STEP; model.type = INTERFACE_CS;
    const fmi3ValueReference input_vr = {input_vr};
    const fmi3ValueReference output_vr = {output_vr};
    fmi3Float64 input = 0.375;
    if (fmi3SetFloat64(&model, &input_vr, 1, &input, 1) != fmi3OK) return 1;
    fmi3Float64 output;
    if (fmi3GetFloat64(&model, &output_vr, 1, &output, 1) != fmi3OK) return 2;
    if (fabs(output - sqrt(0.625)) > 1.0e-12) return 3;
    fmi3Boolean event_needed, terminate, early_return;
    fmi3Float64 last_time;
    if (fmi3DoStep(&model, 0.0, 1.0, fmi3True, &event_needed, &terminate,
                   &early_return, &last_time) != fmi3Error) return 4;
    if (model.time != 0.0 || model.y[0] != 1.0 || model.y[1] != sqrt(0.625)) return 5;
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
    execute_emitted_fmi3_kernel(&rendered, &body);
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
        &rendered,
        r#"
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
    model_h: String,
    assign_c: String,
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
        model_h: content("sources/model.h"),
        assign_c: content("sources/rmc_assign.c").replace("#include \"model.h\"", ""),
        model_description: files
            .iter()
            .find(|file| file.path == "modelDescription.xml")
            .expect("FMI3 runtime fixture emits modelDescription.xml")
            .content
            .clone(),
    }
}

fn execute_emitted_fmi3_kernel(rendered: &RenderedFmi3, body: &str) {
    let model_c = &rendered.model_c;
    let assign_c = &rendered.assign_c;
    // Keep the emitted layout and observer declarations; this isolated driver
    // supplies the FMI ABI types instead of linking the vendor entry points.
    let model_h = rendered.model_h.replace("#include \"fmi3Functions.h\"", "");
    let kernel_start = model_c
        .find("#include \"model.h\"")
        .map(|offset| offset + "#include \"model.h\"".len())
        .expect("rendered FMI3 C owns its initialization and integration kernels");
    let kernel_end = model_c[kernel_start..]
        .find("enum RmcColumn")
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
    let do_step = &model_c[do_step_start..do_step_end];
    let driver = format!(
        r#"
#include <float.h>
#include <math.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define FMI3_Export
typedef void* fmi3Instance;
typedef void* fmi3InstanceEnvironment;
typedef double fmi3Float64;
typedef int32_t fmi3Int32;
typedef int64_t fmi3Int64;
typedef const char* fmi3String;
typedef int fmi3Boolean;
typedef unsigned int fmi3ValueReference;
typedef enum {{ fmi3OK = 0, fmi3Discard = 2, fmi3Error = 3 }} fmi3Status;
typedef void (*fmi3LogMessageCallback)(void*, fmi3Status, fmi3String, fmi3String);
enum {{ fmi3False = 0, fmi3True = 1 }};
{model_h}
{assign_c}
{kernel}
{value_helpers}
{do_step}
int main(void) {{
    ModelInstance model = {{0}};
    initialize_values(&model);
    if (initialize_assertions(&model) != fmi3OK) return 90;
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

fn execute_emitted_algebraic_refresh(model_c: &str, assign_c: &str, prefix: &str) {
    let start = model_c
        .find(&format!("static {prefix}Status update_discrete_equations"))
        .expect("rendered FMI C owns parameter and algebraic refresh");
    let tail = &model_c[start..];
    let end = tail
        .find(&format!("static {prefix}Status settle_values"))
        .expect("rendered FMI C terminates the algebraic refresh kernel");
    let refresh = &tail[..end];
    let assign = assign_c.replace("#include \"model.h\"", "");
    let driver = format!(
        r#"
#include <math.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>
#define Y_LEN 2
typedef enum {{ {prefix}OK = 0, {prefix}Error = 3 }} {prefix}Status;
#define RMC_API
typedef struct {{ bool parameters_dirty; double time; double y[2]; double p[1]; }} ModelInstance;
{assign}
{refresh}
int main(void) {{
    ModelInstance model = {{0}};
    (void)update_assertion_memory;
    model.y[0] = 3.0;
    if (refresh_algebraics(&model) != {prefix}OK) return 1;
    if (fabs(model.y[1] - 6.0) > 1.0e-12) return 2;
    if (refresh_derivative_values(&model) != {prefix}OK || fabs(model.y[1] - 6.0) > 1.0e-12) return 4;
    model.y[0] = NAN;
    if (refresh_algebraics(&model) != {prefix}Error) return 3;
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

    // Pin the private `dae_backend::TEMPLATE_SCHEMA_VERSION` so a changed
    // semantic projection requires an explicit review of this consumer.
    assert_eq!(rendered, "rumoca.checked-dae-template:9");
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
    let generated_module = directory.path().join("generated.rs");
    fs::copy(directory.path().join(&module.path), &generated_module)
        .expect("copy Rust ODE module beside its normal module root");
    let harness = directory.path().join("main.rs");
    fs::write(
        &harness,
        r#"mod generated;

fn main() {
    let mut out = [123.0];
    generated::derivative_rhs(0.0, &[1.0], &[], &mut out).unwrap();
    assert!((out[0] + 2.0).abs() < 1e-12);
}
"#,
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
    let generated_module = directory.path().join("generated.rs");
    fs::copy(directory.path().join(&module.path), &generated_module)
        .expect("copy fixed Rust ODE module beside its normal module root");
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

mod generated;

fn main() {{
    let before = ALLOCATIONS.load(Ordering::SeqCst);
    let out = generated::derivative_rhs(0.0, &[1.0], &[]).unwrap();
    let after = ALLOCATIONS.load(Ordering::SeqCst);
    assert_eq!(before, after, "fixed ODE evaluation allocated");
    assert!((out[0] + 2.0).abs() < 1e-12);
}}
"#,
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
#define __host__
#define __device__
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
    assert_jax_decay(directory.path(), &files);
}

#[test]
fn jax_ode_target_handles_reported_explicit_algebraic_assignments() {
    let compiled = Compiler::new()
        .model("B")
        .compile_str(
            include_str!(
                "../../../rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_418.mo"
            ),
            "B.mo",
        )
        .expect("the original issue 418 model compiles");
    let (directory, files) = rendered_model_target(&compiled, "B", "jax-ode");
    assert_jax_decay(directory.path(), &files);
}

fn assert_jax_decay(directory: &Path, files: &[RenderedTargetFile]) {
    let module = files
        .iter()
        .find(|file| file.path.ends_with(".py"))
        .expect("JAX target emits Python");
    run_python(
        &directory.join(&module.path),
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

/// One rendered source unit's text.
fn source_unit(files: &[RenderedTargetFile], path: &str) -> String {
    files
        .iter()
        .find(|file| file.path == path)
        .unwrap_or_else(|| panic!("FMI emits {path}"))
        .content
        .clone()
}
