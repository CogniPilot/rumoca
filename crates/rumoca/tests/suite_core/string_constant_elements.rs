//! Translation-time String operations in executable expressions (MLS §3.4/§3.5).
use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package StringElements
  package Data
    constant String siteNames[2] = {"imu", "camera"};
  end Data;
  model Initial
    Real x(start=1, fixed=true);
  initial equation
    assert(Data.siteNames[1] == "imu" and Data.siteNames[2] == "camera", "roster mismatch");
  equation
    der(x) = -x;
  end Initial;
  model Equation
    Real x(start=1, fixed=true);
  equation
    assert(Data.siteNames[1] <> "camera", "roster mismatch");
    der(x) = -x;
  end Equation;
  model Algorithm
    Real x(start=1, fixed=true);
  initial algorithm
    assert(Data.siteNames[1] == "imu", "roster mismatch");
  equation
    der(x) = -x;
  end Algorithm;
  model Conditional
    package D = Data;
    Real x(start=1, fixed=true);
  equation
    der(x) = -(if "head_" + D.siteNames[2] == "head_camera" then 2 else 3)*x;
  end Conditional;
  model Mixed
    parameter Real limit = -1;
    Real x(start=1, fixed=true);
  equation
    assert(Data.siteNames[1] == "imu" and time >= limit, "runtime guard");
    der(x) = -x;
  end Mixed;
  model Violated
    Real x(start=1, fixed=true);
  initial equation
    assert(Data.siteNames[1] == "camera", "roster mismatch");
  equation
    der(x) = -x;
  end Violated;
end StringElements;
"#;

fn run(model: &str) -> Result<rumoca_sim::SimResult, String> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, "StringElements.mo")
        .map_err(|error| format!("{error:?}"))?;
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.02,
            dt: Some(0.01),
            solver_mode: SimSolverMode::RkLike,
            ..SimOptions::default()
        },
    )
    .map_err(|error| format!("{error:?}"))
}

#[test]
fn constant_string_elements_in_initial_assertions_simulate() {
    run("StringElements.Initial")
        .expect("constant roster check must not require numeric String storage");
}

#[test]
fn constant_string_elements_in_equation_assertions_simulate() {
    run("StringElements.Equation").expect("String inequality folds before numeric lowering");
}

#[test]
fn constant_string_elements_in_algorithm_assertions_simulate() {
    run("StringElements.Algorithm")
        .expect("initial algorithm uses the same translation-time String values");
}

#[test]
fn constant_string_concatenation_selects_the_numeric_branch() {
    let result = run("StringElements.Conditional").expect("package alias and concatenation fold");
    let x = result.names.iter().position(|name| name == "x").unwrap();
    for (&time, &actual) in result.times.iter().zip(&result.data[x]) {
        assert!(
            (actual - (-2.0 * time).exp()).abs() < 1e-5,
            "x at {time}: {actual}"
        );
    }
}

#[test]
fn mixed_string_and_runtime_assertion_preserves_the_numeric_guard() {
    run("StringElements.Mixed").expect("constant String part folds beside a tunable numeric guard");
    let source = SOURCE.replace("parameter Real limit = -1", "parameter Real limit = 1");
    let compiled = Compiler::new()
        .model("StringElements.Mixed")
        .compile_str(&source, "StringElements.mo")
        .unwrap();
    let error = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.02,
            ..SimOptions::default()
        },
    )
    .unwrap_err();
    assert!(format!("{error:?}").contains("runtime guard"));
}

#[test]
fn a_false_constant_string_assertion_is_not_dropped() {
    let error = run("StringElements.Violated").expect_err("the roster condition is false");
    assert!(
        error.contains("roster mismatch"),
        "unexpected refusal: {error}"
    );
    assert!(
        !error.contains("String values are not numeric"),
        "must report the violated assertion: {error}"
    );
}
