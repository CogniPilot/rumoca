//! MLS §8.3.7 assertion levels.
//!
//! "If the level is AssertionLevel.warning, the current evaluation is not
//! aborted", and "the assert(..) statement shall have no influence on the
//! behavior of the model. For example, by evaluating the condition to report
//! the message an event is not triggered." A violated warning-level
//! assertion, in a function body (the pump characteristics of
//! `Modelica.Fluid.Machines`) or in an equation section (the table
//! extrapolation checks of `Modelica.Blocks.Tables`), never fails the
//! simulation and leaves the trace bit-identical; each site is reported once
//! as SPEC_0008 `WX001` at the first accepted point that observes it. An
//! explicit `AssertionLevel.error` is the default level and fails it.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

const MODEL: &str = r#"
model Levels
  function head
    input Real q;
    output Real h;
  algorithm
    FUNCTION_ASSERT
    h := 2 - q;
  end head;
  Real h = head(time);
  Real x(start = 0, fixed = true);
equation
  der(x) = h;
  MODEL_ASSERT
end Levels;
"#;

fn source(level: Option<&str>) -> String {
    let (function_assert, model_assert) = match level {
        Some(level) => (
            format!(
                "assert(q < 0.5, \"flow \" + String(q) + \" beyond the nominal curve\", level = {level});"
            ),
            format!("assert(x < 0.5, \"x beyond its nominal range\", level = {level});"),
        ),
        None => (String::new(), String::new()),
    };
    MODEL
        .replace("FUNCTION_ASSERT", &function_assert)
        .replace("MODEL_ASSERT", &model_assert)
}

fn simulate(level: Option<&str>) -> Result<SimResult, String> {
    let source = source(level);
    let compiled = Compiler::new()
        .model("Levels")
        .compile_str(&source, "Levels.mo")
        .unwrap_or_else(|error| panic!("Levels compiles: {error:?}"));
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .map_err(|error| format!("{error:?}"))
}

#[test]
fn violated_warning_assertions_never_abort_the_simulation() {
    let result = simulate(Some("AssertionLevel.warning")).expect("warnings never abort");
    let x = result
        .names
        .iter()
        .position(|name| name == "x")
        .expect("x is recorded");
    let final_x = result.data[x].last().copied().expect("x has samples");
    // der(x) = 2 - t, so x(1) = 1.5: both assertions are violated before t = 1.
    assert!((final_x - 1.5).abs() < 1e-6, "x(1) = {final_x}");
}

#[test]
fn each_warning_site_is_reported_once_at_its_first_violation() {
    let result = simulate(Some("AssertionLevel.warning")).expect("warnings never abort");
    let reports = result
        .diagnostics
        .iter()
        .map(|diagnostic| {
            (
                diagnostic.code,
                diagnostic.time,
                diagnostic.message.as_str(),
            )
        })
        .collect::<Vec<_>>();
    assert_eq!(reports.len(), 2, "{reports:?}");
    // x = 2t - t^2/2 reaches 0.5 at t = 2 - sqrt(3), between output points.
    let crossing = 2.0 - 3.0_f64.sqrt();
    let (code, time, message) = reports[0];
    assert_eq!(code, "WX001");
    assert_eq!(message, "x beyond its nominal range");
    assert!(
        time >= crossing && time < crossing + 0.01,
        "x reported at {time}"
    );
    let (code, time, message) = reports[1];
    assert_eq!(code, "WX001");
    assert!((time - 0.5).abs() < 1e-12, "flow reported at {time}");
    assert_eq!(message, "flow 0.5 beyond the nominal curve");
}

#[test]
fn warning_assertions_leave_the_trace_unchanged() {
    let with = simulate(Some("AssertionLevel.warning")).expect("warnings never abort");
    let without = simulate(None).expect("the model without assertions simulates");
    assert_eq!(with.names, without.names);
    assert_eq!(
        with.times, without.times,
        "warnings create no event or step"
    );
    assert_eq!(with.data, without.data);
    assert!(without.diagnostics.is_empty());
}

#[test]
fn an_explicit_error_level_fails_like_the_default() {
    let error = simulate(Some("AssertionLevel.error")).expect_err("error-level assertions abort");
    assert!(
        error.contains("x beyond its nominal range") || error.contains("flow"),
        "{error}"
    );
}

/// The FMI C component reports a violated warning site once through the
/// importer's logger at its warning status, at accepted points only, and its
/// error-level check never aborts on it.
#[test]
fn fmi_components_report_warnings_through_the_logger() {
    let source = r#"
model Packaged
  parameter Real lim = 0.5;
  Real x(start = 0, fixed = true);
equation
  der(x) = 1;
  assert(x < lim, "x " + String(x) + " beyond its nominal range", level = AssertionLevel.warning);
end Packaged;
"#;
    let compiled = Compiler::new()
        .model("Packaged")
        .compile_str(source, "Packaged.mo")
        .expect("Packaged compiles");
    let work = tempfile::tempdir().expect("packaging work directory");
    for (target, status) in [("fmi2", "fmi2Warning"), ("fmi3", "fmi3Warning")] {
        let destination = work.path().join(target);
        rumoca::compile_packaged_target(&compiled, "Packaged", target, destination.clone())
            .unwrap_or_else(|error| panic!("{target}: {error:#}"));
        let model =
            std::fs::read_to_string(destination.join("Packaged").join("sources").join("model.c"))
                .expect("the packaged component has its model source");
        assert!(
            model.contains(&format!("{status}, \"warning\"")),
            "{target} logs at the warning status"
        );
        assert!(
            model.contains("rmc_warned[0] = true"),
            "{target} reports once"
        );
        assert!(
            !model.contains("log_assertion("),
            "{target} has no aborting check for a warning site"
        );
    }
}

/// A warning-level assertion in a clocked partition is checked on that
/// clock's ticks and reported once, at the first tick that violates it.
#[test]
fn a_clocked_warning_assertion_reports_at_its_first_violating_tick() {
    let source = r#"
model ClockedWarning
  Clock c = Clock(1, 10);
  Real x(start = 0);
equation
  when c then
    x = previous(x) + 1;
    assert(x < 3, "x reached 3", level = AssertionLevel.warning);
  end when;
end ClockedWarning;
"#;
    let compiled = Compiler::new()
        .model("ClockedWarning")
        .compile_str(source, "ClockedWarning.mo")
        .unwrap_or_else(|error| panic!("ClockedWarning compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.5,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("warnings never abort: {error}"));
    let reports = result
        .diagnostics
        .iter()
        .map(|diagnostic| {
            (
                diagnostic.code,
                diagnostic.time,
                diagnostic.message.as_str(),
            )
        })
        .collect::<Vec<_>>();
    // Ticks at 0, 0.1, 0.2, ... give x = 1, 2, 3, ...; x < 3 first fails at 0.2.
    assert_eq!(reports.len(), 1, "{reports:?}");
    let (code, time, message) = reports[0];
    assert_eq!(code, "WX001");
    assert_eq!(message, "x reached 3");
    assert!((time - 0.2).abs() < 1e-9, "reported at {time}");
}
