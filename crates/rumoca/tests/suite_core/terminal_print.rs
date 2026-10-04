//! MLS 3.7 §12.9 terminal prints in `when` statements.
//!
//! `Modelica.Utilities.Streams.print` is an impure external function whose
//! foreign body, `ModelicaInternal_print`, writes a line to the terminal when
//! its file name is empty. A model algorithm calls it in a `when` statement
//! (`Modelica.Utilities.Examples.ReadRealMatrixFromFile` prints at
//! `initial()`); each activation reports the rendered message once as
//! SPEC_0008 `WX002` and leaves the trace unchanged. A print to a file and a
//! print outside a `when` statement are refused.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

const STREAMS: &str = r#"
package Streams
  impure function print
    input String string = "";
    input String fileName = "";
  external "C" ModelicaInternal_print(string, fileName);
  end print;
end Streams;
"#;

fn compile(model: &str) -> Result<rumoca::CompilationResult, String> {
    let source = format!("{STREAMS}\n{model}");
    Compiler::new()
        .model("Messages")
        .compile_str(&source, "Messages.mo")
        .map_err(|error| format!("{error:?}"))
}

fn simulate(model: &str) -> SimResult {
    let compiled = compile(model).expect("the model compiles");
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.125),
            ..SimOptions::default()
        },
    )
    .expect("the model simulates")
}

#[test]
fn each_activation_of_a_terminal_print_reports_its_message() {
    let result = simulate(
        r#"
model Messages
  parameter Integer k = 2;
  Real x(start = 1, fixed = true);
equation
  der(x) = -x;
algorithm
  when initial() then
    Streams.print("start k = " + String(k));
  end when;
  when sample(0.5, 0.5) then
    Streams.print("tick");
  end when;
end Messages;
"#,
    );
    let messages = result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "WX002")
        .map(|diagnostic| (diagnostic.time, diagnostic.message.as_str()))
        .collect::<Vec<_>>();
    assert_eq!(
        messages,
        vec![(0.0, "start k = 2"), (0.5, "tick"), (1.0, "tick")],
        "one report per activation, in time order"
    );
    let x = result
        .names
        .iter()
        .position(|name| name == "x")
        .expect("x is recorded");
    let final_x = result.data[x].last().copied().expect("x has samples");
    assert!(
        (final_x - (-1.0_f64).exp()).abs() < 1e-5,
        "prints leave the trace unchanged: x(1) = {final_x}"
    );
}

#[test]
fn a_print_to_a_file_is_refused() {
    let error = compile(
        r#"
model Messages
  Real x(start = 1, fixed = true);
equation
  der(x) = -x;
algorithm
  when initial() then
    Streams.print("start", "log.txt");
  end when;
end Messages;
"#,
    )
    .expect_err("a file write has no owner");
    assert!(
        error.contains("must retain at least one output"),
        "refused as a result-less call: {error}"
    );
}

#[test]
fn a_print_outside_a_when_statement_is_refused() {
    let error = compile(
        r#"
model Messages
  Real x(start = 1, fixed = true);
  Real y;
equation
  der(x) = -x;
algorithm
  y := x;
  Streams.print("every step");
end Messages;
"#,
    )
    .expect_err("MLS 3.7 §12.3 confines impure calls");
    assert!(
        error.contains("IllegalImpureCallContext") || error.contains("impure"),
        "refused as an impure call context: {error}"
    );
}

fn reports(model: &str) -> Vec<(f64, String)> {
    simulate(model)
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "WX002")
        .map(|diagnostic| (diagnostic.time, diagnostic.message.clone()))
        .collect()
}

fn activation_reports(body: &str) -> Vec<(f64, String)> {
    reports(&format!(
        "model Messages\n  Real x(start = 1, fixed = true);\nequation\n  der(x) = -x;\nalgorithm\n{body}\nend Messages;\n"
    ))
}

fn at(entries: &[(f64, &str)]) -> Vec<(f64, String)> {
    entries
        .iter()
        .map(|(time, message)| (*time, (*message).to_string()))
        .collect()
}

/// MLS 3.7 §8.6: `initial()` holds for the initialization instant only, and
/// the event iteration after it acts on the edges since that instant. A
/// phase-zero tick acts once, after initialization, and a vector activation
/// with `initial()` acts at both (as OpenModelica reports these models).
#[test]
fn initialization_event_actions_act_once_per_instant() {
    let tick = "  when sample(0, 0.5) then\n    Streams.print(\"tick\");\n  end when;";
    assert_eq!(
        activation_reports(tick),
        at(&[(0.0, "tick"), (0.5, "tick"), (1.0, "tick")]),
        "a phase-zero tick prints once at the start"
    );
    let initial_only = "  when sample(0, 0.5) then\n    if initial() then\n      Streams.print(\"init\");\n    end if;\n  end when;";
    assert_eq!(
        activation_reports(initial_only),
        at(&[]),
        "the phase-zero tick acts after initialization"
    );
    let both = "  when {initial(), sample(0, 0.5)} then\n    Streams.print(\"both\");\n  end when;";
    assert_eq!(
        activation_reports(both),
        at(&[(0.0, "both"), (0.0, "both"), (0.5, "both"), (1.0, "both")]),
        "initialization and the phase-zero tick are two activations"
    );
}

/// A condition `not initial()` makes false at the initialization instant and
/// true right after it rises in the event iteration at the start.
#[test]
fn a_not_initial_guard_acts_after_initialization() {
    let post = "  when time >= 0 and not initial() then\n    Streams.print(\"post\");\n  end when;\n  when initial() then\n    Streams.print(\"init\");\n  end when;";
    assert_eq!(
        activation_reports(post),
        at(&[(0.0, "init"), (0.0, "post")]),
        "one report at initialization, one after it"
    );
}
