//! `rumoca sim --inspect trajectory-sensitivity|objective-gradient|linearize`
//! drive the trajectory sensitivities from the command line.

use std::path::Path;
use std::process::{Command, Output};

use tempfile::tempdir;

const FIRST_ORDER: &str = "model FirstOrder
  parameter Real a = 2;
  parameter Real b = 1;
  Real x(start = 1, fixed = true);
equation
  der(x) = -a * x + b;
  annotation(experiment(StartTime = 0, StopTime = 1));
end FirstOrder;
";

const STATE_SPACE: &str = "model StateSpace
  parameter Real a = 2;
  parameter Real b = 3;
  parameter Real c = 5;
  parameter Real d = 7;
  input Real u = 0.5;
  output Real y;
  Real x(start = 1, fixed = true);
equation
  der(x) = -a * x + b * u;
  y = c * x + d * u;
end StateSpace;
";

fn rumoca(dir: &Path, model: &str, source: &str, args: &[&str]) -> Output {
    let file = dir.join(format!("{model}.mo"));
    std::fs::write(&file, source).unwrap();
    Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("sim")
        .arg(&file)
        .args(["--model", model])
        .args(args)
        .output()
        .unwrap_or_else(|error| panic!("run rumoca sim: {error}"))
}

fn stdout_json(output: &Output) -> serde_json::Value {
    assert!(output.status.success(), "{output:?}");
    serde_json::from_slice(&output.stdout).expect("JSON on stdout")
}

fn close(got: f64, want: f64, tolerance: f64, what: &str) {
    assert!(
        (got - want).abs() <= tolerance * (1.0 + want.abs()),
        "{what}: got {got}, want {want}"
    );
}

/// `x = 1/2 + e^{-2t}/2` for `a = 2`, `b = 1`.
#[test]
fn trajectory_sensitivity_writes_sensitivity_columns_beside_the_trace() {
    let dir = tempdir().unwrap();
    let csv = dir.path().join("sens.csv");
    let output = rumoca(
        dir.path(),
        "FirstOrder",
        FIRST_ORDER,
        &[
            "--inspect",
            "trajectory-sensitivity",
            "--wrt",
            "a,b",
            "--dt",
            "0.5",
            "--rtol",
            "1e-9",
            "--atol",
            "1e-11",
            "-o",
            csv.to_str().unwrap(),
        ],
    );
    assert!(output.status.success(), "{output:?}");
    let table = std::fs::read_to_string(&csv).unwrap();
    let mut lines = table.lines();
    assert_eq!(lines.next(), Some("time,x,d(x)/d(a),d(x)/d(b)"));
    let last: Vec<f64> = lines
        .last()
        .expect("rows")
        .split(',')
        .map(|cell| cell.parse().expect("number"))
        .collect();
    let decay = (-2.0_f64).exp();
    close(last[0], 1.0, 1.0e-12, "time");
    close(last[1], 0.5 + 0.5 * decay, 1.0e-7, "x");
    close(
        last[2],
        -0.5 * decay - 0.25 * (1.0 - decay),
        1.0e-7,
        "dx/da",
    );
    close(last[3], 0.5 * (1.0 - decay), 1.0e-7, "dx/db");
}

#[test]
fn trajectory_sensitivity_summarizes_the_end_of_the_run_without_an_output_file() {
    let dir = tempdir().unwrap();
    let output = rumoca(
        dir.path(),
        "FirstOrder",
        FIRST_ORDER,
        &["--inspect", "trajectory-sensitivity", "--dt", "0.5"],
    );
    assert!(output.status.success(), "{output:?}");
    let text = String::from_utf8_lossy(&output.stdout);
    assert!(
        text.contains("trajectory sensitivity: model `FirstOrder` at t=1"),
        "{text}"
    );
    assert!(text.contains("d(x)/d(a)"), "{text}");

    let json = rumoca(
        dir.path(),
        "FirstOrder",
        FIRST_ORDER,
        &["--inspect", "trajectory-sensitivity", "--format", "json"],
    );
    let value = stdout_json(&json);
    assert_eq!(value["model"], "FirstOrder");
    assert!(
        value["columns"]
            .as_array()
            .is_some_and(|columns| columns.len() == 3)
    );
}

#[test]
fn trajectory_sensitivity_writes_the_html_report_and_refuses_other_formats() {
    let dir = tempdir().unwrap();
    let html = dir.path().join("sens.html");
    let output = rumoca(
        dir.path(),
        "FirstOrder",
        FIRST_ORDER,
        &[
            "--inspect",
            "trajectory-sensitivity",
            "-o",
            html.to_str().unwrap(),
        ],
    );
    assert!(output.status.success(), "{output:?}");
    let report = std::fs::read_to_string(&html).unwrap();
    assert!(
        report.contains("d(x)/d(a)"),
        "the report carries the sensitivity columns"
    );

    let output = rumoca(
        dir.path(),
        "FirstOrder",
        FIRST_ORDER,
        &[
            "--inspect",
            "trajectory-sensitivity",
            "-o",
            dir.path().join("sens.txt").to_str().unwrap(),
        ],
    );
    assert!(!output.status.success());
    assert!(
        String::from_utf8_lossy(&output.stderr).contains(".csv"),
        "{output:?}"
    );
}

/// `J = integral x^2 dt` fitted against zero data; the closed form is the
/// one pinned in `trajectory_sensitivity_test`.
#[test]
fn objective_gradient_over_a_trajectory_agrees_between_forward_and_adjoint() {
    let dir = tempdir().unwrap();
    let data = dir.path().join("zero.csv");
    std::fs::write(&data, "time,x\n0,0\n1,0\n").unwrap();
    let mut gradients = Vec::new();
    for mode in ["forward", "adjoint"] {
        let output = rumoca(
            dir.path(),
            "FirstOrder",
            FIRST_ORDER,
            &[
                "--inspect",
                "objective-gradient",
                "--fit-data",
                data.to_str().unwrap(),
                "--grad-mode",
                mode,
                "--rtol",
                "1e-9",
                "--atol",
                "1e-11",
                "--format",
                "json",
            ],
        );
        let value = stdout_json(&output);
        assert_eq!(value["mode"], mode);
        close(
            value["objective"].as_f64().unwrap(),
            0.527_521_4,
            1.0e-6,
            "J",
        );
        let parameters = value["parameters"].as_array().unwrap();
        assert_eq!(parameters.len(), 2);
        gradients.push(
            parameters
                .iter()
                .map(|entry| entry["gradient"].as_f64().unwrap())
                .collect::<Vec<_>>(),
        );
    }
    for (forward, adjoint) in gradients[0].iter().zip(&gradients[1]) {
        close(*forward, *adjoint, 1.0e-5, "forward vs adjoint");
    }
}

#[test]
fn running_and_terminal_terms_select_the_trajectory_mode() {
    let dir = tempdir().unwrap();
    let output = rumoca(
        dir.path(),
        "FirstOrder",
        FIRST_ORDER,
        &[
            "--inspect",
            "objective-gradient",
            "--integral",
            "x",
            "--terminal",
            "x",
            "--grad-mode",
            "adjoint",
            "--rtol",
            "1e-9",
            "--atol",
            "1e-11",
        ],
    );
    assert!(output.status.success(), "{output:?}");
    let text = String::from_utf8_lossy(&output.stdout);
    assert!(text.contains("trajectory objective gradient"), "{text}");
    assert!(
        text.contains("dJ/d(a)") && text.contains("dJ/d(b)"),
        "{text}"
    );
}

#[test]
fn a_steady_objective_cannot_be_combined_with_a_trajectory_term() {
    let dir = tempdir().unwrap();
    let output = rumoca(
        dir.path(),
        "FirstOrder",
        FIRST_ORDER,
        &[
            "--inspect",
            "objective-gradient",
            "--objective",
            "x",
            "--integral",
            "x",
        ],
    );
    assert!(!output.status.success());
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("cannot be used"),
        "{output:?}"
    );
}

#[test]
fn a_fit_data_file_that_does_not_cover_the_run_is_refused() {
    let dir = tempdir().unwrap();
    let data = dir.path().join("short.csv");
    std::fs::write(&data, "time,x\n0,0\n0.5,0\n").unwrap();
    let output = rumoca(
        dir.path(),
        "FirstOrder",
        FIRST_ORDER,
        &[
            "--inspect",
            "objective-gradient",
            "--fit-data",
            data.to_str().unwrap(),
        ],
    );
    assert!(!output.status.success());
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("span"),
        "{output:?}"
    );
}

#[test]
fn linearize_reports_the_state_space_matrices() {
    let dir = tempdir().unwrap();
    let output = rumoca(
        dir.path(),
        "StateSpace",
        STATE_SPACE,
        &["--inspect", "linearize", "--format", "json"],
    );
    let value = stdout_json(&output);
    assert_eq!(value["states"], serde_json::json!(["x"]));
    assert_eq!(value["inputs"], serde_json::json!(["u"]));
    assert_eq!(value["outputs"], serde_json::json!(["y"]));
    assert_eq!(value["A"], serde_json::json!([[-2.0]]));
    assert_eq!(value["B"], serde_json::json!([[3.0]]));
    assert_eq!(value["C"], serde_json::json!([[5.0]]));
    assert_eq!(value["D"], serde_json::json!([[7.0]]));

    // The operating point names an input and a state alike.
    let at = rumoca(
        dir.path(),
        "StateSpace",
        STATE_SPACE,
        &[
            "--inspect",
            "linearize",
            "--at",
            "u=2,x=3@0",
            "--format",
            "json",
        ],
    );
    let value = stdout_json(&at);
    assert_eq!(value["state_values"], serde_json::json!([3.0]));
    assert_eq!(value["input_values"], serde_json::json!([2.0]));

    let human = rumoca(
        dir.path(),
        "StateSpace",
        STATE_SPACE,
        &["--inspect", "linearize"],
    );
    assert!(human.status.success(), "{human:?}");
    let text = String::from_utf8_lossy(&human.stdout);
    assert!(text.contains("B[x][u] = 3"), "{text}");
    assert!(text.contains("D[y][u] = 7"), "{text}");
}

#[test]
fn compile_refuses_a_trajectory_inspection_that_needs_a_window() {
    let dir = tempdir().unwrap();
    let file = dir.path().join("FirstOrder.mo");
    std::fs::write(&file, FIRST_ORDER).unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("compile")
        .arg(&file)
        .args([
            "--model",
            "FirstOrder",
            "--inspect",
            "trajectory-sensitivity",
        ])
        .output()
        .unwrap();
    assert!(!output.status.success());
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("integrates a run"),
        "{output:?}"
    );

    let linearize = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("compile")
        .arg(&file)
        .args(["--model", "FirstOrder", "--inspect", "linearize"])
        .output()
        .unwrap();
    assert!(linearize.status.success(), "{linearize:?}");
}

#[test]
fn an_implicit_solver_is_refused_for_the_sensitivity_systems_not_ignored() {
    let dir = tempdir().unwrap();
    let output = rumoca(
        dir.path(),
        "FirstOrder",
        FIRST_ORDER,
        &["--inspect", "trajectory-sensitivity", "--solver", "bdf"],
    );
    assert!(!output.status.success());
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("rk-like"),
        "{output:?}"
    );
    let explicit = rumoca(
        dir.path(),
        "FirstOrder",
        FIRST_ORDER,
        &[
            "--inspect",
            "trajectory-sensitivity",
            "--solver",
            "rk-like",
            "--dt",
            "0.5",
        ],
    );
    assert!(explicit.status.success(), "{explicit:?}");
}

#[test]
fn a_default_request_reports_what_it_differentiates_and_prints_zero_not_negative_zero() {
    let dir = tempdir().unwrap();
    let csv = dir.path().join("sens.csv");
    let output = rumoca(
        dir.path(),
        "FirstOrder",
        FIRST_ORDER,
        &[
            "--inspect",
            "trajectory-sensitivity",
            "--dt",
            "0.5",
            "-o",
            csv.to_str().unwrap(),
        ],
    );
    assert!(output.status.success(), "{output:?}");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("differentiating 2 parameter(s): a, b"),
        "{stderr}"
    );
    let table = std::fs::read_to_string(&csv).unwrap();
    assert!(!table.contains("-0,") && !table.contains("-0\n"), "{table}");
}

#[test]
fn a_linearization_names_the_input_that_needs_an_operating_value() {
    let dir = tempdir().unwrap();
    let source = "model NeedsInput
  parameter Real a = 1.5;
  input Real u(start = 0.5);
  output Real y;
  Real x(start = 1, fixed = true);
equation
  der(x) = -a * x + u;
  y = x;
end NeedsInput;
";
    let missing = rumoca(
        dir.path(),
        "NeedsInput",
        source,
        &["--inspect", "linearize"],
    );
    assert!(!missing.status.success());
    assert!(
        String::from_utf8_lossy(&missing.stderr).contains("u=<value>"),
        "{missing:?}"
    );
    let given = rumoca(
        dir.path(),
        "NeedsInput",
        source,
        &[
            "--inspect",
            "linearize",
            "--at",
            "u=0.5",
            "--format",
            "json",
        ],
    );
    let value = stdout_json(&given);
    assert_eq!(value["B"], serde_json::json!([[1.0]]));
}
