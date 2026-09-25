//! A direct `rumoca sim` runs to the model's `experiment(StopTime)` unless
//! `--t-end` is given.

use std::process::Command;

use tempfile::tempdir;

const SOURCE: &str = "model StopTimeFixture
  Real x(start = 1, fixed = true);
equation
  der(x) = -x;
  annotation(experiment(StopTime = 2.5));
end StopTimeFixture;
";

/// The end time a direct run reports and the last time its CSV samples.
fn simulated_end(extra: &[&str]) -> (String, f64) {
    let dir = tempdir().unwrap();
    let file = dir.path().join("StopTimeFixture.mo");
    std::fs::write(&file, SOURCE).unwrap();
    let csv = dir.path().join("result.csv");
    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("sim")
        .arg(&file)
        .args(["--model", "StopTimeFixture", "--output"])
        .arg(&csv)
        .args(extra)
        .output()
        .unwrap_or_else(|err| panic!("run rumoca sim: {err}"));
    assert!(output.status.success(), "{output:?}");
    let stderr = String::from_utf8_lossy(&output.stderr);
    let Some(announced) = stderr
        .lines()
        .find(|line| line.starts_with("Simulating StopTimeFixture to t="))
    else {
        panic!("the run announces its end time: {stderr}");
    };
    let table = std::fs::read_to_string(&csv).unwrap();
    let Some(last) = table.lines().last() else {
        panic!("the CSV has samples");
    };
    let Some(time) = last.split(',').next().and_then(|cell| cell.parse().ok()) else {
        panic!("the last CSV row starts with its time: {last}");
    };
    (announced.to_string(), time)
}

#[test]
fn a_direct_run_uses_the_experiment_stop_time() {
    let (announced, time) = simulated_end(&[]);
    assert_eq!(announced, "Simulating StopTimeFixture to t=2.5...");
    assert!((time - 2.5).abs() < 1e-12, "last sample at {time}");
}

#[test]
fn an_explicit_end_time_overrides_the_experiment() {
    let (announced, time) = simulated_end(&["--t-end", "1"]);
    assert_eq!(announced, "Simulating StopTimeFixture to t=1...");
    assert!((time - 1.0).abs() < 1e-12, "last sample at {time}");
}
