//! `rumoca sim --serve-stdio` drives one session from JSON-lines commands.

use std::io::{self, BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, ChildStdout, Command, ExitStatus, Stdio};
use std::sync::mpsc::{self, Receiver, RecvTimeoutError};
use std::thread;
use std::time::{Duration, Instant};

use rumoca_sim::{SessionCommand, SessionEvent};
use rumoca_solver::SimExecutionEngine;
use serde_json::{Value, json};
use tempfile::{TempDir, tempdir};

const PLANT: &str = "model Plant
  input Real u;
  Real y(start = 0, fixed = true);
equation
  der(y) = -2 * y + 4 * u;
end Plant;
";

struct Served {
    _dir: TempDir,
    child: Child,
    stdin: Option<ChildStdin>,
    stdout: BufReader<ChildStdout>,
}

impl Served {
    fn spawn() -> Self {
        Self::spawn_source(
            PLANT,
            "Plant",
            &[
                "--solver", "bdf", "--atol", "1e-10", "--rtol", "1e-10", "--input", "u=0",
            ],
        )
    }

    fn spawn_source(source: &str, model: &str, options: &[&str]) -> Self {
        let dir = tempdir().unwrap();
        let file = dir.path().join(format!("{model}.mo"));
        std::fs::write(&file, source).unwrap();
        let mut child = Command::new(env!("CARGO_BIN_EXE_rumoca"))
            .arg("sim")
            .arg("--serve-stdio")
            .arg(&file)
            .args(["-m", model])
            .args(options)
            .arg("--cache-dir")
            .arg(dir.path().join("cache"))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .unwrap_or_else(|err| panic!("spawn rumoca sim --serve-stdio: {err}"));
        let stdin = child.stdin.take();
        let stdout = BufReader::new(child.stdout.take().unwrap());
        let mut served = Self {
            _dir: dir,
            child,
            stdin,
            stdout,
        };
        let hello = served.read();
        assert_eq!(hello["event"], "hello");
        assert_eq!(hello["protocol_version"], 1);
        assert!(hello["engine"]["engine"].is_string(), "{hello}");
        assert!(hello["engine"].get("refusal").is_some(), "{hello}");
        served
    }

    fn read(&mut self) -> Value {
        let mut line = String::new();
        self.stdout.read_line(&mut line).unwrap();
        serde_json::from_str(&line).unwrap_or_else(|err| panic!("event line {line:?}: {err}"))
    }

    fn send(&mut self, command: &Value) {
        let stdin = self.stdin.as_mut().expect("stdin is open");
        writeln!(stdin, "{command}").unwrap();
        stdin.flush().unwrap();
    }

    fn call(&mut self, command: Value) -> Value {
        self.send(&command);
        self.read()
    }

    fn command(&mut self, command: SessionCommand) -> SessionEvent {
        serde_json::from_value(self.call(serde_json::to_value(command).unwrap())).unwrap()
    }

    fn y(&mut self) -> f64 {
        self.call(json!({"command": "get", "name": "y"}))["value"]
            .as_f64()
            .unwrap()
    }
}

fn wait_for_exit(child: &mut Child) -> io::Result<ExitStatus> {
    let deadline = Instant::now() + Duration::from_secs(10);
    let failure = loop {
        match child.try_wait() {
            Ok(Some(status)) => return Ok(status),
            Err(error) => break error,
            Ok(None) => {}
        }
        if Instant::now() >= deadline {
            break io::Error::new(
                io::ErrorKind::TimedOut,
                "stdio child did not exit within 10 seconds",
            );
        }
        thread::sleep(Duration::from_millis(10));
    };
    let _ = child.kill();
    child.wait()?;
    Err(failure)
}

fn keep_writing_state(
    mut stdin: ChildStdin,
    stop: Receiver<()>,
) -> (ChildStdin, Option<io::Error>) {
    let command = json!({"command": "state"});
    loop {
        if let Err(error) = writeln!(stdin, "{command}").and_then(|()| stdin.flush()) {
            // Keep stdin alive until the parent has observed the child's exit.
            return (stdin, Some(error));
        }
        match stop.recv_timeout(Duration::from_millis(10)) {
            Ok(()) | Err(RecvTimeoutError::Disconnected) => return (stdin, None),
            Err(RecvTimeoutError::Timeout) => {}
        }
    }
}

#[test]
fn issue_422_release_cli_runs_original_u4_without_native_fallback() {
    let source = include_str!(
        "../../../rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_422.mo"
    );
    let mut served = Served::spawn_source(
        source,
        "U4",
        &[
            "--solver", "rk-like", "--t-end", "1", "--dt", "0.1", "--atol", "1e-9", "--rtol",
            "1e-9",
        ],
    );
    for sample in 0..=10 {
        let time = f64::from(sample) / 10.0;
        if sample != 0 {
            assert_eq!(
                served.command(SessionCommand::AdvanceTo { time }),
                SessionEvent::Ok { time }
            );
        }
        let SessionEvent::State {
            time: actual_time,
            values,
        } = served.command(SessionCommand::State)
        else {
            panic!("original U4 must return its state");
        };
        assert_eq!(actual_time, time);
        let actual = values["y"];
        let expected = (-time).exp();
        let bound = 20.0 * (1e-9 + 1e-9 * expected.abs());
        assert!(
            actual.is_finite() && (actual - expected).abs() <= bound,
            "original U4/{sample}: y={actual}, expected {expected}, bound {bound}"
        );
        let SessionEvent::Hello { engine, .. } = served.command(SessionCommand::Hello {
            protocol_version: 1,
        }) else {
            panic!("original U4 must return its execution receipt");
        };
        assert_eq!(engine.engine, SimExecutionEngine::Cranelift, "{engine:?}");
        assert_eq!(engine.refusal, None, "{engine:?}");
        assert!(engine.declined.is_empty(), "{engine:?}");
        eprintln!("original U4 release CLI/{sample} time={time} y={actual}: {engine:?}");
    }
    assert_eq!(
        served.command(SessionCommand::Close),
        SessionEvent::Closed { time: 1.0 }
    );
    drop(served.stdin.take());
    assert!(wait_for_exit(&mut served.child).unwrap().success());
}

/// Exact sampled-data solution of `y' = -2y + 4u` under `u = 0.8 (1 - y)` held
/// for `dt` at each step: `y_{k+1} = rho y_k + 1.6 (1 - exp(-2 dt))` with
/// `rho = 2.6 exp(-2 dt) - 1.6`, so `y_n = 8/13 (1 - rho^n)`. At `dt = 0.1`,
/// `n = 10` this is 0.614334 (the steady state is 8/13 = 0.615385).
fn closed_loop_analytic(dt: f64, steps: i32) -> f64 {
    let rho = 2.6 * (-2.0 * dt).exp() - 1.6;
    8.0 / 13.0 * (1.0 - rho.powi(steps))
}

#[test]
fn an_external_proportional_controller_closes_the_loop() {
    let mut served = Served::spawn();
    let (setpoint, dt) = (1.0, 0.1);
    for _ in 0..10 {
        let y = served.y();
        let reply = served
            .call(json!({"command": "set_input", "name": "u", "value": 0.8 * (setpoint - y)}));
        assert_eq!(reply["event"], "ok");
        assert_eq!(
            served.call(json!({"command": "step", "dt": dt}))["event"],
            "ok"
        );
    }
    let state = served.call(json!({"command": "state"}));
    assert_eq!(state["event"], "state");
    assert!((state["time"].as_f64().unwrap() - 1.0).abs() < 1e-12);
    let y = state["values"]["y"].as_f64().unwrap();
    let expected = closed_loop_analytic(dt, 10);
    assert!((expected - 0.614334).abs() < 1e-6, "{expected}");
    assert!(
        (y - expected).abs() < 1e-7,
        "y(1.0) = {y}, expected {expected}"
    );
    assert_eq!(served.call(json!({"command": "close"}))["event"], "closed");
    drop(served.stdin.take());
    assert!(wait_for_exit(&mut served.child).unwrap().success());
}

#[test]
fn a_bad_line_is_a_typed_error_and_the_session_continues() {
    let mut served = Served::spawn();
    let stdin = served.stdin.as_mut().unwrap();
    writeln!(stdin, "{{\"command\":\"warp\"}}").unwrap();
    let error = served.read();
    assert_eq!(error["event"], "error");
    assert_eq!(error["code"], "EX011");
    let names = served.call(json!({"command": "input_names"}));
    assert_eq!(names, json!({"event": "input_names", "names": ["u"]}));
    let batch = served.call(json!({"command": "set_inputs", "inputs": [["u", 1.0]]}));
    assert_eq!(batch["event"], "ok");
    let advanced = served.call(json!({"command": "advance_to", "time": 0.5}));
    assert!((advanced["time"].as_f64().unwrap() - 0.5).abs() < 1e-12);
}

#[test]
fn an_unsupported_protocol_version_ends_the_session_with_status_72() {
    let mut served = Served::spawn();
    let reply = served.call(json!({"command": "hello", "protocol_version": 99}));
    assert_eq!(reply["event"], "error");
    assert_eq!(reply["code"], "EX010");
    assert_eq!(wait_for_exit(&mut served.child).unwrap().code(), Some(72));
}

#[test]
fn closing_stdin_without_close_exits_with_the_disconnect_status() {
    let mut served = Served::spawn();
    assert_eq!(
        served.call(json!({"command": "step", "dt": 0.1}))["event"],
        "ok"
    );
    drop(served.stdin.take());
    assert_eq!(wait_for_exit(&mut served.child).unwrap().code(), Some(71));
}

#[test]
fn serve_stdio_excludes_batch_outputs() {
    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .args(["sim", "--serve-stdio", "Plant.mo", "--inspect", "structure"])
        .output()
        .unwrap();
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("cannot be used with"));
}

#[test]
fn initial_inputs_are_required_and_checked() {
    let dir = tempdir().unwrap();
    let file = dir.path().join("Plant.mo");
    std::fs::write(&file, PLANT).unwrap();
    let serve = |extra: &[&str]| {
        Command::new(env!("CARGO_BIN_EXE_rumoca"))
            .args(["sim", "--serve-stdio"])
            .arg(&file)
            .args(["-m", "Plant", "--cache-dir"])
            .arg(dir.path().join("cache"))
            .args(extra)
            .stdin(Stdio::null())
            .output()
            .unwrap()
    };
    let missing = serve(&[]);
    assert!(!missing.status.success());
    assert!(
        missing.stdout.is_empty(),
        "stdout carries only protocol events"
    );
    let stderr = String::from_utf8_lossy(&missing.stderr);
    assert!(
        stderr.contains("[EX002]") && stderr.contains("input `u`"),
        "{stderr}"
    );
    let malformed = serve(&["--input", "u"]);
    assert!(!malformed.status.success());
    assert!(String::from_utf8_lossy(&malformed.stderr).contains("expected NAME=VALUE"));
    let not_a_number = serve(&["--input", "u=fast"]);
    assert!(!not_a_number.status.success());
    assert!(String::from_utf8_lossy(&not_a_number.stderr).contains("is not a number"));
    // An exhausted stdin is a parent disconnect after the hello.
    let ready = serve(&["--input", "u=0"]);
    assert_eq!(ready.status.code(), Some(71));
    let stdout = String::from_utf8_lossy(&ready.stdout);
    let hello: Value = serde_json::from_str(stdout.trim()).unwrap();
    assert_eq!(hello["event"], "hello");
    assert_eq!(hello["protocol_version"], 1);
    assert!(hello["engine"]["engine"].is_string(), "{hello}");
}

#[test]
fn a_closed_stdout_exits_with_the_disconnect_status() {
    let Served {
        _dir,
        mut child,
        stdin,
        stdout,
    } = Served::spawn();
    drop(stdout);
    let (stop, requests) = mpsc::channel();
    // Concurrent child launches can briefly inherit the dropped reader. Keep
    // writing in this one session until the closed pipe is actually observed.
    let writer = thread::spawn(move || keep_writing_state(stdin.unwrap(), requests));
    let status = wait_for_exit(&mut child);
    let _ = stop.send(());
    let (_stdin, write_error) = writer.join().unwrap();
    assert_eq!(status.unwrap().code(), Some(71));
    if let Some(error) = write_error {
        assert_eq!(error.kind(), io::ErrorKind::BrokenPipe);
    }
}

#[test]
fn undecodable_input_is_a_typed_error_and_the_session_continues() {
    let mut served = Served::spawn();
    let stdin = served.stdin.as_mut().unwrap();
    stdin.write_all(b"\xff\xfe\n").unwrap();
    let error = served.read();
    assert_eq!(error["event"], "error");
    assert_eq!(error["code"], "EX011");
    assert_eq!(served.call(json!({"command": "state"}))["event"], "state");
}
