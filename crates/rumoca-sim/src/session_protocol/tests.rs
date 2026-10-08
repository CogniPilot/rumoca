use std::io::{self, Cursor, Write};

use rumoca_compile::compile::{Session, SessionConfig};

use super::*;
use crate::{SimOptions, SimSolverMode};

const PLANT: &str = r#"
model Plant
  input Real u;
  input Real v;
  Real y(start = 0, fixed = true);
equation
  der(y) = -2 * y + 4 * u + v;
end Plant;
"#;

fn session(solver_mode: SimSolverMode) -> SimulationSession {
    let mut compiler = Session::new(SessionConfig::default());
    compiler.add_document("plant.mo", PLANT).unwrap();
    let dae = compiler.compile_model("Plant").unwrap().dae;
    SimulationSession::new(
        &dae,
        SimOptions {
            solver_mode,
            atol: 1e-10,
            rtol: 1e-10,
            initial_inputs: vec![("u".into(), 0.0), ("v".into(), 0.0)],
            ..SimOptions::default()
        },
    )
    .unwrap()
}

fn error_code(event: SessionEvent) -> String {
    match event {
        SessionEvent::Error { code, .. } => code,
        other => panic!("expected an error event, got {other:?}"),
    }
}

fn time_of(event: SessionEvent) -> f64 {
    match event {
        SessionEvent::Ok { time } => time,
        other => panic!("expected ok, got {other:?}"),
    }
}

/// Exact sampled-data solution of `y' = -2y + 4u` under `u = 0.8 (1 - y)` held
/// over each step: `y_{k+1} = rho y_k + 1.6 (1 - alpha)` with
/// `alpha = exp(-2 dt)` and `rho = 2.6 alpha - 1.6`, so
/// `y_n = 8/13 (1 - rho^n)`.
fn closed_loop_analytic(dt: f64, steps: i32) -> f64 {
    let rho = 2.6 * (-2.0 * dt).exp() - 1.6;
    8.0 / 13.0 * (1.0 - rho.powi(steps))
}

#[test]
fn every_command_has_one_outcome() {
    for mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let mut s = session(mode);
        assert_eq!(
            s.apply(SessionCommand::Hello {
                protocol_version: SESSION_PROTOCOL_VERSION
            }),
            SessionEvent::Hello {
                protocol_version: SESSION_PROTOCOL_VERSION
            }
        );
        assert_eq!(
            s.apply(SessionCommand::InputNames),
            SessionEvent::InputNames {
                names: vec!["u".into(), "v".into()]
            }
        );
        let SessionEvent::VariableNames { names } = s.apply(SessionCommand::VariableNames) else {
            panic!("variable names");
        };
        assert!(names.iter().any(|name| name == "y"));
        assert_eq!(
            time_of(s.apply(SessionCommand::SetInput {
                name: "u".into(),
                value: 1.0
            })),
            0.0
        );
        assert!((time_of(s.apply(SessionCommand::Step { dt: 0.5 })) - 0.5).abs() < 1e-12);
        assert!((time_of(s.apply(SessionCommand::AdvanceTo { time: 1.0 })) - 1.0).abs() < 1e-12);
        let SessionEvent::Value { name, time, value } =
            s.apply(SessionCommand::Get { name: "y".into() })
        else {
            panic!("value");
        };
        let expected = 2.0 * (1.0 - (-2.0_f64).exp());
        assert_eq!((name.as_str(), time), ("y", 1.0));
        assert!(
            (value.unwrap() - expected).abs() < 1e-6,
            "{mode:?} {value:?}"
        );
        let SessionEvent::State { time, values } = s.apply(SessionCommand::State) else {
            panic!("state");
        };
        assert_eq!(time, 1.0);
        assert_eq!(values["u"], 1.0);
        assert!((values["y"] - expected).abs() < 1e-6);
        assert_eq!(
            s.apply(SessionCommand::Get {
                name: "missing".into()
            }),
            SessionEvent::Value {
                name: "missing".into(),
                time: 1.0,
                value: None
            }
        );
        assert_eq!(time_of(s.apply(SessionCommand::Reset { time: None })), 0.0);
        assert_eq!(
            time_of(s.apply(SessionCommand::Reset { time: Some(0.25) })),
            0.25
        );
        assert_eq!(
            s.apply(SessionCommand::Close),
            SessionEvent::Closed { time: 0.25 }
        );
    }
}

#[test]
fn set_inputs_applies_the_whole_frame() {
    let mut s = session(SimSolverMode::Bdf);
    let event = s.apply(SessionCommand::SetInputs {
        inputs: vec![("u".into(), 1.0), ("v".into(), -4.0)],
    });
    assert_eq!(time_of(event), 0.0);
    s.apply(SessionCommand::AdvanceTo { time: 2.0 });
    // der(y) = -2y + 4 - 4 = -2y with y(0) = 0.
    assert!(s.get("y").unwrap().unwrap().abs() < 1e-9);
}

#[test]
fn closed_loop_matches_the_sampled_data_solution() {
    let mut s = session(SimSolverMode::Bdf);
    for _ in 0..10 {
        let SessionEvent::Value { value, .. } = s.apply(SessionCommand::Get { name: "y".into() })
        else {
            panic!("value");
        };
        let u = 0.8 * (1.0 - value.unwrap());
        s.apply(SessionCommand::SetInput {
            name: "u".into(),
            value: u,
        });
        s.apply(SessionCommand::Step { dt: 0.1 });
    }
    let y = s.get("y").unwrap().unwrap();
    assert!((y - closed_loop_analytic(0.1, 10)).abs() < 1e-7, "{y}");
}

#[test]
fn rejected_commands_report_typed_codes_and_change_nothing() {
    let mut s = session(SimSolverMode::Bdf);
    assert_eq!(
        error_code(s.apply(SessionCommand::Hello {
            protocol_version: SESSION_PROTOCOL_VERSION + 1
        })),
        EX010_SESSION_PROTOCOL_VERSION
    );
    for command in [
        SessionCommand::Step { dt: f64::NAN },
        SessionCommand::Step { dt: -0.1 },
        SessionCommand::AdvanceTo {
            time: f64::INFINITY,
        },
        SessionCommand::Reset { time: Some(-1.0) },
        SessionCommand::Reset {
            time: Some(f64::NAN),
        },
    ] {
        assert_eq!(error_code(s.apply(command)), EX012_SESSION_INVALID_ARGUMENT);
    }
    assert_eq!(
        error_code(s.apply(SessionCommand::SetInput {
            name: "nope".into(),
            value: 1.0
        })),
        "EX001"
    );
    assert_eq!(
        error_code(s.apply(SessionCommand::SetInputs {
            inputs: vec![("u".into(), 3.0), ("nope".into(), 1.0)]
        })),
        "EX001"
    );
    assert_eq!(s.time(), 0.0);
    assert_eq!(
        s.get("u").unwrap(),
        Some(0.0),
        "a rejected batch changes nothing"
    );
}

fn run(commands: &str) -> (SessionServeExit, Vec<SessionEvent>) {
    let mut s = session(SimSolverMode::Bdf);
    let mut out = Vec::new();
    let exit = serve_session(&mut s, Cursor::new(commands.to_owned()), &mut out).unwrap();
    let events = String::from_utf8(out)
        .unwrap()
        .lines()
        .map(|line| serde_json::from_str(line).unwrap())
        .collect();
    (exit, events)
}

#[test]
fn serve_answers_each_line_and_closes() {
    let (exit, events) = run(concat!(
        "{\"command\":\"hello\",\"protocol_version\":1}\n",
        "\n",
        "{\"command\":\"set_input\",\"name\":\"u\",\"value\":1.0}\n",
        "{\"command\":\"step\",\"dt\":0.1}\n",
        "not json\n",
        "{\"command\":\"nonsense\"}\n",
        "{\"command\":\"close\"}\n",
        "{\"command\":\"state\"}\n",
    ));
    assert_eq!(exit, SessionServeExit::Closed);
    assert_eq!(exit.exit_code(), 0);
    assert_eq!(events.len(), 7, "{events:?}");
    assert_eq!(
        events[0],
        SessionEvent::Hello {
            protocol_version: SESSION_PROTOCOL_VERSION
        }
    );
    assert!(matches!(events[1], SessionEvent::Hello { .. }));
    assert!(matches!(events[2], SessionEvent::Ok { time } if time == 0.0));
    assert!(matches!(events[3], SessionEvent::Ok { time } if (time - 0.1).abs() < 1e-12));
    for event in &events[4..6] {
        let SessionEvent::Error { code, message } = event else {
            panic!("{event:?}");
        };
        assert_eq!(code, EX011_SESSION_MALFORMED_COMMAND);
        assert!(message.contains("invalid session command"), "{message}");
    }
    assert!(matches!(events[6], SessionEvent::Closed { .. }));
}

#[test]
fn serve_reports_parent_disconnect_without_close() {
    let (exit, events) = run("{\"command\":\"step\",\"dt\":0.1}\n");
    assert_eq!(exit, SessionServeExit::ParentDisconnected);
    assert_eq!(exit.exit_code(), SESSION_PARENT_DISCONNECTED_EXIT_CODE);
    assert_eq!(events.len(), 2);
}

#[test]
fn serve_ends_on_protocol_mismatch() {
    let (exit, events) = run(concat!(
        "{\"command\":\"hello\",\"protocol_version\":99}\n",
        "{\"command\":\"state\"}\n",
    ));
    assert_eq!(exit, SessionServeExit::ProtocolMismatch);
    assert_eq!(exit.exit_code(), SESSION_PROTOCOL_MISMATCH_EXIT_CODE);
    assert_eq!(events.len(), 2, "no command after the mismatch is served");
    assert_eq!(
        error_code(events[1].clone()),
        EX010_SESSION_PROTOCOL_VERSION
    );
}

struct FailingWriter(io::ErrorKind);

impl Write for FailingWriter {
    fn write(&mut self, _: &[u8]) -> io::Result<usize> {
        Err(self.0.into())
    }
    fn flush(&mut self) -> io::Result<()> {
        Err(self.0.into())
    }
}

#[test]
fn serve_maps_a_broken_pipe_to_disconnect_and_surfaces_other_write_errors() {
    let mut s = session(SimSolverMode::Bdf);
    let exit = serve_session(
        &mut s,
        Cursor::new(String::new()),
        &mut FailingWriter(io::ErrorKind::BrokenPipe),
    )
    .unwrap();
    assert_eq!(exit, SessionServeExit::ParentDisconnected);
    let error = serve_session(
        &mut s,
        Cursor::new(String::new()),
        &mut FailingWriter(io::ErrorKind::PermissionDenied),
    )
    .unwrap_err();
    assert_eq!(error.kind(), io::ErrorKind::PermissionDenied);

    // A pipe that breaks after the hello, while answering a command.
    struct BreaksAfterHello(usize);
    impl Write for BreaksAfterHello {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            if self.0 == 0 {
                return Err(io::ErrorKind::BrokenPipe.into());
            }
            self.0 -= usize::from(buf == b"\n");
            Ok(buf.len())
        }
        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }
    let exit = serve_session(
        &mut s,
        Cursor::new("{\"command\":\"state\"}\n".to_owned()),
        &mut BreaksAfterHello(1),
    )
    .unwrap();
    assert_eq!(exit, SessionServeExit::ParentDisconnected);
}

#[test]
fn wire_forms_are_stable() {
    let command: SessionCommand =
        serde_json::from_str(r#"{"command":"set_inputs","inputs":[["u",1.0],["v",2.0]]}"#).unwrap();
    assert_eq!(
        command,
        SessionCommand::SetInputs {
            inputs: vec![("u".into(), 1.0), ("v".into(), 2.0)]
        }
    );
    assert_eq!(
        serde_json::to_string(&SessionEvent::Ok { time: 0.5 }).unwrap(),
        r#"{"event":"ok","time":0.5}"#
    );
    assert_eq!(
        serde_json::to_string(&SessionEvent::Value {
            name: "y".into(),
            time: 1.0,
            value: None
        })
        .unwrap(),
        r#"{"event":"value","name":"y","time":1.0,"value":null}"#
    );
}

#[test]
fn non_utf8_and_over_long_lines_are_typed_refusals_and_the_session_continues() {
    let mut s = session(SimSolverMode::Bdf);
    let mut input = Vec::new();
    input.extend_from_slice(b"\xff\xfe not utf-8\n");
    input.extend_from_slice(&[b'x'; 100]);
    input.extend_from_slice(b"\n{\"command\":\"state\"}\n");
    input.extend_from_slice(b"{\"command\":\"input_names\"}\n");
    let mut out = Vec::new();
    let exit = serve_with_line_limit(&mut s, Cursor::new(input), &mut out, 64).unwrap();
    assert_eq!(exit, SessionServeExit::ParentDisconnected);
    let events: Vec<SessionEvent> = String::from_utf8(out)
        .unwrap()
        .lines()
        .map(|line| serde_json::from_str(line).unwrap())
        .collect();
    assert_eq!(events.len(), 5, "{events:?}");
    let SessionEvent::Error { code, message } = &events[1] else {
        panic!("{:?}", events[1]);
    };
    assert_eq!(code, EX011_SESSION_MALFORMED_COMMAND);
    assert!(message.contains("UTF-8"), "{message}");
    let SessionEvent::Error { code, message } = &events[2] else {
        panic!("{:?}", events[2]);
    };
    assert_eq!(code, EX011_SESSION_MALFORMED_COMMAND);
    assert!(message.contains("exceeds 64 bytes"), "{message}");
    assert!(matches!(events[3], SessionEvent::State { .. }));
    assert!(matches!(events[4], SessionEvent::InputNames { .. }));
}

#[test]
fn an_over_long_final_line_without_newline_is_refused_at_end_of_stream() {
    let mut s = session(SimSolverMode::Bdf);
    let mut out = Vec::new();
    let exit = serve_with_line_limit(&mut s, Cursor::new(vec![b'y'; 200]), &mut out, 64).unwrap();
    assert_eq!(exit, SessionServeExit::ParentDisconnected);
    assert_eq!(String::from_utf8(out).unwrap().lines().count(), 2);
}
