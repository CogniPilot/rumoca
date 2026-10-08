//! Versioned command and event protocol of [`SimulationSession`].
//!
//! [`SimulationSession::apply`] is the single dispatcher of the session's
//! externally driven surface: every binding that exposes the session to another
//! language or process (the WASM binding, the CLI's `--serve-stdio` mode) sends
//! [`SessionCommand`] values through it and receives [`SessionEvent`] values
//! back, so the semantics of each command are defined once.
//!
//! On the wire both enums are single-line JSON objects tagged by `command` and
//! `event`, one per line. Failures are `error` events carrying a SPEC_0008
//! code: `EX001`/`EX002`/`EX003` from the solver for runtime failures, and the
//! protocol codes below for malformed or unacceptable commands.

use std::io::{self, BufRead, Write};

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use crate::{SimulationDiagnosticError, SimulationSession};

#[cfg(test)]
mod tests;

/// Version of the command/event protocol; bumped on any incompatible change.
pub const SESSION_PROTOCOL_VERSION: u32 = 1;

/// Exit status when the controlling process closes its command stream without
/// sending `close` (the same status `rumoca-worker` reports).
pub const SESSION_PARENT_DISCONNECTED_EXIT_CODE: i32 = 71;

/// Exit status when the controlling process declares an unsupported version.
pub const SESSION_PROTOCOL_MISMATCH_EXIT_CODE: i32 = 72;

/// The controlling process declared a protocol version this build does not speak.
pub const EX010_SESSION_PROTOCOL_VERSION: &str = "EX010";
/// A command line was not a valid [`SessionCommand`].
pub const EX011_SESSION_MALFORMED_COMMAND: &str = "EX011";
/// A command carried an argument outside its domain (non-finite or negative time).
pub const EX012_SESSION_INVALID_ARGUMENT: &str = "EX012";

/// One request to a [`SimulationSession`].
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "command", rename_all = "snake_case")]
pub enum SessionCommand {
    /// Declare the protocol version the controller speaks.
    Hello { protocol_version: u32 },
    /// Set one input; takes effect on the next advance.
    SetInput { name: String, value: f64 },
    /// Apply one atomic input frame, `[["name", value], ...]`. The integrator
    /// history restarts once for the whole frame, and not at all when every
    /// value is bit-identical to the current input.
    SetInputs { inputs: Vec<(String, f64)> },
    /// Advance by a relative time step in seconds.
    Step { dt: f64 },
    /// Advance to an absolute time in seconds.
    AdvanceTo { time: f64 },
    /// Read one variable.
    Get { name: String },
    /// Read every variable.
    State,
    /// Restart initialization at an absolute time (default `0`).
    Reset {
        #[serde(default)]
        time: Option<f64>,
    },
    /// List the declared input names.
    InputNames,
    /// List the solver variable names.
    VariableNames,
    /// End the session.
    Close,
}

/// One response from a [`SimulationSession`].
///
/// A value JSON cannot represent (NaN, infinity) is written as `null`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "event", rename_all = "snake_case")]
pub enum SessionEvent {
    /// The protocol version this build speaks.
    Hello { protocol_version: u32 },
    /// The command took effect; `time` is the session time afterwards.
    Ok { time: f64 },
    /// One variable; `value` is `null` when the name is not a variable.
    Value {
        name: String,
        time: f64,
        value: Option<f64>,
    },
    /// Every variable at `time`.
    State {
        time: f64,
        values: IndexMap<String, f64>,
    },
    /// Declared input names.
    InputNames { names: Vec<String> },
    /// Solver variable names.
    VariableNames { names: Vec<String> },
    /// The session ended at `time`.
    Closed { time: f64 },
    /// The command was rejected or failed; the session state is unchanged by a
    /// rejection and follows the solver's own atomicity for a failure.
    Error { code: String, message: String },
}

impl SessionEvent {
    pub(crate) fn error(code: &str, message: impl Into<String>) -> Self {
        Self::Error {
            code: code.to_owned(),
            message: message.into(),
        }
    }

    fn fault(error: &SimulationDiagnosticError) -> Self {
        Self::error(error.diagnostic_code(), error.to_string())
    }
}

fn finite_time(label: &str, value: f64) -> Result<f64, SessionEvent> {
    if value.is_finite() {
        Ok(value)
    } else {
        Err(SessionEvent::error(
            EX012_SESSION_INVALID_ARGUMENT,
            format!("{label} must be finite"),
        ))
    }
}

fn nonnegative_time(label: &str, value: f64) -> Result<f64, SessionEvent> {
    finite_time(label, value).and_then(|value| {
        if value >= 0.0 {
            Ok(value)
        } else {
            Err(SessionEvent::error(
                EX012_SESSION_INVALID_ARGUMENT,
                format!("{label} must be nonnegative"),
            ))
        }
    })
}

impl SimulationSession {
    /// Execute one protocol command and report its outcome.
    ///
    /// Total: every command, valid or not, yields exactly one event.
    pub fn apply(&mut self, command: SessionCommand) -> SessionEvent {
        self.dispatch(command).unwrap_or_else(|event| event)
    }

    fn dispatch(&mut self, command: SessionCommand) -> Result<SessionEvent, SessionEvent> {
        let fault = |error: SimulationDiagnosticError| SessionEvent::fault(&error);
        match command {
            SessionCommand::Hello { protocol_version } => {
                if protocol_version != SESSION_PROTOCOL_VERSION {
                    return Err(SessionEvent::error(
                        EX010_SESSION_PROTOCOL_VERSION,
                        format!(
                            "unsupported session protocol version {protocol_version}; \
                             this build speaks {SESSION_PROTOCOL_VERSION}"
                        ),
                    ));
                }
                Ok(SessionEvent::Hello {
                    protocol_version: SESSION_PROTOCOL_VERSION,
                })
            }
            SessionCommand::SetInput { name, value } => {
                self.set_input(&name, value).map_err(fault)?;
                Ok(self.ok())
            }
            SessionCommand::SetInputs { inputs } => {
                let batch: Vec<_> = inputs
                    .iter()
                    .map(|(name, value)| (name.as_str(), *value))
                    .collect();
                self.set_inputs(&batch).map_err(fault)?;
                Ok(self.ok())
            }
            SessionCommand::Step { dt } => {
                let dt = nonnegative_time("dt", dt)?;
                self.ensure_end_time(self.time() + dt);
                self.step(dt).map_err(fault)?;
                Ok(self.ok())
            }
            SessionCommand::AdvanceTo { time } => {
                let time = finite_time("time", time)?;
                self.ensure_end_time(time);
                self.advance_to(time).map_err(fault)?;
                Ok(self.ok())
            }
            SessionCommand::Get { name } => {
                let value = self.get(&name).map_err(fault)?;
                Ok(SessionEvent::Value {
                    name,
                    time: self.time(),
                    value,
                })
            }
            SessionCommand::State => {
                let state = self.state().map_err(fault)?;
                Ok(SessionEvent::State {
                    time: state.time,
                    values: state.values,
                })
            }
            SessionCommand::Reset { time } => {
                let time = nonnegative_time("reset time", time.unwrap_or(0.0))?;
                self.reset(time).map_err(fault)?;
                Ok(self.ok())
            }
            SessionCommand::InputNames => Ok(SessionEvent::InputNames {
                names: self.input_names().to_vec(),
            }),
            SessionCommand::VariableNames => Ok(SessionEvent::VariableNames {
                names: self.variable_names().to_vec(),
            }),
            SessionCommand::Close => Ok(SessionEvent::Closed { time: self.time() }),
        }
    }

    fn ok(&self) -> SessionEvent {
        SessionEvent::Ok { time: self.time() }
    }
}

/// How a [`serve_session`] loop ended.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SessionServeExit {
    /// The controller sent `close`.
    Closed,
    /// The controller's streams ended without `close`.
    ParentDisconnected,
    /// The controller declared an unsupported protocol version.
    ProtocolMismatch,
}

impl SessionServeExit {
    /// Process exit status for this ending.
    #[must_use]
    pub fn exit_code(self) -> i32 {
        match self {
            Self::Closed => 0,
            Self::ParentDisconnected => SESSION_PARENT_DISCONNECTED_EXIT_CODE,
            Self::ProtocolMismatch => SESSION_PROTOCOL_MISMATCH_EXIT_CODE,
        }
    }
}

/// Drive `session` from JSON-lines commands on `input`, writing one JSON event
/// line to `output` per command after an initial `hello`.
///
/// A line that is not a valid command is answered with an `EX011` error and the
/// loop continues. A broken output pipe is a parent disconnect; any other I/O
/// failure is returned.
pub fn serve_session(
    session: &mut SimulationSession,
    input: impl BufRead,
    output: &mut impl Write,
) -> io::Result<SessionServeExit> {
    let hello = SessionEvent::Hello {
        protocol_version: SESSION_PROTOCOL_VERSION,
    };
    if let Some(exit) = write_event(output, &hello)? {
        return Ok(exit);
    }
    for line in input.lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        let event = match serde_json::from_str::<SessionCommand>(&line) {
            Ok(command) => session.apply(command),
            Err(error) => SessionEvent::error(
                EX011_SESSION_MALFORMED_COMMAND,
                format!("invalid session command: {error}"),
            ),
        };
        if let Some(exit) = write_event(output, &event)? {
            return Ok(exit);
        }
        match event {
            SessionEvent::Closed { .. } => return Ok(SessionServeExit::Closed),
            SessionEvent::Error { ref code, .. } if code == EX010_SESSION_PROTOCOL_VERSION => {
                return Ok(SessionServeExit::ProtocolMismatch);
            }
            _ => {}
        }
    }
    Ok(SessionServeExit::ParentDisconnected)
}

/// Write one event line; `Some` when the reader has gone away.
fn write_event(
    output: &mut impl Write,
    event: &SessionEvent,
) -> io::Result<Option<SessionServeExit>> {
    let written = serde_json::to_writer(&mut *output, event)
        .map_err(io::Error::from)
        .and_then(|()| writeln!(output))
        .and_then(|()| output.flush());
    match written {
        Ok(()) => Ok(None),
        Err(error) if error.kind() == io::ErrorKind::BrokenPipe => {
            Ok(Some(SessionServeExit::ParentDisconnected))
        }
        Err(error) => Err(error),
    }
}
