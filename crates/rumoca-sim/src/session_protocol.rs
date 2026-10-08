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

use std::io::{self, BufRead, Read, Write};

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use crate::{
    EX010_SESSION_PROTOCOL_VERSION, EX011_SESSION_MALFORMED_COMMAND,
    EX012_SESSION_INVALID_ARGUMENT, SimExecutionReceipt, SimulationDiagnosticError,
    SimulationSession,
};

#[cfg(test)]
mod tests;

/// Version of the command/event protocol; bumped on any incompatible change.
pub const SESSION_PROTOCOL_VERSION: u32 = 1;

/// Exit status when the controlling process closes its command stream without
/// sending `close` (the same status `rumoca-worker` reports).
pub const SESSION_PARENT_DISCONNECTED_EXIT_CODE: i32 = 71;

/// Exit status when the controlling process declares an unsupported version.
pub const SESSION_PROTOCOL_MISMATCH_EXIT_CODE: i32 = 72;

/// Longest accepted command line, newline excluded; a longer line is refused
/// with `EX011` and skipped.
pub const SESSION_MAX_LINE_BYTES: usize = 16 * 1024 * 1024;

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
    /// The protocol version this build speaks and the execution engine the
    /// session selected.
    Hello {
        protocol_version: u32,
        engine: SimExecutionReceipt,
    },
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
                Ok(self.hello())
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

    /// The `hello` event of this session.
    pub(crate) fn hello(&self) -> SessionEvent {
        SessionEvent::Hello {
            protocol_version: SESSION_PROTOCOL_VERSION,
            engine: self.execution_receipt(),
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
    serve_with_line_limit(session, input, output, SESSION_MAX_LINE_BYTES)
}

fn serve_with_line_limit(
    session: &mut SimulationSession,
    mut input: impl BufRead,
    output: &mut impl Write,
    max_line_bytes: usize,
) -> io::Result<SessionServeExit> {
    let hello = session.hello();
    if let Some(exit) = write_event(output, &hello)? {
        return Ok(exit);
    }
    let mut buffer = Vec::new();
    while let Some(line) = next_command_line(&mut input, &mut buffer, max_line_bytes)? {
        let event = match line {
            Ok(line) if line.trim().is_empty() => continue,
            Ok(line) => match serde_json::from_str::<SessionCommand>(&line) {
                Ok(command) => session.apply(command),
                Err(error) => malformed(format!("invalid session command: {error}")),
            },
            Err(reason) => malformed(reason),
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

fn malformed(message: String) -> SessionEvent {
    SessionEvent::error(EX011_SESSION_MALFORMED_COMMAND, message)
}

/// Read the next command line without buffering more than `max_line_bytes`.
///
/// `Ok(None)` is the end of the stream. An over-long or non-UTF-8 line is
/// consumed whole and reported as `Err(reason)`, so the stream stays aligned on
/// line boundaries and the session continues.
fn next_command_line(
    input: &mut impl BufRead,
    buffer: &mut Vec<u8>,
    max_line_bytes: usize,
) -> io::Result<Option<Result<String, String>>> {
    buffer.clear();
    let limit = u64::try_from(max_line_bytes)
        .unwrap_or(u64::MAX)
        .saturating_add(1);
    let mut bounded = Read::take(&mut *input, limit);
    if bounded.read_until(b'\n', buffer)? == 0 {
        return Ok(None);
    }
    if buffer.last() != Some(&b'\n') && buffer.len() > max_line_bytes {
        discard_rest_of_line(input)?;
        return Ok(Some(Err(format!(
            "session command line exceeds {max_line_bytes} bytes"
        ))));
    }
    Ok(Some(String::from_utf8(std::mem::take(buffer)).map_err(
        |error| format!("session command line is not valid UTF-8: {error}"),
    )))
}

fn discard_rest_of_line(input: &mut impl BufRead) -> io::Result<()> {
    loop {
        let chunk = input.fill_buf()?;
        if chunk.is_empty() {
            return Ok(());
        }
        if let Some(end) = chunk.iter().position(|byte| *byte == b'\n') {
            input.consume(end + 1);
            return Ok(());
        }
        let length = chunk.len();
        input.consume(length);
    }
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
