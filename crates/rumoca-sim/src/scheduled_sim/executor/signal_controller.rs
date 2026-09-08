//! Owned SIGINT/SIGTERM delivery and external-interface cleanup.

use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
#[cfg(unix)]
use std::thread;

#[cfg(unix)]
use crate::scheduled_sim::devices;

use super::{ExternalInterfaceHandle, ExternalInterfaceStopFailure, write_control_diagnostic};

#[derive(Debug, thiserror::Error)]
pub(super) enum SignalControllerInstallFailure {
    #[error("failed to register SIGINT/SIGTERM delivery: {detail}")]
    Register { detail: String },
    #[error("failed to spawn the owned signal-handler thread: {detail}")]
    ThreadSpawn { detail: String },
}

#[derive(Debug)]
pub(super) enum SignalControllerShutdownFailure {
    ThreadPanicked,
    CleanupFailures {
        failures: Box<[ExternalInterfaceStopFailure]>,
    },
}

impl std::fmt::Display for SignalControllerShutdownFailure {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ThreadPanicked => {
                formatter.write_str("owned signal-handler thread panicked during shutdown")
            }
            Self::CleanupFailures { failures } => {
                write!(
                    formatter,
                    "{} signal cleanup attempt(s) failed",
                    failures.len()
                )?;
                for failure in failures {
                    write!(formatter, "; {failure}")?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for SignalControllerShutdownFailure {}

struct SignalControllerThreadOutcome {
    cleanup_failures: Box<[ExternalInterfaceStopFailure]>,
}

pub(super) enum SignalAction {
    Continue,
    ExitAfterCleanup,
}

#[derive(Clone, Copy)]
pub(super) enum SignalStage {
    Initial,
    Forced,
}

pub(super) struct SignalController {
    #[cfg(unix)]
    handle: signal_hook::iterator::Handle,
    #[cfg(unix)]
    thread: Option<thread::JoinHandle<SignalControllerThreadOutcome>>,
}

impl SignalController {
    #[cfg(not(unix))]
    pub(super) fn install(
        _external_interface: ExternalInterfaceHandle,
        _quit: Arc<AtomicBool>,
    ) -> std::result::Result<Self, SignalControllerInstallFailure> {
        Ok(Self {})
    }

    #[cfg(unix)]
    pub(super) fn install(
        external_interface: ExternalInterfaceHandle,
        quit: Arc<AtomicBool>,
    ) -> std::result::Result<Self, SignalControllerInstallFailure> {
        use signal_hook::consts::{SIGINT, SIGTERM};
        use signal_hook::iterator::Signals;

        let mut signals = Signals::new([SIGINT, SIGTERM]).map_err(|error| {
            SignalControllerInstallFailure::Register {
                detail: error.to_string(),
            }
        })?;
        let handle = signals.handle();
        let thread = thread::Builder::new()
            .name("rumoca-sim-signal-handler".to_string())
            .spawn(move || signal_controller_loop(&mut signals, &external_interface, &quit))
            .map_err(|error| SignalControllerInstallFailure::ThreadSpawn {
                detail: error.to_string(),
            })?;
        write_control_diagnostic(format_args!(
            "  Shutdown: Ctrl-C once for clean exit; twice to force quit."
        ));
        Ok(Self {
            handle,
            thread: Some(thread),
        })
    }

    #[cfg(not(unix))]
    pub(super) fn shutdown(&mut self) -> std::result::Result<(), SignalControllerShutdownFailure> {
        Ok(())
    }

    #[cfg(unix)]
    pub(super) fn shutdown(&mut self) -> std::result::Result<(), SignalControllerShutdownFailure> {
        self.handle.close();
        let Some(thread) = self.thread.take() else {
            return Ok(());
        };
        let outcome = thread
            .join()
            .map_err(|_| SignalControllerShutdownFailure::ThreadPanicked)?;
        if outcome.cleanup_failures.is_empty() {
            Ok(())
        } else {
            Err(SignalControllerShutdownFailure::CleanupFailures {
                failures: outcome.cleanup_failures,
            })
        }
    }
}

#[cfg(unix)]
fn signal_controller_loop(
    signals: &mut signal_hook::iterator::Signals,
    external_interface: &ExternalInterfaceHandle,
    quit: &Arc<AtomicBool>,
) -> SignalControllerThreadOutcome {
    let mut stage = SignalStage::Initial;
    for signal in signals.forever() {
        request_shutdown(stage, quit);
        let cleanup = stop_external_interface_shared(external_interface);
        write_control_diagnostic(format_args!(
            "\r[sim] signal {signal} received                    \r"
        ));
        report_signal_stage(stage);
        match signal_action_after_cleanup(stage, cleanup) {
            Ok(SignalAction::Continue) => {}
            Ok(SignalAction::ExitAfterCleanup) => {
                devices::disable_terminal_raw_mode();
                std::process::exit(130);
            }
            Err(failure) => return signal_cleanup_failure_outcome(failure),
        }
        stage = SignalStage::Forced;
        devices::disable_terminal_raw_mode();
    }
    SignalControllerThreadOutcome {
        cleanup_failures: Box::default(),
    }
}

fn stop_external_interface_shared(
    external_interface: &ExternalInterfaceHandle,
) -> std::result::Result<(), ExternalInterfaceStopFailure> {
    external_interface.cancel()
}

fn request_shutdown(stage: SignalStage, quit: &Arc<AtomicBool>) {
    if matches!(stage, SignalStage::Initial) {
        quit.store(true, Ordering::Relaxed);
    }
}

#[cfg(unix)]
fn report_signal_stage(stage: SignalStage) {
    match stage {
        SignalStage::Initial => {
            write_control_diagnostic(format_args!(
                "[sim] shutdown requested — press Ctrl-C again to force quit"
            ));
        }
        SignalStage::Forced => {
            write_control_diagnostic(format_args!(
                "[sim] force quit requested after owned cleanup"
            ));
        }
    }
}

#[cfg(unix)]
fn signal_cleanup_failure_outcome(
    failure: ExternalInterfaceStopFailure,
) -> SignalControllerThreadOutcome {
    write_control_diagnostic(format_args!(
        "[external_interface] signal cleanup failure; exit refused: {failure}"
    ));
    SignalControllerThreadOutcome {
        cleanup_failures: Box::new([failure]),
    }
}

pub(super) fn signal_action_after_cleanup(
    stage: SignalStage,
    cleanup: std::result::Result<(), ExternalInterfaceStopFailure>,
) -> std::result::Result<SignalAction, ExternalInterfaceStopFailure> {
    cleanup?;
    match stage {
        SignalStage::Initial => Ok(SignalAction::Continue),
        SignalStage::Forced => Ok(SignalAction::ExitAfterCleanup),
    }
}

impl Drop for SignalController {
    fn drop(&mut self) {
        if let Err(failure) = self.shutdown() {
            write_control_diagnostic(format_args!(
                "[sim] signal-controller shutdown failure: {failure}"
            ));
        }
    }
}
