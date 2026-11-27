//! A pool of persistent OMC sessions: the OMC analogue of the rumoca warm
//! worker queue.
//!
//! Each worker thread owns one [`OmcSession`] that loads the MSL once, then
//! pulls models from a shared index and runs one [`SessionJob`] step on each.
//! A step that poisons its session (a timeout or a transport failure) has the
//! session killed, and the next model respawns a fresh one, so a single hung
//! model never blocks the others. Sessions are also recycled every
//! [`SESSION_RECYCLE_MODELS`] models to bound their memory.
//!
//! The pool knows nothing about what a step does: the timed reference run and
//! the post-timing tight reference run are two jobs on the same pool.

use super::omc_session::OmcSession;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

/// Recycle an omc session after this many models to bound its memory growth.
///
/// A long-lived omc interactive process accumulates memory across simulate()
/// calls (loaded result sets, codegen scratch); recycling bounds RSS while
/// still amortizing the MSL load across the batch (one reload per recycle, not
/// per model).
const SESSION_RECYCLE_MODELS: usize = 25;

/// What every session of a pool is started with.
pub(super) struct SessionPoolConfig {
    pub(super) msl_exprs: Vec<String>,
    pub(super) services_package: PathBuf,
    pub(super) work_dir: PathBuf,
    pub(super) omc_threads: usize,
    pub(super) startup_timeout: Duration,
    pub(super) load_timeout: Duration,
    pub(super) worker_count: usize,
}

/// One model's step on a live session.
pub(super) trait SessionJob: Sync {
    type Outcome: Send;

    /// Run the step for `model`. The flag is true when the step poisoned the
    /// session (a timeout or a transport failure), which is then killed.
    fn run(&self, session: &mut OmcSession, model: &str) -> (Self::Outcome, bool);

    /// The outcome of a model whose session could not be spawned.
    fn spawn_failed(&self, error: String) -> Self::Outcome;
}

/// Run `job` on every model over `config.worker_count` sessions, each worker
/// pinned to its own core when there are enough, handing each outcome to
/// `on_outcome` (with the model's index into `models`) as it completes.
pub(super) fn run_session_pool<J: SessionJob>(
    models: &[String],
    config: &SessionPoolConfig,
    job: &J,
    mut on_outcome: impl FnMut(usize, J::Outcome),
) {
    let worker_count = config.worker_count.max(1).min(models.len());
    let core_plan = rumoca_worker::cpu_core_plan(worker_count);
    let next = AtomicUsize::new(0);
    let (tx, rx) = mpsc::channel::<(usize, J::Outcome)>();
    thread::scope(|scope| {
        for worker_idx in 0..worker_count {
            let tx = tx.clone();
            let cpu_core_id = core_plan.get(worker_idx).copied().flatten();
            let next = &next;
            scope.spawn(move || run_worker(models, next, &tx, config, job, cpu_core_id));
        }
        drop(tx);
        for (idx, outcome) in rx {
            on_outcome(idx, outcome);
        }
    });
}

fn run_worker<J: SessionJob>(
    models: &[String],
    next: &AtomicUsize,
    tx: &mpsc::Sender<(usize, J::Outcome)>,
    config: &SessionPoolConfig,
    job: &J,
    cpu_core_id: Option<usize>,
) {
    // Pin before spawning OMC: on Linux the omc child inherits this thread's
    // CPU affinity, so the whole session stays on one core.
    if let Some(core_id) = cpu_core_id
        && let Err(error) = rumoca_worker::pin_current_thread_to_cpu_core(core_id)
    {
        eprintln!("  OMC session worker: failed to pin to CPU core {core_id}: {error}");
    }
    let mut session: Option<OmcSession> = None;
    let mut models_on_session = 0usize;
    loop {
        let idx = next.fetch_add(1, Ordering::Relaxed);
        let Some(model) = models.get(idx) else {
            break;
        };
        if session.is_none() {
            match OmcSession::spawn(
                &config.work_dir,
                &config.msl_exprs,
                &config.services_package,
                config.omc_threads,
                config.startup_timeout,
                config.load_timeout,
            ) {
                Ok(spawned) => {
                    session = Some(spawned);
                    models_on_session = 0;
                }
                Err(error) => {
                    let outcome = job.spawn_failed(format!("omc session spawn failed: {error}"));
                    let _ = tx.send((idx, outcome));
                    continue;
                }
            }
        }
        let live = session.as_mut().expect("session present after spawn");
        let (outcome, poisoned) = job.run(live, model);
        if poisoned && let Some(mut dead) = session.take() {
            // A poisoned REQ socket after a hung model: kill and respawn.
            dead.kill();
        }
        let _ = tx.send((idx, outcome));
        if session.is_some() {
            models_on_session += 1;
            if models_on_session >= SESSION_RECYCLE_MODELS
                && let Some(mut spent) = session.take()
            {
                spent.kill();
            }
        }
    }
}
