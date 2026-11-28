//! Rule B's calibration pass: the pinned calibration set
//! (`rumoca_msl_quality::speed::CALIBRATION_MODELS`), OMC-timed fresh on every
//! shard after the shard's own timed pass, so every shard's runner speed is
//! measured on the same work and the speeds compare directly. Never read from
//! the cache: a reused timing was taken on another runner.

use super::*;
use rumoca_msl_quality::speed::CALIBRATION_MODELS;

/// The context a timing of this process is taken under: the worker count, OMC
/// threads, host, and the CI run and runner.
pub(super) fn timing_context(args: &Args, workers: usize) -> Value {
    json!({
        "workers": workers,
        "omc_threads": args.omc_threads,
        "host": output::host_description(),
        "run": output::run_provenance(),
    })
}

/// Time the calibration set in a session pool of `workers`, like the timed
/// pass, and return the section the reference file records.
pub(super) fn time_calibration_set(args: &Args, workers: usize, paths: &MslPaths) -> Result<Value> {
    let models = CALIBRATION_MODELS
        .iter()
        .map(|model| (*model).to_string())
        .collect::<Vec<_>>();
    let config = session_pool_config(args, workers, paths, models.len())?;
    let job = TimedSimulation {
        stop_time: args.stop_time,
        use_experiment: args.use_experiment_stop_time,
        sim_timeout: Duration::from_secs(args.batch_timeout_seconds.max(1)),
    };
    let start = Instant::now();
    let mut timings = serde_json::Map::new();
    run_session_pool(&models, &config, &job, |idx, outcome| {
        timings.insert(
            models[idx].clone(),
            json!({
                "status": outcome.result.status,
                "omc_wall_seconds": outcome.result.omc_wall_seconds,
                "timed_out": outcome.timed_out,
            }),
        );
    });
    let elapsed = start.elapsed().as_secs_f64();
    println!(
        "OMC calibration pass: {} models in {elapsed:.1} s",
        models.len()
    );
    Ok(json!({
        "context": timing_context(args, workers),
        "elapsed_seconds": elapsed,
        "models": timings,
    }))
}
