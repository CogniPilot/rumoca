//! `bench <component> <token> <t_start> <steps> <dt>`: the wall-clock cost of
//! loading the FMU (compile and instantiate), of its initialization, and of
//! each Co-Simulation step of `dt`, printed as one JSON line. The host writes
//! no input, so every input keeps its published instantiation default, and
//! reads no value: this measures cost only, not behaviour. A step is counted
//! only when it completed its requested target exactly with no event,
//! termination or early return; any other outcome ends the run.

use std::time::Instant;

use anyhow::{Context, Result, bail, ensure};

use super::{Status, load};

fn elapsed_ms(started: Instant) -> f64 {
    started.elapsed().as_secs_f64() * 1.0e3
}

/// What the host saw of one step: the fields of `do-step-result`.
#[derive(Clone, Copy, Debug)]
struct Outcome {
    last_successful_time: f64,
    event_handling_needed: bool,
    terminate_simulation: bool,
    early_return: bool,
}

/// The time a step from `current` of `dt` completed at, when it completed the
/// requested target `current + dt` exactly, bit for bit, with a finite,
/// advancing request and no event, termination or early return.
fn completed_step(current: f64, dt: f64, outcome: Outcome) -> Result<f64> {
    let target = current + dt;
    ensure!(
        current.is_finite() && dt.is_finite() && dt > 0.0 && target.is_finite() && target > current,
        "step request from t={current} of {dt} is not finite and advancing"
    );
    ensure!(
        !outcome.event_handling_needed && !outcome.terminate_simulation && !outcome.early_return,
        "step from t={current} returned {outcome:?}"
    );
    ensure!(
        outcome.last_successful_time.to_bits() == target.to_bits(),
        "step from t={current} reached {} instead of {target}",
        outcome.last_successful_time
    );
    Ok(target)
}

pub(super) fn run(component_path: &str, token: &str, args: &[String]) -> Result<()> {
    ensure!(args.len() == 3, "bench needs t_start steps dt");
    let t_start: f64 = args[0].parse().context("t_start")?;
    let steps: usize = args[1].parse().context("steps")?;
    let dt: f64 = args[2].parse().context("dt")?;
    ensure!(
        t_start.is_finite() && dt.is_finite() && dt > 0.0 && steps > 0,
        "invalid bench window"
    );

    let started = Instant::now();
    let mut fmu = load(component_path, token)?;
    let load_ms = elapsed_ms(started);

    let started = Instant::now();
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface.call_enter_initialization_mode(
            &mut fmu.store,
            fmu.instance,
            None,
            t_start,
            None
        )? == Status::Ok
    );
    ensure!(interface.call_exit_initialization_mode(&mut fmu.store, fmu.instance)? == Status::Ok);
    let init_ms = elapsed_ms(started);

    let mut current = t_start;
    let mut step_ms = Vec::with_capacity(steps);
    for _ in 0..steps {
        let started = Instant::now();
        let outcome = fmu
            .world
            .fmi_fmi3_co_simulation()
            .co_simulation_instance()
            .call_do_step(&mut fmu.store, fmu.instance, current, dt, false)?;
        let elapsed = elapsed_ms(started);
        let result = match outcome {
            Ok(result) => result,
            Err(status) => bail!("do-step failed at t={current}: {status:?}"),
        };
        current = completed_step(
            current,
            dt,
            Outcome {
                last_successful_time: result.last_successful_time,
                event_handling_needed: result.event_handling_needed,
                terminate_simulation: result.terminate_simulation,
                early_return: result.early_return,
            },
        )?;
        step_ms.push(elapsed);
    }
    ensure!(
        fmu.world
            .fmi_fmi3_co_simulation()
            .co_simulation_instance()
            .call_terminate(&mut fmu.store, fmu.instance)?
            == Status::Ok
    );

    let mean = step_ms.iter().sum::<f64>() / steps as f64;
    let mut sorted = step_ms;
    sorted.sort_by(f64::total_cmp);
    let quantile = |q: f64| sorted[((sorted.len() - 1) as f64 * q).round() as usize];
    println!(
        "{{\"load_ms\":{load_ms},\"init_ms\":{init_ms},\"steps\":{steps},\"dt\":{dt},\
         \"end_time\":{current},\"step_ms\":{{\"mean\":{mean},\"p50\":{},\"p95\":{},\"max\":{}}}}}",
        quantile(0.5),
        quantile(0.95),
        quantile(1.0)
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{Outcome, completed_step};

    fn reached(last_successful_time: f64) -> Outcome {
        Outcome {
            last_successful_time,
            event_handling_needed: false,
            terminate_simulation: false,
            early_return: false,
        }
    }

    #[test]
    fn only_an_exactly_completed_plain_step_is_counted() {
        assert_eq!(
            completed_step(0.04, 0.02, reached(0.04 + 0.02)).unwrap(),
            0.04 + 0.02
        );
        let partial = reached(0.05);
        assert!(completed_step(0.04, 0.02, partial).is_err(), "partial");
        assert!(
            completed_step(0.04, 0.02, reached(0.04)).is_err(),
            "no progress"
        );
        for flagged in [
            Outcome {
                early_return: true,
                ..reached(0.06)
            },
            Outcome {
                terminate_simulation: true,
                ..reached(0.06)
            },
            Outcome {
                event_handling_needed: true,
                ..reached(0.06)
            },
        ] {
            assert!(completed_step(0.04, 0.02, flagged).is_err(), "{flagged:?}");
        }
        for (current, dt) in [
            (f64::NAN, 0.02),
            (0.0, f64::INFINITY),
            (0.0, 0.0),
            (0.0, -0.02),
        ] {
            assert!(
                completed_step(current, dt, reached(0.02)).is_err(),
                "{current} {dt}"
            );
        }
        assert!(completed_step(f64::MAX, f64::MAX, reached(f64::INFINITY)).is_err());
    }
}
