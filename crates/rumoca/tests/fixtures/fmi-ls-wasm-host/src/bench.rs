//! `bench <component> <token> <t_start> <steps> <dt>`: the wall-clock cost of
//! loading the FMU (compile and instantiate), of its initialization, and of
//! each Co-Simulation step of `dt`, printed as one JSON line. The host writes
//! no input, so every input keeps its published instantiation default, and
//! reads no value: this measures cost only, not behaviour.

use std::time::Instant;

use anyhow::{Context, Result, bail, ensure};

use super::{Status, load};

fn elapsed_ms(started: Instant) -> f64 {
    started.elapsed().as_secs_f64() * 1.0e3
}

pub(super) fn run(component_path: &str, token: &str, args: &[String]) -> Result<()> {
    ensure!(args.len() == 3, "bench needs t_start steps dt");
    let t_start: f64 = args[0].parse().context("t_start")?;
    let steps: usize = args[1].parse().context("steps")?;
    let dt: f64 = args[2].parse().context("dt")?;
    ensure!(dt > 0.0 && steps > 0, "invalid bench window");

    let started = Instant::now();
    let mut fmu = load(component_path, token)?;
    let load_ms = elapsed_ms(started);

    let started = Instant::now();
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface.call_enter_initialization_mode(&mut fmu.store, fmu.instance, None, t_start, None)?
            == Status::Ok
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
        step_ms.push(elapsed_ms(started));
        match outcome {
            Ok(result) if result.last_successful_time > current => {
                current = result.last_successful_time;
            }
            Ok(_) => bail!("do-step made no progress at t={current}"),
            Err(status) => bail!("do-step failed at t={current}: {status:?}"),
        }
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
