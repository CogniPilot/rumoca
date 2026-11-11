//! Production opt-in plugin through the same correlated FMI ME host as RK.

use super::{SimOptions, SimSolverMode, compile};
use rumoca_solver::{
    SimExecutionPolicy, SimResult,
    fmi_me::{MeRetainedComponent, driver::batch_output_cursor},
};

fn run(source: &str, end: f64, step: f64) -> Result<SimResult, String> {
    run_core(source, end, step, false).map(|(result, _)| result)
}

fn run_counted(
    source: &str,
    end: f64,
    step: f64,
) -> Result<(SimResult, rumoca_solver_semiimplicit::CartesianWorkCounts), String> {
    run_core(source, end, step, true)
}

fn run_core(
    source: &str,
    end: f64,
    step: f64,
    observed: bool,
) -> Result<(SimResult, rumoca_solver_semiimplicit::CartesianWorkCounts), String> {
    let opts = SimOptions {
        execution_policy: SimExecutionPolicy::Interpreter,
        solver_mode: SimSolverMode::RkLike,
        t_end: end,
        dt: Some(0.02),
        ..Default::default()
    };
    let dae = compile(source, "Case");
    let (artifact, backend) = super::super::lower_runtime_fmi_artifact(&dae, &opts).unwrap();
    let backend =
        rumoca_solver::fmi_me::admit_execution_backend(opts.execution_policy, backend).unwrap();
    let options = crate::me_backend::batch_options(&artifact.root_location(), &opts).unwrap();
    let mut cursor = batch_output_cursor(&options).unwrap();
    let retained = MeRetainedComponent::instantiate(
        artifact.source(),
        &crate::me_backend::instance_config("cartesian-test", &opts).unwrap(),
        backend,
    )
    .unwrap();
    let host = retained.into_lease(options).unwrap();
    let setup = host.numerical_setup(Some(step)).unwrap();
    let (plugin, observer) = if observed {
        rumoca_solver_semiimplicit::model_exchange_integrator_observed(setup, step)
            .map_err(|error| error.to_string())?
    } else {
        let plugin = if step == 0.005 {
            rumoca_solver_semiimplicit::model_exchange_integrator(setup)
        } else {
            rumoca_solver_semiimplicit::model_exchange_integrator_with_step(setup, step)
                .map_err(|error| error.to_string())?
        };
        (
            plugin,
            rumoca_solver_semiimplicit::CartesianWorkObserver::default(),
        )
    };
    let mut session = host
        .into_session(Some(plugin))
        .map_err(|error| error.to_string())?;
    if observed {
        // The walking importer yields on the 20 ms policy grid, rather than
        // presenting one uninterrupted four-second horizon.
        let ticks = (end / 0.02).ceil() as usize;
        for tick in 1..=ticks {
            session
                .advance_to((tick as f64 * 0.02).min(end), &mut cursor)
                .map_err(|error| error.to_string())?;
        }
    } else {
        session
            .run_to_stop(&mut cursor)
            .map_err(|error| error.to_string())?;
    }
    Ok((session.finish(), observer.snapshot()))
}

fn last(result: &SimResult, name: &str) -> f64 {
    let index = result.names.iter().position(|value| value == name).unwrap();
    *result.data[index].last().unwrap()
}

#[test]
fn free_particle_and_constant_acceleration_use_new_velocity() {
    for acceleration in [0.0, -9.81] {
        let source = format!(
            "model Case Real q(start=1,fixed=true); Real v(start=2,fixed=true); equation der(q)=v; der(v)={acceleration}; end Case;"
        );
        let result = run(&source, 0.02, 0.005).unwrap();
        assert!((last(&result, "v") - (2.0 + 0.02 * acceleration)).abs() < 1e-12);
        // Four steps: sum of updated velocities is 4*v0 + h*a*(1+2+3+4).
        let expected = 1.0 + 0.02 * 2.0 + 0.005_f64.powi(2) * acceleration * 10.0;
        assert!((last(&result, "q") - expected).abs() < 1e-12);
    }
}

#[test]
fn oscillator_converges_at_first_order() {
    let source = "model Case Real q(start=1,fixed=true); Real v(start=0,fixed=true); equation der(q)=v; der(v)=-q; end Case;";
    let errors = [0.005, 0.0025, 0.00125]
        .map(|step| (last(&run(source, 1.0, step).unwrap(), "q") - 1.0_f64.cos()).abs());
    for pair in errors.windows(2) {
        assert!((1.9..2.1).contains(&(pair[0] / pair[1])), "{errors:?}");
    }
}

#[test]
fn hard_stop_clips_without_overshooting() {
    let result = run("model Case Real q(start=1,fixed=true); Real v(start=2,fixed=true); equation der(q)=v; der(v)=0; end Case;", 0.012, 0.005).unwrap();
    assert!((result.times.last().unwrap() - 0.012).abs() < 1e-14);
    assert!((last(&result, "q") - 1.024).abs() < 1e-12);
}

#[test]
fn four_seconds_require_800_step_start_derivative_requests() {
    let (result, counts) = run_counted("model Case Real q(start=1,fixed=true); Real v(start=2,fixed=true); equation der(q)=v; der(v)=0; end Case;", 4.0, 0.005).unwrap();
    assert_eq!(counts.derivative_requests, 800);
    assert_eq!(counts.step_candidates, 800);
    assert!((last(&result, "q") - 9.0).abs() < 1e-10);
    println!(
        "Cartesian numerical request counts: {counts:?}; guest/mass/constraint observer calls outside this counter's coverage"
    );
}

#[test]
fn incomplete_or_ambiguous_partitions_refuse_only_the_opt_in_plugin() {
    for source in [
        "model Case Real x(start=1,fixed=true); equation der(x)=-x; end Case;",
        "model Case Real q(start=1,fixed=true); Real v(start=0,fixed=true); equation der(q)=v; der(v)=q; end Case;",
    ] {
        assert!(run(source, 0.02, 0.005).is_err());
        assert!(
            super::simulate_dae(
                &compile(source, "Case"),
                &SimOptions {
                    solver_mode: SimSolverMode::RkLike,
                    t_end: 0.02,
                    dt: Some(0.02),
                    ..Default::default()
                }
            )
            .is_ok()
        );
    }
}

#[test]
fn time_event_reinitializes_velocity_through_the_common_host() {
    let source = "model Case Real q(start=0,fixed=true); Real v(start=1,fixed=true); equation der(q)=v; der(v)=0; when time>=0.01 then reinit(v,2); end when; end Case;";
    let result = run(source, 0.02, 0.005).unwrap();
    assert!((last(&result, "v") - 2.0).abs() < 1e-12);
    assert!((last(&result, "q") - 0.03).abs() < 1e-10);
}
