//! A fixed-step classical RK4 drive of one component through the Model
//! Exchange calls a fixed-step importer makes: the four stage derivatives,
//! then the accepted point, the completed integrator step, and the event
//! iteration any requested event needs. A generated component driven by the
//! same calls is compared with it step for step, reduced-chart switches
//! included (SPEC_0040 STRUCT-T07 constraint-fold chart rows).

use super::{
    MeError, MeEventCause, MeEventEntry, MeInstanceConfig, MeModelArtifact, MeTime, SolveMeKernel,
};

/// One reduced-chart switch the drive requested at an accepted step.
#[derive(Clone, Debug, PartialEq)]
pub struct FixedStepChartSwitch {
    /// The accepted step, counted from one.
    pub step: usize,
    pub time: f64,
    pub from: usize,
    pub to: usize,
    /// The active chart's and the target's conditioning at the request.
    pub sigma_active: f64,
    pub sigma_target: f64,
}

/// The states after every accepted step (and any event at it) and the chart
/// switches requested along the way.
#[derive(Clone, Debug, Default)]
pub struct FixedStepRun {
    pub times: Vec<f64>,
    pub states: Vec<Vec<f64>>,
    pub switches: Vec<FixedStepChartSwitch>,
}

/// Drive `artifact` from time zero to `stop` in steps of `step`, with the
/// component's default refresh tolerance as the FMI tolerance.
pub fn fixed_step_rk4(
    artifact: &MeModelArtifact,
    step: f64,
    stop: f64,
) -> Result<FixedStepRun, MeError> {
    let tolerance = rumoca_eval_solve::projection_policy::ALGEBRAIC_REFRESH_TOLERANCE;
    let config = MeInstanceConfig::new("fixed-step", tolerance, 0.0, stop)?;
    let mut kernel = SolveMeKernel::instantiate(artifact.source(), &config)?;
    kernel.enter_initialization_mode()?;
    kernel.exit_initialization_mode()?;
    settle_events(&mut kernel)?;
    kernel.enter_continuous_time_mode()?;
    let mut states = vec![0.0; artifact.continuous_state_count()];
    kernel.get_continuous_states(&mut states)?;
    let steps = (stop / step).round() as usize;
    let mut run = FixedStepRun::default();
    for index in 1..=steps {
        let time = if index == steps {
            stop
        } else {
            index as f64 * step
        };
        advance_rk4(&mut kernel, &mut states, (index - 1) as f64 * step, step)?;
        kernel.set_time(MeTime::at(time))?;
        kernel.set_continuous_states(&states)?;
        let completed = kernel.completed_integrator_step(true)?;
        if completed.enter_event_mode {
            if let Some((from, to, sigma_active, sigma_target)) = kernel.requested_chart_switch() {
                run.switches.push(FixedStepChartSwitch {
                    step: index,
                    time,
                    from,
                    to,
                    sigma_active,
                    sigma_target,
                });
            }
            kernel.enter_event_mode(MeEventEntry {
                cause: MeEventCause::BasisChange,
                event_time: time,
                horizon: stop,
            })?;
            settle_events(&mut kernel)?;
            kernel.enter_continuous_time_mode()?;
            kernel.get_continuous_states(&mut states)?;
        }
        run.times.push(time);
        run.states.push(states.clone());
    }
    Ok(run)
}

fn settle_events(kernel: &mut SolveMeKernel) -> Result<(), MeError> {
    while kernel.update_discrete_states()?.discrete_states_need_update {}
    Ok(())
}

/// One classical RK4 step of `states` from `start`, evaluated as a fixed-step
/// importer does: set the time and states, then read the derivatives.
fn advance_rk4(
    kernel: &mut SolveMeKernel,
    states: &mut [f64],
    start: f64,
    step: f64,
) -> Result<(), MeError> {
    let mut derivative = |time: f64, point: &[f64]| -> Result<Vec<f64>, MeError> {
        kernel.set_time(MeTime::at(time))?;
        kernel.set_continuous_states(point)?;
        let mut out = vec![0.0; point.len()];
        kernel.continuous_state_derivatives_into(&mut out)?;
        Ok(out)
    };
    let along = |slope: &[f64], fraction: f64| -> Vec<f64> {
        states
            .iter()
            .zip(slope)
            .map(|(value, rate)| value + fraction * step * rate)
            .collect()
    };
    let k1 = derivative(start, states)?;
    let k2 = derivative(start + 0.5 * step, &along(&k1, 0.5))?;
    let k3 = derivative(start + 0.5 * step, &along(&k2, 0.5))?;
    let k4 = derivative(start + step, &along(&k3, 1.0))?;
    for (index, value) in states.iter_mut().enumerate() {
        *value += step / 6.0 * (k1[index] + 2.0 * k2[index] + 2.0 * k3[index] + k4[index]);
    }
    Ok(())
}
