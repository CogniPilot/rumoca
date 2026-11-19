//! A fixed-step classical RK4 drive of one component through the Model
//! Exchange calls a fixed-step importer makes: the four stage derivatives,
//! then the accepted point, the completed integrator step, and event
//! iteration at scheduled boundaries and requested chart switches. A
//! generated component driven by the same calls is compared with it step
//! for step, including chart switches (SPEC_0040 STRUCT-T07).

use crate::SimTermination;

use super::integrator::reached_event_time;
use super::{
    MeCompletedIntegratorStep, MeError, MeEventCause, MeEventEntry, MeInstanceConfig,
    MeModelArtifact, MeTime, SolveMeKernel,
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
    /// The model's termination request, when the final row is an event stop.
    pub termination: Option<SimTermination>,
}

struct FixedStepImporter {
    kernel: SolveMeKernel,
    states: Vec<f64>,
    time: f64,
    stop: f64,
    next_event_time: Option<f64>,
    run: FixedStepRun,
}

/// Drive `artifact` from time zero to `stop` in steps of `step`, with the
/// component's default refresh tolerance as the FMI tolerance. The drive is an
/// importer, so the component refreshes from its committed seed
/// (`RefreshExecutor::ImporterDriven`, SPEC_0044 ME-PROJ-005), as the
/// generated component it is compared with does.
pub fn fixed_step_rk4(
    artifact: &MeModelArtifact,
    step: f64,
    stop: f64,
) -> Result<FixedStepRun, MeError> {
    let tolerance = rumoca_eval_solve::projection_policy::ALGEBRAIC_REFRESH_TOLERANCE;
    let config = MeInstanceConfig::new("fixed-step", tolerance, 0.0, stop)?.importer_driven();
    let mut kernel = SolveMeKernel::instantiate(artifact.source(), &config)?;
    kernel.enter_initialization_mode()?;
    kernel.exit_initialization_mode()?;
    let mut importer = FixedStepImporter {
        kernel,
        states: vec![0.0; artifact.continuous_state_count()],
        time: 0.0,
        stop,
        next_event_time: None,
        run: FixedStepRun::default(),
    };
    importer.settle_events()?;
    let steps = (stop / step).round() as usize;
    for index in 1..=steps {
        if importer.run.termination.is_some() {
            break;
        }
        let time = if index == steps {
            stop
        } else {
            index as f64 * step
        };
        importer.advance_to_output(index, time, step)?;
    }
    Ok(importer.run)
}

impl FixedStepImporter {
    fn advance_to_output(&mut self, index: usize, time: f64, step: f64) -> Result<(), MeError> {
        let nominal_start = (index - 1) as f64 * step;
        let mut duration = if self.time.to_bits() == nominal_start.to_bits() {
            step
        } else {
            time - self.time
        };
        while let Some(event_time) = self
            .next_event_time
            .filter(|event| *event < time || reached_event_time(Some(*event), time).is_some())
        {
            self.advance_to_scheduled_boundary(event_time)?;
            self.settle_boundary(MeEventCause::TimeEvent)?;
            if self.run.termination.is_some() {
                return Ok(());
            }
            if self.next_event_time.is_some_and(|next| next <= event_time) {
                return Err(MeError::Contract {
                    reason: format!("scheduled event did not advance beyond {event_time}"),
                });
            }
            duration = if reached_event_time(Some(event_time), time).is_some() {
                0.0
            } else {
                time - self.time
            };
        }
        if duration > 0.0
            && self
                .complete_step(duration, MeTime::at(time))?
                .enter_event_mode
        {
            self.settle_step_event(index)?;
        }
        if self.run.termination.is_none() {
            self.record(time);
        }
        Ok(())
    }

    fn advance_to_scheduled_boundary(&mut self, event_time: f64) -> Result<(), MeError> {
        if event_time > self.time {
            self.complete_step(
                event_time - self.time,
                MeTime::new(event_time, Some(event_time)),
            )?;
            return Ok(());
        }
        if reached_event_time(Some(event_time), self.time).is_some() {
            self.kernel.set_time(MeTime::at(event_time))?;
            self.time = event_time;
            return Ok(());
        }
        Err(MeError::Contract {
            reason: format!(
                "scheduled event at {event_time} precedes accepted time {}",
                self.time
            ),
        })
    }

    fn complete_step(
        &mut self,
        duration: f64,
        end: MeTime,
    ) -> Result<MeCompletedIntegratorStep, MeError> {
        advance_rk4(
            &mut self.kernel,
            &mut self.states,
            self.time,
            duration,
            end.event_boundary,
        )?;
        self.kernel.set_time(MeTime::at(end.time))?;
        self.kernel.set_continuous_states(&self.states)?;
        let completed = self.kernel.completed_integrator_step(true)?;
        self.time = end.time;
        Ok(completed)
    }

    fn settle_step_event(&mut self, index: usize) -> Result<(), MeError> {
        if let Some((from, to, sigma_active, sigma_target)) = self.kernel.requested_chart_switch() {
            self.run.switches.push(FixedStepChartSwitch {
                step: index,
                time: self.time,
                from,
                to,
                sigma_active,
                sigma_target,
            });
        }
        self.settle_boundary(MeEventCause::BasisChange)
    }

    fn settle_boundary(&mut self, cause: MeEventCause) -> Result<(), MeError> {
        self.kernel.enter_event_mode(MeEventEntry {
            cause,
            event_time: self.time,
            horizon: self.stop,
        })?;
        self.settle_events()
    }

    fn settle_events(&mut self) -> Result<(), MeError> {
        let discrete = loop {
            let discrete = self.kernel.update_discrete_states()?;
            if discrete.terminate_simulation.is_some() || !discrete.discrete_states_need_update {
                break discrete;
            }
        };
        if let Some(termination) = discrete.terminate_simulation {
            self.kernel.get_continuous_states(&mut self.states)?;
            self.kernel.terminate()?;
            self.record(self.time);
            self.run.termination = Some(termination);
            return Ok(());
        }
        self.next_event_time = discrete.next_event_time;
        self.kernel.enter_continuous_time_mode()?;
        self.kernel.get_continuous_states(&mut self.states)
    }

    fn record(&mut self, time: f64) {
        self.run.times.push(time);
        self.run.states.push(self.states.clone());
    }
}

/// One classical RK4 step of `states` from `start`, evaluated as a fixed-step
/// importer does: set the time and states, then read the derivatives.
fn advance_rk4(
    kernel: &mut SolveMeKernel,
    states: &mut [f64],
    start: f64,
    step: f64,
    event_boundary: Option<f64>,
) -> Result<(), MeError> {
    let mut derivative = |time: f64, point: &[f64]| -> Result<Vec<f64>, MeError> {
        kernel.set_time(MeTime::new(time, event_boundary))?;
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
    let k4 = derivative(event_boundary.unwrap_or(start + step), &along(&k3, 1.0))?;
    for (index, value) in states.iter_mut().enumerate() {
        *value += step / 6.0 * (k1[index] + 2.0 * k2[index] + 2.0 * k3[index] + k4[index]);
    }
    Ok(())
}
