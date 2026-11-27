//! One definition of a model's OMC-over-Rumoca runtime ratio.
//!
//! The OMC reference's runtime statistics, the shard merge's statistics and
//! the quality gate's per-model runtime evidence all count "the models both
//! tools timed". They must count the same models, or a snapshot's recorded
//! `sample_count` and medians disagree with its own per-model ratios, which
//! the gate and the baseline ratchet then refuse. So every one of them asks
//! [`model_runtime_ratio`].
//!
//! A model has a runtime ratio when both tools simulated it successfully and
//! every timing the two ratios need is a finite positive number, as are both
//! ratios. Rumoca's simulation time is its run phase, or its whole simulation
//! time when the run phase was not measured or rounds to zero. Ratios are
//! OMC over Rumoca, so a value above one means Rumoca is faster.

use serde_json::Value;

/// The timings one model's runtime ratio is read from.
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct RuntimeSeconds {
    /// Both OMC and Rumoca simulated the model successfully.
    pub both_succeeded: bool,
    pub rumoca_sim_run_seconds: Option<f64>,
    pub rumoca_sim_seconds: Option<f64>,
    pub rumoca_sim_wall_seconds: Option<f64>,
    pub omc_sim_system_seconds: Option<f64>,
    pub omc_wall_seconds: Option<f64>,
}

impl RuntimeSeconds {
    /// Read from one model's entry of an OMC simulation reference payload.
    pub fn from_model_json(model: &Value) -> Self {
        let number = |key: &str| model.get(key).and_then(Value::as_f64);
        let text = |key: &str| model.get(key).and_then(Value::as_str);
        Self {
            both_succeeded: text("status") == Some("success")
                && text("rumoca_status") == Some("sim_ok"),
            rumoca_sim_run_seconds: number("rumoca_sim_run_seconds"),
            rumoca_sim_seconds: number("rumoca_sim_seconds"),
            rumoca_sim_wall_seconds: number("rumoca_sim_wall_seconds"),
            omc_sim_system_seconds: number("sim_system_seconds"),
            omc_wall_seconds: number("omc_wall_seconds"),
        }
    }
}

/// A model's runtime ratios, with the timings they divide.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct RuntimeRatio {
    pub omc_system: f64,
    pub rumoca_system: f64,
    pub omc_wall: f64,
    pub rumoca_wall: f64,
}

impl RuntimeRatio {
    /// OMC simulation time over Rumoca's.
    pub fn system(&self) -> f64 {
        self.omc_system / self.rumoca_system
    }

    /// OMC wall time over Rumoca's.
    pub fn wall(&self) -> f64 {
        self.omc_wall / self.rumoca_wall
    }
}

fn positive(value: Option<f64>) -> Option<f64> {
    value.filter(|value| value.is_finite() && *value > 0.0)
}

/// The model's runtime ratio, or `None` when it is not one both tools timed;
/// see the module docs.
pub fn model_runtime_ratio(seconds: &RuntimeSeconds) -> Option<RuntimeRatio> {
    if !seconds.both_succeeded {
        return None;
    }
    let ratio = RuntimeRatio {
        omc_system: positive(seconds.omc_sim_system_seconds)?,
        rumoca_system: positive(seconds.rumoca_sim_run_seconds)
            .or_else(|| positive(seconds.rumoca_sim_seconds))?,
        omc_wall: positive(seconds.omc_wall_seconds)?,
        rumoca_wall: positive(seconds.rumoca_sim_wall_seconds)?,
    };
    (ratio.system().is_finite() && ratio.wall().is_finite()).then_some(ratio)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn timed() -> RuntimeSeconds {
        RuntimeSeconds {
            both_succeeded: true,
            rumoca_sim_run_seconds: Some(0.5),
            rumoca_sim_seconds: Some(0.8),
            rumoca_sim_wall_seconds: Some(1.0),
            omc_sim_system_seconds: Some(2.0),
            omc_wall_seconds: Some(4.0),
        }
    }

    #[test]
    fn a_model_both_tools_timed_has_both_ratios() {
        let ratio = model_runtime_ratio(&timed()).unwrap();
        assert_eq!((ratio.system(), ratio.wall()), (4.0, 4.0));
    }

    #[test]
    fn a_run_phase_that_rounds_to_zero_falls_back_to_the_simulation_time() {
        let seconds = RuntimeSeconds {
            rumoca_sim_run_seconds: Some(0.0),
            ..timed()
        };
        assert_eq!(model_runtime_ratio(&seconds).unwrap().system(), 2.5);
    }

    #[test]
    fn a_model_missing_either_ratio_or_a_success_has_none() {
        for seconds in [
            RuntimeSeconds {
                both_succeeded: false,
                ..timed()
            },
            RuntimeSeconds {
                omc_wall_seconds: None,
                ..timed()
            },
            RuntimeSeconds {
                rumoca_sim_wall_seconds: Some(0.0),
                ..timed()
            },
            RuntimeSeconds {
                omc_sim_system_seconds: Some(f64::NAN),
                ..timed()
            },
            RuntimeSeconds {
                rumoca_sim_run_seconds: Some(f64::MIN_POSITIVE),
                omc_sim_system_seconds: Some(f64::MAX),
                ..timed()
            },
        ] {
            assert_eq!(model_runtime_ratio(&seconds), None, "{seconds:?}");
        }
    }
}
