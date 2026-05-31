use std::{collections::HashMap, error::Error};

use rumoca_ir_dae as dae;

use crate::SimOptions;

pub trait InteractiveStepper: Sized {
    type Error: Error + Send + Sync + 'static;

    fn new_from_dae(dae_model: &dae::Dae, opts: SimOptions) -> Result<Self, Self::Error>;
    fn set_input(&mut self, name: &str, value: f64) -> Result<(), Self::Error>;
    fn step(&mut self, dt: f64) -> Result<(), Self::Error>;
    fn time(&self) -> f64;
    fn get(&self, name: &str) -> Option<f64>;
    fn input_names(&self) -> &[String];

    fn values_for(&self, _names: &[String]) -> Option<HashMap<String, f64>> {
        None
    }

    fn max_runner_step_dt(&self) -> Option<f64> {
        Some(0.002)
    }
}
