use std::error::Error;

use indexmap::IndexMap;
pub(crate) trait SimulationSessionApi {
    type Error: Error + Send + Sync + 'static;

    fn reset(&mut self, t_start: f64) -> Result<(), Self::Error>;
    fn set_input(&mut self, name: &str, value: f64) -> Result<(), Self::Error>;
    fn advance_to(&mut self, target_time: f64) -> Result<(), Self::Error>;
    fn time(&self) -> f64;
    fn get(&self, name: &str) -> Result<Option<f64>, Self::Error>;

    fn values_for(&self, _names: &[String]) -> Result<Option<IndexMap<String, f64>>, Self::Error> {
        Ok(None)
    }

    fn max_runner_advance_dt(&self) -> Option<f64> {
        Some(0.002)
    }
}
