use std::error::Error;

use indexmap::IndexMap;
pub(crate) trait SimulationSessionApi {
    type Error: Error + Send + Sync + 'static;

    fn retime(&mut self, t_start: f64) -> Result<(), Self::Error>;
    fn set_input(&mut self, name: &str, value: f64) -> Result<(), Self::Error>;
    fn advance_to(&mut self, target_time: f64) -> Result<(), Self::Error>;
    fn time(&self) -> f64;

    fn values_for(&self, names: &[String]) -> Result<IndexMap<String, f64>, Self::Error>;
    fn max_schedule_advance_dt(&self) -> Option<f64>;
}
