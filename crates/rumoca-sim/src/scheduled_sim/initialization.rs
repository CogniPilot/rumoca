//! Route host-owned values before the component solves its initial equations.

use anyhow::{Context, Result, bail};
use rumoca_input::{InputEngine, RuntimeContext, SignalMapper};

use crate::scenario_config::SimulationConfig;

pub(super) fn initial_inputs(cfg: &SimulationConfig) -> Result<Vec<(String, f64)>> {
    let input_cfg = cfg
        .input
        .as_ref()
        .context("Config missing [input] section")?;
    let signals = cfg
        .signals
        .as_ref()
        .context("Config missing [signals] section")?;
    let mut engine = InputEngine::new(input_cfg, &cfg.locals, &cfg.derive)?;
    engine.poll_idle();
    let mapper = SignalMapper::new(signals, &cfg.locals)?;
    let model_get = |name: &str| {
        bail!(
            "Initial input route requires model '{name}' before initialization; use a local or constant source"
        )
    };
    let runtime = RuntimeContext {
        frame_num: 0,
        wall_ms: std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .context("read initial wall clock time")?
            .as_millis() as f64,
        input_connected: false,
        input_mode: engine.mode(),
        input_message: engine.last_message(),
        model_time: 0.0,
        model_get: &model_get,
    };
    mapper.build_model_inputs(&engine, &runtime)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fixedwing_routes_supply_every_control_before_initialization() {
        let cfg: SimulationConfig = toml::from_str(include_str!(
            "../../../../examples/interactive/fixedwing/rumoca-scenario.toml"
        ))
        .expect("the shipped FixedWing scenario parses");
        assert_eq!(
            initial_inputs(&cfg).unwrap(),
            [
                "armed",
                "stick_pitch",
                "stick_roll",
                "stick_throttle",
                "stick_yaw"
            ]
            .map(|name| (name.to_owned(), 0.0))
        );
    }
}
