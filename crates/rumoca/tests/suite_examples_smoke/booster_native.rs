//! The unchanged authored Booster scenario through the native session owner.
use rumoca::Compiler;
use rumoca_compile::scenario::parse_scenario_config_file;
use rumoca_sim::{SimOptions, SimSolverMode, SimulationSession};
use rumoca_solver::SimExecutionEngine;

const CONFIG: &str =
    include_str!("../../../../examples/interactive/reusable_booster/rumoca-scenario.toml");

#[test]
fn original_booster_scenario_initializes_and_advances_the_authored_grid() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let directory = root.join("examples/interactive/reusable_booster");
    let config = parse_scenario_config_file(CONFIG).unwrap();
    assert_eq!(config.sim.solver.as_deref(), Some("rk-like"));
    let dt = config.sim.dt.expect("authored interval");
    let mut compiler = Compiler::new().model(config.model.name.as_deref().unwrap());
    for path in &config.source_roots {
        let path = directory.join(path);
        assert!(
            path.is_dir(),
            "run cargo xtask repo modelica-deps ensure: {}",
            path.display()
        );
        compiler = compiler.source_root(path.to_str().unwrap());
    }
    let compiled = compiler
        .compile_path(&directory.join(config.model.file.as_deref().unwrap()))
        .unwrap();
    let options = SimOptions {
        solver_mode: SimSolverMode::RkLike,
        dt: Some(dt),
        atol: config.sim.atol.unwrap(),
        rtol: config.sim.rtol.unwrap(),
        param_overrides: config.parameters.into_iter().collect(),
        initial_inputs: authored_initial_inputs(),
        ..Default::default()
    };
    let mut session = SimulationSession::new(&compiled.dae, options)
        .expect("original Booster native initialization");
    let mut previous_time = None;
    for step in 0..=7 {
        let time = step as f64 * dt;
        if step != 0 {
            session
                .advance_to(time)
                .expect("original Booster native advance");
        }
        let state = session.state().unwrap();
        assert!(state.time.is_finite());
        assert!(
            (state.time - time).abs() <= rumoca_solver::fmi_me::accepted_step_roundoff(time, 0.0),
            "requested t={time:.17}, observed t={:.17}",
            state.time
        );
        assert!(previous_time.is_none_or(|previous| state.time > previous));
        previous_time = Some(state.time);
        assert!(!state.values.is_empty());
        assert!(state.values.values().all(|value| value.is_finite()));
        let receipt = session.execution_receipt();
        assert_eq!(receipt.engine, SimExecutionEngine::Cranelift, "{receipt:?}");
        assert_eq!(receipt.refusal, None, "{receipt:?}");
        assert!(receipt.declined.is_empty(), "{receipt:?}");
        eprintln!(
            "[Booster native] t={time}, values={}, {receipt:?}",
            state.values.len()
        );
    }
}

/// Observe the authored local-to-input routes; no model expression is evaluated.
fn authored_initial_inputs() -> Vec<(String, f64)> {
    let config: toml::Value = toml::from_str(CONFIG).unwrap();
    config["signals"]["model_inputs"]
        .as_table()
        .unwrap()
        .iter()
        .map(|(input, route)| {
            let name = route
                .as_str()
                .or_else(|| route.get("from").and_then(toml::Value::as_str))
                .unwrap();
            let local = name
                .strip_prefix("local:")
                .expect("authored routes use locals before initialization");
            let value = &config["locals"][local]["default"];
            let value = if let Some(boolean) = value.as_bool() {
                route[if boolean { "when_true" } else { "when_false" }]
                    .as_integer()
                    .unwrap() as f64
            } else {
                value
                    .as_float()
                    .unwrap_or_else(|| value.as_integer().unwrap() as f64)
            };
            (input.clone(), value)
        })
        .collect()
}
