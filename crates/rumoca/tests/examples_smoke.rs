// SPEC_0021 file-size exception: example smoke coverage spans CLI, template,
// and runtime examples. split plan: move smoke cases by target/runtime family
// into focused integration test modules.

use std::path::{Path, PathBuf};
use std::{env, fs};

#[path = "examples_smoke/solve_tensor_smoke_tests.rs"]
mod solve_tensor_smoke_tests;

use rumoca::Compiler;
#[cfg(feature = "runner")]
use rumoca_sim::{SimOptions, SimPacingMode, SimSolverMode, SimulationSession};
use solve_tensor_smoke_tests::cached_cmm_root;
use tempfile::tempdir;

fn example_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../examples")
}

fn compile_ball_example() -> rumoca::CompilationResult {
    let model_path = example_root().join("models/Ball.mo");
    assert!(
        model_path.is_file(),
        "expected example model at {}",
        model_path.display()
    );

    Compiler::new()
        .model("Ball")
        .compile_file(
            model_path
                .to_str()
                .expect("example path should be utf8 for this test"),
        )
        .expect("Ball example should compile")
}

fn compile_circuit_example() -> rumoca::CompilationResult {
    let model_path = example_root().join("models/Circuit.mo");
    assert!(
        model_path.is_file(),
        "expected example model at {}",
        model_path.display()
    );

    Compiler::new()
        .model("Circuit.Test")
        .compile_file(
            model_path
                .to_str()
                .expect("example path should be utf8 for this test"),
        )
        .expect("Circuit example should compile")
}

#[derive(serde::Deserialize)]
struct ExampleTomlConfig {
    #[serde(default)]
    rumoca: ExampleRumocaMarker,
    #[serde(default)]
    source_roots: Vec<String>,
    model: ExampleTomlModel,
    #[serde(default)]
    sim: ExampleTomlSim,
    #[serde(default)]
    codegen: Option<ExampleTomlCodegen>,
}

#[derive(Default, serde::Deserialize)]
struct ExampleRumocaMarker {
    #[serde(default)]
    task: Option<String>,
}

#[derive(serde::Deserialize)]
struct ExampleTomlModel {
    file: String,
    name: String,
}

#[derive(Default, serde::Deserialize)]
struct ExampleTomlSim {
    dt: Option<f64>,
    solver: Option<String>,
}

#[derive(serde::Deserialize)]
struct ExampleTomlCodegen {
    target: String,
    output_dir: String,
}

fn compile_quadrotor_acro_if_cmm_available() -> Option<rumoca::CompilationResult> {
    let cmm_root = cached_cmm_root()?;
    let model_path = example_root().join("interactive/quadrotor/QuadrotorSIL.mo");
    Some(
        Compiler::new()
            .source_root(cmm_root.to_string_lossy().as_ref())
            .model("QuadrotorAcro")
            .compile_file(
                model_path
                    .to_str()
                    .expect("quadrotor example path should be utf8"),
            )
            .expect("QuadrotorAcro should compile with cached CMM"),
    )
}

#[cfg(feature = "runner")]
fn compile_quadrotor_acro_config_if_cmm_available() -> Option<rumoca::CompilationResult> {
    let config_path = example_root().join("interactive/quadrotor/rumoca-scenario.acro.toml");
    compile_toml_model_if_source_roots_exist(&config_path)
}

fn collect_examples_scenario_files(out: &mut Vec<PathBuf>) {
    let config_root = example_root();
    collect_scenario_files(&config_root, out);
}

fn collect_scenario_files(root: &Path, out: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(root).expect("example directory should be readable") {
        let entry = entry.expect("example directory entry should be readable");
        let path = entry.path();
        if path.is_dir() {
            collect_scenario_files(&path, out);
        } else if is_scenario_file(&path) {
            out.push(path);
        }
    }
}

fn is_scenario_file(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .is_some_and(rumoca_compile::scenario::is_rumoca_task_filename)
}

fn load_example_toml_config(config_path: &Path) -> ExampleTomlConfig {
    let text = fs::read_to_string(config_path).unwrap_or_else(|error| {
        panic!(
            "example TOML {} should be readable: {error}",
            config_path.display()
        )
    });
    toml::from_str(&text).unwrap_or_else(|error| {
        panic!(
            "example TOML {} should parse: {error}",
            config_path.display()
        )
    })
}

fn resolve_config_path(config_dir: &Path, raw: &str) -> PathBuf {
    let path = Path::new(raw);
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        config_dir.join(path)
    }
}

fn push_unique_path(paths: &mut Vec<PathBuf>, path: PathBuf) {
    let key = path
        .canonicalize()
        .unwrap_or_else(|_| path.clone())
        .to_string_lossy()
        .to_string();
    let already_seen = paths.iter().any(|existing| {
        existing
            .canonicalize()
            .unwrap_or_else(|_| existing.clone())
            .to_string_lossy()
            == key
    });
    if !already_seen {
        paths.push(path);
    }
}

fn modelicapath_source_roots() -> Vec<PathBuf> {
    env::var_os("MODELICAPATH")
        .map(|raw| env::split_paths(&raw).collect::<Vec<_>>())
        .into_iter()
        .flatten()
        .filter(|path| !path.as_os_str().is_empty())
        .collect()
}

fn configured_source_roots(config_path: &Path, config: &ExampleTomlConfig) -> Option<Vec<PathBuf>> {
    let config_dir = config_path
        .parent()
        .expect("example TOML should have a parent directory");
    let mut paths = Vec::new();
    for source_root in &config.source_roots {
        let path = resolve_config_path(config_dir, source_root);
        if !path.exists() {
            eprintln!(
                "skipping {}: missing configured source root {}",
                config_path.display(),
                path.display()
            );
            return None;
        }
        push_unique_path(&mut paths, path);
    }
    for path in modelicapath_source_roots() {
        if path.exists() {
            push_unique_path(&mut paths, path);
        }
    }
    Some(paths)
}

fn compile_toml_model_if_source_roots_exist(
    config_path: &Path,
) -> Option<rumoca::CompilationResult> {
    let config = load_example_toml_config(config_path);
    let source_roots = configured_source_roots(config_path, &config)?;
    let config_dir = config_path
        .parent()
        .expect("example TOML should have a parent directory");
    let model_path = resolve_config_path(config_dir, &config.model.file);
    assert!(
        model_path.is_file(),
        "example TOML {} should point at a model file: {}",
        config_path.display(),
        model_path.display()
    );

    let mut compiler = Compiler::new().model(&config.model.name);
    for source_root in &source_roots {
        compiler = compiler.source_root(
            source_root
                .to_str()
                .expect("example TOML source-root path should be utf8"),
        );
    }
    Some(
        compiler
            .compile_file(
                model_path
                    .to_str()
                    .expect("example TOML model path should be utf8"),
            )
            .unwrap_or_else(|error| {
                panic!(
                    "example TOML {} should compile {}: {error}",
                    config_path.display(),
                    config.model.name
                )
            }),
    )
}

fn assert_codegen_config_renders(
    config_path: &Path,
    config: &ExampleTomlConfig,
    result: &rumoca::CompilationResult,
) {
    let Some(codegen) = config.codegen.as_ref() else {
        panic!(
            "codegen TOML {} should declare [codegen].target",
            config_path.display()
        );
    };
    assert!(
        Path::new(&codegen.output_dir)
            .components()
            .next()
            .is_some_and(|component| component.as_os_str() == "gen"),
        "codegen TOML {} should write under examples/codegen/gen, got {}",
        config_path.display(),
        codegen.output_dir
    );
    if assert_raw_template_config_renders(config_path, config, result, &codegen.target) {
        return;
    }
    let target = renderable_codegen_target(config_path, &codegen.target);
    let rendered_files = rumoca::render_target_files(
        result,
        &config.model.name,
        target.to_str().expect("example target path should be utf8"),
        None,
    )
    .unwrap_or_else(|error| {
        panic!(
            "example TOML {} should render target {}: {error}",
            config_path.display(),
            codegen.target
        )
    });
    assert!(
        !rendered_files.is_empty(),
        "example TOML {} rendered no files for target {}",
        config_path.display(),
        codegen.target
    );
    for rendered in rendered_files {
        assert!(
            !rendered.content.trim().is_empty(),
            "example TOML {} rendered empty {} output",
            config_path.display(),
            rendered.path
        );
    }
}

fn assert_raw_template_config_renders(
    config_path: &Path,
    config: &ExampleTomlConfig,
    result: &rumoca::CompilationResult,
    target: &str,
) -> bool {
    let config_dir = config_path
        .parent()
        .expect("example TOML should have a parent directory");
    let target_path = resolve_config_path(config_dir, target);
    if target_path.extension().and_then(|ext| ext.to_str()) != Some("jinja") {
        return false;
    }
    let rendered = result
        .render_template(
            target_path
                .to_str()
                .expect("raw example template path should be utf8"),
        )
        .unwrap_or_else(|error| {
            panic!(
                "example TOML {} should render raw template {} for {}: {error}",
                config_path.display(),
                target_path.display(),
                config.model.name
            )
        });
    assert!(
        !rendered.trim().is_empty(),
        "example TOML {} rendered empty raw template {}",
        config_path.display(),
        target_path.display()
    );
    true
}

fn renderable_codegen_target(config_path: &Path, target: &str) -> PathBuf {
    if rumoca_compile::codegen::targets::TargetBundle::builtin(target).is_some() {
        return PathBuf::from(target);
    }
    let config_dir = config_path
        .parent()
        .expect("example TOML should have a parent directory");
    resolve_config_path(config_dir, target)
}

#[cfg(feature = "runner")]
fn assert_sim_config_creates_session(
    config_path: &Path,
    config: &ExampleTomlConfig,
    result: &rumoca::CompilationResult,
) {
    let (solver_mode, _) = SimSolverMode::parse_request(config.sim.solver.as_deref());
    let dt = config.sim.dt.unwrap_or(0.004);
    let mut session = SimulationSession::new_with_diagnostics(
        &result.dae,
        SimOptions {
            rtol: 1e-3,
            atol: 1e-3,
            dt: Some(dt),
            solver_mode,
            pacing_mode: SimPacingMode::AsFastAsPossible,
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| {
        panic!(
            "example TOML {} should create a simulation session: {error}",
            config_path.display()
        )
    });
    for input_name in session.input_names().to_vec() {
        session.set_input(&input_name, 0.0).unwrap_or_else(|error| {
            panic!(
                "example TOML {} should bind input {input_name}: {error}",
                config_path.display()
            )
        });
    }
    session
        .advance_to(session.time() + dt)
        .unwrap_or_else(|error| {
            panic!(
                "example TOML {} session should advance one frame: {error}",
                config_path.display()
            )
        });
    assert!(
        session.time() > 0.0,
        "example TOML {} session should advance time",
        config_path.display()
    );
}

fn assert_toml_config_task_smoke(
    config_path: &Path,
    config: &ExampleTomlConfig,
    result: &rumoca::CompilationResult,
) {
    match config.rumoca.task.as_deref().unwrap_or("simulate") {
        "codegen" => assert_codegen_config_renders(config_path, config, result),
        #[cfg(feature = "runner")]
        "simulate" => assert_sim_config_creates_session(config_path, config, result),
        #[cfg(not(feature = "runner"))]
        "simulate" => {
            let _ = (config_path, config, result);
        }
        other => panic!(
            "example TOML {} uses unsupported task {other}",
            config_path.display()
        ),
    }
}

#[test]
fn examples_directory_scenario_configs_compile_and_smoke_requested_task() {
    let mut configs = Vec::new();
    collect_examples_scenario_files(&mut configs);
    configs.sort();

    assert!(
        !configs.is_empty(),
        "examples directory should contain rumoca-scenario.toml scenarios to smoke-test"
    );
    for config_path in configs {
        let config = load_example_toml_config(&config_path);
        let Some(result) = compile_toml_model_if_source_roots_exist(&config_path) else {
            continue;
        };
        assert_toml_config_task_smoke(&config_path, &config, &result);
    }
}

#[test]
fn basic_usage_flow_compiles_and_serializes_json() {
    let source = r#"
model Integrator
    Real x(start=0.0);
equation
    der(x) = 1.0;
end Integrator;
"#;

    let result = Compiler::new()
        .model("Integrator")
        .compile_str(source, "Integrator.mo")
        .expect("basic usage example compile should succeed");

    assert_eq!(result.dae.variables.states.len(), 1);
    assert!(!result.dae.continuous.equations.is_empty());
    let json = result.to_json().expect("json serialization should succeed");
    assert!(json.contains("\"f_x\""));
}

#[test]
fn file_compilation_flow_compiles_from_disk() {
    let dir = tempdir().expect("tempdir should be creatable");
    let model_file = dir.path().join("file_example.mo");
    fs::write(
        &model_file,
        r#"
model FileExample
    Real x(start=0.0);
equation
    der(x) = 2.0;
end FileExample;
"#,
    )
    .expect("model file should be writable");

    let result = Compiler::new()
        .model("FileExample")
        .compile_file(
            model_file
                .to_str()
                .expect("temp model path should be utf8 for this test"),
        )
        .expect("file compilation example should compile");

    assert_eq!(result.dae.variables.states.len(), 1);
    assert_eq!(result.dae.continuous.equations.len(), 1);
}

#[cfg(feature = "runner")]
#[test]
fn rover_config_steering_input_changes_delta_and_heading() {
    let config_path = example_root().join("interactive/rover/rumoca-scenario.toml");
    let result = compile_toml_model_if_source_roots_exist(&config_path)
        .expect("rover rumoca-scenario.toml should compile without extra source roots");
    let mut session = SimulationSession::new_with_diagnostics(
        &result.dae,
        SimOptions {
            rtol: 1e-5,
            atol: 1e-7,
            dt: Some(0.01),
            solver_mode: SimSolverMode::RkLike,
            pacing_mode: SimPacingMode::AsFastAsPossible,
            ..Default::default()
        },
    )
    .expect("rover rumoca-scenario.toml should create an RK-like simulation session");

    assert!(
        session.input_names().iter().any(|name| name == "steering"),
        "rover solve input layout should expose steering"
    );
    session
        .set_inputs(&[("throttle", 1.0), ("steering", 1.0)])
        .expect("rover should accept throttle and steering inputs");
    for _ in 0..100 {
        session
            .advance_to(session.time() + 0.01)
            .expect("rover should advance");
    }

    let state = session.state().expect("rover state read should succeed");
    let delta = state_value(&state.values, "delta");
    let theta = state_value(&state.values, "theta");
    assert!(
        delta > 0.3,
        "full steering should move the steering state; delta={delta}"
    );
    assert!(
        theta > 0.1,
        "full steering while moving should change heading; theta={theta}"
    );
}

#[cfg(feature = "runner")]
#[test]
fn quadrotor_acro_config_creates_rk_session_when_cmm_available() {
    let Some(result) = compile_quadrotor_acro_config_if_cmm_available() else {
        eprintln!(
            "skipping QuadrotorAcro config runtime regression: requires cached CMM at \
             target/cmm/CMM-v0.0.2; run `rum repo cmm ensure`"
        );
        return;
    };

    let mut session = SimulationSession::new_with_diagnostics(
        &result.dae,
        SimOptions {
            rtol: 1e-3,
            atol: 1e-3,
            dt: Some(0.01),
            solver_mode: SimSolverMode::RkLike,
            pacing_mode: SimPacingMode::AsFastAsPossible,
            ..Default::default()
        },
    )
    .expect("rumoca-scenario.acro.toml should create an RK-like simulation session");
    session
        .set_inputs(&[
            ("stick_roll", 0.0),
            ("stick_pitch", 0.0),
            ("stick_yaw", 0.0),
            ("stick_throttle", 0.0),
            ("armed", 0.0),
        ])
        .expect("rumoca-scenario.acro.toml inputs should bind to the session");
    session
        .advance_to(session.time() + 0.01)
        .expect("rumoca-scenario.acro.toml session should advance one frame");

    assert!(session.time() > 0.0);
}

#[cfg(feature = "runner")]
#[test]
fn quadrotor_acro_roll_command_generates_body_rate_when_cmm_available() {
    let Some(result) = compile_quadrotor_acro_config_if_cmm_available() else {
        eprintln!(
            "skipping QuadrotorAcro roll response regression: requires cached CMM at \
             target/cmm/CMM-v0.0.2; run `rum repo cmm ensure`"
        );
        return;
    };

    for (axis_input, gyro_output) in [
        ("stick_roll", "gyro[1]"),
        ("stick_pitch", "gyro[2]"),
        ("stick_yaw", "gyro[3]"),
    ] {
        let mut session = SimulationSession::new_with_diagnostics(
            &result.dae,
            SimOptions {
                rtol: 1e-3,
                atol: 1e-3,
                dt: Some(0.01),
                solver_mode: SimSolverMode::RkLike,
                pacing_mode: SimPacingMode::AsFastAsPossible,
                ..Default::default()
            },
        )
        .expect("rumoca-scenario.acro.toml should create an RK-like simulation session");
        session
            .set_inputs(&[
                ("stick_roll", 0.0),
                ("stick_pitch", 0.0),
                ("stick_yaw", 0.0),
                ("stick_throttle", 0.65),
                ("armed", 1.0),
            ])
            .expect("rumoca-scenario.acro.toml inputs should bind to the session");
        session
            .set_input(axis_input, 1.0)
            .expect("rumoca-scenario.acro.toml axis input should bind to the session");
        for _ in 0..20 {
            session
                .advance_to(session.time() + 0.01)
                .expect("rumoca-scenario.acro.toml session should advance under axis command");
        }

        let state = session
            .state()
            .expect("quadrotor state read should succeed");
        let rate = state
            .values
            .get(gyro_output)
            .copied()
            .unwrap_or_else(|| panic!("quadrotor state should contain {gyro_output}"));
        assert!(
            rate.abs() > 0.05,
            "{axis_input} should generate visible body rate at hover throttle; \
             {gyro_output}={rate}"
        );
    }
}

#[cfg(feature = "runner")]
#[test]
fn quadrotor_acro_roll_command_changes_attitude_when_cmm_available() {
    let Some(result) = compile_quadrotor_acro_config_if_cmm_available() else {
        eprintln!(
            "skipping QuadrotorAcro attitude regression: requires cached CMM at \
             target/cmm/CMM-v0.0.2; run `rum repo cmm ensure`"
        );
        return;
    };

    let sim_options = SimOptions {
        rtol: 1e-3,
        atol: 1e-3,
        dt: Some(0.01),
        solver_mode: SimSolverMode::RkLike,
        pacing_mode: SimPacingMode::AsFastAsPossible,
        ..Default::default()
    };
    let sim_dae =
        rumoca_sim::structurally_lowered_dae_for_simulation_artifact(&result.dae, &sim_options)
            .expect("QuadrotorAcro simulation structural lowering should succeed");
    let sim_solve = rumoca_phase_solve::lower_dae_to_solve_model_owned(sim_dae)
        .expect("QuadrotorAcro simulation DAE should lower to SolveModel");
    let runtime = rumoca_eval_solve::SolveRuntime::new(&sim_solve)
        .expect("QuadrotorAcro SolveRuntime should prepare");
    let mut derivative_probe_state = sim_solve.initial_y[..sim_solve.state_scalar_count()].to_vec();
    for (name, value) in [
        ("vehicle.omega[1]", 1.0),
        ("vehicle.omega[2]", 0.0),
        ("vehicle.omega[3]", 0.0),
        ("vehicle.attitude.q[1]", 1.0),
        ("vehicle.attitude.q[2]", 0.0),
        ("vehicle.attitude.q[3]", 0.0),
        ("vehicle.attitude.q[4]", 0.0),
    ] {
        let index = solve_state_index(&sim_solve, name);
        derivative_probe_state[index] = value;
    }
    let derivative_probe = runtime
        .eval_state_derivatives(
            0.0,
            &derivative_probe_state,
            &sim_solve.parameters,
            1.0e-10,
            32,
        )
        .expect("QuadrotorAcro derivative probe should evaluate");
    let q2_dot = derivative_probe[solve_state_index(&sim_solve, "vehicle.attitude.q[2]")];
    assert!(
        q2_dot > 0.4,
        "unit roll rate at identity attitude should drive quaternion roll component; q2_dot={q2_dot}"
    );

    let mut session = SimulationSession::new_with_diagnostics(&result.dae, sim_options)
        .expect("rumoca-scenario.acro.toml should create an RK-like simulation session");

    session
        .set_inputs(&[
            ("stick_roll", 0.0),
            ("stick_pitch", 0.0),
            ("stick_yaw", 0.0),
            ("stick_throttle", 0.0),
            ("armed", 1.0),
        ])
        .expect("rumoca-scenario.acro.toml should arm while throttle is low");
    session
        .advance_to(session.time() + 0.01)
        .expect("rumoca-scenario.acro.toml should advance after arming");
    session
        .set_inputs(&[("stick_throttle", 1.0), ("stick_roll", 1.0)])
        .expect("rumoca-scenario.acro.toml should accept throttle and roll commands");

    for _ in 0..1_000 {
        session
            .advance_to(session.time() + 0.01)
            .expect("rumoca-scenario.acro.toml should advance under full roll command");
    }

    let state = session
        .state()
        .expect("quadrotor state read should succeed");
    let roll_degrees = roll_degrees_from_state(&state.values);
    assert!(
        roll_degrees.abs() > 10.0,
        "full-roll command should change attitude by more than 10 deg after 10 s; \
         roll={roll_degrees:.3} deg"
    );
}

#[cfg(feature = "runner")]
fn solve_state_index(model: &rumoca_ir_solve::SolveModel, name: &str) -> usize {
    model
        .problem
        .solve_layout
        .solver_maps
        .names
        .iter()
        .take(model.state_scalar_count())
        .position(|state_name| state_name == name)
        .unwrap_or_else(|| panic!("quadrotor solve state should contain {name}"))
}

#[cfg(feature = "runner")]
fn roll_degrees_from_state(values: &indexmap::IndexMap<String, f64>) -> f64 {
    let q0 = state_value(values, "quat[1]");
    let q1 = state_value(values, "quat[2]");
    let q2 = state_value(values, "quat[3]");
    let q3 = state_value(values, "quat[4]");
    let sinr_cosp = 2.0 * (q0 * q1 + q2 * q3);
    let cosr_cosp = 1.0 - 2.0 * (q1 * q1 + q2 * q2);
    sinr_cosp.atan2(cosr_cosp).to_degrees()
}

#[cfg(feature = "runner")]
fn state_value(values: &indexmap::IndexMap<String, f64>, name: &str) -> f64 {
    values.get(name).copied().unwrap_or_else(|| {
        let mut keys = values.keys().cloned().collect::<Vec<_>>();
        keys.sort();
        panic!("quadrotor state should contain {name}; visible keys={keys:?}");
    })
}

#[test]
fn protected_flow_marks_protected_components_in_flat_ir() {
    let source = r#"
model ProtectedDemo
    parameter Real public_gain = 2;
protected
    parameter Real protected_gain = 3;
    Real hidden(start = 0);
equation
    hidden = public_gain + protected_gain;
end ProtectedDemo;
"#;

    let result = Compiler::new()
        .model("ProtectedDemo")
        .compile_str(source, "<protected_demo>")
        .expect("protected example should compile");

    let find_var = |name: &str| {
        result
            .flat
            .variables
            .iter()
            .find(|(var_name, _)| var_name.as_str() == name)
            .map(|(_, var)| var)
            .unwrap_or_else(|| panic!("variable '{name}' should exist"))
    };

    let public = find_var("public_gain");
    assert!(
        !public.is_protected,
        "public variable should not be marked protected"
    );

    let protected = find_var("protected_gain");
    assert!(
        protected.is_protected,
        "protected variable should be marked protected"
    );

    let hidden = find_var("hidden");
    assert!(
        hidden.is_protected,
        "variables declared in protected section should be marked protected"
    );
}

#[test]
fn ball_example_file_compiles_from_examples_directory() {
    let result = compile_ball_example();

    assert_eq!(result.dae.variables.states.len(), 2);
    assert_eq!(result.dae.continuous.equations.len(), 2);
}

#[test]
fn ball_example_rk_simulation_applies_reinit_bounce() {
    let result = compile_ball_example();
    let sim = rumoca_sim::simulate_dae_with_diagnostics(
        &result.dae,
        &rumoca_sim::SimOptions {
            t_end: 1.8,
            dt: Some(0.004),
            solver_mode: rumoca_sim::SimSolverMode::RkLike,
            ..Default::default()
        },
    )
    .expect("Ball example should simulate through the first bounce");
    let v_idx = sim
        .names
        .iter()
        .position(|name| name == "v")
        .expect("Ball velocity should be visible");
    let bounced = sim.data[v_idx].iter().any(|velocity| *velocity > 1.0);

    assert!(
        bounced,
        "Ball.mo reinit event should reverse velocity after the first ground contact"
    );
}

#[test]
fn ball_results_panel_path_applies_reinit_bounce() {
    let config_path = example_root().join("simulation/rumoca-scenario.ball.toml");
    let config = load_example_toml_config(&config_path);
    let result = compile_toml_model_if_source_roots_exist(&config_path)
        .expect("Ball simulation config should not require external source roots");
    let (solver_mode, _) = SimSolverMode::parse_request(config.sim.solver.as_deref());
    let sim = rumoca_sim::simulate_dae_with_diagnostics(
        &result.dae,
        &rumoca_sim::SimOptions {
            t_end: 1.8,
            dt: config.sim.dt.or(Some(0.004)),
            solver_mode,
            ..Default::default()
        },
    )
    .expect("Ball results-panel simulation path should simulate through the first bounce");
    let v_idx = sim
        .names
        .iter()
        .position(|name| name == "v")
        .expect("Ball velocity should be visible");
    let x_idx = sim
        .names
        .iter()
        .position(|name| name == "x")
        .expect("Ball height should be visible");
    let bounced = sim.data[v_idx].iter().any(|velocity| *velocity > 1.0);
    let final_x = sim.data[x_idx].last().copied().unwrap_or_default();

    assert!(
        bounced,
        "examples/simulation/rumoca-scenario.ball.toml should exercise the same RK-like \
         results-panel path and reverse velocity after ground contact"
    );
    assert!(
        final_x > 0.0,
        "Ball should be above the ground after the first bounce at t=1.8; x={final_x}"
    );
}

#[test]
fn circuit_example_simulates_rc_transient() {
    let result = compile_circuit_example();
    let sim = rumoca_sim::simulate_dae_with_diagnostics(
        &result.dae,
        &rumoca_sim::SimOptions {
            t_end: 0.2,
            dt: Some(0.005),
            solver_mode: rumoca_sim::SimSolverMode::RkLike,
            ..Default::default()
        },
    )
    .expect("Circuit example should simulate");
    let cap_v_idx = sim
        .names
        .iter()
        .position(|name| name == "cap.v")
        .expect("capacitor voltage should be visible");
    let cap_v = &sim.data[cap_v_idx];
    let value_at = |target: f64| {
        sim.times
            .iter()
            .zip(cap_v.iter())
            .find(|(time, _)| (**time - target).abs() < 1.0e-12)
            .map(|(_, value)| *value)
            .unwrap_or_else(|| panic!("expected output sample at t={target}"))
    };

    let initial = cap_v.first().copied().expect("simulation should have data");
    let one_tau = value_at(0.025);
    let final_v = cap_v.last().copied().expect("simulation should have data");
    let expected_one_tau = 5.0 * (1.0 - f64::exp(-1.0));

    assert!(
        initial.abs() < 1.0e-9,
        "RC capacitor should start discharged; cap.v(0)={initial}"
    );
    assert!(
        (one_tau - expected_one_tau).abs() < 0.08,
        "RC capacitor should follow the one-time-constant transient; \
         cap.v(0.025)={one_tau}, expected {expected_one_tau}"
    );
    assert!(
        final_v > 4.95,
        "RC capacitor should settle near source voltage; cap.v(0.2)={final_v}"
    );
}

/// Regression test for vector derivative simulation (GitHub issue: Vector Derivative Problems).
///
/// Verifies that `der(x) = {1,2}` where `x` is `Real[2]` compiles and simulates
/// without a mass-matrix isolation error.
#[test]
fn vector_derivative_compiles_and_simulates() {
    let source = r#"
model Simple
  Real[2] x;
equation
  der(x) = {1, 2};
end Simple;
"#;

    let result = Compiler::new()
        .model("Simple")
        .compile_str(source, "Simple.mo")
        .expect("vector derivative model should compile");

    assert_eq!(result.dae.variables.states.len(), 1, "one array state 'x'");

    let opts = rumoca_sim::SimOptions {
        t_end: 1.0,
        ..Default::default()
    };
    let sim = rumoca_sim::simulate_dae(&result.dae, &opts)
        .expect("vector derivative model should simulate without mass-matrix error");

    // After t=1, x[1] ≈ 1.0 and x[2] ≈ 2.0 (integrating constants from zero)
    let x1_idx = sim
        .names
        .iter()
        .position(|n| n == "x[1]")
        .expect("x[1] should be in simulation output");
    let x2_idx = sim
        .names
        .iter()
        .position(|n| n == "x[2]")
        .expect("x[2] should be in simulation output");
    let x1_final = sim.data[x1_idx].last().copied().unwrap();
    let x2_final = sim.data[x2_idx].last().copied().unwrap();
    assert!(
        (x1_final - 1.0).abs() < 0.01,
        "x[1] at t=1 should be ~1.0, got {x1_final}"
    );
    assert!(
        (x2_final - 2.0).abs() < 0.01,
        "x[2] at t=1 should be ~2.0, got {x2_final}"
    );
}
