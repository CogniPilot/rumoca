use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model FiniteConductanceStartup
  parameter Real secondsPerUnit = 1;
  parameter Real voltage = 70;
  parameter Real conductance = 1e-5;
  parameter Real inductance = 0.0005;
  Real current(start=0, fixed=true);
  Real switchVoltage;
equation
  current = conductance*switchVoltage;
  inductance*der(current) = secondsPerUnit*(voltage - switchVoltage);
end FiniteConductanceStartup;
"#;

fn check_startup(solver_mode: SimSolverMode, seconds_per_unit: f64, origin: f64) {
    let source = SOURCE.replace(
        "secondsPerUnit = 1;",
        &format!("secondsPerUnit = {seconds_per_unit};"),
    );
    let dae = Compiler::new()
        .model("FiniteConductanceStartup")
        .compile_str(&source, "finite_conductance_startup.mo")
        .unwrap()
        .dae;
    let result = simulate_dae_with_diagnostics(
        &dae,
        &SimOptions {
            t_start: origin,
            t_end: origin + 1e-8 / seconds_per_unit,
            dt: Some(1e-10 / seconds_per_unit),
            rtol: 1e-6,
            atol: 1e-10,
            max_wall_seconds: Some(12.0),
            solver_mode,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{solver_mode:?}, seconds/unit={seconds_per_unit}: {error}"));
    for (name, scale) in [("switchVoltage", 70.0), ("current", 0.0007)] {
        let index = result.names.iter().position(|value| value == name).unwrap();
        assert_eq!(result.data[index][0], 0.0);
        for (&time, &actual) in result.times.iter().zip(&result.data[index]) {
            let expected = -scale * (-(time - origin) * seconds_per_unit / 5e-9).exp_m1();
            assert!(
                (actual - expected).abs() <= 1e-10 + 1e-6 * expected.abs(),
                "{solver_mode:?}, seconds/unit={seconds_per_unit}, {name} at {time}: {actual} != {expected}"
            );
        }
    }
}

#[test]
fn bdf_in_seconds() {
    check_startup(SimSolverMode::Bdf, 1.0, 0.0);
}

#[test]
fn bdf_in_microseconds() {
    check_startup(SimSolverMode::Bdf, 1e-6, 0.0);
}

#[test]
fn rk_in_seconds() {
    check_startup(SimSolverMode::RkLike, 1.0, 0.0);
}

#[test]
fn rk_in_microseconds() {
    check_startup(SimSolverMode::RkLike, 1e-6, 0.0);
}

#[test]
fn bdf_in_kiloseconds() {
    check_startup(SimSolverMode::Bdf, 1e3, 0.0);
}

#[test]
fn rk_in_kiloseconds() {
    check_startup(SimSolverMode::RkLike, 1e3, 0.0);
}

#[test]
fn both_plugins_with_positive_and_negative_time_origins() {
    for solver in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        for origin in [-10.0, 10.0] {
            check_startup(solver, 1e-6, origin);
        }
    }
}
