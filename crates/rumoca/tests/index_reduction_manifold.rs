use rumoca::Compiler;
use rumoca_sim::{
    DiffsolMethod, SimOptions, SimSolverMode, SimulationSession, lower_dae_for_simulation,
    simulate_dae_with_diagnostics,
};

const CARTESIAN_PENDULUM: &str = r#"
model CartesianPendulum
  parameter Real length = 1.0;
  parameter Real gravity = 9.81;
  Real x(start = 1.0, fixed = true);
  Real y(start = 0.0, fixed = true);
  Real vx(start = 0.0, fixed = true);
  Real vy(start = 0.0, fixed = true);
  Real lambda;
equation
  der(x) = vx;
  der(y) = vy;
  der(vx) = -lambda*x;
  der(vy) = -gravity - lambda*y;
  x*x + y*y = length*length;
end CartesianPendulum;
"#;

fn trace<'a>(result: &'a rumoca_sim::SimResult, name: &str) -> &'a [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("missing `{name}` in {:?}", result.names));
    &result.data[index]
}

fn assert_manifold_trace(
    result: &rumoca_sim::SimResult,
    solver: SimSolverMode,
    method: DiffsolMethod,
    dt: f64,
) {
    let x = trace(result, "x");
    let y = trace(result, "y");
    let vx = trace(result, "vx");
    let vy = trace(result, "vy");
    let mut max_holonomic = 0.0_f64;
    let mut max_velocity = 0.0_f64;
    for (((x, y), vx), vy) in x.iter().zip(y).zip(vx).zip(vy) {
        max_holonomic = max_holonomic.max((x * x + y * y - 1.0).abs());
        max_velocity = max_velocity.max((x * vx + y * vy).abs());
    }
    assert!(
        max_holonomic <= 2.0e-8,
        "{solver:?}/{method:?} dt={dt} drifted off g=0: {max_holonomic:e}"
    );
    assert!(
        max_velocity <= 2.0e-8,
        "{solver:?}/{method:?} dt={dt} drifted off g'=0: {max_velocity:e}"
    );
}

#[test]
fn cartesian_pendulum_stays_on_reduced_manifold_to_t60() {
    let compiled = Compiler::new()
        .model("CartesianPendulum")
        .compile_str(CARTESIAN_PENDULUM, "CartesianPendulum.mo")
        .expect("compile Cartesian pendulum");
    let metadata = lower_dae_for_simulation(
        &compiled.dae,
        &SimOptions {
            solver_mode: SimSolverMode::Bdf,
            ..SimOptions::default()
        },
    )
    .expect("lower index-reduced pendulum");
    assert_eq!(
        metadata
            .problem
            .continuous
            .manifold_residual
            .len()
            .expect("manifold residual shape"),
        2,
        "index-3 reduction must retain both g and g'"
    );
    assert_eq!(
        metadata
            .problem
            .continuous
            .manifold_projection_plan
            .blocks
            .iter()
            .map(|block| block.rows.len())
            .sum::<usize>(),
        2
    );

    let cases = [
        (SimSolverMode::RkLike, DiffsolMethod::Bdf, 0.002),
        (SimSolverMode::RkLike, DiffsolMethod::Bdf, 0.01),
        (SimSolverMode::Bdf, DiffsolMethod::Bdf, 0.002),
        (SimSolverMode::Bdf, DiffsolMethod::Bdf, 0.02),
    ];
    for (solver_mode, diffsol_method, dt) in cases {
        let opts = SimOptions {
            t_end: 60.0,
            rtol: 1.0e-8,
            atol: 1.0e-9,
            dt: Some(dt),
            max_wall_seconds: Some(30.0),
            solver_mode,
            diffsol_method,
            ..SimOptions::default()
        };
        let result = simulate_dae_with_diagnostics(&compiled.dae, &opts).unwrap_or_else(|error| {
            panic!("{solver_mode:?}/{diffsol_method:?} dt={dt} failed: {error:?}")
        });
        assert_manifold_trace(&result, solver_mode, diffsol_method, dt);
    }

    // The SDIRK tableaus used to be *containable* on a manifold model: they were
    // reachable, unvalidated against the index-reduction manifold, and rejected
    // at build time by a `diffsol_method != Bdf` guard. They went with the
    // general/implicit DAE path they were wired onto (SPEC 0038), so the guard
    // that has to hold now is at the point of *naming* one: the request is
    // reported against the valid set, never dropped and silently run as BDF on
    // this manifold model.
    for name in ["esdirk34", "trbdf2"] {
        let error = rumoca_core::canonical_solver_name(name)
            .expect_err("a solver this tree cannot run must not resolve to one it can");
        let rendered = error.to_string();
        assert!(
            rendered.contains(name) && rendered.contains("bdf") && rendered.contains("rk-like"),
            "'{name}' must be reported against the valid solver set: {rendered}"
        );
    }

    let mut session = SimulationSession::new_with_diagnostics(
        &compiled.dae,
        SimOptions {
            t_end: 60.0,
            rtol: 1.0e-8,
            atol: 1.0e-9,
            dt: Some(0.01),
            solver_mode: SimSolverMode::Bdf,
            ..SimOptions::default()
        },
    )
    .expect("create projected BDF session");
    session
        .advance_to(60.0)
        .expect("advance projected BDF session to t=60");
    let state = session.state().expect("read projected BDF session");
    let value = |name: &str| {
        *state
            .values
            .get(name)
            .unwrap_or_else(|| panic!("session state omitted `{name}`"))
    };
    let (x, y, vx, vy) = (value("x"), value("y"), value("vx"), value("vy"));
    assert!((x * x + y * y - 1.0).abs() <= 2.0e-8);
    assert!((x * vx + y * vy).abs() <= 2.0e-8);
}
