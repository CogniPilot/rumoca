//! A fixed position on a strict contact boundary remains exact during §8.6.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model InitialFixedBoundary
  parameter Real stop = -0.004;
  parameter Real length = 0;
  Real position(start=0);
  Real x(start=stop, fixed=true);
  Real other(start=0.8);
  Real signal(start=0, fixed=true);
  Boolean contact;
equation
  der(position) = 1;
  x = position - length/2;
  der(other) = 0;
  signal = 100*(other - 1);
  contact = position < stop;
end InitialFixedBoundary;
"#;

#[test]
fn fixed_boundary_survives_unrelated_algebraic_initial_row() {
    check_fixed_boundary(SOURCE);
}

#[test]
fn fixed_boundary_corrects_a_seed_within_projection_tolerance() {
    for seed in ["stop-1e-16", "stop+1e-16"] {
        check_fixed_boundary(
            &SOURCE.replace("position(start=0)", &format!("position(start={seed})")),
        );
    }
}

#[test]
fn fixed_boundary_corrects_a_seed_when_all_initial_rows_are_within_tolerance() {
    let source = SOURCE
        .replace("position(start=0)", "position(start=stop-1e-16)")
        .replace("other(start=0.8)", "other(start=1)");
    check_fixed_boundary(&source);
}

fn check_fixed_boundary(source: &str) {
    let dae = Compiler::new()
        .model("InitialFixedBoundary")
        .compile_str(source, "initial_fixed_boundary.mo")
        .expect("source model compiles")
        .dae;
    for solver_mode in [SimSolverMode::RkLike, SimSolverMode::Bdf] {
        let result = simulate_dae_with_diagnostics(
            &dae,
            &SimOptions {
                t_end: 0.001,
                dt: Some(0.001),
                rtol: 1.0e-7,
                atol: 1.0e-7,
                solver_mode,
                ..SimOptions::default()
            },
        )
        .expect("fixed position permits simulation");
        let initial = |name: &str| {
            let index = result
                .names
                .iter()
                .position(|candidate| candidate == name)
                .unwrap();
            result.data[index][0]
        };
        assert_eq!(initial("position"), -0.004, "{solver_mode:?}");
        assert_eq!(initial("x"), -0.004, "{solver_mode:?}");
        assert_eq!(initial("contact"), 0.0, "{solver_mode:?}");
        assert!(initial("signal").abs() < 1.0e-9, "{solver_mode:?}");
    }
}
