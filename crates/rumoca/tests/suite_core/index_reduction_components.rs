//! MLS §10.5: differentiation respects the component selected by an index.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model SelectedPosition
  Real x(start=0.5, fixed=true);
  Real p(start=0.25, fixed=true);
  Real v(start=1, fixed=true);
  Real f;
  Real s(start=1);
  Real w(start=2);
  Real road[3];
equation
  der(x) = 1;
  der(p) = v;
  der(v) = f;
  road = {s, w, x*x};
  p = road[3];
  s*s = 1;
  w*w = 4;
end SelectedPosition;
"#;

#[test]
fn selected_component_does_not_require_unrelated_algebraic_tangents() {
    check_position(
        SOURCE,
        &[("road[1]", "s"), ("road[2]", "w"), ("road[3]", "p")],
    );
}

#[test]
fn matrix_component_preserves_row_and_column_selection() {
    let source = SOURCE
        .replace("Real road[3];", "Real road[2,2];")
        .replace("road = {s, w, x*x};", "road = {{s, w}, {w, x*x}};")
        .replace("p = road[3];", "p = road[2,2];");
    check_position(
        &source,
        &[
            ("road[1,1]", "s"),
            ("road[1,2]", "w"),
            ("road[2,1]", "w"),
            ("road[2,2]", "p"),
        ],
    );
}

fn check_position(source: &str, road: &[(&str, &str)]) {
    let compiled = Compiler::new()
        .model("SelectedPosition")
        .compile_str(source, "selected_position.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                dt: Some(0.01),
                solver_mode,
                ..Default::default()
            },
        )
        .unwrap_or_else(|error| panic!("{solver_mode:?}: {error}"));
        let primary = [
            ("x", "x"),
            ("p", "p"),
            ("v", "v"),
            ("f", "f"),
            ("s", "s"),
            ("w", "w"),
        ];
        for &(name, expected_name) in primary.iter().chain(road) {
            let column = result.names.iter().position(|value| value == name).unwrap();
            for (&t, &actual) in result.times.iter().zip(&result.data[column]) {
                let x = 0.5 + t;
                let expected = match expected_name {
                    "x" => x,
                    "p" => x * x,
                    "v" => 2.0 * x,
                    "f" | "w" => 2.0,
                    "s" => 1.0,
                    _ => unreachable!(),
                };
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "{solver_mode:?}: {name}({t})={actual}, expected {expected}"
                );
            }
        }
    }
}

#[test]
fn selected_component_keeps_conflicting_fixed_initial_values() {
    let source = SOURCE.replace("p(start=0.25, fixed=true)", "p(start=0.3, fixed=true)");
    check_rejection(&source, "initial variable projection");
}

#[test]
fn an_unselected_element_keeps_its_function_assertion() {
    let source = SOURCE
        .replace(
            "model SelectedPosition",
            r#"model SelectedPosition
  function checked
    input Real u;
    output Real y;
  algorithm
    assert(u < 0.55, "kept unselected assertion");
    y := u;
  end checked;
"#,
        )
        .replace("road = {s, w, x*x};", "road = {checked(x), w, x*x};");
    check_rejection(&source, "kept unselected assertion");
}

fn check_rejection(source: &str, expected: &str) {
    let compiled = Compiler::new()
        .model("SelectedPosition")
        .compile_str(source, "selected_position_rejection.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let error = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                dt: Some(0.01),
                solver_mode,
                ..Default::default()
            },
        )
        .expect_err("differentiating one component cannot discard source obligations");
        assert!(
            error.to_string().contains(expected),
            "{solver_mode:?}: {error}"
        );
    }
}
