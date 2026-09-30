//! MLS 3.7 §4.9.7.1 `StateSelect` priorities in the static state selection:
//! `prefer` values the source does not differentiate compete with the
//! differentiated coordinates, `never` values leave the basis or refuse, and a
//! `prefer` request whose prolongation cannot be differentiated is withheld.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

fn dae(source: &str, model: &str) -> std::sync::Arc<rumoca_ir_dae::Dae> {
    Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap()
        .dae
}

fn integrated(source: &str, model: &str) -> Vec<String> {
    let mut names = rumoca_phase_solve::integrated_state_names(&dae(source, model)).unwrap();
    names.sort();
    names
}

fn column(result: &rumoca_sim::SimResult, name: &str) -> usize {
    result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("trace exposes {name}"))
}

/// The lumped-volume balance of an open tank (Modelica.Fluid.Vessels): mass and
/// internal energy are differentiated, level and temperature are preferred.
const TANK: &str = r#"
model PreferredTank
  Real U, m, u;
  Real T(stateSelect = StateSelect.prefer, start = 290);
  Real level(stateSelect = StateSelect.prefer, start = 0.5);
equation
  m = 2*level;
  U = m*u;
  u = 4184*(T - 298.15);
  der(U) = -1;
  der(m) = -0.001;
initial equation
  T = 300;
  level = 1;
end PreferredTank;
"#;

#[test]
fn preferred_values_replace_the_differentiated_balances() {
    assert_eq!(integrated(TANK, "PreferredTank"), ["T", "level"]);
    let result = simulate_dae_with_diagnostics(
        &dae(TANK, "PreferredTank"),
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("the preferred basis simulates");
    let (temperature, level) = (column(&result, "T"), column(&result, "level"));
    let initial_energy = 2.0 * 4184.0 * (300.0 - 298.15);
    for (row, &time) in result.times.iter().enumerate() {
        let mass = 2.0 - 0.001 * time;
        let expected = (initial_energy - time) / mass / 4184.0 + 298.15;
        assert!(
            (result.data[level][row] - mass / 2.0).abs() < 1e-6,
            "level at {time}"
        );
        assert!(
            (result.data[temperature][row] - expected).abs() < 1e-6,
            "T at {time}"
        );
    }
}

/// One differentiated `x` and one algebraic `y = 2*x + 1`, each with a
/// `StateSelect` value; the basis is the single integrated scalar.
fn pair(x: &str, y: &str) -> String {
    format!(
        "model Pair
  Real x(stateSelect = StateSelect.{x}, start = 1);
  Real y(stateSelect = StateSelect.{y});
equation
  der(x) = -x;
  y = 2*x + 1;
end Pair;"
    )
}

#[test]
fn a_preferred_algebraic_outranks_default_avoid_and_never_differentiated_values() {
    for x in ["default", "avoid", "never"] {
        assert_eq!(integrated(&pair(x, "prefer"), "Pair"), ["y"], "x {x}");
    }
    // Equal preferences leave the choice to the selection; one scalar is integrated.
    assert_eq!(integrated(&pair("prefer", "prefer"), "Pair").len(), 1);
    assert_eq!(integrated(&pair("always", "prefer"), "Pair"), ["x"]);
    // A requested algebraic is promoted before selection (STRUCT-T07); its
    // definitional constraint on x is retained on the state manifold.
    assert!(integrated(&pair("prefer", "always"), "Pair").contains(&"y".to_owned()));
}

#[test]
fn only_differentiated_values_compete_without_a_preference() {
    // MLS: `default` and `avoid` are states only when they appear differentiated.
    for (x, y) in [
        ("default", "default"),
        ("avoid", "default"),
        ("avoid", "avoid"),
    ] {
        assert_eq!(integrated(&pair(x, y), "Pair"), ["x"], "x {x}, y {y}");
    }
}

#[test]
fn differentiated_values_rank_prefer_default_avoid() {
    let coupled = |a: &str, b: &str| {
        format!(
            "model Coupled
  Real a(stateSelect = StateSelect.{a});
  Real b(stateSelect = StateSelect.{b});
equation
  der(a) + der(b) = -(a + b);
  b = 2*a;
initial equation
  a = 1;
end Coupled;"
        )
    };
    for (a, b, expected) in [
        ("prefer", "default", "a"),
        ("default", "prefer", "b"),
        ("avoid", "default", "b"),
        ("default", "avoid", "a"),
        ("never", "avoid", "b"),
    ] {
        assert_eq!(
            integrated(&coupled(a, b), "Coupled"),
            [expected],
            "a {a}, b {b}"
        );
    }
}

#[test]
fn a_preference_the_equations_determine_adds_no_state() {
    let source = "model Determined
  Real x(start = 1, fixed = true);
  Real y(stateSelect = StateSelect.prefer, start = 0.5);
equation
  der(x) = -x;
  y + exp(y) = 2 + time;
end Determined;";
    assert_eq!(integrated(source, "Determined"), ["x"]);
}

#[test]
fn a_never_value_no_basis_avoids_is_refused() {
    let source = "model Never
  Real x(stateSelect = StateSelect.never, start = 1, fixed = true);
equation
  der(x) = -x;
end Never;";
    let error = rumoca_phase_solve::integrated_state_names(&dae(source, "Never"))
        .expect_err("x cannot leave the basis");
    assert!(error.to_string().contains("StateSelect.never"), "{error}");
}

#[test]
fn a_preference_whose_prolongation_is_not_differentiable_is_withheld() {
    let algorithmic = "model Withheld
  function f
    input Real u;
    output Real y;
  algorithm
    y := 2*u;
    for i in 1:3 loop
      y := y + 0.1*u;
    end for;
  end f;
  Real x(start = 1, fixed = true);
  Real y(stateSelect = StateSelect.prefer);
equation
  der(x) = -x;
  y = f(x);
end Withheld;";
    let integer = "model Withheld
  Real x(start = 1, fixed = true);
  Real y(stateSelect = StateSelect.prefer);
  Integer k;
equation
  der(x) = -x;
  k = integer(3*x);
  y = x + k;
end Withheld;";
    for source in [algorithmic, integer] {
        assert_eq!(integrated(source, "Withheld"), ["x"]);
        let result = simulate_dae_with_diagnostics(
            &dae(source, "Withheld"),
            &SimOptions {
                t_end: 1.0,
                ..Default::default()
            },
        )
        .expect("the withheld preference keeps the differentiated basis");
        let x = column(&result, "x");
        for (row, &time) in result.times.iter().enumerate() {
            assert!((result.data[x][row] - (-time).exp()).abs() < 1e-5);
        }
    }
}

#[test]
fn a_preferred_chart_switches_where_its_slope_vanishes() {
    // y = x^3 is integrated while x < 0; its reconstruction of x folds at
    // x = 0, where the selection exchanges the chart and continues.
    let source = "model Cubic
  Real x(start = -1, fixed = true);
  Real y(stateSelect = StateSelect.prefer);
equation
  der(x) = 1;
  y = x^3;
end Cubic;";
    assert_eq!(integrated(source, "Cubic"), ["y"]);
    let result = simulate_dae_with_diagnostics(
        &dae(source, "Cubic"),
        &SimOptions {
            t_end: 3.0,
            dt: Some(0.05),
            ..Default::default()
        },
    )
    .expect("the cubic chart switches past its fold");
    let (x, y) = (column(&result, "x"), column(&result, "y"));
    for (row, &time) in result.times.iter().enumerate() {
        let expected = time - 1.0;
        assert!((result.data[x][row] - expected).abs() < 1e-4, "x at {time}");
        assert!(
            (result.data[y][row] - expected.powi(3)).abs() < 1e-3,
            "y at {time}"
        );
    }
}
