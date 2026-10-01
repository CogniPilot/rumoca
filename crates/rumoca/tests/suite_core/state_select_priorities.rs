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

#[test]
fn a_preference_whose_selection_is_refused_keeps_the_reducer_basis_and_records_why() {
    // The trial point seeds x at its start guess 0, where y = 1/x is not
    // finite, so the checked selection refuses the requested basis. `prefer`
    // is a request: the differentiated basis is kept and the refusal recorded.
    let source = "model Refused
  Real x(start = 0);
  Real y(stateSelect = StateSelect.prefer);
initial equation
  x = 1;
equation
  der(x) = -x;
  y = 1/x;
end Refused;";
    assert_eq!(integrated(source, "Refused"), ["x"]);
    let reason = rumoca_phase_solve::withheld_state_preferences(&dae(source, "Refused"))
        .unwrap()
        .expect("the refusal is recorded");
    assert!(reason.contains("independent state selection"), "{reason}");
    let result = simulate_dae_with_diagnostics(
        &dae(source, "Refused"),
        &SimOptions {
            t_end: 1.0,
            ..Default::default()
        },
    )
    .expect("the reducer basis simulates");
    let x = column(&result, "x");
    for (row, &time) in result.times.iter().enumerate() {
        assert!((result.data[x][row] - (-time).exp()).abs() < 1e-5);
    }
}

#[test]
fn preferred_values_replace_a_retained_constraint_manifold() {
    // An open tank with a compressible medium (Modelica.Fluid OpenTank with
    // LinearColdWater): `der(V)` in the volume work makes the reducer retain
    // the definitional constraint `u = h - p/d` among U, m, and V on a
    // manifold, whose source coordinates cannot be reconstructed. The formal
    // selection integrates the preferred T and level instead.
    let source = "model Compressible
  parameter Real p = 1e5;
  Real U, m, u, d, h, Wb;
  Real V(stateSelect = StateSelect.never);
  Real T(stateSelect = StateSelect.prefer, start = 300);
  Real level(stateSelect = StateSelect.prefer, start = 1);
equation
  V = 2*level;
  m = V*d;
  d = 1000*(1 - 2e-4*(T - 293));
  h = 4184*(T - 293) + (p - 1e5)/1000;
  u = h - p/d;
  U = m*u;
  Wb = -p*der(V);
  der(m) = -1;
  der(U) = -h + Wb;
initial equation
  T = 300;
  level = 1;
end Compressible;";
    assert_eq!(integrated(source, "Compressible"), ["T", "level"]);
    let result = simulate_dae_with_diagnostics(
        &dae(source, "Compressible"),
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("the preferred basis simulates");
    let m = column(&result, "m");
    let initial = result.data[m][0];
    for (row, &time) in result.times.iter().enumerate() {
        assert!(
            (result.data[m][row] - (initial - time)).abs() < 1e-6,
            "m at {time}"
        );
    }
}

#[test]
fn a_selection_that_honors_no_preference_better_keeps_the_reducer_basis() {
    // The equations determine the preferred y, so no basis can integrate it.
    // The formal selection then ranks no higher than the reducer's basis and
    // gives no reason to replace it, whichever differentiated coordinate of
    // equal rank it chose.
    let source = "model EqualRank
  Real a(start = 1);
  Real b;
  Real y(stateSelect = StateSelect.prefer, start = 0.5);
equation
  der(a) + der(b) = -(a + b);
  b = 2*a + 1;
  y + exp(y) = 2 + time;
initial equation
  a = 1;
end EqualRank;";
    assert_eq!(integrated(source, "EqualRank"), ["a"]);
}

#[test]
fn an_always_value_the_equations_determine_is_refused() {
    // MLS 3.7 §4.9.7.1 "Do use it as a state" and §3.7.3 "It is an error if
    // the variable cannot be selected as a state": y = sin(time) has no
    // independent integration slot, so the selection is refused and names y.
    let source = "model Determined
  Real x(start = 1, fixed = true);
  Real y(stateSelect = StateSelect.always);
equation
  der(x) = -x;
  y = sin(time);
end Determined;";
    let error = rumoca_phase_solve::integrated_state_names(&dae(source, "Determined"))
        .expect_err("y cannot be a state");
    let message = error.to_string();
    assert!(message.contains("StateSelect.always values y"), "{message}");
    assert!(message.contains("MLS 3.7"), "{message}");
}

#[test]
fn always_values_the_selection_can_integrate_are_the_states() {
    // The tank balance with `always` on temperature and level: the formal
    // selection integrates exactly the requested values in place of U and m.
    let tank = TANK.replace("StateSelect.prefer", "StateSelect.always");
    assert_eq!(integrated(&tank, "PreferredTank"), ["T", "level"]);
    // A position-level alias and a rate alias of one oscillator, as the
    // rolling wheel set's coordinates and wheel speeds: each `always` value
    // is integrated at the derivative level its own equations place it.
    let source = "model Aliased
  Real s(start = 0, fixed = true);
  Real v(start = 1, fixed = true);
  Real x(stateSelect = StateSelect.always);
  Real w(stateSelect = StateSelect.always);
equation
  der(s) = v;
  der(v) = -s;
  x = 2*s;
  w = der(s);
end Aliased;";
    assert_eq!(integrated(source, "Aliased"), ["w", "x"]);
    let result = simulate_dae_with_diagnostics(
        &dae(source, "Aliased"),
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("the requested basis simulates");
    let (x, w) = (column(&result, "x"), column(&result, "w"));
    for (row, &time) in result.times.iter().enumerate() {
        assert!(
            (result.data[x][row] - 2.0 * time.sin()).abs() < 1e-5,
            "x at {time}"
        );
        assert!(
            (result.data[w][row] - time.cos()).abs() < 1e-5,
            "w at {time}"
        );
    }
}
