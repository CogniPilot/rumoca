//! A preferred coordinate basis must not be lost because uninitialized stored
//! quantities keep zero construction-time guesses despite causal definitions.

use super::tangent_jacobian::compile_with_roots;
use crate::{SimOptions, simulate_dae};

#[test]
fn preferred_coordinates_seed_unfixed_zero_storage_for_trial_selection() {
    let source = r#"model StoredCoordinates
  Real m;
  Real U;
  Real p(start=150000,stateSelect=StateSelect.prefer);
  Real T(start=310.15,stateSelect=StateSelect.prefer);
  Real d;
  Real h;
  Real u;
initial equation
  p=150000;
  T=310.15;
equation
  der(m)=0;
  der(U)=0;
  m=10*d;
  U=m*u;
  d=1000*(1+(p-100000)*5e-10-(T-300)*0.0002);
  h=100000+(T-300)*4000+(p-100000)*0.94/1000;
  u=h-p/d;
end StoredCoordinates;
"#;
    let dae = compile_with_roots(source, "StoredCoordinates", &[]);
    let result = simulate_dae(
        &dae,
        &SimOptions {
            t_end: 0.0,
            ..SimOptions::default()
        },
    )
    .expect("preferred coordinates initialize without dividing by zero storage");
    let value = |name: &str| {
        let column = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .expect("source variable is observable");
        result.data[column][0]
    };
    assert!(result.data.iter().flatten().all(|value| value.is_finite()));
    assert!((value("p") - 150000.0).abs() < 1e-6);
    assert!((value("T") - 310.15).abs() < 1e-9);
    let density = 1000.0 * (1.0 + 50000.0 * 5e-10 - 10.15 * 0.0002);
    let enthalpy = 100000.0 + 10.15 * 4000.0 + 50000.0 * 0.94 / 1000.0;
    let mass = 10.0 * density;
    let energy = mass * (enthalpy - 150000.0 / density);
    assert!((value("m") - mass).abs() < 1e-8);
    assert!((value("U") - energy).abs() < 1e-5);
}

#[test]
fn closed_array_comprehension_defines_a_required_state_coordinate_scale() {
    let source = r#"model RequiredCoordinate
  parameter Real a[2]={2,3};
  Real v[2]={a[i]*a[i] for i in 1:2};
  Real m;
  Real x(start=2,stateSelect=StateSelect.always);
initial equation
  x=2;
equation
  der(m)=0;
  m=v[1]*x;
end RequiredCoordinate;"#;
    let dae = compile_with_roots(source, "RequiredCoordinate", &[]);
    let result = simulate_dae(
        &dae,
        &SimOptions {
            t_end: 0.0,
            ..Default::default()
        },
    )
    .expect("the comprehension closes its own index and defines the scale");
    for (name, expected) in [("v[1]", 4.0), ("v[2]", 9.0), ("m", 8.0), ("x", 2.0)] {
        let index = result.names.iter().position(|n| n == name).unwrap();
        assert_eq!(result.data[index][0], expected);
    }
}

#[test]
fn preferred_coordinate_accepts_an_empty_argument_in_a_causal_guess() {
    let source = r#"model EmptyArgument
  function f
    input Real u;
    input Real v[:];
    output Real y;
  algorithm
    y := u;
    for i in 1:size(v,1) loop
      y := y + v[i];
    end for;
    annotation(Inline=false, smoothOrder=2);
  end f;
  Real xi[0];
  Real w[0];
  Real z;
  Real x(start=2, stateSelect=StateSelect.prefer);
  Real m;
initial equation
  x = 2;
equation
  xi = w;
  w = fill(time, 0);
  z = f(x, xi);
  m = z*x;
  der(m) = 0;
end EmptyArgument;"#;
    let dae = compile_with_roots(source, "EmptyArgument", &[]);
    assert_eq!(
        rumoca_phase_solve::integrated_state_names(&dae).unwrap(),
        ["x"],
        "withheld preferences: {:?}",
        rumoca_phase_solve::withheld_state_preferences(&dae).unwrap()
    );
    let result = simulate_dae(
        &dae,
        &SimOptions {
            t_end: 0.0,
            ..Default::default()
        },
    )
    .expect("initialize the selected coordinate from the causal guess");
    assert_eq!(result.times[0], 0.0);
    for (name, expected) in [("x", 2.0), ("m", 4.0)] {
        let column = result.names.iter().position(|n| n == name).unwrap();
        assert!((result.data[column][0] - expected).abs() < 1e-9, "{name}");
    }
}

fn homotopy_guess(value: &str, declarations: &str) -> std::sync::Arc<rumoca_ir_dae::Dae> {
    let source = format!(
        r#"model HomotopyGuess
  {declarations}
  Real z; Real dp; Real E; Real w;
  Real x(start=2, stateSelect=StateSelect.prefer);
initial equation
  x = 2; w = 0;
equation
  z = {value};
  w = z - 2;
  E = x*z;
  der(E) = 1;
  der(w) = -dp;
end HomotopyGuess;"#
    );
    compile_with_roots(&source, "HomotopyGuess", &[])
}

fn preferred_guess_states(dae: &rumoca_ir_dae::Dae) -> Vec<String> {
    let mut states = rumoca_phase_solve::integrated_state_names(dae).unwrap();
    let withheld = rumoca_phase_solve::withheld_state_preferences(dae).unwrap();
    assert!(states.iter().any(|s| s == "x"), "{states:?}, {withheld:?}");
    assert!(withheld.is_empty(), "{withheld:?}");
    states.sort_unstable();
    states
}

#[test]
fn homotopy_causal_guesses_select_the_actual_basis_and_initialize() {
    let expected_states = preferred_guess_states(&homotopy_guess("dp + dp^3", ""));
    for value in [
        "homotopy(dp + dp^3, 2*dp)",
        "if time >= -1 then homotopy(dp + dp^3, 2*dp) else sqrt(1 + dp*dp)",
    ] {
        let dae = homotopy_guess(value, "");
        assert_eq!(preferred_guess_states(&dae), expected_states, "{value}");
        let result = simulate_dae(
            &dae,
            &SimOptions {
                t_end: 0.0,
                ..Default::default()
            },
        )
        .expect("initialize the preferred basis");
        assert_eq!(result.times[0], 0.0);
        for (name, expected) in [("z", 2.0), ("dp", 1.0), ("x", 2.0), ("E", 4.0), ("w", 0.0)] {
            let column = result.names.iter().position(|n| n == name).unwrap();
            let initial = result.data[column][0];
            assert!(
                (initial - expected).abs() < 1e-8,
                "{value}: {name} = {initial}"
            );
        }
    }
}

#[test]
fn homotopy_causal_guess_omits_simplified_values_and_their_dependencies() {
    let expected_states = preferred_guess_states(&homotopy_guess("dp + dp^3", ""));
    let checked = r#"function checkedValue
    input Real u;
    output Real y;
  algorithm
    assert(u > 0, "positive value required");
    y := u;
    annotation(Inline=false);
  end checkedValue;
  Real q(start=0) = 1/dp;"#;
    for (value, declarations) in [
        ("homotopy(dp + dp^3, 1/dp)", ""),
        ("homotopy(dp + dp^3, q + checkedValue(dp))", checked),
    ] {
        let dae = homotopy_guess(value, declarations);
        assert_eq!(preferred_guess_states(&dae), expected_states, "{value}");
    }
}
