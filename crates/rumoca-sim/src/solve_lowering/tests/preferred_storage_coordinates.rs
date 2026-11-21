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
