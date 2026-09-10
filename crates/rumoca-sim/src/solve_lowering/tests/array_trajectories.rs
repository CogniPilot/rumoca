//! MLS §7.2.5 `each` and §8.3.2 equation loops: every array coordinate matters.

use super::{SimOptions, compile, simulate_dae};

#[test]
fn each_start_broadcasts_scalar_expressions_and_parameter_bindings() {
    for start in ["1/(g*(g-1))", "e0"] {
        let source = format!(
            "model EachStart
               parameter Real g=1.4;
               parameter Real e0=1/(g*(g-1));
               Real z[8](each start={start}, each fixed=true);
             equation der(z)=-z; end EachStart;"
        );
        let result = simulate_dae(&compile(&source, "EachStart"), &options())
            .expect("scalar each-start expressions initialize the complete array");
        assert_eq!(result.n_states, 8);
        for coordinate in 1..=8 {
            let name = format!("z[{coordinate}]");
            let column = result
                .names
                .iter()
                .position(|candidate| candidate == &name)
                .unwrap();
            for (time, actual) in result.times.iter().zip(&result.data[column]) {
                let expected = (-time).exp() / (1.4 * 0.4);
                assert!(
                    (actual - expected).abs() < 1.0e-7,
                    "{name} at {time}: {actual} != {expected}"
                );
            }
        }
    }
}

const FLUX_MODEL: &str = r#"
model Flux
  parameter Integer N=8;
  parameter Real dx=1.0/N;
  Real e[N](each start=1, each fixed=true);
  Real g[N](each start=1, each fixed=true);
  Real Fe[N+1];
  Real Fg[N+1];
  SCALAR_DECLARATION
equation
  SCALAR_EQUATION
  Fe[1]=0; Fg[1]=0;
  Fe[N+1]=99; Fg[N+1]=55;
  for k in 2:N loop
    Fe[k]=k*0.5*(e[k-1]+e[k]);
    Fg[k]=k*0.25*(g[k-1]+g[k]);
  end for;
  for i in 1:N loop
    der(e[i])=-(Fe[i+1]-Fe[i])/dx;
    SPLIT_LOOP
    der(g[i])=-(Fg[i+1]-Fg[i])/dx;
  end for;
end Flux;
"#;

fn options() -> SimOptions {
    SimOptions {
        t_end: 0.1,
        dt: Some(0.01),
        rtol: 1.0e-11,
        atol: 1.0e-12,
        ..SimOptions::default()
    }
}

fn flux(values: &[f64; 8], factor: f64, boundary: f64) -> [f64; 9] {
    let mut faces = [0.0; 9];
    faces[8] = boundary;
    for (face, pair) in values.windows(2).enumerate() {
        faces[face + 1] = (face + 2) as f64 * factor * (pair[0] + pair[1]);
    }
    faces
}

fn rates(values: &[f64; 8], factor: f64, boundary: f64) -> [f64; 8] {
    let faces = flux(values, factor, boundary);
    std::array::from_fn(|cell| -8.0 * (faces[cell + 1] - faces[cell]))
}

/// Independent fixed-step RK4 over the finite-volume balances, without compiler IR.
fn reference(time: f64, factor: f64, boundary: f64) -> [f64; 8] {
    let steps = (time / 0.0001).round() as usize;
    if steps == 0 {
        return [1.0; 8];
    }
    let dt = time / steps as f64;
    let mut values = [1.0; 8];
    for _ in 0..steps {
        let a = rates(&values, factor, boundary);
        let b = rates(
            &std::array::from_fn(|i| values[i] + dt * a[i] / 2.0),
            factor,
            boundary,
        );
        let c = rates(
            &std::array::from_fn(|i| values[i] + dt * b[i] / 2.0),
            factor,
            boundary,
        );
        let d = rates(
            &std::array::from_fn(|i| values[i] + dt * c[i]),
            factor,
            boundary,
        );
        values =
            std::array::from_fn(|i| values[i] + dt * (a[i] + 2.0 * b[i] + 2.0 * c[i] + d[i]) / 6.0);
    }
    values
}

fn assert_flux_sample(result: &rumoca_solver::SimResult, sample: usize, time: f64) {
    for (name, factor, boundary) in [("e", 0.5, 99.0), ("g", 0.25, 55.0)] {
        let values = reference(time, factor, boundary);
        let faces = flux(&values, factor, boundary);
        for (prefix, expected) in [
            (name.to_string(), values.as_slice()),
            (format!("F{name}"), faces.as_slice()),
        ] {
            for (cell, value) in expected.iter().enumerate() {
                let channel = format!("{prefix}[{}]", cell + 1);
                let column = result
                    .names
                    .iter()
                    .position(|candidate| candidate == &channel)
                    .unwrap();
                let actual = result.data[column][sample];
                assert!(
                    (actual - value).abs() < 1.0e-6,
                    "{channel} at {time}: {actual} != {value}"
                );
            }
        }
    }
}

#[test]
fn combined_and_split_derivative_loops_preserve_every_state_and_flux_channel() {
    for split in [false, true] {
        for scalar in [false, true] {
            let source = FLUX_MODEL
                .replace(
                    "SPLIT_LOOP",
                    if split {
                        "end for; for i in 1:N loop"
                    } else {
                        ""
                    },
                )
                .replace("SCALAR_DECLARATION", if scalar { "Real q;" } else { "" })
                .replace("SCALAR_EQUATION", if scalar { "q=5;" } else { "" });
            let result =
                simulate_dae(&compile(&source, "Flux"), &options()).expect("flux model simulates");
            assert_eq!(result.n_states, 16);
            for (sample, time) in result.times.iter().copied().enumerate() {
                assert_flux_sample(&result, sample, time);
            }
            if scalar {
                let q = result.names.iter().position(|name| name == "q").unwrap();
                assert!(result.data[q].iter().all(|value| *value == 5.0));
            }
        }
    }
}
