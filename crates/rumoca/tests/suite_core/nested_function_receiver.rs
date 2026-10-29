//! MLS §5.4 and §7.3: outer function calls retain the nearest inner's actuals.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package GravityReceiver
  partial function Gravity
    input Real r[3];
    output Real a[3];
  end Gravity;
  function Uniform
    input Real r[3];
    input Real g;
    input Real n[3];
    output Real a[3];
  algorithm
    a := g*n;
  end Uniform;
  model World
    parameter Real g = 9.81;
    parameter Real n[3] = {0,-1,0};
    replaceable function gravity = Uniform(g=g,n=n) constrainedby Gravity;
  end World;
  model Body
    outer World world;
    Real z(start=1,fixed=true);
    Real v(start=0,fixed=true);
    Real a[3] = world.gravity({0,0,z});
  equation
    der(z) = v;
    der(v) = a[3];
  end Body;
  model Drop
    parameter Real g=7.5;
    inner World world(g=g,n={0,0,-1});
    Body body;
    output Real z=body.z;
  end Drop;
  model Wrap
    Drop d;
  end Wrap;
  model WrapTwice
    Wrap first;
  end WrapTwice;
  model Siblings
    Drop first(g=2);
    Drop second(g=7.5);
  end Siblings;
end GravityReceiver;
"#;

#[test]
fn nested_outer_function_actuals_keep_each_inner_instance() {
    for mode in [SimSolverMode::RkLike, SimSolverMode::Bdf] {
        for (model, outputs) in [
            ("GravityReceiver.Drop", vec![("z", 7.5)]),
            ("GravityReceiver.Wrap", vec![("d.z", 7.5)]),
            ("GravityReceiver.WrapTwice", vec![("first.d.z", 7.5)]),
            (
                "GravityReceiver.Siblings",
                vec![("first.z", 2.0), ("second.z", 7.5)],
            ),
        ] {
            let compiled = Compiler::new()
                .model(model)
                .compile_str(SOURCE, "nested_function_receiver.mo")
                .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
            let result = simulate_dae_with_diagnostics(
                &compiled.dae,
                &SimOptions {
                    t_end: 0.5,
                    dt: Some(0.05),
                    solver_mode: mode,
                    ..SimOptions::default()
                },
            )
            .unwrap_or_else(|error| panic!("{model}/{mode:?}: {error:?}"));
            for (name, g) in outputs {
                let column = result
                    .names
                    .iter()
                    .position(|n| n == name)
                    .unwrap_or_else(|| panic!("{model} records {name}"));
                let actual = *result.data[column].last().expect("final sample");
                let expected = 1.0 - 0.5 * g * 0.5_f64.powi(2);
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "{model}/{mode:?}/{name}: actual {actual}, expected {expected}"
                );
            }
        }
    }
}
