//! Receiver-specific function selection and modifier environments, MLS §7.2–7.3.

use rumoca::Compiler;

const SOURCE: &str = r#"
package P
  partial function PartialF
    input Real x;
    output Real y;
  end PartialF;
  function Double
    extends PartialF;
  algorithm
    y := 2*x;
  end Double;
  function Scale
    extends PartialF;
    input Real gain;
  algorithm
    y := gain*x;
  end Scale;
  model World
    replaceable function F = Double constrainedby PartialF;
  end World;
  model Body
    outer World world;
    Real y;
  equation
    y = world.F(time + 1);
  end Body;
  model Assembly
    parameter Real gain = 3;
    inner World world(redeclare function F = Scale(gain=gain));
    Body body;
    Real direct;
  equation
    direct = world.F(time + 1);
  end Assembly;
  model Siblings
    Assembly first(gain=3);
    Assembly second(gain=5);
  end Siblings;
end P;
"#;

fn check(model: &str, channels: &[(&str, f64)]) {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, "receiver_function_redeclare.mo")
        .expect("receiver fixture compiles");
    let result = rumoca_sim::simulate_dae(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 0.2,
            dt: Some(0.05),
            ..Default::default()
        },
    )
    .expect("receiver fixture simulates");
    assert!(result.times.len() > 1);
    for (name, gain) in channels {
        let index = result.names.iter().position(|n| n == name).unwrap();
        for (time, actual) in result.times.iter().zip(&result.data[index]) {
            assert!(
                (actual - gain * (time + 1.0)).abs() < 1.0e-9,
                "{name} at {time}: expected {}, got {actual}",
                gain * (time + 1.0),
            );
        }
    }
}

#[test]
fn direct_and_outer_calls_use_the_redeclare_modifier_environment() {
    check("P.Assembly", &[("direct", 3.0), ("body.y", 3.0)]);
}

#[test]
fn nested_instances_keep_distinct_redeclare_modifier_values() {
    check(
        "P.Siblings",
        &[
            ("first.direct", 3.0),
            ("first.body.y", 3.0),
            ("second.direct", 5.0),
            ("second.body.y", 5.0),
        ],
    );
}
