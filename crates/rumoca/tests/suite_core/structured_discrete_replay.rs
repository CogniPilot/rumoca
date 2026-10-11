//! KinematicPTP2's indexed Boolean equation retains its compact B.1c owner
//! through STRUCT-T10 parameter reconstruction (SPEC_0032 §§1,4).
use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model StructuredBooleanReplay
  final parameter Real gain = 1;
  parameter Real q_begin[2] = {0, 1};
  parameter Real q_end[2] = {1, 1};
  Boolean motion_ref;
  Boolean moving[2];
  Real x(start=0, fixed=true);
equation
  der(x) = gain;
  motion_ref = time < 0.5;
  for i in 1:2 loop
    moving[i] = if abs(q_begin[i] - q_end[i]) > 0.01 then motion_ref else false;
  end for;
end StructuredBooleanReplay;
"#;

#[test]
fn parameter_reconstruction_preserves_indexed_boolean_equations() {
    let compiled = Compiler::new()
        .model("StructuredBooleanReplay")
        .compile_str(SOURCE, "StructuredBooleanReplay.mo")
        .expect("the original indexed Boolean equation compiles");
    compiled.dae.inspect(|view| {
        let owners = (0..view.discrete_value_owner_count())
            .map(|index| {
                view.discrete_value_owner(view.discrete_value_owner_id(index).unwrap())
                    .unwrap()
            })
            .filter_map(|owner| owner.structure())
            .collect::<Vec<_>>();
        assert_eq!(owners.len(), 1);
        assert_eq!(owners[0].scalar_rows(), 2);
    });
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.1),
            ..SimOptions::default()
        },
    )
    .expect("structural reconstruction preserves the Boolean owner");
    assert!(!result.times.is_empty());
    assert_eq!(result.times[0], 0.0);
    assert!((result.times.last().unwrap() - 1.0).abs() < 1e-12);
    for point in 0..=10 {
        let expected_time = f64::from(point) / 10.0;
        assert!(
            result
                .times
                .iter()
                .any(|time| (time - expected_time).abs() < 1e-12)
        );
    }
    for (name, expected) in [("x", 2), ("moving[1]", 1), ("moving[2]", 0)] {
        assert_replayed_channel(&result, name, expected);
    }
}

fn assert_replayed_channel(result: &SimResult, name: &str, expected: u8) {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .expect("complete trace");
    let values = &result.data[index];
    assert_eq!(values.len(), result.times.len());
    for (&time, &actual) in result.times.iter().zip(values) {
        assert!(time.is_finite() && actual.is_finite());
        if expected == 2 {
            assert!((actual - time).abs() < 1e-7, "x({time})={actual}");
        } else if (time - 0.5).abs() > 1e-9 {
            let value = if expected == 1 && time < 0.5 {
                1.0
            } else {
                0.0
            };
            assert_eq!(actual, value, "{name}({time})");
        }
    }
}
