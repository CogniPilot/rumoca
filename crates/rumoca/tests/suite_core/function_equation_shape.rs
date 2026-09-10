use rumoca_compile::compile::{Session, SessionConfig};
use rumoca_sim::{SimOptions, simulate_dae};

#[test]
fn vector_result_equation_preserves_relative_angular_velocity() {
    // MLS §§12.4.3 and 10.6.1: the function's vector result contributes one
    // equation per component, including on the left of an equality.
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "relative_velocity.mo",
            r#"
record FrameVelocity
  Real w[3];
end FrameVelocity;

function angularVelocity
  input FrameVelocity frame;
  output Real w[3];
algorithm
  w := frame.w;
end angularVelocity;

model RelativeAngularVelocity
  FrameVelocity a(w={time, 2*time, 3*time});
  FrameVelocity b(w={4*time, 6*time, 8*time});
  Real relative[3];
equation
  angularVelocity(b) = angularVelocity(a) + relative;
end RelativeAngularVelocity;
"#,
        )
        .expect("relative angular velocity parses");
    let compiled = session
        .compile_model("RelativeAngularVelocity")
        .expect("three relative angular velocity equations balance all components");
    assert_eq!(compiled.balance_detail.equations_unknowns(), (9, 9));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.25,
            ..SimOptions::default()
        },
    )
    .expect("relative angular velocity simulates");
    assert!(result.times.len() > 1);
    for (name, slope) in [
        ("relative[1]", 3.0),
        ("relative[2]", 4.0),
        ("relative[3]", 5.0),
    ] {
        let index = result.names.iter().position(|n| n == name).unwrap();
        assert_eq!(result.data[index].len(), result.times.len());
        for (actual, time) in result.data[index].iter().zip(&result.times) {
            assert!(
                (actual - slope * time).abs() < 1.0e-10,
                "{name} at {time}: {actual}"
            );
        }
    }
}

#[test]
fn scalar_result_declaration_does_not_scalarize_a_vectorized_equation() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "vectorized_equation.mo",
            r#"
function shift
  input Real x;
  output Real y;
algorithm
  y := x + 1;
end shift;

model VectorizedEquation
  Real x[3];
equation
  shift(x) = {1, 2, 3};
end VectorizedEquation;
"#,
        )
        .unwrap();
    let compiled = session.compile_model("VectorizedEquation").unwrap();
    assert_eq!(compiled.balance_detail.equations_unknowns(), (3, 3));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.01,
            ..SimOptions::default()
        },
    )
    .unwrap();
    for (name, expected) in [("x[1]", 0.0), ("x[2]", 1.0), ("x[3]", 2.0)] {
        let index = result.names.iter().position(|n| n == name).unwrap();
        assert_eq!(result.data[index].len(), result.times.len());
        assert!(
            result.data[index]
                .iter()
                .all(|value| (value - expected).abs() < 1.0e-10)
        );
    }
}
