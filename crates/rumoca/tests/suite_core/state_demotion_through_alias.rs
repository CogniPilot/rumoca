use rumoca_compile::compile::{Session, SessionConfig};
use rumoca_sim::{SimOptions, simulate_dae};

#[test]
fn vector_kinematics_through_an_alias_preserves_initial_values() {
    check_kinematic_oscillator("framePosition = n*x;", "position = framePosition;");
}

#[test]
fn signed_alias_keeps_the_derivative_sign() {
    check_kinematic_oscillator("framePosition = -n*x;", "position = -framePosition;");
}

#[test]
fn reversed_kinematic_equation_has_the_same_reduction() {
    check_kinematic_oscillator("n*x = framePosition;", "position = framePosition;");
}

fn check_kinematic_oscillator(geometry: &str, alias: &str) {
    // MLS §§8.3.1 and 8.6: the vector and scalar coordinates describe one
    // oscillator, whose stated initial displacement and velocity must survive.
    let source = format!(
        r#"
model KinematicOscillator
  parameter Real n[3] = {{0,1,0}};
  Real x(start=1, fixed=true);
  Real v(start=0, fixed=true);
  Real a;
  Real position[3];
  Real framePosition[3];
  Real velocity[3];
  Real acceleration[3];
  Real force[3];
equation
  {geometry}
  {alias}
  velocity = der(position);
  acceleration = der(velocity);
  v = der(x);
  a = der(v);
  force = acceleration;
  force[2] = -x;
end KinematicOscillator;
"#
    );
    let mut session = Session::new(SessionConfig::default());
    session.add_document("kinematics.mo", &source).unwrap();
    let compiled = session.compile_model("KinematicOscillator").unwrap();
    assert_eq!(compiled.balance_detail.equations_unknowns(), (18, 18));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.5,
            ..SimOptions::default()
        },
    )
    .expect("an exact alias supplies the vector state's kinematic definition");
    assert!(result.times.len() > 1);
    for (name, cosine, sine) in [
        ("x", 1.0, 0.0),
        ("v", 0.0, -1.0),
        ("a", -1.0, 0.0),
        ("position[1]", 0.0, 0.0),
        ("position[2]", 1.0, 0.0),
        ("position[3]", 0.0, 0.0),
        ("velocity[2]", 0.0, -1.0),
        ("acceleration[2]", -1.0, 0.0),
        ("force[2]", -1.0, 0.0),
    ] {
        let index = result.names.iter().position(|n| n == name).unwrap();
        assert_eq!(result.data[index].len(), result.times.len());
        for (actual, time) in result.data[index].iter().zip(&result.times) {
            let expected = cosine * time.cos() + sine * time.sin();
            assert!(
                (actual - expected).abs() < 5.0e-6,
                "{name} at {time}: {actual}"
            );
        }
        assert!((result.data[index][0] - cosine).abs() < 1.0e-10);
    }
}
