use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};
use rumoca_solver::SimExecutionPolicy;

const SOURCE: &str = r#"
model ParameterRotation
  type Rotation = enumeration(Axis, Other, Identity);
  record Orientation
    Real T[2,2];
  end Orientation;
  function planarRotation
    input Real angle;
    output Orientation R;
  algorithm
    R.T := [cos(angle), sin(angle); -sin(angle), cos(angle)];
  end planarRotation;
  parameter Rotation rotationType = Rotation.Axis;
  parameter Real angle = 0.5235987755982988;
  final parameter Orientation R = if rotationType == Rotation.Axis then
      planarRotation(angle) else if rotationType == Rotation.Other then
      planarRotation(-angle) else planarRotation(0.0);
  Real force[2];
  Real position(start=0, fixed=true);
equation
  force = transpose(R.T)*{0.0, 1000.0};
  der(position) = force[1];
end ParameterRotation;

model ParameterRotationOther
  extends ParameterRotation(rotationType=Rotation.Other);
end ParameterRotationOther;

model ParameterRotationIdentity
  extends ParameterRotation(rotationType=Rotation.Identity);
end ParameterRotationIdentity;
"#;

#[test]
fn parameter_relations_do_not_own_continuous_crossing_roots() {
    let compiled = Compiler::new()
        .model("ParameterRotation")
        .compile_str(SOURCE, "parameter_rotation.mo")
        .unwrap();
    compiled.dae.inspect(|view| {
        assert_eq!(
            view.roots().count(),
            0,
            "parameter-only conditionals cannot own continuous crossing roots"
        );
    });
}

#[test]
fn parameter_relations_preserve_record_binding_during_initialization() {
    for (model, force) in [
        ("ParameterRotation", [-500.0, 500.0 * 3.0_f64.sqrt()]),
        ("ParameterRotationOther", [500.0, 500.0 * 3.0_f64.sqrt()]),
        ("ParameterRotationIdentity", [0.0, 1000.0]),
    ] {
        check_rotation(model, force);
    }
}

fn check_rotation(model: &str, force: [f64; 2]) {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, "parameter_rotation.mo")
        .unwrap();
    for execution in [SimExecutionPolicy::Auto, SimExecutionPolicy::Interpreter] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.02,
                dt: Some(0.01),
                execution_policy: execution,
                ..Default::default()
            },
        )
        .unwrap();
        for (name, expected) in [("force[1]", force[0]), ("force[2]", force[1])] {
            let index = result.names.iter().position(|value| value == name).unwrap();
            for (time, value) in result.times.iter().zip(&result.data[index]) {
                assert!(
                    (value - expected).abs() < 1e-8,
                    "{model}/{execution:?}: {name} at {time} is {value}, expected {expected}"
                );
            }
        }
    }
}

#[test]
fn parameter_relations_in_when_activations_have_no_crossing_roots() {
    let source = r#"
model ParameterActivation
  parameter Real level = 1;
  discrete Real held(start=0, fixed=true);
  Real x(start=0, fixed=true);
equation
  der(x) = held;
  when level > 0 then
    held = 1;
  end when;
end ParameterActivation;
"#;
    let compiled = Compiler::new()
        .model("ParameterActivation")
        .compile_str(source, "parameter_activation.mo")
        .unwrap();
    compiled
        .dae
        .inspect(|view| assert_eq!(view.roots().count(), 0));
}
