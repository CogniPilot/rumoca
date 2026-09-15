use rumoca::Compiler;
use rumoca_core::VarName;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model Element
  parameter Real offDiagonal = 1;
  parameter Real gain = 1;
  final parameter Real matrix[2,2] = [gain, offDiagonal; offDiagonal, gain];
  Real x[2](each start=0, each fixed=true);
equation
  der(x) = matrix * {1,2};
end Element;
model FinalTensor
  parameter Real gain = 2;
  Element element(final offDiagonal=0, final gain=gain);
end FinalTensor;
model FinalTensorModified
  extends FinalTensor(gain=3);
end FinalTensorModified;
"#;

#[test]
fn final_tensor_modifiers_retain_flags_and_symbolic_parent_binding_in_flat() {
    let compiled = Compiler::new()
        .model("FinalTensor")
        .compile_str(SOURCE, "FinalTensor.mo")
        .unwrap();
    for name in ["element.offDiagonal", "element.gain", "element.matrix"] {
        assert_eq!(
            compiled.flat.variable_final_flags.get(&VarName::new(name)),
            Some(&true)
        );
    }
    let parent = &compiled.flat.variables[&VarName::new("gain")];
    assert!(!parent.evaluate);
    let child = &compiled.flat.variables[&VarName::new("element.gain")];
    assert!(matches!(
        child.binding,
        Some(rumoca_core::Expression::VarRef { .. })
    ));
}

fn check_trajectory(model: &str, override_gain: Option<f64>, gain: f64) {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, "FinalTensor.mo")
        .unwrap();
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.2,
            dt: Some(0.05),
            param_overrides: override_gain
                .map(|value| vec![("gain".into(), value)])
                .unwrap_or_default(),
            ..Default::default()
        },
    )
    .unwrap();
    for (name, factor) in [("element.x[1]", 1.0), ("element.x[2]", 2.0)] {
        let index = result.names.iter().position(|n| n == name).unwrap();
        for (&t, &value) in result.times.iter().zip(&result.data[index]) {
            assert!(
                (value - factor * gain * t).abs() < 1e-7,
                "{model}: {name} at {t}: {value}"
            );
        }
    }
}

#[test]
fn final_tensor_bindings_follow_parent_model_modifications() {
    check_trajectory("FinalTensor", None, 2.0);
    check_trajectory("FinalTensorModified", None, 3.0);
}

#[test]
fn final_tensor_bindings_follow_legal_parent_parameter_overrides() {
    check_trajectory("FinalTensor", Some(4.0), 4.0);
}
