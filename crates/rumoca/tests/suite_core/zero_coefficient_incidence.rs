use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"model ZeroCoefficientProbe
  parameter Real gain=2;
  final parameter Real offDiagonal=0;
  final parameter Real I[2,2]=[gain,offDiagonal;offDiagonal,gain];
  Real x[2];
equation
  I*x={1,2};
end ZeroCoefficientProbe;
model ParentCoefficientProbe
  parameter Real gain=2;
  parameter Real parent=0;
  final parameter Real offDiagonal=parent;
  final parameter Real I[2,2]=[gain,offDiagonal;offDiagonal,gain];
  Real x[2];
equation
  I*x={1,2};
end ParentCoefficientProbe;
"#;

fn check(model: &str, overrides: Vec<(String, f64)>, expected: [f64; 2]) {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, "ZeroCoefficient.mo")
        .unwrap();
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.2,
            dt: Some(0.05),
            param_overrides: overrides,
            ..Default::default()
        },
    )
    .unwrap();
    for (name, expected) in [("x[1]", expected[0]), ("x[2]", expected[1])] {
        let index = result.names.iter().position(|n| n == name).unwrap();
        assert!(!result.data[index].is_empty());
        for &actual in &result.data[index] {
            assert!(
                (actual - expected).abs() < 1e-9,
                "{model}: {name}={actual}, expected {expected}"
            );
        }
    }
}

#[test]
fn fixed_zero_tensor_coefficients_keep_live_diagonal_parameters() {
    check("ZeroCoefficientProbe", vec![], [0.5, 1.0]);
    check(
        "ZeroCoefficientProbe",
        vec![("gain".into(), 4.0)],
        [0.25, 0.5],
    );
}

#[test]
fn final_tensor_bindings_do_not_freeze_changeable_parent_zeros() {
    check("ParentCoefficientProbe", vec![], [0.5, 1.0]);
    check(
        "ParentCoefficientProbe",
        vec![("parent".into(), 0.5)],
        [1.0 / 3.75, 3.5 / 3.75],
    );
}

#[test]
fn zero_valued_parent_binding_stays_symbolic_in_flat() {
    let compiled = Compiler::new()
        .model("ParentCoefficientProbe")
        .compile_str(SOURCE, "ZeroCoefficient.mo")
        .unwrap();
    let binding = &compiled.flat.variables[&rumoca_core::VarName::new("offDiagonal")].binding;
    assert!(
        matches!(binding, Some(rumoca_core::Expression::VarRef { name, .. }) if name.as_str() == "parent"),
        "binding changed: {binding:?}"
    );
}
