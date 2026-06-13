//! End-to-end coverage for `rumoca_sim::refresh_prepared_vectors`: overriding
//! a tunable parameter after lowering must re-settle dependent algebraic
//! slots without re-lowering the model.

use rumoca::Compiler;
use rumoca_ir_solve::ScalarSlot;
use rumoca_sim::{PreparedVectorError, SimOptions, lower_dae_for_simulation};

fn lower_tunable_model() -> rumoca_ir_solve::SolveModel {
    let compiled = Compiler::new()
        .model("TunableAngle")
        .compile_str(TUNABLE_SOURCE, "tunable.mo")
        .expect("compile TunableAngle");
    lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).expect("lower solve model")
}

const TUNABLE_SOURCE: &str = r#"
model TunableAngle
  parameter Real aoa = 0;
  output Real ca;
  Real x(start = 2);
equation
  ca = cos(aoa * 3.141592653589793 / 180.0);
  der(x) = -ca * x;
end TunableAngle;
"#;

fn slot_index(model: &rumoca_ir_solve::SolveModel, name: &str) -> (bool, usize) {
    match model.problem.layout.binding(name) {
        Some(ScalarSlot::Y { index, .. }) => (true, index),
        Some(ScalarSlot::P { index, .. }) => (false, index),
        other => panic!("unexpected slot for `{name}`: {other:?}"),
    }
}

#[test]
fn refresh_overrides_parameter_and_resettles_algebraics() {
    let model = lower_tunable_model();

    let (is_y, aoa_idx) = slot_index(&model, "aoa");
    assert!(!is_y, "aoa should be a parameter slot");
    let (is_y, ca_idx) = slot_index(&model, "ca");
    assert!(is_y, "ca should be a solver vector slot");

    let (y_base, p_base) =
        rumoca_sim::refresh_prepared_vectors(&model, 0.0, &[]).expect("settle without overrides");
    assert!((p_base[aoa_idx] - 0.0).abs() < 1e-12);
    assert!(
        (y_base[ca_idx] - 1.0).abs() < 1e-9,
        "cos(0 deg) settle, got {}",
        y_base[ca_idx]
    );

    let (y_new, p_new) =
        rumoca_sim::refresh_prepared_vectors(&model, 0.0, &[("aoa".to_string(), 60.0)])
            .expect("settle with aoa override");
    assert!((p_new[aoa_idx] - 60.0).abs() < 1e-12);
    assert!(
        (y_new[ca_idx] - 0.5).abs() < 1e-9,
        "cos(60 deg) settle, got {}",
        y_new[ca_idx]
    );

    // The lowered model itself must stay untouched so further refreshes
    // start from the original prepared vectors.
    assert!((model.parameters[aoa_idx] - 0.0).abs() < 1e-12);
}

#[test]
fn refresh_rejects_non_parameter_names() {
    let model = lower_tunable_model();

    for name in ["ca", "x", "does_not_exist"] {
        let err = rumoca_sim::refresh_prepared_vectors(&model, 0.0, &[(name.to_string(), 1.0)])
            .expect_err("non-parameter override must fail");
        assert!(
            matches!(&err, PreparedVectorError::NotAParameter { name: n } if n == name),
            "expected NotAParameter for `{name}`, got {err:?}"
        );
    }
}
