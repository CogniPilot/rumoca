//! Overriding a tunable parameter must re-derive *promoted array masks* that
//! depend on it. A parameter-derived array that a state derivative reads
//! (`der(x[i]) = -m[i]*x[i]`) is promoted to a derived parameter; before the
//! override-aware lowering path it was baked at the declared `aoa`, so changing
//! `aoa` had no effect (the user's "aoa forces a recompile" bug). This covers the
//! root-cause fix: array-valued dependents re-derive at parameter-set time.

use rumoca::Compiler;
use rumoca_ir_solve::ScalarSlot;
use rumoca_sim::{
    SimOptions, build_simulation, lower_dae_for_simulation, lower_for_simulation_with_overrides,
    refresh_prepared_vectors,
};

// Mirrors the airfoil's structure: a parameter-derived array mask `m` computed in
// its own loop, then read by the state-derivative loop. That makes `m` a
// derivative-reachable, parameter-variable structured family, which is promoted to
// a derived parameter array (asserted below) — exactly the `sc/nc/sig` case.
const SOURCE: &str = r#"
model PromotedMask
  parameter Real aoa = 0;
  parameter Real tau = 0.1;
  Real m[3];
  Real x[3](start = {1, 1, 1});
equation
  for i in 1:3 loop
    m[i] = cos(aoa * 3.141592653589793 / 180.0) + i;
  end for;
  for i in 1:3 loop
    der(x[i]) = -x[i] / tau - m[i] * x[i];
  end for;
end PromotedMask;
"#;

fn compile_mask_dae() -> rumoca_ir_dae::Dae {
    Compiler::new()
        .model("PromotedMask")
        .compile_str(SOURCE, "promoted_mask.mo")
        .expect("compile PromotedMask")
        .dae
}

/// The three `m[i]` parameter-slot values. Asserts `m` is a parameter slot (i.e.
/// it was promoted), so this test actually exercises the promoted-parameter path
/// rather than the already-working solver-algebraic settle.
fn mask_param_values(model: &rumoca_ir_solve::SolveModel) -> Vec<f64> {
    (1..=3)
        .map(|i| match model.problem.layout.binding(&format!("m[{i}]")) {
            Some(ScalarSlot::P { index, .. }) => model.parameters[index],
            other => panic!("m[{i}] must be a promoted parameter slot, got {other:?}"),
        })
        .collect()
}

#[test]
fn aoa_override_rederives_promoted_array_mask() {
    let dae = compile_mask_dae();

    // Declared aoa = 0: cos(0 deg) = 1, so m[i] = 1 + i = {2, 3, 4}.
    let base = lower_dae_for_simulation(&dae, &SimOptions::default()).expect("lower default");
    let m_base = mask_param_values(&base);
    for (i, value) in m_base.iter().enumerate() {
        let expected = 1.0 + (i as f64 + 1.0);
        assert!(
            (value - expected).abs() < 1e-9,
            "default m[{}] = {value}, expected {expected}",
            i + 1
        );
    }

    // Override aoa = 60: cos(60 deg) = 0.5, so m[i] = 0.5 + i = {1.5, 2.5, 3.5}.
    let opts = SimOptions {
        param_overrides: vec![("aoa".to_string(), 60.0)],
        ..SimOptions::default()
    };
    let overridden = lower_for_simulation_with_overrides(&dae, &opts).expect("lower override");
    let m_over = mask_param_values(&overridden);
    for (i, value) in m_over.iter().enumerate() {
        let expected = 0.5 + (i as f64 + 1.0);
        assert!(
            (value - expected).abs() < 1e-9,
            "overridden m[{}] = {value}, expected {expected} (mask must follow aoa)",
            i + 1
        );
    }

    let timed = build_simulation(&dae, &opts).expect("build prepared override path");
    assert_eq!(
        mask_param_values(timed.model()),
        m_over,
        "timed/prepared lowering must re-derive promoted masks the same way as one-shot lowering"
    );
}

/// The WebGPU host (`update_gpu_parameters`) path: override-aware lowering followed
/// by `refresh_prepared_vectors`. The returned parameter vector `p0` must carry the
/// re-derived mask, so the on-device kernels read a mask that follows the slider.
#[test]
fn gpu_parameter_update_path_rederives_mask_in_p0() {
    let dae = compile_mask_dae();
    let opts = SimOptions {
        param_overrides: vec![("aoa".to_string(), 60.0)],
        ..SimOptions::default()
    };
    let model = lower_for_simulation_with_overrides(&dae, &opts).expect("lower override");
    let (_y0, p0) =
        refresh_prepared_vectors(&model, 0.0, &[("aoa".to_string(), 60.0)]).expect("refresh");

    for i in 1..=3 {
        let ScalarSlot::P { index, .. } = model
            .problem
            .layout
            .binding(&format!("m[{i}]"))
            .expect("m slot")
        else {
            panic!("m[{i}] must be a parameter slot");
        };
        let expected = 0.5 + i as f64;
        assert!(
            (p0[index] - expected).abs() < 1e-9,
            "p0 m[{i}] = {}, expected {expected} (GPU host must see the re-derived mask)",
            p0[index]
        );
    }
}
