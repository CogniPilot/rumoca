//! Overriding a tunable parameter must re-derive *promoted array masks* that
//! depend on it. A parameter-derived array that a state derivative reads
//! (`der(x[i]) = -m[i]*x[i]`) is promoted to a derived parameter; before the
//! override-aware lowering path it was baked at the declared `aoa`, so changing
//! `aoa` had no effect (the user's "aoa forces a recompile" bug). This covers the
//! root-cause fix: array-valued dependents re-derive at parameter-set time.

use rumoca::Compiler;
use rumoca_ir_solve::ScalarSlot;
use rumoca_sim::{
    SimOptions, build_simulation_with_stage_timing_and_solve_model, lower_dae_for_simulation,
    lower_for_simulation_with_overrides,
};
use std::sync::Arc;

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

fn compile_mask_dae() -> Arc<rumoca_ir_dae::Dae> {
    let compiled = Compiler::new()
        .model("PromotedMask")
        .compile_str(SOURCE, "promoted_mask.mo")
        .expect("compile PromotedMask");
    Arc::clone(compiled.dae())
}

/// The three `m[i]` parameter-slot values. Asserts `m` is a parameter slot (i.e.
/// it was promoted), so this test actually exercises the promoted-parameter path
/// rather than the already-working solver-algebraic settle.
fn mask_param_values(model: &rumoca_ir_solve::SolveModel) -> Vec<f64> {
    (1..=3)
        .map(
            |i| match model.problem.layout().binding(&format!("m[{i}]")) {
                Some(ScalarSlot::P { index, .. }) => model.parameters[index],
                other => panic!("m[{i}] must be a promoted parameter slot, got {other:?}"),
            },
        )
        .collect()
}

#[test]
fn aoa_override_rederives_promoted_array_mask() {
    let dae = compile_mask_dae();
    dae.inspect(|view| {
        let (_, mask) = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "m")
            .expect("checked DAE retains the source declaration identity for m");
        assert_eq!(mask.role(), rumoca_ir_dae::VariableRole::Parameter);
        assert_eq!(
            mask.causality(),
            rumoca_ir_dae::VariableCausality::CalculatedParameter
        );
        let binding = view
            .expression(
                mask.binding()
                    .expect("calculated parameter has one binding"),
            )
            .expect("binding expression resolves");
        assert!(matches!(
            binding.operation(),
            rumoca_ir_dae::ExpressionOperation::Comprehension { .. }
        ));
        assert_eq!(
            binding.provenance().origin(),
            rumoca_ir_dae::DaeProvenanceOrigin::Generated(
                rumoca_ir_dae::DaeGeneration::DerivedParameterLowering
            )
        );
    });

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

    let mut timed_mask = None;
    let (_prepared, _timings) = build_simulation_with_stage_timing_and_solve_model(
        &dae,
        &opts,
        |_| {},
        |model| timed_mask = Some(mask_param_values(model)),
    )
    .expect("build prepared override path");
    assert_eq!(
        timed_mask.expect("prepared lowering exposes its correlated model to the build observer"),
        m_over,
        "timed/prepared lowering must re-derive promoted masks the same way as one-shot lowering"
    );
}
