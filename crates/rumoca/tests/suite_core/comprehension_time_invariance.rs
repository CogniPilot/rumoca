//! A continuous variable bound to an array comprehension over parameters,
//! such as the distributed pipe's
//! `fluidVolumes = {crossAreas[i]*lengths[i] for i in 1:n}*nParallel`, is
//! time-invariant: the comprehension domain is fixed at translation (MLS 3.7
//! §10.4.1) and its body reads only parameters and the binder. Index
//! reduction differentiates `ms[i] = fluidVolumes[i]*d[i]` under the volume
//! temperature preference; the invariance closure must fold
//! `der(fluidVolumes)` to zero instead of asking for a formal derivative of
//! the comprehension, which has none.

use rumoca::Compiler;

const SOURCE: &str = r#"
model T1
  model Volume
    Real T(stateSelect = StateSelect.prefer);
    Real u;
    Real d;
  equation
    u = 2*T;
    d = 1;
  end Volume;
  parameter Integer n = 2;
  parameter Real crossAreas[n] = {1, 2};
  parameter Real lengths[n] = {0.5, 0.5};
  parameter Real nParallel = 1;
  Real fluidVolumes[n] = {crossAreas[i]*lengths[i] for i in 1:n}*nParallel;
  Volume mediums[n];
  Real ms[n];
  Real Us[n];
  Real mb_flows[n];
  Real Hb_flows[n];
  Real m_flows[n + 1];
  Real H_flows[n + 1];
initial equation
  for i in 1:n loop
    mediums[i].T = 1;
  end for;
equation
  for i in 1:n loop
    ms[i] = fluidVolumes[i]*mediums[i].d;
    Us[i] = ms[i]*mediums[i].u;
    der(Us[i]) = Hb_flows[i];
    der(ms[i]) = mb_flows[i];
    mb_flows[i] = m_flows[i] - m_flows[i + 1];
    Hb_flows[i] = H_flows[i] - H_flows[i + 1];
  end for;
  m_flows[1] = 1;
  H_flows[1] = m_flows[1]*4;
  for i in 2:n + 1 loop
    H_flows[i] = m_flows[i]*mediums[i - 1].u;
  end for;
  annotation(experiment(StopTime = 1));
end T1;
"#;

#[test]
fn a_comprehension_bound_volume_is_time_invariant_under_index_reduction() {
    let compiled = Compiler::new()
        .model("T1")
        .compile_str(SOURCE, "ComprehensionVolumes.mo")
        .unwrap_or_else(|error| panic!("T1 compiles: {error:?}"));
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 1.0,
            ..Default::default()
        },
    )
    .expect("T1 simulates");
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name);
        &result.data[index.unwrap_or_else(|| panic!("{name} in {:?}", result.names))]
    };
    // The volumes hold constant mass, so every mass flow is 1 and the
    // temperatures obey dT1/dt = 4 - 2*T1 and dT2/dt = T1 - T2 from T = 1.
    for (index, &time) in result.times.iter().enumerate() {
        let decay = (-time).exp();
        let t1 = 2.0 - decay * decay;
        let t2 = 2.0 - 2.0 * decay + decay * decay;
        let actual1 = column("mediums[1].T")[index];
        let actual2 = column("mediums[2].T")[index];
        assert!(
            (actual1 - t1).abs() < 1e-4,
            "T1({time}) = {actual1}, expected {t1}"
        );
        assert!(
            (actual2 - t2).abs() < 1e-4,
            "T2({time}) = {actual2}, expected {t2}"
        );
    }
}
