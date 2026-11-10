use super::*;

#[test]
fn issued_exact_artifact_roundtrip_keeps_native_bits_and_refuses_foreign_binding() {
    let mut session = rumoca_compile::Session::default();
    session.add_document("exact-wire.mo", "model ExactWire parameter Real gain=2; Real x(start=1,fixed=true); Real y; equation der(x)=-gain*x; y=x+1; end ExactWire;").unwrap();
    let compiled = session.compile_model("ExactWire").unwrap();
    let model = rumoca_phase_solve::lower_solve_model_for_profile(
        &compiled.dae,
        &Default::default(),
        Default::default(),
        rumoca_ir_solve::RefreshProfile::Scalar,
        |_| {},
    )
    .unwrap()
    .into_model();
    let layout = &model.problem.layout;
    let source = &model.problem.continuous.implicit_rhs;
    let owners = &model.problem.continuous.refresh_owners;
    let schedule = owners
        .exact_assignment_schedules()
        .iter()
        .find(|s| !s.program_ids().is_empty())
        .unwrap();
    let original =
        compile_exact_assignment_schedule_wasm(source, owners, schedule, layout, &model.pure_calls)
            .unwrap();
    let wire = PrecompiledExactAssignments::from_compiled(&original);
    let decoded: PrecompiledExactAssignments =
        serde_json::from_slice(&serde_json::to_vec(&wire).unwrap()).unwrap();
    let key = rumoca_ir_solve::PreparedKernelSource::new(layout, &model.pure_calls).unwrap();
    let replay = decoded.bind(&key, source, layout).unwrap();
    assert_eq!(original.module_bytes(), replay.module_bytes());
    let mut results = Vec::new();
    for code in [&original, &replay] {
        let runtime = crate::host_runtime::HostKernelRuntime::new_named(
            code.module_bytes(),
            "eval_assignments",
        )
        .unwrap();
        let mut y = vec![1.5; layout.y_scalars()];
        let p = vec![2.0; layout.p_scalars()];
        let mut scratch = vec![0; (code.scratch_bytes() as usize).div_ceil(8)];
        assert_eq!(
            runtime
                .call_native(&mut y, &p, 0.125, &mut scratch, &mut [])
                .unwrap(),
            0
        );
        results.push(y.into_iter().map(f64::to_bits).collect::<Vec<_>>());
    }
    assert_eq!(results[0], results[1]);
    assert!(
        decoded
            .bind(&key, &ComputeBlock::default(), layout)
            .is_err()
    );
    let mut changed = serde_json::to_value(&wire).unwrap();
    changed["version"] = serde_json::json!(0);
    let changed: PrecompiledExactAssignments = serde_json::from_value(changed).unwrap();
    assert!(changed.bind(&key, source, layout).is_err());
    let mut changed = serde_json::to_value(&wire).unwrap();
    changed["payload"]["scratch_bytes"] = serde_json::json!(u32::MAX);
    let changed: PrecompiledExactAssignments = serde_json::from_value(changed).unwrap();
    assert!(changed.bind(&key, source, layout).is_err());
}
