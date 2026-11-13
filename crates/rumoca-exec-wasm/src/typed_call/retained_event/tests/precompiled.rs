use super::*;

#[test]
fn event_wire_rebinds_original_observation_and_refuses_missing_or_overflowing_inventory() {
    let original = compile();
    let program = || {
        original
            .source()
            .source()
            .event_program(s::RetainedAssertionSource::ActionConditions, 0)
            .unwrap()
    };
    let wire = super::super::PrecompiledRetainedEvent::from_compiled(&original);
    let bytes = serde_json::to_vec(&wire).unwrap();
    let decoded: super::super::PrecompiledRetainedEvent = serde_json::from_slice(&bytes).unwrap();
    let replay = decoded.bind(program()).unwrap();
    let model = original.source().source().model();
    let y = [0.0, 2.0];
    let p = vec![0.0; model.problem.layout.p_scalars()];
    let mut results = Vec::new();
    for code in [&original, &replay] {
        let mut out =
            vec![0.0; s::ScalarProgramBlock::program_output_count(code.source().program())];
        let mut captures = Vec::new();
        code.call(&y, &p, 0.0, &mut out, |_, values, _, _| {
            captures.extend(values.iter().flat_map(|(_, cells)| cells.bits()).copied());
            Ok(())
        })
        .unwrap();
        results.push((
            out.into_iter().map(f64::to_bits).collect::<Vec<_>>(),
            captures,
        ));
    }
    assert_eq!(results[0], results[1]);
    let mut changed = serde_json::to_value(&wire).unwrap();
    changed["invocations"] = serde_json::json!([]);
    let changed: super::super::PrecompiledRetainedEvent = serde_json::from_value(changed).unwrap();
    assert!(changed.bind(program()).is_err());
    let mut changed = serde_json::to_value(&wire).unwrap();
    changed["invocations"][0]["status"] = serde_json::json!(u32::MAX);
    let changed: super::super::PrecompiledRetainedEvent = serde_json::from_value(changed).unwrap();
    assert!(changed.bind(program()).is_err());
}

/// A bound product carries no Y certificate and runs on a copy; its fault is
/// the certified original's, with the same source fault entry.
#[test]
fn a_bound_event_has_no_y_certificate_and_faults_as_the_certified_original() {
    let original = compile();
    let program = original
        .source()
        .source()
        .event_program(s::RetainedAssertionSource::ActionConditions, 0)
        .unwrap();
    let replay = super::super::PrecompiledRetainedEvent::from_compiled(&original)
        .bind(program)
        .unwrap();
    assert!(original.read_only_y.is_some());
    assert!(replay.read_only_y.is_none());
    let faults = [&original, &replay].map(|code| {
        let mut out = [37.25];
        let error = code
            .call(&[0.0, 0.0], &[], 0.0, &mut out, |_, _, _, _| Ok(()))
            .unwrap_err();
        let WasmCompileError::TypedSource(fault) = error else {
            panic!("the event fault must keep its source-fault category");
        };
        assert_eq!(out, [37.25]);
        fault
    });
    assert_eq!(faults[0], faults[1]);
    assert_eq!(faults[0].kind, TypedCallFaultKind::IntegerArithmetic);
    assert_eq!(faults[0].provenance, span(9));
}
