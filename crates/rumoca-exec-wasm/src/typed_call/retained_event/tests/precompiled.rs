use super::*;

#[test]
fn event_wire_rebinds_original_observation_and_refuses_missing_or_overflowing_inventory() {
    let original = compile();
    let wire = super::super::PrecompiledRetainedEvent::from_compiled(&original);
    let bytes = serde_json::to_vec(&wire).unwrap();
    let decoded: super::super::PrecompiledRetainedEvent = serde_json::from_slice(&bytes).unwrap();
    let replay = decoded.bind(original.source().clone()).unwrap();
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
    assert!(changed.bind(original.source().clone()).is_err());
    let mut changed = serde_json::to_value(&wire).unwrap();
    changed["invocations"][0]["status"] = serde_json::json!(u32::MAX);
    let changed: super::super::PrecompiledRetainedEvent = serde_json::from_value(changed).unwrap();
    assert!(changed.bind(original.source().clone()).is_err());
}
