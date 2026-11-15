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
    let replay = decoded.bind(crate::SavedBinding::Certify, program()).unwrap();
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
    assert!(changed.bind(crate::SavedBinding::Certify, program()).is_err());
    let mut changed = serde_json::to_value(&wire).unwrap();
    changed["invocations"][0]["status"] = serde_json::json!(u32::MAX);
    let changed: super::super::PrecompiledRetainedEvent = serde_json::from_value(changed).unwrap();
    assert!(changed.bind(crate::SavedBinding::Certify, program()).is_err());
}

fn bind_wire(
    original: &CompiledRetainedEventWasm,
    wire: serde_json::Value,
) -> CompiledRetainedEventWasm {
    let program = original
        .source()
        .source()
        .event_program(s::RetainedAssertionSource::ActionConditions, 0)
        .unwrap();
    serde_json::from_value::<super::super::PrecompiledRetainedEvent>(wire)
        .unwrap()
        .bind(crate::SavedBinding::Certify, program)
        .unwrap()
}

/// Outputs or source fault of `code` over passing and faulting Y.
fn results(code: &CompiledRetainedEventWasm) -> Vec<Result<Vec<u64>, crate::TypedCallFault>> {
    [[0.0, 1.0], [1.0, 1.0], [0.0, 0.0]]
        .iter()
        .map(|y| {
            let mut out = [37.25];
            match code.call(y, &[], 0.0, &mut out, |_, _, _, _| Ok(())) {
                Ok(()) => Ok(out.map(f64::to_bits).to_vec()),
                Err(WasmCompileError::TypedSource(fault)) => {
                    assert_eq!(out, [37.25]);
                    Err(fault)
                }
                Err(error) => panic!("unexpected event error: {error}"),
            }
        })
        .collect()
}

/// Saved bytes equal to the emission at bind carry the certificate that
/// emission issued: the bound program runs on the caller's Y with the bits,
/// status and source fault of the full-copy call.
#[test]
fn a_bound_event_with_the_emitted_bytes_is_certified_and_matches_the_copy_path() {
    let original = compile();
    let wire = super::super::PrecompiledRetainedEvent::from_compiled(&original);
    let replay = bind_wire(&original, serde_json::to_value(&wire).unwrap());
    assert!(original.read_only_y.is_some());
    assert!(replay.read_only_y.is_some());
    let cells = replay.scratch.borrow().len();
    for y in [[0.0, 1.0], [1.0, 1.0], [0.0, 0.0]] {
        let bits = y.map(f64::to_bits);
        let mut shared = vec![u64::MAX; cells];
        let mut copied = shared.clone();
        let status = replay
            .runtime
            .call_native_read_only(&y, &[], 0.0, &mut shared, &mut [])
            .unwrap();
        let copied_status = replay
            .runtime
            .call_native(&mut y.to_vec(), &[], 0.0, &mut copied, &mut [])
            .unwrap();
        assert_eq!((status, &shared), (copied_status, &copied));
        assert_eq!(y.map(f64::to_bits), bits);
    }
    let expected = results(&original);
    assert!(
        matches!(&expected[2], Err(fault) if fault.kind == TypedCallFaultKind::IntegerArithmetic && fault.provenance == span(9))
    );
    assert_eq!(results(&replay), expected);
}

/// A saved module one byte away from the emission (a larger minimum of its
/// imported memory, which still validates and runs) is bound as before but
/// issued no certificate, and runs on a copy of Y with the original results.
#[test]
fn a_bound_event_with_an_altered_module_byte_runs_uncertified_on_a_copy() {
    let original = compile();
    let wire = super::super::PrecompiledRetainedEvent::from_compiled(&original);
    let mut changed = serde_json::to_value(&wire).unwrap();
    let bytes = changed["artifact"]["bytes"].as_array_mut().unwrap();
    let module = bytes
        .iter()
        .map(|b| b.as_u64().unwrap() as u8)
        .collect::<Vec<_>>();
    let import = b"\x03env\x06memory\x02";
    let at = module
        .windows(import.len())
        .position(|window| window == import)
        .unwrap()
        + import.len()
        + 1;
    assert!(module[at] < 0x7f, "the memory minimum is one LEB byte");
    bytes[at] = serde_json::json!(module[at] + 1);
    let replay = bind_wire(&original, changed);
    assert!(replay.read_only_y.is_none());
    assert_eq!(results(&replay), results(&original));
}

/// A trusted bind takes the carried program, invocations and read-only-Y flag
/// as they are: the same results as the certified bind, the certificate only
/// when the flag carries it, and a refusal of another wire version.
#[test]
fn a_trusted_event_bind_uses_the_carried_flag_and_refuses_another_version() {
    let original = compile();
    let program = || {
        original
            .source()
            .source()
            .event_program(s::RetainedAssertionSource::ActionConditions, 0)
            .unwrap()
    };
    let wire = super::super::PrecompiledRetainedEvent::from_compiled(&original);
    let trusted = |wire: serde_json::Value| {
        serde_json::from_value::<super::super::PrecompiledRetainedEvent>(wire)
            .unwrap()
            .bind(crate::SavedBinding::Trusted, program())
    };
    let replay = trusted(serde_json::to_value(&wire).unwrap()).unwrap();
    assert!(replay.read_only_y.is_some());
    assert_eq!(results(&replay), results(&original));
    let mut changed = serde_json::to_value(&wire).unwrap();
    changed["read_only_y"] = serde_json::json!(false);
    let replay = trusted(changed).unwrap();
    assert!(replay.read_only_y.is_none());
    assert_eq!(results(&replay), results(&original));
    let mut changed = serde_json::to_value(&wire).unwrap();
    changed["version"] = serde_json::json!(1);
    assert!(trusted(changed).is_err());
}

/// The certifying linked bind compares the carried invocation table with the
/// emitted one field by field: an equal table matches, and +8 on any one of
/// the eight top-level scalar fields of the first invocation does not. This
/// does not show the changed table stays in bounds, and it does not mutate
/// the nested carrier plan.
#[test]
fn the_certified_invocation_table_differs_on_any_one_changed_field() {
    let original = compile();
    let same_invocations = super::super::artifact::same_invocations;
    let table = serde_json::to_value(&original.invocations).unwrap();
    let decode = |value: serde_json::Value| {
        serde_json::from_value::<Vec<super::super::InvocationStorage>>(value).unwrap()
    };
    assert!(same_invocations(&original.invocations, &decode(table.clone())));
    for field in [
        "operation",
        "input",
        "input_bytes",
        "output",
        "output_bytes",
        "scratch",
        "scratch_bytes",
        "status",
    ] {
        let mut changed = table.clone();
        let value = changed[0][field].as_u64().unwrap();
        changed[0][field] = serde_json::json!(value + 8);
        assert!(
            !same_invocations(&original.invocations, &decode(changed)),
            "{field}"
        );
    }
}
