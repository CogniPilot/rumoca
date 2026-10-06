//! Exact source-issued fixture wires; integer bit patterns never pass JS Number.
use super::*;

pub(super) fn issued_revision(
    json: &str,
    source: &str,
    revision: &str,
) -> solve::SolvePureCallTable {
    let mut wire: serde_json::Value = serde_json::from_str(json).unwrap();
    assert_eq!(wire["source"].as_str().unwrap(), source);
    assert_eq!(wire["compilerRevision"], revision);
    let table: solve::SolvePureCallTable = serde_json::from_value(wire["table"].take()).unwrap();
    assert_eq!(table.owners().len(), 1);
    assert_eq!(table.owners()[0].inputs()[0].dimensions(), &[14400]);
    table
}

pub(super) fn issued_current(json: &str, source: &str) -> solve::SolvePureCallTable {
    let wire: serde_json::Value = serde_json::from_str(json).unwrap();
    assert_eq!(
        wire["compilerSourceSha256"],
        "a88161891ff1851c7586dd71a7c3e834123fcc7c0926b7bdc9bee5d97132819b"
    );
    assert_eq!(
        wire["producerBinarySha256"],
        "262ee681e0cdb87b499ee8907508d8ea77c9640caa2b480bb3eda782886d1457"
    );
    issued_revision(
        json,
        source,
        "14694fd7e4369b775b15cd2dafc86f0b06bd4608-dirty",
    )
}

fn legacy_wire_is_refused(json: &str, source: &str) {
    let mut wire: serde_json::Value = serde_json::from_str(json).unwrap();
    assert_eq!(wire["source"], source);
    assert_eq!(wire["compilerRevision"], "943aa7d7b745");
    let error = serde_json::from_value::<solve::SolvePureCallTable>(wire["table"].take())
        .expect_err("legacy wire must not receive missing recursion defaults");
    assert_eq!(error.to_string(), "missing field `recursion`");
}

fn arguments(values: &[f64], initial: f64, final_value: f64) -> Vec<Vec<solve::SolveValueKind>> {
    vec![
        values.iter().copied().map(real).collect(),
        vec![real(initial)],
        vec![real(final_value)],
    ]
}

fn real_output(bytes: &[u8]) -> f64 {
    f64::from_le_bytes(bytes[..8].try_into().unwrap())
}

#[test]
fn historical_source_wires_are_refused_and_current_dyadic_recurrence_is_exact() {
    let source = include_str!("OrderedFrameFold.mo");
    for (legacy, current, source, gain, subtract) in [
        (
            include_str!("source_tables/baseline.json"),
            include_str!("fixed_tables/baseline.json"),
            source.to_owned(),
            1.0,
            false,
        ),
        (
            include_str!("source_tables/gain-two.json"),
            include_str!("fixed_tables/gain-two.json"),
            source.replace("gain = 1.0", "gain = 2.0"),
            2.0,
            false,
        ),
        (
            include_str!("source_tables/subtract.json"),
            include_str!("fixed_tables/subtract.json"),
            source.replace("total + gain*values", "total - gain*values"),
            1.0,
            true,
        ),
    ] {
        legacy_wire_is_refused(legacy, &source);
        let table = issued_current(current, &source);
        let site = table.owners()[0].call_site();
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let mut runner = Runner::new(&compiled);
        let values = (0..14400)
            .map(|i| (i % 17) as f64 / 8.0 - 1.0)
            .collect::<Vec<_>>();
        let expected = values.iter().fold(7.25, |sum, value| {
            if subtract {
                sum - gain * value
            } else {
                sum + gain * value
            }
        });
        let inputs = arguments(&values, 7.25, -8.3);
        let actual = runner.run(&cells(inputs.iter().flatten().copied()));
        assert_eq!(actual, (0, cells([real(expected), real(-9.0)])));
        assert_eq!(actual.1, oracle(&table, &site, &inputs).unwrap());
    }
}

#[test]
fn historical_seed_reassociation_counterexamples_remain_explicit() {
    legacy_wire_is_refused(
        include_str!("source_tables/baseline.json"),
        include_str!("OrderedFrameFold.mo"),
    );
    let table = issued_current(
        include_str!("fixed_tables/baseline.json"),
        include_str!("OrderedFrameFold.mo"),
    );
    let site = table.owners()[0].call_site();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let values = (0..14400)
        .map(|i| [1e16, 1.0, -1e16, 3.0][i % 4])
        .collect::<Vec<_>>();
    let source_order: f64 = values.iter().fold(7.25, |sum, value| sum + value);
    let historical_reassociated: f64 = values
        .iter()
        .skip(1)
        .fold(values[0], |sum, value| sum + value)
        + 7.25;
    assert_ne!(source_order.to_bits(), historical_reassociated.to_bits());
    let inputs = arguments(&values, 7.25, 0.0);
    let actual = runner.run(&cells(inputs.iter().flatten().copied()));
    assert_eq!(actual, (0, cells([real(source_order), real(0.0)])));
    assert_eq!(actual.1, oracle(&table, &site, &inputs).unwrap());
    let zeros = vec![-0.0; 14400];
    let zero = real_output(&oracle(&table, &site, &arguments(&zeros, -0.0, 0.0)).unwrap());
    assert_eq!(zero.to_bits(), (-0.0f64).to_bits());
    let mut overflow = vec![0.0; 14400];
    overflow[0] = f64::MAX;
    overflow[1] = -f64::MAX;
    assert_eq!(
        overflow.iter().fold(f64::MAX, |sum, value| sum + value),
        f64::INFINITY
    );
    assert_eq!(
        overflow
            .iter()
            .skip(1)
            .fold(overflow[0], |sum, value| sum + value)
            + f64::MAX,
        f64::MAX
    );
    let actual = real_output(&oracle(&table, &site, &arguments(&overflow, f64::MAX, 0.0)).unwrap());
    assert_eq!(actual, f64::INFINITY);
}

#[test]
fn historical_integer_reassociation_overflow_is_refused_and_current_source_recovers() {
    legacy_wire_is_refused(
        include_str!("integer_tables/baseline.json"),
        include_str!("IntegerSeedFold.mo"),
    );
    let table = issued_current(
        include_str!("fixed_integer_tables/baseline.json"),
        include_str!("IntegerSeedFold.mo"),
    );
    let site = table.owners()[0].call_site();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut values = vec![0i64; 14400];
    values[0] = i64::MAX;
    values[1] = 1;
    assert_eq!(
        values
            .iter()
            .try_fold(-i64::MAX, |sum, value| sum.checked_add(*value)),
        Some(1)
    );
    assert_eq!(
        values
            .iter()
            .skip(1)
            .try_fold(values[0], |sum, value| sum.checked_add(*value)),
        None
    );
    let inputs = vec![
        values
            .into_iter()
            .map(solve::SolveValueKind::Integer)
            .collect::<Vec<_>>(),
        vec![solve::SolveValueKind::Integer(-i64::MAX)],
    ];
    let actual = Runner::new(&compiled).run(&cells(inputs.iter().flatten().copied()));
    assert_eq!(actual, (0, cells([solve::SolveValueKind::Integer(1)])));
    assert_eq!(actual.1, oracle(&table, &site, &inputs).unwrap());
}

#[test]
fn fixed_full_modelica_source_owner_executes_exact_recurrence_in_wasmi() {
    let original = include_str!("OrderedFrameFold.mo");
    for (json, source, gain, subtract) in [
        (
            include_str!("fixed_tables/baseline.json"),
            original.to_owned(),
            1.0,
            false,
        ),
        (
            include_str!("fixed_tables/gain-two.json"),
            original.replace("gain = 1.0", "gain = 2.0"),
            2.0,
            false,
        ),
        (
            include_str!("fixed_tables/subtract.json"),
            original.replace("total + gain*values", "total - gain*values"),
            1.0,
            true,
        ),
    ] {
        let table = issued_current(json, &source);
        let owner = &table.owners()[0];
        assert_eq!(
            owner
                .body()
                .operations()
                .iter()
                .filter(|op| matches!(op.operation(), solve::SolveOperation::Fold { .. }))
                .count(),
            1
        );
        assert!(!owner.body().operations().iter().any(|op| matches!(
            op.operation(),
            solve::SolveOperation::Map { .. } | solve::SolveOperation::Reduce { .. }
        )));
        let site = owner.call_site();
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        assert_eq!(compiled.layout().input_bytes, (14400 + 2) * 8);
        assert!(compiled.module_bytes().len() < 6000);
        fixed_source_cases(&table, &compiled, gain, subtract);
        eprintln!(
            "SOURCE_NATIVE_FULL source_edit_gain={gain} subtract={subtract} wasm_bytes={} scratch_bytes={}",
            compiled.module_bytes().len(),
            compiled.layout().scratch_bytes
        );
    }
}

fn fixed_source_cases(
    table: &solve::SolvePureCallTable,
    compiled: &CompiledTypedCallWasm,
    gain: f64,
    subtract: bool,
) {
    let site = table.owners()[0].call_site();
    let mut runner = Runner::new(compiled);
    for (values, initial) in [
        (
            (0..14400)
                .map(|i| (i % 17) as f64 / 8.0 - 1.0)
                .collect::<Vec<_>>(),
            7.25,
        ),
        (
            (0..14400).map(|i| [1e16, 1.0, -1e16, 3.0][i % 4]).collect(),
            7.25,
        ),
        (vec![-0.0; 14400], -0.0),
    ] {
        let expected = values.iter().fold(initial, |sum, value| {
            if subtract {
                sum - gain * value
            } else {
                sum + gain * value
            }
        });
        let inputs = arguments(&values, initial, -8.3);
        let bytes = cells(inputs.iter().flatten().copied());
        let (status, actual) = runner.run(&bytes);
        assert_eq!(status, 0);
        assert_eq!(actual, oracle(table, &site, &inputs).unwrap());
        assert_eq!(actual, cells([real(expected), real(-9.0)]));
        let bad = arguments(&values, initial, f64::INFINITY);
        let failure = oracle(table, &site, &bad).unwrap_err();
        let (status, output) = runner.run(&cells(bad.iter().flatten().copied()));
        assert_ne!(status, 0);
        assert_eq!(output, vec![0xa5; compiled.layout().output_bytes as usize]);
        assert_eq!(
            compiled
                .faults()
                .iter()
                .find(|fault| fault.status == status as u32)
                .unwrap()
                .provenance,
            failure.source_span().unwrap()
        );
        assert_eq!(runner.run(&bytes), (0, actual));
    }
}

#[test]
fn fixed_native_modelica_seed_overflow_retains_source_ieee_result() {
    let table = issued_current(
        include_str!("fixed_tables/baseline.json"),
        include_str!("OrderedFrameFold.mo"),
    );
    let site = table.owners()[0].call_site();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut values = vec![0.0; 14400];
    values[0] = f64::MAX;
    values[1] = -f64::MAX;
    let expected = values.iter().fold(f64::MAX, |sum, value| sum + value);
    assert_eq!(expected, f64::INFINITY);
    let inputs = arguments(&values, f64::MAX, 0.0);
    let actual = Runner::new(&compiled).run(&cells(inputs.iter().flatten().copied()));
    assert_eq!(actual, (0, cells([real(expected), real(0.0)])));
    assert_eq!(actual.1, oracle(&table, &site, &inputs).unwrap());
}

#[test]
fn fixed_full_modelica_integer_fold_preserves_checked_order_atomic_fault_and_recovery() {
    let json = include_str!("fixed_integer_tables/baseline.json");
    let table = issued_current(json, include_str!("IntegerSeedFold.mo"));
    let owner = &table.owners()[0];
    assert!(
        owner
            .body()
            .operations()
            .iter()
            .any(|op| matches!(op.operation(), solve::SolveOperation::Fold { .. }))
    );
    assert!(!owner.body().operations().iter().any(|op| matches!(
        op.operation(),
        solve::SolveOperation::Map { .. } | solve::SolveOperation::Reduce { .. }
    )));
    let site = owner.call_site();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let mut values = vec![0i64; 14400];
    values[0] = i64::MAX;
    values[1] = 1;
    for initial in [-i64::MAX, 0, -i64::MAX] {
        let expected = values
            .iter()
            .try_fold(initial, |sum, value| sum.checked_add(*value));
        let inputs = vec![
            values
                .iter()
                .copied()
                .map(solve::SolveValueKind::Integer)
                .collect(),
            vec![solve::SolveValueKind::Integer(initial)],
        ];
        let actual = runner.run(&cells(inputs.iter().flatten().copied()));
        match expected {
            Some(value) => {
                assert_eq!(actual, (0, cells([solve::SolveValueKind::Integer(value)])));
                assert_eq!(actual.1, oracle(&table, &site, &inputs).unwrap());
            }
            None => {
                let failure = oracle(&table, &site, &inputs).unwrap_err();
                assert_ne!(actual.0, 0);
                assert_eq!(
                    actual.1,
                    vec![0xa5; compiled.layout().output_bytes as usize]
                );
                let fault = compiled
                    .faults()
                    .iter()
                    .find(|f| f.status == actual.0 as u32)
                    .unwrap();
                assert_eq!(fault.kind, TypedCallFaultKind::IntegerArithmetic);
                assert_eq!(Some(fault.provenance), failure.source_span());
                assert!(!fault.region_path.is_empty());
            }
        }
    }
    eprintln!(
        "SOURCE_NATIVE_INTEGER_FULL wasm_bytes={} scratch_bytes={} input_bytes={}",
        compiled.module_bytes().len(),
        compiled.layout().scratch_bytes,
        compiled.layout().input_bytes
    );
}
