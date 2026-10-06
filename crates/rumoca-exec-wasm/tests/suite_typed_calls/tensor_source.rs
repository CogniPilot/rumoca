//! Unmodified full-domain Modelica source issued by the isolated producer.
use super::*;

fn literal_moments(values: &[f64], initial: f64, gain: f64) -> [f64; 3] {
    let mut moments = [initial, initial, -0.0];
    for value in values {
        moments[1] = moments[0];
        moments[0] += gain * value;
        moments[2] += value;
    }
    moments
}

fn source_inputs(
    values: &[f64],
    initial: f64,
    final_value: f64,
) -> Vec<Vec<solve::SolveValueKind>> {
    vec![
        values.iter().copied().map(real).collect(),
        vec![real(initial)],
        vec![real(final_value)],
    ]
}

fn check_source_variant(json: &str, source: &str, gain: f64) {
    let table = source_tables::issued_current(json, source);
    let owner = &table.owners()[0];
    let fold = owner
        .body()
        .operations()
        .iter()
        .find_map(|op| match op.operation() {
            solve::SolveOperation::Fold {
                domain, initial, ..
            } => Some((domain, initial)),
            _ => None,
        })
        .unwrap();
    assert_eq!(fold.0.scalar_count().unwrap(), 14400);
    assert!(
        fold.1
            .iter()
            .any(|r| owner.body().register_types()[r.index()].dimensions() == [3])
    );
    let site = owner.call_site();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    for (pattern, initial) in [
        ([1e16, 1.0, -1e16, 3.0], 7.25),
        ([-0.0; 4], -0.0),
        ([0.5, 1.0, 2.0, -4.0], 2.0),
    ] {
        let values = (0..14400).map(|i| pattern[i % 4]).collect::<Vec<_>>();
        let inputs = source_inputs(&values, initial, -8.3);
        let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
        assert_eq!(status, 0);
        let expected = literal_moments(&values, initial, gain);
        assert_eq!(actual, cells(expected.into_iter().chain([-9.0]).map(real)));
        assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
    }
    let values = vec![0.5; 14400];
    let invalid = source_inputs(&values, 7.25, f64::INFINITY);
    let failure = oracle(&table, &site, &invalid).unwrap_err();
    let (status, actual) = runner.run(&cells(invalid.iter().flatten().copied()));
    assert_eq!(actual, vec![0xa5; 32]);
    let fault = compiled
        .faults()
        .iter()
        .find(|f| f.status == status as u32)
        .unwrap();
    assert_eq!(fault.owner, site.owner());
    assert_eq!(fault.kind, TypedCallFaultKind::IntegerConversion);
    assert_eq!(failure.source_span(), Some(fault.provenance));
    let recovered = source_inputs(&values, 7.25, -8.3);
    let (status, actual) = runner.run(&cells(recovered.iter().flatten().copied()));
    assert_eq!(status, 0);
    assert_eq!(actual, oracle(&table, &site, &recovered).unwrap());
    assert!(compiled.layout().scratch_bytes < 14400 * 8 + 2048);
    assert!(compiled.module_bytes().len() < 4096);
    eprintln!(
        "SOURCE_TENSOR_FULL14400 gain={gain} module_bytes={} scratch_bytes={}",
        compiled.module_bytes().len(),
        compiled.layout().scratch_bytes
    );
}

#[test]
fn full_modelica_tensor_fold_source_edit_order_fault_atomicity_and_recovery() {
    let source = include_str!("OrderedTensorFrameFold.mo");
    check_source_variant(include_str!("tensor_tables/baseline.json"), source, 1.0);
    check_source_variant(
        include_str!("tensor_tables/gain-two.json"),
        &source.replace("gain = 1.0", "gain = 2.0"),
        2.0,
    );
}
