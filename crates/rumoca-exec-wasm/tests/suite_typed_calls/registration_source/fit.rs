//! Full-domain analytic geometry, independent of the source quaternion fit.
use super::*;
mod fixtures;
use fixtures::{Case, N, cases};

pub(super) fn check(
    table: &solve::SolvePureCallTable,
    site: &solve::SolvePureCallSite,
    compiled: &CompiledTypedCallWasm,
) {
    assert_eq!(site.inputs().len(), 7);
    assert_eq!(site.inputs()[0].dimensions(), [N as u32, 3]);
    assert_eq!(site.inputs()[1].dimensions(), [N as u32, 3]);
    assert_eq!(site.inputs()[2].dimensions(), [N as u32]);
    assert_eq!(compiled.layout().output_bytes, 26 * 8);
    let mut runner = Runner::new(compiled);
    let mut first = None;
    let filter = std::env::var("RUMOCA_NATIVE_REGISTRATION_CASE_FILTER").ok();
    let cases = cases();
    assert!(
        filter
            .as_ref()
            .is_none_or(|name| cases.iter().any(|case| case.name == name.as_str())),
        "unknown full-domain diagnostic case"
    );
    for case in cases.into_iter().filter(|case| {
        filter
            .as_ref()
            .is_none_or(|name| case.name == name.as_str())
    }) {
        let inputs = inputs(&case);
        let start = std::time::Instant::now();
        let (status, output) = runner.run(&cells(inputs.iter().flatten().copied()));
        let native_ms = start.elapsed().as_secs_f64() * 1000.0;
        assert_eq!(
            status, 0,
            "source refusal is a successful identity-valued result"
        );
        eprintln!(
            "HORN_NATIVE_CASE name={} native_ms={native_ms:.3}",
            case.name
        );
        let start = std::time::Instant::now();
        assert_eq!(
            output,
            oracle(table, site, &inputs).unwrap(),
            "canonical full source case {}",
            case.name
        );
        eprintln!(
            "HORN_CANONICAL_CASE name={} canonical_ms={:.3}",
            case.name,
            start.elapsed().as_secs_f64() * 1000.0
        );
        let values = output
            .chunks_exact(8)
            .map(|bytes| f64::from_le_bytes(bytes.try_into().unwrap()))
            .collect::<Vec<_>>();
        check_result(&case, &values);
        if case.name == "full non-axis rigid transform" {
            first = Some(output);
        } else if case.name == "recovery and repeat" {
            assert_eq!(Some(output), first);
        }
    }
}

fn inputs(case: &Case) -> Vec<Vec<solve::SolveValueKind>> {
    vec![
        case.source.iter().flatten().copied().map(real).collect(),
        case.target.iter().flatten().copied().map(real).collect(),
        case.enabled.iter().copied().map(real).collect(),
        vec![real(case.count)],
        vec![real(1e6)],
        vec![real(1e-8)],
        vec![real(case.maximum_rms)],
    ]
}

fn near(actual: f64, expected: f64, label: &str) {
    assert!(
        actual.is_finite() && (actual - expected).abs() <= 2e-8 * expected.abs().max(1.0),
        "{label}: {actual} != {expected}"
    );
}

fn check_result(case: &Case, values: &[f64]) {
    assert_eq!(values.len(), 26);
    assert_eq!(
        values[0],
        f64::from(u8::from(case.reason == 0)),
        "{} acceptance",
        case.name
    );
    assert_eq!(values[1], case.reason as f64, "{} refusal", case.name);
    let rotation = &values[2..11];
    for (actual, expected) in rotation.iter().zip(case.rotation) {
        near(*actual, expected, case.name);
    }
    for (actual, expected) in values[11..14].iter().zip(case.translation) {
        near(*actual, expected, case.name);
    }
    let determinant = rotation[0] * (rotation[4] * rotation[8] - rotation[5] * rotation[7])
        - rotation[1] * (rotation[3] * rotation[8] - rotation[5] * rotation[6])
        + rotation[2] * (rotation[3] * rotation[7] - rotation[4] * rotation[6]);
    near(determinant, 1.0, "proper determinant");
    check_orthogonality(rotation);
    assert_eq!(values[14], case.valid);
    assert_eq!(values[15], case.invalid);
    if let Some(rank) = case.rank {
        assert_eq!(values[16], rank);
    }
    if case.reason == 0 {
        check_accepted(case, values);
    }
}

fn check_orthogonality(rotation: &[f64]) {
    for row in 0..3 {
        for column in 0..3 {
            let dot = (0..3)
                .map(|k| rotation[3 * k + row] * rotation[3 * k + column])
                .sum::<f64>();
            near(
                dot,
                f64::from(u8::from(row == column)),
                "orthonormal rotation",
            );
        }
    }
}

fn check_accepted(case: &Case, values: &[f64]) {
    let mut source = [0.0; 3];
    let mut target = [0.0; 3];
    let mut cost = 0.0;
    for index in 0..N {
        if case.enabled[index] != 1.0 {
            continue;
        }
        for axis in 0..3 {
            source[axis] += case.source[index][axis];
            target[axis] += case.target[index][axis];
            let prediction = values[11 + axis]
                + (0..3)
                    .map(|k| values[2 + 3 * axis + k] * case.source[index][k])
                    .sum::<f64>();
            cost += (prediction - case.target[index][axis]).powi(2);
        }
    }
    for axis in 0..3 {
        near(
            values[20 + axis],
            source[axis] / case.valid,
            "source centroid",
        );
        near(
            values[23 + axis],
            target[axis] / case.valid,
            "target centroid",
        );
    }
    near(values[17], cost, "cost from independent point residuals");
    near(
        values[18],
        (cost / case.valid).sqrt(),
        "rms from independent point residuals",
    );
}
