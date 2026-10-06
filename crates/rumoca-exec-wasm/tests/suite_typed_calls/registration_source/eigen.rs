use super::*;

pub(super) fn check(
    table: &solve::SolvePureCallTable,
    site: &solve::SolvePureCallSite,
    compiled: &CompiledTypedCallWasm,
) {
    assert_eq!(site.inputs()[0].dimensions(), &[4, 4]);
    let mut runner = Runner::new(compiled);
    for (matrix, spectrum) in cases() {
        let inputs = vec![matrix.iter().copied().map(real).collect::<Vec<_>>()];
        let (status, output) = runner.run(&cells(inputs.iter().flatten().copied()));
        assert_eq!(status, 0);
        assert_eq!(
            output,
            oracle(table, site, &inputs).unwrap(),
            "full source-issued Eigen canonical arithmetic/iteration oracle"
        );
        let output = output
            .chunks_exact(8)
            .map(|bytes| f64::from_le_bytes(bytes.try_into().unwrap()))
            .collect::<Vec<_>>();
        let mut values = output[..4].to_vec();
        values.sort_by(|a, b| b.total_cmp(a));
        for (actual, expected) in values.iter().zip(spectrum) {
            assert!((actual - expected).abs() < 2e-12);
        }
        let vector = &output[4..8];
        let norm = vector.iter().map(|v| v * v).sum::<f64>();
        assert!((norm - 1.0).abs() < 2e-12);
        for row in 0..4 {
            let value = (0..4)
                .map(|column| matrix[4 * row + column] * vector[column])
                .sum::<f64>();
            assert!((value - spectrum[0] * vector[row]).abs() < 2e-12);
        }
        assert!((output[8] - (spectrum[0] - spectrum[1])).abs() < 2e-12);
        assert!(output[9] >= 0.0 && output[9] < 2e-12);
    }
}

fn cases() -> Vec<([f64; 16], [f64; 4])> {
    let diagonal = [
        4.0, 0.0, 0.0, 0.0, 0.0, 3.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0, 0.0, 1.0,
    ];
    let block = [
        2.0, 1.0, 0.0, 0.0, 1.0, 2.0, 0.0, 0.0, 0.0, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.25,
    ];
    let signs = [
        [1.0, 1.0, 1.0, 1.0],
        [1.0, -1.0, 1.0, -1.0],
        [1.0, 1.0, -1.0, -1.0],
        [1.0, -1.0, -1.0, 1.0],
    ];
    let spectrum = [6.0, 4.0, 2.0, 1.0];
    let mut hadamard = [0.0; 16];
    for row in 0..4 {
        for column in 0..4 {
            hadamard[4 * row + column] = (0..4)
                .map(|k| 0.25 * signs[row][k] * spectrum[k] * signs[column][k])
                .sum();
        }
    }
    vec![
        (diagonal, [4.0, 3.0, 2.0, 1.0]),
        (block, [3.0, 1.0, 0.5, 0.25]),
        (hadamard, spectrum),
        ([0.0; 16], [0.0; 4]),
    ]
}
