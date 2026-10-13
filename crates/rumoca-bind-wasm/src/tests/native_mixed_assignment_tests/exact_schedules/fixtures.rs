//! Full owned vectors: the issued initial point and deterministic variations.
use super::*;

pub(super) struct Case {
    pub(super) y: Vec<f64>,
    pub(super) p: Vec<f64>,
    pub(super) time: f64,
}

pub(super) fn cases(model: &solve::SolveModel) -> Vec<Case> {
    let varied = |scale: f64, time: f64| Case {
        y: model
            .initial_y
            .iter()
            .enumerate()
            .map(|(index, value)| value + scale * (index as f64 + 1.0))
            .collect(),
        p: model.parameters.to_vec(),
        time,
    };
    let cases = vec![varied(0.0, 0.0), varied(0.25, 0.5), varied(-0.75, 3.0)];
    for case in &cases {
        assert_eq!(case.y.len(), model.problem.layout.y_scalars());
        assert_eq!(case.p.len(), model.problem.layout.p_scalars());
        assert!(case.y.iter().chain(&case.p).all(|v| v.is_finite()));
    }
    cases
}

pub(super) fn bytes(values: &[f64]) -> Vec<u8> {
    values
        .iter()
        .flat_map(|value| value.to_le_bytes())
        .collect()
}
