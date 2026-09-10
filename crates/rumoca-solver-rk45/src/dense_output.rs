use rumoca_solver::fmi_me::{MeIntegrationError, MeNumericalFailure};

const METHOD: &str = "rk45";

const DOPRI5_DENSE_COEFFICIENTS: [[f64; 4]; 7] = [
    [
        1.0,
        -8048581381.0 / 2820520608.0,
        8663915743.0 / 2820520608.0,
        -12715105075.0 / 11282082432.0,
    ],
    [0.0, 0.0, 0.0, 0.0],
    [
        0.0,
        131558114200.0 / 32700410799.0,
        -68118460800.0 / 10900136933.0,
        87487479700.0 / 32700410799.0,
    ],
    [
        0.0,
        -1754552775.0 / 470086768.0,
        14199869525.0 / 1410260304.0,
        -10690763975.0 / 1880347072.0,
    ],
    [
        0.0,
        127303824393.0 / 49829197408.0,
        -318862633887.0 / 49829197408.0,
        701980252875.0 / 199316789632.0,
    ],
    [
        0.0,
        -282668133.0 / 205662961.0,
        2019193451.0 / 616988883.0,
        -1453857185.0 / 822651844.0,
    ],
    [
        0.0,
        40617522.0 / 29380423.0,
        -110615467.0 / 29380423.0,
        69997945.0 / 29380423.0,
    ],
];

/// Fourth-order continuous extension for one accepted Dormand-Prince 5(4)
/// step. The seven stage derivatives are compressed into four polynomial
/// coefficients per state, so root probes require no derivative evaluations.
pub(super) struct Dopri5DenseOutput {
    t_start: f64,
    step: f64,
    start_state: Vec<f64>,
    polynomial: Vec<[f64; 4]>,
}

impl Dopri5DenseOutput {
    pub(super) fn new(
        t_start: f64,
        step: f64,
        start_state: &[f64],
        stages: [&[f64]; 7],
    ) -> Result<Self, MeIntegrationError> {
        if !t_start.is_finite() || !step.is_finite() || step <= 0.0 {
            return Err(contract_error(
                "DOPRI5 dense output requires finite time and a positive step",
            ));
        }
        for (index, stage) in stages.iter().enumerate() {
            if stage.len() != start_state.len() {
                return Err(contract_error(format!(
                    "DOPRI5 dense stage {index} length {} does not match state width {}",
                    stage.len(),
                    start_state.len()
                )));
            }
        }
        let mut polynomial =
            checked_vec_with_capacity(start_state.len(), "DOPRI5 dense coefficients")?;
        for state_index in 0..start_state.len() {
            polynomial.push(dense_coefficients_for_state(&stages, state_index));
        }
        let mut start_state_copy =
            checked_vec_with_capacity(start_state.len(), "DOPRI5 dense start state")?;
        start_state_copy.extend_from_slice(start_state);
        Ok(Self {
            t_start,
            step,
            start_state: start_state_copy,
            polynomial,
        })
    }

    pub(super) fn start_time(&self) -> f64 {
        self.t_start
    }

    pub(super) fn end_time(&self) -> f64 {
        self.t_start + self.step
    }

    #[cfg(test)]
    pub(super) fn evaluate(&self, time: f64) -> Result<Vec<f64>, MeIntegrationError> {
        let mut state = checked_vec_with_capacity(self.start_state.len(), "DOPRI5 dense state")?;
        state.resize(self.start_state.len(), 0.0);
        self.evaluate_into(time, &mut state)?;
        Ok(state)
    }

    pub(super) fn evaluate_into(
        &self,
        time: f64,
        state: &mut [f64],
    ) -> Result<(), MeIntegrationError> {
        if !time.is_finite() {
            return Err(contract_error("DOPRI5 dense output time must be finite"));
        }
        if state.len() != self.start_state.len() {
            return Err(contract_error(format!(
                "DOPRI5 dense output buffer has {} states for width {}",
                state.len(),
                self.start_state.len()
            )));
        }
        let theta = ((time - self.t_start) / self.step).clamp(0.0, 1.0);
        for ((slot, start), coefficients) in state
            .iter_mut()
            .zip(&self.start_state)
            .zip(&self.polynomial)
        {
            let [q1, q2, q3, q4] = *coefficients;
            let weighted = theta * (q1 + theta * (q2 + theta * (q3 + theta * q4)));
            *slot = start + self.step * weighted;
        }
        Ok(())
    }
}

fn checked_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
) -> Result<Vec<T>, MeIntegrationError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(capacity)
        .map_err(|_| MeIntegrationError::Allocation {
            context,
            entries: capacity,
        })?;
    Ok(values)
}

fn contract_error(reason: impl Into<String>) -> MeIntegrationError {
    MeIntegrationError::numerical(METHOD, MeNumericalFailure::Construction, reason)
}

fn dense_coefficients_for_state(stages: &[&[f64]; 7], state_index: usize) -> [f64; 4] {
    let mut coefficients = [0.0; 4];
    for (stage, weights) in stages.iter().zip(DOPRI5_DENSE_COEFFICIENTS) {
        for (coefficient, weight) in coefficients.iter_mut().zip(weights) {
            *coefficient += stage[state_index] * weight;
        }
    }
    coefficients
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dense_output_matches_constant_derivative_throughout_step() {
        let stages = [
            [2.0_f64],
            [2.0_f64],
            [2.0_f64],
            [2.0_f64],
            [2.0_f64],
            [2.0_f64],
            [2.0_f64],
        ];
        let dense = Dopri5DenseOutput::new(
            1.0,
            0.5,
            &[3.0],
            stages.each_ref().map(|stage| stage.as_slice()),
        )
        .expect("valid dense output");

        assert!((dense.evaluate(1.0).unwrap()[0] - 3.0).abs() < 1.0e-14);
        assert!((dense.evaluate(1.25).unwrap()[0] - 3.5).abs() < 1.0e-14);
        assert!((dense.evaluate(1.5).unwrap()[0] - 4.0).abs() < 1.0e-14);
    }

    #[test]
    fn dense_output_endpoint_matches_dopri5_fifth_order_weights() {
        let stages = [
            [1.0_f64],
            [2.0_f64],
            [3.0_f64],
            [4.0_f64],
            [5.0_f64],
            [6.0_f64],
            [7.0_f64],
        ];
        let step = 0.25;
        let dense = Dopri5DenseOutput::new(
            0.0,
            step,
            &[2.0],
            stages.each_ref().map(|stage| stage.as_slice()),
        )
        .expect("valid dense output");
        let expected = 2.0
            + step
                * (35.0 / 384.0 * stages[0][0]
                    + 500.0 / 1113.0 * stages[2][0]
                    + 125.0 / 192.0 * stages[3][0]
                    - 2187.0 / 6784.0 * stages[4][0]
                    + 11.0 / 84.0 * stages[5][0]);

        assert!((dense.evaluate(step).unwrap()[0] - expected).abs() < 1.0e-13);
    }
}
