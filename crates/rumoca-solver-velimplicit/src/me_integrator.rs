//! Fixed-step linearly implicit Euler behind the solver-neutral FMI ME plugin
//! contract.
//!
//! One step from `x_n` is `x_{n+1} = x_n + dx` with `(I - h J) dx = h f(t_n, x_n)`.
//! For a mechanical model whose states are positions `q` and velocities `v`
//! with `q' = v`, the velocity rows of this step are the implicit-in-velocity
//! step `(M - h dF/dv) dv = h F` that MuJoCo's implicit integrators take, and
//! the position rows additionally treat position stiffness (contact springs)
//! implicitly. It is this full-state, MuJoCo-style step, not a velocity-only
//! equivalent, and it needs no position/velocity split, so any ME component
//! works.
//!
//! The Jacobian is built from forward-mode directional derivatives, one per
//! colour of the component's structural state-Jacobian pattern, and reused for
//! `jacobian_reuse` steps; it is refreshed after every initialize and
//! truncate/reset (events, reinit), so a step never crosses a structural change
//! with a stale matrix. The continuous extension is linear (order 1). A
//! component that refuses the accepted start point gets its typed discard back
//! at once: no evaluation of this method lies elsewhere, so retrying with a
//! smaller step could not succeed.
//!
//! # References
//!
//! Linearly implicit (Rosenbrock-Euler) method: E. Hairer and G. Wanner,
//! "Solving Ordinary Differential Equations II: Stiff and Differential-Algebraic
//! Problems", 2nd rev. ed., Springer 1996, section IV.7 (W-methods, which allow
//! an approximate, reused Jacobian at first order).

use rumoca_solver::fmi_me::{
    MeAdvanceRequest, MeContinuousPoint, MeDerivativeHandle, MeIntegrationError,
    MeIntegratorBackend, MeNumericalFailure, MeNumericalSetup, MeStepCandidate,
    accepted_interval_contains,
};

use crate::linear::DenseLu;

const METHOD: &str = "velimplicit";
const CONTINUOUS_EXTENSION_ORDER: u32 = 1;

/// Fixed step and Jacobian reuse of the velocity-implicit plugin.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct VelImplicitConfig {
    /// The fixed internal step `h` in seconds.
    pub step: f64,
    /// Refresh the Jacobian every this many accepted steps (at least 1).
    pub jacobian_reuse: u32,
}

impl Default for VelImplicitConfig {
    fn default() -> Self {
        Self {
            step: 1.0e-3,
            jacobian_reuse: 200,
        }
    }
}

/// Build the velocity-implicit numerical plugin accepted by the common FMI ME
/// host.
#[must_use]
pub fn model_exchange_integrator(
    setup: MeNumericalSetup,
    config: VelImplicitConfig,
) -> Box<dyn MeIntegratorBackend + 'static> {
    Box::new(VelImplicitIntegrator::new(setup, config))
}

struct VelImplicitIntegrator {
    width: usize,
    step: f64,
    jacobian_reuse: u32,
    derivatives: Option<MeDerivativeHandle>,
    /// Column groups whose structural row sets are disjoint.
    colours: Vec<Vec<usize>>,
    /// Structural rows of each state column; `None` means dense.
    pattern: Option<Vec<Vec<usize>>>,
    /// Row-major state Jacobian, valid while `jacobian_valid`.
    jacobian: Vec<f64>,
    jacobian_valid: bool,
    steps_since_refresh: u32,
    /// Factors of `I - h J` for `factored_step`.
    lu: DenseLu,
    factored_step: Option<f64>,
    work: Work,
    accepted_interval: Option<LinearInterval>,
}

#[derive(Default)]
struct Work {
    derivative: Vec<f64>,
    rhs: Vec<f64>,
    increment: Vec<f64>,
    seed: Vec<f64>,
    column: Vec<f64>,
    matrix: Vec<f64>,
}

struct LinearInterval {
    start_time: f64,
    end_time: f64,
    start: Vec<f64>,
    end: Vec<f64>,
}

impl VelImplicitIntegrator {
    fn new(setup: MeNumericalSetup, config: VelImplicitConfig) -> Self {
        Self {
            width: setup.state_nominals().len(),
            step: config.step,
            jacobian_reuse: config.jacobian_reuse.max(1),
            derivatives: None,
            colours: Vec::new(),
            pattern: None,
            jacobian: Vec::new(),
            jacobian_valid: false,
            steps_since_refresh: 0,
            lu: DenseLu::default(),
            factored_step: None,
            work: Work::default(),
            accepted_interval: None,
        }
    }

    /// Greedy colouring of the structural pattern: columns sharing no row can
    /// be seeded together in one directional derivative.
    fn colour_columns(&mut self) {
        let width = self.width;
        self.colours.clear();
        let Some(pattern) = self.pattern.as_ref() else {
            self.colours.extend((0..width).map(|column| vec![column]));
            return;
        };
        let mut used_rows: Vec<Vec<bool>> = Vec::new();
        for (column, rows) in pattern.iter().enumerate() {
            let slot = used_rows
                .iter()
                .position(|used| rows.iter().all(|&row| !used[row]));
            let slot = slot.unwrap_or_else(|| {
                used_rows.push(vec![false; width]);
                self.colours.push(Vec::new());
                used_rows.len() - 1
            });
            for &row in rows {
                used_rows[slot][row] = true;
            }
            self.colours[slot].push(column);
        }
    }

    fn refresh_jacobian(&mut self, time: f64, state: &[f64]) -> Result<(), MeIntegrationError> {
        let width = self.width;
        let derivatives = self.derivatives.as_ref().ok_or_else(missing_handle)?;
        let Work { seed, column, .. } = &mut self.work;
        self.jacobian.clear();
        self.jacobian.resize(width * width, 0.0);
        for colour in &self.colours {
            seed.fill(0.0);
            colour
                .iter()
                .for_each(|&state_column| seed[state_column] = 1.0);
            derivatives.directional_derivative_into(time, state, seed, column);
            if derivatives.take_discard() {
                return Err(MeIntegrationError::ComponentDiscard { time });
            }
            if derivatives.has_failed() {
                return Err(MeIntegrationError::DerivativeRefused);
            }
            for &state_column in colour {
                let rows = self
                    .pattern
                    .as_ref()
                    .map(|pattern| pattern[state_column].as_slice());
                scatter_column(&mut self.jacobian, width, state_column, rows, column);
            }
        }
        self.jacobian_valid = true;
        self.steps_since_refresh = 0;
        self.factored_step = None;
        Ok(())
    }

    fn factor(&mut self, step: f64) -> Result<(), MeIntegrationError> {
        let width = self.width;
        let matrix = &mut self.work.matrix;
        matrix.clear();
        matrix.extend(self.jacobian.iter().map(|entry| -step * entry));
        for diagonal in 0..width {
            matrix[diagonal * width + diagonal] += 1.0;
        }
        if !self.lu.factor(matrix, width) {
            return Err(MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::LinearAlgebra,
                "the iteration matrix I - h J is singular",
            ));
        }
        self.factored_step = Some(step);
        Ok(())
    }

    /// One trial step of length `step`; the endpoint lands in `work.rhs`.
    fn trial_step(
        &mut self,
        time: f64,
        state: &[f64],
        step: f64,
    ) -> Result<(), MeIntegrationError> {
        let derivatives = self.derivatives.as_ref().ok_or_else(missing_handle)?;
        derivatives.derivatives_into(time, state, &mut self.work.derivative);
        if derivatives.take_discard() {
            return Err(MeIntegrationError::ComponentDiscard { time });
        }
        if derivatives.has_failed() {
            return Err(MeIntegrationError::DerivativeRefused);
        }
        if !self.jacobian_valid || self.steps_since_refresh >= self.jacobian_reuse {
            self.refresh_jacobian(time, state)?;
        }
        if self
            .factored_step
            .is_none_or(|factored| factored.to_bits() != step.to_bits())
        {
            self.factor(step)?;
        }
        let Work {
            derivative,
            rhs,
            increment,
            ..
        } = &mut self.work;
        for (value, derivative) in rhs.iter_mut().zip(derivative.iter()) {
            *value = step * derivative;
        }
        self.lu.solve_into(rhs, increment);
        for ((endpoint, start), delta) in rhs.iter_mut().zip(state).zip(increment.iter()) {
            *endpoint = start + delta;
        }
        if rhs.iter().any(|value| !value.is_finite()) {
            return Err(MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::AdvanceExhausted,
                "the linearly implicit step produced a non-finite state",
            ));
        }
        Ok(())
    }

    /// Clip `step` to the request's bounds, or report underflow.
    fn bounded_step(
        &self,
        request: &MeAdvanceRequest,
        step: f64,
    ) -> Result<f64, MeIntegrationError> {
        let now = request.current().time();
        let latest = request.latest_accepted_time();
        representable_step(now, latest, request.max_step_duration(), step).ok_or(
            MeIntegrationError::StepSizeUnderflow {
                method: METHOD,
                from_time: now,
                to_time: latest,
            },
        )
    }
}

impl MeIntegratorBackend for VelImplicitIntegrator {
    fn continuous_extension_order(&self) -> Option<u32> {
        Some(CONTINUOUS_EXTENSION_ORDER)
    }

    fn initialize(
        &mut self,
        point: &MeContinuousPoint,
        derivatives: MeDerivativeHandle,
    ) -> Result<(), MeIntegrationError> {
        if derivatives.state_count() != point.width() || self.width != point.width() {
            return Err(MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Construction,
                "numerical setup, derivative source, and component state widths differ",
            ));
        }
        let width = self.width;
        self.pattern = derivatives
            .state_jacobian_columns()
            .filter(|columns| {
                columns.len() == width && columns.iter().flatten().all(|&row| row < width)
            })
            .map(<[Vec<usize>]>::to_vec);
        self.colour_columns();
        for buffer in [
            &mut self.work.derivative,
            &mut self.work.rhs,
            &mut self.work.increment,
            &mut self.work.seed,
            &mut self.work.column,
        ] {
            buffer.clear();
            buffer.resize(width, 0.0);
        }
        self.derivatives = Some(derivatives);
        self.jacobian_valid = false;
        self.factored_step = None;
        self.accepted_interval = None;
        Ok(())
    }

    fn advance(
        &mut self,
        request: &MeAdvanceRequest,
    ) -> Result<MeStepCandidate, MeIntegrationError> {
        let step = self.bounded_step(request, self.step)?;
        let start = request.current();
        // Every evaluation of a step (the RHS and, when due, the Jacobian) is
        // at the accepted start point, so a component that refuses it would
        // refuse any smaller step too: the refusal is returned at once as the
        // typed discard, with nothing committed (the accepted interval and
        // step count are untouched) and the Jacobian marked for rebuild.
        if let Err(error) = self.trial_step(start.time(), start.states(), step) {
            if matches!(error, MeIntegrationError::ComponentDiscard { .. }) {
                self.jacobian_valid = false;
            }
            return Err(error);
        }
        self.steps_since_refresh += 1;
        let end_time = start.time() + step;
        let end = self.work.rhs.clone();
        self.accepted_interval = Some(LinearInterval {
            start_time: start.time(),
            end_time,
            start: start.states().to_vec(),
            end: end.clone(),
        });
        Ok(MeStepCandidate::new(
            end_time,
            end,
            CONTINUOUS_EXTENSION_ORDER,
        ))
    }

    fn sample(&self, time: f64, states: &mut [f64]) -> Result<(), MeIntegrationError> {
        let interval = self.accepted_interval.as_ref().ok_or_else(|| {
            MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Interpolation,
                "no accepted interval is available",
            )
        })?;
        if states.len() != self.width {
            return Err(MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Interpolation,
                "the sampler output width differs from the initialized component width",
            ));
        }
        if !accepted_interval_contains(interval.start_time, interval.end_time, time) {
            return Err(MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Interpolation,
                format!(
                    "sample time {time} lies outside [{}, {}]",
                    interval.start_time, interval.end_time
                ),
            ));
        }
        let span = interval.end_time - interval.start_time;
        let theta = if span > 0.0 {
            ((time - interval.start_time) / span).clamp(0.0, 1.0)
        } else {
            1.0
        };
        for ((out, start), end) in states.iter_mut().zip(&interval.start).zip(&interval.end) {
            *out = start + theta * (end - start);
        }
        Ok(())
    }

    fn truncate_reset(&mut self, point: &MeContinuousPoint) -> Result<(), MeIntegrationError> {
        if point.width() != self.width {
            return Err(MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Reset,
                "the reset point width differs from the initialized component width",
            ));
        }
        self.accepted_interval = None;
        self.jacobian_valid = false;
        Ok(())
    }
}

fn missing_handle() -> MeIntegrationError {
    MeIntegrationError::numerical(
        METHOD,
        MeNumericalFailure::Construction,
        "the host has not initialized the derivative capability",
    )
}

/// `step` clipped to `latest - now` and `max_step`, or `None` when it is not
/// positive and finite or `now + step` does not advance the coordinate.
fn representable_step(now: f64, latest: f64, max_step: Option<f64>, step: f64) -> Option<f64> {
    let bounded = max_step.map_or(step, |max| step.min(max)).min(latest - now);
    (bounded.is_finite() && bounded > 0.0 && now + bounded > now).then_some(bounded)
}

/// Write one Jacobian column from a directional derivative: only its
/// structural rows when a pattern is known (the seed may have mixed several
/// columns into the other rows), every row otherwise.
fn scatter_column(
    jacobian: &mut [f64],
    width: usize,
    state_column: usize,
    rows: Option<&[usize]>,
    derivative: &[f64],
) {
    match rows {
        Some(rows) => rows
            .iter()
            .for_each(|&row| jacobian[row * width + state_column] = derivative[row]),
        None => (0..width).for_each(|row| jacobian[row * width + state_column] = derivative[row]),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn integrator(width: usize) -> VelImplicitIntegrator {
        let setup = MeNumericalSetup::new(1.0e-6, 1.0e-9, vec![1.0; width], width, None)
            .expect("fixture setup is checked");
        VelImplicitIntegrator::new(setup, VelImplicitConfig::default())
    }

    #[test]
    fn the_continuous_extension_is_declared_first_order() {
        assert_eq!(integrator(2).continuous_extension_order(), Some(1));
    }

    #[test]
    fn a_jacobian_before_initialize_reports_the_missing_derivative_capability() {
        let mut backend = integrator(1);
        let error = backend
            .refresh_jacobian(0.0, &[1.0])
            .expect_err("no derivative capability before initialize");
        assert_eq!(error.to_string(), missing_handle().to_string());
    }

    #[test]
    fn a_step_that_cannot_advance_the_coordinate_is_refused() {
        // At t = 1e6 a 2.5e-11 s step rounds back to t.
        assert_eq!(representable_step(1.0e6, 2.0e6, None, 2.5e-11), None);
        assert_eq!(representable_step(1.0, 2.0, None, 0.0), None);
        assert_eq!(representable_step(1.0, 1.5, Some(0.1), 1.0), Some(0.1));
        assert_eq!(representable_step(1.0, 1.05, None, 1.0), Some(1.05 - 1.0));
    }

    #[test]
    fn a_structural_column_writes_only_its_rows() {
        let mut jacobian = vec![0.0; 4];
        scatter_column(&mut jacobian, 2, 1, Some(&[1]), &[3.0, 5.0]);
        assert_eq!(jacobian, vec![0.0, 0.0, 0.0, 5.0]);
        scatter_column(&mut jacobian, 2, 0, None, &[3.0, 5.0]);
        assert_eq!(jacobian, vec![3.0, 0.0, 5.0, 5.0]);
    }

    #[test]
    fn disjoint_columns_share_a_colour() {
        let mut backend = integrator(4);
        backend.pattern = Some(vec![vec![0], vec![1], vec![0, 2], vec![3]]);
        backend.colour_columns();
        assert_eq!(backend.colours, vec![vec![0, 1, 3], vec![2]]);
    }

    #[test]
    fn without_a_pattern_every_column_is_its_own_colour() {
        let mut backend = integrator(3);
        backend.colour_columns();
        assert_eq!(backend.colours, vec![vec![0], vec![1], vec![2]]);
    }

    #[test]
    fn the_step_is_unconditionally_stable_on_a_stiff_decay() {
        // x' = -k x with k h = 1000: explicit Euler explodes, the implicit step
        // contracts by 1 / (1 + k h).
        let mut backend = integrator(1);
        backend.jacobian = vec![-1.0e6];
        backend.jacobian_valid = true;
        backend.factor(1.0e-3).expect("regular");
        let mut out = [0.0];
        backend.lu.solve_into(&[1.0e-3 * -1.0e6], &mut out);
        let next = 1.0 + out[0];
        assert!((next - 1.0 / 1001.0).abs() < 1.0e-15);
    }

    #[test]
    fn the_default_is_the_gated_configuration() {
        assert_eq!(
            VelImplicitConfig::default(),
            VelImplicitConfig {
                step: 1.0e-3,
                jacobian_reuse: 200,
            }
        );
    }
}
