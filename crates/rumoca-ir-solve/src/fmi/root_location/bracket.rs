//! The arithmetic of [`RootRefinementMethod::IllinoisMinmax`].
//!
//! Every executor narrows a state-event bracket through these operations, and
//! the generated C locator (`templates/fmi3/scalar_events.jinja`,
//! `root_refinement`) repeats them operation for operation, so both locate the
//! same coordinates from the same indicator values.
//!
//! Correctness does not depend on the trial point. The caller keeps the
//! bracket invariant: at `low` no changed indicator has left its domain, at
//! `high` at least one has, and [`RootBracket::narrow`] moves exactly the end
//! the evaluation at the trial decides. Every trial lies strictly inside the
//! bracket, so a located bracket is no wider than the tolerance and still holds
//! the earliest crossing whenever each changed indicator crosses once in it.
//!
//! The evaluation count does not depend on the indicators. With `n` the least
//! count for which `tolerance * 2^n >= high - low` (the bisection count) and
//! `s` the plan's slack, the schedule starts at `tolerance * 2^(n + s)` and
//! halves with every evaluation. A trial within `schedule / 2 - width / 2` of
//! the midpoint leaves a bracket no wider than `schedule / 2`, so the bracket
//! never exceeds its schedule and is located after at most `n + s`
//! evaluations. The Illinois trial is projected onto that radius; the midpoint
//! always satisfies it.
//!
//! [`RootRefinementMethod::IllinoisMinmax`]: super::RootRefinementMethod::IllinoisMinmax

/// Which end of a bracket the last evaluation moved.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum BracketEnd {
    Low,
    High,
}

/// One domain change being narrowed to the location tolerance.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RootBracket {
    low: f64,
    high: f64,
    tolerance: f64,
    /// `tolerance * 2^(n + s - j)` after `j` evaluations.
    schedule: f64,
    /// `n + s`.
    evaluation_bound: usize,
    /// The Illinois weights of the indicator values held at each end.
    low_weight: f64,
    high_weight: f64,
    last_moved: Option<BracketEnd>,
}

/// Doublings that carry any positive finite tolerance past any finite width.
const DOUBLING_LIMIT: usize = 2100;

impl RootBracket {
    pub(super) fn open(low: f64, high: f64, tolerance: f64, slack: u32) -> Self {
        let width = high - low;
        let mut schedule = tolerance;
        let mut halvings = 0_usize;
        while halvings < DOUBLING_LIMIT && schedule < width {
            schedule *= 2.0;
            halvings += 1;
        }
        for _ in 0..slack {
            schedule *= 2.0;
        }
        Self {
            low,
            high,
            tolerance,
            schedule,
            evaluation_bound: halvings + slack as usize,
            low_weight: 1.0,
            high_weight: 1.0,
            last_moved: None,
        }
    }

    #[must_use]
    pub const fn low(&self) -> f64 {
        self.low
    }

    #[must_use]
    pub const fn high(&self) -> f64 {
        self.high
    }

    /// The evaluations after which the bracket is located, in exact
    /// arithmetic, whatever the indicators.
    #[must_use]
    pub const fn evaluation_bound(&self) -> usize {
        self.evaluation_bound
    }

    /// Whether the bracket is no wider than the location tolerance.
    #[must_use]
    pub fn is_located(&self) -> bool {
        self.high - self.low <= self.tolerance
    }

    /// Where, as a fraction of the bracket from `low`, the Illinois-weighted
    /// secant of one indicator that has entered its new domain at `high`
    /// crosses zero, from its values `at_low` and `at_high` at the two ends.
    ///
    /// The fraction is in `[0, 1]` for any finite values; two values whose
    /// weighted magnitudes do not sum to a positive finite number give the
    /// midpoint.
    #[must_use]
    pub fn crossing_fraction(&self, at_low: f64, at_high: f64) -> f64 {
        let low = self.low_weight * at_low.abs();
        let high = self.high_weight * at_high.abs();
        let sum = low + high;
        if sum > 0.0 && sum.is_finite() {
            low / sum
        } else {
            0.5
        }
    }

    /// The next coordinate to evaluate, from the least
    /// [`crossing_fraction`](Self::crossing_fraction) over the indicators that
    /// have entered their new domain at `high`.
    ///
    /// The secant point is kept half a tolerance inside each end, so a bracket
    /// whose crossing lies within that distance of an end is located by the
    /// next evaluation, then projected onto the minmax radius around the
    /// midpoint. `None` means no representable coordinate lies strictly inside
    /// the bracket: it is as narrow as `f64` allows.
    #[must_use]
    pub fn trial(&self, least_fraction: f64) -> Option<f64> {
        let width = self.high - self.low;
        let middle = self.low + 0.5 * width;
        let secant = (self.low + width * least_fraction)
            .max(self.low + 0.5 * self.tolerance)
            .min(self.high - 0.5 * self.tolerance);
        let radius = (0.5 * self.schedule - 0.5 * width).max(0.0);
        let projected = if secant - middle > radius {
            middle + radius
        } else if middle - secant > radius {
            middle - radius
        } else {
            secant
        };
        if self.low < projected && projected < self.high {
            Some(projected)
        } else if self.low < middle && middle < self.high {
            Some(middle)
        } else {
            None
        }
    }

    /// The end of the [`ToleranceWindow`](super::RootTieBreak::ToleranceWindow)
    /// of a located bracket whose scan sample ends at `upper`: one location
    /// tolerance after the low end, rounded to the nearest coordinate, never
    /// past `upper`.
    ///
    /// The earliest crossing lies in `(low, high]` and `high - low` is within
    /// the tolerance, so the window end is at least `high` and within one
    /// tolerance after the earliest crossing, on its far side.
    #[must_use]
    pub fn application_window(&self, upper: f64) -> f64 {
        (self.low + self.tolerance).min(upper)
    }

    /// Narrow to the evaluation at `trial`: it becomes `high` when an indicator
    /// had entered its new domain there and `low` otherwise. An end retained
    /// twice in a row has its weight halved (the Illinois step), and the end
    /// just moved restarts at weight one.
    pub fn narrow(&mut self, trial: f64, entered: bool) {
        let moved = if entered {
            self.high = trial;
            self.high_weight = 1.0;
            BracketEnd::High
        } else {
            self.low = trial;
            self.low_weight = 1.0;
            BracketEnd::Low
        };
        if self.last_moved == Some(moved) {
            match moved {
                BracketEnd::High => self.low_weight *= 0.5,
                BracketEnd::Low => self.high_weight *= 0.5,
            }
        }
        self.last_moved = Some(moved);
        self.schedule *= 0.5;
    }
}

#[cfg(test)]
mod tests;
