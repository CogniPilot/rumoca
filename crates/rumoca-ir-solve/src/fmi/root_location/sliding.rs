//! The sliding-mode policy of a [`RootLocationPlan`] (SPEC_0044 ME-EVENT-005).
//!
//! When the dynamics on both sides of a relation's surface point back at it
//! (`der(x) = if x < c then a else b` with `a > 0 > b`), the state slides along
//! the surface (A. F. Filippov, "Differential Equations with Discontinuous
//! Righthand Sides", Kluwer, 1988, chapter 2) and every state event (MLS §8.5)
//! is followed by an immediate re-crossing. Each crossing located to the
//! location tolerance advances time by little more than that tolerance, so an
//! executor would spend an unbounded number of events on one interval.
//!
//! An indicator slides once [`RootLocationPlan::sliding_streak`] consecutive
//! state events changed it, each within [`RootLocationPlan::sliding_gap`] of
//! the one before: far below every time scale the scan resolves, so ordinary
//! switching, however fast the model's own period, never qualifies. While it
//! slides, a domain change made only by sliding indicators applies at the scan
//! coordinate that observes it, without refinement, so the surface is crossed
//! at most once per scanned interval. The first interval that completes
//! without such an event ends every sliding mode, and the next crossing is
//! again located to the full tolerance.
//!
//! The generated C locator (`templates/fmi3/scalar_events.jinja`, `cs_events`)
//! repeats these operations in the same order over its scan intervals.
//!
//! [`RootLocationPlan`]: super::RootLocationPlan
//! [`RootLocationPlan::sliding_streak`]: super::RootLocationPlan::sliding_streak
//! [`RootLocationPlan::sliding_gap`]: super::RootLocationPlan::sliding_gap

/// Which indicators of one component are sliding, and the event history that
/// decides it.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct SlidingModes {
    /// Time of each indicator's latest state event.
    last_event: Vec<f64>,
    /// Consecutive close state events of each indicator, ending at
    /// `last_event`.
    streak: Vec<u32>,
    /// Whether a domain change of each indicator in the current interval
    /// applies without refinement.
    sliding: Vec<bool>,
    /// Intervals begun since the latest state event.
    intervals_since_event: u32,
}

impl SlidingModes {
    /// No indicator sliding, with no event history.
    #[must_use]
    pub fn new(indicator_count: usize) -> Self {
        Self {
            last_event: vec![f64::NEG_INFINITY; indicator_count],
            streak: vec![0; indicator_count],
            sliding: vec![false; indicator_count],
            intervals_since_event: 0,
        }
    }

    /// Forget every event, as a restarted component must.
    pub fn reset(&mut self) {
        *self = Self::new(self.last_event.len());
    }

    /// Whether a domain change of `indicator` in the current interval applies
    /// at the scan coordinate that observes it.
    #[must_use]
    pub fn is_sliding(&self, indicator: usize) -> bool {
        self.sliding.get(indicator).copied().unwrap_or(false)
    }

    /// Whether every indicator in `changed` slides, and there is one.
    #[must_use]
    pub fn only_sliding(&self, changed: &[usize]) -> bool {
        !changed.is_empty() && changed.iter().all(|&index| self.is_sliding(index))
    }

    /// Begin one integration interval, before it is scanned.
    ///
    /// The second interval begun since the latest state event follows one
    /// that completed without re-crossing, which ends every sliding mode.
    pub fn begin_interval(&mut self) {
        self.intervals_since_event = self.intervals_since_event.saturating_add(1);
        if self.intervals_since_event == 2 {
            self.sliding.fill(false);
        }
    }

    /// Record a state event at `time` that changed exactly the `changed`
    /// indicators; `gap` is the plan's [`super::RootLocationPlan::sliding_gap`]
    /// and `streak` its [`super::RootLocationPlan::sliding_streak`].
    pub fn record_event(&mut self, time: f64, changed: &[usize], gap: f64, streak: u32) {
        for index in 0..self.last_event.len() {
            if !changed.contains(&index) {
                self.streak[index] = 0;
                self.sliding[index] = false;
                continue;
            }
            let close = self.sliding[index] || time - self.last_event[index] <= gap;
            self.streak[index] = if close {
                self.streak[index].saturating_add(1)
            } else {
                1
            };
            self.last_event[index] = time;
            self.sliding[index] = self.streak[index] >= streak;
        }
        self.intervals_since_event = 0;
    }
}

#[cfg(test)]
mod tests;
