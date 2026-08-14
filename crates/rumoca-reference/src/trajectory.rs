//! The externally supplied continuous trajectory.
//!
//! Slice 1 does not integrate, and that is a scope decision rather than an
//! omission. The reference exists to say what the *event* semantics are; how a
//! host advances continuous state between events is the host's business, and
//! folding an integrator in here would put a numerical method inside the thing
//! that is supposed to adjudicate semantics. So continuous variables arrive as
//! a function of time and the reference never differentiates anything.

/// A continuous trajectory the host supplies.
pub trait Trajectory {
    /// The value of continuous variable `name` at `time`, or `None` when this
    /// trajectory says nothing about that name.
    fn value_at(&self, name: &str, time: f64) -> Option<f64>;
}

/// A model with no continuous state.
///
/// The differential harness prefers models this trajectory suffices for,
/// because they let the reference and the pipeline be compared without any
/// integrator in between: a disagreement is then a semantics disagreement and
/// nothing else.
#[derive(Debug, Clone, Copy, Default)]
pub struct NoContinuousState;

impl Trajectory for NoContinuousState {
    fn value_at(&self, _name: &str, _time: f64) -> Option<f64> {
        None
    }
}

/// Straight lines: `value(t) = start + slope * (t - t_start)`.
///
/// This is exactly the trajectory `der(x) = slope` with `x(start = start)`
/// produces, which is what makes it the useful one — a model whose only
/// continuous variable is a clock ramp can be compared against the pipeline
/// with no integration error to argue about.
#[derive(Debug, Clone, Default)]
pub struct Ramps {
    entries: Vec<RampEntry>,
    t_start: f64,
}

#[derive(Debug, Clone)]
struct RampEntry {
    name: String,
    start: f64,
    slope: f64,
}

impl Ramps {
    /// A ramp set anchored at `t_start`.
    #[must_use]
    pub fn new(t_start: f64) -> Self {
        Self {
            entries: Vec::new(),
            t_start,
        }
    }

    /// Adds `name(t) = start + slope * (t - t_start)`.
    #[must_use]
    pub fn with(mut self, name: &str, start: f64, slope: f64) -> Self {
        self.entries.push(RampEntry {
            name: name.to_owned(),
            start,
            slope,
        });
        self
    }
}

impl Trajectory for Ramps {
    fn value_at(&self, name: &str, time: f64) -> Option<f64> {
        self.entries
            .iter()
            .find(|entry| entry.name == name)
            .map(|entry| entry.start + entry.slope * (time - self.t_start))
    }
}
