//! Time-event scheduling.
//!
//! A condition that depends only on `time` has a computable crossing instant,
//! so MLS §8.5 makes it a *time event* rather than a state event: the solver is
//! told where to stop instead of searching for a sign change. Slice 1 models
//! exactly that case and no other, because it is the case where the reference
//! and a real solver can be expected to agree on an instant exactly rather than
//! to within a root-finder tolerance.
//!
//! The instant expression is evaluated in the current environment, so a
//! condition like `time >= nextTime` — where `nextTime` is a discrete variable
//! the when-body advances — reschedules itself. That is the whole of dynamic
//! time events; it needs no separate mechanism because the instant is just an
//! expression and expressions are re-evaluated at every event.

use crate::eval::{Environment, EvalError, eval};
use crate::model::{BinaryOp, Equation, Expr, Model};

/// Where a time relation changes value, relative to its threshold.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Crossing {
    /// The value differs at the threshold from just before it: `time >= c`
    /// becomes true at `c`, and `time < c` becomes false at `c`.
    At,
    /// The value at the threshold still equals the value before it, and only
    /// differs after: `time > c` is false at `c`, and `time <= c` is true at
    /// `c`.
    ///
    /// The event therefore has to be scheduled at the next representable
    /// instant, or the activation is simply missed — a `when time > c` whose
    /// event is placed at `c` finds `c > c` false and never fires. Slice 1 has
    /// no superdense time, so the smallest real instant after `c` is the
    /// closest it can put the event, and the offset is one ULP.
    After,
}

/// The threshold expression of a time-dependent relation, if `expr` is one.
///
/// Recognizes all four operators in both operand orders, normalizing the
/// reversed form (`c > time` is `time < c`).
#[must_use]
pub fn crossing_instant(expr: &Expr) -> Option<(&Expr, Crossing)> {
    let Expr::Binary(op, lhs, rhs) = expr else {
        return None;
    };
    let (threshold, normalized) = match (lhs.as_ref(), rhs.as_ref()) {
        (Expr::Time, other) => (other, *op),
        (other, Expr::Time) => (other, reverse(*op)?),
        _ => return None,
    };
    let crossing = match normalized {
        BinaryOp::GreaterEqual | BinaryOp::Less => Crossing::At,
        BinaryOp::Greater | BinaryOp::LessEqual => Crossing::After,
        _ => return None,
    };
    Some((threshold, crossing))
}

/// `a op b` rewritten as `b op' a`, for the orderings slice 1 recognizes.
fn reverse(op: BinaryOp) -> Option<BinaryOp> {
    Some(match op {
        BinaryOp::Less => BinaryOp::Greater,
        BinaryOp::LessEqual => BinaryOp::GreaterEqual,
        BinaryOp::Greater => BinaryOp::Less,
        BinaryOp::GreaterEqual => BinaryOp::LessEqual,
        _ => return None,
    })
}

/// Every crossing-instant expression the model's equations mention.
///
/// Reads the expanded model, so the conditions it finds are the buffer-defining
/// equations MLS §8.3.5.1 produced. A model that never expanded would hide its
/// conditions inside `Equation::When` and this would find nothing, which is why
/// [`crate::expand::expand`] runs first.
#[must_use]
pub fn crossing_expressions(model: &Model) -> Vec<(&Expr, Crossing)> {
    let mut found = Vec::new();
    for equation in &model.equations {
        match equation {
            Equation::Assign { value, .. } | Equation::InitialAssign { value, .. } => {
                collect_crossings(value, &mut found);
            }
            // Unreachable on an expanded model, and deliberately not an error:
            // this function reports what it can see, and a caller that skipped
            // the expansion gets an empty schedule rather than a panic.
            Equation::When(_) => {}
        }
    }
    found
}

/// Walks `expr` collecting every crossing instant beneath it.
fn collect_crossings<'expr>(expr: &'expr Expr, found: &mut Vec<(&'expr Expr, Crossing)>) {
    if let Some(entry) = crossing_instant(expr) {
        found.push(entry);
    }
    match expr {
        Expr::Not(operand) | Expr::Negate(operand) => collect_crossings(operand, found),
        Expr::Binary(_, lhs, rhs) => {
            collect_crossings(lhs, found);
            collect_crossings(rhs, found);
        }
        Expr::If(condition, consequent, alternative) => {
            collect_crossings(condition, found);
            collect_crossings(consequent, found);
            collect_crossings(alternative, found);
        }
        _ => {}
    }
}

/// The next instant strictly after `env.time` and at most `t_stop`.
///
/// "Strictly after" is what keeps an event from re-firing at the instant it
/// just handled. It is also why a self-rescheduling counter must advance its
/// next instant by a positive amount: one that rescheduled to the same instant
/// would simply never be scheduled again, rather than looping forever.
pub fn next_event_time(
    model: &Model,
    env: &Environment,
    t_stop: f64,
) -> Result<Option<f64>, EvalError> {
    let mut next: Option<f64> = None;
    for (threshold, crossing) in crossing_expressions(model) {
        let Some(value) = eval(threshold, env)?.as_real() else {
            continue;
        };
        if !value.is_finite() {
            continue;
        }
        let candidate = match crossing {
            Crossing::At => value,
            Crossing::After => value.next_up(),
        };
        if candidate <= env.time || candidate > t_stop {
            continue;
        }
        next = Some(next.map_or(candidate, |best: f64| best.min(candidate)));
    }
    Ok(next)
}

/// Every static instant the model schedules: the crossings whose instant
/// expression is a literal.
///
/// The differential harness compares this against the compiler's own scheduled
/// time events, which is a comparison of two independent readings of the same
/// model rather than of two runs.
#[must_use]
pub fn static_instants(model: &Model) -> Vec<f64> {
    let mut instants: Vec<f64> = crossing_expressions(model)
        .into_iter()
        .filter_map(|(expr, crossing)| match expr {
            // Reported at the threshold the model wrote, not at the ULP-shifted
            // instant the run stops on: this is compared against the
            // compiler's scheduled instants, which are the thresholds.
            Expr::Literal(value) => {
                let _ = crossing;
                value.as_real()
            }
            _ => None,
        })
        .collect();
    instants.sort_by(f64::total_cmp);
    instants.dedup();
    instants
}
