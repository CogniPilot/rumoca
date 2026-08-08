//! A generator for mini models in the slice-1 fragment.
//!
//! The hand-written cases pin the semantics that were expensive to learn. This
//! generator covers the space around them.
//!
//! Note what is and is not shared. One generated *description* is rendered
//! twice — once to Modelica text, once to a reference [`Model`] — so the two
//! syntaxes do agree by construction. That is fine and deliberate: the claim
//! under test is that the two **evaluators** agree, and those share nothing.
//! The hand-written cases are where the two *readings* are independent, which
//! is why they exist alongside this.
//!
//! Every generated instant is a multiple of an eighth, so it is exact in `f64`
//! and exact as the compiler's rational clock lattice; probes sit on odd
//! sixteenths and therefore never land on an instant.
//!
//! # What the generator deliberately does not reach
//!
//! Stated so the coverage is not overread:
//!
//! * **Instant `0`** (`instant_eighths` starts at 1). A threshold at the start
//!   instant is the already-true-at-start shape, whose semantics is subtle
//!   enough that it belongs in a named hand-written case — it is
//!   `AlreadyTrueAtStart` — rather than in a case the generator can produce
//!   without anyone noticing which rule it exercised.
//! * **Instant `8/8 = 1.0`**, the stop time. An event landing exactly on the
//!   horizon is a horizon question rather than a semantics one, and the two
//!   sides are entitled to differ there.
//! * **Non-dyadic thresholds.** Eighths are exact in binary; a threshold like
//!   `0.1` is not, and comparing a `f64` threshold against a rational clock
//!   lattice would test rounding rather than event semantics.
//! * **Strict inequalities, `elsewhen`, `initial()`, state conditions, and
//!   self-rescheduling guards.** All are hand-written cases; the generator
//!   emits only `time >= c`.

use rumoca_reference::model::{BinaryOp, Equation, Expr, Model, Variable, WhenBranch};
use rumoca_reference::value::Value;

/// The targets a generated model may assign.
///
/// MLS §8.3.5 contract EQN-020 forbids two when-equations defining the same
/// variable, so the generator gives each activation its own target rather than
/// generating models the compiler is required to reject.
pub(crate) const TARGETS: [&str; 2] = ["y0", "y1"];

/// One generated when-equation.
#[derive(Debug, Clone, Copy)]
pub(crate) struct WhenSpec {
    /// The activation instant, in eighths of a second.
    pub(crate) instant_eighths: u32,
    /// Whether the body accumulates (`pre(v) + 1`) or assigns a constant.
    pub(crate) accumulates: bool,
}

/// A generated model: one when-equation per target, in target order.
#[derive(Debug, Clone)]
pub(crate) struct Spec {
    /// The activations, at most one per entry of [`TARGETS`].
    pub(crate) whens: Vec<WhenSpec>,
}

impl Spec {
    /// The instant of `when` number `index`, in seconds.
    fn instant(&self, index: usize) -> f64 {
        f64::from(self.whens[index].instant_eighths) / 8.0
    }

    /// The Modelica source the pipeline compiles.
    pub(crate) fn source(&self) -> String {
        let mut text = String::from("model Generated\n");
        for target in TARGETS.iter().take(self.whens.len()) {
            text.push_str(&format!(
                "  discrete Real {target}(start = 0, fixed = true);\n"
            ));
        }
        // The simulate entry rejects a model with no continuous state; this
        // clock is never read by the semantics under test.
        text.push_str("  Real clock_(start = 0, fixed = true);\nequation\n  der(clock_) = 1;\n");
        for (index, spec) in self.whens.iter().enumerate() {
            let target = TARGETS[index];
            let body = if spec.accumulates {
                format!("{target} = pre({target}) + 1")
            } else {
                format!("{target} = 1")
            };
            text.push_str(&format!(
                "  when time >= {} then\n    {body};\n  end when;\n",
                self.instant(index)
            ));
        }
        text.push_str("end Generated;\n");
        text
    }

    /// The same model as a reference value.
    pub(crate) fn model(&self) -> Model {
        let mut model = Model::new();
        for target in TARGETS.iter().take(self.whens.len()) {
            model = model.with_variable(Variable::discrete(target, Value::Real(0.0)));
        }
        for (index, spec) in self.whens.iter().enumerate() {
            let target = TARGETS[index];
            let body = if spec.accumulates {
                Expr::binary(BinaryOp::Add, Expr::pre(target), Expr::real(1.0))
            } else {
                Expr::real(1.0)
            };
            model = model.with_equation(Equation::When(vec![WhenBranch::assigning(
                Expr::binary(
                    BinaryOp::GreaterEqual,
                    Expr::Time,
                    Expr::real(self.instant(index)),
                ),
                target,
                body,
            )]));
        }
        model
    }

    /// The variables both sides must agree on.
    pub(crate) fn observed(&self) -> Vec<&'static str> {
        TARGETS.iter().take(self.whens.len()).copied().collect()
    }
}

/// Odd sixteenths in `(0, 1)`: never an eighth, so never an instant.
pub(crate) fn probes() -> Vec<f64> {
    (0..8).map(|k| (2.0 * f64::from(k) + 1.0) / 16.0).collect()
}
