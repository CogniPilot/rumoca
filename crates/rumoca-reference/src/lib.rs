//! Executable reference semantics for the Modelica discrete/event core.
//!
//! # What this crate is
//!
//! A definitional interpreter: a second, independent statement of what a
//! Modelica model *means* at an event, written so that a human can check it
//! against the Modelica Language Specification by reading it.
//!
//! **This crate is the specification.** If it disagrees with the compiler, one
//! of the two is wrong, and this one is the one that can be audited in an
//! afternoon. That asymmetry is the entire value proposition, so it is defended
//! by an explicit anti-goal:
//!
//! # The anti-goal
//!
//! *Nothing here may be made faster, cleverer, or more general.*
//!
//! No caching, no memoization, no incremental evaluation, no fixed-point
//! acceleration, no interning, no arenas, no data structure chosen for its
//! complexity class. Values are cloned freely and maps are `BTreeMap`. Every
//! function is small enough to hold in your head at once and does one thing the
//! specification names. A change that improves performance at the cost of
//! obviousness is a regression here, and should be rejected on that ground
//! alone — the compiler is where speed lives, and the compiler having a fast
//! path is exactly why this crate must not.
//!
//! The reference is also deliberately *narrow*. It refuses models outside its
//! slice instead of approximating them, because an oracle that guesses is worse
//! than no oracle: it makes the compiler agree with it for the wrong reason.
//!
//! # Slice 1 — the discrete/event core
//!
//! * flat scalar `Real` / `Integer` / `Boolean` equations;
//! * `when` / `elsewhen` equations, expanded literally per MLS §8.3.5.1 into
//!   an activation buffer, `b = condition`, and
//!   `v = if edge(b) then expr else pre(v)`, then solved as ordinary
//!   simultaneous equations by the Appendix B event iteration;
//! * §8.6 initialization, where `pre` is the identity and `initial()` is true;
//! * the `pre`, `edge`, and `initial` operators;
//! * scheduled time events, static and self-rescheduling.
//!
//! # What slice 1 refuses
//!
//! "This crate is the specification" is a claim about the fragment above and
//! nothing wider, so the boundary is enforced rather than described. A model
//! outside it is **refused by name** — see [`simulate::RefError`] — never
//! approximated:
//!
//! * **Continuous integration.** Continuous variables are supplied by the host
//!   as a [`trajectory::Trajectory`]; an equation that also determines one is
//!   refused. Integration is the host's job, and a numerical method inside the
//!   oracle would make every disagreement arguable as a tolerance question.
//! * **Continuous crossings.** Slice 1 locates crossings computable from
//!   `time`. If a condition over a continuous variable changes value between
//!   two scheduled instants, it crossed somewhere slice 1 never looked, and the
//!   run stops with `UnlocatedCrossing` rather than reporting the activation at
//!   the next scheduled instant — a time the model does not put it. Locating
//!   these is slice 2.
//! * **Over- and under-determination.** A parameter target, an undeclared
//!   target, or two equations for one variable are each refused by their own
//!   name, because an oracle that guessed would let the compiler agree with it
//!   for the wrong reason.
//!
//! See `ROADMAP.md` for slices 2–4.
//!
//! # Status of the evidence this crate produces
//!
//! Agreement between this reference and the compiler is *validation* evidence,
//! not proof evidence. SPEC_0037 says so directly — "OMC/MSL parity, fuzzing,
//! property tests, and differential traces remain validation evidence for the
//! formal definitions. They are not proof evidence." This crate is a candidate
//! for that spec's "One IR semantics implemented — executable formal
//! definition" promotion criterion; it does not on its own discharge it.
//!
//! # Example
//!
//! ```
//! use rumoca_reference::model::{Equation, Expr, Model, Variable, WhenBranch};
//! use rumoca_reference::simulate::{Options, simulate};
//! use rumoca_reference::trajectory::NoContinuousState;
//! use rumoca_reference::value::Value;
//!
//! // model M discrete Real y(start = 0); equation when time >= 0.5 then y = 1; end when; end M;
//! let model = Model::new()
//!     .with_variable(Variable::discrete("y", Value::Real(0.0)))
//!     .with_equation(Equation::When(vec![WhenBranch::assigning(
//!         Expr::binary(
//!             rumoca_reference::model::BinaryOp::GreaterEqual,
//!             Expr::Time,
//!             Expr::real(0.5),
//!         ),
//!         "y",
//!         Expr::real(1.0),
//!     )]));
//!
//! let trace = simulate(&model, &NoContinuousState, Options::default()).unwrap();
//! assert_eq!(trace.event_times(), vec![0.5]);
//! assert_eq!(trace.final_value("y"), Some(Value::Real(1.0)));
//! ```

pub mod eval;
pub mod expand;
pub mod model;
pub mod schedule;
pub mod simulate;
pub mod trajectory;
pub mod value;
