//! The hand-written differential cases.
//!
//! Each case carries a Modelica source and a [`Model`] written by hand to
//! mirror it. Writing both is the point: they are two independent readings of
//! one model, so they disagree loudly. A shared front end would make them agree
//! by construction and prove nothing.
//!
//! Every case pins semantics this project has already paid for once — the rows
//! named in each `registry` field are the formal-statement registry entries the
//! case exercises.

use rumoca_reference::model::{BinaryOp, Equation, Expr, Model, Variable, WhenBranch};
use rumoca_reference::value::Value;

// Several sources declare `Real clock_` with `der(clock_) = 1` and never read
// it. The simulate entry point rejects a model with no continuous state
// ("empty system: no state equations to simulate"), and an inert clock keeps
// each case about `when` rather than about that restriction. It is absent from
// the reference models on purpose: the reference has no such restriction, and
// adding a variable there that the equations never touch would misrepresent
// what is being compared.

/// One differential case.
pub(crate) struct Case {
    /// The model name, matching the Modelica `model` declaration.
    pub(crate) name: &'static str,
    /// The Modelica source the pipeline compiles.
    pub(crate) source: &'static str,
    /// The hand-written reference model.
    pub(crate) model: Model,
    /// Discrete variables both sides must agree on.
    pub(crate) observed: &'static [&'static str],
    /// `(name, slope)` for each continuous variable, matching the `der(...)`
    /// the Modelica source declares. The reference does not integrate, so the
    /// trajectory is supplied; every case here uses a constant derivative, so
    /// the supplied ramp is exact and no integration error enters the
    /// comparison.
    pub(crate) slopes: &'static [(&'static str, f64)],
    /// Instants at which the two traces are compared.
    pub(crate) probes: &'static [f64],
    /// The formal-statement registry rows this case exercises.
    pub(crate) registry: &'static [&'static str],
    /// What the case is for, in one sentence.
    pub(crate) note: &'static str,
}

/// `time >= instant`.
fn after(instant: f64) -> Expr {
    Expr::binary(BinaryOp::GreaterEqual, Expr::Time, Expr::real(instant))
}

/// `when <condition> then <target> = <value>; end when;`
fn when_assign(condition: Expr, target: &str, value: Expr) -> Equation {
    Equation::When(vec![WhenBranch::assigning(condition, target, value)])
}

/// Every hand-written case.
pub(crate) fn cases() -> Vec<Case> {
    vec![
        literal_when_never_fires(),
        already_true_at_start(),
        falling_condition_never_activates(),
        time_event_fires_once(),
        elsewhen_priority(),
        initial_gated_activation(),
        initial_does_not_enable_its_neighbour(),
        self_rescheduling_counter(),
        state_condition_cascade(),
        strict_inequality_crossing(),
    ]
}

/// One activation's body supplies the operand another activation tests.
///
/// The case exists because the reference got it wrong. An earlier version
/// latched every buffer at the instant's two limits and held it fixed for the
/// whole event iteration; `x > 2` was then sampled before `x` was written, so
/// the second clause never fired, and never fired later either because by then
/// the condition held on both limits. Appendix B solves the two activations and
/// the two bodies together.
///
/// It also closes a coverage hole: every other state-condition case here starts
/// true and stays true, so none of them ever crosses *within* a run.
fn state_condition_cascade() -> Case {
    Case {
        name: "StateConditionCascade",
        source: "model StateConditionCascade
  discrete Real x(start = 0, fixed = true);
  discrete Real y(start = 0, fixed = true);
  Real clock_(start = 0, fixed = true);
equation
  der(clock_) = 1;
  when time >= 0.5 then
    x = 3;
  end when;
  when x > 2 then
    y = 1;
  end when;
end StateConditionCascade;
",
        model: Model::new()
            .with_variable(Variable::discrete("x", Value::Real(0.0)))
            .with_variable(Variable::discrete("y", Value::Real(0.0)))
            .with_equation(when_assign(after(0.5), "x", Expr::real(3.0)))
            .with_equation(when_assign(
                Expr::binary(BinaryOp::Greater, Expr::var("x"), Expr::real(2.0)),
                "y",
                Expr::real(1.0),
            )),
        observed: &["x", "y"],
        slopes: &[],
        probes: &[0.0, 0.25, 0.75, 1.0],
        registry: &["FS-EQN-019"],
        note: "The second activation rises because the first one's body wrote \
               its operand inside the same instant; a reference that froze the \
               condition would report y = 0 forever.",
    }
}

/// `time > c` is false *at* `c`, so its event cannot be placed there.
///
/// The reference schedules this one instant later, at the next representable
/// time. The case exists because that offset was load-bearing and unpinned:
/// removing it left every test green while `when time > c` silently stopped
/// firing.
fn strict_inequality_crossing() -> Case {
    Case {
        name: "StrictInequalityCrossing",
        source: "model StrictInequalityCrossing
  discrete Real y(start = 0, fixed = true);
  Real clock_(start = 0, fixed = true);
equation
  der(clock_) = 1;
  when time > 0.5 then
    y = 1;
  end when;
end StrictInequalityCrossing;
",
        model: Model::new()
            .with_variable(Variable::discrete("y", Value::Real(0.0)))
            .with_equation(when_assign(
                Expr::binary(BinaryOp::Greater, Expr::Time, Expr::real(0.5)),
                "y",
                Expr::real(1.0),
            )),
        observed: &["y"],
        slopes: &[],
        probes: &[0.0, 0.25, 0.75, 1.0],
        registry: &["FS-EQN-005"],
        note: "The activation must still occur, one representable instant after \
               the threshold rather than at it.",
    }
}

/// `when true then` has no rising edge anywhere, so its body never runs.
fn literal_when_never_fires() -> Case {
    Case {
        name: "LiteralWhenNeverFires",
        source: "model LiteralWhenNeverFires
  discrete Real y(start = 0, fixed = true);
  Real clock_(start = 0, fixed = true);
equation
  der(clock_) = 1;
  when true then
    y = 1;
  end when;
end LiteralWhenNeverFires;
",
        model: Model::new()
            .with_variable(Variable::discrete("y", Value::Real(0.0)))
            .with_equation(when_assign(Expr::boolean(true), "y", Expr::real(1.0))),
        observed: &["y"],
        slopes: &[],
        probes: &[0.0, 0.25, 0.5, 0.75, 1.0],
        registry: &["FS-EQN-001", "FS-EQN-002"],
        note: "The buffer starts true because the condition is true at the \
               initialization instant, and §8.6 forbids the initial instant \
               from manufacturing the edge, so the body never runs at all.",
    }
}

/// A condition already true at the initialization instant presents no edge.
fn already_true_at_start() -> Case {
    Case {
        name: "AlreadyTrueAtStart",
        source: "model AlreadyTrueAtStart
  Real x(start = 5, fixed = true);
  discrete Real y(start = 0, fixed = true);
equation
  der(x) = 1;
  when x > 2 then
    y = 1;
  end when;
end AlreadyTrueAtStart;
",
        model: Model::new()
            .with_variable(Variable::continuous("x", 5.0))
            .with_variable(Variable::discrete("y", Value::Real(0.0)))
            .with_equation(when_assign(
                Expr::binary(BinaryOp::Greater, Expr::var("x"), Expr::real(2.0)),
                "y",
                Expr::real(1.0),
            )),
        observed: &["y"],
        slopes: &[("x", 1.0)],
        probes: &[0.0, 0.5, 1.0],
        registry: &["FS-EQN-001", "FS-EQN-002"],
        note: "x starts at 5 and only grows, so `x > 2` is true throughout and \
               never rises; seeding the buffer false is what used to \
               manufacture an activation at the initial event.",
    }
}

/// A condition true at the start and falling later never activates.
fn falling_condition_never_activates() -> Case {
    Case {
        name: "FallingConditionNeverActivates",
        source: "model FallingConditionNeverActivates
  discrete Real y(start = 0, fixed = true);
  Real clock_(start = 0, fixed = true);
equation
  der(clock_) = 1;
  when time < 0.5 then
    y = 1;
  end when;
end FallingConditionNeverActivates;
",
        model: Model::new()
            .with_variable(Variable::discrete("y", Value::Real(0.0)))
            .with_equation(when_assign(
                Expr::binary(BinaryOp::Less, Expr::Time, Expr::real(0.5)),
                "y",
                Expr::real(1.0),
            )),
        observed: &["y"],
        slopes: &[],
        probes: &[0.0, 0.25, 0.75, 1.0],
        registry: &["FS-EQN-002"],
        note: "True at t = 0 and false from 0.5, so the only crossing is a \
               falling one and `edge` is false at it.",
    }
}

/// A single false-to-true crossing fires the body exactly once.
fn time_event_fires_once() -> Case {
    Case {
        name: "TimeEventFiresOnce",
        source: "model TimeEventFiresOnce
  discrete Real count(start = 0, fixed = true);
  Real clock_(start = 0, fixed = true);
equation
  der(clock_) = 1;
  when time >= 0.5 then
    count = pre(count) + 1;
  end when;
end TimeEventFiresOnce;
",
        model: Model::new()
            .with_variable(Variable::discrete("count", Value::Real(0.0)))
            .with_equation(when_assign(
                after(0.5),
                "count",
                Expr::binary(BinaryOp::Add, Expr::pre("count"), Expr::real(1.0)),
            )),
        observed: &["count"],
        slopes: &[],
        probes: &[0.0, 0.25, 0.75, 1.0],
        registry: &["FS-EQN-005"],
        note: "Re-applying the body at the event's right limit would count to \
               two; the edge exists at one instant only.",
    }
}

/// Branch order is priority order when two branches rise together.
fn elsewhen_priority() -> Case {
    Case {
        name: "ElsewhenPriority",
        source: "model ElsewhenPriority
  discrete Real y(start = 0, fixed = true);
  Real clock_(start = 0, fixed = true);
equation
  der(clock_) = 1;
  when time >= 0.5 then
    y = 1;
  elsewhen time >= 0.5 then
    y = 2;
  end when;
end ElsewhenPriority;
",
        model: Model::new()
            .with_variable(Variable::discrete("y", Value::Real(0.0)))
            .with_equation(Equation::When(vec![
                WhenBranch::assigning(after(0.5), "y", Expr::real(1.0)),
                WhenBranch::assigning(after(0.5), "y", Expr::real(2.0)),
            ])),
        observed: &["y"],
        slopes: &[],
        probes: &[0.0, 0.75, 1.0],
        registry: &["FS-EQN-005"],
        note: "Both branches rise at the same instant; the §8.3.5.1 expansion \
               nests them in branch order, so the first wins without needing a \
               priority rule of its own.",
    }
}

/// A when-clause is active during initialization exactly when `initial()` says.
fn initial_gated_activation() -> Case {
    Case {
        name: "InitialGatedActivation",
        source: "model InitialGatedActivation
  discrete Real y(start = 0, fixed = true);
  Real clock_(start = 0, fixed = true);
equation
  der(clock_) = 1;
  when initial() then
    y = 1;
  end when;
end InitialGatedActivation;
",
        model: Model::new()
            .with_variable(Variable::discrete("y", Value::Real(0.0)))
            .with_equation(when_assign(Expr::Initial, "y", Expr::real(1.0))),
        observed: &["y"],
        slopes: &[],
        probes: &[0.0, 0.5, 1.0],
        registry: &["FS-EQN-003"],
        note: "§8.6: the equations of a when-clause are active during \
               initialization if and only if explicitly enabled with initial().",
    }
}

/// An `initial()` activation does not enable the activation beside it.
fn initial_does_not_enable_its_neighbour() -> Case {
    Case {
        name: "InitialDoesNotEnableNeighbour",
        source: "model InitialDoesNotEnableNeighbour
  Real x(start = 5, fixed = true);
  discrete Real a(start = 0, fixed = true);
  discrete Real b(start = 0, fixed = true);
equation
  der(x) = 1;
  when initial() then
    a = 1;
  end when;
  when x > 2 then
    b = 1;
  end when;
end InitialDoesNotEnableNeighbour;
",
        model: Model::new()
            .with_variable(Variable::continuous("x", 5.0))
            .with_variable(Variable::discrete("a", Value::Real(0.0)))
            .with_variable(Variable::discrete("b", Value::Real(0.0)))
            .with_equation(when_assign(Expr::Initial, "a", Expr::real(1.0)))
            .with_equation(when_assign(
                Expr::binary(BinaryOp::Greater, Expr::var("x"), Expr::real(2.0)),
                "b",
                Expr::real(1.0),
            )),
        observed: &["a", "b"],
        slopes: &[("x", 1.0)],
        probes: &[0.0, 0.5, 1.0],
        registry: &["FS-EQN-003", "FS-EQN-004"],
        note: "The seeded buffer must remove the spurious initial activation \
               of `b` without removing the §8.6 one on `a`; a fix that took \
               both would read as a suppression.",
    }
}

/// A counter that reschedules its own next instant from its own state.
fn self_rescheduling_counter() -> Case {
    Case {
        name: "SelfReschedulingCounter",
        source: "model SelfReschedulingCounter
  discrete Real nextTime(start = 0.2, fixed = true);
  discrete Real count(start = 0, fixed = true);
  Real clock_(start = 0, fixed = true);
equation
  der(clock_) = 1;
  when time >= nextTime then
    count = pre(count) + 1;
    nextTime = pre(nextTime) + 0.2;
  end when;
end SelfReschedulingCounter;
",
        model: Model::new()
            .with_variable(Variable::discrete("nextTime", Value::Real(0.2)))
            .with_variable(Variable::discrete("count", Value::Real(0.0)))
            .with_equation(Equation::When(vec![WhenBranch {
                condition: Expr::binary(BinaryOp::GreaterEqual, Expr::Time, Expr::var("nextTime")),
                body: vec![
                    (
                        "count".to_owned(),
                        Expr::binary(BinaryOp::Add, Expr::pre("count"), Expr::real(1.0)),
                    ),
                    (
                        "nextTime".to_owned(),
                        Expr::binary(BinaryOp::Add, Expr::pre("nextTime"), Expr::real(0.2)),
                    ),
                ],
            }])),
        observed: &["count", "nextTime"],
        slopes: &[],
        // 1.0 is deliberately not probed: an event landing exactly on the stop
        // time is a horizon question, not a semantics one, and probing it would
        // report a horizon disagreement as an event-semantics disagreement.
        probes: &[0.0, 0.1, 0.3, 0.5, 0.7, 0.9],
        registry: &["FS-EQN-005"],
        note: "The activation instant is an expression over a discrete \
               variable the body advances, so the schedule is dynamic; the \
               reference needs no separate mechanism because it re-evaluates \
               the instant at every event.",
    }
}
