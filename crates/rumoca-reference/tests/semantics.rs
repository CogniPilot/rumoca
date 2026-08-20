//! What the reference says, asserted without the pipeline anywhere near it.
//!
//! The differential harness checks that the compiler agrees with the reference.
//! These tests check what the reference actually claims, so a reader can
//! adjudicate the claims against MLS without running a compiler — and so a
//! change to the reference that quietly moved a semantics shows up here rather
//! than as a mysterious differential failure.

use rumoca_reference::model::{BinaryOp, Equation, Expr, Model, Variable, WhenBranch};
use rumoca_reference::simulate::{Options, RefError, Trace, simulate};
use rumoca_reference::trajectory::{NoContinuousState, Ramps};
use rumoca_reference::value::Value;

fn options() -> Options {
    Options {
        t_start: 0.0,
        t_stop: 1.0,
        ..Options::default()
    }
}

fn run(model: &Model) -> Trace {
    simulate(model, &NoContinuousState, options()).expect("model is inside slice 1")
}

/// `time >= instant`.
fn after(instant: f64) -> Expr {
    Expr::binary(BinaryOp::GreaterEqual, Expr::Time, Expr::real(instant))
}

fn when_assign(condition: Expr, target: &str, value: Expr) -> Equation {
    Equation::When(vec![WhenBranch::assigning(condition, target, value)])
}

fn counter(condition: Expr) -> Model {
    Model::new()
        .with_variable(Variable::discrete("count", Value::Real(0.0)))
        .with_equation(when_assign(
            condition,
            "count",
            Expr::binary(BinaryOp::Add, Expr::pre("count"), Expr::real(1.0)),
        ))
}

/// MLS §8.3.5.1 gives the buffer of `when true then` a true start value, so it
/// has no rising edge anywhere and the body never runs. Registry FS-EQN-001.
#[test]
fn a_literal_true_activation_never_fires() {
    let trace = run(&counter(Expr::boolean(true)));
    assert_eq!(trace.final_value("count"), Some(Value::Real(0.0)));
    assert!(trace.event_times().is_empty());
}

/// A condition already true at the initialization instant presents no edge
/// there. Registry FS-EQN-001, FS-EQN-002.
#[test]
fn a_condition_already_true_at_the_start_never_activates() {
    let model = Model::new()
        .with_variable(Variable::continuous("x", 5.0))
        .with_variable(Variable::discrete("y", Value::Real(0.0)))
        .with_equation(when_assign(
            Expr::binary(BinaryOp::Greater, Expr::var("x"), Expr::real(2.0)),
            "y",
            Expr::real(1.0),
        ));
    let ramps = Ramps::new(0.0).with("x", 5.0, 1.0);
    let trace = simulate(&model, &ramps, options()).expect("inside slice 1");
    assert_eq!(trace.final_value("y"), Some(Value::Real(0.0)));
}

/// A falling condition is a crossing but not an activation. Registry FS-EQN-002.
#[test]
fn a_falling_condition_produces_an_event_but_no_activation() {
    let trace = run(&counter(Expr::binary(
        BinaryOp::Less,
        Expr::Time,
        Expr::real(0.5),
    )));
    assert_eq!(trace.event_times(), vec![0.5]);
    assert_eq!(trace.final_value("count"), Some(Value::Real(0.0)));
}

/// One false-to-true crossing fires the body exactly once. Registry FS-EQN-005.
#[test]
fn a_single_crossing_fires_exactly_once() {
    let trace = run(&counter(after(0.5)));
    assert_eq!(trace.event_times(), vec![0.5]);
    assert_eq!(trace.final_value("count"), Some(Value::Real(1.0)));
    assert_eq!(trace.value_at("count", 0.25), Some(Value::Real(0.0)));
    assert_eq!(trace.value_at("count", 0.75), Some(Value::Real(1.0)));
}

/// Branch order decides when two branches rise together. Registry FS-EQN-005.
#[test]
fn the_first_elsewhen_branch_wins_a_simultaneous_rise() {
    let model = Model::new()
        .with_variable(Variable::discrete("y", Value::Real(0.0)))
        .with_equation(Equation::When(vec![
            WhenBranch::assigning(after(0.5), "y", Expr::real(1.0)),
            WhenBranch::assigning(after(0.5), "y", Expr::real(2.0)),
        ]));
    assert_eq!(run(&model).final_value("y"), Some(Value::Real(1.0)));
}

/// A when-clause is active during initialization exactly when `initial()` says.
/// Registry FS-EQN-003.
#[test]
fn an_initial_gated_activation_runs_at_initialization() {
    let trace = run(&counter(Expr::Initial));
    assert_eq!(trace.final_value("count"), Some(Value::Real(1.0)));
    assert!(
        trace.event_times().is_empty(),
        "it runs at the initialization instant, which is not an event"
    );
}

/// The `initial()` clause does not enable the clause beside it. Registry
/// FS-EQN-004.
#[test]
fn an_initial_activation_does_not_enable_its_neighbour() {
    let model = Model::new()
        .with_variable(Variable::continuous("x", 5.0))
        .with_variable(Variable::discrete("a", Value::Real(0.0)))
        .with_variable(Variable::discrete("b", Value::Real(0.0)))
        .with_equation(when_assign(Expr::Initial, "a", Expr::real(1.0)))
        .with_equation(when_assign(
            Expr::binary(BinaryOp::Greater, Expr::var("x"), Expr::real(2.0)),
            "b",
            Expr::real(1.0),
        ));
    let ramps = Ramps::new(0.0).with("x", 5.0, 1.0);
    let trace = simulate(&model, &ramps, options()).expect("inside slice 1");
    assert_eq!(trace.final_value("a"), Some(Value::Real(1.0)));
    assert_eq!(trace.final_value("b"), Some(Value::Real(0.0)));
}

/// A guard whose instant its own body advances re-arms at every crossing.
#[test]
fn a_self_rescheduling_guard_fires_on_its_own_grid() {
    let model = Model::new()
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
        }]));
    let trace = run(&model);
    assert_eq!(trace.event_times().len(), 5, "0.2, 0.4, 0.6, 0.8, 1.0");
    assert_eq!(trace.final_value("count"), Some(Value::Real(5.0)));
}

/// The same guard, already met at the start instant, never arms at all.
///
/// Registry FS-SIM-009 states this outcome and names omc as what fixes it. The
/// reference reaches it from §8.3.5.1 alone: the buffer's start value is the
/// condition on the start values, which is already true, so there is no first
/// edge and therefore never a later one.
#[test]
fn a_self_rescheduling_guard_already_met_at_the_start_never_arms() {
    let model = Model::new()
        .with_variable(Variable::discrete("nextTime", Value::Real(0.0)))
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
        }]));
    let trace = run(&model);
    assert_eq!(trace.final_value("count"), Some(Value::Real(0.0)));
    assert!(trace.event_times().is_empty());
}

/// A discrete equation that negates its own `pre` never reaches `z == pre(z)`.
///
/// This is the witness for "the Appendix B event iteration does not terminate in
/// general" (see `ROADMAP.md`, proof obligation 4). It has to be a *bare*
/// equation: an activation-driven flip converges instead, because advancing
/// `pre` clears the edge after one iteration, which is why the obvious
/// candidate `when b then b = not pre(b)` is not a witness.
#[test]
fn a_self_negating_discrete_equation_does_not_converge() {
    let model = Model::new()
        .with_variable(Variable::discrete("a", Value::Boolean(false)))
        .with_equation(Equation::Assign {
            target: "a".to_owned(),
            value: Expr::Not(Box::new(Expr::pre("a"))),
        });
    assert_eq!(
        simulate(&model, &NoContinuousState, options()),
        Err(RefError::NotConverged {
            phase: "event iteration"
        })
    );
}

/// The activation-driven flip that looks like the witness above but converges.
#[test]
fn an_activation_driven_flip_converges_because_the_edge_clears() {
    let model = Model::new()
        .with_variable(Variable::discrete("y", Value::Boolean(false)))
        .with_equation(when_assign(
            after(0.5),
            "y",
            Expr::Not(Box::new(Expr::pre("y"))),
        ));
    let trace = run(&model);
    assert_eq!(trace.final_value("y"), Some(Value::Boolean(true)));
}

/// The reference refuses a model whose equations determine a continuous
/// variable, rather than letting the trajectory and the equation both claim it.
#[test]
fn a_determined_continuous_variable_is_refused() {
    let model = Model::new()
        .with_variable(Variable::continuous("x", 0.0))
        .with_equation(Equation::Assign {
            target: "x".to_owned(),
            value: Expr::real(1.0),
        });
    assert_eq!(
        simulate(&model, &NoContinuousState, options()),
        Err(RefError::ContinuousIsDetermined("x".to_owned()))
    );
}

/// Every inadmissible shape is named, not approximated.
///
/// Each of these used to be silent in a different way: a parameter target
/// simulated happily where the compiler rejects the model, an undeclared target
/// sprang into existence and then sat outside the Appendix B convergence test,
/// and a duplicated target surfaced as "did not converge", which names a
/// symptom rather than the over-determination causing it.
#[test]
fn inadmissible_models_are_refused_by_name() {
    let parameter = Model::new()
        .with_variable(Variable::parameter("p", Value::Real(1.0)))
        .with_equation(Equation::Assign {
            target: "p".to_owned(),
            value: Expr::real(2.0),
        });
    assert_eq!(
        simulate(&parameter, &NoContinuousState, options()),
        Err(RefError::ParameterIsDetermined("p".to_owned()))
    );

    let undeclared = Model::new().with_equation(Equation::Assign {
        target: "ghost".to_owned(),
        value: Expr::real(1.0),
    });
    assert_eq!(
        simulate(&undeclared, &NoContinuousState, options()),
        Err(RefError::UndeclaredTarget("ghost".to_owned()))
    );

    // Two agreeing equations are still two equations for one unknown; MLS §4.8
    // counts equations, not distinct answers, so this is refused even though
    // iterating it would have settled.
    let duplicated = Model::new()
        .with_variable(Variable::discrete("y", Value::Real(0.0)))
        .with_equation(Equation::Assign {
            target: "y".to_owned(),
            value: Expr::real(1.0),
        })
        .with_equation(Equation::Assign {
            target: "y".to_owned(),
            value: Expr::real(1.0),
        });
    assert_eq!(
        simulate(&duplicated, &NoContinuousState, options()),
        Err(RefError::TargetIsOverDetermined("y".to_owned()))
    );
}

/// A condition over a continuous variable that crosses between two scheduled
/// instants is refused rather than reported at the wrong instant.
///
/// Slice 1 locates time crossings only. Before this refusal the reference
/// reported the activation at the next scheduled event, which is a time the
/// model does not put it — a worse answer than declining to answer.
#[test]
fn a_continuous_crossing_between_instants_is_refused() {
    let model = Model::new()
        .with_variable(Variable::continuous("x", 0.0))
        .with_variable(Variable::discrete("y", Value::Real(0.0)))
        .with_equation(when_assign(after(0.9), "y", Expr::real(1.0)))
        .with_equation(when_assign(
            Expr::binary(BinaryOp::Greater, Expr::var("x"), Expr::real(0.5)),
            "y2",
            Expr::real(1.0),
        ))
        .with_variable(Variable::discrete("y2", Value::Real(0.0)));
    let ramps = Ramps::new(0.0).with("x", 0.0, 1.0);
    assert!(matches!(
        simulate(&model, &ramps, options()),
        Err(RefError::UnlocatedCrossing(_))
    ));
}
