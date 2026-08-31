//! Call-specialized function-assertion messages (SPEC_0040 SOLVE-C25).
//!
//! A function assertion reached through a call-specialized schedule renders
//! its message from the same shared pure-call owner the root and action
//! projections consume, so a converted value is a projection of that owner
//! rather than a second copy of the function body. A message whose evaluation
//! would invoke a Modelica function is refused at its own span instead of
//! being hoisted into an unconditional call-scoped effect that fires while the
//! enclosing assertion still holds.

use rumoca::Compiler;
use rumoca_ir_solve as solve;
use rumoca_sim::{SimOptions, SimResult, simulate_dae};

const CONCATENATED_MESSAGE: &str = r#"
model ConcatenatedMessage
  function f
    input Real u;
    output Real y;
  algorithm
    y := 3*u;
    assert(y < 4.0, "f rejects: " + "too big");
  end f;
  Real x(start = 1.0, fixed = true);
equation
  der(x) = -f(x);
end ConcatenatedMessage;
"#;

const CONVERTED_RESULT_MESSAGE: &str = r#"
model ConvertedResultMessage
  function f
    input Real u;
    output Real y;
  algorithm
    y := 3*u;
    assert(y < 4.0, "f rejects: " + String(y) + " from u=" + String(u));
  end f;
  Real x(start = 1.0, fixed = true);
equation
  der(x) = -f(x);
end ConvertedResultMessage;
"#;

const EAGER_MESSAGE_CALL: &str = r#"
model EagerCallLit
  function sq
    input Real u;
    output Real y;
  algorithm
    assert(u > 0.6, "sq rejects");
    y := u*u;
  end sq;
  function run
    input Real u;
    output Real y;
  algorithm
    assert(u > -100.0, "never shown: " + String(sq(u)));
    y := u + 1.0;
  end run;
  Real x(start = 1.0, fixed = true);
equation
  der(x) = -run(x);
end EagerCallLit;
"#;

// The converted argument is itself another call's result. The projection must
// read the value that call already computed for this owner's input rather than
// invoking `gate` a second time inside the message.
const ARGUMENT_FROM_A_CALL: &str = r#"
model ArgCallMessage
  function gate
    input Real u;
    output Real y;
  algorithm
    assert(u > -100.0, "gate rejects");
    y := u + 0.5;
  end gate;
  function f
    input Real u;
    output Real y;
  algorithm
    y := 3*u;
    assert(y < 6.0, "f rejects: u=" + String(u) + " y=" + String(y));
  end f;
  Real x(start = 1.0, fixed = true);
equation
  der(x) = f(gate(x));
end ArgCallMessage;
"#;

fn series<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("simulation result missing column {name}"));
    result.data[index].as_slice()
}

fn lower(source: &str, model: &str, file: &str) -> rumoca_phase_solve::LoweredSolvePackage {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, file)
        .unwrap_or_else(|error| panic!("{model} compiles: {error}"));
    rumoca_phase_solve::lower_solve_package(compiled.dae())
        .unwrap_or_else(|error| panic!("{model} lowers: {error}"))
}

fn simulate(source: &str, model: &str, file: &str, t_end: f64) -> SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, file)
        .unwrap_or_else(|error| panic!("{model} compiles: {error}"));
    simulate_dae(
        compiled.dae(),
        &SimOptions {
            t_end,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} simulates: {error}"))
}

/// Every `PureCall` owner a program projects, in emission order.
fn projected_owners(program: &[solve::LinearOp]) -> Vec<solve::SolvePureCallOwnerId> {
    program
        .iter()
        .filter_map(|op| match op {
            solve::LinearOp::PureCall { site, .. } => Some(site.owner()),
            _ => None,
        })
        .collect()
}

fn message_conversions(action: &solve::SolveEventAction) -> Vec<&[solve::LinearOp]> {
    action
        .message
        .parts
        .iter()
        .filter_map(|part| match part {
            solve::SolveEventMessagePart::Conversion { value, .. } => Some(value.as_slice()),
            solve::SolveEventMessagePart::Text(_) => None,
        })
        .collect()
}

fn assertion_message(package: &rumoca_phase_solve::LoweredSolvePackage, y: &[f64]) -> String {
    let events = package.problem.events();
    let request = rumoca_eval_solve::eval_event_action_request(
        events,
        y,
        &vec![0.0; package.problem.layout().p_scalars()],
        0.0,
        rumoca_eval_solve::RowEvalContext {
            pure_calls: Some(&package.pure_calls),
            ..Default::default()
        },
    )
    .expect("the call-specialized assertion action evaluates");
    match request {
        rumoca_eval_solve::EventActionRequest::AssertionFailed { message } => message,
        other => panic!("expected a failed assertion at y={y:?}, got {other:?}"),
    }
}

#[test]
fn concatenated_literal_message_keeps_its_source_order_and_simulates() {
    let package = lower(
        CONCATENATED_MESSAGE,
        "ConcatenatedMessage",
        "ConcatenatedMessage.mo",
    );
    let [action] = package.problem.events().actions.as_slice() else {
        panic!("one call-specialized assertion owns one action");
    };
    assert_eq!(
        action.message.parts,
        vec![
            solve::SolveEventMessagePart::Text("f rejects: ".to_string()),
            solve::SolveEventMessagePart::Text("too big".to_string()),
        ],
        "a concatenated literal message keeps one text part per operand"
    );

    let result = simulate(
        CONCATENATED_MESSAGE,
        "ConcatenatedMessage",
        "ConcatenatedMessage.mo",
        0.5,
    );
    let xs = series(&result, "x");
    let ts = &result.times;
    assert_eq!(xs.len(), ts.len());
    for (t, x) in ts.iter().zip(xs) {
        let expected = (-3.0 * t).exp();
        assert!(
            (x - expected).abs() < 1e-5,
            "der(x) = -3x at t={t}: simulated {x}, closed form {expected}"
        );
    }
}

#[test]
fn converted_message_projects_the_shared_call_owner_instead_of_the_body() {
    let package = lower(
        CONVERTED_RESULT_MESSAGE,
        "ConvertedResultMessage",
        "ConvertedResultMessage.mo",
    );
    let events = package.problem.events();
    let [action] = events.actions.as_slice() else {
        panic!("one call-specialized assertion owns one action");
    };
    let [action_program] = events.action_conditions.programs() else {
        panic!("one call-specialized assertion owns one action-condition program");
    };
    let action_owners = projected_owners(action_program);
    let [action_owner] = action_owners.as_slice() else {
        panic!("the assertion-action projection consumes exactly one call owner");
    };
    let action_owner = *action_owner;

    let conversions = message_conversions(action);
    assert_eq!(
        conversions.len(),
        2,
        "the message converts the call result and the call argument"
    );
    for conversion in conversions {
        assert_eq!(
            projected_owners(conversion),
            vec![action_owner],
            "SOLVE-C25: a converted message value projects the sibling call owner"
        );
        assert!(
            !conversion
                .iter()
                .any(|op| matches!(op, solve::LinearOp::Binary { .. })),
            "SOLVE-C25: a converted message value does not rebuild the function body: {conversion:?}"
        );
    }
}

#[test]
fn converted_message_renders_the_projected_result_and_argument() {
    let package = lower(
        CONVERTED_RESULT_MESSAGE,
        "ConvertedResultMessage",
        "ConvertedResultMessage.mo",
    );
    // f(2) = 6 violates `y < 4`, so the action fires and renders both the
    // projected result and the projected argument of that same call.
    assert_eq!(assertion_message(&package, &[2.0]), "f rejects: 6 from u=2");
    assert_eq!(
        assertion_message(&package, &[1.5]),
        "f rejects: 4.5 from u=1.5"
    );
}

#[test]
fn a_converted_argument_reads_the_value_the_owner_was_already_given() {
    let compiled = Compiler::new()
        .model("ArgCallMessage")
        .compile_str(ARGUMENT_FROM_A_CALL, "ArgCallMessage.mo")
        .expect("ArgCallMessage compiles");
    let error = simulate_dae(
        compiled.dae(),
        &SimOptions {
            t_end: 0.5,
            ..SimOptions::default()
        },
    )
    .expect_err("`y < 6` is violated once x reaches 1.5");
    // x = 1.5*exp(3t) - 0.5 reaches 1.5 at t = ln(4/3)/3, where the owner was
    // called with gate(1.5) = 2 and returned 3*2 = 6.
    assert_eq!(
        error.to_string(),
        "Modelica assert failed at t=0.095894271: f rejects: u=2 y=6"
    );
}

#[test]
fn safe_converted_message_never_renders_and_the_run_stays_exact() {
    let result = simulate(
        CONVERTED_RESULT_MESSAGE,
        "ConvertedResultMessage",
        "ConvertedResultMessage.mo",
        0.5,
    );
    let xs = series(&result, "x");
    for (t, x) in result.times.iter().zip(xs) {
        let expected = (-3.0 * t).exp();
        assert!(
            (x - expected).abs() < 1e-5,
            "der(x) = -3x at t={t}: simulated {x}, closed form {expected}"
        );
    }
}

#[test]
fn a_message_that_would_call_a_function_is_refused_at_its_own_span() {
    let compiled = Compiler::new()
        .model("EagerCallLit")
        .compile_str(EAGER_MESSAGE_CALL, "EagerCallLit.mo")
        .expect("the model itself is well typed");
    let error = rumoca_phase_solve::lower_solve_package(compiled.dae())
        .err()
        .expect("a message that would invoke a function is refused");
    assert_eq!(
        error.code(),
        rumoca_phase_solve::diagnostic_codes::EL001_UNSUPPORTED_EXPRESSION,
        "an unsupported message shape keeps its own unsupported-feature code: {error}"
    );
    assert!(
        error.source_span().is_some(),
        "the refusal names a source span: {error}"
    );
    assert!(
        error
            .to_string()
            .contains("call-specialized assertion message"),
        "the refusal names the construct it rejects: {error}"
    );
}

#[test]
fn a_refused_message_never_fires_the_nested_assertion_it_would_have_called() {
    let compiled = Compiler::new()
        .model("EagerCallLit")
        .compile_str(EAGER_MESSAGE_CALL, "EagerCallLit.mo")
        .expect("the model itself is well typed");
    // `run`'s condition holds for every reachable state, so no message may be
    // rendered and `sq`'s assertion may never be reached. Compiling the message
    // eagerly must not turn that unreachable effect into a live action row.
    let error = simulate_dae(
        compiled.dae(),
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect_err("the refused message keeps the model out of the solver");
    let reported = error.to_string();
    assert!(
        !reported.contains("sq rejects"),
        "a message-only call must not schedule the callee's assertion: {reported}"
    );
    assert!(
        reported.contains("call-specialized assertion message"),
        "the run stops at the refusal, not at a spurious abort: {reported}"
    );
}
