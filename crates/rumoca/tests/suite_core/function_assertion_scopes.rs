//! Assertions of calls that a function reaches inside a loop body or in the
//! first condition of a correlated conditional (MLS 3.7 §8.3.7, §11.2.8.1).
//!
//! The first failing assertion ends the function: inside a `for` loop that is
//! the first failing iteration, and a call in a conditional's condition runs
//! before either branch. Both shapes occur in `Modelica.Media.Water.IF97`
//! property functions.

use rumoca::Compiler;
use rumoca_eval_solve::{
    AssertionReport, CheckedEventObservationContext, RowEvalContext,
    eval_scalar_program_block_with_context,
};
use rumoca_ir_solve::SolveEventActionKind;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model Scopes
  function g
    input Real x;
    output Real y;
  algorithm
    assert(x > -10, "x too small: " + String(x));
    y := 2*x;
  end g;
  function sumg
    input Real x;
    input Integer n;
    output Real s;
  algorithm
    s := 0;
    for i in 1:n loop
      s := 0.5*s + g(x + i);
    end for;
  end sumg;
  function h
    input Real x;
    output Real y;
  algorithm
    assert(x < 10, "x too large");
    y := x;
  end h;
  function pick
    input Real x;
    output Real y;
    output Real z;
  algorithm
    if h(x) > 0.5 then
      y := 1;
      z := 2;
    else
      y := 3;
      z := 4;
    end if;
  end pick;
  Real s = sumg(time + OFFSET, 3);
  Real a;
  Real b;
equation
  (a, b) = pick(time + SHIFT);
end Scopes;
"#;

fn simulate(offset: &str, shift: &str) -> Result<SimResult, String> {
    let source = source(offset, shift);
    let compiled = Compiler::new()
        .model("Scopes")
        .compile_str(&source, "Scopes.mo")
        .unwrap_or_else(|error| panic!("Scopes compiles: {error:?}"));
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .map_err(|error| format!("{error:?}"))
}

fn column<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{name} is recorded"));
    &result.data[index]
}

#[test]
fn calls_in_loops_and_conditions_keep_their_values_when_assertions_hold() {
    let result = simulate("0", "0").expect("Scopes simulates");
    for (sample, time) in result.times.iter().enumerate() {
        let s = column(&result, "s")[sample];
        // s = 0.25 g(x + 1) + 0.5 g(x + 2) + g(x + 3) with g(v) = 2 v.
        assert!((s - (3.5 * time + 8.5)).abs() < 1e-9, "s({time}) = {s}");
        let (a, b) = (column(&result, "a")[sample], column(&result, "b")[sample]);
        let (ea, eb) = if *time > 0.5 { (1.0, 2.0) } else { (3.0, 4.0) };
        if (time - 0.5).abs() > 1e-6 {
            assert!(
                (a - ea).abs() < 1e-12 && (b - eb).abs() < 1e-12,
                "({a}, {b}) at {time}"
            );
        }
    }
}

// SPEC_0036's source-ordered assertion cutover separates the values a row
// computes from C25's checked message observation: the row expands the call
// for its values, and only the exact table/program/action context consumes the
// stopped invocation and its initialized first-failure captures.
fn source(offset: &str, shift: &str) -> String {
    SOURCE.replace("OFFSET", offset).replace("SHIFT", shift)
}

fn checked_reports(offset: &str, shift: &str) -> Vec<AssertionReport> {
    let source = source(offset, shift);
    let compiled = Compiler::new()
        .model("Scopes")
        .compile_str(&source, "Scopes.mo")
        .unwrap_or_else(|error| panic!("the original Scopes source compiles: {error}"));
    let model = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &SimOptions::default())
        .expect("the original Scopes source retains its checked call/action owners");
    let events = &model.problem.events;
    let observations = CheckedEventObservationContext::construct(
        &model.pure_calls,
        &events.action_conditions,
        &events.actions,
    )
    .expect("the original table, program and actions issue one observation context");
    let mut predicates = vec![0.0; events.action_conditions.len()];
    eval_scalar_program_block_with_context(
        &events.action_conditions,
        &model.initial_y.to_vec(),
        &model.parameters.to_vec(),
        0.0,
        RowEvalContext {
            pure_calls: Some(&model.pure_calls),
            event_observations: Some(&observations),
            ..Default::default()
        },
        &mut predicates,
    )
    .expect("predicate-only observation consumes the actual stopped call");
    let reports = observations.take_reports();
    for report in &reports {
        let action = &events.actions[report.action_index];
        assert_eq!(report.kind, action.kind);
        assert_eq!(report.span, action.span);
        let provenance = rumoca_ir_dae::DaeProvenance::source(report.span)
            .expect("the issued action retains its original source coordinate");
        compiled.dae.inspect(|view| {
            assert!(
                (0..view.function_count())
                    .filter_map(|index| view.function(view.function_id(index).unwrap()))
                    .any(|function| function.statements().any(|statement| matches!(
                        statement,
                        rumoca_ir_dae::FunctionStatementView::Assertion { provenance, .. }
                            if provenance.span() == report.span
                    ))),
                "C25 identifies an original checked authored assertion, not its model call"
            );
            assert_eq!(
                view.source_text(provenance),
                source.get(report.span.start.0..report.span.end.0),
                "the checked report belongs to the original source map"
            );
        });
    }
    reports
}

fn assert_failure(offset: &str, shift: &str, call: &str, assertion: &str, message: &str) {
    let source = source(offset, shift);
    let error = simulate(offset, shift).expect_err("the original reached assertion stops the call");
    assert!(
        error.contains(&format!(
            "Modelica assert failed at t=0.000000000: {message}"
        )),
        "the scheduled assertion owner reports its authored message: {error}"
    );
    assert!(
        source.contains(call),
        "the original reached model call is part of the source"
    );
    let reports = checked_reports(offset, shift);
    let [report] = reports.as_slice() else {
        panic!(
            "the first fatal check produces one observation, got {}",
            reports.len()
        );
    };
    assert_eq!(report.kind, SolveEventActionKind::Assert);
    assert_eq!(report.message, message);
    let assertion_start = source
        .find(assertion)
        .expect("the original authored assertion");
    assert_eq!(report.span.start.0, assertion_start);
    let authored = source
        .get(report.span.start.0..report.span.end.0)
        .expect("the canonical authored assertion span belongs to the original source");
    assert_eq!(authored, "assert", "the original checked assertion keyword");
    assert!(
        checked_reports("0", "0").is_empty(),
        "healthy calls observe no failed assertion"
    );
}

#[test]
fn the_first_failing_loop_iteration_ends_the_call() {
    assert_failure(
        "(-13)",
        "0",
        "sumg(time + (-13), 3)",
        "assert(x > -10,",
        "x too small: -12",
    );
}

#[test]
fn a_call_in_a_condition_asserts_before_either_branch() {
    assert_failure(
        "0",
        "20",
        "pick(time + 20)",
        "assert(x < 10,",
        "x too large",
    );
}
