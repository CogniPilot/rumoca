//! `while` loops in Modelica functions whose iteration count has no
//! translation-time proof (MLS 3.7 §11.2.3): a convergence loop ends when its
//! values say so. Such a loop runs under a fixed iteration capacity. Within
//! the capacity it computes what the source computes; a loop that outlives it
//! is an error naming the function and the capacity, never a truncated value.
//! A loop without a proof keeps its refusal when it can also leave through
//! `break`, or when its condition calls a function: the fold would evaluate
//! that condition in every pass of the capacity.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const MODELS: &str = r#"
package Capacity
  function root "Newton iteration: the pass count depends on x alone"
    input Real x;
    output Real y;
  algorithm
    y := x;
    while abs(y*y - x) > 1e-12 loop
      y := (y + x/y)/2;
    end while;
  end root;
  function climb "its condition never fails"
    input Real x;
    output Real y;
  algorithm
    y := x;
    while y > 0 loop
      y := y + 1;
    end while;
  end climb;
  function leave "no proof, and an exit the condition does not describe"
    input Real x;
    output Real y;
  algorithm
    y := x;
    while y > 0 loop
      y := y + 1;
      if y > 5 then
        break;
      end if;
    end while;
  end leave;
  function residual
    input Real y;
    input Real x;
    output Real e;
  algorithm
    e := abs(y*y - x);
  end residual;
  function calling "its condition calls a function, which the fold's exit never tests"
    input Real x;
    output Real y;
  algorithm
    y := x;
    while residual(y, x) > 1e-12 loop
      y := (y + x/y)/2;
    end while;
  end calling;
  model Converging
    Real r = root(2 + time);
  end Converging;
  model Endless
    Real r = climb(1 + time);
  end Endless;
  model Leaving
    Real r = leave(1 + time);
  end Leaving;
  model Calling
    Real r = calling(2 + time);
  end Calling;
end Capacity;
"#;

fn options() -> SimOptions {
    SimOptions {
        t_end: 1.0,
        dt: Some(0.5),
        ..SimOptions::default()
    }
}

#[test]
fn a_while_loop_without_a_proven_bound_runs_to_its_end() {
    let compiled = Compiler::new()
        .model("Capacity.Converging")
        .compile_str(MODELS, "Capacity.mo")
        .expect("a loop without a proven bound compiles under the capacity");
    let result = simulate_dae_with_diagnostics(&compiled.dae, &options())
        .expect("the loop ends well within the capacity");
    let column = result
        .names
        .iter()
        .position(|name| name == "r")
        .expect("the result records r");
    for (time, value) in result.times.iter().zip(&result.data[column]) {
        assert!(
            (value - (2.0 + time).sqrt()).abs() < 1e-9,
            "r({time}) = {value}"
        );
    }
}

#[test]
fn a_loop_that_outlives_the_capacity_is_an_error_naming_its_function() {
    let compiled = Compiler::new()
        .model("Capacity.Endless")
        .compile_str(MODELS, "Capacity.mo")
        .expect("the loop compiles; only running it shows it does not end");
    let error = simulate_dae_with_diagnostics(&compiled.dae, &options())
        .map(|_| ())
        .expect_err("a loop that does not end is an error, not a value");
    let message = format!("{error:?}");
    assert!(
        message.contains("climb") && message.contains("iteration capacity of 10000 passes"),
        "{message}"
    );
}

#[test]
fn a_loop_without_a_proof_that_can_break_keeps_its_refusal() {
    let error = Compiler::new()
        .model("Capacity.Leaving")
        .compile_str(MODELS, "Capacity.mo")
        .map(|_| ())
        .expect_err("the condition does not describe how this loop ends");
    let message = format!("{error:?}");
    assert!(
        message.contains("has no translation-time iteration bound"),
        "{message}"
    );
}

#[test]
fn a_loop_whose_condition_calls_a_function_keeps_its_refusal() {
    let error = Compiler::new()
        .model("Capacity.Calling")
        .compile_str(MODELS, "Capacity.mo")
        .map(|_| ())
        .expect_err("the fold would evaluate this condition in every pass of the capacity");
    let message = format!("{error:?}");
    assert!(
        message.contains("has no translation-time iteration bound"),
        "{message}"
    );
}
