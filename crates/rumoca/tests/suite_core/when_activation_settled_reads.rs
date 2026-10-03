//! A `when` body active at an event reads the values settled at that event.
//!
//! MLS §8.3.5.1 reads `when c then v = e; end when` as `b = c; v = if
//! edge(b) then e else pre(v)`, and Appendix B solves the equations of one
//! event iteration simultaneously with every `pre` fixed. The activation
//! buffer is `pre(b)`, so it holds while the iteration settles: a body that
//! reads a discrete value defined at the same instant takes that value, not
//! the one the value had when the event began.
//!
//! `initial()` holds for the initialization pass only (MLS §8.6): the event
//! iteration that follows it at the same instant sees `initial()` false, so a
//! `change` that the advanced `pre` values cause there is an event of its own.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const SOURCE: &str = r#"
package P
  model InitialRead
    Integer x(start = 5);
    Integer lh;
  equation
    x = 1;
    when initial() then
      lh = x;
    end when;
  end InitialRead;
  model InitialAlgorithmRead
    Integer x(start = 5, fixed = true);
    Integer lh;
  algorithm
    when initial() then
      lh := x;
    end when;
  equation
    x = if time < 0.5 then 1 else 2;
  end InitialAlgorithmRead;
  model LaterRead
    Integer x;
    Integer y;
    Integer lh(start = 0, fixed = true);
    Integer count(start = 0, fixed = true);
  equation
    x = if time < 0.5 then 1 else 2;
    y = 10 * x;
    when time >= 0.5 then
      lh = y;
      count = pre(count) + 1;
    end when;
  end LaterRead;
  model InitialFollowUp
    Integer a(start = 0, fixed = true);
    Integer x;
    Integer n(start = 0, fixed = true);
    Integer seen;
  equation
    a = 1;
    x = pre(a);
    when {initial(), change(x) and not initial()} then
      n = pre(n) + 1;
      seen = x;
    end when;
  end InitialFollowUp;
end P;
"#;

fn final_values(model: &str, names: &[&str]) -> Vec<f64> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, "WhenActivationSettledReads.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} simulates: {error:?}"));
    names
        .iter()
        .map(|name| {
            let index = result
                .names
                .iter()
                .position(|candidate| candidate == name)
                .unwrap_or_else(|| panic!("{model} exposes {name}"));
            *result.data[index].last().expect("a simulated trace")
        })
        .collect()
}

#[test]
fn an_initial_when_reads_the_settled_initial_value() {
    assert_eq!(final_values("P.InitialRead", &["lh"]), [1.0]);
    assert_eq!(final_values("P.InitialAlgorithmRead", &["lh"]), [1.0]);
}

#[test]
fn a_later_when_reads_values_settled_at_its_event_and_runs_once() {
    assert_eq!(final_values("P.LaterRead", &["lh", "count"]), [20.0, 1.0]);
}

#[test]
fn the_event_iteration_after_initialization_sees_initial_false() {
    assert_eq!(
        final_values("P.InitialFollowUp", &["n", "seen"]),
        [2.0, 1.0]
    );
}
