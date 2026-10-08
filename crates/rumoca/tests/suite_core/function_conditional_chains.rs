//! MLS 3.7 section 12.4.1: a function body is a sequence of assignments, each
//! reading the values the previous ones produced. A long straight chain of
//! conditionals, every one capturing its predecessor, lowers to Solve with one
//! pending emission per link kept on the heap, so its length is not bounded by
//! the native stack.
//!
//! The function input is a model input, so the call has no derivative relation
//! and Solve lowering expands its body in place.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

/// Links in each chain.
const LINKS: usize = 3000;

/// Stack of the lowering thread. A capture resolved by a nested call spends a
/// few hundred bytes per link, so a chain of `LINKS` overflows it; the
/// explicit pending-emission stack spends none.
const LOWERING_STACK_BYTES: usize = 256 * 1024;

fn chain_value(body: &str) -> f64 {
    let body = body.to_string();
    std::thread::Builder::new()
        .stack_size(LOWERING_STACK_BYTES)
        .spawn(move || lower_chain(&body))
        .expect("the lowering thread starts")
        .join()
        .expect("the chain lowers within the bounded stack")
}

fn lower_chain(body: &str) -> f64 {
    let source = format!(
        r#"
function Chain
  input Real s;
  output Real y;
protected
  Real x;
algorithm
  x := s;
{body}
  y := x;
end Chain;
model Probe
  input Real s = 0.5;
  output Real r;
equation
  r = Chain(s);
end Probe;
"#
    );
    let compiled = Compiler::new()
        .model("Probe")
        .compile_str(&source, "Probe.mo")
        .unwrap_or_else(|error| panic!("the chain compiles: {error}"));
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .unwrap_or_else(|error| panic!("the chain lowers to Solve: {error}"));
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name == "r")
        .expect("the chain result is a solver output")
        .value
}

#[test]
fn a_chain_of_conditional_values_longer_than_the_native_stack_allows_lowers() {
    // Every link adds one while the value stays positive: 0.5 + 3000.
    let body = "  x := if x > 0 then x + 1 else x - 1;\n".repeat(LINKS);
    assert_eq!(chain_value(&body), 0.5 + LINKS as f64);
}
