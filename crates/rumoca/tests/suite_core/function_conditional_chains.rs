//! MLS 3.7 section 12.4.1: a function body is a sequence of assignments, each
//! reading the values the previous ones produced. A long straight chain of
//! conditionals, every one reading its predecessor, lowers to Solve as one typed
//! pure-call owner with a conditional per link. Every phase that walks the
//! chain (call-owner construction, conditional emission, the phasor-source
//! proofs) keeps its pending work on the heap, so the length of the chain is
//! not bounded by the native stack.

use rumoca::Compiler;
use rumoca_ir_solve::{SolveOperation, TypedProgram};
use rumoca_sim::{SimOptions, eval_dae_at};

/// Links in each chain.
const LINKS: usize = 3000;

/// Stack of the lowering thread. A reduction that recurses once per link spends
/// a few hundred bytes per link, so a chain of `LINKS` overflows it; an
/// explicit work stack spends none. Source compilation and runtime
/// inspection have their own fixed stack frames and run on the test thread.
const LOWERING_STACK_BYTES: usize = 256 * 1024;

/// Conditional operations in `program`, nested regions included; a total
/// scalar conditional lowers as one `Select`, so a selection counts as one.
fn conditional_count(program: &TypedProgram) -> usize {
    program
        .operations()
        .iter()
        .map(|spanned| match spanned.operation() {
            SolveOperation::AssignedConditional { program } => {
                1 + conditional_count(program.if_true().body())
                    + conditional_count(program.if_false().body())
            }
            SolveOperation::Conditional {
                if_true, if_false, ..
            } => 1 + conditional_count(if_true.body()) + conditional_count(if_false.body()),
            SolveOperation::Select { .. } => 1,
            _ => 0,
        })
        .sum()
}

fn chain_value(body: &str) -> f64 {
    let source = format!(
        r#"
function Chain
  input Real s;
  output Real y;
protected
  Real x;
algorithm
  x := product({{s, 1.0}});
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
    let lowered = std::thread::scope(|scope| {
        std::thread::Builder::new()
            .name("conditional-chain-lowering".into())
            .stack_size(LOWERING_STACK_BYTES)
            .spawn_scoped(scope, || {
                rumoca_phase_solve::lower_solve_model(
                    &compiled.dae,
                    &std::collections::HashMap::new(),
                    |_| {},
                )
                .expect("the chain lowers through the checked phase owner")
            })
            .expect("the lowering thread starts")
            .join()
            .expect("the chain lowers within the bounded stack")
    });
    let conditionals = lowered
        .model()
        .pure_calls
        .owners()
        .iter()
        .map(|owner| conditional_count(owner.body()))
        .sum::<usize>();
    assert!(
        conditionals >= LINKS,
        "the chain must lower as one conditional per link, found {conditionals}"
    );
    drop(lowered);
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
