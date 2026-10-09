//! MLS 3.7 section 12.4.1: a function body is a sequence of assignments, each
//! reading the values the previous ones produced. A long straight chain of
//! conditionals, every one capturing its predecessor, lowers to Solve with one
//! pending emission per link kept on the heap, so its length is not bounded by
//! the native stack.
//!
//! The array-maximum reduction has no directional body, so scalar
//! differentiation expands the call and exercises conditional capture emission.

use rumoca::Compiler;
use rumoca_ir_solve::{LinearOp, LinearOpSliceKind, SolveVisitor};
use rumoca_sim::{SimOptions, eval_dae_at};

/// Links in each chain.
const LINKS: usize = 3000;

/// Stack of the lowering thread. A capture resolved by a nested call spends a
/// few hundred bytes per link, so a chain of `LINKS` overflows it; the
/// explicit pending-emission stack spends none. Source compilation and runtime
/// inspection have their own fixed stack frames and run on the test thread.
const LOWERING_STACK_BYTES: usize = 256 * 1024;

#[derive(Default)]
struct ConditionalCensus(usize);

impl SolveVisitor for ConditionalCensus {
    type Error = std::convert::Infallible;

    fn visit_linear_op(
        &mut self,
        _kind: LinearOpSliceKind,
        _op_index: usize,
        op: &LinearOp,
    ) -> Result<(), Self::Error> {
        if matches!(op, LinearOp::FunctionConditional { .. }) {
            self.0 += 1;
        }
        Ok(())
    }
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
  x := max({{s, 0.0}});
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
    let mut census = ConditionalCensus::default();
    census
        .visit_solve_model(lowered.model())
        .expect("infallible census");
    assert!(
        census.0 >= LINKS,
        "the chain must exercise scalar conditional emission"
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
