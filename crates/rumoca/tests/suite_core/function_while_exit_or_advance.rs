//! A `while` loop over a linked chain (MLS 3.7 §11.2.3) whose condition has
//! no counter conjunct: `while node > 0 and valid loop`. Every pass either
//! sets `valid := false` (a corrupt index or a chain longer than the table)
//! or advances `traversed` while its guard proves `traversed < size(link, 1)`,
//! so at most `size(link, 1) + 1` passes run. The loop is one compact fold
//! that ends at the first pass whose condition is false, so a short chain
//! costs its own length, and a cycle is cut off by the table size.

use rumoca::Compiler;
use rumoca_ir_dae as dae;
use rumoca_sim::{SimOptions, SimResult, eval_dae_at, simulate_dae_with_diagnostics};

const MODELS: &str = r#"
package Chains
  function walk
    input Integer head;
    input Integer link[:];
    input Integer key[:];
    input Integer wanted;
    output Real found;
    output Real steps;
    output Real accepted;
  protected
    Integer node;
    Integer traversed;
    Boolean valid;
  algorithm
    found := 0;
    steps := 0;
    valid := true;
    node := head;
    traversed := 0;
    while node > 0 and valid loop
      if node > size(link, 1) or traversed >= size(link, 1) then
        valid := false;
      else
        traversed := traversed + 1;
        steps := steps + 1;
        if key[node] == wanted then
          found := node;
        end if;
        node := link[node];
      end if;
    end while;
    accepted := if valid then 1 else 0;
  end walk;

  model Walks
    Real found;
    Real steps;
    Real accepted;
    Real cycleFound;
    Real cycleSteps;
    Real cycleAccepted;
  equation
    (found, steps, accepted) = walk(3, {4, 0, 1, 0}, {10, 20, 30, 40}, 40);
    (cycleFound, cycleSteps, cycleAccepted) = walk(1, {2, 1, 0, 0}, {10, 20, 30, 40}, 99);
  end Walks;
end Chains;
"#;

fn compile() -> rumoca::CompilationResult {
    Compiler::new()
        .model("Chains.Walks")
        .compile_str(MODELS, "Chains.mo")
        .unwrap_or_else(|error| panic!("Chains.Walks should compile: {error}"))
}

fn at_start(result: &SimResult, name: &str) -> f64 {
    let column = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("the result records {name}"));
    result.data[column][0]
}

#[test]
fn a_chain_walk_stops_at_the_end_of_the_chain_and_a_cycle_at_the_table_size() {
    let compiled = compile();
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.1),
            ..SimOptions::default()
        },
    )
    .expect("Chains.Walks simulates");
    // 3 -> 1 -> 4 -> end: three nodes, the third holds the wanted key.
    assert_eq!(at_start(&result, "found"), 4.0);
    assert_eq!(at_start(&result, "steps"), 3.0);
    assert_eq!(at_start(&result, "accepted"), 1.0);
    // 1 -> 2 -> 1 -> ... is cut off after four nodes and refused.
    assert_eq!(at_start(&result, "cycleFound"), 0.0);
    assert_eq!(at_start(&result, "cycleSteps"), 4.0);
    assert_eq!(at_start(&result, "cycleAccepted"), 0.0);
}

#[test]
fn the_dae_evaluator_ends_the_walk_at_the_same_pass() {
    let compiled = compile();
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("Chains.Walks evaluates");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let value = |name: &str| {
        probe
            .report
            .solver_y
            .iter()
            .find(|slot| slot.name == name)
            .unwrap_or_else(|| panic!("Chains.Walks has {name}"))
            .value
    };
    assert_eq!(
        ["found", "steps", "accepted", "cycleSteps", "cycleAccepted"].map(value),
        [4.0, 3.0, 1.0, 4.0, 0.0]
    );
}

#[test]
fn the_while_loop_is_one_fold_that_ends_at_its_condition() {
    let compiled = compile();
    compiled.dae.inspect(|view| {
        let function = (0..view.function_count())
            .filter_map(|index| view.function_id(index).and_then(|id| view.function(id)))
            .find(|function| function.name().as_str().ends_with("walk"))
            .expect("the walk function remains visible");
        let fold = function
            .statements()
            .find_map(|statement| match statement {
                dae::FunctionStatementView::For { fold, .. } => Some(fold),
                _ => None,
            })
            .and_then(|fold| view.function_fold(fold))
            .expect("the while loop is one compact fold");
        assert!(
            fold.continuation().is_some(),
            "the walk loop carries its condition as a continuation"
        );
    });
}
