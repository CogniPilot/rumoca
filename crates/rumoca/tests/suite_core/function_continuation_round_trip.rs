//! A `while` loop whose body continues under one invariant condition is one
//! fold with a continuation. The continuation survives the checked DAE wire
//! (replay rebuilds it through the same owner) and lowers to Solve rows that
//! carry the loop state between iterations and stop at the pass the DAE
//! evaluator stops at.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

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

#[test]
fn a_fold_continuation_replays_from_the_wire_and_lowers_to_solve_rows() {
    let compiled = Compiler::new()
        .model("Chains.Walks")
        .compile_str(MODELS, "chains_round_trip.mo")
        .expect("a continued while loop constructs checked DAE");
    let wire = serde_json::to_string(&compiled.dae).expect("the continuation serializes");
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_str(&wire).expect("wire replay rebuilds the continuation");
    assert_eq!(
        serde_json::to_string(&decoded).expect("the replayed DAE serializes"),
        wire,
        "a replayed continuation has one wire representation"
    );

    let options = SimOptions {
        t_end: 0.1,
        dt: Some(0.1),
        ..SimOptions::default()
    };
    let simulation =
        simulate_dae(&decoded, &options).expect("the continuation lowers to computable Solve IR");
    // 3 -> 1 -> 4 -> end: three nodes, the third holds the wanted key.
    // 1 -> 2 -> 1 -> ... is cut off after four nodes and refused.
    for (name, expected) in [
        ("found", 4.0),
        ("steps", 3.0),
        ("accepted", 1.0),
        ("cycleFound", 0.0),
        ("cycleSteps", 4.0),
        ("cycleAccepted", 0.0),
    ] {
        let variable = simulation
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is visible"));
        let value = simulation.data[variable]
            .first()
            .copied()
            .expect("the walk trace is non-empty");
        assert_eq!(value, expected, "{name}");
    }
}
