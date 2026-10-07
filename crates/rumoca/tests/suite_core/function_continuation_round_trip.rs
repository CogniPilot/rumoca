//! A `while` loop whose body continues under one invariant condition is one
//! fold with a continuation. The continuation survives the checked DAE wire
//! (replay rebuilds it through the same owner) and lowers to Solve rows that
//! carry the loop state between iterations and stop at the pass the DAE
//! evaluator stops at. The keys depend on a state so the calls reach Solve
//! lowering instead of folding at translation.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const MODELS: &str = r#"
package Chains
  function walk
    input Integer head;
    input Integer link[:];
    input Real key[:];
    input Real wanted;
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
    Real x(start = 1.0, fixed = true);
    Real found;
    Real steps;
    Real accepted;
    Real cycleFound;
    Real cycleSteps;
    Real cycleAccepted;
  equation
    der(x) = 0;
    (found, steps, accepted) = walk(3, {4, 0, 1, 0}, {10 * x, 20 * x, 30 * x, 40 * x}, 40 * x);
    (cycleFound, cycleSteps, cycleAccepted) = walk(1, {2, 1, 0, 0}, {10 * x, 20 * x, 30 * x, 40 * x}, 99 * x);
  end Walks;
end Chains;
"#;

/// A `for` loop whose whole body is one `if` on a carried flag: the fold
/// continues while the flag is clear, and the function is small enough to be
/// lowered into the equation rows rather than called.
const GATES: &str = r#"
function firstOver
  input Real u;
  input Integer n;
  output Real k;
protected
  Boolean found;
  Real acc;
algorithm
  found := false;
  k := 0;
  acc := 0;
  for i in 1:n loop
    if not found then
      acc := acc + u;
      if acc > 2.5 then
        found := true;
        k := i;
      end if;
    end if;
  end for;
end firstOver;

model Gates
  Real x(start = 1.0, fixed = true);
  Real k;
equation
  der(x) = 0;
  k = firstOver(x, 10);
end Gates;
"#;

#[test]
fn an_inlined_fold_continuation_lowers_to_solve_rows() {
    let compiled = Compiler::new()
        .model("Gates")
        .compile_str(GATES, "gates.mo")
        .expect("a flag-continued for loop constructs checked DAE");
    let options = SimOptions {
        t_end: 0.1,
        dt: Some(0.1),
        ..SimOptions::default()
    };
    let simulation =
        simulate_dae(&compiled.dae, &options).expect("the continuation lowers to Solve rows");
    let variable = simulation
        .names
        .iter()
        .position(|candidate| candidate == "k")
        .expect("k is visible");
    // acc reaches 3.0 on the third iteration; later iterations are skipped.
    assert_eq!(simulation.data[variable].first().copied(), Some(3.0));
}

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
