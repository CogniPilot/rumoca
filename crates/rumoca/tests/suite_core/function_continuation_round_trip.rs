//! A `while` loop whose body continues under one invariant condition is one
//! fold with a continuation. The continuation survives the checked DAE wire
//! (replay rebuilds it through the same owner) and lowers to Solve rows that
//! carry the loop state between iterations and stop at the pass the DAE
//! evaluator stops at. The keys depend on a state so the calls reach Solve
//! lowering instead of folding at translation.

use rumoca::Compiler;
use rumoca_ir_solve::{LinearOp, LinearOpSliceKind, SolveVisitor};
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

/// The function's assertion reads the fold's result, so the call-scoped
/// assertion replays the continuation fold inside the equation rows.
const CHECKED_GATES: &str = r#"
function firstOverChecked
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
  assert(found, "no partial sum exceeded the threshold");
end firstOverChecked;

model CheckedGates
  parameter Integer n = 10 annotation(Evaluate = true);
  Real x(start = 1.0, fixed = true);
  Real k;
equation
  der(x) = 0;
  k = firstOverChecked(x, n);
end CheckedGates;

model ShortGates
  extends CheckedGates(n = 2);
end ShortGates;
"#;

#[test]
fn a_call_scoped_assertion_after_a_fold_continuation_replays_it_in_the_rows() {
    let options = SimOptions {
        t_end: 0.1,
        dt: Some(0.1),
        ..SimOptions::default()
    };
    let compiled = Compiler::new()
        .model("CheckedGates")
        .compile_str(CHECKED_GATES, "checked_gates.mo")
        .expect("a checked flag-continued loop constructs checked DAE");
    let simulation =
        simulate_dae(&compiled.dae, &options).expect("the held assertion keeps the simulation");
    let variable = simulation
        .names
        .iter()
        .position(|candidate| candidate == "k")
        .expect("k is visible");
    assert_eq!(simulation.data[variable].first().copied(), Some(3.0));

    let short = Compiler::new()
        .model("ShortGates")
        .compile_str(CHECKED_GATES, "checked_gates.mo")
        .expect("the two-iteration variant constructs checked DAE");
    let error = simulate_dae(&short.dae, &options)
        .expect_err("two partial sums of 1.0 never exceed 2.5, so the assertion fails");
    assert!(
        error
            .to_string()
            .contains("no partial sum exceeded the threshold"),
        "{error}"
    );
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

/// The callee takes an array-maximum reduction, which has no directional
/// body, so scalar differentiation expands the call in place and its
/// flag-continued fold lowers through scalar inline lowering.
const UNDIRECTED_GATES: &str = r#"
function firstOverPeak
  input Real u;
  input Real w[:];
  input Integer n;
  output Real k;
  output Real peak;
protected
  Boolean found;
  Real acc;
algorithm
  found := false;
  k := 0;
  acc := 0;
  peak := max(w) * u;
  for i in 1:n loop
    if not found then
      acc := acc + u;
      if acc > 2.5 then
        found := true;
        k := i;
      end if;
    end if;
  end for;
end firstOverPeak;

model UndirectedGates
  Real x(start = 1.0, fixed = true);
  Real k;
  Real peak;
equation
  der(x) = 0;
  (k, peak) = firstOverPeak(x, {1.0, 4.0, 2.0}, 10);
end UndirectedGates;
"#;

/// Counts the typed calls, conditional regions, and continued fold programs
/// of a Solve model.
#[derive(Default)]
struct FoldCensus {
    typed_calls: usize,
    continued_folds: usize,
    conditional_regions: usize,
}

impl SolveVisitor for FoldCensus {
    type Error = std::convert::Infallible;

    fn visit_linear_op(
        &mut self,
        _kind: LinearOpSliceKind,
        _op_index: usize,
        op: &LinearOp,
    ) -> Result<(), Self::Error> {
        match op {
            LinearOp::PureCall { .. } => self.typed_calls += 1,
            LinearOp::FunctionConditional { .. } => self.conditional_regions += 1,
            LinearOp::FunctionFold { program, .. }
            | LinearOp::GuardedFunctionFold { program, .. }
            | LinearOp::StoreOutputFunctionFold { program, .. }
                if program.continuation.is_some() =>
            {
                self.continued_folds += 1;
            }
            _ => {}
        }
        Ok(())
    }
}

#[test]
fn a_continued_fold_in_a_callee_without_a_directional_body_lowers_inline() {
    let compiled = Compiler::new()
        .model("UndirectedGates")
        .compile_str(UNDIRECTED_GATES, "undirected_gates.mo")
        .expect("a flag-continued loop constructs checked DAE");
    let options = SimOptions {
        t_end: 0.1,
        dt: Some(0.1),
        ..SimOptions::default()
    };
    let model = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &options)
        .expect("the continuation lowers to Solve rows");
    let mut census = FoldCensus::default();
    census
        .visit_solve_model(&model)
        .expect("the census walk is infallible");
    assert_eq!(census.typed_calls, 0, "the call is expanded in place");
    assert!(
        census.continued_folds > 0,
        "the inline path carries the fold continuation"
    );
    let simulation =
        simulate_dae(&compiled.dae, &options).expect("the continuation lowers to Solve rows");
    for (name, expected) in [("k", 3.0), ("peak", 4.0)] {
        let variable = simulation
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is visible"));
        assert_eq!(simulation.data[variable].first().copied(), Some(expected));
    }
}

/// A chain walk under two outer loops: the continued fold's body reads a
/// value built from the outer binders.
const NESTED_WALKS: &str = r#"
function nestedWalk
  input Real u;
  input Real w[:];
  input Integer head[:];
  input Integer link[:];
  input Integer cell[:];
  output Integer found;
  output Integer visited;
  output Real peak;
protected
  Integer node;
  Integer traversed;
  Integer neighbor;
  Boolean valid;
algorithm
  found := 0;
  visited := 0;
  peak := max(w) * u;
  for dx in -1:1 loop
    for dy in 0:1 loop
      neighbor := dx + 2 * dy + 2;
      node := head[neighbor];
      traversed := 0;
      valid := node >= 0 and node <= size(link, 1);
      while node > 0 and valid loop
        if node > size(link, 1) or traversed >= size(link, 1) then
          valid := false;
        else
          traversed := traversed + 1;
          visited := visited + 1;
          if cell[node] == neighbor and (found == 0 or node < found) then
            found := node;
          end if;
          node := link[node];
        end if;
      end while;
    end for;
  end for;
end nestedWalk;

model NestedWalks
  Real x(start = 1.0, fixed = true);
  Integer found;
  Integer visited;
  Real peak;
equation
  der(x) = 0;
  (found, visited, peak) = nestedWalk(x, {1.0, 4.0}, {2, 0, 3, 0, 1}, {0, 4, 5, 0, 0, 0}, {0, 3, 1, 5, 4, 2});
end NestedWalks;
"#;

#[test]
fn a_continued_fold_under_outer_loops_reads_the_outer_binders() {
    let compiled = Compiler::new()
        .model("NestedWalks")
        .compile_str(NESTED_WALKS, "nested_walks.mo")
        .expect("a loop nest with a continued walk constructs checked DAE");
    let options = SimOptions {
        t_end: 0.1,
        dt: Some(0.1),
        ..SimOptions::default()
    };
    let model = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &options)
        .expect("the nested continuation lowers to Solve rows");
    let mut census = FoldCensus::default();
    census
        .visit_solve_model(&model)
        .expect("the census walk is infallible");
    assert_eq!(census.typed_calls, 0, "the call is expanded in place");
    let simulation =
        simulate_dae(&compiled.dae, &options).expect("the nested continuation simulates");
    let value = |name: &str| {
        let variable = simulation
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is visible"));
        simulation.data[variable].first().copied()
    };
    assert_eq!(value("peak"), Some(4.0));
}

/// A fold-bearing callee with its own guarded folds, called from an outer
/// loop with a slice of the loop binder.
const GUARDED_FOLDS_UNDER_LOOP: &str = r#"
function orthogonal
  input Real R[3, 3];
  output Boolean valid;
protected
  Real gram[3, 3];
algorithm
  valid := true;
  for a in 1:3 loop
    for b in 1:3 loop
      valid := valid and abs(R[a, b]) <= 1.000001;
    end for;
  end for;
  if valid then
    gram := transpose(R) * R;
    for a in 1:3 loop
      for b in 1:3 loop
        valid := valid and abs(gram[a, b] - (if a == b then 1.0 else 0.0)) <= 1e-6;
      end for;
    end for;
  end if;
end orthogonal;

function sweep
  input Real u;
  input Real w[:];
  input Real rotations[:, 3, 3];
  output Boolean valid;
  output Real peak;
algorithm
  peak := max(w) * u;
  valid := true;
  for slot in 1:size(rotations, 1) loop
    valid := valid and orthogonal(rotations[slot, :, :]);
  end for;
end sweep;

model SweepBroken
  Real x(start = 1.0, fixed = true);
  Boolean valid;
  Real peak;
equation
  der(x) = 0;
  (valid, peak) = sweep(x, {1.0, 4.0}, {identity(3), 2 * identity(3)});
end SweepBroken;

model Sweep
  Real x(start = 1.0, fixed = true);
  Boolean valid;
  Real peak;
equation
  der(x) = 0;
  (valid, peak) = sweep(x, {1.0, 4.0}, {identity(3), [0, -1, 0; 1, 0, 0; 0, 0, 1]});
end Sweep;
"#;

#[test]
fn a_guarded_fold_in_a_callee_under_an_outer_loop_lowers_inline() {
    let compiled = Compiler::new()
        .model("Sweep")
        .compile_str(GUARDED_FOLDS_UNDER_LOOP, "sweep.mo")
        .expect("a loop over guarded folds constructs checked DAE");
    let options = SimOptions {
        t_end: 0.1,
        dt: Some(0.1),
        ..SimOptions::default()
    };
    let model = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &options)
        .expect("the guarded folds under the outer loop lower to Solve rows");
    let mut census = FoldCensus::default();
    census
        .visit_solve_model(&model)
        .expect("the census walk is infallible");
    assert_eq!(census.typed_calls, 0, "the call is expanded in place");
    assert!(
        census.conditional_regions > 0,
        "the guarded folds lower as conditional regions"
    );
    let simulation = simulate_dae(&compiled.dae, &options).expect("the sweep simulates");
    let first = |name: &str| {
        let variable = simulation
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is visible"));
        simulation.data[variable].first().copied()
    };
    assert_eq!(first("valid"), Some(1.0));
    assert_eq!(first("peak"), Some(4.0));

    let broken = Compiler::new()
        .model("SweepBroken")
        .compile_str(GUARDED_FOLDS_UNDER_LOOP, "sweep.mo")
        .expect("the non-orthogonal variant constructs checked DAE");
    let simulation = simulate_dae(&broken.dae, &options).expect("the broken sweep simulates");
    let variable = simulation
        .names
        .iter()
        .position(|candidate| candidate == "valid")
        .expect("valid is visible");
    assert_eq!(simulation.data[variable].first().copied(), Some(0.0));
}

/// A carried value of an outer loop guards a fold inside the callee.
const CARRIED_GUARD: &str = r#"
function gated
  input Real a;
  input Real R[3, 3];
  output Real y;
algorithm
  y := a;
  if a > 0 then
    for i in 1:3 loop
      y := y + R[i, i];
    end for;
  end if;
end gated;

function chain
  input Real u;
  input Real w[:];
  input Real rotations[:, 3, 3];
  output Real total;
  output Real peak;
algorithm
  peak := max(w) * u;
  total := u;
  for slot in 1:size(rotations, 1) loop
    total := gated(total, rotations[slot, :, :]);
  end for;
end chain;

model Chain
  Real x(start = 1.0, fixed = true);
  Real total;
  Real peak;
equation
  der(x) = 0;
  (total, peak) = chain(x, {1.0, 4.0}, {identity(3), identity(3)});
end Chain;
"#;

#[test]
fn a_carried_value_of_an_outer_loop_guards_a_fold_in_a_callee() {
    let compiled = Compiler::new()
        .model("Chain")
        .compile_str(CARRIED_GUARD, "chain.mo")
        .expect("a guarded fold under a carried value constructs checked DAE");
    let options = SimOptions {
        t_end: 0.1,
        dt: Some(0.1),
        ..SimOptions::default()
    };
    let simulation = simulate_dae(&compiled.dae, &options)
        .expect("the carried guard lowers to Solve rows");
    for (name, expected) in [("total", 7.0), ("peak", 4.0)] {
        let variable = simulation
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is visible"));
        assert_eq!(simulation.data[variable].first().copied(), Some(expected));
    }
}
