//! MLS 3.6 §11.2.6 / §11.2.2: an `if` statement selects its branch once, at
//! the statement, and the selected sequence runs to completion even when it
//! changes the values its conditions read. A conditional inside a loop body
//! selects again on every iteration. Every expected value below is computed by
//! hand from those rules; a live re-evaluation of a mutated predicate gives a
//! different, stated result.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const MODELS: &str = r#"
function mutableGate
  input Boolean mask[3];
  output Real y[3];
protected
  Boolean valid;
  Real s;
algorithm
  y := zeros(3);
  valid := false;
  s := 0.0;
  for i in 1:3 loop
    valid := mask[i];
    s := 0.0;
    if valid then
      for j in 1:2 loop
        s := s + 1.0;
        valid := false;
      end for;
    end if;
    y[i] := if valid then s else -s;
  end for;
end mutableGate;

model MutableGate
  input Boolean mask[3] = {true, false, true};
  output Real y[3];
equation
  y = mutableGate(mask);
end MutableGate;

model ReseededGate
  input Boolean mask[3] = {false, true, false};
  output Real y[3];
equation
  y = mutableGate(mask);
end ReseededGate;

function toggledGate
  input Boolean mask[3];
  output Real y[3];
protected
  Boolean valid;
  Real count;
algorithm
  y := zeros(3);
  valid := false;
  count := 0.0;
  for i in 1:3 loop
    valid := mask[i];
    count := 0.0;
    if valid then
      for j in 1:2 loop
        count := count + 1.0;
        valid := not valid;
      end for;
    end if;
    y[i] := if valid then count else -count;
  end for;
end toggledGate;

model ToggledGate
  input Boolean mask[3] = {true, false, true};
  output Real y[3];
equation
  y = toggledGate(mask);
end ToggledGate;

function nestedGuardSnapshot
  input Real enabled[4];
  input Real checks[4,2];
  input Integer partner[4];
  input Real table[4];
  output Real observed[4,2];
  output Real accepted[4];
protected
  Boolean valid;
algorithm
  observed := zeros(4,2);
  accepted := zeros(4);
  valid := false;
  for sample in 1:4 loop
    valid := enabled[sample] > 0.0;
    if valid then
      for coordinate in 1:2 loop
        observed[sample,coordinate] := table[partner[sample]] + coordinate;
        valid := valid and checks[sample,coordinate] >= 0.0;
      end for;
      if valid then
        accepted[sample] := 1.0;
      end if;
    end if;
  end for;
end nestedGuardSnapshot;

model NestedConditionalLoop
  input Real enabled[4] = {1.0,1.0,0.0,1.0};
  input Real checks[4,2] = [-1.0,1.0;1.0,1.0;1.0,1.0;1.0,1.0];
  input Integer partner[4] = {1,2,99,4};
  input Real table[4] = {10.0,20.0,30.0,40.0};
  output Real observed[4,2];
  output Real accepted[4];
equation
  (observed,accepted) = nestedGuardSnapshot(enabled,checks,partner,table);
end NestedConditionalLoop;

function gatherGate
  input Boolean mask[3];
  input Integer rows[3];
  input Real samples[:];
  output Real y[3];
protected
  Boolean valid;
algorithm
  y := zeros(3);
  valid := false;
  for i in 1:3 loop
    valid := mask[i];
    if valid then
      for j in 1:2 loop
        y[i] := y[i] + samples[rows[i]];
        valid := false;
      end for;
    end if;
  end for;
end gatherGate;

model FalseGather
  input Boolean mask[3] = {false, false, false};
  input Integer rows[3] = {999, 999, 999};
  input Real samples[1] = {3.0};
  output Real y[3];
equation
  y = gatherGate(mask, rows, samples);
end FalseGather;

model MixedGather
  input Boolean mask[3] = {false, true, false};
  input Integer rows[3] = {999, 1, 999};
  input Real samples[1] = {3.0};
  output Real y[3];
equation
  y = gatherGate(mask, rows, samples);
end MixedGather;

function firstTrueGate
  input Boolean selected[3];
  input Real samples[:];
  input Integer row;
  output Real y[3];
protected
  Boolean valid;
algorithm
  y := zeros(3);
  valid := false;
  for i in 1:3 loop
    valid := selected[i];
    if valid then
      for j in 1:2 loop
        y[i] := y[i] + 1.0;
        valid := false;
      end for;
    elseif samples[row] > 0.0 then
      for j in 1:2 loop
        y[i] := y[i] + 100.0;
      end for;
    else
      y[i] := -1.0;
    end if;
  end for;
end firstTrueGate;

model LazyElseif
  input Boolean selected[3] = {true, true, true};
  input Real samples[1] = {3.0};
  input Integer row = 999;
  output Real y[3];
equation
  y = firstTrueGate(selected, samples, row);
end LazyElseif;

function frozenSelection
  input Real x[3];
  output Real y[3];
protected
  Real a;
algorithm
  y := zeros(3);
  a := 0.0;
  for i in 1:3 loop
    a := x[i];
    if a > 0.0 then
      for j in 1:2 loop
        a := a - 10.0;
        y[i] := y[i] + 1.0;
      end for;
    elseif a < 0.0 then
      for j in 1:3 loop
        a := a + 10.0;
        y[i] := y[i] + 10.0;
      end for;
    else
      for j in 1:4 loop
        y[i] := y[i] + 100.0;
      end for;
    end if;
    y[i] := y[i] + a;
  end for;
end frozenSelection;

model FrozenSelection
  input Real x[3] = {1.0, -1.0, 0.0};
  output Real y[3];
equation
  y = frozenSelection(x);
end FrozenSelection;

function returnGate
  input Boolean stop;
  input Boolean mask[2];
  output Real y;
protected
  Boolean valid;
  Real s;
algorithm
  if stop then
    y := 0.0;
    return;
  end if;
  y := 0.0;
  valid := false;
  s := 0.0;
  for i in 1:2 loop
    valid := mask[i];
    s := 0.0;
    if valid then
      for j in 1:2 loop
        s := s + 1.0;
        valid := false;
      end for;
    end if;
    y := y + (if valid then s else -s);
  end for;
end returnGate;

model ReturnedGate
  input Boolean stop = true;
  input Boolean mask[2] = {true, true};
  output Real y;
equation
  y = returnGate(stop, mask);
end ReturnedGate;

model ContinuedGate
  input Boolean stop = false;
  input Boolean mask[2] = {true, true};
  output Real y;
equation
  y = returnGate(stop, mask);
end ContinuedGate;
"#;

fn evaluate(model: &str, names: &[&str]) -> Vec<f64> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(MODELS, "NestedLoopConditional.mo")
        .unwrap_or_else(|error| panic!("{model} should compile: {error}"));
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .unwrap_or_else(|error| panic!("{model} should evaluate: {error}"));
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    names
        .iter()
        .map(|name| {
            probe
                .report
                .solver_y
                .iter()
                .find(|slot| slot.name.replace(' ', "") == *name)
                .unwrap_or_else(|| {
                    let available = probe
                        .report
                        .solver_y
                        .iter()
                        .map(|slot| slot.name.as_str())
                        .collect::<Vec<_>>();
                    panic!("{model} has no {name}; slots: {available:?}")
                })
                .value
        })
        .collect()
}

const Y3: [&str; 3] = ["y[1]", "y[2]", "y[3]"];

#[test]
fn a_selected_branch_runs_every_iteration_after_its_predicate_turns_false() {
    // A live predicate would stop after the first inner iteration: -1.
    assert_eq!(evaluate("MutableGate", &Y3), vec![-2.0, 0.0, -2.0]);
}

#[test]
fn each_outer_iteration_selects_again() {
    assert_eq!(evaluate("ReseededGate", &Y3), vec![0.0, -2.0, 0.0]);
}

#[test]
fn a_toggled_predicate_keeps_the_frozen_selection_across_iterations() {
    // Selected: count 2, valid true again. A live predicate gives -1.
    assert_eq!(evaluate("ToggledGate", &Y3), vec![2.0, 0.0, 2.0]);
}

#[test]
fn the_generic_reproduction_matches_its_hand_computed_outputs() {
    let names = [
        "observed[1,1]",
        "observed[1,2]",
        "observed[2,1]",
        "observed[2,2]",
        "observed[3,1]",
        "observed[3,2]",
        "observed[4,1]",
        "observed[4,2]",
        "accepted[1]",
        "accepted[2]",
        "accepted[3]",
        "accepted[4]",
    ];
    assert_eq!(
        evaluate("NestedConditionalLoop", &names),
        vec![
            11.0, 12.0, 21.0, 22.0, 0.0, 0.0, 41.0, 42.0, 0.0, 1.0, 0.0, 1.0
        ]
    );
}

#[test]
fn an_unselected_branch_never_gathers_out_of_range() {
    assert_eq!(evaluate("FalseGather", &Y3), vec![0.0, 0.0, 0.0]);
    assert_eq!(evaluate("MixedGather", &Y3), vec![0.0, 6.0, 0.0]);
}

#[test]
fn a_later_elseif_predicate_stays_unevaluated_after_the_first_branch_mutates() {
    // `samples[999]` would fault if the elseif predicate were evaluated.
    assert_eq!(evaluate("LazyElseif", &Y3), vec![2.0, 2.0, 2.0]);
}

#[test]
fn every_branch_of_a_multibranch_selection_stays_frozen() {
    // x = 1: a = 1 - 20, y = 2 - 19. x = -1: a = -1 + 30, y = 30 + 29.
    // x = 0: y = 400. Reselecting after the mutation would run another arm.
    assert_eq!(evaluate("FrozenSelection", &Y3), vec![-17.0, 59.0, 400.0]);
}

#[test]
fn an_early_return_skips_and_a_continuation_keeps_the_nested_selection() {
    assert_eq!(evaluate("ReturnedGate", &["y"]), vec![0.0]);
    assert_eq!(evaluate("ContinuedGate", &["y"]), vec![-4.0]);
}
