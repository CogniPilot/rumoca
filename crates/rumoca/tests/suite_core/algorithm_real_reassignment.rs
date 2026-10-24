//! MLS §11.1.2: the statements of an algorithm execute in order, so the last
//! assignment to a discrete `Real` (scalar or array) inside one `when` block
//! wins, and a read between assignments sees the latest one. Before the fix the
//! first assignment won whenever no model-event transaction owned the block: in
//! every unclocked `when`, and in a sampled one after a structural rebuild (a
//! runtime read of an `Evaluate = true` parameter) dropped the transaction.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package Reassign
  model Unclocked "state-event when: scalar and array reassigned under an if"
    Real x(start = 0, fixed = true);
    discrete Real r[3](each start = 0, each fixed = true);
    discrete Real s(start = 0, fixed = true);
    discrete Integer k(start = 0, fixed = true);
  equation
    der(x) = 1;
  algorithm
    when x > 0.1 then
      r := pre(r);
      s := pre(s);
      k := pre(k) + 1;
      if k > 0 then
        r := fill(1.0, 3);
        s := 1.0;
      end if;
    end when;
  end Unclocked;

  model Sampled "sampled when whose transaction a folded parameter used to drop"
    parameter Integer n = 8 annotation(Evaluate = true);
    parameter Integer m = 13 annotation(Evaluate = true);
    discrete Integer steps(start = 0, fixed = true);
    discrete Integer stored(start = 0, fixed = true);
    discrete Real replay[n, m](each start = 0, each fixed = true);
    discrete Integer slot(start = 0, fixed = true);
  algorithm
    when sample(0, 0.1) then
      replay := pre(replay);
      stored := pre(stored);
      slot := 1 + mod(pre(stored), n);
      if time >= 0.05 and pre(steps) > 0 then
        replay := fill(1.0, n, m);
        stored := pre(stored) + 1;
      end if;
      steps := pre(steps) + 1;
    end when;
  end Sampled;

  model IntermediateRead "a read between two assignments sees the first"
    discrete Real a(start = 0, fixed = true);
    discrete Real b(start = 0, fixed = true);
    discrete Real c[2](each start = 0, each fixed = true);
    discrete Real d(start = 0, fixed = true);
  algorithm
    when sample(0.05, 0.1) then
      a := 1;
      b := a + 1;
      a := 10;
      c := {1, 2};
      d := c[2];
      c := {3, 4};
    end when;
  end IntermediateRead;

  model LastWins "the last assignment whose guard holds wins"
    parameter Boolean flag = true;
    Real x(start = 0, fixed = true);
    discrete Real r(start = 0, fixed = true);
  equation
    der(x) = 1;
  algorithm
    when x > 0.1 then
      r := 1;
      if flag then
        r := 2;
      end if;
    end when;
  end LastWins;

  model LastWinsFalse
    extends LastWins(flag = false);
  end LastWinsFalse;

  model ConditionalOnly "assigned only under a toggling condition: holds pre() when false"
    discrete Real x(start = 0, fixed = true);
    discrete Real r[3](each start = 0, each fixed = true);
    discrete Integer i(start = 0, fixed = true);
  algorithm
    when sample(0.05, 0.1) then
      i := pre(i) + 1;
      if mod(i, 2) == 1 then
        x := 10 * i;
        r[2] := i;
      end if;
    end when;
  end ConditionalOnly;

  model ElsewhenPriority "an active when block suppresses its elsewhen at the same instant"
    Integer x;
    discrete Real t_next(start = 0, fixed = true);
    discrete Integer y(start = -1, fixed = true);
    discrete Real yr(start = -1, fixed = true);
  equation
    x = if time >= 1 then 1 else 0;
  algorithm
    when {initial(), change(x) and not initial()} then
      t_next := time + (if initial() then 1 else 2);
    elsewhen time >= t_next then
      y := x;
      yr := x;
    end when;
  end ElsewhenPriority;

  record Rec
    Real v;
  end Rec;

  model ElsewhenHeldShapes "a later block writes an array element and a record field"
    Integer x;
    discrete Real t_next(start = 0, fixed = true);
    discrete Real r[3](each start = 0, each fixed = true);
    discrete Rec rec(v(start = -1, fixed = true));
  equation
    x = if time >= 1 then 1 else 0;
  algorithm
    when {initial(), change(x) and not initial()} then
      t_next := time + (if initial() then 1 else 2);
    elsewhen time >= t_next then
      r[2] := x;
      rec.v := x;
    end when;
  end ElsewhenHeldShapes;

  model SameCondition "a later block under the same condition never runs"
    Real s(start = 0, fixed = true);
    discrete Real a(start = 0, fixed = true);
    discrete Real y(start = -1, fixed = true);
  equation
    der(s) = 1;
  algorithm
    when s > 0.5 then
      a := 1;
    elsewhen s > 0.5 then
      y := 2;
    end when;
  end SameCondition;

  model LaterAction "an event action in a later elsewhen block is refused"
    Real s(start = 0, fixed = true);
    discrete Real a(start = 0, fixed = true);
  equation
    der(s) = 1;
  algorithm
    when s > 0.5 then
      a := 1;
    elsewhen s > 0.8 then
      assert(a > 0, "never");
    end when;
  end LaterAction;

  function replaceRowAssert
    input Real matrix[:, :];
    input Integer slot;
    input Real row[size(matrix, 2)];
    output Real result[size(matrix, 1), size(matrix, 2)];
  algorithm
    assert(slot >= 1 and slot <= size(matrix, 1), "slot outside capacity");
    result := matrix;
    result[slot, :] := row;
  end replaceRowAssert;

  model GuardedAssertCall "bug A: a guarded call to a function with an assert"
    discrete Real ring[3, 2](each start = 0, each fixed = true);
    discrete Integer stored(start = 0, fixed = true);
    Integer slot;
  algorithm
    when sample(0, 0.1) then
      ring := pre(ring);
      stored := pre(stored);
      slot := 1 + mod(pre(stored), 3);
      if time > 0.05 then
        ring := replaceRowAssert(pre(ring), slot, {time, 1.0});
        stored := pre(stored) + 1;
      end if;
    end when;
  end GuardedAssertCall;
end Reassign;
"#;

/// The final value of every named column after simulating `model` to `t_end`.
fn final_values(model: &str, t_end: f64, names: &[&str]) -> Vec<f64> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, "algorithm_real_reassignment.mo")
        .unwrap();
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end,
            dt: Some(0.01),
            solver_mode: SimSolverMode::RkLike,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model}: {error}"));
    names
        .iter()
        .map(|name| {
            let column = result
                .names
                .iter()
                .position(|candidate| candidate == name)
                .unwrap_or_else(|| panic!("{model} has no column {name}"));
            *result.data[column].last().unwrap()
        })
        .collect()
}

/// Every value within 1e-12 of the expected one: sample instants are
/// `k * period` in floating point.
fn assert_values(model: &str, actual: &[f64], expected: &[f64]) {
    assert_eq!(actual.len(), expected.len());
    for (index, (actual, expected)) in actual.iter().zip(expected).enumerate() {
        assert!(
            (actual - expected).abs() <= 1e-12,
            "{model} value {index}: {actual}, expected {expected}"
        );
    }
}

#[test]
fn an_unclocked_when_keeps_the_last_real_assignment() {
    assert_values(
        "Reassign.Unclocked",
        &final_values(
            "Reassign.Unclocked",
            0.3,
            &["r[1]", "r[2]", "r[3]", "s", "k"],
        ),
        &[1.0, 1.0, 1.0, 1.0, 1.0],
    );
}

#[test]
fn a_sampled_when_with_a_folded_parameter_keeps_the_last_real_assignment() {
    // Ticks at 0, 0.1, …, 0.8; the guard first holds at 0.1, so 8 stores.
    let values = final_values(
        "Reassign.Sampled",
        0.81,
        &["stored", "replay[1,1]", "replay[8,13]", "replay[4,7]"],
    );
    assert_values("Reassign.Sampled", &values, &[8.0, 1.0, 1.0, 1.0]);
}

#[test]
fn a_read_between_assignments_sees_the_earlier_one() {
    assert_values(
        "Reassign.IntermediateRead",
        &final_values(
            "Reassign.IntermediateRead",
            0.1,
            &["a", "b", "c[1]", "c[2]", "d"],
        ),
        &[10.0, 2.0, 3.0, 4.0, 2.0],
    );
}

#[test]
fn the_last_assignment_whose_guard_holds_wins() {
    assert_values(
        "Reassign.LastWins",
        &final_values("Reassign.LastWins", 0.3, &["r"]),
        &[2.0],
    );
    assert_values(
        "Reassign.LastWinsFalse",
        &final_values("Reassign.LastWinsFalse", 0.3, &["r"]),
        &[1.0],
    );
}

#[test]
fn a_conditional_only_assignment_holds_when_its_guard_is_false() {
    // The single definition is `if c then v else pre(x)`: ticks at 0.05 (true),
    // 0.15 (false, hold) and 0.25 (true). Main holds the same way.
    for (t_end, expected) in [
        (0.1, [10.0, 0.0, 1.0, 0.0]),
        (0.2, [10.0, 0.0, 1.0, 0.0]),
        (0.3, [30.0, 0.0, 3.0, 0.0]),
    ] {
        assert_values(
            "Reassign.ConditionalOnly",
            &final_values(
                "Reassign.ConditionalOnly",
                t_end,
                &["x", "r[1]", "r[2]", "r[3]"],
            ),
            &expected,
        );
    }
}

#[test]
fn an_active_when_block_suppresses_its_elsewhen() {
    // MSL Digital InertialDelaySensitive's shape (VectorDelay): t_next = 1 from
    // the initial block, and x changes at t = 1, so both blocks become active at
    // once. Only the first runs (MLS §8.3.5): t_next becomes 3 and y holds.
    assert_values(
        "Reassign.ElsewhenPriority",
        &final_values("Reassign.ElsewhenPriority", 2.0, &["t_next", "y", "yr"]),
        &[3.0, -1.0, -1.0],
    );
    assert_values(
        "Reassign.ElsewhenPriority",
        &final_values("Reassign.ElsewhenPriority", 4.0, &["t_next", "y", "yr"]),
        &[3.0, 1.0, 1.0],
    );
}

#[test]
fn an_active_when_block_holds_later_array_elements_and_record_fields() {
    let names = ["r[1]", "r[2]", "r[3]", "rec.v"];
    assert_values(
        "Reassign.ElsewhenHeldShapes",
        &final_values("Reassign.ElsewhenHeldShapes", 2.0, &names),
        &[0.0, 0.0, 0.0, -1.0],
    );
    assert_values(
        "Reassign.ElsewhenHeldShapes",
        &final_values("Reassign.ElsewhenHeldShapes", 4.0, &names),
        &[0.0, 1.0, 0.0, 1.0],
    );
}

#[test]
fn a_later_block_with_the_same_condition_never_runs() {
    // Main ran it: y = 2. Only the first block runs when both rise (MLS §8.3.5).
    assert_values(
        "Reassign.SameCondition",
        &final_values("Reassign.SameCondition", 1.0, &["a", "y"]),
        &[1.0, -1.0],
    );
}

#[test]
fn an_event_action_in_a_later_elsewhen_block_is_refused() {
    let Err(error) = Compiler::new()
        .model("Reassign.LaterAction")
        .compile_str(SOURCE, "algorithm_real_reassignment.mo")
    else {
        panic!("a later-block assert must be refused until its suppression is representable");
    };
    let (display, debug) = (error.to_string(), format!("{error:?}"));
    assert!(
        display.contains("later `elsewhen` block") || debug.contains("UnsupportedLaterWhenAction"),
        "{display} / {debug}"
    );
}

#[test]
fn a_guarded_call_with_an_assert_lowers_and_writes_its_row() {
    // Ticks 0.1 … 0.5 store into slots 1, 2, 3, 1, 2.
    assert_values(
        "Reassign.GuardedAssertCall",
        &final_values(
            "Reassign.GuardedAssertCall",
            0.55,
            &["stored", "ring[1,1]", "ring[2,1]", "ring[3,1]", "ring[3,2]"],
        ),
        &[5.0, 0.4, 0.5, 0.3, 1.0],
    );
}
