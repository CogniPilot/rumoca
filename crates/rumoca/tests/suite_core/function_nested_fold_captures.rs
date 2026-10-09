//! A fold nested in the update of another fold reads the carried tuples of
//! every enclosing fold, not only its immediate parent: a callee expanded in
//! the body of a doubly nested loop builds its folds from an argument that
//! the outer loop carries.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const SOURCE: &str = r#"
function windowed
  input Real differences[16];
  output Real score;
protected
  Real extended[24];
  Real low2[22];
algorithm
  for i in 1:16 loop
    extended[i] := differences[i];
  end for;
  for i in 1:8 loop
    extended[i + 16] := differences[i];
  end for;
  for i in 1:22 loop
    low2[i] := if noEvent(extended[i] < extended[i + 1]) then extended[i] else extended[i + 1];
  end for;
  score := 0.0;
  for arc in 1:16 loop
    score := if noEvent(score > low2[arc]) then score else low2[arc];
  end for;
end windowed;

function sweep
  input Real u;
  input Real w[:];
  output Real scores[6];
  output Real peak;
protected
  Real filled[16];
algorithm
  peak := max(w) * u;
  scores := zeros(6);
  for r in 1:3 loop
    for c in 1:2 loop
      for s in 1:16 loop
        filled[s] := (r + c) * u;
      end for;
      scores[(r - 1) * 2 + c] := windowed(filled);
    end for;
  end for;
end sweep;

model Sweep
  Real x(start = 1.0, fixed = true);
  Real scores[6];
  Real peak;
equation
  der(x) = 0;
  (scores, peak) = sweep(x, {1.0, 4.0});
end Sweep;
"#;

#[test]
fn a_callee_fold_reads_a_carried_array_of_an_outer_loop_nest() {
    let compiled = Compiler::new()
        .model("Sweep")
        .compile_str(SOURCE, "sweep.mo")
        .expect("the loop nest constructs checked DAE");
    let options = SimOptions {
        t_end: 0.1,
        dt: Some(0.1),
        ..SimOptions::default()
    };
    let simulation =
        simulate_dae(&compiled.dae, &options).expect("the nested folds lower to Solve rows");
    let first = |name: &str| {
        let variable = simulation
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is visible"));
        simulation.data[variable].first().copied()
    };
    // Every window of a constant array has that constant as its minimum.
    for (index, expected) in [2.0, 3.0, 3.0, 4.0, 4.0, 5.0].into_iter().enumerate() {
        assert_eq!(first(&format!("scores[{}]", index + 1)), Some(expected));
    }
    assert_eq!(first("peak"), Some(4.0));
}

/// A conditional under two loops holds the inner folds that read both loop
/// binders through a constant offset table.
const GUARDED_WINDOWS: &str = r#"
package Ring
  constant Integer offsets[4, 2] = [-1, 0; 0, 1; 1, 0; 0, -1];
end Ring;

function reachable
  input Real values[4];
  input Real floor;
  output Boolean possible;
algorithm
  possible := false;
  for k in 1:4 loop
    possible := possible or values[k] >= floor;
  end for;
end reachable;

function windowed
  input Real values[4];
  output Real score;
protected
  Real doubled[8];
algorithm
  for i in 1:4 loop
    doubled[i] := values[i];
  end for;
  for i in 1:4 loop
    doubled[i + 4] := values[i];
  end for;
  score := 0.0;
  for k in 1:5 loop
    score := if noEvent(score > doubled[k] + doubled[k + 3]) then score else doubled[k] + doubled[k + 3];
  end for;
end windowed;

function scan
  input Real image[:, :];
  input Real w[:];
  input Real u;
  output Real scores[16];
  output Real peak;
protected
  Real around[4];
algorithm
  peak := max(w) * u;
  scores := zeros(16);
  for row in 2:3 loop
    for column in 2:3 loop
      for s in 1:4 loop
        around[s] := image[row + Ring.offsets[s, 1], column + Ring.offsets[s, 2]];
      end for;
      if reachable(around, 2.5 * u) then
        scores[(row - 1) * 4 + column] := windowed(around);
      end if;
    end for;
  end for;
end scan;

model Scan
  Real x(start = 1.0, fixed = true);
  Real scores[16];
  Real peak;
equation
  der(x) = 0;
  (scores, peak) = scan(x * [1, 2, 3, 4; 5, 6, 7, 8; 9, 10, 11, 12; 13, 14, 15, 16], {1.0, 4.0}, x);
end Scan;
"#;

#[test]
fn guarded_windows_under_two_loops_read_both_loop_binders() {
    let compiled = Compiler::new()
        .model("Scan")
        .compile_str(GUARDED_WINDOWS, "scan.mo")
        .expect("the guarded loop nest constructs checked DAE");
    let options = SimOptions {
        t_end: 0.1,
        dt: Some(0.1),
        ..SimOptions::default()
    };
    let simulation =
        simulate_dae(&compiled.dae, &options).expect("the guarded windows lower to Solve rows");
    let variable = simulation
        .names
        .iter()
        .position(|candidate| candidate == "scores[6]")
        .expect("scores[6] is visible");
    // Around (2,2): 2, 7, 10, 5; the largest window sum is 10 + 7.
    assert_eq!(simulation.data[variable].first().copied(), Some(17.0));
}
