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
