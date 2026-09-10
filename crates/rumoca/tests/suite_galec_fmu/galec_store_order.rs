//! What the projection does with an assignment group no emission order can
//! serve.
//!
//! A checked assignment group commits its definitions atomically: every value
//! in it is stated against the storage the group found. GALEC has statements,
//! so the group is emitted one store at a time, and that is faithful only
//! while each remaining value still finds the content it was stated against.
//! `lower::user_functions::update_aliasing` discharges the obligation by
//! choosing an emission order, and
//! [`galec_equivalence::embedded_c_expands_a_substituted_scratch_where_the_storage_it_reads_still_holds_it`]
//! pins the shapes an order does serve.
//!
//! Some shapes have no such order. When two slots each carry an expansion that
//! reads the storage the other overwrites, either store invalidates the other
//! slot's reads, and there is nothing left downstream to restore what it
//! overwrote. This module pins that the projection refuses instead of emitting
//! one of the two wrong answers.

use super::cli_support::{diagnostic_contains, run_compile_target, strip_ansi, write_fixture};

use tempfile::tempdir;

const TARGET: &str = "embedded-c-galec";

/// Two eliminations whose scale factors cross: `y`'s row update divides by
/// `z`, and `z`'s row update divides by `y`.
///
/// Both factors are written under a guard nest, so neither keeps an owner and
/// each is spliced into the update that reads it. `y`'s update therefore
/// carries an expansion over `z`, and `z`'s carries one over `y`, while the
/// two updates store into `y` and `z`. Emitting `y` first makes `z`'s
/// expansion divide values `y`'s store has replaced; emitting `z` first does
/// the same to `y`'s.
const CROSSED_SCRATCH: &str = r#"
function crossedScratch
  input Real m[3, 3];
  input Real b[3, 3];
  input Integer mode;
  output Real y[3, 3];
  output Real z[3, 3];
protected
  Real fromZ;
  Real fromY;
algorithm
  y := m;
  z := b + m;
  for column in 1:2 loop
    if mode == 2 then
      for row in 2:3 loop
        if row > column then
          fromZ := z[row, column] / z[column, column];
          fromY := y[row, column] / y[column, column];
          y[row, :] := y[row, :] - fromZ * y[column, :];
          z[row, :] := z[row, :] - fromY * z[column, :];
        end if;
      end for;
    end if;
  end for;
end crossedScratch;

model CrossedScratchSmoke
  constant Real samplePeriod = 0.1;
  parameter Real m[3, 3] = [4.0, 2.0, 1.0; 4.0, 5.0, 3.0; 4.0, 7.0, 9.0];
  parameter Real b[3, 3] = [1.0, 0.0, 0.0; 0.0, 1.0, 0.0; 0.0, 0.0, 1.0];
  discrete output Real upper[3, 3](each start = 0.0);
  discrete output Real accumulator[3, 3](each start = 0.0);
algorithm
  when sample(0.0, samplePeriod) then
    (upper, accumulator) := crossedScratch(m, b, 2);
  end when;
end CrossedScratchSmoke;
"#;

/// The refusal is the whole point: for `m = [4 2 1; 4 5 3; 4 7 9]` against
/// `b + m`, OpenModelica evaluates the third row of `y` to
/// `[-0.2, 1.15, 5.45]` and of `z` to `[0.588235, -1.35294, 5.82353]`. An
/// emission that stores `y` before `z`'s expansion runs reports
/// `[-0.142857, 1.392857, 5.607142]` and `[1.771008, 4.305882, 8.652942]`
/// instead, with no signal that anything went wrong. There is no order that
/// serves both, so the projection must say so rather than pick one.
#[test]
fn a_group_whose_slots_read_what_each_other_overwrites_is_refused() {
    let work = tempdir().expect("temp dir");
    let fixture = write_fixture(work.path(), "CrossedScratchSmoke", CROSSED_SCRATCH);
    let out = work.path().join("out");
    let output = run_compile_target(&fixture, TARGET, &out);
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    let plain = strip_ansi(&stderr);
    assert!(
        !output.status.success(),
        "a group no emission order serves must be refused, not emitted:\n{plain}"
    );
    assert!(
        diagnostic_contains(&stderr, "function-group-store-order"),
        "the refusal must name the store-order obligation:\n{plain}"
    );
    assert!(
        diagnostic_contains(&stderr, "reads what a sibling assignment"),
        "the refusal must say what it could not place:\n{plain}"
    );
}
