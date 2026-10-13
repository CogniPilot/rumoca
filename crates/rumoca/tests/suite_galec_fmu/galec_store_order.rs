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
//! A group whose slots each carried an expansion over the storage the other
//! overwrites has no such order, and the projection refuses it. The crossed
//! row eliminations below no longer produce that group, because each scale
//! factor keeps an owner evaluated before either row update stores; this
//! module pins the values that emission computes.

use std::fs;
use std::process::Command;

use super::cc_support::assurance_c99_cc;
use super::cli_support::{run_compile_target, strip_ansi, write_fixture};

use tempfile::tempdir;

const TARGET: &str = "embedded-c-galec";

/// Two eliminations whose scale factors cross: `y`'s row update divides by
/// `z`, and `z`'s row update divides by `y`.
///
/// Both factors are written under a guard nest. Were either spliced into the
/// update that reads it, `y`'s update would carry an expansion over `z` and
/// `z`'s one over `y`, and either store order would divide by replaced values.
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

/// Each scale factor keeps an owner of its own, computed against the storage
/// the group found and before either row update stores, so no emission order is
/// needed: for `m = [4 2 1; 4 5 3; 4 7 9]` against `b + m`, OpenModelica
/// evaluates the third row of `y` to `[-0.2, 1.15, 5.45]` and of `z` to
/// `[0.588235, -1.35294, 5.82353]`. An emission that stored `y` before `z`'s
/// factor was evaluated would report `[-0.142857, 1.392857, 5.607142]` and
/// `[1.771008, 4.305882, 8.652942]` instead.
#[test]
fn crossed_row_eliminations_keep_each_factor_against_the_storage_it_was_stated_on() {
    let work = tempdir().expect("temp dir");
    let fixture = write_fixture(work.path(), "CrossedScratchSmoke", CROSSED_SCRATCH);
    let out = work.path().join("out");
    let output = run_compile_target(&fixture, TARGET, &out);
    let plain = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        output.status.success(),
        "crossed eliminations with owned factors must export:\n{plain}"
    );
    let driver = out.join("main.c");
    fs::write(&driver, CROSSED_DRIVER).expect("write driver");
    let program = out.join("probe");
    let compile = assurance_c99_cc()
        .arg("-o")
        .arg(&program)
        .arg(&driver)
        .arg(out.join("CrossedScratchSmoke.c"))
        .arg(out.join("rumoca_galec_kernels.c"))
        .arg("-lm")
        .output()
        .expect("run cc");
    assert!(
        compile.status.success(),
        "strict cc -std=c99 compile failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&program).output().expect("run probe");
    assert!(run.status.success(), "probe exited with {:?}", run.status);
    let stdout = String::from_utf8_lossy(&run.stdout).into_owned();
    assert_eq!(
        stdout.lines().last(),
        Some("-0.20000 1.15000 5.45000 | 0.58824 -1.35294 5.82353"),
        "the third rows must match the reference evaluation:\n{stdout}"
    );
}

const CROSSED_DRIVER: &str = "\
#include <stdio.h>
#include \"CrossedScratchSmoke.h\"
int main(void) {
    CrossedScratchSmokeState state;
    int i;
    CrossedScratchSmoke_startup(&state);
    CrossedScratchSmoke_recalibrate(&state);
    CrossedScratchSmoke_dostep(&state);
    for (i = 0; i < 3; i++) {
        printf(\"%.5f %.5f %.5f | %.5f %.5f %.5f\\n\",
            (double)state.upper[i][0], (double)state.upper[i][1], (double)state.upper[i][2],
            (double)state.accumulator[i][0], (double)state.accumulator[i][1],
            (double)state.accumulator[i][2]);
    }
    return 0;
}
";
