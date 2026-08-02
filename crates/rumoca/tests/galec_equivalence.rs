//! GALEC embedded-C vs Rumoca-evaluation equivalence harness (roadmap
//! Phase 7 — Runtime and Conformance).
//!
//! Each fixture is compiled with `--target galec-c`; the generated
//! C is compiled (`cc -Wall -Werror`), linked (`-lm`), and run through a
//! per-fixture driver that prints every compared block output as one CSV
//! line per `dostep` tick. The same fixture is evaluated in-process by
//! Rumoca's own solver (`rumoca_sim::simulate_dae`), and each tick's C
//! output is asserted equal to the reference at the aligned sample time.
//!
//! **Alignment (empirically grounded against `simulate_dae`).** The block
//! life-cycle is `startup` (seeds state) then one `dostep` per fixed sample
//! tick. For `when sample(t0, period)` the first tick fires *at* `t0`
//! (verified: `sample(0.0, 0.1)` makes `count = 1` at `t = 0`), so the
//! j-th `dostep` (j = 1..N) corresponds to the sample at `t_j = t0 +
//! (j-1)*period`. The reference is read at the strictly-interior hold-
//! interval midpoint `t0 + (j-1)*period + period/2` via a right-continuous
//! hold, so both sides carry the post-tick value regardless of where the
//! solver plants event rows. The startup seed is never compared against the
//! reference (for a `t0 = 0` model no reference row equals it).
//!
//! `cc` is a hard dependency: a missing compiler FAILS, never skips
//! (SPEC_0034 GAL-012). The generated condition vector `c[1]` is
//! projection-internal and absent from the emitted C, so it is never
//! compared — only manifest outputs/states that exist as C struct fields.
//!
//! Fixtures here are original works authored for the Rumoca test suite; no
//! third-party Modelica sources (e.g. the Modelica Standard Library) are
//! copied. The IIR difference equation `y[k] = a*y[k-1] + b*u[k]` is a
//! standard textbook form, not copyrightable content.

use std::fs;
use std::path::Path;
use std::process::Command;

use rumoca_sim::{SimOptions, SimResult, simulate_dae};

#[path = "galec_cli_support/cc.rs"]
mod cc_support;
#[path = "galec_cli_support/cli.rs"]
mod cli_support;

/// How a compared block field is checked against the reference.
#[derive(Clone, Copy)]
enum FieldKind {
    /// Discrete Integer state: exact on both sides (`i64` round-trip).
    Integer,
    /// Discrete Real state: `|c - ref| <= ATOL + RTOL*|ref|`.
    Real,
}

/// A block output compared per tick: its reference name in `SimResult.names`
/// (equal to the C struct field name for these fixtures) and its check kind.
struct Field {
    name: &'static str,
    kind: FieldKind,
}

const ATOL: f64 = 1.0e-9;
const RTOL: f64 = 1.0e-9;

/// Reference value of `name` at the last recorded sample at or before `t`
/// (right-continuous hold). Panics if `name` is absent — a missing signal is
/// a hard failure, never a silent skip (SPEC_0008).
fn value_at(sim: &SimResult, name: &str, t: f64) -> f64 {
    let idx = sim
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("reference must record `{name}`; names = {:?}", sim.names));
    let values = &sim.data[idx];
    let mut held = values[0];
    for (i, &time) in sim.times.iter().enumerate() {
        if time <= t + 1.0e-9 {
            held = values[i];
        } else {
            break;
        }
    }
    held
}

/// Compile a fixture with `--target galec-c` into `out_dir`, failing
/// loudly on any CLI error. Asserts the `<Model>.h` / `<Model>.c` pair exists.
fn compile_embedded_c(work_dir: &Path, out_dir: &Path, model: &str, source: &str) {
    let file = cli_support::write_fixture(work_dir, model, source);
    let output = cli_support::run_compile_target(&file, "galec-c", out_dir);
    assert!(
        output.status.success(),
        "`compile --target galec-c` failed for {model} (status {:?}).\nstderr:\n{}",
        output.status.code(),
        cli_support::strip_ansi(&String::from_utf8_lossy(&output.stderr))
    );
    for ext in ["h", "c"] {
        let path = out_dir.join(format!("{model}.{ext}"));
        assert!(path.is_file(), "missing generated {}", path.display());
    }
}

/// Build+link the driver against the generated source and run it, returning
/// one row of parsed field values per tick. The driver prints
/// `k,<field0>,<field1>,…` per tick with `k` the 0-based `dostep` index; the
/// index is asserted against the loop counter so a dropped or duplicated
/// tick fails loudly.
fn run_c_ticks(out_dir: &Path, model: &str, driver: &str, n_fields: usize) -> Vec<Vec<f64>> {
    let driver_path = out_dir.join("main.c");
    fs::write(&driver_path, driver).expect("write driver");
    let program = out_dir.join("equiv_block");
    let source = out_dir.join(format!("{model}.c"));

    let compile = cc_support::cc()
        .arg("-Wall")
        .arg("-Werror")
        .arg("-o")
        .arg(&program)
        .arg(&driver_path)
        .arg(&source)
        .arg("-lm")
        .output()
        .expect("run cc");
    assert!(
        compile.status.success(),
        "cc -Wall -Werror failed for {model}.\nstderr:\n{}\nsource:\n{}",
        String::from_utf8_lossy(&compile.stderr),
        fs::read_to_string(&source).unwrap_or_default()
    );

    let run = Command::new(&program)
        .output()
        .expect("run generated block");
    assert!(
        run.status.success(),
        "generated block driver for {model} exited with {:?}",
        run.status.code()
    );
    let stdout = String::from_utf8(run.stdout).expect("driver stdout is UTF-8");

    stdout
        .lines()
        .enumerate()
        .map(|(k, line)| {
            let mut fields = line.split(',');
            let index: usize = fields
                .next()
                .and_then(|s| s.parse().ok())
                .unwrap_or_else(|| panic!("driver line {k} has no tick index: {line:?}"));
            assert_eq!(
                index, k,
                "tick index gap (dropped/duplicated dostep): {line:?}"
            );
            let values: Vec<f64> = fields
                .map(|s| {
                    s.parse()
                        .unwrap_or_else(|_| panic!("unparseable field in {line:?}"))
                })
                .collect();
            assert_eq!(
                values.len(),
                n_fields,
                "driver row {k} has {} fields, expected {n_fields}: {line:?}",
                values.len()
            );
            values
        })
        .collect()
}

/// The in-process Rumoca reference: compile the fixture and simulate it, then
/// read each field at the aligned midpoint of every tick's hold interval.
fn reference_ticks(
    model: &str,
    source: &str,
    fields: &[Field],
    t0: f64,
    period: f64,
    n_ticks: usize,
) -> Vec<Vec<f64>> {
    let compiled = rumoca::Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .expect("reference model should compile");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            // Strictly past tick N so its hold interval is fully recorded;
            // tick N+1 lands on the boundary and is never queried.
            t_end: t0 + (n_ticks as f64) * period,
            dt: Some(period / 5.0),
            ..SimOptions::default()
        },
    )
    .expect("reference model should simulate");

    (1..=n_ticks)
        .map(|j| {
            let t = t0 + (j as f64 - 1.0) * period + period / 2.0;
            fields.iter().map(|f| value_at(&sim, f.name, t)).collect()
        })
        .collect()
}

/// Assert the C block reproduces the reference tick-for-tick, and that the
/// sequence is non-vacuous (strictly advancing on the first field, so a
/// seed-only or constant block cannot pass).
fn assert_equivalent(c_ticks: &[Vec<f64>], ref_ticks: &[Vec<f64>], fields: &[Field]) {
    assert_eq!(c_ticks.len(), ref_ticks.len(), "tick count mismatch");
    assert!(c_ticks.len() >= 5, "need >= 5 ticks to be non-vacuous");

    for (j, (c_row, ref_row)) in c_ticks.iter().zip(ref_ticks).enumerate() {
        for (field, (&c, &r)) in fields.iter().zip(c_row.iter().zip(ref_row)) {
            match field.kind {
                FieldKind::Integer => {
                    assert!(
                        (r - r.round()).abs() < 1.0e-9,
                        "reference `{}` not integral at tick {}: {r}",
                        field.name,
                        j + 1
                    );
                    assert_eq!(
                        c as i64,
                        r.round() as i64,
                        "tick {} field `{}`: C {c} != reference {r}",
                        j + 1,
                        field.name
                    );
                }
                FieldKind::Real => {
                    let delta = (c - r).abs();
                    assert!(
                        delta <= ATOL + RTOL * r.abs(),
                        "tick {} field `{}`: C {c} vs reference {r}, delta {delta} exceeds tolerance",
                        j + 1,
                        field.name
                    );
                }
            }
        }
    }

    // Non-vacuous: the first compared field strictly advances across ticks,
    // so a constant/seed-only block (which would trivially "match" an initial
    // value) cannot pass this harness.
    for pair in c_ticks.windows(2) {
        assert!(
            pair[1][0] > pair[0][0],
            "first field must strictly advance (non-vacuous check): {:?} -> {:?}",
            pair[0][0],
            pair[1][0]
        );
    }
}

// ===========================================================================
// Fixture 1 — Integer counter: exact match.
// ===========================================================================

const EQUIV_COUNTER: &str = r#"
model EquivCounter
  constant Real samplePeriod = 0.1;
  discrete Integer count(start = 0, fixed = true);
equation
  when sample(0.0, samplePeriod) then
    count = pre(count) + 1;
  end when;
end EquivCounter;
"#;

const COUNTER_DRIVER: &str = r#"#include <stdio.h>
#include "EquivCounter.h"
int main(void) {
    EquivCounterState state;
    EquivCounter_startup(&state);
    EquivCounter_recalibrate(&state);
    for (int step = 0; step < 5; ++step) {
        EquivCounter_dostep(&state);
        printf("%d,%d\n", step, (int)state.count);   /* order: count */
    }
    return 0;
}
"#;

#[test]
fn embedded_c_counter_matches_rumoca_evaluation_exactly() {
    let dir = tempfile::tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let fields = [Field {
        name: "count",
        kind: FieldKind::Integer,
    }];

    compile_embedded_c(dir.path(), &out_dir, "EquivCounter", EQUIV_COUNTER);
    let c_ticks = run_c_ticks(&out_dir, "EquivCounter", COUNTER_DRIVER, fields.len());
    let ref_ticks = reference_ticks("EquivCounter", EQUIV_COUNTER, &fields, 0.0, 0.1, 5);

    assert_equivalent(&c_ticks, &ref_ticks, &fields);
    // Anchor the absolute values so a coincidental C-vs-ref agreement on a
    // wrong recurrence is still caught.
    let counts: Vec<i64> = c_ticks.iter().map(|row| row[0] as i64).collect();
    assert_eq!(counts, vec![1, 2, 3, 4, 5], "counter recurrence");
}

// ===========================================================================
// Fixture 2 — Real IIR: floating-point equivalence within tolerance.
// ===========================================================================

const DISCRETE_IIR: &str = r#"
model DiscreteIirSmoke
  constant Real samplePeriod = 0.1;
  parameter Real a = 0.9;
  parameter Real b = 0.1;
  discrete Real u(start = 0.0);
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    u = pre(u) + 1.0;
    y = a * pre(y) + b * u;
  end when;
end DiscreteIirSmoke;
"#;

const IIR_DRIVER: &str = r#"#include <stdio.h>
#include "DiscreteIirSmoke.h"
int main(void) {
    DiscreteIirSmokeState state;
    DiscreteIirSmoke_startup(&state);
    DiscreteIirSmoke_recalibrate(&state);
    for (int step = 0; step < 5; ++step) {
        DiscreteIirSmoke_dostep(&state);
        printf("%d,%.17g,%.17g\n", step, state.u, state.y);   /* order: u, y */
    }
    return 0;
}
"#;

#[test]
fn embedded_c_iir_matches_rumoca_evaluation_within_tolerance() {
    let dir = tempfile::tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let fields = [
        Field {
            name: "u",
            kind: FieldKind::Real,
        },
        Field {
            name: "y",
            kind: FieldKind::Real,
        },
    ];

    compile_embedded_c(dir.path(), &out_dir, "DiscreteIirSmoke", DISCRETE_IIR);
    let c_ticks = run_c_ticks(&out_dir, "DiscreteIirSmoke", IIR_DRIVER, fields.len());
    let ref_ticks = reference_ticks("DiscreteIirSmoke", DISCRETE_IIR, &fields, 0.0, 0.1, 5);

    assert_equivalent(&c_ticks, &ref_ticks, &fields);
    // Anchor y against the closed-form recurrence so both sides agreeing on a
    // wrong difference equation is still caught.
    let y: Vec<f64> = c_ticks.iter().map(|row| row[1]).collect();
    let expected = [0.1, 0.29, 0.561, 0.9049, 1.31441];
    for (got, want) in y.iter().zip(expected) {
        assert!(
            (got - want).abs() <= 1.0e-9 + 1.0e-9 * want.abs(),
            "IIR y sequence: got {got}, want {want}"
        );
    }
}
