//! Regression: `galec-production` structured lowering must emit every indexed
//! left-hand-side assignment inside a conditional branch, not just the last.
//!
//! MLS §11 gives an algorithm section sequential semantics: `v[1] := ...;
//! v[2] := ...; v[3] := ...;` inside one `if`/`elseif`/`else` branch defines
//! three distinct elements of `v`, and §12.4.4 lets a function value be defined
//! element by element when it has no declared binding. A branch lowering that
//! keyed a definition by the variable name rather than by the element would
//! keep only the last write and fold its scalar dependencies into it, silently
//! dropping `v[1]` and `v[2]`.
//!
//! The DAE function construction keys coverage per element
//! (`rumoca-phase-dae` `construction/analysis/function_definitions.rs`,
//! `ValueCoverage::Elements`), so the structured emission carries every write.
//! These tests pin that on the two shapes the RDD2 exports exercise: a rotation
//! vector logarithm (`log_map`) and Shepperd's four-branch quaternion recovery
//! (`from_DCM`). Both function copies here are library-free constructions.

use std::fs;
use std::process::Command;

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};
use tempfile::tempdir;

use super::cc_support::{GALEC_KERNEL_LIBRARY, assurance_c99_cc};
use super::cli_support::{run_compile_target_with, strip_ansi, write_fixture};

/// The published minimal reproducer: three element writes to one array inside
/// each `if`/`else` branch, plus a control that writes the same three elements
/// at the top level of the algorithm section.
const REPRO_GALEC: &str = "\
function branchAssign
  input Real x;
  output Real v[3];
protected
  Real s;
algorithm
  if x < 0.5 then
    s := 1.0;
    v[1] := s * x;
    v[2] := 2.0 * s * x;
    v[3] := 3.0 * s * x;
  else
    s := 4.0;
    v[1] := s * x;
    v[2] := 2.0 * s * x;
    v[3] := 3.0 * s * x;
  end if;
end branchAssign;

function straightAssign
  input Real x;
  output Real v[3];
algorithm
  v[1] := 7.0 * x;
  v[2] := 8.0 * x;
  v[3] := 9.0 * x;
end straightAssign;

model BranchElementDrop
  constant Real samplePeriod = 0.1;
  input Real u;
  discrete output Real vb[3](each start = 0.0);
  discrete output Real vs[3](each start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    vb = branchAssign(u);
    vs = straightAssign(u);
  end when;
end BranchElementDrop;
";

/// The same `branchAssign` body evaluated through a continuous equation so the
/// checked-DAE evaluator reports the array elements directly.
const REPRO_EVAL: &str = "\
within;
function branchAssign
  input Real x;
  output Real v[3];
protected
  Real s;
algorithm
  if x < 0.5 then
    s := 1.0;
    v[1] := s * x;
    v[2] := 2.0 * s * x;
    v[3] := 3.0 * s * x;
  else
    s := 4.0;
    v[1] := s * x;
    v[2] := 2.0 * s * x;
    v[3] := 3.0 * s * x;
  end if;
end branchAssign;
model BranchElementEval
  Real lo[3];
  Real hi[3];
equation
  lo = branchAssign(0.2);
  hi = branchAssign(0.8);
end BranchElementEval;
";

/// Shepperd's method: four branches, each writing all four quaternion
/// components. A library-free copy of the affected `from_DCM`, with a minimal
/// `to_DCM` and `normalize` so the round trip can be evaluated.
const FROM_DCM_FUNCTIONS: &str = "\
function normalize
  input Real q[4];
  output Real q_n[4];
protected
  Real n;
  constant Real tolerance = 1.0e-12;
algorithm
  n := sqrt(q[1]^2 + q[2]^2 + q[3]^2 + q[4]^2);
  q_n := if n > tolerance then q / n else {1.0, 0.0, 0.0, 0.0};
end normalize;

function to_DCM
  input Real q[4];
  output Real R[3, 3];
protected
  Real a, b, c, d;
algorithm
  a := q[1]; b := q[2]; c := q[3]; d := q[4];
  R[1, 1] := a*a + b*b - c*c - d*d;
  R[1, 2] := 2*(b*c - a*d);
  R[1, 3] := 2*(b*d + a*c);
  R[2, 1] := 2*(b*c + a*d);
  R[2, 2] := a*a - b*b + c*c - d*d;
  R[2, 3] := 2*(c*d - a*b);
  R[3, 1] := 2*(b*d - a*c);
  R[3, 2] := 2*(c*d + a*b);
  R[3, 3] := a*a - b*b - c*c + d*d;
end to_DCM;

function from_DCM
  input Real R[3, 3];
  output Real q[4];
protected
  Real tr;
  Real b1, b2, b3, b4;
  constant Real eps = 1e-10;
algorithm
  tr := R[1, 1] + R[2, 2] + R[3, 3];
  if tr > 0 then
    b1 := 0.5 * sqrt(max(1.0 + tr, eps));
    q[1] := b1;
    q[2] := (R[3, 2] - R[2, 3]) / max(4.0*b1, eps);
    q[3] := (R[1, 3] - R[3, 1]) / max(4.0*b1, eps);
    q[4] := (R[2, 1] - R[1, 2]) / max(4.0*b1, eps);
  elseif R[1, 1] > R[2, 2] and R[1, 1] > R[3, 3] then
    b2 := 0.5 * sqrt(max(1.0 + R[1, 1] - R[2, 2] - R[3, 3], eps));
    q[1] := (R[3, 2] - R[2, 3]) / max(4.0*b2, eps);
    q[2] := b2;
    q[3] := (R[1, 2] + R[2, 1]) / max(4.0*b2, eps);
    q[4] := (R[1, 3] + R[3, 1]) / max(4.0*b2, eps);
  elseif R[2, 2] > R[3, 3] then
    b3 := 0.5 * sqrt(max(1.0 - R[1, 1] + R[2, 2] - R[3, 3], eps));
    q[1] := (R[1, 3] - R[3, 1]) / max(4.0*b3, eps);
    q[2] := (R[1, 2] + R[2, 1]) / max(4.0*b3, eps);
    q[3] := b3;
    q[4] := (R[2, 3] + R[3, 2]) / max(4.0*b3, eps);
  else
    b4 := 0.5 * sqrt(max(1.0 - R[1, 1] - R[2, 2] + R[3, 3], eps));
    q[1] := (R[2, 1] - R[1, 2]) / max(4.0*b4, eps);
    q[2] := (R[1, 3] + R[3, 1]) / max(4.0*b4, eps);
    q[3] := (R[2, 3] + R[3, 2]) / max(4.0*b4, eps);
    q[4] := b4;
  end if;
  q := normalize(q);
  q := if q[1] < 0 then -q else q;
end from_DCM;
";

fn from_dcm_galec_source() -> String {
    format!(
        "{FROM_DCM_FUNCTIONS}
model ShepperdBranches
  constant Real samplePeriod = 0.1;
  input Real r11; input Real r12; input Real r13;
  input Real r21; input Real r22; input Real r23;
  input Real r31; input Real r32; input Real r33;
  discrete output Real q[4](each start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    q = from_DCM({{{{r11, r12, r13}}, {{r21, r22, r23}}, {{r31, r32, r33}}}});
  end when;
end ShepperdBranches;
"
    )
}

/// Four unit quaternions, one per Shepperd diagonal case, with positive scalar
/// part so `from_DCM(to_DCM(q))` recovers exactly `q`.
const ROUND_TRIP_QUATERNIONS: [[f64; 4]; 4] = [
    [
        0.9233805168766388,
        0.10259783520851543,
        0.20519567041703086,
        0.3077935056255463,
    ],
    [
        0.10327955589886444,
        0.9811557810392122,
        0.15491933384829667,
        0.05163977794943222,
    ],
    [
        0.10327955589886444,
        0.15491933384829667,
        0.9811557810392122,
        0.05163977794943222,
    ],
    [
        0.10327955589886444,
        0.05163977794943222,
        0.15491933384829667,
        0.9811557810392122,
    ],
];

fn from_dcm_eval_source() -> String {
    let mut equations = String::new();
    let mut declarations = String::new();
    for (index, q) in ROUND_TRIP_QUATERNIONS.iter().enumerate() {
        declarations.push_str(&format!("  Real r{index}[4];\n"));
        equations.push_str(&format!(
            "  r{index} = from_DCM(to_DCM({{{}, {}, {}, {}}}));\n",
            q[0], q[1], q[2], q[3]
        ));
    }
    format!(
        "within;
{FROM_DCM_FUNCTIONS}
model ShepperdRoundTrip
{declarations}equation
{equations}end ShepperdRoundTrip;
"
    )
}

/// Export `model` with `galec-production`, link the emitted unit with
/// `driver` under the strict profile, run it, and return its stdout.
fn run_production_c(source: &str, model: &str, driver: &str) -> String {
    let dir = tempdir().expect("tempdir");
    let file = write_fixture(dir.path(), model, source);
    let out_dir = dir.path().join("out");
    let output = run_compile_target_with(&file, "galec-production", &out_dir, &["--model", model]);
    assert!(
        output.status.success(),
        "`galec-production` must compile `{model}`.\nstderr:\n{}",
        strip_ansi(&String::from_utf8_lossy(&output.stderr))
    );
    let code = out_dir.join(model).join("ProductionCode");
    let driver_path = code.join("driver.c");
    fs::write(&driver_path, driver).expect("write driver");
    let program = dir.path().join("driver");
    let compile = assurance_c99_cc()
        .arg("-o")
        .arg(&program)
        .arg(&driver_path)
        .arg(code.join(format!("{model}.c")))
        .arg(code.join(GALEC_KERNEL_LIBRARY))
        .arg("-lm")
        .output()
        .expect("run cc");
    assert!(
        compile.status.success(),
        "strict C99 rejected `{model}`.\nstderr:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&program).output().expect("run driver");
    assert!(
        run.status.success(),
        "`{model}` driver exited with {:?}",
        run.status.code()
    );
    String::from_utf8(run.stdout).expect("driver prints UTF-8")
}

const BRANCH_ELEMENT_DRIVER: &str = r#"
#include <stdio.h>
#include <string.h>
#include "BranchElementDrop.h"

int main(void) {
    static BranchElementDropState state;
    const float inputs[2] = { 0.2f, 0.8f };
    memset(&state, 0, sizeof state);
    BranchElementDrop_startup(&state);
    for (int index = 0; index < 2; ++index) {
        state.u = inputs[index];
        BranchElementDrop_dostep(&state);
        printf("%.4f %.4f %.4f | %.4f %.4f %.4f\n",
               (double)state.vb[0], (double)state.vb[1], (double)state.vb[2],
               (double)state.vs[0], (double)state.vs[1], (double)state.vs[2]);
    }
    return 0;
}
"#;

/// Feeds `to_DCM(q)` for each of `ROUND_TRIP_QUATERNIONS` (one per Shepperd
/// branch) and prints the recovered quaternion.
const SHEPPERD_DRIVER: &str = r#"
#include <stdio.h>
#include <string.h>
#include "ShepperdBranches.h"

static const double quaternions[4][4] = {
    { 0.9233805168766388, 0.10259783520851543, 0.20519567041703086, 0.3077935056255463 },
    { 0.10327955589886444, 0.9811557810392122, 0.15491933384829667, 0.05163977794943222 },
    { 0.10327955589886444, 0.15491933384829667, 0.9811557810392122, 0.05163977794943222 },
    { 0.10327955589886444, 0.05163977794943222, 0.15491933384829667, 0.9811557810392122 },
};

int main(void) {
    static ShepperdBranchesState state;
    memset(&state, 0, sizeof state);
    ShepperdBranches_startup(&state);
    for (int index = 0; index < 4; ++index) {
        const double a = quaternions[index][0];
        const double b = quaternions[index][1];
        const double c = quaternions[index][2];
        const double d = quaternions[index][3];
        state.r11 = (float)(a * a + b * b - c * c - d * d);
        state.r12 = (float)(2.0 * (b * c - a * d));
        state.r13 = (float)(2.0 * (b * d + a * c));
        state.r21 = (float)(2.0 * (b * c + a * d));
        state.r22 = (float)(a * a - b * b + c * c - d * d);
        state.r23 = (float)(2.0 * (c * d - a * b));
        state.r31 = (float)(2.0 * (b * d - a * c));
        state.r32 = (float)(2.0 * (c * d + a * b));
        state.r33 = (float)(a * a - b * b - c * c + d * d);
        ShepperdBranches_dostep(&state);
        printf("%.9g %.9g %.9g %.9g\n",
               (double)state.q[0], (double)state.q[1], (double)state.q[2], (double)state.q[3]);
    }
    return 0;
}
"#;

fn algebraic(report: &rumoca_sim::EvalAtReport, name: &str) -> f64 {
    report
        .solver_y
        .iter()
        .find(|slot| slot.name.replace(' ', "") == name)
        .unwrap_or_else(|| {
            panic!(
                "missing solver value {name}; have: {:?}",
                report
                    .solver_y
                    .iter()
                    .map(|slot| slot.name.clone())
                    .collect::<Vec<_>>()
            )
        })
        .value
}

fn evaluate(source: &str, model: &str, file: &str) -> rumoca_sim::EvalAtReport {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, file)
        .expect("model should compile to a checked DAE");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("checked DAE should evaluate");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    probe.report
}

#[test]
fn each_branch_emits_every_element_write() {
    // u = 0.2 takes the s = 1 branch, u = 0.8 the s = 4 one; a dropped element
    // write would leave that component at its previous value.
    assert_eq!(
        run_production_c(REPRO_GALEC, "BranchElementDrop", BRANCH_ELEMENT_DRIVER),
        "0.2000 0.4000 0.6000 | 1.4000 1.6000 1.8000\n\
         3.2000 6.4000 9.6000 | 5.6000 6.4000 7.2000\n",
        "every branch and the top-level control must keep all three element writes"
    );
}

#[test]
fn branch_element_values_survive_evaluation() {
    let report = evaluate(REPRO_EVAL, "BranchElementEval", "BranchElementEval.mo");
    // x = 0.2 < 0.5 takes the s = 1 branch: {x, 2x, 3x}.
    assert_eq!(algebraic(&report, "lo[1]"), 0.2);
    assert_eq!(algebraic(&report, "lo[2]"), 0.4);
    assert!((algebraic(&report, "lo[3]") - 0.6).abs() < 1e-12);
    // x = 0.8 >= 0.5 takes the s = 4 branch: {4x, 8x, 12x}.
    assert!((algebraic(&report, "hi[1]") - 3.2).abs() < 1e-12);
    assert!((algebraic(&report, "hi[2]") - 6.4).abs() < 1e-12);
    assert!((algebraic(&report, "hi[3]") - 9.6).abs() < 1e-12);
}

#[test]
fn every_shepperd_branch_emits_four_components() {
    let stdout = run_production_c(
        &from_dcm_galec_source(),
        "ShepperdBranches",
        SHEPPERD_DRIVER,
    );
    let rows = stdout.lines().collect::<Vec<_>>();
    assert_eq!(rows.len(), ROUND_TRIP_QUATERNIONS.len(), "{stdout}");
    for (branch, (row, expected)) in rows.iter().zip(ROUND_TRIP_QUATERNIONS).enumerate() {
        for (component, (got, want)) in row.split(' ').zip(expected).enumerate() {
            let got = got.parse::<f64>().expect("a printed component");
            assert!(
                (got - want).abs() < 1e-6,
                "Shepperd branch {branch} must recover q[{}]: expected {want}, got {got}",
                component + 1
            );
        }
    }
}

#[test]
fn from_dcm_round_trip_recovers_every_component() {
    let report = evaluate(
        &from_dcm_eval_source(),
        "ShepperdRoundTrip",
        "ShepperdRoundTrip.mo",
    );
    for (index, expected) in ROUND_TRIP_QUATERNIONS.iter().enumerate() {
        for (component, value) in expected.iter().enumerate() {
            let name = format!("r{index}[{}]", component + 1);
            let recovered = algebraic(&report, &name);
            assert!(
                (recovered - value).abs() < 1e-9,
                "from_DCM(to_DCM(q)) must recover {name}: expected {value}, got {recovered}"
            );
        }
    }
}
