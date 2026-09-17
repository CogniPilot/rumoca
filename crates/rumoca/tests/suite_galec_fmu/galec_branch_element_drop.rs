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

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};
use tempfile::tempdir;

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

/// Compile `source` to `galec-production` through the real binary and return
/// the emitted ProductionCode C.
fn production_c(source: &str, model: &str) -> String {
    let dir = tempdir().expect("tempdir");
    let file = write_fixture(dir.path(), model, source);
    let out_dir = dir.path().join("out");
    let output = run_compile_target_with(&file, "galec-production", &out_dir, &["--model", model]);
    assert!(
        output.status.success(),
        "`galec-production` must compile `{model}`.\nstderr:\n{}",
        strip_ansi(&String::from_utf8_lossy(&output.stderr))
    );
    let path = out_dir
        .join(model)
        .join("ProductionCode")
        .join(format!("{model}.c"));
    fs::read_to_string(&path).unwrap_or_else(|error| panic!("read {}: {error}", path.display()))
}

/// The inclusive end index of the balanced-brace block that opens at `open`.
fn balanced_end(text: &str, open: usize) -> Option<usize> {
    let bytes = text.as_bytes();
    let mut depth = 0usize;
    for (index, byte) in bytes.iter().enumerate().skip(open) {
        depth = match byte {
            b'{' => depth + 1,
            b'}' => depth - 1,
            _ => depth,
        };
        if *byte == b'}' && depth == 0 {
            return Some(index);
        }
    }
    None
}

/// The balanced-brace body of the named C function definition (not its forward
/// declaration).
fn definition_body<'a>(source: &'a str, name: &str) -> &'a str {
    let needle = format!("static void {name}(");
    let mut cursor = source;
    loop {
        let start = cursor
            .find(&needle)
            .unwrap_or_else(|| panic!("no definition of `{name}` in emitted C"));
        let rest = &cursor[start..];
        let open = rest.find('{');
        let semi = rest.find(';');
        // A forward declaration closes its signature with `;` before any body
        // brace; skip past it and keep looking for the definition.
        if let Some(semi) = semi
            && open.is_none_or(|open| semi < open)
        {
            cursor = &rest[semi + 1..];
            continue;
        }
        let open = open.unwrap_or_else(|| panic!("no body for `{name}`"));
        let end = balanced_end(rest, open)
            .unwrap_or_else(|| panic!("unbalanced braces in definition of `{name}`"));
        return &rest[open..=end];
    }
}

/// Sizes of each contiguous run of literal-indexed writes to `array` in `body`.
///
/// A branch's element writes are emitted as a contiguous block, so the run
/// sizes are the per-branch write counts. Tensor-loop initializations index by
/// an induction variable, not a literal, so they are excluded and do not join a
/// branch run.
fn assignment_runs(body: &str, array: &str) -> Vec<usize> {
    let prefix = format!("ctx->{array}[");
    let mut runs = Vec::new();
    let mut current = 0usize;
    for line in body.lines() {
        let trimmed = line.trim_start();
        let is_literal_write = trimmed.starts_with(&prefix)
            && trimmed.contains('=')
            && trimmed
                .as_bytes()
                .get(prefix.len())
                .is_some_and(u8::is_ascii_digit);
        if is_literal_write {
            current += 1;
        } else if current > 0 {
            runs.push(current);
            current = 0;
        }
    }
    if current > 0 {
        runs.push(current);
    }
    runs
}

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
    let generated = production_c(REPRO_GALEC, "BranchElementDrop");
    let branch = definition_body(&generated, "branchAssign");
    assert_eq!(
        assignment_runs(branch, "v"),
        vec![3, 3],
        "each branch of `branchAssign` must emit all three element writes:\n{branch}"
    );
    let straight = definition_body(&generated, "straightAssign");
    assert_eq!(
        assignment_runs(straight, "v").iter().sum::<usize>(),
        3,
        "the top-level control must emit all three element writes:\n{straight}"
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
    let generated = production_c(&from_dcm_galec_source(), "ShepperdBranches");
    let body = definition_body(&generated, "from_DCM");
    assert_eq!(
        assignment_runs(body, "q"),
        vec![4, 4, 4, 4],
        "each of Shepperd's four branches must emit all four quaternion components:\n{body}"
    );
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

#[test]
fn no_conditional_function_drops_indexed_writes() {
    // Structural invariant applied to both emitted models: for every protected
    // function that writes an array element inside a branch, the number of
    // element writes emitted per branch is at least the number in the source.
    // Dropping any would shorten a run below its source count.
    let repro = production_c(REPRO_GALEC, "BranchElementDrop");
    assert!(
        assignment_runs(definition_body(&repro, "branchAssign"), "v")
            .iter()
            .all(|run| *run >= 3),
        "a `branchAssign` branch emitted fewer than the three source writes"
    );

    let shepperd = production_c(&from_dcm_galec_source(), "ShepperdBranches");
    assert!(
        assignment_runs(definition_body(&shepperd, "from_DCM"), "q")
            .iter()
            .all(|run| *run >= 4),
        "a `from_DCM` branch emitted fewer than the four source writes"
    );
}
