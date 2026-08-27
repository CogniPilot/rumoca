//! End-to-end CLI coverage for `rumoca compile --target galec-production`
//! (SPEC_0034 GAL-021/GAL-024 conformant track, contract rows E1-E8).
//!
//! Invokes the real binary so the whole chain is exercised: CLI dispatch →
//! generic capability gate → GALEC projection facade → a product-agnostic
//! context validated in Rust → jinja templates (the eFMI manifests + C) plus
//! the typed GALEC `.alg` view → the declared checksum and `[package]` graph
//! two-representation container packaging. The target claims the "eFMI Production Code export"
//! rung of the SPEC_0034 conformance ladder, so these tests machine-check
//! that rung:
//!
//! - E1: complete eFMU directory form (`__content.xml` + `schemas/` +
//!   `AlgorithmCode/` + `ProductionCode/`), all three XMLs valid against
//!   the vendored XSDs through a real `xmllint` (a CI-installed hard
//!   dependency — its absence fails, never skips);
//! - E2: the full SHA-1 checksum web recomputed from the exact written
//!   bytes, including the Production Code `ManifestReference` staleness
//!   pair against the on-disk Algorithm Code manifest;
//! - E3: the LogicalData cross-reference chain — every foreign reference
//!   resolves into the Algorithm Code manifest, every AC variable and all
//!   three BlockMethods are mapped exactly once;
//! - E4: determinism boundary (only UUIDs, timestamps, and the checksums
//!   derived from them may differ across runs);
//! - E5: container hygiene (re-run replaces, foreign directories refused);
//! - E6: the ProductionCode C compiles under `cc -Wall -Werror` (hard
//!   dependency, never skip), links with a driver, and reproduces the
//!   discrete dynamics tick for tick;
//! - E7: capability rejection before any output exists;
//! - E8: `rumoca targets` lists the target.
//!
//! Output layout under the chosen out dir (decision documented in
//! `src/efmu.rs`): `<Model>/` is the eFMU directory form (kept pristine,
//! since eFMI defines it as a package format) and `<Model>.efmu` is the
//! zip form beside it.

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::io::Read as _;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use tempfile::tempdir;

// The shared helpers are declared once by the umbrella binary
// that owns this file (see `suite_galec_fmu/main.rs`), so the sibling suites share
// one copy instead of compiling the same file several times per binary.
use super::cc_support::assurance_c99_cc;
use super::cli_support::{diagnostic_contains, run_compile_target, strip_ansi, write_fixture};
use super::container_xml_support::{
    assert_xsd_rejects, attribute_values, mask_attribute, mask_uuids, move_line_after,
    relative_file_paths, sole_attribute_value, surgically, validate_against_xsd,
    vendored_schemas_dir, without_block, without_line,
};
use super::metadata_support::{assert_manifest_id, assert_strict_utc_timestamp};

/// The kernel library's header: the prototypes the model unit includes, and
/// the `RUMOCA_GALEC_KERNELS_VERSION` constant the model unit's `#error`
/// check reads. Emitted beside `cc_support::GALEC_KERNEL_LIBRARY` by both C
/// targets; only this suite's manifest/file-set assertions name it.
const GALEC_KERNEL_HEADER: &str = "rumoca_galec_kernels.h";

/// Fixed-sample discrete fixture: a parameter, a `pre()` state, an output,
/// and one `when sample(...)` clock — the shape the GALEC projection
/// admits (mirrors `cli_target_galec.rs`).
const DISCRETE_FIXTURE: &str = "\
model GalecProdCliSmoke
  constant Real samplePeriod = 0.1;
  parameter Real gain = 2.0;
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = gain * (pre(y) + 1.0);
  end when;
end GalecProdCliSmoke;
";

const MODEL: &str = "GalecProdCliSmoke";

/// Continuous model the capability gate must reject (GAL-006, row E7).
const CONTINUOUS_FIXTURE: &str = "\
model GalecProdCliContinuous
  Real x(start = 1.0);
  parameter Real k = 2.0;
equation
  der(x) = -k * x;
end GalecProdCliContinuous;
";

/// A component block with an algebraic equation, sampled by the parent. The
/// component output is a hidden algebraic alias of sampled/discrete values and
/// must be folded before the target capability gate sees residual `f_x` rows.
const ALGEBRAIC_COMPONENT_FIXTURE: &str = "\
block AlgebraicGain
  parameter Real k = 2.0;
  input Real u;
  output Real y;
equation
  y = k * u;
end AlgebraicGain;

block AlgorithmGain
  parameter Real k = 2.0;
  input Real u;
  output Real y;
algorithm
  y := k * u;
end AlgorithmGain;

model GalecProdAlgebraicComponent
  constant Real dt = 0.02;
  input Real error;
  discrete output Real out(start = 0.0);
  discrete output Real out2(start = 0.0);
  AlgebraicGain gain(k = 3.0);
  AlgorithmGain alg(k = 4.0);
algorithm
  when sample(0.0, dt) then
    gain.u := error;
    alg.u := error;
    out := gain.y;
    out2 := alg.y;
  end when;
end GalecProdAlgebraicComponent;
";

/// Two dependent parameters whose bindings each hold a conditional. Every
/// dependent parameter is lowered by its own `ExpressionLowerer`, so each one
/// restarts the temporary counter at zero, while their locals all land in the
/// one list that Startup and Recalibrate both declare. Under a shared
/// temporary namespace the two conditionals would both be minted
/// `rumoca_value_conditional_0` and collide (EG012).
const DEPENDENT_CONDITIONAL_FIXTURE: &str = "\
model GalecProdDependentConditionals
  constant Real samplePeriod = 0.1;
  parameter Real trim = 0.25;
  parameter Real limit = 1.5;
  parameter Real lowerBound = if trim > 0.0 then -limit else -1.0;
  parameter Real upperBound = if trim > 0.0 then limit else 1.0;
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = lowerBound + upperBound * pre(y);
  end when;
end GalecProdDependentConditionals;
";

const HELPER_IDIOMS_FIXTURE: &str = "\
function clip
  input Real value;
  input Real lower;
  input Real upper;
  output Real result;
algorithm
  result := min(max(value, lower), upper);
annotation(
  Inline = true);
end clip;

function wrapAngle
  input Real angle(unit = \"rad\");
  output Real result(unit = \"rad\");
algorithm
  result := atan2(sin(angle), cos(angle));
annotation(
  Inline = true);
end wrapAngle;

function lowPass
  input Real sample[:];
  input Real previous[size(sample, 1)];
  input Real sampleWeight;
  output Real result[size(sample, 1)];
algorithm
  result := sampleWeight * sample + (1.0 - sampleWeight) * previous;
annotation(
  Inline = true);
end lowPass;

function vectorNorm
  input Real v[:];
  output Real result;
algorithm
  result := sqrt(v * v);
annotation(
  Inline = true);
end vectorNorm;

function rateLimit
  input Real target;
  input Real current;
  input Real maxStep;
  output Real result;
algorithm
  result := current + clip(target - current, -maxStep, maxStep);
annotation(
  Inline = true);
end rateLimit;

function splitCommand
  input Real value;
  output Real low;
  output Real high;
algorithm
  low := value - 1.0;
  if value > 0.0 then
    high := value;
  else
    high := 0.0;
  end if;
annotation(
  Inline = true);
end splitCommand;

model GalecProdHelperIdioms
  constant Real dt = 0.02;
  parameter Real waypoints[2, 3] = [0.0, 0.0, 0.0; 1.0, 2.0, 3.0];
  input Real sample[3];
  discrete output Real filtered[3](each start = 0.0);
  discrete output Real segmentStart[3](each start = 0.0);
  discrete output Real horizontal[2](each start = 0.0);
  discrete output Real bounded(start = 0.0);
  discrete output Real yaw(start = 0.0);
  discrete output Real roll(start = 0.0);
  discrete output Real splitLow(start = 0.0);
  discrete output Real splitHigh(start = 0.0);
  discrete output Integer currentWaypoint(min = 1, max = 2, start = 1);
algorithm
  when sample(0.0, dt) then
    filtered := lowPass(sample, pre(filtered), 0.5);
    segmentStart := waypoints[currentWaypoint, :];
    horizontal := lowPass(sample[1:2], pre(horizontal), 0.5);
    bounded := clip(vectorNorm(sample[1:2]), -1.0, 1.0);
    yaw := wrapAngle(pre(yaw) + 0.25);
    roll := rateLimit(clip(yaw, -1.0, 1.0), pre(roll), 0.1);
    (splitLow, splitHigh) := splitCommand(sample[1]);
    currentWaypoint := pre(currentWaypoint);
  end when;
end GalecProdHelperIdioms;
";

const GUARDED_TENSOR_CONTRACTION_FIXTURE: &str = "\
function updateSlice
  input Real J[3, 3];
  input Real xi[6];
  output Real result[6];
algorithm
  result := zeros(6);
  result[1:3] := J * xi[1:3];
end updateSlice;

function blockCovariance
  input Real rotation[3, 3];
  input Real positionCovariance[3, 3];
  input Real velocityCovariance[3, 3];
  output Real covariance[6, 6];
algorithm
  covariance := cat(1,
    cat(2, transpose(rotation) * positionCovariance * rotation, zeros(3, 3)),
    cat(2, zeros(3, 3), transpose(rotation) * velocityCovariance * rotation));
end blockCovariance;

model GalecProdGuardedTensorContraction
  constant Real dt = 0.02;
  parameter Real rotation[3, 3] = identity(3);
  parameter Real positionCovariance[3, 3] = diagonal({1.0, 1.0, 1.0});
  parameter Real velocityCovariance[3, 3] = diagonal({2.0, 2.0, 2.0});
  parameter Real xi[6] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0};
  discrete output Real slice[6](each start = 0.0);
  discrete output Real covariance[6, 6](each start = 0.0);
algorithm
  when sample(0.0, dt) then
    slice := updateSlice(rotation, xi);
    covariance := blockCovariance(
      rotation, positionCovariance, velocityCovariance);
  end when;
end GalecProdGuardedTensorContraction;
";

/// A back substitution whose guard the DAE pushes into the loops.
///
/// `if accepted then for ... end if` becomes a guard on each statement inside
/// the loop, so the write index is assigned under a guard while the loop entry
/// carries the value the previous iteration left. Proving the write in range
/// therefore needs the loop-entry range to be the union over the back edge, not
/// just the range established before the loop.
const PUSHED_GUARD_BACK_SUBSTITUTION_FIXTURE: &str = "\
function backSubstitution
  input Real rightHandSide[3, 2];
  input Boolean accepted;
  output Real x[3, 2];
protected
  Integer n = 3;
  Integer rightHandSides = 2;
  Integer pivotRow;
algorithm
  x := zeros(n, rightHandSides);
  pivotRow := 1;
  for reverseIndex in 1:n loop
    if accepted then
      pivotRow := n - reverseIndex + 1;
    end if;
    for rhsIndex in 1:rightHandSides loop
      if accepted then
        x[pivotRow, rhsIndex] := rightHandSide[pivotRow, rhsIndex];
      end if;
    end for;
  end for;
end backSubstitution;

model GalecProdPushedGuardBackSubstitution
  constant Real dt = 0.02;
  discrete output Real y(start = 0.0);
protected
  discrete Real solution[3, 2];
algorithm
  when sample(0.0, dt) then
    solution := backSubstitution([1.0, 2.0; 3.0, 4.0; 5.0, 6.0], true);
    y := solution[1, 1] + solution[3, 2];
  end when;
end GalecProdPushedGuardBackSubstitution;
";

/// The same shape with the write index carrying no range into the loop.
///
/// `pivotRow` holds its declared default where the loop begins, so the union
/// over the back edge spans zero as well as the guarded assignment's range, and
/// zero is outside the extent. Only the correlation between the two guards
/// would rule it out, and that correlation is not modeled, so the write is
/// refused. Pinned so the day it is admitted is a reviewed diff.
const UNSEEDED_GUARD_BACK_SUBSTITUTION_FIXTURE: &str = "\
function backSubstitution
  input Real rightHandSide[3, 2];
  input Boolean accepted;
  output Real x[3, 2];
protected
  Integer n = 3;
  Integer rightHandSides = 2;
  Integer pivotRow;
algorithm
  x := zeros(n, rightHandSides);
  if accepted then
    for reverseIndex in 1:n loop
      pivotRow := n - reverseIndex + 1;
      for rhsIndex in 1:rightHandSides loop
        x[pivotRow, rhsIndex] := rightHandSide[pivotRow, rhsIndex];
      end for;
    end for;
  end if;
end backSubstitution;

model GalecProdUnseededGuardBackSubstitution
  constant Real dt = 0.02;
  discrete output Real y(start = 0.0);
protected
  discrete Real solution[3, 2];
algorithm
  when sample(0.0, dt) then
    solution := backSubstitution([1.0, 2.0; 3.0, 4.0; 5.0, 6.0], true);
    y := solution[1, 1] + solution[3, 2];
  end when;
end GalecProdUnseededGuardBackSubstitution;
";

const UNSUPPORTED_GALEC_BUILTIN_FIXTURE: &str = "\
model GalecProdUnsupportedBuiltin
  constant Real dt = 0.02;
  discrete output Integer y(start = 0);
algorithm
  when sample(0.0, dt) then
    y := mod(3, 2);
  end when;
end GalecProdUnsupportedBuiltin;
";

/// Driver exercising the packaged block: startup, recalibrate, then three
/// dostep ticks of `y = gain * (pre(y) + 1)` with `gain = 2`, `y0 = 0`
/// (expected 2, 6, 14) — row E6.
const DRIVER_MAIN: &str = "\
#include <stdio.h>
#include \"GalecProdCliSmoke.h\"

int main(void) {
    GalecProdCliSmokeState state;
    GalecProdCliSmoke_startup(&state);
    GalecProdCliSmoke_recalibrate(&state);
    for (int step = 0; step < 3; ++step) {
        GalecProdCliSmoke_dostep(&state);
        printf(\"%.1f\\n\", state.y);
    }
    return 0;
}
";

fn run_compile_galec_production(file: &Path, out_dir: &Path) -> Output {
    run_compile_target(file, "galec-production", out_dir)
}

/// One packaged two-representation eFMU produced by a real CLI run.
struct BuiltContainer {
    /// The eFMU directory-form root (`<out_dir>/<Model>/`).
    root: PathBuf,
    /// The `.efmu` zip form (`<out_dir>/<Model>.efmu`).
    efmu_zip: PathBuf,
    /// CLI stderr of the successful run (completion-message assertions).
    cli_stderr: String,
}

impl BuiltContainer {
    fn content_xml(&self) -> PathBuf {
        self.root.join("__content.xml")
    }

    fn ac_manifest(&self) -> PathBuf {
        self.root.join("AlgorithmCode").join("manifest.xml")
    }

    fn alg_file(&self) -> PathBuf {
        self.root.join("AlgorithmCode").join(format!("{MODEL}.alg"))
    }

    fn pc_manifest(&self) -> PathBuf {
        self.root.join("ProductionCode").join("manifest.xml")
    }

    fn c_header(&self) -> PathBuf {
        self.root.join("ProductionCode").join(format!("{MODEL}.h"))
    }

    fn c_source(&self) -> PathBuf {
        self.root.join("ProductionCode").join(format!("{MODEL}.c"))
    }

    /// The container's second Production Code translation unit: the shared
    /// array-kernel library the model unit calls into. Every link of
    /// [`Self::c_source`] must include it.
    fn c_kernels(&self) -> PathBuf {
        self.root
            .join("ProductionCode")
            .join(super::cc_support::GALEC_KERNEL_LIBRARY)
    }

    /// The kernel library's header (prototypes + the version constant the
    /// model unit's `#error` check reads).
    fn c_kernels_header(&self) -> PathBuf {
        self.root.join("ProductionCode").join(GALEC_KERNEL_HEADER)
    }
}

/// Negative schema cases (contract §7): the vendored Production Code XSD must
/// REJECT a corrupted manifest — the proof a template regression cannot slip
/// past the positive xmllint pass. Ported from the dissolved `rumoca-efmi`
/// crate, now corrupting the TEMPLATE-rendered manifest.
#[test]
fn corrupted_production_code_manifest_is_rejected_by_the_xsd() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let container = build_container(dir.path(), &out_dir);
    let manifest = fs::read_to_string(container.pc_manifest()).expect("read PC manifest");
    let xsd = vendored_schemas_dir("galec-production")
        .join("ProductionCode/efmiProductionCodeManifest.xsd");

    validate_against_xsd(&container.pc_manifest(), &xsd)
        .expect("pristine rendered PC manifest must be schema-valid");

    // Bad language enumeration value.
    assert_xsd_rejects(
        "bad language enum",
        &surgically(&manifest, "language=\"C\"", "language=\"Rust\""),
        &xsd,
    );
    // Malformed ManifestReference UUID: braces required by the id type.
    assert_xsd_rejects(
        "unbraced ManifestReference UUID",
        &surgically(&manifest, "manifestRefId=\"{", "manifestRefId=\""),
        &xsd,
    );
    // Missing required Target element.
    assert_xsd_rejects(
        "missing Target",
        &without_line(&manifest, "<Target>Generic</Target>"),
        &xsd,
    );
    // Missing required LogicalData block.
    assert_xsd_rejects(
        "missing LogicalData",
        &without_block(&manifest, "<LogicalData>", "</LogicalData>"),
        &xsd,
    );
    // Wrong child order: Target moved after CodeFiles (violates the
    // CodeContainer xs:sequence, which places Target before CodeFiles).
    assert_xsd_rejects(
        "Target after CodeFiles",
        &move_line_after(&manifest, "<Target>Generic</Target>", "</CodeFiles>"),
        &xsd,
    );
}

/// Compile the discrete fixture into `out_dir` and return the container
/// paths, failing loudly on any CLI error.
fn build_container(work_dir: &Path, out_dir: &Path) -> BuiltContainer {
    let file = write_fixture(work_dir, MODEL, DISCRETE_FIXTURE);
    let output = run_compile_galec_production(&file, out_dir);
    assert!(
        output.status.success(),
        "`compile --target galec-production` failed (status {:?}).\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    BuiltContainer {
        root: out_dir.join(MODEL),
        efmu_zip: out_dir.join(format!("{MODEL}.efmu")),
        cli_stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
    }
}

#[test]
fn failed_compile_invalidates_previous_container_and_archive() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let container = build_container(dir.path(), &out_dir);
    assert!(container.root.is_dir());
    assert!(container.efmu_zip.is_file());

    let file = write_fixture(
        dir.path(),
        MODEL,
        &DISCRETE_FIXTURE.replace("gain *", "missingSymbol *"),
    );
    let output = run_compile_galec_production(&file, &out_dir);
    assert!(
        !output.status.success(),
        "broken source must fail compilation"
    );
    assert!(
        !container.root.exists(),
        "failed compilation must invalidate the previous container directory"
    );
    assert!(
        !container.efmu_zip.exists(),
        "failed compilation must invalidate the previous eFMU archive"
    );
}

#[test]
fn algebraic_component_output_read_by_sampled_parent_compiles() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let model = "GalecProdAlgebraicComponent";
    let file = write_fixture(dir.path(), model, ALGEBRAIC_COMPONENT_FIXTURE);
    let output = run_compile_galec_production(&file, &out_dir);
    assert!(
        output.status.success(),
        "algebraic component output should not leave residual continuous equations.\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(out_dir.join(model).join("__content.xml").is_file());
    assert!(out_dir.join(format!("{model}.efmu")).is_file());
}

#[test]
fn two_dependent_parameters_each_holding_a_conditional_get_distinct_temporaries() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let model = "GalecProdDependentConditionals";
    let file = write_fixture(dir.path(), model, DEPENDENT_CONDITIONAL_FIXTURE);
    let output = run_compile_galec_production(&file, &out_dir);
    assert!(
        output.status.success(),
        "two dependent parameters that each materialize a conditional must not \
         mint the same temporary.\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let alg = fs::read_to_string(
        out_dir
            .join(model)
            .join("AlgorithmCode")
            .join(format!("{model}.alg")),
    )
    .expect("read generated Algorithm Code");
    // Both conditionals are still the zeroth temporary of their own lowerer;
    // what separates them is the namespace naming the parameter that owns it.
    let minted = alg
        .match_indices("_conditional_0")
        .map(|(index, _)| {
            let start = alg[..index]
                .rfind(|character: char| !character.is_alphanumeric() && character != '_')
                .map_or(0, |boundary| boundary + 1);
            alg[start..index + "_conditional_0".len()].to_owned()
        })
        .collect::<BTreeSet<_>>();
    assert_eq!(
        minted.len(),
        2,
        "each dependent parameter owns one conditional temporary, and the two \
         must carry different names: {minted:?}\n{alg}"
    );
    assert!(
        minted
            .iter()
            .all(|name| name.starts_with("rumoca_dependent")),
        "a dependent parameter's temporary is namespaced by the parameter that \
         owns it: {minted:?}"
    );
}

#[test]
fn helper_specializations_multi_output_calls_and_row_slices_compile() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let model = "GalecProdHelperIdioms";
    let file = write_fixture(dir.path(), model, HELPER_IDIOMS_FIXTURE);
    // The subject here is that one helper called at two shapes becomes two
    // specializations, each emitted and each called. That is a property of the
    // fully structured emission, so it is asked for by name: the fixture's
    // helpers carry `annotation(Inline = true)`, which the default inline
    // policy now honours, and an inlined helper has no call to assert about.
    let output = super::cli_support::run_compile_target_with(
        &file,
        "galec-production",
        &out_dir,
        &["--inline-policy", "none"],
    );
    assert!(
        output.status.success(),
        "GALEC production target should compile inline scalar helpers, \
         vector-returning helpers, and row slices.\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let alg = fs::read_to_string(
        out_dir
            .join(model)
            .join("AlgorithmCode")
            .join(format!("{model}.alg")),
    )
    .expect("read generated Algorithm Code");
    assert!(alg.contains("self.sample[1]"), "{alg}");
    assert!(alg.contains("self.waypoints["), "{alg}");
    assert!(alg.contains("self.segmentStart[3] :="), "{alg}");
    assert!(alg.contains("self.splitHigh :="), "{alg}");
    assert!(alg.contains("self.currentWaypoint :="), "{alg}");
    assert!(alg.contains("function lowPass_specialization_0"), "{alg}");
    assert!(alg.contains("function lowPass_specialization_1"), "{alg}");
    assert!(alg.contains("lowPass_specialization_0("), "{alg}");
    assert!(alg.contains("lowPass_specialization_1("), "{alg}");
    assert!(alg.contains("vectorNorm("), "{alg}");
    assert!(alg.contains("rateLimit("), "{alg}");
    assert!(alg.contains("function splitCommand"), "{alg}");
    assert!(alg.contains(") := splitCommand("), "{alg}");
    assert!(alg.contains("clip("), "{alg}");
    assert!(alg.contains("wrapAngle("), "{alg}");
    assert!(alg.contains(" in 1:2 loop"), "{alg}");
}

#[test]
fn guarded_tensor_contractions_compile_as_strict_c99_without_eager_out_of_bounds_access() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let model = "GalecProdGuardedTensorContraction";
    let file = write_fixture(dir.path(), model, GUARDED_TENSOR_CONTRACTION_FIXTURE);
    let output = run_compile_galec_production(&file, &out_dir);
    assert!(
        output.status.success(),
        "guarded tensor contraction fixture failed to export.\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let source = out_dir
        .join(model)
        .join("ProductionCode")
        .join(format!("{model}.c"));
    let object = dir.path().join("guarded-tensor-contraction.o");
    let compile = assurance_c99_cc()
        .arg("-c")
        .arg(&source)
        .arg("-o")
        .arg(&object)
        .output()
        .expect("run strict C compiler");
    assert!(
        compile.status.success(),
        "strict C99 rejected guarded tensor contractions.\nstderr:\n{}\nsource:\n{}",
        String::from_utf8_lossy(&compile.stderr),
        fs::read_to_string(source).expect("read generated source")
    );
}

#[test]
fn a_guard_pushed_into_a_loop_keeps_the_write_index_proven_across_the_back_edge() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let model = "GalecProdPushedGuardBackSubstitution";
    let file = write_fixture(dir.path(), model, PUSHED_GUARD_BACK_SUBSTITUTION_FIXTURE);
    let output = run_compile_galec_production(&file, &out_dir);
    assert!(
        output.status.success(),
        "a guard pushed into a loop should keep its write index proven.\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let alg = fs::read_to_string(
        out_dir
            .join(model)
            .join("AlgorithmCode")
            .join(format!("{model}.alg")),
    )
    .expect("read generated Algorithm Code");
    // The write is dispatched over the proven range, so the last arm carries no
    // test of its own: that arm is only reachable for an index inside it.
    assert!(alg.contains("if pivotRow == 1 then"), "{alg}");
    assert!(alg.contains("elseif pivotRow == 2 then"), "{alg}");
}

#[test]
fn a_write_index_the_loop_entry_cannot_bound_is_still_refused() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let model = "GalecProdUnseededGuardBackSubstitution";
    let file = write_fixture(dir.path(), model, UNSEEDED_GUARD_BACK_SUBSTITUTION_FIXTURE);
    let output = run_compile_galec_production(&file, &out_dir);
    assert!(
        !output.status.success(),
        "an index whose loop entry spans zero must not be admitted"
    );
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(stderr.contains("EGT017"), "{stderr}");
    assert!(stderr.contains("dynamic-array-index"), "{stderr}");
}

#[test]
fn unsupported_galec_projection_diagnostic_points_at_source_expression() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let model = "GalecProdUnsupportedBuiltin";
    let file = write_fixture(dir.path(), model, UNSUPPORTED_GALEC_BUILTIN_FIXTURE);
    let output = run_compile_galec_production(&file, &out_dir);
    assert!(
        !output.status.success(),
        "unsupported GALEC builtin should be rejected"
    );
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(stderr.contains("EGT017"), "{stderr}");
    assert!(stderr.contains("builtin:mod"), "{stderr}");
    assert!(
        stderr.contains("mod(3, 2)"),
        "projection diagnostic should show the source expression:\n{stderr}"
    );
}

/// Attribute name → value maps for every element called `element_name`,
/// in document order (quick-xml over the exact on-disk bytes).
fn element_attribute_maps(xml_path: &Path, element_name: &str) -> Vec<BTreeMap<String, String>> {
    let bytes = fs::read(xml_path).expect("read XML file");
    let mut reader = quick_xml::Reader::from_reader(bytes.as_slice());
    let mut maps = Vec::new();
    let mut buf = Vec::new();
    loop {
        use quick_xml::events::Event;
        match reader.read_event_into(&mut buf).expect("well-formed XML") {
            Event::Eof => break,
            Event::Start(element) | Event::Empty(element)
                if element.name().as_ref() == element_name.as_bytes() =>
            {
                maps.push(all_attributes(&element));
            }
            _ => {}
        }
        buf.clear();
    }
    maps
}

/// All attributes of one element as a name → value map.
fn all_attributes(element: &quick_xml::events::BytesStart<'_>) -> BTreeMap<String, String> {
    element
        .attributes()
        .map(|attribute| attribute.expect("well-formed attribute"))
        .map(|attribute| {
            let key = String::from_utf8(attribute.key.as_ref().to_vec()).expect("UTF-8 attribute");
            let value = attribute
                .unescape_value()
                .expect("unescapable attribute")
                .into_owned();
            (key, value)
        })
        .collect()
}

/// The attribute map of the single element called `element_name`.
fn sole_element_attributes(xml_path: &Path, element_name: &str) -> BTreeMap<String, String> {
    let mut maps = element_attribute_maps(xml_path, element_name);
    assert_eq!(
        maps.len(),
        1,
        "expected exactly one `{element_name}` element in {}",
        xml_path.display()
    );
    maps.pop().expect("length checked")
}

/// `id` attribute values of every element nested inside the `wrapper`
/// element (the wrapper's own attributes excluded), in document order.
fn ids_inside_wrapper(xml_path: &Path, wrapper: &str) -> Vec<String> {
    let bytes = fs::read(xml_path).expect("read XML file");
    let mut reader = quick_xml::Reader::from_reader(bytes.as_slice());
    let mut ids = Vec::new();
    let mut buf = Vec::new();
    let mut inside: usize = 0;
    loop {
        use quick_xml::events::Event;
        match reader.read_event_into(&mut buf).expect("well-formed XML") {
            Event::Eof => break,
            Event::Start(element) => {
                if element.name().as_ref() == wrapper.as_bytes() {
                    inside += 1;
                } else if inside > 0 {
                    ids.extend(all_attributes(&element).remove("id"));
                }
            }
            Event::Empty(element) if inside > 0 => {
                ids.extend(all_attributes(&element).remove("id"));
            }
            Event::End(element) if element.name().as_ref() == wrapper.as_bytes() => {
                inside = inside.checked_sub(1).expect("well-nested wrapper elements");
            }
            _ => {}
        }
        buf.clear();
    }
    ids
}

/// The document's root-element `id` (the first `id` in document order).
fn root_id(xml_path: &Path) -> String {
    attribute_values(xml_path, "id")
        .into_iter()
        .next()
        .unwrap_or_else(|| panic!("no id attribute in {}", xml_path.display()))
}

/// Mask `attribute` only inside the opening tags of `element` — used for
/// the PC `ManifestReference@checksum`, which derives from the (UUID- and
/// timestamp-bearing) AC manifest bytes, while `File@checksum` entries in
/// the same document must stay byte-identical across runs.
fn mask_element_scoped_attribute(text: &str, element: &str, attribute: &str) -> String {
    let open = format!("<{element}");
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    while let Some(at) = rest.find(&open) {
        let tag_end = at
            + rest[at..]
                .find('>')
                .expect("element opening tag must close");
        out.push_str(&rest[..at]);
        out.push_str(&mask_attribute(&rest[at..=tag_end], attribute));
        rest = &rest[tag_end + 1..];
    }
    out.push_str(rest);
    out
}

/// Row E1: the on-disk layout is a complete two-representation eFMU
/// directory form and all three XML documents validate against the
/// vendored XSDs.
#[test]
fn compile_target_galec_production_emits_schema_valid_two_representation_efmu() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let container = build_container(dir.path(), &out_dir);

    // The eFMU root holds exactly __content.xml, schemas/, and the two
    // model representation containers (eFMI ch. 2; co-emission of the
    // Algorithm Code representation is mandatory — a PC-only container
    // would be non-conformant).
    let root_entries: BTreeSet<String> = fs::read_dir(&container.root)
        .expect("read container root")
        .map(|entry| entry.expect("dir entry").file_name().into_string().unwrap())
        .collect();
    let expected: BTreeSet<String> = [
        "__content.xml",
        "schemas",
        "AlgorithmCode",
        "ProductionCode",
    ]
    .into_iter()
    .map(str::to_owned)
    .collect();
    assert_eq!(
        root_entries, expected,
        "eFMU root must hold exactly __content.xml, schemas/, AlgorithmCode/, ProductionCode/"
    );

    // Each representation directory holds exactly its manifest and code.
    assert_eq!(
        relative_file_paths(&container.root.join("AlgorithmCode")),
        [format!("{MODEL}.alg"), "manifest.xml".to_owned()]
            .into_iter()
            .collect::<BTreeSet<_>>(),
        "AlgorithmCode/ must hold exactly the .alg and its manifest"
    );
    assert_eq!(
        relative_file_paths(&container.root.join("ProductionCode")),
        [
            ".clang-format".to_owned(),
            format!("{MODEL}.c"),
            format!("{MODEL}.h"),
            super::cc_support::GALEC_KERNEL_LIBRARY.to_owned(),
            GALEC_KERNEL_HEADER.to_owned(),
            "manifest.xml".to_owned(),
        ]
        .into_iter()
        .collect::<BTreeSet<_>>(),
        "ProductionCode/ must hold exactly the model C pair, the shared \
         kernel-library pair, the formatter standard those C bytes already \
         satisfy, and its manifest"
    );

    // schemas/ is the complete target-owned Beta-1 asset tree, byte for byte
    // (GAL-023), including its origin/license README.
    let schemas = vendored_schemas_dir("galec-production");
    let vendored = relative_file_paths(&schemas);
    let emitted = relative_file_paths(&container.root.join("schemas"));
    assert_eq!(emitted, vendored, "schemas/ must mirror the vendored tree");
    for relative in &vendored {
        let vendored_bytes = fs::read(schemas.join(relative)).unwrap();
        let emitted_bytes = fs::read(container.root.join("schemas").join(relative)).unwrap();
        assert_eq!(
            emitted_bytes, vendored_bytes,
            "schemas/{relative} must be byte-identical to the vendored file"
        );
    }

    // Hard requirement: a missing xmllint surfaces as
    // EfmiError::XmllintUnavailable and fails these expects — the test
    // never skips schema validation (GAL-012/GAL-021).
    validate_against_xsd(
        &container.content_xml(),
        &vendored_schemas_dir("galec-production").join("efmiContainerManifest.xsd"),
    )
    .expect("__content.xml must validate against the vendored container XSD");
    validate_against_xsd(
        &container.ac_manifest(),
        &vendored_schemas_dir("galec-production")
            .join("AlgorithmCode/efmiAlgorithmCodeManifest.xsd"),
    )
    .expect("AlgorithmCode/manifest.xml must validate against the vendored AC XSD");
    validate_against_xsd(
        &container.pc_manifest(),
        &vendored_schemas_dir("galec-production")
            .join("ProductionCode/efmiProductionCodeManifest.xsd"),
    )
    .expect("ProductionCode/manifest.xml must validate against the vendored PC XSD");
    let production_manifest =
        fs::read_to_string(container.pc_manifest()).expect("read Production Code manifest");
    assert!(
        production_manifest.contains("floatPrecision=\"32-bit\"")
            && production_manifest.contains("kind=\"efmiFloat32\"")
            && production_manifest.contains("codedType=\"float\""),
        "Production Code precision metadata must exactly match generated float storage"
    );
    assert!(
        !production_manifest.contains("efmiFloat64")
            && !production_manifest.contains("codedType=\"double\""),
        "the embedded Production Code target must not claim or expose binary64 storage"
    );
}

/// Row E2: the full checksum web recomputes from the bytes actually on
/// disk — `__content.xml` ↔ both manifests, AC manifest ↔ `.alg`, PC
/// manifest ↔ `.h`/`.c`, and the PC `ManifestReference` staleness pair
/// (`@checksum` ↔ on-disk AC manifest bytes AND `@manifestRefId` == AC
/// root id). All five texts must come from one projection pass; pinning
/// the files against each other (never against golden values) catches any
/// re-projection or stale-reference regression.
#[test]
fn container_checksum_web_recomputes_from_written_bytes() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let container = build_container(dir.path(), &out_dir);

    let ac_manifest_bytes = fs::read(container.ac_manifest()).expect("read AC manifest bytes");
    let pc_manifest_bytes = fs::read(container.pc_manifest()).expect("read PC manifest bytes");

    // __content.xml ↔ both manifests: checksum over the written manifest
    // bytes and manifestRefId equal to each manifest's own root id.
    let representations = element_attribute_maps(&container.content_xml(), "ModelRepresentation");
    assert_eq!(
        representations.len(),
        2,
        "__content.xml must list exactly the two representations"
    );
    for (name, manifest_path, manifest_bytes) in [
        ("AlgorithmCode", container.ac_manifest(), &ac_manifest_bytes),
        (
            "ProductionCode",
            container.pc_manifest(),
            &pc_manifest_bytes,
        ),
    ] {
        let entry = representations
            .iter()
            .find(|attrs| attrs.get("name").map(String::as_str) == Some(name))
            .unwrap_or_else(|| panic!("__content.xml must list the {name} representation"));
        assert_eq!(
            entry.get("kind").map(String::as_str),
            Some(name),
            "the {name} entry's kind must match its container directory"
        );
        assert_eq!(
            entry.get("checksum").map(String::as_str),
            Some(rumoca::sha1_hex(manifest_bytes).as_str()),
            "__content.xml {name} checksum must be the SHA-1 of the written manifest.xml"
        );
        assert_eq!(
            entry.get("manifestRefId"),
            Some(&root_id(&manifest_path)),
            "__content.xml {name} manifestRefId must be the manifest's own root id"
        );
    }

    // AC manifest ↔ .alg (the AC manifest carries exactly one checksum).
    let alg_bytes = fs::read(container.alg_file()).expect("read .alg bytes");
    assert_eq!(
        sole_attribute_value(&container.ac_manifest(), "checksum"),
        rumoca::sha1_hex(&alg_bytes),
        "AC manifest File checksum must be the SHA-1 of the written .alg"
    );

    // PC manifest ↔ .h/.c/.clang-format: each listed File checksum recomputes
    // from the written bytes. The formatter standard is a manifested container
    // file, not a stray one: reformatting the C with any other style would
    // break the code checksums above, so the style that governs them has to
    // carry its own recomputable digest.
    let files = element_attribute_maps(&container.pc_manifest(), "File");
    assert_eq!(
        files.len(),
        5,
        "PC manifest must list the model C pair, the kernel-library pair, \
         and the formatter standard"
    );
    for (name, path) in [
        (format!("{MODEL}.h"), container.c_header()),
        (format!("{MODEL}.c"), container.c_source()),
        (GALEC_KERNEL_HEADER.to_owned(), container.c_kernels_header()),
        (
            super::cc_support::GALEC_KERNEL_LIBRARY.to_owned(),
            container.c_kernels(),
        ),
        (
            ".clang-format".to_owned(),
            container.root.join("ProductionCode").join(".clang-format"),
        ),
    ] {
        let entry = files
            .iter()
            .find(|attrs| attrs.get("name") == Some(&name))
            .unwrap_or_else(|| panic!("PC manifest must list {name}"));
        let code_bytes = fs::read(&path).expect("read code file bytes");
        assert_eq!(
            entry.get("checksum").map(String::as_str),
            Some(rumoca::sha1_hex(&code_bytes).as_str()),
            "PC manifest File checksum for {name} must be the SHA-1 of the written bytes"
        );
    }

    // The staleness pair: the PC ManifestReference pins the exact AC
    // manifest this container was assembled against — both the byte
    // checksum AND the typed UUID must match the on-disk AC manifest.
    let reference = sole_element_attributes(&container.pc_manifest(), "ManifestReference");
    assert_eq!(
        reference.get("checksum").map(String::as_str),
        Some(rumoca::sha1_hex(&ac_manifest_bytes).as_str()),
        "PC ManifestReference checksum must be the SHA-1 of the written AC manifest"
    );
    assert_eq!(
        reference.get("manifestRefId"),
        Some(&root_id(&container.ac_manifest())),
        "PC ManifestReference manifestRefId must be the AC manifest's root id"
    );
}

/// Row E3: the LogicalData cross-reference chain. Every foreign reference
/// resolves into the Algorithm Code manifest through the (single)
/// ManifestReference anchor; every AC variable is data-referenced exactly
/// once and all three BlockMethods are function-referenced exactly once
/// (rumoca's own conformance invariant — the XSD alone allows zero
/// references); the local ends of the mapping resolve to declared
/// FormalParameter/Function ids.
#[test]
fn logical_data_cross_references_resolve_and_cover_the_algorithm_code() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let container = build_container(dir.path(), &out_dir);

    // Foreign side: the AC manifest's variable and method id sets.
    let mut ac_variable_ids = ids_inside_wrapper(&container.ac_manifest(), "Variables");
    let mut ac_method_ids = ids_inside_wrapper(&container.ac_manifest(), "BlockMethods");
    ac_variable_ids.sort();
    ac_method_ids.sort();
    assert_eq!(
        ac_variable_ids.len(),
        5,
        "fixture projects four model variables plus the explicit checked clock-period variable, \
         got {ac_variable_ids:?}"
    );
    assert_eq!(
        ac_method_ids.len(),
        3,
        "the block interface has exactly three methods, got {ac_method_ids:?}"
    );

    // Every LogicalData foreign reference names the single ManifestReference
    // anchor (whose UUID E2 pins to the AC manifest).
    let anchor = sole_element_attributes(&container.pc_manifest(), "ManifestReference");
    let anchor_id = anchor.get("id").expect("ManifestReference id");
    let data_refs = element_attribute_maps(&container.pc_manifest(), "ForeignVariableReference");
    let function_refs =
        element_attribute_maps(&container.pc_manifest(), "ForeignFunctionReference");
    for foreign in data_refs.iter().chain(&function_refs) {
        assert_eq!(
            foreign.get("manifestReferenceRefId"),
            Some(anchor_id),
            "every foreign reference must anchor at the ManifestReference"
        );
    }

    // Exactly-once coverage: sorted multiset equality catches both unmapped
    // and doubly-mapped AC entities. ErrorSignalStatus is a sibling of the
    // Variables wrapper in the Algorithm Code schema, but it is runtime data
    // and therefore participates in the same LogicalData mapping proof.
    let error_signal_status =
        sole_element_attributes(&container.ac_manifest(), "ErrorSignalStatus")
            .get("id")
            .expect("ErrorSignalStatus id")
            .clone();
    ac_variable_ids.push(error_signal_status);
    ac_variable_ids.sort();
    let mut data_foreign_ids: Vec<String> = data_refs
        .iter()
        .map(|attrs| attrs.get("foreignRefId").expect("foreignRefId").clone())
        .collect();
    data_foreign_ids.sort();
    assert_eq!(
        data_foreign_ids, ac_variable_ids,
        "every AC variable must be data-referenced exactly once"
    );
    let mut function_foreign_ids: Vec<String> = function_refs
        .iter()
        .map(|attrs| attrs.get("foreignRefId").expect("foreignRefId").clone())
        .collect();
    function_foreign_ids.sort();
    assert_eq!(
        function_foreign_ids, ac_method_ids,
        "all three BlockMethods must be function-referenced exactly once"
    );

    // Local side: formalParameterRefId / functionRefId resolve to declared
    // PC entities (declarations carry `id`, LogicalData arms carry refs).
    let declared_parameter_ids: BTreeSet<String> =
        element_attribute_maps(&container.pc_manifest(), "FormalParameter")
            .into_iter()
            .filter_map(|mut attrs| attrs.remove("id"))
            .collect();
    let declared_function_ids: BTreeSet<String> =
        element_attribute_maps(&container.pc_manifest(), "Function")
            .into_iter()
            .filter_map(|mut attrs| attrs.remove("id"))
            .collect();
    for arm in element_attribute_maps(&container.pc_manifest(), "FormalParameter") {
        let Some(target) = arm.get("formalParameterRefId") else {
            continue; // a declaration, not a LogicalData arm
        };
        assert!(
            declared_parameter_ids.contains(target),
            "formalParameterRefId `{target}` must resolve to a declared FormalParameter"
        );
    }
    let global_functions = element_attribute_maps(&container.pc_manifest(), "GlobalFunction");
    assert_eq!(
        global_functions.len(),
        function_refs.len(),
        "every FunctionReference must carry its GlobalFunction arm"
    );
    for arm in global_functions {
        let target = arm.get("functionRefId").expect("functionRefId");
        assert!(
            declared_function_ids.contains(target),
            "functionRefId `{target}` must resolve to a declared Function"
        );
    }
}

/// Id discipline and generation metadata across all three documents:
/// every `id` is unique over `__content.xml` and BOTH manifests together,
/// both manifest root ids are brace-wrapped UUIDs, and all three documents
/// carry strict UTC timestamps and name this tool.
#[test]
fn container_ids_unique_and_generation_metadata_strict() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let container = build_container(dir.path(), &out_dir);

    let documents = [
        container.content_xml(),
        container.ac_manifest(),
        container.pc_manifest(),
    ];
    let mut seen: BTreeSet<String> = BTreeSet::new();
    for path in &documents {
        for id in attribute_values(path, "id") {
            assert!(
                seen.insert(id.clone()),
                "id `{id}` appears more than once across the container's XML documents"
            );
        }
    }

    for path in [container.ac_manifest(), container.pc_manifest()] {
        let id = root_id(&path);
        assert_manifest_id(&id, &path);
    }

    for path in &documents {
        let timestamp = sole_attribute_value(path, "generationDateAndTime");
        assert_strict_utc_timestamp(&timestamp, path);
        let tool = sole_attribute_value(path, "generationTool");
        assert!(
            tool.starts_with("rumoca "),
            "generationTool must start with `rumoca `, got `{tool}` in {}",
            path.display()
        );
    }
}

/// The `.efmu` zip form holds `__content.xml` at the zip root and is
/// entry-for-entry, byte-for-byte the directory form.
#[test]
fn efmu_zip_matches_directory_form() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let container = build_container(dir.path(), &out_dir);

    let zip_file = fs::File::open(&container.efmu_zip).expect(".efmu zip must exist");
    let mut archive = zip::ZipArchive::new(zip_file).expect("open .efmu as zip");
    let mut zip_entries: BTreeMap<String, Vec<u8>> = BTreeMap::new();
    for index in 0..archive.len() {
        let mut entry = archive.by_index(index).expect("read zip entry");
        assert!(!entry.is_dir(), "zip must contain file entries only");
        let mut bytes = Vec::new();
        entry.read_to_end(&mut bytes).expect("read entry bytes");
        zip_entries.insert(entry.name().to_owned(), bytes);
    }

    assert!(
        zip_entries.contains_key("__content.xml"),
        "__content.xml must sit at the zip root (no wrapper directory)"
    );
    let dir_paths = relative_file_paths(&container.root);
    let zip_paths: BTreeSet<String> = zip_entries.keys().cloned().collect();
    assert_eq!(
        zip_paths, dir_paths,
        "zip entry set must equal the directory form's file set"
    );
    for (relative, zip_bytes) in &zip_entries {
        let disk_bytes = fs::read(container.root.join(relative)).expect("read dir-form file");
        assert_eq!(
            zip_bytes, &disk_bytes,
            "zip entry `{relative}` must be byte-identical to the directory form"
        );
    }
}

/// Row E4, determinism boundary (pins against accidental re-projection):
/// two runs differ ONLY in freshly minted UUIDs, generation timestamps,
/// and the checksums derived from those bytes — in `__content.xml` both
/// representation checksums, and in the PC manifest the ManifestReference
/// checksum over the (UUID-bearing) AC manifest. Everything else — the
/// `.alg` text, the C pair and their recorded File checksums, the schema
/// copies, all structural XML — is byte-identical.
#[test]
fn efmu_builds_differ_only_in_uuids_timestamps_and_derived_checksums() {
    let dir = tempdir().expect("tempdir");
    let first = build_container(dir.path(), &dir.path().join("out1"));
    let second = build_container(dir.path(), &dir.path().join("out2"));

    let first_paths = relative_file_paths(&first.root);
    assert_eq!(
        first_paths,
        relative_file_paths(&second.root),
        "both runs must produce the same file set"
    );

    for relative in &first_paths {
        let bytes_a = fs::read(first.root.join(relative)).unwrap();
        let bytes_b = fs::read(second.root.join(relative)).unwrap();
        let text = |bytes: &[u8]| String::from_utf8(bytes.to_vec()).expect("UTF-8 XML");
        match relative.as_str() {
            "__content.xml" => {
                // Both representation checksums are pure functions of the
                // (uuid/timestamp-bearing) manifest bytes, so they are
                // masked together with the documented nondeterminism.
                let normalize = |bytes: &[u8]| {
                    mask_attribute(
                        &mask_attribute(&mask_uuids(&text(bytes)), "generationDateAndTime"),
                        "checksum",
                    )
                };
                assert_eq!(
                    normalize(&bytes_a),
                    normalize(&bytes_b),
                    "__content.xml may differ only in UUIDs/timestamp/derived checksums"
                );
            }
            "AlgorithmCode/manifest.xml" => {
                // The File checksum is NOT masked: the .alg bytes are
                // deterministic, so their recorded SHA-1 must be too.
                let normalize = |bytes: &[u8]| {
                    mask_attribute(&mask_uuids(&text(bytes)), "generationDateAndTime")
                };
                assert_eq!(
                    normalize(&bytes_a),
                    normalize(&bytes_b),
                    "AC manifest may differ only in its UUID and timestamp"
                );
            }
            "ProductionCode/manifest.xml" => {
                // Masked: root UUID, the ManifestReference's AC UUID (both
                // via mask_uuids), the timestamp, and ONLY the
                // ManifestReference checksum (derived from AC manifest
                // bytes). The File checksums of the C pair must stay
                // byte-identical — the C code is deterministic.
                let normalize = |bytes: &[u8]| {
                    mask_element_scoped_attribute(
                        &mask_attribute(&mask_uuids(&text(bytes)), "generationDateAndTime"),
                        "ManifestReference",
                        "checksum",
                    )
                };
                assert_eq!(
                    normalize(&bytes_a),
                    normalize(&bytes_b),
                    "PC manifest may differ only in UUIDs/timestamp/ManifestReference checksum"
                );
            }
            _ => {
                assert_eq!(
                    bytes_a, bytes_b,
                    "`{relative}` must be byte-identical across runs"
                );
            }
        }
    }

    // Zip forms package the same entry sets.
    for built in [&first, &second] {
        assert!(built.efmu_zip.is_file(), ".efmu zip must exist");
    }
}

/// Row E5, part 1: re-running the identical command into the same
/// --output must replace the previous container (the edit-recompile loop):
/// fresh manifests, and the replaced container's `__content.xml` checksums
/// recompute from its own bytes.
#[test]
fn rerunning_same_command_replaces_previous_container() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let first = build_container(dir.path(), &out_dir);
    let first_pc_manifest = fs::read(first.pc_manifest()).expect("read first PC manifest");

    // Same fixture, same out dir: build_container asserts CLI success.
    let second = build_container(dir.path(), &out_dir);
    let second_pc_manifest = fs::read(second.pc_manifest()).expect("read second PC manifest");
    assert_ne!(
        first_pc_manifest, second_pc_manifest,
        "the container must be rebuilt (fresh manifest UUIDs), not left stale"
    );
    for (name, manifest_path) in [
        ("AlgorithmCode", second.ac_manifest()),
        ("ProductionCode", second.pc_manifest()),
    ] {
        let entry = element_attribute_maps(&second.content_xml(), "ModelRepresentation")
            .into_iter()
            .find(|attrs| attrs.get("name").map(String::as_str) == Some(name))
            .unwrap_or_else(|| panic!("__content.xml must list the {name} representation"));
        let manifest_bytes = fs::read(&manifest_path).expect("read replaced manifest");
        assert_eq!(
            entry.get("checksum").map(String::as_str),
            Some(rumoca::sha1_hex(&manifest_bytes).as_str()),
            "the replaced container's {name} checksum must recompute from its own bytes"
        );
    }
    assert!(second.efmu_zip.is_file(), ".efmu zip must be rebuilt too");
}

/// Row E5, part 2: a foreign non-empty directory at `<out_dir>/<Model>`
/// (no __content.xml) is NOT a previous build product and must be refused
/// with the remedy — never deleted.
#[test]
fn foreign_directory_at_container_path_is_refused_with_remedy() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let foreign = out_dir.join(MODEL);
    fs::create_dir_all(&foreign).expect("create foreign directory");
    let keep = foreign.join("keep.txt");
    fs::write(&keep, b"user data").expect("write foreign file");

    let file = write_fixture(dir.path(), MODEL, DISCRETE_FIXTURE);
    let output = run_compile_galec_production(&file, &out_dir);
    assert!(
        !output.status.success(),
        "packaging over a foreign directory must fail.\nstdout:\n{}",
        String::from_utf8_lossy(&output.stdout)
    );
    let raw_stderr = String::from_utf8_lossy(&output.stderr);
    let stderr = strip_ansi(&raw_stderr);
    assert!(
        diagnostic_contains(&raw_stderr, "refusing to remove")
            && diagnostic_contains(&raw_stderr, &foreign.display().to_string())
            && diagnostic_contains(
                &raw_stderr,
                "Delete it or choose a different `--output` directory."
            ),
        "the error must state the remedy, got stderr:\n{stderr}"
    );
    assert_eq!(
        fs::read(&keep).expect("foreign file must survive"),
        b"user data",
        "foreign content must never be deleted"
    );
}

/// Row E6: the container's ProductionCode C compiles under
/// `cc -Wall -Werror`, links against libm with a real driver, and the
/// executed block reproduces the discrete dynamics tick for tick — the
/// container itself stays pristine (build artifacts live outside the
/// eFMU package).
#[test]
fn production_code_compiles_links_and_reproduces_the_discrete_dynamics() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let container = build_container(dir.path(), &out_dir);
    let container_files_before = relative_file_paths(&container.root);

    let driver = dir.path().join("main.c");
    fs::write(&driver, DRIVER_MAIN).expect("write driver");
    let program = dir.path().join("smoke");
    let compile = assurance_c99_cc()
        .arg("-I")
        .arg(container.root.join("ProductionCode"))
        .arg("-o")
        .arg(&program)
        .arg(&driver)
        .arg(container.c_source())
        // Both Production Code translation units: the model unit calls into
        // the shared kernel library beside it (both are manifested CodeFiles),
        // so this is the link the PC manifest describes.
        .arg(container.c_kernels())
        .arg("-lm")
        .output()
        .expect("run cc");
    assert!(
        compile.status.success(),
        "strict cc -std=c99 compile failed.\nstderr:\n{}\nheader:\n{}\nsource:\n{}",
        String::from_utf8_lossy(&compile.stderr),
        fs::read_to_string(container.c_header()).unwrap_or_default(),
        fs::read_to_string(container.c_source()).unwrap_or_default()
    );

    let run = Command::new(&program)
        .output()
        .expect("run packaged block driver");
    assert!(
        run.status.success(),
        "packaged block driver exited with {:?}",
        run.status.code()
    );
    assert_eq!(
        // Normalize CRLF: Windows text-mode stdio emits `\n` as `\r\n`.
        String::from_utf8_lossy(&run.stdout).replace("\r\n", "\n"),
        "2.0\n6.0\n14.0\n",
        "three dostep ticks of y := gain * (previous(y) + 1) with gain = 2"
    );

    assert_eq!(
        relative_file_paths(&container.root),
        container_files_before,
        "compiling the driver must not add files to the eFMU package"
    );
}

/// Honesty: the emitted C header claims the Production Code representation
/// and points at the manifest as the conformance surface, and the CLI
/// completion message claims the rung while naming what stays unclaimed.
#[test]
fn export_claims_the_production_code_rung_and_names_the_conformance_surface() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let container = build_container(dir.path(), &out_dir);

    let header = fs::read_to_string(container.c_header()).expect("read header");
    assert!(
        header.contains("eFMI Production Code representation"),
        "header must claim the Production Code representation:\n{header}"
    );
    assert!(
        header.contains("ProductionCode/manifest.xml"),
        "header must point at the manifest as the conformance surface:\n{header}"
    );

    let stderr = strip_ansi(&container.cli_stderr);
    assert!(
        stderr.contains("Conformance: \"eFMI Production Code export\""),
        "completion message must claim the rung, got:\n{stderr}"
    );
    assert!(
        stderr.contains("round-trip parses (\"GALEC language conformance\")"),
        "completion message must state the earned language-conformance claim, got:\n{stderr}"
    );
}

/// Row E7: the continuous fixture is rejected by the generic capability
/// gate (GAL-006) before any output exists.
#[test]
fn compile_target_galec_production_rejects_continuous_model_before_any_output() {
    let dir = tempdir().expect("tempdir");
    let file = write_fixture(dir.path(), "GalecProdCliContinuous", CONTINUOUS_FIXTURE);
    let out_dir = dir.path().join("out");

    let output = run_compile_galec_production(&file, &out_dir);
    assert!(
        !output.status.success(),
        "`compile --target galec-production` must fail for a continuous model.\nstdout:\n{}",
        String::from_utf8_lossy(&output.stdout)
    );
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("unsupported-feature:continuous_states"),
        "expected the generic capability diagnostic (GAL-006), got stderr:\n{stderr}"
    );
    // The gate runs before any rendering: nothing may be written on rejection.
    assert!(
        !out_dir.exists(),
        "capability rejection must happen before the output directory is created"
    );
}

/// Row E8: `rumoca targets` lists the target.
#[test]
fn targets_listing_includes_galec_production() {
    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("targets")
        .output()
        .expect("run rumoca targets");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        output.status.success(),
        "`rumoca targets` failed.\nstdout:\n{stdout}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        stdout.contains("galec-production"),
        "`rumoca targets` must list the galec-production target:\n{stdout}"
    );
}

/// Copy a target directory tree, so a test can pass it to `--target <dir>`
/// exactly as a user customizing a built-in target would.
fn copy_tree(from: &Path, to: &Path) {
    fs::create_dir_all(to).expect("create target directory copy");
    for entry in fs::read_dir(from).expect("read target directory") {
        let entry = entry.expect("target directory entry");
        let source = entry.path();
        let destination = to.join(entry.file_name());
        if entry.file_type().expect("stat target entry").is_dir() {
            copy_tree(&source, &destination);
        } else {
            fs::copy(&source, &destination).expect("copy target file");
        }
    }
}

/// Row E9: the target directory copied out of the tree is usable as
/// `--target <dir>` and emits the same container the built-in emits.
///
/// `--target <dir>` is the documented way to customize a target: copy the
/// directory, edit it, pass it. This target's schema bundle is borrowed from
/// `galec` rather than vendored, so the copy contains no `schemas/` of its own,
/// and only the runtime resolving `shared_from` keeps the copy shipping the
/// schemas the eFMU is not conformant without. Nothing else in the repository
/// copies this directory out, so without this row the borrow could break the
/// documented workflow silently.
#[test]
fn galec_production_directory_copy_is_usable_as_a_directory_target() {
    let dir = tempdir().expect("temp dir");
    let target_copy = dir.path().join("galec-production-copy");
    let builtin_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../rumoca-phase-codegen/src/templates/galec-production");
    copy_tree(&builtin_dir, &target_copy);
    assert!(
        !target_copy.join("schemas").exists(),
        "this target borrows its schema bundle, so the copy must hold no schemas/ \
         of its own: that is exactly what makes this row worth running"
    );

    let file = write_fixture(dir.path(), "GalecProdCliSmoke", DISCRETE_FIXTURE);
    let out_dir = dir.path().join("out");
    let output = run_compile_target(
        &file,
        target_copy.to_str().expect("UTF-8 target directory"),
        &out_dir,
    );
    assert!(
        output.status.success(),
        "compiling with the copied target directory failed.\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        strip_ansi(&String::from_utf8_lossy(&output.stderr))
    );

    // The borrowed bundle must arrive whole and byte for byte (GAL-023): an
    // eFMU whose schemas/ is short a file, or holds a different file, is not a
    // conformant container.
    let emitted_schemas = out_dir.join("GalecProdCliSmoke").join("schemas");
    let vendored = vendored_schemas_dir("galec-production");
    let vendored_files = relative_file_paths(&vendored);
    assert_eq!(
        relative_file_paths(&emitted_schemas),
        vendored_files,
        "a directory target must emit the same borrowed schema tree the built-in emits"
    );
    for relative in &vendored_files {
        assert_eq!(
            fs::read(emitted_schemas.join(relative)).expect("read emitted schema"),
            fs::read(vendored.join(relative)).expect("read vendored schema"),
            "schemas/{relative} must be byte-identical to the lender's file"
        );
    }
}
