//! End-to-end coverage for the two emission axes: `--inline-policy` (how much
//! CALL structure the emitted C keeps) and `--scalarize-policy` (whether a
//! tensor operation may be expanded into per-element statements), plus the
//! `--emission-policy` shorthand over the pair.
//!
//! The axes are separate because they differ in KIND. Inlining is
//! information-preserving: an inlined contraction is still a contraction, so it
//! costs flash and reviewability but no structural right, and it stays eligible
//! for the certification path. Scalarization is information-destroying: index
//! sets, symmetry and bandedness do not survive it. Every property pinned here
//! follows from that distinction:
//!
//! 1. **The fully structured emission is reachable and is a no-op.**
//!    `--emission-policy reviewable` and `--inline-policy none
//!    --scalarize-policy never` are the same artifact, byte for byte, and it is
//!    what a compiler with no dial at all emits.
//! 2. **Only expansion taints.** A block built with inlining still declares
//!    itself eligible; a block that expanded tensors would not.
//! 3. **The refusal is clean.** Expansion is not implemented, so asking for it
//!    is an error naming what is missing, never a silent no-op that emits
//!    structured code under a header claiming otherwise.
//! 4. **Every setting computes the same numbers**, exactly. Inlining removes
//!    boundaries and copies without reordering any floating-point operation, so
//!    this is equality, not a tolerance.
//!
//! The suite drives the real binary, so clap parsing, CLI dispatch, the target
//! manifest, the GALEC projection and the templates are all inside what the
//! assertions cover.

use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::tempdir;

use super::cc_support::assurance_c99_cc;
use super::cli_support::{run_compile_target_with, write_fixture};

const TARGET: &str = "embedded-c-galec";

const MODEL: &str = "EmissionPolicyDial";

/// One callee with a single call site, one callee with two, one callee the
/// source forbids inlining, and one callee returning a tensor.
///
/// The four populations are exactly the ones the cost model, the annotations
/// and the scalarization axis have to tell apart, and keeping them in one
/// fixture means every assertion below reads the same emitted block.
const FIXTURE: &str = "\
function scaleOnce
  input Real value;
  input Real gain;
  output Real result;
algorithm
  result := gain * value + 1.0;
end scaleOnce;

function squareTwice
  input Real value;
  output Real result;
algorithm
  result := value * value;
annotation(
  Inline = true);
end squareTwice;

function keepAsCall
  input Real value;
  output Real result;
algorithm
  result := value + 0.5;
annotation(
  Inline = false);
end keepAsCall;

function scaleVector
  input Real v[3];
  input Real gain;
  output Real result[3];
algorithm
  result := gain * v;
end scaleVector;

model EmissionPolicyDial
  constant Real samplePeriod = 0.1;
  input Real u;
  discrete output Real scaled(start = 0.0);
  discrete output Real squared(start = 0.0);
  discrete output Real kept(start = 0.0);
  discrete output Real vector[3](each start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    scaled = scaleOnce(u, 2.0);
    squared = squareTwice(u) + squareTwice(pre(scaled));
    kept = keepAsCall(u);
    vector = scaleVector({u, 2.0 * u, 3.0 * u}, 0.5);
  end when;
end EmissionPolicyDial;
";

const DRIVER: &str = "\
#include <stdio.h>
#include \"EmissionPolicyDial.h\"

int main(void) {
    EmissionPolicyDialState state;
    EmissionPolicyDial_startup(&state);
    for (int step = 0; step < 4; ++step) {
        state.u = 0.25f * (float)(step + 1);
        EmissionPolicyDial_dostep(&state);
        printf(\"%.9e %.9e %.9e %.9e %.9e %.9e\\n\",
               (double)state.scaled, (double)state.squared, (double)state.kept,
               (double)state.vector[0], (double)state.vector[1],
               (double)state.vector[2]);
    }
    return 0;
}
";

/// Run `compile` on the fixture with `flags` and return the process output plus
/// the directory it wrote into.
fn compile(dir: &Path, flags: &[&str]) -> (std::process::Output, std::path::PathBuf) {
    let out_dir = dir.join("out");
    let file = write_fixture(dir, MODEL, FIXTURE);
    let output = run_compile_target_with(&file, TARGET, &out_dir, flags);
    (output, out_dir)
}

/// Generate the fixture under `flags` and return the emitted model C.
fn generate(flags: &[&str]) -> String {
    let dir = tempdir().expect("tempdir");
    let (output, out_dir) = compile(dir.path(), flags);
    assert!(
        output.status.success(),
        "`compile --target {TARGET} {flags:?}` failed.\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    fs::read_to_string(out_dir.join(format!("{MODEL}.c"))).expect("read generated C source")
}

/// Generate under `flags`, build the emitted C with the driver, run it, and
/// return the program's stdout.
fn generate_and_run(flags: &[&str]) -> String {
    let dir = tempdir().expect("tempdir");
    let (output, out_dir) = compile(dir.path(), flags);
    assert!(
        output.status.success(),
        "`compile --target {TARGET} {flags:?}` failed.\nstderr:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let source = out_dir.join(format!("{MODEL}.c"));
    build_and_run(&out_dir, &source, flags)
}

fn build_and_run(out_dir: &Path, source: &Path, flags: &[&str]) -> String {
    let driver_path = out_dir.join("main.c");
    fs::write(&driver_path, DRIVER).expect("write driver");
    let program = out_dir.join("probe");
    let compile = assurance_c99_cc()
        .arg("-o")
        .arg(&program)
        .arg(&driver_path)
        .arg(source)
        .arg(out_dir.join(super::cc_support::GALEC_KERNEL_LIBRARY))
        .arg("-lm")
        .output()
        .expect("run cc");
    assert!(
        compile.status.success(),
        "strict cc compile failed for {flags:?}.\nstderr:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&program).output().expect("run probe");
    assert!(
        run.status.success(),
        "probe for {flags:?} exited with {:?}",
        run.status.code()
    );
    String::from_utf8_lossy(&run.stdout).replace("\r\n", "\n")
}

/// The heading the emitted block carries when something was collapsed or
/// expanded.
const DISCLOSURE: &str = "How this block was built";

/// Whether the emitted translation unit still defines a C function for one
/// Modelica function.
///
/// The C symbol allocator may shorten a name, but a protected function keeps
/// the model's spelling when it is unambiguous, which it is for every callee in
/// this fixture.
fn defines_function(generated: &str, name: &str) -> bool {
    generated.contains(&format!("static void {name}("))
}

/// The fully structured emission is reachable two ways and is the same artifact
/// both ways, with nothing to disclose.
///
/// This is the invariant the whole feature rests on: a dial can only be trusted
/// as a choice if the point that changes nothing provably changes nothing.
#[test]
fn the_fully_structured_emission_is_one_artifact_under_either_spelling() {
    let preset = generate(&["--emission-policy", "reviewable"]);
    let axes = generate(&["--inline-policy", "none", "--scalarize-policy", "never"]);
    assert_eq!(
        preset, axes,
        "the `reviewable` preset must be exactly (inline none, scalarize never)"
    );
    assert!(
        !preset.contains(DISCLOSURE),
        "an artifact that kept every boundary and every tensor has nothing to disclose:\n{preset}"
    );
    assert!(
        defines_function(&preset, "squareTwice") && defines_function(&preset, "scaleVector"),
        "`--inline-policy none` must keep every declared function a call:\n{preset}"
    );
}

/// Inlining discloses itself and stays eligible; it is not a taint.
#[test]
fn inlining_discloses_itself_and_keeps_the_artifact_eligible() {
    for inline in ["annotated", "cost-model", "all"] {
        let generated = generate(&["--inline-policy", inline]);
        assert!(
            generated.contains(DISCLOSURE),
            "`--inline-policy {inline}` must disclose how the block was built:\n{generated}"
        );
        assert!(
            generated.contains(&format!("inline policy:    {inline}")),
            "the disclosure must name the inline policy in force:\n{generated}"
        );
        assert!(
            generated.contains("scalarize policy: never"),
            "the disclosure must name the scalarization axis too:\n{generated}"
        );
        assert!(
            generated.contains("ELIGIBLE for the certification path"),
            "inlining preserves information, so it must not taint the artifact:\n{generated}"
        );
    }
}

/// Expansion is not implemented, so asking for it is refused by name.
///
/// A silent no-op would be the worst outcome available: structured code under a
/// header claiming a tradeoff the compiler never made.
#[test]
fn an_unimplemented_scalarization_setting_is_refused_by_name() {
    for flags in [
        vec!["--scalarize-policy", "all"],
        vec!["--scalarize-policy", "cost-model"],
        vec!["--emission-policy", "flat"],
    ] {
        let dir = tempdir().expect("tempdir");
        let (output, _) = compile(dir.path(), &flags);
        assert!(
            !output.status.success(),
            "`{flags:?}` must be refused while expansion is unimplemented"
        );
        let stderr = super::cli_support::strip_ansi(&String::from_utf8_lossy(&output.stderr));
        assert!(
            stderr.replace('\n', " ").contains("scalarize-policy"),
            "the refusal must name the setting that is missing:\n{stderr}"
        );
    }
}

/// A tensor-returning callee keeps its call at every implemented setting.
///
/// Substituting it through this projection would lower one element at a time,
/// which is scalarization by another route, and the scalarization axis declines
/// it. The refusal is what keeps `--inline-policy all` an inlining setting
/// rather than a quiet expansion setting.
#[test]
fn a_tensor_returning_callee_keeps_its_call_at_every_inline_setting() {
    for inline in ["none", "annotated", "cost-model", "all"] {
        let generated = generate(&["--inline-policy", inline]);
        assert!(
            defines_function(&generated, "scaleVector"),
            "`--inline-policy {inline}` must not expand a tensor result:\n{generated}"
        );
    }
}

/// Every setting computes the same numbers, exactly.
///
/// Substituting a body removes a boundary and copies; it reorders no
/// floating-point operation. So this is equality of the printed values, and a
/// tolerance here would be hiding a defect rather than accommodating one.
#[test]
fn every_inline_setting_computes_bit_identical_values() {
    let structured = generate_and_run(&["--inline-policy", "none"]);
    assert!(
        !structured.is_empty(),
        "the driver must print the values being compared"
    );
    for inline in ["annotated", "cost-model", "all"] {
        let observed = generate_and_run(&["--inline-policy", inline]);
        assert_eq!(
            observed, structured,
            "`--inline-policy {inline}` changed what the block computes"
        );
    }
}
