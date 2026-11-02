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
    // A function owner returns its fault flag (#483: Solve-printed C).
    generated.contains(&format!("static int {name}("))
}

/// The fully structured emission is what no flag produces, is reachable by
/// preset and by axis, is one artifact all three ways, and discloses nothing.
///
/// This is the invariant the whole feature rests on. A structural decision is
/// taken deliberately or not at all, so the point that changes nothing has to
/// be both the default and provably a no-op.
#[test]
fn the_fully_structured_emission_is_one_artifact_under_every_spelling() {
    let absent = generate(&[]);
    let preset = generate(&["--emission-policy", "reviewable"]);
    let axes = generate(&["--inline-policy", "none", "--scalarize-policy", "never"]);
    assert_eq!(
        absent, preset,
        "no flag must be exactly the `reviewable` preset"
    );
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

/// `annotation(Inline = false)` refuses at every setting.
///
/// A refusal a compiler flag can override is not a refusal, so this is checked
/// at the setting that inlines the most, not only at the default.
#[test]
fn an_inline_false_annotation_refuses_every_policy() {
    for inline in ["none", "annotated", "cost-model", "all"] {
        let generated = generate(&["--inline-policy", inline]);
        assert!(
            defines_function(&generated, "keepAsCall"),
            "`Inline = false` must keep `keepAsCall` a call under `{inline}`:\n{generated}"
        );
    }
}

/// `annotation(Inline = true)` is honored from `annotated` upward, and `none`
/// answers to no annotation.
///
/// `none` is defined as "the emitted functions are the model's functions", and
/// an annotation that could delete one would make that property depend on the
/// model instead of on the setting.
#[test]
fn an_inline_true_annotation_is_honored_from_annotated_upward() {
    let untouched = generate(&["--inline-policy", "none"]);
    assert!(
        defines_function(&untouched, "squareTwice"),
        "`--inline-policy none` must keep every declared function:\n{untouched}"
    );
    for inline in ["annotated", "cost-model", "all"] {
        let generated = generate(&["--inline-policy", inline]);
        assert!(
            !defines_function(&generated, "squareTwice"),
            "`Inline = true` must be honored under `{inline}`:\n{generated}"
        );
    }
}

/// The cost model's one rule: a callee called from exactly one place is
/// substituted, and `annotated` leaves it alone.
///
/// `scaleOnce` carries no annotation and has one call site. There is nothing to
/// duplicate, so the rule has no threshold and nothing to tune: the body moves
/// rather than being copied, and the boundary it crossed once was buying
/// nothing.
#[test]
fn a_callee_with_one_call_site_is_substituted_by_the_cost_model() {
    let annotated = generate(&["--inline-policy", "annotated"]);
    assert!(
        defines_function(&annotated, "scaleOnce"),
        "`annotated` answers only to annotations, and `scaleOnce` carries none:\n{annotated}"
    );
    let cost_model = generate(&["--inline-policy", "cost-model"]);
    assert!(
        !defines_function(&cost_model, "scaleOnce"),
        "a callee with one call site must be substituted by the cost model:\n{cost_model}"
    );
}

/// Substituting every legal call loses no anchor on any line of arithmetic.
///
/// This is the traceability gate stated as something checkable. Substituting a
/// body moves arithmetic between functions; if the emitted statement then took
/// the CALL SITE's span, the callee's line would stop being named by the
/// artifact at all, and a whole source file could vanish from the trace legend
/// while the code it wrote is still executing. That is what this pins: every
/// line the structured artifact anchored is still anchored, and anchors may be
/// added by flattening, as they are.
///
/// The exception is exact and is the removed calls themselves. A call the
/// policy substituted has no emitted statement left to anchor, so the span of
/// the call EXPRESSION goes with it, while every line of the callee's body it
/// executed stays anchored. The removed function's preamble (its status and
/// output seeds, anchored at its declaration) goes the same way. Each such loss
/// is checked to be exactly that: a line of the model that writes one of the
/// substituted calls or declares one of the substituted functions, and nothing
/// else. Naming those call sites in the emitted artifact is the inline-chain
/// breadcrumb that D6 asks for and this projection does not yet emit, because
/// the GALEC statement carries a span and no annotation channel.
#[test]
fn substituting_every_legal_call_loses_no_anchor_on_a_line_of_arithmetic() {
    let structured = source_anchors(&generate(&["--inline-policy", "none"]));
    assert!(
        !structured.is_empty(),
        "the fixture must carry source anchors for this to mean anything"
    );
    let flattened = source_anchors(&generate(&["--inline-policy", "all"]));
    let unexplained = structured
        .difference(&flattened)
        .filter(|anchor| !writes_a_substituted_call(anchor))
        .collect::<Vec<_>>();
    assert!(
        unexplained.is_empty(),
        "substituting every legal call dropped anchors that no removed call \
         accounts for: {unexplained:#?}"
    );
    // A vacuous pass would be the failure this test cannot detect on its own,
    // so the accounted-for losses are required to exist.
    assert!(
        structured.difference(&flattened).next().is_some(),
        "the fixture must actually have calls that get substituted"
    );
}

/// Whether an anchor points at a line of the fixture that writes a call the
/// policy substitutes, or declares the substituted function.
///
/// The fixture is right here, so the model line an anchor names is read from
/// the source rather than guessed from the anchor's shape.
fn writes_a_substituted_call(anchor: &str) -> bool {
    const SUBSTITUTED: [&str; 2] = ["scaleOnce", "squareTwice"];
    let Some((_, position)) = anchor.split_once(&format!("{MODEL}.mo:")) else {
        return false;
    };
    let Some(line) = position
        .split(':')
        .next()
        .and_then(|number| number.parse::<usize>().ok())
        .and_then(|number| FIXTURE.lines().nth(number - 1))
    else {
        return false;
    };
    // A call site, or the substituted function's own declaration, which
    // anchors its preamble (status and output seeds) while it is emitted.
    SUBSTITUTED
        .iter()
        .any(|name| line.contains(&format!("{name}(")) || line.trim() == format!("function {name}"))
}

/// The `path:line:column-line:column` anchors a translation unit carries.
///
/// Read off the emitted comment rather than reconstructed, because the emitted
/// comment IS the traceability artifact: what a reviewer can act on is exactly
/// what these lines say.
fn source_anchors(generated: &str) -> std::collections::BTreeSet<String> {
    const OPEN: &str = "/* Modelica trace: ";
    const CLOSE: &str = " */";
    generated
        .lines()
        .filter_map(|line| line.trim().strip_prefix(OPEN))
        .filter_map(|rest| rest.strip_suffix(CLOSE))
        .map(|anchor| anchor.trim_end_matches(['.']).to_owned())
        .collect()
}

/// A container with no C has nothing a policy could shape, so `galec`, which
/// ships only the structured AlgorithmCode reference, refuses the flag rather
/// than silently dropping it: an artifact that does not match what was asked
/// for is worse than an error saying so.
#[test]
fn a_container_with_no_c_refuses_a_policy() {
    let dir = tempdir().expect("tempdir");
    let file = write_fixture(dir.path(), MODEL, FIXTURE);
    // The default is admitted: the container still compiles.
    let structured = run_compile_target_with(&file, "galec", &dir.path().join("out"), &[]);
    assert!(
        structured.status.success(),
        "`galec` must still compile under the default.\nstderr:\n{}",
        String::from_utf8_lossy(&structured.stderr)
    );
    let out_dir = dir.path().join("out-policy");
    let refused =
        run_compile_target_with(&file, "galec", &out_dir, &["--inline-policy", "cost-model"]);
    assert!(
        !refused.status.success(),
        "`galec` emits no C, so it must refuse a policy that only shapes C"
    );
    let stderr = super::cli_support::strip_ansi(&String::from_utf8_lossy(&refused.stderr))
        .replace('\n', " ");
    assert!(
        stderr.contains("AlgorithmCode") && stderr.contains("galec-production"),
        "the refusal must say what it protects and where a policy applies:\n{stderr}"
    );
    assert!(!out_dir.exists(), "a refused compile must write nothing");
}

/// An eFMI container ships its AlgorithmCode as the structured reference
/// whatever shaped its ProductionCode C (#483).
///
/// Under a policy, `galec-production` projects twice: the `.alg` and its
/// manifest are byte for byte the default container's, while the C is printed
/// from the policy-shaped projection, says so, and computes the same numbers.
#[test]
fn a_container_keeps_its_reference_structured_and_shapes_only_its_c() {
    let dir = tempdir().expect("tempdir");
    let file = write_fixture(dir.path(), MODEL, FIXTURE);
    let build = |name: &str, flags: &[&str]| {
        let out_dir = dir.path().join(name);
        let output = run_compile_target_with(&file, "galec-production", &out_dir, flags);
        assert!(
            output.status.success(),
            "`galec-production {flags:?}` failed.\nstderr:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
        out_dir.join(MODEL)
    };
    let structured = build("structured", &[]);
    let inlined = build("inlined", &["--inline-policy", "all"]);
    let read = |root: &Path, path: &str| {
        fs::read_to_string(root.join(path)).unwrap_or_else(|error| panic!("read {path}: {error}"))
    };
    // A manifest's `id` and `generationDateAndTime` name the build, not the
    // block, so they differ between any two builds.
    let per_build = |manifest: String| {
        manifest
            .split(' ')
            .filter(|attribute| {
                !attribute.starts_with("id=") && !attribute.starts_with("generationDateAndTime=")
            })
            .collect::<Vec<_>>()
            .join(" ")
    };
    let alg_path = format!("AlgorithmCode/{MODEL}.alg");
    assert_eq!(
        read(&inlined, &alg_path),
        read(&structured, &alg_path),
        "the .alg must stay the structured reference under a policy"
    );
    assert_eq!(
        per_build(read(&inlined, "AlgorithmCode/manifest.xml")),
        per_build(read(&structured, "AlgorithmCode/manifest.xml")),
        "the AlgorithmCode manifest must describe the structured reference under a policy"
    );
    let alg = read(&inlined, &alg_path);
    assert!(
        alg.contains("squareTwice"),
        "the reference keeps every function:\n{alg}"
    );
    let c_path = format!("ProductionCode/{MODEL}.c");
    let c = read(&inlined, &c_path);
    assert!(
        c.contains(DISCLOSURE) && c.contains("inline policy:    all"),
        "the ProductionCode C must disclose the policy it was printed under:\n{c}"
    );
    assert!(
        !defines_function(&c, "squareTwice") && defines_function(&c, "keepAsCall"),
        "`--inline-policy all` must collapse every legal call in the C only:\n{c}"
    );
    assert!(
        defines_function(&read(&structured, &c_path), "squareTwice"),
        "the default container's C keeps the call"
    );
    let production = |root: &Path| {
        let pc = root.join("ProductionCode");
        build_and_run(&pc, &pc.join(format!("{MODEL}.c")), &["galec-production"])
    };
    let observed = production(&inlined);
    assert!(!observed.is_empty(), "the driver must print the values");
    assert_eq!(
        observed,
        production(&structured),
        "the policy-shaped container C must compute exactly what the structured one does"
    );
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
