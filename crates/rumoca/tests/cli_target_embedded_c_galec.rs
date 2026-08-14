//! End-to-end CLI coverage for `rumoca compile --target embedded-c-galec`
//! (SPEC_0034 GAL-011/GAL-012/GAL-024).
//!
//! Invokes the real binary so the whole chain is exercised: CLI dispatch →
//! generic capability gate → GALEC projection → C mangler/printer → typed
//! template context → thin C templates. The emitted sources are then
//! compiled with `cc -Wall -Werror` and LINKED against a generated driver
//! (`-lm`), and the driver is executed to check the discrete dynamics —
//! the roadmap-mandated compile check plus a behavioral check on top
//! (GAL-012: generated C is compile-checked, never skip-and-mark-covered).
//! Like the `galec` suite's `xmllint` requirement, a missing `cc` is a
//! hard failure, never a skip.
//!
//! This target is the non-eFMI track of GAL-024: a GALEC-derived embedded
//! C export that must self-describe as NOT an eFMI Production Code
//! container — the tests pin that honesty in both the emitted header and
//! the CLI completion message.

use std::fs;
use std::path::Path;
use std::process::{Command, Output};

use tempfile::tempdir;

// The `galec_cli_support/` helpers are declared once by the umbrella binary
// that owns this file (see `suite_core.rs`), so the sibling suites share one
// copy instead of compiling the same file several times per binary.
use super::cc_support::assurance_c99_cc;
use super::cli_support::{run_compile_target, strip_ansi, write_fixture};

/// Fixed-sample discrete fixture: a parameter, a `pre()` state, an output,
/// and one `when sample(...)` clock — the shape the GALEC projection
/// admits (mirrors `cli_target_galec.rs`).
const DISCRETE_FIXTURE: &str = "\
model EmbeddedGalecSmoke
  constant Real samplePeriod = 0.1;
  parameter Real gain = 2.0;
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = gain * (pre(y) + 1.0);
  end when;
end EmbeddedGalecSmoke;
";

const MODEL: &str = "EmbeddedGalecSmoke";

const MIN_MAX_MODEL: &str = "EmbeddedGalecMinMax";

const MIN_MAX_FIXTURE: &str = "\
model EmbeddedGalecMinMax
  constant Real samplePeriod = 0.1;
  input Real u;
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = min(u, max(u, 0.0));
  end when;
end EmbeddedGalecMinMax;
";

/// Continuous model the capability gate must reject (GAL-006).
const CONTINUOUS_FIXTURE: &str = "\
model EmbeddedGalecContinuous
  Real x(start = 1.0);
  parameter Real k = 2.0;
equation
  der(x) = -k * x;
end EmbeddedGalecContinuous;
";

/// Driver exercising the generated block: startup, recalibrate, then three
/// dostep ticks of `y = gain * (pre(y) + 1)` with `gain = 2`, `y0 = 0`
/// (expected 2, 6, 14).
const DRIVER_MAIN: &str = "\
#include <stdio.h>
#include \"EmbeddedGalecSmoke.h\"

int main(void) {
    EmbeddedGalecSmokeState state;
    EmbeddedGalecSmoke_startup(&state);
    EmbeddedGalecSmoke_recalibrate(&state);
    for (int step = 0; step < 3; ++step) {
        EmbeddedGalecSmoke_dostep(&state);
        printf(\"%.1f\\n\", state.y);
    }
    return 0;
}
";

const MIN_MAX_DRIVER: &str = r#"
#include <math.h>
#include "EmbeddedGalecMinMax.c"

int main(void) {
    const float qnan = NAN;

    if (rumoca_galec_min(2.0f, 3.0f) != 2.0f
        || rumoca_galec_min(3.0f, 2.0f) != 2.0f
        || rumoca_galec_max(2.0f, 3.0f) != 3.0f
        || rumoca_galec_max(3.0f, 2.0f) != 3.0f) {
        return 1;
    }
    if (rumoca_galec_min(2.0f, 2.0f) != 2.0f
        || rumoca_galec_max(2.0f, 2.0f) != 2.0f) {
        return 2;
    }
    if (rumoca_galec_min(qnan, 2.0f) != 2.0f
        || rumoca_galec_max(qnan, 2.0f) != 2.0f) {
        return 3;
    }
    if (!isnan(rumoca_galec_min(2.0f, qnan))
        || !isnan(rumoca_galec_max(2.0f, qnan))) {
        return 4;
    }
    return 0;
}
"#;

fn run_compile_embedded_c_galec(file: &Path, out_dir: &Path) -> Output {
    run_compile_target(file, "embedded-c-galec", out_dir)
}

/// Compile the discrete fixture into `out_dir`, failing loudly on any CLI
/// error, and return the CLI stderr for message assertions.
fn build_sources(work_dir: &Path, out_dir: &Path) -> String {
    let file = write_fixture(work_dir, MODEL, DISCRETE_FIXTURE);
    let output = run_compile_embedded_c_galec(&file, out_dir);
    assert!(
        output.status.success(),
        "`compile --target embedded-c-galec` failed (status {:?}).\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8_lossy(&output.stderr).into_owned()
}

#[test]
fn real_min_max_use_order_sensitive_relational_nan_semantics() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let file = write_fixture(dir.path(), MIN_MAX_MODEL, MIN_MAX_FIXTURE);
    let output = run_compile_embedded_c_galec(&file, &out_dir);
    assert!(
        output.status.success(),
        "min/max fixture failed to compile: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let driver = out_dir.join("min_max.c");
    fs::write(&driver, MIN_MAX_DRIVER).expect("write min/max driver");
    let generated_source = fs::read_to_string(out_dir.join(format!("{MIN_MAX_MODEL}.c")))
        .expect("read generated C source");
    assert!(
        generated_source.contains("static inline float rumoca_galec_min")
            && generated_source.contains("static inline float rumoca_galec_max"),
        "reachable min/max helpers were not emitted"
    );
    let program = out_dir.join("min_max");
    // The driver includes the model unit textually to reach its `static inline`
    // helpers; that still makes this a link of a generated model unit, so the
    // shared kernel library goes on the command line like everywhere else.
    let program_kernels = out_dir.join(super::cc_support::GALEC_KERNEL_LIBRARY);
    let compile = assurance_c99_cc()
        .arg("-o")
        .arg(&program)
        .arg(&driver)
        .arg(&program_kernels)
        .arg("-lm")
        .output()
        .expect("run cc");
    assert!(
        compile.status.success(),
        "strict min/max probe compile failed.\nstderr:\n{}\nsource:\n{}",
        String::from_utf8_lossy(&compile.stderr),
        generated_source
    );

    let run = Command::new(&program).output().expect("run min/max probe");
    assert!(
        run.status.success(),
        "GALEC relational min/max probe exited with {:?}",
        run.status.code()
    );
}

/// The emitted sources compile under strict ISO C99 warnings, link against libm
/// with a real driver, and the executed block reproduces the discrete
/// dynamics tick for tick.
#[test]
fn emitted_c_compiles_links_and_reproduces_the_discrete_dynamics() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    build_sources(dir.path(), &out_dir);

    let header = out_dir.join(format!("{MODEL}.h"));
    let source = out_dir.join(format!("{MODEL}.c"));
    assert!(header.is_file(), "missing {}", header.display());
    assert!(source.is_file(), "missing {}", source.display());
    let source_text = fs::read_to_string(&source).expect("read generated C source");
    for unused_helper in [
        "static inline float rumoca_galec_sign",
        "static inline float rumoca_galec_min",
        "static inline float rumoca_galec_max",
        "static inline bool rumoca_galec_compare_",
        "static inline int32_t rumoca_galec_imin",
        "static inline int32_t rumoca_galec_imax",
        "static inline int32_t rumoca_galec_division_towards_zero",
    ] {
        assert!(
            !source_text.contains(unused_helper),
            "unreachable helper `{unused_helper}` was emitted"
        );
    }
    for generated in [&header, &source] {
        let bytes = fs::read(generated).expect("read generated C artifact");
        assert!(
            bytes.ends_with(b"\n"),
            "{} must end with a newline for strict C toolchains",
            generated.display()
        );
    }

    let driver = out_dir.join("main.c");
    fs::write(&driver, DRIVER_MAIN).expect("write driver");
    let program = out_dir.join("smoke");
    // Both emitted translation units: the model unit calls into the shared
    // kernel library beside it, so a link that omits the library is not the
    // link an integrator following `target.toml`'s completion message performs.
    let kernels = out_dir.join(super::cc_support::GALEC_KERNEL_LIBRARY);
    assert!(kernels.is_file(), "missing {}", kernels.display());
    let compile = assurance_c99_cc()
        .arg("-o")
        .arg(&program)
        .arg(&driver)
        .arg(&source)
        .arg(&kernels)
        .arg("-lm")
        .output()
        .expect("run cc");
    assert!(
        compile.status.success(),
        "strict cc -std=c99 compile failed.\nstderr:\n{}\nheader:\n{}\nsource:\n{}",
        String::from_utf8_lossy(&compile.stderr),
        fs::read_to_string(&header).unwrap_or_default(),
        fs::read_to_string(&source).unwrap_or_default()
    );

    let run = Command::new(&program)
        .output()
        .expect("run generated block");
    assert!(
        run.status.success(),
        "generated block driver exited with {:?}",
        run.status.code()
    );
    assert_eq!(
        // Normalize CRLF: Windows text-mode stdio emits `\n` as `\r\n`.
        String::from_utf8_lossy(&run.stdout).replace("\r\n", "\n"),
        "2.0\n6.0\n14.0\n",
        "three dostep ticks of y := gain * (previous(y) + 1) with gain = 2"
    );
}

/// GAL-024/GAL-029 honesty: target scope and assurance claims stay explicit.
#[test]
fn export_self_describes_as_not_an_efmi_production_code_container() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let stderr = build_sources(dir.path(), &out_dir);

    let header = fs::read_to_string(out_dir.join(format!("{MODEL}.h"))).expect("read header");
    assert!(
        header.contains("NOT an eFMI Production Code container"),
        "header must carry the GAL-024 self-description:\n{header}"
    );
    for api_name in [
        "EmbeddedGalecSmokeState",
        "EmbeddedGalecSmoke_startup",
        "EmbeddedGalecSmoke_recalibrate",
        "EmbeddedGalecSmoke_dostep",
    ] {
        assert!(
            header.contains(api_name),
            "header must expose {api_name}:\n{header}"
        );
    }
    assert!(!header.contains("#  define EFMI_"), "{header}");
    assert!(header.contains("MISRA compliance and DO-178C compliance are not claimed"));
    assert!(
        strip_ansi(&stderr).contains("NOT an eFMI Production Code"),
        "completion message must carry the GAL-024 self-description, got:\n{stderr}"
    );
    assert!(
        strip_ansi(&stderr).contains("MISRA compliance and DO-178C compliance are not claimed")
    );
}

#[test]
fn continuous_model_is_rejected_by_the_capability_gate() {
    let dir = tempdir().expect("tempdir");
    let file = write_fixture(dir.path(), "EmbeddedGalecContinuous", CONTINUOUS_FIXTURE);
    let out_dir = dir.path().join("out");

    let output = run_compile_embedded_c_galec(&file, &out_dir);
    assert!(
        !output.status.success(),
        "`compile --target embedded-c-galec` must fail for a continuous model.\nstdout:\n{}",
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

/// A return-carrying function whose output declares a default (MLS §12.4.4),
/// driven from a fixed-sample `when` so the GALEC capability gate admits it.
///
/// The non-returning path stores nothing into `y`: it keeps the declaration
/// binding, which is exactly the path 55ea493c found returning `0` because the
/// generated return seed was emitted on top of the default.
const DEFAULTED_OUTPUT_MODEL: &str = "EmbeddedGalecDefaultedOutput";

/// `decoy` comes first on purpose, and has the same body shape: it satisfies
/// every assertion below on its own. A body reader that anchors on the
/// forward-declaration block instead of the definition lands on the decoy and
/// passes while never looking at `defaultedReturn` — the tautology
/// [`the_body_reader_selects_the_definition_not_a_neighbour`] pins, and which
/// review demonstrated against the first version of this test.
const DEFAULTED_OUTPUT_FIXTURE: &str = "\
function decoy
  input Real x;
  output Real h = 7.0;
algorithm
  if x > 0 then
    h := x;
    return;
  end if;
end decoy;

function defaultedReturn
  input Real x;
  output Real y = 7.0;
algorithm
  if x > 0 then
    y := x;
    return;
  end if;
end defaultedReturn;

model EmbeddedGalecDefaultedOutput
  constant Real samplePeriod = 0.1;
  input Real u;
  discrete output Real y(start = 0.0);
  discrete output Real shadow(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    shadow = decoy(u);
    y = defaultedReturn(u);
  end when;
end EmbeddedGalecDefaultedOutput;
";

/// End-to-end GALEC-production evidence for the §12.4.4 rule: the C the
/// shipped CLI writes must initialize a defaulted function output with its
/// declaration binding, and must not follow that with a zero store.
///
/// `crates/rumoca-phase-codegen/tests/galec_c_output_defaults.rs` pins the same
/// property over a hand-built GALEC block, which is blind to the lowering; the
/// `function_return_checked` regressions pin the lowering and the simulated
/// values, which are blind to the C. This test is the one that sees the whole
/// chain: if the return seed comes back — in the DAE, in the GALEC projection,
/// or in the C printer — the emitted `defaultedReturn` body gains a second
/// constant store into its output and this fails with that body attached.
#[test]
fn a_defaulted_function_output_is_not_zeroed_in_the_emitted_c() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let file = write_fixture(dir.path(), DEFAULTED_OUTPUT_MODEL, DEFAULTED_OUTPUT_FIXTURE);
    let output = run_compile_embedded_c_galec(&file, &out_dir);
    assert!(
        output.status.success(),
        "defaulted-output fixture failed to compile.\nstderr:\n{}",
        strip_ansi(&String::from_utf8_lossy(&output.stderr))
    );

    let source = fs::read_to_string(out_dir.join(format!("{DEFAULTED_OUTPUT_MODEL}.c")))
        .expect("read generated C source");
    let body = emitted_definition_body(&source, "defaultedReturn", "embedded-c-galec");
    let constants = constant_stores(body);

    // Locate the output by the value it is declared with, not by guessing how
    // the C printer spells the parameter.
    let bindings = constants
        .iter()
        .filter(|store| store.is_value(7.0))
        .collect::<Vec<_>>();
    assert_eq!(
        bindings.len(),
        1,
        "the emitted body must store the declared 7.0 into its output exactly \
         once; losing it means the declaration binding is no longer the live \
         seed.\nEmitted body:\n{body}"
    );
    let declared = bindings[0];
    let overwrites = constants
        .iter()
        .filter(|store| store.target == declared.target && !store.is_value(7.0))
        .map(|store| store.value.as_str())
        .collect::<Vec<_>>();
    assert!(
        overwrites.is_empty(),
        "the defaulted output `{}` is stored a second constant {overwrites:?}; a \
         dead-value store over a live §12.4.4 default is exactly the defect \
         55ea493c removed.\nEmitted body:\n{body}",
        declared.target
    );
    let first_store = body
        .find(&format!("{} = ", declared.target))
        .expect("the target was just read out of this body");
    let declaration_binding = body
        .find(&format!("{} = {};", declared.target, declared.value))
        .expect("the declaration binding was just read out of this body");
    assert_eq!(
        declaration_binding, first_store,
        "the declaration binding must be the FIRST store into `{}`, so nothing \
         can seed the output ahead of it.\nEmitted body:\n{body}",
        declared.target
    );
}

/// The assertions above are only about `defaultedReturn` if the body reader
/// actually returns `defaultedReturn`'s body. It did not, in the first version
/// of this test: anchoring on the first `defaultedReturn(` landed on the
/// forward declaration, and the following `{` opened the neighbouring function
/// — whose body has the same shape and satisfies every assertion. The test
/// passed with the clobber restored.
///
/// This pins the reader against exactly that: prove the hazard is real (the
/// unit does contain a prototype ahead of the definition, and does define the
/// same-shaped `decoy` first), then require the returned body to store
/// `defaultedReturn`'s own output and never `decoy`'s.
#[test]
fn the_body_reader_selects_the_definition_not_a_neighbour() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let file = write_fixture(dir.path(), DEFAULTED_OUTPUT_MODEL, DEFAULTED_OUTPUT_FIXTURE);
    let output = run_compile_embedded_c_galec(&file, &out_dir);
    assert!(
        output.status.success(),
        "defaulted-output fixture failed to compile.\nstderr:\n{}",
        strip_ansi(&String::from_utf8_lossy(&output.stderr))
    );
    let source = fs::read_to_string(out_dir.join(format!("{DEFAULTED_OUTPUT_MODEL}.c")))
        .expect("read generated C source");

    let definition = definition_brace(&source, "defaultedReturn")
        .unwrap_or_else(|| panic!("the unit must define defaultedReturn:\n{source}"));
    let decoy = definition_brace(&source, "decoy")
        .unwrap_or_else(|| panic!("the unit must define decoy:\n{source}"));
    assert!(
        source
            .find("defaultedReturn(")
            .expect("the unit names defaultedReturn")
            < definition,
        "this test is only meaningful while the printer emits a forward \
         declaration ahead of the definition. If that changed, re-derive the \
         hazard rather than deleting the test.\n{source}"
    );
    assert!(
        decoy < definition,
        "the decoy must be defined AHEAD of defaultedReturn, or it cannot stand \
         in for it.\n{source}"
    );

    let body = emitted_definition_body(&source, "defaultedReturn", "embedded-c-galec");
    let decoy_body = emitted_definition_body(&source, "decoy", "embedded-c-galec");
    assert!(
        body.contains("->y = ") && !body.contains("->h = "),
        "the reader must return `defaultedReturn`'s own body, which stores its \
         output `y` and never `decoy`'s `h`.\nReturned body:\n{body}"
    );
    assert!(
        decoy_body.contains("->h = ") && !decoy_body.contains("->y = "),
        "reading `decoy` must return `decoy`'s body.\nReturned body:\n{decoy_body}"
    );
}

// ---------------------------------------------------------------------------
// Emitted-C readers.
//
// TWIN: everything between this banner and the closing one is duplicated
// verbatim across two files, which live in different crates:
//
//   * crates/rumoca-phase-codegen/tests/galec_c_output_defaults.rs
//   * crates/rumoca/tests/cli_target_embedded_c_galec.rs
//
// A test-only reader has no home either crate can import without adding it to
// a shipped crate's public surface, so the copies are kept BYTE-identical
// instead — including this banner, so the two sections can simply be diffed.
// Change one, change the other.
// ---------------------------------------------------------------------------

/// Byte offset of the `{` opening the emitted C **definition** of `name`.
///
/// The GALEC C printer emits a forward-declaration block ahead of the
/// definition block, so the *first* occurrence of `name(` is the prototype and
/// the next `{` opens whichever function is defined first in the unit. A reader
/// anchored there returns some other function's body, and every assertion made
/// on it passes without ever looking at `name` — including with the clobber
/// these tests exist to catch fully restored. So: walk every occurrence,
/// balance its parameter list, and accept only the one whose `)` is followed by
/// `{` rather than `;`. Call sites (`name(a, b);`) fall out by the same rule.
fn definition_brace(source: &str, name: &str) -> Option<usize> {
    let needle = format!("{name}(");
    let mut cursor = 0;
    while let Some(offset) = source[cursor..].find(&needle) {
        let open = cursor + offset + needle.len() - 1;
        cursor = open + 1;
        let Some(close) = matching_parenthesis(source, open) else {
            continue;
        };
        let after = &source[close + 1..];
        if !after.trim_start().starts_with('{') {
            continue;
        }
        return Some(close + 1 + after.find('{').expect("the tail starts with a brace"));
    }
    None
}

/// The body of the emitted C definition of `name`. A missing definition is a
/// hard failure — an assertion that silently matched nothing would be worse
/// than no assertion.
fn emitted_definition_body<'a>(source: &'a str, name: &str, label: &str) -> &'a str {
    let brace = definition_brace(source, name)
        .unwrap_or_else(|| panic!("{label}: the emitted unit must define `{name}`:\n{source}"));
    let body = &source[brace + 1..];
    let end = body.find("\n}").unwrap_or_else(|| {
        panic!("{label}: the `{name}` definition must close its body:\n{source}")
    });
    &body[..end]
}

/// The index of the `)` that closes the `(` at `open`.
fn matching_parenthesis(source: &str, open: usize) -> Option<usize> {
    let mut depth = 0usize;
    for (offset, character) in source[open..].char_indices() {
        match character {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    return Some(open + offset);
                }
            }
            _ => {}
        }
    }
    None
}

/// One `<lvalue> = <numeric literal>;` store found in an emitted body.
struct ConstantStore {
    target: String,
    value: String,
}

impl ConstantStore {
    fn is_value(&self, expected: f64) -> bool {
        self.value
            .trim_end_matches('f')
            .parse::<f64>()
            .is_ok_and(|value| (value - expected).abs() < 1.0e-9)
    }
}

/// Every constant store in an emitted body, keyed by lvalue.
///
/// Matched on the assignment form rather than on one expected spelling, so a
/// zero written `0`, `0.0f`, `-0.0f` or `0.0e0f` is caught the same way, and so
/// the test never depends on how the C printer spells the output.
fn constant_stores(body: &str) -> Vec<ConstantStore> {
    body.lines()
        .filter_map(|line| line.trim().strip_suffix(';'))
        .filter_map(|statement| statement.split_once(" = "))
        .filter(|(_, value)| {
            !value.is_empty()
                && value
                    .trim_start_matches('-')
                    .trim_end_matches('f')
                    .chars()
                    .all(|character| {
                        character.is_ascii_digit() || matches!(character, '.' | 'e' | '+')
                    })
        })
        .map(|(target, value)| ConstantStore {
            target: target.to_owned(),
            value: value.to_owned(),
        })
        .collect()
}

// ---------------------------------------------------------------------------
// End of the twinned section.
// ---------------------------------------------------------------------------

#[test]
fn targets_listing_includes_embedded_c_galec() {
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
        stdout.contains("embedded-c-galec"),
        "`rumoca targets` must list the embedded-c-galec target:\n{stdout}"
    );
}

// ---------------------------------------------------------------------------
// Branch multi-write erasure (MLS §11.5 + §11.2.1).
//
// A branch of a function conditional is an ordinary algorithm section, so
// several element writes to one array run in sequence and each one lands.
// DAE keeps that sequence as one compact tensor SSA value — each write wraps
// the aggregate the previous one produced — and the GALEC projection has to
// unwrap the whole chain back into statements. Unwrapping only the outermost
// level kept a branch's *last* write and silently dropped every earlier one,
// which is wrong code with no diagnostic: the dropped outputs simply read as
// whatever the dead entry seed left behind.
//
// The three fixtures below pin the three shapes that matter: every write of a
// branch survives, branches that write *different* element subsets each keep
// their own writes without disturbing elements neither names, and the real
// quaternion-to-Euler conversion this was found in computes all three angles.
// ---------------------------------------------------------------------------

const BRANCH_WRITES_MODEL: &str = "EmbeddedGalecBranchWrites";

/// Three element writes per branch, in an order that differs between the
/// branches and is not the element order, with a distinct constant per
/// (branch, element) so a dropped write can never be mistaken for a kept one.
const BRANCH_WRITES_FIXTURE: &str = "\
model EmbeddedGalecBranchWrites
  function triple
    input Real u;
    output Real e[3];
  algorithm
    if u > 0.5 then
      e[2] := 20.0;
      e[3] := 30.0;
      e[1] := 10.0;
    else
      e[1] := 11.0;
      e[2] := 22.0;
      e[3] := 33.0;
    end if;
  end triple;

  constant Real samplePeriod = 0.1;
  input Real u;
  discrete output Real y1(start = 0.0);
  discrete output Real y2(start = 0.0);
  discrete output Real y3(start = 0.0);
protected
  discrete Real e[3];
equation
  when sample(0.0, samplePeriod) then
    e = triple(u);
    y1 = e[1];
    y2 = e[2];
    y3 = e[3];
  end when;
end EmbeddedGalecBranchWrites;
";

const BRANCH_WRITES_DRIVER: &str = "\
#include <stdio.h>
#include \"EmbeddedGalecBranchWrites.h\"

static void probe(float u) {
    EmbeddedGalecBranchWritesState state;
    EmbeddedGalecBranchWrites_startup(&state);
    EmbeddedGalecBranchWrites_recalibrate(&state);
    state.u = u;
    EmbeddedGalecBranchWrites_dostep(&state);
    printf(\"%.1f %.1f %.1f\\n\", (double)state.y1, (double)state.y2, (double)state.y3);
}

int main(void) {
    probe(1.0f);
    probe(0.0f);
    return 0;
}
";

const PARTIAL_BRANCH_MODEL: &str = "EmbeddedGalecPartialBranchWrites";

/// Branches whose write sets *overlap partially*: the `then` branch writes
/// elements 1 and 2, the `else` branch writes 2 and 3. Element 3 on one path
/// and element 1 on the other are named by no branch write at all and must
/// keep the value the algorithm gave them before the conditional. This is the
/// shape a merge that reduced a branch to a single owning write would still
/// get wrong even after the last-write-only bug was gone.
const PARTIAL_BRANCH_FIXTURE: &str = "\
model EmbeddedGalecPartialBranchWrites
  function subsetWrites
    input Real u;
    output Real e[3];
  algorithm
    e[1] := 1.0;
    e[2] := 2.0;
    e[3] := 3.0;
    if u > 0.5 then
      e[1] := 10.0;
      e[2] := 20.0;
    else
      e[2] := 22.0;
      e[3] := 33.0;
    end if;
  end subsetWrites;

  constant Real samplePeriod = 0.1;
  input Real u;
  discrete output Real y1(start = 0.0);
  discrete output Real y2(start = 0.0);
  discrete output Real y3(start = 0.0);
protected
  discrete Real e[3];
equation
  when sample(0.0, samplePeriod) then
    e = subsetWrites(u);
    y1 = e[1];
    y2 = e[2];
    y3 = e[3];
  end when;
end EmbeddedGalecPartialBranchWrites;
";

const PARTIAL_BRANCH_DRIVER: &str = "\
#include <stdio.h>
#include \"EmbeddedGalecPartialBranchWrites.h\"

static void probe(float u) {
    EmbeddedGalecPartialBranchWritesState state;
    EmbeddedGalecPartialBranchWrites_startup(&state);
    EmbeddedGalecPartialBranchWrites_recalibrate(&state);
    state.u = u;
    EmbeddedGalecPartialBranchWrites_dostep(&state);
    printf(\"%.1f %.1f %.1f\\n\", (double)state.y1, (double)state.y2, (double)state.y3);
}

int main(void) {
    probe(1.0f);
    probe(0.0f);
    return 0;
}
";

const FROM_QUAT_MODEL: &str = "EmbeddedGalecFromQuat";

/// `LieGroups.SO3.EulerB321.from_Quat` verbatim in shape: the gimbal-lock
/// branch and the ordinary branch each assign all three Euler components, and
/// the component each branch writes *last* differs. Dropping all but the last
/// write per branch zeroes pitch on both paths and yaw on the ordinary one.
const FROM_QUAT_FIXTURE: &str = "\
model EmbeddedGalecFromQuat
  function fromQuat
    input Real q[4];
    output Real euler[3];
  protected
    Real a, b, c, d;
    Real sinp;
  algorithm
    a := q[1];
    b := q[2];
    c := q[3];
    d := q[4];
    sinp := 2.0*(a*c - d*b);
    sinp := min(max(sinp, -1.0), 1.0);
    if sinp * sinp > 0.9999 * 0.9999 then
      euler[2] := asin(sinp);
      euler[3] := 0.0;
      euler[1] := atan2(2.0*(b*c + a*d), 1.0 - 2.0*(c*c + d*d));
    else
      euler[1] := atan2(2.0*(a*d + b*c), 1.0 - 2.0*(c*c + d*d));
      euler[2] := asin(sinp);
      euler[3] := atan2(2.0*(a*b + c*d), 1.0 - 2.0*(b*b + c*c));
    end if;
  end fromQuat;

  constant Real samplePeriod = 0.1;
  input Real q[4];
  discrete output Real yaw(start = 0.0);
  discrete output Real pitch(start = 0.0);
  discrete output Real roll(start = 0.0);
protected
  discrete Real e[3];
equation
  when sample(0.0, samplePeriod) then
    e = fromQuat(q);
    yaw = e[1];
    pitch = e[2];
    roll = e[3];
  end when;
end EmbeddedGalecFromQuat;
";

/// Drives the emitted block with unit quaternions for a pure 0.5 rad rotation
/// about each axis, then one at exactly +90 degrees pitch to enter the
/// gimbal-lock branch. `%.2f` is the tolerance: the block computes in `float`,
/// and `asin` at the clamp is a worst case for it, so the printed value is
/// compared at a precision `float` carries on any conforming libm.
const FROM_QUAT_DRIVER: &str = "\
#include <stdio.h>
#include <math.h>
#include \"EmbeddedGalecFromQuat.h\"

static void probe(float w, float x, float y, float z) {
    EmbeddedGalecFromQuatState state;
    EmbeddedGalecFromQuat_startup(&state);
    EmbeddedGalecFromQuat_recalibrate(&state);
    state.q[0] = w;
    state.q[1] = x;
    state.q[2] = y;
    state.q[3] = z;
    EmbeddedGalecFromQuat_dostep(&state);
    printf(\"%.2f %.2f %.2f\\n\",
           (double)state.yaw, (double)state.pitch, (double)state.roll);
}

int main(void) {
    const float h = 0.25f;             /* half of 0.5 rad */
    const float q = 0.78539816f;       /* half of pi/2   */
    probe(cosf(h), 0.0f, sinf(h), 0.0f);
    probe(cosf(h), 0.0f, 0.0f, sinf(h));
    probe(cosf(h), sinf(h), 0.0f, 0.0f);
    probe(cosf(q), 0.0f, sinf(q), 0.0f);
    return 0;
}
";

/// Compile `fixture` to embedded C, build it with `driver`, run it, and return
/// the generated C source together with the program's stdout.
fn build_and_run(model: &str, fixture: &str, driver: &str) -> (String, String) {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let file = write_fixture(dir.path(), model, fixture);
    let output = run_compile_embedded_c_galec(&file, &out_dir);
    assert!(
        output.status.success(),
        "`compile --target embedded-c-galec` failed for {model}.\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let source = out_dir.join(format!("{model}.c"));
    let generated = fs::read_to_string(&source).expect("read generated C source");
    let kernels = out_dir.join(super::cc_support::GALEC_KERNEL_LIBRARY);
    assert!(kernels.is_file(), "missing {}", kernels.display());
    let driver_path = out_dir.join("main.c");
    fs::write(&driver_path, driver).expect("write driver");
    let program = out_dir.join("probe");
    let compile = assurance_c99_cc()
        .arg("-o")
        .arg(&program)
        .arg(&driver_path)
        .arg(&source)
        .arg(&kernels)
        .arg("-lm")
        .output()
        .expect("run cc");
    assert!(
        compile.status.success(),
        "strict cc -std=c99 compile failed for {model}.\nstderr:\n{}\nsource:\n{generated}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let run = Command::new(&program).output().expect("run probe");
    assert!(
        run.status.success(),
        "{model} probe exited with {:?}",
        run.status.code()
    );
    let stdout = String::from_utf8_lossy(&run.stdout).replace("\r\n", "\n");
    (generated, stdout)
}

/// Every element write of a conditional branch reaches the emitted code.
#[test]
fn every_element_write_of_a_function_branch_survives_projection() {
    let (generated, stdout) = build_and_run(
        BRANCH_WRITES_MODEL,
        BRANCH_WRITES_FIXTURE,
        BRANCH_WRITES_DRIVER,
    );
    let body = emitted_definition_body(&generated, "triple", BRANCH_WRITES_MODEL);
    let stores = constant_stores(body);
    for expected in [10.0, 20.0, 30.0, 11.0, 22.0, 33.0] {
        assert!(
            stores.iter().any(|store| store.is_value(expected)),
            "the {expected} write of `triple` was dropped from the emitted body:\n{body}"
        );
    }
    assert_eq!(
        stdout, "10.0 20.0 30.0\n11.0 22.0 33.0\n",
        "each branch of `triple` must leave all three of its writes behind"
    );
}

/// Branches that write different element subsets keep both their own writes
/// and the pre-conditional value of every element they do not name.
#[test]
fn partially_overlapping_branch_write_sets_keep_untouched_elements() {
    let (_, stdout) = build_and_run(
        PARTIAL_BRANCH_MODEL,
        PARTIAL_BRANCH_FIXTURE,
        PARTIAL_BRANCH_DRIVER,
    );
    assert_eq!(
        stdout, "10.0 20.0 3.0\n1.0 22.0 33.0\n",
        "an element no branch write names must keep its pre-conditional value"
    );
}

/// The quaternion-to-Euler conversion the erasure was found in produces all
/// three angles, on the ordinary path and in gimbal lock.
#[test]
fn from_quat_shaped_conversion_computes_yaw_pitch_and_roll() {
    let (generated, stdout) = build_and_run(FROM_QUAT_MODEL, FROM_QUAT_FIXTURE, FROM_QUAT_DRIVER);
    let body = emitted_definition_body(&generated, "fromQuat", FROM_QUAT_MODEL);
    for branch in body.split("} else {") {
        for component in ["euler[0]", "euler[1]", "euler[2]"] {
            assert!(
                branch.contains(&format!("{component} = ")),
                "`fromQuat` must assign {component} on this path:\n{branch}"
            );
        }
    }
    assert_eq!(
        stdout, "0.00 0.50 0.00\n0.50 0.00 0.00\n0.00 0.00 0.50\n0.00 1.57 0.00\n",
        "pitch, yaw and roll must each be recovered, gimbal-lock branch included"
    );
}
