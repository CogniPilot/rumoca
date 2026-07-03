//! End-to-end CLI coverage for `rumoca compile --target galec`
//! (SPEC_0034 GAL-011/GAL-012).
//!
//! Invokes the real binary so the whole chain is exercised: CLI dispatch →
//! generic capability gate → GALEC projection facade → typed printer / XML
//! serializer → passthrough templates → files on disk. The emitted
//! `manifest.xml` is validated against the vendored Beta-1 Algorithm Code
//! XSD with a real `xmllint` run — `xmllint` is a CI-installed dependency,
//! so its absence is a hard failure, never a skip.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use tempfile::tempdir;

/// Fixed-sample discrete fixture: a parameter, a `pre()` state, an output,
/// and one `when sample(...)` clock — the shape the galec target exists for.
const DISCRETE_FIXTURE: &str = "\
model GalecCliSmoke
  constant Real samplePeriod = 0.1;
  parameter Real gain = 2.0;
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = gain * (pre(y) + 1.0);
  end when;
end GalecCliSmoke;
";

/// Continuous model the galec capability gate must reject (GAL-006).
const CONTINUOUS_FIXTURE: &str = "\
model GalecCliContinuous
  Real x(start = 1.0);
  parameter Real k = 2.0;
equation
  der(x) = -k * x;
end GalecCliContinuous;
";

fn write_fixture(dir: &Path, model: &str, source: &str) -> PathBuf {
    let file = dir.join(format!("{model}.mo"));
    fs::write(&file, source).expect("write fixture");
    file
}

fn run_compile_target_galec(file: &Path, out_dir: &Path) -> Output {
    Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("compile")
        .arg(file)
        .arg("--target")
        .arg("galec")
        .arg("-o")
        .arg(out_dir)
        .output()
        .expect("run rumoca compile --target galec")
}

/// The vendored eFMI Beta-1 Algorithm Code manifest schema (GAL-023).
fn vendored_algorithm_code_xsd() -> PathBuf {
    let xsd = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../rumoca-efmi/assets/efmi-schemas/AlgorithmCode/efmiAlgorithmCodeManifest.xsd");
    assert!(
        xsd.is_file(),
        "vendored Algorithm Code XSD missing at {}",
        xsd.display()
    );
    xsd
}

/// Drop ANSI SGR escapes so assertions see the plain diagnostic text
/// (miette colorizes stderr even when piped).
fn strip_ansi(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut chars = text.chars();
    while let Some(ch) = chars.next() {
        if ch != '\u{1b}' {
            out.push(ch);
            continue;
        }
        for escaped in chars.by_ref() {
            if escaped.is_ascii_alphabetic() {
                break;
            }
        }
    }
    out
}

#[test]
fn compile_target_galec_emits_alg_and_schema_valid_manifest() {
    let dir = tempdir().expect("tempdir");
    let file = write_fixture(dir.path(), "GalecCliSmoke", DISCRETE_FIXTURE);
    let out_dir = dir.path().join("out");

    let output = run_compile_target_galec(&file, &out_dir);
    assert!(
        output.status.success(),
        "`compile --target galec` failed (status {:?}).\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let alg = fs::read_to_string(out_dir.join("GalecCliSmoke.alg"))
        .expect("out dir must contain GalecCliSmoke.alg");
    assert!(
        alg.contains("method DoStep"),
        "GALEC block source must contain the DoStep method:\n{alg}"
    );

    let manifest = out_dir.join("manifest.xml");
    assert!(
        manifest.is_file(),
        "out dir must contain manifest.xml, found: {:?}",
        fs::read_dir(&out_dir)
            .map(|entries| entries
                .filter_map(Result::ok)
                .map(|entry| entry.file_name())
                .collect::<Vec<_>>())
            .unwrap_or_default()
    );
    // Hard requirement: a missing xmllint surfaces as EfmiError::XmllintUnavailable
    // and fails this expect — the test never skips schema validation (GAL-012).
    rumoca_efmi::validate_against_xsd(&manifest, &vendored_algorithm_code_xsd())
        .expect("emitted manifest.xml must validate against the vendored Algorithm Code XSD");

    // Cross-artifact checksum consistency (GAL-021): the manifest's `<File>`
    // checksum must be the SHA-1 of the sibling `.alg` file written by the
    // SAME CLI run. Both artifacts must be fed from one projection pass —
    // if the CLI ever re-projects per file again, any nondeterminism in
    // lowering/printing silently invalidates the eFMU; this pins the two
    // files against each other, not against a golden value.
    let alg_bytes = fs::read(out_dir.join("GalecCliSmoke.alg")).expect("read .alg bytes");
    let expected_checksum = rumoca_efmi::Sha1Hex::of_bytes(&alg_bytes);
    let manifest_text = fs::read_to_string(&manifest).expect("read manifest.xml");
    assert!(
        manifest_text.contains(&format!("checksum=\"{}\"", expected_checksum.as_str())),
        "manifest.xml must carry the SHA-1 of the .alg file written by the same run \
         (expected {}):\n{manifest_text}",
        expected_checksum.as_str()
    );
}

#[test]
fn compile_target_galec_rejects_continuous_model_with_capability_diagnostic() {
    let dir = tempdir().expect("tempdir");
    let file = write_fixture(dir.path(), "GalecCliContinuous", CONTINUOUS_FIXTURE);
    let out_dir = dir.path().join("out");

    let output = run_compile_target_galec(&file, &out_dir);
    assert!(
        !output.status.success(),
        "`compile --target galec` must fail for a continuous model.\nstdout:\n{}",
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

#[test]
fn targets_listing_includes_galec() {
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
        stdout.contains("galec"),
        "`rumoca targets` must list the galec target:\n{stdout}"
    );
}
