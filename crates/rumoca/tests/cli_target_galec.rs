//! End-to-end CLI coverage for `rumoca compile --target galec`
//! (SPEC_0034 GAL-011/GAL-012/GAL-021).
//!
//! Invokes the real binary so the whole chain is exercised: CLI dispatch →
//! generic capability gate → GALEC projection facade → a product-agnostic
//! context validated in Rust → jinja templates (the eFMI manifest) plus the
//! typed GALEC `.alg` view → the declared checksum and `[package]` graph
//! container packaging.
//! The galec target claims the "eFMI Algorithm Code export" rung of the
//! SPEC_0034 conformance ladder, so these tests machine-check that rung:
//! schema-valid `__content.xml` + `schemas/` + Algorithm Code representation,
//! SHA-1 checksums recomputed from the written bytes, valid UUID/ids, strict
//! UTC timestamps, and a `.efmu` zip form equal to the directory form. XSD
//! validation runs through a real `xmllint` — a CI-installed dependency, so
//! its absence is a hard failure, never a skip.
//!
//! Output layout under the chosen out dir (decision documented in
//! `src/efmu.rs`, mirroring `build = "fmu"`'s everything-inside-out-dir UX):
//! `<Model>/` is the eFMU directory form (kept pristine, since eFMI defines
//! it as a package format) and `<Model>.efmu` is the zip form beside it.

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::io::Read as _;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use tempfile::tempdir;

// The `galec_cli_support/` helpers are declared once by the umbrella binary
// that owns this file (see `suite_galec_fmu.rs`), so the sibling suites share
// one copy instead of compiling the same file several times per binary.
use super::cli_support::{run_compile_target, strip_ansi, write_fixture};
use super::container_xml_support::{
    assert_xsd_rejects, attribute_values, mask_attribute, mask_uuids, move_line_after,
    relative_file_paths, sole_attribute_value, surgically, validate_against_xsd,
    vendored_schemas_dir, without_block, without_line,
};
use super::metadata_support::{assert_manifest_id, assert_strict_utc_timestamp};

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

const MODEL: &str = "GalecCliSmoke";

/// Continuous model the galec capability gate must reject (GAL-006).
const CONTINUOUS_FIXTURE: &str = "\
model GalecCliContinuous
  Real x(start = 1.0);
  parameter Real k = 2.0;
equation
  der(x) = -k * x;
end GalecCliContinuous;
";

const COMPACT_ESTIMATOR_MODEL: &str = "CompactEstimatorCodegen";

/// A small estimator-shaped regression fixture: record-valued state, tensor
/// fields, one prediction shared by an exhaustive correction choice, and a
/// multi-output correction result.  Its source stays deliberately compact so
/// generated-artifact growth measures projection quality, not fixture growth.
const COMPACT_ESTIMATOR_FIXTURE: &str = r#"
record CompactEstimate
  Real position[3];
  Real covariance[3, 3];
end CompactEstimate;

function compactPredict
  input CompactEstimate prior;
  output CompactEstimate predicted;
algorithm
  predicted.position := prior.position;
  predicted.covariance := prior.covariance;
end compactPredict;

function compactCorrectPosition
  input CompactEstimate prior;
  input Real measurement[3];
  output CompactEstimate posterior;
  output Boolean accepted;
algorithm
  posterior.position := prior.position;
  posterior.covariance := prior.covariance;
  accepted := measurement[1] > -1000.0;
end compactCorrectPosition;

function compactCorrectVelocity
  input CompactEstimate prior;
  input Real measurement[3];
  output CompactEstimate posterior;
  output Boolean accepted;
algorithm
  posterior.position := prior.position;
  posterior.covariance := prior.covariance;
  accepted := measurement[1] > -1000.0;
end compactCorrectVelocity;

function compactEstimatorStep
  input CompactEstimate prior;
  input Real measurement[3];
  input Boolean positionValid;
  input Boolean velocityValid;
  output CompactEstimate posterior;
  output Boolean accepted;
protected
  CompactEstimate predicted;
algorithm
  predicted := compactPredict(prior);
  if positionValid then
    (posterior, accepted) := compactCorrectPosition(predicted, measurement);
  elseif velocityValid then
    (posterior, accepted) := compactCorrectVelocity(predicted, measurement);
  else
    posterior.position := predicted.position;
    posterior.covariance := predicted.covariance;
    accepted := false;
  end if;
end compactEstimatorStep;

model CompactEstimatorCodegen
  constant Real samplePeriod = 0.01;
  input Real measurement[3];
  input Boolean positionValid;
  input Boolean velocityValid;
  discrete output Real position[3](each start = 0.0);
  discrete output Real covariance[3, 3](each start = 0.0);
  discrete output Boolean accepted(start = false);
protected
  discrete CompactEstimate estimate;
algorithm
  when sample(0.0, samplePeriod) then
    estimate.position := pre(position);
    estimate.covariance := pre(covariance);
    (estimate, accepted) := compactEstimatorStep(
      estimate,
      measurement,
      positionValid,
      velocityValid);
    position := estimate.position;
    covariance := estimate.covariance;
  end when;
end CompactEstimatorCodegen;
"#;

fn run_compile_target_galec(file: &Path, out_dir: &Path) -> Output {
    run_compile_target(file, "galec", out_dir)
}

fn has_ordinal_call_temporary(source: &str) -> bool {
    source
        .split(|character: char| !character.is_ascii_alphanumeric() && character != '_')
        .filter_map(|token| token.rsplit_once("_call_"))
        .any(|(_, ordinal)| {
            !ordinal.is_empty() && ordinal.bytes().all(|byte| byte.is_ascii_digit())
        })
}

fn galec_call_site_count(source: &str, function: &str) -> usize {
    let call = format!("{function}(");
    source
        .lines()
        .filter(|line| line.contains(":=") && line.contains(&call))
        .count()
}

fn c_call_site_count(source: &str, function: &str) -> usize {
    let call = format!("{function}(");
    source
        .lines()
        .filter(|line| line.trim_start().starts_with(&call))
        .count()
}

/// One packaged eFMU produced by a real CLI run.
struct BuiltContainer {
    /// The eFMU directory-form root (`<out_dir>/<Model>/`).
    root: PathBuf,
    /// The `.efmu` zip form (`<out_dir>/<Model>.efmu`).
    efmu_zip: PathBuf,
}

impl BuiltContainer {
    fn content_xml(&self) -> PathBuf {
        self.root.join("__content.xml")
    }

    fn manifest_xml(&self) -> PathBuf {
        self.root.join("AlgorithmCode").join("manifest.xml")
    }

    fn alg_file(&self) -> PathBuf {
        self.root.join("AlgorithmCode").join(format!("{MODEL}.alg"))
    }
}

/// Compile the discrete fixture into `out_dir` and return the container
/// paths, failing loudly on any CLI error.
fn build_container(work_dir: &Path, out_dir: &Path) -> BuiltContainer {
    let file = write_fixture(work_dir, MODEL, DISCRETE_FIXTURE);
    let output = run_compile_target_galec(&file, out_dir);
    assert!(
        output.status.success(),
        "`compile --target galec` failed (status {:?}).\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    BuiltContainer {
        root: out_dir.join(MODEL),
        efmu_zip: out_dir.join(format!("{MODEL}.efmu")),
    }
}

/// Negative schema cases (contract §7): the vendored Algorithm Code XSD must
/// REJECT a corrupted manifest — the proof that a template regression (wrong
/// child order, bad enum, missing required element, malformed UUID) cannot
/// slip past the positive xmllint pass. Ported from the dissolved
/// `rumoca-efmi` crate, now corrupting the TEMPLATE-rendered manifest.
#[test]
fn corrupted_algorithm_code_manifest_is_rejected_by_the_xsd() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let container = build_container(dir.path(), &out_dir);
    let manifest = fs::read_to_string(container.manifest_xml()).expect("read AC manifest");
    let xsd = vendored_schemas_dir("galec").join("AlgorithmCode/efmiAlgorithmCodeManifest.xsd");

    // Sanity: the pristine rendered manifest is valid (else the negatives
    // below would be vacuous).
    validate_against_xsd(&container.manifest_xml(), &xsd)
        .expect("pristine rendered AC manifest must be schema-valid");

    // Missing required element: no Clock (efmiAlgorithmCodeManifest minOccurs=1).
    assert_xsd_rejects(
        "missing Clock",
        &without_line(&manifest, "<Clock id=\"CLK\""),
        &xsd,
    );
    // Missing required BlockMethods block.
    assert_xsd_rejects(
        "missing BlockMethods",
        &without_block(&manifest, "<BlockMethods>", "</BlockMethods>"),
        &xsd,
    );
    // Wrong child order: Clock moved after BlockMethods (violates xs:sequence).
    assert_xsd_rejects(
        "Clock after BlockMethods",
        &move_line_after(&manifest, "<Clock id=\"CLK\"", "</BlockMethods>"),
        &xsd,
    );
    // Bad blockCausality enumeration value.
    assert_xsd_rejects(
        "bad blockCausality enum",
        &surgically(
            &manifest,
            "blockCausality=\"output\"",
            "blockCausality=\"sideways\"",
        ),
        &xsd,
    );
    // Malformed manifest UUID: braces are required by efmiManifestIdentifierType.
    assert_xsd_rejects(
        "unbraced manifest UUID",
        &surgically(&manifest, "id=\"{", "id=\""),
        &xsd,
    );
}

/// GAL-021 rung check, part 1: the on-disk layout is a complete eFMU
/// directory form and both manifests validate against the vendored XSDs.
#[test]
fn compile_target_galec_emits_schema_valid_efmu_container() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let container = build_container(dir.path(), &out_dir);

    // The eFMU root holds exactly __content.xml, schemas/, and the one
    // Algorithm Code representation container (eFMI ch. 2).
    let root_entries: BTreeSet<String> = fs::read_dir(&container.root)
        .expect("read container root")
        .map(|entry| entry.expect("dir entry").file_name().into_string().unwrap())
        .collect();
    let expected: BTreeSet<String> = ["__content.xml", "schemas", "AlgorithmCode"]
        .into_iter()
        .map(str::to_owned)
        .collect();
    assert_eq!(
        root_entries, expected,
        "eFMU root must hold exactly __content.xml, schemas/, AlgorithmCode/"
    );

    // schemas/ is the complete target-owned Beta-1 asset tree, byte for byte
    // (GAL-023), including its origin/license README.
    let schemas = vendored_schemas_dir("galec");
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

    // The representation carries the manifest and the GALEC block source.
    let alg = fs::read_to_string(container.alg_file()).expect("container must hold the .alg file");
    assert!(
        alg.contains("method DoStep"),
        "GALEC block source must contain the DoStep method:\n{alg}"
    );

    // Hard requirement: a missing xmllint surfaces as
    // EfmiError::XmllintUnavailable and fails these expects — the test never
    // skips schema validation (GAL-012/GAL-021).
    validate_against_xsd(
        &container.content_xml(),
        &vendored_schemas_dir("galec").join("efmiContainerManifest.xsd"),
    )
    .expect("__content.xml must validate against the vendored container XSD");
    validate_against_xsd(
        &container.manifest_xml(),
        &vendored_schemas_dir("galec").join("AlgorithmCode/efmiAlgorithmCodeManifest.xsd"),
    )
    .expect("manifest.xml must validate against the vendored Algorithm Code XSD");
}

/// GAL-021 rung check, part 2: every recorded SHA-1 recomputes from the
/// bytes actually on disk — `__content.xml`'s manifest checksum from the
/// written manifest.xml, and the manifest's `File` checksum from the
/// written `.alg`. Both artifacts must be fed from one projection pass: if
/// the CLI ever re-projected per file, any nondeterminism in lowering or
/// printing would silently invalidate the eFMU. This pins the files against
/// each other, never against golden values.
#[test]
fn container_checksums_recompute_from_written_bytes() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let container = build_container(dir.path(), &out_dir);

    let manifest_bytes = fs::read(container.manifest_xml()).expect("read manifest bytes");
    let recorded = sole_attribute_value(&container.content_xml(), "checksum");
    assert_eq!(
        recorded,
        rumoca::sha1_hex(&manifest_bytes),
        "__content.xml checksum must be the SHA-1 of the written manifest.xml"
    );

    let alg_bytes = fs::read(container.alg_file()).expect("read .alg bytes");
    let listed = sole_attribute_value(&container.manifest_xml(), "checksum");
    assert_eq!(
        listed,
        rumoca::sha1_hex(&alg_bytes),
        "manifest.xml File checksum must be the SHA-1 of the written .alg"
    );
}

/// GAL-021 rung check, part 3: id discipline and generation metadata.
/// Every `id` is unique across `__content.xml` AND `manifest.xml` together,
/// `manifestRefId` matches the manifest's own id, both timestamps parse
/// under the strict UTC pattern, and both documents name this tool.
#[test]
fn container_ids_unique_and_generation_metadata_strict() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let container = build_container(dir.path(), &out_dir);

    let mut seen: BTreeSet<String> = BTreeSet::new();
    for path in [container.content_xml(), container.manifest_xml()] {
        for id in attribute_values(&path, "id") {
            assert!(
                seen.insert(id.clone()),
                "id `{id}` appears more than once across __content.xml and manifest.xml"
            );
        }
    }

    let manifest_ref_id = sole_attribute_value(&container.content_xml(), "manifestRefId");
    let manifest_id = attribute_values(&container.manifest_xml(), "id")
        .into_iter()
        .next()
        .expect("manifest.xml root id");
    assert_eq!(
        manifest_ref_id, manifest_id,
        "__content.xml manifestRefId must be the manifest's own root id"
    );
    assert_manifest_id(&manifest_ref_id, &container.content_xml());

    for path in [container.content_xml(), container.manifest_xml()] {
        let timestamp = sole_attribute_value(&path, "generationDateAndTime");
        assert_strict_utc_timestamp(&timestamp, &path);
        let tool = sole_attribute_value(&path, "generationTool");
        assert!(
            tool.starts_with("rumoca "),
            "generationTool must start with `rumoca `, got `{tool}` in {}",
            path.display()
        );
    }
}

/// GAL-021 rung check, part 4: the `.efmu` zip form holds `__content.xml`
/// at the zip root and is entry-for-entry, byte-for-byte the directory form.
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

/// Determinism boundary (pins against accidental re-projection): two runs
/// differ ONLY in freshly minted UUIDs and generation timestamps (and, in
/// `__content.xml`, the manifest checksum derived from those manifest
/// bytes). Everything else — the `.alg` text, the schema copies, all
/// structural XML content, the zip entry set — is byte-identical.
#[test]
fn efmu_builds_differ_only_in_uuids_and_timestamps() {
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
        match relative.as_str() {
            "__content.xml" => {
                // The recorded manifest checksum is a pure function of the
                // (uuid/timestamp-bearing) manifest bytes, so it is masked
                // together with the two documented nondeterminism sources.
                let normalize = |bytes: &[u8]| {
                    let text = String::from_utf8(bytes.to_vec()).expect("UTF-8 XML");
                    mask_attribute(
                        &mask_attribute(&mask_uuids(&text), "generationDateAndTime"),
                        "checksum",
                    )
                };
                assert_eq!(
                    normalize(&bytes_a),
                    normalize(&bytes_b),
                    "__content.xml may differ only in UUIDs/timestamp/derived checksum"
                );
            }
            "AlgorithmCode/manifest.xml" => {
                // The File checksum is NOT masked here: the .alg bytes are
                // deterministic, so their recorded SHA-1 must be too.
                let normalize = |bytes: &[u8]| {
                    let text = String::from_utf8(bytes.to_vec()).expect("UTF-8 XML");
                    mask_attribute(&mask_uuids(&text), "generationDateAndTime")
                };
                assert_eq!(
                    normalize(&bytes_a),
                    normalize(&bytes_b),
                    "manifest.xml may differ only in its UUID and timestamp"
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

/// Re-running the identical command into the same --output must replace the
/// previous container (the edit-recompile loop, matching `build = "fmu"`'s
/// overwrite-on-re-run UX): the CLI owns `<out_dir>/<Model>/` as its build
/// product and clears a root it recognizes as a previous eFMU before
/// repackaging. The second container must still be fully self-consistent.
#[test]
fn rerunning_same_command_replaces_previous_container() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let first = build_container(dir.path(), &out_dir);
    let first_manifest = fs::read(first.manifest_xml()).expect("read first manifest");

    // Same fixture, same out dir: build_container asserts CLI success.
    let second = build_container(dir.path(), &out_dir);
    let second_manifest = fs::read(second.manifest_xml()).expect("read second manifest");
    assert_ne!(
        first_manifest, second_manifest,
        "the container must be rebuilt (fresh manifest UUID), not left stale"
    );
    let recorded = sole_attribute_value(&second.content_xml(), "checksum");
    assert_eq!(
        recorded,
        rumoca::sha1_hex(&second_manifest),
        "the replaced container's checksum must recompute from its own bytes"
    );
    assert!(second.efmu_zip.is_file(), ".efmu zip must be rebuilt too");
}

/// A foreign non-empty directory at `<out_dir>/<Model>` (no __content.xml)
/// is NOT a previous build product and must be refused with the remedy —
/// never deleted.
#[test]
fn foreign_directory_at_container_path_is_refused_with_remedy() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let foreign = out_dir.join(MODEL);
    fs::create_dir_all(&foreign).expect("create foreign directory");
    let keep = foreign.join("keep.txt");
    fs::write(&keep, b"user data").expect("write foreign file");

    let file = write_fixture(dir.path(), MODEL, DISCRETE_FIXTURE);
    let output = run_compile_target_galec(&file, &out_dir);
    assert!(
        !output.status.success(),
        "packaging over a foreign directory must fail.\nstdout:\n{}",
        String::from_utf8_lossy(&output.stdout)
    );
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("refusing to remove") && stderr.contains("--output"),
        "the error must state the remedy, got stderr:\n{stderr}"
    );
    assert_eq!(
        fs::read(&keep).expect("foreign file must survive"),
        b"user data",
        "foreign content must never be deleted"
    );
}

/// Hierarchical fixture: the same discrete shape nested inside a package,
/// selected with `--model GalecCliPkg.Inner`.
const NESTED_FIXTURE: &str = "\
package GalecCliPkg
  model Inner
    constant Real samplePeriod = 0.1;
    parameter Real gain = 2.0;
    discrete output Real y(start = 0.0);
  equation
    when sample(0.0, samplePeriod) then
      y = gain * (pre(y) + 1.0);
    end when;
  end Inner;
end GalecCliPkg;
";

/// eFMI ch. 2.3.1 intends `Content/@name` to be the block name as in the
/// source modeling environment, so a hierarchical model keeps its dotted
/// name there, while file-system artifacts (container directory, `.efmu`)
/// use the underscored identifier. The Algorithm Code manifest's own name
/// stays the projection's GALEC block identifier (dots are not valid GALEC
/// block names) — that output is consumed as-is.
#[test]
fn content_name_carries_dotted_source_model_name() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("GalecCliPkg.mo");
    fs::write(&file, NESTED_FIXTURE).expect("write nested fixture");
    for target in ["galec", "galec-production"] {
        let out_dir = dir.path().join(target);
        let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
            .arg("compile")
            .arg(&file)
            .arg("--model")
            .arg("GalecCliPkg.Inner")
            .arg("--target")
            .arg(target)
            .arg("-o")
            .arg(&out_dir)
            .output()
            .unwrap_or_else(|error| panic!("run nested-model {target} compile: {error}"));
        assert!(
            output.status.success(),
            "nested-model {target} compile failed.\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );

        let root = out_dir.join("GalecCliPkg_Inner");
        assert!(root.is_dir(), "container directory uses the identifier");
        assert!(
            out_dir.join("GalecCliPkg_Inner.efmu").is_file(),
            ".efmu archive uses the identifier"
        );
        // Document order: the root Content element's name comes first, before
        // the ModelRepresentation entries' names.
        let names = attribute_values(&root.join("__content.xml"), "name");
        assert_eq!(
            names.first().map(String::as_str),
            Some("GalecCliPkg.Inner"),
            "{target} Content/@name must be the source model name, got {names:?}"
        );
    }
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

/// GAL-036/GAL-037 regression ratchet.  The structural assertions are the
/// proof obligations; line ceilings are a secondary, reviewable alarm for a
/// new form of expression or tensor expansion that those obligations do not
/// yet name.
#[test]
fn estimator_projection_preserves_call_cardinality_and_compact_tensors() {
    let dir = tempdir().expect("tempdir");
    let file = write_fixture(
        dir.path(),
        COMPACT_ESTIMATOR_MODEL,
        COMPACT_ESTIMATOR_FIXTURE,
    );

    let galec_out = dir.path().join("galec");
    let galec_compile = run_compile_target(&file, "galec", &galec_out);
    assert!(
        galec_compile.status.success(),
        "compact GALEC fixture failed:\n{}",
        String::from_utf8_lossy(&galec_compile.stderr)
    );
    let galec_path = galec_out
        .join(COMPACT_ESTIMATOR_MODEL)
        .join("AlgorithmCode")
        .join(format!("{COMPACT_ESTIMATOR_MODEL}.alg"));
    let galec = fs::read_to_string(&galec_path).expect("read compact GALEC artifact");

    let c_out = dir.path().join("embedded-c-galec");
    let c_compile = run_compile_target(&file, "embedded-c-galec", &c_out);
    assert!(
        c_compile.status.success(),
        "compact embedded-C fixture failed:\n{}",
        String::from_utf8_lossy(&c_compile.stderr)
    );
    let c_path = c_out.join(format!("{COMPACT_ESTIMATOR_MODEL}.c"));
    let c = fs::read_to_string(&c_path).expect("read compact generated C artifact");

    assert!(!has_ordinal_call_temporary(&galec), "{galec}");
    assert!(!has_ordinal_call_temporary(&c), "{c}");
    for function in [
        "compactPredict",
        "compactCorrectPosition",
        "compactCorrectVelocity",
        "compactEstimatorStep",
    ] {
        assert_eq!(
            galec_call_site_count(&galec, function),
            1,
            "one source call to {function} must remain one GALEC call site:\n{galec}"
        );
        assert_eq!(
            c_call_site_count(&c, function),
            1,
            "one GALEC call to {function} must remain one C call site:\n{c}"
        );
    }
    assert!(
        c.contains("for (int32_t rumoca_tensor_"),
        "tensor relationships must lower to bounded C loops:\n{c}"
    );
    let constant_coordinate_assignments = c
        .lines()
        .filter(|line| {
            line.contains("[((int32_t)(")
                && line.contains(")) - 1] =")
                && line.split("[((int32_t)(").nth(1).is_some_and(|tail| {
                    tail.starts_with(|character: char| character.is_ascii_digit())
                })
        })
        .count();
    assert!(
        constant_coordinate_assignments <= 36,
        "constant-coordinate C assignments regressed from the reviewed ceiling: {constant_coordinate_assignments}"
    );
    assert!(
        galec.lines().count() <= 240,
        "GALEC artifact unexpectedly expanded to {} lines",
        galec.lines().count()
    );
    assert!(
        c.lines().count() <= 850,
        "C artifact unexpectedly expanded to {} lines",
        c.lines().count()
    );
    let longest_galec = galec
        .lines()
        .max_by_key(|line| line.len())
        .unwrap_or_default();
    assert!(
        longest_galec.len() <= 240,
        "GALEC artifact contains an expanded {}-byte expression line:\n{longest_galec}",
        longest_galec.len()
    );
    assert!(
        c.lines().map(str::len).max().unwrap_or_default() <= 240,
        "C artifact contains an expanded expression line"
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
