//! End-to-end CLI coverage for `rumoca compile --target galec`
//! (SPEC_0034 GAL-011/GAL-012/GAL-021).
//!
//! Invokes the real binary so the whole chain is exercised: CLI dispatch →
//! generic capability gate → GALEC projection facade → typed printer / XML
//! serializer → passthrough templates → `build = "efmu"` container packaging.
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

/// The vendored eFMI Beta-1 schema tree (GAL-023).
fn vendored_schemas_dir() -> PathBuf {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("../rumoca-efmi/assets/efmi-schemas");
    assert!(
        dir.is_dir(),
        "vendored schema tree missing at {}",
        dir.display()
    );
    dir
}

/// Relative `/`-separated paths of every file under `root`.
fn relative_file_paths(root: &Path) -> BTreeSet<String> {
    walkdir::WalkDir::new(root)
        .into_iter()
        .map(|entry| entry.expect("walk directory tree"))
        .filter(|entry| entry.file_type().is_file())
        .map(|entry| {
            entry
                .path()
                .strip_prefix(root)
                .expect("walked path is under root")
                .components()
                .map(|c| c.as_os_str().to_str().expect("UTF-8 path").to_owned())
                .collect::<Vec<_>>()
                .join("/")
        })
        .collect()
}

/// All values of attribute `name` anywhere in an XML document, in document
/// order (quick-xml over the exact on-disk bytes).
fn attribute_values(xml_path: &Path, name: &str) -> Vec<String> {
    let bytes = fs::read(xml_path).expect("read XML file");
    let mut reader = quick_xml::Reader::from_reader(bytes.as_slice());
    let mut values = Vec::new();
    let mut buf = Vec::new();
    loop {
        use quick_xml::events::Event;
        match reader.read_event_into(&mut buf).expect("well-formed XML") {
            Event::Eof => break,
            Event::Start(element) | Event::Empty(element) => {
                values.extend(named_attribute_values(&element, name));
            }
            _ => {}
        }
        buf.clear();
    }
    values
}

/// Values of every attribute called `name` on one element.
fn named_attribute_values(element: &quick_xml::events::BytesStart<'_>, name: &str) -> Vec<String> {
    element
        .attributes()
        .map(|attribute| attribute.expect("well-formed attribute"))
        .filter(|attribute| attribute.key.as_ref() == name.as_bytes())
        .map(|attribute| {
            attribute
                .unescape_value()
                .expect("unescapable attribute")
                .into_owned()
        })
        .collect()
}

/// The single value of attribute `name` in the document.
fn sole_attribute_value(xml_path: &Path, name: &str) -> String {
    let values = attribute_values(xml_path, name);
    assert_eq!(
        values.len(),
        1,
        "expected exactly one `{name}` attribute in {}, found {values:?}",
        xml_path.display()
    );
    values.into_iter().next().expect("length checked")
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

    // schemas/ is the complete vendored Beta-1 tree, byte for byte
    // (GAL-023; the repository-only README.md is not part of the copies).
    let mut vendored = relative_file_paths(&vendored_schemas_dir());
    vendored.remove("README.md");
    let emitted = relative_file_paths(&container.root.join("schemas"));
    assert_eq!(emitted, vendored, "schemas/ must mirror the vendored tree");
    for relative in &vendored {
        let vendored_bytes = fs::read(vendored_schemas_dir().join(relative)).unwrap();
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
    rumoca_efmi::validate_against_xsd(
        &container.content_xml(),
        &vendored_schemas_dir().join("efmiContainerManifest.xsd"),
    )
    .expect("__content.xml must validate against the vendored container XSD");
    rumoca_efmi::validate_against_xsd(
        &container.manifest_xml(),
        &vendored_schemas_dir().join("AlgorithmCode/efmiAlgorithmCodeManifest.xsd"),
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
        rumoca_efmi::Sha1Hex::of_bytes(&manifest_bytes).as_str(),
        "__content.xml checksum must be the SHA-1 of the written manifest.xml"
    );

    let alg_bytes = fs::read(container.alg_file()).expect("read .alg bytes");
    let listed = sole_attribute_value(&container.manifest_xml(), "checksum");
    assert_eq!(
        listed,
        rumoca_efmi::Sha1Hex::of_bytes(&alg_bytes).as_str(),
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
    rumoca_efmi::ManifestId::parse(&manifest_ref_id)
        .expect("manifestRefId must be a brace-wrapped UUID");

    for path in [container.content_xml(), container.manifest_xml()] {
        let timestamp = sole_attribute_value(&path, "generationDateAndTime");
        rumoca_efmi::UtcTimestamp::parse(&timestamp).unwrap_or_else(|error| {
            panic!(
                "generationDateAndTime `{timestamp}` in {} must match the strict \
                 UTC pattern: {error}",
                path.display()
            )
        });
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

/// Replace every brace-wrapped UUID with `{UUID}` — the documented
/// per-build nondeterminism of the container metadata.
fn mask_uuids(text: &str) -> String {
    /// Length of the hyphenated hex form between the braces.
    const INNER: usize = 36;
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    while let Some(at) = rest.find('{') {
        let candidate = &rest.as_bytes()[at..];
        let is_uuid = candidate.len() > INNER + 1
            && candidate[INNER + 1] == b'}'
            && candidate[1..=INNER]
                .iter()
                .all(|b| b.is_ascii_hexdigit() || *b == b'-');
        out.push_str(&rest[..at]);
        if is_uuid {
            out.push_str("{UUID}");
            rest = &rest[at + INNER + 2..];
        } else {
            out.push('{');
            rest = &rest[at + 1..];
        }
    }
    out.push_str(rest);
    out
}

/// Replace the value of `attribute="…"` occurrences with a fixed marker.
fn mask_attribute(text: &str, attribute: &str) -> String {
    let needle = format!("{attribute}=\"");
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    while let Some(at) = rest.find(&needle) {
        let value_start = at + needle.len();
        let value_len = rest[value_start..]
            .find('"')
            .expect("attribute value must be quote-terminated");
        out.push_str(&rest[..value_start]);
        out.push_str("MASKED");
        rest = &rest[value_start + value_len..];
    }
    out.push_str(rest);
    out
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
        rumoca_efmi::Sha1Hex::of_bytes(&second_manifest).as_str(),
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
    let out_dir = dir.path().join("out");

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("compile")
        .arg(&file)
        .arg("--model")
        .arg("GalecCliPkg.Inner")
        .arg("--target")
        .arg("galec")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("run rumoca compile --model GalecCliPkg.Inner --target galec");
    assert!(
        output.status.success(),
        "nested-model compile failed.\nstdout:\n{}\nstderr:\n{}",
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
        "Content/@name must be the source model name, got {names:?}"
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
