//! End-to-end eFMU container tests (GAL-021/023):
//!
//! - emit-then-XSD-validate `__content.xml` and a full Algorithm Code
//!   manifest against the vendored Beta-1 XSDs via `xmllint`;
//! - full-container write: layout, re-validation of every XML against the
//!   *emitted* schema copies, SHA-1 recomputation from disk bytes, and
//!   per-manifest id uniqueness;
//! - negative schema cases (each wrong in exactly one way) that `xmllint`
//!   must reject;
//! - `.efmu` zip form: `__content.xml` at the zip root, byte-equal to the
//!   directory form.
//!
//! All fixtures are authored independently from the XSDs; nothing is copied
//! from the CC-BY-SA eFMI Standard text (GAL-023).

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::io::Read as _;
use std::path::{Path, PathBuf};

use quick_xml::Reader;
use quick_xml::events::Event;
use rumoca_efmi::algorithm_code_manifest::{
    AlgorithmCodeManifest, AlgorithmCodeManifestParts, BlockCausality, BlockMethod, BlockMethods,
    Clock, ErrorSignal, ErrorSignalStatus, RealVariable, StartValue, Variable, VariableCommon,
};
use rumoca_efmi::content::{Content, ContentParts, ModelRepresentation, ModelRepresentationKind};
use rumoca_efmi::manifest_common::{
    BaseUnit, File, FileChecksum, FileRole, ManifestAttributes, Unit,
};
use rumoca_efmi::{
    EfmiError, EfmuLayout, EfmuMeta, FilePath, Identifier, ManifestId, ModelRepresentationFiles,
    NameWithoutSlashes, NormalizedText, RepresentationFile, Sha1Hex, UtcTimestamp,
    algorithm_code_manifest_to_xml, content_to_xml, validate_against_xsd, write_efmu_container,
    write_efmu_zip,
};

/// Tiny synthetic code file, authored for this test — not derived from any
/// eFMI Standard example.
const ALG_TEXT: &str = "/* Synthetic GALEC-style placeholder authored for rumoca tests. */\n\
                        block RumocaSynthetic\n\
                        /* intentionally minimal body */\n\
                        end RumocaSynthetic\n";

const MANIFEST_UUID: &str = "{5b0c2a7e-11d4-4c5f-9a6d-3e2b804cf917}";
const CONTENT_UUID: &str = "{9c41d6b2-70aa-4f28-8d35-6f1e5a20c483}";
const TIMESTAMP: &str = "2026-07-02T12:00:00Z";
const CONTAINER_NAME: &str = "AlgorithmCode";

fn ident(value: &str) -> Identifier {
    Identifier::new(value).unwrap()
}

fn text(value: &str) -> NormalizedText {
    NormalizedText::new(value).unwrap()
}

fn attributes(uuid: &str) -> ManifestAttributes {
    ManifestAttributes {
        id: ManifestId::parse(uuid).unwrap(),
        name: text("RumocaTest.Synthetic"),
        description: None,
        version: None,
        generation_date_and_time: UtcTimestamp::parse(TIMESTAMP).unwrap(),
        generation_tool: Some(text("rumoca")),
        copyright: None,
        license: None,
    }
}

fn real(id: &str, name: &str, causality: BlockCausality, start: StartValue<f64>) -> Variable {
    Variable::Real(RealVariable {
        common: VariableCommon {
            id: ident(id),
            name: text(name),
            description: None,
            block_causality: causality,
            dimensions: vec![],
            annotations: vec![],
        },
        start,
        unit_ref_id: None,
        relative_quantity: false,
        min: None,
        max: None,
        nominal: None,
    })
}

fn fixture_variables() -> Vec<Variable> {
    let mut sample_period = real(
        "V_T",
        "T",
        BlockCausality::Constant,
        StartValue::Scalar(0.01),
    );
    let Variable::Real(ref mut v) = sample_period else {
        unreachable!()
    };
    v.unit_ref_id = Some(ident("U_S"));
    let mut gains = real(
        "V_GAIN",
        "gain",
        BlockCausality::Constant,
        StartValue::Array(vec![1.0, 2.0]),
    );
    let Variable::Real(ref mut v) = gains else {
        unreachable!()
    };
    v.common.dimensions = vec![2];
    vec![
        real("V_U", "u", BlockCausality::Input, StartValue::Scalar(0.0)),
        real("V_Y", "y", BlockCausality::Output, StartValue::Scalar(0.0)),
        real(
            "V_PX",
            "previous(x)",
            BlockCausality::State,
            StartValue::Scalar(0.0),
        ),
        sample_period,
        gains,
    ]
}

fn fixture_manifest() -> AlgorithmCodeManifest {
    AlgorithmCodeManifest::new(AlgorithmCodeManifestParts {
        attributes: attributes(MANIFEST_UUID),
        file_ref_id: ident("F_ALG"),
        files: vec![
            File {
                id: ident("F_ALG"),
                name: NameWithoutSlashes::new("Model.alg").unwrap(),
                path: FilePath::root(),
                checksum: FileChecksum::Sha1(Sha1Hex::of_bytes(ALG_TEXT.as_bytes())),
                role: FileRole::Code,
                description: None,
            },
            File {
                id: ident("F_MANIFEST"),
                name: NameWithoutSlashes::new("manifest.xml").unwrap(),
                path: FilePath::root(),
                checksum: FileChecksum::NotNeeded,
                role: FileRole::Manifest,
                description: None,
            },
        ],
        clock: Clock {
            id: ident("CLK"),
            variable_ref_id: ident("V_T"),
        },
        block_methods: BlockMethods {
            startup: BlockMethod {
                id: ident("BM_STARTUP"),
                signals: vec![],
            },
            recalibrate: BlockMethod {
                id: ident("BM_RECALIBRATE"),
                signals: vec![],
            },
            do_step: BlockMethod {
                id: ident("BM_DOSTEP"),
                signals: vec![ErrorSignal::Nan],
            },
        },
        error_signal_status: ErrorSignalStatus { id: ident("ESS") },
        units: vec![Unit {
            id: ident("U_S"),
            name: text("s"),
            base_unit: Some(BaseUnit {
                s: 1,
                ..BaseUnit::default()
            }),
        }],
        variables: fixture_variables(),
        annotations: vec![],
    })
    .expect("fixture manifest must validate")
}

fn fixture_content(manifest_checksum: Sha1Hex) -> Content {
    Content::new(ContentParts {
        attributes: attributes(CONTENT_UUID),
        active_fmu: None,
        model_representations: vec![ModelRepresentation {
            name: NameWithoutSlashes::new(CONTAINER_NAME).unwrap(),
            kind: ModelRepresentationKind::AlgorithmCode,
            manifest: NameWithoutSlashes::new("manifest.xml").unwrap(),
            checksum: manifest_checksum,
            manifest_ref_id: ManifestId::parse(MANIFEST_UUID).unwrap(),
        }],
    })
    .expect("fixture content must validate")
}

fn fixture_representation() -> ModelRepresentationFiles {
    ModelRepresentationFiles {
        name: NameWithoutSlashes::new(CONTAINER_NAME).unwrap(),
        kind: ModelRepresentationKind::AlgorithmCode,
        manifest_name: NameWithoutSlashes::new("manifest.xml").unwrap(),
        manifest_ref_id: ManifestId::parse(MANIFEST_UUID).unwrap(),
        manifest_bytes: algorithm_code_manifest_to_xml(&fixture_manifest()).unwrap(),
        files: vec![RepresentationFile {
            directory: FilePath::root(),
            name: NameWithoutSlashes::new("Model.alg").unwrap(),
            bytes: ALG_TEXT.as_bytes().to_vec(),
        }],
    }
}

fn fixture_meta() -> EfmuMeta {
    EfmuMeta {
        attributes: attributes(CONTENT_UUID),
    }
}

fn vendored_schema(relative: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("assets")
        .join("efmi-schemas")
        .join(relative)
}

/// Parse an XML document into `(element name, attribute map)` pairs in
/// document order.
fn elements(xml: &str) -> Vec<(String, BTreeMap<String, String>)> {
    let mut reader = Reader::from_str(xml);
    let mut out = Vec::new();
    loop {
        match reader.read_event().expect("test XML is well-formed") {
            Event::Eof => break,
            Event::Start(element) | Event::Empty(element) => {
                let name = String::from_utf8(element.name().as_ref().to_vec()).unwrap();
                let mut attrs = BTreeMap::new();
                for attr in element.attributes() {
                    let attr = attr.expect("test XML attributes are well-formed");
                    attrs.insert(
                        String::from_utf8(attr.key.as_ref().to_vec()).unwrap(),
                        attr.unescape_value().unwrap().into_owned(),
                    );
                }
                out.push((name, attrs));
            }
            _ => {}
        }
    }
    out
}

/// Recursively collect `/`-separated file paths relative to `root`.
fn walk_relative(root: &Path, dir: &Path, out: &mut BTreeSet<String>) {
    for entry in fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            walk_relative(root, &path, out);
            continue;
        }
        let relative = path
            .strip_prefix(root)
            .unwrap()
            .components()
            .map(|c| c.as_os_str().to_str().unwrap().to_owned())
            .collect::<Vec<_>>()
            .join("/");
        out.insert(relative);
    }
}

/// (a) Emitted `__content.xml` and Algorithm Code manifest are schema-valid
/// against the vendored XSD tree, checked by a real `xmllint` run.
#[test]
fn emitted_xml_validates_against_vendored_xsds() {
    let dir = tempfile::tempdir().unwrap();
    let manifest_bytes = algorithm_code_manifest_to_xml(&fixture_manifest()).unwrap();
    let manifest_path = dir.path().join("manifest.xml");
    fs::write(&manifest_path, &manifest_bytes).unwrap();
    validate_against_xsd(
        &manifest_path,
        &vendored_schema("AlgorithmCode/efmiAlgorithmCodeManifest.xsd"),
    )
    .expect("emitted manifest must be schema-valid");

    let content = fixture_content(Sha1Hex::of_bytes(&manifest_bytes));
    let content_path = dir.path().join("__content.xml");
    fs::write(&content_path, content_to_xml(&content).unwrap()).unwrap();
    validate_against_xsd(&content_path, &vendored_schema("efmiContainerManifest.xsd"))
        .expect("emitted __content.xml must be schema-valid");
}

fn write_fixture_container(root: &Path) -> EfmuLayout {
    write_efmu_container(&[fixture_representation()], &fixture_meta(), root)
        .expect("container write must succeed")
}

/// (b) Full container: exact root layout, every XML re-validated against the
/// *emitted* schema copies, every SHA-1 recomputed from disk bytes, id
/// uniqueness per manifest, and a byte-identical `schemas/` copy (GAL-023).
#[test]
fn full_container_layout_checksums_and_ids() {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path().join("efmu");
    let layout = write_fixture_container(&root);

    assert_root_layout(&root);
    validate_container_xml(&layout);
    assert_content_checksums(&layout);
    assert_manifest_file_checksums(&layout);
    assert_id_uniqueness(&layout);
    assert_schemas_copy_matches_vendored(&layout);
}

/// The eFMU root holds EXACTLY `__content.xml`, `schemas/`, and one directory
/// per representation.
fn assert_root_layout(root: &Path) {
    let entries: BTreeSet<String> = fs::read_dir(root)
        .unwrap()
        .map(|e| e.unwrap().file_name().to_str().unwrap().to_owned())
        .collect();
    let expected: BTreeSet<String> = ["__content.xml", "schemas", CONTAINER_NAME]
        .into_iter()
        .map(str::to_owned)
        .collect();
    assert_eq!(entries, expected, "unexpected eFMU root entries");
}

/// Every XML in the container validates against the emitted `schemas/` tree
/// (proving both the documents and the copied schema tree, including its
/// relative `xs:include`s).
fn validate_container_xml(layout: &EfmuLayout) {
    validate_against_xsd(
        &layout.content_xml,
        &layout.schemas_dir.join("efmiContainerManifest.xsd"),
    )
    .expect("__content.xml must validate against the emitted schemas");
    for representation in &layout.representations {
        validate_against_xsd(
            &representation.manifest_path,
            &layout
                .schemas_dir
                .join("AlgorithmCode")
                .join("efmiAlgorithmCodeManifest.xsd"),
        )
        .expect("manifest must validate against the emitted schemas");
    }
}

/// Every `ModelRepresentation/@checksum` in `__content.xml` equals the SHA-1
/// recomputed from the manifest bytes on disk (GAL-021: never a placeholder).
fn assert_content_checksums(layout: &EfmuLayout) {
    let content = fs::read_to_string(&layout.content_xml).unwrap();
    let representations: Vec<BTreeMap<String, String>> = elements(&content)
        .into_iter()
        .filter(|(name, _)| name == "ModelRepresentation")
        .map(|(_, attrs)| attrs)
        .collect();
    assert_eq!(representations.len(), layout.representations.len());
    for attrs in &representations {
        let manifest_path = layout.root.join(&attrs["name"]).join(&attrs["manifest"]);
        let recomputed = Sha1Hex::of_bytes(&fs::read(&manifest_path).unwrap());
        assert_eq!(
            recomputed.as_str(),
            attrs["checksum"],
            "stale checksum for {}",
            attrs["name"]
        );
    }
    for written in &layout.representations {
        let recomputed = Sha1Hex::of_bytes(&fs::read(&written.manifest_path).unwrap());
        assert_eq!(recomputed, written.manifest_checksum);
    }
}

/// Every manifest `File` entry with `needsChecksum="true"` matches the SHA-1
/// of the actual bytes written into the container.
fn assert_manifest_file_checksums(layout: &EfmuLayout) {
    for representation in &layout.representations {
        let manifest = fs::read_to_string(&representation.manifest_path).unwrap();
        let mut checked = 0;
        for (name, attrs) in elements(&manifest) {
            if name != "File" || attrs["needsChecksum"] != "true" {
                continue;
            }
            let mut target = representation.directory.clone();
            for segment in attrs["path"].split_terminator('/').skip(1) {
                target.push(segment);
            }
            target.push(&attrs["name"]);
            let recomputed = Sha1Hex::of_bytes(&fs::read(&target).unwrap());
            assert_eq!(
                recomputed.as_str(),
                attrs["checksum"],
                "stale file checksum for {}",
                attrs["name"]
            );
            checked += 1;
        }
        assert!(checked > 0, "fixture must exercise needsChecksum files");
    }
}

/// All `id` attribute values are unique across each manifest file as a whole
/// (eFMI manifest principle 6).
fn assert_id_uniqueness(layout: &EfmuLayout) {
    let mut paths = vec![layout.content_xml.clone()];
    paths.extend(
        layout
            .representations
            .iter()
            .map(|r| r.manifest_path.clone()),
    );
    for path in paths {
        let xml = fs::read_to_string(&path).unwrap();
        let mut seen = BTreeSet::new();
        for (_, attrs) in elements(&xml) {
            if let Some(id) = attrs.get("id") {
                assert!(seen.insert(id.clone()), "duplicate id `{id}` in {path:?}");
            }
        }
        assert!(!seen.is_empty(), "no ids found in {path:?}");
    }
}

/// The emitted `schemas/` tree is byte-identical to the vendored assets
/// (minus the repository-only `README.md`), LICENSE included.
fn assert_schemas_copy_matches_vendored(layout: &EfmuLayout) {
    let vendored_root = vendored_schema("");
    let mut vendored = BTreeSet::new();
    walk_relative(&vendored_root, &vendored_root, &mut vendored);
    vendored.remove("README.md");
    let mut emitted = BTreeSet::new();
    walk_relative(&layout.schemas_dir, &layout.schemas_dir, &mut emitted);
    assert_eq!(emitted, vendored, "schemas/ tree drifted from the assets");
    assert!(emitted.contains("LICENSE"), "LICENSE must be copied");
    for relative in &emitted {
        assert_eq!(
            fs::read(layout.schemas_dir.join(relative)).unwrap(),
            fs::read(vendored_root.join(relative)).unwrap(),
            "schema copy differs for {relative}"
        );
    }
}

/// Replace `needle` with `replacement`, asserting the surgery changed
/// something (so a stale needle cannot silently produce a passing test).
fn surgically(xml: &str, needle: &str, replacement: &str) -> String {
    assert!(xml.contains(needle), "surgery needle `{needle}` not found");
    xml.replacen(needle, replacement, 1)
}

/// Write `xml` next to nothing and assert `xmllint` rejects it against the
/// vendored `xsd` (error EFM032, not a missing-tool error).
fn assert_rejected(label: &str, xml: &str, xsd: &Path) {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("case.xml");
    fs::write(&path, xml).unwrap();
    let error = validate_against_xsd(&path, xsd).expect_err(label);
    assert!(
        matches!(error, EfmiError::XsdValidationFailed { .. }),
        "case `{label}`: expected schema rejection, got {error:?}"
    );
}

/// (c) Negative schema cases: each document is wrong in exactly one way and
/// `xmllint` must reject it.
#[test]
fn negative_schema_cases_are_rejected() {
    let manifest_xsd = vendored_schema("AlgorithmCode/efmiAlgorithmCodeManifest.xsd");
    let content_xsd = vendored_schema("efmiContainerManifest.xsd");
    let manifest =
        String::from_utf8(algorithm_code_manifest_to_xml(&fixture_manifest()).unwrap()).unwrap();
    let content = String::from_utf8(
        content_to_xml(&fixture_content(Sha1Hex::of_bytes(manifest.as_bytes()))).unwrap(),
    )
    .unwrap();

    let clock_line = "  <Clock id=\"CLK\" variableRefId=\"V_T\"/>\n";
    // Missing required element: no Clock at all.
    assert_rejected(
        "missing Clock element",
        &surgically(&manifest, clock_line, ""),
        &manifest_xsd,
    );
    // Wrong child order: Clock moved after BlockMethods (violates the
    // xs:sequence order).
    let moved = surgically(
        &surgically(&manifest, clock_line, ""),
        "  </BlockMethods>\n",
        &format!("  </BlockMethods>\n{clock_line}"),
    );
    assert_rejected("Clock after BlockMethods", &moved, &manifest_xsd);
    // Bad blockCausality enumeration value.
    assert_rejected(
        "bad blockCausality enum",
        &surgically(
            &manifest,
            "blockCausality=\"input\"",
            "blockCausality=\"sideways\"",
        ),
        &manifest_xsd,
    );
    // Malformed manifest UUID: braces are required by efmiManifestIdentifierType.
    assert_rejected(
        "unbraced manifest UUID",
        &surgically(
            &manifest,
            &format!("id=\"{MANIFEST_UUID}\""),
            &format!("id=\"{}\"", MANIFEST_UUID.trim_matches(['{', '}'])),
        ),
        &manifest_xsd,
    );
    // Malformed timestamp: the pattern requires the trailing `Z`.
    assert_rejected(
        "timestamp without Z",
        &surgically(
            &manifest,
            &format!("generationDateAndTime=\"{TIMESTAMP}\""),
            &format!(
                "generationDateAndTime=\"{}\"",
                TIMESTAMP.trim_end_matches('Z')
            ),
        ),
        &manifest_xsd,
    );
    // Dimension size below the XSD minimum of 1.
    assert_rejected(
        "dimension size 0",
        &surgically(&manifest, "size=\"2\"", "size=\"0\""),
        &manifest_xsd,
    );
    // Malformed manifestRefId in __content.xml.
    assert_rejected(
        "unbraced manifestRefId",
        &surgically(
            &content,
            &format!("manifestRefId=\"{MANIFEST_UUID}\""),
            &format!(
                "manifestRefId=\"{}\"",
                MANIFEST_UUID.trim_matches(['{', '}'])
            ),
        ),
        &content_xsd,
    );
}

/// (e) API-contract regression (GAL-021): editing a representation file
/// after the manifest was serialized — leaving the manifest's `File/@checksum`
/// stale — is rejected at write time, before anything lands on disk.
#[test]
fn stale_representation_file_checksum_rejected() {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path().join("efmu");
    let mut representation = fixture_representation();
    representation.files[0]
        .bytes
        .extend_from_slice(b"/* edited after manifest serialization */\n");
    let err = write_efmu_container(&[representation], &fixture_meta(), &root)
        .expect_err("stale File/@checksum must be rejected");
    assert!(
        matches!(err, EfmiError::StaleFileChecksum { .. }),
        "expected StaleFileChecksum, got {err:?}"
    );
    assert!(!root.exists(), "nothing may be written on rejection");
}

/// (d) `.efmu` zip form: `__content.xml` at the zip root (no wrapper
/// directory), same file set as the directory form, byte-equal contents.
#[test]
fn efmu_zip_matches_directory_form() {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path().join("efmu");
    write_fixture_container(&root);
    let zip_path = dir.path().join("RumocaSynthetic.efmu");
    write_efmu_zip(&root, &zip_path).expect("zip write must succeed");

    let mut expected = BTreeSet::new();
    walk_relative(&root, &root, &mut expected);
    assert!(expected.contains("__content.xml"));

    let file = fs::File::open(&zip_path).unwrap();
    let mut archive = zip::ZipArchive::new(file).unwrap();
    let names: BTreeSet<String> = archive.file_names().map(str::to_owned).collect();
    assert_eq!(
        names, expected,
        "zip entries must mirror the directory form"
    );
    assert!(
        names.contains("__content.xml"),
        "__content.xml must sit at the zip root"
    );
    for relative in &expected {
        let mut entry = archive.by_name(relative).unwrap();
        let mut zipped = Vec::new();
        entry.read_to_end(&mut zipped).unwrap();
        let on_disk = fs::read(root.join(relative)).unwrap();
        assert_eq!(zipped, on_disk, "zip bytes differ for {relative}");
    }
}
