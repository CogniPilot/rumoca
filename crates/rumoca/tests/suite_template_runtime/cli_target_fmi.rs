//! Executable conformance gate for the FMI 2.0.5 and FMI 3.0.2 ME+CS targets.
//!
//! The gate uses the official schemas and FMPy as an independent importer. It
//! checks packaged source FMUs, builds their shared libraries, and executes
//! both advertised interfaces against analytic tensor and scalar models.

mod lifecycle;

use std::collections::BTreeMap;
use std::fs;
use std::io::{Cursor, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use serde::Deserialize;
use tempfile::tempdir;
use zip::write::SimpleFileOptions;
use zip::{ZipArchive, ZipWriter};

const MODEL: &str = "FmiTensorDecay";
const SOURCE: &str = r#"
model FmiTensorDecay
  output Real x[2](each start = 1.0);
equation
  der(x) = {-0.5 * x[1], -x[2]};
end FmiTensorDecay;
"#;

const UNIT_DERIVATIVE_MODEL: &str = "UnitDerivative";
const UNIT_DERIVATIVE_SOURCE: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/fixtures/golden/UnitDerivative.mo"
));
const UNIT_DERIVATIVE_FMI3_ARCHIVE_MEMBERS: &[&str] = &[
    "modelDescription.xml",
    "sources/buildDescription.xml",
    "sources/model.c",
];

#[derive(Clone, Copy)]
enum Interface {
    ModelExchange,
    CoSimulation,
}

impl Interface {
    const fn name(self) -> &'static str {
        match self {
            Self::ModelExchange => "ModelExchange",
            Self::CoSimulation => "CoSimulation",
        }
    }
}

struct BuiltFmu {
    model_name: String,
    version: &'static str,
    root: PathBuf,
    archive: PathBuf,
    extracted_root: PathBuf,
}

#[derive(Deserialize)]
struct ConformanceConfig {
    fmi2_standard_dir: PathBuf,
    fmi3_standard_dir: PathBuf,
    fmi2_vdm_check: PathBuf,
    fmi3_vdm_check: PathBuf,
}

struct FmiStandard {
    root: PathBuf,
    vdm_check: PathBuf,
}

#[derive(Debug, PartialEq, Eq)]
enum ArchiveInventoryError {
    UnreadableArchive,
    UnreadableMember(usize),
    NonFile(String),
    NonCanonicalPath(String),
    Duplicate(String),
    Confusable { first: String, second: String },
    Missing(String),
    Extra(String),
    WrongOrder { expected: String, found: String },
}

#[test]
fn packaged_fmi2_and_fmi3_execute_me_and_cs_with_tensor_trace_parity() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().expect("create FMI conformance work directory");
    let result = rumoca::Compiler::new()
        .model(MODEL)
        .compile_str(SOURCE, "FmiTensorDecay.mo")
        .expect("compile FMI tensor fixture");
    let fmi2 = build_fmu(work.path(), &result, "fmi2");
    let fmi3 = build_fmu(work.path(), &result, "fmi3");
    validate_package(&fmi2, &standards.0);
    validate_package(&fmi3, &standards.1);

    let traces = [
        execute_profile(work.path(), &fmi2, Interface::ModelExchange),
        execute_profile(work.path(), &fmi2, Interface::CoSimulation),
        execute_profile(work.path(), &fmi3, Interface::ModelExchange),
        execute_profile(work.path(), &fmi3, Interface::CoSimulation),
    ];
    for trace in &traces {
        assert_trace_matches_analytic_solution(trace);
    }
    for trace in &traces[1..] {
        assert_trace_close(&traces[0], trace, 5.0e-5);
    }
}

#[test]
fn unit_derivative_fmi3_package_matches_me_and_cs_lifecycle_contract() {
    // A prerequisite skip is not golden evidence. This exact test refuses to
    // pass until every external validator and lifecycle prerequisite exists;
    // `cargo xtask verify template-runtimes` enforces the same policy for the
    // complete template-runtime gate.
    assert!(
        conformance_prerequisites_are_available(),
        "UnitDerivative golden evidence requires every pinned FMI conformance prerequisite"
    );
    assert_pinned_fmpy();
    let (_, fmi3_standard) = standard_roots();
    let work = tempdir().expect("create FMI 3 UnitDerivative conformance directory");
    let result = rumoca::Compiler::new()
        .model(UNIT_DERIVATIVE_MODEL)
        .compile_str(UNIT_DERIVATIVE_SOURCE, "UnitDerivative.mo")
        .expect("compile FMI 3 UnitDerivative fixture");
    let fmi3 = build_fmu(work.path(), &result, "fmi3");
    assert_exact_unit_derivative_fmi3_archive_inventory(&fmi3);
    validate_package_artifact(&fmi3, &fmi3_standard);
    let model_description = fs::read_to_string(fmi3.extracted_root.join("modelDescription.xml"))
        .expect("read archived FMI 3 UnitDerivative model description");
    let build_description =
        fs::read_to_string(fmi3.extracted_root.join("sources/buildDescription.xml"))
            .expect("read archived FMI 3 UnitDerivative build description");
    lifecycle::validate_unit_derivative(
        &fmi3.extracted_root,
        &fmi3_standard.root,
        &model_description,
        &build_description,
    );
}

#[test]
fn unit_derivative_fmi3_archive_inventory_rejects_nonissued_members() {
    let expected = UNIT_DERIVATIVE_FMI3_ARCHIVE_MEMBERS;
    assert!(check_test_archive_inventory(expected).is_ok());

    let extra = [
        expected[0],
        expected[1],
        expected[2],
        "resources/unclaimed.bin",
    ];
    assert_eq!(
        check_test_archive_inventory(&extra),
        Err(ArchiveInventoryError::Extra(
            "resources/unclaimed.bin".to_owned()
        ))
    );

    let missing = [expected[0], expected[2]];
    assert_eq!(
        check_test_archive_inventory(&missing),
        Err(ArchiveInventoryError::Missing(expected[1].to_owned()))
    );

    let path_confusable = [expected[0], expected[1], "sources/./model.c"];
    assert_eq!(
        check_test_archive_inventory(&path_confusable),
        Err(ArchiveInventoryError::NonCanonicalPath(
            "sources/./model.c".to_owned()
        ))
    );

    let case_confusable = [expected[0], expected[1], expected[2], "sources/MODEL.c"];
    assert_eq!(
        check_test_archive_inventory(&case_confusable),
        Err(ArchiveInventoryError::Confusable {
            first: expected[2].to_owned(),
            second: "sources/MODEL.c".to_owned(),
        })
    );

    let wrong_order = [expected[1], expected[0], expected[2]];
    assert_eq!(
        check_test_archive_inventory(&wrong_order),
        Err(ArchiveInventoryError::WrongOrder {
            expected: expected[0].to_owned(),
            found: expected[1].to_owned(),
        })
    );
}

fn conformance_prerequisites_are_available() -> bool {
    let config = workspace_root().join("target/fmi-conformance/config.json");
    let programs = ["bash", "cc", "cmake", "fmpy", "java", "xmllint"];
    let mut prerequisites: Vec<(&str, bool)> = programs
        .iter()
        .map(|program| (*program, command_is_available(program)))
        .collect();
    prerequisites.push(("FMI conformance configuration", config.is_file()));
    super::template_runtime_policy::prerequisites_are_available(
        "FMI standards-conformance check",
        &prerequisites,
    )
}

fn command_is_available(program: &str) -> bool {
    Command::new(program).arg("--version").output().is_ok()
}

fn assert_pinned_fmpy() {
    let output = checked_output(Command::new("fmpy").arg("--version"), "query FMPy version");
    let version = String::from_utf8_lossy(&output.stdout);
    assert!(
        version.contains("0.3.30"),
        "FMI conformance requires pinned FMPy 0.3.30, found: {version}"
    );
}

fn standard_roots() -> (FmiStandard, FmiStandard) {
    let config_path = workspace_root().join("target/fmi-conformance/config.json");
    let source = fs::read_to_string(&config_path).unwrap_or_else(|error| {
        panic!(
            "read fixed FMI conformance config {}: {error}",
            config_path.display()
        )
    });
    let config: ConformanceConfig =
        serde_json::from_str(&source).expect("parse fixed FMI conformance config");
    (
        FmiStandard {
            root: required_path(config.fmi2_standard_dir, true),
            vdm_check: required_path(config.fmi2_vdm_check, false),
        },
        FmiStandard {
            root: required_path(config.fmi3_standard_dir, true),
            vdm_check: required_path(config.fmi3_vdm_check, false),
        },
    )
}

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("rumoca crate must be two levels below the workspace root")
        .to_path_buf()
}

fn required_path(path: PathBuf, directory: bool) -> PathBuf {
    let path = if path.is_absolute() {
        path
    } else {
        workspace_root().join(path)
    };
    assert!(
        if directory {
            path.is_dir()
        } else {
            path.is_file()
        },
        "configured FMI conformance path has the wrong type: {}",
        path.display()
    );
    path
}

fn build_fmu(work: &Path, result: &rumoca::CompilationResult, target: &'static str) -> BuiltFmu {
    let out = work.join(target);
    let published = rumoca::compile_target(
        result,
        target,
        &out,
        crate::artifact_session::pinned_artifact_input(),
    )
    .unwrap_or_else(|error| panic!("compile {target} source FMU: {error:#}"));
    BuiltFmu {
        model_name: result.model_name().to_owned(),
        version: target,
        root: published.root().to_owned(),
        archive: published
            .archive()
            .expect("an FMI package publishes an archive")
            .to_owned(),
        extracted_root: work.join(format!("{target}-archive-members")),
    }
}

fn validate_package(fmu: &BuiltFmu, standard: &FmiStandard) {
    validate_package_artifact(fmu, standard);
    lifecycle::validate(
        fmu.version,
        &fmu.extracted_root,
        &standard.root,
        &fs::read_to_string(fmu.extracted_root.join("modelDescription.xml"))
            .expect("read archived model description for lifecycle test"),
    );
}

fn validate_package_artifact(fmu: &BuiltFmu, standard: &FmiStandard) {
    assert_flat_archive(fmu);
    bind_archive_members_to_published_root(fmu);
    let schema = standard.root.join("schema").join(match fmu.version {
        "fmi2" => "fmi2ModelDescription.xsd",
        "fmi3" => "fmi3ModelDescription.xsd",
        other => panic!("unexpected FMI version {other}"),
    });
    validate_xml(&fmu.extracted_root.join("modelDescription.xml"), &schema);
    if fmu.version == "fmi3" {
        validate_xml(
            &fmu.extracted_root.join("sources/buildDescription.xml"),
            &standard.root.join("schema/fmi3BuildDescription.xsd"),
        );
    }
    checked_output(
        Command::new("fmpy").arg("validate").arg(&fmu.archive),
        &format!("validate {} metadata with FMPy", fmu.version),
    );
    checked_output(
        Command::new("bash")
            .arg(&standard.vdm_check)
            .arg(&fmu.archive),
        &format!("validate {} against the FMI VDM model", fmu.version),
    );
    checked_output(
        Command::new("fmpy")
            .arg("compile")
            .arg(&fmu.archive)
            .arg("--all-warnings")
            .arg("--warning-as-error"),
        &format!("build {} packaged sources with FMPy", fmu.version),
    );
    bind_archive_members_to_published_root(fmu);
    assert_validator_rejection_controls(fmu, &schema, &standard.vdm_check);
}

fn assert_exact_unit_derivative_fmi3_archive_inventory(fmu: &BuiltFmu) {
    assert_eq!(fmu.version, "fmi3");
    let bytes = fs::read(&fmu.archive).expect("read UnitDerivative FMI 3 archive");
    check_exact_archive_inventory(&bytes, UNIT_DERIVATIVE_FMI3_ARCHIVE_MEMBERS).unwrap_or_else(
        |error| panic!("UnitDerivative FMI 3 archive inventory mismatch: {error:?}"),
    );
}

fn check_exact_archive_inventory(
    bytes: &[u8],
    expected: &[&str],
) -> Result<(), ArchiveInventoryError> {
    let mut archive = ZipArchive::new(Cursor::new(bytes))
        .map_err(|_| ArchiveInventoryError::UnreadableArchive)?;
    let mut members = Vec::with_capacity(archive.len());
    for index in 0..archive.len() {
        let entry = archive
            .by_index(index)
            .map_err(|_| ArchiveInventoryError::UnreadableMember(index))?;
        members.push((entry.name().to_owned(), entry.is_file()));
    }
    check_archive_member_inventory(&members, expected)
}

fn check_archive_member_inventory(
    members: &[(String, bool)],
    expected: &[&str],
) -> Result<(), ArchiveInventoryError> {
    let mut found = Vec::with_capacity(members.len());
    let mut collision_keys = BTreeMap::<String, String>::new();
    for (name, is_file) in members {
        if !is_file {
            return Err(ArchiveInventoryError::NonFile(name.clone()));
        }
        let collision_key = portable_archive_member_collision_key(name)?;
        if let Some(previous) = collision_keys.insert(collision_key, name.clone()) {
            return if previous == *name {
                Err(ArchiveInventoryError::Duplicate(name.clone()))
            } else {
                Err(ArchiveInventoryError::Confusable {
                    first: previous,
                    second: name.clone(),
                })
            };
        }
        found.push(name.clone());
    }

    if let Some(missing) = expected
        .iter()
        .find(|expected| !found.iter().any(|found| found == **expected))
    {
        return Err(ArchiveInventoryError::Missing((*missing).to_owned()));
    }
    if let Some(extra) = found
        .iter()
        .find(|found| !expected.iter().any(|expected| *found == expected))
    {
        return Err(ArchiveInventoryError::Extra(extra.clone()));
    }
    for (expected, found) in expected.iter().zip(&found) {
        if *expected != found {
            return Err(ArchiveInventoryError::WrongOrder {
                expected: (*expected).to_owned(),
                found: found.clone(),
            });
        }
    }
    Ok(())
}

fn portable_archive_member_collision_key(name: &str) -> Result<String, ArchiveInventoryError> {
    let path_is_noncanonical = name.is_empty()
        || name.starts_with('/')
        || name.contains('\\')
        || !name.is_ascii()
        || name.split('/').any(|component| {
            component.is_empty()
                || component == "."
                || component == ".."
                || component.contains(':')
                || component.bytes().any(|byte| byte.is_ascii_control())
        });
    if path_is_noncanonical {
        return Err(ArchiveInventoryError::NonCanonicalPath(name.to_owned()));
    }
    Ok(name.to_ascii_lowercase())
}

fn check_test_archive_inventory(members: &[&str]) -> Result<(), ArchiveInventoryError> {
    check_test_archive_bytes(&test_archive_bytes(members))
}

fn test_archive_bytes(members: &[&str]) -> Vec<u8> {
    let mut output = ZipWriter::new(Cursor::new(Vec::new()));
    let options = SimpleFileOptions::default()
        .compression_method(zip::CompressionMethod::Deflated)
        .last_modified_time(zip::DateTime::default())
        .unix_permissions(0o644);
    for member in members {
        output
            .start_file(*member, options)
            .expect("write archive-inventory test member");
        output
            .write_all(b"x")
            .expect("write archive-inventory test bytes");
    }
    output
        .finish()
        .expect("finish archive-inventory test zip")
        .into_inner()
}

fn check_test_archive_bytes(bytes: &[u8]) -> Result<(), ArchiveInventoryError> {
    check_exact_archive_inventory(bytes, UNIT_DERIVATIVE_FMI3_ARCHIVE_MEMBERS)
}

fn bind_archive_members_to_published_root(fmu: &BuiltFmu) {
    let required_members: &[&str] = match fmu.version {
        "fmi2" => &["modelDescription.xml", "sources/model.c"],
        "fmi3" => &[
            "modelDescription.xml",
            "sources/buildDescription.xml",
            "sources/model.c",
        ],
        other => panic!("unexpected FMI version {other}"),
    };
    let file = fs::File::open(&fmu.archive).expect("open packaged FMU for byte binding");
    let mut archive = ZipArchive::new(file).expect("read packaged FMU for byte binding");
    for &member in required_members {
        let occurrences = archive.file_names().filter(|name| *name == member).count();
        assert_eq!(
            occurrences, 1,
            "FMU archive contains exactly one `{member}` entry"
        );
        let mut entry = archive
            .by_name(member)
            .unwrap_or_else(|error| panic!("read archived `{member}`: {error}"));
        assert!(!entry.is_dir(), "archived `{member}` is a file");
        let mut archived_bytes = Vec::new();
        entry
            .read_to_end(&mut archived_bytes)
            .unwrap_or_else(|error| panic!("extract archived `{member}`: {error}"));
        let published_bytes = fs::read(fmu.root.join(member))
            .unwrap_or_else(|error| panic!("read published `{member}`: {error}"));
        assert_eq!(
            archived_bytes, published_bytes,
            "archived `{member}` bytes equal the published tree"
        );
        let extracted = fmu.extracted_root.join(member);
        fs::create_dir_all(extracted.parent().expect("archive member has a parent"))
            .expect("create archive-member extraction directory");
        fs::write(&extracted, archived_bytes)
            .unwrap_or_else(|error| panic!("write extracted `{member}`: {error}"));
    }
}

fn validate_xml(xml: &Path, schema: &Path) {
    checked_output(
        Command::new("xmllint")
            .arg("--noout")
            .arg("--schema")
            .arg(schema)
            .arg(xml),
        &format!("validate {} against {}", xml.display(), schema.display()),
    );
}

fn assert_validator_rejection_controls(fmu: &BuiltFmu, schema: &Path, checker: &Path) {
    let work = tempdir().expect("create FMI validator-control directory");
    let invalid_xml = work.path().join("invalid-modelDescription.xml");
    fs::write(&invalid_xml, "<fmiModelDescription/>").expect("write invalid FMI XML control");
    assert_command_failed(
        Command::new("xmllint")
            .arg("--noout")
            .arg("--schema")
            .arg(schema)
            .arg(&invalid_xml),
        "official XSD must reject invalid FMI metadata",
    );

    let duplicate_name = work
        .path()
        .join(format!("{}-duplicate-name.fmu", fmu.version));
    write_duplicate_name_archive(fmu, &duplicate_name);
    assert_command_failed(
        Command::new("fmpy").arg("validate").arg(&duplicate_name),
        "FMPy must reject duplicate FMI variable names",
    );
    assert_command_failed(
        Command::new("bash").arg(checker).arg(&duplicate_name),
        "VDM checker must reject duplicate FMI variable names",
    );
}

fn write_duplicate_name_archive(fmu: &BuiltFmu, output: &Path) {
    let input = fs::File::open(&fmu.archive).expect("open valid FMU control");
    let mut input = ZipArchive::new(input).expect("read valid FMU control");
    let output = fs::File::create(output).expect("create invalid FMU control");
    let mut output = ZipWriter::new(output);
    let options = SimpleFileOptions::default();
    for index in 0..input.len() {
        let mut entry = input.by_index(index).expect("read FMU control entry");
        let name = entry.name().to_owned();
        if entry.is_dir() {
            output
                .add_directory(name, options)
                .expect("copy FMU control directory");
            continue;
        }
        let mut bytes = Vec::new();
        entry
            .read_to_end(&mut bytes)
            .expect("read FMU control bytes");
        if name == "modelDescription.xml" {
            let text = String::from_utf8(bytes).expect("UTF-8 FMI model description");
            let (from, to) = match fmu.version {
                "fmi2" => ("name=\"x[2]\"", "name=\"x[1]\""),
                "fmi3" => ("name=\"der(x)\"", "name=\"x\""),
                other => panic!("unexpected FMI version {other}"),
            };
            let changed = text.replace(from, to);
            assert_ne!(text, changed, "duplicate-name control must alter metadata");
            bytes = changed.into_bytes();
        }
        output
            .start_file(name, options)
            .expect("copy FMU control file");
        output.write_all(&bytes).expect("write FMU control bytes");
    }
    output.finish().expect("finish invalid FMU control");
}

fn assert_command_failed(command: &mut Command, label: &str) {
    let output = command
        .output()
        .unwrap_or_else(|error| panic!("{label}: failed to start command: {error}"));
    assert!(
        !output.status.success(),
        "{label}: validator unexpectedly accepted the negative control"
    );
}

fn assert_flat_archive(fmu: &BuiltFmu) {
    let file = fs::File::open(&fmu.archive).expect("open packaged FMU");
    let mut archive = ZipArchive::new(file).expect("read packaged FMU zip");
    let mut names = Vec::with_capacity(archive.len());
    for index in 0..archive.len() {
        let entry = archive.by_index(index).expect("read packaged FMU entry");
        assert!(
            entry.enclosed_name().is_some(),
            "FMU contains unsafe path: {}",
            entry.name()
        );
        names.push(entry.name().to_string());
    }
    assert!(names.iter().any(|name| name == "modelDescription.xml"));
    assert!(names.iter().any(|name| name == "sources/model.c"));
    let rooted_model_prefix = format!("{}/", fmu.model_name);
    assert!(
        names
            .iter()
            .all(|name| name != &fmu.model_name && !name.starts_with(&rooted_model_prefix)),
        "FMU archive must be flat, found: {names:?}"
    );
}

fn execute_profile(work: &Path, fmu: &BuiltFmu, interface: Interface) -> Vec<[f64; 3]> {
    let csv = work.join(format!("{}-{}.csv", fmu.version, interface.name()));
    checked_output(
        Command::new("fmpy")
            .arg("simulate")
            .arg(&fmu.archive)
            .arg("--validate")
            .arg("--interface-type")
            .arg(interface.name())
            .arg("--start-time")
            .arg("0")
            .arg("--stop-time")
            .arg("1")
            .arg("--output-interval")
            .arg("0.1")
            .arg("--output-file")
            .arg(&csv),
        &format!("execute {} {}", fmu.version, interface.name()),
    );
    parse_trace(&csv, fmu.version)
}

fn parse_trace(csv: &Path, version: &str) -> Vec<[f64; 3]> {
    let text = fs::read_to_string(csv).expect("read FMPy trace");
    let mut lines = text.lines();
    let header = lines.next().expect("FMPy trace header");
    match version {
        "fmi2" => assert_eq!(header, "\"time\",\"x[1]\",\"x[2]\""),
        "fmi3" => assert_eq!(header, "\"time\",\"x\""),
        other => panic!("unexpected FMI version {other}"),
    }
    lines.map(|line| parse_trace_row(line, version)).collect()
}

fn parse_trace_row(line: &str, version: &str) -> [f64; 3] {
    match version {
        "fmi2" => {
            let values = line
                .split(',')
                .map(|value| value.parse::<f64>().expect("numeric FMPy trace value"))
                .collect::<Vec<_>>();
            assert_eq!(values.len(), 3, "unexpected FMPy trace row: {line}");
            [values[0], values[1], values[2]]
        }
        "fmi3" => {
            let (time, tensor) = line.split_once(',').expect("FMI 3 tensor trace row");
            let tensor = tensor
                .split_ascii_whitespace()
                .map(|value| value.parse::<f64>().expect("numeric FMI 3 tensor value"))
                .collect::<Vec<_>>();
            assert_eq!(tensor.len(), 2, "unexpected FMI 3 tensor row: {line}");
            [
                time.parse().expect("numeric FMPy time value"),
                tensor[0],
                tensor[1],
            ]
        }
        other => panic!("unexpected FMI version {other}"),
    }
}

fn assert_trace_matches_analytic_solution(trace: &[[f64; 3]]) {
    assert_eq!(trace.len(), 11);
    for [time, x1, x2] in trace {
        assert_close(*x1, (-0.5 * time).exp(), 5.0e-5);
        assert_close(*x2, (-time).exp(), 5.0e-5);
    }
}

fn assert_trace_close(lhs: &[[f64; 3]], rhs: &[[f64; 3]], tolerance: f64) {
    assert_eq!(lhs.len(), rhs.len());
    for (lhs, rhs) in lhs.iter().zip(rhs) {
        for (left, right) in lhs.iter().zip(rhs) {
            assert_close(*left, *right, tolerance);
        }
    }
}

fn assert_close(actual: f64, expected: f64, tolerance: f64) {
    assert!(
        (actual - expected).abs() <= tolerance,
        "expected {expected:.16e}, found {actual:.16e} (tolerance {tolerance:.3e})"
    );
}

fn checked_output(command: &mut Command, label: &str) -> Output {
    let output = command
        .output()
        .unwrap_or_else(|error| panic!("{label}: failed to start command: {error}"));
    assert!(
        output.status.success(),
        "{label}: command failed with {:?}\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    output
}
