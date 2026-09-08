//! End-to-end proof for the correlated `efmu` product.

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::io::Read as _;
use std::path::{Path, PathBuf};
use std::process::Command;

use tempfile::tempdir;

use super::cli_support::{run_compile_target, sha1_hex, write_fixture};
use super::container_xml_support::{
    assert_xsd_rejects, attribute_values, element_attribute_maps, element_attribute_maps_bytes,
    ids_inside_wrapper, relative_file_paths, root_id, sole_attribute_value,
    sole_element_attributes, surgically, validate_against_xsd, vendored_schemas_dir, without_block,
    without_line,
};

const MODEL: &str = "EfmuScalarLifecycle";
const FIXTURE: &str = "\
model EfmuScalarLifecycle
  constant Real samplePeriod = 0.1;
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = 1.25;
  end when;
end EfmuScalarLifecycle;
";

struct BuiltContainer {
    root: PathBuf,
    archive: PathBuf,
}

impl BuiltContainer {
    fn content(&self) -> PathBuf {
        self.root.join("__content.xml")
    }

    fn ac_manifest(&self) -> PathBuf {
        self.root.join("AlgorithmCode/manifest.xml")
    }

    fn algorithm_code(&self) -> PathBuf {
        let file = sole_element_attributes(&self.ac_manifest(), "File");
        self.root
            .join("AlgorithmCode")
            .join(file.get("name").expect("Algorithm Code filename"))
    }

    fn pc_manifest(&self) -> PathBuf {
        self.root.join("ProductionCode/manifest.xml")
    }

    fn header(&self) -> PathBuf {
        self.production_file(".h")
    }

    fn source(&self) -> PathBuf {
        self.production_file(".c")
    }

    fn production_file(&self, suffix: &str) -> PathBuf {
        let file = element_attribute_maps(&self.pc_manifest(), "File")
            .into_iter()
            .find(|attributes| {
                attributes
                    .get("name")
                    .is_some_and(|name| name.ends_with(suffix))
            })
            .unwrap_or_else(|| panic!("missing Production Code `{suffix}` file"));
        let directory = file
            .get("path")
            .and_then(|path| path.strip_prefix("./"))
            .expect("relative eFMI file path");
        self.root
            .join("ProductionCode")
            .join(directory)
            .join(file.get("name").expect("Production Code file name"))
    }
}

fn build_container(work: &Path) -> BuiltContainer {
    let output_root = work.join("out");
    let source = write_fixture(work, MODEL, FIXTURE);
    let output = run_compile_target(&source, "efmu", &output_root);
    assert!(
        output.status.success(),
        "`compile --target efmu` failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    BuiltContainer {
        root: output_root.join(MODEL),
        archive: output_root.join(format!("{MODEL}.efmu")),
    }
}

#[test]
fn efmu_is_a_schema_valid_two_representation_container() {
    let directory = tempdir().expect("tempdir");
    let container = build_container(directory.path());
    let root_entries = fs::read_dir(&container.root)
        .expect("read container root")
        .map(|entry| entry.expect("directory entry").file_name())
        .map(|name| name.into_string().expect("UTF-8 name"))
        .collect::<BTreeSet<_>>();
    assert_eq!(
        root_entries,
        [
            "__content.xml",
            "schemas",
            "AlgorithmCode",
            "ProductionCode"
        ]
        .into_iter()
        .map(str::to_owned)
        .collect()
    );
    assert_eq!(
        relative_file_paths(&container.root.join("AlgorithmCode")),
        [
            container
                .algorithm_code()
                .file_name()
                .expect("Algorithm Code filename")
                .to_string_lossy()
                .into_owned(),
            "manifest.xml".to_owned(),
        ]
        .into_iter()
        .collect()
    );
    assert_eq!(
        relative_file_paths(&container.root.join("ProductionCode")),
        [
            format!(
                "sources/{}",
                container
                    .source()
                    .file_name()
                    .expect("source name")
                    .to_string_lossy()
            ),
            format!(
                "sources/{}",
                container
                    .header()
                    .file_name()
                    .expect("header name")
                    .to_string_lossy()
            ),
            "manifest.xml".to_owned(),
        ]
        .into_iter()
        .collect()
    );

    let schemas = vendored_schemas_dir("efmu");
    assert_eq!(
        relative_file_paths(&container.root.join("schemas")),
        relative_file_paths(&schemas),
        "borrowed schema tree must be complete"
    );
    for relative in relative_file_paths(&schemas) {
        assert_eq!(
            fs::read(container.root.join("schemas").join(&relative)).expect("read shipped schema"),
            fs::read(schemas.join(&relative)).expect("read vendored schema"),
            "borrowed schema bytes drifted for {relative}"
        );
    }
    for (xml, xsd) in [
        (
            container.content(),
            schemas.join("efmiContainerManifest.xsd"),
        ),
        (
            container.ac_manifest(),
            schemas.join("AlgorithmCode/efmiAlgorithmCodeManifest.xsd"),
        ),
        (
            container.pc_manifest(),
            schemas.join("ProductionCode/efmiProductionCodeManifest.xsd"),
        ),
    ] {
        validate_against_xsd(&xml, &xsd)
            .unwrap_or_else(|error| panic!("{}: {error}", xml.display()));
        let ids = attribute_values(&xml, "id");
        assert_eq!(
            ids.len(),
            ids.iter().collect::<BTreeSet<_>>().len(),
            "all local identifiers in {} must be unique",
            xml.display()
        );
    }
    let manifest_ids = [
        root_id(&container.content()),
        root_id(&container.ac_manifest()),
        root_id(&container.pc_manifest()),
    ];
    assert_eq!(manifest_ids.iter().collect::<BTreeSet<_>>().len(), 3);
}

#[test]
fn complete_checksum_web_recomputes_from_exact_container_bytes() {
    let directory = tempdir().expect("tempdir");
    let container = build_container(directory.path());
    let ac_bytes = fs::read(container.ac_manifest()).expect("read AC manifest");
    let pc_bytes = fs::read(container.pc_manifest()).expect("read PC manifest");
    let representations = element_attribute_maps(&container.content(), "ModelRepresentation");
    assert_eq!(representations.len(), 2);
    for (name, manifest, bytes) in [
        (
            "AlgorithmCode",
            container.ac_manifest(),
            ac_bytes.as_slice(),
        ),
        (
            "ProductionCode",
            container.pc_manifest(),
            pc_bytes.as_slice(),
        ),
    ] {
        let entry = representations
            .iter()
            .find(|attributes| attributes.get("name").map(String::as_str) == Some(name))
            .unwrap_or_else(|| panic!("missing {name} representation"));
        assert_eq!(entry.get("checksum"), Some(&sha1_hex(bytes)));
        assert_eq!(entry.get("manifestRefId"), Some(&root_id(&manifest)));
    }

    let alg = fs::read(container.algorithm_code()).expect("read .alg");
    assert_eq!(
        sole_attribute_value(&container.ac_manifest(), "checksum"),
        sha1_hex(&alg)
    );
    let pc_files = element_attribute_maps(&container.pc_manifest(), "File");
    for path in [container.header(), container.source()] {
        let name = path
            .file_name()
            .expect("Production Code filename")
            .to_string_lossy()
            .into_owned();
        let entry = pc_files
            .iter()
            .find(|attributes| attributes.get("name") == Some(&name))
            .unwrap_or_else(|| panic!("missing PC File for {name}"));
        assert_eq!(entry.get("path").map(String::as_str), Some("./sources/"));
        let bytes = fs::read(path).expect("read Production Code file");
        assert_eq!(entry.get("checksum"), Some(&sha1_hex(&bytes)));
    }
    let reference = sole_element_attributes(&container.pc_manifest(), "ManifestReference");
    assert_eq!(reference.get("checksum"), Some(&sha1_hex(&ac_bytes)));
    assert_eq!(
        reference.get("manifestRefId"),
        Some(&root_id(&container.ac_manifest()))
    );
}

#[test]
fn logical_data_is_total_and_all_cross_references_resolve() {
    let directory = tempdir().expect("tempdir");
    let container = build_container(directory.path());
    let anchor = sole_element_attributes(&container.pc_manifest(), "ManifestReference");
    let anchor_id = anchor.get("id").expect("ManifestReference id");
    let mut foreign_variables =
        element_attribute_maps(&container.pc_manifest(), "ForeignVariableReference");
    let mut foreign_functions =
        element_attribute_maps(&container.pc_manifest(), "ForeignFunctionReference");
    for reference in foreign_variables.iter().chain(&foreign_functions) {
        assert_eq!(reference.get("manifestReferenceRefId"), Some(anchor_id));
    }

    let mut expected_variables = ids_inside_wrapper(&container.ac_manifest(), "Variables");
    expected_variables.push(
        sole_element_attributes(&container.ac_manifest(), "ErrorSignalStatus")
            .remove("id")
            .expect("ErrorSignalStatus id"),
    );
    let mut actual_variables = foreign_variables
        .iter_mut()
        .map(|reference| reference.remove("foreignRefId").expect("foreignRefId"))
        .collect::<Vec<_>>();
    expected_variables.sort();
    actual_variables.sort();
    assert_eq!(actual_variables, expected_variables);

    let mut expected_methods = ids_inside_wrapper(&container.ac_manifest(), "BlockMethods");
    let mut actual_methods = foreign_functions
        .iter_mut()
        .map(|reference| reference.remove("foreignRefId").expect("foreignRefId"))
        .collect::<Vec<_>>();
    expected_methods.sort();
    actual_methods.sort();
    assert_eq!(actual_methods, expected_methods);

    let local_parameters = element_attribute_maps(&container.pc_manifest(), "FormalParameter")
        .into_iter()
        .filter_map(|mut attributes| attributes.remove("id"))
        .collect::<BTreeSet<_>>();
    let component_names = element_attribute_maps(&container.pc_manifest(), "Component")
        .into_iter()
        .filter_map(|mut attributes| attributes.remove("name"))
        .collect::<BTreeSet<_>>();
    for mapping in element_attribute_maps(&container.pc_manifest(), "FormalParameter") {
        if let Some(reference) = mapping.get("formalParameterRefId") {
            assert!(local_parameters.contains(reference));
        }
        if let Some(component) = mapping.get("componentIdentifier") {
            assert!(component_names.contains(component));
        }
    }
    let data_parameter_anchors =
        element_attribute_maps(&container.pc_manifest(), "FormalParameter")
            .into_iter()
            .filter_map(|mut attributes| attributes.remove("formalParameterRefId"))
            .collect::<BTreeSet<_>>();
    assert_eq!(
        data_parameter_anchors.len(),
        1,
        "all LogicalData components must share the construction-issued storage anchor"
    );
    let local_functions = element_attribute_maps(&container.pc_manifest(), "Function")
        .into_iter()
        .filter_map(|mut attributes| attributes.remove("id"))
        .collect::<BTreeSet<_>>();
    for mapping in element_attribute_maps(&container.pc_manifest(), "GlobalFunction") {
        assert!(local_functions.contains(mapping.get("functionRefId").expect("functionRefId")));
    }
}

struct ProductionAbi {
    storage_type: String,
    status_ok: String,
    startup: String,
    do_step: String,
    output_component: String,
    error_status_component: String,
}

fn production_abi(container: &BuiltContainer) -> ProductionAbi {
    let variables = ["RealVariable", "IntegerVariable", "BooleanVariable"]
        .into_iter()
        .flat_map(|kind| element_attribute_maps(&container.ac_manifest(), kind))
        .collect::<Vec<_>>();
    let output_id = variables
        .iter()
        .find(|attributes| attributes.get("name").map(String::as_str) == Some("y"))
        .and_then(|attributes| attributes.get("id"))
        .expect("fixture output variable id");
    let foreign_variables =
        element_attribute_maps(&container.pc_manifest(), "ForeignVariableReference");
    let data_arms = element_attribute_maps(&container.pc_manifest(), "FormalParameter")
        .into_iter()
        .filter(|attributes| attributes.contains_key("formalParameterRefId"))
        .collect::<Vec<_>>();
    assert_eq!(foreign_variables.len(), data_arms.len());
    let mapped_component = |foreign_id: &str| {
        foreign_variables
            .iter()
            .zip(&data_arms)
            .find(|(foreign, _)| {
                foreign.get("foreignRefId").map(String::as_str) == Some(foreign_id)
            })
            .and_then(|(_, arm)| arm.get("componentIdentifier"))
            .unwrap_or_else(|| panic!("missing component mapping for `{foreign_id}`"))
            .clone()
    };
    let output_component = mapped_component(output_id);
    let error_status_id = sole_element_attributes(&container.ac_manifest(), "ErrorSignalStatus")
        .remove("id")
        .expect("ErrorSignalStatus id");
    let error_status_component = mapped_component(&error_status_id);

    let block_methods = element_attribute_maps(&container.ac_manifest(), "BlockMethod");
    let foreign_methods =
        element_attribute_maps(&container.pc_manifest(), "ForeignFunctionReference");
    let global_functions = element_attribute_maps(&container.pc_manifest(), "GlobalFunction");
    assert_eq!(foreign_methods.len(), global_functions.len());
    let functions = element_attribute_maps(&container.pc_manifest(), "Function");
    let mapped_function = |kind: &str| {
        let foreign_id = block_methods
            .iter()
            .find(|attributes| attributes.get("kind").map(String::as_str) == Some(kind))
            .and_then(|attributes| attributes.get("id"))
            .unwrap_or_else(|| panic!("missing {kind} BlockMethod"));
        let function_id = foreign_methods
            .iter()
            .zip(&global_functions)
            .find(|(foreign, _)| foreign.get("foreignRefId") == Some(foreign_id))
            .and_then(|(_, local)| local.get("functionRefId"))
            .unwrap_or_else(|| panic!("missing {kind} function mapping"));
        functions
            .iter()
            .find(|attributes| attributes.get("id") == Some(function_id))
            .and_then(|attributes| attributes.get("name"))
            .unwrap_or_else(|| panic!("missing {kind} C function"))
            .clone()
    };
    let formal = element_attribute_maps(&container.pc_manifest(), "FormalParameter")
        .into_iter()
        .find(|attributes| attributes.contains_key("id"))
        .expect("lifecycle self formal declaration");
    let storage_typedef = formal.get("typeDefRefId").expect("self formal type");
    let storage_type = element_attribute_maps(&container.pc_manifest(), "Typedef")
        .into_iter()
        .find(|attributes| attributes.get("id") == Some(storage_typedef))
        .and_then(|attributes| attributes.get("name").cloned())
        .expect("storage typedef name");
    let status_ok = element_attribute_maps(&container.pc_manifest(), "Variable")
        .into_iter()
        .find(|attributes| {
            attributes.get("value").map(String::as_str) == Some("0")
                && attributes.get("const").map(String::as_str) == Some("true")
                && attributes.get("static").map(String::as_str) == Some("true")
        })
        .and_then(|attributes| attributes.get("name").cloned())
        .expect("manifested success status constant");
    ProductionAbi {
        storage_type,
        status_ok,
        startup: mapped_function("Startup"),
        do_step: mapped_function("DoStep"),
        output_component,
        error_status_component,
    }
}

#[test]
fn generated_c_implements_the_mapped_startup_and_do_step() {
    let directory = tempdir().expect("tempdir");
    let container = build_container(directory.path());
    let abi = production_abi(&container);
    let driver = directory.path().join("driver.c");
    let executable = directory.path().join("efmu-probe");
    let header_name = container
        .header()
        .file_name()
        .expect("Production Code header filename")
        .to_string_lossy()
        .into_owned();
    fs::write(
        &driver,
        format!(
            "#include \"{header}\"\nint main(void) {{\n    {storage} self;\n    self.{error} = 1;\n    if ({startup}(&self) != {status_ok}) return 2;\n    if (self.{error} != 0) return 3;\n    self.{error} = 1;\n    if ({do_step}(&self) != {status_ok}) return 4;\n    if (self.{error} != 0) return 5;\n    return self.{output} == 1.25F ? 0 : 1;\n}}\n",
            header = header_name,
            storage = abi.storage_type,
            status_ok = abi.status_ok,
            startup = abi.startup,
            do_step = abi.do_step,
            output = abi.output_component,
            error = abi.error_status_component,
        ),
    )
    .expect("write driver");
    let output = Command::new("cc")
        .args(["-std=c99", "-Wall", "-Wextra", "-Werror"])
        .arg(&driver)
        .arg(container.source())
        .arg("-I")
        .arg(container.root.join("ProductionCode/sources"))
        .arg("-o")
        .arg(&executable)
        .output()
        .expect("invoke cc");
    assert!(
        output.status.success(),
        "cc failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        Command::new(executable)
            .status()
            .expect("run probe")
            .success()
    );
}

#[test]
fn malformed_production_manifests_are_rejected_by_the_vendored_xsd() {
    let directory = tempdir().expect("tempdir");
    let container = build_container(directory.path());
    let manifest = fs::read_to_string(container.pc_manifest()).expect("read PC manifest");
    let xsd = vendored_schemas_dir("efmu").join("ProductionCode/efmiProductionCodeManifest.xsd");
    assert_xsd_rejects(
        "bad language",
        &surgically(&manifest, "language=\"C\"", "language=\"Rust\""),
        &xsd,
    );
    assert_xsd_rejects(
        "missing target",
        &without_line(&manifest, "<Target>Generic</Target>"),
        &xsd,
    );
    assert_xsd_rejects(
        "missing logical data",
        &without_block(&manifest, "<LogicalData>", "</LogicalData>"),
        &xsd,
    );
}

fn sole_archive_element(
    entries: &BTreeMap<String, Vec<u8>>,
    member: &str,
    element: &str,
) -> BTreeMap<String, String> {
    let bytes = entries
        .get(member)
        .unwrap_or_else(|| panic!("missing archive member `{member}`"));
    let mut maps = element_attribute_maps_bytes(bytes, element);
    assert_eq!(maps.len(), 1, "expected one `{element}` in `{member}`");
    maps.pop().expect("length checked")
}

fn assert_archive_checksum_web(entries: &BTreeMap<String, Vec<u8>>) {
    let content_member = "__content.xml";
    let content = entries.get(content_member).expect("root content member");
    let representations = element_attribute_maps_bytes(content, "ModelRepresentation");
    for name in ["AlgorithmCode", "ProductionCode"] {
        let representation = representations
            .iter()
            .find(|attributes| attributes.get("name").map(String::as_str) == Some(name))
            .unwrap_or_else(|| panic!("missing archived {name} representation"));
        assert_eq!(
            representation.get("manifest").map(String::as_str),
            Some("manifest.xml")
        );
        let member = format!("{name}/manifest.xml");
        let bytes = entries
            .get(&member)
            .expect("representation manifest member");
        assert_eq!(representation.get("checksum"), Some(&sha1_hex(bytes)));
        assert_eq!(
            representation.get("manifestRefId"),
            sole_archive_element(entries, &member, "Manifest").get("id")
        );
    }

    let ac_member = "AlgorithmCode/manifest.xml";
    let ac_file = sole_archive_element(entries, ac_member, "File");
    assert_eq!(ac_file.get("path").map(String::as_str), Some("./"));
    let alg_member = format!(
        "AlgorithmCode/{}",
        ac_file.get("name").expect("Algorithm Code filename")
    );
    assert_eq!(
        ac_file.get("checksum"),
        Some(&sha1_hex(
            entries.get(&alg_member).expect("Algorithm Code member")
        ))
    );

    let pc_member = "ProductionCode/manifest.xml";
    let pc = entries
        .get(pc_member)
        .expect("Production Code manifest member");
    for file in element_attribute_maps_bytes(pc, "File") {
        assert_eq!(file.get("path").map(String::as_str), Some("./sources/"));
        let member = format!(
            "ProductionCode/sources/{}",
            file.get("name").expect("Production Code filename")
        );
        assert_eq!(
            file.get("checksum"),
            Some(&sha1_hex(
                entries.get(&member).expect("Production Code member")
            ))
        );
    }
    let reference = sole_archive_element(entries, pc_member, "ManifestReference");
    let ac = entries
        .get(ac_member)
        .expect("Algorithm Code manifest member");
    assert_eq!(reference.get("checksum"), Some(&sha1_hex(ac)));
    assert_eq!(
        reference.get("manifestRefId"),
        sole_archive_element(entries, ac_member, "Manifest").get("id")
    );
}

#[test]
fn flat_efmu_archive_is_byte_identical_to_the_directory_form() {
    let directory = tempdir().expect("tempdir");
    let container = build_container(directory.path());
    let file = fs::File::open(&container.archive).expect("open .efmu");
    let mut archive = zip::ZipArchive::new(file).expect("read .efmu zip");
    let mut entries = BTreeMap::new();
    for index in 0..archive.len() {
        let mut entry = archive.by_index(index).expect("read zip entry");
        assert!(entry.is_file());
        let mut bytes = Vec::new();
        entry.read_to_end(&mut bytes).expect("read zip bytes");
        let previous = entries.insert(entry.name().to_owned(), bytes);
        assert!(
            previous.is_none(),
            "flat eFMU archive must not contain duplicate member names"
        );
    }
    assert!(entries.contains_key("__content.xml"));
    assert_eq!(
        entries.keys().cloned().collect::<BTreeSet<_>>(),
        relative_file_paths(&container.root)
    );
    assert_archive_checksum_web(&entries);
    for (relative, bytes) in entries {
        assert_eq!(
            bytes,
            fs::read(container.root.join(relative)).expect("read file")
        );
    }
}
