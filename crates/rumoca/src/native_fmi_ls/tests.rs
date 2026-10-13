use super::*;
use rumoca_ir_solve::fmi::FmiCarryPair;

const SOURCE: &str = include_str!("../../tests/fixtures/native_return_predicates.mo");

fn pair(source: &str, name: &str) -> PreparedNativeFmiLsModel {
    let compiled = crate::Compiler::new()
        .model(name)
        .compile_str(source, "publication.mo")
        .unwrap();
    let component = rumoca_sim::lower_fmi_export_component(&compiled.dae).unwrap();
    let root = component.runtime_view().retain_assertion_root();
    let code = rumoca_exec_wasm::compile_retained_native_preparation_wasm(
        root.native_preparation().unwrap(),
    )
    .unwrap();
    let deployment = component
        .into_codegen_view()
        .try_deployment(Default::default())
        .unwrap();
    PreparedNativeFmiLsModel::from_library(
        rumoca_exec_wasm::NativeModelLibrary::from_preparation(code, deployment).unwrap(),
    )
    .unwrap()
}

#[test]
fn native_publication_renders_actual_metadata_without_building_a_c_kernel() {
    let model = pair(SOURCE, "ReturnPredicates");
    let access = FmiNativeDeploymentView::from_preparation(model.library().inputs()).unwrap();
    let prepared = prepare_metadata(&access, "ReturnPredicates").unwrap();
    let xml = std::str::from_utf8(
        prepared
            .rendered_file(Path::new("modelDescription.xml"))
            .unwrap(),
    )
    .unwrap();
    let token = &prepared.artifact().identities["fmu"];
    assert!(xml.contains(&format!("instantiationToken=\"{token}\"")));
    assert!(xml.contains("<Int64 name=\"k\""));
    assert!(xml.contains("<Boolean name=\"first\""));
    assert!(!xml.contains("<ModelExchange "));
    assert!(!xml.contains("canGetAndSetFMUState"));
    assert!(!xml.contains("name=\"projection\""));
    let request = prepared.build_request("component").unwrap();
    assert!(
        request
            .files()
            .keys()
            .all(|path| path.extension().is_some_and(|extension| extension == "xml"))
    );
    assert!(
        access
            .root()
            .shares_root_owner(model.library().inputs().root())
    );
}

#[test]
#[cfg(all(rumoca_qualification, feature = "native-fmi-ls-build"))]
fn native_publication_actual_component_rejects_swapped_request_before_archive_installation() {
    let config = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/verification/native-publication.json");
    let text = std::fs::read_to_string(config)
        .expect("ROOT must provide the fixed-path publication fixture config");
    let config: serde_json::Value = serde_json::from_str(&text).unwrap();
    let output = std::path::PathBuf::from(config["output"].as_str().unwrap())
        .canonicalize()
        .unwrap();
    let scratch = std::path::PathBuf::from(std::env::var_os("HOME").unwrap())
        .join("scratch")
        .canonicalize()
        .unwrap();
    assert!(output.starts_with(&scratch) && output != scratch);
    assert!(output.read_dir().unwrap().next().is_none());
    let source = output.join("fixed-source");
    let target = output.join("fixed-target");
    std::fs::create_dir(&source).unwrap();
    std::fs::create_dir(&target).unwrap();
    let core = rumoca_exec_wasm::build_fixed_native_guest_core(&source, &target).unwrap();
    let product = NativeFmiLsPackage::construct(
        pair(SOURCE, "ReturnPredicates"),
        core,
        "ReturnPredicates",
        &[],
    )
    .unwrap();
    // The positive whole graph must exist before testing a foreign XML request.
    assert!(!product.component_bytes().is_empty());
    let foreign_source = SOURCE.replace("ReturnPredicates", "ForeignPublication");
    let foreign = pair(&foreign_source, "ForeignPublication");
    let access = FmiNativeDeploymentView::from_preparation(foreign.library().inputs()).unwrap();
    let request = prepare_metadata(&access, "ForeignPublication").unwrap();
    let error = check_correlation(&request, &product.component, &access).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("different publication inventory")
    );
    // Even the original request cannot attach a separately replayed inventory.
    let error = check_correlation(&product.package, &product.component, &access).unwrap_err();
    assert!(error.to_string().contains("different emitted source"));
    let xml = product.model_description().to_vec();
    let wasm = product.component_bytes().to_vec();
    let token = product.artifact().identities["fmu"].clone();
    product.publish(&output).unwrap();
    let file = std::fs::File::open(output.join("ReturnPredicates.fmu")).unwrap();
    let mut archive = zip::ZipArchive::new(file).unwrap();
    use std::io::Read;
    let mut actual = Vec::new();
    archive
        .by_name("modelDescription.xml")
        .unwrap()
        .read_to_end(&mut actual)
        .unwrap();
    assert_eq!(actual, xml);
    actual.clear();
    archive
        .by_name(&format!(
            "binaries/wasm32-wasip2/Rumoca_{}.wasm",
            token.replace('-', "_")
        ))
        .unwrap()
        .read_to_end(&mut actual)
        .unwrap();
    assert_eq!(actual, wasm);
}

const PARAMETERS: &str = "
model ParameterPublication
  parameter Integer n = 3;
  parameter Boolean enabled = true;
  parameter Real gain = 2.5;
  constant Integer capacity = 4;
  final parameter Integer rows = capacity * 2;
  input Real u = 1;
  output Real y;
equation
  y = if enabled then gain * u * n else 0;
end ParameterPublication;
";

#[test]
fn native_publication_declares_every_parameter_and_constant_the_model_publishes() {
    let model = pair(PARAMETERS, "ParameterPublication");
    let access = FmiNativeDeploymentView::from_preparation(model.library().inputs())
        .expect("every published parameter and constant is covered by the typed inventory");
    let prepared = prepare_metadata(&access, "ParameterPublication").unwrap();
    let xml = std::str::from_utf8(
        prepared
            .rendered_file(Path::new("modelDescription.xml"))
            .unwrap(),
    )
    .unwrap();
    for declaration in [
        "<Int64 name=\"n\"",
        "<Boolean name=\"enabled\"",
        "<Float64 name=\"gain\"",
        "<Int64 name=\"capacity\"",
        "<Int64 name=\"rows\"",
    ] {
        assert!(xml.contains(declaration), "{declaration} in {xml}");
    }
    assert!(xml.contains("causality=\"parameter\""));
    assert!(xml.contains("causality=\"calculatedParameter\""));
}

const STEPPER: &str = r#"
model Stepper
  input Real previous[3];
  input Integer count = 0;
  output Real next[3];
  output Real total;
equation
  next = 2 * previous;
  total = sum(previous);
end Stepper;
"#;

fn reference(access: &FmiNativeDeploymentView, name: &str) -> u32 {
    access
        .variables()
        .iter()
        .find(|variable| variable.name() == name)
        .unwrap()
        .value_reference_fmi3()
}

#[test]
fn a_declared_carry_is_recorded_in_the_model_description_by_value_reference() {
    let model = pair(STEPPER, "Stepper");
    let inputs = model.library().inputs();
    let access = FmiNativeDeploymentView::from_preparation(inputs).unwrap();
    let declared = [FmiCarryPair {
        output: reference(&access, "next"),
        input: reference(&access, "previous"),
    }];
    let (access, state) = access
        .declaring_carried(inputs.schedule(), &declared)
        .unwrap();
    assert_eq!(state.runs().len(), 1);
    assert_eq!(access.carried(), declared);
    let prepared = prepare_metadata(&access, "Stepper").unwrap();
    let xml = std::str::from_utf8(
        prepared
            .rendered_file(Path::new("modelDescription.xml"))
            .unwrap(),
    )
    .unwrap();
    assert!(xml.contains(&format!(
        "<Carry output=\"{}\" input=\"{}\"/>",
        declared[0].output, declared[0].input
    )));
    let plain = FmiNativeDeploymentView::from_preparation(inputs).unwrap();
    let xml = prepare_metadata(&plain, "Stepper").unwrap();
    let xml = std::str::from_utf8(
        xml.rendered_file(Path::new("modelDescription.xml"))
            .unwrap(),
    )
    .unwrap()
    .to_owned();
    assert!(!xml.contains("Carry"));
}

#[test]
fn a_carry_declaration_is_refused_unless_an_output_feeds_an_input_of_the_same_type_and_shape() {
    let model = pair(STEPPER, "Stepper");
    let inputs = model.library().inputs();
    let access = FmiNativeDeploymentView::from_preparation(inputs).unwrap();
    let (next, total, previous) = (
        reference(&access, "next"),
        reference(&access, "total"),
        reference(&access, "previous"),
    );
    let refused = |output: u32, input: u32| {
        access
            .clone()
            .declaring_carried(inputs.schedule(), &[FmiCarryPair { output, input }])
            .map(|_| ())
            .unwrap_err()
            .to_string()
    };
    assert!(refused(next, 9999).contains("names no published variable"));
    assert!(refused(previous, previous).contains("is not a model output"));
    assert!(refused(next, next).contains("is not a model input"));
    assert!(refused(total, previous).contains("differs in value type or dimensions"));
    assert!(refused(next, reference(&access, "count")).contains("differs in value type"));
}

const COUNTER: &str = r#"
model Counter
  input Integer count;
  input Real level;
  output Integer total;
equation
  total = count;
end Counter;
"#;

#[test]
fn an_integer_carry_is_declared_between_a_typed_output_and_its_input_lane() {
    let model = pair(COUNTER, "Counter");
    let inputs = model.library().inputs();
    let access = FmiNativeDeploymentView::from_preparation(inputs).unwrap();
    let (total, count, level) = (
        reference(&access, "total"),
        reference(&access, "count"),
        reference(&access, "level"),
    );
    let declared = [FmiCarryPair {
        output: total,
        input: count,
    }];
    let (access, state) = access
        .clone()
        .declaring_carried(inputs.schedule(), &declared)
        .unwrap();
    assert_eq!(state.lanes().len(), 1);
    assert_eq!(state.lanes()[0].bytes(), 8);
    assert!(state.runs().is_empty());
    let xml = prepare_metadata(&access, "Counter").unwrap();
    let xml = std::str::from_utf8(
        xml.rendered_file(Path::new("modelDescription.xml"))
            .unwrap(),
    )
    .unwrap()
    .to_owned();
    assert!(xml.contains(&format!("<Carry output=\"{total}\" input=\"{count}\"/>")));
    let refused = FmiNativeDeploymentView::from_preparation(inputs)
        .unwrap()
        .declaring_carried(
            inputs.schedule(),
            &[FmiCarryPair {
                output: total,
                input: level,
            }],
        )
        .map(|_| ())
        .unwrap_err()
        .to_string();
    assert!(refused.contains("differs in value type"), "{refused}");
}
