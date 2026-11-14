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

fn scalar_root(source: &str, name: &str) -> rumoca_ir_solve::RetainedAssertionRoot {
    let compiled = crate::Compiler::new()
        .model(name)
        .compile_str(source, "scalar.mo")
        .unwrap();
    rumoca_sim::lower_fmi_export_component(&compiled.dae)
        .unwrap()
        .runtime_view()
        .retain_assertion_root()
}

/// A stateful model renders a Co-Simulation-only description through the
/// scalar view: its state, the state's derivative and its VRs survive, and no
/// Model Exchange, FMU state, directional derivatives, event mode or event
/// indicator is advertised.
#[test]
fn a_stateful_model_renders_a_co_simulation_only_scalar_description() {
    let root = scalar_root(
        "model Decay\n  Real x(start = 1);\nequation\n  der(x) = -x;\nend Decay;\n",
        "Decay",
    );
    assert!(root.native_preparation().is_err());
    let view = rumoca_ir_solve::fmi::FmiScalarDeploymentView::from_root(&root).unwrap();
    let prepared = prepare_metadata_with(
        NativeFmiTemplateRenderer::new_scalar(view).unwrap(),
        "Decay",
    )
    .unwrap();
    let xml = std::str::from_utf8(
        prepared
            .rendered_file(Path::new("modelDescription.xml"))
            .unwrap(),
    )
    .unwrap();
    assert!(xml.contains("<CoSimulation "));
    assert!(xml.contains("<Float64 name=\"x\""));
    assert!(xml.contains("name=\"der(x)\""));
    assert!(xml.contains("<ContinuousStateDerivative "));
    for absent in [
        "<ModelExchange ",
        "canGetAndSetFMUState",
        "providesDirectionalDerivatives",
        "hasEventMode=\"true\"",
        "<EventIndicator ",
    ] {
        assert!(
            !xml.contains(absent),
            "scalar description advertises {absent}"
        );
    }
}

/// The scalar route owns Real, Integer and Boolean variables: a stateful model
/// with an Integer parameter, a Boolean input and a Boolean output renders
/// each under its own FMI 3 type with its own start, and advertises those
/// kinds.
#[test]
fn integer_and_boolean_variables_render_in_the_scalar_view() {
    let root = scalar_root(
        "model Selector\n  parameter Integer n = 3;\n  input Boolean enable = true;\n  \
         output Boolean active;\n  Real x(start = 1, fixed = true);\nequation\n  \
         der(x) = if enable then -n * x else 0;\n  active = x > 0.5;\nend Selector;\n",
        "Selector",
    );
    let view = rumoca_ir_solve::fmi::FmiScalarDeploymentView::from_root(&root).unwrap();
    assert_eq!(
        view.capabilities().variable_kinds,
        [
            rumoca_ir_solve::SolveVariableValueKind::Real,
            rumoca_ir_solve::SolveVariableValueKind::Integer,
            rumoca_ir_solve::SolveVariableValueKind::Boolean,
        ]
    );
    let prepared = prepare_metadata_with(
        NativeFmiTemplateRenderer::new_scalar(view).unwrap(),
        "Selector",
    )
    .unwrap();
    let xml = std::str::from_utf8(
        prepared
            .rendered_file(Path::new("modelDescription.xml"))
            .unwrap(),
    )
    .unwrap();
    for present in [
        "<Int64 name=\"n\"",
        "<Boolean name=\"enable\"",
        "<Boolean name=\"active\"",
        "start=\"true\"",
    ] {
        assert!(xml.contains(present), "scalar description lacks {present}");
    }
}

/// The scalar route also owns enumerations, served as FMI 3 Int64 ordinals
/// under their declared type, and String parameters with a literal start: a
/// stateful model with both renders each with its start and advertises both
/// kinds. A String without a literal start has no value the route could serve
/// (String values are never evaluated), so it is refused by name before any
/// description is rendered, never hidden.
#[test]
fn enumeration_and_literal_string_parameters_render_in_the_scalar_view() {
    let root = scalar_root(
        "model Labelled\n  type Mode = enumeration(off, slow, fast);\n  \
         parameter Mode mode = Mode.slow;\n  parameter String label = \"a<b\";\n  \
         Real x(start = 1, fixed = true);\nequation\n  \
         der(x) = if mode == Mode.fast then -2 * x else -x;\nend Labelled;\n",
        "Labelled",
    );
    let view = rumoca_ir_solve::fmi::FmiScalarDeploymentView::from_root(&root).unwrap();
    assert_eq!(
        view.capabilities().variable_kinds,
        [
            rumoca_ir_solve::SolveVariableValueKind::Real,
            rumoca_ir_solve::SolveVariableValueKind::Enumeration,
            rumoca_ir_solve::SolveVariableValueKind::String,
        ]
    );
    let prepared = prepare_metadata_with(
        NativeFmiTemplateRenderer::new_scalar(view).unwrap(),
        "Labelled",
    )
    .unwrap();
    let xml = std::str::from_utf8(
        prepared
            .rendered_file(Path::new("modelDescription.xml"))
            .unwrap(),
    )
    .unwrap();
    for present in [
        "<Item name=\"slow\" value=\"2\"/>",
        "<Enumeration name=\"mode\"",
        "start=\"2\"",
        "<String name=\"label\"",
        "<Start value=\"a&lt;b\"/>",
    ] {
        assert!(xml.contains(present), "scalar description lacks {present}");
    }

    let computed = scalar_root(
        "model Computed\n  parameter Integer n = 3;\n  \
         parameter String label = if n > 2 then \"many\" else \"few\";\n  \
         Real x(start = 1, fixed = true);\nequation\n  der(x) = -n * x;\nend Computed;\n",
        "Computed",
    );
    let error = rumoca_ir_solve::fmi::FmiScalarDeploymentView::from_root(&computed)
        .err()
        .expect("a String without a literal start has no value to serve");
    assert!(error.to_string().contains("(label) is not one"), "{error}");
}

/// Scalar preparation admits exactly the complement of the native profile:
/// a stateless Real-only model the native profile admits is refused as the
/// wrong product (the scalar guest would refuse it at instantiation), and a
/// stateful model is admitted.
#[test]
fn scalar_preparation_refuses_a_native_admissible_root_and_admits_a_stateful_one() {
    let lower = |source: &str, name: &str| {
        let compiled = crate::Compiler::new()
            .model(name)
            .compile_str(source, "kind.mo")
            .unwrap();
        rumoca_sim::lower_fmi_export_component(&compiled.dae).unwrap()
    };
    let stateless = lower(
        "model Gain\n  input Real u;\n  output Real y;\nequation\n  y = 2 * u;\nend Gain;\n",
        "Gain",
    );
    assert!(
        stateless
            .runtime_view()
            .retain_assertion_root()
            .native_preparation()
            .is_ok(),
        "the fixture is native-admissible"
    );
    assert!(matches!(
        rumoca_sim::PreparedScalarFmiLsModel::from_component(&stateless),
        Err(rumoca_sim::NativeFmiLsPreparationError::WrongProduct(_))
    ));
    let stateful = lower(
        "model Decay\n  Real x(start = 1);\nequation\n  der(x) = -x;\nend Decay;\n",
        "Decay",
    );
    assert!(rumoca_sim::PreparedScalarFmiLsModel::from_component(&stateful).is_ok());
}

const PAGE_OPTIONS: &str = r#"{"dt":0.002,"solver":"fixed-step","atol":1e-8,"rtol":1e-6,
    "initial_inputs":[],"execution_policy":"auto","fixed_step":{"step":0.001,"order":2},
    "parameters":{"k":2.0}}"#;

fn decay(options: &ScalarPackageOptions) -> anyhow::Result<rumoca_ir_solve::fmi::FmiComponent> {
    let compiled = crate::Compiler::new()
        .model("Decay")
        .compile_str(
            "model Decay\n  parameter Real k = 1;\n  Real x(start = 1, fixed = true);\n\
             equation\n  der(x) = -k * x;\nend Decay;\n",
            "decay.mo",
        )
        .unwrap();
    Ok(rumoca_sim::lower_fmi_scalar_export_component(
        &compiled.dae,
        options.sim_options(),
    )?)
}

/// The interpreted scalar product refuses a policy that requires compiled
/// code. The scalar-linked product admits it with the same step plan, and
/// refuses only a document that asks for interpretation.
#[test]
fn the_linked_product_admits_compiled_policies_and_refuses_interpretation() {
    let with_policy = |policy: &str| {
        ScalarPackageOptions::parse(&PAGE_OPTIONS.replace(
            r#""execution_policy":"auto""#,
            &format!(r#""execution_policy":"{policy}""#),
        ))
        .unwrap()
    };
    let native = with_policy("native_required");
    assert_eq!(
        native
            .bind_runtime_choices(decay(&native).unwrap())
            .unwrap_err()
            .to_string(),
        "execution policy `native_required` is refused: this guest is INTERPRETED"
    );
    let linked = native
        .bind_linked_runtime_choices(decay(&native).unwrap())
        .unwrap();
    let auto = with_policy("auto");
    let interpreted = auto.bind_runtime_choices(decay(&auto).unwrap()).unwrap();
    assert_eq!(linked.co_simulation(), interpreted.co_simulation());
    assert!(
        auto.bind_linked_runtime_choices(decay(&auto).unwrap())
            .is_ok()
    );
    let interpreter = with_policy("interpreter");
    assert_eq!(
        interpreter
            .bind_linked_runtime_choices(decay(&interpreter).unwrap())
            .unwrap_err()
            .to_string(),
        "execution policy `interpreter` is refused: the scalar-linked guest executes only compiled entries"
    );
}

/// The page's options document binds its exact solver, step, order and both
/// tolerances as the component's step plan, its parameter override reaches
/// the component, and the Scalar-profile component is the scalar product.
#[test]
fn page_options_bind_their_fixed_step_plan_and_parameters_to_the_component() {
    use rumoca_ir_solve::fmi::{CoSimulationMethod, CoSimulationSubstep};
    let options = ScalarPackageOptions::parse(PAGE_OPTIONS).unwrap();
    let component = options
        .bind_runtime_choices(decay(&options).unwrap())
        .unwrap();
    let plan = component.co_simulation();
    assert_eq!(
        plan.method(),
        CoSimulationMethod::ExplicitRungeKutta { order: 2 }
    );
    assert_eq!(plan.substep(), CoSimulationSubstep::Fixed { step: 0.001 });
    assert_eq!(plan.default_tolerance().to_bits(), 1.0e-6_f64.to_bits());
    assert_eq!(plan.absolute_tolerance(), Some(1.0e-8));
    let root = component.runtime_view().retain_assertion_root();
    let k = root
        .metadata()
        .variables()
        .iter()
        .find(|variable| variable.name() == "k")
        .and_then(|variable| variable.storage())
        .unwrap();
    assert_eq!(root.model().parameters.value(k.base()).unwrap(), 2.0);
    assert!(rumoca_sim::PreparedScalarFmiLsModel::from_component(&component).is_ok());
}

/// No family alias selects a solver, no equal custom tolerance pair or other
/// solver falls back to the standard plan, and no tolerance or policy is
/// defaulted; an override the lowering cannot apply is refused, not dropped.
#[test]
fn package_options_refuse_unknown_names_unbound_tolerances_and_unknown_overrides() {
    let refused = |json: &str| match ScalarPackageOptions::parse(json) {
        Err(error) => error.to_string(),
        Ok(options) => match decay(&options).and_then(|c| options.bind_runtime_choices(c)) {
            Err(error) => error.to_string(),
            Ok(_) => panic!("{json} was admitted"),
        },
    };
    for alias in ["rk-madeup", "euler", "dopri5", "rk45"] {
        let json =
            format!(r#"{{"solver":"{alias}","atol":1e-6,"rtol":1e-6,"execution_policy":"auto"}}"#);
        let refusal = refused(&json);
        assert!(refusal.contains("unknown solver"), "{alias}: {refusal}");
    }
    for json in [
        r#"{"solver":"rk-like","atol":1e-4,"rtol":1e-4,"execution_policy":"auto"}"#,
        r#"{"solver":"rk-like","atol":1e-8,"rtol":1e-6,"execution_policy":"auto"}"#,
        r#"{"solver":"bdf","atol":1e-6,"rtol":1e-6,"execution_policy":"auto"}"#,
    ] {
        assert!(
            refused(json).contains("no Co-Simulation step plan"),
            "{json}"
        );
    }
    for (json, reason) in [
        (
            r#"{"solver":"fixed-step","rtol":1e-6,"execution_policy":"auto"}"#,
            "none is defaulted",
        ),
        (
            r#"{"solver":"fixed-step","atol":1e-8,"rtol":1e-6,"execution_policy":"native_required"}"#,
            "INTERPRETED",
        ),
        (
            r#"{"solver":"fixed-step","atol":1e-8,"rtol":1e-6,"execution_policy":"auto"}"#,
            "explicit fixed_step",
        ),
    ] {
        assert!(refused(json).contains(reason), "{json}");
    }
    let unknown = r#"{"solver":"fixed-step","atol":1e-8,"rtol":1e-6,"parameters":{"nope":1},
        "execution_policy":"auto",
        "fixed_step":{"step":0.001,"order":2}}"#;
    refused(unknown);

    let standard = ScalarPackageOptions::parse(
        r#"{"solver":"rk-like","atol":1e-6,"rtol":1e-6,"execution_policy":"auto"}"#,
    )
    .unwrap();
    let component = standard
        .bind_runtime_choices(decay(&standard).unwrap())
        .unwrap();
    assert_eq!(
        *component.co_simulation(),
        rumoca_ir_solve::fmi::CoSimulationStepPlan::STANDARD
    );
}

/// Source roots are existing absolute paths listed once.
#[test]
fn source_roots_must_exist_be_absolute_and_be_listed_once() {
    let dir = tempfile::tempdir().unwrap();
    let library = dir.path().join("Library.mo");
    std::fs::write(&library, "package Library end Library;\n").unwrap();
    let list = dir.path().join("roots.json");
    let write = |roots: serde_json::Value| std::fs::write(&list, roots.to_string()).unwrap();
    let library = library.to_str().unwrap().to_owned();

    write(serde_json::json!([library]));
    assert_eq!(read_source_roots(&list).unwrap(), [library.clone()]);
    write(serde_json::json!([library, library]));
    assert!(
        read_source_roots(&list)
            .unwrap_err()
            .to_string()
            .contains("listed twice")
    );
    write(serde_json::json!(["Library.mo"]));
    assert!(
        read_source_roots(&list).is_err(),
        "a relative root is refused"
    );
    write(serde_json::json!([dir.path().join("Missing.mo")]));
    assert!(
        read_source_roots(&list).is_err(),
        "a missing root is refused"
    );
}
