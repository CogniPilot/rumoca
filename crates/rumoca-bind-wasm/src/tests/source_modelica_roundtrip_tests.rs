use super::*;

const MINI_RESISTOR_EXAMPLE_SOURCE_ROOTS: &str = r#"{
  "Modelica/package.mo": "within ;\npackage Modelica\nend Modelica;\n",
  "Modelica/Blocks/package.mo": "within Modelica;\npackage Blocks\nend Blocks;\n",
  "Modelica/Blocks/Sources/package.mo": "within Modelica.Blocks;\npackage Sources\nend Sources;\n",
  "Modelica/Blocks/Sources/Sine.mo": "within Modelica.Blocks.Sources;\nmodel Sine\n  parameter Real amplitude = 1;\n  parameter Real f = 1;\n  parameter Real phase = 0;\nend Sine;\n",
  "Modelica/Electrical/package.mo": "within Modelica;\npackage Electrical\nend Electrical;\n",
  "Modelica/Electrical/Analog/package.mo": "within Modelica.Electrical;\npackage Analog\nend Analog;\n",
  "Modelica/Electrical/Analog/Interfaces/package.mo": "within Modelica.Electrical.Analog;\npackage Interfaces\nend Interfaces;\n",
  "Modelica/Electrical/Analog/Interfaces/VoltageSource.mo": "within Modelica.Electrical.Analog.Interfaces;\npartial model VoltageSource\n  replaceable Modelica.Blocks.Sources.Sine signalSource;\nend VoltageSource;\n",
  "Modelica/Electrical/Analog/Sources/package.mo": "within Modelica.Electrical.Analog;\npackage Sources\nend Sources;\n",
  "Modelica/Electrical/Analog/Sources/SineVoltage.mo": "within Modelica.Electrical.Analog.Sources;\nmodel SineVoltage\n  parameter Real V(start=1);\n  parameter Real phase=0;\n  parameter Real f(start=1);\n  extends Interfaces.VoltageSource(\n    redeclare Modelica.Blocks.Sources.Sine signalSource(\n      final amplitude=V,\n      final f=f,\n      final phase=phase));\nend SineVoltage;\n",
  "Modelica/Electrical/Analog/Examples/package.mo": "within Modelica.Electrical.Analog;\npackage Examples\nend Examples;\n",
  "Modelica/Electrical/Analog/Examples/Resistor.mo": "within Modelica.Electrical.Analog.Examples;\nmodel Resistor\n  Modelica.Electrical.Analog.Sources.SineVoltage SineVoltage1(V=220, f=1);\nend Resistor;\n"
}"#;

const MINI_DRUM_BOILER_SOURCE_ROOTS: &str = r#"{
  "Modelica/package.mo": "within ;\npackage Modelica\nend Modelica;\n",
  "Modelica/Media/package.mo": "within Modelica;\npackage Media\nend Media;\n",
  "Modelica/Media/Water/package.mo": "within Modelica.Media;\npackage Water\nend Water;\n",
  "Modelica/Media/Water/StandardWater.mo": "within Modelica.Media.Water;\npackage StandardWater\nend StandardWater;\n",
  "Modelica/Fluid/package.mo": "within Modelica;\npackage Fluid\nend Fluid;\n",
  "Modelica/Fluid/Interfaces/package.mo": "within Modelica.Fluid;\npackage Interfaces\nend Interfaces;\n",
  "Modelica/Fluid/Interfaces/PartialTwoPort.mo": "within Modelica.Fluid.Interfaces;\npartial model PartialTwoPort\n  replaceable package Medium = Modelica.Media.Water.StandardWater;\nend PartialTwoPort;\n",
  "Modelica/Fluid/Examples/package.mo": "within Modelica.Fluid;\npackage Examples\nend Examples;\n",
  "Modelica/Fluid/Examples/DrumBoiler.mo": "within Modelica.Fluid.Examples;\nmodel DrumBoiler\n  extends Modelica.Fluid.Interfaces.PartialTwoPort(\n    redeclare package Medium = Modelica.Media.Water.StandardWater);\nend DrumBoiler;\n"
}"#;

#[test]
fn test_get_class_info_sine_voltage_roundtrip_characterization() {
    let _guard = session_test_guard();
    clear_source_root_cache();

    let source_roots = MINI_RESISTOR_EXAMPLE_SOURCE_ROOTS.to_string();
    load_source_roots(&source_roots).expect("load_source_roots should succeed");

    // Sanity: parser accepts the Resistor source from source roots.
    let resistor_source = "within Modelica.Electrical.Analog.Examples;\nmodel Resistor\n  Modelica.Electrical.Analog.Sources.SineVoltage SineVoltage1(V=220, f=1);\nend Resistor;\n";
    #[cfg(target_arch = "wasm32")]
    parse_source_root_file(
        resistor_source,
        "Modelica/Electrical/Analog/Examples/Resistor.mo",
    )
    .expect("expected source-root resistor content to parse");
    #[cfg(not(target_arch = "wasm32"))]
    parse_source_to_ast(
        resistor_source,
        "Modelica/Electrical/Analog/Examples/Resistor.mo",
    )
    .expect("expected source-root resistor content to parse");

    let resistor_json = get_class_info("Modelica.Electrical.Analog.Examples.Resistor")
        .expect("get_class_info should succeed for Resistor");
    let resistor_info: serde_json::Value =
        serde_json::from_str(&resistor_json).expect("valid Resistor class info JSON");
    let components = resistor_info
        .get("components")
        .and_then(|value| value.as_array())
        .expect("Resistor class info should include component list");
    let sine_voltage_type = components
        .iter()
        .find(|component| {
            component.get("name").and_then(|value| value.as_str()) == Some("SineVoltage1")
        })
        .and_then(|component| component.get("type_name"))
        .and_then(|value| value.as_str())
        .expect("Resistor should contain SineVoltage1 component with type_name");
    assert_eq!(
        sine_voltage_type, "Modelica.Electrical.Analog.Sources.SineVoltage",
        "Resistor component type should resolve to fully-qualified SineVoltage"
    );

    let sine_json = get_class_info(sine_voltage_type)
        .expect("get_class_info should succeed for resolved SineVoltage type");
    let sine_info: serde_json::Value =
        serde_json::from_str(&sine_json).expect("valid class info JSON");
    let source_modelica = sine_info
        .get("source_modelica")
        .and_then(|value| value.as_str())
        .expect("class info should include source_modelica");
    assert!(
        source_modelica.contains("extends Interfaces.VoltageSource"),
        "unexpected class_info source_modelica payload: {source_modelica}"
    );

    // Ensure serializer emits valid redeclare class modification form:
    // `redeclare Type instanceName(...)`.
    assert!(
        source_modelica.contains("redeclare Modelica.Blocks.Sources.Sine signalSource"),
        "expected valid redeclare serialization shape in SineVoltage class_info source_modelica, got: {source_modelica}"
    );

    #[cfg(target_arch = "wasm32")]
    let roundtrip_ok = parse_source_root_file(
        source_modelica,
        "Modelica/Electrical/Analog/Sources/SineVoltage.mo",
    )
    .is_ok();
    #[cfg(not(target_arch = "wasm32"))]
    let roundtrip_ok = parse_source_to_ast(
        source_modelica,
        "Modelica/Electrical/Analog/Sources/SineVoltage.mo",
    )
    .is_ok();
    assert!(
        roundtrip_ok,
        "expected class_info source_modelica to round-trip parse for SineVoltage"
    );

    clear_source_root_cache();
}

#[test]
fn test_get_class_info_package_redeclare_roundtrip_characterization() {
    let _guard = session_test_guard();
    clear_source_root_cache();

    let source_roots = MINI_DRUM_BOILER_SOURCE_ROOTS.to_string();
    load_source_roots(&source_roots).expect("load_source_roots should succeed");

    let drum_json = get_class_info("Modelica.Fluid.Examples.DrumBoiler")
        .expect("get_class_info should succeed for DrumBoiler");
    let drum_info: serde_json::Value =
        serde_json::from_str(&drum_json).expect("valid DrumBoiler class info JSON");
    let source_modelica = drum_info
        .get("source_modelica")
        .and_then(|value| value.as_str())
        .expect("class info should include source_modelica");

    assert!(
        source_modelica.contains("redeclare package Medium = Modelica.Media.Water.StandardWater"),
        "expected package redeclare assignment form in class_info source_modelica, got: {source_modelica}"
    );

    #[cfg(target_arch = "wasm32")]
    let roundtrip_ok =
        parse_source_root_file(source_modelica, "Modelica/Fluid/Examples/DrumBoiler.mo").is_ok();
    #[cfg(not(target_arch = "wasm32"))]
    let roundtrip_ok =
        parse_source_to_ast(source_modelica, "Modelica/Fluid/Examples/DrumBoiler.mo").is_ok();
    assert!(
        roundtrip_ok,
        "expected class_info source_modelica to round-trip parse for DrumBoiler"
    );

    clear_source_root_cache();
}
