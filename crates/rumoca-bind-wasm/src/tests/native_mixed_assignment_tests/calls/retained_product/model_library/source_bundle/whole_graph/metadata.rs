//! Bounded test projection of the original fixture inventory, not an XML issuer.

use super::*;
use rumoca_ir_solve::{SolveDeclarationStart, fmi::FmiVariable};

const CONTROL_TOKEN: &str = "96b75631-296f-4935-a94a-f21264a6e7f0";

pub(super) fn case_manifest(
    component: &FmiComponent,
    code: &PreparedNativeWasmCode,
) -> serde_json::Value {
    let variable = |name| {
        component
            .variables()
            .iter()
            .find(|variable| variable.name() == name)
            .expect("original source fixture declaration is present")
    };
    let vr = |name| variable(name).value_reference_fmi3();
    let first = match variable("first").declaration_start() {
        SolveDeclarationStart::Boolean(values) => *values.value(0).unwrap(),
        _ => panic!("original Boolean input has its exact source start"),
    };
    let integer = match variable("k").declaration_start() {
        SolveDeclarationStart::Integer(values) => *values.value(0).unwrap(),
        _ => panic!("original Integer input has its exact source start"),
    };
    let samples = match variable("samples").declaration_start() {
        SolveDeclarationStart::Real(values) => (0..values.len())
            .map(|ordinal| values.value(ordinal).unwrap())
            .collect::<Vec<_>>(),
        _ => panic!("original Real array has its actual source start"),
    };
    assert_eq!(
        (first, integer, samples.as_slice()),
        (true, 1, [5.0].as_slice())
    );
    let variables = component.variables().iter().map(|variable| serde_json::json!({
        "name":variable.name(), "valueReference":variable.value_reference_fmi3(),
        "type":variable.fmi3_type(), "count":variable.scalar_count(),
        "dimensions":variable.dimensions(), "causality":variable.causality(),
        "variability":variable.variability(), "declarationStart":variable.declaration_start(),
    })).collect::<Vec<_>>();
    serde_json::json!({
        "schema":1,
        "scope":"actual fixed native FMI-LS WIT control, not a checked FMU publication",
        "xmlScope":"test-only original-inventory projection; production metadata/source binding remains pending",
        "modelName":"ReturnPredicates", "instantiationToken":CONTROL_TOKEN,
        "variables":variables,
        "references":{"first":vr("first"),"integer":vr("k"),"samples":vr("samples"),"result":vr("result"),"time":0},
        "defaults":{"first":first,"integer":integer,"samples":samples,"result":1.0},
        "expectedCases":[
            {"first":true,"integer":2,"samples":[5.0],"result":1.0},
            {"first":false,"integer":1,"samples":[5.0],"result":2.0},
            {"first":false,"integer":1,"samples":[-5.0],"result":3.0},
            {"first":true,"integer":i64::MIN,"samples":[-0.0],"result":1.0},
            {"first":true,"integer":i64::MAX,"samples":[5.0],"result":1.0},
        ],
        "faultCase":{"first":false,"integer":2,"samples":[5.0],"kind":"IndexBounds","acceptedResultUnchanged":true,"atomicYProofScope":"source/raw-entry oracle; WIT host checks Error/no returned tuple/recovery"},
        "indexBoundsStatuses":code.compiled().faults().iter().filter(|fault| fault.kind == rumoca_exec_wasm::TypedCallFaultKind::IndexBounds).map(|fault| fault.status).collect::<Vec<_>>(),
        "starts":[2.0,5.0],"fmiResources":2,"componentInstances":1,
        "largeCapacityControl":"not covered by this original scalar fixture",
        "componentProfile":"canonical co-simulation-fmu; actual original ME owner",
    })
}

pub(super) fn fixture_xml(component: &FmiComponent, token: &str) -> String {
    let mut xml = format!(
        r#"<?xml version="1.0" encoding="UTF-8"?>
<fmiModelDescription fmiVersion="3.0" modelName="ReturnPredicates" instantiationToken="{token}">
<CoSimulation modelIdentifier="Rumoca_control" canHandleVariableCommunicationStepSize="true"/>
<LogCategories><Category name="error"/><Category name="warning"/><Category name="message"/></LogCategories>
<ModelVariables><Float64 name="time" valueReference="0" causality="independent" variability="continuous"/>
"#
    );
    for variable in component.variables() {
        assert!(
            variable
                .name()
                .chars()
                .all(|character| character.is_ascii_alphanumeric() || character == '_'),
            "bounded fixture names must be XML-safe; this is not a production renderer"
        );
        let kind = variable.fmi3_type();
        let causality = serde_json::to_value(variable.causality()).unwrap();
        let variability = serde_json::to_value(variable.variability()).unwrap();
        xml.push_str(&format!(
            "<{kind} name=\"{}\" valueReference=\"{}\" causality=\"{}\" variability=\"{}\"",
            variable.name(),
            variable.value_reference_fmi3(),
            causality.as_str().unwrap(),
            variability.as_str().unwrap()
        ));
        if let Some(initial) = variable.initial() {
            let initial = serde_json::to_value(initial).unwrap();
            xml.push_str(&format!(" initial=\"{}\"", initial.as_str().unwrap()));
        }
        if let Some(values) = start_values(variable) {
            xml.push_str(&format!(" start=\"{}\"", values.join(" ")));
        }
        xml.push('>');
        for dimension in variable.dimensions() {
            xml.push_str(&format!("<Dimension start=\"{dimension}\"/>"));
        }
        xml.push_str(&format!("</{kind}>\n"));
    }
    xml.push_str("</ModelVariables><ModelStructure>");
    for variable in component.variables() {
        if variable.causality() == rumoca_ir_solve::fmi::FmiCausality::Output {
            xml.push_str(&format!(
                "<Output valueReference=\"{}\"/>",
                variable.value_reference_fmi3()
            ));
        }
    }
    xml.push_str("</ModelStructure></fmiModelDescription>\n");
    xml
}

fn start_values(variable: &FmiVariable) -> Option<Vec<String>> {
    let start = variable.typed_start()?;
    let values = (0..start.len()).map(|ordinal| match start {
        SolveDeclarationStart::Real(values) => values.value(ordinal).unwrap().to_string(),
        SolveDeclarationStart::Integer(values) | SolveDeclarationStart::Enumeration(values) => {
            values.value(ordinal).unwrap().to_string()
        }
        SolveDeclarationStart::Boolean(values) => values.value(ordinal).unwrap().to_string(),
        SolveDeclarationStart::Missing { .. } => unreachable!("handled before projecting values"),
    });
    if start.is_missing() {
        None
    } else {
        Some(values.collect())
    }
}
