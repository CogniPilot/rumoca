//! Narrow, XML-aware view of the FMI 3 metadata needed by lifecycle drivers.

use std::collections::BTreeMap;

use quick_xml::Reader;
use quick_xml::events::{BytesStart, Event};

#[derive(Debug)]
pub(super) struct Variable {
    pub(super) name: String,
    pub(super) value_reference: u32,
    pub(super) causality: String,
    pub(super) variability: String,
    pub(super) initial: Option<String>,
    pub(super) start: Option<Vec<f64>>,
    pub(super) derivative: Option<u32>,
    pub(super) reinit: bool,
    pub(super) unit: Option<String>,
}

#[derive(Debug)]
pub(super) struct Capability {
    pub(super) attributes: BTreeMap<String, String>,
}

impl Capability {
    pub(super) fn boolean_or_false(&self, name: &str) -> bool {
        match self.attributes.get(name).map(String::as_str) {
            None | Some("false") => false,
            Some("true") => true,
            Some(value) => panic!("FMI 3 capability `{name}` has invalid Boolean `{value}`"),
        }
    }
}

#[derive(Debug)]
pub(super) struct Description {
    pub(super) fmi_version: String,
    pub(super) model_name: String,
    pub(super) instantiation_token: String,
    pub(super) model_identifier: String,
    pub(super) model_exchange: Capability,
    pub(super) co_simulation: Capability,
    pub(super) scheduled_execution_count: usize,
    pub(super) variables: Vec<Variable>,
    pub(super) unexpected_variable_kinds: Vec<String>,
    pub(super) dimension_count: usize,
    pub(super) continuous_state_derivatives: Vec<u32>,
    pub(super) initial_unknowns: Vec<u32>,
    pub(super) unexpected_model_structure_children: Vec<String>,
}

#[derive(Default)]
struct DescriptionParts {
    fmi_version: Option<String>,
    model_name: Option<String>,
    token: Option<String>,
    model_exchange: Option<Capability>,
    co_simulation: Option<Capability>,
    scheduled_execution_count: usize,
    variables: Vec<Variable>,
    unexpected_variable_kinds: Vec<String>,
    dimension_count: usize,
    continuous_state_derivatives: Vec<u32>,
    initial_unknowns: Vec<u32>,
    unexpected_model_structure_children: Vec<String>,
}

impl Description {
    pub(super) fn parse(xml: &str) -> Self {
        let mut reader = Reader::from_str(xml);
        let mut parts = DescriptionParts::default();
        let mut in_model_variables = false;
        let mut in_model_structure = false;
        loop {
            match reader.read_event().expect("well-formed FMI 3 model XML") {
                Event::Eof => break,
                Event::Start(element) => match element.name().as_ref() {
                    b"fmiModelDescription" => {
                        let attributes = attributes(&element);
                        parts.fmi_version = Some(required(&attributes, "fmiVersion").to_owned());
                        parts.model_name = Some(required(&attributes, "modelName").to_owned());
                        parts.token = Some(required(&attributes, "instantiationToken").to_owned());
                    }
                    b"ModelExchange" => {
                        set_capability(&mut parts.model_exchange, &element, "ModelExchange")
                    }
                    b"CoSimulation" => {
                        set_capability(&mut parts.co_simulation, &element, "CoSimulation")
                    }
                    b"ModelVariables" => in_model_variables = true,
                    b"ModelStructure" => in_model_structure = true,
                    b"ScheduledExecution" => parts.scheduled_execution_count += 1,
                    b"Float64" if in_model_variables => parts.variables.push(variable(&element)),
                    name if in_model_variables && is_variable_kind(name) => {
                        parts.unexpected_variable_kinds.push(element_name(name))
                    }
                    b"Dimension" => parts.dimension_count += 1,
                    b"ContinuousStateDerivative" if in_model_structure => {
                        push_structure_reference(
                            &mut parts.continuous_state_derivatives,
                            &element,
                            "ContinuousStateDerivative",
                        );
                    }
                    b"InitialUnknown" if in_model_structure => {
                        push_structure_reference(
                            &mut parts.initial_unknowns,
                            &element,
                            "InitialUnknown",
                        );
                    }
                    name if in_model_structure => parts
                        .unexpected_model_structure_children
                        .push(element_name(name)),
                    _ => {}
                },
                Event::Empty(element) => match element.name().as_ref() {
                    b"ModelExchange" => {
                        set_capability(&mut parts.model_exchange, &element, "ModelExchange")
                    }
                    b"CoSimulation" => {
                        set_capability(&mut parts.co_simulation, &element, "CoSimulation")
                    }
                    b"ScheduledExecution" => parts.scheduled_execution_count += 1,
                    b"Float64" if in_model_variables => parts.variables.push(variable(&element)),
                    name if in_model_variables && is_variable_kind(name) => {
                        parts.unexpected_variable_kinds.push(element_name(name))
                    }
                    b"Dimension" => parts.dimension_count += 1,
                    b"ContinuousStateDerivative" if in_model_structure => {
                        push_structure_reference(
                            &mut parts.continuous_state_derivatives,
                            &element,
                            "ContinuousStateDerivative",
                        );
                    }
                    b"InitialUnknown" if in_model_structure => {
                        push_structure_reference(
                            &mut parts.initial_unknowns,
                            &element,
                            "InitialUnknown",
                        );
                    }
                    name if in_model_structure => parts
                        .unexpected_model_structure_children
                        .push(element_name(name)),
                    _ => {}
                },
                Event::End(element) if element.name().as_ref() == b"ModelVariables" => {
                    in_model_variables = false;
                }
                Event::End(element) if element.name().as_ref() == b"ModelStructure" => {
                    in_model_structure = false;
                }
                _ => {}
            }
        }
        parts.finish()
    }

    pub(super) fn variable(&self, name: &str) -> &Variable {
        let mut matches = self
            .variables
            .iter()
            .filter(|variable| variable.name == name);
        let variable = matches
            .next()
            .unwrap_or_else(|| panic!("FMI 3 metadata declares variable `{name}`"));
        assert!(
            matches.next().is_none(),
            "FMI 3 metadata declares variable `{name}` once"
        );
        variable
    }
}

impl DescriptionParts {
    fn finish(self) -> Description {
        let model_exchange = self
            .model_exchange
            .expect("FMI 3 metadata declares ModelExchange");
        let co_simulation = self
            .co_simulation
            .expect("FMI 3 metadata declares CoSimulation");
        let model_exchange_identifier = required(&model_exchange.attributes, "modelIdentifier");
        let co_simulation_identifier = required(&co_simulation.attributes, "modelIdentifier");
        assert_eq!(
            model_exchange_identifier, co_simulation_identifier,
            "FMI 3 ME and CS use one source model identifier"
        );
        assert_c_identifier(model_exchange_identifier);
        Description {
            fmi_version: self
                .fmi_version
                .expect("FMI 3 metadata declares fmiVersion"),
            model_name: self.model_name.expect("FMI 3 metadata declares modelName"),
            instantiation_token: self
                .token
                .expect("FMI 3 metadata declares an instantiation token"),
            model_identifier: model_exchange_identifier.to_owned(),
            model_exchange,
            co_simulation,
            scheduled_execution_count: self.scheduled_execution_count,
            variables: self.variables,
            unexpected_variable_kinds: self.unexpected_variable_kinds,
            dimension_count: self.dimension_count,
            continuous_state_derivatives: self.continuous_state_derivatives,
            initial_unknowns: self.initial_unknowns,
            unexpected_model_structure_children: self.unexpected_model_structure_children,
        }
    }
}

fn is_variable_kind(name: &[u8]) -> bool {
    matches!(
        name,
        b"Float32"
            | b"Int8"
            | b"UInt8"
            | b"Int16"
            | b"UInt16"
            | b"Int32"
            | b"UInt32"
            | b"Int64"
            | b"UInt64"
            | b"Boolean"
            | b"String"
            | b"Binary"
            | b"Enumeration"
            | b"Clock"
    )
}

fn element_name(name: &[u8]) -> String {
    std::str::from_utf8(name)
        .expect("UTF-8 FMI 3 XML element name")
        .to_owned()
}

fn set_capability(slot: &mut Option<Capability>, element: &BytesStart<'_>, label: &str) {
    assert!(
        slot.replace(Capability {
            attributes: attributes(element),
        })
        .is_none(),
        "FMI 3 metadata declares {label} once"
    );
}

fn push_structure_reference(inventory: &mut Vec<u32>, element: &BytesStart<'_>, label: &str) {
    let values = attributes(element);
    inventory.push(parse_u32(
        required(&values, "valueReference"),
        &format!("{label} valueReference"),
    ));
}

fn variable(element: &BytesStart<'_>) -> Variable {
    let attributes = attributes(element);
    Variable {
        name: required(&attributes, "name").to_owned(),
        value_reference: parse_u32(
            required(&attributes, "valueReference"),
            "Float64 valueReference",
        ),
        causality: required(&attributes, "causality").to_owned(),
        variability: required(&attributes, "variability").to_owned(),
        initial: attributes.get("initial").cloned(),
        start: attributes.get("start").map(|start| {
            start
                .split_ascii_whitespace()
                .map(|value| {
                    value
                        .parse()
                        .unwrap_or_else(|_| panic!("FMI 3 start value `{value}` is numeric"))
                })
                .collect()
        }),
        derivative: attributes
            .get("derivative")
            .map(|value| parse_u32(value, "Float64 derivative link")),
        reinit: match attributes.get("reinit").map(String::as_str) {
            None | Some("false") => false,
            Some("true") => true,
            Some(value) => panic!("FMI 3 Float64 reinit `{value}` is Boolean"),
        },
        unit: attributes.get("unit").cloned(),
    }
}

#[derive(Debug)]
pub(super) struct SourceFileSet {
    pub(super) language: String,
    pub(super) source_files: Vec<String>,
    pub(super) preprocessor_definitions: Vec<PreprocessorDefinition>,
}

#[derive(Debug)]
pub(super) struct PreprocessorDefinition {
    pub(super) name: String,
    pub(super) value: String,
}

#[derive(Debug)]
pub(super) struct BuildDescription {
    pub(super) fmi_version: String,
    pub(super) model_identifiers: Vec<String>,
    pub(super) source_file_sets: Vec<SourceFileSet>,
}

impl BuildDescription {
    pub(super) fn parse(xml: &str) -> Self {
        let mut reader = Reader::from_str(xml);
        let mut fmi_version = None;
        let mut model_identifiers = Vec::new();
        let mut source_file_sets = Vec::new();
        let mut current_source_file_set = None;
        loop {
            match reader
                .read_event()
                .expect("well-formed FMI 3 build-description XML")
            {
                Event::Eof => break,
                Event::Start(element) => match element.name().as_ref() {
                    b"fmiBuildDescription" => {
                        let values = attributes(&element);
                        fmi_version = Some(required(&values, "fmiVersion").to_owned());
                    }
                    b"BuildConfiguration" => {
                        let values = attributes(&element);
                        model_identifiers.push(required(&values, "modelIdentifier").to_owned());
                    }
                    b"SourceFileSet" => {
                        assert!(
                            current_source_file_set
                                .replace(source_file_set(&element))
                                .is_none(),
                            "FMI 3 build description does not nest SourceFileSet elements"
                        );
                    }
                    b"SourceFile" => push_source_file(&mut current_source_file_set, &element),
                    b"PreprocessorDefinition" => {
                        push_preprocessor_definition(&mut current_source_file_set, &element)
                    }
                    _ => {}
                },
                Event::Empty(element) => match element.name().as_ref() {
                    b"BuildConfiguration" => {
                        let values = attributes(&element);
                        model_identifiers.push(required(&values, "modelIdentifier").to_owned());
                    }
                    b"SourceFile" => push_source_file(&mut current_source_file_set, &element),
                    b"PreprocessorDefinition" => {
                        push_preprocessor_definition(&mut current_source_file_set, &element)
                    }
                    b"SourceFileSet" => source_file_sets.push(source_file_set(&element)),
                    _ => {}
                },
                Event::End(element) if element.name().as_ref() == b"SourceFileSet" => {
                    source_file_sets.push(
                        current_source_file_set
                            .take()
                            .expect("terminate an open SourceFileSet"),
                    );
                }
                _ => {}
            }
        }
        assert!(
            current_source_file_set.is_none(),
            "FMI 3 build description terminates every SourceFileSet"
        );
        Self {
            fmi_version: fmi_version.expect("FMI 3 build description declares fmiVersion"),
            model_identifiers,
            source_file_sets,
        }
    }
}

fn source_file_set(element: &BytesStart<'_>) -> SourceFileSet {
    let values = attributes(element);
    SourceFileSet {
        language: required(&values, "language").to_owned(),
        source_files: Vec::new(),
        preprocessor_definitions: Vec::new(),
    }
}

fn push_preprocessor_definition(current: &mut Option<SourceFileSet>, element: &BytesStart<'_>) {
    let values = attributes(element);
    current
        .as_mut()
        .expect("PreprocessorDefinition belongs to a SourceFileSet")
        .preprocessor_definitions
        .push(PreprocessorDefinition {
            name: required(&values, "name").to_owned(),
            value: required(&values, "value").to_owned(),
        });
}

fn push_source_file(current: &mut Option<SourceFileSet>, element: &BytesStart<'_>) {
    let values = attributes(element);
    current
        .as_mut()
        .expect("SourceFile belongs to a SourceFileSet")
        .source_files
        .push(required(&values, "name").to_owned());
}

fn attributes(element: &BytesStart<'_>) -> BTreeMap<String, String> {
    let mut values = BTreeMap::new();
    for attribute in element.attributes() {
        let attribute = attribute.expect("well-formed FMI 3 XML attribute");
        let key = std::str::from_utf8(attribute.key.as_ref())
            .expect("UTF-8 FMI 3 XML attribute name")
            .to_owned();
        let value = attribute
            .unescape_value()
            .expect("unescapable FMI 3 XML attribute")
            .into_owned();
        assert!(
            values.insert(key.clone(), value).is_none(),
            "FMI 3 XML element has duplicate `{key}` attributes"
        );
    }
    values
}

fn required<'a>(attributes: &'a BTreeMap<String, String>, name: &str) -> &'a str {
    attributes
        .get(name)
        .unwrap_or_else(|| panic!("FMI 3 XML element has a `{name}` attribute"))
}

fn parse_u32(value: &str, label: &str) -> u32 {
    value
        .parse()
        .unwrap_or_else(|_| panic!("{label} `{value}` is an unsigned 32-bit integer"))
}

fn assert_c_identifier(identifier: &str) {
    let mut bytes = identifier.bytes();
    let first = bytes.next().expect("FMI 3 modelIdentifier is non-empty");
    assert!(
        first == b'_' || first.is_ascii_alphabetic(),
        "FMI 3 modelIdentifier starts like a C identifier"
    );
    assert!(
        bytes.all(|byte| byte == b'_' || byte.is_ascii_alphanumeric()),
        "FMI 3 modelIdentifier is a C identifier"
    );
}

pub(super) fn c_string_contents(value: &str) -> String {
    let mut escaped = String::new();
    for byte in value.bytes() {
        match byte {
            b'\\' => escaped.push_str("\\\\"),
            b'\"' => escaped.push_str("\\\""),
            b'\n' => escaped.push_str("\\n"),
            b'\r' => escaped.push_str("\\r"),
            b'\t' => escaped.push_str("\\t"),
            0x20..=0x7e => escaped.push(char::from(byte)),
            _ => panic!("FMI 3 instantiation token is printable ASCII"),
        }
    }
    escaped
}
