//! Deterministic XML serialization of `__content.xml`, the Algorithm Code
//! manifest, and the Production Code manifest via quick-xml's event writer
//! (D3: typed emission, no templates).
//!
//! Guarantees:
//!
//! - **byte-stable**: output is a pure function of the validated model; the
//!   same model always serializes to identical bytes (checksums depend on it,
//!   GAL-021);
//! - **escaped**: attribute values are XML-escaped by the event writer;
//! - **fixed attribute order**: attributes are emitted in XSD declaration
//!   order; fixed attributes (`xsdVersion`, `efmiVersion`, `kind`) come from
//!   crate constants (GAL-022);
//! - **declaration header**: `<?xml version="1.0" encoding="UTF-8"?>` plus a
//!   trailing newline at end of file.
//!
//! Ids and timestamps are serialized from the values carried by the models;
//! nothing here mints identities or reads clocks.

use std::path::Path;
use std::process::Command;

use quick_xml::Writer;
use quick_xml::events::{BytesDecl, BytesEnd, BytesStart, BytesText, Event};

use crate::algorithm_code_manifest::{
    AlgorithmCodeManifest, BlockMethod, BlockMethods, StartValue, Variable, VariableCommon,
};
use crate::content::Content;
use crate::diagnostic::EfmiError;
use crate::ids::Identifier;
use crate::manifest_common::{Annotation, BaseUnit, File, FileChecksum, ManifestAttributes, Unit};
use crate::production_code_manifest::{
    CodeContainer, CodeFile, Component, ForeignReference, FormalParameter, Function, LogicalData,
    ManifestReference, ParameterCore, ProductionCodeManifest, TargetType, Typedef, TypedefBody,
};
use crate::{
    ALGORITHM_CODE_MANIFEST_XSD_VERSION, CONTAINER_MANIFEST_XSD_VERSION, EFMI_VERSION,
    PRODUCTION_CODE_MANIFEST_XSD_VERSION,
};

/// XML Schema instance namespace bound to the `xsi` prefix on root elements.
const XSI_NAMESPACE: &str = "http://www.w3.org/2001/XMLSchema-instance";

/// Schema location hint for `__content.xml`, which always sits next to the
/// `schemas/` directory at the eFMU root (eFMI ch. 2).
pub const CONTENT_SCHEMA_LOCATION: &str = "schemas/efmiContainerManifest.xsd";

/// Schema location hint for an Algorithm Code manifest, which always sits at
/// the root of its container directory, one level below the eFMU root (the
/// container name is slash-free by `efmiNameWithoutSlashesType`).
pub const ALGORITHM_CODE_SCHEMA_LOCATION: &str =
    "../schemas/AlgorithmCode/efmiAlgorithmCodeManifest.xsd";

/// Schema location hint for a Production Code manifest, which always sits at
/// the root of its container directory, one level below the eFMU root (the
/// container name is slash-free by `efmiNameWithoutSlashesType`).
pub const PRODUCTION_CODE_SCHEMA_LOCATION: &str =
    "../schemas/ProductionCode/efmiProductionCodeManifest.xsd";

/// Serialize a validated [`Content`] model to `__content.xml` bytes.
pub fn content_to_xml(content: &Content) -> Result<Vec<u8>, EfmiError> {
    let parts = content.parts();
    let mut xml = XmlOut::new()?;
    let mut root = BytesStart::new("Content");
    root.push_attribute(("xmlns:xsi", XSI_NAMESPACE));
    root.push_attribute(("xsi:noNamespaceSchemaLocation", CONTENT_SCHEMA_LOCATION));
    root.push_attribute(("xsdVersion", CONTAINER_MANIFEST_XSD_VERSION));
    push_opt(
        &mut root,
        "activeFmu",
        parts.active_fmu.as_ref().map(|n| n.as_str()),
    );
    push_manifest_attributes(&mut root, &parts.attributes);
    xml.start(root)?;
    for representation in &parts.model_representations {
        let mut el = BytesStart::new("ModelRepresentation");
        el.push_attribute(("name", representation.name.as_str()));
        el.push_attribute(("kind", representation.kind.as_str()));
        el.push_attribute(("manifest", representation.manifest.as_str()));
        el.push_attribute(("checksum", representation.checksum.as_str()));
        el.push_attribute((
            "manifestRefId",
            representation.manifest_ref_id.to_string().as_str(),
        ));
        xml.empty(el)?;
    }
    xml.end("Content")?;
    Ok(xml.finish())
}

/// Serialize a validated [`AlgorithmCodeManifest`] model to manifest bytes.
pub fn algorithm_code_manifest_to_xml(
    manifest: &AlgorithmCodeManifest,
) -> Result<Vec<u8>, EfmiError> {
    let parts = manifest.parts();
    let mut xml = XmlOut::new()?;
    let mut root = BytesStart::new("Manifest");
    root.push_attribute(("xmlns:xsi", XSI_NAMESPACE));
    root.push_attribute((
        "xsi:noNamespaceSchemaLocation",
        ALGORITHM_CODE_SCHEMA_LOCATION,
    ));
    root.push_attribute(("xsdVersion", ALGORITHM_CODE_MANIFEST_XSD_VERSION));
    root.push_attribute(("kind", "AlgorithmCode"));
    push_manifest_attributes(&mut root, &parts.attributes);
    root.push_attribute(("fileRefId", parts.file_ref_id.as_str()));
    xml.start(root)?;

    write_files(&mut xml, &parts.files)?;
    let mut clock = BytesStart::new("Clock");
    clock.push_attribute(("id", parts.clock.id.as_str()));
    clock.push_attribute(("variableRefId", parts.clock.variable_ref_id.as_str()));
    xml.empty(clock)?;
    write_block_methods(&mut xml, &parts.block_methods)?;
    let mut status = BytesStart::new("ErrorSignalStatus");
    status.push_attribute(("id", parts.error_signal_status.id.as_str()));
    xml.empty(status)?;
    write_units(&mut xml, &parts.units)?;
    write_variables(&mut xml, &parts.variables)?;
    write_annotations(&mut xml, &parts.annotations)?;

    xml.end("Manifest")?;
    Ok(xml.finish())
}

/// Serialize a validated [`ProductionCodeManifest`] model to manifest bytes.
///
/// Serializer-owned structural rules (validated facts come from
/// [`ProductionCodeManifest::new`]; everything below is emission shape):
///
/// - optional list wrappers (`Includes`, `Typedefs`, `Functions`,
///   `TargetTypes`, `Dimensions`, `FormalParameters`) are emitted only when
///   non-empty (their inner elements are `minOccurs="1"`);
/// - the `DataReferences`/`FunctionReferences` wrappers are ALWAYS emitted
///   (1..1 with optional children — the inverse rule);
/// - `Dimension/@number` and `FormalParameter/@number` are derived from Vec
///   position, 0-based (decision D1; the Algorithm Code schema documents
///   1-based, so the Algorithm Code serializer differs deliberately);
/// - the single `ManifestReference` carries `origin="true"` unconditionally
///   (with exactly one reference it must be the origin);
/// - XSD-defaulted boolean attributes (`const`, `pointer`, `constPointer`,
///   `Component/@pointer`) are emitted only when non-default (`true`).
///
/// Callers hash the returned bytes exactly once and never re-serialize
/// (checksum stability, GAL-021).
pub fn production_code_manifest_to_xml(
    manifest: &ProductionCodeManifest,
) -> Result<Vec<u8>, EfmiError> {
    let parts = manifest.parts();
    let mut xml = XmlOut::new()?;
    let mut root = BytesStart::new("Manifest");
    root.push_attribute(("xmlns:xsi", XSI_NAMESPACE));
    root.push_attribute((
        "xsi:noNamespaceSchemaLocation",
        PRODUCTION_CODE_SCHEMA_LOCATION,
    ));
    root.push_attribute(("xsdVersion", PRODUCTION_CODE_MANIFEST_XSD_VERSION));
    root.push_attribute(("kind", "ProductionCode"));
    push_manifest_attributes(&mut root, &parts.attributes);
    xml.start(root)?;

    write_manifest_references(&mut xml, &parts.manifest_reference)?;
    write_files(&mut xml, &parts.files)?;
    write_code_container(&mut xml, &parts.code_container)?;
    write_annotations(&mut xml, &parts.annotations)?;

    xml.end("Manifest")?;
    Ok(xml.finish())
}

/// Validate an XML file against an XSD by shelling out to `xmllint`
/// (`xmllint --noout --schema <xsd> <xml>`).
///
/// The XSD path must point into an intact schema tree (the vendored
/// `assets/efmi-schemas/` or an emitted `schemas/` copy) so that relative
/// `xs:include`/`xs:import` references resolve.
///
/// A missing `xmllint` is a hard, actionable error
/// ([`EfmiError::XmllintUnavailable`]) — validation is never silently
/// skipped; a rejected document yields [`EfmiError::XsdValidationFailed`]
/// carrying xmllint's diagnostics.
pub fn validate_against_xsd(xml_path: &Path, xsd_path: &Path) -> Result<(), EfmiError> {
    let output = Command::new("xmllint")
        .arg("--noout")
        .arg("--schema")
        .arg(xsd_path)
        .arg(xml_path)
        .output()
        .map_err(|error| match error.kind() {
            std::io::ErrorKind::NotFound => EfmiError::XmllintUnavailable,
            _ => EfmiError::Io {
                path: "xmllint".to_owned(),
                message: error.to_string(),
            },
        })?;
    if output.status.success() {
        return Ok(());
    }
    let mut combined = String::from_utf8_lossy(&output.stderr).into_owned();
    combined.push_str(&String::from_utf8_lossy(&output.stdout));
    Err(EfmiError::XsdValidationFailed {
        xml: xml_path.display().to_string(),
        xsd: xsd_path.display().to_string(),
        output: combined,
    })
}

/// Thin wrapper over the quick-xml event writer with uniform error mapping
/// and two-space indentation.
struct XmlOut {
    writer: Writer<Vec<u8>>,
}

impl XmlOut {
    fn new() -> Result<Self, EfmiError> {
        let mut writer = Writer::new_with_indent(Vec::new(), b' ', 2);
        writer
            .write_event(Event::Decl(BytesDecl::new("1.0", Some("UTF-8"), None)))
            .map_err(xml_err)?;
        Ok(Self { writer })
    }

    fn start(&mut self, element: BytesStart<'_>) -> Result<(), EfmiError> {
        self.writer
            .write_event(Event::Start(element))
            .map_err(xml_err)
    }

    fn empty(&mut self, element: BytesStart<'_>) -> Result<(), EfmiError> {
        self.writer
            .write_event(Event::Empty(element))
            .map_err(xml_err)
    }

    fn end(&mut self, name: &str) -> Result<(), EfmiError> {
        self.writer
            .write_event(Event::End(BytesEnd::new(name)))
            .map_err(xml_err)
    }

    /// Write `<name>text</name>` on one line, with escaped text content.
    fn text_element(&mut self, name: &str, text: &str) -> Result<(), EfmiError> {
        self.start(BytesStart::new(name))?;
        self.writer
            .write_event(Event::Text(BytesText::new(text)))
            .map_err(xml_err)?;
        self.end(name)
    }

    fn finish(self) -> Vec<u8> {
        let mut bytes = self.writer.into_inner();
        bytes.push(b'\n');
        bytes
    }
}

fn xml_err(error: impl std::fmt::Display) -> EfmiError {
    EfmiError::XmlWrite {
        message: error.to_string(),
    }
}

fn push_opt(element: &mut BytesStart<'_>, name: &str, value: Option<&str>) {
    if let Some(value) = value {
        element.push_attribute((name, value));
    }
}

/// Shared `efmiManifestAttributesBase` group, in XSD declaration order.
/// `efmiVersion` is fixed by the standard and emitted as a constant.
fn push_manifest_attributes(element: &mut BytesStart<'_>, attributes: &ManifestAttributes) {
    element.push_attribute(("efmiVersion", EFMI_VERSION));
    element.push_attribute(("id", attributes.id.to_string().as_str()));
    element.push_attribute(("name", attributes.name.as_str()));
    push_opt(
        element,
        "description",
        attributes.description.as_ref().map(|v| v.as_str()),
    );
    push_opt(
        element,
        "version",
        attributes.version.as_ref().map(|v| v.as_str()),
    );
    element.push_attribute((
        "generationDateAndTime",
        attributes.generation_date_and_time.to_string().as_str(),
    ));
    push_opt(
        element,
        "generationTool",
        attributes.generation_tool.as_ref().map(|v| v.as_str()),
    );
    push_opt(
        element,
        "copyright",
        attributes.copyright.as_ref().map(|v| v.as_str()),
    );
    push_opt(
        element,
        "license",
        attributes.license.as_ref().map(|v| v.as_str()),
    );
}

fn write_files(xml: &mut XmlOut, files: &[File]) -> Result<(), EfmiError> {
    if files.is_empty() {
        return xml.empty(BytesStart::new("Files"));
    }
    xml.start(BytesStart::new("Files"))?;
    for file in files {
        let mut el = BytesStart::new("File");
        el.push_attribute(("id", file.id.as_str()));
        el.push_attribute(("name", file.name.as_str()));
        el.push_attribute(("path", file.path.as_str()));
        match &file.checksum {
            FileChecksum::Sha1(checksum) => {
                el.push_attribute(("needsChecksum", "true"));
                el.push_attribute(("checksum", checksum.as_str()));
            }
            FileChecksum::NotNeeded => el.push_attribute(("needsChecksum", "false")),
        }
        el.push_attribute(("role", file.role.as_str()));
        push_opt(
            &mut el,
            "description",
            file.description.as_ref().map(|v| v.as_str()),
        );
        xml.empty(el)?;
    }
    xml.end("Files")
}

fn write_block_methods(xml: &mut XmlOut, methods: &BlockMethods) -> Result<(), EfmiError> {
    xml.start(BytesStart::new("BlockMethods"))?;
    write_block_method(xml, "Startup", &methods.startup)?;
    write_block_method(xml, "Recalibrate", &methods.recalibrate)?;
    write_block_method(xml, "DoStep", &methods.do_step)?;
    xml.end("BlockMethods")
}

fn write_block_method(xml: &mut XmlOut, kind: &str, method: &BlockMethod) -> Result<(), EfmiError> {
    let mut el = BytesStart::new("BlockMethod");
    el.push_attribute(("id", method.id.as_str()));
    el.push_attribute(("kind", kind));
    if method.signals.is_empty() {
        return xml.empty(el);
    }
    xml.start(el)?;
    xml.start(BytesStart::new("Signals"))?;
    for signal in &method.signals {
        let mut signal_el = BytesStart::new("Signal");
        signal_el.push_attribute(("value", signal.as_str()));
        xml.empty(signal_el)?;
    }
    xml.end("Signals")?;
    xml.end("BlockMethod")
}

fn write_units(xml: &mut XmlOut, units: &[Unit]) -> Result<(), EfmiError> {
    if units.is_empty() {
        // The Units element is required by the XSD but may be empty.
        return xml.empty(BytesStart::new("Units"));
    }
    xml.start(BytesStart::new("Units"))?;
    for unit in units {
        let mut el = BytesStart::new("Unit");
        el.push_attribute(("id", unit.id.as_str()));
        el.push_attribute(("name", unit.name.as_str()));
        match &unit.base_unit {
            None => xml.empty(el)?,
            Some(base_unit) => {
                xml.start(el)?;
                xml.empty(base_unit_element(base_unit))?;
                xml.end("Unit")?;
            }
        }
    }
    xml.end("Units")
}

/// XSD-defaulted BaseUnit attributes are emitted only when non-default,
/// in XSD declaration order.
fn base_unit_element(base_unit: &BaseUnit) -> BytesStart<'static> {
    let mut el = BytesStart::new("BaseUnit");
    for (name, value) in [
        ("kg", base_unit.kg),
        ("m", base_unit.m),
        ("s", base_unit.s),
        ("A", base_unit.ampere),
        ("K", base_unit.kelvin),
        ("mol", base_unit.mol),
        ("cd", base_unit.cd),
        ("rad", base_unit.rad),
    ] {
        if value != 0 {
            el.push_attribute((name, value.to_string().as_str()));
        }
    }
    if base_unit.factor != 1.0 {
        el.push_attribute(("factor", format_real(base_unit.factor).as_str()));
    }
    if base_unit.offset != 0.0 {
        el.push_attribute(("offset", format_real(base_unit.offset).as_str()));
    }
    el
}

fn write_variables(xml: &mut XmlOut, variables: &[Variable]) -> Result<(), EfmiError> {
    xml.start(BytesStart::new("Variables"))?;
    for variable in variables {
        write_variable(xml, variable)?;
    }
    xml.end("Variables")
}

fn write_variable(xml: &mut XmlOut, variable: &Variable) -> Result<(), EfmiError> {
    let (tag, el) = match variable {
        Variable::Real(real) => {
            let mut el =
                base_variable_element("RealVariable", &real.common, real_start(&real.start));
            push_opt(
                &mut el,
                "unitRefId",
                real.unit_ref_id.as_ref().map(|i| i.as_str()),
            );
            if real.relative_quantity {
                el.push_attribute(("relativeQuantity", "true"));
            }
            push_opt(&mut el, "min", real.min.map(format_real).as_deref());
            push_opt(&mut el, "max", real.max.map(format_real).as_deref());
            push_opt(&mut el, "nominal", real.nominal.map(format_real).as_deref());
            ("RealVariable", el)
        }
        Variable::Integer(integer) => {
            let mut el = base_variable_element(
                "IntegerVariable",
                &integer.common,
                integer_start(&integer.start),
            );
            push_opt(
                &mut el,
                "min",
                integer.min.map(|v| v.to_string()).as_deref(),
            );
            push_opt(
                &mut el,
                "max",
                integer.max.map(|v| v.to_string()).as_deref(),
            );
            ("IntegerVariable", el)
        }
        Variable::Boolean(boolean) => (
            "BooleanVariable",
            base_variable_element(
                "BooleanVariable",
                &boolean.common,
                boolean_start(&boolean.start),
            ),
        ),
    };
    let common = variable.common();
    if common.dimensions.is_empty() && common.annotations.is_empty() {
        return xml.empty(el);
    }
    xml.start(el)?;
    write_dimensions(xml, &common.dimensions)?;
    write_annotations(xml, &common.annotations)?;
    xml.end(tag)
}

fn base_variable_element<'a>(
    tag: &'a str,
    common: &VariableCommon,
    start: String,
) -> BytesStart<'a> {
    let mut el = BytesStart::new(tag);
    el.push_attribute(("id", common.id.as_str()));
    el.push_attribute(("name", common.name.as_str()));
    push_opt(
        &mut el,
        "description",
        common.description.as_ref().map(|v| v.as_str()),
    );
    el.push_attribute(("blockCausality", common.block_causality.as_str()));
    el.push_attribute(("start", start.as_str()));
    el
}

fn write_dimensions(xml: &mut XmlOut, dimensions: &[u64]) -> Result<(), EfmiError> {
    if dimensions.is_empty() {
        return Ok(());
    }
    xml.start(BytesStart::new("Dimensions"))?;
    for (index, size) in dimensions.iter().enumerate() {
        let mut el = BytesStart::new("Dimension");
        el.push_attribute(("number", (index + 1).to_string().as_str()));
        el.push_attribute(("size", size.to_string().as_str()));
        xml.empty(el)?;
    }
    xml.end("Dimensions")
}

fn write_annotations(xml: &mut XmlOut, annotations: &[Annotation]) -> Result<(), EfmiError> {
    if annotations.is_empty() {
        return Ok(());
    }
    xml.start(BytesStart::new("Annotations"))?;
    for annotation in annotations {
        let mut el = BytesStart::new("Annotation");
        el.push_attribute(("type", annotation.annotation_type.as_str()));
        xml.empty(el)?;
    }
    xml.end("Annotations")
}

// ---- Production Code manifest writers ---------------------------------

/// The single `ManifestReferences/ManifestReference`. With exactly one
/// reference it is necessarily the origin (the XSD requires exactly one
/// `origin="true"`), so the attribute is a serializer constant.
fn write_manifest_references(
    xml: &mut XmlOut,
    reference: &ManifestReference,
) -> Result<(), EfmiError> {
    xml.start(BytesStart::new("ManifestReferences"))?;
    let mut el = BytesStart::new("ManifestReference");
    el.push_attribute(("id", reference.id.as_str()));
    el.push_attribute((
        "manifestRefId",
        reference.manifest_ref_id.to_string().as_str(),
    ));
    el.push_attribute(("checksum", reference.checksum.as_str()));
    el.push_attribute(("origin", "true"));
    xml.empty(el)?;
    xml.end("ManifestReferences")
}

fn write_code_container(xml: &mut XmlOut, container: &CodeContainer) -> Result<(), EfmiError> {
    let mut el = BytesStart::new("CodeContainer");
    el.push_attribute(("language", container.language.as_str()));
    el.push_attribute(("standard", container.standard.as_str()));
    el.push_attribute(("platform", container.platform.as_str()));
    el.push_attribute(("floatPrecision", container.float_precision.as_str()));
    push_opt(
        &mut el,
        "description",
        container.description.as_ref().map(|v| v.as_str()),
    );
    xml.start(el)?;
    // Required element; always emitted explicitly (the XSD default "Generic"
    // would only fill an empty element).
    xml.text_element("Target", container.target.as_str())?;
    write_target_types(xml, &container.target_types)?;
    write_code_files(xml, &container.code_files)?;
    write_logical_data(xml, &container.logical_data)?;
    xml.end("CodeContainer")
}

/// Optional wrapper (0..1, inner 1..unbounded): emitted only when non-empty.
fn write_target_types(xml: &mut XmlOut, target_types: &[TargetType]) -> Result<(), EfmiError> {
    if target_types.is_empty() {
        return Ok(());
    }
    xml.start(BytesStart::new("TargetTypes"))?;
    for target_type in target_types {
        let mut el = BytesStart::new("TargetType");
        el.push_attribute(("id", target_type.id.as_str()));
        el.push_attribute(("kind", target_type.kind.as_str()));
        el.push_attribute(("codedType", target_type.coded_type.as_str()));
        xml.empty(el)?;
    }
    xml.end("TargetTypes")
}

/// Required wrapper (1..1, inner 1..unbounded; emptiness is rejected at
/// model construction, EFM043).
fn write_code_files(xml: &mut XmlOut, code_files: &[CodeFile]) -> Result<(), EfmiError> {
    xml.start(BytesStart::new("CodeFiles"))?;
    for code_file in code_files {
        write_code_file(xml, code_file)?;
    }
    xml.end("CodeFiles")
}

fn write_code_file(xml: &mut XmlOut, code_file: &CodeFile) -> Result<(), EfmiError> {
    let mut el = BytesStart::new("CodeFile");
    el.push_attribute(("id", code_file.id.as_str()));
    el.push_attribute(("fileType", code_file.file_type.as_str()));
    el.push_attribute(("codeType", code_file.code_type.as_str()));
    xml.start(el)?;
    let mut file_reference = BytesStart::new("FileReference");
    file_reference.push_attribute(("fileRefId", code_file.file_ref_id.as_str()));
    push_opt(
        &mut file_reference,
        "kind",
        code_file.file_ref_kind.as_ref().map(|v| v.as_str()),
    );
    xml.empty(file_reference)?;
    write_includes(xml, &code_file.includes)?;
    write_typedefs(xml, &code_file.typedefs)?;
    write_functions(xml, &code_file.functions)?;
    xml.end("CodeFile")
}

/// Optional wrapper: emitted only when non-empty.
fn write_includes(xml: &mut XmlOut, includes: &[Identifier]) -> Result<(), EfmiError> {
    if includes.is_empty() {
        return Ok(());
    }
    xml.start(BytesStart::new("Includes"))?;
    for include in includes {
        let mut el = BytesStart::new("Include");
        el.push_attribute(("codeFileRefId", include.as_str()));
        xml.empty(el)?;
    }
    xml.end("Includes")
}

/// Optional wrapper: emitted only when non-empty.
fn write_typedefs(xml: &mut XmlOut, typedefs: &[Typedef]) -> Result<(), EfmiError> {
    if typedefs.is_empty() {
        return Ok(());
    }
    xml.start(BytesStart::new("Typedefs"))?;
    for typedef in typedefs {
        let mut el = BytesStart::new("Typedef");
        el.push_attribute(("id", typedef.id.as_str()));
        el.push_attribute(("name", typedef.name.as_str()));
        xml.start(el)?;
        write_typedef_body(xml, &typedef.body)?;
        xml.end("Typedef")?;
    }
    xml.end("Typedefs")
}

fn write_typedef_body(xml: &mut XmlOut, body: &TypedefBody) -> Result<(), EfmiError> {
    match body {
        TypedefBody::Alias {
            target_type_ref_id,
            type_def_ref_id,
        } => {
            let mut el = BytesStart::new("Alias");
            el.push_attribute(("targetTypeRefId", target_type_ref_id.as_str()));
            push_opt(
                &mut el,
                "typeDefRefId",
                type_def_ref_id.as_ref().map(|i| i.as_str()),
            );
            xml.empty(el)
        }
        TypedefBody::Components(components) => {
            xml.start(BytesStart::new("Components"))?;
            for component in components {
                write_component(xml, component)?;
            }
            xml.end("Components")
        }
    }
}

fn write_component(xml: &mut XmlOut, component: &Component) -> Result<(), EfmiError> {
    let mut el = BytesStart::new("Component");
    el.push_attribute(("id", component.id.as_str()));
    el.push_attribute(("name", component.name.as_str()));
    el.push_attribute(("typeDefRefId", component.type_def_ref_id.as_str()));
    if component.pointer {
        el.push_attribute(("pointer", "true"));
    }
    if component.dimensions.is_empty() {
        return xml.empty(el);
    }
    xml.start(el)?;
    write_dimensions_zero_based(xml, &component.dimensions)?;
    xml.end("Component")
}

/// Optional wrapper: emitted only when non-empty.
fn write_functions(xml: &mut XmlOut, functions: &[Function]) -> Result<(), EfmiError> {
    if functions.is_empty() {
        return Ok(());
    }
    xml.start(BytesStart::new("Functions"))?;
    for function in functions {
        let mut el = BytesStart::new("Function");
        el.push_attribute(("id", function.id.as_str()));
        el.push_attribute(("name", function.name.as_str()));
        xml.start(el)?;
        let mut return_parameter = BytesStart::new("ReturnParameter");
        push_parameter_core(&mut return_parameter, &function.return_parameter);
        write_parameter_body(
            xml,
            "ReturnParameter",
            return_parameter,
            &function.return_parameter.dimensions,
        )?;
        write_formal_parameters(xml, &function.formal_parameters)?;
        xml.end("Function")?;
    }
    xml.end("Functions")
}

/// Optional wrapper: emitted only when non-empty. `FormalParameter/@number`
/// is derived from Vec position, 0-based (decision D1).
fn write_formal_parameters(
    xml: &mut XmlOut,
    parameters: &[FormalParameter],
) -> Result<(), EfmiError> {
    if parameters.is_empty() {
        return Ok(());
    }
    xml.start(BytesStart::new("FormalParameters"))?;
    for (number, parameter) in parameters.iter().enumerate() {
        let mut el = BytesStart::new("FormalParameter");
        el.push_attribute(("name", parameter.name.as_str()));
        el.push_attribute(("number", number.to_string().as_str()));
        push_parameter_core(&mut el, &parameter.core);
        write_parameter_body(xml, "FormalParameter", el, &parameter.core.dimensions)?;
    }
    xml.end("FormalParameters")
}

/// The `FunctionParameterAttributes` group in XSD declaration order.
/// XSD-defaulted booleans are emitted only when non-default (`true`).
fn push_parameter_core(el: &mut BytesStart<'_>, core: &ParameterCore) {
    el.push_attribute(("id", core.id.as_str()));
    el.push_attribute(("typeDefRefId", core.type_def_ref_id.as_str()));
    if core.constant {
        el.push_attribute(("const", "true"));
    }
    if core.pointer {
        el.push_attribute(("pointer", "true"));
    }
    if core.const_pointer {
        el.push_attribute(("constPointer", "true"));
    }
}

/// Close a parameter element: self-closing when scalar, otherwise with its
/// `Dimensions` child.
fn write_parameter_body(
    xml: &mut XmlOut,
    tag: &str,
    el: BytesStart<'_>,
    dimensions: &[u64],
) -> Result<(), EfmiError> {
    if dimensions.is_empty() {
        return xml.empty(el);
    }
    xml.start(el)?;
    write_dimensions_zero_based(xml, dimensions)?;
    xml.end(tag)
}

/// Production Code `Dimension/@number` is derived from Vec position,
/// 0-based (decision D1). The Algorithm Code schema explicitly documents
/// 1-based numbering, so [`write_dimensions`] differs deliberately — two
/// schemas, two conventions.
fn write_dimensions_zero_based(xml: &mut XmlOut, dimensions: &[u64]) -> Result<(), EfmiError> {
    if dimensions.is_empty() {
        return Ok(());
    }
    xml.start(BytesStart::new("Dimensions"))?;
    for (number, size) in dimensions.iter().enumerate() {
        let mut el = BytesStart::new("Dimension");
        el.push_attribute(("number", number.to_string().as_str()));
        el.push_attribute(("size", size.to_string().as_str()));
        xml.empty(el)?;
    }
    xml.end("Dimensions")
}

/// `LogicalData` with its two ALWAYS-emitted wrappers (1..1 in the XSD with
/// optional children — the inverse of the absent-when-empty list rule).
fn write_logical_data(xml: &mut XmlOut, logical_data: &LogicalData) -> Result<(), EfmiError> {
    xml.start(BytesStart::new("LogicalData"))?;
    if logical_data.data_references.is_empty() {
        xml.empty(BytesStart::new("DataReferences"))?;
    } else {
        xml.start(BytesStart::new("DataReferences"))?;
        for reference in &logical_data.data_references {
            xml.start(BytesStart::new("DataReference"))?;
            xml.empty(foreign_reference_element(
                "ForeignVariableReference",
                &reference.foreign,
            ))?;
            let mut formal_parameter = BytesStart::new("FormalParameter");
            formal_parameter.push_attribute((
                "formalParameterRefId",
                reference.formal_parameter_ref_id.as_str(),
            ));
            push_opt(
                &mut formal_parameter,
                "componentIdentifier",
                reference.component_identifier.as_ref().map(|v| v.as_str()),
            );
            xml.empty(formal_parameter)?;
            xml.end("DataReference")?;
        }
        xml.end("DataReferences")?;
    }
    if logical_data.function_references.is_empty() {
        xml.empty(BytesStart::new("FunctionReferences"))?;
    } else {
        xml.start(BytesStart::new("FunctionReferences"))?;
        for reference in &logical_data.function_references {
            xml.start(BytesStart::new("FunctionReference"))?;
            xml.empty(foreign_reference_element(
                "ForeignFunctionReference",
                &reference.foreign,
            ))?;
            let mut global_function = BytesStart::new("GlobalFunction");
            global_function.push_attribute(("functionRefId", reference.function_ref_id.as_str()));
            xml.empty(global_function)?;
            xml.end("FunctionReference")?;
        }
        xml.end("FunctionReferences")?;
    }
    xml.end("LogicalData")
}

/// A `ForeignReference`-typed element, attributes in XSD declaration order.
fn foreign_reference_element<'a>(tag: &'a str, foreign: &ForeignReference) -> BytesStart<'a> {
    let mut el = BytesStart::new(tag);
    el.push_attribute((
        "manifestReferenceRefId",
        foreign.manifest_reference_ref_id.as_str(),
    ));
    el.push_attribute(("foreignRefId", foreign.foreign_ref_id.as_str()));
    el
}

/// Render a finite Real with an explicit decimal point (`2` → `2.0`), using
/// Rust's shortest round-trip formatting. Deterministic; non-finite values
/// have no `xs:double` lexical form under this scheme and are rejected at
/// model construction (EFM022/EFM033), so they can never reach this point.
fn format_real(value: f64) -> String {
    debug_assert!(
        value.is_finite(),
        "non-finite Reals must be rejected at model construction"
    );
    let rendered = format!("{value}");
    if rendered.contains('.') {
        rendered
    } else {
        format!("{rendered}.0")
    }
}

fn real_start(start: &StartValue<f64>) -> String {
    join_start(start, |v| format_real(*v))
}

fn integer_start(start: &StartValue<i32>) -> String {
    join_start(start, |v| v.to_string())
}

fn boolean_start(start: &StartValue<bool>) -> String {
    join_start(start, |v| if *v { "true".into() } else { "false".into() })
}

/// Row-major whitespace-separated encoding of a start value.
fn join_start<T>(start: &StartValue<T>, render: impl Fn(&T) -> String) -> String {
    start.iter().map(render).collect::<Vec<_>>().join(" ")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checksum::Sha1Hex;
    use crate::content::{ContentParts, ModelRepresentation, ModelRepresentationKind};
    use crate::ids::{ManifestId, NameWithoutSlashes, NormalizedText, UtcTimestamp};

    /// Every rendering must be a valid `xs:double` lexical form: an optional
    /// sign, decimal digits, and exactly one `.` — never exponent notation,
    /// `inf`, or `NaN`.
    fn assert_xs_double_lexical(rendered: &str) {
        let digits = rendered.strip_prefix('-').unwrap_or(rendered);
        assert_eq!(
            digits.matches('.').count(),
            1,
            "`{rendered}` must contain exactly one decimal point"
        );
        assert!(
            digits.chars().all(|c| c.is_ascii_digit() || c == '.'),
            "`{rendered}` must contain only digits and a decimal point"
        );
    }

    #[test]
    fn format_real_has_explicit_decimal_point() {
        assert_eq!(format_real(2.0), "2.0");
        assert_eq!(format_real(0.1), "0.1");
        assert_eq!(format_real(-10.0), "-10.0");
        // Extreme magnitudes: Rust's f64 Display never emits exponent
        // notation, so the rendering must stay a plain decimal literal. This
        // catches any future switch to an exponent-producing formatter.
        for value in [1.0e300, -1.0e300, 5.0e-324, f64::MAX, f64::MIN_POSITIVE] {
            assert_xs_double_lexical(&format_real(value));
        }
        assert!(format_real(1.0e300).ends_with(".0"));
    }

    /// `activeFmu` is emitted in its XSD declaration position: directly after
    /// `xsdVersion`, before the shared manifest attribute group.
    #[test]
    fn active_fmu_emitted_in_xsd_attribute_position() {
        let content = Content::new(ContentParts {
            attributes: ManifestAttributes {
                id: ManifestId::parse("{2f1a03de-9f14-4a3d-8b1e-73c60d0a1c11}").unwrap(),
                name: NormalizedText::new("TestBlock").unwrap(),
                description: None,
                version: None,
                generation_date_and_time: UtcTimestamp::parse("2026-07-02T12:00:00Z").unwrap(),
                generation_tool: None,
                copyright: None,
                license: None,
            },
            active_fmu: Some(NameWithoutSlashes::new("AlgorithmCode").unwrap()),
            model_representations: vec![ModelRepresentation {
                name: NameWithoutSlashes::new("AlgorithmCode").unwrap(),
                kind: ModelRepresentationKind::AlgorithmCode,
                manifest: NameWithoutSlashes::new("manifest.xml").unwrap(),
                checksum: Sha1Hex::of_bytes(b"abc"),
                manifest_ref_id: ManifestId::parse("{7d6b1a52-4f0e-4d59-9d3b-215f8e5b6a20}")
                    .unwrap(),
            }],
        })
        .unwrap();
        let rendered = String::from_utf8(content_to_xml(&content).unwrap()).unwrap();
        assert!(
            rendered.contains(
                "xsdVersion=\"0.11.0\" activeFmu=\"AlgorithmCode\" efmiVersion=\"1.0.0\""
            ),
            "activeFmu must sit between xsdVersion and the attribute group:\n{rendered}"
        );
    }
}
