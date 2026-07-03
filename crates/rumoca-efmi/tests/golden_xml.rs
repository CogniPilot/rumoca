//! End-to-end fixture set: a full Algorithm Code manifest, its
//! `__content.xml`, and a full Production Code manifest, serialized and
//! asserted against independently authored expected XML (GAL-021, GAL-023 —
//! fixtures written from the XSDs, not copied from the standard), plus
//! byte-determinism and checksum-flow tests.

use rumoca_efmi::algorithm_code_manifest::BooleanVariable;
use rumoca_efmi::algorithm_code_manifest::{
    AlgorithmCodeManifest, AlgorithmCodeManifestParts, BlockCausality, BlockMethod, BlockMethods,
    Clock, ErrorSignal, ErrorSignalStatus, IntegerVariable, RealVariable, StartValue, Variable,
    VariableCommon,
};
use rumoca_efmi::content::{Content, ContentParts, ModelRepresentation, ModelRepresentationKind};
use rumoca_efmi::manifest_common::{
    BaseUnit, File, FileChecksum, FileRole, ManifestAttributes, Unit,
};
use rumoca_efmi::production_code_manifest::{
    CodeContainer, CodeFile, CodeFileType, CodeType, Component, DataReference, FloatPrecision,
    ForeignReference, FormalParameter, Function, FunctionReference, LogicalData, ManifestReference,
    ParameterCore, ProductionCodeManifest, ProductionCodeManifestParts, SupportedLanguage,
    SupportedPlatform, TargetType, TargetTypeKind, Typedef, TypedefBody,
};
use rumoca_efmi::{
    FilePath, Identifier, ManifestId, NameWithoutSlashes, NormalizedText, Sha1Hex, UtcTimestamp,
    algorithm_code_manifest_to_xml, content_to_xml, production_code_manifest_to_xml,
};

const MANIFEST_UUID: &str = "{7d6b1a52-4f0e-4d59-9d3b-215f8e5b6a20}";
const CONTENT_UUID: &str = "{2f1a03de-9f14-4a3d-8b1e-73c60d0a1c11}";
const PC_MANIFEST_UUID: &str = "{3c9aa74e-1d2f-4b6a-9e07-51c4f0d88b31}";
const TIMESTAMP: &str = "2026-07-02T12:00:00Z";

fn ident(value: &str) -> Identifier {
    Identifier::new(value).unwrap()
}

fn text(value: &str) -> NormalizedText {
    NormalizedText::new(value).unwrap()
}

fn file_name(value: &str) -> NameWithoutSlashes {
    NameWithoutSlashes::new(value).unwrap()
}

fn common(id: &str, name: &str, causality: BlockCausality) -> VariableCommon {
    VariableCommon {
        id: ident(id),
        name: text(name),
        description: None,
        block_causality: causality,
        dimensions: vec![],
        annotations: vec![],
    }
}

fn real(id: &str, name: &str, causality: BlockCausality, start: f64) -> RealVariable {
    RealVariable {
        common: common(id, name, causality),
        start: StartValue::Scalar(start),
        unit_ref_id: None,
        relative_quantity: false,
        min: None,
        max: None,
        nominal: None,
    }
}

fn fixture_variables() -> Vec<Variable> {
    let mut y = real("V_Y", "y", BlockCausality::Output, 0.0);
    y.min = Some(-10.0);
    y.max = Some(10.0);
    let mut sample_period = real("V_T", "T", BlockCausality::Constant, 0.01);
    sample_period.unit_ref_id = Some(ident("U_S"));
    let mut gain_matrix = real("V_A", "A", BlockCausality::Constant, 0.0);
    gain_matrix.common.dimensions = vec![2, 2];
    gain_matrix.start = StartValue::Array(vec![1.0, 0.0, 0.0, 1.0]);
    vec![
        Variable::Real(real("V_U", "u", BlockCausality::Input, 0.0)),
        Variable::Real(y),
        Variable::Real(real("V_K", "k", BlockCausality::TunableParameter, 2.0)),
        Variable::Real(real("V_K2", "k2", BlockCausality::DependentParameter, 4.0)),
        Variable::Real(sample_period),
        Variable::Real(real("V_PREV_X", "previous(x)", BlockCausality::State, 0.0)),
        Variable::Real(gain_matrix),
        Variable::Integer(IntegerVariable {
            common: common("V_N", "n", BlockCausality::Constant),
            start: StartValue::Scalar(4),
            min: Some(1),
            max: Some(8),
        }),
        Variable::Boolean(BooleanVariable {
            common: common("V_FIRST", "firstTick", BlockCausality::State),
            start: StartValue::Scalar(true),
        }),
    ]
}

fn fixture_files() -> Vec<File> {
    vec![
        File {
            id: ident("F_ALG"),
            name: file_name("RumocaTest.Pi.alg"),
            path: FilePath::root(),
            checksum: FileChecksum::Sha1(Sha1Hex::of_bytes(b"abc")),
            role: FileRole::Code,
            description: None,
        },
        File {
            id: ident("F_MANIFEST"),
            name: file_name("manifest.xml"),
            path: FilePath::root(),
            checksum: FileChecksum::NotNeeded,
            role: FileRole::Manifest,
            description: None,
        },
    ]
}

fn fixture_attributes(uuid: &str) -> ManifestAttributes {
    ManifestAttributes {
        id: ManifestId::parse(uuid).unwrap(),
        name: text("RumocaTest.Pi"),
        description: Some(text("Discrete PI test block")),
        version: Some(text("0.1")),
        generation_date_and_time: UtcTimestamp::parse(TIMESTAMP).unwrap(),
        generation_tool: Some(text("rumoca")),
        copyright: None,
        license: None,
    }
}

fn fixture_manifest() -> AlgorithmCodeManifest {
    AlgorithmCodeManifest::new(AlgorithmCodeManifestParts {
        attributes: fixture_attributes(MANIFEST_UUID),
        file_ref_id: ident("F_ALG"),
        files: fixture_files(),
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
                signals: vec![ErrorSignal::Nan, ErrorSignal::UnspecifiedError],
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
    let mut attributes = fixture_attributes(CONTENT_UUID);
    attributes.description = None;
    attributes.version = None;
    Content::new(ContentParts {
        attributes,
        active_fmu: None,
        model_representations: vec![ModelRepresentation {
            name: NameWithoutSlashes::new("AlgorithmCode").unwrap(),
            kind: ModelRepresentationKind::AlgorithmCode,
            manifest: NameWithoutSlashes::new("manifest.xml").unwrap(),
            checksum: manifest_checksum,
            manifest_ref_id: ManifestId::parse(MANIFEST_UUID).unwrap(),
        }],
    })
    .expect("fixture content must validate")
}

const EXPECTED_MANIFEST_XML: &str = r#"<?xml version="1.0" encoding="UTF-8"?>
<Manifest xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="../schemas/AlgorithmCode/efmiAlgorithmCodeManifest.xsd" xsdVersion="0.14.0" kind="AlgorithmCode" efmiVersion="1.0.0" id="{7d6b1a52-4f0e-4d59-9d3b-215f8e5b6a20}" name="RumocaTest.Pi" description="Discrete PI test block" version="0.1" generationDateAndTime="2026-07-02T12:00:00Z" generationTool="rumoca" fileRefId="F_ALG">
  <Files>
    <File id="F_ALG" name="RumocaTest.Pi.alg" path="./" needsChecksum="true" checksum="a9993e364706816aba3e25717850c26c9cd0d89d" role="Code"/>
    <File id="F_MANIFEST" name="manifest.xml" path="./" needsChecksum="false" role="Manifest"/>
  </Files>
  <Clock id="CLK" variableRefId="V_T"/>
  <BlockMethods>
    <BlockMethod id="BM_STARTUP" kind="Startup"/>
    <BlockMethod id="BM_RECALIBRATE" kind="Recalibrate"/>
    <BlockMethod id="BM_DOSTEP" kind="DoStep">
      <Signals>
        <Signal value="NAN"/>
        <Signal value="UNSPECIFIED_ERROR"/>
      </Signals>
    </BlockMethod>
  </BlockMethods>
  <ErrorSignalStatus id="ESS"/>
  <Units>
    <Unit id="U_S" name="s">
      <BaseUnit s="1"/>
    </Unit>
  </Units>
  <Variables>
    <RealVariable id="V_U" name="u" blockCausality="input" start="0.0"/>
    <RealVariable id="V_Y" name="y" blockCausality="output" start="0.0" min="-10.0" max="10.0"/>
    <RealVariable id="V_K" name="k" blockCausality="tunableParameter" start="2.0"/>
    <RealVariable id="V_K2" name="k2" blockCausality="dependentParameter" start="4.0"/>
    <RealVariable id="V_T" name="T" blockCausality="constant" start="0.01" unitRefId="U_S"/>
    <RealVariable id="V_PREV_X" name="previous(x)" blockCausality="state" start="0.0"/>
    <RealVariable id="V_A" name="A" blockCausality="constant" start="1.0 0.0 0.0 1.0">
      <Dimensions>
        <Dimension number="1" size="2"/>
        <Dimension number="2" size="2"/>
      </Dimensions>
    </RealVariable>
    <IntegerVariable id="V_N" name="n" blockCausality="constant" start="4" min="1" max="8"/>
    <BooleanVariable id="V_FIRST" name="firstTick" blockCausality="state" start="true"/>
  </Variables>
</Manifest>
"#;

const EXPECTED_CONTENT_XML: &str = r#"<?xml version="1.0" encoding="UTF-8"?>
<Content xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="schemas/efmiContainerManifest.xsd" xsdVersion="0.11.0" efmiVersion="1.0.0" id="{2f1a03de-9f14-4a3d-8b1e-73c60d0a1c11}" name="RumocaTest.Pi" generationDateAndTime="2026-07-02T12:00:00Z" generationTool="rumoca">
  <ModelRepresentation name="AlgorithmCode" kind="AlgorithmCode" manifest="manifest.xml" checksum="da39a3ee5e6b4b0d3255bfef95601890afd80709" manifestRefId="{7d6b1a52-4f0e-4d59-9d3b-215f8e5b6a20}"/>
</Content>
"#;

#[test]
fn golden_algorithm_code_manifest() {
    let bytes = algorithm_code_manifest_to_xml(&fixture_manifest()).unwrap();
    let rendered = String::from_utf8(bytes).unwrap();
    assert_eq!(rendered, EXPECTED_MANIFEST_XML);
}

#[test]
fn golden_content_xml() {
    let checksum = Sha1Hex::parse("da39a3ee5e6b4b0d3255bfef95601890afd80709").unwrap();
    let bytes = content_to_xml(&fixture_content(checksum)).unwrap();
    let rendered = String::from_utf8(bytes).unwrap();
    assert_eq!(rendered, EXPECTED_CONTENT_XML);
}

/// Serializing the same logical model twice — including from two separately
/// constructed fixtures — yields identical bytes (checksums depend on it).
#[test]
fn serialization_is_byte_deterministic() {
    let manifest_a = fixture_manifest();
    let manifest_b = fixture_manifest();
    let bytes_a1 = algorithm_code_manifest_to_xml(&manifest_a).unwrap();
    let bytes_a2 = algorithm_code_manifest_to_xml(&manifest_a).unwrap();
    let bytes_b = algorithm_code_manifest_to_xml(&manifest_b).unwrap();
    assert_eq!(bytes_a1, bytes_a2);
    assert_eq!(bytes_a1, bytes_b);

    let checksum = Sha1Hex::of_bytes(&bytes_a1);
    let content = fixture_content(checksum);
    assert_eq!(
        content_to_xml(&content).unwrap(),
        content_to_xml(&content).unwrap()
    );
}

/// GAL-021: the checksum recorded in `__content.xml` is the SHA-1 of the
/// exact manifest bytes, never a placeholder.
#[test]
fn content_checksum_flows_from_manifest_bytes() {
    let manifest_bytes = algorithm_code_manifest_to_xml(&fixture_manifest()).unwrap();
    let checksum = Sha1Hex::of_bytes(&manifest_bytes);
    let content_bytes = content_to_xml(&fixture_content(checksum.clone())).unwrap();
    let rendered = String::from_utf8(content_bytes).unwrap();
    assert!(
        rendered.contains(&format!("checksum=\"{}\"", checksum.as_str())),
        "content must carry the real manifest digest"
    );
    // Recomputation over the same bytes matches (no normalization anywhere).
    assert_eq!(Sha1Hex::of_bytes(&manifest_bytes), checksum);
}

/// Attribute values pass through XML escaping.
#[test]
fn attribute_values_are_escaped() {
    let mut manifest_parts = fixture_manifest().parts().clone();
    manifest_parts.attributes.description = Some(text(r#"a<b&"c'd>e"#));
    let manifest = AlgorithmCodeManifest::new(manifest_parts).unwrap();
    let rendered = String::from_utf8(algorithm_code_manifest_to_xml(&manifest).unwrap()).unwrap();
    assert!(
        rendered.contains("description=\"a&lt;b&amp;&quot;c&apos;d&gt;e\""),
        "special characters must be escaped, got: {rendered}"
    );
}

// ---- Production Code manifest fixture (authored from the ProductionCode
// XSDs, GAL-023) ---------------------------------------------------------

fn pc_alias_typedef(id: &str, name: &str, target_type_id: &str) -> Typedef {
    Typedef {
        id: ident(id),
        name: text(name),
        body: TypedefBody::Alias {
            target_type_ref_id: ident(target_type_id),
            type_def_ref_id: None,
        },
    }
}

fn pc_component(id: &str, name: &str, type_def_id: &str, dimensions: Vec<u64>) -> Component {
    Component {
        id: ident(id),
        name: text(name),
        type_def_ref_id: ident(type_def_id),
        dimensions,
        pointer: false,
    }
}

fn pc_parameter(id: &str, type_def_id: &str) -> ParameterCore {
    ParameterCore {
        id: ident(id),
        type_def_ref_id: ident(type_def_id),
        constant: false,
        pointer: false,
        const_pointer: false,
        dimensions: vec![],
    }
}

fn pc_self_parameter(id: &str) -> FormalParameter {
    let mut core = pc_parameter(id, "TD_STATE");
    core.pointer = true;
    FormalParameter {
        name: text("self"),
        core,
    }
}

fn pc_function(
    id: &str,
    name: &str,
    return_parameter_id: &str,
    formal_parameters: Vec<FormalParameter>,
) -> Function {
    Function {
        id: ident(id),
        name: text(name),
        return_parameter: pc_parameter(return_parameter_id, "TD_VOID"),
        formal_parameters,
    }
}

fn pc_data_reference(foreign_id: &str, component: &str) -> DataReference {
    DataReference {
        foreign: ForeignReference {
            manifest_reference_ref_id: ident("MR_AC"),
            foreign_ref_id: ident(foreign_id),
        },
        formal_parameter_ref_id: ident("FP_DOSTEP_SELF"),
        component_identifier: Some(text(component)),
    }
}

fn pc_function_reference(foreign_id: &str, function_id: &str) -> FunctionReference {
    FunctionReference {
        foreign: ForeignReference {
            manifest_reference_ref_id: ident("MR_AC"),
            foreign_ref_id: ident(foreign_id),
        },
        function_ref_id: ident(function_id),
    }
}

fn pc_code_file_entry(id: &str, name: &str, content: &[u8]) -> File {
    File {
        id: ident(id),
        name: file_name(name),
        path: FilePath::root(),
        checksum: FileChecksum::Sha1(Sha1Hex::of_bytes(content)),
        role: FileRole::Code,
        description: None,
    }
}

fn pc_header_code_file() -> CodeFile {
    CodeFile {
        id: ident("CF_H"),
        file_type: CodeFileType::ProductionCode,
        code_type: CodeType::HeaderFile,
        file_ref_id: ident("F_H"),
        file_ref_kind: None,
        includes: vec![],
        typedefs: vec![
            pc_alias_typedef("TD_F64", "double", "TT_F64"),
            pc_alias_typedef("TD_BOOL", "bool", "TT_BOOL"),
            pc_alias_typedef("TD_VOID", "void", "TT_VOID"),
            Typedef {
                id: ident("TD_REAL"),
                name: text("Real"),
                body: TypedefBody::Alias {
                    target_type_ref_id: ident("TT_F64"),
                    type_def_ref_id: Some(ident("TD_F64")),
                },
            },
            Typedef {
                id: ident("TD_STATE"),
                name: text("TestBlock_state"),
                body: TypedefBody::Components(vec![
                    pc_component("CO_X", "x", "TD_F64", vec![]),
                    pc_component("CO_A", "A", "TD_F64", vec![2, 3]),
                    Component {
                        pointer: true,
                        ..pc_component("CO_FLAG", "flag", "TD_BOOL", vec![])
                    },
                ]),
            },
        ],
        functions: vec![],
    }
}

fn pc_source_code_file() -> CodeFile {
    let mut u_core = pc_parameter("FP_DOSTEP_U", "TD_F64");
    u_core.constant = true;
    u_core.pointer = true;
    u_core.const_pointer = true;
    u_core.dimensions = vec![2];
    CodeFile {
        id: ident("CF_C"),
        file_type: CodeFileType::ProductionCode,
        code_type: CodeType::SourceFile,
        file_ref_id: ident("F_C"),
        file_ref_kind: Some(text("implementation")),
        includes: vec![ident("CF_H")],
        typedefs: vec![],
        functions: vec![
            pc_function(
                "FN_STARTUP",
                "TestBlock_startup",
                "RP_STARTUP",
                vec![pc_self_parameter("FP_STARTUP_SELF")],
            ),
            pc_function(
                "FN_RECALIBRATE",
                "TestBlock_recalibrate",
                "RP_RECALIBRATE",
                vec![pc_self_parameter("FP_RECALIBRATE_SELF")],
            ),
            pc_function(
                "FN_DOSTEP",
                "TestBlock_dostep",
                "RP_DOSTEP",
                vec![
                    pc_self_parameter("FP_DOSTEP_SELF"),
                    FormalParameter {
                        name: text("u"),
                        core: u_core,
                    },
                ],
            ),
        ],
    }
}

/// Full Production Code manifest, parametrized over the identity fields so
/// determinism tests can vary exactly UUID and timestamp.
fn pc_fixture_with(uuid: &str, timestamp: &str) -> ProductionCodeManifest {
    ProductionCodeManifest::new(ProductionCodeManifestParts {
        attributes: ManifestAttributes {
            id: ManifestId::parse(uuid).unwrap(),
            name: text("TestBlock"),
            description: Some(text("Production code for TestBlock")),
            version: None,
            generation_date_and_time: UtcTimestamp::parse(timestamp).unwrap(),
            generation_tool: Some(text("rumoca")),
            copyright: None,
            license: None,
        },
        manifest_reference: ManifestReference {
            id: ident("MR_AC"),
            manifest_ref_id: ManifestId::parse(MANIFEST_UUID).unwrap(),
            checksum: Sha1Hex::of_bytes(b"synthetic algorithm code manifest bytes"),
        },
        files: vec![
            pc_code_file_entry(
                "F_H",
                "TestBlock.h",
                b"/* synthetic header authored for rumoca tests */",
            ),
            pc_code_file_entry(
                "F_C",
                "TestBlock.c",
                b"/* synthetic source authored for rumoca tests */",
            ),
        ],
        code_container: CodeContainer {
            language: SupportedLanguage::C,
            standard: text("C99"),
            platform: SupportedPlatform::Legacy,
            float_precision: FloatPrecision::Bits64,
            description: Some(text("Generated C99 code")),
            target: text("Generic"),
            target_types: vec![
                TargetType {
                    id: ident("TT_F64"),
                    kind: TargetTypeKind::EfmiFloat64,
                    coded_type: text("double"),
                },
                TargetType {
                    id: ident("TT_BOOL"),
                    kind: TargetTypeKind::EfmiBool,
                    coded_type: text("bool"),
                },
                TargetType {
                    id: ident("TT_VOID"),
                    kind: TargetTypeKind::EfmiVoid,
                    coded_type: text("void"),
                },
            ],
            code_files: vec![pc_header_code_file(), pc_source_code_file()],
            logical_data: LogicalData {
                data_references: vec![
                    pc_data_reference("V_X", "x"),
                    pc_data_reference("V_A", "A"),
                    pc_data_reference("V_FLAG", "flag"),
                ],
                function_references: vec![
                    pc_function_reference("BM_STARTUP", "FN_STARTUP"),
                    pc_function_reference("BM_RECALIBRATE", "FN_RECALIBRATE"),
                    pc_function_reference("BM_DOSTEP", "FN_DOSTEP"),
                ],
            },
        },
        annotations: vec![],
    })
    .expect("fixture production code manifest must validate")
}

fn pc_fixture_manifest() -> ProductionCodeManifest {
    pc_fixture_with(PC_MANIFEST_UUID, TIMESTAMP)
}

const EXPECTED_PC_MANIFEST_XML: &str = r#"<?xml version="1.0" encoding="UTF-8"?>
<Manifest xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="../schemas/ProductionCode/efmiProductionCodeManifest.xsd" xsdVersion="0.17.0" kind="ProductionCode" efmiVersion="1.0.0" id="{3c9aa74e-1d2f-4b6a-9e07-51c4f0d88b31}" name="TestBlock" description="Production code for TestBlock" generationDateAndTime="2026-07-02T12:00:00Z" generationTool="rumoca">
  <ManifestReferences>
    <ManifestReference id="MR_AC" manifestRefId="{7d6b1a52-4f0e-4d59-9d3b-215f8e5b6a20}" checksum="474c7398381045e071a299e9ec2468ed60d6fefb" origin="true"/>
  </ManifestReferences>
  <Files>
    <File id="F_H" name="TestBlock.h" path="./" needsChecksum="true" checksum="64fd18d344a2855d62f8c6caacf0774818dffee7" role="Code"/>
    <File id="F_C" name="TestBlock.c" path="./" needsChecksum="true" checksum="e98dd51d9cf238d8ffb77a04fe0bb1ab6e07230a" role="Code"/>
  </Files>
  <CodeContainer language="C" standard="C99" platform="Legacy" floatPrecision="64-bit" description="Generated C99 code">
    <Target>Generic</Target>
    <TargetTypes>
      <TargetType id="TT_F64" kind="efmiFloat64" codedType="double"/>
      <TargetType id="TT_BOOL" kind="efmiBool" codedType="bool"/>
      <TargetType id="TT_VOID" kind="efmiVoid" codedType="void"/>
    </TargetTypes>
    <CodeFiles>
      <CodeFile id="CF_H" fileType="ProductionCode" codeType="HeaderFile">
        <FileReference fileRefId="F_H"/>
        <Typedefs>
          <Typedef id="TD_F64" name="double">
            <Alias targetTypeRefId="TT_F64"/>
          </Typedef>
          <Typedef id="TD_BOOL" name="bool">
            <Alias targetTypeRefId="TT_BOOL"/>
          </Typedef>
          <Typedef id="TD_VOID" name="void">
            <Alias targetTypeRefId="TT_VOID"/>
          </Typedef>
          <Typedef id="TD_REAL" name="Real">
            <Alias targetTypeRefId="TT_F64" typeDefRefId="TD_F64"/>
          </Typedef>
          <Typedef id="TD_STATE" name="TestBlock_state">
            <Components>
              <Component id="CO_X" name="x" typeDefRefId="TD_F64"/>
              <Component id="CO_A" name="A" typeDefRefId="TD_F64">
                <Dimensions>
                  <Dimension number="0" size="2"/>
                  <Dimension number="1" size="3"/>
                </Dimensions>
              </Component>
              <Component id="CO_FLAG" name="flag" typeDefRefId="TD_BOOL" pointer="true"/>
            </Components>
          </Typedef>
        </Typedefs>
      </CodeFile>
      <CodeFile id="CF_C" fileType="ProductionCode" codeType="SourceFile">
        <FileReference fileRefId="F_C" kind="implementation"/>
        <Includes>
          <Include codeFileRefId="CF_H"/>
        </Includes>
        <Functions>
          <Function id="FN_STARTUP" name="TestBlock_startup">
            <ReturnParameter id="RP_STARTUP" typeDefRefId="TD_VOID"/>
            <FormalParameters>
              <FormalParameter name="self" number="0" id="FP_STARTUP_SELF" typeDefRefId="TD_STATE" pointer="true"/>
            </FormalParameters>
          </Function>
          <Function id="FN_RECALIBRATE" name="TestBlock_recalibrate">
            <ReturnParameter id="RP_RECALIBRATE" typeDefRefId="TD_VOID"/>
            <FormalParameters>
              <FormalParameter name="self" number="0" id="FP_RECALIBRATE_SELF" typeDefRefId="TD_STATE" pointer="true"/>
            </FormalParameters>
          </Function>
          <Function id="FN_DOSTEP" name="TestBlock_dostep">
            <ReturnParameter id="RP_DOSTEP" typeDefRefId="TD_VOID"/>
            <FormalParameters>
              <FormalParameter name="self" number="0" id="FP_DOSTEP_SELF" typeDefRefId="TD_STATE" pointer="true"/>
              <FormalParameter name="u" number="1" id="FP_DOSTEP_U" typeDefRefId="TD_F64" const="true" pointer="true" constPointer="true">
                <Dimensions>
                  <Dimension number="0" size="2"/>
                </Dimensions>
              </FormalParameter>
            </FormalParameters>
          </Function>
        </Functions>
      </CodeFile>
    </CodeFiles>
    <LogicalData>
      <DataReferences>
        <DataReference>
          <ForeignVariableReference manifestReferenceRefId="MR_AC" foreignRefId="V_X"/>
          <FormalParameter formalParameterRefId="FP_DOSTEP_SELF" componentIdentifier="x"/>
        </DataReference>
        <DataReference>
          <ForeignVariableReference manifestReferenceRefId="MR_AC" foreignRefId="V_A"/>
          <FormalParameter formalParameterRefId="FP_DOSTEP_SELF" componentIdentifier="A"/>
        </DataReference>
        <DataReference>
          <ForeignVariableReference manifestReferenceRefId="MR_AC" foreignRefId="V_FLAG"/>
          <FormalParameter formalParameterRefId="FP_DOSTEP_SELF" componentIdentifier="flag"/>
        </DataReference>
      </DataReferences>
      <FunctionReferences>
        <FunctionReference>
          <ForeignFunctionReference manifestReferenceRefId="MR_AC" foreignRefId="BM_STARTUP"/>
          <GlobalFunction functionRefId="FN_STARTUP"/>
        </FunctionReference>
        <FunctionReference>
          <ForeignFunctionReference manifestReferenceRefId="MR_AC" foreignRefId="BM_RECALIBRATE"/>
          <GlobalFunction functionRefId="FN_RECALIBRATE"/>
        </FunctionReference>
        <FunctionReference>
          <ForeignFunctionReference manifestReferenceRefId="MR_AC" foreignRefId="BM_DOSTEP"/>
          <GlobalFunction functionRefId="FN_DOSTEP"/>
        </FunctionReference>
      </FunctionReferences>
    </LogicalData>
  </CodeContainer>
</Manifest>
"#;

/// Golden byte-compare for the Production Code manifest serializer: fixed
/// attributes (`xsdVersion`/`kind`/`efmiVersion`), unconditional
/// `origin="true"`, absent-when-empty vs always-emitted wrappers, 0-based
/// `Dimension`/`FormalParameter` numbering (decision D1 — the Algorithm Code
/// golden above stays 1-based), and only-when-true XSD-defaulted booleans.
#[test]
fn golden_production_code_manifest() {
    let bytes = production_code_manifest_to_xml(&pc_fixture_manifest()).unwrap();
    let rendered = String::from_utf8(bytes).unwrap();
    assert_eq!(rendered, EXPECTED_PC_MANIFEST_XML);
}

/// Serializing the same logical model twice — including from two separately
/// constructed fixtures — yields identical bytes (checksums depend on it).
#[test]
fn production_code_serialization_is_byte_deterministic() {
    let manifest_a = pc_fixture_manifest();
    let manifest_b = pc_fixture_manifest();
    let bytes_a1 = production_code_manifest_to_xml(&manifest_a).unwrap();
    let bytes_a2 = production_code_manifest_to_xml(&manifest_a).unwrap();
    let bytes_b = production_code_manifest_to_xml(&manifest_b).unwrap();
    assert_eq!(bytes_a1, bytes_a2);
    assert_eq!(bytes_a1, bytes_b);
}

/// Two renders of the same parts differ ONLY in the UUID and timestamp
/// fields: masking those two values makes the documents byte-identical,
/// proving the serializer mints no identities and reads no clocks.
#[test]
fn production_code_renders_differ_only_in_uuid_and_timestamp() {
    const OTHER_UUID: &str = "{8f5e21c7-6a4d-4b90-b3e2-0d97c54a1f68}";
    const OTHER_TIMESTAMP: &str = "2027-01-15T08:30:00Z";
    let mask = |rendered: &[u8], uuid: &str, timestamp: &str| {
        let rendered = String::from_utf8(rendered.to_vec()).unwrap();
        assert!(rendered.contains(uuid), "fixture UUID must be emitted");
        assert!(
            rendered.contains(timestamp),
            "fixture timestamp must be emitted"
        );
        rendered
            .replace(uuid, "{MASKED-UUID}")
            .replace(timestamp, "MASKED-TIMESTAMP")
    };
    let base =
        production_code_manifest_to_xml(&pc_fixture_with(PC_MANIFEST_UUID, TIMESTAMP)).unwrap();
    let other =
        production_code_manifest_to_xml(&pc_fixture_with(OTHER_UUID, OTHER_TIMESTAMP)).unwrap();
    assert_ne!(base, other, "identity fields must actually differ");
    assert_eq!(
        mask(&base, PC_MANIFEST_UUID, TIMESTAMP),
        mask(&other, OTHER_UUID, OTHER_TIMESTAMP)
    );
}

/// The `DataReferences`/`FunctionReferences` wrappers are 1..1 in the XSD
/// with optional children: they are emitted even when empty (the inverse of
/// the absent-when-empty rule for the optional list wrappers).
#[test]
fn logical_data_wrappers_always_emitted() {
    let mut parts = pc_fixture_manifest().parts().clone();
    parts.code_container.logical_data = LogicalData {
        data_references: vec![],
        function_references: vec![],
    };
    let manifest = ProductionCodeManifest::new(parts).unwrap();
    let rendered = String::from_utf8(production_code_manifest_to_xml(&manifest).unwrap()).unwrap();
    assert!(
        rendered.contains("<DataReferences/>"),
        "empty DataReferences wrapper must still be emitted:\n{rendered}"
    );
    assert!(
        rendered.contains("<FunctionReferences/>"),
        "empty FunctionReferences wrapper must still be emitted:\n{rendered}"
    );
}
