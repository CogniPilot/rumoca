//! End-to-end fixture pair: a full Algorithm Code manifest and its
//! `__content.xml`, serialized and asserted against independently authored
//! expected XML (GAL-021, GAL-023 — fixture written from the XSDs, not
//! copied from the standard), plus byte-determinism and checksum-flow tests.

use rumoca_efmi::algorithm_code_manifest::BooleanVariable;
use rumoca_efmi::algorithm_code_manifest::{
    AlgorithmCodeManifest, AlgorithmCodeManifestParts, BlockCausality, BlockMethod, BlockMethods,
    Clock, ErrorSignal, ErrorSignalStatus, IntegerVariable, RealVariable, StartValue, Variable,
    VariableCommon,
};
use rumoca_efmi::content::{
    Content, ContentParts, ManifestAttributes, ModelRepresentation, ModelRepresentationKind,
};
use rumoca_efmi::manifest_common::{BaseUnit, File, FileChecksum, FileRole, Unit};
use rumoca_efmi::{
    FilePath, Identifier, ManifestId, NameWithoutSlashes, NormalizedText, Sha1Hex, UtcTimestamp,
    algorithm_code_manifest_to_xml, content_to_xml,
};

const MANIFEST_UUID: &str = "{7d6b1a52-4f0e-4d59-9d3b-215f8e5b6a20}";
const CONTENT_UUID: &str = "{2f1a03de-9f14-4a3d-8b1e-73c60d0a1c11}";
const TIMESTAMP: &str = "2026-07-02T12:00:00Z";

fn ident(value: &str) -> Identifier {
    Identifier::new(value).unwrap()
}

fn text(value: &str) -> NormalizedText {
    NormalizedText::new(value).unwrap()
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
            name: text("RumocaTest.Pi.alg"),
            path: FilePath::root(),
            checksum: FileChecksum::Sha1(Sha1Hex::of_bytes(b"abc")),
            role: FileRole::Code,
            description: None,
        },
        File {
            id: ident("F_MANIFEST"),
            name: text("manifest.xml"),
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
        description: Some("Discrete PI test block".to_owned()),
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
    manifest_parts.attributes.description = Some(r#"a<b&"c'd>e"#.to_owned());
    let manifest = AlgorithmCodeManifest::new(manifest_parts).unwrap();
    let rendered = String::from_utf8(algorithm_code_manifest_to_xml(&manifest).unwrap()).unwrap();
    assert!(
        rendered.contains("description=\"a&lt;b&amp;&quot;c&apos;d&gt;e\""),
        "special characters must be escaped, got: {rendered}"
    );
}
