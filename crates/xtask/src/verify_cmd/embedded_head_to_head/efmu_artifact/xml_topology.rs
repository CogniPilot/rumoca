//! Single-pass, exact-parent XML topology authentication for the three eFMI
//! checksum-web documents.

use anyhow::{Context, Result, ensure};
use quick_xml::Reader;
use quick_xml::encoding::Decoder;
use quick_xml::events::{BytesStart, Event};
use std::collections::BTreeMap;

pub(super) type Attributes = BTreeMap<String, String>;

pub(super) struct EfmuXmlTopology {
    pub(super) container: ContainerTopology,
    pub(super) algorithm: AlgorithmManifestTopology,
    pub(super) production: ProductionManifestTopology,
}

pub(super) struct ContainerTopology {
    pub(super) root: Attributes,
    pub(super) algorithm_representation: Attributes,
    pub(super) production_representation: Attributes,
}

pub(super) struct AlgorithmManifestTopology {
    pub(super) root: Attributes,
    pub(super) source_file: Attributes,
}

pub(super) struct ProductionManifestTopology {
    pub(super) root: Attributes,
    pub(super) algorithm_reference: Attributes,
    pub(super) header_file: Attributes,
    pub(super) source_file: Attributes,
}

#[derive(Clone, Copy)]
enum TopologyElement {
    ModelRepresentation,
    Files,
    File,
    ManifestReferences,
    ManifestReference,
}

impl TopologyElement {
    fn from_name(name: &str) -> Option<Self> {
        match name {
            "ModelRepresentation" => Some(Self::ModelRepresentation),
            "Files" => Some(Self::Files),
            "File" => Some(Self::File),
            "ManifestReferences" => Some(Self::ManifestReferences),
            "ManifestReference" => Some(Self::ManifestReference),
            _ => None,
        }
    }

    const fn name(self) -> &'static str {
        match self {
            Self::ModelRepresentation => "ModelRepresentation",
            Self::Files => "Files",
            Self::File => "File",
            Self::ManifestReferences => "ManifestReferences",
            Self::ManifestReference => "ManifestReference",
        }
    }
}

struct ChildElement<'borrow, 'event> {
    tag: &'borrow BytesStart<'event>,
    decoder: Decoder,
    is_empty: bool,
    member: &'borrow str,
    parents: &'borrow [String],
}

impl ChildElement<'_, '_> {
    fn attributes(&self) -> Result<Attributes> {
        attributes(self.tag, self.decoder, self.member)
    }
}

trait DocumentTopologyState {
    type Output;

    const ROOT: &'static str;

    fn consume(&mut self, element: TopologyElement, child: ChildElement<'_, '_>) -> Result<()>;
    fn finish(self, root: Attributes) -> Result<Self::Output>;
}

struct DocumentParser<State> {
    state: State,
    root: Option<Attributes>,
    stack: Vec<String>,
    root_closed: bool,
    declaration_seen: bool,
}

impl<State> DocumentParser<State>
where
    State: DocumentTopologyState,
{
    fn new(state: State) -> Self {
        Self {
            state,
            root: None,
            stack: Vec::new(),
            root_closed: false,
            declaration_seen: false,
        }
    }

    fn declaration(&mut self, member: &str) -> Result<()> {
        ensure!(
            !self.declaration_seen && self.root.is_none() && self.stack.is_empty(),
            "`{member}` has a misplaced or duplicate XML declaration"
        );
        self.declaration_seen = true;
        Ok(())
    }

    fn start(
        &mut self,
        tag: &BytesStart<'_>,
        decoder: Decoder,
        is_empty: bool,
        member: &str,
    ) -> Result<()> {
        let observed = name(tag.name().as_ref(), member)?;
        ensure!(
            !self.root_closed,
            "`{member}` contains a second document root or content after its root"
        );
        if self.stack.is_empty() {
            return self.start_root(tag, decoder, is_empty, member, observed);
        }
        if let Some(element) = TopologyElement::from_name(&observed) {
            self.state.consume(
                element,
                ChildElement {
                    tag,
                    decoder,
                    is_empty,
                    member,
                    parents: &self.stack,
                },
            )?;
        }
        if !is_empty {
            self.stack.push(observed);
        }
        Ok(())
    }

    fn start_root(
        &mut self,
        tag: &BytesStart<'_>,
        decoder: Decoder,
        is_empty: bool,
        member: &str,
        observed: String,
    ) -> Result<()> {
        ensure!(
            self.root.is_none() && observed == State::ROOT && !is_empty,
            "`{member}` root is `{observed}`; expected one nonempty `{}` root",
            State::ROOT
        );
        self.root = Some(attributes(tag, decoder, member)?);
        self.stack.push(observed);
        Ok(())
    }

    fn end(&mut self, tag_name: &[u8], member: &str) -> Result<()> {
        let observed = name(tag_name, member)?;
        let Some(open) = self.stack.pop() else {
            anyhow::bail!("`{member}` closes `{observed}` without an open element");
        };
        ensure!(
            open == observed,
            "`{member}` closes `{observed}` while `{open}` is open"
        );
        if self.stack.is_empty() {
            self.root_closed = true;
        }
        Ok(())
    }

    fn finish(self, member: &str) -> Result<State::Output> {
        ensure!(
            self.root_closed && self.stack.is_empty(),
            "`{member}` does not contain one balanced complete document"
        );
        let Some(root) = self.root else {
            anyhow::bail!("`{member}` has no document root");
        };
        self.state.finish(root)
    }
}

struct ExactlyOne<Value> {
    value: Option<Value>,
}

impl<Value> ExactlyOne<Value> {
    const fn new() -> Self {
        Self { value: None }
    }

    fn push(&mut self, value: Value, error: &str) -> Result<()> {
        ensure!(self.value.is_none(), error.to_owned());
        self.value = Some(value);
        Ok(())
    }

    fn finish(self, error: &str) -> Result<Value> {
        self.value.ok_or_else(|| anyhow::anyhow!(error.to_owned()))
    }
}

struct ExactlyTwo<Value> {
    first: Option<Value>,
    second: Option<Value>,
}

impl<Value> ExactlyTwo<Value> {
    const fn new() -> Self {
        Self {
            first: None,
            second: None,
        }
    }

    fn push(&mut self, value: Value, error: &str) -> Result<()> {
        if self.first.is_none() {
            self.first = Some(value);
        } else if self.second.is_none() {
            self.second = Some(value);
        } else {
            anyhow::bail!(error.to_owned());
        }
        Ok(())
    }

    fn finish(self, error: &str) -> Result<(Value, Value)> {
        match (self.first, self.second) {
            (Some(first), Some(second)) => Ok((first, second)),
            _ => anyhow::bail!(error.to_owned()),
        }
    }
}

struct ContainerDocumentState {
    representations: ExactlyTwo<Attributes>,
}

impl ContainerDocumentState {
    const fn new() -> Self {
        Self {
            representations: ExactlyTwo::new(),
        }
    }
}

impl DocumentTopologyState for ContainerDocumentState {
    type Output = ContainerTopology;

    const ROOT: &'static str = "Content";

    fn consume(&mut self, element: TopologyElement, child: ChildElement<'_, '_>) -> Result<()> {
        ensure!(
            matches!(element, TopologyElement::ModelRepresentation)
                && child.parents == ["Content"]
                && child.is_empty,
            "`{}` {} must be an empty direct Content child",
            child.member,
            element.name()
        );
        self.representations.push(
            child.attributes()?,
            "`__content.xml` must contain exactly two direct ModelRepresentation children",
        )
    }

    fn finish(self, root: Attributes) -> Result<Self::Output> {
        let (algorithm_representation, production_representation) = self.representations.finish(
            "`__content.xml` must contain exactly two direct ModelRepresentation children",
        )?;
        Ok(ContainerTopology {
            root,
            algorithm_representation,
            production_representation,
        })
    }
}

struct AlgorithmManifestDocumentState {
    files_section_seen: bool,
    source_file: ExactlyOne<Attributes>,
}

impl AlgorithmManifestDocumentState {
    const fn new() -> Self {
        Self {
            files_section_seen: false,
            source_file: ExactlyOne::new(),
        }
    }
}

impl DocumentTopologyState for AlgorithmManifestDocumentState {
    type Output = AlgorithmManifestTopology;

    const ROOT: &'static str = "Manifest";

    fn consume(&mut self, element: TopologyElement, child: ChildElement<'_, '_>) -> Result<()> {
        match element {
            TopologyElement::Files => {
                ensure!(
                    child.parents == ["Manifest"] && !self.files_section_seen,
                    "`{}` Files must be the sole direct Manifest checksum section",
                    child.member
                );
                self.files_section_seen = true;
                Ok(())
            }
            TopologyElement::File => {
                ensure!(
                    child.parents == ["Manifest", "Files"] && child.is_empty,
                    "`{}` File must be an empty direct Manifest/Files child",
                    child.member
                );
                self.source_file.push(
                    child.attributes()?,
                    "`AlgorithmCode/manifest.xml` must contain exactly one direct Files/File child",
                )
            }
            _ => anyhow::bail!(
                "`{}` {} is not valid Algorithm Manifest checksum topology",
                child.member,
                element.name()
            ),
        }
    }

    fn finish(self, root: Attributes) -> Result<Self::Output> {
        ensure!(
            self.files_section_seen,
            "`AlgorithmCode/manifest.xml` has invalid checksum-web section topology"
        );
        Ok(AlgorithmManifestTopology {
            root,
            source_file: self.source_file.finish(
                "`AlgorithmCode/manifest.xml` must contain exactly one direct Files/File child",
            )?,
        })
    }
}

enum ProductionSectionState {
    ManifestReferences,
    Files,
    Complete,
}

struct ProductionManifestDocumentState {
    next_section: ProductionSectionState,
    algorithm_reference: ExactlyOne<Attributes>,
    files: ExactlyTwo<Attributes>,
}

impl ProductionManifestDocumentState {
    const fn new() -> Self {
        Self {
            next_section: ProductionSectionState::ManifestReferences,
            algorithm_reference: ExactlyOne::new(),
            files: ExactlyTwo::new(),
        }
    }
}

impl DocumentTopologyState for ProductionManifestDocumentState {
    type Output = ProductionManifestTopology;

    const ROOT: &'static str = "Manifest";

    fn consume(&mut self, element: TopologyElement, child: ChildElement<'_, '_>) -> Result<()> {
        match element {
            TopologyElement::ManifestReferences => self.manifest_references(child),
            TopologyElement::ManifestReference => self.manifest_reference(child),
            TopologyElement::Files => self.files(child),
            TopologyElement::File => self.file(child),
            TopologyElement::ModelRepresentation => anyhow::bail!(
                "`{}` ModelRepresentation is not valid Production Manifest checksum topology",
                child.member
            ),
        }
    }

    fn finish(self, root: Attributes) -> Result<Self::Output> {
        ensure!(
            matches!(self.next_section, ProductionSectionState::Complete),
            "`ProductionCode/manifest.xml` has invalid checksum-web section topology"
        );
        let algorithm_reference = self.algorithm_reference.finish(
            "`ProductionCode/manifest.xml` must contain exactly one direct ManifestReferences/ManifestReference child",
        )?;
        let (header_file, source_file) = self.files.finish(
            "`ProductionCode/manifest.xml` must contain exactly two direct Files/File children",
        )?;
        Ok(ProductionManifestTopology {
            root,
            algorithm_reference,
            header_file,
            source_file,
        })
    }
}

impl ProductionManifestDocumentState {
    fn manifest_references(&mut self, child: ChildElement<'_, '_>) -> Result<()> {
        ensure!(
            child.parents == ["Manifest"]
                && matches!(
                    self.next_section,
                    ProductionSectionState::ManifestReferences
                ),
            "`{}` ManifestReferences must be the first direct Production Manifest checksum section",
            child.member
        );
        self.next_section = ProductionSectionState::Files;
        Ok(())
    }

    fn manifest_reference(&mut self, child: ChildElement<'_, '_>) -> Result<()> {
        ensure!(
            child.parents == ["Manifest", "ManifestReferences"] && child.is_empty,
            "`{}` ManifestReference must be an empty direct ManifestReferences child",
            child.member
        );
        self.algorithm_reference.push(
            child.attributes()?,
            "`ProductionCode/manifest.xml` must contain exactly one direct ManifestReferences/ManifestReference child",
        )
    }

    fn files(&mut self, child: ChildElement<'_, '_>) -> Result<()> {
        ensure!(
            child.parents == ["Manifest"]
                && matches!(self.next_section, ProductionSectionState::Files),
            "`{}` Files must follow ManifestReferences exactly once",
            child.member
        );
        self.next_section = ProductionSectionState::Complete;
        Ok(())
    }

    fn file(&mut self, child: ChildElement<'_, '_>) -> Result<()> {
        ensure!(
            child.parents == ["Manifest", "Files"] && child.is_empty,
            "`{}` File must be an empty direct Manifest/Files child",
            child.member
        );
        self.files.push(
            child.attributes()?,
            "`ProductionCode/manifest.xml` must contain exactly two direct Files/File children",
        )
    }
}

impl EfmuXmlTopology {
    pub(super) fn authenticate(
        container: &[u8],
        algorithm_manifest: &[u8],
        production_manifest: &[u8],
    ) -> Result<Self> {
        Ok(Self {
            container: parse_document(container, "__content.xml", ContainerDocumentState::new())?,
            algorithm: parse_document(
                algorithm_manifest,
                "AlgorithmCode/manifest.xml",
                AlgorithmManifestDocumentState::new(),
            )?,
            production: parse_document(
                production_manifest,
                "ProductionCode/manifest.xml",
                ProductionManifestDocumentState::new(),
            )?,
        })
    }
}

fn parse_document<State>(bytes: &[u8], member: &str, state: State) -> Result<State::Output>
where
    State: DocumentTopologyState,
{
    let mut reader = Reader::from_reader(bytes);
    reader.config_mut().trim_text(false);
    let mut parser = DocumentParser::new(state);
    loop {
        match reader
            .read_event()
            .with_context(|| format!("parse exact XML topology in `{member}`"))?
        {
            Event::Decl(_) => parser.declaration(member)?,
            Event::Start(tag) => parser.start(&tag, reader.decoder(), false, member)?,
            Event::Empty(tag) => parser.start(&tag, reader.decoder(), true, member)?,
            Event::End(tag) => parser.end(tag.name().as_ref(), member)?,
            Event::Text(text) if parser.stack.is_empty() => {
                ensure!(
                    text.iter().all(u8::is_ascii_whitespace),
                    "`{member}` has text outside its document root"
                );
            }
            Event::CData(_) if parser.stack.is_empty() => {
                anyhow::bail!("`{member}` has CDATA outside its document root")
            }
            Event::DocType(_) => {
                anyhow::bail!("DOCTYPE is forbidden in eFMI XML member `{member}`")
            }
            Event::PI(_) | Event::Comment(_) => {
                anyhow::bail!(
                    "`{member}` contains a noncanonical processing instruction or comment"
                )
            }
            Event::Eof => break,
            _ => {}
        }
    }
    parser.finish(member)
}

fn attributes(tag: &BytesStart<'_>, decoder: Decoder, member: &str) -> Result<Attributes> {
    let mut attributes = BTreeMap::new();
    for attribute in tag.attributes() {
        let attribute = attribute.with_context(|| format!("parse XML attributes in `{member}`"))?;
        let name = name(attribute.key.as_ref(), member)?;
        let value = attribute
            .decode_and_unescape_value(decoder)
            .with_context(|| format!("decode `{name}` attribute in `{member}`"))?
            .into_owned();
        ensure!(
            attributes.insert(name.clone(), value).is_none(),
            "duplicate `{name}` attribute in `{member}`"
        );
    }
    Ok(attributes)
}

fn name(bytes: &[u8], member: &str) -> Result<String> {
    std::str::from_utf8(bytes)
        .with_context(|| format!("non-UTF-8 XML name in `{member}`"))
        .map(str::to_owned)
}

#[cfg(test)]
mod tests {
    use super::*;

    const CONTAINER: &[u8] = br#"<Content id="root"><ModelRepresentation id="ac"/><ModelRepresentation id="pc"/></Content>"#;
    const ALGORITHM: &[u8] =
        br#"<Manifest id="ac"><Files><File name="model.alg"/></Files></Manifest>"#;
    const PRODUCTION: &[u8] = br#"<Manifest id="pc"><ManifestReferences><ManifestReference manifestRefId="ac"/></ManifestReferences><Files><File name="production.h"/><File name="production.c"/></Files></Manifest>"#;

    struct RejectionCase {
        container: &'static [u8],
        algorithm: &'static [u8],
        production: &'static [u8],
        expected: &'static str,
    }

    fn rejection(container: &[u8], algorithm: &[u8], production: &[u8]) -> String {
        match EfmuXmlTopology::authenticate(container, algorithm, production) {
            Ok(_) => panic!("malformed checksum topology must reject"),
            Err(error) => format!("{error:#}"),
        }
    }

    #[test]
    fn exact_document_topologies_are_accepted() {
        let topology = EfmuXmlTopology::authenticate(CONTAINER, ALGORITHM, PRODUCTION)
            .expect("exact checksum topology");
        assert_eq!(
            topology.container.root.get("id").map(String::as_str),
            Some("root")
        );
        assert_eq!(
            topology
                .algorithm
                .source_file
                .get("name")
                .map(String::as_str),
            Some("model.alg")
        );
        assert_eq!(
            topology
                .production
                .source_file
                .get("name")
                .map(String::as_str),
            Some("production.c")
        );
    }

    #[test]
    fn file_requires_the_exact_files_parent() {
        for algorithm in [
            br#"<Manifest><File/><Files><File/></Files></Manifest>"#.as_slice(),
            br#"<Manifest><Detached><File/></Detached><Files><File/></Files></Manifest>"#
                .as_slice(),
        ] {
            let error = rejection(CONTAINER, algorithm, PRODUCTION);
            assert!(
                error.contains("File must be an empty direct Manifest/Files child"),
                "unexpected File rejection: {error}"
            );
        }
        let production = br#"<Manifest><ManifestReferences><ManifestReference/></ManifestReferences><File/><Files><File/><File/></Files></Manifest>"#;
        let error = rejection(CONTAINER, ALGORITHM, production);
        assert!(
            error.contains("File must be an empty direct Manifest/Files child"),
            "unexpected Production File rejection: {error}"
        );
    }

    #[test]
    fn manifest_reference_requires_the_exact_manifest_references_parent() {
        for production in [
            br#"<Manifest><ManifestReference/><ManifestReferences><ManifestReference/></ManifestReferences><Files><File/><File/></Files></Manifest>"#.as_slice(),
            br#"<Manifest><ManifestReferences><Detached><ManifestReference/></Detached><ManifestReference/></ManifestReferences><Files><File/><File/></Files></Manifest>"#.as_slice(),
        ] {
            let error = rejection(CONTAINER, ALGORITHM, production);
            assert!(
                error.contains(
                    "ManifestReference must be an empty direct ManifestReferences child"
                ),
                "unexpected ManifestReference rejection: {error}"
            );
        }
    }

    #[test]
    fn model_representation_cannot_be_nested() {
        let nested = br#"<Content><Wrapper><ModelRepresentation/></Wrapper><ModelRepresentation/><ModelRepresentation/></Content>"#;
        let error = rejection(nested, ALGORITHM, PRODUCTION);
        assert!(
            error.contains("ModelRepresentation must be an empty direct Content child"),
            "unexpected nested representation rejection: {error}"
        );
    }

    #[test]
    fn production_sections_have_one_exact_order() {
        let reordered = br#"<Manifest><Files><File/><File/></Files><ManifestReferences><ManifestReference/></ManifestReferences></Manifest>"#;
        let error = rejection(CONTAINER, ALGORITHM, reordered);
        assert!(
            error.contains("Files must follow ManifestReferences exactly once"),
            "unexpected section-order rejection: {error}"
        );
    }

    #[test]
    fn checksum_target_cardinalities_are_exact() {
        let cases = [
            RejectionCase {
                container: br#"<Content><ModelRepresentation/><ModelRepresentation/><ModelRepresentation/></Content>"#,
                algorithm: ALGORITHM,
                production: PRODUCTION,
                expected: "exactly two direct ModelRepresentation children",
            },
            RejectionCase {
                container: CONTAINER,
                algorithm: br#"<Manifest><Files><File/><File/></Files></Manifest>"#,
                production: PRODUCTION,
                expected: "exactly one direct Files/File child",
            },
            RejectionCase {
                container: CONTAINER,
                algorithm: ALGORITHM,
                production: br#"<Manifest><ManifestReferences><ManifestReference/><ManifestReference/></ManifestReferences><Files><File/><File/></Files></Manifest>"#,
                expected: "exactly one direct ManifestReferences/ManifestReference child",
            },
            RejectionCase {
                container: CONTAINER,
                algorithm: ALGORITHM,
                production: br#"<Manifest><ManifestReferences><ManifestReference/></ManifestReferences><Files><File/><File/><File/></Files></Manifest>"#,
                expected: "exactly two direct Files/File children",
            },
        ];
        for case in cases {
            let error = rejection(case.container, case.algorithm, case.production);
            assert!(
                error.contains(case.expected),
                "unexpected cardinality rejection: {error}"
            );
        }
    }

    #[test]
    fn a_second_or_trailing_root_is_rejected() {
        for container in [
            br#"<Content><ModelRepresentation/><ModelRepresentation/></Content><Content><ModelRepresentation/><ModelRepresentation/></Content>"#.as_slice(),
            br#"<Content><ModelRepresentation/><ModelRepresentation/></Content><Trailing/>"#
                .as_slice(),
        ] {
            let error = rejection(container, ALGORITHM, PRODUCTION);
            assert!(
                error.contains("second document root or content after its root"),
                "unexpected trailing-root rejection: {error}"
            );
        }
    }
}
