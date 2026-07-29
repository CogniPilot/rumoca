use super::*;

/// A parsed in-memory source whose syntax and source text were produced
/// together.
///
/// Keeping the text beside the parsed definition preserves the parser's stable
/// source identity through session assembly and into provenance-bearing IR.
#[derive(Debug, Clone)]
pub struct ParsedSourceDocument {
    uri: String,
    source: Arc<str>,
    definition: ast::StoredDefinition,
}

impl ParsedSourceDocument {
    pub fn parse(uri: impl Into<String>, source: impl Into<String>) -> Result<Self> {
        let uri = uri.into();
        let source = Arc::<str>::from(source.into());
        let definition = rumoca_phase_parse::parse_to_ast(&source, &uri)?;
        Ok(Self {
            uri,
            source,
            definition,
        })
    }

    pub fn uri(&self) -> &str {
        &self.uri
    }

    pub(super) fn from_parsed(
        uri: String,
        source: Arc<str>,
        definition: ast::StoredDefinition,
    ) -> Self {
        Self {
            uri,
            source,
            definition,
        }
    }

    pub(super) fn into_parts(self) -> (String, Arc<str>, ast::StoredDefinition) {
        (self.uri, self.source, self.definition)
    }
}

#[derive(Debug, Clone)]
pub struct ParsedSourceRootLoad<'a> {
    pub source_root_kind: SourceRootKind,
    pub source_root_path: &'a Path,
    pub cache_status: SourceRootCacheStatus,
    pub path_key: &'a str,
    pub current_document_path: Option<&'a str>,
    pub documents: Vec<(String, ast::StoredDefinition)>,
    pub expected_epoch: u64,
}
