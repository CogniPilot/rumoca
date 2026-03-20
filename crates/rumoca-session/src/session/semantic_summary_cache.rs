use super::declaration_index::PersistedDeclarationIndex;
use super::file_summary::{FileSummary, PersistedFileSummary};
use super::package_def_map::{PackageDefMap, PersistedPackageDefMap};
use super::{DeclarationIndex, FileId, Fingerprint, file_summary};
use anyhow::{Context, Result};
use indexmap::IndexMap;
use rumoca_ir_ast as ast;
use serde::{Deserialize, Serialize};
use std::fs;
use std::fs::File;
use std::io::{BufReader, BufWriter, Write};
use std::path::{Path, PathBuf};

use crate::library_cache::cache_compiler_version;

const SEMANTIC_SUMMARY_CACHE_SCHEMA_VERSION: u32 = 3;
const SEMANTIC_SUMMARY_CACHE_DIR: &str = "semantic-summaries";

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CachedFileSemanticSummary {
    uri: String,
    summary_fingerprint: Fingerprint,
    file_summary: PersistedFileSummary,
    declaration_index: PersistedDeclarationIndex,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CachedLibrarySemanticSummaryPayload {
    schema_version: u32,
    compiler_version: String,
    cache_key: String,
    source_path: String,
    source_root_fingerprint: Fingerprint,
    package_def_map: PersistedPackageDefMap,
    files: Vec<CachedFileSemanticSummary>,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct LibrarySemanticSummary {
    declarations_by_uri: IndexMap<String, PersistedDeclarationIndex>,
    file_summaries_by_uri: IndexMap<String, PersistedFileSummary>,
    summary_fingerprints_by_uri: IndexMap<String, Fingerprint>,
    package_def_map: PersistedPackageDefMap,
    source_root_fingerprint: Fingerprint,
}

impl LibrarySemanticSummary {
    pub(crate) fn from_documents(docs: &[(String, ast::StoredDefinition)]) -> Self {
        let declarations_by_uri = docs
            .iter()
            .map(|(uri, definition)| {
                let index = DeclarationIndex::from_definition(FileId::default(), definition);
                (uri.clone(), index.to_persisted())
            })
            .collect();
        let file_summaries: IndexMap<_, _> = docs
            .iter()
            .map(|(uri, definition)| {
                let summary = FileSummary::from_definition(FileId::default(), definition);
                (uri.clone(), summary)
            })
            .collect();
        let file_summaries_by_uri = file_summaries
            .iter()
            .map(|(uri, summary)| (uri.clone(), summary.to_persisted()))
            .collect();
        let summary_fingerprints_by_uri = docs
            .iter()
            .map(|(uri, definition)| (uri.clone(), file_summary::summary_fingerprint(definition)))
            .collect();
        Self {
            declarations_by_uri,
            file_summaries_by_uri,
            package_def_map: PersistedPackageDefMap::from_file_summaries(&file_summaries),
            source_root_fingerprint: source_root_fingerprint(&summary_fingerprints_by_uri),
            summary_fingerprints_by_uri,
        }
    }

    pub(crate) fn declaration_index_for_uri(
        &self,
        uri: &str,
        file_id: FileId,
    ) -> Option<DeclarationIndex> {
        self.declarations_by_uri
            .get(uri)
            .map(|persisted| DeclarationIndex::from_persisted(file_id, persisted))
    }

    pub(crate) fn summary_fingerprint_for_uri(&self, uri: &str) -> Option<Fingerprint> {
        self.summary_fingerprints_by_uri.get(uri).copied()
    }

    pub(crate) fn file_summary_for_uri(&self, uri: &str, file_id: FileId) -> Option<FileSummary> {
        self.file_summaries_by_uri
            .get(uri)
            .map(|persisted| FileSummary::from_persisted(file_id, persisted))
    }

    pub(crate) fn package_def_map(
        &self,
        file_id_for_uri: impl Fn(&str) -> Option<FileId>,
    ) -> PackageDefMap {
        PackageDefMap::from_persisted(&self.package_def_map, file_id_for_uri)
    }

    fn matches_documents(&self, docs: &[(String, ast::StoredDefinition)]) -> bool {
        if self.summary_fingerprints_by_uri.len() != docs.len() {
            return false;
        }

        let current_fingerprints: IndexMap<_, _> = docs
            .iter()
            .map(|(uri, definition)| (uri.clone(), file_summary::summary_fingerprint(definition)))
            .collect();
        self.source_root_fingerprint == source_root_fingerprint(&current_fingerprints)
            && current_fingerprints.iter().all(|(uri, fingerprint)| {
                self.summary_fingerprints_by_uri.get(uri) == Some(fingerprint)
            })
    }
}

pub(crate) fn resolve_semantic_summary_cache_dir_from_root(root: Option<&Path>) -> Option<PathBuf> {
    root.map(|root| root.join(SEMANTIC_SUMMARY_CACHE_DIR))
}

pub(crate) fn read_library_semantic_summary(
    cache_dir: Option<&Path>,
    cache_key: &str,
    docs: &[(String, ast::StoredDefinition)],
) -> Option<LibrarySemanticSummary> {
    let cache_dir = cache_dir?;
    let cache_file = cache_file_path(cache_dir, cache_key);
    let file = File::open(cache_file).ok()?;
    let payload =
        bincode::deserialize_from::<_, CachedLibrarySemanticSummaryPayload>(BufReader::new(file))
            .ok()?;
    summary_from_payload(payload, cache_key, docs)
}

pub(crate) fn write_library_semantic_summary(
    cache_dir: Option<&Path>,
    source_path: &Path,
    cache_key: &str,
    summary: &LibrarySemanticSummary,
) -> bool {
    let Some(cache_dir) = cache_dir else {
        return false;
    };
    if fs::create_dir_all(cache_dir).is_err() {
        return false;
    }

    let cache_file = cache_file_path(cache_dir, cache_key);
    write_summary_cache(&cache_file, source_path, cache_key, summary).is_ok()
}

fn cache_file_path(cache_dir: &Path, cache_key: &str) -> PathBuf {
    cache_dir.join(format!("{cache_key}.bin"))
}

fn summary_from_payload(
    payload: CachedLibrarySemanticSummaryPayload,
    cache_key: &str,
    docs: &[(String, ast::StoredDefinition)],
) -> Option<LibrarySemanticSummary> {
    if payload.schema_version != SEMANTIC_SUMMARY_CACHE_SCHEMA_VERSION {
        return None;
    }
    if payload.compiler_version != cache_compiler_version() {
        return None;
    }
    if payload.cache_key != cache_key {
        return None;
    }

    let mut declarations_by_uri = IndexMap::new();
    let mut file_summaries_by_uri = IndexMap::new();
    let mut summary_fingerprints_by_uri = IndexMap::new();
    for file in payload.files {
        declarations_by_uri.insert(file.uri.clone(), file.declaration_index);
        file_summaries_by_uri.insert(file.uri.clone(), file.file_summary);
        summary_fingerprints_by_uri.insert(file.uri, file.summary_fingerprint);
    }
    let summary = LibrarySemanticSummary {
        declarations_by_uri,
        file_summaries_by_uri,
        summary_fingerprints_by_uri,
        package_def_map: payload.package_def_map,
        source_root_fingerprint: payload.source_root_fingerprint,
    };
    summary.matches_documents(docs).then_some(summary)
}

fn write_summary_cache(
    path: &Path,
    source_path: &Path,
    cache_key: &str,
    summary: &LibrarySemanticSummary,
) -> Result<()> {
    let payload = CachedLibrarySemanticSummaryPayload {
        schema_version: SEMANTIC_SUMMARY_CACHE_SCHEMA_VERSION,
        compiler_version: cache_compiler_version(),
        cache_key: cache_key.to_string(),
        source_path: source_path.to_string_lossy().to_string(),
        source_root_fingerprint: summary.source_root_fingerprint,
        package_def_map: summary.package_def_map.clone(),
        files: summary
            .declarations_by_uri
            .iter()
            .map(|(uri, declaration_index)| CachedFileSemanticSummary {
                uri: uri.clone(),
                summary_fingerprint: summary
                    .summary_fingerprints_by_uri
                    .get(uri)
                    .copied()
                    .expect("summary fingerprint should exist for persisted declaration"),
                file_summary: summary
                    .file_summaries_by_uri
                    .get(uri)
                    .cloned()
                    .expect("file summary should exist for persisted declaration"),
                declaration_index: declaration_index.clone(),
            })
            .collect(),
    };
    let tmp_path = path.with_extension(format!("{}.tmp", std::process::id()));
    let file = File::create(&tmp_path).with_context(|| format!("create {}", tmp_path.display()))?;
    let mut writer = BufWriter::new(file);
    bincode::serialize_into(&mut writer, &payload).context("serialize semantic summary payload")?;
    writer
        .flush()
        .with_context(|| format!("flush {}", tmp_path.display()))?;
    if let Err(rename_err) = fs::rename(&tmp_path, path) {
        fs::copy(&tmp_path, path)
            .with_context(|| format!("copy {} -> {}", tmp_path.display(), path.display()))?;
        let _ = fs::remove_file(&tmp_path);
        if !path.is_file() {
            return Err(rename_err).context("finalize semantic summary cache file");
        }
    }
    Ok(())
}

fn source_root_fingerprint(summary_fingerprints: &IndexMap<String, Fingerprint>) -> Fingerprint {
    let mut entries = summary_fingerprints
        .iter()
        .map(|(uri, fingerprint)| (uri.as_str(), *fingerprint))
        .collect::<Vec<_>>();
    entries.sort_by(|left, right| left.0.cmp(right.0));

    let mut hasher = blake3::Hasher::new();
    for (uri, fingerprint) in entries {
        hasher.update(uri.as_bytes());
        hasher.update(&fingerprint);
    }
    *hasher.finalize().as_bytes()
}
