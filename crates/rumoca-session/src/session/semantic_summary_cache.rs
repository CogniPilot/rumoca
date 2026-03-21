use super::declaration_index::PersistedDeclarationIndex;
use super::file_summary::{FileSummary, PersistedFileSummary};
use super::namespace_completion::NamespaceCompletionCache;
use super::package_def_map::{PackageDefMap, PersistedPackageDefMap};
use super::{DeclarationIndex, FileId, Fingerprint, SourceRootResolvedAggregate, file_summary};
use anyhow::{Context, Result};
use bincode::Options;
use indexmap::IndexMap;
use rumoca_ir_ast as ast;
use serde::{Deserialize, Serialize};
use std::fs;
use std::fs::File;
use std::io::{BufReader, BufWriter, Read, Write};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::path::{Path, PathBuf};

use crate::source_root_cache::source_root_cache_compiler_version;

const SEMANTIC_SUMMARY_CACHE_SCHEMA_VERSION: u32 = 8;
const SEMANTIC_SUMMARY_CACHE_DIR: &str = "semantic-summaries";
const SEMANTIC_SUMMARY_CACHE_MAGIC: &[u8] = b"rumoca-semantic-summary-v1";
const MAX_SEMANTIC_SUMMARY_CACHE_BYTES: u64 = 512 * 1024 * 1024;

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CachedSourceRootFileFingerprint {
    uri: String,
    summary_fingerprint: Fingerprint,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct SourceRootSemanticCacheArtifacts {
    package_def_map: bool,
    file_summaries: bool,
    declaration_indices: bool,
    resolved_aggregate: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct SourceRootSemanticCacheManifest {
    schema_version: u32,
    compiler_version: String,
    source_root_id: String,
    cache_key: String,
    source_root_path: String,
    source_root_fingerprint: Fingerprint,
    class_graph_fingerprint: Fingerprint,
    resolved_aggregate_fingerprint: Option<Fingerprint>,
    file_fingerprints: Vec<CachedSourceRootFileFingerprint>,
    artifacts: SourceRootSemanticCacheArtifacts,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CachedSourceRootSemanticFile {
    uri: String,
    summary_fingerprint: Fingerprint,
    file_summary: PersistedFileSummary,
    declaration_index: PersistedDeclarationIndex,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CachedSourceRootSemanticSummaryPayload {
    manifest: SourceRootSemanticCacheManifest,
    package_def_map: PersistedPackageDefMap,
    files: Vec<CachedSourceRootSemanticFile>,
    resolved_aggregate: Option<SourceRootResolvedAggregate>,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct SourceRootSemanticSummary {
    declarations_by_uri: IndexMap<String, PersistedDeclarationIndex>,
    file_summaries_by_uri: IndexMap<String, PersistedFileSummary>,
    summary_fingerprints_by_uri: IndexMap<String, Fingerprint>,
    package_def_map: PersistedPackageDefMap,
    source_root_fingerprint: Fingerprint,
    class_graph_fingerprint: Fingerprint,
    resolved_aggregate: Option<SourceRootResolvedAggregate>,
}

impl SourceRootSemanticSummary {
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
        let class_graph_fingerprint = class_graph_fingerprint_for_summaries(&file_summaries);
        let summary_fingerprints_by_uri = docs
            .iter()
            .map(|(uri, definition)| (uri.clone(), file_summary::summary_fingerprint(definition)))
            .collect();
        Self {
            declarations_by_uri,
            file_summaries_by_uri,
            package_def_map: PersistedPackageDefMap::from_file_summaries(&file_summaries),
            source_root_fingerprint: source_root_fingerprint(&summary_fingerprints_by_uri),
            class_graph_fingerprint,
            summary_fingerprints_by_uri,
            resolved_aggregate: None,
        }
    }

    pub(crate) fn with_resolved_aggregate(
        mut self,
        resolved_aggregate: Option<SourceRootResolvedAggregate>,
    ) -> Self {
        self.resolved_aggregate = resolved_aggregate;
        self
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
        let mut package_def_map = PackageDefMap::default();
        for (uri, persisted) in &self.file_summaries_by_uri {
            let Some(file_id) = file_id_for_uri(uri) else {
                continue;
            };
            let summary = FileSummary::from_persisted(file_id, persisted);
            package_def_map.extend_from_summary(&summary);
        }
        if package_def_map.class_entries().next().is_some() {
            package_def_map
        } else {
            PackageDefMap::from_persisted(&self.package_def_map, file_id_for_uri)
        }
    }

    pub(crate) fn resolved_aggregate(&self) -> Option<&SourceRootResolvedAggregate> {
        self.resolved_aggregate.as_ref()
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

pub(crate) fn source_root_semantic_cache_key(
    source_root_id: &str,
    docs: &[(String, ast::StoredDefinition)],
) -> String {
    let summary_fingerprints_by_uri = docs
        .iter()
        .map(|(uri, definition)| (uri.clone(), file_summary::summary_fingerprint(definition)))
        .collect::<IndexMap<_, _>>();
    let mut hasher = blake3::Hasher::new();
    hasher.update(b"rumoca-source-root-semantic-summary-v1");
    hasher.update(source_root_id.as_bytes());
    hasher.update(&source_root_fingerprint(&summary_fingerprints_by_uri));
    hasher.finalize().to_hex().to_string()
}

pub(crate) fn read_source_root_semantic_summary(
    cache_dir: Option<&Path>,
    source_root_id: &str,
    cache_key: &str,
    docs: &[(String, ast::StoredDefinition)],
) -> Option<SourceRootSemanticSummary> {
    let cache_dir = cache_dir?;
    let cache_file = cache_file_path(cache_dir, cache_key);
    let file = File::open(cache_file).ok()?;
    let payload = read_summary_payload(file).ok()?;
    summary_from_payload(payload, source_root_id, cache_key, docs)
}

fn read_summary_payload(file: File) -> Result<CachedSourceRootSemanticSummaryPayload> {
    let mut reader = BufReader::new(file);
    let mut magic = [0u8; SEMANTIC_SUMMARY_CACHE_MAGIC.len()];
    reader
        .read_exact(&mut magic)
        .context("semantic summary cache header read failed")?;
    if magic != *SEMANTIC_SUMMARY_CACHE_MAGIC {
        anyhow::bail!("semantic summary cache header mismatch");
    }
    catch_unwind(AssertUnwindSafe(|| {
        semantic_summary_bincode_options()
            .deserialize_from::<_, CachedSourceRootSemanticSummaryPayload>(&mut reader)
    }))
    .map_err(|_| anyhow::anyhow!("semantic summary cache deserialize panicked"))?
    .context("semantic summary cache deserialize failed")
}

fn semantic_summary_bincode_options() -> impl Options {
    bincode::DefaultOptions::new()
        .with_fixint_encoding()
        .allow_trailing_bytes()
        .with_limit(MAX_SEMANTIC_SUMMARY_CACHE_BYTES)
}

pub(crate) fn write_source_root_semantic_summary(
    cache_dir: Option<&Path>,
    source_root_id: &str,
    source_root_path: &Path,
    cache_key: &str,
    summary: &SourceRootSemanticSummary,
) -> bool {
    let Some(cache_dir) = cache_dir else {
        return false;
    };
    if fs::create_dir_all(cache_dir).is_err() {
        return false;
    }

    let cache_file = cache_file_path(cache_dir, cache_key);
    write_summary_cache(
        &cache_file,
        source_root_id,
        source_root_path,
        cache_key,
        summary,
    )
    .is_ok()
}

fn cache_file_path(cache_dir: &Path, cache_key: &str) -> PathBuf {
    cache_dir.join(format!("{cache_key}.bin"))
}

fn summary_from_payload(
    payload: CachedSourceRootSemanticSummaryPayload,
    source_root_id: &str,
    cache_key: &str,
    docs: &[(String, ast::StoredDefinition)],
) -> Option<SourceRootSemanticSummary> {
    if payload.manifest.schema_version != SEMANTIC_SUMMARY_CACHE_SCHEMA_VERSION {
        return None;
    }
    if payload.manifest.compiler_version != source_root_cache_compiler_version() {
        return None;
    }
    if payload.manifest.source_root_id != source_root_id {
        return None;
    }
    if payload.manifest.cache_key != cache_key {
        return None;
    }
    if !payload.manifest.artifacts.package_def_map
        || !payload.manifest.artifacts.file_summaries
        || !payload.manifest.artifacts.declaration_indices
    {
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
    let class_graph_fingerprint =
        class_graph_fingerprint_for_persisted_summaries(&file_summaries_by_uri);
    let summary = SourceRootSemanticSummary {
        declarations_by_uri,
        file_summaries_by_uri,
        summary_fingerprints_by_uri,
        package_def_map: payload.package_def_map,
        source_root_fingerprint: payload.manifest.source_root_fingerprint,
        class_graph_fingerprint,
        resolved_aggregate: payload.resolved_aggregate,
    };
    let manifest_fingerprints: IndexMap<_, _> = payload
        .manifest
        .file_fingerprints
        .into_iter()
        .map(|file| (file.uri, file.summary_fingerprint))
        .collect();
    if manifest_fingerprints != summary.summary_fingerprints_by_uri {
        return None;
    }
    if payload.manifest.class_graph_fingerprint != summary.class_graph_fingerprint {
        return None;
    }
    if payload.manifest.resolved_aggregate_fingerprint
        != summary
            .resolved_aggregate
            .as_ref()
            .map(resolved_aggregate_fingerprint)
    {
        return None;
    }
    summary.matches_documents(docs).then_some(summary)
}

fn write_summary_cache(
    path: &Path,
    source_root_id: &str,
    source_root_path: &Path,
    cache_key: &str,
    summary: &SourceRootSemanticSummary,
) -> Result<()> {
    write_summary_cache_with_manifest_fingerprint(
        path,
        source_root_id,
        source_root_path,
        cache_key,
        summary,
        summary.class_graph_fingerprint,
    )
}

fn write_summary_cache_with_manifest_fingerprint(
    path: &Path,
    source_root_id: &str,
    source_root_path: &Path,
    cache_key: &str,
    summary: &SourceRootSemanticSummary,
    class_graph_fingerprint: Fingerprint,
) -> Result<()> {
    let file_fingerprints = summary
        .summary_fingerprints_by_uri
        .iter()
        .map(
            |(uri, summary_fingerprint)| CachedSourceRootFileFingerprint {
                uri: uri.clone(),
                summary_fingerprint: *summary_fingerprint,
            },
        )
        .collect();
    let payload = CachedSourceRootSemanticSummaryPayload {
        manifest: SourceRootSemanticCacheManifest {
            schema_version: SEMANTIC_SUMMARY_CACHE_SCHEMA_VERSION,
            compiler_version: source_root_cache_compiler_version(),
            source_root_id: source_root_id.to_string(),
            cache_key: cache_key.to_string(),
            source_root_path: source_root_path.to_string_lossy().to_string(),
            source_root_fingerprint: summary.source_root_fingerprint,
            class_graph_fingerprint,
            resolved_aggregate_fingerprint: summary
                .resolved_aggregate
                .as_ref()
                .map(resolved_aggregate_fingerprint),
            file_fingerprints,
            artifacts: SourceRootSemanticCacheArtifacts {
                package_def_map: true,
                file_summaries: true,
                declaration_indices: true,
                resolved_aggregate: summary.resolved_aggregate.is_some(),
            },
        },
        package_def_map: summary.package_def_map.clone(),
        files: summary
            .declarations_by_uri
            .iter()
            .map(|(uri, declaration_index)| CachedSourceRootSemanticFile {
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
        resolved_aggregate: summary.resolved_aggregate.clone(),
    };
    let tmp_path = path.with_extension(format!("{}.tmp", std::process::id()));
    let file = File::create(&tmp_path).with_context(|| format!("create {}", tmp_path.display()))?;
    let mut writer = BufWriter::new(file);
    writer
        .write_all(SEMANTIC_SUMMARY_CACHE_MAGIC)
        .context("write semantic summary cache header")?;
    semantic_summary_bincode_options()
        .serialize_into(&mut writer, &payload)
        .context("serialize semantic summary payload")?;
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

fn class_graph_fingerprint_for_summaries(
    file_summaries: &IndexMap<String, FileSummary>,
) -> Fingerprint {
    let mut package_def_map = PackageDefMap::default();
    for summary in file_summaries.values() {
        package_def_map.extend_from_summary(summary);
    }
    class_graph_fingerprint_for_package_def_map(&package_def_map)
}

fn class_graph_fingerprint_for_persisted_summaries(
    file_summaries_by_uri: &IndexMap<String, PersistedFileSummary>,
) -> Fingerprint {
    let mut package_def_map = PackageDefMap::default();
    for (index, persisted) in file_summaries_by_uri.values().enumerate() {
        let Some(file_id) = u32::try_from(index).ok().map(FileId::new) else {
            return [0; 32];
        };
        package_def_map.extend_from_summary(&FileSummary::from_persisted(file_id, persisted));
    }
    class_graph_fingerprint_for_package_def_map(&package_def_map)
}

fn class_graph_fingerprint_for_package_def_map(package_def_map: &PackageDefMap) -> Fingerprint {
    let mut namespace_cache = NamespaceCompletionCache::default();
    namespace_cache.extend_from_package_def_map(package_def_map);
    namespace_cache.finalize().aggregate_fingerprint()
}

fn resolved_aggregate_fingerprint(aggregate: &SourceRootResolvedAggregate) -> Fingerprint {
    let mut hasher = blake3::Hasher::new();
    hasher.update(b"rumoca-source-root-resolved-aggregate-v1");
    for model_name in &aggregate.model_names {
        hasher.update(model_name.as_bytes());
    }
    hasher.update(&aggregate.dependency_fingerprints.aggregate_fingerprint());
    *hasher.finalize().as_bytes()
}

#[cfg(test)]
pub(crate) fn write_source_root_semantic_summary_with_class_graph_fingerprint(
    cache_dir: Option<&Path>,
    source_root_id: &str,
    source_root_path: &Path,
    cache_key: &str,
    summary: &SourceRootSemanticSummary,
    class_graph_fingerprint: Fingerprint,
) -> bool {
    let Some(cache_dir) = cache_dir else {
        return false;
    };
    if fs::create_dir_all(cache_dir).is_err() {
        return false;
    }

    let cache_file = cache_file_path(cache_dir, cache_key);
    write_summary_cache_with_manifest_fingerprint(
        &cache_file,
        source_root_id,
        source_root_path,
        cache_key,
        summary,
        class_graph_fingerprint,
    )
    .is_ok()
}
