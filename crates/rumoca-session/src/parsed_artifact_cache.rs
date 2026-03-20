use anyhow::{Context, Result};
use indexmap::IndexMap;
use rumoca_ir_ast::StoredDefinition;
use std::fs;
use std::fs::File;
use std::io::{BufReader, BufWriter, Write};
use std::path::{Path, PathBuf};
use std::sync::{Mutex, OnceLock};

use crate::instrumentation::{
    record_parsed_file_artifact_cache_hit, record_parsed_file_artifact_cache_miss,
    record_parsed_file_parse, record_parsed_file_parse_duration,
};
use crate::library_cache::{cache_compiler_version, resolve_library_cache_dir};

const PARSED_ARTIFACT_CACHE_SCHEMA_VERSION: u32 = 2;
const PARSED_ARTIFACT_CACHE_DIR: &str = "parsed-files";
const MAX_IN_MEMORY_PARSED_ARTIFACTS: usize = 256;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ParsedArtifactCacheStatus {
    Hit,
    Miss,
}

static IN_MEMORY_PARSED_ARTIFACTS: OnceLock<Mutex<IndexMap<String, StoredDefinition>>> =
    OnceLock::new();

fn in_memory_parsed_artifacts() -> &'static Mutex<IndexMap<String, StoredDefinition>> {
    IN_MEMORY_PARSED_ARTIFACTS.get_or_init(|| Mutex::new(IndexMap::new()))
}

fn content_hash(source: &str) -> String {
    blake3::hash(source.as_bytes()).to_hex().to_string()
}

fn artifact_cache_key(file_name: &str, source_hash: &str) -> String {
    let mut hasher = blake3::Hasher::new();
    hasher.update(format!("schema={PARSED_ARTIFACT_CACHE_SCHEMA_VERSION}\n").as_bytes());
    hasher.update(format!("compiler={}\n", cache_compiler_version()).as_bytes());
    hasher.update(file_name.as_bytes());
    hasher.update(b"\n");
    hasher.update(source_hash.as_bytes());
    hasher.finalize().to_hex().to_string()
}

fn cache_file_path(cache_dir: &Path, cache_key: &str) -> PathBuf {
    cache_dir.join(format!("{cache_key}.bin"))
}

fn try_read_cache(path: &Path) -> Option<StoredDefinition> {
    let file = File::open(path).ok()?;
    bincode::deserialize_from::<_, StoredDefinition>(BufReader::new(file)).ok()
}

fn write_cache(path: &Path, definition: &StoredDefinition) -> Result<()> {
    let tmp_path = path.with_extension(format!("{}.tmp", std::process::id()));
    let file = File::create(&tmp_path).with_context(|| format!("create {}", tmp_path.display()))?;
    let mut writer = BufWriter::new(file);
    bincode::serialize_into(&mut writer, definition)
        .context("serialize parsed artifact payload")?;
    writer
        .flush()
        .with_context(|| format!("flush {}", tmp_path.display()))?;
    if let Err(rename_err) = fs::rename(&tmp_path, path) {
        fs::copy(&tmp_path, path)
            .with_context(|| format!("copy {} -> {}", tmp_path.display(), path.display()))?;
        let _ = fs::remove_file(&tmp_path);
        if !path.is_file() {
            return Err(rename_err).context("finalize parsed artifact cache file");
        }
    }
    Ok(())
}

fn get_in_memory(cache_key: &str) -> Option<StoredDefinition> {
    let mut cache = in_memory_parsed_artifacts()
        .lock()
        .expect("parsed artifact cache poisoned");
    let definition = cache.shift_remove(cache_key)?;
    cache.insert(cache_key.to_string(), definition.clone());
    Some(definition)
}

fn insert_in_memory(cache_key: String, definition: StoredDefinition) {
    let mut cache = in_memory_parsed_artifacts()
        .lock()
        .expect("parsed artifact cache poisoned");
    cache.shift_remove(&cache_key);
    cache.insert(cache_key, definition);
    while cache.len() > MAX_IN_MEMORY_PARSED_ARTIFACTS {
        let oldest = cache.keys().next().cloned();
        if let Some(oldest) = oldest {
            cache.shift_remove(&oldest);
        } else {
            break;
        }
    }
}

pub(crate) fn resolve_parsed_artifact_cache_dir_from_root(root: Option<&Path>) -> Option<PathBuf> {
    root.map(|root| root.join(PARSED_ARTIFACT_CACHE_DIR))
}

pub(crate) fn resolve_parsed_artifact_cache_dir() -> Option<PathBuf> {
    let root = resolve_library_cache_dir();
    resolve_parsed_artifact_cache_dir_from_root(root.as_deref())
}

pub(crate) fn parse_file_with_artifact_cache(
    path: &Path,
    cache_dir: Option<&Path>,
) -> Result<(String, StoredDefinition)> {
    let (file_name, definition, _) = parse_file_with_artifact_cache_status(path, cache_dir)?;
    Ok((file_name, definition))
}

pub(crate) fn parse_file_with_artifact_cache_status(
    path: &Path,
    cache_dir: Option<&Path>,
) -> Result<(String, StoredDefinition, ParsedArtifactCacheStatus)> {
    let source = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read file: {}", path.display()))?;
    parse_file_with_preloaded_source_status(path, source, cache_dir)
}

pub(crate) fn parse_file_with_precomputed_hash_status(
    path: &Path,
    source_hash: &str,
    cache_dir: Option<&Path>,
) -> Result<(String, StoredDefinition, ParsedArtifactCacheStatus)> {
    let file_name = path.to_string_lossy().to_string();
    let cache_key = artifact_cache_key(&file_name, source_hash);

    if let Some(definition) = get_in_memory(&cache_key) {
        record_parsed_file_artifact_cache_hit();
        return Ok((file_name, definition, ParsedArtifactCacheStatus::Hit));
    }

    if let Some(cache_dir) = cache_dir
        && fs::create_dir_all(cache_dir).is_ok()
    {
        let cache_file = cache_file_path(cache_dir, &cache_key);
        if let Some(definition) = try_read_cache(&cache_file) {
            insert_in_memory(cache_key, definition.clone());
            record_parsed_file_artifact_cache_hit();
            return Ok((file_name, definition, ParsedArtifactCacheStatus::Hit));
        }
    }

    let source = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read file: {}", path.display()))?;
    parse_file_with_preloaded_source_status(path, source, cache_dir)
}

fn parse_file_with_preloaded_source_status(
    path: &Path,
    source: String,
    cache_dir: Option<&Path>,
) -> Result<(String, StoredDefinition, ParsedArtifactCacheStatus)> {
    let file_name = path.to_string_lossy().to_string();
    let cache_key = artifact_cache_key(&file_name, &content_hash(&source));

    if let Some(definition) = get_in_memory(&cache_key) {
        record_parsed_file_artifact_cache_hit();
        return Ok((file_name, definition, ParsedArtifactCacheStatus::Hit));
    }

    if let Some(cache_dir) = cache_dir
        && fs::create_dir_all(cache_dir).is_ok()
    {
        let cache_file = cache_file_path(cache_dir, &cache_key);
        if let Some(definition) = try_read_cache(&cache_file) {
            insert_in_memory(cache_key, definition.clone());
            record_parsed_file_artifact_cache_hit();
            return Ok((file_name, definition, ParsedArtifactCacheStatus::Hit));
        }
    }

    let parse_started = rumoca_core::maybe_start_timer();
    let definition = rumoca_phase_parse::parse_to_ast(&source, &file_name)
        .with_context(|| format!("Failed to parse file: {}", path.display()))?;
    record_parsed_file_artifact_cache_miss();
    record_parsed_file_parse();
    if let Some(elapsed) = rumoca_core::maybe_elapsed_duration(parse_started) {
        record_parsed_file_parse_duration(elapsed);
    }
    insert_in_memory(cache_key.clone(), definition.clone());

    if let Some(cache_dir) = cache_dir
        && fs::create_dir_all(cache_dir).is_ok()
    {
        let cache_file = cache_file_path(cache_dir, &cache_key);
        let _ = write_cache(&cache_file, &definition);
    }

    Ok((file_name, definition, ParsedArtifactCacheStatus::Miss))
}
