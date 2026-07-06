//! `rumoca-docs-cache`: build the docs live-example source-root cache.
//!
//! Parses every `.mo` file under each `--source-root key=dir` into a
//! `StoredDefinition` and writes the combined `gzip(bincode(Vec<(uri, def)>))`
//! blob that the mdBook's browser LSP hydrates. This is the only part of the
//! `docs` workflow that links the compiler, so it lives here and `xtask` shells
//! out to it — keeping `xtask` free of any `rumoca-*` dependency.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::fs;
use std::io::Write;
use std::path::{Component, Path, PathBuf};

use anyhow::{Context, Result};
use clap::Parser;
use flate2::{Compression, write::GzEncoder};
use rayon::prelude::*;
use rumoca_compile::parsing::{StoredDefinition, parse_source_to_ast};

#[derive(Parser)]
#[command(name = "rumoca-docs-cache")]
#[command(about = "Build the mdBook live-example source-root cache (cache.bin.gz)")]
struct Args {
    /// Output cache path. Written as `gzip(bincode(Vec<(uri, StoredDefinition)>))`,
    /// and ONLY if at least one file parses — so the caller detects an empty
    /// cache by the file's absence rather than parsing this program's output.
    #[arg(long)]
    out: PathBuf,

    /// A source root as `key=dir`. Every `.mo` under `dir` is included and keyed
    /// `key/<relative path>` (with a trailing version suffix stripped from the
    /// first path segment). Repeatable.
    #[arg(long = "source-root", value_parser = parse_source_root)]
    source_roots: Vec<(String, PathBuf)>,
}

fn parse_source_root(raw: &str) -> std::result::Result<(String, PathBuf), String> {
    let (key, dir) = raw
        .split_once('=')
        .ok_or_else(|| format!("expected `key=dir`, got `{raw}`"))?;
    if key.is_empty() || dir.is_empty() {
        return Err(format!("expected non-empty `key=dir`, got `{raw}`"));
    }
    Ok((key.to_string(), PathBuf::from(dir)))
}

#[derive(Clone)]
struct StagedModelicaFile {
    source_path: PathBuf,
    uri: String,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let mut files = Vec::new();
    for (key, dir) in &args.source_roots {
        if !dir.exists() {
            eprintln!(
                "rumoca-docs-cache: skipping missing source root {}",
                dir.display()
            );
            continue;
        }
        collect_modelica_sources_inner(dir, dir, key, &mut files)?;
    }

    let parsed = parse_staged_modelica_files(&files)?;
    if parsed.is_empty() {
        // Leave no cache file so the caller records `cache: null` in its manifest.
        eprintln!(
            "rumoca-docs-cache: no Modelica sources parsed; not writing {}",
            args.out.display()
        );
        return Ok(());
    }

    if let Some(parent) = args.out.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    let file = fs::File::create(&args.out)
        .with_context(|| format!("failed to create {}", args.out.display()))?;
    let mut encoder = GzEncoder::new(file, Compression::best());
    encoder
        .write_all(&bincode::serialize(&parsed)?)
        .with_context(|| format!("failed to write {}", args.out.display()))?;
    encoder
        .finish()
        .with_context(|| format!("failed to finish {}", args.out.display()))?;
    eprintln!(
        "rumoca-docs-cache: wrote {} definitions to {}",
        parsed.len(),
        args.out.display()
    );
    Ok(())
}

fn collect_modelica_sources_inner(
    root: &Path,
    dir: &Path,
    source_root_key: &str,
    files: &mut Vec<StagedModelicaFile>,
) -> Result<()> {
    for entry in sorted_dir_entries(dir)? {
        let path = entry.path();
        let file_type = entry
            .file_type()
            .with_context(|| format!("failed to stat {}", path.display()))?;
        if file_type.is_dir() {
            collect_modelica_sources_inner(root, &path, source_root_key, files)?;
        } else if file_type.is_file() && path.extension().is_some_and(|ext| ext == "mo") {
            let relative = path
                .strip_prefix(root)
                .with_context(|| format!("failed to relativize {}", path.display()))?;
            let normalized_relative = normalize_modelica_source_root_path(relative);
            let normalized_path = normalized_relative.to_string_lossy().replace('\\', "/");
            files.push(StagedModelicaFile {
                source_path: path,
                uri: format!("{source_root_key}/{normalized_path}"),
            });
        }
    }
    Ok(())
}

fn parse_staged_modelica_files(
    files: &[StagedModelicaFile],
) -> Result<Vec<(String, StoredDefinition)>> {
    let parsed = files
        .par_iter()
        .map(parse_staged_modelica_file)
        .collect::<Vec<_>>();
    let mut definitions = Vec::new();
    for result in parsed {
        if let Some(definition) = result? {
            definitions.push(definition);
        }
    }
    Ok(definitions)
}

fn parse_staged_modelica_file(
    file: &StagedModelicaFile,
) -> Result<Option<(String, StoredDefinition)>> {
    let source = fs::read_to_string(&file.source_path)
        .with_context(|| format!("failed to read {}", file.source_path.display()))?;
    match parse_source_to_ast(&source, &file.uri) {
        Ok(definition) => Ok(Some((file.uri.clone(), definition))),
        Err(error) => {
            eprintln!(
                "Skipping docs source-root cache entry {}: {error}",
                file.uri
            );
            Ok(None)
        }
    }
}

fn normalize_modelica_source_root_path(relative: &Path) -> PathBuf {
    let mut components = relative.components();
    let Some(first) = components.next() else {
        return PathBuf::new();
    };
    let mut normalized = PathBuf::new();
    match first {
        Component::Normal(name) => {
            normalized.push(strip_trailing_version_suffix(&name.to_string_lossy()));
        }
        other => normalized.push(other.as_os_str()),
    }
    for component in components {
        normalized.push(component.as_os_str());
    }
    normalized
}

fn strip_trailing_version_suffix(name: &str) -> String {
    let Some(separator) = name.rfind([' ', '-']) else {
        return name.to_owned();
    };
    let suffix = &name[separator + 1..];
    if suffix.chars().any(|ch| ch.is_ascii_digit())
        && suffix.chars().all(|ch| ch.is_ascii_digit() || ch == '.')
    {
        name[..separator].to_owned()
    } else {
        name.to_owned()
    }
}

fn sorted_dir_entries(dir: &Path) -> Result<Vec<fs::DirEntry>> {
    let mut entries = fs::read_dir(dir)
        .with_context(|| format!("failed to read {}", dir.display()))?
        .collect::<std::result::Result<Vec<_>, _>>()
        .with_context(|| format!("failed to read entries in {}", dir.display()))?;
    entries.sort_by_key(|entry| entry.file_name());
    Ok(entries)
}
