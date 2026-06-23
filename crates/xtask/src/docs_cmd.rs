use anyhow::{Context, Result};
use clap::{Args, Subcommand, ValueEnum};
use flate2::{Compression, write::GzEncoder};
use rayon::prelude::*;
use rumoca_compile::parsing::{StoredDefinition, parse_source_to_ast};
use serde_json::json;
use std::fs;
use std::io::Write;
use std::path::{Component, Path, PathBuf};
use std::process::Command;

use crate::{run_status, static_server};

const USER_GUIDE_DIR: &str = "docs/user-guide";
const DEV_GUIDE_DIR: &str = "docs/dev-guide";
const USER_GUIDE_REPO_EXAMPLES_DIR: &str = "docs/user-guide/book/repo-examples";
const USER_GUIDE_URL: &str = "/docs/user-guide/book/";
const DEV_GUIDE_URL: &str = "/docs/dev-guide/book/";
const SOURCE_ROOTS_DIR: &str = "source-roots";
const SOURCE_ROOT_CACHE_FILE: &str = "cache.bin.gz";
const WORKSPACE_CONFIG_FILE: &str = "rumoca-workspace.toml";

struct DocsSourceRoot {
    key: &'static str,
    source: &'static str,
    matches: &'static [&'static str],
}

#[derive(Clone)]
struct StagedModelicaFile {
    source_path: PathBuf,
    uri: String,
}

const DOCS_SOURCE_ROOTS: &[DocsSourceRoot] = &[
    DocsSourceRoot {
        key: "target/msl/ModelicaStandardLibrary-4.1.0",
        source: "target/msl/ModelicaStandardLibrary-4.1.0",
        matches: &[
            "target/msl/ModelicaStandardLibrary-4.1.0",
            "ModelicaStandardLibrary-4.1.0",
        ],
    },
    DocsSourceRoot {
        key: "target/cmm/CMM-v0.0.2",
        source: "target/cmm/CMM-v0.0.2",
        matches: &["target/cmm/CMM-v0.0.2", "CMM-v0.0.2"],
    },
];

#[derive(Debug, Args, Clone)]
pub(crate) struct DocsArgs {
    #[command(subcommand)]
    pub(crate) command: DocsCommand,
}

#[derive(Debug, Subcommand, Clone, PartialEq, Eq)]
pub(crate) enum DocsCommand {
    /// Build mdBook user/developer guides
    Build(DocsBuildArgs),
    /// Build and serve the mdBook guides locally
    Serve(DocsServeArgs),
}

#[derive(Debug, Args, Clone, PartialEq, Eq)]
pub(crate) struct DocsBuildArgs {
    /// Book to build
    #[arg(long, value_enum, default_value_t = DocsBook::All)]
    pub(crate) book: DocsBook,
}

#[derive(Debug, Args, Clone, PartialEq, Eq)]
pub(crate) struct DocsServeArgs {
    /// Book to build before serving
    #[arg(long, value_enum, default_value_t = DocsBook::All)]
    pub(crate) book: DocsBook,
    /// Override serve port (default: PORT env or 8080)
    #[arg(long)]
    pub(crate) port: Option<u16>,
    /// Serve existing book output without rebuilding first
    #[arg(long)]
    pub(crate) skip_build: bool,
    /// Do not build a missing WASM package for live examples
    #[arg(long)]
    pub(crate) skip_wasm_build: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub(crate) enum DocsBook {
    All,
    User,
    Dev,
}

pub(crate) fn run(args: DocsArgs, root: &Path) -> Result<()> {
    match args.command {
        DocsCommand::Build(args) => build_books(root, args.book),
        DocsCommand::Serve(args) => serve(root, args),
    }
}

pub(crate) fn check(root: &Path) -> Result<()> {
    run_rustdoc(root)?;
    build_books(root, DocsBook::All)
}

fn serve(root: &Path, args: DocsServeArgs) -> Result<()> {
    if !args.skip_build {
        build_books(root, args.book)?;
    }
    if !args.skip_wasm_build {
        crate::ensure_docs_wasm_package(root)?;
    }
    let urls = urls_for_book(args.book);
    static_server::serve_with_options(
        root,
        static_server::ServeOptions {
            explicit_port: args.port,
            editor_pkg_subdir: None,
            default_path: default_path_for_book(args.book),
            urls: &urls,
        },
    )
}

fn build_books(root: &Path, book: DocsBook) -> Result<()> {
    for book_dir in book_dirs(book) {
        run_mdbook(root, book_dir)?;
        if *book_dir == USER_GUIDE_DIR {
            stage_user_guide_repo_examples(root)?;
        }
    }
    Ok(())
}

fn run_rustdoc(root: &Path) -> Result<()> {
    let mut doc = Command::new("cargo");
    doc.arg("doc")
        .arg("--workspace")
        .arg("--no-deps")
        .arg("--exclude")
        .arg("rumoca-bind-python")
        .arg("--exclude")
        .arg("rumoca-bind-wasm")
        .env("RUSTDOCFLAGS", "-D warnings")
        .current_dir(root);
    run_status(doc)
}

fn run_mdbook(root: &Path, book_dir: &str) -> Result<()> {
    let mut cmd = Command::new("mdbook");
    cmd.arg("build").arg(book_dir).current_dir(root);
    run_status(cmd)
}

fn stage_user_guide_repo_examples(root: &Path) -> Result<()> {
    let out_dir = root.join(USER_GUIDE_REPO_EXAMPLES_DIR);
    if out_dir.exists() {
        fs::remove_dir_all(&out_dir)
            .with_context(|| format!("failed to remove {}", out_dir.display()))?;
    }
    fs::create_dir_all(&out_dir)
        .with_context(|| format!("failed to create {}", out_dir.display()))?;

    for (source, target) in [
        ("examples/models", "models"),
        ("examples/simulation", "simulation"),
        ("examples/interactive", "interactive"),
        ("examples/assets", "assets"),
    ] {
        copy_dir_recursive(&root.join(source), &out_dir.join(target))?;
    }
    let staged_source_roots = stage_user_guide_source_roots(root, &out_dir.join(SOURCE_ROOTS_DIR))?;
    write_user_guide_workspace_config(&out_dir, &staged_source_roots)?;
    Ok(())
}

fn stage_user_guide_source_roots(root: &Path, out_dir: &Path) -> Result<Vec<String>> {
    fs::create_dir_all(out_dir)
        .with_context(|| format!("failed to create {}", out_dir.display()))?;
    let mut roots = Vec::new();
    let mut workspace_roots = Vec::new();
    let mut cache_files = Vec::new();
    for source_root in DOCS_SOURCE_ROOTS {
        let source = root.join(source_root.source);
        if !source.exists() {
            eprintln!(
                "Skipping docs source root {}; run `cargo xtask repo modelica-deps ensure` to stage it.",
                source.display()
            );
            continue;
        }
        cache_files.extend(collect_modelica_sources(&source, source_root.key)?);
        workspace_roots.push(source_root.key.to_string());
        roots.push(json!({
            "key": source_root.key,
            "matches": source_root.matches,
        }));
    }
    let parsed_count =
        write_source_root_cache(&cache_files, &out_dir.join(SOURCE_ROOT_CACHE_FILE))?;
    let manifest = json!({
        "version": 1,
        "cache": if parsed_count > 0 { Some(SOURCE_ROOT_CACHE_FILE) } else { None },
        "roots": roots
    });
    fs::write(
        out_dir.join("manifest.json"),
        serde_json::to_vec_pretty(&manifest)?,
    )
    .with_context(|| {
        format!(
            "failed to write {}",
            out_dir.join("manifest.json").display()
        )
    })?;
    Ok(workspace_roots)
}

fn write_user_guide_workspace_config(out_dir: &Path, source_roots: &[String]) -> Result<()> {
    let path = out_dir.join(WORKSPACE_CONFIG_FILE);
    let mut content = String::from("# Generated by `cargo xtask docs build`.\n");
    content.push_str("source_roots = [\n");
    for source_root in source_roots {
        content.push_str("    ");
        content.push_str(&serde_json::to_string(source_root)?);
        content.push_str(",\n");
    }
    content.push_str("]\n");
    fs::write(&path, content).with_context(|| format!("failed to write {}", path.display()))
}

fn collect_modelica_sources(
    source: &Path,
    source_root_key: &str,
) -> Result<Vec<StagedModelicaFile>> {
    let mut files = Vec::new();
    collect_modelica_sources_inner(source, source, source_root_key, &mut files)?;
    Ok(files)
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

fn write_source_root_cache(files: &[StagedModelicaFile], target: &Path) -> Result<usize> {
    let parsed = parse_staged_modelica_files(files)?;
    let file = fs::File::create(target)
        .with_context(|| format!("failed to create {}", target.display()))?;
    let mut encoder = GzEncoder::new(file, Compression::best());
    encoder
        .write_all(&bincode::serialize(&parsed)?)
        .with_context(|| format!("failed to write {}", target.display()))?;
    encoder
        .finish()
        .with_context(|| format!("failed to finish {}", target.display()))?;
    Ok(parsed.len())
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

fn copy_dir_recursive(source: &Path, target: &Path) -> Result<()> {
    fs::create_dir_all(target).with_context(|| format!("failed to create {}", target.display()))?;
    for entry in sorted_dir_entries(source)? {
        let source_path = entry.path();
        let target_path = target.join(entry.file_name());
        let file_type = entry
            .file_type()
            .with_context(|| format!("failed to stat {}", source_path.display()))?;
        if file_type.is_dir() {
            copy_dir_recursive(&source_path, &target_path)?;
        } else if file_type.is_file() {
            fs::copy(&source_path, &target_path).with_context(|| {
                format!(
                    "failed to copy {} to {}",
                    source_path.display(),
                    target_path.display()
                )
            })?;
        }
    }
    Ok(())
}

fn book_dirs(book: DocsBook) -> &'static [&'static str] {
    match book {
        DocsBook::All => &[USER_GUIDE_DIR, DEV_GUIDE_DIR],
        DocsBook::User => &[USER_GUIDE_DIR],
        DocsBook::Dev => &[DEV_GUIDE_DIR],
    }
}

fn default_path_for_book(book: DocsBook) -> &'static str {
    match book {
        DocsBook::All | DocsBook::User => "docs/user-guide/book/index.html",
        DocsBook::Dev => "docs/dev-guide/book/index.html",
    }
}

fn urls_for_book(book: DocsBook) -> Vec<(&'static str, &'static str)> {
    match book {
        DocsBook::All => vec![
            ("User guide", USER_GUIDE_URL),
            ("Developer guide", DEV_GUIDE_URL),
        ],
        DocsBook::User => vec![("User guide", USER_GUIDE_URL)],
        DocsBook::Dev => vec![("Developer guide", DEV_GUIDE_URL)],
    }
}
