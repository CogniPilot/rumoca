use anyhow::{Context, Result};
use clap::{Args, Subcommand, ValueEnum};
use serde_json::json;
use std::fs;
use std::path::Path;
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
        key: "target/cmm/CMM-a642c381",
        source: "target/cmm/CMM-a642c381",
        matches: &["target/cmm/CMM-a642c381", "CMM-a642c381"],
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
        ("examples/codegen", "codegen"),
        ("examples/interactive", "interactive"),
        ("examples/assets", "assets"),
    ] {
        if source == "examples/codegen" {
            copy_dir_recursive_excluding(&root.join(source), &out_dir.join(target), &["gen"])?;
        } else {
            copy_dir_recursive(&root.join(source), &out_dir.join(target))?;
        }
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
    let mut source_root_specs = Vec::new();
    for source_root in DOCS_SOURCE_ROOTS {
        let source = root.join(source_root.source);
        if !source.exists() {
            eprintln!(
                "Skipping docs source root {}; run `cargo xtask repo modelica-deps ensure` to stage it.",
                source.display()
            );
            continue;
        }
        source_root_specs.push(format!("{}={}", source_root.key, source.display()));
        workspace_roots.push(source_root.key.to_string());
        roots.push(json!({
            "key": source_root.key,
            "matches": source_root.matches,
        }));
    }

    let cache_path = out_dir.join(SOURCE_ROOT_CACHE_FILE);
    build_source_root_cache(root, &cache_path, &source_root_specs)?;
    // The builder writes the cache only when at least one file parses, so its
    // presence is the signal for whether the manifest should advertise it.
    let cache_present = cache_path.is_file();
    let manifest = json!({
        "version": 1,
        "cache": if cache_present { Some(SOURCE_ROOT_CACHE_FILE) } else { None },
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

/// Build the source-root cache by shelling out to the `rumoca-docs-cache` bin
/// (in rumoca-tool-docs, which links the compiler). This is the only
/// compiler-linked step of the docs workflow; keeping it behind a subprocess is
/// what lets `xtask` carry no `rumoca-*` dependency.
fn build_source_root_cache(
    root: &Path,
    cache_path: &Path,
    source_root_specs: &[String],
) -> Result<()> {
    if source_root_specs.is_empty() {
        return Ok(());
    }
    let mut cmd = Command::new("cargo");
    cmd.current_dir(root)
        .arg("run")
        .arg("--package")
        .arg("rumoca-tool-docs")
        .arg("--bin")
        .arg("rumoca-docs-cache")
        .arg("--")
        .arg("--out")
        .arg(cache_path);
    for spec in source_root_specs {
        cmd.arg("--source-root").arg(spec);
    }
    run_status(cmd)
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

fn sorted_dir_entries(dir: &Path) -> Result<Vec<fs::DirEntry>> {
    let mut entries = fs::read_dir(dir)
        .with_context(|| format!("failed to read {}", dir.display()))?
        .collect::<std::result::Result<Vec<_>, _>>()
        .with_context(|| format!("failed to read entries in {}", dir.display()))?;
    entries.sort_by_key(|entry| entry.file_name());
    Ok(entries)
}

fn copy_dir_recursive(source: &Path, target: &Path) -> Result<()> {
    copy_dir_recursive_excluding(source, target, &[])
}

fn copy_dir_recursive_excluding(
    source: &Path,
    target: &Path,
    excluded_names: &[&str],
) -> Result<()> {
    fs::create_dir_all(target).with_context(|| format!("failed to create {}", target.display()))?;
    for entry in sorted_dir_entries(source)? {
        let file_name = entry.file_name();
        if excluded_names
            .iter()
            .any(|excluded| file_name.to_string_lossy() == *excluded)
        {
            continue;
        }
        let source_path = entry.path();
        let target_path = target.join(file_name);
        let file_type = entry
            .file_type()
            .with_context(|| format!("failed to stat {}", source_path.display()))?;
        if file_type.is_dir() {
            copy_dir_recursive_excluding(&source_path, &target_path, excluded_names)?;
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
