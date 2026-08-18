//! Parser ownership gates for pure IR crates (SPEC_0029 §3).

use super::*;
use std::path::{Path, PathBuf};

const PARSER_DEPENDENCIES: &[&str] = &[
    "lalrpop",
    "lalrpop-util",
    "parol",
    "parol_runtime",
    "pest",
    "pest_derive",
    "scnr2",
    "tree-sitter",
];

const GRAMMAR_EXTENSIONS: &[&str] = &["grammar", "lalrpop", "par", "pest"];

const GENERATED_PARSER_MARKERS: &[&str] = &[
    "parol_runtime",
    "scnr2",
    "LookaheadDFA",
    "GrammarAuto",
    "grammar_trait",
];

#[test]
fn test_ir_crates_do_not_own_source_parsers() {
    let root = workspace_root();
    let mut offenders = Vec::new();
    for crate_dir in ir_crate_directories(&root.join("crates")) {
        inspect_ir_crate(&crate_dir, &mut offenders);
    }
    assert!(
        offenders.is_empty(),
        "IR crates are pure checked data and must not own source parsers; \
move parser grammar, generated code, state, diagnostics, dependencies, and \
features into a rumoca-phase-parse* crate: {offenders:#?}"
    );
}

#[test]
fn test_galec_parser_public_api_returns_checked_data() {
    let root = workspace_root().join("crates/rumoca-phase-parse-galec/src");
    let public_api = fs::read_to_string(root.join("lib.rs")).expect("read GALEC parser API");
    let parser = fs::read_to_string(root.join("parse/mod.rs")).expect("read GALEC parser");

    assert!(
        public_api.contains(
            "pub fn parse(source: &str, file_name: &str) -> Result<CheckedAlgorithmBlock, GalecParseError>"
        ),
        "the production GALEC parse entry must return the opaque checked block"
    );
    assert!(
        !public_api.contains("pub use parse::{")
            && !public_api.contains("pub fn parse_expression")
            && parser.contains("pub(crate) fn parse_block")
            && parser.contains("pub(crate) fn parse_expression"),
        "raw GALEC block/expression parsing must remain private to the parse phase"
    );
}

fn ir_crate_directories(crates_dir: &Path) -> Vec<PathBuf> {
    let mut directories = fs::read_dir(crates_dir)
        .expect("read crates directory")
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| {
            path.is_dir()
                && path
                    .file_name()
                    .is_some_and(|name| name.to_string_lossy().starts_with("rumoca-ir-"))
        })
        .collect::<Vec<_>>();
    directories.sort();
    directories
}

fn inspect_ir_crate(crate_dir: &Path, offenders: &mut Vec<String>) {
    let source_parser = crate_dir.join("src/parse");
    if source_parser.exists() {
        offenders.push(source_parser.display().to_string());
    }

    collect_grammar_files(&crate_dir.join("src"), offenders);
    collect_generated_parser_sources(&crate_dir.join("src"), offenders);
    inspect_build_script(crate_dir, offenders);
    inspect_manifest(crate_dir, offenders);
}

fn collect_grammar_files(directory: &Path, offenders: &mut Vec<String>) {
    let Ok(entries) = fs::read_dir(directory) else {
        return;
    };
    for entry in entries.filter_map(Result::ok) {
        let path = entry.path();
        if path.is_dir() {
            collect_grammar_files(&path, offenders);
        } else if path.extension().is_some_and(|extension| {
            GRAMMAR_EXTENSIONS
                .iter()
                .any(|candidate| extension == *candidate)
        }) {
            offenders.push(path.display().to_string());
        }
    }
}

fn collect_generated_parser_sources(directory: &Path, offenders: &mut Vec<String>) {
    let Ok(entries) = fs::read_dir(directory) else {
        return;
    };
    for entry in entries.filter_map(Result::ok) {
        let path = entry.path();
        if path.is_dir() {
            collect_generated_parser_sources(&path, offenders);
            continue;
        }
        let file_name = path
            .file_name()
            .map(|name| name.to_string_lossy())
            .unwrap_or_default();
        let parser_named = file_name.contains("grammar") || file_name.contains("parser");
        let generated_marker = fs::read_to_string(&path).is_ok_and(|content| {
            GENERATED_PARSER_MARKERS
                .iter()
                .any(|marker| content.contains(marker))
        });
        if parser_named || generated_marker {
            offenders.push(path.display().to_string());
        }
    }
}

fn inspect_build_script(crate_dir: &Path, offenders: &mut Vec<String>) {
    let build_script = crate_dir.join("build.rs");
    let Ok(content) = fs::read_to_string(&build_script) else {
        return;
    };
    if PARSER_DEPENDENCIES
        .iter()
        .any(|dependency| content.contains(dependency))
    {
        offenders.push(build_script.display().to_string());
    }
}

fn inspect_manifest(crate_dir: &Path, offenders: &mut Vec<String>) {
    let manifest = crate_dir.join("Cargo.toml");
    let content = fs::read_to_string(&manifest).expect("read IR crate manifest");
    for dependency in PARSER_DEPENDENCIES {
        if manifest_declares_key(&content, dependency) {
            offenders.push(format!("{}: dependency `{dependency}`", manifest.display()));
        }
    }
    for feature in manifest_feature_names(&content) {
        if feature.contains("parse") || feature.contains("parser") {
            offenders.push(format!(
                "{}: parser feature `{feature}`",
                manifest.display()
            ));
        }
    }
}

fn manifest_declares_key(content: &str, key: &str) -> bool {
    content.lines().any(|line| {
        let line = line.split('#').next().unwrap_or_default().trim();
        line.split_once('=')
            .is_some_and(|(candidate, _)| candidate.trim() == key)
    })
}

fn manifest_feature_names(content: &str) -> Vec<&str> {
    let mut in_features = false;
    let mut features = Vec::new();
    for line in content.lines() {
        let line = line.split('#').next().unwrap_or_default().trim();
        if line.starts_with('[') {
            in_features = line == "[features]";
            continue;
        }
        if in_features && let Some((name, _)) = line.split_once('=') {
            features.push(name.trim());
        }
    }
    features
}
