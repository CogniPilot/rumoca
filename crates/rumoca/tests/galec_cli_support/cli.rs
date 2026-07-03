//! Shared CLI-driving helpers for the GALEC-family end-to-end suites
//! (`cli_target_galec.rs`, `cli_target_embedded_c_galec.rs`,
//! `cli_target_galec_production.rs`).
//!
//! Each suite includes the helper files it needs via
//! `#[path = "galec_cli_support/<file>.rs"]` (the `examples_smoke.rs`
//! include pattern), so every test binary compiles only helpers whose
//! every item it uses — keeping the workspace's zero-`allow` dead-code
//! discipline. This file is the set used by all three suites.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

/// Write `<model>.mo` holding `source` into `dir` and return its path.
pub(super) fn write_fixture(dir: &Path, model: &str, source: &str) -> PathBuf {
    let file = dir.join(format!("{model}.mo"));
    fs::write(&file, source).expect("write fixture");
    file
}

/// Run `rumoca compile <file> --target <target> -o <out_dir>` through the
/// real binary, so the whole chain is exercised: CLI dispatch → generic
/// capability gate → projection facade → templates → packaging.
pub(super) fn run_compile_target(file: &Path, target: &str, out_dir: &Path) -> Output {
    Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("compile")
        .arg(file)
        .arg("--target")
        .arg(target)
        .arg("-o")
        .arg(out_dir)
        .output()
        .unwrap_or_else(|error| panic!("run rumoca compile --target {target}: {error}"))
}

/// Drop ANSI SGR escapes so assertions see the plain diagnostic text
/// (miette colorizes stderr even when piped).
pub(super) fn strip_ansi(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut chars = text.chars();
    while let Some(ch) = chars.next() {
        if ch != '\u{1b}' {
            out.push(ch);
            continue;
        }
        for escaped in chars.by_ref() {
            if escaped.is_ascii_alphabetic() {
                break;
            }
        }
    }
    out
}
