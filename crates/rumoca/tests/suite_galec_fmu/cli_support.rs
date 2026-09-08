//! Shared CLI-driving helpers for the GALEC-family end-to-end suites
//! (`cli_target_galec.rs`, `cli_target_embedded_c_galec.rs`,
//! `cli_target_galec_production.rs`, `galec_equivalence.rs`).
//!
//! `suite_galec_fmu/main.rs` owns this normal module once, and its sibling suites
//! reach it as `super::cli_support`. A single owner prevents helper drift and
//! satisfies the workspace's zero-`allow` dead-code discipline.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use sha1::{Digest as _, Sha1};

/// Independently recompute one eFMI SHA-1 digest from shipped bytes.
pub(super) fn sha1_hex(bytes: &[u8]) -> String {
    format!("{:x}", Sha1::digest(bytes))
}

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
    run_compile_target_with(file, target, out_dir, &[])
}

/// As [`run_compile_target`], with additional `compile` flags appended.
///
/// The emission-policy suite needs the same invocation under several values of
/// one flag, and running the real binary is what makes the flag's whole path
/// (clap parse, CLI dispatch, target manifest, GALEC projection, templates)
/// part of what the assertion covers.
pub(super) fn run_compile_target_with(
    file: &Path,
    target: &str,
    out_dir: &Path,
    extra: &[&str],
) -> Output {
    Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("compile")
        .arg(file)
        .arg("--target")
        .arg(target)
        .arg("-o")
        .arg(out_dir)
        .args(extra)
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

/// Match semantic diagnostic text independently of miette's rendered layout.
///
/// The renderer may wrap both between words and inside a long path, prefixing
/// continuation lines with a `│` gutter. Removing only renderer whitespace and
/// that gutter from both sides preserves every non-layout character while
/// making assertions independent of terminal width.
pub(super) fn diagnostic_contains(text: &str, expected: &str) -> bool {
    fn without_layout(text: &str) -> String {
        strip_ansi(text)
            .chars()
            .filter(|character| !character.is_whitespace() && *character != '\u{2502}')
            .collect()
    }

    without_layout(text).contains(&without_layout(expected))
}
