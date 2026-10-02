//! SPEC_0008: every diagnostic code names one diagnostic.
//!
//! A code is minted in one of three ways: a `code(rumoca::<phase>::<CODE>)`
//! diagnostic attribute, a `const NAME: &str = "<CODE>";` registry entry, or a
//! bracketed `"[<CODE>] ..."` prefix of an `#[error(...)]` message. Two
//! attributes or registry entries with one code, or two messages with one
//! code, would let a report name either diagnostic; this gate refuses that.
//! Crates that only read codes (the MSL harness, the worker, xtask) mint none
//! and are not scanned for registry entries.

use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

/// Codes two variants of one diagnostic share on purpose: the same error
/// with and without a source span.
const SHARED_VARIANT_CODES: &[(&str, usize)] = &[("EI001", 2)];

/// Crates that read codes minted elsewhere.
const CONSUMER_CRATES: &[&str] = &["rumoca-test-msl", "rumoca-worker", "xtask"];

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("workspace root")
        .to_path_buf()
}

/// A diagnostic code: two or three uppercase letters, then three digits.
fn diagnostic_code(text: &str) -> Option<&str> {
    let letters = text.chars().take_while(char::is_ascii_uppercase).count();
    let digits = text[letters..]
        .chars()
        .take_while(char::is_ascii_digit)
        .count();
    ((2..=3).contains(&letters) && digits == 3).then(|| &text[..letters + 3])
}

#[derive(Default)]
struct Sites {
    /// Attribute and registry registrations.
    registrations: BTreeMap<String, Vec<String>>,
    /// Bracketed `#[error]` message prefixes.
    messages: BTreeMap<String, Vec<String>>,
}

impl Sites {
    fn scan_file(&mut self, path: &Path, consumer: bool) {
        let Ok(text) = fs::read_to_string(path) else {
            return;
        };
        let mut after_error_attribute = false;
        for (index, line) in text.lines().enumerate() {
            let trimmed = line.trim_start();
            let place = format!("{}:{}", path.display(), index + 1);
            if trimmed.starts_with("//") {
                continue;
            }
            if let Some(rest) = trimmed.split("code(rumoca::").nth(1)
                && let Some(code) = rest.split("::").nth(1).and_then(diagnostic_code)
            {
                push(&mut self.registrations, code, &place);
            }
            if !consumer
                && trimmed.contains("const ")
                && trimmed.contains(": &str = \"")
                && let Some(value) = trimmed.split(": &str = \"").nth(1)
                && let Some(code) = diagnostic_code(value)
                && value[code.len()..].starts_with("\";")
            {
                push(&mut self.registrations, code, &place);
            }
            let message = trimmed
                .strip_prefix("#[error(\"[")
                .or_else(|| {
                    after_error_attribute
                        .then(|| trimmed.strip_prefix("\"["))
                        .flatten()
                })
                .and_then(diagnostic_code);
            if let Some(code) = message {
                push(&mut self.messages, code, &place);
            }
            after_error_attribute = trimmed == "#[error(";
        }
    }
}

fn push(sites: &mut BTreeMap<String, Vec<String>>, code: &str, place: &str) {
    sites
        .entry(code.to_string())
        .or_default()
        .push(place.to_string());
}

fn scan() -> Sites {
    let mut sites = Sites::default();
    let crates = workspace_root().join("crates");
    for entry in walkdir::WalkDir::new(&crates)
        .into_iter()
        .filter_map(Result::ok)
    {
        let path = entry.path();
        let Ok(relative) = path.strip_prefix(&crates) else {
            continue;
        };
        let mut parts = relative.components().map(|part| part.as_os_str());
        let crate_name = parts.next().and_then(|name| name.to_str()).unwrap_or("");
        let in_src = parts.next().is_some_and(|part| part == "src");
        let in_tests = relative
            .components()
            .any(|part| part.as_os_str() == "tests" || part.as_os_str() == "tests.rs");
        if !in_src || in_tests || path.extension().is_none_or(|ext| ext != "rs") {
            continue;
        }
        sites.scan_file(path, CONSUMER_CRATES.contains(&crate_name));
    }
    sites
}

fn duplicates(sites: &BTreeMap<String, Vec<String>>, allowed: &[(&str, usize)]) -> Vec<String> {
    sites
        .iter()
        .filter(|(code, places)| {
            let limit = allowed
                .iter()
                .find(|(shared, _)| shared == code)
                .map_or(1, |(_, count)| *count);
            places.len() > limit
        })
        .map(|(code, places)| format!("{code}: {}", places.join(", ")))
        .collect()
}

#[test]
fn every_diagnostic_code_is_registered_once() {
    let sites = scan();
    assert!(
        sites.registrations.len() > 100,
        "the scan found {} registered codes; the attribute or registry form changed",
        sites.registrations.len()
    );
    let mut problems = duplicates(&sites.registrations, SHARED_VARIANT_CODES);
    problems.extend(duplicates(&sites.messages, &[]));
    assert!(
        problems.is_empty(),
        "diagnostic codes minted more than once:\n{}",
        problems.join("\n")
    );
}

#[test]
fn the_code_shape_is_recognized() {
    assert_eq!(diagnostic_code("EX004] x"), Some("EX004"));
    assert_eq!(diagnostic_code("EGT017"), Some("EGT017"));
    assert_eq!(diagnostic_code("E1"), None);
    assert_eq!(diagnostic_code("ABCD123"), None);
}
