//! Compiler-lint coverage for construction-obligation mistakes.
//!
//! Rust ownership makes a consuming commit affine: the same guard cannot be
//! committed twice. These lints additionally reject the two easy omission
//! spellings, a bare `#[must_use]` value and `let _ = value`. Rust's forbid is
//! compiler-enforced. The Clippy deny plus first-party attribute inventory is
//! a hygiene backstop because external macros may emit group-level allows.
//! Neither is a linear proof that every guard is committed: a named binding or
//! explicit `drop` can still abandon a value, so semantic APIs must make
//! abandonment safe and construction must remain transactional.

use std::collections::BTreeSet;
use std::env;
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use quote::ToTokens;
use syn::parse::Parser;
use syn::punctuated::Punctuated;
use syn::visit::{self, Visit};

use super::architecture_hardening_support::{collect_rs_files, workspace_root};

const RUST_CONSTRUCTION_LINT: &str = "unused_must_use";
const CLIPPY_CONSTRUCTION_LINT: &str = "let_underscore_must_use";

#[test]
fn every_actual_workspace_member_enforces_dropped_construction_obligation_lints() {
    let root = workspace_root();
    let workspace = parse_manifest(&root.join("Cargo.toml"));
    assert_workspace_lint_level(&workspace, "rust", RUST_CONSTRUCTION_LINT, "forbid");
    assert_workspace_lint_level(&workspace, "clippy", CLIPPY_CONSTRUCTION_LINT, "deny");

    let mut offenders = Vec::new();
    for path in actual_workspace_member_manifests(&root) {
        let manifest = parse_manifest(&path);
        let inherits_workspace = manifest_value(&manifest, &["lints", "workspace"])
            .and_then(toml::Value::as_bool)
            == Some(true);
        let explicitly_enforces_both =
            manifest_value(&manifest, &["lints", "rust", RUST_CONSTRUCTION_LINT])
                .is_some_and(|value| lint_has_level(value, "forbid"))
                && manifest_value(&manifest, &["lints", "clippy", CLIPPY_CONSTRUCTION_LINT])
                    .is_some_and(|value| lint_has_level(value, "deny"));
        if !inherits_workspace && !explicitly_enforces_both {
            offenders.push(path.display().to_string());
        }
    }

    assert!(
        offenders.is_empty(),
        "actual workspace members require Rust `{RUST_CONSTRUCTION_LINT} = \"forbid\"` and \
Clippy `{CLIPPY_CONSTRUCTION_LINT} = \"deny\"`; inherit `[lints] workspace = true` or repeat \
both exact levels in a deliberately overridden lint table: \
{offenders:#?}"
    );
}

#[test]
fn lint_manifest_gate_requires_the_exact_reviewed_levels() {
    for (tool, lint, required, rejected) in [
        (
            "rust",
            RUST_CONSTRUCTION_LINT,
            "forbid",
            &["allow", "warn", "deny"][..],
        ),
        (
            "clippy",
            CLIPPY_CONSTRUCTION_LINT,
            "deny",
            &["allow", "warn", "forbid"][..],
        ),
    ] {
        for level in rejected {
            let fixture = format!("[lints.{tool}]\n{lint} = {{ level = \"{level}\" }}");
            let manifest: toml::Value = fixture.parse().expect("parse wrong lint fixture");
            assert!(
                !manifest_value(&manifest, &["lints", tool, lint])
                    .is_some_and(|value| lint_has_level(value, required)),
                "{tool}::{lint} unexpectedly accepted level {level} instead of {required}"
            );
        }
        let fixture = format!("[lints.{tool}]\n{lint} = \"{required}\"");
        let manifest: toml::Value = fixture.parse().expect("parse required lint fixture");
        assert!(
            manifest_value(&manifest, &["lints", tool, lint])
                .is_some_and(|value| lint_has_level(value, required)),
            "{tool}::{lint} must accept {required}"
        );
    }
}

#[test]
fn rust_forbid_cannot_be_downgraded_by_a_local_allow() {
    let temporary = tempfile::tempdir().expect("temporary rustc fixture");
    let source = temporary.path().join("lib.rs");
    fs::write(
        &source,
        r#"
            #![forbid(unused_must_use)]

            #[allow(unused_must_use)]
            fn discard() {
                Result::<(), ()>::Ok(());
            }
        "#,
    )
    .expect("write rustc forbid fixture");
    let output = Command::new(tool_from_env("RUSTC", "rustc"))
        .arg("--crate-name")
        .arg("construction_lint_fixture")
        .arg("--crate-type=lib")
        .arg("--edition=2024")
        .arg("--out-dir")
        .arg(temporary.path())
        .arg(&source)
        .output()
        .expect("run rustc forbid fixture");

    assert_local_allow_rejected(output, "rustc unused_must_use");
}

#[test]
fn clippy_deny_rejects_a_wildcard_must_use_discard() {
    let temporary = tempfile::tempdir().expect("temporary Clippy fixture");
    let source_dir = temporary.path().join("src");
    fs::create_dir(&source_dir).expect("create Clippy fixture source directory");
    fs::write(
        temporary.path().join("Cargo.toml"),
        r#"
            [package]
            name = "construction-lint-fixture"
            version = "0.0.0"
            edition = "2024"

            [lints.clippy]
            let_underscore_must_use = "deny"
        "#,
    )
    .expect("write Clippy fixture manifest");
    fs::write(
        source_dir.join("lib.rs"),
        r#"
            #[must_use]
            fn obligation() -> bool { true }

            fn discard() {
                let _ = obligation();
            }
        "#,
    )
    .expect("write Clippy deny fixture");
    let output = Command::new(tool_from_env("CARGO", "cargo"))
        .args(["clippy", "--offline", "--quiet"])
        .env("CARGO_TARGET_DIR", temporary.path().join("target"))
        .current_dir(temporary.path())
        .output()
        .expect("run Clippy deny fixture");

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !output.status.success(),
        "Clippy unexpectedly accepted a wildcard must-use discard"
    );
    assert!(
        stderr.contains("let_underscore_must_use") || stderr.contains("let-underscore-must-use"),
        "Clippy fixture failed for an unrelated reason: {stderr}"
    );
}

#[test]
fn first_party_source_cannot_locally_weaken_the_clippy_discard_lint() {
    let root = workspace_root();
    let mut files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut files);
    files.sort();
    let mut offenders = Vec::new();
    for path in files {
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
        offenders.extend(local_clippy_downgrades(&path, &source));
    }
    assert!(
        offenders.is_empty(),
        "first-party source may not locally allow/expect the exact Clippy discard lint, its \
restriction group, or the global warnings group; the workspace deny is a hygiene backstop and \
local weakening defeats it: \
{offenders:#?}"
    );
}

#[test]
fn local_clippy_downgrade_inventory_is_mutation_tested() {
    let fixture = r#"
        #[allow(clippy::let_underscore_must_use)]
        fn exact() {}

        #[expect(clippy::restriction)]
        fn group() {}

        #[cfg_attr(test, allow(clippy::restriction))]
        fn conditional() {}

        #[allow(warnings)]
        fn global_group() {}
    "#;
    assert_eq!(
        local_clippy_downgrades(Path::new("mutation.rs"), fixture).len(),
        4
    );
    let unrelated = format!("#{}[allow(clippy::too_many_lines)] fn unrelated() {{}}", "");
    assert!(local_clippy_downgrades(Path::new("control.rs"), &unrelated).is_empty());
}

#[test]
fn ci_lint_job_keeps_the_workspace_clippy_gate_connected() {
    let root = workspace_root();
    let command_source = fs::read_to_string(root.join("crates/xtask/src/test_cmd.rs"))
        .expect("read canonical Clippy command");
    for required_argument in [
        ".arg(\"clippy\")",
        ".arg(\"--workspace\")",
        ".arg(\"--all-targets\")",
        ".arg(\"--all-features\")",
    ] {
        assert!(
            command_source.contains(required_argument),
            "canonical Clippy command lost `{required_argument}`"
        );
    }
    let workflow = fs::read_to_string(root.join(".github/workflows/ci.yml"))
        .expect("read required CI workflow");
    assert!(
        workflow.contains("cargo xtask verify lint"),
        "required CI must execute the canonical workspace Clippy gate"
    );
}

fn local_clippy_downgrades(path: &Path, source: &str) -> Vec<String> {
    let syntax = syn::parse_file(source)
        .unwrap_or_else(|error| panic!("parse {} for lint downgrades: {error}", path.display()));
    let mut visitor = LocalClippyDowngradeVisitor {
        path,
        offenders: Vec::new(),
    };
    visitor.visit_file(&syntax);
    visitor.offenders
}

struct LocalClippyDowngradeVisitor<'a> {
    path: &'a Path,
    offenders: Vec<String>,
}

impl Visit<'_> for LocalClippyDowngradeVisitor<'_> {
    fn visit_attribute(&mut self, attribute: &syn::Attribute) {
        let compact = attribute
            .meta
            .to_token_stream()
            .to_string()
            .replace(' ', "");
        if meta_weakens_clippy_discard_lint(&attribute.meta) {
            self.offenders
                .push(format!("{}: {compact}", self.path.display()));
        }
        visit::visit_attribute(self, attribute);
    }
}

fn meta_weakens_clippy_discard_lint(meta: &syn::Meta) -> bool {
    let syn::Meta::List(list) = meta else {
        return false;
    };
    let Ok(arguments) =
        Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated.parse2(list.tokens.clone())
    else {
        return false;
    };
    if list.path.is_ident("allow") || list.path.is_ident("expect") {
        return arguments.iter().any(meta_names_protected_lint);
    }
    list.path.is_ident("cfg_attr")
        && arguments
            .iter()
            .skip(1)
            .any(meta_weakens_clippy_discard_lint)
}

fn meta_names_protected_lint(meta: &syn::Meta) -> bool {
    let syn::Meta::Path(path) = meta else {
        return false;
    };
    let lint = path.to_token_stream().to_string().replace(' ', "");
    matches!(
        lint.as_str(),
        "clippy::let_underscore_must_use" | "clippy::restriction" | "warnings"
    )
}

fn assert_workspace_lint_level(manifest: &toml::Value, tool: &str, lint: &str, required: &str) {
    assert!(
        manifest_value(manifest, &["workspace", "lints", tool, lint])
            .is_some_and(|value| lint_has_level(value, required)),
        "[workspace.lints.{tool}].{lint} must remain {required}"
    );
}

fn actual_workspace_member_manifests(root: &Path) -> Vec<PathBuf> {
    let output = Command::new(tool_from_env("CARGO", "cargo"))
        .args(["metadata", "--format-version=1", "--no-deps"])
        .current_dir(root)
        .output()
        .expect("run cargo metadata for actual workspace membership");
    assert!(
        output.status.success(),
        "cargo metadata failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let metadata: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("parse cargo metadata JSON");
    let workspace_members = metadata["workspace_members"]
        .as_array()
        .expect("cargo metadata workspace_members")
        .iter()
        .map(|member| member.as_str().expect("workspace member package id"))
        .collect::<BTreeSet<_>>();
    let mut manifests = metadata["packages"]
        .as_array()
        .expect("cargo metadata packages")
        .iter()
        .filter(|package| {
            package["id"]
                .as_str()
                .is_some_and(|id| workspace_members.contains(id))
        })
        .map(|package| {
            package["manifest_path"]
                .as_str()
                .map(PathBuf::from)
                .expect("workspace package manifest path")
        })
        .collect::<Vec<_>>();
    manifests.sort();
    assert_eq!(
        manifests.len(),
        workspace_members.len(),
        "every cargo workspace member must resolve to one package manifest"
    );
    manifests
}

fn parse_manifest(path: &Path) -> toml::Value {
    fs::read_to_string(path)
        .unwrap_or_else(|error| panic!("read {}: {error}", path.display()))
        .parse()
        .unwrap_or_else(|error| panic!("parse {}: {error}", path.display()))
}

fn lint_has_level(value: &toml::Value, required: &str) -> bool {
    value.as_str() == Some(required)
        || value
            .get("level")
            .and_then(toml::Value::as_str)
            .is_some_and(|level| level == required)
}

fn manifest_value<'a>(manifest: &'a toml::Value, path: &[&str]) -> Option<&'a toml::Value> {
    path.iter().try_fold(manifest, |value, key| value.get(*key))
}

fn tool_from_env(variable: &str, fallback: &str) -> OsString {
    env::var_os(variable).unwrap_or_else(|| OsString::from(fallback))
}

fn assert_local_allow_rejected(output: Output, label: &str) {
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !output.status.success(),
        "{label} fixture unexpectedly allowed a local downgrade"
    );
    assert!(
        stderr.contains("incompatible with previous forbid") || stderr.contains("E0453"),
        "{label} fixture failed for an unrelated reason: {stderr}"
    );
}
