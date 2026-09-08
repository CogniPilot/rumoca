//! SPEC_0007 / SPEC_0029 section 12 presentation-boundary ratchets.
//!
//! The syntax scan deliberately covers both `codegen/` and `views/`: moving a
//! semantic renderer into a directory with a permitted name does not change
//! what it does. Existing findings are an exact reviewed debt inventory; an
//! addition fails unless the protected gate and ledger change with it. The
//! legacy renderer surface has no allowance and is a tombstone.
//!
//! This is a source/AST architecture ratchet, not a semantic proof. It does
//! not scan `build.rs`, expand procedural/declarative macros or inspect
//! build-generated Rust, and it cannot prove the contents of opaque wrapper
//! types imported from elsewhere.
//! Changes to this gate and its exact debt ledger therefore remain review
//! boundaries rather than a substitute for compiler correctness proofs.

mod debt;
mod generated_template_registry;
mod jinja_scan;
#[cfg(test)]
mod mutation_tests;
mod rust_scan;

use debt::ALLOWED_SOURCE_DEBT;
use jinja_scan::{analyze_template_source, template_source_set_finding};
use rust_scan::{
    analyze_registry_usage, analyze_source_contexts, analyze_sources,
    production_rust_source_contexts, production_rust_sources, production_rust_target_roots,
    production_source_set_finding,
};

use super::*;
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

const CODEGEN_CRATE: &str = "crates/rumoca-phase-codegen";
const LEGACY_FILES: &[&str] = &["render_expr.rs", "render_stmt.rs", "render_stms.rs"];
const LEGACY_SPELLINGS: &[&str] = &[
    "render_expr",
    "render_expression",
    "render_stmt",
    "render_stms",
    "render_statement",
    "render_statements",
    "render_equation",
    "render_flat_equation",
];

fn content_fingerprint(content: &str) -> String {
    blake3::hash(content.as_bytes()).to_hex().to_string()
}

// These production dependencies predate the presentation boundary.  The
// ledger must equal the live findings: deleting debt requires deleting its
// entry in the same change, so a stale allowance cannot resurrect it later.
const ALLOWED_SEMANTIC_DEPENDENCY_DEBT: &[(&str, usize)] = &[
    ("dependencies:rumoca-eval-dae", 1),
    ("dependencies:rumoca-eval-solve", 1),
];

#[test]
fn test_legacy_semantic_renderers_remain_deleted() {
    let root = workspace_root().join(CODEGEN_CRATE);
    let offenders = legacy_surface_offenders(&root);
    assert!(
        offenders.is_empty(),
        "SPEC_0007/SPEC_0029: legacy semantic renderers are tombstoned; move semantics into a checked IR/view and spell syntax in templates:\n{}",
        offenders.join("\n")
    );
}

#[test]
fn test_codegen_presentation_boundary_matches_exact_reviewed_debt() {
    let root = workspace_root();
    let manifest = fs::read_to_string(root.join(CODEGEN_CRATE).join("Cargo.toml"))
        .expect("read phase-codegen Cargo.toml");
    let workspace_manifest =
        fs::read_to_string(root.join("Cargo.toml")).expect("read workspace Cargo.toml");
    let codec_dependency_violations =
        canonical_modelica_codec_dependency_violations(&manifest, &workspace_manifest);
    assert!(
        codec_dependency_violations.is_empty(),
        "the exact Modelica lexical delegate must resolve through the canonical local rumoca-core package; Cargo alias/package/path spoofing is forbidden:\n{codec_dependency_violations:#?}"
    );
    let allowed_dependencies = ALLOWED_SEMANTIC_DEPENDENCY_DEBT
        .iter()
        .copied()
        .collect::<BTreeMap<_, _>>();
    let dependency_findings = occurrence_counts(forbidden_manifest_dependencies(
        &manifest,
        &workspace_manifest,
    ));
    let dependency_mismatch = debt_mismatch(&dependency_findings, &allowed_dependencies);
    assert!(
        dependency_mismatch.is_empty(),
        "phase-codegen evaluator/lowering dependency ledger is not exact (Cargo package identity, including workspace aliases and target dependencies, is checked; remove stale allowances in the same change):\n{dependency_mismatch:#?}"
    );

    let source_findings = analyze_codegen_sources(&root);
    let unclassified_or_duplicate_registry = absolute_registry_findings(&source_findings);
    assert!(
        unclassified_or_duplicate_registry.is_empty(),
        "template-callable registrations require an explicit reviewed disposition and unique public name; unclassified/duplicate registrations can never enter the debt ledger:\n{unclassified_or_duplicate_registry:#?}"
    );
    let findings = grouped_findings(&source_findings);
    let allowed = source_debt_baseline();
    let mismatch = debt_mismatch(&findings, &allowed);
    assert!(
        mismatch.is_empty(),
        "phase-codegen semantic lowering/target-text debt ledger is not exact. Additions require an explicit gate/ledger review; remove stale entries with deleted debt:\n{mismatch:#?}"
    );
}

fn absolute_registry_findings(findings: &BTreeSet<String>) -> Vec<&str> {
    findings
        .iter()
        .filter(|finding| {
            finding.contains(":disposition-unclassified")
                || finding.contains(":template-registry-duplicate-public-name:")
                || finding.contains(":template-registry-unresolved-token-count:")
                || finding.contains(":template-callable-constructor-unresolved-token-count:")
                || finding.contains(":template-object-call-method-token-count:")
                || finding.contains(":template-object-call-token-count:")
                || finding.contains(":template-object-trait-alias-token-count:")
                || finding.contains(":template-environment-alias-token-count:")
                || finding.contains(":template-environment-type-alias-token-count:")
                || finding.contains(":template-unknown-method-callback-token-count:")
                || finding.contains(":template-custom-formatter-token-count:")
                || finding.contains(":template-ambiguous-default-constructor-token-count:")
                || finding.contains(":template-environment-policy:")
                || finding.contains(":template-registry-unused-public-command:")
                || finding.contains(":template-lex-error:")
        })
        .map(String::as_str)
        .collect()
}

fn debt_mismatch<K, A>(findings: &BTreeMap<K, usize>, allowed: &BTreeMap<A, usize>) -> Vec<String>
where
    K: Ord + AsRef<str>,
    A: Ord + AsRef<str>,
{
    let current = findings
        .iter()
        .map(|(key, count)| (key.as_ref(), *count))
        .collect::<BTreeMap<_, _>>();
    let baseline = allowed
        .iter()
        .map(|(key, count)| (key.as_ref(), *count))
        .collect::<BTreeMap<_, _>>();
    current
        .keys()
        .chain(baseline.keys())
        .copied()
        .collect::<BTreeSet<_>>()
        .into_iter()
        .filter_map(|identity| {
            let live = current.get(identity).copied().unwrap_or(0);
            let recorded = baseline.get(identity).copied().unwrap_or(0);
            (live != recorded).then(|| format!("{identity}: live {live}, ledger {recorded}"))
        })
        .collect()
}

fn source_debt_baseline() -> BTreeMap<&'static str, usize> {
    parse_source_debt_baseline(ALLOWED_SOURCE_DEBT)
}

fn parse_source_debt_baseline<'a>(partitions: &'a [&'a str]) -> BTreeMap<&'a str, usize> {
    let mut baseline = BTreeMap::new();
    for line in partitions.iter().flat_map(|partition| partition.lines()) {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let (identity, count) = line
            .rsplit_once('|')
            .unwrap_or_else(|| panic!("invalid codegen debt identity: {line}"));
        let count = count
            .parse::<usize>()
            .unwrap_or_else(|error| panic!("invalid codegen debt count in {line}: {error}"));
        assert!(
            baseline.insert(identity, count).is_none(),
            "duplicate codegen debt identity: {identity}"
        );
    }
    baseline
}

fn grouped_findings(findings: &BTreeSet<String>) -> BTreeMap<String, usize> {
    let mut grouped = BTreeMap::new();
    for finding in findings {
        let key = finding
            .rsplit_once('#')
            .filter(|(_, ordinal)| ordinal.parse::<usize>().is_ok())
            .map_or(finding.as_str(), |(key, _)| key);
        *grouped.entry(key.to_string()).or_default() += 1;
    }
    grouped
}

fn occurrence_counts(findings: Vec<String>) -> BTreeMap<String, usize> {
    let mut counts = BTreeMap::new();
    for finding in findings {
        *counts.entry(finding).or_default() += 1;
    }
    counts
}

fn legacy_surface_offenders(crate_root: &Path) -> Vec<String> {
    let mut files = Vec::new();
    collect_regular_files(crate_root, &mut files);
    let mut offenders = Vec::new();
    for path in files {
        if path
            .file_name()
            .and_then(|name| name.to_str())
            .is_some_and(|name| LEGACY_FILES.contains(&name))
        {
            offenders.push(format!("legacy file: {}", path.display()));
        }
        let Ok(source) = fs::read_to_string(&path) else {
            continue;
        };
        for spelling in LEGACY_SPELLINGS {
            for ordinal in word_occurrences(&source, spelling) {
                offenders.push(format!(
                    "legacy spelling `{spelling}` occurrence {ordinal}: {}",
                    path.display()
                ));
            }
        }
    }
    if crate_root.join("Cargo.toml").is_file() {
        for (path, source) in production_rust_sources(crate_root, crate_root) {
            let syntax = syn::parse_file(&source).unwrap_or_else(|error| {
                panic!("parse production module {}: {error}", path.display())
            });
            let mut visitor = ProductionPathAttributeVisitor::default();
            syn::visit::Visit::visit_file(&mut visitor, &syntax);
            offenders.extend(visitor.conditional_modules.into_iter().map(|module| {
                format!(
                    "production #[cfg_attr(..., path = ...)] module `{module}`: {}",
                    path.display()
                )
            }));
        }
    }
    offenders.sort();
    offenders
}

#[derive(Default)]
struct ProductionPathAttributeVisitor {
    conditional_modules: Vec<String>,
}

impl<'ast> syn::visit::Visit<'ast> for ProductionPathAttributeVisitor {
    fn visit_item_mod(&mut self, module: &'ast syn::ItemMod) {
        if super::architecture_hardening_support::attributes_require_test(&module.attrs) {
            return;
        }
        if module.attrs.iter().any(attribute_conditionally_sets_path) {
            self.conditional_modules.push(module.ident.to_string());
        }
        syn::visit::visit_item_mod(self, module);
    }
}

fn attribute_conditionally_sets_path(attribute: &syn::Attribute) -> bool {
    let syn::Meta::List(list) = &attribute.meta else {
        return false;
    };
    super::cfg_attr_sets_production_path(list)
}

#[test]
fn conditional_module_paths_follow_production_attributes() {
    for (attribute, expected) in [
        ("#[cfg_attr(test, path = \"tests.rs\")]", false),
        ("#[cfg_attr(any(test, kani), path = \"proof.rs\")]", false),
        ("#[cfg_attr(feature = \"fmi\", path = \"fmi.rs\")]", true),
        ("#[cfg_attr(not(kani), path = \"production.rs\")]", true),
        ("#[cfg_attr(feature = \"fmi\", doc = \"path\")]", false),
        (
            "#[cfg_attr(feature = \"a,b\", cfg_attr(not(kani), path = \"nested.rs\"))]",
            true,
        ),
        (
            "#[cfg_attr(feature = \"a(b\", cfg_attr(kani, path = \"proof.rs\"))]",
            false,
        ),
    ] {
        let item = syn::parse_str::<syn::ItemMod>(&format!("{attribute} mod subject;")).unwrap();
        assert_eq!(
            attribute_conditionally_sets_path(&item.attrs[0]),
            expected,
            "{attribute}"
        );
    }
}

fn collect_regular_files(directory: &Path, output: &mut Vec<PathBuf>) {
    let mut entries = fs::read_dir(directory)
        .unwrap_or_else(|error| panic!("read {}: {error}", directory.display()))
        .map(|entry| entry.expect("directory entry").path())
        .collect::<Vec<_>>();
    entries.sort();
    for path in entries {
        if path.is_dir() {
            collect_regular_files(&path, output);
        } else if path.is_file() {
            output.push(path);
        }
    }
}

fn word_occurrences(source: &str, needle: &str) -> Vec<usize> {
    source
        .match_indices(needle)
        .filter(|(offset, _)| {
            let before = source[..*offset].chars().next_back();
            let after = source[*offset + needle.len()..].chars().next();
            !before.is_some_and(is_identifier_character)
                && !after.is_some_and(is_identifier_character)
        })
        .enumerate()
        .map(|(ordinal, _)| ordinal + 1)
        .collect()
}

fn is_identifier_character(character: char) -> bool {
    character == '_' || character.is_ascii_alphanumeric()
}

fn forbidden_manifest_dependencies(manifest: &str, workspace_manifest: &str) -> Vec<String> {
    let workspace_packages = workspace_dependency_packages(workspace_manifest);
    let manifest = manifest
        .parse::<toml::Value>()
        .unwrap_or_else(|error| panic!("parse phase-codegen Cargo.toml: {error}"));
    manifest_dependencies(&manifest)
        .into_iter()
        .filter(|(table, _, _)| {
            matches!(table.as_str(), "dependencies" | "build-dependencies")
                || (table.starts_with("target.")
                    && (table.ends_with(".dependencies") || table.ends_with(".build-dependencies")))
        })
        .filter_map(|(table, declared_name, declared_package)| {
            let package = declared_package.as_deref().unwrap_or_else(|| {
                workspace_packages
                    .get(&declared_name)
                    .map_or(declared_name.as_str(), String::as_str)
            });
            (package.starts_with("rumoca-eval-")
                || (package.starts_with("rumoca-phase-") && package != "rumoca-phase-codegen"))
                .then(|| format!("{table}:{package}"))
        })
        .collect()
}

fn canonical_modelica_codec_dependency_violations(
    manifest: &str,
    workspace_manifest: &str,
) -> Vec<String> {
    let manifest = manifest
        .parse::<toml::Value>()
        .unwrap_or_else(|error| panic!("parse phase-codegen Cargo.toml: {error}"));
    let workspace_manifest = workspace_manifest
        .parse::<toml::Value>()
        .unwrap_or_else(|error| panic!("parse workspace Cargo.toml: {error}"));
    let mut violations = Vec::new();

    if manifest.get("workspace").is_some()
        || manifest
            .get("package")
            .and_then(|package| package.get("workspace"))
            .is_some()
    {
        violations.push(
            "phase-codegen must remain a member of the root workspace and must not declare or select a different workspace"
                .to_string(),
        );
    }
    let is_root_member = workspace_manifest
        .get("workspace")
        .and_then(|workspace| workspace.get("members"))
        .and_then(toml::Value::as_array)
        .is_some_and(|members| {
            members
                .iter()
                .any(|member| member.as_str() == Some("crates/rumoca-phase-codegen"))
        });
    if !is_root_member {
        violations.push(
            "root [workspace].members must contain crates/rumoca-phase-codegen exactly".to_string(),
        );
    }

    let phase_dependency = manifest
        .get("dependencies")
        .and_then(|dependencies| dependencies.get("rumoca-core"));
    match phase_dependency.and_then(toml::Value::as_table) {
        Some(specification)
            if specification.get("workspace").and_then(toml::Value::as_bool) == Some(true)
                && specification.get("package").is_none()
                && specification.get("path").is_none() => {}
        _ => violations.push(
            "phase-codegen [dependencies].rumoca-core must inherit the workspace dependency without a package or path remap"
                .to_string(),
        ),
    }

    let workspace_dependency = workspace_manifest
        .get("workspace")
        .and_then(|workspace| workspace.get("dependencies"))
        .and_then(|dependencies| dependencies.get("rumoca-core"));
    match workspace_dependency.and_then(toml::Value::as_table) {
        Some(specification)
            if specification
                .get("package")
                .and_then(toml::Value::as_str)
                .is_none_or(|package| package == "rumoca-core")
                && specification.get("path").and_then(toml::Value::as_str)
                    == Some("crates/rumoca-core") => {}
        _ => violations.push(
            "[workspace.dependencies].rumoca-core must resolve to package rumoca-core at crates/rumoca-core"
                .to_string(),
        ),
    }

    violations
}

fn workspace_dependency_packages(manifest: &str) -> BTreeMap<String, String> {
    let manifest = manifest
        .parse::<toml::Value>()
        .unwrap_or_else(|error| panic!("parse workspace Cargo.toml: {error}"));
    manifest
        .get("workspace")
        .and_then(|workspace| workspace.get("dependencies"))
        .and_then(toml::Value::as_table)
        .map(|dependencies| {
            dependencies
                .iter()
                .map(|(alias, specification)| {
                    let package = specification
                        .get("package")
                        .and_then(toml::Value::as_str)
                        .unwrap_or(alias);
                    (alias.clone(), package.to_string())
                })
                .collect()
        })
        .unwrap_or_default()
}

fn manifest_dependencies(manifest: &toml::Value) -> Vec<(String, String, Option<String>)> {
    fn walk(path: &str, value: &toml::Value, output: &mut Vec<(String, String, Option<String>)>) {
        let Some(table) = value.as_table() else {
            return;
        };
        for (key, child) in table {
            let child_path = if path.is_empty() {
                key.clone()
            } else {
                format!("{path}.{key}")
            };
            if matches!(key.as_str(), "dependencies" | "build-dependencies") {
                append_dependency_table(&child_path, child, output);
            } else {
                walk(&child_path, child, output);
            }
        }
    }

    fn append_dependency_table(
        path: &str,
        value: &toml::Value,
        output: &mut Vec<(String, String, Option<String>)>,
    ) {
        let dependencies = value
            .as_table()
            .unwrap_or_else(|| panic!("Cargo dependency table `{path}` is not a table"));
        for (alias, specification) in dependencies {
            let package = specification
                .get("package")
                .and_then(toml::Value::as_str)
                .map(str::to_string);
            output.push((path.to_string(), alias.clone(), package));
        }
    }

    let mut output = Vec::new();
    walk("", manifest, &mut output);
    output
}

fn analyze_codegen_sources(workspace: &Path) -> BTreeSet<String> {
    let crate_root = workspace.join(CODEGEN_CRATE);
    let src = crate_root.join("src");
    let roots = production_rust_target_roots(&crate_root, workspace);
    let contexts = production_rust_source_contexts(&crate_root, workspace);
    let sources = production_rust_sources(&crate_root, workspace);
    let mut findings = analyze_source_contexts(&contexts);
    let manifest = fs::read_to_string(crate_root.join("Cargo.toml"))
        .expect("read phase-codegen Cargo manifest for source-set identity");
    findings.insert(production_source_set_finding(&manifest, &roots, &sources));
    let templates = src.join("templates");
    let mut template_files = Vec::new();
    collect_regular_files(&templates, &mut template_files);
    let mut template_sources = Vec::new();
    for path in template_files {
        if path
            .extension()
            .is_some_and(|extension| extension == "jinja")
        {
            let relative = path.strip_prefix(workspace).unwrap_or(&path);
            let relative = relative.display().to_string();
            let source = fs::read_to_string(&path).expect("read builtin Jinja template");
            findings.extend(analyze_template_source(&relative, &source));
            template_sources.push((relative, source));
        }
    }
    findings.extend(analyze_registry_usage(&sources, &template_sources));
    findings.insert(template_source_set_finding(&template_sources));
    findings
}
