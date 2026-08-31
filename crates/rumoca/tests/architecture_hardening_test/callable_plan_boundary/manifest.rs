//! Cargo-surface allowlist for the callable crates.
//!
//! The Rust scans read the module graph Cargo compiles from a library or
//! binary root. A build script, a proc-macro crate, or an unreviewed
//! dependency introduces code the scans never see, so the manifest itself is
//! pinned here: no build script, no proc-macro target, no build dependencies,
//! and an exact production and dev dependency roster.

use std::collections::BTreeSet;

use super::super::architecture_hardening_support::{
    all_manifest_dependency_names, production_dependency_names, section_dependency_names,
};
use super::CALLABLE_CRATES;

/// Every dependency a callable crate may declare, by table.
struct CallableManifestPolicy {
    crate_name: &'static str,
    production: &'static [&'static str],
    dev: &'static [&'static str],
}

const POLICIES: &[CallableManifestPolicy] = &[
    CallableManifestPolicy {
        crate_name: "rumoca-plan-callable",
        production: &["rumoca-core", "rumoca-ir-dae", "thiserror"],
        // Wire-replay round-trips in the crate's own tests reissue a checked
        // DAE through its current-version wire form. Test-only on purpose: no
        // production module may name a codec.
        dev: &["serde_json"],
    },
    CallableManifestPolicy {
        crate_name: "rumoca-phase-callable",
        production: &["rumoca-core", "rumoca-ir-dae", "rumoca-plan-callable"],
        dev: &[],
    },
];

fn manifest_findings(
    policy: &CallableManifestPolicy,
    manifest: &str,
    has_build_script_file: bool,
) -> BTreeSet<String> {
    let mut findings = BTreeSet::new();
    let parsed: toml::Value = manifest
        .parse()
        .unwrap_or_else(|error| panic!("parse {} manifest: {error}", policy.crate_name));

    if has_build_script_file {
        findings.insert("crate ships a build.rs the Rust scans never read".to_owned());
    }
    if parsed
        .get("package")
        .and_then(|package| package.get("build"))
        .is_some()
    {
        findings.insert("manifest declares a `package.build` script".to_owned());
    }
    if parsed
        .get("lib")
        .and_then(|library| library.get("proc-macro"))
        .and_then(toml::Value::as_bool)
        .unwrap_or(false)
    {
        findings.insert("crate is a proc-macro target".to_owned());
    }

    for (table, dependency) in all_manifest_dependency_names(manifest) {
        if table.ends_with("build-dependencies") {
            findings.insert(format!(
                "build dependency `{dependency}` runs outside the scanned module graph"
            ));
        }
    }

    for dependency in production_dependency_names(manifest) {
        if !policy.production.contains(&dependency.as_str()) {
            findings.insert(format!("unreviewed production dependency `{dependency}`"));
        }
    }
    for dependency in section_dependency_names(manifest, "dev-dependencies") {
        if !policy.dev.contains(&dependency.as_str()) {
            findings.insert(format!("unreviewed dev dependency `{dependency}`"));
        }
    }
    findings
}

#[test]
fn callable_manifests_declare_no_build_script_proc_macro_or_unreviewed_dependency() {
    let root = super::super::workspace_root();
    let mut findings = Vec::new();
    for policy in POLICIES {
        let crate_root = root.join("crates").join(policy.crate_name);
        let manifest = std::fs::read_to_string(crate_root.join("Cargo.toml"))
            .unwrap_or_else(|error| panic!("read {} manifest: {error}", policy.crate_name));
        findings.extend(
            manifest_findings(policy, &manifest, crate_root.join("build.rs").is_file())
                .into_iter()
                .map(|finding| format!("{}: {finding}", policy.crate_name)),
        );
    }
    assert!(
        findings.is_empty(),
        "the callable Cargo surface grew a route the Rust authority scans cannot see. \
Adding an edge here requires reviewing it against SPEC_0029 and listing it in this gate: \
{findings:#?}"
    );
}

#[test]
fn every_callable_crate_has_a_manifest_policy() {
    let covered = POLICIES
        .iter()
        .map(|policy| policy.crate_name)
        .collect::<BTreeSet<_>>();
    let expected = CALLABLE_CRATES.iter().copied().collect::<BTreeSet<_>>();
    assert_eq!(
        covered, expected,
        "a callable crate without a manifest policy is unscanned"
    );
}

#[test]
fn callable_manifest_gate_detects_build_proc_macro_and_dependency_mutations() {
    let policy = &POLICIES[0];
    let clean = "[package]\nname = \"rumoca-plan-callable\"\n\n[dependencies]\n\
rumoca-core = { workspace = true }\nrumoca-ir-dae = { workspace = true }\n\
thiserror = { workspace = true }\n\n[dev-dependencies]\nserde_json = { workspace = true }\n";
    assert!(
        manifest_findings(policy, clean, false).is_empty(),
        "the current declared surface must pass"
    );

    for (mutation, reason) in [
        (
            format!("{clean}\n[build-dependencies]\nserde_codegen = \"1\"\n"),
            "build dependency",
        ),
        (
            format!("{clean}\n[lib]\nproc-macro = true\n"),
            "proc-macro target",
        ),
        (
            clean.replace(
                "name = \"rumoca-plan-callable\"",
                "name = \"rumoca-plan-callable\"\nbuild = \"build.rs\"",
            ),
            "declared build script",
        ),
        (
            format!("{clean}[target.'cfg(unix)'.dependencies]\nbincode = \"2\"\n"),
            "target-gated production dependency",
        ),
        (
            clean.replace(
                "thiserror = { workspace = true }",
                "innocent = { package = \"bincode\", version = \"2\" }",
            ),
            "renamed production dependency",
        ),
        (format!("{clean}postcard = \"1\"\n"), "extra dev dependency"),
    ] {
        assert!(
            !manifest_findings(policy, &mutation, false).is_empty(),
            "manifest mutation escaped the gate: {reason}"
        );
    }

    assert!(
        !manifest_findings(policy, clean, true).is_empty(),
        "an on-disk build.rs must be reported even when the manifest is silent"
    );
}
