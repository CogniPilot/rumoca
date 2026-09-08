//! Crate dependency-tier edge enforcement for the solve/runtime stack.
//!
//! Policy: SPEC_0029 §"Dependency Tiers" ("dependencies flow downward") and
//! §3b ("Runtime Policy Ownership"). `rumoca-eval-solve` is a Tier 3 crate: it
//! is a pure Solve-IR row evaluator / scalarizer. Component-private evaluation
//! (`SolveRuntime`) lives in `rumoca-solver::runtime`; the sole master driver
//! and session live in `rumoca-solver::fmi_me`.
//!
//! Before this module existed, `rumoca-eval-solve` depended *upward* on
//! `rumoca-solver` and hosted the driver, which even hard-coded the diffsol
//! backend's tracing namespace. These tests pin the corrected layering so the
//! edge cannot silently come back.

use super::*;
use std::collections::BTreeSet;
use std::path::PathBuf;

/// The complete, exact `[dependencies]` set of `rumoca-eval-solve`.
///
/// Pinned (not a "must not contain rumoca-solver" check) so that *any* new
/// dependency on a Tier 4/5 crate is a deliberate, reviewable change rather
/// than an accident.
const EVAL_SOLVE_DEPENDENCIES: &[&str] = &["indexmap", "rumoca-core", "rumoca-ir-solve", "tracing"];

fn read_manifest(crate_name: &str) -> String {
    let path = workspace_root()
        .join("crates")
        .join(crate_name)
        .join("Cargo.toml");
    fs::read_to_string(&path).unwrap_or_else(|error| {
        panic!("read {} manifest: {error}", path.display());
    })
}

#[test]
fn test_eval_solve_dependency_set_is_pinned() {
    let content = read_manifest("rumoca-eval-solve");
    let mut names = production_dependency_names(&content);
    names.sort();

    let expected: Vec<String> = EVAL_SOLVE_DEPENDENCIES
        .iter()
        .map(|name| (*name).to_string())
        .collect();

    assert_eq!(
        names, expected,
        "rumoca-eval-solve [dependencies] must stay exactly {expected:?} \
(SPEC_0029 Dependency Tiers: a Tier 3 evaluation crate may not depend on a \
Tier 4/5 crate such as rumoca-solver). Found {names:?}."
    );
}

#[test]
fn test_eval_solve_has_no_runtime_or_driver_sources() {
    let eval_solve_src = workspace_root().join("crates/rumoca-eval-solve/src");
    for relocated in [
        "sim_driver.rs",
        "runtime.rs",
        "runtime_events.rs",
        "jacobian.rs",
        "eval_at.rs",
        "delay.rs",
        "runtime",
    ] {
        let path = eval_solve_src.join(relocated);
        assert!(
            !path.exists(),
            "{} must not exist: the runtime state machine and simulation driver \
belong to rumoca-solver::runtime (SPEC_0029 §3b), not to the Tier 3 row evaluator",
            path.display()
        );
    }

    let solver_root = workspace_root().join("crates/rumoca-solver/src");
    for relocated in [
        "runtime/solve_runtime.rs",
        "runtime/solve_events.rs",
        "fmi_me/driver.rs",
        "fmi_me/session.rs",
    ] {
        let path = solver_root.join(relocated);
        assert!(
            path.exists(),
            "{} must exist: rumoca-solver owns component-private evaluation and the sole FMI \
ME master algorithm (SPEC_0029 §3b, SPEC_0044 §6-7)",
            path.display()
        );
    }
}

#[test]
fn test_no_backend_namespace_in_neutral_crates() {
    let mut files: Vec<PathBuf> = Vec::new();
    for crate_name in ["rumoca-eval-solve", "rumoca-solver"] {
        collect_rs_files(
            &workspace_root().join("crates").join(crate_name).join("src"),
            &mut files,
        );
    }

    let offenders: Vec<String> = files
        .iter()
        .filter(|path| {
            let content = fs::read_to_string(path).expect("read backend-neutral source");
            content.contains("rumoca_solver_diffsol")
        })
        .map(|path| path.display().to_string())
        .collect();

    assert!(
        offenders.is_empty(),
        "backend-neutral crates must not name a concrete backend's tracing \
namespace (SPEC_0029 §3b); the shared driver traces under \
rumoca_solver::driver. Offenders: {offenders:#?}"
    );

    let report = workspace_root().join("crates/rumoca-solver/src/runtime/report.rs");
    let report_content = fs::read_to_string(&report).expect("read runtime report module");
    assert!(
        report_content.contains("\"rumoca_solver::driver\""),
        "{} must define the backend-neutral DRIVER_TRACE_TARGET",
        report.display()
    );
}

/// Crates the runtime-contract crate may not name in `[dependencies]`.
///
/// DAE and phase preparation stay upstream of the contract; concrete backends
/// stay downstream of it.
const SOLVER_CONTRACT_BANNED_DEPENDENCIES: &[&str] = &[
    "rumoca-ir-dae",
    "rumoca-eval-dae",
    "rumoca-phase-dae",
    "rumoca-phase-structural",
    "rumoca-phase-solve",
    "diffsol",
    "rumoca-solver-diffsol",
    "rumoca-solver-rk45",
];

/// Every way `manifest` breaks the runtime-contract crate's dependency rule.
///
/// Reads the manifest through [`production_dependency_names`], so a banned
/// edge is found under every spelling and every production scope cargo
/// resolves, target-gated tables included, not only the inline plain-table
/// form.
fn solver_contract_offences(manifest: &str) -> Vec<String> {
    let declared = production_dependency_names(manifest);
    let mut offences = Vec::new();

    if !declared.iter().any(|name| name == "rumoca-eval-solve") {
        offences.push(
            "does not depend on `rumoca-eval-solve`: the runtime state machine evaluates \
prepared Solve-IR rows through the Tier 3 evaluator"
                .to_string(),
        );
    }

    offences.extend(
        SOLVER_CONTRACT_BANNED_DEPENDENCIES
            .iter()
            .filter(|banned| declared.iter().any(|name| name == *banned))
            .map(|banned| format!("depends on `{banned}`")),
    );

    offences
}

#[test]
fn test_solver_contract_crate_consumes_row_evaluator_only() {
    let content = read_manifest("rumoca-solver");
    let offences = solver_contract_offences(&content);

    assert!(
        offences.is_empty(),
        "rumoca-solver is the runtime-contract crate: DAE/phase preparation stays upstream \
and concrete backends stay downstream (SPEC_0029 Dependency Tiers). Found: {offences:#?}"
    );
}

/// A banned edge spelled `[dependencies.dep]` is the edge cargo resolves, so
/// the gate must refuse it exactly as it refuses `dep = { workspace = true }`.
///
/// A scan that reads only inline entries reports this manifest as clean, which
/// is why the two spellings are pinned against each other here.
#[test]
fn test_solver_contract_gate_reads_the_dependency_subtable_spelling() {
    const SUBTABLE_BANNED_EDGE: &str = "[package]\nname = \"rumoca-solver\"\n\n\
[dependencies]\nrumoca-eval-solve = { workspace = true }\n\n\
[dependencies.rumoca-ir-dae]\nworkspace = true\n\n[lints]\nworkspace = true\n";
    const SUBTABLE_REQUIRED_EDGE: &str = "[package]\nname = \"rumoca-solver\"\n\n\
[dependencies.rumoca-eval-solve]\nworkspace = true\n\n[lints]\nworkspace = true\n";

    assert!(
        solver_contract_offences(SUBTABLE_BANNED_EDGE)
            .iter()
            .any(|offence| offence.contains("rumoca-ir-dae")),
        "a banned dependency declared as `[dependencies.dep]` must be named in the refusal: \
cargo resolves it like any other production edge"
    );
    assert_eq!(
        solver_contract_offences(SUBTABLE_REQUIRED_EDGE),
        Vec::<String>::new(),
        "the same spelling must satisfy the required edge, so the gate reads one manifest \
model rather than two"
    );
}

// ---------------------------------------------------------------------------
// Phase-to-phase dependency policy
// ---------------------------------------------------------------------------

/// The complete current forward proof-edge set from SPEC_0029 §4.
///
/// Keep this separate from prerequisite services: these edges may name the
/// predecessor's opaque proof only in the successor mint's input position.
const PHASE_FORWARD_PROOF_EDGES: &[(&str, &str, &str)] = &[
    (
        "rumoca-phase-typecheck",
        "rumoca-phase-resolve",
        "Typecheck's sole mint accepts the opaque Resolve proof and immutable views",
    ),
    (
        "rumoca-phase-flatten",
        "rumoca-phase-typecheck",
        "Flatten's sole mint consumes the affine Typecheck proof by value",
    ),
];

/// Production phase-to-phase shared prerequisite services.
///
/// Phases lower one IR into the next, so they should compose through the IR and
/// eval crates rather than through each other: a phase that reaches into
/// another drags its neighbour's internals into its own contract and makes
/// either one harder to replace, reorder, or drop. The exceptions below are
/// shared *analysis* phases, which several later phases legitimately consume as
/// a prerequisite, rather than one lowering stage calling another.
///
/// Adding an entry here is the intended escape hatch, but it must be a
/// deliberate one with a stated reason. Removing an edge requires removing its
/// entry too: the gate rejects stale exceptions so this list cannot rot into a
/// record of dependencies that no longer exist.
///
/// Scope is `[dependencies]` only. Test fixtures legitimately build their
/// inputs by running upstream phases, so `[dev-dependencies]` edges are not
/// restricted.
const PHASE_SHARED_PREREQUISITE_EDGES: &[(&str, &str, &str)] = &[
    (
        "rumoca-phase-instantiate",
        "rumoca-phase-resolve",
        "name resolution is a shared prerequisite analysis consumed by later phases, not a lowering stage",
    ),
    (
        "rumoca-phase-galec",
        "rumoca-phase-structural",
        "structural analysis is a shared prerequisite consumed by both lowering targets",
    ),
    (
        "rumoca-phase-solve",
        "rumoca-phase-structural",
        "structural analysis is a shared prerequisite consumed by both lowering targets",
    ),
    (
        "rumoca-phase-autodiff",
        "rumoca-phase-parse",
        "Jacobian synthesis emits Modelica text and reads it back through the one parser, so \
parsing is a prerequisite service here and not a lowering stage",
    ),
];

#[test]
fn test_phase_crates_depend_on_each_other_only_by_recorded_exception() {
    let root = workspace_root();
    let mut observed: BTreeSet<(String, String)> = BTreeSet::new();

    for dir in workspace_crate_dirs(&root) {
        let Some(crate_name) = dir.file_name().and_then(|name| name.to_str()) else {
            continue;
        };
        if !crate_name.starts_with("rumoca-phase-") {
            continue;
        }
        let Ok(content) = fs::read_to_string(dir.join("Cargo.toml")) else {
            continue;
        };
        for dependency in production_dependency_names(&content) {
            if dependency.starts_with("rumoca-phase-") && dependency != crate_name {
                observed.insert((crate_name.to_string(), dependency));
            }
        }
    }

    assert_eq!(
        PHASE_FORWARD_PROOF_EDGES.len(),
        2,
        "SPEC_0029 §4 defines exactly two current forward proof edges"
    );
    let allowed: BTreeSet<(String, String)> = PHASE_FORWARD_PROOF_EDGES
        .iter()
        .chain(PHASE_SHARED_PREREQUISITE_EDGES)
        .map(|(from, to, _)| ((*from).to_string(), (*to).to_string()))
        .collect();

    let unrecorded: Vec<String> = observed
        .difference(&allowed)
        .map(|(from, to)| format!("{from} -> {to}"))
        .collect();
    assert!(
        unrecorded.is_empty(),
        "phase crates must not depend on other phase crates; compose through the IR/eval crates \
instead. Add a proof edge only to PHASE_FORWARD_PROOF_EDGES; add a genuine shared prerequisite \
only to PHASE_SHARED_PREREQUISITE_EDGES. Unrecorded: {unrecorded:?}"
    );

    let stale: Vec<String> = allowed
        .difference(&observed)
        .map(|(from, to)| format!("{from} -> {to}"))
        .collect();
    assert!(
        stale.is_empty(),
        "the phase proof/prerequisite catalogs record edges that no longer exist; delete them so \
the exception list stays a live record rather than history: {stale:?}"
    );
}

// ---------------------------------------------------------------------------
// Definitional-semantics trusted base
// ---------------------------------------------------------------------------

/// The complete, exact `[dependencies]` set of `rumoca-reference`.
///
/// Empty: the definitional semantics links nothing, so its trusted base is its
/// own source (SPEC_0037 §5). Every entry added here enlarges what a proof
/// about the semantics has to assume, which is why the set is pinned rather
/// than filtered for compiler crates alone.
const REFERENCE_DEPENDENCY_ALLOWLIST: &[&str] = &[];

/// Manifest sections `rumoca-reference` may declare.
///
/// Any other top-level section can carry production surface this gate does not
/// model: a `[target.'cfg(unix)'.dependencies]` table or a `[build-dependencies]`
/// table each add links that the `[dependencies]` allowlist never sees. The
/// gate refuses a section it cannot classify instead of passing it.
const REFERENCE_MANIFEST_SECTIONS: &[&str] =
    &["package", "dependencies", "dev-dependencies", "lints"];

/// The `rumoca-reference` manifest as its top-level sections, or the reason it
/// could not be read as such.
///
/// Every check below reads this, so text the gate cannot parse with certainty
/// becomes a named refusal rather than an empty finding (SPEC_0037 §2). There
/// is no path on which an unreadable manifest leaves a check silently
/// satisfied.
fn reference_manifest_sections(manifest: &str) -> Result<toml::Table, String> {
    toml::from_str::<toml::Table>(manifest)
        .map_err(|error| format!("manifest does not parse as TOML: {error}"))
}

/// Every name a `[dependencies]` entry puts into the trusted base.
///
/// A rename declares the real crate in `package`, so both the alias and the
/// package it resolves to are judged.
fn declared_dependency_names(alias: &str, spec: &toml::Value) -> Vec<String> {
    let mut names = vec![alias.to_string()];
    if let Some(package) = spec.get("package").and_then(toml::Value::as_str)
        && package != alias
    {
        names.push(package.to_string());
    }
    names
}

/// Why `name` may not sit in the definitional semantics' production set, if it
/// may not.
fn trusted_base_offence(name: &str) -> Option<String> {
    if name.starts_with("rumoca-") {
        Some(format!("production dependency on compiler crate `{name}`"))
    } else if REFERENCE_DEPENDENCY_ALLOWLIST.contains(&name) {
        None
    } else {
        Some(format!("production dependency `{name}` is not allowlisted"))
    }
}

/// Every way `manifest` grows the definitional semantics' trusted base.
///
/// Total over arbitrary text and closed against what it cannot classify: an
/// empty result means the gate read the whole manifest and found nothing, and
/// never that the gate failed to read it.
fn reference_trusted_base_offences(manifest: &str) -> Vec<String> {
    let sections = match reference_manifest_sections(manifest) {
        Ok(sections) => sections,
        Err(diagnostic) => return vec![diagnostic],
    };

    let mut offences: Vec<String> = sections
        .keys()
        .filter(|section| !REFERENCE_MANIFEST_SECTIONS.contains(&section.as_str()))
        .map(|section| {
            format!("section `[{section}]` is not one of {REFERENCE_MANIFEST_SECTIONS:?}")
        })
        .collect();

    match sections.get("dependencies") {
        None => {}
        Some(toml::Value::Table(dependencies)) => offences.extend(
            dependencies
                .iter()
                .flat_map(|(alias, spec)| declared_dependency_names(alias, spec))
                .filter_map(|name| trusted_base_offence(&name)),
        ),
        Some(other) => offences.push(format!(
            "`dependencies` is a {}, which this gate cannot classify",
            other.type_str()
        )),
    }

    offences
}

/// Whether the differential harness still links the real pipeline, or the
/// reason the gate cannot confirm that it does.
fn reference_keeps_compiler_dev_dependency(manifest: &str) -> Result<(), String> {
    let sections = reference_manifest_sections(manifest)?;
    match sections.get("dev-dependencies") {
        Some(toml::Value::Table(dev)) if dev.contains_key("rumoca-compile") => Ok(()),
        Some(toml::Value::Table(_)) => {
            Err("[dev-dependencies] does not name `rumoca-compile`".to_string())
        }
        Some(other) => Err(format!(
            "`dev-dependencies` is a {}, which this gate cannot classify",
            other.type_str()
        )),
        None => Err("the manifest declares no [dev-dependencies] section".to_string()),
    }
}

#[test]
fn test_reference_semantics_trusted_base_is_pinned() {
    let content = read_manifest("rumoca-reference");
    let offences = reference_trusted_base_offences(&content);

    assert!(
        offences.is_empty(),
        "rumoca-reference is the definitional semantics: the differentials and any future \
proof transcription trust whatever it links, so its production dependency set is pinned \
to {REFERENCE_DEPENDENCY_ALLOWLIST:?} by SPEC_0037 §5. Growing it enlarges the semantics' \
trusted base and is a deliberate decision, not a build detail: add the crate to \
REFERENCE_DEPENDENCY_ALLOWLIST (or the section to REFERENCE_MANIFEST_SECTIONS) in the same \
change, so the enlargement is reviewed here. Found: {offences:#?}"
    );

    if let Err(reason) = reference_keeps_compiler_dev_dependency(&content) {
        panic!(
            "rumoca-reference must keep its compiler dependency in [dev-dependencies]: the \
differential harness needs the real pipeline, and the pinned production set above is only \
meaningful while that harness still runs (SPEC_0037 §5). {reason}"
        );
    }
}

#[test]
fn test_reference_trusted_base_gate_rejects_synthetic_growth() {
    const COMPILER_DEPENDENCY: &str = "[package]\nname = \"rumoca-reference\"\n\n\
[dependencies]\nrumoca-compile = { workspace = true }\n\n[lints]\nworkspace = true\n";
    const EXTERNAL_DEPENDENCY: &str = "[package]\nname = \"rumoca-reference\"\n\n\
[dependencies]\nserde = \"1\"\n\n[lints]\nworkspace = true\n";
    const DEPENDENCY_SUB_TABLE: &str = "[package]\nname = \"rumoca-reference\"\n\n\
[dependencies.serde]\nversion = \"1\"\n\n[lints]\nworkspace = true\n";
    const TARGET_DEPENDENCY: &str = "[package]\nname = \"rumoca-reference\"\n\n\
[target.'cfg(unix)'.dependencies]\nlibc = \"0.2\"\n\n[lints]\nworkspace = true\n";
    const COMMENTED_HEADER: &str = "[package]\nname = \"rumoca-reference\"\n\n\
[dependencies] # production\nrumoca-compile = { workspace = true }\n\n\
[lints]\nworkspace = true\n";
    const ROOT_INLINE_TABLE: &str = "dependencies = { serde = \"1\" }\n\n\
[package]\nname = \"rumoca-reference\"\n\n[lints]\nworkspace = true\n";
    const RENAMED_COMPILER: &str = "[package]\nname = \"rumoca-reference\"\n\n\
[dependencies]\npipeline = { package = \"rumoca-compile\", workspace = true }\n\n\
[lints]\nworkspace = true\n";
    const DEPENDENCIES_NOT_A_TABLE: &str = "dependencies = \"everything\"\n\n\
[package]\nname = \"rumoca-reference\"\n\n[lints]\nworkspace = true\n";
    const SELF_NAMED_PACKAGE: &str = "[package]\nname = \"rumoca-reference\"\n\n\
[dependencies]\nserde = { package = \"serde\", version = \"1\" }\n\n\
[lints]\nworkspace = true\n";
    const UNPARSEABLE: &str = "[package]\nname = \"rumoca-reference\"\n\n\
[dependencies\nrumoca-compile = { workspace = true }\n";
    const PINNED_SHAPE: &str = "[package]\nname = \"rumoca-reference\"\n\n# comment\n\
[dependencies]\n\n[dev-dependencies]\nrumoca-compile = { workspace = true }\n\n\
[lints]\nworkspace = true\n";

    for (label, manifest) in [
        ("compiler dependency", COMPILER_DEPENDENCY),
        ("external dependency", EXTERNAL_DEPENDENCY),
        ("dependency sub-table", DEPENDENCY_SUB_TABLE),
        ("target-specific dependency", TARGET_DEPENDENCY),
        ("comment-suffixed section header", COMMENTED_HEADER),
        ("root-level inline dependency table", ROOT_INLINE_TABLE),
        ("renamed compiler dependency", RENAMED_COMPILER),
        ("non-table dependencies key", DEPENDENCIES_NOT_A_TABLE),
        ("manifest that does not parse", UNPARSEABLE),
    ] {
        assert!(
            !reference_trusted_base_offences(manifest).is_empty(),
            "the gate must reject a {label} added to rumoca-reference"
        );
    }

    for (label, manifest) in [
        ("compiler dependency", COMPILER_DEPENDENCY),
        ("comment-suffixed section header", COMMENTED_HEADER),
        ("renamed compiler dependency", RENAMED_COMPILER),
    ] {
        assert!(
            reference_trusted_base_offences(manifest)
                .iter()
                .any(|offence| offence.contains("rumoca-compile")),
            "a compiler dependency behind a {label} must be named in the refusal, not merely \
counted"
        );
    }

    assert!(
        reference_trusted_base_offences(RENAMED_COMPILER)
            .iter()
            .any(|offence| offence.contains("`pipeline`")),
        "a rename must be refused under its alias as well as under the package it resolves to, \
so the offence list can be read against the manifest text"
    );
    assert!(
        reference_trusted_base_offences(COMPILER_DEPENDENCY)
            .iter()
            .any(|offence| offence.contains("compiler crate")),
        "a compiler dependency and an unallowlisted external crate break two different rules of \
SPEC_0037 §5, and the refusal must say which one"
    );

    assert_eq!(
        reference_trusted_base_offences(SELF_NAMED_PACKAGE),
        vec!["production dependency `serde` is not allowlisted".to_string()],
        "a `package` key repeating its own alias names one crate, so it must be refused once"
    );

    assert!(
        reference_trusted_base_offences(UNPARSEABLE)
            .iter()
            .any(|offence| offence.contains("does not parse")),
        "text the gate cannot parse must refuse with a parse diagnostic, so an unreadable \
manifest reads as a failure rather than as a clean one"
    );
    assert!(
        reference_keeps_compiler_dev_dependency(UNPARSEABLE).is_err(),
        "the dev-dependency guard must also refuse text it cannot parse"
    );

    assert_eq!(
        reference_trusted_base_offences(PINNED_SHAPE),
        Vec::<String>::new(),
        "the shipped manifest shape must pass, or the gate cannot tell growth from it"
    );
    assert_eq!(
        reference_keeps_compiler_dev_dependency(PINNED_SHAPE),
        Ok(()),
        "the shipped manifest shape must satisfy the dev-dependency guard"
    );
    assert!(
        reference_keeps_compiler_dev_dependency(COMPILER_DEPENDENCY).is_err(),
        "moving the compiler dependency out of [dev-dependencies] must refuse, so the pinned \
production set cannot be satisfied by a manifest whose harness no longer runs"
    );
}

/// An array-of-tables root such as `[[bin]]` parses to an array, not a table,
/// and it carries production surface the `[dependencies]` allowlist never sees.
///
/// The section scan must therefore classify every top-level key by name. A scan
/// narrowed to the table-valued keys passes this manifest in silence, which is
/// the one shape the surrounding rejection cases cannot reach: every other
/// manifest they carry declares its surface in a table.
#[test]
fn test_reference_gate_classifies_array_of_tables_sections() {
    const ARRAY_OF_TABLES_SECTION: &str = "[package]\nname = \"rumoca-reference\"\n\n\
[[bin]]\nname = \"reference-cli\"\npath = \"src/main.rs\"\n\n\
[dev-dependencies]\nrumoca-compile = { workspace = true }\n\n[lints]\nworkspace = true\n";

    assert!(
        reference_trusted_base_offences(ARRAY_OF_TABLES_SECTION)
            .iter()
            .any(|offence| offence.contains("[bin]")),
        "a `[[bin]]` section must be refused by name, so the gate cannot regress to \
classifying only the table-valued sections"
    );
}
