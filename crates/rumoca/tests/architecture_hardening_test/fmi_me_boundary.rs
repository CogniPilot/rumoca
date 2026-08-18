//! SPEC_0038 §Internal Solver Boundary enforcement for the rk-like host.

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

use super::architecture_hardening_support::workspace_root;

/// Runtime exports that are host infrastructure rather than model access.
///
/// Every other `pub use runtime::...` name is discovered from
/// `rumoca-solver/src/lib.rs`, so adding a new private re-export cannot silently
/// open a hole in this boundary. An allowlist entry that disappears is an error:
/// stale exemptions should be deleted, not quietly retained.
const ALLOWED_HOST_RUNTIME_SURFACES: [&str; 3] =
    ["TimeoutBudget", "TimeoutExceeded", "time_match_with_tol"];

/// SPEC_0038 §Internal Solver Boundary: integrators reach the model only
/// through the FMI 3 ME contract.
///
/// "MUST NOT inspect Solve rows, layouts, opcodes, events, or private runtime
/// objects" is enforced here two ways, both compile-visible:
///
/// 1. `rumoca-solver-rk45` links `rumoca-ir-solve` / `rumoca-eval-solve` in no
///    dependency table other than `dev-dependencies`, so naming a Solve type
///    from a production path is a compile error. A dev-dependency is fine: it
///    is not linked into the library, and unit-test fixtures need it to build
///    Solve models by hand.
/// 2. No production source in the crate names a private runtime object, even
///    though several are re-exported from `rumoca-solver`'s root.
///
/// Both checks are written to survive the obvious ways around them: every
/// `*dependencies*` table is scanned (not just the literal `[dependencies]`
/// header), and test sources are recognized by `#[cfg(test)]` module-graph
/// position rather than by being named `tests.rs`.
#[test]
fn test_rk_like_host_reaches_the_model_only_through_the_fmi_me_contract() {
    let crate_root = workspace_root().join("crates/rumoca-solver-rk45");
    let offenders = me_boundary_offenders(&crate_root);

    assert!(
        offenders.is_empty(),
        "SPEC_0038: the rk-like host must reach the model only through \
rumoca_solver::fmi_me; these production paths step around it instead:\n  {}",
        offenders.join("\n  "),
    );
}

/// Every way `crate_root` steps around the ME contract, as reportable lines.
///
/// Factored out of the assertion so the detector itself can be run against a
/// planted offender (see the negative controls below). A check whose parsers
/// are never exercised on a known-bad input can pass vacuously — if
/// `manifest_dependencies` or `test_only_paths` silently returned nothing, an
/// empty offender list would look exactly like compliance.
fn me_boundary_offenders(crate_root: &Path) -> Vec<String> {
    let mut offenders = Vec::new();

    // (1) Every dependency table except dev-dependencies, in every form:
    // `[dependencies]`, `[build-dependencies]`, `[dependencies.<name>]`, and
    // any `[target.'cfg(...)'.<kind>]` variant of those.
    let manifest = fs::read_to_string(crate_root.join("Cargo.toml"))
        .unwrap_or_else(|error| panic!("read {} Cargo.toml: {error}", crate_root.display()));
    let workspace_manifest_path = workspace_root().join("Cargo.toml");
    let workspace_manifest = fs::read_to_string(&workspace_manifest_path)
        .unwrap_or_else(|error| panic!("read {}: {error}", workspace_manifest_path.display()));
    for (table, name) in manifest_dependencies(&manifest, Some(&workspace_manifest)) {
        if table == "dev-dependencies" {
            continue;
        }
        if matches!(name.as_str(), "rumoca-ir-solve" | "rumoca-eval-solve") {
            offenders.push(format!(
                "Cargo.toml: `{name}` linked from `[{table}]`; only dev-dependencies may name Solve IR"
            ));
        }
    }

    // (2) Production sources: everything under src/ that is not reachable
    // only through a `#[cfg(test)]` module.
    let src = crate_root.join("src");
    let test_only = test_only_paths(&src);
    let private_runtime_surfaces = private_runtime_surfaces();
    let mut sources = Vec::new();
    collect_rust_sources(&src, &mut sources);
    sources.sort();
    for path in sources {
        if is_test_only(&path, &test_only) {
            continue;
        }
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
        for surface in &private_runtime_surfaces {
            if source.contains(surface) {
                offenders.push(format!(
                    "{}: names `{surface}`",
                    path.strip_prefix(crate_root).unwrap_or(&path).display()
                ));
            }
        }
    }

    offenders
}

/// Root-exported runtime names an integrator host may not use.
fn private_runtime_surfaces() -> BTreeSet<String> {
    let solver_root = workspace_root().join("crates/rumoca-solver/src/lib.rs");
    let source = fs::read_to_string(&solver_root)
        .unwrap_or_else(|error| panic!("read {}: {error}", solver_root.display()));
    let mut surfaces = BTreeSet::new();
    let mut remaining = source.as_str();
    while let Some(start) = remaining.find("pub use runtime::") {
        let tail = &remaining[start..];
        let end = tail
            .find(';')
            .expect("every runtime re-export declaration terminates");
        let declaration = &tail[..end];
        let open = declaration
            .find('{')
            .expect("runtime re-exports use an explicit name list");
        let close = declaration
            .rfind('}')
            .expect("runtime re-export name list closes");
        for item in declaration[open + 1..close].split(',') {
            let public_name = item
                .split_once(" as ")
                .map_or(item, |(_, alias)| alias)
                .trim();
            if !public_name.is_empty() {
                surfaces.insert(public_name.to_string());
            }
        }
        remaining = &tail[end + 1..];
    }
    assert!(
        !surfaces.is_empty(),
        "the runtime re-export inventory must not pass vacuously"
    );
    for allowed in ALLOWED_HOST_RUNTIME_SURFACES {
        assert!(
            surfaces.remove(allowed),
            "stale host-runtime allowlist entry `{allowed}`"
        );
    }
    surfaces
}

/// Every `(table-kind, dependency-name)` pair a Cargo manifest declares.
///
/// Handles the plain tables, the `[<kind>.<name>]` sub-table form, and any
/// `[target.'cfg(...)'.<kind>]` prefix. The table kind is normalized to the
/// bare `dependencies` / `dev-dependencies` / `build-dependencies` segment.
fn manifest_dependencies(
    manifest: &str,
    workspace_manifest: Option<&str>,
) -> Vec<(String, String)> {
    let parsed: toml::Value = toml::from_str(manifest).expect("planted Cargo.toml parses");
    let workspace_packages = workspace_manifest
        .map(workspace_dependency_packages)
        .unwrap_or_default();
    let mut found = Vec::new();
    let root = parsed.as_table().expect("Cargo.toml root is a table");
    collect_dependency_tables(root, &workspace_packages, &mut found);
    if let Some(targets) = root.get("target").and_then(toml::Value::as_table) {
        for target in targets.values().filter_map(toml::Value::as_table) {
            collect_dependency_tables(target, &workspace_packages, &mut found);
        }
    }
    found
}

fn workspace_dependency_packages(manifest: &str) -> BTreeMap<String, String> {
    let parsed: toml::Value = toml::from_str(manifest).expect("workspace Cargo.toml parses");
    parsed
        .get("workspace")
        .and_then(toml::Value::as_table)
        .and_then(|workspace| workspace.get("dependencies"))
        .and_then(toml::Value::as_table)
        .map(|dependencies| {
            dependencies
                .iter()
                .map(|(alias, specification)| {
                    let package = specification
                        .as_table()
                        .and_then(|table| table.get("package"))
                        .and_then(toml::Value::as_str)
                        .unwrap_or(alias);
                    (alias.clone(), package.to_string())
                })
                .collect()
        })
        .unwrap_or_default()
}

fn collect_dependency_tables(
    manifest_table: &toml::map::Map<String, toml::Value>,
    workspace_packages: &BTreeMap<String, String>,
    found: &mut Vec<(String, String)>,
) {
    for kind in ["dependencies", "dev-dependencies", "build-dependencies"] {
        let Some(dependencies) = manifest_table.get(kind).and_then(toml::Value::as_table) else {
            continue;
        };
        for (alias, specification) in dependencies {
            let package = specification
                .as_table()
                .and_then(|table| table.get("package"))
                .and_then(toml::Value::as_str)
                .or_else(|| {
                    specification
                        .as_table()
                        .and_then(|table| table.get("workspace"))
                        .and_then(toml::Value::as_bool)
                        .filter(|inherited| *inherited)
                        .and_then(|_| workspace_packages.get(alias).map(String::as_str))
                })
                .unwrap_or(alias);
            found.push((kind.to_string(), package.to_string()));
        }
    }
}

/// Paths that are only compiled into the crate under `#[cfg(test)]`.
///
/// Resolved by walking the real module graph from `src/lib.rs`: a `mod x;`
/// declaration is followed to its file, and a `#[cfg(test)]`-gated
/// declaration marks that file *and its subtree* as test-only. Keying on the
/// module name alone would let any `src/<dir>/tests.rs` inherit the exemption
/// earned by the crate's real `#[cfg(test)] mod tests;`.
fn test_only_paths(src: &Path) -> BTreeSet<PathBuf> {
    let mut test_only = BTreeSet::new();
    let mut seen = BTreeSet::new();
    let mut queue = vec![src.join("lib.rs")];
    while let Some(file) = queue.pop() {
        if !seen.insert(file.clone()) {
            continue;
        }
        let Ok(source) = fs::read_to_string(&file) else {
            continue;
        };
        let dir = module_dir(&file, src);
        for (name, gated) in declared_modules(&source) {
            let Some(child) = resolve_module(&dir, &name) else {
                continue;
            };
            if gated {
                test_only.insert(child);
                test_only.insert(dir.join(&name));
            } else {
                queue.push(child);
            }
        }
    }
    test_only
}

/// The directory a source file's child modules live in.
fn module_dir(file: &Path, src: &Path) -> PathBuf {
    let parent = file.parent().unwrap_or(src).to_path_buf();
    let stem = file
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("");
    if stem == "lib" || stem == "mod" {
        parent
    } else {
        parent.join(stem)
    }
}

fn resolve_module(dir: &Path, name: &str) -> Option<PathBuf> {
    let flat = dir.join(format!("{name}.rs"));
    if flat.is_file() {
        return Some(flat);
    }
    let nested = dir.join(name).join("mod.rs");
    nested.is_file().then_some(nested)
}

/// `(module name, whether it is `#[cfg(test)]`-gated)` for each `mod x;`.
fn declared_modules(source: &str) -> Vec<(String, bool)> {
    let mut modules = Vec::new();
    let mut gated = false;
    for line in source.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("#[cfg(test)]") {
            gated = true;
            continue;
        }
        if trimmed.is_empty() || trimmed.starts_with("//") || trimmed.starts_with("#[") {
            continue;
        }
        if let Some(name) = declared_module(trimmed) {
            modules.push((name, gated));
        }
        gated = false;
    }
    modules
}

/// The module name in a `mod x;` / `pub mod x;` declaration, if any.
fn declared_module(line: &str) -> Option<String> {
    let rest = line
        .strip_prefix("pub(crate) ")
        .or_else(|| line.strip_prefix("pub "))
        .unwrap_or(line)
        .trim_start()
        .strip_prefix("mod ")?;
    let name = rest.trim_end_matches(';').trim();
    (!name.is_empty() && !name.contains(['{', ' '])).then(|| name.to_string())
}

/// Whether `path` is a test-only file or sits under a test-only directory.
fn is_test_only(path: &Path, test_only: &BTreeSet<PathBuf>) -> bool {
    test_only
        .iter()
        .any(|root| path == root || path.starts_with(root))
}

fn collect_rust_sources(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_rust_sources(&path, out);
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("rs") {
            out.push(path);
        }
    }
}

// -- negative controls ---------------------------------------------------
//
// SPEC_0038 phase 2 extends this boundary to a second host crate. Before that
// happens the detector has to be shown to *detect*: every check below plants a
// crate that violates the rule in one specific way and asserts the offender
// line appears. Without these, a parser that quietly returned nothing would
// make the real assertion above pass on any input at all.

/// A throwaway crate tree, removed when the guard drops.
struct PlantedCrate {
    root: PathBuf,
}

impl PlantedCrate {
    /// `manifest` becomes `Cargo.toml`; each `(relative path, contents)` pair
    /// becomes a file under `src/`.
    fn new(name: &str, manifest: &str, sources: &[(&str, &str)]) -> Self {
        let root = std::env::temp_dir().join(format!(
            "rumoca-fmi-me-boundary-{name}-{}-{:?}",
            std::process::id(),
            std::thread::current().id()
        ));
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(root.join("src")).expect("create planted crate");
        fs::write(root.join("Cargo.toml"), manifest).expect("write planted manifest");
        for (relative, contents) in sources {
            let path = root.join("src").join(relative);
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent).expect("create planted module dir");
            }
            fs::write(path, contents).expect("write planted source");
        }
        Self { root }
    }
}

impl Drop for PlantedCrate {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.root);
    }
}

const CLEAN_MANIFEST: &str =
    "[package]\nname = \"planted\"\n\n[dependencies]\nrumoca-solver = { workspace = true }\n";

#[test]
fn negative_control_a_clean_planted_host_reports_nothing() {
    let planted = PlantedCrate::new(
        "clean",
        CLEAN_MANIFEST,
        &[("lib.rs", "pub fn host() -> u32 { 0 }\n")],
    );

    assert!(
        me_boundary_offenders(&planted.root).is_empty(),
        "a host that names nothing private must produce no offenders, or every \
         other control here proves nothing"
    );
}

#[test]
fn negative_control_a_production_source_naming_a_private_runtime_object_is_caught() {
    let planted = PlantedCrate::new(
        "named-surface",
        CLEAN_MANIFEST,
        &[(
            "lib.rs",
            "use rumoca_solver::SolveRuntime;\npub fn host(_: &SolveRuntime) {}\n",
        )],
    );

    let offenders = me_boundary_offenders(&planted.root);
    assert!(
        offenders.iter().any(|line| line.contains("SolveRuntime")),
        "naming a private runtime object from a production path must be \
         reported, got {offenders:?}"
    );
}

#[test]
fn negative_control_a_newly_reexported_private_runtime_object_is_caught() {
    let planted = PlantedCrate::new(
        "discovered-surface",
        CLEAN_MANIFEST,
        &[(
            "lib.rs",
            "use rumoca_solver::ProjectedInitialEventOutcome;\npub fn host(_: &ProjectedInitialEventOutcome) {}\n",
        )],
    );

    let offenders = me_boundary_offenders(&planted.root);
    assert!(
        offenders
            .iter()
            .any(|line| line.contains("ProjectedInitialEventOutcome")),
        "a private surface discovered from rumoca-solver's re-exports must be \
         reported, got {offenders:?}"
    );
}

/// The exemption is earned by `#[cfg(test)]` module-graph position, so a
/// non-test module that merely *looks* like one must not inherit it.
#[test]
fn negative_control_a_module_named_tests_does_not_inherit_the_test_exemption() {
    let planted = PlantedCrate::new(
        "fake-tests",
        CLEAN_MANIFEST,
        &[
            ("lib.rs", "mod tests;\n"),
            ("tests.rs", "use rumoca_solver::SolveStopSchedule;\n"),
        ],
    );

    let offenders = me_boundary_offenders(&planted.root);
    assert!(
        offenders
            .iter()
            .any(|line| line.contains("tests.rs") && line.contains("SolveStopSchedule")),
        "an ungated `mod tests;` is production code and must be scanned, got \
         {offenders:?}"
    );
}

/// The complement of the control above: a genuinely `#[cfg(test)]`-gated
/// module — and its whole subtree — is exempt, because unit-test fixtures do
/// build Solve models by hand.
#[test]
fn negative_control_a_cfg_test_module_subtree_keeps_its_exemption() {
    let planted = PlantedCrate::new(
        "gated-tests",
        CLEAN_MANIFEST,
        &[
            ("lib.rs", "#[cfg(test)]\nmod fixtures;\n"),
            ("fixtures/mod.rs", "mod deep;\n"),
            ("fixtures/deep.rs", "use rumoca_solver::SolveRuntime;\n"),
        ],
    );

    assert!(
        me_boundary_offenders(&planted.root).is_empty(),
        "a `#[cfg(test)]` module and its subtree are not production paths"
    );
}

#[test]
fn negative_control_solve_ir_in_a_plain_dependency_table_is_caught() {
    let planted = PlantedCrate::new(
        "plain-dep",
        "[package]\nname = \"planted\"\n\n[dependencies]\nrumoca-ir-solve = { workspace = true }\n",
        &[("lib.rs", "\n")],
    );

    let offenders = me_boundary_offenders(&planted.root);
    assert!(
        offenders
            .iter()
            .any(|line| line.contains("rumoca-ir-solve") && line.contains("[dependencies]")),
        "Solve IR in `[dependencies]` must be reported, got {offenders:?}"
    );
}

#[test]
fn negative_control_a_renamed_solve_dependency_is_caught_by_package_name() {
    let planted = PlantedCrate::new(
        "renamed-dep",
        "[package]\nname = \"planted\"\n\n[dependencies]\n\
         solve_alias = { package = \"rumoca-ir-solve\", workspace = true }\n\n\
         [build-dependencies.eval_alias]\npackage = \"rumoca-eval-solve\"\nworkspace = true\n",
        &[("lib.rs", "\n")],
    );

    let offenders = me_boundary_offenders(&planted.root);
    assert!(
        offenders
            .iter()
            .any(|line| line.contains("rumoca-ir-solve") && line.contains("[dependencies]")),
        "an inline renamed Solve IR dependency must be reported by its package \
         name, got {offenders:?}"
    );
    assert!(
        offenders
            .iter()
            .any(|line| line.contains("rumoca-eval-solve") && line.contains("[build-dependencies]")),
        "a sub-table renamed Solve evaluator dependency must be reported by its \
         package name, got {offenders:?}"
    );
}

#[test]
fn negative_control_a_workspace_inherited_rename_is_resolved_to_its_package() {
    let workspace = "[workspace]\nmembers = []\n\n[workspace.dependencies]\n\
                     solve_alias = { package = \"rumoca-ir-solve\", path = \"crates/solve\" }\n";
    let member = "[package]\nname = \"planted\"\n\n[dependencies]\n\
                  solve_alias = { workspace = true }\n";

    let dependencies = manifest_dependencies(member, Some(workspace));
    assert!(
        dependencies
            .iter()
            .any(|(kind, package)| kind == "dependencies" && package == "rumoca-ir-solve"),
        "an inherited renamed dependency must resolve through \
         `[workspace.dependencies]`, got {dependencies:?}"
    );
}

/// The two manifest shapes the plain-header scan would miss: a
/// `[<kind>.<name>]` sub-table, and a `[target.'cfg(...)'.<kind>]` table.
#[test]
fn negative_control_solve_ir_hidden_in_a_sub_table_or_target_table_is_caught() {
    let planted = PlantedCrate::new(
        "hidden-dep",
        "[package]\nname = \"planted\"\n\n[dependencies.rumoca-ir-solve]\nworkspace = true\n\n\
         [target.'cfg(target_arch = \"wasm32\")'.build-dependencies]\n\
         rumoca-eval-solve = { workspace = true }\n",
        &[("lib.rs", "\n")],
    );

    let offenders = me_boundary_offenders(&planted.root);
    assert!(
        offenders
            .iter()
            .any(|line| line.contains("rumoca-ir-solve") && line.contains("[dependencies]")),
        "a `[dependencies.<name>]` sub-table must be reported, got {offenders:?}"
    );
    assert!(
        offenders
            .iter()
            .any(|line| line.contains("rumoca-eval-solve") && line.contains("[build-dependencies]")),
        "a `[target.'cfg(...)'.build-dependencies]` table must be reported, got \
         {offenders:?}"
    );
}

#[test]
fn negative_control_solve_ir_in_dev_dependencies_is_allowed() {
    let planted = PlantedCrate::new(
        "dev-dep",
        "[package]\nname = \"planted\"\n\n[dev-dependencies]\nrumoca-ir-solve = { workspace = true }\n",
        &[("lib.rs", "\n")],
    );

    assert!(
        me_boundary_offenders(&planted.root).is_empty(),
        "a dev-dependency is not linked into the library, so it does not \
         breach the boundary"
    );
}
