//! SPEC_0038 §Internal Solver Boundary enforcement for the rk-like host.

use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

use super::architecture_hardening_support::workspace_root;

/// Private runtime objects `rumoca-solver` still re-exports at its root.
/// A host that names one has stepped around the ME contract.
const PRIVATE_RUNTIME_SURFACES: [&str; 20] = [
    "SolveRuntime",
    "SolveStopSchedule",
    "RuntimeEventStop",
    "RuntimeEventBoundary",
    "RuntimeEventBoundaryHandler",
    "process_runtime_event_boundary",
    "runtime_event_horizon",
    "EventPreMode",
    "EventActionOutcome",
    "EventUpdateRowFilter",
    "RootCrossing",
    "ProjectedEventUpdateInput",
    "ProjectedInitialEventInput",
    "InitialEventObservation",
    "RuntimeSolveError",
    "commit_pre_params_after_event_at",
    "clear_scheduled_root_relation_memory",
    "root_crossings_with_relation_memory",
    "filter_scheduled_root_crossings",
    "convert_variable_meta",
];

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
    let manifest = fs::read_to_string(crate_root.join("Cargo.toml"))
        .expect("read rumoca-solver-rk45 Cargo.toml");

    let mut offenders = Vec::new();

    // (1) Every dependency table except dev-dependencies, in every form:
    // `[dependencies]`, `[build-dependencies]`, `[dependencies.<name>]`, and
    // any `[target.'cfg(...)'.<kind>]` variant of those.
    for (table, name) in manifest_dependencies(&manifest) {
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
    let mut sources = Vec::new();
    collect_rust_sources(&src, &mut sources);
    for path in sources {
        if is_test_only(&path, &test_only) {
            continue;
        }
        let source = fs::read_to_string(&path).expect("read rk45 source");
        for surface in PRIVATE_RUNTIME_SURFACES {
            if source.contains(surface) {
                offenders.push(format!(
                    "{}: names `{surface}`",
                    path.strip_prefix(&crate_root).unwrap_or(&path).display()
                ));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "SPEC_0038: the rk-like host must reach the model only through \
rumoca_solver::fmi_me; these production paths step around it instead:\n  {}",
        offenders.join("\n  "),
    );
}

/// Every `(table-kind, dependency-name)` pair a Cargo manifest declares.
///
/// Handles the plain tables, the `[<kind>.<name>]` sub-table form, and any
/// `[target.'cfg(...)'.<kind>]` prefix. The table kind is normalized to the
/// bare `dependencies` / `dev-dependencies` / `build-dependencies` segment.
fn manifest_dependencies(manifest: &str) -> Vec<(String, String)> {
    let mut found = Vec::new();
    let mut table: Option<(String, Option<String>)> = None;
    for line in manifest.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') && trimmed.ends_with(']') {
            let header = &trimmed[1..trimmed.len() - 1];
            table = dependency_table(&split_toml_header(header));
            if let Some((kind, Some(name))) = &table {
                found.push((kind.clone(), name.clone()));
            }
            continue;
        }
        let Some((kind, None)) = &table else {
            continue;
        };
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        if let Some((name, _)) = trimmed.split_once('=') {
            found.push((kind.clone(), name.trim().trim_matches('"').to_string()));
        }
    }
    found
}

/// Split a TOML table header on `.`, honoring quoted segments such as the
/// `'cfg(target_arch = "wasm32")'` in a `[target....]` header.
fn split_toml_header(header: &str) -> Vec<String> {
    let mut segments = Vec::new();
    let mut current = String::new();
    let mut quote: Option<char> = None;
    for ch in header.chars() {
        match quote {
            Some(open) => {
                current.push(ch);
                if ch == open {
                    quote = None;
                }
            }
            None if ch == '\'' || ch == '"' => {
                quote = Some(ch);
                current.push(ch);
            }
            None if ch == '.' => segments.push(std::mem::take(&mut current)),
            None => current.push(ch),
        }
    }
    segments.push(current);
    segments
}

/// `(kind, Some(name))` for a `[<kind>.<name>]` sub-table, `(kind, None)` for
/// a plain dependency table, `None` when the header is not a dependency table.
fn dependency_table(segments: &[String]) -> Option<(String, Option<String>)> {
    let index = segments.iter().position(|segment| {
        matches!(
            segment.trim(),
            "dependencies" | "dev-dependencies" | "build-dependencies"
        )
    })?;
    let kind = segments[index].trim().to_string();
    let name = segments
        .get(index + 1)
        .map(|segment| segment.trim().trim_matches('"').to_string());
    Some((kind, name))
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
