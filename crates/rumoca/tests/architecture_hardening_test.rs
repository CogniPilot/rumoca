use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("workspace root")
        .to_path_buf()
}

fn collect_rs_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let entries = fs::read_dir(dir).unwrap_or_else(|error| {
        panic!("failed to read {}: {error}", dir.display());
    });
    for entry in entries {
        let entry = entry.expect("directory entry");
        let path = entry.path();
        if path.is_dir() {
            collect_rs_files(&path, out);
            continue;
        }
        if path.extension().is_some_and(|ext| ext == "rs") {
            out.push(path);
        }
    }
}

fn has_direct_ir_symbol_import(line: &str) -> bool {
    [
        "use rumoca_ir_ast::{",
        "use rumoca_ir_flat::{",
        "use rumoca_ir_dae::{",
    ]
    .iter()
    .any(|needle| line.contains(needle))
}

fn collect_direct_import_offenders(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };

    content
        .lines()
        .enumerate()
        .filter(|(_, line)| has_direct_ir_symbol_import(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

fn section_contains_dependency(content: &str, section: &str, dependency: &str) -> bool {
    let header = format!("[{section}]");
    let mut in_section = false;

    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') && trimmed.ends_with(']') {
            in_section = trimmed == header;
            continue;
        }
        if !in_section || trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        if let Some((name, _)) = trimmed.split_once('=')
            && name.trim() == dependency
        {
            return true;
        }
    }

    false
}

fn section_dependency_names(content: &str, section: &str) -> Vec<String> {
    let header = format!("[{section}]");
    let mut in_section = false;
    let mut names = Vec::new();

    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') && trimmed.ends_with(']') {
            in_section = trimmed == header;
            continue;
        }
        if !in_section || trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        if let Some((name, _)) = trimmed.split_once('=') {
            names.push(name.trim().to_string());
        }
    }

    names
}

fn is_cross_crate_pub_type_alias(trimmed: &str) -> bool {
    if !trimmed.starts_with("pub type ") {
        return false;
    }
    let Some((_, rhs)) = trimmed.split_once('=') else {
        return false;
    };
    let rhs = rhs.trim_start();
    rhs.starts_with("rumoca_") || rhs.starts_with("::rumoca_")
}

fn cross_crate_public_export_statement(line: &str) -> Option<&str> {
    let trimmed = line.trim_start();
    if trimmed.starts_with("pub use rumoca_") || is_cross_crate_pub_type_alias(trimmed) {
        Some(trimmed)
    } else {
        None
    }
}

fn normalized_rel_path(rel: &Path) -> String {
    rel.to_string_lossy().replace('\\', "/")
}

fn is_low_level_crate_file(rel: &Path) -> bool {
    let rel = normalized_rel_path(rel);
    rel.starts_with("crates/rumoca-ir-")
        || rel.starts_with("crates/rumoca-phase-")
        || rel.starts_with("crates/rumoca-eval-")
}

fn collect_root_pub_use_statements(content: &str) -> Vec<String> {
    let mut statements = Vec::new();
    let mut lines = content.lines();

    while let Some(line) = lines.next() {
        if !line.starts_with("pub use ") {
            continue;
        }
        let mut statement = line.trim().to_string();
        while !statement.trim_end().ends_with(';') {
            let Some(next_line) = lines.next() else {
                break;
            };
            statement.push(' ');
            statement.push_str(next_line.trim());
        }
        statements.push(statement);
    }

    statements
}

const ALLOWED_CROSS_CRATE_PUBLIC_EXPORTS: &[(&str, &str)] = &[
    (
        "crates/rumoca-ir-ast/src/lib.rs",
        "pub type Causality = rumoca_ir_core::Causality;",
    ),
    (
        "crates/rumoca-ir-ast/src/lib.rs",
        "pub type ClassType = rumoca_ir_core::ClassType;",
    ),
    (
        "crates/rumoca-ir-ast/src/lib.rs",
        "pub type Location = rumoca_ir_core::Location;",
    ),
    (
        "crates/rumoca-ir-ast/src/lib.rs",
        "pub type OpBinary = rumoca_ir_core::OpBinary;",
    ),
    (
        "crates/rumoca-ir-ast/src/lib.rs",
        "pub type OpUnary = rumoca_ir_core::OpUnary;",
    ),
    (
        "crates/rumoca-ir-ast/src/lib.rs",
        "pub type StateSelect = rumoca_ir_core::StateSelect;",
    ),
    (
        "crates/rumoca-ir-ast/src/lib.rs",
        "pub type Token = rumoca_ir_core::Token;",
    ),
    (
        "crates/rumoca-ir-ast/src/lib.rs",
        "pub type Variability = rumoca_ir_core::Variability;",
    ),
    (
        "crates/rumoca-ir-dae/src/types.rs",
        "pub use rumoca_ir_core::BuiltinFunction;",
    ),
    (
        "crates/rumoca-ir-dae/src/types.rs",
        "pub use rumoca_ir_core::DerivativeAnnotation;",
    ),
    (
        "crates/rumoca-ir-dae/src/types.rs",
        "pub use rumoca_ir_core::ExternalFunction;",
    ),
    (
        "crates/rumoca-ir-dae/src/types.rs",
        "pub use rumoca_ir_core::Literal;",
    ),
    (
        "crates/rumoca-ir-dae/src/types.rs",
        "pub use rumoca_ir_core::VarName;",
    ),
    (
        "crates/rumoca-ir-flat/src/lib.rs",
        "pub type BuiltinFunction = rumoca_ir_core::BuiltinFunction;",
    ),
    (
        "crates/rumoca-ir-flat/src/lib.rs",
        "pub type Causality = rumoca_ir_core::Causality;",
    ),
    (
        "crates/rumoca-ir-flat/src/lib.rs",
        "pub type ClassType = rumoca_ir_core::ClassType;",
    ),
    (
        "crates/rumoca-ir-flat/src/lib.rs",
        "pub use rumoca_ir_core::DerivativeAnnotation;",
    ),
    (
        "crates/rumoca-ir-flat/src/lib.rs",
        "pub use rumoca_ir_core::ExternalFunction;",
    ),
    (
        "crates/rumoca-ir-flat/src/lib.rs",
        "pub use rumoca_ir_core::Literal;",
    ),
    (
        "crates/rumoca-ir-flat/src/lib.rs",
        "pub type OpBinary = rumoca_ir_core::OpBinary;",
    ),
    (
        "crates/rumoca-ir-flat/src/lib.rs",
        "pub type OpUnary = rumoca_ir_core::OpUnary;",
    ),
    (
        "crates/rumoca-ir-flat/src/lib.rs",
        "pub type StateSelect = rumoca_ir_core::StateSelect;",
    ),
    (
        "crates/rumoca-ir-flat/src/lib.rs",
        "pub type Token = rumoca_ir_core::Token;",
    ),
    (
        "crates/rumoca-ir-flat/src/lib.rs",
        "pub use rumoca_ir_core::VarName;",
    ),
    (
        "crates/rumoca-ir-flat/src/lib.rs",
        "pub type Variability = rumoca_ir_core::Variability;",
    ),
];

#[test]
fn test_no_tail_rs_files_in_crates() {
    let root = workspace_root().join("crates");
    let mut rs_files = Vec::new();
    collect_rs_files(&root, &mut rs_files);

    let offenders: Vec<String> = rs_files
        .iter()
        .filter_map(|path| {
            let stem = path.file_stem()?.to_string_lossy();
            (stem.contains("_tail")).then(|| path.display().to_string())
        })
        .collect();

    assert!(
        offenders.is_empty(),
        "found banned *_tail*.rs files: {offenders:?}"
    );
}

#[test]
fn test_no_manual_msl_ignore_markers() {
    let root = workspace_root().join("crates").join("rumoca").join("tests");
    let mut rs_files = Vec::new();
    collect_rs_files(&root, &mut rs_files);

    let mut offenders = Vec::new();
    for path in rs_files {
        if path
            .file_name()
            .is_some_and(|name| name == "architecture_hardening_test.rs")
        {
            continue;
        }
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        for (line_idx, line) in content.lines().enumerate() {
            if line.contains("#[ignore") && line.contains("manual-msl-") {
                offenders.push(format!("{}:{}", path.display(), line_idx + 1));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "found legacy manual MSL ignore markers: {offenders:?}"
    );
}

#[test]
fn test_sim_sources_use_ir_namespace_aliases() {
    let root = workspace_root();
    let sim_dirs = [
        root.join("crates/rumoca-sim/src"),
        root.join("crates/rumoca-sim-diffsol/src"),
        root.join("crates/rumoca-sim-rk45/src"),
    ];

    let mut offenders = Vec::new();
    for dir in sim_dirs {
        let mut rs_files = Vec::new();
        collect_rs_files(&dir, &mut rs_files);
        for path in rs_files {
            offenders.extend(collect_direct_import_offenders(&path));
        }
    }

    assert!(
        offenders.is_empty(),
        "found direct IR symbol imports in sim sources; use namespace aliases instead: {offenders:?}"
    );
}

#[test]
fn test_sim_diffsol_dag_boundary_no_flat_or_ast_dependency() {
    for crate_name in ["rumoca-sim", "rumoca-sim-diffsol", "rumoca-sim-rk45"] {
        let cargo_toml = workspace_root().join(format!("crates/{crate_name}/Cargo.toml"));
        let content = fs::read_to_string(&cargo_toml).expect("read sim Cargo.toml");

        assert!(
            !content.contains("rumoca-ir-flat"),
            "{crate_name} must not depend on rumoca-ir-flat"
        );
        assert!(
            !content.contains("rumoca-ir-ast"),
            "{crate_name} must not depend on rumoca-ir-ast"
        );
    }
}

#[test]
fn test_ir_flat_dag_boundary_no_ast_dependency() {
    let cargo_toml = workspace_root().join("crates/rumoca-ir-flat/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read ir-flat Cargo.toml");

    assert!(
        !section_contains_dependency(&content, "dependencies", "rumoca-ir-ast"),
        "ir-flat must not depend on rumoca-ir-ast in [dependencies]; \
AST->Flat conversion belongs in rumoca-phase-flatten"
    );
}

#[test]
fn test_eval_crates_follow_ir_layer_mapping() {
    let root = workspace_root();

    let eval_ast = fs::read_to_string(root.join("crates/rumoca-eval-ast/Cargo.toml"))
        .expect("read eval-ast Cargo.toml");
    assert!(
        section_contains_dependency(&eval_ast, "dependencies", "rumoca-ir-ast"),
        "eval-ast must depend on rumoca-ir-ast"
    );
    assert!(
        section_contains_dependency(&eval_ast, "dependencies", "rumoca-ir-core"),
        "eval-ast must depend on rumoca-ir-core"
    );
    assert!(
        !section_contains_dependency(&eval_ast, "dependencies", "rumoca-ir-flat"),
        "eval-ast must not depend on rumoca-ir-flat"
    );
    assert!(
        !section_contains_dependency(&eval_ast, "dependencies", "rumoca-ir-dae"),
        "eval-ast must not depend on rumoca-ir-dae"
    );
    assert!(
        !section_contains_dependency(&eval_ast, "dependencies", "rumoca-phase-typecheck"),
        "eval-ast must own AST eval logic directly; do not depend on rumoca-phase-typecheck"
    );

    let eval_flat = fs::read_to_string(root.join("crates/rumoca-eval-flat/Cargo.toml"))
        .expect("read eval-flat Cargo.toml");
    assert!(
        section_contains_dependency(&eval_flat, "dependencies", "rumoca-ir-flat"),
        "eval-flat must depend on rumoca-ir-flat"
    );
    assert!(
        section_contains_dependency(&eval_flat, "dependencies", "rumoca-ir-core"),
        "eval-flat must depend on rumoca-ir-core"
    );
    assert!(
        !section_contains_dependency(&eval_flat, "dependencies", "rumoca-ir-ast"),
        "eval-flat must not depend on rumoca-ir-ast"
    );
    assert!(
        !section_contains_dependency(&eval_flat, "dependencies", "rumoca-ir-dae"),
        "eval-flat must not depend on rumoca-ir-dae"
    );

    let eval_dae = fs::read_to_string(root.join("crates/rumoca-eval-dae/Cargo.toml"))
        .expect("read eval-dae Cargo.toml");
    assert!(
        section_contains_dependency(&eval_dae, "dependencies", "rumoca-ir-dae"),
        "eval-dae must depend on rumoca-ir-dae"
    );
    assert!(
        !section_contains_dependency(&eval_dae, "dependencies", "rumoca-ir-ast"),
        "eval-dae must not depend on rumoca-ir-ast"
    );
    assert!(
        !section_contains_dependency(&eval_dae, "dependencies", "rumoca-ir-flat"),
        "eval-dae must not depend on rumoca-ir-flat"
    );
}

#[test]
fn test_bind_wasm_uses_session_lsp_facades() {
    let cargo_toml = workspace_root().join("crates/rumoca-bind-wasm/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read bind-wasm Cargo.toml");

    for required in ["rumoca-session", "rumoca-tool-lsp", "rumoca-tool-lint"] {
        assert!(
            section_contains_dependency(&content, "dependencies", required),
            "rumoca-bind-wasm must depend on {required}"
        );
    }

    for banned in [
        "rumoca-phase-parse",
        "rumoca-phase-codegen",
        "rumoca-ir-ast",
        "rumoca-eval-dae",
        "rumoca-ir-dae",
        "rumoca-ir-core",
    ] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-bind-wasm must not depend on {banned}; \
Author reminder: bind-wasm should use rumoca-session/rumoca-tool-lsp facade APIs."
        );
    }
}

#[test]
fn test_tool_lsp_uses_session_parsing_facade() {
    let cargo_toml = workspace_root().join("crates/rumoca-tool-lsp/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read tool-lsp Cargo.toml");

    for required in ["rumoca-session", "rumoca-tool-fmt", "rumoca-tool-lint"] {
        assert!(
            section_contains_dependency(&content, "dependencies", required),
            "rumoca-tool-lsp must depend on {required}; \
Author reminder: route formatting/lint through tool crates, compile context through session."
        );
    }

    for banned in [
        "rumoca-phase-parse",
        "rumoca-ir-ast",
        "rumoca-ir-core",
        "rumoca-core",
    ] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-tool-lsp must not depend directly on {banned}; \
Author reminder: use rumoca-session facade APIs instead."
        );
    }
}

#[test]
fn test_tool_fmt_uses_session_facade() {
    let cargo_toml = workspace_root().join("crates/rumoca-tool-fmt/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read tool-fmt Cargo.toml");

    assert!(
        section_contains_dependency(&content, "dependencies", "rumoca-session"),
        "rumoca-tool-fmt must depend on rumoca-session"
    );

    let banned = "rumoca-phase-parse";
    assert!(
        !section_contains_dependency(&content, "dependencies", banned),
        "rumoca-tool-fmt must not depend directly on {banned}; \
Author reminder: use rumoca-session parsing/session APIs."
    );
}

#[test]
fn test_tool_lint_uses_session_facade() {
    let cargo_toml = workspace_root().join("crates/rumoca-tool-lint/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read tool-lint Cargo.toml");

    assert!(
        section_contains_dependency(&content, "dependencies", "rumoca-session"),
        "rumoca-tool-lint must depend on rumoca-session"
    );

    let banned = "rumoca-phase-parse";
    assert!(
        !section_contains_dependency(&content, "dependencies", banned),
        "rumoca-tool-lint must not depend directly on {banned}; \
Author reminder: use rumoca-session parsing/session APIs."
    );
}

#[test]
fn test_session_has_no_fmt_or_lint_surface() {
    let session_lib = workspace_root().join("crates/rumoca-session/src/lib.rs");
    let content = fs::read_to_string(&session_lib).expect("read rumoca-session lib.rs");

    for banned in [
        "FormatOptions",
        "FormatError",
        "format_source",
        "LintOptions",
        "LintMessage",
        "lint_source",
    ] {
        assert!(
            !content.contains(banned),
            "rumoca-session must not expose {banned}; \
Author reminder: fmt/lint APIs live in rumoca-tool-fmt and rumoca-tool-lint."
        );
    }
}

#[test]
fn test_tool_dev_uses_session_facade() {
    let cargo_toml = workspace_root().join("crates/rumoca-tool-dev/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read tool-dev Cargo.toml");

    for banned in [
        "rumoca",
        "rumoca-core",
        "rumoca-eval-dae",
        "rumoca-ir-ast",
        "rumoca-ir-flat",
        "rumoca-ir-dae",
        "rumoca-phase-parse",
    ] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-tool-dev must not depend directly on {banned}; \
Author reminder: use rumoca-session facade APIs instead."
        );
    }
}

#[test]
fn test_session_is_compile_only() {
    let cargo_toml = workspace_root().join("crates/rumoca-session/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca-session Cargo.toml");

    assert!(
        !section_contains_dependency(&content, "dependencies", "rumoca-sim"),
        "rumoca-session must not depend on rumoca-sim; runtime contracts belong outside the compile/session facade"
    );
    assert!(
        section_contains_dependency(&content, "dependencies", "rumoca-phase-solve"),
        "rumoca-session must depend on rumoca-phase-solve for structural solve APIs"
    );
    assert!(
        section_contains_dependency(&content, "dependencies", "rumoca-phase-codegen"),
        "rumoca-session must depend on rumoca-phase-codegen for explicit codegen helpers"
    );
    assert!(
        !section_contains_dependency(&content, "dependencies", "rumoca-sim-diffsol"),
        "rumoca-session must not depend on rumoca-sim-diffsol; concrete runtime backends belong outside the compile/session facade"
    );
    assert!(
        !section_contains_dependency(&content, "dependencies", "rumoca-viz-web"),
        "rumoca-session must not depend on rumoca-viz-web; visualization belongs outside the compile/session facade"
    );
    assert!(
        !section_contains_dependency(&content, "dependencies", "diffsol"),
        "rumoca-session must not depend directly on the concrete `diffsol` package; \
Author reminder: keep backend-specific packages below the session facade."
    );

    let banned = "rumoca-eval-dae";
    assert!(
        !section_contains_dependency(&content, "dependencies", banned),
        "rumoca-session must not depend directly on {banned} in [dependencies]; \
Author reminder: keep evaluation/runtime internals out of rumoca-session."
    );
}

#[test]
fn test_session_has_no_direct_eval_dependencies() {
    let cargo_toml = workspace_root().join("crates/rumoca-session/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca-session Cargo.toml");
    let deps = section_dependency_names(&content, "dependencies");

    let direct_eval_deps: Vec<_> = deps
        .into_iter()
        .filter(|name| name.starts_with("rumoca-eval-"))
        .collect();

    assert!(
        direct_eval_deps.is_empty(),
        "rumoca-session must not directly depend on eval crates ({direct_eval_deps:?}); \
Author reminder: session should orchestrate and expose facades, not implement evaluation internals."
    );
}

#[test]
fn test_sim_contract_crate_has_no_backend_dependency() {
    let cargo_toml = workspace_root().join("crates/rumoca-sim/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca-sim Cargo.toml");

    assert!(
        !section_contains_dependency(&content, "dependencies", "diffsol"),
        "rumoca-sim must not depend on the concrete diffsol backend package"
    );
    assert!(
        !section_contains_dependency(&content, "dependencies", "rumoca-phase-codegen"),
        "rumoca-sim must not depend on rumoca-phase-codegen; \
Author reminder: keep codegen/template rendering outside the runtime-contract crate."
    );
    assert!(
        !section_contains_dependency(&content, "dependencies", "rumoca-viz-web"),
        "rumoca-sim must not depend on rumoca-viz-web; \
Author reminder: keep visualization assets outside the runtime-contract crate."
    );
}

#[test]
fn test_sim_diffsol_crate_owns_backend_dependency() {
    let cargo_toml = workspace_root().join("crates/rumoca-sim-diffsol/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca-sim-diffsol Cargo.toml");

    assert!(
        section_contains_dependency(&content, "dependencies", "rumoca-sim"),
        "rumoca-sim-diffsol must depend on rumoca-sim for runtime contracts and shared helpers"
    );
    assert!(
        section_contains_dependency(&content, "dependencies", "diffsol"),
        "rumoca-sim-diffsol must own the concrete diffsol dependency"
    );
    assert!(
        !section_contains_dependency(&content, "dependencies", "rumoca-viz-web"),
        "rumoca-sim-diffsol must not depend on rumoca-viz-web"
    );
}

#[test]
fn test_sim_rk45_crate_owns_second_backend_without_diffsol_dependency() {
    let cargo_toml = workspace_root().join("crates/rumoca-sim-rk45/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca-sim-rk45 Cargo.toml");

    assert!(
        section_contains_dependency(&content, "dependencies", "rumoca-sim"),
        "rumoca-sim-rk45 must depend on rumoca-sim for shared runtime contracts"
    );
    assert!(
        !section_contains_dependency(&content, "dependencies", "diffsol"),
        "rumoca-sim-rk45 must stay pure Rust and must not depend on diffsol"
    );
    assert!(
        !section_contains_dependency(&content, "dependencies", "rumoca-viz-web"),
        "rumoca-sim-rk45 must not depend on rumoca-viz-web"
    );
}

#[test]
fn test_sim_facade_owns_no_visualization_assets() {
    let sim_lib = workspace_root().join("crates/rumoca-sim/src/lib.rs");
    let sim_lib_content = fs::read_to_string(&sim_lib).expect("read rumoca-sim src/lib.rs");
    assert!(
        !sim_lib_content.contains("pub mod results_web;"),
        "rumoca-sim must not expose a results_web module once visualization moves to rumoca-viz-web"
    );

    for removed_path in [
        "crates/rumoca-sim/src/results_web.rs",
        "crates/rumoca-sim/web/results_app.css",
        "crates/rumoca-sim/web/results_app.js",
        "crates/rumoca-sim/web/three.min.js",
        "crates/rumoca-sim/web/visualization_shared.js",
        "crates/rumoca-sim/src/with_diffsol/vendor/uplot.min.css",
        "crates/rumoca-sim/src/with_diffsol/vendor/uplot.min.js",
    ] {
        assert!(
            !workspace_root().join(removed_path).exists(),
            "rumoca-sim must not retain visualization asset path {removed_path}"
        );
    }
}

#[test]
fn test_viz_web_is_isolated_from_session_and_backends() {
    let cargo_toml = workspace_root().join("crates/rumoca-viz-web/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca-viz-web Cargo.toml");

    for banned in [
        "rumoca-session",
        "rumoca-sim",
        "rumoca-sim-report",
        "diffsol",
    ] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-viz-web must not depend on {banned}; \
Author reminder: keep web visualization independent of session, backend, and report-contract ownership."
        );
    }
}

#[test]
fn test_rumoca_entry_uses_session_facade_for_ir() {
    let cargo_toml = workspace_root().join("crates/rumoca/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca Cargo.toml");

    for required in [
        "rumoca-session",
        "rumoca-tool-fmt",
        "rumoca-tool-lint",
        "rumoca-viz-web",
    ] {
        assert!(
            section_contains_dependency(&content, "dependencies", required),
            "rumoca must depend on {required}; \
Author reminder: rumoca CLI delegates fmt/lint via tool crates and compile/runtime via session."
        );
    }

    for banned in ["rumoca-ir-ast", "rumoca-ir-flat", "rumoca-ir-dae"] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca must not depend directly on {banned} in [dependencies]; \
Author reminder: use rumoca-session facade types/APIs instead."
        );
    }
}

#[test]
fn test_test_msl_uses_explicit_runtime_crates() {
    let cargo_toml = workspace_root().join("crates/rumoca-test-msl/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca-test-msl Cargo.toml");

    for required in ["rumoca-session", "rumoca-sim", "rumoca-sim-diffsol"] {
        assert!(
            section_contains_dependency(&content, "dependencies", required),
            "rumoca-test-msl must depend on {required}; \
Author reminder: compile/session and runtime ownership should be explicit."
        );
    }

    for banned in ["rumoca", "rumoca-ir-dae", "rumoca-eval-dae", "rumoca-core"] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-test-msl must not depend directly on {banned} in [dependencies]; \
Author reminder: use rumoca-session::compile plus explicit runtime crates."
        );
    }
    assert!(
        !section_contains_dependency(&content, "dev-dependencies", "rumoca"),
        "rumoca-test-msl must not depend directly on rumoca in [dev-dependencies]; \
Author reminder: use rumoca-session facade APIs."
    );
    assert!(
        !section_contains_dependency(&content, "dev-dependencies", "rumoca-core"),
        "rumoca-test-msl must not depend directly on rumoca-core in [dev-dependencies]; \
Author reminder: use rumoca-session::compile::core facade APIs."
    );
}

#[test]
fn test_no_session_runtime_facade_imports_remain() {
    let mut files = Vec::new();
    collect_rs_files(&workspace_root().join("crates"), &mut files);
    for file in files {
        if normalized_rel_path(
            file.strip_prefix(workspace_root())
                .expect("workspace-relative file"),
        ) == "crates/rumoca/tests/architecture_hardening_test.rs"
        {
            continue;
        }
        let content = fs::read_to_string(&file).expect("read source file");
        let has_runtime_import = content.lines().any(|line| {
            let trimmed = line.trim_start();
            trimmed.starts_with("use rumoca_session::runtime")
                || trimmed.contains(" rumoca_session::runtime::")
        });
        assert!(
            !has_runtime_import,
            "source file {} must not import rumoca_session::runtime; \
Author reminder: use rumoca_session::codegen for template helpers and explicit sim crates for runtime APIs.",
            file.display()
        );
    }
}

#[test]
fn test_contracts_use_session_facade() {
    let cargo_toml = workspace_root().join("crates/rumoca-contracts/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca-contracts Cargo.toml");

    for banned in ["rumoca-phase-parse", "rumoca-eval-dae", "rumoca-ir-ast"] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-contracts must not depend directly on {banned} in [dependencies]; \
Author reminder: use rumoca-session facade APIs instead."
        );
    }
}

#[test]
fn test_ir_dae_no_behavioral_analysis_methods() {
    let path = workspace_root().join("crates/rumoca-ir-dae/src/lib.rs");
    let content = fs::read_to_string(&path).expect("read ir-dae lib.rs");

    for banned in [
        "pub fn is_balanced(&self)",
        "pub fn balance(&self)",
        "pub fn balance_detail(&self)",
        "pub fn runtime_defined_unknown_names(&self)",
        "pub fn runtime_defined_continuous_unknown_names(&self)",
        "fn runtime_assignment_target_names(",
        "fn expression_contains_clocked_or_event_operators(",
        "fn is_connection_origin(",
    ] {
        assert!(
            !content.contains(banned),
            "found behavior in rumoca-ir-dae ({banned}). \
Author reminder: SPEC_0029_CRATE_BOUNDARIES.md §3 requires IR crates to stay data-only; \
move analysis/evaluation helpers to rumoca-eval-dae."
        );
    }
}

#[test]
fn test_no_new_cross_crate_public_exports() {
    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);

    let mut actual = BTreeSet::new();
    for path in rs_files {
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        let rel = path.strip_prefix(&root).unwrap_or(&path);
        if !is_low_level_crate_file(rel) {
            continue;
        }
        let rel_display = normalized_rel_path(rel);
        for line in content.lines() {
            if let Some(statement) = cross_crate_public_export_statement(line) {
                actual.insert(format!("{rel_display}|{statement}"));
            }
        }
    }

    let allowed: BTreeSet<String> = ALLOWED_CROSS_CRATE_PUBLIC_EXPORTS
        .iter()
        .map(|(path, statement)| format!("{path}|{statement}"))
        .collect();

    let unexpected: Vec<String> = actual.difference(&allowed).cloned().collect();
    let stale_allowlist: Vec<String> = allowed.difference(&actual).cloned().collect();

    assert!(
        unexpected.is_empty(),
        "found disallowed cross-crate public aliases. \
Author reminder: SPEC_0029_CRATE_BOUNDARIES.md §8 forbids this in low-level crates \
(ir/phase/eval). \
Do not expose another Rumoca crate's symbols via `pub use rumoca_*::...` or \
`pub type X = rumoca_*::...`; import from the owning crate directly. \
Violations: {unexpected:#?}"
    );
    assert!(
        stale_allowlist.is_empty(),
        "stale cross-crate re-export allowlist entries; remove these from \
ALLOWED_CROSS_CRATE_PUBLIC_EXPORTS: {stale_allowlist:#?}"
    );
}

#[test]
fn test_session_root_facade_exports_are_minimal() {
    let session_lib = workspace_root().join("crates/rumoca-session/src/lib.rs");
    let content = fs::read_to_string(&session_lib).expect("read rumoca-session lib.rs");
    let root_pub_uses = collect_root_pub_use_statements(&content);

    let expected = vec!["pub use compile::{Session, SessionConfig};".to_string()];

    assert_eq!(
        root_pub_uses, expected,
        "unexpected rumoca-session root exports. \
Author reminder: SPEC_0029_CRATE_BOUNDARIES.md §9 requires `rumoca-session` \
root exports to stay minimal (`Session`, `SessionConfig`) and to keep other APIs namespaced."
    );
}
