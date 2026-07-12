use std::collections::BTreeSet;
use std::fs;
use std::path::Path;

mod architecture_hardening_support;
use architecture_hardening_support::*;

const ALLOWED_CROSS_CRATE_PUBLIC_EXPORTS: &[(&str, &str)] = &[];

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
fn test_no_ignored_tests() {
    let root = workspace_root().join("crates");
    let mut rs_files = Vec::new();
    collect_rs_files(&root, &mut rs_files);

    let mut offenders = Vec::new();
    let ignore_attr = concat!("#[", "ignore");
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
            if line.contains(ignore_attr) {
                offenders.push(format!("{}:{}", path.display(), line_idx + 1));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "found ignored tests; use Cargo features/filters for heavy suites or remove stale tests: {offenders:?}"
    );
}

#[test]
fn test_no_direct_dot_tokenization_for_model_paths() {
    assert_no_direct_dot_tokenization_for_model_paths();
}

#[test]
fn test_builtin_codegen_has_no_kelvin_or_boptest_adapters() {
    let root = workspace_root();
    let codegen_root = root.join("crates/rumoca-phase-codegen/src");
    let mut rs_files = Vec::new();
    collect_rs_files(&codegen_root, &mut rs_files);

    let banned_terms = ["boptest", "top_down", "top-down", "kelvin"];
    let mut offenders = Vec::new();

    for path in rs_files {
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        let lower = content.to_ascii_lowercase();
        for term in banned_terms {
            if lower.contains(term) {
                offenders.push(format!("{} contains {term}", path.display()));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "Rumoca built-in codegen must stay project-neutral; Kelvin/BOPTEST adapters belong in Kelvin: {offenders:?}"
    );
}

#[test]
fn test_semantic_code_does_not_add_textual_model_path_recovery() {
    assert_semantic_code_does_not_add_textual_model_path_recovery();
}

#[test]
fn test_sim_sources_use_ir_namespace_aliases() {
    let root = workspace_root();
    let sim_dirs = [
        root.join("crates/rumoca-solver/src"),
        root.join("crates/rumoca-solver-diffsol/src"),
        root.join("crates/rumoca-solver-rk45/src"),
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
fn test_solver_diffsol_dag_boundary_no_flat_or_ast_dependency() {
    for crate_name in [
        "rumoca-solver",
        "rumoca-solver-diffsol",
        "rumoca-solver-rk45",
    ] {
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
fn test_concrete_solver_backends_consume_solve_ir_only() {
    let root = workspace_root();
    let offenders = ["rumoca-solver-diffsol", "rumoca-solver-rk45"]
        .iter()
        .flat_map(|crate_name| solver_backend_boundary_offenders(&root, crate_name))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "concrete solver backends must remain thin Solve-IR consumers; \
Modelica semantics belong in DAE/Solve lowering or shared eval-solve contracts: {offenders:?}"
    );
}

fn solver_backend_boundary_offenders(root: &Path, crate_name: &str) -> Vec<String> {
    let cargo_toml = root.join(format!("crates/{crate_name}/Cargo.toml"));
    let content = fs::read_to_string(&cargo_toml).expect("read solver backend Cargo.toml");
    ["dependencies", "dev-dependencies"]
        .iter()
        .flat_map(|section| {
            let section = *section;
            section_dependency_names(&content, section)
                .into_iter()
                .filter(|dep| solver_backend_dep_is_banned(dep))
                .map(move |dep| format!("{crate_name} [{section}] {dep}"))
        })
        .collect()
}

fn solver_backend_dep_is_banned(dep: &str) -> bool {
    const BANNED_EXACT: &[&str] = &[
        "rumoca-compile",
        "rumoca-ir-ast",
        "rumoca-ir-flat",
        "rumoca-ir-dae",
        "rumoca-eval-ast",
        "rumoca-eval-flat",
        "rumoca-eval-dae",
        "rumoca-sim",
    ];

    dep.starts_with("rumoca-phase-")
        || dep.starts_with("rumoca-exec-")
        || dep == "rumoca-phase-codegen"
        || BANNED_EXACT.contains(&dep)
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
fn test_cli_uses_facades_not_phase_crates() {
    let cargo_toml = workspace_root().join("crates/rumoca/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read CLI Cargo.toml");

    for phase_dep in ["rumoca-phase-dae", "rumoca-phase-solve"] {
        assert!(
            !section_contains_dependency(&content, "dependencies", phase_dep),
            "CLI production dependencies must go through rumoca-compile/rumoca-sim facades; \
found direct dependency on {phase_dep}"
        );
    }
}

#[test]
fn test_single_source_dae_expression_helpers_are_not_reimplemented() {
    let root = workspace_root();
    let owner = root.join("crates/rumoca-ir-dae/src/expr_query.rs");
    let self_file = root.join("crates/rumoca/tests/architecture_hardening_test.rs");
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);

    let helper_names = [
        "expr_contains_var",
        "expr_refers_to_var",
        "expr_contains_der_of",
    ];
    let mut offenders = Vec::new();
    for path in rs_files {
        if path == owner || path == self_file {
            continue;
        }
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        offenders.extend(file_helper_definition_offenders(
            &path,
            &content,
            &helper_names,
        ));
    }

    assert!(
        offenders.is_empty(),
        "SPEC_0029 single-source DAE expression helpers must only be implemented \
in rumoca-ir-dae::expr_query: {offenders:?}"
    );
}

fn file_helper_definition_offenders(
    path: &Path,
    content: &str,
    helper_names: &[&str],
) -> Vec<String> {
    content
        .lines()
        .enumerate()
        .filter(|(_, line)| {
            helper_names
                .iter()
                .any(|helper| is_helper_definition(line.trim_start(), helper))
        })
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

fn is_helper_definition(trimmed: &str, helper: &str) -> bool {
    trimmed.starts_with(&format!("fn {helper}("))
        || trimmed.starts_with(&format!("pub fn {helper}("))
        || trimmed.starts_with(&format!("pub(crate) fn {helper}("))
}

fn semantic_enum_body<'a>(content: &'a str, enum_name: &str, next_impl: &str) -> &'a str {
    let enum_start = content
        .find(&format!("pub enum {enum_name} {{"))
        .unwrap_or_else(|| panic!("semantic {enum_name} enum"));
    let impl_start = content[enum_start..]
        .find(next_impl)
        .map(|offset| enum_start + offset)
        .unwrap_or_else(|| panic!("{enum_name} impl"));
    &content[enum_start..impl_start]
}

#[test]
fn test_semantic_ir_has_direct_spans_not_spanned_wrappers() {
    let path = workspace_root().join("crates/rumoca-core/src/ir_primitives.rs");
    let content = fs::read_to_string(&path).expect("read core IR primitives");
    let statement_path = workspace_root()
        .join("crates/rumoca-core/src/ir_primitives/component_refs_and_functions.rs");
    let statement_content =
        fs::read_to_string(&statement_path).expect("read core component IR primitives");
    let expression_enum = semantic_enum_body(&content, "Expression", "impl Expression");
    let statement_enum = semantic_enum_body(&statement_content, "Statement", "impl Statement");

    assert!(
        !expression_enum.contains("Spanned"),
        "semantic Expression must carry spans directly on variants, not through a Spanned wrapper"
    );
    assert!(
        !statement_enum.contains("Spanned"),
        "semantic Statement must carry spans directly on variants, not through a Spanned wrapper"
    );
}

#[test]
fn test_semantic_operator_enums_do_not_store_tokens() {
    assert_eq!(
        std::mem::size_of::<rumoca_core::OpBinary>(),
        1,
        "semantic binary operators must stay token-free; source spans belong on expressions, not operator payloads"
    );
    assert_eq!(
        std::mem::size_of::<rumoca_core::OpUnary>(),
        1,
        "semantic unary operators must stay token-free; source spans belong on expressions, not operator payloads"
    );
}

#[test]
fn test_var_name_uses_interned_identity_not_string_newtype() {
    let path = workspace_root().join("crates/rumoca-core/src/ir_primitives/var_name.rs");
    let content = fs::read_to_string(&path)
        .expect("read core IR primitives")
        .replace("\r\n", "\n");

    assert!(
        content.contains("pub struct VarNameId(pub u32);"),
        "VarName must expose the compact interned identity required by SPEC_0029"
    );
    assert!(
        content
            .contains("pub struct VarName {\n    id: VarNameId,\n    data: Arc<VarNameData>,\n}"),
        "VarName must store interned identity plus one shared payload, not owned path strings"
    );
    assert!(
        content.contains("top_level_dots: Box<[u32]>"),
        "VarName segmentation must be precomputed at intern time (SPEC_0029): \
semantic code reads boundaries, it does not parse rendered paths"
    );
    assert!(
        !content.contains("pub struct VarName(String)"),
        "VarName must not regress to hashing owned flattened path strings"
    );
}

#[test]
fn test_generated_parser_contract_is_pinned_and_documented() {
    let root = workspace_root();
    let workspace_toml =
        fs::read_to_string(root.join("Cargo.toml")).expect("read workspace Cargo.toml");

    assert!(
        section_dependency_line(&workspace_toml, "workspace.dependencies", "parol")
            .is_some_and(|line| line == r#"parol = "=4.2.2""#),
        "workspace parol dependency must be pinned exactly so generated parser output is reproducible"
    );
    assert!(
        section_dependency_line(&workspace_toml, "workspace.dependencies", "parol_runtime")
            .is_some_and(|line| line == r#"parol_runtime = "=4.2.0""#),
        "workspace parol_runtime dependency must be pinned exactly with the generated parser"
    );

    let build_rs =
        fs::read_to_string(root.join("crates/rumoca-phase-parse/build.rs")).expect("read build.rs");
    for required in [
        r#"Builder::with_explicit_output_dir("src/generated")"#,
        r#".grammar_file(par_file)"#,
        r#".parser_output_file("modelica_parser.rs")"#,
        r#".actions_output_file("modelica_grammar_trait.rs")"#,
        r#"println!("cargo:rerun-if-changed=src/modelica.par");"#,
    ] {
        assert!(
            build_rs.contains(required),
            "parser build script must keep the generated parser contract stable; missing `{required}`"
        );
    }

    let contributing =
        fs::read_to_string(root.join("CONTRIBUTING.md")).expect("read CONTRIBUTING.md");
    for required in [
        "## Parser Grammar Regeneration",
        "cargo check -p rumoca-phase-parse",
        "cargo test -p rumoca-phase-parse --test recovery_corpus --quiet",
        "git diff -- crates/rumoca-phase-parse/src/generated",
    ] {
        assert!(
            contributing.contains(required),
            "CONTRIBUTING.md must document parser regeneration; missing `{required}`"
        );
    }

    let ci = fs::read_to_string(root.join(".github/workflows/ci.yml")).expect("read CI workflow");
    assert!(
        ci.contains("git diff --exit-code -- crates/rumoca-phase-parse/src/generated"),
        "CI lint gate must fail when the checked-in generated parser is stale"
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
        section_contains_dependency(&eval_ast, "dependencies", "rumoca-core"),
        "eval-ast must depend on rumoca-core for foundation primitives (SPEC_0029 §3a)"
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
        section_contains_dependency(&eval_flat, "dependencies", "rumoca-core"),
        "eval-flat must depend on rumoca-core for foundation primitives (SPEC_0029 §3a)"
    );
    assert!(
        !section_contains_dependency(&eval_flat, "dependencies", "rumoca-ir-ast"),
        "eval-flat must not depend on rumoca-ir-ast"
    );
    assert!(
        !section_contains_dependency(&eval_flat, "dependencies", "rumoca-ir-dae"),
        "eval-flat must not depend on rumoca-ir-dae"
    );
}

#[test]
fn test_bind_wasm_keeps_simulation_optional() {
    let wasm_toml = workspace_root().join("crates/rumoca-bind-wasm/Cargo.toml");
    let content = fs::read_to_string(&wasm_toml).expect("read bind-wasm Cargo.toml");

    for required in ["rumoca-compile", "rumoca-tool-lsp", "rumoca-tool-lint"] {
        assert!(
            section_contains_dependency(&content, "dependencies", required),
            "rumoca-bind-wasm must depend on {required}"
        );
    }

    for banned in [
        "rumoca-phase-parse",
        "rumoca-phase-codegen",
        "rumoca-ir-ast",
        "rumoca-phase-solve",
        "rumoca-ir-dae",
    ] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-bind-wasm must not depend on {banned}; \
Author reminder: bind-wasm should route through session/tool crates."
        );
    }

    for feature in ["sim-diffsol", "sim-rk45", "session-diffsol", "full-web"] {
        assert!(
            section_contains_dependency(&content, "features", feature),
            "rumoca-bind-wasm must expose a `{feature}` feature so optional runtime surfaces are explicit"
        );
    }

    // The scheduled simulation facade (rumoca-sim) is the single optional surface; it
    // re-exports sim-core + solver crates behind its own feature flags.
    let optional_dep = "rumoca-sim";
    let line =
        section_dependency_line(&content, "dependencies", optional_dep).unwrap_or_else(|| {
            panic!("rumoca-bind-wasm must declare dependency line for {optional_dep}")
        });
    assert!(
        line.contains("optional = true"),
        "rumoca-bind-wasm dependency `{optional_dep}` must be optional; found `{line}`"
    );
}

#[test]
fn test_bind_wasm_full_web_uses_browser_safe_solver_graph() {
    let wasm_toml = workspace_root().join("crates/rumoca-bind-wasm/Cargo.toml");
    let content = fs::read_to_string(&wasm_toml).expect("read bind-wasm Cargo.toml");
    let line = section_dependency_line(&content, "features", "full-web")
        .expect("rumoca-bind-wasm must declare a full-web feature");

    assert!(
        line.contains("\"sim-rk45\""),
        "full-web must include the browser-safe RK45 backend; found `{line}`"
    );
    assert!(
        !line.contains("\"sim-diffsol\"") && !line.contains("\"session-diffsol\""),
        "full-web must not pull the Diffsol/Pulp relaxed-SIMD graph into \
browser smoke builds; found `{line}`"
    );
}

#[test]
fn test_tool_lsp_uses_session_parsing_facade() {
    let cargo_toml = workspace_root().join("crates/rumoca-tool-lsp/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read tool-lsp Cargo.toml");

    for required in ["rumoca-compile", "rumoca-tool-fmt", "rumoca-tool-lint"] {
        assert!(
            section_contains_dependency(&content, "dependencies", required),
            "rumoca-tool-lsp must depend on {required}; \
Author reminder: route formatting/lint through tool crates, compile context through session."
        );
    }

    for banned in ["rumoca-phase-parse", "rumoca-ir-ast", "rumoca-core"] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-tool-lsp must not depend directly on {banned}; \
Author reminder: use rumoca-compile facade APIs instead."
        );
    }
}

#[test]
fn test_tool_fmt_uses_session_facade() {
    let cargo_toml = workspace_root().join("crates/rumoca-tool-fmt/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read tool-fmt Cargo.toml");

    assert!(
        section_contains_dependency(&content, "dependencies", "rumoca-compile"),
        "rumoca-tool-fmt must depend on rumoca-compile"
    );

    let banned = "rumoca-phase-parse";
    assert!(
        !section_contains_dependency(&content, "dependencies", banned),
        "rumoca-tool-fmt must not depend directly on {banned}; \
Author reminder: use rumoca-compile parsing/session APIs."
    );
}

#[test]
fn test_tool_lint_uses_session_facade() {
    let cargo_toml = workspace_root().join("crates/rumoca-tool-lint/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read tool-lint Cargo.toml");

    assert!(
        section_contains_dependency(&content, "dependencies", "rumoca-compile"),
        "rumoca-tool-lint must depend on rumoca-compile"
    );

    let banned = "rumoca-phase-parse";
    assert!(
        !section_contains_dependency(&content, "dependencies", banned),
        "rumoca-tool-lint must not depend directly on {banned}; \
Author reminder: use rumoca-compile parsing/session APIs."
    );
}

#[test]
fn test_session_has_no_fmt_or_lint_surface() {
    let session_lib = workspace_root().join("crates/rumoca-compile/src/lib.rs");
    let content = fs::read_to_string(&session_lib).expect("read rumoca-compile lib.rs");

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
            "rumoca-compile must not expose {banned}; \
Author reminder: fmt/lint APIs live in rumoca-tool-fmt and rumoca-tool-lint."
        );
    }
}

#[test]
fn test_tool_dev_uses_session_facade() {
    let cargo_toml = workspace_root().join("crates/xtask/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read tool-dev Cargo.toml");

    for banned in [
        "rumoca",
        "rumoca-core",
        "rumoca-phase-solve",
        "rumoca-ir-ast",
        "rumoca-ir-flat",
        "rumoca-ir-dae",
        "rumoca-phase-parse",
    ] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "xtask must not depend directly on {banned}; \
Author reminder: use rumoca-compile facade APIs instead."
        );
    }
}

#[test]
fn test_session_is_compile_only() {
    let cargo_toml = workspace_root().join("crates/rumoca-compile/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca-compile Cargo.toml");

    for banned in [
        "rumoca-solver",
        "rumoca-codec-flatbuffers",
        "rumoca-input",
        "rumoca-input-gamepad",
        "rumoca-input-keyboard",
        "rumoca-transport-udp",
        "rumoca-transport-websocket",
    ] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-compile must not depend on {banned}; runtime/app contracts belong outside the compile/session facade"
        );
    }
    assert!(
        section_contains_dependency(&content, "dependencies", "rumoca-phase-structural"),
        "rumoca-compile must depend on rumoca-phase-structural for structural solve APIs"
    );
    assert!(
        section_contains_dependency(&content, "dependencies", "rumoca-phase-codegen"),
        "rumoca-compile must depend on rumoca-phase-codegen for explicit codegen helpers"
    );
    assert!(
        !section_contains_dependency(&content, "dependencies", "rumoca-solver-diffsol"),
        "rumoca-compile must not depend on rumoca-solver-diffsol; concrete runtime backends belong outside the compile/session facade"
    );
    assert!(
        !section_contains_dependency(&content, "dependencies", "rumoca-web"),
        "rumoca-compile must not depend on rumoca-web; visualization belongs outside the compile/session facade"
    );
    assert!(
        !section_contains_dependency(&content, "dependencies", "diffsol"),
        "rumoca-compile must not depend directly on the concrete `diffsol` package; \
Author reminder: keep backend-specific packages below the session facade."
    );

    let banned = "rumoca-phase-solve";
    assert!(
        !section_contains_dependency(&content, "dependencies", banned),
        "rumoca-compile must not depend directly on {banned} in [dependencies]; \
Author reminder: keep evaluation/runtime internals out of rumoca-compile."
    );
}

#[test]
fn test_session_has_no_direct_eval_dependencies() {
    let cargo_toml = workspace_root().join("crates/rumoca-compile/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca-compile Cargo.toml");
    let deps = section_dependency_names(&content, "dependencies");

    let direct_eval_deps: Vec<_> = deps
        .into_iter()
        .filter(|name| name.starts_with("rumoca-eval-"))
        .collect();

    assert!(
        direct_eval_deps.is_empty(),
        "rumoca-compile must not directly depend on eval crates ({direct_eval_deps:?}); \
Author reminder: session should orchestrate and expose facades, not implement evaluation internals."
    );
}

#[test]
fn test_sim_contract_crate_has_no_backend_dependency() {
    let cargo_toml = workspace_root().join("crates/rumoca-solver/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca-solver Cargo.toml");

    assert!(
        !section_contains_dependency(&content, "dependencies", "diffsol"),
        "rumoca-solver must not depend on the concrete diffsol backend package"
    );
    assert!(
        !section_contains_dependency(&content, "dependencies", "rumoca-phase-codegen"),
        "rumoca-solver must not depend on rumoca-phase-codegen; \
Author reminder: keep codegen/template rendering outside the runtime-contract crate."
    );
    assert!(
        !section_contains_dependency(&content, "dependencies", "rumoca-web"),
        "rumoca-solver must not depend on rumoca-web; \
Author reminder: keep visualization assets outside the runtime-contract crate."
    );
    assert!(
        !section_contains_dependency(&content, "dependencies", "rumoca-solver"),
        "rumoca-solver owns backend-neutral solver contracts directly; do not reintroduce a tiny rumoca-solver interface crate"
    );
    assert!(
        workspace_root()
            .join("crates/rumoca-solver/src/solver.rs")
            .exists(),
        "rumoca-solver must expose backend-neutral solver contracts from src/solver.rs"
    );
    assert!(
        section_contains_dependency(&content, "dependencies", "rumoca-ir-solve"),
        "rumoca-solver must depend on rumoca-ir-solve for solver-facing prepared layout data"
    );
    for banned in [
        "rumoca-ir-dae",
        "rumoca-eval-dae",
        "rumoca-eval-solve",
        "rumoca-phase-dae",
        "rumoca-phase-structural",
        "rumoca-phase-solve",
    ] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-solver must not depend on {banned}; DAE/eval/phase preparation belongs upstream of the runtime-contract crate"
        );
    }
    assert!(
        !workspace_root()
            .join("crates/rumoca-solver/src/with_diffsol")
            .exists(),
        "rumoca-solver must not retain a with_diffsol backend tree; diffsol implementation belongs in rumoca-solver-diffsol"
    );
}

#[test]
fn test_sim_core_does_not_reexport_ir_or_phase_crates() {
    let lib_rs = workspace_root().join("crates/rumoca-solver/src/lib.rs");
    let content = fs::read_to_string(&lib_rs).expect("read rumoca-solver lib.rs");

    for banned in [
        "pub mod ir_dae",
        "pub mod ir_core",
        "pub mod core",
        "pub mod phase_structural",
        "pub mod phase_solve_lower",
        "pub mod analysis_dae",
        "pub use rumoca_ir_",
        "pub use rumoca_core",
        "pub use rumoca_phase_",
        "pub use rumoca_analysis_",
    ] {
        assert!(
            !content.contains(banned),
            "rumoca-solver must not expose cross-crate facade `{banned}`; \
DAE-aware preparation should live in the owning phase crate, and concrete solver crates should consume solve-IR"
        );
    }
}

#[test]
fn test_solver_crates_do_not_use_sim_core_facade_modules() {
    let root = workspace_root();
    let solver_dirs = [
        root.join("crates/rumoca-solver-diffsol/src"),
        root.join("crates/rumoca-solver-rk45/src"),
    ];
    let banned = [
        "rumoca_solver::ir_dae",
        "rumoca_solver::ir_core",
        "rumoca_solver::core",
        "rumoca_solver::phase_structural",
        "rumoca_solver::phase_solve_lower",
        "rumoca_solver::analysis_dae",
        "rumoca_solver::{ir_dae",
        "rumoca_solver::{ir_core",
        "rumoca_solver::{core",
        "rumoca_solver::{phase_structural",
        "rumoca_solver::{phase_solve_lower",
        "rumoca_solver::{analysis_dae",
    ];

    let mut offenders = Vec::new();
    for dir in solver_dirs {
        let mut rs_files = Vec::new();
        collect_rs_files(&dir, &mut rs_files);
        for path in rs_files {
            let Ok(content) = fs::read_to_string(&path) else {
                continue;
            };
            offenders.extend(find_banned_source_lines(&path, &content, &banned, "uses"));
        }
    }

    assert!(
        offenders.is_empty(),
        "solver crates must not tunnel IR or phase access through rumoca-solver: {offenders:?}"
    );
}

#[test]
fn test_non_codegen_phase_crates_do_not_own_target_encoder_dependencies() {
    let root = workspace_root();
    let banned_deps = ["wasm-encoder", "inkwell", "cranelift-codegen"];
    let banned_source_tokens = ["wasm_encoder", "inkwell", "cranelift"];
    let mut offenders = Vec::new();

    for entry in fs::read_dir(root.join("crates")).expect("read crates dir") {
        let path = entry.expect("crate entry").path();
        let Some(crate_name) = path.file_name().and_then(|name| name.to_str()) else {
            continue;
        };
        if !crate_name.starts_with("rumoca-phase-") || crate_name == "rumoca-phase-codegen" {
            continue;
        }

        let cargo_toml = path.join("Cargo.toml");
        let cargo_content = fs::read_to_string(&cargo_toml).expect("read phase Cargo.toml");
        for banned in banned_deps {
            if section_contains_dependency(&cargo_content, "dependencies", banned)
                || section_contains_dependency(&cargo_content, "dev-dependencies", banned)
                || section_contains_dependency(&cargo_content, "build-dependencies", banned)
            {
                offenders.push(format!("{} depends on {banned}", cargo_toml.display()));
            }
        }

        let mut rs_files = Vec::new();
        collect_rs_files(&path.join("src"), &mut rs_files);
        for source_path in rs_files {
            let Ok(content) = fs::read_to_string(&source_path) else {
                continue;
            };
            offenders.extend(find_banned_source_lines(
                &source_path,
                &content,
                &banned_source_tokens,
                "mentions target encoder",
            ));
        }
    }

    assert!(
        offenders.is_empty(),
        "non-codegen phase crates must not own target encoder dependencies or imports; \
target-specific execution/emission belongs in codegen or rumoca-exec-* crates: {offenders:#?}"
    );
}

#[test]
fn test_solver_diffsol_crate_owns_backend_dependency() {
    let cargo_toml = workspace_root().join("crates/rumoca-solver-diffsol/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca-solver-diffsol Cargo.toml");

    assert!(
        section_contains_dependency(&content, "dependencies", "rumoca-solver"),
        "rumoca-solver-diffsol must depend on rumoca-solver for shared runtime helpers"
    );
    assert!(
        !section_contains_dependency(&content, "dependencies", "rumoca-sim"),
        "rumoca-solver-diffsol must not depend on the rumoca-sim facade"
    );
    assert!(
        section_contains_dependency(&content, "dependencies", "diffsol"),
        "rumoca-solver-diffsol must own the concrete diffsol dependency"
    );
    assert!(
        section_contains_dependency(&content, "dependencies", "rumoca-ir-solve"),
        "rumoca-solver-diffsol must consume solver-facing IR"
    );
    for banned in [
        "rumoca-ir-dae",
        "rumoca-core",
        "rumoca-phase-structural",
        "rumoca-phase-solve",
    ] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-solver-diffsol must not depend on {banned}; \
DAE-to-Solve lowering belong upstream in rumoca-phase-solve"
        );
    }
    assert!(
        !section_contains_dependency(&content, "dependencies", "rumoca-web"),
        "rumoca-solver-diffsol must not depend on rumoca-web"
    );
}

#[test]
fn test_concrete_solver_sources_use_solve_ir_only() {
    let root = workspace_root();
    let banned = [
        "rumoca_ir_dae",
        "rumoca_core",
        "rumoca_phase_structural",
        "rumoca_phase_solve",
    ];
    let mut offenders = Vec::new();
    for src in [
        root.join("crates/rumoca-solver-diffsol/src"),
        root.join("crates/rumoca-solver-rk45/src"),
    ] {
        let mut rs_files = Vec::new();
        collect_rs_files(&src, &mut rs_files);
        for path in rs_files {
            let content = fs::read_to_string(&path).expect("read solver source");
            offenders.extend(find_banned_source_lines(
                &path, &content, &banned, "contains",
            ));
        }
    }

    assert!(
        offenders.is_empty(),
        "concrete solver crates must be boring solver wiring over solve-IR only: {offenders:?}"
    );
}

fn find_banned_source_lines(
    path: &Path,
    content: &str,
    banned: &[&str],
    verb: &str,
) -> Vec<String> {
    content
        .lines()
        .enumerate()
        .filter_map(|(line_idx, line)| {
            let needle = banned.iter().find(|needle| line.contains(**needle))?;
            Some(format!(
                "{}:{} {verb} {needle}",
                path.display(),
                line_idx + 1
            ))
        })
        .collect()
}

fn non_test_module_source_lines(content: &str) -> impl Iterator<Item = (usize, &str)> {
    let mut pending_cfg_test = false;
    let mut test_module_depth: Option<usize> = None;
    let mut brace_depth = 0usize;

    content
        .lines()
        .enumerate()
        .filter_map(move |(line_idx, line)| {
            let trimmed = line.trim_start();
            let entering_test_module = pending_cfg_test
                && (trimmed.starts_with("mod ") || trimmed.starts_with("pub mod "));
            pending_cfg_test = trimmed.starts_with("#[cfg(test)]");

            let in_test_module = test_module_depth.is_some() || entering_test_module;
            let open_count = line.matches('{').count();
            let close_count = line.matches('}').count();

            if entering_test_module {
                test_module_depth = Some(brace_depth);
            }

            brace_depth = brace_depth.saturating_add(open_count);
            brace_depth = brace_depth.saturating_sub(close_count);

            if let Some(module_depth) = test_module_depth
                && brace_depth <= module_depth
            {
                test_module_depth = None;
            }

            (!in_test_module).then_some((line_idx, line))
        })
}

fn is_test_or_example_path(path: &Path) -> bool {
    let path = normalized_rel_path(path);
    path.contains("/tests/")
        || path.ends_with("/tests.rs")
        || path.ends_with("_tests.rs")
        || path.ends_with("/main_tests.rs")
        || path.contains("/examples/")
        || path.contains("/test_support.rs")
}

#[test]
fn test_production_code_has_no_panic_todo_or_unimplemented() {
    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);

    let banned = ["panic!(", "todo!(", "unimplemented!("];
    let mut offenders = Vec::new();
    for path in rs_files {
        let rel = path.strip_prefix(&root).unwrap_or(&path);
        if is_test_or_example_path(rel) {
            continue;
        }
        let content = fs::read_to_string(&path).expect("read Rust source");
        offenders.extend(
            non_test_module_source_lines(&content).filter_map(|(line_idx, line)| {
                let token = banned.iter().find(|token| line.contains(**token))?;
                Some(format!(
                    "{}:{} contains {token}",
                    path.display(),
                    line_idx + 1
                ))
            }),
        );
    }

    assert!(
        offenders.is_empty(),
        "production code must not use panic!/todo!/unimplemented!; return a typed error or document an invariant with expect instead: {offenders:#?}"
    );
}

#[test]
fn test_reviewed_tooling_crates_have_no_production_unwraps() {
    let root = workspace_root();
    let mut offenders = Vec::new();

    for src in [
        root.join("crates/rumoca-tool-lint/src"),
        root.join("crates/rumoca-codec-flatbuffers/src"),
    ] {
        let mut rs_files = Vec::new();
        collect_rs_files(&src, &mut rs_files);
        for path in rs_files {
            let content = fs::read_to_string(&path).expect("read reviewed crate source");
            offenders.extend(
                non_test_module_source_lines(&content)
                    .filter(|(_, line)| line.contains(".unwrap()"))
                    .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1)),
            );
        }
    }

    assert!(
        offenders.is_empty(),
        "production code in the reviewed tooling/codec crates must not use `.unwrap()`; \
return errors or use a justified `expect` with an invariant instead: {offenders:#?}"
    );
}

#[test]
fn test_eval_solve_has_no_silent_default_value_fallbacks() {
    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates/rumoca-eval-solve/src"), &mut rs_files);

    let banned = [".unwrap_or(0.0)", ".unwrap_or_default()"];
    let mut offenders = Vec::new();
    for path in rs_files {
        let content = fs::read_to_string(&path).expect("read rumoca-eval-solve source");
        offenders.extend(
            non_test_module_source_lines(&content).filter_map(|(line_idx, line)| {
                let token = banned.iter().find(|token| line.contains(**token))?;
                Some(format!(
                    "{}:{} contains {token}",
                    path.display(),
                    line_idx + 1
                ))
            }),
        );
    }

    assert!(
        offenders.is_empty(),
        "rumoca-eval-solve must fail loudly on missing data instead of substituting default values: {offenders:#?}"
    );
}

#[test]
fn test_eval_dae_silent_default_fallback_inventory_is_explicit() {
    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates/rumoca-eval-dae/src"), &mut rs_files);

    let allowed: BTreeSet<&str> = BTreeSet::new();

    let mut unexpected = Vec::new();
    let mut seen = BTreeSet::new();
    for path in rs_files {
        let rel = path.strip_prefix(&root).unwrap_or(&path);
        let rel = normalized_rel_path(rel);
        let content = fs::read_to_string(&path).expect("read rumoca-eval-dae source");
        for (_, line) in non_test_module_source_lines(&content) {
            if !(line.contains(".unwrap_or(0.0)") || line.contains(".unwrap_or_default()")) {
                continue;
            }
            let entry = format!("{rel}:{}", line.trim());
            if allowed.contains(entry.as_str()) {
                seen.insert(entry);
            } else {
                unexpected.push(entry);
            }
        }
    }

    let missing: Vec<_> = allowed
        .iter()
        .filter(|entry| !seen.contains(**entry))
        .copied()
        .collect();

    assert!(
        unexpected.is_empty() && missing.is_empty(),
        "rumoca-eval-dae default fallbacks are Phase 0 migration debt and must not change without updating the roadmap backlog; unexpected={unexpected:#?}, missing={missing:#?}"
    );
}

#[test]
fn test_phase_solve_explicit_starts_do_not_default_failed_checked_eval() {
    let path = workspace_root().join("crates/rumoca-phase-solve/src/solve_model.rs");
    let content = fs::read_to_string(&path).expect("read phase-solve solve_model");
    let production = content
        .split("#[cfg(test)]")
        .next()
        .expect("solve_model source should include production section");
    let banned = [
        "eval_expr::<f64>(expr, env).unwrap_or(default_start)",
        ".map(|value| finite_start_value(value, default_start)).unwrap_or(default_start)",
    ];

    let offenders: Vec<&str> = production
        .lines()
        .filter(|line| banned.iter().any(|pattern| line.contains(pattern)))
        .collect();

    assert!(
        offenders.is_empty(),
        "explicit DAE/Solve start expressions must fail lowering when checked evaluation fails, not silently use type defaults: {offenders:#?}"
    );
}

#[test]
fn test_phase_solve_does_not_reset_global_eval_dae_runtime() {
    let root = workspace_root().join("crates/rumoca-phase-solve/src");
    let mut rs_files = Vec::new();
    collect_rs_files(&root, &mut rs_files);

    let offenders: Vec<String> = rs_files
        .iter()
        .filter_map(|path| {
            let content = fs::read_to_string(path).ok()?;
            content
                .contains("rumoca_eval_dae::eval::clear_runtime_state()")
                .then(|| path.display().to_string())
        })
        .collect();

    assert!(
        offenders.is_empty(),
        "phase-solve lowering must thread request-local EvalRuntimeState instead of clearing eval-DAE global runtime state: {offenders:#?}"
    );
}

#[test]
fn test_phase_solve_does_not_seed_global_eval_dae_pre_store() {
    let root = workspace_root().join("crates/rumoca-phase-solve/src");
    let mut rs_files = Vec::new();
    collect_rs_files(&root, &mut rs_files);

    let offenders: Vec<String> = rs_files
        .iter()
        .filter_map(|path| {
            let content = fs::read_to_string(path).ok()?;
            content
                .contains("seed_pre_values_from_env")
                .then(|| path.display().to_string())
        })
        .collect();

    assert!(
        offenders.is_empty(),
        "phase-solve lowering must seed pre() values into request-local EvalRuntimeState, not the eval-DAE global compatibility pre-store: {offenders:#?}"
    );
}

#[test]
fn test_production_code_outside_eval_dae_does_not_use_global_eval_dae_runtime_wrappers() {
    let root = workspace_root();
    let eval_dae_src = root.join("crates/rumoca-eval-dae/src");
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);

    let banned = [
        "rumoca_eval_dae::clear_pre_values",
        "rumoca_eval_dae::clear_runtime_state",
        "rumoca_eval_dae::snapshot_pre_values",
        "rumoca_eval_dae::restore_pre_values",
        "rumoca_eval_dae::seed_pre_values_from_env",
        "rumoca_eval_dae::set_pre_value",
        "rumoca_eval_dae::get_pre_value",
        "rumoca_eval_dae::eval::clear_pre_values",
        "rumoca_eval_dae::eval::clear_runtime_state",
        "rumoca_eval_dae::eval::snapshot_pre_values",
        "rumoca_eval_dae::eval::restore_pre_values",
        "rumoca_eval_dae::eval::seed_pre_values_from_env",
        "rumoca_eval_dae::eval::set_pre_value",
        "rumoca_eval_dae::eval::get_pre_value",
        "use rumoca_eval_dae::{clear_pre_values",
        "use rumoca_eval_dae::{clear_runtime_state",
        "use rumoca_eval_dae::{snapshot_pre_values",
        "use rumoca_eval_dae::{restore_pre_values",
        "use rumoca_eval_dae::{seed_pre_values_from_env",
        "use rumoca_eval_dae::{set_pre_value",
        "use rumoca_eval_dae::{get_pre_value",
        "use rumoca_eval_dae::eval::{clear_pre_values",
        "use rumoca_eval_dae::eval::{clear_runtime_state",
        "use rumoca_eval_dae::eval::{snapshot_pre_values",
        "use rumoca_eval_dae::eval::{restore_pre_values",
        "use rumoca_eval_dae::eval::{seed_pre_values_from_env",
        "use rumoca_eval_dae::eval::{set_pre_value",
        "use rumoca_eval_dae::eval::{get_pre_value",
    ];

    let mut offenders = Vec::new();
    for path in rs_files {
        let rel = path.strip_prefix(&root).unwrap_or(&path);
        let rel = normalized_rel_path(rel);
        if path.starts_with(&eval_dae_src)
            || rel.contains("/tests/")
            || rel.contains("/benches/")
            || rel.ends_with("_test.rs")
        {
            continue;
        }
        let content = fs::read_to_string(&path).expect("read Rust source");
        for (line_idx, line) in non_test_module_source_lines(&content) {
            let trimmed = line.trim_start();
            if trimmed.starts_with("//") {
                continue;
            }
            if let Some(token) = banned.iter().find(|token| line.contains(**token)) {
                offenders.push(format!(
                    "{}:{} contains {token}",
                    path.display(),
                    line_idx + 1
                ));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "production code outside rumoca-eval-dae must use request-local EvalRuntimeState APIs \
instead of eval-DAE global compatibility runtime wrappers: {offenders:#?}"
    );
}

#[test]
fn test_codegen_fallback_inventory_does_not_grow() {
    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(
        &root.join("crates/rumoca-phase-codegen/src/codegen"),
        &mut rs_files,
    );

    let mut value_undefined = Vec::new();
    let mut optional_empty = Vec::new();
    let mut unwrap_or_default = Vec::new();
    for path in rs_files {
        let content = fs::read_to_string(&path).expect("read rumoca-phase-codegen source");
        for (line_idx, line) in non_test_module_source_lines(&content) {
            let location = format!("{}:{}", path.display(), line_idx + 1);
            if line.contains("Value::UNDEFINED") {
                value_undefined.push(location.clone());
            }
            if line.contains("Ok(None)") {
                optional_empty.push(location.clone());
            }
            if line.contains(".unwrap_or_default()") {
                unwrap_or_default.push(location);
            }
        }
    }

    assert!(
        value_undefined.is_empty(),
        "production codegen must not construct Minijinja Value::UNDEFINED sentinels; \
use explicit optional objects or structured errors instead: {value_undefined:#?}"
    );
    assert!(
        optional_empty.is_empty(),
        "production codegen optional render misses must go through an explicit helper \
instead of raw Ok(None) fallback returns: {optional_empty:#?}"
    );
    assert!(
        unwrap_or_default.is_empty(),
        "codegen must not add unwrap_or_default fallback paths: {unwrap_or_default:#?}"
    );
}

#[test]
fn test_solver_rk45_crate_owns_second_backend_without_diffsol_dependency() {
    let cargo_toml = workspace_root().join("crates/rumoca-solver-rk45/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca-solver-rk45 Cargo.toml");

    assert!(
        section_contains_dependency(&content, "dependencies", "rumoca-solver"),
        "rumoca-solver-rk45 must depend on rumoca-solver for shared runtime helpers"
    );
    assert!(
        !section_contains_dependency(&content, "dependencies", "rumoca-sim"),
        "rumoca-solver-rk45 must not depend on the rumoca-sim facade"
    );
    assert!(
        !section_contains_dependency(&content, "dependencies", "diffsol"),
        "rumoca-solver-rk45 must stay pure Rust and must not depend on diffsol"
    );
    assert!(
        section_contains_dependency(&content, "dependencies", "rumoca-ir-solve"),
        "rumoca-solver-rk45 must consume solver-facing IR"
    );
    for banned in [
        "rumoca-ir-dae",
        "rumoca-core",
        "rumoca-phase-structural",
        "rumoca-phase-solve",
    ] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-solver-rk45 must not depend on {banned}; \
DAE-to-Solve lowering belong upstream in rumoca-phase-solve"
        );
    }
    assert!(
        !section_contains_dependency(&content, "dependencies", "rumoca-web"),
        "rumoca-solver-rk45 must not depend on rumoca-web"
    );
}

#[test]
fn test_io_contract_crate_is_runtime_and_visualization_free() {
    let cargo_toml = workspace_root().join("crates/rumoca-codec/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca-codec Cargo.toml");

    for banned in [
        "rumoca-compile",
        "rumoca-solver",
        "rumoca-solver-diffsol",
        "rumoca-solver-rk45",
        "rumoca-web",
        "tiny_http",
        "tungstenite",
    ] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-codec must not depend on {banned}; \
Author reminder: keep the generic lockstep I/O contract transport-free and solver-free."
        );
    }
}

#[test]
fn test_codec_facade_cross_crate_exports_are_curated() {
    let lib_rs = workspace_root().join("crates/rumoca-codec/src/lib.rs");
    let content = fs::read_to_string(&lib_rs).expect("read rumoca-codec lib.rs");
    let root_exports = collect_root_pub_use_statements(&content);

    assert_eq!(
        root_exports,
        vec!["pub use rumoca_signal_frame::SignalFrame;"],
        "rumoca-codec is an approved facade, but root cross-crate exports must stay curated"
    );
    assert!(
        content
            .contains("pub use rumoca_codec_flatbuffers::config::{MessageConfig, SchemaConfig};"),
        "rumoca-codec may re-export typed active-backend config only through its config module"
    );
    assert!(
        !content.contains("pub use rumoca_codec_flatbuffers::*"),
        "rumoca-codec must not wildcard-forward backend APIs"
    );
}

#[test]
fn test_sim_facade_cross_crate_exports_are_curated() {
    let lib_rs = workspace_root().join("crates/rumoca-sim/src/lib.rs");
    let content = fs::read_to_string(&lib_rs).expect("read rumoca-sim lib.rs");
    let root_exports: Vec<String> = collect_root_pub_use_statements(&content)
        .into_iter()
        .filter(|export| export.contains("rumoca_"))
        .collect();

    assert_eq!(
        root_exports.len(),
        3,
        "rumoca-sim root facade should expose only solve-preparation helpers, the curated solver \
         API, and the NaN-trace debug facade"
    );
    assert!(
        root_exports.iter().any(|export| {
            export == "pub use rumoca_phase_solve::{lower_solve_artifacts, lower_solve_problem};"
        }),
        "rumoca-sim may expose solve lowering/artifact preparation as its simulation-preparation facade"
    );
    assert!(
        root_exports
            .iter()
            .any(|export| export.starts_with("pub use rumoca_solver::{")),
        "rumoca-sim may expose the curated backend-neutral solver API"
    );
    assert!(
        root_exports
            .iter()
            .any(|export| export == "pub use rumoca_eval_solve::nan_trace;"),
        "rumoca-sim may expose the NaN-trace module so the CLI can drive it without an env var"
    );
    assert!(
        !content.contains("pub use rumoca_solver::*")
            && !content.contains("pub use rumoca_phase_solve::*")
            && !content.contains("pub use rumoca_solver_diffsol::*")
            && !content.contains("pub use rumoca_solver_rk45::*")
            && !content.contains("pub use rumoca_web::*"),
        "rumoca-sim must not wildcard-forward lower-layer APIs"
    );
}

#[test]
fn test_codec_flatbuffers_crate_is_protocol_only() {
    let cargo_toml = workspace_root().join("crates/rumoca-codec-flatbuffers/Cargo.toml");
    let content =
        fs::read_to_string(&cargo_toml).expect("read rumoca-codec-flatbuffers Cargo.toml");

    // The shared contract now lives in rumoca-signal-frame; rumoca-codec is
    // a higher-level facade that pulls in rumoca-codec-flatbuffers as one of
    // its impls, so depending on rumoca-codec from here would invert the dep
    // direction.
    assert!(
        section_contains_dependency(&content, "dependencies", "rumoca-signal-frame"),
        "rumoca-codec-flatbuffers must depend on rumoca-signal-frame for the generic signal-frame contract"
    );

    for banned in [
        "rumoca-codec",
        "rumoca-compile",
        "rumoca-solver",
        "rumoca-solver-diffsol",
        "rumoca-solver-rk45",
        "rumoca-web",
        "tiny_http",
        "tungstenite",
        "gilrs",
        "crossterm",
        "signal-hook",
    ] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-codec-flatbuffers must not depend on {banned}; \
Author reminder: keep FlatBuffer IO support protocol-only."
        );
    }
}

#[test]
fn test_input_crate_is_device_adapter_free() {
    // rumoca-input owns the vocabulary and engine contracts.
    // Concrete device adapter crates depend on rumoca-input, not the other way around.
    // The Devices factory that composes adapters lives in rumoca-sim.
    let content = fs::read_to_string(workspace_root().join("crates/rumoca-input/Cargo.toml"))
        .expect("read rumoca-input Cargo.toml");

    for banned in [
        "rumoca-input-gamepad",
        "rumoca-input-keyboard",
        "gilrs",
        "crossterm",
    ] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-input must not depend on {banned}; \
Author reminder: rumoca-input owns the vocabulary and engine — adapter crates depend on it, not the other way around. \
The Devices runtime factory belongs in rumoca-sim."
        );
    }

    // Adapter crates must depend on rumoca-input (not a separate types crate).
    for adapter in ["rumoca-input-gamepad", "rumoca-input-keyboard"] {
        let adapter_toml =
            fs::read_to_string(workspace_root().join(format!("crates/{adapter}/Cargo.toml")))
                .unwrap_or_else(|error| panic!("read {adapter} Cargo.toml: {error}"));
        assert!(
            section_contains_dependency(&adapter_toml, "dependencies", "rumoca-input"),
            "{adapter} must depend on rumoca-input for shared vocabulary"
        );
        assert!(
            !section_contains_dependency(&adapter_toml, "dependencies", "rumoca-input-types"),
            "{adapter} must not depend on the removed rumoca-input-types crate"
        );
    }
}

#[test]
fn test_sim_facade_owns_no_visualization_assets() {
    let sim_lib = workspace_root().join("crates/rumoca-solver/src/lib.rs");
    let sim_lib_content = fs::read_to_string(&sim_lib).expect("read rumoca-solver src/lib.rs");
    assert!(
        !sim_lib_content.contains("pub mod results_web;"),
        "rumoca-solver must not expose a results_web module; browser visualization assets are package-owned"
    );

    for removed_path in [
        "crates/rumoca-solver/src/results_web.rs",
        "crates/rumoca-solver/web/results_app.css",
        "crates/rumoca-solver/web/results_app.js",
        "crates/rumoca-solver/web/three.min.js",
        "crates/rumoca-solver/web/visualization_shared.js",
    ] {
        assert!(
            !workspace_root().join(removed_path).exists(),
            "rumoca-solver must not retain visualization asset path {removed_path}"
        );
    }
}

#[test]
fn test_web_assets_are_package_owned_not_a_rust_crate() {
    assert!(
        !workspace_root()
            .join("crates/rumoca-web/Cargo.toml")
            .exists(),
        "web visualization assets must be package-owned under packages/rumoca-web; \
         Rust crates may consume prepared assets but must not own browser package builds"
    );
    assert!(
        workspace_root()
            .join("packages/rumoca-web/package.json")
            .is_file(),
        "packages/rumoca-web must remain the browser asset package"
    );
}

// The rumoca-sim-fb and rumoca-web crates were dissolved. App-level simulation
// composition lives in rumoca-sim/rumoca, while browser dependency ownership
// lives in packages/rumoca-web.

#[test]
fn test_rumoca_entry_uses_session_facade_for_ir() {
    let cargo_toml = workspace_root().join("crates/rumoca/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca Cargo.toml");

    // rumoca-sim is the scheduled-sim-facade dep; it transitively wraps web,
    // transports, codecs, input devices, and signal-hook so the CLI no
    // longer names the lower-level runtime crates directly.
    for required in [
        "rumoca-compile",
        "rumoca-tool-fmt",
        "rumoca-tool-lint",
        "rumoca-sim",
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
Author reminder: use rumoca-compile facade types/APIs instead."
        );
    }
}

#[test]
fn test_test_msl_uses_explicit_runtime_crates() {
    let cargo_toml = workspace_root().join("crates/rumoca-test-msl/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca-test-msl Cargo.toml");

    // rumoca-sim is the scheduled simulation facade that owns sim-core + solver wiring;
    // test-msl pulls runtime-specific crates only as dev-dependencies for
    // direct IR introspection in regression tests.
    for required in ["rumoca-compile", "rumoca-sim"] {
        assert!(
            section_contains_dependency(&content, "dependencies", required),
            "rumoca-test-msl must depend on {required}; \
Author reminder: compile/session and runtime ownership should be explicit."
        );
    }

    for banned in [
        "rumoca",
        "rumoca-ir-dae",
        "rumoca-phase-solve",
        "rumoca-core",
    ] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-test-msl must not depend directly on {banned} in [dependencies]; \
Author reminder: use rumoca-compile::compile plus explicit runtime crates."
        );
    }
    assert!(
        !section_contains_dependency(&content, "dev-dependencies", "rumoca"),
        "rumoca-test-msl must not depend directly on rumoca in [dev-dependencies]; \
Author reminder: use rumoca-compile facade APIs."
    );
    assert!(
        !section_contains_dependency(&content, "dev-dependencies", "rumoca-core"),
        "rumoca-test-msl must not depend directly on rumoca-core in [dev-dependencies]; \
Author reminder: use rumoca-compile::compile::core facade APIs."
    );
    assert!(
        !section_contains_dependency(&content, "dev-dependencies", "rumoca-phase-codegen"),
        "rumoca-test-msl must not depend directly on rumoca-phase-codegen in [dev-dependencies]; \
Author reminder: adapter/regression harnesses should render templates through rumoca_compile::codegen."
    );
}

#[test]
fn test_test_msl_uses_session_codegen_facade_for_template_harnesses() {
    let tests_dir = workspace_root().join("crates/rumoca-test-msl/tests");
    let mut files = Vec::new();
    collect_rs_files(&tests_dir, &mut files);

    let offenders: Vec<_> = files
        .into_iter()
        .filter_map(|file| {
            let content = fs::read_to_string(&file).expect("read source file");
            content.contains("rumoca_phase_codegen").then(|| {
                normalized_rel_path(
                    file.strip_prefix(workspace_root())
                        .expect("workspace-relative file"),
                )
            })
        })
        .collect();

    assert!(
        offenders.is_empty(),
        "rumoca-test-msl must render templates through rumoca_compile::codegen, not rumoca_phase_codegen ({offenders:?}); \
Author reminder: phase-codegen direct access belongs in phase-local tests, not adapter/regression harnesses."
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
            trimmed.starts_with("use rumoca_compile::runtime")
                || trimmed.contains(" rumoca_compile::runtime::")
        });
        assert!(
            !has_runtime_import,
            "source file {} must not import rumoca_compile::runtime; \
Author reminder: use rumoca_compile::codegen for template helpers and explicit sim crates for runtime APIs.",
            file.display()
        );
    }
}

#[test]
fn test_contracts_use_session_facade() {
    let cargo_toml = workspace_root().join("crates/rumoca-contracts/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca-contracts Cargo.toml");

    for banned in ["rumoca-phase-parse", "rumoca-phase-solve", "rumoca-ir-ast"] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-contracts must not depend directly on {banned} in [dependencies]; \
Author reminder: use rumoca-compile facade APIs instead."
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
move analysis/evaluation helpers to rumoca-analysis-dae or rumoca-phase-solve."
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
        if !is_non_facade_crate_file(rel) {
            continue;
        }
        let rel_display = normalized_rel_path(rel);
        let cross_crate_aliases = content
            .lines()
            .filter_map(cross_crate_alias)
            .collect::<BTreeSet<_>>();
        for line in content.lines() {
            if let Some(statement) = cross_crate_public_export_statement(line, &cross_crate_aliases)
            {
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
Author reminder: SPEC_0029_CRATE_BOUNDARIES.md §8 forbids this in every \
non-facade crate. \
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
fn test_solve_ir_owns_backend_neutral_row_ops() {
    let root = workspace_root();
    let solve_ir = root.join("crates/rumoca-ir-solve/src/linear_op.rs");
    let solve_text = fs::read_to_string(&solve_ir).expect("read rumoca-ir-solve linear_op.rs");
    assert!(
        solve_text.contains("pub enum LinearOp"),
        "rumoca-ir-solve must own the backend-neutral row operation IR"
    );
    assert!(
        root.join("crates/rumoca-phase-solve/src/lower.rs").exists(),
        "DAE-to-solve row lowering must live in rumoca-phase-solve"
    );
    assert!(
        root.join("crates/rumoca-exec-cranelift/src/lib.rs")
            .exists(),
        "native Cranelift row compilation must live in rumoca-exec-cranelift"
    );
    assert!(
        !root
            .join("crates/rumoca-phase-solve/src/cranelift")
            .exists(),
        "rumoca-phase-solve must not own concrete Cranelift execution adapter code"
    );
}

#[test]
fn test_cranelift_exec_runtime_uses_finalized_jit_function_pointers() {
    let root = workspace_root();
    let emit_path = root.join("crates/rumoca-exec-cranelift/src/emit.rs");
    let emit_text = fs::read_to_string(&emit_path).expect("read Cranelift emit.rs");
    assert!(
        emit_text.contains("get_finalized_function"),
        "Cranelift execution adapter must retain finalized JIT function pointers for runtime calls"
    );
    assert!(
        emit_text.contains("call_residual_jit") && emit_text.contains("call_jacobian_jit"),
        "Cranelift residual and Jacobian runtime paths must invoke finalized JIT functions"
    );
    assert!(
        emit_text.contains("validate_jit_matches_interpreter"),
        "Cranelift execution adapter must keep the checked row-plan interpreter as a differential guard"
    );
}

#[test]
fn test_exec_crates_do_not_depend_on_eval_dae() {
    let root = workspace_root();
    let offenders: Vec<String> = workspace_crate_dirs(&root)
        .into_iter()
        .filter(|path| {
            path.file_name()
                .is_some_and(|name| name.to_string_lossy().starts_with("rumoca-exec-"))
        })
        .filter_map(|path| {
            let cargo_toml = path.join("Cargo.toml");
            let content = fs::read_to_string(&cargo_toml).ok()?;
            let has_runtime_dep =
                section_contains_dependency(&content, "dependencies", "rumoca-eval-dae");
            let has_dev_dep =
                section_contains_dependency(&content, "dev-dependencies", "rumoca-eval-dae");
            (has_runtime_dep || has_dev_dep).then(|| cargo_toml.display().to_string())
        })
        .collect();

    assert!(
        offenders.is_empty(),
        "execution adapter crates must consume solve/runtime APIs instead of DAE evaluator internals: {offenders:#?}"
    );
}

#[test]
fn test_exec_wasm_consumes_solve_ir_not_dae_or_lowering_phase() {
    let root = workspace_root();
    let cargo_toml = root.join("crates/rumoca-exec-wasm/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read rumoca-exec-wasm Cargo.toml");

    assert!(
        section_contains_dependency(&content, "dependencies", "rumoca-ir-solve"),
        "rumoca-exec-wasm must consume prepared Solve-IR row kernels"
    );
    for banned in ["rumoca-ir-dae", "rumoca-phase-solve"] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-exec-wasm must not depend on {banned}; DAE-to-Solve lowering belongs upstream"
        );
    }
}

#[test]
fn test_runtime_and_codegen_crates_do_not_depend_on_eval_dae() {
    let root = workspace_root();
    let checked_crates = [
        "rumoca-phase-codegen",
        "rumoca-sim",
        "rumoca-solver",
        "rumoca-solver-diffsol",
        "rumoca-solver-rk45",
    ];
    let offenders: Vec<String> = checked_crates
        .iter()
        .filter_map(|crate_name| {
            let cargo_toml = root.join(format!("crates/{crate_name}/Cargo.toml"));
            let content = fs::read_to_string(&cargo_toml).ok()?;
            let has_runtime_dep =
                section_contains_dependency(&content, "dependencies", "rumoca-eval-dae");
            let has_dev_dep =
                section_contains_dependency(&content, "dev-dependencies", "rumoca-eval-dae");
            (has_runtime_dep || has_dev_dep).then(|| cargo_toml.display().to_string())
        })
        .collect();

    assert!(
        offenders.is_empty(),
        "simulation codegen/runtime/solver crates must not depend on rumoca-eval-dae; \
table runtime helpers belong behind solver-facing APIs in rumoca-eval-solve: {offenders:#?}"
    );
}

#[test]
fn test_phase_solve_does_not_own_public_scalarization_api() {
    let root = workspace_root();
    assert!(
        !root
            .join("crates/rumoca-phase-solve/src/scalarize_compute.rs")
            .exists(),
        "phase-solve must not expose scalarization as a phase-owned helper; \
use rumoca-eval-solve scalar fallback APIs at backend boundaries"
    );

    let phase_solve_lib = fs::read_to_string(root.join("crates/rumoca-phase-solve/src/lib.rs"))
        .expect("read phase-solve lib.rs");
    assert!(
        !phase_solve_lib.contains("scalarize_compute"),
        "phase-solve must not publicly re-export scalarization helpers"
    );
}

#[test]
fn test_phase_debug_output_uses_tracing_not_env_stderr() {
    let root = workspace_root();
    let checks: &[(&str, &[&str])] = &[
        (
            "crates/rumoca-phase-structural/src",
            &["RUMOCA_SIM_TRACE", "RUMOCA_SIM_INTROSPECT", "eprintln!"],
        ),
        (
            "crates/rumoca-phase-dae/src",
            &[
                "eprintln!",
                "RUMOCA_DEBUG_TODAE",
                "RUMOCA_DEBUG_EQ_FILTER",
                "RUMOCA_TODAE_PROFILE",
                "RUMOCA_DEBUG_FM_CANON",
                "RUMOCA_DAE_CLOCK_DEBUG",
            ],
        ),
        (
            "crates/rumoca-phase-instantiate/src",
            &["eprintln!", "RUMOCA_DEBUG_CONNECTION_PARAMS"],
        ),
    ];

    let mut offenders = Vec::new();
    for (src, banned) in checks {
        let mut rs_files = Vec::new();
        collect_rs_files(&root.join(src), &mut rs_files);
        for path in rs_files {
            let content = fs::read_to_string(&path).expect("read phase source");
            offenders.extend(find_banned_source_lines(
                &path, &content, banned, "contains",
            ));
        }
    }

    assert!(
        offenders.is_empty(),
        "phase debug output must use the tracing feature instead of \
stderr writes or phase-level debug environment variables: {offenders:?}"
    );
}

#[test]
fn test_phase_typecheck_errors_go_through_phase_error_type() {
    let root = workspace_root();
    let typecheck_src = root.join("crates/rumoca-phase-typecheck/src");
    let allowed_error_module = typecheck_src.join("lib.rs");
    let mut rs_files = Vec::new();
    collect_rs_files(&typecheck_src, &mut rs_files);

    let mut offenders = Vec::new();
    for path in rs_files {
        if path == allowed_error_module {
            continue;
        }
        let content = fs::read_to_string(&path).expect("read phase-typecheck source");
        offenders.extend(find_banned_source_lines(
            &path,
            &content,
            &[
                "CommonDiagnostic::error(",
                "rumoca_core::Diagnostic::error(",
            ],
            "constructs",
        ));
    }

    assert!(
        offenders.is_empty(),
        "phase-typecheck fatal diagnostics must go through TypeCheckError/PhaseError \
instead of constructing CommonDiagnostic::error in helper modules: {offenders:?}"
    );
}

#[test]
fn test_ir_crates_have_no_public_hashmap_or_hashset_fields() {
    // SPEC_0021: public IR/DAE struct fields must use IndexMap/IndexSet, not
    // HashMap/HashSet, so that serialised payloads are deterministically ordered.
    // Method parameters and return types are permitted to use std collections.
    let root = workspace_root();
    let mut offenders = Vec::new();

    for path in collect_ir_crate_rs_files(&root) {
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        offenders.extend(content.lines().enumerate().filter_map(|(line_idx, line)| {
            public_hash_collection_field_location(&path, line_idx, line)
        }));
    }

    assert!(
        offenders.is_empty(),
        "found public HashMap/HashSet fields in rumoca-ir-* crates (SPEC_0021 violation). \
Replace with IndexMap/IndexSet for deterministic serialisation: {offenders:#?}"
    );
}

#[path = "architecture_hardening/size_and_validation.rs"]
mod size_and_validation;

#[path = "architecture_hardening/env_var_registry.rs"]
mod env_var_registry;
