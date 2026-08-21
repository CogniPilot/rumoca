use crate::architecture_hardening_support::{collect_rs_files, workspace_root};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};
use syn::visit::{self, Visit};

/// Declaration-only surfaces removed by the workspace API audit.
///
/// Rust's `dead_code` lint deliberately treats externally reachable `pub`
/// items as potential downstream API. These path/name pairs were separately
/// proved to have no workspace caller or documented external contract. Pinning
/// them here prevents a compatibility shim or copied abstraction from silently
/// restoring the dead surface.
const REMOVED_DECLARATION_ONLY_SURFACES: &[(&str, &str)] = &[
    (
        "crates/rumoca-compile/src/session.rs",
        "prewarm_document_read_queries",
    ),
    (
        "crates/rumoca-contracts/src/test_support.rs",
        "expect_simulation_failure",
    ),
    (
        "crates/rumoca-core/src/lib.rs",
        "CLOCK_FACTOR_INTEGER_TOLERANCE",
    ),
    (
        "crates/rumoca-core/src/lib.rs",
        "INTERNAL_SAMPLE_FUNCTION_NAME",
    ),
    (
        "crates/rumoca-eval-solve/src/lib.rs",
        "build_root_refresh_plan",
    ),
    (
        "crates/rumoca-eval-solve/src/lib.rs",
        "report_state_jacobian",
    ),
    (
        "crates/rumoca-eval-solve/src/refresh_plan.rs",
        "root_condition_dependencies",
    ),
    (
        "crates/rumoca-exec-cranelift/src/lib.rs",
        "compile_residual_scalar_program_block",
    ),
    ("crates/rumoca-ir-ast/src/instance.rs", "symbol_def_ids"),
    ("crates/rumoca-ir-flat/src/connections.rs", "add_set"),
    ("crates/rumoca-ir-flat/src/connections.rs", "num_nodes"),
    (
        "crates/rumoca-ir-solve/src/model.rs",
        "set_integrator_history_effect",
    ),
    (
        "crates/rumoca-phase-resolve/src/lib.rs",
        "resolve_with_stats",
    ),
    (
        "crates/rumoca-phase-resolve/src/semantic_checks.rs",
        "check_chained_relationals",
    ),
    (
        "crates/rumoca-phase-resolve/src/semantic_checks.rs",
        "check_der_in_functions",
    ),
    (
        "crates/rumoca-phase-resolve/src/semantic_checks.rs",
        "check_semantics",
    ),
    (
        "crates/rumoca-phase-solve/src/lib.rs",
        "lower_solve_artifacts_with_mass_matrix",
    ),
    (
        "crates/rumoca-phase-structural/src/lib.rs",
        "maximum_regular_subsystem",
    ),
    ("crates/rumoca-sim/src/lib.rs", "compare_trace_files"),
    ("crates/rumoca-sim/src/lib.rs", "clear_parameter_overrides"),
    ("crates/rumoca-sim/src/lib.rs", "set_parameter_values"),
    ("crates/rumoca-solver/src/session.rs", "initial_point"),
    (
        "crates/rumoca-solver/src/runtime/solve_ops.rs",
        "filter_scheduled_root_crossings",
    ),
    (
        "crates/rumoca-solver/src/runtime/solve_runtime.rs",
        "root_condition_is_search_active",
    ),
    (
        "crates/rumoca-solver/src/runtime/solve_runtime/plans.rs",
        "root_search_is_uniformly_inactive",
    ),
    (
        "crates/rumoca-tool-lsp/src/util.rs",
        "token_to_range_in_source",
    ),
];

#[derive(Default)]
struct PublicDeclarations {
    declarations: BTreeSet<(PathBuf, String)>,
    current_path: PathBuf,
}

#[derive(Default)]
struct SyntaxIdentifiers {
    counts: BTreeMap<String, usize>,
}

impl<'ast> Visit<'ast> for SyntaxIdentifiers {
    fn visit_ident(&mut self, ident: &'ast syn::Ident) {
        *self.counts.entry(ident.to_string()).or_default() += 1;
        visit::visit_ident(self, ident);
    }

    fn visit_macro(&mut self, item: &'ast syn::Macro) {
        visit::visit_path(self, &item.path);
        self.visit_token_stream(item.tokens.clone());
    }
}

impl SyntaxIdentifiers {
    fn visit_token_stream(&mut self, tokens: proc_macro2::TokenStream) {
        for token in tokens {
            match token {
                proc_macro2::TokenTree::Group(group) => self.visit_token_stream(group.stream()),
                proc_macro2::TokenTree::Ident(ident) => {
                    *self.counts.entry(ident.to_string()).or_default() += 1;
                }
                proc_macro2::TokenTree::Punct(_) | proc_macro2::TokenTree::Literal(_) => {}
            }
        }
    }
}

impl PublicDeclarations {
    fn record(&mut self, visibility: &syn::Visibility, ident: &syn::Ident) {
        if matches!(visibility, syn::Visibility::Public(_)) {
            self.declarations
                .insert((self.current_path.clone(), ident.to_string()));
        }
    }
}

impl<'ast> Visit<'ast> for PublicDeclarations {
    fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
        self.record(&item.vis, &item.sig.ident);
        visit::visit_item_fn(self, item);
    }

    fn visit_item_struct(&mut self, item: &'ast syn::ItemStruct) {
        self.record(&item.vis, &item.ident);
        visit::visit_item_struct(self, item);
    }

    fn visit_item_enum(&mut self, item: &'ast syn::ItemEnum) {
        self.record(&item.vis, &item.ident);
        visit::visit_item_enum(self, item);
    }

    fn visit_item_const(&mut self, item: &'ast syn::ItemConst) {
        self.record(&item.vis, &item.ident);
        visit::visit_item_const(self, item);
    }

    fn visit_item_static(&mut self, item: &'ast syn::ItemStatic) {
        self.record(&item.vis, &item.ident);
        visit::visit_item_static(self, item);
    }

    fn visit_item_trait(&mut self, item: &'ast syn::ItemTrait) {
        self.record(&item.vis, &item.ident);
        visit::visit_item_trait(self, item);
    }

    fn visit_item_type(&mut self, item: &'ast syn::ItemType) {
        self.record(&item.vis, &item.ident);
        visit::visit_item_type(self, item);
    }

    fn visit_impl_item_fn(&mut self, item: &'ast syn::ImplItemFn) {
        self.record(&item.vis, &item.sig.ident);
        visit::visit_impl_item_fn(self, item);
    }
}

fn is_generated(path: &Path) -> bool {
    path.components()
        .any(|component| component.as_os_str() == "generated")
}

fn is_audit_source(path: &Path) -> bool {
    path.ends_with("crates/rumoca/tests/architecture_hardening_test/public_api_surface.rs")
}

fn collect_audited_text_files(dir: &Path, files: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            if path.file_name().is_some_and(|name| {
                matches!(
                    name.to_str(),
                    Some("target" | ".git" | "generated" | "vendor" | "node_modules" | "dist")
                )
            }) {
                continue;
            }
            collect_audited_text_files(&path, files);
            continue;
        }
        if is_audit_source(&path) {
            continue;
        }
        let extension = path.extension().and_then(|extension| extension.to_str());
        if matches!(extension, Some("rs" | "md" | "mjs" | "js" | "py" | "ts")) {
            files.push(path);
        }
    }
}

fn count_identifiers(source: &str, counts: &mut BTreeMap<String, usize>) {
    for identifier in source.split(|ch: char| !(ch.is_alphanumeric() || ch == '_')) {
        if identifier.is_empty() {
            continue;
        }
        *counts.entry(identifier.to_string()).or_default() += 1;
    }
}

fn declaration_only_public_surfaces(root: &Path) -> BTreeSet<(PathBuf, String)> {
    let mut crate_rust_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut crate_rust_files);
    crate_rust_files.retain(|path| !is_generated(path));
    crate_rust_files.sort();

    let mut declarations = PublicDeclarations::default();
    for path in &crate_rust_files {
        let source = fs::read_to_string(path).expect("read Rust source for public API inventory");
        let file = syn::parse_file(&source).unwrap_or_else(|error| {
            panic!("parse {} for public API inventory: {error}", path.display())
        });
        declarations.current_path = path.clone();
        declarations.visit_file(&file);
    }

    let mut audited_files = Vec::new();
    for relative in [
        "crates",
        "packages",
        "infra",
        "examples",
        "spec",
        "docs/dev-guide/src",
        "docs/user-guide/src",
    ] {
        let path = root.join(relative);
        collect_audited_text_files(&path, &mut audited_files);
    }
    audited_files.sort();
    audited_files.dedup();

    let mut identifier_counts = BTreeMap::new();
    for path in audited_files {
        let source = fs::read_to_string(&path).expect("read source for public API usage inventory");
        if path.extension().is_some_and(|extension| extension == "rs") {
            let file = syn::parse_file(&source).unwrap_or_else(|error| {
                panic!("parse {} for API usage inventory: {error}", path.display())
            });
            let mut identifiers = SyntaxIdentifiers::default();
            identifiers.visit_file(&file);
            for (name, count) in identifiers.counts {
                *identifier_counts.entry(name).or_default() += count;
            }
        } else {
            count_identifiers(&source, &mut identifier_counts);
        }
    }

    declarations
        .declarations
        .into_iter()
        .filter(|(_, name)| identifier_counts.get(name).copied() == Some(1))
        .collect()
}

#[test]
fn new_declaration_only_public_surfaces_require_explicit_disposition() {
    let root = workspace_root();
    let candidates = declaration_only_public_surfaces(&root);
    assert!(
        candidates.is_empty(),
        "public declarations with no syntactic workspace use or external contract must be narrowed to pub(crate); dead_code = deny will then prove whether to delete them:\n  {}",
        candidates
            .iter()
            .map(|(path, name)| format!(
                "{}: {name}",
                path.strip_prefix(&root).unwrap_or(path).display()
            ))
            .collect::<Vec<_>>()
            .join("\n  ")
    );
}

#[test]
fn public_surface_inventory_ignores_source_comments_and_sees_facade_reexports() {
    let file = syn::parse_file(
        r#"
        /// dead_leaf is deliberately mentioned in its own documentation.
        pub fn dead_leaf() {}
        mod implementation { pub struct Exported; }
        pub use implementation::Exported;
        pub const USED_IN_MACRO: &str = "value";
        fn consume() { print!("{}", USED_IN_MACRO); }
        "#,
    )
    .expect("parse public surface inventory control");
    let mut identifiers = SyntaxIdentifiers::default();
    identifiers.visit_file(&file);
    assert_eq!(identifiers.counts.get("dead_leaf"), Some(&1));
    assert_eq!(identifiers.counts.get("Exported"), Some(&2));
    assert_eq!(identifiers.counts.get("USED_IN_MACRO"), Some(&2));
}

#[test]
fn removed_declaration_only_public_items_do_not_return() {
    let root = workspace_root();
    let mut files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut files);
    files.retain(|path| !is_generated(path));
    files.sort();

    let mut inventory = PublicDeclarations::default();
    for path in files {
        let source = fs::read_to_string(&path).expect("read Rust source for public API audit");
        let file = syn::parse_file(&source).unwrap_or_else(|error| {
            panic!("parse {} for public API audit: {error}", path.display())
        });
        inventory.current_path = path;
        inventory.visit_file(&file);
    }

    let restored = REMOVED_DECLARATION_ONLY_SURFACES
        .iter()
        .filter(|(path, name)| {
            inventory
                .declarations
                .contains(&(root.join(path), (*name).to_owned()))
        })
        .map(|(path, name)| format!("{path}: {name}"))
        .collect::<Vec<_>>();
    assert!(
        restored.is_empty(),
        "audited declaration-only public surfaces were restored:\n{}",
        restored.join("\n")
    );
}

#[test]
fn audited_facades_do_not_restore_forwarded_or_private_surfaces() {
    let root = workspace_root();
    let compile_facade = fs::read_to_string(root.join("crates/rumoca-compile/src/lib.rs"))
        .expect("read compile facade");
    for forbidden in [
        "pub use rumoca_core as",
        "AstCausality",
        "AstComponent",
        "AstExpression",
        "AstToken",
        "AstVariability",
    ] {
        assert!(
            !compile_facade.contains(forbidden),
            "rumoca-compile restored audited forwarding surface `{forbidden}`"
        );
    }

    let solver_root = fs::read_to_string(root.join("crates/rumoca-solver/src/lib.rs"))
        .expect("read solver facade");
    assert!(
        !solver_root.contains("pub use runtime::projection"),
        "rumoca-solver restored component-private projection re-exports"
    );
    let runtime_root = fs::read_to_string(root.join("crates/rumoca-solver/src/runtime/mod.rs"))
        .expect("read solver runtime modules");
    assert!(
        !runtime_root.contains("pub mod projection;"),
        "rumoca-solver restored public access to component-private projection policy"
    );
}
