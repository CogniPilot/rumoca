use crate::architecture_hardening_support::{collect_rs_files, workspace_root};
use std::collections::BTreeSet;
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

    fn visit_impl_item_fn(&mut self, item: &'ast syn::ImplItemFn) {
        self.record(&item.vis, &item.sig.ident);
        visit::visit_impl_item_fn(self, item);
    }
}

fn is_generated(path: &Path) -> bool {
    path.components()
        .any(|component| component.as_os_str() == "generated")
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
