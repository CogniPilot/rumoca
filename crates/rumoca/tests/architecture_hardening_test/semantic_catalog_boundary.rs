//! Resolve-issued semantic-catalog authority boundary (SPEC_0036 / AS-025).

use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

use syn::visit::{self, Visit};

use super::architecture_hardening_support::{
    attributes_require_test, production_rust_sources, workspace_crate_dirs, workspace_root,
};

const RAW_ISSUER_CALLS: [(&str, &str); 6] = [
    (
        "from_resolve_registration",
        "crates/rumoca-phase-resolve/src/lib.rs",
    ),
    (
        "begin_resolve_check",
        "crates/rumoca-phase-resolve/src/semantic_checks/external_objects.rs",
    ),
    (
        "insert_from_resolve_check",
        "crates/rumoca-phase-resolve/src/semantic_checks/external_objects.rs",
    ),
    (
        "from_resolve_check",
        "crates/rumoca-phase-resolve/src/semantic_checks/external_objects.rs",
    ),
    (
        "from_resolve_issued",
        "crates/rumoca-phase-resolve/src/lib.rs",
    ),
    (
        "clone_for_typecheck_publication",
        "crates/rumoca-phase-typecheck/src/instanced.rs",
    ),
];

struct RawIssuerCallVisitor<'a> {
    calls: &'a mut Vec<String>,
}

impl<'ast> Visit<'ast> for RawIssuerCallVisitor<'_> {
    fn visit_item_mod(&mut self, module: &'ast syn::ItemMod) {
        if !attributes_require_test(&module.attrs) {
            visit::visit_item_mod(self, module);
        }
    }

    fn visit_item_fn(&mut self, function: &'ast syn::ItemFn) {
        if !attributes_require_test(&function.attrs) {
            visit::visit_item_fn(self, function);
        }
    }

    fn visit_impl_item_fn(&mut self, function: &'ast syn::ImplItemFn) {
        if !attributes_require_test(&function.attrs) {
            visit::visit_impl_item_fn(self, function);
        }
    }

    fn visit_item_use(&mut self, item: &'ast syn::ItemUse) {
        if !attributes_require_test(&item.attrs) {
            self.record_renamed_import(&item.tree);
        }
    }

    fn visit_expr_path(&mut self, path: &'ast syn::ExprPath) {
        if let Some(segment) = path.path.segments.last() {
            self.record(segment.ident.to_string());
        }
        visit::visit_expr_path(self, path);
    }

    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        self.record(call.method.to_string());
        visit::visit_expr_method_call(self, call);
    }
}

impl RawIssuerCallVisitor<'_> {
    fn record_renamed_import(&mut self, tree: &syn::UseTree) {
        match tree {
            syn::UseTree::Path(path) => self.record_renamed_import(&path.tree),
            syn::UseTree::Group(group) => {
                for item in &group.items {
                    self.record_renamed_import(item);
                }
            }
            syn::UseTree::Rename(rename) => self.record(rename.ident.to_string()),
            syn::UseTree::Name(_) | syn::UseTree::Glob(_) => {}
        }
    }

    fn record(&mut self, name: String) {
        if RAW_ISSUER_CALLS
            .iter()
            .any(|(expected, _)| *expected == name)
        {
            self.calls.push(name);
        }
    }
}

#[test]
fn resolve_is_the_only_production_semantic_catalog_issuer() {
    let root = workspace_root();
    let mut observed = BTreeMap::<String, Vec<String>>::new();
    for crate_root in workspace_crate_dirs(&root) {
        for (path, source) in production_rust_sources(&crate_root, &root) {
            let syntax = syn::parse_file(&source)
                .unwrap_or_else(|error| panic!("parse {}: {error}", path.display()));
            let mut calls = Vec::new();
            RawIssuerCallVisitor { calls: &mut calls }.visit_file(&syntax);
            for call in calls {
                observed
                    .entry(call)
                    .or_default()
                    .push(path.display().to_string());
            }
        }
    }

    for (call, expected_path) in RAW_ISSUER_CALLS {
        let paths = observed.get(call).map(Vec::as_slice).unwrap_or_default();
        assert_eq!(
            paths.len(),
            1,
            "raw semantic-catalog issuer `{call}` must have one production callsite: {paths:?}"
        );
        assert!(
            Path::new(&paths[0]).ends_with(expected_path),
            "raw semantic-catalog issuer `{call}` moved from its reviewed Resolve boundary: {paths:?}"
        );
    }
}

#[test]
fn raw_class_tree_semantic_projector_stays_deleted() {
    let root = workspace_root();
    let source =
        fs::read_to_string(root.join("crates/rumoca-ir-ast/src/instance/semantic_catalogs.rs"))
            .expect("read semantic catalog owner");
    assert!(
        !source.contains("fn project("),
        "a raw ClassTree projector would bypass the Resolve-issued proof"
    );

    for crate_root in workspace_crate_dirs(&root) {
        for (path, source) in production_rust_sources(&crate_root, &root) {
            assert!(
                !source.contains("SemanticCatalogProjection::project"),
                "production raw-tree semantic projection reappeared in {}",
                path.display()
            );
        }
    }
}

#[test]
fn public_instanced_typecheck_requires_resolve_brand() {
    let root = workspace_root();
    let path = root.join("crates/rumoca-phase-typecheck/src/typechecker/api.rs");
    let source = fs::read_to_string(&path).expect("read public Typecheck API");
    let syntax = syn::parse_file(&source).expect("parse public Typecheck API");
    let function = syntax
        .items
        .iter()
        .find_map(|item| match item {
            syn::Item::Fn(function) if function.sig.ident == "typecheck_instanced" => {
                Some(function)
            }
            _ => None,
        })
        .expect("public instanced Typecheck entry exists");
    assert!(
        matches!(function.vis, syn::Visibility::Public(_)),
        "the checked instanced Typecheck entry must remain public"
    );
    let first = function
        .sig
        .inputs
        .first()
        .expect("instanced Typecheck has a Resolve input");
    let syn::FnArg::Typed(first) = first else {
        panic!("free Typecheck function cannot have a self receiver");
    };
    let syn::Type::Reference(reference) = first.ty.as_ref() else {
        panic!("instanced Typecheck must borrow the Resolve brand");
    };
    let syn::Type::Path(path) = reference.elem.as_ref() else {
        panic!("instanced Typecheck input must be a named Resolve brand");
    };
    assert_eq!(
        path.path
            .segments
            .last()
            .map(|segment| segment.ident.to_string())
            .as_deref(),
        Some("ResolvedTree"),
        "public instanced Typecheck must not accept a raw ClassTree"
    );
}

#[test]
fn raw_issuer_aliases_do_not_evade_the_reference_inventory() {
    let syntax = syn::parse_file(
        r#"
        use elsewhere::from_resolve_issued as issue_catalog;

        fn forge() {
            issue_catalog();
            let clone_catalog = Projection::clone_for_typecheck_publication;
            clone_catalog();
        }
        "#,
    )
    .expect("parse issuer-alias mutation fixture");
    let mut calls = Vec::new();
    RawIssuerCallVisitor { calls: &mut calls }.visit_file(&syntax);
    calls.sort();
    assert_eq!(
        calls,
        [
            "clone_for_typecheck_publication".to_string(),
            "from_resolve_issued".to_string(),
        ],
        "renamed imports and local function aliases must retain the raw issuer identity"
    );
}

#[test]
fn connection_graph_roles_remain_a_closed_core_vocabulary() {
    let root = workspace_root();
    let owner = root.join("crates/rumoca-core/src/connection_graph.rs");
    let source = fs::read_to_string(&owner).expect("read Connections role owner");
    let syntax = syn::parse_file(&source).expect("parse Connections role owner");
    let role = syntax
        .items
        .iter()
        .find_map(|item| match item {
            syn::Item::Enum(item) if item.ident == "ConnectionGraphOperatorRole" => Some(item),
            _ => None,
        })
        .expect("core owns the typed Connections role vocabulary");
    assert!(
        role.attrs
            .iter()
            .all(|attribute| !attribute.path().is_ident("non_exhaustive")),
        "Connections roles must remain exhaustive"
    );
    let variants = role
        .variants
        .iter()
        .map(|variant| variant.ident.to_string())
        .collect::<Vec<_>>();
    assert_eq!(
        variants,
        ["Branch", "Root", "PotentialRoot", "IsRoot", "Rooted"],
        "the MLS section 9.4 role vocabulary is exactly the reviewed five-role set"
    );

    for crate_root in workspace_crate_dirs(&root) {
        for (path, source) in production_rust_sources(&crate_root, &root) {
            assert!(
                !source.contains(concat!("ConnectionGraphOperator", "Names"))
                    && !source.contains(concat!("CONNECTION_GRAPH_", "OPERATORS"))
                    && !source.contains(concat!("ConnectionOperator", "Role")),
                "legacy string or duplicate role vocabulary reappeared in {}",
                path.display()
            );
        }
    }
}
