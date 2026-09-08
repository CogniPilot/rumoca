//! Exact canonical-helper ownership required by SPEC_0029 / SPEC_0041.

use super::architecture_hardening_support::{production_rust_sources, workspace_root};
use std::fs;
use std::path::{Path, PathBuf};
use syn::visit::{self, Visit};

const OWNERS: &[(&str, &str)] = &[
    (
        "escape_modelica_string",
        "crates/rumoca-core/src/modelica_builtins.rs",
    ),
    (
        "row_major_strides",
        "crates/rumoca-core/src/structured_domain.rs",
    ),
];

const TYPE_OWNERS: &[(&str, &str)] = &[(
    "RealMatrixMultiplySemantics",
    "crates/rumoca-core/src/matrix_multiply.rs",
)];

#[test]
fn exact_spec_0041_helpers_have_one_canonical_owner() {
    let root = workspace_root();
    let sources = workspace_production_sources(&root);
    let declarations = helper_declarations(&sources);
    for (name, owner) in OWNERS {
        let actual = declarations
            .iter()
            .filter(|(_, declared)| declared == name)
            .map(|(path, _)| path.as_str())
            .collect::<Vec<_>>();
        assert_eq!(
            actual,
            [*owner],
            "SPEC_0041 assigns `{name}` to exactly one canonical owner"
        );
    }
}

#[test]
fn duplicate_helper_fixture_is_detected() {
    let sources = vec![
        (
            PathBuf::from("crates/rumoca-core/src/modelica_builtins.rs"),
            "fn escape_modelica_string(_: &str) -> String { String::new() }".to_string(),
        ),
        (
            PathBuf::from("crates/other/src/lib.rs"),
            "fn escape_modelica_string(_: &str) -> String { String::new() }".to_string(),
        ),
    ];
    let declarations = helper_declarations(&sources);
    assert_eq!(
        declarations
            .iter()
            .filter(|(_, name)| name == "escape_modelica_string")
            .count(),
        2
    );
}

#[test]
fn exact_spec_0041_types_have_one_canonical_owner_and_no_cross_crate_reexport() {
    let root = workspace_root();
    let sources = workspace_production_sources(&root);
    let declarations = type_declarations(&sources);
    for (name, owner) in TYPE_OWNERS {
        let actual = declarations
            .iter()
            .filter(|(_, declared)| declared == name)
            .map(|(path, _)| path.as_str())
            .collect::<Vec<_>>();
        assert_eq!(
            actual,
            [*owner],
            "SPEC_0041 assigns `{name}` to exactly one canonical owner"
        );
    }

    let reexports = public_type_reexports(&sources);
    assert_eq!(
        reexports,
        ["crates/rumoca-core/src/lib.rs"],
        "the owning rumoca-core crate root is the only public forwarding surface for \
         RealMatrixMultiplySemantics; consumers import rumoca_core directly"
    );
}

#[test]
fn duplicate_owned_type_and_cross_crate_reexport_fixtures_are_detected() {
    let sources = vec![
        (
            PathBuf::from("crates/rumoca-core/src/matrix_multiply.rs"),
            "pub enum RealMatrixMultiplySemantics { FirstProduct, PositiveZero }".to_string(),
        ),
        (
            PathBuf::from("crates/other/src/lib.rs"),
            [
                "pub type RealMatrixMultiplySemantics = u8; ",
                "pub use rumoca_core::RealMatrixMultiplySemantics;",
            ]
            .concat(),
        ),
    ];
    assert_eq!(type_declarations(&sources).len(), 2);
    assert_eq!(public_type_reexports(&sources), ["crates/other/src/lib.rs"]);
}

fn workspace_production_sources(root: &Path) -> Vec<(PathBuf, String)> {
    let mut crates = fs::read_dir(root.join("crates"))
        .expect("read workspace crates")
        .map(|entry| entry.expect("workspace crate entry").path())
        .filter(|path| path.join("Cargo.toml").is_file())
        .collect::<Vec<_>>();
    crates.sort();
    crates
        .into_iter()
        .flat_map(|crate_root| production_rust_sources(&crate_root, root))
        .collect()
}

fn helper_declarations(sources: &[(PathBuf, String)]) -> Vec<(String, String)> {
    struct DeclarationVisitor<'a> {
        path: &'a Path,
        declarations: &'a mut Vec<(String, String)>,
    }

    impl<'ast> Visit<'ast> for DeclarationVisitor<'_> {
        fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
            let name = item.sig.ident.to_string();
            if OWNERS.iter().any(|(owned, _)| *owned == name) {
                self.declarations
                    .push((self.path.display().to_string(), name));
            }
            visit::visit_item_fn(self, item);
        }
    }

    let mut declarations = Vec::new();
    for (path, source) in sources {
        let syntax = syn::parse_file(source)
            .unwrap_or_else(|error| panic!("parse {}: {error}", path.display()));
        DeclarationVisitor {
            path,
            declarations: &mut declarations,
        }
        .visit_file(&syntax);
    }
    declarations.sort();
    declarations
}

fn type_declarations(sources: &[(PathBuf, String)]) -> Vec<(String, String)> {
    struct DeclarationVisitor<'a> {
        path: &'a Path,
        declarations: &'a mut Vec<(String, String)>,
    }

    impl DeclarationVisitor<'_> {
        fn record(&mut self, ident: &syn::Ident) {
            let name = ident.to_string();
            if TYPE_OWNERS.iter().any(|(owned, _)| *owned == name) {
                self.declarations
                    .push((self.path.display().to_string(), name));
            }
        }
    }

    impl<'ast> Visit<'ast> for DeclarationVisitor<'_> {
        fn visit_item_enum(&mut self, item: &'ast syn::ItemEnum) {
            self.record(&item.ident);
            visit::visit_item_enum(self, item);
        }

        fn visit_item_struct(&mut self, item: &'ast syn::ItemStruct) {
            self.record(&item.ident);
            visit::visit_item_struct(self, item);
        }

        fn visit_item_type(&mut self, item: &'ast syn::ItemType) {
            self.record(&item.ident);
            visit::visit_item_type(self, item);
        }
    }

    let mut declarations = Vec::new();
    for (path, source) in sources {
        let syntax = syn::parse_file(source)
            .unwrap_or_else(|error| panic!("parse {}: {error}", path.display()));
        DeclarationVisitor {
            path,
            declarations: &mut declarations,
        }
        .visit_file(&syntax);
    }
    declarations.sort();
    declarations
}

fn public_type_reexports(sources: &[(PathBuf, String)]) -> Vec<String> {
    fn contains_owned_type(tree: &syn::UseTree) -> bool {
        match tree {
            syn::UseTree::Path(path) => contains_owned_type(&path.tree),
            syn::UseTree::Name(name) => TYPE_OWNERS.iter().any(|(owned, _)| name.ident == *owned),
            syn::UseTree::Rename(rename) => {
                TYPE_OWNERS.iter().any(|(owned, _)| rename.ident == *owned)
            }
            syn::UseTree::Group(group) => group.items.iter().any(contains_owned_type),
            syn::UseTree::Glob(_) => false,
        }
    }

    let mut reexports = Vec::new();
    for (path, source) in sources {
        let syntax = syn::parse_file(source)
            .unwrap_or_else(|error| panic!("parse {}: {error}", path.display()));
        if syntax.items.iter().any(|item| {
            matches!(item, syn::Item::Use(item_use)
                if matches!(item_use.vis, syn::Visibility::Public(_))
                    && contains_owned_type(&item_use.tree))
        }) {
            reexports.push(path.display().to_string());
        }
    }
    reexports.sort();
    reexports
}
