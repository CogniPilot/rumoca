//! Reserved `der` callable identity must be structural from parse through Flat.

use std::path::Path;

use quote::ToTokens;
use syn::visit::{self, Visit};

const SEMANTIC_ROOTS: &[&str] = &[
    "crates/rumoca-phase-resolve/src",
    "crates/rumoca-phase-typecheck/src",
    "crates/rumoca-phase-flatten/src",
];

#[derive(Default)]
struct DerSpellingVisitor {
    exact_der_literals: usize,
}

impl<'ast> Visit<'ast> for DerSpellingVisitor {
    fn visit_item_mod(&mut self, node: &'ast syn::ItemMod) {
        if has_test_attribute(&node.attrs) {
            return;
        }
        visit::visit_item_mod(self, node);
    }

    fn visit_item_fn(&mut self, node: &'ast syn::ItemFn) {
        if has_test_attribute(&node.attrs) {
            return;
        }
        visit::visit_item_fn(self, node);
    }

    fn visit_impl_item_fn(&mut self, node: &'ast syn::ImplItemFn) {
        if has_test_attribute(&node.attrs) {
            return;
        }
        visit::visit_impl_item_fn(self, node);
    }

    fn visit_lit_str(&mut self, literal: &'ast syn::LitStr) {
        if literal.value() == "der" {
            self.exact_der_literals += 1;
        }
    }
}

fn has_test_attribute(attributes: &[syn::Attribute]) -> bool {
    attributes.iter().any(|attribute| {
        attribute.path().is_ident("test")
            || (attribute.path().is_ident("cfg")
                && attribute
                    .meta
                    .to_token_stream()
                    .to_string()
                    .contains("test"))
    })
}

fn production_der_spelling_count(source: &str) -> usize {
    let syntax = syn::parse_file(source).expect("production Rust source parses");
    let mut visitor = DerSpellingVisitor::default();
    visitor.visit_file(&syntax);
    visitor.exact_der_literals
}

fn collect_production_sources(root: &Path) -> Vec<std::path::PathBuf> {
    let mut sources = Vec::new();
    super::collect_rs_files(root, &mut sources);
    sources
        .into_iter()
        .filter(|path| !path.components().any(|part| part.as_os_str() == "tests"))
        .collect()
}

#[test]
fn semantic_phases_never_recover_derivative_identity_from_spelling() {
    let root = super::workspace_root();
    let mut offenders = Vec::new();
    for relative in SEMANTIC_ROOTS {
        for path in collect_production_sources(&root.join(relative)) {
            let source = std::fs::read_to_string(&path).expect("read semantic phase source");
            if production_der_spelling_count(&source) != 0 {
                offenders.push(path.strip_prefix(&root).expect("workspace path").to_owned());
            }
        }
    }
    assert!(
        offenders.is_empty(),
        "Resolve, Typecheck, and Flat may consume Expression::DerivativeCall but may not compare or remint its reserved spelling: {offenders:#?}"
    );
}

#[test]
fn shared_builtin_name_lookup_cannot_mint_derivative_identity() {
    assert_eq!(
        rumoca_core::BuiltinFunction::from_name("der"),
        None,
        "only the parser's typed derivative production may mint derivative identity"
    );
    let root = super::workspace_root();
    let core = std::fs::read_to_string(root.join("crates/rumoca-core/src/ir_primitives.rs"))
        .expect("read shared builtin identity owner");
    assert_eq!(
        production_der_spelling_count(&core),
        1,
        "the only exact `der` literal in the identity owner must remain BuiltinFunction::name; from_name must not accept it"
    );
}

#[test]
fn typed_derivative_identity_is_constructed_and_consumed_at_phase_boundaries() {
    let root = super::workspace_root();
    let nodes = std::fs::read_to_string(root.join("crates/rumoca-ir-ast/src/nodes.rs"))
        .expect("read AST nodes");
    let parser = std::fs::read_to_string(root.join("crates/rumoca-phase-parse/src/expressions.rs"))
        .expect("read parser expression conversion");
    let lower = std::fs::read_to_string(root.join("crates/rumoca-phase-flatten/src/ast_lower.rs"))
        .expect("read Flat AST lowering");
    assert!(nodes.contains("DerivativeCall {"));
    assert!(parser.contains("Expression::DerivativeCall {"));
    assert!(lower.contains("ast::Expression::DerivativeCall { args, span }"));
    assert!(lower.contains("BuiltinFunction::Der"));
}

#[test]
fn der_spelling_tombstone_detects_comparison_and_remint_mutations() {
    let comparison = r#"fn dispatch(name: &str) { if name == "der" {} }"#;
    let remint = r#"fn lower() { BuiltinFunction::from_name("der"); }"#;
    assert_eq!(production_der_spelling_count(comparison), 1);
    assert_eq!(production_der_spelling_count(remint), 1);
}
