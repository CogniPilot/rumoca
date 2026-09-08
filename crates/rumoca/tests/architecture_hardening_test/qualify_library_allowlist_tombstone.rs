//! Tombstone: qualification must never gate on a hard-coded library-name list.
//!
//! History. `rumoca-phase-flatten`'s qualifier once carried a function,
//! `is_likely_fully_qualified`, that decided whether a component reference was
//! "already fully qualified" by testing its first identifier against a hard
//! array of top-level library spellings (`Modelica`, `Buildings`, `OpenIPSL`,
//! `PowerSystems`, `ThermoPower`, ...). It was wrong: it changed rendered
//! display text based on spelling and misclassified a local component whose
//! name collided with a listed library (for example, `Real Buildings;`). The
//! semantic package-rooted value cases do not need that branch: package
//! constants and enumeration literals are materialized from exact `DefId` and
//! structured member-path evidence before a later phase could consume their
//! display text. Function references use a separate route, while class and
//! type references do not survive as runtime values.
//!
//! Doctrine. SPEC_0001 §Identity Domains and §Semantic Identity Keys: a
//! rendered name is "never compiler semantic identity"; "component-reference
//! display text ... are not valid identity fields"; "compiler semantic
//! identity is resolved-id based." A list of library spellings is exactly the
//! rendered-name-as-identity move that doctrine forbids.
//!
//! Guarantee split.
//!
//! * Type-level (already enforced by the compiler, not by this test): package
//!   constants and enumeration literals are selected by resolved `DefId` and
//!   structured member path, then substituted before downstream consumers can
//!   interpret a qualifier display string. Function references follow the
//!   separate function-qualification authority, and class/type references are
//!   erased. The removed branch could therefore alter presentation but could
//!   not select a different semantic declaration.
//!
//! * Textual (this test): the narrow *source shape* that reintroduces the
//!   defect — a hard-coded list of library-name string literals inside the
//!   qualifier, or the resurrection of the named predicate — is a drift
//!   backstop. It cannot prove the absence of every conceivable spelling
//!   heuristic (that would be a semantic claim source scanning cannot make);
//!   it rejects the exact construction that was removed so it cannot silently
//!   return.
//!
//! Scope. Deliberately narrow. It scans only the production qualifier source
//! and flags (a) a function named `is_likely_fully_qualified` and (b) an array
//! literal of two or more library-shaped string identifiers. A blanket ban on
//! string comparison in the qualifier would rot: the qualifier legitimately
//! compares interned identifiers against `locals`, resolves import aliases, and
//! matches the MLS-global spellings `StateSelect`/`AssertionLevel` (a
//! `matches!` pattern, not an array). Those are untouched.

use syn::visit::{self, Visit};

const QUALIFIER_SOURCE: &str = "crates/rumoca-phase-flatten/src/qualify.rs";

/// The predicate name that carried the removed allowlist. Its literal return
/// is banned even if the backing array is spelled differently.
const BANNED_PREDICATE: &str = "is_likely_fully_qualified";

/// A string literal is "library-shaped" when it is a bare Modelica top-level
/// identifier: an uppercase initial followed by identifier characters, with no
/// dot, space, or other punctuation. Two or more of these gathered in one
/// array literal is the allowlist shape and nothing else.
fn is_library_shaped_identifier(text: &str) -> bool {
    let mut chars = text.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_ascii_uppercase() {
        return false;
    }
    chars.all(|character| character.is_ascii_alphanumeric() || character == '_')
}

#[derive(Default)]
struct AllowlistShapeVisitor {
    banned_predicate: Vec<String>,
    library_arrays: Vec<String>,
}

impl<'ast> Visit<'ast> for AllowlistShapeVisitor {
    fn visit_item_fn(&mut self, node: &'ast syn::ItemFn) {
        // A `#[cfg(test)]`-gated function is not part of the production
        // qualification decision.
        if super::attributes_require_test(&node.attrs) {
            return;
        }
        if node.sig.ident == BANNED_PREDICATE {
            self.banned_predicate.push(format!("fn {}", node.sig.ident));
        }
        visit::visit_item_fn(self, node);
    }

    fn visit_expr_array(&mut self, node: &'ast syn::ExprArray) {
        let literals: Vec<String> = node
            .elems
            .iter()
            .filter_map(|element| match element {
                syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(literal),
                    ..
                }) => Some(literal.value()),
                _ => None,
            })
            .collect();

        // Every element must be a string literal, and at least two of them
        // must be library-shaped identifiers, for this to be the allowlist.
        if literals.len() == node.elems.len() {
            let library_named = literals
                .iter()
                .filter(|text| is_library_shaped_identifier(text))
                .count();
            if library_named >= 2 {
                self.library_arrays
                    .push(format!("[{}]", literals.join(", ")));
            }
        }
        visit::visit_expr_array(self, node);
    }
}

fn qualifier_items_excluding_tests(source: &syn::File) -> Vec<syn::Item> {
    source
        .items
        .iter()
        .filter(|item| {
            let attributes: &[syn::Attribute] = match item {
                syn::Item::Fn(item) => &item.attrs,
                syn::Item::Mod(item) => &item.attrs,
                syn::Item::Use(item) => &item.attrs,
                syn::Item::Impl(item) => &item.attrs,
                syn::Item::Const(item) => &item.attrs,
                syn::Item::Static(item) => &item.attrs,
                _ => &[],
            };
            !super::attributes_require_test(attributes)
        })
        .cloned()
        .collect()
}

#[test]
fn qualifier_carries_no_hard_coded_library_name_allowlist() {
    let path = super::workspace_root().join(QUALIFIER_SOURCE);
    let source = std::fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("read {QUALIFIER_SOURCE}: {error}"));
    let file = syn::parse_file(&source)
        .unwrap_or_else(|error| panic!("parse {QUALIFIER_SOURCE}: {error}"));

    let mut visitor = AllowlistShapeVisitor::default();
    for item in qualifier_items_excluding_tests(&file) {
        visitor.visit_item(&item);
    }

    assert!(
        visitor.banned_predicate.is_empty(),
        "{QUALIFIER_SOURCE} reintroduced the removed library-allowlist predicate \
         `{BANNED_PREDICATE}`: {:?}. Qualification identity is resolved-`DefId` \
         based (SPEC_0001); it must not gate on a source spelling.",
        visitor.banned_predicate
    );
    assert!(
        visitor.library_arrays.is_empty(),
        "{QUALIFIER_SOURCE} reintroduced a hard-coded library-name allowlist \
         (an array of library-shaped string literals): {:?}. A top-level \
         package's spelling is not its identity (SPEC_0001); a reference is \
         qualified from its resolved `DefId`, so no such list may decide it.",
        visitor.library_arrays
    );
}
