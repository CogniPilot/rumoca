//! Enumeration-literal recognition for instantiation-time evaluation.
//!
//! MLS §4.8.5.1 writes an enumeration literal as `EnumTypeName.literal`, and the
//! instantiate evaluator compares enumeration values through their qualified
//! spelling. It therefore needs the rendered path of such a reference — but only
//! when the reference *is* an enumeration literal.
//!
//! Rendering any unresolved dotted name as its own value is what SPEC_0008
//! prohibits ("substituting … any other invented value") and what SPEC_0032 §3
//! means by "strings are not semantics": it made `Medium.ThermoStates`, an
//! unresolvable package alias, answer a confident `false` when compared against
//! `IndependentVariables.ph`. This module answers the question from the class
//! tree instead — the prefix must denote an enumeration type that declares the
//! trailing literal — and works from `ComponentReference::parts` so the
//! hierarchy comes from the AST rather than from re-splitting display text.

use super::{ast, component_ref_to_dotted_no_subscripts, find_class_in_tree};

/// MLS §4.4.4.2 / §8.3.7 predefined enumeration types. These have no `ClassDef`
/// in the tree, so their literals are listed here.
const PREDEFINED_ENUMERATIONS: &[(&str, &[&str])] = &[
    (
        "StateSelect",
        &["never", "avoid", "default", "prefer", "always"],
    ),
    ("AssertionLevel", &["warning", "error"]),
];

/// The rendered path of `comp_ref`, but only when it denotes an enumeration
/// literal (MLS §4.8.5.1). Returns `None` for every other reference, including
/// ones this phase simply failed to resolve.
pub(super) fn enumeration_literal_path(
    comp_ref: &ast::ComponentReference,
    tree: &ast::ClassTree,
) -> Option<String> {
    let dotted = component_ref_to_dotted_no_subscripts(comp_ref)?;
    let parts: Vec<&str> = comp_ref
        .parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect();
    let (literal, type_parts) = parts.split_last()?;
    if type_parts.is_empty() {
        return None;
    }

    (predefined_enumeration_declares(type_parts, literal)
        || enumeration_class_declares(tree, type_parts, literal))
    .then_some(dotted)
}

fn predefined_enumeration_declares(type_parts: &[&str], literal: &str) -> bool {
    let Some(type_name) = type_parts.last() else {
        return false;
    };
    PREDEFINED_ENUMERATIONS
        .iter()
        .any(|(name, literals)| name == type_name && literals.contains(&literal))
}

fn class_declares_literal(class: &ast::ClassDef, literal: &str) -> bool {
    class
        .enum_literals
        .iter()
        .any(|declared| declared.ident.text.as_ref() == literal)
}

fn enumeration_class_declares(tree: &ast::ClassTree, type_parts: &[&str], literal: &str) -> bool {
    // MLS §5.3: the enumeration type may be named relative to any enclosing
    // scope, so the written prefix can be a trailing fragment of its qualified
    // name. Climb one leading segment at a time.
    for start in 0..type_parts.len() {
        let candidate = type_parts[start..].join(".");
        let class = tree
            .get_class_by_qualified_name(&candidate)
            .or_else(|| find_class_in_tree(tree, &candidate));
        if let Some(class) = class
            && class_declares_literal(class, literal)
        {
            return true;
        }
    }
    enumeration_declared_under_leaf_name(tree, type_parts, literal)
}

/// Last resort: an enumeration class whose own (leaf) name is the reference's
/// type segment and which declares `literal`.
///
/// Where such a class lives does not change the answer: the caller compares
/// enumeration values by type-leaf plus literal, so any declaring class yields
/// the same comparison. What this establishes is only that the reference is an
/// enumeration literal at all, rather than a name of unknown meaning.
fn enumeration_declared_under_leaf_name(
    tree: &ast::ClassTree,
    type_parts: &[&str],
    literal: &str,
) -> bool {
    let Some(type_name) = type_parts.last() else {
        return false;
    };
    let suffix = format!(".{type_name}");
    tree.name_map
        .keys()
        .filter(|qualified| qualified.as_str() == *type_name || qualified.ends_with(&suffix))
        .filter_map(|qualified| tree.get_class_by_qualified_name(qualified))
        .any(|class| class_declares_literal(class, literal))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    fn comp_ref(parts: &[&str]) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: parts
                .iter()
                .map(|name| ast::ComponentRefPart {
                    ident: rumoca_core::Token {
                        text: Arc::from(*name),
                        ..Default::default()
                    },
                    subs: None,
                })
                .collect(),
            def_id: None,
            target_def_id: None,
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn enum_class(name: &str, literals: &[&str]) -> ast::ClassDef {
        ast::ClassDef {
            name: rumoca_core::Token {
                text: Arc::from(name),
                ..Default::default()
            },
            class_type: rumoca_core::ClassType::Type,
            enum_literals: literals
                .iter()
                .map(|literal| ast::EnumLiteral {
                    ident: rumoca_core::Token {
                        text: Arc::from(*literal),
                        ..Default::default()
                    },
                    description: Vec::new(),
                })
                .collect(),
            ..Default::default()
        }
    }

    fn tree_with_enum() -> ast::ClassTree {
        let mut tree = ast::ClassTree::default();
        tree.definitions.classes.insert(
            "IndependentVariables".to_string(),
            enum_class("IndependentVariables", &["T", "pT", "ph", "phX", "pTX"]),
        );
        tree
    }

    #[test]
    fn declared_enumeration_literal_yields_its_path() {
        let tree = tree_with_enum();
        assert_eq!(
            enumeration_literal_path(&comp_ref(&["IndependentVariables", "ph"]), &tree),
            Some("IndependentVariables.ph".to_string())
        );
    }

    #[test]
    fn unresolved_package_alias_field_is_not_a_literal() {
        let tree = tree_with_enum();
        assert_eq!(
            enumeration_literal_path(&comp_ref(&["Medium", "ThermoStates"]), &tree),
            None
        );
    }

    #[test]
    fn undeclared_literal_of_a_known_enumeration_is_rejected() {
        let tree = tree_with_enum();
        assert_eq!(
            enumeration_literal_path(&comp_ref(&["IndependentVariables", "pS"]), &tree),
            None
        );
    }

    #[test]
    fn predefined_enumeration_literals_are_recognized() {
        let tree = ast::ClassTree::default();
        assert_eq!(
            enumeration_literal_path(&comp_ref(&["StateSelect", "always"]), &tree),
            Some("StateSelect.always".to_string())
        );
        assert_eq!(
            enumeration_literal_path(&comp_ref(&["AssertionLevel", "warning"]), &tree),
            Some("AssertionLevel.warning".to_string())
        );
        assert_eq!(
            enumeration_literal_path(&comp_ref(&["StateSelect", "sometimes"]), &tree),
            None
        );
    }

    #[test]
    fn single_part_reference_is_never_a_literal() {
        let tree = tree_with_enum();
        assert_eq!(enumeration_literal_path(&comp_ref(&["ph"]), &tree), None);
    }
}
