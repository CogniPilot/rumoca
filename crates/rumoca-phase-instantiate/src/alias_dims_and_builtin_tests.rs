//! Source-text witnesses for alias dimension collection across multiple
//! inheritance (MLS §7.1) and builtin class roots in modifier references
//! (MLS §4.4.4.2).
//!
//! These tests compile Modelica source through Parse → Resolve → Instantiate,
//! so the resolved identities they exercise are the ones the production
//! pipeline mints. In particular, builtin classes such as `StateSelect` carry
//! predefined `DefId`s that never appear in hand-built AST fixtures, and the
//! type-alias dimension walk runs for every component of the instantiated
//! model, not only for components of short-class aliases.

use crate::{InstantiationOutcome, instantiate_model_with_outcome};
use rumoca_ir_ast as ast;
use rumoca_phase_parse::parse_to_ast;
use rumoca_phase_resolve::resolve;

fn successful_overlay(source: &str, model: &str) -> ast::InstanceOverlay {
    let file_name = "<alias_dims_and_builtin_test>";
    let stored = parse_to_ast(source, file_name).expect("parse should succeed");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved = resolve(ast::ParsedTree::new(tree)).expect("resolve should succeed");
    let tree = resolved.inner().clone();
    match instantiate_model_with_outcome(&tree, model) {
        InstantiationOutcome::Success(overlay) => overlay,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("`{model}` unexpectedly needs inner declarations: {missing_inners:?}")
        }
        InstantiationOutcome::Error(error) => {
            panic!("`{model}` should instantiate, got: {error}")
        }
    }
}

fn component_names(overlay: &ast::InstanceOverlay) -> Vec<String> {
    overlay
        .components
        .values()
        .map(|data| data.qualified_name.to_flat_string())
        .collect()
}

fn instance_data<'a>(overlay: &'a ast::InstanceOverlay, name: &str) -> &'a ast::InstanceData {
    overlay
        .components
        .values()
        .find(|data| data.qualified_name.to_flat_string() == name)
        .unwrap_or_else(|| panic!("instance `{name}` should exist"))
}

/// MLS §7.1 permits a long class definition to inherit through several
/// `extends` clauses, and the alias-dimension walk visits every component's
/// class. A multi-extends class terminates the walk exactly like a class with
/// no extends at all; it must never be an error.
///
/// Kills the mutation that restores the old `extends.len() != 1` error in
/// `dims::collect_type_alias_subscripts` (corpus signature EI007).
#[test]
fn component_of_multi_extends_class_instantiates() {
    let source = r#"
        model Base1 Real p; end Base1;
        model Base2 Real q; end Base2;
        model Two
          extends Base1;
          extends Base2;
        end Two;
        model W2 Two t; end W2;
    "#;
    let overlay = successful_overlay(source, "W2");
    let names = component_names(&overlay);
    assert!(
        names.iter().any(|name| name == "t.p"),
        "missing t.p: {names:?}"
    );
    assert!(
        names.iter().any(|name| name == "t.q"),
        "missing t.q: {names:?}"
    );
}

/// Ablation twin for the multi-extends termination: a short-class alias chain
/// (`type Q = Real[4]; type O = Q;`) still contributes its array subscripts to
/// a component of the alias type (MLS §4.6).
///
/// Kills the mutation that terminates the alias walk before following a
/// single-extends short definition (e.g. stopping at every class instead of
/// only at zero/multi-extends classes).
#[test]
fn short_alias_chain_still_collects_array_dimensions() {
    let source = r#"
        model GuardAlias
          type Q = Real[4];
          type O = Q;
          O v;
        equation
          v = {1, 2, 3, 4};
        end GuardAlias;
    "#;
    let overlay = successful_overlay(source, "GuardAlias");
    let data = instance_data(&overlay, "v");
    assert_eq!(data.dims, vec![4], "alias chain must collect Real[4] dims");
}

/// A binding whose reference is rooted at a builtin class (`StateSelect`,
/// MLS §4.4.4.2) classifies the root as a class through the predefined-type
/// authority of `ClassDefIndex`; builtins have no `ClassDef` in the user tree.
///
/// Kills the mutation that removes the builtin classification in
/// `mod_env::exact_modifier_component` and errors on `DefId`s absent from the
/// user class graph (corpus signature EI033).
#[test]
fn builtin_enumeration_binding_in_nested_component_instantiates() {
    let source = r#"
        model InnerC
          parameter StateSelect ss = StateSelect.prefer;
          Real x;
        equation
          x = 1;
        end InnerC;
        model W1g InnerC sub; end W1g;
    "#;
    let overlay = successful_overlay(source, "W1g");
    let names = component_names(&overlay);
    assert!(
        names.iter().any(|name| name == "sub.ss"),
        "missing sub.ss: {names:?}"
    );
}

/// The two-part modifier variant: `sub(ss = StateSelect.prefer)` written in
/// the enclosing class must classify `StateSelect` as a class root, not a
/// sibling component, and land on `sub.ss` as its binding.
#[test]
fn builtin_enumeration_two_part_modifier_instantiates() {
    let source = r#"
        model InnerM
          parameter StateSelect ss = StateSelect.default;
          Real x;
        equation
          x = 1;
        end InnerM;
        model W1m
          InnerM sub(ss = StateSelect.prefer);
        end W1m;
    "#;
    let overlay = successful_overlay(source, "W1m");
    let data = instance_data(&overlay, "sub.ss");
    let Some(ast::Expression::ComponentReference(cref)) = &data.binding else {
        panic!(
            "sub.ss should keep a component-reference binding: {:?}",
            data.binding
        );
    };
    assert_eq!(cref.to_string(), "StateSelect.prefer");
}

/// Attribute-position guard: `x(stateSelect = StateSelect.prefer)` already
/// worked before the builtin classification and must keep working; the
/// attribute route resolves the builtin literal to a typed `StateSelect`
/// value rather than a sibling-component binding.
#[test]
fn builtin_enumeration_attribute_position_keeps_working() {
    let source = r#"
        model InnerA
          Real x(stateSelect = StateSelect.prefer);
        equation
          x = 1;
        end InnerA;
        model W1a InnerA sub; end W1a;
    "#;
    let overlay = successful_overlay(source, "W1a");
    let data = instance_data(&overlay, "sub.x");
    assert_eq!(data.state_select, rumoca_core::StateSelect::Prefer);
}
