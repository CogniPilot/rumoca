//! Exact declaration identity for qualified component references.

use super::*;

fn binding_reference<'a>(
    tree: &'a ClassTree,
    component: &str,
) -> &'a rumoca_ir_ast::ComponentReference {
    let binding = tree.definitions.classes["Top"].components[component]
        .binding
        .as_ref()
        .expect("fixture component has a binding");
    let rumoca_ir_ast::Expression::ComponentReference(reference) = binding else {
        panic!("fixture binding must be a component reference");
    };
    reference
}

#[test]
fn identical_leaf_names_in_distinct_packages_keep_distinct_exact_targets() {
    let source = r#"
package A
  package Constants
    constant Real eps = 0.125;
  end Constants;
end A;

package B
  package Constants
    constant Real eps = 0.5;
  end Constants;
end B;

model Top
  Real from_a = A.Constants.eps;
  Real from_b = B.Constants.eps;
end Top;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let from_a = binding_reference(&tree, "from_a");
    let from_b = binding_reference(&tree, "from_b");

    let a_root = from_a
        .root_def_id()
        .expect("A reference has a resolved root");
    let b_root = from_b
        .root_def_id()
        .expect("B reference has a resolved root");
    let a_target = from_a
        .target_def_id()
        .expect("A.Constants.eps has an exact target");
    let b_target = from_b
        .target_def_id()
        .expect("B.Constants.eps has an exact target");

    assert_eq!(tree.def_map[&a_root], "A");
    assert_eq!(tree.def_map[&b_root], "B");
    assert_eq!(tree.def_map[&a_target], "A.Constants.eps");
    assert_eq!(tree.def_map[&b_target], "B.Constants.eps");
    assert_ne!(a_root, a_target);
    assert_ne!(b_root, b_target);
    assert_ne!(a_target, b_target, "same spelling cannot cross-bind");

    for (reference, text) in [(from_a, "A.Constants.eps"), (from_b, "B.Constants.eps")] {
        assert_eq!(
            &source[reference.span.start.0..reference.span.end.0],
            text,
            "identity attachment must preserve the exact use occurrence"
        );
    }
}

#[test]
fn unresolved_qualified_tail_cannot_produce_resolve_success() {
    let source = r#"
package A
  package Constants
  end Constants;
end A;

model Top
  Real value = A.Constants.missing;
end Top;
"#;
    let diagnostics = resolve_parsed_tree_source(source)
        .expect_err("a resolved package root cannot prove a missing member");
    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ER002")
                && diagnostic
                    .message
                    .contains("unresolved component reference: 'A.Constants.missing'")
        }),
        "the missing final declaration must be rejected at Resolve: {diagnostics:?}"
    );
}

#[test]
fn component_type_prepass_resolves_forward_declared_three_hop_path() {
    let source = r#"
model Top
  Container holder;
  Real value = holder.member.value;
end Top;

record Leaf
  Real value;
end Leaf;

record Container
  Leaf member;
end Container;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let reference = binding_reference(&tree, "value");
    let root = reference
        .root_def_id()
        .expect("holder has an exact declaration identity");
    let target = reference
        .target_def_id()
        .expect("forward-declared component types prove the full path");

    assert_eq!(tree.def_map[&root], "Top.holder");
    assert_eq!(tree.def_map[&target], "Leaf.value");
    assert_ne!(root, target);
}

#[test]
fn replaceable_declared_type_does_not_certify_instance_specific_tail() {
    let source = r#"
model Base
  replaceable model Medium
    Real original;
  end Medium;
  Medium medium;
end Base;

model Replacement
  Real replacement;
end Replacement;

model Top
  Base holder(redeclare model Medium = Replacement);
  Real value = holder.medium.replacement;
end Top;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(
        result.is_ok(),
        "replaceable fixture should resolve: {:?}",
        result.as_ref().err()
    );
    let tree = result.expect("result was checked above").into_inner();
    let reference = binding_reference(&tree, "value");

    assert_eq!(
        tree.def_map[&reference.root_def_id().expect("holder root is resolved")],
        "Top.holder"
    );
    assert_eq!(
        reference.target_def_id(),
        None,
        "Resolve cannot guess a target across an instance-specific redeclare"
    );
}

#[test]
fn replaceable_package_type_path_retains_only_its_dynamic_root() {
    let source = r#"
package DefaultMedium
  record State
    Real default_value;
  end State;
end DefaultMedium;

package ReplacementMedium
  record State
    Real replacement_value;
  end State;
end ReplacementMedium;

model Holder
  replaceable package Medium = DefaultMedium;
  Medium.State state;
end Holder;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let holder = &tree.definitions.classes["Holder"];
    let medium = &holder.classes["Medium"];
    let state = &holder.components["state"];

    assert_eq!(state.type_name.def_id, medium.def_id);
    assert_eq!(
        state.type_def_id, None,
        "Resolve must not freeze a type member selected by a replaceable package"
    );
}

#[test]
fn concrete_component_missing_tail_is_a_resolve_error() {
    let source = r#"
record Known
  Real present;
end Known;

model Top
  Known holder;
  Real value = holder.missing;
end Top;
"#;
    let diagnostics = resolve_parsed_tree_source(source)
        .expect_err("a concrete component type cannot defer a missing member");

    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ER002")
                && diagnostic
                    .message
                    .contains("unresolved component reference: 'holder.missing'")
        }),
        "the statically missing tail must be rejected at Resolve: {diagnostics:?}"
    );
}

#[test]
fn missing_predefined_enum_literal_is_a_resolve_error() {
    let source = r#"
model Top
  StateSelect value = StateSelect.missing;
end Top;
"#;
    let diagnostics = resolve_parsed_tree_source(source)
        .expect_err("a predefined enum cannot defer a missing literal");

    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ER002")
                && diagnostic
                    .message
                    .contains("unresolved component reference: 'StateSelect.missing'")
        }),
        "the missing predefined literal must be rejected at Resolve: {diagnostics:?}"
    );
}
