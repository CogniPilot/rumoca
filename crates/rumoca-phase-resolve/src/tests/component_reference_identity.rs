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

    let a_root = from_a.def_id.expect("A reference has a resolved root");
    let b_root = from_b.def_id.expect("B reference has a resolved root");
    let a_target = from_a
        .target_def_id
        .expect("A.Constants.eps has an exact target");
    let b_target = from_b
        .target_def_id
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
