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

#[test]
fn connection_graph_operators_resolve_to_their_predefined_identities() {
    let source = r#"
connector Pin
  Real e;
  flow Real f;
end Pin;

model Top
  Pin a;
  Pin b;
  Boolean at_root = Connections.isRoot(a);
  Boolean below = Connections.rooted(a);
equation
  Connections.branch(a, b);
  Connections.root(a);
  Connections.potentialRoot(b, 1);
  connect(a, b);
end Top;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let operator_targets: Vec<String> = tree.definitions.classes["Top"]
        .equations
        .iter()
        .filter_map(|equation| match equation {
            rumoca_ir_ast::Equation::FunctionCall { comp, .. } => Some(comp),
            _ => None,
        })
        .map(|comp| {
            let target = comp
                .target_def_id()
                .expect("a Connections operator names an exact predefined declaration");
            tree.def_map[&target].clone()
        })
        .collect();

    assert_eq!(
        operator_targets,
        vec![
            "Connections.branch".to_string(),
            "Connections.root".to_string(),
            "Connections.potentialRoot".to_string(),
        ],
        "MLS §9.4 graph operators are predefined members of the Connections namespace"
    );
}

#[test]
fn unknown_connections_member_is_still_a_resolve_error() {
    let source = r#"
connector Pin
  Real e;
  flow Real f;
end Pin;

model Top
  Pin a;
equation
  Connections.notAnOperator(a);
end Top;
"#;
    let diagnostics = resolve_parsed_tree_source(source)
        .expect_err("the predefined Connections namespace has a closed member set");

    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ER002")
                && diagnostic
                    .message
                    .contains("unresolved component reference: 'Connections.notAnOperator'")
        }),
        "an operator outside MLS §9.4 must be rejected at Resolve: {diagnostics:?}"
    );
}

#[test]
fn declared_expandable_connector_member_keeps_its_exact_identity() {
    let source = r#"
model Top
  expandable connector Bus
    Real sig;
  end Bus;
  Bus b;
  Real value = b.sig;
end Top;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let reference = binding_reference(&tree, "value");
    let target = reference
        .target_def_id()
        .expect("a declared expandable member is proven at Resolve");
    assert_eq!(tree.def_map[&target], "Top.Bus.sig");
}

#[test]
fn undeclared_expandable_connector_member_is_deferred_not_rejected() {
    // MLS §9.1.3: an expandable connector gains members from the connect
    // equations that name them, so Resolve cannot prove a member absent.
    // Flatten owns that proof once every connection is known.
    let source = r#"
model Top
  expandable connector Bus
  end Bus;
  Bus b1;
  Bus b2;
equation
  connect(b1.sig, b2.sig);
end Top;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let rumoca_ir_ast::Equation::Connect { lhs, rhs } =
        &tree.definitions.classes["Top"].equations[0]
    else {
        panic!("fixture equation must be a connect");
    };
    for reference in [lhs, rhs] {
        assert!(
            reference.root_def_id().is_some(),
            "the expandable bus declaration itself stays exact"
        );
        assert_eq!(
            reference.target_def_id(),
            None,
            "the virtual member identity belongs to instantiation, not Resolve"
        );
    }
}

#[test]
fn undeclared_member_of_a_plain_connector_is_still_a_resolve_error() {
    let source = r#"
model Top
  connector Bus
  end Bus;
  Bus b;
  Real value = b.sig;
end Top;
"#;
    let diagnostics = resolve_parsed_tree_source(source)
        .expect_err("a non-expandable connector has a closed member set");

    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ER002")
                && diagnostic
                    .message
                    .contains("unresolved component reference: 'b.sig'")
        }),
        "only expandable connectors may defer a missing member: {diagnostics:?}"
    );
}

#[test]
fn source_ordered_component_modifiers_carry_exact_identities() {
    // `Component::source_modifications` is the copy every read-only AST walker
    // prefers, so its references must carry the same exact identities as the
    // keyed modification map.
    let source = r#"
package Medium
  function setState_p
    input Real p;
    output Real s;
  algorithm
    s := p;
  end setState_p;
end Medium;

model Inner
  parameter Real state;
end Inner;

model Top
  parameter Real p = 1;
  Inner part(state = Medium.setState_p(p));
end Top;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let component = &tree.definitions.classes["Top"].components["part"];
    let [rumoca_ir_ast::Expression::Modification { value, .. }] =
        component.source_modifications.as_slice()
    else {
        panic!("fixture declares exactly one source-ordered modifier");
    };
    let rumoca_ir_ast::Expression::FunctionCall { comp, args, .. } = value.as_ref() else {
        panic!("the modifier value is a function call");
    };
    let target = comp
        .target_def_id()
        .expect("the source-ordered modifier value names an exact function");
    assert_eq!(tree.def_map[&target], "Medium.setState_p");

    let [rumoca_ir_ast::Expression::ComponentReference(argument)] = args.as_slice() else {
        panic!("the call takes exactly one component-reference argument");
    };
    let argument_target = argument
        .target_def_id()
        .expect("the argument names an exact enclosing declaration");
    assert_eq!(tree.def_map[&argument_target], "Top.p");
}

#[test]
fn source_ordered_redeclare_function_values_carry_exact_identities() {
    // A component modifier's outer target is instance-owned, but the function
    // substituted on its RHS is looked up where the modifier is written (MLS
    // §7.2). Strict reachability walks this source-ordered copy, so the RHS
    // must retain the exact function identity rather than only its spelling.
    let source = r#"
package Shapes
  partial function Characteristic
    input Real length = 1;
    output Real x;
  end Characteristic;

  function defaultCharacteristic
    extends Characteristic;
  algorithm
    x := 0;
  end defaultCharacteristic;

  function rectangle
    extends Characteristic;
  algorithm
    x := length;
  end rectangle;

  model Surface
    replaceable function surfaceCharacteristic = defaultCharacteristic
      constrainedby Characteristic;
  end Surface;

  model Top
    Surface surface(
      redeclare function surfaceCharacteristic = rectangle(length = 2));
  end Top;
end Shapes;
    "#;
    let tree = resolve_tree_source(source).into_inner();
    let component = &tree.definitions.classes["Shapes"].classes["Top"].components["surface"];
    let [
        rumoca_ir_ast::Expression::Modification {
            target: slot,
            value,
            ..
        },
    ] = component.source_modifications.as_slice()
    else {
        panic!("fixture declares exactly one source-ordered modifier");
    };
    assert_eq!(
        slot.target_def_id(),
        None,
        "the modified slot remains instance-owned"
    );
    let rumoca_ir_ast::Expression::ClassModification { target, .. } = value.as_ref() else {
        panic!("redeclare function RHS is a class modification");
    };
    let source_target = target
        .target_def_id()
        .expect("source-ordered redeclare RHS names an exact function");
    let rumoca_ir_ast::Expression::ClassModification {
        target: keyed_target,
        ..
    } = &component.modifications["surfaceCharacteristic"]
    else {
        panic!("keyed redeclare value is a class modification");
    };

    assert_eq!(tree.def_map[&source_target], "Shapes.rectangle");
    assert_eq!(keyed_target.target_def_id(), Some(source_target));
}
