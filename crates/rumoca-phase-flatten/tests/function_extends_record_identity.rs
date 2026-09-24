//! Source regression for a record redeclared in an `extends` modifier.
//!
//! The selected record is not a nested class of the derived package.  Its
//! source identity is carried by the modifier on the `extends` clause:
//! `extends Base(redeclare record State = Common.ConcreteState)`.

use rumoca_core::VarName;
use rumoca_ir_ast as ast;
use rumoca_phase_flatten::FlattenError;

const SOURCE: &str = r#"
package P
  package Common
    record ConcreteState
      extends Base.State;
      Real T;
    end ConcreteState;
  end Common;

  partial package Base
    replaceable record State
    end State;

    replaceable partial function f
      input State state;
      output Real y;
    algorithm
      y := state.T;
    end f;
  end Base;

  package Concrete
    extends Base(redeclare record State = Common.ConcreteState);
    redeclare function extends f
    algorithm
      y := state.T;
    end f;
  end Concrete;

  model Holder
    replaceable package Medium = Base constrainedby Base;
    Medium.State state;
    Real y = Medium.f(state);
  end Holder;

  model Root
    Holder h(redeclare package Medium = Concrete);
  end Root;
end P;
"#;

const WRONG_SLOT_SOURCE: &str = r#"
package P
  package Common
    record ConcreteState
      extends Base.State;
      Real T;
    end ConcreteState;
  end Common;

  partial package Base
    replaceable record State
    end State;

    replaceable partial function f
      input State state;
      output Real y;
    algorithm
      y := state.T;
    end f;
  end Base;

  package Concrete
    extends Base(redeclare record Unrelated = Common.ConcreteState);
    redeclare function extends f
    algorithm
      y := state.T;
    end f;
  end Concrete;

  model Holder
    replaceable package Medium = Base constrainedby Base;
    Medium.State state;
    Real y = Medium.f(state);
  end Holder;

  model Root
    Holder h(redeclare package Medium = Concrete);
  end Root;
end P;
"#;

fn resolved(source: &str, file: &str) -> ast::ClassTree {
    let parsed = rumoca_phase_parse::parse_to_ast(source, file).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(parsed);
    tree.source_map.add(file, source);
    rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
        .expect("source resolves")
        .into_inner()
}

fn prepared(
    source: &str,
    file: &str,
) -> Result<(ast::ClassTree, ast::InstanceOverlay), Box<dyn std::error::Error>> {
    let parsed =
        rumoca_phase_parse::parse_to_ast(source, file).map_err(|error| format!("{error:?}"))?;
    let mut tree = ast::ClassTree::from_parsed(parsed);
    tree.source_map.add(file, source);
    let resolved = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
        .map_err(|error| format!("{error:?}"))?;
    let instanced = rumoca_phase_instantiate::instantiate(resolved, "P.Root")
        .map_err(|error| format!("{error:?}"))?;
    let ast::InstancedTree { tree, mut overlay } = instanced;
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, "P.Root")
        .map_err(|error| format!("{error:?}"))?;
    Ok((tree, overlay))
}

#[test]
fn extends_redeclare_preserves_exact_record_slot_and_value_identity() {
    let tree = resolved(SOURCE, "<function_extends_record_identity>");
    let concrete = tree
        .get_class_by_qualified_name("P.Concrete")
        .expect("derived package");
    let base_slot = tree
        .get_class_by_qualified_name("P.Base.State")
        .and_then(|class| class.def_id)
        .expect("formal record slot DefId");
    let selected = tree
        .get_class_by_qualified_name("P.Common.ConcreteState")
        .and_then(|class| class.def_id)
        .expect("selected record DefId");

    let [extend] = concrete.extends.as_slice() else {
        panic!("fixture has one extends clause");
    };
    let [modification] = extend.modifications.as_slice() else {
        panic!("fixture has one extends redeclare");
    };
    assert!(modification.redeclare);
    let ast::Expression::Modification { target, value, .. } = &modification.expr else {
        panic!("record redeclare is a named modification");
    };
    assert_eq!(target.target_def_id(), Some(base_slot));
    let ast::Expression::ClassModification {
        target: selected_ref,
        ..
    } = value.as_ref()
    else {
        panic!("record redeclare RHS is a class modification");
    };
    assert_eq!(selected_ref.target_def_id(), Some(selected));
    assert_ne!(base_slot, selected);

    let (tree, overlay) =
        prepared(SOURCE, "<function_extends_record_identity>").expect("fixture must reach flatten");
    let selected_field = tree
        .get_class_by_qualified_name("P.Common.ConcreteState")
        .and_then(|class| class.components.get("T"))
        .and_then(|component| component.def_id)
        .expect("selected T declaration identity");
    let flat = rumoca_phase_flatten::flatten_ref(&tree, &overlay, "P.Root")
        .expect("extends redeclare must select the concrete record");
    let function = flat
        .functions
        .get(&VarName::new("Medium.f"))
        .expect("selected callable");
    let field_ids = function
        .body
        .iter()
        .filter_map(|statement| match statement {
            rumoca_core::Statement::Assignment {
                value: rumoca_core::Expression::FieldAccess { field_def_id, .. },
                ..
            } => Some(*field_def_id),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert!(!field_ids.is_empty());
    assert!(field_ids.iter().all(|field| *field == selected_field));
}

#[test]
fn missing_extends_rhs_identity_rejects_rendered_name_recovery() {
    let (mut tree, overlay) = prepared(SOURCE, "<function_extends_record_identity_missing_rhs>")
        .expect("fixture must reach flatten");
    let package = tree.definitions.classes.get_mut("P").expect("root package");
    let concrete = package
        .classes
        .get_mut("Concrete")
        .expect("derived package");
    let modification = concrete.extends[0]
        .modifications
        .first_mut()
        .expect("extends redeclare");
    let ast::Expression::Modification { value, .. } = &mut modification.expr else {
        panic!("record redeclare is a named modification");
    };
    let ast::Expression::ClassModification { target, .. } = std::sync::Arc::make_mut(value) else {
        panic!("record redeclare RHS is a class modification");
    };
    target.set_target_def_id(None);

    let error = rumoca_phase_flatten::flatten_ref(&tree, &overlay, "P.Root")
        .expect_err("a missing RHS identity must not fall back to its rendered name");
    assert!(matches!(
        error,
        FlattenError::MissingFlatVariableIdentity { name, .. } if name == "T"
    ));
}

#[test]
fn extends_redeclare_of_unrelated_slot_cannot_select_record_value() {
    let tree = resolved(
        WRONG_SLOT_SOURCE,
        "<function_extends_record_identity_wrong_slot>",
    );
    let concrete = tree
        .get_class_by_qualified_name("P.Concrete")
        .expect("derived package");
    let state_slot = tree
        .get_class_by_qualified_name("P.Base.State")
        .and_then(|class| class.def_id)
        .expect("formal record slot DefId");
    let ast::Expression::Modification { target, .. } = &concrete.extends[0].modifications[0].expr
    else {
        panic!("record redeclare is a named modification");
    };
    assert_ne!(
        target.target_def_id(),
        Some(state_slot),
        "the negative fixture must target a different inherited slot"
    );

    let (tree, overlay) = prepared(
        WRONG_SLOT_SOURCE,
        "<function_extends_record_identity_wrong_slot>",
    )
    .expect("the wrong-slot fixture must reach flatten");
    let error = rumoca_phase_flatten::flatten_ref(&tree, &overlay, "P.Root")
        .expect_err("an unrelated extends redeclare must not be accepted as State");
    assert!(matches!(
        error,
        FlattenError::MissingFlatVariableIdentity { name, .. } if name == "T"
    ));
}
