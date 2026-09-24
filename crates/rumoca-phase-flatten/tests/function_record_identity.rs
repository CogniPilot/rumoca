//! Source driven proof for deferred record members in selected callables.

use rumoca_core::VarName;
use rumoca_ir_ast as ast;
use rumoca_phase_flatten::FlattenError;

const SOURCE: &str = r#"
package P
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

  package Middle
    extends Base;
    redeclare replaceable record extends State
      Real M;
    end State;
  end Middle;

  package Concrete
    extends Middle;
    redeclare record extends State
      Real T;
    end State;
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

const SAME_NAME_WRONG_ID_SOURCE: &str = r#"
package P
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
    extends Base;
    record State
      Real T;
    end State;
    record Unrelated
      extends State;
      Real T;
    end Unrelated;
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

fn prepare(source: &str) -> (ast::ClassTree, ast::InstanceOverlay) {
    let file = "<function_record_identity>";
    let stored = rumoca_phase_parse::parse_to_ast(source, file).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file, source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let ast::InstancedTree { tree, mut overlay } =
        rumoca_phase_instantiate::instantiate(resolved, "P.Root").expect("source instantiates");
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, "P.Root")
        .expect("source typechecks");
    (tree, overlay)
}

#[test]
fn selected_record_field_uses_real_source_def_id() {
    let (tree, overlay) = prepare(SOURCE);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let concrete_state = class_index
        .get_by_qualified_name("P.Concrete.State")
        .expect("selected record class");
    let base_state = class_index
        .get_by_qualified_name("P.Base.State")
        .expect("generic record class");
    let selected_field = concrete_state
        .components
        .get("T")
        .and_then(|component| component.def_id)
        .expect("selected T declaration identity");
    assert_ne!(concrete_state.def_id, base_state.def_id);

    let flat = rumoca_phase_flatten::flatten_ref(&tree, &overlay, "P.Root")
        .expect("selected record field must flatten");
    let function = flat
        .functions
        .get(&VarName::new("Medium.f"))
        .expect("selected callable");
    let selected_callable = class_index
        .get_by_qualified_name("P.Concrete.f")
        .expect("selected callable class");
    assert_eq!(
        function.def_id, selected_callable.def_id,
        "the flattened callable keeps the selected source callable DefId"
    );
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
    assert!(
        !field_ids.is_empty(),
        "fixture must retain state.T field access"
    );
    assert!(field_ids.iter().all(|field| *field == selected_field));
}

#[test]
fn missing_selected_record_field_rejects_rendered_name_recovery() {
    let source = SOURCE.replace("      Real T;\n", "");
    let (tree, overlay) = prepare(&source);
    let error = rumoca_phase_flatten::flatten_ref(&tree, &overlay, "P.Root")
        .expect_err("a missing selected field must remain unresolved");
    assert!(matches!(
        error,
        FlattenError::MissingFlatVariableIdentity { name, .. } if name == "T"
    ));
}

const UNRELATED_DERIVED_RECORD_SOURCE: &str = r#"
package P
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
    extends Base;
    record Unrelated
      extends Base.State;
      Real T;
    end Unrelated;
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

const DIAMOND_RECORD_SOURCE: &str = r#"
package P
  partial package Base
    replaceable record State
      Real T;
    end State;

    replaceable partial function f
      input State state;
      output Real y;
    algorithm
      y := state.T;
    end f;
  end Base;

  package Left
    extends Base;
  end Left;

  package Right
    extends Base;
  end Right;

  package Concrete
    extends Left;
    extends Right;
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

const ENCLOSING_ARRAY_MEMBER_SOURCE: &str = r#"
package P
  partial package Base
    replaceable record State
    end State;
    constant State[1] states = {State()};
    model Properties
      Real y = states[1].T;
    end Properties;
  end Base;

  package Concrete
    extends Base;
    redeclare record extends State
      Real T = 300;
    end State;
  end Concrete;

  model Holder
    replaceable package Medium = Base constrainedby Base;
    Medium.Properties properties;
  end Holder;

  model Root
    Holder h(redeclare package Medium = Concrete);
  end Root;
end P;
"#;

const ENCLOSING_ARRAY_SAME_NAME_NEGATIVE_SOURCE: &str = r#"
package P
  partial package Base
    replaceable record State
    end State;
    constant State[1] states = {State()};
    model Properties
      Real y = states[1].T;
    end Properties;
  end Base;

  package Concrete
    extends Base;
    record State
      Real T = 300;
    end State;
  end Concrete;

  model Holder
    replaceable package Medium = Base constrainedby Base;
    Medium.Properties properties;
  end Holder;

  model Root
    Holder h(redeclare package Medium = Concrete);
  end Root;
end P;
"#;

#[test]
fn same_name_wrong_identity_does_not_fill_formal_record_slot() {
    let (tree, overlay) = prepare(SAME_NAME_WRONG_ID_SOURCE);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let base_state = class_index
        .get_by_qualified_name("P.Base.State")
        .expect("generic record class");
    let wrong_same_name = class_index
        .get_by_qualified_name("P.Concrete.State")
        .expect("same-name wrong-identity record");
    assert_ne!(wrong_same_name.def_id, base_state.def_id);
    assert!(
        wrong_same_name.components.contains_key("T"),
        "the wrong-identity same-name record must offer a tempting T field"
    );
    let error = rumoca_phase_flatten::flatten_ref(&tree, &overlay, "P.Root")
        .expect_err("a same-name record with the wrong identity must not prove state.T");
    assert!(matches!(
        error,
        FlattenError::MissingFlatVariableIdentity { name, .. } if name == "T"
    ));
}

#[test]
fn unrelated_derived_record_does_not_fill_formal_record_slot() {
    let (tree, overlay) = prepare(UNRELATED_DERIVED_RECORD_SOURCE);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let base_state = class_index
        .get_by_qualified_name("P.Base.State")
        .expect("generic record class");
    let unrelated = class_index
        .get_by_qualified_name("P.Concrete.Unrelated")
        .expect("unrelated derived record");
    assert!(
        unrelated
            .components
            .get("T")
            .and_then(|component| component.def_id)
            .is_some(),
        "the unrelated record must have a real T declaration"
    );
    assert_eq!(
        unrelated
            .extends
            .first()
            .and_then(|extend| extend.base_def_id),
        base_state.def_id,
        "the negative fixture must extend the formal record by exact identity"
    );
    assert!(!unrelated.is_redeclare);
    assert_eq!(unrelated.redeclare_target_def_id, None);

    let error = rumoca_phase_flatten::flatten_ref(&tree, &overlay, "P.Root")
        .expect_err("an unrelated derived record must not prove state.T");
    assert!(matches!(
        error,
        FlattenError::MissingFlatVariableIdentity { name, .. } if name == "T"
    ));
}

#[test]
fn enclosing_array_member_rejects_same_name_record_identity_during_instantiation() {
    let file = "<function_record_identity_negative>";
    let stored = rumoca_phase_parse::parse_to_ast(ENCLOSING_ARRAY_SAME_NAME_NEGATIVE_SOURCE, file)
        .expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map
        .add(file, ENCLOSING_ARRAY_SAME_NAME_NEGATIVE_SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let tree = resolved.into_inner();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let base_state = class_index
        .get_by_qualified_name("P.Base.State")
        .expect("formal record class");
    let wrong_same_name = class_index
        .get_by_qualified_name("P.Concrete.State")
        .expect("unselected same-name record");
    assert_ne!(wrong_same_name.def_id, base_state.def_id);
    assert!(wrong_same_name.components.contains_key("T"));

    let error = rumoca_phase_instantiate::instantiate_model(&tree, "P.Root")
        .expect_err("the enclosing constant must retain the formal record identity");
    assert!(
        error
            .to_string()
            .contains("selected redeclare class has no such member")
    );
}

#[test]
fn diamond_inheritance_accepts_one_shared_record_field_identity() {
    let (tree, overlay) = prepare(DIAMOND_RECORD_SOURCE);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let shared_field = class_index
        .get_by_qualified_name("P.Base.State")
        .and_then(|state| state.components.get("T"))
        .and_then(|field| field.def_id)
        .expect("shared diamond field identity");

    let flat = rumoca_phase_flatten::flatten_ref(&tree, &overlay, "P.Root")
        .expect("a diamond with one shared field declaration must flatten");
    let function = flat
        .functions
        .get(&VarName::new("Medium.f"))
        .expect("selected diamond callable");
    let field_ids = function
        .inputs
        .iter()
        .filter(|input| input.name == "state_T")
        .filter_map(|input| input.def_id)
        .collect::<Vec<_>>();
    assert_eq!(field_ids, vec![shared_field]);
}

#[test]
fn enclosing_array_member_keeps_exact_source_and_selected_type_identity() {
    let (tree, overlay) = prepare(ENCLOSING_ARRAY_MEMBER_SOURCE);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let base = class_index
        .get_by_qualified_name("P.Base")
        .expect("base package");
    let states = base
        .components
        .get("states")
        .expect("source array declaration");
    let source_root = states.def_id.expect("source array DefId");
    let selected_state = class_index
        .get_by_qualified_name("P.Concrete.State")
        .expect("selected state");
    let selected_field = selected_state
        .components
        .get("T")
        .and_then(|component| component.def_id)
        .expect("selected field DefId");

    let equation_references = overlay
        .classes
        .values()
        .flat_map(|class| class.equations.iter())
        .filter_map(|equation| match &equation.equation {
            ast::Equation::Simple { lhs, rhs } => Some([lhs, rhs]),
            _ => None,
        })
        .flatten()
        .flat_map(ast::collect_component_refs)
        .collect::<Vec<_>>();
    let binding_references = overlay
        .components
        .values()
        .filter_map(|component| component.binding.as_ref())
        .flat_map(ast::collect_component_refs)
        .collect::<Vec<_>>();
    let references = equation_references
        .into_iter()
        .chain(binding_references)
        .filter(|reference| reference.to_string() == "states[1].T")
        .collect::<Vec<_>>();
    assert!(
        !references.is_empty(),
        "fixture must retain the array member reference"
    );
    assert!(references.iter().all(|reference| {
        reference.root_def_id() == Some(source_root)
            && reference.target_def_id() == Some(selected_field)
    }));
}
