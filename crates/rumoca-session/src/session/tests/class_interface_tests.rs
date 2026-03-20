use super::*;

#[test]
fn class_interface_queries_collect_imports_and_local_members() {
    let mut session = Session::default();
    let source = r#"
within Demo;
model Outer
  import Alias = Lib.Helper;
  import Lib.Components.Gain;
  import Lib.Components.{Limiter};
  import Lib.Components.*;
  extends Base;
  Gain gain;
  replaceable Alias helper constrainedby Lib.HelperBase;
  partial model Inner
  end Inner;
end Outer;
"#;

    session
        .add_document("input.mo", source)
        .expect("test source should parse");

    let class_interface = session
        .class_interface_query("input.mo", "Demo.Outer")
        .expect("class interface should exist");
    let imports = class_interface.import_map();
    assert_eq!(
        imports.explicit_bindings(),
        &[
            ("Alias".to_string(), "Lib.Helper".to_string()),
            ("Gain".to_string(), "Lib.Components.Gain".to_string()),
            ("Limiter".to_string(), "Lib.Components.Limiter".to_string()),
        ],
        "import map should preserve explicit import bindings in source order"
    );
    assert_eq!(
        imports.wildcard_paths(),
        &["Lib.Components".to_string()],
        "import map should preserve wildcard import prefixes"
    );
    assert_eq!(
        imports.resolve_candidates("Gain"),
        vec!["Lib.Components.Gain".to_string()],
        "import candidate resolution should deduplicate explicit and wildcard matches"
    );

    assert_eq!(class_interface.component_type("gain"), Some("Gain"));
    assert_eq!(class_interface.component_type("helper"), Some("Alias"));
    assert_eq!(
        class_interface
            .nested_class_interfaces()
            .iter()
            .map(|nested_class| nested_class.name().to_string())
            .collect::<Vec<_>>(),
        vec!["Inner".to_string()]
    );
    assert_eq!(class_interface.extends_bases(), &["Base".to_string()]);
    let helper = class_interface
        .component_interface("helper")
        .expect("helper component interface should exist");
    assert!(helper.is_replaceable());
    assert_eq!(helper.constrainedby(), Some("Lib.HelperBase"));
    let inner = class_interface
        .nested_class_interfaces()
        .first()
        .expect("nested class header should exist");
    assert!(inner.is_partial());
}

#[test]
fn class_type_resolution_candidates_query_uses_class_interface_scope() {
    let mut session = Session::default();
    let source = r#"
within Demo;
model Outer
  import Alias = Lib.Helper;
  import Lib.Components.Gain;
  import Lib.Components.*;
  model Inner
  end Inner;
  Gain gain;
  Alias helper;
end Outer;
"#;

    session
        .add_document("input.mo", source)
        .expect("test source should parse");

    assert_eq!(
        session.class_type_resolution_candidates_query("input.mo", "Demo.Outer", "Alias"),
        vec![
            "Lib.Helper".to_string(),
            "Lib.Components.Alias".to_string(),
            "Alias".to_string(),
        ],
        "type candidates should preserve explicit import, wildcard import, and raw-name fallback order"
    );
    assert_eq!(
        session.class_type_resolution_candidates_query("input.mo", "Demo.Outer", "Gain"),
        vec!["Lib.Components.Gain".to_string(), "Gain".to_string()],
        "type candidates should include imported class names before the raw name"
    );
    assert_eq!(
        session.class_type_resolution_candidates_query("input.mo", "Demo.Outer", "Inner"),
        vec![
            "Demo.Outer.Inner".to_string(),
            "Lib.Components.Inner".to_string(),
            "Inner".to_string(),
        ],
        "type candidates should keep nested classes ahead of wildcard imports and the raw fallback"
    );
    assert_eq!(
        session.class_type_resolution_candidates_query(
            "input.mo",
            "Demo.Outer",
            "Lib.Components.Gain",
        ),
        vec!["Lib.Components.Gain".to_string()],
        "qualified names should stay exact"
    );
}

#[test]
fn class_local_completion_items_query_uses_class_interface_scope() {
    let mut session = Session::default();
    let source = r#"
within Demo;
model Outer
  parameter Real kp;
  input Real u;
  Real x;
  model Inner
  end Inner;
end Outer;
"#;

    session
        .add_document("input.mo", source)
        .expect("test source should parse");

    let items = session.class_local_completion_items_query("input.mo", "Demo.Outer");
    assert_eq!(
        items,
        vec![
            ClassLocalCompletionItem {
                name: "kp".to_string(),
                detail: "Real".to_string(),
                kind: ClassLocalCompletionKind::Constant,
            },
            ClassLocalCompletionItem {
                name: "u".to_string(),
                detail: "Real".to_string(),
                kind: ClassLocalCompletionKind::Property,
            },
            ClassLocalCompletionItem {
                name: "x".to_string(),
                detail: "Real".to_string(),
                kind: ClassLocalCompletionKind::Variable,
            },
            ClassLocalCompletionItem {
                name: "Inner".to_string(),
                detail: "Model".to_string(),
                kind: ClassLocalCompletionKind::Class,
            },
        ],
        "local completion query should surface components and nested classes with stable kinds"
    );
}

#[test]
fn enclosing_class_qualified_name_query_uses_session_owned_syntax() {
    let mut session = Session::default();
    let source = r#"
within Demo;
model Outer
  model Inner
    Real x;
  end Inner;
  Inner child;
end Outer;
"#;

    session
        .add_document("input.mo", source)
        .expect("test source should parse");

    assert_eq!(
        session.enclosing_class_qualified_name_query("input.mo", 2),
        Some("Demo.Outer".to_string()),
        "outer body lines should resolve to the outer class"
    );
    assert_eq!(
        session.enclosing_class_qualified_name_query("input.mo", 3),
        Some("Demo.Outer.Inner".to_string()),
        "nested body lines should resolve to the innermost class"
    );
}

#[test]
fn class_interface_item_keys_survive_body_edits() {
    let mut session = Session::default();
    let source_v1 = r#"
within Demo;
model Outer
  model Inner
    Real x;
  equation
    x = 1;
  end Inner;
equation
  1 = 1;
end Outer;
"#;
    let source_v2 = r#"
within Demo;
model Outer
  model Inner
    Real x;
  algorithm
    x := 2;
  end Inner;
algorithm
  assert(true, "ok");
end Outer;
"#;

    session
        .add_document("test.mo", source_v1)
        .expect("initial source should parse");
    let keys_before = session
        .class_interface_index_query("test.mo")
        .expect("class interface index should build");
    let outer_before = keys_before
        .item_key_for_name("Demo.Outer")
        .expect("outer key should exist")
        .clone();
    let inner_before = keys_before
        .item_key_for_name("Demo.Outer.Inner")
        .expect("inner key should exist")
        .clone();

    let parse_error = session.update_document("test.mo", source_v2);
    assert!(parse_error.is_none(), "body edit should remain valid");
    let keys_after = session
        .class_interface_index_query("test.mo")
        .expect("class interface index should rebuild");

    assert_eq!(
        keys_after.item_key_for_name("Demo.Outer"),
        Some(&outer_before),
        "body edits should keep the outer scope key stable"
    );
    assert_eq!(
        keys_after.item_key_for_name("Demo.Outer.Inner"),
        Some(&inner_before),
        "body edits should keep nested scope keys stable"
    );
}

#[test]
fn class_interface_cache_is_invalidated_per_file() {
    let mut session = Session::default();
    session
        .add_document("a.mo", "model A\n  Real x;\nend A;\n")
        .expect("A should parse");
    session
        .add_document("b.mo", "model B\n  Real y;\nend B;\n")
        .expect("B should parse");

    let file_a = session.file_id("a.mo").expect("A should have a file id");
    let file_b = session.file_id("b.mo").expect("B should have a file id");

    session
        .class_interface_index_query("a.mo")
        .expect("A class interface query should build");
    session
        .class_interface_index_query("b.mo")
        .expect("B class interface query should build");
    let b_fingerprint_before = session
        .query_state
        .ast
        .class_interface_query_cache
        .get(&file_b)
        .expect("B class interface cache should exist")
        .fingerprint;

    assert!(
        session
            .query_state
            .ast
            .class_interface_query_cache
            .contains_key(&file_a),
        "A class interface cache should be populated"
    );
    assert!(
        session
            .query_state
            .ast
            .class_interface_query_cache
            .contains_key(&file_b),
        "B class interface cache should be populated"
    );

    let parse_error = session.update_document("b.mo", "model B\n  Real y;\n  Real z;\nend B;\n");
    assert!(parse_error.is_none(), "B update should remain valid");

    assert!(
        session
            .query_state
            .ast
            .class_interface_query_cache
            .contains_key(&file_a),
        "editing B should keep A's class interface cache entry warm"
    );
    assert!(
        session
            .query_state
            .ast
            .class_interface_query_cache
            .contains_key(&file_b),
        "editing B should keep the stale class interface cache resident until B is queried again"
    );

    session
        .class_interface_index_query("b.mo")
        .expect("B class interface query should rebuild");
    assert_ne!(
        session
            .query_state
            .ast
            .class_interface_query_cache
            .get(&file_b)
            .expect("B class interface cache should still exist")
            .fingerprint,
        b_fingerprint_before,
        "B's class interface fingerprint should change after the structural edit"
    );
}
