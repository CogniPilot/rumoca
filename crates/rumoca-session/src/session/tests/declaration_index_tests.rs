use super::*;

fn declaration_keys_by_path(session: &mut Session, uri: &str) -> IndexMap<String, ItemKey> {
    session
        .declaration_index_query(uri)
        .expect("declaration index should exist")
        .iter()
        .map(|(key, _)| (key.qualified_name(), key.clone()))
        .collect()
}

#[test]
fn declaration_index_item_keys_survive_body_edits() {
    let mut session = Session::default();
    let source_v1 = r#"
within Pkg;
model Outer
  parameter Real gain = 1;
  model Inner
    Real x(start=0);
  equation
    der(x) = gain;
  end Inner;
equation
  gain = 2;
end Outer;
"#;
    let source_v2 = r#"
within Pkg;
model Outer
  parameter Real gain = 3;
  model Inner
    Real x(start=0);
  equation
    der(x) = gain + 1;
  end Inner;
algorithm
  gain := 4;
end Outer;
"#;

    session
        .add_document("test.mo", source_v1)
        .expect("initial source should parse");
    let keys_before = declaration_keys_by_path(&mut session, "test.mo");

    let parse_error = session.update_document("test.mo", source_v2);
    assert!(parse_error.is_none(), "body edit should remain valid");
    let keys_after = declaration_keys_by_path(&mut session, "test.mo");

    for path in [
        "Pkg.Outer",
        "Pkg.Outer.gain",
        "Pkg.Outer.Inner",
        "Pkg.Outer.Inner.x",
    ] {
        assert_eq!(
            keys_before.get(path),
            keys_after.get(path),
            "declaration key should remain stable across body-only edits for {path}"
        );
    }
}

#[test]
fn declaration_index_keeps_unrelated_file_cache_warm() {
    let mut session = Session::default();
    session
        .add_document("a.mo", "model A\n  Real x;\nend A;\n")
        .expect("A should parse");
    session
        .add_document("b.mo", "model B\n  Real y;\nend B;\n")
        .expect("B should parse");

    let file_a = session.file_id("a.mo").expect("A should have a file id");
    let file_b = session.file_id("b.mo").expect("B should have a file id");

    let keys_before = declaration_keys_by_path(&mut session, "a.mo");
    session
        .declaration_index_query("b.mo")
        .expect("B declaration index should build");
    let b_fingerprint_before = session
        .query_state
        .ast
        .declaration_index_cache
        .get(&file_b)
        .expect("B declaration index cache should exist")
        .fingerprint;

    assert!(
        session
            .query_state
            .ast
            .declaration_index_cache
            .contains_key(&file_a),
        "A declaration index should be cached after the first query"
    );
    assert!(
        session
            .query_state
            .ast
            .declaration_index_cache
            .contains_key(&file_b),
        "B declaration index should be cached after the first query"
    );

    let parse_error = session.update_document("b.mo", "model B\n  Real y;\n  Real z;\nend B;\n");
    assert!(parse_error.is_none(), "B update should remain valid");

    assert!(
        session
            .query_state
            .ast
            .declaration_index_cache
            .contains_key(&file_a),
        "editing B should not invalidate A's declaration index"
    );
    assert!(
        session
            .query_state
            .ast
            .declaration_index_cache
            .contains_key(&file_b),
        "editing B should keep the stale cache entry resident until B is queried again"
    );

    let b_keys_after = declaration_keys_by_path(&mut session, "b.mo");
    assert!(
        b_keys_after.contains_key("B.z"),
        "re-querying B should rebuild the declaration index for the structural edit"
    );
    assert_ne!(
        session
            .query_state
            .ast
            .declaration_index_cache
            .get(&file_b)
            .expect("B declaration index cache should still exist")
            .fingerprint,
        b_fingerprint_before,
        "B's declaration index fingerprint should change after the structural edit"
    );

    let keys_after = declaration_keys_by_path(&mut session, "a.mo");
    assert_eq!(
        keys_before.get("A"),
        keys_after.get("A"),
        "unrelated edits should keep A's top-level declaration key stable"
    );
    assert_eq!(
        keys_before.get("A.x"),
        keys_after.get("A.x"),
        "unrelated edits should keep A's component declaration key stable"
    );
}

#[test]
fn declaration_index_changes_keys_when_owner_path_changes() {
    let mut session = Session::default();
    let source_v1 = "within Pkg;\nmodel M\n  Real x;\nend M;\n";
    let source_v2 = "within Other;\nmodel M\n  Real x;\nend M;\n";

    session
        .add_document("test.mo", source_v1)
        .expect("initial source should parse");
    let keys_before = declaration_keys_by_path(&mut session, "test.mo");

    let parse_error = session.update_document("test.mo", source_v2);
    assert!(parse_error.is_none(), "within edit should remain valid");
    let keys_after = declaration_keys_by_path(&mut session, "test.mo");

    assert!(
        keys_before.contains_key("Pkg.M"),
        "initial declaration index should include the original within path"
    );
    assert!(
        keys_after.contains_key("Other.M"),
        "updated declaration index should include the new within path"
    );
    assert_ne!(
        keys_before.get("Pkg.M"),
        keys_after.get("Other.M"),
        "changing the owner path should produce a new stable declaration key"
    );
}
