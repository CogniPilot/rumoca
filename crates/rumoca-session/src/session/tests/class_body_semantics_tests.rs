use super::*;

#[test]
fn class_body_semantics_query_tracks_local_component_references() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            "model M\n  Real x;\nequation\n  x = x + 1;\nend M;\n",
        )
        .expect("source should parse");

    let references = session
        .navigation_references_query("test.mo", 1, "  Real x".len() as u32, true)
        .expect("component references should resolve");
    assert_eq!(
        references.len(),
        3,
        "component query should include declaration and both use sites"
    );
    assert_eq!(references[0].1.start_line, 2);
    assert_eq!(references[1].1.start_line, 4);
    assert_eq!(references[2].1.start_line, 4);

    let rename_span = session
        .navigation_prepare_rename_query("test.mo", 3, 3)
        .expect("rename span should exist for local component use");
    assert_eq!(rename_span.start_line, 4);
    assert_eq!(rename_span.start_column, 3);

    let rename_locations = session
        .navigation_rename_locations_query("test.mo", 3, 3)
        .expect("rename locations should exist");
    assert_eq!(
        rename_locations.len(),
        3,
        "rename should target the same declaration and reference sites"
    );
}

#[test]
fn local_component_info_query_resolves_hover_and_definition_data() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            "model M\n  parameter Real kp;\n  Real x[2];\nequation\n  x[1] = kp;\nend M;\n",
        )
        .expect("source should parse");

    let info = session
        .local_component_info_query("test.mo", 4, "  x".len() as u32)
        .expect("local component info should resolve");
    assert_eq!(info.name, "x");
    assert_eq!(info.type_name, "Real");
    assert_eq!(info.keyword_prefix, None);
    assert_eq!(info.shape, vec![2]);
    assert_eq!(info.declaration_location.start_line, 3);

    let parameter = session
        .local_component_info_query("test.mo", 1, "  parameter Real kp".len() as u32)
        .expect("parameter component info should resolve");
    assert_eq!(parameter.name, "kp");
    assert_eq!(parameter.keyword_prefix.as_deref(), Some("parameter"));
}

#[test]
fn class_body_semantics_cache_keeps_unrelated_file_warm() {
    let mut session = Session::default();
    session
        .add_document("a.mo", "model A\n  Real x;\nequation\n  x = x;\nend A;\n")
        .expect("A should parse");
    session
        .add_document("b.mo", "model B\n  Real y;\nequation\n  y = y;\nend B;\n")
        .expect("B should parse");

    let file_a = session.file_id("a.mo").expect("A should have a file id");
    let file_b = session.file_id("b.mo").expect("B should have a file id");

    let refs_before = session
        .navigation_references_query("a.mo", 1, "  Real x".len() as u32, true)
        .expect("A references should resolve");
    session
        .navigation_references_query("b.mo", 1, "  Real y".len() as u32, true)
        .expect("B references should resolve");
    let b_fingerprint_before = session
        .query_state
        .ast
        .class_body_semantics_cache
        .get(&file_b)
        .expect("B class-body semantics should be cached")
        .fingerprint;

    assert!(
        session
            .query_state
            .ast
            .class_body_semantics_cache
            .contains_key(&file_a),
        "A class-body semantics should be cached after the first query"
    );
    assert!(
        session
            .query_state
            .ast
            .class_body_semantics_cache
            .contains_key(&file_b),
        "B class-body semantics should be cached after the first query"
    );

    let parse_error = session.update_document(
        "b.mo",
        "model B\n  Real y;\n  Real z;\nequation\n  y = z;\nend B;\n",
    );
    assert!(parse_error.is_none(), "B update should remain valid");

    assert!(
        session
            .query_state
            .ast
            .class_body_semantics_cache
            .contains_key(&file_a),
        "editing B should not invalidate A's class-body semantics"
    );
    assert!(
        session
            .query_state
            .ast
            .class_body_semantics_cache
            .contains_key(&file_b),
        "editing B should keep the stale semantics resident until B is queried again"
    );

    let refs_after = session
        .navigation_references_query("a.mo", 1, "  Real x".len() as u32, true)
        .expect("A references should stay available");
    assert_eq!(
        refs_before.len(),
        refs_after.len(),
        "unrelated edits should preserve A's local navigation results"
    );
    session
        .navigation_references_query("b.mo", 1, "  Real y".len() as u32, true)
        .expect("B references should rebuild after the edit");
    assert_ne!(
        session
            .query_state
            .ast
            .class_body_semantics_cache
            .get(&file_b)
            .expect("B class-body semantics should be cached after rebuild")
            .fingerprint,
        b_fingerprint_before,
        "B's class-body semantics fingerprint should change after the edit"
    );
}

#[test]
fn class_body_semantics_rebuilds_when_body_references_move_without_outline_change() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            "model M\n  Real x;\n  Real y;\nequation\n  x = y;\nend M;\n",
        )
        .expect("initial source should parse");

    let file_id = session.file_id("test.mo").expect("file id should exist");
    let outline_before = session
        .get_document("test.mo")
        .expect("document should exist")
        .outline_fingerprint();
    let navigation_before = session
        .get_document("test.mo")
        .expect("document should exist")
        .navigation_fingerprint();
    let references_before = session
        .navigation_references_query("test.mo", 2, "  Real y".len() as u32, true)
        .expect("initial navigation references should resolve");
    assert_eq!(references_before.len(), 2);
    let cached_before = session
        .query_state
        .ast
        .class_body_semantics_cache
        .get(&file_id)
        .expect("class-body semantics should be cached")
        .fingerprint;

    let parse_error = session.update_document(
        "test.mo",
        "model M\n  Real x;\n  Real y;\nequation\n  y = x;\nend M;\n",
    );
    assert!(parse_error.is_none(), "body-only edit should remain valid");
    assert_eq!(
        session
            .get_document("test.mo")
            .expect("updated document should exist")
            .outline_fingerprint(),
        outline_before,
        "same-span body edits should keep the outline fingerprint stable"
    );
    assert_ne!(
        session
            .get_document("test.mo")
            .expect("updated document should exist")
            .navigation_fingerprint(),
        navigation_before,
        "moving local reference sites should change the navigation fingerprint"
    );
    assert_eq!(
        session
            .query_state
            .ast
            .class_body_semantics_cache
            .get(&file_id)
            .expect("stale class-body semantics should remain resident until re-query")
            .fingerprint,
        cached_before,
        "editing should not eagerly rebuild class-body semantics"
    );

    let references_after = session
        .navigation_references_query("test.mo", 2, "  Real y".len() as u32, true)
        .expect("updated navigation references should resolve");
    assert_eq!(references_after.len(), 2);
    assert_ne!(
        session
            .query_state
            .ast
            .class_body_semantics_cache
            .get(&file_id)
            .expect("class-body semantics should rebuild after re-query")
            .fingerprint,
        cached_before,
        "re-query should rebuild the class-body semantics with the new fingerprint"
    );
}

#[test]
fn navigation_query_tracks_class_targets_across_files() {
    let mut session = Session::default();
    session
        .add_document(
            "lib.mo",
            "package Lib\n  block Target\n    Real y;\n  equation\n    y = 1;\n  end Target;\nend Lib;\n",
        )
        .expect("source-root source should parse");
    session
        .add_document(
            "active.mo",
            "model M\n  import Lib.Target;\n  import Renamed = Lib.Target;\n  Target a;\n  Lib.Target b;\n  Renamed c;\nequation\n  a.y = b.y + c.y;\nend M;\n",
        )
        .expect("active source should parse");

    let references = session
        .navigation_references_query("active.mo", 3, "  Target".len() as u32, true)
        .expect("class target references should resolve");
    assert_eq!(
        references.len(),
        6,
        "references should include the declaration and all parsed class uses"
    );
    assert!(
        references
            .iter()
            .any(|(uri, location)| uri == "lib.mo" && location.start_line == 2),
        "references should include the source-root declaration"
    );
    assert!(
        references
            .iter()
            .any(|(uri, location)| uri == "active.mo" && location.start_line == 6),
        "references should include alias-based type uses"
    );

    let rename_span = session
        .navigation_prepare_rename_query("active.mo", 3, "  Target".len() as u32)
        .expect("rename span should exist for query-backed class target");
    assert_eq!(rename_span.start_line, 4);
    assert_eq!(rename_span.start_column, 3);

    let rename_locations = session
        .navigation_rename_locations_query("active.mo", 3, "  Target".len() as u32)
        .expect("rename locations should resolve for query-backed class target");
    assert_eq!(
        rename_locations.len(),
        6,
        "rename should target declaration, end name, and matching class-name tokens"
    );
    assert!(
        rename_locations
            .iter()
            .any(|(uri, location)| uri == "lib.mo" && location.start_line == 6),
        "rename should update the end-name token in the declaring file"
    );
    assert!(
        rename_locations
            .iter()
            .all(|(uri, location)| !(uri == "active.mo" && location.start_line == 6)),
        "rename should not rewrite alias tokens that resolve to the class"
    );
}

#[test]
fn navigation_class_target_query_resolves_imported_and_qualified_paths() {
    let mut session = Session::default();
    session
        .add_document(
            "lib.mo",
            "package Lib\n  block Target \"test target\"\n    Real y;\n  equation\n    y = 1;\n  end Target;\nend Lib;\n",
        )
        .expect("source-root source should parse");
    session
        .add_document(
            "active.mo",
            "model M\n  import Alias = Lib.Target;\n  Alias a;\n  Lib.Target b;\nequation\n  a.y = b.y;\nend M;\n",
        )
        .expect("active source should parse");

    let imported = session
        .navigation_class_target_query("active.mo", 1, "  import Alias".len() as u32)
        .expect("imported alias target should resolve");
    assert_eq!(imported.target_uri, "lib.mo");
    assert_eq!(imported.qualified_name, "Lib.Target");
    assert_eq!(imported.class_name, "Target");
    assert_eq!(imported.class_type, ast::ClassType::Block);
    assert_eq!(imported.description.as_deref(), Some("test target"));
    assert_eq!(imported.component_count, 1);
    assert_eq!(imported.equation_count, 1);
    assert_eq!(imported.declaration_location.start_line, 2);
    assert_eq!(imported.declaration_location.start_column, 9);

    let qualified = session
        .navigation_class_target_query("active.mo", 3, "  Lib.".len() as u32)
        .expect("qualified path target should resolve from dotted token position");
    assert_eq!(qualified.target_uri, "lib.mo");
    assert_eq!(qualified.qualified_name, "Lib.Target");
    assert_eq!(qualified.declaration_location.start_line, 2);
    assert_eq!(qualified.declaration_location.start_column, 9);
}

#[test]
fn navigation_query_tracks_class_targets_in_modifier_bodies() {
    let mut session = Session::default();
    session
        .add_document(
            "active.mo",
            "model DefaultVariant\n  Real x;\nend DefaultVariant;\n\nmodel Base\n  replaceable model Variant = DefaultVariant;\nend Base;\n\nmodel Test\n  Base base(replaceable model Variant = DefaultVariant);\nend Test;\n",
        )
        .expect("active source should parse");

    let target = session
        .navigation_class_target_query(
            "active.mo",
            9,
            "  Base base(replaceable model Variant = Default".len() as u32,
        )
        .expect("modifier class target should resolve");
    assert_eq!(target.target_uri, "active.mo");
    assert_eq!(target.qualified_name, "DefaultVariant");
    assert_eq!(target.declaration_location.start_line, 1);

    let references = session
        .navigation_references_query(
            "active.mo",
            9,
            "  Base base(replaceable model Variant = Default".len() as u32,
            true,
        )
        .expect("modifier class target references should resolve");
    assert_eq!(
        references.len(),
        3,
        "references should include the declaration and both modifier-body uses"
    );
    assert!(
        references
            .iter()
            .any(|(_, location)| location.start_line == 6),
        "references should include the base-model modifier use"
    );
    assert!(
        references
            .iter()
            .any(|(_, location)| location.start_line == 10),
        "references should include the component modifier use"
    );

    let rename_span = session
        .navigation_prepare_rename_query(
            "active.mo",
            9,
            "  Base base(replaceable model Variant = Default".len() as u32,
        )
        .expect("modifier class target should be renameable");
    assert_eq!(rename_span.start_line, 10);

    let rename_locations = session
        .navigation_rename_locations_query(
            "active.mo",
            9,
            "  Base base(replaceable model Variant = Default".len() as u32,
        )
        .expect("modifier class target rename locations should resolve");
    assert_eq!(
        rename_locations.len(),
        4,
        "rename should include declaration, end name, and both modifier-body uses"
    );
    assert!(
        rename_locations
            .iter()
            .any(|(_, location)| location.start_line == 3),
        "rename should include the end-name token"
    );
}
