use super::*;

fn assert_prewarmed_snapshot_cache_state(
    snapshot: &SessionSnapshot,
    local_uri: &str,
) -> (FileId, usize, usize) {
    let snapshot_session = snapshot
        .session
        .lock()
        .expect("snapshot session lock should not be poisoned");
    let file_id = *snapshot_session
        .file_ids
        .get(local_uri)
        .expect("prewarmed snapshot should preserve the local file id");
    assert!(
        snapshot_session
            .query_state
            .ast
            .class_interface_query_cache
            .contains_key(&file_id),
        "prewarm should build the local class-interface cache"
    );
    assert!(
        snapshot_session
            .query_state
            .ast
            .package_def_map
            .orphan_cache
            .is_some(),
        "prewarm should build detached-package lookup state"
    );
    assert!(
        snapshot_session
            .query_state
            .ast
            .class_component_members_query_cache
            .contains_key("Lib.Helper"),
        "prewarm should build member completion state for referenced classes"
    );
    (
        file_id,
        snapshot_session
            .query_state
            .ast
            .class_interface_query_cache
            .len(),
        snapshot_session
            .query_state
            .ast
            .class_component_members_query_cache
            .len(),
    )
}

fn assert_snapshot_prewarm_remains_warm(
    snapshot: &SessionSnapshot,
    file_id: FileId,
    prewarmed_scope_entries: usize,
    prewarmed_member_entries: usize,
) {
    let snapshot_session = snapshot
        .session
        .lock()
        .expect("snapshot session lock should not be poisoned");
    assert!(
        snapshot_session
            .query_state
            .ast
            .class_interface_query_cache
            .contains_key(&file_id),
        "query-backed reads should keep the prewarmed class-interface cache available"
    );
    assert_eq!(
        snapshot_session
            .query_state
            .ast
            .class_interface_query_cache
            .len(),
        prewarmed_scope_entries,
        "prewarmed document queries should not create extra class-interface cache entries"
    );
    assert_eq!(
        snapshot_session
            .query_state
            .ast
            .class_component_members_query_cache
            .len(),
        prewarmed_member_entries,
        "prewarmed document queries should keep member completion caches warm"
    );
}

#[test]
fn class_lookup_query_resolves_unique_suffix_and_rejects_ambiguity() {
    let mut session = Session::default();
    session
        .add_document(
            "lib.mo",
            "package Lib\n  model Plane\n  end Plane;\nend Lib;\n",
        )
        .expect("library should parse");

    assert_eq!(
        session.class_lookup_query("Lib.Plane"),
        Some("Lib.Plane".to_string())
    );
    assert_eq!(
        session.class_lookup_query("Plane"),
        Some("Lib.Plane".to_string()),
        "unique suffix lookup should resolve across session documents"
    );

    session
        .add_document(
            "other.mo",
            "package Other\n  model Plane\n  end Plane;\nend Other;\n",
        )
        .expect("second library should parse");

    assert_eq!(
        session.class_lookup_query("Plane"),
        None,
        "simple-name lookup should reject ambiguous suffix matches"
    );
    assert_eq!(
        session.class_lookup_query("Lib.Plane"),
        Some("Lib.Plane".to_string()),
        "qualified lookup should keep resolving exactly after suffix ambiguity appears"
    );
    assert_eq!(
        session.class_lookup_query("Other.Plane"),
        Some("Other.Plane".to_string()),
        "qualified lookup should resolve the matching package member directly"
    );
}

#[test]
fn class_component_members_query_collects_extends_and_breaks_across_files() {
    let mut session = Session::default();
    session
        .add_document(
            "base.mo",
            "package Lib\n  model Base\n    parameter Real kp;\n    Real y;\n  end Base;\nend Lib;\n",
        )
        .expect("base should parse");
    session
        .add_document(
            "derived.mo",
            "within Lib;\nmodel Derived\n  extends Base(break y);\n  Real z;\nend Derived;\n",
        )
        .expect("derived should parse");

    assert_eq!(
        session.class_component_members_query("Lib.Derived"),
        vec![
            ("kp".to_string(), "Real".to_string()),
            ("z".to_string(), "Real".to_string()),
        ],
        "query-backed member collection should inherit base members and apply break exclusions"
    );
}

#[test]
fn class_component_members_query_cache_stays_warm_for_unrelated_edits_and_rebuilds_for_dependencies()
 {
    let mut session = Session::default();
    session
        .add_document(
            "base.mo",
            "package Lib\n  model Base\n    Real x;\n  end Base;\nend Lib;\n",
        )
        .expect("base should parse");
    session
        .add_document(
            "derived.mo",
            "within Lib;\nmodel Derived\n  extends Base;\n  Real z;\nend Derived;\n",
        )
        .expect("derived should parse");
    session
        .add_document("other.mo", "model Other\n  Real y;\nend Other;\n")
        .expect("other should parse");

    let initial_members = session.class_component_members_query("Lib.Derived");
    assert_eq!(
        initial_members,
        vec![
            ("x".to_string(), "Real".to_string()),
            ("z".to_string(), "Real".to_string()),
        ]
    );
    assert!(
        session
            .query_state
            .ast
            .class_component_members_query_cache
            .contains_key("Lib.Derived"),
        "first query should populate the member-query cache"
    );

    let parse_error = session.update_document(
        "other.mo",
        "model Other\n  Real y;\n  Real q;\nend Other;\n",
    );
    assert!(parse_error.is_none(), "unrelated edit should remain valid");
    assert!(
        session
            .query_state
            .ast
            .class_component_members_query_cache
            .contains_key("Lib.Derived"),
        "unrelated edits should keep the cached query entry available"
    );
    assert_eq!(
        session.class_component_members_query("Lib.Derived"),
        initial_members,
        "unrelated edits should keep member-query results warm"
    );

    let parse_error = session.update_document(
        "base.mo",
        "package Lib\n  model Base\n    Real x;\n    Real w;\n  end Base;\nend Lib;\n",
    );
    assert!(parse_error.is_none(), "dependency edit should remain valid");
    assert_eq!(
        session.class_component_members_query("Lib.Derived"),
        vec![
            ("x".to_string(), "Real".to_string()),
            ("w".to_string(), "Real".to_string()),
            ("z".to_string(), "Real".to_string()),
        ],
        "dependency edits should rebuild the member-query result"
    );
}

#[test]
fn snapshot_prewarm_document_ide_queries_warms_local_member_completion_inputs() {
    let mut session = Session::default();
    session
        .add_document(
            "input.mo",
            r#"package Lib
  model Helper
    parameter Real gain = 1;
    output Real y;
  equation
    y = gain;
  end Helper;
end Lib;

model M
  import Alias = Lib.Helper;
  Alias helperInst;
equation
  helperInst.y = sin(helperInst.gain);
end M;
"#,
        )
        .expect("synthetic source should parse");

    let snapshot = session.lightweight_snapshot();
    snapshot.prewarm_document_ide_queries("input.mo");
    let (file_id, prewarmed_scope_entries, prewarmed_member_entries) =
        assert_prewarmed_snapshot_cache_state(&snapshot, "input.mo");

    assert_eq!(
        snapshot.enclosing_class_qualified_name_query("input.mo", 11),
        Some("M".to_string())
    );
    assert_eq!(
        snapshot.class_component_type_query("input.mo", "M", "helperInst"),
        Some("Alias".to_string())
    );
    assert_eq!(
        snapshot.class_type_resolution_candidates_query("input.mo", "M", "Alias"),
        vec!["Lib.Helper".to_string(), "Alias".to_string()],
        "prewarmed snapshots should preserve exact import resolution"
    );
    assert_eq!(
        snapshot.class_component_members_query("Lib.Helper"),
        vec![
            ("gain".to_string(), "Real".to_string()),
            ("y".to_string(), "Real".to_string()),
        ]
    );

    assert_snapshot_prewarm_remains_warm(
        &snapshot,
        file_id,
        prewarmed_scope_entries,
        prewarmed_member_entries,
    );
}
