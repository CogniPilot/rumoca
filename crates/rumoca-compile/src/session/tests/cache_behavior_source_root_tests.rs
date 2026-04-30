use super::*;
use std::collections::HashSet;

fn namespace_class_names(session: &mut Session) -> Vec<String> {
    let mut stack = vec![String::new()];
    let mut seen = HashSet::new();
    let mut names = Vec::new();

    while let Some(prefix) = stack.pop() {
        let entries = session
            .namespace_index_query(&prefix)
            .expect("query namespace completion cache");
        for (_child, full_name, has_children) in entries {
            if !seen.insert(full_name.clone()) {
                continue;
            }
            names.push(full_name.clone());
            if has_children {
                stack.push(format!("{full_name}."));
            }
        }
    }

    names.sort_unstable();
    names
}

fn workspace_a_subtree_root_v1() -> Vec<(String, ast::StoredDefinition)> {
    vec![
        (
            "A/package.mo".to_string(),
            parse_definition("within ;\npackage A\nend A;\n", "A/package.mo"),
        ),
        (
            "A/Sub1/package.mo".to_string(),
            parse_definition("within A;\npackage Sub1\nend Sub1;\n", "A/Sub1/package.mo"),
        ),
        (
            "A/Sub1/M.mo".to_string(),
            parse_definition("within A.Sub1;\nmodel M\nend M;\n", "A/Sub1/M.mo"),
        ),
        (
            "A/Sub2/package.mo".to_string(),
            parse_definition("within A;\npackage Sub2\nend Sub2;\n", "A/Sub2/package.mo"),
        ),
        (
            "A/Sub2/N.mo".to_string(),
            parse_definition("within A.Sub2;\nmodel N\nend N;\n", "A/Sub2/N.mo"),
        ),
    ]
}

fn workspace_a_subtree_root_v2() -> Vec<(String, ast::StoredDefinition)> {
    vec![
        (
            "A/package.mo".to_string(),
            parse_definition("within ;\npackage A\nend A;\n", "A/package.mo"),
        ),
        (
            "A/Sub1/package.mo".to_string(),
            parse_definition("within A;\npackage Sub1\nend Sub1;\n", "A/Sub1/package.mo"),
        ),
        (
            "A/Sub1/M.mo".to_string(),
            parse_definition("within A.Sub1;\nmodel M\nend M;\n", "A/Sub1/M.mo"),
        ),
        (
            "A/Sub1/Added.mo".to_string(),
            parse_definition(
                "within A.Sub1;\nmodel Added\nend Added;\n",
                "A/Sub1/Added.mo",
            ),
        ),
        (
            "A/Sub2/package.mo".to_string(),
            parse_definition("within A;\npackage Sub2\nend Sub2;\n", "A/Sub2/package.mo"),
        ),
        (
            "A/Sub2/N.mo".to_string(),
            parse_definition("within A.Sub2;\nmodel N\nend N;\n", "A/Sub2/N.mo"),
        ),
    ]
}

fn partitioned_workspace_definitions_v1() -> Vec<(String, ast::StoredDefinition)> {
    vec![
        (
            "NewFolder/package.mo".to_string(),
            parse_definition(
                "within ;\npackage NewFolder\nend NewFolder;\n",
                "NewFolder/package.mo",
            ),
        ),
        (
            "NewFolder/Test.mo".to_string(),
            parse_definition(
                "within NewFolder;\nmodel Test\nend Test;\n",
                "NewFolder/Test.mo",
            ),
        ),
        (
            "Other/package.mo".to_string(),
            parse_definition(
                "within ;\npackage Other\n  model M1\n  end M1;\nend Other;\n",
                "Other/package.mo",
            ),
        ),
    ]
}

fn partitioned_workspace_definitions_v2() -> Vec<(String, ast::StoredDefinition)> {
    vec![
        (
            "NewFolder/package.mo".to_string(),
            parse_definition(
                "within ;\npackage NewFolder\nend NewFolder;\n",
                "NewFolder/package.mo",
            ),
        ),
        (
            "NewFolder/Test.mo".to_string(),
            parse_definition(
                "within NewFolder;\nmodel Test\n  Real x;\nend Test;\n",
                "NewFolder/Test.mo",
            ),
        ),
        (
            "Other/package.mo".to_string(),
            parse_definition(
                "within ;\npackage Other\n  model M1\n  end M1;\nend Other;\n",
                "Other/package.mo",
            ),
        ),
    ]
}

fn assert_namespace_source_set_signature(
    session: &Session,
    source_set_id: SourceSetId,
    expected_signature: &SourceSetClassGraphSignature,
) {
    let cache = session
        .query_state
        .ast
        .source_root_namespace_cache
        .as_ref()
        .expect("source-root namespace cache should be present");
    let membership = &session.query_state.ast.package_def_map;

    assert_eq!(
        &membership
            .source_set_caches
            .get(&source_set_id)
            .expect("source-set membership cache should exist")
            .signature,
        expected_signature,
        "source-set membership cache should retain the expected signature"
    );
    assert_eq!(
        cache
            .merged_source_set_signatures
            .get(&source_set_id)
            .expect("merged source-set signature should exist"),
        expected_signature,
        "merged source-set signature should retain the expected value"
    );
    assert_eq!(
        &cache
            .source_set_caches
            .get(&source_set_id)
            .expect("source-set namespace cache should exist")
            .signature,
        expected_signature,
        "source-set namespace cache should retain the expected signature"
    );
}

fn assert_namespace_source_set_rebuild_pending(
    session: &Session,
    source_set_id: SourceSetId,
    previous_signature: &SourceSetClassGraphSignature,
) {
    let cache = session
        .query_state
        .ast
        .source_root_namespace_cache
        .as_ref()
        .expect("namespace cache should remain resident");
    let membership = &session.query_state.ast.package_def_map;

    assert!(
        membership
            .source_set_caches
            .get(&source_set_id)
            .is_some_and(|entry| entry.signature == *previous_signature),
        "source-set membership cache should stay resident until the next query rebuilds it",
    );
    assert!(
        cache
            .source_set_caches
            .get(&source_set_id)
            .is_some_and(|entry| entry.signature == *previous_signature),
        "source-set namespace cache should stay resident until the next query rebuilds it",
    );
    assert!(
        !cache
            .merged_source_set_signatures
            .contains_key(&source_set_id),
        "merged source-set signature should be evicted so the next merge recomputes it",
    );
}

fn source_root_namespace_signature(
    session: &Session,
    source_set_id: SourceSetId,
) -> SourceSetClassGraphSignature {
    session
        .query_state
        .ast
        .source_root_namespace_cache
        .as_ref()
        .expect("namespace cache should be present")
        .source_set_caches
        .get(&source_set_id)
        .expect("source-set namespace cache should be present")
        .signature
        .clone()
}

fn load_two_source_sets(
    session: &mut Session,
    kind: SourceRootKind,
    prefix: &str,
) -> (
    SourceSetId,
    SourceSetId,
    SourceSetClassGraphSignature,
    SourceSetClassGraphSignature,
) {
    let parsed_a = parse_definition("package A\n  model MA\n  end MA;\nend A;\n", "A/package.mo");
    let parsed_b = parse_definition("package B\n  model MB\n  end MB;\nend B;\n", "B/package.mo");
    let key_a = format!("{prefix}::A");
    let key_b = format!("{prefix}::B");
    assert_eq!(
        session.replace_parsed_source_set(
            &key_a,
            kind,
            vec![("A/package.mo".to_string(), parsed_a)],
            None
        ),
        1
    );
    assert_eq!(
        session.replace_parsed_source_set(
            &key_b,
            kind,
            vec![("B/package.mo".to_string(), parsed_b)],
            None
        ),
        1
    );
    session
        .namespace_index_query("")
        .expect("prime namespace cache");
    let source_set_a = source_set_record(session, &key_a).id;
    let source_set_b = source_set_record(session, &key_b).id;
    let a_signature = source_root_namespace_signature(session, source_set_a);
    let b_signature = source_root_namespace_signature(session, source_set_b);
    (source_set_a, source_set_b, a_signature, b_signature)
}

fn load_two_external_source_sets(
    session: &mut Session,
) -> (
    SourceSetId,
    SourceSetId,
    SourceSetClassGraphSignature,
    SourceSetClassGraphSignature,
) {
    load_two_source_sets(session, SourceRootKind::External, "external")
}

fn load_two_workspace_source_sets(
    session: &mut Session,
) -> (
    SourceSetId,
    SourceSetId,
    SourceSetClassGraphSignature,
    SourceSetClassGraphSignature,
) {
    load_two_source_sets(session, SourceRootKind::Workspace, "workspace")
}

#[test]
fn external_source_root_namespace_fingerprint_ignores_unrelated_external_root_changes() {
    let mut session = Session::default();

    let lib_src = r#"
        package Lib
          package Electrical
            model Resistor
              Real v;
            equation
              der(v) = 1;
            end Resistor;
          end Electrical;
        end Lib;
    "#;
    let other_v1 = r#"
        package Other
          model A
          end A;
        end Other;
    "#;
    let other_v2 = r#"
        package Other
          model B
          end B;
        end Other;
    "#;
    let lib_parsed =
        rumoca_phase_parse::parse_to_ast(lib_src, "Lib/package.mo").expect("parse Lib");
    let other_parsed_v1 =
        rumoca_phase_parse::parse_to_ast(other_v1, "Other/package.mo").expect("parse Other v1");
    let other_parsed_v2 =
        rumoca_phase_parse::parse_to_ast(other_v2, "Other/package.mo").expect("parse Other v2");

    assert_eq!(
        session.replace_parsed_source_set(
            "external::Lib",
            SourceRootKind::External,
            vec![("Lib/package.mo".to_string(), lib_parsed)],
            None,
        ),
        1
    );
    assert_eq!(
        session.replace_parsed_source_set(
            "external::Other",
            SourceRootKind::External,
            vec![("Other/package.mo".to_string(), other_parsed_v1)],
            None,
        ),
        1
    );

    session
        .namespace_index_query("")
        .expect("prime namespace cache");
    let before = session
        .namespace_fingerprint_cached("Lib.")
        .expect("Lib namespace fingerprint");

    assert_eq!(
        session.replace_parsed_source_set(
            "external::Other",
            SourceRootKind::External,
            vec![("Other/package.mo".to_string(), other_parsed_v2)],
            None,
        ),
        1
    );
    session
        .namespace_index_query("")
        .expect("rebuild namespace cache");
    let after = session
        .namespace_fingerprint_cached("Lib.")
        .expect("Lib namespace fingerprint after rebuild");

    assert_eq!(
        before, after,
        "unrelated external root changes should not perturb Lib namespace closure fingerprint"
    );
}

#[test]
fn source_set_scoped_invalidation_keeps_other_namespace_cache_entries_warm() {
    let mut session = Session::default();
    let (source_set_a, source_set_b, a_signature_before, b_signature_before) =
        load_two_external_source_sets(&mut session);
    let parsed_b_v2 = parse_definition(
        "package B\n  model MB\n  end MB;\n  model MB2\n  end MB2;\nend B;\n",
        "B/package.mo",
    );
    let replaced = session.replace_parsed_source_set(
        "external::B",
        SourceRootKind::External,
        vec![("B/package.mo".to_string(), parsed_b_v2)],
        None,
    );
    let cache_after_replace = session
        .query_state
        .ast
        .source_root_namespace_cache
        .as_ref()
        .expect("cache should remain after scoped invalidation");

    assert_eq!(replaced, 1, "B replacement should update one document");
    assert_namespace_source_set_signature(&session, source_set_a, &a_signature_before);
    assert_namespace_source_set_rebuild_pending(&session, source_set_b, &b_signature_before);
    assert!(
        cache_after_replace.merged_cache.is_none(),
        "merged cache must rebuild lazily"
    );

    let class_names = namespace_class_names(&mut session);
    assert!(
        class_names.contains(&"B.MB2".to_string()),
        "updated B cache should include MB2"
    );

    let cache_after_rebuild = session
        .query_state
        .ast
        .source_root_namespace_cache
        .as_ref()
        .expect("source-root namespace cache should be present");

    assert_namespace_source_set_signature(&session, source_set_a, &a_signature_before);
    assert!(
        session
            .query_state
            .ast
            .package_def_map
            .source_set_caches
            .get(&source_set_b)
            .is_some_and(|entry| entry.signature != b_signature_before),
        "B source-set membership cache should be rebuilt"
    );
    assert!(
        cache_after_rebuild
            .merged_source_set_signatures
            .get(&source_set_a)
            .is_some_and(|signature| *signature == a_signature_before),
        "A merged source-set signature should be reused"
    );
    assert!(
        cache_after_rebuild
            .merged_source_set_signatures
            .get(&source_set_b)
            .is_some_and(|signature| *signature != b_signature_before),
        "B merged source-set signature should be rebuilt"
    );
    assert!(
        cache_after_rebuild.merged_cache.is_some(),
        "merged cache should be rebuilt"
    );
}

#[test]
fn all_source_root_query_signatures_track_summary_not_revision() {
    let signature_after_body_only_change = |kind| {
        let mut session = Session::default();
        let key = "source-root";
        let root_v1 = vec![
            (
                "Lib/package.mo".to_string(),
                parse_definition("within ;\npackage Lib\nend Lib;\n", "Lib/package.mo"),
            ),
            (
                "Lib/M.mo".to_string(),
                parse_definition(
                    "within Lib;\nmodel M\n  Real x(start=0);\nequation\n  der(x) = 1;\nend M;\n",
                    "Lib/M.mo",
                ),
            ),
        ];
        session.replace_parsed_source_set(key, kind, root_v1, None);
        let source_set_id = session
            .source_set_id(key)
            .expect("source-root id should exist");
        let signature_before = session
            .source_set_query_signature(source_set_id)
            .expect("initial source-root query signature");

        let root_v2 = vec![
            (
                "Lib/package.mo".to_string(),
                parse_definition("within ;\npackage Lib\nend Lib;\n", "Lib/package.mo"),
            ),
            (
                "Lib/M.mo".to_string(),
                parse_definition(
                    "within Lib;\nmodel M\n  Real x(start=0);\nequation\n  der(x) = 2;\nend M;\n",
                    "Lib/M.mo",
                ),
            ),
        ];
        session.replace_parsed_source_set(key, kind, root_v2, None);
        let signature_after = session
            .source_set_query_signature(source_set_id)
            .expect("body-only source-root query signature");

        (signature_before, signature_after)
    };
    let signature_after_interface_change = |kind| {
        let mut session = Session::default();
        let key = "source-root";
        let root_v1 = vec![
            (
                "Lib/package.mo".to_string(),
                parse_definition("within ;\npackage Lib\nend Lib;\n", "Lib/package.mo"),
            ),
            (
                "Lib/M.mo".to_string(),
                parse_definition(
                    "within Lib;\nmodel M\n  Real x(start=0);\nequation\n  der(x) = 1;\nend M;\n",
                    "Lib/M.mo",
                ),
            ),
        ];
        session.replace_parsed_source_set(key, kind, root_v1, None);
        let source_set_id = session
            .source_set_id(key)
            .expect("source-root id should exist");
        let signature_before = session
            .source_set_query_signature(source_set_id)
            .expect("initial source-root query signature");

        let root_v2 = vec![
            (
                "Lib/package.mo".to_string(),
                parse_definition("within ;\npackage Lib\nend Lib;\n", "Lib/package.mo"),
            ),
            (
                "Lib/M.mo".to_string(),
                parse_definition(
                    "within Lib;\nmodel M\n  Real x(start=0);\n  parameter Real gain = 2;\nequation\n  der(x) = gain;\nend M;\n",
                    "Lib/M.mo",
                ),
            ),
        ];
        session.replace_parsed_source_set(key, kind, root_v2, None);
        let signature_after = session
            .source_set_query_signature(source_set_id)
            .expect("interface-change source-root query signature");

        (signature_before, signature_after)
    };

    for kind in [
        SourceRootKind::Workspace,
        SourceRootKind::External,
        SourceRootKind::DurableExternal,
    ] {
        let (body_before, body_after) = signature_after_body_only_change(kind);
        assert_eq!(
            body_before, body_after,
            "body-only edits should not perturb the live source-root query signature for {kind:?}"
        );

        let (interface_before, interface_after) = signature_after_interface_change(kind);
        assert_ne!(
            interface_before, interface_after,
            "interface edits should still perturb the live source-root query signature for {kind:?}"
        );
    }
}

#[test]
fn source_set_scoped_invalidation_keeps_other_namespace_cache_entries_warm_across_root_kinds() {
    let scenarios = [
        (SourceRootKind::Workspace, "workspace"),
        (SourceRootKind::External, "external"),
        (SourceRootKind::DurableExternal, "durable"),
    ];

    for (kind, prefix) in scenarios {
        let mut session = Session::default();
        let (source_set_a, source_set_b, a_signature_before, b_signature_before) =
            load_two_source_sets(&mut session, kind, prefix);
        let parsed_b_v2 = parse_definition(
            "package B\n  model MB\n  end MB;\n  model MB2\n  end MB2;\nend B;\n",
            "B/package.mo",
        );
        let source_set_b_key = format!("{prefix}::B");
        let replaced = session.replace_parsed_source_set(
            &source_set_b_key,
            kind,
            vec![("B/package.mo".to_string(), parsed_b_v2)],
            None,
        );
        let cache_after_replace = session
            .query_state
            .ast
            .source_root_namespace_cache
            .as_ref()
            .expect("cache should remain after scoped invalidation");

        assert_eq!(
            replaced, 1,
            "{kind:?} replacement should update one document"
        );
        assert_namespace_source_set_signature(&session, source_set_a, &a_signature_before);
        assert_namespace_source_set_rebuild_pending(&session, source_set_b, &b_signature_before);
        assert!(
            cache_after_replace.merged_cache.is_none(),
            "{kind:?} merged cache must rebuild lazily"
        );

        let class_names = namespace_class_names(&mut session);
        assert!(
            class_names.contains(&"B.MB2".to_string()),
            "{kind:?} updated B cache should include MB2"
        );

        let cache_after_rebuild = session
            .query_state
            .ast
            .source_root_namespace_cache
            .as_ref()
            .expect("namespace cache should be present");

        assert_namespace_source_set_signature(&session, source_set_a, &a_signature_before);
        assert!(
            session
                .query_state
                .ast
                .package_def_map
                .source_set_caches
                .get(&source_set_b)
                .is_some_and(|entry| entry.signature != b_signature_before),
            "{kind:?} changed source-set membership cache should be rebuilt"
        );
        assert!(
            cache_after_rebuild
                .merged_source_set_signatures
                .get(&source_set_a)
                .is_some_and(|signature| *signature == a_signature_before),
            "{kind:?} unchanged source-set signature should be reused"
        );
        assert!(
            cache_after_rebuild
                .merged_source_set_signatures
                .get(&source_set_b)
                .is_some_and(|signature| *signature != b_signature_before),
            "{kind:?} changed source-set signature should be rebuilt"
        );
    }
}

#[test]
fn workspace_namespace_fingerprint_ignores_unrelated_workspace_root_changes() {
    let mut session = Session::default();

    let package_a = parse_definition(
        "package A\n  package Electrical\n    model Resistor\n      Real v;\n    equation\n      der(v) = 1;\n    end Resistor;\n  end Electrical;\nend A;\n",
        "A/package.mo",
    );
    let package_b_v1 =
        parse_definition("package B\n  model M1\n  end M1;\nend B;\n", "B/package.mo");
    let package_b_v2 =
        parse_definition("package B\n  model M2\n  end M2;\nend B;\n", "B/package.mo");

    assert_eq!(
        session.replace_parsed_source_set(
            "workspace::A",
            SourceRootKind::Workspace,
            vec![("A/package.mo".to_string(), package_a)],
            None,
        ),
        1
    );
    assert_eq!(
        session.replace_parsed_source_set(
            "workspace::B",
            SourceRootKind::Workspace,
            vec![("B/package.mo".to_string(), package_b_v1)],
            None,
        ),
        1
    );

    session
        .namespace_index_query("")
        .expect("prime namespace cache");
    let before = session
        .namespace_fingerprint_cached("A.")
        .expect("A namespace fingerprint");

    assert_eq!(
        session.replace_parsed_source_set(
            "workspace::B",
            SourceRootKind::Workspace,
            vec![("B/package.mo".to_string(), package_b_v2)],
            None,
        ),
        1
    );
    session
        .namespace_index_query("")
        .expect("rebuild namespace cache");
    let after = session
        .namespace_fingerprint_cached("A.")
        .expect("A namespace fingerprint after rebuild");

    assert_eq!(
        before, after,
        "unrelated workspace root changes should not perturb A namespace closure fingerprint"
    );
}

#[test]
fn nested_subtree_interface_change_updates_ancestors_but_not_siblings() {
    let mut session = Session::default();
    let root_v1 = vec![
        (
            "A/package.mo".to_string(),
            parse_definition("within ;\npackage A\nend A;\n", "A/package.mo"),
        ),
        (
            "A/Sub1/package.mo".to_string(),
            parse_definition("within A;\npackage Sub1\nend Sub1;\n", "A/Sub1/package.mo"),
        ),
        (
            "A/Sub1/M.mo".to_string(),
            parse_definition(
                "within A.Sub1;\nmodel M\n  Real x;\nequation\n  der(x) = 1;\nend M;\n",
                "A/Sub1/M.mo",
            ),
        ),
        (
            "A/Sub2/package.mo".to_string(),
            parse_definition("within A;\npackage Sub2\nend Sub2;\n", "A/Sub2/package.mo"),
        ),
        (
            "A/Sub2/N.mo".to_string(),
            parse_definition("within A.Sub2;\nmodel N\nend N;\n", "A/Sub2/N.mo"),
        ),
    ];
    assert_eq!(
        session.replace_parsed_source_set(
            "workspace::A",
            SourceRootKind::Workspace,
            root_v1.clone(),
            None,
        ),
        5
    );

    session
        .namespace_index_query("")
        .expect("prime namespace cache");
    let root_before = session
        .namespace_fingerprint_cached("A.")
        .expect("A namespace fingerprint before interface change");
    let sub1_before = session
        .namespace_fingerprint_cached("A.Sub1.")
        .expect("A.Sub1 namespace fingerprint before interface change");
    let sub2_before = session
        .namespace_fingerprint_cached("A.Sub2.")
        .expect("A.Sub2 namespace fingerprint before interface change");

    let root_v2 = vec![
        (
            "A/package.mo".to_string(),
            parse_definition("within ;\npackage A\nend A;\n", "A/package.mo"),
        ),
        (
            "A/Sub1/package.mo".to_string(),
            parse_definition("within A;\npackage Sub1\nend Sub1;\n", "A/Sub1/package.mo"),
        ),
        (
            "A/Sub1/M.mo".to_string(),
            parse_definition(
                "within A.Sub1;\nmodel M\n  Real x;\n  parameter Real gain = 1;\nequation\n  der(x) = gain;\nend M;\n",
                "A/Sub1/M.mo",
            ),
        ),
        (
            "A/Sub2/package.mo".to_string(),
            parse_definition("within A;\npackage Sub2\nend Sub2;\n", "A/Sub2/package.mo"),
        ),
        (
            "A/Sub2/N.mo".to_string(),
            parse_definition("within A.Sub2;\nmodel N\nend N;\n", "A/Sub2/N.mo"),
        ),
    ];
    assert_eq!(
        session.replace_parsed_source_set("workspace::A", SourceRootKind::Workspace, root_v2, None),
        5
    );

    session
        .namespace_index_query("")
        .expect("rebuild namespace cache after interface change");
    let root_after = session
        .namespace_fingerprint_cached("A.")
        .expect("A namespace fingerprint after interface change");
    let sub1_after = session
        .namespace_fingerprint_cached("A.Sub1.")
        .expect("A.Sub1 namespace fingerprint after interface change");
    let sub2_after = session
        .namespace_fingerprint_cached("A.Sub2.")
        .expect("A.Sub2 namespace fingerprint after interface change");

    assert_ne!(
        root_before, root_after,
        "interface changes in A.Sub1 should perturb the containing root fingerprint"
    );
    assert_ne!(
        sub1_before, sub1_after,
        "interface changes in A.Sub1 should perturb that subtree fingerprint"
    );
    assert_eq!(
        sub2_before, sub2_after,
        "interface changes in A.Sub1 should not perturb sibling subtree fingerprints"
    );
}

#[test]
fn detached_membership_edit_updates_affected_namespace_subtree_only() {
    let mut session = Session::default();
    let root = vec![
        (
            "A/package.mo".to_string(),
            parse_definition("within ;\npackage A\nend A;\n", "A/package.mo"),
        ),
        (
            "A/Sub1/package.mo".to_string(),
            parse_definition("within A;\npackage Sub1\nend Sub1;\n", "A/Sub1/package.mo"),
        ),
        (
            "A/Sub1/M.mo".to_string(),
            parse_definition("within A.Sub1;\nmodel M\nend M;\n", "A/Sub1/M.mo"),
        ),
        (
            "A/Sub2/package.mo".to_string(),
            parse_definition("within A;\npackage Sub2\nend Sub2;\n", "A/Sub2/package.mo"),
        ),
        (
            "A/Sub2/N.mo".to_string(),
            parse_definition("within A.Sub2;\nmodel N\nend N;\n", "A/Sub2/N.mo"),
        ),
    ];
    assert_eq!(
        session.replace_parsed_source_set("workspace::A", SourceRootKind::Workspace, root, None),
        5
    );

    session
        .namespace_index_query("")
        .expect("prime namespace cache");
    session.update_document("A/Sub1/M.mo", "within A.Sub1;\nmodel M\nend M;\n");
    session
        .namespace_index_query("")
        .expect("rebuild namespace cache after detaching source-root document");
    let sub2_before = session
        .namespace_fingerprint_cached("A.Sub2.")
        .expect("A.Sub2 namespace fingerprint before detached membership edit");

    session.update_document(
        "A/Sub1/M.mo",
        "within A.Sub1;\nmodel Renamed\nend Renamed;\n",
    );

    session
        .namespace_index_query("")
        .expect("rebuild namespace cache after detached membership edit");
    let sub1_children = session.namespace_children_cached("A.Sub1.");
    let sub2_after = session
        .namespace_fingerprint_cached("A.Sub2.")
        .expect("A.Sub2 namespace fingerprint after detached membership edit");

    assert_eq!(
        sub1_children,
        vec![("Renamed".to_string(), "A.Sub1.Renamed".to_string(), false)],
        "detached membership edits should refresh the affected namespace subtree from the live document"
    );
    assert_eq!(
        sub2_before, sub2_after,
        "detached membership edits in A.Sub1 should not perturb sibling subtree fingerprints"
    );
}

#[test]
fn within_change_updates_enclosing_subtree_but_not_sibling_fingerprint() {
    let mut session = Session::default();
    let root_v1 = vec![
        (
            "A/package.mo".to_string(),
            parse_definition("within ;\npackage A\nend A;\n", "A/package.mo"),
        ),
        (
            "A/Sub1/package.mo".to_string(),
            parse_definition("within A;\npackage Sub1\nend Sub1;\n", "A/Sub1/package.mo"),
        ),
        (
            "A/Sub1/M.mo".to_string(),
            parse_definition("within A.Sub1;\nmodel M\nend M;\n", "A/Sub1/M.mo"),
        ),
        (
            "A/Sub2/package.mo".to_string(),
            parse_definition("within A;\npackage Sub2\nend Sub2;\n", "A/Sub2/package.mo"),
        ),
        (
            "A/Sub2/N.mo".to_string(),
            parse_definition("within A.Sub2;\nmodel N\nend N;\n", "A/Sub2/N.mo"),
        ),
    ];
    assert_eq!(
        session.replace_parsed_source_set(
            "workspace::A",
            SourceRootKind::Workspace,
            root_v1.clone(),
            None
        ),
        5
    );

    session
        .namespace_index_query("")
        .expect("prime namespace cache");
    let root_before = session
        .namespace_fingerprint_cached("A.")
        .expect("A namespace fingerprint before within change");
    let sub1_before = session
        .namespace_fingerprint_cached("A.Sub1.")
        .expect("A.Sub1 namespace fingerprint before within change");
    let sub2_before = session
        .namespace_fingerprint_cached("A.Sub2.")
        .expect("A.Sub2 namespace fingerprint before within change");

    let root_v2 = vec![
        (
            "A/package.mo".to_string(),
            parse_definition("within ;\npackage A\nend A;\n", "A/package.mo"),
        ),
        (
            "A/Sub1/package.mo".to_string(),
            parse_definition("within A;\npackage Sub1\nend Sub1;\n", "A/Sub1/package.mo"),
        ),
        (
            "A/Sub1/M.mo".to_string(),
            parse_definition("within A.Sub1.Inner;\nmodel M\nend M;\n", "A/Sub1/M.mo"),
        ),
        (
            "A/Sub2/package.mo".to_string(),
            parse_definition("within A;\npackage Sub2\nend Sub2;\n", "A/Sub2/package.mo"),
        ),
        (
            "A/Sub2/N.mo".to_string(),
            parse_definition("within A.Sub2;\nmodel N\nend N;\n", "A/Sub2/N.mo"),
        ),
    ];
    assert_eq!(
        session.replace_parsed_source_set("workspace::A", SourceRootKind::Workspace, root_v2, None),
        5
    );

    session
        .namespace_index_query("")
        .expect("rebuild namespace cache after within change");
    let root_after = session
        .namespace_fingerprint_cached("A.")
        .expect("A namespace fingerprint after within change");
    let sub1_after = session
        .namespace_fingerprint_cached("A.Sub1.")
        .expect("A.Sub1 namespace fingerprint after within change");
    let sub2_after = session
        .namespace_fingerprint_cached("A.Sub2.")
        .expect("A.Sub2 namespace fingerprint after within change");

    assert_ne!(
        root_before, root_after,
        "within changes under A.Sub1 should perturb the containing root fingerprint"
    );
    assert_ne!(
        sub1_before, sub1_after,
        "within changes under A.Sub1 should perturb that subtree fingerprint"
    );
    assert_eq!(
        sub2_before, sub2_after,
        "within changes under A.Sub1 should not perturb sibling subtree fingerprints"
    );
}

#[test]
fn file_add_and_remove_under_subtree_keeps_sibling_fingerprint_warm() {
    let mut session = Session::default();
    let root_v1 = workspace_a_subtree_root_v1();
    assert_eq!(
        session.replace_parsed_source_set(
            "workspace::A",
            SourceRootKind::Workspace,
            root_v1.clone(),
            None,
        ),
        5
    );

    session
        .namespace_index_query("")
        .expect("prime namespace cache");
    let sub2_before_add = session
        .namespace_fingerprint_cached("A.Sub2.")
        .expect("A.Sub2 namespace fingerprint before add");
    let sub1_before_add = session
        .namespace_fingerprint_cached("A.Sub1.")
        .expect("A.Sub1 namespace fingerprint before add");

    let root_v2 = workspace_a_subtree_root_v2();
    assert_eq!(
        session.replace_parsed_source_set("workspace::A", SourceRootKind::Workspace, root_v2, None),
        6
    );

    session
        .namespace_index_query("")
        .expect("rebuild namespace cache after add");
    let sub1_after_add = session
        .namespace_fingerprint_cached("A.Sub1.")
        .expect("A.Sub1 namespace fingerprint after add");
    let sub2_after_add = session
        .namespace_fingerprint_cached("A.Sub2.")
        .expect("A.Sub2 namespace fingerprint after add");

    assert_ne!(
        sub1_before_add, sub1_after_add,
        "adding a file under A.Sub1 should perturb that subtree fingerprint"
    );
    assert_eq!(
        sub2_before_add, sub2_after_add,
        "adding a file under A.Sub1 should not perturb sibling subtree fingerprints"
    );

    assert_eq!(
        session.replace_parsed_source_set("workspace::A", SourceRootKind::Workspace, root_v1, None),
        5
    );

    session
        .namespace_index_query("")
        .expect("rebuild namespace cache after remove");
    let sub2_after_remove = session
        .namespace_fingerprint_cached("A.Sub2.")
        .expect("A.Sub2 namespace fingerprint after remove");
    let sub1_after_remove = session
        .namespace_fingerprint_cached("A.Sub1.")
        .expect("A.Sub1 namespace fingerprint after remove");

    assert_ne!(
        sub1_after_add, sub1_after_remove,
        "removing a file under A.Sub1 should perturb that subtree fingerprint again"
    );
    assert_eq!(
        sub2_after_add, sub2_after_remove,
        "removing a file under A.Sub1 should not perturb sibling subtree fingerprints"
    );
}

#[test]
fn subtree_refresh_keeps_sibling_namespace_fingerprint_warm_across_compile() {
    let mut session = Session::default();
    let root = workspace_a_subtree_root_v1();
    assert_eq!(
        session.replace_parsed_source_set("workspace::A", SourceRootKind::Workspace, root, None),
        5
    );
    session
        .add_document(
            "Ball.mo",
            "model Ball\n  A.Sub1.M m;\n  A.Sub2.N n;\nend Ball;\n",
        )
        .expect("focus model should parse");

    session
        .namespace_index_query("")
        .expect("prime namespace cache");
    let sub2_before = session
        .namespace_fingerprint_cached("A.Sub2.")
        .expect("A.Sub2 namespace fingerprint before subtree refresh");

    let open_error = session.update_document(
        "A/Sub1/M.mo",
        "within A.Sub1;\nmodel M\n  Real x(start=0);\nequation\n  der(x) = 1;\nend M;\n",
    );
    assert!(
        open_error.is_none(),
        "detaching the source-root-backed document should stay parseable"
    );
    let parse_error = session.update_document(
        "A/Sub1/M.mo",
        "within A.Sub1;\nmodel M\n  Real x(start=0);\n  parameter Real gain = 1;\nequation\n  der(x) = gain;\nend M;\n",
    );
    assert!(
        parse_error.is_none(),
        "structural subtree edit should stay parseable"
    );

    let plan = session
        .apply_source_root_refresh_plan("workspace::A")
        .expect("dirty workspace root should apply a subtree refresh plan");
    assert!(
        !plan.full_root_fallback,
        "ordinary subtree edits should stay on the subtree refresh path"
    );
    assert!(
        session.dirty_source_root_keys().is_empty(),
        "applying the subtree refresh should clear pending refresh state"
    );

    let phases = session
        .compile_model_phases("Ball")
        .expect("compile after subtree refresh should succeed");
    assert!(
        matches!(phases, PhaseResult::Success(_)),
        "compile after subtree refresh should stay on the success path"
    );
    let sub2_after = session
        .namespace_fingerprint_cached("A.Sub2.")
        .expect("A.Sub2 namespace fingerprint after subtree refresh compile");

    assert_eq!(
        sub2_before, sub2_after,
        "compile after refreshing A.Sub1 should keep the unaffected A.Sub2 subtree warm"
    );
}

#[test]
fn partitioned_workspace_family_sync_keeps_unrelated_package_fingerprints_warm() {
    let mut session = Session::default();
    let workspace_v1 = partitioned_workspace_definitions_v1();

    assert_eq!(
        session.sync_partitioned_source_root_family(
            "workspace::project",
            SourceRootKind::Workspace,
            workspace_v1,
            None,
            None,
        ),
        3
    );

    session
        .namespace_index_query("")
        .expect("prime namespace cache");
    let new_folder_id = session
        .source_set_id("workspace::project::NewFolder")
        .expect("NewFolder source set should exist");
    let other_id = session
        .source_set_id("workspace::project::Other")
        .expect("Other source set should exist");
    let new_folder_signature_before = source_root_namespace_signature(&session, new_folder_id);
    let other_signature_before = source_root_namespace_signature(&session, other_id);
    let other_fingerprint_before = session
        .namespace_fingerprint_cached("Other.")
        .expect("Other namespace fingerprint");

    let workspace_v2 = partitioned_workspace_definitions_v2();

    assert_eq!(
        session.sync_partitioned_source_root_family(
            "workspace::project",
            SourceRootKind::Workspace,
            workspace_v2,
            None,
            None,
        ),
        3
    );
    assert_namespace_source_set_rebuild_pending(
        &session,
        new_folder_id,
        &new_folder_signature_before,
    );
    assert_namespace_source_set_signature(&session, other_id, &other_signature_before);
    assert!(
        session
            .query_state
            .ast
            .source_root_namespace_cache
            .as_ref()
            .is_some_and(|cache| cache.merged_cache.is_none()),
        "merged cache should rebuild lazily after the family resync"
    );

    session
        .namespace_index_query("")
        .expect("rebuild namespace cache");
    let other_fingerprint_after = session
        .namespace_fingerprint_cached("Other.")
        .expect("Other namespace fingerprint after rebuild");

    assert_eq!(
        other_fingerprint_before, other_fingerprint_after,
        "changing one top-level workspace package root should not perturb another root"
    );
    assert_namespace_source_set_signature(&session, other_id, &other_signature_before);
}

#[test]
fn workspace_source_set_scoped_invalidation_keeps_other_namespace_cache_entries_warm() {
    let mut session = Session::default();
    let (source_set_a, source_set_b, a_signature_before, b_signature_before) =
        load_two_workspace_source_sets(&mut session);
    let parsed_b_v2 = parse_definition(
        "package B\n  model MB\n  end MB;\n  model MB2\n  end MB2;\nend B;\n",
        "B/package.mo",
    );
    let replaced = session.replace_parsed_source_set(
        "workspace::B",
        SourceRootKind::Workspace,
        vec![("B/package.mo".to_string(), parsed_b_v2)],
        None,
    );
    let cache_after_replace = session
        .query_state
        .ast
        .source_root_namespace_cache
        .as_ref()
        .expect("cache should remain after scoped invalidation");

    assert_eq!(replaced, 1, "B replacement should update one document");
    assert_namespace_source_set_signature(&session, source_set_a, &a_signature_before);
    assert_namespace_source_set_rebuild_pending(&session, source_set_b, &b_signature_before);
    assert!(
        cache_after_replace.merged_cache.is_none(),
        "merged cache must rebuild lazily"
    );

    let class_names = namespace_class_names(&mut session);
    assert!(
        class_names.contains(&"B.MB2".to_string()),
        "updated B cache should include MB2"
    );

    let cache_after_rebuild = session
        .query_state
        .ast
        .source_root_namespace_cache
        .as_ref()
        .expect("workspace namespace cache should be present");

    assert_namespace_source_set_signature(&session, source_set_a, &a_signature_before);
    assert!(
        session
            .query_state
            .ast
            .package_def_map
            .source_set_caches
            .get(&source_set_b)
            .is_some_and(|entry| entry.signature != b_signature_before),
        "B source-set membership cache should be rebuilt"
    );
    assert!(
        cache_after_rebuild
            .merged_source_set_signatures
            .get(&source_set_a)
            .is_some_and(|signature| *signature == a_signature_before),
        "A merged source-set signature should be reused"
    );
    assert!(
        cache_after_rebuild
            .merged_source_set_signatures
            .get(&source_set_b)
            .is_some_and(|signature| *signature != b_signature_before),
        "B merged source-set signature should be rebuilt"
    );
    assert!(
        cache_after_rebuild.merged_cache.is_some(),
        "merged cache should be rebuilt"
    );
}
