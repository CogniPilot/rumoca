use super::*;
use std::path::Path;

#[test]
fn removing_live_source_root_document_restores_latest_detached_source_root_document() {
    let mut session = Session::default();
    let uri = "external/Lib.mo";
    let parsed_v1 = parse_definition("package Lib\n  model A\n  end A;\nend Lib;\n", uri);
    let parsed_v2 = parse_definition("package Lib\n  model B\n  end B;\nend Lib;\n", uri);

    session.replace_parsed_source_set(
        "external",
        SourceRootKind::External,
        vec![(uri.to_string(), parsed_v1)],
        None,
    );
    let parse_error =
        session.update_document(uri, "package Lib\n  model Open\n  end Open;\nend Lib;\n");
    assert!(
        parse_error.is_none(),
        "live source-root edit should parse cleanly"
    );

    session.replace_parsed_source_set(
        "external",
        SourceRootKind::External,
        vec![(uri.to_string(), parsed_v2.clone())],
        Some(uri),
    );
    session.remove_document(uri);

    let restored = session.get_document(uri).cloned().expect(
        "closing a live source-root document should restore the cached source-root document",
    );
    assert!(
        restored.content.is_empty(),
        "restored source-root-backed document should return to parsed-source-set ownership"
    );
    assert_eq!(
        restored.parsed(),
        Some(&parsed_v2),
        "source-root rebuilds while the file is open should refresh the detached backing document"
    );
    assert!(
        session
            .source_set_uris("external")
            .is_some_and(|uris| uris.contains(uri)),
        "restoring a detached source-root document should reattach it to the source root"
    );
}

#[test]
fn detached_source_root_edits_keep_source_root_membership_and_mark_source_root_for_refresh() {
    let mut session = Session::default();
    let uri = "external/Lib.mo";
    let parsed = parse_definition("package Lib\n  model A\n  end A;\nend Lib;\n", uri);

    session.replace_parsed_source_set(
        "external",
        SourceRootKind::External,
        vec![(uri.to_string(), parsed)],
        None,
    );
    session.update_document(uri, "package Lib\n  model Open\n  end Open;\nend Lib;\n");
    assert!(
        session.is_non_workspace_source_root_document(uri),
        "detached live source-root documents should still resolve as loaded source-root documents"
    );
    assert!(
        session.dirty_non_workspace_source_root_keys().is_empty(),
        "opening a detached source-root document should not mark the source root dirty yet"
    );
    assert_eq!(
        session.non_workspace_source_set_ids_for_uri(uri),
        vec![
            session
                .source_set_id("external")
                .expect("source-set id should exist")
        ],
        "detached live source-root documents should still resolve to their external source root"
    );

    session.update_document(
        uri,
        "package Lib\n  model Open\n    equation\n      1 = 1;\n  end Open;\nend Lib;\n",
    );
    assert!(
        session.dirty_non_workspace_source_root_keys().is_empty(),
        "body-only edits in an already-detached non-workspace document should not dirty the source root"
    );
    assert!(
        session
            .dirty_source_root_class_prefixes("external")
            .is_empty(),
        "body-only edits should not accumulate dirty class prefixes"
    );

    session.update_document(
        uri,
        "package Lib\n  model Open\n    Real x;\n  end Open;\nend Lib;\n",
    );
    assert_eq!(
        session.dirty_non_workspace_source_root_keys(),
        vec!["external".to_string()],
        "editing an already-detached source-root document should mark its source root for refresh"
    );
    assert_eq!(
        session.dirty_source_root_class_prefixes("external"),
        vec!["Lib.Open".to_string(), "Lib".to_string()],
        "interface edits should dirty the changed class and its ancestors"
    );

    let refreshed = parse_definition("package Lib\n  model B\n  end B;\nend Lib;\n", uri);
    session.replace_parsed_source_set(
        "external",
        SourceRootKind::External,
        vec![(uri.to_string(), refreshed)],
        Some(uri),
    );
    assert!(
        session.dirty_non_workspace_source_root_keys().is_empty(),
        "reloading the source root should clear the pending-refresh flag"
    );
}

#[test]
fn detached_workspace_source_root_edits_mark_workspace_source_root_for_refresh() {
    let mut session = Session::default();
    let uri = "workspace/NewFolder/Test.mo";
    let parsed = parse_definition("within NewFolder;\nmodel Test\nend Test;\n", uri);

    session.replace_parsed_source_set(
        "workspace",
        SourceRootKind::Workspace,
        vec![(uri.to_string(), parsed)],
        None,
    );

    session.update_document(uri, "within NewFolder;\nmodel Test\n  Real x;\nend Test;\n");
    assert!(
        !session
            .source_set_uris("workspace")
            .is_some_and(|uris| uris.contains(uri)),
        "opening a source-root-backed workspace document should detach the parsed backing entry"
    );
    assert!(
        !source_set_record(&session, "workspace").needs_refresh,
        "opening the detached workspace document should not mark the source root dirty yet"
    );

    session.update_document(
        uri,
        "within NewFolder;\nmodel Test\n  Real x;\nequation\n  x = 1;\nend Test;\n",
    );
    assert!(
        !source_set_record(&session, "workspace").needs_refresh,
        "body-only edits in an already-detached workspace document should not dirty the source root"
    );
    assert!(
        session
            .dirty_source_root_class_prefixes("workspace")
            .is_empty(),
        "body-only edits should not accumulate dirty class prefixes"
    );

    session.update_document(
        uri,
        "within NewFolder;\nmodel Test\n  Real x;\n  Real y;\nend Test;\n",
    );
    assert!(
        source_set_record(&session, "workspace").needs_refresh,
        "editing an already-detached workspace source-root document should mark the source root dirty"
    );
    assert_eq!(
        session.dirty_source_root_class_prefixes("workspace"),
        vec!["NewFolder.Test".to_string(), "NewFolder".to_string()],
        "interface edits should dirty the changed class and its ancestors"
    );
    let pending_status = session
        .source_root_status("workspace")
        .expect("workspace source root should expose status");
    assert_eq!(
        pending_status.current,
        Some(SourceRootActivitySnapshot {
            kind: SourceRootActivityKind::SubtreeReindex,
            phase: SourceRootActivityPhase::Pending,
            dirty_class_prefixes: vec!["NewFolder.Test".to_string(), "NewFolder".to_string()],
        }),
        "interface edits should surface a pending subtree reindex"
    );
    assert_eq!(
        pending_status.last_completed, None,
        "no subtree reindex should be completed before the refresh runs"
    );

    let refreshed = parse_definition(
        "within NewFolder;\nmodel Test\n  Real x;\n  Real y;\nend Test;\n",
        uri,
    );
    session.replace_parsed_source_set(
        "workspace",
        SourceRootKind::Workspace,
        vec![(uri.to_string(), refreshed)],
        Some(uri),
    );

    let completed_status = session
        .source_root_status("workspace")
        .expect("workspace source root should keep status after refresh");
    assert_eq!(
        completed_status.current, None,
        "refresh completion should clear the in-flight subtree reindex state"
    );
    assert_eq!(
        completed_status.last_completed,
        Some(SourceRootActivitySnapshot {
            kind: SourceRootActivityKind::SubtreeReindex,
            phase: SourceRootActivityPhase::Completed,
            dirty_class_prefixes: vec!["NewFolder.Test".to_string(), "NewFolder".to_string()],
        }),
        "refresh completion should retain the last completed subtree reindex summary"
    );
}

#[test]
fn detached_workspace_membership_edits_record_old_and_new_subtree_prefixes() {
    let mut session = Session::default();
    let uri = "workspace/NewFolder/Test.mo";
    let parsed = parse_definition("within NewFolder;\nmodel Test\nend Test;\n", uri);

    session.replace_parsed_source_set(
        "workspace",
        SourceRootKind::Workspace,
        vec![(uri.to_string(), parsed)],
        None,
    );

    session.update_document(uri, "within NewFolder;\nmodel Test\n  Real x;\nend Test;\n");
    assert!(
        !source_set_record(&session, "workspace").needs_refresh,
        "opening the detached workspace document should not mark the source root dirty yet"
    );

    session.update_document(uri, "within NewFolder;\nmodel Other\nend Other;\n");

    assert!(
        source_set_record(&session, "workspace").needs_refresh,
        "membership edits should mark the owning source root dirty"
    );
    assert_eq!(
        session.dirty_source_root_class_prefixes("workspace"),
        vec![
            "NewFolder.Test".to_string(),
            "NewFolder".to_string(),
            "NewFolder.Other".to_string(),
        ],
        "membership edits should carry both old and new subtree prefixes"
    );
}

#[test]
fn source_root_indexing_coordinator_tracks_reservations_loaded_paths_and_epoch() {
    let mut session = Session::default();
    let path_key = "external::modelica";
    let source_root_path = Path::new("/tmp/Modelica");
    let uri = "external/Lib.mo";
    let parsed = parse_definition("package Lib\n  model A\n  end A;\nend Lib;\n", uri);

    assert_eq!(session.source_root_state_epoch(), 0);
    assert!(session.reserve_source_root_load(path_key, 0));
    assert_eq!(
        session.source_root_load_reservation_epoch(path_key),
        Some(0)
    );
    assert!(
        !session.reserve_source_root_load(path_key, 0),
        "duplicate in-flight reservations should be rejected"
    );

    session.cancel_source_root_load(path_key, 1);
    assert_eq!(
        session.source_root_load_reservation_epoch(path_key),
        Some(0),
        "mismatched owner epochs must not clear the reservation"
    );

    let applied = session.apply_parsed_source_root_if_current(
        "external",
        ParsedSourceRootLoad {
            source_root_kind: SourceRootKind::External,
            source_root_path,
            cache_status: SourceRootCacheStatus::Disabled,
            path_key,
            current_document_path: None,
            documents: vec![(uri.to_string(), parsed)],
            expected_epoch: 0,
        },
    );
    assert_eq!(
        applied.map(|(inserted, _)| inserted),
        Some(1),
        "current reservations should apply and report inserted files"
    );
    assert!(session.is_source_root_path_loaded(path_key));
    assert_eq!(session.source_root_load_reservation_epoch(path_key), None);
    assert_eq!(
        session.source_root_state_epoch(),
        1,
        "successful source-root apply should advance the coordinator epoch"
    );
}

#[test]
fn source_root_read_prewarm_state_is_session_owned_and_clears_on_revision_change() {
    let mut session = Session::default();
    let initial_revision = session.revision();

    assert!(session.begin_source_root_read_prewarm(initial_revision));
    assert!(session.source_root_read_prewarm_is_pending(initial_revision));
    assert!(
        !session.begin_source_root_read_prewarm(initial_revision),
        "the same revision should not enqueue duplicate prewarm work"
    );

    session.finish_source_root_read_prewarm(initial_revision.saturating_add(1));
    assert!(
        session.source_root_read_prewarm_is_pending(initial_revision),
        "finishing a different revision must not clear the active prewarm marker"
    );

    session.replace_parsed_source_set(
        "workspace",
        SourceRootKind::Workspace,
        vec![(
            "/tmp/active.mo".to_string(),
            parse_definition("model Active\nend Active;\n", "/tmp/active.mo"),
        )],
        None,
    );
    assert!(
        !session.source_root_read_prewarm_is_pending(initial_revision),
        "revision changes should clear stale prewarm state"
    );

    let epoch_before_reset = session.source_root_state_epoch();
    session.reset_to_open_documents();
    assert_eq!(
        session.source_root_state_epoch(),
        epoch_before_reset.saturating_add(1),
        "reset should preserve coordinator ownership while advancing the epoch"
    );
}

#[test]
fn source_root_read_prewarm_finish_promotes_namespace_cache_to_host_session() {
    let mut session = Session::default();
    session.replace_parsed_source_set(
        "Modelica",
        SourceRootKind::DurableExternal,
        vec![(
            "Modelica/package.mo".to_string(),
            parse_definition(
                "package Modelica\n  package Electrical\n    package Analog\n      model Resistor\n      end Resistor;\n    end Analog;\n  end Electrical;\nend Modelica;\n",
                "Modelica/package.mo",
            ),
        )],
        None,
    );

    let revision = session.revision();
    assert!(session.begin_source_root_read_prewarm(revision));

    let snapshot = session.snapshot();
    snapshot.prewarm_source_root_read_queries();

    assert!(
        session.namespace_class_names_cached().is_empty(),
        "host session should remain cold until the background prewarm is marked complete"
    );

    session.finish_source_root_read_prewarm(revision);

    assert!(
        session
            .namespace_class_names_cached()
            .iter()
            .any(|name| name == "Modelica.Electrical.Analog.Resistor"),
        "finishing a source-root read prewarm should merge warmed namespace completion back into the host session"
    );
}

#[test]
fn workspace_refresh_plan_targets_detached_subtree_without_full_root_fallback() {
    let mut session = Session::default();
    let uri = "workspace/NewFolder/Test.mo";
    let parsed = parse_definition("within NewFolder;\nmodel Test\nend Test;\n", uri);

    session.replace_parsed_source_set(
        "workspace",
        SourceRootKind::Workspace,
        vec![(uri.to_string(), parsed)],
        None,
    );
    session.update_document(uri, "within NewFolder;\nmodel Test\n  Real x;\nend Test;\n");
    session.update_document(
        uri,
        "within NewFolder;\nmodel Test\n  Real x;\n  Real y;\nend Test;\n",
    );

    let plan = session
        .source_root_refresh_plan("workspace")
        .expect("dirty workspace root should produce a refresh plan");

    assert_eq!(
        plan.dirty_class_prefixes,
        vec!["NewFolder.Test".to_string(), "NewFolder".to_string()],
        "the refresh plan should retain the full dirty prefix chain"
    );
    assert_eq!(
        plan.refresh_class_prefixes,
        vec!["NewFolder.Test".to_string()],
        "ancestor-only prefixes should collapse to the leaf refresh subtree"
    );
    assert_eq!(
        plan.affected_uris,
        vec![uri.to_string()],
        "the detached edited document should be the only affected file"
    );
    assert_eq!(
        plan.unmatched_class_prefixes,
        Vec::<String>::new(),
        "interface-only subtree edits should be fully explainable from current class summaries"
    );
    assert!(
        plan.rebuild_package_membership,
        "structural edits should still request package-membership refresh"
    );
    assert!(
        !plan.full_root_fallback,
        "a local subtree edit should not require whole-root fallback planning"
    );
}

#[test]
fn workspace_refresh_plan_falls_back_when_membership_move_loses_old_subtree_match() {
    let mut session = Session::default();
    let uri = "workspace/NewFolder/Test.mo";
    let parsed = parse_definition("within NewFolder;\nmodel Test\nend Test;\n", uri);

    session.replace_parsed_source_set(
        "workspace",
        SourceRootKind::Workspace,
        vec![(uri.to_string(), parsed)],
        None,
    );
    session.update_document(uri, "within NewFolder;\nmodel Test\n  Real x;\nend Test;\n");
    session.update_document(uri, "within NewFolder;\nmodel Other\nend Other;\n");

    let plan = session
        .source_root_refresh_plan("workspace")
        .expect("dirty workspace root should produce a refresh plan");

    assert_eq!(
        plan.refresh_class_prefixes,
        vec!["NewFolder.Test".to_string(), "NewFolder.Other".to_string()],
        "the plan should keep both old and new leaf subtree prefixes for a membership move"
    );
    assert_eq!(
        plan.affected_uris,
        vec![uri.to_string()],
        "the moved detached document should still be recognized as the affected file"
    );
    assert_eq!(
        plan.unmatched_class_prefixes,
        vec!["NewFolder.Test".to_string()],
        "the removed old subtree should force fallback because the current live summaries no longer describe it"
    );
    assert!(
        plan.full_root_fallback,
        "membership moves that lose the old subtree from live summaries should require whole-root fallback"
    );
}

#[test]
fn apply_workspace_refresh_plan_clears_dirty_state_without_full_root_fallback() {
    let mut session = Session::default();
    let uri = "workspace/NewFolder/Test.mo";
    let parsed = parse_definition("within NewFolder;\nmodel Test\nend Test;\n", uri);

    session.replace_parsed_source_set(
        "workspace",
        SourceRootKind::Workspace,
        vec![(uri.to_string(), parsed)],
        None,
    );
    session
        .namespace_children_for_completion("NewFolder.")
        .expect("namespace cache should warm before the edit");
    session.update_document(uri, "within NewFolder;\nmodel Test\n  Real x;\nend Test;\n");
    session.update_document(
        uri,
        "within NewFolder;\nmodel Test\n  Real x;\n  Real y;\nend Test;\n",
    );

    let plan = session
        .apply_source_root_refresh_plan("workspace")
        .expect("dirty workspace root should apply a refresh plan");

    assert!(
        !plan.full_root_fallback,
        "ordinary subtree interface edits should stay on the session-owned subtree refresh path"
    );
    assert!(
        session.dirty_source_root_keys().is_empty(),
        "applying a subtree refresh plan should clear the pending refresh state"
    );
    assert_eq!(
        session
            .namespace_children_for_completion("NewFolder.")
            .expect("namespace completion should remain available"),
        vec![("Test".to_string(), "NewFolder.Test".to_string(), false)],
        "subtree refresh should keep namespace membership available without a full source-root reload"
    );
    assert_eq!(
        session
            .source_root_status("workspace")
            .expect("workspace status should exist")
            .last_completed,
        Some(SourceRootActivitySnapshot {
            kind: SourceRootActivityKind::SubtreeReindex,
            phase: SourceRootActivityPhase::Completed,
            dirty_class_prefixes: vec!["NewFolder.Test".to_string(), "NewFolder".to_string()],
        }),
        "subtree refresh should report a completed subtree reindex"
    );
}

#[test]
fn detached_source_root_interface_edits_share_refresh_plan_behavior_across_root_kinds() {
    let scenarios = [
        ("workspace", SourceRootKind::Workspace),
        ("external", SourceRootKind::External),
        ("durable", SourceRootKind::DurableExternal),
    ];

    for (source_root_key, kind) in scenarios {
        let mut session = Session::default();
        let uri = format!("{source_root_key}/NewFolder/Test.mo");
        let parsed = parse_definition("within NewFolder;\nmodel Test\nend Test;\n", &uri);

        session.replace_parsed_source_set(source_root_key, kind, vec![(uri.clone(), parsed)], None);
        session.update_document(
            &uri,
            "within NewFolder;\nmodel Test\n  Real x;\nend Test;\n",
        );
        session.update_document(
            &uri,
            "within NewFolder;\nmodel Test\n  Real x;\n  Real y;\nend Test;\n",
        );

        let plan = session
            .source_root_refresh_plan(source_root_key)
            .expect("dirty source root should produce a refresh plan");
        assert_eq!(
            plan.dirty_class_prefixes,
            vec!["NewFolder.Test".to_string(), "NewFolder".to_string()],
            "{kind:?} interface edits should dirty the changed class and its ancestors"
        );
        assert_eq!(
            plan.refresh_class_prefixes,
            vec!["NewFolder.Test".to_string()],
            "{kind:?} refresh planning should collapse to the leaf subtree"
        );
        assert_eq!(
            plan.affected_uris,
            vec![uri.clone()],
            "{kind:?} refresh planning should target the detached edited document"
        );
        assert!(
            !plan.full_root_fallback,
            "{kind:?} ordinary subtree edits should not require whole-root fallback"
        );

        let applied = session
            .apply_source_root_refresh_plan(source_root_key)
            .expect("dirty source root should apply a refresh plan");
        assert!(
            !applied.full_root_fallback,
            "{kind:?} applied refresh should stay on the subtree path"
        );
        assert!(
            session.dirty_source_root_keys().is_empty(),
            "{kind:?} applying the refresh plan should clear pending refresh state"
        );
        assert_eq!(
            session
                .source_root_status(source_root_key)
                .expect("source-root status should exist")
                .last_completed,
            Some(SourceRootActivitySnapshot {
                kind: SourceRootActivityKind::SubtreeReindex,
                phase: SourceRootActivityPhase::Completed,
                dirty_class_prefixes: vec!["NewFolder.Test".to_string(), "NewFolder".to_string()],
            }),
            "{kind:?} refresh completion should report the same subtree reindex summary"
        );
    }
}

#[test]
fn external_and_durable_external_roots_share_lookup_completion_and_compile_behavior() {
    let source_root_snapshot = |kind| {
        let mut session = Session::default();
        session.replace_parsed_source_set(
            "source-root",
            kind,
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
                        "within NewFolder;\nmodel Test\n  Real x;\n equation\n  der(x) = 1;\nend Test;\n",
                        "NewFolder/Test.mo",
                    ),
                ),
            ],
            None,
        );
        session
            .add_document(
                "Ball.mo",
                "model Ball\n  import NewFolder.Test;\n  Test test;\nend Ball;\n",
            )
            .expect("workspace document should parse");

        let lookup = session.class_lookup_query("NewFolder.Test");
        let namespace_children = session
            .namespace_index_query("NewFolder.")
            .expect("namespace completion should succeed");
        let completion_names = session
            .all_class_names_for_completion()
            .expect("class-name completion should succeed");
        let diagnostics = session.compile_model_diagnostics("Ball");
        session
            .compile_model_phases("Ball")
            .expect("compile should succeed");

        (
            session.source_root_durability("source-root"),
            lookup,
            namespace_children,
            completion_names,
            diagnostics
                .diagnostics
                .into_iter()
                .map(|diagnostic| (diagnostic.severity, diagnostic.code, diagnostic.message))
                .collect::<Vec<_>>(),
        )
    };

    let external_root = source_root_snapshot(SourceRootKind::External);
    let durable = source_root_snapshot(SourceRootKind::DurableExternal);

    assert_eq!(
        external_root.0,
        Some(SourceRootDurability::Normal),
        "plain external roots should keep normal durability"
    );
    assert_eq!(
        durable.0,
        Some(SourceRootDurability::Durable),
        "durable external roots should keep durable retention metadata"
    );
    assert_eq!(
        external_root.1, durable.1,
        "class lookup should not depend on external-root durability"
    );
    assert_eq!(
        external_root.2, durable.2,
        "namespace completion should not depend on external-root durability"
    );
    assert_eq!(
        external_root.3, durable.3,
        "class-name completion should not depend on external-root durability"
    );
    assert_eq!(
        external_root.4, durable.4,
        "compile diagnostics should not depend on external-root durability"
    );
}
