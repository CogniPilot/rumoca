use super::*;

fn write_source_root_file(path: &std::path::Path, contents: &str) {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("mkdirs");
    }
    std::fs::write(path, contents).expect("write source-root file");
}

fn write_two_file_source_root(root: &std::path::Path) {
    write_source_root_file(&root.join("package.mo"), "package Lib\nend Lib;\n");
    write_source_root_file(
        &root.join("M.mo"),
        "within Lib;\nmodel M\n  Real x;\nend M;\n",
    );
}

fn index_source_root_with_cache(
    session: &mut Session,
    cache_dir: &std::path::Path,
    source_root_dir: &std::path::Path,
) -> SourceRootLoadReport {
    session.load_source_root_tolerant_with_cache_dir_for_tests(
        "external::Lib",
        SourceRootKind::External,
        source_root_dir,
        None,
        Some(cache_dir),
    )
}

fn source_set_workspace_symbol_cache(
    snapshot: &SessionSnapshot,
    source_set_id: SourceSetId,
) -> Arc<SourceSetWorkspaceSymbolCache> {
    let snapshot_session = snapshot
        .session
        .lock()
        .expect("workspace-symbol snapshot lock should not be poisoned");
    snapshot_session
        .query_state
        .ast
        .workspace_symbol_query_cache
        .as_ref()
        .and_then(|cache| cache.source_set_caches.get(&source_set_id))
        .cloned()
        .expect("workspace-symbol source-set cache should exist")
}

fn detached_workspace_symbol_cache(
    snapshot: &SessionSnapshot,
) -> Arc<DetachedWorkspaceSymbolCache> {
    let snapshot_session = snapshot
        .session
        .lock()
        .expect("workspace-symbol snapshot lock should not be poisoned");
    snapshot_session
        .query_state
        .ast
        .workspace_symbol_query_cache
        .as_ref()
        .and_then(|cache| cache.detached_cache.as_ref())
        .cloned()
        .expect("workspace-symbol detached cache should exist")
}

fn session_detached_workspace_symbol_cache(session: &Session) -> Arc<DetachedWorkspaceSymbolCache> {
    session
        .query_state
        .ast
        .workspace_symbol_query_cache
        .as_ref()
        .and_then(|cache| cache.detached_cache.as_ref())
        .cloned()
        .expect("session workspace-symbol detached cache should exist")
}

fn assert_workspace_symbol_snapshot_is_narrow(snapshot: &SessionSnapshot) {
    let snapshot_session = snapshot
        .session
        .lock()
        .expect("workspace-symbol snapshot lock should not be poisoned");
    assert!(
        snapshot_session.source_sets.is_empty(),
        "warm workspace-symbol snapshots should not clone durable source-set records"
    );
    assert_eq!(
        snapshot_session.source_set_keys.len(),
        0,
        "warm workspace-symbol snapshots should not carry source-root key maps"
    );
    assert_eq!(
        snapshot_session.source_set_signature_overrides.len(),
        1,
        "warm workspace-symbol snapshots should retain only source-set signatures"
    );
    assert!(
        snapshot_session.file_path_keys.is_empty(),
        "warm workspace-symbol snapshots should not rebuild canonical path-key maps"
    );
    assert!(
        snapshot_session.file_uris.is_empty(),
        "warm workspace-symbol snapshots should not carry file-id uri reverse maps"
    );
    assert!(
        snapshot_session.file_revisions.is_empty(),
        "warm workspace-symbol snapshots should not clone per-file revision tables"
    );
}

fn assert_workspace_symbol_rebuild_snapshot_is_trimmed(snapshot: &SessionSnapshot) {
    let snapshot_session = snapshot
        .session
        .lock()
        .expect("workspace-symbol rebuild snapshot lock should not be poisoned");
    assert!(
        !snapshot_session.source_sets.is_empty(),
        "source-set rebuild snapshots should retain source-root membership"
    );
    assert_eq!(
        snapshot_session.source_set_keys.len(),
        0,
        "source-set rebuild snapshots should not carry source-root key maps"
    );
    assert!(
        !snapshot_session.source_set_signature_overrides.is_empty(),
        "source-set rebuild snapshots should retain source-set signatures"
    );
    assert!(
        snapshot_session.file_path_keys.is_empty(),
        "source-set rebuild snapshots should not rebuild canonical path-key maps"
    );
    assert!(
        snapshot_session.file_uris.is_empty(),
        "source-set rebuild snapshots should not carry file-id uri reverse maps"
    );
    assert!(
        snapshot_session.file_revisions.is_empty(),
        "source-set rebuild snapshots should not clone per-file revision tables"
    );
}

#[test]
fn session_workspace_symbols_keep_durable_external_roots_warm_across_local_summary_edits() {
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
    session
        .add_document("test.mo", "model LocalA\n  Real x;\nend LocalA;\n")
        .expect("local document should parse");

    let first_snapshot = session.workspace_symbol_snapshot();
    let first = first_snapshot.workspace_symbol_query("");
    assert!(
        first.iter().any(|symbol| symbol.name == "Resistor"),
        "initial workspace symbol query should include the durable external root class"
    );
    assert!(
        first.iter().any(|symbol| symbol.name == "LocalA"),
        "initial workspace symbol query should include the local model"
    );
    let substring = session.workspace_symbol_query("sist");
    assert!(
        substring.iter().any(|symbol| symbol.name == "Resistor"),
        "workspace symbol query should preserve substring matching semantics"
    );
    let durable_file_id = session
        .file_id_for_uri("Modelica/package.mo")
        .expect("durable external root file id should exist");
    let durable_file_item_fingerprint = session
        .query_state
        .ast
        .file_item_index_cache
        .get(&durable_file_id)
        .expect("durable external root file item cache should exist")
        .fingerprint;
    let first_detached_cache = session_detached_workspace_symbol_cache(&session);

    assert!(
        session
            .update_document("test.mo", "model LocalB\n  Real x;\nend LocalB;\n")
            .is_none(),
        "summary edit should stay parseable"
    );

    let (second_snapshot, second_timing) = session.workspace_symbol_snapshot_with_timing();
    assert!(
        !second_timing.cache_hit,
        "post-edit workspace symbol snapshots should rebuild for the new revision"
    );
    assert!(
        !second_timing.used_source_set_rebuild_snapshot,
        "warm workspace symbol snapshots should stay on the narrow source-set cache path"
    );
    assert_workspace_symbol_snapshot_is_narrow(&second_snapshot);
    let second = second_snapshot.workspace_symbol_query("");
    assert!(
        second.iter().any(|symbol| symbol.name == "Resistor"),
        "durable external root symbols should remain available after local edits"
    );
    assert!(
        second.iter().any(|symbol| symbol.name == "LocalB"),
        "workspace symbol query should reflect the edited local model"
    );
    let second_detached_cache = detached_workspace_symbol_cache(&second_snapshot);
    assert!(
        session
            .query_state
            .ast
            .file_item_index_cache
            .get(&durable_file_id)
            .is_some_and(|cache| cache.fingerprint == durable_file_item_fingerprint),
        "local summary edits should keep the durable external root file-item cache warm"
    );
    assert!(
        !Arc::ptr_eq(&first_detached_cache, &second_detached_cache),
        "local summary edits should rebuild only the detached local workspace-symbol slice"
    );
}

#[test]
fn workspace_symbol_snapshot_rebuilds_source_set_slices_without_full_snapshot() {
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
    session
        .add_document("test.mo", "model LocalA\n  Real x;\nend LocalA;\n")
        .expect("local document should parse");

    session.query_state.ast.workspace_symbol_query_cache = None;
    let (snapshot, timing) = session.workspace_symbol_snapshot_with_timing();
    assert!(
        timing.used_source_set_rebuild_snapshot,
        "missing source-set caches should use the trimmed source-set rebuild snapshot"
    );
    assert_workspace_symbol_rebuild_snapshot_is_trimmed(&snapshot);

    let symbols = snapshot.workspace_symbol_query("Resistor");
    assert!(
        symbols.iter().any(|symbol| symbol.name == "Resistor"),
        "source-set rebuild snapshots should still answer durable external root workspace symbols"
    );
}

#[test]
fn workspace_symbol_snapshot_prewarm_populates_source_set_symbol_slices() {
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

    let prewarmed = session.workspace_symbol_snapshot();
    prewarmed.prewarm_source_root_read_queries();
    let source_set_id = session
        .source_sets
        .get("Modelica")
        .expect("source-set record should exist")
        .id;
    let prewarmed_cache = source_set_workspace_symbol_cache(&prewarmed, source_set_id);

    let snapshot = session.workspace_symbol_snapshot();
    let cache_before_query = source_set_workspace_symbol_cache(&snapshot, source_set_id);
    let symbols = snapshot.workspace_symbol_query("Resistor");
    let cache_after_query = source_set_workspace_symbol_cache(&snapshot, source_set_id);

    assert!(
        symbols.iter().any(|symbol| symbol.name == "Resistor"),
        "prewarmed workspace symbol snapshots should still answer durable external root symbols"
    );
    assert!(
        Arc::ptr_eq(&prewarmed_cache, &cache_before_query),
        "prewarmed snapshots should reuse the warmed durable external root symbol slice"
    );
    assert!(
        Arc::ptr_eq(&cache_before_query, &cache_after_query),
        "prewarmed workspace symbol queries should not rebuild the durable external root symbol slice"
    );
}

#[test]
fn workspace_symbol_snapshot_prewarm_survives_detached_document_revisions() {
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

    let prewarmed = session.workspace_symbol_snapshot();
    prewarmed.prewarm_source_root_read_queries();

    session
        .add_document("test.mo", "model LocalA\n  Real x;\nend LocalA;\n")
        .expect("local document should parse");

    let (snapshot, timing) = session.workspace_symbol_snapshot_with_timing();
    assert!(
        !timing.used_source_set_rebuild_snapshot,
        "promoted source-set caches should avoid the source-set rebuild snapshot after detached edits"
    );

    let symbols = snapshot.workspace_symbol_query("Resistor");
    assert!(
        symbols.iter().any(|symbol| symbol.name == "Resistor"),
        "promoted workspace symbol caches should still answer durable external root symbols"
    );
}

#[test]
fn warm_source_root_restore_keeps_workspace_symbol_slices_on_restored_aggregate_path() {
    let _guard = session_stats_test_guard();
    let temp = tempfile::tempdir().expect("tempdir");
    let source_root_dir = temp.path().join("Lib");
    let cache_dir = temp.path().join("cache");
    write_two_file_source_root(&source_root_dir);

    let mut first = Session::default();
    let first_report = index_source_root_with_cache(&mut first, &cache_dir, &source_root_dir);
    assert_eq!(
        first_report.cache_status,
        Some(crate::source_roots::SourceRootCacheStatus::Miss),
        "initial load should populate the source-root cache",
    );

    let mut second = Session::default();
    let second_report = index_source_root_with_cache(&mut second, &cache_dir, &source_root_dir);
    assert_eq!(
        second_report.cache_status,
        Some(crate::source_roots::SourceRootCacheStatus::Hit),
        "warm reopen should reuse the outer parsed source-root snapshot",
    );

    let source_set_id = second
        .source_set_id("external::Lib")
        .expect("source-set id should exist after warm reopen");
    assert!(
        second
            .query_state
            .ast
            .workspace_symbol_query_cache
            .as_ref()
            .and_then(|cache| cache.source_set_caches.get(&source_set_id))
            .is_some(),
        "warm reopen should hydrate the source-root workspace-symbol slice before the first snapshot"
    );

    crate::compile::reset_session_cache_stats();
    let (snapshot, timing) = second.workspace_symbol_snapshot_with_timing();
    let before = crate::compile::session_cache_stats();
    let symbols = snapshot.workspace_symbol_query("M");
    let delta = crate::compile::session_cache_stats().delta_since(before);

    assert!(
        !timing.used_source_set_rebuild_snapshot,
        "warm workspace-symbol snapshots should stay on the restored aggregate path after reopen"
    );
    assert_workspace_symbol_snapshot_is_narrow(&snapshot);
    assert!(
        symbols.iter().any(|symbol| symbol.name == "M"),
        "warm workspace-symbol snapshots should still answer restored source-root symbols"
    );
    assert_eq!(
        delta.file_item_index_query_misses, 0,
        "warm workspace-symbol slices should not rebuild per-file symbol indexes on the first query after reopen"
    );
    assert_eq!(
        delta.source_root_files_parsed, 0,
        "warm workspace-symbol snapshots should not reparse source-root files on the first query after reopen"
    );
}
