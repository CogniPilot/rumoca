use super::*;
use crate::compile::core::{Diagnostic as CommonDiagnostic, PrimaryLabel, Span};
use std::sync::{Arc, Mutex, MutexGuard};

static SESSION_STATS_TEST_MUTEX: Mutex<()> = Mutex::new(());

pub(super) fn session_stats_test_guard() -> MutexGuard<'static, ()> {
    SESSION_STATS_TEST_MUTEX
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

fn parse_definition(source: &str, file_name: &str) -> ast::StoredDefinition {
    rumoca_phase_parse::parse_to_ast(source, file_name).expect("test definition should parse")
}

fn source_set_record<'a>(session: &'a Session, source_set_id: &str) -> &'a SourceSetRecord {
    session
        .source_sets
        .get(source_set_id)
        .expect("source-set record should exist")
}

fn insert_lru_cache_entry<T>(
    cache: &mut IndexMap<String, T>,
    key: String,
    value: T,
    max_entries: usize,
) {
    cache.shift_remove(&key);
    cache.insert(key, value);
    while cache.len() > max_entries {
        let Some(oldest) = cache.keys().next().cloned() else {
            break;
        };
        cache.shift_remove(&oldest);
    }
}

fn get_lru_cache_entry<T: Clone>(cache: &mut IndexMap<String, T>, key: &str) -> Option<T> {
    let entry = cache.shift_remove(key)?;
    let cloned = entry.clone();
    cache.insert(key.to_string(), entry);
    Some(cloned)
}

fn model_stage_semantic_diagnostics_artifact_mut<'a>(
    session: &'a mut Session,
    model_name: &str,
    mode: SemanticDiagnosticsMode,
) -> &'a mut SemanticDiagnosticsArtifact {
    let key = SemanticDiagnosticsCacheKey::new(model_name, mode);
    session
        .query_state
        .flat
        .semantic_diagnostics
        .model_stage_artifacts
        .get_mut(&key)
        .expect("model-stage diagnostics should be cached")
}

fn interface_semantic_diagnostics_artifact_mut<'a>(
    session: &'a mut Session,
    model_name: &str,
    mode: SemanticDiagnosticsMode,
) -> &'a mut InterfaceSemanticDiagnosticsArtifact {
    let key = SemanticDiagnosticsCacheKey::new(model_name, mode);
    session
        .query_state
        .flat
        .semantic_diagnostics
        .interface_artifacts
        .get_mut(&key)
        .expect("interface diagnostics should be cached")
}

fn body_semantic_diagnostics_artifact_mut<'a>(
    session: &'a mut Session,
    model_name: &str,
    mode: SemanticDiagnosticsMode,
) -> &'a mut BodySemanticDiagnosticsArtifact {
    let key = SemanticDiagnosticsCacheKey::new(model_name, mode);
    session
        .query_state
        .flat
        .semantic_diagnostics
        .body_artifacts
        .get_mut(&key)
        .expect("body diagnostics should be cached")
}

fn standard_instantiation_cache_key(
    session: &mut Session,
    model_name: &str,
) -> InstantiatedModelCacheKey {
    InstantiatedModelCacheKey::new(
        session
            .model_key_query(model_name)
            .expect("model key should resolve"),
        ResolveBuildMode::Standard,
    )
}

fn standard_typed_cache_key(session: &mut Session, model_name: &str) -> TypedModelCacheKey {
    TypedModelCacheKey::new(
        session
            .model_key_query(model_name)
            .expect("model key should resolve"),
        ResolveBuildMode::Standard,
    )
}

fn standard_flat_cache_key(session: &mut Session, model_name: &str) -> FlatModelCacheKey {
    FlatModelCacheKey::new(
        session
            .model_key_query(model_name)
            .expect("model key should resolve"),
        ResolveBuildMode::Standard,
    )
}

fn standard_dae_cache_key(session: &mut Session, model_name: &str) -> DaeModelCacheKey {
    DaeModelCacheKey::new(
        session
            .model_key_query(model_name)
            .expect("model key should resolve"),
        ResolveBuildMode::Standard,
    )
}

fn assert_diagnostics_have_code(diagnostics: &ModelDiagnostics, code: &str) {
    assert!(
        diagnostics
            .diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some(code)),
        "expected diagnostic code `{code}`"
    );
}

fn assert_diagnostics_lack_code(diagnostics: &ModelDiagnostics, code: &str) {
    assert!(
        diagnostics
            .diagnostics
            .iter()
            .all(|diag| diag.code.as_deref() != Some(code)),
        "did not expect diagnostic code `{code}`"
    );
}

#[test]
fn test_session_add_document() {
    let mut session = Session::default();
    session
        .add_document("test.mo", "model M Real x; end M;")
        .unwrap();
    assert!(session.get_document("test.mo").is_some());
}

#[test]
fn session_snapshot_keeps_document_and_query_view_after_host_edit() {
    let mut session = Session::default();
    let source_v1 = "model M\n  Real x;\nend M;\n";
    let source_v2 = "model M\n  Real y;\nend M;\n";
    session
        .add_document("test.mo", source_v1)
        .expect("initial document should parse");

    let snapshot = session.snapshot();

    session.update_document("test.mo", source_v2);

    let snapshot_doc = snapshot
        .get_document("test.mo")
        .expect("snapshot should retain original document");
    let host_doc = session
        .get_document("test.mo")
        .expect("host should expose updated document");
    assert_eq!(snapshot_doc.content, source_v1);
    assert_eq!(host_doc.content, source_v2);

    let snapshot_items = snapshot.class_local_completion_items_query("test.mo", "M");
    let host_items = session.class_local_completion_items_query("test.mo", "M");
    assert_eq!(
        snapshot_items
            .iter()
            .map(|item| item.name.as_str())
            .collect::<Vec<_>>(),
        vec!["x"]
    );
    assert_eq!(
        host_items
            .iter()
            .map(|item| item.name.as_str())
            .collect::<Vec<_>>(),
        vec!["y"]
    );
}

#[test]
fn session_snapshot_keeps_loaded_library_membership_after_host_removal() {
    let mut session = Session::default();
    session.replace_parsed_source_set(
        "Modelica",
        SourceRootKind::DurableLibrary,
        vec![(
            "Modelica/package.mo".to_string(),
            parse_definition("package Modelica\nend Modelica;\n", "Modelica/package.mo"),
        )],
        None,
    );
    assert!(session.is_loaded_library_document("Modelica/package.mo"));

    let snapshot = session.snapshot();

    session.remove_source_set("Modelica");

    assert!(
        snapshot.is_loaded_library_document("Modelica/package.mo"),
        "snapshot should retain loaded library ownership"
    );
    assert!(
        snapshot
            .document_uris()
            .contains(&"Modelica/package.mo".to_string()),
        "snapshot should retain the source-root document view"
    );
    assert!(
        !session.is_loaded_library_document("Modelica/package.mo"),
        "host should reflect the removed source root"
    );
}

#[test]
fn session_snapshots_share_query_warmth_across_reads() {
    let _guard = session_stats_test_guard();
    crate::compile::reset_session_cache_stats();

    let mut session = Session::default();
    session
        .add_document("test.mo", "model M\n  Real x;\nend M;\n")
        .expect("document should parse");

    let first_snapshot = session.snapshot();
    let first_symbols = first_snapshot
        .document_symbol_query("test.mo")
        .expect("first snapshot should build an outline");
    drop(first_snapshot);
    assert!(!first_symbols.is_empty());
    let stats_after_first = crate::compile::session_cache_stats();

    let second_snapshot = session.snapshot();
    let second_symbols = second_snapshot
        .document_symbol_query("test.mo")
        .expect("second snapshot should reuse the outline");
    drop(second_snapshot);
    assert_eq!(first_symbols.len(), second_symbols.len());

    let delta = crate::compile::session_cache_stats().delta_since(stats_after_first);
    assert!(
        delta.document_symbol_query_hits >= 1,
        "second snapshot should reuse warmed query state"
    );
    assert_eq!(
        delta.document_symbol_query_misses, 0,
        "second snapshot should not rebuild the outline from cold state"
    );
}

#[test]
fn session_snapshots_preserve_workspace_symbol_warmth_across_body_only_edits() {
    let _guard = session_stats_test_guard();
    crate::compile::reset_session_cache_stats();

    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            "package Lib\n  model Helper\n    Real y;\n  equation\n    y = 1;\n  end Helper;\nend Lib;\n\nmodel M\n  import Alias = Lib.Helper;\n  Alias helperInst;\nequation\n  helperInst.y = sin(helperInst.y);\nend M;\n",
        )
        .expect("document should parse");

    let first_snapshot = session.snapshot();
    let first_symbols = first_snapshot.workspace_symbol_query("Helper");
    drop(first_snapshot);
    assert!(
        first_symbols.iter().any(|symbol| symbol.name == "Helper"),
        "first snapshot should build workspace symbols"
    );

    session.update_document(
        "test.mo",
        "package Lib\n  model Helper\n    Real y;\n  equation\n    y = 1;\n  end Helper;\nend Lib;\n\nmodel M\n  import Alias = Lib.Helper;\n  Alias helperInst;\nequation\n  helperInst.y = cos(helperInst.y);\nend M;\n",
    );

    let stats_after_edit = crate::compile::session_cache_stats();
    let second_snapshot = session.snapshot();
    let second_symbols = second_snapshot.workspace_symbol_query("Helper");
    drop(second_snapshot);
    assert!(
        second_symbols.iter().any(|symbol| symbol.name == "Helper"),
        "body-only edits should preserve workspace symbol results"
    );

    let delta = crate::compile::session_cache_stats().delta_since(stats_after_edit);
    assert!(
        delta.workspace_symbol_query_hits >= 1,
        "body-only edits should keep the workspace symbol cache warm across snapshots"
    );
    assert_eq!(
        delta.workspace_symbol_query_misses, 0,
        "body-only edits should not rebuild workspace symbols from cold state"
    );
    assert_eq!(
        delta.file_item_index_query_hits + delta.file_item_index_query_misses,
        0,
        "warm workspace symbol reuse should avoid rebuilding per-file symbol indexes"
    );
}

#[test]
fn session_package_queries_keep_durable_library_roots_warm_across_local_summary_edits() {
    let _guard = session_stats_test_guard();
    crate::compile::reset_session_cache_stats();

    let mut session = Session::default();
    session.replace_parsed_source_set(
        "Modelica",
        SourceRootKind::DurableLibrary,
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

    let first = session.class_lookup_query("Modelica.Electrical.Analog.Resistor");
    assert_eq!(
        first.as_deref(),
        Some("Modelica.Electrical.Analog.Resistor"),
        "first lookup should resolve the durable library class"
    );
    let stats_after_first = crate::compile::session_cache_stats();

    assert!(
        session
            .update_document("test.mo", "model LocalB\n  Real x;\nend LocalB;\n")
            .is_none(),
        "summary edit should stay parseable"
    );

    let second = session.class_lookup_query("Modelica.Electrical.Analog.Resistor");
    assert_eq!(
        second.as_deref(),
        Some("Modelica.Electrical.Analog.Resistor"),
        "lookup should still resolve the durable library class after local edits"
    );

    let delta = crate::compile::session_cache_stats().delta_since(stats_after_first);
    assert!(
        delta.source_set_package_membership_query_hits >= 1,
        "session-wide package queries should reuse the durable library source-set cache"
    );
    assert_eq!(
        delta.source_set_package_membership_query_misses, 0,
        "local edits should not rebuild the durable library source-set query"
    );
}

#[test]
fn session_tracks_stable_file_ids_and_revisions_across_document_lifecycle() {
    let mut session = Session::default();
    let source_v1 = "model M\n  Real x;\nend M;\n";
    let source_v2 = "model M\n  Real x;\n  Real y;\nend M;\n";

    assert_eq!(session.current_revision, RevisionId::default());

    session
        .add_document("test.mo", source_v1)
        .expect("first document should parse");
    let file_id = *session
        .file_ids
        .get("test.mo")
        .expect("file id should be assigned on insert");
    let first_revision = session.current_revision;
    assert_eq!(session.file_revisions.get(&file_id), Some(&first_revision));

    let unchanged = session.update_document("test.mo", source_v1);
    assert!(
        unchanged.is_none(),
        "no-op update should keep parse success"
    );
    assert_eq!(
        session.current_revision, first_revision,
        "no-op update must not bump the session revision"
    );
    assert_eq!(
        session.file_ids.get("test.mo"),
        Some(&file_id),
        "file id should remain stable on no-op updates"
    );

    let changed = session.update_document("test.mo", source_v2);
    assert!(changed.is_none(), "updated document should parse");
    let second_revision = session.current_revision;
    assert!(
        second_revision > first_revision,
        "successful edits must bump the session revision"
    );
    assert_eq!(
        session.file_ids.get("test.mo"),
        Some(&file_id),
        "file id should remain stable across edits"
    );
    assert_eq!(
        session.file_revisions.get(&file_id),
        Some(&second_revision),
        "latest file revision should follow the session revision"
    );

    session.remove_document("test.mo");
    let removed_revision = session.current_revision;
    assert!(
        removed_revision > second_revision,
        "document removal must bump the session revision"
    );

    session
        .add_document("test.mo", source_v1)
        .expect("re-added document should parse");
    assert_eq!(
        session.file_ids.get("test.mo"),
        Some(&file_id),
        "re-adding the same URI should preserve the stable file id for the session lifetime"
    );
    assert!(
        session.current_revision > removed_revision,
        "re-adding the document must bump the session revision again"
    );
}

#[test]
fn source_set_records_keep_stable_ids_and_revision_history() {
    let mut session = Session::default();
    let defs_v1 = vec![(
        "lib/Lib.mo".to_string(),
        parse_definition(
            "package Lib\n  model A\n    Real x;\n  end A;\nend Lib;\n",
            "lib/Lib.mo",
        ),
    )];
    let defs_v2 = vec![(
        "lib/Lib.mo".to_string(),
        parse_definition(
            "package Lib\n  model A\n    Real x;\n  equation\n    x = 1;\n  end A;\nend Lib;\n",
            "lib/Lib.mo",
        ),
    )];

    let inserted =
        session.replace_parsed_source_set("lib", SourceRootKind::Library, defs_v1.clone(), None);
    assert_eq!(inserted, 1, "first source-set load should insert one file");
    let first_record = source_set_record(&session, "lib");
    let first_id = first_record.id;
    let first_revision = first_record.revision;
    assert!(
        !first_record.uris.is_empty(),
        "source-set should track its files"
    );

    let warm_inserted =
        session.replace_parsed_source_set("lib", SourceRootKind::Library, defs_v1, None);
    assert_eq!(
        warm_inserted, 1,
        "unchanged replacement still reports file count"
    );
    let warm_record = source_set_record(&session, "lib");
    assert_eq!(
        warm_record.id, first_id,
        "unchanged source-set refresh must preserve the stable source-set id"
    );
    assert_eq!(
        warm_record.revision, first_revision,
        "unchanged source-set refresh must not bump the revision"
    );

    let updated = session.replace_parsed_source_set("lib", SourceRootKind::Library, defs_v2, None);
    assert_eq!(
        updated, 1,
        "changed source-set should still insert one file"
    );
    let updated_record = source_set_record(&session, "lib");
    assert_eq!(
        updated_record.id, first_id,
        "source-set id should remain stable across replacements"
    );
    assert!(
        updated_record.revision > first_revision,
        "changing a source-set must bump its tracked revision"
    );

    session.remove_source_set("lib");
    let removed_record = source_set_record(&session, "lib");
    assert_eq!(
        removed_record.id, first_id,
        "source-set identity should survive removal for later reloads"
    );
    assert!(
        removed_record.uris.is_empty(),
        "removed source-set should no longer own any URIs"
    );
    let removed_revision = removed_record.revision;

    let reloaded = session.replace_parsed_source_set(
        "lib",
        SourceRootKind::Library,
        vec![(
            "lib/Other.mo".to_string(),
            parse_definition(
                "package Lib\n  model B\n    Real y;\n  end B;\nend Lib;\n",
                "lib/Other.mo",
            ),
        )],
        None,
    );
    assert_eq!(reloaded, 1, "reloaded source-set should insert one file");
    let reloaded_record = source_set_record(&session, "lib");
    assert_eq!(
        reloaded_record.id, first_id,
        "reloading a source-set with the same key should reuse the stable source-set id"
    );
    assert!(
        reloaded_record.revision > removed_revision,
        "reloading must advance the source-set revision"
    );
}

#[test]
fn session_change_keeps_workspace_root_membership_and_detaches_library_overlay() {
    let mut session = Session::default();
    let source = "model M\n  Real x;\nend M;\n";

    let mut workspace_change = SessionChange::default();
    workspace_change
        .replace_source_root("workspace", SourceRootKind::Workspace, ["workspace/M.mo"])
        .set_file_text("workspace/M.mo", source);
    session.apply_change(workspace_change);

    let workspace_revision = session.current_revision;
    let workspace_record = session
        .source_sets
        .get("workspace")
        .expect("workspace root should be recorded");
    assert_eq!(workspace_record.kind, SourceRootKind::Workspace);
    assert_eq!(workspace_record.durability, SourceRootDurability::Volatile);
    assert!(
        workspace_record.uris.contains("workspace/M.mo"),
        "workspace text updates should stay inside the workspace root"
    );
    let workspace_file_id = *session
        .file_ids
        .get("workspace/M.mo")
        .expect("workspace file id should exist");
    assert_eq!(
        session.file_revisions.get(&workspace_file_id),
        Some(&workspace_revision),
        "workspace file change should share the transaction revision"
    );

    let mut library_change = SessionChange::default();
    library_change
        .replace_source_root("library", SourceRootKind::Library, ["library/Lib.mo"])
        .set_file_text("library/Lib.mo", source);
    session.apply_change(library_change);

    let library_record = session
        .source_sets
        .get("library")
        .expect("library root should be recorded");
    assert_eq!(library_record.kind, SourceRootKind::Library);
    assert_eq!(library_record.durability, SourceRootDurability::Normal);
    assert!(
        library_record.uris.is_empty(),
        "text overlays should detach files from library-backed roots"
    );
    assert!(
        session.get_document("library/Lib.mo").is_some(),
        "detached library overlay should still exist as a live document"
    );
}

#[test]
fn session_change_empty_and_noop_updates_do_not_bump_revision() {
    let mut session = Session::default();

    session.apply_change(SessionChange::default());
    assert_eq!(session.current_revision, RevisionId::new(0));

    let source = "model M\n  Real x;\nend M;\n";
    let mut first = SessionChange::default();
    first
        .replace_source_root("workspace", SourceRootKind::Workspace, ["workspace/M.mo"])
        .set_file_text("workspace/M.mo", source);
    session.apply_change(first);

    let revision = session.current_revision;
    let mut noop = SessionChange::default();
    noop.replace_source_root("workspace", SourceRootKind::Workspace, ["workspace/M.mo"])
        .set_file_text("workspace/M.mo", source);
    session.apply_change(noop);

    assert_eq!(
        session.current_revision, revision,
        "identical transactional inputs should not advance the revision"
    );
}

#[test]
fn removing_live_library_document_restores_latest_detached_source_root_document() {
    let mut session = Session::default();
    let uri = "library/Lib.mo";
    let parsed_v1 = parse_definition("package Lib\n  model A\n  end A;\nend Lib;\n", uri);
    let parsed_v2 = parse_definition("package Lib\n  model B\n  end B;\nend Lib;\n", uri);

    session.replace_parsed_source_set(
        "library",
        SourceRootKind::Library,
        vec![(uri.to_string(), parsed_v1)],
        None,
    );
    let parse_error =
        session.update_document(uri, "package Lib\n  model Open\n  end Open;\nend Lib;\n");
    assert!(
        parse_error.is_none(),
        "live library edit should parse cleanly"
    );

    session.replace_parsed_source_set(
        "library",
        SourceRootKind::Library,
        vec![(uri.to_string(), parsed_v2.clone())],
        Some(uri),
    );
    session.remove_document(uri);

    let restored = session
        .get_document(uri)
        .cloned()
        .expect("closing a live library document should restore the cached source-root document");
    assert!(
        restored.content.is_empty(),
        "restored library-backed document should return to parsed-source-set ownership"
    );
    assert_eq!(
        restored.parsed(),
        Some(&parsed_v2),
        "library rebuilds while the file is open should refresh the detached backing document"
    );
    assert!(
        session
            .source_set_uris("library")
            .is_some_and(|uris| uris.contains(uri)),
        "restoring a detached library document should reattach it to the source root"
    );
}

#[test]
fn detached_library_edits_keep_library_membership_and_mark_source_root_for_refresh() {
    let mut session = Session::default();
    let uri = "library/Lib.mo";
    let parsed = parse_definition("package Lib\n  model A\n  end A;\nend Lib;\n", uri);

    session.replace_parsed_source_set(
        "library",
        SourceRootKind::Library,
        vec![(uri.to_string(), parsed)],
        None,
    );
    session.update_document(uri, "package Lib\n  model Open\n  end Open;\nend Lib;\n");
    assert!(
        session.is_loaded_library_document(uri),
        "detached live library documents should still resolve as loaded library documents"
    );
    assert!(
        session.dirty_library_source_root_keys().is_empty(),
        "opening a detached library document should not mark the source root dirty yet"
    );
    assert_eq!(
        session.library_source_set_ids_for_uri(uri),
        vec![
            session
                .source_set_id("library")
                .expect("source-set id should exist")
        ],
        "detached live library documents should still resolve to their library source root"
    );

    session.update_document(
        uri,
        "package Lib\n  model Open\n    Real x;\n  end Open;\nend Lib;\n",
    );
    assert_eq!(
        session.dirty_library_source_root_keys(),
        vec!["library".to_string()],
        "editing an already-detached library document should mark its source root for refresh"
    );

    let refreshed = parse_definition("package Lib\n  model B\n  end B;\nend Lib;\n", uri);
    session.replace_parsed_source_set(
        "library",
        SourceRootKind::Library,
        vec![(uri.to_string(), refreshed)],
        Some(uri),
    );
    assert!(
        session.dirty_library_source_root_keys().is_empty(),
        "reloading the source root should clear the pending-refresh flag"
    );
}

#[test]
fn file_ids_and_library_membership_reuse_path_aliases_for_same_file() {
    let temp = tempfile::tempdir().expect("temp dir");
    let library_root = temp.path().join("Lib");
    let nested = library_root.join("nested");
    std::fs::create_dir_all(&nested).expect("mkdir nested");
    let canonical_path = library_root.join("package.mo");
    std::fs::write(
        &canonical_path,
        "package Lib\n  model A\n  end A;\nend Lib;\n",
    )
    .expect("write package.mo");
    let alias_path = nested.join("..").join("package.mo");
    let canonical_uri = canonical_path.to_string_lossy().to_string();
    let alias_uri = alias_path.to_string_lossy().to_string();

    let mut session = Session::default();
    session.replace_parsed_source_set(
        "library",
        SourceRootKind::Library,
        vec![(
            alias_uri.clone(),
            parse_definition("package Lib\n  model A\n  end A;\nend Lib;\n", &alias_uri),
        )],
        None,
    );

    let source_set_id = session
        .source_set_id("library")
        .expect("source-set id should exist");
    assert_eq!(
        session.file_id(&canonical_uri),
        session.file_id(&alias_uri),
        "the same file should reuse one stable file id across path aliases"
    );
    assert_eq!(
        session.library_source_set_ids_for_uri(&canonical_uri),
        vec![source_set_id],
        "canonical-path lookups should resolve the attached library source root by file id"
    );

    session.update_document(
        &canonical_uri,
        "package Lib\n  model Open\n  end Open;\nend Lib;\n",
    );
    assert_eq!(
        session.library_source_set_ids_for_uri(&canonical_uri),
        vec![source_set_id],
        "detached live library documents should keep the same library membership across path aliases"
    );
}

#[test]
fn query_helpers_expose_file_and_source_set_revisions() {
    let mut session = Session::default();
    let source_a = "model A\n  Real x;\nend A;\n";
    let source_b = "package B\n  model C\n    Real y;\n  end C;\nend B;\n";

    session
        .add_document("a.mo", source_a)
        .expect("a source file should parse");
    session
        .add_document("b.mo", source_b)
        .expect("b source file should parse");

    let id_a = session
        .file_id("a.mo")
        .expect("file id should exist for a.mo");
    let id_b = session
        .file_id("b.mo")
        .expect("file id should exist for b.mo");
    let rev_a = session
        .file_revision("a.mo")
        .expect("file revision should exist for a.mo");
    let changed_after_default = session.changed_file_ids_since(RevisionId::default());
    assert!(
        changed_after_default.contains(&id_a),
        "changed_file_ids_since should include changed file ids"
    );
    assert!(
        changed_after_default.contains(&id_b),
        "changed_file_ids_since should include all changed file ids"
    );
    let changed_uris_after_default = session.changed_file_uris_since(RevisionId::default());
    assert!(
        changed_uris_after_default.contains(&"a.mo".to_string()),
        "changed_file_uris_since should include changed file URIs"
    );

    let source_set_file = vec![(
        "lib/Lib.mo".to_string(),
        parse_definition(
            "package Lib\n  model M\n    Real z;\n  end M;\nend Lib;\n",
            "lib/Lib.mo",
        ),
    )];
    session.replace_parsed_source_set(
        "stdlib",
        SourceRootKind::DurableLibrary,
        source_set_file,
        None,
    );
    assert!(session.source_set_id("stdlib").is_some());
    assert!(session.source_set_revision("stdlib").is_some());
    let stdlib_record = session
        .source_sets
        .get("stdlib")
        .expect("durable source-set should be tracked");
    assert_eq!(stdlib_record.durability, SourceRootDurability::Durable);
    assert_eq!(
        session.source_set_file_ids("stdlib").len(),
        1,
        "source set helper should expose file ids"
    );
    let lib_file_id = session
        .source_set_file_ids("stdlib")
        .first()
        .copied()
        .expect("library source set should expose file id");
    assert_ne!(
        lib_file_id, id_a,
        "library ids should be distinct from workspace file ids"
    );
    let lib_file_uri = session
        .file_uri(lib_file_id)
        .expect("file uri should be retrievable by id");
    assert_eq!(lib_file_uri, "lib/Lib.mo");

    let changed_after_source_set = session.changed_file_ids_since(rev_a);
    assert!(
        changed_after_source_set.contains(&lib_file_id),
        "library source-set load should be reported as a changed file id"
    );
    assert!(
        !changed_after_source_set.contains(&id_a),
        "files touched before the revision should not be reported as changed"
    );
    assert!(
        changed_after_source_set.contains(&id_b),
        "later-file changes should still be reported as changed"
    );
}

#[test]
fn test_session_compile() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            "model M Real x(start=0); equation der(x) = 1; end M;",
        )
        .unwrap();

    let names = session.model_names().unwrap();
    assert_eq!(names, &["M"]);

    let result = session.compile_model("M").unwrap();
    assert!(result.is_balanced());
}

#[test]
fn test_compile_extracts_experiment_stop_time() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                model M
                  Real x(start=0);
                equation
                  der(x) = 1;
                annotation(experiment(StopTime=2.5));
                end M;
                "#,
        )
        .unwrap();

    let result = session.compile_model("M").unwrap();
    assert_eq!(result.experiment_start_time, None);
    assert_eq!(result.experiment_stop_time, Some(2.5));
    assert_eq!(result.experiment_tolerance, None);
    assert_eq!(result.experiment_interval, None);
    assert_eq!(result.experiment_solver, None);
}

#[test]
fn test_compile_ignores_negative_experiment_stop_time() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                model M
                  Real x(start=0);
                equation
                  der(x) = 1;
                annotation(experiment(StopTime=-1));
                end M;
                "#,
        )
        .unwrap();

    let result = session.compile_model("M").unwrap();
    assert_eq!(result.experiment_stop_time, None);
}

#[test]
fn test_compile_extracts_experiment_tolerance_interval_and_solver() {
    let mut session = Session::default();
    session
            .add_document(
                "test.mo",
                r#"
                model M
                  Real x(start=0);
                equation
                  der(x) = 1;
                annotation(experiment(StartTime=0.1, StopTime=2.5, Tolerance=1e-5, Interval=0.01, Algorithm="Dassl"));
                end M;
                "#,
            )
            .unwrap();

    let result = session.compile_model("M").unwrap();
    assert_eq!(result.experiment_start_time, Some(0.1));
    assert_eq!(result.experiment_stop_time, Some(2.5));
    assert_eq!(result.experiment_tolerance, Some(1e-5));
    assert_eq!(result.experiment_interval, Some(0.01));
    assert_eq!(result.experiment_solver.as_deref(), Some("Dassl"));
}

#[test]
fn test_compile_extracts_solver_from_openmodelica_simulation_flags() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                model M
                  Real x(start=0);
                equation
                  der(x) = 1;
                annotation(experiment(__OpenModelica_simulationFlags(s="rungekutta")));
                end M;
                "#,
        )
        .unwrap();

    let result = session.compile_model("M").unwrap();
    assert_eq!(result.experiment_solver.as_deref(), Some("rungekutta"));
}

#[test]
fn test_typecheck_error_code_preserves_et004() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                model M
                  parameter Real a[:];
                  Real x[size(a, 1)];
                equation
                  x = a;
                end M;
                "#,
        )
        .unwrap();

    let phase_result = session.compile_model_phases("M").unwrap();
    match phase_result {
        PhaseResult::Failed {
            phase, error_code, ..
        } => {
            assert_eq!(phase, FailedPhase::Typecheck);
            assert_eq!(error_code.as_deref(), Some("ET004"));
        }
        other => panic!("expected typecheck failure, got {:?}", other),
    }
}

#[test]
fn test_record_forwarding_rebinds_dependent_record_fields() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                package P
                  record R
                    parameter Real a = 2;
                    final parameter Real b = a;
                  end R;

                  model Inner
                    parameter R r;
                    parameter Real x = r.b;
                  end Inner;

                  model Mid
                    parameter R r;
                    Inner i(r = r);
                  end Mid;

                  model Top
                    parameter R r(a = 5);
                    Mid mid(r = r);
                  end Top;
                end P;
                "#,
        )
        .unwrap();

    let result = session.compile_model("P.Top").unwrap();
    let mid_rb = result
        .dae
        .parameters
        .get(&dae::VarName::new("mid.r.b"))
        .expect("mid.r.b must exist in DAE parameters");
    let mid_irb = result
        .dae
        .parameters
        .get(&dae::VarName::new("mid.i.r.b"))
        .expect("mid.i.r.b must exist in DAE parameters");

    let mid_rb_start = mid_rb.start.as_ref().expect("mid.r.b start expected");
    let mid_irb_start = mid_irb.start.as_ref().expect("mid.i.r.b start expected");

    match mid_rb_start {
        dae::Expression::Literal(dae::Literal::Integer(5)) => {}
        dae::Expression::Literal(dae::Literal::Real(v)) if (v - 5.0).abs() <= f64::EPSILON => {}
        other => panic!(
            "record forwarding must propagate dependent field b via overridden a, got {:?}",
            other
        ),
    }
    match mid_irb_start {
        dae::Expression::Literal(dae::Literal::Integer(5)) => {}
        dae::Expression::Literal(dae::Literal::Real(v)) if (v - 5.0).abs() <= f64::EPSILON => {}
        other => panic!(
            "nested forwarding must preserve dependent record field values, got {:?}",
            other
        ),
    }
}

#[test]
fn test_compile_model_surfaces_todae_unresolved_reference_code() {
    let mut session = Session::default();
    session
        .add_document(
            "model.mo",
            r#"
                function F
                  input Real x;
                  output Real y;
                algorithm
                  y := x + missingRef;
                end F;

                model M
                  Real x(start=0);
                equation
                  der(x) = F(x);
                end M;
                "#,
        )
        .unwrap();
    // Add a second document so resolve runs in multi-document mode; unresolved
    // refs are then checked by downstream phases.
    session
        .add_document(
            "helper.mo",
            r#"
                model Helper
                  Real y;
                equation
                  y = 1.0;
                end Helper;
                "#,
        )
        .unwrap();

    let err = session
        .compile_model("M")
        .expect_err("compile_model should fail on unresolved ToDae reference");
    let err_text = err.to_string();
    assert!(
        err_text.contains("rumoca::todae::ED008"),
        "expected ToDae ED008 in compile_model error, got: {err_text}"
    );

    let phase_result = session.compile_model_phases("M").unwrap();
    match phase_result {
        PhaseResult::Failed {
            phase, error_code, ..
        } => {
            assert_eq!(phase, FailedPhase::ToDae);
            assert_eq!(error_code.as_deref(), Some("rumoca::todae::ED008"));
        }
        other => panic!("expected ToDae failure, got {other:?}"),
    }
}

#[test]
fn test_typecheck_error_code_preserves_et001_for_unknown_builtin_modifier() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                model M
                  Real x(startd = 1.0);
                equation
                  der(x) = -x;
                end M;
                "#,
        )
        .unwrap();

    let phase_result = session.compile_model_phases("M").unwrap();
    match phase_result {
        PhaseResult::Failed {
            phase, error_code, ..
        } => {
            assert_eq!(phase, FailedPhase::Typecheck);
            assert_eq!(error_code.as_deref(), Some("ET001"));
        }
        other => panic!("expected typecheck failure, got {:?}", other),
    }
}

#[test]
fn test_unknown_builtin_modifier_is_not_ignored_with_multiple_classes() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                package Lib
                  model Helper
                    Real y;
                  equation
                    y = 1.0;
                  end Helper;
                end Lib;

                model M
                  Real x(startd = 1.0);
                equation
                  der(x) = -x;
                end M;
                "#,
        )
        .unwrap();

    let phase_result = session.compile_model_phases("M").unwrap();
    match phase_result {
        PhaseResult::Failed {
            phase, error_code, ..
        } => {
            assert_eq!(phase, FailedPhase::Typecheck);
            assert_eq!(error_code.as_deref(), Some("ET001"));
        }
        other => panic!("expected typecheck failure, got {:?}", other),
    }

    let diagnostics = session.compile_model_diagnostics("M");
    assert!(
        diagnostics
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("ET001")),
        "expected ET001 diagnostics, got: {:?}",
        diagnostics.diagnostics
    );
}

#[test]
fn test_unknown_class_start_modifier_is_not_ignored_with_multiple_components() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                model Main
                  Test t1(start=1), t2(start=2);
                end Main;

                model Test
                  Real x;
                end Test;
                "#,
        )
        .unwrap();

    let phase_result = session.compile_model_phases("Main").unwrap();
    match phase_result {
        PhaseResult::Failed {
            phase, error_code, ..
        } => {
            assert_eq!(phase, FailedPhase::Typecheck);
            assert_eq!(error_code.as_deref(), Some("ET001"));
        }
        other => panic!("expected typecheck failure, got {:?}", other),
    }

    let diagnostics = session.compile_model_diagnostics("Main");
    assert!(
        diagnostics
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("ET001")
                && d.message.contains("unknown modifier `start`")),
        "expected ET001 unknown class start modifier diagnostics, got: {:?}",
        diagnostics.diagnostics
    );
}

#[test]
fn test_compile_model_diagnostics_for_valid_function_has_no_phase_errors() {
    let mut session = Session::default();
    session
        .add_document(
            "func.mo",
            r#"
                function F
                  input Real x;
                  output Real y;
                algorithm
                  y := x;
                end F;
                "#,
        )
        .unwrap();

    let diagnostics = session.compile_model_diagnostics("F");
    assert!(
        diagnostics.diagnostics.is_empty(),
        "expected no diagnostics for valid function, got: {:?}",
        diagnostics.diagnostics
    );
}

#[test]
fn semantic_navigation_cache_reuses_active_target_tree() {
    let source = r#"package P
  model Dep
    Real y;
  equation
    y = 1;
  end Dep;

  model Root
    Dep dep;
  equation
    dep.y = 2;
  end Root;
end P;
"#;

    let mut session = Session::default();
    session
        .add_document("test.mo", source)
        .expect("document should parse");

    let first = session
        .resolved_for_semantic_navigation("P.Root")
        .expect("navigation tree should build");
    assert!(
        first.0.get_class_by_qualified_name("P.Root").is_some(),
        "navigation tree should include the active target"
    );
    let cached = session
        .query_state
        .resolved
        .semantic_navigation
        .get_mut("P.Root")
        .expect("navigation artifact should be cached");
    cached.resolved = Arc::new(ast::ResolvedTree::new(ast::ClassTree::new()));

    let second = session
        .resolved_for_semantic_navigation("P.Root")
        .expect("navigation tree should reuse cache");
    assert!(
        second.0.definitions.classes.is_empty(),
        "second navigation lookup must reuse the cached artifact"
    );
}

#[test]
fn strict_recovery_resolved_cache_reuses_tree_and_diagnostics() {
    let source = r#"package P
  model Root
    Missing dep;
  equation
    dep.y = 2;
  end Root;
end P;
"#;

    let mut session = Session::default();
    session
        .add_document("test.mo", source)
        .expect("document should parse");

    let (_first_resolved, first_diags) = session
        .build_resolved_for_strict_compile_with_diagnostics()
        .expect("strict recovery resolve should succeed");
    assert!(
        session
            .query_state
            .resolved
            .builds
            .strict_compile_recovery
            .is_some(),
        "first strict recovery lookup should cache the resolved tree"
    );
    assert!(
        !first_diags.is_empty(),
        "strict recovery resolve should preserve diagnostics"
    );

    let cached = session
        .query_state
        .resolved
        .builds
        .strict_compile_recovery
        .as_mut()
        .expect("strict recovery tree should be cached");
    *cached = Arc::new(ast::ResolvedTree::new(ast::ClassTree::new()));

    let (second_resolved, second_diags) = session
        .build_resolved_for_strict_compile_with_diagnostics()
        .expect("warm strict recovery resolve should reuse cache");
    assert!(
        second_resolved.0.definitions.classes.is_empty(),
        "warm strict recovery resolve must reuse the cached tree"
    );
    let first_messages: Vec<_> = first_diags
        .iter()
        .map(|diag| diag.message.clone())
        .collect();
    let second_messages: Vec<_> = second_diags
        .iter()
        .map(|diag| diag.message.clone())
        .collect();
    assert_eq!(
        second_messages, first_messages,
        "warm strict recovery resolve must preserve cached diagnostics"
    );
}

#[test]
fn semantic_navigation_cache_survives_unrelated_document_edit() {
    let source = r#"package P
  model Dep
    Real y;
  equation
    y = 1;
  end Dep;

  model Root
    Dep dep;
  equation
    dep.y = 2;
  end Root;
end P;
"#;

    let mut session = Session::default();
    session
        .add_document("root.mo", source)
        .expect("root document should parse");
    session
        .add_document("other.mo", "model Other\n  Real z;\nend Other;\n")
        .expect("unrelated document should parse");

    session
        .resolved_for_semantic_navigation("P.Root")
        .expect("navigation tree should build");
    let cached = session
        .query_state
        .resolved
        .semantic_navigation
        .get_mut("P.Root")
        .expect("navigation artifact should be cached");
    cached.resolved = Arc::new(ast::ResolvedTree::new(ast::ClassTree::new()));

    let parse_err = session.update_document("other.mo", "model Other\n  Real z = 1;\nend Other;\n");
    assert!(parse_err.is_none(), "unrelated edit should remain valid");
    assert!(
        session.has_semantic_navigation_cached("P.Root"),
        "unrelated edits should not invalidate active-target navigation cache"
    );

    let second = session
        .resolved_for_semantic_navigation("P.Root")
        .expect("navigation tree should still reuse cache");
    assert!(
        second.0.definitions.classes.is_empty(),
        "unrelated edits must preserve the cached navigation artifact"
    );
}

#[test]
fn semantic_navigation_cache_invalidates_after_document_edit() {
    let source = r#"package P
  model Dep
    Real y;
  equation
    y = 1;
  end Dep;

  model Root
    Dep dep;
  equation
    dep.y = 2;
  end Root;
end P;
"#;
    let updated = r#"package P
  model Dep
    Real y;
  equation
    y = 3;
  end Dep;

  model Root
    Dep dep;
  equation
    dep.y = 4;
  end Root;
end P;
"#;

    let mut session = Session::default();
    session
        .add_document("test.mo", source)
        .expect("document should parse");

    session
        .resolved_for_semantic_navigation("P.Root")
        .expect("navigation tree should build");
    session
        .query_state
        .resolved
        .semantic_navigation
        .get_mut("P.Root")
        .expect("navigation artifact should be cached")
        .resolved = Arc::new(ast::ResolvedTree::new(ast::ClassTree::new()));

    let parse_err = session.update_document("test.mo", updated);
    assert!(parse_err.is_none(), "edited document should remain valid");

    let rebuilt = session
        .resolved_for_semantic_navigation("P.Root")
        .expect("navigation tree should rebuild");
    assert!(
        rebuilt.0.get_class_by_qualified_name("P.Root").is_some(),
        "rebuilt navigation tree should include the active target"
    );
    assert!(
        !rebuilt.0.definitions.classes.is_empty(),
        "edited document must rebuild semantic navigation instead of reusing stale cache"
    );
}

#[test]
fn semantic_navigation_cache_survives_unrelated_edits_but_rebuilds_after_dependency_changes() {
    let base_v1 = r#"model Base
  Real y;
equation
  y = 1;
end Base;
"#;
    let base_v2 = r#"model Base
  Real y;
equation
  y = 3;
end Base;
"#;
    let child = r#"model Child
  Base base;
equation
  base.y = 2;
end Child;
"#;
    let other_v1 = "model Other\n  Real z;\nequation\n  z = 0;\nend Other;\n";
    let other_v2 = "model Other\n  Real z;\nequation\n  z = 4;\nend Other;\n";

    let mut session = Session::default();
    session
        .add_document("base.mo", base_v1)
        .expect("Base should parse");
    session
        .add_document("child.mo", child)
        .expect("Child should parse");
    session
        .add_document("other.mo", other_v1)
        .expect("Other should parse");

    let first = session
        .resolved_for_semantic_navigation("Child")
        .expect("navigation tree should build");
    assert!(
        first.0.get_class_by_qualified_name("Child").is_some(),
        "navigation tree should include Child"
    );
    session
        .query_state
        .resolved
        .semantic_navigation
        .get_mut("Child")
        .expect("navigation artifact should be cached")
        .resolved = Arc::new(ast::ResolvedTree::new(ast::ClassTree::new()));

    session.update_document("other.mo", other_v2);
    let second = session
        .resolved_for_semantic_navigation("Child")
        .expect("unrelated edit should keep cached navigation artifact");
    assert!(
        second.0.definitions.classes.is_empty(),
        "unrelated edits should not evict Child semantic navigation cache"
    );

    session.update_document("base.mo", base_v2);
    let third = session
        .resolved_for_semantic_navigation("Child")
        .expect("dependency edit should rebuild navigation artifact");
    assert!(
        third.0.get_class_by_qualified_name("Child").is_some(),
        "dependency edits must rebuild semantic navigation instead of reusing stale cache"
    );
    assert!(
        !third.0.definitions.classes.is_empty(),
        "rebuilt navigation tree must not reuse the sentinel cache entry"
    );
}

#[test]
fn compile_model_diagnostics_reuses_semantic_closure_cache() {
    let source = r#"model M
  Real x(start=0);
equation
  der(x) = 1;
end M;
"#;

    let mut session = Session::default();
    session
        .add_document("test.mo", source)
        .expect("document should parse");

    let first = session.compile_model_diagnostics("M");
    assert!(
        first.diagnostics.is_empty(),
        "test model should be clean before cache mutation"
    );
    let cached = model_stage_semantic_diagnostics_artifact_mut(
        &mut session,
        "M",
        SemanticDiagnosticsMode::Standard,
    );
    cached
        .diagnostics
        .diagnostics
        .push(CommonDiagnostic::warning(
            "ETEST",
            "cached semantic diagnostics reused",
            PrimaryLabel::new(Span::DUMMY).with_message("cache sentinel"),
        ));

    let second = session.compile_model_diagnostics("M");
    assert!(
        second
            .diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("ETEST")),
        "second diagnostics request must reuse the cached artifact"
    );
}

#[test]
fn semantic_diagnostics_cache_invalidates_after_document_edit() {
    let source = r#"model M
  Real x(start=0);
equation
  der(x) = 1;
end M;
"#;
    let updated = r#"model M
  Real x(start=0);
equation
  der(x) = 2;
end M;
"#;

    let mut session = Session::default();
    session
        .add_document("test.mo", source)
        .expect("document should parse");

    let first = session.compile_model_diagnostics("M");
    assert!(
        first.diagnostics.is_empty(),
        "test model should be clean before cache mutation"
    );
    let cached = model_stage_semantic_diagnostics_artifact_mut(
        &mut session,
        "M",
        SemanticDiagnosticsMode::Standard,
    );
    cached
        .diagnostics
        .diagnostics
        .push(CommonDiagnostic::warning(
            "ETEST",
            "cached semantic diagnostics reused",
            PrimaryLabel::new(Span::DUMMY).with_message("cache sentinel"),
        ));

    let parse_err = session.update_document("test.mo", updated);
    assert!(parse_err.is_none(), "edited document should remain valid");

    let second = session.compile_model_diagnostics("M");
    assert!(
        second
            .diagnostics
            .iter()
            .all(|diag| diag.code.as_deref() != Some("ETEST")),
        "document edits must rebuild diagnostics instead of reusing stale cache"
    );
}

#[test]
fn semantic_diagnostics_cache_survives_unrelated_edits_but_rebuilds_after_dependency_changes() {
    let base_v1 = r#"model Base
  Real y(start=0);
equation
  der(y) = 1;
end Base;
"#;
    let base_v2 = r#"model Base
  Real y(start=0);
equation
  der(y) = 3;
end Base;
"#;
    let child = r#"model Child
  Base base;
  Real x(start=0);
equation
  der(x) = base.y;
end Child;
"#;
    let other_v1 = "model Other\n  Real z(start=0);\nequation\n  der(z) = 0;\nend Other;\n";
    let other_v2 = "model Other\n  Real z(start=0);\nequation\n  der(z) = 4;\nend Other;\n";

    let mut session = Session::default();
    session
        .add_document("base.mo", base_v1)
        .expect("Base should parse");
    session
        .add_document("child.mo", child)
        .expect("Child should parse");
    session
        .add_document("other.mo", other_v1)
        .expect("Other should parse");

    let first = session.compile_model_diagnostics("Child");
    assert!(
        first.diagnostics.is_empty(),
        "Child should be clean before cache mutation"
    );
    model_stage_semantic_diagnostics_artifact_mut(
        &mut session,
        "Child",
        SemanticDiagnosticsMode::Standard,
    )
    .diagnostics
    .diagnostics
    .push(CommonDiagnostic::warning(
        "ETEST",
        "cached semantic diagnostics reused",
        PrimaryLabel::new(Span::DUMMY).with_message("cache sentinel"),
    ));

    session.update_document("other.mo", other_v2);
    let second = session.compile_model_diagnostics("Child");
    assert!(
        second
            .diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("ETEST")),
        "unrelated edits should keep the cached semantic diagnostics artifact"
    );

    session.update_document("base.mo", base_v2);
    let third = session.compile_model_diagnostics("Child");
    assert!(
        third
            .diagnostics
            .iter()
            .all(|diag| diag.code.as_deref() != Some("ETEST")),
        "dependency edits must rebuild semantic diagnostics instead of reusing stale cache"
    );
}

fn set_child_compile_cache_marker(session: &mut Session, marker: String) {
    session
        .query_state
        .dae
        .compile_results
        .get_mut("Child")
        .expect("Child should have a compile cache entry")
        .result = PhaseResult::NeedsInner {
        missing_inners: vec![marker],
    };
}

fn expect_cached_child_compile(session: &mut Session, marker: &str) {
    match session
        .compile_model_phases("Child")
        .expect("Child should compile after unrelated edit")
    {
        PhaseResult::NeedsInner { missing_inners } => {
            assert_eq!(missing_inners, vec![marker.to_string()]);
        }
        other => panic!("expected cached Child compile marker, got {other:?}"),
    }
}

fn set_child_navigation_cache_sentinel(session: &mut Session) {
    session
        .query_state
        .resolved
        .semantic_navigation
        .get_mut("Child")
        .expect("Child semantic navigation should be cached")
        .resolved = Arc::new(ast::ResolvedTree::new(ast::ClassTree::new()));
}

fn expect_warm_child_navigation(session: &mut Session) {
    set_child_navigation_cache_sentinel(session);
    let resolved = session
        .resolved_for_semantic_navigation("Child")
        .expect("Child semantic navigation should succeed");
    assert!(
        resolved.0.definitions.classes.is_empty(),
        "unrelated edits should not rebuild Child semantic navigation"
    );
}

fn expect_cold_child_navigation(session: &mut Session) {
    let resolved = session
        .resolved_for_semantic_navigation("Child")
        .expect("Child semantic navigation should rebuild after dependency edit");
    assert!(
        resolved.0.get_class_by_qualified_name("Child").is_some(),
        "rebuilt semantic navigation should still resolve Child"
    );
    assert!(
        !resolved.0.definitions.classes.is_empty(),
        "dependency edits should rebuild Child semantic navigation"
    );
}

fn set_child_diagnostics_cache_sentinel(session: &mut Session) {
    let cached = model_stage_semantic_diagnostics_artifact_mut(
        session,
        "Child",
        SemanticDiagnosticsMode::Standard,
    );
    cached.diagnostics.diagnostics = vec![CommonDiagnostic::warning(
        "ETEST",
        "cached semantic diagnostics reused",
        PrimaryLabel::new(Span::DUMMY).with_message("cache sentinel"),
    )];
}

#[test]
fn lru_cache_helpers_bound_size_and_refresh_recent_entries() {
    let mut cache = IndexMap::new();
    insert_lru_cache_entry(&mut cache, "A".to_string(), 1_u8, 2);
    insert_lru_cache_entry(&mut cache, "B".to_string(), 2_u8, 2);

    assert_eq!(get_lru_cache_entry(&mut cache, "A"), Some(1));

    insert_lru_cache_entry(&mut cache, "C".to_string(), 3_u8, 2);

    assert!(
        cache.contains_key("A"),
        "recently touched entry should stay cached"
    );
    assert!(
        !cache.contains_key("B"),
        "least-recently-used entry should be evicted first"
    );
    assert!(cache.contains_key("C"), "new entry should be inserted");
}

mod compile_diagnostics_tests;

mod cache_behavior_tests;

mod declaration_index_tests;

mod class_body_tests;

mod file_outline_tests;

mod file_summary_tests;

mod package_def_map_tests;

mod class_interface_tests;

mod class_member_query_tests;

mod model_closure_tests;

mod instantiation_query_tests;

mod typed_model_query_tests;

mod flat_model_query_tests;

mod dae_model_query_tests;

mod class_body_semantics_tests;

mod semantic_diagnostics_tests;

mod persisted_summary_tests;

mod workspace_symbol_snapshot_tests;
