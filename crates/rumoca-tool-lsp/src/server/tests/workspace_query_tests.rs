use super::*;

use super::editor_surface_tests::{
    assert_code_actions_wrap_handler, assert_document_links_and_inlay_hints,
    assert_formatting_wraps_handler,
};

const QUERY_CLASS_LIBRARY_SOURCE: &str = r#"package Lib
  block Target
    Real y;
  equation
    y = 1;
  end Target;
end Lib;
"#;

const QUERY_CLASS_ACTIVE_SOURCE: &str = r#"model M
  import Lib.Target;
  import Renamed = Lib.Target;
  Target a;
  Lib.Target b;
  Renamed c;
equation
  a.y = b.y + c.y;
end M;
"#;

fn query_class_reference_position() -> Position {
    Position {
        line: 3,
        character: "  Target".len() as u32,
    }
}

async fn seed_query_class_workspace_documents(
    server: &ModelicaLanguageServer,
    library_path: &Path,
    active_uri: &Url,
) {
    let active_key = session_document_uri_key(active_uri);
    let mut session = server.session.write().await;
    session.update_document(&library_path.to_string_lossy(), QUERY_CLASS_LIBRARY_SOURCE);
    session.update_document(&active_key, QUERY_CLASS_ACTIVE_SOURCE);
}

async fn fetch_query_class_references(
    server: &ModelicaLanguageServer,
    active_uri: &Url,
) -> Vec<Location> {
    server
        .references(ReferenceParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: active_uri.clone(),
                },
                position: query_class_reference_position(),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
            context: ReferenceContext {
                include_declaration: true,
            },
        })
        .await
        .expect("references should succeed")
        .expect("references should exist")
}

async fn prepare_query_class_rename(
    server: &ModelicaLanguageServer,
    active_uri: &Url,
) -> PrepareRenameResponse {
    server
        .prepare_rename(TextDocumentPositionParams {
            text_document: TextDocumentIdentifier {
                uri: active_uri.clone(),
            },
            position: query_class_reference_position(),
        })
        .await
        .expect("prepare rename should succeed")
        .expect("prepare rename should allow the class target")
}

async fn rename_query_class_target(
    server: &ModelicaLanguageServer,
    active_uri: &Url,
) -> WorkspaceEdit {
    server
        .rename(RenameParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: active_uri.clone(),
                },
                position: query_class_reference_position(),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            new_name: "RenamedTarget".to_string(),
        })
        .await
        .expect("rename should succeed")
        .expect("rename should produce edits")
}

fn assert_query_class_rename_edits(rename: &WorkspaceEdit, library_uri: &Url, active_uri: &Url) {
    let changes = rename.changes.as_ref().expect("rename changes");
    let library_edits = changes
        .get(library_uri)
        .expect("rename should edit the library declaration file");
    assert!(
        library_edits.iter().any(|edit| edit.range.start.line == 1),
        "rename should update the library declaration"
    );
    assert!(
        library_edits.iter().any(|edit| edit.range.start.line == 5),
        "rename should update the library end name"
    );

    let active_edits = changes
        .get(active_uri)
        .expect("rename should edit the active document");
    assert!(
        active_edits
            .iter()
            .all(|edit| edit.new_text == "RenamedTarget"),
        "rename should apply the requested class name"
    );
    assert!(
        active_edits.iter().any(|edit| edit.range.start.line == 1),
        "rename should update qualified imports"
    );
    assert!(
        active_edits.iter().any(|edit| edit.range.start.line == 2),
        "rename should update renamed-import target paths"
    );
    assert!(
        active_edits.iter().any(|edit| edit.range.start.line == 3),
        "rename should update imported simple type references"
    );
    assert!(
        active_edits.iter().any(|edit| edit.range.start.line == 4),
        "rename should update qualified type references"
    );
    assert!(
        active_edits.iter().all(|edit| edit.range.start.line != 5),
        "rename should not rewrite alias tokens that resolve to the class"
    );
}

#[test]
fn document_symbol_returns_outline_for_active_document() {
    run_async_test(async {
        let path = new_temp_dir("document-symbols").join("active.mo");
        let uri = Url::from_file_path(&path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        seed_surface_document(server, &uri).await;

        let response = server
            .document_symbol(DocumentSymbolParams {
                text_document: TextDocumentIdentifier { uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("document symbols should succeed")
            .expect("document symbols should exist");

        let DocumentSymbolResponse::Nested(symbols) = response else {
            panic!("expected nested document symbols");
        };
        assert!(
            symbols.iter().any(|symbol| symbol.name == "Lib"),
            "document symbols should include the top-level package"
        );
        assert!(
            symbols.iter().any(|symbol| symbol.name == "M"),
            "document symbols should include the active model"
        );
    });
}

#[test]
fn document_symbol_request_reuses_session_cache() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        let path = new_temp_dir("document-symbol-query-cache").join("active.mo");
        let uri = Url::from_file_path(&path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        seed_surface_document(server, &uri).await;

        reset_session_cache_stats();
        let before_first = session_cache_stats();
        let first = fetch_document_symbols(server, &uri).await;
        let first_delta = session_cache_stats().delta_since(before_first);
        assert!(
            first_delta.document_symbol_query_misses >= 1,
            "first document symbol request should miss query cache"
        );
        assert!(
            document_symbol_names(&first).contains(&"Lib"),
            "document outline should include top-level package"
        );

        let before_second = session_cache_stats();
        let second = fetch_document_symbols(server, &uri).await;
        let second_delta = session_cache_stats().delta_since(before_second);
        assert_eq!(
            first.len(),
            second.len(),
            "outline should be stable when unchanged"
        );
        assert!(
            second_delta.document_symbol_query_hits >= 1,
            "document symbol request should hit cache on repeated call"
        );

        let changed_source = r#"package Lib
  model Helper2
    parameter Real gain = 1;
    output Real y;
  equation
    y = gain;
  end Helper2;
end Lib;

model M
  import Alias = Lib.Helper2;
  Real arr[2, 3];
  String local = \"./Lib/package.mo\";
  Alias helperInst;
equation
  helperInst.y = sin(helperInst.gain);
end M;
"#;
        server
            .did_change(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: uri.clone(),
                    version: 2,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: changed_source.to_string(),
                }],
            })
            .await;

        let before_edit = session_cache_stats();
        let after_edit = fetch_document_symbols(server, &uri).await;
        let edit_delta = session_cache_stats().delta_since(before_edit);
        assert!(
            document_symbol_tree_contains_name(&after_edit, "Helper2"),
            "edited document should expose updated outline symbol"
        );
        assert!(
            edit_delta.document_symbol_query_misses >= 1,
            "document change should invalidate document symbol cache"
        );
    });
}

#[test]
fn document_symbol_returns_outline_for_open_library_document() {
    run_async_test(async {
        let temp = new_temp_dir("document-symbol-library-overlay");
        let library_root = write_test_library(&temp, "Lib");
        let library_file = library_root.join("package.mo");
        let uri = Url::from_file_path(&library_file).expect("file uri");
        let source = std::fs::read_to_string(&library_file).expect("read test library");

        let service = new_test_service();
        let server = service.inner();
        *server.library_paths.write().await = vec![library_root.to_string_lossy().to_string()];

        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "modelica".to_string(),
                    version: 1,
                    text: source,
                },
            })
            .await;

        let symbols = fetch_document_symbols(server, &uri).await;
        assert!(
            document_symbol_tree_contains_name(&symbols, "Lib"),
            "open library document should return package symbols"
        );
        assert!(
            document_symbol_tree_contains_name(&symbols, "A"),
            "open library document should return nested class symbols"
        );
    });
}

#[test]
fn semantic_tokens_full_returns_non_empty_tokens() {
    run_async_test(async {
        let path = new_temp_dir("semantic-tokens").join("active.mo");
        let uri = Url::from_file_path(&path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        seed_surface_document(server, &uri).await;

        let response = server
            .semantic_tokens_full(SemanticTokensParams {
                text_document: TextDocumentIdentifier { uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("semantic tokens should succeed")
            .expect("semantic tokens should exist");

        let SemanticTokensResult::Tokens(tokens) = response else {
            panic!("expected full semantic tokens");
        };
        assert!(
            !tokens.data.is_empty(),
            "semantic token response should contain encoded tokens"
        );
    });
}

#[test]
fn references_prepare_rename_and_rename_follow_component_occurrences() {
    run_async_test(async {
        let path = new_temp_dir("references-rename").join("active.mo");
        let uri = Url::from_file_path(&path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        seed_surface_document(server, &uri).await;

        let references = server
            .references(ReferenceParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: surface_component_position(),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: ReferenceContext {
                    include_declaration: true,
                },
            })
            .await
            .expect("references should succeed")
            .expect("references should exist");
        assert!(
            references.len() >= 2,
            "references should include the declaration and use sites"
        );

        let prepare = server
            .prepare_rename(TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: surface_component_position(),
            })
            .await
            .expect("prepare rename should succeed")
            .expect("prepare rename should allow the component");
        let PrepareRenameResponse::Range(range) = prepare else {
            panic!("expected rename range");
        };
        assert_eq!(range.start.line, 15);

        let rename = server
            .rename(RenameParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: surface_component_position(),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                new_name: "renamedInst".to_string(),
            })
            .await
            .expect("rename should succeed")
            .expect("rename should produce edits");
        let edits = rename
            .changes
            .as_ref()
            .and_then(|changes| changes.get(&uri))
            .expect("rename edits should target the active document");
        assert!(
            edits.len() >= 2,
            "rename should edit both declaration and use sites"
        );
        assert!(
            edits.iter().all(|edit| edit.new_text == "renamedInst"),
            "rename edits should use the requested name"
        );
    });
}

#[test]
fn references_prepare_rename_and_rename_follow_query_backed_class_targets() {
    run_async_test(async {
        let temp = new_temp_dir("references-rename-query-class");
        let library_path = temp.join("lib.mo");
        let library_uri = Url::from_file_path(&library_path).expect("file uri");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        seed_query_class_workspace_documents(server, &library_path, &active_uri).await;

        let references = fetch_query_class_references(server, &active_uri).await;
        assert!(
            references
                .iter()
                .any(|location| { location.uri == library_uri && location.range.start.line == 1 }),
            "references should include the library declaration"
        );
        assert!(
            references
                .iter()
                .any(|location| { location.uri == active_uri && location.range.start.line == 5 }),
            "references should include alias-based type uses that resolve to the same class"
        );

        let prepare = prepare_query_class_rename(server, &active_uri).await;
        let PrepareRenameResponse::Range(range) = prepare else {
            panic!("expected rename range");
        };
        assert_eq!(range.start.line, 3);

        let rename = rename_query_class_target(server, &active_uri).await;
        assert_query_class_rename_edits(&rename, &library_uri, &active_uri);
    });
}

#[test]
fn references_prepare_rename_and_rename_follow_modifier_body_class_targets() {
    run_async_test(async {
        let path = new_temp_dir("references-rename-modifier-class").join("active.mo");
        let uri = Url::from_file_path(&path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        let uri_key = session_document_uri_key(&uri);
        let source = r#"model DefaultVariant
  Real x;
end DefaultVariant;

model Base
  replaceable model Variant = DefaultVariant;
end Base;

model Test
  Base base(replaceable model Variant = DefaultVariant);
end Test;
"#;

        let mut session = server.session.write().await;
        session.update_document(&uri_key, source);
        drop(session);

        let modifier_target_position = Position {
            line: 9,
            character: "  Base base(replaceable model Variant = Default".len() as u32,
        };

        let references = server
            .references(ReferenceParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: modifier_target_position,
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: ReferenceContext {
                    include_declaration: true,
                },
            })
            .await
            .expect("references should succeed")
            .expect("references should exist");
        assert!(
            references
                .iter()
                .any(|location| location.range.start.line == 0),
            "references should include the declaring class"
        );
        assert!(
            references
                .iter()
                .any(|location| location.range.start.line == 5),
            "references should include the base-model modifier use"
        );
        assert!(
            references
                .iter()
                .any(|location| location.range.start.line == 9),
            "references should include the component modifier use"
        );

        let prepare = server
            .prepare_rename(TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: modifier_target_position,
            })
            .await
            .expect("prepare rename should succeed")
            .expect("prepare rename should allow the modifier class target");
        let PrepareRenameResponse::Range(range) = prepare else {
            panic!("expected rename range");
        };
        assert_eq!(range.start.line, 9);

        let rename = server
            .rename(RenameParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: modifier_target_position,
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                new_name: "RenamedVariant".to_string(),
            })
            .await
            .expect("rename should succeed")
            .expect("rename should produce edits");
        let edits = rename
            .changes
            .as_ref()
            .and_then(|changes| changes.get(&uri))
            .expect("rename edits should target the active document");
        assert!(
            edits.iter().any(|edit| edit.range.start.line == 2),
            "rename should update the end name of the declaring class"
        );
        assert!(
            edits.iter().any(|edit| edit.range.start.line == 5),
            "rename should update the base-model modifier use"
        );
        assert!(
            edits.iter().any(|edit| edit.range.start.line == 9),
            "rename should update the component modifier use"
        );
    });
}

#[test]
fn workspace_symbol_signature_help_and_folding_cover_active_document() {
    run_async_test(async {
        let path = new_temp_dir("workspace-symbol-signature").join("active.mo");
        let uri = Url::from_file_path(&path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        seed_surface_document(server, &uri).await;

        let symbols = server
            .symbol(WorkspaceSymbolParams {
                query: "Helper".to_string(),
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("workspace symbol should succeed")
            .expect("workspace symbol response");
        assert!(
            symbols.iter().any(|symbol| symbol.name == "Helper"),
            "workspace symbol should match nested classes"
        );

        let signature = server
            .signature_help(SignatureHelpParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: surface_signature_position(),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                context: None,
            })
            .await
            .expect("signature help should succeed")
            .expect("signature help should exist");
        assert_eq!(signature.active_parameter, Some(0));
        assert!(
            signature.signatures[0].label.starts_with("sin("),
            "signature help should resolve the builtin signature"
        );

        let ranges = server
            .folding_range(FoldingRangeParams {
                text_document: TextDocumentIdentifier { uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("folding range should succeed")
            .expect("folding ranges should exist");
        assert!(
            ranges
                .iter()
                .any(|range| range.kind == Some(FoldingRangeKind::Comment)),
            "folding should include the consecutive comment region"
        );
        assert!(
            ranges
                .iter()
                .any(|range| range.kind == Some(FoldingRangeKind::Region)),
            "folding should include class or section regions"
        );
    });
}

#[test]
fn workspace_symbol_query_reuses_cached_symbols() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        reset_session_cache_stats();
        let path = new_temp_dir("workspace-symbol-cache").join("active.mo");
        let uri = Url::from_file_path(&path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        seed_surface_document(server, &uri).await;

        let before_first = session_cache_stats();
        let first = fetch_workspace_symbols(server, "Helper").await;
        let first_delta = session_cache_stats().delta_since(before_first);
        assert!(
            has_workspace_symbol_named(&first, "Helper"),
            "first workspace symbol query should include top-level model symbol"
        );
        assert!(
            first_delta.workspace_symbol_query_misses >= 1,
            "first workspace symbol call should miss the query cache"
        );
        assert!(
            first_delta.file_item_index_query_misses >= 1,
            "first workspace symbol call should build per-file symbol indexes"
        );

        let before_second = session_cache_stats();
        let second = fetch_workspace_symbols(server, "Helper").await;
        let second_delta = session_cache_stats().delta_since(before_second);
        assert_eq!(
            second, first,
            "repeated query should use cached workspace symbols"
        );
        assert!(
            second_delta.workspace_symbol_query_hits >= 1,
            "second workspace symbol call should hit the query cache"
        );
        assert_eq!(
            second_delta.file_item_index_query_hits, 0,
            "cached workspace query should avoid rebuilding file indexes"
        );

        let changed_source = r#"package Lib
  model Helper2
    parameter Real gain = 1;
    output Real y;
  equation
    y = gain;
  end Helper2;
end Lib;

model M
  import Alias = Lib.Helper2;
  Real arr[2, 3];
  String local = \"./Lib/package.mo\";
  Alias helperInst;
equation
  helperInst.y = sin(helperInst.gain);
end M;
"#;
        server
            .did_change(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: uri.clone(),
                    version: 2,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: changed_source.to_string(),
                }],
            })
            .await;

        let before_edit = session_cache_stats();
        let after_edit = fetch_workspace_symbols(server, "Helper2").await;
        let edit_delta = session_cache_stats().delta_since(before_edit);
        assert!(
            has_workspace_symbol_named(&after_edit, "Helper2"),
            "edited document should update workspace symbol query results"
        );
        assert!(
            edit_delta.workspace_symbol_query_misses >= 1,
            "edit should invalidate workspace symbol cache"
        );
    });
}

#[test]
fn workspace_symbol_query_stays_warm_for_body_only_edits() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        reset_session_cache_stats();
        let path = new_temp_dir("workspace-symbol-body-edit-cache").join("active.mo");
        let uri = Url::from_file_path(&path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        seed_surface_document(server, &uri).await;

        let first = fetch_workspace_symbols(server, "Helper").await;
        assert!(
            has_workspace_symbol_named(&first, "Helper"),
            "initial workspace symbol query should include the helper model"
        );

        let changed_source = r#"package Lib
  model Helper
    parameter Real gain = 1;
    output Real y;
  equation
    y = gain;
  end Helper;
end Lib;

// docs
// https://example.com/docs
model M
  import Alias = Lib.Helper;
  Real arr[2, 3];
  String local = "./Lib/package.mo";
  Alias helperInst;
equation
  helperInst.y = cos(helperInst.gain);
end M;
"#;
        server
            .did_change(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: uri.clone(),
                    version: 2,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: changed_source.to_string(),
                }],
            })
            .await;

        let before_edit_query = session_cache_stats();
        let after_edit = fetch_workspace_symbols(server, "Helper").await;
        let edit_delta = session_cache_stats().delta_since(before_edit_query);
        assert!(
            has_workspace_symbol_named(&after_edit, "Helper"),
            "body-only edit should preserve workspace symbol results"
        );
        assert!(
            edit_delta.workspace_symbol_query_hits >= 1,
            "body-only edit should keep the workspace symbol cache warm"
        );
        assert_eq!(
            edit_delta.workspace_symbol_query_misses, 0,
            "body-only edit should not invalidate the workspace symbol cache"
        );
        assert_eq!(
            edit_delta.file_item_index_query_hits + edit_delta.file_item_index_query_misses,
            0,
            "warm workspace symbol reuse should avoid rebuilding per-file symbol indexes"
        );
    });
}

#[test]
fn workspace_symbol_timing_summary_reports_snapshot_and_query_breakdown() {
    let _guard = session_stats_test_guard();
    let temp = new_temp_dir("workspace-symbol-timing");
    let timing_path = temp.join("navigation-timings.jsonl");

    run_async_test(async {
        reset_session_cache_stats();
        let path = temp.join("active.mo");
        let uri = Url::from_file_path(&path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        *server.navigation_timing_path.write().await = Some(timing_path.clone());
        seed_surface_document(server, &uri).await;

        let symbols = fetch_workspace_symbols(server, "Helper").await;
        assert!(
            has_workspace_symbol_named(&symbols, "Helper"),
            "workspace symbol request should include the helper model"
        );
    });

    let entries: Vec<LoggedNavigationTimingSummary> = read_jsonl(&timing_path);
    assert_eq!(
        entries.len(),
        1,
        "expected one workspace symbol timing entry"
    );

    let entry = &entries[0];
    assert_eq!(entry.request, "workspace_symbol");
    assert_eq!(entry.request_path, "query_only");
    assert_eq!(entry.semantic_layer, "workspace_symbol");
    assert!(
        entry.snapshot_ms.is_some(),
        "workspace symbol timing should record snapshot acquisition"
    );
    assert!(
        entry.snapshot_lock_ms.is_some(),
        "workspace symbol timing should record snapshot lock wait"
    );
    assert!(
        entry.snapshot_build_ms.is_some(),
        "workspace symbol timing should record snapshot build time"
    );
    assert!(
        entry
            .detail
            .as_deref()
            .is_some_and(|detail| detail.contains("validate=")),
        "workspace symbol timing should include snapshot substep detail"
    );
    assert!(
        entry.query_ms.is_some(),
        "workspace symbol timing should record session query time"
    );
    assert!(
        entry.format_ms.is_some(),
        "workspace symbol timing should record LSP formatting time"
    );
    assert!(
        !entry.built_resolved_tree,
        "workspace symbol timing should stay on the query-backed path"
    );
    assert_eq!(
        entry.session_cache_delta.semantic_navigation_builds, 0,
        "workspace symbol request should not build semantic navigation"
    );
}

async fn fetch_workspace_symbols(
    server: &ModelicaLanguageServer,
    query: &str,
) -> Vec<SymbolInformation> {
    server
        .symbol(WorkspaceSymbolParams {
            query: query.to_string(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        })
        .await
        .expect("workspace symbol should succeed")
        .expect("workspace symbol response")
}

async fn fetch_document_symbols(server: &ModelicaLanguageServer, uri: &Url) -> Vec<DocumentSymbol> {
    match server
        .document_symbol(DocumentSymbolParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        })
        .await
        .expect("document symbols should succeed")
    {
        Some(DocumentSymbolResponse::Nested(symbols)) => symbols,
        Some(other) => panic!("expected nested document symbols, got: {other:?}"),
        None => panic!("expected document symbols for active document"),
    }
}

fn document_symbol_names(symbols: &[DocumentSymbol]) -> Vec<&str> {
    symbols.iter().map(|symbol| symbol.name.as_str()).collect()
}

fn document_symbol_tree_contains_name(symbols: &[DocumentSymbol], target: &str) -> bool {
    symbols.iter().any(|symbol| {
        symbol.name == target
            || symbol
                .children
                .as_ref()
                .is_some_and(|children| document_symbol_tree_contains_name(children, target))
    })
}

fn has_workspace_symbol_named(symbols: &[SymbolInformation], name: &str) -> bool {
    symbols.iter().any(|symbol| symbol.name == name)
}

#[test]
fn formatting_code_actions_inlay_hints_and_document_links_wrap_handler_results() {
    run_async_test(async {
        let formatting_path = new_temp_dir("formatting-links").join("active.mo");
        let formatting_uri = Url::from_file_path(&formatting_path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &session_document_uri_key(&formatting_uri),
                "model F\nReal x;\nequation\nx=1;\nend F;\n",
            );
        }
        assert_formatting_wraps_handler(server, &formatting_uri).await;

        let surface_uri =
            Url::from_file_path(new_temp_dir("links").join("surface.mo")).expect("file uri");
        seed_surface_document(server, &surface_uri).await;
        assert_document_links_and_inlay_hints(server, &surface_uri).await;

        {
            let mut session = server.session.write().await;
            session.update_document(
                &session_document_uri_key(&formatting_uri),
                BROKEN_ACTION_SOURCE,
            );
        }
        assert_code_actions_wrap_handler(server, &formatting_uri).await;
    });
}
