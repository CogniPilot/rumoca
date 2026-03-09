use super::*;
use std::collections::HashSet;
use std::path::PathBuf;
use tower_lsp::LspService;

fn run_async_test<F>(future: F)
where
    F: std::future::Future<Output = ()>,
{
    tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime")
        .block_on(future);
}

#[test]
fn import_prefix_detection_handles_partial_qualified_name() {
    let source = "model M\n  import Mo\nend M;\n";
    let pos = Position {
        line: 1,
        character: "  import Mo".len() as u32,
    };
    assert_eq!(
        extract_import_completion_prefix(source, pos),
        Some("Mo".to_string())
    );

    let source = "model M\n  import Modelica.Blocks.Co\nend M;\n";
    let pos = Position {
        line: 1,
        character: "  import Modelica.Blocks.Co".len() as u32,
    };
    assert_eq!(
        extract_import_completion_prefix(source, pos),
        Some("Modelica.Blocks.Co".to_string())
    );
}

#[test]
fn import_prefix_detection_ignores_non_import_lines() {
    let source = "model M\n  Real x;\nend M;\n";
    let pos = Position {
        line: 1,
        character: "  Real x".len() as u32,
    };
    assert_eq!(extract_import_completion_prefix(source, pos), None);
}

#[test]
fn completion_context_requires_resolved_session_for_import_root_prefix() {
    let source = "model M\n  import Mo\nend M;\n";
    let pos = Position {
        line: 1,
        character: "  import Mo".len() as u32,
    };
    let import_prefix = extract_import_completion_prefix(source, pos);
    assert!(completion_context_needs_resolved_session(
        source,
        pos,
        import_prefix.as_deref()
    ));
}

#[test]
fn completion_context_does_not_require_resolved_session_for_plain_identifier() {
    let source = "model M\n  Re\nend M;\n";
    let pos = Position {
        line: 1,
        character: "  Re".len() as u32,
    };
    let import_prefix = extract_import_completion_prefix(source, pos);
    assert!(!completion_context_needs_resolved_session(
        source,
        pos,
        import_prefix.as_deref()
    ));
}

#[test]
fn reserve_library_load_rejects_loaded_loading_and_stale_epoch() {
    let mut loaded = HashSet::new();
    loaded.insert("library::modelica".to_string());
    let mut loading = HashMap::new();
    loading.insert("library::vendor".to_string(), 5);

    assert!(!should_reserve_library_load(
        &loaded,
        &loading,
        "library::modelica",
        5,
        5
    ));
    assert!(!should_reserve_library_load(
        &loaded,
        &loading,
        "library::vendor",
        5,
        5
    ));
    assert!(!should_reserve_library_load(
        &loaded,
        &loading,
        "library::new",
        4,
        5
    ));
    assert!(should_reserve_library_load(
        &loaded,
        &loading,
        "library::new",
        5,
        5
    ));
}

#[test]
fn apply_library_load_requires_fresh_epoch_and_unloaded_path() {
    let mut loaded = HashSet::new();
    loaded.insert("library::modelica".to_string());

    assert!(!should_apply_library_load(
        &loaded,
        "library::modelica",
        8,
        8
    ));
    assert!(!should_apply_library_load(&loaded, "library::new", 7, 8));
    assert!(should_apply_library_load(&loaded, "library::new", 8, 8));
}

#[test]
fn clear_library_load_requires_matching_owner_epoch() {
    let mut loading = HashMap::new();
    loading.insert("library::modelica".to_string(), 9);

    assert!(!should_clear_library_load(&loading, "library::modelica", 8));
    assert!(should_clear_library_load(&loading, "library::modelica", 9));
    assert!(!should_clear_library_load(&loading, "library::new", 9));
}

#[test]
fn stale_epoch_load_cannot_clear_new_epoch_reservation() {
    run_async_test(async {
        let (service, _socket) = LspService::new(ModelicaLanguageServer::new);
        let server = service.inner();
        let path_key = "library::modelica";

        assert!(server.reserve_library_load(path_key, 0).await);
        server.reset_session_and_loaded_libraries().await;
        assert_eq!(server.current_library_state_epoch(), 1);

        assert!(server.reserve_library_load(path_key, 1).await);

        server.cancel_library_load(path_key, 0).await;
        assert_eq!(
            server.loading_libraries.read().await.get(path_key),
            Some(&1)
        );

        let stale_apply = server
            .apply_parsed_library_if_current(
                "library::modelica",
                path_key,
                "active.mo",
                Vec::new(),
                0,
            )
            .await;
        assert!(stale_apply.is_none(), "stale epoch apply should be ignored");
        assert_eq!(
            server.loading_libraries.read().await.get(path_key),
            Some(&1)
        );

        let current_apply = server
            .apply_parsed_library_if_current(
                "library::modelica",
                path_key,
                "active.mo",
                Vec::new(),
                1,
            )
            .await;
        assert_eq!(current_apply, Some(0));
        assert!(
            server
                .loading_libraries
                .read()
                .await
                .get(path_key)
                .is_none(),
            "successful current epoch apply should clear reservation"
        );
        assert!(
            server.loaded_libraries.read().await.contains(path_key),
            "successful current epoch apply should mark library as loaded"
        );
    });
}

#[test]
fn reserve_library_load_blocks_duplicate_inflight_work() {
    run_async_test(async {
        let (service, _socket) = LspService::new(ModelicaLanguageServer::new);
        let server = service.inner();
        let path_key = "library::modelica";

        assert!(server.reserve_library_load(path_key, 0).await);
        assert!(
            !server.reserve_library_load(path_key, 0).await,
            "same path should not be reservable twice while in-flight"
        );

        server.cancel_library_load(path_key, 0).await;
        assert!(
            server.reserve_library_load(path_key, 0).await,
            "path should become reservable after matching-owner cancel"
        );
    });
}

#[test]
fn session_document_uri_key_uses_decoded_file_path() {
    let path = std::env::temp_dir()
        .join("Modelica Standard Library")
        .join("Blocks")
        .join("Continuous.mo");
    let uri = Url::from_file_path(&path).expect("file uri");
    let key = session_document_uri_key(&uri);
    assert_eq!(PathBuf::from(&key), path);
    assert!(
        !key.contains("%20"),
        "session key should be filesystem path, not URL-encoded: {key}"
    );
}

#[test]
fn project_config_uri_detection_handles_file_paths_with_spaces() {
    let path = std::env::temp_dir()
        .join("workspace with spaces")
        .join(".rumoca")
        .join("project.toml");
    let uri = Url::from_file_path(path).expect("file uri");
    assert!(is_project_config_uri(&uri));
}

#[test]
fn simulation_doc_for_compile_filters_workspace_documents() {
    let focus_uri = "/tmp/focus.mo";
    let focus_key = canonical_path_key(focus_uri);
    let parsed = ast::StoredDefinition::default();
    let loaded_libraries = HashSet::from([canonical_path_key("/opt/msl")]);

    let focus_doc = Document {
        uri: focus_uri.to_string(),
        content: "model Focus end Focus;".to_string(),
        parsed: Some(parsed.clone()),
        parse_error: None,
    };
    let other_workspace_doc = Document {
        uri: "/tmp/other.mo".to_string(),
        content: "model Other end Other;".to_string(),
        parsed: Some(parsed.clone()),
        parse_error: None,
    };
    let library_doc = Document {
        uri: "/opt/msl/Modelica/Blocks/Continuous.mo".to_string(),
        content: String::new(),
        parsed: Some(parsed),
        parse_error: None,
    };

    let focus = simulation_doc_for_compile(focus_uri, &focus_doc, &focus_key, &loaded_libraries)
        .expect("focus doc should be accepted")
        .expect("focus doc should be included");
    assert!(focus.0, "focus document marker should be true");

    let other = simulation_doc_for_compile(
        &other_workspace_doc.uri,
        &other_workspace_doc,
        &focus_key,
        &loaded_libraries,
    )
    .expect("other workspace doc should be evaluated");
    assert!(
        other.is_none(),
        "non-focus workspace docs should be excluded"
    );

    let library = simulation_doc_for_compile(
        &library_doc.uri,
        &library_doc,
        &focus_key,
        &loaded_libraries,
    )
    .expect("library doc should be accepted")
    .expect("library doc should be included");
    assert!(
        !library.0,
        "library docs should be included without marking as focus"
    );
}

#[test]
fn collect_simulation_parsed_docs_keeps_focus_and_libraries_only() {
    let focus_uri = "focus.mo";
    let other_uri = "other.mo";
    let library_uri = "/opt/msl/Modelica/package.mo";
    let source = "model M end M;";

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(focus_uri, source)
        .expect("focus should parse");
    session
        .add_document(other_uri, source)
        .expect("other should parse");
    session.add_parsed(library_uri, ast::StoredDefinition::default());

    let focus_key = canonical_path_key(focus_uri);
    let loaded_libraries = HashSet::from([canonical_path_key("/opt/msl")]);
    let docs = collect_simulation_parsed_docs(&session, focus_uri, &focus_key, &loaded_libraries)
        .expect("docs should build");
    let uris: HashSet<String> = docs.into_iter().map(|(uri, _)| uri).collect();

    assert!(uris.contains(focus_uri), "focus doc must be included");
    assert!(uris.contains(library_uri), "library docs must be included");
    assert!(
        !uris.contains(other_uri),
        "non-focus workspace docs must be excluded"
    );
}

#[test]
fn library_root_for_document_prefers_longest_root() {
    let loaded_libraries = HashSet::from([
        canonical_path_key("/opt"),
        canonical_path_key("/opt/msl"),
        canonical_path_key("/opt/msl/Modelica"),
    ]);
    let root =
        library_root_for_document("/opt/msl/Modelica/Blocks/Continuous.mo", &loaded_libraries)
            .expect("library root should be detected");
    assert_eq!(root, canonical_path_key("/opt/msl/Modelica"));
}

#[test]
fn did_open_library_document_keeps_cached_session_document() {
    run_async_test(async {
        let (service, _socket) = LspService::new(ModelicaLanguageServer::new);
        let server = service.inner();
        let library_root = canonical_path_key("/opt/msl");
        let library_uri = canonical_path_key("/opt/msl/Modelica/Blocks/Continuous.mo");
        let text = "within Modelica.Blocks;\npackage Continuous\nend Continuous;\n";

        {
            let mut session = server.session.write().await;
            session.add_parsed(&library_uri, ast::StoredDefinition::default());
        }
        {
            let mut loaded = server.loaded_libraries.write().await;
            loaded.insert(library_root);
        }

        let uri = Url::from_file_path(&library_uri).expect("file uri");
        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri,
                    language_id: "modelica".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            })
            .await;

        let session_doc = server
            .session
            .read()
            .await
            .get_document(&library_uri)
            .cloned()
            .expect("cached library document must remain in session");
        assert!(
            session_doc.content.is_empty(),
            "shared session should keep cached library snapshot content empty"
        );
        let overlay = server
            .library_document_overlays
            .read()
            .await
            .get(&library_uri)
            .cloned()
            .expect("library overlay should be tracked");
        assert_eq!(overlay.content, text);
    });
}

#[test]
fn did_close_library_document_preserves_cached_session_document() {
    run_async_test(async {
        let (service, _socket) = LspService::new(ModelicaLanguageServer::new);
        let server = service.inner();
        let library_root = canonical_path_key("/opt/msl");
        let library_uri = canonical_path_key("/opt/msl/Modelica/Blocks/Continuous.mo");

        {
            let mut session = server.session.write().await;
            session.add_parsed(&library_uri, ast::StoredDefinition::default());
        }
        {
            let mut loaded = server.loaded_libraries.write().await;
            loaded.insert(library_root);
        }
        server
            .update_library_document_overlay(&library_uri, "within Modelica.Blocks;")
            .await;

        let uri = Url::from_file_path(&library_uri).expect("file uri");
        server
            .did_close(DidCloseTextDocumentParams {
                text_document: TextDocumentIdentifier { uri },
            })
            .await;

        let session_doc = server
            .session
            .read()
            .await
            .get_document(&library_uri)
            .cloned();
        assert!(
            session_doc.is_some(),
            "closing overlay should not drop cached library document from session"
        );
        assert!(
            !server
                .library_document_overlays
                .read()
                .await
                .contains_key(&library_uri),
            "overlay must be removed on close"
        );
    });
}
