use super::*;

#[test]
fn workspace_document_mutations_do_not_reschedule_namespace_prewarm() {
    run_async_test(async {
        let workspace_root = new_temp_dir("workspace-no-namespace-prewarm");
        let library_root = write_test_library(&workspace_root, "InitLib");
        let workspace_uri = Url::from_directory_path(&workspace_root).expect("workspace uri");
        let service = new_test_service();
        let server = service.inner();

        server
            .initialize(InitializeParams {
                root_uri: Some(workspace_uri),
                initialization_options: Some(serde_json::json!({
                    "modelicaPath": [library_root.to_string_lossy().to_string()]
                })),
                ..InitializeParams::default()
            })
            .await
            .expect("initialize should succeed");
        server.wait_for_namespace_prewarm_if_pending().await;
        assert!(
            server.namespace_prewarm_state.read().await.is_none(),
            "startup durable-root prewarm should finish before the workspace edit probe"
        );

        let document_path = workspace_root.join("active.mo");
        let document_uri = Url::from_file_path(&document_path).expect("file uri");
        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: document_uri.clone(),
                    language_id: "modelica".to_string(),
                    version: 1,
                    text: "model First\nend First;\n".to_string(),
                },
            })
            .await;
        assert!(
            server.namespace_prewarm_state.read().await.is_none(),
            "workspace did_open should not reschedule durable-library namespace prewarm"
        );

        server
            .did_change(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: document_uri.clone(),
                    version: 2,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: "model Second\nend Second;\n".to_string(),
                }],
            })
            .await;
        assert!(
            server.namespace_prewarm_state.read().await.is_none(),
            "workspace did_change should not reschedule durable-library namespace prewarm"
        );

        server
            .did_close(DidCloseTextDocumentParams {
                text_document: TextDocumentIdentifier { uri: document_uri },
            })
            .await;
        assert!(
            server.namespace_prewarm_state.read().await.is_none(),
            "workspace did_close should not reschedule durable-library namespace prewarm"
        );
    });
}

#[test]
fn workspace_did_change_does_not_wait_for_pending_namespace_prewarm() {
    run_async_test(async {
        let workspace_root = new_temp_dir("workspace-no-prewarm-wait");
        let document_path = workspace_root.join("active.mo");
        let document_uri = Url::from_file_path(&document_path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();

        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: document_uri.clone(),
                    language_id: "modelica".to_string(),
                    version: 1,
                    text: "model First\nend First;\n".to_string(),
                },
            })
            .await;

        let pending = NamespacePrewarmState {
            session_revision: server.current_analysis_revision().await,
            done: std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)),
            finished: std::sync::Arc::new(tokio::sync::Notify::new()),
        };
        *server.namespace_prewarm_state.write().await = Some(pending);

        let changed = tokio::time::timeout(
            std::time::Duration::from_millis(50),
            server.did_change(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: document_uri,
                    version: 2,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: "model Second\nend Second;\n".to_string(),
                }],
            }),
        )
        .await;
        assert!(
            changed.is_ok(),
            "workspace did_change should not wait for unrelated namespace prewarm"
        );
    });
}

#[test]
fn opening_loaded_library_document_does_not_reschedule_namespace_prewarm() {
    run_async_test(async {
        let workspace_root = new_temp_dir("library-open-no-namespace-prewarm");
        let library_root = write_test_library(&workspace_root, "InitLib");
        let workspace_uri = Url::from_directory_path(&workspace_root).expect("workspace uri");
        let service = new_test_service();
        let server = service.inner();

        server
            .initialize(InitializeParams {
                root_uri: Some(workspace_uri),
                initialization_options: Some(serde_json::json!({
                    "modelicaPath": [library_root.to_string_lossy().to_string()]
                })),
                ..InitializeParams::default()
            })
            .await
            .expect("initialize should succeed");
        server.wait_for_namespace_prewarm_if_pending().await;
        assert!(
            server.namespace_prewarm_state.read().await.is_none(),
            "startup durable-root prewarm should finish before opening the library document"
        );

        let library_document_path = library_root.join("package.mo");
        let library_document_uri = Url::from_file_path(&library_document_path).expect("file uri");
        let library_source =
            std::fs::read_to_string(&library_document_path).expect("read cached library source");
        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: library_document_uri,
                    language_id: "modelica".to_string(),
                    version: 1,
                    text: library_source,
                },
            })
            .await;
        assert!(
            server.namespace_prewarm_state.read().await.is_none(),
            "opening a loaded library document with current text should not reschedule namespace prewarm"
        );
    });
}
