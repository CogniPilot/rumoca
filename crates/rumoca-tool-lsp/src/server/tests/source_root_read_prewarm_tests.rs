use super::*;

#[test]
fn workspace_document_mutations_do_not_reschedule_source_root_read_prewarm() {
    run_async_test(async {
        let workspace_root = new_temp_dir("workspace-no-namespace-prewarm");
        let source_root_dir = write_test_source_root(&workspace_root, "InitLib");
        let workspace_uri = Url::from_directory_path(&workspace_root).expect("workspace uri");
        let service = new_test_service();
        let server = service.inner();

        server
            .initialize(InitializeParams {
                root_uri: Some(workspace_uri),
                initialization_options: Some(serde_json::json!({
                    "sourceRootPaths": [source_root_dir.to_string_lossy().to_string()]
                })),
                ..InitializeParams::default()
            })
            .await
            .expect("initialize should succeed");
        server.wait_for_source_root_read_prewarm_if_pending().await;
        let current_revision = server.current_analysis_revision().await;
        assert!(
            !server
                .session
                .read()
                .await
                .source_root_read_prewarm_is_pending(current_revision),
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
        let current_revision = server.current_analysis_revision().await;
        assert!(
            !server
                .session
                .read()
                .await
                .source_root_read_prewarm_is_pending(current_revision),
            "workspace did_open should not reschedule durable source-root read prewarm"
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
        let current_revision = server.current_analysis_revision().await;
        assert!(
            !server
                .session
                .read()
                .await
                .source_root_read_prewarm_is_pending(current_revision),
            "workspace did_change should not reschedule durable source-root read prewarm"
        );

        server
            .did_close(DidCloseTextDocumentParams {
                text_document: TextDocumentIdentifier { uri: document_uri },
            })
            .await;
        let current_revision = server.current_analysis_revision().await;
        assert!(
            !server
                .session
                .read()
                .await
                .source_root_read_prewarm_is_pending(current_revision),
            "workspace did_close should not reschedule durable source-root read prewarm"
        );
    });
}

#[test]
fn workspace_did_change_does_not_wait_for_pending_source_root_read_prewarm() {
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

        let session_revision = server.current_analysis_revision().await;
        server
            .session
            .write()
            .await
            .begin_source_root_read_prewarm(session_revision);

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
            "workspace did_change should not wait for unrelated source-root read prewarm"
        );
    });
}

#[test]
fn opening_loaded_source_root_document_does_not_leave_source_root_read_prewarm_pending() {
    run_async_test(async {
        let workspace_root = new_temp_dir("source-root-open-no-namespace-prewarm");
        let source_root_dir = write_test_source_root(&workspace_root, "InitLib");
        let workspace_uri = Url::from_directory_path(&workspace_root).expect("workspace uri");
        let service = new_test_service();
        let server = service.inner();

        server
            .initialize(InitializeParams {
                root_uri: Some(workspace_uri),
                initialization_options: Some(serde_json::json!({
                    "sourceRootPaths": [source_root_dir.to_string_lossy().to_string()]
                })),
                ..InitializeParams::default()
            })
            .await
            .expect("initialize should succeed");
        server.wait_for_source_root_read_prewarm_if_pending().await;
        let current_revision = server.current_analysis_revision().await;
        assert!(
            !server
                .session
                .read()
                .await
                .source_root_read_prewarm_is_pending(current_revision),
            "startup durable-root prewarm should finish before opening the source-root document"
        );

        let source_root_document_path = source_root_dir.join("package.mo");
        let source_root_document_uri =
            Url::from_file_path(&source_root_document_path).expect("file uri");
        let source_root_source = std::fs::read_to_string(&source_root_document_path)
            .expect("read cached source-root source");
        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: source_root_document_uri,
                    language_id: "modelica".to_string(),
                    version: 1,
                    text: source_root_source,
                },
            })
            .await;
        let current_revision = server.current_analysis_revision().await;
        assert!(
            !server
                .session
                .read()
                .await
                .source_root_read_prewarm_is_pending(current_revision),
            "opening a loaded source-root document with current text should finish any rescheduled source-root read prewarm before did_open returns"
        );
    });
}
