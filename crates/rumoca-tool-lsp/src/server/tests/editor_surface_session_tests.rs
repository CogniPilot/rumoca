use super::*;

#[test]
fn did_open_source_root_document_moves_live_text_into_session() {
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();
        let source_root_dir = std::env::temp_dir().join("rumoca-lsp-test-open");
        let source_root_path = source_root_dir
            .join("Modelica")
            .join("Blocks")
            .join("Continuous.mo");
        let source_root_uri = canonical_path_key(source_root_path.to_string_lossy().as_ref());
        let text = "within Modelica.Blocks;\npackage Continuous\nend Continuous;\n";

        {
            let mut session = server.session.write().await;
            session.replace_parsed_source_set(
                "source-root::Modelica",
                SourceRootKind::External,
                vec![(source_root_uri.clone(), ast::StoredDefinition::default())],
                None,
            );
        }
        let uri = Url::from_file_path(&source_root_path).expect("file uri");
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
            .get_document(&source_root_uri)
            .cloned()
            .expect("opened source-root document must be owned by the session");
        assert_eq!(
            &*session_doc.content, text,
            "session should track the live source-root document text directly"
        );
    });
}

#[test]
fn did_change_ignores_stale_document_versions() {
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();
        let path = new_temp_dir("stale-version").join("active.mo");
        let uri = Url::from_file_path(&path).expect("file uri");
        let uri_path = session_document_uri_key(&uri);
        let fresh = "model Fresh\nend Fresh;\n";
        let stale = "model Stale\nend Stale;\n";

        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "modelica".to_string(),
                    version: 2,
                    text: fresh.to_string(),
                },
            })
            .await;

        server
            .did_change(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier { uri, version: 1 },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: stale.to_string(),
                }],
            })
            .await;

        let doc = server
            .session
            .read()
            .await
            .get_document(&uri_path)
            .cloned()
            .expect("document should remain tracked");
        assert_eq!(
            &*doc.content, fresh,
            "older document versions must not overwrite newer session state"
        );
    });
}

#[test]
fn did_close_source_root_document_restores_cached_session_document() {
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();
        let source_root_dir = std::env::temp_dir().join("rumoca-lsp-test-close");
        let source_root_path = source_root_dir
            .join("Modelica")
            .join("Blocks")
            .join("Continuous.mo");
        let source_root_uri = canonical_path_key(source_root_path.to_string_lossy().as_ref());

        {
            let mut session = server.session.write().await;
            session.replace_parsed_source_set(
                "source-root::Modelica",
                SourceRootKind::External,
                vec![(source_root_uri.clone(), ast::StoredDefinition::default())],
                None,
            );
        }
        let uri = Url::from_file_path(&source_root_path).expect("file uri");
        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "modelica".to_string(),
                    version: 1,
                    text: "within Modelica.Blocks;".to_string(),
                },
            })
            .await;
        server
            .did_close(DidCloseTextDocumentParams {
                text_document: TextDocumentIdentifier { uri },
            })
            .await;

        let session_doc = server
            .session
            .read()
            .await
            .get_document(&source_root_uri)
            .cloned();
        assert!(
            session_doc.is_some(),
            "closing a live source-root document should restore the cached source-root document"
        );
        assert!(
            session_doc.is_some_and(|doc| doc.content.is_empty()),
            "restored source-root document should return to source-root ownership"
        );
        let current_revision = server.current_analysis_revision().await;
        assert!(
            !server
                .session
                .read()
                .await
                .source_root_read_prewarm_is_pending(current_revision),
            "closing a live source-root document should not enqueue background source-root read prewarm"
        );
    });
}
