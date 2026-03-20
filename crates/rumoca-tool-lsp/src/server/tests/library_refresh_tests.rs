use super::*;

#[test]
fn live_library_edits_mark_and_clear_session_owned_refresh_state() {
    run_async_test(async {
        let temp = new_temp_dir("dirty-library-refresh");
        let library_path = write_test_library(&temp, "Lib");
        let package_path = library_path.join("package.mo");
        let package_uri = Url::from_file_path(&package_path).expect("file uri");
        let package_key = session_document_uri_key(&package_uri);
        let library_key = canonical_path_key(library_path.to_string_lossy().as_ref());
        let source_set_id = library_source_set_id(library_path.to_string_lossy().as_ref());
        let initial_text = std::fs::read_to_string(&package_path).expect("read package.mo");

        let service = new_test_service();
        let server = service.inner();
        server
            .load_library_source_set_if_current(
                library_path.to_string_lossy().as_ref(),
                &library_key,
                &source_set_id,
                Some(package_key.as_str()),
                server.current_library_state_epoch(),
                LibraryIndexingReason::CompletionImports,
            )
            .await
            .expect("library load should succeed")
            .expect("library should be applied");

        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: package_uri.clone(),
                    language_id: "modelica".to_string(),
                    version: 1,
                    text: initial_text,
                },
            })
            .await;
        assert!(
            server
                .session
                .read()
                .await
                .dirty_library_source_root_keys()
                .is_empty(),
            "opening a library document should not mark its source root dirty"
        );

        server
            .did_change(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: package_uri,
                    version: 2,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: "package Lib\n  model A\n    Real x;\n    Real y;\n  equation\n    der(x) = 1;\n  end A;\nend Lib;\n".to_string(),
                }],
            })
            .await;
        assert_eq!(
            server.session.read().await.dirty_library_source_root_keys(),
            vec![source_set_id.clone()],
            "session should own the pending library refresh state after live edits"
        );

        server
            .rebuild_dirty_libraries_before_compile(&package_key)
            .await
            .expect("dirty library rebuild should succeed");
        assert!(
            server
                .session
                .read()
                .await
                .dirty_library_source_root_keys()
                .is_empty(),
            "successful source-root rebuild should clear the session-owned refresh state"
        );
    });
}
