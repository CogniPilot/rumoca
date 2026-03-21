use super::*;

#[test]
fn live_source_root_edits_mark_and_clear_session_owned_refresh_state() {
    run_async_test(async {
        let temp = new_temp_dir("dirty-source-root-refresh");
        let source_root_path = write_test_source_root(&temp, "Lib");
        let package_path = source_root_path.join("package.mo");
        let package_uri = Url::from_file_path(&package_path).expect("file uri");
        let package_key = session_document_uri_key(&package_uri);
        let source_root_key = canonical_path_key(source_root_path.to_string_lossy().as_ref());
        let source_set_id = source_root_source_set_key(source_root_path.to_string_lossy().as_ref());
        let initial_text = std::fs::read_to_string(&package_path).expect("read package.mo");

        let service = new_test_service();
        let server = service.inner();
        let source_root_epoch = server.session.read().await.source_root_state_epoch();
        server
            .load_source_root_if_current(
                source_root_path.to_string_lossy().as_ref(),
                &source_root_key,
                &source_set_id,
                Some(package_key.as_str()),
                source_root_epoch,
                SourceRootIndexingReason::CompletionImports,
            )
            .await
            .expect("source-root load should succeed")
            .expect("source root should be applied");

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
                .dirty_non_workspace_source_root_keys()
                .is_empty(),
            "opening a source-root document should not mark its source root dirty"
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
            server
                .session
                .read()
                .await
                .dirty_non_workspace_source_root_keys(),
            vec![source_set_id.clone()],
            "session should own the pending source-root refresh state after live edits"
        );

        server
            .rebuild_dirty_source_roots_before_compile(&package_key)
            .await
            .expect("dirty source-root rebuild should succeed");
        assert!(
            server
                .session
                .read()
                .await
                .dirty_non_workspace_source_root_keys()
                .is_empty(),
            "successful source-root rebuild should clear the session-owned refresh state"
        );
    });
}

#[test]
fn live_workspace_source_root_edits_mark_and_clear_session_owned_refresh_state() {
    run_async_test(async {
        let temp = new_temp_dir("dirty-workspace-root-refresh");
        let workspace_root = temp.join("WorkspacePkg");
        std::fs::create_dir_all(&workspace_root).expect("mkdir workspace root");
        let package_path = workspace_root.join("package.mo");
        std::fs::write(
            &package_path,
            "within ;\npackage WorkspacePkg\nend WorkspacePkg;\n",
        )
        .expect("write package.mo");
        let package_uri = Url::from_file_path(&package_path).expect("file uri");
        let package_key = session_document_uri_key(&package_uri);
        let source_set_id = format!(
            "workspace::{}",
            canonical_path_key(workspace_root.to_string_lossy().as_ref())
        );
        let initial_text = std::fs::read_to_string(&package_path).expect("read package.mo");

        let parsed = parse_source_root_with_cache(&workspace_root).expect("parse workspace root");
        let service = new_test_service();
        let server = service.inner();
        server.session.write().await.replace_parsed_source_set(
            &source_set_id,
            SourceRootKind::Workspace,
            parsed.documents,
            None,
        );

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
                .dirty_source_root_keys()
                .is_empty(),
            "opening a workspace source-root document should not mark its source root dirty"
        );
        assert!(
            server
                .session
                .read()
                .await
                .dirty_non_workspace_source_root_keys()
                .is_empty(),
            "workspace roots should stay out of the compatibility non-workspace refresh view"
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
                    text: "within ;\npackage WorkspacePkg\n  model A\n    Real x;\n  equation\n    der(x) = 1;\n  end A;\nend WorkspacePkg;\n".to_string(),
                }],
            })
            .await;
        assert_eq!(
            server.session.read().await.dirty_source_root_keys(),
            vec![source_set_id.clone()],
            "session should own the pending workspace source-root refresh state after live edits"
        );
        assert!(
            server
                .session
                .read()
                .await
                .dirty_non_workspace_source_root_keys()
                .is_empty(),
            "workspace roots should stay out of the compatibility non-workspace refresh view"
        );

        server
            .rebuild_dirty_source_roots_before_compile(&package_key)
            .await
            .expect("dirty workspace source-root rebuild should succeed");
        assert!(
            server
                .session
                .read()
                .await
                .dirty_source_root_keys()
                .is_empty(),
            "successful workspace source-root rebuild should clear the session-owned refresh state"
        );
    });
}

#[test]
fn subtree_workspace_refresh_before_compile_does_not_reparse_missing_root() {
    run_async_test(async {
        let temp = new_temp_dir("subtree-workspace-refresh");
        let workspace_root = temp.join("WorkspacePkg");
        std::fs::create_dir_all(&workspace_root).expect("mkdir workspace root");
        let package_path = workspace_root.join("package.mo");
        let model_path = workspace_root.join("Test.mo");
        std::fs::write(
            &package_path,
            "within ;\npackage WorkspacePkg\nend WorkspacePkg;\n",
        )
        .expect("write package.mo");
        std::fs::write(&model_path, "within WorkspacePkg;\nmodel Test\nend Test;\n")
            .expect("write Test.mo");
        let model_uri = Url::from_file_path(&model_path).expect("file uri");
        let model_key = session_document_uri_key(&model_uri);
        let source_set_id = format!(
            "workspace::{}",
            canonical_path_key(workspace_root.to_string_lossy().as_ref())
        );
        let initial_text = std::fs::read_to_string(&model_path).expect("read Test.mo");

        let parsed = parse_source_root_with_cache(&workspace_root).expect("parse workspace root");
        let service = new_test_service();
        let server = service.inner();
        server.session.write().await.replace_parsed_source_set(
            &source_set_id,
            SourceRootKind::Workspace,
            parsed.documents,
            None,
        );

        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: model_uri.clone(),
                    language_id: "modelica".to_string(),
                    version: 1,
                    text: initial_text,
                },
            })
            .await;
        server
            .did_change(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: model_uri,
                    version: 2,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: "within WorkspacePkg;\nmodel Test\n  Real x;\n  Real y;\nend Test;\n"
                        .to_string(),
                }],
            })
            .await;

        std::fs::remove_dir_all(&workspace_root).expect("remove workspace root");

        server
            .rebuild_dirty_source_roots_before_compile(&model_key)
            .await
            .expect("subtree workspace refresh should not require reparsing the missing root");
        assert!(
            server
                .session
                .read()
                .await
                .dirty_source_root_keys()
                .is_empty(),
            "successful subtree refresh should clear the session-owned refresh state"
        );
    });
}
