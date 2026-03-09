use super::*;
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
