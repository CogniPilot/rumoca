//! Hover's flattened-DAE preview must be cached and must not run under the
//! session write lock.
//!
//! The previous implementation called
//! `compile_model_strict_reachable_uncached_with_recovery` while holding
//! `session.write()`, so every mouse move over a model name blocked completions,
//! diagnostics and edits for a whole strict compile — and repeated the compile
//! identically on the next hover.

use super::*;

const PREVIEW_MODEL: &str =
    "model Helper\n  Real x(start=0);\nequation\n  der(x) = 1;\nend Helper;\n";

async fn seed_preview_document(server: &ModelicaLanguageServer, temp: &Path) -> String {
    seed_preview_source(server, temp, PREVIEW_MODEL).await
}

async fn seed_preview_source(server: &ModelicaLanguageServer, temp: &Path, source: &str) -> String {
    let path = temp.join("preview.mo");
    let uri = Url::from_file_path(&path).expect("file uri");
    let key = session_document_uri_key(&uri);
    let mut session = server.session.write().await;
    session.update_document(&key, source);
    key
}

#[test]
fn hover_flat_preview_is_cached_across_repeated_hovers() {
    let temp = new_temp_dir("hover-preview-cache");
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();
        let key = seed_preview_document(server, &temp).await;

        let token = server.begin_analysis_request().await;
        let first = server
            .hover_flat_preview("Helper", &key, token)
            .await
            .expect("first hover should render a preview");
        assert!(first.contains("Flattened DAE Preview"));

        let second = server
            .hover_flat_preview("Helper", &key, token)
            .await
            .expect("second hover should render a preview");
        assert!(
            Arc::ptr_eq(&first, &second),
            "the second hover must reuse the memoized preview, not recompile"
        );
    });
    let _ = std::fs::remove_dir_all(&temp);
}

#[test]
fn hover_flat_preview_retains_ordered_when_branch_values_and_sources() {
    let temp = new_temp_dir("hover-preview-b1c");
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();
        let key = seed_preview_source(
            server,
            &temp,
            r#"
model PreviewWhen
  Real x(start = 0);
  discrete Integer selected(start = 0);
equation
  der(x) = 1;
  when x >= 0.5 then
    selected = 1;
  elsewhen x >= 0.75 then
    selected = 2;
  end when;
end PreviewWhen;
"#,
        )
        .await;

        let token = server.begin_analysis_request().await;
        let preview = server
            .hover_flat_preview("PreviewWhen", &key, token)
            .await
            .expect("checked when model should render a preview");
        for expected in [
            "[selected] := 2 ordered branch(es)",
            "trigger=`x >= 0.5`",
            "trigger=`x >= 0.75`",
            "selected := `1`",
            "selected := `2`",
        ] {
            assert!(
                preview.contains(expected),
                "preview should retain {expected:?}, got:\n{preview}"
            );
        }
    });
    let _ = std::fs::remove_dir_all(&temp);
}

#[test]
fn hover_flat_preview_memoizes_models_that_do_not_compile() {
    let temp = new_temp_dir("hover-preview-negative");
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();
        let key = seed_preview_document(server, &temp).await;

        let token = server.begin_analysis_request().await;
        assert!(
            server
                .hover_flat_preview("NotAModel", &key, token)
                .await
                .is_none(),
            "an unknown model has no preview"
        );
        assert_eq!(
            server.hover_preview_cache.read().await.len(),
            1,
            "the failure must be memoized so a broken model is not recompiled on \
             every mouse move"
        );
    });
    let _ = std::fs::remove_dir_all(&temp);
}

#[test]
fn hover_flat_preview_does_not_need_the_session_write_lock() {
    let temp = new_temp_dir("hover-preview-lock");
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();
        let key = seed_preview_document(server, &temp).await;

        // Holding a *read* guard for the whole request deadlocks anything that
        // needs `session.write()`, which is exactly what the old hover did.
        let read_guard = server.session.read().await;
        let token = server.begin_analysis_request().await;
        let preview = tokio::time::timeout(
            std::time::Duration::from_secs(30),
            server.hover_flat_preview("Helper", &key, token),
        )
        .await
        .expect("hover preview must not wait on the session write lock");
        drop(read_guard);

        assert!(
            preview.is_some_and(|text| text.contains("Flattened DAE Preview")),
            "hover preview should still render while a session reader is active"
        );
    });
    let _ = std::fs::remove_dir_all(&temp);
}

#[test]
fn hover_preview_cache_is_bounded() {
    let temp = new_temp_dir("hover-preview-bound");
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();
        let key = seed_preview_document(server, &temp).await;

        let token = server.begin_analysis_request().await;
        for index in 0..HoverPreviewCache::CAPACITY + 4 {
            let _ = server
                .hover_flat_preview(&format!("Missing{index}"), &key, token)
                .await;
        }
        assert_eq!(
            server.hover_preview_cache.read().await.len(),
            HoverPreviewCache::CAPACITY,
            "hover is unbounded user input, so the memo must stay bounded"
        );
    });
    let _ = std::fs::remove_dir_all(&temp);
}
