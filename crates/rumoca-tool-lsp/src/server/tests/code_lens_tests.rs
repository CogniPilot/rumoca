//! Code-lens surface: the lens request stays parse-only until it is resolved,
//! and resolution reports the owning compile phase (or the pre-phase resolve
//! failure) without inventing titles or swallowing staleness.

use super::*;
use rumoca_compile::scenario::SimulationModelOverride;

#[test]
fn code_lens_request_stays_parse_only_until_resolved() {
    run_async_test(async {
        let temp = new_temp_dir("code-lens-parse-only");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_key = session_document_uri_key(&active_uri);

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &active_key,
                r#"
                    model Root
                      Real x(start=0);
                    equation
                      der(x) = 1;
                    end Root;
                    "#,
            );
        }

        let lenses = server
            .code_lens(CodeLensParams {
                text_document: TextDocumentIdentifier {
                    uri: active_uri.clone(),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("code lens request should succeed")
            .expect("code lens should return the model lens");
        assert_eq!(lenses.len(), 1, "expected one model code lens");
        assert!(
            lenses[0].command.is_none(),
            "initial code lens response should be unresolved"
        );
        assert!(
            lenses[0].data.is_some(),
            "initial code lens response should carry resolve data"
        );
        assert!(
            !server.session.read().await.has_resolved_cached(),
            "code lens list request should stay parse-only"
        );

        let resolved = server
            .code_lens_resolve(lenses[0].clone())
            .await
            .expect("code lens resolve should succeed");
        let title = resolved
            .command
            .as_ref()
            .map(|command| command.title.clone())
            .expect("resolved code lens should supply a title");
        assert!(
            title.starts_with("Balanced"),
            "resolved code lens should reflect the strict compile result: {title}"
        );
        assert!(
            server.session.read().await.has_resolved_cached(),
            "code lens resolve should build resolved state on a cold request"
        );
        assert!(
            !server.session.read().await.has_standard_resolved_cached(),
            "code lens resolve should avoid the standard resolved session"
        );
    });
}

#[test]
fn code_lens_resolve_compiles_within_qualified_model_name() {
    run_async_test(async {
        let temp = new_temp_dir("code-lens-within-qualified-model");
        let active_path = temp.join("Ball.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_key = session_document_uri_key(&active_uri);

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &active_key,
                r#"
                    within Examples;
                    model Ball
                      Real x(start=0);
                    equation
                      der(x) = 1;
                    end Ball;
                    "#,
            );
        }

        let lenses = server
            .code_lens(CodeLensParams {
                text_document: TextDocumentIdentifier {
                    uri: active_uri.clone(),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("code lens request should succeed")
            .expect("code lens should return the model lens");
        assert_eq!(lenses.len(), 1, "expected one model code lens");
        assert_eq!(
            lenses[0]
                .data
                .as_ref()
                .and_then(|data| data.get("modelName"))
                .and_then(serde_json::Value::as_str),
            Some("Examples.Ball"),
            "CodeLens compile requests must use the stored definition's within-qualified model name",
        );

        let resolved = server
            .code_lens_resolve(lenses[0].clone())
            .await
            .expect("code lens resolve should succeed");
        let title = resolved
            .command
            .as_ref()
            .map(|command| command.title.clone())
            .expect("resolved code lens should supply a title");
        assert!(
            title.starts_with("Balanced"),
            "within-qualified code lens should compile the same model simulation can run: {title}"
        );
    });
}

#[test]
fn code_lens_resolve_skips_when_request_becomes_stale() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        let temp = new_temp_dir("code-lens-stale-resolve");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_key = session_document_uri_key(&active_uri);

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &active_key,
                r#"
                    model Root
                      Real x(start=0);
                    equation
                      der(x) = 1;
                    end Root;
                    "#,
            );
        }

        let lenses = server
            .code_lens(CodeLensParams {
                text_document: TextDocumentIdentifier {
                    uri: active_uri.clone(),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("code lens request should succeed")
            .expect("code lens should return the model lens");
        let lens = lenses
            .into_iter()
            .next()
            .expect("code lens should return one model code lens");

        reset_session_cache_stats();
        let before = session_cache_stats();
        let strict_guard = server.work_lanes.strict.lock().await;
        let resolve_task = tokio::spawn({
            let server = server.clone();
            let lens = lens.clone();
            async move { server.code_lens_resolve(lens).await }
        });
        tokio::task::yield_now().await;
        server
            .did_change(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: active_uri.clone(),
                    version: 2,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: r#"
                    model Root
                      Real x(start=1);
                    equation
                      der(x) = 1;
                    end Root;
                    "#
                    .to_string(),
                }],
            })
            .await;
        drop(strict_guard);

        let resolved = resolve_task
            .await
            .expect("code lens resolve task should finish")
            .expect("code lens resolve should succeed");
        assert_eq!(
            resolved.command, lens.command,
            "stale code lens resolve should keep response unchanged"
        );
        assert_eq!(
            resolved.data, lens.data,
            "stale code lens resolve should keep existing data"
        );

        let delta = session_cache_stats().delta_since(before);
        assert_eq!(
            delta.strict_resolved_builds, 0,
            "stale code lens resolve should skip strict compile"
        );
    });
}

/// A model-phase compile failure must still leave the save-diagnostics cache
/// warm, so the Problems panel can publish the semantic errors that the code
/// lens just discovered.
///
/// The model resolves cleanly and fails in Typecheck on the unknown modifier
/// `startt` — a real model phase. That is what makes the cache assertion
/// meaningful: the save-diagnostics pass builds the interface and body artifacts
/// for `Test2` on the way to that error. A failure *before* any model phase
/// leaves nothing to cache; that path is covered separately by
/// `code_lens_resolve_reports_pre_phase_resolve_failure`.
#[test]
fn code_lens_resolve_failure_warms_save_diagnostics_for_problems() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        let temp = new_temp_dir("code-lens-save-diagnostics-on-failure");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_key = session_document_uri_key(&active_uri);
        let active_source =
            "model Test2\n  Real x(startt = 0.0);\nequation\n  der(x) = -x;\nend Test2;\n";

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(&active_key, active_source);
        }
        assert!(
            !server
                .session
                .read()
                .await
                .has_semantic_diagnostics_cached("Test2"),
            "failure test should start with a cold save-diagnostics cache"
        );

        let lenses = server
            .code_lens(CodeLensParams {
                text_document: TextDocumentIdentifier { uri: active_uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("code lens request should succeed")
            .expect("code lens should return the model lens");
        let resolved = server
            .code_lens_resolve(
                lenses
                    .into_iter()
                    .next()
                    .expect("expected one unresolved code lens"),
            )
            .await
            .expect("code lens resolve should succeed");
        let title = resolved
            .command
            .as_ref()
            .map(|command| command.title.clone())
            .expect("resolved code lens should supply a title");
        assert!(
            title.starts_with("Compile failed"),
            "expected strict compile failure title, got: {title}"
        );
        assert!(
            server
                .session
                .read()
                .await
                .has_semantic_diagnostics_cached("Test2"),
            "code lens failure should warm save diagnostics so Problems can publish semantic errors"
        );
        assert!(
            !server.session.read().await.has_standard_resolved_cached(),
            "warming save diagnostics from code lens failure should stay off the standard resolved cache"
        );
    });
}

/// `pose.z` is an unknown member of the operator record `SE2`, which Resolve
/// owns as `ER002` (SPEC_0008: unresolved references are hard errors in
/// Resolve). Such a compile never reaches a model phase, so
/// `StrictCompileFailure::phase` is `None` and the lens must render the
/// pre-phase title `Compile error (...)` rather than inventing a phase name.
/// The title still carries the diagnostic text so the lens is actionable.
#[test]
fn code_lens_resolve_reports_pre_phase_resolve_failure() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        let temp = new_temp_dir("code-lens-pre-phase-resolve-failure");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_key = session_document_uri_key(&active_uri);
        let active_source = "operator record SE2\n  Real x;\n  Real y;\n  Real theta;\nend SE2;\n\nmodel Test2\n  SE2 pose;\nequation\n  der(pose.x) = 1;\n  der(pose.y) = 0;\n  der(pose.z) = 2;\nend Test2;\n";

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(&active_key, active_source);
        }

        let lenses = server
            .code_lens(CodeLensParams {
                text_document: TextDocumentIdentifier { uri: active_uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("code lens request should succeed")
            .expect("code lens should return the model lens");
        let resolved = server
            .code_lens_resolve(
                lenses
                    .into_iter()
                    .next()
                    .expect("expected one unresolved code lens"),
            )
            .await
            .expect("code lens resolve should succeed");
        let title = resolved
            .command
            .as_ref()
            .map(|command| command.title.clone())
            .expect("resolved code lens should supply a title");
        assert_eq!(
            title, "Compile error (unresolved component reference: 'pose.z')",
            "a pre-phase resolve failure must report the resolve diagnostic without a phase label"
        );
    });
}

#[test]
fn code_lens_defers_when_required_source_roots_are_unloaded() {
    run_async_test(async {
        let temp = new_temp_dir("code-lens-source-root-defer");
        let source_root_path = write_test_source_root(&temp, "Lib");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_source = "model Active\n  Lib.A a;\nend Active;\n";

        let service = new_test_service();
        let server = service.inner();
        *server.source_root_paths.write().await =
            vec![source_root_path.to_string_lossy().to_string()];
        {
            let mut session = server.session.write().await;
            session.update_document(&active_path.to_string_lossy(), active_source);
        }

        let response = server
            .code_lens(CodeLensParams {
                text_document: TextDocumentIdentifier { uri: active_uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("code lens request should succeed");

        assert!(
            response.is_none(),
            "code lens should defer until required source roots are loaded"
        );
        assert!(
            server
                .session
                .read()
                .await
                .loaded_source_root_path_keys()
                .is_empty(),
            "code lens should not synchronously load source roots"
        );
    });
}

#[test]
fn code_lens_resolve_uses_model_simulation_source_root_overrides() {
    run_async_test(async {
        let temp = new_temp_dir("code-lens-model-source-root-overrides");
        let source_root_path = write_test_source_root(&temp, "Lib");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_key = session_document_uri_key(&active_uri);
        let source_root_key = canonical_path_key(source_root_path.to_string_lossy().as_ref());

        write_model_simulation_preset(
            &temp,
            "Active",
            SimulationModelOverride {
                source_root_overrides: vec![source_root_path.to_string_lossy().to_string()],
                ..Default::default()
            },
        )
        .expect("write model simulation preset");

        let service = new_test_service();
        let server = service.inner();
        *server.workspace_root.write().await = Some(temp.clone());
        {
            let mut session = server.session.write().await;
            session.update_document(&active_key, "model Active\n  Lib.A a;\nend Active;\n");
        }

        let lenses = server
            .code_lens(CodeLensParams {
                text_document: TextDocumentIdentifier {
                    uri: active_uri.clone(),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("code lens request should succeed")
            .expect("code lens should return the model lens");

        let resolved = server
            .code_lens_resolve(
                lenses
                    .into_iter()
                    .next()
                    .expect("expected one unresolved code lens"),
            )
            .await
            .expect("code lens resolve should succeed");
        let title = resolved
            .command
            .as_ref()
            .map(|command| command.title.clone())
            .expect("resolved code lens should supply a title");

        assert!(
            title.starts_with("Balanced"),
            "code lens resolve should compile with model-specific source roots: {title}"
        );
        assert!(
            server
                .session
                .read()
                .await
                .is_source_root_path_loaded(&source_root_key),
            "code lens resolve should load source roots from the model simulation preset"
        );
    });
}

#[test]
fn code_lens_ignores_unrelated_source_root_resolve_errors() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        reset_session_cache_stats();
        let temp = new_temp_dir("code-lens-strict-closure");
        let active_path = temp.join("root.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_key = session_document_uri_key(&active_uri);
        let dep_key = temp.join("good_dep.mo").to_string_lossy().to_string();
        let broken_key = temp.join("broken.mo").to_string_lossy().to_string();
        let lib_key = temp.join("lib.mo").to_string_lossy().to_string();

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session
                .add_document(
                    &dep_key,
                    r#"
                    within Lib;
                    model GoodDep
                      Real x(start=0);
                    equation
                      der(x) = 1;
                    end GoodDep;
                    "#,
                )
                .expect("good dependency should parse");
            session
                .add_document(
                    &broken_key,
                    r#"
                    connector Bus
                    end Bus;

                    block BusTranscription
                      Bus stackBus;
                    end BusTranscription;
                    "#,
                )
                .expect("broken sibling should parse");
            session
                .add_document(&lib_key, "package Lib\nend Lib;\n")
                .expect("source-root package should parse");
            session
                .add_document(
                    &active_key,
                    r#"
                    model Root
                      Lib.GoodDep dep;
                    end Root;
                    "#,
                )
                .expect("root should parse");
        }

        let response = server
            .code_lens(CodeLensParams {
                text_document: TextDocumentIdentifier { uri: active_uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("code lens request should succeed")
            .expect("root model should produce a code lens");
        assert!(
            response.iter().all(|lens| lens.command.is_none()),
            "code lens request should return unresolved items"
        );
        let before_resolve = session_cache_stats();
        let resolved = server
            .code_lens_resolve(
                response
                    .into_iter()
                    .next()
                    .expect("root model should produce one unresolved code lens"),
            )
            .await
            .expect("code lens resolve should succeed");
        let resolve_delta = session_cache_stats().delta_since(before_resolve);

        let titles = vec![
            resolved
                .command
                .map(|command| command.title)
                .expect("resolved code lens should supply a title"),
        ];
        assert!(
            titles.iter().any(|title| title.starts_with("Balanced")),
            "strict code lens should stay focused on the requested model: {titles:?}"
        );
        assert!(
            titles.iter().all(|title| !title.contains("stackBus")),
            "unrelated source-root resolve errors must not leak into code lens titles: {titles:?}"
        );
        assert!(
            resolve_delta.strict_resolved_builds >= 1,
            "code lens resolve should build strict resolved state when needed"
        );
    });
}
