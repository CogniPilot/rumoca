use super::*;

async fn wait_for_simulation_compile_cache_model(
    server: &ModelicaLanguageServer,
    model: &str,
) -> bool {
    tokio::time::timeout(std::time::Duration::from_secs(1), async {
        loop {
            let cache = server.simulation_compile_cache.read().await;
            let cache_debug = format!("{:?}", cache.keys().collect::<Vec<_>>());
            if cache_debug.contains(model) {
                return;
            }
            drop(cache);
            tokio::time::sleep(std::time::Duration::from_millis(10)).await;
        }
    })
    .await
    .is_ok()
}

async fn has_simulation_compile_cache_model(server: &ModelicaLanguageServer, model: &str) -> bool {
    let cache = server.simulation_compile_cache.read().await;
    format!("{:?}", cache.keys().collect::<Vec<_>>()).contains(model)
}

#[test]
fn compile_model_for_simulation_ignores_unrelated_local_parse_errors() {
    run_async_test(async {
        let temp = new_temp_dir("compile-sibling-parse-error");
        let focus = temp.join("Root.mo");
        let sibling = temp.join("Helper.mo");
        let broken = temp.join("Broken.mo");
        std::fs::write(&focus, "model Root\n  Helper h;\nend Root;\n").expect("write focus");
        std::fs::write(
            &sibling,
            "model Helper\n  Real x(start=0);\nequation\n  der(x) = 1;\nend Helper;\n",
        )
        .expect("write sibling");
        std::fs::write(&broken, "model Broken\n  Real x\nend Broken;\n").expect("write broken");

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }

        let compiled = server
            .compile_model_for_simulation("Root", &focus.to_string_lossy())
            .await
            .expect("compile should ignore unrelated local parse errors");
        assert_eq!(compiled.dae.states.len(), 1);
    });
}

#[test]
fn compile_model_for_simulation_repeated_runs_ignore_new_unrelated_local_parse_errors() {
    run_async_test(async {
        let temp = new_temp_dir("compile-repeated-sibling-parse-error");
        let focus = temp.join("Root.mo");
        let sibling = temp.join("Helper.mo");
        let broken = temp.join("Broken.mo");
        std::fs::write(&focus, "model Root\n  Helper h;\nend Root;\n").expect("write focus");
        std::fs::write(
            &sibling,
            "model Helper\n  Real x(start=0);\nequation\n  der(x) = 1;\nend Helper;\n",
        )
        .expect("write sibling");

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }

        let first = server
            .compile_model_for_simulation("Root", &focus.to_string_lossy())
            .await
            .expect("first focused compile should succeed");
        assert_eq!(first.dae.states.len(), 1);

        std::fs::write(&broken, "model Broken\n  Real x\nend Broken;\n").expect("write broken");

        let second = server
            .compile_model_for_simulation("Root", &focus.to_string_lossy())
            .await
            .expect("second focused compile should ignore unrelated local parse errors");
        assert_eq!(second.dae.states.len(), 1);
    });
}

#[test]
fn compile_model_for_simulation_reports_required_local_parse_errors() {
    run_async_test(async {
        let temp = new_temp_dir("compile-required-parse-error");
        let focus = temp.join("Root.mo");
        let sibling = temp.join("Helper.mo");
        std::fs::write(&focus, "model Root\n  Helper h;\nend Root;\n").expect("write focus");
        std::fs::write(&sibling, "model Helper\n  Real x\nend Helper;\n").expect("write sibling");

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }

        let err = server
            .compile_model_for_simulation("Root", &focus.to_string_lossy())
            .await
            .expect_err("required broken sibling must fail simulation compile");
        assert!(
            err.contains(&sibling.to_string_lossy().to_string()),
            "required parse error should mention the broken sibling file: {err}"
        );
        assert!(
            !err.contains("unresolved type reference"),
            "required parse error must not degrade into unresolved type errors: {err}"
        );
    });
}

#[test]
fn compile_model_for_simulation_reports_active_local_parse_errors() {
    run_async_test(async {
        let temp = new_temp_dir("compile-active-parse-error");
        let focus = temp.join("Broken.mo");
        std::fs::write(&focus, "model Broken\n  Real x\nend Broken;\n").expect("write focus");

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }

        let err = server
            .compile_model_for_simulation("Broken", &focus.to_string_lossy())
            .await
            .expect_err("active broken document must fail simulation compile");
        assert!(
            err.contains("unexpected"),
            "active parse error should come from structured parse diagnostics: {err}"
        );
        assert!(
            !err.contains("parse error in active document"),
            "active parse errors must not use the old string short-circuit: {err}"
        );
    });
}

#[test]
fn compile_model_for_simulation_reuses_warm_save_diagnostics_for_single_document_model() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        let temp = new_temp_dir("compile-warm-save-diagnostics");
        let focus = temp.join("Decay.mo");
        std::fs::write(
            &focus,
            "model Decay\n  Real x(start=1);\nequation\n  der(x) = -x;\nend Decay;\n",
        )
        .expect("write focus");

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
            let diagnostics = session.semantic_diagnostics_query(
                "Decay",
                rumoca_session::compile::SemanticDiagnosticsMode::Save,
            );
            assert!(
                diagnostics.diagnostics.is_empty(),
                "warm save diagnostics should succeed before simulation compile: {:?}",
                diagnostics.diagnostics
            );
        }

        let before = session_cache_stats();
        let compiled = server
            .compile_model_for_simulation("Decay", &focus.to_string_lossy())
            .await
            .expect("simulation compile should reuse warmed save artifacts");
        let delta = session_cache_stats().delta_since(before);

        assert_eq!(compiled.dae.states.len(), 1);
        assert_eq!(
            delta.strict_resolved_builds, 0,
            "simulation compile should not rebuild strict resolved state when save diagnostics already warmed it"
        );
        assert_eq!(
            delta.instantiated_model_builds, 0,
            "simulation compile should reuse the instantiated-model artifact from save diagnostics"
        );
        assert_eq!(
            delta.typed_model_builds, 0,
            "simulation compile should reuse the typed-model artifact from save diagnostics"
        );
        assert_eq!(
            delta.flat_model_builds, 0,
            "simulation compile should reuse the flat-model artifact from save diagnostics"
        );
        assert_eq!(
            delta.dae_model_builds, 0,
            "simulation compile should reuse the dae-model artifact from save diagnostics"
        );
    });
}

#[test]
fn isolated_simulation_session_skips_loaded_libraries_for_local_only_models() {
    run_async_test(async {
        let temp = new_temp_dir("simulation-isolate-local-only");
        let focus = temp.join("Decay.mo");
        std::fs::write(
            &focus,
            "model Decay\n  Real x(start=1);\nequation\n  der(x) = -x;\nend Decay;\n",
        )
        .expect("write focus");
        let library_root = write_test_library(&temp, "Lib");
        let library_doc =
            canonical_path_key(library_root.join("package.mo").to_string_lossy().as_ref());

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.replace_parsed_source_set(
                &library_source_set_id(library_root.to_string_lossy().as_ref()),
                SourceRootKind::Library,
                vec![(library_doc.clone(), ast::StoredDefinition::default())],
                None,
            );
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }
        server
            .loaded_libraries
            .write()
            .await
            .insert(canonical_path_key(library_root.to_string_lossy().as_ref()));

        let uris = server
            .isolated_simulation_document_uris_for_focus(&focus.to_string_lossy())
            .await
            .expect("isolated session uris");

        assert!(
            uris.iter()
                .any(|uri| uri == focus.to_string_lossy().as_ref()),
            "isolated simulation session should keep the focus document",
        );
        assert!(
            !uris.iter().any(|uri| uri == &library_doc),
            "local-only simulation compile should not clone unrelated loaded library documents",
        );
    });
}

#[test]
fn isolated_simulation_session_keeps_loaded_libraries_when_referenced() {
    run_async_test(async {
        let temp = new_temp_dir("simulation-isolate-with-library");
        let focus = temp.join("Decay.mo");
        std::fs::write(&focus, "model Decay\n  Lib.A a;\nend Decay;\n").expect("write focus");
        let library_root = write_test_library(&temp, "Lib");
        let library_doc =
            canonical_path_key(library_root.join("package.mo").to_string_lossy().as_ref());

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.replace_parsed_source_set(
                &library_source_set_id(library_root.to_string_lossy().as_ref()),
                SourceRootKind::Library,
                vec![(library_doc.clone(), ast::StoredDefinition::default())],
                None,
            );
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }
        server
            .loaded_libraries
            .write()
            .await
            .insert(canonical_path_key(library_root.to_string_lossy().as_ref()));

        let uris = server
            .isolated_simulation_document_uris_for_focus(&focus.to_string_lossy())
            .await
            .expect("isolated session uris");

        assert!(
            uris.iter().any(|uri| uri == &library_doc),
            "simulation compile should keep loaded library documents when the local compile unit references that root",
        );
    });
}

#[test]
fn prepare_simulation_models_populates_cache_for_each_requested_model() {
    run_async_test(async {
        let temp = new_temp_dir("prepare-simulation-models");
        let focus = temp.join("Bundle.mo");
        std::fs::write(
            &focus,
            "model First\n  Real x(start=0);\nequation\n  der(x) = 1;\nend First;\n\nmodel Second\n  Real y(start=1);\nequation\n  der(y) = -y;\nend Second;\n",
        )
        .expect("write focus");

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }

        let (prepared_models, failures, error) = server
            .run_prepare_simulation_models_request(
                Url::from_file_path(&focus).expect("focus file url"),
                vec!["Second".to_string(), "First".to_string()],
                SimulationRequestSettings {
                    solver: "auto".to_string(),
                    t_end: 10.0,
                    dt: None,
                    library_paths: Vec::new(),
                },
                None,
            )
            .await;

        assert!(
            error.is_none(),
            "prepare request should not fail: {error:?}"
        );
        assert!(failures.is_empty(), "all requested models should prepare");
        assert_eq!(
            prepared_models,
            vec!["Second".to_string(), "First".to_string()]
        );
        assert_eq!(
            server.simulation_compile_cache.read().await.len(),
            2,
            "prepare should populate a compile cache entry for each requested model"
        );
    });
}

#[test]
fn get_simulation_config_prewarms_open_document_model() {
    run_async_test(async {
        let temp = new_temp_dir("get-simulation-config-prewarm");
        let focus = temp.join("Decay.mo");
        std::fs::write(
            &focus,
            "model Decay\n  Real x(start=1);\nequation\n  der(x) = -x;\nend Decay;\n",
        )
        .expect("write focus");

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }

        let response = server
            .execute_get_simulation_config(Some(json!({
                "workspaceRoot": temp.to_string_lossy(),
                "model": "Decay",
                "fallback": {
                    "solver": "auto",
                    "tEnd": 1.0,
                    "dt": 0.1,
                    "outputDir": "",
                    "modelicaPath": [],
                }
            })))
            .await
            .expect("simulation config response");

        assert!(
            response.get("effective").is_some(),
            "getSimulationConfig should still return an effective config payload"
        );
        let warmed = tokio::time::timeout(std::time::Duration::from_secs(1), async {
            while server.simulation_compile_cache.read().await.len() != 1 {
                tokio::time::sleep(std::time::Duration::from_millis(10)).await;
            }
        })
        .await;
        assert!(
            warmed.is_ok(),
            "getSimulationConfig should prewarm the simulation compile cache for the open model",
        );
    });
}

#[test]
fn get_simulation_config_waits_for_matching_prewarm() {
    run_async_test(async {
        let temp = new_temp_dir("get-simulation-config-awaits-prewarm");
        let focus = temp.join("Decay.mo");
        std::fs::write(
            &focus,
            "model Decay\n  Real x(start=1);\nequation\n  der(x) = -x;\nend Decay;\n",
        )
        .expect("write focus");

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }

        server
            .execute_get_simulation_models(Some(json!({
                "uri": Url::from_file_path(&focus).expect("focus file url"),
                "defaultModel": "Decay",
            })))
            .await
            .expect("simulation models response");

        let response = server
            .execute_get_simulation_config(Some(json!({
                "workspaceRoot": temp.to_string_lossy(),
                "model": "Decay",
                "fallback": {
                    "solver": "auto",
                    "tEnd": 1.0,
                    "dt": 0.1,
                    "outputDir": "",
                    "modelicaPath": [],
                }
            })))
            .await
            .expect("simulation config response");

        assert!(
            response.get("effective").is_some(),
            "getSimulationConfig should still return an effective config payload"
        );
        assert!(
            has_simulation_compile_cache_model(server, "Decay").await,
            "getSimulationConfig should await the already-started model prewarm",
        );
    });
}

#[test]
fn get_simulation_models_prewarms_selected_model() {
    run_async_test(async {
        let temp = new_temp_dir("get-simulation-models-prewarm");
        let focus = temp.join("Decay.mo");
        std::fs::write(
            &focus,
            "model Decay\n  Real x(start=1);\nequation\n  der(x) = -x;\nend Decay;\n",
        )
        .expect("write focus");

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }

        let response = server
            .execute_get_simulation_models(Some(json!({
                "uri": Url::from_file_path(&focus).expect("focus file url"),
                "defaultModel": "Decay",
            })))
            .await
            .expect("simulation models response");

        assert_eq!(
            response.get("selectedModel").and_then(Value::as_str),
            Some("Decay"),
            "getSimulationModels should select the requested default model",
        );
        assert!(
            wait_for_simulation_compile_cache_model(server, "Decay").await,
            "getSimulationModels should prewarm the selected model compile cache",
        );
    });
}

#[test]
fn set_selected_simulation_model_prewarms_selected_model() {
    run_async_test(async {
        let temp = new_temp_dir("set-selected-simulation-model-prewarm");
        let focus = temp.join("Bundle.mo");
        std::fs::write(
            &focus,
            "model First\n  Real x(start=0);\nequation\n  der(x) = 1;\nend First;\n\nmodel Second\n  Real y(start=1);\nequation\n  der(y) = -y;\nend Second;\n",
        )
        .expect("write focus");

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }

        let response = server
            .execute_set_selected_simulation_model(Some(json!({
                "uri": Url::from_file_path(&focus).expect("focus file url"),
                "model": "Second",
            })))
            .await
            .expect("set selected simulation model response");

        assert_eq!(
            response.get("selectedModel").and_then(Value::as_str),
            Some("Second"),
            "setSelectedSimulationModel should report the selected model",
        );
        assert!(
            wait_for_simulation_compile_cache_model(server, "Second").await,
            "setSelectedSimulationModel should prewarm the selected model compile cache",
        );
    });
}

#[test]
fn prepare_simulation_models_request_returns_stale_error_after_revision_bump() {
    run_async_test(async {
        let temp = new_temp_dir("prepare-simulation-stale");
        let focus = temp.join("Bundle.mo");
        std::fs::write(
            &focus,
            "model First\n  Real x(start=0);\nequation\n  der(x) = 1;\nend First;\n",
        )
        .expect("write focus");
        let focus_key = focus.to_string_lossy().to_string();

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &focus_key,
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }

        let token = server.begin_analysis_request().await;
        let mut session_guard = server.session.write().await;
        let prepare_task = tokio::spawn({
            let server = server.clone();
            let focus_uri = Url::from_file_path(&focus).expect("focus file url");
            async move {
                server
                    .run_prepare_simulation_models_request(
                        focus_uri,
                        vec!["First".to_string()],
                        SimulationRequestSettings {
                            solver: "auto".to_string(),
                            t_end: 10.0,
                            dt: None,
                            library_paths: Vec::new(),
                        },
                        Some(token),
                    )
                    .await
            }
        });
        tokio::task::yield_now().await;
        session_guard.update_document(
            &focus_key,
            "model First\n  Real x(start=1);\nequation\n  der(x) = 2;\nend First;\n",
        );
        drop(session_guard);

        let (prepared_models, failures, error) = prepare_task
            .await
            .expect("prepare simulation task should finish");
        assert!(
            prepared_models.is_empty(),
            "stale prepare request should not report prepared models"
        );
        assert!(
            failures.is_empty(),
            "stale prepare request should not report compile failures"
        );
        assert_eq!(
            error.as_deref(),
            Some("request became stale after newer session changes"),
            "stale prepare request should report the stale-session error",
        );
        assert!(
            server.simulation_compile_cache.read().await.is_empty(),
            "stale prepare request should not populate the compile cache"
        );
    });
}

#[test]
fn simulate_model_returns_stale_error_after_revision_bump() {
    run_async_test(async {
        let temp = new_temp_dir("simulate-stale");
        let focus = temp.join("Root.mo");
        std::fs::write(
            &focus,
            "model Root\n  Real x(start=0);\nequation\n  der(x) = 1;\nend Root;\n",
        )
        .expect("write focus");
        let focus_uri = Url::from_file_path(&focus).expect("focus file url");
        let focus_key = session_document_uri_key(&focus_uri);

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &focus_key,
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }

        let token = server.begin_analysis_request().await;
        let mut session_guard = server.session.write().await;
        let simulate_task = tokio::spawn({
            let server = server.clone();
            let focus_uri = focus_uri.clone();
            async move {
                server
                    .execute_simulate_model(
                        Some(json!({
                            "uri": focus_uri,
                            "model": "Root",
                            "settings": {
                                "solver": "auto",
                                "tEnd": 1.0,
                                "dt": 0.1,
                                "modelicaPath": []
                            }
                        })),
                        Some(token),
                    )
                    .await
            }
        });
        tokio::task::yield_now().await;
        session_guard.update_document(
            &focus_key,
            "model Root\n  Real x(start=1);\nequation\n  der(x) = 2;\nend Root;\n",
        );
        drop(session_guard);

        let response = simulate_task
            .await
            .expect("simulate task should finish")
            .expect("simulate task should return a response");
        assert_eq!(response.get("ok").and_then(Value::as_bool), Some(false));
        assert_eq!(
            response.get("error").and_then(Value::as_str),
            Some("request became stale after newer session changes"),
            "stale simulation request should return the stale-session error",
        );
    });
}
