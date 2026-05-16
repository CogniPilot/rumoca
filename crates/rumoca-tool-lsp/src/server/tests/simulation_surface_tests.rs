use super::*;
use std::path::{Path, PathBuf};

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

fn write_simulation_subtree_workspace(temp: &Path) -> (PathBuf, PathBuf, PathBuf) {
    let workspace_root = temp.join("A");
    std::fs::create_dir_all(workspace_root.join("Sub1")).expect("mkdir Sub1");
    std::fs::create_dir_all(workspace_root.join("Sub2")).expect("mkdir Sub2");
    std::fs::write(
        workspace_root.join("package.mo"),
        "within ;\npackage A\nend A;\n",
    )
    .expect("write A/package.mo");
    std::fs::write(
        workspace_root.join("Sub1/package.mo"),
        "within A;\npackage Sub1\nend Sub1;\n",
    )
    .expect("write A/Sub1/package.mo");
    let model_path = workspace_root.join("Sub1/M.mo");
    std::fs::write(
        &model_path,
        "within A.Sub1;\nmodel M\n  Real x(start=0);\nequation\n  der(x) = 1;\nend M;\n",
    )
    .expect("write A/Sub1/M.mo");
    std::fs::write(
        workspace_root.join("Sub2/package.mo"),
        "within A;\npackage Sub2\nend Sub2;\n",
    )
    .expect("write A/Sub2/package.mo");
    std::fs::write(
        workspace_root.join("Sub2/N.mo"),
        "within A.Sub2;\nmodel N\nend N;\n",
    )
    .expect("write A/Sub2/N.mo");
    let focus = temp.join("Ball.mo");
    std::fs::write(
        &focus,
        "model Ball\n  A.Sub1.M m;\n  A.Sub2.N n;\nend Ball;\n",
    )
    .expect("write focus");
    (workspace_root, model_path, focus)
}

fn apply_structural_subtree_edit(session: &mut Session, model_uri: &str) {
    let open_error = session.update_document(
        model_uri,
        "within A.Sub1;\nmodel M\n  Real x(start=0);\nequation\n  der(x) = 1;\nend M;\n",
    );
    assert!(
        open_error.is_none(),
        "detaching the source-root-backed document should stay parseable"
    );
    let parse_error = session.update_document(
        model_uri,
        "within A.Sub1;\nmodel M\n  Real x(start=0);\n  parameter Real gain = 2;\nequation\n  der(x) = gain;\nend M;\n",
    );
    assert!(
        parse_error.is_none(),
        "structural subtree edit should stay parseable"
    );
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
fn render_template_command_renders_compiled_open_document_model() {
    run_async_test(async {
        let temp = new_temp_dir("render-template-command");
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
            .execute_command(ExecuteCommandParams {
                command: "rumoca.workspace.renderTemplate".to_string(),
                arguments: vec![serde_json::json!({
                    "uri": Url::from_file_path(&focus)
                        .expect("file uri")
                        .to_string(),
                    "model": "Decay",
                    "template": "{{ model_name }}",
                })],
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await
            .expect("execute command should succeed")
            .expect("execute command should return a payload");
        assert_eq!(
            response.get("ok").and_then(serde_json::Value::as_bool),
            Some(true),
            "render template command should report success"
        );
        assert_eq!(
            response.get("output").and_then(serde_json::Value::as_str),
            Some("Decay"),
            "render template command should render with the compiled model name"
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
                rumoca_compile::compile::SemanticDiagnosticsMode::Save,
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
fn simulate_model_returns_shared_report_payload_and_metrics() {
    run_async_test(async {
        let temp = new_temp_dir("simulate-shared-report");
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
            .execute_simulate_model(
                Some(serde_json::json!({
                    "uri": Url::from_file_path(&focus)
                        .expect("file uri")
                        .to_string(),
                    "model": "Decay",
                    "settings": {
                        "solver": "auto",
                        "tEnd": 1.0,
                        "dt": 0.1,
                    },
                })),
                None,
            )
            .await
            .expect("simulate should return a response");

        assert_eq!(
            response.get("ok").and_then(serde_json::Value::as_bool),
            Some(true),
            "simulation command should report success"
        );
        assert_eq!(response["payload"]["nStates"], 1);
        assert_eq!(
            response["payload"]["simDetails"]["requested"]["solver"],
            "auto"
        );
        assert!(
            response["metrics"]["compilePhaseSeconds"]["prepareContext"].is_number(),
            "shared metrics payload should include prepareContext timing: {response:?}"
        );
        assert!(
            response["payload"]["simDetails"]["timing"]["compile_phase_seconds"]["strict_resolve"]
                .is_number(),
            "shared payload should include strict_resolve timing: {response:?}"
        );
    });
}

#[test]
fn simulation_compile_keeps_sibling_namespace_fingerprint_warm_after_subtree_refresh() {
    run_async_test(async {
        let temp = new_temp_dir("simulation-subtree-refresh");
        let (workspace_root, model_path, focus) = write_simulation_subtree_workspace(&temp);

        let parsed = parse_source_root_with_cache(&workspace_root).expect("parse workspace root");
        let source_set_id = format!(
            "workspace::{}",
            canonical_path_key(workspace_root.to_string_lossy().as_ref())
        );
        let model_uri = model_path.to_string_lossy().to_string();

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.replace_parsed_source_set(
                &source_set_id,
                SourceRootKind::Workspace,
                parsed.documents,
                None,
            );
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
            session
                .namespace_index_query("")
                .expect("prime namespace cache");
        }
        let sub2_before = server
            .session
            .read()
            .await
            .namespace_fingerprint_cached("A.Sub2.")
            .expect("A.Sub2 namespace fingerprint before subtree refresh");

        {
            let mut session = server.session.write().await;
            apply_structural_subtree_edit(&mut session, &model_uri);
        }

        let compiled = server
            .compile_model_for_simulation("Ball", &focus.to_string_lossy())
            .await
            .expect("simulation compile after subtree refresh should succeed");

        assert_eq!(compiled.dae.states.len(), 1);
        let session = server.session.read().await;
        assert!(
            session.dirty_source_root_keys().is_empty(),
            "simulation compile should clear the pending subtree refresh state"
        );
        let sub2_after = session
            .namespace_fingerprint_cached("A.Sub2.")
            .expect("A.Sub2 namespace fingerprint after subtree refresh");
        assert_eq!(
            sub2_before, sub2_after,
            "simulation compile after refreshing A.Sub1 should keep the unaffected A.Sub2 subtree warm"
        );
    });
}

#[test]
fn isolated_simulation_session_skips_loaded_source_roots_for_local_only_models() {
    run_async_test(async {
        let temp = new_temp_dir("simulation-isolate-local-only");
        let focus = temp.join("Decay.mo");
        std::fs::write(
            &focus,
            "model Decay\n  Real x(start=1);\nequation\n  der(x) = -x;\nend Decay;\n",
        )
        .expect("write focus");
        let source_root_dir = write_test_source_root(&temp, "Lib");
        let source_root_doc = canonical_path_key(
            source_root_dir
                .join("package.mo")
                .to_string_lossy()
                .as_ref(),
        );

        let service = new_test_service();
        let server = service.inner();
        let source_set_key = source_root_source_set_key(source_root_dir.to_string_lossy().as_ref());
        let source_root_key = canonical_path_key(source_root_dir.to_string_lossy().as_ref());
        {
            let mut session = server.session.write().await;
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }
        let source_root_epoch = server.session.read().await.source_root_state_epoch();
        server
            .load_source_root_if_current(
                source_root_dir.to_string_lossy().as_ref(),
                &source_root_key,
                &source_set_key,
                None,
                source_root_epoch,
                SourceRootIndexingReason::CompletionImports,
            )
            .await
            .expect("source-root load should succeed")
            .expect("source root should load");

        let uris = server
            .isolated_simulation_document_uris_for_focus(&focus.to_string_lossy())
            .await
            .expect("isolated session uris");
        let canonical_uris = uris
            .iter()
            .map(|uri| canonical_path_key(uri))
            .collect::<Vec<_>>();

        assert!(
            uris.iter()
                .any(|uri| uri == focus.to_string_lossy().as_ref()),
            "isolated simulation session should keep the focus document",
        );
        assert!(
            !canonical_uris.iter().any(|uri| uri == &source_root_doc),
            "local-only simulation compile should not clone unrelated loaded source-root documents",
        );
    });
}

#[test]
fn isolated_simulation_session_keeps_loaded_source_roots_when_referenced() {
    run_async_test(async {
        let temp = new_temp_dir("simulation-isolate-with-source-root");
        let focus = temp.join("Decay.mo");
        std::fs::write(&focus, "model Decay\n  Lib.A a;\nend Decay;\n").expect("write focus");
        let source_root_dir = write_test_source_root(&temp, "Lib");
        let source_root_doc = canonical_path_key(
            source_root_dir
                .join("package.mo")
                .to_string_lossy()
                .as_ref(),
        );

        let service = new_test_service();
        let server = service.inner();
        let source_set_key = source_root_source_set_key(source_root_dir.to_string_lossy().as_ref());
        let source_root_key = canonical_path_key(source_root_dir.to_string_lossy().as_ref());
        {
            let mut session = server.session.write().await;
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }
        let source_root_epoch = server.session.read().await.source_root_state_epoch();
        server
            .load_source_root_if_current(
                source_root_dir.to_string_lossy().as_ref(),
                &source_root_key,
                &source_set_key,
                None,
                source_root_epoch,
                SourceRootIndexingReason::CompletionImports,
            )
            .await
            .expect("source-root load should succeed")
            .expect("source root should load");

        let uris = server
            .isolated_simulation_document_uris_for_focus(&focus.to_string_lossy())
            .await
            .expect("isolated session uris");
        let canonical_uris = uris
            .iter()
            .map(|uri| canonical_path_key(uri))
            .collect::<Vec<_>>();

        assert!(
            canonical_uris.iter().any(|uri| uri == &source_root_doc),
            "simulation compile should keep loaded source-root documents when the local compile unit references that root",
        );
    });
}

#[test]
fn isolated_simulation_session_keeps_workspace_source_root_documents() {
    run_async_test(async {
        let temp = new_temp_dir("simulation-isolate-with-workspace-root");
        let focus = temp.join("Decay.mo");
        std::fs::write(&focus, "model Decay\n  NewFolder.Test test;\nend Decay;\n")
            .expect("write focus");
        let workspace_doc = "workspace/NewFolder/Test.mo".to_string();

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.replace_parsed_source_set(
                "workspace",
                SourceRootKind::Workspace,
                vec![(workspace_doc.clone(), ast::StoredDefinition::default())],
                None,
            );
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }

        let uris = server
            .isolated_simulation_document_uris_for_focus(&focus.to_string_lossy())
            .await
            .expect("isolated session uris");

        assert!(
            uris.iter().any(|uri| uri == &workspace_doc),
            "simulation isolation should keep source-root-backed workspace documents",
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
                    source_root_paths: Vec::new(),
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
                    "sourceRootPaths": [],
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
                    "sourceRootPaths": [],
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
                            source_root_paths: Vec::new(),
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
                                "sourceRootPaths": []
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
