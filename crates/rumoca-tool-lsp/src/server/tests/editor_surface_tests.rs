//! Editor-surface handler coverage: the shared per-handler assertions that
//! `workspace_query_tests` reuses against a fully warmed workspace, plus the
//! workspace command-dispatch and scenario-config reload surfaces.
//!
//! Request-scoped timing surfaces live in the sibling modules:
//! `completion_surface_tests`, `hover_definition_timing_tests`,
//! `code_lens_tests`, `compile_unit_source_tests` and
//! `source_root_load_diagnostic_tests`.

use super::*;

pub(super) async fn assert_formatting_wraps_handler(
    server: &ModelicaLanguageServer,
    formatting_uri: &Url,
) {
    let formatting = server
        .formatting(DocumentFormattingParams {
            text_document: TextDocumentIdentifier {
                uri: formatting_uri.clone(),
            },
            options: FormattingOptions::default(),
            work_done_progress_params: WorkDoneProgressParams::default(),
        })
        .await
        .expect("formatting should succeed")
        .expect("formatting should return an edit");
    assert_eq!(formatting.len(), 1);
    assert!(
        formatting[0].new_text.ends_with("end F;\n"),
        "formatting should insert the default final newline"
    );
}

pub(super) async fn assert_document_links_and_inlay_hints(
    server: &ModelicaLanguageServer,
    surface_uri: &Url,
) {
    let links = server
        .document_link(DocumentLinkParams {
            text_document: TextDocumentIdentifier {
                uri: surface_uri.clone(),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        })
        .await
        .expect("document links should succeed")
        .expect("document links response");
    assert!(
        links.iter().any(|link| {
            link.target
                .as_ref()
                .is_some_and(|target| target.as_str().starts_with("https://example.com/docs"))
        }),
        "document links should include the external URL"
    );
    assert!(
        links.iter().any(|link| {
            link.target
                .as_ref()
                .is_some_and(|target| target.as_str().ends_with("/Lib/package.mo"))
        }),
        "document links should include the quoted file path"
    );

    let hints = server
        .inlay_hint(InlayHintParams {
            text_document: TextDocumentIdentifier {
                uri: surface_uri.clone(),
            },
            range: Range {
                start: Position::new(0, 0),
                end: Position::new(20, 0),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
        })
        .await
        .expect("inlay hints should succeed")
        .expect("inlay hints response");
    assert!(
        hints.iter().any(|hint| matches!(
            &hint.label,
            InlayHintLabel::String(label) if label.contains("[2x3]")
        )),
        "inlay hints should include the array-dimension hint"
    );
    // Both special-case hint families must stay live: the array-dimension hint
    // above and the builtin parameter-name hint for `sin(helperInst.gain)`.
    // The full-MSL editor gate asserts the same two families over the wire.
    assert!(
        hints.iter().any(|hint| {
            hint.kind == Some(InlayHintKind::PARAMETER)
                && matches!(&hint.label, InlayHintLabel::String(label) if label == "u:")
        }),
        "inlay hints should include the builtin parameter-name hint: {hints:?}"
    );
}

pub(super) async fn assert_code_actions_wrap_handler(
    server: &ModelicaLanguageServer,
    formatting_uri: &Url,
) {
    let mut diagnostic_session = Session::default();
    let diagnostics = handlers::compute_diagnostics(
        BROKEN_ACTION_SOURCE,
        "input.mo",
        Some(&mut diagnostic_session),
    );
    let range = diagnostics
        .iter()
        .find(|diag| diag.code.as_ref() == Some(&NumberOrString::String("ET001".to_string())))
        .map(|diag| diag.range)
        .expect("ET001 diagnostic range");
    let actions = server
        .code_action(CodeActionParams {
            text_document: TextDocumentIdentifier {
                uri: formatting_uri.clone(),
            },
            range,
            context: CodeActionContext {
                diagnostics,
                only: None,
                trigger_kind: Some(CodeActionTriggerKind::INVOKED),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        })
        .await
        .expect("code action should succeed")
        .expect("code actions should exist");
    assert!(
        actions.iter().any(|action| matches!(
            action,
            CodeActionOrCommand::CodeAction(code_action)
                if code_action.title.contains("Replace `startdt` with `start`")
        )),
        "code actions should surface the unknown-modifier fix"
    );
}

#[test]
fn execute_command_dispatches_safe_scenario_command() {
    run_async_test(async {
        let workspace_root = new_temp_dir("execute-command");
        let workspace_uri = Url::from_directory_path(&workspace_root).expect("workspace uri");
        let service = new_test_service();
        let server = service.inner();
        server
            .initialize(InitializeParams {
                root_uri: Some(workspace_uri),
                ..InitializeParams::default()
            })
            .await
            .expect("initialize should succeed");

        let response = server
            .execute_command(ExecuteCommandParams {
                command: "rumoca.scenario.getSimulationConfig".to_string(),
                arguments: vec![serde_json::json!({
                    "workspaceRoot": workspace_root.display().to_string(),
                    "model": "Ball",
                    "fallback": {
                        "solver": "auto",
                        "tEnd": 10.0,
                        "dt": null,
                        "outputDir": "",
                        "sourceRootPaths": [],
                    },
                })],
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await
            .expect("execute command should succeed")
            .expect("execute command should return a payload");
        assert_eq!(
            response
                .get("effective")
                .and_then(|value| value.get("solver")),
            Some(&serde_json::json!("auto"))
        );
    });
}

#[test]
fn execute_command_dispatches_workspace_target_catalog_command() {
    run_async_test(async {
        let workspace_root = new_temp_dir("execute-workspace-target-command");
        let workspace_uri = Url::from_directory_path(&workspace_root).expect("workspace uri");
        let service = new_test_service();
        let server = service.inner();
        server
            .initialize(InitializeParams {
                root_uri: Some(workspace_uri),
                ..InitializeParams::default()
            })
            .await
            .expect("initialize should succeed");

        let response = server
            .execute_command(ExecuteCommandParams {
                command: "rumoca.workspace.getBuiltinTargets".to_string(),
                arguments: Vec::new(),
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await
            .expect("execute command should succeed")
            .expect("execute command should return a payload");
        let targets = response
            .as_array()
            .expect("target catalog should serialize to an array");
        assert!(
            targets.iter().any(|target| {
                target.get("id").and_then(serde_json::Value::as_str) == Some("c-ode")
            }),
            "workspace target catalog should include the checked Solve C built-in"
        );
    });
}

#[test]
fn reload_scenario_config_rewarms_durable_libraries_when_paths_change() {
    run_async_test(async {
        let workspace_root = new_temp_dir("reload-scenario-config-source-root-reset");
        let focus = workspace_root.join("Root.mo");
        let source_root_a = write_test_source_root(&workspace_root, "LibA");
        let source_root_b = write_test_source_root(&workspace_root, "LibB");
        std::fs::write(
            &focus,
            "model Root\n  Real x(start=0);\nequation\n  der(x) = 1;\nend Root;\n",
        )
        .expect("write focus");

        let service = new_test_service();
        let server = service.inner();
        *server.workspace_root.write().await = Some(workspace_root.clone());
        *server.initial_source_root_paths.write().await =
            vec![source_root_a.to_string_lossy().to_string()];
        *server.source_root_paths.write().await = vec![source_root_a.to_string_lossy().to_string()];

        {
            let mut session = server.session.write().await;
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }

        server
            .compile_model_for_simulation("Root", &focus.to_string_lossy())
            .await
            .expect("initial simulation compile should succeed");
        {
            let source_root_key = canonical_path_key(&source_root_a.to_string_lossy());
            let source_set_key =
                source_root_source_set_key(source_root_a.to_string_lossy().as_ref());
            let source_root_epoch = server.session.read().await.source_root_state_epoch();
            server
                .load_source_root_if_current(
                    source_root_a.to_string_lossy().as_ref(),
                    &source_root_key,
                    &source_set_key,
                    None,
                    source_root_epoch,
                    SourceRootIndexingReason::CompletionImports,
                )
                .await
                .expect("source-root load should succeed")
                .expect("source root should load");
        }

        *server.initial_source_root_paths.write().await =
            vec![source_root_b.to_string_lossy().to_string()];
        server.reload_scenario_config().await;

        assert!(
            server.simulation_compile_cache.read().await.is_empty(),
            "changing effective source-root paths must flush simulation cache"
        );
        assert!(
            server
                .session
                .read()
                .await
                .is_source_root_path_loaded(&canonical_path_key(&source_root_b.to_string_lossy())),
            "changing effective source-root paths should immediately rewarm durable roots"
        );
        assert!(
            wait_for_namespace_cache_prewarm(server)
                .await
                .contains(&"LibB.A".to_string()),
            "changing effective source-root paths should also prewarm namespace completion for the new durable root"
        );
        let source_root_path_keys = server
            .source_root_paths
            .read()
            .await
            .iter()
            .map(|path| canonical_path_key(path))
            .collect::<Vec<_>>();
        assert_eq!(
            source_root_path_keys,
            vec![canonical_path_key(&source_root_b.to_string_lossy())],
            "reloaded scenario config should publish the updated source-root path set"
        );
    });
}

#[test]
fn reload_scenario_config_uses_open_document_for_workspace_config_focus() {
    run_async_test(async {
        let workspace_root = new_temp_dir("reload-workspace-config-focused-document");
        let focus_dir = workspace_root.join("examples/control");
        std::fs::create_dir_all(&focus_dir).expect("mkdir focus dir");
        let base_root = workspace_root.join("lib/Base");
        let scoped_root = workspace_root.join("lib/Control");
        let child_root = focus_dir.join("local/Child");
        std::fs::create_dir_all(&base_root).expect("mkdir base root");
        std::fs::create_dir_all(&scoped_root).expect("mkdir scoped root");
        std::fs::create_dir_all(&child_root).expect("mkdir child root");
        std::fs::write(
            workspace_root.join("rumoca-workspace.toml"),
            r#"
source_roots = ["lib/Base"]

[source_root_scopes."examples/control"]
source_roots = ["lib/Control"]
"#,
        )
        .expect("write root workspace config");
        std::fs::write(
            focus_dir.join("rumoca-workspace.toml"),
            r#"source_roots = ["local/Child"]"#,
        )
        .expect("write child workspace config");
        let focus = focus_dir.join("Plant.mo");
        std::fs::write(&focus, "model Plant\nend Plant;\n").expect("write focus");

        let service = new_test_service();
        let server = service.inner();
        *server.workspace_root.write().await = Some(workspace_root.clone());
        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Url::from_file_path(&focus).expect("focus uri"),
                    language_id: "modelica".to_string(),
                    version: 1,
                    text: std::fs::read_to_string(&focus).expect("read focus"),
                },
            })
            .await;

        let paths = server.source_root_paths.read().await.clone();
        let expected_paths = [
            base_root.as_path(),
            scoped_root.as_path(),
            child_root.as_path(),
        ]
        .into_iter()
        .map(|path| {
            std::fs::canonicalize(path)
                .expect("canonical source root")
                .to_string_lossy()
                .to_string()
        })
        .collect::<Vec<_>>();
        assert_eq!(paths, expected_paths);

        server
            .did_close(DidCloseTextDocumentParams {
                text_document: TextDocumentIdentifier {
                    uri: Url::from_file_path(&focus).expect("focus uri"),
                },
            })
            .await;

        let paths = server.source_root_paths.read().await.clone();
        let expected_paths = vec![
            std::fs::canonicalize(&base_root)
                .expect("canonical base root")
                .to_string_lossy()
                .to_string(),
        ];
        assert_eq!(
            paths, expected_paths,
            "did_close should remove closed documents from workspace-config focus"
        );
    });
}
