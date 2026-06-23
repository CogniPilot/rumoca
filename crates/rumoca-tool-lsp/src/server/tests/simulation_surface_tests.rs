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

#[test]
fn render_scenario_config_command_renders_shared_toml() {
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();

        let response = server
            .execute_render_scenario_config(Some(serde_json::json!({
                "config": {
                    "rumoca": { "version": "1", "task": "simulate" },
                    "model": { "file": "Ball.mo", "name": "Ball" },
                    "sim": { "solver": "auto", "t_end": 10.0 },
                    "viewer": { "mode": "results_panel" },
                    "plot": {
                        "views": [{
                            "id": "states_time",
                            "title": "States vs Time",
                            "type": "timeseries",
                            "x": "time",
                            "y": ["*states"]
                        }]
                    }
                }
            })))
            .await
            .expect("scenario render response");

        assert_eq!(
            response.get("ok").and_then(serde_json::Value::as_bool),
            Some(true)
        );
        let content = response
            .get("content")
            .and_then(serde_json::Value::as_str)
            .expect("rendered content");
        assert!(content.contains("[rumoca]"));
        assert!(content.contains("task = \"simulate\""));
        assert!(content.contains("[[plot.views]]"));

        let commands = ModelicaLanguageServer::server_capabilities()
            .execute_command_provider
            .expect("execute command provider")
            .commands;
        assert!(
            commands.contains(&"rumoca.scenario.renderScenarioConfig".to_string()),
            "render scenario config command must be advertised"
        );
    });
}

#[test]
fn full_scenario_config_commands_round_trip_shared_toml() {
    run_async_test(async {
        let temp = new_temp_dir("full-scenario-config-command");
        let scenario_path = temp.join("rumoca-scenario.toml");
        let original_scenario = scenario_config_text("Ball.mo", "Ball", Some("auto"));
        std::fs::write(&scenario_path, &original_scenario).expect("write scenario");

        let service = new_test_service();
        let server = service.inner();
        {
            let mut workspace_root = server.workspace_root.write().await;
            *workspace_root = Some(temp);
        }

        let uri = Url::from_file_path(&scenario_path)
            .expect("scenario uri")
            .to_string();
        let full = server
            .execute_get_scenario_config_full(Some(serde_json::json!({ "uri": uri })))
            .await
            .expect("full scenario response");
        assert_eq!(
            full.get("ok").and_then(serde_json::Value::as_bool),
            Some(true)
        );
        assert_eq!(
            full.pointer("/config/model/name")
                .and_then(serde_json::Value::as_str),
            Some("Ball")
        );
        let live_scenario = scenario_config_text("Live.mo", "Live", None);
        let live_full = server
            .execute_get_scenario_config_full(Some(serde_json::json!({
                "uri": uri,
                "source": live_scenario,
            })))
            .await
            .expect("live full scenario response");
        assert_eq!(
            live_full
                .pointer("/config/model/name")
                .and_then(serde_json::Value::as_str),
            Some("Live"),
            "explicit scenario source should take precedence over disk contents"
        );

        let mut config = full.get("config").cloned().expect("config tree");
        config["sim"]["solver"] = serde_json::Value::String("bdf".to_string());
        let saved = server
            .execute_set_scenario_config(Some(serde_json::json!({
                "uri": uri,
                "config": config,
            })))
            .await
            .expect("set scenario response");
        assert_eq!(
            saved.get("ok").and_then(serde_json::Value::as_bool),
            Some(true)
        );
        let updated = std::fs::read_to_string(&scenario_path).expect("read updated scenario");
        assert!(updated.contains("solver = \"bdf\""));

        let scenario_uri = Url::parse(&uri).expect("scenario uri parse");
        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: scenario_uri,
                    language_id: "toml".to_string(),
                    version: 1,
                    text: original_scenario,
                },
            })
            .await;
        let rejected = server
            .execute_set_scenario_config(Some(serde_json::json!({
                "uri": uri,
                "config": config,
            })))
            .await
            .expect("open set scenario response");
        assert_eq!(
            rejected.get("ok").and_then(serde_json::Value::as_bool),
            Some(false)
        );
        assert!(
            rejected
                .get("error")
                .and_then(serde_json::Value::as_str)
                .is_some_and(|error| error.contains("open in the editor")),
            "open scenario save should require an editor edit"
        );
        let disk_after_rejected_save =
            std::fs::read_to_string(&scenario_path).expect("read scenario after rejected save");
        assert!(disk_after_rejected_save.contains("solver = \"bdf\""));

        assert_full_scenario_config_commands_advertised();
    });
}

fn scenario_config_text(model_file: &str, model_name: &str, solver: Option<&str>) -> String {
    let mut lines = vec![
        "[rumoca]".to_string(),
        "version = \"1\"".to_string(),
        "task = \"simulate\"".to_string(),
        String::new(),
        "[model]".to_string(),
        format!("file = \"{model_file}\""),
        format!("name = \"{model_name}\""),
        String::new(),
    ];
    if let Some(solver) = solver {
        lines.extend([
            "[sim]".to_string(),
            format!("solver = \"{solver}\""),
            String::new(),
        ]);
    }
    lines.join("\n")
}

fn assert_full_scenario_config_commands_advertised() {
    let commands = ModelicaLanguageServer::server_capabilities()
        .execute_command_provider
        .expect("execute command provider")
        .commands;
    for command in [
        "rumoca.scenario.getScenarioConfigFull",
        "rumoca.scenario.setScenarioConfig",
        "rumoca.scenario.getCodegenConfig",
        "rumoca.scenario.setCodegenConfig",
        "rumoca.scenario.getSourceRoots",
        "rumoca.scenario.setSourceRoots",
        "rumoca.model.parameterMetadata",
    ] {
        assert!(
            commands.contains(&command.to_string()),
            "{command} must be advertised"
        );
    }
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
        assert_eq!(compiled.dae.variables.states.len(), 1);
    });
}

#[test]
fn compile_model_for_simulation_ignores_unreferenced_library_typecheck_errors() {
    run_async_test(async {
        // Editor setup from .vscode/settings.json: a configured library source
        // root that does not fully typecheck on its own (the MSL `Modelica`
        // package without its `ModelicaServices` sibling references
        // `ModelicaServices.Types.SolverMethod`). A focus model that never
        // references the library must still compile, exactly like the CLI's
        // tolerant load + strict-reachable recovery path.
        let temp = new_temp_dir("compile-unreferenced-library-typecheck");
        let focus = temp.join("Ball.mo");
        std::fs::write(
            &focus,
            "model Ball\n  Real x(start=1);\nequation\n  der(x) = -x;\nend Ball;\n",
        )
        .expect("write focus");
        let lib_dir = temp.join("lib").join("Lib");
        std::fs::create_dir_all(&lib_dir).expect("mkdir lib");
        std::fs::write(
            lib_dir.join("package.mo"),
            "package Lib\n  model Broken\n    MissingServices.Types.SolverMethod method;\n  end Broken;\nend Lib;\n",
        )
        .expect("write lib package");

        let service = new_test_service();
        let server = service.inner();
        let lib_root = lib_dir.to_string_lossy().to_string();
        let source_set_key = source_root_source_set_key(&lib_root);
        let source_root_key = canonical_path_key(&lib_root);
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
                &lib_root,
                &source_root_key,
                &source_set_key,
                None,
                source_root_epoch,
                SourceRootIndexingReason::CompletionImports,
            )
            .await
            .expect("source-root load should succeed")
            .expect("source root should load");

        let compiled = server
            .compile_model_for_simulation("Ball", &focus.to_string_lossy())
            .await
            .expect("focus model must compile despite unreferenced library typecheck errors");
        assert_eq!(compiled.dae.variables.states.len(), 1);
    });
}

#[test]
fn compile_model_for_simulation_ignores_sibling_pulled_library_typecheck_errors() {
    run_async_test(async {
        // The user-reported editor failure: Ball.mo sits in examples/models/
        // next to siblings (PIDMSL.mo) that DO reference the configured
        // library, and the library does not fully typecheck on its own (MSL
        // `Modelica` without `ModelicaServices`). Compiling Ball must not
        // fail on the library's unresolved references.
        let temp = new_temp_dir("compile-sibling-pulled-library");
        let focus = temp.join("Ball.mo");
        std::fs::write(
            &focus,
            "model Ball\n  Real x(start=1);\nequation\n  der(x) = -x;\nend Ball;\n",
        )
        .expect("write focus");
        std::fs::write(
            temp.join("UsesLib.mo"),
            "model UsesLib\n  Lib.Good g;\nend UsesLib;\n",
        )
        .expect("write sibling");
        let lib_dir = temp.join("lib").join("Lib");
        std::fs::create_dir_all(&lib_dir).expect("mkdir lib");
        std::fs::write(
            lib_dir.join("package.mo"),
            "package Lib\n  model Good\n    Real x(start=0);\n  equation\n    der(x) = 1;\n  end Good;\n  model Broken\n    MissingServices.Types.SolverMethod method;\n  end Broken;\nend Lib;\n",
        )
        .expect("write lib package");

        let service = new_test_service();
        let server = service.inner();
        let lib_root = lib_dir.to_string_lossy().to_string();
        let source_set_key = source_root_source_set_key(&lib_root);
        let source_root_key = canonical_path_key(&lib_root);
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
                &lib_root,
                &source_root_key,
                &source_set_key,
                None,
                source_root_epoch,
                SourceRootIndexingReason::CompletionImports,
            )
            .await
            .expect("source-root load should succeed")
            .expect("source root should load");

        let compiled = server
            .compile_model_for_simulation("Ball", &focus.to_string_lossy())
            .await
            .expect("focus model must compile despite sibling-pulled library typecheck errors");
        assert_eq!(compiled.dae.variables.states.len(), 1);
    });
}

#[test]
fn compile_model_for_simulation_handles_real_examples_ball_with_msl_root() {
    run_async_test(async {
        // Exact user-reported editor setup: focus examples/models/Ball.mo
        // (siblings PIDMSL.mo etc. reference Modelica) with the repo
        // .vscode/settings.json source root `.../Modelica 4.1.0` (no
        // ModelicaServices sibling). Reported failure: "Ball failed in
        // Typecheck: undefined type: `ModelicaServices.Types.SolverMethod`".
        let Some(msl_root) = cached_msl_source_root() else {
            eprintln!("SKIP: MSL cache not present");
            return;
        };
        let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
        let focus = workspace.join("examples/models/Ball.mo");
        assert!(focus.is_file(), "repo Ball.mo expected");

        let service = new_test_service();
        let server = service.inner();
        let lib_root = msl_root.to_string_lossy().to_string();
        let source_set_key = source_root_source_set_key(&lib_root);
        let source_root_key = canonical_path_key(&lib_root);
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
                &lib_root,
                &source_root_key,
                &source_set_key,
                None,
                source_root_epoch,
                SourceRootIndexingReason::CompletionImports,
            )
            .await
            .expect("source-root load should succeed")
            .expect("source root should load");

        let compiled = server
            .compile_model_for_simulation("Ball", &focus.to_string_lossy())
            .await
            .expect("Ball must compile with the editor-configured MSL source root");
        assert_eq!(compiled.dae.variables.states.len(), 2, "Ball has x and v");
    });
}

#[test]
fn compile_model_for_simulation_still_fails_models_that_use_broken_library_alias() {
    run_async_test(async {
        // Counterpart to the Ball test above: deferring unresolvable alias
        // targets must not weaken checking for models that actually USE the
        // alias — they keep failing in Typecheck with the missing target.
        let Some(msl_root) = cached_msl_source_root() else {
            eprintln!("SKIP: MSL cache not present");
            return;
        };
        let temp = new_temp_dir("compile-uses-broken-alias");
        let focus = temp.join("UsesClocked.mo");
        std::fs::write(
            &focus,
            "model UsesClocked\n  parameter Modelica.Clocked.Types.SolverMethod m = \"x\";\n  Real x(start = 1);\nequation\n  der(x) = -x;\nend UsesClocked;\n",
        )
        .expect("write focus");

        let service = new_test_service();
        let server = service.inner();
        let lib_root = msl_root.to_string_lossy().to_string();
        let source_set_key = source_root_source_set_key(&lib_root);
        let source_root_key = canonical_path_key(&lib_root);
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
                &lib_root,
                &source_root_key,
                &source_set_key,
                None,
                source_root_epoch,
                SourceRootIndexingReason::CompletionImports,
            )
            .await
            .expect("source-root load should succeed")
            .expect("source root should load");

        let error = server
            .compile_model_for_simulation("UsesClocked", &focus.to_string_lossy())
            .await
            .expect_err("a model using the broken alias must keep failing");
        assert!(
            error.contains("ModelicaServices.Types.SolverMethod") || error.contains("SolverMethod"),
            "failure should name the unresolved alias target, got: {error}"
        );
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
        assert_eq!(first.dae.variables.states.len(), 1);

        std::fs::write(&broken, "model Broken\n  Real x\nend Broken;\n").expect("write broken");

        let second = server
            .compile_model_for_simulation("Root", &focus.to_string_lossy())
            .await
            .expect("second focused compile should ignore unrelated local parse errors");
        assert_eq!(second.dae.variables.states.len(), 1);
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
fn render_target_command_renders_compiled_open_document_model() {
    run_async_test(async {
        let temp = new_temp_dir("render-target-command");
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
                command: "rumoca.workspace.renderTarget".to_string(),
                arguments: vec![serde_json::json!({
                    "uri": Url::from_file_path(&focus)
                        .expect("file uri")
                        .to_string(),
                    "model": "Decay",
                    "target": "sympy",
                })],
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await
            .expect("execute command should succeed")
            .expect("execute command should return a payload");
        assert_eq!(
            response.get("ok").and_then(serde_json::Value::as_bool),
            Some(true),
            "render target command should report success"
        );
        assert!(
            response
                .get("files")
                .and_then(serde_json::Value::as_array)
                .is_some_and(|files| files.iter().any(|file| {
                    file.get("path").and_then(serde_json::Value::as_str) == Some("Decay_sympy.py")
                })),
            "render target command should return the built-in SymPy model file"
        );
    });
}

#[test]
fn render_target_command_renders_relative_raw_jinja_from_rum_scenario() {
    run_async_test(async {
        let temp = new_temp_dir("render-target-raw-jinja-scenario");
        let model_dir = temp.join("models");
        let scenario_dir = temp.join("codegen");
        std::fs::create_dir_all(&model_dir).expect("mkdir models");
        std::fs::create_dir_all(&scenario_dir).expect("mkdir codegen");
        let model_path = model_dir.join("Decay.mo");
        let scenario_path = scenario_dir.join("rumoca-scenario.toml");
        let template_path = scenario_dir.join("custom.py.jinja");
        std::fs::write(
            &model_path,
            "model Decay\n  Real x(start=1);\nequation\n  der(x) = -x;\nend Decay;\n",
        )
        .expect("write model");
        std::fs::write(
            &scenario_path,
            "[rumoca]\nversion = \"1\"\ntask = \"codegen\"\n\n[model]\nfile = \"../models/Decay.mo\"\nname = \"Decay\"\n\n[codegen]\ntarget = \"custom.py.jinja\"\n",
        )
        .expect("write scenario");
        std::fs::write(
            &template_path,
            "model={{ model_name }} f_x={{ dae.f_x | length }}",
        )
        .expect("write template");

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &scenario_path.to_string_lossy(),
                &std::fs::read_to_string(&scenario_path).expect("read scenario"),
            );
        }

        let response = server
            .execute_command(ExecuteCommandParams {
                command: "rumoca.workspace.renderTarget".to_string(),
                arguments: vec![serde_json::json!({
                    "uri": Url::from_file_path(&scenario_path)
                        .expect("scenario file uri")
                        .to_string(),
                    "model": "Decay",
                    "target": "custom.py.jinja",
                })],
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await
            .expect("execute command should succeed")
            .expect("execute command should return a payload");

        assert_eq!(
            response.get("ok").and_then(serde_json::Value::as_bool),
            Some(true),
            "raw Jinja render target command should report success: {response:#?}"
        );
        let files = response
            .get("files")
            .and_then(serde_json::Value::as_array)
            .expect("render response should include files");
        assert!(
            files.iter().any(|file| {
                file.get("path").and_then(serde_json::Value::as_str) == Some("custom.py")
                    && file
                        .get("content")
                        .and_then(serde_json::Value::as_str)
                        .is_some_and(|content| content.contains("model=Decay"))
            }),
            "render target command should return rendered raw template output: {response:#?}"
        );
    });
}

#[test]
fn render_target_command_renders_relative_target_directory_from_rum_scenario() {
    run_async_test(async {
        let temp = new_temp_dir("render-target-directory-scenario");
        let model_dir = temp.join("models");
        let scenario_dir = temp.join("codegen");
        let target_dir = scenario_dir.join("standalone_web");
        std::fs::create_dir_all(&model_dir).expect("mkdir models");
        std::fs::create_dir_all(&target_dir).expect("mkdir target");
        let model_path = model_dir.join("Decay.mo");
        let scenario_path = scenario_dir.join("rumoca-scenario.toml");
        std::fs::write(
            &model_path,
            "model Decay\n  Real x(start=1);\nequation\n  der(x) = -x;\nend Decay;\n",
        )
        .expect("write model");
        std::fs::write(
            &scenario_path,
            "[rumoca]\nversion = \"1\"\ntask = \"codegen\"\n\n[model]\nfile = \"../models/Decay.mo\"\nname = \"Decay\"\n\n[codegen]\ntarget = \"standalone_web\"\n",
        )
        .expect("write scenario");
        std::fs::write(
            target_dir.join("target.toml"),
            "version = 1\nir = \"dae\"\n\n[[files]]\npath = \"{{ model_name }}.txt\"\ntemplate = \"model.txt.jinja\"\n",
        )
        .expect("write target manifest");
        std::fs::write(
            target_dir.join("model.txt.jinja"),
            "target={{ model_name }} f_x={{ dae.f_x | length }}",
        )
        .expect("write target template");

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &scenario_path.to_string_lossy(),
                &std::fs::read_to_string(&scenario_path).expect("read scenario"),
            );
        }

        let response = server
            .execute_command(ExecuteCommandParams {
                command: "rumoca.workspace.renderTarget".to_string(),
                arguments: vec![serde_json::json!({
                    "uri": Url::from_file_path(&scenario_path)
                        .expect("scenario file uri")
                        .to_string(),
                    "model": "Decay",
                    "target": "standalone_web",
                })],
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await
            .expect("execute command should succeed")
            .expect("execute command should return a payload");

        assert_eq!(
            response.get("ok").and_then(serde_json::Value::as_bool),
            Some(true),
            "target-directory render command should report success: {response:#?}"
        );
        assert!(
            response
                .get("files")
                .and_then(serde_json::Value::as_array)
                .is_some_and(|files| files.iter().any(|file| {
                    file.get("path").and_then(serde_json::Value::as_str) == Some("Decay.txt")
                        && file
                            .get("content")
                            .and_then(serde_json::Value::as_str)
                            .is_some_and(|content| content.contains("target=Decay"))
                })),
            "render command should return rendered directory target output: {response:#?}"
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

        assert_eq!(compiled.dae.variables.states.len(), 1);
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
fn simulate_rum_scenario_can_use_closed_model_file() {
    run_async_test(async {
        let temp = new_temp_dir("simulate-rum-closed-model");
        let model_path = temp.join("Decay.mo");
        let scenario_path = temp.join("rumoca-scenario.toml");
        std::fs::write(
            &model_path,
            "model Decay\n  Real x(start=1);\nequation\n  der(x) = -x;\nend Decay;\n",
        )
        .expect("write model");
        std::fs::write(
            &scenario_path,
            "[rumoca]\nversion = \"1\"\ntask = \"simulate\"\n\n[model]\nfile = \"Decay.mo\"\nname = \"Decay\"\n",
        )
        .expect("write scenario");

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &scenario_path.to_string_lossy(),
                &std::fs::read_to_string(&scenario_path).expect("read scenario"),
            );
        }

        let response = server
            .execute_simulate_model(
                Some(serde_json::json!({
                    "uri": Url::from_file_path(&scenario_path)
                        .expect("scenario file uri")
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
            "simulation from a rumoca-scenario.toml scenario should not require the referenced model file to be open: {response:#?}"
        );
    });
}

#[test]
fn scenario_config_defaults_input_scenarios_to_embedded_viewer() {
    run_async_test(async {
        let temp = new_temp_dir("scenario-embedded-input-viewer");
        let scenario_path = temp.join("rumoca-scenario.toml");
        let scenario = [
            "[rumoca]",
            "version = \"1\"",
            "task = \"simulate\"",
            "",
            "[model]",
            "file = \"Rover.mo\"",
            "name = \"Rover\"",
            "",
            "[sim]",
            "mode = \"realtime\"",
            "",
            "[input]",
            "mode = \"auto\"",
            "",
            "[transport.http]",
            "port = 8080",
            "scene = \"rover_scene.js\"",
            "",
        ]
        .join("\n");
        std::fs::write(&scenario_path, &scenario).expect("write scenario");

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(&scenario_path.to_string_lossy(), &scenario);
        }

        let response = server
            .execute_get_scenario_config(Some(serde_json::json!({
                "uri": Url::from_file_path(&scenario_path)
                    .expect("scenario uri")
                    .to_string(),
            })))
            .await
            .expect("scenario response");

        assert_eq!(
            response.get("ok").and_then(serde_json::Value::as_bool),
            Some(true)
        );
        assert_eq!(
            response
                .get("viewerMode")
                .and_then(serde_json::Value::as_str),
            Some("results_panel")
        );
        assert_eq!(
            response.get("simMode").and_then(serde_json::Value::as_str),
            Some("realtime")
        );
        assert_eq!(
            response.get("httpPort").and_then(serde_json::Value::as_i64),
            Some(8080)
        );
        assert_eq!(
            response
                .get("httpScene")
                .and_then(serde_json::Value::as_str),
            Some("rover_scene.js")
        );
        assert_eq!(
            response
                .get("inputEnabled")
                .and_then(serde_json::Value::as_bool),
            Some(true)
        );
    });
}

#[test]
fn scenario_config_explicit_results_panel_overrides_input_inference() {
    run_async_test(async {
        let temp = new_temp_dir("scenario-results-panel-viewer");
        let scenario_path = temp.join("rumoca-scenario.toml");
        let scenario = [
            "[rumoca]",
            "version = \"1\"",
            "task = \"simulate\"",
            "",
            "[viewer]",
            "mode = \"results_panel\"",
            "",
            "[model]",
            "file = \"Ball.mo\"",
            "name = \"Ball\"",
            "",
            "[input]",
            "mode = \"auto\"",
            "",
        ]
        .join("\n");
        std::fs::write(&scenario_path, &scenario).expect("write scenario");

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(&scenario_path.to_string_lossy(), &scenario);
        }

        let response = server
            .execute_get_scenario_config(Some(serde_json::json!({
                "uri": Url::from_file_path(&scenario_path)
                    .expect("scenario uri")
                    .to_string(),
            })))
            .await
            .expect("scenario response");

        assert_eq!(
            response.get("ok").and_then(serde_json::Value::as_bool),
            Some(true)
        );
        assert_eq!(
            response
                .get("viewerMode")
                .and_then(serde_json::Value::as_str),
            Some("results_panel")
        );
        assert_eq!(
            response
                .get("inputEnabled")
                .and_then(serde_json::Value::as_bool),
            Some(true)
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

        assert_eq!(compiled.dae.variables.states.len(), 1);
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
                    parameter_overrides: Vec::new(),
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
                            parameter_overrides: Vec::new(),
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
            "stale prepare request should not report solve models"
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
