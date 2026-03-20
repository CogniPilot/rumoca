use super::*;
use futures_util::StreamExt;
use rumoca_session::compile::{reset_session_cache_stats, session_cache_stats};
use serde::Deserialize;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::atomic::Ordering;
use std::time::{SystemTime, UNIX_EPOCH};
use tower_lsp::LspService;

static SESSION_STATS_TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());
static ASYNC_TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

fn run_async_test<F>(future: F)
where
    F: std::future::Future<Output = ()>,
{
    // Session cache/timing instrumentation is process-global. Serialize this
    // test harness so request-local cache assertions are not polluted by
    // unrelated async LSP tests running in parallel in the same binary.
    let _guard = ASYNC_TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime")
        .block_on(future);
}

fn session_stats_test_guard() -> std::sync::MutexGuard<'static, ()> {
    SESSION_STATS_TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
}

fn new_temp_dir(name: &str) -> PathBuf {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("rumoca-{name}-{unique}"));
    std::fs::create_dir_all(&dir).expect("mkdir temp dir");
    dir
}

fn new_test_service() -> LspService<ModelicaLanguageServer> {
    let (service, mut socket) = LspService::new(ModelicaLanguageServer::new);
    tokio::spawn(async move { while socket.next().await.is_some() {} });
    service
}

pub(super) async fn wait_for_library_namespace_cache(
    server: &ModelicaLanguageServer,
) -> Vec<String> {
    for _ in 0..64 {
        let class_names = server
            .session_snapshot()
            .await
            .all_library_class_names_cached();
        if !class_names.is_empty() {
            return class_names;
        }
        tokio::task::yield_now().await;
    }
    panic!("expected background namespace prewarm to populate library class names");
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct LoggedCompletionTimingSummary {
    requested_edit_epoch: u64,
    request_was_stale: bool,
    uri: String,
    #[serde(default)]
    semantic_layer: String,
    #[serde(default)]
    library_completion_prime_ms: u64,
    #[serde(default)]
    needs_resolved_session: bool,
    #[serde(default)]
    ast_fast_path_matched: bool,
    #[serde(default)]
    query_fast_path_matched: bool,
    built_resolved_tree: bool,
    had_resolved_cache_before: bool,
    class_name_count_after_ensure: usize,
    namespace_index_query_hits: u64,
    namespace_index_query_misses: u64,
    file_item_index_query_hits: u64,
    file_item_index_query_misses: u64,
    #[serde(default)]
    declaration_index_query_hits: u64,
    #[serde(default)]
    declaration_index_query_misses: u64,
    #[serde(default)]
    source_set_package_membership_query_hits: u64,
    #[serde(default)]
    source_set_package_membership_query_misses: u64,
    session_cache_delta: rumoca_session::compile::SessionCacheStatsSnapshot,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct LoggedDiagnosticsTimingSummary {
    #[serde(default)]
    requested_edit_epoch: u64,
    #[serde(default)]
    request_was_stale: bool,
    uri: String,
    trigger: String,
    #[serde(default)]
    semantic_layer: String,
    requested_library_load: bool,
    ran_compile: bool,
    session_cache_delta: rumoca_session::compile::SessionCacheStatsSnapshot,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct LoggedNavigationTimingSummary {
    #[serde(default)]
    requested_edit_epoch: u64,
    #[serde(default)]
    request_was_stale: bool,
    uri: String,
    request: String,
    #[serde(default)]
    request_path: String,
    #[serde(default)]
    semantic_layer: String,
    #[serde(default)]
    snapshot_ms: Option<u64>,
    #[serde(default)]
    snapshot_lock_ms: Option<u64>,
    #[serde(default)]
    snapshot_build_ms: Option<u64>,
    #[serde(default)]
    detail: Option<String>,
    #[serde(default)]
    query_ms: Option<u64>,
    #[serde(default)]
    format_ms: Option<u64>,
    built_resolved_tree: bool,
    had_resolved_cache_before: bool,
    session_cache_delta: rumoca_session::compile::SessionCacheStatsSnapshot,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct LoggedStartupTimingSummary {
    initial_library_paths: usize,
    library_paths_changed: bool,
    parse_init_options_ms: u64,
    workspace_root_ms: u64,
    reload_project_config_ms: u64,
    project_discover_ms: u64,
    resolve_library_paths_ms: u64,
    reset_session_ms: u64,
    durable_prewarm_ms: u64,
    #[serde(default)]
    durable_collect_files_ms: u64,
    #[serde(default)]
    durable_hash_inputs_ms: u64,
    #[serde(default)]
    durable_cache_lookup_ms: u64,
    #[serde(default)]
    durable_cache_deserialize_ms: u64,
    #[serde(default)]
    durable_parse_files_ms: u64,
    #[serde(default)]
    durable_validate_layout_ms: u64,
    #[serde(default)]
    durable_cache_write_ms: u64,
    #[serde(default)]
    durable_apply_ms: u64,
    workspace_symbol_prewarm_ms: u64,
    namespace_prewarm_spawn_ms: u64,
    total_ms: u64,
}

fn read_jsonl<T: serde::de::DeserializeOwned>(path: &Path) -> Vec<T> {
    let contents = std::fs::read_to_string(path).expect("timing file should exist");
    contents
        .lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| serde_json::from_str(line).expect("valid timing json"))
        .collect()
}

fn assert_no_model_query_activity(
    delta: rumoca_session::compile::SessionCacheStatsSnapshot,
    context: &str,
) {
    for (stage, hits, misses, builds) in [
        (
            "instantiated_model",
            delta.instantiated_model_cache_hits,
            delta.instantiated_model_cache_misses,
            delta.instantiated_model_builds,
        ),
        (
            "typed_model",
            delta.typed_model_cache_hits,
            delta.typed_model_cache_misses,
            delta.typed_model_builds,
        ),
        (
            "flat_model",
            delta.flat_model_cache_hits,
            delta.flat_model_cache_misses,
            delta.flat_model_builds,
        ),
        (
            "dae_model",
            delta.dae_model_cache_hits,
            delta.dae_model_cache_misses,
            delta.dae_model_builds,
        ),
    ] {
        assert_eq!(hits, 0, "{context} should not hit {stage}");
        assert_eq!(misses, 0, "{context} should not miss {stage}");
        assert_eq!(builds, 0, "{context} should not build {stage}");
    }
}

fn assert_model_query_build_chain(
    delta: rumoca_session::compile::SessionCacheStatsSnapshot,
    context: &str,
) {
    for (stage, misses, builds) in [
        (
            "instantiated_model",
            delta.instantiated_model_cache_misses,
            delta.instantiated_model_builds,
        ),
        (
            "typed_model",
            delta.typed_model_cache_misses,
            delta.typed_model_builds,
        ),
        (
            "flat_model",
            delta.flat_model_cache_misses,
            delta.flat_model_builds,
        ),
        (
            "dae_model",
            delta.dae_model_cache_misses,
            delta.dae_model_builds,
        ),
    ] {
        assert!(
            misses >= 1,
            "{context} should miss the {stage} cache at least once"
        );
        assert!(builds >= 1, "{context} should build the {stage} stage");
    }
}

fn assert_no_semantic_diagnostics_activity(
    delta: rumoca_session::compile::SessionCacheStatsSnapshot,
    context: &str,
) {
    for (stage, hits, misses, builds) in [
        (
            "interface_semantic_diagnostics",
            delta.interface_semantic_diagnostics_cache_hits,
            delta.interface_semantic_diagnostics_cache_misses,
            delta.interface_semantic_diagnostics_builds,
        ),
        (
            "body_semantic_diagnostics",
            delta.body_semantic_diagnostics_cache_hits,
            delta.body_semantic_diagnostics_cache_misses,
            delta.body_semantic_diagnostics_builds,
        ),
        (
            "model_stage_semantic_diagnostics",
            delta.model_stage_semantic_diagnostics_cache_hits,
            delta.model_stage_semantic_diagnostics_cache_misses,
            delta.model_stage_semantic_diagnostics_builds,
        ),
    ] {
        assert_eq!(hits, 0, "{context} should not hit {stage}");
        assert_eq!(misses, 0, "{context} should not miss {stage}");
        assert_eq!(builds, 0, "{context} should not build {stage}");
    }
}

fn hover_text(hover: &Hover) -> String {
    match &hover.contents {
        HoverContents::Markup(markup) => markup.value.clone(),
        HoverContents::Scalar(MarkedString::String(text)) => text.clone(),
        HoverContents::Scalar(MarkedString::LanguageString(text)) => text.value.clone(),
        HoverContents::Array(parts) => parts
            .iter()
            .map(|part| match part {
                MarkedString::String(text) => text.clone(),
                MarkedString::LanguageString(text) => text.value.clone(),
            })
            .collect::<Vec<_>>()
            .join("\n"),
    }
}

const ALIAS_NAVIGATION_SOURCE: &str = r#"package Lib
  block Target
    Real y;
  equation
    y = 1;
  end Target;
end Lib;

model M
  import Alias = Lib.Target;
  Alias a;
equation
  a.y = 1;
end M;
"#;

const CROSS_FILE_ALIAS_LIBRARY_SOURCE: &str = r#"package Lib
  block Target
    Real y;
  equation
    y = 1;
  end Target;
end Lib;
"#;

const CROSS_FILE_ALIAS_ACTIVE_SOURCE: &str = r#"model M
  import Alias = Lib.Target;
  Alias a;
equation
  a.y = 1;
end M;
"#;

const QUALIFIED_PATH_LIBRARY_SOURCE: &str = r#"package Lib
  block Target
    Real y;
  equation
    y = 1;
  end Target;
end Lib;
"#;

const QUALIFIED_PATH_ACTIVE_SOURCE: &str = r#"model M
  Lib.Target a;
equation
  a.y = 1;
end M;
"#;

fn alias_navigation_position() -> Position {
    Position {
        line: 9,
        character: "  import Alias".len() as u32,
    }
}

fn hover_alias_request(uri: &Url) -> HoverParams {
    HoverParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: alias_navigation_position(),
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
    }
}

fn cross_file_alias_definition_request(uri: &Url) -> GotoDefinitionParams {
    GotoDefinitionParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 1,
                character: "  import Alias".len() as u32,
            },
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    }
}

async fn seed_alias_navigation_document(
    server: &ModelicaLanguageServer,
    active_uri: &Url,
    navigation_timing_path: Option<&Path>,
) {
    if let Some(path) = navigation_timing_path {
        *server.navigation_timing_path.write().await = Some(path.to_path_buf());
    }
    let active_key = session_document_uri_key(active_uri);
    let mut session = server.session.write().await;
    session.update_document(&active_key, ALIAS_NAVIGATION_SOURCE);
}

fn cross_file_alias_hover_request(uri: &Url) -> HoverParams {
    HoverParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 1,
                character: "  import Alias".len() as u32,
            },
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
    }
}

async fn seed_cross_file_alias_navigation_document(
    server: &ModelicaLanguageServer,
    library_path: &Path,
    active_uri: &Url,
    navigation_timing_path: Option<&Path>,
) {
    if let Some(path) = navigation_timing_path {
        *server.navigation_timing_path.write().await = Some(path.to_path_buf());
    }
    let active_key = session_document_uri_key(active_uri);
    let mut session = server.session.write().await;
    session.update_document(
        &library_path.to_string_lossy(),
        CROSS_FILE_ALIAS_LIBRARY_SOURCE,
    );
    session.update_document(&active_key, CROSS_FILE_ALIAS_ACTIVE_SOURCE);
}

fn qualified_path_navigation_position() -> Position {
    Position {
        line: 1,
        character: "  Lib.".len() as u32,
    }
}

fn qualified_path_hover_request(uri: &Url) -> HoverParams {
    HoverParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: qualified_path_navigation_position(),
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
    }
}

fn qualified_path_definition_request(uri: &Url) -> GotoDefinitionParams {
    GotoDefinitionParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: qualified_path_navigation_position(),
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    }
}

async fn seed_cross_file_qualified_path_document(
    server: &ModelicaLanguageServer,
    library_path: &Path,
    active_uri: &Url,
    navigation_timing_path: Option<&Path>,
) {
    if let Some(path) = navigation_timing_path {
        *server.navigation_timing_path.write().await = Some(path.to_path_buf());
    }
    let active_key = session_document_uri_key(active_uri);
    let mut session = server.session.write().await;
    session.update_document(
        &library_path.to_string_lossy(),
        QUALIFIED_PATH_LIBRARY_SOURCE,
    );
    session.update_document(&active_key, QUALIFIED_PATH_ACTIVE_SOURCE);
}

const SURFACE_SOURCE: &str = r#"package Lib
  model Helper
    parameter Real gain = 1;
    output Real y;
  equation
    y = gain;
  end Helper;
end Lib;

// docs
// https://example.com/docs
model M
  import Alias = Lib.Helper;
  Real arr[2, 3];
  String local = "./Lib/package.mo";
  Alias helperInst;
equation
  helperInst.y = sin(helperInst.gain);
end M;
"#;

const BROKEN_ACTION_SOURCE: &str = r#"model Broken
  Real x(startdt = 1.0);
equation
  der(x) = -x;
end Broken;
"#;

fn surface_component_position() -> Position {
    Position {
        line: 15,
        character: "  Alias helperInst".len() as u32,
    }
}

fn surface_usage_position() -> Position {
    Position {
        line: 17,
        character: "  helperInst".len() as u32,
    }
}

fn surface_signature_position() -> Position {
    Position {
        line: 17,
        character: "  helperInst.y = sin(".len() as u32,
    }
}

fn local_hover_request(uri: &Url) -> HoverParams {
    HoverParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: surface_component_position(),
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
    }
}

fn local_definition_request(uri: &Url) -> GotoDefinitionParams {
    GotoDefinitionParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: surface_usage_position(),
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    }
}

async fn seed_surface_document(server: &ModelicaLanguageServer, uri: &Url) {
    let uri_key = session_document_uri_key(uri);
    let mut session = server.session.write().await;
    session.update_document(&uri_key, SURFACE_SOURCE);
}

#[test]
fn work_lanes_keep_interactive_and_strict_paths_independent() {
    run_async_test(async {
        let lanes = ServerWorkLanes::default();
        let _strict_guard = lanes.strict.lock().await;
        let interactive_guard = tokio::time::timeout(
            std::time::Duration::from_millis(50),
            lanes.interactive.lock(),
        )
        .await;
        assert!(
            interactive_guard.is_ok(),
            "interactive lane should not wait on strict lane"
        );
    });
}

fn write_test_library(root: &Path, package_name: &str) -> PathBuf {
    let lib = root.join(package_name);
    std::fs::create_dir_all(&lib).expect("mkdir test library");
    std::fs::write(
        lib.join("package.mo"),
        format!(
            "package {package_name}\n  model A\n    Real x;\n  equation\n    der(x) = 1;\n  end A;\nend {package_name};\n"
        ),
    )
    .expect("write package.mo");
    lib
}

fn cached_msl_library_root() -> Option<PathBuf> {
    let cache_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join("target/msl");
    let root = cache_dir
        .join("ModelicaStandardLibrary-4.1.0")
        .join("Modelica 4.1.0");
    root.is_dir().then_some(root)
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
fn library_completion_prefix_detection_handles_qualified_references() {
    let source = "model M\n  Modelica.Electrical.Analog.Basic.Ground g;\nend M;\n";
    let pos = Position {
        line: 1,
        character: "  Modelica.".len() as u32,
    };
    assert_eq!(
        extract_library_completion_prefix(source, pos),
        Some("Modelica.".to_string())
    );
}

#[test]
fn library_completion_prefix_detection_ignores_lowercase_component_members() {
    let source = "model M\n  pid.\nend M;\n";
    let pos = Position {
        line: 1,
        character: "  pid.".len() as u32,
    };
    assert_eq!(extract_library_completion_prefix(source, pos), None);
}

#[test]
fn library_indexing_started_message_explains_cpu_use() {
    let message = library_indexing_started_message(
        "/opt/modelica/Modelica",
        LibraryIndexingReason::CompletionImports,
    );
    assert!(message.contains("Indexing library Modelica"));
    assert!(message.contains("This may use CPU"));
    assert!(message.contains("editor completion/imports"));
    assert!(message.contains("Path: /opt/modelica/Modelica"));
}

#[test]
fn library_indexing_finished_message_reports_outcome() {
    let message = library_indexing_finished_message(
        "/opt/modelica/Modelica",
        LibraryIndexingReason::SaveDiagnostics.label(),
        2510,
        2510,
        LibraryCacheStatus::Miss,
    );
    assert!(message.contains("Indexing done for library Modelica"));
    assert!(message.contains("save diagnostics"));
    assert!(message.contains("2510 files indexed"));
    assert!(message.contains("cache miss"));
}

#[test]
fn library_indexing_failed_message_reports_reason() {
    let message = library_indexing_failed_message(
        "/opt/modelica/Modelica",
        LibraryIndexingReason::SimulationCompile,
        "missing package.mo",
    );
    assert!(message.contains("Indexing failed for library Modelica"));
    assert!(message.contains("simulation compile after library edits"));
    assert!(message.contains("missing package.mo"));
}

#[test]
fn library_display_name_prefers_inferred_roots_for_wrapped_layout() {
    let wrapped = new_temp_dir("wrapped-library-layout");
    let modelica = wrapped.join("Modelica 4.1.0");
    let services = wrapped.join("ModelicaServices 4.1.0");
    std::fs::create_dir_all(&modelica).expect("mkdir Modelica");
    std::fs::create_dir_all(&services).expect("mkdir ModelicaServices");
    std::fs::write(
        modelica.join("package.mo"),
        "package Modelica\nend Modelica;\n",
    )
    .expect("write Modelica package");
    std::fs::write(
        services.join("package.mo"),
        "package ModelicaServices\nend ModelicaServices;\n",
    )
    .expect("write ModelicaServices package");

    let display_name = library_display_name(wrapped.to_string_lossy().as_ref());
    assert_eq!(display_name, "Modelica, ModelicaServices");
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
        let service = new_test_service();
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
                SourceRootKind::Library,
                path_key,
                Some("active.mo"),
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
                SourceRootKind::Library,
                path_key,
                Some("active.mo"),
                Vec::new(),
                1,
            )
            .await;
        assert_eq!(
            current_apply.map(|applied| applied.inserted_file_count),
            Some(0)
        );
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
        let service = new_test_service();
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

#[test]
fn session_document_uri_key_uses_decoded_file_path() {
    let path = std::env::temp_dir()
        .join("Modelica Standard Library")
        .join("Blocks")
        .join("Continuous.mo");
    let uri = Url::from_file_path(&path).expect("file uri");
    let key = session_document_uri_key(&uri);
    assert_eq!(PathBuf::from(&key), path);
    assert!(
        !key.contains("%20"),
        "session key should be filesystem path, not URL-encoded: {key}"
    );
}

#[test]
fn project_config_uri_detection_handles_file_paths_with_spaces() {
    let path = std::env::temp_dir()
        .join("workspace with spaces")
        .join(".rumoca")
        .join("project.toml");
    let uri = Url::from_file_path(path).expect("file uri");
    assert!(is_project_config_uri(&uri));
}

#[test]
fn initial_library_paths_are_classified_as_durable_roots() {
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();
        let durable_root = std::env::temp_dir().join("rumoca-msl-root");
        let durable_path = durable_root.to_string_lossy().to_string();
        *server.initial_library_paths.write().await = vec![durable_path.clone()];

        assert_eq!(
            server.library_source_root_kind(&durable_path).await,
            SourceRootKind::DurableLibrary
        );
        assert_eq!(
            server.library_source_root_kind("/tmp/project-lib").await,
            SourceRootKind::Library
        );
    });
}

#[test]
fn simulation_doc_for_compile_snapshot_filters_workspace_documents() {
    let focus_uri = "/tmp/focus.mo";
    let focus_key = canonical_path_key(focus_uri);
    let parsed = ast::StoredDefinition::default();
    let mut session = Session::new(SessionConfig::default());
    session.replace_parsed_source_set(
        "library::/opt/msl",
        SourceRootKind::DurableLibrary,
        vec![(
            "/opt/msl/Modelica/Blocks/Continuous.mo".to_string(),
            parsed.clone(),
        )],
        None,
    );

    let focus_doc = Document::from_parsed(
        focus_uri.to_string(),
        "model Focus end Focus;".to_string(),
        parsed.clone(),
    );
    let other_workspace_doc = Document::from_parsed(
        "/tmp/other.mo".to_string(),
        "model Other end Other;".to_string(),
        parsed.clone(),
    );
    let library_doc = Document::from_parsed(
        "/opt/msl/Modelica/Blocks/Continuous.mo".to_string(),
        String::new(),
        parsed,
    );

    let snapshot = session.snapshot();

    let focus = simulation_doc_for_compile_snapshot(&snapshot, focus_uri, &focus_doc, &focus_key)
        .expect("focus doc should be accepted")
        .expect("focus doc should be included");
    assert!(focus.0, "focus document marker should be true");

    let other = simulation_doc_for_compile_snapshot(
        &snapshot,
        &other_workspace_doc.uri,
        &other_workspace_doc,
        &focus_key,
    )
    .expect("other workspace doc should be evaluated");
    assert!(
        other.is_none(),
        "non-focus workspace docs should be excluded"
    );

    let library =
        simulation_doc_for_compile_snapshot(&snapshot, &library_doc.uri, &library_doc, &focus_key)
            .expect("library doc should be accepted")
            .expect("library doc should be included");
    assert!(
        !library.0,
        "library docs should be included without marking as focus"
    );
}

#[test]
fn live_diagnostics_do_not_load_libraries() {
    run_async_test(async {
        let temp = new_temp_dir("live-library-load");
        let library_path = write_test_library(&temp, "Lib");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_source = "model Active\n  Lib.A a;\nend Active;\n";
        let library_key = canonical_path_key(library_path.to_string_lossy().as_ref());

        let service = new_test_service();
        let server = service.inner();
        *server.library_paths.write().await = vec![library_path.to_string_lossy().to_string()];

        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: active_uri,
                    language_id: "modelica".to_string(),
                    version: 1,
                    text: active_source.to_string(),
                },
            })
            .await;

        assert!(
            !server.loaded_libraries.read().await.contains(&library_key),
            "live diagnostics should not load libraries on first open"
        );
    });
}

#[test]
fn library_load_primitive_loads_libraries() {
    run_async_test(async {
        let temp = new_temp_dir("save-library-load");
        let library_path = write_test_library(&temp, "Lib");
        let active_path = temp.join("active.mo");
        let active_path_str = active_path.to_string_lossy().to_string();
        let library_key = canonical_path_key(library_path.to_string_lossy().as_ref());
        let source_set_id = library_source_set_id(library_path.to_string_lossy().as_ref());

        let service = new_test_service();
        let server = service.inner();
        let outcome = server
            .load_library_source_set_if_current(
                library_path.to_string_lossy().as_ref(),
                &library_key,
                &source_set_id,
                Some(active_path_str.as_str()),
                server.current_library_state_epoch(),
                LibraryIndexingReason::CompletionImports,
            )
            .await;
        let outcome = outcome
            .expect("library load should succeed")
            .expect("library should be applied");

        assert!(
            server.loaded_libraries.read().await.contains(&library_key),
            "library-load primitive should mark the library as loaded"
        );
        assert!(
            outcome.indexed_file_count >= 1,
            "library-load primitive should index at least one file"
        );
    });
}

#[test]
#[ignore = "requires cached MSL under target/msl"]
fn msl_live_diagnostics_do_not_load_libraries_when_source_requires_them() {
    run_async_test(async {
        let msl_root = cached_msl_library_root().expect("cached MSL should exist");
        let temp = new_temp_dir("msl-live-diagnostics");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_source = "model Active\n  import Modelica;\nend Active;\n";
        let library_key = canonical_path_key(msl_root.to_string_lossy().as_ref());

        let service = new_test_service();
        let server = service.inner();
        *server.library_paths.write().await = vec![msl_root.to_string_lossy().to_string()];

        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: active_uri,
                    language_id: "modelica".to_string(),
                    version: 1,
                    text: active_source.to_string(),
                },
            })
            .await;

        assert!(
            !server.loaded_libraries.read().await.contains(&library_key),
            "live diagnostics should not load cached MSL during typing"
        );
    });
}

#[test]
fn completion_member_lookup_uses_query_layer_without_semantic_navigation() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        reset_session_cache_stats();
        let temp = new_temp_dir("completion-semantic-navigation");
        let library_path = temp.join("lib.mo");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let library_source = r#"package Lib
  model Plane
  Real x, y, theta;
equation
  der(x) = cos(theta);
  der(y) = sin(theta);
  der(theta) = 1;
end Plane;
end Lib;
"#;

        let active_source = r#"model Sim
  import Lib.Plane;
  Plane p1, p2;
equation
  p1.x = 1;
end Sim;
"#;
        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(&library_path.to_string_lossy(), library_source);
            session.update_document(&active_path.to_string_lossy(), active_source);
        }

        let completion = server
            .completion(CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: active_uri },
                    position: Position {
                        line: 4,
                        character: "  p1.".len() as u32,
                    },
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: None,
            })
            .await
            .expect("completion request should succeed");

        let items = match completion {
            Some(CompletionResponse::Array(items)) => items,
            other => panic!("expected array completion response, got {other:?}"),
        };

        assert!(
            items.iter().any(|item| item.label == "x"),
            "expected member completion from the query layer"
        );
        assert!(
            !server
                .session
                .read()
                .await
                .has_semantic_navigation_cached("Sim"),
            "completion should not build a semantic navigation artifact when the query layer can answer"
        );
        assert!(
            !server.session.read().await.has_standard_resolved_cached(),
            "completion should not build the standard resolved session"
        );
        assert!(
            !server.session.read().await.has_resolved_cached(),
            "completion should stay off resolved caches entirely on the query fast path"
        );
    });
}

#[test]
fn completion_member_lookup_uses_query_layer_for_local_alias() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        let path = new_temp_dir("completion-local-alias").join("active.mo");
        let uri = Url::from_file_path(&path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        seed_surface_document(server, &uri).await;

        let completion = server
            .completion(CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position {
                        line: 17,
                        character: "  helperInst.".len() as u32,
                    },
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: None,
            })
            .await
            .expect("completion request should succeed");
        let items = match completion {
            Some(CompletionResponse::Array(items)) => items,
            other => panic!("expected array completion response, got {other:?}"),
        };
        assert!(
            items.iter().any(|item| item.label == "gain"),
            "expected local alias member `gain` completion"
        );
    });
}

#[test]
fn completion_preparation_marks_stale_requests_after_edit_epoch_bump() {
    run_async_test(async {
        let temp = new_temp_dir("completion-preparation-stale");
        let active_path = temp.join("active.mo");
        let uri = Url::from_file_path(&active_path).expect("file uri");
        let uri_path = session_document_uri_key(&uri);
        let source = "model Active\n  Lib.\nend Active;\n";
        let service = new_test_service();
        let server = service.inner();
        let request_edit_epoch = server.completion_mutation_epoch();
        server
            .completion_mutation_epoch
            .fetch_add(1, Ordering::AcqRel);

        let preparation = server
            .prepare_completion(
                source,
                Position {
                    line: 1,
                    character: "  Lib.".len() as u32,
                },
                &uri_path,
                request_edit_epoch,
            )
            .await;

        let expected_prefix = extract_library_completion_prefix(
            source,
            Position {
                line: 1,
                character: "  Lib.".len() as u32,
            },
        );
        assert!(preparation.request_was_stale);
        assert_eq!(preparation.source_library_load_ms, 0);
        assert_eq!(preparation.completion_library_load_ms, 0);
        assert_eq!(preparation.resolved_build_ms, None);
        assert!(!preparation.built_resolved_tree);
        assert!(!preparation.had_resolved_cache_before);
        assert_eq!(preparation.completion_prefix, expected_prefix);
    });
}

#[test]
fn analysis_request_token_marks_stale_after_session_revision_bump() {
    run_async_test(async {
        let temp = new_temp_dir("analysis-request-stale-revision");
        let active_path = temp.join("active.mo");
        let uri = Url::from_file_path(&active_path).expect("file uri");
        let uri_path = session_document_uri_key(&uri);
        let service = new_test_service();
        let server = service.inner();
        seed_surface_document(server, &uri).await;

        let token = server.begin_analysis_request().await;
        {
            let mut session = server.session.write().await;
            session.update_document(
                &uri_path,
                r#"package Lib
  model Helper2
    parameter Real gain = 1;
    output Real y;
  equation
    y = gain;
  end Helper2;
end Lib;

model M
  import Alias = Lib.Helper2;
  Real arr[2, 3];
  String local = "./Lib/package.mo";
  Alias helperInst;
equation
  helperInst.y = sin(helperInst.gain);
end M;
"#,
            );
        }

        assert!(
            server.analysis_request_is_stale(token).await,
            "session revision changes should stale outstanding analysis requests"
        );
    });
}

#[test]
fn hover_alias_uses_query_layer_without_semantic_navigation() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        reset_session_cache_stats();
        let temp = new_temp_dir("hover-alias-ast");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");

        let service = new_test_service();
        let server = service.inner();
        seed_alias_navigation_document(server, &active_uri, None).await;

        let before = session_cache_stats();
        let hover = server
            .hover(hover_alias_request(&active_uri))
            .await
            .expect("hover should succeed")
            .expect("hover should return a payload");
        let delta = session_cache_stats().delta_since(before);
        let HoverContents::Markup(contents) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            contents.value.contains("block Target"),
            "expected imported alias hover from the query layer, got: {}",
            contents.value
        );
        assert_eq!(
            delta.semantic_navigation_builds, 0,
            "import alias hover should not build semantic navigation"
        );
        assert_eq!(
            delta.semantic_navigation_cache_hits, 0,
            "import alias hover should not touch the navigation cache"
        );
        assert_eq!(
            delta.strict_resolved_builds, 0,
            "import alias hover should not build strict resolved state"
        );
        assert!(
            !server.session.read().await.has_standard_resolved_cached(),
            "import alias hover should stay off the standard resolved session"
        );
    });
}

#[test]
fn hover_imported_class_uses_query_layer_without_semantic_navigation() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        reset_session_cache_stats();
        let temp = new_temp_dir("hover-semantic-navigation");
        let library_path = temp.join("lib.mo");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");

        let service = new_test_service();
        let server = service.inner();
        seed_cross_file_alias_navigation_document(server, &library_path, &active_uri, None).await;

        let before_first = session_cache_stats();
        let first = server
            .hover(cross_file_alias_hover_request(&active_uri))
            .await
            .expect("cold hover should succeed")
            .expect("cold hover should return a payload");
        let after_first = session_cache_stats();
        let first_delta = after_first.delta_since(before_first);
        let HoverContents::Markup(first_contents) = first.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            first_contents.value.contains("block Target"),
            "expected imported class hover from query layer, got: {}",
            first_contents.value
        );
        assert_eq!(
            first_delta.semantic_navigation_builds, 0,
            "cold hover should stay off semantic navigation"
        );
        assert_eq!(
            first_delta.strict_resolved_builds, 0,
            "cold hover should stay off strict resolved recovery"
        );
        assert!(
            !server
                .session
                .read()
                .await
                .has_semantic_navigation_cached("M"),
            "cold hover should not populate the active-model navigation cache"
        );

        let second = server
            .hover(cross_file_alias_hover_request(&active_uri))
            .await
            .expect("warm hover should succeed")
            .expect("warm hover should return a payload");
        let second_delta = session_cache_stats().delta_since(after_first);
        let HoverContents::Markup(second_contents) = second.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            second_contents.value.contains("block Target"),
            "warm hover should preserve imported class hover"
        );
        assert_eq!(
            second_delta.semantic_navigation_builds, 0,
            "warm hover should stay off semantic navigation"
        );
        assert_eq!(
            second_delta.semantic_navigation_cache_hits, 0,
            "warm hover should not touch the semantic navigation cache"
        );
        assert_eq!(
            second_delta.strict_resolved_builds, 0,
            "warm hover should stay off strict resolved recovery"
        );
        assert!(
            !server.session.read().await.has_standard_resolved_cached(),
            "hover should never populate the standard resolved cache"
        );
    });
}

#[test]
fn goto_definition_imported_class_uses_query_layer_without_semantic_navigation() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        reset_session_cache_stats();
        let temp = new_temp_dir("goto-semantic-navigation");
        let library_path = temp.join("lib.mo");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");

        let service = new_test_service();
        let server = service.inner();
        seed_cross_file_alias_navigation_document(server, &library_path, &active_uri, None).await;

        let before_first = session_cache_stats();
        let first = server
            .goto_definition(cross_file_alias_definition_request(&active_uri))
            .await
            .expect("cold goto should succeed")
            .expect("cold goto should resolve a target");
        let after_first = session_cache_stats();
        let first_delta = after_first.delta_since(before_first);
        match first {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(
                    location.uri,
                    Url::from_file_path(&library_path).expect("file uri")
                );
                assert_eq!(location.range.start.line, 1);
            }
            other => panic!("expected scalar goto response, got {other:?}"),
        }
        assert_eq!(
            first_delta.semantic_navigation_builds, 0,
            "cold goto should stay off semantic navigation"
        );
        assert_eq!(
            first_delta.strict_resolved_builds, 0,
            "cold goto should stay off strict resolved recovery"
        );
        assert!(
            !server
                .session
                .read()
                .await
                .has_semantic_navigation_cached("M"),
            "cold goto should not populate the active-model navigation cache"
        );

        let second = server
            .goto_definition(cross_file_alias_definition_request(&active_uri))
            .await
            .expect("warm goto should succeed")
            .expect("warm goto should resolve a target");
        let second_delta = session_cache_stats().delta_since(after_first);
        match second {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(
                    location.uri,
                    Url::from_file_path(&library_path).expect("file uri")
                );
                assert_eq!(location.range.start.line, 1);
            }
            other => panic!("expected scalar goto response, got {other:?}"),
        }
        assert_eq!(
            second_delta.semantic_navigation_builds, 0,
            "warm goto should stay off semantic navigation"
        );
        assert_eq!(
            second_delta.semantic_navigation_cache_hits, 0,
            "warm goto should not touch the semantic navigation cache"
        );
        assert_eq!(
            second_delta.strict_resolved_builds, 0,
            "warm goto should stay off strict resolved recovery"
        );
        assert!(
            !server.session.read().await.has_standard_resolved_cached(),
            "goto definition should never populate the standard resolved cache"
        );
    });
}

#[test]
fn local_hover_uses_query_layer_without_semantic_navigation() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        reset_session_cache_stats();
        let temp = new_temp_dir("hover-local-ast");
        let active_path = temp.join("surface.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");

        let service = new_test_service();
        let server = service.inner();
        seed_surface_document(server, &active_uri).await;

        let before = session_cache_stats();
        let hover = server
            .hover(local_hover_request(&active_uri))
            .await
            .expect("local hover should succeed")
            .expect("local hover should return a payload");
        let delta = session_cache_stats().delta_since(before);
        let HoverContents::Markup(contents) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            contents.value.contains("Alias helperInst"),
            "expected component hover from the query layer, got: {}",
            contents.value
        );
        assert_eq!(
            delta.semantic_navigation_builds, 0,
            "local hover should not build semantic navigation"
        );
        assert_eq!(
            delta.semantic_navigation_cache_hits, 0,
            "local hover should not touch the navigation cache"
        );
        assert_eq!(
            delta.strict_resolved_builds, 0,
            "local hover should not build strict resolved state"
        );
        assert!(
            !server.session.read().await.has_standard_resolved_cached(),
            "local hover should stay off the standard resolved session"
        );
    });
}

#[test]
fn local_goto_definition_uses_query_layer_without_semantic_navigation() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        reset_session_cache_stats();
        let temp = new_temp_dir("goto-local-ast");
        let active_path = temp.join("surface.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");

        let service = new_test_service();
        let server = service.inner();
        seed_surface_document(server, &active_uri).await;

        let before = session_cache_stats();
        let response = server
            .goto_definition(local_definition_request(&active_uri))
            .await
            .expect("local goto should succeed")
            .expect("local goto should resolve a target");
        let delta = session_cache_stats().delta_since(before);
        match response {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.range.start.line, 15);
            }
            other => panic!("expected scalar goto response, got {other:?}"),
        }
        assert_eq!(
            delta.semantic_navigation_builds, 0,
            "local goto should not build semantic navigation"
        );
        assert_eq!(
            delta.semantic_navigation_cache_hits, 0,
            "local goto should not touch the navigation cache"
        );
        assert_eq!(
            delta.strict_resolved_builds, 0,
            "local goto should not build strict resolved state"
        );
        assert!(
            !server.session.read().await.has_standard_resolved_cached(),
            "local goto should stay off the standard resolved session"
        );
    });
}

#[test]
fn hover_on_qualified_type_path_resolves_cross_file_target() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        reset_session_cache_stats();
        let temp = new_temp_dir("hover-qualified-path");
        let library_path = temp.join("lib.mo");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");

        let service = new_test_service();
        let server = service.inner();
        seed_cross_file_qualified_path_document(server, &library_path, &active_uri, None).await;

        let before = session_cache_stats();
        let hover = server
            .hover(qualified_path_hover_request(&active_uri))
            .await
            .expect("hover should succeed")
            .expect("hover should return a payload");
        let delta = session_cache_stats().delta_since(before);
        let HoverContents::Markup(contents) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            contents.value.contains("block Target"),
            "expected qualified type-path hover to resolve the library class, got: {}",
            contents.value
        );
        assert_eq!(
            delta.semantic_navigation_builds, 0,
            "qualified type-path hover should resolve from parsed library documents"
        );
        assert_eq!(
            delta.strict_resolved_builds, 0,
            "qualified type-path hover should avoid strict resolved state"
        );
        assert!(
            !server.session.read().await.has_standard_resolved_cached(),
            "qualified type-path hover should avoid the standard resolved session"
        );
    });
}

#[test]
fn goto_definition_on_qualified_type_path_resolves_cross_file_target() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        reset_session_cache_stats();
        let temp = new_temp_dir("definition-qualified-path");
        let library_path = temp.join("lib.mo");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");

        let service = new_test_service();
        let server = service.inner();
        seed_cross_file_qualified_path_document(server, &library_path, &active_uri, None).await;

        let before = session_cache_stats();
        let definition = server
            .goto_definition(qualified_path_definition_request(&active_uri))
            .await
            .expect("goto-definition should succeed")
            .expect("goto-definition should resolve a target");
        let delta = session_cache_stats().delta_since(before);
        let GotoDefinitionResponse::Scalar(location) = definition else {
            panic!("expected scalar goto-definition response");
        };
        assert_eq!(
            location.uri,
            Url::from_file_path(&library_path).expect("library uri"),
            "expected qualified type-path goto-definition to jump to the library file"
        );
        assert_eq!(
            delta.semantic_navigation_builds, 0,
            "qualified type-path goto-definition should resolve from parsed library documents"
        );
        assert_eq!(
            delta.strict_resolved_builds, 0,
            "qualified type-path goto-definition should avoid strict resolved state"
        );
        assert!(
            !server.session.read().await.has_standard_resolved_cached(),
            "qualified type-path goto-definition should avoid the standard resolved session"
        );
    });
}

#[test]
fn initialize_advertises_supported_capabilities_and_tracks_workspace_root() {
    run_async_test(async {
        let workspace_root = new_temp_dir("initialize-capabilities");
        let library_root = write_test_library(&workspace_root, "InitLib");
        let library_path = library_root.to_string_lossy().to_string();
        let workspace_uri = Url::from_directory_path(&workspace_root).expect("workspace uri");
        let service = new_test_service();
        let server = service.inner();

        let result = server
            .initialize(InitializeParams {
                root_uri: Some(workspace_uri.clone()),
                initialization_options: Some(serde_json::json!({
                    "modelicaPath": [library_path]
                })),
                workspace_folders: Some(vec![WorkspaceFolder {
                    uri: workspace_uri,
                    name: "workspace".to_string(),
                }]),
                ..InitializeParams::default()
            })
            .await
            .expect("initialize should succeed");

        let capabilities = result.capabilities;
        assert_eq!(
            capabilities.text_document_sync,
            Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL))
        );
        assert_eq!(
            capabilities.hover_provider,
            Some(HoverProviderCapability::Simple(true))
        );
        assert_eq!(
            capabilities.document_symbol_provider,
            Some(OneOf::Left(true))
        );
        assert_eq!(capabilities.definition_provider, Some(OneOf::Left(true)));
        assert_eq!(capabilities.references_provider, Some(OneOf::Left(true)));
        assert_eq!(
            capabilities.workspace_symbol_provider,
            Some(OneOf::Left(true))
        );
        assert_eq!(
            capabilities.folding_range_provider,
            Some(FoldingRangeProviderCapability::Simple(true))
        );
        assert_eq!(
            capabilities.document_formatting_provider,
            Some(OneOf::Left(true))
        );
        assert!(
            capabilities.completion_provider.is_some(),
            "completion provider should be advertised"
        );
        assert!(
            capabilities.signature_help_provider.is_some(),
            "signature help should be advertised"
        );
        assert!(
            capabilities.semantic_tokens_provider.is_some(),
            "semantic tokens should be advertised"
        );
        assert!(
            capabilities.code_lens_provider.is_some(),
            "code lens should be advertised"
        );
        assert_eq!(
            capabilities
                .code_lens_provider
                .as_ref()
                .and_then(|options| options.resolve_provider),
            Some(true),
            "code lens resolve should be advertised"
        );
        assert!(
            capabilities.code_action_provider.is_some(),
            "code actions should be advertised"
        );
        assert!(
            capabilities.document_link_provider.is_some(),
            "document links should be advertised"
        );
        assert!(
            capabilities.execute_command_provider.is_some(),
            "execute command should be advertised"
        );
        assert_eq!(
            capabilities.inlay_hint_provider, None,
            "inlay hints stay disabled until the selective mode is re-enabled"
        );
        assert_eq!(
            server.workspace_root.read().await.as_ref(),
            Some(&workspace_root)
        );
        assert_eq!(
            *server.initial_library_paths.read().await,
            vec![library_path.clone()]
        );

        server.initialized(InitializedParams {}).await;
        server.shutdown().await.expect("shutdown should succeed");
    });
}

#[test]
fn initialize_prewarms_configured_durable_libraries() {
    run_async_test(async {
        let workspace_root = new_temp_dir("initialize-durable-prewarm");
        let library_root = write_test_library(&workspace_root, "InitLib");
        let library_path = library_root.to_string_lossy().to_string();
        let library_key = canonical_path_key(&library_path);
        let workspace_uri = Url::from_directory_path(&workspace_root).expect("workspace uri");
        let service = new_test_service();
        let server = service.inner();

        server
            .initialize(InitializeParams {
                root_uri: Some(workspace_uri),
                initialization_options: Some(serde_json::json!({
                    "modelicaPath": [library_path]
                })),
                ..InitializeParams::default()
            })
            .await
            .expect("initialize should succeed");

        let class_names = wait_for_library_namespace_cache(server).await;
        assert!(
            server.loaded_libraries.read().await.contains(&library_key),
            "initialize should prewarm configured durable roots before the first request"
        );
        assert!(
            class_names.iter().any(|name| name == "InitLib.A"),
            "initialize should also prewarm the durable library namespace cache"
        );
    });
}

#[test]
fn initialize_writes_startup_timing_breakdown() {
    run_async_test(async {
        let workspace_root = new_temp_dir("initialize-startup-timing");
        let library_root = write_test_library(&workspace_root, "InitLib");
        let library_path = library_root.to_string_lossy().to_string();
        let workspace_uri = Url::from_directory_path(&workspace_root).expect("workspace uri");
        let timing_path = workspace_root.join("startup-timings.jsonl");
        let service = new_test_service();
        let server = service.inner();
        *server.startup_timing_path.write().await = Some(timing_path.clone());

        server
            .initialize(InitializeParams {
                root_uri: Some(workspace_uri.clone()),
                initialization_options: Some(serde_json::json!({
                    "modelicaPath": [library_path]
                })),
                workspace_folders: Some(vec![WorkspaceFolder {
                    uri: workspace_uri,
                    name: "workspace".to_string(),
                }]),
                ..InitializeParams::default()
            })
            .await
            .expect("initialize should succeed");

        let entries: Vec<LoggedStartupTimingSummary> = read_jsonl(&timing_path);
        assert_eq!(entries.len(), 1, "expected one startup timing entry");
        let entry = &entries[0];
        assert_eq!(entry.initial_library_paths, 1);
        assert!(
            entry.library_paths_changed,
            "initialize should reset/prewarm when configured library paths change"
        );
        assert!(
            entry.total_ms >= entry.reload_project_config_ms,
            "initialize total should include reload_project_config"
        );
        assert!(
            entry.reload_project_config_ms >= entry.project_discover_ms,
            "reload_project_config should dominate project discovery"
        );
        assert!(
            entry.reload_project_config_ms >= entry.resolve_library_paths_ms,
            "reload_project_config should include library path resolution"
        );
        assert!(
            entry.reload_project_config_ms >= entry.reset_session_ms,
            "reload_project_config should include session reset when paths change"
        );
        assert!(
            entry.reload_project_config_ms >= entry.durable_prewarm_ms,
            "reload_project_config should include durable library prewarm"
        );
        let durable_substeps_ms = entry.durable_collect_files_ms
            + entry.durable_hash_inputs_ms
            + entry.durable_cache_lookup_ms
            + entry.durable_cache_deserialize_ms
            + entry.durable_parse_files_ms
            + entry.durable_validate_layout_ms
            + entry.durable_cache_write_ms
            + entry.durable_apply_ms;
        assert!(
            entry.durable_prewarm_ms >= durable_substeps_ms,
            "durable prewarm should dominate its measured substeps"
        );
        assert!(
            entry.reload_project_config_ms >= entry.workspace_symbol_prewarm_ms,
            "reload_project_config should include workspace symbol prewarm"
        );
        assert!(
            entry.parse_init_options_ms <= entry.total_ms
                && entry.workspace_root_ms <= entry.total_ms
                && entry.namespace_prewarm_spawn_ms <= entry.reload_project_config_ms,
            "startup breakdown should stay internally consistent"
        );
    });
}

#[test]
fn did_open_change_and_close_workspace_document_update_session_view() {
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();
        let path = new_temp_dir("lsp-open-change-close").join("active.mo");
        let uri = Url::from_file_path(&path).expect("file uri");

        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "modelica".to_string(),
                    version: 1,
                    text: "model First\nend First;\n".to_string(),
                },
            })
            .await;

        let after_open = server
            .symbol(WorkspaceSymbolParams {
                query: "First".to_string(),
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("workspace symbol should succeed")
            .expect("workspace symbol response");
        assert!(
            after_open.iter().any(|symbol| symbol.name == "First"),
            "did_open should make the workspace symbol query see the document"
        );

        server
            .did_change(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: uri.clone(),
                    version: 2,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: "model Second\nend Second;\n".to_string(),
                }],
            })
            .await;

        let after_change = server
            .symbol(WorkspaceSymbolParams {
                query: "Second".to_string(),
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("workspace symbol should succeed")
            .expect("workspace symbol response");
        assert!(
            after_change.iter().any(|symbol| symbol.name == "Second"),
            "did_change should refresh the indexed workspace symbols"
        );

        server
            .did_close(DidCloseTextDocumentParams {
                text_document: TextDocumentIdentifier { uri },
            })
            .await;

        let after_close = server
            .symbol(WorkspaceSymbolParams {
                query: "Second".to_string(),
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("workspace symbol should succeed")
            .expect("workspace symbol response");
        assert!(
            after_close.is_empty(),
            "did_close should remove normal workspace documents from the session"
        );
    });
}

mod workspace_query_tests;

mod diagnostics_timing_tests;
mod editor_surface_tests;
mod library_refresh_tests;
mod multi_library_completion_tests;
mod namespace_prewarm_tests;
mod simulation_surface_tests;
