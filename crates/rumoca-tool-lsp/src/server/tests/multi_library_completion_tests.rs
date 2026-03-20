use super::*;
use rumoca_session::compile::reset_session_cache_stats;
use serde::Deserialize;
use std::path::Path;

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct LoggedCompletionTimingSummary {
    requested_edit_epoch: u64,
    request_was_stale: bool,
    #[serde(default)]
    semantic_layer: String,
    session_cache_delta: rumoca_session::compile::SessionCacheStatsSnapshot,
}

async fn assert_two_completions_contain_label<F>(
    server: &ModelicaLanguageServer,
    mut request: F,
    label: &str,
) where
    F: FnMut() -> CompletionParams,
{
    for response in [request(), request()] {
        let response = server
            .completion(response)
            .await
            .expect("completion should succeed");
        let Some(CompletionResponse::Array(items)) = response else {
            panic!("expected array completion response");
        };
        assert!(
            items.iter().any(|item| item.label == label),
            "namespace completion should include {label}"
        );
    }
}

fn assert_multi_library_namespace_timing(
    cold: &LoggedCompletionTimingSummary,
    warm: &LoggedCompletionTimingSummary,
) {
    assert_eq!(cold.requested_edit_epoch, warm.requested_edit_epoch);
    assert!(!cold.request_was_stale);
    assert!(!warm.request_was_stale);
    assert_eq!(cold.semantic_layer, "package_def_map");
    assert_eq!(warm.semantic_layer, "package_def_map");
}

fn read_jsonl<T: serde::de::DeserializeOwned>(path: &Path) -> Vec<T> {
    let contents = std::fs::read_to_string(path).expect("timing file should exist");
    contents
        .lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| serde_json::from_str(line).expect("valid timing json"))
        .collect()
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

#[test]
fn completion_warm_namespace_cache_reuse_with_multiple_library_paths() {
    let _guard = session_stats_test_guard();
    let temp = new_temp_dir("multi-library-completion-timing");
    let timing_path = temp.join("completion-timings.jsonl");

    run_async_test(async {
        reset_session_cache_stats();
        let lib_path = write_test_library(&temp, "Lib");
        let aux_path = write_test_library(&temp, "Aux");
        let extra_path = write_test_library(&temp, "Extra");
        let lib_key = canonical_path_key(lib_path.to_string_lossy().as_ref());
        let aux_key = canonical_path_key(aux_path.to_string_lossy().as_ref());
        let extra_key = canonical_path_key(extra_path.to_string_lossy().as_ref());
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_key = session_document_uri_key(&active_uri);
        let active_source = "model Active\n  Lib.\nend Active;\n";

        let service = new_test_service();
        let server = service.inner();
        *server.completion_timing_path.write().await = Some(timing_path.clone());
        *server.library_paths.write().await = vec![
            lib_path.to_string_lossy().to_string(),
            aux_path.to_string_lossy().to_string(),
            extra_path.to_string_lossy().to_string(),
        ];
        {
            let mut session = server.session.write().await;
            session.update_document(&active_key, active_source);
        }

        let request = || CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: active_uri.clone(),
                },
                position: Position {
                    line: 1,
                    character: "  Lib.".len() as u32,
                },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
            context: None,
        };

        assert_two_completions_contain_label(server, request, "A").await;
        {
            let loaded_libraries = server.loaded_libraries.read().await;
            assert!(
                loaded_libraries.contains(&lib_key),
                "first completion should load the referenced library root"
            );
            assert!(
                loaded_libraries.contains(&aux_key),
                "first completion should load sibling library roots in the same request"
            );
            assert!(
                loaded_libraries.contains(&extra_key),
                "first completion should load every remaining sibling library root in the same request"
            );
        }
    });

    let entries: Vec<LoggedCompletionTimingSummary> = read_jsonl(&timing_path);
    assert_eq!(
        entries.len(),
        2,
        "expected cold and warm completion timings"
    );
    assert_multi_library_namespace_timing(&entries[0], &entries[1]);
    assert!(
        entries[0]
            .session_cache_delta
            .library_completion_cache_misses
            >= 1,
        "cold namespace completion should miss the library completion cache"
    );
    assert!(
        entries[1].session_cache_delta.library_completion_cache_hits >= 1,
        "warm namespace completion should hit the library completion cache"
    );
    assert_eq!(
        entries[1]
            .session_cache_delta
            .library_completion_cache_misses,
        0,
        "warm namespace completion should not miss after sibling roots are preloaded"
    );
}
