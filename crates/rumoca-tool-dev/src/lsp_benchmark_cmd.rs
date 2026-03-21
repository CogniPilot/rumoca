use anyhow::{Context, Result, bail, ensure};
use lsp_types::Url;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::env;
use std::fs;
use std::io::{BufRead, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver, RecvTimeoutError};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use tempfile::tempdir;

use crate::{exe_name, run_status_quiet, verify_cmd, vscode_cmd};

mod msl_completion;
mod render;
mod runtime;
mod surface_contracts;
mod types;
pub(crate) use msl_completion::run_lsp_msl_completion_timings;
pub(crate) use render::{display_output_path, short_bool_label};
use surface_contracts::{SurfaceCoverageSpec, VSCODE_SURFACE_SPECS, WASM_SURFACE_SPECS};
pub(crate) use types::*;

const COMPLETION_TIMEOUT: Duration = Duration::from_secs(120);
const DIAGNOSTICS_TIMEOUT: Duration = Duration::from_secs(120);
const VALIDATION_TIMEOUT: Duration = Duration::from_secs(30);
const SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(5);
const TERMINAL_TABLE_WIDTH: usize = 80;
const TABLE_CONTENT_WIDTH: usize = TERMINAL_TABLE_WIDTH - 2;
const VALIDATION_ITEM_COL_WIDTH: usize = 18;
const RUNTIME_NOTE_COL_WIDTH: usize = 35;
const WARM_LATENCY_SAMPLE_COUNT: usize = 20;
const LOCAL_COMPLETION_TARGET_MS: u64 = 30;
const SOURCE_ROOT_COMPLETION_TARGET_MS: u64 = 50;
const HOVER_TARGET_MS: u64 = 30;
const DEFINITION_TARGET_MS: u64 = 30;
const LIVE_DIAGNOSTICS_TARGET_MS: u64 = 75;
const SAVE_DIAGNOSTICS_TARGET_MS: u64 = 250;

const VALIDATION_SURFACE_SOURCE: &str = r#"package Lib
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

const VALIDATION_CHANGED_SOURCE: &str = "model Shifted\n  Real x;\nend Shifted;\n";
const VALIDATION_FORMATTING_SOURCE: &str = "model F\nReal x;\nequation\nx=1;\nend F;\n";
const VALIDATION_SAVE_SOURCE: &str =
    "model Active\n  Real x;\nequation\n  der(x) = -x;\nend Active;\n";
const VALIDATION_CODE_ACTION_SOURCE: &str = r#"model Broken
  Real x(start=0)
equation
  der(x) = -x;
end Broken;
"#;
const VALIDATION_LIVE_DIAGNOSTICS_SOURCE_A: &str = "model Live\n  Real x;\nend Live;\n";
const VALIDATION_LIVE_DIAGNOSTICS_SOURCE_B: &str =
    "model Live\n  Real x;\neqn\n  x = 1;\nend Live;\n";

#[derive(Debug, Clone, Copy)]
struct CompletionProbeSpec {
    probe_name: &'static str,
    relative_document: &'static [&'static str],
    probe_text: &'static str,
    expected_completion_label: &'static str,
}

const FULL_MSL_COMPLETION_PROBES: &[CompletionProbeSpec] = &[
    CompletionProbeSpec {
        probe_name: "electrical-resistor",
        relative_document: &[
            "Modelica 4.1.0",
            "Electrical",
            "Analog",
            "Examples",
            "Resistor.mo",
        ],
        probe_text: "Modelica.",
        expected_completion_label: "Electrical",
    },
    CompletionProbeSpec {
        probe_name: "thermal-pump-and-valve",
        relative_document: &[
            "Modelica 4.1.0",
            "Thermal",
            "FluidHeatFlow",
            "Examples",
            "PumpAndValve.mo",
        ],
        probe_text: "Modelica.",
        expected_completion_label: "Thermal",
    },
    CompletionProbeSpec {
        probe_name: "fluid-tanks",
        relative_document: &["Modelica 4.1.0", "Fluid", "Examples", "Tanks.mo"],
        probe_text: "Modelica.",
        expected_completion_label: "Fluid",
    },
    CompletionProbeSpec {
        probe_name: "mechanics-vehicle",
        relative_document: &[
            "Modelica 4.1.0",
            "Mechanics",
            "Translational",
            "Examples",
            "Vehicle.mo",
        ],
        probe_text: "Modelica.",
        expected_completion_label: "Mechanics",
    },
    CompletionProbeSpec {
        probe_name: "clocked-sample1",
        relative_document: &[
            "Modelica 4.1.0",
            "Clocked",
            "Examples",
            "Elementary",
            "RealSignals",
            "Sample1.mo",
        ],
        probe_text: "Modelica.",
        expected_completion_label: "Clocked",
    },
];

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct LspMslCompletionBenchmarkReport {
    lsp_binary: String,
    modelica_paths: Vec<String>,
    probe_results: Vec<LspCompletionProbeReport>,
    lsp_api_validation: LspApiValidationReport,
    vscode_runtime_smoke: RuntimeSmokeReport,
    wasm_runtime_smoke: RuntimeSmokeReport,
    wasm_surface_validation: SurfaceCoverageReport,
    vscode_surface_validation: SurfaceCoverageReport,
}

#[derive(Debug)]
struct CompletionResponseMetrics {
    client_ms: u64,
    completion_count: usize,
    expected_completion_present: bool,
}

#[derive(Debug)]
enum LspInbound {
    Json(Value),
    Closed,
    ReadError(String),
}

struct LspStdioClient {
    child: Child,
    stdin: Option<ChildStdin>,
    inbound: Receiver<LspInbound>,
    completion_progress_path: Option<PathBuf>,
    next_id: u64,
}

impl Drop for LspStdioClient {
    fn drop(&mut self) {
        if self.child.try_wait().ok().flatten().is_none() {
            let _ = self.child.kill();
            let _ = self.child.wait();
        }
    }
}

impl LspStdioClient {
    fn spawn(
        root: &Path,
        lsp_binary: &Path,
        modelica_paths: &[String],
        timing_paths: &LspTimingPaths,
    ) -> Result<Self> {
        let mut command = Command::new(lsp_binary);
        command
            .arg("--stdio")
            .current_dir(root)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit());
        if let Some(path) = &timing_paths.completion {
            command.env("RUMOCA_LSP_COMPLETION_TIMING_FILE", path);
        }
        if let Some(path) = &timing_paths.startup {
            command.env("RUMOCA_LSP_STARTUP_TIMING_FILE", path);
        }
        if let Some(path) = &timing_paths.completion_progress {
            command.env("RUMOCA_LSP_COMPLETION_PROGRESS_FILE", path);
        }
        if let Some(path) = &timing_paths.diagnostics {
            command.env("RUMOCA_LSP_DIAGNOSTICS_TIMING_FILE", path);
        }
        if let Some(path) = &timing_paths.navigation {
            command.env("RUMOCA_LSP_NAVIGATION_TIMING_FILE", path);
        }
        if let Ok(modelica_path_env) = env::join_paths(modelica_paths.iter().map(PathBuf::from)) {
            command.env("MODELICAPATH", modelica_path_env);
        }
        let mut child = command
            .spawn()
            .with_context(|| format!("failed to spawn {}", lsp_binary.display()))?;
        let stdin = child.stdin.take().context("missing rumoca-lsp stdin")?;
        let stdout = child.stdout.take().context("missing rumoca-lsp stdout")?;
        let (tx, inbound) = mpsc::channel();
        thread::spawn(move || forward_lsp_messages(stdout, tx));
        Ok(Self {
            child,
            stdin: Some(stdin),
            inbound,
            completion_progress_path: timing_paths.completion_progress.clone(),
            next_id: 1,
        })
    }

    fn initialize(&mut self, workspace_uri: &Url, modelica_paths: &[String]) -> Result<Value> {
        let params = json!({
            "processId": std::process::id(),
            "rootUri": workspace_uri,
            "capabilities": {},
            "workspaceFolders": [{
                "uri": workspace_uri,
                "name": "full-msl-benchmark",
            }],
            "initializationOptions": {
                "sourceRootPaths": modelica_paths,
            }
        });
        let response = self.request("initialize", params, COMPLETION_TIMEOUT)?;
        self.notify("initialized", json!({}))
            .context("failed to send initialized notification")?;
        Ok(response)
    }

    fn did_open(&mut self, document_uri: &Url, text: &str) -> Result<()> {
        self.notify(
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": document_uri,
                    "languageId": "modelica",
                    "version": 1,
                    "text": text,
                }
            }),
        )
    }

    fn wait_for_publish_diagnostics(
        &mut self,
        document_uri: &Url,
        timeout: Duration,
    ) -> Result<Value> {
        let target = document_uri.as_str().to_string();
        self.recv_matching(timeout, |message| {
            message.get("method").and_then(Value::as_str) == Some("textDocument/publishDiagnostics")
                && message
                    .get("params")
                    .and_then(|params| params.get("uri"))
                    .and_then(Value::as_str)
                    == Some(target.as_str())
        })
    }

    fn did_change(&mut self, document_uri: &Url, version: i32, text: &str) -> Result<()> {
        self.notify(
            "textDocument/didChange",
            json!({
                "textDocument": {
                    "uri": document_uri,
                    "version": version,
                },
                "contentChanges": [{
                    "text": text,
                }],
            }),
        )
    }

    fn did_save(&mut self, document_uri: &Url, text: &str) -> Result<()> {
        self.notify(
            "textDocument/didSave",
            json!({
                "textDocument": { "uri": document_uri },
                "text": text,
            }),
        )
    }

    fn did_close(&mut self, document_uri: &Url) -> Result<()> {
        self.notify(
            "textDocument/didClose",
            json!({
                "textDocument": { "uri": document_uri },
            }),
        )
    }

    fn request_timed(
        &mut self,
        method: &str,
        params: Value,
        timeout: Duration,
    ) -> Result<(u64, Value)> {
        let started = Instant::now();
        let response = self.request(method, params, timeout)?;
        Ok((started.elapsed().as_millis() as u64, response))
    }

    fn completion(
        &mut self,
        document_uri: &Url,
        position: ProbePosition,
        expected_label: &str,
    ) -> Result<CompletionResponseMetrics> {
        let started = Instant::now();
        let response = self.request(
            "textDocument/completion",
            json!({
                "textDocument": { "uri": document_uri },
                "position": {
                    "line": position.line,
                    "character": position.character,
                },
                "context": { "triggerKind": 1 }
            }),
            COMPLETION_TIMEOUT,
        )?;
        let client_ms = started.elapsed().as_millis() as u64;
        let result = response.get("result").cloned().unwrap_or(Value::Null);
        let (completion_count, expected_completion_present) =
            extract_completion_metrics(&result, expected_label);
        Ok(CompletionResponseMetrics {
            client_ms,
            completion_count,
            expected_completion_present,
        })
    }

    fn shutdown(mut self) -> Result<()> {
        let _ = self.request("shutdown", Value::Null, COMPLETION_TIMEOUT)?;
        self.notify("exit", Value::Null)?;
        drop(self.stdin.take());
        wait_for_exit(&mut self.child, SHUTDOWN_TIMEOUT)
    }

    fn request(&mut self, method: &str, params: Value, timeout: Duration) -> Result<Value> {
        let id = self.next_id;
        self.next_id += 1;
        let mut payload = json!({
            "jsonrpc": "2.0",
            "id": id,
            "method": method,
        });
        insert_params_field(&mut payload, params);
        self.write_message(&payload)?;
        let response = match self.recv_matching(timeout, |message| {
            message.get("id").and_then(Value::as_u64) == Some(id)
        }) {
            Ok(response) => response,
            Err(error) => {
                if method == "textDocument/completion"
                    && let Some(detail) = self.latest_completion_progress_detail()
                {
                    return Err(error.context(detail));
                }
                return Err(error);
            }
        };
        if let Some(error) = response.get("error") {
            bail!(
                "rumoca-lsp request `{method}` failed: {}",
                serde_json::to_string_pretty(error)
                    .unwrap_or_else(|_| String::from("<unserializable lsp error>"))
            );
        }
        Ok(response)
    }

    fn notify(&mut self, method: &str, params: Value) -> Result<()> {
        let mut payload = json!({
            "jsonrpc": "2.0",
            "method": method,
        });
        insert_params_field(&mut payload, params);
        self.write_message(&payload)
    }

    fn write_message(&mut self, payload: &Value) -> Result<()> {
        let stdin = self
            .stdin
            .as_mut()
            .context("rumoca-lsp stdin already closed")?;
        let bytes = serde_json::to_vec(payload).context("failed to encode LSP payload")?;
        write!(stdin, "Content-Length: {}\r\n\r\n", bytes.len())
            .context("failed to write LSP frame header")?;
        stdin
            .write_all(&bytes)
            .context("failed to write LSP frame body")?;
        stdin.flush().context("failed to flush LSP stdin")
    }

    fn recv_matching<F>(&mut self, timeout: Duration, mut predicate: F) -> Result<Value>
    where
        F: FnMut(&Value) -> bool,
    {
        let deadline = Instant::now() + timeout;
        loop {
            let now = Instant::now();
            let remaining = deadline.saturating_duration_since(now);
            if remaining.is_zero() {
                bail!("timed out waiting for rumoca-lsp response");
            }
            let inbound = match self.inbound.recv_timeout(remaining) {
                Ok(inbound) => inbound,
                Err(RecvTimeoutError::Timeout) => {
                    bail!("timed out waiting for rumoca-lsp response");
                }
                Err(RecvTimeoutError::Disconnected) => {
                    bail!("rumoca-lsp output channel disconnected unexpectedly");
                }
            };
            if let Some(message) = match_inbound_message(inbound, &mut predicate)? {
                return Ok(message);
            }
        }
    }

    fn latest_completion_progress_detail(&self) -> Option<String> {
        let path = self.completion_progress_path.as_ref()?;
        let entries = read_completion_progress(path).ok()?;
        let latest = entries.last()?;
        let latest_with_detail = entries.iter().rev().find(|entry| entry.detail.is_some());
        let mut detail = format!(
            "last completion checkpoint: stage={} status={} elapsedMs={} prefix={} needsResolved={} queryFastPath={}",
            latest.stage,
            latest.status,
            latest.elapsed_ms,
            latest.completion_prefix.as_deref().unwrap_or("-"),
            latest
                .needs_resolved_session
                .map(short_bool_label)
                .unwrap_or("-"),
            latest
                .query_fast_path_matched
                .map(short_bool_label)
                .unwrap_or("-"),
        );
        if let Some(entry) = latest_with_detail
            && let Some(extra) = &entry.detail
        {
            detail.push_str(&format!(
                "; last detail: stage={} status={} {}",
                entry.stage, entry.status, extra
            ));
        }
        Some(detail)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ProbePosition {
    line: u32,
    character: u32,
}

fn run_completion_probe(
    root: &Path,
    lsp_binary: &Path,
    modelica_paths: &[String],
    msl_archive_root: &Path,
    report_dir: &Path,
    probe: CompletionProbeSpec,
) -> Result<LspCompletionProbeReport> {
    let document_path = completion_probe_document_path(msl_archive_root, probe)?;
    let document_uri = file_url(&document_path)?;
    let workspace_uri = directory_url(msl_archive_root)?;
    let (source, position) = load_probe_source(&document_path, probe)?;
    let timing_path = unique_timing_path(report_dir, probe.probe_name);
    let diagnostics_path =
        unique_timing_path(report_dir, &format!("{}-diagnostics", probe.probe_name));
    let progress_path = unique_timing_path(report_dir, &format!("{}-progress", probe.probe_name));
    let stable_timing_path = report_dir.join(format!("{}.jsonl", probe.probe_name));

    println!("Benchmarking {}...", probe.probe_name);
    let mut client = LspStdioClient::spawn(
        root,
        lsp_binary,
        modelica_paths,
        &LspTimingPaths {
            startup: None,
            completion: Some(timing_path.clone()),
            completion_progress: Some(progress_path),
            diagnostics: Some(diagnostics_path.clone()),
            navigation: None,
        },
    )?;
    let _ = client.initialize(&workspace_uri, modelica_paths)?;
    client.did_open(&document_uri, &source)?;
    client.wait_for_publish_diagnostics(&document_uri, DIAGNOSTICS_TIMEOUT)?;

    let cold_client =
        client.completion(&document_uri, position, probe.expected_completion_label)?;
    let warm_client =
        client.completion(&document_uri, position, probe.expected_completion_label)?;
    let diagnostics_start = read_diagnostics_timings(&diagnostics_path)?.len();
    let edited_source = apply_probe_small_edit(&source);
    let edit_started = Instant::now();
    client.did_change(&document_uri, 2, &edited_source)?;
    let _ = client.wait_for_publish_diagnostics(&document_uri, DIAGNOSTICS_TIMEOUT)?;
    let live_edit =
        diagnostics_entries_since(&diagnostics_path, diagnostics_start, "live", &document_uri)?
            .into_iter()
            .last()
            .context("missing live diagnostics timing for edited completion probe")?;
    let edited_client =
        client.completion(&document_uri, position, probe.expected_completion_label)?;
    client.shutdown()?;

    let timings = read_completion_timings(&timing_path)?;
    let document_key = validate_completion_probe_results(
        probe,
        &document_path,
        &cold_client,
        &warm_client,
        &edited_client,
        &live_edit,
        &timings,
    )?;
    stage_timing_artifact(&timing_path, &stable_timing_path)?;
    Ok(build_completion_probe_report(
        probe,
        document_key,
        CompletionProbeMeasurements {
            cold_client,
            warm_client,
            edited_client,
            edit_client_ms: edit_started.elapsed().as_millis() as u64,
            live_edit,
            timings,
        },
    ))
}

fn load_probe_source(
    document_path: &Path,
    probe: CompletionProbeSpec,
) -> Result<(String, ProbePosition)> {
    let source = fs::read_to_string(document_path)
        .with_context(|| format!("failed to read {}", document_path.display()))?;
    let position = probe_position(&source, probe.probe_text).with_context(|| {
        format!(
            "failed to find probe text `{}` in {}",
            probe.probe_text,
            document_path.display()
        )
    })?;
    Ok((source, position))
}

fn validate_completion_probe_results(
    probe: CompletionProbeSpec,
    document_path: &Path,
    cold_client: &CompletionResponseMetrics,
    warm_client: &CompletionResponseMetrics,
    edited_client: &CompletionResponseMetrics,
    live_edit: &DiagnosticsTimingEntry,
    timings: &[CompletionTimingEntry],
) -> Result<String> {
    ensure!(
        timings.len() == 3,
        "expected 3 completion timing entries for {}, got {}",
        probe.probe_name,
        timings.len()
    );

    let document_key = document_path.display().to_string();
    ensure!(
        timings.iter().all(|entry| entry.uri == document_key),
        "completion timing entries for {} did not match {}",
        probe.probe_name,
        document_path.display()
    );
    ensure_expected_completion_present("cold", probe, cold_client.expected_completion_present)?;
    ensure_expected_completion_present("warm", probe, warm_client.expected_completion_present)?;
    ensure_expected_completion_present("edited", probe, edited_client.expected_completion_present)?;
    ensure_namespace_cache_reuse(probe.probe_name, &timings[0], &timings[1])?;
    ensure_edited_probe_reuse(probe.probe_name, &timings[2], live_edit)?;
    Ok(document_key)
}

fn ensure_expected_completion_present(
    run_label: &str,
    probe: CompletionProbeSpec,
    expected_present: bool,
) -> Result<()> {
    ensure!(
        expected_present,
        "{run_label} completion for {} did not include expected label `{}`",
        probe.probe_name,
        probe.expected_completion_label
    );
    Ok(())
}

fn ensure_namespace_cache_reuse(
    probe_name: &str,
    cold: &CompletionTimingEntry,
    warm: &CompletionTimingEntry,
) -> Result<()> {
    ensure!(
        cold.session_cache_delta.namespace_completion_cache_hits >= 1,
        "cold completion for {} did not observe a prewarmed namespace completion cache hit",
        probe_name
    );
    ensure!(
        cold.session_cache_delta.namespace_completion_cache_misses == 0,
        "cold completion for {} unexpectedly rebuilt the namespace completion cache",
        probe_name
    );
    ensure!(
        warm.session_cache_delta.namespace_completion_cache_hits >= 1,
        "warm completion for {} did not record a namespace completion cache hit",
        probe_name
    );
    ensure!(
        warm.session_cache_delta.namespace_completion_cache_misses == 0,
        "warm completion for {} unexpectedly missed the namespace completion cache",
        probe_name
    );
    ensure!(
        !warm.built_resolved_tree && !warm.had_resolved_cache_before,
        "warm namespace completion for {} should stay off semantic navigation state",
        probe_name
    );
    ensure!(
        warm.session_cache_delta.standard_resolved_builds == 0
            && warm.session_cache_delta.semantic_navigation_builds == 0,
        "warm namespace completion for {} should avoid resolved rebuilds",
        probe_name
    );
    ensure!(
        warm.session_cache_delta.instantiated_model_builds == 0
            && warm.session_cache_delta.typed_model_builds == 0
            && warm.session_cache_delta.flat_model_builds == 0
            && warm.session_cache_delta.dae_model_builds == 0,
        "warm namespace completion for {} should not rebuild model query stages",
        probe_name
    );
    Ok(())
}

fn ensure_edited_probe_reuse(
    probe_name: &str,
    edited: &CompletionTimingEntry,
    live_edit: &DiagnosticsTimingEntry,
) -> Result<()> {
    ensure!(
        live_edit.session_cache_delta.document_parse_calls >= 1,
        "edited completion probe for {} did not record a document parse during didChange",
        probe_name
    );
    ensure!(
        edited.session_cache_delta.namespace_completion_cache_hits >= 1,
        "edited completion for {} should reuse the warmed namespace completion cache",
        probe_name
    );
    ensure!(
        edited.session_cache_delta.namespace_completion_cache_misses == 0,
        "edited completion for {} unexpectedly missed the namespace completion cache",
        probe_name
    );
    ensure!(
        edited.session_cache_delta.standard_resolved_builds == 0
            && edited.session_cache_delta.semantic_navigation_builds == 0,
        "edited completion for {} should stay off resolved rebuilds",
        probe_name
    );
    ensure!(
        edited.session_cache_delta.instantiated_model_builds == 0
            && edited.session_cache_delta.typed_model_builds == 0
            && edited.session_cache_delta.flat_model_builds == 0
            && edited.session_cache_delta.dae_model_builds == 0,
        "edited completion for {} should not rebuild model query stages",
        probe_name
    );
    Ok(())
}

fn stage_timing_artifact(timing_path: &Path, stable_timing_path: &Path) -> Result<()> {
    fs::copy(timing_path, stable_timing_path).with_context(|| {
        format!(
            "failed to stage {} as {}",
            timing_path.display(),
            stable_timing_path.display()
        )
    })?;
    let _ = fs::remove_file(timing_path);
    Ok(())
}

struct CompletionProbeMeasurements {
    cold_client: CompletionResponseMetrics,
    warm_client: CompletionResponseMetrics,
    edited_client: CompletionResponseMetrics,
    edit_client_ms: u64,
    live_edit: DiagnosticsTimingEntry,
    timings: Vec<CompletionTimingEntry>,
}

fn build_completion_probe_report(
    probe: CompletionProbeSpec,
    document_key: String,
    measurements: CompletionProbeMeasurements,
) -> LspCompletionProbeReport {
    let CompletionProbeMeasurements {
        cold_client,
        warm_client,
        edited_client,
        edit_client_ms,
        live_edit,
        timings,
    } = measurements;
    LspCompletionProbeReport {
        probe_name: probe.probe_name.to_string(),
        document_path: document_key,
        cold: CompletionMeasurementReport {
            client_ms: cold_client.client_ms,
            completion_count: cold_client.completion_count,
            expected_completion_present: cold_client.expected_completion_present,
            preceding_edit_client_ms: None,
            preceding_live_diagnostics_ms: None,
            preceding_document_parse_ms: None,
            lsp: timings[0].clone(),
        },
        warm: CompletionMeasurementReport {
            client_ms: warm_client.client_ms,
            completion_count: warm_client.completion_count,
            expected_completion_present: warm_client.expected_completion_present,
            preceding_edit_client_ms: None,
            preceding_live_diagnostics_ms: None,
            preceding_document_parse_ms: None,
            lsp: timings[1].clone(),
        },
        edited: CompletionMeasurementReport {
            client_ms: edited_client.client_ms,
            completion_count: edited_client.completion_count,
            expected_completion_present: edited_client.expected_completion_present,
            preceding_edit_client_ms: Some(edit_client_ms),
            preceding_live_diagnostics_ms: Some(live_edit.total_ms),
            preceding_document_parse_ms: Some(
                live_edit.session_cache_delta.document_parse_total_nanos / 1_000_000,
            ),
            lsp: timings[2].clone(),
        },
    }
}

fn apply_probe_small_edit(source: &str) -> String {
    format!("{source}\n")
}

fn unique_timing_path(report_dir: &Path, probe_name: &str) -> PathBuf {
    let run_id = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|duration| duration.as_millis())
        .unwrap_or(0);
    report_dir.join(format!(
        "{probe_name}-{}-{run_id}.jsonl",
        std::process::id()
    ))
}

fn completion_probe_document_path(
    msl_archive_root: &Path,
    probe: CompletionProbeSpec,
) -> Result<PathBuf> {
    let mut path = msl_archive_root.to_path_buf();
    for segment in probe.relative_document {
        path.push(segment);
    }
    ensure!(
        path.is_file(),
        "missing benchmark document {}",
        path.display()
    );
    path.canonicalize()
        .with_context(|| format!("failed to canonicalize {}", path.display()))
}

fn file_url(path: &Path) -> Result<Url> {
    Url::from_file_path(path)
        .map_err(|_| anyhow::anyhow!("failed to convert {} to file URL", path.display()))
}

fn directory_url(path: &Path) -> Result<Url> {
    Url::from_directory_path(path)
        .map_err(|_| anyhow::anyhow!("failed to convert {} to directory URL", path.display()))
}

fn uri_path_string(uri: &Url) -> Result<String> {
    uri.to_file_path()
        .map_err(|_| anyhow::anyhow!("failed to decode file URL {}", uri))
        .map(|path| path.display().to_string())
}

fn canonicalize_string(path: &Path) -> Result<String> {
    path.canonicalize()
        .with_context(|| format!("failed to canonicalize {}", path.display()))
        .map(|path| path.display().to_string())
}

fn probe_position(source: &str, probe_text: &str) -> Result<ProbePosition> {
    let byte_offset = source
        .find(probe_text)
        .with_context(|| format!("probe text `{probe_text}` not found"))?;
    probe_position_at_offset(source, byte_offset + probe_text.len())
}

fn probe_position_at_offset(source: &str, byte_offset: usize) -> Result<ProbePosition> {
    ensure!(
        byte_offset <= source.len(),
        "probe byte offset {} exceeded source length {}",
        byte_offset,
        source.len()
    );
    let prefix = &source[..byte_offset];
    let line = prefix.bytes().filter(|byte| *byte == b'\n').count() as u32;
    let character = prefix
        .rsplit_once('\n')
        .map(|(_, tail)| tail.chars().count() as u32)
        .unwrap_or(prefix.chars().count() as u32);
    Ok(ProbePosition { line, character })
}

fn completion_probe_position_from_source(source: &str) -> Result<ProbePosition> {
    let preferred = "Modelica.Electrical.Analog.Basic.Ground";
    if let Some(offset) = source.find(preferred) {
        return probe_position_at_offset(source, offset + "Modelica.".len());
    }
    probe_position(source, "Modelica.")
}

fn local_hover_probe_from_source(source: &str) -> Result<ProbePosition> {
    let preferred = "Basic.Resistor resistor(";
    if let Some(offset) = source.find(preferred) {
        let probe_offset = offset + preferred.rfind("resistor").unwrap_or(0);
        return probe_position_at_offset(source, probe_offset);
    }
    let fallback = source
        .find("model Resistor")
        .context("expected full-MSL probe file to contain a hoverable symbol")?;
    probe_position_at_offset(source, fallback + "model ".len())
}

fn local_definition_probe_from_source(source: &str) -> Result<ProbePosition> {
    let probe = "resistor.p)";
    let offset = source
        .find(probe)
        .context("expected full-MSL probe file to contain a local definition target")?;
    probe_position_at_offset(source, offset)
}

fn response_result(response: &Value) -> Value {
    response.get("result").cloned().unwrap_or(Value::Null)
}

fn diagnostics_from_notification(message: &Value) -> Vec<Value> {
    message
        .get("params")
        .and_then(|params| params.get("diagnostics"))
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default()
}

fn json_array_len(value: &Value) -> usize {
    value.as_array().map_or(0, Vec::len)
}

fn hover_text(value: &Value) -> String {
    if let Some(contents) = value.get("contents") {
        return hover_contents_text(contents);
    }
    String::new()
}

fn hover_contents_text(value: &Value) -> String {
    if let Some(text) = value.as_str() {
        return text.to_string();
    }
    if let Some(items) = value.as_array() {
        return items
            .iter()
            .map(hover_contents_text)
            .filter(|part| !part.is_empty())
            .collect::<Vec<_>>()
            .join("\n");
    }
    value
        .get("value")
        .and_then(Value::as_str)
        .unwrap_or_default()
        .to_string()
}

fn definition_targets(value: &Value) -> Vec<String> {
    if value.is_null() {
        return Vec::new();
    }
    if let Some(uri) = value.get("uri").and_then(Value::as_str) {
        return vec![uri.to_string()];
    }
    if let Some(uri) = value.get("targetUri").and_then(Value::as_str) {
        return vec![uri.to_string()];
    }
    value
        .as_array()
        .map(|items| {
            items
                .iter()
                .flat_map(definition_targets)
                .collect::<Vec<_>>()
        })
        .unwrap_or_default()
}

fn semantic_token_count(value: &Value) -> usize {
    value
        .get("data")
        .and_then(Value::as_array)
        .map_or(0, Vec::len)
}

fn workspace_edit_change_count(value: &Value, uri: &Url) -> usize {
    value
        .get("changes")
        .and_then(|changes| changes.get(uri.as_str()))
        .and_then(Value::as_array)
        .map_or(0, Vec::len)
}

fn expected_diagnostic_range(diagnostics: &[Value], code: &str) -> Option<Value> {
    diagnostics.iter().find_map(|diag| {
        (diag.get("code").and_then(Value::as_str) == Some(code))
            .then(|| diag.get("range").cloned())
            .flatten()
    })
}

fn ok_validation(
    operation: &str,
    kind: &str,
    client_ms: Option<u64>,
    detail: impl Into<String>,
) -> LspApiValidationEntry {
    LspApiValidationEntry {
        operation: operation.to_string(),
        kind: kind.to_string(),
        ok: true,
        client_ms,
        detail: detail.into(),
    }
}

fn extract_completion_metrics(result: &Value, expected_label: &str) -> (usize, bool) {
    let items = result
        .as_array()
        .cloned()
        .or_else(|| result.get("items").and_then(Value::as_array).cloned())
        .unwrap_or_default();
    let expected_present = items.iter().any(|item| {
        item.get("label")
            .and_then(completion_item_label)
            .is_some_and(|label| label == expected_label)
    });
    (items.len(), expected_present)
}

fn completion_item_label(value: &Value) -> Option<String> {
    if let Some(label) = value.as_str() {
        return Some(label.to_string());
    }
    value
        .get("label")
        .and_then(Value::as_str)
        .map(ToOwned::to_owned)
}

fn read_completion_timings(path: &Path) -> Result<Vec<CompletionTimingEntry>> {
    read_jsonl(path, "completion timing")
}

fn read_startup_timings(path: &Path) -> Result<Vec<StartupTimingEntry>> {
    read_jsonl(path, "startup timing")
}

fn read_completion_progress(path: &Path) -> Result<Vec<CompletionProgressEntry>> {
    read_jsonl(path, "completion progress")
}

fn read_diagnostics_timings(path: &Path) -> Result<Vec<DiagnosticsTimingEntry>> {
    read_jsonl(path, "diagnostics timing")
}

fn read_navigation_timings(path: &Path) -> Result<Vec<NavigationTimingEntry>> {
    read_jsonl(path, "navigation timing")
}

fn completion_entries_since(
    path: &Path,
    start: usize,
    uri: &Url,
) -> Result<Vec<CompletionTimingEntry>> {
    let entries = read_completion_timings(path)?;
    let uri = uri_path_string(uri)?;
    Ok(entries
        .into_iter()
        .skip(start)
        .filter(|entry| entry.uri == uri)
        .collect())
}

fn diagnostics_entries_since(
    path: &Path,
    start: usize,
    trigger: &str,
    uri: &Url,
) -> Result<Vec<DiagnosticsTimingEntry>> {
    let entries = read_diagnostics_timings(path)?;
    let uri = uri_path_string(uri)?;
    Ok(entries
        .into_iter()
        .skip(start)
        .filter(|entry| entry.trigger == trigger && entry.uri == uri)
        .collect())
}

fn read_jsonl<T>(path: &Path, label: &str) -> Result<Vec<T>>
where
    T: for<'de> Deserialize<'de>,
{
    if !path.exists() {
        return Ok(Vec::new());
    }
    let raw =
        fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
    raw.lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| {
            serde_json::from_str(line).with_context(|| format!("failed to parse {label} JSONL"))
        })
        .collect()
}

fn build_warm_latency_measurement(
    label: &str,
    budget_ms: u64,
    samples: &[u64],
    request_path: Option<NavigationRequestPath>,
    semantic_layer: Option<&str>,
) -> Result<WarmLatencyMeasurement> {
    ensure!(
        !samples.is_empty(),
        "warm latency snapshot for {label} requires at least one sample"
    );
    let p50_ms = percentile_ms(samples, 50)?;
    let p95_ms = percentile_ms(samples, 95)?;
    Ok(WarmLatencyMeasurement {
        label: label.to_string(),
        request_path,
        semantic_layer: semantic_layer.map(str::to_string),
        samples: samples.len(),
        p50_ms,
        p95_ms,
        budget_ms,
        within_budget: p95_ms <= budget_ms,
    })
}

fn percentile_ms(samples: &[u64], percentile: usize) -> Result<u64> {
    ensure!(
        !samples.is_empty(),
        "cannot compute percentile for an empty sample set"
    );
    ensure!(
        (1..=100).contains(&percentile),
        "percentile must be between 1 and 100, got {percentile}"
    );
    let mut sorted = samples.to_vec();
    sorted.sort_unstable();
    let index = (sorted.len() * percentile).div_ceil(100).saturating_sub(1);
    Ok(sorted[index])
}

fn build_surface_coverage_report(
    root: &Path,
    area: &str,
    specs: &[SurfaceCoverageSpec],
) -> Result<SurfaceCoverageReport> {
    let mut entries = Vec::with_capacity(specs.len());
    for spec in specs {
        let source = fs::read_to_string(root.join(spec.source_path))
            .with_context(|| format!("failed to read {}", root.join(spec.source_path).display()))?;
        let proof = fs::read_to_string(root.join(spec.proof_path))
            .with_context(|| format!("failed to read {}", root.join(spec.proof_path).display()))?;
        let ok = source.contains(spec.source_pattern) && proof.contains(spec.proof_pattern);
        entries.push(SurfaceCoverageEntry {
            surface: spec.surface.to_string(),
            kind: spec.kind.to_string(),
            ok,
            proof: spec.proof_label.to_string(),
        });
    }
    let failures = entries
        .iter()
        .filter(|entry| !entry.ok)
        .map(|entry| format!("{} [{}]", entry.surface, entry.proof))
        .collect::<Vec<_>>();
    ensure!(
        failures.is_empty(),
        "{} surface validation drifted: {}",
        area,
        failures.join(", ")
    );
    Ok(SurfaceCoverageReport {
        area: area.to_string(),
        entries,
    })
}

fn run_vscode_runtime_smoke(
    root: &Path,
    msl_archive_root: &Path,
    report_dir: &Path,
    install_prereqs: bool,
) -> Result<RuntimeSmokeReport> {
    if !install_prereqs && !vscode_cmd::can_launch_vscode_msl_smoke() {
        ensure_editor_runtime_optional(
            "VS Code runtime smoke",
            "requires node/npm and DISPLAY or xvfb-run+xauth on Linux",
        )?;
        return Ok(skip_runtime_smoke_report(
            "VS Code runtime smoke",
            "missing headless VS Code smoke prerequisites",
        ));
    }
    let summary = vscode_cmd::run_vscode_msl_smoke_report(
        root,
        msl_archive_root,
        report_dir,
        vscode_cmd::VscodeSmokeOptions { install_prereqs },
    )?;
    Ok(build_vscode_runtime_report(&summary))
}

fn run_wasm_runtime_smoke(
    root: &Path,
    msl_archive_root: &Path,
    report_dir: &Path,
) -> Result<RuntimeSmokeReport> {
    if !verify_cmd::can_launch_wasm_browser_msl_smoke() {
        ensure_editor_runtime_optional(
            "WASM runtime smoke",
            "requires node plus a Chromium-compatible browser",
        )?;
        return Ok(skip_runtime_smoke_report(
            "WASM runtime smoke",
            "missing headless browser smoke prerequisites",
        ));
    }
    let summary =
        verify_cmd::run_wasm_browser_msl_smoke_report(root, msl_archive_root, report_dir)?;
    Ok(build_wasm_runtime_report(&summary))
}

fn build_vscode_runtime_report(summary: &vscode_cmd::VscodeMslSmokeSummary) -> RuntimeSmokeReport {
    let warm_stage = summary.warm_stage_timings.as_ref();
    RuntimeSmokeReport {
        area: "VS Code runtime smoke".to_string(),
        status: "pass".to_string(),
        note: "headless extension-host smoke against full MSL".to_string(),
        entries: vec![
            runtime_entry(
                "activate",
                summary.activate_ms.is_some(),
                summary.activate_ms,
                "extension host",
            ),
            runtime_entry(
                "open",
                summary.open_ms.is_some(),
                summary.open_ms,
                "full-MSL doc",
            ),
            runtime_entry(
                "codeLens",
                summary.code_lens_count.is_some(),
                summary.code_lens_ms,
                &format!("count={}", summary.code_lens_count.unwrap_or(0)),
            ),
            runtime_entry(
                "source-root-load",
                summary
                    .source_root_expected_completion_present
                    .unwrap_or(false),
                summary.source_root_load_ms,
                &vscode_completion_stage_detail(
                    summary.source_root_load_completion_count.unwrap_or(0),
                    summary.source_root_stage_timings.as_ref(),
                ),
            ),
            runtime_entry(
                "completion:cold",
                summary.expected_completion_present.unwrap_or(false),
                summary.completion_ms,
                &vscode_completion_stage_detail(
                    summary.completion_count.unwrap_or(0),
                    summary.cold_stage_timings.as_ref(),
                ),
            ),
            runtime_entry(
                "completion:warm",
                summary.warm_expected_completion_present.unwrap_or(false),
                summary.warm_completion_ms,
                &vscode_completion_stage_detail(
                    summary.warm_completion_count.unwrap_or(0),
                    summary.warm_stage_timings.as_ref(),
                ),
            ),
            completion_cache_runtime_entry(warm_stage),
            runtime_entry(
                "hover",
                summary.expected_hover_present.unwrap_or(false),
                summary.hover_ms,
                &format!("hits={}", summary.hover_count.unwrap_or(0)),
            ),
            runtime_entry(
                "definition",
                summary.expected_definition_present.unwrap_or(false)
                    && summary.cross_file_definition_present.unwrap_or(false),
                summary.definition_ms,
                &format!(
                    "hits={} cross={}",
                    summary.definition_count.unwrap_or(0),
                    short_bool_label(summary.cross_file_definition_present.unwrap_or(false))
                ),
            ),
        ],
    }
}

fn completion_stage_metrics(
    items: u64,
    total_ms: Option<u64>,
    source_root_load_ms: Option<u64>,
    completion_source_root_load_ms: Option<u64>,
) -> String {
    format!(
        "i={items} l={} s={} c={}",
        total_ms.unwrap_or(0),
        source_root_load_ms.unwrap_or(0),
        completion_source_root_load_ms.unwrap_or(0),
    )
}

fn validation_completion_stage_detail(
    scope: &str,
    items: usize,
    timing: &CompletionTimingEntry,
) -> String {
    format!(
        "{scope} {} layer={}",
        completion_stage_metrics(
            items as u64,
            Some(timing.total_ms),
            Some(timing.source_root_load_ms),
            Some(timing.completion_source_root_load_ms),
        ),
        timing.semantic_layer
    )
}

fn vscode_completion_stage_detail(
    items: u64,
    timing: Option<&vscode_cmd::VscodeStageTimingSummary>,
) -> String {
    completion_stage_metrics(
        items,
        timing.and_then(|entry| entry.total_ms),
        timing.and_then(|entry| entry.source_root_load_ms),
        timing.and_then(|entry| entry.completion_source_root_load_ms),
    )
}

fn build_wasm_runtime_report(summary: &verify_cmd::WasmSmokeSummary) -> RuntimeSmokeReport {
    let warm_stage = summary.warm_stage_timings.as_ref();
    RuntimeSmokeReport {
        area: "WASM runtime smoke".to_string(),
        status: "pass".to_string(),
        note: "headless browser smoke against full MSL".to_string(),
        entries: vec![
            runtime_entry(
                "open",
                summary.open_ms.is_some(),
                summary.open_ms,
                "full-MSL doc",
            ),
            runtime_entry(
                "codeLens",
                summary.code_lens_count.is_some(),
                summary.code_lens_ms,
                &format!("count={}", summary.code_lens_count.unwrap_or(0)),
            ),
            runtime_entry(
                "archive-load",
                summary.source_root_count.unwrap_or(0) > 0,
                summary.archive_load_ms,
                &format!("roots={}", summary.source_root_count.unwrap_or(0)),
            ),
            runtime_entry(
                "source-root-load",
                summary
                    .source_root_expected_completion_present
                    .unwrap_or(false),
                summary.source_root_load_ms,
                &vscode_completion_stage_detail(
                    summary.source_root_load_completion_count.unwrap_or(0),
                    summary.source_root_stage_timings.as_ref(),
                ),
            ),
            runtime_entry(
                "completion:cold",
                summary.expected_completion_present.unwrap_or(false),
                summary.completion_ms,
                &vscode_completion_stage_detail(
                    summary.completion_count.unwrap_or(0),
                    summary.cold_stage_timings.as_ref(),
                ),
            ),
            runtime_entry(
                "completion:warm",
                summary.warm_expected_completion_present.unwrap_or(false),
                summary.warm_completion_ms,
                &vscode_completion_stage_detail(
                    summary.warm_completion_count.unwrap_or(0),
                    summary.warm_stage_timings.as_ref(),
                ),
            ),
            completion_cache_runtime_entry(warm_stage),
            runtime_entry(
                "hover",
                summary.expected_hover_present.unwrap_or(false),
                summary.hover_ms,
                &format!("hits={}", summary.hover_count.unwrap_or(0)),
            ),
            runtime_entry(
                "definition",
                summary.expected_definition_present.unwrap_or(false),
                summary.definition_ms,
                &format!(
                    "hits={} cross={}",
                    summary.definition_count.unwrap_or(0),
                    short_bool_label(summary.cross_file_definition_present.unwrap_or(false)),
                ),
            ),
            runtime_entry(
                "compile",
                summary.compile_ms.is_some() && summary.error.is_none(),
                summary.compile_ms,
                summary.model_name.as_deref().unwrap_or("model=unknown"),
            ),
        ],
    }
}

fn completion_cache_runtime_entry(
    warm_stage: Option<&vscode_cmd::VscodeStageTimingSummary>,
) -> RuntimeSmokeEntry {
    let warm_cache = warm_stage.and_then(|timing| timing.session_cache_delta.as_ref());
    runtime_entry(
        "completion:cache",
        warm_cache
            .and_then(|delta| delta.namespace_completion_cache_hits)
            .unwrap_or(0)
            >= 1
            && warm_cache
                .and_then(|delta| delta.namespace_completion_cache_misses)
                .unwrap_or(0)
                == 0
            && warm_cache
                .and_then(|delta| delta.source_root_files_parsed)
                .unwrap_or(0)
                == 0,
        warm_stage.and_then(|timing| timing.total_ms),
        &format!(
            "hit={} miss={} parsed={}",
            warm_cache
                .and_then(|delta| delta.namespace_completion_cache_hits)
                .unwrap_or(0),
            warm_cache
                .and_then(|delta| delta.namespace_completion_cache_misses)
                .unwrap_or(0),
            warm_cache
                .and_then(|delta| delta.source_root_files_parsed)
                .unwrap_or(0),
        ),
    )
}

use runtime::*;

#[cfg(test)]
mod tests {
    use super::{NavigationRequestPath, build_warm_latency_measurement, percentile_ms};

    #[test]
    fn percentile_ms_uses_upper_rank_for_p95() {
        let samples = [4_u64, 5, 6, 7, 8, 9, 10, 11, 12, 40];

        assert_eq!(percentile_ms(&samples, 50).expect("p50"), 8);
        assert_eq!(percentile_ms(&samples, 95).expect("p95"), 40);
    }

    #[test]
    fn warm_latency_measurement_marks_budget_miss() {
        let measurement = build_warm_latency_measurement(
            "hover",
            30,
            &[5, 8, 11, 42],
            Some(NavigationRequestPath::QueryOnly),
            Some("class_interface"),
        )
        .expect("measurement");

        assert_eq!(
            measurement.request_path,
            Some(NavigationRequestPath::QueryOnly)
        );
        assert_eq!(
            measurement.semantic_layer.as_deref(),
            Some("class_interface")
        );
        assert_eq!(measurement.samples, 4);
        assert_eq!(measurement.p50_ms, 8);
        assert_eq!(measurement.p95_ms, 42);
        assert_eq!(measurement.budget_ms, 30);
        assert!(!measurement.within_budget);
    }
}
