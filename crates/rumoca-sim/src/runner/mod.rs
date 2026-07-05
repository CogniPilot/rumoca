//! Interactive simulation runtime driven by a `SimulationConfig` TOML.
//!
//! Axis crates (input, transport, codec, solver) are composed here into a
//! running app. Optionally couples to an external process over UDP with
//! a configured codec; otherwise runs standalone.

pub mod config;
pub mod devices;
pub mod executor;

/// Template TOML printed by `rumoca sim init`.
pub const CONFIG_TEMPLATE: &str = include_str!("template.toml");

use std::path::PathBuf;
use std::thread;

use crate::runner::config::SimulationConfig;
use crate::{
    DiffsolMethod, SimOptions, SimSolverMode, SimulationDiagnosticError, SimulationSession,
};
use anyhow::{Context, Result};
use rumoca_compile::compile::{DaeCompilationResult, Session, SourceRootKind};
use rumoca_compile::source_roots::{
    parse_source_root_with_cache, resolve_source_root_cache_dir, source_root_source_set_key,
};
use rumoca_core::{Diagnostic, PrimaryLabel, SourceMap};

#[derive(Debug, thiserror::Error)]
pub enum RunnerError {
    #[error(transparent)]
    Other(#[from] anyhow::Error),

    #[error("{context}\n\nCaused by:\n  {error}")]
    SimulationDiagnostic {
        context: String,
        #[source]
        error: Box<SimulationDiagnosticError>,
        source_map: Option<Box<SourceMap>>,
    },
}

impl RunnerError {
    pub fn simulation_diagnostic(
        context: impl Into<String>,
        error: SimulationDiagnosticError,
        source_map: Option<SourceMap>,
    ) -> Self {
        Self::SimulationDiagnostic {
            context: context.into(),
            error: Box::new(error),
            source_map: source_map.map(Box::new),
        }
    }

    pub fn source_diagnostic(&self) -> Option<(Diagnostic, SourceMap)> {
        let Self::SimulationDiagnostic {
            context,
            error,
            source_map,
        } = self
        else {
            return None;
        };
        let span = error.source_span()?;
        let source_map = source_map.as_deref()?.clone();
        Some((
            Diagnostic::error(
                error.diagnostic_code(),
                format!("{context}: {error}"),
                PrimaryLabel::new(span).with_message(error.diagnostic_label()),
            ),
            source_map,
        ))
    }
}

/// Arguments for the `rumoca sim --config` command.
pub struct SimArgs {
    /// Modelica source code content.
    pub model_source: String,
    /// Path to the source file that supplied `model_source`.
    pub model_path: Option<PathBuf>,
    /// Model name to simulate.
    pub model_name: String,
    /// Parsed simulation app configuration.
    pub config: SimulationConfig,
    /// Solver selected by CLI override or `[sim].solver`.
    pub solver_mode: SimSolverMode,
    /// Original solver label, used for BDF-family method selection.
    pub solver_label: String,
    /// Optional absolute tolerance selected by CLI or `[sim].atol`.
    pub atol: Option<f64>,
    /// Optional relative tolerance selected by CLI or `[sim].rtol`.
    pub rtol: Option<f64>,
    /// HTTP server port.
    pub http_port: u16,
    /// WebSocket viz port.
    pub ws_port: u16,
    /// Scene script content (None = minimal placeholder scene).
    pub scene_script: Option<String>,
    /// Directory used to serve scene-relative `/assets/...` files.
    pub scene_asset_dir: Option<PathBuf>,
    /// Additional Modelica package roots.
    pub source_roots: Vec<PathBuf>,
    /// Enable debug features (overlays, log downloads).
    pub debug: bool,
}

pub(super) fn load_source_roots_into_session(
    session: &mut Session,
    source_roots: &[PathBuf],
) -> Result<()> {
    for source_root in source_roots {
        let parsed = parse_source_root_with_cache(source_root)
            .with_context(|| format!("Load source root: {}", source_root.display()))?;
        let source_root_path = source_root.to_string_lossy();
        let source_root_key = source_root_source_set_key(source_root_path.as_ref());
        session.replace_parsed_source_set(
            &source_root_key,
            SourceRootKind::External,
            parsed.documents,
            None,
        );
        let cache_dir = resolve_source_root_cache_dir();
        let _ = session.sync_source_root_semantic_summary_cache(
            &source_root_key,
            source_root,
            cache_dir.as_deref(),
        );
    }
    Ok(())
}

pub(super) fn compile_model_with_diagnostics(
    session: &mut Session,
    model_name: &str,
    context: &str,
) -> Result<Box<DaeCompilationResult>> {
    match session.compile_model_dae_strict_reachable_uncached_with_recovery(model_name) {
        Ok(result) => Ok(result),
        Err(failure_summary) => {
            eprintln!("  {context}:");
            for line in failure_summary.lines() {
                eprintln!("    {line}");
            }
            Err(anyhow::anyhow!("{context}:\n{failure_summary}"))
        }
    }
}

/// Run the interactive simulation app.
pub fn run(args: SimArgs) -> std::result::Result<(), RunnerError> {
    eprintln!("rumoca sim");
    eprintln!("  Model: {}", args.model_name);
    eprintln!("  Solver: {}", solver_mode_label(args.solver_mode));
    eprintln!("  HTTP:  http://localhost:{}", args.http_port);
    eprintln!("  WS:   ws://localhost:{}", args.ws_port);
    if args.scene_script.is_some() {
        eprintln!("  Scene: custom");
    } else {
        eprintln!("  Scene: placeholder (pass --scene to render a vehicle)");
    }

    if let Some(schema) = &args.config.schema {
        for path_str in &schema.bfbs {
            eprintln!("  Schema: {path_str}");
        }
    } else {
        eprintln!("  Mode:   standalone (no external interface)");
    }

    // Compile Modelica model
    eprintln!("  Compiling model...");
    let mut session = Session::default();
    load_source_roots_into_session(&mut session, &args.source_roots)?;
    let model_uri = args
        .model_path
        .as_ref()
        .map(|path| path.to_string_lossy().to_string())
        .unwrap_or_else(|| format!("{}.mo", args.model_name));
    session
        .add_document(&model_uri, &args.model_source)
        .map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;
    let result = compile_model_with_diagnostics(
        &mut session,
        &args.model_name,
        "Failed to compile Modelica model",
    )?;

    let solver_mode = interactive_solver_mode(args.solver_mode);
    let mut sim_options = SimOptions {
        t_end: args.config.sim.t_end,
        dt: Some(args.config.sim.dt),
        solver_mode,
        diffsol_method: diffsol_method_for_solver_label(&args.solver_label),
        pacing_mode: args.config.effective_pacing_mode(),
        ..Default::default()
    };
    if let Some(atol) = args.atol {
        sim_options.atol = atol;
    }
    if let Some(rtol) = args.rtol {
        sim_options.rtol = rtol;
    }
    let mut session = SimulationSession::new_with_diagnostics(result.dae.as_ref(), sim_options)
        .map_err(|error| {
            RunnerError::simulation_diagnostic(
                "Failed to create simulation session",
                error,
                result.source_map.clone(),
            )
        })?;
    eprintln!("  Inputs: {:?}", session.input_names());

    // Start HTTP viewer server in background
    let http_port = args.http_port;
    let ws_port = args.ws_port;
    let scene_script = args.scene_script.clone();
    let scene_asset_dir = args.scene_asset_dir.clone();
    let debug = args.debug;
    let viewer_config = args
        .config
        .viewer
        .as_ref()
        .map(serde_json::to_value)
        .transpose()
        .context("Serialize viewer config")?
        .unwrap_or_else(|| serde_json::json!({}));
    thread::spawn(move || {
        if let Err(e) = crate::web::start_viewer_server(
            http_port,
            ws_port,
            scene_script.as_deref(),
            scene_asset_dir.as_deref(),
            &viewer_config,
            debug,
        ) {
            eprintln!("HTTP server error: {e}");
        }
    });

    // Run main sim loop (blocks)
    Ok(executor::run_sim_loop(
        &mut session,
        executor::SimLoopArgs {
            cfg: &args.config,
            http_port: args.http_port,
            ws_port: args.ws_port,
            debug: args.debug,
        },
    )?)
}

fn interactive_solver_mode(requested: SimSolverMode) -> SimSolverMode {
    requested
}

fn diffsol_method_for_solver_label(solver_label: &str) -> DiffsolMethod {
    DiffsolMethod::from_external_name(solver_label).unwrap_or_default()
}

fn solver_mode_label(mode: SimSolverMode) -> &'static str {
    match mode {
        SimSolverMode::Auto => "auto",
        SimSolverMode::Bdf => "bdf",
        SimSolverMode::RkLike => "rk-like",
    }
}
