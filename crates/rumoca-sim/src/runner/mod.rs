//! Lockstep simulation runtime driven by a `SimulationConfig` TOML.
//!
//! Axis crates (input, transport, codec, solver) are composed here into a
//! running app. Optionally couples to an external autopilot over UDP with
//! a configured codec; otherwise runs standalone.

pub mod compose;
pub mod config;
pub mod executor;

/// Template TOML printed by `rumoca lockstep init`.
pub const CONFIG_TEMPLATE: &str = include_str!("template.toml");

use std::path::PathBuf;
use std::thread;

use crate::runner::config::SimulationConfig;
use anyhow::{Context, Result};
use rumoca_compile::compile::{CompilationResult, PhaseResult, Session, SourceRootKind};
use rumoca_compile::source_roots::{
    parse_source_root_with_cache, resolve_source_root_cache_dir, source_root_source_set_key,
};
use rumoca_solver_diffsol::{SimStepper, StepperOptions};

/// Arguments for the `rumoca lockstep run` command.
pub struct SimArgs {
    /// Modelica source code content.
    pub model_source: String,
    /// Model name to simulate.
    pub model_name: String,
    /// Parsed lockstep app configuration.
    pub config: SimulationConfig,
    /// HTTP server port.
    pub http_port: u16,
    /// WebSocket viz port.
    pub ws_port: u16,
    /// Scene script content (None = minimal placeholder scene).
    pub scene_script: Option<String>,
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
) -> Result<CompilationResult> {
    let mut report = session.compile_model_strict_reachable_with_recovery(model_name);
    let failure_summary = report.failure_summary(usize::MAX);
    match report.requested_result.take() {
        Some(PhaseResult::Success(result)) if report.failures.is_empty() => Ok(*result),
        _ => {
            eprintln!("  {context}:");
            for line in failure_summary.lines() {
                eprintln!("    {line}");
            }
            Err(anyhow::anyhow!("{context}:\n{failure_summary}"))
        }
    }
}

/// Run the lockstep simulation app.
pub fn run(args: SimArgs) -> Result<()> {
    eprintln!("rumoca lockstep");
    eprintln!("  Model: {}", args.model_name);
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
        eprintln!("  Mode:   standalone (no autopilot coupling)");
    }

    // Compile Modelica model
    eprintln!("  Compiling model...");
    let mut session = Session::default();
    load_source_roots_into_session(&mut session, &args.source_roots)?;
    session
        .add_document(&format!("{}.mo", args.model_name), &args.model_source)
        .map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;
    let result = compile_model_with_diagnostics(
        &mut session,
        &args.model_name,
        "Failed to compile Modelica model",
    )?;

    let mut stepper = SimStepper::new(
        &result.dae,
        StepperOptions {
            rtol: 1e-3,
            atol: 1e-3,
            nominal_dt: Some(args.config.sim.dt),
            ..Default::default()
        },
    )
    .context("Failed to create simulation stepper")?;
    eprintln!("  Inputs: {:?}", stepper.input_names());

    // Start HTTP viewer server in background
    let http_port = args.http_port;
    let ws_port = args.ws_port;
    let scene_script = args.scene_script.clone();
    let debug = args.debug;
    thread::spawn(move || {
        if let Err(e) =
            rumoca_viz_web::start_viewer_server(http_port, ws_port, scene_script.as_deref(), debug)
        {
            eprintln!("HTTP server error: {e}");
        }
    });

    eprintln!("  Open http://localhost:{} in a browser.", args.http_port);

    // Run main sim loop (blocks)
    executor::run_sim_loop(
        &args.config,
        &mut stepper,
        &args.model_source,
        &args.model_name,
        &args.source_roots,
        args.ws_port,
        args.debug,
    )
}
