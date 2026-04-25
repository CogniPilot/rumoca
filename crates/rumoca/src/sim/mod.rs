//! Lockstep simulation runtime driven by a `SimulationConfig` TOML.
//!
//! Axis crates (input, transport, codec, solver) are composed here into a
//! running app. Optionally couples to an external autopilot over UDP with
//! a configured codec; otherwise runs standalone.

pub(crate) mod compose;
pub(crate) mod config;
pub(crate) mod executor;

use std::path::Path;
use std::thread;

use crate::sim::config::SimulationConfig;
use anyhow::{Context, Result};
use rumoca_codec_flatbuffers::bfbs::SchemaSet;
use rumoca_compile::compile::Session;
use rumoca_solver_diffsol::{SimStepper, StepperOptions};

/// Arguments for the `rumoca lockstep run` command.
pub(crate) struct SimArgs {
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
    /// Enable debug features (overlays, log downloads).
    pub debug: bool,
}

/// Run the lockstep simulation app.
pub(crate) fn run(args: SimArgs) -> Result<()> {
    eprintln!("rumoca lockstep");
    eprintln!("  Model: {}", args.model_name);
    eprintln!("  HTTP:  http://localhost:{}", args.http_port);
    eprintln!("  WS:   ws://localhost:{}", args.ws_port);
    if args.scene_script.is_some() {
        eprintln!("  Scene: custom");
    } else {
        eprintln!("  Scene: placeholder (pass --scene to render a vehicle)");
    }

    // Load FlatBuffer schemas (only when configured for autopilot coupling).
    let schema_set = match args.config.schema.as_ref() {
        Some(schema_cfg) => {
            let mut ss = SchemaSet::new();
            for path_str in &schema_cfg.bfbs {
                ss.load_bfbs(Path::new(path_str))
                    .with_context(|| format!("Load schema: {path_str}"))?;
                eprintln!("  Schema: {path_str}");
            }
            Some(ss)
        }
        None => {
            eprintln!("  Mode:   standalone (no autopilot coupling)");
            None
        }
    };

    // Compile Modelica model
    eprintln!("  Compiling model...");
    let mut session = Session::default();
    session
        .add_document(&format!("{}.mo", args.model_name), &args.model_source)
        .map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;
    let result = session
        .compile_model(&args.model_name)
        .context("Failed to compile Modelica model")?;

    let mut stepper = SimStepper::new(
        &result.dae,
        StepperOptions {
            rtol: 1e-3,
            atol: 1e-3,
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
        schema_set.as_ref(),
        &mut stepper,
        &args.model_source,
        &args.model_name,
        args.ws_port,
        args.debug,
    )
}
