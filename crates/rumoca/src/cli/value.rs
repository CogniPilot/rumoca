//! Value-returning entrypoints for the reusable CLI.
//!
//! These mirror the CLI's compute paths (the same `Compiler` engine, model
//! inference, source-root merge, IR/codegen rendering) but return structured
//! JSON instead of printing/writing files, so the Python `cli` binding can run
//! "verbatim CLI" operations and get data back. The clap arg types are shared
//! with the binary, so there is zero drift between what the binary parses and
//! what the binding parses.
//!
//! Compilation runs fully in memory (`Compiler::compile_str*`), keyed under the
//! positional file name so source-span identifiers match what the equivalent
//! one-shot Python `compile`/`simulate` calls produce. This is a child module of
//! [`crate::cli`], so it reuses `cli`'s private compute helpers via `super::`.

use std::path::Path;
use std::time::Instant;

use anyhow::{Result, bail};
use serde_json::{Value, json};

use super::{
    CompilationResult, CompileArgs, CompilePhase, EarlyIrArtifact, SimCommandArgs, SimOptions,
    SimulationRequestSummary, SimulationRunMetrics, TemplateIr,
    compile_str_dae_with_inferred_model, compile_str_early_ir_with_inferred_model,
    compile_str_with_inferred_model, diffsol_method_for_solver_label, direct_sim_defaults,
    render_early_ir_as_modelica_ast, render_early_ir_as_modelica_flat, render_ir_as_modelica,
    simulate_solver_or_auto, target_manifest,
};

/// Compile `source` (inline Modelica text) according to `args` and return the
/// result as a [`serde_json::Value`] instead of printing/writing files.
///
/// JSON shapes by mode:
/// - default (no `--emit`/`--target`/`--inspect`): the DAE as JSON (the same
///   `result.dae` serialization the Python `compile_source` returns).
/// - `--emit <stage>-json`: the IR JSON for that stage, parsed into a `Value`.
/// - `--emit <stage>-mo`: `{"format":"modelica","source": "<rendered>"}`.
/// - `--target <NAME>`: `{"target":"<NAME>","files":[{"path":..,"content":..}, ...]}`.
/// - `--inspect ...`: returned as an error (the inspect codepaths only print).
pub fn compile_to_value(args: &CompileArgs, source: &str) -> Result<Value> {
    if let Some(kind) = args.inspect {
        bail!(
            "compile --inspect {:?} is not available as structured data; \
             it only writes a human-readable report to stdout",
            kind
        );
    }

    let file_name = source_file_name(&args.input.model_file);
    let options = &args.input.options;

    // Early IR (--emit ast-*/flat-*) doesn't lower to the DAE.
    if let Some(emit) = args.emit
        && matches!(emit.phase(), CompilePhase::Ast | CompilePhase::Flat)
    {
        let (artifact, model) = compile_str_early_ir_with_inferred_model(
            source,
            file_name,
            options,
            emit.phase(),
            args.diagnostics.verbose,
        )?;
        return early_ir_value(&artifact, &model, emit.is_json());
    }

    let (result, model) =
        compile_str_with_inferred_model(source, file_name, options, args.diagnostics.verbose)?;

    match (args.emit, args.target.as_deref()) {
        (Some(emit), _) => ir_value(&result, &model, emit.phase(), emit.is_json()),
        (None, Some(target)) => {
            target_value(&result, &model, target, args.phase.map(TemplateIr::from))
        }
        (None, None) => serde_json::to_value(&result.dae)
            .map_err(|e| anyhow::anyhow!("serialize DAE to JSON: {e}")),
    }
}

fn early_ir_value(artifact: &EarlyIrArtifact, model: &str, json: bool) -> Result<Value> {
    if json {
        return match artifact {
            EarlyIrArtifact::Ast(resolved) => serde_json::to_value(resolved.inner())
                .map_err(|e| anyhow::anyhow!("serialize AST to JSON: {e}")),
            EarlyIrArtifact::Flat(flat) => serde_json::to_value(flat.as_ref())
                .map_err(|e| anyhow::anyhow!("serialize flat model to JSON: {e}")),
        };
    }
    let rendered = match artifact {
        EarlyIrArtifact::Ast(resolved) => render_early_ir_as_modelica_ast(resolved, model)?,
        EarlyIrArtifact::Flat(flat) => render_early_ir_as_modelica_flat(flat, model)?,
    };
    Ok(json!({ "format": "modelica", "source": rendered }))
}

fn ir_value(
    result: &CompilationResult,
    model: &str,
    phase: CompilePhase,
    json: bool,
) -> Result<Value> {
    if json {
        let rendered = result.to_ir_json(phase.into())?;
        return serde_json::from_str(&rendered).map_err(|e| anyhow::anyhow!("parse IR JSON: {e}"));
    }
    let rendered = render_ir_as_modelica(result, model, phase)?;
    Ok(json!({ "format": "modelica", "source": rendered }))
}

fn target_value(
    result: &CompilationResult,
    model: &str,
    target: &str,
    phase: Option<TemplateIr>,
) -> Result<Value> {
    let files = target_manifest::render_target_files(result, model, target, phase)?;
    let files_json = files
        .into_iter()
        .map(|file| json!({ "path": file.path, "content": file.content }))
        .collect::<Vec<_>>();
    Ok(json!({ "target": target, "files": files_json }))
}

/// Compile + simulate `source` according to `args` and return the simulation
/// result as a [`serde_json::Value`] (the same `{model, payload, metrics}`
/// shape the Python `simulate` binding produces).
///
/// `--inspect`/`--config`/`sim` subcommands are not supported here (the binding
/// only needs direct simulation of inline source); they return an error.
pub fn simulate_to_value(args: &SimCommandArgs, source: &str) -> Result<Value> {
    if args.command.is_some() {
        bail!("sim subcommands (check/init/bench) are not available as structured data");
    }
    if args.config.is_some() {
        bail!("sim --config scenario runs are not available as structured data");
    }
    if let Some(kind) = args.inspect {
        bail!(
            "sim --inspect {:?} is not available as structured data; \
             it only writes a human-readable report to stdout",
            kind
        );
    }

    let file_name = match &args.model_file {
        Some(model_file) => source_file_name(model_file),
        None => "input.mo",
    };
    let compile_started = Instant::now();
    let (result, model) = compile_str_dae_with_inferred_model(
        source,
        file_name,
        &args.model_options,
        args.diagnostics.verbose,
    )?;
    let compile_seconds = compile_started.elapsed().as_secs_f64();
    let solver = simulate_solver_or_auto(args.solver, result.experiment_solver.as_deref());
    let sim_defaults = direct_sim_defaults(args.t_end, args.dt, args.atol, args.rtol, &result);

    let opts = SimOptions {
        t_start: sim_defaults.t_start,
        t_end: sim_defaults.t_end,
        dt: sim_defaults.dt,
        atol: sim_defaults.atol,
        rtol: sim_defaults.rtol,
        solver_mode: solver.into(),
        diffsol_method: diffsol_method_for_solver_label(solver.as_label()),
        ..SimOptions::default()
    };

    let sim_started = Instant::now();
    // Dispatch on `opts.solver_mode` (auto / bdf / rk-like) exactly like the
    // binary's direct-sim path. The plain `simulate_dae` alias resolves to the
    // diffsol/BDF-only entry, so it would silently ignore `--solver`.
    let sim = rumoca_sim::simulate_dae_with_diagnostics(result.dae.as_ref(), &opts)
        .map_err(|e| anyhow::anyhow!("Simulation error: {e}"))?;

    let request = SimulationRequestSummary {
        solver: solver.as_label().to_string(),
        t_start: opts.t_start,
        t_end: opts.t_end,
        dt: opts.dt,
        rtol: opts.rtol,
        atol: opts.atol,
    };
    // Match the Python `simulate` binding's metrics so `cli sim` output is
    // consistent (timing populated, not left at the zero default).
    let metrics = SimulationRunMetrics {
        compile_seconds: Some(compile_seconds),
        simulate_seconds: Some(sim_started.elapsed().as_secs_f64()),
        ..SimulationRunMetrics::default()
    };
    Ok(json!({
        "model": model,
        "payload": rumoca_sim::build_simulation_payload(&sim, &request, &metrics),
        "metrics": rumoca_sim::build_simulation_metrics_value(&sim, &metrics),
    }))
}

/// Resolve the document name used to key in-memory compilation: the positional's
/// own `*.mo` file name (so model inference by file stem and source spans match
/// the file-based path), or a generic `input.mo` when the positional is a bare
/// token without a `.mo` extension.
fn source_file_name(model_file: &str) -> &str {
    Path::new(model_file)
        .file_name()
        .and_then(|name| name.to_str())
        .filter(|name| name.ends_with(".mo"))
        .unwrap_or("input.mo")
}
