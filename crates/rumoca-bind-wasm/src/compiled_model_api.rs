//! Retained compilation artifacts. Lowering belongs to compilation, never rendering.

use crate::{
    WasmError, build_compile_response, compile_requested_model, qualify_input_model_name,
    with_singleton_session,
};
use rumoca_compile::codegen::SolveTemplateRenderer;
use rumoca_compile::codegen::targets::{
    RenderedTargetFile, TargetBundle, TargetManifest, TargetTemplateIr, TargetTemplateSource,
    parse_target_manifest, render_dae_target_files, validate_dae_target_capabilities,
    validate_solve_target_capabilities_with_calls,
};
use rumoca_compile::compile::{
    CompilationResult, compile_phase_timing_stats, reset_compile_phase_timing_stats,
};
use serde::Deserialize;
use std::collections::BTreeMap;
use wasm_bindgen::prelude::*;

#[derive(Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
enum Representation {
    Dae,
    Solve,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct CompileOptions {
    #[serde(default = "default_representations")]
    representations: Vec<Representation>,
}

fn default_representations() -> Vec<Representation> {
    vec![Representation::Dae]
}

struct RetainedSolve {
    model: std::sync::Arc<rumoca_ir_solve::SolveModel>,
    renderer: SolveTemplateRenderer,
    json: String,
}

/// Checked artifacts owned by this WASM instance; call `free()` when replaced.
/// No global handle registry or cross-worker object transfer is needed.
#[wasm_bindgen]
pub struct WasmCompiledModel {
    compiled: CompilationResult,
    model_name: String,
    representations: Vec<Representation>,
    solve: Result<Option<RetainedSolve>, String>,
    compile_response: String,
}

/// Options use the current schema: `{"representations":["dae","solve"]}`.
#[wasm_bindgen]
pub fn compile_with_options(
    source: &str,
    model_name: &str,
    options_json: &str,
) -> Result<WasmCompiledModel, WasmError> {
    let options: CompileOptions = serde_json::from_str(options_json)
        .map_err(|error| WasmError::new(format!("Invalid compile options: {error}")))?;
    if options.representations.is_empty() {
        return Err(WasmError::new(
            "At least one representation must be requested",
        ));
    }
    with_singleton_session(|session| {
        reset_compile_phase_timing_stats();
        session.update_document("input.mo", source);
        let name = qualify_input_model_name(session, model_name);
        let compiled = compile_requested_model(session, &name)?;
        let compile_response = build_compile_response(&compiled, compile_phase_timing_stats())?;
        let solve = if options.representations.contains(&Representation::Solve) {
            prepare_solve(&compiled).map(Some)
        } else {
            Ok(None)
        };
        Ok(WasmCompiledModel {
            compiled,
            model_name: name,
            representations: options.representations,
            solve,
            compile_response,
        })
    })
}

fn prepare_solve(compiled: &CompilationResult) -> Result<RetainedSolve, String> {
    let options = rumoca_sim::SimOptions::default();
    let lowered =
        rumoca_sim::lower_correlated_for_simulation_with_overrides(&compiled.dae, &options)
            .map_err(|error| error.to_string())?;
    let model = std::sync::Arc::new(lowered.model().clone());
    let renderer =
        SolveTemplateRenderer::new_owned_model_with_dae(model.clone(), lowered.prepared_dae())
            .map_err(|error| error.to_string())?;
    let wire = rumoca_sim::solve_model_wire(lowered.model()).map_err(|error| error.to_string())?;
    let json = serde_json::to_string(&wire).map_err(|error| error.to_string())?;
    Ok(RetainedSolve {
        model,
        renderer,
        json,
    })
}

#[wasm_bindgen]
impl WasmCompiledModel {
    /// Exact canonical JSON, exposed only for requested representations.
    pub fn representation_json(&self, representation: &str) -> Result<String, WasmError> {
        match representation {
            "dae" if self.representations.contains(&Representation::Dae) => {
                serde_json::to_string(self.compiled.dae.as_ref())
                    .map_err(|error| WasmError::new(error.to_string()))
            }
            "solve" => Ok(self.retained_solve()?.json.clone()),
            "dae" => Err(WasmError::new("DAE representation was not requested")),
            _ => Err(WasmError::new(format!(
                "Unknown representation '{representation}'"
            ))),
        }
    }

    /// Original-system metrics, independently of reduced solver coordinates.
    pub fn analysis_json(&self) -> Result<String, WasmError> {
        let (equations, unknowns) = self.compiled.balance_detail.equations_unknowns();
        serde_json::to_string(&serde_json::json!({
            "balance": {
                "is_balanced": self.compiled.balance_detail.balance() == 0,
                "num_equations": equations, "num_unknowns": unknowns,
            },
            "solve_error": self.solve_error(),
        }))
        .map_err(|error| WasmError::new(error.to_string()))
    }

    /// Existing compile payload, only when its DAE was requested.
    pub fn compile_response_json(&self) -> Result<String, WasmError> {
        if !self.representations.contains(&Representation::Dae) {
            return Err(WasmError::new("DAE representation was not requested"));
        }
        Ok(self.compile_response.clone())
    }

    /// Lowering failure does not discard the original compiled DAE.
    pub fn solve_error(&self) -> Option<String> {
        self.solve.as_ref().err().cloned()
    }

    /// Render from retained artifacts, without compilation or lowering.
    pub fn render_target(
        &self,
        target: &str,
        manifest_source: &str,
        templates_json: &str,
    ) -> Result<String, WasmError> {
        let templates: BTreeMap<String, String> = serde_json::from_str(templates_json)
            .map_err(|error| WasmError::new(format!("Invalid templates: {error}")))?;
        let files = if manifest_source.trim().is_empty() {
            let bundle = TargetBundle::builtin(target)
                .ok_or_else(|| WasmError::new(format!("Unknown target '{target}'")))?;
            let manifest = bundle
                .parse_manifest()
                .map_err(|e| WasmError::new(e.to_string()))?;
            self.render_manifest(&manifest, &bundle)?
        } else {
            self.render_files(manifest_source, &templates)?
        };
        serde_json::to_string(&crate::WasmRenderedTarget { ok: true, files })
            .map_err(|error| WasmError::new(error.to_string()))
    }
}

impl WasmCompiledModel {
    fn retained_solve(&self) -> Result<&RetainedSolve, WasmError> {
        match &self.solve {
            Ok(Some(solve)) => Ok(solve),
            Ok(None) => Err(WasmError::new("Solve representation was not requested")),
            Err(error) => Err(WasmError::new(format!("Solve lowering failed: {error}"))),
        }
    }

    pub(crate) fn render_files(
        &self,
        manifest_source: &str,
        templates: &BTreeMap<String, String>,
    ) -> Result<Vec<RenderedTargetFile>, WasmError> {
        let manifest = parse_target_manifest(manifest_source)
            .map_err(|error| WasmError::new(error.to_string()))?;
        self.render_manifest(&manifest, templates)
    }

    fn render_manifest(
        &self,
        manifest: &TargetManifest,
        source: &impl TargetTemplateSource,
    ) -> Result<Vec<RenderedTargetFile>, WasmError> {
        match manifest.ir {
            TargetTemplateIr::Dae => {
                if !self.representations.contains(&Representation::Dae) {
                    return Err(WasmError::new("DAE representation was not requested"));
                }
                render_dae_target_files(source, manifest, &self.compiled.dae, &self.model_name)
                    .map_err(|error| WasmError::new(error.to_string()))
            }
            TargetTemplateIr::Solve => self.render_solve(manifest, source),
            _ => Err(WasmError::new(
                "Compiled model supports dae and solve targets",
            )),
        }
    }

    fn render_solve(
        &self,
        manifest: &TargetManifest,
        source: &impl TargetTemplateSource,
    ) -> Result<Vec<RenderedTargetFile>, WasmError> {
        let retained = self.retained_solve()?;
        let capabilities = manifest
            .capabilities
            .as_ref()
            .ok_or_else(|| WasmError::new("Solve target must declare capabilities"))?;
        validate_dae_target_capabilities(&self.compiled.dae, manifest, capabilities)
            .map_err(|error| WasmError::new(error.to_string()))?;
        validate_solve_target_capabilities_with_calls(
            &retained.model.problem,
            Some(&retained.model.pure_calls),
            manifest,
            capabilities,
        )
        .map_err(|error| WasmError::new(error.to_string()))?;
        if capabilities.scalar_fallback == Some(true) {
            retained
                .renderer
                .require_scalar_derivative_programs()
                .map_err(|error| WasmError::new(error.to_string()))?;
        }
        manifest
            .files
            .iter()
            .map(|file| {
                let template = source
                    .template_source(&file.template)
                    .map_err(|error| WasmError::new(error.to_string()))?;
                let path = retained
                    .renderer
                    .render_with_name(&file.path, &self.model_name)
                    .map_err(|error| WasmError::new(error.to_string()))?;
                let content = retained
                    .renderer
                    .render_with_name(template.as_ref(), &self.model_name)
                    .map_err(|error| WasmError::new(error.to_string()))?;
                Ok(RenderedTargetFile { path, content })
            })
            .collect()
    }
}
