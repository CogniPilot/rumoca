//! Browser code generation: renders built-in and workspace targets.
//!
//! `render_target` renders from a checked DAE handed back as JSON (Dae,
//! Algorithm Code and Solve targets). The `*_for_source` /
//! `*_with_workspace_sources` entry points compile in the session and render
//! from the in-memory result, which also carries the Flat model and avoids
//! re-encoding the DAE through JavaScript numbers.

#[cfg(test)]
mod source_preview_tests;

use std::borrow::Cow;
use std::collections::BTreeMap;

use anyhow::{Context, bail};
#[cfg(target_arch = "wasm32")]
use serde::Serialize;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

use rumoca_compile::codegen::targets::{
    RenderedTargetFile, TargetBundle, TargetManifest, TargetTemplateIr, TargetTemplateSource,
    builtin_target_descriptors_for_ir, ensure_target_has_rendered_files, parse_target_manifest,
    render_dae_target_files, safe_target_join, validate_dae_target_capabilities,
    validate_flat_target_capabilities,
};
use rumoca_compile::codegen::{AlgorithmCodeTemplateRenderer, render_flat_template_with_name};
use rumoca_compile::compile::{Dae, FlatModel};

#[cfg(target_arch = "wasm32")]
use crate::source_root_api::load_workspace_sources_in_session;
use crate::{WasmError, compile_requested_model, qualify_input_model_name};
#[cfg(target_arch = "wasm32")]
use crate::{serialize_js_value, with_singleton_session};

/// Solve lowering ships with the simulation runtime; light builds without it
/// cannot render Solve targets.
const SOLVE_TARGETS_AVAILABLE: bool = cfg!(any(
    feature = "sim-wasm",
    feature = "sim-diffsol",
    feature = "sim-rk45",
    feature = "native-assignments"
));

#[cfg(target_arch = "wasm32")]
#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct WasmRenderedTarget {
    ok: bool,
    files: Vec<RenderedTargetFile>,
}

enum TargetTemplates {
    Builtin(TargetBundle),
    Custom(BTreeMap<String, String>),
}

impl TargetTemplateSource for TargetTemplates {
    fn template_source<'a>(&'a self, template: &str) -> anyhow::Result<Cow<'a, str>> {
        match self {
            Self::Builtin(bundle) => bundle.template_source(template),
            Self::Custom(templates) => templates.template_source(template),
        }
    }
}

fn browser_renderable_ir(ir: TargetTemplateIr) -> bool {
    match ir {
        TargetTemplateIr::Dae | TargetTemplateIr::AlgorithmCode | TargetTemplateIr::Flat => true,
        TargetTemplateIr::Solve => SOLVE_TARGETS_AVAILABLE,
        TargetTemplateIr::Fmi | TargetTemplateIr::Ast => false,
    }
}

/// Packaged targets and runtime asset bundles need the CLI's filesystem
/// writer. Algorithm Code targets are exempt: the browser previews their
/// source files only (see `render_algorithm_code_source_files`).
fn needs_cli_packaging(manifest: &TargetManifest) -> bool {
    manifest.ir != TargetTemplateIr::AlgorithmCode
        && (manifest.package.is_some() || !manifest.assets.is_empty())
}

fn browser_renderable_manifest(manifest: &TargetManifest) -> bool {
    browser_renderable_ir(manifest.ir) && !needs_cli_packaging(manifest)
}

fn browser_builtin_target_descriptors()
-> Vec<rumoca_compile::codegen::targets::BuiltinTargetDescriptor> {
    [
        TargetTemplateIr::Dae,
        TargetTemplateIr::Flat,
        TargetTemplateIr::Solve,
        TargetTemplateIr::AlgorithmCode,
    ]
    .into_iter()
    .flat_map(builtin_target_descriptors_for_ir)
    .filter(|descriptor| {
        parse_target_manifest(descriptor.manifest)
            .is_ok_and(|manifest| browser_renderable_manifest(&manifest))
    })
    .collect()
}

/// Get the built-in codegen targets this WASM build can render.
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn get_builtin_targets() -> Result<JsValue, WasmError> {
    serialize_js_value(
        &browser_builtin_target_descriptors(),
        "Serialize built-in target descriptors",
    )
}

fn resolve_target(
    target: &str,
    manifest_source: &str,
    templates_json: &str,
) -> anyhow::Result<(TargetTemplates, TargetManifest)> {
    if manifest_source.trim().is_empty() {
        let bundle = TargetBundle::builtin(target)
            .with_context(|| format!("Unknown built-in target '{target}'"))?;
        let manifest = bundle.parse_manifest()?;
        return Ok((TargetTemplates::Builtin(bundle), manifest));
    }
    let templates: BTreeMap<String, String> =
        serde_json::from_str(templates_json).context("Invalid target template map")?;
    let manifest = parse_target_manifest(manifest_source)?;
    Ok((TargetTemplates::Custom(templates), manifest))
}

fn target_label<'a>(target: &'a str, manifest: &'a TargetManifest) -> &'a str {
    manifest.name.as_deref().unwrap_or(target)
}

pub(crate) fn render_target_files(
    target: &str,
    manifest_source: &str,
    templates_json: &str,
    dae: &Dae,
    flat: Option<&FlatModel>,
    model_name: &str,
) -> anyhow::Result<Vec<RenderedTargetFile>> {
    let (templates, manifest) = resolve_target(target, manifest_source, templates_json)?;
    let label = target_label(target, &manifest);
    if needs_cli_packaging(&manifest) {
        bail!(
            "target '{label}' packages an archive or runtime assets, which the browser cannot \
             write; generate it with the rumoca CLI"
        );
    }
    match manifest.ir {
        TargetTemplateIr::Dae => render_dae_target_files(&templates, &manifest, dae, model_name),
        TargetTemplateIr::AlgorithmCode => {
            render_algorithm_code_source_files(&templates, &manifest, dae, model_name)
        }
        TargetTemplateIr::Solve => {
            render_solve_target_files(&templates, &manifest, dae, model_name)
        }
        TargetTemplateIr::Flat => {
            let flat = flat.with_context(|| {
                format!(
                    "flat target '{label}' renders from model source; call \
                     render_target_for_source or render_target_with_workspace_sources"
                )
            })?;
            render_flat_target_files(&templates, &manifest, flat, model_name)
        }
        ir @ (TargetTemplateIr::Fmi | TargetTemplateIr::Ast) => bail!(
            "{ir:?} target '{label}' is not available in the browser; generate it with the \
             rumoca CLI"
        ),
    }
}

/// Render every `[[files]]` entry (output path and content) through `render`.
fn render_manifest_files(
    templates: &impl TargetTemplateSource,
    manifest: &TargetManifest,
    render: impl Fn(&str) -> anyhow::Result<String>,
) -> anyhow::Result<Vec<RenderedTargetFile>> {
    ensure_target_has_rendered_files(manifest)?;
    manifest
        .files
        .iter()
        .map(|file| {
            let path = render(&file.path)
                .with_context(|| format!("Render target output path '{}'", file.path))?;
            let template = templates.template_source(&file.template)?;
            let content = render(template.as_ref())
                .with_context(|| format!("Render target template '{}'", file.template))?;
            Ok(RenderedTargetFile {
                path: path.trim().to_owned(),
                content,
            })
        })
        .collect()
}

/// Mirrors the CLI's Solve path: the checked DAE and the lowered Solve
/// problem both pass the target's capability table before anything renders.
#[cfg(any(
    feature = "sim-wasm",
    feature = "sim-diffsol",
    feature = "sim-rk45",
    feature = "native-assignments"
))]
fn render_solve_target_files(
    templates: &impl TargetTemplateSource,
    manifest: &TargetManifest,
    dae: &Dae,
    model_name: &str,
) -> anyhow::Result<Vec<RenderedTargetFile>> {
    use rumoca_compile::codegen::SolveTemplateRenderer;
    use rumoca_compile::codegen::targets::validate_solve_target_capabilities_with_calls;

    let capabilities = manifest
        .capabilities
        .as_ref()
        .context("Solve target manifest must declare a [capabilities] table")?;
    validate_dae_target_capabilities(dae, manifest, capabilities)?;
    // As in the CLI, the pure-call owners lowered with the problem let the
    // scalar targets inline `PureCall` ops; the gate reads the same pair.
    let package = rumoca_sim::lower_solve_package(dae).context("Lower checked DAE to Solve IR")?;
    validate_solve_target_capabilities_with_calls(
        &package.problem,
        Some(&package.pure_calls),
        manifest,
        capabilities,
    )?;
    // `wgsl-ode` never reads the DAE entry or solver artifacts (as in the CLI).
    let renderer = if manifest.name.as_deref() == Some("wgsl-ode") {
        SolveTemplateRenderer::new_owned_with_calls(
            package.problem,
            rumoca_ir_solve::SolveArtifacts::default(),
            package.pure_calls,
            "",
        )?
    } else {
        let artifacts = rumoca_sim::lower_solve_artifacts(&package.problem)
            .context("Lower checked Solve artifacts")?;
        SolveTemplateRenderer::new_owned_with_dae_and_calls(
            package.problem,
            artifacts,
            package.pure_calls,
            dae,
        )?
    };
    if capabilities.scalar_fallback == Some(true) {
        renderer.require_scalar_derivative_programs()?;
    }
    render_manifest_files(templates, manifest, |template| {
        Ok(renderer.render_with_name(template, model_name)?)
    })
}

#[cfg(not(any(
    feature = "sim-wasm",
    feature = "sim-diffsol",
    feature = "sim-rk45",
    feature = "native-assignments"
)))]
fn render_solve_target_files(
    _templates: &impl TargetTemplateSource,
    manifest: &TargetManifest,
    _dae: &Dae,
    _model_name: &str,
) -> anyhow::Result<Vec<RenderedTargetFile>> {
    bail!(
        "Solve target '{}' needs a WASM build with the simulation runtime (full-web)",
        manifest.name.as_deref().unwrap_or("<unnamed>")
    )
}

fn render_flat_target_files(
    templates: &impl TargetTemplateSource,
    manifest: &TargetManifest,
    flat: &FlatModel,
    model_name: &str,
) -> anyhow::Result<Vec<RenderedTargetFile>> {
    if let Some(capabilities) = manifest.capabilities.as_ref() {
        validate_flat_target_capabilities(flat, manifest, capabilities)?;
    }
    render_manifest_files(templates, manifest, |template| {
        Ok(render_flat_template_with_name(flat, template, model_name)?)
    })
}

fn render_algorithm_code_source_files(
    templates: &impl TargetTemplateSource,
    manifest: &TargetManifest,
    dae: &Dae,
    model_name: &str,
) -> anyhow::Result<Vec<RenderedTargetFile>> {
    let capabilities = manifest
        .capabilities
        .as_ref()
        .context("Algorithm Code target manifest must declare a [capabilities] table")?;
    validate_dae_target_capabilities(dae, manifest, capabilities)?;
    ensure_target_has_rendered_files(manifest)?;
    let model_id = model_name.replace('.', "_");
    let package = rumoca_phase_galec::lower_to_algorithm_code(
        &rumoca_phase_galec::GalecInput::new(dae, &model_id),
        &rumoca_phase_galec::GalecOptions::default(),
    )
    .map_err(|diagnostics| {
        anyhow::anyhow!(
            "GALEC projection failed: {}",
            diagnostics
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("; ")
        )
    })?;
    let renderer = AlgorithmCodeTemplateRenderer::new(&package, dae.source_map())?;
    let mut files = Vec::new();
    for file in &manifest.files {
        let path = renderer.render_with_name(&file.path, &model_id)?;
        let path = path.trim();
        safe_target_join(std::path::Path::new("."), path)?;
        let extension = std::path::Path::new(path)
            .extension()
            .and_then(|value| value.to_str());
        if !matches!(extension, Some("alg" | "h" | "c")) {
            continue;
        }
        let template = templates.template_source(&file.template)?;
        let content = renderer.render_with_name(template.as_ref(), &model_id)?;
        files.push(RenderedTargetFile {
            path: path.trim().to_owned(),
            content,
        });
    }
    if files.is_empty() {
        bail!("Algorithm Code source preview contains no source files (.alg, .h or .c)");
    }
    Ok(files)
}

#[cfg(target_arch = "wasm32")]
fn rendered_target_js_value(
    files: anyhow::Result<Vec<RenderedTargetFile>>,
) -> Result<JsValue, WasmError> {
    let files = files.map_err(|e| WasmError::new(format!("Target render failed: {e:#}")))?;
    serde_wasm_bindgen::to_value(&WasmRenderedTarget { ok: true, files })
        .map_err(|e| WasmError::new(format!("Serialize target output: {e}")))
}

/// Render a target from a checked DAE serialized by `compile`. Flat targets
/// need the model source; use [`render_target_for_source`] for those.
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn render_target(
    dae_json: &str,
    model_name: &str,
    target: &str,
    manifest_source: &str,
    templates_json: &str,
) -> Result<JsValue, WasmError> {
    let dae = serde_json::from_str::<Dae>(dae_json)
        .map_err(|e| WasmError::new(format!("Invalid DAE JSON: {e}")))?;
    rendered_target_js_value(render_target_files(
        target,
        manifest_source,
        templates_json,
        &dae,
        None,
        model_name,
    ))
}

pub(crate) fn render_target_in_session(
    session: &mut rumoca_compile::Session,
    source: &str,
    model_name: &str,
    target: &str,
    manifest_source: &str,
    templates_json: &str,
) -> Result<Vec<RenderedTargetFile>, WasmError> {
    session.update_document("input.mo", source);
    let requested_model = qualify_input_model_name(session, model_name);
    let result = compile_requested_model(session, &requested_model)?;
    render_target_files(
        target,
        manifest_source,
        templates_json,
        &result.dae,
        Some(&result.flat),
        model_name,
    )
    .map_err(|e| WasmError::new(format!("Target render failed: {e:#}")))
}

/// Compile `model_name` from `source` and render a target from the result.
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn render_target_for_source(
    source: &str,
    model_name: &str,
    target: &str,
    manifest_source: &str,
    templates_json: &str,
) -> Result<JsValue, WasmError> {
    let files = with_singleton_session(|session| {
        render_target_in_session(
            session,
            source,
            model_name,
            target,
            manifest_source,
            templates_json,
        )
    })?;
    rendered_target_js_value(Ok(files))
}

/// Like [`render_target_for_source`], resolving against the given workspace
/// sources (the same map `compile_with_workspace_sources` takes).
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn render_target_with_workspace_sources(
    source: &str,
    model_name: &str,
    workspace_sources_json: &str,
    target: &str,
    manifest_source: &str,
    templates_json: &str,
) -> Result<JsValue, WasmError> {
    let files = with_singleton_session(|session| {
        load_workspace_sources_in_session(session, workspace_sources_json)?;
        render_target_in_session(
            session,
            source,
            model_name,
            target,
            manifest_source,
            templates_json,
        )
    })?;
    rendered_target_js_value(Ok(files))
}

#[cfg(test)]
mod tests {
    use super::*;

    const DECAY: &str = "model Decay\n  Real x(start = 1);\n  parameter Real k = 0.5;\nequation\n  der(x) = -k * x;\nend Decay;\n";

    fn render(target: &str) -> anyhow::Result<Vec<RenderedTargetFile>> {
        let mut session = rumoca_compile::Session::default();
        render_target_in_session(&mut session, DECAY, "Decay", target, "", "{}")
            .map_err(|e| anyhow::anyhow!("{e:?}"))
    }

    #[test]
    fn browser_target_list_covers_solve_and_flat_but_not_packaged_targets() {
        let ids: Vec<String> = browser_builtin_target_descriptors()
            .into_iter()
            .map(|descriptor| descriptor.id)
            .collect();
        for expected in ["dae-modelica", "base-modelica", "galec"] {
            assert!(
                ids.iter().any(|id| id == expected),
                "missing {expected}: {ids:?}"
            );
        }
        assert_eq!(
            ids.iter().any(|id| id == "c-ode"),
            SOLVE_TARGETS_AVAILABLE,
            "{ids:?}"
        );
        for excluded in ["mlir", "fmi2", "fmi3"] {
            assert!(
                !ids.iter().any(|id| id == excluded),
                "{excluded} listed: {ids:?}"
            );
        }
    }

    #[test]
    fn source_render_produces_flat_target_files() {
        let files = render("base-modelica").expect("base-modelica renders");
        assert!(
            files.iter().any(|file| file.content.contains("Decay")),
            "{files:?}"
        );
    }

    #[cfg(any(
        feature = "sim-wasm",
        feature = "sim-diffsol",
        feature = "sim-rk45",
        feature = "native-assignments"
    ))]
    #[test]
    fn source_render_produces_solve_target_files() {
        for target in ["c-ode", "jax-ode", "casadi-ode", "rust-ode"] {
            let files = render(target).unwrap_or_else(|e| panic!("{target}: {e:#}"));
            assert!(!files.is_empty(), "{target} rendered no files");
            assert!(
                files
                    .iter()
                    .all(|file| !file.path.is_empty() && !file.content.is_empty()),
                "{target}: {files:?}"
            );
        }
    }

    #[test]
    fn dae_json_render_explains_flat_targets_need_source() {
        let mut session = rumoca_compile::Session::default();
        session.update_document("input.mo", DECAY);
        let model = qualify_input_model_name(&session, "Decay");
        let result = compile_requested_model(&mut session, &model).expect("compiles");
        let error = render_target_files("base-modelica", "", "{}", &result.dae, None, "Decay")
            .expect_err("flat target needs the flat model");
        assert!(
            error.to_string().contains("renders from model source"),
            "{error:#}"
        );
    }
}
