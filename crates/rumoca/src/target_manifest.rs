use std::path::Path;
#[cfg(feature = "scheduled-sim")]
use std::path::PathBuf;
#[cfg(all(test, feature = "scheduled-sim"))]
use std::process::Command;

use crate::{CompilationResult, TemplateIr, error::CompilerError};
use anyhow::{Context, Result, bail};
use rumoca_compile::codegen::targets::{
    RenderedTargetFile, TargetBuildKind, TargetBundle, TargetCapabilities, TargetFileRenderContext,
    TargetManifest, TargetTemplateIr, TargetTemplateSource, TensorCapability,
    ensure_target_has_rendered_files, validate_dae_target_capabilities,
};
#[cfg(feature = "scheduled-sim")]
use rumoca_compile::codegen::targets::{TargetFile, safe_target_join};
use rumoca_compile::compile::core::{Diagnostic as CommonDiagnostic, PrimaryLabel, SourceMap};
use rumoca_compile::galec::{GalecExportError, GalecTargetError};

#[cfg(feature = "scheduled-sim")]
pub(crate) fn compile_target(
    result: &CompilationResult,
    model: &str,
    target: &str,
    output: Option<PathBuf>,
    phase: Option<TemplateIr>,
    template_overrides: &[String],
) -> Result<()> {
    if raw_template_target(target) {
        if !template_overrides.is_empty() {
            bail!(
                "--template overrides apply to a manifest target such as `--target galec`; \
                 a raw .jinja target is already the selected template"
            );
        }
        // A raw .jinja receives the IR chosen by --phase (default DAE).
        return compile_raw_template_target(
            result,
            model,
            target,
            output,
            phase.unwrap_or(TemplateIr::Dae),
        );
    }
    let (bundle, manifest) = resolve_manifest_target(target, phase)?;
    let bundle = apply_template_overrides(bundle, &manifest, template_overrides)?;
    compile_manifest_target(result, model, &bundle, &manifest, output)
}

/// Apply the `--phase`/`-ir`/unknown-target guards and load a built-in or
/// directory target's bundle + manifest.
///
/// Shared by the file-writing [`compile_target`] and the in-memory
/// [`render_target_files`] so the two paths can never disagree on which targets
/// are valid. Only reached for non-raw targets (the caller handles `.jinja`).
fn resolve_manifest_target(
    target: &str,
    phase: Option<TemplateIr>,
) -> Result<(TargetBundle, TargetManifest)> {
    // --phase only picks the IR fed to a raw .jinja template; a built-in /
    // directory target dictates its own IR.
    if phase.is_some() {
        bail!(
            "--phase only applies to a raw .jinja --target (it picks the IR fed to the \
             template); the code-gen target '{target}' dictates its own IR."
        );
    }
    // The old `*-ir` pseudo-targets are now `--emit <stage>-json` / `<stage>-mo`.
    if let Some(stage) = target.strip_suffix("-ir") {
        bail!(
            "`--target {target}` was removed; dump the IR with `--emit {stage}-json` \
             (or `--emit {stage}-mo` for Modelica)."
        );
    }
    // Distinguish an unknown target from a real built-in / directory before the
    // loader emits a cryptic `<target>/target.toml: No such file` error.
    if TargetBundle::builtin(target).is_none() && !Path::new(target).is_dir() {
        bail!(
            "unknown target '{target}'. Run `rumoca targets` to list built-in targets, \
             or pass a directory containing target.toml or a .jinja template."
        );
    }
    let bundle = TargetBundle::load(target)?;
    let manifest = bundle.parse_manifest()?;
    Ok((bundle, manifest))
}

/// Render a code-gen `target` against a compiled model and return the rendered
/// files in memory (path + content) instead of writing them to disk.
///
/// This mirrors `compile_target`'s rendering (capability validation, per-file
/// path/template rendering, the same `--phase` semantics for raw `.jinja`
/// targets) so the structured output is byte-compatible with what the CLI would
/// write — only the destination differs. Packaging steps (e.g. FMU build) are
/// skipped: the caller gets the raw rendered sources.
///
/// Public (re-exported at the crate root) so template-target CI exercises the
/// exact render path the CLI uses — including capability validation and the
/// specialized renderers (Solve kernels and GALEC projection) that the
/// generic serialized-IR template context cannot
/// reach.
pub fn render_target_files(
    result: &CompilationResult,
    model: &str,
    target: &str,
    phase: Option<TemplateIr>,
) -> Result<Vec<RenderedTargetFile>> {
    render_target_files_with_overrides(result, model, target, phase, &[])
}

pub(crate) fn render_target_files_with_overrides(
    result: &CompilationResult,
    model: &str,
    target: &str,
    phase: Option<TemplateIr>,
    template_overrides: &[String],
) -> Result<Vec<RenderedTargetFile>> {
    if raw_template_target(target) {
        if !template_overrides.is_empty() {
            bail!(
                "--template overrides apply to a manifest target such as `--target galec`; \
                 a raw .jinja target is already the selected template"
            );
        }
        let rendered =
            render_raw_template(result, model, target, phase.unwrap_or(TemplateIr::Dae))?;
        let model_identifier = model.replace('.', "_");
        let file_name = Path::new(target)
            .file_stem()
            .and_then(|stem| stem.to_str())
            .map(str::to_string)
            .unwrap_or(model_identifier);
        return Ok(vec![RenderedTargetFile {
            path: file_name,
            content: rendered,
        }]);
    }
    let (bundle, manifest) = resolve_manifest_target(target, phase)?;
    let bundle = apply_template_overrides(bundle, &manifest, template_overrides)?;
    ensure_target_has_rendered_files(&manifest)?;
    validate_target_requirements(result, &manifest)?;

    let model_identifier = model.replace('.', "_");
    // `ir = "galec"` eFMU targets render their manifests +
    // `__content.xml` through the declarative checksum-web build step (contract
    // §9 WI-5). In memory that is `packaging::render_web_files` — the same
    // topological render + hash-inject the CLI writes, minus the on-disk
    // packaging — so CI exercises the exact web the container writer will.
    if let Some(plan) = build_galec_plan(result, &manifest, model, &model_identifier)? {
        let render = galec_manifest_render(
            &plan,
            &bundle,
            manifest.name.as_deref().unwrap_or("galec"),
            &model_identifier,
        );
        return crate::packaging::render_web_files(&manifest.files, render);
    }
    let renderer = resolve_manifest_renderer(result, &manifest, &model_identifier)?;
    render_manifest_files(result, &renderer, &bundle, &manifest, &model_identifier)
}

fn apply_template_overrides(
    bundle: TargetBundle,
    manifest: &TargetManifest,
    overrides: &[String],
) -> Result<TargetBundle> {
    if overrides.is_empty() {
        return Ok(bundle);
    }
    if manifest.ir != TargetTemplateIr::Galec {
        bail!("--template overrides currently require an `ir = \"galec\"` target");
    }

    let mut file_sources = std::collections::BTreeMap::new();
    let mut overridden_ids = std::collections::BTreeSet::new();
    for override_spec in overrides {
        let (id, path) = override_spec.split_once('=').ok_or_else(|| {
            anyhow::anyhow!(
                "invalid --template '{override_spec}'; expected FILE_ID=PATH, for example \
                 `--template alg=./my-algorithm.jinja`"
            )
        })?;
        if id.is_empty() || path.is_empty() {
            bail!("invalid --template '{override_spec}'; both FILE_ID and PATH must be non-empty");
        }
        if !overridden_ids.insert(id) {
            bail!("--template specifies file id '{id}' more than once");
        }
        if !manifest
            .files
            .iter()
            .any(|file| file.id.as_deref() == Some(id))
        {
            return Err({
                let available = manifest
                    .files
                    .iter()
                    .filter_map(|file| file.id.as_deref())
                    .collect::<Vec<_>>()
                    .join(", ");
                anyhow::anyhow!(
                    "--template names unknown file id '{id}'; available ids: {available}"
                )
            });
        }
        let source = std::fs::read_to_string(path)
            .with_context(|| format!("Read --template override for '{id}' from {path}"))?;
        file_sources.insert(id.to_owned(), source);
    }
    Ok(bundle.with_file_template_sources(file_sources))
}

/// Build the switch-dispatch eFMU packaging plan (contract §9 WI-5) for an
/// `ir = "galec"` eFMU target, or `None` for any other target. The
/// GALEC projection runs here — once, before any filesystem effect — so a
/// rejection surfaces before an output directory is created.
fn build_galec_plan(
    result: &CompilationResult,
    manifest: &TargetManifest,
    model: &str,
    model_identifier: &str,
) -> Result<Option<rumoca_compile::galec::GalecPackagingPlan>> {
    if manifest.ir != TargetTemplateIr::Galec || manifest.build != Some(TargetBuildKind::Efmu) {
        return Ok(None);
    }
    let target = manifest.name.as_deref().unwrap_or("custom");
    let plan = if galec_target_has_production_code(manifest) {
        rumoca_compile::galec::plan_galec_production_export(
            &result.dae,
            &result.flat,
            model_identifier,
            model,
        )
    } else {
        rumoca_compile::galec::plan_galec_export(&result.dae, &result.flat, model_identifier, model)
    };
    Ok(Some(plan.map_err(|error| {
        galec_plan_error(result, error, target)
    })?))
}

fn galec_target_has_production_code(manifest: &TargetManifest) -> bool {
    manifest.files.iter().any(|file| {
        file.path.starts_with("ProductionCode/")
            || matches!(
                file.render_context,
                Some(
                    TargetFileRenderContext::GalecC
                        | TargetFileRenderContext::EfmiProductionManifest
                )
            )
            || matches!(
                file.template.as_str(),
                "model.c.jinja" | "model.h.jinja" | "pc_manifest.xml.jinja"
            )
    })
}

fn galec_plan_error(
    result: &CompilationResult,
    error: GalecExportError,
    target: &str,
) -> anyhow::Error {
    match error {
        GalecExportError::Projection(diagnostics) => {
            if diagnostics
                .iter()
                .any(|diagnostic| diagnostic.span().is_some())
            {
                return CompilerError::SourceDiagnosticsError {
                    summary: format!("GALEC projection rejected target '{target}'"),
                    diagnostics: diagnostics
                        .iter()
                        .map(galec_projection_diagnostic)
                        .collect(),
                    source_map: Box::new(source_map(result)),
                }
                .into();
            }
            anyhow::Error::new(GalecExportError::Projection(diagnostics))
                .context(galec_plan_context(target))
        }
        other => anyhow::Error::new(other).context(galec_plan_context(target)),
    }
}

fn galec_projection_diagnostic(error: &GalecTargetError) -> CommonDiagnostic {
    let Some(span) = error.span() else {
        return CommonDiagnostic::global_error(error.code(), error.to_string());
    };
    CommonDiagnostic::error(
        error.code(),
        error.to_string(),
        PrimaryLabel::new(span).with_message(galec_projection_label(error)),
    )
}

fn galec_projection_label(error: &GalecTargetError) -> String {
    match error {
        GalecTargetError::UnsupportedFeature { feature, .. } => {
            format!("unsupported GALEC projection feature `{feature}`")
        }
        _ => "GALEC projection rejected this construct".to_owned(),
    }
}

fn galec_plan_context(target: &str) -> String {
    match target {
        "galec" => "GALEC eFMU plan for target 'galec'".to_owned(),
        "efmi" => "eFMI Production Code eFMU plan for target 'efmi'".to_owned(),
        _ => format!("GALEC target plan for target '{target}'"),
    }
}

fn source_map(result: &CompilationResult) -> SourceMap {
    result.resolved.0.source_map.clone()
}

/// The per-file render closure driving the declarative eFMU build step for a
/// GALEC packaging plan (contract §9 WI-5). It resolves each `[[files]]`
/// template from the bundle (or renders a `path` template inline), asks the
/// plan for the product-agnostic manifest context — into which the plan slots
/// the build-step-injected checksums (keyed by their `as` name) — and renders
/// under a strict-undefined env with the `xml_escape`/`xs_double` filters. The
/// Code templates read structured GALEC/C codegen context; manifest templates
/// read their validated product contexts under `ctx`.
fn galec_manifest_render<'a>(
    plan: &'a rumoca_compile::galec::GalecPackagingPlan,
    bundle: &'a TargetBundle,
    target_name: &'a str,
    model_identifier: &'a str,
) -> impl Fn(
    &str,
    &std::collections::BTreeMap<String, String>,
    Option<&rumoca_compile::codegen::targets::TargetFile>,
) -> Result<String>
+ 'a {
    let mut env = minijinja::Environment::new();
    env.set_undefined_behavior(minijinja::UndefinedBehavior::Strict);
    enable_parent_relative_template_paths(&mut env);
    register_manifest_filters(&mut env);
    let galec_bundle = TargetBundle::builtin(rumoca_compile::galec::GALEC_TARGET)
        .expect("builtin galec templates are embedded");
    env.add_template_owned(
        "galec/model.alg.jinja",
        galec_bundle
            .template_source("model.alg.jinja")
            .expect("builtin GALEC algorithm template exists")
            .into_owned(),
    )
    .expect("builtin GALEC algorithm template parses");
    let c_bundle = TargetBundle::builtin(rumoca_compile::galec::GALEC_C_TARGET)
        .expect("builtin galec-c templates are embedded");
    for template in ["model.h.jinja", "model.c.jinja"] {
        env.add_template_owned(
            format!("galec-c/{template}"),
            c_bundle
                .template_source(template)
                .expect("builtin GALEC C template exists")
                .into_owned(),
        )
        .expect("builtin GALEC C template parses");
    }
    move |template: &str,
          checksums: &std::collections::BTreeMap<String, String>,
          file: Option<&rumoca_compile::codegen::targets::TargetFile>| {
        let render_context = file.and_then(|file| file.render_context);
        let source = if let Some(file) = file {
            bundle.template_source_for_file(file)?
        } else if template.ends_with(".jinja") {
            bundle.template_source(template)?
        } else {
            std::borrow::Cow::Borrowed(template)
        };
        let ctx_value = plan
            .template_ctx(
                galec_context_template(render_context).unwrap_or(template),
                checksums,
            )
            .map_err(anyhow::Error::from)?;
        render_galec_template_source(
            &env,
            source.as_ref(),
            &format!("{target_name}/{template}"),
            render_context,
            model_identifier,
            &ctx_value,
            PRODUCTION_C_CONFORMANCE_HEADER,
        )
        .map_err(|error| anyhow::anyhow!("Render galec template '{template}': {error}"))
    }
}

fn galec_context_template(context: Option<TargetFileRenderContext>) -> Option<&'static str> {
    match context {
        Some(TargetFileRenderContext::GalecAlgorithm) => Some("model.alg.jinja"),
        Some(TargetFileRenderContext::GalecC) => Some("model.c.jinja"),
        Some(TargetFileRenderContext::EfmiAlgorithmManifest) => Some("ac_manifest.xml.jinja"),
        Some(TargetFileRenderContext::EfmiProductionManifest) => Some("pc_manifest.xml.jinja"),
        Some(TargetFileRenderContext::EfmiContent) => Some("__content.xml.jinja"),
        Some(
            TargetFileRenderContext::FmiModelDescription
            | TargetFileRenderContext::FmiImplementation,
        )
        | None => None,
    }
}

fn enable_parent_relative_template_paths(env: &mut minijinja::Environment<'_>) {
    env.set_path_join_callback(|name, parent| {
        let mut resolved = parent.split('/').collect::<Vec<_>>();
        resolved.pop();
        for segment in name.split('/') {
            match segment {
                "" | "." => {}
                ".." => {
                    resolved.pop();
                }
                segment => resolved.push(segment),
            }
        }
        resolved.join("/").into()
    });
}

/// Render one GALEC target template with its intended namespace contract.
///
/// The shared C templates predate the `ctx` namespace and intentionally read
/// their structured C IR at top level. Every other template, especially the
/// eFMI manifests, sees projection data only below `ctx`; strict undefined
/// handling therefore catches accidental `ac`/`pc`/`content` access instead
/// of silently accepting an un-namespaced field.
fn render_galec_template_source(
    env: &minijinja::Environment<'_>,
    source: &str,
    template: &str,
    render_context: Option<TargetFileRenderContext>,
    model_identifier: &str,
    ctx_value: &serde_json::Value,
    conformance_header: CConformanceHeader,
) -> Result<String, minijinja::Error> {
    let file_name = template.rsplit('/').next().unwrap_or(template);
    if render_context == Some(TargetFileRenderContext::GalecC)
        || matches!(file_name, "model.h.jinja" | "model.c.jinja")
    {
        return env.render_named_str(
            template,
            source,
            minijinja::context! {
                model_name => model_identifier,
                conformance_header => conformance_header.context_value(),
                ctx => minijinja::Value::from_serialize(ctx_value),
                ..minijinja::Value::from_serialize(ctx_value)
            },
        );
    }
    env.render_named_str(
        template,
        source,
        minijinja::context! {
            model_name => model_identifier,
            conformance_header => conformance_header.context_value(),
            ctx => minijinja::Value::from_serialize(ctx_value),
        },
    )
}

/// Render every `[[files]]` entry of a manifest target in memory from one
/// resolved renderer.
///
/// Shared by [`render_target_files`] and the `build = "efmu"` packaging path
/// of [`compile_manifest_target`], so the bytes the eFMU container packages
/// are exactly the bytes this invocation's single renderer produced (module
/// docs on [`ManifestRenderer`]: re-rendering from a second projection would
/// rest checksum validity on cross-run determinism).
fn render_manifest_files(
    result: &CompilationResult,
    renderer: &ManifestRenderer,
    bundle: &TargetBundle,
    manifest: &TargetManifest,
    model_identifier: &str,
) -> Result<Vec<RenderedTargetFile>> {
    let mut files = Vec::with_capacity(manifest.files.len());
    for file in &manifest.files {
        let path = renderer
            .render(result, &file.path, None, model_identifier)
            .with_context(|| format!("Render target output path '{}'", file.path))?;
        let template = bundle.template_source_for_file(file)?;
        let content = render_manifest_template(
            result,
            renderer,
            file.render_context,
            template.as_ref(),
            model_identifier,
        )
        .with_context(|| format!("Render target template '{}'", file.template))?;
        files.push(RenderedTargetFile {
            path: path.trim().to_string(),
            content,
        });
    }
    Ok(files)
}

fn raw_template_target(target: &str) -> bool {
    Path::new(target)
        .extension()
        .and_then(|extension| extension.to_str())
        .is_some_and(|extension| extension == "jinja")
}

/// Read and render a raw `.jinja` template against the chosen IR. Shared by the
/// file-writing and in-memory raw-template paths.
fn render_raw_template(
    result: &CompilationResult,
    model: &str,
    target: &str,
    ir: TemplateIr,
) -> Result<String> {
    let template =
        std::fs::read_to_string(target).with_context(|| format!("Read template: {target}"))?;
    let model_identifier = model.replace('.', "_");
    match raw_template_render_context(&template)? {
        Some(TargetFileRenderContext::FmiModelDescription) => result
            .render_fmi_model_description_template_str_with_name(&template, &model_identifier)
            .with_context(|| format!("Render raw FMI modelDescription template: {target}")),
        Some(TargetFileRenderContext::FmiImplementation) => result
            .render_fmi_implementation_template_str_with_name(&template, &model_identifier)
            .with_context(|| format!("Render raw FMI implementation template: {target}")),
        Some(context) => bail!(
            "raw template render context '{}' requires a manifest target directory",
            context.as_str()
        ),
        None => result
            .render_template_str_with_name_and_ir(&template, &model_identifier, ir)
            .with_context(|| format!("Render raw template: {target}")),
    }
}

fn raw_template_render_context(template: &str) -> Result<Option<TargetFileRenderContext>> {
    for line in template.lines().take(8) {
        let comment = line
            .trim()
            .strip_prefix("{#")
            .and_then(|line| line.strip_suffix("#}"))
            .map(str::trim)
            .map(|line| line.trim_start_matches('-').trim_end_matches('-').trim());
        let Some(comment) = comment else {
            continue;
        };
        let Some(value) = comment.strip_prefix("rumoca-render-context:") else {
            continue;
        };
        return match value.trim() {
            "fmi-model-description" => Ok(Some(TargetFileRenderContext::FmiModelDescription)),
            "fmi-implementation" => Ok(Some(TargetFileRenderContext::FmiImplementation)),
            other => bail!("unknown raw template render context '{other}'"),
        };
    }
    Ok(None)
}

#[cfg(feature = "scheduled-sim")]
fn compile_raw_template_target(
    result: &CompilationResult,
    model: &str,
    target: &str,
    output: Option<PathBuf>,
    ir: TemplateIr,
) -> Result<()> {
    let rendered = render_raw_template(result, model, target, ir)?;
    let Some(output_path) = output else {
        print!("{rendered}");
        return Ok(());
    };
    if let Some(parent) = output_path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
    {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(&output_path, rendered)?;
    eprintln!("Rendered raw template to: {}", output_path.display());
    Ok(())
}

fn template_ir_to_cli(value: TargetTemplateIr) -> TemplateIr {
    match value {
        TargetTemplateIr::Dae => TemplateIr::Dae,
        TargetTemplateIr::Solve => TemplateIr::Solve,
        TargetTemplateIr::Flat => TemplateIr::Flat,
        TargetTemplateIr::Ast => TemplateIr::Ast,
        TargetTemplateIr::Galec => {
            unreachable!("GALEC targets must resolve a GALEC renderer before generic IR rendering")
        }
    }
}

#[cfg(feature = "scheduled-sim")]
fn compile_manifest_target(
    result: &CompilationResult,
    model: &str,
    bundle: &TargetBundle,
    manifest: &TargetManifest,
    output: Option<PathBuf>,
) -> Result<()> {
    ensure_target_has_rendered_files(manifest)?;
    validate_target_requirements(result, manifest)?;

    let model_identifier = model.replace('.', "_");

    // `ir = "galec"` eFMU targets render their manifests +
    // `__content.xml` through the declarative checksum-web build step and
    // package the two eFMU forms (contract §9 WI-5) — a path distinct from the
    // generic `ManifestRenderer` targets below.
    if manifest.build == Some(TargetBuildKind::Efmu) {
        return compile_efmu_target(result, model, bundle, manifest, output, &model_identifier);
    }

    // Resolved before any filesystem effect: a renderer-level rejection
    // (e.g. the GALEC projection) must not leave an output directory behind.
    let renderer = resolve_manifest_renderer(result, manifest, &model_identifier)?;
    let out_dir = output.unwrap_or_else(|| default_target_output_dir(manifest, &model_identifier));

    eprintln!(
        "Compiling target '{}' for {}",
        bundle.label(manifest),
        model_identifier
    );
    if let Some(description) = &manifest.description {
        eprintln!("  {description}");
    }

    // The target.toml `build` field decides whether/how to package the
    // rendered output (FMU zip); the eFMU case is handled above. There is no
    // CLI flag.
    match manifest.build {
        Some(TargetBuildKind::Efmu) => unreachable!("efmu targets are dispatched above"),
        Some(TargetBuildKind::Fmu) => {
            write_manifest_files(
                result,
                &renderer,
                bundle,
                manifest,
                &out_dir,
                &model_identifier,
            )?;
            crate::fmu::build_fmu(&out_dir, &model_identifier, manifest.name.as_deref())?;
        }
        None => {
            write_manifest_files(
                result,
                &renderer,
                bundle,
                manifest,
                &out_dir,
                &model_identifier,
            )?;
            print_target_completion_message(manifest, &out_dir, &model_identifier)?;
        }
    }

    Ok(())
}

/// Compile an `ir = "galec"`, `build = "efmu"` target (contract §9 WI-5):
/// project once into a packaging plan, then drive the declarative
/// checksum-web build step to render every manifest + `__content.xml` and
/// package both eFMU forms (directory + `.efmu` zip).
///
/// The directory form lives in its own pristine `<out_dir>/<model>/` root
/// (eFMI ch. 2 defines the directory as a package format whose root must hold
/// exactly `__content.xml`, `schemas/`, and representation containers), and the
/// `.efmu` zip sits beside it — matching the `build = "fmu"` overwrite-on-re-run
/// UX. The plan (and its GALEC projection) is built before any filesystem
/// effect, so a rejected model leaves no directory behind.
#[cfg(feature = "scheduled-sim")]
fn compile_efmu_target(
    result: &CompilationResult,
    model: &str,
    bundle: &TargetBundle,
    manifest: &TargetManifest,
    output: Option<PathBuf>,
    model_identifier: &str,
) -> Result<()> {
    for file in &manifest.files {
        if file.mode.is_some() {
            bail!(
                "build = \"efmu\" targets do not support per-file `mode` (file '{}'): \
                 the container writer owns the on-disk layout",
                file.path
            );
        }
    }
    let plan = build_galec_plan(result, manifest, model, model_identifier)?.ok_or_else(|| {
        anyhow::anyhow!(
            "build = \"efmu\" target '{}' is not a known GALEC eFMU target",
            manifest.name.as_deref().unwrap_or("custom")
        )
    })?;
    let out_dir = output.unwrap_or_else(|| default_target_output_dir(manifest, model_identifier));

    eprintln!(
        "Compiling target '{}' for {}",
        bundle.label(manifest),
        model_identifier
    );
    if let Some(description) = &manifest.description {
        eprintln!("  {description}");
    }

    let container_root = out_dir.join(model_identifier);
    let package = crate::packaging::PackageSpec {
        index: EFMU_CONTENT_INDEX.to_string(),
        zip: Some(crate::packaging::ZipPackage {
            archive_path: out_dir.join(format!("{model_identifier}.efmu")),
        }),
    };
    let render = galec_manifest_render(
        &plan,
        bundle,
        manifest.name.as_deref().unwrap_or("galec"),
        model_identifier,
    );
    crate::packaging::render_and_package(
        &manifest.files,
        render,
        &manifest.assets,
        crate::packaging::efmi_asset_source,
        &package,
        &container_root,
    )?;
    print_target_completion_message(manifest, &out_dir, model_identifier)?;
    Ok(())
}

/// The eFMU root index file: a directory holding it is recognized as a prior
/// build of this product (contract §4b; eFMI ch. 2 package format 1).
#[cfg(feature = "scheduled-sim")]
const EFMU_CONTENT_INDEX: &str = "__content.xml";

/// Write every `[[files]]` entry of a manifest target under `out_dir` (the
/// non-packaged and FMU paths; the eFMU path packages the declarative build
/// step's renders instead).
#[cfg(feature = "scheduled-sim")]
fn write_manifest_files(
    result: &CompilationResult,
    renderer: &ManifestRenderer,
    bundle: &TargetBundle,
    manifest: &TargetManifest,
    out_dir: &Path,
    model_identifier: &str,
) -> Result<()> {
    std::fs::create_dir_all(out_dir)?;
    for file in &manifest.files {
        write_manifest_file(result, renderer, bundle, file, out_dir, model_identifier)?;
    }
    Ok(())
}

fn validate_target_requirements(
    result: &CompilationResult,
    manifest: &TargetManifest,
) -> Result<()> {
    let Some(capabilities) = &manifest.capabilities else {
        return Ok(());
    };
    let projection_dae = (manifest.ir == TargetTemplateIr::Galec)
        .then(|| rumoca_compile::galec::dae_for_galec_projection(&result.dae));
    let dae = projection_dae.as_ref().unwrap_or(&result.dae);
    validate_dae_target_capabilities(dae, manifest, capabilities)?;
    if manifest.ir == TargetTemplateIr::Solve {
        validate_solve_target_capabilities(result, manifest, capabilities)?;
    }
    Ok(())
}

fn validate_solve_target_capabilities(
    result: &CompilationResult,
    manifest: &TargetManifest,
    capabilities: &TargetCapabilities,
) -> Result<()> {
    let scalar_fallback = capabilities.scalar_fallback.unwrap_or(true);
    let tensor = capabilities.tensor.as_ref();
    let solve = rumoca_sim::lower_solve_problem(&result.dae)
        .context("Lower Solve IR for target capability validation")?;
    let inventory = solve.compute_node_counts();

    validate_solve_tensor_feature(
        manifest,
        "tensor.matmul",
        "MatMul",
        inventory.matmul,
        tensor.and_then(|tensor| tensor.matmul),
        scalar_fallback,
    )?;
    validate_solve_tensor_feature(
        manifest,
        "tensor.linsolve",
        "LinSolve",
        inventory.linsolve
            + if solve.uses_linear_solve_component() {
                1
            } else {
                0
            },
        tensor.and_then(|tensor| tensor.linsolve),
        scalar_fallback,
    )?;
    Ok(())
}

fn validate_solve_tensor_feature(
    manifest: &TargetManifest,
    feature: &str,
    display_name: &str,
    count: usize,
    capability: Option<TensorCapability>,
    scalar_fallback: bool,
) -> Result<()> {
    if count == 0 {
        return Ok(());
    }
    match capability {
        Some(TensorCapability::Native) => Ok(()),
        Some(TensorCapability::Scalar) if scalar_fallback => Ok(()),
        Some(TensorCapability::Scalar) => unsupported_tensor_feature(
            manifest,
            feature,
            format!(
                "{display_name} is configured for scalar fallback but scalar fallback is disabled"
            ),
        ),
        Some(TensorCapability::Unsupported) => unsupported_tensor_feature(
            manifest,
            feature,
            format!(
                "{display_name} nodes are present but the target declares {feature} unsupported"
            ),
        ),
        None if scalar_fallback => Ok(()),
        None => unsupported_tensor_feature(
            manifest,
            feature,
            format!(
                "{display_name} nodes are present but the target does not declare native {display_name} support and scalar fallback is disabled"
            ),
        ),
    }
}

fn unsupported_tensor_feature(
    manifest: &TargetManifest,
    feature: &str,
    detail: impl std::fmt::Display,
) -> Result<()> {
    bail!(
        "unsupported-feature:{feature}: Target '{}' does not support feature '{feature}': {detail}",
        manifest.name.as_deref().unwrap_or("custom")
    )
}

#[cfg(feature = "scheduled-sim")]
fn default_target_output_dir(manifest: &TargetManifest, model_identifier: &str) -> PathBuf {
    match manifest.build {
        Some(TargetBuildKind::Fmu) => PathBuf::from(format!("{model_identifier}.fmu")),
        // The eFMU out dir holds both package forms (`<model>/` directory
        // form + `<model>.efmu` zip; layout docs in `efmu.rs`), so it keeps
        // the plain model name rather than a package extension.
        Some(TargetBuildKind::Efmu) | None => PathBuf::from(model_identifier),
    }
}

#[cfg(feature = "scheduled-sim")]
fn write_manifest_file(
    result: &CompilationResult,
    renderer: &ManifestRenderer,
    bundle: &TargetBundle,
    file: &TargetFile,
    out_dir: &Path,
    model_identifier: &str,
) -> Result<()> {
    let rendered_rel_path = renderer
        .render(result, &file.path, None, model_identifier)
        .with_context(|| format!("Render target output path '{}'", file.path))?;
    let output_path = safe_target_join(out_dir, rendered_rel_path.trim())?;
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let template = bundle.template_source_for_file(file)?;
    let rendered = render_manifest_template(
        result,
        renderer,
        file.render_context,
        template.as_ref(),
        model_identifier,
    )
    .with_context(|| format!("Render target template '{}'", file.template))?;
    std::fs::write(&output_path, rendered)?;
    apply_manifest_file_mode(&output_path, file.mode.as_deref())?;
    eprintln!("  wrote {}", output_path.display());
    Ok(())
}

/// Per-invocation renderer for a manifest target's file templates.
///
/// Resolved exactly once per target invocation, before the per-file loop,
/// so every rendered artifact of one compile comes from the same underlying
/// computation. GALEC eFMU targets do NOT go through
/// this enum — they drive the declarative checksum-web build step
/// ([`compile_efmu_target`] / [`galec_manifest_render`], contract §9 WI-5).
enum ManifestRenderer {
    /// Generic path: the IR-keyed JSON template context.
    Ir(TemplateIr),
    /// `wgsl-solve` renders Solve kernels without the DAE JSON context.
    WgslSolve,
    /// A non-packaged `ir = "galec"` target renders each file from its
    /// explicitly declared GALEC context.
    Galec(rumoca_compile::galec::GalecPackagingPlan),
}

/// Resolve the renderer for one non-eFMU target invocation (module docs on
/// [`ManifestRenderer`]): specialized IR renderers first, the
/// generic IR-keyed context otherwise. The GALEC C projection runs here —
/// once — so a rejection surfaces before any file or directory is created.
/// (GALEC eFMU targets are dispatched separately via
/// [`build_galec_plan`], before this is reached.)
fn resolve_manifest_renderer(
    result: &CompilationResult,
    manifest: &TargetManifest,
    model_identifier: &str,
) -> Result<ManifestRenderer> {
    if manifest.ir == TargetTemplateIr::Solve && manifest.name.as_deref() == Some("wgsl-solve") {
        return Ok(ManifestRenderer::WgslSolve);
    }
    if manifest.ir == TargetTemplateIr::Galec {
        let target = manifest.name.as_deref().unwrap_or("custom");
        let plan = if galec_target_has_production_code(manifest) {
            rumoca_compile::galec::plan_galec_production_export(
                &result.dae,
                &result.flat,
                model_identifier,
                model_identifier,
            )
        } else {
            rumoca_compile::galec::plan_galec_export(
                &result.dae,
                &result.flat,
                model_identifier,
                model_identifier,
            )
        }
        .map_err(|error| galec_plan_error(result, error, target))?;
        return Ok(ManifestRenderer::Galec(plan));
    }
    Ok(ManifestRenderer::Ir(template_ir_to_cli(manifest.ir)))
}

fn render_manifest_template(
    result: &CompilationResult,
    renderer: &ManifestRenderer,
    context: Option<TargetFileRenderContext>,
    template: &str,
    model_identifier: &str,
) -> Result<String> {
    match context {
        Some(TargetFileRenderContext::FmiModelDescription) => result
            .render_fmi_model_description_template_str_with_name(template, model_identifier)
            .map_err(Into::into),
        Some(TargetFileRenderContext::FmiImplementation) => result
            .render_fmi_implementation_template_str_with_name(template, model_identifier)
            .map_err(Into::into),
        Some(
            TargetFileRenderContext::GalecAlgorithm
            | TargetFileRenderContext::GalecC
            | TargetFileRenderContext::EfmiAlgorithmManifest
            | TargetFileRenderContext::EfmiProductionManifest
            | TargetFileRenderContext::EfmiContent,
        )
        | None => renderer.render(result, template, context, model_identifier),
    }
}

impl ManifestRenderer {
    /// Render one template string (a `[[files]]` path or content template)
    /// against this invocation's resolved context.
    fn render(
        &self,
        result: &CompilationResult,
        template: &str,
        context: Option<TargetFileRenderContext>,
        model_identifier: &str,
    ) -> Result<String> {
        match self {
            Self::Ir(ir) => result
                .render_template_str_with_name_and_ir(template, model_identifier, *ir)
                .map_err(Into::into),
            Self::WgslSolve => result
                .render_solve_template_str_without_dae(template, model_identifier)
                .map_err(Into::into),
            Self::Galec(plan) => {
                let context_template = galec_context_template(context).unwrap_or(template);
                let ctx_value = plan
                    .template_ctx(context_template, &std::collections::BTreeMap::new())
                    .map_err(anyhow::Error::from)?;
                let mut env = minijinja::Environment::new();
                env.set_undefined_behavior(minijinja::UndefinedBehavior::Strict);
                render_galec_template_source(
                    &env,
                    template,
                    template,
                    context,
                    model_identifier,
                    &ctx_value,
                    EMBEDDED_C_GALEC_CONFORMANCE_HEADER,
                )
                .context("Render GALEC target template")
            }
        }
    }
}

/// Register the eFMI manifest render filters (contract §3b) on a bare
/// minijinja environment: `xml_escape` (autoescape is OFF, so every text
/// value is escaped explicitly) and `xs_double` (raw `f64` → valid
/// `xs:double` lexical). The real `galec`/`efmi` manifest
/// templates pipe every interpolated text value through `xml_escape` and
/// every raw `f64` through `xs_double`.
fn register_manifest_filters(env: &mut minijinja::Environment<'_>) {
    env.add_filter("xml_escape", |text: String| {
        rumoca_compile::galec::xml_escape(&text)
    });
    env.add_filter("xs_double", rumoca_compile::galec::xs_double);
}

/// Conformance/honesty header the shared GALEC C-layout templates
/// interpolate (SPEC_0034 GAL-024 two-track rule / D10).
///
/// `model.h.jinja`/`model.c.jinja` are shared by every target rendering
/// the GALEC C projection, so the claim text is target IDENTITY, not
/// projection data: each render path supplies its own value under the
/// strict-undefined `conformance_header` context key instead of the
/// templates baking one target's claim in. The eFMI Production Code target
/// (`efmi`) supplies its PC-representation claim the same way
/// at its render site inside the compile facade (`galec_api.rs`, where the
/// C files must render so their SHA-1s enter the Production Code manifest).
#[derive(Clone, Copy)]
struct CConformanceHeader {
    /// Full conformance statement for the header file, pre-wrapped: each
    /// entry becomes one ` * <line>` C comment line, so entries must stay
    /// comment-safe (no newlines, no `*/` — pinned by unit test).
    lines: &'static [&'static str],
    /// One-line short claim (also comment-safe) spliced into the source
    /// file's header comment ahead of the template-owned, target-agnostic
    /// layout-mechanics text.
    summary: &'static str,
}

impl CConformanceHeader {
    fn context_value(&self) -> minijinja::Value {
        minijinja::context! {
            lines => self.lines,
            summary => self.summary,
        }
    }
}

/// The `galec-c` claim: the non-eFMI track of GAL-024 must
/// self-describe as NOT an eFMI Production Code container (pinned by the
/// CLI honesty test `export_self_describes_as_not_an_efmi_production_code_container`).
/// The spelling is owned by `rumoca_compile::galec` — the single source shared
/// with the compile facade, the LSP, and the WASM addon.
const EMBEDDED_C_GALEC_CONFORMANCE_HEADER: CConformanceHeader = CConformanceHeader {
    lines: rumoca_compile::galec::EMBEDDED_C_GALEC_CONFORMANCE_LINES,
    summary: rumoca_compile::galec::EMBEDDED_C_GALEC_CONFORMANCE_SUMMARY,
};

const PRODUCTION_C_CONFORMANCE_HEADER: CConformanceHeader = CConformanceHeader {
    lines: rumoca_compile::galec::PRODUCTION_CONFORMANCE_LINES,
    summary: rumoca_compile::galec::PRODUCTION_CONFORMANCE_SUMMARY,
};

#[cfg(feature = "scheduled-sim")]
fn apply_manifest_file_mode(path: &Path, mode: Option<&str>) -> Result<()> {
    let Some(mode) = mode else {
        return Ok(());
    };

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;

        let mode = u32::from_str_radix(mode.trim_start_matches("0o"), 8)
            .with_context(|| format!("Parse file mode '{mode}' for {}", path.display()))?;
        std::fs::set_permissions(path, std::fs::Permissions::from_mode(mode))?;
    }
    #[cfg(not(unix))]
    {
        let _ = path;
        let _ = mode;
    }
    Ok(())
}

#[cfg(feature = "scheduled-sim")]
fn print_target_completion_message(
    manifest: &TargetManifest,
    out_dir: &Path,
    model_identifier: &str,
) -> Result<()> {
    if let Some(message) = &manifest.completion_message {
        let mut env = minijinja::Environment::new();
        env.set_undefined_behavior(minijinja::UndefinedBehavior::Strict);
        env.add_template("completion_message", message)
            .context("Parse target completion_message")?;
        let template = env
            .get_template("completion_message")
            .context("Load target completion_message")?;
        let rendered = template
            .render(minijinja::context! {
                out_dir => out_dir.display().to_string(),
                model_name => model_identifier,
                target_name => manifest.name.as_deref().unwrap_or("custom"),
            })
            .context("Render target completion_message")?;
        eprintln!("\n{rendered}");
    } else {
        eprintln!("\nTarget sources compiled to: {}", out_dir.display());
    }
    Ok(())
}

#[cfg(all(test, feature = "scheduled-sim"))]
mod tests {
    use super::*;
    use crate::Compiler;
    use std::io::Write;

    fn parse_manifest(source: &str) -> TargetManifest {
        rumoca_compile::codegen::targets::parse_target_manifest(source)
            .expect("target manifest should parse")
    }

    fn solve_manifest(capabilities: &str) -> TargetManifest {
        parse_manifest(&format!(
            r#"
version = 1
ir = "solve"
name = "test-solve-target"

{capabilities}

[[files]]
path = "model.out"
template = "model.out.jinja"
"#
        ))
    }

    fn compile_tensor_target_demo() -> CompilationResult {
        let source = r#"
model TensorTargetDemo
  Real omega[2](start={0, 0});
  parameter Real J[2,2] = [2, 0; 0, 4];
  parameter Real tau[2] = {8, 20};
equation
  J * der(omega) = tau;
end TensorTargetDemo;
"#;

        Compiler::new()
            .model("TensorTargetDemo")
            .compile_str(source, "TensorTargetDemo.mo")
            .expect("tensor target demo should compile")
    }

    fn compile_matmul_derivative_target_demo() -> CompilationResult {
        let source = r#"
model MatMulDerivativeTargetDemo
  Real x[2](start={1, 2});
  parameter Real A[2,2] = [1, 0; 0, 2];
equation
  der(x) = A * x;
end MatMulDerivativeTargetDemo;
"#;

        Compiler::new()
            .model("MatMulDerivativeTargetDemo")
            .compile_str(source, "MatMulDerivativeTargetDemo.mo")
            .expect("MatMul derivative target demo should compile")
    }

    fn compile_scalar_cuda_smoke_demo() -> CompilationResult {
        let source = r#"
model ScalarCudaSmoke
  Real x(start=1);
equation
  der(x) = -2 * x;
end ScalarCudaSmoke;
"#;

        Compiler::new()
            .model("ScalarCudaSmoke")
            .compile_str(source, "ScalarCudaSmoke.mo")
            .expect("scalar CUDA smoke demo should compile")
    }

    fn compile_galec_demo() -> CompilationResult {
        let source = r#"
model GalecTargetDemo
  constant Real samplePeriod = 0.1;
  parameter Real gain = 2.0;
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = gain * (pre(y) + 1.0);
  end when;
end GalecTargetDemo;
"#;
        Compiler::new()
            .model("GalecTargetDemo")
            .compile_str(source, "GalecTargetDemo.mo")
            .expect("GALEC target demo should compile")
    }

    fn command_available(command: &str) -> bool {
        Command::new(command).arg("--version").output().is_ok()
    }

    #[test]
    fn galec_manifest_context_is_available_only_below_ctx() {
        let mut env = minijinja::Environment::new();
        env.set_undefined_behavior(minijinja::UndefinedBehavior::Strict);
        let context = serde_json::json!({"ac": {"name": "Demo"}});

        let rendered = render_galec_template_source(
            &env,
            "{{ ctx.ac.name }}",
            "manifest.xml.jinja",
            None,
            "Demo",
            &context,
            PRODUCTION_C_CONFORMANCE_HEADER,
        )
        .expect("namespaced manifest field should render");
        assert_eq!(rendered, "Demo");

        let error = render_galec_template_source(
            &env,
            "{{ ac.name }}",
            "manifest.xml.jinja",
            None,
            "Demo",
            &context,
            PRODUCTION_C_CONFORMANCE_HEADER,
        )
        .expect_err("bare manifest field must stay undefined");
        assert_eq!(error.kind(), minijinja::ErrorKind::UndefinedError);
    }

    #[test]
    fn galec_templates_resolve_parent_relative_include_paths() {
        let mut env = minijinja::Environment::new();
        enable_parent_relative_template_paths(&mut env);
        env.add_template("galec/shared.jinja", "shared")
            .expect("shared template should register");

        let rendered = env
            .render_named_str(
                "efmi/model.alg.jinja",
                r#"{% include "../galec/shared.jinja" %}"#,
                (),
            )
            .expect("sibling target template should resolve relative include");
        assert_eq!(rendered, "shared");
    }

    #[test]
    fn galec_production_renders_all_three_relative_template_includes() {
        let source = r#"
model RelativeIncludeDemo
  constant Real samplePeriod = 0.1;
  parameter Real gain = 2.0;
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = gain * (pre(y) + 1.0);
  end when;
end RelativeIncludeDemo;
"#;
        let result = Compiler::new()
            .model("RelativeIncludeDemo")
            .compile_str(source, "RelativeIncludeDemo.mo")
            .expect("relative-include demo should compile");

        let files = render_target_files(&result, "RelativeIncludeDemo", "efmi", None)
            .expect("efmi should render sibling target templates");
        for path in [
            "AlgorithmCode/RelativeIncludeDemo.alg",
            "ProductionCode/RelativeIncludeDemo.h",
            "ProductionCode/RelativeIncludeDemo.c",
        ] {
            let rendered = files
                .iter()
                .find(|file| file.path == path)
                .unwrap_or_else(|| panic!("rendered target should contain {path}"));
            assert!(!rendered.content.is_empty(), "{path} should not be empty");
            assert!(
                !rendered.content.contains("{% include"),
                "{path} should contain the included template output"
            );
        }
    }

    #[test]
    fn galec_c_templates_retain_their_legacy_top_level_context() {
        let mut env = minijinja::Environment::new();
        env.set_undefined_behavior(minijinja::UndefinedBehavior::Strict);
        let context = serde_json::json!({"struct_name": "DemoState"});

        let rendered = render_galec_template_source(
            &env,
            "{{ struct_name }}",
            "model.h.jinja",
            Some(TargetFileRenderContext::GalecC),
            "Demo",
            &context,
            EMBEDDED_C_GALEC_CONFORMANCE_HEADER,
        )
        .expect("C template top-level field should render");
        assert_eq!(rendered, "DemoState");
    }

    #[test]
    fn explicit_galec_render_context_removes_filename_magic() {
        let result = compile_galec_demo();
        let manifest = parse_manifest(
            r#"
version = 1
ir = "galec"
name = "custom-template-name"
build = "efmu"

[[files]]
path = "AlgorithmCode/GalecTargetDemo.alg"
template = "anything-the-user-wants.jinja"
render_context = "galec-algorithm"
"#,
        );
        let plan = build_galec_plan(&result, &manifest, "GalecTargetDemo", "GalecTargetDemo")
            .expect("custom GALEC plan should build")
            .expect("GALEC eFMU should have a plan");
        let context_name = galec_context_template(manifest.files[0].render_context)
            .expect("explicit context maps to canonical GALEC context");
        let context = plan
            .template_ctx(context_name, &std::collections::BTreeMap::new())
            .expect("arbitrarily named template should receive algorithm context");
        assert!(context.get("name").is_some(), "{context}");
    }

    #[test]
    fn galec_template_override_replaces_only_the_selected_builtin_file() {
        let result = compile_galec_demo();
        let mut custom = tempfile::NamedTempFile::new().expect("create custom template");
        custom
            .write_all(b"custom={{ ctx.name.value }}")
            .expect("write custom template");
        let override_spec = format!("alg={}", custom.path().display());

        let files = render_target_files_with_overrides(
            &result,
            "GalecTargetDemo",
            "galec",
            None,
            &[override_spec],
        )
        .expect("custom Algorithm Code template should override the built-in");
        let algorithm = files
            .iter()
            .find(|file| file.path == "AlgorithmCode/GalecTargetDemo.alg")
            .expect("Algorithm Code output");
        assert_eq!(algorithm.content, "custom=GalecTargetDemo");
        assert!(
            files
                .iter()
                .find(|file| file.path == "AlgorithmCode/manifest.xml")
                .is_some_and(|file| file.content.contains("efmiAlgorithmCode")),
            "unspecified manifest template should remain built in"
        );
    }

    #[test]
    fn galec_template_override_lists_available_ids_on_typo() {
        let result = compile_galec_demo();
        let error = render_target_files_with_overrides(
            &result,
            "GalecTargetDemo",
            "galec",
            None,
            &["wrong=/tmp/unused.jinja".to_owned()],
        )
        .expect_err("unknown override id must fail before reading the path");
        let message = error.to_string();
        assert!(message.contains("unknown file id 'wrong'"), "{message}");
        assert!(message.contains("alg, ac_manifest, content"), "{message}");
    }

    #[test]
    fn non_packaged_galec_algorithm_target_uses_algorithm_context() {
        let result = compile_galec_demo();
        let target = tempfile::tempdir().expect("create custom target");
        std::fs::write(
            target.path().join("target.toml"),
            r#"
version = 1
ir = "galec"
name = "standalone-algorithm"

[[files]]
id = "alg"
path = "GalecTargetDemo.alg"
template = "custom.jinja"
render_context = "galec-algorithm"
"#,
        )
        .expect("write target manifest");
        std::fs::write(
            target.path().join("custom.jinja"),
            "algorithm={{ ctx.name.value }}",
        )
        .expect("write algorithm template");

        let files = render_target_files(
            &result,
            "GalecTargetDemo",
            target.path().to_str().expect("UTF-8 target path"),
            None,
        )
        .expect("non-packaged Algorithm Code target should render");
        assert_eq!(files[0].content, "algorithm=GalecTargetDemo");
    }

    #[test]
    fn reused_template_source_keeps_each_files_galec_context() {
        let result = compile_galec_demo();
        let target = tempfile::tempdir().expect("create custom target");
        std::fs::write(
            target.path().join("target.toml"),
            r#"
version = 1
ir = "galec"
name = "reused-template-contexts"
build = "efmu"

[[files]]
id = "alg"
path = "AlgorithmCode/GalecTargetDemo.alg"
template = "shared.jinja"
render_context = "galec-algorithm"

[[files]]
id = "c_source"
path = "ProductionCode/GalecTargetDemo.c"
template = "shared.jinja"
render_context = "galec-c"
"#,
        )
        .expect("write target manifest");
        std::fs::write(
            target.path().join("shared.jinja"),
            "{% if ctx.name is defined %}algorithm={{ ctx.name.value }}\
             {% else %}c={{ ctx.struct_name }}{% endif %}",
        )
        .expect("write shared template");

        let files = render_target_files(
            &result,
            "GalecTargetDemo",
            target.path().to_str().expect("UTF-8 target path"),
            None,
        )
        .expect("same template source should render once per declared context");
        assert_eq!(files[0].content, "algorithm=GalecTargetDemo");
        assert!(
            files[1].content.starts_with("c="),
            "second file should receive C context: {}",
            files[1].content
        );
    }

    #[test]
    fn template_override_applies_only_to_selected_file_id_when_source_is_reused() {
        let result = compile_galec_demo();
        let target = tempfile::tempdir().expect("create custom target");
        std::fs::write(
            target.path().join("target.toml"),
            r#"
version = 1
ir = "galec"
name = "reused-template-override"
build = "efmu"

[[files]]
id = "alg"
path = "AlgorithmCode/GalecTargetDemo.alg"
template = "shared.jinja"
render_context = "galec-algorithm"

[[files]]
id = "c_source"
path = "ProductionCode/GalecTargetDemo.c"
template = "shared.jinja"
render_context = "galec-c"
"#,
        )
        .expect("write target manifest");
        std::fs::write(
            target.path().join("shared.jinja"),
            "{% if ctx.name is defined %}builtin-algorithm\
             {% else %}builtin-c={{ ctx.struct_name }}{% endif %}",
        )
        .expect("write shared template");
        let mut custom = tempfile::NamedTempFile::new().expect("create override template");
        custom
            .write_all(b"custom-algorithm={{ ctx.name.value }}")
            .expect("write override template");
        let override_spec = format!("alg={}", custom.path().display());

        let files = render_target_files_with_overrides(
            &result,
            "GalecTargetDemo",
            target.path().to_str().expect("UTF-8 target path"),
            None,
            &[override_spec],
        )
        .expect("override should apply only to the selected file id");

        assert_eq!(files[0].content, "custom-algorithm=GalecTargetDemo");
        assert!(
            files[1].content.starts_with("builtin-c="),
            "the reused C template must retain its manifest source: {}",
            files[1].content
        );
    }

    #[test]
    fn galec_ir_dispatch_does_not_depend_on_builtin_target_names() {
        let result = compile_galec_demo();
        let ac_manifest = parse_manifest(
            r#"
version = 1
ir = "galec"
name = "my-readable-algorithm-code"
build = "efmu"

[[files]]
path = "AlgorithmCode/{{ model_name }}.alg"
template = "model.alg.jinja"
"#,
        );
        let ac_plan = build_galec_plan(&result, &ac_manifest, "GalecTargetDemo", "GalecTargetDemo")
            .expect("custom GALEC target should dispatch")
            .expect("custom GALEC eFMU target should build a plan");
        assert!(
            ac_plan
                .template_ctx("model.c.jinja", &std::collections::BTreeMap::new())
                .is_err(),
            "AC-only target must not acquire Production Code context"
        );

        let pc_manifest = parse_manifest(
            r#"
version = 1
ir = "galec"
name = "my-readable-production-code"
build = "efmu"

[[files]]
path = "ProductionCode/{{ model_name }}.c"
template = "model.c.jinja"
"#,
        );
        let pc_plan = build_galec_plan(&result, &pc_manifest, "GalecTargetDemo", "GalecTargetDemo")
            .expect("custom GALEC target should dispatch")
            .expect("custom GALEC eFMU target should build a plan");
        pc_plan
            .template_ctx("model.c.jinja", &std::collections::BTreeMap::new())
            .expect("ProductionCode file should select the production plan");
    }

    #[test]
    fn solve_target_capabilities_allow_scalar_tensor_fallback() {
        let result = compile_tensor_target_demo();
        let manifest = solve_manifest(
            r#"
[capabilities]
scalar_fallback = true

[capabilities.tensor]
linsolve = "scalar"
"#,
        );

        validate_target_requirements(&result, &manifest)
            .expect("scalar tensor fallback target should accept LinSolve Solve IR");
    }

    #[test]
    fn solve_target_capabilities_reject_missing_native_tensor_without_fallback() {
        let result = compile_tensor_target_demo();
        let manifest = solve_manifest(
            r#"
[capabilities]
scalar_fallback = false

[capabilities.tensor]
matmul = "native"
"#,
        );

        let err = validate_target_requirements(&result, &manifest)
            .expect_err("LinSolve without native support or scalar fallback should fail");
        let message = err.to_string();
        assert!(message.contains("tensor.linsolve"), "{message}");
        assert!(message.contains("scalar fallback is disabled"), "{message}");
    }

    #[test]
    fn rust_fixed_solve_builtin_target_accepts_scalarized_matmul() {
        let result = compile_matmul_derivative_target_demo();
        let bundle =
            TargetBundle::load("rust-fixed-solve").expect("load built-in rust-fixed-solve target");
        let manifest = bundle
            .parse_manifest()
            .expect("parse rust-fixed-solve manifest");
        let out_dir = tempfile::tempdir().expect("temp output dir");

        compile_manifest_target(
            &result,
            "MatMulDerivativeTargetDemo",
            &bundle,
            &manifest,
            Some(out_dir.path().to_path_buf()),
        )
        .expect("rust-fixed-solve should render scalarized MatMul derivative sources");

        let generated = std::fs::read_to_string(
            out_dir
                .path()
                .join("MatMulDerivativeTargetDemo_fixed_solve.rs"),
        )
        .expect("read generated fixed Rust source");
        assert!(generated.contains("pub type State = [Scalar; Y_LEN];"));
        assert!(generated.contains("pub fn derivative_rhs_into"));
        assert!(!generated.contains("Vec<"));
    }

    #[test]
    fn rust_fixed_solve_builtin_target_rejects_linsolve_before_writing_source() {
        let result = compile_tensor_target_demo();
        let bundle =
            TargetBundle::load("rust-fixed-solve").expect("load built-in rust-fixed-solve target");
        let manifest = bundle
            .parse_manifest()
            .expect("parse rust-fixed-solve manifest");
        let out_dir = tempfile::tempdir().expect("temp output dir");

        let err = compile_manifest_target(
            &result,
            "TensorTargetDemo",
            &bundle,
            &manifest,
            Some(out_dir.path().to_path_buf()),
        )
        .expect_err("rust-fixed-solve must reject LinSolve before writing source");
        let message = format!("{err:#}");
        assert!(
            message.contains("unsupported-feature:tensor.linsolve"),
            "{message}"
        );
        assert!(
            !out_dir
                .path()
                .join("TensorTargetDemo_fixed_solve.rs")
                .exists(),
            "target validation must fail before rendering an uncompilable source file"
        );
    }

    #[test]
    fn solve_target_capabilities_reject_tensor_ir_when_fallback_disabled_without_tensor_table() {
        let result = compile_tensor_target_demo();
        let manifest = solve_manifest(
            r#"
[capabilities]
scalar_fallback = false
"#,
        );

        let err = validate_target_requirements(&result, &manifest)
            .expect_err("tensor Solve IR should require native support or scalar fallback");
        let message = err.to_string();
        assert!(message.contains("tensor.linsolve"), "{message}");
        assert!(message.contains("scalar fallback is disabled"), "{message}");
    }

    #[test]
    fn cuda_c_builtin_target_generates_level_one_skeleton() {
        let result = compile_tensor_target_demo();
        let bundle = TargetBundle::load("cuda-c").expect("load built-in cuda-c target");
        let manifest = bundle.parse_manifest().expect("parse cuda-c manifest");
        let out_dir = tempfile::tempdir().expect("temp output dir");

        compile_manifest_target(
            &result,
            "TensorTargetDemo",
            &bundle,
            &manifest,
            Some(out_dir.path().to_path_buf()),
        )
        .expect("cuda-c target should render level-one sources");

        let generated = std::fs::read_to_string(out_dir.path().join("TensorTargetDemo_solve.cu"))
            .expect("read generated CUDA C source");
        assert!(generated.contains("TensorTargetDemo_derivative_rhs_batch"));
        assert!(generated.contains("Readiness level 1"));
        assert!(
            generated.contains("LinSolve"),
            "tensor inventory should be visible in generated source: {generated}"
        );
    }

    #[test]
    fn cuda_c_builtin_target_nvcc_smoke_for_scalar_model_when_available() {
        if !command_available("nvcc") {
            eprintln!("skipping cuda-c NVCC smoke: nvcc is not installed");
            return;
        }

        let result = compile_scalar_cuda_smoke_demo();
        let bundle = TargetBundle::load("cuda-c").expect("load built-in cuda-c target");
        let manifest = bundle.parse_manifest().expect("parse cuda-c manifest");
        let out_dir = tempfile::tempdir().expect("temp output dir");

        compile_manifest_target(
            &result,
            "ScalarCudaSmoke",
            &bundle,
            &manifest,
            Some(out_dir.path().to_path_buf()),
        )
        .expect("cuda-c target should render scalar smoke source");

        let source = out_dir.path().join("ScalarCudaSmoke_solve.cu");
        let object = out_dir.path().join("ScalarCudaSmoke_solve.o");
        let output = Command::new("nvcc")
            .arg("-c")
            .arg(&source)
            .arg("-o")
            .arg(&object)
            .output()
            .expect("run nvcc");
        assert!(
            output.status.success(),
            "nvcc failed for {}:\nstdout:\n{}\nstderr:\n{}",
            source.display(),
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    /// The shared GALEC C templates interpolate the conformance header
    /// inside C block comments (one ` * <line>` per `lines` entry, the
    /// `summary` spliced into an existing comment line): every entry must
    /// stay a single comment-safe line or the emitted sources would break
    /// under `cc -Wall -Werror`. The claim text itself is pinned end-to-end
    /// by the CLI honesty test
    /// (`export_self_describes_as_not_an_efmi_production_code_container`).
    #[test]
    fn embedded_c_galec_conformance_header_is_c_comment_safe() {
        let header = &EMBEDDED_C_GALEC_CONFORMANCE_HEADER;
        assert!(
            !header.lines.is_empty(),
            "the GAL-024 honesty statement must not be empty"
        );
        for text in header.lines.iter().chain(std::iter::once(&header.summary)) {
            assert!(
                !text.contains('\n') && !text.contains("*/"),
                "conformance-header text must be a single C comment line: {text:?}"
            );
        }
    }
}
