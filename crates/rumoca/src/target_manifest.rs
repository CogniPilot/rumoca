use std::path::Path;
#[cfg(any(feature = "scheduled-sim", feature = "fmu-packaging"))]
use std::path::PathBuf;
#[cfg(all(test, feature = "scheduled-sim"))]
use std::process::Command;

use crate::{CompilationResult, TemplateIr, error::CompilerError};
use anyhow::{Context, Result, bail};
#[cfg(feature = "scheduled-sim")]
use rumoca_compile::codegen::targets::TargetFile;
#[cfg(test)]
use rumoca_compile::codegen::targets::validate_solve_tensor_inventory;
use rumoca_compile::codegen::targets::{
    RenderedTargetFile, TargetBundle, TargetCapabilities, TargetManifest, TargetTemplateIr,
    TargetTemplateSource, ensure_target_has_rendered_files, validate_dae_target_capabilities,
    validate_solve_target_capabilities,
};
#[cfg(any(feature = "scheduled-sim", feature = "fmu-packaging"))]
use rumoca_compile::codegen::targets::{TargetArchiveFormat, TargetArchiveRoot, safe_target_join};
use rumoca_core::{PhaseError, SourceMap};
use rumoca_ir_galec::package::EmissionPolicy;
use rumoca_phase_galec::{GalecInput, GalecOptions, GalecTargetError};

struct TargetModelIdentity<'a> {
    semantic_name: &'a str,
    artifact_stem: String,
}

impl<'a> TargetModelIdentity<'a> {
    fn new(semantic_name: &'a str) -> Self {
        Self {
            semantic_name,
            artifact_stem: semantic_name.replace('.', "_"),
        }
    }
}

#[cfg(feature = "scheduled-sim")]
pub(crate) fn compile_target(
    result: &CompilationResult,
    model: &str,
    target: &str,
    output: Option<PathBuf>,
    phase: Option<TemplateIr>,
    emission_policy: EmissionPolicy,
) -> Result<()> {
    if raw_template_target(target) {
        let identity = TargetModelIdentity::new(model);
        // A raw .jinja receives the IR chosen by --phase (default DAE).
        return compile_raw_template_target(
            result,
            &identity,
            target,
            output,
            phase.unwrap_or(TemplateIr::Dae),
        );
    }
    let (bundle, manifest) = resolve_manifest_target(target, phase)?;
    compile_manifest_target(result, model, &bundle, &manifest, output, emission_policy)
}

/// Invalidate artifacts from a previous built-in target invocation before the
/// semantic compiler runs. Built-in output paths depend only on the selected
/// model name; external target paths may depend on arbitrary semantic context
/// and therefore remain owned by their caller until that target has rendered.
#[cfg(feature = "scheduled-sim")]
pub(crate) fn invalidate_target_output(
    model: &str,
    target: &str,
    output: Option<&Path>,
    phase: Option<TemplateIr>,
) -> Result<()> {
    if raw_template_target(target) {
        if let Some(path) = output {
            invalidate_output_file(path)?;
        }
        return Ok(());
    }

    let is_builtin = TargetBundle::builtin(target).is_some();
    let (_bundle, manifest) = resolve_manifest_target(target, phase)?;
    if !is_builtin {
        return Ok(());
    }

    let identity = TargetModelIdentity::new(model);
    let out_dir = output
        .map(Path::to_path_buf)
        .unwrap_or_else(|| default_target_output_dir(&manifest, &identity.artifact_stem));
    if let Some(package) = &manifest.package {
        let root = render_builtin_output_path(&package.root, &identity.artifact_stem)?;
        let root = safe_target_join(&out_dir, root.trim())?;
        let archive = package
            .archive
            .as_ref()
            .map(|archive| {
                render_builtin_output_path(&archive.path, &identity.artifact_stem)
                    .and_then(|path| safe_target_join(&out_dir, path.trim()))
            })
            .transpose()?;
        #[cfg(feature = "fmu-packaging")]
        crate::packaging::invalidate_existing_package(
            &root,
            archive.as_deref(),
            &package.required_files,
        )?;
        #[cfg(not(feature = "fmu-packaging"))]
        let _ = (root, archive);
        return Ok(());
    }

    for file in &manifest.files {
        let path = render_builtin_output_path(&file.path, &identity.artifact_stem)?;
        invalidate_output_file(&safe_target_join(&out_dir, path.trim())?)?;
    }
    Ok(())
}

#[cfg(feature = "scheduled-sim")]
fn render_builtin_output_path(template: &str, model_identifier: &str) -> Result<String> {
    let mut env = minijinja::Environment::new();
    env.set_undefined_behavior(minijinja::UndefinedBehavior::Strict);
    env.add_template("output_path", template)
        .context("Parse built-in target output path")?;
    env.get_template("output_path")?
        .render(minijinja::context! { model_name => model_identifier })
        .context("Render built-in target output path")
}

#[cfg(feature = "scheduled-sim")]
fn invalidate_output_file(path: &Path) -> Result<()> {
    if !path.exists() {
        return Ok(());
    }
    if path.is_dir() {
        bail!(
            "output path `{}` is a directory; refusing to remove it as a generated file",
            path.display()
        );
    }
    std::fs::remove_file(path)
        .with_context(|| format!("Invalidate previous output '{}'", path.display()))
}

/// Compile one manifest-declared package through the same checked renderer and
/// transactional package writer used by `rumoca compile --target`.
///
/// This entry point deliberately owns packaging only. It keeps standards gates
/// independent of simulation, transport, input-device, and viewer features.
#[cfg(feature = "fmu-packaging")]
pub fn compile_packaged_target(
    result: &CompilationResult,
    model: &str,
    target: &str,
    output: PathBuf,
) -> Result<()> {
    if raw_template_target(target) {
        bail!("raw template targets do not declare packages");
    }
    let (bundle, manifest) = resolve_manifest_target(target, None)?;
    if manifest.package.is_none() {
        bail!("target '{target}' does not declare a package");
    }
    ensure_target_has_rendered_files(&manifest)?;
    validate_target_requirements(result, &manifest)?;
    let identity = TargetModelIdentity::new(model);
    // Packaging CI drives the certifiable default; the dial is a `compile`
    // flag, and this entry point deliberately owns packaging only.
    let renderer =
        resolve_manifest_renderer(result, &manifest, &identity, EmissionPolicy::reviewable())?;
    compile_manifest_package(
        result,
        &renderer,
        &bundle,
        &manifest,
        &output,
        &identity.artifact_stem,
    )
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
/// name-dispatched renderers (`wgsl-ode`, `galec`, `embedded-c-galec`,
/// `galec-production`) that the generic DAE-JSON template context cannot
/// reach.
pub fn render_target_files(
    result: &CompilationResult,
    model: &str,
    target: &str,
    phase: Option<TemplateIr>,
) -> Result<Vec<RenderedTargetFile>> {
    let identity = TargetModelIdentity::new(model);
    if raw_template_target(target) {
        let rendered =
            render_raw_template(result, &identity, target, phase.unwrap_or(TemplateIr::Dae))?;
        let file_name = Path::new(target)
            .file_stem()
            .and_then(|stem| stem.to_str())
            .map(str::to_string)
            .unwrap_or(identity.artifact_stem);
        return Ok(vec![RenderedTargetFile {
            path: file_name,
            content: rendered,
        }]);
    }
    let (bundle, manifest) = resolve_manifest_target(target, phase)?;
    ensure_target_has_rendered_files(&manifest)?;
    validate_target_requirements(result, &manifest)?;

    // Algorithm Code package targets render their artifact graph through the
    // generic checksum-web build step. In memory that is
    // `packaging::render_web_files` — the same
    // topological render + hash-inject the CLI writes, minus the on-disk
    // packaging — so CI exercises the exact web the container writer will.
    if manifest.ir == TargetTemplateIr::AlgorithmCode {
        let package = lower_algorithm_code(
            result,
            identity.semantic_name,
            manifest.name.as_deref(),
            EmissionPolicy::reviewable(),
        )?;
        let renderer = rumoca_phase_codegen::AlgorithmCodeTemplateRenderer::new(
            &package,
            result.dae.source_map(),
        )?;
        let render = algorithm_code_web_render(&renderer, &bundle, &identity.artifact_stem);
        return crate::packaging::render_web_files(&manifest.files, render);
    }
    let renderer =
        resolve_manifest_renderer(result, &manifest, &identity, EmissionPolicy::reviewable())?;
    render_manifest_files(
        result,
        &renderer,
        &bundle,
        &manifest,
        &identity.artifact_stem,
    )
}

/// Build the switch-dispatch eFMU packaging plan (contract §9 WI-5) for the
/// `galec`/`galec-production` targets, or `None` for any other target. The
/// GALEC projection runs here — once, before any filesystem effect — so a
/// rejection surfaces before an output directory is created.
fn lower_algorithm_code(
    result: &CompilationResult,
    model: &str,
    target: Option<&str>,
    emission_policy: EmissionPolicy,
) -> Result<rumoca_ir_galec::package::AlgorithmCodePackage> {
    rumoca_phase_galec::lower_to_algorithm_code(
        &GalecInput::new(&result.dae, model),
        &GalecOptions {
            emission_policy,
            ..GalecOptions::default()
        },
    )
    .map_err(|diagnostics| galec_projection_error(result, diagnostics, target.unwrap_or("custom")))
}

fn galec_projection_error(
    result: &CompilationResult,
    diagnostics: Vec<GalecTargetError>,
    target: &str,
) -> anyhow::Error {
    if diagnostics
        .iter()
        .any(|diagnostic| diagnostic.span().is_some())
    {
        return CompilerError::SourceDiagnosticsError {
            summary: format!("GALEC projection rejected target '{target}'"),
            diagnostics: diagnostics.iter().map(PhaseError::to_diagnostic).collect(),
            source_map: Box::new(source_map(result)),
        }
        .into();
    }
    anyhow::anyhow!(
        "GALEC projection rejected target '{target}': {}",
        diagnostics
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join("; ")
    )
}

fn source_map(result: &CompilationResult) -> SourceMap {
    result.resolved.inner().source_map.clone()
}

/// The per-file render closure driving the declarative eFMU build step for a
/// GALEC packaging plan (contract §9 WI-5). It resolves each `[[files]]`
/// template from the bundle (or renders a `path` template inline), asks the
/// plan for the product-agnostic manifest context — into which the plan slots
/// the build-step-injected checksums (keyed by their `as` name) — and renders
/// under a strict-undefined env with the `xml_escape`/`xs_double` filters. The
/// `.alg` reads its established printer-owned text, C templates read the
/// typed semantic C context at top level, and manifest templates read only
/// their validated product context below `ctx`.
fn algorithm_code_web_render<'a>(
    renderer: &'a rumoca_phase_codegen::AlgorithmCodeTemplateRenderer,
    bundle: &'a TargetBundle,
    model_identifier: &'a str,
) -> impl Fn(&str, &crate::packaging::ArtifactRenderContext<'_>) -> Result<String> + 'a {
    move |template_or_path, artifact| {
        let source = bundle
            .template_source(template_or_path)
            .unwrap_or(std::borrow::Cow::Borrowed(template_or_path));
        renderer
            .render_with_name_and_artifact(source.as_ref(), model_identifier, artifact)
            .map_err(anyhow::Error::from)
    }
}

/// Render every `[[files]]` entry of a manifest target in memory from one
/// resolved renderer.
///
/// Shared by [`render_target_files`] and manifest-declared package builds, so
/// the bytes the package contains
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
            .render(result, &file.path, model_identifier)
            .with_context(|| format!("Render target output path '{}'", file.path))?;
        let template = bundle.template_source(&file.template)?;
        let content =
            render_manifest_template(result, renderer, template.as_ref(), model_identifier)
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
    identity: &TargetModelIdentity<'_>,
    target: &str,
    ir: TemplateIr,
) -> Result<String> {
    let template =
        std::fs::read_to_string(target).with_context(|| format!("Read template: {target}"))?;
    result
        .render_template_str_with_name_and_ir(&template, &identity.artifact_stem, ir)
        .with_context(|| format!("Render raw template: {target}"))
}

#[cfg(feature = "scheduled-sim")]
fn compile_raw_template_target(
    result: &CompilationResult,
    identity: &TargetModelIdentity<'_>,
    target: &str,
    output: Option<PathBuf>,
    ir: TemplateIr,
) -> Result<()> {
    let rendered = render_raw_template(result, identity, target, ir)?;
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
        TargetTemplateIr::Fmi => {
            unreachable!("checked FMI components use their dedicated renderer")
        }
        TargetTemplateIr::Flat => TemplateIr::Flat,
        TargetTemplateIr::Ast => TemplateIr::Ast,
        TargetTemplateIr::AlgorithmCode => {
            unreachable!("Algorithm Code targets use their checked phase-owned renderer")
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
    emission_policy: EmissionPolicy,
) -> Result<()> {
    ensure_target_has_rendered_files(manifest)?;
    validate_target_requirements(result, manifest)?;

    let identity = TargetModelIdentity::new(model);

    // Resolved before any filesystem effect: a renderer-level rejection
    // (e.g. the GALEC projection) must not leave an output directory behind.
    let renderer = resolve_manifest_renderer(result, manifest, &identity, emission_policy)?;
    let out_dir =
        output.unwrap_or_else(|| default_target_output_dir(manifest, &identity.artifact_stem));

    eprintln!(
        "Compiling target '{}' for {}",
        bundle.label(manifest),
        identity.artifact_stem
    );
    if let Some(description) = &manifest.description {
        eprintln!("  {description}");
    }

    if manifest.package.is_some() {
        #[cfg(feature = "fmu-packaging")]
        compile_manifest_package(
            result,
            &renderer,
            bundle,
            manifest,
            &out_dir,
            &identity.artifact_stem,
        )?;
        #[cfg(not(feature = "fmu-packaging"))]
        bail!(
            "target '{}' requires the fmu-packaging feature",
            bundle.label(manifest)
        );
    } else {
        write_manifest_files(
            result,
            &renderer,
            bundle,
            manifest,
            &out_dir,
            &identity.artifact_stem,
        )?;
    }
    print_target_completion_message(manifest, &out_dir, &identity.artifact_stem)?;
    Ok(())
}

#[cfg(feature = "fmu-packaging")]
fn compile_manifest_package(
    result: &CompilationResult,
    renderer: &ManifestRenderer,
    bundle: &TargetBundle,
    manifest: &TargetManifest,
    out_dir: &Path,
    model_identifier: &str,
) -> Result<()> {
    let declared = manifest
        .package
        .as_ref()
        .context("internal: packaged target has no [package] declaration")?;
    for file in &manifest.files {
        if file.mode.is_some() {
            bail!(
                "[package] targets do not support per-file `mode` (file '{}'): \
                 the package writer owns the on-disk layout",
                file.path
            );
        }
    }
    let package_root = renderer
        .render(result, &declared.root, model_identifier)
        .context("Render [package] root")?;
    let package_root = safe_target_join(out_dir, package_root.trim())?;
    let archive_path = declared
        .archive
        .as_ref()
        .map(|archive| {
            if archive.format != TargetArchiveFormat::Zip || archive.root != TargetArchiveRoot::Flat
            {
                bail!("Only flat zip archives are currently supported");
            }
            let rendered = renderer
                .render(result, &archive.path, model_identifier)
                .context("Render [package.archive] path")?;
            safe_target_join(out_dir, rendered.trim())
        })
        .transpose()?;
    let package = crate::packaging::PackageSpec {
        required_files: declared.required_files.clone(),
        zip: archive_path.map(|archive_path| crate::packaging::ZipPackage { archive_path }),
    };
    let render = |template_or_path: &str,
                  artifact: &crate::packaging::ArtifactRenderContext<'_>| {
        let source = bundle
            .template_source(template_or_path)
            .unwrap_or(std::borrow::Cow::Borrowed(template_or_path));
        renderer.render_with_artifact(result, source.as_ref(), model_identifier, artifact)
    };
    crate::packaging::render_and_package(
        &manifest.files,
        render,
        &manifest.assets,
        |asset| bundle.asset_files(asset),
        &package,
        &package_root,
    )?;
    Ok(())
}

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
    match manifest.ir {
        TargetTemplateIr::Dae | TargetTemplateIr::AlgorithmCode => {
            let capabilities = declared_capabilities(manifest)?;
            validate_dae_target_capabilities(&result.dae, manifest, capabilities)?;
        }
        TargetTemplateIr::Solve | TargetTemplateIr::Fmi => {
            let capabilities = declared_capabilities(manifest)?;
            // Solve remains a projection of checked DAE semantics. Inspect the
            // source artifact as well so a partial kernel cannot erase tables,
            // randomness, events, or another target capability obligation.
            validate_dae_target_capabilities(&result.dae, manifest, capabilities)?;
            let solve = rumoca_sim::lower_solve_problem(&result.dae)
                .context("Lower Solve IR for target capability validation")?;
            validate_solve_target_capabilities(&solve, manifest, capabilities)?;
        }
        // Flat and AST templates carry no capability obligation. The Flat
        // render context proves its own contract before any byte is produced
        // (`rumoca::codegen::EC007`, the non-materialized structured-family
        // refusal), and the AST is source structure with no lowering claim a
        // capability column could describe.
        TargetTemplateIr::Flat | TargetTemplateIr::Ast => {}
    }
    Ok(())
}

/// The `[capabilities]` table a DAE-derived target must declare before it may
/// render.
///
/// An absent table is a refusal, not a waiver. The entire DAE/Solve
/// admissibility proof runs through this table: the compact-family gate, the
/// residual-algebraic gate, the Phase-DAE temporal-operator invariant, and
/// `SolveProblem::validate` itself. Reading "undeclared" as "unconstrained"
/// therefore lets a target that states nothing render a model it cannot
/// express and publish the bytes as if they had been checked.
///
/// `parse_target_manifest` refuses an undeclared `dae`, `fmi`, or
/// `algorithm-code` manifest outright, and the DAE-only and LSP Solve render
/// paths demand the table of their own callers, so this is the guard at the
/// point of use rather than a second rule: it is what makes the refusal
/// structural here instead of inherited from whoever produced the manifest.
///
/// The fault is in the target manifest, not in the model, so this diagnostic
/// carries no source span: no span over the compiled Modelica points at the
/// thing that has to change.
fn declared_capabilities(manifest: &TargetManifest) -> Result<&TargetCapabilities> {
    manifest.capabilities.as_ref().ok_or_else(|| {
        anyhow::anyhow!(
            "unsupported-feature:target-capabilities-undeclared: target '{}' consumes {:?} IR \
             but declares no [capabilities] table, so nothing states which models it can \
             render; add a [capabilities] table to target.toml declaring every column this \
             target implements",
            manifest.name.as_deref().unwrap_or("custom"),
            manifest.ir,
        )
    })
}

#[cfg(feature = "scheduled-sim")]
fn default_target_output_dir(manifest: &TargetManifest, model_identifier: &str) -> PathBuf {
    let _ = manifest;
    PathBuf::from(model_identifier)
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
        .render(result, &file.path, model_identifier)
        .with_context(|| format!("Render target output path '{}'", file.path))?;
    let output_path = safe_target_join(out_dir, rendered_rel_path.trim())?;
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let template = bundle.template_source(&file.template)?;
    let rendered = render_manifest_template(result, renderer, template.as_ref(), model_identifier)
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
/// computation. Packaged and unpackaged targets share this renderer; package
/// assembly is a later generic artifact operation.
enum ManifestRenderer {
    /// Generic path: the IR-keyed JSON template context.
    Ir(TemplateIr),
    /// `wgsl-ode` renders Solve kernels without the DAE JSON context.
    WgslSolve,
    /// One checked tensor-native FMI component plus its lazy Solve program.
    #[cfg(feature = "fmi")]
    Fmi {
        renderer: rumoca_phase_codegen::SolveTemplateRenderer,
        artifact: crate::packaging::ArtifactSession,
    },
    /// A checked Algorithm Code package plus immutable artifact facts.
    AlgorithmCode {
        renderer: rumoca_phase_codegen::AlgorithmCodeTemplateRenderer,
        artifact: crate::packaging::ArtifactSession,
    },
}

/// Resolve the renderer for one non-eFMU target invocation (module docs on
/// [`ManifestRenderer`]): the name-dispatched special cases first, the
/// generic IR-keyed context otherwise. The GALEC C projection runs here —
/// once — so a rejection surfaces before any file or directory is created.
/// (The `galec`/`galec-production` eFMU targets are dispatched separately via
/// [`build_galec_plan`], before this is reached.)
fn resolve_manifest_renderer(
    result: &CompilationResult,
    manifest: &TargetManifest,
    identity: &TargetModelIdentity<'_>,
    emission_policy: EmissionPolicy,
) -> Result<ManifestRenderer> {
    if manifest.ir == TargetTemplateIr::Solve && manifest.name.as_deref() == Some("wgsl-ode") {
        return Ok(ManifestRenderer::WgslSolve);
    }
    if manifest.ir == TargetTemplateIr::AlgorithmCode {
        let package = lower_algorithm_code(
            result,
            identity.semantic_name,
            manifest.name.as_deref(),
            emission_policy,
        )?;
        let renderer = rumoca_phase_codegen::AlgorithmCodeTemplateRenderer::new(
            &package,
            result.dae.source_map(),
        )?;
        let artifact = crate::packaging::ArtifactSession::new(&manifest.files)?;
        return Ok(ManifestRenderer::AlgorithmCode { renderer, artifact });
    }
    if manifest.ir == TargetTemplateIr::Fmi {
        #[cfg(not(feature = "fmi"))]
        bail!("FMI targets require the `fmi` feature");
        #[cfg(feature = "fmi")]
        {
            let component = rumoca_sim::lower_fmi_component(&result.dae)
                .context("Construct checked FMI component")?;
            // The built-in FMI targets declare `events`, `runtime_events`, and
            // `clocks` false, so a component these templates cannot render in
            // full is already refused at the capability gate above. Narrowing
            // here is what lets the renderer take the proved view, and the
            // renderer needs nothing else: the view carries its own kernel.
            let event_free = component
                .into_codegen_view()
                .try_event_free()
                .context("Project checked FMI component for a storage-backed target")?;
            let renderer =
                rumoca_phase_codegen::SolveTemplateRenderer::new_owned_with_fmi(event_free)?;
            let artifact = crate::packaging::ArtifactSession::new(&manifest.files)?;
            return Ok(ManifestRenderer::Fmi { renderer, artifact });
        }
    }
    Ok(ManifestRenderer::Ir(template_ir_to_cli(manifest.ir)))
}

fn render_manifest_template(
    result: &CompilationResult,
    renderer: &ManifestRenderer,
    template: &str,
    model_identifier: &str,
) -> Result<String> {
    renderer.render(result, template, model_identifier)
}

impl ManifestRenderer {
    /// Render one template string (a `[[files]]` path or content template)
    /// against this invocation's resolved context.
    fn render(
        &self,
        result: &CompilationResult,
        template: &str,
        model_identifier: &str,
    ) -> Result<String> {
        let checksums = std::collections::BTreeMap::new();
        match self {
            Self::Ir(ir) => result
                .render_template_str_with_name_and_ir(template, model_identifier, *ir)
                .map_err(Into::into),
            Self::WgslSolve => result
                .render_solve_template_str_without_dae(template, model_identifier)
                .map_err(Into::into),
            #[cfg(feature = "fmi")]
            Self::Fmi { renderer, artifact } => renderer
                .render_with_name_and_artifact(template, model_identifier, artifact)
                .map_err(Into::into),
            Self::AlgorithmCode { renderer, artifact } => {
                let context = crate::packaging::ArtifactRenderContext {
                    session: artifact,
                    checksums: &checksums,
                };
                renderer
                    .render_with_name_and_artifact(template, model_identifier, &context)
                    .map_err(anyhow::Error::from)
            }
        }
    }

    /// Render one template string against a packaging artifact session.
    ///
    /// Only the declarative packaging path renders with an artifact context;
    /// the feature gate follows packaging ownership and does not pull in the
    /// scheduled simulator or any transport surface.
    #[cfg(feature = "fmu-packaging")]
    fn render_with_artifact(
        &self,
        result: &CompilationResult,
        template: &str,
        model_identifier: &str,
        artifact: &crate::packaging::ArtifactRenderContext<'_>,
    ) -> Result<String> {
        match self {
            Self::Ir(ir) => result
                .render_template_str_with_name_and_ir(template, model_identifier, *ir)
                .map_err(Into::into),
            Self::WgslSolve => result
                .render_solve_template_str_without_dae(template, model_identifier)
                .map_err(Into::into),
            #[cfg(feature = "fmi")]
            Self::Fmi { renderer, .. } => renderer
                .render_with_name_and_artifact(template, model_identifier, artifact)
                .map_err(Into::into),
            Self::AlgorithmCode { renderer, .. } => renderer
                .render_with_name_and_artifact(template, model_identifier, artifact)
                .map_err(anyhow::Error::from),
        }
    }
}

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

    fn compile_clocked_target_demo() -> CompilationResult {
        let source = r#"
model ClockedTargetDemo
  discrete Real y(start=0, fixed=true);
equation
  when sample(0, 0.1) then
    y = pre(y) + 1;
  end when;
end ClockedTargetDemo;
"#;

        Compiler::new()
            .model("ClockedTargetDemo")
            .compile_str(source, "ClockedTargetDemo.mo")
            .expect("clocked target demo should compile")
    }

    /// A model whose delay history the generated FMI C does not implement.
    ///
    /// Both built-in FMI targets declare `runtime_events = false`, so this is
    /// the shape their capability gate exists to stop (SPEC_0044 §8, dated
    /// disposition 2026-08-18).
    fn compile_delayed_target_demo() -> CompilationResult {
        let source = r#"
model FmiDelayedDecay
  Real x(start = 1.0);
equation
  der(x) = -delay(x, 0.5);
end FmiDelayedDecay;
"#;

        Compiler::new()
            .model("FmiDelayedDecay")
            .compile_str(source, "FmiDelayedDecay.mo")
            .expect("delayed target demo should compile")
    }

    /// A model whose state event the generated FMI C does not implement.
    ///
    /// Both built-in FMI targets declare `events = false`. This is the second
    /// event class the same gate stops, and the checked component of such a
    /// model has no event-free type-state either.
    fn compile_switched_target_demo() -> CompilationResult {
        let source = r#"
model FmiSwitchedDecay
  Real x(start = 1.0);
equation
  der(x) = if x > 0.5 then -1.0 else 1.0;
end FmiSwitchedDecay;
"#;

        Compiler::new()
            .model("FmiSwitchedDecay")
            .compile_str(source, "FmiSwitchedDecay.mo")
            .expect("switched target demo should compile")
    }

    fn compile_undelayed_target_demo() -> CompilationResult {
        let source = r#"
model FmiUndelayedDecay
  output Real x(start = 1.0);
equation
  der(x) = -x;
end FmiUndelayedDecay;
"#;

        Compiler::new()
            .model("FmiUndelayedDecay")
            .compile_str(source, "FmiUndelayedDecay.mo")
            .expect("undelayed target demo should compile")
    }

    fn command_available(command: &str) -> bool {
        Command::new(command).arg("--version").output().is_ok()
    }

    /// The in-memory render entry point rejects a delayed model at the
    /// capability gate, before a renderer for it exists.
    ///
    /// The gate's typed feature id is what proves the ordering: the checked FMI
    /// component of such a model has no event-free type-state either, so if the
    /// renderer had been resolved first the failure would carry that narrowing
    /// rejection instead.
    #[test]
    fn fmi_targets_reject_a_delayed_model_before_constructing_a_renderer() {
        assert_gate_refuses_first(
            &compile_delayed_target_demo(),
            "FmiDelayedDecay",
            "unsupported-feature:runtime_events",
        );
    }

    /// The same ordering for the other event class the built-in targets
    /// declare false, so the gate is not preserved for `runtime_events` alone.
    #[test]
    fn fmi_targets_reject_a_state_event_model_before_constructing_a_renderer() {
        assert_gate_refuses_first(
            &compile_switched_target_demo(),
            "FmiSwitchedDecay",
            "unsupported-feature:events",
        );
    }

    /// Both FMI targets must refuse `model` with exactly `feature`, and must do
    /// it at the manifest capability gate rather than at the checked
    /// component's narrowing, whose refusals all name a semantic event class.
    fn assert_gate_refuses_first(result: &CompilationResult, model: &str, feature: &str) {
        for target in ["fmi2", "fmi3"] {
            let error = render_target_files(result, model, target, None)
                .expect_err("an FMI target must reject a model outside its capabilities");
            let message = format!("{error:#}");
            assert!(message.contains(feature), "{target}: {message}");
            assert!(
                !message.contains("semantic events"),
                "{target} must fail the capability gate before the component is narrowed: {message}"
            );
        }
    }

    /// The same rejection on the packaging entry point, with nothing written.
    #[cfg(feature = "fmu-packaging")]
    #[test]
    fn fmi_packaging_rejects_a_delayed_model_before_writing_any_output() {
        let result = compile_delayed_target_demo();

        for target in ["fmi2", "fmi3"] {
            let out_dir = tempfile::tempdir().expect("temp output dir");
            let error = compile_packaged_target(
                &result,
                "FmiDelayedDecay",
                target,
                out_dir.path().to_path_buf(),
            )
            .expect_err("an FMI package must reject a delay-bearing model");
            let message = format!("{error:#}");
            assert!(
                message.contains("unsupported-feature:runtime_events"),
                "{target}: {message}"
            );
            assert_eq!(
                std::fs::read_dir(out_dir.path())
                    .expect("read temp output dir")
                    .count(),
                0,
                "{target} must reject a delay-bearing model before writing any artifact"
            );
        }
    }

    /// The positive control for both cases above: an event-free model still
    /// renders both descriptions through the same public entry point.
    #[cfg(feature = "fmi")]
    #[test]
    fn fmi_targets_render_an_event_free_model_through_the_same_entry_point() {
        let result = compile_undelayed_target_demo();

        for target in ["fmi2", "fmi3"] {
            let files = render_target_files(&result, "FmiUndelayedDecay", target, None)
                .map_err(|error| format!("{target}: {error:#}"))
                .expect("an FMI target renders an event-free model");
            let description = files
                .iter()
                .find(|file| file.path == "modelDescription.xml")
                .ok_or(target)
                .expect("an FMI target renders a model description");
            assert!(
                description.content.contains("name=\"x\"")
                    || description.content.contains("name=\"x[1]\""),
                "{target}: {}",
                description.content
            );
            assert!(
                !description
                    .content
                    .contains(rumoca_ir_solve::fmi::MAX_STEP_DURATION_NAME),
                "{target} must not name a local an event-free component never publishes: {}",
                description.content
            );
        }
    }

    #[test]
    fn target_model_identity_keeps_semantics_separate_from_artifact_paths() {
        let identity = TargetModelIdentity::new("Package.Controller");

        assert_eq!(identity.semantic_name, "Package.Controller");
        assert_eq!(identity.artifact_stem, "Package_Controller");
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
    fn solve_target_capabilities_reject_clocked_model_before_rendering() {
        let result = compile_clocked_target_demo();
        let manifest = solve_manifest(
            r#"
[capabilities]
events = false
runtime_events = false
clocks = false
"#,
        );

        let error = validate_target_requirements(&result, &manifest)
            .expect_err("an event-free Solve target must reject a clocked model");
        assert!(
            error.to_string().contains("unsupported-feature:events"),
            "{error}"
        );
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
    fn solve_target_capabilities_fail_closed_for_map_and_affine_stencil() {
        let manifest = solve_manifest(
            r#"
[capabilities]
scalar_fallback = false
"#,
        );
        let capabilities = manifest.capabilities.as_ref().expect("capabilities");
        let cases = [
            (
                rumoca_ir_solve::ComputeNodeCounts {
                    map: 1,
                    ..Default::default()
                },
                "tensor.elementwise",
            ),
            (
                rumoca_ir_solve::ComputeNodeCounts {
                    affine_stencil: 1,
                    ..Default::default()
                },
                "tensor.stencil",
            ),
        ];

        for (inventory, expected_feature) in cases {
            let error = validate_solve_tensor_inventory(&manifest, capabilities, inventory, false)
                .expect_err("undeclared native tensor node must be rejected");
            let message = error.to_string();
            assert!(
                message.contains(&format!("unsupported-feature:{expected_feature}")),
                "{message}"
            );
            assert!(message.contains("scalar fallback is disabled"), "{message}");
        }
    }

    #[test]
    fn solve_target_capabilities_accept_declared_native_map_and_affine_stencil() {
        let manifest = solve_manifest(
            r#"
[capabilities]
scalar_fallback = false

[capabilities.tensor]
elementwise = "native"
stencil = "native"
"#,
        );
        let capabilities = manifest.capabilities.as_ref().expect("capabilities");
        let inventory = rumoca_ir_solve::ComputeNodeCounts {
            map: 1,
            affine_stencil: 1,
            ..Default::default()
        };

        validate_solve_tensor_inventory(&manifest, capabilities, inventory, false)
            .expect("declared native Map and AffineStencil support should be accepted");
    }

    #[test]
    fn rust_fixed_ode_builtin_target_accepts_scalarized_matmul() {
        let result = compile_matmul_derivative_target_demo();
        let bundle =
            TargetBundle::load("rust-fixed-ode").expect("load built-in rust-fixed-ode target");
        let manifest = bundle
            .parse_manifest()
            .expect("parse rust-fixed-ode manifest");
        let out_dir = tempfile::tempdir().expect("temp output dir");

        compile_manifest_target(
            &result,
            "MatMulDerivativeTargetDemo",
            &bundle,
            &manifest,
            Some(out_dir.path().to_path_buf()),
            EmissionPolicy::reviewable(),
        )
        .expect("rust-fixed-ode should render scalarized MatMul derivative sources");

        let generated = std::fs::read_to_string(
            out_dir
                .path()
                .join("MatMulDerivativeTargetDemo_fixed_ode.rs"),
        )
        .expect("read generated fixed Rust source");
        assert!(generated.contains("pub type State = [Scalar; Y_LEN];"));
        assert!(generated.contains("pub fn derivative_rhs_into"));
        assert!(!generated.contains("Vec<"));
    }

    #[test]
    fn rust_fixed_ode_builtin_target_rejects_linsolve_before_writing_source() {
        let result = compile_tensor_target_demo();
        let bundle =
            TargetBundle::load("rust-fixed-ode").expect("load built-in rust-fixed-ode target");
        let manifest = bundle
            .parse_manifest()
            .expect("parse rust-fixed-ode manifest");
        let out_dir = tempfile::tempdir().expect("temp output dir");

        let err = compile_manifest_target(
            &result,
            "TensorTargetDemo",
            &bundle,
            &manifest,
            Some(out_dir.path().to_path_buf()),
            EmissionPolicy::reviewable(),
        )
        .expect_err("rust-fixed-ode must reject LinSolve before writing source");
        let message = format!("{err:#}");
        assert!(
            message.contains("unsupported-feature:tensor.linsolve"),
            "{message}"
        );
        assert!(
            !out_dir
                .path()
                .join("TensorTargetDemo_fixed_ode.rs")
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
    fn cuda_ode_builtin_target_generates_level_one_scalar_matmul_skeleton() {
        let result = compile_matmul_derivative_target_demo();
        let bundle = TargetBundle::load("cuda-ode").expect("load built-in cuda-ode target");
        let manifest = bundle.parse_manifest().expect("parse cuda-ode manifest");
        let out_dir = tempfile::tempdir().expect("temp output dir");

        compile_manifest_target(
            &result,
            "MatMulDerivativeTargetDemo",
            &bundle,
            &manifest,
            Some(out_dir.path().to_path_buf()),
            EmissionPolicy::reviewable(),
        )
        .expect("cuda-ode target should render its declared scalar MatMul fallback");

        let generated =
            std::fs::read_to_string(out_dir.path().join("MatMulDerivativeTargetDemo_ode.cu"))
                .expect("read generated CUDA C source");
        assert!(generated.contains("MatMulDerivativeTargetDemo_derivative_rhs_batch"));
        assert!(generated.contains("Readiness level 1"));
        assert!(
            generated.contains("MatMul"),
            "tensor inventory should be visible in generated source: {generated}"
        );
    }

    #[test]
    fn cuda_ode_builtin_target_rejects_linsolve_before_writing_source() {
        let result = compile_tensor_target_demo();
        let bundle = TargetBundle::load("cuda-ode").expect("load built-in cuda-ode target");
        let manifest = bundle.parse_manifest().expect("parse cuda-ode manifest");
        let out_dir = tempfile::tempdir().expect("temp output dir");
        let source = out_dir.path().join("TensorTargetDemo_ode.cu");

        let error = compile_manifest_target(
            &result,
            "TensorTargetDemo",
            &bundle,
            &manifest,
            Some(out_dir.path().to_path_buf()),
            EmissionPolicy::reviewable(),
        )
        .expect_err("cuda-ode must reject LinSolve without a device linear-solve ABI");
        let message = format!("{error:#}");
        assert!(
            message.contains("unsupported-feature:tensor.linsolve"),
            "{message}"
        );
        assert!(
            message.contains("target declares tensor.linsolve unsupported"),
            "{message}"
        );
        assert!(
            !source.exists(),
            "capability validation must reject LinSolve before writing {}",
            source.display()
        );
    }

    #[test]
    fn cuda_ode_builtin_target_nvcc_smoke_for_scalar_model_when_available() {
        if !command_available("nvcc") {
            eprintln!("skipping cuda-ode NVCC smoke: nvcc is not installed");
            return;
        }

        let result = compile_scalar_cuda_smoke_demo();
        let bundle = TargetBundle::load("cuda-ode").expect("load built-in cuda-ode target");
        let manifest = bundle.parse_manifest().expect("parse cuda-ode manifest");
        let out_dir = tempfile::tempdir().expect("temp output dir");

        compile_manifest_target(
            &result,
            "ScalarCudaSmoke",
            &bundle,
            &manifest,
            Some(out_dir.path().to_path_buf()),
            EmissionPolicy::reviewable(),
        )
        .expect("cuda-ode target should render scalar smoke source");

        let source = out_dir.path().join("ScalarCudaSmoke_ode.cu");
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
        let source = rumoca_phase_codegen::templates::builtin_template_source(
            "embedded-c-galec",
            "model.h.jinja",
        )
        .expect("embedded C header template");
        assert!(source.contains("GALEC-derived embedded C export"));
        assert!(source.contains("Target syntax and layout are owned by this template"));
    }
}
