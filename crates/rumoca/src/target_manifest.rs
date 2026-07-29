use std::path::Path;
#[cfg(feature = "scheduled-sim")]
use std::path::PathBuf;
#[cfg(all(test, feature = "scheduled-sim"))]
use std::process::Command;

use crate::{CompilationResult, TemplateIr, error::CompilerError};
use anyhow::{Context, Result, bail};
#[cfg(test)]
use rumoca_compile::codegen::targets::validate_solve_tensor_inventory;
use rumoca_compile::codegen::targets::{
    RenderedTargetFile, TargetBundle, TargetManifest, TargetTemplateIr, TargetTemplateSource,
    ensure_target_has_rendered_files, validate_dae_target_capabilities,
    validate_solve_target_capabilities,
};
#[cfg(feature = "scheduled-sim")]
use rumoca_compile::codegen::targets::{
    TargetArchiveFormat, TargetArchiveRoot, TargetFile, safe_target_join,
};
use rumoca_compile::compile::core::{Diagnostic as CommonDiagnostic, PrimaryLabel, SourceMap};
use rumoca_phase_galec::{GalecInput, GalecOptions, GalecTargetError};

#[cfg(feature = "scheduled-sim")]
pub(crate) fn compile_target(
    result: &CompilationResult,
    model: &str,
    target: &str,
    output: Option<PathBuf>,
    phase: Option<TemplateIr>,
) -> Result<()> {
    if raw_template_target(target) {
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
/// name-dispatched renderers (`wgsl-solve`, `galec`, `embedded-c-galec`,
/// `galec-production`) that the generic DAE-JSON template context cannot
/// reach.
pub fn render_target_files(
    result: &CompilationResult,
    model: &str,
    target: &str,
    phase: Option<TemplateIr>,
) -> Result<Vec<RenderedTargetFile>> {
    if raw_template_target(target) {
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
    ensure_target_has_rendered_files(&manifest)?;
    validate_target_requirements(result, &manifest)?;

    let model_identifier = model.replace('.', "_");
    // Algorithm Code package targets render their artifact graph through the
    // generic checksum-web build step. In memory that is
    // `packaging::render_web_files` — the same
    // topological render + hash-inject the CLI writes, minus the on-disk
    // packaging — so CI exercises the exact web the container writer will.
    if manifest.ir == TargetTemplateIr::AlgorithmCode {
        let package = lower_algorithm_code(result, model, manifest.name.as_deref())?;
        let render = algorithm_code_web_render(&package, &bundle, &model_identifier);
        return crate::packaging::render_web_files(&manifest.files, render);
    }
    let renderer = resolve_manifest_renderer(result, &manifest, &model_identifier)?;
    render_manifest_files(result, &renderer, &bundle, &manifest, &model_identifier)
}

/// Build the switch-dispatch eFMU packaging plan (contract §9 WI-5) for the
/// `galec`/`galec-production` targets, or `None` for any other target. The
/// GALEC projection runs here — once, before any filesystem effect — so a
/// rejection surfaces before an output directory is created.
fn lower_algorithm_code(
    result: &CompilationResult,
    model: &str,
    target: Option<&str>,
) -> Result<rumoca_ir_galec::package::AlgorithmCodePackage> {
    rumoca_phase_galec::lower_to_algorithm_code(
        &GalecInput::new(&result.dae, model),
        &GalecOptions::default(),
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
            diagnostics: diagnostics
                .iter()
                .map(galec_projection_diagnostic)
                .collect(),
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
    package: &'a rumoca_ir_galec::package::AlgorithmCodePackage,
    bundle: &'a TargetBundle,
    model_identifier: &'a str,
) -> impl Fn(&str, &crate::packaging::ArtifactRenderContext<'_>) -> Result<String> + 'a {
    move |template_or_path, artifact| {
        let source = bundle
            .template_source(template_or_path)
            .unwrap_or_else(|_| std::borrow::Cow::Borrowed(template_or_path));
        rumoca_phase_codegen::render_algorithm_code_template_with_artifact(
            package,
            artifact,
            source.as_ref(),
            model_identifier,
        )
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
    model: &str,
    target: &str,
    ir: TemplateIr,
) -> Result<String> {
    let template =
        std::fs::read_to_string(target).with_context(|| format!("Read template: {target}"))?;
    let model_identifier = model.replace('.', "_");
    result
        .render_template_str_with_name_and_ir(&template, &model_identifier, ir)
        .with_context(|| format!("Render raw template: {target}"))
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
) -> Result<()> {
    ensure_target_has_rendered_files(manifest)?;
    validate_target_requirements(result, manifest)?;

    let model_identifier = model.replace('.', "_");

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

    if manifest.package.is_some() {
        compile_packaged_target(
            result,
            &renderer,
            bundle,
            manifest,
            &out_dir,
            &model_identifier,
        )?;
    } else {
        write_manifest_files(
            result,
            &renderer,
            bundle,
            manifest,
            &out_dir,
            &model_identifier,
        )?;
    }
    print_target_completion_message(manifest, &out_dir, &model_identifier)?;
    Ok(())
}

#[cfg(feature = "scheduled-sim")]
fn compile_packaged_target(
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
            if archive.format != TargetArchiveFormat::Zip
                || archive.root != TargetArchiveRoot::Flat
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
            .unwrap_or_else(|_| std::borrow::Cow::Borrowed(template_or_path));
        renderer.render_with_artifact(result, source.as_ref(), model_identifier, artifact)
    };
    crate::packaging::render_and_package(
        &manifest.files,
        render,
        &manifest.assets,
        |source| bundle.asset_files(source),
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
    let Some(capabilities) = &manifest.capabilities else {
        return Ok(());
    };
    match manifest.ir {
        TargetTemplateIr::Dae | TargetTemplateIr::AlgorithmCode => {
            validate_dae_target_capabilities(&result.dae, manifest, capabilities)?;
        }
        TargetTemplateIr::Solve => {
            let solve = rumoca_sim::lower_solve_problem(&result.dae)
                .context("Lower Solve IR for target capability validation")?;
            validate_solve_target_capabilities(&solve, manifest, capabilities)?;
        }
        TargetTemplateIr::Flat | TargetTemplateIr::Ast => {}
    }
    Ok(())
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
    /// `wgsl-solve` renders Solve kernels without the DAE JSON context.
    WgslSolve,
    /// A checked Algorithm Code package plus immutable artifact facts.
    AlgorithmCode {
        package: rumoca_ir_galec::package::AlgorithmCodePackage,
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
    model_identifier: &str,
) -> Result<ManifestRenderer> {
    if manifest.ir == TargetTemplateIr::Solve && manifest.name.as_deref() == Some("wgsl-solve") {
        return Ok(ManifestRenderer::WgslSolve);
    }
    if manifest.ir == TargetTemplateIr::AlgorithmCode {
        let package = lower_algorithm_code(result, model_identifier, manifest.name.as_deref())?;
        let artifact = crate::packaging::ArtifactSession::new(&manifest.files)?;
        return Ok(ManifestRenderer::AlgorithmCode { package, artifact });
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
            Self::AlgorithmCode { package, artifact } => {
                let context = crate::packaging::ArtifactRenderContext {
                    session: artifact,
                    checksums: &checksums,
                };
                rumoca_phase_codegen::render_algorithm_code_template_with_artifact(
                    package,
                    &context,
                    template,
                    model_identifier,
                )
                .map_err(anyhow::Error::from)
            }
        }
    }

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
            Self::AlgorithmCode { package, .. } => {
                rumoca_phase_codegen::render_algorithm_code_template_with_artifact(
                    package,
                    artifact,
                    template,
                    model_identifier,
                )
                .map_err(anyhow::Error::from)
            }
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

    fn command_available(command: &str) -> bool {
        Command::new(command).arg("--version").output().is_ok()
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
        let source = rumoca_phase_codegen::templates::builtin_template_source(
            "embedded-c-galec",
            "model.h.jinja",
        )
        .expect("embedded C header template");
        assert!(source.contains("GALEC-derived embedded C export"));
        assert!(source.contains("Target syntax and layout are owned by this template"));
    }
}
