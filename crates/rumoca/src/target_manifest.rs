use std::path::{Path, PathBuf};
#[cfg(test)]
use std::process::Command;

use crate::{CompilationResult, TemplateIr};
use anyhow::{Context, Result, bail};
use rumoca_compile::codegen::targets::{
    RenderedTargetFile, TargetBuildKind, TargetBundle, TargetCapabilities, TargetFile,
    TargetManifest, TargetTemplateIr, TargetTemplateSource, TensorCapability,
    ensure_target_has_rendered_files, safe_target_join, validate_dae_target_capabilities,
};

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
    // The `galec`/`galec-production` eFMU targets render their manifests +
    // `__content.xml` through the declarative checksum-web build step (contract
    // §9 WI-5). In memory that is `packaging::render_web_files` — the same
    // topological render + hash-inject the CLI writes, minus the on-disk
    // packaging — so CI exercises the exact web the container writer will.
    if let Some(plan) = build_galec_plan(result, &manifest, model, &model_identifier)? {
        let render = galec_manifest_render(&plan, &bundle, &model_identifier);
        return crate::packaging::render_web_files(&manifest.files, render);
    }
    let renderer = resolve_manifest_renderer(result, &manifest, &model_identifier)?;
    render_manifest_files(result, &renderer, &bundle, &manifest, &model_identifier)
}

/// Build the switch-dispatch eFMU packaging plan (contract §9 WI-5) for the
/// `galec`/`galec-production` targets, or `None` for any other target. The
/// GALEC projection runs here — once, before any filesystem effect — so a
/// rejection surfaces before an output directory is created.
fn build_galec_plan(
    result: &CompilationResult,
    manifest: &TargetManifest,
    model: &str,
    model_identifier: &str,
) -> Result<Option<rumoca_compile::galec::GalecPackagingPlan>> {
    if manifest.ir != TargetTemplateIr::Dae {
        return Ok(None);
    }
    match manifest.name.as_deref() {
        Some("galec") => Ok(Some(
            rumoca_compile::galec::plan_galec_export(
                &result.dae,
                &result.flat,
                model_identifier,
                model,
            )
            .context("GALEC eFMU plan for target 'galec'")?,
        )),
        Some("galec-production") => Ok(Some(
            rumoca_compile::galec::plan_galec_production_export(
                &result.dae,
                &result.flat,
                model_identifier,
                model,
            )
            .context("eFMI Production Code eFMU plan for target 'galec-production'")?,
        )),
        _ => Ok(None),
    }
}

/// The per-file render closure driving the declarative eFMU build step for a
/// GALEC packaging plan (contract §9 WI-5). It resolves each `[[files]]`
/// template from the bundle (or renders a `path` template inline), asks the
/// plan for the product-agnostic manifest context — into which the plan slots
/// the build-step-injected checksums (keyed by their `as` name) — and renders
/// under a strict-undefined env with the `xml_escape`/`xs_double` filters. The
/// `.alg`/`.h`/`.c` passthrough templates read the top-level `galec_*` keys;
/// the manifest templates read `ctx`.
fn galec_manifest_render<'a>(
    plan: &'a rumoca_compile::galec::GalecPackagingPlan,
    bundle: &'a TargetBundle,
    model_identifier: &'a str,
) -> impl Fn(&str, &std::collections::BTreeMap<String, String>) -> Result<String> + 'a {
    let mut env = minijinja::Environment::new();
    env.set_undefined_behavior(minijinja::UndefinedBehavior::Strict);
    register_manifest_filters(&mut env);
    move |template: &str, checksums: &std::collections::BTreeMap<String, String>| {
        let source = if template.ends_with(".jinja") {
            bundle.template_source(template)?
        } else {
            std::borrow::Cow::Borrowed(template)
        };
        let ctx_value = plan
            .template_ctx(template, checksums)
            .map_err(anyhow::Error::from)?;
        env.render_str(
            source.as_ref(),
            minijinja::context! {
                model_name => model_identifier,
                galec_alg_source => plan.alg_text(),
                galec_c_header => plan.c_header(),
                galec_c_source => plan.c_source(),
                ctx => minijinja::Value::from_serialize(&ctx_value),
            },
        )
        .map_err(|error| anyhow::anyhow!("Render galec template '{template}': {error}"))
    }
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
            .render(result, &file.path, model_identifier)
            .with_context(|| format!("Render target output path '{}'", file.path))?;
        let template = bundle.template_source(&file.template)?;
        let content = renderer
            .render(result, template.as_ref(), model_identifier)
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
    }
}

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

    // The `galec`/`galec-production` eFMU targets render their manifests +
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

/// Compile a `build = "efmu"` target (`galec`/`galec-production`, contract §9
/// WI-5): project once into a packaging plan, then drive the declarative
/// checksum-web build step to render every manifest + `__content.xml` and
/// package both eFMU forms (directory + `.efmu` zip).
///
/// The directory form lives in its own pristine `<out_dir>/<model>/` root
/// (eFMI ch. 2 defines the directory as a package format whose root must hold
/// exactly `__content.xml`, `schemas/`, and representation containers), and the
/// `.efmu` zip sits beside it — matching the `build = "fmu"` overwrite-on-re-run
/// UX. The plan (and its GALEC projection) is built before any filesystem
/// effect, so a rejected model leaves no directory behind.
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
    let render = galec_manifest_render(&plan, bundle, model_identifier);
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
const EFMU_CONTENT_INDEX: &str = "__content.xml";

/// Write every `[[files]]` entry of a manifest target under `out_dir` (the
/// non-packaged and FMU paths; the eFMU path packages the declarative build
/// step's renders instead).
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
    validate_dae_target_capabilities(&result.dae, manifest, capabilities)?;
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
    if scalar_fallback {
        return Ok(());
    }
    let tensor = capabilities.tensor.as_ref();
    let solve = rumoca_sim::lower_solve_problem(&result.dae)
        .context("Lower Solve IR for target capability validation")?;
    let inventory = solve.compute_node_counts();
    let matmul_capability = tensor.and_then(|tensor| tensor.matmul);
    let linsolve_capability = tensor.and_then(|tensor| tensor.linsolve);

    if inventory.matmul > 0 && matmul_capability.is_none() && !scalar_fallback {
        unsupported_tensor_feature(
            manifest,
            "tensor.matmul",
            "MatMul nodes are present but the target does not declare native MatMul support and scalar fallback is disabled",
        )?;
    }
    if inventory.linsolve > 0 && linsolve_capability.is_none() && !scalar_fallback {
        unsupported_tensor_feature(
            manifest,
            "tensor.linsolve",
            "LinSolve nodes are present but the target does not declare native LinSolve support and scalar fallback is disabled",
        )?;
    }
    if inventory.matmul > 0
        && matmul_capability == Some(TensorCapability::Scalar)
        && !scalar_fallback
    {
        unsupported_tensor_feature(
            manifest,
            "tensor.matmul",
            "MatMul is configured for scalar fallback but scalar fallback is disabled",
        )?;
    }
    if inventory.linsolve > 0
        && linsolve_capability == Some(TensorCapability::Scalar)
        && !scalar_fallback
    {
        unsupported_tensor_feature(
            manifest,
            "tensor.linsolve",
            "LinSolve is configured for scalar fallback but scalar fallback is disabled",
        )?;
    }
    Ok(())
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

fn default_target_output_dir(manifest: &TargetManifest, model_identifier: &str) -> PathBuf {
    match manifest.build {
        Some(TargetBuildKind::Fmu) => PathBuf::from(format!("{model_identifier}.fmu")),
        // The eFMU out dir holds both package forms (`<model>/` directory
        // form + `<model>.efmu` zip; layout docs in `efmu.rs`), so it keeps
        // the plain model name rather than a package extension.
        Some(TargetBuildKind::Efmu) | None => PathBuf::from(model_identifier),
    }
}

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
    let rendered = renderer
        .render(result, template.as_ref(), model_identifier)
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
/// computation. The `galec`/`galec-production` eFMU targets do NOT go through
/// this enum — they drive the declarative checksum-web build step
/// ([`compile_efmu_target`] / [`galec_manifest_render`], contract §9 WI-5).
enum ManifestRenderer {
    /// Generic path: the IR-keyed JSON template context.
    Ir(TemplateIr),
    /// `wgsl-solve` renders Solve kernels without the DAE JSON context.
    WgslSolve,
    /// `embedded-c-galec` renders thin C templates over one typed
    /// projection context (SPEC_0034 GAL-024/D2) — never the generic DAE
    /// JSON context.
    GalecC(rumoca_compile::galec::GalecCExport),
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
    if manifest.ir == TargetTemplateIr::Dae && manifest.name.as_deref() == Some("embedded-c-galec")
    {
        let export = rumoca_compile::galec::render_galec_c_export(
            &result.dae,
            &result.flat,
            model_identifier,
        )
        .context("GALEC C export for target 'embedded-c-galec'")?;
        return Ok(ManifestRenderer::GalecC(export));
    }
    Ok(ManifestRenderer::Ir(template_ir_to_cli(manifest.ir)))
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
        match self {
            Self::Ir(ir) => result
                .render_template_str_with_name_and_ir(template, model_identifier, *ir)
                .map_err(Into::into),
            Self::WgslSolve => result
                .render_solve_template_str_without_dae(template, model_identifier)
                .map_err(Into::into),
            Self::GalecC(export) => render_galec_c_template(export, template),
        }
    }
}

/// Register the eFMI manifest render filters (contract §3b) on a bare
/// minijinja environment: `xml_escape` (autoescape is OFF, so every text
/// value is escaped explicitly) and `xs_double` (raw `f64` → valid
/// `xs:double` lexical). The real `galec`/`galec-production` manifest
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
/// (`galec-production`) supplies its PC-representation claim the same way
/// at its render site inside the compile facade (`galec_api.rs`, where the
/// C files must render so their SHA-1s enter the Production Code manifest).
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

/// The `embedded-c-galec` claim: the non-eFMI track of GAL-024 must
/// self-describe as NOT an eFMI Production Code container (pinned by the
/// CLI honesty test `export_self_describes_as_not_an_efmi_production_code_container`).
const EMBEDDED_C_GALEC_CONFORMANCE_HEADER: CConformanceHeader = CConformanceHeader {
    lines: &[
        "NOT an eFMI Production Code container: no eFMU manifest mapping exists",
        "for this artifact. Generated by rumoca --target embedded-c-galec.",
    ],
    summary: "NOT an eFMI Production Code container.",
};

/// Render an `embedded-c-galec` target template (file path or C file) from
/// the invocation's single [`rumoca_compile::galec::GalecCExport`].
///
/// The context is the projection's serialized typed `CContext`
/// (`model_name`, `block_name`, `struct_name`, `function_prefix`,
/// `include_guard`, `variables`, `methods`): all C expression/statement
/// text comes pre-printed by the typed Rust printer, the templates only
/// lay out the files (SPEC_0034 D2/GAL-008 split). The renderer adds the
/// one key that is target identity rather than projection data: this
/// target's [`CConformanceHeader`].
fn render_galec_c_template(
    export: &rumoca_compile::galec::GalecCExport,
    template: &str,
) -> Result<String> {
    let mut env = minijinja::Environment::new();
    env.set_undefined_behavior(minijinja::UndefinedBehavior::Strict);
    env.render_str(
        template,
        minijinja::context! {
            conformance_header => EMBEDDED_C_GALEC_CONFORMANCE_HEADER.context_value(),
            ..minijinja::Value::from_serialize(&export.context)
        },
    )
    .context("Render embedded-c-galec target template")
}

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

#[cfg(test)]
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
