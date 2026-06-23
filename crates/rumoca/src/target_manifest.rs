use std::path::{Path, PathBuf};
#[cfg(test)]
use std::process::Command;

use anyhow::{Context, Result, bail};
use rumoca::{CompilationResult, TemplateIr};
use rumoca_compile::codegen::render_dae_template_with_json_and_name;
use rumoca_compile::codegen::targets::{
    TargetBuildKind, TargetBundle, TargetCapabilities, TargetFile, TargetManifest,
    TargetTemplateIr, TargetTemplateSource, TensorCapability, ensure_target_has_rendered_files,
    safe_target_join, validate_dae_target_capabilities,
};
use rumoca_compile::codegen::{DaeTemplateContext, dae_to_template_json};
use serde_json::Value;

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
    compile_manifest_target(result, model, &bundle, &manifest, output)
}

fn raw_template_target(target: &str) -> bool {
    Path::new(target)
        .extension()
        .and_then(|extension| extension.to_str())
        .is_some_and(|extension| extension == "jinja")
}

fn compile_raw_template_target(
    result: &CompilationResult,
    model: &str,
    target: &str,
    output: Option<PathBuf>,
    ir: TemplateIr,
) -> Result<()> {
    let template =
        std::fs::read_to_string(target).with_context(|| format!("Read template: {target}"))?;
    let model_identifier = model.replace('.', "_");
    let rendered = result
        .render_template_str_with_name_and_ir(&template, &model_identifier, ir)
        .with_context(|| format!("Render raw template: {target}"))?;
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
    let out_dir = output.unwrap_or_else(|| default_target_output_dir(manifest, &model_identifier));
    std::fs::create_dir_all(&out_dir)?;

    eprintln!(
        "Compiling target '{}' for {}",
        bundle.label(manifest),
        model_identifier
    );
    if let Some(description) = &manifest.description {
        eprintln!("  {description}");
    }

    let render_contexts = TemplateRenderContexts::new(result, manifest)?;

    for file in &manifest.files {
        write_manifest_file(
            result,
            &render_contexts,
            bundle,
            manifest,
            file,
            &out_dir,
            &model_identifier,
        )?;
    }

    // The target.toml `build` field decides whether to package the rendered
    // output (e.g. into an FMU); there is no CLI flag.
    match manifest.build {
        Some(TargetBuildKind::Fmu) => {
            crate::fmu::build_fmu(&out_dir, &model_identifier, manifest.name.as_deref())?;
        }
        None => print_target_completion_message(manifest, &out_dir, &model_identifier)?,
    }

    Ok(())
}

struct TemplateRenderContexts {
    dae: Option<DaeTemplateContext>,
    solve: Option<Value>,
}

impl TemplateRenderContexts {
    fn new(result: &CompilationResult, manifest: &TargetManifest) -> Result<Self> {
        let needs_dae = manifest.ir == TargetTemplateIr::Dae
            || manifest
                .files
                .iter()
                .any(|file| file.ir == Some(TargetTemplateIr::Dae));
        let needs_solve = manifest.ir == TargetTemplateIr::Solve
            || manifest
                .files
                .iter()
                .any(|file| file.ir == Some(TargetTemplateIr::Solve));

        let dae = if needs_dae {
            let template_dae = result.scalarized_template_dae();
            let template_json = dae_to_template_json(&template_dae)
                .map_err(|error| anyhow::anyhow!(error.to_string()))?;
            Some(DaeTemplateContext::from_dae_json(&template_json))
        } else {
            None
        };

        let solve = if needs_solve {
            Some(solve_template_json(result)?)
        } else {
            None
        };

        Ok(Self { dae, solve })
    }
}

fn solve_template_json(result: &CompilationResult) -> Result<Value> {
    let template_dae = result.scalarized_template_dae();
    let mut value =
        dae_to_template_json(&template_dae).map_err(|error| anyhow::anyhow!(error.to_string()))?;
    let solve_model = rumoca_sim::lower_dae_to_solve_model_owned(template_dae)
        .context("Lower Solve model for target template rendering")?;
    let mut solve_value = serde_json::to_value(&solve_model.problem)
        .context("Serialize Solve IR for target template rendering")?;
    let solve_object = solve_value
        .as_object_mut()
        .ok_or_else(|| anyhow::anyhow!("Solve template JSON root must be an object"))?;
    solve_object.insert(
        "artifacts".to_string(),
        serde_json::to_value(&solve_model.artifacts)
            .context("Serialize Solve artifacts for target template rendering")?,
    );
    solve_object.insert(
        "visible_names".to_string(),
        serde_json::to_value(&solve_model.visible_names)
            .context("Serialize Solve visible names for target template rendering")?,
    );
    solve_object.insert(
        "visible_value_rows".to_string(),
        serde_json::to_value(&solve_model.visible_value_rows)
            .context("Serialize Solve visible rows for target template rendering")?,
    );
    let object = value
        .as_object_mut()
        .ok_or_else(|| anyhow::anyhow!("DAE template JSON root must be an object"))?;
    object.insert("solve".to_string(), solve_value);
    object.insert(
        "__ir_kind".to_string(),
        serde_json::Value::String("solve".to_string()),
    );
    Ok(value)
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
    if capabilities.tensor.is_none() && scalar_fallback {
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
        None => PathBuf::from(model_identifier),
    }
}

fn write_manifest_file(
    result: &CompilationResult,
    contexts: &TemplateRenderContexts,
    bundle: &TargetBundle,
    manifest: &TargetManifest,
    file: &TargetFile,
    out_dir: &Path,
    model_identifier: &str,
) -> Result<()> {
    let file_ir = file.ir.unwrap_or(manifest.ir);
    let ir = template_ir_to_cli(file_ir);
    let render_template = |template: &str| -> Result<String> {
        match file_ir {
            TargetTemplateIr::Dae => {
                let Some(context) = contexts.dae.as_ref() else {
                    bail!(
                        "target file '{}' requested DAE IR without DAE context",
                        file.path
                    )
                };
                context
                    .render_with_name(template, model_identifier)
                    .map_err(|error| anyhow::anyhow!(error.to_string()))
            }
            TargetTemplateIr::Solve => {
                let Some(context) = contexts.solve.as_ref() else {
                    bail!(
                        "target file '{}' requested Solve IR without Solve context",
                        file.path
                    )
                };
                render_dae_template_with_json_and_name(context, template, model_identifier)
                    .map_err(|error| anyhow::anyhow!(error.to_string()))
            }
            _ => result
                .render_template_str_with_name_and_ir(template, model_identifier, ir)
                .map_err(|error| anyhow::anyhow!(error.to_string())),
        }
    };

    let rendered_rel_path = render_template(&file.path)
        .with_context(|| format!("Render target output path '{}'", file.path))?;
    let output_path = safe_target_join(out_dir, rendered_rel_path.trim())?;
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let template = bundle.template_source(&file.template)?;
    let rendered = render_template(template.as_ref())
        .with_context(|| format!("Render target template '{}'", file.template))?;
    std::fs::write(&output_path, rendered)?;
    apply_manifest_file_mode(&output_path, file.mode.as_deref())?;
    eprintln!("  wrote {}", output_path.display());
    Ok(())
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
    use rumoca::Compiler;

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
    fn solve_target_can_render_dae_metadata_file_before_solve_files() {
        let result = compile_scalar_cuda_smoke_demo();
        let target_dir = tempfile::tempdir().expect("temp target dir");
        std::fs::write(
            target_dir.path().join("target.toml"),
            r#"
version = 1
ir = "solve"
name = "mixed-ir-target"

[[files]]
path = "resources/ir-kind.txt"
template = "ir-kind.txt.jinja"
ir = "dae"

[[files]]
path = "solve-kind.txt"
template = "solve-kind.txt.jinja"
"#,
        )
        .expect("write target manifest");
        std::fs::write(
            target_dir.path().join("ir-kind.txt.jinja"),
            "metadata={{ ir_kind }} model={{ model_name }}",
        )
        .expect("write DAE template");
        std::fs::write(
            target_dir.path().join("solve-kind.txt.jinja"),
            "runtime={{ ir_kind }} model={{ model_name }}",
        )
        .expect("write solve template");

        let bundle = TargetBundle::load(
            target_dir
                .path()
                .to_str()
                .expect("target dir path should be utf-8"),
        )
        .expect("load temp target");
        let manifest = bundle.parse_manifest().expect("parse temp manifest");
        let out_dir = tempfile::tempdir().expect("temp output dir");

        compile_manifest_target(
            &result,
            "ScalarCudaSmoke",
            &bundle,
            &manifest,
            Some(out_dir.path().to_path_buf()),
        )
        .expect("mixed IR target should render");

        let metadata = std::fs::read_to_string(out_dir.path().join("resources/ir-kind.txt"))
            .expect("read DAE metadata file");
        let runtime = std::fs::read_to_string(out_dir.path().join("solve-kind.txt"))
            .expect("read solve file");
        assert_eq!(metadata, "metadata=dae model=ScalarCudaSmoke");
        assert_eq!(runtime, "runtime=solve model=ScalarCudaSmoke");
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
}
