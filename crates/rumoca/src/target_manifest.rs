use std::borrow::Cow;
use std::path::{Component, Path, PathBuf};

use anyhow::{Context, Result, bail};
use rumoca::CompilationResult;
use serde::Deserialize;

pub(crate) fn compile_target(
    result: &CompilationResult,
    model: &str,
    target: &str,
    output: Option<PathBuf>,
    build: bool,
) -> Result<()> {
    let bundle = TargetBundle::load(target)?;
    let manifest = bundle.parse_manifest()?;
    compile_manifest_target(result, model, &bundle, &manifest, output, build)
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct TargetManifest {
    version: u32,
    name: Option<String>,
    description: Option<String>,
    build: Option<TargetBuildKind>,
    #[serde(alias = "requires")]
    requirements: Option<TargetRequirements>,
    files: Vec<TargetFile>,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
enum TargetBuildKind {
    Fmu,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct TargetRequirements {
    continuous_states: Option<bool>,
    residual_equations: Option<bool>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct TargetFile {
    path: String,
    template: String,
    mode: Option<String>,
}

enum TargetBundle {
    Builtin {
        name: &'static str,
        manifest: &'static str,
    },
    Directory {
        dir: PathBuf,
        manifest: String,
    },
}

impl TargetBundle {
    fn load(target: &str) -> Result<Self> {
        use rumoca_session::codegen::templates;

        match target {
            "fmi2" => Ok(Self::Builtin {
                name: "fmi2",
                manifest: templates::FMI2_TARGET_MANIFEST,
            }),
            "fmi3" => Ok(Self::Builtin {
                name: "fmi3",
                manifest: templates::FMI3_TARGET_MANIFEST,
            }),
            "embedded-c" => Ok(Self::Builtin {
                name: "embedded-c",
                manifest: templates::EMBEDDED_C_TARGET_MANIFEST,
            }),
            custom => {
                let dir = PathBuf::from(custom);
                let manifest_path = dir.join("target.yaml");
                let manifest = std::fs::read_to_string(&manifest_path).with_context(|| {
                    format!(
                        "Read target manifest for '{}' at {}",
                        custom,
                        manifest_path.display()
                    )
                })?;
                Ok(Self::Directory { dir, manifest })
            }
        }
    }

    fn parse_manifest(&self) -> Result<TargetManifest> {
        let manifest: TargetManifest = match self {
            Self::Builtin { manifest, .. } => serde_yaml::from_str(manifest),
            Self::Directory { manifest, .. } => serde_yaml::from_str(manifest),
        }
        .context("Parse target.yaml")?;

        if manifest.version != 1 {
            bail!(
                "Unsupported target manifest version {}; expected version 1",
                manifest.version
            );
        }
        if manifest.files.is_empty() {
            bail!("target.yaml must contain at least one file entry");
        }
        Ok(manifest)
    }

    fn label<'a>(&'a self, manifest: &'a TargetManifest) -> &'a str {
        manifest.name.as_deref().unwrap_or(match self {
            Self::Builtin { name, .. } => name,
            Self::Directory { dir, .. } => dir.to_str().unwrap_or("custom"),
        })
    }

    fn template_source(&self, template: &str) -> Result<Cow<'static, str>> {
        match self {
            Self::Builtin { .. } => builtin_target_template_source(template)
                .map(Cow::Borrowed)
                .ok_or_else(|| {
                    anyhow::anyhow!("Built-in target references unknown template '{template}'")
                }),
            Self::Directory { dir, .. } => {
                let path = safe_join(dir, template)?;
                if path.is_file() {
                    return std::fs::read_to_string(&path)
                        .map(Cow::Owned)
                        .with_context(|| format!("Read target template {}", path.display()));
                }
                builtin_target_template_source(template)
                    .map(Cow::Borrowed)
                    .ok_or_else(|| anyhow::anyhow!("Target template not found: {}", path.display()))
            }
        }
    }
}

fn compile_manifest_target(
    result: &CompilationResult,
    model: &str,
    bundle: &TargetBundle,
    manifest: &TargetManifest,
    output: Option<PathBuf>,
    build: bool,
) -> Result<()> {
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

    for file in &manifest.files {
        write_manifest_file(result, bundle, file, &out_dir, &model_identifier)?;
    }

    if build {
        match manifest.build {
            Some(TargetBuildKind::Fmu) => crate::build_fmu(&out_dir, &model_identifier)?,
            None => bail!(
                "--build is not supported for target '{}'",
                bundle.label(manifest)
            ),
        }
    } else {
        print_target_completion_message(manifest, &out_dir, &model_identifier);
    }

    Ok(())
}

fn builtin_target_template_source(template: &str) -> Option<&'static str> {
    use rumoca_session::codegen::templates;

    match template {
        "fmi2/modelDescription.xml.jinja" => Some(templates::FMI2_MODEL_DESCRIPTION),
        "fmi2/model.c.jinja" => Some(templates::FMI2_MODEL),
        "fmi3/modelDescription.xml.jinja" => Some(templates::FMI3_MODEL_DESCRIPTION),
        "fmi3/model.c.jinja" => Some(templates::FMI3_MODEL),
        "fmu/CMakeLists.txt.jinja" => Some(templates::FMU_CMAKE_LISTS),
        "fmu/build.sh.jinja" => Some(templates::FMU_BUILD_SCRIPT),
        "embedded_c/model.h.jinja" => Some(templates::EMBEDDED_C_H),
        "embedded_c/model.c.jinja" => Some(templates::EMBEDDED_C_IMPL),
        _ => None,
    }
}

fn validate_target_requirements(
    result: &CompilationResult,
    manifest: &TargetManifest,
) -> Result<()> {
    let Some(requirements) = &manifest.requirements else {
        return Ok(());
    };

    if requirements.continuous_states == Some(false)
        && (!result.dae.states.is_empty() || !result.dae.f_x.is_empty())
    {
        bail!(
            "Target '{}' does not support continuous dynamics: {} state(s), {} residual derivative equation(s)",
            manifest.name.as_deref().unwrap_or("custom"),
            result.dae.states.len(),
            result.dae.f_x.len()
        );
    }
    if requirements.residual_equations == Some(false) && !result.dae.f_x.is_empty() {
        bail!(
            "Target '{}' does not support residual derivative equations: {} equation(s)",
            manifest.name.as_deref().unwrap_or("custom"),
            result.dae.f_x.len()
        );
    }
    Ok(())
}

fn default_target_output_dir(manifest: &TargetManifest, model_identifier: &str) -> PathBuf {
    match manifest.build {
        Some(TargetBuildKind::Fmu) => PathBuf::from(format!("{model_identifier}.fmu")),
        None => PathBuf::from(model_identifier),
    }
}

fn write_manifest_file(
    result: &CompilationResult,
    bundle: &TargetBundle,
    file: &TargetFile,
    out_dir: &Path,
    model_identifier: &str,
) -> Result<()> {
    let rendered_rel_path = result
        .render_template_str_with_name(&file.path, model_identifier)
        .with_context(|| format!("Render target output path '{}'", file.path))?;
    let output_path = safe_join(out_dir, rendered_rel_path.trim())?;
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let template = bundle.template_source(&file.template)?;
    let rendered = result
        .render_template_str_with_name(template.as_ref(), model_identifier)
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
) {
    match manifest.build {
        Some(TargetBuildKind::Fmu) => eprintln!(
            "\nFMU sources compiled to: {}\nRun ./build.sh to compile and package the .fmu",
            out_dir.display()
        ),
        None if manifest.name.as_deref() == Some("embedded-c") => eprintln!(
            "\nEmbedded C sources compiled to: {}\nCompile: cc -O2 -Wall -c {}/{}.c",
            out_dir.display(),
            out_dir.display(),
            model_identifier,
        ),
        None => eprintln!("\nTarget sources compiled to: {}", out_dir.display()),
    }
}

fn safe_join(root: &Path, relative: impl AsRef<Path>) -> Result<PathBuf> {
    let relative = relative.as_ref();
    if relative.as_os_str().is_empty() {
        bail!("Target manifest path must not be empty");
    }
    if relative.is_absolute() {
        bail!(
            "Target manifest path '{}' must be relative",
            relative.display()
        );
    }
    for component in relative.components() {
        match component {
            Component::Normal(_) | Component::CurDir => {}
            Component::ParentDir | Component::RootDir | Component::Prefix(_) => {
                bail!(
                    "Target manifest path '{}' must not escape the target root",
                    relative.display()
                );
            }
        }
    }
    Ok(root.join(relative))
}

#[cfg(test)]
mod tests {
    use super::safe_join;
    use std::path::Path;

    #[test]
    fn target_manifest_rejects_escaping_paths() {
        let root = Path::new("out");
        assert!(safe_join(root, "../escape").is_err());
        assert!(safe_join(root, "/absolute").is_err());
        assert_eq!(
            safe_join(root, "nested/file.c").unwrap(),
            root.join("nested/file.c")
        );
    }
}
