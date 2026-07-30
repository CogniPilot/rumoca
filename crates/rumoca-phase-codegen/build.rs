use std::fs;
use std::io;
use std::path::{Path, PathBuf};

type BuildResult<T> = Result<T, Box<dyn std::error::Error>>;

#[derive(Debug)]
struct TargetDir {
    name: String,
    manifest_path: PathBuf,
    templates: Vec<TemplateFile>,
    assets: Vec<AssetFile>,
}

#[derive(Debug)]
struct TemplateFile {
    path: String,
    const_name: String,
    source_path: PathBuf,
}

#[derive(Debug)]
struct AssetFile {
    path: String,
    const_name: String,
    source_path: PathBuf,
}

fn main() -> BuildResult<()> {
    let manifest_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR")?);
    let templates_dir = manifest_dir.join("src/templates");
    println!("cargo:rerun-if-changed={}", templates_dir.display());

    let targets = discover_targets(&templates_dir)?;
    let generated = render_generated_templates_module(&manifest_dir, &targets);
    let out_dir = PathBuf::from(std::env::var("OUT_DIR")?);
    fs::write(out_dir.join("templates_generated.rs"), generated)?;
    Ok(())
}

fn discover_targets(templates_dir: &Path) -> BuildResult<Vec<TargetDir>> {
    let mut targets = Vec::new();
    for entry in fs::read_dir(templates_dir)? {
        let entry = entry.map_err(|error| {
            build_error(format!(
                "read entry in codegen templates directory {}: {error}",
                templates_dir.display()
            ))
        })?;
        let file_type = entry
            .file_type()
            .map_err(|error| build_error(format!("stat {}: {error}", entry.path().display())))?;
        if file_type.is_symlink() {
            return Err(build_error(format!(
                "built-in target roots may not be symlinks: {}",
                entry.path().display()
            ))
            .into());
        }
        if file_type.is_dir() && entry.path().join("target.toml").is_file() {
            targets.push(discover_target_dir(&entry.path())?);
        }
    }
    targets.sort_by(|lhs, rhs| lhs.name.cmp(&rhs.name));
    Ok(targets)
}

fn discover_target_dir(dir: &Path) -> BuildResult<TargetDir> {
    let name = dir
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or_else(|| build_error("target directory must have a UTF-8 name"))?
        .to_string();
    let manifest_path = dir.join("target.toml");
    let mut templates = Vec::new();
    for entry in fs::read_dir(dir)? {
        let entry = entry.map_err(|error| {
            build_error(format!("read target entry in {}: {error}", dir.display()))
        })?;
        let file_type = entry
            .file_type()
            .map_err(|error| build_error(format!("stat {}: {error}", entry.path().display())))?;
        if file_type.is_symlink() {
            return Err(build_error(format!(
                "built-in targets may not contain symlinks: {}",
                entry.path().display()
            ))
            .into());
        }
        let source_path = entry.path();
        if file_type.is_file()
            && source_path.extension().and_then(|ext| ext.to_str()) == Some("jinja")
        {
            let path = source_path
                .file_name()
                .and_then(|name| name.to_str())
                .ok_or_else(|| build_error("template file must have a UTF-8 name"))?
                .to_string();
            templates.push(TemplateFile {
                const_name: generated_template_const_name(&name, &path),
                path,
                source_path,
            });
        }
    }
    templates.sort_by(|lhs, rhs| lhs.path.cmp(&rhs.path));
    let mut asset_paths = Vec::new();
    collect_asset_files(dir, dir, &mut asset_paths)?;
    let assets = asset_paths
        .into_iter()
        .map(|source_path| {
            let path = relative_path(dir, &source_path)?;
            Ok(AssetFile {
                const_name: generated_asset_const_name(&name, &path),
                path,
                source_path,
            })
        })
        .collect::<BuildResult<Vec<_>>>()?;
    validate_manifest_templates(&manifest_path, &templates)?;
    validate_manifest_assets(&manifest_path, dir, &assets)?;
    Ok(TargetDir {
        name,
        manifest_path,
        templates,
        assets,
    })
}

fn collect_asset_files(root: &Path, dir: &Path, out: &mut Vec<PathBuf>) -> BuildResult<()> {
    let mut entries = fs::read_dir(dir)
        .map_err(|error| {
            build_error(format!(
                "read target asset directory {}: {error}",
                dir.display()
            ))
        })?
        .map(|entry| {
            entry.map_err(|error| {
                build_error(format!(
                    "read target asset entry in {}: {error}",
                    dir.display()
                ))
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    entries.sort_by_key(|entry| entry.path());
    for entry in entries {
        let path = entry.path();
        let file_type = entry.file_type().map_err(|error| {
            build_error(format!("stat target asset {}: {error}", path.display()))
        })?;
        if file_type.is_symlink() {
            return Err(build_error(format!(
                "built-in targets may not contain symlinks: {}",
                path.display()
            ))
            .into());
        }
        if file_type.is_dir() {
            collect_asset_files(root, &path, out)?;
        } else if file_type.is_file()
            && path != root.join("target.toml")
            && path.extension().and_then(|extension| extension.to_str()) != Some("jinja")
        {
            out.push(path);
        }
    }
    Ok(())
}

fn validate_manifest_assets(
    manifest_path: &Path,
    target_dir: &Path,
    assets: &[AssetFile],
) -> BuildResult<()> {
    let manifest = fs::read_to_string(manifest_path)?;
    for source in manifest_asset_sources(&manifest) {
        let prefix = format!("{}/", source.trim_end_matches('/'));
        if !target_dir.join(&source).is_dir() {
            return Err(build_error(format!(
                "{} references missing asset source {source}",
                manifest_path.display()
            ))
            .into());
        }
        if !assets.iter().any(|asset| asset.path.starts_with(&prefix)) {
            return Err(build_error(format!(
                "{} asset source {source} contains no regular files",
                manifest_path.display()
            ))
            .into());
        }
    }
    Ok(())
}

fn manifest_asset_sources(manifest: &str) -> Vec<String> {
    manifest
        .lines()
        .filter_map(|line| {
            let trimmed = line.trim();
            let value = trimmed.strip_prefix("source")?.trim();
            let value = value.strip_prefix('=')?.trim();
            let value = value.strip_prefix('"')?;
            let end = value.find('"')?;
            Some(value[..end].to_string())
        })
        .collect()
}

fn relative_path(root: &Path, path: &Path) -> BuildResult<String> {
    let components = path
        .strip_prefix(root)
        .map_err(|error| {
            build_error(format!(
                "target asset {} is not under target root {}: {error}",
                path.display(),
                root.display()
            ))
        })?
        .components()
        .map(|component| {
            component
                .as_os_str()
                .to_str()
                .map(ToOwned::to_owned)
                .ok_or_else(|| build_error("target asset paths must be UTF-8"))
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(components.join("/"))
}

fn validate_manifest_templates(
    manifest_path: &Path,
    templates: &[TemplateFile],
) -> BuildResult<()> {
    let manifest = fs::read_to_string(manifest_path)?;
    for referenced in manifest_template_references(&manifest) {
        if !templates.iter().any(|template| template.path == referenced) {
            return Err(build_error(format!(
                "{} references missing template {}",
                manifest_path.display(),
                referenced
            ))
            .into());
        }
    }
    Ok(())
}

fn build_error(message: impl Into<String>) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message.into())
}

fn manifest_template_references(manifest: &str) -> Vec<String> {
    manifest
        .lines()
        .filter_map(|line| {
            let trimmed = line.trim();
            let value = trimmed.strip_prefix("template")?.trim();
            let value = value.strip_prefix('=')?.trim();
            let value = value.strip_prefix('"')?;
            let end = value.find('"')?;
            Some(value[..end].to_string())
        })
        .collect()
}

fn render_generated_templates_module(manifest_dir: &Path, targets: &[TargetDir]) -> String {
    let mut out = String::from("// @generated by build.rs; do not edit by hand.\n\n");
    for target in targets {
        render_target_constants(&mut out, manifest_dir, target);
    }
    for target in targets {
        render_target_template_array(&mut out, target);
        render_target_asset_array(&mut out, target);
    }
    render_builtin_targets(&mut out, targets);
    out
}

fn render_target_constants(out: &mut String, manifest_dir: &Path, target: &TargetDir) {
    let manifest_const = generated_manifest_const_name(&target.name);
    let manifest_path = include_path(manifest_dir, &target.manifest_path);
    out.push_str(&format!(
        "const {manifest_const}: &str = include_str!(\"{manifest_path}\");\n"
    ));
    for template in &target.templates {
        let include_path = include_path(manifest_dir, &template.source_path);
        out.push_str(&format!(
            "const {}: &str = include_str!(\"{}\");\n",
            template.const_name, include_path
        ));
    }
    for asset in &target.assets {
        let include_path = include_path(manifest_dir, &asset.source_path);
        out.push_str(&format!(
            "const {}: &[u8] = include_bytes!(\"{}\");\n",
            asset.const_name, include_path
        ));
    }
    out.push('\n');
}

fn render_target_template_array(out: &mut String, target: &TargetDir) {
    let array_const = generated_target_templates_const_name(&target.name);
    out.push_str(&format!(
        "const {array_const}: &[BuiltinTargetTemplate] = &[\n"
    ));
    for template in &target.templates {
        out.push_str(&format!(
            "    BuiltinTargetTemplate {{ path: \"{}\", source: {} }},\n",
            template.path, template.const_name
        ));
    }
    out.push_str("];\n\n");
}

fn render_target_asset_array(out: &mut String, target: &TargetDir) {
    let array_const = generated_target_assets_const_name(&target.name);
    out.push_str(&format!(
        "const {array_const}: &[BuiltinTargetAsset] = &[\n"
    ));
    for asset in &target.assets {
        out.push_str(&format!(
            "    BuiltinTargetAsset {{ path: \"{}\", bytes: {} }},\n",
            asset.path, asset.const_name
        ));
    }
    out.push_str("];\n\n");
}

fn render_builtin_targets(out: &mut String, targets: &[TargetDir]) {
    out.push_str("pub const BUILTIN_TARGETS: &[BuiltinTarget] = &[\n");
    for target in targets {
        out.push_str(&format!(
            "    BuiltinTarget {{ name: \"{}\", manifest: {}, templates: {}, assets: {} }},\n",
            target.name,
            generated_manifest_const_name(&target.name),
            generated_target_templates_const_name(&target.name),
            generated_target_assets_const_name(&target.name)
        ));
    }
    out.push_str("];\n");
}

fn include_path(manifest_dir: &Path, path: &Path) -> String {
    let _ = manifest_dir;
    path.to_string_lossy().replace('\\', "/")
}

fn generated_manifest_const_name(target: &str) -> String {
    format!("{}_TARGET_MANIFEST", screaming_identifier(target))
}

fn generated_target_templates_const_name(target: &str) -> String {
    format!("{}_TARGET_TEMPLATES", screaming_identifier(target))
}

fn generated_target_assets_const_name(target: &str) -> String {
    format!("{}_TARGET_ASSETS", screaming_identifier(target))
}

fn generated_template_const_name(target: &str, template: &str) -> String {
    format!(
        "{}_{}",
        screaming_identifier(target),
        screaming_identifier(template)
    )
}

fn generated_asset_const_name(target: &str, asset: &str) -> String {
    format!(
        "{}_ASSET_{}",
        screaming_identifier(target),
        screaming_identifier(asset)
    )
}

fn screaming_identifier(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut last_was_separator = true;
    for ch in input.chars() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch.to_ascii_uppercase());
            last_was_separator = false;
        } else if !last_was_separator {
            out.push('_');
            last_was_separator = true;
        }
    }
    while out.ends_with('_') {
        out.pop();
    }
    out
}
