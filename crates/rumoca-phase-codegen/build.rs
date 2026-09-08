//! Build-time discovery of the built-in code-gen target bundles.
//!
//! # Artifact templates
//!
//! Every `.jinja` file in a target directory is an artifact template declared
//! by exactly one `[[files]]` entry. A declaration may instead borrow that one
//! complete template from an explicitly named built-in owner with the same
//! artifact kind and semantic context; the borrower then has no local copy.
//! Global partials, aliases, composition, and fallback lookup are rejected
//! because they can bridge semantic contexts. A syntax helper uniquely owned
//! by one artifact stays local to that template.
//!
//! Note that the `__` prefix of `__content.xml.jinja` carries no meaning for
//! this classification: `__content.xml` is the eFMI container registry file
//! name, and that template is an ordinary `[[files]]` artifact.
//!
//! # Shared asset bundles
//!
//! A `[[assets]]` bundle names a directory of non-template files copied into
//! the product verbatim. Two targets that must ship the *same* bytes declare
//! one owning copy and borrow it: the borrower writes `shared_from = "<owning
//! target>"` on its `[[assets]]` entry and keeps no directory of its own. The
//! borrowed files enter the borrower's bundle under the identical relative
//! paths, so nothing downstream can tell a borrowed bundle from an owned one,
//! and they enter it through the owner's `include_bytes!` constants, so the
//! bytes are embedded once however many targets ship them. Two vendored copies
//! of one upstream tree drift, and a drifted copy is a non-conformant
//! container.

use std::collections::BTreeMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

type BuildResult<T> = Result<T, Box<dyn std::error::Error>>;

#[derive(Debug)]
struct TargetDir {
    name: String,
    manifest_path: PathBuf,
    readme_path: PathBuf,
    templates: Vec<TemplateFile>,
    template_declarations: BTreeMap<String, TemplateDeclaration>,
    assets: Vec<AssetFile>,
    /// `[[assets]]` bundles this target borrows from another target, as
    /// (`source`, owning target name). Resolved once every target directory
    /// has been read, since the owner may be discovered later.
    borrowed_assets: Vec<(String, String)>,
}

#[derive(Debug)]
struct TemplateFile {
    path: String,
    const_name: String,
    source_path: PathBuf,
}

#[derive(Debug, Clone)]
struct TemplateDeclaration {
    shared_from: Option<String>,
    artifact_kind: String,
    semantic_context: String,
}

#[derive(Debug, Clone)]
struct AssetFile {
    path: String,
    const_name: String,
    source_path: PathBuf,
    /// The target whose directory holds these bytes, which is this target for
    /// an owned asset and the lending target for a borrowed one. Only the
    /// owner emits the `include_bytes!` constant; every borrower's bundle
    /// points at that same constant.
    owner: String,
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
    validate_borrowed_templates(&targets)?;
    resolve_borrowed_assets(&mut targets)?;
    Ok(targets)
}

/// Prove every per-file template borrowing edge against the closed built-in
/// target registry before generated constants exist. Borrowing lends one
/// complete artifact template; it never performs a fallback lookup and never
/// forms a chain through another borrower.
fn validate_borrowed_templates(targets: &[TargetDir]) -> BuildResult<()> {
    for borrower in targets {
        for (path, declaration) in &borrower.template_declarations {
            let Some(owner_name) = declaration.shared_from.as_deref() else {
                continue;
            };
            if owner_name == borrower.name {
                return Err(build_error(format!(
                    "{}: template {path} cannot borrow from its declaring target {owner_name}",
                    borrower.manifest_path.display()
                ))
                .into());
            }
            let Some(owner) = targets.iter().find(|target| target.name == owner_name) else {
                return Err(build_error(format!(
                    "{}: template {path} borrows from unknown built-in target {owner_name}",
                    borrower.manifest_path.display()
                ))
                .into());
            };
            let Some(owner_declaration) = owner.template_declarations.get(path) else {
                return Err(build_error(format!(
                    "{}: target {owner_name} owns no declared template {path}",
                    borrower.manifest_path.display()
                ))
                .into());
            };
            if owner_declaration.shared_from.is_some()
                || !owner
                    .templates
                    .iter()
                    .any(|template| template.path == *path)
            {
                return Err(build_error(format!(
                    "{}: template {path} must name its canonical byte owner, not borrowing target {owner_name}",
                    borrower.manifest_path.display()
                ))
                .into());
            }
            if (
                declaration.artifact_kind.as_str(),
                declaration.semantic_context.as_str(),
            ) != (
                owner_declaration.artifact_kind.as_str(),
                owner_declaration.semantic_context.as_str(),
            ) {
                return Err(build_error(format!(
                    "{}: borrowed template {path} changes artifact kind or semantic context from owner {owner_name}",
                    borrower.manifest_path.display()
                ))
                .into());
            }
        }
    }
    Ok(())
}

/// Copy every `shared_from` bundle's file list from its owning target into the
/// borrower, under the identical relative paths and pointing at the owner's
/// byte constants.
///
/// The lender snapshot is taken before any borrow is applied, so a target can
/// only lend what its own directory holds: borrowing a bundle that is itself
/// borrowed reports the lender as bundling no such files, which is the right
/// answer, because the owning target is the one to name.
fn resolve_borrowed_assets(targets: &mut [TargetDir]) -> BuildResult<()> {
    let owned: BTreeMap<String, Vec<AssetFile>> = targets
        .iter()
        .map(|target| (target.name.clone(), target.assets.clone()))
        .collect();
    for target in targets.iter_mut() {
        let borrowed = std::mem::take(&mut target.borrowed_assets);
        for (source, owner) in borrowed {
            let prefix = format!("{}/", source.trim_end_matches('/'));
            if owner == target.name {
                return Err(build_error(format!(
                    "{}: [[assets]] source {source} declares shared_from = \"{owner}\", \
                     which is the declaring target itself",
                    target.manifest_path.display()
                ))
                .into());
            }
            let Some(owner_assets) = owned.get(&owner) else {
                return Err(build_error(format!(
                    "{}: [[assets]] source {source} borrows from unknown target {owner}",
                    target.manifest_path.display()
                ))
                .into());
            };
            if target.assets.iter().any(|asset| {
                asset.path.starts_with(&prefix) || asset.path == source.trim_end_matches('/')
            }) {
                return Err(build_error(format!(
                    "{}: [[assets]] source {source} borrows from {owner} but this target \
                     also bundles files under {prefix}; a borrowed bundle must have no \
                     local copy to drift from",
                    target.manifest_path.display()
                ))
                .into());
            }
            let lent = owner_assets
                .iter()
                .filter(|asset| asset.path.starts_with(&prefix))
                .cloned()
                .collect::<Vec<_>>();
            if lent.is_empty() {
                return Err(build_error(format!(
                    "{}: [[assets]] source {source} borrows from {owner}, which bundles no \
                     files under {prefix}",
                    target.manifest_path.display()
                ))
                .into());
            }
            target.assets.extend(lent);
        }
        target.assets.sort_by(|lhs, rhs| lhs.path.cmp(&rhs.path));
    }
    Ok(())
}

fn discover_target_dir(dir: &Path) -> BuildResult<TargetDir> {
    let name = dir
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or_else(|| build_error("target directory must have a UTF-8 name"))?
        .to_string();
    let manifest_path = dir.join("target.toml");
    let readme_path = dir.join("README.md");
    validate_target_readme(&readme_path, &name)?;
    let declarations = ManifestDeclarations::read(&manifest_path)?;
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
            declarations.require_artifact(&manifest_path, &path)?;
            templates.push(TemplateFile {
                const_name: generated_template_const_name(&name, &path),
                path,
                source_path,
            });
        }
    }
    templates.sort_by(|lhs, rhs| lhs.path.cmp(&rhs.path));
    declarations.validate_every_declaration_has_a_file(&manifest_path, &templates)?;
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
                owner: name.clone(),
            })
        })
        .collect::<BuildResult<Vec<_>>>()?;
    let borrowed_assets = validate_manifest_assets(&manifest_path, dir, &assets)?;
    Ok(TargetDir {
        name,
        manifest_path,
        readme_path,
        templates,
        template_declarations: declarations.files,
        assets,
        borrowed_assets,
    })
}

fn validate_target_readme(path: &Path, target: &str) -> BuildResult<()> {
    let readme = fs::read_to_string(path).map_err(|error| {
        build_error(format!(
            "built-in target {target} requires README.md at {}: {error}",
            path.display()
        ))
    })?;
    let required = [
        format!("# `{target}`"),
        "## Use case".to_string(),
        "## Contract".to_string(),
        "## Unsupported".to_string(),
        "## Verification".to_string(),
        "## Example".to_string(),
    ];
    let missing = required
        .iter()
        .filter(|heading| !readme.lines().any(|line| line == heading.as_str()))
        .cloned()
        .collect::<Vec<_>>();
    if !missing.is_empty() {
        return Err(build_error(format!(
            "{} is missing required target documentation headings: {}",
            path.display(),
            missing.join(", ")
        ))
        .into());
    }
    Ok(())
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
            && path != root.join("README.md")
            && path.extension().and_then(|extension| extension.to_str()) != Some("jinja")
        {
            out.push(path);
        }
    }
    Ok(())
}

/// Check that every `[[assets]]` bundle the manifest declares is backed by
/// files, and return the bundles this target borrows from another one.
///
/// A borrowed bundle has no directory here by construction, so it is checked
/// after discovery instead, once its lender is known
/// (`resolve_borrowed_assets`).
fn validate_manifest_assets(
    manifest_path: &Path,
    target_dir: &Path,
    assets: &[AssetFile],
) -> BuildResult<Vec<(String, String)>> {
    let manifest = fs::read_to_string(manifest_path)?;
    let mut borrowed = Vec::new();
    for (source, shared_from) in manifest_asset_sources(&manifest) {
        if let Some(owner) = shared_from {
            if owner.is_empty() {
                return Err(build_error(format!(
                    "{}: [[assets]] source {source} declares an empty shared_from target",
                    manifest_path.display()
                ))
                .into());
            }
            borrowed.push((source, owner));
            continue;
        }
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
    Ok(borrowed)
}

/// Every `[[assets]]` bundle as (`source`, `shared_from`).
fn manifest_asset_sources(manifest: &str) -> Vec<(String, Option<String>)> {
    scan_manifest_tables(manifest)
        .into_iter()
        .filter(|(table, _)| table == "assets")
        .filter_map(|(_, row)| {
            let source = row_value(&row, "source")?.to_string();
            let shared_from = row_value(&row, "shared_from").map(ToOwned::to_owned);
            Some((source, shared_from))
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

fn build_error(message: impl Into<String>) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message.into())
}

/// The artifact-template declarations a `target.toml` makes.
///
/// The semantic manifest parser lives in `rumoca-compile`
/// (`codegen_target::parse_target_manifest_construction`, which owns the typed
/// `TargetFile` shape). This build script only needs template paths, so it reads them with the narrow scanner
/// below rather than taking a TOML build-dependency; disagreement between the
/// two readers cannot go unnoticed, because a template this scanner fails to
/// see is reported as an undeclared file and fails the build.
#[derive(Debug, Default)]
struct ManifestDeclarations {
    /// Exact `[[files]].template` declarations and optional single-owner
    /// borrowing edges. Global aliases and fallback lookup remain forbidden.
    files: BTreeMap<String, TemplateDeclaration>,
}

impl ManifestDeclarations {
    fn read(manifest_path: &Path) -> BuildResult<Self> {
        let manifest = fs::read_to_string(manifest_path)
            .map_err(|error| build_error(format!("read {}: {error}", manifest_path.display())))?;
        let mut declarations = Self::default();
        for (table, row) in scan_manifest_tables(&manifest) {
            match table.as_str() {
                "files" => declarations.add_file(manifest_path, &row)?,
                "partials" => {
                    return Err(build_error(format!(
                        "{}: [[partials]] is forbidden; keep presentation helpers local to their sole artifact template",
                        manifest_path.display()
                    ))
                    .into());
                }
                _ => {}
            }
        }
        Ok(declarations)
    }

    fn add_file(&mut self, manifest_path: &Path, row: &[(String, String)]) -> BuildResult<()> {
        let Some(template) = row_value(row, "template") else {
            return Err(build_error(format!(
                "{}: every [[files]] entry must declare a template",
                manifest_path.display()
            ))
            .into());
        };
        if row_value(row, "shared_as").is_some() {
            return Err(build_error(format!(
                "{}: [[files]].shared_as is forbidden; global aliases can bridge semantic contexts",
                manifest_path.display()
            ))
            .into());
        }
        let artifact_kind = row_value(row, "artifact_kind").ok_or_else(|| {
            build_error(format!(
                "{}: [[files]] template {template} must declare artifact_kind",
                manifest_path.display()
            ))
        })?;
        let semantic_context = row_value(row, "semantic_context").ok_or_else(|| {
            build_error(format!(
                "{}: [[files]] template {template} must declare semantic_context",
                manifest_path.display()
            ))
        })?;
        let declaration = TemplateDeclaration {
            shared_from: row_value(row, "template_shared_from").map(str::to_owned),
            artifact_kind: artifact_kind.to_owned(),
            semantic_context: semantic_context.to_owned(),
        };
        if self
            .files
            .insert(template.to_owned(), declaration)
            .is_some()
        {
            return Err(build_error(format!(
                "{}: template {template} has more than one [[files]] entry",
                manifest_path.display()
            ))
            .into());
        }
        Ok(())
    }

    /// Require one discovered `.jinja` file to be an artifact declared by a
    /// `[[files]]` row.
    fn require_artifact(&self, manifest_path: &Path, path: &str) -> BuildResult<()> {
        let Some(declaration) = self.files.get(path) else {
            return Err(build_error(format!(
                "{}: template {path} is bundled but undeclared; every template must have one [[files]] artifact entry",
                manifest_path.display()
            ))
            .into());
        };
        if let Some(owner) = declaration.shared_from.as_deref() {
            return Err(build_error(format!(
                "{}: borrowed template {path} from {owner} also exists locally; one target must own the bytes",
                manifest_path.display()
            ))
            .into());
        }
        Ok(())
    }

    fn validate_every_declaration_has_a_file(
        &self,
        manifest_path: &Path,
        templates: &[TemplateFile],
    ) -> BuildResult<()> {
        for (declared, declaration) in &self.files {
            let local = templates.iter().any(|template| &template.path == declared);
            if declaration.shared_from.is_none() && !local {
                return Err(build_error(format!(
                    "{} references missing template {declared}",
                    manifest_path.display()
                ))
                .into());
            }
            if declaration.shared_from.is_some() && local {
                return Err(build_error(format!(
                    "{} borrowed template {declared} must not have a local copy",
                    manifest_path.display()
                ))
                .into());
            }
        }
        Ok(())
    }
}

fn row_value<'a>(row: &'a [(String, String)], key: &str) -> Option<&'a str> {
    row.iter()
        .find(|(name, _)| name == key)
        .map(|(_, value)| value.as_str())
}

/// Collect `(table header, [(key, string value)])` rows from a target
/// manifest.
///
/// Deliberately narrow: only bare keys with single-line basic-string values
/// are collected, and multi-line (`"""`) string bodies are skipped whole so
/// prose inside `completion_message` cannot be mistaken for a declaration.
/// Everything else — arrays, integers, booleans — is ignored, because the
/// only declarations this build script owns are string-valued. A key that
/// follows a sub-table header (`[[files.checksums]]`) belongs to that
/// sub-table, exactly as TOML defines it.
fn scan_manifest_tables(manifest: &str) -> Vec<(String, Vec<(String, String)>)> {
    let mut tables: Vec<(String, Vec<(String, String)>)> = vec![(String::new(), Vec::new())];
    let mut in_multiline_string = false;
    for line in manifest.lines() {
        if in_multiline_string {
            if line.contains("\"\"\"") {
                in_multiline_string = false;
            }
            continue;
        }
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        if let Some(header) = trimmed
            .strip_prefix("[[")
            .and_then(|rest| rest.strip_suffix("]]"))
        {
            tables.push((header.trim().to_string(), Vec::new()));
            continue;
        }
        if let Some(header) = trimmed
            .strip_prefix('[')
            .and_then(|rest| rest.strip_suffix(']'))
        {
            tables.push((header.trim().to_string(), Vec::new()));
            continue;
        }
        let Some((key, value)) = trimmed.split_once('=') else {
            continue;
        };
        let key = key.trim();
        let value = value.trim();
        if key.is_empty()
            || !key
                .chars()
                .all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '-')
        {
            continue;
        }
        if let Some(rest) = value.strip_prefix("\"\"\"") {
            if !rest.contains("\"\"\"") {
                in_multiline_string = true;
            }
            continue;
        }
        let Some(rest) = value.strip_prefix('"') else {
            continue;
        };
        let Some(end) = rest.find('"') else {
            continue;
        };
        if let Some((_, row)) = tables.last_mut() {
            row.push((key.to_string(), rest[..end].to_string()));
        }
    }
    tables
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
    let readme_const = generated_readme_const_name(&target.name);
    let readme_path = include_path(manifest_dir, &target.readme_path);
    out.push_str(&format!(
        "const {manifest_const}: &str = include_str!(\"{manifest_path}\");\n"
    ));
    out.push_str(&format!(
        "const {readme_const}: &str = include_str!(\"{readme_path}\");\n"
    ));
    for template in &target.templates {
        let include_path = include_path(manifest_dir, &template.source_path);
        out.push_str(&format!(
            "const {}: &str = include_str!(\"{}\");\n",
            template.const_name, include_path
        ));
    }
    // A borrowed asset is embedded once, by the target that owns the bytes.
    // Every borrower's bundle names that same constant.
    for asset in target
        .assets
        .iter()
        .filter(|asset| asset.owner == target.name)
    {
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
            "    BuiltinTarget {{ name: \"{}\", manifest: {}, readme: {}, templates: {}, assets: {} }},\n",
            target.name,
            generated_manifest_const_name(&target.name),
            generated_readme_const_name(&target.name),
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

fn generated_readme_const_name(target: &str) -> String {
    format!("{}_TARGET_README", screaming_identifier(target))
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
