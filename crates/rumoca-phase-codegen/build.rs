//! Build-time discovery of the built-in code-gen target bundles.
//!
//! # Shared render-environment templates (support partials and shared bases)
//!
//! A target directory holds two kinds of `.jinja` file, and this build script
//! is the authority that classifies every one of them:
//!
//! * an **artifact template**, declared by a `[[files]]` entry, whose render
//!   produces exactly one product file; and
//! * a **support partial**, declared by a `[[partials]]` entry, which produces
//!   no product file at all and exists only to be `import`ed, `include`d, or
//!   `extends`ed by artifact templates.
//!
//! Both kinds may additionally be published to the shared render environment
//! under a globally unique name — `[[partials]].name` for a support partial,
//! `[[files]].shared_as` for an artifact template other targets extend. That
//! declaration is what makes the name resolvable from a template; there is no
//! second, Rust-side registration list to keep in step (see
//! `codegen::create_environment`, which registers exactly the names generated
//! here).
//!
//! Every `.jinja` file in a target directory must be declared exactly once,
//! as one kind or the other. An undeclared template, a template declared as
//! both, a partial that also has a `[[files]]` entry, or a duplicated shared
//! name fails the build — not a test — so a target bundle cannot reach the
//! renderer in an ambiguous shape.
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
//! bytes are embedded once however many targets ship them. This is the asset
//! counterpart of `[[files]].shared_as`, and it exists for the same reason:
//! two vendored copies of one upstream tree drift, and a drifted copy is a
//! non-conformant container.

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
    /// Shared render-environment name, when the manifest publishes this
    /// template under one (`[[partials]].name` / `[[files]].shared_as`).
    shared_name: Option<String>,
    role: TemplateRole,
}

/// Which manifest declaration owns a template file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TemplateRole {
    /// Declared by `[[files]]`: renders one product file.
    Artifact,
    /// Declared by `[[partials]]`: renders no product file.
    SupportPartial,
}

impl TemplateRole {
    fn generated_variant(self) -> &'static str {
        match self {
            Self::Artifact => "BuiltinTemplateRole::Artifact",
            Self::SupportPartial => "BuiltinTemplateRole::SupportPartial",
        }
    }
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

/// Target names that are PERMANENTLY RETIRED, with the message resolution
/// reports. A directory reappearing under one of these names fails the build
/// here, so the retirement cannot be undone by re-adding files: removing a
/// name from this list is an explicit reviewed decision, and these
/// identities never return.
///
/// `c-ode` and `embedded-c-galec` are retired because the C export surface is
/// the combined FMI 3 ME+CS product plus the Solve-rendered embedded target,
/// and because GALEC never emits C: all Production/Embedded C renders from
/// the refined Solve product.
const RETIRED_TARGETS: &[(&str, &str)] = &[
    (
        "c-ode",
        "target 'c-ode' is retired: use the 'fmi3' target (FMI 3.0 ME+CS); Model Exchange serves host-owned integration and Co-Simulation serves the built-in solver",
    ),
    (
        "embedded-c-galec",
        "target 'embedded-c-galec' is retired: GALEC never emits C; it is superseded by the Solve-rendered embedded C target",
    ),
];

/// Target names that are SUSPENDED, with the message resolution reports.
/// Distinct from retirement: a suspended product's identity is valid
/// architecture and RETURNS when the checked roots its message names have
/// landed; removal from this list is that re-registration act, reviewed
/// against those roots. A directory reappearing while the name is listed
/// fails the build with the suspension message.
///
/// `galec-production` (the eFMU container) is suspended pending its
/// Solve-rendered Production Code leaf: it returns with two checked per-file
/// roots, the Algorithm Code leaf from GALEC and the Production Code C/H
/// leaf from the refined `SolveAlgorithmBlock`. Only its invalid
/// AC-to-C implementation was deleted.
const SUSPENDED_TARGETS: &[(&str, &str)] = &[(
    "galec-production",
    "target 'galec-production' is suspended pending its Solve-rendered Production Code leaf: the eFMU container returns when both checked per-file roots land (Algorithm Code from GALEC, Production Code C from the refined SolveAlgorithmBlock); the Algorithm Code representation is available today via the 'galec' target",
)];

fn main() -> BuildResult<()> {
    let manifest_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR")?);
    let templates_dir = manifest_dir.join("src/templates");
    println!("cargo:rerun-if-changed={}", templates_dir.display());

    let targets = discover_targets(&templates_dir)?;
    reject_retired_target_dirs(&targets)?;
    reject_algorithm_code_c_templates(&targets)?;
    validate_unique_shared_names(&targets)?;
    let generated = render_generated_templates_module(&manifest_dir, &targets);
    let out_dir = PathBuf::from(std::env::var("OUT_DIR")?);
    fs::write(out_dir.join("templates_generated.rs"), generated)?;
    Ok(())
}

/// Fail the build when a retired or suspended target directory reappears.
/// Both lists are authoritative over the filesystem, so creep-back is a
/// compile error carrying the retirement or suspension message rather than a
/// silently revived product. The two states stay distinct: retired
/// identities never return; a suspended identity returns by removing its
/// list entry once the checked roots its message names have landed.
fn reject_retired_target_dirs(targets: &[TargetDir]) -> BuildResult<()> {
    for target in targets {
        if let Some((_, message)) = RETIRED_TARGETS
            .iter()
            .find(|(name, _)| *name == target.name)
        {
            return Err(build_error(format!(
                "retired target directory reappeared at {}: {message}",
                target.manifest_path.display()
            ))
            .into());
        }
        if let Some((_, message)) = SUSPENDED_TARGETS
            .iter()
            .find(|(name, _)| *name == target.name)
        {
            return Err(build_error(format!(
                "suspended target directory reappeared at {}: {message}",
                target.manifest_path.display()
            ))
            .into());
        }
    }
    Ok(())
}

/// Fail the build when any Algorithm Code target bundles a C/H template or
/// declares a C/H product file. GALEC never emits C: every Production or
/// Embedded C artifact renders from the refined Solve product.
///
/// This line/extension scan is a CREEP-BACK TRIPWIRE, not the construction
/// proof: the authoritative gate is the mandatory closed per-file
/// kind/context schema and compatibility table (SPEC_0034 GAL-043).
fn reject_algorithm_code_c_templates(targets: &[TargetDir]) -> BuildResult<()> {
    for target in targets {
        let manifest = fs::read_to_string(&target.manifest_path)?;
        let is_algorithm_code = manifest
            .lines()
            .any(|line| line.trim_start().starts_with("ir") && line.contains("\"algorithm-code\""));
        if !is_algorithm_code {
            continue;
        }
        for template in &target.templates {
            let stem = template.path.trim_end_matches(".jinja");
            if stem.ends_with(".c") || stem.ends_with(".h") {
                return Err(build_error(format!(
                    "Algorithm Code target '{}' bundles C/H template '{}': GALEC never emits C; render C from the refined Solve product instead",
                    target.name, template.path
                ))
                .into());
            }
        }
        for line in manifest.lines() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("path") && (trimmed.contains(".c\"") || trimmed.contains(".h\""))
            {
                return Err(build_error(format!(
                    "Algorithm Code target '{}' declares C/H product '{}': GALEC never emits C; render C from the refined Solve product instead",
                    target.name,
                    trimmed
                ))
                .into());
            }
        }
    }
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
    resolve_borrowed_assets(&mut targets)?;
    Ok(targets)
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
            let (role, shared_name) = declarations.classify(&manifest_path, &path)?;
            templates.push(TemplateFile {
                const_name: generated_template_const_name(&name, &path),
                path,
                source_path,
                shared_name,
                role,
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

/// The template declarations a `target.toml` makes: one row per `[[files]]`
/// entry and one per `[[partials]]` entry.
///
/// The semantic manifest parser lives in `rumoca-compile`
/// (`codegen_target::parse_target_manifest`, which owns the typed
/// `TargetPartial`/`TargetFile::shared_as` shape). This build script only
/// needs the template/name pairs, so it reads them with the narrow scanner
/// below rather than taking a TOML build-dependency; disagreement between the
/// two readers cannot go unnoticed, because a template this scanner fails to
/// see is reported as an undeclared file and fails the build.
#[derive(Debug, Default)]
struct ManifestDeclarations {
    /// `[[files]].template` -> `[[files]].shared_as`.
    files: BTreeMap<String, Option<String>>,
    /// `[[partials]].template` -> `[[partials]].name`.
    partials: BTreeMap<String, String>,
}

impl ManifestDeclarations {
    fn read(manifest_path: &Path) -> BuildResult<Self> {
        let manifest = fs::read_to_string(manifest_path)
            .map_err(|error| build_error(format!("read {}: {error}", manifest_path.display())))?;
        let mut declarations = Self::default();
        for (table, row) in scan_manifest_tables(&manifest) {
            match table.as_str() {
                "files" => declarations.add_file(manifest_path, &row)?,
                "partials" => declarations.add_partial(manifest_path, &row)?,
                _ => {}
            }
        }
        for template in declarations.partials.keys() {
            if declarations.files.contains_key(template) {
                return Err(build_error(format!(
                    "{}: {template} is declared both as a [[files]] artifact and as a \
                     [[partials]] support partial; a support partial renders no product \
                     file, so the two declarations are mutually exclusive",
                    manifest_path.display()
                ))
                .into());
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
        let shared_as = row_value(row, "shared_as").map(ToOwned::to_owned);
        if self.files.insert(template.to_owned(), shared_as).is_some() {
            return Err(build_error(format!(
                "{}: template {template} has more than one [[files]] entry",
                manifest_path.display()
            ))
            .into());
        }
        Ok(())
    }

    fn add_partial(&mut self, manifest_path: &Path, row: &[(String, String)]) -> BuildResult<()> {
        let (Some(template), Some(name)) = (row_value(row, "template"), row_value(row, "name"))
        else {
            return Err(build_error(format!(
                "{}: every [[partials]] entry must declare both `template` (the file in \
                 this target directory) and `name` (the shared render-environment name \
                 templates import it under)",
                manifest_path.display()
            ))
            .into());
        };
        if self
            .partials
            .insert(template.to_owned(), name.to_owned())
            .is_some()
        {
            return Err(build_error(format!(
                "{}: template {template} has more than one [[partials]] entry",
                manifest_path.display()
            ))
            .into());
        }
        Ok(())
    }

    /// Classify one discovered `.jinja` file against the manifest. An
    /// undeclared template fails the build: silently bundling it would leave
    /// the renderer unable to say whether it is an artifact or a partial.
    fn classify(
        &self,
        manifest_path: &Path,
        path: &str,
    ) -> BuildResult<(TemplateRole, Option<String>)> {
        if let Some(shared_as) = self.files.get(path) {
            return Ok((TemplateRole::Artifact, shared_as.clone()));
        }
        if let Some(name) = self.partials.get(path) {
            return Ok((TemplateRole::SupportPartial, Some(name.clone())));
        }
        Err(build_error(format!(
            "{}: template {path} is bundled but undeclared. Declare it as a [[files]] \
             entry if rendering it produces a product file, or as a [[partials]] entry \
             (`template = \"{path}\"`, `name = \"<shared render-environment name>\"`) \
             if it is a support partial that other templates import, include, or extend",
            manifest_path.display()
        ))
        .into())
    }

    fn validate_every_declaration_has_a_file(
        &self,
        manifest_path: &Path,
        templates: &[TemplateFile],
    ) -> BuildResult<()> {
        for declared in self.files.keys().chain(self.partials.keys()) {
            if !templates.iter().any(|template| &template.path == declared) {
                return Err(build_error(format!(
                    "{} references missing template {declared}",
                    manifest_path.display()
                ))
                .into());
            }
        }
        for (template, shared_as) in &self.files {
            if shared_as.as_deref().is_some_and(str::is_empty) {
                return Err(build_error(format!(
                    "{}: [[files]] entry {template} declares an empty shared_as name",
                    manifest_path.display()
                ))
                .into());
            }
        }
        for (template, name) in &self.partials {
            if name.is_empty() {
                return Err(build_error(format!(
                    "{}: [[partials]] entry {template} declares an empty name",
                    manifest_path.display()
                ))
                .into());
            }
        }
        Ok(())
    }
}

/// Every shared render-environment name is global: one `import "name"` in any
/// target's template must resolve to exactly one file. Two targets publishing
/// the same name would make the winner depend on registration order, so the
/// collision fails the build.
fn validate_unique_shared_names(targets: &[TargetDir]) -> BuildResult<()> {
    let mut owners = BTreeMap::<&str, String>::new();
    for target in targets {
        for template in &target.templates {
            let Some(shared_name) = template.shared_name.as_deref() else {
                continue;
            };
            let owner = format!("{}/{}", target.name, template.path);
            if let Some(previous) = owners.insert(shared_name, owner.clone()) {
                return Err(build_error(format!(
                    "shared render-environment name {shared_name} is declared by both \
                     {previous} and {owner}; shared names are global and must be unique"
                ))
                .into());
            }
        }
    }
    Ok(())
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
    render_shared_templates(&mut out, targets);
    render_retired_targets(&mut out);
    out
}

fn render_retired_targets(out: &mut String) {
    out.push_str("\npub const RETIRED_TARGETS: &[RetiredTarget] = &[\n");
    for (name, message) in RETIRED_TARGETS {
        out.push_str(&format!(
            "    RetiredTarget {{ name: \"{name}\", message: \"{message}\" }},\n"
        ));
    }
    out.push_str("];\n");
    out.push_str("\npub const SUSPENDED_TARGETS: &[SuspendedTarget] = &[\n");
    for (name, message) in SUSPENDED_TARGETS {
        out.push_str(&format!(
            "    SuspendedTarget {{ name: \"{name}\", message: \"{message}\" }},\n"
        ));
    }
    out.push_str("];\n");
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
            "    BuiltinTargetTemplate {{ path: \"{}\", source: {}, shared_name: {}, role: {} }},\n",
            template.path,
            template.const_name,
            generated_option_str(template.shared_name.as_deref()),
            template.role.generated_variant()
        ));
    }
    out.push_str("];\n\n");
}

/// The flat, name-sorted registry of every shared render-environment template.
/// `codegen::create_environment` registers exactly this list, so a template's
/// availability under a shared name is decided by the owning target manifest
/// and nowhere else.
fn render_shared_templates(out: &mut String, targets: &[TargetDir]) {
    let mut shared = Vec::new();
    for target in targets {
        for template in &target.templates {
            if let Some(shared_name) = template.shared_name.as_deref() {
                shared.push((shared_name, target.name.as_str(), template));
            }
        }
    }
    shared.sort_by(|lhs, rhs| lhs.0.cmp(rhs.0));
    out.push_str("pub const SHARED_TEMPLATES: &[BuiltinSharedTemplate] = &[\n");
    for (shared_name, target_name, template) in shared {
        out.push_str(&format!(
            "    BuiltinSharedTemplate {{ name: \"{}\", target: \"{}\", path: \"{}\", source: {}, role: {} }},\n",
            shared_name,
            target_name,
            template.path,
            template.const_name,
            template.role.generated_variant()
        ));
    }
    out.push_str("];\n");
}

fn generated_option_str(value: Option<&str>) -> String {
    match value {
        Some(value) => format!("Some(\"{value}\")"),
        None => "None".to_string(),
    }
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
