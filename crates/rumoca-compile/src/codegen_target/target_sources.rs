use super::*;

#[derive(Debug, Clone)]
pub(super) struct TargetAssetFile {
    pub(super) relative_path: String,
    pub(super) bytes: Vec<u8>,
}

/// The files a `shared_from` bundle borrows, read from the owning built-in
/// target's embedded bundle.
pub(super) fn borrowed_asset_files(owner: &str, source: &str) -> Result<Vec<TargetAssetFile>> {
    let target = rumoca_phase_codegen::templates::builtin_target(owner).with_context(|| {
        format!("[[assets]] source '{source}' borrows from unknown target '{owner}'")
    })?;
    let files = target
        .asset_files(source)
        .with_context(|| format!("Target '{owner}' lends no asset source '{source}' to borrow"))?;
    Ok(files
        .into_iter()
        .map(|(relative_path, bytes)| TargetAssetFile {
            relative_path: relative_path.to_owned(),
            bytes: bytes.to_vec(),
        })
        .collect())
}

/// Freeze one asset source into canonical lexical member order while refusing
/// duplicate logical paths. Built-in registry order is presentation data and
/// cannot become package order authority.
pub(super) fn close_asset_member_order(
    mut files: Vec<TargetAssetFile>,
) -> Result<Vec<TargetAssetFile>> {
    files.sort_by(|left, right| left.relative_path.cmp(&right.relative_path));
    let mut paths = BTreeSet::new();
    for file in &files {
        if !paths.insert(file.relative_path.as_str()) {
            bail!(
                "target asset source contains duplicate member path '{}'",
                file.relative_path
            );
        }
    }
    Ok(files)
}

pub(super) fn collect_target_assets(root: &Path) -> Result<Vec<TargetAssetFile>> {
    let root_metadata = fs::symlink_metadata(root)
        .with_context(|| format!("Inspect target asset source '{}'", root.display()))?;
    if root_metadata.file_type().is_symlink() || !root_metadata.is_dir() {
        bail!(
            "Target asset source '{}' must be a non-symlink directory",
            root.display()
        );
    }
    let mut paths = Vec::new();
    collect_target_asset_paths(root, root, &mut paths)?;
    paths.sort_by(|left, right| left.0.cmp(&right.0));
    paths
        .into_iter()
        .map(|(relative_path, path)| {
            Ok(TargetAssetFile {
                relative_path,
                bytes: read_regular_file_bounded(&path)
                    .with_context(|| format!("Read target asset '{}'", path.display()))?,
            })
        })
        .collect()
}

pub(super) fn collect_target_asset_paths(
    root: &Path,
    dir: &Path,
    out: &mut Vec<(String, PathBuf)>,
) -> Result<()> {
    for entry in std::fs::read_dir(dir)
        .with_context(|| format!("Read target asset directory '{}'", dir.display()))?
    {
        let entry =
            entry.with_context(|| format!("Read target asset entry in '{}'", dir.display()))?;
        let file_type = entry
            .file_type()
            .with_context(|| format!("Stat target asset '{}'", entry.path().display()))?;
        if file_type.is_symlink() {
            bail!(
                "Target asset source may not contain symlinks: '{}'",
                entry.path().display()
            );
        }
        if file_type.is_dir() {
            collect_target_asset_paths(root, &entry.path(), out)?;
        } else if file_type.is_file() {
            let relative_path = target_asset_relative_path(root, &entry.path())?;
            out.push((relative_path, entry.path()));
        }
    }
    Ok(())
}

pub(super) fn target_asset_relative_path(root: &Path, path: &Path) -> Result<String> {
    let relative = path.strip_prefix(root).with_context(|| {
        format!(
            "Target asset '{}' is not beneath source root '{}'",
            path.display(),
            root.display()
        )
    })?;
    let mut parts = Vec::new();
    for component in relative.components() {
        let Component::Normal(part) = component else {
            bail!(
                "Target asset path must contain only normal components: '{}'",
                path.display()
            );
        };
        let part = part
            .to_str()
            .with_context(|| format!("Target asset path must be UTF-8: '{}'", path.display()))?;
        parts.push(part);
    }
    if parts.is_empty() {
        bail!(
            "Target asset path must identify a file beneath source root: '{}'",
            path.display()
        );
    }
    Ok(parts.join("/"))
}

impl TargetTemplateSource for TargetBundle {
    fn template_source<'a>(&'a self, template: &str) -> Result<Cow<'a, str>> {
        match &self.source {
            TargetBundleSource::Builtin { target } => target
                .template_source(template)
                .map(Cow::Borrowed)
                .ok_or_else(|| {
                    anyhow::anyhow!("Built-in target references unknown template '{template}'")
                }),
            TargetBundleSource::Directory { dir, .. } => {
                let path = safe_target_join(dir, template)?;
                read_utf8_regular_file_bounded(&path)
                    .map(Cow::Owned)
                    .with_context(|| format!("Read target template {}", path.display()))
            }
            TargetBundleSource::InMemory { templates, .. } => templates
                .get(template)
                .map(|source| Cow::Borrowed(source.as_str()))
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "In-memory target references missing exact template '{template}'"
                    )
                }),
        }
    }
}

pub(super) trait TargetTemplateSource {
    fn template_source<'a>(&'a self, template: &str) -> Result<Cow<'a, str>>;
}

pub(super) fn target_template_source_for<'a>(
    source: &'a impl TargetTemplateSource,
    file: &TargetDeclaredFileSpec,
) -> Result<Cow<'a, str>> {
    let Some(owner) = file.template_shared_from.as_deref() else {
        return source.template_source(&file.template);
    };
    borrowed_template_source(owner, &file.template).map(Cow::Borrowed)
}

pub(super) fn borrowed_template_source(owner: &str, template: &str) -> Result<&'static str> {
    let target = templates::builtin_target(owner).with_context(|| {
        format!("template '{template}' borrows from unknown built-in target '{owner}'")
    })?;
    target
        .template_source(template)
        .with_context(|| format!("target '{owner}' owns no template '{template}' to lend"))
}

impl TargetTemplateSource for BTreeMap<String, String> {
    fn template_source<'a>(&'a self, template: &str) -> Result<Cow<'a, str>> {
        self.get(template)
            .map(|source| Cow::Borrowed(source.as_str()))
            .ok_or_else(|| anyhow::anyhow!("Target template not found: {template}"))
    }
}
