use std::fs;
use std::path::{Component, Path, PathBuf};

use anyhow::{Context, Result};
use indexmap::{IndexMap, IndexSet};
use serde::{Deserialize, Serialize};

pub const WORKSPACE_CONFIG_FILE_NAME: &str = "rumoca-workspace.toml";

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct WorkspaceConfigFile {
    pub source_roots: Vec<String>,
    pub replace_source_roots: bool,
    pub source_root_scopes: IndexMap<String, WorkspaceSourceRootScope>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct WorkspaceSourceRootScope {
    pub source_roots: Vec<String>,
    pub replace_source_roots: bool,
}

#[derive(Debug, Clone, Default)]
pub struct WorkspaceConfig {
    files: Vec<ResolvedWorkspaceConfigFile>,
}

#[derive(Debug, Clone)]
struct ResolvedWorkspaceConfigFile {
    base_dir: PathBuf,
    data: WorkspaceConfigFile,
}

impl WorkspaceConfig {
    pub fn discover(workspace_root: &Path, focus_path: &Path) -> Result<Option<Self>> {
        let config = Self::load(workspace_root, focus_path)?;
        if config.files.is_empty() {
            Ok(None)
        } else {
            Ok(Some(config))
        }
    }

    pub fn load(workspace_root: &Path, focus_path: &Path) -> Result<Self> {
        let mut files = Vec::new();
        for path in
            collect_workspace_config_paths_with(workspace_root, focus_path, &|path| path.is_file())
        {
            let text =
                fs::read_to_string(&path).with_context(|| format!("read {}", path.display()))?;
            let data = toml::from_str::<WorkspaceConfigFile>(&text)
                .with_context(|| format!("parse {}", path.display()))?;
            let base_dir = path.parent().unwrap_or(workspace_root).to_path_buf();
            files.push(ResolvedWorkspaceConfigFile { base_dir, data });
        }
        Ok(Self { files })
    }

    pub fn load_from_files<I>(
        workspace_root: &Path,
        focus_path: &Path,
        input_files: I,
    ) -> Result<Self>
    where
        I: IntoIterator<Item = (PathBuf, String)>,
    {
        let mut input_by_path = IndexMap::new();
        for (path, text) in input_files {
            if !path
                .file_name()
                .and_then(|name| name.to_str())
                .is_some_and(is_workspace_config_filename)
            {
                continue;
            }
            let resolved = resolve_input_path(workspace_root, &path);
            input_by_path.insert(normalized_path_key(&resolved), (resolved, text));
        }

        let mut files = Vec::new();
        for path in collect_workspace_config_paths_with(workspace_root, focus_path, &|path| {
            input_by_path.contains_key(&normalized_path_key(path))
        }) {
            let Some((resolved, text)) = input_by_path.get(&normalized_path_key(&path)) else {
                continue;
            };
            let data = toml::from_str::<WorkspaceConfigFile>(text)
                .with_context(|| format!("parse {}", resolved.display()))?;
            let base_dir = resolved.parent().unwrap_or(workspace_root).to_path_buf();
            files.push(ResolvedWorkspaceConfigFile { base_dir, data });
        }
        Ok(Self { files })
    }

    pub fn effective_source_roots_for(&self, focus_path: &Path) -> Vec<String> {
        let mut seen = IndexSet::new();
        let mut roots = Vec::new();
        for file in &self.files {
            if file.data.replace_source_roots {
                seen.clear();
                roots.clear();
            }
            push_roots(
                &file.base_dir,
                &file.data.source_roots,
                &mut seen,
                &mut roots,
            );
            self.push_matching_scope_roots(file, focus_path, &mut seen, &mut roots);
        }
        roots
    }

    fn push_matching_scope_roots(
        &self,
        file: &ResolvedWorkspaceConfigFile,
        focus_path: &Path,
        seen: &mut IndexSet<String>,
        roots: &mut Vec<String>,
    ) {
        for (scope_path, scope) in &file.data.source_root_scopes {
            if !focus_is_inside_scope(&file.base_dir, scope_path, focus_path) {
                continue;
            }
            if scope.replace_source_roots {
                seen.clear();
                roots.clear();
            }
            push_roots(&file.base_dir, &scope.source_roots, seen, roots);
        }
    }
}

pub fn is_workspace_config_filename(name: &str) -> bool {
    name == WORKSPACE_CONFIG_FILE_NAME
}

pub fn collect_workspace_config_paths(workspace_root: &Path, focus_path: &Path) -> Vec<PathBuf> {
    collect_workspace_config_paths_with(workspace_root, focus_path, &|path| path.is_file())
}

fn collect_workspace_config_paths_with<F>(
    workspace_root: &Path,
    focus_path: &Path,
    is_present: &F,
) -> Vec<PathBuf>
where
    F: Fn(&Path) -> bool,
{
    let focus_dir = if focus_path.is_dir() {
        focus_path
    } else {
        focus_path.parent().unwrap_or(workspace_root)
    };
    let Ok(relative_focus_dir) = focus_dir.strip_prefix(workspace_root) else {
        return workspace_config_path_if_present(workspace_root, is_present)
            .into_iter()
            .collect();
    };

    let mut paths = Vec::new();
    push_if_present(&mut paths, workspace_root, is_present);
    let mut current = workspace_root.to_path_buf();
    for component in relative_focus_dir.components() {
        current.push(component.as_os_str());
        push_if_present(&mut paths, &current, is_present);
    }
    paths
}

fn workspace_config_path_if_present<F>(dir: &Path, is_present: &F) -> Option<PathBuf>
where
    F: Fn(&Path) -> bool,
{
    let path = dir.join(WORKSPACE_CONFIG_FILE_NAME);
    is_present(&path).then_some(path)
}

fn push_if_present<F>(paths: &mut Vec<PathBuf>, dir: &Path, is_present: &F)
where
    F: Fn(&Path) -> bool,
{
    if let Some(path) = workspace_config_path_if_present(dir, is_present) {
        paths.push(path);
    }
}

fn push_roots(
    base_dir: &Path,
    raw_roots: &[String],
    seen: &mut IndexSet<String>,
    roots: &mut Vec<String>,
) {
    for raw_root in raw_roots {
        let raw_root = raw_root.trim();
        if raw_root.is_empty() {
            continue;
        }
        let resolved = resolve_config_path(base_dir, raw_root);
        let key = canonical_display_path(&resolved);
        if seen.insert(key.clone()) {
            roots.push(key);
        }
    }
}

fn focus_is_inside_scope(base_dir: &Path, raw_scope_path: &str, focus_path: &Path) -> bool {
    let scope_path = resolve_config_path(base_dir, raw_scope_path);
    let focus_dir = if focus_path.is_dir() {
        focus_path
    } else {
        focus_path.parent().unwrap_or(focus_path)
    };
    focus_dir == scope_path || focus_dir.starts_with(&scope_path)
}

fn resolve_config_path(base_dir: &Path, raw: &str) -> PathBuf {
    let path = Path::new(raw);
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        base_dir.join(path)
    }
}

fn resolve_input_path(workspace_root: &Path, path: &Path) -> PathBuf {
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        workspace_root.join(path)
    }
}

fn canonical_display_path(path: &Path) -> String {
    path.canonicalize()
        .unwrap_or_else(|_| normalize_path_lexically(path))
        .to_string_lossy()
        .to_string()
}

fn normalized_path_key(path: &Path) -> String {
    normalize_path_lexically(path).to_string_lossy().to_string()
}

fn normalize_path_lexically(path: &Path) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                normalized.pop();
            }
            Component::Prefix(prefix) => normalized.push(prefix.as_os_str()),
            Component::RootDir => normalized.push(component.as_os_str()),
            Component::Normal(value) => normalized.push(value),
        }
    }
    normalized
}

#[cfg(test)]
mod tests;
