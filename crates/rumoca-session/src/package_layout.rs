use anyhow::{Result, bail};
use rumoca_ir_ast::StoredDefinition;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs;
use std::path::{Path, PathBuf};

pub(crate) fn validate_library_package_layout(
    path: &Path,
    docs: &[(String, StoredDefinition)],
) -> Result<()> {
    let roots = discover_package_roots(path)?;
    if roots.is_empty() {
        return Ok(());
    }

    let mut docs_by_path: HashMap<PathBuf, &StoredDefinition> = HashMap::new();
    for (uri, definition) in docs {
        docs_by_path.insert(PathBuf::from(uri), definition);
    }

    let mut violations = Vec::new();
    for root in roots {
        validate_package_root(&root, &docs_by_path, &mut violations)?;
    }

    if violations.is_empty() {
        return Ok(());
    }

    let message = violations
        .into_iter()
        .map(|violation| format!("- {violation}"))
        .collect::<Vec<_>>()
        .join("\n");
    bail!(
        "invalid Modelica package layout under '{}':\n{}",
        path.display(),
        message
    );
}

fn validate_package_root(
    root: &Path,
    docs_by_path: &HashMap<PathBuf, &StoredDefinition>,
    violations: &mut Vec<String>,
) -> Result<()> {
    let root_name = root_package_name(root, docs_by_path, violations)?;
    let mut dirs = Vec::new();
    collect_dirs_recursive(root, &mut dirs)?;

    for dir in dirs {
        validate_directory(root, &root_name, &dir, docs_by_path, violations)?;
    }

    Ok(())
}

fn root_package_name(
    root: &Path,
    docs_by_path: &HashMap<PathBuf, &StoredDefinition>,
    violations: &mut Vec<String>,
) -> Result<String> {
    let package_path = root.join("package.mo");
    let Some(definition) = docs_by_path.get(&package_path) else {
        violations.push(format!(
            "PKG-006 directory '{}' is missing package.mo",
            root.display()
        ));
        return Ok(root
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("Root")
            .to_string());
    };

    let mut package_names: Vec<String> = definition
        .classes
        .iter()
        .filter(|(_, class)| class.class_type == rumoca_ir_ast::ClassType::Package)
        .map(|(name, _)| name.clone())
        .collect();
    package_names.sort();
    package_names.dedup();

    if package_names.len() == 1 {
        return Ok(package_names.remove(0));
    }

    let mut top_level_names: Vec<String> = definition.classes.keys().cloned().collect();
    top_level_names.sort();
    top_level_names.dedup();
    if top_level_names.len() == 1 {
        return Ok(top_level_names.remove(0));
    }

    bail!(
        "ambiguous package root '{}': package.mo must declare exactly one top-level root",
        package_path.display()
    )
}

fn validate_directory(
    root: &Path,
    root_name: &str,
    dir: &Path,
    docs_by_path: &HashMap<PathBuf, &StoredDefinition>,
    violations: &mut Vec<String>,
) -> Result<()> {
    let mut entries: Vec<_> = fs::read_dir(dir)?.collect::<std::io::Result<Vec<_>>>()?;
    entries.sort_by_key(|entry| entry.path());

    let package_path = dir.join("package.mo");
    let has_package_file = package_path.is_file();

    let mut child_dirs = Vec::new();
    let mut child_files = Vec::new();
    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            child_dirs.push(path);
            continue;
        }
        if path.extension().and_then(|ext| ext.to_str()) == Some("mo")
            && path.file_name().and_then(|name| name.to_str()) != Some("package.mo")
        {
            child_files.push(path);
        }
    }

    let has_subentities = !child_dirs.is_empty() || !child_files.is_empty();
    if has_subentities && !has_package_file {
        violations.push(format!(
            "PKG-006 directory '{}' is missing package.mo",
            dir.display()
        ));
    }

    let child_dir_names: BTreeSet<String> = child_dirs
        .iter()
        .filter_map(|path| {
            path.file_name()
                .and_then(|name| name.to_str())
                .map(str::to_string)
        })
        .collect();
    let child_file_names: BTreeSet<String> = child_files
        .iter()
        .filter_map(|path| {
            path.file_stem()
                .and_then(|name| name.to_str())
                .map(str::to_string)
        })
        .collect();

    for conflict in child_dir_names.intersection(&child_file_names) {
        violations.push(format!(
            "PKG-008 directory '{}' contains both subdirectory '{}' and file '{}.mo'",
            dir.display(),
            conflict,
            conflict
        ));
    }

    let mut owners_by_name: BTreeMap<String, String> = BTreeMap::new();
    for child in &child_files {
        let entity = child
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("<unknown>")
            .to_string();
        for top_level_name in top_level_names(child, docs_by_path)? {
            record_child_name(&top_level_name, &entity, &mut owners_by_name, violations);
        }
        validate_within_clause(root, root_name, child, docs_by_path, violations);
    }

    for child in &child_dirs {
        let entity = format!("{}/package.mo", child.display());
        let names = if child.join("package.mo").is_file() {
            top_level_names(&child.join("package.mo"), docs_by_path)?
        } else {
            vec![
                child
                    .file_name()
                    .and_then(|name| name.to_str())
                    .unwrap_or("<unknown>")
                    .to_string(),
            ]
        };
        for top_level_name in names {
            record_child_name(&top_level_name, &entity, &mut owners_by_name, violations);
        }
        if child.join("package.mo").is_file() {
            validate_within_clause(
                root,
                root_name,
                &child.join("package.mo"),
                docs_by_path,
                violations,
            );
        }
    }

    Ok(())
}

fn validate_within_clause(
    root: &Path,
    root_name: &str,
    file_path: &Path,
    docs_by_path: &HashMap<PathBuf, &StoredDefinition>,
    violations: &mut Vec<String>,
) {
    let Some(definition) = docs_by_path.get(file_path) else {
        return;
    };

    let expected = expected_within_clause(root, root_name, file_path);
    let actual = definition.within.as_ref().map(ToString::to_string);

    match (expected, actual) {
        (None, _) => {}
        (Some(expected), None) => violations.push(format!(
            "PKG-009 file '{}' is missing required within-clause `within {};`",
            file_path.display(),
            expected
        )),
        (Some(expected), Some(actual)) if actual != expected => violations.push(format!(
            "PKG-010 file '{}' has `within {};` but expected `within {};`",
            file_path.display(),
            actual,
            expected
        )),
        _ => {}
    }
}

fn expected_within_clause(root: &Path, root_name: &str, file_path: &Path) -> Option<String> {
    let parent = file_path.parent()?;
    if file_path.file_name().and_then(|name| name.to_str()) == Some("package.mo") {
        if parent == root {
            return None;
        }
        let enclosing_dir = parent.parent()?;
        return package_path_for_dir(root, root_name, enclosing_dir);
    }
    package_path_for_dir(root, root_name, parent)
}

fn package_path_for_dir(root: &Path, root_name: &str, dir: &Path) -> Option<String> {
    if dir == root {
        return Some(root_name.to_string());
    }

    let rel = dir.strip_prefix(root).ok()?;
    let mut parts = vec![root_name.to_string()];
    for component in rel.components() {
        parts.push(component.as_os_str().to_string_lossy().to_string());
    }
    Some(parts.join("."))
}

fn record_child_name(
    name: &str,
    owner: &str,
    owners_by_name: &mut BTreeMap<String, String>,
    violations: &mut Vec<String>,
) {
    if let Some(previous_owner) = owners_by_name.get(name) {
        violations.push(format!(
            "PKG-007 duplicate class name '{}' defined by sibling entities '{}' and '{}'",
            name, previous_owner, owner
        ));
        return;
    }
    owners_by_name.insert(name.to_string(), owner.to_string());
}

fn top_level_names(
    file_path: &Path,
    docs_by_path: &HashMap<PathBuf, &StoredDefinition>,
) -> Result<Vec<String>> {
    let Some(definition) = docs_by_path.get(file_path) else {
        bail!("missing parsed definition for '{}'", file_path.display());
    };
    let mut names: Vec<String> = definition.classes.keys().cloned().collect();
    names.sort();
    names.dedup();
    Ok(names)
}

fn collect_dirs_recursive(root: &Path, out: &mut Vec<PathBuf>) -> Result<()> {
    out.push(root.to_path_buf());
    let mut entries: Vec<_> = fs::read_dir(root)?.collect::<std::io::Result<Vec<_>>>()?;
    entries.sort_by_key(|entry| entry.path());
    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            collect_dirs_recursive(&path, out)?;
        }
    }
    Ok(())
}

fn direct_subdirs(path: &Path) -> Result<Vec<PathBuf>> {
    let mut dirs: Vec<_> = fs::read_dir(path)?
        .collect::<std::io::Result<Vec<_>>>()?
        .into_iter()
        .map(|entry| entry.path())
        .filter(|entry| entry.is_dir())
        .collect();
    dirs.sort();
    Ok(dirs)
}

fn collect_package_dirs_recursive(path: &Path, out: &mut Vec<PathBuf>) -> Result<()> {
    if path.join("package.mo").is_file() {
        out.push(path.to_path_buf());
    }
    for dir in direct_subdirs(path)? {
        collect_package_dirs_recursive(&dir, out)?;
    }
    Ok(())
}

fn discover_package_roots(path: &Path) -> Result<Vec<PathBuf>> {
    if path.is_file() {
        return Ok(Vec::new());
    }

    let mut package_dirs = Vec::new();
    collect_package_dirs_recursive(path, &mut package_dirs)?;
    package_dirs.sort();
    package_dirs.dedup();

    let mut roots = Vec::new();
    for dir in package_dirs {
        if roots.iter().any(|root: &PathBuf| dir.starts_with(root)) {
            continue;
        }
        roots.push(dir);
    }
    Ok(roots)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_docs(files: &[(&Path, &str)]) -> Vec<(String, StoredDefinition)> {
        files
            .iter()
            .map(|(path, source)| {
                (
                    path.display().to_string(),
                    rumoca_phase_parse::parse_to_ast(source, &path.display().to_string())
                        .expect("parse test document"),
                )
            })
            .collect()
    }

    #[test]
    fn package_layout_requires_within_for_child_file() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path().join("Pkg");
        fs::create_dir_all(&root).expect("mkdir");
        let package_mo = root.join("package.mo");
        let child_mo = root.join("A.mo");
        fs::write(&package_mo, "package Pkg end Pkg;").expect("write package");
        fs::write(&child_mo, "model A end A;").expect("write child");

        let docs = parse_docs(&[
            (&package_mo, "package Pkg end Pkg;"),
            (&child_mo, "model A end A;"),
        ]);

        let err =
            validate_library_package_layout(&root, &docs).expect_err("missing within must fail");
        assert!(err.to_string().contains("PKG-009"));
    }

    #[test]
    fn package_layout_accepts_correct_within_for_child_file() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path().join("Pkg");
        fs::create_dir_all(&root).expect("mkdir");
        let package_mo = root.join("package.mo");
        let child_mo = root.join("A.mo");
        fs::write(&package_mo, "package Pkg end Pkg;").expect("write package");
        fs::write(&child_mo, "within Pkg; model A end A;").expect("write child");

        let docs = parse_docs(&[
            (&package_mo, "package Pkg end Pkg;"),
            (&child_mo, "within Pkg; model A end A;"),
        ]);

        validate_library_package_layout(&root, &docs).expect("valid within should pass");
    }

    #[test]
    fn package_layout_discovers_deep_package_roots_without_heuristics() {
        let temp = tempfile::tempdir().expect("tempdir");
        let library_root = temp.path().join("workspace");
        let root = library_root.join("nested/vendor/Pkg");
        fs::create_dir_all(&root).expect("mkdir");
        let package_mo = root.join("package.mo");
        let child_mo = root.join("A.mo");
        fs::write(&package_mo, "package Pkg end Pkg;").expect("write package");
        fs::write(&child_mo, "within Pkg; model A end A;").expect("write child");

        let docs = parse_docs(&[
            (&package_mo, "package Pkg end Pkg;"),
            (&child_mo, "within Pkg; model A end A;"),
        ]);

        validate_library_package_layout(&library_root, &docs)
            .expect("deep package root should be discovered");
    }
}
