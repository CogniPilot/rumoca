use std::fs;
use std::path::Path;
use std::path::PathBuf;

fn is_identifier_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

fn sanitize_root(name: &str) -> Option<String> {
    let root = name.trim_matches(|c: char| !c.is_ascii_alphanumeric() && c != '_');
    if root.is_empty() {
        return None;
    }
    Some(root.to_string())
}

/// Returns true if `needle` appears in `source` as an identifier token.
pub fn source_contains_identifier(source: &str, needle: &str) -> bool {
    if needle.is_empty() {
        return false;
    }
    let source_bytes = source.as_bytes();
    let needle_len = needle.len();
    let mut start = 0;
    while let Some(found) = source[start..].find(needle) {
        let idx = start + found;
        let left_ok = idx == 0 || !is_identifier_char(source_bytes[idx - 1]);
        let right_idx = idx + needle_len;
        let right_ok =
            right_idx >= source_bytes.len() || !is_identifier_char(source_bytes[right_idx]);
        if left_ok && right_ok {
            return true;
        }
        start = idx + 1;
    }
    false
}

/// Extract likely root class/package names declared at top-level in a file.
pub fn extract_declared_roots(source: &str) -> Vec<String> {
    let mut roots = Vec::new();
    for line in source.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") {
            continue;
        }
        let mut iter = trimmed.split_whitespace().peekable();
        while matches!(
            iter.peek().copied(),
            Some("encapsulated" | "partial" | "final" | "redeclare")
        ) {
            iter.next();
        }

        // operator record Foo
        if matches!(iter.peek().copied(), Some("operator")) {
            iter.next();
            if !matches!(iter.peek().copied(), Some("record")) {
                continue;
            }
            iter.next();
            let Some(name) = iter.next() else {
                continue;
            };
            if let Some(root) = sanitize_root(name) {
                roots.push(root);
            }
            continue;
        }

        if matches!(
            iter.peek().copied(),
            Some(
                "package"
                    | "model"
                    | "block"
                    | "class"
                    | "record"
                    | "connector"
                    | "function"
                    | "type"
            )
        ) {
            iter.next();
            let Some(name) = iter.next() else {
                continue;
            };
            if let Some(root) = sanitize_root(name) {
                roots.push(root);
            }
        }
    }
    roots.sort();
    roots.dedup();
    roots
}

fn extract_top_level_roots_from_file(path: &Path) -> std::io::Result<Vec<String>> {
    let source = fs::read_to_string(path)?;
    let file_name = path.to_string_lossy().to_string();
    let mut roots = match rumoca_phase_parse::parse_to_ast(&source, &file_name) {
        Ok(def) => def.classes.keys().cloned().collect::<Vec<_>>(),
        Err(_) => extract_declared_roots(&source),
    };
    roots.sort();
    roots.dedup();
    Ok(roots)
}

fn collect_nested_package_roots(level1: &[PathBuf]) -> std::io::Result<Vec<String>> {
    let mut roots = Vec::new();
    for dir in level1 {
        let mut level2: Vec<_> = fs::read_dir(dir)?
            .collect::<std::io::Result<Vec<_>>>()?
            .into_iter()
            .map(|entry| entry.path())
            .filter(|entry| entry.is_dir())
            .collect();
        level2.sort();
        for subdir in level2 {
            let pkg = subdir.join("package.mo");
            if !pkg.is_file() {
                continue;
            }
            roots.extend(extract_top_level_roots_from_file(&pkg)?);
        }
    }
    Ok(roots)
}

/// Infer root package/class names for a library path.
///
/// If inference fails to determine roots (e.g. directory without package.mo), returns
/// an empty list so callers can conservatively load.
pub fn infer_library_roots(path: &Path) -> std::io::Result<Vec<String>> {
    if path.is_file() {
        return extract_top_level_roots_from_file(path);
    }

    if path.is_dir() {
        let package_file = path.join("package.mo");
        if package_file.is_file() {
            return extract_top_level_roots_from_file(&package_file);
        }

        // Support wrapped library layouts (e.g., ModelicaStandardLibrary_vX.Y.Z/Modelica X.Y.Z/package.mo)
        // by searching a shallow depth for nested package.mo files.
        let mut roots = Vec::new();
        let mut level1: Vec<_> = fs::read_dir(path)?
            .collect::<std::io::Result<Vec<_>>>()?
            .into_iter()
            .map(|entry| entry.path())
            .filter(|entry| entry.is_dir())
            .collect();
        level1.sort();

        for dir in &level1 {
            let pkg = dir.join("package.mo");
            if pkg.is_file() {
                roots.extend(extract_top_level_roots_from_file(&pkg)?);
            }
        }

        if roots.is_empty() {
            roots.extend(collect_nested_package_roots(&level1)?);
        }

        roots.sort();
        roots.dedup();
        return Ok(roots);
    }

    Err(std::io::Error::new(
        std::io::ErrorKind::NotFound,
        "library path does not exist",
    ))
}

/// Decide whether a library path should be loaded for this source.
///
/// Returns true when:
/// - root inference fails (conservative), or
/// - any inferred root package/class appears as an identifier token in source.
pub fn should_load_library_for_source(source: &str, path: &Path) -> std::io::Result<bool> {
    let roots = infer_library_roots(path)?;
    if roots.is_empty() {
        return Ok(true);
    }
    Ok(roots
        .iter()
        .any(|root| source_contains_identifier(source, root)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_contains_identifier_honors_boundaries() {
        assert!(source_contains_identifier(
            "import Modelica.Blocks;",
            "Modelica"
        ));
        assert!(!source_contains_identifier(
            "import SuperModelica.Blocks;",
            "Modelica"
        ));
    }

    #[test]
    fn extract_declared_roots_finds_package_and_operator_record() {
        let source = r#"
package Modelica
end Modelica;
operator record SE2
end SE2;
"#;
        let roots = extract_declared_roots(source);
        assert!(roots.contains(&"Modelica".to_string()));
        assert!(roots.contains(&"SE2".to_string()));
    }

    #[test]
    fn infer_library_roots_supports_wrapped_layout() {
        let temp = tempfile::tempdir().expect("tempdir");
        let wrapped = temp.path().join("ModelicaStandardLibrary_v4.1.0");
        let nested = wrapped.join("Modelica 4.1.0");
        std::fs::create_dir_all(&nested).expect("mkdir");
        std::fs::write(nested.join("package.mo"), "package Modelica\nend Modelica;")
            .expect("write package.mo");

        let roots = infer_library_roots(&wrapped).expect("infer roots");
        assert_eq!(roots, vec!["Modelica".to_string()]);
    }
}
