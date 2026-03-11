use anyhow::{Context, Result, bail};
use rumoca_core::{Diagnostic as CommonDiagnostic, Label, PrimaryLabel, SourceMap, Span};
use rumoca_ir_ast::{ClassDef, Name, StoredDefinition, Token};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs;
use std::path::{Path, PathBuf};
use thiserror::Error;

#[derive(Debug, Clone, Error)]
#[error("invalid Modelica package layout under '{}': {summary}", library_path.display())]
pub struct PackageLayoutError {
    library_path: PathBuf,
    summary: String,
    diagnostics: Vec<CommonDiagnostic>,
    source_map: SourceMap,
}

impl PackageLayoutError {
    fn new(
        library_path: PathBuf,
        diagnostics: Vec<CommonDiagnostic>,
        source_map: SourceMap,
    ) -> Self {
        let summary = diagnostics
            .iter()
            .map(|diagnostic| {
                diagnostic
                    .code
                    .as_ref()
                    .map(|code| format!("{code} {}", diagnostic.message))
                    .unwrap_or_else(|| diagnostic.message.clone())
            })
            .collect::<Vec<_>>()
            .join("; ");
        Self {
            library_path,
            summary,
            diagnostics,
            source_map,
        }
    }

    pub fn diagnostics(&self) -> &[CommonDiagnostic] {
        &self.diagnostics
    }

    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }
}

fn build_package_layout_source_map(docs: &[(String, StoredDefinition)]) -> Result<SourceMap> {
    let mut source_map = SourceMap::new();
    let mut paths: Vec<&str> = docs.iter().map(|(uri, _)| uri.as_str()).collect();
    paths.sort();
    paths.dedup();
    for path in paths {
        let source = fs::read_to_string(path)
            .with_context(|| format!("read package-layout source '{}'", path))?;
        source_map.add(path, &source);
    }
    Ok(source_map)
}

fn location_has_valid_span(location: &rumoca_ir_ast::Location) -> bool {
    !location.file_name.is_empty() && location.end > location.start
}

fn token_span(token: &Token, source_map: Option<&SourceMap>) -> Option<Span> {
    let source_map = source_map?;
    location_has_valid_span(&token.location).then(|| {
        source_map.location_to_span(
            &token.location.file_name,
            token.location.start as usize,
            token.location.end as usize,
        )
    })
}

fn name_span(name: &Name, source_map: Option<&SourceMap>) -> Option<Span> {
    let source_map = source_map?;
    let first = name.name.first()?;
    let last = name.name.last()?;
    if !location_has_valid_span(&first.location) || !location_has_valid_span(&last.location) {
        return None;
    }
    Some(source_map.location_to_span(
        &first.location.file_name,
        first.location.start as usize,
        last.location.end as usize,
    ))
}

fn class_name_span(class: &ClassDef, source_map: Option<&SourceMap>) -> Option<Span> {
    token_span(&class.name, source_map)
}

fn top_level_name_entries(
    file_path: &Path,
    docs_by_path: &HashMap<PathBuf, &StoredDefinition>,
    source_map: Option<&SourceMap>,
) -> Result<Vec<(String, Option<Span>)>> {
    let Some(definition) = docs_by_path.get(file_path) else {
        bail!("missing parsed definition for '{}'", file_path.display());
    };
    let mut entries = definition
        .classes
        .iter()
        .map(|(name, class)| (name.clone(), class_name_span(class, source_map)))
        .collect::<Vec<_>>();
    entries.sort_by(|a, b| a.0.cmp(&b.0));
    entries.dedup_by(|a, b| a.0 == b.0);
    Ok(entries)
}

pub fn collect_compile_unit_source_files(path: &Path) -> Result<Vec<PathBuf>> {
    if !path.is_file() {
        bail!("compile-unit path is not a file: {}", path.display());
    }

    let Some(parent) = path.parent() else {
        bail!(
            "compile-unit file has no parent directory: {}",
            path.display()
        );
    };

    let mut files = Vec::new();
    if let Some(root) = topmost_contiguous_package_root(parent) {
        collect_package_tree_files(&root, &mut files)?;
    } else {
        collect_direct_modelica_files(parent, &mut files)?;
    }
    files.sort();
    files.dedup();
    Ok(files)
}

pub(crate) fn collect_library_source_files(path: &Path) -> Result<Vec<PathBuf>> {
    if path.is_file() {
        return Ok(vec![path.to_path_buf()]);
    }
    if !path.is_dir() {
        bail!("library path does not exist: {}", path.display());
    }

    let roots = discover_package_roots(path)?;
    if roots.is_empty() {
        let mut files = Vec::new();
        collect_all_modelica_files_recursive(path, &mut files)?;
        files.sort();
        files.dedup();
        return Ok(files);
    }

    let mut files = Vec::new();
    collect_direct_modelica_files(path, &mut files)?;
    for root in roots {
        collect_package_tree_files(&root, &mut files)?;
    }
    files.sort();
    files.dedup();
    Ok(files)
}

/// Validate package-directory structure and within-clauses (MLS §13.4.1, §13.4.3).
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
    for root in &roots {
        validate_package_root(root, &docs_by_path, None, &mut violations)?;
    }

    if violations.is_empty() {
        return Ok(());
    }

    let source_map = build_package_layout_source_map(docs)?;
    let mut violations = Vec::new();
    for root in roots {
        validate_package_root(&root, &docs_by_path, Some(&source_map), &mut violations)?;
    }

    Err(PackageLayoutError::new(path.to_path_buf(), violations, source_map).into())
}

fn validate_package_root(
    root: &Path,
    docs_by_path: &HashMap<PathBuf, &StoredDefinition>,
    source_map: Option<&SourceMap>,
    violations: &mut Vec<CommonDiagnostic>,
) -> Result<()> {
    let root_name = root_package_name(root, docs_by_path, source_map, violations)?;
    validate_directory(root, &root_name, root, docs_by_path, source_map, violations)?;
    Ok(())
}

fn root_package_name(
    root: &Path,
    docs_by_path: &HashMap<PathBuf, &StoredDefinition>,
    source_map: Option<&SourceMap>,
    violations: &mut Vec<CommonDiagnostic>,
) -> Result<String> {
    let package_path = root.join("package.mo");
    let Some(definition) = docs_by_path.get(&package_path) else {
        violations.push(CommonDiagnostic::global_error(
            "PKG-006",
            format!("directory '{}' is missing package.mo", root.display()),
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

    let mut top_level_names = top_level_name_entries(&package_path, docs_by_path, source_map)?;
    if top_level_names.len() == 1 {
        return Ok(top_level_names.remove(0).0);
    }

    let message = format!(
        "package.mo '{}' must declare exactly one top-level root",
        package_path.display()
    );
    if let Some((_, Some(primary_span))) = top_level_names.first() {
        let mut diagnostic = CommonDiagnostic::error(
            "PKG-006",
            message,
            PrimaryLabel::new(*primary_span).with_message("package root declared here"),
        );
        if let Some((_, Some(secondary_span))) = top_level_names.get(1) {
            diagnostic = diagnostic.with_label(
                Label::secondary(*secondary_span).with_message("additional top-level root here"),
            );
        }
        violations.push(diagnostic);
    } else {
        violations.push(CommonDiagnostic::global_error(
            "PKG-006",
            format!(
                "package.mo '{}' must declare exactly one top-level root",
                package_path.display()
            ),
        ));
    }
    Ok(root
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("Root")
        .to_string())
}

struct DirectoryChildren {
    has_package_file: bool,
    package_child_dirs: Vec<PathBuf>,
    invalid_modelica_child_dirs: Vec<PathBuf>,
    child_files: Vec<PathBuf>,
}

impl DirectoryChildren {
    fn has_subentities(&self) -> bool {
        !self.package_child_dirs.is_empty()
            || !self.invalid_modelica_child_dirs.is_empty()
            || !self.child_files.is_empty()
    }
}

fn validate_directory(
    root: &Path,
    root_name: &str,
    dir: &Path,
    docs_by_path: &HashMap<PathBuf, &StoredDefinition>,
    source_map: Option<&SourceMap>,
    violations: &mut Vec<CommonDiagnostic>,
) -> Result<()> {
    // MLS §13.4.1: directories participating in the package tree must carry
    // a `package.mo` node; MLS §13.4.3 requires child entities to match their
    // enclosing package via `within`.
    let children = collect_directory_children(dir)?;
    if children.has_subentities() && !children.has_package_file {
        violations.push(CommonDiagnostic::global_error(
            "PKG-006",
            format!("directory '{}' is missing package.mo", dir.display()),
        ));
    }

    emit_child_name_conflicts(dir, &children, docs_by_path, source_map, violations);

    let mut owners_by_name: BTreeMap<String, (String, Option<Span>)> = BTreeMap::new();
    register_file_children(
        root,
        root_name,
        &children.child_files,
        docs_by_path,
        source_map,
        &mut owners_by_name,
        violations,
    )?;

    register_package_child_dirs(
        root,
        root_name,
        &children.package_child_dirs,
        docs_by_path,
        source_map,
        &mut owners_by_name,
        violations,
    )?;
    register_invalid_child_dirs(
        &children.invalid_modelica_child_dirs,
        &mut owners_by_name,
        violations,
    );

    for child in children.package_child_dirs {
        validate_directory(
            root,
            root_name,
            &child,
            docs_by_path,
            source_map,
            violations,
        )?;
    }

    Ok(())
}

fn collect_directory_children(dir: &Path) -> Result<DirectoryChildren> {
    let mut entries: Vec<_> = fs::read_dir(dir)?.collect::<std::io::Result<Vec<_>>>()?;
    entries.sort_by_key(|entry| entry.path());

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

    let (package_child_dirs, invalid_modelica_child_dirs) = classify_child_dirs(child_dirs)?;
    Ok(DirectoryChildren {
        has_package_file: dir.join("package.mo").is_file(),
        package_child_dirs,
        invalid_modelica_child_dirs,
        child_files,
    })
}

fn emit_child_name_conflicts(
    dir: &Path,
    children: &DirectoryChildren,
    docs_by_path: &HashMap<PathBuf, &StoredDefinition>,
    source_map: Option<&SourceMap>,
    violations: &mut Vec<CommonDiagnostic>,
) {
    let child_dir_names: BTreeSet<String> = children
        .package_child_dirs
        .iter()
        .chain(children.invalid_modelica_child_dirs.iter())
        .filter_map(|path| {
            path.file_name()
                .and_then(|name| name.to_str())
                .map(str::to_string)
        })
        .collect();
    let child_file_names: BTreeSet<String> = children
        .child_files
        .iter()
        .filter_map(|path| {
            path.file_stem()
                .and_then(|name| name.to_str())
                .map(str::to_string)
        })
        .collect();

    for conflict in child_dir_names.intersection(&child_file_names) {
        let message = format!(
            "directory '{}' contains both subdirectory '{}' and file '{}.mo'",
            dir.display(),
            conflict,
            conflict
        );
        emit_child_name_conflict_diagnostic(
            dir,
            conflict,
            &message,
            children,
            docs_by_path,
            source_map,
            violations,
        );
    }
}

fn emit_child_name_conflict_diagnostic(
    dir: &Path,
    conflict: &str,
    message: &str,
    children: &DirectoryChildren,
    docs_by_path: &HashMap<PathBuf, &StoredDefinition>,
    source_map: Option<&SourceMap>,
    violations: &mut Vec<CommonDiagnostic>,
) {
    let file_path = dir.join(format!("{conflict}.mo"));
    let file_span = top_level_name_entries(&file_path, docs_by_path, source_map)
        .ok()
        .and_then(|entries| entries.into_iter().find(|(name, _)| name == conflict))
        .and_then(|(_, span)| span);
    let dir_span = children
        .package_child_dirs
        .iter()
        .find(|path| path.file_name().and_then(|name| name.to_str()) == Some(conflict))
        .and_then(|child_dir| {
            let package_mo = child_dir.join("package.mo");
            top_level_name_entries(&package_mo, docs_by_path, source_map)
                .ok()
                .and_then(|entries| entries.into_iter().find(|(name, _)| name == conflict))
                .and_then(|(_, span)| span)
        });

    if let Some(primary_span) = file_span.or(dir_span) {
        let mut diagnostic = CommonDiagnostic::error(
            "PKG-008",
            message.to_string(),
            PrimaryLabel::new(primary_span).with_message("conflicting file/package name"),
        );
        if let (Some(file_span), Some(dir_span)) = (file_span, dir_span)
            && file_span != dir_span
        {
            diagnostic = diagnostic.with_label(
                Label::secondary(dir_span).with_message("conflicting package declared here"),
            );
        }
        violations.push(diagnostic);
        return;
    }

    violations.push(CommonDiagnostic::global_error(
        "PKG-008",
        message.to_string(),
    ));
}

fn register_file_children(
    root: &Path,
    root_name: &str,
    child_files: &[PathBuf],
    docs_by_path: &HashMap<PathBuf, &StoredDefinition>,
    source_map: Option<&SourceMap>,
    owners_by_name: &mut BTreeMap<String, (String, Option<Span>)>,
    violations: &mut Vec<CommonDiagnostic>,
) -> Result<()> {
    for child in child_files {
        let entity = child
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("<unknown>")
            .to_string();
        for (top_level_name, span) in top_level_name_entries(child, docs_by_path, source_map)? {
            record_child_name(&top_level_name, &entity, span, owners_by_name, violations);
        }
        validate_within_clause(root, root_name, child, docs_by_path, source_map, violations);
    }
    Ok(())
}

fn classify_child_dirs(child_dirs: Vec<PathBuf>) -> Result<(Vec<PathBuf>, Vec<PathBuf>)> {
    let mut package_child_dirs = Vec::new();
    let mut invalid_modelica_child_dirs = Vec::new();
    for child in child_dirs {
        if child.join("package.mo").is_file() {
            package_child_dirs.push(child);
            continue;
        }
        if contains_direct_modelica_entities(&child)? {
            invalid_modelica_child_dirs.push(child);
        }
    }
    Ok((package_child_dirs, invalid_modelica_child_dirs))
}

fn register_package_child_dirs(
    root: &Path,
    root_name: &str,
    package_child_dirs: &[PathBuf],
    docs_by_path: &HashMap<PathBuf, &StoredDefinition>,
    source_map: Option<&SourceMap>,
    owners_by_name: &mut BTreeMap<String, (String, Option<Span>)>,
    violations: &mut Vec<CommonDiagnostic>,
) -> Result<()> {
    for child in package_child_dirs {
        let entity = format!("{}/package.mo", child.display());
        let package_mo = child.join("package.mo");
        let names = top_level_name_entries(&package_mo, docs_by_path, source_map)?;
        for (top_level_name, span) in names {
            record_child_name(&top_level_name, &entity, span, owners_by_name, violations);
        }
        validate_within_clause(
            root,
            root_name,
            &package_mo,
            docs_by_path,
            source_map,
            violations,
        );
    }
    Ok(())
}

fn register_invalid_child_dirs(
    invalid_modelica_child_dirs: &[PathBuf],
    owners_by_name: &mut BTreeMap<String, (String, Option<Span>)>,
    violations: &mut Vec<CommonDiagnostic>,
) {
    for child in invalid_modelica_child_dirs {
        violations.push(CommonDiagnostic::global_error(
            "PKG-006",
            format!("directory '{}' is missing package.mo", child.display()),
        ));
        let entity = child.display().to_string();
        let name = child
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("<unknown>");
        record_child_name(name, &entity, None, owners_by_name, violations);
    }
}

fn validate_within_clause(
    root: &Path,
    root_name: &str,
    file_path: &Path,
    docs_by_path: &HashMap<PathBuf, &StoredDefinition>,
    source_map: Option<&SourceMap>,
    violations: &mut Vec<CommonDiagnostic>,
) {
    let Some(definition) = docs_by_path.get(file_path) else {
        return;
    };

    let expected = expected_within_clause(root, root_name, file_path);
    let actual = definition.within.as_ref().map(ToString::to_string);
    let actual_span = definition
        .within
        .as_ref()
        .and_then(|within| name_span(within, source_map));
    let fallback_span = definition
        .classes
        .values()
        .next()
        .and_then(|class| class_name_span(class, source_map));

    match (expected, actual) {
        (None, _) => {}
        (Some(expected), None) => {
            let message = format!(
                "file '{}' is missing required within-clause `within {};`",
                file_path.display(),
                expected
            );
            if let Some(span) = fallback_span {
                violations.push(CommonDiagnostic::error(
                    "PKG-009",
                    message,
                    PrimaryLabel::new(span).with_message("within-clause required here"),
                ));
            } else {
                violations.push(CommonDiagnostic::global_error("PKG-009", message));
            }
        }
        (Some(expected), Some(actual)) if actual != expected => {
            let message = format!(
                "file '{}' has `within {};` but expected `within {};`",
                file_path.display(),
                actual,
                expected
            );
            if let Some(span) = actual_span.or(fallback_span) {
                violations.push(CommonDiagnostic::error(
                    "PKG-010",
                    message,
                    PrimaryLabel::new(span).with_message("within-clause is incorrect"),
                ));
            } else {
                violations.push(CommonDiagnostic::global_error("PKG-010", message));
            }
        }
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
    span: Option<Span>,
    owners_by_name: &mut BTreeMap<String, (String, Option<Span>)>,
    violations: &mut Vec<CommonDiagnostic>,
) {
    if let Some((previous_owner, previous_span)) = owners_by_name.get(name) {
        let message = format!(
            "duplicate class name '{}' defined by sibling entities '{}' and '{}'",
            name, previous_owner, owner
        );
        if let Some(primary_span) = span.or(*previous_span) {
            let mut diagnostic = CommonDiagnostic::error(
                "PKG-007",
                message,
                PrimaryLabel::new(primary_span).with_message("duplicate class name declared here"),
            );
            if let (Some(current_span), Some(previous_span)) = (span, *previous_span)
                && current_span != previous_span
            {
                diagnostic = diagnostic.with_label(
                    Label::secondary(previous_span)
                        .with_message("same class name already declared here"),
                );
            }
            violations.push(diagnostic);
        } else {
            violations.push(CommonDiagnostic::global_error("PKG-007", message));
        }
        return;
    }
    owners_by_name.insert(name.to_string(), (owner.to_string(), span));
}

fn contains_direct_modelica_entities(dir: &Path) -> Result<bool> {
    let mut entries: Vec<_> = fs::read_dir(dir)?.collect::<std::io::Result<Vec<_>>>()?;
    entries.sort_by_key(|entry| entry.path());
    for entry in entries {
        let path = entry.path();
        if path.is_dir() && path.join("package.mo").is_file() {
            return Ok(true);
        }
        if path.is_file() && path.extension().and_then(|ext| ext.to_str()) == Some("mo") {
            return Ok(true);
        }
    }
    Ok(false)
}

fn collect_all_modelica_files_recursive(dir: &Path, out: &mut Vec<PathBuf>) -> Result<()> {
    let mut entries: Vec<_> = fs::read_dir(dir)?.collect::<std::io::Result<Vec<_>>>()?;
    entries.sort_by_key(|entry| entry.path());
    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            collect_all_modelica_files_recursive(&path, out)?;
            continue;
        }
        if path.extension().and_then(|ext| ext.to_str()) == Some("mo") {
            out.push(path);
        }
    }
    Ok(())
}

fn collect_direct_modelica_files(dir: &Path, out: &mut Vec<PathBuf>) -> Result<()> {
    let mut entries: Vec<_> = fs::read_dir(dir)?.collect::<std::io::Result<Vec<_>>>()?;
    entries.sort_by_key(|entry| entry.path());
    for entry in entries {
        let path = entry.path();
        if path.is_file() && path.extension().and_then(|ext| ext.to_str()) == Some("mo") {
            out.push(path);
        }
    }
    Ok(())
}

fn collect_package_tree_files(dir: &Path, out: &mut Vec<PathBuf>) -> Result<()> {
    let package_path = dir.join("package.mo");
    if package_path.is_file() {
        out.push(package_path);
    }

    let mut entries: Vec<_> = fs::read_dir(dir)?.collect::<std::io::Result<Vec<_>>>()?;
    entries.sort_by_key(|entry| entry.path());
    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            if path.join("package.mo").is_file() {
                collect_package_tree_files(&path, out)?;
            }
            continue;
        }
        if path.extension().and_then(|ext| ext.to_str()) == Some("mo")
            && path.file_name().and_then(|name| name.to_str()) != Some("package.mo")
        {
            out.push(path);
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

fn topmost_contiguous_package_root(dir: &Path) -> Option<PathBuf> {
    if !dir.join("package.mo").is_file() {
        return None;
    }

    let mut root = dir.to_path_buf();
    let mut current = dir;
    while let Some(parent) = current.parent() {
        if !parent.join("package.mo").is_file() {
            break;
        }
        root = parent.to_path_buf();
        current = parent;
    }
    Some(root)
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
        let err = err
            .downcast_ref::<PackageLayoutError>()
            .expect("typed package-layout error");
        assert_eq!(err.diagnostics()[0].code.as_deref(), Some("PKG-009"));
        assert!(
            !err.diagnostics()[0].labels.is_empty(),
            "missing within should point at the class declaration"
        );
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
    fn package_layout_valid_layout_does_not_require_reloading_sources() {
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
        fs::remove_file(&child_mo).expect("remove child after parse");

        validate_library_package_layout(&root, &docs)
            .expect("valid layout should not reread missing source files on the success path");
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

    #[test]
    fn package_layout_ignores_non_package_resource_dirs() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path().join("Pkg");
        fs::create_dir_all(root.join("Resources/Images/Docs")).expect("mkdir");
        let package_mo = root.join("package.mo");
        let child_mo = root.join("A.mo");
        let resource_mo = root.join("Resources/Images/Docs/Demo.mo");
        fs::write(&package_mo, "package Pkg end Pkg;").expect("write package");
        fs::write(&child_mo, "within Pkg; model A end A;").expect("write child");
        fs::write(&resource_mo, "model Demo end Demo;").expect("write resource");

        let docs = parse_docs(&[
            (&package_mo, "package Pkg end Pkg;"),
            (&child_mo, "within Pkg; model A end A;"),
        ]);

        validate_library_package_layout(&root, &docs)
            .expect("resource directories outside package tree should be ignored");
    }

    #[test]
    fn compile_unit_collects_same_directory_siblings_for_loose_files() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path().join("workspace");
        fs::create_dir_all(&root).expect("mkdir");
        let focus = root.join("A.mo");
        let sibling = root.join("B.mo");
        let nested = root.join("nested/C.mo");
        fs::write(&focus, "model A end A;").expect("write focus");
        fs::write(&sibling, "model B end B;").expect("write sibling");
        fs::create_dir_all(nested.parent().expect("nested parent")).expect("mkdir nested");
        fs::write(&nested, "model C end C;").expect("write nested");

        let files = collect_compile_unit_source_files(&focus).expect("collect compile unit");
        assert_eq!(files, vec![focus, sibling]);
    }

    #[test]
    fn compile_unit_collects_outermost_package_tree_only() {
        let temp = tempfile::tempdir().expect("tempdir");
        let workspace = temp.path().join("workspace");
        let pkg = workspace.join("Pkg");
        let sub = pkg.join("Sub");
        fs::create_dir_all(&sub).expect("mkdir");
        fs::write(pkg.join("package.mo"), "package Pkg end Pkg;").expect("write pkg");
        fs::write(sub.join("package.mo"), "within Pkg; package Sub end Sub;")
            .expect("write sub package");
        let focus = sub.join("A.mo");
        let sibling = sub.join("B.mo");
        let cousin = pkg.join("C.mo");
        let unrelated = workspace.join("Other.mo");
        fs::write(&focus, "within Pkg.Sub; model A end A;").expect("write focus");
        fs::write(&sibling, "within Pkg.Sub; model B end B;").expect("write sibling");
        fs::write(&cousin, "within Pkg; model C end C;").expect("write cousin");
        fs::write(&unrelated, "model Other end Other;").expect("write unrelated");

        let files = collect_compile_unit_source_files(&focus).expect("collect compile unit");
        assert_eq!(
            files,
            vec![
                cousin,
                focus,
                sibling,
                sub.join("package.mo"),
                pkg.join("package.mo")
            ]
        );
    }
}
