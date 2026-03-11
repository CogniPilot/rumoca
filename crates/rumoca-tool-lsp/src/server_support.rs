use super::*;

pub(super) fn should_reserve_library_load(
    loaded: &HashSet<String>,
    loading: &HashMap<String, u64>,
    path_key: &str,
    expected_epoch: u64,
    current_epoch: u64,
) -> bool {
    expected_epoch == current_epoch && !loaded.contains(path_key) && !loading.contains_key(path_key)
}

pub(super) fn should_apply_library_load(
    loaded: &HashSet<String>,
    path_key: &str,
    expected_epoch: u64,
    current_epoch: u64,
) -> bool {
    expected_epoch == current_epoch && !loaded.contains(path_key)
}

pub(super) fn should_clear_library_load(
    loading: &HashMap<String, u64>,
    path_key: &str,
    reservation_epoch: u64,
) -> bool {
    loading
        .get(path_key)
        .is_some_and(|owner_epoch| *owner_epoch == reservation_epoch)
}

pub(super) fn extract_import_completion_prefix(source: &str, position: Position) -> Option<String> {
    let line = source.lines().nth(position.line as usize)?;
    let col = (position.character as usize).min(line.len());
    let line_prefix = line[..col].trim_start();
    if !line_prefix.starts_with("import") {
        return None;
    }
    let after_import = line_prefix["import".len()..].trim_start();
    let token: String = after_import
        .chars()
        .take_while(|c| c.is_ascii_alphanumeric() || *c == '_' || *c == '.')
        .collect();
    Some(token)
}

pub(super) fn completion_context_needs_resolved_session(
    source: &str,
    position: Position,
    import_prefix: Option<&str>,
) -> bool {
    let prefix = get_text_before_cursor(source, position).unwrap_or_default();
    let trimmed = prefix.trim();
    let requires_import_resolution = import_prefix.is_some();
    trimmed.contains('.')
        || (trimmed.contains('(') && !trimmed.contains(')'))
        || requires_import_resolution
}

#[derive(Debug, Clone)]
pub(super) struct SimulationRequestSettings {
    pub(super) solver: String,
    pub(super) t_end: f64,
    pub(super) dt: Option<f64>,
    pub(super) library_paths: Vec<String>,
}

pub(super) fn parse_simulation_request_settings(
    value: Option<&Value>,
) -> Option<SimulationRequestSettings> {
    let obj = value?.as_object()?;
    let solver = normalize_solver_opt(
        obj.get("solver")
            .and_then(Value::as_str)
            .map(str::to_string),
    )
    .unwrap_or_else(|| "auto".to_string());
    let t_end = obj
        .get("tEnd")
        .and_then(Value::as_f64)
        .filter(|v| v.is_finite() && *v > 0.0)
        .unwrap_or(10.0);
    let dt = normalize_dt_opt(obj.get("dt").and_then(Value::as_f64));
    let library_paths = obj
        .get("modelicaPath")
        .and_then(Value::as_array)
        .map(|items| {
            items
                .iter()
                .filter_map(Value::as_str)
                .map(str::trim)
                .filter(|v| !v.is_empty())
                .map(str::to_string)
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();
    Some(SimulationRequestSettings {
        solver,
        t_end,
        dt,
        library_paths,
    })
}

pub(super) fn parse_fallback_simulation(
    value: Option<&Value>,
) -> Option<EffectiveSimulationConfig> {
    let value = value?;
    let obj = value.as_object()?;
    let solver = normalize_solver_opt(
        obj.get("solver")
            .and_then(Value::as_str)
            .map(str::to_string),
    )
    .unwrap_or_else(|| "auto".to_string());
    let t_end = obj
        .get("tEnd")
        .and_then(Value::as_f64)
        .filter(|v| v.is_finite() && *v > 0.0)
        .unwrap_or(10.0);
    let dt = normalize_dt_opt(obj.get("dt").and_then(Value::as_f64));
    let output_dir = obj
        .get("outputDir")
        .and_then(Value::as_str)
        .unwrap_or_default()
        .to_string();
    let library_paths = obj
        .get("modelicaPath")
        .and_then(Value::as_array)
        .map(|items| {
            items
                .iter()
                .filter_map(Value::as_str)
                .map(str::trim)
                .filter(|v| !v.is_empty())
                .map(str::to_string)
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();
    Some(EffectiveSimulationConfig {
        solver,
        t_end,
        dt,
        output_dir,
        library_paths,
    })
}

pub(super) fn simulation_settings_to_json(settings: &EffectiveSimulationConfig) -> Value {
    json!({
        "solver": settings.solver,
        "tEnd": settings.t_end,
        "dt": settings.dt,
        "outputDir": settings.output_dir,
        "modelicaPath": settings.library_paths,
    })
}

pub(super) fn simulation_preset_to_json(preset: &EffectiveSimulationPreset) -> Value {
    json!({
        "solver": preset.solver,
        "tEnd": preset.t_end,
        "dt": preset.dt,
        "outputDir": preset.output_dir,
        "libraryOverrides": preset.library_overrides,
    })
}

pub(super) fn simulation_override_from_json(value: &Value) -> Option<SimulationModelOverride> {
    let obj = value.as_object()?;
    let solver = obj
        .get("solver")
        .and_then(Value::as_str)
        .map(str::to_string);
    let t_end = obj.get("tEnd").and_then(Value::as_f64);
    let dt = obj.get("dt").and_then(Value::as_f64);
    let output_dir = obj
        .get("outputDir")
        .and_then(Value::as_str)
        .map(str::to_string);
    let library_overrides = obj
        .get("libraryOverrides")
        .and_then(Value::as_array)
        .map(|items| {
            items
                .iter()
                .filter_map(Value::as_str)
                .map(str::trim)
                .filter(|value| !value.is_empty())
                .map(str::to_string)
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();
    Some(SimulationModelOverride {
        solver,
        t_end,
        dt,
        output_dir,
        library_overrides,
    })
}

pub(super) fn parse_views_payload(value: &Value) -> Option<Vec<PlotViewConfig>> {
    serde_json::from_value(value.clone()).ok()
}

pub(super) fn normalize_solver_opt(value: Option<String>) -> Option<String> {
    match value
        .as_deref()
        .map(str::trim)
        .map(str::to_ascii_lowercase)
        .as_deref()
    {
        Some("auto") => Some("auto".to_string()),
        Some("bdf") => Some("bdf".to_string()),
        Some("rk-like") => Some("rk-like".to_string()),
        _ => None,
    }
}

pub(super) fn normalize_dt_opt(value: Option<f64>) -> Option<f64> {
    value.filter(|v| v.is_finite() && *v > 0.0)
}

pub(super) fn should_eager_load_library_for_import_prefix(
    lib_path: &str,
    import_prefix: &str,
) -> bool {
    if import_prefix.is_empty() {
        return true;
    }
    let head = import_prefix.split('.').next().unwrap_or(import_prefix);
    match infer_library_roots(Path::new(lib_path)) {
        Ok(roots) if !roots.is_empty() => roots
            .iter()
            .any(|root| root.starts_with(head) || import_prefix.starts_with(root)),
        Ok(_) => true,
        Err(_) => true,
    }
}

pub(super) fn canonical_path_key(path: &str) -> String {
    std::fs::canonicalize(path)
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|_| path.to_string())
}

pub(super) fn simulation_doc_for_compile(
    uri: &str,
    doc: &Document,
    focus_key: &str,
    loaded_libraries: &HashSet<String>,
) -> std::result::Result<Option<(bool, ast::StoredDefinition)>, String> {
    let is_focus_document = canonical_path_key(uri) == focus_key;
    let is_library_document = library_root_for_document(uri, loaded_libraries).is_some();
    if !is_focus_document && !is_library_document {
        return Ok(None);
    }
    let parsed = if is_focus_document {
        doc.parsed.clone().or_else(|| doc.partial.clone())
    } else {
        doc.parsed.clone()
    };
    match parsed {
        Some(parsed) => Ok(Some((is_focus_document, parsed))),
        None if is_focus_document => Err(format!(
            "active document has no parsed or recovered AST: {}",
            doc.uri
        )),
        None if is_library_document => {
            Err(format!("library document has no parsed AST: {}", doc.uri))
        }
        None => Ok(None),
    }
}

pub(super) fn collect_local_compile_unit_sources(
    session: &Session,
    focus_document_path: &str,
) -> std::result::Result<Vec<(String, String)>, String> {
    let paths = collect_compile_unit_source_files(Path::new(focus_document_path))
        .map_err(|err| format!("failed to collect local compile unit: {err}"))?;
    let mut sources = Vec::new();

    for path in paths {
        let uri = path.to_string_lossy().to_string();
        if let Some(doc) = session.get_document(&uri)
            && !doc.content.is_empty()
        {
            sources.push((uri, doc.content.clone()));
            continue;
        }

        let source = std::fs::read_to_string(&path).map_err(|err| {
            format!(
                "failed to read local compile unit document '{}': {}",
                uri, err
            )
        })?;
        sources.push((uri, source));
    }

    Ok(sources)
}

pub(super) fn collect_isolated_library_parsed_docs(
    session: &Session,
    focus_document_path: &str,
    focus_key: &str,
    loaded_libraries: &HashSet<String>,
    local_doc_keys: &std::collections::BTreeSet<String>,
) -> std::result::Result<std::collections::BTreeMap<String, (String, ast::StoredDefinition)>, String>
{
    let mut parsed_docs_by_key = std::collections::BTreeMap::new();
    for (uri, parsed) in
        collect_simulation_parsed_docs(session, focus_document_path, focus_key, loaded_libraries)?
    {
        let key = canonical_path_key(&uri);
        if local_doc_keys.contains(&key) {
            continue;
        }
        parsed_docs_by_key.entry(key).or_insert((uri, parsed));
    }
    Ok(parsed_docs_by_key)
}

pub(super) fn collect_simulation_parsed_docs(
    session: &Session,
    focus_document_path: &str,
    focus_key: &str,
    loaded_libraries: &HashSet<String>,
) -> std::result::Result<Vec<(String, ast::StoredDefinition)>, String> {
    let uris: Vec<String> = session
        .document_uris()
        .into_iter()
        .map(ToString::to_string)
        .collect();
    let mut has_focus_document = false;
    let mut parsed_docs = Vec::new();

    for uri in uris {
        let Some(doc) = session.get_document(&uri) else {
            continue;
        };
        let Some((is_focus_document, parsed)) =
            simulation_doc_for_compile(&uri, doc, focus_key, loaded_libraries)?
        else {
            continue;
        };
        has_focus_document |= is_focus_document;
        parsed_docs.push((doc.uri.clone(), parsed));
    }

    if !has_focus_document {
        return Err(format!(
            "active document not found in session: {focus_document_path}"
        ));
    }
    Ok(parsed_docs)
}

pub(super) fn library_root_for_document(
    document_path: &str,
    loaded_libraries: &HashSet<String>,
) -> Option<String> {
    let document_key = canonical_path_key(document_path);
    let document_path = Path::new(&document_key);
    loaded_libraries
        .iter()
        .filter_map(|root| {
            let root_path = Path::new(root);
            document_path.starts_with(root_path).then(|| root.clone())
        })
        .max_by_key(|root| root.len())
}

pub(super) fn library_source_set_id(library_path: &str) -> String {
    format!("library::{}", canonical_path_key(library_path))
}

pub(super) fn duplicate_root_provider(
    inferred_roots: &[String],
    claimed_roots: &HashMap<String, String>,
) -> Option<(String, String)> {
    inferred_roots.iter().find_map(|root| {
        claimed_roots
            .get(root)
            .map(|provider| (root.clone(), provider.clone()))
    })
}

pub(super) fn claim_roots(
    claimed_roots: &mut HashMap<String, String>,
    inferred_roots: Vec<String>,
    provider: &str,
) {
    for root in inferred_roots {
        claimed_roots
            .entry(root)
            .or_insert_with(|| provider.to_string());
    }
}

pub(super) fn is_project_config_uri(uri: &Url) -> bool {
    if let Ok(path) = uri.to_file_path() {
        return path
            .components()
            .any(|component| component.as_os_str() == ".rumoca");
    }
    uri.path().contains("/.rumoca/")
}

pub(super) fn session_document_uri_key(uri: &Url) -> String {
    if let Ok(path) = uri.to_file_path() {
        return path.to_string_lossy().to_string();
    }
    uri.path().to_string()
}

pub(super) fn parse_file_move_hints(value: Option<&Value>) -> Vec<ProjectFileMoveHint> {
    let Some(Value::Array(items)) = value else {
        return Vec::new();
    };
    let mut hints = Vec::new();
    for item in items {
        let Some(obj) = item.as_object() else {
            continue;
        };
        let old_path = obj
            .get("oldPath")
            .or_else(|| obj.get("old_path"))
            .and_then(Value::as_str)
            .unwrap_or("")
            .trim()
            .to_string();
        let new_path = obj
            .get("newPath")
            .or_else(|| obj.get("new_path"))
            .and_then(Value::as_str)
            .unwrap_or("")
            .trim()
            .to_string();
        if old_path.is_empty() || new_path.is_empty() {
            continue;
        }
        hints.push(ProjectFileMoveHint { old_path, new_path });
    }
    hints
}

pub(super) fn session_uri_path_to_pathbuf(uri_path: &str) -> PathBuf {
    #[cfg(windows)]
    {
        if let Some(rest) = uri_path.strip_prefix('/') {
            let bytes = rest.as_bytes();
            if bytes.len() >= 2 && bytes[0].is_ascii_alphabetic() && bytes[1] == b':' {
                return PathBuf::from(rest);
            }
        }
    }
    PathBuf::from(uri_path)
}

pub(super) fn collect_workspace_known_models_from_session(
    session: &Session,
    workspace_root: &Path,
) -> Vec<String> {
    let mut parsed_docs: Vec<(String, ast::StoredDefinition)> = Vec::new();
    for uri in session.document_uris() {
        let Some(document) = session.get_document(uri) else {
            continue;
        };
        let Some(parsed) = &document.parsed else {
            continue;
        };
        let path = session_uri_path_to_pathbuf(uri);
        if !path.starts_with(workspace_root) || !path.is_file() {
            continue;
        }
        parsed_docs.push((uri.to_string(), parsed.clone()));
    }

    if parsed_docs.is_empty() {
        return Vec::new();
    }

    match merge_stored_definitions(parsed_docs) {
        Ok(merged) => {
            let mut names = collect_model_names(&merged);
            names.sort();
            names.dedup();
            names
        }
        Err(_) => Vec::new(),
    }
}
