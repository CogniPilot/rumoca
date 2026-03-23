use super::*;

fn write_source_root_file(path: &std::path::Path, contents: &str) {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("mkdirs");
    }
    std::fs::write(path, contents).expect("write source-root file");
}

fn write_two_file_source_root(root: &std::path::Path, child_name: &str, component_name: &str) {
    write_source_root_file(&root.join("package.mo"), "package Lib\nend Lib;\n");
    write_source_root_file(
        &root.join(format!("{child_name}.mo")),
        &format!("within Lib;\nmodel {child_name}\n  Real {component_name};\nend {child_name};\n"),
    );
}

fn write_dependency_source_root(root: &std::path::Path) {
    write_source_root_file(&root.join("package.mo"), "package Lib\nend Lib;\n");
    write_source_root_file(
        &root.join("Base.mo"),
        "within Lib;\nmodel Base\n  Real x;\nend Base;\n",
    );
    write_source_root_file(
        &root.join("Derived.mo"),
        "within Lib;\nmodel Derived\n  Base base;\nend Derived;\n",
    );
}

fn index_source_root_with_cache(
    session: &mut Session,
    cache_dir: &std::path::Path,
    source_root_dir: &std::path::Path,
) -> SourceRootLoadReport {
    session.load_source_root_tolerant_with_cache_dir_for_tests(
        "external::Lib",
        SourceRootKind::External,
        source_root_dir,
        None,
        Some(cache_dir),
    )
}

fn hydrated_file_ids(session: &Session) -> Vec<FileId> {
    session.source_set_file_ids("external::Lib")
}

fn qualified_source_set_members(session: &mut Session) -> Vec<String> {
    let source_set_id = session
        .source_set_id("external::Lib")
        .expect("source-set id should exist");
    session
        .source_set_package_def_map_query(source_set_id)
        .expect("membership query should be available")
        .member_item_keys("Lib.")
        .into_iter()
        .map(|item_key| item_key.qualified_name())
        .collect()
}

fn expected_dependency_fingerprint_for_source_root(
    source_root_dir: &std::path::Path,
    model_name: &str,
) -> ([u8; 32], Vec<String>) {
    let package_uri = source_root_dir
        .join("package.mo")
        .to_string_lossy()
        .to_string();
    let base_uri = source_root_dir
        .join("Base.mo")
        .to_string_lossy()
        .to_string();
    let derived_uri = source_root_dir
        .join("Derived.mo")
        .to_string_lossy()
        .to_string();
    let mut session = Session::default();
    session.add_parsed_batch(vec![
        (
            package_uri.clone(),
            parse_definition("package Lib\nend Lib;\n", &package_uri),
        ),
        (
            base_uri.clone(),
            parse_definition("within Lib;\nmodel Base\n  Real x;\nend Base;\n", &base_uri),
        ),
        (
            derived_uri.clone(),
            parse_definition(
                "within Lib;\nmodel Derived\n  Base base;\nend Derived;\n",
                &derived_uri,
            ),
        ),
    ]);
    let (resolved, _) = session
        .build_resolved_for_strict_compile_with_diagnostics()
        .expect("dependency source root should resolve tolerantly");
    let mut dependency_fingerprints =
        super::super::dependency_fingerprint::DependencyFingerprintCache::from_tree(&resolved.0);
    (
        dependency_fingerprints.model_fingerprint(model_name),
        session.query_state.resolved.model_names.clone(),
    )
}

fn workspace_source_root_definitions(
    root: &std::path::Path,
    child_name: &str,
    component_name: &str,
) -> Vec<(String, ast::StoredDefinition)> {
    let package_uri = root.join("package.mo").to_string_lossy().to_string();
    let child_uri = root
        .join(format!("{child_name}.mo"))
        .to_string_lossy()
        .to_string();
    vec![
        (
            package_uri.clone(),
            parse_definition("package Lib\nend Lib;\n", &package_uri),
        ),
        (
            child_uri.clone(),
            parse_definition(
                &format!(
                    "within Lib;\nmodel {child_name}\n  Real {component_name};\nend {child_name};\n"
                ),
                &child_uri,
            ),
        ),
    ]
}

fn workspace_family_source_root_definitions() -> Vec<(String, ast::StoredDefinition)> {
    let new_folder_package_uri = "NewFolder/package.mo".to_string();
    let new_folder_child_uri = "NewFolder/Test.mo".to_string();
    let other_package_uri = "Other/package.mo".to_string();
    let other_child_uri = "Other/Thing.mo".to_string();
    vec![
        (
            new_folder_package_uri.clone(),
            parse_definition(
                "within ;\npackage NewFolder\nend NewFolder;\n",
                &new_folder_package_uri,
            ),
        ),
        (
            new_folder_child_uri.clone(),
            parse_definition(
                "within NewFolder;\nmodel Test\n  Real x;\nend Test;\n",
                &new_folder_child_uri,
            ),
        ),
        (
            other_package_uri.clone(),
            parse_definition("within ;\npackage Other\nend Other;\n", &other_package_uri),
        ),
        (
            other_child_uri.clone(),
            parse_definition(
                "within Other;\nmodel Thing\n  Real y;\nend Thing;\n",
                &other_child_uri,
            ),
        ),
    ]
}

fn replace_workspace_source_root(
    session: &mut Session,
    root: &std::path::Path,
    child_name: &str,
    component_name: &str,
) {
    session.replace_parsed_source_set(
        "workspace::Lib",
        SourceRootKind::Workspace,
        workspace_source_root_definitions(root, child_name, component_name),
        None,
    );
}

fn hydrated_workspace_file_ids(session: &Session) -> Vec<FileId> {
    session.source_set_file_ids("workspace::Lib")
}

fn qualified_workspace_source_set_members(session: &mut Session) -> Vec<String> {
    let source_set_id = session
        .source_set_id("workspace::Lib")
        .expect("source-set id should exist");
    session
        .source_set_package_def_map_query(source_set_id)
        .expect("membership query should be available")
        .member_item_keys("Lib.")
        .into_iter()
        .map(|item_key| item_key.qualified_name())
        .collect()
}

fn qualified_members_for_source_set(
    session: &mut Session,
    source_set_key: &str,
    prefix: &str,
) -> Vec<String> {
    let source_set_id = session
        .source_set_id(source_set_key)
        .expect("source-set id should exist");
    session
        .source_set_package_def_map_query(source_set_id)
        .expect("membership query should be available")
        .member_item_keys(prefix)
        .into_iter()
        .map(|item_key| item_key.qualified_name())
        .collect()
}

#[test]
fn load_source_root_tolerant_hydrates_query_caches_from_persisted_summary() {
    let temp = tempfile::tempdir().expect("tempdir");
    let source_root_dir = temp.path().join("Lib");
    let cache_dir = temp.path().join("cache");
    write_two_file_source_root(&source_root_dir, "M", "x");

    let mut first = Session::default();
    let first_report = index_source_root_with_cache(&mut first, &cache_dir, &source_root_dir);
    assert_eq!(
        first_report.inserted_file_count, 2,
        "initial load should insert both source-root files"
    );

    let summary_dir = resolve_semantic_summary_cache_dir_from_root(Some(&cache_dir))
        .expect("summary cache dir should resolve");
    let docs = first
        .source_root_parsed_documents("external::Lib")
        .expect("loaded source root should expose parsed documents");
    let cache_key = super::super::semantic_summary_cache::source_root_semantic_cache_key(
        "external::Lib",
        &docs,
    );
    let summary_file = summary_dir.join(format!("{}.bin", cache_key));
    assert!(
        summary_file.is_file(),
        "semantic summary should be written alongside the source-root cache"
    );

    let mut second = Session::default();
    let second_report = index_source_root_with_cache(&mut second, &cache_dir, &source_root_dir);
    assert_eq!(
        second_report.cache_status,
        Some(crate::source_roots::SourceRootCacheStatus::Hit),
        "second load should reuse the parsed source-root cache",
    );

    let source_set_id = second
        .source_set_id("external::Lib")
        .expect("source-set id should exist");
    for file_id in hydrated_file_ids(&second) {
        assert!(
            second
                .query_state
                .ast
                .file_summary_cache
                .contains_key(&file_id),
            "file summary should be hydrated during source-root load"
        );
        assert!(
            second
                .query_state
                .ast
                .declaration_index_cache
                .contains_key(&file_id),
            "declaration index should be hydrated during source-root load"
        );
        assert!(
            second
                .query_state
                .ast
                .file_item_index_cache
                .contains_key(&file_id),
            "workspace symbol index should be hydrated from the declaration summary"
        );
        assert!(
            second
                .query_state
                .ast
                .class_interface_query_cache
                .contains_key(&file_id),
            "class interface index should be hydrated from the persisted file summary"
        );
    }
    assert!(
        second
            .query_state
            .ast
            .package_def_map
            .source_set_caches
            .contains_key(&source_set_id),
        "source-set package membership should be hydrated before the first query"
    );
    assert!(
        second
            .query_state
            .ast
            .source_root_namespace_cache
            .as_ref()
            .and_then(|cache| cache.source_set_caches.get(&source_set_id))
            .is_some(),
        "source-set namespace cache should be hydrated from the persisted class graph state"
    );
    assert_eq!(
        qualified_source_set_members(&mut second),
        vec!["Lib.M".to_string()],
        "hydrated membership should preserve the declared class set"
    );
}

#[test]
fn warm_source_root_restore_hydrates_resolved_aggregate_inputs_from_persisted_summary() {
    let temp = tempfile::tempdir().expect("tempdir");
    let source_root_dir = temp.path().join("Lib");
    let cache_dir = temp.path().join("cache");
    write_dependency_source_root(&source_root_dir);

    let (expected_fingerprint, expected_model_names) =
        expected_dependency_fingerprint_for_source_root(&source_root_dir, "Lib.Derived");

    let mut first = Session::default();
    let first_report = index_source_root_with_cache(&mut first, &cache_dir, &source_root_dir);
    assert_eq!(
        first_report.inserted_file_count, 3,
        "initial load should insert all dependency-source-root files"
    );

    let mut second = Session::default();
    let second_report = index_source_root_with_cache(&mut second, &cache_dir, &source_root_dir);
    assert_eq!(
        second_report.cache_status,
        Some(crate::source_roots::SourceRootCacheStatus::Hit),
        "warm reopen should reuse the parsed source-root cache",
    );

    assert_eq!(
        second.query_state.resolved.model_names, expected_model_names,
        "warm restore should hydrate resolved-tier model names from the persisted source-root aggregate"
    );
    let standard_cache = second
        .query_state
        .resolved
        .dependency_fingerprints
        .standard
        .as_mut()
        .expect("warm restore should hydrate standard dependency fingerprints");
    assert_eq!(
        standard_cache.model_fingerprint("Lib.Derived"),
        expected_fingerprint,
        "warm restore should hydrate dependency fingerprints from the persisted source-root aggregate"
    );
    let strict_cache = second
        .query_state
        .resolved
        .dependency_fingerprints
        .strict_compile_recovery
        .as_mut()
        .expect("warm restore should hydrate strict dependency fingerprints");
    assert_eq!(
        strict_cache.model_fingerprint("Lib.Derived"),
        expected_fingerprint,
        "warm restore should hydrate strict compile dependency inputs from the same persisted source-root aggregate"
    );
}

#[test]
fn malformed_semantic_summary_cache_is_ignored() {
    let temp = tempfile::tempdir().expect("tempdir");
    let source_root_dir = temp.path().join("Lib");
    let cache_dir = temp.path().join("cache");
    write_two_file_source_root(&source_root_dir, "M", "x");

    let mut first = Session::default();
    let first_report = index_source_root_with_cache(&mut first, &cache_dir, &source_root_dir);
    assert_eq!(
        first_report.inserted_file_count, 2,
        "initial load should insert both source-root files"
    );

    let summary_dir = resolve_semantic_summary_cache_dir_from_root(Some(&cache_dir))
        .expect("summary cache dir should resolve");
    let docs = first
        .source_root_parsed_documents("external::Lib")
        .expect("loaded source root should expose parsed documents");
    let cache_key = super::super::semantic_summary_cache::source_root_semantic_cache_key(
        "external::Lib",
        &docs,
    );
    let summary_file = summary_dir.join(format!("{cache_key}.bin"));
    std::fs::write(&summary_file, b"not-a-semantic-summary").expect("write malformed summary");

    let mut second = Session::default();
    let second_report = index_source_root_with_cache(&mut second, &cache_dir, &source_root_dir);
    assert_eq!(
        second_report.cache_status,
        Some(crate::source_roots::SourceRootCacheStatus::Hit),
        "parsed source-root cache should still be reusable",
    );
    assert_eq!(
        qualified_source_set_members(&mut second),
        vec!["Lib.M".to_string()],
        "a malformed semantic summary cache should fall back to a rebuild, not panic"
    );
}

#[test]
fn source_root_status_tracks_cold_index_build_then_warm_cache_restore() {
    let temp = tempfile::tempdir().expect("tempdir");
    let source_root_dir = temp.path().join("Lib");
    let cache_dir = temp.path().join("cache");
    write_two_file_source_root(&source_root_dir, "M", "x");

    let mut first = Session::default();
    let first_report = index_source_root_with_cache(&mut first, &cache_dir, &source_root_dir);
    assert_eq!(
        first_report.inserted_file_count, 2,
        "initial load should insert both source-root files"
    );
    let first_status = first
        .source_root_status("external::Lib")
        .expect("source-root status should exist after the first load");
    assert_eq!(
        first_status.source_root_path,
        Some(source_root_dir.display().to_string()),
        "status should retain the source-root path for client rendering"
    );
    assert_eq!(
        first_status.current, None,
        "synchronous cold index build should complete without leaving a running status"
    );
    assert_eq!(
        first_status.last_completed,
        Some(SourceRootActivitySnapshot {
            kind: SourceRootActivityKind::ColdIndexBuild,
            phase: SourceRootActivityPhase::Completed,
            dirty_class_prefixes: Vec::new(),
        }),
        "first load should report a cold index build"
    );

    let mut second = Session::default();
    let second_report = index_source_root_with_cache(&mut second, &cache_dir, &source_root_dir);
    assert_eq!(
        second_report.cache_status,
        Some(crate::source_roots::SourceRootCacheStatus::Hit),
        "second load should reuse the parsed source-root cache"
    );
    let second_status = second
        .source_root_status("external::Lib")
        .expect("source-root status should exist after warm restore");
    assert_eq!(
        second_status.current, None,
        "warm restore should finish without a lingering running status"
    );
    assert_eq!(
        second_status.last_completed,
        Some(SourceRootActivitySnapshot {
            kind: SourceRootActivityKind::WarmCacheRestore,
            phase: SourceRootActivityPhase::Completed,
            dirty_class_prefixes: Vec::new(),
        }),
        "second load should report a warm cache restore"
    );
}

#[test]
fn source_root_edit_ignores_stale_persisted_summary() {
    let temp = tempfile::tempdir().expect("tempdir");
    let source_root_dir = temp.path().join("Lib");
    let cache_dir = temp.path().join("cache");
    write_two_file_source_root(&source_root_dir, "M", "x");

    let mut first = Session::default();
    index_source_root_with_cache(&mut first, &cache_dir, &source_root_dir);

    std::fs::remove_file(source_root_dir.join("M.mo")).expect("remove old leaf");
    write_two_file_source_root(&source_root_dir, "N", "y");

    let mut second = Session::default();
    index_source_root_with_cache(&mut second, &cache_dir, &source_root_dir);

    assert_eq!(
        qualified_source_set_members(&mut second),
        vec!["Lib.N".to_string()],
        "changed source-root contents must not reuse the stale persisted summary"
    );

    let file_id = second
        .file_id(&source_root_dir.join("N.mo").to_string_lossy())
        .expect("updated file should have a stable file id");
    let declaration_index = second
        .query_state
        .ast
        .declaration_index_cache
        .get(&file_id)
        .expect("declaration index should be hydrated for the edited file");
    let declared = declaration_index
        .index
        .iter()
        .map(|(item_key, _)| item_key.qualified_name())
        .collect::<Vec<_>>();
    assert!(
        declared
            .iter()
            .any(|qualified_name| qualified_name == "Lib.N")
    );
    assert!(
        declared
            .iter()
            .all(|qualified_name| qualified_name != "Lib.M")
    );
}

#[test]
fn mismatched_file_summary_ignores_persisted_semantic_summary() {
    let temp = tempfile::tempdir().expect("tempdir");
    let source_root_dir = temp.path().join("Lib");
    let cache_dir = temp.path().join("cache");
    write_two_file_source_root(&source_root_dir, "M", "x");

    let mut first = Session::default();
    let first_report = index_source_root_with_cache(&mut first, &cache_dir, &source_root_dir);
    let cache_key = first_report
        .cache_key
        .as_deref()
        .expect("source-root cache key should be reported");
    let summary_dir =
        super::super::semantic_summary_cache::resolve_semantic_summary_cache_dir_from_root(Some(
            &cache_dir,
        ))
        .expect("summary cache dir should resolve");

    let package_uri = source_root_dir
        .join("package.mo")
        .to_string_lossy()
        .to_string();
    let leaf_uri = source_root_dir.join("M.mo").to_string_lossy().to_string();
    let tampered_summary =
        super::super::semantic_summary_cache::SourceRootSemanticSummary::from_documents(&[
            (
                package_uri.clone(),
                parse_definition("package Lib\nend Lib;\n", &package_uri),
            ),
            (
                leaf_uri.clone(),
                parse_definition(
                    "within Lib;\nmodel Wrong\n  Real z;\nend Wrong;\n",
                    &leaf_uri,
                ),
            ),
        ]);
    assert!(
        super::super::semantic_summary_cache::write_source_root_semantic_summary(
            Some(&summary_dir),
            "external::Lib",
            &source_root_dir,
            cache_key,
            &tampered_summary,
        ),
        "tampered persisted summary should overwrite the original test cache entry"
    );

    let mut second = Session::default();
    index_source_root_with_cache(&mut second, &cache_dir, &source_root_dir);

    assert_eq!(
        qualified_source_set_members(&mut second),
        vec!["Lib.M".to_string()],
        "source-root load must ignore persisted summaries whose file-summary fingerprints do not match"
    );
}

#[test]
fn mismatched_source_root_id_ignores_persisted_semantic_summary() {
    let temp = tempfile::tempdir().expect("tempdir");
    let source_root_dir = temp.path().join("Lib");
    let cache_dir = temp.path().join("cache");
    write_two_file_source_root(&source_root_dir, "M", "x");

    let mut first = Session::default();
    let first_report = index_source_root_with_cache(&mut first, &cache_dir, &source_root_dir);
    let cache_key = first_report
        .cache_key
        .as_deref()
        .expect("source-root cache key should be reported");
    let summary_dir =
        super::super::semantic_summary_cache::resolve_semantic_summary_cache_dir_from_root(Some(
            &cache_dir,
        ))
        .expect("summary cache dir should resolve");

    let package_uri = source_root_dir
        .join("package.mo")
        .to_string_lossy()
        .to_string();
    let leaf_uri = source_root_dir.join("M.mo").to_string_lossy().to_string();
    let tampered_summary =
        super::super::semantic_summary_cache::SourceRootSemanticSummary::from_documents(&[
            (
                package_uri.clone(),
                parse_definition("package Lib\nend Lib;\n", &package_uri),
            ),
            (
                leaf_uri.clone(),
                parse_definition(
                    "within Lib;\nmodel Wrong\n  Real z;\nend Wrong;\n",
                    &leaf_uri,
                ),
            ),
        ]);
    assert!(
        super::super::semantic_summary_cache::write_source_root_semantic_summary(
            Some(&summary_dir),
            "workspace::Lib",
            &source_root_dir,
            cache_key,
            &tampered_summary,
        ),
        "tampered persisted summary should overwrite the original test cache entry"
    );

    let mut second = Session::default();
    index_source_root_with_cache(&mut second, &cache_dir, &source_root_dir);

    assert_eq!(
        qualified_source_set_members(&mut second),
        vec!["Lib.M".to_string()],
        "source-root id mismatches must not hydrate a summary from another root"
    );
}

#[test]
fn workspace_source_root_semantic_summary_round_trips_through_generic_manifest_helpers() {
    let temp = tempfile::tempdir().expect("tempdir");
    let workspace_root = temp.path().join("workspace-root");
    let cache_dir = temp.path().join("cache");
    let summary_dir = resolve_semantic_summary_cache_dir_from_root(Some(&cache_dir))
        .expect("summary cache dir should resolve");

    let mut first = Session::default();
    replace_workspace_source_root(&mut first, &workspace_root, "M", "x");
    let docs = first
        .source_root_parsed_documents("workspace::Lib")
        .expect("workspace source root should expose parsed documents");
    let cache_key = super::super::semantic_summary_cache::source_root_semantic_cache_key(
        "workspace::Lib",
        &docs,
    );
    let summary = first
        .build_and_write_source_root_semantic_summary(
            Some(&summary_dir),
            "workspace::Lib",
            &workspace_root,
        )
        .expect("workspace source root should produce a semantic summary");
    assert!(
        summary_dir.join(format!("{cache_key}.bin")).is_file(),
        "workspace semantic summary should be written via the generic manifest path"
    );

    let child_uri = workspace_root.join("M.mo").to_string_lossy().to_string();
    assert!(
        summary.summary_fingerprint_for_uri(&child_uri).is_some(),
        "workspace semantic summary should track per-file fingerprints"
    );

    let mut second = Session::default();
    replace_workspace_source_root(&mut second, &workspace_root, "M", "x");
    assert!(
        second
            .hydrate_source_root_semantic_summary_from_cache(Some(&summary_dir), "workspace::Lib"),
        "workspace source root should hydrate through the generic manifest helper"
    );

    let source_set_id = second
        .source_set_id("workspace::Lib")
        .expect("workspace source-set id should exist");
    for file_id in hydrated_workspace_file_ids(&second) {
        assert!(
            second
                .query_state
                .ast
                .file_summary_cache
                .contains_key(&file_id),
            "workspace source root should hydrate file summaries"
        );
        assert!(
            second
                .query_state
                .ast
                .declaration_index_cache
                .contains_key(&file_id),
            "workspace source root should hydrate declaration indexes"
        );
        assert!(
            second
                .query_state
                .ast
                .class_interface_query_cache
                .contains_key(&file_id),
            "workspace source root should hydrate class interface indexes"
        );
    }
    assert!(
        second
            .query_state
            .ast
            .package_def_map
            .source_set_caches
            .contains_key(&source_set_id),
        "workspace source root membership should hydrate before the first query"
    );
    assert!(
        second
            .query_state
            .ast
            .source_root_namespace_cache
            .as_ref()
            .and_then(|cache| cache.source_set_caches.get(&source_set_id))
            .is_some(),
        "workspace source root namespace cache should hydrate with the class graph state"
    );
    assert_eq!(
        qualified_workspace_source_set_members(&mut second),
        vec!["Lib.M".to_string()],
        "workspace source root hydration should preserve declared members"
    );
}

#[test]
fn mismatched_class_graph_fingerprint_ignores_persisted_semantic_summary() {
    let temp = tempfile::tempdir().expect("tempdir");
    let workspace_root = temp.path().join("workspace-root");
    let cache_dir = temp.path().join("cache");
    let summary_dir = resolve_semantic_summary_cache_dir_from_root(Some(&cache_dir))
        .expect("summary cache dir should resolve");

    let mut first = Session::default();
    replace_workspace_source_root(&mut first, &workspace_root, "M", "x");
    let docs = first
        .source_root_parsed_documents("workspace::Lib")
        .expect("workspace source root should expose parsed documents");
    let cache_key = super::super::semantic_summary_cache::source_root_semantic_cache_key(
        "workspace::Lib",
        &docs,
    );
    let summary = first
        .build_and_write_source_root_semantic_summary(
            Some(&summary_dir),
            "workspace::Lib",
            &workspace_root,
        )
        .expect("workspace source root should produce a semantic summary");

    assert!(
        super::super::semantic_summary_cache::write_source_root_semantic_summary_with_class_graph_fingerprint(
            Some(&summary_dir),
            "workspace::Lib",
            &workspace_root,
            &cache_key,
            &summary,
            [0; 32],
        ),
        "tampered class graph fingerprint should overwrite the original test cache entry"
    );

    let mut second = Session::default();
    replace_workspace_source_root(&mut second, &workspace_root, "M", "x");
    assert!(
        !second
            .hydrate_source_root_semantic_summary_from_cache(Some(&summary_dir), "workspace::Lib"),
        "class-graph fingerprint mismatches must not hydrate a stale semantic summary"
    );
}

#[test]
fn partitioned_workspace_family_reuses_aggregate_cache_contract_after_reopen() {
    let temp = tempfile::tempdir().expect("tempdir");
    let cache_root = temp.path().join("cache");
    let source_root_prefix = "workspace::family";

    let mut first = Session::default();
    let inserted = first.sync_partitioned_source_root_family(
        source_root_prefix,
        SourceRootKind::Workspace,
        workspace_family_source_root_definitions(),
        Some(&cache_root),
        None,
    );
    assert_eq!(
        inserted, 4,
        "initial family sync should insert all workspace files"
    );

    let mut root_keys = first.source_root_keys_with_prefix(source_root_prefix);
    root_keys.sort();
    assert_eq!(
        root_keys,
        vec![
            "workspace::family::NewFolder".to_string(),
            "workspace::family::Other".to_string()
        ],
        "partitioned workspace sync should create one source root per top-level package"
    );

    let summary_dir = resolve_semantic_summary_cache_dir_from_root(Some(&cache_root))
        .expect("summary cache dir should resolve");
    let summary_files = std::fs::read_dir(&summary_dir)
        .expect("summary cache dir should exist")
        .collect::<Result<Vec<_>, _>>()
        .expect("summary cache dir entries should read");
    assert_eq!(
        summary_files.len(),
        2,
        "partitioned workspace sync should persist one semantic summary per top-level package root"
    );

    let mut second = Session::default();
    let reinserted = second.sync_partitioned_source_root_family(
        source_root_prefix,
        SourceRootKind::Workspace,
        workspace_family_source_root_definitions(),
        Some(&cache_root),
        None,
    );
    assert_eq!(
        reinserted, 4,
        "reopened family sync should still populate all workspace files"
    );

    let new_folder_id = second
        .source_set_id("workspace::family::NewFolder")
        .expect("NewFolder source root should exist after reopen");
    let other_id = second
        .source_set_id("workspace::family::Other")
        .expect("Other source root should exist after reopen");

    assert!(
        second
            .query_state
            .ast
            .package_def_map
            .source_set_caches
            .contains_key(&new_folder_id),
        "reopened workspace family sync should hydrate package membership for NewFolder"
    );
    assert!(
        second
            .query_state
            .ast
            .package_def_map
            .source_set_caches
            .contains_key(&other_id),
        "reopened workspace family sync should hydrate package membership for Other"
    );
    let namespace_cache = second
        .query_state
        .ast
        .source_root_namespace_cache
        .as_ref()
        .expect("reopened workspace family sync should hydrate namespace cache state");
    assert!(
        namespace_cache
            .source_set_caches
            .contains_key(&new_folder_id),
        "reopened workspace family sync should hydrate namespace cache for NewFolder"
    );
    assert!(
        namespace_cache.source_set_caches.contains_key(&other_id),
        "reopened workspace family sync should hydrate namespace cache for Other"
    );
    assert_eq!(
        qualified_members_for_source_set(&mut second, "workspace::family::NewFolder", "NewFolder.",),
        vec!["NewFolder.Test".to_string()],
        "reopened workspace family sync should preserve NewFolder membership"
    );
    assert_eq!(
        qualified_members_for_source_set(&mut second, "workspace::family::Other", "Other."),
        vec!["Other.Thing".to_string()],
        "reopened workspace family sync should preserve Other membership"
    );
}

#[test]
fn warm_source_root_restore_keeps_namespace_completion_on_restored_aggregate_path() {
    let _guard = session_stats_test_guard();
    let temp = tempfile::tempdir().expect("tempdir");
    let source_root_dir = temp.path().join("Lib");
    let cache_dir = temp.path().join("cache");
    write_two_file_source_root(&source_root_dir, "M", "x");

    let mut first = Session::default();
    let first_report = index_source_root_with_cache(&mut first, &cache_dir, &source_root_dir);
    assert_eq!(
        first_report.cache_status,
        Some(crate::source_roots::SourceRootCacheStatus::Miss),
        "initial load should populate the source-root cache",
    );

    let mut second = Session::default();
    let second_report = index_source_root_with_cache(&mut second, &cache_dir, &source_root_dir);
    assert_eq!(
        second_report.cache_status,
        Some(crate::source_roots::SourceRootCacheStatus::Hit),
        "second load should reuse the outer parsed source-root snapshot",
    );

    crate::compile::reset_session_cache_stats();
    let children = second
        .namespace_children_for_completion("Lib.")
        .expect("namespace completion should succeed after warm restore");
    let delta = crate::compile::session_cache_stats();

    assert!(
        children.iter().any(|(name, _, _)| name == "M"),
        "warm namespace completion should still expose the restored class graph"
    );
    assert_eq!(
        delta.source_set_package_membership_query_misses, 0,
        "warm restore should not rebuild source-set membership from raw summaries on first namespace query"
    );
}
