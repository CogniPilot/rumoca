use super::*;

fn write_library_file(path: &std::path::Path, contents: &str) {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("mkdirs");
    }
    std::fs::write(path, contents).expect("write library file");
}

fn write_two_file_library(root: &std::path::Path, child_name: &str, component_name: &str) {
    write_library_file(&root.join("package.mo"), "package Lib\nend Lib;\n");
    write_library_file(
        &root.join(format!("{child_name}.mo")),
        &format!("within Lib;\nmodel {child_name}\n  Real {component_name};\nend {child_name};\n"),
    );
}

fn index_library_with_cache(
    session: &mut Session,
    cache_dir: &std::path::Path,
    library_dir: &std::path::Path,
) -> IndexingReport {
    session.index_library_tolerant_with_cache_dir_for_tests(
        "library::Lib",
        SourceRootKind::Library,
        library_dir,
        None,
        Some(cache_dir),
    )
}

fn hydrated_file_ids(session: &Session) -> Vec<FileId> {
    session.source_set_file_ids("library::Lib")
}

fn qualified_source_set_members(session: &mut Session) -> Vec<String> {
    let source_set_id = session
        .source_set_id("library::Lib")
        .expect("source-set id should exist");
    session
        .source_set_package_def_map_query(source_set_id)
        .expect("membership query should be available")
        .member_item_keys("Lib.")
        .into_iter()
        .map(|item_key| item_key.qualified_name())
        .collect()
}

#[test]
fn index_library_tolerant_hydrates_query_caches_from_persisted_summary() {
    let temp = tempfile::tempdir().expect("tempdir");
    let library_dir = temp.path().join("Lib");
    let cache_dir = temp.path().join("cache");
    write_two_file_library(&library_dir, "M", "x");

    let mut first = Session::default();
    let first_report = index_library_with_cache(&mut first, &cache_dir, &library_dir);
    assert_eq!(
        first_report.inserted_file_count, 2,
        "initial load should insert both library files"
    );

    let summary_dir = resolve_semantic_summary_cache_dir_from_root(Some(&cache_dir))
        .expect("summary cache dir should resolve");
    let summary_file = summary_dir.join(format!(
        "{}.bin",
        first_report
            .cache_key
            .as_deref()
            .expect("library cache key should be reported")
    ));
    assert!(
        summary_file.is_file(),
        "semantic summary should be written alongside the library cache"
    );

    let mut second = Session::default();
    let second_report = index_library_with_cache(&mut second, &cache_dir, &library_dir);
    assert_eq!(
        second_report.cache_status,
        Some(crate::libraries::LibraryCacheStatus::Hit),
        "second load should reuse the parsed library cache",
    );

    let source_set_id = second
        .source_set_id("library::Lib")
        .expect("source-set id should exist");
    for file_id in hydrated_file_ids(&second) {
        assert!(
            second
                .query_state
                .ast
                .file_summary_cache
                .contains_key(&file_id),
            "file summary should be hydrated during library load"
        );
        assert!(
            second
                .query_state
                .ast
                .declaration_index_cache
                .contains_key(&file_id),
            "declaration index should be hydrated during library load"
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
    assert_eq!(
        qualified_source_set_members(&mut second),
        vec!["Lib.M".to_string()],
        "hydrated membership should preserve the declared class set"
    );
}

#[test]
fn library_edit_ignores_stale_persisted_summary() {
    let temp = tempfile::tempdir().expect("tempdir");
    let library_dir = temp.path().join("Lib");
    let cache_dir = temp.path().join("cache");
    write_two_file_library(&library_dir, "M", "x");

    let mut first = Session::default();
    index_library_with_cache(&mut first, &cache_dir, &library_dir);

    std::fs::remove_file(library_dir.join("M.mo")).expect("remove old leaf");
    write_two_file_library(&library_dir, "N", "y");

    let mut second = Session::default();
    index_library_with_cache(&mut second, &cache_dir, &library_dir);

    assert_eq!(
        qualified_source_set_members(&mut second),
        vec!["Lib.N".to_string()],
        "changed library contents must not reuse the stale persisted summary"
    );

    let file_id = second
        .file_id(&library_dir.join("N.mo").to_string_lossy())
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
    let library_dir = temp.path().join("Lib");
    let cache_dir = temp.path().join("cache");
    write_two_file_library(&library_dir, "M", "x");

    let mut first = Session::default();
    let first_report = index_library_with_cache(&mut first, &cache_dir, &library_dir);
    let cache_key = first_report
        .cache_key
        .as_deref()
        .expect("library cache key should be reported");
    let summary_dir =
        super::super::semantic_summary_cache::resolve_semantic_summary_cache_dir_from_root(Some(
            &cache_dir,
        ))
        .expect("summary cache dir should resolve");

    let package_uri = library_dir.join("package.mo").to_string_lossy().to_string();
    let leaf_uri = library_dir.join("M.mo").to_string_lossy().to_string();
    let tampered_summary =
        super::super::semantic_summary_cache::LibrarySemanticSummary::from_documents(&[
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
        super::super::semantic_summary_cache::write_library_semantic_summary(
            Some(&summary_dir),
            &library_dir,
            cache_key,
            &tampered_summary,
        ),
        "tampered persisted summary should overwrite the original test cache entry"
    );

    let mut second = Session::default();
    index_library_with_cache(&mut second, &cache_dir, &library_dir);

    assert_eq!(
        qualified_source_set_members(&mut second),
        vec!["Lib.M".to_string()],
        "library load must ignore persisted summaries whose file-summary fingerprints do not match"
    );
}
