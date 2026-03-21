use super::*;

#[test]
fn test_compile_with_project_sources_resolves_workspace_package_imports() {
    let _guard = session_test_guard();
    clear_source_root_cache();

    let compiled = compile_with_project_sources(
        USES_WORKSPACE_PACKAGE_SOURCE,
        "UsesWorkspacePackage",
        &workspace_package_sources_json(),
    )
    .expect("compile_with_project_sources should succeed");
    let compiled_result: serde_json::Value =
        serde_json::from_str(&compiled).expect("compile should return valid JSON");

    assert!(
        compiled_result
            .get("balance")
            .and_then(|b| b.get("is_balanced"))
            .and_then(|v| v.as_bool())
            .unwrap_or(false),
        "expected compile_with_project_sources to resolve project-local packages, got: {compiled_result:?}"
    );

    clear_source_root_cache();
}

#[test]
fn test_sync_project_sources_enables_workspace_package_completion_and_diagnostics() {
    let _guard = session_test_guard();
    clear_source_root_cache();
    sync_project_sources("{}").expect("empty project sync should succeed");

    sync_project_sources(&workspace_package_sources_json())
        .expect("sync_project_sources should succeed");

    let import_completion_source = r#"
    model UsesWorkspaceImport
      import New
    end UsesWorkspaceImport;
    "#;
    let import_completion_json =
        lsp_completion(import_completion_source, 2, "      import New".len() as u32)
            .expect("import completion should succeed with synced workspace sources");
    let import_labels = completion_labels(&import_completion_json);
    assert!(
        import_labels.iter().any(|label| label == "NewFolder"),
        "expected import completion to include NewFolder, got: {import_labels:?}"
    );

    let completion_source = r#"
    model UsesWorkspacePackage
      NewFolder.
    end UsesWorkspacePackage;
    "#;
    let completion_json = lsp_completion(completion_source, 2, "      NewFolder.".len() as u32)
        .expect("completion should succeed with synced workspace sources");
    let labels = completion_labels(&completion_json);
    assert!(
        labels.iter().any(|label| label == "Test"),
        "expected workspace package completion to include Test, got: {labels:?}"
    );

    let diagnostics_json =
        lsp_diagnostics(USES_WORKSPACE_PACKAGE_SOURCE).expect("diagnostics should succeed");
    let diagnostics: Vec<lsp_types::Diagnostic> =
        serde_json::from_str(&diagnostics_json).expect("diagnostics JSON should decode");
    assert!(
        diagnostics.iter().all(|diag| {
            let message = diag.message.to_ascii_lowercase();
            !message.contains("unresolved import") && !message.contains("unresolved type reference")
        }),
        "workspace package diagnostics should resolve NewFolder.Test, got: {diagnostics:?}"
    );

    clear_source_root_cache();
    sync_project_sources("{}").expect("empty project sync should succeed");
}

#[test]
#[cfg(not(target_arch = "wasm32"))]
fn test_sync_project_sources_writes_workspace_semantic_summary_cache_when_cache_root_exists() {
    let _guard = session_test_guard();
    clear_source_root_cache();
    let cache_root = unique_test_cache_root();

    sync_project_sources_with_cache_root_for_tests(&workspace_package_sources_json(), &cache_root)
        .expect("sync_project_sources should succeed");

    let summary_dir = cache_root.join("semantic-summaries");
    let entries: Vec<_> = std::fs::read_dir(&summary_dir)
        .expect("workspace summary cache dir should exist")
        .collect::<Result<Vec<_>, _>>()
        .expect("workspace summary cache dir entries should read");
    assert!(
        !entries.is_empty(),
        "workspace source sync should populate the generic source-root summary cache"
    );

    clear_source_root_cache();
    sync_project_sources_with_cache_root_for_tests(&workspace_package_sources_json(), &cache_root)
        .expect("second sync_project_sources should succeed");

    let diagnostics_json =
        lsp_diagnostics(USES_WORKSPACE_PACKAGE_SOURCE).expect("diagnostics should succeed");
    let diagnostics: Vec<lsp_types::Diagnostic> =
        serde_json::from_str(&diagnostics_json).expect("diagnostics JSON should decode");
    assert!(
        diagnostics.iter().all(|diag| {
            let message = diag.message.to_ascii_lowercase();
            !message.contains("unresolved import") && !message.contains("unresolved type reference")
        }),
        "workspace source cache restore should preserve NewFolder.Test resolution, got: {diagnostics:?}"
    );

    let _ = std::fs::remove_dir_all(&cache_root);

    clear_source_root_cache();
}

#[test]
fn test_sync_project_sources_removes_stale_workspace_package_roots() {
    let _guard = session_test_guard();
    clear_source_root_cache();

    let initial_sources = serde_json::json!({
        "PkgA/package.mo": "within ; package PkgA end PkgA;",
        "PkgA/A.mo": "within PkgA; model A Real x; equation der(x)=1; end A;",
        "PkgB/package.mo": "within ; package PkgB end PkgB;",
        "PkgB/B.mo": "within PkgB; model B Real y; equation der(y)=1; end B;",
    })
    .to_string();
    sync_project_sources(&initial_sources).expect("initial project sync should succeed");

    let source = r#"
    model UsesBoth
      import PkgA.A;
      import PkgB.B;
      A a;
      B b;
    end UsesBoth;
    "#;
    let initial_diagnostics = lsp_diagnostics(source).expect("initial diagnostics should succeed");
    let initial_diagnostics: Vec<lsp_types::Diagnostic> =
        serde_json::from_str(&initial_diagnostics).expect("diagnostics JSON should decode");
    assert!(
        initial_diagnostics.is_empty(),
        "both package roots should resolve before removal, got: {initial_diagnostics:?}"
    );

    let trimmed_sources = serde_json::json!({
        "PkgA/package.mo": "within ; package PkgA end PkgA;",
        "PkgA/A.mo": "within PkgA; model A Real x; equation der(x)=1; end A;",
    })
    .to_string();
    sync_project_sources(&trimmed_sources).expect("trimmed project sync should succeed");

    let trimmed_diagnostics = lsp_diagnostics(source).expect("trimmed diagnostics should succeed");
    let trimmed_diagnostics: Vec<lsp_types::Diagnostic> =
        serde_json::from_str(&trimmed_diagnostics).expect("diagnostics JSON should decode");
    assert!(
        trimmed_diagnostics.iter().any(|diag| {
            let message = diag.message.to_ascii_lowercase();
            message.contains("pkgb") || message.contains("unresolved import")
        }),
        "removed package root should stop resolving after sync, got: {trimmed_diagnostics:?}"
    );

    clear_source_root_cache();
}
