//! Compile-unit source collection: a simulation compile must see the focused
//! document plus its same-directory siblings and durable libraries, and nothing
//! else the editor happens to have open.

use super::*;

#[test]
fn collect_simulation_parsed_docs_snapshot_keeps_focus_and_libraries_only() {
    let focus_uri = "focus.mo";
    let other_uri = "other.mo";
    let source_root_uri = "/opt/msl/Modelica/package.mo";
    let source = "model M end M;";

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(focus_uri, source)
        .expect("focus should parse");
    session
        .add_document(other_uri, source)
        .expect("other should parse");
    session.replace_parsed_source_set(
        "source-root::/opt/msl",
        SourceRootKind::DurableExternal,
        vec![(
            source_root_uri.to_string(),
            ast::StoredDefinition::default(),
        )],
        None,
    );

    let snapshot = session.snapshot();
    let focus_key = canonical_path_key(focus_uri);
    let docs = collect_simulation_parsed_docs_snapshot(&snapshot, focus_uri, &focus_key)
        .expect("docs should build");
    let uris: HashSet<String> = docs.into_iter().map(|(uri, _)| uri).collect();

    assert!(uris.contains(focus_uri), "focus doc must be included");
    assert!(
        uris.contains(source_root_uri),
        "source-root docs must be included"
    );
    assert!(
        !uris.contains(other_uri),
        "non-focus workspace docs must be excluded"
    );
}

#[test]
fn collect_local_compile_unit_sources_loads_same_directory_siblings() {
    let temp = new_temp_dir("local-compile-unit");
    let focus = temp.join("Root.mo");
    let sibling = temp.join("Helper.mo");
    std::fs::write(&focus, "model Root\n  Helper h;\nend Root;\n").expect("write focus");
    std::fs::write(
        &sibling,
        "model Helper\n  Real x(start=0);\nequation\n  der(x) = 1;\nend Helper;\n",
    )
    .expect("write sibling");

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            &focus.to_string_lossy(),
            &std::fs::read_to_string(&focus).expect("read"),
        )
        .expect("focus should parse");

    let snapshot = session.snapshot();
    let docs = collect_local_compile_unit_sources_snapshot(&snapshot, &focus.to_string_lossy())
        .expect("local compile unit docs should load");
    let uris: HashSet<String> = docs.into_iter().map(|(uri, _)| uri).collect();

    assert!(
        uris.contains(&focus.to_string_lossy().to_string()),
        "focus document must be included"
    );
    assert!(
        uris.contains(&sibling.to_string_lossy().to_string()),
        "same-directory sibling must be included"
    );
}

#[test]
fn collect_local_compile_unit_sources_keep_unrelated_syntax_errors_as_sources() {
    let temp = new_temp_dir("local-compile-unit-errors");
    let focus = temp.join("Root.mo");
    let sibling = temp.join("Helper.mo");
    let broken = temp.join("Broken.mo");
    std::fs::write(&focus, "model Root\n  Helper h;\nend Root;\n").expect("write focus");
    std::fs::write(
        &sibling,
        "model Helper\n  Real x(start=0);\nequation\n  der(x) = 1;\nend Helper;\n",
    )
    .expect("write sibling");
    std::fs::write(&broken, "model Broken\n  Real x\nend Broken;\n").expect("write broken");

    let mut session = Session::new(SessionConfig::default());
    session.update_document(
        &focus.to_string_lossy(),
        &std::fs::read_to_string(&focus).expect("read"),
    );

    let snapshot = session.snapshot();
    let docs = collect_local_compile_unit_sources_snapshot(&snapshot, &focus.to_string_lossy())
        .expect("local compile unit sources should load");
    let uris: HashSet<String> = docs.into_iter().map(|(uri, _)| uri).collect();

    assert!(uris.contains(&broken.to_string_lossy().to_string()));
}

#[test]
fn compile_model_for_simulation_loads_same_directory_siblings_from_disk() {
    run_async_test(async {
        let temp = new_temp_dir("compile-siblings");
        let focus = temp.join("Root.mo");
        let sibling = temp.join("Helper.mo");
        std::fs::write(&focus, "model Root\n  Helper h;\nend Root;\n").expect("write focus");
        std::fs::write(
            &sibling,
            "model Helper\n  Real x(start=0);\nequation\n  der(x) = 1;\nend Helper;\n",
        )
        .expect("write sibling");

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session
                .add_document(
                    &focus.to_string_lossy(),
                    &std::fs::read_to_string(&focus).expect("read focus"),
                )
                .expect("focus should parse");
        }

        let compiled = server
            .compile_model_for_simulation("Root", &focus.to_string_lossy())
            .await
            .expect("compile with sibling file should succeed");
        assert_eq!(
            checked_variable_count(&compiled.dae, rumoca_compile::compile::VariableRole::State),
            1
        );
    });
}
