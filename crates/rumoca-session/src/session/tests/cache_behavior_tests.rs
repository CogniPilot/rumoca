#[test]
fn test_update_document_keeps_last_successful_parse_for_semantic_features() {
    let mut session = Session::default();
    let valid = r#"
        model Ball
          Real x(start=0);
          Real v(start=1);
        equation
          der(x) = v;
          der(v) = -9.81;
        end Ball;
        "#;
    let invalid = r#"
        model Ball
          Real x(start=0);
          Real v(start=1);
        equation
          der(x) = v;
          der(v) = -9.81;
          de
        end Ball;
        "#;

    let first_err = session.update_document("input.mo", valid);
    assert!(first_err.is_none(), "valid source should parse");

    // Build and cache resolved state from the valid source.
    let models = session.model_names().expect("model_names should resolve");
    assert!(models.iter().any(|name| name == "Ball"));
    assert!(session.has_resolved_cached(), "resolved cache should exist");

    let parse_err = session.update_document("input.mo", invalid);
    assert!(
        parse_err.is_some(),
        "invalid source should produce parse error"
    );
    assert!(
        session
            .get_document("input.mo")
            .and_then(|doc| doc.parsed())
            .is_some(),
        "last successful parse should be retained for semantic features"
    );
    assert!(
        session.has_resolved_cached(),
        "resolved cache should be retained while source is temporarily invalid"
    );

    // Compile paths must still fail while parse errors are present.
    assert!(
        session.compile_model_phases("Ball").is_err(),
        "compile should fail while parse error exists"
    );
}

#[test]
fn test_replace_parsed_source_set_excludes_active_document() {
    let mut session = Session::default();

    let lib_src = "package Lib model M Real x; equation der(x)=1; end M; end Lib;";
    let parsed = rumoca_phase_parse::parse_to_ast(lib_src, "lib.mo").expect("parse library");
    let inserted = session.replace_parsed_source_set(
        "library::lib",
        SourceRootKind::Library,
        vec![("lib.mo".to_string(), parsed)],
        Some("lib.mo"),
    );
    assert_eq!(inserted, 0, "active document path should be excluded");
    assert!(
        session.get_document("lib.mo").is_none(),
        "excluded document must not be inserted from source-set"
    );
}

use super::*;

use std::collections::HashSet;

fn namespace_class_names(session: &mut Session) -> Vec<String> {
    let mut stack = vec![String::new()];
    let mut seen = HashSet::new();
    let mut names = Vec::new();

    while let Some(prefix) = stack.pop() {
        let entries = session
            .namespace_index_query(&prefix)
            .expect("query namespace completion cache");
        for (_child, full_name, has_children) in entries {
            if !seen.insert(full_name.clone()) {
                continue;
            }
            names.push(full_name.clone());
            if has_children {
                stack.push(format!("{full_name}."));
            }
        }
    }

    names.sort_unstable();
    names
}

fn workspace_symbol_name_uris(session: &mut Session) -> Vec<(String, String)> {
    session
        .workspace_symbol_query("")
        .into_iter()
        .map(|symbol| (symbol.name, symbol.uri))
        .collect()
}

fn assert_namespace_source_set_signature(
    session: &Session,
    source_set_id: SourceSetId,
    expected_signature: &SourceSetQuerySignature,
) {
    let cache = session
        .query_state
        .ast
        .library_namespace_cache
        .as_ref()
        .expect("library namespace cache should be present");
    let membership = &session.query_state.ast.package_def_map;

    assert_eq!(
        &membership
            .source_set_caches
            .get(&source_set_id)
            .expect("source-set membership cache should exist")
            .signature,
        expected_signature,
        "source-set membership cache should retain the expected signature"
    );
    assert_eq!(
        cache
            .merged_source_set_signatures
            .get(&source_set_id)
            .expect("merged source-set signature should exist"),
        expected_signature,
        "merged source-set signature should retain the expected value"
    );
}

fn assert_namespace_source_set_invalidated(session: &Session, source_set_id: SourceSetId) {
    let cache = session
        .query_state
        .ast
        .library_namespace_cache
        .as_ref()
        .expect("library namespace cache should be present");
    let membership = &session.query_state.ast.package_def_map;

    assert!(
        !membership.source_set_caches.contains_key(&source_set_id),
        "source-set membership cache should be invalidated"
    );
    assert!(
        !cache
            .merged_source_set_signatures
            .contains_key(&source_set_id),
        "merged source-set signature should be invalidated"
    );
}

fn file_summary_cache_fingerprints(
    session: &Session,
    file_id: FileId,
) -> (Fingerprint, Fingerprint, Fingerprint) {
    let declaration = session
        .query_state
        .ast
        .declaration_index_cache
        .get(&file_id)
        .expect("declaration cache should exist")
        .fingerprint;
    let class_interface = session
        .query_state
        .ast
        .class_interface_query_cache
        .get(&file_id)
        .expect("class interface cache should exist")
        .fingerprint;
    let file_item = session
        .query_state
        .ast
        .file_item_index_cache
        .get(&file_id)
        .expect("file item cache should exist")
        .fingerprint;
    (declaration, class_interface, file_item)
}

fn library_source_set_signature(
    session: &Session,
    source_set_id: SourceSetId,
) -> SourceSetQuerySignature {
    session
        .query_state
        .ast
        .library_namespace_cache
        .as_ref()
        .expect("library namespace cache should be initialized")
        .merged_source_set_signatures
        .get(&source_set_id)
        .cloned()
        .expect("merged source-set signature should be present")
}

fn workspace_symbol_signature(session: &Session) -> SessionQuerySignature {
    session
        .query_state
        .ast
        .workspace_symbol_query_cache
        .as_ref()
        .expect("workspace symbol cache should exist")
        .signature
        .clone()
}

fn load_two_library_source_sets(
    session: &mut Session,
) -> (
    SourceSetId,
    SourceSetId,
    SourceSetQuerySignature,
    SourceSetQuerySignature,
) {
    let package_a = parse_definition("package A\n  model MA\n  end MA;\nend A;\n", "A/package.mo");
    let package_b = parse_definition("package B\n  model MB\n  end MB;\nend B;\n", "B/package.mo");

    session.replace_parsed_source_set(
        "library::A",
        SourceRootKind::Library,
        vec![("A/package.mo".to_string(), package_a)],
        None,
    );
    session.replace_parsed_source_set(
        "library::B",
        SourceRootKind::Library,
        vec![("B/package.mo".to_string(), package_b)],
        None,
    );

    session
        .namespace_index_query("")
        .expect("build namespace cache");

    let source_set_a_id = session
        .source_set_id("library::A")
        .expect("A source set should exist");
    let source_set_b_id = session
        .source_set_id("library::B")
        .expect("B source set should exist");
    let signature_a = library_source_set_signature(session, source_set_a_id);
    let signature_b = library_source_set_signature(session, source_set_b_id);

    (source_set_a_id, source_set_b_id, signature_a, signature_b)
}

#[test]
fn library_completion_cache_survives_local_document_edits() {
    let mut session = Session::default();

    let lib_src = r#"
        package Modelica
          package Electrical
            package Analog
              model Resistor
                Real v;
              equation
                der(v) = 1;
              end Resistor;
            end Analog;
          end Electrical;
        end Modelica;
    "#;
    let parsed =
        rumoca_phase_parse::parse_to_ast(lib_src, "Modelica/package.mo").expect("parse library");
    let inserted = session.replace_parsed_source_set(
        "library::Modelica",
        SourceRootKind::Library,
        vec![("Modelica/package.mo".to_string(), parsed)],
        None,
    );
    assert_eq!(inserted, 1, "library source-set should be inserted");

    session
        .add_document("local.mo", "model Local\n  import Modelica;\nend Local;\n")
        .expect("add local document");

    let first = namespace_class_names(&mut session);
    assert!(
        first
            .iter()
            .any(|name| name == "Modelica.Electrical.Analog.Resistor"),
        "library completion cache should include nested library classes"
    );
    assert!(
        session.query_state.ast.library_namespace_cache.is_some(),
        "library completion cache should be populated"
    );
    let namespace_before = session
        .namespace_index_query("Modelica.")
        .expect("cache namespace children");
    let fingerprint_before = session
        .library_namespace_fingerprint_cached("Modelica.")
        .expect("namespace fingerprint should be cached");

    session.update_document(
        "local.mo",
        "model Local\n  import Modelica.Electrical;\n  Real x;\nend Local;\n",
    );

    assert!(
        session.query_state.ast.library_namespace_cache.is_some(),
        "editing a local document should not invalidate library completion cache"
    );
    let second = namespace_class_names(&mut session);
    assert_eq!(
        first, second,
        "library completion cache should survive local document edits"
    );
    assert_eq!(
        namespace_before,
        session
            .namespace_index_query("Modelica.")
            .expect("cache namespace children after local edit"),
        "namespace children should survive local document edits"
    );
    assert_eq!(
        Some(fingerprint_before),
        session.library_namespace_fingerprint_cached("Modelica."),
        "namespace closure fingerprint should be stable across unrelated local edits"
    );
}

#[test]
fn library_completion_cache_collects_from_parsed_library_docs() {
    let mut session = Session::default();

    let package_src = r#"
        package Modelica
          package Blocks
            package Continuous
              block PID
                parameter Real k = 1;
              end PID;
            end Continuous;
          end Blocks;
        end Modelica;
    "#;
    let package_parsed = rumoca_phase_parse::parse_to_ast(package_src, "Modelica/package.mo")
        .expect("parse library package");
    let complex_src = r#"
        record Complex
          Real re;
          Real im;
        end Complex;
    "#;
    let complex_parsed =
        rumoca_phase_parse::parse_to_ast(complex_src, "Complex.mo").expect("parse Complex");

    let inserted = session.replace_parsed_source_set(
        "library::Modelica",
        SourceRootKind::Library,
        vec![
            ("Modelica/package.mo".to_string(), package_parsed),
            ("Complex.mo".to_string(), complex_parsed),
        ],
        None,
    );
    assert_eq!(inserted, 2, "library source-set should be inserted");

    let class_names = namespace_class_names(&mut session);
    assert!(
        class_names
            .iter()
            .any(|name| name == "Modelica.Blocks.Continuous.PID"),
        "expected nested package class in completion cache: {class_names:?}"
    );
    assert!(
        class_names.iter().any(|name| name == "Complex"),
        "expected standalone library record in completion cache: {class_names:?}"
    );
    assert!(
        session.query_state.ast.library_namespace_cache.is_some(),
        "library completion cache should be populated"
    );
    assert!(
        session.query_state.ast.declaration_index_cache.is_empty(),
        "library namespace priming should not eagerly build full declaration indexes"
    );
    let namespace_children = session
        .namespace_index_query("Modelica.Blocks.")
        .expect("cache namespace children");
    assert_eq!(
        namespace_children,
        vec![(
            "Continuous".to_string(),
            "Modelica.Blocks.Continuous".to_string(),
            true,
        )],
        "namespace cache should return immediate children only"
    );
}

#[test]
fn library_completion_cache_collects_names_from_split_within_documents() {
    let mut session = Session::default();

    let package_src = r#"
        package Modelica
          package Blocks
            package Continuous
            end Continuous;
          end Blocks;
        end Modelica;
    "#;
    let package_parsed = rumoca_phase_parse::parse_to_ast(package_src, "Modelica/package.mo")
        .expect("parse library package");
    let pid_src = r#"
        within Modelica.Blocks.Continuous;
        block PID
          parameter Real k = 1;
        end PID;
    "#;
    let pid_parsed = rumoca_phase_parse::parse_to_ast(pid_src, "Modelica/Blocks/Continuous/PID.mo")
        .expect("parse split library document");

    let inserted = session.replace_parsed_source_set(
        "library::Modelica",
        SourceRootKind::Library,
        vec![
            ("Modelica/package.mo".to_string(), package_parsed),
            ("Modelica/Blocks/Continuous/PID.mo".to_string(), pid_parsed),
        ],
        None,
    );
    assert_eq!(inserted, 2, "library source-set should be inserted");

    let class_names = namespace_class_names(&mut session);
    assert!(
        class_names
            .iter()
            .any(|name| name == "Modelica.Blocks.Continuous.PID"),
        "expected split within-document class in completion cache: {class_names:?}"
    );
}

#[test]
fn body_only_edits_keep_summary_backed_query_caches_warm() {
    let mut session = Session::default();
    session
        .add_document("m.mo", "model M\n  Real x;\nequation\n  x = 1;\nend M;\n")
        .expect("initial source should parse");

    let file_id = session.file_id("m.mo").expect("file id should exist");
    let summary_before = session
        .get_document("m.mo")
        .expect("document should exist")
        .summary_fingerprint();
    let body_before = session
        .get_document("m.mo")
        .expect("document should exist")
        .body_fingerprint();

    session
        .declaration_index_query("m.mo")
        .expect("declaration index should build");
    session
        .class_interface_index_query("m.mo")
        .expect("class interface query should build");
    let _ = session.file_item_index_query("m.mo");
    let workspace_before = workspace_symbol_name_uris(&mut session);
    let (declaration_before, class_interface_before, file_item_before) =
        file_summary_cache_fingerprints(&session, file_id);
    let workspace_signature_before = workspace_symbol_signature(&session);

    let parse_error = session.update_document(
        "m.mo",
        "model M\n  Real x;\nequation\n  x = 2;\n  x = 3;\nend M;\n",
    );
    assert!(parse_error.is_none(), "body-only edit should remain valid");

    let document = session
        .get_document("m.mo")
        .expect("updated document should exist");
    assert_eq!(
        document.summary_fingerprint(),
        summary_before,
        "body-only edits should keep the summary fingerprint stable"
    );
    assert_ne!(
        document.body_fingerprint(),
        body_before,
        "body-only edits should change the body fingerprint"
    );

    session
        .declaration_index_query("m.mo")
        .expect("declaration index should still resolve");
    session
        .class_interface_index_query("m.mo")
        .expect("class interface query should still resolve");
    let _ = session.file_item_index_query("m.mo");
    let workspace_after = workspace_symbol_name_uris(&mut session);
    let (declaration_after, class_interface_after, file_item_after) =
        file_summary_cache_fingerprints(&session, file_id);
    let workspace_signature_after = workspace_symbol_signature(&session);

    assert_eq!(
        declaration_after, declaration_before,
        "body-only edits should keep the declaration index cache key warm"
    );
    assert_eq!(
        class_interface_after, class_interface_before,
        "body-only edits should keep the class interface cache key warm"
    );
    assert_eq!(
        file_item_after, file_item_before,
        "body-only edits should keep the per-file workspace symbol cache key warm"
    );
    assert_eq!(
        workspace_signature_after, workspace_signature_before,
        "body-only edits should keep the global workspace symbol signature warm"
    );
    assert_eq!(
        workspace_after, workspace_before,
        "body-only edits should not perturb summary-backed workspace symbols"
    );
}

#[test]
fn model_key_query_uses_class_interface_index_without_warming_declaration_index() {
    let mut session = Session::default();
    session
        .add_document("m.mo", "within Demo;\nmodel M\n  Real x;\nend M;\n")
        .expect("source should parse");

    let model_key = session
        .model_key_query("Demo.M")
        .expect("model key should resolve from class interface");
    assert_eq!(
        model_key.qualified_name(),
        "Demo.M",
        "model key should preserve the fully qualified class name"
    );
    assert!(
        session.query_state.ast.declaration_index_cache.is_empty(),
        "model key lookup should not need declaration index warming"
    );

    let file_id = session.file_id("m.mo").expect("file id should exist");
    assert!(
        session
            .query_state
            .ast
            .class_interface_query_cache
            .contains_key(&file_id),
        "model key lookup should warm the class interface cache"
    );
}

#[test]
fn add_document_invalidates_library_completion_cache_for_library_backed_uri() {
    let mut session = Session::default();

    let lib_src = r#"
        package Modelica
          model Resistor
            Real v;
          equation
            der(v) = 1;
          end Resistor;
        end Modelica;
    "#;
    let parsed =
        rumoca_phase_parse::parse_to_ast(lib_src, "Modelica/package.mo").expect("parse library");
    let inserted = session.replace_parsed_source_set(
        "library::Modelica",
        SourceRootKind::Library,
        vec![("Modelica/package.mo".to_string(), parsed)],
        None,
    );
    assert_eq!(inserted, 1, "library source-set should be inserted");

    let class_names = namespace_class_names(&mut session);
    assert!(
        class_names.iter().any(|name| name == "Modelica.Resistor"),
        "expected library class in completion cache: {class_names:?}"
    );
    assert!(
        session.query_state.ast.library_namespace_cache.is_some(),
        "library completion cache should be populated"
    );
    let fingerprint_before = session
        .library_namespace_fingerprint_cached("Modelica.")
        .expect("Modelica namespace fingerprint should be cached");

    session
        .add_document(
            "Modelica/package.mo",
            "package Modelica\n  model Local\n  end Local;\nend Modelica;\n",
        )
        .expect("replace library-backed uri with editor document");

    assert!(
        session.query_state.ast.library_namespace_cache.is_some(),
        "opening a library-backed uri should keep the namespace cache resident until the next query"
    );
    let updated_names = namespace_class_names(&mut session);
    assert!(
        updated_names.iter().all(|name| name != "Modelica.Resistor"),
        "the rebuilt namespace cache should drop the replaced library class"
    );
    assert_ne!(
        session.library_namespace_fingerprint_cached("Modelica."),
        Some(fingerprint_before),
        "the namespace fingerprint should change after replacing the live library document"
    );
    assert!(
        updated_names.iter().all(|name| name != "Modelica.Local"),
        "detached live library documents should not be treated as library namespace members"
    );
}

#[test]
fn library_namespace_fingerprint_ignores_unrelated_library_root_changes() {
    let mut session = Session::default();

    let lib_src = r#"
        package Lib
          package Electrical
            model Resistor
              Real v;
            equation
              der(v) = 1;
            end Resistor;
          end Electrical;
        end Lib;
    "#;
    let other_v1 = r#"
        package Other
          model A
          end A;
        end Other;
    "#;
    let other_v2 = r#"
        package Other
          model B
          end B;
        end Other;
    "#;
    let lib_parsed =
        rumoca_phase_parse::parse_to_ast(lib_src, "Lib/package.mo").expect("parse Lib");
    let other_parsed_v1 =
        rumoca_phase_parse::parse_to_ast(other_v1, "Other/package.mo").expect("parse Other v1");
    let other_parsed_v2 =
        rumoca_phase_parse::parse_to_ast(other_v2, "Other/package.mo").expect("parse Other v2");

    assert_eq!(
        session.replace_parsed_source_set(
            "library::Lib",
            SourceRootKind::Library,
            vec![("Lib/package.mo".to_string(), lib_parsed)],
            None,
        ),
        1
    );
    assert_eq!(
        session.replace_parsed_source_set(
            "library::Other",
            SourceRootKind::Library,
            vec![("Other/package.mo".to_string(), other_parsed_v1)],
            None,
        ),
        1
    );

    session
        .namespace_index_query("")
        .expect("prime namespace cache");
    let before = session
        .library_namespace_fingerprint_cached("Lib.")
        .expect("Lib namespace fingerprint");

    assert_eq!(
        session.replace_parsed_source_set(
            "library::Other",
            SourceRootKind::Library,
            vec![("Other/package.mo".to_string(), other_parsed_v2)],
            None,
        ),
        1
    );
    session
        .namespace_index_query("")
        .expect("rebuild namespace cache");
    let after = session
        .library_namespace_fingerprint_cached("Lib.")
        .expect("Lib namespace fingerprint after rebuild");

    assert_eq!(
        before, after,
        "unrelated library root changes should not perturb Lib namespace closure fingerprint"
    );
}

#[test]
fn source_set_scoped_invalidation_keeps_other_namespace_cache_entries_warm() {
    let mut session = Session::default();
    let (source_set_a, source_set_b, a_signature_before, b_signature_before) =
        load_two_library_source_sets(&mut session);
    let parsed_b_v2 = parse_definition(
        "package B\n  model MB\n  end MB;\n  model MB2\n  end MB2;\nend B;\n",
        "B/package.mo",
    );
    let replaced = session.replace_parsed_source_set(
        "library::B",
        SourceRootKind::Library,
        vec![("B/package.mo".to_string(), parsed_b_v2)],
        None,
    );
    let cache_after_replace = session
        .query_state
        .ast
        .library_namespace_cache
        .as_ref()
        .expect("cache should remain after scoped invalidation");

    assert_eq!(replaced, 1, "B replacement should update one document");
    assert_namespace_source_set_signature(&session, source_set_a, &a_signature_before);
    assert_namespace_source_set_invalidated(&session, source_set_b);
    assert!(
        cache_after_replace.merged_cache.is_none(),
        "merged cache must rebuild lazily"
    );

    let class_names = namespace_class_names(&mut session);
    assert!(
        class_names.contains(&"B.MB2".to_string()),
        "updated B cache should include MB2"
    );

    let cache_after_rebuild = session
        .query_state
        .ast
        .library_namespace_cache
        .as_ref()
        .expect("library namespace cache should be present");

    assert_namespace_source_set_signature(&session, source_set_a, &a_signature_before);
    assert!(
        session
            .query_state
            .ast
            .package_def_map
            .source_set_caches
            .get(&source_set_b)
            .is_some_and(|entry| entry.signature != b_signature_before),
        "B source-set membership cache should be rebuilt"
    );
    assert!(
        cache_after_rebuild
            .merged_source_set_signatures
            .get(&source_set_a)
            .is_some_and(|signature| *signature == a_signature_before),
        "A merged source-set signature should be reused"
    );
    assert!(
        cache_after_rebuild
            .merged_source_set_signatures
            .get(&source_set_b)
            .is_some_and(|signature| *signature != b_signature_before),
        "B merged source-set signature should be rebuilt"
    );
    assert!(
        cache_after_rebuild.merged_cache.is_some(),
        "merged cache should be rebuilt"
    );
}

#[test]
fn test_index_library_tolerant_loads_valid_library_source_set() {
    let temp = tempfile::tempdir().expect("tempdir");
    let lib_dir = temp.path().join("lib");
    std::fs::create_dir_all(&lib_dir).expect("mkdir");
    std::fs::write(
        lib_dir.join("package.mo"),
        "package Lib model M Real x; equation der(x)=1; end M; end Lib;",
    )
    .expect("write package");

    let mut session = Session::default();
    let report =
        session.index_library_tolerant("library::lib", SourceRootKind::Library, &lib_dir, None);
    assert!(
        report.diagnostics.is_empty(),
        "valid library indexing should not emit diagnostics: {:?}",
        report.diagnostics
    );
    assert_eq!(report.source_set_id, "library::lib");
    assert_eq!(report.indexed_file_count, 1);
    assert_eq!(report.inserted_file_count, 1);
    assert!(
        report.cache_status.is_some(),
        "cache status should be reported on successful indexing"
    );
    assert_eq!(session.document_uris().len(), 1);
}

#[test]
fn test_index_library_tolerant_reports_parse_failure_without_inserting_docs() {
    let temp = tempfile::tempdir().expect("tempdir");
    let lib_dir = temp.path().join("lib");
    std::fs::create_dir_all(&lib_dir).expect("mkdir");
    std::fs::write(lib_dir.join("Broken.mo"), "model Broken Real x end Broken;")
        .expect("write broken file");

    let mut session = Session::default();
    let report =
        session.index_library_tolerant("library::broken", SourceRootKind::Library, &lib_dir, None);
    assert_eq!(report.source_set_id, "library::broken");
    assert_eq!(report.indexed_file_count, 0);
    assert_eq!(report.inserted_file_count, 0);
    assert!(
        !report.diagnostics.is_empty(),
        "parse failure should be surfaced in tolerant indexing report"
    );
    assert!(session.document_uris().is_empty());
}

#[test]
fn test_compile_model_phases_uses_cache_until_session_invalidated() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            "model M Real x(start=0); equation der(x) = 1; end M;",
        )
        .expect("test setup should parse");

    let first = session
        .compile_model_phases("M")
        .expect("first compile should run");
    assert!(matches!(first, PhaseResult::Success(_)));
    let cache_entry = session
        .query_state
        .dae
        .compile_results
        .get_mut("M")
        .expect("M should have compile cache entry after first compile");
    cache_entry.result = PhaseResult::NeedsInner {
        missing_inners: vec!["cached-M".to_string()],
    };

    let second = session
        .compile_model_phases("M")
        .expect("second compile should use cache");
    match second {
        PhaseResult::NeedsInner { missing_inners } => {
            assert_eq!(missing_inners, vec!["cached-M".to_string()]);
        }
        other => panic!("expected cached result on second compile, got {other:?}"),
    }

    let parse_err = session.update_document(
        "test.mo",
        "model M Real x(start=0); equation der(x) = 2; end M;",
    );
    assert!(
        parse_err.is_none(),
        "valid update should not return parse error"
    );

    let third = session
        .compile_model_phases("M")
        .expect("third compile should run after invalidation");
    assert!(
        matches!(third, PhaseResult::Success(_)),
        "cache should invalidate after document update"
    );
}

#[test]
fn test_compile_cache_survives_unrelated_document_update() {
    let mut session = Session::default();
    session
        .add_document(
            "a.mo",
            r#"
            model A
              Real x(start=0);
            equation
              der(x) = 1;
            end A;
            "#,
        )
        .expect("A should parse");
    session
        .add_document(
            "b.mo",
            r#"
            model B
              Real y(start=0);
            equation
              der(y) = 2;
            end B;
            "#,
        )
        .expect("B should parse");

    let _ = session
        .compile_model_phases("A")
        .expect("first compile should run");
    let cache_entry = session
        .query_state
        .dae
        .compile_results
        .get_mut("A")
        .expect("A should have compile cache entry after first compile");
    cache_entry.result = PhaseResult::NeedsInner {
        missing_inners: vec!["cached-A".to_string()],
    };

    let b_update_err = session.update_document(
        "b.mo",
        r#"
            model B
              Real y(start=0);
            equation
              der(y) = 3;
            end B;
            "#,
    );
    assert!(b_update_err.is_none(), "B update should remain valid");

    let second = session
        .compile_model_phases("A")
        .expect("A should still compile after unrelated edit");
    match second {
        PhaseResult::NeedsInner { missing_inners } => {
            assert_eq!(missing_inners, vec!["cached-A".to_string()]);
        }
        other => panic!("expected cached result after unrelated edit, got {other:?}"),
    }
}

#[test]
fn test_compile_cache_invalidates_when_dependency_changes() {
    let mut session = Session::default();
    session
        .add_document(
            "base.mo",
            r#"
            model Base
              Real x(start=0);
            equation
              der(x) = 1;
            end Base;
            "#,
        )
        .expect("Base should parse");
    session
        .add_document(
            "child.mo",
            r#"
            model Child
              Base base;
              Real y(start=0);
            equation
              der(y) = base.x;
            end Child;
            "#,
        )
        .expect("Child should parse");

    let _ = session
        .compile_model_phases("Child")
        .expect("first Child compile should run");
    let cache_entry = session
        .query_state
        .dae
        .compile_results
        .get_mut("Child")
        .expect("Child should have compile cache entry after first compile");
    cache_entry.result = PhaseResult::NeedsInner {
        missing_inners: vec!["cached-Child".to_string()],
    };

    let base_update_err = session.update_document(
        "base.mo",
        r#"
            model Base
              Real x(start=0);
            equation
              der(x) = 5;
            end Base;
            "#,
    );
    assert!(base_update_err.is_none(), "Base update should remain valid");

    let second = session
        .compile_model_phases("Child")
        .expect("Child should recompile after Base changed");
    assert!(
        matches!(second, PhaseResult::Success(_)),
        "Child cache must invalidate when dependency Base changes"
    );
}

#[test]
fn test_compile_models_parallel_reuses_cache() {
    let mut session = Session::default();
    session
        .add_document(
            "models.mo",
            r#"
            model A
              Real x(start=0);
            equation
              der(x) = 1;
            end A;

            model B
              Real y(start=0);
            equation
              der(y) = 2;
            end B;
            "#,
        )
        .expect("models should parse");

    let _first = session
        .compile_models_parallel(&["A", "B"])
        .expect("first parallel compile should run");
    session
        .query_state
        .dae
        .compile_results
        .get_mut("A")
        .expect("A cache entry")
        .result = PhaseResult::NeedsInner {
        missing_inners: vec!["cached-A".to_string()],
    };
    session
        .query_state
        .dae
        .compile_results
        .get_mut("B")
        .expect("B cache entry")
        .result = PhaseResult::NeedsInner {
        missing_inners: vec!["cached-B".to_string()],
    };

    let second = session
        .compile_models_parallel(&["A", "B"])
        .expect("second parallel compile should hit cache");
    assert_eq!(second.len(), 2);
    match &second[0].1 {
        PhaseResult::NeedsInner { missing_inners } => {
            assert_eq!(missing_inners, &vec!["cached-A".to_string()])
        }
        other => panic!("expected cached A result, got {other:?}"),
    }
    match &second[1].1 {
        PhaseResult::NeedsInner { missing_inners } => {
            assert_eq!(missing_inners, &vec!["cached-B".to_string()])
        }
        other => panic!("expected cached B result, got {other:?}"),
    }
}

#[test]
fn test_compile_model_strict_reachable_with_recovery_reuses_cache() {
    let mut session = Session::default();
    session
        .add_document(
            "pkg.mo",
            r#"
            package P
              model A
                Real x(start=0);
              equation
                der(x) = 1;
              end A;

              model B
                Real y(start=0);
              equation
                der(y) = 2;
              end B;
            end P;
            "#,
        )
        .expect("package should parse");

    let first = session.compile_model_strict_reachable_with_recovery("P.A");
    assert!(first.requested_succeeded(), "P.A should compile");
    session
        .query_state
        .dae
        .compile_results
        .get_mut("P.A")
        .expect("P.A cache entry")
        .result = PhaseResult::NeedsInner {
        missing_inners: vec!["cached-P.A".to_string()],
    };

    let second = session.compile_model_strict_reachable_with_recovery("P.A");
    assert!(
        !second.requested_succeeded(),
        "requested result should come from cache override"
    );
    match second.requested_result {
        Some(PhaseResult::NeedsInner { missing_inners }) => {
            assert_eq!(missing_inners, vec!["cached-P.A".to_string()]);
        }
        other => panic!("expected cached strict requested result, got {other:?}"),
    }
}

#[test]
fn test_compile_model_strict_reachable_uncached_with_recovery_ignores_cache() {
    let mut session = Session::default();
    session
        .add_document(
            "pkg.mo",
            r#"
            package P
              model A
                Real x(start=0);
              equation
                der(x) = 1;
              end A;
            end P;
            "#,
        )
        .expect("package should parse");

    let first = session.compile_model_strict_reachable_uncached_with_recovery("P.A");
    assert!(first.requested_succeeded(), "P.A should compile");

    session.query_state.dae.compile_results.insert(
        "P.A".to_string(),
        CompileCacheEntry {
            fingerprint: [123; 32],
            result: PhaseResult::NeedsInner {
                missing_inners: vec!["cached-P.A".to_string()],
            },
        },
    );

    let second = session.compile_model_strict_reachable_uncached_with_recovery("P.A");
    assert!(
        second.requested_succeeded(),
        "uncached strict compile should ignore cache override"
    );
    assert!(
        matches!(second.requested_result, Some(PhaseResult::Success(_))),
        "expected uncached strict requested result to compile successfully, got {:?}",
        second.requested_result
    );
}

#[test]
fn query_methods_fallback_to_partial_parse_and_invalidate_on_update() {
    let mut session = Session::default();
    session
        .add_document(
            "active.mo",
            r#"
            model M
              Real x(start=0);
            end M;
            "#,
        )
        .expect("valid source should parse");

    let parsed = session.parsed_file_query("active.mo");
    assert!(parsed.is_some(), "parsed query should return valid AST");
    let recovered = session.recovered_file_query("active.mo");
    assert!(
        recovered.is_some(),
        "recovered query should also return valid AST"
    );
    let symbols = session.file_item_index_query("active.mo");
    assert!(
        symbols.iter().any(|symbol| symbol.name == "M"),
        "file index should include model"
    );

    assert!(
        session
            .update_document("active.mo", "model M\n  Real x(start=0);\n")
            .is_some(),
        "invalid update should return parse error"
    );
    assert!(
        session.recovered_file_query("active.mo").is_some(),
        "recovered query should preserve last parse on syntax errors"
    );
    assert!(
        session.parsed_file_query("active.mo").is_some(),
        "parsed query should still expose fallback parse on syntax errors"
    );
    let symbols_after_error = session.workspace_symbol_query("M");
    assert!(
        !symbols_after_error.is_empty(),
        "workspace symbols should still be available while typing errors"
    );

    let updated = session.update_document("active.mo", "model M\n  Real y;\nend M;\n");
    assert!(updated.is_none(), "valid recovery update should succeed");
    assert!(
        session
            .workspace_symbol_query("y")
            .iter()
            .any(|symbol| symbol.name == "y"),
        "workspace symbols should reflect recovered content after successful update"
    );
}
