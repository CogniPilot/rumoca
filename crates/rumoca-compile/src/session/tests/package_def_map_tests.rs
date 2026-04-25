use super::*;

fn parse_definition(source: &str, file_name: &str) -> ast::StoredDefinition {
    rumoca_phase_parse::parse_to_ast(source, file_name).expect("test definition should parse")
}

fn single_source_root_definition(path: &str, source: &str) -> Vec<(String, ast::StoredDefinition)> {
    vec![(path.to_string(), parse_definition(source, path))]
}

fn member_item_names(def_map: &PackageDefMap, prefix: &str) -> Vec<String> {
    def_map
        .member_item_keys(prefix)
        .into_iter()
        .map(|item_key| item_key.qualified_name())
        .collect()
}

#[test]
fn source_set_package_def_map_query_collects_split_within_members() {
    let mut session = Session::default();
    let package_src = r#"
        package Modelica
          package Blocks
            package Continuous
            end Continuous;
          end Blocks;
        end Modelica;
    "#;
    let pid_src = r#"
        within Modelica.Blocks.Continuous;
        block PID
          parameter Real k = 1;
        end PID;
    "#;

    session.replace_parsed_source_set(
        "external::Modelica",
        SourceRootKind::External,
        vec![
            (
                "Modelica/package.mo".to_string(),
                parse_definition(package_src, "Modelica/package.mo"),
            ),
            (
                "Modelica/Blocks/Continuous/PID.mo".to_string(),
                parse_definition(pid_src, "Modelica/Blocks/Continuous/PID.mo"),
            ),
        ],
        None,
    );

    let source_set_id = session
        .source_set_id("external::Modelica")
        .expect("external source-root should have a stable id");
    let def_map = session
        .source_set_package_def_map_query(source_set_id)
        .expect("package def map should be built");

    assert_eq!(
        def_map.children("Modelica.Blocks."),
        vec!["Modelica.Blocks.Continuous".to_string()],
        "package def map should preserve direct package containment"
    );
    assert_eq!(
        def_map
            .member_item_keys("Modelica.Blocks.Continuous.")
            .into_iter()
            .map(|item_key| item_key.qualified_name())
            .collect::<Vec<_>>(),
        vec!["Modelica.Blocks.Continuous.PID".to_string()],
        "split within-document members should resolve under their owning package path"
    );
}

#[test]
fn source_set_package_def_map_cache_is_scoped_by_source_set_revision() {
    let mut session = Session::default();
    session.replace_parsed_source_set(
        "external::A",
        SourceRootKind::External,
        single_source_root_definition("A/package.mo", "package A\n  model M\n  end M;\nend A;\n"),
        None,
    );
    session.replace_parsed_source_set(
        "external::B",
        SourceRootKind::External,
        single_source_root_definition("B/package.mo", "package B\n  model N\n  end N;\nend B;\n"),
        None,
    );

    let source_set_a = session
        .source_set_id("external::A")
        .expect("A source-set should exist");
    let source_set_b = session
        .source_set_id("external::B")
        .expect("B source-set should exist");
    let b_members_before = session
        .source_set_package_def_map_query(source_set_b)
        .map(|def_map| member_item_names(def_map, "B."))
        .expect("B package def map should build");
    session
        .source_set_package_def_map_query(source_set_a)
        .expect("A package def map should build");
    let source_set_caches = &session.query_state.ast.package_def_map.source_set_caches;
    let a_signature_before = source_set_caches
        .get(&source_set_a)
        .expect("A package def map cache should be populated")
        .signature
        .clone();

    assert!(
        source_set_caches.contains_key(&source_set_a),
        "A package def map cache should be populated"
    );
    assert!(
        source_set_caches.contains_key(&source_set_b),
        "B package def map cache should be populated"
    );

    session.replace_parsed_source_set(
        "external::A",
        SourceRootKind::External,
        single_source_root_definition(
            "A/package.mo",
            "package A\n  model M\n    Real x;\n  end M;\nend A;\n",
        ),
        None,
    );
    let source_set_caches = &session.query_state.ast.package_def_map.source_set_caches;

    assert!(
        source_set_caches.contains_key(&source_set_a),
        "changing A should keep A's package def map cache resident until the next query rebuild"
    );
    assert_eq!(
        source_set_caches
            .get(&source_set_a)
            .expect("A package def map cache should stay resident")
            .signature,
        a_signature_before,
        "changing A should keep the previous A membership signature resident until the next query rebuild"
    );
    assert!(
        source_set_caches.contains_key(&source_set_b),
        "changing A should keep B's package def map cache entry warm"
    );

    let b_members_after = session
        .source_set_package_def_map_query(source_set_b)
        .map(|def_map| member_item_names(def_map, "B."))
        .expect("B package def map should remain available");
    let a_members_after = session
        .source_set_package_def_map_query(source_set_a)
        .map(|def_map| member_item_names(def_map, "A."))
        .expect("A package def map should rebuild lazily");
    assert_eq!(
        b_members_before, b_members_after,
        "unrelated source-set updates should not change B package def map results"
    );
    assert_eq!(
        a_members_after,
        vec!["A.M".to_string()],
        "the changed source-set should rebuild on demand after keeping its membership cache resident"
    );
}
