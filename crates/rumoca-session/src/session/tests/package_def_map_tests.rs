use super::*;

fn parse_definition(source: &str, file_name: &str) -> ast::StoredDefinition {
    rumoca_phase_parse::parse_to_ast(source, file_name).expect("test definition should parse")
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
        "library::Modelica",
        SourceRootKind::Library,
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
        .source_set_id("library::Modelica")
        .expect("library source-set should have a stable id");
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
        "library::A",
        SourceRootKind::Library,
        vec![(
            "A/package.mo".to_string(),
            parse_definition("package A\n  model M\n  end M;\nend A;\n", "A/package.mo"),
        )],
        None,
    );
    session.replace_parsed_source_set(
        "library::B",
        SourceRootKind::Library,
        vec![(
            "B/package.mo".to_string(),
            parse_definition("package B\n  model N\n  end N;\nend B;\n", "B/package.mo"),
        )],
        None,
    );

    let source_set_a = session
        .source_set_id("library::A")
        .expect("A source-set should exist");
    let source_set_b = session
        .source_set_id("library::B")
        .expect("B source-set should exist");
    let b_members_before = session
        .source_set_package_def_map_query(source_set_b)
        .expect("B package def map should build")
        .member_item_keys("B.")
        .into_iter()
        .map(|item_key| item_key.qualified_name())
        .collect::<Vec<_>>();
    session
        .source_set_package_def_map_query(source_set_a)
        .expect("A package def map should build");

    assert!(
        session
            .query_state
            .ast
            .package_def_map
            .source_set_caches
            .contains_key(&source_set_a),
        "A package def map cache should be populated"
    );
    assert!(
        session
            .query_state
            .ast
            .package_def_map
            .source_set_caches
            .contains_key(&source_set_b),
        "B package def map cache should be populated"
    );

    session.replace_parsed_source_set(
        "library::A",
        SourceRootKind::Library,
        vec![(
            "A/package.mo".to_string(),
            parse_definition(
                "package A\n  model M\n    Real x;\n  end M;\nend A;\n",
                "A/package.mo",
            ),
        )],
        None,
    );

    assert!(
        !session
            .query_state
            .ast
            .package_def_map
            .source_set_caches
            .contains_key(&source_set_a),
        "changing A should invalidate only A's package def map cache entry"
    );
    assert!(
        session
            .query_state
            .ast
            .package_def_map
            .source_set_caches
            .contains_key(&source_set_b),
        "changing A should keep B's package def map cache entry warm"
    );

    let b_members_after = session
        .source_set_package_def_map_query(source_set_b)
        .expect("B package def map should remain available")
        .member_item_keys("B.")
        .into_iter()
        .map(|item_key| item_key.qualified_name())
        .collect::<Vec<_>>();
    assert_eq!(
        b_members_before, b_members_after,
        "unrelated source-set updates should not change B package def map results"
    );
}
