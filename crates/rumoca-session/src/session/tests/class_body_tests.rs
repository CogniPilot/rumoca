use super::*;

#[test]
fn file_class_body_query_tracks_local_component_occurrences() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            "model M\n  Real x;\nequation\n  x = x + 1;\nend M;\n",
        )
        .expect("source should parse");

    let item_key = session
        .file_summary_query("test.mo")
        .expect("file summary should exist")
        .item_key_for_name("M")
        .expect("class item key should exist")
        .clone();
    let class_body = session
        .file_class_body_query("test.mo")
        .expect("class body query should exist")
        .class_body(&item_key)
        .expect("class body should exist");

    let occurrences = class_body.component_occurrences("x");
    assert_eq!(
        occurrences.len(),
        2,
        "body query should track both local component use sites in the equation body"
    );
    assert_eq!(occurrences[0].start_line, 4);
    assert_eq!(occurrences[1].start_line, 4);
}

#[test]
fn file_class_body_query_remains_available_for_recovered_body_errors() {
    let mut session = Session::default();
    let source_v1 = r#"
model M
  Real x;
equation
  x = 1;
end M;
"#;
    let source_v2 = r#"
model M
  Real x;
equation
  x = 1
end M;
"#;

    session
        .add_document("input.mo", source_v1)
        .expect("initial source should parse");
    let item_key = session
        .file_summary_query("input.mo")
        .expect("file summary should exist")
        .item_key_for_name("M")
        .expect("class item key should exist")
        .clone();

    let parse_error = session.update_document("input.mo", source_v2);
    assert!(
        parse_error.is_some(),
        "updated source should keep a recoverable parse error"
    );

    let class_body = session
        .file_class_body_query("input.mo")
        .expect("class body query should exist for recovered syntax")
        .class_body(&item_key)
        .expect("class body should still exist");
    assert_eq!(
        class_body.component_occurrences("x").len(),
        0,
        "recovered body syntax may drop broken occurrences, but the body query should still exist"
    );
}

#[test]
fn file_class_body_query_tracks_equation_and_algorithm_sections() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            "model M\n  Real x;\nequation\n  x = 1;\nalgorithm\n  x := 2;\nalgorithm\n  x := 3;\nend M;\n",
        )
        .expect("source should parse");

    let item_key = session
        .file_summary_query("test.mo")
        .expect("file summary should exist")
        .item_key_for_name("M")
        .expect("class item key should exist")
        .clone();
    let class_body = session
        .file_class_body_query("test.mo")
        .expect("class body query should exist")
        .class_body(&item_key)
        .expect("class body should exist");

    let equation_section = class_body
        .equation_section()
        .expect("equation section should exist");
    assert_eq!(equation_section.count(), 1);
    assert_eq!(
        equation_section
            .range()
            .expect("equation section should have a range")
            .start_line,
        4
    );

    let algorithm_section = class_body
        .algorithm_section()
        .expect("algorithm section should exist");
    assert_eq!(algorithm_section.count(), 2);
    let algorithm_range = algorithm_section
        .range()
        .expect("algorithm section should have a range");
    assert_eq!(algorithm_range.start_line, 6);
    assert_eq!(algorithm_range.end_line, 8);
}

#[test]
fn file_class_body_query_tracks_modifier_class_targets() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            "model DefaultVariant\n  Real x;\nend DefaultVariant;\n\nmodel Base\n  replaceable model Variant = DefaultVariant;\nend Base;\n\nmodel Test\n  Base base(replaceable model Variant = DefaultVariant);\nend Test;\n",
        )
        .expect("source should parse");

    let item_key = session
        .file_summary_query("test.mo")
        .expect("file summary should exist")
        .item_key_for_name("Test")
        .expect("class item key should exist")
        .clone();
    let class_body = session
        .file_class_body_query("test.mo")
        .expect("class body query should exist")
        .class_body(&item_key)
        .expect("class body should exist");

    let modifier_targets = class_body.modifier_class_targets();
    assert_eq!(modifier_targets.len(), 1);
    assert_eq!(modifier_targets[0].raw_name(), "DefaultVariant");
    assert_eq!(modifier_targets[0].token_text(), "DefaultVariant");
    assert_eq!(modifier_targets[0].location().start_line, 10);
}
