use crate::parse_to_ast;

fn exact_span(source: &str, name: &str, expected: &str) {
    let definition = parse_to_ast(source, "span-fixture.mo").expect("valid source");
    let class = match name.split_once('.') {
        Some((parent, child)) => &definition.classes[parent].classes[child],
        None => &definition.classes[name],
    };
    let range = class.location.start as usize..class.location.end as usize;
    assert_eq!(
        source.get(range.clone()),
        Some(expected),
        "{name}: {range:?}"
    );
    assert!(
        parse_to_ast(
            &format!("package Extracted {expected} end Extracted;"),
            "extracted.mo"
        )
        .is_ok(),
        "complete declaration in its element context"
    );
}

#[test]
fn top_level_class_span_keeps_qualifiers_comments_and_semicolon() {
    let expected = "final /* α */ encapsulated partial model Part Real x; end Part /*tail*/ ;";
    exact_span(
        &format!("within R;\r\n{expected}\r\nmodel After end After;"),
        "Part",
        expected,
    );
}

#[test]
fn nested_class_span_keeps_element_qualifiers_and_constraint() {
    let expected =
        "final /*q*/ replaceable model Part Real x; end Part constrainedby Base /*constraint*/ ;";
    exact_span(
        &format!("package Root model Base end Base; protected {expected} end Root;"),
        "Root.Part",
        expected,
    );
}

#[test]
fn short_class_span_covers_its_definition_and_annotations() {
    let expected =
        "type Score = Real(min=0) \"score\" annotation(Dialog(group=\"values\")) /*tail*/ ;";
    exact_span(
        &format!("package Root {expected} type After=Real; end Root;"),
        "Root.Score",
        expected,
    );
}

#[test]
fn same_line_siblings_have_independent_source_spans() {
    let source = "package Root model A end A; model B end B /*b*/ ; end Root;";
    exact_span(source, "Root.A", "model A end A;");
    exact_span(source, "Root.B", "model B end B /*b*/ ;");
}
