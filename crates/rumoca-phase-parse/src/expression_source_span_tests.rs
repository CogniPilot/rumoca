use crate::parse_to_ast;
use rumoca_core::SourceId;

fn exact_binding_span(expression: &str) {
    let source = format!(
        "model M\r\n // untouched λ\r\n Real value = {expression} \"description\"; Real sibling=7;\r\nend M;"
    );
    let definition = parse_to_ast(&source, "expression-span.mo").expect("valid syntax");
    let binding = definition.classes["M"].components["value"]
        .binding
        .as_ref()
        .expect("binding");
    let span = binding.span();
    assert_eq!(
        span.source,
        SourceId::from_source_name("expression-span.mo")
    );
    assert_eq!(
        source.get(span.start.0..span.end.0),
        Some(expression),
        "complete expression, including its grammar delimiters: {binding:?}"
    );
}

#[test]
fn expression_source_spans_cover_conditional_and_parenthesized_syntax() {
    for expression in [
        "2 /* λ */ * 3",
        "- /* unary */ 2*3",
        "not /* unary */ false",
        "( /* leading */ 2*3 /* trailing */ )",
        "((2*3))",
        "(2, /* tuple */ 3)",
        "(,3,)",
        "()",
        "if /* condition */ true then 2 else 3",
        "if true then 2 elseif false then 4 else (3)",
        "if true then (2) else if false then 3 else 4",
    ] {
        exact_binding_span(expression);
    }
}

#[test]
fn expression_source_spans_cover_array_constructor_delimiters() {
    for expression in [
        "{}",
        "{ /* one */ 2 }",
        "{2, /* two */ 3}",
        "{i /* body */ for i in 1:3}",
        "[ /* row */ 2,3 /* end */ ]",
        "[2,3; /* row */ 4,5]",
    ] {
        exact_binding_span(expression);
    }
}

#[test]
fn expression_source_spans_cover_calls_references_and_subscripts() {
    for expression in [
        "max(/* call */ 2,3)",
        "max(2,3)[ /* index */ 1 ]",
        "der(x)[1]",
        "(2,3)[1]",
        "(2).field",
        "some[1]",
        ".P.some[1].other[ /* index */ 2 ]",
        "some[1].other",
    ] {
        exact_binding_span(expression);
    }
}
