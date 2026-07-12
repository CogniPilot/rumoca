use super::*;

#[test]
fn vector_subscript_consumes_one_distributed_result_index() {
    let selection = vec![ast::Subscript::Expression(ast::Expression::Array {
        elements: vec![make_int_expr(1), make_int_expr(5)],
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    })];

    let projected = project_array_selection_for_element(
        &ast::ClassTree::default(),
        &IndexMap::default(),
        Some(&selection),
        &[2],
        test_span(),
    )
    .expect("vector subscript composition should succeed");

    assert_eq!(projected.len(), 1);
    let ast::Subscript::Expression(ast::Expression::Terminal { token, .. }) = &projected[0] else {
        panic!("the selected vector element should be the composed subscript");
    };
    assert_eq!(token.text.as_ref(), "5");
}
