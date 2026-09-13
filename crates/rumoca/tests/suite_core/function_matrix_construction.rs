//! MLS §10.4.2.1: bracket separators determine promoted concatenation axes.

use rumoca::Compiler;
use rumoca_ir_ast as ast;

const BLOCKS: &str = r#"
model ParameterMatrixBlocks
  function assemble
    output Real y[3,2];
  protected
    Real a[2] = {11,21};
    Real b[2] = {12,22};
    Real c[1] = {31};
    Real d[1] = {32};
    Real cm[3,2];
  algorithm
    cm := [a,b;c,d];
    y := cm[1:3,:];
  end assemble;
  parameter Real table[3,2] = assemble();
  Real y[3,2] = table;
end ParameterMatrixBlocks;
"#;

fn check_matrix(source: &str) {
    let compiled = Compiler::new()
        .model("ParameterMatrixBlocks")
        .compile_str(source, "function_matrix_construction.mo")
        .expect("block concatenation must compile");
    let result = rumoca_sim::simulate_dae(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 0.2,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("constant block concatenation must simulate");
    for row in 1..=3 {
        for column in 1..=2 {
            let name = format!("y[{row},{column}]");
            let index = result.names.iter().position(|n| n == &name).unwrap();
            let expected = f64::from(10 * row + column);
            for actual in &result.data[index] {
                assert_eq!(*actual, expected, "wrong matrix coordinate {name}");
            }
        }
    }
}

#[test]
fn vector_blocks_concatenate_as_columns_then_rows() {
    check_matrix(BLOCKS);
}

#[test]
fn explicit_scalar_matrix_control() {
    check_matrix(&BLOCKS.replace("[a,b;c,d]", "[11,12;21,22;31,32]"));
}

#[test]
fn nested_bracket_blocks_keep_their_own_axes() {
    check_matrix(&BLOCKS.replace("[a,b;c,d]", "[[a,b];[c,d]]"));
}

#[test]
fn horizontal_matrix_blocks_join_columns_without_adding_rows() {
    check_matrix(&BLOCKS.replace("[a,b;c,d]", "[[11;21;31],[12;22;32]]"));
}

fn constructor_structure(expr: &ast::Expression) -> serde_json::Value {
    match expr {
        ast::Expression::Array { elements, kind, .. } => serde_json::json!({
            "kind": kind,
            "elements": elements.iter().map(constructor_structure).collect::<Vec<_>>(),
        }),
        ast::Expression::Terminal { token, .. } => serde_json::json!(token.text),
        _ => panic!("the fixture contains only bracket constructors and literals"),
    }
}

#[test]
fn parser_preserves_horizontal_versus_vertical_concatenation() {
    let parse = |binding: &str| {
        let source = format!("model M parameter Real a[:,:] = {binding}; end M;");
        let parsed = rumoca_phase_parse::parse_to_ast(&source, "matrix.mo").unwrap();
        let binding = parsed.classes["M"].components["a"]
            .binding
            .as_ref()
            .unwrap();
        constructor_structure(binding)
    };
    assert_ne!(
        parse("[[11,12],[21,22]]"),
        parse("[11,12;21,22]"),
        "1-by-4 and 2-by-2 constructors must have distinct semantic IR, independently of spans",
    );
}
