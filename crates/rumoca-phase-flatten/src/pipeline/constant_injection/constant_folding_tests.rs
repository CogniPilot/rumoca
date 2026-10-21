use std::sync::Arc;

use super::{
    eval_size_call_with_scope, extract_named_record_constructor_fields, infer_dims_from_expr,
    try_eval_const_boolean_with_scope,
};
use crate::Context;
use rumoca_ir_ast as ast;

fn token(text: &str) -> rumoca_core::Token {
    rumoca_core::Token {
        text: Arc::from(text),
        ..rumoca_core::Token::default()
    }
}

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("constant_folding_test.mo"),
        1,
        2,
    )
}

fn fixture_def_id(name: &str) -> rumoca_core::DefId {
    let hash = name.bytes().fold(2_166_136_261_u32, |hash, byte| {
        hash.wrapping_mul(16_777_619) ^ u32::from(byte)
    });
    rumoca_core::DefId::new(hash.max(1))
}

fn bool_expr(value: bool) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::Bool,
        token: token(if value { "true" } else { "false" }),
        span: rumoca_core::Span::DUMMY,
    }
}

fn real_expr(value: &str) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedReal,
        token: token(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn int_expr(value: &str) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: token(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn string_expr(value: &str) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::String,
        token: token(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn unknown_bool_ref(name: &str) -> ast::Expression {
    ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: token(name),
            subs: None,
            def_id: Some(fixture_def_id(name)),
        }],
        span: test_span(),
        qualified_display_name: None,
    })
}

fn component_ref(path: &str, target_def_id: rumoca_core::DefId) -> ast::ComponentReference {
    let mut reference = ast::ComponentReference {
        local: false,
        parts: crate::path_utils::segments(path)
            .into_iter()
            .map(|part| ast::ComponentRefPart {
                ident: token(part),
                subs: None,
                def_id: Some(fixture_def_id(part)),
            })
            .collect(),
        span: test_span(),
        qualified_display_name: None,
    };
    reference.set_target_def_id(Some(target_def_id));
    reference
}

fn real_binary(
    op: rumoca_core::OpBinary,
    lhs: ast::Expression,
    rhs: ast::Expression,
) -> ast::Expression {
    ast::Expression::Binary {
        op,
        lhs: Arc::new(lhs),
        rhs: Arc::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn bool_binary(
    op: rumoca_core::OpBinary,
    lhs: ast::Expression,
    rhs: ast::Expression,
) -> ast::Expression {
    ast::Expression::Binary {
        op,
        lhs: Arc::new(lhs),
        rhs: Arc::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn if_expr(
    cond: ast::Expression,
    then_expr: ast::Expression,
    else_expr: ast::Expression,
) -> ast::Expression {
    ast::Expression::If {
        branches: vec![(cond, then_expr)],
        else_branch: Arc::new(else_expr),
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn infers_constant_range_binding_dimensions() {
    let expr = ast::Expression::Range {
        start: Arc::new(int_expr("1")),
        step: Some(Arc::new(int_expr("2"))),
        end: Arc::new(int_expr("5")),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        infer_dims_from_expr(&expr, &Context::default(), ""),
        Some(vec![3])
    );
}

#[test]
fn fill_with_array_seed_prefixes_dimensions_including_zero() {
    let seed = ast::Expression::Array {
        elements: vec![int_expr("1"), int_expr("2"), int_expr("3")],
        kind: rumoca_core::ArrayConstructor::Array,
        span: rumoca_core::Span::DUMMY,
    };
    let expr = ast::Expression::FunctionCall {
        comp: component_ref("fill", fixture_def_id("fill")),
        args: vec![seed, int_expr("0"), int_expr("2")],
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        infer_dims_from_expr(&expr, &Context::default(), ""),
        Some(vec![0, 2, 3])
    );
}

#[test]
fn size_of_string_matrix_literal_uses_shape_without_value_evaluation() {
    let matrix = ast::Expression::Array {
        elements: vec![
            ast::Expression::Array {
                elements: vec![string_expr("\"a\""), string_expr("\"b\"")],
                kind: rumoca_core::ArrayConstructor::Array,
                span: rumoca_core::Span::DUMMY,
            },
            ast::Expression::Array {
                elements: vec![string_expr("\"c\""), string_expr("\"d\"")],
                kind: rumoca_core::ArrayConstructor::Array,
                span: rumoca_core::Span::DUMMY,
            },
        ],
        kind: rumoca_core::ArrayConstructor::Array,
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_size_call_with_scope(&matrix, &int_expr("2"), &Context::default(), ""),
        Some(2)
    );
}

#[test]
fn boolean_constant_folding_short_circuits_unknown_and_rhs() {
    let expr = bool_binary(
        rumoca_core::OpBinary::And,
        bool_expr(false),
        unknown_bool_ref("unknownFlag"),
    );
    assert_eq!(
        try_eval_const_boolean_with_scope(&expr, &Context::default(), ""),
        Some(false)
    );
}

#[test]
fn boolean_constant_folding_short_circuits_unknown_or_rhs() {
    let expr = bool_binary(
        rumoca_core::OpBinary::Or,
        bool_expr(true),
        unknown_bool_ref("unknownFlag"),
    );
    assert_eq!(
        try_eval_const_boolean_with_scope(&expr, &Context::default(), ""),
        Some(true)
    );
}

#[test]
fn boolean_constant_folding_handles_if_expressions() {
    let expr = if_expr(bool_expr(false), bool_expr(false), bool_expr(true));
    assert_eq!(
        try_eval_const_boolean_with_scope(&expr, &Context::default(), ""),
        Some(true)
    );
}

#[test]
fn relational_constant_folding_compares_real_values() {
    let expr = real_binary(
        rumoca_core::OpBinary::Lt,
        real_expr("-1.5"),
        real_expr("-1.0"),
    );
    assert_eq!(
        try_eval_const_boolean_with_scope(&expr, &Context::default(), ""),
        Some(true)
    );
}

#[test]
fn named_record_constructor_extraction_preserves_target_identity() {
    let record_def_id = rumoca_core::DefId::new(42);
    let expr = ast::Expression::ClassModification {
        target: component_ref("Utilities.ParameterRecords.MachineData", record_def_id),
        modifications: vec![ast::Expression::Modification {
            target: component_ref("PRef", fixture_def_id("PRef")),
            value: Arc::new(real_expr("1000.0")),
            span: rumoca_core::Span::DUMMY,
        }],
        each_flags: vec![false],
        final_flags: vec![false],
        redeclare_flags: vec![false],
        span: rumoca_core::Span::DUMMY,
    };

    let (constructor, fields) = extract_named_record_constructor_fields(&expr)
        .expect("named record constructor should be extracted");

    assert_eq!(constructor.target_def_id(), Some(record_def_id));
    assert_eq!(
        constructor.as_str(),
        "Utilities.ParameterRecords.MachineData"
    );
    assert_eq!(fields.len(), 1);
}

#[test]
fn record_array_alias_materializes_indexed_fields() {
    let source = r#"
        record R
            Real molarMass;
            Integer n;
            Boolean enabled;
        end R;
        model Test
            constant R source[2] = {R(molarMass=0.018, n=2, enabled=true), R(molarMass=0.044, n=3, enabled=false)};
        end Test;
    "#;
    let parsed = rumoca_phase_parse::parse_to_ast(source, "record_array_alias.mo").unwrap();
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(ast::ClassTree::from_parsed(parsed)))
            .unwrap();
    let model = resolved.get_class_by_qualified_name("Test").unwrap();
    let binding = model.components["source"].binding.as_ref().unwrap();
    let mut ctx = Context::new();
    let value = crate::record_constant_arrays::try_extract_record_array_constructor_constant(
        binding, &mut ctx, "", "source",
    )
    .expect("record array is evaluated");
    ctx.constant_values.insert("source".to_string(), value);
    assert_eq!(ctx.real_parameter_values["source[1].molarMass"], 0.018);
    crate::constant_extraction::materialize_record_constant_alias_fields(
        &mut ctx,
        "medium",
        "fluidConstants",
        "medium.fluidConstants",
        "source",
    );
    assert_eq!(
        ctx.real_parameter_values
            .get("medium.fluidConstants[1].molarMass"),
        Some(&0.018)
    );
    assert_eq!(
        ctx.real_parameter_values
            .get("medium.fluidConstants[2].molarMass"),
        Some(&0.044)
    );
    assert_eq!(
        ctx.parameter_values.get("medium.fluidConstants[2].n"),
        Some(&3)
    );
    assert_eq!(
        ctx.boolean_parameter_values
            .get("medium.fluidConstants[1].enabled"),
        Some(&true)
    );
    // An effective field override takes precedence over the constructor default.
    ctx.real_parameter_values
        .insert("source[1].molarMass".into(), 0.020);
    crate::constant_extraction::materialize_record_constant_alias_fields(
        &mut ctx,
        "medium",
        "fluidConstants",
        "medium.fluidConstants",
        "source",
    );
    assert_eq!(
        ctx.real_parameter_values
            .get("medium.fluidConstants[1].molarMass"),
        Some(&0.020)
    );
}

#[test]
fn scalar_record_alias_keeps_scoped_fields_and_dimensions() {
    let source = r#"
        record R
            Real values[2];
        end R;
        model Test
            constant R source = R(values={1.0, 2.0});
        end Test;
    "#;
    let parsed = rumoca_phase_parse::parse_to_ast(source, "record_alias.mo").unwrap();
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(ast::ClassTree::from_parsed(parsed)))
            .unwrap();
    let model = resolved.get_class_by_qualified_name("Test").unwrap();
    let mut ctx = Context::default();
    let value = super::try_extract_named_record_constructor_constant(
        model.components["source"].binding.as_ref().unwrap(),
        &mut ctx,
        "",
        "source",
    )
    .unwrap();
    ctx.constant_values.insert("source".into(), value);
    ctx.array_dimensions.insert("source.values".into(), vec![2]);
    let expected = ctx.constant_values["source.values"].clone();
    crate::constant_extraction::materialize_record_constant_alias_fields(
        &mut ctx,
        "medium",
        "data",
        "medium.data",
        "source",
    );
    assert_eq!(
        ctx.constant_values.get("medium.data.values"),
        Some(&expected)
    );
    assert_eq!(
        ctx.array_dimensions.get("medium.data.values"),
        Some(&vec![2])
    );
}
