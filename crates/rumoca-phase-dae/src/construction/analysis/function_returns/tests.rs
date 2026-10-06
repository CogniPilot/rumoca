//! MLS 3.6 §11.2.6: an inactive elseif predicate is never evaluated.

use rumoca_core::{
    ComponentRefPart, ComponentReference, DefId, EffectiveType, Function, FunctionParam, SourceMap,
    Statement, StatementBlock, Subscript, TypeId, component_ref_to_base_reference,
};
use rumoca_eval_flat::constant::{EvalError, EvalLimits, Value, eval_function};

use super::*;

fn reference(name: &str, span: Span) -> ComponentReference {
    let id = match name {
        "first" => 1,
        "samples" => 2,
        "k" => 3,
        "result" => 4,
        _ => unreachable!("fixture has four source declarations"),
    };
    ComponentReference::construct(
        true,
        span,
        vec![ComponentRefPart {
            ident: name.into(),
            span,
            subs: vec![],
            def_id: DefId::new(id),
        }],
    )
    .unwrap()
}

fn read(name: &str, span: Span) -> Expression {
    Expression::VarRef {
        name: component_ref_to_base_reference(&reference(name, span)),
        subscripts: vec![],
        span,
    }
}

fn real(value: f64, span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span,
    }
}

fn store(value: f64, span: Span) -> Statement {
    Statement::Assignment {
        comp: reference("result", span),
        value: real(value, span),
        span,
    }
}

fn fixture() -> (Function, Span) {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "lazy-return.mo",
        "function lazy_return\nalgorithm\nend lazy_return;",
    );
    let span = Span::from_offsets(source, 21, 30);
    let mut function = Function::new("lazy_return", span);
    let param = |name: &str, type_name: &str, id, dimensions| {
        FunctionParam::new(
            name,
            type_name,
            EffectiveType::new(TypeId(id), TypeId(id), dimensions).unwrap(),
            span,
        )
    };
    function.inputs = vec![
        param("first", "Boolean", 1, vec![]),
        param("samples", "Real", 2, vec![1]),
        param("k", "Integer", 3, vec![]),
    ];
    function.outputs = vec![param("result", "Real", 2, vec![])];
    let mut gather = read("samples", span);
    let Expression::VarRef { subscripts, .. } = &mut gather else {
        unreachable!()
    };
    subscripts.push(Subscript::Expr {
        expr: Box::new(read("k", span)),
        span,
    });
    function.body = vec![
        store(0.0, span),
        Statement::If {
            cond_blocks: vec![
                StatementBlock {
                    cond: read("first", span),
                    stmts: vec![store(1.0, span), Statement::Return { span }],
                },
                StatementBlock {
                    cond: Expression::Binary {
                        op: OpBinary::Gt,
                        lhs: Box::new(gather),
                        rhs: Box::new(real(0.0, span)),
                        span,
                    },
                    stmts: vec![store(2.0, span), Statement::Return { span }],
                },
            ],
            else_block: None,
            span,
        },
        store(3.0, span),
    ];
    (function, span)
}

fn evaluate_predicate(
    value: Expression,
    first: bool,
    sample: f64,
    k: i64,
) -> Result<Value, EvalError> {
    let (mut function, span) = fixture();
    function.outputs = vec![FunctionParam::new(
        "result",
        "Boolean",
        EffectiveType::new(TypeId(1), TypeId(1), vec![]).unwrap(),
        span,
    )];
    function.body = vec![Statement::Assignment {
        comp: reference("result", span),
        value,
        span,
    }];
    eval_function(
        &function,
        vec![
            Value::Bool(first),
            Value::Array(vec![Value::Real(sample)]),
            Value::Integer(k),
        ],
        &EvalContext::new(),
        &EvalLimits::default(),
        0,
        span,
    )
}

#[test]
fn normalized_return_predicate_skips_an_inactive_invalid_elseif() {
    let (function, span) = fixture();
    let oracle = eval_function(
        &function,
        vec![
            Value::Bool(true),
            Value::Array(vec![Value::Real(5.0)]),
            Value::Integer(2),
        ],
        &EvalContext::new(),
        &EvalLimits::default(),
        0,
        span,
    )
    .unwrap();
    assert_eq!(oracle, Value::Real(1.0));
    let normalized = normalize_function_returns(&function.body).unwrap();
    let value = evaluate_predicate(normalized.guards[0].value.clone(), true, 5.0, 2);
    assert_eq!(value.unwrap(), Value::Bool(true));
}

#[test]
fn normalized_return_predicate_keeps_an_active_invalid_elseif_fault() {
    let (function, _) = fixture();
    let normalized = normalize_function_returns(&function.body).unwrap();
    assert!(evaluate_predicate(normalized.guards[0].value.clone(), false, 5.0, 2).is_err());
}

#[test]
fn normalized_return_predicate_keeps_the_valid_second_branch_and_fallthrough() {
    let (function, _) = fixture();
    let normalized = normalize_function_returns(&function.body).unwrap();
    for (sample, expected) in [(5.0, true), (-5.0, false)] {
        assert_eq!(
            evaluate_predicate(normalized.guards[0].value.clone(), false, sample, 1).unwrap(),
            Value::Bool(expected)
        );
    }
}

#[test]
fn an_inactive_continuation_skips_its_source_predicate() {
    let (function, span) = fixture();
    let Statement::If { cond_blocks, .. } = &function.body[1] else {
        unreachable!()
    };
    let value = and_condition(
        Expression::Literal {
            value: Literal::Boolean(false),
            span,
        },
        cond_blocks[1].cond.clone(),
        span,
    );
    assert_eq!(
        evaluate_predicate(value, false, 5.0, 2).unwrap(),
        Value::Bool(false)
    );
}

#[test]
fn an_active_continuation_keeps_its_source_predicate_fault() {
    let (function, span) = fixture();
    let Statement::If { cond_blocks, .. } = &function.body[1] else {
        unreachable!()
    };
    let value = and_condition(
        Expression::Literal {
            value: Literal::Boolean(true),
            span,
        },
        cond_blocks[1].cond.clone(),
        span,
    );
    assert!(evaluate_predicate(value, false, 5.0, 2).is_err());
}
