use rumoca_core::{
    ComponentRefPart, ComponentReference, DefId, EffectiveType, Expression, ForIndex, Function,
    FunctionInstanceId, FunctionParam, Literal, OpBinary, SourceMap, Span, Statement,
    StatementBlock, TypeId, component_ref_to_base_reference,
};

pub(super) const VALID: u32 = 1;
pub(super) const RESULT: u32 = 2;
pub(super) const DATA: u32 = 3;
pub(super) const INDEX: u32 = 4;
pub(super) const REPLACEMENT: u32 = 5;

pub(super) fn source() -> (SourceMap, Span) {
    let mut sources = SourceMap::new();
    let id = sources.add("normalized-function.mo", "function f\nalgorithm\nend f;");
    (sources, Span::from_offsets(id, 11, 20))
}

pub(super) fn owner(span: Span) -> Function {
    let mut function = Function::new("normalized.f", span);
    function.def_id = Some(DefId::new(100));
    function.instance_id = Some(FunctionInstanceId::new(0));
    function
}

pub(super) fn component(name: &str, id: u32, span: Span) -> ComponentReference {
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

pub(super) fn variable(name: &str, id: u32, span: Span) -> Expression {
    Expression::VarRef {
        name: component_ref_to_base_reference(&component(name, id, span)),
        subscripts: vec![],
        span,
    }
}

pub(super) fn boolean(value: bool, span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Boolean(value),
        span,
    }
}

pub(super) fn integer(value: i64, span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span,
    }
}

pub(super) fn assignment(name: &str, id: u32, value: Expression, span: Span) -> Statement {
    Statement::Assignment {
        comp: component(name, id, span),
        value,
        span,
    }
}

pub(super) fn conditional(
    condition: Expression,
    statements: Vec<Statement>,
    span: Span,
) -> Statement {
    Statement::If {
        cond_blocks: vec![StatementBlock {
            cond: condition,
            stmts: statements,
        }],
        else_block: None,
        span,
    }
}

pub(super) fn for_loop(name: &str, count: i64, body: Vec<Statement>, span: Span) -> Statement {
    Statement::For {
        indices: vec![ForIndex {
            ident: name.into(),
            range: Expression::Range {
                start: Box::new(integer(1, span)),
                step: None,
                end: Box::new(integer(count, span)),
                span,
            },
        }],
        equations: body,
        span,
    }
}

pub(super) fn mutable_gate_function(span: Span) -> Function {
    let mut function = owner(span);
    let boolean_type = EffectiveType::new(TypeId(1), TypeId(1), vec![]).unwrap();
    let integer_type = EffectiveType::new(TypeId(2), TypeId(2), vec![]).unwrap();
    function
        .locals
        .push(FunctionParam::new("valid", "Boolean", boolean_type, span));
    function
        .outputs
        .push(FunctionParam::new("result", "Integer", integer_type, span));
    let increment = Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(variable("result", RESULT, span)),
        rhs: Box::new(integer(1, span)),
        span,
    };
    let active = Expression::Binary {
        op: OpBinary::Neq,
        lhs: Box::new(variable("i", INDEX, span)),
        rhs: Box::new(integer(2, span)),
        span,
    };
    function.body = vec![
        assignment("result", RESULT, integer(0, span), span),
        assignment("valid", VALID, boolean(false, span), span),
        for_loop(
            "i",
            3,
            vec![
                assignment("valid", VALID, active, span),
                conditional(
                    variable("valid", VALID, span),
                    vec![for_loop(
                        "j",
                        2,
                        vec![
                            assignment("valid", VALID, boolean(false, span), span),
                            assignment("result", RESULT, increment, span),
                        ],
                        span,
                    )],
                    span,
                ),
            ],
            span,
        ),
    ];
    function
}
