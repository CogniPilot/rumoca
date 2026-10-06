use super::*;
use rumoca_core::{
    ComponentRefPart, ComponentReference, ForIndex, Reference, SourceId, StatementBlock,
};

pub(super) const COUNTER: DefId = DefId(8101);
pub(super) fn span() -> Span {
    Span::from_offsets(SourceId::DUMMY, 100, 110)
}
pub(super) fn integer(value: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span: span(),
    }
}
pub(super) fn component(name: &str, id: DefId) -> ComponentReference {
    ComponentReference::construct(
        false,
        span(),
        vec![ComponentRefPart {
            ident: name.to_owned(),
            span: span(),
            subs: Vec::new(),
            def_id: id,
        }],
    )
    .expect("one checked local component")
}
pub(super) fn variable(name: &str, id: DefId) -> Expression {
    Expression::VarRef {
        name: Reference::from_component_reference(component(name, id)),
        subscripts: Vec::new(),
        span: span(),
    }
}
pub(super) fn count() -> Expression {
    variable("count", COUNTER)
}
pub(super) fn binary(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: span(),
    }
}
pub(super) fn assignment(name: &str, id: DefId, value: Expression) -> Statement {
    Statement::Assignment {
        comp: component(name, id),
        value,
        span: span(),
    }
}
pub(super) fn update(value: Expression) -> Statement {
    assignment("count", COUNTER, value)
}
pub(super) fn increment() -> Statement {
    update(binary(OpBinary::Add, count(), integer(1)))
}
pub(super) fn conditional(body: Vec<Statement>) -> Statement {
    Statement::If {
        cond_blocks: vec![StatementBlock {
            cond: variable("accept", DefId::new(8102)),
            stmts: body,
        }],
        else_block: None,
        span: span(),
    }
}
pub(super) fn range(start: i64, step: Option<i64>, end: i64) -> Expression {
    Expression::Range {
        start: Box::new(integer(start)),
        step: step.map(integer).map(Box::new),
        end: Box::new(integer(end)),
        span: span(),
    }
}
pub(super) fn for_range(range: Expression, body: Vec<Statement>) -> Statement {
    Statement::For {
        indices: vec![ForIndex {
            ident: "i".to_owned(),
            range,
        }],
        equations: body,
        span: span(),
    }
}
pub(super) fn program(initial: i64, range: Expression, body: Vec<Statement>) -> Vec<Statement> {
    vec![update(integer(initial)), for_range(range, body)]
}
pub(super) fn prove(statements: &[Statement], shapes: &mut ShapeEnvironment) -> Option<(i64, i64)> {
    infer_finite_for_counter_bounds(
        statements,
        shapes,
        &[(VarName::new("count"), COUNTER)],
        &[
            (VarName::new("scores"), DefId::new(8103)),
            (VarName::new("input"), DefId::new(8104)),
            (VarName::new("limit"), DefId::new(8110)),
        ],
    );
    shapes.proven_integer_bounds(&count())
}
pub(super) fn empty_shapes() -> ShapeEnvironment {
    ShapeEnvironment::with_capacity(4)
}
