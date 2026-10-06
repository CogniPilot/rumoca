use super::*;
use rumoca_core::{
    ComponentRefPart, ComponentReference, DefId, Reference, SourceId, StatementBlock,
};

pub(super) const COUNTER: DefId = DefId(8101);
pub(super) fn span() -> Span {
    Span::from_offsets(SourceId::DUMMY, 100, 110)
}
pub(super) fn integer(value: i64) -> Expression {
    Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
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
            subs: vec![],
            def_id: id,
        }],
    )
    .expect("checked scalar component")
}
pub(super) fn variable(name: &str, id: DefId) -> Expression {
    Expression::VarRef {
        name: Reference::from_component_reference(component(name, id)),
        subscripts: vec![],
        span: span(),
    }
}
pub(super) fn count() -> Expression {
    variable("count", COUNTER)
}
pub(super) fn binary(op: rumoca_core::OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: span(),
    }
}
pub(super) fn assign(name: &str, id: DefId, value: Expression) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: component(name, id),
        value,
        span: span(),
    }
}
pub(super) fn range(end: Expression) -> Expression {
    Expression::Range {
        start: Box::new(integer(1)),
        step: None,
        end: Box::new(end),
        span: span(),
    }
}
pub(super) fn for_loop(
    name: &str,
    end: Expression,
    body: Vec<rumoca_core::Statement>,
) -> rumoca_core::Statement {
    rumoca_core::Statement::For {
        indices: vec![rumoca_core::ForIndex {
            ident: name.to_owned(),
            range: range(end),
        }],
        equations: body,
        span: span(),
    }
}
pub(super) fn branch(
    predicate: Expression,
    body: Vec<rumoca_core::Statement>,
) -> rumoca_core::Statement {
    rumoca_core::Statement::If {
        cond_blocks: vec![StatementBlock {
            cond: predicate,
            stmts: body,
        }],
        else_block: None,
        span: span(),
    }
}
pub(super) fn source(predicate: Expression) -> Vec<rumoca_core::Statement> {
    use rumoca_core::OpBinary;
    let sum_id = DefId::new(8108);
    let j = variable("j", DefId::new(8109));
    let ordered = binary(
        OpBinary::Add,
        binary(OpBinary::Mul, variable("sum", sum_id), integer(3)),
        j,
    );
    let ordered = Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Mod,
        args: vec![ordered, integer(1000003)],
        span: span(),
    };
    vec![
        assign("count", COUNTER, integer(0)),
        for_loop(
            "i",
            integer(14400),
            vec![branch(
                predicate,
                vec![assign(
                    "count",
                    COUNTER,
                    binary(OpBinary::Add, count(), integer(1)),
                )],
            )],
        ),
        assign("sum", sum_id, integer(0)),
        for_loop("j", count(), vec![assign("sum", sum_id, ordered)]),
    ]
}
pub(super) fn entry() -> Environment {
    [
        ("count", Value::Integer(-99)),
        ("sum", Value::Integer(-7)),
        ("i", Value::Integer(80000)),
        ("j", Value::Integer(90000)),
    ]
    .into_iter()
    .map(|(name, value)| (VarName::new(name), value))
    .collect()
}
