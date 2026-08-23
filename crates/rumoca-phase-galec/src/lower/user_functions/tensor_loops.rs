//! Building the loop nest that walks one tensor assignment's axes.
//!
//! The nest is built innermost axis first. At each axis the statements whose
//! values cannot change with that axis are lifted out of its loop, so work
//! that names only the outer axes runs once per outer coordinate rather than
//! once per element. This is what makes the split contraction bodies in
//! `expression_projection` pay: their invariant half names the row and the
//! contracted index, and lands between the row loop and the column loop.

use super::*;

/// Wrap `body` in one loop per tensor axis, innermost axis first, lifting the
/// statements that cannot change with an axis out of that axis's loop.
///
/// The result is the statement sequence that replaces `body`: the lifted
/// statements of the outermost axis, if any, followed by that axis's loop.
pub(in crate::lower) fn nest_tensor_loops(
    mut body: Vec<gast::Spanned<gast::Statement>>,
    iterators: &[gast::Name],
    extents: &[u32],
    span: Span,
) -> Vec<gast::Spanned<gast::Statement>> {
    for (iterator, &extent) in iterators.iter().zip(extents).rev() {
        let (invariant, dependent) = if is_reorderable(&body) {
            partition_by_dependence(body, std::slice::from_ref(iterator))
        } else {
            (Vec::new(), body)
        };
        body = invariant;
        body.push(gast::Spanned::new(
            gast::Statement::For(gast::ForLoop {
                iterator: Some(iterator.clone()),
                start: gast::Expression::Integer(1),
                step: None,
                stop: gast::Expression::Integer(i64::from(extent)),
                body: dependent,
            }),
            span,
        ));
    }
    body
}

/// Whether a statement sequence may be reordered on the strength of the names
/// it reads and writes alone.
///
/// A call reaches its outputs through the callee's context region rather than
/// through its own targets, and `limit` and `signal` reach the entities they
/// saturate and the signals they set the same way, so a partition that reads
/// only names cannot place any of them. Sequences holding one keep the order
/// they were built in.
pub(in crate::lower) fn is_reorderable(statements: &[gast::Spanned<gast::Statement>]) -> bool {
    statements.iter().all(|statement| match &statement.node {
        gast::Statement::Assignment { .. } => true,
        gast::Statement::MultiAssignment { .. }
        | gast::Statement::Call(_)
        | gast::Statement::Limit(_)
        | gast::Statement::Signal(_) => false,
        gast::Statement::If(value) => is_reorderable_guard(value),
        gast::Statement::For(value) => is_reorderable(&value.body),
    })
}

fn is_reorderable_guard(value: &gast::IfStatement) -> bool {
    value
        .branches
        .iter()
        .all(|branch| is_reorderable(&branch.body))
        && value
            .else_body
            .as_ref()
            .is_none_or(|body| is_reorderable(body))
}

#[cfg(test)]
mod tests {
    use super::{is_reorderable, nest_tensor_loops};
    use rumoca_core::Span;
    use rumoca_ir_galec::ast as gast;

    fn local(name: &str) -> gast::Expression {
        gast::Expression::Ref(gast::Reference::local(gast::Name::ident(name)))
    }

    fn element(name: &str, subscripts: &[&str]) -> gast::Expression {
        gast::Expression::Ref(gast::Reference::Local(gast::RefPart {
            name: gast::Name::ident(name),
            subscripts: subscripts.iter().copied().map(local).collect(),
            span: Span::DUMMY,
        }))
    }

    fn write(target: gast::Expression, value: gast::Expression) -> gast::Spanned<gast::Statement> {
        let gast::Expression::Ref(target) = target else {
            panic!("a write needs a reference target")
        };
        gast::Spanned::dummy(gast::Statement::Assignment { target, value })
    }

    fn loop_over(
        iterator: &str,
        body: Vec<gast::Spanned<gast::Statement>>,
    ) -> gast::Spanned<gast::Statement> {
        gast::Spanned::dummy(gast::Statement::For(gast::ForLoop {
            iterator: Some(gast::Name::ident(iterator)),
            start: gast::Expression::Integer(1),
            step: None,
            stop: gast::Expression::Integer(4),
            body,
        }))
    }

    fn iterators() -> Vec<gast::Name> {
        vec![gast::Name::ident("row"), gast::Name::ident("column")]
    }

    fn loop_iterator(statement: &gast::Spanned<gast::Statement>) -> &str {
        let gast::Statement::For(value) = &statement.node else {
            panic!("expected a loop")
        };
        value
            .iterator
            .as_ref()
            .expect("the nest declares every iterator")
            .lexeme()
    }

    fn loop_body(statement: &gast::Spanned<gast::Statement>) -> &[gast::Spanned<gast::Statement>] {
        let gast::Statement::For(value) = &statement.node else {
            panic!("expected a loop")
        };
        &value.body
    }

    /// The shape a split quadratic form leaves behind: a producer loop naming
    /// the row and the contracted index, then the accumulation naming the
    /// column as well, then the element write.
    fn split_quadratic_body() -> Vec<gast::Spanned<gast::Statement>> {
        let inner = gast::Expression::binary(
            gast::BinaryOp::Mul,
            element("carried", &["k"]),
            element("b", &["column", "k"]),
        );
        vec![
            loop_over(
                "k",
                vec![write(
                    element("carried", &["k"]),
                    element("a", &["row", "k"]),
                )],
            ),
            write(local("total"), gast::Expression::Real(0.0)),
            loop_over(
                "k",
                vec![write(
                    local("total"),
                    gast::Expression::binary(gast::BinaryOp::Add, local("total"), inner),
                )],
            ),
            write(element("out", &["row", "column"]), local("total")),
        ]
    }

    #[test]
    fn the_producer_loop_lands_between_the_row_and_the_column_loop() {
        let nest = nest_tensor_loops(split_quadratic_body(), &iterators(), &[3, 4], Span::DUMMY);
        let [outer] = nest.as_slice() else {
            panic!("the nest is one outer loop")
        };
        assert_eq!(loop_iterator(outer), "row");
        let row = loop_body(outer);
        assert_eq!(
            row.len(),
            2,
            "the row loop holds the producer and the column loop"
        );
        assert_eq!(
            loop_iterator(&row[0]),
            "k",
            "the producer runs once per row"
        );
        assert_eq!(loop_iterator(&row[1]), "column");
        let column = loop_body(&row[1]);
        assert_eq!(
            column.len(),
            3,
            "the reset, the accumulation and the element write stay per element"
        );
    }

    #[test]
    fn a_call_pins_every_statement_to_the_innermost_loop() {
        let mut body = split_quadratic_body();
        body.push(gast::Spanned::dummy(gast::Statement::Call(
            gast::FunctionCall {
                function: gast::Name::ident("record"),
                arguments: vec![local("total")],
            },
        )));
        assert!(!is_reorderable(&body));
        let nest = nest_tensor_loops(body, &iterators(), &[3, 4], Span::DUMMY);
        let [outer] = nest.as_slice() else {
            panic!("the nest is one outer loop")
        };
        let row = loop_body(outer);
        assert_eq!(row.len(), 1, "nothing is lifted past the call");
        assert_eq!(loop_iterator(&row[0]), "column");
        assert_eq!(
            loop_body(&row[0]).len(),
            5,
            "the whole body stays innermost"
        );
    }
}
