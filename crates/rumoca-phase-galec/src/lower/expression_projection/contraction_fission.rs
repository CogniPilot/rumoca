//! The retired loop-fission recognizer for materialized tensor contractions,
//! kept as the assertion that its pattern is unreachable.
//!
//! A quadratic form `A*X*A'` used to lower to a contraction whose left operand
//! was itself a contraction, leaving the inner product
//! `(A*X)[row][contracted]` inside the loop over the result column even though
//! no value of that column could change it. This pass recovered the shape
//! afterwards, by splitting the emitted body at the outer index and widening
//! the scalars the invariant half defined into arrays.
//!
//! The composed contraction now carries that structure from the checked DAE
//! ([`super::composed_contraction`]): the intermediate tensor is an explicit
//! IR value decided on the contraction node, so no body reaches emission with
//! a split still owed. What survives here is the recognizer alone, called
//! under `debug_assertions` on every contraction body the projection emits and
//! required to find nothing. The rewriter it used to drive is gone; when the
//! assertion has ridden a release silently, this module goes with it.

use super::*;

/// A contraction body split at the outer tensor index.
pub(super) struct ContractionFission {
    /// The half no value of the outer index can change.
    produced: Vec<gast::Spanned<gast::Statement>>,
    /// The half that still varies with the outer index.
    consumed: Vec<gast::Spanned<gast::Statement>>,
    /// Scalars `produced` defines and the rest of the contraction reads; each
    /// would become an array over the contracted index.
    carried: Vec<gast::Name>,
}

impl ContractionFission {
    /// The split this body would have taken, for the retirement assertion to
    /// report when it finds one.
    pub(super) fn describe(&self) -> String {
        format!(
            "{} hoistable statement(s), {} left with the accumulation, carrying {:?}",
            self.produced.len(),
            self.consumed.len(),
            self.carried
                .iter()
                .map(|name| name.lexeme().to_owned())
                .collect::<Vec<_>>()
        )
    }
}

/// Split a contraction body so the work the outer index cannot change runs
/// once per contracted value.
///
/// Returns `None` when the split would buy nothing: when the contraction has
/// no outer index to be replicated over, when the invariant half carries no
/// loop of its own, or when it hands nothing on to the accumulation.
pub(super) fn fission_contraction_body(
    body: &[gast::Spanned<gast::Statement>],
    outer: &[gast::Expression],
    product: &gast::Expression,
) -> Option<ContractionFission> {
    let mut outer_names = Vec::new();
    for index in outer {
        read_expression_names(index, &mut outer_names);
    }
    if outer_names.is_empty() || !user_functions::is_reorderable(body) {
        return None;
    }
    let (produced, consumed) = user_functions::partition_by_dependence(body.to_vec(), &outer_names);
    if !produced.iter().any(is_repeated_work) {
        return None;
    }
    let mut defined = Vec::new();
    for statement in &produced {
        user_functions::collect_defined_names(statement, &mut defined);
    }
    let carried = defined
        .into_iter()
        .filter(|name| is_read_downstream(name, &consumed, product))
        .collect::<Vec<_>>();
    if carried.is_empty() {
        return None;
    }
    Some(ContractionFission {
        produced,
        consumed,
        carried,
    })
}

/// Whether a statement's cost grows with a loop bound, which is what makes
/// evaluating it once per row instead of once per element worth a temporary.
fn is_repeated_work(statement: &gast::Spanned<gast::Statement>) -> bool {
    matches!(statement.node, gast::Statement::For(_))
}

fn is_read_downstream(
    name: &gast::Name,
    consumed: &[gast::Spanned<gast::Statement>],
    product: &gast::Expression,
) -> bool {
    let names = std::slice::from_ref(name);
    user_functions::expression_depends_on(product, names)
        || consumed
            .iter()
            .any(|statement| user_functions::statement_depends_on(statement, names))
}

fn read_expression_names(expression: &gast::Expression, names: &mut Vec<gast::Name>) {
    match expression {
        gast::Expression::Bool(_) | gast::Expression::Integer(_) | gast::Expression::Real(_) => {}
        gast::Expression::Ref(reference) | gast::Expression::Neg(reference) => {
            read_reference_names(reference, names);
        }
        gast::Expression::Size { array, dimension } => {
            read_reference_names(array, names);
            read_expression_names(dimension, names);
        }
        gast::Expression::Call(call) => {
            for argument in &call.arguments {
                read_expression_names(argument, names);
            }
        }
        gast::Expression::Paren(value) | gast::Expression::Not(value) => {
            read_expression_names(value, names);
        }
        gast::Expression::If(value) => read_conditional_names(value, names),
        gast::Expression::Array(values) => {
            for value in values {
                read_expression_names(value, names);
            }
        }
        gast::Expression::Binary { lhs, rhs, .. } => {
            read_expression_names(lhs, names);
            read_expression_names(rhs, names);
        }
    }
}

fn read_conditional_names(value: &gast::IfExpression, names: &mut Vec<gast::Name>) {
    for (condition, branch) in &value.branches {
        read_expression_names(condition, names);
        read_expression_names(branch, names);
    }
    read_expression_names(&value.else_value, names);
}

fn read_reference_names(reference: &gast::Reference, names: &mut Vec<gast::Name>) {
    let gast::Reference::Local(part) = reference else {
        return;
    };
    if !names.contains(&part.name) {
        names.push(part.name.clone());
    }
    for subscript in &part.subscripts {
        read_expression_names(subscript, names);
    }
}

#[cfg(test)]
mod tests {
    use super::fission_contraction_body;
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

    fn assign(target: &str, value: gast::Expression) -> gast::Spanned<gast::Statement> {
        gast::Spanned::dummy(gast::Statement::Assignment {
            target: gast::Reference::local(gast::Name::ident(target)),
            value,
        })
    }

    fn accumulate(target: &str, term: gast::Expression) -> gast::Spanned<gast::Statement> {
        assign(
            target,
            gast::Expression::binary(gast::BinaryOp::Add, local(target), term),
        )
    }

    fn loop_over(
        iterator: &str,
        extent: i64,
        body: gast::Spanned<gast::Statement>,
    ) -> gast::Spanned<gast::Statement> {
        gast::Spanned::dummy(gast::Statement::for_loop(gast::ForLoop::new(
            Some(gast::Name::ident(iterator)),
            gast::Expression::Integer(1),
            None,
            gast::Expression::Integer(extent),
            vec![body],
        )))
    }

    fn product(lhs: gast::Expression, rhs: gast::Expression) -> gast::Expression {
        gast::Expression::binary(gast::BinaryOp::Mul, lhs, rhs)
    }

    /// `(A*X)[row][contracted]`: the inner accumulator names the row and the
    /// contracted index, never the result column.
    fn left_nested_body() -> Vec<gast::Spanned<gast::Statement>> {
        let term = product(element("a", &["row", "l"]), element("x", &["l", "k"]));
        vec![
            assign("inner", gast::Expression::Real(0.0)),
            loop_over("l", 12, accumulate("inner", term)),
        ]
    }

    /// `(X*A')[contracted][column]`: the inner accumulator names the result
    /// column, so no value of it may be reused across columns.
    fn right_nested_body() -> Vec<gast::Spanned<gast::Statement>> {
        let term = product(element("x", &["k", "l"]), element("a", &["column", "l"]));
        vec![
            assign("inner", gast::Expression::Real(0.0)),
            loop_over("l", 12, accumulate("inner", term)),
        ]
    }

    #[test]
    fn a_left_nested_contraction_hoists_the_half_the_result_column_cannot_change() {
        let body = left_nested_body();
        let term = product(local("inner"), element("a", &["column", "k"]));
        let fission = fission_contraction_body(&body, &[local("column")], &term)
            .expect("the inner product does not read the result column");
        assert_eq!(fission.produced.len(), 2);
        assert!(fission.consumed.is_empty());
        assert_eq!(
            fission.carried,
            vec![gast::Name::ident("inner")],
            "the inner accumulator is the only value handed to the accumulation"
        );
    }

    #[test]
    fn an_inner_accumulator_that_reads_the_result_column_is_not_hoisted() {
        let body = right_nested_body();
        let term = product(element("a", &["row", "k"]), local("inner"));
        assert!(
            fission_contraction_body(&body, &[local("column")], &term).is_none(),
            "an accumulator that varies with the result column has no reusable value"
        );
    }

    #[test]
    fn a_contraction_without_an_outer_index_is_not_split() {
        let body = left_nested_body();
        let term = product(local("inner"), element("a", &["k"]));
        assert!(
            fission_contraction_body(&body, &[], &term).is_none(),
            "a dot product replicates over nothing, so splitting it buys nothing"
        );
        assert!(
            fission_contraction_body(&body, &[gast::Expression::Integer(3)], &term).is_none(),
            "a literal outer index replicates over nothing either"
        );
    }

    #[test]
    fn a_body_whose_invariant_half_carries_no_loop_is_not_split() {
        let body = vec![assign("inner", element("a", &["row", "k"]))];
        let term = product(local("inner"), element("a", &["column", "k"]));
        assert!(
            fission_contraction_body(&body, &[local("column")], &term).is_none(),
            "a single scalar read costs less than the array it would need"
        );
    }
}
