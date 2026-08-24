//! Loop fission for materialized tensor contractions.
//!
//! A quadratic form `A*X*A'` lowers to a contraction whose left operand is
//! itself a contraction, so the inner product `(A*X)[row][contracted]` sits
//! inside the loop over the result column even though it does not vary with
//! that column. Splitting the contraction body at the outer index, and
//! widening the scalars the invariant half defines into arrays over the
//! contracted index, evaluates that half once per row instead of once per
//! element. The split is a property of the statements, not of any particular
//! model: it fires wherever a contraction body carries work the outer index
//! cannot change.

use super::*;

/// A contraction body split at the outer tensor index.
pub(super) struct ContractionFission {
    /// The half no value of the outer index can change.
    pub(super) produced: Vec<gast::Spanned<gast::Statement>>,
    /// The half that still varies with the outer index.
    pub(super) consumed: Vec<gast::Spanned<gast::Statement>>,
    /// Scalars `produced` defines and the rest of the contraction reads; each
    /// becomes an array over the contracted index.
    pub(super) carried: Vec<gast::Name>,
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

/// Subscript every reference to a carried scalar with the contracted index, so
/// the two halves of the split contraction address the same array element.
///
/// Reports `false` when a carried scalar is read inside a conditional
/// expression, whose legalized form is held beside the branches and would not
/// see the new subscript. The caller then keeps the unsplit body.
#[must_use]
pub(super) fn subscript_carried_scalars(
    fission: &mut ContractionFission,
    product: &mut gast::Expression,
    contracted: &gast::Expression,
) -> bool {
    let ContractionFission {
        produced,
        consumed,
        carried,
    } = fission;
    let mut subscripter = Subscripter {
        carried,
        index: contracted,
        blocked: false,
    };
    subscripter.body(produced);
    subscripter.body(consumed);
    subscripter.expression(product);
    !subscripter.blocked
}

/// Rewrites references to the carried scalars into references to their widened
/// arrays, recording whether any reference sat where the rewrite cannot reach.
struct Subscripter<'a> {
    carried: &'a [gast::Name],
    index: &'a gast::Expression,
    blocked: bool,
}

impl Subscripter<'_> {
    fn body(&mut self, body: &mut [gast::Spanned<gast::Statement>]) {
        for statement in body {
            self.statement(statement);
        }
    }

    fn statement(&mut self, statement: &mut gast::Spanned<gast::Statement>) {
        match &mut statement.node {
            gast::Statement::Assignment { target, value } => {
                self.reference(target);
                self.expression(value);
            }
            gast::Statement::MultiAssignment { targets, call } => {
                for target in targets {
                    self.reference(target);
                }
                self.call(call);
            }
            gast::Statement::Call(call) => self.call(call),
            gast::Statement::If(value) => self.if_statement(value),
            gast::Statement::For(value) => self.for_loop(value),
            gast::Statement::Limit(targets) => self.limits(targets),
            gast::Statement::Signal(_) => {}
        }
    }

    fn limits(&mut self, targets: &mut [gast::LimitTarget]) {
        for target in targets {
            if let gast::LimitTarget::Reference(reference) = target {
                self.reference(reference);
            }
        }
    }

    fn if_statement(&mut self, value: &mut gast::IfStatement) {
        for branch in &mut value.branches {
            self.condition(&mut branch.condition);
            self.body(&mut branch.body);
        }
        if let Some(body) = &mut value.else_body {
            self.body(body);
        }
    }

    fn condition(&mut self, condition: &mut gast::Condition) {
        match condition {
            gast::Condition::Expression(expression) => self.expression(expression),
            gast::Condition::SignalCheck(check) => {
                if let Some(fallback) = &mut check.fallback {
                    self.expression(fallback);
                }
            }
        }
    }

    fn for_loop(&mut self, value: &mut gast::ForLoop) {
        self.expression(&mut value.start);
        if let Some(step) = &mut value.step {
            self.expression(step);
        }
        self.expression(&mut value.stop);
        self.body(&mut value.body);
    }

    fn call(&mut self, call: &mut gast::FunctionCall) {
        for argument in &mut call.arguments {
            self.expression(argument);
        }
    }

    fn expression(&mut self, expression: &mut gast::Expression) {
        match expression {
            gast::Expression::Bool(_)
            | gast::Expression::Integer(_)
            | gast::Expression::Real(_) => {}
            gast::Expression::Ref(reference) | gast::Expression::Neg(reference) => {
                self.reference(reference);
            }
            gast::Expression::Size { array, dimension } => {
                self.reference(array);
                self.expression(dimension);
            }
            gast::Expression::Call(call) => self.call(call),
            gast::Expression::Paren(value) | gast::Expression::Not(value) => {
                self.expression(value);
            }
            gast::Expression::If(value) => self.conditional(value),
            gast::Expression::Array(values) => {
                for value in values {
                    self.expression(value);
                }
            }
            gast::Expression::Binary { lhs, rhs, .. } => {
                self.expression(lhs);
                self.expression(rhs);
            }
        }
    }

    /// A conditional expression carries a legalized twin beside its branches,
    /// so rewriting the branches alone would leave the two disagreeing. Read
    /// the whole subtree instead and refuse the split if it names a carried
    /// scalar.
    fn conditional(&mut self, value: &gast::IfExpression) {
        let mut names = Vec::new();
        read_conditional_names(value, &mut names);
        if names.iter().any(|name| self.carried.contains(name)) {
            self.blocked = true;
        }
    }

    fn reference(&mut self, reference: &mut gast::Reference) {
        match reference {
            gast::Reference::Local(part) => {
                self.subscripts(std::slice::from_mut(part));
                if self.carried.contains(&part.name) {
                    part.subscripts.insert(0, self.index.clone());
                }
            }
            gast::Reference::State(parts) => self.subscripts(parts),
        }
    }

    fn subscripts(&mut self, parts: &mut [gast::RefPart]) {
        for part in parts {
            for subscript in &mut part.subscripts {
                self.expression(subscript);
            }
        }
    }
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
    use super::{fission_contraction_body, subscript_carried_scalars};
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

    fn assign_element(
        target: &str,
        index: &str,
        value: gast::Expression,
    ) -> gast::Spanned<gast::Statement> {
        gast::Spanned::dummy(gast::Statement::Assignment {
            target: gast::Reference::Local(gast::RefPart {
                name: gast::Name::ident(target),
                subscripts: vec![local(index)],
                span: Span::DUMMY,
            }),
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
        gast::Spanned::dummy(gast::Statement::For(gast::ForLoop {
            iterator: Some(gast::Name::ident(iterator)),
            start: gast::Expression::Integer(1),
            step: None,
            stop: gast::Expression::Integer(extent),
            body: vec![body],
        }))
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

    #[test]
    fn the_split_halves_address_the_carried_accumulator_by_the_contracted_index() {
        let body = left_nested_body();
        let mut term = product(local("inner"), element("a", &["column", "k"]));
        let mut fission = fission_contraction_body(&body, &[local("column")], &term)
            .expect("the inner product does not read the result column");
        assert!(subscript_carried_scalars(
            &mut fission,
            &mut term,
            &local("k")
        ));
        assert_eq!(
            term,
            product(element("inner", &["k"]), element("a", &["column", "k"]))
        );
        assert_eq!(
            fission.produced[0],
            assign_element("inner", "k", gast::Expression::Real(0.0)),
            "the reset writes the element this contracted value owns"
        );
    }

    /// A guard condition, a branch body, a call argument, and an `else` arm
    /// all read the carried scalar; every one of those reads must address the
    /// widened array, or the two halves of the split disagree on one of them.
    #[test]
    fn carried_reads_inside_guards_and_calls_take_the_contracted_subscript() {
        let guarded = gast::Spanned::dummy(gast::Statement::If(gast::IfStatement {
            branches: vec![gast::IfBranch {
                condition: gast::Condition::Expression(gast::Expression::binary(
                    gast::BinaryOp::Gt,
                    local("inner"),
                    gast::Expression::Real(0.0),
                )),
                body: vec![gast::Spanned::dummy(gast::Statement::Call(
                    gast::FunctionCall {
                        function: gast::Name::ident("observe"),
                        arguments: vec![local("inner")],
                    },
                ))],
                span: Span::DUMMY,
            }],
            else_body: Some(vec![assign("out", local("inner"))]),
        }));
        let mut fission = super::ContractionFission {
            produced: vec![assign("inner", gast::Expression::Real(0.0))],
            consumed: vec![guarded],
            carried: vec![gast::Name::ident("inner")],
        };
        let mut term = local("inner");
        assert!(subscript_carried_scalars(
            &mut fission,
            &mut term,
            &local("k")
        ));
        assert_eq!(term, element("inner", &["k"]));
        let gast::Statement::If(rewritten) = &fission.consumed[0].node else {
            panic!("the guard statement survives the rewrite")
        };
        assert_eq!(
            rewritten.branches[0].condition,
            gast::Condition::Expression(gast::Expression::binary(
                gast::BinaryOp::Gt,
                element("inner", &["k"]),
                gast::Expression::Real(0.0),
            )),
            "the guard reads the element this contracted value owns"
        );
        let gast::Statement::Call(call) = &rewritten.branches[0].body[0].node else {
            panic!("the call statement survives the rewrite")
        };
        assert_eq!(
            call.arguments,
            vec![element("inner", &["k"])],
            "the call hands over the element, not the collapsed scalar"
        );
        assert_eq!(
            rewritten.else_body,
            Some(vec![assign("out", element("inner", &["k"]))]),
            "the else arm reads the element as well"
        );
    }

    /// A conditional expression holds its legalized twin beside the branches,
    /// so a carried read inside one sits where the subscript cannot reach; the
    /// rewrite must refuse the split rather than leave the twin stale.
    #[test]
    fn a_carried_scalar_read_inside_a_conditional_expression_blocks_the_split() {
        let carried_conditional = gast::Expression::If(gast::IfExpression::new(
            vec![(local("p"), local("inner"))],
            gast::Expression::Real(1.0),
        ));
        let mut fission = super::ContractionFission {
            produced: vec![assign("inner", gast::Expression::Real(0.0))],
            consumed: vec![assign("out", carried_conditional)],
            carried: vec![gast::Name::ident("inner")],
        };
        let mut term = local("out");
        assert!(
            !subscript_carried_scalars(&mut fission, &mut term, &local("k")),
            "a read the rewrite cannot reach keeps the unsplit body"
        );

        let free_conditional = gast::Expression::If(gast::IfExpression::new(
            vec![(local("p"), local("q"))],
            gast::Expression::Real(1.0),
        ));
        let mut fission = super::ContractionFission {
            produced: vec![assign("inner", gast::Expression::Real(0.0))],
            consumed: vec![assign("out", free_conditional)],
            carried: vec![gast::Name::ident("inner")],
        };
        let mut term = local("inner");
        assert!(
            subscript_carried_scalars(&mut fission, &mut term, &local("k")),
            "a conditional that names no carried scalar blocks nothing"
        );
    }
}
