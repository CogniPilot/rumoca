//! Fixed-extent function slices written as one-iterator comprehensions.
//!
//! MLS §10.4.1 sizes `a:s:b` from `b - a` and `s` alone, so a slice whose
//! bounds read a loop index, such as `gray[row - radius:row + radius]`, has an
//! exact extent whenever its bounds differ by a translation-time Integer. That
//! distance is the one the function shape proof sizes the slice by
//! ([`exact_range_distance`]), evaluated in the same lexical scope: each
//! enclosing loop or comprehension index shadows any settled value of its name
//! (MLS §11.2.2), and every other operand the specialization settles, such as a
//! protected `constant Integer radius` or `size(rgb, 1)`, folds (MLS §12.2).
//! The rewrite keeps the original start expression, so the view reads the base
//! at run time through one compact index map; a slice without such a distance
//! is left unchanged for the shape proof to refuse.

use super::*;
use crate::construction::function_shapes::exact_range_distance;
use rumoca_core::{ComprehensionIndex, ForIndex};

pub(super) struct AffineSliceRanges {
    shapes: ShapeEnvironment,
}

impl AffineSliceRanges {
    pub(super) fn new(shapes: &ShapeEnvironment) -> Self {
        Self {
            shapes: shapes.clone(),
        }
    }

    /// Run `body` with `indices` bound left to right as lexical binders.
    fn in_binder_scope<'a, T>(
        &mut self,
        indices: impl IntoIterator<Item = (&'a str, &'a Expression)>,
        body: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let mut scoped = self.shapes.clone();
        for (name, range) in indices {
            scoped.bind_range_binder(VarName::new(name), range);
        }
        let outer = std::mem::replace(&mut self.shapes, scoped);
        let result = body(self);
        self.shapes = outer;
        result
    }

    fn affine_slice_subscript(&self, rewritten: &Expression, span: Span) -> Option<Subscript> {
        let Expression::Range {
            start,
            step,
            end,
            span: range_span,
        } = rewritten
        else {
            return None;
        };
        let mut references = Vec::new();
        start.collect_var_refs(&mut references);
        end.collect_var_refs(&mut references);
        if references.is_empty() {
            return None;
        }
        let step_value = match step.as_deref() {
            None => 1,
            Some(step) => self
                .shapes
                .proven_extent(step)
                .filter(|value| *value != 0)?,
        };
        let distance = exact_range_distance(start, end, &self.shapes)?;
        if distance.checked_rem(step_value) != Some(0) {
            return None;
        }
        let extent = distance
            .checked_div(step_value)
            .and_then(|value| value.checked_add(1))?;
        if extent <= 0 {
            return None;
        }
        let binder = rumoca_core::affine_slice_binder_name(range_span.start.0);
        let span_of = *range_span;
        let binary = |op, lhs, rhs| Expression::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: span_of,
        };
        let literal = |value| Expression::Literal {
            value: Literal::Integer(value),
            span: span_of,
        };
        let binder_reference = Expression::VarRef {
            name: Reference::generated(&binder),
            subscripts: Vec::new(),
            span: span_of,
        };
        let coordinate = binary(
            OpBinary::Add,
            start.as_ref().clone(),
            binary(
                OpBinary::Mul,
                binary(OpBinary::Sub, binder_reference, literal(1)),
                literal(step_value),
            ),
        );
        Some(Subscript::Expr {
            expr: Box::new(Expression::ArrayComprehension {
                expr: Box::new(coordinate),
                indices: vec![ComprehensionIndex {
                    name: binder,
                    range: integer_range(1, extent, span_of),
                }],
                filter: None,
                span: span_of,
            }),
            span,
        })
    }
}

impl ExpressionRewriter for AffineSliceRanges {
    fn rewrite_subscript(&mut self, subscript: &Subscript) -> Subscript {
        let Subscript::Expr { expr, span } = subscript else {
            return subscript.clone();
        };
        let rewritten = self.rewrite_expression(expr);
        self.affine_slice_subscript(&rewritten, *span)
            .unwrap_or(Subscript::Expr {
                expr: Box::new(rewritten),
                span: *span,
            })
    }

    fn walk_array_comprehension_expression(
        &mut self,
        expr: &Expression,
        indices: &[ComprehensionIndex],
        filter: Option<&Expression>,
        span: Span,
    ) -> Expression {
        let rewritten_indices = self.rewrite_comprehension_indices(indices);
        let binders = indices
            .iter()
            .map(|index| (index.name.as_str(), &index.range));
        self.in_binder_scope(binders, |scope| Expression::ArrayComprehension {
            expr: Box::new(scope.rewrite_expression(expr)),
            indices: rewritten_indices,
            filter: filter.map(|filter| Box::new(scope.rewrite_expression(filter))),
            span,
        })
    }
}

impl StatementRewriter for AffineSliceRanges {
    fn rewrite_statement(&mut self, statement: &rumoca_core::Statement) -> rumoca_core::Statement {
        let rumoca_core::Statement::For {
            indices,
            equations,
            span,
        } = statement
        else {
            return self.walk_statement(statement);
        };
        let rewritten_indices = self.rewrite_for_indices(indices);
        let binders = indices
            .iter()
            .map(|index: &ForIndex| (index.ident.as_str(), &index.range));
        self.in_binder_scope(binders, |scope| rumoca_core::Statement::For {
            indices: rewritten_indices,
            equations: scope.rewrite_statements(equations),
            span: *span,
        })
    }
}
