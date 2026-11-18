//! Scalar extrema of a runtime-sized, unit-stride Real slice (MLS §10.3.4,
//! §10.4.1, §10.5). The base has a fixed nonempty extent: a guarded compact
//! comprehension over that extent preserves exactly the selected elements.
//! The adjacent assertion preserves the original nonempty slice's index
//! obligation. Empty slices use the finite Real type extrema, never zero.

use super::super::guard_facts::FactScope;
use super::*;
use rumoca_core::{ComprehensionIndex, IndexBinderProvenance, Statement};

pub(super) fn normalize_extremum_slices(
    statements: &[Statement],
    scope: FactScope<'_>,
) -> Vec<Statement> {
    ExtremumSlices {
        scope,
        shadowed: HashSet::new(),
    }
    .rewrite_statements(statements)
}

struct ExtremumSlices<'a> {
    scope: FactScope<'a>,
    shadowed: HashSet<VarName>,
}

impl ExpressionRewriter for ExtremumSlices<'_> {}

impl StatementRewriter for ExtremumSlices<'_> {
    fn rewrite_statements(&mut self, statements: &[Statement]) -> Vec<Statement> {
        let mut result = Vec::with_capacity(statements.len());
        for statement in statements {
            let rewritten = self.rewrite_statement(statement);
            if let Some((assertion, assignment)) = self.normalize_assignment(&rewritten) {
                result.extend([assertion, assignment]);
            } else {
                result.push(rewritten);
            }
        }
        result
    }

    fn rewrite_statement(&mut self, statement: &Statement) -> Statement {
        let Statement::For { indices, .. } = statement else {
            return self.walk_statement(statement);
        };
        // A loop may shadow a function declaration with a different type.
        // Do not borrow the outer declaration's kind for any such reference.
        let outer = self.shadowed.clone();
        self.shadowed
            .extend(indices.iter().map(|index| VarName::new(&index.ident)));
        let result = self.walk_statement(statement);
        self.shadowed = outer;
        result
    }
}

impl ExtremumSlices<'_> {
    fn normalize_assignment(&self, statement: &Statement) -> Option<(Statement, Statement)> {
        let Statement::Assignment { comp, value, span } = statement else {
            return None;
        };
        // Do not move a slice check ahead of an effectful target index.
        if comp.parts().iter().any(|part| {
            part.subs.iter().any(|subscript| match subscript {
                Subscript::Index { .. } => false,
                Subscript::Expr { expr, .. } => !matches!(
                    expr.as_ref(),
                    Expression::Literal {
                        value: Literal::Integer(_),
                        ..
                    }
                ),
                _ => true,
            })
        }) {
            return None;
        }
        let (condition, value, call_span) = self.normalize_reduction(value)?;
        Some((
            Statement::Assert {
                condition,
                message: Box::new(Expression::Literal {
                    value: Literal::String("slice index out of bounds".to_owned()),
                    span: call_span,
                }),
                level: None,
                span: call_span,
            },
            Statement::Assignment {
                comp: comp.clone(),
                value,
                span: *span,
            },
        ))
    }

    fn normalize_reduction(&self, value: &Expression) -> Option<(Expression, Expression, Span)> {
        let Expression::BuiltinCall {
            function: function @ (BuiltinFunction::Min | BuiltinFunction::Max),
            args,
            span: call_span,
        } = value
        else {
            return None;
        };
        let [operand] = args.as_slice() else {
            return None;
        };
        let (name, subscripts) = match operand {
            Expression::VarRef {
                name, subscripts, ..
            } => (name, subscripts),
            Expression::Index {
                base, subscripts, ..
            } => match base.as_ref() {
                Expression::VarRef {
                    name,
                    subscripts: outer,
                    ..
                } if outer.is_empty() => (name, subscripts),
                _ => return None,
            },
            _ => return None,
        };
        if self.shadowed.contains(name.var_name()) || !self.scope.reals.contains(name.var_name()) {
            return None;
        }
        let [extent] = self.scope.shapes.get(name.var_name())?.as_slice() else {
            return None;
        };
        if *extent == 0 {
            return None;
        }
        let [Subscript::Expr { expr, .. }] = subscripts.as_slice() else {
            return None;
        };
        let Expression::Range {
            start,
            step,
            end,
            span: range_span,
        } = expr.as_ref()
        else {
            return None;
        };
        // Already-settled slices retain their existing, smaller domain.
        if self.scope.shapes.proven_extent(start).is_some()
            && self.scope.shapes.proven_extent(end).is_some()
        {
            return None;
        }
        if step.as_deref().is_some_and(|step| {
            !matches!(
                step,
                Expression::Literal {
                    value: Literal::Integer(1),
                    ..
                }
            )
        }) || !self.integer_bound(start)
            || !self.integer_bound(end)
        {
            return None;
        }
        let (condition, value) =
            guarded_extremum(*function, name, start, end, *extent, *range_span);
        Some((condition, value, *call_span))
    }

    fn integer_bound(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Literal {
                value: Literal::Integer(_),
                ..
            } => true,
            Expression::VarRef {
                name, subscripts, ..
            } => {
                subscripts.is_empty()
                    && !self.shadowed.contains(name.var_name())
                    && self.scope.integers.contains(name.var_name())
                    && self
                        .scope
                        .shapes
                        .get(name.var_name())
                        .is_some_and(Vec::is_empty)
            }
            Expression::Binary {
                op: OpBinary::Add | OpBinary::Sub,
                lhs,
                rhs,
                ..
            } => self.integer_bound(lhs) && self.integer_bound(rhs),
            _ => false,
        }
    }
}

fn guarded_extremum(
    function: BuiltinFunction,
    name: &Reference,
    start: &Expression,
    end: &Expression,
    extent: u32,
    span: Span,
) -> (Expression, Expression) {
    let binary = |op, lhs, rhs| Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    };
    let integer = |value| Expression::Literal {
        value: Literal::Integer(value),
        span,
    };
    let condition = binary(
        OpBinary::Or,
        binary(OpBinary::Gt, start.clone(), end.clone()),
        binary(
            OpBinary::And,
            binary(OpBinary::Ge, start.clone(), integer(1)),
            binary(OpBinary::Le, end.clone(), integer(i64::from(extent))),
        ),
    );
    let binder_name = rumoca_core::affine_slice_binder_name(span.start.0);
    let active = binary(
        OpBinary::And,
        binary(
            OpBinary::Ge,
            generated_slice_binder(&binder_name, span),
            start.clone(),
        ),
        binary(
            OpBinary::Le,
            generated_slice_binder(&binder_name, span),
            end.clone(),
        ),
    );
    let element = Expression::If {
        branches: vec![(
            active,
            Expression::VarRef {
                name: name.clone(),
                subscripts: vec![binder_subscript(&binder_name, span)],
                span,
            },
        )],
        else_branch: Box::new(Expression::Literal {
            value: Literal::Real(if function == BuiltinFunction::Max {
                -f64::MAX
            } else {
                f64::MAX
            }),
            span,
        }),
        span,
    };
    let value = Expression::BuiltinCall {
        function,
        args: vec![Expression::ArrayComprehension {
            expr: Box::new(element),
            indices: vec![ComprehensionIndex {
                binder: IndexBinderProvenance::Generated,
                name: binder_name,
                range: integer_range(1, i64::from(extent), span),
            }],
            filter: None,
            span,
        }],
        span,
    };
    (condition, value)
}
