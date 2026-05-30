use super::*;
use rumoca_core::{ExpressionRewriter, StatementRewriter};

pub(super) fn substitute_expr_with_bindings(
    expr: &Expression,
    bindings: &HashMap<String, i64>,
) -> Expression {
    BindingSubstitutionRewriter { bindings }.rewrite_expression(expr)
}

struct BindingSubstitutionRewriter<'a> {
    bindings: &'a HashMap<String, i64>,
}

impl ExpressionRewriter for BindingSubstitutionRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        match expr {
            Expression::VarRef {
                name,
                subscripts,
                span,
            } if subscripts.is_empty() && self.bindings.contains_key(name.as_str()) => {
                Expression::Literal {
                    value: Literal::Integer(self.bindings[name.as_str()]),
                    span: *span,
                }
            }
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                span,
            } => self.rewrite_array_comprehension(expr, indices, filter, *span),
            _ => self.walk_expression(expr),
        }
    }
}

impl StatementRewriter for BindingSubstitutionRewriter<'_> {
    fn rewrite_statement(&mut self, statement: &Statement) -> Statement {
        if let Statement::For {
            indices,
            equations,
            span,
        } = statement
        {
            let nested_bindings = bindings_without_for_indices(self.bindings, indices);
            return Statement::For {
                indices: self.rewrite_for_indices(indices),
                equations: BindingSubstitutionRewriter {
                    bindings: &nested_bindings,
                }
                .rewrite_statements(equations),
                span: *span,
            };
        }
        self.walk_statement(statement)
    }
}

impl BindingSubstitutionRewriter<'_> {
    fn rewrite_array_comprehension(
        &mut self,
        expr: &Expression,
        indices: &[ComprehensionIndex],
        filter: &Option<Box<Expression>>,
        span: Span,
    ) -> Expression {
        let local_bindings = bindings_without_comprehension_indices(self.bindings, indices);
        Expression::ArrayComprehension {
            expr: Box::new(substitute_expr_with_bindings(expr, &local_bindings)),
            indices: indices
                .iter()
                .map(|index| ComprehensionIndex {
                    name: index.name.clone(),
                    range: self.rewrite_expression(&index.range),
                })
                .collect(),
            filter: filter.as_ref().map(|filter_expr| {
                Box::new(substitute_expr_with_bindings(filter_expr, &local_bindings))
            }),
            span,
        }
    }
}

pub(super) fn bindings_without_comprehension_indices(
    bindings: &HashMap<String, i64>,
    indices: &[ComprehensionIndex],
) -> HashMap<String, i64> {
    let mut local_bindings = bindings.clone();
    for index in indices {
        local_bindings.remove(&index.name);
    }
    local_bindings
}

pub(super) fn substitute_statement_with_bindings(
    statement: &Statement,
    bindings: &HashMap<String, i64>,
) -> Statement {
    BindingSubstitutionRewriter { bindings }.rewrite_statement(statement)
}

pub(super) fn bindings_without_for_indices(
    bindings: &HashMap<String, i64>,
    indices: &[rumoca_core::ForIndex],
) -> HashMap<String, i64> {
    let mut nested_bindings = bindings.clone();
    for index in indices {
        nested_bindings.remove(&index.ident);
    }
    nested_bindings
}
