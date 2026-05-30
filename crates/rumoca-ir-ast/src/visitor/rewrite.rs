use crate::{ComponentReference, Expression, ForIndex, Subscript};
use std::sync::Arc;

/// Trait for transforming expressions.
///
/// Override specific `transform_*` methods to customize behavior.
/// Default implementations recursively transform children.
pub trait ExpressionTransformer {
    /// Transform any expression.
    // SPEC_0021: Exception - exhaustive visitor transform over AST expression variants.
    #[allow(clippy::too_many_lines)]
    fn transform_expression(&mut self, expr: Expression) -> Expression {
        match expr {
            Expression::Empty { span } => Expression::Empty { span },
            Expression::Terminal {
                terminal_type,
                token,
                span,
            } => Expression::Terminal {
                terminal_type,
                token,
                span,
            },
            Expression::Range {
                start,
                step,
                end,
                span,
            } => Expression::Range {
                start: Arc::new(self.transform_expression((*start).clone())),
                step: step.map(|s| Arc::new(self.transform_expression((*s).clone()))),
                end: Arc::new(self.transform_expression((*end).clone())),
                span,
            },
            Expression::Unary { op, rhs, span } => Expression::Unary {
                op,
                rhs: Arc::new(self.transform_expression((*rhs).clone())),
                span,
            },
            Expression::Binary { op, lhs, rhs, span } => Expression::Binary {
                op,
                lhs: Arc::new(self.transform_expression((*lhs).clone())),
                rhs: Arc::new(self.transform_expression((*rhs).clone())),
                span,
            },
            Expression::ComponentReference(cr) => self.transform_component_reference(cr),
            Expression::FunctionCall { comp, args, span } => {
                self.transform_function_call(comp, args, span)
            }
            Expression::ClassModification {
                target,
                modifications,
                each_flags,
                final_flags,
                redeclare_flags,
                span,
            } => Expression::ClassModification {
                target: self.transform_component_ref_inner(target),
                modifications: modifications
                    .into_iter()
                    .map(|m| self.transform_expression(m))
                    .collect(),
                each_flags,
                final_flags,
                redeclare_flags,
                span,
            },
            Expression::NamedArgument { name, value, span } => Expression::NamedArgument {
                name,
                value: Arc::new(self.transform_expression((*value).clone())),
                span,
            },
            Expression::Modification {
                target,
                value,
                span,
            } => Expression::Modification {
                target: self.transform_component_ref_inner(target),
                value: Arc::new(self.transform_expression((*value).clone())),
                span,
            },
            Expression::Array {
                elements,
                is_matrix,
                span,
            } => Expression::Array {
                elements: elements
                    .into_iter()
                    .map(|e| self.transform_expression(e))
                    .collect(),
                is_matrix,
                span,
            },
            Expression::Tuple { elements, span } => Expression::Tuple {
                elements: elements
                    .into_iter()
                    .map(|e| self.transform_expression(e))
                    .collect(),
                span,
            },
            Expression::If {
                branches,
                else_branch,
                span,
            } => Expression::If {
                branches: branches
                    .into_iter()
                    .map(|(c, t)| (self.transform_expression(c), self.transform_expression(t)))
                    .collect(),
                else_branch: Arc::new(self.transform_expression((*else_branch).clone())),
                span,
            },
            Expression::Parenthesized { inner, span } => Expression::Parenthesized {
                inner: Arc::new(self.transform_expression((*inner).clone())),
                span,
            },
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                span,
            } => Expression::ArrayComprehension {
                expr: Arc::new(self.transform_expression((*expr).clone())),
                indices: indices
                    .into_iter()
                    .map(|idx| self.transform_for_index(idx))
                    .collect(),
                filter: filter.map(|f| Arc::new(self.transform_expression((*f).clone()))),
                span,
            },
            Expression::ArrayIndex {
                base,
                subscripts,
                span,
            } => Expression::ArrayIndex {
                base: Arc::new(self.transform_expression((*base).clone())),
                subscripts: subscripts
                    .into_iter()
                    .map(|s| self.transform_subscript(s))
                    .collect(),
                span,
            },
            Expression::FieldAccess { base, field, span } => Expression::FieldAccess {
                base: Arc::new(self.transform_expression((*base).clone())),
                field,
                span,
            },
        }
    }

    /// Transform a component reference expression.
    fn transform_component_reference(&mut self, cr: ComponentReference) -> Expression {
        Expression::ComponentReference(self.transform_component_ref_inner(cr))
    }

    /// Transform a ComponentReference struct (internal helper).
    fn transform_component_ref_inner(&mut self, mut cr: ComponentReference) -> ComponentReference {
        for part in &mut cr.parts {
            if let Some(subscripts) = &mut part.subs {
                *subscripts = subscripts
                    .drain(..)
                    .map(|subscript| self.transform_subscript(subscript))
                    .collect();
            }
        }
        cr
    }

    /// Transform a function call.
    fn transform_function_call(
        &mut self,
        comp: ComponentReference,
        args: Vec<Expression>,
        span: rumoca_core::Span,
    ) -> Expression {
        Expression::FunctionCall {
            comp: self.transform_component_ref_inner(comp),
            args: args
                .into_iter()
                .map(|a| self.transform_expression(a))
                .collect(),
            span,
        }
    }

    /// Transform a for-loop index.
    fn transform_for_index(&mut self, idx: ForIndex) -> ForIndex {
        ForIndex {
            ident: idx.ident,
            range: self.transform_expression(idx.range),
        }
    }

    /// Transform a subscript.
    fn transform_subscript(&mut self, sub: Subscript) -> Subscript {
        match sub {
            Subscript::Expression(expr) => Subscript::Expression(self.transform_expression(expr)),
            other => other,
        }
    }
}
