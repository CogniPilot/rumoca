use crate::{ComponentReference, Expression, ForIndex, Subscript};
use std::sync::Arc;

/// Trait for transforming expressions.
///
/// Override specific `transform_*` methods to customize behavior.
/// Default implementations recursively transform children.
pub trait ExpressionTransformer {
    /// Transform any expression.
    fn transform_expression(&mut self, expr: Expression) -> Expression {
        self.walk_expression(expr)
    }

    /// Transform an `Arc`-held child without deep-copying the subtree.
    ///
    /// When the `Arc` is uniquely owned (the overwhelmingly common case for a
    /// rewrite pass that owns its tree) the existing allocation is reused and
    /// the child is transformed in place. When the node is shared, the trait
    /// falls back to copy-on-write so the other holder keeps the old subtree.
    fn transform_arc(&mut self, mut arc: Arc<Expression>) -> Arc<Expression> {
        let Some(slot) = Arc::get_mut(&mut arc) else {
            return Arc::new(self.transform_expression((*arc).clone()));
        };
        // The placeholder only has to survive until the line below overwrites
        // it; it reuses the child's own span so no dummy provenance is minted.
        let placeholder = Expression::Empty { span: slot.span() };
        let taken = std::mem::replace(slot, placeholder);
        *slot = self.transform_expression(taken);
        arc
    }

    /// Recursively transform an expression using the default traversal.
    // SPEC_0021: Exception - exhaustive visitor transform over AST expression variants.
    #[allow(clippy::too_many_lines)]
    fn walk_expression(&mut self, expr: Expression) -> Expression {
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
                start: self.transform_arc(start),
                step: step.map(|s| self.transform_arc(s)),
                end: self.transform_arc(end),
                span,
            },
            Expression::Unary { op, rhs, span } => Expression::Unary {
                op,
                rhs: self.transform_arc(rhs),
                span,
            },
            Expression::Binary { op, lhs, rhs, span } => Expression::Binary {
                op,
                lhs: self.transform_arc(lhs),
                rhs: self.transform_arc(rhs),
                span,
            },
            Expression::ComponentReference(cr) => self.transform_component_reference(cr),
            Expression::FunctionCall {
                comp,
                args,
                is_partial_application,
                span,
            } => self.transform_function_call(comp, args, is_partial_application, span),
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
                value: self.transform_arc(value),
                span,
            },
            Expression::Modification {
                target,
                value,
                span,
            } => Expression::Modification {
                target: self.transform_component_ref_inner(target),
                value: self.transform_arc(value),
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
                else_branch: self.transform_arc(else_branch),
                span,
            },
            Expression::Parenthesized { inner, span } => Expression::Parenthesized {
                inner: self.transform_arc(inner),
                span,
            },
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                span,
            } => Expression::ArrayComprehension {
                expr: self.transform_arc(expr),
                indices: indices
                    .into_iter()
                    .map(|idx| self.transform_for_index(idx))
                    .collect(),
                filter: filter.map(|f| self.transform_arc(f)),
                span,
            },
            Expression::ArrayIndex {
                base,
                subscripts,
                span,
            } => Expression::ArrayIndex {
                base: self.transform_arc(base),
                subscripts: subscripts
                    .into_iter()
                    .map(|s| self.transform_subscript(s))
                    .collect(),
                span,
            },
            Expression::FieldAccess {
                base,
                field,
                field_def_id,
                span,
            } => Expression::FieldAccess {
                base: self.transform_arc(base),
                field,
                field_def_id,
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
        is_partial_application: bool,
        span: rumoca_core::Span,
    ) -> Expression {
        Expression::FunctionCall {
            comp: self.transform_component_ref_inner(comp),
            args: args
                .into_iter()
                .map(|a| self.transform_expression(a))
                .collect(),
            is_partial_application,
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
