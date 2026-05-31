use crate::{
    BuiltinFunction, ComprehensionIndex, Expression, Literal, OpBinary, Reference, Span, Subscript,
};

pub trait ExpressionRewriter {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        self.walk_expression(expr)
    }

    fn walk_expression(&mut self, expr: &Expression) -> Expression {
        match expr {
            Expression::Binary { op, lhs, rhs, span } => {
                self.walk_binary_expression(op, lhs, rhs, *span)
            }
            Expression::Unary { op, rhs, span } => self.walk_unary_expression(op, rhs, *span),
            Expression::VarRef {
                name,
                subscripts,
                span,
            } => self.rewrite_var_ref_expression(name, subscripts, *span),
            Expression::BuiltinCall {
                function,
                args,
                span,
            } => self.walk_builtin_call_expression(*function, args, *span),
            Expression::FunctionCall {
                name,
                args,
                is_constructor,
                span,
            } => self.walk_function_call_expression(name, args, *is_constructor, *span),
            Expression::Literal { value, span } => self.walk_literal_expression(value, *span),
            Expression::If {
                branches,
                else_branch,
                span,
            } => self.walk_if_expression(branches, else_branch, *span),
            Expression::Array {
                elements,
                is_matrix,
                span,
            } => self.walk_array_expression(elements, *is_matrix, *span),
            Expression::Tuple { elements, span } => self.walk_tuple_expression(elements, *span),
            Expression::Range {
                start,
                step,
                end,
                span,
            } => self.walk_range_expression(start, step.as_deref(), end, *span),
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                span,
            } => self.walk_array_comprehension_expression(expr, indices, filter.as_deref(), *span),
            Expression::Index {
                base,
                subscripts,
                span,
            } => self.walk_index_expression(base, subscripts, *span),
            Expression::FieldAccess { base, field, span } => {
                self.walk_field_access_expression(base, field, *span)
            }
            Expression::Empty { span } => Expression::Empty { span: *span },
        }
    }

    fn walk_binary_expression(
        &mut self,
        op: &OpBinary,
        lhs: &Expression,
        rhs: &Expression,
        span: Span,
    ) -> Expression {
        Expression::Binary {
            op: op.clone(),
            lhs: Box::new(self.rewrite_expression(lhs)),
            rhs: Box::new(self.rewrite_expression(rhs)),
            span,
        }
    }

    fn walk_unary_expression(
        &mut self,
        op: &crate::OpUnary,
        rhs: &Expression,
        span: Span,
    ) -> Expression {
        Expression::Unary {
            op: op.clone(),
            rhs: Box::new(self.rewrite_expression(rhs)),
            span,
        }
    }

    fn walk_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Expression {
        Expression::VarRef {
            name: name.clone(),
            subscripts: self.rewrite_subscripts(subscripts),
            span,
        }
    }

    fn rewrite_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Expression {
        self.walk_var_ref_expression(name, subscripts, span)
    }

    fn walk_builtin_call_expression(
        &mut self,
        function: BuiltinFunction,
        args: &[Expression],
        span: Span,
    ) -> Expression {
        Expression::BuiltinCall {
            function,
            args: self.rewrite_expressions(args),
            span,
        }
    }

    fn walk_function_call_expression(
        &mut self,
        name: &Reference,
        args: &[Expression],
        is_constructor: bool,
        span: Span,
    ) -> Expression {
        Expression::FunctionCall {
            name: name.clone(),
            args: self.rewrite_expressions(args),
            is_constructor,
            span,
        }
    }

    fn walk_literal_expression(&mut self, value: &Literal, span: Span) -> Expression {
        Expression::Literal {
            value: value.clone(),
            span,
        }
    }

    fn walk_if_expression(
        &mut self,
        branches: &[(Expression, Expression)],
        else_branch: &Expression,
        span: Span,
    ) -> Expression {
        Expression::If {
            branches: branches
                .iter()
                .map(|(condition, value)| {
                    (
                        self.rewrite_expression(condition),
                        self.rewrite_expression(value),
                    )
                })
                .collect(),
            else_branch: Box::new(self.rewrite_expression(else_branch)),
            span,
        }
    }

    fn walk_array_expression(
        &mut self,
        elements: &[Expression],
        is_matrix: bool,
        span: Span,
    ) -> Expression {
        Expression::Array {
            elements: self.rewrite_expressions(elements),
            is_matrix,
            span,
        }
    }

    fn walk_tuple_expression(&mut self, elements: &[Expression], span: Span) -> Expression {
        Expression::Tuple {
            elements: self.rewrite_expressions(elements),
            span,
        }
    }

    fn walk_range_expression(
        &mut self,
        start: &Expression,
        step: Option<&Expression>,
        end: &Expression,
        span: Span,
    ) -> Expression {
        Expression::Range {
            start: Box::new(self.rewrite_expression(start)),
            step: step.map(|step| Box::new(self.rewrite_expression(step))),
            end: Box::new(self.rewrite_expression(end)),
            span,
        }
    }

    fn walk_array_comprehension_expression(
        &mut self,
        expr: &Expression,
        indices: &[ComprehensionIndex],
        filter: Option<&Expression>,
        span: Span,
    ) -> Expression {
        Expression::ArrayComprehension {
            expr: Box::new(self.rewrite_expression(expr)),
            indices: self.rewrite_comprehension_indices(indices),
            filter: filter.map(|filter| Box::new(self.rewrite_expression(filter))),
            span,
        }
    }

    fn walk_index_expression(
        &mut self,
        base: &Expression,
        subscripts: &[Subscript],
        span: Span,
    ) -> Expression {
        Expression::Index {
            base: Box::new(self.rewrite_expression(base)),
            subscripts: self.rewrite_subscripts(subscripts),
            span,
        }
    }

    fn walk_field_access_expression(
        &mut self,
        base: &Expression,
        field: &str,
        span: Span,
    ) -> Expression {
        Expression::FieldAccess {
            base: Box::new(self.rewrite_expression(base)),
            field: field.to_owned(),
            span,
        }
    }

    fn rewrite_expressions(&mut self, exprs: &[Expression]) -> Vec<Expression> {
        exprs
            .iter()
            .map(|expr| self.rewrite_expression(expr))
            .collect()
    }

    fn rewrite_subscripts(&mut self, subscripts: &[Subscript]) -> Vec<Subscript> {
        subscripts
            .iter()
            .map(|subscript| self.rewrite_subscript(subscript))
            .collect()
    }

    fn rewrite_subscript(&mut self, subscript: &Subscript) -> Subscript {
        match subscript {
            Subscript::Index { value, span } => Subscript::Index {
                value: *value,
                span: *span,
            },
            Subscript::Colon { span } => Subscript::Colon { span: *span },
            Subscript::Expr { expr, span } => Subscript::Expr {
                expr: Box::new(self.rewrite_expression(expr)),
                span: *span,
            },
        }
    }

    fn rewrite_comprehension_indices(
        &mut self,
        indices: &[ComprehensionIndex],
    ) -> Vec<ComprehensionIndex> {
        indices
            .iter()
            .map(|index| ComprehensionIndex {
                name: index.name.clone(),
                range: self.rewrite_expression(&index.range),
            })
            .collect()
    }
}
