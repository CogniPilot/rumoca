use crate::{
    BuiltinFunction, ComprehensionIndex, Expression, Literal, OpBinary, OpUnary, Reference,
    Subscript,
};

pub enum ExpressionScope<'a> {
    ArrayComprehension(&'a [ComprehensionIndex]),
}

pub trait ExpressionVisitor {
    fn visit_expression(&mut self, expr: &Expression) {
        self.walk_expression(expr);
    }

    fn walk_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Binary { op, lhs, rhs, .. } => self.visit_binary(op, lhs, rhs),
            Expression::Unary { op, rhs, .. } => self.visit_unary(op, rhs),
            Expression::VarRef {
                name, subscripts, ..
            } => self.visit_var_ref(name, subscripts),
            Expression::BuiltinCall { function, args, .. } => {
                self.visit_builtin_call(function, args)
            }
            Expression::FunctionCall {
                name,
                args,
                is_constructor,
                ..
            } => self.visit_function_call(name, args, *is_constructor),
            Expression::Literal { value, .. } => self.visit_literal(value),
            Expression::If {
                branches,
                else_branch,
                ..
            } => self.visit_if(branches, else_branch),
            Expression::Array {
                elements,
                is_matrix,
                ..
            } => self.visit_array(elements, *is_matrix),
            Expression::Tuple { elements, .. } => self.visit_tuple(elements),
            Expression::Range {
                start, step, end, ..
            } => self.visit_range(start, step.as_deref(), end),
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                ..
            } => self.visit_array_comprehension(expr, indices, filter.as_deref()),
            Expression::Index {
                base, subscripts, ..
            } => self.visit_index(base, subscripts),
            Expression::FieldAccess { base, field, .. } => self.visit_field_access(base, field),
            Expression::Empty { .. } => {}
        }
    }

    fn visit_binary(&mut self, op: &OpBinary, lhs: &Expression, rhs: &Expression) {
        self.walk_binary(op, lhs, rhs);
    }

    fn walk_binary(&mut self, _op: &OpBinary, lhs: &Expression, rhs: &Expression) {
        self.visit_expression(lhs);
        self.visit_expression(rhs);
    }

    fn visit_unary(&mut self, op: &OpUnary, rhs: &Expression) {
        self.walk_unary(op, rhs);
    }

    fn walk_unary(&mut self, _op: &OpUnary, rhs: &Expression) {
        self.visit_expression(rhs);
    }

    fn visit_var_ref(&mut self, name: &Reference, subscripts: &[Subscript]) {
        self.walk_var_ref(name, subscripts);
    }

    fn walk_var_ref(&mut self, _name: &Reference, subscripts: &[Subscript]) {
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }

    fn visit_subscript(&mut self, subscript: &Subscript) {
        self.walk_subscript(subscript);
    }

    fn walk_subscript(&mut self, subscript: &Subscript) {
        if let Subscript::Expr { expr, .. } = subscript {
            self.visit_expression(expr);
        }
    }

    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        self.walk_builtin_call(function, args);
    }

    fn walk_builtin_call(&mut self, _function: &BuiltinFunction, args: &[Expression]) {
        for arg in args {
            self.visit_expression(arg);
        }
    }

    fn visit_function_call(&mut self, name: &Reference, args: &[Expression], is_constructor: bool) {
        self.walk_function_call(name, args, is_constructor);
    }

    fn walk_function_call(
        &mut self,
        _name: &Reference,
        args: &[Expression],
        _is_constructor: bool,
    ) {
        for arg in args {
            self.visit_expression(arg);
        }
    }

    fn visit_literal(&mut self, _lit: &Literal) {}

    fn visit_if(&mut self, branches: &[(Expression, Expression)], else_branch: &Expression) {
        for (condition, value) in branches {
            self.visit_expression(condition);
            self.visit_expression(value);
        }
        self.visit_expression(else_branch);
    }

    fn visit_array(&mut self, elements: &[Expression], _is_matrix: bool) {
        for element in elements {
            self.visit_expression(element);
        }
    }

    fn visit_tuple(&mut self, elements: &[Expression]) {
        for element in elements {
            self.visit_expression(element);
        }
    }

    fn visit_range(&mut self, start: &Expression, step: Option<&Expression>, end: &Expression) {
        self.visit_expression(start);
        if let Some(step) = step {
            self.visit_expression(step);
        }
        self.visit_expression(end);
    }

    fn visit_array_comprehension(
        &mut self,
        expr: &Expression,
        indices: &[ComprehensionIndex],
        filter: Option<&Expression>,
    ) {
        for index in indices {
            self.visit_expression(&index.range);
        }
        self.enter_scope(ExpressionScope::ArrayComprehension(indices));
        self.visit_expression(expr);
        if let Some(filter) = filter {
            self.visit_expression(filter);
        }
        self.exit_scope(ExpressionScope::ArrayComprehension(indices));
    }

    fn enter_scope(&mut self, _scope: ExpressionScope<'_>) {}

    fn exit_scope(&mut self, _scope: ExpressionScope<'_>) {}

    fn visit_index(&mut self, base: &Expression, subscripts: &[Subscript]) {
        self.visit_expression(base);
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }

    fn visit_field_access(&mut self, base: &Expression, _field: &str) {
        self.visit_expression(base);
    }
}

pub trait FallibleExpressionVisitor {
    type Error;

    fn visit_expression(&mut self, expr: &Expression) -> Result<(), Self::Error> {
        self.walk_expression(expr)
    }

    fn walk_expression(&mut self, expr: &Expression) -> Result<(), Self::Error> {
        match expr {
            Expression::Binary { op, lhs, rhs, .. } => self.visit_binary(op, lhs, rhs),
            Expression::Unary { op, rhs, .. } => self.visit_unary(op, rhs),
            Expression::VarRef {
                name, subscripts, ..
            } => self.visit_var_ref(name, subscripts),
            Expression::BuiltinCall { function, args, .. } => {
                self.visit_builtin_call(function, args)
            }
            Expression::FunctionCall {
                name,
                args,
                is_constructor,
                ..
            } => self.visit_function_call(name, args, *is_constructor),
            Expression::Literal { value, .. } => self.visit_literal(value),
            Expression::If {
                branches,
                else_branch,
                ..
            } => self.visit_if(branches, else_branch),
            Expression::Array {
                elements,
                is_matrix,
                ..
            } => self.visit_array(elements, *is_matrix),
            Expression::Tuple { elements, .. } => self.visit_tuple(elements),
            Expression::Range {
                start, step, end, ..
            } => self.visit_range(start, step.as_deref(), end),
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                ..
            } => self.visit_array_comprehension(expr, indices, filter.as_deref()),
            Expression::Index {
                base, subscripts, ..
            } => self.visit_index(base, subscripts),
            Expression::FieldAccess { base, field, .. } => self.visit_field_access(base, field),
            Expression::Empty { .. } => Ok(()),
        }
    }

    fn visit_binary(
        &mut self,
        _op: &OpBinary,
        lhs: &Expression,
        rhs: &Expression,
    ) -> Result<(), Self::Error> {
        self.visit_expression(lhs)?;
        self.visit_expression(rhs)
    }

    fn visit_unary(&mut self, _op: &OpUnary, rhs: &Expression) -> Result<(), Self::Error> {
        self.visit_expression(rhs)
    }

    fn visit_var_ref(
        &mut self,
        _name: &Reference,
        subscripts: &[Subscript],
    ) -> Result<(), Self::Error> {
        for subscript in subscripts {
            self.visit_subscript(subscript)?;
        }
        Ok(())
    }

    fn visit_subscript(&mut self, subscript: &Subscript) -> Result<(), Self::Error> {
        if let Subscript::Expr { expr, .. } = subscript {
            self.visit_expression(expr)?;
        }
        Ok(())
    }

    fn visit_builtin_call(
        &mut self,
        _function: &BuiltinFunction,
        args: &[Expression],
    ) -> Result<(), Self::Error> {
        for arg in args {
            self.visit_expression(arg)?;
        }
        Ok(())
    }

    fn visit_function_call(
        &mut self,
        _name: &Reference,
        args: &[Expression],
        _is_constructor: bool,
    ) -> Result<(), Self::Error> {
        for arg in args {
            self.visit_expression(arg)?;
        }
        Ok(())
    }

    fn visit_literal(&mut self, _lit: &Literal) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_if(
        &mut self,
        branches: &[(Expression, Expression)],
        else_branch: &Expression,
    ) -> Result<(), Self::Error> {
        for (condition, value) in branches {
            self.visit_expression(condition)?;
            self.visit_expression(value)?;
        }
        self.visit_expression(else_branch)
    }

    fn visit_array(
        &mut self,
        elements: &[Expression],
        _is_matrix: bool,
    ) -> Result<(), Self::Error> {
        for element in elements {
            self.visit_expression(element)?;
        }
        Ok(())
    }

    fn visit_tuple(&mut self, elements: &[Expression]) -> Result<(), Self::Error> {
        for element in elements {
            self.visit_expression(element)?;
        }
        Ok(())
    }

    fn visit_range(
        &mut self,
        start: &Expression,
        step: Option<&Expression>,
        end: &Expression,
    ) -> Result<(), Self::Error> {
        self.visit_expression(start)?;
        if let Some(step) = step {
            self.visit_expression(step)?;
        }
        self.visit_expression(end)
    }

    fn visit_array_comprehension(
        &mut self,
        expr: &Expression,
        indices: &[ComprehensionIndex],
        filter: Option<&Expression>,
    ) -> Result<(), Self::Error> {
        for index in indices {
            self.visit_expression(&index.range)?;
        }
        self.enter_scope(ExpressionScope::ArrayComprehension(indices))?;
        let body_result = (|| {
            self.visit_expression(expr)?;
            if let Some(filter) = filter {
                self.visit_expression(filter)?;
            }
            Ok(())
        })();
        let exit_result = self.exit_scope(ExpressionScope::ArrayComprehension(indices));
        body_result?;
        exit_result
    }

    fn enter_scope(&mut self, _scope: ExpressionScope<'_>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn exit_scope(&mut self, _scope: ExpressionScope<'_>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_index(
        &mut self,
        base: &Expression,
        subscripts: &[Subscript],
    ) -> Result<(), Self::Error> {
        self.visit_expression(base)?;
        for subscript in subscripts {
            self.visit_subscript(subscript)?;
        }
        Ok(())
    }

    fn visit_field_access(&mut self, base: &Expression, _field: &str) -> Result<(), Self::Error> {
        self.visit_expression(base)
    }
}
