//! Instantiate one already-lowered contraction iteration.
//!
//! First-product arithmetic emits the first product as the accumulator seed
//! and runs the remaining accumulation loop from two. Lowering the operands a
//! second time would create a second call/materialization authority, so the
//! contraction constructor clones its one-iteration legalization and replaces
//! only its exact private iterator with the literal first coordinate.

use super::*;

pub(super) fn instantiate_iteration(
    statements: &[gast::Spanned<gast::Statement>],
    value: &gast::Expression,
    iterator: &gast::Name,
    coordinate: i64,
) -> (Vec<gast::Spanned<gast::Statement>>, gast::Expression) {
    let mut statements = statements.to_vec();
    let mut value = value.clone();
    let instantiation = IterationInstantiation {
        iterator,
        coordinate,
    };
    instantiation.statements(&mut statements);
    instantiation.expression(&mut value);
    (statements, value)
}

struct IterationInstantiation<'a> {
    iterator: &'a gast::Name,
    coordinate: i64,
}

impl IterationInstantiation<'_> {
    fn statements(&self, statements: &mut [gast::Spanned<gast::Statement>]) {
        for statement in statements {
            self.statement(&mut statement.node);
        }
    }

    fn statement(&self, statement: &mut gast::Statement) {
        match statement {
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
            gast::Statement::Limit(targets) => self.limit_targets(targets),
            gast::Statement::Signal(_) => {}
        }
    }

    fn limit_targets(&self, targets: &mut [gast::LimitTarget]) {
        for target in targets {
            if let gast::LimitTarget::Reference(reference) = target {
                self.reference(reference);
            }
        }
    }

    fn for_loop(&self, loop_: &mut gast::ForLoop) {
        self.expression(&mut loop_.start);
        if let Some(step) = &mut loop_.step {
            self.expression(step);
        }
        self.expression(&mut loop_.stop);
        if loop_.iterator.as_ref() != Some(self.iterator) {
            self.statements(&mut loop_.body);
        }
    }

    fn if_statement(&self, value: &mut gast::IfStatement) {
        for branch in &mut value.branches {
            self.condition(&mut branch.condition);
            self.statements(&mut branch.body);
        }
        if let Some(body) = &mut value.else_body {
            self.statements(body);
        }
    }

    fn condition(&self, condition: &mut gast::Condition) {
        match condition {
            gast::Condition::Expression(condition) => self.expression(condition),
            gast::Condition::SignalCheck(check) => {
                if let Some(fallback) = &mut check.fallback {
                    self.expression(fallback);
                }
            }
        }
    }

    fn call(&self, call: &mut gast::FunctionCall) {
        for argument in &mut call.arguments {
            self.expression(argument);
        }
    }

    fn reference(&self, reference: &mut gast::Reference) {
        let parts = match reference {
            gast::Reference::Local(part) => std::slice::from_mut(part),
            gast::Reference::State(parts) => parts,
        };
        for part in parts {
            for subscript in &mut part.subscripts {
                self.expression(subscript);
            }
        }
    }

    fn expression(&self, expression: &mut gast::Expression) {
        if self.replace_bare_iterator(expression) {
            return;
        }
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
            gast::Expression::If(value) => self.if_expression(value),
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

    fn replace_bare_iterator(&self, expression: &mut gast::Expression) -> bool {
        let replacement = match expression {
            gast::Expression::Ref(reference) if self.is_bare_iterator(reference) => {
                Some(self.coordinate)
            }
            gast::Expression::Neg(reference) if self.is_bare_iterator(reference) => {
                self.coordinate.checked_neg()
            }
            _ => return false,
        };
        if let Some(value) = replacement {
            *expression = gast::Expression::Integer(value);
            true
        } else {
            false
        }
    }

    fn is_bare_iterator(&self, reference: &gast::Reference) -> bool {
        matches!(
            reference,
            gast::Reference::Local(part)
                if part.name == *self.iterator && part.subscripts.is_empty()
        )
    }

    fn if_expression(&self, value: &mut gast::IfExpression) {
        let correlation = value.bounded_selection_correlation().map(|correlation| {
            (
                correlation.reference().clone(),
                correlation.extents().to_vec(),
            )
        });
        if let Some((mut reference, extents)) = correlation {
            self.reference(&mut reference);
            if let Ok(rebuilt) = gast::IfExpression::bounded_selection(reference, extents) {
                *value = rebuilt;
                return;
            }
        }
        for (condition, branch) in &mut value.branches {
            self.expression(condition);
            self.expression(branch);
        }
        self.expression(&mut value.else_value);
        let else_value = *std::mem::replace(
            &mut value.else_value,
            Box::new(gast::Expression::Integer(0)),
        );
        *value = gast::IfExpression::new(std::mem::take(&mut value.branches), else_value);
    }
}
