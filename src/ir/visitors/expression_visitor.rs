//! Visitor trait for traversing expression trees

use crate::ir::ast::{ComponentReference, Expression};

/// Trait for visiting nodes in an expression tree
pub trait ExpressionVisitor {
    fn visit_component_reference(&mut self, _comp: &ComponentReference) {}

    fn visit_function_call(&mut self, _func: &ComponentReference, _args: &[Expression]) {}

    fn visit_binary_op(&mut self, _op: &str, _left: &Expression, _right: &Expression) {}

    fn visit_unary_op(&mut self, _op: &str, _operand: &Expression) {}

    fn visit_literal(&mut self, _value: &str) {}
}
