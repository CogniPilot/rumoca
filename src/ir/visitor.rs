//! This module defines the `Visitor` and `Visitable` traits for implementing
//! the Visitor design pattern in the context of an intermediate representation (IR)
//! for an abstract syntax tree (AST).
//!
//! ## Overview
//!
//! The `Visitor` trait provides a set of methods for entering and exiting various
//! types of AST nodes. These methods can be overridden to implement custom behavior
//! when traversing the AST.
//!
//! The `Visitable` trait is implemented by AST node types to allow them to accept
//! a `Visitor`. This enables recursive traversal of the AST, where each node
//! delegates the visitation of its children to the visitor.
//!
//! ## Key Components
//!
//! - **`Visitor` Trait**: Defines methods for entering and exiting specific AST node types,
//!   such as `StoredDefinition`, `ClassDefinition`, `Equation`, `Expression`, `Component`,
//!   and `ComponentReference`. These methods are no-op by default and can be overridden
//!   as needed.
//!
//! - **`Visitable` Trait**: Provides the `accept` method, which takes a mutable reference
//!   to a `Visitor` and allows the visitor to traverse the node and its children.
//!
//! - **Implementations of `Visitable`**: Each AST node type implements the `Visitable`
//!   trait, defining how the visitor should traverse its children. For example, a
//!   `ClassDefinition` node delegates visitation to its equations, and an `Equation`
//!   node delegates visitation to its left-hand side (LHS) and right-hand side (RHS).
//!
//! ## Usage
//!
//! To use this module, define a struct that implements the `Visitor` trait, overriding
//! the methods for the node types you are interested in. Then, call the `accept` method
//! on the root node of the AST, passing a mutable reference to your visitor.
//!
//! This design pattern is useful for implementing operations such as code generation,
//! optimization, or analysis on the AST.
use crate::ir;

#[allow(unused)]
pub trait Visitor {
    fn enter_stored_definition(&mut self, _node: &mut ir::ast::StoredDefinition) {}
    fn exit_stored_definition(&mut self, _node: &mut ir::ast::StoredDefinition) {}

    fn enter_class_definition(&mut self, _node: &mut ir::ast::ClassDefinition) {}
    fn exit_class_definition(&mut self, _node: &mut ir::ast::ClassDefinition) {}

    fn enter_equation(&mut self, _node: &mut ir::ast::Equation) {}
    fn exit_equation(&mut self, _node: &mut ir::ast::Equation) {}

    fn enter_expression(&mut self, _node: &mut ir::ast::Expression) {}
    fn exit_expression(&mut self, _node: &mut ir::ast::Expression) {}

    fn enter_component(&mut self, _node: &mut ir::ast::Component) {}
    fn exit_component(&mut self, _node: &mut ir::ast::Component) {}

    fn enter_component_reference(&mut self, _node: &mut ir::ast::ComponentReference) {}
    fn exit_component_reference(&mut self, _node: &mut ir::ast::ComponentReference) {}
}

pub trait Visitable {
    fn accept<V: Visitor>(&mut self, visitor: &mut V);
}

#[allow(unused)]
impl Visitable for ir::ast::StoredDefinition {
    fn accept<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.enter_stored_definition(self);
        for (_name, class) in &mut self.class_list {
            class.accept(visitor);
        }
        visitor.exit_stored_definition(self);
    }
}

#[allow(unused)]
impl Visitable for ir::ast::ClassDefinition {
    fn accept<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.enter_class_definition(self);
        for eq in &mut self.equations {
            eq.accept(visitor);
        }
        visitor.exit_class_definition(self);
    }
}

#[allow(unused)]
impl Visitable for ir::ast::Equation {
    fn accept<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.enter_equation(self);
        match self {
            ir::ast::Equation::Simple { lhs, rhs } => {
                lhs.accept(visitor);
                rhs.accept(visitor);
            }
            ir::ast::Equation::FunctionCall { comp, args } => {
                comp.accept(visitor);
                for arg in args {
                    arg.accept(visitor);
                }
            }
            ir::ast::Equation::For { indices, equations } => {
                for index in indices {
                    index.range.accept(visitor);
                }
                for eq in equations {
                    eq.accept(visitor);
                }
            }
            ir::ast::Equation::Connect { lhs, rhs } => {
                lhs.accept(visitor);
                rhs.accept(visitor);
            }
            ir::ast::Equation::When(blocks) => {
                for block in blocks {
                    block.cond.accept(visitor);
                    for eq in &mut block.eqs {
                        eq.accept(visitor);
                    }
                }
            }
            ir::ast::Equation::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    block.cond.accept(visitor);
                    for eq in &mut block.eqs {
                        eq.accept(visitor);
                    }
                }
                if let Some(else_block) = else_block {
                    for eq in else_block {
                        eq.accept(visitor);
                    }
                }
            }
            ir::ast::Equation::Empty => {}
        }
        visitor.exit_equation(self);
    }
}

#[allow(unused)]
impl Visitable for ir::ast::Expression {
    fn accept<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.enter_expression(self);
        match self {
            ir::ast::Expression::Unary { op, rhs } => {
                rhs.accept(visitor);
            }
            ir::ast::Expression::Binary { lhs, op, rhs } => {
                lhs.accept(visitor);
                rhs.accept(visitor);
            }
            ir::ast::Expression::ComponentReference(cref) => {
                cref.accept(visitor);
            }
            ir::ast::Expression::FunctionCall { comp, args } => {
                comp.accept(visitor);
                for arg in args {
                    arg.accept(visitor);
                }
            }
            ir::ast::Expression::Array { elements } => {
                for element in elements {
                    element.accept(visitor);
                }
            }
            ir::ast::Expression::Range { start, step, end } => {
                start.accept(visitor);
                if step.is_some() {
                    // SAFETY: We just checked that step is Some above
                    step.as_mut().unwrap().accept(visitor);
                }
                end.accept(visitor);
            }
            ir::ast::Expression::Terminal { .. } => {}
            ir::ast::Expression::Empty => {}
            ir::ast::Expression::Tuple { elements } => {
                for element in elements {
                    element.accept(visitor);
                }
            }
            ir::ast::Expression::If {
                branches,
                else_branch,
            } => {
                for (cond, then_expr) in branches {
                    cond.accept(visitor);
                    then_expr.accept(visitor);
                }
                else_branch.accept(visitor);
            }
        }
        visitor.exit_expression(self);
    }
}

#[allow(unused)]
impl Visitable for ir::ast::Component {
    fn accept<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.enter_component(self);
        visitor.exit_component(self);
    }
}

#[allow(unused)]
impl Visitable for ir::ast::ComponentReference {
    fn accept<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.enter_component_reference(self);
        visitor.exit_component_reference(self);
    }
}
