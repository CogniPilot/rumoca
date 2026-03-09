//! AST visitor and transformer traits.
//!
//! These traits provide reusable patterns for traversing and transforming
//! AST trees, reducing code duplication across compiler phases.
//!
//! # Usage
//!
//! ## Visitor (read-only traversal)
//!
//! The `Visitor` trait provides methods for traversing all AST node types.
//! All methods have default implementations that traverse children, so you
//! only override the ones you care about.
//!
//! ```ignore
//! use rumoca_ir_ast::{Visitor, ComponentReference, Expression};
//! use std::ops::ControlFlow::{self, Continue};
//!
//! struct DerCollector {
//!     states: Vec<String>,
//! }
//!
//! impl Visitor for DerCollector {
//!     fn visit_expr_function_call(&mut self, comp: &ComponentReference, args: &[Expression]) -> ControlFlow<()> {
//!         if comp.to_string() == "der" {
//!             if let Some(Expression::ComponentReference(cr)) = args.first() {
//!                 self.states.push(cr.to_string());
//!             }
//!         }
//!         // Visit children
//!         self.visit_each(args, Self::visit_expression)
//!     }
//! }
//! ```
//!
//! ## Early termination
//!
//! Return `Break(())` to stop traversal early:
//!
//! ```ignore
//! use std::ops::ControlFlow::{self, Break, Continue};
//!
//! impl Visitor for FirstConnectFinder {
//!     fn visit_connect(&mut self, lhs: &ComponentReference, rhs: &ComponentReference) -> ControlFlow<()> {
//!         self.found = Some((lhs.to_string(), rhs.to_string()));
//!         Break(()) // Stop traversal
//!     }
//! }
//! ```

use crate::{
    ClassDef, Component, ComponentReference, Equation, EquationBlock, Expression, Extend, ForIndex,
    Name, Statement, StatementBlock, StoredDefinition, Subscript,
};
use std::ops::ControlFlow::{self, Break, Continue};
use std::sync::Arc;

/// Context for traversed function call targets.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionCallContext {
    Expression,
    Equation,
    Statement,
}

/// Context for traversed component references.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComponentReferenceContext {
    Expression,
    EquationConnectLhs,
    EquationConnectRhs,
    EquationFunctionCallTarget,
    StatementFunctionCallTarget,
    AssignmentTarget,
    ReinitTarget,
    ClassModificationTarget,
    ModificationTarget,
}

/// Context for traversed type names.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeNameContext {
    ExtendsBase,
    ComponentType,
    ClassConstrainedBy,
    ComponentConstrainedBy,
}

/// Context for traversed subscript expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SubscriptContext {
    ComponentReferencePart,
    ArrayIndex,
}

/// Context for traversed expressions from declaration/equation/statement fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpressionContext {
    Generic,
    ComponentStart,
    ComponentModification,
    ComponentCondition,
    ComponentAnnotation,
    EquationAssertCondition,
    EquationAssertMessage,
    EquationAssertLevel,
    StatementAssertCondition,
    StatementAssertMessage,
    StatementAssertLevel,
    StatementFunctionOutput,
    ExtendModification,
}

/// Trait for visiting AST nodes without modification.
///
/// All methods return `ControlFlow<()>`:
/// - `Continue(())` = continue traversal
/// - `Break(())` = stop traversal early
///
/// Override methods to add custom behavior. Call child visitors with `?` operator.
pub trait Visitor {
    // =========================================================================
    // Helper methods
    // =========================================================================

    /// Visit each item in a slice, stopping on Break.
    fn visit_each<T, F>(&mut self, items: &[T], mut f: F) -> ControlFlow<()>
    where
        F: FnMut(&mut Self, &T) -> ControlFlow<()>,
    {
        for item in items {
            f(self, item)?;
        }
        Continue(())
    }

    /// Visit an expression with contextual metadata.
    ///
    /// Default behavior defers to [`Visitor::visit_expression`].
    fn visit_expression_ctx(
        &mut self,
        expr: &Expression,
        _ctx: ExpressionContext,
    ) -> ControlFlow<()> {
        self.visit_expression(expr)
    }

    /// Visit a component reference with contextual metadata.
    ///
    /// Default behavior defers to [`Visitor::visit_component_reference`].
    fn visit_component_reference_ctx(
        &mut self,
        cr: &ComponentReference,
        _ctx: ComponentReferenceContext,
    ) -> ControlFlow<()> {
        self.visit_component_reference(cr)
    }

    /// Visit a function-call target with contextual metadata.
    ///
    /// Default behavior defers to [`Visitor::visit_expr_function_call`].
    fn visit_expr_function_call_ctx(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        _ctx: FunctionCallContext,
    ) -> ControlFlow<()> {
        self.visit_expr_function_call(comp, args)
    }

    /// Visit a type name in declaration context.
    ///
    /// Default behavior is no-op to preserve existing traversal behavior.
    fn visit_type_name(&mut self, _name: &Name, _ctx: TypeNameContext) -> ControlFlow<()> {
        Continue(())
    }

    /// Visit a subscript with contextual metadata.
    ///
    /// Default behavior defers to [`Visitor::visit_subscript`].
    fn visit_subscript_ctx(&mut self, sub: &Subscript, _ctx: SubscriptContext) -> ControlFlow<()> {
        self.visit_subscript(sub)
    }

    // =========================================================================
    // Expression methods
    // =========================================================================

    /// Visit any expression.
    fn visit_expression(&mut self, expr: &Expression) -> ControlFlow<()> {
        match expr {
            Expression::Empty | Expression::Terminal { .. } => Continue(()),
            Expression::Range { start, step, end } => {
                self.visit_expression(start)?;
                if let Some(s) = step {
                    self.visit_expression(s)?;
                }
                self.visit_expression(end)
            }
            Expression::Unary { rhs, .. } => self.visit_expression(rhs),
            Expression::Binary { lhs, rhs, .. } => {
                self.visit_expression(lhs)?;
                self.visit_expression(rhs)
            }
            Expression::ComponentReference(cr) => {
                self.visit_component_reference_ctx(cr, ComponentReferenceContext::Expression)
            }
            Expression::FunctionCall { comp, args } => {
                self.visit_expr_function_call_ctx(comp, args, FunctionCallContext::Expression)
            }
            Expression::ClassModification {
                target,
                modifications,
            } => {
                self.visit_component_reference_ctx(
                    target,
                    ComponentReferenceContext::ClassModificationTarget,
                )?;
                self.visit_each(modifications, Self::visit_expression)
            }
            Expression::NamedArgument { value, .. } => self.visit_expression(value),
            Expression::Modification { target, value } => {
                self.visit_component_reference_ctx(
                    target,
                    ComponentReferenceContext::ModificationTarget,
                )?;
                self.visit_expression(value)
            }
            Expression::Array { elements, .. } | Expression::Tuple { elements } => {
                self.visit_each(elements, Self::visit_expression)
            }
            Expression::If {
                branches,
                else_branch,
            } => {
                for (cond, then_expr) in branches {
                    self.visit_expression(cond)?;
                    self.visit_expression(then_expr)?;
                }
                self.visit_expression(else_branch)
            }
            Expression::Parenthesized { inner } => self.visit_expression(inner),
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
            } => {
                self.visit_expression(expr)?;
                self.visit_each(indices, Self::visit_for_index)?;
                if let Some(f) = filter {
                    self.visit_expression(f)?;
                }
                Continue(())
            }
            Expression::ArrayIndex { base, subscripts } => {
                self.visit_expression(base)?;
                for subscript in subscripts {
                    self.visit_subscript_ctx(subscript, SubscriptContext::ArrayIndex)?;
                }
                Continue(())
            }
            Expression::FieldAccess { base, .. } => self.visit_expression(base),
        }
    }

    /// Visit a component reference.
    fn visit_component_reference(&mut self, cr: &ComponentReference) -> ControlFlow<()> {
        for part in &cr.parts {
            let Some(subs) = &part.subs else {
                continue;
            };
            for subscript in subs {
                self.visit_subscript_ctx(subscript, SubscriptContext::ComponentReferencePart)?;
            }
        }
        Continue(())
    }

    /// Visit a function call in expression context.
    fn visit_expr_function_call(
        &mut self,
        _comp: &ComponentReference,
        args: &[Expression],
    ) -> ControlFlow<()> {
        self.visit_each(args, Self::visit_expression)
    }

    /// Visit a subscript.
    fn visit_subscript(&mut self, sub: &Subscript) -> ControlFlow<()> {
        if let Subscript::Expression(expr) = sub {
            return self.visit_expression(expr);
        }
        Continue(())
    }

    /// Visit a for-loop index.
    fn visit_for_index(&mut self, idx: &ForIndex) -> ControlFlow<()> {
        self.visit_expression(&idx.range)
    }

    // =========================================================================
    // Equation methods
    // =========================================================================

    /// Visit any equation.
    fn visit_equation(&mut self, eq: &Equation) -> ControlFlow<()> {
        match eq {
            Equation::Empty => Continue(()),
            Equation::Simple { lhs, rhs } => self.visit_simple_equation(lhs, rhs),
            Equation::Connect { lhs, rhs } => self.visit_connect(lhs, rhs),
            Equation::For { indices, equations } => self.visit_for_equation(indices, equations),
            Equation::When(blocks) => self.visit_when_equation(blocks),
            Equation::If {
                cond_blocks,
                else_block,
            } => self.visit_if_equation(cond_blocks, else_block.as_deref()),
            Equation::FunctionCall { comp, args } => self.visit_equation_function_call(comp, args),
            Equation::Assert {
                condition,
                message,
                level,
            } => self.visit_equation_assert(condition, message, level.as_ref()),
        }
    }

    /// Visit a simple equation: lhs = rhs
    fn visit_simple_equation(&mut self, lhs: &Expression, rhs: &Expression) -> ControlFlow<()> {
        self.visit_expression(lhs)?;
        self.visit_expression(rhs)
    }

    /// Visit a connect equation: connect(lhs, rhs)
    fn visit_connect(
        &mut self,
        lhs: &ComponentReference,
        rhs: &ComponentReference,
    ) -> ControlFlow<()> {
        self.visit_component_reference_ctx(lhs, ComponentReferenceContext::EquationConnectLhs)?;
        self.visit_component_reference_ctx(rhs, ComponentReferenceContext::EquationConnectRhs)
    }

    /// Visit a for-equation.
    fn visit_for_equation(
        &mut self,
        indices: &[ForIndex],
        equations: &[Equation],
    ) -> ControlFlow<()> {
        self.visit_each(indices, Self::visit_for_index)?;
        self.visit_each(equations, Self::visit_equation)
    }

    /// Visit a when-equation.
    fn visit_when_equation(&mut self, blocks: &[EquationBlock]) -> ControlFlow<()> {
        self.visit_each(blocks, Self::visit_equation_block)
    }

    /// Visit an if-equation.
    fn visit_if_equation(
        &mut self,
        cond_blocks: &[EquationBlock],
        else_block: Option<&[Equation]>,
    ) -> ControlFlow<()> {
        self.visit_each(cond_blocks, Self::visit_equation_block)?;
        if let Some(else_eqs) = else_block {
            self.visit_each(else_eqs, Self::visit_equation)?;
        }
        Continue(())
    }

    /// Visit a function call equation.
    fn visit_equation_function_call(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
    ) -> ControlFlow<()> {
        self.visit_component_reference_ctx(
            comp,
            ComponentReferenceContext::EquationFunctionCallTarget,
        )?;
        self.visit_expr_function_call_ctx(comp, args, FunctionCallContext::Equation)
    }

    /// Visit an assert equation.
    fn visit_equation_assert(
        &mut self,
        condition: &Expression,
        message: &Expression,
        level: Option<&Expression>,
    ) -> ControlFlow<()> {
        self.visit_expression_ctx(condition, ExpressionContext::EquationAssertCondition)?;
        self.visit_expression_ctx(message, ExpressionContext::EquationAssertMessage)?;
        if let Some(lvl) = level {
            self.visit_expression_ctx(lvl, ExpressionContext::EquationAssertLevel)?;
        }
        Continue(())
    }

    /// Visit an equation block (condition + equations).
    fn visit_equation_block(&mut self, block: &EquationBlock) -> ControlFlow<()> {
        self.visit_expression(&block.cond)?;
        self.visit_each(&block.eqs, Self::visit_equation)
    }

    // =========================================================================
    // Statement methods
    // =========================================================================

    /// Visit any statement.
    fn visit_statement(&mut self, stmt: &Statement) -> ControlFlow<()> {
        match stmt {
            Statement::Empty | Statement::Return { .. } | Statement::Break { .. } => Continue(()),
            Statement::Assignment { comp, value } => self.visit_assignment(comp, value),
            Statement::For { indices, equations } => self.visit_for_statement(indices, equations),
            Statement::While(block) => self.visit_statement_block(block),
            Statement::If {
                cond_blocks,
                else_block,
            } => self.visit_if_statement(cond_blocks, else_block.as_deref()),
            Statement::When(blocks) => self.visit_when_statement(blocks),
            Statement::FunctionCall {
                comp,
                args,
                outputs,
            } => self.visit_statement_function_call(comp, args, outputs),
            Statement::Reinit { variable, value } => self.visit_reinit(variable, value),
            Statement::Assert {
                condition,
                message,
                level,
            } => self.visit_statement_assert(condition, message, level.as_ref()),
        }
    }

    /// Visit an assignment statement.
    fn visit_assignment(
        &mut self,
        comp: &ComponentReference,
        value: &Expression,
    ) -> ControlFlow<()> {
        self.visit_component_reference_ctx(comp, ComponentReferenceContext::AssignmentTarget)?;
        self.visit_expression(value)
    }

    /// Visit a for-statement.
    fn visit_for_statement(
        &mut self,
        indices: &[ForIndex],
        statements: &[Statement],
    ) -> ControlFlow<()> {
        self.visit_each(indices, Self::visit_for_index)?;
        self.visit_each(statements, Self::visit_statement)
    }

    /// Visit an if-statement.
    fn visit_if_statement(
        &mut self,
        cond_blocks: &[StatementBlock],
        else_block: Option<&[Statement]>,
    ) -> ControlFlow<()> {
        self.visit_each(cond_blocks, Self::visit_statement_block)?;
        if let Some(else_stmts) = else_block {
            self.visit_each(else_stmts, Self::visit_statement)?;
        }
        Continue(())
    }

    /// Visit a when-statement.
    fn visit_when_statement(&mut self, blocks: &[StatementBlock]) -> ControlFlow<()> {
        self.visit_each(blocks, Self::visit_statement_block)
    }

    /// Visit a function call statement.
    fn visit_statement_function_call(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        outputs: &[Expression],
    ) -> ControlFlow<()> {
        self.visit_component_reference_ctx(
            comp,
            ComponentReferenceContext::StatementFunctionCallTarget,
        )?;
        self.visit_expr_function_call_ctx(comp, args, FunctionCallContext::Statement)?;
        for output in outputs {
            self.visit_expression_ctx(output, ExpressionContext::StatementFunctionOutput)?;
        }
        Continue(())
    }

    /// Visit a reinit statement.
    fn visit_reinit(
        &mut self,
        variable: &ComponentReference,
        value: &Expression,
    ) -> ControlFlow<()> {
        self.visit_component_reference_ctx(variable, ComponentReferenceContext::ReinitTarget)?;
        self.visit_expression(value)
    }

    /// Visit an assert statement.
    fn visit_statement_assert(
        &mut self,
        condition: &Expression,
        message: &Expression,
        level: Option<&Expression>,
    ) -> ControlFlow<()> {
        self.visit_expression_ctx(condition, ExpressionContext::StatementAssertCondition)?;
        self.visit_expression_ctx(message, ExpressionContext::StatementAssertMessage)?;
        if let Some(lvl) = level {
            self.visit_expression_ctx(lvl, ExpressionContext::StatementAssertLevel)?;
        }
        Continue(())
    }

    /// Visit a statement block (condition + statements).
    fn visit_statement_block(&mut self, block: &StatementBlock) -> ControlFlow<()> {
        self.visit_expression(&block.cond)?;
        self.visit_each(&block.stmts, Self::visit_statement)
    }

    // =========================================================================
    // Class tree methods
    // =========================================================================

    /// Visit a stored definition (root of class tree).
    fn visit_stored_definition(&mut self, def: &StoredDefinition) -> ControlFlow<()> {
        for (_, class) in &def.classes {
            self.visit_class_def(class)?;
        }
        Continue(())
    }

    /// Visit a class definition.
    fn visit_class_def(&mut self, class: &ClassDef) -> ControlFlow<()> {
        if let Some(constrainedby) = &class.constrainedby {
            self.visit_type_name(constrainedby, TypeNameContext::ClassConstrainedBy)?;
        }
        for ext in &class.extends {
            self.visit_extend(ext)?;
        }
        for (_, nested) in &class.classes {
            self.visit_class_def(nested)?;
        }
        for (_, comp) in &class.components {
            self.visit_component(comp)?;
        }
        self.visit_each(&class.equations, Self::visit_equation)?;
        self.visit_each(&class.initial_equations, Self::visit_equation)?;
        for section in &class.algorithms {
            self.visit_each(section, Self::visit_statement)?;
        }
        for section in &class.initial_algorithms {
            self.visit_each(section, Self::visit_statement)?;
        }
        Continue(())
    }

    /// Visit an extends clause.
    fn visit_extend(&mut self, ext: &Extend) -> ControlFlow<()> {
        self.visit_type_name(&ext.base_name, TypeNameContext::ExtendsBase)?;
        for modification in &ext.modifications {
            self.visit_expression_ctx(&modification.expr, ExpressionContext::ExtendModification)?;
        }
        Continue(())
    }

    /// Visit a component declaration.
    fn visit_component(&mut self, comp: &Component) -> ControlFlow<()> {
        self.visit_type_name(&comp.type_name, TypeNameContext::ComponentType)?;
        if let Some(constrainedby) = &comp.constrainedby {
            self.visit_type_name(constrainedby, TypeNameContext::ComponentConstrainedBy)?;
        }
        if !matches!(comp.start, Expression::Empty) {
            self.visit_expression_ctx(&comp.start, ExpressionContext::ComponentStart)?;
        }
        for (_, mod_expr) in &comp.modifications {
            self.visit_expression_ctx(mod_expr, ExpressionContext::ComponentModification)?;
        }
        if let Some(cond) = &comp.condition {
            self.visit_expression_ctx(cond, ExpressionContext::ComponentCondition)?;
        }
        for annotation in &comp.annotation {
            self.visit_expression_ctx(annotation, ExpressionContext::ComponentAnnotation)?;
        }
        Continue(())
    }
}

// =============================================================================
// ExpressionTransformer: Mutation
// =============================================================================

/// Trait for transforming expressions.
///
/// Override specific `transform_*` methods to customize behavior.
/// Default implementations recursively transform children.
pub trait ExpressionTransformer {
    /// Transform any expression.
    fn transform_expression(&mut self, expr: Expression) -> Expression {
        match expr {
            Expression::Empty => Expression::Empty,
            Expression::Terminal {
                terminal_type,
                token,
            } => Expression::Terminal {
                terminal_type,
                token,
            },
            Expression::Range { start, step, end } => Expression::Range {
                start: Arc::new(self.transform_expression((*start).clone())),
                step: step.map(|s| Arc::new(self.transform_expression((*s).clone()))),
                end: Arc::new(self.transform_expression((*end).clone())),
            },
            Expression::Unary { op, rhs } => Expression::Unary {
                op,
                rhs: Arc::new(self.transform_expression((*rhs).clone())),
            },
            Expression::Binary { op, lhs, rhs } => Expression::Binary {
                op,
                lhs: Arc::new(self.transform_expression((*lhs).clone())),
                rhs: Arc::new(self.transform_expression((*rhs).clone())),
            },
            Expression::ComponentReference(cr) => self.transform_component_reference(cr),
            Expression::FunctionCall { comp, args } => self.transform_function_call(comp, args),
            Expression::ClassModification {
                target,
                modifications,
            } => Expression::ClassModification {
                target: self.transform_component_ref_inner(target),
                modifications: modifications
                    .into_iter()
                    .map(|m| self.transform_expression(m))
                    .collect(),
            },
            Expression::NamedArgument { name, value } => Expression::NamedArgument {
                name,
                value: Arc::new(self.transform_expression((*value).clone())),
            },
            Expression::Modification { target, value } => Expression::Modification {
                target: self.transform_component_ref_inner(target),
                value: Arc::new(self.transform_expression((*value).clone())),
            },
            Expression::Array {
                elements,
                is_matrix,
            } => Expression::Array {
                elements: elements
                    .into_iter()
                    .map(|e| self.transform_expression(e))
                    .collect(),
                is_matrix,
            },
            Expression::Tuple { elements } => Expression::Tuple {
                elements: elements
                    .into_iter()
                    .map(|e| self.transform_expression(e))
                    .collect(),
            },
            Expression::If {
                branches,
                else_branch,
            } => Expression::If {
                branches: branches
                    .into_iter()
                    .map(|(c, t)| (self.transform_expression(c), self.transform_expression(t)))
                    .collect(),
                else_branch: Arc::new(self.transform_expression((*else_branch).clone())),
            },
            Expression::Parenthesized { inner } => Expression::Parenthesized {
                inner: Arc::new(self.transform_expression((*inner).clone())),
            },
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
            } => Expression::ArrayComprehension {
                expr: Arc::new(self.transform_expression((*expr).clone())),
                indices: indices
                    .into_iter()
                    .map(|idx| self.transform_for_index(idx))
                    .collect(),
                filter: filter.map(|f| Arc::new(self.transform_expression((*f).clone()))),
            },
            Expression::ArrayIndex { base, subscripts } => Expression::ArrayIndex {
                base: Arc::new(self.transform_expression((*base).clone())),
                subscripts: subscripts
                    .into_iter()
                    .map(|s| self.transform_subscript(s))
                    .collect(),
            },
            Expression::FieldAccess { base, field } => Expression::FieldAccess {
                base: Arc::new(self.transform_expression((*base).clone())),
                field,
            },
        }
    }

    /// Transform a component reference expression.
    fn transform_component_reference(&mut self, cr: ComponentReference) -> Expression {
        Expression::ComponentReference(self.transform_component_ref_inner(cr))
    }

    /// Transform a ComponentReference struct (internal helper).
    fn transform_component_ref_inner(&mut self, cr: ComponentReference) -> ComponentReference {
        cr
    }

    /// Transform a function call.
    fn transform_function_call(
        &mut self,
        comp: ComponentReference,
        args: Vec<Expression>,
    ) -> Expression {
        Expression::FunctionCall {
            comp,
            args: args
                .into_iter()
                .map(|a| self.transform_expression(a))
                .collect(),
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

// =============================================================================
// Convenience functions
// =============================================================================

/// Check if an expression contains any component references matching a predicate.
pub fn contains_component_ref<F>(expr: &Expression, predicate: F) -> bool
where
    F: Fn(&ComponentReference) -> bool,
{
    struct Finder<'a, F> {
        predicate: &'a F,
        found: bool,
    }

    impl<F: Fn(&ComponentReference) -> bool> Visitor for Finder<'_, F> {
        fn visit_component_reference(&mut self, cr: &ComponentReference) -> ControlFlow<()> {
            if (self.predicate)(cr) {
                self.found = true;
                return Break(());
            }
            Continue(())
        }
    }

    let mut finder = Finder {
        predicate: &predicate,
        found: false,
    };
    let _ = finder.visit_expression(expr);
    finder.found
}

/// Check if an expression contains a function call matching a predicate.
pub fn contains_function_call<F>(expr: &Expression, predicate: F) -> bool
where
    F: Fn(&ComponentReference, &[Expression]) -> bool,
{
    struct Finder<'a, F> {
        predicate: &'a F,
        found: bool,
    }

    impl<F: Fn(&ComponentReference, &[Expression]) -> bool> Visitor for Finder<'_, F> {
        fn visit_expr_function_call(
            &mut self,
            comp: &ComponentReference,
            args: &[Expression],
        ) -> ControlFlow<()> {
            if (self.predicate)(comp, args) {
                self.found = true;
                return Break(());
            }
            self.visit_each(args, Self::visit_expression)
        }
    }

    let mut finder = Finder {
        predicate: &predicate,
        found: false,
    };
    let _ = finder.visit_expression(expr);
    finder.found
}

/// Helper struct for collecting component references.
struct ComponentRefCollector {
    refs: Vec<ComponentReference>,
}

impl ComponentRefCollector {
    fn new() -> Self {
        Self { refs: Vec::new() }
    }

    fn walk_subscripts(&mut self, cr: &ComponentReference) -> ControlFlow<()> {
        for part in &cr.parts {
            let Some(subs) = &part.subs else { continue };
            self.visit_each(subs, Self::visit_subscript)?;
        }
        Continue(())
    }
}

impl Visitor for ComponentRefCollector {
    fn visit_component_reference(&mut self, cr: &ComponentReference) -> ControlFlow<()> {
        self.refs.push(cr.clone());
        self.walk_subscripts(cr)
    }
}

/// Collect all component references in an expression.
pub fn collect_component_refs(expr: &Expression) -> Vec<ComponentReference> {
    let mut collector = ComponentRefCollector::new();
    let _ = collector.visit_expression(expr);
    collector.refs
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ComponentRefPart, OpBinary, TerminalType, Token};
    use indexmap::IndexMap;

    fn make_var(name: &str) -> Expression {
        Expression::ComponentReference(make_comp_ref(name))
    }

    fn make_int(value: i64) -> Expression {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: Token {
                text: std::sync::Arc::from(value.to_string()),
                ..Default::default()
            },
        }
    }

    fn make_comp_ref(name: &str) -> ComponentReference {
        ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: std::sync::Arc::from(name),
                    ..Default::default()
                },
                subs: None,
            }],
            def_id: None,
        }
    }

    fn make_comp_ref_with_subscript(name: &str, sub: Expression) -> ComponentReference {
        ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: std::sync::Arc::from(name),
                    ..Default::default()
                },
                subs: Some(vec![Subscript::Expression(sub)]),
            }],
            def_id: None,
        }
    }

    #[test]
    fn test_collect_component_refs() {
        let expr = Expression::Binary {
            op: OpBinary::Add(Token::default()),
            lhs: Arc::new(make_var("x")),
            rhs: Arc::new(make_var("y")),
        };
        let refs = collect_component_refs(&expr);
        assert_eq!(refs.len(), 2);
        assert_eq!(refs[0].to_string(), "x");
        assert_eq!(refs[1].to_string(), "y");
    }

    #[test]
    fn test_contains_component_ref() {
        let expr = Expression::Binary {
            op: OpBinary::Add(Token::default()),
            lhs: Arc::new(make_var("x")),
            rhs: Arc::new(make_int(1)),
        };
        assert!(contains_component_ref(&expr, |cr| cr.to_string() == "x"));
        assert!(!contains_component_ref(&expr, |cr| cr.to_string() == "y"));
    }

    struct Renamer;
    impl ExpressionTransformer for Renamer {
        fn transform_component_reference(&mut self, mut cr: ComponentReference) -> Expression {
            if cr.to_string() == "x" {
                cr.parts[0].ident.text = std::sync::Arc::from("renamed");
            }
            Expression::ComponentReference(cr)
        }
    }

    #[test]
    fn test_transformer_rename() {
        let expr = Expression::Binary {
            op: OpBinary::Add(Token::default()),
            lhs: Arc::new(make_var("x")),
            rhs: Arc::new(make_int(1)),
        };
        let result = Renamer.transform_expression(expr);
        let refs = collect_component_refs(&result);
        assert_eq!(refs[0].to_string(), "renamed");
    }

    #[test]
    fn test_equation_visitor_collect_connects() {
        struct ConnectCollector(Vec<(String, String)>);
        impl Visitor for ConnectCollector {
            fn visit_connect(
                &mut self,
                lhs: &ComponentReference,
                rhs: &ComponentReference,
            ) -> ControlFlow<()> {
                self.0.push((lhs.to_string(), rhs.to_string()));
                Continue(())
            }
        }

        let equations = vec![
            Equation::Connect {
                lhs: make_comp_ref("a"),
                rhs: make_comp_ref("b"),
            },
            Equation::Simple {
                lhs: make_var("x"),
                rhs: make_int(1),
            },
            Equation::Connect {
                lhs: make_comp_ref("c"),
                rhs: make_comp_ref("d"),
            },
        ];

        let mut collector = ConnectCollector(Vec::new());
        for eq in &equations {
            let _ = collector.visit_equation(eq);
        }
        assert_eq!(
            collector.0,
            vec![("a".into(), "b".into()), ("c".into(), "d".into())]
        );
    }

    #[test]
    fn test_early_termination() {
        struct FirstConnect(Option<String>);
        impl Visitor for FirstConnect {
            fn visit_connect(
                &mut self,
                lhs: &ComponentReference,
                _: &ComponentReference,
            ) -> ControlFlow<()> {
                self.0 = Some(lhs.to_string());
                Break(())
            }
        }

        let eq = Equation::For {
            indices: vec![ForIndex {
                ident: Token {
                    text: std::sync::Arc::from("i"),
                    ..Default::default()
                },
                range: make_int(1),
            }],
            equations: vec![
                Equation::Connect {
                    lhs: make_comp_ref("first"),
                    rhs: make_comp_ref("a"),
                },
                Equation::Connect {
                    lhs: make_comp_ref("second"),
                    rhs: make_comp_ref("b"),
                },
            ],
        };

        let mut finder = FirstConnect(None);
        let _ = finder.visit_equation(&eq);
        assert_eq!(finder.0, Some("first".into()));
    }

    #[test]
    fn test_function_call_context_dispatch() {
        #[derive(Default)]
        struct ContextRecorder {
            function_contexts: Vec<FunctionCallContext>,
            component_contexts: Vec<ComponentReferenceContext>,
            statement_output_expression_contexts: usize,
        }

        impl Visitor for ContextRecorder {
            fn visit_expr_function_call_ctx(
                &mut self,
                comp: &ComponentReference,
                args: &[Expression],
                ctx: FunctionCallContext,
            ) -> ControlFlow<()> {
                self.function_contexts.push(ctx);
                self.visit_expr_function_call(comp, args)
            }

            fn visit_component_reference_ctx(
                &mut self,
                cr: &ComponentReference,
                ctx: ComponentReferenceContext,
            ) -> ControlFlow<()> {
                self.component_contexts.push(ctx);
                self.visit_component_reference(cr)
            }

            fn visit_expression_ctx(
                &mut self,
                expr: &Expression,
                ctx: ExpressionContext,
            ) -> ControlFlow<()> {
                self.statement_output_expression_contexts +=
                    usize::from(ctx == ExpressionContext::StatementFunctionOutput);
                self.visit_expression(expr)
            }
        }

        let expr_call = Expression::FunctionCall {
            comp: make_comp_ref("f_expr"),
            args: vec![make_int(1)],
        };
        let equation_call = Equation::FunctionCall {
            comp: make_comp_ref("f_eq"),
            args: vec![make_int(2)],
        };
        let statement_call = Statement::FunctionCall {
            comp: make_comp_ref("f_stmt"),
            args: vec![make_int(3)],
            outputs: vec![make_var("out")],
        };

        let mut recorder = ContextRecorder::default();
        let _ = recorder.visit_expression(&expr_call);
        let _ = recorder.visit_equation(&equation_call);
        let _ = recorder.visit_statement(&statement_call);

        assert_eq!(
            recorder.function_contexts,
            vec![
                FunctionCallContext::Expression,
                FunctionCallContext::Equation,
                FunctionCallContext::Statement,
            ]
        );
        assert!(
            recorder
                .component_contexts
                .contains(&ComponentReferenceContext::EquationFunctionCallTarget)
        );
        assert!(
            recorder
                .component_contexts
                .contains(&ComponentReferenceContext::StatementFunctionCallTarget)
        );
        assert_eq!(recorder.statement_output_expression_contexts, 1);
    }

    #[test]
    fn test_type_name_context_dispatch() {
        #[derive(Default)]
        struct TypeNameRecorder {
            seen: Vec<(String, TypeNameContext)>,
        }

        impl Visitor for TypeNameRecorder {
            fn visit_type_name(&mut self, name: &Name, ctx: TypeNameContext) -> ControlFlow<()> {
                self.seen.push((name.to_string(), ctx));
                Continue(())
            }
        }

        let mut components = IndexMap::new();
        components.insert(
            "comp".to_string(),
            Component {
                type_name: Name::from_string("MyComponentType"),
                constrainedby: Some(Name::from_string("MyComponentConstraint")),
                ..Default::default()
            },
        );

        let class = ClassDef {
            constrainedby: Some(Name::from_string("MyClassConstraint")),
            extends: vec![Extend {
                base_name: Name::from_string("MyBaseClass"),
                ..Default::default()
            }],
            components,
            ..Default::default()
        };

        let mut recorder = TypeNameRecorder::default();
        let _ = recorder.visit_class_def(&class);

        assert!(recorder.seen.contains(&(
            "MyClassConstraint".to_string(),
            TypeNameContext::ClassConstrainedBy
        )));
        assert!(
            recorder
                .seen
                .contains(&("MyBaseClass".to_string(), TypeNameContext::ExtendsBase))
        );
        assert!(recorder.seen.contains(&(
            "MyComponentType".to_string(),
            TypeNameContext::ComponentType
        )));
        assert!(recorder.seen.contains(&(
            "MyComponentConstraint".to_string(),
            TypeNameContext::ComponentConstrainedBy
        )));
    }

    #[test]
    fn test_subscript_context_dispatch() {
        #[derive(Default)]
        struct SubscriptRecorder {
            seen: Vec<SubscriptContext>,
        }

        impl Visitor for SubscriptRecorder {
            fn visit_subscript_ctx(
                &mut self,
                sub: &Subscript,
                ctx: SubscriptContext,
            ) -> ControlFlow<()> {
                self.seen.push(ctx);
                self.visit_subscript(sub)
            }
        }

        let expr = Expression::ArrayIndex {
            base: Arc::new(Expression::ComponentReference(
                make_comp_ref_with_subscript("a", make_int(1)),
            )),
            subscripts: vec![Subscript::Expression(make_int(2))],
        };

        let mut recorder = SubscriptRecorder::default();
        let _ = recorder.visit_expression(&expr);

        assert!(
            recorder
                .seen
                .contains(&SubscriptContext::ComponentReferencePart)
        );
        assert!(recorder.seen.contains(&SubscriptContext::ArrayIndex));
    }

    fn make_expression_context_dispatch_class() -> ClassDef {
        let mut component = Component {
            type_name: Name::from_string("Real"),
            start: make_int(1),
            condition: Some(make_var("cond")),
            ..Default::default()
        };
        component.modifications.insert("k".to_string(), make_int(2));
        component.annotation.push(make_int(3));

        ClassDef {
            extends: vec![Extend {
                base_name: Name::from_string("Base"),
                modifications: vec![crate::ExtendModification {
                    expr: make_int(4),
                    ..Default::default()
                }],
                ..Default::default()
            }],
            components: {
                let mut comps = IndexMap::new();
                comps.insert("x".to_string(), component);
                comps
            },
            equations: vec![Equation::Assert {
                condition: make_var("eq_cond"),
                message: make_int(5),
                level: Some(make_int(6)),
            }],
            algorithms: vec![vec![
                Statement::Assert {
                    condition: make_var("stmt_cond"),
                    message: make_int(7),
                    level: Some(make_int(8)),
                },
                Statement::FunctionCall {
                    comp: make_comp_ref("f"),
                    args: vec![make_int(9)],
                    outputs: vec![make_var("y")],
                },
            ]],
            ..Default::default()
        }
    }

    fn assert_expression_contexts_seen(seen: &[ExpressionContext]) {
        assert!(seen.contains(&ExpressionContext::ComponentStart));
        assert!(seen.contains(&ExpressionContext::ComponentModification));
        assert!(seen.contains(&ExpressionContext::ComponentCondition));
        assert!(seen.contains(&ExpressionContext::ComponentAnnotation));
        assert!(seen.contains(&ExpressionContext::ExtendModification));
        assert!(seen.contains(&ExpressionContext::EquationAssertCondition));
        assert!(seen.contains(&ExpressionContext::EquationAssertMessage));
        assert!(seen.contains(&ExpressionContext::EquationAssertLevel));
        assert!(seen.contains(&ExpressionContext::StatementAssertCondition));
        assert!(seen.contains(&ExpressionContext::StatementAssertMessage));
        assert!(seen.contains(&ExpressionContext::StatementAssertLevel));
        assert!(seen.contains(&ExpressionContext::StatementFunctionOutput));
    }

    #[test]
    fn test_expression_context_dispatch() {
        #[derive(Default)]
        struct ExpressionContextRecorder {
            seen: Vec<ExpressionContext>,
        }

        impl Visitor for ExpressionContextRecorder {
            fn visit_expression_ctx(
                &mut self,
                expr: &Expression,
                ctx: ExpressionContext,
            ) -> ControlFlow<()> {
                self.seen.push(ctx);
                self.visit_expression(expr)
            }
        }

        let class = make_expression_context_dispatch_class();

        let mut recorder = ExpressionContextRecorder::default();
        let _ = recorder.visit_class_def(&class);
        assert_expression_contexts_seen(&recorder.seen);
    }

    struct ClassNames(Vec<String>);
    impl Visitor for ClassNames {
        fn visit_class_def(&mut self, class: &ClassDef) -> ControlFlow<()> {
            self.0.push(class.name.text.to_string());
            for (_, nested) in &class.classes {
                self.visit_class_def(nested)?;
            }
            Continue(())
        }
    }

    #[test]
    fn test_class_visitor_nested() {
        let mut inner = IndexMap::new();
        inner.insert(
            "Inner".into(),
            ClassDef {
                name: Token {
                    text: "Inner".into(),
                    ..Default::default()
                },
                ..Default::default()
            },
        );
        let mut classes = IndexMap::new();
        classes.insert(
            "Outer".into(),
            ClassDef {
                name: Token {
                    text: "Outer".into(),
                    ..Default::default()
                },
                classes: inner,
                ..Default::default()
            },
        );

        let mut visitor = ClassNames(Vec::new());
        let _ = visitor.visit_stored_definition(&StoredDefinition {
            classes,
            within: None,
        });
        assert_eq!(visitor.0, vec!["Outer", "Inner"]);
    }
}
