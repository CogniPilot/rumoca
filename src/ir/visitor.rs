//! This module defines the `Visitor` and `MutVisitor` traits for implementing
//! the Visitor design pattern in the context of an intermediate representation (IR)
//! for an abstract syntax tree (AST).
//!
//! ## Overview
//!
//! Two visitor traits are provided:
//! - `Visitor` - For immutable traversal (analysis, code generation, LSP features)
//! - `MutVisitor` - For mutable traversal (transformations, optimizations)
//!
//! Each trait provides methods for entering and exiting various types of AST nodes.
//! These methods can be overridden to implement custom behavior when traversing the AST.
//!
//! ## Key Components
//!
//! - **`Visitor` Trait**: Immutable visitor for read-only traversal. Used for analysis,
//!   code generation, and LSP features like semantic tokens, references, etc.
//!
//! - **`MutVisitor` Trait**: Mutable visitor with `enter_*` and `exit_*` methods for each
//!   AST node type. Used for transformations that modify the AST.
use crate::ir;

// =============================================================================
// Immutable Visitor (for analysis, LSP features)
// =============================================================================

/// Immutable visitor trait for AST analysis.
///
/// Implement this trait for read-only traversal such as:
/// - Semantic token collection
/// - Reference finding
/// - Symbol collection
/// - Code analysis
pub trait Visitor {
    fn enter_stored_definition(&mut self, _node: &ir::ast::StoredDefinition) {}
    fn exit_stored_definition(&mut self, _node: &ir::ast::StoredDefinition) {}

    fn enter_class_definition(&mut self, _node: &ir::ast::ClassDefinition) {}
    fn exit_class_definition(&mut self, _node: &ir::ast::ClassDefinition) {}

    fn enter_equation(&mut self, _node: &ir::ast::Equation) {}
    fn exit_equation(&mut self, _node: &ir::ast::Equation) {}

    fn enter_statement(&mut self, _node: &ir::ast::Statement) {}
    fn exit_statement(&mut self, _node: &ir::ast::Statement) {}

    fn enter_expression(&mut self, _node: &ir::ast::Expression) {}
    fn exit_expression(&mut self, _node: &ir::ast::Expression) {}

    fn enter_component(&mut self, _node: &ir::ast::Component) {}
    fn exit_component(&mut self, _node: &ir::ast::Component) {}

    fn enter_component_reference(&mut self, _node: &ir::ast::ComponentReference) {}
    fn exit_component_reference(&mut self, _node: &ir::ast::ComponentReference) {}
}

/// Trait for AST nodes that can accept an immutable visitor.
pub trait Visitable {
    fn accept<V: Visitor>(&self, visitor: &mut V);
}

// =============================================================================
// Mutable Visitor (for transformations)
// =============================================================================

/// Mutable visitor trait for AST transformations.
///
/// Implement this trait to modify the AST during traversal.
pub trait MutVisitor {
    fn enter_stored_definition(&mut self, _node: &mut ir::ast::StoredDefinition) {}
    fn exit_stored_definition(&mut self, _node: &mut ir::ast::StoredDefinition) {}

    fn enter_class_definition(&mut self, _node: &mut ir::ast::ClassDefinition) {}
    fn exit_class_definition(&mut self, _node: &mut ir::ast::ClassDefinition) {}

    fn enter_equation(&mut self, _node: &mut ir::ast::Equation) {}
    fn exit_equation(&mut self, _node: &mut ir::ast::Equation) {}

    fn enter_statement(&mut self, _node: &mut ir::ast::Statement) {}
    fn exit_statement(&mut self, _node: &mut ir::ast::Statement) {}

    fn enter_expression(&mut self, _node: &mut ir::ast::Expression) {}
    fn exit_expression(&mut self, _node: &mut ir::ast::Expression) {}

    fn enter_component(&mut self, _node: &mut ir::ast::Component) {}
    fn exit_component(&mut self, _node: &mut ir::ast::Component) {}

    fn enter_component_reference(&mut self, _node: &mut ir::ast::ComponentReference) {}
    fn exit_component_reference(&mut self, _node: &mut ir::ast::ComponentReference) {}
}

/// Trait for AST nodes that can accept a mutable visitor.
pub trait MutVisitable {
    fn accept_mut<V: MutVisitor>(&mut self, visitor: &mut V);
}

// =============================================================================
// Immutable Visitable Implementations
// =============================================================================

impl Visitable for ir::ast::StoredDefinition {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.enter_stored_definition(self);
        for (_name, class) in &self.class_list {
            class.accept(visitor);
        }
        visitor.exit_stored_definition(self);
    }
}

impl Visitable for ir::ast::ClassDefinition {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.enter_class_definition(self);

        // Visit components
        for comp in self.components.values() {
            comp.accept(visitor);
        }

        // Visit equations
        for eq in &self.equations {
            eq.accept(visitor);
        }
        for eq in &self.initial_equations {
            eq.accept(visitor);
        }

        // Visit algorithms (statements)
        for algo in &self.algorithms {
            for stmt in algo {
                stmt.accept(visitor);
            }
        }
        for algo in &self.initial_algorithms {
            for stmt in algo {
                stmt.accept(visitor);
            }
        }

        // Visit nested classes
        for nested in self.classes.values() {
            nested.accept(visitor);
        }

        visitor.exit_class_definition(self);
    }
}

impl Visitable for ir::ast::Equation {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
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
                    for eq in &block.eqs {
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
                    for eq in &block.eqs {
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

impl Visitable for ir::ast::Statement {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.enter_statement(self);
        match self {
            ir::ast::Statement::Empty => {}
            ir::ast::Statement::Assignment { comp, value } => {
                comp.accept(visitor);
                value.accept(visitor);
            }
            ir::ast::Statement::Return { .. } | ir::ast::Statement::Break { .. } => {}
            ir::ast::Statement::For { indices, equations } => {
                for index in indices {
                    index.range.accept(visitor);
                }
                for stmt in equations {
                    stmt.accept(visitor);
                }
            }
            ir::ast::Statement::While(block) => {
                block.cond.accept(visitor);
                for stmt in &block.stmts {
                    stmt.accept(visitor);
                }
            }
            ir::ast::Statement::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    block.cond.accept(visitor);
                    for stmt in &block.stmts {
                        stmt.accept(visitor);
                    }
                }
                if let Some(else_stmts) = else_block {
                    for stmt in else_stmts {
                        stmt.accept(visitor);
                    }
                }
            }
            ir::ast::Statement::When(blocks) => {
                for block in blocks {
                    block.cond.accept(visitor);
                    for stmt in &block.stmts {
                        stmt.accept(visitor);
                    }
                }
            }
            ir::ast::Statement::FunctionCall { comp, args } => {
                comp.accept(visitor);
                for arg in args {
                    arg.accept(visitor);
                }
            }
        }
        visitor.exit_statement(self);
    }
}

impl Visitable for ir::ast::Expression {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.enter_expression(self);
        match self {
            ir::ast::Expression::Unary { rhs, .. } => {
                rhs.accept(visitor);
            }
            ir::ast::Expression::Binary { lhs, rhs, .. } => {
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
                if let Some(step) = step {
                    step.accept(visitor);
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
            ir::ast::Expression::Parenthesized { inner } => {
                inner.accept(visitor);
            }
            ir::ast::Expression::ArrayComprehension { expr, indices } => {
                expr.accept(visitor);
                for idx in indices {
                    idx.range.accept(visitor);
                }
            }
        }
        visitor.exit_expression(self);
    }
}

impl Visitable for ir::ast::Component {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.enter_component(self);
        self.start.accept(visitor);
        visitor.exit_component(self);
    }
}

impl Visitable for ir::ast::ComponentReference {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.enter_component_reference(self);
        visitor.exit_component_reference(self);
    }
}

// =============================================================================
// Mutable Visitable Implementations
// =============================================================================

impl MutVisitable for ir::ast::StoredDefinition {
    fn accept_mut<V: MutVisitor>(&mut self, visitor: &mut V) {
        visitor.enter_stored_definition(self);
        for (_name, class) in &mut self.class_list {
            class.accept_mut(visitor);
        }
        visitor.exit_stored_definition(self);
    }
}

impl MutVisitable for ir::ast::ClassDefinition {
    fn accept_mut<V: MutVisitor>(&mut self, visitor: &mut V) {
        visitor.enter_class_definition(self);

        // Visit components
        for comp in self.components.values_mut() {
            comp.accept_mut(visitor);
        }

        // Visit equations
        for eq in &mut self.equations {
            eq.accept_mut(visitor);
        }
        for eq in &mut self.initial_equations {
            eq.accept_mut(visitor);
        }

        // Visit algorithms (statements)
        for algo in &mut self.algorithms {
            for stmt in algo {
                stmt.accept_mut(visitor);
            }
        }
        for algo in &mut self.initial_algorithms {
            for stmt in algo {
                stmt.accept_mut(visitor);
            }
        }

        // Visit nested classes
        for nested in self.classes.values_mut() {
            nested.accept_mut(visitor);
        }

        visitor.exit_class_definition(self);
    }
}

impl MutVisitable for ir::ast::Equation {
    fn accept_mut<V: MutVisitor>(&mut self, visitor: &mut V) {
        visitor.enter_equation(self);
        match self {
            ir::ast::Equation::Simple { lhs, rhs } => {
                lhs.accept_mut(visitor);
                rhs.accept_mut(visitor);
            }
            ir::ast::Equation::FunctionCall { comp, args } => {
                comp.accept_mut(visitor);
                for arg in args {
                    arg.accept_mut(visitor);
                }
            }
            ir::ast::Equation::For { indices, equations } => {
                for index in indices {
                    index.range.accept_mut(visitor);
                }
                for eq in equations {
                    eq.accept_mut(visitor);
                }
            }
            ir::ast::Equation::Connect { lhs, rhs } => {
                lhs.accept_mut(visitor);
                rhs.accept_mut(visitor);
            }
            ir::ast::Equation::When(blocks) => {
                for block in blocks {
                    block.cond.accept_mut(visitor);
                    for eq in &mut block.eqs {
                        eq.accept_mut(visitor);
                    }
                }
            }
            ir::ast::Equation::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    block.cond.accept_mut(visitor);
                    for eq in &mut block.eqs {
                        eq.accept_mut(visitor);
                    }
                }
                if let Some(else_block) = else_block {
                    for eq in else_block {
                        eq.accept_mut(visitor);
                    }
                }
            }
            ir::ast::Equation::Empty => {}
        }
        visitor.exit_equation(self);
    }
}

impl MutVisitable for ir::ast::Statement {
    fn accept_mut<V: MutVisitor>(&mut self, visitor: &mut V) {
        visitor.enter_statement(self);
        match self {
            ir::ast::Statement::Empty => {}
            ir::ast::Statement::Assignment { comp, value } => {
                comp.accept_mut(visitor);
                value.accept_mut(visitor);
            }
            ir::ast::Statement::Return { .. } | ir::ast::Statement::Break { .. } => {}
            ir::ast::Statement::For { indices, equations } => {
                for index in indices {
                    index.range.accept_mut(visitor);
                }
                for stmt in equations {
                    stmt.accept_mut(visitor);
                }
            }
            ir::ast::Statement::While(block) => {
                block.cond.accept_mut(visitor);
                for stmt in &mut block.stmts {
                    stmt.accept_mut(visitor);
                }
            }
            ir::ast::Statement::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    block.cond.accept_mut(visitor);
                    for stmt in &mut block.stmts {
                        stmt.accept_mut(visitor);
                    }
                }
                if let Some(else_stmts) = else_block {
                    for stmt in else_stmts {
                        stmt.accept_mut(visitor);
                    }
                }
            }
            ir::ast::Statement::When(blocks) => {
                for block in blocks {
                    block.cond.accept_mut(visitor);
                    for stmt in &mut block.stmts {
                        stmt.accept_mut(visitor);
                    }
                }
            }
            ir::ast::Statement::FunctionCall { comp, args } => {
                comp.accept_mut(visitor);
                for arg in args {
                    arg.accept_mut(visitor);
                }
            }
        }
        visitor.exit_statement(self);
    }
}

impl MutVisitable for ir::ast::Expression {
    fn accept_mut<V: MutVisitor>(&mut self, visitor: &mut V) {
        visitor.enter_expression(self);
        match self {
            ir::ast::Expression::Unary { rhs, .. } => {
                rhs.accept_mut(visitor);
            }
            ir::ast::Expression::Binary { lhs, rhs, .. } => {
                lhs.accept_mut(visitor);
                rhs.accept_mut(visitor);
            }
            ir::ast::Expression::ComponentReference(cref) => {
                cref.accept_mut(visitor);
            }
            ir::ast::Expression::FunctionCall { comp, args } => {
                comp.accept_mut(visitor);
                for arg in args {
                    arg.accept_mut(visitor);
                }
            }
            ir::ast::Expression::Array { elements } => {
                for element in elements {
                    element.accept_mut(visitor);
                }
            }
            ir::ast::Expression::Range { start, step, end } => {
                start.accept_mut(visitor);
                if let Some(step) = step {
                    step.accept_mut(visitor);
                }
                end.accept_mut(visitor);
            }
            ir::ast::Expression::Terminal { .. } => {}
            ir::ast::Expression::Empty => {}
            ir::ast::Expression::Tuple { elements } => {
                for element in elements {
                    element.accept_mut(visitor);
                }
            }
            ir::ast::Expression::If {
                branches,
                else_branch,
            } => {
                for (cond, then_expr) in branches {
                    cond.accept_mut(visitor);
                    then_expr.accept_mut(visitor);
                }
                else_branch.accept_mut(visitor);
            }
            ir::ast::Expression::Parenthesized { inner } => {
                inner.accept_mut(visitor);
            }
            ir::ast::Expression::ArrayComprehension { expr, indices } => {
                expr.accept_mut(visitor);
                for idx in indices {
                    idx.range.accept_mut(visitor);
                }
            }
        }
        visitor.exit_expression(self);
    }
}

impl MutVisitable for ir::ast::Component {
    fn accept_mut<V: MutVisitor>(&mut self, visitor: &mut V) {
        visitor.enter_component(self);
        self.start.accept_mut(visitor);
        visitor.exit_component(self);
    }
}

impl MutVisitable for ir::ast::ComponentReference {
    fn accept_mut<V: MutVisitor>(&mut self, visitor: &mut V) {
        visitor.enter_component_reference(self);
        visitor.exit_component_reference(self);
    }
}

// =============================================================================
// Generic Collector Visitors
// =============================================================================

/// A generic collector that extracts items from ComponentReferences.
///
/// This provides a reusable pattern for collecting data from the AST
/// using a closure-based API.
///
/// # Example
///
/// ```
/// use rumoca::ir::visitor::{Collector, Visitable};
/// use rumoca::ir::ast::ComponentReference;
///
/// // Collect all component reference names
/// let collector: Collector<String, _> = Collector::new(|cref: &ComponentReference| {
///     cref.parts.first().map(|p| p.ident.text.clone())
/// });
/// // Then call: class.accept(&mut collector);
/// // let names: Vec<String> = collector.into_collected();
/// ```
pub struct Collector<T, F>
where
    F: FnMut(&ir::ast::ComponentReference) -> Option<T>,
{
    extractor: F,
    collected: Vec<T>,
}

impl<T, F> Collector<T, F>
where
    F: FnMut(&ir::ast::ComponentReference) -> Option<T>,
{
    /// Create a new collector with the given extractor function.
    pub fn new(extractor: F) -> Self {
        Self {
            extractor,
            collected: Vec::new(),
        }
    }

    /// Get the collected items as a reference.
    pub fn collected(&self) -> &[T] {
        &self.collected
    }

    /// Consume the collector and return the collected items.
    pub fn into_collected(self) -> Vec<T> {
        self.collected
    }
}

impl<T, F> Visitor for Collector<T, F>
where
    F: FnMut(&ir::ast::ComponentReference) -> Option<T>,
{
    fn enter_component_reference(&mut self, node: &ir::ast::ComponentReference) {
        if let Some(item) = (self.extractor)(node) {
            self.collected.push(item);
        }
    }
}

/// A generic collector that extracts items from Expressions.
///
/// Useful for collecting data from function calls, literals, etc.
pub struct ExpressionCollector<T, F>
where
    F: FnMut(&ir::ast::Expression) -> Option<T>,
{
    extractor: F,
    collected: Vec<T>,
}

impl<T, F> ExpressionCollector<T, F>
where
    F: FnMut(&ir::ast::Expression) -> Option<T>,
{
    /// Create a new expression collector with the given extractor function.
    pub fn new(extractor: F) -> Self {
        Self {
            extractor,
            collected: Vec::new(),
        }
    }

    /// Get the collected items as a reference.
    pub fn collected(&self) -> &[T] {
        &self.collected
    }

    /// Consume the collector and return the collected items.
    pub fn into_collected(self) -> Vec<T> {
        self.collected
    }
}

impl<T, F> Visitor for ExpressionCollector<T, F>
where
    F: FnMut(&ir::ast::Expression) -> Option<T>,
{
    fn enter_expression(&mut self, node: &ir::ast::Expression) {
        if let Some(item) = (self.extractor)(node) {
            self.collected.push(item);
        }
    }
}

/// Collect strings from component references in an AST node.
///
/// This is a convenience function for the common case of collecting
/// component reference names.
///
/// # Example
///
/// ```
/// use rumoca::ir::visitor::{collect_component_refs, Visitable};
/// use rumoca::ir::ast::ClassDefinition;
///
/// let class = ClassDefinition::default();
/// let names = collect_component_refs(&class, |cref| {
///     cref.parts.first().map(|p| p.ident.text.clone())
/// });
/// ```
pub fn collect_component_refs<V: Visitable>(
    node: &V,
    mut extractor: impl FnMut(&ir::ast::ComponentReference) -> Option<String>,
) -> std::collections::HashSet<String> {
    let mut collector = Collector::new(|cref| extractor(cref));
    node.accept(&mut collector);
    collector.into_collected().into_iter().collect()
}

/// Collect strings from expressions in an AST node.
///
/// This is a convenience function for collecting data from expressions.
pub fn collect_from_expressions<V: Visitable>(
    node: &V,
    mut extractor: impl FnMut(&ir::ast::Expression) -> Option<String>,
) -> std::collections::HashSet<String> {
    let mut collector = ExpressionCollector::new(|expr| extractor(expr));
    node.accept(&mut collector);
    collector.into_collected().into_iter().collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::*;
    use crate::modelica_grammar::ModelicaGrammar;
    use crate::modelica_parser::parse;

    fn parse_test_code(code: &str) -> StoredDefinition {
        let mut grammar = ModelicaGrammar::new();
        parse(code, "test.mo", &mut grammar).expect("Failed to parse test code");
        grammar.modelica.expect("No AST produced")
    }

    /// Test visitor that counts classes, components, and expressions
    struct CountingVisitor {
        classes: usize,
        components: usize,
        expressions: usize,
        equations: usize,
    }

    impl Visitor for CountingVisitor {
        fn enter_class_definition(&mut self, _node: &ClassDefinition) {
            self.classes += 1;
        }

        fn enter_component(&mut self, _node: &Component) {
            self.components += 1;
        }

        fn enter_expression(&mut self, _node: &Expression) {
            self.expressions += 1;
        }

        fn enter_equation(&mut self, _node: &Equation) {
            self.equations += 1;
        }
    }

    #[test]
    fn test_visitor_counts() {
        let code = r#"
model Test
  Real x;
  Real y;
equation
  x = 1.0;
  y = x + 2.0;
end Test;
"#;
        let ast = parse_test_code(code);
        let mut visitor = CountingVisitor {
            classes: 0,
            components: 0,
            expressions: 0,
            equations: 0,
        };

        ast.accept(&mut visitor);

        assert_eq!(visitor.classes, 1, "Should have 1 class");
        assert_eq!(visitor.components, 2, "Should have 2 components (x, y)");
        assert_eq!(visitor.equations, 2, "Should have 2 equations");
        // Expressions: x, 1.0, y, x, 2.0, (x + 2.0)
        assert!(visitor.expressions >= 4, "Should have multiple expressions");
    }

    #[test]
    fn test_nested_classes() {
        let code = r#"
model Outer
  Real x;
  model Inner
    Real y;
  end Inner;
end Outer;
"#;
        let ast = parse_test_code(code);
        let mut visitor = CountingVisitor {
            classes: 0,
            components: 0,
            expressions: 0,
            equations: 0,
        };

        ast.accept(&mut visitor);

        assert_eq!(visitor.classes, 2, "Should have 2 classes (Outer, Inner)");
        assert_eq!(
            visitor.components, 2,
            "Should have 2 components (x in Outer, y in Inner)"
        );
    }

    #[test]
    fn test_generic_collector() {
        let code = r#"
model Test
  Real x;
  Real y;
equation
  x = y + 1.0;
end Test;
"#;
        let ast = parse_test_code(code);
        let class = ast.class_list.get("Test").expect("Test class not found");

        // Use the generic collector to collect component reference names
        let names = collect_component_refs(class, |cref| {
            cref.parts.first().map(|p| p.ident.text.clone())
        });

        assert!(names.contains("x"), "Should contain 'x'");
        assert!(names.contains("y"), "Should contain 'y'");
    }

    #[test]
    fn test_collector_struct_directly() {
        let code = r#"
model Test
  Real a;
  Real b;
equation
  a = b * 2.0;
end Test;
"#;
        let ast = parse_test_code(code);
        let class = ast.class_list.get("Test").expect("Test class not found");

        // Use the Collector struct directly
        let mut collector = Collector::new(|cref: &ComponentReference| {
            cref.parts.first().map(|p| p.ident.text.clone())
        });
        class.accept(&mut collector);

        let names: Vec<String> = collector.into_collected();
        assert!(names.contains(&"a".to_string()));
        assert!(names.contains(&"b".to_string()));
    }
}
