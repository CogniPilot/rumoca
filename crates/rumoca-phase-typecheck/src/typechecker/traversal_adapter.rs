use std::ops::ControlFlow;

use rumoca_ir_ast as ast;
use rumoca_ir_ast::Visitor;

type ComponentReference = ast::ComponentReference;
type Equation = ast::Equation;
type Expression = ast::Expression;
type Statement = ast::Statement;
type TypeTable = ast::TypeTable;

/// Callback contract for typecheck traversal.
///
/// AST recursion is delegated to `rumoca-ir-ast`; this adapter only injects
/// typecheck-specific semantic callbacks at the points the type checker needs.
pub(crate) trait TypeCheckTraversalCallbacks {
    /// Called when a component reference appears in an equation/statement/expression.
    fn on_component_reference(
        &mut self,
        _comp: &ComponentReference,
        _context: ast::ComponentReferenceContext,
        _type_table: &TypeTable,
    ) {
    }

    /// Called after the base of a field access has been traversed.
    fn on_field_access(&mut self, _base: &Expression, _field: &str, _type_table: &TypeTable) {}

    /// Called after both sides of a simple equation are traversed.
    fn on_simple_equation(&mut self, lhs: &Expression, rhs: &Expression, type_table: &TypeTable);

    /// Called after an algorithm assignment target and value are traversed.
    fn on_assignment(
        &mut self,
        _target: &ComponentReference,
        _value: &Expression,
        _type_table: &TypeTable,
    ) {
    }

    /// Called after an expression and all of its children are traversed.
    fn on_expression(&mut self, _expression: &Expression, _type_table: &TypeTable) {}

    /// Make a `for` iterator visible as an Integer while visiting its body.
    fn push_integer_iterator(&mut self, _name: &str) {}

    /// Leave one or more nested `for` iterator scopes.
    fn pop_integer_iterators(&mut self, _count: usize) {}

    /// Called after an expression-form function call and all arguments are traversed.
    fn on_expression_function_call(
        &mut self,
        _comp: &ComponentReference,
        _args: &[Expression],
        _type_table: &TypeTable,
    ) {
    }

    /// Called for every when-equation branch condition.
    fn on_when_condition(&mut self, _condition: &Expression, _type_table: &TypeTable) {}

    /// MLS §8.3.4: report which branch of an if-equation is selected at
    /// translation time.
    ///
    /// `Some(index)` selects `cond_blocks[index]`, `Some(cond_blocks.len())`
    /// selects the `else` branch, and `None` means the conditions are not
    /// translation-time evaluable, so every branch is part of the model and
    /// must be checked.
    fn select_if_equation_branch(
        &mut self,
        _cond_blocks: &[ast::EquationBlock],
        _type_table: &TypeTable,
    ) -> Option<usize> {
        None
    }
}

struct TypeCheckTraversal<'a, C> {
    callbacks: &'a mut C,
    type_table: &'a TypeTable,
}

impl<C: TypeCheckTraversalCallbacks> TypeCheckTraversal<'_, C> {
    fn new<'a>(callbacks: &'a mut C, type_table: &'a TypeTable) -> TypeCheckTraversal<'a, C> {
        TypeCheckTraversal {
            callbacks,
            type_table,
        }
    }

    fn visit_array_comprehension(
        &mut self,
        expr: &Expression,
        indices: &[rumoca_ir_ast::ForIndex],
        filter: Option<&Expression>,
    ) -> ControlFlow<()> {
        let mut pushed = 0;
        for index in indices {
            self.visit_expression(&index.range)?;
            self.callbacks
                .push_integer_iterator(index.ident.text.as_ref());
            pushed += 1;
        }
        let result = self.visit_array_comprehension_body(expr, filter);
        self.callbacks.pop_integer_iterators(pushed);
        result
    }

    /// MLS §8.3.4: walk the conditions up to and including the selected one,
    /// then only the equations the selected branch contributes to the model.
    fn visit_selected_if_branch(
        &mut self,
        cond_blocks: &[ast::EquationBlock],
        else_block: Option<&[Equation]>,
        selected: usize,
    ) -> ControlFlow<()> {
        for block in cond_blocks.iter().take(selected.saturating_add(1)) {
            self.visit_expression(&block.cond)?;
        }
        match cond_blocks.get(selected) {
            Some(block) => self.visit_each(&block.eqs, Self::visit_equation),
            None => match else_block {
                Some(equations) => self.visit_each(equations, Self::visit_equation),
                None => ControlFlow::Continue(()),
            },
        }
    }

    fn visit_array_comprehension_body(
        &mut self,
        expr: &Expression,
        filter: Option<&Expression>,
    ) -> ControlFlow<()> {
        self.visit_expression(expr)?;
        match filter {
            Some(filter) => self.visit_expression(filter),
            None => ControlFlow::Continue(()),
        }
    }
}

impl<C: TypeCheckTraversalCallbacks> Visitor for TypeCheckTraversal<'_, C> {
    fn visit_component_reference_ctx(
        &mut self,
        comp: &ComponentReference,
        context: ast::ComponentReferenceContext,
    ) -> ControlFlow<()> {
        self.callbacks
            .on_component_reference(comp, context, self.type_table);
        self.visit_component_reference(comp)
    }

    fn visit_equation(&mut self, equation: &ast::Equation) -> ControlFlow<()> {
        match equation {
            ast::Equation::When(blocks) => {
                for block in blocks {
                    self.callbacks
                        .on_when_condition(&block.cond, self.type_table);
                }
            }
            ast::Equation::If {
                cond_blocks,
                else_block,
            } => {
                if let Some(selected) = self
                    .callbacks
                    .select_if_equation_branch(cond_blocks, self.type_table)
                {
                    return self.visit_selected_if_branch(
                        cond_blocks,
                        else_block.as_deref(),
                        selected,
                    );
                }
            }
            _ => {}
        }
        ast::visitor::walk_equation_default(self, equation)
    }

    fn visit_simple_equation(&mut self, lhs: &Expression, rhs: &Expression) -> ControlFlow<()> {
        self.visit_expression(lhs)?;
        self.visit_expression(rhs)?;
        self.callbacks.on_simple_equation(lhs, rhs, self.type_table);
        ControlFlow::Continue(())
    }

    fn visit_assignment(
        &mut self,
        comp: &ComponentReference,
        value: &Expression,
    ) -> ControlFlow<()> {
        self.visit_component_reference_ctx(comp, ast::ComponentReferenceContext::AssignmentTarget)?;
        self.visit_expression(value)?;
        self.callbacks.on_assignment(comp, value, self.type_table);
        ControlFlow::Continue(())
    }

    fn visit_for_equation(
        &mut self,
        indices: &[ast::ForIndex],
        equations: &[Equation],
    ) -> ControlFlow<()> {
        let mut pushed = 0;
        for index in indices {
            self.visit_expression(&index.range)?;
            self.callbacks
                .push_integer_iterator(index.ident.text.as_ref());
            pushed += 1;
        }
        let result = self.visit_each(equations, Self::visit_equation);
        self.callbacks.pop_integer_iterators(pushed);
        result
    }

    fn visit_for_statement(
        &mut self,
        indices: &[ast::ForIndex],
        statements: &[Statement],
    ) -> ControlFlow<()> {
        let mut pushed = 0;
        for index in indices {
            self.visit_expression(&index.range)?;
            self.callbacks
                .push_integer_iterator(index.ident.text.as_ref());
            pushed += 1;
        }
        let result = self.visit_each(statements, Self::visit_statement);
        self.callbacks.pop_integer_iterators(pushed);
        result
    }

    fn visit_expr_function_call_ctx(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        ctx: ast::FunctionCallContext,
    ) -> ControlFlow<()> {
        // Equation/statement-form calls (`reinit(x, e)`, `assert(...)`) get
        // the same argument checks as expression-form calls.
        if matches!(
            ctx,
            ast::FunctionCallContext::Expression
                | ast::FunctionCallContext::Equation
                | ast::FunctionCallContext::Statement
        ) {
            self.visit_each(args, Self::visit_expression)?;
            self.callbacks
                .on_expression_function_call(comp, args, self.type_table);
            return ControlFlow::Continue(());
        }
        ast::visitor::walk_expr_function_call_ctx_default(self, comp, args, ctx)
    }

    fn visit_expression(&mut self, expression: &Expression) -> ControlFlow<()> {
        if let Expression::FieldAccess { base, field, .. } = expression {
            self.visit_expression(base)?;
            self.callbacks.on_field_access(base, field, self.type_table);
            self.callbacks.on_expression(expression, self.type_table);
            return ControlFlow::Continue(());
        }
        if let Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } = expression
        {
            self.visit_array_comprehension(expr, indices, filter.as_deref())?;
            self.callbacks.on_expression(expression, self.type_table);
            return ControlFlow::Continue(());
        }
        ast::visitor::walk_expression_default(self, expression)?;
        self.callbacks.on_expression(expression, self.type_table);
        ControlFlow::Continue(())
    }
}

pub(crate) fn walk_equations<C: TypeCheckTraversalCallbacks>(
    callbacks: &mut C,
    equations: &[Equation],
    type_table: &TypeTable,
) {
    let mut visitor = TypeCheckTraversal::new(callbacks, type_table);
    let _ = visitor.visit_each(equations, TypeCheckTraversal::visit_equation);
}

pub(crate) fn walk_equation<C: TypeCheckTraversalCallbacks>(
    callbacks: &mut C,
    equation: &Equation,
    type_table: &TypeTable,
) {
    let mut visitor = TypeCheckTraversal::new(callbacks, type_table);
    let _ = visitor.visit_equation(equation);
}

pub(crate) fn walk_statements<C: TypeCheckTraversalCallbacks>(
    callbacks: &mut C,
    statements: &[Statement],
    type_table: &TypeTable,
) {
    let mut visitor = TypeCheckTraversal::new(callbacks, type_table);
    let _ = visitor.visit_each(statements, TypeCheckTraversal::visit_statement);
}

pub(crate) fn walk_statement<C: TypeCheckTraversalCallbacks>(
    callbacks: &mut C,
    statement: &Statement,
    type_table: &TypeTable,
) {
    let mut visitor = TypeCheckTraversal::new(callbacks, type_table);
    let _ = visitor.visit_statement(statement);
}

pub(crate) fn walk_expression<C: TypeCheckTraversalCallbacks>(
    callbacks: &mut C,
    expression: &Expression,
    type_table: &TypeTable,
) {
    let mut visitor = TypeCheckTraversal::new(callbacks, type_table);
    let _ = visitor.visit_expression(expression);
}
