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
    fn on_component_reference(&mut self, _comp: &ComponentReference, _type_table: &TypeTable) {}

    /// Called after the base of a field access has been traversed.
    fn on_field_access(&mut self, _base: &Expression, _field: &str, _type_table: &TypeTable) {}

    /// Called after both sides of a simple equation are traversed.
    fn on_simple_equation(&mut self, lhs: &Expression, rhs: &Expression, type_table: &TypeTable);

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
}

impl<C: TypeCheckTraversalCallbacks> Visitor for TypeCheckTraversal<'_, C> {
    fn visit_component_reference_ctx(
        &mut self,
        comp: &ComponentReference,
        _ctx: ast::ComponentReferenceContext,
    ) -> ControlFlow<()> {
        self.callbacks.on_component_reference(comp, self.type_table);
        self.visit_component_reference(comp)
    }

    fn visit_equation(&mut self, equation: &ast::Equation) -> ControlFlow<()> {
        if let ast::Equation::When(blocks) = equation {
            for block in blocks {
                self.callbacks
                    .on_when_condition(&block.cond, self.type_table);
            }
        }
        ast::visitor::walk_equation_default(self, equation)
    }

    fn visit_simple_equation(&mut self, lhs: &Expression, rhs: &Expression) -> ControlFlow<()> {
        self.visit_expression(lhs)?;
        self.visit_expression(rhs)?;
        self.callbacks.on_simple_equation(lhs, rhs, self.type_table);
        ControlFlow::Continue(())
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
            return ControlFlow::Continue(());
        }
        ast::visitor::walk_expression_default(self, expression)
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

pub(crate) fn walk_statements<C: TypeCheckTraversalCallbacks>(
    callbacks: &mut C,
    statements: &[Statement],
    type_table: &TypeTable,
) {
    let mut visitor = TypeCheckTraversal::new(callbacks, type_table);
    let _ = visitor.visit_each(statements, TypeCheckTraversal::visit_statement);
}

pub(crate) fn walk_expression<C: TypeCheckTraversalCallbacks>(
    callbacks: &mut C,
    expression: &Expression,
    type_table: &TypeTable,
) {
    let mut visitor = TypeCheckTraversal::new(callbacks, type_table);
    let _ = visitor.visit_expression(expression);
}
