use crate::{
    ClassDef, Component, ComponentReference, Equation, EquationBlock, Expression, Extend,
    ExternalFunction, ForIndex, Import, Name, Statement, StatementBlock, StoredDefinition,
    Subscript,
};
use std::ops::ControlFlow::{self, Break, Continue};

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
    ExpressionFunctionCallTarget,
    EquationConnectLhs,
    EquationConnectRhs,
    EquationFunctionCallTarget,
    StatementFunctionCallTarget,
    AssignmentTarget,
    ReinitTarget,
    ClassModificationTarget,
    ModificationTarget,
    ExternalOutput,
}

/// Context for traversed type names.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeNameContext {
    ExtendsBase,
    ComponentType,
    ClassConstrainedBy,
    ComponentConstrainedBy,
}

/// Context for traversed names that are not type names.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NameContext {
    WithinClause,
    ImportPath,
}

/// Context for traversed subscript expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SubscriptContext {
    ComponentReferencePart,
    ArrayIndex,
    ClassArraySubscript,
    ComponentShape,
}

/// Context for traversed expressions from declaration/equation/statement fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpressionContext {
    Generic,
    ComponentStart,
    ComponentBinding,
    ComponentModification,
    ComponentCondition,
    ComponentAnnotation,
    ClassAnnotation,
    ExtendAnnotation,
    EquationAssertCondition,
    EquationAssertMessage,
    EquationAssertLevel,
    StatementAssertCondition,
    StatementAssertMessage,
    StatementAssertLevel,
    StatementFunctionOutput,
    ExtendModification,
    ExternalArgument,
}

pub enum VisitScope<'a> {
    Class(&'a ClassDef),
    ForEquation(&'a [ForIndex]),
    ForStatement(&'a [ForIndex]),
    ArrayComprehension(&'a [ForIndex]),
}

/// Default recursion for function calls with context-aware call-target traversal.
pub fn walk_expr_function_call_ctx_default<V: Visitor + ?Sized>(
    visitor: &mut V,
    comp: &ComponentReference,
    args: &[Expression],
    ctx: FunctionCallContext,
) -> ControlFlow<()> {
    if matches!(ctx, FunctionCallContext::Expression) {
        visitor.visit_component_reference_ctx(
            comp,
            ComponentReferenceContext::ExpressionFunctionCallTarget,
        )?;
    }
    visitor.visit_expr_function_call(comp, args)
}

/// Default recursion for an expression node.
pub fn walk_expression_default<V: Visitor + ?Sized>(
    visitor: &mut V,
    expr: &Expression,
) -> ControlFlow<()> {
    match expr {
        Expression::Empty { .. } | Expression::Terminal { .. } => Continue(()),
        Expression::Range {
            start, step, end, ..
        } => {
            visitor.visit_expression(start)?;
            if let Some(s) = step {
                visitor.visit_expression(s)?;
            }
            visitor.visit_expression(end)
        }
        Expression::Unary { rhs, .. } => visitor.visit_expression(rhs),
        Expression::Binary { lhs, rhs, .. } => {
            visitor.visit_expression(lhs)?;
            visitor.visit_expression(rhs)
        }
        Expression::ComponentReference(cr) => {
            visitor.visit_component_reference_ctx(cr, ComponentReferenceContext::Expression)
        }
        Expression::FunctionCall { comp, args, .. } => {
            visitor.visit_expr_function_call_ctx(comp, args, FunctionCallContext::Expression)
        }
        Expression::ClassModification {
            target,
            modifications,
            ..
        } => {
            visitor.visit_component_reference_ctx(
                target,
                ComponentReferenceContext::ClassModificationTarget,
            )?;
            visitor.visit_each(modifications, V::visit_expression)
        }
        Expression::NamedArgument { value, .. } => visitor.visit_expression(value),
        Expression::Modification { target, value, .. } => {
            visitor.visit_component_reference_ctx(
                target,
                ComponentReferenceContext::ModificationTarget,
            )?;
            visitor.visit_expression(value)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            visitor.visit_each(elements, V::visit_expression)
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                visitor.visit_expression(cond)?;
                visitor.visit_expression(then_expr)?;
            }
            visitor.visit_expression(else_branch)
        }
        Expression::Parenthesized { inner, .. } => visitor.visit_expression(inner),
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            visitor.visit_each(indices, V::visit_for_index)?;
            visitor.enter_scope(VisitScope::ArrayComprehension(indices))?;
            let result = (|| {
                visitor.visit_expression(expr)?;
                if let Some(f) = filter {
                    visitor.visit_expression(f)?;
                }
                Continue(())
            })();
            finish_scope(
                result,
                visitor.exit_scope(VisitScope::ArrayComprehension(indices)),
            )
        }
        Expression::ArrayIndex {
            base, subscripts, ..
        } => {
            visitor.visit_expression(base)?;
            for subscript in subscripts {
                visitor.visit_subscript_ctx(subscript, SubscriptContext::ArrayIndex)?;
            }
            Continue(())
        }
        Expression::FieldAccess { base, .. } => visitor.visit_expression(base),
    }
}

/// Default recursion for an equation node.
pub fn walk_equation_default<V: Visitor + ?Sized>(
    visitor: &mut V,
    eq: &Equation,
) -> ControlFlow<()> {
    match eq {
        Equation::Empty => Continue(()),
        Equation::Simple { lhs, rhs } => visitor.visit_simple_equation(lhs, rhs),
        Equation::Connect { lhs, rhs } => visitor.visit_connect(lhs, rhs),
        Equation::For { indices, equations } => visitor.visit_for_equation(indices, equations),
        Equation::When(blocks) => visitor.visit_when_equation(blocks),
        Equation::If {
            cond_blocks,
            else_block,
        } => visitor.visit_if_equation(cond_blocks, else_block.as_deref()),
        Equation::FunctionCall { comp, args } => visitor.visit_equation_function_call(comp, args),
        Equation::Assert {
            condition,
            message,
            level,
        } => visitor.visit_equation_assert(condition, message, level.as_ref()),
    }
}

/// Default recursion for a statement node.
pub fn walk_statement_default<V: Visitor + ?Sized>(
    visitor: &mut V,
    stmt: &Statement,
) -> ControlFlow<()> {
    match stmt {
        Statement::Empty | Statement::Return { .. } | Statement::Break { .. } => Continue(()),
        Statement::Assignment { comp, value } => visitor.visit_assignment(comp, value),
        Statement::For { indices, equations } => visitor.visit_for_statement(indices, equations),
        Statement::While(block) => visitor.visit_statement_block(block),
        Statement::If {
            cond_blocks,
            else_block,
        } => visitor.visit_if_statement(cond_blocks, else_block.as_deref()),
        Statement::When(blocks) => visitor.visit_when_statement(blocks),
        Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => visitor.visit_statement_function_call(comp, args, outputs),
        Statement::Reinit { variable, value } => visitor.visit_reinit(variable, value),
        Statement::Assert {
            condition,
            message,
            level,
        } => visitor.visit_statement_assert(condition, message, level.as_deref()),
    }
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
        ctx: FunctionCallContext,
    ) -> ControlFlow<()> {
        walk_expr_function_call_ctx_default(self, comp, args, ctx)
    }

    /// Visit a type name in declaration context.
    ///
    /// Default behavior is no-op to preserve existing traversal behavior.
    fn visit_type_name(&mut self, _name: &Name, _ctx: TypeNameContext) -> ControlFlow<()> {
        Continue(())
    }

    /// Visit a non-type name with contextual metadata.
    ///
    /// Default behavior is no-op.
    fn visit_name_ctx(&mut self, _name: &Name, _ctx: NameContext) -> ControlFlow<()> {
        Continue(())
    }

    /// Visit a subscript with contextual metadata.
    ///
    /// Default behavior defers to [`Visitor::visit_subscript`].
    fn visit_subscript_ctx(&mut self, sub: &Subscript, _ctx: SubscriptContext) -> ControlFlow<()> {
        self.visit_subscript(sub)
    }

    fn enter_scope(&mut self, _scope: VisitScope<'_>) -> ControlFlow<()> {
        Continue(())
    }

    fn exit_scope(&mut self, _scope: VisitScope<'_>) -> ControlFlow<()> {
        Continue(())
    }

    // =========================================================================
    // Expression methods
    // =========================================================================

    /// Visit any expression.
    fn visit_expression(&mut self, expr: &Expression) -> ControlFlow<()> {
        walk_expression_default(self, expr)
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
        walk_equation_default(self, eq)
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
        self.enter_scope(VisitScope::ForEquation(indices))?;
        let result = self.visit_each(equations, Self::visit_equation);
        finish_scope(result, self.exit_scope(VisitScope::ForEquation(indices)))
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
        walk_statement_default(self, stmt)
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
        self.enter_scope(VisitScope::ForStatement(indices))?;
        let result = self.visit_each(statements, Self::visit_statement);
        finish_scope(result, self.exit_scope(VisitScope::ForStatement(indices)))
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
        if let Some(within) = &def.within {
            self.visit_name_ctx(within, NameContext::WithinClause)?;
        }
        for (_, class) in &def.classes {
            self.visit_class_def(class)?;
        }
        Continue(())
    }

    /// Visit a class definition.
    fn visit_class_def(&mut self, class: &ClassDef) -> ControlFlow<()> {
        self.enter_scope(VisitScope::Class(class))?;
        let result = (|| {
            if let Some(constrainedby) = &class.constrainedby {
                self.visit_type_name(constrainedby, TypeNameContext::ClassConstrainedBy)?;
            }
            for ext in &class.extends {
                self.visit_extend(ext)?;
            }
            self.visit_each(&class.imports, Self::visit_import)?;
            for subscript in &class.array_subscripts {
                self.visit_subscript_ctx(subscript, SubscriptContext::ClassArraySubscript)?;
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
            for annotation in &class.annotation {
                self.visit_expression_ctx(annotation, ExpressionContext::ClassAnnotation)?;
            }
            if let Some(external) = &class.external {
                self.visit_external_function(external)?;
            }
            Continue(())
        })();
        finish_scope(result, self.exit_scope(VisitScope::Class(class)))
    }

    /// Visit an import clause.
    fn visit_import(&mut self, import: &Import) -> ControlFlow<()> {
        self.visit_name_ctx(import.base_path(), NameContext::ImportPath)
    }

    /// Visit an extends clause.
    fn visit_extend(&mut self, ext: &Extend) -> ControlFlow<()> {
        self.visit_type_name(&ext.base_name, TypeNameContext::ExtendsBase)?;
        for modification in &ext.modifications {
            self.visit_expression_ctx(&modification.expr, ExpressionContext::ExtendModification)?;
        }
        for annotation in &ext.annotation {
            self.visit_expression_ctx(annotation, ExpressionContext::ExtendAnnotation)?;
        }
        Continue(())
    }

    /// Visit a component declaration.
    fn visit_component(&mut self, comp: &Component) -> ControlFlow<()> {
        self.visit_type_name(&comp.type_name, TypeNameContext::ComponentType)?;
        if let Some(constrainedby) = &comp.constrainedby {
            self.visit_type_name(constrainedby, TypeNameContext::ComponentConstrainedBy)?;
        }
        for subscript in &comp.shape_expr {
            self.visit_subscript_ctx(subscript, SubscriptContext::ComponentShape)?;
        }
        if !matches!(comp.start, Expression::Empty { .. }) {
            self.visit_expression_ctx(&comp.start, ExpressionContext::ComponentStart)?;
        }
        if let Some(binding) = &comp.binding {
            self.visit_expression_ctx(binding, ExpressionContext::ComponentBinding)?;
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

    /// Visit an external function declaration.
    fn visit_external_function(&mut self, external: &ExternalFunction) -> ControlFlow<()> {
        if let Some(output) = &external.output {
            self.visit_component_reference_ctx(output, ComponentReferenceContext::ExternalOutput)?;
        }
        for arg in &external.args {
            self.visit_expression_ctx(arg, ExpressionContext::ExternalArgument)?;
        }
        Continue(())
    }
}

fn finish_scope(result: ControlFlow<()>, exit: ControlFlow<()>) -> ControlFlow<()> {
    match result {
        Break(()) => Break(()),
        Continue(()) => exit,
    }
}
