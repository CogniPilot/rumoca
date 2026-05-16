use rumoca_compile::parsing::ast;
use rumoca_compile::parsing::ast::{
    ComponentReferenceContext, ExpressionContext, FunctionCallContext, NameContext,
    SubscriptContext,
};
use std::ops::ControlFlow::{self, Continue};

pub(crate) fn walk_stored_definition<V: ast::visitor::Visitor>(
    visitor: &mut V,
    def: &ast::StoredDefinition,
) -> ControlFlow<()> {
    if let Some(within) = &def.within {
        visitor.visit_name_ctx(within, NameContext::WithinClause)?;
    }
    for (_, class) in &def.classes {
        visitor.visit_class_def(class)?;
    }
    Continue(())
}

pub(crate) fn walk_class_sections<V: ast::visitor::Visitor>(
    visitor: &mut V,
    class: &ast::ClassDef,
    include_extends: bool,
) -> ControlFlow<()> {
    if include_extends {
        for ext in &class.extends {
            visitor.visit_extend(ext)?;
        }
    }
    for import in &class.imports {
        visitor.visit_import(import)?;
    }
    for subscript in &class.array_subscripts {
        visitor.visit_subscript_ctx(subscript, SubscriptContext::ClassArraySubscript)?;
    }
    for (_, comp) in &class.components {
        visitor.visit_component(comp)?;
    }
    visitor.visit_each(&class.equations, V::visit_equation)?;
    visitor.visit_each(&class.initial_equations, V::visit_equation)?;
    for section in &class.algorithms {
        visitor.visit_each(section, V::visit_statement)?;
    }
    for section in &class.initial_algorithms {
        visitor.visit_each(section, V::visit_statement)?;
    }
    for annotation in &class.annotation {
        visitor.visit_expression_ctx(annotation, ExpressionContext::ClassAnnotation)?;
    }
    if let Some(external) = &class.external {
        visitor.visit_external_function(external)?;
    }
    for (_, nested) in &class.classes {
        visitor.visit_class_def(nested)?;
    }
    Continue(())
}

pub(crate) fn walk_component_fields<V: ast::visitor::Visitor>(
    visitor: &mut V,
    component: &ast::Component,
) -> ControlFlow<()> {
    for subscript in &component.shape_expr {
        visitor.visit_subscript_ctx(subscript, SubscriptContext::ComponentShape)?;
    }
    if !matches!(component.start, ast::Expression::Empty) {
        visitor.visit_expression_ctx(&component.start, ExpressionContext::ComponentStart)?;
    }
    if let Some(binding) = &component.binding {
        visitor.visit_expression_ctx(binding, ExpressionContext::ComponentBinding)?;
    }
    for (_, mod_expr) in &component.modifications {
        visitor.visit_expression_ctx(mod_expr, ExpressionContext::ComponentModification)?;
    }
    if let Some(cond) = &component.condition {
        visitor.visit_expression_ctx(cond, ExpressionContext::ComponentCondition)?;
    }
    for annotation in &component.annotation {
        visitor.visit_expression_ctx(annotation, ExpressionContext::ComponentAnnotation)?;
    }
    Continue(())
}

pub(crate) fn walk_expression_default<V: ast::visitor::Visitor>(
    visitor: &mut V,
    expression: &ast::Expression,
) -> ControlFlow<()> {
    match expression {
        ast::Expression::Empty | ast::Expression::Terminal { .. } => Continue(()),
        ast::Expression::Range { start, step, end } => {
            visitor.visit_expression(start)?;
            if let Some(s) = step {
                visitor.visit_expression(s)?;
            }
            visitor.visit_expression(end)
        }
        ast::Expression::Unary { rhs, .. } => visitor.visit_expression(rhs),
        ast::Expression::Binary { lhs, rhs, .. } => {
            visitor.visit_expression(lhs)?;
            visitor.visit_expression(rhs)
        }
        ast::Expression::ComponentReference(cr) => {
            visitor.visit_component_reference_ctx(cr, ComponentReferenceContext::Expression)
        }
        ast::Expression::FunctionCall { comp, args } => {
            visitor.visit_expr_function_call_ctx(comp, args, FunctionCallContext::Expression)
        }
        ast::Expression::ClassModification {
            target,
            modifications,
        } => {
            visitor.visit_component_reference_ctx(
                target,
                ComponentReferenceContext::ClassModificationTarget,
            )?;
            visitor.visit_each(modifications, V::visit_expression)
        }
        ast::Expression::NamedArgument { value, .. } => visitor.visit_expression(value),
        ast::Expression::Modification { target, value } => {
            visitor.visit_component_reference_ctx(
                target,
                ComponentReferenceContext::ModificationTarget,
            )?;
            visitor.visit_expression(value)
        }
        ast::Expression::Array { elements, .. } | ast::Expression::Tuple { elements } => {
            visitor.visit_each(elements, V::visit_expression)
        }
        ast::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                visitor.visit_expression(cond)?;
                visitor.visit_expression(then_expr)?;
            }
            visitor.visit_expression(else_branch)
        }
        ast::Expression::Parenthesized { inner } => visitor.visit_expression(inner),
        ast::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            visitor.visit_expression(expr)?;
            visitor.visit_each(indices, V::visit_for_index)?;
            if let Some(f) = filter {
                visitor.visit_expression(f)?;
            }
            Continue(())
        }
        ast::Expression::ArrayIndex { base, subscripts } => {
            visitor.visit_expression(base)?;
            for subscript in subscripts {
                visitor.visit_subscript_ctx(subscript, SubscriptContext::ArrayIndex)?;
            }
            Continue(())
        }
        ast::Expression::FieldAccess { base, .. } => visitor.visit_expression(base),
    }
}
