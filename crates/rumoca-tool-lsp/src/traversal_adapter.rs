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
    if !matches!(component.start, ast::Expression::Empty { .. }) {
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
        ast::Expression::Empty { .. } | ast::Expression::Terminal { .. } => Continue(()),
        ast::Expression::Range {
            start, step, end, ..
        } => {
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
        ast::Expression::DerivativeCall { args, .. } => {
            visitor.visit_each(args, V::visit_expression)
        }
        ast::Expression::FunctionCall { comp, args, .. } => {
            visitor.visit_expr_function_call_ctx(comp, args, FunctionCallContext::Expression)
        }
        ast::Expression::ClassModification {
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
        ast::Expression::NamedArgument { value, .. } => visitor.visit_expression(value),
        ast::Expression::Modification { target, value, .. } => {
            visitor.visit_component_reference_ctx(
                target,
                ComponentReferenceContext::ModificationTarget,
            )?;
            match value {
                Some(value) => visitor.visit_expression(value),
                None => ControlFlow::Continue(()),
            }
        }
        ast::Expression::Array { elements, .. } | ast::Expression::Tuple { elements, .. } => {
            visitor.visit_each(elements, V::visit_expression)
        }
        ast::Expression::If {
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
        ast::Expression::Parenthesized { inner, .. } => visitor.visit_expression(inner),
        ast::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            visitor.visit_expression(expr)?;
            visitor.visit_each(indices, V::visit_for_index)?;
            if let Some(f) = filter {
                visitor.visit_expression(f)?;
            }
            Continue(())
        }
        ast::Expression::ArrayIndex {
            base, subscripts, ..
        } => {
            visitor.visit_expression(base)?;
            for subscript in subscripts {
                visitor.visit_subscript_ctx(subscript, SubscriptContext::ArrayIndex)?;
            }
            Continue(())
        }
        ast::Expression::FieldAccess { base, .. } => visitor.visit_expression(base),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_compile::parsing::ast::{
        ComponentRefPart, ComponentReference, Expression, FunctionCallContext,
    };
    use rumoca_core::{Location, SourceId, Span, Token};
    use std::ops::ControlFlow::Break;
    use std::sync::Arc;

    fn identifier(name: &str) -> Token {
        Token {
            text: Arc::from(name),
            location: Location {
                start_line: 1,
                start_column: 1,
                end_line: 1,
                end_column: 1,
                start: 0,
                end: 0,
                source: SourceId::DUMMY,
            },
            token_number: 0,
            token_type: 0,
        }
    }

    fn callee(name: &str) -> ComponentReference {
        ComponentReference {
            local: false,
            span: Span::DUMMY,
            qualified_display_name: None,
            parts: vec![ComponentRefPart {
                ident: identifier(name),
                subs: None,
                def_id: None,
            }],
        }
    }

    fn variable(name: &str) -> Expression {
        Expression::ComponentReference(callee(name))
    }

    /// Build the reserved derivative node over `names`.
    ///
    /// Several arguments are used purely as a traversal witness: the walk must
    /// visit each child in order and stop on the first `Break`. This says
    /// nothing about how many arguments MLS permits `der` to take; argument
    /// arity is a semantic question owned elsewhere.
    fn derivative_of(names: &[&str]) -> Expression {
        Expression::DerivativeCall {
            args: names.iter().copied().map(variable).collect(),
            span: Span::DUMMY,
        }
    }

    /// Records each visited argument, optionally breaking at one.
    ///
    /// Any routing through the function-call callback is recorded as a failure:
    /// the reserved derivative node must never be walked as an ordinary call,
    /// because that is exactly how an invented `der` callee would slip in.
    struct ArgumentRecorder {
        seen: Vec<String>,
        stop_at: Option<&'static str>,
        function_call_callbacks: Vec<String>,
    }

    impl ArgumentRecorder {
        fn new() -> Self {
            Self {
                seen: Vec::new(),
                stop_at: None,
                function_call_callbacks: Vec::new(),
            }
        }

        fn stopping_at(name: &'static str) -> Self {
            Self {
                stop_at: Some(name),
                ..Self::new()
            }
        }
    }

    impl ast::visitor::Visitor for ArgumentRecorder {
        fn visit_expression(&mut self, expression: &Expression) -> ControlFlow<()> {
            let Expression::ComponentReference(reference) = expression else {
                return Continue(());
            };
            let name = reference.parts[0].ident.text.to_string();
            let stop = self.stop_at.is_some_and(|wanted| wanted == name);
            self.seen.push(name);
            if stop { Break(()) } else { Continue(()) }
        }

        fn visit_expr_function_call_ctx(
            &mut self,
            comp: &ComponentReference,
            args: &[Expression],
            _ctx: FunctionCallContext,
        ) -> ControlFlow<()> {
            self.function_call_callbacks
                .push(comp.parts[0].ident.text.to_string());
            self.visit_each(args, Self::visit_expression)
        }
    }

    /// Positive control for the recorder itself.
    ///
    /// Without this, the `function_call_callbacks.is_empty()` assertions in the
    /// two tests below could pass vacuously: an unwired callback records
    /// nothing no matter what the walk does.
    #[test]
    fn ordinary_function_call_does_reach_the_function_call_callback() {
        let mut recorder = ArgumentRecorder::new();
        let call = Expression::FunctionCall {
            comp: callee("f"),
            args: vec![variable("a")],
            is_partial_application: false,
            span: Span::DUMMY,
        };
        let outcome = walk_expression_default(&mut recorder, &call);
        assert_eq!(outcome, Continue(()));
        assert_eq!(
            recorder.function_call_callbacks,
            ["f"],
            "an ordinary call must reach the function-call callback, so the \
             derivative assertions are not vacuous"
        );
        assert_eq!(
            recorder.seen,
            ["a"],
            "the callback must also delegate its argument visits, so a mutant \
             routed through it still satisfies the traversal obligations"
        );
    }

    #[test]
    fn derivative_call_traversal_visits_every_argument_in_order() {
        let mut recorder = ArgumentRecorder::new();
        let outcome = walk_expression_default(&mut recorder, &derivative_of(&["a", "b", "c"]));
        assert_eq!(outcome, Continue(()));
        assert_eq!(
            recorder.seen,
            ["a", "b", "c"],
            "every derivative argument must be visited, in source order"
        );
        assert!(
            recorder.function_call_callbacks.is_empty(),
            "the derivative node must not be routed through the function-call \
             callback: {:?}",
            recorder.function_call_callbacks
        );
    }

    #[test]
    fn derivative_call_traversal_propagates_early_stop() {
        let mut recorder = ArgumentRecorder::stopping_at("b");
        let outcome = walk_expression_default(&mut recorder, &derivative_of(&["a", "b", "c"]));
        assert_eq!(
            outcome,
            Break(()),
            "ControlFlow::Break must propagate out of the derivative arm"
        );
        assert_eq!(
            recorder.seen,
            ["a", "b"],
            "traversal must stop before the argument following the break"
        );
        assert!(
            recorder.function_call_callbacks.is_empty(),
            "the derivative node must not be routed through the function-call \
             callback: {:?}",
            recorder.function_call_callbacks
        );
    }
}
