//! Eligibility of symbolic family templates with nested reduction domains.
//!
//! A reduction can use an enclosing static for-equation index in its bounds.
//! Its concrete rows then have exact domains, but a single symbolic family body
//! does not have one fixed reduction shape. Keep those already-specialized rows
//! instead of attaching a template that loses the index's translation-time value.

use std::collections::HashSet;
use std::ops::ControlFlow;

use ast::Visitor;
use rumoca_ir_ast as ast;

pub(super) fn has_binder_dependent_domain(
    expression: &ast::Expression,
    binders: &HashSet<String>,
) -> bool {
    DependentDomains {
        binders: binders.clone(),
    }
    .visit_expression(expression)
    .is_break()
}

struct DependentDomains {
    binders: HashSet<String>,
}

impl ast::Visitor for DependentDomains {
    fn visit_expression(&mut self, expression: &ast::Expression) -> ControlFlow<()> {
        let ast::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } = expression
        else {
            return ast::walk_expression_default(self, expression);
        };
        // Iterator ranges are evaluated in the enclosing scope, before the new
        // comprehension binders shadow names in its body (MLS §10.3.4.1).
        for index in indices {
            if BinderReferences(&self.binders)
                .visit_expression(&index.range)
                .is_break()
            {
                return ControlFlow::Break(());
            }
            self.visit_expression(&index.range)?;
        }
        let enclosing = self.binders.clone();
        for index in indices {
            self.binders.remove(index.ident.text.as_ref());
        }
        let result = (|| {
            self.visit_expression(expr)?;
            if let Some(filter) = filter {
                self.visit_expression(filter)?;
            }
            ControlFlow::Continue(())
        })();
        self.binders = enclosing;
        result
    }
}

struct BinderReferences<'a>(&'a HashSet<String>);

impl ast::Visitor for BinderReferences<'_> {
    fn visit_component_reference(
        &mut self,
        reference: &ast::ComponentReference,
    ) -> ControlFlow<()> {
        if reference.parts.len() == 1 && self.0.contains(reference.parts[0].ident.text.as_ref()) {
            return ControlFlow::Break(());
        }
        ast::walk_component_reference_default(self, reference)
    }
}
