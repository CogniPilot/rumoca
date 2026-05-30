use super::Visitor;
use crate::{ComponentReference, Expression};
use std::ops::ControlFlow::{self, Break, Continue};

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
