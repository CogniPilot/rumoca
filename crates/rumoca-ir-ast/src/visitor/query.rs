use super::Visitor;
use crate::{ComponentReference, Expression};
use rumoca_core::ComponentPath;
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

/// Return the structured component path denoted by a path-shaped expression.
///
/// Instantiation can represent a projected reference as `FieldAccess` and
/// `ArrayIndex` nodes rather than a single `ComponentReference`. Keeping this
/// conversion in the AST crate gives evaluators and semantic phases one
/// definition of the path spelling without re-parsing rendered expressions.
pub fn expression_component_path(expr: &Expression) -> Option<ComponentPath> {
    match expr {
        Expression::ComponentReference(reference) if !reference.parts.is_empty() => Some(
            ComponentPath::from_parts(reference.parts.iter().map(ToString::to_string)),
        ),
        Expression::Parenthesized { inner, .. } => expression_component_path(inner),
        Expression::FieldAccess { base, field, .. } => {
            let base = expression_component_path(base)?;
            Some(base.join(&ComponentPath::from_parts([field.clone()])))
        }
        Expression::ArrayIndex {
            base, subscripts, ..
        } => {
            let base = expression_component_path(base)?;
            let mut parts = base.into_parts();
            let last = parts.last_mut()?;
            let subscripts = subscripts
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(",");
            last.push('[');
            last.push_str(&subscripts);
            last.push(']');
            Some(ComponentPath::from_parts(parts))
        }
        _ => None,
    }
}
