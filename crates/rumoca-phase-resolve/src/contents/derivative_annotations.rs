use super::*;
use std::sync::Arc;

impl Resolver {
    pub(super) fn resolve_derivative_annotations(
        &mut self,
        annotations: &mut [Expression],
        scope: ScopeId,
    ) {
        for annotation in annotations {
            self.resolve_derivative_annotation(annotation, scope);
        }
    }

    fn resolve_derivative_annotation(&mut self, annotation: &mut Expression, scope: ScopeId) {
        match annotation {
            Expression::NamedArgument { name, value, .. } if name.text.as_ref() == "derivative" => {
                self.resolve_derivative_target(Arc::make_mut(value), scope);
            }
            Expression::Modification { target, value, .. }
                if annotation_name(target) == Some("derivative") =>
            {
                self.resolve_derivative_target(Arc::make_mut(value), scope);
                self.resolve_derivative_subscripts(target, scope);
            }
            Expression::Binary {
                op: rumoca_core::OpBinary::Assign,
                lhs,
                rhs,
                ..
            } => {
                if let Expression::ClassModification {
                    target,
                    modifications,
                    ..
                } = Arc::make_mut(lhs)
                    && annotation_name(target) == Some("derivative")
                {
                    self.resolve_derivative_target(Arc::make_mut(rhs), scope);
                    self.resolve_derivative_restrictions(modifications, scope);
                }
            }
            Expression::ClassModification {
                target,
                modifications,
                ..
            } if annotation_name(target) == Some("derivative") => {
                for modification in modifications.iter_mut() {
                    self.resolve_derivative_target(modification, scope);
                }
                self.resolve_derivative_restrictions(modifications, scope);
            }
            _ => {}
        }
    }

    fn resolve_derivative_target(&mut self, expression: &mut Expression, scope: ScopeId) {
        if let Expression::ComponentReference(reference) = expression {
            self.resolve_function_reference(reference, scope);
        }
    }

    fn resolve_derivative_subscripts(&mut self, target: &mut ComponentReference, scope: ScopeId) {
        let Some(subscripts) = target.parts[0].subs.as_mut() else {
            return;
        };
        for subscript in subscripts {
            if let ast::Subscript::Expression(expression) = subscript {
                self.resolve_derivative_restriction(expression, scope);
            }
        }
    }

    fn resolve_derivative_restrictions(&mut self, restrictions: &mut [Expression], scope: ScopeId) {
        for restriction in restrictions {
            self.resolve_derivative_restriction(restriction, scope);
        }
    }

    fn resolve_derivative_restriction(&mut self, restriction: &mut Expression, scope: ScopeId) {
        let (name, value) = match restriction {
            Expression::Modification { target, value, .. } => (annotation_name(target), value),
            Expression::NamedArgument { name, value, .. } => (Some(name.text.as_ref()), value),
            _ => return,
        };
        if matches!(name, Some("noDerivative" | "zeroDerivative")) {
            self.resolve_expression(Arc::make_mut(value), scope);
        }
    }
}

fn annotation_name(reference: &ComponentReference) -> Option<&str> {
    let [part] = reference.parts.as_slice() else {
        return None;
    };
    Some(part.ident.text.as_ref())
}
