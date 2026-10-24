//! Resolve materialized component indices before algorithm ownership analysis.
use std::borrow::Cow;

use super::*;
use rumoca_core::{ComponentReference, ExpressionRewriter, StatementRewriter};

/// All algorithm consumers must see the same storage coordinate. A literal
/// index in a declared component path is part of its identity, not an element
/// update on the declared variable (MLS 11.1.2, 10.5).
pub(super) fn resolve_algorithm_coordinates(
    mut flat: Cow<'_, flat::Model>,
) -> Cow<'_, flat::Model> {
    let mut resolver = Coordinates { flat: &flat };
    let updates = flat
        .algorithms
        .iter()
        .enumerate()
        .filter_map(|(index, algorithm)| {
            let statements = resolver.rewrite_statements(&algorithm.statements);
            (statements != algorithm.statements).then_some((index, statements))
        })
        .collect::<Vec<_>>();
    for (index, statements) in updates {
        let algorithm = &mut flat.to_mut().algorithms[index];
        algorithm.outputs = rumoca_core::extract_algorithm_outputs(&statements);
        algorithm.statements = statements;
    }
    flat
}

struct Coordinates<'a> {
    flat: &'a flat::Model,
}

impl ExpressionRewriter for Coordinates<'_> {
    fn rewrite_expression(&mut self, expression: &Expression) -> Expression {
        expression.clone()
    }
}

impl StatementRewriter for Coordinates<'_> {
    fn rewrite_statement(&mut self, statement: &rumoca_core::Statement) -> rumoca_core::Statement {
        if matches!(statement, rumoca_core::Statement::Reinit { .. }) {
            statement.clone()
        } else {
            self.walk_statement(statement)
        }
    }

    fn rewrite_reference(&mut self, reference: &rumoca_core::Reference) -> rumoca_core::Reference {
        reference.clone()
    }

    fn rewrite_component_reference(
        &mut self,
        reference: &ComponentReference,
    ) -> ComponentReference {
        let mut parts = reference.parts().to_vec();
        // Probe declared storage names with progressively materialized prefixes.
        // Only the matching prefix is consumed; later subscripts still index
        // the variable value and are lowered by the existing array-update path.
        for part in &mut parts {
            part.subs.clear();
        }
        let mut selected = None;
        for (index, original) in reference.parts().iter().enumerate() {
            if original.subs.is_empty() {
                continue;
            }
            let Some(indices) = original
                .subs
                .iter()
                .map(literal_index)
                .collect::<Option<Vec<_>>>()
            else {
                break;
            };
            parts[index].ident.push('[');
            parts[index].ident.push_str(&indices.join(","));
            parts[index].ident.push(']');
            let candidate =
                ComponentReference::construct(reference.local(), reference.span(), parts.clone())
                    .expect("materializing indices preserves reference identities");
            let name = candidate.to_var_name();
            if self.flat.variables.contains_key(&name)
                || self.flat.record_instances.contains_key(&name)
            {
                selected = Some((index, parts.clone()));
            }
        }
        let Some((last, mut parts)) = selected else {
            return reference.clone();
        };
        for (part, original) in parts.iter_mut().zip(reference.parts()).skip(last + 1) {
            part.subs.clone_from(&original.subs);
        }
        ComponentReference::construct(reference.local(), reference.span(), parts)
            .expect("materializing indices preserves reference identities")
    }
}

fn literal_index(subscript: &Subscript) -> Option<String> {
    match subscript {
        Subscript::Index { value, .. } => Some(value.to_string()),
        Subscript::Expr { expr, .. } => match expr.as_ref() {
            Expression::Literal {
                value: rumoca_core::Literal::Integer(value),
                ..
            } => Some(value.to_string()),
            _ => None,
        },
        Subscript::Colon { .. } => None,
    }
}
