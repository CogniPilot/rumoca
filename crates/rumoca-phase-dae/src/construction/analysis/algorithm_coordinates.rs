//! Resolve materialized component indices before algorithm ownership analysis.
use std::borrow::Cow;

use super::*;
use rumoca_core::{ComponentReference, FallibleExpressionRewriter, FallibleStatementRewriter};

/// The Flat model every analysis and the construction read: carrying
/// algorithm loops unrolled and structural selections applied (SPEC_0040
/// DAE-C22), both from one model-scope shape analysis, with algorithm targets
/// resolved to declared storage coordinates. Borrowed when no change is needed.
pub(in crate::construction) fn prepared_flat(
    flat: &flat::Model,
) -> Result<std::borrow::Cow<'_, flat::Model>, ToDaeError> {
    let carrying = model_algorithm_loops::has_carrying_loops(flat);
    if !carrying && !has_decidable_conditionals(flat) {
        return resolve_algorithm_coordinates(std::borrow::Cow::Borrowed(flat));
    }
    let constants = constant_context(flat)?;
    let evaluable = evaluable_parameters(flat);
    let shapes = FunctionShapeAnalysis::analyze_model(flat, &constants, Some(&evaluable))?;
    let unrolled =
        model_algorithm_loops::unroll_carrying_algorithm_loops(flat, shapes.model_values())?;
    resolve_algorithm_coordinates(select_structural_branches(unrolled, shapes.model_values()))
}

/// All algorithm consumers must see the same storage coordinate. A literal
/// index in a declared component path is part of its identity, not an element
/// update on the declared variable (MLS 11.1.2, 10.5).
fn resolve_algorithm_coordinates(
    mut flat: Cow<'_, flat::Model>,
) -> Result<Cow<'_, flat::Model>, ToDaeError> {
    let mut resolver = Coordinates { flat: &flat };
    let mut updates = Vec::new();
    for (index, algorithm) in flat.algorithms.iter().enumerate() {
        let statements = resolver.rewrite_statements(&algorithm.statements)?;
        if statements != algorithm.statements {
            updates.push((index, statements));
        }
    }
    for (index, statements) in updates {
        let algorithm = &mut flat.to_mut().algorithms[index];
        algorithm.outputs = rumoca_core::extract_algorithm_outputs(&statements);
        algorithm.statements = statements;
    }
    Ok(flat)
}

struct Coordinates<'a> {
    flat: &'a flat::Model,
}

impl FallibleExpressionRewriter for Coordinates<'_> {
    type Error = ToDaeError;

    fn rewrite_expression(&mut self, expression: &Expression) -> Result<Expression, Self::Error> {
        Ok(expression.clone())
    }
}

impl FallibleStatementRewriter for Coordinates<'_> {
    fn rewrite_statement(
        &mut self,
        statement: &rumoca_core::Statement,
    ) -> Result<rumoca_core::Statement, Self::Error> {
        if matches!(statement, rumoca_core::Statement::Reinit { .. }) {
            Ok(statement.clone())
        } else {
            self.walk_statement(statement)
        }
    }

    fn rewrite_reference(
        &mut self,
        reference: &rumoca_core::Reference,
    ) -> Result<rumoca_core::Reference, Self::Error> {
        Ok(reference.clone())
    }

    fn rewrite_component_reference(
        &mut self,
        reference: &ComponentReference,
    ) -> Result<ComponentReference, Self::Error> {
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
            let candidate = replace_parts(reference, parts.clone())?;
            let name = candidate.to_var_name();
            if self.flat.variables.contains_key(&name)
                || self.flat.record_instances.contains_key(&name)
            {
                selected = Some((index, parts.clone()));
            }
        }
        let Some((last, mut parts)) = selected else {
            return Ok(reference.clone());
        };
        for (part, original) in parts.iter_mut().zip(reference.parts()).skip(last + 1) {
            part.subs.clone_from(&original.subs);
        }
        replace_parts(reference, parts)
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

fn replace_parts(
    reference: &ComponentReference,
    parts: Vec<rumoca_core::ComponentRefPart>,
) -> Result<ComponentReference, ToDaeError> {
    reference
        .with_replaced_parts(parts)
        .map_err(|error| ToDaeError::UnsupportedAlgorithm {
            section: "model".to_owned(),
            origin: format!(
                "invalid algorithm target {}: {error}",
                reference.to_var_name()
            ),
            span: reference.span(),
        })
}
