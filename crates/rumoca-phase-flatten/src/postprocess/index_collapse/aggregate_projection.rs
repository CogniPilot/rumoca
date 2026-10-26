//! MLS §§10.4.4, 10.5, 10.7: field projection distributes over record
//! array construction and concatenation, retaining the field's trailing axes.

use super::KnownFlatVars;
use rumoca_core::{BuiltinFunction, DefId, Expression, Literal, Span};

pub(super) fn project(
    base: &Expression,
    field: &str,
    field_def_id: DefId,
    span: Span,
    known: &KnownFlatVars,
) -> Option<Expression> {
    let project = |element: &Expression| Expression::FieldAccess {
        base: Box::new(element.clone()),
        field: field.to_owned(),
        field_def_id,
        span,
    };
    match base {
        Expression::Array { elements, kind, .. } if !elements.is_empty() => {
            Some(Expression::Array {
                elements: elements.iter().map(project).collect(),
                kind: *kind,
                span,
            })
        }
        Expression::BuiltinCall {
            function: BuiltinFunction::Cat,
            args,
            ..
        } => {
            let (dimension, operands) = args.split_first()?;
            let along_first = matches!(
                dimension,
                Expression::Literal {
                    value: Literal::Integer(1),
                    ..
                }
            );
            let projected = operands
                .iter()
                .filter(|operand| !along_first || !empty_vector_field(operand, field_def_id, known))
                .map(project)
                .collect::<Vec<_>>();
            // An all-empty result needs its full element type and trailing
            // shape. Leave it to the typed owner instead of guessing Real[0].
            if projected.is_empty() {
                return None;
            }
            Some(Expression::BuiltinCall {
                function: BuiltinFunction::Cat,
                args: std::iter::once(dimension.clone())
                    .chain(projected)
                    .collect(),
                span,
            })
        }
        _ => None,
    }
}

fn empty_vector_field(expression: &Expression, field: DefId, known: &KnownFlatVars) -> bool {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return false;
    };
    subscripts.is_empty()
        && name
            .instance_id()
            .is_some_and(|instance| known.empty_record_fields.contains(&(instance, field)))
}
