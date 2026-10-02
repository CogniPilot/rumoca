//! Element records an operand of a whole-record equation denotes.
//!
//! MLS 3.7 §10.6.1 defines equality of arrays element-wise, and §10.5.2 makes
//! `a[i].m` of an array of components the array of the elements' members. A
//! record-array equation `s[1:n] = m[1:n].state` over arrays whose elements
//! Flat stores as separate record occurrences (`s[1]`, `m[1].state`, ...)
//! therefore states one whole-record equality per element pair. This module
//! resolves such an operand to its element record occurrences, in order.
//!
//! Every element is proved, not inferred from its rendered name: the record
//! occurrence must exist, the path part that carries the element subscript
//! must be the declaration the operand indexes, with exactly that subscript,
//! and every projected field part must be the projected field declaration.

use super::*;
use rumoca_core::DefId;

/// One element record occurrence of a whole-record equation operand.
pub(super) struct RecordElement<'flat> {
    pub(super) record: &'flat flat::RecordInstance,
    pub(super) name: VarName,
}

/// A rank-1 array of element records, optionally projected through record
/// fields: `base[subscript].f1.f2`.
struct ElementOperand<'scope> {
    base: &'scope rumoca_core::Reference,
    subscript: Option<&'scope Subscript>,
    fields: Vec<(&'scope str, DefId)>,
}

/// The element records `operand` denotes, or `None` when it is not an array of
/// element-expanded records.
pub(super) fn record_array_operand_elements<'flat>(
    flat: &'flat flat::Model,
    operand: &Expression,
    values: &ShapeEnvironment,
) -> Option<Vec<RecordElement<'flat>>> {
    if let Expression::Array { elements, .. } = operand {
        // `{r1, r2}`, as Flat spells the member `m.state` of a component
        // array: each element is one whole record occurrence.
        return elements
            .iter()
            .map(|element| record_occurrence(flat, element))
            .collect();
    }
    let operand = element_operand(operand)?;
    let indices = match operand.subscript {
        None | Some(Subscript::Colon { .. }) => None,
        Some(subscript) => Some(subscript_indices(subscript, values)?),
    };
    let element = |index: i64| element_record(flat, &operand, index);
    match indices {
        Some(indices) => indices.into_iter().map(element).collect(),
        None => {
            let elements = (1..).map_while(element).collect::<Vec<_>>();
            (!elements.is_empty()).then_some(elements)
        }
    }
}

/// A whole record occurrence named by an unsubscripted reference whose
/// identity is the occurrence's own path.
fn record_occurrence<'flat>(
    flat: &'flat flat::Model,
    expression: &Expression,
) -> Option<RecordElement<'flat>> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return None;
    };
    let record = flat.record_instances.get(name.var_name())?;
    let same_path = name.parts().len() == record.component_ref.parts().len()
        && name
            .parts()
            .iter()
            .zip(record.component_ref.parts())
            .all(|(used, declared)| {
                used.def_id == declared.def_id
                    && index_values(&used.subs) == index_values(&declared.subs)
            });
    (subscripts.is_empty() && same_path).then(|| RecordElement {
        record,
        name: name.var_name().clone(),
    })
}

/// The literal indices of a resolved path part, or `None` for any other
/// subscript form.
fn index_values(subscripts: &[Subscript]) -> Option<Vec<i64>> {
    subscripts
        .iter()
        .map(|subscript| match subscript {
            Subscript::Index { value, .. } => Some(*value),
            _ => None,
        })
        .collect()
}

fn element_operand(expression: &Expression) -> Option<ElementOperand<'_>> {
    match expression {
        Expression::VarRef {
            name, subscripts, ..
        } => Some(ElementOperand {
            base: name,
            subscript: single_subscript(subscripts)?,
            fields: Vec::new(),
        }),
        Expression::Index {
            base, subscripts, ..
        } => {
            let Expression::VarRef {
                name,
                subscripts: own,
                ..
            } = base.as_ref()
            else {
                return None;
            };
            if !own.is_empty() || subscripts.len() != 1 {
                return None;
            }
            Some(ElementOperand {
                base: name,
                subscript: subscripts.first(),
                fields: Vec::new(),
            })
        }
        Expression::FieldAccess {
            base,
            field,
            field_def_id,
            ..
        } => {
            let mut operand = element_operand(base)?;
            operand.fields.push((field.as_str(), *field_def_id));
            Some(operand)
        }
        _ => None,
    }
}

/// `None` for no subscript, `Some(Some(s))` for exactly one, and `None` overall
/// for a higher-rank selection.
fn single_subscript(subscripts: &[Subscript]) -> Option<Option<&Subscript>> {
    match subscripts {
        [] => Some(None),
        [subscript] => Some(Some(subscript)),
        _ => None,
    }
}

/// The element indices one rank-1 subscript other than a colon selects.
fn subscript_indices(subscript: &Subscript, values: &ShapeEnvironment) -> Option<Vec<i64>> {
    match subscript {
        Subscript::Index { value, .. } => Some(vec![*value]),
        Subscript::Colon { .. } => None,
        Subscript::Expr { expr, .. } => match expr.as_ref() {
            Expression::Range {
                start, step, end, ..
            } => {
                let start = values.proven_extent(start)?;
                let step = match step {
                    Some(step) => values.proven_extent(step)?,
                    None => 1,
                };
                let end = values.proven_extent(end)?;
                range_indices(start, step, end)
            }
            expression => Some(vec![values.proven_extent(expression)?]),
        },
    }
}

fn range_indices(start: i64, step: i64, end: i64) -> Option<Vec<i64>> {
    if step == 0 {
        return None;
    }
    let mut indices = Vec::new();
    let mut index = start;
    while (step > 0 && index <= end) || (step < 0 && index >= end) {
        indices.push(index);
        index = index.checked_add(step)?;
    }
    Some(indices)
}

fn element_record<'flat>(
    flat: &'flat flat::Model,
    operand: &ElementOperand<'_>,
    index: i64,
) -> Option<RecordElement<'flat>> {
    let mut rendered = format!("{}[{index}]", operand.base.as_str());
    for (field, _) in &operand.fields {
        rendered.push('.');
        rendered.push_str(field);
    }
    let name = VarName::new(rendered);
    let record = flat.record_instances.get(&name)?;
    let parts = record.component_ref.parts();
    let element_position = parts.len().checked_sub(operand.fields.len() + 1)?;
    let element = &parts[element_position];
    let indexed_exactly = matches!(
        element.subs.as_slice(),
        [Subscript::Index { value, .. }] if *value == index
    );
    let fields_exact = parts[element_position + 1..]
        .iter()
        .zip(&operand.fields)
        .all(|(part, (_, field))| part.def_id == *field && part.subs.is_empty());
    (operand.base.target_def_id() == Some(element.def_id) && indexed_exactly && fields_exact)
        .then_some(RecordElement { record, name })
}
