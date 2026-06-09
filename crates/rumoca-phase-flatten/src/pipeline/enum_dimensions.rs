use super::*;

pub(super) fn enum_type_dimension(expr: &ast::Expression, tree: &ClassTree) -> Option<i64> {
    let ast::Expression::ComponentReference(reference) = expr else {
        return None;
    };
    enum_literal_count_for_reference(reference, tree).and_then(|count| i64::try_from(count).ok())
}

pub(super) fn infer_enum_range_dimensions(expr: &Expression, tree: &ClassTree) -> Option<Vec<i64>> {
    let Expression::Range {
        start, step, end, ..
    } = expr
    else {
        return None;
    };
    if step.is_some() {
        return None;
    }
    let (start_type, start_ordinal) = enum_literal_ordinal(start, tree)?;
    let (end_type, end_ordinal) = enum_literal_ordinal(end, tree)?;
    if start_type != end_type {
        return None;
    }
    let len = if end_ordinal >= start_ordinal {
        end_ordinal - start_ordinal + 1
    } else {
        0
    };
    Some(vec![len])
}

fn enum_literal_ordinal(expr: &Expression, tree: &ClassTree) -> Option<(rumoca_core::DefId, i64)> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let reference = name.component_ref()?;
    let literal = reference.parts.last()?.ident.as_str();
    let enum_class = enum_class_for_literal_reference(reference, tree)?;
    let enum_def_id = enum_class.def_id?;
    let ordinal = enum_class
        .enum_literals
        .iter()
        .position(|candidate| candidate.ident.text.as_ref() == literal)? as i64
        + 1;
    Some((enum_def_id, ordinal))
}

fn enum_class_for_literal_reference<'a>(
    reference: &rumoca_core::ComponentReference,
    tree: &'a ClassTree,
) -> Option<&'a ast::ClassDef> {
    if reference.parts.len() < 2 {
        return None;
    }
    let first_def_id = reference.def_id?;
    let mut class = tree.get_class_by_def_id(first_def_id)?;
    if !class.enum_literals.is_empty() {
        return Some(class);
    }
    for part in &reference.parts[1..reference.parts.len() - 1] {
        class = class.classes.get(part.ident.as_str())?;
    }
    (!class.enum_literals.is_empty()).then_some(class)
}

fn enum_literal_count_for_reference(
    reference: &ast::ComponentReference,
    tree: &ClassTree,
) -> Option<usize> {
    let def_id = reference.def_id?;
    tree.get_class_by_def_id(def_id)
        .map(|class| class.enum_literals.len())
        .filter(|count| *count > 0)
}
