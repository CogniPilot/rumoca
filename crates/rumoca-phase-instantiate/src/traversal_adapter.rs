use rumoca_ir_ast as ast;

/// Walk direct nested classes declared in `class`.
pub(super) fn walk_nested_classes<'a>(
    class: &'a ast::ClassDef,
    mut callback: impl FnMut(&'a str, &'a ast::ClassDef),
) {
    for (name, nested) in &class.classes {
        callback(name, nested);
    }
}

/// Walk direct `extends` clauses declared in `class`.
pub(super) fn walk_extends<'a>(
    class: &'a ast::ClassDef,
    mut callback: impl FnMut(&'a ast::Extend),
) {
    for extend in &class.extends {
        callback(extend);
    }
}

/// Walk all modifications in an `extends` clause.
pub(super) fn walk_extend_modifications<'a>(
    extend: &'a ast::Extend,
    mut callback: impl FnMut(&'a ast::ExtendModification),
) {
    for modification in &extend.modifications {
        callback(modification);
    }
}

/// Walk all `extends` modifications in a class.
pub(super) fn walk_class_extends_modifications<'a>(
    class: &'a ast::ClassDef,
    mut callback: impl FnMut(&'a ast::Extend, &'a ast::ExtendModification),
) {
    walk_extends(class, |extend| {
        walk_extend_modifications(extend, |modification| callback(extend, modification));
    });
}

/// Parse a redeclare-value modification:
/// `redeclare ... target = value`.
pub(super) fn redeclare_target_value(
    modification: &ast::ExtendModification,
) -> Option<(&str, &ast::Expression)> {
    if !modification.redeclare {
        return None;
    }
    let ast::Expression::Modification { target, value, .. } = &modification.expr else {
        return None;
    };
    let first_target = target.parts.first()?;
    Some((first_target.ident.text.as_ref(), value.as_ref()))
}
