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

/// Whether a modification expression contains a redeclaration at any depth
/// (MLS §7.3).
///
/// A redeclaration is not always the outermost modification: both
/// `extends Wrap(h(redeclare C a[2]))` and `Wrap w(h(redeclare C a[2]))` carry
/// it one level down, where the flag lives in the enclosing
/// `ClassModification`'s `redeclare_flags` rather than on the modification that
/// callers see first. Any consumer that must know "was a redeclaration involved
/// here" has to look through the whole modification subtree.
pub(super) fn expression_contains_redeclare(expr: &ast::Expression) -> bool {
    match expr {
        ast::Expression::ClassModification {
            modifications,
            redeclare_flags,
            ..
        } => {
            redeclare_flags.iter().any(|redeclare| *redeclare)
                || modifications.iter().any(expression_contains_redeclare)
        }
        ast::Expression::Modification { value, .. } => expression_contains_redeclare(value),
        _ => false,
    }
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
