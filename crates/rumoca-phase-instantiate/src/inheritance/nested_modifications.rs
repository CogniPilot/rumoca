//! Nested extends arguments retained at their inherited component boundary.
use super::*;

/// Merge nested class modifications from extends clause into inherited components.
///
/// MLS §7.2: When an extends clause has modifications like
/// `extends Foo(friction(useHeatPort=true))`, the nested modifications should be
/// merged into the inherited `friction` component's `modifications` map. This ensures
/// that when `friction` is later instantiated, the modification `useHeatPort=true`
/// is visible via `shift_modifications_down` and `populate_modification_environment`.
pub(super) fn merge_nested_extends_modifications(
    target: &mut InheritedContent,
    extend: &ast::Extend,
) {
    walk_extend_modifications(extend, |modification| {
        // Extract target name and nested modifications from the expression.
        // Three formats exist:
        //   1. ClassModification { target: comp_name, modifications: [...] }
        //      For: extends Foo(friction(useHeatPort=true))
        //   2. Modification { target: comp_name, value: ClassModification { target: TypeName, modifications: [...] } }
        //      For: extends Foo(redeclare final NewType comp(nested=val))
        //      Type changes are handled by collect_redeclarations(); here we merge nested mods.
        //   3. Binary { Assign, lhs: ClassModification { target: comp_name, .. }, rhs: binding }
        //      For: extends Foo(k(min=0) = 5); the binding is a value modification.
        let Some((target_name, nested)) = extend_nested_target_modifications(extend, modification)
        else {
            return;
        };
        // `a.b(c = 1)` modifies `b` of the inherited `a`, also in `extends P.a(a.b(c = 1))`.
        let Some(reference) = modification.expr.modification_target() else {
            return;
        };
        let path = reference.parts.iter().map(|part| part.ident.text.as_ref());
        let target_name = match path.clone().next() {
            Some(first) if target.components.contains_key(first) => first.to_string(),
            _ => target_name,
        };
        let nested_prefix: String = path
            .skip_while(|part| *part != target_name)
            .skip(1)
            .map(|part| format!("{part}."))
            .collect();
        let Some(comp) = target.components.get_mut(&target_name) else {
            return;
        };
        let ast::Expression::ClassModification {
            modifications,
            each_flags,
            final_flags,
            redeclare_flags,
            ..
        } = nested
        else {
            return;
        };
        if modifications.iter().any(expression_contains_redeclare)
            || enclosing_component_of_nested_redeclare(modification).is_some()
        {
            comp.has_unapplied_redeclare = true;
        }
        for (index, nested_mod) in modifications.iter().enumerate() {
            let each = modification.each || each_flags.get(index).copied().unwrap_or(false);
            let final_ = modification.final_ || final_flags.get(index).copied().unwrap_or(false);
            insert_nested_modification(comp, &nested_prefix, nested_mod, each, final_);
            if nested_prefix.is_empty() && redeclare_flags.get(index).copied().unwrap_or(false) {
                preserve_nested_source_argument(
                    comp,
                    nested_mod,
                    each,
                    final_,
                    redeclare_flags.get(index).copied().unwrap_or(false),
                );
            }
        }
    });
}

fn extend_nested_target_modifications<'a>(
    extend: &ast::Extend,
    modification: &'a ast::ExtendModification,
) -> Option<(String, &'a ast::Expression)> {
    match &modification.expr {
        ast::Expression::ClassModification { target, .. } => Some((
            extend_relative_component_target(extend, target)?,
            &modification.expr,
        )),
        ast::Expression::Modification { target, value, .. }
            if matches!(value.as_ref(), ast::Expression::ClassModification { .. }) =>
        {
            Some((extend_relative_component_target(extend, target)?, value))
        }
        // `x(...) = e`: the nested part; the binding is a value modification.
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Assign,
            lhs,
            ..
        } if matches!(lhs.as_ref(), ast::Expression::ClassModification { .. }) => Some((
            extend_relative_component_target(extend, modification.expr.modification_target()?)?,
            lhs,
        )),
        _ => None,
    }
}

/// Preserve nested modifier values and their MLS §7.2.5/7.2.6 prefixes together.
fn insert_nested_modification(
    comp: &mut ast::Component,
    nested_prefix: &str,
    nested_mod: &ast::Expression,
    each: bool,
    final_: bool,
) {
    let (name, value) = match nested_mod {
        ast::Expression::Modification { target, value, .. } => {
            let Some(part) = target.parts.first() else {
                return;
            };
            (part.ident.text.to_string(), value.as_ref())
        }
        ast::Expression::NamedArgument { name, value, .. } => {
            (name.text.to_string(), value.as_ref())
        }
        ast::Expression::ClassModification { .. } => {
            let Some(name) = extract_modification_target(nested_mod) else {
                return;
            };
            (name, nested_mod)
        }
        _ => return,
    };
    let name = format!("{nested_prefix}{name}");
    if each {
        comp.each_modifications.insert(name.clone());
    } else {
        comp.each_modifications.shift_remove(&name);
    }
    if final_ {
        comp.final_attributes.insert(name.clone());
    }
    comp.modifications.insert(name, value.clone());
}

/// Keep the source argument and its prefixes at the same component boundary
/// as the normalized value. Replacement selection consumes the resolved source
/// reference, not the flattened value alone (MLS §7.2/§7.3).
fn preserve_nested_source_argument(
    comp: &mut ast::Component,
    argument: &ast::Expression,
    each: bool,
    final_: bool,
    redeclare: bool,
) {
    let target = argument.modification_target().map(ToString::to_string);
    let existing = target.as_ref().and_then(|target| {
        comp.source_modifications.iter().position(|previous| {
            previous
                .modification_target()
                .map(ToString::to_string)
                .as_ref()
                == Some(target)
        })
    });
    let count = comp.source_modifications.len();
    let index = existing.unwrap_or(count);
    if index == count {
        comp.source_modifications.push(argument.clone());
    } else {
        comp.source_modifications[index] = argument.clone();
    }
    for (flags, value) in [
        (&mut comp.source_modification_each_flags, each),
        (&mut comp.source_modification_final_flags, final_),
        (&mut comp.source_modification_redeclare_flags, redeclare),
    ] {
        flags.resize(comp.source_modifications.len(), false);
        flags[index] = value;
    }
}
