//! MLS §7.3.2: constraining-clause modifiers merged below the modifiers a
//! declaration or a short class states, by the types of the bound elements.
use super::*;

/// MLS §7.3.2: a replaceable class's constraining-clause modifiers that
/// parsing kept apart, merged below its one base's modifiers now that the
/// base's member types decide how; `None` when it has none. A merge the
/// types cannot settle, or one the class makes over a `final` modifier, is
/// refused here, where the class is used.
pub(super) fn constrained_extend(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
) -> InstantiateResult<Option<ast::Extend>> {
    if class.constraining_modifications.is_empty() {
        return Ok(None);
    }
    let [extend] = class.extends.as_slice() else {
        let span = location_to_span(&class.location, &tree.source_map, "constraining clause")?;
        return Err(Box::new(InstantiateError::redeclare_error(
            class.name.text.to_string(),
            "constraining-clause modifiers of a replaceable class need a short class definition",
            span,
        )));
    };
    let base_class = resolve_base_class(tree, extend)?;
    let mut extend = extend.clone();
    for lower in &class.constraining_modifications {
        let Some(target) = lower.expr.modification_target().map(ToString::to_string) else {
            continue;
        };
        let stated = extend.modifications.iter_mut().find(|modification| {
            modification
                .expr
                .modification_target()
                .map(ToString::to_string)
                .as_ref()
                == Some(&target)
        });
        let Some(stated) = stated else {
            extend.modifications.push(lower.clone());
            continue;
        };
        let refused = if lower.final_ {
            Some(ast::MergeRefusal::Final(target.clone()))
        } else {
            merge_constrained(
                tree,
                Some(base_class),
                &target,
                &mut stated.expr,
                &lower.expr,
            )
        };
        refuse_constrained(class.name.text.as_ref(), refused, &stated.expr)?;
    }
    Ok(Some(extend))
}

/// The error for a refused constraining-clause merge into `owner`.
pub(super) fn refuse_constrained(
    owner: &str,
    refused: Option<ast::MergeRefusal>,
    stated: &ast::Expression,
) -> InstantiateResult<()> {
    match refused {
        None => Ok(()),
        Some(ast::MergeRefusal::Final(name)) => Err(crate::attributes::final_override_error(
            format!("{owner}.{name}"),
            stated,
        )),
        Some(ast::MergeRefusal::Unclassified(name)) => {
            Err(Box::new(InstantiateError::redeclare_error(
                format!("{owner}.{name}"),
                "a declared binding meets constraining-clause modifiers whose element type is not resolved",
                stated.span(),
            )))
        }
    }
}

/// Merge the constraining clause's `value` for `target` below the stated one,
/// classifying bound elements by their type in `class`.
pub(super) fn merge_constrained(
    tree: &ast::ClassTree,
    class: Option<&ast::ClassDef>,
    target: &str,
    stated: &mut ast::Expression,
    value: &ast::Expression,
) -> Option<ast::MergeRefusal> {
    let classify = |path: &[String]| {
        let path: Vec<_> = std::iter::once(target.to_string())
            .chain(path.iter().cloned())
            .collect();
        nested_arguments(tree, class, &path)
    };
    stated
        .merge_modification_below(value, &classify)
        .err()
        .map(|refusal| refusal.within(target))
}

/// Whether the element at `path` below `class` takes attributes (a primitive
/// type) or elements (MLS 4.9); `None` when a type on the way is unresolved.
fn nested_arguments(
    tree: &ast::ClassTree,
    class: Option<&ast::ClassDef>,
    path: &[String],
) -> Option<ast::NestedArguments> {
    let (last, parents) = path.split_last()?;
    let mut class = class?;
    for name in parents {
        let member = get_effective_components(tree, class)
            .ok()?
            .get(name)?
            .clone();
        class = tree.get_class_by_def_id(member.type_def_id?)?;
    }
    let member = get_effective_components(tree, class)
        .ok()?
        .get(last)?
        .clone();
    if is_builtin_type(&member.type_name.to_string()) {
        return Some(ast::NestedArguments::Attributes);
    }
    let class = tree.get_class_by_def_id(member.type_def_id?)?;
    Some(if is_effectively_primitive_transitive(tree, class) {
        ast::NestedArguments::Attributes
    } else {
        ast::NestedArguments::Elements
    })
}
