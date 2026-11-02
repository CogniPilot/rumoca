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
        if class.components.contains_key(&target) || class.classes.contains_key(&target) {
            return Err(Box::new(InstantiateError::redeclare_error(
                format!("{}.{target}", class.name.text),
                "its constraining-clause modifier names an element the class declares \
                 itself, which only a short class definition's modifiers reach yet",
                lower.expr.span(),
            )));
        }
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
        if let Some(read) = enclosing_instance_read(tree, base_class, &lower.expr) {
            return Err(Box::new(InstantiateError::redeclare_error(
                format!("{}.{target}", class.name.text),
                format!(
                    "its constraining-clause modifier reads `{read}` from an enclosing class, \
                     which a short class's modifiers are not read in yet"
                ),
                stated.expr.span(),
            )));
        }
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

/// The first component a kept modifier reads from an instance scope other
/// than its base's. Instantiation reads a short class's modifiers in the
/// base, so such a value would silently change (MLS §7.2: it belongs to the
/// enclosing class). Package constants and classes read the same anywhere.
fn enclosing_instance_read(
    tree: &ast::ClassTree,
    base_class: &ast::ClassDef,
    expr: &ast::Expression,
) -> Option<String> {
    let bases = inheritance_chain(tree, base_class);
    ast::collect_component_refs(expr)
        .into_iter()
        .find_map(|reference| {
            let def_id = reference.root_def_id()?;
            if tree.get_class_by_def_id(def_id).is_some() {
                return None;
            }
            let (owner, _) = path_utils::class_scope_split(tree.def_map.get(&def_id)?)?;
            let owner_class = tree
                .name_map
                .get(owner)
                .and_then(|id| tree.get_class_by_def_id(*id))?;
            let instance_owned = owner_class.class_type != rumoca_core::ClassType::Package;
            (instance_owned && !bases.contains(owner)).then(|| reference.to_string())
        })
}

/// The qualified names of `class` and every class it extends.
fn inheritance_chain(tree: &ast::ClassTree, class: &ast::ClassDef) -> IndexSet<String> {
    let mut chain = IndexSet::new();
    let mut pending = vec![class];
    while let Some(current) = pending.pop() {
        let Some(name) = current.def_id.and_then(|id| tree.def_map.get(&id)) else {
            continue;
        };
        if !chain.insert(name.clone()) {
            continue;
        }
        pending.extend(
            current
                .extends
                .iter()
                .filter_map(|extend| extend.base_def_id)
                .filter_map(|id| tree.get_class_by_def_id(id)),
        );
    }
    chain
}
