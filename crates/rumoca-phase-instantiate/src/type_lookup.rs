use crate::path_utils;
use rumoca_core::{DefId, TypeId};
use rumoca_ir_ast as ast;

use super::inheritance;
#[cfg(test)]
use super::is_type_subtype;
use super::type_overrides::resolve_redeclare_value_def_id;
use super::{InstantiateError, InstantiateResult, find_class_in_tree, location_to_span};

/// Type information for a component, resolved from the class tree.
pub(super) struct TypeInfo<'a> {
    pub(super) class_def: Option<&'a ast::ClassDef>,
    pub(super) is_primitive: bool,
    pub(super) is_discrete: bool,
}

/// Map a builtin primitive name to its TypeId in the class tree.
fn builtin_type_id(tree: &ast::ClassTree, name: &str) -> Option<TypeId> {
    let simple = path_utils::class_name_leaf(name);
    match simple {
        "Real" => Some(tree.type_table.real()),
        "Integer" => Some(tree.type_table.integer()),
        "Boolean" => Some(tree.type_table.boolean()),
        "String" => Some(tree.type_table.string()),
        "Clock" => tree.type_table.lookup("Clock"),
        _ => None,
    }
}

/// Resolve MLS predefined types registered in the class-tree type table.
fn predefined_type_id(tree: &ast::ClassTree, name: &str) -> Option<TypeId> {
    tree.type_table.lookup(name)
}

/// MLS §3.8.3: Integer, Boolean, String, and enumeration components are
/// discrete-valued by type, independent of any variability prefix. Only `Real`
/// (and `Clock`, which carries its own clocked semantics) stays continuous.
fn predefined_type_is_discrete(tree: &ast::ClassTree, type_id: TypeId) -> bool {
    matches!(
        tree.type_table.get(type_id),
        Some(ast::Type::Builtin(
            ast::BuiltinType::Integer | ast::BuiltinType::Boolean | ast::BuiltinType::String
        )) | Some(ast::Type::Enumeration(_))
    )
}

/// Resolve the provisional primitive base `TypeId` for an instance component.
///
/// For direct builtins this returns the corresponding TypeId. For type aliases
/// and short class definitions, this follows a single-inheritance chain until
/// a builtin primitive is found. User enumeration `TypeId`s are issued by the
/// typecheck type-context owner, so Instance IR retains their exact `DefId` and
/// leaves this provisional slot unknown. Missing or contradictory inheritance
/// evidence remains an instantiation error.
pub(super) fn resolve_primitive_type_id(
    tree: &ast::ClassTree,
    type_name: &str,
    class_def: Option<&ast::ClassDef>,
) -> InstantiateResult<TypeId> {
    if class_def.is_none()
        && let Some(id) = builtin_type_id(tree, type_name)
    {
        return Ok(id);
    }
    let class = class_def
        .ok_or_else(|| Box::new(InstantiateError::ModelNotFound(type_name.to_string())))?;
    let mut pending = vec![class];
    let mut visited = std::collections::HashSet::new();
    let mut primitive_ids = indexmap::IndexSet::new();
    let mut reaches_enumeration = false;
    while let Some(owner) = pending.pop() {
        let owner_def_id = owner.def_id.ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!("primitive type owner `{}`", owner.name.text),
                owner.location.span(),
            ))
        })?;
        if !visited.insert(owner_def_id) {
            continue;
        }
        if !owner.enum_literals.is_empty() {
            reaches_enumeration = true;
            continue;
        }
        for extend in &owner.extends {
            if let Some(base) = inheritance::predefined_extend_name(tree, extend)? {
                primitive_ids.extend(builtin_type_id(tree, &base));
                continue;
            }
            let base_def_id = extend
                .base_def_id
                .or(extend.base_name.def_id)
                .ok_or_else(|| {
                    Box::new(InstantiateError::missing_resolved_identity(
                        format!("primitive extends edge `{}`", extend.base_name),
                        extend.location.span(),
                    ))
                })?;
            let base = tree.get_class_by_def_id(base_def_id).ok_or_else(|| {
                Box::new(InstantiateError::missing_resolved_identity(
                    format!(
                        "primitive extends edge `{}` ({base_def_id:?})",
                        extend.base_name
                    ),
                    extend.location.span(),
                ))
            })?;
            pending.push(base);
        }
    }
    if reaches_enumeration {
        if !primitive_ids.is_empty() {
            return Err(Box::new(InstantiateError::redeclare_error(
                type_name,
                "primitive classification reaches both enumeration and predefined bases",
                class.location.span(),
            )));
        }
        return Ok(TypeId::UNKNOWN);
    }
    let mut primitive_ids = primitive_ids.into_iter();
    let Some(primitive_id) = primitive_ids.next() else {
        return Err(Box::new(InstantiateError::redeclare_error(
            type_name,
            "primitive classification has no predefined base type",
            class.location.span(),
        )));
    };
    if primitive_ids.any(|candidate| candidate != primitive_id) {
        return Err(Box::new(InstantiateError::redeclare_error(
            type_name,
            "primitive classification reaches contradictory predefined base types",
            class.location.span(),
        )));
    }
    Ok(primitive_id)
}

/// Find a nested type member within a class, following the extends chain.
///
/// When `Medium.AbsolutePressure` resolves to the `Medium` package alias,
/// we need to find `AbsolutePressure` by looking in the package's own classes
/// and then in its base classes (via extends). This handles replaceable packages
/// like `package Medium = PartialMedium` where types are defined in the base.
pub(super) fn find_member_type_in_class<'a>(
    tree: &'a ast::ClassTree,
    class: &'a ast::ClassDef,
    member_name: &str,
) -> InstantiateResult<Option<&'a ast::ClassDef>> {
    let mut hierarchy = Vec::new();
    let mut pending = vec![(class, false)];
    let mut visited = std::collections::HashSet::new();
    while let Some((owner, exiting)) = pending.pop() {
        let owner_def_id = owner.def_id.ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!("member lookup owner `{}`", owner.name.text),
                owner.location.span(),
            ))
        })?;
        if exiting {
            hierarchy.push(owner);
            continue;
        }
        if !visited.insert(owner_def_id) {
            continue;
        }
        pending.push((owner, true));
        for extend in owner.extends.iter().rev() {
            if inheritance::predefined_extend_name(tree, extend)?.is_some() {
                continue;
            }
            let base_def_id = extend
                .base_def_id
                .or(extend.base_name.def_id)
                .ok_or_else(|| {
                    Box::new(InstantiateError::missing_resolved_identity(
                        format!("member lookup extends edge `{}`", extend.base_name),
                        extend.location.span(),
                    ))
                })?;
            let base = tree.get_class_by_def_id(base_def_id).ok_or_else(|| {
                Box::new(InstantiateError::missing_resolved_identity(
                    format!(
                        "member lookup extends edge `{}` ({base_def_id:?})",
                        extend.base_name
                    ),
                    extend.location.span(),
                ))
            })?;
            pending.push((base, false));
        }
    }
    let mut effective = indexmap::IndexMap::<DefId, Option<&ast::ClassDef>>::new();

    for owner in hierarchy {
        let owner_def_id = owner
            .def_id
            .expect("member traversal admits only identity-bearing classes");
        let direct = find_extends_redeclared_member_type(tree, owner, member_name)?
            .or_else(|| owner.classes.get(member_name));
        if let Some(member) = direct {
            require_member_identity(member, member_name)?;
            effective.insert(owner_def_id, Some(member));
            continue;
        }

        let mut inherited = indexmap::IndexMap::<DefId, &ast::ClassDef>::new();
        for extend in &owner.extends {
            if inheritance::predefined_extend_name(tree, extend)?.is_some() {
                continue;
            }
            let base_def_id = extend
                .base_def_id
                .or(extend.base_name.def_id)
                .expect("member traversal admits only exact extends edges");
            if let Some(Some(candidate)) = effective.get(&base_def_id) {
                let candidate_def_id = require_member_identity(candidate, member_name)?;
                inherited.entry(candidate_def_id).or_insert(candidate);
            }
        }
        if inherited.len() > 1 {
            return Err(Box::new(InstantiateError::redeclare_error(
                member_name,
                format!(
                    "inherited member selection is ambiguous across exact identities {:?}",
                    inherited.keys().collect::<Vec<_>>()
                ),
                owner.location.span(),
            )));
        }
        effective.insert(owner_def_id, inherited.into_values().next());
    }

    let root_def_id = class
        .def_id
        .expect("member traversal admits only an identity-bearing root");
    Ok(effective.get(&root_def_id).copied().flatten())
}

fn require_member_identity(member: &ast::ClassDef, member_name: &str) -> InstantiateResult<DefId> {
    member.def_id.ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("nested member `{member_name}`"),
            member.location.span(),
        ))
    })
}

/// Check if inner and outer types are compatible using resolved identity.
///
/// MLS §5.4: The inner declaration's type must be a subtype of the outer's type.
/// DefIds are preferred because relative and qualified spellings should already
/// have been resolved before compatibility checking reaches this path.
pub(super) fn is_type_compatible_with_def_id(
    tree: &ast::ClassTree,
    outer_type: &str,
    outer_def_id: Option<DefId>,
    inner_type: &str,
    inner_def_id: Option<DefId>,
    span: rumoca_core::Span,
) -> InstantiateResult<bool> {
    let outer_id = outer_def_id.ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("outer type `{outer_type}`"),
            span,
        ))
    })?;
    let inner_id = inner_def_id.ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("inner type `{inner_type}`"),
            span,
        ))
    })?;
    inheritance::is_type_subtype_by_def_id(
        tree,
        inner_id,
        outer_id,
        &mut inheritance::SubtypeCache::default(),
    )
}

/// Check if inner type is compatible with outer type (for tests and simple cases).
/// Inner must be a subtype of outer for compatibility.
#[cfg(test)]
pub(super) fn is_type_compatible(
    tree: &ast::ClassTree,
    outer_type: &str,
    inner_type: &str,
) -> InstantiateResult<bool> {
    is_type_subtype(tree, inner_type, outer_type)
}

fn find_extends_redeclared_member_type<'a>(
    tree: &'a ast::ClassTree,
    class: &ast::ClassDef,
    member_name: &str,
) -> InstantiateResult<Option<&'a ast::ClassDef>> {
    let mut selected = indexmap::IndexMap::<DefId, &ast::ClassDef>::new();
    for ext in &class.extends {
        for ext_mod in &ext.modifications {
            if !ext_mod.redeclare {
                continue;
            }
            let ast::Expression::Modification {
                target,
                value: Some(value),
                ..
            } = &ext_mod.expr
            else {
                return Err(Box::new(InstantiateError::redeclare_error(
                    member_name,
                    "extends redeclare has no exact modification target/value shape",
                    ext_mod.expr.span(),
                )));
            };
            let Some(first_target) = target.parts.first() else {
                return Err(Box::new(InstantiateError::redeclare_error(
                    member_name,
                    "extends redeclare has an empty target",
                    ext_mod.expr.span(),
                )));
            };
            if first_target.ident.text.as_ref() != member_name {
                continue;
            }
            let Some(redeclared_def_id) = resolve_redeclare_value_def_id(tree, value, None)? else {
                return Err(Box::new(InstantiateError::redeclare_error(
                    member_name,
                    "extends redeclare value is not a class reference",
                    value.span(),
                )));
            };
            let redeclared_class =
                tree.get_class_by_def_id(redeclared_def_id).ok_or_else(|| {
                    Box::new(InstantiateError::missing_resolved_identity(
                        format!("redeclared nested member `{member_name}` ({redeclared_def_id:?})"),
                        value.span(),
                    ))
                })?;
            selected
                .entry(redeclared_def_id)
                .or_insert(redeclared_class);
        }
    }
    if selected.len() > 1 {
        return Err(Box::new(InstantiateError::redeclare_error(
            member_name,
            format!(
                "multiple redeclarations select conflicting identities {:?}",
                selected.keys().collect::<Vec<_>>()
            ),
            class.location.span(),
        )));
    }
    Ok(selected.into_values().next())
}

/// Look up type information for a component.
/// Uses type_def_id for O(1) lookup when available, falling back to name lookup.
pub(super) fn lookup_type_info<'a>(
    tree: &'a ast::ClassTree,
    comp: &ast::Component,
    type_name: &str,
) -> InstantiateResult<TypeInfo<'a>> {
    let predefined_type_id = predefined_type_id(tree, type_name);
    let mut class_def = comp
        .type_def_id
        .or(comp.type_name.def_id)
        .and_then(|def_id| tree.get_class_by_def_id(def_id))
        .or_else(|| find_class_in_tree(tree, type_name));

    // When a dotted type name like "Medium.AbsolutePressure",
    // "Medium.ThermodynamicState", or "Medium.BaseProperties" resolves to the
    // package ("Medium") rather than the nested member type, drill down into
    // the package member.
    //
    // Components must instantiate the referenced member class, not the
    // containing package. This is required for package-member model components
    // such as `Medium.BaseProperties medium`.
    if let Some(cd) = class_def
        && matches!(cd.class_type, rumoca_core::ClassType::Package)
        && let Some((_, member_name)) = path_utils::class_scope_split(type_name)
        && let Some(member) = find_member_type_in_class(tree, cd, member_name)?
    {
        class_def = Some(member);
    }

    // is_effectively_primitive_transitive follows inheritance chains to detect:
    // - Type aliases to Real/Integer/Boolean/String (primitive)
    // - Records like Complex with components .re/.im (not primitive)
    // - Operator records like SI.ComplexVoltage extending Complex (not primitive)
    if class_def.is_none() && predefined_type_id.is_none() {
        return Err(InstantiateError::type_not_found(
            type_name,
            location_to_span(&comp.location, &tree.source_map, "component type name")?,
        )
        .into());
    }

    let is_primitive = builtin_type_id(tree, type_name).is_some()
        || match class_def {
            Some(class) => inheritance::is_effectively_primitive_transitive(tree, class)?,
            None => false,
        };

    let is_discrete = predefined_type_id
        .is_some_and(|type_id| predefined_type_is_discrete(tree, type_id))
        || inheritance::is_discrete_by_type(tree, type_name, class_def)?;

    Ok(TypeInfo {
        class_def,
        is_primitive,
        is_discrete,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{Location, Token};

    fn class(name: &str, def_id: u32) -> ast::ClassDef {
        ast::ClassDef {
            def_id: Some(DefId::new(def_id)),
            name: Token {
                text: name.into(),
                ..Default::default()
            },
            ..Default::default()
        }
    }

    fn extends(name: &str, def_id: u32) -> ast::Extend {
        ast::Extend {
            base_name: ast::Name::from_string(name),
            base_def_id: Some(DefId::new(def_id)),
            location: Location::default(),
            modifications: Vec::new(),
            break_names: Vec::new(),
            is_protected: false,
            annotation: Vec::new(),
        }
    }

    fn insert_top(tree: &mut ast::ClassTree, name: &str, class: ast::ClassDef) {
        let def_id = class.def_id.expect("test class identity");
        tree.name_map.insert(name.to_string(), def_id);
        tree.def_map.insert(def_id, name.to_string());
        tree.definitions.classes.insert(name.to_string(), class);
    }

    #[test]
    fn member_lookup_distinguishes_same_named_base_classes_by_def_id() {
        let first_foo = class("Foo", 2);
        let mut second_foo = class("Foo", 4);
        second_foo
            .classes
            .insert("Wanted".to_string(), class("Wanted", 5));

        let mut package_a = class("A", 1);
        package_a.classes.insert("Foo".to_string(), first_foo);
        let mut package_b = class("B", 3);
        package_b.classes.insert("Foo".to_string(), second_foo);

        let mut root = class("Root", 6);
        root.extends.push(extends("A.Foo", 2));
        root.extends.push(extends("B.Foo", 4));

        let mut tree = ast::ClassTree::new();
        tree.definitions.classes.insert("A".to_string(), package_a);
        tree.definitions.classes.insert("B".to_string(), package_b);
        tree.definitions.classes.insert("Root".to_string(), root);
        for (id, name) in [
            (1, "A"),
            (2, "A.Foo"),
            (3, "B"),
            (4, "B.Foo"),
            (5, "B.Foo.Wanted"),
            (6, "Root"),
        ] {
            tree.def_map.insert(DefId::new(id), name.to_string());
        }

        let root = tree.get_class_by_def_id(DefId::new(6)).expect("root class");
        let found = find_member_type_in_class(&tree, root, "Wanted")
            .expect("member lookup succeeds")
            .expect("the second same-named base remains searchable");

        assert_eq!(found.def_id, Some(DefId::new(5)));
    }

    #[test]
    fn member_lookup_follows_more_than_the_old_limit() {
        let mut tree = ast::ClassTree::new();
        let mut base = class("Level0", 100);
        base.classes
            .insert("Wanted".to_string(), class("Wanted", 99));
        tree.def_map
            .insert(DefId::new(99), "Level0.Wanted".to_string());
        insert_top(&mut tree, "Level0", base);
        for index in 1..20_u32 {
            let mut owner = class(&format!("Level{index}"), 100 + index);
            owner
                .extends
                .push(extends(&format!("Level{}", index - 1), 99 + index));
            insert_top(&mut tree, &format!("Level{index}"), owner);
        }
        let root = tree
            .get_class_by_def_id(DefId::new(119))
            .expect("long hierarchy root exists");
        let found = find_member_type_in_class(&tree, root, "Wanted")
            .expect("long acyclic hierarchy is valid")
            .expect("inherited member is selected");
        assert_eq!(found.def_id, Some(DefId::new(99)));
    }

    #[test]
    fn member_lookup_rejects_ambiguous_same_spelling_identities() {
        let mut tree = ast::ClassTree::new();
        for (owner_name, owner_id, member_id) in [("A", 200, 201), ("B", 202, 203)] {
            let mut owner = class(owner_name, owner_id);
            owner
                .classes
                .insert("Wanted".to_string(), class("Wanted", member_id));
            tree.def_map
                .insert(DefId::new(member_id), format!("{owner_name}.Wanted"));
            insert_top(&mut tree, owner_name, owner);
        }
        let mut root = class("Root", 204);
        root.extends.push(extends("A", 200));
        root.extends.push(extends("B", 202));
        insert_top(&mut tree, "Root", root);
        let root = tree.get_class_by_def_id(DefId::new(204)).expect("root");
        assert!(find_member_type_in_class(&tree, root, "Wanted").is_err());
    }

    #[test]
    fn member_lookup_rejects_missing_edge() {
        let mut tree = ast::ClassTree::new();
        let mut broken = class("Broken", 302);
        broken.extends.push(extends("Missing", 399));
        insert_top(&mut tree, "Broken", broken);
        let broken = tree.get_class_by_def_id(DefId::new(302)).expect("Broken");
        assert!(find_member_type_in_class(&tree, broken, "Wanted").is_err());
    }

    #[test]
    fn exact_inner_outer_compatibility_accepts_alias_extending_predefined_type() {
        let real_id = DefId::new(600);
        let voltage_id = DefId::new(601);
        let mut tree = ast::ClassTree::new();
        tree.scope_tree
            .add_predefined_member(rumoca_core::ComponentPath::from_flat_path("Real"), real_id);
        let mut voltage = class("Voltage", 601);
        voltage.extends.push(extends("Real", 600));
        insert_top(&mut tree, "Voltage", voltage);

        assert!(
            is_type_compatible_with_def_id(
                &tree,
                "Real",
                Some(real_id),
                "Voltage",
                Some(voltage_id),
                rumoca_core::Span::DUMMY,
            )
            .expect("exact predefined edge is valid")
        );
    }
}
