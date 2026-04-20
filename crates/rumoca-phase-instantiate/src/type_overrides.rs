use super::find_class_in_tree;
use super::traversal_adapter::{
    redeclare_target_value, walk_class_extends_modifications, walk_nested_classes,
};
use super::type_lookup::{find_member_type_in_class, find_member_type_path_in_class};
use crate::{InstantiateError, InstantiateResult, location_to_span};
use indexmap::IndexMap;
use rumoca_core::DefId;
use rumoca_ir_ast as ast;

/// Build a type override map for replaceable type redeclarations (MLS §7.3).
///
/// When a class redeclares a replaceable type (e.g.,
/// `redeclare record extends ThermodynamicState`), inherited components
/// referencing the original type should use the redeclared version.
///
/// This collects type name -> DefId mappings from:
/// 1. The class's own nested classes (redeclared types in this class)
/// 2. The enclosing class's nested classes (sibling type redeclarations)
///
/// Returns a map from unqualified type name to the DefId of the local version.
pub(super) fn build_type_override_map(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    mod_env: Option<&ast::ModificationEnvironment>,
) -> IndexMap<String, DefId> {
    let mut overrides = IndexMap::new();

    // 1. Collect from the class's own nested classes
    walk_nested_classes(class, |name, nested| {
        if let Some(def_id) = nested.def_id {
            overrides.insert(name.to_string(), def_id);
        }
    });

    // 2. Collect from the enclosing class's nested classes.
    // This handles the pattern where a record type (like ThermodynamicState)
    // is redeclared in the enclosing package, and components in the model
    // reference it by its short name.
    collect_enclosing_type_overrides(tree, class, mod_env, &mut overrides);

    // 3. Collect package/type redeclarations from extends-modifications
    // (e.g., extends Base(redeclare replaceable package Medium = ...)).
    collect_extends_redeclare_overrides(tree, class, mod_env, &mut overrides);

    // 4. Active component/class modifiers are the most specific context while
    // instantiating a component. MLS §7.2/§7.3 require a forwarding redeclare
    // such as `redeclare package Medium = Medium` to see the enclosing active
    // replacement instead of the replaceable declaration's static default.
    collect_active_redeclare_overrides(tree, mod_env, &mut overrides);

    overrides
}

fn collect_active_redeclare_overrides(
    tree: &ast::ClassTree,
    mod_env: Option<&ast::ModificationEnvironment>,
    overrides: &mut IndexMap<String, DefId>,
) {
    let Some(mod_env) = mod_env else {
        return;
    };
    for (key, value) in &mod_env.active {
        if key.parts.len() != 1 {
            continue;
        }
        let Some(name) = key.first_name() else {
            continue;
        };
        if let Some(def_id) = resolve_redeclare_value_def_id(tree, &value.value, Some(mod_env)) {
            overrides.insert(name.to_string(), def_id);
        }
    }
}

/// Collect type overrides from the enclosing class's nested classes.
///
/// Helper for [`build_type_override_map`] to reduce nesting depth.
fn collect_enclosing_type_overrides(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    mod_env: Option<&ast::ModificationEnvironment>,
    overrides: &mut IndexMap<String, DefId>,
) {
    let Some(class_def_id) = class.def_id else {
        return;
    };
    let Some(qualified_name) = tree.def_map.get(&class_def_id) else {
        return;
    };
    let Some(dot_pos) = qualified_name.rfind('.') else {
        return;
    };
    let parent_name = &qualified_name[..dot_pos];
    let Some(parent_class) = tree.get_class_by_qualified_name(parent_name) else {
        return;
    };
    collect_nested_overrides_in_extends_chain(tree, parent_class, mod_env, overrides);
}

/// Collect nested class overrides from a class and all of its base classes.
///
/// MLS §7.3 redeclarations are inherited through extends-chains, so a derived
/// package can provide the effective type used by descendant models even when
/// the redeclare is not declared directly in the immediate parent package.
fn collect_nested_overrides_in_extends_chain(
    tree: &ast::ClassTree,
    root: &ast::ClassDef,
    mod_env: Option<&ast::ModificationEnvironment>,
    overrides: &mut IndexMap<String, DefId>,
) {
    const MAX_DEPTH: usize = 32;

    let mut to_visit = vec![root];
    let mut visited_def_ids = std::collections::HashSet::<DefId>::new();
    let mut visited_names = std::collections::HashSet::<String>::new();

    for _ in 0..MAX_DEPTH {
        if to_visit.is_empty() {
            break;
        }

        let mut next = Vec::new();
        for class in to_visit.drain(..) {
            if is_visited_class(class, &mut visited_def_ids, &mut visited_names) {
                continue;
            }

            insert_nested_class_overrides(class, overrides);
            insert_extends_redeclare_overrides(tree, class, mod_env, overrides);
            next.extend(extends_base_classes(tree, class));
        }
        to_visit = next;
    }
}

fn insert_extends_redeclare_overrides(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    mod_env: Option<&ast::ModificationEnvironment>,
    overrides: &mut IndexMap<String, DefId>,
) {
    walk_class_extends_modifications(class, |_, ext_mod| {
        let Some((target_name, value_expr)) = redeclare_target_value(ext_mod) else {
            return;
        };
        let Some(def_id) = resolve_redeclare_value_def_id(tree, value_expr, mod_env) else {
            return;
        };
        overrides.entry(target_name.to_string()).or_insert(def_id);
    });
}

fn is_visited_class(
    class: &ast::ClassDef,
    visited_def_ids: &mut std::collections::HashSet<DefId>,
    visited_names: &mut std::collections::HashSet<String>,
) -> bool {
    match class.def_id {
        Some(def_id) => !visited_def_ids.insert(def_id),
        None => !visited_names.insert(class.name.text.to_string()),
    }
}

fn insert_nested_class_overrides(class: &ast::ClassDef, overrides: &mut IndexMap<String, DefId>) {
    walk_nested_classes(class, |name, nested| {
        if let Some(def_id) = nested.def_id {
            // Keep nearest declaration if names repeat in deeper bases.
            overrides.entry(name.to_string()).or_insert(def_id);
        }
    });
}

fn extends_base_classes<'a>(
    tree: &'a ast::ClassTree,
    class: &'a ast::ClassDef,
) -> Vec<&'a ast::ClassDef> {
    class
        .extends
        .iter()
        .filter_map(|ext| {
            let base_name = ext.base_name.to_string();
            ext.base_def_id
                .and_then(|def_id| tree.get_class_by_def_id(def_id))
                .or_else(|| find_class_in_tree(tree, &base_name))
        })
        .collect()
}

/// Collect redeclared type/package overrides from extends clause modifications.
///
/// MLS §7.3: A redeclare in an extends-modification overrides inherited replaceable
/// declarations in the derived class context.
fn collect_extends_redeclare_overrides(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    mod_env: Option<&ast::ModificationEnvironment>,
    overrides: &mut IndexMap<String, DefId>,
) {
    walk_class_extends_modifications(class, |_, ext_mod| {
        let Some((target_name, value_expr)) = redeclare_target_value(ext_mod) else {
            return;
        };
        if let Some(def_id) = resolve_redeclare_value_def_id(tree, value_expr, mod_env) {
            overrides.insert(target_name.to_string(), def_id);
        }
    });
}

pub(super) fn resolve_redeclare_value_def_id(
    tree: &ast::ClassTree,
    value: &ast::Expression,
    mod_env: Option<&ast::ModificationEnvironment>,
) -> Option<DefId> {
    resolve_redeclare_value_def_id_with_depth(tree, value, mod_env, 0)
}

fn resolve_redeclare_value_def_id_with_depth(
    tree: &ast::ClassTree,
    value: &ast::Expression,
    mod_env: Option<&ast::ModificationEnvironment>,
    depth: usize,
) -> Option<DefId> {
    const MAX_REDECLARE_RESOLVE_DEPTH: usize = 8;
    if depth > MAX_REDECLARE_RESOLVE_DEPTH {
        return None;
    }

    match value {
        ast::Expression::ClassModification { target, .. } => {
            resolve_cref_via_mod_env(tree, target, mod_env, depth)
                .or_else(|| resolve_cref_def_id(tree, target))
        }
        ast::Expression::ComponentReference(cref) => {
            resolve_cref_via_mod_env(tree, cref, mod_env, depth)
                .or_else(|| resolve_cref_def_id(tree, cref))
        }
        _ => None,
    }
}

fn resolve_cref_via_mod_env(
    tree: &ast::ClassTree,
    cref: &ast::ComponentReference,
    mod_env: Option<&ast::ModificationEnvironment>,
    depth: usize,
) -> Option<DefId> {
    let mod_env = mod_env?;
    let qn = cref_to_qualified_name(cref)?;
    let mod_value = mod_env.get(&qn).or_else(|| {
        cref.parts
            .last()
            .map(|part| ast::QualifiedName::from_ident(part.ident.text.as_ref()))
            .and_then(|last_qn| mod_env.get(&last_qn))
    })?;
    if modifier_value_targets_cref(&mod_value.value, cref) {
        return None;
    }
    resolve_redeclare_value_def_id_with_depth(tree, &mod_value.value, Some(mod_env), depth + 1)
}

fn modifier_value_targets_cref(value: &ast::Expression, cref: &ast::ComponentReference) -> bool {
    match value {
        ast::Expression::ClassModification { target, .. }
        | ast::Expression::ComponentReference(target) => target == cref,
        _ => false,
    }
}

fn cref_to_qualified_name(cref: &ast::ComponentReference) -> Option<ast::QualifiedName> {
    let mut parts = cref.parts.iter();
    let first = parts.next()?;
    let mut qn = ast::QualifiedName::from_ident(first.ident.text.as_ref());
    for part in parts {
        qn = qn.child(part.ident.text.as_ref());
    }
    Some(qn)
}

pub(super) fn resolve_cref_def_id(
    tree: &ast::ClassTree,
    cref: &ast::ComponentReference,
) -> Option<DefId> {
    // MLS §7.3: For multi-part class references (e.g.
    // `Modelica.Media.Water.StandardWater`), resolve the full path target.
    // Parser metadata may attach def_id to the first segment only.
    if cref.parts.len() > 1 {
        let full_name = cref.to_string();
        if let Some(def_id) = tree.get_def_id_by_name(&full_name).or_else(|| {
            tree.get_class_by_qualified_name(&full_name)
                .and_then(|class_def| class_def.def_id)
        }) {
            return Some(def_id);
        }

        // Fallback: walk segments from the first resolved class.
        let first_segment = cref.parts.first()?.ident.text.as_ref();
        let mut current = cref
            .def_id
            .and_then(|def_id| tree.get_class_by_def_id(def_id))
            .filter(|class_def| class_def.name.text.as_ref() == first_segment)
            .or_else(|| {
                tree.get_class_by_qualified_name(first_segment)
                    .or_else(|| find_class_in_tree(tree, first_segment))
            });

        for part in cref.parts.iter().skip(1) {
            current = current.and_then(|class_def| {
                find_member_type_in_class(tree, class_def, part.ident.text.as_ref())
            });
        }

        if let Some(def_id) = current.and_then(|class_def| class_def.def_id) {
            return Some(def_id);
        }
    }

    cref.def_id.or_else(|| {
        find_class_in_tree(tree, &cref.to_string()).and_then(|class_def| class_def.def_id)
    })
}

/// Apply type override for replaceable type redeclarations (MLS §7.3).
pub(super) fn apply_type_override<'a>(
    tree: &ast::ClassTree,
    comp: &'a ast::Component,
    type_overrides: &IndexMap<String, DefId>,
    type_name: &str,
    mod_env: Option<&ast::ModificationEnvironment>,
) -> std::borrow::Cow<'a, ast::Component> {
    // MLS §7.3: Apply type redeclarations by exact type name first.
    // For dotted type names (e.g., `Medium.ThermodynamicState`), also honor
    // package-level redeclarations keyed by the dotted prefix (`Medium`) when
    // the target member exists in the redeclared package.
    //
    // This must apply to package-member model types too (e.g.
    // `Medium.BaseProperties`), not only primitive/record members.
    let exact_override = type_overrides.get(type_name).copied();
    // Instance-level package redeclarations in active mod_env are more specific
    // than enclosing-class defaults when resolving dotted member types.
    let mod_env_override = resolve_dotted_type_from_mod_env(tree, type_name, mod_env);
    let prefix_override = (|| {
        let (prefix, rest) = type_name.split_once('.')?;
        let prefix_override = type_overrides.get(prefix).copied()?;
        let member_name = rest.split('.').next().unwrap_or(rest);
        let override_class = tree.get_class_by_def_id(prefix_override)?;
        find_member_type_in_class(tree, override_class, member_name)?;
        find_member_type_path_in_class(tree, override_class, rest)
            .and_then(|member| member.def_id)
            .or(Some(prefix_override))
    })();

    let override_def_id = exact_override.or(mod_env_override).or(prefix_override);

    if let Some(override_def_id) = override_def_id
        && comp.type_def_id != Some(override_def_id)
    {
        let mut overridden = comp.clone();
        overridden.type_def_id = Some(override_def_id);
        return std::borrow::Cow::Owned(overridden);
    }
    std::borrow::Cow::Borrowed(comp)
}

fn resolve_dotted_type_from_mod_env(
    tree: &ast::ClassTree,
    type_name: &str,
    mod_env: Option<&ast::ModificationEnvironment>,
) -> Option<DefId> {
    let mod_env = mod_env?;
    let (prefix, rest) = type_name.split_once('.')?;
    let qn = ast::QualifiedName::from_ident(prefix);
    let mv = mod_env.get(&qn)?;
    let pkg_def_id = resolve_redeclare_value_def_id(tree, &mv.value, Some(mod_env))?;
    let pkg_class = tree.get_class_by_def_id(pkg_def_id)?;
    find_member_type_path_in_class(tree, pkg_class, rest)
        .and_then(|member| member.def_id)
        .or(Some(pkg_def_id))
}

/// Find a nested class by name in a class and its extends chain.
///
/// MLS §7.3 redeclare targets can be inherited via extends, so component-level
/// redeclare modifiers must recognize replaceable nested classes from base types.
pub(super) fn find_nested_class_in_hierarchy<'a>(
    tree: &'a ast::ClassTree,
    root: &'a ast::ClassDef,
    nested_name: &str,
) -> Option<&'a ast::ClassDef> {
    const MAX_DEPTH: usize = 32;

    let mut to_visit = vec![root];
    let mut visited_def_ids = std::collections::HashSet::<DefId>::new();
    let mut visited_names = std::collections::HashSet::<String>::new();

    for _ in 0..MAX_DEPTH {
        if to_visit.is_empty() {
            break;
        }

        let mut next = Vec::new();
        for class in to_visit.drain(..) {
            let already_seen = match class.def_id {
                Some(def_id) => !visited_def_ids.insert(def_id),
                None => !visited_names.insert(class.name.text.to_string()),
            };
            if already_seen {
                continue;
            }

            if let Some(nested) = class.classes.get(nested_name) {
                return Some(nested);
            }

            next.extend(class.extends.iter().filter_map(|ext| {
                let base_name = ext.base_name.to_string();
                ext.base_def_id
                    .and_then(|def_id| tree.get_class_by_def_id(def_id))
                    .or_else(|| find_class_in_tree(tree, &base_name))
            }));
        }
        to_visit = next;
    }

    None
}

/// Extract active class/package redeclare overrides from a component's modifiers.
///
fn validate_component_class_redeclare_target(
    tree: &ast::ClassTree,
    target_name: &str,
    nested_class: &ast::ClassDef,
    mod_expr: &ast::Expression,
) -> InstantiateResult<()> {
    let ast::Expression::ClassModification { target, .. } = mod_expr else {
        return Err(Box::new(InstantiateError::redeclare_error(
            target_name,
            "redeclare target is missing source span",
            location_to_span(&nested_class.location, &tree.source_map),
        )));
    };
    let Some(part) = target.parts.first() else {
        return Err(Box::new(InstantiateError::redeclare_error(
            target_name,
            "redeclare target is missing source span",
            location_to_span(&nested_class.location, &tree.source_map),
        )));
    };
    let span = location_to_span(&part.ident.location, &tree.source_map);

    if nested_class.is_final {
        return Err(Box::new(InstantiateError::redeclare_final(
            target_name,
            span,
        )));
    }
    if !nested_class.is_replaceable {
        return Err(Box::new(InstantiateError::redeclare_non_replaceable(
            target_name,
            span,
        )));
    }

    Ok(())
}

/// MLS §7.3: component-level redeclare modifiers can target replaceable nested
/// classes declared in base classes (via extends). Persisting these resolved
/// overrides enables downstream phases to evaluate instance-scoped constants.
pub(super) fn extract_component_class_overrides(
    tree: &ast::ClassTree,
    comp: &ast::Component,
    target_class: Option<&ast::ClassDef>,
    mod_env: Option<&ast::ModificationEnvironment>,
) -> InstantiateResult<IndexMap<String, DefId>> {
    let mut overrides = IndexMap::new();
    let Some(target_class) = target_class else {
        return Ok(overrides);
    };

    for (target_name, mod_expr) in &comp.modifications {
        let ast::Expression::ClassModification { .. } = mod_expr else {
            continue;
        };

        let Some(nested_class) = find_nested_class_in_hierarchy(tree, target_class, target_name)
        else {
            continue;
        };
        validate_component_class_redeclare_target(tree, target_name, nested_class, mod_expr)?;
        let resolved_def_id = resolve_redeclare_value_def_id(tree, mod_expr, mod_env);

        if let Some(def_id) = resolved_def_id {
            overrides.insert(target_name.clone(), def_id);
        }
    }

    Ok(overrides)
}

#[cfg(test)]
mod tests {
    use super::{apply_type_override, resolve_cref_def_id};
    use rumoca_core::DefId;
    use rumoca_ir_ast as ast;
    use std::sync::Arc;

    fn make_token(text: &str) -> rumoca_ir_core::Token {
        rumoca_ir_core::Token {
            text: Arc::from(text),
            location: rumoca_ir_core::Location::default(),
            token_number: 0,
            token_type: 0,
        }
    }

    fn make_name(text: &str) -> ast::Name {
        ast::Name {
            name: text.split('.').map(make_token).collect(),
            def_id: None,
        }
    }

    #[test]
    fn test_resolve_cref_def_id_prefers_full_multi_part_path() {
        // Reproduces MSL-style redeclare values such as:
        // `redeclare package Medium = Modelica.Media.Water.StandardWater`
        // where parser metadata can attach def_id to the first segment ("Modelica").
        let modelica_id = DefId::new(1);
        let media_id = DefId::new(2);
        let water_id = DefId::new(3);
        let standard_water_id = DefId::new(4);

        let standard_water = ast::ClassDef {
            name: make_token("StandardWater"),
            def_id: Some(standard_water_id),
            ..Default::default()
        };
        let mut water = ast::ClassDef {
            name: make_token("Water"),
            class_type: ast::ClassType::Package,
            def_id: Some(water_id),
            ..Default::default()
        };
        water
            .classes
            .insert("StandardWater".to_string(), standard_water);

        let mut media = ast::ClassDef {
            name: make_token("Media"),
            class_type: ast::ClassType::Package,
            def_id: Some(media_id),
            ..Default::default()
        };
        media.classes.insert("Water".to_string(), water);

        let mut modelica = ast::ClassDef {
            name: make_token("Modelica"),
            class_type: ast::ClassType::Package,
            def_id: Some(modelica_id),
            ..Default::default()
        };
        modelica.classes.insert("Media".to_string(), media);

        let mut tree = ast::ClassTree::default();
        tree.definitions
            .classes
            .insert("Modelica".to_string(), modelica);
        for (name, id) in [
            ("Modelica", modelica_id),
            ("Modelica.Media", media_id),
            ("Modelica.Media.Water", water_id),
            ("Modelica.Media.Water.StandardWater", standard_water_id),
        ] {
            tree.name_map.insert(name.to_string(), id);
            tree.def_map.insert(id, name.to_string());
        }

        let cref = ast::ComponentReference {
            local: false,
            parts: ["Modelica", "Media", "Water", "StandardWater"]
                .iter()
                .map(|part| ast::ComponentRefPart {
                    ident: make_token(part),
                    subs: None,
                })
                .collect(),
            // Simulate parser metadata that points to the first segment only.
            def_id: Some(modelica_id),
        };

        assert_eq!(
            resolve_cref_def_id(&tree, &cref),
            Some(standard_water_id),
            "multi-part class references must resolve to the full path target, not the first segment"
        );
    }

    #[test]
    fn test_apply_type_override_resolves_dotted_type_through_mod_env_alias_chain() {
        // Covers alias + replaceable package chain behavior:
        // Medium => MediumAlias, where MediumAlias extends MediumB and
        // BaseProperties is inherited from MediumB.
        let medium_b_id = DefId::new(10);
        let medium_alias_id = DefId::new(11);
        let base_properties_id = DefId::new(12);

        let base_properties = ast::ClassDef {
            name: make_token("BaseProperties"),
            class_type: ast::ClassType::Model,
            def_id: Some(base_properties_id),
            ..Default::default()
        };

        let mut medium_b = ast::ClassDef {
            name: make_token("MediumB"),
            class_type: ast::ClassType::Package,
            def_id: Some(medium_b_id),
            ..Default::default()
        };
        medium_b
            .classes
            .insert("BaseProperties".to_string(), base_properties);

        let medium_alias = ast::ClassDef {
            name: make_token("MediumAlias"),
            class_type: ast::ClassType::Package,
            def_id: Some(medium_alias_id),
            extends: vec![ast::Extend {
                base_name: make_name("MediumB"),
                base_def_id: Some(medium_b_id),
                ..Default::default()
            }],
            ..Default::default()
        };

        let mut tree = ast::ClassTree::default();
        tree.definitions
            .classes
            .insert("MediumB".to_string(), medium_b);
        tree.definitions
            .classes
            .insert("MediumAlias".to_string(), medium_alias);
        for (name, def_id) in [
            ("MediumB", medium_b_id),
            ("MediumB.BaseProperties", base_properties_id),
            ("MediumAlias", medium_alias_id),
        ] {
            tree.name_map.insert(name.to_string(), def_id);
            tree.def_map.insert(def_id, name.to_string());
        }

        let comp = ast::Component {
            name: "state".to_string(),
            type_name: make_name("Medium.BaseProperties"),
            type_def_id: None,
            ..Default::default()
        };

        let mut mod_env = ast::ModificationEnvironment::new();
        mod_env.add(
            ast::QualifiedName::from_ident("Medium"),
            ast::ModificationValue::simple(ast::Expression::ComponentReference(
                ast::ComponentReference {
                    local: false,
                    parts: vec![ast::ComponentRefPart {
                        ident: make_token("MediumAlias"),
                        subs: None,
                    }],
                    def_id: Some(medium_alias_id),
                },
            )),
        );

        let overridden = apply_type_override(
            &tree,
            &comp,
            &indexmap::IndexMap::new(),
            "Medium.BaseProperties",
            Some(&mod_env),
        );

        assert_eq!(
            overridden.type_def_id,
            Some(base_properties_id),
            "dotted type should resolve through mod-env package alias chain"
        );
    }
}
