use super::find_class_in_tree;
use super::type_lookup::{find_member_type_in_class, find_member_type_path_in_class};
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
    for (name, nested) in &class.classes {
        if let Some(def_id) = nested.def_id {
            overrides.insert(name.clone(), def_id);
        }
    }

    // 2. Collect from the enclosing class's nested classes.
    // This handles the pattern where a record type (like ThermodynamicState)
    // is redeclared in the enclosing package, and components in the model
    // reference it by its short name.
    collect_enclosing_type_overrides(tree, class, mod_env, &mut overrides);

    // 3. Collect package/type redeclarations from extends-modifications
    // (e.g., extends Base(redeclare replaceable package Medium = ...)).
    collect_extends_redeclare_overrides(tree, class, mod_env, &mut overrides);

    overrides
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
    for ext in &class.extends {
        for ext_mod in &ext.modifications {
            if !ext_mod.redeclare {
                continue;
            }
            let ast::Expression::Modification { target, value } = &ext_mod.expr else {
                continue;
            };
            let Some(first_target) = target.parts.first() else {
                continue;
            };
            let target_name = first_target.ident.text.to_string();
            let Some(def_id) = resolve_redeclare_value_def_id(tree, value, mod_env) else {
                continue;
            };
            overrides.entry(target_name).or_insert(def_id);
        }
    }
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
    for (name, nested) in &class.classes {
        if let Some(def_id) = nested.def_id {
            // Keep nearest declaration if names repeat in deeper bases.
            overrides.entry(name.clone()).or_insert(def_id);
        }
    }
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
    for ext in &class.extends {
        for ext_mod in &ext.modifications {
            if !ext_mod.redeclare {
                continue;
            }

            let ast::Expression::Modification { target, value } = &ext_mod.expr else {
                continue;
            };
            let Some(first_target) = target.parts.first() else {
                continue;
            };
            let target_name = first_target.ident.text.to_string();

            let replacement_def_id = resolve_redeclare_value_def_id(tree, value, mod_env);
            if let Some(def_id) = replacement_def_id {
                overrides.insert(target_name, def_id);
            }
        }
    }
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
        ast::Expression::ClassModification { target, .. } => resolve_cref_def_id(tree, target)
            .or_else(|| resolve_cref_via_mod_env(tree, target, mod_env, depth)),
        ast::Expression::ComponentReference(cref) => resolve_cref_def_id(tree, cref)
            .or_else(|| resolve_cref_via_mod_env(tree, cref, mod_env, depth)),
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
    let mod_value = mod_env.get(&qn)?;
    if mod_value.value == ast::Expression::ComponentReference(cref.clone()) {
        return None;
    }
    resolve_redeclare_value_def_id_with_depth(tree, &mod_value.value, Some(mod_env), depth + 1)
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
/// MLS §7.3: component-level redeclare modifiers can target replaceable nested
/// classes declared in base classes (via extends). Persisting these resolved
/// overrides enables downstream phases to evaluate instance-scoped constants.
pub(super) fn extract_component_class_overrides(
    tree: &ast::ClassTree,
    comp: &ast::Component,
    target_class: Option<&ast::ClassDef>,
    mod_env: Option<&ast::ModificationEnvironment>,
) -> IndexMap<String, DefId> {
    let mut overrides = IndexMap::new();
    let Some(target_class) = target_class else {
        return overrides;
    };

    for (target_name, mod_expr) in &comp.modifications {
        let ast::Expression::ClassModification { .. } = mod_expr else {
            continue;
        };

        let Some(nested_class) = find_nested_class_in_hierarchy(tree, target_class, target_name)
        else {
            continue;
        };
        if !nested_class.is_replaceable {
            continue;
        }

        if let Some(def_id) = resolve_redeclare_value_def_id(tree, mod_expr, mod_env) {
            overrides.insert(target_name.clone(), def_id);
        }
    }

    overrides
}

#[cfg(test)]
mod tests {
    use super::resolve_cref_def_id;
    use rumoca_core::DefId;
    use rumoca_ir_ast as ast;
    use std::sync::Arc;

    fn make_token(text: &str) -> ast::Token {
        ast::Token {
            text: Arc::from(text),
            location: ast::Location::default(),
            token_number: 0,
            token_type: 0,
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
}
