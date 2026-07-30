use super::find_class_in_tree;
use super::traversal_adapter::{
    redeclare_target_value, walk_class_extends_modifications, walk_nested_classes,
};
use super::type_lookup::find_member_type_in_class;
use crate::{InstantiateError, InstantiateResult, location_to_span};
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;

#[derive(Debug, Clone, Default)]
pub(super) struct TypeOverrideMap {
    targets_by_alias_def_id: IndexMap<DefId, DefId>,
    targets_by_alias_path: IndexMap<ast::QualifiedName, DefId>,
}

impl TypeOverrideMap {
    pub(super) fn new() -> Self {
        Self::default()
    }

    pub(super) fn insert_alias(
        &mut self,
        alias_path: ast::QualifiedName,
        alias_def_id: Option<DefId>,
        target_def_id: DefId,
    ) {
        if let Some(alias_def_id) = alias_def_id {
            self.targets_by_alias_def_id
                .insert(alias_def_id, target_def_id);
        }
        self.targets_by_alias_path.insert(alias_path, target_def_id);
    }

    fn insert_alias_if_absent(
        &mut self,
        alias_path: ast::QualifiedName,
        alias_def_id: Option<DefId>,
        target_def_id: DefId,
    ) {
        if let Some(alias_def_id) = alias_def_id {
            self.targets_by_alias_def_id
                .entry(alias_def_id)
                .or_insert(target_def_id);
        }
        self.targets_by_alias_path
            .entry(alias_path)
            .or_insert(target_def_id);
    }

    pub(super) fn insert_class_override(&mut self, class_override: &ast::ClassOverride) {
        self.insert_alias(
            ast::QualifiedName::from_ident(&class_override.alias),
            Some(class_override.alias_def_id),
            class_override.target_def_id,
        );
    }

    pub(super) fn extend_from(&mut self, other: &TypeOverrideMap) {
        for (alias_def_id, target_def_id) in &other.targets_by_alias_def_id {
            self.targets_by_alias_def_id
                .insert(*alias_def_id, *target_def_id);
        }
        for (alias_path, target_def_id) in &other.targets_by_alias_path {
            self.targets_by_alias_path
                .insert(alias_path.clone(), *target_def_id);
        }
    }

    pub(super) fn target_for_alias_def_id(&self, alias_def_id: DefId) -> Option<DefId> {
        self.targets_by_alias_def_id.get(&alias_def_id).copied()
    }

    pub(super) fn target_for_alias_name(&self, alias: &str) -> Option<DefId> {
        self.targets_by_alias_path
            .get(&ast::QualifiedName::from_ident(alias))
            .copied()
    }

    pub(super) fn target_for_reference(
        &self,
        reference: &ast::ComponentReference,
    ) -> Option<DefId> {
        let exact_def_id = (reference.parts.len() == 1)
            .then_some(reference.def_id)
            .flatten()
            .and_then(|def_id| self.target_for_alias_def_id(def_id));
        exact_def_id.or_else(|| {
            let path = cref_to_qualified_name(reference)?;
            self.targets_by_alias_path.get(&path).copied()
        })
    }

    /// Materialize the effective virtual-class selections for instance IR.
    ///
    /// Identity mappings describe locally declared classes, not overrides, so
    /// only changed declaration slots are retained.
    pub(super) fn class_overrides(&self, tree: &ast::ClassTree) -> ast::ClassOverrideMap {
        self.targets_by_alias_def_id
            .iter()
            .filter_map(|(alias_def_id, target_def_id)| {
                if alias_def_id == target_def_id {
                    return None;
                }
                let alias = tree
                    .get_class_by_def_id(*alias_def_id)?
                    .name
                    .text
                    .to_string();
                Some((
                    *alias_def_id,
                    ast::ClassOverride::new(alias, *alias_def_id, *target_def_id, None),
                ))
            })
            .collect()
    }

    fn target_for_name(&self, name: &ast::Name) -> Option<DefId> {
        let exact_def_id = (name.name.len() == 1)
            .then_some(name.def_id)
            .flatten()
            .and_then(|def_id| self.target_for_alias_def_id(def_id));
        exact_def_id.or_else(|| {
            let path = qualified_name_from_name(name)?;
            self.targets_by_alias_path.get(&path).copied()
        })
    }

    fn target_for_path(&self, path: &ast::QualifiedName) -> Option<DefId> {
        self.targets_by_alias_path.get(path).copied()
    }

    /// Specialize inherited nested type identities for one effective package.
    ///
    /// A package alias such as `Medium` can select a derived package while the
    /// selected `BaseProperties` model is declared in an ancestor package.
    /// Components inherited by that model still carry the ancestor's resolved
    /// `DefId` (for example `PartialMedium.ThermodynamicState`). Once the
    /// package is selected for a concrete component instance, map every
    /// declaration of a nested member in its extends chain to the member that
    /// is effective in the selected package.
    pub(super) fn specialize_inherited_nested_types(
        &mut self,
        tree: &ast::ClassTree,
        effective_package_def_id: DefId,
    ) {
        let Some(effective_package) = tree.get_class_by_def_id(effective_package_def_id) else {
            return;
        };
        let mut hierarchy = vec![effective_package];
        let mut visited = std::collections::HashSet::new();

        for index in 0..hierarchy.len() {
            let class = hierarchy[index];
            if let Some(def_id) = class.def_id
                && !visited.insert(def_id)
            {
                continue;
            }
            hierarchy.extend(extends_base_classes(tree, class));
        }

        let member_names: std::collections::HashSet<String> = hierarchy
            .iter()
            .flat_map(|class| class.classes.keys().cloned())
            .collect();
        for member_name in member_names {
            let Some(effective_member_def_id) =
                find_member_type_in_class(tree, effective_package, &member_name)
                    .and_then(|member| member.def_id)
            else {
                continue;
            };
            for inherited_member_def_id in hierarchy.iter().filter_map(|class| {
                class
                    .classes
                    .get(&member_name)
                    .and_then(|member| member.def_id)
            }) {
                self.insert_alias(
                    ast::QualifiedName::from_ident(&member_name),
                    Some(inherited_member_def_id),
                    effective_member_def_id,
                );
            }
        }
    }
}

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
) -> TypeOverrideMap {
    let mut overrides = TypeOverrideMap::new();

    // 1. Collect from the class's own nested classes
    walk_nested_classes(class, |name, nested| {
        if let Some(def_id) = nested.def_id {
            overrides.insert_alias(ast::QualifiedName::from_ident(name), Some(def_id), def_id);
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

    overrides
}

/// Collect type overrides from the enclosing class's nested classes.
///
/// Helper for [`build_type_override_map`] to reduce nesting depth.
fn collect_enclosing_type_overrides(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    mod_env: Option<&ast::ModificationEnvironment>,
    overrides: &mut TypeOverrideMap,
) {
    let Some(class_def_id) = class.def_id else {
        return;
    };
    let Some(qualified_name) = tree.def_map.get(&class_def_id) else {
        return;
    };
    let Some(parent_name) = tree.enclosing_class_names_of(qualified_name).next() else {
        return;
    };
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
    overrides: &mut TypeOverrideMap,
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
    overrides: &mut TypeOverrideMap,
) {
    walk_class_extends_modifications(class, |_, ext_mod| {
        let Some((target_name, value_expr)) = redeclare_target_value(ext_mod) else {
            return;
        };
        let Some(def_id) = resolve_redeclare_value_def_id(tree, value_expr, mod_env) else {
            return;
        };
        let alias_def_id = find_nested_class_in_hierarchy(tree, class, target_name)
            .and_then(|nested| nested.def_id);
        overrides.insert_alias_if_absent(
            ast::QualifiedName::from_ident(target_name),
            alias_def_id,
            def_id,
        );
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

fn insert_nested_class_overrides(class: &ast::ClassDef, overrides: &mut TypeOverrideMap) {
    walk_nested_classes(class, |name, nested| {
        if let Some(def_id) = nested.def_id {
            let alias_path = ast::QualifiedName::from_ident(name);
            let target_def_id = overrides.target_for_path(&alias_path).unwrap_or(def_id);
            overrides.insert_alias_if_absent(alias_path, Some(def_id), target_def_id);
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
    overrides: &mut TypeOverrideMap,
) {
    walk_class_extends_modifications(class, |_, ext_mod| {
        let Some((target_name, value_expr)) = redeclare_target_value(ext_mod) else {
            return;
        };
        if let Some(def_id) = resolve_redeclare_value_def_id(tree, value_expr, mod_env) {
            let alias_def_id = find_nested_class_in_hierarchy(tree, class, target_name)
                .and_then(|nested| nested.def_id);
            overrides.insert_alias(
                ast::QualifiedName::from_ident(target_name),
                alias_def_id,
                def_id,
            );
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
        ast::Expression::Modification { value, .. } => {
            resolve_redeclare_value_def_id_with_depth(tree, value, mod_env, depth + 1)
        }
        ast::Expression::ClassModification { target, .. } => resolve_cref_def_id(tree, target)
            .or_else(|| resolve_cref_via_mod_env(tree, target, mod_env, depth)),
        ast::Expression::FunctionCall { comp, .. } => resolve_cref_def_id(tree, comp)
            .or_else(|| resolve_cref_via_mod_env(tree, comp, mod_env, depth)),
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
    type_overrides: &TypeOverrideMap,
    mod_env: Option<&ast::ModificationEnvironment>,
) -> InstantiateResult<std::borrow::Cow<'a, ast::Component>> {
    // MLS §7.3: Apply type redeclarations by exact type name first.
    // For dotted type names (e.g., `Medium.ThermodynamicState`), also honor
    // package-level redeclarations keyed by the dotted prefix (`Medium`) when
    // the target member exists in the redeclared package.
    //
    // This must apply to package-member model types too (e.g.
    // `Medium.BaseProperties`), not only primitive/record members.
    let exact_override = comp.type_def_id.and_then(|source_def_id| {
        type_overrides
            .target_for_alias_def_id(source_def_id)
            .filter(|target_def_id| {
                exact_type_override_preserves_declaration_slot(tree, source_def_id, *target_def_id)
            })
    });
    // A resolved type identity is authoritative. Name-only lookup is solely a
    // fallback for unresolved declarations; otherwise an active outer scope
    // containing an unrelated same-named type can capture an inherited
    // component after resolve.
    let unresolved_name_override = comp
        .type_def_id
        .is_none()
        .then(|| type_overrides.target_for_name(&comp.type_name))
        .flatten();
    // Instance-level package redeclarations in active mod_env are more specific
    // than enclosing-class defaults when resolving dotted member types.
    let mod_env_override = resolve_dotted_type_from_mod_env(tree, &comp.type_name, mod_env);
    let prefix_override = (|| {
        let (prefix, rest) = name_prefix_and_rest(&comp.type_name)?;
        let prefix_override = type_overrides.target_for_path(&prefix)?;
        let member_name = *rest.first()?;
        let override_class = tree.get_class_by_def_id(prefix_override)?;
        find_member_type_in_class(tree, override_class, member_name)?;
        find_member_type_path_segments(tree, override_class, &rest)
            .and_then(|member| member.def_id)
            .or(Some(prefix_override))
    })();

    let override_def_id = exact_override
        .or(unresolved_name_override)
        .or(mod_env_override)
        .or(prefix_override);
    if let Some(override_def_id) = override_def_id
        && comp.type_def_id != Some(override_def_id)
    {
        // Note: the MLS §7.3.2 constraining-type check happens in the
        // extends-redeclare path (`validate_redeclaration`); this override
        // map also carries package-member type remaps (Medium.X), which are
        // constrained at the package level and must not be re-checked here.
        let mut overridden = comp.clone();
        overridden.type_def_id = Some(override_def_id);
        return Ok(std::borrow::Cow::Owned(overridden));
    }
    Ok(std::borrow::Cow::Borrowed(comp))
}

fn exact_type_override_preserves_declaration_slot(
    tree: &ast::ClassTree,
    source_def_id: DefId,
    target_def_id: DefId,
) -> bool {
    if source_def_id == target_def_id {
        return true;
    }
    let (Some(source), Some(target)) = (
        tree.get_class_by_def_id(source_def_id),
        tree.get_class_by_def_id(target_def_id),
    ) else {
        return false;
    };

    // Differently named targets are explicit class/package aliases. For
    // same-named declarations, require a structural redeclaration or extends
    // relationship so unrelated lexical collisions cannot masquerade as an
    // override.
    source.name.text != target.name.text
        || class_identity_reaches(tree, target, source_def_id)
        || class_identity_reaches(tree, source, target_def_id)
}

fn class_identity_reaches(
    tree: &ast::ClassTree,
    root: &ast::ClassDef,
    target_def_id: DefId,
) -> bool {
    const MAX_DEPTH: usize = 32;
    let mut pending = vec![root];
    let mut visited = std::collections::HashSet::new();

    for _ in 0..MAX_DEPTH {
        let Some(class) = pending.pop() else {
            return false;
        };
        let Some(def_id) = class.def_id else {
            continue;
        };
        if def_id == target_def_id {
            return true;
        }
        if !visited.insert(def_id) {
            continue;
        }
        if let Some(redeclare_target) = class
            .redeclare_target_def_id
            .and_then(|def_id| tree.get_class_by_def_id(def_id))
        {
            pending.push(redeclare_target);
        }
        pending.extend(extends_base_classes(tree, class));
    }
    false
}

fn resolve_dotted_type_from_mod_env(
    tree: &ast::ClassTree,
    type_name: &ast::Name,
    mod_env: Option<&ast::ModificationEnvironment>,
) -> Option<DefId> {
    let mod_env = mod_env?;
    let (prefix, rest) = name_prefix_and_rest(type_name)?;
    let mv = mod_env.get(&prefix)?;
    let pkg_def_id = resolve_redeclare_value_def_id(tree, &mv.value, Some(mod_env))?;
    let pkg_class = tree.get_class_by_def_id(pkg_def_id)?;
    find_member_type_path_segments(tree, pkg_class, &rest)
        .and_then(|member| member.def_id)
        .or(Some(pkg_def_id))
}

fn qualified_name_from_name(name: &ast::Name) -> Option<ast::QualifiedName> {
    let mut parts = name.name.iter();
    let first = parts.next()?;
    let mut qn = ast::QualifiedName::from_ident(first.text.as_ref());
    for part in parts {
        qn.push(part.text.to_string(), Vec::new());
    }
    Some(qn)
}

fn name_prefix_and_rest(name: &ast::Name) -> Option<(ast::QualifiedName, Vec<&str>)> {
    let mut parts = name.name.iter();
    let first = parts.next()?;
    let rest: Vec<&str> = parts.map(|part| part.text.as_ref()).collect();
    if rest.is_empty() {
        return None;
    }
    Some((ast::QualifiedName::from_ident(first.text.as_ref()), rest))
}

fn find_member_type_path_segments<'a>(
    tree: &'a ast::ClassTree,
    class: &'a ast::ClassDef,
    member_path: &[&str],
) -> Option<&'a ast::ClassDef> {
    let mut current = class;
    for segment in member_path {
        current = find_member_type_in_class(tree, current, segment)?;
    }
    Some(current)
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
pub(super) fn validate_component_class_redeclare_target(
    tree: &ast::ClassTree,
    target_name: &str,
    nested_class: &ast::ClassDef,
    mod_expr: &ast::Expression,
    replacement_def_id: DefId,
) -> InstantiateResult<()> {
    let Some(target_ref) = class_redeclare_alias_ref(mod_expr) else {
        return Err(Box::new(InstantiateError::redeclare_error(
            target_name,
            "redeclare target is missing source span",
            location_to_span(
                &nested_class.location,
                &tree.source_map,
                "component class redeclare target",
            )?,
        )));
    };
    let Some(part) = target_ref.parts.first() else {
        return Err(Box::new(InstantiateError::redeclare_error(
            target_name,
            "redeclare target is missing source span",
            location_to_span(
                &nested_class.location,
                &tree.source_map,
                "component class redeclare target",
            )?,
        )));
    };
    let span = location_to_span(
        &part.ident.location,
        &tree.source_map,
        "component class redeclare name",
    )?;

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

    validate_component_redeclare_constraint(
        tree,
        target_name,
        nested_class,
        replacement_def_id,
        span,
    )?;

    Ok(())
}

fn validate_component_redeclare_constraint(
    tree: &ast::ClassTree,
    target_name: &str,
    nested_class: &ast::ClassDef,
    replacement_def_id: DefId,
    span: rumoca_core::Span,
) -> InstantiateResult<()> {
    let Some(constraint_def_id) = component_redeclare_constraint_def_id(nested_class) else {
        return Err(Box::new(InstantiateError::redeclare_error(
            target_name,
            "resolved replaceable declaration has no constraining-type identity",
            span,
        )));
    };
    let replacement_name = tree
        .def_map
        .get(&replacement_def_id)
        .cloned()
        .or_else(|| {
            tree.get_class_by_def_id(replacement_def_id)
                .map(|class| class.name.text.to_string())
        })
        .ok_or_else(|| {
            Box::new(InstantiateError::redeclare_error(
                target_name,
                "resolved redeclare value has no class identity",
                span,
            ))
        })?;
    let constraint_name = tree
        .def_map
        .get(&constraint_def_id)
        .cloned()
        .or_else(|| {
            tree.get_class_by_def_id(constraint_def_id)
                .map(|class| class.name.text.to_string())
        })
        .ok_or_else(|| {
            Box::new(InstantiateError::redeclare_error(
                target_name,
                "resolved constraining type has no class identity",
                span,
            ))
        })?;

    if !crate::inheritance::is_type_subtype(tree, &replacement_name, &constraint_name) {
        return Err(Box::new(InstantiateError::redeclare_constraint_violation(
            target_name,
            &replacement_name,
            &constraint_name,
            span,
        )));
    }

    Ok(())
}

fn component_redeclare_constraint_def_id(nested_class: &ast::ClassDef) -> Option<DefId> {
    nested_class
        .constrainedby
        .as_ref()
        .and_then(|constraint| constraint.def_id)
        .or_else(|| {
            nested_class
                .extends
                .first()
                .and_then(|extend| extend.base_def_id.or(extend.base_name.def_id))
        })
        .or(nested_class.def_id)
}

/// MLS §7.3: component-level redeclare modifiers can target replaceable nested
/// classes declared in base classes (via extends). Persisting these resolved
/// overrides enables downstream phases to evaluate instance-scoped constants.
pub(super) fn extract_component_class_overrides(
    tree: &ast::ClassTree,
    comp: &ast::Component,
    target_class: Option<&ast::ClassDef>,
    mod_env: Option<&ast::ModificationEnvironment>,
) -> InstantiateResult<ast::ClassOverrideMap> {
    let mut overrides = IndexMap::default();
    let Some(target_class) = target_class else {
        return Ok(overrides);
    };

    validate_component_source_modifier_metadata(tree, comp)?;
    for (index, mod_expr) in comp.source_modifications.iter().enumerate() {
        let is_redeclare = comp
            .source_modification_redeclare_flags
            .get(index)
            .copied()
            .unwrap_or(false);
        let Some(target_name) = component_source_modifier_target_name(mod_expr) else {
            continue;
        };

        let Some(nested_class) = find_nested_class_in_hierarchy(tree, target_class, &target_name)
        else {
            continue;
        };
        if !is_redeclare {
            reject_unmarked_component_class_replacement(tree, &target_name, mod_expr)?;
            continue;
        }
        let Some(alias_def_id) = nested_class.def_id else {
            return Err(Box::new(InstantiateError::redeclare_error(
                &target_name,
                "resolved redeclare target has no DefId",
                location_to_span(
                    &nested_class.location,
                    &tree.source_map,
                    "resolved component class redeclare target",
                )?,
            )));
        };
        if is_forwarding_component_redeclare(mod_expr, &target_name) {
            // The enclosing override is instance-local and is applied by
            // `resolve_component_nested_type_overrides`; validating the
            // lexical alias itself would compare the wrong class identity.
            continue;
        }
        // `source_modifications` owns the redeclare keyword and source span;
        // the corresponding normalized modification owns resolved `DefId`
        // metadata. Keep those roles paired by the structured target key.
        let resolved_mod_expr = comp.modifications.get(&target_name).unwrap_or(mod_expr);
        let Some(def_id) = resolve_redeclare_value_def_id(tree, resolved_mod_expr, mod_env)
            .or_else(|| {
                class_redeclare_target_ref(resolved_mod_expr)
                    .and_then(|target| target.def_id.or_else(|| resolve_cref_def_id(tree, &target)))
            })
            .or_else(|| resolve_redeclare_value_def_id(tree, mod_expr, mod_env))
            .or_else(|| {
                class_redeclare_target_ref(mod_expr)
                    .and_then(|target| target.def_id.or_else(|| resolve_cref_def_id(tree, &target)))
            })
        else {
            return Err(Box::new(InstantiateError::redeclare_error(
                &target_name,
                "component redeclare value did not resolve to a class",
                mod_expr.span(),
            )));
        };
        validate_component_class_redeclare_target(
            tree,
            &target_name,
            nested_class,
            mod_expr,
            def_id,
        )?;
        overrides.insert(
            alias_def_id,
            ast::ClassOverride::new(
                target_name,
                alias_def_id,
                def_id,
                class_redeclare_target_ref(mod_expr),
            )
            .with_modifier_args(class_redeclare_modifier_args(mod_expr)),
        );
    }

    Ok(overrides)
}

fn is_forwarding_component_redeclare(mod_expr: &ast::Expression, target_name: &str) -> bool {
    let Some(target) = class_redeclare_target_ref(mod_expr) else {
        return false;
    };
    let [part] = target.parts.as_slice() else {
        return false;
    };
    part.subs.is_none() && part.ident.text.as_ref() == target_name
}

fn validate_component_source_modifier_metadata(
    tree: &ast::ClassTree,
    comp: &ast::Component,
) -> InstantiateResult<()> {
    if comp.source_modifications.len() == comp.source_modification_redeclare_flags.len() {
        return Ok(());
    }
    Err(Box::new(InstantiateError::redeclare_error(
        &comp.name,
        "component source modifiers lost their redeclare metadata",
        location_to_span(
            &comp.location,
            &tree.source_map,
            "component source modifier metadata",
        )?,
    )))
}

fn component_source_modifier_target_name(mod_expr: &ast::Expression) -> Option<String> {
    class_redeclare_alias_ref(mod_expr)?
        .parts
        .first()
        .map(|part| part.ident.text.to_string())
}

fn reject_unmarked_component_class_replacement(
    tree: &ast::ClassTree,
    target_name: &str,
    mod_expr: &ast::Expression,
) -> InstantiateResult<()> {
    let ast::Expression::Modification { value, .. } = mod_expr else {
        return Ok(());
    };
    if resolve_redeclare_value_def_id(tree, value, None).is_none() {
        return Ok(());
    }
    let span = class_redeclare_alias_ref(mod_expr)
        .expect("source modifier target was validated before replacement checking")
        .span;
    Err(Box::new(InstantiateError::redeclare_error(
        target_name,
        "changing a class or package requires the `redeclare` keyword",
        span,
    )))
}

fn class_redeclare_alias_ref(mod_expr: &ast::Expression) -> Option<&ast::ComponentReference> {
    match mod_expr {
        ast::Expression::Modification { target, .. }
        | ast::Expression::ClassModification { target, .. } => Some(target),
        _ => None,
    }
}

fn class_redeclare_target_ref(mod_expr: &ast::Expression) -> Option<ast::ComponentReference> {
    match mod_expr {
        ast::Expression::Modification { target, value, .. } => {
            class_redeclare_target_ref(value).or_else(|| Some(target.clone()))
        }
        ast::Expression::ClassModification { target, .. } => Some(target.clone()),
        ast::Expression::FunctionCall { comp, .. } => Some(comp.clone()),
        ast::Expression::ComponentReference(cref) => Some(cref.clone()),
        _ => None,
    }
}

pub(super) fn class_redeclare_modifier_args(mod_expr: &ast::Expression) -> Vec<ast::Expression> {
    match mod_expr {
        ast::Expression::Modification { value, .. } => class_redeclare_modifier_args(value),
        ast::Expression::ClassModification { modifications, .. } => modifications.clone(),
        ast::Expression::FunctionCall { args, .. } => args.clone(),
        _ => Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::{
        TypeOverrideMap, apply_type_override, build_type_override_map, resolve_cref_def_id,
    };
    use miette::Diagnostic;
    use rumoca_core::DefId;
    use rumoca_ir_ast as ast;
    use std::sync::Arc;

    fn make_token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: Arc::from(text),
            location: rumoca_core::Location::default(),
            token_number: 0,
            token_type: 0,
        }
    }

    fn make_name(text: &str) -> ast::Name {
        ast::Name::from_string(text)
    }

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("type_overrides_test.mo"),
            1,
            2,
        )
    }

    const COMPONENT_REDECLARE_SOURCE: &str = r"
package Constraint
  constant Real k = 10.0;
end Constraint;

package Good
  extends Constraint(k = 20.0);
end Good;

package Bad
  constant Real k = 30.0;
end Bad;

model Inner
  replaceable package Medium = Constraint
    constrainedby Constraint;
  Real y = Medium.k;
end Inner;

model FinalInner
  final package Medium = Constraint;
  Real y = Medium.k;
end FinalInner;

model NonReplaceableInner
  package Medium = Constraint;
  Real y = Medium.k;
end NonReplaceableInner;

model ComponentGood
  Inner i(redeclare package Medium = Good);
end ComponentGood;

model ComponentReplaceableGood
  Inner i(replaceable package Medium = Good);
end ComponentReplaceableGood;

model ComponentExplicit
  Inner i(redeclare package Medium = Good(k = 25.0));
end ComponentExplicit;

model ComponentBad
  Inner i(redeclare package Medium = Bad);
end ComponentBad;

model ExtendsBad
  extends Inner(redeclare package Medium = Bad);
end ExtendsBad;

model ComponentFinal
  FinalInner i(redeclare package Medium = Good);
end ComponentFinal;

model ComponentNonReplaceable
  NonReplaceableInner i(redeclare package Medium = Good);
end ComponentNonReplaceable;

model ComponentWithoutRedeclare
  Inner i(Medium = Good);
end ComponentWithoutRedeclare;

model ComponentClassModification
  Inner i(Medium(k = 15.0));
end ComponentClassModification;
";

    fn resolved_component_redeclare_tree() -> ast::ClassTree {
        let file_name = "<component_redeclare_test>";
        let stored = rumoca_phase_parse::parse_to_ast(COMPONENT_REDECLARE_SOURCE, file_name)
            .expect("component redeclare fixture should parse");
        let mut tree = ast::ClassTree::from_parsed(stored);
        tree.source_map.add(file_name, COMPONENT_REDECLARE_SOURCE);
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
            .expect("component redeclare fixture should resolve")
            .into_inner()
    }

    fn instantiate_component_redeclare_error(model: &str) -> Box<crate::InstantiateError> {
        crate::instantiate_model(&resolved_component_redeclare_tree(), model)
            .expect_err("component redeclare fixture should fail")
    }

    fn diagnostic_code(error: &crate::InstantiateError) -> Option<String> {
        error.code().map(|code| code.to_string())
    }

    #[test]
    fn component_redeclare_is_source_marked_and_selects_the_resolved_target() {
        let tree = resolved_component_redeclare_tree();
        let component_declaration = tree
            .get_class_by_qualified_name("ComponentGood")
            .and_then(|class| class.components.get("i"))
            .expect("component declaration i");
        assert_eq!(
            component_declaration.source_modification_redeclare_flags,
            vec![true]
        );
        let overlay = crate::instantiate_model(&tree, "ComponentGood")
            .expect("valid component redeclare should instantiate");
        let component = overlay
            .components
            .values()
            .find(|component| component.qualified_name.to_flat_string() == "i")
            .expect("component instance i");
        let class_override = component
            .class_overrides
            .values()
            .find(|class_override| class_override.alias == "Medium")
            .expect("source-marked Medium redeclare");

        assert_eq!(
            tree.def_map.get(&class_override.target_def_id),
            Some(&"Good".to_string())
        );
        assert!(class_override.modifier_args.is_empty());
    }

    #[test]
    fn replaceable_component_modifier_is_a_source_marked_redeclare() {
        let tree = resolved_component_redeclare_tree();
        let component_declaration = tree
            .get_class_by_qualified_name("ComponentReplaceableGood")
            .and_then(|class| class.components.get("i"))
            .expect("component declaration i");
        assert_eq!(
            component_declaration.source_modification_redeclare_flags,
            vec![true]
        );
        let overlay = crate::instantiate_model(&tree, "ComponentReplaceableGood")
            .expect("replaceable modifier should act as a redeclare");
        let class_override = overlay
            .components
            .values()
            .find(|component| component.qualified_name.to_flat_string() == "i")
            .and_then(|component| {
                component
                    .class_overrides
                    .values()
                    .find(|class_override| class_override.alias == "Medium")
            })
            .expect("source-marked Medium redeclare");

        assert_eq!(
            tree.def_map.get(&class_override.target_def_id),
            Some(&"Good".to_string())
        );
    }

    #[test]
    fn component_redeclare_rejects_constraining_type_violation() {
        let component_error = instantiate_component_redeclare_error("ComponentBad");
        let extends_error = instantiate_component_redeclare_error("ExtendsBad");
        assert_eq!(
            diagnostic_code(&component_error),
            Some("rumoca::instantiate::EI027".to_string())
        );
        assert_eq!(
            diagnostic_code(&extends_error),
            Some("rumoca::instantiate::EI027".to_string())
        );
    }

    #[test]
    fn component_redeclare_preserves_explicit_replacement_modifiers() {
        let tree = resolved_component_redeclare_tree();
        let overlay = crate::instantiate_model(&tree, "ComponentExplicit")
            .expect("valid modified component redeclare should instantiate");
        let class_override = overlay
            .components
            .values()
            .find(|component| component.qualified_name.to_flat_string() == "i")
            .and_then(|component| {
                component
                    .class_overrides
                    .values()
                    .find(|class_override| class_override.alias == "Medium")
            })
            .expect("source-marked Medium redeclare");

        assert_eq!(class_override.modifier_args.len(), 1);
        let ast::Expression::Modification { target, .. } = &class_override.modifier_args[0] else {
            panic!("explicit replacement modifier should remain a modification");
        };
        assert_eq!(target.to_string(), "k");
    }

    #[test]
    fn component_redeclare_rejects_final_and_nonreplaceable_targets() {
        let final_error = instantiate_component_redeclare_error("ComponentFinal");
        assert_eq!(
            diagnostic_code(&final_error),
            Some("rumoca::instantiate::EI028".to_string())
        );

        let nonreplaceable_error = instantiate_component_redeclare_error("ComponentNonReplaceable");
        assert_eq!(
            diagnostic_code(&nonreplaceable_error),
            Some("rumoca::instantiate::EI014".to_string())
        );
    }

    #[test]
    fn class_replacement_without_redeclare_is_not_inferred_from_expression_shape() {
        let tree = resolved_component_redeclare_tree();
        let component_declaration = tree
            .get_class_by_qualified_name("ComponentWithoutRedeclare")
            .and_then(|class| class.components.get("i"))
            .expect("component declaration i");
        assert_eq!(
            component_declaration.source_modification_redeclare_flags,
            vec![false]
        );
        let error = crate::instantiate_model(&tree, "ComponentWithoutRedeclare")
            .expect_err("unmarked class replacement should fail");
        assert_eq!(
            diagnostic_code(&error),
            Some("rumoca::instantiate::EI007".to_string())
        );
        assert!(
            error
                .to_string()
                .contains("requires the `redeclare` keyword")
        );
    }

    #[test]
    fn ordinary_class_modification_without_redeclare_does_not_select_a_new_target() {
        let tree = resolved_component_redeclare_tree();
        let component_declaration = tree
            .get_class_by_qualified_name("ComponentClassModification")
            .and_then(|class| class.components.get("i"))
            .expect("component declaration i");
        assert_eq!(
            component_declaration.source_modification_redeclare_flags,
            vec![false]
        );
        let overlay = crate::instantiate_model(&tree, "ComponentClassModification")
            .expect("ordinary nested class modification should instantiate");
        let component = overlay
            .components
            .values()
            .find(|component| component.qualified_name.to_flat_string() == "i")
            .expect("component instance i");
        assert!(
            component
                .class_overrides
                .values()
                .all(|class_override| class_override.alias != "Medium")
        );
    }

    fn nested_type_override_fixture() -> (ast::ClassTree, DefId, DefId) {
        let base_package_id = DefId::new(1);
        let base_state_id = DefId::new(2);
        let derived_package_id = DefId::new(3);
        let derived_state_id = DefId::new(4);
        let base_properties_id = DefId::new(5);

        let base_state = ast::ClassDef {
            name: make_token("ThermodynamicState"),
            def_id: Some(base_state_id),
            class_type: rumoca_core::ClassType::Record,
            is_replaceable: true,
            ..Default::default()
        };
        let mut base_package = ast::ClassDef {
            name: make_token("BaseMedium"),
            def_id: Some(base_package_id),
            class_type: rumoca_core::ClassType::Package,
            ..Default::default()
        };
        base_package
            .classes
            .insert("ThermodynamicState".to_string(), base_state);

        let derived_state = ast::ClassDef {
            name: make_token("ThermodynamicState"),
            def_id: Some(derived_state_id),
            class_type: rumoca_core::ClassType::Record,
            is_replaceable: true,
            is_redeclare: true,
            redeclare_target_def_id: Some(base_state_id),
            ..Default::default()
        };
        let base_properties = ast::ClassDef {
            name: make_token("BaseProperties"),
            def_id: Some(base_properties_id),
            class_type: rumoca_core::ClassType::Model,
            ..Default::default()
        };
        let mut derived_package = ast::ClassDef {
            name: make_token("DerivedMedium"),
            def_id: Some(derived_package_id),
            class_type: rumoca_core::ClassType::Package,
            extends: vec![ast::Extend {
                base_name: make_name("BaseMedium"),
                base_def_id: Some(base_package_id),
                ..Default::default()
            }],
            ..Default::default()
        };
        derived_package
            .classes
            .insert("ThermodynamicState".to_string(), derived_state);
        derived_package
            .classes
            .insert("BaseProperties".to_string(), base_properties);

        let mut tree = ast::ClassTree::default();
        // Scope structure mirrors what resolve registration produces: the
        // enclosing-class walk traverses the scope tree, not rendered names.
        let derived_scope = tree
            .scope_tree
            .create_scope(rumoca_core::ScopeId::GLOBAL, ast::ScopeKind::Class);
        let base_properties_scope = tree
            .scope_tree
            .create_scope(derived_scope, ast::ScopeKind::Class);
        tree.scope_to_class
            .insert(derived_scope, derived_package_id);
        tree.scope_to_class
            .insert(base_properties_scope, base_properties_id);
        if let Some(base_properties) = derived_package.classes.get_mut("BaseProperties") {
            base_properties.scope_id = Some(base_properties_scope);
        }
        derived_package.scope_id = Some(derived_scope);
        tree.definitions
            .classes
            .insert("BaseMedium".to_string(), base_package);
        tree.definitions
            .classes
            .insert("DerivedMedium".to_string(), derived_package);
        for (name, def_id) in [
            ("BaseMedium", base_package_id),
            ("BaseMedium.ThermodynamicState", base_state_id),
            ("DerivedMedium", derived_package_id),
            ("DerivedMedium.ThermodynamicState", derived_state_id),
            ("DerivedMedium.BaseProperties", base_properties_id),
        ] {
            tree.name_map.insert(name.to_string(), def_id);
            tree.def_map.insert(def_id, name.to_string());
        }
        (tree, base_state_id, derived_state_id)
    }

    #[test]
    fn test_redeclared_nested_type_remaps_inherited_type_def_id() {
        let (tree, base_state_id, derived_state_id) = nested_type_override_fixture();
        let base_properties = tree
            .get_class_by_qualified_name("DerivedMedium.BaseProperties")
            .expect("base properties class");
        let overrides = build_type_override_map(&tree, base_properties, None);
        let comp = ast::Component {
            name: "state".to_string(),
            type_name: make_name("ThermodynamicState"),
            type_def_id: Some(base_state_id),
            ..ast::Component::empty_with_span(test_span())
        };

        let overridden =
            apply_type_override(&tree, &comp, &overrides, None).expect("override should validate");

        assert_eq!(
            overridden.type_def_id,
            Some(derived_state_id),
            "inherited references resolved to the base nested DefId must use the active redeclared nested type"
        );
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
            class_type: rumoca_core::ClassType::Package,
            def_id: Some(water_id),
            ..Default::default()
        };
        water
            .classes
            .insert("StandardWater".to_string(), standard_water);

        let mut media = ast::ClassDef {
            name: make_token("Media"),
            class_type: rumoca_core::ClassType::Package,
            def_id: Some(media_id),
            ..Default::default()
        };
        media.classes.insert("Water".to_string(), water);

        let mut modelica = ast::ClassDef {
            name: make_token("Modelica"),
            class_type: rumoca_core::ClassType::Package,
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
            target_def_id: Some(standard_water_id),
            span: rumoca_core::Span::DUMMY,
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
            class_type: rumoca_core::ClassType::Model,
            def_id: Some(base_properties_id),
            ..Default::default()
        };

        let mut medium_b = ast::ClassDef {
            name: make_token("MediumB"),
            class_type: rumoca_core::ClassType::Package,
            def_id: Some(medium_b_id),
            ..Default::default()
        };
        medium_b
            .classes
            .insert("BaseProperties".to_string(), base_properties);

        let medium_alias = ast::ClassDef {
            name: make_token("MediumAlias"),
            class_type: rumoca_core::ClassType::Package,
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
            ..ast::Component::empty_with_span(test_span())
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
                    target_def_id: Some(medium_alias_id),
                    span: rumoca_core::Span::DUMMY,
                },
            )),
        );

        let overridden = apply_type_override(&tree, &comp, &TypeOverrideMap::new(), Some(&mod_env))
            .expect("override should validate");

        assert_eq!(
            overridden.type_def_id,
            Some(base_properties_id),
            "dotted type should resolve through mod-env package alias chain"
        );
    }

    #[test]
    fn test_apply_type_override_uses_dotted_member_not_partial_name_def_id() {
        let medium_alias_id = DefId::new(20);
        let concrete_medium_id = DefId::new(21);
        let base_properties_id = DefId::new(22);

        let base_properties = ast::ClassDef {
            name: make_token("BaseProperties"),
            class_type: rumoca_core::ClassType::Model,
            def_id: Some(base_properties_id),
            ..Default::default()
        };
        let mut concrete_medium = ast::ClassDef {
            name: make_token("ConcreteMedium"),
            class_type: rumoca_core::ClassType::Package,
            def_id: Some(concrete_medium_id),
            ..Default::default()
        };
        concrete_medium
            .classes
            .insert("BaseProperties".to_string(), base_properties);

        let mut tree = ast::ClassTree::default();
        tree.definitions
            .classes
            .insert("ConcreteMedium".to_string(), concrete_medium);
        tree.name_map
            .insert("ConcreteMedium".to_string(), concrete_medium_id);
        tree.name_map.insert(
            "ConcreteMedium.BaseProperties".to_string(),
            base_properties_id,
        );
        tree.def_map
            .insert(concrete_medium_id, "ConcreteMedium".to_string());
        tree.def_map.insert(
            base_properties_id,
            "ConcreteMedium.BaseProperties".to_string(),
        );

        let mut type_name = make_name("Medium.BaseProperties");
        type_name.def_id = Some(medium_alias_id);
        let comp = ast::Component {
            name: "medium".to_string(),
            type_name,
            type_def_id: None,
            ..ast::Component::empty_with_span(test_span())
        };
        let mut type_overrides = TypeOverrideMap::new();
        type_overrides.insert_alias(
            ast::QualifiedName::from_ident("Medium"),
            Some(medium_alias_id),
            concrete_medium_id,
        );

        let overridden = apply_type_override(&tree, &comp, &type_overrides, None)
            .expect("override should validate");

        assert_eq!(
            overridden.type_def_id,
            Some(base_properties_id),
            "dotted type names with partial first-segment DefIds must resolve to the concrete member"
        );
    }

    #[test]
    fn test_selected_package_specializes_types_in_inherited_member_models() {
        let partial_medium_id = DefId::new(30);
        let partial_state_id = DefId::new(31);
        let base_properties_id = DefId::new(32);
        let concrete_medium_id = DefId::new(33);
        let concrete_state_id = DefId::new(34);

        let partial_state = ast::ClassDef {
            name: make_token("ThermodynamicState"),
            class_type: rumoca_core::ClassType::Record,
            def_id: Some(partial_state_id),
            ..Default::default()
        };
        let base_properties = ast::ClassDef {
            name: make_token("BaseProperties"),
            class_type: rumoca_core::ClassType::Model,
            def_id: Some(base_properties_id),
            ..Default::default()
        };
        let mut partial_medium = ast::ClassDef {
            name: make_token("PartialMedium"),
            class_type: rumoca_core::ClassType::Package,
            def_id: Some(partial_medium_id),
            ..Default::default()
        };
        partial_medium
            .classes
            .insert("ThermodynamicState".to_string(), partial_state);
        partial_medium
            .classes
            .insert("BaseProperties".to_string(), base_properties);

        let concrete_state = ast::ClassDef {
            name: make_token("ThermodynamicState"),
            class_type: rumoca_core::ClassType::Record,
            def_id: Some(concrete_state_id),
            is_redeclare: true,
            redeclare_target_def_id: Some(partial_state_id),
            ..Default::default()
        };
        let mut concrete_medium = ast::ClassDef {
            name: make_token("ConcreteMedium"),
            class_type: rumoca_core::ClassType::Package,
            def_id: Some(concrete_medium_id),
            extends: vec![ast::Extend {
                base_name: make_name("PartialMedium"),
                base_def_id: Some(partial_medium_id),
                ..Default::default()
            }],
            ..Default::default()
        };
        concrete_medium
            .classes
            .insert("ThermodynamicState".to_string(), concrete_state);

        let mut tree = ast::ClassTree::default();
        tree.definitions
            .classes
            .insert("PartialMedium".to_string(), partial_medium);
        tree.definitions
            .classes
            .insert("ConcreteMedium".to_string(), concrete_medium);
        for (name, def_id) in [
            ("PartialMedium", partial_medium_id),
            ("PartialMedium.ThermodynamicState", partial_state_id),
            ("PartialMedium.BaseProperties", base_properties_id),
            ("ConcreteMedium", concrete_medium_id),
            ("ConcreteMedium.ThermodynamicState", concrete_state_id),
        ] {
            tree.name_map.insert(name.to_string(), def_id);
            tree.def_map.insert(def_id, name.to_string());
        }

        let inherited_state_component = ast::Component {
            name: "state".to_string(),
            type_name: make_name("ThermodynamicState"),
            type_def_id: Some(partial_state_id),
            ..ast::Component::empty_with_span(test_span())
        };
        let mut type_overrides = TypeOverrideMap::new();
        type_overrides.insert_alias(
            ast::QualifiedName::from_ident("Medium"),
            None,
            concrete_medium_id,
        );
        type_overrides.specialize_inherited_nested_types(&tree, concrete_medium_id);

        let overridden =
            apply_type_override(&tree, &inherited_state_component, &type_overrides, None)
                .expect("selected package should specialize inherited member types");
        assert_eq!(
            overridden.type_def_id,
            Some(concrete_state_id),
            "an inherited BaseProperties model must use the selected medium's state type"
        );
    }

    #[test]
    fn test_resolved_type_identity_rejects_unrelated_same_named_override() {
        let internal_constants_id = DefId::new(40);
        let unrelated_constants_id = DefId::new(41);
        let internal_constants = ast::ClassDef {
            name: make_token("SpiceConstants"),
            class_type: rumoca_core::ClassType::Record,
            def_id: Some(internal_constants_id),
            ..Default::default()
        };
        let unrelated_constants = ast::ClassDef {
            name: make_token("SpiceConstants"),
            class_type: rumoca_core::ClassType::Record,
            def_id: Some(unrelated_constants_id),
            ..Default::default()
        };
        let mut tree = ast::ClassTree::default();
        tree.definitions
            .classes
            .insert("InternalConstants".to_string(), internal_constants);
        tree.definitions
            .classes
            .insert("UnrelatedConstants".to_string(), unrelated_constants);
        for (name, def_id) in [
            ("Root.Internal.SpiceConstants", internal_constants_id),
            ("Root.Examples.Test.SpiceConstants", unrelated_constants_id),
        ] {
            tree.name_map.insert(name.to_string(), def_id);
            tree.def_map.insert(def_id, name.to_string());
        }

        let component = ast::Component {
            name: "constants".to_string(),
            type_name: make_name("SpiceConstants"),
            type_def_id: Some(internal_constants_id),
            ..ast::Component::empty_with_span(test_span())
        };
        let mut type_overrides = TypeOverrideMap::new();
        type_overrides.insert_alias(
            ast::QualifiedName::from_ident("SpiceConstants"),
            Some(internal_constants_id),
            unrelated_constants_id,
        );

        let overridden = apply_type_override(&tree, &component, &type_overrides, None)
            .expect("unrelated type collision should be ignored");
        assert_eq!(
            overridden.type_def_id,
            Some(internal_constants_id),
            "resolve's exact type identity must survive unrelated same-named outer types"
        );
    }
}
