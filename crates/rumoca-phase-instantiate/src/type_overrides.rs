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
    overrides: &mut TypeOverrideMap,
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
            overrides.insert_alias(ast::QualifiedName::from_ident(name), None, def_id);
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

            insert_nested_class_overrides(tree, class, overrides);
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

fn insert_nested_class_overrides(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    overrides: &mut TypeOverrideMap,
) {
    walk_nested_classes(class, |name, nested| {
        if let Some(def_id) = nested.def_id {
            let alias_path = ast::QualifiedName::from_ident(name);
            let target_def_id = overrides.target_for_path(&alias_path).unwrap_or(def_id);
            overrides.insert_alias_if_absent(alias_path, Some(def_id), target_def_id);
            insert_redeclared_base_type_aliases(tree, class, name, nested, def_id, overrides);
        }
    });
}

fn insert_redeclared_base_type_aliases(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    name: &str,
    nested: &ast::ClassDef,
    redeclared_def_id: DefId,
    overrides: &mut TypeOverrideMap,
) {
    for ext in &nested.extends {
        if let Some(base_name) = ext
            .base_def_id
            .and_then(|def_id| tree.def_map.get(&def_id).cloned())
        {
            let alias_path = ast::QualifiedName::from_dotted(&base_name);
            overrides.insert_alias_if_absent(alias_path, ext.base_def_id, redeclared_def_id);
        }
    }

    for base_class in extends_base_classes(tree, class) {
        if let Some(base_nested) = base_class.classes.get(name)
            && let Some(base_name) = base_nested
                .def_id
                .and_then(|def_id| tree.def_map.get(&def_id).cloned())
        {
            let alias_path = ast::QualifiedName::from_dotted(&base_name);
            overrides.insert_alias_if_absent(alias_path, base_nested.def_id, redeclared_def_id);
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
    resolve_redeclare_value_def_id_with_overrides(tree, value, mod_env, None)
}

pub(super) fn resolve_redeclare_value_def_id_with_overrides(
    tree: &ast::ClassTree,
    value: &ast::Expression,
    mod_env: Option<&ast::ModificationEnvironment>,
    type_overrides: Option<&TypeOverrideMap>,
) -> Option<DefId> {
    resolve_redeclare_value_def_id_with_depth(tree, value, mod_env, type_overrides, 0)
}

fn apply_redeclare_type_override(
    type_overrides: Option<&TypeOverrideMap>,
    cref: &ast::ComponentReference,
    def_id: DefId,
) -> DefId {
    type_overrides
        .and_then(|overrides| {
            overrides
                .target_for_reference(cref)
                .or_else(|| overrides.target_for_alias_def_id(def_id))
        })
        .unwrap_or(def_id)
}

fn resolve_redeclare_reference_def_id(
    tree: &ast::ClassTree,
    cref: &ast::ComponentReference,
    mod_env: Option<&ast::ModificationEnvironment>,
    type_overrides: Option<&TypeOverrideMap>,
    depth: usize,
) -> Option<DefId> {
    let def_id = resolve_cref_def_id(tree, cref)
        .or_else(|| resolve_cref_via_mod_env(tree, cref, mod_env, type_overrides, depth))?;
    Some(apply_redeclare_type_override(type_overrides, cref, def_id))
}

fn resolve_redeclare_value_def_id_with_depth(
    tree: &ast::ClassTree,
    value: &ast::Expression,
    mod_env: Option<&ast::ModificationEnvironment>,
    type_overrides: Option<&TypeOverrideMap>,
    depth: usize,
) -> Option<DefId> {
    const MAX_REDECLARE_RESOLVE_DEPTH: usize = 8;
    if depth > MAX_REDECLARE_RESOLVE_DEPTH {
        return None;
    }

    match value {
        ast::Expression::Modification { value, .. } => resolve_redeclare_value_def_id_with_depth(
            tree,
            value,
            mod_env,
            type_overrides,
            depth + 1,
        ),
        ast::Expression::ClassModification { target, .. } => {
            resolve_redeclare_reference_def_id(tree, target, mod_env, type_overrides, depth)
        }
        ast::Expression::FunctionCall { comp, .. } => {
            resolve_redeclare_reference_def_id(tree, comp, mod_env, type_overrides, depth)
        }
        ast::Expression::ComponentReference(cref) => {
            resolve_redeclare_reference_def_id(tree, cref, mod_env, type_overrides, depth)
        }
        _ => None,
    }
}

fn resolve_cref_via_mod_env(
    tree: &ast::ClassTree,
    cref: &ast::ComponentReference,
    mod_env: Option<&ast::ModificationEnvironment>,
    type_overrides: Option<&TypeOverrideMap>,
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
    resolve_redeclare_value_def_id_with_depth(
        tree,
        &mod_value.value,
        Some(mod_env),
        type_overrides,
        depth + 1,
    )
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
    let exact_override = comp
        .type_def_id
        .and_then(|def_id| type_overrides.target_for_alias_def_id(def_id))
        .or_else(|| type_overrides.target_for_name(&comp.type_name));
    // Instance-level package redeclarations in active mod_env are more specific
    // than enclosing-class defaults when resolving dotted member types.
    let mod_env_override =
        resolve_dotted_type_from_mod_env(tree, &comp.type_name, mod_env, type_overrides);
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

    let override_def_id = exact_override.or(prefix_override).or(mod_env_override);
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

fn resolve_dotted_type_from_mod_env(
    tree: &ast::ClassTree,
    type_name: &ast::Name,
    mod_env: Option<&ast::ModificationEnvironment>,
    type_overrides: &TypeOverrideMap,
) -> Option<DefId> {
    let mod_env = mod_env?;
    let (prefix, rest) = name_prefix_and_rest(type_name)?;
    let mv = mod_env.get(&prefix)?;
    let pkg_def_id = resolve_redeclare_value_def_id_with_overrides(
        tree,
        &mv.value,
        Some(mod_env),
        Some(type_overrides),
    )?;
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
fn validate_component_class_redeclare_target(
    tree: &ast::ClassTree,
    target_name: &str,
    nested_class: &ast::ClassDef,
    mod_expr: &ast::Expression,
) -> InstantiateResult<()> {
    let Some(target_ref) = class_redeclare_target_ref(mod_expr) else {
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
    type_overrides: &TypeOverrideMap,
) -> InstantiateResult<ast::ClassOverrideMap> {
    let mut overrides = IndexMap::default();
    let Some(target_class) = target_class else {
        return Ok(overrides);
    };

    for (target_name, mod_expr) in &comp.modifications {
        if class_redeclare_target_ref(mod_expr).is_none() {
            continue;
        }

        insert_class_override_from_component_redeclare(
            tree,
            target_class,
            comp,
            target_name,
            mod_expr,
            mod_env,
            type_overrides,
            &mut overrides,
        )?;
    }

    for (key, mod_expr) in &comp.modifications {
        let Some(target_name) = key.strip_prefix(CONSTRAINEDBY_MOD_PREFIX) else {
            continue;
        };
        let Some(nested_class) =
            find_redeclare_target_class_in_hierarchy(tree, Some(target_class), comp, target_name)
        else {
            continue;
        };
        validate_component_class_redeclare_target(tree, target_name, nested_class, mod_expr)?;
        let Some(alias_def_id) = nested_class.def_id else {
            return Err(Box::new(InstantiateError::redeclare_error(
                target_name,
                "resolved redeclare target has no DefId",
                location_to_span(
                    &nested_class.location,
                    &tree.source_map,
                    "resolved component class redeclare target",
                )?,
            )));
        };
        let resolved_def_id =
            resolve_redeclare_value_def_id(tree, mod_expr, mod_env).or_else(|| {
                class_redeclare_target_ref(mod_expr)
                    .and_then(|target| target.def_id.or_else(|| resolve_cref_def_id(tree, &target)))
            });

        if let Some(def_id) = resolved_def_id {
            overrides.insert(
                alias_def_id,
                ast::ClassOverride::new(
                    target_name.to_string(),
                    alias_def_id,
                    def_id,
                    class_redeclare_target_ref(mod_expr),
                )
                .with_modifier_args(class_redeclare_modifier_args(mod_expr)),
            );
        }
        if class_redeclare_target_ref(mod_expr).is_none() {
            continue;
        }

        insert_class_override_from_component_redeclare(
            tree,
            target_class,
            comp,
            target_name,
            mod_expr,
            mod_env,
            type_overrides,
            &mut overrides,
        )?;
    }

    Ok(overrides)
}

const CONSTRAINEDBY_MOD_PREFIX: &str = "__constrainedby__.";

fn find_redeclare_target_class_in_hierarchy<'a>(
    tree: &'a ast::ClassTree,
    target_class: Option<&'a ast::ClassDef>,
    comp: &ast::Component,
    target_name: &str,
) -> Option<&'a ast::ClassDef> {
    if let Some(target_class) = target_class
        && let Some(nested) = find_nested_class_in_hierarchy(tree, target_class, target_name)
    {
        return Some(nested);
    }

    comp.type_def_id
        .and_then(|def_id| tree.get_class_by_def_id(def_id))
        .and_then(|comp_type| find_nested_class_in_hierarchy(tree, comp_type, target_name))
}

#[allow(clippy::too_many_arguments)]
fn insert_class_override_from_component_redeclare(
    tree: &ast::ClassTree,
    target_class: &ast::ClassDef,
    comp: &ast::Component,
    target_name: &str,
    mod_expr: &ast::Expression,
    mod_env: Option<&ast::ModificationEnvironment>,
    type_overrides: &TypeOverrideMap,
    overrides: &mut ast::ClassOverrideMap,
) -> InstantiateResult<()> {
    let Some(nested_class) =
        find_redeclare_target_class_in_hierarchy(tree, Some(target_class), comp, target_name)
    else {
        return Ok(());
    };
    validate_component_class_redeclare_target(tree, target_name, nested_class, mod_expr)?;
    let Some(alias_def_id) = nested_class.def_id else {
        return Err(Box::new(InstantiateError::redeclare_error(
            target_name,
            "resolved redeclare target has no DefId",
            location_to_span(
                &nested_class.location,
                &tree.source_map,
                "resolved component class redeclare target",
            )?,
        )));
    };
    let resolved_def_id = resolve_redeclare_value_def_id_with_overrides(
        tree,
        mod_expr,
        mod_env,
        Some(type_overrides),
    );

    if let Some(def_id) = resolved_def_id {
        overrides.insert(
            alias_def_id,
            ast::ClassOverride::new(
                target_name.to_string(),
                alias_def_id,
                def_id,
                class_redeclare_target_ref(mod_expr),
            )
            .with_modifier_args(class_redeclare_modifier_args(mod_expr)),
        );
    }

    Ok(())
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

fn class_redeclare_modifier_args(mod_expr: &ast::Expression) -> Vec<ast::Expression> {
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

    #[test]
    fn test_redeclared_nested_type_remaps_inherited_type_def_id() {
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
}
