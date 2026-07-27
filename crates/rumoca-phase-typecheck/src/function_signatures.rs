use rumoca_core::{Causality, ClassType, DefId};
use rumoca_ir_ast::{
    AstIndexMap as IndexMap, ClassDef, ClassOverrideMap, ClassTree, Component, Expression,
    ExtendModification,
};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Default)]
pub(crate) struct FunctionSignature {
    pub(crate) inputs: Vec<(String, Component)>,
    pub(crate) outputs: Vec<(String, Component)>,
}

#[derive(Clone, Default)]
pub(crate) struct CallTypeOverrides {
    by_alias: HashMap<String, HashMap<DefId, DefId>>,
    unqualified: HashMap<DefId, DefId>,
}

impl CallTypeOverrides {
    pub(crate) fn specialized_type(
        &self,
        call: &rumoca_ir_ast::ComponentReference,
        declaration_type: DefId,
    ) -> Option<DefId> {
        let alias_specialization = call
            .parts
            .first()
            .and_then(|part| self.by_alias.get(part.ident.text.as_ref()))
            .and_then(|overrides| overrides.get(&declaration_type));
        alias_specialization
            .or_else(|| self.unqualified.get(&declaration_type))
            .copied()
    }
}

pub(crate) fn build_function_signatures(tree: &ClassTree) -> HashMap<DefId, FunctionSignature> {
    let mut component_cache = HashMap::new();
    let mut visiting = HashSet::new();
    let mut signatures = HashMap::new();

    for def_id in tree.def_map.keys().copied() {
        let Some(class) = tree.get_class_by_def_id(def_id) else {
            continue;
        };
        if !matches!(class.class_type, ClassType::Function) {
            continue;
        }
        let components =
            collect_function_components(tree, def_id, &mut component_cache, &mut visiting);
        let inputs = components
            .iter()
            .filter(|(_, component)| matches!(component.causality, Causality::Input(_)))
            .map(|(name, component)| (name.clone(), component.clone()))
            .collect();
        let outputs = components
            .iter()
            .filter(|(_, component)| matches!(component.causality, Causality::Output(_)))
            .map(|(name, component)| (name.clone(), component.clone()))
            .collect();
        signatures.insert(def_id, FunctionSignature { inputs, outputs });
    }

    signatures
}

pub(crate) fn build_call_type_overrides(
    tree: &ClassTree,
    class_def_id: DefId,
    instance_overrides: Option<&ClassOverrideMap>,
) -> CallTypeOverrides {
    let Some(class) = tree.get_class_by_def_id(class_def_id) else {
        return CallTypeOverrides::default();
    };
    let mut aliases = IndexMap::default();
    let mut visiting = HashSet::new();
    collect_package_aliases(tree, class, &mut aliases, &mut visiting);
    if let Some(instance_overrides) = instance_overrides {
        for class_override in instance_overrides.values() {
            aliases.insert(class_override.alias.clone(), class_override.target_def_id);
        }
    }

    let by_alias = aliases
        .into_iter()
        .filter_map(|(alias, package_def_id)| {
            let overrides = nested_type_specializations(tree, package_def_id);
            (!overrides.is_empty()).then_some((alias, overrides))
        })
        .collect();
    let unqualified = enclosing_package_def_id(tree, class_def_id)
        .map(|package_def_id| nested_type_specializations(tree, package_def_id))
        .unwrap_or_default();
    CallTypeOverrides {
        by_alias,
        unqualified,
    }
}

fn enclosing_package_def_id(tree: &ClassTree, class_def_id: DefId) -> Option<DefId> {
    let qualified_name = tree.def_map.get(&class_def_id)?;
    tree.enclosing_class_names_of(qualified_name)
        .filter_map(|name| tree.get_class_by_qualified_name(name))
        .find(|class| class.class_type == ClassType::Package)
        .and_then(|class| class.def_id)
}

fn collect_package_aliases(
    tree: &ClassTree,
    class: &ClassDef,
    aliases: &mut IndexMap<String, DefId>,
    visiting: &mut HashSet<DefId>,
) {
    let Some(class_def_id) = class.def_id else {
        return;
    };
    if !visiting.insert(class_def_id) {
        return;
    }
    for extend in &class.extends {
        if let Some(base) = extend
            .base_def_id
            .and_then(|def_id| tree.get_class_by_def_id(def_id))
        {
            collect_package_aliases(tree, base, aliases, visiting);
        }
    }
    for (name, nested) in &class.classes {
        if nested.class_type == ClassType::Package
            && let Some(def_id) = nested.def_id
        {
            aliases.insert(name.clone(), def_id);
        }
    }
    visiting.remove(&class_def_id);
}

fn nested_type_specializations(
    tree: &ClassTree,
    effective_package_def_id: DefId,
) -> HashMap<DefId, DefId> {
    let hierarchy = class_hierarchy(tree, effective_package_def_id);
    let mut member_names = IndexMap::<String, ()>::default();
    // MLS §7.3: `extends Base(redeclare record R = Other)` names the replaced
    // slot by its member name, so the extends modification is itself the
    // authoritative statement that `Base.R` and `Other` are the same slot. The
    // structural `declaration_slot_related` walk cannot see that: a record
    // redeclaration only has to be a subtype of the constraining type, which
    // MSL's `Common.BaseProps_Tpoly` satisfies without extending
    // `ThermodynamicState` and without an element-level redeclare target.
    let mut redeclared_targets = HashMap::<String, DefId>::new();
    for class_def_id in &hierarchy {
        let Some(class) = tree.get_class_by_def_id(*class_def_id) else {
            continue;
        };
        for name in class.classes.keys() {
            member_names.entry(name.clone()).or_insert(());
        }
        for (name, target_def_id) in class_redeclared_members(tree, class) {
            member_names.entry(name.clone()).or_insert(());
            redeclared_targets.entry(name).or_insert(target_def_id);
        }
    }

    let mut overrides = HashMap::new();
    for member_name in member_names.keys() {
        let Some(effective_member_def_id) =
            find_effective_nested_class(tree, effective_package_def_id, member_name)
        else {
            continue;
        };
        let redeclared_here = redeclared_targets.get(member_name) == Some(&effective_member_def_id);
        for class_def_id in &hierarchy {
            let Some(class) = tree.get_class_by_def_id(*class_def_id) else {
                continue;
            };
            let Some(member_def_id) = class
                .classes
                .get(member_name)
                .and_then(|member| member.def_id)
            else {
                continue;
            };
            if redeclared_here
                || declaration_slot_related(tree, member_def_id, effective_member_def_id)
            {
                overrides.insert(member_def_id, effective_member_def_id);
            }
        }
    }
    overrides
}

fn class_hierarchy(tree: &ClassTree, root_def_id: DefId) -> Vec<DefId> {
    const MAX_DEPTH: usize = 64;
    let mut hierarchy = Vec::new();
    let mut pending = vec![(root_def_id, 0usize)];
    let mut visited = HashSet::new();
    while let Some((def_id, depth)) = pending.pop() {
        if depth > MAX_DEPTH || !visited.insert(def_id) {
            continue;
        }
        hierarchy.push(def_id);
        let Some(class) = tree.get_class_by_def_id(def_id) else {
            continue;
        };
        pending.extend(
            class
                .extends
                .iter()
                .filter_map(|extend| extend.base_def_id)
                .rev()
                .map(|base_def_id| (base_def_id, depth + 1)),
        );
    }
    hierarchy
}

fn find_effective_nested_class(
    tree: &ClassTree,
    root_def_id: DefId,
    member_name: &str,
) -> Option<DefId> {
    const MAX_DEPTH: usize = 64;
    let mut pending = vec![(root_def_id, 0usize)];
    let mut visited = HashSet::new();
    while let Some((def_id, depth)) = pending.pop() {
        if depth > MAX_DEPTH || !visited.insert(def_id) {
            continue;
        }
        let class = tree.get_class_by_def_id(def_id)?;
        if let Some(target_def_id) = class_redeclared_members(tree, class)
            .into_iter()
            .find_map(|(name, target_def_id)| (name == member_name).then_some(target_def_id))
        {
            return Some(target_def_id);
        }
        if let Some(member_def_id) = class
            .classes
            .get(member_name)
            .and_then(|member| member.def_id)
        {
            return Some(member_def_id);
        }
        pending.extend(
            class
                .extends
                .iter()
                .filter_map(|extend| extend.base_def_id)
                .rev()
                .map(|base_def_id| (base_def_id, depth + 1)),
        );
    }
    None
}

fn class_redeclared_members(tree: &ClassTree, class: &ClassDef) -> Vec<(String, DefId)> {
    class
        .extends
        .iter()
        .flat_map(|extend| &extend.modifications)
        .filter_map(|modification| {
            redeclared_member(tree, class.def_id, modification)
                .map(|(name, target_def_id)| (name.to_string(), target_def_id))
        })
        .collect()
}

fn redeclared_member<'a>(
    tree: &ClassTree,
    origin_def_id: Option<DefId>,
    modification: &'a ExtendModification,
) -> Option<(&'a str, DefId)> {
    if !modification.redeclare {
        return None;
    }
    let Expression::Modification { target, value, .. } = &modification.expr else {
        return None;
    };
    let name = target.parts.first()?.ident.text.as_ref();
    expression_class_def_id(tree, origin_def_id, value.as_ref()).map(|def_id| (name, def_id))
}

fn expression_class_def_id(
    tree: &ClassTree,
    origin_def_id: Option<DefId>,
    expression: &Expression,
) -> Option<DefId> {
    match expression {
        Expression::Modification { value, .. } => {
            expression_class_def_id(tree, origin_def_id, value)
        }
        Expression::ComponentReference(reference) => {
            resolve_class_reference(tree, origin_def_id, reference)
        }
        Expression::ClassModification { target, .. } => {
            resolve_class_reference(tree, origin_def_id, target)
        }
        Expression::FunctionCall { comp, .. } => resolve_class_reference(tree, origin_def_id, comp),
        _ => None,
    }
}

fn resolve_class_reference(
    tree: &ClassTree,
    origin_def_id: Option<DefId>,
    reference: &rumoca_ir_ast::ComponentReference,
) -> Option<DefId> {
    if let Some(class) = tree.get_class_by_qualified_name(&reference.to_string()) {
        return class.def_id;
    }
    resolve_class_reference_from_root(tree, reference)
        .or_else(|| resolve_class_reference_lexically(tree, origin_def_id?, reference))
}

fn resolve_class_reference_from_root(
    tree: &ClassTree,
    reference: &rumoca_ir_ast::ComponentReference,
) -> Option<DefId> {
    let mut current = reference.def_id?;
    if reference.parts.len() == 1 {
        return tree
            .get_class_by_def_id(current)
            .and_then(|class| class.def_id);
    }
    for part in reference.parts.iter().skip(1) {
        current = find_effective_nested_class(tree, current, part.ident.text.as_ref())?;
    }
    Some(current)
}

/// MLS §5.3.2: a composite name written in a modification is resolved by
/// looking its first identifier up in the lexically enclosing scopes,
/// innermost first, and then following the remaining identifiers through the
/// nested classes of whatever that names.
///
/// `redeclare record ThermodynamicState = Common.BaseProps_Tpoly` in
/// `Modelica.Media.Incompressible.TableBased` therefore names
/// `Modelica.Media.Incompressible.Common.BaseProps_Tpoly`. Neither a
/// root-relative lookup of the written name nor the reference's own resolved
/// `DefId` reaches it, so without this walk the redeclaration is silently not
/// recorded and every function whose declared output type is the replaced
/// slot keeps the constraining type.
fn resolve_class_reference_lexically(
    tree: &ClassTree,
    origin_def_id: DefId,
    reference: &rumoca_ir_ast::ComponentReference,
) -> Option<DefId> {
    let first = reference.parts.first()?.ident.text.as_ref();
    let origin_name = tree.def_map.get(&origin_def_id)?;
    let scope_names = std::iter::once(origin_name.as_str())
        .chain(tree.enclosing_class_names_of(origin_name))
        .collect::<Vec<_>>();
    for scope_name in scope_names {
        let Some(scope_def_id) = tree
            .get_class_by_qualified_name(scope_name)
            .and_then(|class| class.def_id)
        else {
            continue;
        };
        let Some(resolved) = follow_nested_class_path(tree, scope_def_id, first, reference) else {
            continue;
        };
        return Some(resolved);
    }
    None
}

fn follow_nested_class_path(
    tree: &ClassTree,
    scope_def_id: DefId,
    first: &str,
    reference: &rumoca_ir_ast::ComponentReference,
) -> Option<DefId> {
    let mut current = find_declared_nested_class(tree, scope_def_id, first)?;
    for part in reference.parts.iter().skip(1) {
        current = find_declared_nested_class(tree, current, part.ident.text.as_ref())?;
    }
    Some(current)
}

/// Nested-class lookup that reads only declarations and inheritance, never
/// redeclarations. The lexical walk that resolves a redeclaration's target
/// name must not itself consult redeclarations: resolving
/// `redeclare record R = Common.X` inside class `C` would look `Common` up in
/// `C`, which would re-enter the very modification being resolved.
fn find_declared_nested_class(
    tree: &ClassTree,
    root_def_id: DefId,
    member_name: &str,
) -> Option<DefId> {
    const MAX_DEPTH: usize = 64;
    let mut pending = vec![(root_def_id, 0usize)];
    let mut visited = HashSet::new();
    while let Some((def_id, depth)) = pending.pop() {
        if depth > MAX_DEPTH || !visited.insert(def_id) {
            continue;
        }
        let Some(class) = tree.get_class_by_def_id(def_id) else {
            continue;
        };
        if let Some(member_def_id) = class
            .classes
            .get(member_name)
            .and_then(|member| member.def_id)
        {
            return Some(member_def_id);
        }
        pending.extend(
            class
                .extends
                .iter()
                .filter_map(|extend| extend.base_def_id)
                .rev()
                .map(|base_def_id| (base_def_id, depth + 1)),
        );
    }
    None
}

fn declaration_slot_related(tree: &ClassTree, left: DefId, right: DefId) -> bool {
    left == right
        || class_identity_reaches(tree, left, right)
        || class_identity_reaches(tree, right, left)
}

fn class_identity_reaches(tree: &ClassTree, root_def_id: DefId, target_def_id: DefId) -> bool {
    const MAX_DEPTH: usize = 64;
    let mut pending = vec![(root_def_id, 0usize)];
    let mut visited = HashSet::new();
    while let Some((def_id, depth)) = pending.pop() {
        if def_id == target_def_id {
            return true;
        }
        if depth > MAX_DEPTH || !visited.insert(def_id) {
            continue;
        }
        let Some(class) = tree.get_class_by_def_id(def_id) else {
            continue;
        };
        if let Some(redeclare_target_def_id) = class.redeclare_target_def_id {
            pending.push((redeclare_target_def_id, depth + 1));
        }
        pending.extend(
            class
                .extends
                .iter()
                .filter_map(|extend| extend.base_def_id)
                .map(|base_def_id| (base_def_id, depth + 1)),
        );
    }
    false
}

fn collect_function_components(
    tree: &ClassTree,
    def_id: DefId,
    cache: &mut HashMap<DefId, IndexMap<String, Component>>,
    visiting: &mut HashSet<DefId>,
) -> IndexMap<String, Component> {
    if let Some(existing) = cache.get(&def_id) {
        return existing.clone();
    }
    if !visiting.insert(def_id) {
        return IndexMap::default();
    }
    let Some(class) = tree.get_class_by_def_id(def_id) else {
        visiting.remove(&def_id);
        return IndexMap::default();
    };

    let mut components = IndexMap::default();
    let base_ids = class
        .extends
        .iter()
        .filter_map(|extend| extend.base_def_id)
        .chain(class.redeclare_target_def_id);
    for base_def_id in base_ids {
        for (name, component) in collect_function_components(tree, base_def_id, cache, visiting) {
            components.insert(name, component);
        }
    }
    for extend in &class.extends {
        for break_name in &extend.break_names {
            components.shift_remove(break_name);
        }
    }
    for (name, component) in &class.components {
        components.insert(name.clone(), component.clone());
    }

    visiting.remove(&def_id);
    cache.insert(def_id, components.clone());
    components
}
