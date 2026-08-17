use super::*;

pub(super) enum ResolvedTypeRoot<'a> {
    Builtin(&'static str),
    Class(&'a ClassDef),
}

pub(super) fn resolve_component_type_root<'a>(
    comp: &'a ast::Component,
    def: &'a StoredDefinition,
) -> Option<ResolvedTypeRoot<'a>> {
    resolve_type_root(comp.type_def_id, &comp.type_name, def)
}

/// Resolve the type constraint that every legal selection of a deferred,
/// replaceable-package member must satisfy.
///
/// This is deliberately separate from `resolve_component_type_root`: it
/// proves an interface property without claiming the instance-selected member
/// identity that Instantiate owns.
pub(super) fn resolve_component_interface_type_root<'a>(
    comp: &'a ast::Component,
    def: &'a StoredDefinition,
) -> Option<ResolvedTypeRoot<'a>> {
    if let Some(root) = resolve_component_type_root(comp, def) {
        return Some(root);
    }
    if comp.type_def_id.is_some() || comp.type_name.name.len() < 2 {
        return None;
    }

    let root = find_class_by_def_id(def, comp.type_name.def_id?)?;
    if !root.is_replaceable {
        return None;
    }
    let mut interface = replaceable_interface_class(root, def)?;
    let tail = &comp.type_name.name[1..];
    for (index, segment) in tail.iter().enumerate() {
        let member = find_unique_interface_member(interface, segment.text.as_ref(), def)?;
        if index + 1 == tail.len() {
            return resolve_declared_type_constraint_root(member, def);
        }
        interface = replaceable_interface_class(member, def)?;
    }
    None
}

pub(super) fn resolve_named_type_root<'a>(
    type_def_id: Option<DefId>,
    type_name: &ast::Name,
    def: &'a StoredDefinition,
) -> Option<ResolvedTypeRoot<'a>> {
    resolve_type_root(type_def_id, type_name, def)
}

fn resolve_type_root<'a>(
    type_def_id: Option<DefId>,
    type_name: &ast::Name,
    def: &'a StoredDefinition,
) -> Option<ResolvedTypeRoot<'a>> {
    resolve_type_root_from(type_def_id, type_name.to_string(), def)
}

fn resolve_type_root_from<'a>(
    mut current_def_id: Option<DefId>,
    mut current_name: String,
    def: &'a StoredDefinition,
) -> Option<ResolvedTypeRoot<'a>> {
    let mut seen = HashSet::new();

    loop {
        let Some(def_id) = current_def_id else {
            return builtin_type_root_name(&current_name).map(ResolvedTypeRoot::Builtin);
        };
        if !seen.insert(def_id) {
            return None;
        }

        let Some(class) = find_class_by_def_id(def, def_id) else {
            return builtin_type_root_name(&current_name).map(ResolvedTypeRoot::Builtin);
        };
        if class.class_type != ClassType::Type || !class.enum_literals.is_empty() {
            return Some(ResolvedTypeRoot::Class(class));
        }

        let Some(ext) = class.extends.first() else {
            return Some(ResolvedTypeRoot::Class(class));
        };
        current_name = ext.base_name.to_string();
        current_def_id = ext.base_def_id;
    }
}

fn resolve_class_type_root<'a>(
    class: &'a ClassDef,
    def: &'a StoredDefinition,
) -> Option<ResolvedTypeRoot<'a>> {
    resolve_type_root_from(class.def_id, class.name.text.to_string(), def)
}

fn resolve_declared_type_constraint_root<'a>(
    class: &'a ClassDef,
    def: &'a StoredDefinition,
) -> Option<ResolvedTypeRoot<'a>> {
    if class.is_replaceable
        && let Some(constrainedby) = &class.constrainedby
    {
        return resolve_named_type_root(constrainedby.def_id, constrainedby, def);
    }
    resolve_class_type_root(class, def)
}

fn replaceable_interface_class<'a>(
    class: &'a ClassDef,
    def: &'a StoredDefinition,
) -> Option<&'a ClassDef> {
    if class.is_replaceable
        && let Some(constrainedby) = &class.constrainedby
    {
        return find_class_by_def_id(def, constrainedby.def_id?);
    }
    Some(class)
}

fn find_unique_interface_member<'a>(
    class: &'a ClassDef,
    member_name: &str,
    def: &'a StoredDefinition,
) -> Option<&'a ClassDef> {
    let mut candidates = HashSet::new();
    let mut visiting = HashSet::new();
    let mut visited = HashSet::new();
    if !collect_interface_member_ids(
        class,
        member_name,
        def,
        &mut candidates,
        &mut visiting,
        &mut visited,
    ) || candidates.len() != 1
    {
        return None;
    }
    find_class_by_def_id(def, *candidates.iter().next()?)
}

fn collect_interface_member_ids(
    class: &ClassDef,
    member_name: &str,
    def: &StoredDefinition,
    candidates: &mut HashSet<DefId>,
    visiting: &mut HashSet<DefId>,
    visited: &mut HashSet<DefId>,
) -> bool {
    let Some(class_id) = class.def_id else {
        return false;
    };
    if visited.contains(&class_id) {
        return true;
    }
    if !visiting.insert(class_id) {
        return false;
    }

    let complete =
        collect_interface_member_ids_inner(class, member_name, def, candidates, visiting, visited);
    visiting.remove(&class_id);
    if complete {
        visited.insert(class_id);
    }
    complete
}

fn collect_interface_member_ids_inner(
    class: &ClassDef,
    member_name: &str,
    def: &StoredDefinition,
    candidates: &mut HashSet<DefId>,
    visiting: &mut HashSet<DefId>,
    visited: &mut HashSet<DefId>,
) -> bool {
    if let Some(member) = class.classes.get(member_name) {
        let Some(member_id) = member.def_id else {
            return false;
        };
        candidates.insert(member_id);
        return true;
    }

    for ext in &class.extends {
        if ext.break_names.iter().any(|name| name == member_name) {
            continue;
        }
        let Some(base) = ext
            .base_def_id
            .and_then(|base_id| find_class_by_def_id(def, base_id))
        else {
            return false;
        };
        if !collect_interface_member_ids(base, member_name, def, candidates, visiting, visited) {
            return false;
        }
    }
    true
}

fn builtin_type_root_name(name: &str) -> Option<&'static str> {
    match name {
        "Real" => Some("Real"),
        "Integer" => Some("Integer"),
        "Boolean" => Some("Boolean"),
        "String" => Some("String"),
        "Clock" => Some("Clock"),
        _ => None,
    }
}
