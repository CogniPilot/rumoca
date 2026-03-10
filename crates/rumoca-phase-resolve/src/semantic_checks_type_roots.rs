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

fn resolve_type_root<'a>(
    type_def_id: Option<DefId>,
    type_name: &ast::Name,
    def: &'a StoredDefinition,
) -> Option<ResolvedTypeRoot<'a>> {
    let mut current_def_id = type_def_id;
    let mut current_name = type_name.to_string();
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
