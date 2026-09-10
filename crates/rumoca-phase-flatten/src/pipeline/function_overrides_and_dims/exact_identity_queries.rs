//! Exact identity queries over the resolved class index: function
//! exposures, implementations, and package inheritance by `DefId`.

use super::*;

pub(super) fn exact_prefix_owner_def_id(
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    prefix_def_id: rumoca_core::DefId,
) -> Option<rumoca_core::DefId> {
    if class_index.get(prefix_def_id).is_some() {
        return Some(prefix_def_id);
    }
    let declaration_owner = class_index.parent_def_id(prefix_def_id)?;
    let component = class_index
        .get(declaration_owner)?
        .components
        .values()
        .find(|component| component.def_id == Some(prefix_def_id))?;
    component.type_def_id.or(component.type_name.def_id)
}

pub(super) fn collect_function_exposures_for_implementation(
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    owner: rumoca_core::DefId,
    implementation: rumoca_core::DefId,
    visited_owners: &mut FxHashSet<rumoca_core::DefId>,
    exposures: &mut FxHashSet<rumoca_core::DefId>,
) {
    if !visited_owners.insert(owner) {
        return;
    }
    let Some(class_def) = class_index.get(owner) else {
        return;
    };
    for nested in class_def
        .classes
        .values()
        .filter(|nested| nested.class_type == rumoca_core::ClassType::Function)
    {
        let Some(exposure) = nested.def_id else {
            continue;
        };
        let selected = if exposure == implementation {
            Some(exposure)
        } else {
            resolve_function_extends_target_def_id(class_index, exposure)
        };
        if selected == Some(implementation) {
            exposures.insert(exposure);
        }
    }
    for base in class_def
        .extends
        .iter()
        .filter_map(|extend| extend.base_def_id)
    {
        collect_function_exposures_for_implementation(
            class_index,
            base,
            implementation,
            visited_owners,
            exposures,
        );
    }
}

pub(super) fn resolve_function_extends_target_def_id(
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    exposure: rumoca_core::DefId,
) -> Option<rumoca_core::DefId> {
    let mut current = exposure;
    let mut visited = FxHashSet::default();

    loop {
        if !visited.insert(current) {
            return None;
        }
        let class_def = class_index.get(current)?;
        if class_def.class_type != rumoca_core::ClassType::Function {
            return None;
        }
        if !class_def.algorithms.is_empty() || class_def.external.is_some() {
            return (current != exposure).then_some(current);
        }
        let mut candidates = class_def.extends.iter().filter_map(|ext| {
            let target = ext.base_def_id?;
            (class_index.get(target)?.class_type == rumoca_core::ClassType::Function)
                .then_some(target)
        });
        let Some(candidate) = candidates.next() else {
            return (current != exposure).then_some(current);
        };
        if candidates.any(|other| other != candidate) {
            return None;
        }
        current = candidate;
    }
}

pub(super) fn exact_package_chain_contains_def_id(
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    package: rumoca_core::DefId,
    query: rumoca_core::DefId,
    visited: &mut FxHashSet<rumoca_core::DefId>,
) -> Result<bool, &'static str> {
    if package == query {
        return Ok(true);
    }
    if !visited.insert(package) {
        return Err("package inheritance contains a cycle");
    }
    let class_def = class_index
        .get(package)
        .ok_or("selected package DefId is absent from the resolved class index")?;
    for extend in &class_def.extends {
        let base = extend
            .base_def_id
            .or(extend.base_name.def_id)
            .ok_or("selected package base has no resolved DefId")?;
        if exact_package_chain_contains_def_id(class_index, base, query, visited)? {
            visited.remove(&package);
            return Ok(true);
        }
    }
    visited.remove(&package);
    Ok(false)
}

pub(super) fn exact_function_member_name(
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    owner: rumoca_core::DefId,
    exposure: rumoca_core::DefId,
) -> Result<Option<String>, &'static str> {
    let class_def = class_index
        .get(owner)
        .ok_or("function exposure owner is absent from the resolved class index")?;
    let mut matches = class_def
        .classes
        .iter()
        .filter(|(_, nested)| nested.def_id == Some(exposure))
        .map(|(name, _)| name.clone());
    let name = matches.next();
    if matches.next().is_some() {
        return Err("function exposure owner contains duplicate exact DefId slots");
    }
    Ok(name)
}

pub(super) fn exact_package_function_exposure(
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    package: rumoca_core::DefId,
    member: &str,
    visited: &mut FxHashSet<rumoca_core::DefId>,
) -> Result<Option<rumoca_core::DefId>, &'static str> {
    if !visited.insert(package) {
        return Err("package inheritance contains a cycle");
    }
    let class_def = class_index
        .get(package)
        .ok_or("selected package DefId is absent from the resolved class index")?;
    if let Some(nested) = class_def.classes.get(member) {
        if nested.class_type != rumoca_core::ClassType::Function {
            return Err("selected package member is not a function");
        }
        let exposure = nested
            .def_id
            .map(Some)
            .ok_or("selected package function has no resolved DefId")?;
        visited.remove(&package);
        return Ok(exposure);
    }

    let mut selected = None;
    for extend in &class_def.extends {
        let base = extend
            .base_def_id
            .or(extend.base_name.def_id)
            .ok_or("selected package base has no resolved DefId")?;
        let Some(candidate) = exact_package_function_exposure(class_index, base, member, visited)?
        else {
            continue;
        };
        match selected {
            Some(existing) if existing != candidate => {
                return Err("selected package inherits multiple exact function exposures");
            }
            Some(_) => {}
            None => selected = Some(candidate),
        }
    }
    visited.remove(&package);
    Ok(selected)
}

pub(super) fn function_alias_requires_exact_selection(class_def: &rumoca_ir_ast::ClassDef) -> bool {
    class_def.class_type == rumoca_core::ClassType::Function
        && class_def.algorithms.is_empty()
        && class_def.external.is_none()
        && !class_def.extends.is_empty()
}
