use rumoca_ir_ast as ast;

pub(super) fn resolved_imports_with_active_package_constants(
    tree: &ast::ClassTree,
    base_imports: &[(String, String)],
    active_aliases: &[(String, rumoca_core::DefId)],
) -> Vec<(String, String)> {
    let mut imports = base_imports.to_vec();
    for (alias, target_def_id) in active_aliases {
        append_type_override_constant_imports(tree, alias, *target_def_id, &mut imports);
    }
    imports
}

pub(super) fn resolved_imports_with_enclosing_package_constants(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    base_imports: &[(String, String)],
) -> Vec<(String, String)> {
    let mut imports = base_imports.to_vec();
    append_enclosing_package_constant_imports(tree, class, &mut imports);
    imports
}

fn append_enclosing_package_constant_imports(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    imports: &mut Vec<(String, String)>,
) {
    let Some(class_def_id) = class.def_id else {
        return;
    };
    let Some(parent_def_id) = enclosing_class_def_id(tree, class_def_id) else {
        return;
    };
    let Some(parent_name) = tree.def_map.get(&parent_def_id) else {
        return;
    };
    let Some(parent_class) = tree.get_class_by_def_id(parent_def_id) else {
        return;
    };
    append_package_constant_imports(tree, parent_class, parent_name, imports);
}

fn enclosing_class_def_id(
    tree: &ast::ClassTree,
    class_def_id: rumoca_core::DefId,
) -> Option<rumoca_core::DefId> {
    tree.definitions
        .classes
        .values()
        .find_map(|class| enclosing_class_def_id_in_class(class, None, class_def_id))
}

fn enclosing_class_def_id_in_class(
    class: &ast::ClassDef,
    parent_def_id: Option<rumoca_core::DefId>,
    target_def_id: rumoca_core::DefId,
) -> Option<rumoca_core::DefId> {
    if class.def_id == Some(target_def_id) {
        return parent_def_id;
    }
    class
        .classes
        .values()
        .find_map(|nested| enclosing_class_def_id_in_class(nested, class.def_id, target_def_id))
}

fn append_type_override_constant_imports(
    tree: &ast::ClassTree,
    alias: &str,
    target_def_id: rumoca_core::DefId,
    imports: &mut Vec<(String, String)>,
) {
    let Some(package_class) = tree.get_class_by_def_id(target_def_id) else {
        return;
    };
    if !matches!(package_class.class_type, rumoca_core::ClassType::Package) {
        return;
    }
    append_package_constant_imports(tree, package_class, alias, imports);
}

fn append_package_constant_imports(
    tree: &ast::ClassTree,
    package_class: &ast::ClassDef,
    package_name: &str,
    imports: &mut Vec<(String, String)>,
) {
    let mut package_constant_names = Vec::new();
    let mut visited = rustc_hash::FxHashSet::default();
    append_package_constant_imports_recursive(
        tree,
        package_class,
        package_name,
        imports,
        &mut package_constant_names,
        &mut visited,
    );
}

fn append_package_constant_imports_recursive(
    tree: &ast::ClassTree,
    package_class: &ast::ClassDef,
    package_name: &str,
    imports: &mut Vec<(String, String)>,
    package_constant_names: &mut Vec<String>,
    visited: &mut rustc_hash::FxHashSet<rumoca_core::DefId>,
) {
    if let Some(def_id) = package_class.def_id
        && !visited.insert(def_id)
    {
        return;
    }
    for ext in &package_class.extends {
        let base_class = ext
            .base_def_id
            .and_then(|def_id| tree.get_class_by_def_id(def_id))
            .or_else(|| tree.get_class_by_qualified_name(&ext.base_name.to_string()));
        if let Some(base_class) = base_class {
            append_package_constant_imports_recursive(
                tree,
                base_class,
                package_name,
                imports,
                package_constant_names,
                visited,
            );
        }
    }
    for (name, component) in &package_class.components {
        if matches!(
            component.variability,
            rumoca_core::Variability::Constant(_) | rumoca_core::Variability::Parameter(_)
        ) {
            if package_constant_names.contains(name) {
                continue;
            }
            package_constant_names.push(name.clone());
            imports.retain(|(candidate, _)| candidate != name);
            imports.push((name.clone(), format!("{package_name}.{name}")));
        }
    }
}
