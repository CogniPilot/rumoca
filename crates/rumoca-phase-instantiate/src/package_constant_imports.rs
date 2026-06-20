use rumoca_ir_ast as ast;

pub(super) fn resolved_imports_with_active_package_constants(
    tree: &ast::ClassTree,
    base_imports: &[(String, String)],
    active_aliases: &[(String, rumoca_core::DefId)],
) -> Vec<(String, String)> {
    let mut imports = base_imports.to_vec();
    for (_, target_def_id) in active_aliases {
        append_type_override_constant_imports(tree, *target_def_id, &mut imports);
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
    // MLS §5.3.2: members declared by the nested class (or its bases) shadow
    // enclosing-class names; an enclosing-class alias for them would
    // misresolve local references to the enclosing instance's member.
    let shadowed = class_member_names_with_bases(tree, class);
    append_package_constant_imports_shadowed(
        tree,
        parent_class,
        parent_name,
        imports,
        &shadowed,
        false,
    );
}

fn class_member_names_with_bases(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
) -> rustc_hash::FxHashSet<String> {
    let mut names = rustc_hash::FxHashSet::default();
    let mut visited = rustc_hash::FxHashSet::default();
    collect_class_member_names_recursive(tree, class, &mut names, &mut visited);
    names
}

fn collect_class_member_names_recursive(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    names: &mut rustc_hash::FxHashSet<String>,
    visited: &mut rustc_hash::FxHashSet<rumoca_core::DefId>,
) {
    if let Some(def_id) = class.def_id
        && !visited.insert(def_id)
    {
        return;
    }
    names.extend(class.components.keys().cloned());
    for ext in &class.extends {
        let base_class = ext
            .base_def_id
            .and_then(|def_id| tree.get_class_by_def_id(def_id))
            .or_else(|| tree.get_class_by_qualified_name(&ext.base_name.to_string()));
        if let Some(base_class) = base_class {
            collect_class_member_names_recursive(tree, base_class, names, visited);
        }
    }
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
    target_def_id: rumoca_core::DefId,
    imports: &mut Vec<(String, String)>,
) {
    let Some(package_class) = tree.get_class_by_def_id(target_def_id) else {
        return;
    };
    if !matches!(package_class.class_type, rumoca_core::ClassType::Package) {
        return;
    }
    let Some(package_name) = tree.def_map.get(&target_def_id) else {
        return;
    };
    append_package_constant_imports(tree, package_class, package_name, imports, true);
}

fn append_package_constant_imports(
    tree: &ast::ClassTree,
    package_class: &ast::ClassDef,
    package_name: &str,
    imports: &mut Vec<(String, String)>,
    replace_existing: bool,
) {
    append_package_constant_imports_shadowed(
        tree,
        package_class,
        package_name,
        imports,
        &rustc_hash::FxHashSet::default(),
        replace_existing,
    );
}

fn append_package_constant_imports_shadowed(
    tree: &ast::ClassTree,
    package_class: &ast::ClassDef,
    package_name: &str,
    imports: &mut Vec<(String, String)>,
    shadowed: &rustc_hash::FxHashSet<String>,
    replace_existing: bool,
) {
    let mut package_constant_names = Vec::new();
    let mut visited = rustc_hash::FxHashSet::default();
    let options = PackageConstantImportOptions {
        shadowed,
        replace_existing,
    };
    append_package_constant_imports_recursive(
        tree,
        package_class,
        package_name,
        imports,
        &mut package_constant_names,
        &mut visited,
        options,
    );
}

#[derive(Clone, Copy)]
struct PackageConstantImportOptions<'a> {
    shadowed: &'a rustc_hash::FxHashSet<String>,
    replace_existing: bool,
}

fn append_package_constant_imports_recursive(
    tree: &ast::ClassTree,
    package_class: &ast::ClassDef,
    package_name: &str,
    imports: &mut Vec<(String, String)>,
    package_constant_names: &mut Vec<String>,
    visited: &mut rustc_hash::FxHashSet<rumoca_core::DefId>,
    options: PackageConstantImportOptions<'_>,
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
                options,
            );
        }
    }
    // MLS §5.3.2: enclosing-scope lookup reaches class constants (and, for
    // package enclosers, package members). A non-package enclosing class's
    // parameters are instance members and must never become class-qualified
    // alias targets.
    let is_package = matches!(package_class.class_type, rumoca_core::ClassType::Package);
    for (name, component) in &package_class.components {
        let alias_visible = match component.variability {
            rumoca_core::Variability::Constant(_) => true,
            rumoca_core::Variability::Parameter(_) => is_package,
            _ => false,
        };
        if alias_visible {
            if options.shadowed.contains(name) || package_constant_names.contains(name) {
                continue;
            }
            package_constant_names.push(name.clone());
            if imports.iter().any(|(candidate, _)| candidate == name) && !options.replace_existing {
                continue;
            }
            imports.retain(|(candidate, _)| candidate != name);
            imports.push((name.clone(), format!("{package_name}.{name}")));
        }
    }
}
