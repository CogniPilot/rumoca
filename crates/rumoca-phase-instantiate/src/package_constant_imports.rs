use super::inheritance::try_extract_value_modification_any;
use crate::{InstantiateError, InstantiateResult, location_to_span};
use rumoca_eval_ast::eval_instantiate::component_expr_for_structural_eval;
use rumoca_ir_ast as ast;

pub(super) fn resolved_imports_with_active_package_constants(
    tree: &ast::ClassTree,
    base_imports: &[(String, String)],
    active_aliases: &[(String, rumoca_core::DefId)],
) -> InstantiateResult<Vec<(String, String)>> {
    let mut imports = base_imports.to_vec();
    for (alias, target_def_id) in active_aliases {
        append_type_override_constant_imports(tree, alias, *target_def_id, &mut imports)?;
    }
    Ok(imports)
}

pub(super) fn active_package_constant_modifications(
    tree: &ast::ClassTree,
    active_aliases: &[(String, rumoca_core::DefId)],
) -> InstantiateResult<Vec<(ast::QualifiedName, ast::ModificationValue)>> {
    let mut values = Vec::new();
    for (_, target_def_id) in active_aliases {
        append_package_constant_modifications(tree, *target_def_id, &mut values)?;
    }
    Ok(values)
}

type PackageConstantModificationSnapshot =
    ast::AstIndexMap<ast::QualifiedName, Option<ast::ModificationValue>>;

pub(super) fn insert_active_package_constant_modifications(
    tree: &ast::ClassTree,
    active_aliases: &[(String, rumoca_core::DefId)],
    mod_env: &mut ast::ModificationEnvironment,
) -> InstantiateResult<PackageConstantModificationSnapshot> {
    let mut snapshot = ast::AstIndexMap::default();
    for (target, value) in active_package_constant_modifications(tree, active_aliases)? {
        snapshot
            .entry(target.clone())
            .or_insert_with(|| mod_env.active.get(&target).cloned());
        mod_env.add(target, value);
    }
    Ok(snapshot)
}

pub(super) fn remove_inserted_package_constant_modifications(
    mod_env: &mut ast::ModificationEnvironment,
    snapshot: PackageConstantModificationSnapshot,
) {
    for (target, previous_value) in snapshot {
        match previous_value {
            Some(value) => {
                mod_env.active.insert(target, value);
            }
            None => {
                mod_env.active.shift_remove(&target);
            }
        }
    }
}

fn append_package_constant_modifications(
    tree: &ast::ClassTree,
    target_def_id: rumoca_core::DefId,
    values: &mut Vec<(ast::QualifiedName, ast::ModificationValue)>,
) -> InstantiateResult<()> {
    let Some(package_class) = tree.get_class_by_def_id(target_def_id) else {
        return Err(Box::new(InstantiateError::structural_param_error(
            format!("active package alias {target_def_id:?}"),
            "target package class not found",
            rumoca_core::Span::DUMMY,
        )));
    };
    if !matches!(package_class.class_type, rumoca_core::ClassType::Package) {
        return Ok(());
    }
    let mut effective_constants = ast::AstIndexMap::default();
    let mut visited = rustc_hash::FxHashSet::default();
    collect_package_constant_components(
        tree,
        package_class,
        &mut effective_constants,
        &mut visited,
    )?;
    for (name, component) in effective_constants {
        let Some(value_expr) = component_expr_for_structural_eval(&component) else {
            continue;
        };
        values.push((
            ast::QualifiedName::from_ident(&name),
            ast::ModificationValue::simple(value_expr.clone()),
        ));
    }
    Ok(())
}

fn collect_package_constant_components(
    tree: &ast::ClassTree,
    package_class: &ast::ClassDef,
    constants: &mut ast::AstIndexMap<String, ast::Component>,
    visited: &mut rustc_hash::FxHashSet<rumoca_core::DefId>,
) -> InstantiateResult<()> {
    if let Some(def_id) = package_class.def_id
        && !visited.insert(def_id)
    {
        return Ok(());
    }

    for ext in &package_class.extends {
        let base_class = resolve_extends_base_class(tree, ext)?;
        collect_package_constant_components(tree, base_class, constants, visited)?;
        for break_name in &ext.break_names {
            constants.shift_remove(break_name);
        }
        apply_extends_constant_modifications(constants, ext, tree)?;
    }

    for (name, component) in &package_class.components {
        if matches!(
            component.variability,
            rumoca_core::Variability::Constant(_) | rumoca_core::Variability::Parameter(_)
        ) {
            constants.insert(name.clone(), component.clone());
        }
    }

    Ok(())
}

fn resolve_extends_base_class<'tree>(
    tree: &'tree ast::ClassTree,
    ext: &ast::Extend,
) -> InstantiateResult<&'tree ast::ClassDef> {
    ext.base_def_id
        .and_then(|def_id| tree.get_class_by_def_id(def_id))
        .or_else(|| tree.get_class_by_qualified_name(&ext.base_name.to_string()))
        .ok_or_else(|| {
            Box::new(InstantiateError::structural_param_error(
                ext.base_name.to_string(),
                "package constant base class not found",
                location_to_span(&ext.location, &tree.source_map),
            ))
        })
}

fn apply_extends_constant_modifications(
    constants: &mut ast::AstIndexMap<String, ast::Component>,
    ext: &ast::Extend,
    tree: &ast::ClassTree,
) -> InstantiateResult<()> {
    let span = location_to_span(&ext.location, &tree.source_map);
    for modification in &ext.modifications {
        let Some((name, value, is_final)) = try_extract_value_modification_any(modification, ext)
        else {
            continue;
        };
        let Some(component) = constants.get_mut(&name) else {
            continue;
        };
        if component.is_final {
            return Err(Box::new(InstantiateError::redeclare_final(name, span)));
        }
        component.binding = Some(value);
        component.has_explicit_binding = true;
        if is_final {
            component.is_final = true;
        }
    }
    Ok(())
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
) -> InstantiateResult<()> {
    let Some(package_class) = tree.get_class_by_def_id(target_def_id) else {
        return Err(Box::new(InstantiateError::structural_param_error(
            format!("active package alias {target_def_id:?}"),
            "target package class not found",
            rumoca_core::Span::DUMMY,
        )));
    };
    if !matches!(package_class.class_type, rumoca_core::ClassType::Package) {
        return Ok(());
    }
    append_package_constant_imports(tree, package_class, alias, imports);
    Ok(())
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    fn token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: Arc::from(text),
            ..Default::default()
        }
    }

    fn name(text: &str) -> ast::Name {
        ast::Name {
            name: vec![token(text)],
            def_id: None,
        }
    }

    fn bool_expr(value: bool) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token: token(if value { "true" } else { "false" }),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn modification(target: &str, value: ast::Expression) -> ast::ExtendModification {
        ast::ExtendModification {
            expr: ast::Expression::Modification {
                target: ast::ComponentReference {
                    local: false,
                    parts: vec![ast::ComponentRefPart {
                        ident: token(target),
                        subs: None,
                    }],
                    def_id: None,
                    span: rumoca_core::Span::DUMMY,
                },
                value: Arc::new(value),
                span: rumoca_core::Span::DUMMY,
            },
            final_: true,
            ..Default::default()
        }
    }

    #[test]
    fn active_package_constant_modifications_use_effective_extends_values() {
        let base_id = rumoca_core::DefId::new(10);
        let derived_id = rumoca_core::DefId::new(11);

        let mut base = ast::ClassDef {
            name: token("Base"),
            def_id: Some(base_id),
            class_type: rumoca_core::ClassType::Package,
            ..Default::default()
        };
        base.components.insert(
            "ph_explicit".to_string(),
            ast::Component {
                name: "ph_explicit".to_string(),
                type_name: name("Boolean"),
                variability: rumoca_core::Variability::Constant(token("constant")),
                ..Default::default()
            },
        );

        let derived = ast::ClassDef {
            name: token("Derived"),
            def_id: Some(derived_id),
            class_type: rumoca_core::ClassType::Package,
            extends: vec![ast::Extend {
                base_name: name("Base"),
                base_def_id: Some(base_id),
                modifications: vec![modification("ph_explicit", bool_expr(true))],
                ..Default::default()
            }],
            ..Default::default()
        };

        let mut tree = ast::ClassTree::default();
        tree.definitions.classes.insert("Base".to_string(), base);
        tree.definitions
            .classes
            .insert("Derived".to_string(), derived);
        tree.def_map.insert(base_id, "Base".to_string());
        tree.def_map.insert(derived_id, "Derived".to_string());
        tree.name_map.insert("Base".to_string(), base_id);
        tree.name_map.insert("Derived".to_string(), derived_id);

        let values =
            active_package_constant_modifications(&tree, &[("Medium".to_string(), derived_id)])
                .expect("active package constants should resolve");

        let ph_explicit = values
            .iter()
            .find(|(target, _)| target.to_flat_string() == "ph_explicit")
            .expect("package constant should be injected");
        assert_eq!(ph_explicit.1.value, bool_expr(true));
    }

    #[test]
    fn active_package_constant_modifications_reject_missing_target() {
        let missing_id = rumoca_core::DefId::new(404);
        let tree = ast::ClassTree::default();

        let err =
            active_package_constant_modifications(&tree, &[("Medium".to_string(), missing_id)])
                .expect_err("missing active package alias target should fail");

        assert!(err.to_string().contains("target package class not found"));
    }

    #[test]
    fn inserted_active_package_constant_modifications_are_scoped() {
        let package_id = rumoca_core::DefId::new(12);
        let mut package = ast::ClassDef {
            name: token("Medium"),
            def_id: Some(package_id),
            class_type: rumoca_core::ClassType::Package,
            ..Default::default()
        };
        package.components.insert(
            "singleState".to_string(),
            ast::Component {
                name: "singleState".to_string(),
                type_name: name("Boolean"),
                variability: rumoca_core::Variability::Constant(token("constant")),
                binding: Some(bool_expr(true)),
                ..Default::default()
            },
        );

        let mut tree = ast::ClassTree::default();
        tree.definitions
            .classes
            .insert("Medium".to_string(), package);
        tree.def_map.insert(package_id, "Medium".to_string());
        tree.name_map.insert("Medium".to_string(), package_id);

        let target = ast::QualifiedName::from_ident("singleState");
        let mut mod_env = ast::ModificationEnvironment::new();
        mod_env.add(
            target.clone(),
            ast::ModificationValue::simple(bool_expr(false)),
        );

        let snapshot = insert_active_package_constant_modifications(
            &tree,
            &[("Medium".to_string(), package_id)],
            &mut mod_env,
        )
        .expect("active package constants should resolve");
        assert_eq!(
            mod_env
                .get(&target)
                .expect("outer modification should remain active")
                .value,
            bool_expr(false)
        );

        remove_inserted_package_constant_modifications(&mut mod_env, snapshot);
        assert_eq!(
            mod_env
                .get(&target)
                .expect("outer modification should be restored")
                .value,
            bool_expr(false)
        );

        let snapshot = insert_active_package_constant_modifications(
            &tree,
            &[("Medium".to_string(), package_id)],
            &mut mod_env,
        )
        .expect("active package constants should resolve");
        mod_env.active.shift_remove(&target);
        remove_inserted_package_constant_modifications(&mut mod_env, snapshot);
        assert_eq!(
            mod_env
                .get(&target)
                .expect("outer modification should be restored from snapshot")
                .value,
            bool_expr(false)
        );
    }

    #[test]
    fn duplicate_active_package_constant_modifications_restore_original_absence() {
        let package_id = rumoca_core::DefId::new(13);
        let mut package = ast::ClassDef {
            name: token("Medium"),
            def_id: Some(package_id),
            class_type: rumoca_core::ClassType::Package,
            ..Default::default()
        };
        package.components.insert(
            "singleState".to_string(),
            ast::Component {
                name: "singleState".to_string(),
                type_name: name("Boolean"),
                variability: rumoca_core::Variability::Constant(token("constant")),
                binding: Some(bool_expr(true)),
                ..Default::default()
            },
        );

        let mut tree = ast::ClassTree::default();
        tree.definitions
            .classes
            .insert("Medium".to_string(), package);
        tree.def_map.insert(package_id, "Medium".to_string());
        tree.name_map.insert("Medium".to_string(), package_id);

        let target = ast::QualifiedName::from_ident("singleState");
        let mut mod_env = ast::ModificationEnvironment::new();
        let snapshot = insert_active_package_constant_modifications(
            &tree,
            &[
                ("MediumA".to_string(), package_id),
                ("MediumB".to_string(), package_id),
            ],
            &mut mod_env,
        )
        .expect("active package constants should resolve");
        assert!(mod_env.get(&target).is_some());

        remove_inserted_package_constant_modifications(&mut mod_env, snapshot);
        assert!(mod_env.get(&target).is_none());
    }
}
