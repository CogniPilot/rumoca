use super::*;

/// Inject nested package constants for every instantiated component scope.
///
/// Equations flattened under a component prefix (e.g., `tank`) can reference
/// nested package constants unqualified (`nX`) or qualified (`Medium.nX`).
/// This pass mirrors model-level nested constant extraction and emits scoped keys
/// like `tank.nX` and `tank.Medium.nX`.
pub(crate) fn inject_component_instance_nested_class_constants(
    tree: &ClassTree,
    overlay: &InstanceOverlay,
    ctx: &mut Context,
) {
    const MAX_PASSES: usize = 5;
    let component_index: rustc_hash::FxHashMap<String, &rumoca_ir_ast::InstanceData> = overlay
        .components
        .values()
        .map(|comp| (comp.qualified_name.to_flat_string(), comp))
        .collect();
    let disabled_matcher = DisabledComponentMatcher::new(&overlay.disabled_components);
    for _pass in 0..MAX_PASSES {
        let prev = component_constant_footprint(ctx);

        for comp in overlay.components.values() {
            if disabled_matcher.matches(&comp.qualified_name) {
                continue;
            }
            let comp_scope = comp.qualified_name.to_flat_string();
            if comp_scope.is_empty() {
                continue;
            }
            let type_name = comp.type_name.to_string();

            let class_def = comp
                .type_def_id
                .and_then(|def_id| tree.get_class_by_def_id(def_id))
                .or_else(|| resolve_class_in_scope(tree, &type_name, &comp_scope).0)
                .or_else(|| tree.get_class_by_qualified_name(&type_name));
            let Some(class_def) = class_def else {
                continue;
            };

            // Resolve relative names using the component class context, not the
            // instance path (e.g. `sweptVolume`), which has no package nesting.
            let class_context = class_def
                .def_id
                .and_then(|id| tree.def_map.get(&id).cloned())
                .unwrap_or_else(|| type_name.clone());

            inject_component_declared_class_overrides(tree, &comp_scope, comp, &class_context, ctx);

            // Also inject constants from enclosing package/class scopes of the
            // component type (e.g., `fixedX`, `reducedX` for
            // `Medium.BaseProperties`). Conditional equations inside the
            // component can reference these unqualified.
            inject_component_enclosing_class_constants(tree, &comp_scope, &class_context, ctx);

            // Scan the full inheritance chain so package aliases/constants
            // declared in base classes are visible in component scope.
            // Process base->derived so derived declarations/redeclarations win.
            let mut classes_to_scan = collect_ancestor_classes(tree, &class_context);
            if classes_to_scan.is_empty() {
                classes_to_scan.push(class_def);
            }
            for scan_class in classes_to_scan.into_iter().rev() {
                let scan_context = scan_class
                    .def_id
                    .and_then(|id| tree.def_map.get(&id).cloned())
                    .unwrap_or_else(|| class_context.clone());
                inject_scan_class_constants(
                    tree,
                    &component_index,
                    &comp_scope,
                    scan_class,
                    &scan_context,
                    ctx,
                );
            }
        }

        let new = component_constant_footprint(ctx);
        if new == prev {
            break;
        }
    }
}

fn component_constant_footprint(ctx: &Context) -> usize {
    ctx.parameter_values.len()
        + ctx.real_parameter_values.len()
        + ctx.boolean_parameter_values.len()
        + ctx.enum_parameter_values.len()
        + ctx.constant_values.len()
        + ctx.array_dimensions.len()
}

pub(crate) fn inject_component_declared_class_overrides(
    tree: &ClassTree,
    comp_scope: &str,
    comp: &rumoca_ir_ast::InstanceData,
    _resolve_context: &str,
    ctx: &mut Context,
) {
    let active_alias = active_component_alias(&comp.type_name);

    for (alias_name, def_id) in &comp.class_overrides {
        let Some(alias_class) = tree.get_class_by_def_id(*def_id) else {
            continue;
        };
        if !matches!(alias_class.class_type, rumoca_ir_ast::ClassType::Package) {
            continue;
        }

        let alias_resolve_context = tree.def_map.get(def_id).map(String::as_str).unwrap_or("");

        let alias_scope = format!("{comp_scope}.{alias_name}");
        inject_referenced_constant_scopes(tree, alias_class, ctx);
        extract_constants_from_class_with_prefix(&alias_scope, alias_class, ctx);
        for ext in &alias_class.extends {
            apply_extends_constants_for_scope(tree, &alias_scope, ext, alias_resolve_context, ctx);
        }

        // Only expose unqualified component-scope constants (`comp_scope.nX`) for
        // the alias actually used by this component's declared type. This avoids
        // collisions from unrelated package aliases that happen to define the same
        // constant names (e.g., fixedX, nX) in different media packages.
        if active_alias != Some(alias_name.as_str()) {
            continue;
        }

        inject_referenced_constant_scopes(tree, alias_class, ctx);
        extract_constants_from_class_with_prefix(comp_scope, alias_class, ctx);
        for ext in &alias_class.extends {
            apply_extends_constants_for_scope(tree, comp_scope, ext, alias_resolve_context, ctx);
        }
    }
}

pub(crate) fn active_component_alias(type_name: &str) -> Option<&str> {
    crate::path_utils::first_path_segment_without_index(type_name).filter(|name| !name.is_empty())
}

pub(crate) fn inject_component_enclosing_class_constants(
    tree: &ClassTree,
    comp_scope: &str,
    class_context: &str,
    ctx: &mut Context,
) {
    let Some(enclosing_name) = crate::path_utils::parent_scope(class_context) else {
        return;
    };
    let ancestors = collect_ancestor_classes(tree, enclosing_name);
    if ancestors.is_empty() {
        return;
    }

    const MAX_PASSES: usize = 5;
    for _pass in 0..MAX_PASSES {
        let prev = component_constant_footprint(ctx);

        for ancestor in &ancestors {
            let resolve_context = ancestor
                .def_id
                .and_then(|id| tree.def_map.get(&id).cloned())
                .unwrap_or_else(|| enclosing_name.to_string());
            for ext in &ancestor.extends {
                apply_extends_constants_for_scope(tree, comp_scope, ext, &resolve_context, ctx);
            }
            inject_referenced_constant_scopes(tree, ancestor, ctx);
            extract_constants_from_class_with_prefix(comp_scope, ancestor, ctx);
        }

        let new = component_constant_footprint(ctx);
        if new == prev {
            break;
        }
    }
}

fn inject_referenced_constant_scopes(tree: &ClassTree, class_def: &ClassDef, ctx: &mut Context) {
    let mut scopes = rustc_hash::FxHashSet::default();
    for comp in class_def.components.values() {
        if let Some(binding) = &comp.binding {
            collect_qualified_constant_scopes(tree, binding, &mut scopes);
        }
        if !matches!(comp.start, rumoca_ir_ast::Expression::Empty) {
            collect_qualified_constant_scopes(tree, &comp.start, &mut scopes);
        }
        for modification in comp.modifications.values() {
            collect_qualified_constant_scopes(tree, modification, &mut scopes);
        }
    }

    for scope in scopes {
        if !ctx
            .injected_referenced_constant_scopes
            .insert(scope.clone())
        {
            continue;
        }
        let Some(scope_class) = tree.get_class_by_qualified_name(&scope) else {
            continue;
        };
        extract_constants_from_class_with_prefix(&scope, scope_class, ctx);
    }
}

fn collect_qualified_constant_scopes(
    tree: &ClassTree,
    expr: &rumoca_ir_ast::Expression,
    scopes: &mut rustc_hash::FxHashSet<String>,
) {
    match expr {
        rumoca_ir_ast::Expression::ComponentReference(cr) => {
            let parts = cr
                .parts
                .iter()
                .map(|part| part.ident.text.as_ref())
                .collect::<Vec<_>>();
            if parts.len() < 3 {
                return;
            }
            for prefix_len in (1..=parts.len() - 2).rev() {
                let candidate = parts[..prefix_len].join(".");
                if tree.get_class_by_qualified_name(&candidate).is_some() {
                    scopes.insert(candidate);
                    break;
                }
            }
        }
        rumoca_ir_ast::Expression::ClassModification { modifications, .. }
        | rumoca_ir_ast::Expression::Array {
            elements: modifications,
            ..
        }
        | rumoca_ir_ast::Expression::Tuple {
            elements: modifications,
        } => {
            for child in modifications {
                collect_qualified_constant_scopes(tree, child, scopes);
            }
        }
        rumoca_ir_ast::Expression::NamedArgument { value, .. }
        | rumoca_ir_ast::Expression::Parenthesized { inner: value }
        | rumoca_ir_ast::Expression::Unary { rhs: value, .. } => {
            collect_qualified_constant_scopes(tree, value, scopes);
        }
        rumoca_ir_ast::Expression::Modification { value, .. } => {
            collect_qualified_constant_scopes(tree, value, scopes);
        }
        rumoca_ir_ast::Expression::Binary { lhs, rhs, .. } => {
            collect_qualified_constant_scopes(tree, lhs, scopes);
            collect_qualified_constant_scopes(tree, rhs, scopes);
        }
        rumoca_ir_ast::Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_qualified_constant_scopes(tree, arg, scopes);
            }
        }
        rumoca_ir_ast::Expression::FieldAccess { base, .. } => {
            collect_qualified_constant_scopes(tree, base, scopes);
        }
        rumoca_ir_ast::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, value) in branches {
                collect_qualified_constant_scopes(tree, cond, scopes);
                collect_qualified_constant_scopes(tree, value, scopes);
            }
            collect_qualified_constant_scopes(tree, else_branch, scopes);
        }
        rumoca_ir_ast::Expression::Range { start, step, end } => {
            collect_qualified_constant_scopes(tree, start, scopes);
            if let Some(step) = step {
                collect_qualified_constant_scopes(tree, step, scopes);
            }
            collect_qualified_constant_scopes(tree, end, scopes);
        }
        _ => {}
    }
}

pub(crate) fn inject_scan_class_constants(
    tree: &ClassTree,
    component_index: &rustc_hash::FxHashMap<String, &rumoca_ir_ast::InstanceData>,
    comp_scope: &str,
    scan_class: &ClassDef,
    scan_context: &str,
    ctx: &mut Context,
) {
    inject_class_extends_constants(tree, comp_scope, scan_class, scan_context, ctx);

    for (nested_name, nested_class) in &scan_class.classes {
        let nested_scope = format!("{comp_scope}.{nested_name}");
        inject_nested_class_constants(
            tree,
            comp_scope,
            &nested_scope,
            nested_class,
            scan_context,
            ctx,
        );
    }

    for (alias_name, alias_comp) in &scan_class.components {
        inject_alias_component_package_constants(
            tree,
            comp_scope,
            alias_name,
            alias_comp,
            scan_context,
            ctx,
        );
    }

    inject_alias_constants_from_specialized_child_components(
        tree,
        component_index,
        comp_scope,
        scan_class,
        scan_context,
        ctx,
    );
}

/// Inject alias package constants by matching declared child component types
/// against their instantiated specialized types in the overlay.
///
/// MLS §7.3: component-level redeclare package modifiers specialize nested
/// package aliases for that component instance. Flatten branch selection must
/// observe these effective alias constants (e.g., `Medium.ThermoStates`).
pub(crate) fn inject_alias_constants_from_specialized_child_components(
    tree: &ClassTree,
    component_index: &rustc_hash::FxHashMap<String, &rumoca_ir_ast::InstanceData>,
    comp_scope: &str,
    scan_class: &ClassDef,
    scan_context: &str,
    ctx: &mut Context,
) {
    let (parent_class_overrides, parent_has_forwarding_redeclare) =
        if let Some(inst) = component_index.get(comp_scope) {
            (
                Some(&inst.class_overrides),
                inst.has_forwarding_class_redeclare,
            )
        } else {
            (None, false)
        };

    for (child_name, child_comp) in &scan_class.components {
        let declared_type = child_comp.type_name.to_string();
        let Some((alias_name, declared_tail)) = split_alias_declared_type(&declared_type) else {
            continue;
        };

        let child_scope = format!("{comp_scope}.{child_name}");
        let Some(child_inst) = component_index.get(&child_scope) else {
            continue;
        };

        // Prefer explicit parent instance class-overrides for alias package resolution.
        // MLS §7.3: component-level redeclare bindings define effective alias packages
        // even when child type_def_id remains the inherited base member class.
        if parent_has_forwarding_redeclare
            && let Some(overrides) = parent_class_overrides
            && let Some(&package_def_id) = overrides.get(alias_name)
            && let Some(package_class) = tree.get_class_by_def_id(package_def_id)
            && matches!(package_class.class_type, rumoca_ir_ast::ClassType::Package)
        {
            let alias_scope = format!("{comp_scope}.{alias_name}");
            let package_context = tree
                .def_map
                .get(&package_def_id)
                .cloned()
                .unwrap_or_else(|| scan_context.to_string());
            extract_constants_from_class_with_prefix(&alias_scope, package_class, ctx);
            extract_constants_from_class_with_prefix(comp_scope, package_class, ctx);
            extract_constants_from_class_with_prefix(&child_scope, package_class, ctx);
            for ext in &package_class.extends {
                apply_extends_constants_for_scope(tree, &alias_scope, ext, &package_context, ctx);
                apply_extends_constants_for_scope(tree, comp_scope, ext, &package_context, ctx);
                apply_extends_constants_for_scope(tree, &child_scope, ext, &package_context, ctx);
            }
            continue;
        }

        let actual_type_name = child_inst
            .type_def_id
            .and_then(|def_id| tree.def_map.get(&def_id).cloned())
            .unwrap_or_else(|| child_inst.type_name.clone());

        let Some(package_name) = strip_declared_suffix(&actual_type_name, declared_tail) else {
            continue;
        };

        let package_class = resolve_class_in_scope(tree, &package_name, scan_context)
            .0
            .or_else(|| tree.get_class_by_qualified_name(&package_name));
        let Some(package_class) = package_class else {
            continue;
        };
        if !matches!(package_class.class_type, rumoca_ir_ast::ClassType::Package) {
            continue;
        }
        let package_context = package_class
            .def_id
            .and_then(|id| tree.def_map.get(&id).cloned())
            .unwrap_or_else(|| package_name.clone());

        let alias_scope = format!("{comp_scope}.{alias_name}");
        extract_constants_from_class_with_prefix(&alias_scope, package_class, ctx);
        extract_constants_from_class_with_prefix(comp_scope, package_class, ctx);
        for ext in &package_class.extends {
            apply_extends_constants_for_scope(tree, &alias_scope, ext, &package_context, ctx);
            apply_extends_constants_for_scope(tree, comp_scope, ext, &package_context, ctx);
        }
    }
}

pub(crate) fn split_alias_declared_type(type_name: &str) -> Option<(&str, &str)> {
    let (alias_name, declared_tail) = type_name.split_once('.')?;
    if alias_name.starts_with(char::is_uppercase) && !declared_tail.is_empty() {
        Some((alias_name, declared_tail))
    } else {
        None
    }
}

pub(crate) fn strip_declared_suffix(actual_type: &str, declared_tail: &str) -> Option<String> {
    let prefix = actual_type.strip_suffix(declared_tail)?;
    let package_name = prefix.strip_suffix('.').unwrap_or(prefix);
    if package_name.is_empty() {
        None
    } else {
        Some(package_name.to_string())
    }
}

#[cfg(test)]
mod active_component_alias_tests {
    use super::active_component_alias;

    #[test]
    fn resolves_first_segment_for_dotted_type_name() {
        assert_eq!(
            active_component_alias("Medium.BaseProperties"),
            Some("Medium")
        );
    }

    #[test]
    fn ignores_dot_inside_subscript_expression() {
        assert_eq!(
            active_component_alias("Medium[data.medium].BaseProperties"),
            Some("Medium")
        );
    }

    #[test]
    fn returns_none_for_empty_name() {
        assert_eq!(active_component_alias(""), None);
    }
}
