use super::*;
use std::sync::Arc;

/// Propagate a record binding to scalar field bindings.
///
/// MLS §7.2: a record binding like `Complex u = expr` projects bindings for fields
/// (for example `re = expr.re`, `im = expr.im`).
pub(crate) fn propagate_record_binding_to_fields(
    tree: &ast::ClassTree,
    ctx: &mut InstantiateContext,
    binding_expr: &ast::Expression,
    binding_source_scope: Option<ast::QualifiedName>,
    binding_is_each: bool,
    nested_class: &ast::ClassDef,
    targeted_keys: &IndexMap<ast::QualifiedName, ()>,
) -> InstantiateResult<IndexMap<ast::QualifiedName, ()>> {
    // MLS §7.2 record binding projection applies only to record components.
    // For non-record classes (model/block/connector), class modifications must
    // remain component modifiers and must not synthesize per-field bindings.
    if nested_class.class_type != rumoca_core::ClassType::Record {
        return Ok(IndexMap::default());
    }

    // Get effective components including inherited ones (MLS §7.2).
    // For type aliases like `ComplexVoltage = Complex(...)`, direct components may
    // be empty while fields come from a base class.
    let effective = get_effective_components(tree, nested_class)?;
    let components: &IndexMap<String, ast::Component> = if effective.is_empty() {
        &nested_class.components
    } else {
        &effective
    };
    let preserve_declared_defaults = is_default_record_constructor_call(binding_expr, nested_class);
    let mut projected_keys = IndexMap::default();

    for (field_name, field_comp) in components {
        let field_qn = ast::QualifiedName::from_ident(field_name);
        // Preserve explicit field modifiers targeting this record component
        // (either local `state(T=...)` or shifted parent `comp.state.T=...`).
        if targeted_keys.contains_key(&field_qn) {
            continue;
        }
        // MLS §12.6 (record constructors): `R()` without arguments uses the
        // record's declared field defaults. Keep existing field defaults instead
        // of replacing them with synthetic `R().field` bindings.
        if preserve_declared_defaults && has_declared_field_default(field_comp) {
            continue;
        }
        let field_binding = same_type_alias_explicit_field_binding(
            binding_expr,
            nested_class,
            components,
            ctx.mod_env(),
            field_name,
        );
        if field_binding.is_none()
            && should_preserve_same_type_alias_field_default(
                binding_expr,
                nested_class,
                components,
                ctx.mod_env(),
                field_name,
                field_comp,
            )
        {
            continue;
        }

        let field_access = if let Some(field_binding) = field_binding {
            field_binding
        } else {
            project_record_field_binding(
                tree,
                binding_expr,
                binding_source_scope.as_ref(),
                nested_class,
                field_name,
            )?
        };

        ctx.mod_env_mut().active.insert(
            field_qn.clone(),
            ast::ModificationValue::with_source_scope_and_prefixes(
                field_access.clone(),
                Some(field_access),
                binding_source_scope.clone(),
                binding_is_each,
                false,
            ),
        );
        projected_keys.insert(field_qn, ());
    }
    Ok(projected_keys)
}

fn should_preserve_same_type_alias_field_default(
    binding_expr: &ast::Expression,
    target_record: &ast::ClassDef,
    effective_components: &IndexMap<String, ast::Component>,
    mod_env: &ast::ModificationEnvironment,
    field_name: &str,
    field_comp: &ast::Component,
) -> bool {
    if !has_declared_field_default(field_comp) {
        return false;
    }

    let Some(source_name) = simple_record_alias_source_name(binding_expr) else {
        return false;
    };

    let source_component =
        same_type_record_alias_source(binding_expr, target_record, effective_components);
    !record_alias_source_explicitly_binds_field(source_name, source_component, mod_env, field_name)
}

fn same_type_record_alias_source<'a>(
    binding_expr: &ast::Expression,
    target_record: &ast::ClassDef,
    effective_components: &'a IndexMap<String, ast::Component>,
) -> Option<&'a ast::Component> {
    let ast::Expression::ComponentReference(comp_ref) = binding_expr else {
        return None;
    };
    if comp_ref.parts.len() != 1 || comp_ref.parts[0].subs.is_some() {
        return None;
    }

    let source_name = comp_ref.parts[0].ident.text.as_ref();
    let source_component = effective_components.get(source_name)?;
    if comp_ref.def_id.is_some() && source_component.def_id != comp_ref.def_id {
        return None;
    }
    if source_component.type_def_id != target_record.def_id {
        return None;
    }
    Some(source_component)
}

fn simple_record_alias_source_name(binding_expr: &ast::Expression) -> Option<&str> {
    let ast::Expression::ComponentReference(comp_ref) = binding_expr else {
        return None;
    };
    if comp_ref.parts.len() != 1 || comp_ref.parts[0].subs.is_some() {
        return None;
    }
    Some(comp_ref.parts[0].ident.text.as_ref())
}

fn same_type_alias_explicit_field_binding(
    binding_expr: &ast::Expression,
    target_record: &ast::ClassDef,
    effective_components: &IndexMap<String, ast::Component>,
    mod_env: &ast::ModificationEnvironment,
    field_name: &str,
) -> Option<ast::Expression> {
    let source_name = simple_record_alias_source_name(binding_expr)?;
    if let Some(source_component) =
        same_type_record_alias_source(binding_expr, target_record, effective_components)
        && let Some((_, value)) = source_component
            .modifications
            .iter()
            .find(|(name, _)| name.as_str() == field_name)
    {
        return Some(value.clone());
    }

    let source_field = ast::QualifiedName::from_ident(source_name).child(field_name);
    mod_env
        .active
        .iter()
        .find(|(key, _)| **key == source_field)
        .map(|(_, value)| value.source.clone().unwrap_or_else(|| value.value.clone()))
}

fn record_alias_source_explicitly_binds_field(
    source_name: &str,
    source_component: Option<&ast::Component>,
    mod_env: &ast::ModificationEnvironment,
    field_name: &str,
) -> bool {
    if source_component.is_some_and(|source_component| {
        source_component
            .modifications
            .iter()
            .any(|(name, _)| name.as_str() == field_name)
    }) {
        return true;
    }

    let source_field = ast::QualifiedName::from_ident(source_name).child(field_name);
    mod_env.active.keys().any(|key| *key == source_field)
}

fn project_record_field_binding(
    tree: &ast::ClassTree,
    binding_expr: &ast::Expression,
    binding_source_scope: Option<&ast::QualifiedName>,
    target_record: &ast::ClassDef,
    field_name: &str,
) -> InstantiateResult<ast::Expression> {
    Ok(match binding_expr {
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => ast::Expression::If {
            branches: branches
                .iter()
                .map(|(cond, branch_expr)| {
                    Ok((
                        cond.clone(),
                        project_record_field_binding(
                            tree,
                            branch_expr,
                            binding_source_scope,
                            target_record,
                            field_name,
                        )?,
                    ))
                })
                .collect::<InstantiateResult<Vec<_>>>()?,
            else_branch: Arc::new(project_record_field_binding(
                tree,
                else_branch,
                binding_source_scope,
                target_record,
                field_name,
            )?),
            span: binding_expr.span(),
        },
        ast::Expression::Parenthesized { inner, span } => ast::Expression::Parenthesized {
            inner: Arc::new(project_record_field_binding(
                tree,
                inner,
                binding_source_scope,
                target_record,
                field_name,
            )?),
            span: *span,
        },
        _ => {
            if let Some(field_binding) = constructor_projected_field_binding(
                tree,
                binding_expr,
                binding_source_scope,
                field_name,
            )? {
                return Ok(field_binding);
            }
            let base = constructor_record_projection_base(
                tree,
                binding_expr,
                binding_source_scope,
                target_record,
            )?
            .unwrap_or_else(|| binding_expr.clone());
            ast::Expression::FieldAccess {
                base: Arc::new(base),
                field: field_name.to_string(),
                span: binding_expr.span(),
            }
        }
    })
}

fn constructor_record_projection_base(
    tree: &ast::ClassTree,
    binding_expr: &ast::Expression,
    binding_source_scope: Option<&ast::QualifiedName>,
    target_record: &ast::ClassDef,
) -> InstantiateResult<Option<ast::Expression>> {
    let ast::Expression::FunctionCall { comp, .. } = binding_expr else {
        return Ok(None);
    };
    let Some(source_record) = constructor_class_for_call(tree, comp, binding_source_scope) else {
        return Ok(None);
    };
    if source_record.def_id == target_record.def_id {
        return Ok(None);
    }
    let Some(source_field) = unique_constructor_record_field(tree, source_record, target_record)?
    else {
        return Ok(None);
    };
    Ok(Some(ast::Expression::FieldAccess {
        base: Arc::new(binding_expr.clone()),
        field: source_field,
        span: binding_expr.span(),
    }))
}

fn constructor_projected_field_binding(
    tree: &ast::ClassTree,
    binding_expr: &ast::Expression,
    binding_source_scope: Option<&ast::QualifiedName>,
    field_name: &str,
) -> InstantiateResult<Option<ast::Expression>> {
    let ast::Expression::FunctionCall { comp, args, .. } = binding_expr else {
        return Ok(None);
    };
    let Some(source_record) = constructor_class_for_call(tree, comp, binding_source_scope) else {
        return Ok(None);
    };

    let effective = get_effective_components(tree, source_record)?;
    let components = if effective.is_empty() {
        &source_record.components
    } else {
        &effective
    };

    if let Some(arg_binding) = constructor_argument_field_binding(args, components, field_name) {
        return Ok(Some(arg_binding));
    }

    Ok(components
        .get(field_name)
        .and_then(|component| component.binding.clone()))
}

fn constructor_argument_field_binding(
    args: &[ast::Expression],
    components: &IndexMap<String, ast::Component>,
    field_name: &str,
) -> Option<ast::Expression> {
    if let Some(named) = args.iter().find_map(|arg| match arg {
        ast::Expression::NamedArgument { name, value, .. } if name.text.as_ref() == field_name => {
            Some(value.as_ref().clone())
        }
        _ => None,
    }) {
        return Some(named);
    }

    let field_index = components.keys().position(|name| name == field_name)?;
    args.iter()
        .filter(|arg| !matches!(arg, ast::Expression::NamedArgument { .. }))
        .nth(field_index)
        .cloned()
}

fn constructor_class_for_call<'a>(
    tree: &'a ast::ClassTree,
    comp: &ast::ComponentReference,
    binding_source_scope: Option<&ast::QualifiedName>,
) -> Option<&'a ast::ClassDef> {
    comp.def_id
        .and_then(|def_id| tree.get_class_by_def_id(def_id))
        .or_else(|| find_class_in_tree(tree, &comp.to_string()))
        .or_else(|| resolve_scoped_constructor_class(tree, comp, binding_source_scope))
}

fn resolve_scoped_constructor_class<'a>(
    tree: &'a ast::ClassTree,
    comp: &ast::ComponentReference,
    binding_source_scope: Option<&ast::QualifiedName>,
) -> Option<&'a ast::ClassDef> {
    let scope = binding_source_scope?;
    let name = comp.to_string();
    for prefix_len in (0..=scope.parts.len()).rev() {
        let prefix = scope.parts[..prefix_len]
            .iter()
            .map(|part| part.0.as_str())
            .collect::<Vec<_>>()
            .join(".");
        let candidate = if prefix.is_empty() {
            name.clone()
        } else {
            format!("{prefix}.{name}")
        };
        if let Some(class_def) = find_class_in_tree(tree, &candidate) {
            return Some(class_def);
        }
    }
    None
}

fn unique_constructor_record_field(
    tree: &ast::ClassTree,
    source_record: &ast::ClassDef,
    target_record: &ast::ClassDef,
) -> InstantiateResult<Option<String>> {
    let components = get_effective_components(tree, source_record)?;
    let components = if components.is_empty() {
        &source_record.components
    } else {
        &components
    };
    let mut matches = components
        .iter()
        .filter(|(_name, component)| component_record_type_matches(tree, component, target_record))
        .map(|(name, _component)| name.clone());
    let Some(first) = matches.next() else {
        return Ok(None);
    };
    Ok(matches.next().is_none().then_some(first))
}

fn component_record_type_matches(
    tree: &ast::ClassTree,
    component: &ast::Component,
    target_record: &ast::ClassDef,
) -> bool {
    if component.type_def_id.is_some() && component.type_def_id == target_record.def_id {
        return true;
    }
    let component_type = component.type_name.to_string();
    let target_name = target_record
        .def_id
        .and_then(|def_id| tree.def_map.get(&def_id))
        .cloned()
        .unwrap_or_else(|| target_record.name.text.to_string());
    component_type == target_name || is_type_subtype(tree, &component_type, &target_name)
}

fn is_default_record_constructor_call(
    expr: &ast::Expression,
    nested_class: &ast::ClassDef,
) -> bool {
    match expr {
        // MLS §12.6: only `R()` for the declared record `R` preserves the
        // record's own field defaults. A different zero-argument record
        // constructor (e.g. `BaseData x = Derived()`) must still project the
        // bound record fields rather than freezing the declared base defaults.
        ast::Expression::FunctionCall { comp, args, .. } => {
            args.is_empty() && record_constructor_matches_class(comp, nested_class)
        }
        ast::Expression::Parenthesized { inner, .. } => {
            is_default_record_constructor_call(inner, nested_class)
        }
        _ => false,
    }
}

fn record_constructor_matches_class(
    comp: &ast::ComponentReference,
    nested_class: &ast::ClassDef,
) -> bool {
    if let (Some(comp_def_id), Some(class_def_id)) = (comp.def_id, nested_class.def_id) {
        return comp_def_id == class_def_id;
    }

    comp.parts
        .last()
        .is_some_and(|part| part.ident.text.as_ref() == nested_class.name.text.as_ref())
}

fn has_declared_field_default(comp: &ast::Component) -> bool {
    comp.binding.is_some()
}
