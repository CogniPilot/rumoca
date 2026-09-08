use super::*;
use rumoca_core::DefId;
use std::collections::HashMap;
use std::sync::Arc;

/// Cache of record-projection bases, keyed by the address of the AST node they
/// were built from.
///
/// Every field of a record binding projects off the *same* base expression (for
/// example `f(...).out_c` for all fields of `r` in `Bjt3 r = f(...)`). Building
/// that base once and sharing the `Arc` keeps the AST a DAG instead of cloning
/// the whole record-returning call once per field, which for the Spice3 device
/// records means one 80-argument call instead of 67 copies of it.
///
/// The keys are addresses of AST nodes reachable from `binding_expr`, which is
/// borrowed for the whole projection and never mutated while the cache lives,
/// so an address uniquely identifies a node for the cache's lifetime.
type ProjectionBaseCache = HashMap<usize, Arc<ast::Expression>>;

/// The resolved value and written source of a record modifier.
///
/// These expressions can differ when one record parameter forwards another:
/// the value is resolved for instantiation, while the source remains relative
/// to `source_scope` for exact occurrence resolution in Flat.
pub(crate) struct RecordBindingProjection<'a> {
    pub(crate) value: &'a ast::Expression,
    pub(crate) source: Option<&'a ast::Expression>,
    pub(crate) source_scope: Option<ast::QualifiedName>,
    pub(crate) each: bool,
}

/// Propagate a record binding to scalar field bindings.
///
/// MLS §7.2: a record binding like `Complex u = expr` projects bindings for fields
/// (for example `re = expr.re`, `im = expr.im`).
pub(crate) fn propagate_record_binding_to_fields(
    tree: &ast::ClassTree,
    ctx: &mut InstantiateContext,
    binding: RecordBindingProjection<'_>,
    nested_class: &ast::ClassDef,
    targeted_keys: &IndexMap<ast::QualifiedName, ()>,
) -> InstantiateResult<IndexMap<ast::QualifiedName, ()>> {
    // MLS §7.2 record binding projection applies only to record components.
    // For non-record classes (model/block/connector), class modifications must
    // remain component modifiers and must not synthesize per-field bindings.
    if nested_class.class_type != rumoca_core::ClassType::Record {
        return Ok(IndexMap::default());
    }
    validate_projection_reference_identities(binding.value)?;
    if let Some(source) = binding.source {
        validate_projection_reference_identities(source)?;
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
    let preserve_declared_defaults =
        is_default_record_constructor_call(binding.value, nested_class)?;
    let mut projected_keys = IndexMap::default();
    let mut base_cache = ProjectionBaseCache::new();
    let mut planned_bindings = Vec::new();

    for (field_name, field_comp) in components {
        let field_def_id = field_comp.def_id.ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                field_name,
                field_comp.location.span(),
            ))
        })?;
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
            binding.value,
            nested_class,
            components,
            ctx.mod_env(),
            field_name,
        )?;
        if field_binding.is_none()
            && should_preserve_same_type_alias_field_default(
                binding.value,
                nested_class,
                components,
                ctx.mod_env(),
                field_name,
                field_comp,
            )?
        {
            continue;
        }

        let field_access = if let Some(field_binding) = field_binding {
            field_binding
        } else {
            project_record_field_binding(
                tree,
                binding.value,
                binding.source_scope.as_ref(),
                nested_class,
                field_name,
                field_def_id,
                &mut base_cache,
            )?
        };
        let field_source = match binding.source {
            Some(source_expr) if source_expr != binding.value => project_record_field_binding(
                tree,
                source_expr,
                binding.source_scope.as_ref(),
                nested_class,
                field_name,
                field_def_id,
                &mut base_cache,
            )?,
            _ => field_access.clone(),
        };

        planned_bindings.push((
            field_qn.clone(),
            ast::ModificationValue::with_source_scope_and_prefixes(
                field_access,
                Some(field_source),
                binding.source_scope.clone(),
                binding.each,
                false,
            ),
        ));
        projected_keys.insert(field_qn, ());
    }
    for (field_qn, value) in planned_bindings {
        ctx.mod_env_mut().active.insert(field_qn, value);
    }
    Ok(projected_keys)
}

fn validate_projection_reference_identities(expr: &ast::Expression) -> InstantiateResult<()> {
    for reference in ast::visitor::collect_component_refs(expr) {
        for (index, part) in reference.parts.iter().enumerate() {
            if part.def_id.is_none() {
                return Err(Box::new(InstantiateError::missing_resolved_identity(
                    format!(
                        "record projection reference segment {} in `{reference}`",
                        index + 1
                    ),
                    reference.span,
                )));
            }
        }
    }
    Ok(())
}

fn should_preserve_same_type_alias_field_default(
    binding_expr: &ast::Expression,
    target_record: &ast::ClassDef,
    effective_components: &IndexMap<String, ast::Component>,
    mod_env: &ast::ModificationEnvironment,
    field_name: &str,
    field_comp: &ast::Component,
) -> InstantiateResult<bool> {
    if !has_declared_field_default(field_comp) {
        return Ok(false);
    }

    let Some(source_component) =
        same_type_record_alias_source(binding_expr, target_record, effective_components)?
    else {
        // A reference from an outer scope, or one whose effective record type
        // differs from the declared target, can carry different inherited
        // defaults. Project the field instead of freezing the target record's
        // default (for example a transient CellData subtype with nRC = 2).
        return Ok(false);
    };
    Ok(!record_alias_source_explicitly_binds_field(
        &source_component.name,
        source_component,
        mod_env,
        field_name,
    ))
}

fn same_type_record_alias_source<'a>(
    binding_expr: &ast::Expression,
    target_record: &ast::ClassDef,
    effective_components: &'a IndexMap<String, ast::Component>,
) -> InstantiateResult<Option<&'a ast::Component>> {
    let ast::Expression::ComponentReference(comp_ref) = binding_expr else {
        return Ok(None);
    };
    if comp_ref.parts.len() != 1 || comp_ref.parts[0].subs.is_some() {
        return Ok(None);
    }

    let source_name = comp_ref.parts[0].ident.text.as_ref();
    let reference_def_id = comp_ref.root_def_id().ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("record alias source `{source_name}`"),
            comp_ref.span,
        ))
    })?;
    let Some(source_component) = effective_components.get(source_name) else {
        return Ok(None);
    };
    let source_component_def_id = source_component.def_id.ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("record alias component `{source_name}`"),
            source_component.location.span(),
        ))
    })?;
    if source_component_def_id != reference_def_id {
        return Ok(None);
    }
    let source_type_def_id = source_component.type_def_id.ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("record alias component type `{source_name}`"),
            source_component.location.span(),
        ))
    })?;
    let target_type_def_id = target_record.def_id.ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("record projection target `{}`", target_record.name.text),
            target_record.location.span(),
        ))
    })?;
    if source_type_def_id != target_type_def_id {
        return Ok(None);
    }
    Ok(Some(source_component))
}

fn same_type_alias_explicit_field_binding(
    binding_expr: &ast::Expression,
    target_record: &ast::ClassDef,
    effective_components: &IndexMap<String, ast::Component>,
    mod_env: &ast::ModificationEnvironment,
    field_name: &str,
) -> InstantiateResult<Option<ast::Expression>> {
    let Some(source_component) =
        same_type_record_alias_source(binding_expr, target_record, effective_components)?
    else {
        return Ok(None);
    };
    if let Some(field_binding) = source_component
        .modifications
        .iter()
        .find(|(name, _)| name.as_str() == field_name)
        .and_then(|(_, value)| value.component_modifier_binding_value())
    {
        return Ok(Some(field_binding.clone()));
    }

    let source_field = ast::QualifiedName::from_ident(&source_component.name).child(field_name);
    Ok(mod_env
        .active
        .iter()
        .find(|(key, _)| **key == source_field)
        .map(|(_, value)| value.source.clone().unwrap_or_else(|| value.value.clone())))
}

fn record_alias_source_explicitly_binds_field(
    source_name: &str,
    source_component: &ast::Component,
    mod_env: &ast::ModificationEnvironment,
    field_name: &str,
) -> bool {
    if source_component
        .modifications
        .iter()
        .any(|(name, modifier)| {
            name.as_str() == field_name && modifier.component_modifier_binding_value().is_some()
        })
    {
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
    field_def_id: DefId,
    base_cache: &mut ProjectionBaseCache,
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
                            field_def_id,
                            base_cache,
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
                field_def_id,
                base_cache,
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
                field_def_id,
                base_cache,
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
            ast::Expression::FieldAccess {
                base: shared_projection_base(
                    tree,
                    binding_expr,
                    binding_source_scope,
                    target_record,
                    base_cache,
                )?,
                field: field_name.to_string(),
                field_def_id: Some(field_def_id),
                span: binding_expr.span(),
            }
        }
    })
}

/// Return the shared `Arc` for the base every field of `binding_expr` projects
/// off, building it on first use.
///
/// The base does not depend on the field name, so all fields of the record can
/// point at one `Arc` instead of each owning a deep copy of the (possibly very
/// large) record-returning expression.
fn shared_projection_base(
    tree: &ast::ClassTree,
    binding_expr: &ast::Expression,
    binding_source_scope: Option<&ast::QualifiedName>,
    target_record: &ast::ClassDef,
    base_cache: &mut ProjectionBaseCache,
) -> InstantiateResult<Arc<ast::Expression>> {
    let key = std::ptr::from_ref(binding_expr) as usize;
    if let Some(cached) = base_cache.get(&key) {
        return Ok(Arc::clone(cached));
    }
    let base = constructor_record_projection_base(
        tree,
        binding_expr,
        binding_source_scope,
        target_record,
    )?
    .map_or_else(|| Arc::new(binding_expr.clone()), Arc::new);
    base_cache.insert(key, Arc::clone(&base));
    Ok(base)
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
    let source_record = constructor_class_for_call(tree, comp, binding_source_scope)?;
    if source_record.def_id == target_record.def_id {
        return Ok(None);
    }
    let Some((source_field, source_field_def_id)) =
        unique_constructor_record_field(tree, source_record, target_record)?
    else {
        return Ok(None);
    };
    Ok(Some(ast::Expression::FieldAccess {
        base: Arc::new(binding_expr.clone()),
        field: source_field,
        field_def_id: Some(source_field_def_id),
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
    let source_record = constructor_class_for_call(tree, comp, binding_source_scope)?;

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
    _binding_source_scope: Option<&ast::QualifiedName>,
) -> InstantiateResult<&'a ast::ClassDef> {
    // A record constructor call names a class, so the class is the reference's
    // exact *target* segment. `root_def_id` is the first segment, which for a
    // dotted constructor such as `P.Concrete.Element(...)` identifies the
    // enclosing package rather than the record (MLS §5.3, §12.6).
    let target_def_id = comp.target_def_id().ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("record constructor/function call `{comp}`"),
            comp.span,
        ))
    })?;
    tree.get_class_by_def_id(target_def_id).ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("record constructor/function target `{comp}` ({target_def_id:?})"),
            comp.span,
        ))
    })
}

fn unique_constructor_record_field(
    tree: &ast::ClassTree,
    source_record: &ast::ClassDef,
    target_record: &ast::ClassDef,
) -> InstantiateResult<Option<(String, DefId)>> {
    let components = get_effective_components(tree, source_record)?;
    let components = if components.is_empty() {
        &source_record.components
    } else {
        &components
    };
    let mut matches = Vec::new();
    for (name, component) in components {
        if !component_record_type_matches(tree, component, target_record)? {
            continue;
        }
        let def_id = component.def_id.ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                name,
                component.location.span(),
            ))
        })?;
        matches.push((name.clone(), def_id));
    }
    let mut matches = matches.into_iter();
    let Some(first) = matches.next() else {
        return Ok(None);
    };
    match matches.next() {
        Some(_) => Err(Box::new(InstantiateError::redeclare_error(
            target_record.name.text.as_ref(),
            "record-returning call has multiple exact fields compatible with the projection target",
            target_record.location.span(),
        ))),
        None => Ok(Some(first)),
    }
}

fn component_record_type_matches(
    tree: &ast::ClassTree,
    component: &ast::Component,
    target_record: &ast::ClassDef,
) -> InstantiateResult<bool> {
    let component_type_def_id = component.type_def_id.ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("record component type `{}`", component.type_name),
            component.location.span(),
        ))
    })?;
    let target_record_def_id = target_record.def_id.ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("target record `{}`", target_record.name.text),
            target_record.location.span(),
        ))
    })?;
    crate::inheritance::is_type_subtype_by_def_id(
        tree,
        component_type_def_id,
        target_record_def_id,
        &mut crate::inheritance::SubtypeCache::default(),
    )
}

fn is_default_record_constructor_call(
    expr: &ast::Expression,
    nested_class: &ast::ClassDef,
) -> InstantiateResult<bool> {
    match expr {
        // MLS §12.6: only `R()` for the declared record `R` preserves the
        // record's own field defaults. A different zero-argument record
        // constructor (e.g. `BaseData x = Derived()`) must still project the
        // bound record fields rather than freezing the declared base defaults.
        ast::Expression::FunctionCall { comp, args, .. } => {
            Ok(args.is_empty() && record_constructor_matches_class(comp, nested_class)?)
        }
        ast::Expression::Parenthesized { inner, .. } => {
            is_default_record_constructor_call(inner, nested_class)
        }
        _ => Ok(false),
    }
}

fn record_constructor_matches_class(
    comp: &ast::ComponentReference,
    nested_class: &ast::ClassDef,
) -> InstantiateResult<bool> {
    let comp_def_id = comp.target_def_id().ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("record constructor `{comp}`"),
            comp.span,
        ))
    })?;
    let class_def_id = nested_class.def_id.ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("record projection target `{}`", nested_class.name.text),
            nested_class.location.span(),
        ))
    })?;
    Ok(comp_def_id == class_def_id)
}

fn has_declared_field_default(comp: &ast::Component) -> bool {
    comp.binding.is_some()
}
