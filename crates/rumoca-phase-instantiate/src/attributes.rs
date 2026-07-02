use super::*;
use rumoca_eval_ast::eval_instantiate::{
    InstantiateEvalCtx, eval_state_select_expr_with_source_scope, expr_to_bool, expr_to_string,
    parse_state_select,
};

pub(super) struct ComponentAttrsAndBinding {
    pub(super) attrs: ExtractedAttributes,
    pub(super) binding: Option<ast::Expression>,
    pub(super) binding_source: Option<ast::Expression>,
    pub(super) binding_source_scope: Option<ast::QualifiedName>,
    pub(super) binding_from_modification: bool,
}

#[cfg(test)]
pub(super) fn extract_component_attrs_and_binding(
    comp: &ast::Component,
    mod_env: &ast::ModificationEnvironment,
    instance_name: &str,
    eval_ctx: &InstantiateEvalCtx<'_>,
    imports: &[(String, String)],
) -> InstantiateResult<ComponentAttrsAndBinding> {
    extract_component_attrs_and_binding_in_scope(
        comp,
        mod_env,
        instance_name,
        eval_ctx,
        imports,
        None,
    )
}

pub(super) fn extract_component_attrs_and_binding_in_scope(
    comp: &ast::Component,
    mod_env: &ast::ModificationEnvironment,
    instance_name: &str,
    eval_ctx: &InstantiateEvalCtx<'_>,
    imports: &[(String, String)],
    declaration_scope: Option<&ast::QualifiedName>,
) -> InstantiateResult<ComponentAttrsAndBinding> {
    // Pass component name so mod_env can be checked for outer modifications.
    let mut attrs = extract_attributes_in_scope(
        comp,
        mod_env,
        &comp.name,
        instance_name,
        eval_ctx,
        imports,
        declaration_scope,
    )?;
    let (binding, binding_from_modification, mut binding_source_scope) =
        extract_binding(comp, mod_env);
    if binding_from_modification && binding_source_scope.is_none() {
        binding_source_scope = declaration_scope.cloned();
    }
    let binding_source = if binding_from_modification {
        let binding_path = ast::QualifiedName::from_ident(&comp.name);
        mod_env
            .get(&binding_path)
            .and_then(|mod_value| mod_value.source.clone())
    } else {
        None
    };

    // MLS §4.4.4: declaration binding may provide a default start for
    // parameter/constant declarations when no explicit start is present.
    // MLS §7.2.4: outer *modification* bindings do not rewrite the declared
    // start attribute; they set the binding equation value.
    if should_promote_binding_to_start(comp, &attrs, binding_from_modification) && binding.is_some()
    {
        attrs.start = binding.clone();
    }

    Ok(ComponentAttrsAndBinding {
        attrs,
        binding,
        binding_source,
        binding_source_scope,
        binding_from_modification,
    })
}

pub(super) fn infer_local_attribute_source_scopes(
    ctx: &InstantiateContext,
    comp: &ast::Component,
    attrs: &mut ExtractedAttributes,
) {
    let declaration_scope = component_declaration_source_scope(ctx, comp);
    for (attr_name, value) in &comp.modifications {
        if !preserves_attribute_source_scope(attr_name) {
            continue;
        }
        if !attribute_needs_written_scope(ctx, value, declaration_scope.as_ref()) {
            continue;
        }
        if extracted_attribute_expr(attrs, attr_name).is_some_and(|expr| expr == value) {
            insert_attribute_source_scope(ctx, &mut attrs.source_scopes, attr_name, value);
        }
    }

    if attrs
        .start
        .as_ref()
        .is_some_and(|start| start == &comp.start)
        && attribute_needs_written_scope(ctx, &comp.start, declaration_scope.as_ref())
    {
        insert_attribute_source_scope(ctx, &mut attrs.source_scopes, "start", &comp.start);
    }
}

fn attribute_needs_written_scope(
    ctx: &InstantiateContext,
    expr: &ast::Expression,
    declaration_scope: Option<&ast::QualifiedName>,
) -> bool {
    let Some(written_scope) = expression_source_scope(ctx, expr) else {
        return false;
    };
    Some(&written_scope) != declaration_scope
}

fn preserves_attribute_source_scope(attr_name: &str) -> bool {
    matches!(attr_name, "start" | "min" | "max" | "nominal")
}

fn extracted_attribute_expr<'a>(
    attrs: &'a ExtractedAttributes,
    attr_name: &str,
) -> Option<&'a ast::Expression> {
    match attr_name {
        "start" => attrs.start.as_ref(),
        "min" => attrs.min.as_ref(),
        "max" => attrs.max.as_ref(),
        "nominal" => attrs.nominal.as_ref(),
        _ => None,
    }
}

fn insert_attribute_source_scope(
    ctx: &InstantiateContext,
    source_scopes: &mut IndexMap<String, ast::QualifiedName>,
    attr_name: &str,
    expr: &ast::Expression,
) {
    if source_scopes.contains_key(attr_name) {
        return;
    }
    if let Some(scope) = expression_source_scope(ctx, expr) {
        source_scopes.insert(attr_name.to_string(), scope);
    }
}

fn extract_string_attr_value_from_modification_expr(
    expr: &ast::Expression,
    attr_name: &str,
) -> Option<String> {
    match expr {
        ast::Expression::Modification { target, value, .. } => {
            let target_name = target.parts.last()?.ident.text.as_ref();
            if target_name == attr_name {
                expr_to_string(value)
            } else {
                None
            }
        }
        ast::Expression::NamedArgument { name, value, .. } => {
            if name.text.as_ref() == attr_name {
                expr_to_string(value)
            } else {
                None
            }
        }
        ast::Expression::ClassModification { modifications, .. } => modifications
            .iter()
            .find_map(|m| extract_string_attr_value_from_modification_expr(m, attr_name)),
        _ => None,
    }
}

fn has_all_type_string_attrs(attrs: &ExtractedAttributes) -> bool {
    attrs.quantity.is_some() && attrs.unit.is_some() && attrs.display_unit.is_some()
}

fn merge_missing_type_string_attrs_from_expr(
    attrs: &mut ExtractedAttributes,
    expr: &ast::Expression,
) {
    if attrs.quantity.is_none() {
        attrs.quantity = extract_string_attr_value_from_modification_expr(expr, "quantity");
    }
    if attrs.unit.is_none() {
        attrs.unit = extract_string_attr_value_from_modification_expr(expr, "unit");
    }
    if attrs.display_unit.is_none() {
        attrs.display_unit = extract_string_attr_value_from_modification_expr(expr, "displayUnit");
    }
}

/// Fill missing quantity/unit/displayUnit attributes from the declared type hierarchy.
///
/// This is needed for Modelica type aliases (for example
/// `type Resistance = Real(final unit="Ohm", ...)`) where the attribute is defined
/// on the type's `extends` chain rather than on the component declaration itself.
pub(super) fn merge_type_hierarchy_string_attributes(
    tree: &ast::ClassTree,
    class_def: Option<&ast::ClassDef>,
    attrs: &mut ExtractedAttributes,
) {
    if has_all_type_string_attrs(attrs) {
        return;
    }

    let mut stack: Vec<&ast::ClassDef> = class_def.into_iter().collect();
    let mut visited = std::collections::HashSet::<DefId>::new();

    while let Some(class) = stack.pop() {
        if let Some(def_id) = class.def_id
            && !visited.insert(def_id)
        {
            continue;
        }

        for ext in &class.extends {
            for modification in &ext.modifications {
                merge_missing_type_string_attrs_from_expr(attrs, &modification.expr);
            }
        }

        if has_all_type_string_attrs(attrs) {
            break;
        }

        for ext in &class.extends {
            let base_name = ext.base_name.to_string();
            if let Some(base_class) = ext
                .base_def_id
                .and_then(|def_id| tree.get_class_by_def_id(def_id))
                .or_else(|| find_class_in_tree(tree, &base_name))
            {
                stack.push(base_class);
            }
        }
    }
}

pub(super) fn validate_final_type_attribute_overrides(
    tree: &ast::ClassTree,
    class_def: Option<&ast::ClassDef>,
    comp: &ast::Component,
    mod_env: &ast::ModificationEnvironment,
) -> InstantiateResult<()> {
    let final_attrs = final_type_attribute_names(tree, class_def);
    for attr_name in comp.final_attributes.iter().chain(final_attrs.iter()) {
        let attr_path = ast::QualifiedName::from_ident(&comp.name).child(attr_name);
        if let Some(mod_value) = mod_env.get(&attr_path) {
            let span = required_modifier_value_span(mod_value, "final type attribute override")?;
            return Err(Box::new(InstantiateError::redeclare_final(
                format!("{}.{}", comp.name, attr_name),
                span,
            )));
        }
    }
    Ok(())
}

fn required_modifier_value_span(
    mod_value: &ast::ModificationValue,
    context: &'static str,
) -> InstantiateResult<rumoca_core::Span> {
    let expr = mod_value.source.as_ref().unwrap_or(&mod_value.value);
    let span = expr.span();
    if span.is_dummy() {
        return Err(Box::new(InstantiateError::missing_source_context(format!(
            "{context} is missing source provenance"
        ))));
    }
    Ok(span)
}

fn final_type_attribute_names(
    tree: &ast::ClassTree,
    class_def: Option<&ast::ClassDef>,
) -> indexmap::IndexSet<String> {
    let mut final_attrs = indexmap::IndexSet::new();
    let mut stack: Vec<&ast::ClassDef> = class_def.into_iter().collect();
    let mut visited = std::collections::HashSet::<DefId>::new();

    while let Some(class) = stack.pop() {
        if let Some(def_id) = class.def_id
            && !visited.insert(def_id)
        {
            continue;
        }
        for ext in &class.extends {
            for modification in &ext.modifications {
                insert_final_type_attribute_name(&mut final_attrs, modification);
            }

            let base_name = ext.base_name.to_string();
            if let Some(base_class) = ext
                .base_def_id
                .and_then(|def_id| tree.get_class_by_def_id(def_id))
                .or_else(|| find_class_in_tree(tree, &base_name))
            {
                stack.push(base_class);
            }
        }
    }

    final_attrs
}

fn insert_final_type_attribute_name(
    final_attrs: &mut indexmap::IndexSet<String>,
    modification: &ast::ExtendModification,
) {
    if !modification.final_ {
        return;
    }
    if let Some(attr_name) = type_attribute_modification_name(&modification.expr) {
        final_attrs.insert(attr_name);
    }
}

fn type_attribute_modification_name(expr: &ast::Expression) -> Option<String> {
    match expr {
        ast::Expression::Modification { target, .. }
        | ast::Expression::ClassModification { target, .. } => {
            target.parts.first().map(|part| part.ident.text.to_string())
        }
        ast::Expression::NamedArgument { name, .. } => Some(name.text.to_string()),
        _ => None,
    }
}

fn should_promote_binding_to_start(
    comp: &ast::Component,
    attrs: &ExtractedAttributes,
    binding_from_modification: bool,
) -> bool {
    matches!(
        comp.variability,
        rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
    ) && !attrs.start_is_explicit
        && !binding_from_modification
}

/// Extract attributes from a component's modifications.
///
/// MLS §4.9: Attributes like start, fixed, min, max, nominal, quantity, unit,
/// displayUnit, stateSelect can be specified via modifications.
///
/// MLS §7.2: The modification environment is checked for overriding
/// modifications from outer scopes. Outer modifications override inner ones per
/// MLS §7.2.4.
#[cfg(test)]
pub(super) fn extract_attributes(
    comp: &ast::Component,
    mod_env: &ast::ModificationEnvironment,
    comp_name: &str,
    instance_name: &str,
    eval_ctx: &InstantiateEvalCtx<'_>,
    imports: &[(String, String)],
) -> InstantiateResult<ExtractedAttributes> {
    extract_attributes_in_scope(
        comp,
        mod_env,
        comp_name,
        instance_name,
        eval_ctx,
        imports,
        None,
    )
}

pub(super) fn extract_attributes_in_scope(
    comp: &ast::Component,
    mod_env: &ast::ModificationEnvironment,
    comp_name: &str,
    _instance_name: &str,
    eval_ctx: &InstantiateEvalCtx<'_>,
    imports: &[(String, String)],
    declaration_scope: Option<&ast::QualifiedName>,
) -> InstantiateResult<ExtractedAttributes> {
    let mut source_scopes = IndexMap::default();
    let start_path = ast::QualifiedName::from_ident(comp_name).child("start");
    let start_from_mod_env = mod_env.get(&start_path).map(|value| {
        if let Some(scope) = value.source_scope.clone() {
            source_scopes.insert("start".to_string(), scope);
        }
        value.value.clone()
    });
    let mut attr_from_mod_env = |attr_name: &str| {
        let path = ast::QualifiedName::from_ident(comp_name).child(attr_name);
        let value = mod_env.get(&path)?;
        if let Some(scope) = value.source_scope.clone() {
            source_scopes.insert(attr_name.to_string(), scope);
        }
        Some(value.value.clone())
    };

    let state_select_path = ast::QualifiedName::from_ident(comp_name).child("stateSelect");
    let outer_state_select = mod_env.get(&state_select_path);
    let outer_state_select = match outer_state_select {
        Some(value) => Some(parse_required_state_select(
            &value.value,
            eval_ctx,
            imports,
            value.source_scope.as_ref(),
            declaration_scope,
        )?),
        None => None,
    };
    let has_outer_state_select = outer_state_select.is_some();
    let mut attrs = ExtractedAttributes {
        start_is_explicit: start_from_mod_env.is_some(),
        start: start_from_mod_env,
        fixed: mod_env.get_attr(comp_name, "fixed").and_then(expr_to_bool),
        min: attr_from_mod_env("min"),
        max: attr_from_mod_env("max"),
        nominal: attr_from_mod_env("nominal"),
        source_scopes,
        quantity: mod_env
            .get_attr(comp_name, "quantity")
            .and_then(expr_to_string),
        unit: mod_env.get_attr(comp_name, "unit").and_then(expr_to_string),
        display_unit: mod_env
            .get_attr(comp_name, "displayUnit")
            .and_then(expr_to_string),
        state_select: outer_state_select.unwrap_or_default(),
    };

    for (name, value) in &comp.modifications {
        match name.as_str() {
            "start" if attrs.start.is_none() => {
                attrs.start = Some(value.clone());
                attrs.start_is_explicit = true;
            }
            "fixed" if attrs.fixed.is_none() => attrs.fixed = expr_to_bool(value),
            "min" if attrs.min.is_none() => attrs.min = Some(value.clone()),
            "max" if attrs.max.is_none() => attrs.max = Some(value.clone()),
            "nominal" if attrs.nominal.is_none() => attrs.nominal = Some(value.clone()),
            "quantity" if attrs.quantity.is_none() => attrs.quantity = expr_to_string(value),
            "unit" if attrs.unit.is_none() => attrs.unit = expr_to_string(value),
            "displayUnit" if attrs.display_unit.is_none() => {
                attrs.display_unit = expr_to_string(value)
            }
            "stateSelect" if !has_outer_state_select => {
                attrs.state_select =
                    parse_required_state_select(value, eval_ctx, imports, None, declaration_scope)?
            }
            _ => {}
        }
    }

    if attrs.start.is_none() && !matches!(comp.start, ast::Expression::Empty { .. }) {
        attrs.start = Some(comp.start.clone());
        attrs.start_is_explicit = comp.start_is_modification;
    }

    Ok(attrs)
}

fn parse_required_state_select(
    value: &ast::Expression,
    eval_ctx: &InstantiateEvalCtx<'_>,
    imports: &[(String, String)],
    source_scope: Option<&ast::QualifiedName>,
    declaration_scope: Option<&ast::QualifiedName>,
) -> InstantiateResult<rumoca_core::StateSelect> {
    parse_state_select(value)
        .or_else(|| eval_state_select_expr_with_source_scope(eval_ctx, value, source_scope))
        .or_else(|| eval_state_select_expr_with_source_scope(eval_ctx, value, declaration_scope))
        .or_else(|| {
            // Enclosing-scope constants (MLS §5.3.2) appear unqualified in
            // declaration-side attributes; qualify them through the package
            // constant aliases and retry before failing.
            let qualified = crate::dims::qualify_shape_expr_imports(value, imports);
            eval_state_select_expr_with_source_scope(eval_ctx, &qualified, source_scope).or_else(
                || {
                    eval_state_select_expr_with_source_scope(
                        eval_ctx,
                        &qualified,
                        declaration_scope,
                    )
                },
            )
        })
        .ok_or_else(|| {
            Box::new(InstantiateError::InvalidTypeAttribute {
                attribute: "stateSelect".to_string(),
                value: value.to_string(),
                span: rumoca_core::span_to_source_span(value.span()),
            })
        })
}
