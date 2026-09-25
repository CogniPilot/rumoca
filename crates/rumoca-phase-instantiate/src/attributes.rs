use super::*;
use rumoca_eval_ast::eval_instantiate::{
    InstantiateEvalCtx, eval_boolean_attribute_values, eval_state_select_expr_with_source_scope,
    expr_to_string, parse_state_select,
};

pub(super) struct ComponentAttrsAndBinding {
    pub(super) attrs: ExtractedAttributes,
    pub(super) binding: Option<ast::Expression>,
    pub(super) binding_source: Option<ast::Expression>,
    pub(super) binding_source_scope: Option<ast::QualifiedName>,
    pub(super) binding_from_modification: bool,
    pub(super) binding_is_each: bool,
}

pub(super) fn extract_component_attrs_and_binding(
    comp: &ast::Component,
    mod_env: &ast::ModificationEnvironment,
    eval_ctx: &InstantiateEvalCtx<'_>,
    imports: &[(String, String)],
) -> InstantiateResult<ComponentAttrsAndBinding> {
    // Pass component name so mod_env can be checked for outer modifications.
    let mut attrs = extract_attributes(comp, mod_env, &comp.name, eval_ctx, imports)?;
    let (binding, binding_from_modification, binding_source_scope) = extract_binding(comp, mod_env);
    let binding_path = ast::QualifiedName::from_ident(&comp.name);
    let binding_modification = binding_from_modification
        .then(|| mod_env.get(&binding_path))
        .flatten();
    let binding_source = binding_modification.and_then(|value| value.source.clone());
    let binding_is_each = binding_modification.is_some_and(|value| value.each);

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
        binding_is_each,
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

/// The classes of a declared type chain, innermost first (MLS §4.8.1).
///
/// A short class definition `type A = B(...)` and a long `type A extends B(...)`
/// both appear as an `extends` of the base, so the chain is the depth-first
/// order of those base classes. That order is also the precedence of their
/// modifications: a modification in a derived class overrides the one in its
/// base (MLS §7.2.3).
fn type_chain<'a>(
    tree: &'a ast::ClassTree,
    class_def: Option<&'a ast::ClassDef>,
) -> Vec<&'a ast::ClassDef> {
    let mut chain = Vec::new();
    let mut stack: Vec<&ast::ClassDef> = class_def.into_iter().collect();
    let mut visited = std::collections::HashSet::<DefId>::new();
    while let Some(class) = stack.pop() {
        if let Some(def_id) = class.def_id
            && !visited.insert(def_id)
        {
            continue;
        }
        chain.push(class);
        for ext in class.extends.iter().rev() {
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
    chain
}

/// A single attribute modification `name = value` of a type chain element.
fn type_attribute_modification(expr: &ast::Expression) -> Option<(&str, &ast::Expression)> {
    match expr {
        ast::Expression::Modification { target, value, .. } if target.parts.len() == 1 => {
            Some((target.parts[0].ident.text.as_ref(), value))
        }
        ast::Expression::NamedArgument { name, value, .. } => Some((name.text.as_ref(), value)),
        _ => None,
    }
}

/// The component whose attributes the declared type chain completes.
pub(super) struct TypeAttributeMerge<'a, 'e> {
    pub(super) ctx: &'a InstantiateContext,
    pub(super) comp: &'a ast::Component,
    pub(super) class_def: Option<&'a ast::ClassDef>,
    pub(super) eval_ctx: &'a InstantiateEvalCtx<'e>,
}

/// Array ranks of attribute values taken from an array type.
///
/// A value written in `type V = Real[3](start = {1, 2, 3})` has the shape of
/// `V`, so a component `V w[2]` repeats it over its own leading dimensions.
/// Values written with `each`, or in a scalar type, broadcast unchanged.
#[derive(Debug, Default)]
pub(super) struct TypeAttributeShapes {
    ranks: IndexMap<String, usize>,
}

/// Fill the attributes that neither an outer modification nor the component
/// declaration sets from the declared type chain (MLS §4.8.1, §4.9).
///
/// The component modifier wins, then the innermost type definition, then its
/// base types in order (MLS §7.2.3). Only components of a predefined type
/// carry these attributes, so the caller applies this to primitive components.
/// Each value is evaluated in the scope it was written in: the type
/// definition, not the component declaration.
pub(super) fn merge_type_hierarchy_attributes(
    merge: &TypeAttributeMerge<'_, '_>,
    attrs: &mut ExtractedAttributes,
) -> InstantiateResult<TypeAttributeShapes> {
    let state_select_path = ast::QualifiedName::from_ident(&merge.comp.name).child("stateSelect");
    let mut state_select_set = merge.comp.modifications.contains_key("stateSelect")
        || merge.eval_ctx.mod_env.get(&state_select_path).is_some();
    let declaration_scope = component_declaration_source_scope(merge.ctx, merge.comp);
    let chain = type_chain(merge.eval_ctx.tree, merge.class_def);
    let mut shapes = TypeAttributeShapes::default();
    // A value's rank is the dimension count of the class that wrote it: its
    // own subscripts plus those of its bases.
    let modifications = chain.iter().enumerate().flat_map(|(index, class)| {
        let value_rank = chain[index..]
            .iter()
            .map(|class| class.array_subscripts.len())
            .sum::<usize>();
        class
            .extends
            .iter()
            .flat_map(|ext| &ext.modifications)
            .filter(|modification| !modification.redeclare)
            .map(move |modification| (value_rank, modification))
    });
    for (value_rank, modification) in modifications {
        let Some((name, value)) = type_attribute_modification(&modification.expr) else {
            continue;
        };
        let written_scope = expression_source_scope(merge.ctx, value);
        if name == "stateSelect" {
            if !state_select_set {
                attrs.state_select = parse_required_state_select(
                    value,
                    merge.eval_ctx,
                    &[],
                    written_scope.as_ref(),
                )?;
                state_select_set = true;
            }
            continue;
        }
        let value = TypeAttributeValue {
            name,
            value,
            written_scope: written_scope.as_ref(),
            declaration_scope: declaration_scope.as_ref(),
        };
        if merge_type_attribute(merge.eval_ctx, attrs, &value)?
            && !modification.each
            && value_rank > 0
        {
            shapes.ranks.insert(name.to_string(), value_rank);
        }
    }
    Ok(shapes)
}

struct TypeAttributeValue<'a> {
    name: &'a str,
    value: &'a ast::Expression,
    written_scope: Option<&'a ast::QualifiedName>,
    declaration_scope: Option<&'a ast::QualifiedName>,
}

/// Set one attribute from the type chain unless it is already set; returns
/// whether it was set.
fn merge_type_attribute(
    eval_ctx: &InstantiateEvalCtx<'_>,
    attrs: &mut ExtractedAttributes,
    attr: &TypeAttributeValue<'_>,
) -> InstantiateResult<bool> {
    let slot = match attr.name {
        "start" => &mut attrs.start,
        "min" => &mut attrs.min,
        "max" => &mut attrs.max,
        "nominal" => &mut attrs.nominal,
        "quantity" if attrs.quantity.is_none() => {
            attrs.quantity = expr_to_string(attr.value);
            return Ok(false);
        }
        "unit" if attrs.unit.is_none() => {
            attrs.unit = expr_to_string(attr.value);
            return Ok(false);
        }
        "displayUnit" if attrs.display_unit.is_none() => {
            attrs.display_unit = expr_to_string(attr.value);
            return Ok(false);
        }
        "fixed" if attrs.fixed.is_none() => {
            attrs.fixed = Some(parse_required_fixed(
                attr.value,
                eval_ctx,
                &[],
                attr.written_scope,
            )?);
            return Ok(true);
        }
        _ => return Ok(false),
    };
    if slot.is_some() {
        return Ok(false);
    }
    *slot = Some(attr.value.clone());
    if attr.name == "start" {
        attrs.start_is_explicit = true;
    }
    if let Some(scope) = attr.written_scope
        && Some(scope) != attr.declaration_scope
        && !attrs.source_scopes.contains_key(attr.name)
    {
        attrs
            .source_scopes
            .insert(attr.name.to_string(), scope.clone());
    }
    Ok(true)
}

/// Repeat array-type attribute values over the component's own leading
/// dimensions (see [`TypeAttributeShapes`]).
pub(super) fn broadcast_type_attribute_values(
    shapes: &TypeAttributeShapes,
    dims: &[i64],
    attrs: &mut ExtractedAttributes,
) {
    for (name, &rank) in &shapes.ranks {
        let Some(leading) = dims.len().checked_sub(rank).map(|count| &dims[..count]) else {
            continue;
        };
        let Some(leading) = leading
            .iter()
            .map(|&dim| usize::try_from(dim).ok())
            .collect::<Option<Vec<_>>>()
        else {
            continue;
        };
        if leading.is_empty() {
            continue;
        }
        if name == "fixed" {
            if let Some(fixed) = attrs.fixed.as_mut() {
                *fixed = fixed.repeat(leading.iter().product());
            }
            continue;
        }
        let slot = match name.as_str() {
            "start" => &mut attrs.start,
            "min" => &mut attrs.min,
            "max" => &mut attrs.max,
            "nominal" => &mut attrs.nominal,
            _ => continue,
        };
        if let Some(value) = slot.take() {
            *slot = Some(repeat_over_dims(value, &leading));
        }
    }
}

fn repeat_over_dims(value: ast::Expression, leading: &[usize]) -> ast::Expression {
    let span = value.span();
    leading
        .iter()
        .rev()
        .fold(value, |element, &count| ast::Expression::Array {
            elements: vec![element; count],
            kind: rumoca_core::ArrayConstructor::Array,
            span,
        })
}

pub(super) fn validate_final_type_attribute_overrides(
    tree: &ast::ClassTree,
    class_def: Option<&ast::ClassDef>,
    comp: &ast::Component,
    mod_env: &ast::ModificationEnvironment,
) -> InstantiateResult<()> {
    let final_attrs = final_type_attribute_names(tree, class_def)?;
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
    // MLS §7.2.6: the declaration's own modifier is a modification too, so it
    // cannot override an attribute the declared type made final.
    for attr_name in &final_attrs {
        if let Some(value) = comp.modifications.get(attr_name) {
            return Err(final_override_error(
                format!("{}.{}", comp.name, attr_name),
                value,
            ));
        }
    }
    Ok(())
}

fn final_override_error(name: String, value: &ast::Expression) -> Box<InstantiateError> {
    let span = value.span();
    if span.is_dummy() {
        return Box::new(InstantiateError::missing_source_context(
            "final type attribute override is missing source provenance",
        ));
    }
    Box::new(InstantiateError::redeclare_final(name, span))
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

/// Names the declared type chain makes final, rejecting a derived type that
/// modifies an element a base type made final (MLS §7.2.6).
fn final_type_attribute_names(
    tree: &ast::ClassTree,
    class_def: Option<&ast::ClassDef>,
) -> InstantiateResult<indexmap::IndexSet<String>> {
    let mut final_attrs = indexmap::IndexSet::new();
    // Modifications written by the classes already walked. While every class
    // walked has a single base, they all derive from the classes still to come;
    // past a class with several bases the walk order no longer implies that.
    let mut derived_modifications = IndexMap::<String, &ast::Expression>::default();
    let mut single_base_chain = true;
    for class in type_chain(tree, class_def) {
        let modifications = class.extends.iter().flat_map(|ext| &ext.modifications);
        for modification in modifications.clone() {
            let Some(attr_name) = type_attribute_modification_name(&modification.expr) else {
                continue;
            };
            if !modification.final_ {
                continue;
            }
            if single_base_chain && let Some(expr) = derived_modifications.get(&attr_name) {
                return Err(final_override_error(attr_name, expr));
            }
            final_attrs.insert(attr_name);
        }
        single_base_chain &= class.extends.len() <= 1;
        for modification in modifications {
            if let Some(attr_name) = type_attribute_modification_name(&modification.expr) {
                derived_modifications
                    .entry(attr_name)
                    .or_insert(&modification.expr);
            }
        }
    }
    Ok(final_attrs)
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
pub(super) fn extract_attributes(
    comp: &ast::Component,
    mod_env: &ast::ModificationEnvironment,
    comp_name: &str,
    eval_ctx: &InstantiateEvalCtx<'_>,
    imports: &[(String, String)],
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
        )?),
        None => None,
    };
    let has_outer_state_select = outer_state_select.is_some();
    let mut attrs = ExtractedAttributes {
        start_is_explicit: start_from_mod_env.is_some(),
        start: start_from_mod_env,
        fixed: extract_fixed_attribute(comp, comp_name, eval_ctx, imports)?,
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
            "min" if attrs.min.is_none() => attrs.min = Some(value.clone()),
            "max" if attrs.max.is_none() => attrs.max = Some(value.clone()),
            "nominal" if attrs.nominal.is_none() => attrs.nominal = Some(value.clone()),
            "quantity" if attrs.quantity.is_none() => attrs.quantity = expr_to_string(value),
            "unit" if attrs.unit.is_none() => attrs.unit = expr_to_string(value),
            "displayUnit" if attrs.display_unit.is_none() => {
                attrs.display_unit = expr_to_string(value)
            }
            "stateSelect" if !has_outer_state_select => {
                attrs.state_select = parse_required_state_select(value, eval_ctx, imports, None)?
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

fn extract_fixed_attribute(
    comp: &ast::Component,
    comp_name: &str,
    eval_ctx: &InstantiateEvalCtx<'_>,
    imports: &[(String, String)],
) -> InstantiateResult<Option<Vec<bool>>> {
    let path = ast::QualifiedName::from_ident(comp_name).child("fixed");
    match eval_ctx.mod_env.get(&path) {
        Some(value) => {
            parse_required_fixed(&value.value, eval_ctx, imports, value.source_scope.as_ref())
                .map(Some)
        }
        None => comp
            .modifications
            .get("fixed")
            .map(|value| parse_required_fixed(value, eval_ctx, imports, None))
            .transpose(),
    }
}

fn parse_required_fixed(
    value: &ast::Expression,
    eval_ctx: &InstantiateEvalCtx<'_>,
    imports: &[(String, String)],
    source_scope: Option<&ast::QualifiedName>,
) -> InstantiateResult<Vec<bool>> {
    eval_boolean_attribute_values(eval_ctx, value, source_scope)
        .or_else(|| {
            let qualified = crate::dims::qualify_shape_expr_imports(eval_ctx.tree, value, imports);
            eval_boolean_attribute_values(eval_ctx, &qualified, source_scope)
        })
        .ok_or_else(|| {
            Box::new(InstantiateError::UnsupportedFixedAttribute {
                value: value.to_string(),
                span: value.span(),
            })
        })
}

fn parse_required_state_select(
    value: &ast::Expression,
    eval_ctx: &InstantiateEvalCtx<'_>,
    imports: &[(String, String)],
    source_scope: Option<&ast::QualifiedName>,
) -> InstantiateResult<rumoca_core::StateSelect> {
    parse_state_select(value)
        .or_else(|| eval_state_select_expr_with_source_scope(eval_ctx, value, source_scope))
        .or_else(|| {
            // Enclosing-scope constants (MLS §5.3.2) appear unqualified in
            // declaration-side attributes; qualify them through the package
            // constant aliases and retry before failing.
            let qualified = crate::dims::qualify_shape_expr_imports(eval_ctx.tree, value, imports);
            eval_state_select_expr_with_source_scope(eval_ctx, &qualified, source_scope)
        })
        .ok_or_else(|| {
            Box::new(InstantiateError::InvalidTypeAttribute {
                attribute: "stateSelect".to_string(),
                value: value.to_string(),
                span: value.span(),
            })
        })
}
