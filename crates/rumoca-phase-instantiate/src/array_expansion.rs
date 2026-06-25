use super::inheritance::resolve_effective_components_for_eval;
use super::instantiate_component;
use super::source_scope::component_declaration_source_scope;
use super::type_overrides::TypeOverrideMap;
use super::{InstantiateContext, InstantiateResult, find_class_in_tree, get_effective_components};
use rumoca_eval_ast::eval_instantiate::{InstantiateEvalCtx, try_eval_integer_expr};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use std::sync::Arc;

/// Expand an array component into individual indexed instances.
///
/// MLS §10.1: Array component expansion creates indexed instances.
/// Also registers the parent array path with its dimensions for use in
/// array equation expansion (MLS §10.5).
pub(super) struct ArrayExpansionScope<'a> {
    pub(super) tree: &'a ast::ClassTree,
    pub(super) effective_components: &'a IndexMap<String, ast::Component>,
    pub(super) type_overrides: &'a TypeOverrideMap,
    pub(super) imports: super::ComponentImports<'a>,
}

pub(super) fn expand_array_component(
    scope: &ArrayExpansionScope<'_>,
    name: &str,
    comp: &ast::Component,
    dims: &[i64],
    ctx: &mut InstantiateContext,
    overlay: &mut rumoca_ir_ast::InstanceOverlay,
) -> InstantiateResult<()> {
    // Register the array parent dimensions for use in array equation expansion.
    // When plug_p.pin[3] is expanded, we register "prefix.plug_p.pin" -> [3]
    // so that equations like v = plug_p.pin.v can be properly expanded.
    ctx.push_path(name);
    let parent_path = ctx.current_path().to_string();
    ctx.pop_path();
    overlay.array_parent_dims.insert(parent_path, dims.to_vec());

    let indices = super::generate_array_indices(dims);
    let active_nested_mod_keys = active_nested_modifier_keys(ctx.mod_env(), name);

    // Extract the original binding for indexing. Check active component modifier first,
    // then comp.binding, then fall back to comp.start for modification-only declarations.
    let binding_qn = ast::QualifiedName::from_ident(name);
    let mod_env_binding = ctx.mod_env().get(&binding_qn).cloned();

    // Check comp.binding first,
    // then fall back to comp.start with has_explicit_binding (modification-only declarations).
    // We only READ the binding here - do not clear comp.start or has_explicit_binding,
    // since they may be needed for dimension inference in typecheck.
    let original_binding = mod_env_binding
        .as_ref()
        .map(|mv| mv.value.clone())
        .or_else(|| {
            comp.binding.clone().or_else(|| {
                if comp.has_explicit_binding
                    && !comp.start_is_modification
                    && !matches!(comp.start, ast::Expression::Empty { .. })
                {
                    Some(comp.start.clone())
                } else {
                    None
                }
            })
        });
    let binding_source_scope = mod_env_binding
        .as_ref()
        .and_then(|mv| mv.source_scope.clone())
        .or_else(|| component_declaration_source_scope(ctx, comp));
    let modifier_state = ArrayElementModifierState {
        binding_qn,
        mod_env_binding,
        binding_source_scope,
        active_nested_mod_keys,
    };

    // MLS §7.2.5: Pre-resolve non-`each` modifications that reference array values.
    // Resolve once so each element can be indexed from the resolved array.
    // Also clear parent-scope entries for these modifications so distributed scalar
    // values are not shadowed during populate_modification_environment.
    let resolved_mods = pre_resolve_array_modifications(
        comp,
        ctx.mod_env(),
        scope.effective_components,
        scope.tree,
    );
    let resolved_mod_names: std::collections::HashSet<&str> = resolved_mods
        .iter()
        .map(|(name, _)| name.as_str())
        .collect();
    for (mod_name, _) in &resolved_mods {
        let qn = ast::QualifiedName::from_ident(mod_name);
        ctx.mod_env_mut().active.shift_remove(&qn);
    }

    // Create indexed components.
    let mut scalar_comp = comp.clone();
    scalar_comp.shape = vec![];
    scalar_comp.shape_expr = vec![];

    for idx in indices {
        scalar_comp.start =
            indexed_array_component_start(scope.tree, scope.effective_components, comp, &idx)?;

        // MLS §10.1: When an array component has a binding (e.g., `v1[m] = plug1.pin.v`),
        // each expanded element `v1[k]` gets the indexed binding `plug1.pin[k].v`.
        // This is essential for propagating bindings through array-of-record types
        // (e.g., Complex output arrays in QS transformer models).
        if let Some(ref binding) = original_binding {
            scalar_comp.binding = Some(index_binding_for_element(
                scope.tree,
                scope.effective_components,
                binding,
                &idx,
            )?);
        }

        // MLS §7.2.5: Distribute non-`each` modifications per array element.
        distribute_mods_for_element(&mut scalar_comp, &resolved_mods, &idx);
        distribute_component_ref_mods_for_element(
            &mut scalar_comp,
            comp,
            &resolved_mod_names,
            scope.tree,
            scope.effective_components,
            &idx,
        )?;

        instantiate_scalar_array_element(
            scope,
            name,
            &scalar_comp,
            &idx,
            ctx,
            overlay,
            &modifier_state,
        )?;
    }

    Ok(())
}

struct ArrayElementModifierState {
    binding_qn: ast::QualifiedName,
    mod_env_binding: Option<ast::ModificationValue>,
    binding_source_scope: Option<ast::QualifiedName>,
    active_nested_mod_keys: Vec<ast::QualifiedName>,
}

fn instantiate_scalar_array_element(
    scope: &ArrayExpansionScope<'_>,
    name: &str,
    scalar_comp: &ast::Component,
    idx: &[i64],
    ctx: &mut InstantiateContext,
    overlay: &mut rumoca_ir_ast::InstanceOverlay,
    modifier_state: &ArrayElementModifierState,
) -> InstantiateResult<()> {
    let previous_binding =
        install_array_element_binding_override(scope, scalar_comp, idx, ctx, modifier_state)?;
    let previous_nested_mods = distribute_active_nested_mods_for_element(
        scope,
        ctx,
        &modifier_state.active_nested_mod_keys,
        idx,
    )?;

    ctx.push_path_part(name, idx.to_vec());
    let inst_result = instantiate_component(
        scope.tree,
        scalar_comp,
        ctx,
        overlay,
        scope.effective_components,
        scope.type_overrides,
        scope.imports,
    );
    ctx.pop_path();

    restore_array_element_binding(ctx, modifier_state, previous_binding);
    restore_active_nested_mods(ctx, previous_nested_mods);
    inst_result
}

fn install_array_element_binding_override(
    scope: &ArrayExpansionScope<'_>,
    scalar_comp: &ast::Component,
    idx: &[i64],
    ctx: &mut InstantiateContext,
    modifier_state: &ArrayElementModifierState,
) -> InstantiateResult<Option<ast::ModificationValue>> {
    let previous_binding = ctx
        .mod_env()
        .active
        .get(&modifier_state.binding_qn)
        .cloned();
    if let Some(binding_expr) = &scalar_comp.binding
        && modifier_state.mod_env_binding.is_some()
    {
        ctx.mod_env_mut().active.insert(
            modifier_state.binding_qn.clone(),
            array_element_binding_modification(
                scope,
                binding_expr,
                idx,
                modifier_state.mod_env_binding.as_ref(),
                modifier_state.binding_source_scope.clone(),
            )?,
        );
    }
    Ok(previous_binding)
}

fn restore_array_element_binding(
    ctx: &mut InstantiateContext,
    modifier_state: &ArrayElementModifierState,
    previous_binding: Option<ast::ModificationValue>,
) {
    match previous_binding {
        Some(prev) => {
            ctx.mod_env_mut()
                .active
                .insert(modifier_state.binding_qn.clone(), prev);
        }
        None => {
            ctx.mod_env_mut()
                .active
                .shift_remove(&modifier_state.binding_qn);
        }
    }
}

fn active_nested_modifier_keys(
    mod_env: &ast::ModificationEnvironment,
    component_name: &str,
) -> Vec<ast::QualifiedName> {
    mod_env
        .active
        .keys()
        .filter(|key| key.strip_prefix(component_name).is_some())
        .cloned()
        .collect()
}

fn distribute_active_nested_mods_for_element(
    scope: &ArrayExpansionScope<'_>,
    ctx: &mut InstantiateContext,
    keys: &[ast::QualifiedName],
    idx: &[i64],
) -> InstantiateResult<Vec<(ast::QualifiedName, Option<ast::ModificationValue>)>> {
    let mut previous = Vec::with_capacity(keys.len());
    for key in keys {
        let Some(existing) = ctx.mod_env().active.get(key).cloned() else {
            previous.push((key.clone(), None));
            continue;
        };
        if existing.each {
            continue;
        }
        previous.push((key.clone(), Some(existing.clone())));
        let value = index_binding_for_element(
            scope.tree,
            scope.effective_components,
            &existing.value,
            idx,
        )?;
        let source = existing
            .source
            .as_ref()
            .map(|source| {
                index_binding_for_element(scope.tree, scope.effective_components, source, idx)
            })
            .transpose()?;
        ctx.mod_env_mut().active.insert(
            key.clone(),
            ast::ModificationValue {
                value,
                source,
                source_scope: existing.source_scope,
                each: existing.each,
                final_: existing.final_,
            },
        );
    }
    Ok(previous)
}

fn restore_active_nested_mods(
    ctx: &mut InstantiateContext,
    previous: Vec<(ast::QualifiedName, Option<ast::ModificationValue>)>,
) {
    for (key, value) in previous {
        match value {
            Some(value) => {
                ctx.mod_env_mut().active.insert(key, value);
            }
            None => {
                ctx.mod_env_mut().active.shift_remove(&key);
            }
        }
    }
}

fn indexed_array_component_start(
    tree: &ast::ClassTree,
    parent_components: &IndexMap<String, ast::Component>,
    comp: &ast::Component,
    idx: &[i64],
) -> InstantiateResult<ast::Expression> {
    if comp.start_has_each || matches!(comp.start, ast::Expression::Empty { .. }) {
        return Ok(comp.start.clone());
    }
    Ok(
        index_array_expression_for_element(tree, parent_components, &comp.start, idx)?
            .unwrap_or_else(|| comp.start.clone()),
    )
}

fn array_element_binding_modification(
    scope: &ArrayExpansionScope<'_>,
    binding_expr: &ast::Expression,
    idx: &[i64],
    parent_mod: Option<&ast::ModificationValue>,
    binding_source_scope: Option<ast::QualifiedName>,
) -> InstantiateResult<ast::ModificationValue> {
    let Some(parent_mod) = parent_mod else {
        return Ok(ast::ModificationValue::with_source_scope(
            binding_expr.clone(),
            Some(binding_expr.clone()),
            binding_source_scope,
        ));
    };
    let source = parent_mod
        .source
        .as_ref()
        .map(|source_expr| {
            index_binding_for_element(scope.tree, scope.effective_components, source_expr, idx)
        })
        .transpose()?
        .or_else(|| Some(binding_expr.clone()));
    let source_scope = parent_mod.source_scope.clone().or(binding_source_scope);
    Ok(ast::ModificationValue::with_source_scope(
        binding_expr.clone(),
        source,
        source_scope,
    ))
}

/// Pre-resolve non-`each` modifications that have array values.
///
/// MLS §7.2.5: Returns a list of (mod_name, resolved_array_expr) pairs for
/// modifications that can be distributed across array elements.
pub(super) fn pre_resolve_array_modifications(
    comp: &ast::Component,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Vec<(String, ast::Expression)> {
    let mut resolved = Vec::new();
    for (name, expr) in &comp.modifications {
        if comp.each_modifications.contains(name) {
            continue; // `each` modifications are not distributed
        }
        // Keep component-reference modifiers (for example `R=R`) in symbolic
        // form so per-element indexing can preserve owner scope (`R[1]`).
        // Eager resolution here can over-collapse to outer-source expressions
        // like `RRef.d`, losing the declaring component context.
        if matches!(expr, ast::Expression::ComponentReference(_)) {
            continue;
        }
        let val = resolve_mod_to_array(expr, mod_env, effective_components, tree);
        if matches!(val, ast::Expression::Array { .. }) {
            resolved.push((name.clone(), val));
        }
    }
    resolved
}

/// Apply pre-resolved array modifications to a scalar array element.
///
/// MLS §7.2.5: For each resolved array modification, indexes the array
/// to get the element-specific value.
pub(super) fn distribute_mods_for_element(
    scalar_comp: &mut ast::Component,
    resolved_mods: &[(String, ast::Expression)],
    indices: &[i64],
) {
    for (name, resolved) in resolved_mods {
        if let Some(indexed) = index_array_modification(resolved, indices) {
            scalar_comp.modifications.insert(name.clone(), indexed);
        }
    }
}

/// Distribute non-`each` component-reference modifications by indexing proven array parts.
///
/// This covers cases where a modifier is array-valued but not materialized as an
/// `ast::Expression::Array` during pre-resolution (e.g., `cellData=stackData.cellData`).
fn distribute_component_ref_mods_for_element(
    scalar_comp: &mut ast::Component,
    original_comp: &ast::Component,
    resolved_mod_names: &std::collections::HashSet<&str>,
    tree: &ast::ClassTree,
    parent_components: &IndexMap<String, ast::Component>,
    indices: &[i64],
) -> InstantiateResult<()> {
    for (name, expr) in &original_comp.modifications {
        if original_comp.each_modifications.contains(name)
            || resolved_mod_names.contains(name.as_str())
        {
            continue;
        }

        if let Some(indexed) =
            index_nested_modification_expr_for_element(tree, parent_components, expr, indices)?
        {
            scalar_comp.modifications.insert(name.clone(), indexed);
            continue;
        }

        let indexed = index_binding_for_element(tree, parent_components, expr, indices)?;
        if !matches!(indexed, ast::Expression::ArrayIndex { .. }) {
            scalar_comp.modifications.insert(name.clone(), indexed);
        }
    }
    Ok(())
}

fn index_nested_modification_expr_for_element(
    tree: &ast::ClassTree,
    parent_components: &IndexMap<String, ast::Component>,
    expr: &ast::Expression,
    indices: &[i64],
) -> InstantiateResult<Option<ast::Expression>> {
    match expr {
        ast::Expression::ClassModification {
            target,
            modifications,
            each_flags,
            final_flags,
            redeclare_flags,
            span,
        } => Ok(Some(ast::Expression::ClassModification {
            target: target.clone(),
            modifications: index_nested_modification_list_for_element(
                tree,
                parent_components,
                modifications,
                each_flags,
                indices,
            )?,
            each_flags: each_flags.clone(),
            final_flags: final_flags.clone(),
            redeclare_flags: redeclare_flags.clone(),
            span: *span,
        })),
        ast::Expression::Modification {
            target,
            value,
            span,
        } => Ok(Some(ast::Expression::Modification {
            target: target.clone(),
            value: Arc::new(index_binding_for_element(
                tree,
                parent_components,
                value,
                indices,
            )?),
            span: *span,
        })),
        _ => Ok(None),
    }
}

fn index_nested_modification_list_for_element(
    tree: &ast::ClassTree,
    parent_components: &IndexMap<String, ast::Component>,
    modifications: &[ast::Expression],
    each_flags: &[bool],
    indices: &[i64],
) -> InstantiateResult<Vec<ast::Expression>> {
    modifications
        .iter()
        .enumerate()
        .map(|(idx, nested)| {
            if each_flags.get(idx).copied().unwrap_or(false) {
                return Ok(nested.clone());
            }
            index_nested_modification_expr_for_element(tree, parent_components, nested, indices)
                .map(|indexed| indexed.unwrap_or_else(|| nested.clone()))
        })
        .collect()
}

/// Resolve a modification expression to its value, handling component references.
///
/// For component references like `R` that refer to array parameters in the parent
/// scope, this resolves them to their actual array values so they can be indexed.
pub(super) fn resolve_mod_to_array(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> ast::Expression {
    resolve_mod_to_array_depth(expr, mod_env, effective_components, tree, 0)
}

/// Resolve a modification expression to an array value with depth limit.
fn resolve_mod_to_array_depth(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    depth: usize,
) -> ast::Expression {
    const MAX_DEPTH: usize = 5;
    if depth >= MAX_DEPTH {
        return expr.clone();
    }
    if let ast::Expression::ComponentReference(cref) = expr
        && cref.parts.len() == 1
    {
        let name = cref.parts[0].ident.text.as_ref();
        if let Some(resolved) = resolve_single_ref(name, mod_env, effective_components, tree, depth)
        {
            return resolved;
        }
    }
    // MLS §11.1.2.1: Evaluate array comprehensions like {j for j in 1:m}
    if let Some(array) = try_eval_array_comprehension(expr, mod_env, effective_components, tree) {
        return array;
    }
    // Evaluate structural array-valued expressions used in non-`each` modifiers
    // (e.g., k=fill(1, n), phase=-symmetricOrientation(m)). This allows
    // per-element modifier distribution during array component expansion
    // (MLS §7.2.5).
    if let Some(array) = try_eval_structural_array_expr(expr, mod_env, effective_components, tree) {
        return array;
    }
    expr.clone()
}

/// Resolve a single-part component reference to an array value.
fn resolve_single_ref(
    name: &str,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    depth: usize,
) -> Option<ast::Expression> {
    // Check mod_env first
    let qn = ast::QualifiedName::from_ident(name);
    if let Some(mv) = mod_env.active.get(&qn) {
        let resolved =
            resolve_mod_to_array_depth(&mv.value, mod_env, effective_components, tree, depth + 1);
        if matches!(resolved, ast::Expression::Array { .. }) {
            return Some(resolved);
        }
    }
    // Check effective_components for array-valued bindings (including fill/zeros/ones).
    let comp = effective_components.get(name)?;
    if let Some(ref binding) = comp.binding {
        let resolved =
            resolve_mod_to_array_depth(binding, mod_env, effective_components, tree, depth + 1);
        if matches!(resolved, ast::Expression::Array { .. }) {
            return Some(resolved);
        }
    }
    let resolved_start =
        resolve_mod_to_array_depth(&comp.start, mod_env, effective_components, tree, depth + 1);
    if matches!(resolved_start, ast::Expression::Array { .. }) {
        return Some(resolved_start);
    }
    None
}

/// Evaluate simple structural array expressions to concrete 1-D arrays.
///
/// Supports:
/// - `fill(v, n)` -> `{v, v, ..., v}` (n elements)
/// - `zeros(n)` -> `{0, ..., 0}` (n elements)
/// - `ones(n)` -> `{1, ..., 1}` (n elements)
/// - unary signs over structural arrays
/// - MSL polyphase `symmetricOrientation(m)` structural phase vectors
///
/// This is intentionally limited to 1-D constructors because modifier
/// distribution currently indexes one dimension at a time.
fn try_eval_structural_array_expr(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<ast::Expression> {
    match expr {
        ast::Expression::Unary { op, rhs, span } => {
            let array = try_eval_structural_array_expr(rhs, mod_env, effective_components, tree)?;
            return apply_unary_to_structural_array(op, &array, *span);
        }
        ast::Expression::Parenthesized { inner, .. } => {
            return try_eval_structural_array_expr(inner, mod_env, effective_components, tree);
        }
        _ => {}
    }

    let ast::Expression::FunctionCall { comp, args, .. } = expr else {
        return None;
    };

    let call_span = expr.span();
    let func = comp
        .parts
        .last()
        .map(|part| part.ident.text.as_ref())
        .unwrap_or("");
    let make_int_lit = |value: i64| ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: rumoca_core::Token {
            text: value.to_string().into(),
            ..rumoca_core::Token::default()
        },
        span: call_span,
    };

    let make_repeated =
        |value: ast::Expression, count_expr: &ast::Expression| -> Option<ast::Expression> {
            let eval_ctx = InstantiateEvalCtx {
                tree,
                mod_env,
                effective_components,
                resolve_class_components: &resolve_effective_components_for_eval,
            };
            let n = try_eval_integer_expr(&eval_ctx, count_expr)?;
            let len = n.max(0) as usize;
            Some(ast::Expression::Array {
                elements: std::iter::repeat_n(value, len).collect(),
                is_matrix: false,
                span: call_span,
            })
        };

    // MLS §7.2.5: Resolve simple forwarded fill() element expressions in the
    // parent scope before distributing across array elements. This keeps forwarded
    // scalar parameters (e.g. fill(useHeatPort, m)) concrete after per-element
    // modifier propagation without paying for full expression evaluation here.
    let resolve_fill_value = |value: &ast::Expression| -> ast::Expression {
        let ast::Expression::ComponentReference(cref) = value else {
            return value.clone();
        };
        if cref.parts.len() != 1 {
            return value.clone();
        }

        let name = cref.parts[0].ident.text.as_ref();
        let qn = ast::QualifiedName::from_ident(name);
        if let Some(mod_value) = mod_env.get(&qn) {
            return mod_value.value.clone();
        }

        let Some(comp) = effective_components.get(name) else {
            return value.clone();
        };
        if let Some(binding) = &comp.binding {
            return binding.clone();
        }
        if !matches!(comp.start, ast::Expression::Empty { .. }) {
            return comp.start.clone();
        }
        value.clone()
    };

    match func {
        "fill" if comp.parts.len() == 1 && args.len() == 2 => {
            make_repeated(resolve_fill_value(&args[0]), &args[1])
        }
        "zeros" if comp.parts.len() == 1 && args.len() == 1 => {
            make_repeated(make_int_lit(0), &args[0])
        }
        "ones" if comp.parts.len() == 1 && args.len() == 1 => {
            make_repeated(make_int_lit(1), &args[0])
        }
        "symmetricOrientation"
            if is_polyphase_symmetric_orientation_call(comp) && args.len() == 1 =>
        {
            let eval_ctx = InstantiateEvalCtx {
                tree,
                mod_env,
                effective_components,
                resolve_class_components: &resolve_effective_components_for_eval,
            };
            let m = try_eval_integer_expr(&eval_ctx, &args[0])?;
            let values = symmetric_orientation_values(m)?;
            Some(ast::Expression::Array {
                elements: values
                    .into_iter()
                    .map(|value| make_real_lit(value, call_span))
                    .collect(),
                is_matrix: false,
                span: call_span,
            })
        }
        _ => None,
    }
}

fn is_polyphase_symmetric_orientation_call(comp: &ast::ComponentReference) -> bool {
    let path = comp
        .parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");
    path == "Modelica.Electrical.Polyphase.Functions.symmetricOrientation"
        || path == "Electrical.Polyphase.Functions.symmetricOrientation"
        || path == "Polyphase.Functions.symmetricOrientation"
}

fn symmetric_orientation_values(m: i64) -> Option<Vec<f64>> {
    if !(1..=1024).contains(&m) {
        return None;
    }
    if m % 2 == 0 {
        if m == 2 {
            return Some(vec![0.0, std::f64::consts::FRAC_PI_2]);
        }
        let half = m / 2;
        let base = symmetric_orientation_values(half)?;
        let offset = std::f64::consts::PI / m as f64;
        let mut values = Vec::with_capacity(m as usize);
        values.extend(base.iter().copied());
        values.extend(base.into_iter().map(|value| value - offset));
        return Some(values);
    }
    Some(
        (1..=m)
            .map(|k| (k - 1) as f64 * 2.0 * std::f64::consts::PI / m as f64)
            .collect(),
    )
}

fn apply_unary_to_structural_array(
    op: &rumoca_core::OpUnary,
    array: &ast::Expression,
    span: rumoca_core::Span,
) -> Option<ast::Expression> {
    let ast::Expression::Array {
        elements,
        is_matrix,
        ..
    } = array
    else {
        return None;
    };
    let mapped = elements
        .iter()
        .map(|elem| apply_unary_to_structural_array_element(op, elem, span))
        .collect::<Option<Vec<_>>>()?;
    Some(ast::Expression::Array {
        elements: mapped,
        is_matrix: *is_matrix,
        span,
    })
}

fn apply_unary_to_structural_array_element(
    op: &rumoca_core::OpUnary,
    elem: &ast::Expression,
    span: rumoca_core::Span,
) -> Option<ast::Expression> {
    match op {
        rumoca_core::OpUnary::Plus
        | rumoca_core::OpUnary::DotPlus
        | rumoca_core::OpUnary::Empty => Some(elem.clone()),
        rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => {
            Some(negate_structural_numeric_expr(elem, span))
        }
        rumoca_core::OpUnary::Not => None,
    }
}

fn negate_structural_numeric_expr(
    expr: &ast::Expression,
    span: rumoca_core::Span,
) -> ast::Expression {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedReal,
            token,
            ..
        } => token
            .text
            .parse::<f64>()
            .ok()
            .map(|value| make_real_lit(-value, span))
            .unwrap_or_else(|| ast::Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs: Arc::new(expr.clone()),
                span,
            }),
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
            ..
        } => token
            .text
            .parse::<i64>()
            .ok()
            .map(|value| make_real_lit(-(value as f64), span))
            .unwrap_or_else(|| ast::Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs: Arc::new(expr.clone()),
                span,
            }),
        ast::Expression::Parenthesized { inner, .. } => negate_structural_numeric_expr(inner, span),
        _ => ast::Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs: Arc::new(expr.clone()),
            span,
        },
    }
}

fn make_real_lit(value: f64, span: rumoca_core::Span) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedReal,
        token: rumoca_core::Token {
            text: format!("{value:.17}").into(),
            ..rumoca_core::Token::default()
        },
        span,
    }
}

/// Evaluate an array comprehension `{expr for j in start:end}` to a concrete array.
///
/// MLS §11.1.2.1: For simple comprehensions like `{j for j in 1:m}`,
/// evaluates the range and substitutes the loop variable for each value.
fn try_eval_array_comprehension(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<ast::Expression> {
    let ast::Expression::ArrayComprehension {
        expr: body,
        indices,
        filter,
        span,
    } = expr
    else {
        return None;
    };
    // Only handle single-index comprehensions without filters
    if indices.len() != 1 || filter.is_some() {
        return None;
    }
    let for_idx = &indices[0];
    let loop_var = for_idx.ident.text.as_ref();

    // Evaluate the range bounds
    let (start, end) = eval_range_bounds(&for_idx.range, mod_env, effective_components, tree)?;

    // Generate elements by substituting the loop variable
    let mut elements = Vec::with_capacity((end - start + 1).max(0) as usize);
    for val in start..=end {
        let elem = substitute_var(body, loop_var, val);
        elements.push(elem);
    }
    Some(ast::Expression::Array {
        elements,
        is_matrix: false,
        span: *span,
    })
}

/// Evaluate range bounds from a Range expression, returning (start, end).
fn eval_range_bounds(
    range: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<(i64, i64)> {
    match range {
        ast::Expression::Range { start, end, .. } => {
            let eval_ctx = InstantiateEvalCtx {
                tree,
                mod_env,
                effective_components,
                resolve_class_components: &resolve_effective_components_for_eval,
            };
            let s = try_eval_integer_expr(&eval_ctx, start)?;
            let e = try_eval_integer_expr(&eval_ctx, end)?;
            Some((s, e))
        }
        _ => None,
    }
}

/// Substitute a component reference to `var_name` with an integer literal.
fn substitute_var(expr: &ast::Expression, var_name: &str, value: i64) -> ast::Expression {
    if let Some(replacement) = replace_component_reference_with_integer(expr, var_name, value) {
        return replacement;
    }

    match expr {
        ast::Expression::Array {
            elements,
            is_matrix,
            span,
        } => ast::Expression::Array {
            elements: elements
                .iter()
                .map(|elem| substitute_var(elem, var_name, value))
                .collect(),
            is_matrix: *is_matrix,
            span: *span,
        },
        ast::Expression::Binary { op, lhs, rhs, span } => ast::Expression::Binary {
            op: op.clone(),
            lhs: Arc::new(substitute_var(lhs, var_name, value)),
            rhs: Arc::new(substitute_var(rhs, var_name, value)),
            span: *span,
        },
        ast::Expression::Unary { op, rhs, span } => ast::Expression::Unary {
            op: op.clone(),
            rhs: Arc::new(substitute_var(rhs, var_name, value)),
            span: *span,
        },
        ast::Expression::FunctionCall { comp, args, span } => ast::Expression::FunctionCall {
            comp: comp.clone(),
            args: args
                .iter()
                .map(|arg| substitute_var(arg, var_name, value))
                .collect(),
            span: *span,
        },
        ast::Expression::If {
            branches,
            else_branch,
            span,
        } => ast::Expression::If {
            branches: branches
                .iter()
                .map(|(cond, branch_expr)| {
                    (
                        substitute_var(cond, var_name, value),
                        substitute_var(branch_expr, var_name, value),
                    )
                })
                .collect(),
            else_branch: Arc::new(substitute_var(else_branch, var_name, value)),
            span: *span,
        },
        ast::Expression::FieldAccess { base, field, span } => ast::Expression::FieldAccess {
            base: Arc::new(substitute_var(base, var_name, value)),
            field: field.clone(),
            span: *span,
        },
        ast::Expression::ComponentReference(cref) => {
            ast::Expression::ComponentReference(substitute_component_ref_var(cref, var_name, value))
        }
        ast::Expression::ArrayIndex {
            base,
            subscripts,
            span,
        } => ast::Expression::ArrayIndex {
            base: Arc::new(substitute_var(base, var_name, value)),
            subscripts: subscripts
                .iter()
                .map(|sub| substitute_subscript_var(sub, var_name, value))
                .collect(),
            span: *span,
        },
        ast::Expression::Range {
            start,
            step,
            end,
            span,
        } => ast::Expression::Range {
            start: Arc::new(substitute_var(start, var_name, value)),
            step: step
                .as_ref()
                .map(|inner| Arc::new(substitute_var(inner, var_name, value))),
            end: Arc::new(substitute_var(end, var_name, value)),
            span: *span,
        },
        ast::Expression::ArrayComprehension {
            expr: inner_expr,
            indices,
            filter,
            ..
        } => substitute_array_comprehension_var(expr, inner_expr, indices, filter, var_name, value),
        ast::Expression::Parenthesized { inner, span } => ast::Expression::Parenthesized {
            inner: Arc::new(substitute_var(inner, var_name, value)),
            span: *span,
        },
        _ => expr.clone(),
    }
}

fn substitute_component_ref_var(
    cref: &ast::ComponentReference,
    var_name: &str,
    value: i64,
) -> ast::ComponentReference {
    ast::ComponentReference {
        local: cref.local,
        parts: cref
            .parts
            .iter()
            .map(|part| ast::ComponentRefPart {
                ident: part.ident.clone(),
                subs: part.subs.as_ref().map(|subs| {
                    subs.iter()
                        .map(|sub| substitute_subscript_var(sub, var_name, value))
                        .collect()
                }),
            })
            .collect(),
        def_id: cref.def_id,
        span: cref.span,
    }
}

fn replace_component_reference_with_integer(
    expr: &ast::Expression,
    var_name: &str,
    value: i64,
) -> Option<ast::Expression> {
    let ast::Expression::ComponentReference(cref) = expr else {
        return None;
    };
    if cref.parts.len() != 1 || cref.parts[0].ident.text.as_ref() != var_name {
        return None;
    }

    Some(ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: rumoca_core::Token {
            text: Arc::from(value.to_string().as_str()),
            ..rumoca_core::Token::default()
        },
        span: cref.span,
    })
}

fn substitute_subscript_var(
    sub: &rumoca_ir_ast::Subscript,
    var_name: &str,
    value: i64,
) -> rumoca_ir_ast::Subscript {
    match sub {
        rumoca_ir_ast::Subscript::Expression(sub_expr) => {
            rumoca_ir_ast::Subscript::Expression(substitute_var(sub_expr, var_name, value))
        }
        _ => sub.clone(),
    }
}

fn substitute_array_comprehension_var(
    original_expr: &ast::Expression,
    inner_expr: &ast::Expression,
    indices: &[rumoca_ir_ast::ForIndex],
    filter: &Option<Arc<ast::Expression>>,
    var_name: &str,
    value: i64,
) -> ast::Expression {
    if indices
        .iter()
        .any(|for_index| for_index.ident.text.as_ref() == var_name)
    {
        return original_expr.clone();
    }

    ast::Expression::ArrayComprehension {
        expr: Arc::new(substitute_var(inner_expr, var_name, value)),
        indices: indices
            .iter()
            .map(|for_index| rumoca_ir_ast::ForIndex {
                ident: for_index.ident.clone(),
                range: substitute_var(&for_index.range, var_name, value),
            })
            .collect(),
        filter: filter
            .as_ref()
            .map(|filter_expr| Arc::new(substitute_var(filter_expr, var_name, value))),
        span: original_expr.span(),
    }
}

/// Index an array modification value for a specific element.
///
/// If the expression is an `Array { elements }`, returns the element at the
/// given 1-based index. For non-array values, returns None (no change needed).
fn index_array_modification(expr: &ast::Expression, indices: &[i64]) -> Option<ast::Expression> {
    match expr {
        ast::Expression::Array { elements, .. } => {
            let (&first_idx, remaining) = indices.split_first()?;
            let idx = first_idx.checked_sub(1)? as usize; // Convert 1-based to 0-based
            let selected = elements.get(idx)?;
            if remaining.is_empty() {
                Some(selected.clone())
            } else {
                index_array_modification(selected, remaining)
            }
        }
        ast::Expression::Parenthesized { inner, .. } => index_array_modification(inner, indices),
        _ => None,
    }
}

/// Index a binding expression for an array element.
///
/// When an array component has a binding (e.g., `v1[m] = plug1.pin.v`), each expanded
/// element needs an indexed binding (e.g., `v1[1] = plug1.pin[1].v`).
///
/// Uses type information to walk the ast::ComponentReference parts, looking up each part's
/// type in the class tree to find which part introduces array dimensions. The subscript
/// is placed on that part. For example, `plug1.pin.v` where `pin` is declared as
/// `PositivePin pin[m]` becomes `plug1.pin[k].v`.
///
/// MLS §10.5.1: Field access distributes over arrays, so `(a.b.c)[k] = a.b[k].c`
/// when `b` introduces the array dimension.
pub(super) fn index_binding_for_element(
    tree: &ast::ClassTree,
    parent_components: &IndexMap<String, ast::Component>,
    binding: &ast::Expression,
    indices: &[i64],
) -> InstantiateResult<ast::Expression> {
    let make_subscripts = || -> Vec<ast::Subscript> {
        indices
            .iter()
            .map(|&i| {
                ast::Subscript::Expression(ast::Expression::Terminal {
                    terminal_type: ast::TerminalType::UnsignedInteger,
                    token: rumoca_core::Token {
                        text: i.to_string().into(),
                        ..rumoca_core::Token::default()
                    },
                    span: binding.span(),
                })
            })
            .collect()
    };

    // For ast::ComponentReference bindings, walk the cref parts using type info
    // to find which part introduces array dimensions.
    if let ast::Expression::ComponentReference(cref) = binding
        && !cref.parts.is_empty()
    {
        // MLS §10.5.1: if we cannot prove which part introduces array
        // dimensions, preserve the binding shape as `(cref)[k]` instead of
        // guessing a field position.
        let Some(pos) = find_array_part(tree, parent_components, &cref.parts)? else {
            return Ok(ast::Expression::ArrayIndex {
                base: Arc::new(binding.clone()),
                subscripts: make_subscripts(),
                span: binding.span(),
            });
        };
        let mut new_ref = cref.clone();
        new_ref.parts[pos] = ast::ComponentRefPart {
            ident: new_ref.parts[pos].ident.clone(),
            subs: Some(make_subscripts()),
        };
        return Ok(ast::Expression::ComponentReference(new_ref));
    }

    if let Some(indexed) = index_non_component_reference_binding(binding, indices) {
        return Ok(indexed);
    }

    // Fallback for non-ast::ComponentReference bindings: wrap with ArrayIndex
    Ok(ast::Expression::ArrayIndex {
        base: Arc::new(binding.clone()),
        subscripts: make_subscripts(),
        span: binding.span(),
    })
}

fn index_non_component_reference_binding(
    binding: &ast::Expression,
    indices: &[i64],
) -> Option<ast::Expression> {
    match binding {
        ast::Expression::Array { .. } => index_array_modification(binding, indices),
        ast::Expression::ArrayComprehension { .. } => {
            index_array_comprehension_for_element(binding, indices)
        }
        ast::Expression::Parenthesized { inner, .. } => {
            index_non_component_reference_binding(inner, indices)
        }
        _ => None,
    }
}

fn index_array_expression_for_element(
    tree: &ast::ClassTree,
    parent_components: &IndexMap<String, ast::Component>,
    expr: &ast::Expression,
    indices: &[i64],
) -> InstantiateResult<Option<ast::Expression>> {
    match expr {
        ast::Expression::Array { .. } | ast::Expression::ArrayComprehension { .. } => {
            Ok(index_non_component_reference_binding(expr, indices))
        }
        ast::Expression::ComponentReference(cref) => {
            index_component_reference_array_part(tree, parent_components, cref, expr, indices)
        }
        ast::Expression::Binary { op, lhs, rhs, span } => {
            let indexed_lhs =
                index_array_expression_for_element(tree, parent_components, lhs, indices)?;
            let indexed_rhs =
                index_array_expression_for_element(tree, parent_components, rhs, indices)?;
            if indexed_lhs.is_none() && indexed_rhs.is_none() {
                return Ok(None);
            }
            Ok(Some(ast::Expression::Binary {
                op: op.clone(),
                lhs: Arc::new(indexed_lhs.unwrap_or_else(|| lhs.as_ref().clone())),
                rhs: Arc::new(indexed_rhs.unwrap_or_else(|| rhs.as_ref().clone())),
                span: *span,
            }))
        }
        ast::Expression::Unary { op, rhs, span } => {
            Ok(
                index_array_expression_for_element(tree, parent_components, rhs, indices)?.map(
                    |indexed_rhs| ast::Expression::Unary {
                        op: op.clone(),
                        rhs: Arc::new(indexed_rhs),
                        span: *span,
                    },
                ),
            )
        }
        ast::Expression::Parenthesized { inner, span } => {
            Ok(
                index_array_expression_for_element(tree, parent_components, inner, indices)?.map(
                    |indexed_inner| ast::Expression::Parenthesized {
                        inner: Arc::new(indexed_inner),
                        span: *span,
                    },
                ),
            )
        }
        _ => Ok(None),
    }
}

fn index_component_reference_array_part(
    tree: &ast::ClassTree,
    parent_components: &IndexMap<String, ast::Component>,
    cref: &ast::ComponentReference,
    expr: &ast::Expression,
    indices: &[i64],
) -> InstantiateResult<Option<ast::Expression>> {
    if cref.parts.is_empty() {
        return Ok(None);
    }
    let Some(pos) = find_array_part(tree, parent_components, &cref.parts)? else {
        return Ok(None);
    };
    let subscripts = indices
        .iter()
        .map(|&i| {
            ast::Subscript::Expression(ast::Expression::Terminal {
                terminal_type: ast::TerminalType::UnsignedInteger,
                token: rumoca_core::Token {
                    text: i.to_string().into(),
                    ..rumoca_core::Token::default()
                },
                span: expr.span(),
            })
        })
        .collect();
    let mut indexed = cref.clone();
    indexed.parts[pos] = ast::ComponentRefPart {
        ident: indexed.parts[pos].ident.clone(),
        subs: Some(subscripts),
    };
    Ok(Some(ast::Expression::ComponentReference(indexed)))
}

fn index_array_comprehension_for_element(
    expr: &ast::Expression,
    indices: &[i64],
) -> Option<ast::Expression> {
    let ast::Expression::ArrayComprehension {
        expr: body,
        indices: for_indices,
        filter,
        ..
    } = expr
    else {
        return None;
    };

    if filter.is_some() || indices.len() < for_indices.len() {
        return None;
    }

    let mut projected = body.as_ref().clone();
    for (for_index, value) in for_indices.iter().zip(indices.iter().copied()) {
        projected = substitute_var(&projected, for_index.ident.text.as_ref(), value);
    }

    if indices.len() == for_indices.len() {
        return Some(projected);
    }

    let remaining = &indices[for_indices.len()..];
    index_non_component_reference_binding(&projected, remaining).or(Some(projected))
}

/// Walk ast::ComponentReference parts using type information to find which part
/// introduces array dimensions. Returns the index of the array part, if found.
fn find_array_part(
    tree: &ast::ClassTree,
    parent_components: &IndexMap<String, ast::Component>,
    parts: &[rumoca_ir_ast::ComponentRefPart],
) -> InstantiateResult<Option<usize>> {
    // Use Cow-like pattern: borrow parent_components for the first lookup,
    // only allocate owned components when we need to traverse deeper.
    let mut owned: Option<IndexMap<String, ast::Component>>;
    let mut current: &IndexMap<String, ast::Component> = parent_components;

    for (i, part) in parts.iter().enumerate() {
        let Some(comp) = current.get(part.ident.text.as_ref()) else {
            return Ok(None);
        };
        if !comp.shape.is_empty() || !comp.shape_expr.is_empty() {
            return Ok(Some(i));
        }
        if i + 1 < parts.len() {
            let Some(class) = lookup_class_def(tree, comp) else {
                return Ok(None);
            };
            let effective = get_effective_components(tree, class)?;
            owned = Some(if effective.is_empty() {
                class.components.clone()
            } else {
                effective
            });
            current = owned
                .as_ref()
                .expect("owned effective components must be present after insertion");
        }
    }
    Ok(None)
}

/// Look up the ast::ClassDef for a ast::Component's type using the class tree.
fn lookup_class_def<'a>(
    tree: &'a ast::ClassTree,
    comp: &ast::Component,
) -> Option<&'a rumoca_ir_ast::ClassDef> {
    comp.type_def_id
        .or(comp.type_name.def_id)
        .and_then(|def_id| tree.get_class_by_def_id(def_id))
        .or_else(|| find_class_in_tree(tree, &comp.type_name.to_string()))
}

#[cfg(test)]
mod tests {
    use super::{
        ArrayExpansionScope, array_element_binding_modification,
        distribute_component_ref_mods_for_element, distribute_mods_for_element,
        index_array_expression_for_element, index_binding_for_element,
        pre_resolve_array_modifications, resolve_mod_to_array,
    };
    use crate::type_overrides::TypeOverrideMap;
    use rumoca_core::DefId;
    use rumoca_ir_ast as ast;
    use rumoca_ir_ast::AstIndexMap as IndexMap;
    use std::sync::Arc;

    fn make_token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: Arc::from(text),
            location: rumoca_core::Location::default(),
            token_number: 0,
            token_type: 0,
        }
    }

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("array_expansion_test.mo"),
            1,
            2,
        )
    }

    fn make_int_expr(value: i64) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: make_token(&value.to_string()),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn make_comp_ref_expr(names: &[&str]) -> ast::Expression {
        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts: names
                .iter()
                .map(|name| ast::ComponentRefPart {
                    ident: make_token(name),
                    subs: None,
                })
                .collect(),
            def_id: None,
            span: rumoca_core::Span::DUMMY,
        })
    }

    fn make_indexed_comp_ref_expr(name: &str, index_name: &str) -> ast::Expression {
        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: make_token(name),
                subs: Some(vec![ast::Subscript::Expression(make_comp_ref_expr(&[
                    index_name,
                ]))]),
            }],
            def_id: None,
            span: rumoca_core::Span::DUMMY,
        })
    }

    fn make_function_call(name: &str, args: Vec<ast::Expression>) -> ast::Expression {
        ast::Expression::FunctionCall {
            comp: ast::ComponentReference {
                local: false,
                parts: vec![ast::ComponentRefPart {
                    ident: make_token(name),
                    subs: None,
                }],
                def_id: None,
                span: rumoca_core::Span::DUMMY,
            },
            args,
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn make_qualified_function_call(names: &[&str], args: Vec<ast::Expression>) -> ast::Expression {
        ast::Expression::FunctionCall {
            comp: ast::ComponentReference {
                local: false,
                parts: names
                    .iter()
                    .map(|name| ast::ComponentRefPart {
                        ident: make_token(name),
                        subs: None,
                    })
                    .collect(),
                def_id: None,
                span: rumoca_core::Span::DUMMY,
            },
            args,
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn test_array_element_binding_preserves_modifier_source_scope() {
        let tree = ast::ClassTree::new();
        let effective_components = IndexMap::default();
        let type_overrides = TypeOverrideMap::new();
        let imports = Vec::new();
        let scope = ArrayExpansionScope {
            tree: &tree,
            effective_components: &effective_components,
            type_overrides: &type_overrides,
            imports: crate::ComponentImports {
                qualification: &imports,
                attributes: &[],
            },
        };
        let source = make_comp_ref_expr(&["outer", "x"]);
        let parent_mod = ast::ModificationValue::with_source_scope(
            source.clone(),
            Some(source),
            Some(ast::QualifiedName::from_dotted("outerScope")),
        );

        let value = array_element_binding_modification(
            &scope,
            &make_comp_ref_expr(&["resolved", "x"]),
            &[2],
            Some(&parent_mod),
            Some(ast::QualifiedName::from_dotted("declarationScope")),
        )
        .expect("array element binding modification should succeed");

        assert_eq!(
            value.source_scope.map(|scope| scope.to_flat_string()),
            Some("outerScope".to_string())
        );
        let ast::Expression::ArrayIndex { base, .. } =
            value.source.expect("indexed source should be preserved")
        else {
            panic!("expected indexed source");
        };
        let ast::Expression::ComponentReference(source_ref) = base.as_ref() else {
            panic!("expected component reference base");
        };
        assert_eq!(source_ref.parts[0].ident.text.as_ref(), "outer");
    }

    #[test]
    fn test_array_element_binding_preserves_declaration_source_scope() {
        let tree = ast::ClassTree::new();
        let effective_components = IndexMap::default();
        let type_overrides = TypeOverrideMap::new();
        let imports = Vec::new();
        let scope = ArrayExpansionScope {
            tree: &tree,
            effective_components: &effective_components,
            type_overrides: &type_overrides,
            imports: crate::ComponentImports {
                qualification: &imports,
                attributes: &[],
            },
        };
        let binding = make_comp_ref_expr(&["plug", "pin"]);

        let value = array_element_binding_modification(
            &scope,
            &binding,
            &[1],
            None,
            Some(ast::QualifiedName::from_dotted("Model.Source")),
        )
        .expect("array element binding modification should succeed");

        assert_eq!(
            value.source_scope.map(|scope| scope.to_flat_string()),
            Some("Model.Source".to_string())
        );
        assert_eq!(value.source, Some(binding));
    }

    fn real_lit_value(expr: &ast::Expression) -> f64 {
        let ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedReal,
            token,
            ..
        } = expr
        else {
            panic!("expected real literal");
        };
        token.text.parse().expect("real literal should parse")
    }

    fn make_range_expr(start: i64, end: i64) -> ast::Expression {
        ast::Expression::Range {
            start: Arc::new(make_int_expr(start)),
            step: None,
            end: Arc::new(make_int_expr(end)),
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn test_resolve_mod_to_array_symmetric_orientation_with_unary_minus() {
        let call = make_qualified_function_call(
            &["Polyphase", "Functions", "symmetricOrientation"],
            vec![make_int_expr(3)],
        );
        let expr = ast::Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs: Arc::new(call),
            span: rumoca_core::Span::DUMMY,
        };

        let resolved = resolve_mod_to_array(
            &expr,
            &rumoca_ir_ast::ModificationEnvironment::default(),
            &IndexMap::default(),
            &ast::ClassTree::default(),
        );

        let ast::Expression::Array { elements, .. } = resolved else {
            panic!("symmetricOrientation() should resolve to an array");
        };
        assert_eq!(elements.len(), 3);
        let values = elements.iter().map(real_lit_value).collect::<Vec<_>>();
        assert!(values[0].abs() <= 1e-14);
        assert!((values[1] + 2.0 * std::f64::consts::PI / 3.0).abs() <= 1e-14);
        assert!((values[2] + 4.0 * std::f64::consts::PI / 3.0).abs() <= 1e-14);
    }

    #[test]
    fn test_resolve_mod_to_array_fill_constructor() {
        let expr = make_function_call("fill", vec![make_int_expr(7), make_int_expr(3)]);
        let resolved = resolve_mod_to_array(
            &expr,
            &rumoca_ir_ast::ModificationEnvironment::default(),
            &IndexMap::default(),
            &ast::ClassTree::default(),
        );

        let ast::Expression::Array { elements, .. } = resolved else {
            panic!("fill() should resolve to an array for modifier distribution");
        };
        assert_eq!(elements.len(), 3);
        for e in elements {
            match e {
                ast::Expression::Terminal { token, .. } => assert_eq!(token.text.as_ref(), "7"),
                _ => panic!("fill() element should be a scalar expression"),
            }
        }
    }

    #[test]
    fn test_index_binding_for_element_indexes_proven_array_part() {
        let mut parent_components = IndexMap::default();
        let array_comp = ast::Component {
            name: "arr".to_string(),
            shape: vec![3],
            ..ast::Component::empty_with_span(test_span())
        };
        parent_components.insert("arr".to_string(), array_comp);

        let binding = make_comp_ref_expr(&["arr", "v"]);
        let indexed = index_binding_for_element(
            &ast::ClassTree::default(),
            &parent_components,
            &binding,
            &[2],
        )
        .expect("array element binding should index proven array part");

        let ast::Expression::ComponentReference(cref) = indexed else {
            panic!("expected indexed component reference");
        };
        assert_eq!(cref.parts.len(), 2);
        assert_eq!(cref.parts[0].ident.text.as_ref(), "arr");
        let Some(subs) = &cref.parts[0].subs else {
            panic!("array part should be subscripted");
        };
        assert_eq!(subs.len(), 1);
        let ast::Subscript::Expression(ast::Expression::Terminal { token, .. }) = &subs[0] else {
            panic!("expected integer subscript expression");
        };
        assert_eq!(token.text.as_ref(), "2");
        assert!(
            cref.parts[1].subs.is_none(),
            "field part must remain unindexed"
        );
    }

    #[test]
    fn test_index_binding_for_element_no_array_part_uses_array_index_fallback() {
        let binding = make_comp_ref_expr(&["a", "b", "c"]);
        let indexed = index_binding_for_element(
            &ast::ClassTree::default(),
            &IndexMap::default(),
            &binding,
            &[1],
        )
        .expect("array element binding should preserve unproven reference as ArrayIndex");

        let ast::Expression::ArrayIndex {
            base, subscripts, ..
        } = indexed
        else {
            panic!("unproven array part should use ArrayIndex fallback");
        };
        assert_eq!(subscripts.len(), 1);
        let ast::Subscript::Expression(ast::Expression::Terminal { token, .. }) = &subscripts[0]
        else {
            panic!("expected integer subscript expression");
        };
        assert_eq!(token.text.as_ref(), "1");
        assert_eq!(*base, binding);
    }

    #[test]
    fn test_index_binding_for_element_projects_multidim_array_comprehension() {
        let binding = ast::Expression::ArrayComprehension {
            expr: Arc::new(make_comp_ref_expr(&["ks"])),
            indices: vec![
                ast::ForIndex {
                    ident: make_token("ks"),
                    range: make_range_expr(1, 3),
                },
                ast::ForIndex {
                    ident: make_token("kp"),
                    range: make_range_expr(1, 2),
                },
            ],
            filter: None,
            span: rumoca_core::Span::DUMMY,
        };

        let indexed = index_binding_for_element(
            &ast::ClassTree::default(),
            &IndexMap::default(),
            &binding,
            &[2, 1],
        )
        .expect("array comprehension projection should succeed");
        let ast::Expression::Terminal { token, .. } = indexed else {
            panic!("multi-index comprehension should project to a concrete element expression");
        };
        assert_eq!(token.text.as_ref(), "2");
    }

    #[test]
    fn test_index_binding_for_element_substitutes_comprehension_index_in_subscripts() {
        let binding = ast::Expression::ArrayComprehension {
            expr: Arc::new(ast::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Arc::new(make_comp_ref_expr(&["level"])),
                rhs: Arc::new(make_indexed_comp_ref_expr("top_heights", "i")),
                span: rumoca_core::Span::DUMMY,
            }),
            indices: vec![ast::ForIndex {
                ident: make_token("i"),
                range: make_range_expr(1, 3),
            }],
            filter: None,
            span: rumoca_core::Span::DUMMY,
        };

        let indexed = index_binding_for_element(
            &ast::ClassTree::default(),
            &IndexMap::default(),
            &binding,
            &[2],
        )
        .expect("array comprehension projection should succeed");

        let ast::Expression::Binary { rhs, .. } = indexed else {
            panic!("array comprehension should project to expression body");
        };
        let ast::Expression::ComponentReference(cref) = rhs.as_ref() else {
            panic!("expected indexed component reference");
        };
        let Some(subscripts) = cref.parts[0].subs.as_ref() else {
            panic!("expected subscript");
        };
        let ast::Subscript::Expression(ast::Expression::Terminal { token, .. }) = &subscripts[0]
        else {
            panic!("expected literal subscript");
        };
        assert_eq!(token.text.as_ref(), "2");
    }

    #[test]
    fn test_index_array_start_projects_vectorized_binary_comprehension() {
        let start = ast::Expression::Binary {
            op: rumoca_core::OpBinary::Div,
            lhs: Arc::new(ast::Expression::ArrayComprehension {
                expr: Arc::new(make_indexed_comp_ref_expr("m_flows", "i")),
                indices: vec![ast::ForIndex {
                    ident: make_token("i"),
                    range: make_range_expr(1, 3),
                }],
                filter: None,
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Arc::new(make_comp_ref_expr(&["nParallel"])),
            span: rumoca_core::Span::DUMMY,
        };

        let indexed = index_array_expression_for_element(
            &ast::ClassTree::default(),
            &IndexMap::default(),
            &start,
            &[2],
        )
        .expect("array-valued start projection should not fail")
        .expect("array-valued start should project to one scalar element");

        let ast::Expression::Binary { lhs, rhs, .. } = indexed else {
            panic!("expected vectorized binary expression to stay binary");
        };
        let ast::Expression::ComponentReference(cref) = lhs.as_ref() else {
            panic!("expected projected lhs component reference");
        };
        let Some(subscripts) = cref.parts[0].subs.as_ref() else {
            panic!("expected projected subscript");
        };
        let ast::Subscript::Expression(ast::Expression::Terminal { token, .. }) = &subscripts[0]
        else {
            panic!("expected literal projected subscript");
        };
        assert_eq!(token.text.as_ref(), "2");
        let ast::Expression::ComponentReference(rhs_ref) = rhs.as_ref() else {
            panic!("scalar rhs should remain unchanged");
        };
        assert_eq!(rhs_ref.parts[0].ident.text.as_ref(), "nParallel");
    }

    #[test]
    fn test_index_binding_for_element_projects_nested_array_comprehensions() {
        let inner = ast::Expression::ArrayComprehension {
            expr: Arc::new(make_comp_ref_expr(&["ks"])),
            indices: vec![ast::ForIndex {
                ident: make_token("kp"),
                range: make_range_expr(1, 2),
            }],
            filter: None,
            span: rumoca_core::Span::DUMMY,
        };
        let binding = ast::Expression::ArrayComprehension {
            expr: Arc::new(inner),
            indices: vec![ast::ForIndex {
                ident: make_token("ks"),
                range: make_range_expr(1, 3),
            }],
            filter: None,
            span: rumoca_core::Span::DUMMY,
        };

        let indexed = index_binding_for_element(
            &ast::ClassTree::default(),
            &IndexMap::default(),
            &binding,
            &[2, 1],
        )
        .expect("nested array comprehension projection should succeed");
        let ast::Expression::Terminal { token, .. } = indexed else {
            panic!("nested comprehensions should project to a concrete element expression");
        };
        assert_eq!(token.text.as_ref(), "2");
    }

    #[test]
    fn test_index_binding_for_element_indexes_nested_array_part_via_type_walk() {
        let stack_data_id = DefId::new(100);
        let mut tree = ast::ClassTree::default();

        let mut stack_data = ast::ClassDef {
            name: make_token("StackData"),
            def_id: Some(stack_data_id),
            ..Default::default()
        };
        stack_data.components.insert(
            "cellData".to_string(),
            ast::Component {
                name: "cellData".to_string(),
                shape: vec![3, 2],
                ..ast::Component::empty_with_span(test_span())
            },
        );
        tree.definitions
            .classes
            .insert("StackData".to_string(), stack_data);
        tree.def_map.insert(stack_data_id, "StackData".to_string());
        tree.name_map.insert("StackData".to_string(), stack_data_id);

        let mut parent_components = IndexMap::default();
        parent_components.insert(
            "stackData".to_string(),
            ast::Component {
                name: "stackData".to_string(),
                type_name: ast::Name {
                    name: vec![make_token("StackData")],
                    def_id: Some(stack_data_id),
                },
                type_def_id: Some(stack_data_id),
                ..ast::Component::empty_with_span(test_span())
            },
        );

        let binding = make_comp_ref_expr(&["stackData", "cellData"]);
        let indexed = index_binding_for_element(&tree, &parent_components, &binding, &[2, 1])
            .expect("nested array part indexing should succeed");

        let ast::Expression::ComponentReference(cref) = indexed else {
            panic!("expected indexed nested component reference");
        };
        assert_eq!(cref.parts.len(), 2);
        assert!(
            cref.parts[0].subs.is_none(),
            "root record part must remain unindexed"
        );
        let Some(subs) = &cref.parts[1].subs else {
            panic!("nested array field should be indexed");
        };
        assert_eq!(subs.len(), 2);
    }

    #[test]
    fn test_distribute_mods_for_element_fill_modifier() {
        let mut comp = ast::Component::empty_with_span(test_span());
        comp.modifications.insert(
            "k".to_string(),
            make_function_call("fill", vec![make_int_expr(5), make_int_expr(2)]),
        );

        let resolved_mods = pre_resolve_array_modifications(
            &comp,
            &rumoca_ir_ast::ModificationEnvironment::default(),
            &IndexMap::default(),
            &ast::ClassTree::default(),
        );
        assert_eq!(
            resolved_mods.len(),
            1,
            "fill() modifier should be resolved for non-`each` distribution"
        );

        let mut scalar_comp = comp.clone();
        distribute_mods_for_element(&mut scalar_comp, &resolved_mods, &[1]);
        let first = scalar_comp.modifications.get("k").expect("missing k mod");
        match first {
            ast::Expression::Terminal { token, .. } => assert_eq!(token.text.as_ref(), "5"),
            _ => panic!("distributed modifier should be scalar"),
        }

        distribute_mods_for_element(&mut scalar_comp, &resolved_mods, &[2]);
        let second = scalar_comp.modifications.get("k").expect("missing k mod");
        match second {
            ast::Expression::Terminal { token, .. } => assert_eq!(token.text.as_ref(), "5"),
            _ => panic!("distributed modifier should be scalar"),
        }
    }

    #[test]
    fn test_distribute_component_ref_mods_for_element_indexes_proven_array_reference() {
        let mut comp = ast::Component::empty_with_span(test_span());
        comp.modifications
            .insert("cellData".to_string(), make_comp_ref_expr(&["arr", "v"]));

        let mut parent_components = IndexMap::default();
        parent_components.insert(
            "arr".to_string(),
            ast::Component {
                name: "arr".to_string(),
                shape: vec![3],
                ..ast::Component::empty_with_span(test_span())
            },
        );

        let mut scalar_comp = comp.clone();
        let resolved_mod_names = std::collections::HashSet::new();
        distribute_component_ref_mods_for_element(
            &mut scalar_comp,
            &comp,
            &resolved_mod_names,
            &ast::ClassTree::default(),
            &parent_components,
            &[2],
        )
        .expect("component-reference modifier distribution should succeed");

        let ast::Expression::ComponentReference(cref) = scalar_comp
            .modifications
            .get("cellData")
            .expect("missing distributed component reference")
        else {
            panic!("component-reference modifier should be indexed");
        };
        assert_eq!(cref.parts.len(), 2);
        let Some(subs) = &cref.parts[0].subs else {
            panic!("array-introducing part should be indexed");
        };
        let ast::Subscript::Expression(ast::Expression::Terminal { token, .. }) = &subs[0] else {
            panic!("expected integer index");
        };
        assert_eq!(token.text.as_ref(), "2");
    }
}
