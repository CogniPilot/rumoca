use super::{InstantiateContext, InstantiateResult, find_class_in_tree, get_effective_components};
use super::{instantiate_component, try_eval_integer_expr};
use indexmap::IndexMap;
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use std::sync::Arc;

/// Expand an array component into individual indexed instances.
///
/// MLS §10.1: Array component expansion creates indexed instances.
/// Also registers the parent array path with its dimensions for use in
/// array equation expansion (MLS §10.5).
pub(super) struct ArrayExpansionScope<'a> {
    pub(super) tree: &'a ast::ClassTree,
    pub(super) effective_components: &'a IndexMap<String, ast::Component>,
    pub(super) type_overrides: &'a IndexMap<String, DefId>,
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

    // Extract the original binding for indexing. Check active component modifier first,
    // then comp.binding, then fall back to comp.start for modification-only declarations.
    let binding_qn = ast::QualifiedName::from_ident(name);
    let mod_env_binding = ctx.mod_env().get(&binding_qn).map(|mv| mv.value.clone());

    // Check comp.binding first,
    // then fall back to comp.start with has_explicit_binding (modification-only declarations).
    // We only READ the binding here - do not clear comp.start or has_explicit_binding,
    // since they may be needed for dimension inference in typecheck.
    let original_binding = mod_env_binding.or_else(|| {
        comp.binding.clone().or_else(|| {
            if comp.has_explicit_binding
                && !comp.start_is_modification
                && !matches!(comp.start, ast::Expression::Empty)
            {
                Some(comp.start.clone())
            } else {
                None
            }
        })
    });

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
        let indexed_name = format_indexed_name(name, &idx);

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
            ));
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
        );

        // Ensure scalar element instantiation sees the indexed binding (MLS §10.1).
        // Without this scoped override, a parent unindexed modifier entry for this
        // component name would overwrite the per-element indexed binding.
        let previous_binding = ctx.mod_env().active.get(&binding_qn).cloned();
        if let Some(binding_expr) = &scalar_comp.binding {
            ctx.mod_env_mut().active.insert(
                binding_qn.clone(),
                ast::ModificationValue::simple(binding_expr.clone()),
            );
        }

        ctx.push_path(&indexed_name);
        let inst_result = instantiate_component(
            scope.tree,
            &scalar_comp,
            ctx,
            overlay,
            scope.effective_components,
            scope.type_overrides,
        );
        ctx.pop_path();

        // Restore parent binding scope for subsequent elements/siblings.
        match previous_binding {
            Some(prev) => {
                ctx.mod_env_mut().active.insert(binding_qn.clone(), prev);
            }
            None => {
                ctx.mod_env_mut().active.shift_remove(&binding_qn);
            }
        }

        inst_result?;
    }

    Ok(())
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
        if has_explicit_subscripted_component_ref(expr) {
            continue; // preserve element-wise symbolic source, e.g. root[11:15] -> root[11]
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
) {
    for (name, expr) in &original_comp.modifications {
        if original_comp.each_modifications.contains(name)
            || resolved_mod_names.contains(name.as_str())
        {
            continue;
        }

        let indexed = index_binding_for_element(tree, parent_components, expr, indices);
        if !matches!(indexed, ast::Expression::ArrayIndex { .. }) {
            scalar_comp.modifications.insert(name.clone(), indexed);
        }
    }
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
    // Evaluate common array constructors used in non-`each` modifiers
    // (e.g., k=fill(1, n) for component arrays). This allows per-element
    // modifier distribution during array component expansion (MLS §7.2.5).
    if let Some(array) = try_eval_array_constructor(expr, mod_env, effective_components, tree) {
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

/// Evaluate simple array constructors to concrete 1-D arrays.
///
/// Supports:
/// - `fill(v, n)` -> `{v, v, ..., v}` (n elements)
/// - `zeros(n)` -> `{0, ..., 0}` (n elements)
/// - `ones(n)` -> `{1, ..., 1}` (n elements)
///
/// This is intentionally limited to 1-D constructors because modifier
/// distribution currently indexes one dimension at a time.
fn try_eval_array_constructor(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<ast::Expression> {
    let ast::Expression::FunctionCall { comp, args } = expr else {
        return None;
    };
    if comp.parts.len() != 1 {
        return None;
    }

    let func = comp.parts[0].ident.text.as_ref();
    let make_int_lit = |value: i64| ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: rumoca_ir_core::Token {
            text: value.to_string().into(),
            ..rumoca_ir_core::Token::default()
        },
    };

    let make_repeated =
        |value: ast::Expression, count_expr: &ast::Expression| -> Option<ast::Expression> {
            let n = try_eval_integer_expr(count_expr, mod_env, effective_components, tree)?;
            let len = n.max(0) as usize;
            Some(ast::Expression::Array {
                elements: std::iter::repeat_n(value, len).collect(),
                is_matrix: false,
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
        if !matches!(comp.start, ast::Expression::Empty) {
            return comp.start.clone();
        }
        value.clone()
    };

    match func {
        "fill" if args.len() == 2 => make_repeated(resolve_fill_value(&args[0]), &args[1]),
        "zeros" if args.len() == 1 => make_repeated(make_int_lit(0), &args[0]),
        "ones" if args.len() == 1 => make_repeated(make_int_lit(1), &args[0]),
        _ => None,
    }
}

fn has_explicit_subscripted_component_ref(expr: &ast::Expression) -> bool {
    match expr {
        ast::Expression::ComponentReference(cref) => {
            cref.parts.iter().any(|part| part.subs.is_some())
        }
        ast::Expression::Parenthesized { inner } => has_explicit_subscripted_component_ref(inner),
        _ => false,
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
            let s = try_eval_integer_expr(start, mod_env, effective_components, tree)?;
            let e = try_eval_integer_expr(end, mod_env, effective_components, tree)?;
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
        } => ast::Expression::Array {
            elements: elements
                .iter()
                .map(|elem| substitute_var(elem, var_name, value))
                .collect(),
            is_matrix: *is_matrix,
        },
        ast::Expression::Binary { op, lhs, rhs } => ast::Expression::Binary {
            op: op.clone(),
            lhs: Arc::new(substitute_var(lhs, var_name, value)),
            rhs: Arc::new(substitute_var(rhs, var_name, value)),
        },
        ast::Expression::Unary { op, rhs } => ast::Expression::Unary {
            op: op.clone(),
            rhs: Arc::new(substitute_var(rhs, var_name, value)),
        },
        ast::Expression::FunctionCall { comp, args } => ast::Expression::FunctionCall {
            comp: comp.clone(),
            args: args
                .iter()
                .map(|arg| substitute_var(arg, var_name, value))
                .collect(),
        },
        ast::Expression::If {
            branches,
            else_branch,
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
        },
        ast::Expression::FieldAccess { base, field } => ast::Expression::FieldAccess {
            base: Arc::new(substitute_var(base, var_name, value)),
            field: field.clone(),
        },
        ast::Expression::ArrayIndex { base, subscripts } => ast::Expression::ArrayIndex {
            base: Arc::new(substitute_var(base, var_name, value)),
            subscripts: subscripts
                .iter()
                .map(|sub| substitute_subscript_var(sub, var_name, value))
                .collect(),
        },
        ast::Expression::Range { start, step, end } => ast::Expression::Range {
            start: Arc::new(substitute_var(start, var_name, value)),
            step: step
                .as_ref()
                .map(|inner| Arc::new(substitute_var(inner, var_name, value))),
            end: Arc::new(substitute_var(end, var_name, value)),
        },
        ast::Expression::ArrayComprehension {
            expr: inner_expr,
            indices,
            filter,
        } => substitute_array_comprehension_var(expr, inner_expr, indices, filter, var_name, value),
        ast::Expression::Parenthesized { inner } => ast::Expression::Parenthesized {
            inner: Arc::new(substitute_var(inner, var_name, value)),
        },
        _ => expr.clone(),
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
        token: rumoca_ir_core::Token {
            text: Arc::from(value.to_string().as_str()),
            ..rumoca_ir_core::Token::default()
        },
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
    }
}

/// Index an array modification value for a specific element.
///
/// If the expression is an `Array { elements }`, returns the element at the
/// given 1-based index. For non-array values, returns None (no change needed).
fn index_array_modification(expr: &ast::Expression, indices: &[i64]) -> Option<ast::Expression> {
    match expr {
        ast::Expression::Array {
            elements,
            is_matrix,
        } => {
            let (&first_idx, remaining) = indices.split_first()?;
            let idx = first_idx.checked_sub(1)? as usize; // Convert 1-based to 0-based
            let selected = elements.get(idx)?;
            if remaining.is_empty() {
                Some(normalize_distributed_matrix_row(selected, *is_matrix))
            } else {
                index_array_modification(selected, remaining)
            }
        }
        ast::Expression::Parenthesized { inner } => index_array_modification(inner, indices),
        _ => None,
    }
}

fn normalize_distributed_matrix_row(
    selected: &ast::Expression,
    parent_is_matrix: bool,
) -> ast::Expression {
    if !parent_is_matrix {
        return selected.clone();
    }

    // MLS §7.2.5 + §10.4: selecting one element of a matrix-valued
    // non-`each` modifier distributes the row value to the scalar component.
    // The selected row is a 1-D array value, not a single-row matrix.
    match selected {
        ast::Expression::Array { elements, .. } => ast::Expression::Array {
            elements: elements.clone(),
            is_matrix: false,
        },
        _ => selected.clone(),
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
) -> ast::Expression {
    let make_subscripts = || -> Vec<ast::Subscript> {
        indices
            .iter()
            .map(|&i| {
                ast::Subscript::Expression(ast::Expression::Terminal {
                    terminal_type: ast::TerminalType::UnsignedInteger,
                    token: rumoca_ir_core::Token {
                        text: i.to_string().into(),
                        ..rumoca_ir_core::Token::default()
                    },
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
        let Some(pos) = find_array_part(tree, parent_components, &cref.parts) else {
            return ast::Expression::ArrayIndex {
                base: Arc::new(binding.clone()),
                subscripts: make_subscripts(),
            };
        };
        let mut new_ref = cref.clone();
        let subs = new_ref.parts[pos]
            .subs
            .as_ref()
            .and_then(|existing| project_existing_subscripts_for_element(existing, indices))
            .unwrap_or_else(make_subscripts);
        new_ref.parts[pos] = ast::ComponentRefPart {
            ident: new_ref.parts[pos].ident.clone(),
            subs: Some(subs),
        };
        return ast::Expression::ComponentReference(new_ref);
    }

    if let Some(indexed) = index_non_component_reference_binding(binding, indices) {
        return indexed;
    }

    // Fallback for non-ast::ComponentReference bindings: wrap with ArrayIndex
    ast::Expression::ArrayIndex {
        base: Arc::new(binding.clone()),
        subscripts: make_subscripts(),
    }
}

fn project_existing_subscripts_for_element(
    existing: &[ast::Subscript],
    indices: &[i64],
) -> Option<Vec<ast::Subscript>> {
    if existing.len() != indices.len() {
        return None;
    }

    existing
        .iter()
        .zip(indices.iter().copied())
        .map(|(sub, index)| project_existing_subscript_for_element(sub, index))
        .collect()
}

fn project_existing_subscript_for_element(
    sub: &ast::Subscript,
    index: i64,
) -> Option<ast::Subscript> {
    match sub {
        ast::Subscript::Expression(ast::Expression::Range { start, step, .. }) => {
            let start = integer_literal_value(start)?;
            let step = match step.as_deref() {
                Some(expr) => integer_literal_value(expr)?,
                None => 1,
            };
            let selected = start + (index - 1) * step;
            Some(ast::Subscript::Expression(make_int_expr(selected)))
        }
        ast::Subscript::Expression(ast::Expression::Array { elements, .. }) => {
            let idx = index.checked_sub(1)? as usize;
            Some(ast::Subscript::Expression(elements.get(idx)?.clone()))
        }
        ast::Subscript::Expression(expr) => Some(ast::Subscript::Expression(expr.clone())),
        ast::Subscript::Empty | ast::Subscript::Range { .. } => None,
    }
}

fn integer_literal_value(expr: &ast::Expression) -> Option<i64> {
    let ast::Expression::Terminal { token, .. } = expr else {
        return None;
    };
    token.text.as_ref().parse().ok()
}

fn make_int_expr(value: i64) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: rumoca_ir_core::Token {
            text: value.to_string().into(),
            ..rumoca_ir_core::Token::default()
        },
    }
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
        ast::Expression::Parenthesized { inner } => {
            index_non_component_reference_binding(inner, indices)
        }
        _ => None,
    }
}

fn index_array_comprehension_for_element(
    expr: &ast::Expression,
    indices: &[i64],
) -> Option<ast::Expression> {
    let ast::Expression::ArrayComprehension {
        expr: body,
        indices: for_indices,
        filter,
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
) -> Option<usize> {
    // Use Cow-like pattern: borrow parent_components for the first lookup,
    // only allocate owned components when we need to traverse deeper.
    let mut owned: Option<IndexMap<String, ast::Component>>;
    let mut current: &IndexMap<String, ast::Component> = parent_components;

    for (i, part) in parts.iter().enumerate() {
        let comp = current.get(part.ident.text.as_ref())?;
        if !comp.shape.is_empty() || !comp.shape_expr.is_empty() {
            return Some(i);
        }
        if i + 1 < parts.len() {
            let class = lookup_class_def(tree, comp)?;
            let effective = get_effective_components(tree, class).unwrap_or_default();
            owned = Some(if effective.is_empty() {
                class.components.clone()
            } else {
                effective
            });
            current = owned.as_ref().unwrap();
        }
    }
    None
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

/// Format an indexed component name (e.g., "r[1]" or "m[1,2]").
fn format_indexed_name(name: &str, indices: &[i64]) -> String {
    if indices.len() == 1 {
        format!("{}[{}]", name, indices[0])
    } else {
        let idx_str = indices
            .iter()
            .map(|i| i.to_string())
            .collect::<Vec<_>>()
            .join(",");
        format!("{}[{}]", name, idx_str)
    }
}

#[cfg(test)]
mod tests {
    use super::{
        distribute_component_ref_mods_for_element, distribute_mods_for_element,
        index_binding_for_element, pre_resolve_array_modifications, resolve_mod_to_array,
    };
    use indexmap::IndexMap;
    use rumoca_core::DefId;
    use rumoca_ir_ast as ast;
    use std::sync::Arc;

    fn make_token(text: &str) -> rumoca_ir_core::Token {
        rumoca_ir_core::Token {
            text: Arc::from(text),
            location: rumoca_ir_core::Location::default(),
            token_number: 0,
            token_type: 0,
        }
    }

    fn make_int_expr(value: i64) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: make_token(&value.to_string()),
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
            },
            args,
        }
    }

    fn make_range_expr(start: i64, end: i64) -> ast::Expression {
        ast::Expression::Range {
            start: Arc::new(make_int_expr(start)),
            step: None,
            end: Arc::new(make_int_expr(end)),
        }
    }

    #[test]
    fn test_resolve_mod_to_array_fill_constructor() {
        let expr = make_function_call("fill", vec![make_int_expr(7), make_int_expr(3)]);
        let resolved = resolve_mod_to_array(
            &expr,
            &rumoca_ir_ast::ModificationEnvironment::default(),
            &IndexMap::new(),
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
        let mut parent_components = IndexMap::new();
        let array_comp = ast::Component {
            name: "arr".to_string(),
            shape: vec![3],
            ..Default::default()
        };
        parent_components.insert("arr".to_string(), array_comp);

        let binding = make_comp_ref_expr(&["arr", "v"]);
        let indexed = index_binding_for_element(
            &ast::ClassTree::default(),
            &parent_components,
            &binding,
            &[2],
        );

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
    fn test_index_binding_for_element_projects_explicit_range_slice() {
        let mut parent_components = IndexMap::new();
        parent_components.insert(
            "root".to_string(),
            ast::Component {
                name: "root".to_string(),
                shape: vec![20],
                ..Default::default()
            },
        );
        let mut binding = make_comp_ref_expr(&["root"]);
        if let ast::Expression::ComponentReference(cref) = &mut binding {
            cref.parts[0].subs = Some(vec![ast::Subscript::Expression(make_range_expr(11, 15))]);
        }

        let indexed = index_binding_for_element(
            &ast::ClassTree::default(),
            &parent_components,
            &binding,
            &[2],
        );

        let ast::Expression::ComponentReference(cref) = indexed else {
            panic!("expected projected component reference");
        };
        let Some(subs) = &cref.parts[0].subs else {
            panic!("projected array part should retain a scalar subscript");
        };
        let ast::Subscript::Expression(ast::Expression::Terminal { token, .. }) = &subs[0] else {
            panic!("range projection should produce an integer subscript");
        };
        assert_eq!(token.text.as_ref(), "12");
    }

    #[test]
    fn test_index_binding_for_element_no_array_part_uses_array_index_fallback() {
        let binding = make_comp_ref_expr(&["a", "b", "c"]);
        let indexed =
            index_binding_for_element(&ast::ClassTree::default(), &IndexMap::new(), &binding, &[1]);

        let ast::Expression::ArrayIndex { base, subscripts } = indexed else {
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
        };

        let indexed = index_binding_for_element(
            &ast::ClassTree::default(),
            &IndexMap::new(),
            &binding,
            &[2, 1],
        );
        let ast::Expression::Terminal { token, .. } = indexed else {
            panic!("multi-index comprehension should project to a concrete element expression");
        };
        assert_eq!(token.text.as_ref(), "2");
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
        };
        let binding = ast::Expression::ArrayComprehension {
            expr: Arc::new(inner),
            indices: vec![ast::ForIndex {
                ident: make_token("ks"),
                range: make_range_expr(1, 3),
            }],
            filter: None,
        };

        let indexed = index_binding_for_element(
            &ast::ClassTree::default(),
            &IndexMap::new(),
            &binding,
            &[2, 1],
        );
        let ast::Expression::Terminal { token, .. } = indexed else {
            panic!("nested comprehensions should project to a concrete element expression");
        };
        assert_eq!(token.text.as_ref(), "2");
    }

    #[test]
    fn test_distribute_mods_for_element_projects_matrix_row_as_vector() {
        let mut comp = ast::Component::default();
        comp.modifications.insert(
            "VolFloCur".to_string(),
            ast::Expression::Array {
                elements: vec![
                    ast::Expression::Array {
                        elements: vec![make_int_expr(1), make_int_expr(2), make_int_expr(3)],
                        is_matrix: true,
                    },
                    ast::Expression::Array {
                        elements: vec![make_int_expr(4), make_int_expr(5), make_int_expr(6)],
                        is_matrix: true,
                    },
                ],
                is_matrix: true,
            },
        );

        let resolved_mods = pre_resolve_array_modifications(
            &comp,
            &rumoca_ir_ast::ModificationEnvironment::default(),
            &IndexMap::new(),
            &ast::ClassTree::default(),
        );

        let mut scalar_comp = comp.clone();
        distribute_mods_for_element(&mut scalar_comp, &resolved_mods, &[2]);
        let distributed = scalar_comp
            .modifications
            .get("VolFloCur")
            .expect("missing distributed modifier");

        let ast::Expression::Array {
            elements,
            is_matrix,
        } = distributed
        else {
            panic!("distributed row should remain an array");
        };
        assert!(
            !is_matrix,
            "distributed matrix row should be a 1-D array, not a single-row matrix"
        );
        assert_eq!(elements.len(), 3);
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
                ..Default::default()
            },
        );
        tree.definitions
            .classes
            .insert("StackData".to_string(), stack_data);
        tree.def_map.insert(stack_data_id, "StackData".to_string());
        tree.name_map.insert("StackData".to_string(), stack_data_id);

        let mut parent_components = IndexMap::new();
        parent_components.insert(
            "stackData".to_string(),
            ast::Component {
                name: "stackData".to_string(),
                type_name: ast::Name {
                    name: vec![make_token("StackData")],
                    def_id: Some(stack_data_id),
                },
                type_def_id: Some(stack_data_id),
                ..Default::default()
            },
        );

        let binding = make_comp_ref_expr(&["stackData", "cellData"]);
        let indexed = index_binding_for_element(&tree, &parent_components, &binding, &[2, 1]);

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
        let mut comp = ast::Component::default();
        comp.modifications.insert(
            "k".to_string(),
            make_function_call("fill", vec![make_int_expr(5), make_int_expr(2)]),
        );

        let resolved_mods = pre_resolve_array_modifications(
            &comp,
            &rumoca_ir_ast::ModificationEnvironment::default(),
            &IndexMap::new(),
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
        let mut comp = ast::Component::default();
        comp.modifications
            .insert("cellData".to_string(), make_comp_ref_expr(&["arr", "v"]));

        let mut parent_components = IndexMap::new();
        parent_components.insert(
            "arr".to_string(),
            ast::Component {
                name: "arr".to_string(),
                shape: vec![3],
                ..Default::default()
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
        );

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
