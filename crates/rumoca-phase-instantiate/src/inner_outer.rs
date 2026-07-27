//! Inner/outer declaration resolution and synthetic inner retry (MLS §5.4).
//!
//! Extracted from the instantiate facade so the phase entry points stay within
//! the SPEC_0021 file-size budget.

#![allow(clippy::wildcard_imports)]

use super::*;
use rustc_hash::FxHashMap;

/// Error type for synthetic inner retry attempts.
pub(crate) enum SyntheticInnerError {
    /// Some missing inners could not be resolved (type not found or transitive outers).
    StillMissing { names: Vec<String> },
    /// Synthetic declaration construction failed because required source context was missing.
    SourceContext(Box<InstantiateError>),
    /// The retry instantiation itself failed.
    InstantiationFailed,
}

/// Create a minimal synthetic inner `ast::Component` for a missing inner declaration.
///
/// MLS §5.4: When no matching inner is found, the compiler synthesizes a default
/// inner declaration using the type from the outer declaration.
pub(crate) fn create_synthetic_inner_component(
    mi: &MissingInnerInfo,
    class: &ast::ClassDef,
    source_map: &rumoca_core::SourceMap,
) -> InstantiateResult<ast::Component> {
    let span = location_to_span(&mi.source_location, source_map, "synthetic inner component")?;
    let name_token = rumoca_core::Token {
        text: std::sync::Arc::from(mi.name.clone()),
        location: mi.source_location.clone(),
        ..rumoca_core::Token::default()
    };
    let mut type_name = rumoca_ir_ast::Name::from_string(&mi.type_name);
    type_name.def_id = mi.type_def_id;
    for part in &mut type_name.name {
        part.location = mi.source_location.clone();
    }
    Ok(ast::Component {
        name: mi.name.clone(),
        name_token,
        type_name,
        type_def_id: mi.type_def_id,
        inner: true,
        location: mi.source_location.clone(),
        // Use the class's own def_id if available
        def_id: class.def_id,
        ..ast::Component::empty_with_span(span)
    })
}
/// Retry instantiation with synthetic inner declarations.
///
/// MLS §5.4: Creates a fresh context with synthetic inners pre-registered at root
/// scope, then re-runs instantiation. The synthetic inners are instantiated first
/// so their sub-components exist in the overlay before the main model references them.
pub(crate) fn retry_with_synthetic_inners(
    tree: &ast::ClassTree,
    model: &ast::ClassDef,
    missing: &[MissingInnerInfo],
    options: InstantiateOptions,
) -> Result<ast::InstanceOverlay, SyntheticInnerError> {
    let mut ctx = InstantiateContext::with_options(options);
    ctx.index_source_scopes(tree);
    let mut overlay = ast::InstanceOverlay::new();
    ctx.set_allow_partial_instantiation(model.partial);
    overlay.is_partial = model.partial;
    overlay.class_type = model.class_type.clone();
    overlay.root_description = description_tokens_to_string(&model.description);

    // For each missing inner, look up the class, register it in root scope,
    // and instantiate its sub-components at root level.
    for mi in missing {
        let inner_class = match find_class_in_tree(tree, &mi.type_name) {
            Some(c) => c,
            None => continue, // Skip if type not found; will remain missing
        };

        let synthetic = create_synthetic_inner_component(mi, inner_class, &tree.source_map)
            .map_err(SyntheticInnerError::SourceContext)?;

        // Build the qualified name for the root-level synthetic inner
        let qn = ast::QualifiedName::from_ident(&mi.name);

        // Register in root scope so outer lookups will find it
        ctx.register_inner_in_root(&mi.name, qn, &mi.type_name, mi.type_def_id);

        // Instantiate the synthetic inner component at root level
        let empty_siblings = IndexMap::default();
        let empty_type_overrides = TypeOverrideMap::new();
        ctx.push_path(&mi.name);
        if instantiate_component(
            tree,
            &synthetic,
            &mut ctx,
            &mut overlay,
            &empty_siblings,
            &empty_type_overrides,
            ComponentImports::EMPTY,
        )
        .is_err()
        {
            return Err(SyntheticInnerError::InstantiationFailed);
        }
        ctx.pop_path();
    }

    // Re-run the main model instantiation with inners now available
    if instantiate_class(tree, model, &mut ctx, &mut overlay).is_err() {
        return Err(SyntheticInnerError::InstantiationFailed);
    }

    // Check if there are still missing inners (transitive)
    if ctx.has_missing_inners() {
        return Err(SyntheticInnerError::StillMissing {
            names: ctx.missing_inner_names(),
        });
    }

    Ok(overlay)
}
/// Instantiate a component.
///
/// Handle inner/outer component declarations (MLS §5.4).
pub(crate) fn handle_inner_outer(
    tree: &ast::ClassTree,
    comp: &ast::Component,
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    qualified_name: &ast::QualifiedName,
    type_name: &str,
) -> InstantiateResult<()> {
    let resolved_type_name = resolve_inner_outer_type_name(tree, ctx, comp, type_name);
    let resolved_type_def_id = tree
        .name_map
        .get(&resolved_type_name)
        .copied()
        .or(comp.type_def_id);
    if comp.inner || comp.outer {
        // MLS §5.4 registrations are path-dependent; record the event so
        // compact array replication can refuse to derive the other elements.
        ctx.inner_outer_events += 1;
    }
    if comp.inner {
        let inner_decl = InnerDeclaration {
            qualified_name: qualified_name.clone(),
            type_name: resolved_type_name.clone(),
            type_def_id: resolved_type_def_id,
        };
        ctx.register_inner(
            &comp.name,
            qualified_name.clone(),
            &resolved_type_name,
            resolved_type_def_id,
        );
        resolve_pending_outer_refs_for_inner(tree, ctx, overlay, &comp.name, &inner_decl);
    }
    if comp.outer {
        let span = location_to_span(&comp.location, &tree.source_map, "outer component")?;
        // MLS §5.4: For `inner outer`, find the PARENT's inner (skip self).
        // For pure `outer`, find the nearest inner (may be self if inner outer).
        let inner_result = if comp.inner {
            ctx.find_parent_inner(&comp.name)
        } else {
            ctx.find_inner(&comp.name)
        };
        if let Some(inner_decl) = inner_result {
            let outer_path = qualified_name.to_flat_string();
            let inner_path = inner_decl.qualified_name.to_flat_string();
            // MLS §5.4: Record prefix mapping for flatten-phase redirection.
            // Pure outer → outer_prefix_to_inner (child refs redirected to inner).
            // Inner outer → inner_outer_to_parent_inner (same-level flow bridge).
            let target_map = if comp.inner {
                &mut overlay.inner_outer_to_parent_inner
            } else {
                &mut overlay.outer_prefix_to_inner
            };
            if outer_path != inner_path {
                target_map.insert(outer_path, inner_path);
            }
            let types_compatible = is_type_compatible_with_def_id(
                tree,
                &resolved_type_name,
                resolved_type_def_id,
                &inner_decl.type_name,
                inner_decl.type_def_id,
            );
            if !types_compatible {
                return Err(Box::new(InstantiateError::inner_outer_type_mismatch(
                    &comp.name,
                    &resolved_type_name,
                    &inner_decl.type_name,
                    span,
                )));
            }
        } else {
            ctx.record_missing_inner(MissingInnerInfo {
                name: comp.name.clone(),
                type_name: resolved_type_name,
                type_def_id: resolved_type_def_id,
                span,
                source_location: comp.location.clone(),
                outer_path: qualified_name.clone(),
                is_inner_outer: comp.inner,
            });
        }
    }
    Ok(())
}

/// Register every `inner` element of a class scope before its components are built.
///
/// MLS §5.4 makes an `inner` element visible in the whole class that declares it,
/// and MLS §4.5 gives the order of declarations inside a class no semantic meaning.
/// Registration used to happen only when the component loop reached the `inner`
/// declaration itself, so an `outer` reference from a component declared *above*
/// the `inner` element saw no match at all — `Modelica.Mechanics.MultiBody.
/// Examples.Loops.EngineV6` declares `bearing` (a `Revolute`, whose animation
/// shape is conditional on `world.enableAnimation`) before `inner world`, which
/// left the MLS §4.4.5 condition undecidable.
///
/// The pre-pass also records the inner class's Boolean parameter values so those
/// conditions can be decided. Nothing is invented: a modifier this scope cannot
/// evaluate drops the parameter instead of falling back to the class default, and
/// the authoritative values still overwrite these when the inner is instantiated.
pub(crate) fn preregister_class_inners(
    tree: &ast::ClassTree,
    effective_components: &IndexMap<String, ast::Component>,
    ctx: &mut InstantiateContext,
) -> InstantiateResult<()> {
    for (name, comp) in effective_components {
        // `inner outer` resolves against the *parent* scope, and a conditional or
        // array `inner` has no single settled instance path yet, so neither is
        // pre-registered here; both keep the component-loop behaviour.
        if !comp.inner
            || comp.outer
            || comp.condition.is_some()
            || !comp.shape.is_empty()
            || !comp.shape_expr.is_empty()
        {
            continue;
        }
        let type_name = comp.type_name.to_string();
        let resolved_type_name = resolve_inner_outer_type_name(tree, ctx, comp, &type_name);
        let resolved_type_def_id = tree
            .name_map
            .get(&resolved_type_name)
            .copied()
            .or(comp.type_def_id);
        ctx.push_path(name);
        let instance_path = ctx.current_path();
        ctx.pop_path();
        ctx.register_inner(
            name,
            instance_path.clone(),
            &resolved_type_name,
            resolved_type_def_id,
        );
        preregister_inner_params(
            tree,
            InnerParamPrescan {
                comp,
                resolved_type_name: &resolved_type_name,
                instance_path: &instance_path,
                parent_components: effective_components,
            },
            ctx,
        )?;
    }
    Ok(())
}

/// The one `inner` declaration whose parameters are being pre-scanned.
struct InnerParamPrescan<'a> {
    comp: &'a ast::Component,
    resolved_type_name: &'a str,
    instance_path: &'a ast::QualifiedName,
    parent_components: &'a IndexMap<String, ast::Component>,
}

/// Record the Boolean and Real parameter values of a not-yet-instantiated `inner`.
///
/// Class defaults come from the inner class's own declarations; a modifier written
/// in this scope (`inner World world(enableAnimation=animation)`) overrides them and
/// is evaluated in this scope. An undecidable modifier removes the parameter rather
/// than leaving the overridden default in place (SPEC_0008).
///
/// Real parameters are recorded alongside the Booleans because MLS §4.4.5
/// conditions compare them: `Parts.Body` gates its `sphere` visualiser on
/// `world.enableAnimation and animation and sphereDiameter > 0`, where
/// `sphereDiameter` is bound to the inner world's `defaultBodyDiameter`.
fn preregister_inner_params(
    tree: &ast::ClassTree,
    prescan: InnerParamPrescan<'_>,
    ctx: &mut InstantiateContext,
) -> InstantiateResult<()> {
    let Some(inner_class) = find_class_in_tree(tree, prescan.resolved_type_name) else {
        return Ok(());
    };
    let template = get_or_compute_template(tree, inner_class, &mut ctx.template_cache)?;
    let class_scope = ast::ModificationEnvironment::new();
    let inner_ctx = InstantiateEvalCtx {
        tree,
        mod_env: &class_scope,
        effective_components: &template.effective_components,
        resolve_class_components: resolve_effective_components_for_eval,
    };
    let mut bools = extract_bool_params_with_mods(&template.effective_components, &class_scope);
    let class_reals = extract_real_params_with_mods(&inner_ctx, &FxHashMap::default());
    let eval_ctx = InstantiateEvalCtx {
        tree,
        mod_env: ctx.mod_env(),
        effective_components: prescan.parent_components,
        resolve_class_components: resolve_effective_components_for_eval,
    };
    let mut real_overrides = FxHashMap::default();
    for (param, decl) in &template.effective_components {
        let Some(modifier) = inner_bool_modifier(eval_ctx.mod_env, prescan.comp, param) else {
            continue;
        };
        if is_boolean_parameter(decl) {
            match evaluate_component_condition_with_outer_values(
                &eval_ctx,
                modifier,
                OuterValues::default(),
            ) {
                Some(value) => bools.insert(param.clone(), value),
                None => bools.remove(param),
            };
        } else if class_reals.contains_key(param) {
            // A Real parameter this scope overrides with an expression that cannot
            // be decided here poisons every declaration derived from it, so no Real
            // is registered at all rather than some keeping a replaced class default.
            let Some(value) = try_eval_real_expr(&eval_ctx, modifier) else {
                ctx.register_known_bool_params(prescan.instance_path, &bools);
                return Ok(());
            };
            real_overrides.insert(param.clone(), value);
        }
    }
    ctx.register_known_bool_params(prescan.instance_path, &bools);
    ctx.register_known_real_params(
        prescan.instance_path,
        &applied_real_params(&inner_ctx, class_reals, real_overrides),
    );
    Ok(())
}

/// Re-fold the inner class's Real declarations against the overrides written on
/// the instance, so derived parameters follow their modified source (MLS §7.2).
fn applied_real_params(
    inner_ctx: &InstantiateEvalCtx<'_>,
    class_reals: FxHashMap<String, f64>,
    overrides: FxHashMap<String, f64>,
) -> FxHashMap<String, f64> {
    if overrides.is_empty() {
        return class_reals;
    }
    let mut reals = extract_real_params_with_mods(inner_ctx, &overrides);
    reals.extend(overrides);
    reals
}

fn is_boolean_parameter(decl: &ast::Component) -> bool {
    matches!(decl.variability, rumoca_core::Variability::Parameter(_))
        && rumoca_core::qualified_type_name_matches(&decl.type_name.to_string(), "Boolean")
}

/// The modifier applied to `<inner>.<param>` in the enclosing scope, if any.
///
/// MLS §7.2.4: a modification handed down from further out wins over the one
/// written on the declaration itself.
fn inner_bool_modifier<'a>(
    mod_env: &'a ast::ModificationEnvironment,
    comp: &'a ast::Component,
    param: &str,
) -> Option<&'a ast::Expression> {
    let mut path = ast::QualifiedName::from_ident(&comp.name);
    path.parts.push((param.to_string(), Vec::new()));
    if let Some(modification) = mod_env.get(&path) {
        return Some(&modification.value);
    }
    comp.modifications.get(param)
}

fn resolve_inner_outer_type_name(
    tree: &ast::ClassTree,
    ctx: &InstantiateContext,
    comp: &ast::Component,
    type_name: &str,
) -> String {
    if tree.name_map.contains_key(type_name) {
        return type_name.to_string();
    }

    if let Some(qualified) = comp
        .type_def_id
        .and_then(|def_id| tree.def_map.get(&def_id))
        && path_utils::class_name_leaf(qualified) == path_utils::class_name_leaf(type_name)
    {
        return qualified.clone();
    }

    let Some(source_scope) = component_declaration_source_scope(ctx, comp) else {
        return type_name.to_string();
    };
    resolve_type_name_in_source_scope(tree, type_name, &source_scope)
        .unwrap_or_else(|| type_name.to_string())
}

fn resolve_type_name_in_source_scope(
    tree: &ast::ClassTree,
    type_name: &str,
    source_scope: &ast::QualifiedName,
) -> Option<String> {
    // Walk the structured scope's prefixes from innermost outwards; the
    // candidate names are composed, never re-parsed.
    (0..=source_scope.parts.len())
        .rev()
        .map(|end| {
            let prefix = source_scope.parts[..end]
                .iter()
                .map(|(name, _)| name.as_str())
                .collect::<Vec<_>>()
                .join(".");
            if prefix.is_empty() {
                type_name.to_string()
            } else {
                format!("{prefix}.{type_name}")
            }
        })
        .find(|candidate| tree.name_map.contains_key(candidate))
}

fn resolve_pending_outer_refs_for_inner(
    tree: &ast::ClassTree,
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    name: &str,
    inner_decl: &InnerDeclaration,
) {
    let mut remaining = Vec::new();
    for missing in ctx.missing_inners.drain(..) {
        if can_resolve_missing_inner(tree, name, inner_decl, &missing) {
            record_late_inner_outer_mapping(overlay, &missing, inner_decl);
        } else {
            remaining.push(missing);
        }
    }
    ctx.missing_inners = remaining;
}

fn can_resolve_missing_inner(
    tree: &ast::ClassTree,
    name: &str,
    inner_decl: &InnerDeclaration,
    missing: &MissingInnerInfo,
) -> bool {
    missing.name == name
        && inner_visible_to_outer(inner_decl, missing)
        && is_type_compatible_with_def_id(
            tree,
            &missing.type_name,
            missing.type_def_id,
            &inner_decl.type_name,
            inner_decl.type_def_id,
        )
}

pub(crate) fn inner_visible_to_outer(
    inner_decl: &InnerDeclaration,
    missing: &MissingInnerInfo,
) -> bool {
    let inner_scope = inner_decl.qualified_name.parent().unwrap_or_default();
    let outer_scope = missing.outer_path.parent().unwrap_or_default();
    path_is_ancestor_or_same(&inner_scope, &outer_scope)
}

fn path_is_ancestor_or_same(ancestor: &ast::QualifiedName, path: &ast::QualifiedName) -> bool {
    ancestor.parts.len() <= path.parts.len()
        && ancestor.parts.iter().zip(path.parts.iter()).all(
            |((ancestor_name, ancestor_subs), (path_name, path_subs))| {
                ancestor_name == path_name && ancestor_subs == path_subs
            },
        )
}

fn record_late_inner_outer_mapping(
    overlay: &mut ast::InstanceOverlay,
    missing: &MissingInnerInfo,
    inner_decl: &InnerDeclaration,
) {
    let outer_path = missing.outer_path.to_flat_string();
    let inner_path = inner_decl.qualified_name.to_flat_string();
    if outer_path == inner_path {
        return;
    }

    if missing.is_inner_outer {
        overlay
            .inner_outer_to_parent_inner
            .insert(outer_path, inner_path);
    } else {
        overlay.outer_prefix_to_inner.insert(outer_path, inner_path);
    }
}
