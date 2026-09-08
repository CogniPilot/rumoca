//! Inner/outer declaration resolution and synthetic inner retry (MLS §5.4).
//!
//! Extracted from the instantiate facade so the phase entry points stay within
//! the SPEC_0021 file-size budget.

#[cfg(test)]
mod prescan_tests;

use super::{
    ClassOccurrenceConstruction, ComponentImports, ComponentInstantiationScope, IndexMap,
    InnerDeclaration, InstantiateContext, InstantiateError, InstantiateEvalCtx, InstantiateOptions,
    InstantiateResult, MissingInnerInfo, OuterValues, TypeOverrideMap, ast,
    component_allows_structural_evaluation, component_declaration_source_scope,
    description_tokens_to_string, evaluate_component_condition_with_outer_values,
    expression_source_scope, find_class_in_tree, get_or_compute_template, instantiate_class,
    instantiate_component, is_type_compatible_with_def_id, issue_selected_component_types,
    location_to_span, path_utils, resolve_effective_components_for_eval, try_eval_integer_expr,
    try_eval_real_expr,
};
use rumoca_eval_ast::eval_instantiate::{AstScalarKind, canonical_scalar_kind};
use rustc_hash::FxHashMap;

/// Error type for synthetic inner retry attempts.
pub(crate) enum SyntheticInnerError {
    /// Some missing inners could not be resolved (type not found or transitive outers).
    StillMissing {
        missing_inners: Vec<String>,
        missing_spans: Vec<rumoca_core::Span>,
        partial_overlay: Box<ast::InstanceOverlay>,
    },
    /// The exact phase error raised while constructing or instantiating the retry.
    Error(Box<InstantiateError>),
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
    class_index: &ast::ClassDefIndex<'_>,
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
    let root_instance_id = overlay.alloc_id();

    // For each missing inner, look up the class, register it in root scope,
    // and instantiate its sub-components at root level.
    for mi in missing {
        let inner_class = match find_class_in_tree(tree, &mi.type_name) {
            Some(c) => c,
            None => continue, // Skip if type not found; will remain missing
        };

        let synthetic = create_synthetic_inner_component(mi, inner_class, &tree.source_map)
            .map_err(SyntheticInnerError::Error)?;

        let qn = ast::QualifiedName::from_ident(&mi.name);

        // Register in root scope so outer lookups will find it
        ctx.register_inner_in_root(&mi.name, qn, &mi.type_name, mi.type_def_id);

        // Instantiate the synthetic inner component at root level
        let empty_siblings = IndexMap::default();
        let empty_type_overrides = TypeOverrideMap::new();
        let mut synthetic_declaration = IndexMap::default();
        synthetic_declaration.insert(mi.name.clone(), synthetic.clone());
        let selected_component_types = issue_selected_component_types(
            tree,
            &synthetic_declaration,
            &empty_type_overrides,
            None,
        )
        .map_err(SyntheticInnerError::Error)?;
        ctx.push_path(&mi.name);
        instantiate_component(
            tree,
            &synthetic,
            &mut ctx,
            &mut overlay,
            ComponentInstantiationScope {
                owner_class_id: Some(root_instance_id),
                effective_components: &empty_siblings,
                type_overrides: &empty_type_overrides,
                selected_component_types: &selected_component_types,
                imports: ComponentImports {
                    class_index,
                    overriding_constants: &[],
                    enclosing_constants: &[],
                    class_imports: None,
                },
            },
        )
        .map_err(SyntheticInnerError::Error)?;
        ctx.pop_path();
    }

    // Re-run the main model instantiation with inners now available
    instantiate_class(
        tree,
        class_index,
        model,
        ClassOccurrenceConstruction::ReservedRoot(root_instance_id),
        &mut ctx,
        &mut overlay,
    )
    .map_err(SyntheticInnerError::Error)?;

    // Check if there are still missing inners (transitive)
    if ctx.has_missing_inners() {
        let (missing_inners, missing_spans) = ctx.unique_missing_inner_summary();
        return Err(SyntheticInnerError::StillMissing {
            missing_inners,
            missing_spans,
            partial_overlay: Box::new(overlay),
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
    let inner_decl = comp.inner.then(|| InnerDeclaration {
        qualified_name: qualified_name.clone(),
        type_name: resolved_type_name.clone(),
        type_def_id: resolved_type_def_id,
    });
    let pending_resolutions = if let Some(inner_decl) = inner_decl.as_ref() {
        Some(plan_pending_outer_refs_for_inner(
            tree, ctx, &comp.name, inner_decl,
        )?)
    } else {
        None
    };

    let outer_span = if comp.outer {
        Some(location_to_span(
            &comp.location,
            &tree.source_map,
            "outer component",
        )?)
    } else {
        None
    };
    let matching_inner = if comp.outer {
        // MLS §5.4: For `inner outer`, find the PARENT's inner (skip self).
        // For pure `outer`, find the nearest inner (may be self if inner outer).
        let candidate = if comp.inner {
            ctx.find_parent_inner(&comp.name)
        } else {
            ctx.find_inner(&comp.name)
        };
        candidate.cloned()
    } else {
        None
    };
    if let (Some(inner), Some(span)) = (matching_inner.as_ref(), outer_span) {
        let types_compatible = is_type_compatible_with_def_id(
            tree,
            &resolved_type_name,
            resolved_type_def_id,
            &inner.type_name,
            inner.type_def_id,
            span,
        )?;
        if !types_compatible {
            return Err(Box::new(InstantiateError::inner_outer_type_mismatch(
                &comp.name,
                &resolved_type_name,
                &inner.type_name,
                span,
            )));
        }
    }

    // All identity and type evidence is proven before committing any context
    // or overlay mutation, so an error cannot leave a partial inner/outer map.
    if comp.inner || comp.outer {
        // MLS §5.4 registrations are path-dependent; record the event so
        // compact array replication can refuse to derive the other elements.
        ctx.inner_outer_events += 1;
    }
    if let Some(inner_decl) = inner_decl.as_ref() {
        ctx.register_inner(
            &comp.name,
            qualified_name.clone(),
            &resolved_type_name,
            resolved_type_def_id,
        );
        apply_pending_outer_resolutions(
            ctx,
            overlay,
            inner_decl,
            pending_resolutions.expect("inner declaration has a pending-resolution plan"),
        );
    }
    if comp.outer {
        let span = outer_span.expect("outer declaration has a checked source span");
        if let Some(inner_decl) = matching_inner {
            let outer_path = qualified_name.to_component_path();
            let inner_path = inner_decl.qualified_name.to_component_path();
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
/// The pre-pass also records the inner class's scalar structural values so nested
/// conditions, dimensions, and connection domains can be decided. Nothing is
/// invented: a modifier this scope cannot evaluate drops that occurrence and its
/// dependents instead of falling back to the class default. Authoritative values
/// still overwrite the prescan when the inner is instantiated.
pub(crate) fn preregister_class_inners(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
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
            class_index,
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

/// Record the scalar structural parameter values of a not-yet-instantiated `inner`.
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
    class_index: &ast::ClassDefIndex<'_>,
    prescan: InnerParamPrescan<'_>,
    ctx: &mut InstantiateContext,
) -> InstantiateResult<()> {
    let parent_eval_ctx = InstantiateEvalCtx {
        tree,
        mod_env: ctx.mod_env(),
        effective_components: prescan.parent_components,
        resolve_class_components: resolve_effective_components_for_eval,
    };
    let inner_occurrence_allows = !matches!(
        prescan.comp.variability,
        rumoca_core::Variability::Parameter(_)
    ) || component_allows_structural_evaluation(
        &prescan.comp.name,
        prescan.comp,
        &parent_eval_ctx,
    );
    if ctx.structural_evaluation_blocked() || !inner_occurrence_allows {
        return Ok(());
    }
    let Some(inner_class) = find_class_in_tree(tree, prescan.resolved_type_name) else {
        return Ok(());
    };
    let template = get_or_compute_template(tree, inner_class, &mut ctx.template_cache)?;
    let effective_components = occurrence_effective_components(
        tree,
        class_index,
        &template.effective_components,
        &prescan,
        ctx,
    )?;
    let params = fold_inner_structural_params(tree, &effective_components);
    ctx.register_known_bool_params(prescan.instance_path, &params.bools);
    ctx.register_known_int_params(prescan.instance_path, &params.integers);
    ctx.register_known_real_params(prescan.instance_path, &params.reals);
    Ok(())
}

fn occurrence_effective_components(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    declarations: &IndexMap<String, ast::Component>,
    prescan: &InnerParamPrescan<'_>,
    ctx: &mut InstantiateContext,
) -> InstantiateResult<IndexMap<String, ast::Component>> {
    let mut effective_components = declarations.clone();
    for (name, decl) in &mut effective_components {
        if !is_structural_scalar_declaration(decl) {
            continue;
        }
        let modifier = inner_parameter_modifier(ctx, prescan.comp, name);
        if let Some(fixed) = modifier.fixed {
            let Some(effective_fixed) = evaluate_modifier_in_source_scope(
                tree,
                class_index,
                prescan.parent_components,
                &fixed,
                AstScalarKind::Boolean,
                ctx,
            )?
            else {
                decl.binding = None;
                continue;
            };
            if exact_boolean_literal(&effective_fixed) != Some(true) {
                decl.binding = None;
                continue;
            }
            decl.modifications
                .insert("fixed".to_string(), effective_fixed);
        }
        let Some(binding) = modifier.binding else {
            continue;
        };
        let Some(kind) = canonical_scalar_kind(tree, decl) else {
            decl.binding = None;
            continue;
        };
        decl.binding = evaluate_modifier_in_source_scope(
            tree,
            class_index,
            prescan.parent_components,
            &binding,
            kind,
            ctx,
        )?;
    }
    Ok(effective_components)
}

struct PrescannedParams {
    bools: FxHashMap<String, bool>,
    integers: FxHashMap<String, i64>,
    reals: FxHashMap<String, f64>,
}

fn fold_inner_structural_params(
    tree: &ast::ClassTree,
    effective_components: &IndexMap<String, ast::Component>,
) -> PrescannedParams {
    let occurrence_mods = ast::ModificationEnvironment::new();
    let inner_ctx = InstantiateEvalCtx {
        tree,
        mod_env: &occurrence_mods,
        effective_components,
        resolve_class_components: resolve_effective_components_for_eval,
    };
    let mut bools = FxHashMap::default();
    let mut integers = FxHashMap::default();
    let mut reals = FxHashMap::default();
    for (name, decl) in effective_components {
        if !component_allows_structural_evaluation(name, decl, &inner_ctx)
            || !decl.shape.is_empty()
            || !decl.shape_expr.is_empty()
        {
            continue;
        }
        let Some(binding) = decl.binding.as_ref() else {
            continue;
        };
        match canonical_scalar_kind(tree, decl) {
            Some(AstScalarKind::Boolean) => {
                if let Some(value) = evaluate_component_condition_with_outer_values(
                    &inner_ctx,
                    binding,
                    OuterValues::default(),
                ) {
                    bools.insert(name.clone(), value);
                }
            }
            Some(AstScalarKind::Integer) => {
                if let Some(value) = try_eval_integer_expr(&inner_ctx, binding) {
                    integers.insert(name.clone(), value);
                }
            }
            Some(AstScalarKind::Real) => {
                if let Some(value) = try_eval_real_expr(&inner_ctx, binding) {
                    reals.insert(name.clone(), value);
                }
            }
            None => {}
        }
    }
    PrescannedParams {
        bools,
        integers,
        reals,
    }
}

struct InnerParameterModifier {
    binding: Option<ModifierBinding>,
    fixed: Option<ModifierBinding>,
}

struct ModifierBinding {
    expression: ast::Expression,
    source: ModifierSource,
}

enum ModifierSource {
    Applied(Option<ast::QualifiedName>),
    Declaration(Option<ast::QualifiedName>),
}

fn inner_parameter_modifier(
    ctx: &InstantiateContext,
    inner: &ast::Component,
    parameter: &str,
) -> InnerParameterModifier {
    let parameter_path = ast::QualifiedName::from_ident(&inner.name).child(parameter);
    let fixed_path = parameter_path.child("fixed");
    let direct = inner.modifications.get(parameter);
    let binding = ctx.mod_env().get(&parameter_path).map_or_else(
        || {
            direct
                .and_then(ast::Expression::component_modifier_binding_value)
                .map(|expression| ModifierBinding {
                    expression: expression.clone(),
                    source: ModifierSource::Declaration(
                        expression_source_scope(ctx, expression)
                            .map(|(scope, _)| scope)
                            .or_else(|| component_declaration_source_scope(ctx, inner)),
                    ),
                })
        },
        |applied| {
            applied
                .value
                .component_modifier_binding_value()
                .map(|expression| ModifierBinding {
                    expression: expression.clone(),
                    source: ModifierSource::Applied(applied.source_scope.clone()),
                })
        },
    );
    let applied_fixed = ctx.mod_env().get(&fixed_path);
    let direct_fixed = direct.and_then(modifier_fixed_attribute);
    let fixed = applied_fixed.map_or_else(
        || {
            direct_fixed.map(|expression| ModifierBinding {
                expression: expression.clone(),
                source: ModifierSource::Declaration(
                    expression_source_scope(ctx, expression)
                        .map(|(scope, _)| scope)
                        .or_else(|| component_declaration_source_scope(ctx, inner)),
                ),
            })
        },
        |applied| {
            Some(ModifierBinding {
                expression: applied.value.clone(),
                source: ModifierSource::Applied(applied.source_scope.clone()),
            })
        },
    );
    InnerParameterModifier { binding, fixed }
}

fn evaluate_modifier_in_source_scope(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    parent_components: &IndexMap<String, ast::Component>,
    binding: &ModifierBinding,
    kind: AstScalarKind,
    ctx: &mut InstantiateContext,
) -> InstantiateResult<Option<ast::Expression>> {
    let source_class = modifier_source_class(tree, ctx, &binding.source);
    let source_template = source_class
        .map(|class| get_or_compute_template(tree, class, &mut ctx.template_cache))
        .transpose()?;
    let unavailable_explicit_scope = match &binding.source {
        ModifierSource::Declaration(_) => source_template.is_none(),
        ModifierSource::Applied(Some(_)) => source_template.is_none(),
        ModifierSource::Applied(None) => false,
    };
    let empty_components = IndexMap::default();
    let empty_mods = ast::ModificationEnvironment::new();
    let components = source_template.as_ref().map_or_else(
        || {
            if unavailable_explicit_scope {
                &empty_components
            } else {
                parent_components
            }
        },
        |template| &template.effective_components,
    );
    // MLS §13.2: the modifier expression was written in the source class, so
    // that class's own import bindings (never an inherited union) apply.
    let source_imports = source_class
        .and_then(|class| class.scope_id)
        .map(|scope_id| tree.effective_imports(scope_id));
    let rewrite = crate::dims::ImportRewrite {
        class_index,
        overriding_aliases: &[],
        effective: source_imports.as_ref(),
        fallback_aliases: &[],
    };
    let expression = crate::dims::qualify_shape_expr_imports(tree, &binding.expression, rewrite)?;
    let eval_ctx = InstantiateEvalCtx {
        tree,
        mod_env: if unavailable_explicit_scope {
            &empty_mods
        } else {
            ctx.mod_env()
        },
        effective_components: components,
        resolve_class_components: resolve_effective_components_for_eval,
    };
    let span = binding.expression.span();
    Ok(match kind {
        AstScalarKind::Boolean => evaluate_component_condition_with_outer_values(
            &eval_ctx,
            &expression,
            OuterValues::default(),
        )
        .map(|value| boolean_terminal(value, span)),
        AstScalarKind::Integer => {
            try_eval_integer_expr(&eval_ctx, &expression).map(|value| integer_terminal(value, span))
        }
        AstScalarKind::Real => {
            try_eval_real_expr(&eval_ctx, &expression).map(|value| real_terminal(value, span))
        }
    })
}

fn modifier_source_class<'a>(
    tree: &'a ast::ClassTree,
    ctx: &InstantiateContext,
    source: &ModifierSource,
) -> Option<&'a ast::ClassDef> {
    match source {
        ModifierSource::Declaration(Some(scope)) => {
            find_class_in_tree(tree, &scope.to_flat_string())
        }
        ModifierSource::Applied(Some(scope)) => {
            let current_path = ctx.current_path();
            if !path_is_ancestor_or_same(scope, &current_path) {
                return None;
            }
            let frame = ctx.active_instantiations.get(scope.parts.len())?;
            let super::InstantiationFrameKey::Def(def_id) = frame.key;
            tree.get_class_by_def_id(def_id)
        }
        ModifierSource::Declaration(None) | ModifierSource::Applied(None) => None,
    }
}

fn is_structural_scalar_declaration(declaration: &ast::Component) -> bool {
    matches!(
        declaration.variability,
        rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
    ) && declaration.shape.is_empty()
        && declaration.shape_expr.is_empty()
}

fn modifier_fixed_attribute(expression: &ast::Expression) -> Option<&ast::Expression> {
    match expression {
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Assign,
            lhs,
            ..
        }
        | ast::Expression::Parenthesized { inner: lhs, .. } => modifier_fixed_attribute(lhs),
        ast::Expression::ClassModification { modifications, .. } => {
            modifications.iter().find_map(|modifier| match modifier {
                ast::Expression::Modification {
                    target,
                    value: Some(value),
                    ..
                } if reference_is_single_name(target, "fixed") => Some(value.as_ref()),
                ast::Expression::NamedArgument { name, value, .. }
                    if name.text.as_ref() == "fixed" =>
                {
                    Some(value.as_ref())
                }
                _ => None,
            })
        }
        _ => None,
    }
}

fn reference_is_single_name(reference: &ast::ComponentReference, expected: &str) -> bool {
    reference.parts.len() == 1
        && reference.parts[0].subs.is_none()
        && reference.parts[0].ident.text.as_ref() == expected
}

fn exact_boolean_literal(expression: &ast::Expression) -> Option<bool> {
    match expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
            ..
        } => match token.text.as_ref() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },
        ast::Expression::Parenthesized { inner, .. } => exact_boolean_literal(inner),
        _ => None,
    }
}

fn boolean_terminal(value: bool, span: rumoca_core::Span) -> ast::Expression {
    scalar_terminal(ast::TerminalType::Bool, value.to_string(), span)
}

fn integer_terminal(value: i64, span: rumoca_core::Span) -> ast::Expression {
    scalar_terminal(ast::TerminalType::UnsignedInteger, value.to_string(), span)
}

fn real_terminal(value: f64, span: rumoca_core::Span) -> ast::Expression {
    scalar_terminal(ast::TerminalType::UnsignedReal, value.to_string(), span)
}

fn scalar_terminal(
    terminal_type: ast::TerminalType,
    text: String,
    span: rumoca_core::Span,
) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type,
        token: rumoca_core::Token {
            text: text.into(),
            ..rumoca_core::Token::default()
        },
        span,
    }
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

fn plan_pending_outer_refs_for_inner(
    tree: &ast::ClassTree,
    ctx: &InstantiateContext,
    name: &str,
    inner_decl: &InnerDeclaration,
) -> InstantiateResult<Vec<bool>> {
    ctx.missing_inners
        .iter()
        .map(|missing| can_resolve_missing_inner(tree, name, inner_decl, missing))
        .collect()
}

fn apply_pending_outer_resolutions(
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    inner_decl: &InnerDeclaration,
    resolutions: Vec<bool>,
) {
    let mut remaining = Vec::new();
    for (missing, resolves) in ctx.missing_inners.drain(..).zip(resolutions) {
        if resolves {
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
) -> InstantiateResult<bool> {
    if missing.name != name || !inner_visible_to_outer(inner_decl, missing) {
        return Ok(false);
    }
    is_type_compatible_with_def_id(
        tree,
        &missing.type_name,
        missing.type_def_id,
        &inner_decl.type_name,
        inner_decl.type_def_id,
        missing.span,
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
    let outer_path = missing.outer_path.to_component_path();
    let inner_path = inner_decl.qualified_name.to_component_path();
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
