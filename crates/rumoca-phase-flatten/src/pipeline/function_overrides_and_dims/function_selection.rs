//! Callable selection: which function declaration a call occurrence
//! exposes and which implementation it selects.

use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct FunctionSelection {
    pub(super) exposure: rumoca_core::DefId,
    pub(super) implementation: rumoca_core::DefId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum CallOccurrenceIdentity {
    SelectedImplementation,
    ExposedDeclaration,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct ResolvedFunctionRewrite {
    pub(super) display_name: String,
    pub(super) selection: FunctionSelection,
    pub(super) occurrence_identity: CallOccurrenceIdentity,
}

fn resolve_exact_constructor_rewrite(
    reference: &rumoca_core::Reference,
    target: rumoca_core::DefId,
    ctx: &FunctionOverrideRewriteContext<'_>,
    span: rumoca_core::Span,
) -> Result<ResolvedFunctionRewrite, FlattenError> {
    let target_class = ctx.class_index.get(target).ok_or_else(|| {
        FlattenError::missing_function_selection_identity(
            reference.as_str(),
            "constructor target DefId is absent from the resolved class index",
            span,
        )
    })?;
    if target_class.class_type == rumoca_core::ClassType::Package {
        return Err(FlattenError::missing_function_selection_identity(
            reference.as_str(),
            "constructor target DefId resolves to a package",
            span,
        ));
    }
    resolved_function_rewrite(
        reference,
        FunctionSelection {
            exposure: target,
            implementation: target,
        },
        None,
        ctx,
        span,
        "selected constructor has no canonical display entry",
    )
}

fn exact_function_implementation(
    reference: &rumoca_core::Reference,
    exposure: rumoca_core::DefId,
    class_def: &rumoca_ir_ast::ClassDef,
    ctx: &FunctionOverrideRewriteContext<'_>,
    span: rumoca_core::Span,
) -> Result<rumoca_core::DefId, FlattenError> {
    if class_def.class_type != rumoca_core::ClassType::Function {
        return Err(FlattenError::missing_function_selection_identity(
            reference.as_str(),
            "call target DefId does not resolve to a function",
            span,
        ));
    }
    let implementation = if function_alias_requires_exact_selection(class_def) {
        resolve_function_extends_target_def_id(ctx.class_index, exposure).ok_or_else(|| {
            FlattenError::missing_function_selection_identity(
                reference.as_str(),
                "exposed function has no unique exact extends implementation",
                span,
            )
        })?
    } else {
        exposure
    };
    Ok(implementation)
}

fn exact_external_object_constructor_selection(
    reference: &rumoca_core::Reference,
    owner: rumoca_core::DefId,
    ctx: &FunctionOverrideRewriteContext<'_>,
    span: rumoca_core::Span,
) -> Result<Option<FunctionSelection>, FlattenError> {
    let lifecycle = ctx
        .class_index
        .external_object_lifecycle(owner)
        .map_err(|error| {
            FlattenError::missing_function_selection_identity(
                reference.as_str(),
                error.required_fact(),
                span,
            )
        })?;
    Ok(lifecycle.map(|lifecycle| FunctionSelection {
        exposure: lifecycle.owner_def_id(),
        implementation: lifecycle.constructor_def_id(),
    }))
}

fn exact_function_exposure(
    reference: &rumoca_core::Reference,
    component_ref: &rumoca_core::ComponentReference,
    current_target: rumoca_core::DefId,
    implementation: rumoca_core::DefId,
    ctx: &FunctionOverrideRewriteContext<'_>,
    span: rumoca_core::Span,
) -> Result<rumoca_core::DefId, FlattenError> {
    let mut exposures = FxHashSet::default();
    if let Some(prefix) = component_ref.component_scope().prefix_parts().last() {
        let owner = exact_prefix_owner_def_id(ctx.class_index, prefix.def_id).ok_or_else(|| {
            FlattenError::missing_function_selection_identity(
                reference.as_str(),
                "callable prefix DefId has no exact class owner",
                span,
            )
        })?;
        collect_function_exposures_for_implementation(
            ctx.class_index,
            owner,
            implementation,
            &mut FxHashSet::default(),
            &mut exposures,
        );
        if exposures.is_empty() {
            return Err(FlattenError::missing_function_selection_identity(
                reference.as_str(),
                "exact callable owner does not expose the selected implementation",
                span,
            ));
        }
    }
    if current_target != implementation {
        exposures.insert(current_target);
    }
    match exposures.len() {
        0 => Ok(implementation),
        1 => Ok(*exposures
            .iter()
            .next()
            .expect("a singleton exact exposure set is nonempty")),
        _ => Err(FlattenError::missing_function_selection_identity(
            reference.as_str(),
            "callable owner has multiple exact exposures for the selected implementation",
            span,
        )),
    }
}

fn resolved_function_rewrite(
    reference: &rumoca_core::Reference,
    selection: FunctionSelection,
    display_name: Option<String>,
    ctx: &FunctionOverrideRewriteContext<'_>,
    span: rumoca_core::Span,
    missing_display_reason: &'static str,
) -> Result<ResolvedFunctionRewrite, FlattenError> {
    ctx.tree
        .def_map
        .get(&selection.implementation)
        .ok_or_else(|| {
            FlattenError::missing_function_selection_identity(
                reference.as_str(),
                missing_display_reason,
                span,
            )
        })?;
    Ok(ResolvedFunctionRewrite {
        display_name: display_name.unwrap_or_else(|| reference.as_str().to_string()),
        selection,
        occurrence_identity: CallOccurrenceIdentity::SelectedImplementation,
    })
}

fn exact_override_package_for_source_package<'a>(
    reference: &rumoca_core::Reference,
    source_package: rumoca_core::DefId,
    ctx: &'a FunctionOverrideRewriteContext<'a>,
    span: rumoca_core::Span,
) -> Result<Option<&'a OverrideTarget>, FlattenError> {
    let mut active = Vec::new();
    let mut inherited = Vec::new();
    for package in ctx.override_packages {
        let contains = exact_package_chain_contains_def_id(
            ctx.class_index,
            package.def_id,
            source_package,
            &mut FxHashSet::default(),
        )
        .map_err(|reason| {
            FlattenError::missing_function_selection_identity(reference.as_str(), reason, span)
        })?;
        if contains {
            inherited.push(package);
            if package.active {
                active.push(package);
            }
        }
    }
    let candidates = if active.is_empty() { inherited } else { active };
    match candidates.as_slice() {
        [] => Ok(None),
        [package] => Ok(Some(*package)),
        _ => Err(FlattenError::missing_function_selection_identity(
            reference.as_str(),
            "function source package has multiple exact override package selections",
            span,
        )),
    }
}

fn exact_package_function_rewrite(
    reference: &rumoca_core::Reference,
    selection: FunctionSelection,
    ctx: &FunctionOverrideRewriteContext<'_>,
    span: rumoca_core::Span,
) -> Result<Option<ResolvedFunctionRewrite>, FlattenError> {
    let Some(source_owner) = ctx.class_index.parent_def_id(selection.exposure) else {
        return Ok(None);
    };
    let member = exact_function_member_name(ctx.class_index, source_owner, selection.exposure)
        .map_err(|reason| {
            FlattenError::missing_function_selection_identity(reference.as_str(), reason, span)
        })?;
    let Some(member) = member else {
        return Ok(None);
    };
    let Some(package) =
        exact_override_package_for_source_package(reference, source_owner, ctx, span)?
    else {
        return Ok(None);
    };
    let exposure = exact_package_function_exposure(
        ctx.class_index,
        package.def_id,
        &member,
        &mut FxHashSet::default(),
    )
    .map_err(|reason| {
        FlattenError::missing_function_selection_identity(reference.as_str(), reason, span)
    })?
    .ok_or_else(|| {
        FlattenError::missing_function_selection_identity(
            reference.as_str(),
            "selected package does not expose the exact source function slot",
            span,
        )
    })?;
    let class_def = ctx.class_index.get(exposure).ok_or_else(|| {
        FlattenError::missing_function_selection_identity(
            reference.as_str(),
            "selected package function DefId is absent from the resolved class index",
            span,
        )
    })?;
    let implementation = exact_function_implementation(reference, exposure, class_def, ctx, span)?;
    let projected = FunctionSelection {
        exposure,
        implementation,
    };
    if projected == selection {
        return Ok(None);
    }
    resolved_function_rewrite(
        reference,
        projected,
        Some(format!("{}.{}", package.name, member)),
        ctx,
        span,
        "selected package implementation has no canonical display entry",
    )
    .map(Some)
}

pub(super) fn resolve_exact_function_rewrite(
    reference: &rumoca_core::Reference,
    is_constructor: bool,
    ctx: &FunctionOverrideRewriteContext<'_>,
    span: rumoca_core::Span,
) -> Result<Option<ResolvedFunctionRewrite>, FlattenError> {
    let Some(component_ref) = reference.component_ref() else {
        if reference.is_generated() {
            return Ok(None);
        }
        return Err(FlattenError::missing_function_selection_identity(
            reference.as_str(),
            "callable selection requires a structured occurrence identity",
            span,
        ));
    };
    let current_target = component_ref.target_def_id();
    if ctx
        .class_index
        .get(current_target)
        .is_some_and(|class| class.class_type == rumoca_core::ClassType::Record)
    {
        return Ok(None);
    }
    if is_constructor {
        return resolve_exact_constructor_rewrite(reference, current_target, ctx, span).map(Some);
    }
    let target_class = ctx.class_index.get(current_target).ok_or_else(|| {
        FlattenError::missing_function_selection_identity(
            reference.as_str(),
            "callable target DefId is absent from the resolved class index",
            span,
        )
    })?;
    if target_class.class_type != rumoca_core::ClassType::Function
        && let Some(selection) =
            exact_external_object_constructor_selection(reference, current_target, ctx, span)?
    {
        let mut rewrite = resolved_function_rewrite(
            reference,
            selection,
            None,
            ctx,
            span,
            "ExternalObject constructor has no canonical display entry",
        )?;
        rewrite.occurrence_identity = CallOccurrenceIdentity::ExposedDeclaration;
        return Ok(Some(rewrite));
    }
    let implementation =
        exact_function_implementation(reference, current_target, target_class, ctx, span)?;
    let exposure = exact_function_exposure(
        reference,
        component_ref,
        current_target,
        implementation,
        ctx,
        span,
    )?;
    let selection = FunctionSelection {
        exposure,
        implementation,
    };
    if let Some(rewrite) = exact_package_function_rewrite(reference, selection, ctx, span)? {
        return Ok(Some(rewrite));
    }
    resolved_function_rewrite(
        reference,
        selection,
        None,
        ctx,
        span,
        "selected implementation has no canonical display entry",
    )
    .map(Some)
}
