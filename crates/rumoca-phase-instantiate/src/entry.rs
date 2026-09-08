use super::*;

pub(crate) fn description_tokens_to_string(tokens: &[rumoca_core::Token]) -> Option<String> {
    if tokens.is_empty() {
        return None;
    }
    Some(tokens.iter().map(|token| token.text.as_ref()).collect())
}

/// Instantiate a model and return structured outcome.
///
/// This function distinguishes between:
/// - `Success`: Model instantiated successfully
/// - `NeedsInner`: Model has outer components without matching inner declarations
/// - `Error`: Actual instantiation error
///
/// MLS §5.4: Models with `outer` components need `inner` declarations from
/// an enclosing scope. These are not failures - they're context-dependent models.
pub fn instantiate_model_with_outcome(
    tree: &ast::ClassTree,
    model_name: &str,
) -> InstantiationOutcome {
    instantiate_model_with_outcome_options(tree, model_name, InstantiateOptions::default())
}

/// Instantiate a model and return structured outcome with caller-supplied options.
pub fn instantiate_model_with_outcome_options(
    tree: &ast::ClassTree,
    model_name: &str,
    options: InstantiateOptions,
) -> InstantiationOutcome {
    let class_index = ast::ClassDefIndex::from_tree(tree);
    // `options` is still needed below for the missing-inner retry, so clone it
    // into the context (the inner Vec is empty on the common path).
    let mut ctx = InstantiateContext::with_options(options.clone());
    ctx.index_source_scopes(tree);

    // Seed the root modification environment with any synthetic structural
    // overrides. These flow down to nested components exactly like source-level
    // modifications, so array dimensions and conditional components re-evaluate.
    for (target, value) in options.root_modifications.iter().cloned() {
        ctx.mod_env_mut().add(target, value);
    }

    // Find the model to instantiate using qualified name lookup
    let model = match find_class_in_tree(tree, model_name) {
        Some(m) => m,
        None => {
            return InstantiationOutcome::Error(Box::new(InstantiateError::ModelNotFound(
                model_name.to_string(),
            )));
        }
    };

    let mut overlay = ast::InstanceOverlay::new();

    // MLS §4.7: Track if the root model is partial (incomplete for standalone use).
    // Partial models may legally contain partial components.
    ctx.set_allow_partial_instantiation(model.partial);
    overlay.is_partial = model.partial;
    overlay.class_type = model.class_type.clone();
    overlay.root_description = description_tokens_to_string(&model.description);

    // Instantiate the root model
    if let Err(e) = instantiate_class(
        tree,
        &class_index,
        model,
        ClassOccurrenceConstruction::FreshRoot,
        &mut ctx,
        &mut overlay,
    ) {
        return InstantiationOutcome::Error(e);
    }

    // Check if there are missing inner declarations
    if ctx.has_missing_inners() {
        // MLS §5.4: Attempt to synthesize default inner declarations and retry.
        let missing = ctx.missing_inner_infos().to_vec();
        match retry_with_synthetic_inners(tree, &class_index, model, &missing, options) {
            Ok(mut retry_overlay) => {
                retry_overlay.synthesized_inners = missing
                    .iter()
                    .map(|info| info.name.clone())
                    .collect::<std::collections::BTreeSet<_>>()
                    .into_iter()
                    .collect();
                successful_instantiation_outcome(tree, retry_overlay)
            }
            Err(SyntheticInnerError::StillMissing {
                missing_inners,
                missing_spans,
                partial_overlay,
            }) => InstantiationOutcome::NeedsInner {
                missing_inners,
                missing_spans,
                partial_overlay: *partial_overlay,
            },
            Err(SyntheticInnerError::Error(error)) => InstantiationOutcome::Error(error),
        }
    } else {
        successful_instantiation_outcome(tree, overlay)
    }
}

fn successful_instantiation_outcome(
    tree: &ast::ClassTree,
    mut overlay: ast::InstanceOverlay,
) -> InstantiationOutcome {
    if let Err(reason) = overlay.finalize_overconstrained_record_owners() {
        return InstantiationOutcome::Error(instance_owner_finalization_error(
            tree, &overlay, reason,
        ));
    }
    InstantiationOutcome::Success(overlay)
}

fn instance_owner_finalization_error(
    tree: &ast::ClassTree,
    overlay: &ast::InstanceOverlay,
    reason: ast::EqualityConstraintOccurrenceError,
) -> Box<InstantiateError> {
    let Some(occurrence) = reason.occurrence() else {
        return Box::new(InstantiateError::missing_source_context(format!(
            "invalid instance occurrence has no source owner: {reason}"
        )));
    };
    let Some(component) = overlay.components.get(&occurrence) else {
        return Box::new(InstantiateError::missing_source_context(format!(
            "invalid instance occurrence {occurrence:?} is absent: {reason}"
        )));
    };
    let Ok(span) = location_to_span(
        &component.source_location,
        &tree.source_map,
        "invalid instance occurrence",
    ) else {
        return Box::new(InstantiateError::missing_source_context(format!(
            "invalid instance occurrence {occurrence:?} has no source span: {reason}"
        )));
    };
    Box::new(InstantiateError::invalid_instance_occurrence(
        reason.to_string(),
        span,
    ))
}
