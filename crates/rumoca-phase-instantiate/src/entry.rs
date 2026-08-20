use super::*;

/// Instantiate a [`ResolvedTree`], finding and instantiating the named model.
pub fn instantiate(
    resolved: ResolvedTree,
    model_name: &str,
) -> InstantiateResult<ast::InstancedTree> {
    instantiate_with_options(resolved, model_name, InstantiateOptions::default())
}

/// Instantiate a resolved tree with caller-supplied instantiation options.
pub fn instantiate_with_options(
    resolved: ResolvedTree,
    model_name: &str,
    options: InstantiateOptions,
) -> InstantiateResult<ast::InstancedTree> {
    let tree = resolved.into_inner();
    let overlay = instantiate_model_with_options(&tree, model_name, options)?;
    Ok(ast::InstancedTree::new(tree, overlay))
}

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

    // Create the instance overlay
    let mut overlay = ast::InstanceOverlay::new();

    // MLS §4.7: Track if the root model is partial (incomplete for standalone use).
    // Partial models may legally contain partial components.
    ctx.set_allow_partial_instantiation(model.partial);
    overlay.is_partial = model.partial;
    overlay.class_type = model.class_type.clone();
    overlay.root_description = description_tokens_to_string(&model.description);

    // Instantiate the root model
    if let Err(e) = instantiate_class(tree, model, None, None, &mut ctx, &mut overlay) {
        return InstantiationOutcome::Error(e);
    }

    // Check if there are missing inner declarations
    if ctx.has_missing_inners() {
        // MLS §5.4: Attempt to synthesize default inner declarations and retry.
        let missing = ctx.missing_inner_infos().to_vec();
        match retry_with_synthetic_inners(tree, model, &missing, options) {
            Ok(mut retry_overlay) => {
                retry_overlay.synthesized_inners = missing
                    .iter()
                    .map(|info| info.name.clone())
                    .collect::<std::collections::BTreeSet<_>>()
                    .into_iter()
                    .collect();
                successful_instantiation_outcome(tree, retry_overlay)
            }
            Err(SyntheticInnerError::StillMissing { names }) => {
                let span_by_name: std::collections::HashMap<_, _> = missing
                    .iter()
                    .map(|info| (info.name.as_str(), info.span))
                    .collect();
                let missing_spans = names
                    .iter()
                    .filter_map(|name| span_by_name.get(name.as_str()).copied())
                    .collect();
                InstantiationOutcome::NeedsInner {
                    missing_inners: names,
                    missing_spans,
                    partial_overlay: overlay,
                }
            }
            Err(SyntheticInnerError::InstantiationFailed) => {
                // Retry failed; fall back to original NeedsInner result.
                InstantiationOutcome::NeedsInner {
                    missing_inners: ctx.missing_inner_names(),
                    missing_spans: ctx.missing_inner_spans(),
                    partial_overlay: overlay,
                }
            }
            Err(SyntheticInnerError::SourceContext(error)) => InstantiationOutcome::Error(error),
        }
    } else {
        successful_instantiation_outcome(tree, overlay)
    }
}

fn successful_instantiation_outcome(
    tree: &ast::ClassTree,
    mut overlay: ast::InstanceOverlay,
) -> InstantiationOutcome {
    match resolve_post_materialization_component_targets(tree, &mut overlay) {
        Ok(()) => InstantiationOutcome::Success(overlay),
        Err(error) => InstantiationOutcome::Error(error),
    }
}

/// Instantiate a model, returning an error if instantiation fails.
///
/// Convenience wrapper that treats missing inner declarations as errors.
/// For more nuanced handling, use [`instantiate_model_with_outcome`].
///
/// # Arguments
///
/// * `tree` - Reference to the class tree
/// * `model_name` - Name of the model to instantiate as root
///
/// # Returns
///
/// An `ast::InstanceOverlay` with the instantiation results, or an error.
pub fn instantiate_model(
    tree: &ast::ClassTree,
    model_name: &str,
) -> InstantiateResult<ast::InstanceOverlay> {
    instantiate_model_with_options(tree, model_name, InstantiateOptions::default())
}

/// Instantiate a model with caller-supplied instantiation options.
pub fn instantiate_model_with_options(
    tree: &ast::ClassTree,
    model_name: &str,
    options: InstantiateOptions,
) -> InstantiateResult<ast::InstanceOverlay> {
    instantiate_model_with_outcome_options(tree, model_name, options).into_result()
}
