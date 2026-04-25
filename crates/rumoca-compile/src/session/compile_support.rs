use super::*;

pub(super) fn resolve_class_for_completion<'a>(
    tree: &'a ast::ClassTree,
    class_name: &str,
) -> Option<&'a ast::ClassDef> {
    if let Some(class) = tree.get_class_by_qualified_name(class_name) {
        return Some(class);
    }

    let suffix = format!(".{class_name}");
    let mut matched_name: Option<&str> = None;
    for name in tree.name_map.keys() {
        if !(name == class_name || name.ends_with(&suffix)) {
            continue;
        }
        if matched_name.is_some() {
            return None;
        }
        matched_name = Some(name);
    }
    matched_name.and_then(|name| tree.get_class_by_qualified_name(name))
}

pub(super) fn collect_class_component_members(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    members: &mut IndexMap<String, String>,
    visiting: &mut std::collections::HashSet<DefId>,
) {
    if let Some(def_id) = class.def_id
        && !visiting.insert(def_id)
    {
        return;
    }

    for ext in &class.extends {
        let Some(base_def_id) = ext.base_def_id else {
            continue;
        };
        let Some(base_class) = tree.get_class_by_def_id(base_def_id) else {
            continue;
        };
        collect_class_component_members(tree, base_class, members, visiting);
        for break_name in &ext.break_names {
            members.shift_remove(break_name);
        }
    }

    for (name, component) in &class.components {
        members.insert(name.clone(), component.type_name.to_string());
    }

    if let Some(def_id) = class.def_id {
        visiting.remove(&def_id);
    }
}

pub(super) fn missing_inner_label(idx: usize, span: Span) -> Label {
    let label = match idx {
        0 => Label::primary(span),
        _ => Label::secondary(span),
    };
    label.with_message("missing matching `inner`")
}

pub(super) fn diagnostics_to_anyhow(diags: &CommonDiagnostics) -> anyhow::Error {
    let message = diags
        .iter()
        .map(|d| d.message.clone())
        .collect::<Vec<_>>()
        .join("; ");
    if message.is_empty() {
        anyhow::anyhow!("Resolve errors")
    } else {
        anyhow::anyhow!("Resolve errors: {message}")
    }
}

pub(super) fn diagnostics_from_vec(diags: Vec<CommonDiagnostic>) -> CommonDiagnostics {
    let mut out = CommonDiagnostics::new();
    for diag in diags {
        out.emit(diag);
    }
    out
}

pub(super) fn split_cached_target_results(
    cache: &IndexMap<String, PhaseResult>,
    targets: &[String],
) -> (IndexMap<String, PhaseResult>, Vec<String>) {
    let mut results = IndexMap::new();
    let mut missing = Vec::new();
    for target in targets {
        match cache.get(target).cloned() {
            Some(result) => {
                results.insert(target.clone(), result);
            }
            None => missing.push(target.clone()),
        }
    }
    (results, missing)
}

pub(super) fn is_simulatable_class_type(class_type: &ast::ClassType) -> bool {
    matches!(
        class_type,
        ast::ClassType::Model | ast::ClassType::Block | ast::ClassType::Class
    )
}

fn has_multiple_top_level_classes(tree: &ast::ClassTree) -> bool {
    tree.definitions.classes.len() > 1
}

fn summarize_typecheck_error_code(diags: &CommonDiagnostics) -> Option<String> {
    let mut codes = diags.iter().filter_map(|d| d.code.as_deref());
    let Some(first) = codes.next() else {
        return Some("ET000".to_string());
    };
    if codes.all(|code| code == first) {
        Some(first.to_string())
    } else {
        Some("ET000".to_string())
    }
}

pub(super) fn todae_options_for_tree(tree: &ast::ClassTree) -> ToDaeOptions {
    ToDaeOptions {
        error_on_unbalanced: !has_multiple_top_level_classes(tree),
    }
}

pub(super) fn flatten_options_for_tree() -> FlattenOptions {
    // Connection compatibility is model-local at flatten time (overlay-scoped),
    // so strict validation should always be enabled for compiled models even
    // when the source tree contains many external source-root classes.
    FlattenOptions {
        strict_connection_validation: true,
    }
}

/// Internal function for parallel compilation.
///
/// Uses the phase order: Instantiate -> Typecheck -> Flatten -> ToDae
/// Type checking runs after instantiation so it has full access to the
/// modification context for dimension evaluation (MLS §10.1).
pub(super) fn compile_model_internal(tree: &ast::ClassTree, model_name: &str) -> PhaseResult {
    let instantiate_start = maybe_start_timer();
    let instantiate_outcome = InstantiatedModelOutcome::from_instantiation_outcome(
        instantiate_model_with_outcome(tree, model_name),
    );
    maybe_record_compile_phase_timing(FailedPhase::Instantiate, instantiate_start);

    let typecheck_start = maybe_start_timer();
    let (typed_outcome, typechecked_built) =
        typed_model_outcome_from_instantiated(tree, model_name, instantiate_outcome);
    if typechecked_built {
        maybe_record_compile_phase_timing(FailedPhase::Typecheck, typecheck_start);
    }

    let flatten_start = maybe_start_timer();
    let (flat_outcome, flattened_built) =
        flat_model_outcome_from_typed(tree, model_name, typed_outcome);
    if flattened_built {
        maybe_record_compile_phase_timing(FailedPhase::Flatten, flatten_start);
    }

    let todae_start = maybe_start_timer();
    let (dae_outcome, todae_built) = dae_model_outcome_from_flat(tree, flat_outcome);
    if todae_built {
        maybe_record_compile_phase_timing(FailedPhase::ToDae, todae_start);
    }

    compile_phase_result_from_dae(tree, model_name, dae_outcome)
}

pub(super) fn typed_model_outcome_from_instantiated(
    tree: &ast::ClassTree,
    model_name: &str,
    instantiate_outcome: InstantiatedModelOutcome,
) -> (TypedModelOutcome, bool) {
    let mut overlay = match instantiate_outcome {
        InstantiatedModelOutcome::Success(overlay) => *overlay,
        InstantiatedModelOutcome::NeedsInner {
            missing_inners,
            missing_spans,
        } => {
            return (
                TypedModelOutcome::NeedsInner {
                    missing_inners,
                    missing_spans,
                },
                false,
            );
        }
        InstantiatedModelOutcome::Error(error) => {
            return (TypedModelOutcome::InstantiateError(error), false);
        }
    };

    if let Err(diags) = typecheck_instanced(tree, &mut overlay, model_name) {
        return (
            TypedModelOutcome::TypecheckError(diags.iter().cloned().collect()),
            true,
        );
    }

    (TypedModelOutcome::Success(Box::new(overlay)), true)
}

pub(super) fn flat_model_outcome_from_typed(
    tree: &ast::ClassTree,
    model_name: &str,
    typed_outcome: TypedModelOutcome,
) -> (FlatModelOutcome, bool) {
    let overlay = match typed_outcome {
        TypedModelOutcome::Success(overlay) => *overlay,
        TypedModelOutcome::NeedsInner {
            missing_inners,
            missing_spans,
        } => {
            return (
                FlatModelOutcome::NeedsInner {
                    missing_inners,
                    missing_spans,
                },
                false,
            );
        }
        TypedModelOutcome::InstantiateError(error) => {
            return (FlatModelOutcome::InstantiateError(error), false);
        }
        TypedModelOutcome::TypecheckError(diags) => {
            return (FlatModelOutcome::TypecheckError(diags), false);
        }
    };

    match flatten_ref_with_options(tree, &overlay, model_name, flatten_options_for_tree()) {
        Ok(flat) => (
            FlatModelOutcome::Success(Box::new(FlatModelArtifactData { flat })),
            true,
        ),
        Err(error) => (
            FlatModelOutcome::FlattenError {
                error: Box::new(error),
            },
            true,
        ),
    }
}

pub(super) fn dae_model_outcome_from_flat(
    tree: &ast::ClassTree,
    flat_outcome: FlatModelOutcome,
) -> (DaeModelOutcome, bool) {
    let artifact = match flat_outcome {
        FlatModelOutcome::Success(artifact) => *artifact,
        FlatModelOutcome::NeedsInner {
            missing_inners,
            missing_spans,
        } => {
            return (
                DaeModelOutcome::NeedsInner {
                    missing_inners,
                    missing_spans,
                },
                false,
            );
        }
        FlatModelOutcome::InstantiateError(error) => {
            return (DaeModelOutcome::InstantiateError(error), false);
        }
        FlatModelOutcome::TypecheckError(diags) => {
            return (DaeModelOutcome::TypecheckError(diags), false);
        }
        FlatModelOutcome::FlattenError { error } => {
            return (DaeModelOutcome::FlattenError { error }, false);
        }
    };

    // MLS §5.6 / SPEC_0004: ToDae stays downstream of flatten and should
    // consume the cached flat artifact rather than rebuilding earlier phases.
    match to_dae_with_options(&artifact.flat, todae_options_for_tree(tree)) {
        Ok(dae) => (
            DaeModelOutcome::Success(Box::new(DaeModelArtifactData {
                flat: Arc::new(artifact.flat),
                dae: Arc::new(dae),
            })),
            true,
        ),
        Err(error) => (
            DaeModelOutcome::ToDaeError {
                error: Box::new(error),
            },
            true,
        ),
    }
}

fn unwrap_or_clone_arc<T: Clone>(value: Arc<T>) -> T {
    Arc::unwrap_or_clone(value)
}

pub(super) fn dae_phase_result_from_dae(
    tree: &ast::ClassTree,
    model_name: &str,
    dae_outcome: DaeModelOutcome,
) -> DaePhaseResult {
    let experiment_settings = experiment_settings_for_model(tree, model_name);

    match dae_outcome {
        DaeModelOutcome::Success(artifact) => {
            DaePhaseResult::Success(Box::new(DaeCompilationResult {
                dae: artifact.dae,
                experiment_start_time: experiment_settings.start_time,
                experiment_stop_time: experiment_settings.stop_time,
                experiment_tolerance: experiment_settings.tolerance,
                experiment_interval: experiment_settings.interval,
                experiment_solver: experiment_settings.solver,
            }))
        }
        DaeModelOutcome::NeedsInner { missing_inners, .. } => {
            DaePhaseResult::NeedsInner { missing_inners }
        }
        DaeModelOutcome::InstantiateError(error) => {
            use miette::Diagnostic;
            DaePhaseResult::Failed {
                phase: FailedPhase::Instantiate,
                error: format!("{error}"),
                error_code: error.code().map(|code| code.to_string()),
            }
        }
        DaeModelOutcome::TypecheckError(diags) => {
            let diagnostics = diagnostics_from_vec(diags);
            DaePhaseResult::Failed {
                phase: FailedPhase::Typecheck,
                error: diagnostics
                    .iter()
                    .map(|diag| diag.message.clone())
                    .collect::<Vec<_>>()
                    .join("; "),
                error_code: summarize_typecheck_error_code(&diagnostics),
            }
        }
        DaeModelOutcome::FlattenError { error, .. } => {
            use miette::Diagnostic;
            DaePhaseResult::Failed {
                phase: FailedPhase::Flatten,
                error: format!("{error}"),
                error_code: error.code().map(|code| code.to_string()),
            }
        }
        DaeModelOutcome::ToDaeError { error, .. } => {
            use miette::Diagnostic;
            DaePhaseResult::Failed {
                phase: FailedPhase::ToDae,
                error: format!("{error}"),
                error_code: error.code().map(|code| code.to_string()),
            }
        }
    }
}

pub(super) fn compile_phase_result_from_dae(
    tree: &ast::ClassTree,
    model_name: &str,
    dae_outcome: DaeModelOutcome,
) -> PhaseResult {
    let experiment_settings = experiment_settings_for_model(tree, model_name);

    let artifact = match dae_outcome {
        DaeModelOutcome::Success(artifact) => *artifact,
        DaeModelOutcome::NeedsInner { missing_inners, .. } => {
            return PhaseResult::NeedsInner { missing_inners };
        }
        DaeModelOutcome::InstantiateError(error) => {
            use miette::Diagnostic;
            let error_code = error.code().map(|c| c.to_string());
            return PhaseResult::Failed {
                phase: FailedPhase::Instantiate,
                error: format!("{}", error),
                error_code,
            };
        }
        DaeModelOutcome::TypecheckError(diags) => {
            let diagnostics = diagnostics_from_vec(diags);
            return PhaseResult::Failed {
                phase: FailedPhase::Typecheck,
                error: diagnostics
                    .iter()
                    .map(|d| d.message.clone())
                    .collect::<Vec<_>>()
                    .join("; "),
                error_code: summarize_typecheck_error_code(&diagnostics),
            };
        }
        DaeModelOutcome::FlattenError { error, .. } => {
            use miette::Diagnostic;
            let error_code = error.code().map(|c| c.to_string());
            return PhaseResult::Failed {
                phase: FailedPhase::Flatten,
                error: format!("{}", error),
                error_code,
            };
        }
        DaeModelOutcome::ToDaeError { error, .. } => {
            use miette::Diagnostic;
            let error_code = error.code().map(|c| c.to_string());
            return PhaseResult::Failed {
                phase: FailedPhase::ToDae,
                error: format!("{}", error),
                error_code,
            };
        }
    };

    PhaseResult::Success(Box::new(CompilationResult {
        flat: unwrap_or_clone_arc(artifact.flat),
        dae: unwrap_or_clone_arc(artifact.dae),
        experiment_start_time: experiment_settings.start_time,
        experiment_stop_time: experiment_settings.stop_time,
        experiment_tolerance: experiment_settings.tolerance,
        experiment_interval: experiment_settings.interval,
        experiment_solver: experiment_settings.solver,
    }))
}

pub(super) fn finalize_strict_compile_report(
    tree: &ast::ClassTree,
    requested_model: &str,
    target_has_resolve_failures: bool,
    mut failures: Vec<ModelFailureDiagnostic>,
    results: Vec<(String, PhaseResult)>,
) -> StrictCompileReport {
    let summary = CompilationSummary::from_results(&results);
    let mut requested_result = None;

    for (name, result) in results {
        if let Some(failure) = phase_result_to_failure(tree, &name, &result) {
            failures.push(failure);
        }
        if name == requested_model && !target_has_resolve_failures {
            requested_result = Some(result);
        }
    }

    StrictCompileReport {
        requested_model: requested_model.to_string(),
        requested_result,
        summary,
        failures,
        source_map: Some(tree.source_map.clone()),
    }
}
