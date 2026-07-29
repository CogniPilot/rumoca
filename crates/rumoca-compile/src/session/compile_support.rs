use super::*;

pub(super) fn resolve_class_for_completion<'a>(
    tree: &'a ast::ClassTree,
    class_name: &str,
) -> Option<&'a ast::ClassDef> {
    if let Some(class) = tree.get_class_by_qualified_name(class_name) {
        return Some(class);
    }

    let mut matched_name: Option<&str> = None;
    for name in tree.name_map.keys() {
        if !class_name_matches_completion_target(name, class_name) {
            continue;
        }
        if matched_name.is_some() {
            return None;
        }
        matched_name = Some(name);
    }
    matched_name.and_then(|name| tree.get_class_by_qualified_name(name))
}

fn class_name_matches_completion_target(qualified_name: &str, class_name: &str) -> bool {
    qualified_name == class_name
        || rumoca_core::top_level_path_ends_with(qualified_name, class_name)
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

pub(super) fn is_simulatable_class_type(class_type: &rumoca_core::ClassType) -> bool {
    matches!(
        class_type,
        rumoca_core::ClassType::Model
            | rumoca_core::ClassType::Block
            | rumoca_core::ClassType::Class
    )
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

pub(super) fn flatten_options_for_tree() -> FlattenOptions {
    // Connection compatibility is model-local at flatten time (overlay-scoped),
    // so strict validation should always be enabled for compiled models even
    // when the source tree contains many external source-root classes.
    FlattenOptions {
        strict_connection_validation: true,
        simplify_variable_names: false,
        // Family-native lowering: regular state-derivative for-families materialize
        // only their corner cells; the interior cells are reconstructed downstream
        // from the corners (byte-identical output, far less compile-time memory).
        // Full materialization is retained only as a debug toggle.
        materialize_structured_families: false,
    }
}

fn dae_model_outcome_internal_with_options(
    tree: &ast::ClassTree,
    model_name: &str,
    instantiation_options: InstantiateOptions,
) -> DaeModelOutcome {
    notify_compile_phase(FailedPhase::Instantiate, CompilePhaseEvent::Started);
    let instantiate_start = maybe_start_timer();
    let instantiate_outcome = InstantiatedModelOutcome::from_instantiation_outcome(
        instantiate_model_with_outcome_options(tree, model_name, instantiation_options),
    );
    maybe_record_compile_phase_timing(FailedPhase::Instantiate, instantiate_start);
    notify_compile_phase(FailedPhase::Instantiate, CompilePhaseEvent::Completed);

    notify_compile_phase(FailedPhase::Typecheck, CompilePhaseEvent::Started);
    let typecheck_start = maybe_start_timer();
    let (typed_outcome, typechecked_built) =
        typed_model_outcome_from_instantiated(tree, model_name, instantiate_outcome);
    if typechecked_built {
        maybe_record_compile_phase_timing(FailedPhase::Typecheck, typecheck_start);
    }
    notify_compile_phase(FailedPhase::Typecheck, CompilePhaseEvent::Completed);

    notify_compile_phase(FailedPhase::Flatten, CompilePhaseEvent::Started);
    let flatten_start = maybe_start_timer();
    let (flat_outcome, flattened_built) =
        flat_model_outcome_from_typed(tree, model_name, typed_outcome);
    if flattened_built {
        maybe_record_compile_phase_timing(FailedPhase::Flatten, flatten_start);
    }
    notify_compile_phase(FailedPhase::Flatten, CompilePhaseEvent::Completed);

    notify_compile_phase(FailedPhase::ToDae, CompilePhaseEvent::Started);
    let todae_start = maybe_start_timer();
    let (dae_outcome, todae_built) = dae_model_outcome_from_flat(tree, flat_outcome);
    if todae_built {
        maybe_record_compile_phase_timing(FailedPhase::ToDae, todae_start);
    }
    notify_compile_phase(FailedPhase::ToDae, CompilePhaseEvent::Completed);

    dae_outcome
}

fn dae_model_outcome_internal(tree: &ast::ClassTree, model_name: &str) -> DaeModelOutcome {
    dae_model_outcome_internal_with_options(tree, model_name, InstantiateOptions::default())
}

/// Internal function for parallel compilation.
///
/// Uses the phase order: Instantiate -> Typecheck -> Flatten -> ToDae
/// Type checking runs after instantiation so it has full access to the
/// modification context for dimension evaluation (MLS §10.1).
pub(super) fn compile_model_internal(tree: &ast::ClassTree, model_name: &str) -> PhaseResult {
    let dae_outcome = dae_model_outcome_internal(tree, model_name);
    compile_phase_result_from_dae(tree, model_name, dae_outcome)
}

pub(super) fn compile_model_internal_with_options(
    tree: &ast::ClassTree,
    model_name: &str,
    instantiation_options: InstantiateOptions,
) -> PhaseResult {
    let dae_outcome =
        dae_model_outcome_internal_with_options(tree, model_name, instantiation_options);
    compile_phase_result_from_dae(tree, model_name, dae_outcome)
}

pub(super) fn compile_model_dae_internal(
    tree: &ast::ClassTree,
    model_name: &str,
) -> DaePhaseResult {
    let dae_outcome = dae_model_outcome_internal(tree, model_name);
    dae_phase_result_from_dae(tree, model_name, dae_outcome)
}

pub(super) fn compile_model_dae_internal_with_options(
    tree: &ast::ClassTree,
    model_name: &str,
    instantiation_options: InstantiateOptions,
) -> DaePhaseResult {
    let dae_outcome =
        dae_model_outcome_internal_with_options(tree, model_name, instantiation_options);
    dae_phase_result_from_dae(tree, model_name, dae_outcome)
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

    // MLS §5.6 / SPEC_0007: ToDae stays downstream of flatten and should
    // consume the cached flat artifact rather than rebuilding earlier phases.
    match to_dae(&artifact.flat, tree.source_map.clone()) {
        Ok(dae) => match rumoca_phase_dae::balance_detail(&artifact.flat) {
            Ok(balance_detail) => (
                DaeModelOutcome::Success(Box::new(DaeModelArtifactData {
                    flat: Arc::new(artifact.flat),
                    dae: Arc::new(dae),
                    balance_detail,
                })),
                true,
            ),
            Err(error) => (
                DaeModelOutcome::ToDaeError {
                    error: Box::new(error),
                },
                true,
            ),
        },
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

fn active_discrete_scalar_count(dae_model: &dae::Dae) -> i64 {
    dae_model.inspect(|view| {
        let active = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|expression| view.expression(expression))
            .filter_map(dae::ExpressionView::variable_coordinate)
            .collect::<HashSet<_>>();

        view.variables()
            .filter(|(id, variable)| {
                active.contains(id)
                    && matches!(
                        variable.role(),
                        dae::VariableRole::DiscreteReal | dae::VariableRole::DiscreteValue
                    )
            })
            .map(|(_, variable)| variable.scalar_count())
            .sum::<usize>() as i64
    })
}

fn dae_compilation_result_from_artifact(
    artifact: DaeModelArtifactData,
    experiment_settings: ExperimentSettings,
    source_map: SourceMap,
) -> Result<DaeCompilationResult, ToDaeError> {
    let has_unbound_fixed_parameters = artifact.flat.has_unbound_fixed_parameters();
    let active_discrete_scalar_count = active_discrete_scalar_count(&artifact.dae);

    Ok(DaeCompilationResult {
        flat: artifact.flat,
        dae: artifact.dae,
        source_map: Some(source_map),
        has_unbound_fixed_parameters,
        active_discrete_scalar_count,
        balance_detail: artifact.balance_detail,
        experiment_start_time: experiment_settings.start_time,
        experiment_stop_time: experiment_settings.stop_time,
        experiment_tolerance: experiment_settings.tolerance,
        experiment_interval: experiment_settings.interval,
        rumoca_solver_fixed_step: experiment_settings.solver_fixed_step,
        experiment_solver: experiment_settings.solver,
    })
}

fn phase_error_payload(
    error: &(impl PhaseError + std::fmt::Display),
) -> (String, Option<String>, Vec<CommonDiagnostic>) {
    let diagnostic = error.to_diagnostic();
    let code = diagnostic.code.clone();
    (error.to_string(), code, vec![diagnostic])
}

pub(super) fn dae_phase_result_from_dae(
    tree: &ast::ClassTree,
    model_name: &str,
    dae_outcome: DaeModelOutcome,
) -> DaePhaseResult {
    let experiment_settings = experiment_settings_for_model(tree, model_name);

    match dae_outcome {
        DaeModelOutcome::Success(artifact) => match dae_compilation_result_from_artifact(
            *artifact,
            experiment_settings,
            tree.source_map.clone(),
        ) {
            Ok(result) => DaePhaseResult::Success(Box::new(result)),
            Err(error) => {
                let (error, error_code, diagnostics) = phase_error_payload(&error);
                DaePhaseResult::Failed {
                    phase: FailedPhase::ToDae,
                    error,
                    error_code,
                    diagnostics,
                    balance_detail: None,
                }
            }
        },
        DaeModelOutcome::NeedsInner {
            missing_inners,
            missing_spans,
            ..
        } => DaePhaseResult::NeedsInner {
            missing_inners,
            missing_spans,
        },
        DaeModelOutcome::InstantiateError(error) => {
            let (error, error_code, diagnostics) = phase_error_payload(&*error);
            DaePhaseResult::Failed {
                phase: FailedPhase::Instantiate,
                error,
                error_code,
                diagnostics,
                balance_detail: None,
            }
        }
        DaeModelOutcome::TypecheckError(diags) => {
            let diagnostics = diagnostics_from_vec(diags.clone());
            DaePhaseResult::Failed {
                phase: FailedPhase::Typecheck,
                error: diagnostics
                    .iter()
                    .map(|diag| diagnostic_message_with_primary_location(diag, &tree.source_map))
                    .collect::<Vec<_>>()
                    .join("; "),
                error_code: summarize_typecheck_error_code(&diagnostics),
                diagnostics: diags,
                balance_detail: None,
            }
        }
        DaeModelOutcome::FlattenError { error, .. } => {
            let (error, error_code, diagnostics) = phase_error_payload(&*error);
            DaePhaseResult::Failed {
                phase: FailedPhase::Flatten,
                error,
                error_code,
                diagnostics,
                balance_detail: None,
            }
        }
        DaeModelOutcome::ToDaeError { error, .. } => {
            // ED001 carries the full component breakdown; keep it structured so
            // downstream triage never has to re-parse the message text.
            let balance_detail = error
                .balance_detail()
                .map(|detail| Box::new(detail.clone()));
            let (error, error_code, diagnostics) = phase_error_payload(&*error);
            DaePhaseResult::Failed {
                phase: FailedPhase::ToDae,
                error,
                error_code,
                diagnostics,
                balance_detail,
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
        DaeModelOutcome::NeedsInner {
            missing_inners,
            missing_spans,
            ..
        } => {
            return PhaseResult::NeedsInner {
                missing_inners,
                missing_spans,
            };
        }
        DaeModelOutcome::InstantiateError(error) => {
            let (error, error_code, diagnostics) = phase_error_payload(&*error);
            return PhaseResult::Failed {
                phase: FailedPhase::Instantiate,
                error,
                error_code,
                diagnostics,
            };
        }
        DaeModelOutcome::TypecheckError(diags) => {
            let diagnostics = diagnostics_from_vec(diags.clone());
            return PhaseResult::Failed {
                phase: FailedPhase::Typecheck,
                error: diagnostics
                    .iter()
                    .map(|diagnostic| {
                        diagnostic_message_with_primary_location(diagnostic, &tree.source_map)
                    })
                    .collect::<Vec<_>>()
                    .join("; "),
                error_code: summarize_typecheck_error_code(&diagnostics),
                diagnostics: diags,
            };
        }
        DaeModelOutcome::FlattenError { error, .. } => {
            let (error, error_code, diagnostics) = phase_error_payload(&*error);
            return PhaseResult::Failed {
                phase: FailedPhase::Flatten,
                error,
                error_code,
                diagnostics,
            };
        }
        DaeModelOutcome::ToDaeError { error, .. } => {
            let (error, error_code, diagnostics) = phase_error_payload(&*error);
            return PhaseResult::Failed {
                phase: FailedPhase::ToDae,
                error,
                error_code,
                diagnostics,
            };
        }
    };

    PhaseResult::Success(Box::new(CompilationResult {
        flat: unwrap_or_clone_arc(artifact.flat),
        dae: unwrap_or_clone_arc(artifact.dae),
        balance_detail: artifact.balance_detail,
        experiment_start_time: experiment_settings.start_time,
        experiment_stop_time: experiment_settings.stop_time,
        experiment_tolerance: experiment_settings.tolerance,
        experiment_interval: experiment_settings.interval,
        rumoca_solver_fixed_step: experiment_settings.solver_fixed_step,
        experiment_solver: experiment_settings.solver,
    }))
}

pub(super) fn finalize_strict_compile_report(
    tree: &ast::ClassTree,
    requested_model: &str,
    mut failures: Vec<ModelFailureDiagnostic>,
    results: Vec<(String, PhaseResult)>,
) -> StrictCompileReport {
    finalize_strict_compile_report_from_results(tree, requested_model, &mut failures, results)
}

pub(super) fn finalize_strict_compile_report_from_results<I>(
    tree: &ast::ClassTree,
    requested_model: &str,
    failures: &mut Vec<ModelFailureDiagnostic>,
    results: I,
) -> StrictCompileReport
where
    I: IntoIterator<Item = (String, PhaseResult)>,
{
    let mut summary = CompilationSummary::default();
    let mut requested_result = None;

    for (name, result) in results {
        summary.add_result(&result);
        failures.extend(phase_result_to_failures(tree, &name, &result));
        if name == requested_model {
            requested_result = Some(result);
        }
    }

    StrictCompileReport {
        requested_model: requested_model.to_string(),
        requested_result,
        summary,
        failures: std::mem::take(failures),
        source_map: Some(tree.source_map.clone()),
    }
}

pub(super) fn finalize_strict_compile_report_from_uncached_targets(
    tree: &ast::ClassTree,
    requested_model: &str,
    failures: Vec<ModelFailureDiagnostic>,
    targets: &[String],
    instantiation_options: InstantiateOptions,
) -> StrictCompileReport {
    finalize_strict_compile_report_from_uncached_targets_impl(
        tree,
        requested_model,
        failures,
        targets,
        instantiation_options,
    )
}

#[cfg(target_arch = "wasm32")]
fn finalize_strict_compile_report_from_uncached_targets_impl(
    tree: &ast::ClassTree,
    requested_model: &str,
    failures: Vec<ModelFailureDiagnostic>,
    targets: &[String],
    instantiation_options: InstantiateOptions,
) -> StrictCompileReport {
    let mut failures = failures;
    let results = targets.iter().map(|name| {
        (
            name.clone(),
            compile_model_internal_with_options(tree, name, instantiation_options.clone()),
        )
    });
    finalize_strict_compile_report_from_results(tree, requested_model, &mut failures, results)
}

#[cfg(not(target_arch = "wasm32"))]
fn finalize_strict_compile_report_from_uncached_targets_impl(
    tree: &ast::ClassTree,
    requested_model: &str,
    failures: Vec<ModelFailureDiagnostic>,
    targets: &[String],
    instantiation_options: InstantiateOptions,
) -> StrictCompileReport {
    let (result_tx, result_rx) = std::sync::mpsc::sync_channel::<(String, PhaseResult)>(1);

    std::thread::scope(|scope| {
        let consumer = scope.spawn(move || {
            let mut failures = failures;
            finalize_strict_compile_report_from_results(
                tree,
                requested_model,
                &mut failures,
                result_rx,
            )
        });

        targets.par_iter().for_each_with(result_tx, |tx, name| {
            let result =
                compile_model_internal_with_options(tree, name, instantiation_options.clone());
            let _ = tx.send((name.clone(), result));
        });

        consumer
            .join()
            .expect("strict compile result consumer panicked")
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn completion_class_match_requires_top_level_segment_boundary() {
        assert!(class_name_matches_completion_target("Pkg.Target", "Target"));
        assert!(class_name_matches_completion_target(
            "Root.Pkg.Target",
            "Pkg.Target"
        ));
        assert!(!class_name_matches_completion_target(
            "Pkg.MyTarget",
            "Target"
        ));
        assert!(!class_name_matches_completion_target(
            "Pkg.TargetAlias",
            "Target"
        ));
        assert!(!class_name_matches_completion_target(
            "Pkg[index.Target]",
            "Target"
        ));
    }
}
