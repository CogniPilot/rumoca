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

fn is_library_tree(tree: &ast::ClassTree) -> bool {
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
        error_on_unbalanced: !is_library_tree(tree),
    }
}

pub(super) fn flatten_options_for_tree() -> FlattenOptions {
    // Connection compatibility is model-local at flatten time (overlay-scoped),
    // so strict validation should always be enabled for compiled models even
    // when the source tree contains many library classes.
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
    // Prevent thread-local `pre()` state from leaking across model compiles
    // when worker threads are reused (e.g., MSL parallel compile batches).
    rumoca_sim::clear_runtime_pre_values();

    let experiment_settings = experiment_settings_for_model(tree, model_name);

    let instantiate_start = maybe_start_timer();
    let instantiate_outcome = instantiate_model_with_outcome(tree, model_name);
    maybe_record_compile_phase_timing(FailedPhase::Instantiate, instantiate_start);
    let mut overlay = match instantiate_outcome {
        InstantiationOutcome::Success(o) => o,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            return PhaseResult::NeedsInner { missing_inners };
        }
        InstantiationOutcome::Error(e) => {
            use miette::Diagnostic;
            let error_code = e.code().map(|c| c.to_string());
            return PhaseResult::Failed {
                phase: FailedPhase::Instantiate,
                error: format!("{}", e),
                error_code,
            };
        }
    };

    let typecheck_start = maybe_start_timer();
    let typecheck_result = typecheck_instanced(tree, &mut overlay, model_name);
    maybe_record_compile_phase_timing(FailedPhase::Typecheck, typecheck_start);
    if let Err(diags) = typecheck_result {
        return PhaseResult::Failed {
            phase: FailedPhase::Typecheck,
            error: diags
                .iter()
                .map(|d| d.message.clone())
                .collect::<Vec<_>>()
                .join("; "),
            error_code: summarize_typecheck_error_code(&diags),
        };
    }

    let flatten_start = maybe_start_timer();
    let flat_result =
        flatten_ref_with_options(tree, &overlay, model_name, flatten_options_for_tree());
    maybe_record_compile_phase_timing(FailedPhase::Flatten, flatten_start);
    let flat = match flat_result {
        Ok(f) => f,
        Err(e) => {
            use miette::Diagnostic;
            let error_code = e.code().map(|c| c.to_string());
            return PhaseResult::Failed {
                phase: FailedPhase::Flatten,
                error: format!("{}", e),
                error_code,
            };
        }
    };

    let todae_start = maybe_start_timer();
    let dae_result = to_dae_with_options(&flat, todae_options_for_tree(tree));
    maybe_record_compile_phase_timing(FailedPhase::ToDae, todae_start);
    let dae = match dae_result {
        Ok(d) => d,
        Err(e) => {
            use miette::Diagnostic;
            let error_code = e.code().map(|c| c.to_string());
            return PhaseResult::Failed {
                phase: FailedPhase::ToDae,
                error: format!("{}", e),
                error_code,
            };
        }
    };

    PhaseResult::Success(Box::new(CompilationResult {
        flat,
        dae,
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
