use super::*;
use crate::session::compile_support::finalize_strict_compile_report_from_uncached_targets;

/// Pre-compiled source root for efficient multi-model compilation.
///
/// This is a convenience wrapper around [`Session`] that initializes from
/// a [`ast::StoredDefinition`]. Use this when you've already parsed your source root
/// (e.g., from parallel file parsing) and want to compile multiple models.
///
/// For new code, consider using [`Session`] directly with [`Session::add_parsed_batch`].
pub struct CompiledSourceRoot {
    resolved: Arc<ast::ResolvedTree>,
    model_names: Vec<String>,
    class_type_counts: std::collections::HashMap<String, usize>,
    class_dependencies: IndexMap<String, IndexSet<String>>,
    resolve_diagnostics: CommonDiagnostics,
    pub(super) compile_cache: Mutex<IndexMap<String, PhaseResult>>,
}

impl CompiledSourceRoot {
    fn from_indexed_state(
        resolved: Arc<ast::ResolvedTree>,
        model_names: Vec<String>,
        class_type_counts: std::collections::HashMap<String, usize>,
        resolve_diagnostics: CommonDiagnostics,
    ) -> Self {
        let dependency_fingerprints = DependencyFingerprintCache::from_tree(&resolved.0);
        Self {
            resolved,
            model_names,
            class_type_counts,
            class_dependencies: dependency_fingerprints.class_dependencies().clone(),
            resolve_diagnostics,
            compile_cache: Mutex::new(IndexMap::new()),
        }
    }

    /// Create a compiled source root from a ast::StoredDefinition.
    ///
    /// This resolves the AST once. Type checking happens after instantiation.
    pub fn from_stored_definition(def: ast::StoredDefinition) -> Result<Self> {
        let mut session = Session::new(SessionConfig::default());
        session.add_parsed("source_root", def);
        session.build_resolved()?;
        let resolved = session.ensure_resolved()?.clone();
        Ok(Self::from_indexed_state(
            resolved.clone(),
            session.query_state.resolved.model_names.clone(),
            collect_class_type_counts(&resolved.0.definitions),
            CommonDiagnostics::new(),
        ))
    }

    /// Create a compiled source root from a parsed batch, indexing it tolerantly.
    ///
    /// This preserves whole-source-root resolve diagnostics for later strict
    /// target-closure compilation without requiring the entire source root to
    /// resolve cleanly up front.
    pub fn from_parsed_batch_tolerant(
        documents: Vec<(String, ast::StoredDefinition)>,
    ) -> Result<Self> {
        let mut session = Session::new(SessionConfig::default());
        session.add_parsed_batch(documents);
        let (resolved, resolve_diagnostics) = session
            .build_resolved_for_strict_compile_with_diagnostics()
            .map_err(|diags| diagnostics_to_anyhow(&diags))?;
        Ok(Self::from_indexed_state(
            resolved.clone(),
            session.query_state.resolved.model_names.clone(),
            collect_class_type_counts(&resolved.0.definitions),
            resolve_diagnostics,
        ))
    }

    /// Create a compiled source root from an already-resolved tree.
    ///
    /// This avoids re-running resolve and is intended for callers that already
    /// hold a validated resolved tree (e.g., MSL regression harness).
    pub fn from_resolved_tree(resolved: ast::ResolvedTree, model_names: Vec<String>) -> Self {
        let resolved = Arc::new(resolved);
        Self::from_indexed_state(
            resolved.clone(),
            model_names,
            collect_class_type_counts(&resolved.0.definitions),
            CommonDiagnostics::new(),
        )
    }

    /// Get all model names in the source root.
    ///
    /// This is infallible after construction since build_resolved was called.
    pub fn model_names(&self) -> &[String] {
        &self.model_names
    }

    /// Count all class types in the source root.
    pub fn class_type_counts(&self) -> &std::collections::HashMap<String, usize> {
        &self.class_type_counts
    }

    /// Get the class tree.
    ///
    /// This is infallible after construction since build_resolved was called.
    pub fn tree(&self) -> &ast::ClassTree {
        &self.resolved_tree().0
    }

    /// Count classes reachable from a strict target model.
    pub fn reachable_class_count(&self, model_name: &str) -> usize {
        self.reachable_model_closure(model_name)
            .reachable_classes
            .len()
    }

    /// Get the resolved tree reference (guaranteed present after construction).
    fn resolved_tree(&self) -> &Arc<ast::ResolvedTree> {
        &self.resolved
    }

    fn cached_phase_result(&self, model_name: &str) -> PhaseResult {
        if let Some(result) = self
            .compile_cache
            .lock()
            .expect("compiled source-root cache poisoned")
            .get(model_name)
            .cloned()
        {
            return result;
        }

        let result = compile_model_internal(&self.resolved_tree().0, model_name);
        self.compile_cache
            .lock()
            .expect("compiled source-root cache poisoned")
            .entry(model_name.to_string())
            .or_insert_with(|| result.clone());
        result
    }

    fn reachable_model_closure(&self, model_name: &str) -> ReachableModelClosure {
        ReachabilityPlanner::new(&self.class_dependencies, &self.model_names)
            .model_closure(model_name)
    }

    fn compile_targets_with_cache(&self, targets: &[String]) -> Vec<(String, PhaseResult)> {
        let (mut results, missing) = {
            let cache = self
                .compile_cache
                .lock()
                .expect("compiled source-root cache poisoned");
            split_cached_target_results(&cache, targets)
        };

        if !missing.is_empty() {
            let tree = &self.resolved_tree().0;
            let compiled_misses: Vec<_> = missing
                .par_iter()
                .map(|name| (name.clone(), compile_model_internal(tree, name)))
                .collect();

            let mut cache = self
                .compile_cache
                .lock()
                .expect("compiled source-root cache poisoned");
            for (name, result) in compiled_misses {
                cache.entry(name.clone()).or_insert_with(|| result.clone());
                results.insert(name, result);
            }
        }

        targets
            .iter()
            .filter_map(|target| {
                results
                    .shift_remove(target)
                    .map(|result| (target.clone(), result))
            })
            .collect()
    }

    fn compile_targets_streaming_with_cache<F>(
        &self,
        targets: &[String],
        max_in_flight_results: usize,
        consume: F,
    ) where
        F: FnMut(String, PhaseResult) + Send,
    {
        let (mut cached_results, missing) = {
            let cache = self
                .compile_cache
                .lock()
                .expect("compiled source-root cache poisoned");
            split_cached_target_results(&cache, targets)
        };

        let mut consume = consume;
        for target in targets {
            if let Some(result) = cached_results.shift_remove(target) {
                consume(target.clone(), result);
            }
        }
        if missing.is_empty() {
            return;
        }

        self.compile_missing_targets_streaming_with_cache(missing, max_in_flight_results, consume);
    }

    #[cfg(target_arch = "wasm32")]
    fn compile_missing_targets_streaming_with_cache<F>(
        &self,
        missing: Vec<String>,
        _max_in_flight_results: usize,
        mut consume: F,
    ) where
        F: FnMut(String, PhaseResult) + Send,
    {
        let tree = &self.resolved_tree().0;
        for name in missing {
            let result = compile_model_internal(tree, &name);
            self.compile_cache
                .lock()
                .expect("compiled source-root cache poisoned")
                .entry(name.clone())
                .or_insert_with(|| result.clone());
            consume(name, result);
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn compile_missing_targets_streaming_with_cache<F>(
        &self,
        missing: Vec<String>,
        max_in_flight_results: usize,
        consume: F,
    ) where
        F: FnMut(String, PhaseResult) + Send,
    {
        let queue_bound = max_in_flight_results.max(1);
        let (result_tx, result_rx) =
            std::sync::mpsc::sync_channel::<(String, PhaseResult)>(queue_bound);

        std::thread::scope(|scope| {
            let consumer = scope.spawn(move || drain_compile_results(result_rx, consume));
            self.compile_missing_targets_parallel(&missing, result_tx);
            consumer
                .join()
                .expect("bulk compile result consumer panicked");
        });
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn compile_missing_targets_parallel(
        &self,
        missing: &[String],
        result_tx: std::sync::mpsc::SyncSender<(String, PhaseResult)>,
    ) {
        let tree = &self.resolved_tree().0;
        missing.par_iter().for_each_with(result_tx, |tx, name| {
            let result = compile_model_internal(tree, name);
            self.compile_cache
                .lock()
                .expect("compiled source-root cache poisoned")
                .entry(name.clone())
                .or_insert_with(|| result.clone());
            let _ = tx.send((name.clone(), result));
        });
    }

    /// Compile the requested model strictly against its reachable closure while
    /// preserving unrelated source-root diagnostics outside that closure.
    pub fn compile_model_strict_reachable_with_recovery(
        &self,
        model_name: &str,
    ) -> StrictCompileReport {
        let tree = &self.resolved_tree().0;
        let closure = self.reachable_model_closure(model_name);
        let target_source_files = collect_target_source_files(tree, &closure.reachable_classes);
        let failures = collect_resolve_failures_for_files(
            &self.resolve_diagnostics,
            &tree.source_map,
            &target_source_files,
        );
        let target_has_resolve_failures = !failures.is_empty();
        let results = self.compile_targets_with_cache(&closure.compile_targets);
        finalize_strict_compile_report(
            tree,
            model_name,
            target_has_resolve_failures,
            failures,
            results,
        )
    }

    /// Compile the requested model strictly against its reachable closure
    /// without retaining phase results from prior focused compiles.
    pub fn compile_model_strict_reachable_uncached_with_recovery(
        &self,
        model_name: &str,
    ) -> StrictCompileReport {
        let tree = &self.resolved_tree().0;
        let closure = self.reachable_model_closure(model_name);
        let target_source_files = collect_target_source_files(tree, &closure.reachable_classes);
        let failures = collect_resolve_failures_for_files(
            &self.resolve_diagnostics,
            &tree.source_map,
            &target_source_files,
        );
        let target_has_resolve_failures = !failures.is_empty();
        finalize_strict_compile_report_from_uncached_targets(
            tree,
            model_name,
            target_has_resolve_failures,
            failures,
            &closure.compile_targets,
            InstantiateOptions::default(),
        )
    }

    /// Compile the requested model through DAE strictly against its reachable
    /// closure without retaining full Flat+DAE phase results.
    pub fn compile_model_dae_strict_reachable_uncached_with_recovery(
        &self,
        model_name: &str,
    ) -> std::result::Result<Box<DaeCompilationResult>, String> {
        let tree = &self.resolved_tree().0;
        let closure = self.reachable_model_closure(model_name);
        let target_source_files = collect_target_source_files(tree, &closure.reachable_classes);
        let mut failures = collect_resolve_failures_for_files(
            &self.resolve_diagnostics,
            &tree.source_map,
            &target_source_files,
        );
        let target_has_resolve_failures = !failures.is_empty();
        if target_has_resolve_failures {
            let requested = requested_missing_result_message(model_name, &failures);
            return Err(format_strict_failure_summary(
                model_name, requested, &failures, 8,
            ));
        }

        let requested_result = compile_model_dae_internal(tree, model_name);
        let requested = dae_phase_result_requested_message(model_name, &requested_result);
        if let Some(failure) = dae_phase_result_to_failure(tree, model_name, &requested_result) {
            failures.push(failure);
        }
        if !failures.is_empty() {
            return Err(format_strict_failure_summary(
                model_name, requested, &failures, 8,
            ));
        }

        match requested_result {
            DaePhaseResult::Success(result) => Ok(result),
            DaePhaseResult::NeedsInner { .. } | DaePhaseResult::Failed { .. } => Err(
                "strict DAE compile returned non-success requested result without collected diagnostics"
                    .to_string(),
            ),
        }
    }

    /// Compile a specific model.
    ///
    /// Uses the new phase order: Instantiate -> Typecheck -> Flatten -> ToDae
    pub fn compile_model(&self, model_name: &str) -> Result<CompilationResult> {
        match self.cached_phase_result(model_name) {
            PhaseResult::Success(result) => Ok(*result),
            PhaseResult::NeedsInner { missing_inners, .. } => Err(anyhow::anyhow!(
                "Missing inner declarations: {:?}",
                missing_inners
            )),
            PhaseResult::Failed { phase, error, .. } => {
                Err(anyhow::anyhow!("{} error: {}", phase, error))
            }
        }
    }

    /// Compile a model with phase-level tracking.
    ///
    /// Returns PhaseResult directly (infallible for pre-built source roots).
    pub fn compile_model_phases(&self, model_name: &str) -> PhaseResult {
        self.cached_phase_result(model_name)
    }

    /// Compile multiple models in parallel.
    pub fn compile_models_parallel(&self, model_names: &[&str]) -> Vec<(String, PhaseResult)> {
        let names = model_names
            .iter()
            .map(|name| (*name).to_string())
            .collect::<Vec<_>>();
        self.compile_targets_with_cache(&names)
    }

    /// Compile multiple models in parallel and stream each result to `consume`.
    ///
    /// This is intended for bulk callers that should not retain a whole batch of
    /// full compile artifacts before downstream reporting, simulation, or
    /// export can drain them. Cached results are delivered first in requested
    /// order; cache misses are delivered as they finish. `max_in_flight_results`
    /// bounds completed uncached results waiting for the consumer.
    pub fn compile_models_streaming<F>(
        &self,
        model_names: &[&str],
        max_in_flight_results: usize,
        consume: F,
    ) where
        F: FnMut(String, PhaseResult) + Send,
    {
        let names = model_names
            .iter()
            .map(|name| (*name).to_string())
            .collect::<Vec<_>>();
        self.compile_targets_streaming_with_cache(&names, max_in_flight_results, consume);
    }

    /// Compile all models in parallel.
    pub fn compile_all_parallel(&self) -> Vec<(String, PhaseResult)> {
        self.compile_targets_with_cache(&self.model_names)
    }

    /// Compile all models in parallel and stream each result to `consume`.
    pub fn compile_all_streaming<F>(&self, max_in_flight_results: usize, consume: F)
    where
        F: FnMut(String, PhaseResult) + Send,
    {
        self.compile_targets_streaming_with_cache(
            &self.model_names,
            max_in_flight_results,
            consume,
        );
    }

    /// Compile all models and return summary.
    pub fn compile_all_parallel_with_summary(
        &self,
    ) -> (Vec<(String, PhaseResult)>, CompilationSummary) {
        let results = self.compile_all_parallel();
        let summary = CompilationSummary::from_results(&results);
        (results, summary)
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn drain_compile_results<F>(
    result_rx: std::sync::mpsc::Receiver<(String, PhaseResult)>,
    mut consume: F,
) where
    F: FnMut(String, PhaseResult),
{
    for (name, result) in result_rx {
        consume(name, result);
    }
}
