use super::*;
use crate::session::compile_support::finalize_strict_compile_report_from_uncached_targets;

/// Indexed source root for efficient multi-model compilation.
///
/// This is a convenience wrapper around [`Session`] that initializes from
/// a [`ast::StoredDefinition`]. Use this when you've already parsed your source root
/// (e.g., from parallel file parsing) and want to compile multiple models.
///
/// For new code, consider using [`Session`] directly with [`Session::add_parsed_batch`].
pub struct CompiledSourceRoot {
    resolution: SourceRootResolution,
    resolved_targets: Mutex<IndexMap<String, Arc<ResolvedTree>>>,
    model_names: Vec<String>,
    class_type_counts: std::collections::HashMap<String, usize>,
    class_dependencies: IndexMap<String, IndexSet<String>>,
    pub(super) compile_cache: Mutex<IndexMap<String, PhaseResult>>,
}

enum SourceRootResolution {
    Complete(Arc<ResolvedTree>),
    Incomplete {
        tree: Arc<ast::ClassTree>,
        documents: Vec<ParsedSourceDocument>,
    },
}

impl SourceRootResolution {
    fn tree(&self) -> &ast::ClassTree {
        match self {
            Self::Complete(resolved) => resolved.inner(),
            Self::Incomplete { tree, .. } => tree,
        }
    }
}

impl CompiledSourceRoot {
    fn from_indexed_state(
        resolved: Arc<ResolvedTree>,
        model_names: Vec<String>,
        class_type_counts: std::collections::HashMap<String, usize>,
    ) -> Self {
        let dependency_fingerprints = DependencyFingerprintCache::from_tree(resolved.inner());
        Self {
            resolution: SourceRootResolution::Complete(resolved),
            resolved_targets: Mutex::new(IndexMap::new()),
            model_names,
            class_type_counts,
            class_dependencies: dependency_fingerprints.class_dependencies().clone(),
            compile_cache: Mutex::new(IndexMap::new()),
        }
    }

    /// Create a compiled source root from a ast::StoredDefinition.
    ///
    /// This resolves the AST once. Type checking happens after instantiation.
    pub fn from_stored_definition(
        def: ast::StoredDefinition,
        source_map: SourceMap,
    ) -> Result<Self> {
        let mut tree = ast::ClassTree::from_parsed(def);
        tree.source_map = source_map;
        let resolved = Arc::new(
            rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
                .map_err(|diagnostics| diagnostics_to_anyhow(&diagnostics))?,
        );
        let model_names = collect_model_names(&resolved.inner().definitions);
        let class_type_counts = collect_class_type_counts(&resolved.inner().definitions);
        Ok(Self::from_indexed_state(
            resolved,
            model_names,
            class_type_counts,
        ))
    }

    /// Create a compiled source root from a parsed batch.
    ///
    /// The full batch becomes a planning tree. Each requested target then
    /// receives an independently resolved source-closure proof before any
    /// semantic compilation phase runs.
    pub fn from_parsed_batch_with_resolution_planning(
        documents: Vec<(String, ast::StoredDefinition)>,
        source_map: SourceMap,
    ) -> Result<Self> {
        let mut session = Session::new(SessionConfig::default());
        let documents = documents
            .into_iter()
            .map(|(uri, definition)| {
                let source_id = source_map
                    .get_id(&uri)
                    .ok_or_else(|| anyhow::anyhow!("source map is missing `{uri}`"))?;
                let (_, source) = source_map
                    .get_source(source_id)
                    .ok_or_else(|| anyhow::anyhow!("source map is missing `{uri}`"))?;
                Ok(ParsedSourceDocument::from_parsed(
                    uri,
                    Arc::<str>::from(source),
                    definition,
                ))
            })
            .collect::<Result<Vec<_>>>()?;
        session.add_in_memory_parsed_batch(documents.clone());
        let (plan, _) = session
            .build_resolution_plan_for_strict_compile()
            .map_err(|diags| diagnostics_to_anyhow(&diags))?;
        let resolution = match plan {
            ResolutionPlanningTree::Complete(resolved) => SourceRootResolution::Complete(resolved),
            ResolutionPlanningTree::Incomplete(tree) => {
                SourceRootResolution::Incomplete { tree, documents }
            }
        };
        let tree = resolution.tree();
        let dependency_fingerprints = DependencyFingerprintCache::from_tree(tree);
        let class_type_counts = collect_class_type_counts(&tree.definitions);
        let class_dependencies = dependency_fingerprints.class_dependencies().clone();
        Ok(Self {
            resolution,
            resolved_targets: Mutex::new(IndexMap::new()),
            model_names: session.query_state.resolved.model_names.clone(),
            class_type_counts,
            class_dependencies,
            compile_cache: Mutex::new(IndexMap::new()),
        })
    }

    /// Create a compiled source root from an already-resolved tree.
    ///
    /// This avoids re-running resolve and is intended for callers that already
    /// hold a validated resolved tree (e.g., MSL regression harness).
    pub fn from_resolved_tree(resolved: ResolvedTree, model_names: Vec<String>) -> Self {
        let resolved = Arc::new(resolved);
        Self::from_indexed_state(
            resolved.clone(),
            model_names,
            collect_class_type_counts(&resolved.inner().definitions),
        )
    }

    /// Get all model names in the source root.
    ///
    /// Names come from the construction-time planning tree and remain
    /// available even when unrelated references prevent full-root resolution.
    pub fn model_names(&self) -> &[String] {
        &self.model_names
    }

    /// Count all class types in the source root.
    pub fn class_type_counts(&self) -> &std::collections::HashMap<String, usize> {
        &self.class_type_counts
    }

    /// Get the class tree.
    ///
    /// This is the read-only planning view. It is not evidence that every
    /// reference in the source root resolved successfully.
    pub fn tree(&self) -> &ast::ClassTree {
        self.resolution.tree()
    }

    /// Count classes reachable from a strict target model.
    pub fn reachable_class_count(&self, model_name: &str) -> usize {
        self.reachable_model_closure(model_name)
            .reachable_classes
            .len()
    }

    fn resolve_target_detailed(
        &self,
        model_name: &str,
    ) -> std::result::Result<Arc<ResolvedTree>, StrictTargetResolutionFailure> {
        match &self.resolution {
            SourceRootResolution::Complete(resolved) => Ok(resolved.clone()),
            SourceRootResolution::Incomplete { documents, .. } => {
                let cached = self
                    .resolved_targets
                    .lock()
                    .map_err(|_| self.target_resolution_cache_failure(model_name))?
                    .get(model_name)
                    .cloned();
                if let Some(resolved) = cached {
                    return Ok(resolved);
                }
                let mut session = Session::default();
                session.add_in_memory_parsed_batch(documents.clone());
                let resolved = session
                    .resolve_strict_target(model_name)
                    .map(|target| target.resolved)?;
                self.resolved_targets
                    .lock()
                    .map_err(|_| self.target_resolution_cache_failure(model_name))?
                    .insert(model_name.to_string(), resolved.clone());
                Ok(resolved)
            }
        }
    }

    fn target_resolution_cache_failure(&self, model_name: &str) -> StrictTargetResolutionFailure {
        StrictTargetResolutionFailure {
            failures: vec![ModelFailureDiagnostic {
                model_name: model_name.to_string(),
                phase: None,
                error_code: None,
                error: "target resolution cache poisoned".to_string(),
                primary_label: None,
                secondary_labels: Vec::new(),
                notes: Vec::new(),
            }],
            diagnostics: Vec::new(),
            source_map: Box::new(self.tree().source_map.clone()),
        }
    }

    fn resolve_target(&self, model_name: &str) -> Result<Arc<ResolvedTree>> {
        self.resolve_target_detailed(model_name).map_err(|failure| {
            anyhow::anyhow!(
                "{}",
                format_strict_failure_summary(
                    model_name,
                    requested_missing_result_message(model_name, &failure.failures),
                    &failure.failures,
                    8,
                )
            )
        })
    }

    fn cached_phase_result(&self, model_name: &str) -> Result<PhaseResult> {
        if let Some(result) = self.compile_cache()?.get(model_name).cloned() {
            return Ok(result);
        }

        let resolved = self.resolve_target(model_name)?;
        let result = compile_model_internal(resolved.inner(), model_name);
        self.compile_cache()?
            .entry(model_name.to_string())
            .or_insert_with(|| result.clone());
        Ok(result)
    }

    fn compile_cache(&self) -> Result<std::sync::MutexGuard<'_, IndexMap<String, PhaseResult>>> {
        self.compile_cache
            .lock()
            .map_err(|_| anyhow::anyhow!("compiled source-root cache poisoned"))
    }

    fn reachable_model_closure(&self, model_name: &str) -> ReachableModelClosure {
        ReachabilityPlanner::new(&self.class_dependencies, &self.model_names)
            .model_closure(model_name)
    }

    fn compile_targets_with_cache(&self, targets: &[String]) -> Result<Vec<(String, PhaseResult)>> {
        let (mut results, missing) = {
            let cache = self.compile_cache()?;
            split_cached_target_results(&cache, targets)
        };

        if !missing.is_empty() {
            let compiled_misses = missing
                .par_iter()
                .map(|name| {
                    let resolved = self.resolve_target(name)?;
                    Ok((name.clone(), compile_model_internal(resolved.inner(), name)))
                })
                .collect::<Result<Vec<_>>>()?;

            let mut cache = self.compile_cache()?;
            for (name, result) in compiled_misses {
                cache.entry(name.clone()).or_insert_with(|| result.clone());
                results.insert(name, result);
            }
        }

        Ok(targets
            .iter()
            .filter_map(|target| {
                results
                    .shift_remove(target)
                    .map(|result| (target.clone(), result))
            })
            .collect())
    }

    fn compile_targets_with_resolved_cache(
        &self,
        resolved: &ResolvedTree,
        targets: &[String],
    ) -> Result<Vec<(String, PhaseResult)>> {
        let (mut results, missing) = {
            let cache = self.compile_cache()?;
            split_cached_target_results(&cache, targets)
        };

        if !missing.is_empty() {
            let compiled_misses = missing
                .par_iter()
                .map(|name| (name.clone(), compile_model_internal(resolved.inner(), name)))
                .collect::<Vec<_>>();

            let mut cache = self.compile_cache()?;
            for (name, result) in compiled_misses {
                cache.entry(name.clone()).or_insert_with(|| result.clone());
                results.insert(name, result);
            }
        }

        Ok(targets
            .iter()
            .filter_map(|target| {
                results
                    .shift_remove(target)
                    .map(|result| (target.clone(), result))
            })
            .collect())
    }

    fn compile_targets_streaming_with_cache<F>(
        &self,
        targets: &[String],
        max_in_flight_results: usize,
        consume: F,
    ) -> Result<()>
    where
        F: FnMut(String, PhaseResult) + Send,
    {
        let (mut cached_results, missing) = {
            let cache = self.compile_cache()?;
            split_cached_target_results(&cache, targets)
        };

        let mut consume = consume;
        for target in targets {
            if let Some(result) = cached_results.shift_remove(target) {
                consume(target.clone(), result);
            }
        }
        if missing.is_empty() {
            return Ok(());
        }

        self.compile_missing_targets_streaming_with_cache(missing, max_in_flight_results, consume)
    }

    #[cfg(target_arch = "wasm32")]
    fn compile_missing_targets_streaming_with_cache<F>(
        &self,
        missing: Vec<String>,
        _max_in_flight_results: usize,
        mut consume: F,
    ) -> Result<()>
    where
        F: FnMut(String, PhaseResult) + Send,
    {
        for name in missing {
            let resolved = self.resolve_target(&name)?;
            let result = compile_model_internal(resolved.inner(), &name);
            self.compile_cache()?
                .entry(name.clone())
                .or_insert_with(|| result.clone());
            consume(name, result);
        }
        Ok(())
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn compile_missing_targets_streaming_with_cache<F>(
        &self,
        missing: Vec<String>,
        max_in_flight_results: usize,
        consume: F,
    ) -> Result<()>
    where
        F: FnMut(String, PhaseResult) + Send,
    {
        let queue_bound = max_in_flight_results.max(1);
        let (result_tx, result_rx) =
            std::sync::mpsc::sync_channel::<(String, PhaseResult)>(queue_bound);

        std::thread::scope(|scope| {
            let consumer = scope.spawn(move || drain_compile_results(result_rx, consume));
            let producer_result = self.compile_missing_targets_parallel(&missing, result_tx);
            let consumer_result = consumer
                .join()
                .map_err(|_| anyhow::anyhow!("bulk compile result consumer panicked"))?;
            producer_result?;
            consumer_result
        })
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn compile_missing_targets_parallel(
        &self,
        missing: &[String],
        result_tx: std::sync::mpsc::SyncSender<(String, PhaseResult)>,
    ) -> Result<()> {
        let results = missing
            .par_iter()
            .map_with(result_tx, |tx, name| -> Result<()> {
                let resolved = self.resolve_target(name)?;
                let result = compile_model_internal(resolved.inner(), name);
                self.compile_cache()?
                    .entry(name.clone())
                    .or_insert_with(|| result.clone());
                tx.send((name.clone(), result))
                    .map_err(|_| anyhow::anyhow!("bulk compile result consumer disconnected"))?;
                Ok(())
            })
            .collect::<Vec<_>>();
        for result in results {
            result?;
        }
        Ok(())
    }

    /// Compile the requested model strictly against its reachable closure while
    /// preserving unrelated source-root diagnostics outside that closure.
    pub fn compile_model_strict_reachable_with_recovery(
        &self,
        model_name: &str,
    ) -> Result<StrictCompileReport> {
        let resolved = match self.resolve_target_detailed(model_name) {
            Ok(resolved) => resolved,
            Err(failure) => {
                return Ok(StrictCompileReport {
                    requested_model: model_name.to_string(),
                    requested_result: None,
                    summary: CompilationSummary::default(),
                    failures: failure.failures,
                    source_map: Some(*failure.source_map),
                });
            }
        };
        let tree = resolved.inner();
        let closure = self.reachable_model_closure(model_name);
        let failures = Vec::new();
        let results =
            self.compile_targets_with_resolved_cache(&resolved, &closure.compile_targets)?;
        Ok(finalize_strict_compile_report(
            tree, model_name, failures, results,
        ))
    }

    /// Compile the requested model strictly against its reachable closure
    /// without retaining phase results from prior focused compiles.
    pub fn compile_model_strict_reachable_uncached_with_recovery(
        &self,
        model_name: &str,
    ) -> StrictCompileReport {
        let resolved = match self.resolve_target_detailed(model_name) {
            Ok(resolved) => resolved,
            Err(failure) => {
                return StrictCompileReport {
                    requested_model: model_name.to_string(),
                    requested_result: None,
                    summary: CompilationSummary::default(),
                    failures: failure.failures,
                    source_map: Some(*failure.source_map),
                };
            }
        };
        let tree = resolved.inner();
        let closure = self.reachable_model_closure(model_name);
        let failures = Vec::new();
        finalize_strict_compile_report_from_uncached_targets(
            tree,
            model_name,
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
        let resolved = self
            .resolve_target_detailed(model_name)
            .map_err(|failure| {
                format_strict_failure_summary(
                    model_name,
                    requested_missing_result_message(model_name, &failure.failures),
                    &failure.failures,
                    8,
                )
            })?;
        let tree = resolved.inner();
        let mut failures = Vec::new();

        let requested_result = compile_model_dae_internal(tree, model_name);
        let requested = dae_phase_result_requested_message(model_name, &requested_result);
        failures.extend(dae_phase_result_to_failures(
            tree,
            model_name,
            &requested_result,
        ));
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
        match self.cached_phase_result(model_name)? {
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
    /// Returns phase-level status, or an infrastructure error if cached state
    /// cannot be read.
    pub fn compile_model_phases(&self, model_name: &str) -> Result<PhaseResult> {
        self.cached_phase_result(model_name)
    }

    /// Compile multiple models in parallel.
    pub fn compile_models_parallel(
        &self,
        model_names: &[&str],
    ) -> Result<Vec<(String, PhaseResult)>> {
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
    ) -> Result<()>
    where
        F: FnMut(String, PhaseResult) + Send,
    {
        let names = model_names
            .iter()
            .map(|name| (*name).to_string())
            .collect::<Vec<_>>();
        self.compile_targets_streaming_with_cache(&names, max_in_flight_results, consume)
    }

    /// Compile all models in parallel.
    pub fn compile_all_parallel(&self) -> Result<Vec<(String, PhaseResult)>> {
        self.compile_targets_with_cache(&self.model_names)
    }

    /// Compile all models in parallel and stream each result to `consume`.
    pub fn compile_all_streaming<F>(&self, max_in_flight_results: usize, consume: F) -> Result<()>
    where
        F: FnMut(String, PhaseResult) + Send,
    {
        self.compile_targets_streaming_with_cache(&self.model_names, max_in_flight_results, consume)
    }

    /// Compile all models and return summary.
    pub fn compile_all_parallel_with_summary(
        &self,
    ) -> Result<(Vec<(String, PhaseResult)>, CompilationSummary)> {
        let results = self.compile_all_parallel()?;
        let summary = CompilationSummary::from_results(&results);
        Ok((results, summary))
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn drain_compile_results<F>(
    result_rx: std::sync::mpsc::Receiver<(String, PhaseResult)>,
    mut consume: F,
) -> Result<()>
where
    F: FnMut(String, PhaseResult),
{
    for (name, result) in result_rx {
        consume(name, result);
    }
    Ok(())
}
