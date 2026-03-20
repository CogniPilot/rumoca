use super::*;

/// Pre-compiled library for efficient multi-model compilation.
///
/// This is a convenience wrapper around [`Session`] that initializes from
/// a [`ast::StoredDefinition`]. Use this when you've already parsed your library
/// (e.g., from parallel file parsing) and want to compile multiple models.
///
/// For new code, consider using [`Session`] directly with [`Session::add_parsed_batch`].
pub struct CompiledLibrary {
    resolved: Arc<ast::ResolvedTree>,
    model_names: Vec<String>,
    class_type_counts: std::collections::HashMap<String, usize>,
    class_dependencies: IndexMap<String, IndexSet<String>>,
    resolve_diagnostics: CommonDiagnostics,
    pub(super) compile_cache: Mutex<IndexMap<String, PhaseResult>>,
}

impl CompiledLibrary {
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

    /// Create a compiled library from a ast::StoredDefinition.
    ///
    /// This resolves the AST once. Type checking happens after instantiation.
    pub fn from_stored_definition(def: ast::StoredDefinition) -> Result<Self> {
        let mut session = Session::new(SessionConfig::default());
        session.add_parsed("library", def);
        session.build_resolved()?;
        let resolved = session.ensure_resolved()?.clone();
        Ok(Self::from_indexed_state(
            resolved.clone(),
            session.query_state.resolved.model_names.clone(),
            collect_class_type_counts(&resolved.0.definitions),
            CommonDiagnostics::new(),
        ))
    }

    /// Create a compiled library from a parsed batch, indexing it tolerantly.
    ///
    /// This preserves whole-library resolve diagnostics for later strict
    /// target-closure compilation without requiring the entire library to
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

    /// Create a compiled library from an already-resolved tree.
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

    /// Get all model names in the library.
    ///
    /// This is infallible after construction since build_resolved was called.
    pub fn model_names(&self) -> &[String] {
        &self.model_names
    }

    /// Count all class types in the library.
    pub fn class_type_counts(&self) -> &std::collections::HashMap<String, usize> {
        &self.class_type_counts
    }

    /// Get the class tree.
    ///
    /// This is infallible after construction since build_resolved was called.
    pub fn tree(&self) -> &ast::ClassTree {
        &self.resolved_tree().0
    }

    /// Get the resolved tree reference (guaranteed present after construction).
    fn resolved_tree(&self) -> &Arc<ast::ResolvedTree> {
        &self.resolved
    }

    fn cached_phase_result(&self, model_name: &str) -> PhaseResult {
        if let Some(result) = self
            .compile_cache
            .lock()
            .expect("compiled library cache poisoned")
            .get(model_name)
            .cloned()
        {
            return result;
        }

        let result = compile_model_internal(&self.resolved_tree().0, model_name);
        self.compile_cache
            .lock()
            .expect("compiled library cache poisoned")
            .entry(model_name.to_string())
            .or_insert_with(|| result.clone());
        result
    }

    fn reachable_model_closure(&self, model_name: &str) -> ReachableModelClosure {
        ReachabilityPlanner::new(&self.class_dependencies, &self.model_names)
            .model_closure(model_name)
    }

    fn compile_targets_without_cache(&self, targets: &[String]) -> Vec<(String, PhaseResult)> {
        let tree = &self.resolved_tree().0;
        targets
            .par_iter()
            .map(|name| (name.clone(), compile_model_internal(tree, name)))
            .collect()
    }

    fn compile_targets_with_cache(&self, targets: &[String]) -> Vec<(String, PhaseResult)> {
        let (mut results, missing) = {
            let cache = self
                .compile_cache
                .lock()
                .expect("compiled library cache poisoned");
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
                .expect("compiled library cache poisoned");
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

    /// Compile the requested model strictly against its reachable closure while
    /// preserving unrelated library diagnostics outside that closure.
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
        let results = self.compile_targets_without_cache(&closure.compile_targets);
        finalize_strict_compile_report(
            tree,
            model_name,
            target_has_resolve_failures,
            failures,
            results,
        )
    }

    /// Compile a specific model.
    ///
    /// Uses the new phase order: Instantiate -> Typecheck -> Flatten -> ToDae
    pub fn compile_model(&self, model_name: &str) -> Result<CompilationResult> {
        match self.cached_phase_result(model_name) {
            PhaseResult::Success(result) => Ok(*result),
            PhaseResult::NeedsInner { missing_inners } => Err(anyhow::anyhow!(
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
    /// Returns PhaseResult directly (infallible for pre-built libraries).
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

    /// Compile all models in parallel.
    pub fn compile_all_parallel(&self) -> Vec<(String, PhaseResult)> {
        self.compile_targets_with_cache(&self.model_names)
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
