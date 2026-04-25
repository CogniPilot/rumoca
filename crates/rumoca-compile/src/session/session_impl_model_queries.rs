use super::*;

impl Session {
    fn cached_reachable_model_closure(
        &mut self,
        model_key: &ModelKey,
        mode: ResolveBuildMode,
        fingerprint: Fingerprint,
    ) -> Option<ReachableModelClosure> {
        let key = ReachableModelClosureCacheKey::new(model_key.clone(), mode);
        let artifact = self
            .query_state
            .resolved
            .reachable_model_closures
            .shift_remove(&key)?;
        let is_hit = artifact.fingerprint == fingerprint;
        let closure = artifact.closure.clone();
        self.query_state
            .resolved
            .reachable_model_closures
            .insert(key, artifact);
        is_hit.then_some(closure)
    }

    fn insert_reachable_model_closure(
        &mut self,
        model_key: ModelKey,
        mode: ResolveBuildMode,
        fingerprint: Fingerprint,
        closure: ReachableModelClosure,
    ) {
        let key = ReachableModelClosureCacheKey::new(model_key, mode);
        self.query_state
            .resolved
            .reachable_model_closures
            .shift_remove(&key);
        self.query_state.resolved.reachable_model_closures.insert(
            key,
            ReachableModelClosureArtifact {
                fingerprint,
                closure,
            },
        );
        Self::trim_lru_cache(
            &mut self.query_state.resolved.reachable_model_closures,
            MAX_SESSION_MODEL_QUERY_CACHE_ENTRIES,
        );
    }

    fn build_reachable_model_closure(
        &mut self,
        tree: &ast::ClassTree,
        mode: ResolveBuildMode,
        model_name: &str,
    ) -> ReachableModelClosure {
        let dep_cache = self
            .query_state
            .resolved
            .dependency_fingerprints
            .get_or_insert_with(mode, || DependencyFingerprintCache::from_tree(tree));
        let model_names = collect_model_names(&tree.definitions);
        let planner = ReachabilityPlanner::new(dep_cache.class_dependencies(), &model_names);
        planner.model_closure(model_name)
    }

    pub(in crate::session) fn reachable_model_closure_query(
        &mut self,
        tree: &ast::ClassTree,
        mode: ResolveBuildMode,
        model_name: &str,
    ) -> ReachableModelClosure {
        let Some(model_key) = self.model_key_query(model_name) else {
            return self.build_reachable_model_closure(tree, mode, model_name);
        };

        let fingerprint = self.model_dependency_fingerprint(tree, mode, model_name);
        if let Some(cached) = self.cached_reachable_model_closure(&model_key, mode, fingerprint) {
            return cached;
        }

        let closure = self.build_reachable_model_closure(tree, mode, &model_key.qualified_name());
        self.insert_reachable_model_closure(model_key, mode, fingerprint, closure.clone());
        closure
    }

    fn cached_instantiated_model(
        &mut self,
        model_key: &ModelKey,
        mode: ResolveBuildMode,
        fingerprint: Fingerprint,
    ) -> Option<InstantiatedModelOutcome> {
        let key = InstantiatedModelCacheKey::new(model_key.clone(), mode);
        let artifact = self
            .query_state
            .flat
            .instantiated_models
            .shift_remove(&key)?;
        let is_hit = artifact.fingerprint == fingerprint;
        let outcome = artifact.outcome.clone();
        self.query_state
            .flat
            .instantiated_models
            .insert(key, artifact);
        is_hit.then_some(outcome)
    }

    fn insert_instantiated_model(
        &mut self,
        model_key: ModelKey,
        mode: ResolveBuildMode,
        fingerprint: Fingerprint,
        outcome: InstantiatedModelOutcome,
    ) {
        let key = InstantiatedModelCacheKey::new(model_key, mode);
        self.query_state.flat.instantiated_models.shift_remove(&key);
        self.query_state.flat.instantiated_models.insert(
            key,
            InstantiatedModelArtifact {
                fingerprint,
                outcome,
            },
        );
        Self::trim_lru_cache(
            &mut self.query_state.flat.instantiated_models,
            MAX_SESSION_MODEL_QUERY_CACHE_ENTRIES,
        );
    }

    fn build_instantiated_model(
        &mut self,
        tree: &ast::ClassTree,
        model_name: &str,
    ) -> InstantiatedModelOutcome {
        InstantiatedModelOutcome::from_instantiation_outcome(instantiate_model_with_outcome(
            tree, model_name,
        ))
    }

    fn instantiated_model_query_with_status(
        &mut self,
        tree: &ast::ClassTree,
        mode: ResolveBuildMode,
        model_name: &str,
    ) -> (InstantiatedModelOutcome, bool) {
        let Some(model_key) = self.model_key_query(model_name) else {
            record_instantiated_model_cache_miss();
            record_instantiated_model_build();
            return (self.build_instantiated_model(tree, model_name), true);
        };

        let fingerprint = self.model_dependency_fingerprint(tree, mode, model_name);
        if let Some(cached) = self.cached_instantiated_model(&model_key, mode, fingerprint) {
            record_instantiated_model_cache_hit();
            return (cached, false);
        }

        record_instantiated_model_cache_miss();
        let outcome = self.build_instantiated_model(tree, &model_key.qualified_name());
        record_instantiated_model_build();
        self.insert_instantiated_model(model_key, mode, fingerprint, outcome.clone());
        (outcome, true)
    }

    fn cached_typed_model(
        &mut self,
        model_key: &ModelKey,
        mode: ResolveBuildMode,
        fingerprint: Fingerprint,
    ) -> Option<TypedModelOutcome> {
        let key = TypedModelCacheKey::new(model_key.clone(), mode);
        let artifact = self
            .query_state
            .flat
            .typed_models
            .artifacts
            .shift_remove(&key)?;
        let is_hit = artifact.fingerprint == fingerprint;
        let outcome = artifact.outcome.clone();
        self.query_state
            .flat
            .typed_models
            .artifacts
            .insert(key, artifact);
        is_hit.then_some(outcome)
    }

    fn insert_typed_model(
        &mut self,
        model_key: ModelKey,
        mode: ResolveBuildMode,
        fingerprint: Fingerprint,
        outcome: TypedModelOutcome,
    ) {
        let key = TypedModelCacheKey::new(model_key, mode);
        self.query_state
            .flat
            .typed_models
            .artifacts
            .shift_remove(&key);
        self.query_state.flat.typed_models.artifacts.insert(
            key,
            TypedModelArtifact {
                fingerprint,
                outcome,
            },
        );
        Self::trim_lru_cache(
            &mut self.query_state.flat.typed_models.artifacts,
            MAX_SESSION_MODEL_QUERY_CACHE_ENTRIES,
        );
    }

    fn typed_model_query_impl(
        &mut self,
        tree: &ast::ClassTree,
        mode: ResolveBuildMode,
        model_name: &str,
        record_compile_timings: bool,
    ) -> TypedModelOutcome {
        let instantiate_started = if record_compile_timings {
            maybe_start_timer()
        } else {
            None
        };

        let Some(model_key) = self.model_key_query(model_name) else {
            record_typed_model_cache_miss();
            record_instantiated_model_cache_miss();
            let instantiated = self.build_instantiated_model(tree, model_name);
            record_instantiated_model_build();
            if record_compile_timings {
                maybe_record_compile_phase_timing(FailedPhase::Instantiate, instantiate_started);
            }
            let typecheck_started = if record_compile_timings {
                maybe_start_timer()
            } else {
                None
            };
            let (typed, typechecked_built) =
                typed_model_outcome_from_instantiated(tree, model_name, instantiated);
            if record_compile_timings && typechecked_built {
                maybe_record_compile_phase_timing(FailedPhase::Typecheck, typecheck_started);
            }
            if typechecked_built {
                record_typed_model_build();
            }
            return typed;
        };

        let fingerprint = self.model_dependency_fingerprint(tree, mode, model_name);
        if let Some(cached) = self.cached_typed_model(&model_key, mode, fingerprint) {
            record_typed_model_cache_hit();
            return cached;
        }

        record_typed_model_cache_miss();
        let (instantiated, instantiated_built) =
            self.instantiated_model_query_with_status(tree, mode, model_name);
        if record_compile_timings && instantiated_built {
            maybe_record_compile_phase_timing(FailedPhase::Instantiate, instantiate_started);
        }

        let typecheck_started = if record_compile_timings {
            maybe_start_timer()
        } else {
            None
        };
        let (typed, typechecked_built) =
            typed_model_outcome_from_instantiated(tree, model_name, instantiated);
        if record_compile_timings && typechecked_built {
            maybe_record_compile_phase_timing(FailedPhase::Typecheck, typecheck_started);
        }
        if typechecked_built {
            record_typed_model_build();
        }

        self.insert_typed_model(model_key, mode, fingerprint, typed.clone());
        typed
    }

    pub(in crate::session) fn typed_model_query(
        &mut self,
        tree: &ast::ClassTree,
        mode: ResolveBuildMode,
        model_name: &str,
    ) -> TypedModelOutcome {
        self.typed_model_query_impl(tree, mode, model_name, false)
    }

    fn cached_flat_model(
        &mut self,
        model_key: &ModelKey,
        mode: ResolveBuildMode,
        fingerprint: Fingerprint,
    ) -> Option<FlatModelOutcome> {
        let key = FlatModelCacheKey::new(model_key.clone(), mode);
        let artifact = self
            .query_state
            .flat
            .flat_models
            .artifacts
            .shift_remove(&key)?;
        let is_hit = artifact.fingerprint == fingerprint;
        let outcome = artifact.outcome.clone();
        self.query_state
            .flat
            .flat_models
            .artifacts
            .insert(key, artifact);
        is_hit.then_some(outcome)
    }

    fn insert_flat_model(
        &mut self,
        model_key: ModelKey,
        mode: ResolveBuildMode,
        fingerprint: Fingerprint,
        outcome: FlatModelOutcome,
    ) {
        let key = FlatModelCacheKey::new(model_key, mode);
        self.query_state
            .flat
            .flat_models
            .artifacts
            .shift_remove(&key);
        self.query_state.flat.flat_models.artifacts.insert(
            key,
            FlatModelArtifact {
                fingerprint,
                outcome,
            },
        );
        Self::trim_lru_cache(
            &mut self.query_state.flat.flat_models.artifacts,
            MAX_SESSION_MODEL_QUERY_CACHE_ENTRIES,
        );
    }

    fn flat_model_query_impl(
        &mut self,
        tree: &ast::ClassTree,
        mode: ResolveBuildMode,
        model_name: &str,
        record_compile_timings: bool,
    ) -> FlatModelOutcome {
        let Some(model_key) = self.model_key_query(model_name) else {
            record_flat_model_cache_miss();
            let typed = self.typed_model_query_impl(tree, mode, model_name, record_compile_timings);
            let flatten_started = if record_compile_timings {
                maybe_start_timer()
            } else {
                None
            };
            let (flat, flattened_built) = flat_model_outcome_from_typed(tree, model_name, typed);
            if record_compile_timings && flattened_built {
                maybe_record_compile_phase_timing(FailedPhase::Flatten, flatten_started);
            }
            if flattened_built {
                record_flat_model_build();
            }
            return flat;
        };

        let fingerprint = self.model_dependency_fingerprint(tree, mode, model_name);
        if let Some(cached) = self.cached_flat_model(&model_key, mode, fingerprint) {
            record_flat_model_cache_hit();
            return cached;
        }

        record_flat_model_cache_miss();
        let typed = self.typed_model_query_impl(tree, mode, model_name, record_compile_timings);
        let flatten_started = if record_compile_timings {
            maybe_start_timer()
        } else {
            None
        };
        let (flat, flattened_built) = flat_model_outcome_from_typed(tree, model_name, typed);
        if record_compile_timings && flattened_built {
            maybe_record_compile_phase_timing(FailedPhase::Flatten, flatten_started);
        }
        if flattened_built {
            record_flat_model_build();
        }

        self.insert_flat_model(model_key, mode, fingerprint, flat.clone());
        flat
    }

    fn cached_dae_model(
        &mut self,
        model_key: &ModelKey,
        mode: ResolveBuildMode,
        fingerprint: Fingerprint,
    ) -> Option<DaeModelOutcome> {
        let key = DaeModelCacheKey::new(model_key.clone(), mode);
        let artifact = self.query_state.dae.dae_models.shift_remove(&key)?;
        let is_hit = artifact.fingerprint == fingerprint;
        let outcome = artifact.outcome.clone();
        self.query_state.dae.dae_models.insert(key, artifact);
        is_hit.then_some(outcome)
    }

    fn insert_dae_model(
        &mut self,
        model_key: ModelKey,
        mode: ResolveBuildMode,
        fingerprint: Fingerprint,
        outcome: DaeModelOutcome,
    ) {
        let key = DaeModelCacheKey::new(model_key, mode);
        self.query_state.dae.dae_models.shift_remove(&key);
        self.query_state.dae.dae_models.insert(
            key,
            DaeModelArtifact {
                fingerprint,
                outcome,
            },
        );
        Self::trim_lru_cache(
            &mut self.query_state.dae.dae_models,
            MAX_SESSION_MODEL_QUERY_CACHE_ENTRIES,
        );
    }

    fn dae_model_query_impl(
        &mut self,
        tree: &ast::ClassTree,
        mode: ResolveBuildMode,
        model_name: &str,
        record_compile_timings: bool,
    ) -> DaeModelOutcome {
        let Some(model_key) = self.model_key_query(model_name) else {
            record_dae_model_cache_miss();
            let flat = self.flat_model_query_impl(tree, mode, model_name, record_compile_timings);
            let todae_started = if record_compile_timings {
                maybe_start_timer()
            } else {
                None
            };
            let (dae, todae_built) = dae_model_outcome_from_flat(tree, flat);
            if record_compile_timings && todae_built {
                maybe_record_compile_phase_timing(FailedPhase::ToDae, todae_started);
            }
            if todae_built {
                record_dae_model_build();
            }
            return dae;
        };

        let fingerprint = self.model_dependency_fingerprint(tree, mode, model_name);
        if let Some(cached) = self.cached_dae_model(&model_key, mode, fingerprint) {
            record_dae_model_cache_hit();
            return cached;
        }

        record_dae_model_cache_miss();
        let flat = self.flat_model_query_impl(tree, mode, model_name, record_compile_timings);
        let todae_started = if record_compile_timings {
            maybe_start_timer()
        } else {
            None
        };
        let (dae, todae_built) = dae_model_outcome_from_flat(tree, flat);
        if record_compile_timings && todae_built {
            maybe_record_compile_phase_timing(FailedPhase::ToDae, todae_started);
        }
        if todae_built {
            record_dae_model_build();
        }

        self.insert_dae_model(model_key, mode, fingerprint, dae.clone());
        dae
    }

    pub(in crate::session) fn dae_model_query(
        &mut self,
        tree: &ast::ClassTree,
        mode: ResolveBuildMode,
        model_name: &str,
    ) -> DaeModelOutcome {
        self.dae_model_query_impl(tree, mode, model_name, false)
    }

    pub(in crate::session) fn dae_phase_result_query(
        &mut self,
        tree: &ast::ClassTree,
        mode: ResolveBuildMode,
        model_name: &str,
    ) -> DaePhaseResult {
        let dae = self.dae_model_query_impl(tree, mode, model_name, true);
        dae_phase_result_from_dae(tree, model_name, dae)
    }

    pub(in crate::session) fn compile_phase_result_query(
        &mut self,
        tree: &ast::ClassTree,
        mode: ResolveBuildMode,
        model_name: &str,
    ) -> PhaseResult {
        let dae = self.dae_model_query_impl(tree, mode, model_name, true);
        compile_phase_result_from_dae(tree, model_name, dae)
    }
}
