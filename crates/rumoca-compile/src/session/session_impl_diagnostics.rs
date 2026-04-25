use super::*;

#[derive(Debug, Clone)]
struct InterfaceSemanticDiagnosticsResult {
    resolved: Arc<ast::ResolvedTree>,
    fingerprint: Fingerprint,
    class_type: Option<ast::ClassType>,
}

#[derive(Debug, Clone)]
struct BodySemanticDiagnosticsResult {
    diagnostics: ModelDiagnostics,
    blocks_model_stage: bool,
}

impl Session {
    fn cached_interface_semantic_diagnostics(
        &mut self,
        model_name: &str,
        mode: SemanticDiagnosticsMode,
        fingerprint: Fingerprint,
    ) -> Option<Option<ast::ClassType>> {
        let key = SemanticDiagnosticsCacheKey::new(model_name, mode);
        let artifact = self
            .query_state
            .flat
            .semantic_diagnostics
            .interface_artifacts
            .shift_remove(&key)?;
        let is_hit = artifact.fingerprint == fingerprint;
        let class_type = artifact.class_type.clone();
        self.query_state
            .flat
            .semantic_diagnostics
            .interface_artifacts
            .insert(key, artifact);
        is_hit.then_some(class_type)
    }

    fn insert_interface_semantic_diagnostics(
        &mut self,
        model_name: String,
        mode: SemanticDiagnosticsMode,
        fingerprint: Fingerprint,
        class_type: Option<ast::ClassType>,
    ) {
        let key = SemanticDiagnosticsCacheKey::new(&model_name, mode);
        self.query_state
            .flat
            .semantic_diagnostics
            .interface_artifacts
            .shift_remove(&key);
        self.query_state
            .flat
            .semantic_diagnostics
            .interface_artifacts
            .insert(
                key,
                InterfaceSemanticDiagnosticsArtifact {
                    fingerprint,
                    class_type,
                },
            );
        Self::trim_lru_cache(
            &mut self
                .query_state
                .flat
                .semantic_diagnostics
                .interface_artifacts,
            MAX_SESSION_SEMANTIC_DIAGNOSTICS_CACHE_ENTRIES,
        );
    }

    fn cached_body_semantic_diagnostics(
        &mut self,
        model_name: &str,
        mode: SemanticDiagnosticsMode,
        fingerprint: Fingerprint,
    ) -> Option<BodySemanticDiagnosticsResult> {
        let key = SemanticDiagnosticsCacheKey::new(model_name, mode);
        let artifact = self
            .query_state
            .flat
            .semantic_diagnostics
            .body_artifacts
            .shift_remove(&key)?;
        let is_hit = artifact.fingerprint == fingerprint;
        let diagnostics = artifact.diagnostics.clone();
        let blocks_model_stage = artifact.blocks_model_stage;
        self.query_state
            .flat
            .semantic_diagnostics
            .body_artifacts
            .insert(key, artifact);
        is_hit.then_some(BodySemanticDiagnosticsResult {
            diagnostics,
            blocks_model_stage,
        })
    }

    fn insert_body_semantic_diagnostics(
        &mut self,
        model_name: String,
        mode: SemanticDiagnosticsMode,
        fingerprint: Fingerprint,
        diagnostics: ModelDiagnostics,
        blocks_model_stage: bool,
    ) {
        let key = SemanticDiagnosticsCacheKey::new(&model_name, mode);
        self.query_state
            .flat
            .semantic_diagnostics
            .body_artifacts
            .shift_remove(&key);
        self.query_state
            .flat
            .semantic_diagnostics
            .body_artifacts
            .insert(
                key,
                BodySemanticDiagnosticsArtifact {
                    fingerprint,
                    diagnostics,
                    blocks_model_stage,
                },
            );
        Self::trim_lru_cache(
            &mut self.query_state.flat.semantic_diagnostics.body_artifacts,
            MAX_SESSION_SEMANTIC_DIAGNOSTICS_CACHE_ENTRIES,
        );
    }

    fn cached_model_stage_semantic_diagnostics(
        &mut self,
        model_name: &str,
        mode: SemanticDiagnosticsMode,
        fingerprint: Fingerprint,
    ) -> Option<ModelDiagnostics> {
        let key = SemanticDiagnosticsCacheKey::new(model_name, mode);
        let artifact = self
            .query_state
            .flat
            .semantic_diagnostics
            .model_stage_artifacts
            .shift_remove(&key)?;
        let is_hit = artifact.fingerprint == fingerprint;
        let diagnostics = artifact.diagnostics.clone();
        self.query_state
            .flat
            .semantic_diagnostics
            .model_stage_artifacts
            .insert(key, artifact);
        is_hit.then_some(diagnostics)
    }

    fn insert_model_stage_semantic_diagnostics(
        &mut self,
        model_name: String,
        mode: SemanticDiagnosticsMode,
        fingerprint: Fingerprint,
        diagnostics: ModelDiagnostics,
    ) {
        let key = SemanticDiagnosticsCacheKey::new(&model_name, mode);
        self.query_state
            .flat
            .semantic_diagnostics
            .model_stage_artifacts
            .shift_remove(&key);
        self.query_state
            .flat
            .semantic_diagnostics
            .model_stage_artifacts
            .insert(
                key,
                SemanticDiagnosticsArtifact {
                    fingerprint,
                    diagnostics,
                },
            );
        Self::trim_lru_cache(
            &mut self
                .query_state
                .flat
                .semantic_diagnostics
                .model_stage_artifacts,
            MAX_SESSION_SEMANTIC_DIAGNOSTICS_CACHE_ENTRIES,
        );
    }

    fn build_semantic_diagnostics_resolved(
        &mut self,
        mode: SemanticDiagnosticsMode,
    ) -> Result<(Arc<ast::ResolvedTree>, CommonDiagnostics), CommonDiagnostics> {
        if let Some(resolved) = self
            .query_state
            .flat
            .semantic_diagnostics
            .resolved_by_mode
            .get(&mode)
        {
            let diagnostics = diagnostics_from_vec(
                self.query_state
                    .flat
                    .semantic_diagnostics
                    .resolved_diagnostics_by_mode
                    .get(&mode)
                    .cloned()
                    .unwrap_or_default(),
            );
            return Ok((resolved.clone(), diagnostics));
        }

        let build_started = maybe_start_timer();
        let resolved_result = self.resolve_documents_for_mode(mode.resolve_build_mode());
        let _ = maybe_elapsed_duration(build_started);
        let (resolved, diagnostics, _) = resolved_result?;
        self.query_state
            .flat
            .semantic_diagnostics
            .resolved_by_mode
            .insert(mode, resolved.clone());
        self.query_state
            .flat
            .semantic_diagnostics
            .resolved_diagnostics_by_mode
            .insert(mode, diagnostics.iter().cloned().collect());
        Ok((resolved, diagnostics))
    }

    fn save_mode_target_resolve_diagnostics(
        &mut self,
        tree: &ast::ClassTree,
        resolve_diagnostics: &CommonDiagnostics,
        model_name: &str,
    ) -> Vec<CommonDiagnostic> {
        if resolve_diagnostics.is_empty() {
            return Vec::new();
        }

        let closure = self.reachable_model_closure_query(
            tree,
            ResolveBuildMode::StrictCompileRecovery,
            model_name,
        );
        let target_source_files = collect_target_source_files(tree, &closure.reachable_classes);
        if target_source_files.is_empty() {
            return Vec::new();
        }

        resolve_diagnostics
            .iter()
            .filter(|diag| resolve_diagnostic_in_target_files(tree, &target_source_files, diag))
            .cloned()
            .collect()
    }

    fn semantic_diagnostics_fingerprint(
        &mut self,
        tree: &ast::ClassTree,
        mode: SemanticDiagnosticsMode,
        model_name: &str,
    ) -> Fingerprint {
        let started = maybe_start_timer();
        let fingerprint = self
            .query_state
            .flat
            .semantic_diagnostics
            .dependency_fingerprints_by_mode
            .entry(mode)
            .or_insert_with(|| DependencyFingerprintCache::from_tree(tree))
            .model_fingerprint(model_name);
        let _ = maybe_elapsed_duration(started);
        fingerprint
    }

    fn interface_semantic_diagnostics_class_type(
        &mut self,
        model_name: &str,
    ) -> Option<ast::ClassType> {
        let target = self.lookup_query_class_target(model_name)?;
        self.class_interface_query(&target.uri, &target.qualified_name)
            .map(|class_interface| class_interface.class_type().clone())
    }

    fn interface_semantic_diagnostics_query(
        &mut self,
        model_name: &str,
        mode: SemanticDiagnosticsMode,
    ) -> Result<InterfaceSemanticDiagnosticsResult, ModelDiagnostics> {
        let (resolved, resolve_diagnostics) = match self.build_semantic_diagnostics_resolved(mode) {
            Ok(tree) => tree,
            Err(diags) => {
                record_interface_semantic_diagnostics_cache_miss();
                record_interface_semantic_diagnostics_build();
                return Err(global_resolution_failure_diagnostics(
                    self.session_source_map(),
                    diags.iter().cloned().collect(),
                ));
            }
        };

        if mode == SemanticDiagnosticsMode::Save {
            let target_resolve_diagnostics = self.save_mode_target_resolve_diagnostics(
                &resolved.0,
                &resolve_diagnostics,
                model_name,
            );
            if !target_resolve_diagnostics.is_empty() {
                record_interface_semantic_diagnostics_cache_miss();
                record_interface_semantic_diagnostics_build();
                return Err(model_diagnostics_for_tree(
                    &resolved.0,
                    target_resolve_diagnostics,
                ));
            }
        }

        let fingerprint = self.semantic_diagnostics_fingerprint(&resolved.0, mode, model_name);
        if let Some(class_type) =
            self.cached_interface_semantic_diagnostics(model_name, mode, fingerprint)
        {
            record_interface_semantic_diagnostics_cache_hit();
            return Ok(InterfaceSemanticDiagnosticsResult {
                resolved,
                fingerprint,
                class_type,
            });
        }

        record_interface_semantic_diagnostics_cache_miss();
        record_interface_semantic_diagnostics_build();
        let class_type = self.interface_semantic_diagnostics_class_type(model_name);
        self.insert_interface_semantic_diagnostics(
            model_name.to_string(),
            mode,
            fingerprint,
            class_type.clone(),
        );
        Ok(InterfaceSemanticDiagnosticsResult {
            resolved,
            fingerprint,
            class_type,
        })
    }

    fn body_semantic_diagnostics_query(
        &mut self,
        tree: &ast::ClassTree,
        model_name: &str,
        mode: SemanticDiagnosticsMode,
        fingerprint: Fingerprint,
    ) -> BodySemanticDiagnosticsResult {
        if let Some(cached) = self.cached_body_semantic_diagnostics(model_name, mode, fingerprint) {
            record_body_semantic_diagnostics_cache_hit();
            return cached;
        }

        record_body_semantic_diagnostics_cache_miss();
        record_body_semantic_diagnostics_build();
        let typed = self.typed_model_query(tree, mode.resolve_build_mode(), model_name);
        let outcome = build_model_diagnostics_for_typed_model(tree, model_name, typed);
        self.insert_body_semantic_diagnostics(
            model_name.to_string(),
            mode,
            fingerprint,
            outcome.diagnostics.clone(),
            outcome.blocks_model_stage,
        );
        outcome
    }

    fn model_stage_semantic_diagnostics_query(
        &mut self,
        tree: &ast::ClassTree,
        model_name: &str,
        mode: SemanticDiagnosticsMode,
        fingerprint: Fingerprint,
    ) -> ModelDiagnostics {
        if let Some(cached) =
            self.cached_model_stage_semantic_diagnostics(model_name, mode, fingerprint)
        {
            record_model_stage_semantic_diagnostics_cache_hit();
            return cached;
        }

        record_model_stage_semantic_diagnostics_cache_miss();
        record_model_stage_semantic_diagnostics_build();
        let dae = self.dae_model_query(tree, mode.resolve_build_mode(), model_name);
        let diagnostics = build_model_diagnostics_for_dae_model(tree, model_name, dae);
        self.insert_model_stage_semantic_diagnostics(
            model_name.to_string(),
            mode,
            fingerprint,
            diagnostics.clone(),
        );
        diagnostics
    }

    /// Query semantic diagnostics for a model using a phase-owned cache mode.
    pub fn semantic_diagnostics_query(
        &mut self,
        model_name: &str,
        mode: SemanticDiagnosticsMode,
    ) -> ModelDiagnostics {
        let interface = match self.interface_semantic_diagnostics_query(model_name, mode) {
            Ok(interface) => interface,
            Err(diags) => return diags,
        };

        let tree = &interface.resolved.0;
        let body =
            self.body_semantic_diagnostics_query(tree, model_name, mode, interface.fingerprint);
        if interface
            .class_type
            .as_ref()
            .is_some_and(|class_type| !is_simulatable_class_type(class_type))
        {
            return body.diagnostics;
        }
        if body.blocks_model_stage {
            return body.diagnostics;
        }

        let model_stage = self.model_stage_semantic_diagnostics_query(
            tree,
            model_name,
            mode,
            interface.fingerprint,
        );
        merge_model_diagnostics(body.diagnostics, model_stage)
    }

    /// Compile a model and collect all diagnostics from the first failing phase.
    pub fn compile_model_diagnostics(&mut self, model_name: &str) -> ModelDiagnostics {
        self.semantic_diagnostics_query(model_name, SemanticDiagnosticsMode::Standard)
    }
}

fn build_model_diagnostics_for_typed_model(
    tree: &ast::ClassTree,
    model_name: &str,
    typed: TypedModelOutcome,
) -> BodySemanticDiagnosticsResult {
    let mut collected = Vec::new();
    let model_span =
        class_primary_span(tree, model_name).unwrap_or_else(|| default_tree_span(&tree.source_map));
    let overlay = match typed {
        TypedModelOutcome::Success(overlay) => *overlay,
        TypedModelOutcome::NeedsInner {
            missing_inners,
            missing_spans,
            ..
        } => {
            let primary_span = missing_spans.first().copied().unwrap_or(model_span);
            let mut diag = CommonDiagnostic::error(
                "EI008",
                format!(
                    "model needs inner declarations: {}",
                    missing_inners.join(", ")
                ),
                PrimaryLabel::new(primary_span).with_message("missing matching `inner`"),
            );
            for (idx, span) in missing_spans.iter().enumerate().skip(1) {
                diag = diag.with_label(missing_inner_label(idx, *span));
            }
            collected.push(diag);
            return BodySemanticDiagnosticsResult {
                diagnostics: model_diagnostics_for_tree(tree, collected),
                blocks_model_stage: true,
            };
        }
        TypedModelOutcome::InstantiateError(error) => {
            collected.push(miette_error_to_common(
                &*error,
                model_span,
                &tree.source_map,
            ));
            return BodySemanticDiagnosticsResult {
                diagnostics: model_diagnostics_for_tree(tree, collected),
                blocks_model_stage: true,
            };
        }
        TypedModelOutcome::TypecheckError(diags) => {
            return BodySemanticDiagnosticsResult {
                diagnostics: model_diagnostics_for_tree(tree, diags),
                blocks_model_stage: true,
            };
        }
    };

    collected.extend(synthesized_inner_diagnostics(
        &overlay.synthesized_inners,
        model_span,
    ));

    BodySemanticDiagnosticsResult {
        diagnostics: model_diagnostics_for_tree(tree, collected),
        blocks_model_stage: false,
    }
}

fn build_model_diagnostics_for_dae_model(
    tree: &ast::ClassTree,
    model_name: &str,
    dae_outcome: DaeModelOutcome,
) -> ModelDiagnostics {
    let mut collected = Vec::new();
    let model_span =
        class_primary_span(tree, model_name).unwrap_or_else(|| default_tree_span(&tree.source_map));

    match dae_outcome {
        DaeModelOutcome::Success(_) => {}
        DaeModelOutcome::NeedsInner {
            missing_inners,
            missing_spans,
            ..
        } => {
            let primary_span = missing_spans.first().copied().unwrap_or(model_span);
            let mut diag = CommonDiagnostic::error(
                "EI008",
                format!(
                    "model needs inner declarations: {}",
                    missing_inners.join(", ")
                ),
                PrimaryLabel::new(primary_span).with_message("missing matching `inner`"),
            );
            for (idx, span) in missing_spans.iter().enumerate().skip(1) {
                diag = diag.with_label(missing_inner_label(idx, *span));
            }
            collected.push(diag);
            return model_diagnostics_for_tree(tree, collected);
        }
        DaeModelOutcome::InstantiateError(error) => {
            collected.push(miette_error_to_common(
                &*error,
                model_span,
                &tree.source_map,
            ));
            return model_diagnostics_for_tree(tree, collected);
        }
        DaeModelOutcome::TypecheckError(diags) => {
            return model_diagnostics_for_tree(tree, diags);
        }
        DaeModelOutcome::FlattenError { error } => {
            collected.push(miette_error_to_common(
                &*error,
                model_span,
                &tree.source_map,
            ));
            return model_diagnostics_for_tree(tree, collected);
        }
        DaeModelOutcome::ToDaeError { error } => {
            collected.push(miette_error_to_common(
                &*error,
                model_span,
                &tree.source_map,
            ));
            return model_diagnostics_for_tree(tree, collected);
        }
    }

    model_diagnostics_for_tree(tree, collected)
}

fn synthesized_inner_diagnostics(
    synthesized_inners: &[String],
    model_span: Span,
) -> Vec<CommonDiagnostic> {
    synthesized_inner_warning(
        synthesized_inners,
        PrimaryLabel::new(model_span).with_message("synthesized inner declaration"),
    )
    .into_iter()
    .collect()
}

fn resolve_diagnostic_in_target_files(
    tree: &ast::ClassTree,
    target_source_files: &IndexSet<String>,
    diagnostic: &CommonDiagnostic,
) -> bool {
    diagnostic.labels.iter().any(|label| {
        let Some((file_name, _)) = tree.source_map.get_source(label.span.source) else {
            return false;
        };
        target_source_files
            .iter()
            .any(|file| same_path(file, file_name))
    })
}
