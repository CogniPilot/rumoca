use super::*;

impl<'dae> Functions<'_, 'dae> {
    pub fn begin_loop(
        &mut self,
        parent: FunctionBody<'dae>,
        domain: DomainId<'dae>,
        targets: impl IntoIterator<Item = FunctionValueId<'dae>>,
        provenance: DaeProvenance,
    ) -> Result<FunctionLoop<'dae>, DaeConstructionError> {
        self.begin_loop_with_iteration_locals(parent, domain, targets, [], provenance)
    }

    /// Begin a compact loop whose transition owns nonescaping locals in
    /// addition to its explicit carried tuple.
    pub fn begin_loop_with_iteration_locals(
        &mut self,
        parent: FunctionBody<'dae>,
        domain: DomainId<'dae>,
        targets: impl IntoIterator<Item = FunctionValueId<'dae>>,
        iteration_locals: impl IntoIterator<Item = FunctionValueId<'dae>>,
        provenance: DaeProvenance,
    ) -> Result<FunctionLoop<'dae>, DaeConstructionError> {
        let (fold, body, state) =
            self.begin_loop_state(parent, domain, targets, iteration_locals, provenance)?;
        Ok(FunctionLoop {
            fold,
            domain,
            body,
            parents: Vec::new(),
            states: vec![state],
        })
    }

    /// Begin one lexically nested compact loop while retaining the enclosing
    /// fold capability. The child domain must name the active domain as its
    /// checked parent.
    pub fn begin_nested_loop(
        &mut self,
        parent: FunctionLoop<'dae>,
        domain: DomainId<'dae>,
        targets: impl IntoIterator<Item = FunctionValueId<'dae>>,
        provenance: DaeProvenance,
    ) -> Result<FunctionLoop<'dae>, DaeConstructionError> {
        self.begin_nested_loop_with_iteration_locals(parent, domain, targets, [], provenance)
    }

    pub fn begin_nested_loop_with_iteration_locals(
        &mut self,
        mut parent: FunctionLoop<'dae>,
        domain: DomainId<'dae>,
        targets: impl IntoIterator<Item = FunctionValueId<'dae>>,
        iteration_locals: impl IntoIterator<Item = FunctionValueId<'dae>>,
        provenance: DaeProvenance,
    ) -> Result<FunctionLoop<'dae>, DaeConstructionError> {
        let enclosing = EnclosingFunctionLoop {
            fold: parent.fold,
            domain: parent.domain,
        };
        let (fold, body, state) =
            self.begin_loop_state(parent.body, domain, targets, iteration_locals, provenance)?;
        parent.parents.push(enclosing);
        parent.states.push(state);
        parent.fold = fold;
        parent.domain = domain;
        parent.body = body;
        Ok(parent)
    }

    fn begin_loop_state(
        &mut self,
        mut parent: FunctionBody<'dae>,
        domain: DomainId<'dae>,
        targets: impl IntoIterator<Item = FunctionValueId<'dae>>,
        iteration_locals: impl IntoIterator<Item = FunctionValueId<'dae>>,
        provenance: DaeProvenance,
    ) -> Result<
        (
            FunctionFoldId<'dae>,
            FunctionBody<'dae>,
            FunctionLoopParent<'dae>,
        ),
        DaeConstructionError,
    > {
        check_provenance(self.source_map, provenance)?;
        let domain_entry = self
            .storage
            .domains
            .get(domain.index() as usize)
            .ok_or_else(|| unknown("domain", domain.index(), provenance))?;
        if domain_entry.parent != parent.domain.map(DomainId::index) {
            return Err(DaeConstructionError::InvalidBinderScope {
                expected_domain: parent.domain.map(DomainId::index),
                found_domain: domain.index(),
                span: provenance.span(),
            });
        }
        let CheckedFunctionLoopValues {
            targets,
            target_set,
            raw_targets,
            iteration_locals,
            iteration_local_set,
            raw_iteration_locals,
        } = check_function_loop_values(
            self.storage,
            &parent,
            targets,
            iteration_locals,
            provenance,
        )?;
        let build = function_build_state(self.storage, &parent);
        let initial_values = targets
            .iter()
            .map(|target| {
                build.current_values[target.ordinal() as usize].ok_or(
                    DaeConstructionError::IncompleteDefinition {
                        kind: "function loop initial value",
                        index: target.ordinal(),
                        span: provenance.span(),
                    },
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        let enclosing_fold = function_build_state(self.storage, &parent).active_fold;
        let fold = reserve_function_fold(
            self.storage,
            parent.function,
            FunctionFoldReservation {
                parent: enclosing_fold,
                domain: domain.index(),
                targets: raw_targets,
                iteration_locals: raw_iteration_locals,
                initial_values,
                provenance,
            },
        )?;
        let raw = function_fold_raw(self.storage, fold, provenance)?;
        let generated =
            DaeProvenance::generated(DaeGeneration::FunctionLoopLowering, provenance.span())?;
        let build = function_build_state_mut(self.storage, &parent);
        let state = FunctionLoopParent {
            domain: parent.domain,
            current_values: build.current_values.clone(),
            statements: std::mem::take(&mut build.statements),
            carried_targets: std::mem::replace(&mut build.carried_targets, target_set),
            iteration_local_targets: std::mem::replace(
                &mut build.iteration_local_targets,
                iteration_local_set,
            ),
            active_fold: build.active_fold.replace(fold.ordinal()),
        };
        for target in &iteration_locals {
            build.current_values[target.ordinal() as usize] = None;
        }
        parent.domain = Some(domain);
        // The entry parameters below are issued with this fold already open, so
        // they are recorded as its own region's definitions rather than the
        // enclosing scope's.
        for (carried, target) in targets.iter().enumerate() {
            let definition = next_function_definition_id(self.storage, parent.function, generated)?;
            let parameter = crate::expression::insert_function_fold_parameter(
                self.source_map,
                self.storage,
                fold,
                carried,
                definition,
                generated,
            )?;
            let inserted = insert_function_definition(self.storage, *target, parameter, generated)?;
            if inserted != definition {
                return Err(DaeConstructionError::ShapeMismatch {
                    span: generated.span(),
                });
            }
            self.storage.function_folds[raw as usize]
                .parameter_definitions
                .push(definition.ordinal());
            function_build_state_mut(self.storage, &parent).current_values
                [target.ordinal() as usize] = Some(definition.ordinal());
        }
        Ok((fold, parent, state))
    }

    pub fn assign_loop(
        &mut self,
        loop_body: &mut FunctionLoop<'dae>,
        target: FunctionValueId<'dae>,
        value: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        check_function_value_owner(loop_body.body.function, target, provenance)?;
        let carries_target = function_build_state(self.storage, &loop_body.body)
            .carried_targets
            .contains(&target.ordinal());
        let is_iteration_local = function_build_state(self.storage, &loop_body.body)
            .iteration_local_targets
            .contains(&target.ordinal());
        if !carries_target && !is_iteration_local {
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "function loop target",
                index: target.ordinal(),
                span: provenance.span(),
            });
        }
        self.assign_after_owner_checks(&mut loop_body.body, target, value, provenance)
    }

    /// Atomically commit a conditional transition to loop-carried values.
    pub fn assign_all_loop(
        &mut self,
        loop_body: &mut FunctionLoop<'dae>,
        assignments: &[(FunctionValueId<'dae>, ExprId<'dae>)],
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let build = function_build_state(self.storage, &loop_body.body);
        for (target, _) in assignments {
            check_function_value_owner(loop_body.body.function, *target, provenance)?;
            if !build.carried_targets.contains(&target.ordinal())
                && !build.iteration_local_targets.contains(&target.ordinal())
            {
                return Err(DaeConstructionError::IncompleteDefinition {
                    kind: "function loop target",
                    index: target.ordinal(),
                    span: provenance.span(),
                });
            }
        }
        self.assign_all(&mut loop_body.body, assignments, provenance)
    }

    pub fn finish_loop(
        &mut self,
        mut loop_body: FunctionLoop<'dae>,
        provenance: DaeProvenance,
    ) -> Result<FunctionBody<'dae>, DaeConstructionError> {
        if !loop_body.parents.is_empty() {
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "nested function loop",
                index: loop_body.fold.ordinal(),
                span: provenance.span(),
            });
        }
        self.finish_active_loop(&mut loop_body, provenance)?;
        Ok(loop_body.body)
    }

    pub fn finish_nested_loop(
        &mut self,
        mut loop_body: FunctionLoop<'dae>,
        provenance: DaeProvenance,
    ) -> Result<FunctionLoop<'dae>, DaeConstructionError> {
        let parent = loop_body
            .parents
            .pop()
            .ok_or(DaeConstructionError::IncompleteDefinition {
                kind: "enclosing function loop",
                index: loop_body.fold.ordinal(),
                span: provenance.span(),
            })?;
        self.finish_active_loop(&mut loop_body, provenance)?;
        loop_body.fold = parent.fold;
        loop_body.domain = parent.domain;
        Ok(loop_body)
    }

    fn finish_active_loop(
        &mut self,
        loop_body: &mut FunctionLoop<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let raw = function_fold_raw(self.storage, loop_body.fold, provenance)?;
        let (domain, targets) = {
            let entry = &self.storage.function_folds[raw as usize];
            if !entry.update_definitions.is_empty() {
                return Err(duplicate("function fold", raw, provenance));
            }
            (DomainId::from_raw(entry.domain), entry.targets.clone())
        };
        let Some(found_domain) = loop_body.body.domain else {
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "function loop domain",
                index: loop_body.fold.ordinal(),
                span: provenance.span(),
            });
        };
        if found_domain != domain {
            return Err(DaeConstructionError::InvalidBinderScope {
                expected_domain: Some(domain.index()),
                found_domain: found_domain.index(),
                span: provenance.span(),
            });
        }
        let updates = self.validated_loop_updates(&loop_body.body, domain, &targets, provenance)?;
        self.storage.function_folds[raw as usize].update_definitions = updates;
        let generated =
            DaeProvenance::generated(DaeGeneration::FunctionLoopLowering, provenance.span())?;
        let state = loop_body
            .states
            .pop()
            .ok_or(DaeConstructionError::IncompleteDefinition {
                kind: "function loop parent state",
                index: loop_body.fold.ordinal(),
                span: provenance.span(),
            })?;
        // The loop's result of each carried target is a value of the scope that
        // encloses the loop: it is what the statements after the loop read.
        // Closing the region before those definitions are issued is what makes
        // the recorded scope state that fact rather than assert it.
        function_build_state_mut(self.storage, &loop_body.body).active_fold = state.active_fold;
        function_build_state_mut(self.storage, &loop_body.body).current_values =
            state.current_values;
        for (carried, target) in targets.iter().enumerate() {
            let definition =
                next_function_definition_id(self.storage, loop_body.body.function, generated)?;
            let output = crate::expression::insert_function_fold_output(
                self.source_map,
                self.storage,
                loop_body.fold,
                carried,
                definition,
                generated,
            )?;
            let inserted = insert_function_definition(
                self.storage,
                FunctionValueId::from_raw(loop_body.body.function.index(), *target),
                output,
                generated,
            )?;
            if inserted != definition {
                return Err(DaeConstructionError::ShapeMismatch {
                    span: generated.span(),
                });
            }
            self.storage.function_folds[raw as usize]
                .output_definitions
                .push(definition.ordinal());
            function_build_state_mut(self.storage, &loop_body.body).current_values
                [*target as usize] = Some(definition.ordinal());
        }
        let build = function_build_state_mut(self.storage, &loop_body.body);
        let loop_statements = std::mem::take(&mut build.statements);
        build.statements = state.statements;
        build.carried_targets = state.carried_targets;
        build.iteration_local_targets = state.iteration_local_targets;
        build.statements.push(FunctionStatementWire::For {
            fold: loop_body.fold.ordinal(),
            statements: loop_statements,
            provenance,
        });
        loop_body.body.domain = state.domain;
        self.storage.unfilled_function_folds -= 1;
        Ok(())
    }

    fn validated_loop_updates(
        &mut self,
        body: &FunctionBody<'dae>,
        domain: DomainId<'dae>,
        targets: &[u32],
        provenance: DaeProvenance,
    ) -> Result<Vec<u32>, DaeConstructionError> {
        let build = function_build_state(self.storage, body);
        let updates = targets
            .iter()
            .map(|target| {
                build.current_values[*target as usize].ok_or(
                    DaeConstructionError::IncompleteDefinition {
                        kind: "function loop update",
                        index: *target,
                        span: provenance.span(),
                    },
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        for update in &updates {
            let definition = FunctionDefinitionId::from_raw(body.function.index(), *update);
            let update = ExprId::from_raw(
                function_definition_entry(self.storage, definition, provenance)?.rhs,
            );
            self.storage
                .expect_domain_expression(update, domain, provenance)?;
            match self.storage.expr_function_scope(update, provenance)? {
                None => {}
                Some(function) if function == body.function.index() => {}
                Some(function) => {
                    return Err(DaeConstructionError::InvalidFunctionScope {
                        expected_function: Some(body.function.index()),
                        found_function: function,
                        span: provenance.span(),
                    });
                }
            }
        }
        Ok(updates)
    }
}
