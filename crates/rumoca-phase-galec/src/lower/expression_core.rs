//! Scalar GALEC expression lowering and its construction-scoped state transitions.

use super::*;

fn expression_depth(expression: &gast::Expression) -> usize {
    match expression {
        gast::Expression::Bool(_)
        | gast::Expression::Integer(_)
        | gast::Expression::Real(_)
        | gast::Expression::Ref(_)
        | gast::Expression::Neg(_) => 1,
        gast::Expression::Size { dimension, .. }
        | gast::Expression::Paren(dimension)
        | gast::Expression::Not(dimension) => 1 + expression_depth(dimension),
        gast::Expression::Call(call) => {
            1 + call
                .arguments
                .iter()
                .map(expression_depth)
                .max()
                .unwrap_or(0)
        }
        gast::Expression::If(value) => {
            let branch_depth = value
                .branches
                .iter()
                .flat_map(|(condition, result)| [condition, result])
                .map(expression_depth)
                .max()
                .unwrap_or(0);
            1 + branch_depth.max(expression_depth(&value.else_value))
        }
        gast::Expression::Array(elements) => {
            1 + elements.iter().map(expression_depth).max().unwrap_or(0)
        }
        gast::Expression::Binary { lhs, rhs, .. } => {
            1 + expression_depth(lhs).max(expression_depth(rhs))
        }
    }
}

impl<'a, 'dae> ExpressionLowerer<'a, 'dae> {
    pub(super) fn new(
        view: dae::DaeView<'dae>,
        definitions: &'a rumoca_phase_structural::CausalDefinitions<'dae>,
        by_id: &'a HashMap<u32, ClassifiedVariable<'dae>>,
        pre_names: &'a HashMap<u32, gast::Name>,
        arithmetic: AlgorithmCodeArithmeticProfile,
    ) -> Self {
        Self {
            view,
            by_id,
            pre_names,
            definitions,
            call_frames: Vec::new(),
            function_fold_values: Vec::new(),
            function_fold_output_cache: HashMap::new(),
            scalar_projection_cache: HashMap::new(),
            function_fold_projection_cache: HashMap::new(),
            comprehension_frames: Vec::new(),
            iteration_points: Vec::new(),
            loop_index_bounds: Vec::new(),
            conditional_activation_path: Vec::new(),
            selection_points: Vec::new(),
            selection_epoch: 0,
            materialize_function_values: false,
            inline_causal_locals: false,
            arithmetic,
            conditional_depth: 0,
            materialized_function_values: HashMap::new(),
            materialized_function_calls: HashMap::new(),
            imported_materialized_call_regions: HashMap::new(),
            retained_call_dependencies: HashSet::new(),
            call_argument_read_captures: Vec::new(),
            materialized_shared_record_fields: HashMap::new(),
            array_update_index_locals: HashMap::new(),
            local_integer_bounds: LocalIntegerBounds::new(),
            assigned_primitive_expressions: AssignedPrimitives::default(),
            structural_function_locals: structural_locals::StructuralFunctionLocals::default(),
            called_user_functions: HashSet::new(),
            evaluated_root_call_actions: Vec::new(),
            expected_root_call_actions: HashSet::new(),
            reused_root_call_actions: Vec::new(),
            materialized_call_sources: HashMap::new(),
            evaluated_function_call_actions: Vec::new(),
            expected_function_call_actions: HashSet::new(),
            reused_function_call_actions: Vec::new(),
            function_scope: None,
            temporary_locals: Vec::new(),
            temporary_counter: 0,
            temporary_namespace: TemporaryNamespace::Value,
            capture_assertions: false,
            seen_assertion_calls: HashSet::new(),
            state_shapes_by_name: None,
            pending_prefix_statements: Vec::new(),
        }
    }

    pub(super) fn with_do_step_effects(
        view: dae::DaeView<'dae>,
        definitions: &'a rumoca_phase_structural::CausalDefinitions<'dae>,
        by_id: &'a HashMap<u32, ClassifiedVariable<'dae>>,
        pre_names: &'a HashMap<u32, gast::Name>,
        arithmetic: AlgorithmCodeArithmeticProfile,
    ) -> Self {
        Self {
            capture_assertions: true,
            materialize_function_values: true,
            ..Self::new(view, definitions, by_id, pre_names, arithmetic)
        }
    }

    #[cfg(test)]
    pub(super) fn with_assertions(
        view: dae::DaeView<'dae>,
        definitions: &'a rumoca_phase_structural::CausalDefinitions<'dae>,
        by_id: &'a HashMap<u32, ClassifiedVariable<'dae>>,
        pre_names: &'a HashMap<u32, gast::Name>,
        arithmetic: AlgorithmCodeArithmeticProfile,
    ) -> Self {
        Self {
            capture_assertions: true,
            ..Self::new(view, definitions, by_id, pre_names, arithmetic)
        }
    }

    /// Enter one exact point of a checked fold/comprehension owner.
    ///
    /// The id is interned rather than supplied by callers. Re-projecting a
    /// second field of the same point therefore shares the first field's call
    /// transaction, while another point cannot address that transaction. A
    /// non-literal runtime projection is compared as structured GALEC AST;
    /// generated loop names and enclosing point ids make shadowed/nested
    /// binders distinct without rendering or parsing text.
    pub(super) fn enter_iteration_point(
        &mut self,
        owner: IterationOwner,
        domain: u32,
        binders: Vec<gast::Expression>,
    ) -> Result<(), GalecTargetError> {
        let parent = self.iteration_path();
        let point = self
            .iteration_points
            .iter()
            .find(|candidate| {
                candidate.parent == parent
                    && candidate.owner == owner
                    && candidate.binders == binders
            })
            .map(|candidate| candidate.id)
            .map_or_else(
                || {
                    let ordinal = u32::try_from(self.iteration_points.len()).map_err(|_| {
                        GalecTargetError::LoweringInternal {
                            detail: "iteration-point identity capacity exceeded".to_owned(),
                        }
                    })?;
                    let id = IterationPointId(ordinal);
                    self.iteration_points.push(IterationPointIdentity {
                        id,
                        parent,
                        owner,
                        binders: binders.clone(),
                    });
                    Ok(id)
                },
                Ok,
            )?;
        self.comprehension_frames.push(ComprehensionFrame {
            domain,
            binders,
            point,
        });
        Ok(())
    }

    pub(super) fn leave_iteration_point(&mut self) {
        self.comprehension_frames
            .pop()
            .expect("iteration point was entered through this lowerer");
    }

    pub(super) fn iteration_path(&self) -> Vec<IterationPointId> {
        self.comprehension_frames
            .iter()
            .map(|frame| frame.point)
            .collect()
    }

    /// Issue the proof identity for one exact data-dependent selection.
    ///
    /// Candidate expression ids do not identify a selection: projecting the
    /// same array at `i` and `j` makes two independent runtime decisions. The
    /// interner therefore includes the checked operation owner, the complete
    /// enclosing iteration path, and structured (never rendered) projection
    /// coordinates. Re-lowering that exact decision gets the same capability;
    /// a different coordinate or semantic lowerer cannot address it.
    pub(super) fn selection_point(
        &mut self,
        kind: ConditionalActivationKind,
        expression: dae::ExprId<'dae>,
        operands: &[u32],
        coordinates: &[gast::Expression],
    ) -> Result<SelectionPointId, GalecTargetError> {
        let (kind, expression) = match kind {
            ConditionalActivationKind::ConditionalRecord
            | ConditionalActivationKind::FunctionConditional => {
                (ConditionalActivationKind::ConditionalScalar, None)
            }
            ConditionalActivationKind::ConditionalScalar => {
                (ConditionalActivationKind::ConditionalScalar, None)
            }
            kind => (kind, Some(expression.index())),
        };
        self.selection_point_inner(kind, expression, operands, coordinates)
    }

    pub(super) fn conditional_selection_point(
        &mut self,
        operands: &[u32],
        coordinates: &[gast::Expression],
    ) -> Result<SelectionPointId, GalecTargetError> {
        self.selection_point_inner(
            ConditionalActivationKind::ConditionalScalar,
            None,
            operands,
            coordinates,
        )
    }

    fn selection_point_inner(
        &mut self,
        kind: ConditionalActivationKind,
        expression: Option<u32>,
        operands: &[u32],
        coordinates: &[gast::Expression],
    ) -> Result<SelectionPointId, GalecTargetError> {
        // Scalar, record, and protected-function projections are views of the
        // same predicate. The statement epoch makes that correlation local to
        // one atomic evaluation, so a sequential store cannot reuse it.
        let kind = match kind {
            ConditionalActivationKind::ConditionalRecord
            | ConditionalActivationKind::FunctionConditional => {
                ConditionalActivationKind::ConditionalScalar
            }
            kind => kind,
        };
        let iteration_path = self.iteration_path();
        if let Some(point) = self.selection_points.iter().find(|point| {
            point.kind == kind
                && point.expression == expression
                && point.operands == operands
                && point.epoch == self.selection_epoch
                && point.iteration_path == iteration_path
                && point.coordinates == coordinates
        }) {
            return Ok(point.id);
        }
        let ordinal = u32::try_from(self.selection_points.len()).map_err(|_| {
            GalecTargetError::LoweringInternal {
                detail: "selection-point identity capacity exceeded".to_owned(),
            }
        })?;
        let namespace = match self.temporary_namespace {
            TemporaryNamespace::Value => SelectionPointNamespace::Value,
            TemporaryNamespace::Causal => SelectionPointNamespace::Causal,
            TemporaryNamespace::Clocked(clock) => SelectionPointNamespace::Clocked(clock.index()),
            TemporaryNamespace::Dependent(variable) => {
                SelectionPointNamespace::Dependent(variable.index())
            }
        };
        let id = SelectionPointId::issued(namespace, ordinal);
        self.selection_points.push(SelectionPointIdentity {
            id,
            kind,
            expression,
            operands: operands.to_vec(),
            epoch: self.selection_epoch,
            iteration_path,
            coordinates: coordinates.to_vec(),
        });
        Ok(id)
    }

    pub(super) fn shared_record_field_key(
        &self,
        expression: u32,
        field: usize,
    ) -> SharedRecordFieldKey {
        SharedRecordFieldKey {
            iteration_path: self.iteration_path(),
            expression,
            field,
        }
    }

    pub(super) fn array_update_index_key(&self, expression: u32) -> ArrayUpdateIndexKey {
        ArrayUpdateIndexKey {
            iteration_path: self.iteration_path(),
            expression,
        }
    }

    /// Finish one emitted statement group and sever every temporary cache
    /// whose initializer belongs to that group.
    ///
    /// A later scheduler may reorder statement groups. Keeping a cached local
    /// across this boundary would let the later group read a temporary whose
    /// defining assignment moved after the read.
    pub(super) fn take_prefix_statements(&mut self) -> Vec<gast::Spanned<gast::Statement>> {
        self.finish_statement_group();
        self.drain_prefix_statements()
    }

    pub(super) fn conditional_materialization_snapshot(
        &self,
    ) -> ConditionalMaterializationSnapshot {
        ConditionalMaterializationSnapshot {
            function_values: self.materialized_function_values.clone(),
            function_calls: self.materialized_function_calls.clone(),
            call_sources: self.materialized_call_sources.clone(),
            fold_outputs: self.function_fold_output_cache.clone(),
            seen_assertions: self.seen_assertion_calls.clone(),
            assigned_primitive_expressions: self.assigned_primitive_expressions.snapshot(),
        }
    }

    pub(super) fn restore_conditional_materialization(
        &mut self,
        snapshot: &ConditionalMaterializationSnapshot,
    ) {
        self.materialized_function_values
            .clone_from(&snapshot.function_values);
        self.materialized_function_calls
            .clone_from(&snapshot.function_calls);
        self.materialized_call_sources
            .clone_from(&snapshot.call_sources);
        self.function_fold_output_cache
            .clone_from(&snapshot.fold_outputs);
        self.seen_assertion_calls
            .clone_from(&snapshot.seen_assertions);
        self.assigned_primitive_expressions
            .restore(&snapshot.assigned_primitive_expressions);
        self.scalar_projection_cache.clear();
    }

    /// Copy only call facts whose emitted action is guarded by this exact
    /// construction-issued selection. A caller may restore an entry snapshot
    /// and republish these narrow capabilities for a correlated sibling in the
    /// same atomic statement epoch; condition-prefix facts are deliberately
    /// absent because they do not carry the selected-arm activation.
    pub(super) fn materialized_calls_guarded_by(
        &self,
        selection: SelectionPointId,
    ) -> Vec<(
        MaterializedFunctionCallKey,
        Vec<gast::Name>,
        HashSet<CallExecutionSource>,
    )> {
        self.materialized_function_calls
            .iter()
            .filter(|(key, _)| {
                key.activation_path
                    .iter()
                    .any(|activation| activation.selection == selection)
            })
            .filter_map(|(key, names)| {
                self.materialized_call_sources
                    .get(key)
                    .cloned()
                    .map(|sources| (key.clone(), names.clone(), sources))
            })
            .collect()
    }

    /// Name the values whose assigned locals survive a branch boundary, and
    /// return the previous naming so one group can restore it.
    pub(super) fn carry_assigned_primitives(&mut self, carried: HashSet<u32>) -> HashSet<u32> {
        self.assigned_primitive_expressions.carry(carried)
    }

    pub(super) fn drain_prefix_statements(&mut self) -> Vec<gast::Spanned<gast::Statement>> {
        std::mem::take(&mut self.pending_prefix_statements)
    }

    pub(super) fn finish_statement_group(&mut self) {
        self.advance_selection_epoch();
        self.materialized_function_values.clear();
        self.materialized_function_calls.clear();
        self.materialized_call_sources.clear();
        self.materialized_shared_record_fields.clear();
        self.array_update_index_locals.clear();
        self.function_fold_output_cache.clear();
        self.scalar_projection_cache.clear();
        self.seen_assertion_calls.clear();
    }

    /// Finish one sequential protected-function statement while retaining
    /// immutable call-result temporaries that dominate the next statement.
    ///
    /// Direct-to-function-local call emission is disabled in this scope, so
    /// every retained entry names storage declared in `temporary_locals` and
    /// cannot be overwritten by a later Modelica assignment.
    pub(super) fn finish_sequential_function_statement(&mut self) {
        self.advance_selection_epoch();
        self.materialized_function_values.clear();
        self.materialized_shared_record_fields.clear();
        self.array_update_index_locals.clear();
        self.function_fold_output_cache.clear();
        self.scalar_projection_cache.clear();
        self.seen_assertion_calls.clear();
    }

    fn advance_selection_epoch(&mut self) {
        self.selection_epoch = self
            .selection_epoch
            .checked_add(1)
            .expect("checked function statement count fits selection identity capacity");
    }

    pub(super) fn take_temporary_locals(&mut self) -> Vec<gast::VariableDeclaration> {
        std::mem::take(&mut self.temporary_locals)
    }

    /// Record the store of `expression` into `target`.
    pub(super) fn remember_primitive_assignment(&mut self, expression: u32, target: gast::Name) {
        self.assigned_primitive_expressions.remember(
            expression,
            target,
            &self.conditional_activation_path,
        );
        self.scalar_projection_cache.clear();
    }

    /// Record that `target` holds a joined value its branches already stored.
    pub(super) fn remember_joined_assignment(&mut self, expression: u32, target: gast::Name) {
        self.assigned_primitive_expressions.remember_joined(
            expression,
            target,
            &self.conditional_activation_path,
        );
        self.scalar_projection_cache.clear();
    }

    /// Declared shape of the classified block variable `name` refers to, if
    /// any. Backed by [`Self::state_shapes_by_name`], built here on first use.
    pub(super) fn state_shape(&mut self, name: &str) -> Option<(Vec<u32>, gast::ScalarType)> {
        self.state_shapes_by_name
            .get_or_insert_with(|| {
                self.by_id
                    .values()
                    .map(|classified| {
                        (
                            classified.name.lexeme().to_owned(),
                            (
                                classified.variable.value_type().dimensions().to_vec(),
                                classified.scalar_type,
                            ),
                        )
                    })
                    .collect()
            })
            .get(name)
            .cloned()
    }

    pub(super) fn take_called_user_functions(&mut self) -> HashSet<u32> {
        std::mem::take(&mut self.called_user_functions)
    }

    pub(super) fn take_evaluated_root_call_actions(&mut self) -> Vec<RootCallAction> {
        std::mem::take(&mut self.evaluated_root_call_actions)
    }

    pub(super) fn take_expected_root_call_actions(&mut self) -> HashSet<ExpectedRootCallAction> {
        std::mem::take(&mut self.expected_root_call_actions)
    }

    pub(super) fn take_reused_root_call_actions(&mut self) -> Vec<RootCallReuse> {
        std::mem::take(&mut self.reused_root_call_actions)
    }

    pub(super) fn take_evaluated_function_call_actions(&mut self) -> Vec<FunctionCallAction> {
        std::mem::take(&mut self.evaluated_function_call_actions)
    }

    pub(super) fn take_expected_function_call_actions(&mut self) -> HashSet<FunctionCallAction> {
        std::mem::take(&mut self.expected_function_call_actions)
    }

    pub(super) fn take_reused_function_call_actions(&mut self) -> Vec<FunctionCallReuse> {
        std::mem::take(&mut self.reused_function_call_actions)
    }

    pub(super) fn with_temporary_namespace(mut self, namespace: TemporaryNamespace<'dae>) -> Self {
        self.temporary_namespace = namespace;
        self
    }

    pub(super) fn with_structural_function_locals(
        mut self,
        locals: structural_locals::StructuralFunctionLocals<'dae>,
    ) -> Self {
        self.structural_function_locals = locals;
        self
    }

    pub(super) fn with_causal_inlining(mut self) -> Self {
        self.inline_causal_locals = true;
        self
    }

    pub(super) fn lower(
        &mut self,
        id: dae::ExprId<'dae>,
    ) -> Result<TypedExpression, GalecTargetError> {
        self.lower_at(id, &[])
    }

    pub(super) fn lower_element(
        &mut self,
        id: dae::ExprId<'dae>,
        indices: &[u32],
    ) -> Result<TypedExpression, GalecTargetError> {
        let indices = indices
            .iter()
            .map(|index| gast::Expression::Integer(i64::from(*index)))
            .collect::<Vec<_>>();
        self.lower_at(id, &indices)
    }

    pub(super) fn lower_at(
        &mut self,
        id: dae::ExprId<'dae>,
        indices: &[gast::Expression],
    ) -> Result<TypedExpression, GalecTargetError> {
        let cache_key = self.scalar_projection_key(id, indices);
        if let Some(key) = &cache_key
            && let Some(value) = self.scalar_projection_cache.get(key)
        {
            return Ok(value.clone());
        }
        let node = self.view.exact_expression(id);
        if node.value_type().dimensions().len() != indices.len() {
            return Err(unsupported(
                "array-projection",
                format!(
                    "expression rank {} cannot be projected with {} indices",
                    node.value_type().dimensions().len(),
                    indices.len()
                ),
                node.provenance().span(),
            ));
        }
        let scalar_type = scalar_type(
            node.value_type().scalar_type(),
            "<expression>",
            node.provenance().span(),
        )?;
        if let Some(name) = self
            .assigned_primitive_expressions
            .read(id.index(), &self.conditional_activation_path)
        {
            return Ok(TypedExpression {
                expression: self.lower_local_reference(
                    name,
                    node.value_type().dimensions(),
                    indices,
                    node.provenance().span(),
                )?,
                scalar_type,
            });
        }
        let value = self.lower_operation(id, node, indices, scalar_type)?;
        if let Some(key) = cache_key {
            self.scalar_projection_cache.insert(key, value.clone());
        }
        Ok(value)
    }

    fn scalar_projection_key(
        &self,
        expression: dae::ExprId<'dae>,
        indices: &[gast::Expression],
    ) -> Option<ScalarProjectionKey> {
        if !self.materialize_function_values
            || self.conditional_depth != 0
            || !self.comprehension_frames.is_empty()
            || !self.loop_index_bounds.is_empty()
            || !self.function_fold_values.is_empty()
        {
            return None;
        }
        Some(ScalarProjectionKey {
            call_path: self
                .call_frames
                .iter()
                .map(|frame| MaterializedCallKey {
                    owner: frame.owner.index(),
                    function: frame.function.index(),
                    arguments: frame
                        .arguments
                        .iter()
                        .map(|argument| argument.index())
                        .collect(),
                    indices: frame.indices.clone(),
                    iteration_path: self.iteration_path(),
                })
                .collect(),
            iteration_path: self.iteration_path(),
            expression: expression.index(),
            indices: indices
                .iter()
                .map(constant_integer)
                .collect::<Option<Vec<_>>>()?,
        })
    }

    fn lower_operation(
        &mut self,
        id: dae::ExprId<'dae>,
        node: dae::ExpressionView<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
    ) -> Result<TypedExpression, GalecTargetError> {
        let expression = match node.operation() {
            dae::ExpressionOperation::Literal(literal) => {
                lower_literal(literal, node.provenance().span())?
            }
            dae::ExpressionOperation::Coordinate(coordinate) => {
                return self.coordinate_at(coordinate, indices, node.provenance().span());
            }
            dae::ExpressionOperation::ClockTransfer { .. } => {
                return Err(unsupported(
                    "clock-transfer",
                    "cross-clock value transfer is not representable in scalar GALEC".to_owned(),
                    node.provenance().span(),
                ));
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                self.lower_unary_at(operator, operand, indices, node.provenance().span())?
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                return self.lower_binary_at(
                    operator,
                    lhs,
                    rhs,
                    indices,
                    scalar_type,
                    node.provenance().span(),
                );
            }
            dae::ExpressionOperation::Conditional(operands) => self.lower_conditional_at(
                id,
                operands,
                indices,
                scalar_type,
                node.provenance().span(),
            )?,
            dae::ExpressionOperation::Builtin { builtin, arguments } => {
                if matches!(builtin, dae::PureBuiltin::Sum | dae::PureBuiltin::Product) {
                    return self.lower_reduction(builtin, arguments, scalar_type);
                }
                if !indices.is_empty() {
                    return self.lower_elementwise_builtin(
                        id,
                        builtin,
                        arguments,
                        indices,
                        scalar_type,
                        node.provenance().span(),
                    );
                }
                lower_builtin(self, builtin, arguments, node.provenance().span())?
            }
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                return self.lower_function_value(definition, indices, scalar_type);
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                return self.lower_index_at(base, subscripts, indices, node.provenance().span());
            }
            dae::ExpressionOperation::Array(elements) => {
                return self.lower_array_at(id, elements, indices, node.provenance().span());
            }
            _ => return self.lower_aggregate_operation(id, node, indices, scalar_type),
        };
        self.bound_expression(
            TypedExpression {
                expression,
                scalar_type,
            },
            node.provenance().span(),
        )
    }

    pub(super) fn bound_expression(
        &mut self,
        value: TypedExpression,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        const MAX_INLINE_DEPTH: usize = 16;
        if !self.materialize_function_values
            || expression_depth(&value.expression) <= MAX_INLINE_DEPTH
        {
            return Ok(value);
        }
        let name = gast::Name::ident(format!(
            "rumoca_{}_expr_{}",
            self.temporary_namespace, self.temporary_counter
        ));
        self.temporary_counter += 1;
        self.temporary_locals.push(gast::VariableDeclaration {
            ty: gast::TypeRef::Primitive(value.scalar_type),
            name: name.clone(),
            dimensions: Vec::new(),
            range: gast::RangeAttributes::default(),
            span,
        });
        self.pending_prefix_statements.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: gast::Reference::local(name.clone()),
                value: value.expression,
            },
            span,
        ));
        Ok(TypedExpression {
            expression: gast::Expression::Ref(gast::Reference::local(name)),
            scalar_type: value.scalar_type,
        })
    }

    pub(super) fn lower_function_value(
        &mut self,
        definition: dae::FunctionDefinitionView<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
    ) -> Result<TypedExpression, GalecTargetError> {
        if self.uses_stored_function_value(definition) {
            let value = self.view.exact_function_value(definition.target());
            let value_type = self.view.exact_value_type(value.value_type());
            return Ok(TypedExpression {
                expression: self.lower_local_reference(
                    user_functions::value_name(value)?,
                    value_type.dimensions(),
                    indices,
                    definition.provenance().span(),
                )?,
                scalar_type,
            });
        }
        if self.function_scope == Some(definition.id().function()) {
            let value = self.lower_at(definition.rhs(), indices)?;
            return Ok(TypedExpression {
                expression: coerce(value, scalar_type, definition.provenance().span())?,
                scalar_type,
            });
        }
        let Some(key) = self.function_value_key(definition, indices, Vec::new()) else {
            return self.lower_at(definition.rhs(), indices);
        };
        if let Some(name) = self.materialized_function_values.get(&key) {
            return Ok(TypedExpression {
                expression: gast::Expression::Ref(gast::Reference::local(name.clone())),
                scalar_type,
            });
        }

        let value = self.lower_at(definition.rhs(), indices)?;
        self.store_materialized_function_value(
            key,
            value,
            scalar_type,
            definition.provenance().span(),
        )
    }

    pub(super) fn function_value_key(
        &self,
        definition: dae::FunctionDefinitionView<'dae>,
        indices: &[gast::Expression],
        fields: Vec<u32>,
    ) -> Option<MaterializedFunctionValueKey> {
        if !self.materialize_function_values
            || self.conditional_depth != 0
            || self.call_frames.is_empty()
        {
            return None;
        }
        Some(MaterializedFunctionValueKey {
            call_path: self
                .call_frames
                .iter()
                .map(|frame| MaterializedCallKey {
                    owner: frame.owner.index(),
                    function: frame.function.index(),
                    arguments: frame
                        .arguments
                        .iter()
                        .map(|argument| argument.index())
                        .collect(),
                    indices: frame.indices.clone(),
                    iteration_path: self.iteration_path(),
                })
                .collect(),
            iteration_path: self.iteration_path(),
            function: definition.id().function().index(),
            definition: definition.id().ordinal(),
            indices: indices
                .iter()
                .map(|index| match index {
                    gast::Expression::Integer(value) => Some(*value),
                    _ => None,
                })
                .collect::<Option<Vec<_>>>()?,
            fields,
        })
    }

    pub(super) fn store_materialized_function_value(
        &mut self,
        key: MaterializedFunctionValueKey,
        value: TypedExpression,
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let name = gast::Name::ident(format!(
            "rumoca_{}_value_{}",
            self.temporary_namespace, self.temporary_counter
        ));
        self.temporary_counter += 1;
        self.temporary_locals.push(gast::VariableDeclaration {
            ty: gast::TypeRef::Primitive(scalar_type),
            name: name.clone(),
            dimensions: Vec::new(),
            range: gast::RangeAttributes::default(),
            span,
        });
        self.pending_prefix_statements.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: gast::Reference::local(name.clone()),
                value: coerce(value, scalar_type, span)?,
            },
            span,
        ));
        self.materialized_function_values.insert(key, name.clone());
        Ok(TypedExpression {
            expression: gast::Expression::Ref(gast::Reference::local(name)),
            scalar_type,
        })
    }

    fn lower_aggregate_operation(
        &mut self,
        id: dae::ExprId<'dae>,
        node: dae::ExpressionView<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
    ) -> Result<TypedExpression, GalecTargetError> {
        match node.operation() {
            dae::ExpressionOperation::Range(range) => lower_range_at(
                range.start().value(),
                range.effective_step(),
                range.stop().value(),
                indices,
                scalar_type,
                node.provenance().span(),
            ),
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
                ..
            } => self.lower_call_at(
                id,
                function,
                output,
                arguments,
                indices,
                node.provenance().span(),
            ),
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => self.lower_array_update_at(
                id,
                base,
                value,
                subscripts,
                indices,
                node.provenance().span(),
            ),
            dae::ExpressionOperation::Comprehension { domain, body } => {
                self.lower_comprehension_at(id, domain, body, indices, node.provenance().span())
            }
            dae::ExpressionOperation::Field { base, field } => self.lower_record_field_at(
                base,
                field as usize,
                indices,
                scalar_type,
                node.provenance().span(),
            ),
            dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. } => self
                .lower_function_fold_parameter_at(fold, carried, indices, node.provenance().span()),
            dae::ExpressionOperation::FunctionFoldOutput { fold, carried, .. } => {
                self.lower_function_fold_output_at(fold, carried, indices, node.provenance().span())
            }
            dae::ExpressionOperation::Record(_)
            | dae::ExpressionOperation::StringConversion { .. } => Err(unsupported(
                "expression-form",
                format!(
                    "checked expression form {:?} is outside the scalar GALEC projection",
                    node.kind()
                ),
                node.provenance().span(),
            )),
            _ => unreachable!("ordinary scalar operation was lowered before aggregate dispatch"),
        }
    }
}

pub(super) fn conditional_activation_operands(operands: dae::ExpressionOperands<'_>) -> Vec<u32> {
    (0..operands.len().saturating_sub(1))
        .step_by(2)
        .map(|ordinal| {
            operands
                .get(ordinal)
                .expect("checked conditional condition")
                .index()
        })
        .collect()
}

pub(super) fn first_function_assertion(statements: dae::FunctionStatements<'_>) -> Option<Span> {
    for statement in statements {
        match statement {
            dae::FunctionStatementView::Assertion { provenance, .. } => {
                return Some(provenance.span());
            }
            dae::FunctionStatementView::For { statements, .. } => {
                if let Some(span) = first_function_assertion(statements) {
                    return Some(span);
                }
            }
            dae::FunctionStatementView::Assignment { .. }
            | dae::FunctionStatementView::AssignmentGroup { .. } => {}
        }
    }
    None
}
