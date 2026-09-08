use super::*;

#[must_use = "a prepared expression transaction must be committed exactly once"]
struct PreparedExpressionInstall<'a> {
    subject: &'a mut ExpressionSubject,
    capability: capabilities::PreparedExpressionCapabilities<'a>,
    fixed_shape_capability: shapes::PreparedFixedShapeFinalization<'a>,
    ty: Ty,
    fixed_shape: Option<FixedValueShape>,
    evaluated_literal: EvaluatedLiteral,
}

#[must_use = "a prepared declaration-start relation must be committed exactly once"]
struct PreparedDeclarationStartInstall<'a> {
    subject: &'a mut DeclarationSubject,
    capability: capabilities::PreparedDeclarationStartCapability<'a>,
    expression: ExpressionLoc,
    fixed_shape: FixedValueShape,
}

impl PreparedDeclarationStartInstall<'_> {
    fn commit(self) {
        self.capability.commit();
        self.subject.start = RequiredFact::Checked(DeclarationStartFact::Exact {
            expression: self.expression,
            fixed_shape: self.fixed_shape,
        });
    }
}

impl PreparedExpressionInstall<'_> {
    fn commit(self) {
        self.capability.commit();
        self.fixed_shape_capability.commit();
        self.subject.ty = RequiredFact::Checked(self.ty);
        self.subject.fixed_shape = RequiredFact::Checked(self.fixed_shape);
        self.subject.evaluated_literal = RequiredFact::Checked(self.evaluated_literal);
    }
}

#[must_use = "a prepared reference transaction must be committed exactly once"]
struct PreparedReferenceInstall<'a> {
    subject: &'a mut ReferenceSubject,
    capability: capabilities::PreparedReferenceCapabilities<'a>,
    fixed_shape_capability: shapes::PreparedFixedShapeFinalization<'a>,
    resolution: ReferenceResolution,
    fixed_shape: Option<FixedValueShape>,
}

impl PreparedReferenceInstall<'_> {
    fn commit(self) {
        self.capability.commit();
        self.fixed_shape_capability.commit();
        self.subject.resolution = RequiredFact::Checked(self.resolution);
        self.subject.fixed_shape = RequiredFact::Checked(self.fixed_shape);
    }
}

#[must_use = "a prepared resolution transaction must be committed exactly once"]
struct PreparedResolutionInstall<'a> {
    call: &'a mut CallSubject,
    builtin_arena: &'a mut Vec<BuiltinResultSubject>,
    capability: capabilities::PreparedCallResolutionCapabilities<'a>,
    prepared: PreparedCallResolution,
}

#[must_use = "a prepared unknown-call transaction must be committed exactly once"]
struct PreparedUnknownCallInstall<'a> {
    call: &'a mut CallSubject,
    capability: capabilities::PreparedCallResolutionCapabilities<'a>,
}

impl PreparedUnknownCallInstall<'_> {
    fn commit(self) {
        self.capability.commit();
        self.call.resolution = RequiredFact::Checked(CallResolutionFact::Unknown);
    }
}

impl PreparedResolutionInstall<'_> {
    fn commit(mut self) {
        self.capability.commit();
        self.builtin_arena
            .append(&mut self.prepared.builtin_results);
        self.call.resolution =
            RequiredFact::Checked(CallResolutionFact::Known(self.prepared.resolution));
    }
}

#[must_use = "a prepared call-graph transaction must be committed exactly once"]
struct PreparedUserGraphInstall<'a> {
    destination: &'a mut RequiredFact<UserCallGraph>,
    capability: capabilities::PreparedUserCallGraph<'a>,
    graph: UserCallGraph,
}

#[must_use = "a prepared call-result transaction must be committed exactly once"]
struct PreparedCallResultsInstall<'a> {
    call: &'a mut CallSubject,
    projection_arena: &'a mut Vec<CallResultProjectionSubject>,
    capability: capabilities::PreparedCallResultsCapabilities<'a>,
    fixed_shape_capability: shapes::PreparedFixedShapeFinalization<'a>,
    batch: CallResultProjectionBatch,
}

#[must_use = "a prepared projection batch must be installed or discarded as a whole"]
struct CallResultProjectionBatch {
    projections: Vec<CallResultProjectionSubject>,
    children: Vec<(SubjectLoc, ChildRole)>,
    count: u32,
    shape_failure: Option<UnprovenValueShape>,
}

impl PreparedCallResultsInstall<'_> {
    fn commit(mut self) {
        self.capability.commit();
        self.fixed_shape_capability.commit();
        self.projection_arena.append(&mut self.batch.projections);
        self.call.children.append(&mut self.batch.children);
        self.call.result_set = RequiredFact::Checked(ClosedCallResults {
            count: self.batch.count,
        });
    }
}

impl PreparedUserGraphInstall<'_> {
    fn commit(self) {
        self.capability.commit();
        *self.destination = RequiredFact::Checked(self.graph);
    }
}

impl RetainedValidationBuilder {
    pub(in crate::validate) fn install(
        block: &Block,
        declaration_starts: super::super::DeclarationStartContract,
        arithmetic: Option<crate::package::AlgorithmCodeArithmeticProfile>,
    ) -> Result<Self, RetainedValidationError> {
        let (retained, lookup) =
            topology::TopologyPlan::construct(block, declaration_starts, arithmetic)?.into_parts();
        let capabilities = capabilities::ConstructionCapabilities::from_topology(&retained)?;
        let fixed_shapes = shapes::FixedShapeCapabilities::from_topology(&retained)?;
        let builder = Self {
            retained,
            lookup,
            capabilities,
            fixed_shapes,
            reservations: ReservationGate::default(),
        };
        Ok(builder)
    }

    pub(in crate::validate) fn expression_type(&self, expression: &Expression) -> Option<Ty> {
        let loc = self
            .lookup
            .expressions
            .get(&std::ptr::from_ref(expression))?;
        self.retained.expressions[loc.0 as usize]
            .ty
            .checked()
            .copied()
    }

    pub(in crate::validate) fn record_expression_type(
        &mut self,
        expression: &Expression,
        ty: Ty,
    ) -> Result<(), ShapeRelationError> {
        let loc = *self
            .lookup
            .expressions
            .get(&std::ptr::from_ref(expression))
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        let fixed_shape = self.fixed_expression_shape(expression, ty, loc.0 as usize)?;
        let evaluated_literal = self.evaluated_literal(expression)?;
        if !matches!(
            self.retained.expressions[loc.0 as usize].ty,
            RequiredFact::Pending
        ) || !matches!(
            self.retained.expressions[loc.0 as usize].fixed_shape,
            RequiredFact::Pending
        ) || !matches!(
            self.retained.expressions[loc.0 as usize].evaluated_literal,
            RequiredFact::Pending
        ) {
            return Err(RetainedValidationError::DuplicateFact {
                family: "expression-semantics",
                index: loc.0,
            }
            .into());
        }
        let (retained, capabilities, fixed_shapes) = (
            &mut self.retained,
            &mut self.capabilities,
            &mut self.fixed_shapes,
        );
        let subject = retained
            .expressions
            .get_mut(loc.0 as usize)
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        let capability = capabilities.prepare_expression(loc.0)?;
        let fixed_shape_capability =
            fixed_shapes.prepare_expression(loc.0, &fixed_shape, subject.provenance)?;
        PreparedExpressionInstall {
            subject,
            capability,
            fixed_shape_capability,
            ty,
            fixed_shape,
            evaluated_literal,
        }
        .commit();
        Ok(())
    }

    fn evaluated_literal(
        &self,
        expression: &Expression,
    ) -> Result<EvaluatedLiteral, RetainedValidationError> {
        let scalar = match expression {
            Expression::Bool(value) => {
                return Ok(EvaluatedLiteral::Scalar(EvaluatedScalar::Boolean(*value)));
            }
            Expression::Integer(value) => EvaluatedScalar::Integer(*value),
            Expression::Real(value) => EvaluatedScalar::RealBits(value.to_bits()),
            Expression::Array(elements) => return self.uniform_tensor_literal(elements),
            Expression::Ref(_)
            | Expression::Size { .. }
            | Expression::Call(_)
            | Expression::Paren(_)
            | Expression::If(_)
            | Expression::Neg(_)
            | Expression::Not(_)
            | Expression::Binary { .. } => return Ok(EvaluatedLiteral::Symbolic),
        };
        Ok(EvaluatedLiteral::Scalar(scalar))
    }

    fn uniform_tensor_literal(
        &self,
        elements: &[Expression],
    ) -> Result<EvaluatedLiteral, RetainedValidationError> {
        let mut expected = None;
        for element in elements {
            let locator = self
                .lookup
                .expressions
                .get(&std::ptr::from_ref(element))
                .ok_or(RetainedValidationError::MissingResolvedSubject)?;
            let literal = self.retained.expressions[locator.0 as usize]
                .evaluated_literal
                .checked()
                .copied()
                .ok_or(RetainedValidationError::MissingFact {
                    family: "expression-evaluated-literal",
                    index: locator.0,
                })?;
            let scalar = match literal {
                EvaluatedLiteral::Scalar(scalar) | EvaluatedLiteral::UniformTensorFill(scalar) => {
                    scalar
                }
                EvaluatedLiteral::NonUniformTensor => {
                    return Ok(EvaluatedLiteral::NonUniformTensor);
                }
                EvaluatedLiteral::Symbolic => return Ok(EvaluatedLiteral::Symbolic),
            };
            if expected.is_some_and(|expected| expected != scalar) {
                return Ok(EvaluatedLiteral::NonUniformTensor);
            }
            expected = Some(scalar);
        }
        Ok(expected.map_or(
            EvaluatedLiteral::Symbolic,
            EvaluatedLiteral::UniformTensorFill,
        ))
    }

    pub(in crate::validate) fn record_declaration_start(
        &mut self,
        declaration: &VariableDeclaration,
        start: &Expression,
    ) -> Result<(), DeclarationStartRelationError> {
        let declaration_loc = self.declaration_loc(declaration)?;
        let expression_loc = *self
            .lookup
            .expressions
            .get(&std::ptr::from_ref(start))
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        let expression = self
            .retained
            .expressions
            .get(expression_loc.0 as usize)
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        if expression.parent
            != SubjectParent::child(
                SubjectLoc::Declaration(declaration_loc),
                ChildRole::DeclarationStart,
            )
        {
            return Err(RetainedValidationError::InconsistentFact {
                family: "declaration-start-parent",
                index: expression_loc.0,
            }
            .into());
        }
        let expected_type = Ty::of_decl(declaration);
        let found_type =
            expression
                .ty
                .checked()
                .copied()
                .ok_or(RetainedValidationError::MissingFact {
                    family: "expression-type",
                    index: expression_loc.0,
                })?;
        if expected_type != found_type {
            return Err(DeclarationStartRelationError::TypeMismatch {
                expected: expected_type,
                found: found_type,
            });
        }
        let expected_shape = self
            .retained
            .declarations
            .get(declaration_loc.0 as usize)
            .and_then(|subject| subject.fixed_shape.as_ref());
        let found_shape = expression
            .fixed_shape
            .checked()
            .ok_or(RetainedValidationError::MissingFact {
                family: "expression-fixed-shape",
                index: expression_loc.0,
            })?
            .as_ref();
        let (Some(expected_shape), Some(found_shape)) = (expected_shape, found_shape) else {
            return Err(DeclarationStartRelationError::UnprovenShape);
        };
        if expected_shape != found_shape {
            return Err(DeclarationStartRelationError::ShapeMismatch);
        }
        let fixed_shape = expected_shape.clone();
        let (retained, capabilities) = (&mut self.retained, &mut self.capabilities);
        let subject = retained
            .declarations
            .get_mut(declaration_loc.0 as usize)
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        if !matches!(subject.start, RequiredFact::Pending) {
            return Err(RetainedValidationError::DuplicateFact {
                family: "declaration-start",
                index: declaration_loc.0,
            }
            .into());
        }
        PreparedDeclarationStartInstall {
            subject,
            capability: capabilities.prepare_declaration_start(declaration_loc.0)?,
            expression: expression_loc,
            fixed_shape,
        }
        .commit();
        Ok(())
    }

    pub(in crate::validate) fn record_reference_resolution(
        &mut self,
        reference: &Reference,
        resolved: &ResolvedRef<'_>,
    ) -> Result<(), RetainedValidationError> {
        let loc = *self
            .lookup
            .references
            .get(&std::ptr::from_ref(reference))
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        let target = match resolved.target {
            Resolved::Entity { decl, .. }
            | Resolved::Component { decl }
            | Resolved::Local(decl) => ResolvedTarget::Declaration(self.declaration_loc(decl)?),
            Resolved::Parameter(parameter) => {
                ResolvedTarget::Declaration(self.declaration_loc(&parameter.decl)?)
            }
            Resolved::Iterator(iterator) => ResolvedTarget::Binder(
                *self
                    .lookup
                    .binders
                    .get(&std::ptr::from_ref(iterator))
                    .ok_or(RetainedValidationError::MissingResolvedSubject)?,
            ),
        };
        let resolution = ReferenceResolution { target };
        let fixed_shape = fixed_reference_shape(resolved);
        self.require_reference_target(resolution.target, loc.0 as usize)?;
        if !matches!(
            self.retained.references[loc.0 as usize].resolution,
            RequiredFact::Pending
        ) || !matches!(
            self.retained.references[loc.0 as usize].fixed_shape,
            RequiredFact::Pending
        ) {
            return Err(RetainedValidationError::DuplicateFact {
                family: "reference-semantics",
                index: loc.0,
            });
        }
        let (retained, capabilities, fixed_shapes) = (
            &mut self.retained,
            &mut self.capabilities,
            &mut self.fixed_shapes,
        );
        let subject = retained
            .references
            .get_mut(loc.0 as usize)
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        let capability = capabilities.prepare_reference(loc.0)?;
        let fixed_shape_capability =
            fixed_shapes.prepare_reference(loc.0, &fixed_shape, subject.provenance)?;
        PreparedReferenceInstall {
            subject,
            capability,
            fixed_shape_capability,
            resolution,
            fixed_shape,
        }
        .commit();
        Ok(())
    }

    pub(in crate::validate) fn record_call_resolution(
        &mut self,
        resolved: &ResolvedCall<'_>,
    ) -> Result<(), RetainedValidationError> {
        let call = resolved.occurrence();
        let callee = resolved.callee();
        let loc = *self
            .lookup
            .calls
            .get(&std::ptr::from_ref(call))
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        let call_index = loc.0 as usize;
        let stateful = callee.is_stateful();
        let mut prepared_builtin_results = Vec::new();
        let (target, results, inputs, outputs) = match callee {
            Callee::User(function) => {
                let function_loc = *self
                    .lookup
                    .functions
                    .get(&std::ptr::from_ref(*function))
                    .ok_or(RetainedValidationError::MissingResolvedSubject)?;
                let output_count = function
                    .parameters
                    .iter()
                    .filter(|parameter| parameter.direction == Direction::Output)
                    .count();
                let mut results = Vec::new();
                results.try_reserve(output_count).map_err(|_| {
                    RetainedValidationError::AllocationFailed {
                        family: "call-results",
                    }
                })?;
                for parameter in function
                    .parameters
                    .iter()
                    .filter(|parameter| parameter.direction == Direction::Output)
                {
                    results.push(CalleeResultLoc::UserOutput(
                        self.declaration_loc(&parameter.decl)?,
                    ));
                }
                let (inputs, outputs) = user_call_signature(function)?;
                (CallTarget::Function(function_loc), results, inputs, outputs)
            }
            Callee::Builtin(builtin) => {
                let base = builtin_loc(builtin)?;
                let canonical = &crate::builtins::BUILTINS[base.index()];
                let (inputs, outputs) = builtin_call_signature(canonical, 0)?;
                let (subjects, results) =
                    self.prepare_builtin_results(base, 0, canonical.outputs.len())?;
                prepared_builtin_results = subjects;
                (
                    CallTarget::Builtin {
                        base,
                        lifted_rank: 0,
                    },
                    results,
                    inputs,
                    outputs,
                )
            }
            Callee::Lifted { base, rank } => {
                let base_loc = builtin_loc(base)?;
                let lifted_rank =
                    u8::try_from(*rank).map_err(|_| RetainedValidationError::LocatorOverflow)?;
                let canonical = &crate::builtins::BUILTINS[base_loc.index()];
                if !(1..=2).contains(&lifted_rank) || !canonical.is_lifted() {
                    return inconsistent("call-target", call_index);
                }
                let (inputs, outputs) = builtin_call_signature(canonical, lifted_rank)?;
                let (subjects, results) =
                    self.prepare_builtin_results(base_loc, lifted_rank, canonical.outputs.len())?;
                prepared_builtin_results = subjects;
                (
                    CallTarget::Builtin {
                        base: base_loc,
                        lifted_rank,
                    },
                    results,
                    inputs,
                    outputs,
                )
            }
        };
        let resolution = CallResolution {
            target,
            inputs,
            outputs,
            results,
            stateful,
        };
        self.commit_prepared_call_resolution(
            call_index,
            PreparedCallResolution {
                resolution,
                builtin_results: prepared_builtin_results,
            },
        )
    }

    pub(in crate::validate) fn record_unknown_call(
        &mut self,
        call: &FunctionCall,
    ) -> Result<(), RetainedValidationError> {
        let loc = *self
            .lookup
            .calls
            .get(&std::ptr::from_ref(call))
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        let (retained, capabilities) = (&mut self.retained, &mut self.capabilities);
        let subject = retained
            .calls
            .get_mut(loc.0 as usize)
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        let capability = capabilities.prepare_call_resolution(loc.0, 0, 0, false)?;
        PreparedUnknownCallInstall {
            call: subject,
            capability,
        }
        .commit();
        Ok(())
    }

    pub(in crate::validate::retained) fn prepare_builtin_results(
        &self,
        base: BuiltinLoc,
        lifted_rank: u8,
        count: usize,
    ) -> Result<(Vec<BuiltinResultSubject>, Vec<CalleeResultLoc>), RetainedValidationError> {
        if count > usize::from(u16::MAX) + 1 {
            return Err(RetainedValidationError::LocatorOverflow);
        }
        let start = self.retained.builtin_results.len();
        start
            .checked_add(count)
            .and_then(|end| u32::try_from(end).ok())
            .ok_or(RetainedValidationError::LocatorOverflow)?;
        let mut subjects = Vec::new();
        subjects
            .try_reserve(count)
            .map_err(|_| RetainedValidationError::AllocationFailed {
                family: "builtin-result-subjects",
            })?;
        let mut results = Vec::new();
        results
            .try_reserve(count)
            .map_err(|_| RetainedValidationError::AllocationFailed {
                family: "builtin-result-locators",
            })?;
        for output in 0..count {
            let locator = BuiltinResultLoc(to_u32(start + output)?);
            subjects.push(BuiltinResultSubject {
                base,
                lifted_rank,
                output: u16::try_from(output)
                    .map_err(|_| RetainedValidationError::LocatorOverflow)?,
            });
            results.push(CalleeResultLoc::BuiltinOutput(locator));
        }
        Ok((subjects, results))
    }

    pub(in crate::validate::retained) fn commit_prepared_call_resolution(
        &mut self,
        call_index: usize,
        prepared: PreparedCallResolution,
    ) -> Result<(), RetainedValidationError> {
        let call = self
            .retained
            .calls
            .get(call_index)
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        if !matches!(call.resolution, RequiredFact::Pending) {
            return Err(RetainedValidationError::DuplicateFact {
                family: "call-resolution",
                index: to_u32(call_index)?,
            });
        }
        let call_locator = to_u32(call_index)?;
        let result_count = prepared.resolution.results.len();
        let builtin_result_count = prepared.builtin_results.len();
        self.require_call_resolution(&prepared.resolution, call_index)?;
        self.require_prepared_builtin_results(&prepared, call_index)?;
        self.retained
            .builtin_results
            .try_reserve(prepared.builtin_results.len())
            .map_err(|_| RetainedValidationError::AllocationFailed {
                family: "builtin-result-arena",
            })?;

        let (retained, capabilities) = (&mut self.retained, &mut self.capabilities);
        let call = retained
            .calls
            .get_mut(call_index)
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        let capability = capabilities.prepare_call_resolution(
            call_locator,
            builtin_result_count,
            result_count,
            true,
        )?;
        PreparedResolutionInstall {
            call,
            builtin_arena: &mut retained.builtin_results,
            capability,
            prepared,
        }
        .commit();
        Ok(())
    }

    pub(in crate::validate) fn require_assignment_shape(
        &self,
        target: &Reference,
        value: &Expression,
    ) -> Result<(), ShapeRelationError> {
        let target = self.fixed_reference_shape_for(target)?;
        let value = self.fixed_shape_for_expression(value)?;
        if let (Some(target), Some(value)) = (target, value)
            && target != value
        {
            return Err(ShapeRelationError::Mismatch {
                context: "assignment fixed dimensions",
            });
        }
        Ok(())
    }

    pub(in crate::validate) fn close_expression_call_results(
        &mut self,
        call: &FunctionCall,
    ) -> Result<(), ShapeRelationError> {
        self.close_call_results(call, ResultReceiverKind::Expression)
    }

    pub(in crate::validate) fn close_multi_assignment_call_results(
        &mut self,
        call: &FunctionCall,
    ) -> Result<(), ShapeRelationError> {
        self.close_call_results(call, ResultReceiverKind::MultiAssignment)
    }

    pub(in crate::validate) fn close_discarded_call_results(
        &mut self,
        call: &FunctionCall,
    ) -> Result<(), ShapeRelationError> {
        self.close_call_results(call, ResultReceiverKind::Discarded)
    }

    fn close_call_results(
        &mut self,
        call: &FunctionCall,
        expected_use: ResultReceiverKind,
    ) -> Result<(), ShapeRelationError> {
        let call_loc = *self
            .lookup
            .calls
            .get(&std::ptr::from_ref(call))
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        let call_index = call_loc.0 as usize;
        let subject = self
            .retained
            .calls
            .get(call_index)
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        if !matches!(subject.result_set, RequiredFact::Pending) {
            return Err(RetainedValidationError::DuplicateFact {
                family: "call-result-set",
                index: call_loc.0,
            }
            .into());
        }
        let resolution = match &subject.resolution {
            RequiredFact::Checked(CallResolutionFact::Known(resolution)) => resolution.clone(),
            RequiredFact::Checked(CallResolutionFact::Unknown) => {
                return inconsistent_shape("call-result-resolution", call_index);
            }
            RequiredFact::Pending => {
                return Err(RetainedValidationError::MissingFact {
                    family: "call-resolution",
                    index: call_loc.0,
                }
                .into());
            }
            RequiredFact::NotChecked(_) => {
                return Err(RetainedValidationError::UncheckedFact {
                    family: "call-resolution",
                    index: call_loc.0,
                }
                .into());
            }
        };
        let results = &resolution.results;
        if resolution.outputs.len() != results.len() {
            return inconsistent_shape("call-result-origin", call_index);
        }
        let receivers =
            self.call_result_receivers(call_loc, expected_use, resolution.outputs.len())?;
        if receivers.len() != resolution.outputs.len() {
            return inconsistent_shape("call-result-cardinality", call_index);
        }
        let start = self.retained.call_result_projections.len();
        start
            .checked_add(resolution.outputs.len())
            .and_then(|end| u32::try_from(end).ok())
            .ok_or(RetainedValidationError::LocatorOverflow)?;
        let values = self.fixed_call_result_shapes(call_loc, &resolution, results)?;
        if values.len() != resolution.outputs.len() {
            return inconsistent_shape("call-result-shape-cardinality", call_index);
        }

        for (receiver, value) in receivers.iter().copied().zip(&values) {
            self.require_call_result_receiver_shape(receiver, value)?;
        }

        let batch = self.prepare_call_result_projection_batch(
            call_loc,
            start,
            &resolution,
            receivers,
            values,
        )?;
        self.reserve_call_result_destinations(call_index, batch.projections.len())?;
        let (retained, capabilities, fixed_shapes) = (
            &mut self.retained,
            &mut self.capabilities,
            &mut self.fixed_shapes,
        );
        let call = retained
            .calls
            .get_mut(call_index)
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        let capability = capabilities.prepare_call_results(call_loc.0, batch.projections.len())?;
        let fixed_shape_capability =
            fixed_shapes.prepare_call_results(call_loc.0, batch.shape_failure)?;
        PreparedCallResultsInstall {
            call,
            projection_arena: &mut retained.call_result_projections,
            capability,
            fixed_shape_capability,
            batch,
        }
        .commit();
        Ok(())
    }

    fn prepare_call_result_projection_batch(
        &mut self,
        call: CallLoc,
        start: usize,
        resolution: &CallResolution,
        receivers: Vec<CallResultReceiverLoc>,
        values: Vec<Option<FixedValueShape>>,
    ) -> Result<CallResultProjectionBatch, RetainedValidationError> {
        let count = resolution.outputs.len();
        self.reservations
            .pass("call-result-projection-transaction")?;
        let mut projections = Vec::new();
        projections
            .try_reserve(count)
            .map_err(|_| RetainedValidationError::AllocationFailed {
                family: "call-result-projection-transaction",
            })?;
        self.reservations.pass("call-result-child-transaction")?;
        let mut children = Vec::new();
        children
            .try_reserve(count)
            .map_err(|_| RetainedValidationError::AllocationFailed {
                family: "call-result-child-transaction",
            })?;
        for (order, ((receiver, callee_result), value)) in receivers
            .into_iter()
            .zip(resolution.results.iter().copied())
            .zip(values)
            .enumerate()
        {
            let order = to_u32(order)?;
            let locator = CallResultProjectionLoc(to_u32(start + order as usize)?);
            let role = ChildRole::CallResult(order);
            projections.push(CallResultProjectionSubject {
                parent: SubjectParent::child(SubjectLoc::Call(call), role),
                provenance: self.call_result_receiver_provenance(receiver)?,
                call,
                callee_result,
                receiver,
                value,
                order,
            });
            children.push((SubjectLoc::CallResultProjection(locator), role));
        }
        let shape_failure = projections
            .iter()
            .find(|projection| projection.value.is_none())
            .map(|projection| UnprovenValueShape {
                subject: "call-result projection",
                provenance: projection.provenance,
            });
        Ok(CallResultProjectionBatch {
            projections,
            children,
            count: to_u32(count)?,
            shape_failure,
        })
    }

    fn reserve_call_result_destinations(
        &mut self,
        call_index: usize,
        count: usize,
    ) -> Result<(), RetainedValidationError> {
        self.reservations.pass("call-result-projection-arena")?;
        self.retained
            .call_result_projections
            .try_reserve(count)
            .map_err(|_| RetainedValidationError::AllocationFailed {
                family: "call-result-projection-arena",
            })?;
        self.reservations.pass("call-result-child-arena")?;
        self.retained.calls[call_index]
            .children
            .try_reserve(count)
            .map_err(|_| RetainedValidationError::AllocationFailed {
                family: "call-result-child-arena",
            })?;
        Ok(())
    }

    fn call_result_receiver_provenance(
        &self,
        receiver: CallResultReceiverLoc,
    ) -> Result<SubjectProvenance, RetainedValidationError> {
        let subject = match receiver {
            CallResultReceiverLoc::ExpressionValue(loc) => SubjectLoc::Expression(loc),
            CallResultReceiverLoc::MultiAssignmentDestination(loc) => SubjectLoc::Reference(loc),
            CallResultReceiverLoc::DiscardedBy(loc) => SubjectLoc::Statement(loc),
        };
        self.retained.subject_provenance(subject)
    }

    fn require_call_result_receiver_shape(
        &self,
        receiver: CallResultReceiverLoc,
        value: &Option<FixedValueShape>,
    ) -> Result<(), ShapeRelationError> {
        let CallResultReceiverLoc::MultiAssignmentDestination(reference) = receiver else {
            return Ok(());
        };
        let destination = self
            .retained
            .references
            .get(reference.0 as usize)
            .and_then(|subject| subject.fixed_shape.checked())
            .ok_or(RetainedValidationError::MissingFact {
                family: "reference-fixed-shape",
                index: reference.0,
            })?;
        if let (Some(value), Some(destination)) = (value, destination)
            && value != destination
        {
            return Err(ShapeRelationError::Mismatch {
                context: "call-result destination fixed dimensions",
            });
        }
        Ok(())
    }

    /// Project the one call-resolution fact established by type analysis.
    /// `None` means that the type checker already diagnosed an unknown
    /// callee; all other consumers skip that invalid occurrence rather than
    /// resolving its spelling a second time or duplicating EG015.
    pub(in crate::validate) fn call_resolution(
        &self,
        call: &FunctionCall,
    ) -> Result<Option<&CallResolution>, RetainedValidationError> {
        let loc = *self
            .lookup
            .calls
            .get(&std::ptr::from_ref(call))
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        match &self.retained.calls[loc.0 as usize].resolution {
            RequiredFact::Checked(CallResolutionFact::Known(resolution)) => Ok(Some(resolution)),
            RequiredFact::Checked(CallResolutionFact::Unknown) => Ok(None),
            RequiredFact::Pending => Err(RetainedValidationError::MissingFact {
                family: "call-resolution",
                index: loc.0,
            }),
            RequiredFact::NotChecked(_) => Err(RetainedValidationError::UncheckedFact {
                family: "call-resolution",
                index: loc.0,
            }),
        }
    }

    pub(in crate::validate) fn close_user_call_graph(
        &mut self,
    ) -> Result<(), RetainedValidationError> {
        let mut graph = UserCallGraph {
            methods: [Vec::new(), Vec::new(), Vec::new()],
            functions: vec![Vec::new(); self.retained.functions.len()],
        };
        for index in 0..self.retained.calls.len() {
            let call = CallLoc(to_u32(index)?);
            let Some(callee) = self.user_callee(call)? else {
                continue;
            };
            let Some((owner, path)) = self.executable_owner_path(call)? else {
                continue;
            };
            let edge = UserCallEdge {
                callee,
                path: path.into_boxed_slice(),
            };
            match owner {
                ExecutableOwner::Method(method) => graph.methods[method.0 as usize].push(edge),
                ExecutableOwner::Function(function) => {
                    graph.functions[function.index()].push(edge);
                }
            }
        }
        let (retained, capabilities) = (&mut self.retained, &mut self.capabilities);
        let capability = capabilities.prepare_user_call_graph()?;
        PreparedUserGraphInstall {
            destination: &mut retained.user_call_graph,
            capability,
            graph,
        }
        .commit();
        Ok(())
    }

    pub(in crate::validate) fn user_call_graph(
        &self,
    ) -> Result<&UserCallGraph, RetainedValidationError> {
        self.retained
            .user_call_graph
            .checked()
            .ok_or(RetainedValidationError::MissingFact {
                family: "user-call-graph",
                index: 0,
            })
    }

    fn call_result_receivers(
        &self,
        call: CallLoc,
        expected: ResultReceiverKind,
        result_count: usize,
    ) -> Result<Vec<CallResultReceiverLoc>, RetainedValidationError> {
        let use_site = &self
            .retained
            .calls
            .get(call.0 as usize)
            .ok_or(RetainedValidationError::MissingResolvedSubject)?
            .use_site;
        match (use_site, expected) {
            (CallUseLoc::ExpressionValue(expression), ResultReceiverKind::Expression) => {
                Ok(vec![CallResultReceiverLoc::ExpressionValue(*expression)])
            }
            (CallUseLoc::MultiAssignment { targets, .. }, ResultReceiverKind::MultiAssignment) => {
                Ok(targets
                    .iter()
                    .copied()
                    .map(CallResultReceiverLoc::MultiAssignmentDestination)
                    .collect())
            }
            (CallUseLoc::DiscardedBy(statement), ResultReceiverKind::Discarded) => Ok(
                std::iter::repeat_n(CallResultReceiverLoc::DiscardedBy(*statement), result_count)
                    .collect(),
            ),
            _ => inconsistent("call-use-site", call.0 as usize),
        }
    }

    fn fixed_call_result_shapes(
        &self,
        call: CallLoc,
        resolution: &CallResolution,
        results: &[CalleeResultLoc],
    ) -> Result<Vec<Option<FixedValueShape>>, ShapeRelationError> {
        match resolution.target {
            CallTarget::Function(_) => results
                .iter()
                .map(|result| match result {
                    CalleeResultLoc::UserOutput(declaration) => self
                        .declaration_shape(*declaration)
                        .map_err(ShapeRelationError::Index),
                    CalleeResultLoc::BuiltinOutput(_) => {
                        inconsistent_shape("call-result-origin", call.0 as usize)
                    }
                })
                .collect(),
            CallTarget::Builtin { base, lifted_rank } => {
                self.fixed_builtin_result_shapes(call, base, lifted_rank, &resolution.outputs)
            }
        }
    }

    fn fixed_builtin_result_shapes(
        &self,
        call: CallLoc,
        base: BuiltinLoc,
        lifted_rank: u8,
        outputs: &[Ty],
    ) -> Result<Vec<Option<FixedValueShape>>, ShapeRelationError> {
        let arguments = self.call_argument_shapes(call)?;
        if lifted_rank > 0 {
            let mut arrays = arguments
                .iter()
                .filter_map(|shape| shape.as_ref().filter(|shape| !shape.extents.is_empty()));
            let Some(first) = arrays.next() else {
                return Ok(vec![None; outputs.len()]);
            };
            if first.extents.len() != usize::from(lifted_rank)
                || arrays.any(|shape| shape.extents != first.extents)
            {
                return Err(ShapeRelationError::Mismatch {
                    context: "lifted-call argument fixed dimensions",
                });
            }
            return outputs
                .iter()
                .map(|ty| {
                    let scalar = ty.element().ok_or(ShapeRelationError::Index(
                        RetainedValidationError::InconsistentFact {
                            family: "call-result-shape",
                            index: call.0,
                        },
                    ))?;
                    Ok(Some(FixedValueShape {
                        scalar,
                        extents: first.extents.clone(),
                    }))
                })
                .collect();
        }
        let builtin = crate::builtins::BUILTINS
            .get(base.index())
            .ok_or(RetainedValidationError::MissingBuiltin)?;
        let extents = fixed_builtin_output_extents(builtin.name, &arguments, call.0 as usize)?;
        outputs
            .iter()
            .enumerate()
            .map(|(output, ty)| {
                let scalar = ty.element().ok_or(ShapeRelationError::Index(
                    RetainedValidationError::InconsistentFact {
                        family: "call-result-shape",
                        index: call.0,
                    },
                ))?;
                let rank = ty.rank().ok_or(RetainedValidationError::InconsistentFact {
                    family: "call-result-shape",
                    index: call.0,
                })?;
                Ok(if rank == 0 {
                    Some(FixedValueShape::scalar(scalar))
                } else {
                    extents
                        .get(output)
                        .cloned()
                        .flatten()
                        .map(|extents| FixedValueShape { scalar, extents })
                })
            })
            .collect()
    }

    fn call_argument_shapes(
        &self,
        call: CallLoc,
    ) -> Result<Vec<Option<FixedValueShape>>, RetainedValidationError> {
        let subject = self
            .retained
            .calls
            .get(call.0 as usize)
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        let mut arguments = subject
            .children
            .iter()
            .filter_map(|(child, role)| match (child, role) {
                (SubjectLoc::Expression(expression), ChildRole::CallArgument(order)) => {
                    Some((*order, *expression))
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        arguments.sort_unstable_by_key(|(order, _)| *order);
        arguments
            .into_iter()
            .enumerate()
            .map(|(expected, (order, expression))| {
                if usize::try_from(order).ok() != Some(expected) {
                    return inconsistent("call-argument-order", call.0 as usize);
                }
                self.retained.expressions[expression.0 as usize]
                    .fixed_shape
                    .checked()
                    .cloned()
                    .ok_or(RetainedValidationError::MissingFact {
                        family: "expression-fixed-shape",
                        index: expression.0,
                    })
            })
            .collect()
    }

    fn declaration_shape(
        &self,
        declaration: DeclarationLoc,
    ) -> Result<Option<FixedValueShape>, RetainedValidationError> {
        self.retained
            .declarations
            .get(declaration.0 as usize)
            .map(|subject| subject.fixed_shape.clone())
            .ok_or(RetainedValidationError::MissingResolvedSubject)
    }

    fn fixed_expression_shape(
        &self,
        expression: &Expression,
        ty: Ty,
        index: usize,
    ) -> Result<Option<FixedValueShape>, ShapeRelationError> {
        let Some(scalar) = ty.element() else {
            return Ok(None);
        };
        let shape = match expression {
            Expression::Bool(_) | Expression::Integer(_) | Expression::Real(_) => {
                Some(FixedValueShape::scalar(scalar))
            }
            Expression::Ref(reference) | Expression::Neg(reference) => {
                self.fixed_reference_shape_for(reference)?
            }
            Expression::Size { .. } | Expression::Not(_) => Some(FixedValueShape::scalar(scalar)),
            Expression::Call(call) => self.fixed_expression_call_shape(call, index)?,
            Expression::Paren(inner) => self.fixed_shape_for_expression(inner)?,
            Expression::If(value) => {
                let shapes = value
                    .branches
                    .iter()
                    .map(|(_, result)| result)
                    .chain(std::iter::once(value.else_value.as_ref()))
                    .map(|result| self.fixed_shape_for_expression(result))
                    .collect::<Result<Vec<_>, _>>()?;
                equal_fixed_shapes(shapes, "if-expression fixed dimensions")?
            }
            Expression::Array(elements) => {
                let shapes = elements
                    .iter()
                    .map(|element| self.fixed_shape_for_expression(element))
                    .collect::<Result<Vec<_>, _>>()?;
                let Some(element) = equal_fixed_shapes(shapes, "array element fixed dimensions")?
                else {
                    return Ok(None);
                };
                let mut extents = Vec::with_capacity(element.extents.len() + 1);
                extents.push(to_u32(elements.len())?);
                extents.extend(element.extents.iter().copied());
                Some(FixedValueShape {
                    scalar,
                    extents: extents.into_boxed_slice(),
                })
            }
            Expression::Binary { lhs, rhs, .. } => {
                let lhs = self.fixed_shape_for_expression(lhs)?;
                let rhs = self.fixed_shape_for_expression(rhs)?;
                match (lhs, rhs) {
                    (Some(lhs), Some(rhs)) if lhs.extents.is_empty() => Some(FixedValueShape {
                        scalar,
                        extents: rhs.extents,
                    }),
                    (Some(lhs), Some(rhs)) if rhs.extents.is_empty() => Some(FixedValueShape {
                        scalar,
                        extents: lhs.extents,
                    }),
                    (Some(lhs), Some(rhs)) if lhs.extents == rhs.extents => Some(FixedValueShape {
                        scalar,
                        extents: lhs.extents,
                    }),
                    (Some(_), Some(_)) => {
                        return Err(ShapeRelationError::Mismatch {
                            context: "binary expression fixed dimensions",
                        });
                    }
                    (None, _) | (_, None) => None,
                }
            }
        };
        if let (Some(shape), Some(rank)) = (&shape, ty.rank())
            && shape.extents.len() != rank
        {
            return Err(ShapeRelationError::Mismatch {
                context: "expression fixed dimensions",
            });
        }
        Ok(shape)
    }

    fn fixed_shape_for_expression(
        &self,
        expression: &Expression,
    ) -> Result<Option<FixedValueShape>, RetainedValidationError> {
        let loc = *self
            .lookup
            .expressions
            .get(&std::ptr::from_ref(expression))
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        self.retained.expressions[loc.0 as usize]
            .fixed_shape
            .checked()
            .cloned()
            .ok_or(RetainedValidationError::MissingFact {
                family: "expression-fixed-shape",
                index: loc.0,
            })
    }

    fn fixed_reference_shape_for(
        &self,
        reference: &Reference,
    ) -> Result<Option<FixedValueShape>, RetainedValidationError> {
        let loc = *self
            .lookup
            .references
            .get(&std::ptr::from_ref(reference))
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        self.retained.references[loc.0 as usize]
            .fixed_shape
            .checked()
            .cloned()
            .ok_or(RetainedValidationError::MissingFact {
                family: "reference-fixed-shape",
                index: loc.0,
            })
    }

    fn fixed_expression_call_shape(
        &self,
        call: &FunctionCall,
        expression_index: usize,
    ) -> Result<Option<FixedValueShape>, RetainedValidationError> {
        let loc = *self
            .lookup
            .calls
            .get(&std::ptr::from_ref(call))
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        let call = &self.retained.calls[loc.0 as usize];
        let result_set = call
            .result_set
            .checked()
            .ok_or(RetainedValidationError::MissingFact {
                family: "call-result-set",
                index: loc.0,
            })?;
        if result_set.count != 1 {
            return inconsistent("expression-call-result-cardinality", expression_index);
        }
        let Some((SubjectLoc::CallResultProjection(projection), _)) =
            call.children.iter().find(|(subject, role)| {
                matches!(subject, SubjectLoc::CallResultProjection(_))
                    && matches!(role, ChildRole::CallResult(0))
            })
        else {
            return inconsistent("expression-call-result-cardinality", expression_index);
        };
        self.retained
            .call_result_projections
            .get(projection.0 as usize)
            .map(|projection| projection.value.clone())
            .ok_or(RetainedValidationError::MissingResolvedSubject)
    }

    fn user_callee(&self, call: CallLoc) -> Result<Option<FunctionLoc>, RetainedValidationError> {
        let subject = self
            .retained
            .calls
            .get(call.0 as usize)
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        match &subject.resolution {
            RequiredFact::Checked(CallResolutionFact::Known(CallResolution {
                target: CallTarget::Function(function),
                ..
            })) => Ok(Some(*function)),
            RequiredFact::Checked(CallResolutionFact::Known(CallResolution {
                target: CallTarget::Builtin { .. },
                ..
            }))
            | RequiredFact::Checked(CallResolutionFact::Unknown) => Ok(None),
            RequiredFact::Pending => Err(RetainedValidationError::MissingFact {
                family: "call-resolution",
                index: call.0,
            }),
            RequiredFact::NotChecked(_) => Err(RetainedValidationError::UncheckedFact {
                family: "call-resolution",
                index: call.0,
            }),
        }
    }

    fn executable_owner_path(
        &self,
        call: CallLoc,
    ) -> Result<Option<(ExecutableOwner, Vec<CallPathSegment>)>, RetainedValidationError> {
        let mut current = SubjectLoc::Call(call);
        let mut reverse_path = Vec::new();
        loop {
            let parent = self.retained.subject_parent(current)?;
            let SubjectParent::Subject { owner, role } = parent else {
                return Ok(None);
            };
            match (current, owner, role) {
                (
                    SubjectLoc::Statement(_),
                    SubjectLoc::Method(method),
                    ChildRole::MethodAction(index),
                ) => {
                    reverse_path.push(CallPathSegment::Statement(index));
                    reverse_path.reverse();
                    return Ok(Some((ExecutableOwner::Method(method), reverse_path)));
                }
                (
                    SubjectLoc::Statement(_),
                    SubjectLoc::Function(function),
                    ChildRole::FunctionAction(index),
                ) => {
                    reverse_path.push(CallPathSegment::Statement(index));
                    reverse_path.reverse();
                    return Ok(Some((ExecutableOwner::Function(function), reverse_path)));
                }
                (
                    SubjectLoc::Statement(_),
                    SubjectLoc::Statement(_),
                    ChildRole::IfAction { branch, index },
                ) => {
                    reverse_path.push(CallPathSegment::Statement(index));
                    reverse_path.push(CallPathSegment::Branch(branch));
                }
                (
                    SubjectLoc::Statement(_),
                    SubjectLoc::Statement(_),
                    ChildRole::ElseAction(index),
                ) => {
                    reverse_path.push(CallPathSegment::Statement(index));
                    reverse_path.push(CallPathSegment::Else);
                }
                (
                    SubjectLoc::Statement(_),
                    SubjectLoc::Statement(_),
                    ChildRole::LoopAction(index),
                ) => reverse_path.push(CallPathSegment::Statement(index)),
                (
                    SubjectLoc::Expression(_),
                    SubjectLoc::Statement(_),
                    ChildRole::IfCondition(branch) | ChildRole::SignalFallback(branch),
                ) => {
                    reverse_path.push(CallPathSegment::Condition);
                    reverse_path.push(CallPathSegment::Branch(branch));
                }
                (_, SubjectLoc::Method(_) | SubjectLoc::Function(_), _) => return Ok(None),
                _ => {}
            }
            current = owner;
        }
    }

    pub(in crate::validate) fn finish(
        mut self,
    ) -> Result<RetainedValidation, RetainedValidationError> {
        self.capabilities.finish()?;
        self.retained.fixed_shapes = self.fixed_shapes.finish()?;
        Ok(self.retained)
    }

    fn require_reference_target(
        &self,
        target: ResolvedTarget,
        index: usize,
    ) -> Result<(), RetainedValidationError> {
        let installed = match target {
            ResolvedTarget::Declaration(loc) => (loc.0 as usize) < self.retained.declarations.len(),
            ResolvedTarget::Binder(loc) => (loc.0 as usize) < self.retained.binders.len(),
        };
        if !installed {
            return inconsistent("reference-resolution", index);
        }
        Ok(())
    }

    fn require_call_resolution(
        &self,
        resolution: &CallResolution,
        index: usize,
    ) -> Result<(), RetainedValidationError> {
        let expected_stateful =
            match resolution.target {
                CallTarget::Function(loc) => {
                    let function = self
                        .retained
                        .functions
                        .get(loc.0 as usize)
                        .ok_or(RetainedValidationError::MissingResolvedSubject)?;
                    if resolution.results.len() != function.results.len()
                        || !resolution.results.iter().zip(&function.results).all(
                            |(found, expected)| *found == CalleeResultLoc::UserOutput(*expected),
                        )
                    {
                        return inconsistent("call-result-origin", index);
                    }
                    matches!(function.kind, FunctionKind::Stateful)
                }
                CallTarget::Builtin { base, lifted_rank } => {
                    self.require_builtin_call_resolution(resolution, base, lifted_rank, index)?;
                    false
                }
            };
        if resolution.stateful != expected_stateful {
            return inconsistent("call-resolution", index);
        }
        Ok(())
    }

    fn require_builtin_call_resolution(
        &self,
        resolution: &CallResolution,
        base: BuiltinLoc,
        lifted_rank: u8,
        index: usize,
    ) -> Result<(), RetainedValidationError> {
        let Some(builtin) = crate::builtins::BUILTINS.get(base.index()) else {
            return inconsistent("call-target", index);
        };
        if lifted_rank > 0 && (!(1..=2).contains(&lifted_rank) || !builtin.is_lifted()) {
            return inconsistent("call-target", index);
        }
        if resolution.inputs.len() != builtin.inputs.len()
            || resolution.outputs.len() != builtin.outputs.len()
            || resolution.results.len() != builtin.outputs.len()
        {
            return inconsistent("call-signature", index);
        }
        for (found, parameter) in resolution.inputs.iter().zip(builtin.inputs) {
            if *found != builtin_parameter_type(parameter.ty, lifted_rank) {
                return inconsistent("call-signature", index);
            }
        }
        for (found, parameter) in resolution.outputs.iter().zip(builtin.outputs) {
            if *found != builtin_parameter_type(parameter.ty, lifted_rank) {
                return inconsistent("call-signature", index);
            }
        }
        let result_start = self.retained.builtin_results.len();
        for (ordinal, found) in resolution.results.iter().enumerate() {
            let expected =
                CalleeResultLoc::BuiltinOutput(BuiltinResultLoc(to_u32(result_start + ordinal)?));
            if *found != expected {
                return inconsistent("call-result-origin", index);
            }
        }
        Ok(())
    }

    fn require_prepared_builtin_results(
        &self,
        prepared: &PreparedCallResolution,
        index: usize,
    ) -> Result<(), RetainedValidationError> {
        let (base, lifted_rank) = match prepared.resolution.target {
            CallTarget::Function(_) => {
                if prepared.builtin_results.is_empty() {
                    return Ok(());
                }
                return inconsistent("prepared-builtin-results", index);
            }
            CallTarget::Builtin { base, lifted_rank } => (base, lifted_rank),
        };
        if prepared.builtin_results.len() != prepared.resolution.results.len() {
            return inconsistent("prepared-builtin-results", index);
        }
        let start = self.retained.builtin_results.len();
        for (ordinal, (subject, result)) in prepared
            .builtin_results
            .iter()
            .zip(&prepared.resolution.results)
            .enumerate()
        {
            let expected_result =
                CalleeResultLoc::BuiltinOutput(BuiltinResultLoc(to_u32(start + ordinal)?));
            if subject.base != base
                || subject.lifted_rank != lifted_rank
                || usize::from(subject.output) != ordinal
                || *result != expected_result
            {
                return inconsistent("prepared-builtin-results", index);
            }
        }
        Ok(())
    }

    fn declaration_loc(
        &self,
        declaration: &VariableDeclaration,
    ) -> Result<DeclarationLoc, RetainedValidationError> {
        self.lookup
            .declarations
            .get(&std::ptr::from_ref(declaration))
            .copied()
            .ok_or(RetainedValidationError::MissingResolvedSubject)
    }
}
