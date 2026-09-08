//! Conditional lowering for [`ExpressionLowerer`].
//!
//! Kept apart from the scalar and aggregate dispatch so the materialization
//! rules for `if` branches, which decide whether a branch becomes a shared
//! prefix statement or an inline expression, read as one unit.

use super::*;

impl<'a, 'dae> ExpressionLowerer<'a, 'dae> {
    pub(super) fn lower_conditional_at(
        &mut self,
        expression: dae::ExprId<'dae>,
        operands: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        if self.materialize_function_values {
            self.conditional_depth += 1;
            let result = self.lower_materialized_conditional(
                expression,
                operands,
                indices,
                scalar_type,
                span,
            );
            self.conditional_depth -= 1;
            return result;
        }
        self.lower_conditional_branches(operands, indices, scalar_type, span)
    }

    fn lower_materialized_conditional(
        &mut self,
        expression: dae::ExprId<'dae>,
        operands: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let name = gast::Name::ident(format!(
            "rumoca_{}_conditional_{}",
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
        let activation_operands = conditional_activation_operands(operands);
        let selection = self.selection_point(
            ConditionalActivationKind::ConditionalScalar,
            expression,
            &activation_operands,
            indices,
        )?;
        let conditional = MaterializedConditional {
            operands,
            indices,
            scalar_type,
            target: &name,
            activation_operands,
            selection,
            span,
        };
        let entry_materialization = self.conditional_materialization_snapshot();
        let outer_activation = self.conditional_activation_path.clone();
        let statements = self.lower_materialized_conditional_branch(&conditional, 0)?;
        let guarded = self
            .materialized_function_calls
            .iter()
            .filter(|(key, _)| {
                key.activation_path.len() > outer_activation.len()
                    && key.activation_path.starts_with(&outer_activation)
                    && key.activation_path[outer_activation.len()].operands
                        == conditional.activation_operands
                    && key.activation_path[outer_activation.len()].selection
                        == conditional.selection
            })
            .filter_map(|(key, names)| {
                self.materialized_call_sources
                    .get(key)
                    .cloned()
                    .map(|sources| (key.clone(), names.clone(), sources))
            })
            .collect::<Vec<_>>();
        let joined = self.exhaustive_materialized_call_joins(
            &outer_activation,
            &conditional.activation_operands,
            conditional.selection,
            u32::try_from(operands.len().div_ceil(2)).map_err(|_| {
                GalecTargetError::LoweringInternal {
                    detail: "conditional branch count exceeds the activation-key capacity"
                        .to_owned(),
                }
            })?,
        );
        self.restore_conditional_materialization(&entry_materialization);
        for (key, names, sources) in guarded.into_iter().chain(joined) {
            self.materialized_function_calls.insert(key.clone(), names);
            self.materialized_call_sources.insert(key, sources);
        }
        self.pending_prefix_statements.extend(statements);
        Ok(gast::Expression::Ref(gast::Reference::local(name)))
    }

    /// Retain one call result after an exhaustive scalar conditional only when
    /// every branch emitted that exact invocation into the same result locals.
    ///
    /// This is a construction join at the point that owns the `if` statement;
    /// no later pass recognizes statement shapes or guesses guard equivalence.
    fn exhaustive_materialized_call_joins(
        &self,
        outer_activation: &[ConditionalActivationKey],
        operands: &[u32],
        selection: SelectionPointId,
        branch_count: u32,
    ) -> Vec<(
        MaterializedFunctionCallKey,
        Vec<gast::Name>,
        HashSet<CallExecutionSource>,
    )> {
        self.materialized_function_calls
            .iter()
            .filter(|(key, _)| {
                matches_conditional_branch(key, outer_activation, operands, selection, 0)
            })
            .filter_map(|(first, names)| {
                let mut sources = HashSet::new();
                for branch in 0..branch_count {
                    let candidate = self.matching_materialized_call_branch(
                        first,
                        names,
                        outer_activation,
                        operands,
                        selection,
                        branch,
                    )?;
                    sources.extend(
                        self.materialized_call_sources
                            .get(candidate)?
                            .iter()
                            .cloned(),
                    );
                }
                let mut key = first.clone();
                key.activation_path = outer_activation.to_vec();
                Some((key, names.clone(), sources))
            })
            .collect()
    }

    fn matching_materialized_call_branch(
        &self,
        first: &MaterializedFunctionCallKey,
        names: &[gast::Name],
        outer_activation: &[ConditionalActivationKey],
        operands: &[u32],
        selection: SelectionPointId,
        branch: u32,
    ) -> Option<&MaterializedFunctionCallKey> {
        self.materialized_function_calls
            .iter()
            .find(|(candidate, candidate_names)| {
                candidate.same_invocation(first)
                    && candidate_names.as_slice() == names
                    && matches_conditional_branch(
                        candidate,
                        outer_activation,
                        operands,
                        selection,
                        branch,
                    )
            })
            .map(|(candidate, _)| candidate)
    }

    fn lower_materialized_conditional_branch(
        &mut self,
        conditional: &MaterializedConditional<'_, 'dae>,
        ordinal: usize,
    ) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
        let branch_activation = ConditionalActivationKey {
            kind: ConditionalActivationKind::ConditionalScalar,
            operands: conditional.activation_operands.clone(),
            selection: conditional.selection,
            branch: u32::try_from(ordinal / 2).map_err(|_| GalecTargetError::LoweringInternal {
                detail: "conditional branch exceeds the activation-key capacity".to_owned(),
            })?,
        };
        self.conditional_activation_path
            .push(branch_activation.clone());
        if ordinal + 1 == conditional.operands.len() {
            let start = self.pending_prefix_statements.len();
            let value = self.lower_at(
                conditional
                    .operands
                    .get(ordinal)
                    .expect("checked conditional fallback"),
                conditional.indices,
            );
            self.conditional_activation_path.pop();
            let value = value?;
            let mut body = self.pending_prefix_statements.split_off(start);
            body.push(gast::Spanned::new(
                gast::Statement::Assignment {
                    target: gast::Reference::local(conditional.target.clone()),
                    value: coerce(value, conditional.scalar_type, conditional.span)?,
                },
                conditional.span,
            ));
            return Ok(body);
        }

        // The condition executes before this branch is selected. Keeping the
        // selected-arm fact here would let the ownership ledger call it
        // disjoint from work that can execute after a false condition.
        self.conditional_activation_path.pop();
        let condition_start = self.pending_prefix_statements.len();
        let condition = self.lower(
            conditional
                .operands
                .get(ordinal)
                .expect("checked conditional branch condition"),
        );
        let condition = condition?;
        require_boolean(&condition, conditional.span)?;
        let mut statements = self.pending_prefix_statements.split_off(condition_start);

        self.conditional_activation_path.push(branch_activation);
        let value_start = self.pending_prefix_statements.len();
        let value = self.lower_at(
            conditional
                .operands
                .get(ordinal + 1)
                .expect("checked conditional branch value"),
            conditional.indices,
        );
        self.conditional_activation_path.pop();
        let value = value?;
        let mut body = self.pending_prefix_statements.split_off(value_start);
        body.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: gast::Reference::local(conditional.target.clone()),
                value: coerce(value, conditional.scalar_type, conditional.span)?,
            },
            conditional.span,
        ));
        let else_body = self.lower_materialized_conditional_branch(conditional, ordinal + 2)?;
        statements.push(gast::Spanned::new(
            gast::Statement::If(gast::IfStatement {
                branches: vec![gast::IfBranch {
                    condition: gast::Condition::Expression(condition.expression),
                    body,
                    span: conditional.span,
                }],
                else_body: Some(else_body),
            }),
            conditional.span,
        ));
        Ok(statements)
    }

    fn lower_conditional_branches(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let mut branches = Vec::new();
        for ordinal in (0..operands.len() - 1).step_by(2) {
            let condition =
                self.lower(operands.get(ordinal).expect("checked condition operand"))?;
            require_boolean(&condition, span)?;
            let value = self.lower_at(
                operands.get(ordinal + 1).expect("checked value operand"),
                indices,
            )?;
            branches.push((condition.expression, coerce(value, scalar_type, span)?));
        }
        let fallback = self.lower_at(
            operands
                .get(operands.len() - 1)
                .expect("checked conditional fallback"),
            indices,
        )?;
        Ok(gast::Expression::If(gast::IfExpression::new(
            branches,
            coerce(fallback, scalar_type, span)?,
        )))
    }
}

fn matches_conditional_branch(
    key: &MaterializedFunctionCallKey,
    outer_activation: &[ConditionalActivationKey],
    operands: &[u32],
    selection: SelectionPointId,
    branch: u32,
) -> bool {
    key.activation_path.len() == outer_activation.len() + 1
        && key.activation_path.starts_with(outer_activation)
        && key.activation_path.last().is_some_and(|activation| {
            activation.operands == operands
                && activation.selection == selection
                && activation.branch == branch
        })
}
