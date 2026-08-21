//! Scalar tensor, array, builtin, and coordinate projection into GALEC.

mod contraction;

use super::*;
use contraction::{TensorContraction, contraction_indices, sum_terms, tensor_contraction};

pub(super) struct SelectionValue {
    pub(super) prefix: Vec<gast::Spanned<gast::Statement>>,
    pub(super) expression: gast::Expression,
}

pub(super) struct SelectionBranch {
    pub(super) condition_prefix: Vec<gast::Spanned<gast::Statement>>,
    pub(super) condition: gast::Expression,
    pub(super) value: SelectionValue,
}

fn slice_overflow_error(span: Span) -> GalecTargetError {
    unsupported(
        "slice-overflow",
        "slice index arithmetic overflowed".to_owned(),
        span,
    )
}

fn compute_contains_function_fold_projection<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> bool {
    let mut pending = vec![expression];
    let mut seen = HashSet::new();
    let mut found = false;
    while let Some(root) = pending.pop() {
        if !seen.insert(root.index()) {
            continue;
        }
        dae::for_each_expression(view, root, |_, node| match node.operation() {
            dae::ExpressionOperation::FunctionFoldParameter { .. }
            | dae::ExpressionOperation::FunctionFoldOutput { .. } => found = true,
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                pending.push(definition.rhs());
            }
            dae::ExpressionOperation::Call {
                function, output, ..
            } => {
                if !user_functions::is_directly_lowerable(view, function)
                    && let Some(result) = view
                        .function(function)
                        .and_then(|function| function.result_values().rhs(output as usize))
                {
                    pending.push(result);
                }
            }
            _ => {}
        });
    }
    found
}

/// Read an index-split selection guard as `<loop index> <= <literal>`.
///
/// Concatenation and slice selections always guard on the loop coordinate the
/// segment owns, offset by the segments already consumed, so the recognised
/// shapes are the bare iterator and the iterator shifted by a literal. Any
/// other guard returns `None`, which keeps the caller from claiming a
/// reachability proof it does not have.
fn selection_upper_bound(condition: &gast::Expression) -> Option<(String, i64)> {
    let gast::Expression::Binary {
        op: gast::BinaryOp::Le,
        lhs,
        rhs,
    } = condition
    else {
        return None;
    };
    let gast::Expression::Integer(upper) = rhs.as_ref() else {
        return None;
    };
    let (name, offset) = linear_index_term(lhs)?;
    Some((name, upper.checked_sub(offset)?))
}

/// Split `<loop index>`, `<loop index> + k`, or `<loop index> - k` into the
/// iterator name and the literal offset applied to it.
fn linear_index_term(expression: &gast::Expression) -> Option<(String, i64)> {
    match expression {
        gast::Expression::Paren(inner) => linear_index_term(inner),
        gast::Expression::Ref(gast::Reference::Local(part)) if part.subscripts.is_empty() => {
            Some((part.name.lexeme().to_owned(), 0))
        }
        gast::Expression::Binary { op, lhs, rhs } => {
            let gast::Expression::Integer(literal) = rhs.as_ref() else {
                return None;
            };
            let (name, offset) = linear_index_term(lhs)?;
            let offset = match op {
                gast::BinaryOp::Add => offset.checked_add(*literal)?,
                gast::BinaryOp::Sub => offset.checked_sub(*literal)?,
                _ => return None,
            };
            Some((name, offset))
        }
        _ => None,
    }
}

fn truncate_promoted_projection(projection: &mut Vec<gast::Expression>, rank: usize) {
    // MLS promotion appends unit dimensions. The checked concatenation result
    // already proves every non-concatenated promoted extent is one, while the
    // selected segment proves the concatenated local coordinate. A dynamic
    // loop coordinate need not itself simplify to the literal one here.
    projection.truncate(rank);
}

fn selection_assignment(
    mut value: SelectionValue,
    target: &gast::Name,
    span: Span,
) -> Vec<gast::Spanned<gast::Statement>> {
    value.prefix.push(gast::Spanned::new(
        gast::Statement::Assignment {
            target: gast::Reference::local(target.clone()),
            value: value.expression,
        },
        span,
    ));
    value.prefix
}

fn selection_statements(
    mut branches: Vec<SelectionBranch>,
    fallback: SelectionValue,
    target: &gast::Name,
    span: Span,
) -> Vec<gast::Spanned<gast::Statement>> {
    if branches.is_empty() {
        return selection_assignment(fallback, target, span);
    }
    let branch = branches.remove(0);
    let mut statements = branch.condition_prefix;
    statements.push(gast::Spanned::new(
        gast::Statement::If(gast::IfStatement {
            branches: vec![gast::IfBranch {
                condition: gast::Condition::Expression(branch.condition),
                body: selection_assignment(branch.value, target, span),
                span,
            }],
            else_body: Some(selection_statements(branches, fallback, target, span)),
        }),
        span,
    ));
    statements
}

impl<'a, 'dae> ExpressionLowerer<'a, 'dae> {
    fn hoist_loop_invariant_selection_prefixes(
        &mut self,
        branches: &mut [SelectionBranch],
        fallback: &mut SelectionValue,
        span: Span,
    ) {
        let loop_indices = self
            .loop_index_bounds
            .iter()
            .map(|bound| bound.name.clone())
            .collect::<Vec<_>>();
        if loop_indices.is_empty() {
            return;
        }
        if branches
            .iter()
            .any(|branch| !branch.condition_prefix.is_empty())
        {
            return;
        }
        if branches
            .iter()
            .any(|branch| user_functions::expression_depends_on(&branch.condition, &loop_indices))
        {
            self.hoist_taken_selection_prefixes(branches, fallback, &loop_indices);
            return;
        }

        let mut has_hoisted_statements = false;
        let hoisted_branches = branches
            .iter_mut()
            .map(|branch| {
                let (before, guarded) = user_functions::partition_tensor_prefixes(
                    std::mem::take(&mut branch.value.prefix),
                    &loop_indices,
                );
                has_hoisted_statements |= !before.is_empty();
                branch.value.prefix = guarded;
                gast::IfBranch {
                    condition: gast::Condition::Expression(branch.condition.clone()),
                    body: before,
                    span,
                }
            })
            .collect();
        let (fallback_before, guarded) = user_functions::partition_tensor_prefixes(
            std::mem::take(&mut fallback.prefix),
            &loop_indices,
        );
        has_hoisted_statements |= !fallback_before.is_empty();
        fallback.prefix = guarded;
        if has_hoisted_statements {
            self.pending_prefix_statements.push(gast::Spanned::new(
                gast::Statement::If(gast::IfStatement {
                    branches: hoisted_branches,
                    else_body: Some(fallback_before),
                }),
                span,
            ));
        }
    }

    /// Hoist the loop-invariant prefixes of an index-split selection out of
    /// the selection, unguarded.
    ///
    /// A concatenation or slice selection partitions the surrounding loop's
    /// index range, so its guards read that loop index and the guarded hoist
    /// above cannot apply: the guard itself may not leave the loop. The
    /// prefixes are still loop-invariant -- they build one operand of the
    /// selection and never read the loop index -- so the only thing the guard
    /// contributes is *how often* they run. Left in place they run once per
    /// iteration of every enclosing loop, which is how a single
    /// `right_jacobian` operand of a `cat` became one call per element of a
    /// 15x15x15 contraction.
    ///
    /// Moving them ahead of the guard is observationally neutral only when the
    /// guard is taken somewhere in the loop. `taken_selection_segments` proves
    /// exactly that: it walks the branch chain over the proven loop bounds and
    /// requires every segment -- each branch under the preceding branches'
    /// negations, and the fallback under all of them -- to be non-empty. Each
    /// hoisted statement then already ran, on the same operands, in at least
    /// one iteration of the unmodified program, so hoisting introduces no
    /// evaluation that was not already performed and cannot change a value, an
    /// assertion, or a numeric exception.
    fn hoist_taken_selection_prefixes(
        &mut self,
        branches: &mut [SelectionBranch],
        fallback: &mut SelectionValue,
        loop_indices: &[gast::Name],
    ) {
        if !self.taken_selection_segments(branches) {
            return;
        }
        let mut hoisted = Vec::new();
        for branch in branches.iter_mut() {
            let (before, guarded) = user_functions::partition_tensor_prefixes(
                std::mem::take(&mut branch.value.prefix),
                loop_indices,
            );
            branch.value.prefix = guarded;
            hoisted.extend(before);
        }
        let (before, guarded) = user_functions::partition_tensor_prefixes(
            std::mem::take(&mut fallback.prefix),
            loop_indices,
        );
        fallback.prefix = guarded;
        hoisted.extend(before);
        self.pending_prefix_statements.extend(hoisted);
    }

    /// Prove every segment of an index-split selection chain is reachable
    /// within the proven loop bounds.
    ///
    /// Every condition must narrow one proven loop index against a literal, so
    /// the chain is a walk over one integer interval: branch `n` owns the
    /// interval left by the first `n` negations, and the fallback owns what
    /// remains. A shape this reasoning does not cover is rejected rather than
    /// assumed reachable.
    fn taken_selection_segments(&self, branches: &[SelectionBranch]) -> bool {
        let mut remaining: HashMap<String, (i64, i64)> = HashMap::new();
        for branch in branches {
            let Some((name, upper)) = selection_upper_bound(&branch.condition) else {
                return false;
            };
            let Some(bound) = self
                .loop_index_bounds
                .iter()
                .find(|bound| bound.name.lexeme() == name)
            else {
                return false;
            };
            let interval = remaining
                .entry(name)
                .or_insert((bound.minimum, bound.maximum));
            let Some(next) = upper.checked_add(1) else {
                return false;
            };
            // This branch owns `[low, upper]`, the rest of the chain owns
            // `[upper + 1, high]`; hoisting is only neutral when both hold an
            // index the enclosing loop actually visits.
            if interval.0 > upper.min(interval.1) || next.max(interval.0) > interval.1 {
                return false;
            }
            interval.0 = next.max(interval.0);
        }
        true
    }

    pub(super) fn lower_lazy_selection(
        &mut self,
        mut branches: Vec<SelectionBranch>,
        mut fallback: SelectionValue,
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> TypedExpression {
        let conditional_call_names = self
            .materialized_function_calls
            .iter()
            .filter(|(key, _)| !key.activation_path.is_empty())
            .flat_map(|(_, names)| names.iter().cloned())
            .collect::<Vec<_>>();
        let materialized_branch_values = !fallback.prefix.is_empty()
            || branches.iter().any(|branch| {
                !branch.condition_prefix.is_empty() || !branch.value.prefix.is_empty()
            })
            || user_functions::expression_depends_on(&fallback.expression, &conditional_call_names)
            || branches.iter().any(|branch| {
                user_functions::expression_depends_on(
                    &branch.value.expression,
                    &conditional_call_names,
                )
            });
        self.hoist_loop_invariant_selection_prefixes(&mut branches, &mut fallback, span);

        if branches.is_empty() {
            self.pending_prefix_statements.extend(fallback.prefix);
            return TypedExpression {
                expression: fallback.expression,
                scalar_type,
            };
        }
        let needs_statements = materialized_branch_values
            || !fallback.prefix.is_empty()
            || branches.iter().any(|branch| {
                !branch.condition_prefix.is_empty() || !branch.value.prefix.is_empty()
            });
        if !needs_statements {
            return TypedExpression {
                expression: gast::Expression::If(gast::IfExpression::new(
                    branches
                        .into_iter()
                        .map(|branch| (branch.condition, branch.value.expression))
                        .collect(),
                    fallback.expression,
                )),
                scalar_type,
            };
        }

        let target = gast::Name::ident(format!(
            "rumoca_{}_selection_{}",
            self.temporary_namespace, self.temporary_counter
        ));
        self.temporary_counter += 1;
        self.temporary_locals.push(gast::VariableDeclaration {
            ty: gast::TypeRef::Primitive(scalar_type),
            name: target.clone(),
            dimensions: Vec::new(),
            range: gast::RangeAttributes::default(),
            span,
        });
        self.pending_prefix_statements
            .extend(selection_statements(branches, fallback, &target, span));
        TypedExpression {
            expression: gast::Expression::Ref(gast::Reference::local(target)),
            scalar_type,
        }
    }

    fn contains_function_fold_projection(&mut self, expression: dae::ExprId<'dae>) -> bool {
        if let Some(found) = self.function_fold_projection_cache.get(&expression.index()) {
            return *found;
        }
        let found = compute_contains_function_fold_projection(self.view, expression);
        self.function_fold_projection_cache
            .insert(expression.index(), found);
        found
    }

    pub(super) fn lower_unary_at(
        &mut self,
        operator: dae::UnaryOperator,
        operand: dae::ExprId<'dae>,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let operand = self.lower_at(operand, indices)?;
        match operator {
            dae::UnaryOperator::Plus => Ok(operand.expression),
            dae::UnaryOperator::Negate => match operand.scalar_type {
                gast::ScalarType::Real => Ok(gast::Expression::negated_real(operand.expression)),
                gast::ScalarType::Integer => {
                    Ok(gast::Expression::negated_integer(operand.expression))
                }
                gast::ScalarType::Boolean => Err(type_mismatch("numeric", "Boolean", span)),
            },
            dae::UnaryOperator::Not => Ok(gast::Expression::Not(Box::new(operand.expression))),
        }
    }

    pub(super) fn lower_binary_at(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let lhs_type = self.view.expression(lhs).expect("checked lhs").value_type();
        let rhs_type = self.view.expression(rhs).expect("checked rhs").value_type();
        if operator == dae::BinaryOperator::Multiply
            && indices.is_empty()
            && matches!(
                (lhs_type.dimensions(), rhs_type.dimensions()),
                ([lhs_extent], [rhs_extent]) if lhs_extent == rhs_extent
            )
        {
            let extent = lhs_type.dimensions()[0];
            return self.lower_dot_product(lhs, rhs, extent, scalar_type, span);
        }
        if operator == dae::BinaryOperator::Multiply
            && let Some(contraction) =
                tensor_contraction(lhs_type.dimensions(), rhs_type.dimensions(), indices)
        {
            return self.lower_tensor_contraction(lhs, rhs, contraction, scalar_type, span);
        }
        let lhs_indices = operand_projection(lhs_type.dimensions(), indices, span)?;
        let rhs_indices = operand_projection(rhs_type.dimensions(), indices, span)?;
        let lhs = self.lower_at(lhs, &lhs_indices)?;
        let rhs = self.lower_at(rhs, &rhs_indices)?;
        let expression = lower_binary(operator, lhs, rhs, scalar_type, span)?;
        self.bound_expression(
            TypedExpression {
                expression,
                scalar_type,
            },
            span,
        )
    }

    fn lower_dot_product(
        &mut self,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        extent: u32,
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let fold_projection = self.materialize_function_values
            && (self.contains_function_fold_projection(lhs)
                || self.contains_function_fold_projection(rhs));
        if self.materialize_function_values && !fold_projection {
            return self.lower_materialized_contraction(
                lhs,
                rhs,
                TensorContraction {
                    extent,
                    lhs_outer: Vec::new(),
                    rhs_outer: Vec::new(),
                    lhs_matrix: false,
                    rhs_matrix: false,
                },
                scalar_type,
                span,
            );
        }
        let mut terms = Vec::with_capacity(extent as usize);
        for index in 1..=extent {
            let index = [gast::Expression::Integer(i64::from(index))];
            let lhs = self.lower_at(lhs, &index)?;
            let rhs = self.lower_at(rhs, &index)?;
            terms.push(lower_binary(
                dae::BinaryOperator::Multiply,
                lhs,
                rhs,
                scalar_type,
                span,
            )?);
        }
        let expression = sum_terms(
            terms,
            "zero-dot-product",
            "zero-length dot product requires an explicit additive identity",
            span,
        )?;
        self.bound_expression(
            TypedExpression {
                expression,
                scalar_type,
            },
            span,
        )
    }

    fn lower_tensor_contraction(
        &mut self,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        contraction: TensorContraction,
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let fold_projection = self.materialize_function_values
            && (self.contains_function_fold_projection(lhs)
                || self.contains_function_fold_projection(rhs));
        if self.materialize_function_values && !fold_projection {
            return self.lower_materialized_contraction(lhs, rhs, contraction, scalar_type, span);
        }
        let mut terms = Vec::with_capacity(contraction.extent as usize);
        for contracted in 1..=contraction.extent {
            let contracted = gast::Expression::Integer(i64::from(contracted));
            let (lhs_indices, rhs_indices) = contraction_indices(&contraction, contracted);
            let lhs = self.lower_at(lhs, &lhs_indices)?;
            let rhs = self.lower_at(rhs, &rhs_indices)?;
            terms.push(lower_binary(
                dae::BinaryOperator::Multiply,
                lhs,
                rhs,
                scalar_type,
                span,
            )?);
        }
        let expression = sum_terms(
            terms,
            "zero-contraction",
            "zero-length tensor contraction needs an additive identity",
            span,
        )?;
        self.bound_expression(
            TypedExpression {
                expression,
                scalar_type,
            },
            span,
        )
    }

    fn lower_materialized_contraction(
        &mut self,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        contraction: TensorContraction,
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let accumulator = gast::Name::ident(format!(
            "rumoca_{}_contraction_{}",
            self.temporary_namespace, self.temporary_counter
        ));
        self.temporary_counter += 1;
        let iterator = gast::Name::ident(format!(
            "rumoca_{}_contracted_{}",
            self.temporary_namespace, self.temporary_counter
        ));
        self.temporary_counter += 1;
        self.temporary_locals.push(gast::VariableDeclaration {
            ty: gast::TypeRef::Primitive(scalar_type),
            name: accumulator.clone(),
            dimensions: Vec::new(),
            range: gast::RangeAttributes::default(),
            span,
        });
        let zero = match scalar_type {
            gast::ScalarType::Real => gast::Expression::Real(0.0),
            gast::ScalarType::Integer => gast::Expression::Integer(0),
            gast::ScalarType::Boolean => {
                unreachable!("checked tensor contractions are numeric")
            }
        };
        self.pending_prefix_statements.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: gast::Reference::local(accumulator.clone()),
                value: zero,
            },
            span,
        ));

        let contracted = gast::Expression::Ref(gast::Reference::local(iterator.clone()));
        let (lhs_indices, rhs_indices) = contraction_indices(&contraction, contracted);
        self.loop_index_bounds.push(LoopIndexBound {
            name: iterator.clone(),
            minimum: 1,
            maximum: i64::from(contraction.extent),
        });
        let body_start = self.pending_prefix_statements.len();
        let lhs = self.lower_at(lhs, &lhs_indices);
        let rhs = self.lower_at(rhs, &rhs_indices);
        self.loop_index_bounds.pop();
        let product = lower_binary(dae::BinaryOperator::Multiply, lhs?, rhs?, scalar_type, span)?;
        let prefixes = self.pending_prefix_statements.split_off(body_start);
        let (before, mut body) =
            user_functions::partition_tensor_prefixes(prefixes, std::slice::from_ref(&iterator));
        self.pending_prefix_statements.extend(before);
        body.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: gast::Reference::local(accumulator.clone()),
                value: gast::Expression::binary(
                    gast::BinaryOp::Add,
                    gast::Expression::Ref(gast::Reference::local(accumulator.clone())),
                    product,
                ),
            },
            span,
        ));
        self.pending_prefix_statements.push(gast::Spanned::new(
            gast::Statement::For(gast::ForLoop {
                iterator: Some(iterator),
                start: gast::Expression::Integer(1),
                step: None,
                stop: gast::Expression::Integer(i64::from(contraction.extent)),
                body,
            }),
            span,
        ));
        Ok(TypedExpression {
            expression: gast::Expression::Ref(gast::Reference::local(accumulator)),
            scalar_type,
        })
    }

    pub(super) fn lower_index_at(
        &mut self,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        projection: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let mut projected = projection.iter();
        let mut base_indices = Vec::with_capacity(
            self.view
                .expression(base)
                .expect("checked indexed base")
                .value_type()
                .dimensions()
                .len(),
        );
        for subscript in subscripts.iter() {
            match subscript {
                dae::SubscriptView::Index { expression, .. } => {
                    base_indices.push(self.lower(expression)?.expression);
                }
                dae::SubscriptView::Whole { .. } => {
                    base_indices.push(next_projected_index(&mut projected, "whole", span)?);
                }
                dae::SubscriptView::Slice { expression, .. } => {
                    let selected = next_projected_index(&mut projected, "slice", span)?;
                    base_indices.push(self.lower_slice_index(expression, selected, span)?);
                }
            }
        }
        base_indices.extend(projected.cloned());
        self.lower_at(base, &base_indices)
    }

    fn lower_slice_index(
        &mut self,
        expression: dae::ExprId<'dae>,
        projected: gast::Expression,
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let node = self
            .view
            .expression(expression)
            .expect("checked slice expression");
        if let dae::ExpressionOperation::Range(range) = node.operation() {
            if let Some(ordinal) = constant_integer(&projected) {
                return range
                    .start()
                    .value()
                    .checked_add((ordinal - 1).saturating_mul(range.effective_step()))
                    .map(gast::Expression::Integer)
                    .ok_or_else(|| slice_overflow_error(span));
            }
            let offset = gast::Expression::binary(
                gast::BinaryOp::Sub,
                projected,
                gast::Expression::Integer(1),
            );
            let scaled = if range.effective_step() == 1 {
                offset
            } else {
                gast::Expression::binary(
                    gast::BinaryOp::Mul,
                    offset,
                    gast::Expression::Integer(range.effective_step()),
                )
            };
            return Ok(gast::Expression::binary(
                gast::BinaryOp::Add,
                gast::Expression::Integer(range.start().value()),
                scaled,
            ));
        }
        self.lower_at(expression, std::slice::from_ref(&projected))
            .map(|value| value.expression)
    }

    pub(super) fn lower_array_at(
        &mut self,
        elements: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let (first, rest) = indices.split_first().ok_or_else(|| {
            unsupported(
                "array-projection",
                "array constructor requires an element index".to_owned(),
                span,
            )
        })?;
        let ordinal = match first {
            gast::Expression::Integer(value) => usize::try_from(*value)
                .ok()
                .and_then(|value| value.checked_sub(1)),
            _ => None,
        };
        if let Some(ordinal) = ordinal {
            let element = elements.get(ordinal).ok_or_else(|| {
                unsupported(
                    "array-constructor-index",
                    "array constructor projection is outside its checked extent".to_owned(),
                    span,
                )
            })?;
            return self.lower_at(element, rest);
        }
        let extent = u32::try_from(elements.len()).map_err(|_| {
            unsupported(
                "array-constructor-capacity",
                "array constructor extent exceeds the GALEC projection capacity".to_owned(),
                span,
            )
        })?;
        self.prove_dynamic_index(first, extent, span)?;
        let activation_operands = elements
            .iter()
            .map(|element| element.index())
            .collect::<Vec<_>>();
        let mut selected = Vec::with_capacity(elements.len());
        for (ordinal, element) in elements.iter().enumerate() {
            self.conditional_activation_path
                .push(ConditionalActivationKey {
                    kind: ConditionalActivationKind::ArraySelection,
                    operands: activation_operands.clone(),
                    branch: u32::try_from(ordinal).map_err(|_| {
                        GalecTargetError::LoweringInternal {
                            detail: "array-selection ordinal exceeds the activation-key capacity"
                                .to_owned(),
                        }
                    })?,
                });
            let prefix_start = self.pending_prefix_statements.len();
            let value = self.lower_at(element, rest);
            self.conditional_activation_path.pop();
            let value = value?;
            selected.push((
                ordinal + 1,
                SelectionValue {
                    prefix: self.pending_prefix_statements.split_off(prefix_start),
                    expression: value.expression,
                },
                value.scalar_type,
            ));
        }
        let (_, fallback, scalar_type) = selected
            .pop()
            .expect("checked array constructor is nonempty");
        let branches = selected
            .into_iter()
            .map(|(ordinal, value, branch_type)| {
                debug_assert_eq!(branch_type, scalar_type);
                SelectionBranch {
                    condition_prefix: Vec::new(),
                    condition: gast::Expression::binary(
                        gast::BinaryOp::Eq,
                        first.clone(),
                        gast::Expression::Integer(
                            i64::try_from(ordinal)
                                .expect("checked array ordinal fits the GALEC index type"),
                        ),
                    ),
                    value,
                }
            })
            .collect();
        Ok(self.lower_lazy_selection(branches, fallback, scalar_type, span))
    }

    pub(super) fn lower_elementwise_builtin(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        if builtin == dae::PureBuiltin::Transpose {
            return self.lower_transpose_element(arguments, indices);
        }
        if builtin == dae::PureBuiltin::Linspace {
            return self.lower_linspace_element(arguments, indices, span);
        }
        if builtin == dae::PureBuiltin::Cross {
            return self.lower_cross_element(arguments, indices, scalar_type, span);
        }
        if matches!(
            builtin,
            dae::PureBuiltin::PromotedCat1 | dae::PureBuiltin::PromotedCat2
        ) {
            return self.lower_promoted_concatenation_element(
                builtin,
                arguments,
                indices,
                scalar_type,
                span,
            );
        }
        if builtin == dae::PureBuiltin::Identity {
            return Ok(lower_identity_element(indices));
        }
        if builtin == dae::PureBuiltin::Diagonal {
            let [row, column] = indices else {
                unreachable!("checked diagonal result has rank two")
            };
            let diagonal = self.lower_at(
                arguments.get(0).expect("checked diagonal operand"),
                std::slice::from_ref(row),
            )?;
            let condition =
                gast::Expression::binary(gast::BinaryOp::Eq, row.clone(), column.clone());
            return Ok(TypedExpression {
                expression: match condition {
                    gast::Expression::Bool(true) => diagonal.expression,
                    gast::Expression::Bool(false) => gast::Expression::Real(0.0),
                    condition => gast::Expression::If(gast::IfExpression::new(
                        vec![(condition, diagonal.expression)],
                        gast::Expression::Real(0.0),
                    )),
                },
                scalar_type,
            });
        }
        if builtin == dae::PureBuiltin::Vector {
            let operand = arguments.get(0).expect("checked vector operand");
            let dimensions = self
                .view
                .expression(operand)
                .expect("checked vector operand resolves")
                .value_type()
                .dimensions();
            let projection = vector_operand_projection(dimensions, indices);
            return self.lower_at(operand, &projection);
        }
        if builtin == dae::PureBuiltin::Zeros || builtin == dae::PureBuiltin::Ones {
            return Ok(TypedExpression {
                expression: gast::Expression::Real(if builtin == dae::PureBuiltin::Ones {
                    1.0
                } else {
                    0.0
                }),
                scalar_type,
            });
        }
        if builtin == dae::PureBuiltin::Fill {
            return self.lower_at(arguments.get(0).expect("checked fill value argument"), &[]);
        }
        if matches!(
            builtin,
            dae::PureBuiltin::Smooth | dae::PureBuiltin::NoEvent | dae::PureBuiltin::Homotopy
        ) {
            let ordinal = usize::from(builtin == dae::PureBuiltin::Smooth);
            return self.lower_at(
                arguments
                    .get(ordinal)
                    .expect("checked transparent builtin argument"),
                indices,
            );
        }
        let mut lowered = Vec::with_capacity(arguments.len());
        for argument in arguments.iter() {
            let dimensions = self
                .view
                .expression(argument)
                .expect("checked builtin argument")
                .value_type()
                .dimensions();
            let projection = operand_projection(dimensions, indices, span)?;
            lowered.push(self.lower_at(argument, &projection)?);
        }
        let expression = lower_builtin_arguments(builtin, lowered, span)?;
        Ok(TypedExpression {
            expression,
            scalar_type,
        })
    }

    fn lower_transpose_element(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
    ) -> Result<TypedExpression, GalecTargetError> {
        let operand = arguments.get(0).expect("checked transpose operand");
        let mut projection = indices.to_vec();
        debug_assert!(
            projection.len() >= 2,
            "checked transpose has rank at least two"
        );
        projection.swap(0, 1);
        self.lower_at(operand, &projection)
    }

    fn lower_promoted_concatenation_element(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let axis = usize::from(builtin == dae::PureBuiltin::PromotedCat2);
        let selected = indices
            .get(axis)
            .expect("checked concatenation projection has its concatenated axis");
        let Some(coordinate) = constant_integer(selected) else {
            return self.lower_dynamic_concatenation_element(
                arguments,
                indices,
                axis,
                scalar_type,
                span,
            );
        };
        let mut offset = 0_i64;
        for argument in arguments.iter() {
            let dimensions = self
                .view
                .expression(argument)
                .expect("checked concatenation operand resolves")
                .value_type()
                .dimensions();
            let extent = dimensions.get(axis).copied().unwrap_or(1);
            let upper = offset + i64::from(extent);
            if coordinate > offset && coordinate <= upper {
                let mut projected = indices.to_vec();
                projected[axis] = gast::Expression::Integer(coordinate - offset);
                truncate_promoted_projection(&mut projected, dimensions.len());
                return Ok(TypedExpression {
                    expression: coerce(self.lower_at(argument, &projected)?, scalar_type, span)?,
                    scalar_type,
                });
            }
            offset = upper;
        }
        Err(unsupported(
            "concatenation-projection",
            "concatenation projection is outside its checked extent".to_owned(),
            span,
        ))
    }

    fn lower_dynamic_concatenation_element(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        axis: usize,
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let coordinate = indices
            .get(axis)
            .expect("checked concatenation projection has its concatenated axis");
        let mut offset = 0_i64;
        let activation_operands = arguments
            .iter()
            .map(|argument| argument.index())
            .collect::<Vec<_>>();
        let mut values = Vec::with_capacity(arguments.len());
        for (ordinal, argument) in arguments.iter().enumerate() {
            let dimensions = self
                .view
                .expression(argument)
                .expect("checked concatenation operand resolves")
                .value_type()
                .dimensions();
            let extent = dimensions.get(axis).copied().unwrap_or(1);
            let upper = offset + i64::from(extent);
            let mut projected = indices.to_vec();
            projected[axis] = if offset == 0 {
                coordinate.clone()
            } else {
                gast::Expression::binary(
                    gast::BinaryOp::Sub,
                    coordinate.clone(),
                    gast::Expression::Integer(offset),
                )
            };
            truncate_promoted_projection(&mut projected, dimensions.len());
            self.conditional_activation_path
                .push(ConditionalActivationKey {
                    kind: ConditionalActivationKind::Concatenation,
                    operands: activation_operands.clone(),
                    branch: u32::try_from(ordinal).map_err(|_| {
                        GalecTargetError::LoweringInternal {
                            detail: "concatenation ordinal exceeds the activation-key capacity"
                                .to_owned(),
                        }
                    })?,
                });
            let prefix_start = self.pending_prefix_statements.len();
            let value = self.lower_at(argument, &projected);
            self.conditional_activation_path.pop();
            let value = coerce(value?, scalar_type, span)?;
            values.push((
                upper,
                SelectionValue {
                    prefix: self.pending_prefix_statements.split_off(prefix_start),
                    expression: value,
                },
            ));
            offset = upper;
        }
        let (_, fallback) = values.pop().ok_or_else(|| {
            unsupported(
                "empty-concatenation",
                "checked concatenation requires at least one operand".to_owned(),
                span,
            )
        })?;
        let branches = values
            .into_iter()
            .map(|(upper, value)| SelectionBranch {
                condition_prefix: Vec::new(),
                condition: gast::Expression::binary(
                    gast::BinaryOp::Le,
                    coordinate.clone(),
                    gast::Expression::Integer(upper),
                ),
                value,
            })
            .collect::<Vec<_>>();
        Ok(self.lower_lazy_selection(branches, fallback, scalar_type, span))
    }

    pub(super) fn lower_reduction(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        scalar_type: gast::ScalarType,
    ) -> Result<TypedExpression, GalecTargetError> {
        let operand = arguments.get(0).expect("checked reduction operand");
        let dimensions = self
            .view
            .expression(operand)
            .expect("checked reduction operand resolves")
            .value_type()
            .dimensions();
        let operator = if builtin == dae::PureBuiltin::Sum {
            gast::BinaryOp::Add
        } else {
            gast::BinaryOp::Mul
        };
        let mut values = Vec::new();
        for indices in row_major_indices(dimensions) {
            let indices = indices
                .into_iter()
                .map(|index| gast::Expression::Integer(i64::from(index)))
                .collect::<Vec<_>>();
            values.push(self.lower_at(operand, &indices)?.expression);
        }
        let expression = values
            .into_iter()
            .reduce(|lhs, rhs| gast::Expression::binary(operator, lhs, rhs))
            .unwrap_or_else(|| reduction_identity(builtin, scalar_type));
        Ok(TypedExpression {
            expression,
            scalar_type,
        })
    }

    pub(super) fn lower_linspace_element(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let [index] = indices else {
            unreachable!("checked linspace result has rank one")
        };
        let start = coerce(
            self.lower_at(arguments.get(0).expect("checked linspace start"), &[])?,
            gast::ScalarType::Real,
            span,
        )?;
        let stop = coerce(
            self.lower_at(arguments.get(1).expect("checked linspace stop"), &[])?,
            gast::ScalarType::Real,
            span,
        )?;
        let count = coerce(
            self.lower_at(arguments.get(2).expect("checked linspace extent"), &[])?,
            gast::ScalarType::Real,
            span,
        )?;
        let index = coerce(
            TypedExpression {
                expression: index.clone(),
                scalar_type: gast::ScalarType::Integer,
            },
            gast::ScalarType::Real,
            span,
        )?;
        let one = gast::Expression::Real(1.0);
        let offset = gast::Expression::binary(gast::BinaryOp::Sub, index, one.clone());
        let width = gast::Expression::binary(gast::BinaryOp::Sub, count, one);
        let delta = gast::Expression::binary(gast::BinaryOp::Sub, stop, start.clone());
        let scaled = gast::Expression::binary(gast::BinaryOp::Mul, delta, offset);
        Ok(TypedExpression {
            expression: gast::Expression::binary(
                gast::BinaryOp::Add,
                start,
                gast::Expression::binary(gast::BinaryOp::Div, scaled, width),
            ),
            scalar_type: gast::ScalarType::Real,
        })
    }

    pub(super) fn lower_cross_element(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let [index] = indices else {
            unreachable!("checked cross result has rank one")
        };
        let mut component = |ordinal: usize| {
            let (first, second) = [(1, 2), (2, 0), (0, 1)][ordinal];
            let first = [gast::Expression::Integer(i64::from(first + 1))];
            let second = [gast::Expression::Integer(i64::from(second + 1))];
            let lhs_first = self.lower_at(arguments.get(0).expect("checked cross lhs"), &first)?;
            let rhs_second =
                self.lower_at(arguments.get(1).expect("checked cross rhs"), &second)?;
            let positive = lower_binary(
                dae::BinaryOperator::Multiply,
                lhs_first,
                rhs_second,
                scalar_type,
                span,
            )?;
            let lhs_second =
                self.lower_at(arguments.get(0).expect("checked cross lhs"), &second)?;
            let rhs_first = self.lower_at(arguments.get(1).expect("checked cross rhs"), &first)?;
            let negative = lower_binary(
                dae::BinaryOperator::Multiply,
                lhs_second,
                rhs_first,
                scalar_type,
                span,
            )?;
            Ok::<_, GalecTargetError>(gast::Expression::binary(
                gast::BinaryOp::Sub,
                positive,
                negative,
            ))
        };
        let expression = if let gast::Expression::Integer(index) = index {
            component(usize::try_from(*index - 1).expect("checked cross index is 1..=3"))?
        } else {
            let first = component(0)?;
            let second = component(1)?;
            let third = component(2)?;
            gast::Expression::If(gast::IfExpression::new(
                vec![
                    (
                        gast::Expression::binary(
                            gast::BinaryOp::Eq,
                            index.clone(),
                            gast::Expression::Integer(1),
                        ),
                        first,
                    ),
                    (
                        gast::Expression::binary(
                            gast::BinaryOp::Eq,
                            index.clone(),
                            gast::Expression::Integer(2),
                        ),
                        second,
                    ),
                ],
                third,
            ))
        };
        Ok(TypedExpression {
            expression,
            scalar_type,
        })
    }

    pub(super) fn coordinate_at(
        &mut self,
        coordinate: dae::CoordinateView<'dae>,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        match coordinate {
            dae::CoordinateView::ClockInterval(clock) => Ok(TypedExpression {
                expression: gast::Expression::Real(
                    self.view.periodic_clock(clock).period_seconds(),
                ),
                scalar_type: gast::ScalarType::Real,
            }),
            dae::CoordinateView::Binder(binder) => self.binder_coordinate(binder),
            dae::CoordinateView::FunctionParameter(parameter) => {
                self.function_parameter_coordinate(parameter, indices, span)
            }
            _ => {
                if let Some(value) = self.inline_algebraic_coordinate(coordinate, indices)? {
                    return Ok(value);
                }
                self.variable_coordinate(coordinate, indices, span)
            }
        }
    }

    fn binder_coordinate(
        &self,
        binder: dae::DomainBinderId<'dae>,
    ) -> Result<TypedExpression, GalecTargetError> {
        let expression = self
            .comprehension_frames
            .iter()
            .rev()
            .find(|frame| frame.domain == binder.domain().index())
            .and_then(|frame| frame.binders.get(binder.ordinal() as usize))
            .cloned()
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: "domain binder used without its checked comprehension frame".to_owned(),
            })?;
        Ok(TypedExpression {
            expression,
            scalar_type: gast::ScalarType::Integer,
        })
    }

    fn inline_algebraic_coordinate(
        &mut self,
        coordinate: dae::CoordinateView<'dae>,
        indices: &[gast::Expression],
    ) -> Result<Option<TypedExpression>, GalecTargetError> {
        let materialized_algebraic = match coordinate {
            dae::CoordinateView::Algebraic(variable) => {
                self.by_id
                    .get(&dae::VariableId::from(variable).index())
                    .is_some_and(|variable| variable.class == VariableClass::Local)
                    && !self.inline_causal_locals
            }
            _ => false,
        };
        if let dae::CoordinateView::Algebraic(variable) = coordinate
            && !materialized_algebraic
            && let Some(definition) = self.definitions.definition(variable)
        {
            return self.lower_at(definition, indices).map(Some);
        }
        if let dae::CoordinateView::Algebraic(variable) = coordinate
            && !materialized_algebraic
        {
            let variable_id = dae::VariableId::from(variable);
            let dimensions = self
                .view
                .variable(variable_id)
                .expect("checked algebraic variable resolves")
                .value_type()
                .dimensions();
            if let Some(scalar) = literal_scalar_index(dimensions, indices)
                && let Some(definition) = self
                    .definitions
                    .scalar_definition_for_variable(variable_id, scalar)
            {
                return self.lower_at(definition, &[]).map(Some);
            }
        }
        Ok(None)
    }

    fn function_parameter_coordinate(
        &mut self,
        parameter: dae::FunctionParameterId<'dae>,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        if self.function_scope != Some(parameter.function()) {
            let argument = self
                .call_frames
                .iter()
                .rev()
                .find(|frame| frame.function == parameter.function())
                .and_then(|frame| frame.arguments.get(parameter.ordinal() as usize))
                .copied()
                .ok_or_else(|| GalecTargetError::LoweringInternal {
                    detail: "function parameter used without its checked call frame".to_owned(),
                })?;
            return self.lower_at(argument, indices);
        }
        let parameter = self
            .view
            .function(parameter.function())
            .expect("checked function identity resolves")
            .parameters()
            .find(|candidate| candidate.id() == parameter)
            .expect("checked function parameter resolves");
        let value_type = self
            .view
            .value_type(parameter.value_type())
            .expect("checked function parameter type resolves");
        let scalar_type = scalar_type(
            value_type.scalar_type(),
            parameter.name().as_str(),
            parameter.declaration().span(),
        )?;
        Ok(TypedExpression {
            expression: self.lower_local_reference(
                user_functions::parameter_name(parameter)?,
                value_type.dimensions(),
                indices,
                span,
            )?,
            scalar_type,
        })
    }

    pub(super) fn lower_local_reference(
        &mut self,
        name: gast::Name,
        dimensions: &[u32],
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        if dimensions.len() != indices.len() {
            return Err(unsupported(
                "dynamic-array-index",
                "function-local reference does not have one index per checked dimension".to_owned(),
                span,
            ));
        }
        let indices = indices
            .iter()
            .map(|index| {
                constant_integer(index)
                    .map(gast::Expression::Integer)
                    .unwrap_or_else(|| index.clone())
            })
            .collect::<Vec<_>>();
        if indices
            .iter()
            .all(|index| self.is_loop_index_expression(index))
        {
            return Ok(gast::Expression::Ref(gast::Reference::Local(
                gast::RefPart {
                    name,
                    subscripts: indices,
                    span,
                },
            )));
        }
        self.guard_local_reference_bounds(&indices, dimensions, span);
        lower_bounded_reference_selection(
            gast::Reference::Local(gast::RefPart {
                name,
                subscripts: indices,
                span,
            }),
            dimensions,
            span,
        )
    }

    fn guard_local_reference_bounds(
        &mut self,
        indices: &[gast::Expression],
        dimensions: &[u32],
        span: Span,
    ) {
        let invalid = indices
            .iter()
            .zip(dimensions)
            .filter(|(index, _)| !matches!(index, gast::Expression::Integer(_)))
            .flat_map(|(index, extent)| {
                [
                    gast::Expression::binary(
                        gast::BinaryOp::Lt,
                        index.clone(),
                        gast::Expression::Integer(1),
                    ),
                    gast::Expression::binary(
                        gast::BinaryOp::Gt,
                        index.clone(),
                        gast::Expression::Integer(i64::from(*extent)),
                    ),
                ]
            })
            .reduce(|lhs, rhs| gast::Expression::binary(gast::BinaryOp::Or, lhs, rhs))
            .expect("a dynamic local reference has at least one runtime index");
        self.pending_prefix_statements.push(gast::Spanned::new(
            gast::Statement::If(gast::IfStatement {
                branches: vec![gast::IfBranch {
                    condition: gast::Condition::Expression(invalid),
                    body: vec![gast::Spanned::new(
                        gast::Statement::Signal(vec![gast::Identifier::new(
                            gast::PredefinedSignal::InvalidArgument.name(),
                        )]),
                        span,
                    )],
                    span,
                }],
                else_body: None,
            }),
            span,
        ));
    }

    fn variable_coordinate(
        &mut self,
        coordinate: dae::CoordinateView<'dae>,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let (variable, previous) = coordinate_variable(coordinate, span)?;
        let classified = self.by_id.get(&variable.index()).ok_or_else(|| {
            GalecTargetError::UnknownVariableReference {
                name: format!("#{}", variable.index()),
                span: Some(span),
            }
        })?;
        let name = if previous {
            self.pre_names
                .get(&variable.index())
                .ok_or_else(|| GalecTargetError::LoweringInternal {
                    detail: format!(
                        "pre-coordinate for `{}` was not collected",
                        classified.variable.name()
                    ),
                })?
                .clone()
        } else {
            classified.name.clone()
        };
        let expression = if classified.class == VariableClass::Local {
            gast::Expression::Ref(gast::Reference::Local(gast::RefPart {
                name,
                subscripts: indices.to_vec(),
                span,
            }))
        } else if indices
            .iter()
            .all(|index| matches!(index, gast::Expression::Integer(_)))
        {
            gast::Expression::Ref(state_reference_with_subscripts(
                name,
                indices.to_vec(),
                span,
            ))
        } else {
            self.lower_dynamic_reference(classified, name, indices, span)?
        };
        Ok(TypedExpression {
            expression,
            scalar_type: classified.scalar_type,
        })
    }

    pub(super) fn lower_comprehension_at(
        &mut self,
        domain: dae::DomainId<'dae>,
        body: dae::ExprId<'dae>,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let domain_view = self
            .view
            .domain(domain)
            .expect("checked comprehension domain resolves");
        let binder_count = domain_view.extents().len();
        let (ordinals, body_indices) = indices.split_at_checked(binder_count).ok_or_else(|| {
            unsupported(
                "comprehension-projection",
                "comprehension projection lacks one ordinal per checked binder".to_owned(),
                span,
            )
        })?;
        let binders = domain_view
            .structured()
            .binders
            .iter()
            .zip(ordinals)
            .map(|(binder, ordinal)| comprehension_binder_value(binder, ordinal.clone()))
            .collect();
        self.comprehension_frames.push(ComprehensionFrame {
            domain: domain.index(),
            binders,
        });
        let result = self.lower_at(body, body_indices);
        self.comprehension_frames.pop();
        result
    }

    fn lower_dynamic_reference(
        &self,
        classified: &ClassifiedVariable<'dae>,
        name: gast::Name,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let dimensions = classified.variable.value_type().dimensions();
        let indices = indices
            .iter()
            .map(|index| {
                constant_integer(index)
                    .map(gast::Expression::Integer)
                    .unwrap_or_else(|| index.clone())
            })
            .collect::<Vec<_>>();
        if dimensions.len() != indices.len() {
            return Err(unsupported(
                "dynamic-array-index",
                "dynamic reference does not have one index per checked dimension".to_owned(),
                span,
            ));
        }
        for (index, extent) in indices.iter().zip(dimensions) {
            if !matches!(index, gast::Expression::Integer(_)) {
                self.prove_dynamic_index(index, *extent, span)?;
            }
        }
        if indices
            .iter()
            .all(|index| self.is_loop_index_expression(index))
        {
            return Ok(gast::Expression::Ref(state_reference_with_subscripts(
                name, indices, span,
            )));
        }
        lower_bounded_reference_selection(
            gast::Reference::State(vec![gast::RefPart {
                name,
                subscripts: indices,
                span,
            }]),
            dimensions,
            span,
        )
    }

    fn is_loop_index_expression(&self, expression: &gast::Expression) -> bool {
        match expression {
            gast::Expression::Integer(_) => true,
            gast::Expression::Paren(value) => self.is_loop_index_expression(value),
            gast::Expression::Ref(gast::Reference::Local(part)) => {
                part.subscripts.is_empty()
                    && self
                        .loop_index_bounds
                        .iter()
                        .any(|bound| bound.name.lexeme() == part.name.lexeme())
            }
            gast::Expression::Binary { lhs, rhs, .. } => {
                self.is_loop_index_expression(lhs) && self.is_loop_index_expression(rhs)
            }
            _ => false,
        }
    }

    pub(super) fn prove_dynamic_index(
        &self,
        index: &gast::Expression,
        extent: u32,
        span: Span,
    ) -> Result<(), GalecTargetError> {
        if self
            .integer_expression_bounds(index)
            .is_some_and(|(minimum, maximum)| minimum >= 1 && maximum <= i64::from(extent))
        {
            Ok(())
        } else {
            Err(unsupported(
                "dynamic-array-index",
                format!("dynamic index `{index:?}` lacks proven bounds within 1:{extent}"),
                span,
            ))
        }
    }

    pub(super) fn integer_expression_bounds(&self, expression: &gast::Expression) -> Option<(i64, i64)> {
        match expression {
            gast::Expression::Integer(value) => Some((*value, *value)),
            gast::Expression::Paren(value) => self.integer_expression_bounds(value),
            // An if-expression evaluates to exactly one of its branch values,
            // so the union of their bounds bounds the whole expression whatever
            // the conditions do. This is what proves an ordinal selected by an
            // index-list array update in range: the arms are the list
            // positions, so the union is the length of the list.
            gast::Expression::If(selection) => {
                let mut bounds = self.integer_expression_bounds(&selection.else_value)?;
                for (_, value) in &selection.branches {
                    let (minimum, maximum) = self.integer_expression_bounds(value)?;
                    bounds = (bounds.0.min(minimum), bounds.1.max(maximum));
                }
                Some(bounds)
            }
            gast::Expression::Ref(gast::Reference::Local(part)) if part.subscripts.is_empty() => {
                self.loop_index_bounds
                    .iter()
                    .rev()
                    .find(|bound| bound.name.lexeme() == part.name.lexeme())
                    .map(|bound| (bound.minimum, bound.maximum))
                    .or_else(|| {
                        self.local_integer_bounds
                            .get(part.name.lexeme())
                            .copied()
                    })
            }
            gast::Expression::Ref(gast::Reference::State(parts)) if parts.len() == 1 => {
                let part = &parts[0];
                let variable = self
                    .by_id
                    .values()
                    .find(|variable| variable.name.lexeme() == part.name.lexeme())
                    .or_else(|| {
                        self.pre_names
                            .iter()
                            .find(|(_, name)| name.lexeme() == part.name.lexeme())
                            .and_then(|(id, _)| self.by_id.get(id))
                    })?;
                Some((
                    variable
                        .variable
                        .minimum()
                        .and_then(|value| literal_integer(self.view, value))?,
                    variable
                        .variable
                        .maximum()
                        .and_then(|value| literal_integer(self.view, value))?,
                ))
            }
            gast::Expression::Binary { op, lhs, rhs } => {
                let lhs = self.integer_expression_bounds(lhs)?;
                let rhs = self.integer_expression_bounds(rhs)?;
                match op {
                    gast::BinaryOp::Add => {
                        Some((lhs.0.checked_add(rhs.0)?, lhs.1.checked_add(rhs.1)?))
                    }
                    gast::BinaryOp::Sub => {
                        Some((lhs.0.checked_sub(rhs.1)?, lhs.1.checked_sub(rhs.0)?))
                    }
                    gast::BinaryOp::Mul => {
                        let products = [
                            lhs.0.checked_mul(rhs.0)?,
                            lhs.0.checked_mul(rhs.1)?,
                            lhs.1.checked_mul(rhs.0)?,
                            lhs.1.checked_mul(rhs.1)?,
                        ];
                        Some((*products.iter().min()?, *products.iter().max()?))
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }
}

fn lower_bounded_reference_selection(
    reference: gast::Reference,
    dimensions: &[u32],
    span: Span,
) -> Result<gast::Expression, GalecTargetError> {
    gast::IfExpression::bounded_selection(reference, dimensions.to_vec())
        .map(gast::Expression::If)
        .map_err(|error| unsupported("dynamic-array-index", error.to_string(), span))
}
