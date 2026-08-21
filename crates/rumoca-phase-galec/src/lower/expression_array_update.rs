//! Indexed array-update lowering for [`ExpressionLowerer`].
//!
//! DAE keeps an indexed Modelica assignment compact as
//! `ArrayUpdate(base, value, subscripts)`, while GALEC exposes scalar
//! assignments. These methods expand one row-major coordinate at a time and
//! decide, per axis, whether that coordinate selects the updated value or the
//! historical base.

use super::*;
use super::expression_projection::{SelectionBranch, SelectionValue};

type ArrayUpdateSelection = (Vec<gast::Expression>, Vec<gast::Expression>);

enum ArrayUpdateAxis {
    UpdatedIndex,
    DynamicIndex(gast::Expression),
    UpdatedValue(gast::Expression),
    ConditionalValue {
        index: gast::Expression,
        condition: gast::Expression,
    },
    Historical,
}

impl<'a, 'dae> ExpressionLowerer<'a, 'dae> {

    /// Project one scalar from a checked tensor SSA update.
    ///
    /// GALEC exposes scalar assignments, while DAE deliberately retains an
    /// indexed Modelica assignment as `ArrayUpdate(base, value, subscripts)`.
    /// Row-major target expansion supplies literal coordinates here. Those
    /// coordinates select either the updated value or the historical base;
    /// the aggregate owner itself remains compact in DAE.
    pub(super) fn lower_array_update_at(
        &mut self,
        base: dae::ExprId<'dae>,
        value: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let base_dimensions = self
            .view
            .expression(base)
            .expect("checked array-update base resolves")
            .value_type()
            .dimensions();
        for (index, &extent) in indices.iter().zip(base_dimensions) {
            self.prove_dynamic_index(index, extent, span)?;
        }
        let Some((value_indices, dynamic_conditions)) =
            self.array_update_value_indices(subscripts, indices, base_dimensions, span)?
        else {
            return self.lower_at(base, indices);
        };
        let activation_operands = vec![base.index(), value.index()];
        self.conditional_activation_path
            .push(ConditionalActivationKey {
                kind: ConditionalActivationKind::ArrayUpdate,
                operands: activation_operands.clone(),
                branch: 0,
            });
        let updated_start = self.pending_prefix_statements.len();
        let updated = self.lower_at(value, &value_indices);
        self.conditional_activation_path.pop();
        let updated = updated?;
        let Some(condition) = dynamic_conditions
            .into_iter()
            .reduce(|lhs, rhs| gast::Expression::binary(gast::BinaryOp::And, lhs, rhs))
        else {
            return Ok(updated);
        };
        let updated = SelectionValue {
            prefix: self.pending_prefix_statements.split_off(updated_start),
            expression: updated.expression,
        };
        self.conditional_activation_path
            .push(ConditionalActivationKey {
                kind: ConditionalActivationKind::ArrayUpdate,
                operands: activation_operands,
                branch: 1,
            });
        let historical_start = self.pending_prefix_statements.len();
        let historical = self.lower_at(base, indices);
        self.conditional_activation_path.pop();
        let historical = historical?;
        let scalar_type = historical.scalar_type;
        let fallback = SelectionValue {
            prefix: self.pending_prefix_statements.split_off(historical_start),
            expression: historical.expression,
        };
        Ok(self.lower_lazy_selection(
            vec![SelectionBranch {
                condition_prefix: Vec::new(),
                condition,
                value: updated,
            }],
            fallback,
            scalar_type,
            span,
        ))
    }

    fn array_update_value_indices(
        &mut self,
        subscripts: dae::SubscriptsView<'dae>,
        projected: &[gast::Expression],
        base_dimensions: &[u32],
        span: Span,
    ) -> Result<Option<ArrayUpdateSelection>, GalecTargetError> {
        let mut value_indices = Vec::new();
        let mut dynamic_conditions = Vec::new();
        for (axis, (coordinate, &extent)) in projected.iter().zip(base_dimensions).enumerate() {
            match self.array_update_axis(subscripts.get(axis), coordinate, extent, span)? {
                ArrayUpdateAxis::UpdatedIndex => {}
                ArrayUpdateAxis::DynamicIndex(selected) => {
                    dynamic_conditions.push(gast::Expression::binary(
                        gast::BinaryOp::Eq,
                        selected,
                        coordinate.clone(),
                    ));
                }
                ArrayUpdateAxis::UpdatedValue(index) => value_indices.push(index),
                ArrayUpdateAxis::ConditionalValue { index, condition } => {
                    value_indices.push(index);
                    dynamic_conditions.push(condition);
                }
                ArrayUpdateAxis::Historical => return Ok(None),
            }
        }
        Ok(Some((value_indices, dynamic_conditions)))
    }

    fn array_update_axis(
        &mut self,
        subscript: Option<dae::SubscriptView<'dae>>,
        coordinate: &gast::Expression,
        extent: u32,
        span: Span,
    ) -> Result<ArrayUpdateAxis, GalecTargetError> {
        match subscript {
            Some(dae::SubscriptView::Index { expression, .. }) => {
                let selected = self.lower(expression)?;
                Ok(
                    match (
                        constant_integer(&selected.expression),
                        constant_integer(coordinate),
                    ) {
                        (Some(selected), Some(coordinate)) if selected == coordinate => {
                            ArrayUpdateAxis::UpdatedIndex
                        }
                        (Some(_), Some(_)) => ArrayUpdateAxis::Historical,
                        _ => {
                            self.prove_dynamic_index(&selected.expression, extent, span)?;
                            ArrayUpdateAxis::DynamicIndex(selected.expression)
                        }
                    },
                )
            }
            Some(dae::SubscriptView::Whole { .. }) | None => {
                Ok(ArrayUpdateAxis::UpdatedValue(coordinate.clone()))
            }
            Some(dae::SubscriptView::Slice { expression, .. }) => {
                self.array_update_slice_axis(expression, coordinate, extent)
            }
        }
    }

    fn array_update_slice_axis(
        &mut self,
        slice: dae::ExprId<'dae>,
        coordinate: &gast::Expression,
        extent: u32,
    ) -> Result<ArrayUpdateAxis, GalecTargetError> {
        // An index list is its own kind of subscript, not a range, and it is
        // the shape a row swap takes: `a[{i, j}, :] := a[{j, i}, :]`. Handle it
        // before the constant-coordinate shortcut, because the listed indices
        // are runtime values even when the coordinate being written is static.
        {
            let node = self
                .view
                .expression(slice)
                .expect("checked array-update slice resolves");
            if let dae::ExpressionOperation::Array(operands) = node.operation() {
                let span = node.provenance().span();
                let elements: Vec<_> = operands.iter().collect();
                return self.array_update_index_list_axis(&elements, coordinate, span);
            }
        }
        if let Some(coordinate) = constant_integer(coordinate) {
            return Ok(self
                .static_slice_ordinal(slice, coordinate, extent)?
                .map_or(ArrayUpdateAxis::Historical, |ordinal| {
                    ArrayUpdateAxis::UpdatedValue(gast::Expression::Integer(ordinal))
                }));
        }
        let node = self
            .view
            .expression(slice)
            .expect("checked array-update slice resolves");
        let dae::ExpressionOperation::Range(range) = node.operation() else {
            return Err(unsupported(
                "dynamic-array-update-slice",
                "array-update slice requires a constructor-proven range".to_owned(),
                node.provenance().span(),
            ));
        };
        let start = range.start().value();
        let stop = range.stop().value();
        let step = range.effective_step();
        if !matches!(step, -1 | 1) {
            return Err(unsupported(
                "strided-dynamic-array-update-slice",
                "dynamic array-update slices currently require a unit stride".to_owned(),
                node.provenance().span(),
            ));
        }
        let (lower, upper) = if step > 0 {
            (start, stop)
        } else {
            (stop, start)
        };
        let lower_bound = gast::Expression::binary(
            gast::BinaryOp::Ge,
            coordinate.clone(),
            gast::Expression::Integer(lower),
        );
        let upper_bound = gast::Expression::binary(
            gast::BinaryOp::Le,
            coordinate.clone(),
            gast::Expression::Integer(upper),
        );
        let origin = if step > 0 {
            start.checked_sub(1)
        } else {
            start.checked_add(1)
        }
        .ok_or_else(|| {
            unsupported(
                "dynamic-array-update-slice-overflow",
                "dynamic array-update slice index arithmetic overflowed".to_owned(),
                node.provenance().span(),
            )
        })?;
        let offset = if step > 0 {
            gast::Expression::binary(
                gast::BinaryOp::Sub,
                coordinate.clone(),
                gast::Expression::Integer(origin),
            )
        } else {
            gast::Expression::binary(
                gast::BinaryOp::Sub,
                gast::Expression::Integer(origin),
                coordinate.clone(),
            )
        };
        Ok(ArrayUpdateAxis::ConditionalValue {
            index: offset,
            condition: gast::Expression::binary(gast::BinaryOp::And, lower_bound, upper_bound),
        })
    }

    /// Lower an index-list subscript appearing in an array-update target.
    ///
    /// MLS §10.5 gives an index list its own ordinal axis: entry `k` of the
    /// list names the target coordinate that receives source ordinal `k`. The
    /// entries are ordinary expressions, so for a runtime list such as
    /// `{column, pivotRow}` neither the set of written coordinates nor their
    /// order is known at projection time.
    ///
    /// The axis is therefore a selection rather than an offset: the update
    /// applies to a coordinate exactly when that coordinate appears in the
    /// list, and the source ordinal is the position at which it appears.
    ///
    /// The entries are never used as subscripts in the emitted code, only
    /// compared against a coordinate the target provably has, so this lowering
    /// cannot address outside the array and needs no range proof of its own.
    /// That matters because the entries of a real swap are search results: a
    /// pivot row is not a constructor-proven range and never will be.
    ///
    /// MLS §10.5.1 makes a repeated index in an assignment target invalid. The
    /// first match wins here, which keeps the emitted code deterministic
    /// instead of order-dependent if a model violates that rule.
    fn array_update_index_list_axis(
        &mut self,
        elements: &[dae::ExprId<'dae>],
        coordinate: &gast::Expression,
        span: Span,
    ) -> Result<ArrayUpdateAxis, GalecTargetError> {
        if elements.is_empty() {
            return Err(unsupported(
                "empty-array-update-index-list",
                "array-update index list selects no coordinate".to_owned(),
                span,
            ));
        }
        let mut branches = Vec::with_capacity(elements.len());
        let mut condition: Option<gast::Expression> = None;
        for (position, element) in elements.iter().enumerate() {
            let selected = self.array_update_index_list_element(*element, span)?;
            let selects =
                gast::Expression::binary(gast::BinaryOp::Eq, coordinate.clone(), selected);
            let ordinal = i64::try_from(position + 1).map_err(|_| {
                unsupported(
                    "array-update-index-list-overflow",
                    "array-update index list is longer than the ordinal range".to_owned(),
                    span,
                )
            })?;
            branches.push((selects.clone(), gast::Expression::Integer(ordinal)));
            condition = Some(match condition {
                None => selects,
                Some(previous) => {
                    gast::Expression::binary(gast::BinaryOp::Or, previous, selects)
                }
            });
        }
        let condition = condition.expect("index list proven non-empty above");
        // The else value is unreachable: the guard above admits the selection
        // only for coordinates that match a branch. Ordinal 1 keeps the
        // expression total and in range for any consumer that evaluates it
        // unconditionally.
        let index = gast::Expression::If(gast::IfExpression::new(
            branches,
            gast::Expression::Integer(1),
        ));
        Ok(ArrayUpdateAxis::ConditionalValue { index, condition })
    }

    /// Bind one index-list entry to a name that every coordinate of the same
    /// array update can share.
    ///
    /// The axis is decided per target coordinate, so the entry expression is
    /// asked for once per coordinate, and an array update reads the array
    /// produced by the previous update. Inlining the entry at each of those
    /// points multiplies it by the coordinate count and then again along the
    /// update chain. Where the entry is a search, as a pivot row is, that
    /// product is what turns a dense solve into an unbuildable expression.
    /// Evaluating it once into a local and comparing against the local keeps
    /// the emitted size linear in the number of coordinates.
    ///
    /// Literals and plain references are already single reads, so they are
    /// used directly rather than given a name of their own.
    fn array_update_index_list_element(
        &mut self,
        element: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        if let Some(name) = self.array_update_index_locals.get(&element.index()) {
            return Ok(gast::Expression::Ref(gast::Reference::local(name.clone())));
        }
        let lowered = self.lower(element)?;
        if matches!(
            lowered.expression,
            gast::Expression::Integer(_) | gast::Expression::Ref(_)
        ) {
            return Ok(lowered.expression);
        }
        let target = gast::Name::ident(format!(
            "rumoca_{}_index_{}",
            self.temporary_namespace, self.temporary_counter
        ));
        self.temporary_counter += 1;
        self.temporary_locals.push(gast::VariableDeclaration {
            ty: gast::TypeRef::Primitive(gast::ScalarType::Integer),
            name: target.clone(),
            dimensions: Vec::new(),
            range: gast::RangeAttributes::default(),
            span,
        });
        self.pending_prefix_statements.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: gast::Reference::local(target.clone()),
                value: lowered.expression,
            },
            span,
        ));
        self.array_update_index_locals
            .insert(element.index(), target.clone());
        Ok(gast::Expression::Ref(gast::Reference::local(target)))
    }

    fn static_slice_ordinal(
        &mut self,
        slice: dae::ExprId<'dae>,
        coordinate: i64,
        extent: u32,
    ) -> Result<Option<i64>, GalecTargetError> {
        let node = self
            .view
            .expression(slice)
            .expect("checked array-update slice resolves");
        let dae::ExpressionOperation::Range(range) = node.operation() else {
            return Err(unsupported(
                "dynamic-array-update-slice",
                "array-update slice requires a constructor-proven range".to_owned(),
                node.provenance().span(),
            ));
        };
        let start = range.start().value();
        let stop = range.stop().value();
        let step = range.effective_step();
        let in_direction = if step > 0 {
            coordinate >= start && coordinate <= stop
        } else {
            coordinate <= start && coordinate >= stop
        };
        let delta = coordinate - start;
        if !in_direction || step == 0 || delta % step != 0 {
            return Ok(None);
        }
        let ordinal = delta / step + 1;
        if coordinate > i64::from(extent) || ordinal < 1 {
            return Ok(None);
        }
        Ok(Some(ordinal))
    }
}
