use rumoca_core::{
    Expression, Literal, OpBinary, OpUnary, Span, Subscript, component_path_trailing_index,
    is_pre_slot, pre_slot_base,
};
use rumoca_ir_galec::ast::{self as gast, BinaryOp, IfExpression};

use crate::classify::VariableClass;
use crate::diagnostic::GalecTargetError;
use crate::lower::conditions::{condition_ref_index, pre_condition_ref_index};

use super::{
    ExprLowerer, Typed, conjunction, indexed_expression, is_slice_subscript, mismatch, optional,
    slice_len_to_i64, unsupported, vector_dot_shape,
};

impl ExprLowerer<'_> {
    pub(super) fn lower_var_ref(
        &mut self,
        name: &str,
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        if name == "time" && subscripts.is_empty() {
            return Err(unsupported(
                "time-in-discrete-block".to_owned(),
                "reference to the continuous independent variable `time`".to_owned(),
                Some(span),
            ));
        }
        if let Some(condition_base) = &self.conditions.base_name {
            let as_expr = Expression::VarRef {
                name: rumoca_core::Reference::new(name),
                subscripts: subscripts.to_vec(),
                span,
            };
            if let Some(index) = condition_ref_index(&as_expr, condition_base) {
                return self.inline_condition(index, span);
            }
            if pre_condition_ref_index(&as_expr, condition_base).is_some()
                || name == condition_base
                || pre_slot_base(name) == Some(condition_base.as_str())
            {
                return Err(unsupported(
                    "condition-memory-outside-guard".to_owned(),
                    format!(
                        "generated when-edge condition machinery `{name}` referenced \
                         outside a recognizable when-edge guard"
                    ),
                    Some(span),
                ));
            }
        }
        if is_pre_slot(name) {
            return self.lower_pre_ref(name, subscripts, span);
        }
        self.lower_state_ref(name, subscripts, span)
    }

    fn lower_pre_ref(
        &mut self,
        slot_name: &str,
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let (classified, prefix_indices) = match self.classification.find(slot_name) {
            Some(classified) => (classified, Vec::new()),
            None => {
                let Some((base, index)) = component_path_trailing_index(slot_name) else {
                    return Err(GalecTargetError::UnknownVariableReference {
                        name: slot_name.to_owned(),
                        span: optional(span),
                    });
                };
                let Some(classified) = self.classification.find(&base) else {
                    return Err(GalecTargetError::UnknownVariableReference {
                        name: slot_name.to_owned(),
                        span: optional(span),
                    });
                };
                let index =
                    i64::try_from(index).map_err(|_| GalecTargetError::LoweringInternal {
                        detail: format!("index of `{slot_name}` exceeds i64"),
                    })?;
                (classified, vec![index])
            }
        };
        if classified.pre_base.is_none() {
            return Err(GalecTargetError::LoweringInternal {
                detail: format!(
                    "variable `{slot_name}` carries the pre-slot prefix but did not \
                     classify as a pre slot"
                ),
            });
        }
        self.referenced_pre
            .record(classified.variable.name.as_str());
        self.lower_reference_access(classified, prefix_indices, subscripts, span)
    }

    fn lower_state_ref(
        &mut self,
        name: &str,
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let (classified, prefix_indices) = match self.classification.find(name) {
            Some(classified) => (classified, Vec::new()),
            None => {
                let Some((base, index)) = component_path_trailing_index(name) else {
                    return Err(GalecTargetError::UnknownVariableReference {
                        name: name.to_owned(),
                        span: optional(span),
                    });
                };
                let Some(classified) = self.classification.find(&base) else {
                    return Err(GalecTargetError::UnknownVariableReference {
                        name: name.to_owned(),
                        span: optional(span),
                    });
                };
                let index =
                    i64::try_from(index).map_err(|_| GalecTargetError::LoweringInternal {
                        detail: format!("index of `{name}` exceeds i64"),
                    })?;
                (classified, vec![index])
            }
        };
        if classified.projection_internal {
            return Err(unsupported(
                "condition-memory-outside-guard".to_owned(),
                format!("generated condition machinery `{name}` referenced as a value"),
                Some(span),
            ));
        }
        self.lower_reference_access(classified, prefix_indices, subscripts, span)
    }

    fn lower_reference_access(
        &mut self,
        classified: &crate::classify::ClassifiedVariable<'_>,
        prefix_indices: Vec<i64>,
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let dims = &classified.variable.dims;
        let projected_subscripts;
        let base_start = subscripts.len().saturating_sub(dims.len());
        let subscripts =
            if base_start > 0 && subscripts[base_start..].iter().any(is_slice_subscript) {
                projected_subscripts = self.project_indexed_slice_subscripts(
                    classified.variable.name.as_str(),
                    &subscripts[base_start..],
                    &subscripts[..base_start],
                    span,
                )?;
                projected_subscripts.as_slice()
            } else {
                subscripts
            };
        // Flattening can preserve a scalarized trailing index in the rendered
        // reference name while also carrying a full-rank subscript vector for
        // a row/range slice. In that form the explicit subscripts are the
        // authoritative original-rank access; counting the rendered suffix a
        // second time invents an extra dimension.
        let prefix_indices = if subscripts.len() == dims.len() {
            Vec::new()
        } else {
            prefix_indices
        };
        let subscript_count = prefix_indices.len() + subscripts.len();
        if subscript_count == 0 {
            return Ok(Typed::array(
                super::state_ref(classified.galec_name.clone(), Vec::new()),
                classified.scalar_type,
                dims.clone(),
            ));
        }
        if subscript_count != dims.len() {
            return Err(unsupported(
                "array-partial-subscript".to_owned(),
                format!(
                    "reference to `{}` uses {subscript_count} subscript(s) for {} dimension(s)",
                    classified.variable.name.as_str(),
                    dims.len()
                ),
                Some(span),
            ));
        }
        if subscripts.iter().any(is_slice_subscript) {
            return self.lower_slice_reference(classified, prefix_indices, subscripts, span);
        }
        if subscripts
            .iter()
            .any(|subscript| self.static_index_value(subscript).is_none())
        {
            if !prefix_indices.is_empty() {
                return Err(unsupported(
                    "dynamic-subscript-after-rendered-index".to_owned(),
                    format!(
                        "dynamic subscript on scalarized element name `{}`",
                        classified.variable.name.as_str()
                    ),
                    Some(span),
                ));
            }
            return self.lower_dynamic_reference_selection(classified, subscripts, span);
        }
        let mut galec_subscripts = prefix_indices
            .into_iter()
            .map(gast::Expression::Integer)
            .collect::<Vec<_>>();
        galec_subscripts.extend(self.lower_subscripts(subscripts)?);
        Ok(Typed::new(
            super::state_ref(classified.galec_name.clone(), galec_subscripts),
            classified.scalar_type,
        ))
    }

    fn lower_slice_reference(
        &mut self,
        classified: &crate::classify::ClassifiedVariable<'_>,
        prefix_indices: Vec<i64>,
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let mut full_subscripts = prefix_indices
            .into_iter()
            .map(|value| Subscript::index(value, span))
            .collect::<Vec<_>>();
        full_subscripts.extend_from_slice(subscripts);
        let mut shape = Vec::new();
        for (subscript, dimension) in full_subscripts.iter().zip(&classified.variable.dims) {
            if let Some(indices) = self.slice_subscript_indices(
                *dimension,
                subscript,
                classified.variable.name.as_str(),
            )? {
                shape.push(slice_len_to_i64(indices.len())?);
            }
        }
        let expr = self.slice_array_expression(classified, &mut full_subscripts, 0, span)?;
        Ok(Typed::array(expr, classified.scalar_type, shape))
    }

    fn slice_array_expression(
        &mut self,
        classified: &crate::classify::ClassifiedVariable<'_>,
        subscripts: &mut [Subscript],
        position: usize,
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let Some(slice_position) = subscripts
            .iter()
            .enumerate()
            .skip(position)
            .find_map(|(index, subscript)| is_slice_subscript(subscript).then_some(index))
        else {
            let typed = self.lower_reference_access(classified, Vec::new(), subscripts, span)?;
            if !typed.is_scalar() {
                return Err(GalecTargetError::LoweringInternal {
                    detail: "array slice expansion produced a non-scalar element".to_owned(),
                });
            }
            return Ok(typed.expr);
        };
        let dimension = classified.variable.dims[slice_position];
        let indices = self
            .slice_subscript_indices(
                dimension,
                &subscripts[slice_position],
                classified.variable.name.as_str(),
            )?
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: "slice expansion selected a non-slice subscript".to_owned(),
            })?;
        let original = subscripts[slice_position].clone();
        let mut elements = Vec::new();
        for index in indices {
            subscripts[slice_position] = Subscript::index(index, span);
            elements.push(self.slice_array_expression(
                classified,
                subscripts,
                slice_position + 1,
                span,
            )?);
        }
        subscripts[slice_position] = original;
        Ok(gast::Expression::Array(elements))
    }

    fn lower_dynamic_reference_selection(
        &mut self,
        classified: &crate::classify::ClassifiedVariable<'_>,
        subscripts: &[Subscript],
        _span: Span,
    ) -> Result<Typed, GalecTargetError> {
        let dims = &classified.variable.dims;
        let combinations =
            self.static_index_combinations(dims, subscripts, classified.variable.name.as_str())?;
        let Some((else_indices, branch_indices)) = combinations.split_last() else {
            return Err(GalecTargetError::LoweringInternal {
                detail: "dynamic subscript expansion saw an empty index domain".to_owned(),
            });
        };
        let mut branches = Vec::with_capacity(branch_indices.len());
        for indices in branch_indices {
            let condition = self.dynamic_index_condition(subscripts, indices)?;
            branches.push((condition, self.static_reference_value(classified, indices)));
        }
        Ok(Typed::new(
            gast::Expression::If(IfExpression {
                branches,
                else_value: Box::new(self.static_reference_value(classified, else_indices)),
            }),
            classified.scalar_type,
        ))
    }

    fn static_reference_value(
        &self,
        classified: &crate::classify::ClassifiedVariable<'_>,
        indices: &[i64],
    ) -> gast::Expression {
        let subscripts = indices
            .iter()
            .copied()
            .map(gast::Expression::Integer)
            .collect();
        super::state_ref(classified.galec_name.clone(), subscripts)
    }

    fn dynamic_index_condition(
        &mut self,
        subscripts: &[Subscript],
        indices: &[i64],
    ) -> Result<gast::Expression, GalecTargetError> {
        let mut conditions = Vec::new();
        for (subscript, index) in subscripts.iter().zip(indices) {
            if self.static_index_value(subscript).is_some() {
                continue;
            }
            let Subscript::Expr { expr, span } = subscript else {
                return Err(unsupported(
                    "array-slice".to_owned(),
                    "`:` array slice subscript".to_owned(),
                    Some(subscript.span()),
                ));
            };
            let lhs = self.lower_as_integer(expr, *span)?;
            conditions.push(gast::Expression::binary(
                BinaryOp::Eq,
                lhs,
                gast::Expression::Integer(*index),
            ));
        }
        conjunction(conditions).ok_or_else(|| GalecTargetError::LoweringInternal {
            detail: "dynamic subscript expansion produced no dynamic conditions".to_owned(),
        })
    }

    fn lower_subscripts(
        &mut self,
        subscripts: &[Subscript],
    ) -> Result<Vec<gast::Expression>, GalecTargetError> {
        subscripts
            .iter()
            .map(|subscript| match subscript {
                Subscript::Index { value, .. } => Ok(gast::Expression::Integer(*value)),
                Subscript::Expr { expr, span } => {
                    if let Some(value) = self.static_index_value(subscript) {
                        return Ok(gast::Expression::Integer(value));
                    }
                    let typed = self.lower(expr)?;
                    if typed.ty != gast::ScalarType::Integer {
                        return Err(mismatch(
                            "array subscript",
                            "Integer",
                            typed.ty,
                            Some(*span),
                        ));
                    }
                    Ok(typed.expr)
                }
                Subscript::Colon { span } => Err(unsupported(
                    "array-slice".to_owned(),
                    "`:` array slice subscript".to_owned(),
                    Some(*span),
                )),
            })
            .collect()
    }

    pub(super) fn static_index_value(&self, subscript: &Subscript) -> Option<i64> {
        match subscript {
            Subscript::Index { value, .. } => Some(*value),
            Subscript::Expr { expr, .. } => self.static_integer_expression(expr),
            Subscript::Colon { .. } => None,
        }
    }

    fn slice_subscript_indices(
        &self,
        dimension: i64,
        subscript: &Subscript,
        variable: &str,
    ) -> Result<Option<Vec<i64>>, GalecTargetError> {
        match subscript {
            Subscript::Colon { .. } => Ok(Some(self.full_slice_indices(dimension, variable)?)),
            Subscript::Expr { expr, span } if matches!(expr.as_ref(), Expression::Range { .. }) => {
                Ok(Some(self.static_range_subscript_indices(
                    expr, dimension, variable, *span,
                )?))
            }
            _ => Ok(None),
        }
    }

    fn full_slice_indices(
        &self,
        dimension: i64,
        variable: &str,
    ) -> Result<Vec<i64>, GalecTargetError> {
        if dimension < 1 {
            return Err(GalecTargetError::LoweringInternal {
                detail: format!(
                    "non-positive slice dimension {dimension} on `{variable}` survived admissibility"
                ),
            });
        }
        Ok((1..=dimension).collect())
    }

    fn static_range_subscript_indices(
        &self,
        expr: &Expression,
        dimension: i64,
        variable: &str,
        span: Span,
    ) -> Result<Vec<i64>, GalecTargetError> {
        let indices = self.static_range_values(expr, span)?;
        if indices.is_empty() {
            return Err(unsupported(
                "array-slice-range".to_owned(),
                format!("range slice into `{variable}` selects no elements"),
                Some(span),
            ));
        }
        if indices.iter().any(|index| *index < 1 || *index > dimension) {
            return Err(unsupported(
                "array-slice-range".to_owned(),
                format!("range slice into `{variable}` must select indices inside 1..{dimension}"),
                Some(span),
            ));
        }
        Ok(indices)
    }

    fn static_range_values(
        &self,
        expr: &Expression,
        span: Span,
    ) -> Result<Vec<i64>, GalecTargetError> {
        let Expression::Range {
            start, step, end, ..
        } = expr
        else {
            return Err(GalecTargetError::LoweringInternal {
                detail: "static range expansion called on a non-range expression".to_owned(),
            });
        };
        let start = self.static_integer_expression(start).ok_or_else(|| {
            unsupported(
                "array-slice-range".to_owned(),
                "range slice start must be a statically known Integer".to_owned(),
                Some(span),
            )
        })?;
        let step = step
            .as_deref()
            .map(|expr| {
                self.static_integer_expression(expr).ok_or_else(|| {
                    unsupported(
                        "array-slice-range".to_owned(),
                        "range slice step must be a statically known Integer".to_owned(),
                        Some(span),
                    )
                })
            })
            .transpose()?
            .unwrap_or(1);
        let end = self.static_integer_expression(end).ok_or_else(|| {
            unsupported(
                "array-slice-range".to_owned(),
                "range slice end must be a statically known Integer".to_owned(),
                Some(span),
            )
        })?;
        self.inclusive_range_values(start, step, end, span)
    }

    fn inclusive_range_values(
        &self,
        start: i64,
        step: i64,
        end: i64,
        span: Span,
    ) -> Result<Vec<i64>, GalecTargetError> {
        if step == 0 {
            return Err(unsupported(
                "array-slice-range".to_owned(),
                "range slice step must not be zero".to_owned(),
                Some(span),
            ));
        }
        let mut values = Vec::new();
        let mut value = start;
        while (step > 0 && value <= end) || (step < 0 && value >= end) {
            values.push(value);
            value = value.checked_add(step).ok_or_else(|| {
                unsupported(
                    "array-slice-range".to_owned(),
                    "range slice index overflowed i64".to_owned(),
                    Some(span),
                )
            })?;
        }
        Ok(values)
    }

    fn static_integer_expression(&self, expr: &Expression) -> Option<i64> {
        match expr {
            Expression::Literal {
                value: Literal::Integer(value),
                ..
            } => Some(*value),
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => self.constant_integer_value(name.as_str()),
            _ => None,
        }
    }

    fn constant_integer_value(&self, name: &str) -> Option<i64> {
        let classified = self.classification.find(name)?;
        if classified.class != VariableClass::Constant || !classified.variable.dims.is_empty() {
            return None;
        }
        match classified.variable.start.as_ref()? {
            Expression::Literal {
                value: Literal::Integer(value),
                ..
            } => Some(*value),
            _ => None,
        }
    }

    fn static_index_combinations(
        &self,
        dims: &[i64],
        subscripts: &[Subscript],
        variable: &str,
    ) -> Result<Vec<Vec<i64>>, GalecTargetError> {
        let mut choices = Vec::with_capacity(dims.len());
        for (dimension, subscript) in dims.iter().zip(subscripts) {
            choices.push(self.subscript_index_choices(*dimension, subscript, variable)?);
        }
        let mut result = vec![Vec::new()];
        for values in choices {
            result = super::extend_index_combinations(result, &values);
        }
        Ok(result)
    }

    fn subscript_index_choices(
        &self,
        dimension: i64,
        subscript: &Subscript,
        variable: &str,
    ) -> Result<Vec<i64>, GalecTargetError> {
        if let Some(index) = self.static_index_value(subscript) {
            return Ok(vec![index]);
        }
        if dimension < 1 {
            return Err(GalecTargetError::LoweringInternal {
                detail: format!(
                    "non-positive dimension {dimension} on `{variable}` survived admissibility"
                ),
            });
        }
        self.require_dynamic_subscript_range(subscript, dimension, variable)?;
        Ok((1..=dimension).collect())
    }

    fn require_dynamic_subscript_range(
        &self,
        subscript: &Subscript,
        dimension: i64,
        variable: &str,
    ) -> Result<(), GalecTargetError> {
        let Subscript::Expr { expr, span } = subscript else {
            return Ok(());
        };
        let Some((min, max)) = self.integer_expression_range(expr) else {
            return Err(unsupported(
                "dynamic-array-subscript-range".to_owned(),
                format!(
                    "dynamic subscript into `{variable}` must have statically known Integer \
                     bounds inside 1..{dimension}"
                ),
                Some(*span),
            ));
        };
        if min < 1 || max > dimension {
            return Err(unsupported(
                "dynamic-array-subscript-range".to_owned(),
                format!(
                    "dynamic subscript into `{variable}` has proven bounds {min}..{max}, \
                     outside 1..{dimension}"
                ),
                Some(*span),
            ));
        }
        Ok(())
    }

    fn integer_expression_range(&self, expr: &Expression) -> Option<(i64, i64)> {
        match expr {
            Expression::Literal {
                value: Literal::Integer(value),
                ..
            } => Some((*value, *value)),
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => self.variable_integer_range(name.as_str()),
            Expression::Unary {
                op: OpUnary::Minus,
                rhs,
                ..
            } => {
                let (min, max) = self.integer_expression_range(rhs)?;
                Some((max.checked_neg()?, min.checked_neg()?))
            }
            Expression::Binary {
                op: OpBinary::Add,
                lhs,
                rhs,
                ..
            } => {
                let (left_min, left_max) = self.integer_expression_range(lhs)?;
                let (right_min, right_max) = self.integer_expression_range(rhs)?;
                Some((
                    left_min.checked_add(right_min)?,
                    left_max.checked_add(right_max)?,
                ))
            }
            Expression::Binary {
                op: OpBinary::Sub,
                lhs,
                rhs,
                ..
            } => {
                let (left_min, left_max) = self.integer_expression_range(lhs)?;
                let (right_min, right_max) = self.integer_expression_range(rhs)?;
                Some((
                    left_min.checked_sub(right_max)?,
                    left_max.checked_sub(right_min)?,
                ))
            }
            _ => None,
        }
    }

    fn variable_integer_range(&self, name: &str) -> Option<(i64, i64)> {
        self.classified_integer_range(name)
            .or_else(|| pre_slot_base(name).and_then(|base| self.classified_integer_range(base)))
    }

    fn classified_integer_range(&self, name: &str) -> Option<(i64, i64)> {
        let classified = self.classification.find(name)?;
        let min = super::integer_bound(classified.variable.min.as_ref()?)?;
        let max = super::integer_bound(classified.variable.max.as_ref()?)?;
        (min <= max).then_some((min, max))
    }

    fn inline_condition(&mut self, index: usize, span: Span) -> Result<Typed, GalecTargetError> {
        let Some(entry) = self.conditions.entry(index) else {
            return Err(GalecTargetError::LoweringInternal {
                detail: format!("reference to undefined condition slot c[{index}]"),
            });
        };
        if entry.is_sample {
            return Err(unsupported(
                "sample-condition-as-value".to_owned(),
                "the clock sample-tick condition used as a value expression".to_owned(),
                Some(span),
            ));
        }
        if self.inlining.contains(&index) {
            return Err(GalecTargetError::LoweringInternal {
                detail: format!("condition slot c[{index}] is defined in terms of itself"),
            });
        }
        self.inlining.push(index);
        let result = self.lower(entry.rhs);
        self.inlining.pop();
        let typed = result?;
        if typed.ty != gast::ScalarType::Boolean {
            return Err(mismatch(
                "inlined condition expression",
                "Boolean",
                typed.ty,
                optional(span),
            ));
        }
        Ok(typed)
    }

    pub(super) fn lower_index(
        &mut self,
        base: &Expression,
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        if subscripts.is_empty() {
            return self.lower(base);
        }
        match base {
            Expression::VarRef {
                name,
                subscripts: base_subscripts,
                span,
            } => self.lower_indexed_var_ref(name.as_str(), base_subscripts, subscripts, *span),
            Expression::Array { elements, .. } => {
                let selected = self.select_array_element(elements, subscripts, span)?;
                self.lower(selected)
            }
            Expression::Index {
                base: inner_base,
                subscripts: inner_subscripts,
                span: inner_span,
            } => self.lower_nested_index(inner_base, inner_subscripts, subscripts, *inner_span),
            Expression::If {
                branches,
                else_branch,
                span,
            } => {
                let indexed_branches = branches
                    .iter()
                    .map(|(condition, value)| {
                        (
                            condition.clone(),
                            indexed_expression(value.clone(), subscripts.to_vec(), *span),
                        )
                    })
                    .collect();
                let indexed_else =
                    indexed_expression(else_branch.as_ref().clone(), subscripts.to_vec(), *span);
                self.lower(&Expression::If {
                    branches: indexed_branches,
                    else_branch: Box::new(indexed_else),
                    span: *span,
                })
            }
            Expression::Binary { op, lhs, rhs, span } => {
                self.lower_indexed_binary(op, lhs, rhs, *span, subscripts)
            }
            Expression::Unary { op, rhs, span } => {
                let value = self.lower(rhs)?;
                if value.is_scalar() {
                    return Err(unsupported(
                        "scalar-indexed-expression".to_owned(),
                        "indexed expression over a scalar unary result".to_owned(),
                        Some(*span),
                    ));
                }
                self.lower(&Expression::Unary {
                    op: op.clone(),
                    rhs: Box::new(indexed_expression(
                        rhs.as_ref().clone(),
                        subscripts.to_vec(),
                        *span,
                    )),
                    span: *span,
                })
            }
            Expression::FunctionCall {
                name,
                args,
                is_constructor,
                span: call_span,
            } => self.lower_indexed_function_call(
                name,
                args,
                *is_constructor,
                *call_span,
                subscripts,
                span,
            ),
            _ => Err(unsupported(
                "indexed-expression-base".to_owned(),
                format!("indexed expression over {}", super::form_name(base)),
                Some(span),
            )),
        }
    }

    fn lower_nested_index(
        &mut self,
        inner_base: &Expression,
        inner_subscripts: &[Subscript],
        outer_subscripts: &[Subscript],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        if !inner_subscripts.iter().any(is_slice_subscript) {
            return Err(unsupported(
                "indexed-expression-base".to_owned(),
                "indexed expression over indexed expression".to_owned(),
                Some(span),
            ));
        }
        let subscripts = self.project_indexed_slice_subscripts(
            "indexed expression",
            inner_subscripts,
            outer_subscripts,
            span,
        )?;
        self.lower(&Expression::Index {
            base: Box::new(inner_base.clone()),
            subscripts,
            span,
        })
    }

    fn lower_indexed_var_ref(
        &mut self,
        name: &str,
        base_subscripts: &[Subscript],
        index_subscripts: &[Subscript],
        span: Span,
    ) -> Result<Typed, GalecTargetError> {
        if !base_subscripts.iter().any(is_slice_subscript) {
            let mut combined = base_subscripts.to_vec();
            combined.extend_from_slice(index_subscripts);
            return self.lower_var_ref(name, &combined, span);
        }
        let projected =
            self.project_indexed_slice_subscripts(name, base_subscripts, index_subscripts, span)?;
        self.lower_var_ref(name, &projected, span)
    }

    fn project_indexed_slice_subscripts(
        &self,
        name: &str,
        base_subscripts: &[Subscript],
        index_subscripts: &[Subscript],
        span: Span,
    ) -> Result<Vec<Subscript>, GalecTargetError> {
        let mut selected = index_subscripts.iter();
        let mut projected = Vec::with_capacity(base_subscripts.len());
        for base_subscript in base_subscripts {
            if is_slice_subscript(base_subscript) {
                projected.push(self.project_optional_slice_index(base_subscript, selected.next())?);
                continue;
            }
            projected.push(base_subscript.clone());
        }
        if selected.next().is_some() {
            return Err(unsupported(
                "array-slice-rank".to_owned(),
                format!("too many subscripts for indexed slice `{name}`"),
                Some(span),
            ));
        }
        Ok(projected)
    }

    fn project_optional_slice_index(
        &self,
        slice_subscript: &Subscript,
        index_subscript: Option<&Subscript>,
    ) -> Result<Subscript, GalecTargetError> {
        match index_subscript {
            Some(index_subscript) => self.project_slice_index(slice_subscript, index_subscript),
            None => Ok(slice_subscript.clone()),
        }
    }

    fn project_slice_index(
        &self,
        slice_subscript: &Subscript,
        index_subscript: &Subscript,
    ) -> Result<Subscript, GalecTargetError> {
        match slice_subscript {
            Subscript::Colon { .. } => Ok(index_subscript.clone()),
            Subscript::Expr { expr, span } if matches!(expr.as_ref(), Expression::Range { .. }) => {
                let local_index = self.static_index_value(index_subscript).ok_or_else(|| {
                    unsupported(
                        "dynamic-indexed-range-slice".to_owned(),
                        "dynamic subscript into a range slice".to_owned(),
                        Some(index_subscript.span()),
                    )
                })?;
                let values = self.static_range_values(expr, *span)?;
                let local_index = usize::try_from(local_index)
                    .ok()
                    .filter(|index| *index >= 1 && *index <= values.len())
                    .ok_or_else(|| GalecTargetError::LoweringInternal {
                        detail: format!(
                            "range slice local index {local_index} is outside 1..{}",
                            values.len()
                        ),
                    })?;
                Ok(Subscript::index(
                    values[local_index - 1],
                    index_subscript.span(),
                ))
            }
            _ => Err(GalecTargetError::LoweringInternal {
                detail: "slice index projection called on a non-slice subscript".to_owned(),
            }),
        }
    }

    fn lower_indexed_binary(
        &mut self,
        op: &OpBinary,
        lhs: &Expression,
        rhs: &Expression,
        span: Span,
        subscripts: &[Subscript],
    ) -> Result<Typed, GalecTargetError> {
        let left = self.lower(lhs)?;
        let right = self.lower(rhs)?;
        if matches!(op, OpBinary::Mul) && vector_dot_shape(&left, &right).is_some() {
            return Err(unsupported(
                "scalar-indexed-expression".to_owned(),
                "indexed expression over a vector dot-product result".to_owned(),
                Some(span),
            ));
        }
        if left.is_scalar() && right.is_scalar() {
            return Err(unsupported(
                "scalar-indexed-expression".to_owned(),
                "indexed expression over a scalar binary result".to_owned(),
                Some(span),
            ));
        }
        let lhs = if left.is_scalar() {
            lhs.clone()
        } else {
            indexed_expression(lhs.clone(), subscripts.to_vec(), span)
        };
        let rhs = if right.is_scalar() {
            rhs.clone()
        } else {
            indexed_expression(rhs.clone(), subscripts.to_vec(), span)
        };
        self.lower(&Expression::Binary {
            op: op.clone(),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span,
        })
    }

    fn lower_indexed_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[Expression],
        is_constructor: bool,
        call_span: Span,
        subscripts: &[Subscript],
        index_span: Span,
    ) -> Result<Typed, GalecTargetError> {
        self.lower_inlined_function_call(
            name,
            args,
            is_constructor,
            call_span,
            |lowerer, expression| {
                lowerer.lower(&indexed_expression(
                    expression,
                    subscripts.to_vec(),
                    index_span,
                ))
            },
        )
    }

    pub(super) fn select_array_element<'b>(
        &self,
        elements: &'b [Expression],
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<&'b Expression, GalecTargetError> {
        let Some((first, rest)) = subscripts.split_first() else {
            return Err(GalecTargetError::LoweringInternal {
                detail: "array element selection called without subscripts".to_owned(),
            });
        };
        let Some(index) = self.static_index_value(first) else {
            return Err(unsupported(
                "dynamic-indexed-array-constructor".to_owned(),
                "dynamic subscript into an array constructor".to_owned(),
                Some(first.span()),
            ));
        };
        let index = usize::try_from(index)
            .ok()
            .filter(|index| *index >= 1 && *index <= elements.len())
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: format!(
                    "array constructor index {index} is outside 1..{}",
                    elements.len()
                ),
            })?;
        let selected = &elements[index - 1];
        if rest.is_empty() {
            return Ok(selected);
        }
        match selected {
            Expression::Array { elements, .. } => self.select_array_element(elements, rest, span),
            _ => Err(unsupported(
                "array-constructor-rank".to_owned(),
                "too many subscripts for array constructor rank".to_owned(),
                Some(span),
            )),
        }
    }
}
