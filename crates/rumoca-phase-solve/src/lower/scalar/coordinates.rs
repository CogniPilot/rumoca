//! Coordinate, literal, and derivative reads for one scalar program.
//!
//! Every leaf a program loads resolves here: literals and ranges become
//! constants, coordinates become the storage slot the layout assigned them,
//! and a derivative coordinate a row other than its definition reads is
//! recomputed from the continuous row the structural proof matched to it.

use super::*;

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(super) fn pack_coordinate(
        &mut self,
        expression: dae::ExprId<'dae>,
        coordinate: dae::CoordinateView<'dae>,
        span: Span,
    ) -> Result<Option<solve::Reg>, LowerError> {
        if matches!(coordinate, dae::CoordinateView::Derivative(_)) {
            return Ok(None);
        }
        if let dae::CoordinateView::Parameter(parameter) = coordinate
            && let Some(binding) = self
                .parameter_substitutions
                .and_then(|substitutions| substitutions.binding(parameter.index()))
        {
            if self.active_parameters.contains(&parameter.index()) {
                return Err(LowerError::non_computable(
                    "parameter bindings are mutually recursive",
                    span,
                ));
            }
            self.enter_context(ScalarContextFrame::Parameter {
                parent: self.context_id,
                parameter: parameter.index(),
            });
            self.active_parameters.push(parameter.index());
            let value = self.pack_expression(binding);
            self.active_parameters.pop();
            self.leave_context();
            return value.map(Some);
        }
        let (variable, pre_variable) = if let Some(variable) = coordinate_variable(coordinate) {
            (variable, false)
        } else if let Some(variable) = pre_coordinate_variable(coordinate) {
            (variable, true)
        } else {
            return Ok(None);
        };
        let key = (self.context_id, expression);
        if let Some(&(start, _)) = self.tensor_load_cache.get(&key) {
            return Ok(Some(start));
        }
        let count = scalar_count(self.view, expression);
        if count <= 1 {
            return Ok(None);
        }
        let sampled_base = (!pre_variable && self.sampled_source).then(|| {
            self.layout
                .pre_variables
                .get(variable as usize)
                .copied()
                .flatten()
        });
        let sampled_base = sampled_base.flatten();
        let first = match (pre_variable, sampled_base) {
            (true, _) => pre_variable_scalar_slot(self.layout, variable, 0, span)?,
            (false, Some(index)) => solve::scalar_slot_p(index),
            (false, None) => variable_scalar_slot(self.layout, variable, 0, span)?,
        };
        let (input, input_start) = match first {
            solve::ScalarSlot::Y { index, .. } => (solve::TensorInputKind::Y, index),
            solve::ScalarSlot::P { index, .. } => (solve::TensorInputKind::P, index),
            solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => return Ok(None),
        };
        for scalar in 1..count {
            let expected = input_start.checked_add(scalar).ok_or_else(|| {
                LowerError::contract("tensor coordinate storage extent overflow", span)
            })?;
            let slot =
                self.coordinate_scalar_slot(variable, scalar, pre_variable, sampled_base, span)?;
            let contiguous = match slot {
                solve::ScalarSlot::Y { index, .. } => {
                    input == solve::TensorInputKind::Y && index == expected
                }
                solve::ScalarSlot::P { index, .. } => {
                    input == solve::TensorInputKind::P && index == expected
                }
                solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => false,
            };
            if !contiguous {
                return Ok(None);
            }
        }
        let dst_start = self.next_register;
        for _ in 0..count {
            self.register(span)?;
        }
        self.ops.push(solve::LinearOp::TensorLoad {
            dst_start,
            input,
            input_start,
            count,
            seed_start: None,
            lanes: 1,
        });
        self.tensor_load_cache.insert(key, (dst_start, count));
        Ok(Some(dst_start))
    }

    fn coordinate_scalar_slot(
        &self,
        variable: u32,
        scalar: usize,
        pre_variable: bool,
        sampled_base: Option<usize>,
        span: Span,
    ) -> Result<solve::ScalarSlot, LowerError> {
        match (pre_variable, sampled_base) {
            (true, _) => pre_variable_scalar_slot(self.layout, variable, scalar, span),
            (false, Some(base)) => base
                .checked_add(scalar)
                .map(solve::scalar_slot_p)
                .ok_or_else(|| {
                    LowerError::contract("sampled tensor coordinate storage overflow", span)
                }),
            (false, None) => variable_scalar_slot(self.layout, variable, scalar, span),
        }
    }

    pub(super) fn range(
        &mut self,
        start: i64,
        step: i64,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let offset = i64::try_from(scalar)
            .map_err(|_| LowerError::contract("range scalar ordinal overflow", span))?;
        let scaled = step
            .checked_mul(offset)
            .ok_or_else(|| LowerError::contract("range scalar multiplication overflow", span))?;
        let value = start
            .checked_add(scaled)
            .ok_or_else(|| LowerError::contract("range scalar addition overflow", span))?;
        self.constant(value as f64, span)
    }

    pub(super) fn literal(
        &mut self,
        value: &dae::DaeLiteral,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let value = match value {
            dae::DaeLiteral::Real(value) => *value,
            dae::DaeLiteral::Integer(value) => *value as f64,
            dae::DaeLiteral::Enumeration(value) => *value as f64,
            dae::DaeLiteral::Boolean(value) => f64::from(*value),
            dae::DaeLiteral::String(_) => {
                return Err(LowerError::unsupported(
                    "String values are not numeric Solve coordinates",
                    span,
                ));
            }
        };
        self.constant(value, span)
    }

    // SPEC_0021: Exception - exhaustive coordinate dispatch, including scoped binders.
    #[allow(clippy::excessive_nesting, clippy::too_many_lines)]
    pub(super) fn coordinate(
        &mut self,
        coordinate: dae::CoordinateView<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if let dae::CoordinateView::FunctionParameter(parameter) = coordinate {
            return self.function_parameter(parameter, scalar, span);
        }
        if let dae::CoordinateView::Condition(condition) = coordinate {
            return self.condition(condition);
        }
        if let dae::CoordinateView::Binder(binder) = coordinate {
            if let Some((_, values)) = self
                .symbolic_domain_points
                .iter()
                .rev()
                .find(|(domain, _)| *domain == binder.domain())
            {
                return values
                    .get(binder.ordinal() as usize)
                    .copied()
                    .ok_or_else(|| {
                        LowerError::contract("symbolic binder ordinal is out of range", span)
                    });
            }
            if let Some(source) = self
                .deferred_fold_captures
                .as_ref()
                .and_then(|deferred| {
                    deferred
                        .symbolic_domain_points
                        .iter()
                        .rev()
                        .find(|(domain, _)| *domain == binder.domain())
                })
                .and_then(|(_, values)| values.get(binder.ordinal() as usize))
                .copied()
            {
                return self.deferred_fold_capture(source, span);
            }
            let Some((_, values)) = self
                .domain_points
                .iter()
                .rev()
                .find(|(domain, _)| *domain == binder.domain())
            else {
                return Err(LowerError::non_computable(
                    "domain binder escaped its structured owner",
                    span,
                ));
            };
            let value = values
                .get(binder.ordinal() as usize)
                .copied()
                .ok_or_else(|| LowerError::contract("binder ordinal is out of range", span))?;
            return self.constant(value as f64, span);
        }
        if matches!(coordinate, dae::CoordinateView::Time) {
            let dst = self.register(span)?;
            self.ops.push(solve::LinearOp::LoadTime { dst });
            return Ok(dst);
        }
        if let dae::CoordinateView::ClockInterval(clock) = coordinate {
            return self.constant(self.view.periodic_clock(clock).period_seconds(), span);
        }
        if let dae::CoordinateView::Derivative(state) = coordinate {
            return self.derivative_value(state, scalar, span);
        }
        if let dae::CoordinateView::Parameter(parameter) = coordinate
            && let Some(binding) = self
                .parameter_substitutions
                .and_then(|substitutions| substitutions.binding(parameter.index()))
        {
            return self.substituted_parameter_value(parameter.index(), binding, scalar, span);
        }
        let slot = if self.sampled_source
            && let Some(variable) = coordinate_variable(coordinate)
            && let Some(base) = self
                .layout
                .pre_variables
                .get(variable as usize)
                .copied()
                .flatten()
        {
            let index = base.checked_add(scalar).ok_or_else(|| {
                LowerError::contract("sampled value scalar layout overflow", span)
            })?;
            solve::scalar_slot_p(index)
        } else if let dae::CoordinateView::Delay(delay_id) = coordinate {
            delay_value_scalar_slot(self.layout, delay_id.index(), scalar, span)?
        } else if let dae::CoordinateView::Previous(previous_id) = coordinate {
            let previous = self
                .view
                .previous(previous_id)
                .expect("checked previous identity resolves");
            if self.active_clock != Some(previous.clock()) {
                return Err(LowerError::non_computable(
                    "previous coordinate escaped its owning clock schedule",
                    span,
                ));
            }
            previous_value_scalar_slot(self.layout, previous_id.index(), scalar, span)?
        } else if let dae::CoordinateView::Terminal(_) = coordinate {
            solve::scalar_slot_p(
                self.layout
                    .solve_layout
                    .terminal_event_parameter_index
                    .expect("terminal coordinate owns a checked Solve runtime slot"),
            )
        } else if let Some(variable) = pre_coordinate_variable(coordinate) {
            pre_variable_scalar_slot(self.layout, variable, scalar, span)?
        } else {
            let variable = coordinate_variable(coordinate).ok_or_else(|| {
                LowerError::unsupported(
                    "runtime-managed condition, previous, or terminal coordinate",
                    span,
                )
            })?;
            variable_scalar_slot(self.layout, variable, scalar, span)?
        };
        let dst = self.register(span)?;
        match slot {
            solve::ScalarSlot::Y { index, .. } => {
                self.ops.push(solve::LinearOp::LoadY { dst, index });
            }
            solve::ScalarSlot::P { index, .. } => {
                self.ops.push(solve::LinearOp::LoadP { dst, index });
            }
            solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => {
                unreachable!("variable layouts contain only Y/P slots")
            }
        }
        Ok(dst)
    }

    /// Evaluate a calculated parameter from its binding rather than its storage.
    ///
    /// MLS 3.6 §8.6 makes a `parameter` declared `fixed = false` an unknown of
    /// the initialization phase whose `start` is only a guess, so a binding that
    /// reads one holds a seed after the parameter set runs, not a value. A row
    /// the initialization projection iterates must see that binding re-evaluated
    /// at the current iterate; loading the seed instead turns the projection
    /// into a fixed-point iteration around it, which diverges as soon as the
    /// loop gain reaches one.
    ///
    /// A binding cycle among parameters is rejected before Solve lowering. The
    /// active stack keeps that a checked guarantee rather than an assumption.
    fn substituted_parameter_value(
        &mut self,
        parameter: u32,
        binding: dae::ExprId<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if self.active_parameters.contains(&parameter) {
            return Err(LowerError::non_computable(
                "parameter bindings are mutually recursive",
                span,
            ));
        }
        self.enter_context(ScalarContextFrame::Parameter {
            parent: self.context_id,
            parameter,
        });
        self.active_parameters.push(parameter);
        let value = self.expression(binding, scalar);
        self.active_parameters.pop();
        self.leave_context();
        value
    }

    /// Evaluate a derivative coordinate a row other than its definition reads.
    ///
    /// A state derivative has no Solve storage of its own: it is the output of
    /// the continuous row the structural proof matched to it. Reading one from
    /// a different row therefore recomputes that row's defining right-hand
    /// side here. The definition is proven derivative-free before it is
    /// accepted, so this substitution is exact and cannot nest; the active
    /// stack keeps that guarantee checked rather than assumed.
    fn derivative_value(
        &mut self,
        state: dae::StateId<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if let Some(value) = self
            .derivative_seeds
            .as_ref()
            .and_then(|seeds| seeds.get(&(state.index(), scalar)))
            .copied()
        {
            return self.constant(value, span);
        }
        let escaped = || {
            LowerError::non_computable(
                "derivative coordinate escaped checked structural substitution",
                span,
            )
        };
        let definition = self
            .derivative_definitions
            .ok_or_else(escaped)?
            .definition(state, scalar)
            .ok_or_else(escaped)?;
        let key = (state.index(), scalar);
        if self.active_derivatives.contains(&key) {
            return Err(LowerError::non_computable(
                "state derivative definitions are mutually recursive",
                span,
            ));
        }
        let point = definition
            .domain_point
            .as_ref()
            .map(|(domain, values)| (*domain, values.as_slice()));
        let rhs = match derivative_rhs(
            self.view,
            definition.expression,
            definition.scalar,
            point,
            state,
            scalar,
        ) {
            Ok(rhs) => rhs,
            Err(error) => {
                if let Some(value) = self.affine_derivative_component(state, scalar, span)? {
                    return Ok(value);
                }
                let name = self
                    .view
                    .variable(state.into())
                    .expect("derivative state declaration resolves")
                    .name();
                return Err(LowerError::non_computable(
                    format!(
                        "cannot substitute derivative definition for {name}[{scalar}]: {error}"
                    ),
                    span,
                ));
            }
        };
        self.enter_context(ScalarContextFrame::Derivative {
            parent: self.context_id,
            state: state.index(),
            scalar,
        });
        self.active_derivatives.push(key);
        let pushed_point = definition.domain_point.clone();
        if let Some(point) = pushed_point {
            self.domain_points.push(point);
        }
        let value = self.derivative_definition_value(rhs, span);
        if definition.domain_point.is_some() {
            self.domain_points.pop();
        }
        self.active_derivatives.pop();
        self.leave_context();
        value
    }

    fn derivative_definition_value(
        &mut self,
        rhs: DerivativeRhs<'dae>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        match rhs {
            DerivativeRhs::Explicit { expression, scalar } => self.expression(expression, scalar),
            DerivativeRhs::Scaled {
                numerator,
                numerator_scalar,
                coefficient,
                coefficient_scalar,
                span: definition_span,
            } => {
                let numerator = self.expression(numerator, numerator_scalar)?;
                let coefficient = self.expression(coefficient, coefficient_scalar)?;
                self.binary(
                    dae::BinaryOperator::Divide,
                    numerator,
                    coefficient,
                    if definition_span.is_dummy() {
                        span
                    } else {
                        definition_span
                    },
                )
            }
        }
    }
}
