//! Coordinate, literal, and derivative reads for one scalar program.
//!
//! Every leaf a program loads resolves here: literals and ranges become
//! constants, coordinates become the storage slot the layout assigned them,
//! and a derivative coordinate a row other than its definition reads is
//! recomputed from the continuous row the structural proof matched to it.

use super::*;

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
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

    pub(super) fn coordinate(
        &mut self,
        coordinate: dae::CoordinateView<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if let dae::CoordinateView::FunctionParameter(parameter) = coordinate {
            return self.function_parameter(parameter, scalar, span);
        }
        if let dae::CoordinateView::Binder(binder) = coordinate {
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
        let slot = if let dae::CoordinateView::Delay(delay_id) = coordinate {
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
        let rhs = derivative_rhs(
            self.view,
            definition.expression,
            definition.scalar,
            point,
            state,
            scalar,
        )?;
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
