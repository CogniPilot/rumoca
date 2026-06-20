use rumoca_ir_dae as dae;
use rumoca_ir_solve::{BinaryOp, CompareOp, Reg, UnaryOp};

use super::helpers::{binding_base_key, intrinsic_short_name};
use super::{LowerBuilder, LowerError, Scope, ValueMode};

impl<'a> LowerBuilder<'a> {
    pub(super) fn lower_interval_intrinsic(
        &mut self,
        args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let Some(clock_expr) = args.first() else {
            return self.emit_const_at(1.0, required_context_span(call_span, "interval() call")?);
        };
        if let Some(reg) = self.lower_clock_interval_expr(clock_expr, scope, call_depth)? {
            return Ok(reg);
        }
        self.emit_const_at(1.0, required_context_span(call_span, "interval() call")?)
    }

    pub(super) fn lower_clock_constructor_tick(
        &mut self,
        short_name: &str,
        args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        if self.clock_constructor_is_triggered_event(short_name, args) {
            let Some(condition) = args.first() else {
                return self
                    .emit_const_at(
                        0.0,
                        required_context_span(call_span, "triggered Clock() call")?,
                    )
                    .map(Some);
            };
            if self.value_mode == ValueMode::Pre {
                return self
                    .emit_const_at(
                        0.0,
                        source_span_or_context(condition, call_span, "clock condition")?,
                    )
                    .map(Some);
            }
            return self.lower_expr(condition, scope, call_depth).map(Some);
        }
        let Some(period) =
            self.lower_clock_constructor_period(short_name, args, call_span, scope, call_depth)?
        else {
            return Ok(None);
        };
        if self.value_mode == ValueMode::Pre {
            return self
                .emit_const_at(
                    0.0,
                    required_context_span(call_span, "clock constructor call")?,
                )
                .map(Some);
        }
        let phase =
            self.lower_clock_constructor_phase(short_name, args, call_span, scope, call_depth)?;
        self.emit_periodic_tick(
            phase,
            period,
            required_context_span(call_span, "clock constructor call")?,
        )
        .map(Some)
    }

    fn clock_constructor_is_triggered_event(
        &self,
        short_name: &str,
        args: &[rumoca_core::Expression],
    ) -> bool {
        short_name == "Clock"
            && args.len() == 1
            && self
                .triggered_clock_conditions
                .is_some_and(|conditions| conditions.iter().any(|condition| condition == &args[0]))
    }

    pub(super) fn lower_clock_tick_expr(
        &mut self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let Some(period) = self.lower_clock_interval_expr(expr, scope, call_depth)? else {
            return Ok(None);
        };
        let span = required_source_span(expr, "clock tick expression")?;
        if self.value_mode == ValueMode::Pre {
            return self.emit_const_at(0.0, span).map(Some);
        }
        let phase = self.lower_clock_phase_expr(expr, scope, call_depth)?;
        self.emit_periodic_tick(phase, period, span).map(Some)
    }

    pub(super) fn lower_clock_constructor_period(
        &mut self,
        short_name: &str,
        args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        match short_name {
            "Clock" => self.lower_clock_interval_clock_call(args, call_span, scope, call_depth),
            "subSample" => {
                self.lower_scaled_clock_interval(args, call_span, scope, call_depth, BinaryOp::Mul)
            }
            "superSample" => {
                self.lower_scaled_clock_interval(args, call_span, scope, call_depth, BinaryOp::Div)
            }
            "shiftSample" | "backSample" => {
                self.lower_passthrough_clock_interval(args, call_span, scope, call_depth)
            }
            _ => Ok(None),
        }
    }

    pub(super) fn lower_clock_constructor_phase(
        &mut self,
        short_name: &str,
        args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        match short_name {
            "Clock" => self.lower_clock_call_phase(args, call_span),
            "subSample" | "superSample" => {
                if args.len() == 1
                    && let Some(phase) = self
                        .current_update_target_clock_timing()
                        .map(|timing| timing.phase_seconds)
                {
                    return self.emit_const_at(
                        phase,
                        required_context_span(call_span, "subSample/superSample call")?,
                    );
                }
                let Some(base_expr) = args.first() else {
                    return self.emit_const_at(
                        0.0,
                        required_context_span(call_span, "subSample/superSample call")?,
                    );
                };
                self.lower_clock_phase_expr(base_expr, scope, call_depth)
            }
            "shiftSample" | "backSample" => {
                let Some(base_expr) = args.first() else {
                    return self.emit_const_at(
                        0.0,
                        required_context_span(call_span, "shiftSample/backSample call")?,
                    );
                };
                let Some(base_period) =
                    self.lower_clock_interval_expr(base_expr, scope, call_depth)?
                else {
                    return self.lower_clock_phase_expr(base_expr, scope, call_depth);
                };
                let base_phase = self.lower_clock_phase_expr(base_expr, scope, call_depth)?;
                let shift =
                    self.lower_optional_clock_factor(args.get(1), call_span, scope, call_depth)?;
                let resolution =
                    self.lower_optional_clock_factor(args.get(2), call_span, scope, call_depth)?;
                let span = source_span_or_context(base_expr, call_span, "clock base expression")?;
                let fraction = self.emit_binary_at(BinaryOp::Div, shift, resolution, span)?;
                let offset = self.emit_binary_at(BinaryOp::Mul, base_period, fraction, span)?;
                if short_name == "backSample" {
                    self.emit_binary_at(BinaryOp::Sub, base_phase, offset, span)
                } else {
                    self.emit_binary_at(BinaryOp::Add, base_phase, offset, span)
                }
            }
            _ => self.emit_const_at(
                0.0,
                required_context_span(call_span, "clock constructor call")?,
            ),
        }
    }

    fn lower_clock_call_phase(
        &mut self,
        args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        if args.is_empty()
            && let Some(phase) = self
                .current_update_target_clock_timing()
                .map(|timing| timing.phase_seconds)
        {
            return self.emit_const_at(phase, required_context_span(call_span, "Clock() call")?);
        }
        self.emit_const_at(0.0, required_context_span(call_span, "Clock() call")?)
    }

    pub(super) fn lower_optional_clock_factor(
        &mut self,
        expr: Option<&rumoca_core::Expression>,
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        match expr {
            Some(expr) => self.lower_expr(expr, scope, call_depth),
            None => self.emit_const_at(
                1.0,
                required_context_span(call_span, "clock factor default")?,
            ),
        }
    }

    pub(super) fn emit_periodic_tick(
        &mut self,
        phase: Reg,
        period: Reg,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let time = self.emit_load_time_at(span)?;
        let delta = self.emit_binary_at(BinaryOp::Sub, time, phase, span)?;
        let one = self.emit_const_at(1.0, span)?;
        let max_period = self.emit_binary_at(BinaryOp::Max, period, one, span)?;
        let tol_scale = self.emit_const_at(1.0e-9, span)?;
        let tol = self.emit_binary_at(BinaryOp::Mul, tol_scale, max_period, span)?;
        let neg_tol = self.emit_unary_at(UnaryOp::Neg, tol, span)?;
        let after_start = self.emit_compare_at(CompareOp::Ge, delta, neg_tol, span)?;

        let quotient = self.emit_binary_at(BinaryOp::Div, delta, period, span)?;
        let half = self.emit_const_at(0.5, span)?;
        let shifted = self.emit_binary_at(BinaryOp::Add, quotient, half, span)?;
        let tick_index = self.emit_unary_at(UnaryOp::Floor, shifted, span)?;
        let nearest = self.emit_binary_at(BinaryOp::Mul, tick_index, period, span)?;
        let diff = self.emit_binary_at(BinaryOp::Sub, delta, nearest, span)?;
        let abs_diff = self.emit_unary_at(UnaryOp::Abs, diff, span)?;
        let at_tick = self.emit_compare_at(CompareOp::Le, abs_diff, tol, span)?;
        self.emit_binary_at(BinaryOp::And, after_start, at_tick, span)
    }

    pub(super) fn lower_clock_interval_expr(
        &mut self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        match expr {
            // MLS §16.5.1: interval(v) returns the associated clock interval for
            // the clocked variable v when that metadata is known at runtime.
            rumoca_core::Expression::VarRef { .. }
            | rumoca_core::Expression::Index { .. }
            | rumoca_core::Expression::FieldAccess { .. } => self
                .clock_interval_metadata(expr)
                .map(|value| {
                    self.emit_const_at(
                        value,
                        required_source_span(expr, "clock interval expression")?,
                    )
                    .map(Some)
                })
                .unwrap_or(Ok(None)),
            rumoca_core::Expression::FunctionCall {
                name, args, span, ..
            } => {
                let short = intrinsic_short_name(name.as_str());
                match short {
                    "Clock" => self.lower_clock_interval_clock_call(args, *span, scope, call_depth),
                    "subSample" => self.lower_scaled_clock_interval(
                        args,
                        *span,
                        scope,
                        call_depth,
                        BinaryOp::Mul,
                    ),
                    "superSample" => self.lower_scaled_clock_interval(
                        args,
                        *span,
                        scope,
                        call_depth,
                        BinaryOp::Div,
                    ),
                    "shiftSample" | "backSample" => {
                        self.lower_passthrough_clock_interval(args, *span, scope, call_depth)
                    }
                    _ => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }

    pub(super) fn lower_clock_phase_expr(
        &mut self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        match expr {
            rumoca_core::Expression::VarRef { .. }
            | rumoca_core::Expression::Index { .. }
            | rumoca_core::Expression::FieldAccess { .. } => {
                let phase = self
                    .clock_timing_metadata(expr)
                    .map_or(0.0, |timing| timing.phase_seconds);
                self.emit_const_at(phase, required_source_span(expr, "clock phase expression")?)
            }
            rumoca_core::Expression::FunctionCall {
                name, args, span, ..
            } => {
                let short = intrinsic_short_name(name.as_str());
                self.lower_clock_constructor_phase(short, args, *span, scope, call_depth)
            }
            _ => self.emit_const_at(0.0, required_source_span(expr, "clock phase expression")?),
        }
    }

    pub(super) fn lower_clock_interval_clock_call(
        &mut self,
        args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        match args {
            [] => {
                let period = self
                    .current_update_target_clock_timing()
                    .map_or(1.0, |timing| timing.period_seconds);
                self.emit_const_at(period, required_context_span(call_span, "Clock() call")?)
                    .map(Some)
            }
            [interval] => self.lower_expr(interval, scope, call_depth).map(Some),
            [numerator_expr, denominator_expr, ..] => {
                let numerator = self.lower_expr(numerator_expr, scope, call_depth)?;
                let denominator = self.lower_expr(denominator_expr, scope, call_depth)?;
                self.emit_binary_at(
                    BinaryOp::Div,
                    numerator,
                    denominator,
                    denominator_expr
                        .span()
                        .or_else(|| numerator_expr.span())
                        .or_else(|| (!call_span.is_dummy()).then_some(call_span))
                        .ok_or_else(|| missing_clock_source_span("Clock() ratio"))?,
                )
                .map(Some)
            }
        }
    }

    fn current_update_target_clock_timing(&self) -> Option<&dae::ClockSchedule> {
        let target = self.current_update_target?;
        self.clock_timings?.iter().find_map(|(name, timing)| {
            (self.layout.binding(name.as_str()) == Some(target)).then_some(timing)
        })
    }

    fn clock_interval_metadata(&self, expr: &rumoca_core::Expression) -> Option<f64> {
        let intervals = self.clock_intervals?;
        clock_component_metadata(expr, intervals).copied()
    }

    fn clock_timing_metadata(
        &self,
        expr: &rumoca_core::Expression,
    ) -> Option<&'a dae::ClockSchedule> {
        let timings = self.clock_timings?;
        clock_component_metadata(expr, timings)
    }

    pub(super) fn lower_scaled_clock_interval(
        &mut self,
        args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
        op: BinaryOp,
    ) -> Result<Option<Reg>, LowerError> {
        let Some(base_expr) = args.first() else {
            return Ok(None);
        };
        if args.len() == 1
            && let Some(period) = self
                .current_update_target_clock_timing()
                .map(|timing| timing.period_seconds)
        {
            return self
                .emit_const_at(
                    period,
                    source_span_or_context(base_expr, call_span, "scaled clock base expression")?,
                )
                .map(Some);
        }
        let Some(base) = self.lower_clock_interval_expr(base_expr, scope, call_depth)? else {
            return Ok(None);
        };
        let factor = match args.get(1) {
            Some(factor_expr) => self.lower_expr(factor_expr, scope, call_depth)?,
            None => self.emit_const_at(
                1.0,
                source_span_or_context(base_expr, call_span, "scaled clock base expression")?,
            )?,
        };
        let span = source_span_or_context(base_expr, call_span, "scaled clock base expression")?;
        self.emit_binary_at(op, base, factor, span).map(Some)
    }

    pub(super) fn lower_passthrough_clock_interval(
        &mut self,
        args: &[rumoca_core::Expression],
        _call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let Some(base_expr) = args.first() else {
            return Ok(None);
        };
        self.lower_clock_interval_expr(base_expr, scope, call_depth)
    }
}

fn clock_base_metadata_key(expr: &rumoca_core::Expression) -> Option<String> {
    match expr {
        rumoca_core::Expression::VarRef { name, .. } => Some(name.as_str().to_string()),
        rumoca_core::Expression::Index { base, .. } => clock_base_metadata_key(base),
        rumoca_core::Expression::FieldAccess { .. } => binding_base_key(expr).ok(),
        _ => None,
    }
}

fn clock_component_metadata<'m, T>(
    expr: &rumoca_core::Expression,
    metadata: &'m indexmap::IndexMap<String, T>,
) -> Option<&'m T> {
    if let Ok(key) = binding_base_key(expr)
        && let Some(value) = metadata.get(key.as_str())
    {
        return Some(value);
    }
    let key = clock_base_metadata_key(expr)?;
    metadata.get(key.as_str())
}

fn source_span_or_context(
    expr: &rumoca_core::Expression,
    context_span: rumoca_core::Span,
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    expr.span()
        .or_else(|| (!context_span.is_dummy()).then_some(context_span))
        .ok_or_else(|| missing_clock_source_span(context))
}

fn required_context_span(
    span: rumoca_core::Span,
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    (!span.is_dummy())
        .then_some(span)
        .ok_or_else(|| missing_clock_source_span(context))
}

fn required_source_span(
    expr: &rumoca_core::Expression,
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    expr.span()
        .ok_or_else(|| missing_clock_source_span(context))
}

fn missing_clock_source_span(context: &'static str) -> LowerError {
    LowerError::UnspannedContractViolation {
        reason: format!("{context} requires source span"),
    }
}
