use rumoca_ir_dae as dae;
use rumoca_ir_solve::{BinaryOp, CompareOp, Reg, UnaryOp};

use super::helpers::intrinsic_short_name;
use super::{LowerBuilder, LowerError, Scope, ValueMode};

impl<'a> LowerBuilder<'a> {
    pub(super) fn lower_interval_intrinsic(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let Some(clock_expr) = args.first() else {
            return Ok(self.emit_const(1.0));
        };
        if let Some(reg) = self.lower_clock_interval_expr(clock_expr, scope, call_depth)? {
            return Ok(reg);
        }
        Ok(self.emit_const(1.0))
    }

    pub(super) fn lower_clock_constructor_tick(
        &mut self,
        short_name: &str,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        if self.clock_constructor_is_triggered_event(short_name, args) {
            let Some(condition) = args.first() else {
                return Ok(Some(self.emit_const(0.0)));
            };
            if self.value_mode == ValueMode::Pre {
                return Ok(Some(self.emit_const(0.0)));
            }
            return self.lower_expr(condition, scope, call_depth).map(Some);
        }
        let Some(period) =
            self.lower_clock_constructor_period(short_name, args, scope, call_depth)?
        else {
            return Ok(None);
        };
        if self.value_mode == ValueMode::Pre {
            return Ok(Some(self.emit_const(0.0)));
        }
        let phase = self.lower_clock_constructor_phase(short_name, args, scope, call_depth)?;
        Ok(Some(self.emit_periodic_tick(phase, period)))
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
        if self.value_mode == ValueMode::Pre {
            return Ok(Some(self.emit_const(0.0)));
        }
        let phase = self.lower_clock_phase_expr(expr, scope, call_depth)?;
        Ok(Some(self.emit_periodic_tick(phase, period)))
    }

    pub(super) fn lower_clock_constructor_period(
        &mut self,
        short_name: &str,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        match short_name {
            "Clock" => self.lower_clock_interval_clock_call(args, scope, call_depth),
            "subSample" => self.lower_scaled_clock_interval(args, scope, call_depth, BinaryOp::Mul),
            "superSample" => {
                self.lower_scaled_clock_interval(args, scope, call_depth, BinaryOp::Div)
            }
            "shiftSample" | "backSample" => {
                self.lower_passthrough_clock_interval(args, scope, call_depth)
            }
            _ => Ok(None),
        }
    }

    pub(super) fn lower_clock_constructor_phase(
        &mut self,
        short_name: &str,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        match short_name {
            "Clock" => self.lower_clock_call_phase(args, scope, call_depth),
            "subSample" | "superSample" => {
                if args.len() == 1
                    && let Some(phase) = self
                        .current_update_target_clock_timing()
                        .map(|timing| timing.phase_seconds)
                {
                    return Ok(self.emit_const(phase));
                }
                let Some(base_expr) = args.first() else {
                    return Ok(self.emit_const(0.0));
                };
                self.lower_clock_phase_expr(base_expr, scope, call_depth)
            }
            "shiftSample" | "backSample" => {
                let Some(base_expr) = args.first() else {
                    return Ok(self.emit_const(0.0));
                };
                let Some(base_period) =
                    self.lower_clock_interval_expr(base_expr, scope, call_depth)?
                else {
                    return self.lower_clock_phase_expr(base_expr, scope, call_depth);
                };
                let base_phase = self.lower_clock_phase_expr(base_expr, scope, call_depth)?;
                let shift = self.lower_optional_clock_factor(args.get(1), scope, call_depth)?;
                let resolution =
                    self.lower_optional_clock_factor(args.get(2), scope, call_depth)?;
                let fraction = self.emit_binary(BinaryOp::Div, shift, resolution);
                let offset = self.emit_binary(BinaryOp::Mul, base_period, fraction);
                if short_name == "backSample" {
                    Ok(self.emit_binary(BinaryOp::Sub, base_phase, offset))
                } else {
                    Ok(self.emit_binary(BinaryOp::Add, base_phase, offset))
                }
            }
            _ => Ok(self.emit_const(0.0)),
        }
    }

    fn lower_clock_call_phase(
        &mut self,
        args: &[rumoca_core::Expression],
        _scope: &Scope,
        _call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if args.is_empty()
            && let Some(phase) = self
                .current_update_target_clock_timing()
                .map(|timing| timing.phase_seconds)
        {
            return Ok(self.emit_const(phase));
        }
        Ok(self.emit_const(0.0))
    }

    pub(super) fn lower_optional_clock_factor(
        &mut self,
        expr: Option<&rumoca_core::Expression>,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        match expr {
            Some(expr) => self.lower_expr(expr, scope, call_depth),
            None => Ok(self.emit_const(1.0)),
        }
    }

    pub(super) fn emit_periodic_tick(&mut self, phase: Reg, period: Reg) -> Reg {
        let time = self.emit_load_time();
        let delta = self.emit_binary(BinaryOp::Sub, time, phase);
        let one = self.emit_const(1.0);
        let max_period = self.emit_binary(BinaryOp::Max, period, one);
        let tol_scale = self.emit_const(1.0e-9);
        let tol = self.emit_binary(BinaryOp::Mul, tol_scale, max_period);
        let neg_tol = self.emit_unary(UnaryOp::Neg, tol);
        let after_start = self.emit_compare(CompareOp::Ge, delta, neg_tol);

        let quotient = self.emit_binary(BinaryOp::Div, delta, period);
        let half = self.emit_const(0.5);
        let shifted = self.emit_binary(BinaryOp::Add, quotient, half);
        let tick_index = self.emit_unary(UnaryOp::Floor, shifted);
        let nearest = self.emit_binary(BinaryOp::Mul, tick_index, period);
        let diff = self.emit_binary(BinaryOp::Sub, delta, nearest);
        let abs_diff = self.emit_unary(UnaryOp::Abs, diff);
        let at_tick = self.emit_compare(CompareOp::Le, abs_diff, tol);
        self.emit_binary(BinaryOp::And, after_start, at_tick)
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
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => Ok(self
                .clock_intervals
                .and_then(|intervals| intervals.get(name.as_str()).copied())
                .map(|value| self.emit_const(value))),
            rumoca_core::Expression::FunctionCall { name, args, .. } => {
                let short = intrinsic_short_name(name.as_str());
                match short {
                    "Clock" => self.lower_clock_interval_clock_call(args, scope, call_depth),
                    "subSample" => {
                        self.lower_scaled_clock_interval(args, scope, call_depth, BinaryOp::Mul)
                    }
                    "superSample" => {
                        self.lower_scaled_clock_interval(args, scope, call_depth, BinaryOp::Div)
                    }
                    "shiftSample" | "backSample" => {
                        self.lower_passthrough_clock_interval(args, scope, call_depth)
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
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => {
                let phase = self
                    .clock_timings
                    .and_then(|timings| timings.get(name.as_str()))
                    .map_or(0.0, |timing| timing.phase_seconds);
                Ok(self.emit_const(phase))
            }
            rumoca_core::Expression::FunctionCall { name, args, .. } => {
                let short = intrinsic_short_name(name.as_str());
                self.lower_clock_constructor_phase(short, args, scope, call_depth)
            }
            _ => Ok(self.emit_const(0.0)),
        }
    }

    pub(super) fn lower_clock_interval_clock_call(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        match args {
            [] => {
                let period = self
                    .current_update_target_clock_timing()
                    .map_or(1.0, |timing| timing.period_seconds);
                Ok(Some(self.emit_const(period)))
            }
            [interval] => self.lower_expr(interval, scope, call_depth).map(Some),
            [numerator_expr, denominator_expr, ..] => {
                let numerator = self.lower_expr(numerator_expr, scope, call_depth)?;
                let denominator = self.lower_expr(denominator_expr, scope, call_depth)?;
                Ok(Some(self.emit_binary(
                    BinaryOp::Div,
                    numerator,
                    denominator,
                )))
            }
        }
    }

    fn current_update_target_clock_timing(&self) -> Option<&dae::ClockSchedule> {
        let target = self.current_update_target?;
        self.clock_timings?.iter().find_map(|(name, timing)| {
            (self.layout.binding(name.as_str()) == Some(target)).then_some(timing)
        })
    }

    pub(super) fn lower_scaled_clock_interval(
        &mut self,
        args: &[rumoca_core::Expression],
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
            return Ok(Some(self.emit_const(period)));
        }
        let Some(base) = self.lower_clock_interval_expr(base_expr, scope, call_depth)? else {
            return Ok(None);
        };
        let factor = match args.get(1) {
            Some(factor_expr) => self.lower_expr(factor_expr, scope, call_depth)?,
            None => self.emit_const(1.0),
        };
        Ok(Some(self.emit_binary(op, base, factor)))
    }

    pub(super) fn lower_passthrough_clock_interval(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let Some(base_expr) = args.first() else {
            return Ok(None);
        };
        self.lower_clock_interval_expr(base_expr, scope, call_depth)
    }
}
