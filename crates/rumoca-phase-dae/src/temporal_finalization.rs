//! Final elimination of internal temporal call nodes from solver-facing DAE.

use rumoca_core::{Expression, ExpressionRewriter, Literal, OpBinary, Span};
use rumoca_ir_dae as dae;

use crate::ToDaeError;

pub(crate) fn lower_internal_sample_ticks(dae_model: &mut dae::Dae) -> Result<(), ToDaeError> {
    let mut rewriter = InternalSampleTickRewriter { error: None };
    rewrite_equations(&mut dae_model.continuous.equations, &mut rewriter);
    rewrite_equations(&mut dae_model.initialization.equations, &mut rewriter);
    rewrite_equations(&mut dae_model.discrete.real_updates, &mut rewriter);
    rewrite_equations(&mut dae_model.discrete.valued_updates, &mut rewriter);
    rewrite_equations(&mut dae_model.conditions.equations, &mut rewriter);
    rewrite_expressions(&mut dae_model.conditions.relations, &mut rewriter);
    rewrite_expressions(
        &mut dae_model.events.synthetic_root_conditions,
        &mut rewriter,
    );
    rewrite_expressions(&mut dae_model.clocks.triggered_conditions, &mut rewriter);
    rewrite_event_actions(&mut dae_model.events.event_actions, &mut rewriter);
    rewrite_delay_channels(&mut dae_model.events.delay_channels, &mut rewriter);
    match rewriter.error {
        Some(error) => Err(error),
        None => Ok(()),
    }
}

fn rewrite_equations(equations: &mut [dae::Equation], rewriter: &mut InternalSampleTickRewriter) {
    for equation in equations {
        equation.rhs = rewriter.rewrite_expression(&equation.rhs);
    }
}

fn rewrite_expressions(expressions: &mut [Expression], rewriter: &mut InternalSampleTickRewriter) {
    for expression in expressions {
        *expression = rewriter.rewrite_expression(expression);
    }
}

fn rewrite_event_actions(
    actions: &mut [dae::DaeEventAction],
    rewriter: &mut InternalSampleTickRewriter,
) {
    for action in actions {
        action.condition = rewriter.rewrite_expression(&action.condition);
        let message = match &mut action.kind {
            dae::DaeEventActionKind::Assert { message }
            | dae::DaeEventActionKind::Terminate { message } => message,
        };
        *message = rewriter.rewrite_expression(message);
    }
}

fn rewrite_delay_channels(
    channels: &mut [dae::DaeDelayChannel],
    rewriter: &mut InternalSampleTickRewriter,
) {
    for channel in channels {
        channel.source = rewriter.rewrite_expression(&channel.source);
        channel.delay_time = rewriter.rewrite_expression(&channel.delay_time);
        if let Some(delay_max) = &mut channel.delay_max {
            *delay_max = rewriter.rewrite_expression(delay_max);
        }
    }
}

struct InternalSampleTickRewriter {
    error: Option<ToDaeError>,
}

impl ExpressionRewriter for InternalSampleTickRewriter {
    fn rewrite_expression(&mut self, expression: &Expression) -> Expression {
        if self.error.is_some() {
            return expression.clone();
        }
        let Expression::FunctionCall {
            name, args, span, ..
        } = expression
        else {
            return self.walk_expression(expression);
        };
        if name.as_str() != rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME {
            return self.walk_expression(expression);
        }
        match periodic_tick_expression(args, *span) {
            Ok(expression) => expression,
            Err(error) => {
                self.error = Some(error);
                expression.clone()
            }
        }
    }
}

fn periodic_tick_expression(args: &[Expression], span: Span) -> Result<Expression, ToDaeError> {
    let (phase, period) = match args {
        [phase, period] => (phase.clone(), period.clone()),
        [_internal_id, phase, period, ..] => (phase.clone(), period.clone()),
        _ => {
            return Err(ToDaeError::source_temporal_operator_survived_dae_boundary(
                format!(
                    "internal sample tick has {} arguments; expected phase/period metadata",
                    args.len()
                ),
                span,
            ));
        }
    };
    let time = Expression::VarRef {
        name: rumoca_core::Reference::generated("time"),
        subscripts: Vec::new(),
        span,
    };
    let quotient = binary(
        OpBinary::Div,
        binary(OpBinary::Sub, time, phase, span),
        period,
        span,
    );
    let tick_index = builtin(
        rumoca_core::BuiltinFunction::Floor,
        vec![binary(
            OpBinary::Add,
            quotient.clone(),
            real_literal(0.5, span),
            span,
        )],
        span,
    );
    let coordinate_error = builtin(
        rumoca_core::BuiltinFunction::Abs,
        vec![binary(
            OpBinary::Sub,
            quotient.clone(),
            tick_index.clone(),
            span,
        )],
        span,
    );
    let coordinate_scale = builtin(
        rumoca_core::BuiltinFunction::Max,
        vec![
            builtin(
                rumoca_core::BuiltinFunction::Abs,
                vec![quotient.clone()],
                span,
            ),
            builtin(rumoca_core::BuiltinFunction::Abs, vec![tick_index], span),
        ],
        span,
    );
    let tolerance = binary(
        OpBinary::Mul,
        real_literal(rumoca_core::SCHEDULE_TIME_RELATIVE_TOLERANCE, span),
        binary(
            OpBinary::Add,
            real_literal(1.0, span),
            coordinate_scale,
            span,
        ),
        span,
    );
    Ok(binary(
        OpBinary::And,
        binary(OpBinary::Ge, quotient, real_literal(0.0, span), span),
        binary(OpBinary::Le, coordinate_error, tolerance, span),
        span,
    ))
}

fn binary(op: OpBinary, lhs: Expression, rhs: Expression, span: Span) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn builtin(
    function: rumoca_core::BuiltinFunction,
    args: Vec<Expression>,
    span: Span,
) -> Expression {
    Expression::BuiltinCall {
        function,
        args,
        span,
    }
}

fn real_literal(value: f64, span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span,
    }
}
