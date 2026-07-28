//! Final elimination of internal temporal call nodes from solver-facing DAE.

use rumoca_core::{Expression, ExpressionRewriter, Literal, OpBinary, Span};
use rumoca_ir_dae as dae;

use crate::ToDaeError;

pub(crate) fn lower_internal_sample_ticks(dae_model: &mut dae::Dae) -> Result<(), ToDaeError> {
    let mut rewriter = InternalSampleTickRewriter { error: None };
    rewrite_equations(&mut dae_model.continuous.equations, &mut rewriter);
    rewrite_structured_equations(
        &mut dae_model.continuous.structured_equations,
        &mut rewriter,
    );
    rewrite_equations(&mut dae_model.initialization.equations, &mut rewriter);
    rewrite_structured_equations(
        &mut dae_model.initialization.structured_equations,
        &mut rewriter,
    );
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

fn rewrite_structured_equations(
    families: &mut [dae::StructuredEquationFamily],
    rewriter: &mut InternalSampleTickRewriter,
) {
    for expression in families
        .iter_mut()
        .filter_map(|family| family.template.as_mut())
        .flat_map(|template| &mut template.body)
    {
        *expression = rewriter.rewrite_expression(expression);
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
            name,
            args,
            is_constructor,
            span,
        } = expression
        else {
            return self.walk_expression(expression);
        };
        if name.as_str() == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME {
            let args = args
                .iter()
                .map(|arg| self.rewrite_expression(arg))
                .collect::<Vec<_>>();
            return match periodic_tick_expression(&args, *span) {
                Ok(expression) => expression,
                Err(error) => {
                    self.error = Some(error);
                    expression.clone()
                }
            };
        }
        let Some(intrinsic) = source_synchronous_intrinsic(name) else {
            return self.walk_expression(expression);
        };
        Expression::FunctionCall {
            name: rumoca_core::Reference::generated(intrinsic),
            args: args
                .iter()
                .map(|arg| self.rewrite_expression(arg))
                .collect(),
            is_constructor: *is_constructor,
            span: *span,
        }
    }
}

fn source_synchronous_intrinsic(name: &rumoca_core::Reference) -> Option<&'static str> {
    if name.is_generated() {
        return None;
    }
    match name.last_segment() {
        "Clock" => Some("Clock"),
        "hold" => Some("hold"),
        "subSample" => Some("subSample"),
        "superSample" => Some("superSample"),
        "shiftSample" => Some("shiftSample"),
        "backSample" => Some("backSample"),
        "noClock" => Some("noClock"),
        "firstTick" => Some("firstTick"),
        _ => None,
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

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("temporal_finalization_test.mo"),
            4,
            16,
        )
    }

    #[test]
    fn source_synchronous_calls_become_generated_intrinsics() {
        let span = test_span();
        let mut dae_model = dae::Dae::new();
        dae_model
            .discrete
            .real_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new("y"),
                Expression::FunctionCall {
                    name: rumoca_core::Reference::new("Modelica.Clocked.subSample"),
                    args: vec![Expression::VarRef {
                        name: rumoca_core::Reference::new("u"),
                        subscripts: vec![],
                        span,
                    }],
                    is_constructor: false,
                    span,
                },
                span,
                "clocked fixture",
            ));

        lower_internal_sample_ticks(&mut dae_model).expect("synchronous call should lower");

        assert!(matches!(
            &dae_model.discrete.real_updates[0].rhs,
            Expression::FunctionCall { name, span: call_span, .. }
                if name.as_str() == "subSample" && name.is_generated() && *call_span == span
        ));
    }
}
