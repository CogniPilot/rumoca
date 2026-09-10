use rumoca_core::FallibleExpressionVisitor;
use rumoca_ir_flat::FallibleStatementVisitor;

use super::*;

#[derive(Clone, Copy)]
pub(in crate::construction) enum DelayPlan {
    Fixed(PositiveParameterPlan),
    Bounded(PositiveParameterPlan),
}

#[derive(Clone, Copy)]
pub(in crate::construction) struct PositiveParameterPlan {
    value: f64,
    provenance: Span,
}

impl PositiveParameterPlan {
    pub(in crate::construction) fn value(self) -> f64 {
        self.value
    }

    pub(in crate::construction) fn provenance(self) -> Span {
        self.provenance
    }
}

pub(super) fn analyze_delays(
    flat: &flat::Model,
    constants: &EvalContext,
) -> Result<HashMap<Span, DelayPlan>, ToDaeError> {
    let mut analyzer = DelayAnalyzer {
        constants,
        plans: HashMap::new(),
        in_function: false,
    };
    analyzer.visit_model_owners(flat)?;
    Ok(analyzer.plans)
}

struct DelayAnalyzer<'model> {
    constants: &'model EvalContext,
    plans: HashMap<Span, DelayPlan>,
    in_function: bool,
}

impl ModelExpressionOwnerVisitor for DelayAnalyzer<'_> {
    fn enter_function_owners(&mut self) -> Result<(), Self::Error> {
        self.in_function = true;
        Ok(())
    }
}

impl FallibleExpressionVisitor for DelayAnalyzer<'_> {
    type Error = ToDaeError;

    fn visit_expression(&mut self, expression: &Expression) -> Result<(), Self::Error> {
        if let Expression::BuiltinCall {
            function: BuiltinFunction::Delay,
            args,
            span,
        } = expression
        {
            if self.in_function {
                return Err(ToDaeError::unsupported_runtime_operator(
                    "delay",
                    "delay is prohibited in a function semantic owner",
                    *span,
                ));
            }
            let plan = delay_plan(args, self.constants, *span)?;
            self.plans.insert(*span, plan);
        }
        self.walk_expression(expression)
    }
}

impl FallibleStatementVisitor for DelayAnalyzer<'_> {}

fn delay_plan(
    arguments: &[Expression],
    constants: &EvalContext,
    span: Span,
) -> Result<DelayPlan, ToDaeError> {
    match arguments {
        [_, delay_time] => {
            let timing = positive_parameter(delay_time, constants, "delayTime", span)?;
            Ok(DelayPlan::Fixed(timing))
        }
        [_, delay_time, delay_max] => {
            let delay_max = positive_parameter(delay_max, constants, "delayMax", span)?;
            let delay_max_value = delay_max.value();
            if let Ok(value) = eval_expr(delay_time, constants)
                && let Some(delay_time) = value.to_real()
                && (!delay_time.is_finite() || delay_time <= 0.0 || delay_time > delay_max_value)
            {
                return Err(ToDaeError::unsupported_runtime_operator(
                    "delay",
                    format!(
                        "parameter-evaluable delayTime must satisfy 0 < delayTime <= delayMax; \
                         got delayTime={delay_time}, delayMax={delay_max_value}"
                    ),
                    span,
                ));
            }
            Ok(DelayPlan::Bounded(delay_max))
        }
        _ => Err(ToDaeError::unsupported_runtime_operator(
            "delay",
            "delay(source, delayTime[, delayMax]) requires two or three arguments",
            span,
        )),
    }
}

fn positive_parameter(
    expression: &Expression,
    constants: &EvalContext,
    name: &str,
    owner: Span,
) -> Result<PositiveParameterPlan, ToDaeError> {
    let provenance = expression_span(expression)?;
    let value = eval_expr(expression, constants).map_err(|error| {
        ToDaeError::unsupported_runtime_operator(
            "delay",
            format!("{name} is not parameter-evaluable: {error}"),
            owner,
        )
    })?;
    value
        .to_real()
        .filter(|value| value.is_finite() && *value > 0.0)
        .map(|value| PositiveParameterPlan { value, provenance })
        .ok_or_else(|| {
            ToDaeError::unsupported_runtime_operator(
                "delay",
                format!("{name} must evaluate to a finite positive scalar Real"),
                owner,
            )
        })
}
