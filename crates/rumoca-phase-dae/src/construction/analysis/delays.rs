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
    analyzer.visit_model(flat)?;
    Ok(analyzer.plans)
}

struct DelayAnalyzer<'model> {
    constants: &'model EvalContext,
    plans: HashMap<Span, DelayPlan>,
    in_function: bool,
}

impl DelayAnalyzer<'_> {
    fn visit_model(&mut self, flat: &flat::Model) -> Result<(), ToDaeError> {
        all_model_expressions(flat)
            .chain(structured_template_expressions(&flat.structured_equations))
            .chain(structured_template_expressions(
                &flat.initial_structured_equations,
            ))
            .try_for_each(|expression| self.visit_expression(expression))?;
        for assertion in flat
            .assert_equations
            .iter()
            .chain(&flat.initial_assert_equations)
        {
            self.visit_expression(&assertion.condition)?;
            self.visit_expression(&assertion.message)?;
            if let Some(level) = &assertion.level {
                self.visit_expression(level)?;
            }
        }
        flat.algorithms
            .iter()
            .chain(&flat.initial_algorithms)
            .flat_map(|algorithm| &algorithm.statements)
            .try_for_each(|statement| self.visit_statement(statement))?;
        flat.when_chains
            .iter()
            .flat_map(flat::WhenChain::branches)
            .try_for_each(|branch| {
                self.visit_expression(&branch.condition)?;
                self.visit_when_equations(&branch.equations)
            })?;
        self.in_function = true;
        flat.functions
            .values()
            .try_for_each(|function| self.visit_function(function))
    }

    fn visit_function(&mut self, function: &rumoca_core::Function) -> Result<(), ToDaeError> {
        function
            .inputs
            .iter()
            .chain(&function.outputs)
            .chain(&function.locals)
            .try_for_each(|parameter| self.visit_function_parameter(parameter))?;
        function
            .body
            .iter()
            .try_for_each(|statement| self.visit_statement(statement))
    }

    fn visit_function_parameter(
        &mut self,
        parameter: &rumoca_core::FunctionParam,
    ) -> Result<(), ToDaeError> {
        [&parameter.default, &parameter.min, &parameter.max]
            .into_iter()
            .flatten()
            .try_for_each(|expression| self.visit_expression(expression))?;
        parameter
            .shape_expr
            .iter()
            .try_for_each(|subscript| self.visit_subscript(subscript))
    }

    fn visit_when_equations(&mut self, equations: &[flat::WhenEquation]) -> Result<(), ToDaeError> {
        equations
            .iter()
            .try_for_each(|equation| self.visit_when_equation(equation))
    }

    fn visit_when_equation(&mut self, equation: &flat::WhenEquation) -> Result<(), ToDaeError> {
        match equation {
            flat::WhenEquation::Assign { value, .. }
            | flat::WhenEquation::Reinit { value, .. }
            | flat::WhenEquation::Terminate { message: value, .. }
            | flat::WhenEquation::FunctionCallOutputs {
                function: value, ..
            } => self.visit_expression(value),
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                self.visit_expression(condition)?;
                self.visit_expression(message)?;
                level
                    .iter()
                    .try_for_each(|level| self.visit_expression(level))
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => self.visit_conditional_when(branches, else_branch),
        }
    }

    fn visit_conditional_when(
        &mut self,
        branches: &[(Expression, Vec<flat::WhenEquation>)],
        else_branch: &Option<Vec<flat::WhenEquation>>,
    ) -> Result<(), ToDaeError> {
        branches.iter().try_for_each(|(condition, equations)| {
            self.visit_expression(condition)?;
            self.visit_when_equations(equations)
        })?;
        else_branch
            .as_deref()
            .map_or(Ok(()), |equations| self.visit_when_equations(equations))
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
