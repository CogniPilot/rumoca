//! Lower phase-sensitive runtime operators into explicit Appendix-B inputs.

use std::collections::HashSet;

use rumoca_core::{ExpressionRewriter, ExpressionVisitor};
use rumoca_ir_dae::{self as dae, DaeExpressionRewriter, DaeVisitor};

use crate::ToDaeError;

pub(crate) fn lower_runtime_operators(dae_model: &mut dae::Dae) -> Result<(), ToDaeError> {
    lower_terminal_operator(dae_model)?;
    lower_delay_operators(dae_model)?;
    Ok(())
}

fn lower_delay_operators(dae_model: &mut dae::Dae) -> Result<(), ToDaeError> {
    let mut rewriter = DelayRewriter {
        next_channel_index: dae_model.events.delay_channels.len(),
        ..DelayRewriter::default()
    };
    rewriter.rewrite_dae(dae_model);
    if let Some(error) = rewriter.error {
        return Err(error);
    }
    if rewriter.channels.is_empty() {
        return Ok(());
    }

    for channel in &rewriter.channels {
        declare_runtime_parameter(
            dae_model,
            channel.value_parameter.clone(),
            channel.span,
            "runtime transport-delay value",
            false,
            false,
        )?;
        dae_model
            .initialization
            .equations
            .push(dae::Equation::explicit(
                rumoca_core::Reference::generated(channel.value_parameter.as_str()),
                channel.source.clone(),
                channel.span,
                "initial transport-delay identity",
            ));
    }
    dae_model.events.delay_channels.extend(rewriter.channels);
    classify_discrete_delay_sources(dae_model);
    Ok(())
}

fn lower_terminal_operator(dae_model: &mut dae::Dae) -> Result<(), ToDaeError> {
    let mut finder = TerminalFinder { span: None };
    finder.visit_dae(dae_model);
    let Some(span) = finder.span else {
        return Ok(());
    };

    let name = rumoca_core::VarName::new(rumoca_core::TERMINAL_EVENT_PARAMETER_NAME);
    declare_runtime_parameter(
        dae_model,
        name,
        span,
        "runtime terminal-event marker",
        true,
        true,
    )?;
    dae_model.events.has_terminal_event = true;
    let mut rewriter = TerminalRewriter;
    rewriter.rewrite_dae(dae_model);
    Ok(())
}

#[derive(Default)]
struct DelayRewriter {
    next_channel_index: usize,
    channels: Vec<dae::DaeDelayChannel>,
    error: Option<ToDaeError>,
}

impl DelayRewriter {
    fn record_contract_error(&mut self, message: impl Into<String>, span: rumoca_core::Span) {
        if self.error.is_none() {
            self.error = Some(ToDaeError::runtime_contract_violation_with_span(
                message, span,
            ));
        }
    }

    fn rewrite_delay(
        &mut self,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if args.len() != 2 && args.len() != 3 {
            self.record_contract_error(
                format!(
                    "delay() requires two or three arguments, but {} were provided",
                    args.len()
                ),
                span,
            );
            return self.walk_builtin_call_expression(
                rumoca_core::BuiltinFunction::Delay,
                args,
                span,
            );
        }

        let source = self.rewrite_expression(&args[0]);
        let delay_time = self.rewrite_expression(&args[1]);
        let delay_max = args.get(2).map(|expr| self.rewrite_expression(expr));
        if let Some(value) = literal_real(&delay_time) {
            if value < 0.0 {
                self.record_contract_error("delayTime must be non-negative", span);
            }
            if value == 0.0 {
                return source;
            }
        }
        if delay_max
            .as_ref()
            .and_then(literal_real)
            .is_some_and(|value| value < 0.0)
        {
            self.record_contract_error("delayMax must be non-negative", span);
        }
        if let (Some(delay_time), Some(delay_max)) = (
            literal_real(&delay_time),
            delay_max.as_ref().and_then(literal_real),
        ) && delay_time > delay_max
        {
            self.record_contract_error("delayTime must not exceed delayMax", span);
        }

        let value_parameter =
            rumoca_core::delay_slot_name(self.next_channel_index + self.channels.len());
        self.channels.push(dae::DaeDelayChannel {
            value_parameter: value_parameter.clone(),
            source,
            delay_time,
            delay_max,
            source_is_discrete: false,
            span,
        });
        rumoca_core::Expression::VarRef {
            name: value_parameter.into(),
            subscripts: Vec::new(),
            span,
        }
    }
}

impl ExpressionRewriter for DelayRewriter {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        match expr {
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Delay,
                args,
                span,
            } => self.rewrite_delay(args, *span),
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor: false,
                span,
            } if name.var_name().last_segment() == "delay" => self.rewrite_delay(args, *span),
            _ => self.walk_expression(expr),
        }
    }
}

impl DaeExpressionRewriter for DelayRewriter {}

fn literal_real(expression: &rumoca_core::Expression) -> Option<f64> {
    match expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        } => Some(*value),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => Some(*value as f64),
        _ => None,
    }
}

fn classify_discrete_delay_sources(dae_model: &mut dae::Dae) {
    let mut discrete_names = HashSet::new();
    for name in dae_model
        .variables
        .parameters
        .keys()
        .filter(|name| !rumoca_core::is_runtime_managed_slot(name.as_str()))
        .chain(dae_model.variables.constants.keys())
        .chain(dae_model.variables.discrete_reals.keys())
        .chain(dae_model.variables.discrete_valued.keys())
    {
        discrete_names.insert(name.to_string());
    }
    if dae_model.events.has_terminal_event {
        discrete_names.insert(rumoca_core::TERMINAL_EVENT_PARAMETER_NAME.to_string());
    }

    loop {
        let mut changed = false;
        for channel in &dae_model.events.delay_channels {
            if !discrete_names.contains(channel.value_parameter.as_str())
                && expression_only_reads_discrete_values(&channel.source, &discrete_names)
            {
                discrete_names.insert(channel.value_parameter.to_string());
                changed = true;
            }
        }
        for equation in &dae_model.continuous.equations {
            let Some(lhs) = equation.lhs.as_ref() else {
                continue;
            };
            let lhs = lhs.var_name().to_string();
            if !discrete_names.contains(&lhs)
                && expression_only_reads_discrete_values(&equation.rhs, &discrete_names)
            {
                discrete_names.insert(lhs);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    for channel in &mut dae_model.events.delay_channels {
        channel.source_is_discrete = discrete_names.contains(channel.value_parameter.as_str());
    }
}

fn expression_only_reads_discrete_values(
    expression: &rumoca_core::Expression,
    discrete_names: &HashSet<String>,
) -> bool {
    struct Checker<'a> {
        discrete_names: &'a HashSet<String>,
        discrete: bool,
    }

    impl ExpressionVisitor for Checker<'_> {
        fn visit_expression(&mut self, expression: &rumoca_core::Expression) {
            if !self.discrete {
                return;
            }
            match expression {
                rumoca_core::Expression::VarRef { name, .. }
                    if name.var_name().as_str() == "time"
                        || !self.discrete_names.contains(name.var_name().as_str()) =>
                {
                    self.discrete = false;
                }
                rumoca_core::Expression::VarRef { .. } => {}
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Der,
                    ..
                } => self.discrete = false,
                _ => self.walk_expression(expression),
            }
        }
    }

    let mut checker = Checker {
        discrete_names,
        discrete: true,
    };
    checker.visit_expression(expression);
    checker.discrete
}

struct TerminalFinder {
    span: Option<rumoca_core::Span>,
}

impl DaeVisitor for TerminalFinder {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        ExpressionVisitor::visit_expression(self, expr);
    }
}

impl ExpressionVisitor for TerminalFinder {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if self.span.is_some() {
            return;
        }
        match expr {
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Terminal,
                span,
                ..
            } => {
                self.span = Some(*span);
                return;
            }
            rumoca_core::Expression::FunctionCall {
                name,
                is_constructor: false,
                span,
                ..
            } if name.var_name().last_segment() == "terminal" => {
                self.span = Some(*span);
                return;
            }
            _ => {}
        }
        self.walk_expression(expr);
    }
}

struct TerminalRewriter;

impl ExpressionRewriter for TerminalRewriter {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        match expr {
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Terminal,
                args,
                span,
            } if args.is_empty() => return terminal_reference(*span),
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor: false,
                span,
            } if name.var_name().last_segment() == "terminal" && args.is_empty() => {
                return terminal_reference(*span);
            }
            _ => {}
        }
        self.walk_expression(expr)
    }
}

impl DaeExpressionRewriter for TerminalRewriter {}

fn terminal_reference(span: rumoca_core::Span) -> rumoca_core::Expression {
    let name = rumoca_core::VarName::new(rumoca_core::TERMINAL_EVENT_PARAMETER_NAME);
    rumoca_core::Expression::VarRef {
        name: name.into(),
        subscripts: Vec::new(),
        span,
    }
}

fn declare_runtime_parameter(
    dae_model: &mut dae::Dae,
    name: rumoca_core::VarName,
    span: rumoca_core::Span,
    description: &str,
    boolean: bool,
    fixed: bool,
) -> Result<(), ToDaeError> {
    if let Some(existing) = dae_model.variables.parameters.get(&name) {
        if existing.origin == dae::VariableOrigin::Generated {
            return Ok(());
        }
        return Err(ToDaeError::runtime_contract_violation_with_span(
            format!(
                "source parameter `{name}` collides with the compiler-reserved runtime namespace"
            ),
            existing.source_span,
        ));
    }
    dae_model.variables.parameters.insert(
        name.clone(),
        dae::Variable {
            name: name.clone(),
            component_ref: rumoca_core::component_reference_from_flat_name(&name, span),
            source_span: span,
            start: Some(rumoca_core::Expression::Literal {
                value: if boolean {
                    rumoca_core::Literal::Boolean(false)
                } else {
                    rumoca_core::Literal::Real(0.0)
                },
                span,
            }),
            start_span: Some(span),
            fixed: Some(fixed),
            description: Some(description.to_string()),
            causality: dae::VariableCausality::CalculatedParameter,
            origin: dae::VariableOrigin::Generated,
            ..dae::Variable::empty_with_span(span)
        },
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{BuiltinFunction, Literal, Reference, Span, VarName};

    fn var(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: Reference::generated(name),
            subscripts: Vec::new(),
            span: Span::DUMMY,
        }
    }

    fn real(value: f64) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: Literal::Real(value),
            span: Span::DUMMY,
        }
    }

    fn terminal() -> rumoca_core::Expression {
        rumoca_core::Expression::BuiltinCall {
            function: BuiltinFunction::Terminal,
            args: Vec::new(),
            span: Span::DUMMY,
        }
    }

    fn delay(source: rumoca_core::Expression, amount: f64) -> rumoca_core::Expression {
        rumoca_core::Expression::BuiltinCall {
            function: BuiltinFunction::Delay,
            args: vec![source, real(amount)],
            span: Span::DUMMY,
        }
    }

    fn insert_variable(variables: &mut indexmap::IndexMap<VarName, dae::Variable>, name: &str) {
        let name = VarName::new(name);
        variables.insert(name.clone(), dae::Variable::new(name, Span::DUMMY));
    }

    #[test]
    fn runtime_terminal_slot_prevents_parameter_variable_promotion() {
        let mut dae_model = dae::Dae::new();
        insert_variable(&mut dae_model.variables.algebraics, "y");
        dae_model.continuous.equations.push(dae::Equation::explicit(
            VarName::new("y"),
            terminal(),
            Span::DUMMY,
            "terminal promotion regression",
        ));

        lower_runtime_operators(&mut dae_model).expect("terminal should lower");
        crate::promote_parameter_variable::promote_parameter_variable_algebraics(&mut dae_model)
            .expect("promotion should complete");

        let y = VarName::new("y");
        assert!(dae_model.variables.algebraics.contains_key(&y));
        assert!(
            !dae_model.variables.parameters.contains_key(&y),
            "runtime-managed slots must not make dependent algebraics parameter-static"
        );
    }

    #[test]
    fn real_input_delay_source_keeps_continuous_interpolation() {
        let mut dae_model = dae::Dae::new();
        insert_variable(&mut dae_model.variables.inputs, "u");
        dae_model.events.delay_channels.push(dae::DaeDelayChannel {
            value_parameter: rumoca_core::delay_slot_name(0),
            source: var("u"),
            delay_time: real(0.1),
            delay_max: None,
            source_is_discrete: true,
            span: Span::DUMMY,
        });

        classify_discrete_delay_sources(&mut dae_model);

        assert!(!dae_model.events.delay_channels[0].source_is_discrete);
    }

    #[test]
    fn nested_delay_inherits_source_variability() {
        let mut dae_model = dae::Dae::new();
        insert_variable(&mut dae_model.variables.states, "x");
        let first = rumoca_core::delay_slot_name(0);
        dae_model.events.delay_channels = vec![
            dae::DaeDelayChannel {
                value_parameter: first.clone(),
                source: var("x"),
                delay_time: real(0.1),
                delay_max: None,
                source_is_discrete: true,
                span: Span::DUMMY,
            },
            dae::DaeDelayChannel {
                value_parameter: rumoca_core::delay_slot_name(1),
                source: var(first.as_str()),
                delay_time: real(0.2),
                delay_max: None,
                source_is_discrete: true,
                span: Span::DUMMY,
            },
        ];

        classify_discrete_delay_sources(&mut dae_model);

        assert!(
            dae_model
                .events
                .delay_channels
                .iter()
                .all(|channel| !channel.source_is_discrete)
        );
    }

    #[test]
    fn delay_of_terminal_uses_piecewise_constant_history() {
        let mut dae_model = dae::Dae::new();
        insert_variable(&mut dae_model.variables.algebraics, "y");
        dae_model.continuous.equations.push(dae::Equation::explicit(
            VarName::new("y"),
            delay(terminal(), 0.1),
            Span::DUMMY,
            "terminal delay",
        ));

        lower_runtime_operators(&mut dae_model).expect("runtime operators should lower");

        assert!(dae_model.events.has_terminal_event);
        assert!(dae_model.events.delay_channels[0].source_is_discrete);
    }

    #[test]
    fn repeated_lowering_appends_channels_from_late_event_actions() {
        let mut dae_model = dae::Dae::new();
        insert_variable(&mut dae_model.variables.states, "x");
        insert_variable(&mut dae_model.variables.states, "z");
        dae_model.continuous.equations.push(dae::Equation::explicit(
            VarName::new("x"),
            delay(var("x"), 0.1),
            Span::DUMMY,
            "first delay",
        ));
        lower_runtime_operators(&mut dae_model).expect("first delay should lower");
        dae_model.events.event_actions.push(dae::DaeEventAction {
            condition: delay(var("z"), 0.2),
            kind: dae::DaeEventActionKind::Assert { message: real(1.0) },
            span: Span::DUMMY,
            origin: "late assertion".to_string(),
        });

        lower_runtime_operators(&mut dae_model).expect("late delay should lower");

        assert_eq!(dae_model.events.delay_channels.len(), 2);
        assert_eq!(
            dae_model.events.delay_channels[0].value_parameter,
            rumoca_core::delay_slot_name(0)
        );
        assert_eq!(
            dae_model.events.delay_channels[1].value_parameter,
            rumoca_core::delay_slot_name(1)
        );
    }

    #[test]
    fn source_parameter_cannot_collide_with_runtime_namespace() {
        let mut dae_model = dae::Dae::new();
        insert_variable(
            &mut dae_model.variables.parameters,
            rumoca_core::TERMINAL_EVENT_PARAMETER_NAME,
        );
        dae_model
            .variables
            .parameters
            .get_mut(&VarName::new(rumoca_core::TERMINAL_EVENT_PARAMETER_NAME))
            .expect("source parameter fixture")
            .origin = dae::VariableOrigin::Source;
        dae_model.events.synthetic_root_conditions.push(terminal());

        let error = lower_runtime_operators(&mut dae_model)
            .expect_err("source-owned runtime slot collision must fail");

        assert!(
            error
                .to_string()
                .contains("compiler-reserved runtime namespace")
        );
    }
}
