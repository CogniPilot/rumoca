//! DAE feature discovery used to validate target capability manifests.

use rumoca_core::{BuiltinFunction, Expression, ExpressionVisitor, Subscript};
use rumoca_ir_dae as dae;

pub(super) fn dae_has_external_functions(dae: &dae::Dae) -> bool {
    dae.symbols
        .functions
        .values()
        .any(|function| function.external.is_some())
}

pub(super) fn dae_uses_external_tables(dae: &dae::Dae) -> bool {
    dae_expressions(dae).any(|expr| expression_has_named_call(expr, is_external_table_call))
}

fn is_external_table_call(name: &str) -> bool {
    matches!(
        rumoca_core::top_level_last_segment(name),
        "ExternalCombiTimeTable"
            | "ExternalCombiTable1D"
            | "ExternalCombiTable2D"
            | "getTimeTableTmax"
            | "getTimeTableTmin"
            | "getTimeTableValueNoDer"
            | "getTimeTableValueNoDer2"
            | "getTimeTableValue"
            | "getTable1DAbscissaUmax"
            | "getTable1DAbscissaUmin"
            | "getTable1DValueNoDer"
            | "getTable1DValueNoDer2"
            | "getTable1DValue"
            | "getNextTimeEvent"
            | "isValidTable"
    )
}

pub(super) fn dae_uses_random(dae: &dae::Dae) -> bool {
    dae_expressions(dae).any(|expr| expression_has_named_call(expr, is_random_call))
}

fn is_random_call(name: &str) -> bool {
    let short = rumoca_core::top_level_last_segment(name);
    short.contains("Xorshift")
        || matches!(
            short,
            "initialState"
                | "random"
                | "impureRandom"
                | "impureRandomInteger"
                | "initializeImpureRandom"
        )
}

pub(super) fn dae_has_initialization(dae: &dae::Dae) -> bool {
    !dae.initialization.equations.is_empty() || !dae.initialization.structured_equations.is_empty()
}

pub(super) fn dae_has_events(dae: &dae::Dae) -> bool {
    !dae.conditions.equations.is_empty()
        || !dae.conditions.relations.is_empty()
        || !dae.events.synthetic_root_conditions.is_empty()
        || !dae.events.scheduled_time_events.is_empty()
        || !dae.events.event_actions.is_empty()
        || !dae.discrete.real_updates.is_empty()
        || !dae.discrete.valued_updates.is_empty()
        || dae_expressions(dae).any(expression_uses_event_semantics)
}

pub(super) fn dae_has_runtime_events(dae: &dae::Dae) -> bool {
    dae.events.has_terminal_event || !dae.events.delay_channels.is_empty()
}

pub(super) fn dae_has_clocks(dae: &dae::Dae) -> bool {
    !dae.clocks.constructor_exprs.is_empty()
        || !dae.clocks.schedules.is_empty()
        || !dae.clocks.triggered_conditions.is_empty()
        || !dae.clocks.intervals.is_empty()
        || !dae.clocks.timings.is_empty()
        || dae_expressions(dae).any(expression_uses_clock_semantics)
}

pub(super) fn dae_has_unlowered_source_temporal_operators(dae: &dae::Dae) -> bool {
    dae_expressions(dae).any(expression_uses_source_temporal_semantics)
}

pub(super) fn dae_has_dynamic_ranges(dae: &dae::Dae) -> bool {
    dae_expressions(dae).any(expression_has_dynamic_range)
}

pub(super) fn dae_has_dynamic_derivative_subscripts(dae: &dae::Dae) -> bool {
    dae_expressions(dae).any(expression_has_dynamic_derivative_subscripts)
}

fn dae_expressions(dae: &dae::Dae) -> impl Iterator<Item = &Expression> {
    dae.continuous
        .equations
        .iter()
        .map(|equation| &equation.rhs)
        .chain(
            dae.initialization
                .equations
                .iter()
                .map(|equation| &equation.rhs),
        )
        .chain(
            dae.continuous
                .structured_equations
                .iter()
                .flat_map(|family| family.template.iter())
                .flat_map(|template| template.body.iter()),
        )
        .chain(
            dae.initialization
                .structured_equations
                .iter()
                .flat_map(|family| family.template.iter())
                .flat_map(|template| template.body.iter()),
        )
        .chain(
            dae.discrete
                .real_updates
                .iter()
                .map(|equation| &equation.rhs),
        )
        .chain(
            dae.discrete
                .valued_updates
                .iter()
                .map(|equation| &equation.rhs),
        )
        .chain(
            dae.conditions
                .equations
                .iter()
                .map(|equation| &equation.rhs),
        )
        .chain(dae.conditions.relations.iter())
        .chain(dae.events.synthetic_root_conditions.iter())
        .chain(dae.events.event_actions.iter().flat_map(|action| {
            let message = match &action.kind {
                dae::DaeEventActionKind::Assert { message }
                | dae::DaeEventActionKind::Terminate { message } => message,
            };
            [&action.condition, message]
        }))
        .chain(dae.events.delay_channels.iter().flat_map(|channel| {
            std::iter::once(&channel.source)
                .chain(std::iter::once(&channel.delay_time))
                .chain(channel.delay_max.iter())
        }))
        .chain(dae.clocks.constructor_exprs.iter())
        .chain(dae.clocks.triggered_conditions.iter())
        .chain(dae.metadata.variable_starts.values())
}

fn expression_has_named_call(expr: &Expression, predicate: fn(&str) -> bool) -> bool {
    struct Checker {
        predicate: fn(&str) -> bool,
        found: bool,
    }

    impl ExpressionVisitor for Checker {
        fn visit_expression(&mut self, expr: &Expression) {
            if !self.found {
                self.walk_expression(expr);
            }
        }

        fn visit_function_call(
            &mut self,
            name: &rumoca_core::Reference,
            args: &[Expression],
            _: bool,
        ) {
            if (self.predicate)(name.as_str()) {
                self.found = true;
                return;
            }
            for arg in args {
                self.visit_expression(arg);
            }
        }
    }

    let mut checker = Checker {
        predicate,
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

fn expression_uses_event_semantics(expr: &Expression) -> bool {
    struct Checker {
        found: bool,
    }

    impl ExpressionVisitor for Checker {
        fn visit_expression(&mut self, expr: &Expression) {
            if !self.found {
                self.walk_expression(expr);
            }
        }

        fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
            if matches!(
                function,
                BuiltinFunction::Pre
                    | BuiltinFunction::Edge
                    | BuiltinFunction::Change
                    | BuiltinFunction::Reinit
            ) {
                self.found = true;
                return;
            }
            for arg in args {
                self.visit_expression(arg);
            }
        }

        fn visit_function_call(
            &mut self,
            name: &rumoca_core::Reference,
            args: &[Expression],
            _: bool,
        ) {
            if matches!(
                rumoca_core::source_temporal_function_name(name.last_segment()),
                Some("pre" | "edge" | "change" | "reinit")
            ) {
                self.found = true;
                return;
            }
            for arg in args {
                self.visit_expression(arg);
            }
        }
    }

    let mut checker = Checker { found: false };
    checker.visit_expression(expr);
    checker.found
}

fn expression_uses_clock_semantics(expr: &Expression) -> bool {
    struct Checker {
        found: bool,
    }

    impl ExpressionVisitor for Checker {
        fn visit_expression(&mut self, expr: &Expression) {
            if !self.found {
                self.walk_expression(expr);
            }
        }

        fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
            if *function == BuiltinFunction::Sample {
                self.found = true;
                return;
            }
            for arg in args {
                self.visit_expression(arg);
            }
        }

        fn visit_function_call(
            &mut self,
            name: &rumoca_core::Reference,
            args: &[Expression],
            _: bool,
        ) {
            if name.as_str() == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME
                || rumoca_core::source_dae_forbidden_function_name(name.last_segment()).is_some()
            {
                self.found = true;
                return;
            }
            for arg in args {
                self.visit_expression(arg);
            }
        }
    }

    let mut checker = Checker { found: false };
    checker.visit_expression(expr);
    checker.found
}

fn expression_uses_source_temporal_semantics(expr: &Expression) -> bool {
    struct Checker {
        found: bool,
    }

    impl ExpressionVisitor for Checker {
        fn visit_expression(&mut self, expr: &Expression) {
            if !self.found {
                self.walk_expression(expr);
            }
        }

        fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
            if rumoca_core::source_temporal_builtin_name(*function).is_some() {
                self.found = true;
                return;
            }
            for arg in args {
                self.visit_expression(arg);
            }
        }

        fn visit_function_call(
            &mut self,
            name: &rumoca_core::Reference,
            args: &[Expression],
            _: bool,
        ) {
            if name.as_str() == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME
                || rumoca_core::source_dae_forbidden_function_name(name.last_segment()).is_some()
            {
                self.found = true;
                return;
            }
            for arg in args {
                self.visit_expression(arg);
            }
        }
    }

    let mut checker = Checker { found: false };
    checker.visit_expression(expr);
    checker.found
}

fn expression_has_dynamic_range(expr: &Expression) -> bool {
    struct Checker {
        found: bool,
    }

    impl ExpressionVisitor for Checker {
        fn visit_expression(&mut self, expr: &Expression) {
            if !self.found {
                self.walk_expression(expr);
            }
        }

        fn visit_range(&mut self, start: &Expression, step: Option<&Expression>, end: &Expression) {
            if !is_integer_literal(start)
                || step.is_some_and(|step| !is_integer_literal(step))
                || !is_integer_literal(end)
            {
                self.found = true;
                return;
            }
            self.visit_expression(start);
            if let Some(step) = step {
                self.visit_expression(step);
            }
            self.visit_expression(end);
        }
    }

    let mut checker = Checker { found: false };
    checker.visit_expression(expr);
    checker.found
}

fn expression_has_dynamic_derivative_subscripts(expr: &Expression) -> bool {
    struct Checker {
        found: bool,
    }

    impl ExpressionVisitor for Checker {
        fn visit_expression(&mut self, expr: &Expression) {
            if !self.found {
                self.walk_expression(expr);
            }
        }

        fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
            if *function == BuiltinFunction::Der
                && args.iter().any(expression_target_has_dynamic_subscript)
            {
                self.found = true;
                return;
            }
            for arg in args {
                self.visit_expression(arg);
            }
        }
    }

    let mut checker = Checker { found: false };
    checker.visit_expression(expr);
    checker.found
}

fn expression_target_has_dynamic_subscript(expr: &Expression) -> bool {
    match expr {
        Expression::VarRef { subscripts, .. } | Expression::Index { subscripts, .. } => {
            subscripts.iter().any(|subscript| match subscript {
                Subscript::Expr { expr, .. } => !is_integer_literal(expr),
                Subscript::Colon { .. } => true,
                Subscript::Index { .. } => false,
            })
        }
        Expression::FieldAccess { base, .. } => expression_target_has_dynamic_subscript(base),
        _ => false,
    }
}

fn is_integer_literal(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::Literal {
            value: rumoca_core::Literal::Integer(_),
            ..
        }
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{Literal, Reference, Span, VarName};

    fn literal(value: Literal) -> Expression {
        Expression::Literal {
            value,
            span: Span::DUMMY,
        }
    }

    fn var(name: &str) -> Expression {
        Expression::VarRef {
            name: Reference::new(name),
            subscripts: Vec::new(),
            span: Span::DUMMY,
        }
    }

    fn call(name: &str) -> Expression {
        Expression::FunctionCall {
            name: Reference::new(name),
            args: Vec::new(),
            is_constructor: false,
            span: Span::DUMMY,
        }
    }

    #[test]
    fn event_actions_are_events_and_their_expressions_are_analyzed() {
        let mut model = dae::Dae::new();
        model.events.event_actions.push(dae::DaeEventAction {
            condition: literal(Literal::Boolean(true)),
            kind: dae::DaeEventActionKind::Assert {
                message: call("Modelica.Math.Random.Utilities.impureRandom"),
            },
            span: Span::DUMMY,
            origin: "test assert".to_owned(),
        });

        assert!(dae_has_events(&model));
        assert!(dae_uses_random(&model));
    }

    #[test]
    fn delay_channels_are_runtime_events_and_their_expressions_are_analyzed() {
        let mut model = dae::Dae::new();
        model.events.delay_channels.push(dae::DaeDelayChannel {
            value_parameter: VarName::new("__runtime__.delay.0"),
            source: call("Modelica.Math.Random.Utilities.impureRandom"),
            delay_time: Expression::Range {
                start: Box::new(literal(Literal::Integer(1))),
                step: None,
                end: Box::new(var("n")),
                span: Span::DUMMY,
            },
            delay_max: None,
            source_is_discrete: false,
            span: Span::DUMMY,
        });

        assert!(dae_has_runtime_events(&model));
        assert!(dae_uses_random(&model));
        assert!(dae_has_dynamic_ranges(&model));
    }
}
