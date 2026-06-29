use super::*;

fn bool_lit(value: bool) -> Expression {
    Expression::Literal {
        value: Literal::Boolean(value),
        span: crate::test_support::test_span(),
    }
}

fn string_lit(value: &str) -> Expression {
    Expression::Literal {
        value: Literal::String(value.to_string()),
        span: crate::test_support::test_span(),
    }
}

fn lower_assertion_model(flat: &Model) -> rumoca_ir_dae::Dae {
    to_dae_with_options(
        flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("assertion equation should lower into DAE event metadata")
}

fn event_action_is_assert(action: &rumoca_ir_dae::DaeEventAction) -> bool {
    matches!(
        action.kind,
        rumoca_ir_dae::DaeEventActionKind::Assert { .. }
    )
}

fn expression_is_bool(expr: &Expression, expected: bool) -> bool {
    matches!(
        expr,
        Expression::Literal {
            value: Literal::Boolean(value),
            ..
        } if *value == expected
    )
}

fn expression_contains_initial_call(expr: &Expression) -> bool {
    struct InitialFinder {
        found: bool,
    }

    impl rumoca_core::ExpressionVisitor for InitialFinder {
        fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
            if *function == BuiltinFunction::Initial && args.is_empty() {
                self.found = true;
                return;
            }
            self.walk_builtin_call(function, args);
        }
    }

    let mut finder = InitialFinder { found: false };
    rumoca_core::ExpressionVisitor::visit_expression(&mut finder, expr);
    finder.found
}

#[test]
fn test_todae_lowers_assert_equation_to_event_action() {
    let mut flat = Model::new();
    flat.assert_equations.push(flat::AssertEquation::new(
        bool_lit(false),
        string_lit("assertion failed"),
        None,
        crate::test_support::test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let dae = lower_assertion_model(&flat);

    assert_eq!(dae.events.event_actions.len(), 1);
    let action = &dae.events.event_actions[0];
    assert!(event_action_is_assert(action));
    assert_eq!(action.origin, "assert equation");
    assert!(
        expression_is_bool(&action.condition, true),
        "assert(false, ...) must fire the event action"
    );
    let rumoca_ir_dae::DaeEventActionKind::Assert { message } = &action.kind else {
        unreachable!("assert action kind was checked above");
    };
    assert!(
        matches!(message, Expression::Literal { value: Literal::String(text), .. } if text == "assertion failed")
    );
}

#[test]
fn test_todae_lowers_initial_assert_equation_to_initial_event_action() {
    let mut flat = Model::new();
    flat.initial_assert_equations
        .push(flat::AssertEquation::new(
            bool_lit(false),
            string_lit("initial assertion failed"),
            None,
            crate::test_support::test_span(),
            flat::EquationOrigin::ComponentEquation {
                component: String::new(),
            },
        ));

    let dae = lower_assertion_model(&flat);

    assert_eq!(dae.events.event_actions.len(), 1);
    let action = &dae.events.event_actions[0];
    assert!(event_action_is_assert(action));
    assert_eq!(action.origin, "initial assert equation");
    assert!(
        expression_contains_initial_call(&action.condition),
        "initial assertions must remain gated to initialization"
    );
}
