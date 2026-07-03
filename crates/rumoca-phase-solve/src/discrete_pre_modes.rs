use rumoca_core::ExpressionVisitor;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

pub(crate) fn discrete_pre_mode_for_equation(
    dae_model: &dae::Dae,
    eq: &dae::Equation,
) -> solve::DiscreteEventPreMode {
    if clocked_target_reads_pre(dae_model, eq) {
        return solve::DiscreteEventPreMode::EventEntry;
    }
    // DAE lowering rewrites Modelica pre(..), edge(..), change(..), sample(..),
    // and previous(..) reads to explicit `__pre__.*` slots where needed. Those
    // slots are fixed for one event-iteration pass; rows without such reads can
    // follow the current fixed-point pass. Clocked previous(..) is event-entry
    // history for the whole clock tick and is handled above.
    if expression_contains_event_entry_pre_operator(dae_model, &eq.rhs) {
        solve::DiscreteEventPreMode::Fixed
    } else {
        solve::DiscreteEventPreMode::FollowCurrent
    }
}

fn clocked_target_reads_pre(dae_model: &dae::Dae, eq: &dae::Equation) -> bool {
    let Some(lhs) = eq.lhs.as_ref() else {
        return false;
    };
    target_has_clock_metadata(dae_model, lhs.as_str())
        && (expression_contains_event_entry_pre_operator(dae_model, &eq.rhs)
            || expression_contains_lowered_pre_ref(&eq.rhs))
}

pub(crate) fn target_has_clock_metadata(dae_model: &dae::Dae, name: &str) -> bool {
    dae_model.clocks.intervals.contains_key(name)
        || dae_model.clocks.timings.contains_key(name)
        || dae::component_base_name(name).is_some_and(|base| {
            dae_model.clocks.intervals.contains_key(base.as_str())
                || dae_model.clocks.timings.contains_key(base.as_str())
        })
}

pub(crate) fn expression_contains_lowered_pre_ref(expr: &rumoca_core::Expression) -> bool {
    struct LoweredPreChecker {
        contains: bool,
    }

    impl ExpressionVisitor for LoweredPreChecker {
        fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
            if !self.contains {
                self.walk_expression(expr);
            }
        }

        fn visit_var_ref(
            &mut self,
            name: &rumoca_core::Reference,
            subscripts: &[rumoca_core::Subscript],
        ) {
            if rumoca_core::is_pre_slot(name.as_str()) {
                self.contains = true;
                return;
            }
            for subscript in subscripts {
                self.visit_subscript(subscript);
            }
        }
    }

    let mut checker = LoweredPreChecker { contains: false };
    checker.visit_expression(expr);
    checker.contains
}

pub(crate) fn expression_contains_event_entry_pre_operator(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
) -> bool {
    struct EventEntryPreChecker<'a> {
        dae_model: &'a dae::Dae,
        in_event_condition: bool,
        contains: bool,
    }

    impl ExpressionVisitor for EventEntryPreChecker<'_> {
        fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
            if !self.contains {
                self.walk_expression(expr);
            }
        }

        fn visit_builtin_call(
            &mut self,
            function: &rumoca_core::BuiltinFunction,
            args: &[rumoca_core::Expression],
        ) {
            if matches!(
                function,
                rumoca_core::BuiltinFunction::Edge
                    | rumoca_core::BuiltinFunction::Change
                    | rumoca_core::BuiltinFunction::Delay
                    | rumoca_core::BuiltinFunction::Sample
            ) {
                self.contains = true;
                return;
            }
            for arg in args {
                self.visit_expression(arg);
            }
        }

        fn visit_function_call(
            &mut self,
            name: &rumoca_core::Reference,
            args: &[rumoca_core::Expression],
            _is_constructor: bool,
        ) {
            if name.as_str() == "previous" {
                self.contains = true;
                return;
            }
            if name.as_str() == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME {
                self.contains = true;
                return;
            }
            for arg in args {
                self.visit_expression(arg);
            }
        }

        fn visit_if(
            &mut self,
            branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
            else_branch: &rumoca_core::Expression,
        ) {
            for (condition, value) in branches {
                self.visit_event_condition(condition);
                self.visit_expression(value);
            }
            self.visit_expression(else_branch);
        }

        fn visit_var_ref(
            &mut self,
            name: &rumoca_core::Reference,
            subscripts: &[rumoca_core::Subscript],
        ) {
            if is_pre_condition_memory_ref(self.dae_model, name.var_name()) {
                self.contains = true;
                return;
            }
            if rumoca_core::is_pre_slot(name.as_str()) {
                self.contains = true;
                return;
            }
            for subscript in subscripts {
                self.visit_subscript(subscript);
            }
        }
    }

    impl EventEntryPreChecker<'_> {
        fn visit_event_condition(&mut self, expr: &rumoca_core::Expression) {
            let old = self.in_event_condition;
            self.in_event_condition = true;
            self.visit_expression(expr);
            self.in_event_condition = old;
        }
    }

    let mut checker = EventEntryPreChecker {
        dae_model,
        in_event_condition: false,
        contains: false,
    };
    checker.visit_expression(expr);
    checker.contains
}

fn is_pre_condition_memory_ref(dae_model: &dae::Dae, name: &rumoca_core::VarName) -> bool {
    let Some(stripped) = rumoca_core::pre_slot_base(name.as_str()) else {
        return false;
    };
    crate::condition_memory_base_name(dae_model).is_some_and(|condition_name| {
        dae::component_base_name(stripped).as_deref() == Some(condition_name.as_str())
    })
}
