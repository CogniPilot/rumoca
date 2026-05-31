use indexmap::IndexSet;
use rumoca_core::ExpressionVisitor;
use rumoca_core::OpBinary;
use rumoca_ir_dae as dae;

fn expr_is_time_var(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "time" && subscripts.is_empty()
    )
}

fn event_time_guard_name(expr: &rumoca_core::Expression) -> Option<String> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(
            name.as_str()
                .strip_prefix("__pre__.")
                .unwrap_or_else(|| name.as_str())
                .to_string(),
        ),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Pre,
            args,
            ..
        } if args.len() == 1 => match &args[0] {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => Some(name.to_string()),
            _ => None,
        },
        _ => None,
    }
}

fn comparison_time_threshold_expr(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
) -> Option<rumoca_core::Expression> {
    if expr_is_time_var(lhs) && !expr_is_time_var(rhs) {
        return Some(rhs.clone());
    }
    if expr_is_time_var(rhs) && !expr_is_time_var(lhs) {
        return Some(lhs.clone());
    }
    None
}

fn comparison_uses_time_and_event_var(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
) -> Option<String> {
    match lhs {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if name.as_str() == "time" && subscripts.is_empty() => event_time_guard_name(rhs),
        _ => None,
    }
}

fn collect_dynamic_time_event_exprs_from_expr(
    expr: &rumoca_core::Expression,
    exprs: &mut Vec<rumoca_core::Expression>,
) {
    let mut collector = DynamicTimeEventExprCollector { exprs };
    collector.visit_expression(expr);
}

struct DynamicTimeEventExprCollector<'a> {
    exprs: &'a mut Vec<rumoca_core::Expression>,
}

impl ExpressionVisitor for DynamicTimeEventExprCollector<'_> {
    fn visit_binary(
        &mut self,
        op: &OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
    ) {
        if matches!(
            op,
            OpBinary::Ge | OpBinary::Gt | OpBinary::Le | OpBinary::Lt
        ) && let Some(threshold) = comparison_time_threshold_expr(lhs, rhs)
        {
            // MLS §3.7.3 / Appendix B: relations involving `time` are time
            // event generating expressions. Solve-IR records the threshold
            // expression directly so solver backends do not inspect DAE.
            self.exprs.push(threshold);
        }
        self.visit_expression(lhs);
        self.visit_expression(rhs);
    }
}

fn collect_dynamic_time_event_names_from_expr(
    expr: &rumoca_core::Expression,
    names: &mut IndexSet<String>,
) {
    let mut collector = DynamicTimeEventNameCollector { names };
    collector.visit_expression(expr);
}

struct DynamicTimeEventNameCollector<'a> {
    names: &'a mut IndexSet<String>,
}

impl ExpressionVisitor for DynamicTimeEventNameCollector<'_> {
    fn visit_binary(
        &mut self,
        op: &OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
    ) {
        if matches!(
            op,
            OpBinary::Ge | OpBinary::Gt | OpBinary::Le | OpBinary::Lt
        ) {
            // MLS Appendix B: time events must be scheduled at the event
            // instant. Flattened forms may guard on a live next-event variable
            // or on pre(nextEvent), so solve-IR records the slot name instead
            // of leaving concrete solvers to inspect DAE.
            if let Some(name) = comparison_uses_time_and_event_var(lhs, rhs) {
                self.names.insert(name);
            }
            if let Some(name) = comparison_uses_time_and_event_var(rhs, lhs) {
                self.names.insert(name);
            }
        }
        self.visit_expression(lhs);
        self.visit_expression(rhs);
    }
}

pub(crate) fn collect_dynamic_time_event_names(dae_model: &dae::Dae) -> Vec<String> {
    let mut names = IndexSet::new();
    for expr in dae_model
        .discrete
        .real_updates
        .iter()
        .chain(dae_model.discrete.valued_updates.iter())
        .chain(dae_model.conditions.equations.iter())
        .map(|eq| &eq.rhs)
    {
        collect_dynamic_time_event_names_from_expr(expr, &mut names);
    }
    names.into_iter().collect()
}

pub(crate) fn collect_dynamic_time_event_exprs(
    dae_model: &dae::Dae,
) -> Vec<rumoca_core::Expression> {
    let mut exprs = Vec::new();
    for expr in dae_model
        .discrete
        .real_updates
        .iter()
        .chain(dae_model.discrete.valued_updates.iter())
        .chain(dae_model.conditions.equations.iter())
        .map(|eq| &eq.rhs)
        .chain(dae_model.events.synthetic_root_conditions.iter())
    {
        collect_dynamic_time_event_exprs_from_expr(expr, &mut exprs);
    }
    exprs
}

#[cfg(test)]
mod tests {
    use super::*;

    fn time_ref() -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("time").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn var_ref(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new(name).into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn pre_var(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Pre,
            args: vec![var_ref(name)],
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn collect_dynamic_time_event_names_finds_pre_time_guards() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .discrete
            .real_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new("nextEvent"),
                rumoca_core::Expression::If {
                    branches: vec![(
                        rumoca_core::Expression::Binary {
                            op: OpBinary::Ge,
                            lhs: Box::new(time_ref()),
                            rhs: Box::new(pre_var("nextEvent")),
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span: rumoca_core::Span::DUMMY,
                        },
                    )],
                    else_branch: Box::new(rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(0.5),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Span::DUMMY,
                "nextEvent guard",
            ));

        assert_eq!(
            collect_dynamic_time_event_names(&dae_model),
            vec!["nextEvent".to_string()]
        );
    }

    #[test]
    fn collect_dynamic_time_event_names_finds_live_time_guards() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .discrete
            .real_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new("gateOut"),
                rumoca_core::Expression::If {
                    branches: vec![(
                        rumoca_core::Expression::Binary {
                            op: OpBinary::Ge,
                            lhs: Box::new(time_ref()),
                            rhs: Box::new(var_ref("t_next")),
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span: rumoca_core::Span::DUMMY,
                        },
                    )],
                    else_branch: Box::new(rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(0.0),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Span::DUMMY,
                "gateOut guard",
            ));

        assert_eq!(
            collect_dynamic_time_event_names(&dae_model),
            vec!["t_next".to_string()]
        );
    }

    #[test]
    fn collect_dynamic_time_event_exprs_finds_direct_time_thresholds() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .discrete
            .valued_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new("pulse"),
                rumoca_core::Expression::Binary {
                    op: OpBinary::Lt,
                    lhs: Box::new(time_ref()),
                    rhs: Box::new(rumoca_core::Expression::Binary {
                        op: OpBinary::Add,
                        lhs: Box::new(var_ref("pulseStart")),
                        rhs: Box::new(var_ref("width")),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Span::DUMMY,
                "pulse output",
            ));

        assert_eq!(collect_dynamic_time_event_exprs(&dae_model).len(), 1);
    }
}
