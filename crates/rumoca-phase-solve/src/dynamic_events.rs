use crate::LowerError;
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
) -> Result<(), LowerError> {
    let mut collector = DynamicTimeEventExprCollector {
        exprs,
        no_event_depth: 0,
        error: None,
    };
    collector.visit_expression(expr);
    match collector.error {
        Some(err) => Err(err),
        None => Ok(()),
    }
}

struct DynamicTimeEventExprCollector<'a> {
    exprs: &'a mut Vec<rumoca_core::Expression>,
    no_event_depth: usize,
    error: Option<LowerError>,
}

impl ExpressionVisitor for DynamicTimeEventExprCollector<'_> {
    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if self.error.is_some() {
            return;
        }
        if matches!(function, rumoca_core::BuiltinFunction::NoEvent) {
            self.no_event_depth += 1;
            self.walk_builtin_call(function, args);
            self.no_event_depth -= 1;
            return;
        }
        if matches!(
            function,
            rumoca_core::BuiltinFunction::Mod | rumoca_core::BuiltinFunction::Rem
        ) && self.no_event_depth == 0
        {
            match next_period_event_expr(args) {
                Ok(Some(next_event)) => self.exprs.push(next_event),
                Ok(None) => {}
                Err(err) => {
                    self.error = Some(err);
                    return;
                }
            }
        }
        self.walk_builtin_call(function, args);
    }

    fn visit_binary(
        &mut self,
        op: &OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
    ) {
        if self.error.is_some() {
            return;
        }
        if matches!(
            op,
            OpBinary::Ge | OpBinary::Gt | OpBinary::Le | OpBinary::Lt
        ) && self.no_event_depth == 0
            && let Some(threshold) = comparison_time_threshold_expr(lhs, rhs)
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

fn next_period_event_expr(
    args: &[rumoca_core::Expression],
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let [time, period] = args else {
        return Ok(None);
    };
    if !expr_is_time_var(time) {
        return Ok(None);
    }
    let Some(span) = period.span().or_else(|| time.span()) else {
        return Err(LowerError::UnspannedContractViolation {
            reason: "dynamic periodic time event is missing source provenance".to_string(),
        });
    };
    let period = abs_expr(period.clone(), span);
    let advance = sub_expr(
        period.clone(),
        mod_expr(time_ref_expr(span), period.clone(), span),
        span,
    );
    Ok(Some(add_expr(
        time_ref_expr(span),
        max_expr(
            advance,
            dynamic_time_event_min_advance_expr(period, span),
            span,
        ),
        span,
    )))
}

fn dynamic_time_event_min_advance_expr(
    period: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    let scale = max_expr(
        period,
        add_expr(
            literal_expr(1.0, span),
            abs_expr(time_ref_expr(span), span),
            span,
        ),
        span,
    );
    mul_expr(scale, literal_expr(1.0e-12, span), span)
}

fn time_ref_expr(span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new("time").into(),
        subscripts: vec![],
        span,
    }
}

fn abs_expr(expr: rumoca_core::Expression, span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Abs,
        args: vec![expr],
        span,
    }
}

fn mod_expr(
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Mod,
        args: vec![lhs, rhs],
        span,
    }
}

fn max_expr(
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Max,
        args: vec![lhs, rhs],
        span,
    }
}

fn literal_expr(value: f64, span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span,
    }
}

fn add_expr(
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn mul_expr(
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn sub_expr(
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
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
) -> Result<Vec<rumoca_core::Expression>, LowerError> {
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
        collect_dynamic_time_event_exprs_from_expr(expr, &mut exprs)?;
    }
    Ok(exprs)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scalar_var(name: &str) -> dae::Variable {
        dae::Variable::new(
            rumoca_core::VarName::new(name),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        )
    }

    fn test_span(start: usize, end: usize) -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(rumoca_core::SourceId(89), start, end)
    }

    fn time_ref() -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("time").into(),
            subscripts: vec![],
            span: test_span(1, 5),
        }
    }

    fn var_ref(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new(name).into(),
            subscripts: vec![],
            span: test_span(10, 10 + name.len()),
        }
    }

    fn spanned_var_ref(name: &str, span: rumoca_core::Span) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new(name).into(),
            subscripts: vec![],
            span,
        }
    }

    fn pre_var(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Pre,
            args: vec![var_ref(name)],
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn builtin_call(
        function: rumoca_core::BuiltinFunction,
        args: Vec<rumoca_core::Expression>,
    ) -> rumoca_core::Expression {
        rumoca_core::Expression::BuiltinCall {
            function,
            args,
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

        assert_eq!(
            collect_dynamic_time_event_exprs(&dae_model)
                .expect("time threshold collection should succeed")
                .len(),
            1
        );
    }

    #[test]
    fn collect_dynamic_time_event_exprs_finds_mod_and_rem_time_discontinuities() {
        for function in [
            rumoca_core::BuiltinFunction::Mod,
            rumoca_core::BuiltinFunction::Rem,
        ] {
            let mut dae_model = dae::Dae::default();
            dae_model
                .discrete
                .valued_updates
                .push(dae::Equation::explicit(
                    rumoca_core::VarName::new("turn"),
                    rumoca_core::Expression::Binary {
                        op: OpBinary::Lt,
                        lhs: Box::new(builtin_call(function, vec![time_ref(), var_ref("period")])),
                        rhs: Box::new(var_ref("width")),
                        span: rumoca_core::Span::DUMMY,
                    },
                    rumoca_core::Span::DUMMY,
                    "periodic turn guard",
                ));

            assert_eq!(
                collect_dynamic_time_event_exprs(&dae_model)
                    .expect("mod/rem dynamic event collection should succeed")
                    .len(),
                1,
                "{}(time, period) should expose its discontinuity as a dynamic time event",
                function.name()
            );
        }
    }

    #[test]
    fn next_period_event_expr_preserves_period_span() {
        let period_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("periodic.mo"),
            18,
            24,
        );

        let expr = next_period_event_expr(&[time_ref(), spanned_var_ref("period", period_span)])
            .expect("periodic next-event generation should succeed")
            .expect("mod(time, period) should generate a next-event expression");

        assert_eq!(expr.span(), Some(period_span));
        let rumoca_core::Expression::Binary { lhs, rhs, span, .. } = expr else {
            panic!("generated next-event expression should be an addition");
        };
        assert_eq!(span, period_span);
        assert_eq!(lhs.span(), Some(period_span));
        assert_eq!(rhs.span(), Some(period_span));
    }

    #[test]
    fn next_period_event_expr_rejects_missing_source_span() {
        let time = rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("time").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        };
        let period = rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("period").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        };

        let err = next_period_event_expr(&[time, period])
            .expect_err("source-free periodic time events must fail fast");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(err.reason().contains("dynamic periodic time event"));
        assert!(err.reason().contains("source provenance"));
    }

    #[test]
    fn collect_dynamic_time_event_exprs_respects_no_event() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .discrete
            .valued_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new("turn"),
                builtin_call(
                    rumoca_core::BuiltinFunction::NoEvent,
                    vec![rumoca_core::Expression::Binary {
                        op: OpBinary::Lt,
                        lhs: Box::new(builtin_call(
                            rumoca_core::BuiltinFunction::Mod,
                            vec![time_ref(), var_ref("period")],
                        )),
                        rhs: Box::new(var_ref("width")),
                        span: rumoca_core::Span::DUMMY,
                    }],
                ),
                rumoca_core::Span::DUMMY,
                "noEvent periodic guard",
            ));

        assert!(
            collect_dynamic_time_event_exprs(&dae_model)
                .expect("noEvent dynamic event collection should succeed")
                .is_empty()
        );
    }

    #[test]
    fn lowered_mod_time_event_row_advances_past_current_time() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new("period"), scalar_var("period"));
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new("width"), scalar_var("width"));
        dae_model
            .discrete
            .valued_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new("turn"),
                rumoca_core::Expression::Binary {
                    op: OpBinary::Lt,
                    lhs: Box::new(builtin_call(
                        rumoca_core::BuiltinFunction::Mod,
                        vec![time_ref(), var_ref("period")],
                    )),
                    rhs: Box::new(var_ref("width")),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Span::DUMMY,
                "periodic turn guard",
            ));

        let exprs = collect_dynamic_time_event_exprs(&dae_model)
            .expect("mod dynamic event collection should succeed");
        let layout =
            crate::layout::build_var_layout(&dae_model).expect("test DAE layout should build");
        let rows = crate::lower::lower_dynamic_time_event_rhs(&dae_model, &layout, &exprs)
            .expect("dynamic modulo event row should lower");
        let block = rumoca_ir_solve::ScalarProgramBlock::with_source_span(rows, test_span(1, 5));
        let mut p = vec![0.0; layout.p_scalars()];
        let Some(rumoca_ir_solve::ScalarSlot::P { index, .. }) = layout.binding("period") else {
            panic!("period should be a parameter slot");
        };
        p[index] = 0.1;
        let current_t = 0.6;
        let mut out = vec![0.0; block.programs.len()];

        rumoca_eval_solve::eval_scalar_program_block(&block, &[], &p, current_t, None, &mut out)
            .expect("dynamic event row should evaluate");

        assert!(
            out[0] > current_t,
            "dynamic modulo event row must advance past current_t; got {} at current_t {current_t}",
            out[0]
        );
    }
}
