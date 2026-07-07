use super::*;

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("algorithm_lowering_test.mo"),
        1,
        2,
    )
}

fn make_comp_ref(name: &str) -> rumoca_core::ComponentReference {
    let span = test_span();
    rumoca_core::ComponentReference {
        local: false,
        span,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span,
            subs: vec![],
        }],
        def_id: None,
    }
}

fn make_comp_ref_from_flat_name(name: &str) -> rumoca_core::ComponentReference {
    rumoca_core::component_reference_from_flat_name(&VarName::new(name), test_span())
        .expect("test component reference should parse")
}

fn make_subscripted_comp_ref(
    name: &str,
    index_expr: rumoca_core::Expression,
) -> rumoca_core::ComponentReference {
    let span = test_span();
    rumoca_core::ComponentReference {
        local: false,
        span,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span,
            subs: vec![rumoca_core::Subscript::generated_expr(
                Box::new(index_expr),
                span,
            )],
        }],
        def_id: None,
    }
}

fn make_multi_subscripted_comp_ref(
    name: &str,
    subs: Vec<rumoca_core::Subscript>,
) -> rumoca_core::ComponentReference {
    let span = test_span();
    rumoca_core::ComponentReference {
        local: false,
        span,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span,
            subs,
        }],
        def_id: None,
    }
}

fn make_var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: VarName::new(name).into(),
        subscripts: vec![],
        span: test_span(),
    }
}

fn expr_has_var_ref_indices(expr: &rumoca_core::Expression, name: &str, indices: &[i64]) -> bool {
    match expr {
        rumoca_core::Expression::VarRef {
            name: var_name,
            subscripts,
            ..
        } => {
            var_name.as_str() == name
                && subscripts
                    .iter()
                    .map(|sub| match sub {
                        rumoca_core::Subscript::Index { value, .. } => Some(*value),
                        _ => None,
                    })
                    .collect::<Option<Vec<_>>>()
                    .is_some_and(|actual| actual == indices)
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            expr_has_var_ref_indices(lhs, name, indices)
                || expr_has_var_ref_indices(rhs, name, indices)
        }
        rumoca_core::Expression::Unary { rhs, .. } => expr_has_var_ref_indices(rhs, name, indices),
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. } => args
            .iter()
            .any(|arg| expr_has_var_ref_indices(arg, name, indices)),
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => elements
            .iter()
            .any(|element| expr_has_var_ref_indices(element, name, indices)),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(cond, value)| {
                expr_has_var_ref_indices(cond, name, indices)
                    || expr_has_var_ref_indices(value, name, indices)
            }) || expr_has_var_ref_indices(else_branch, name, indices)
        }
        rumoca_core::Expression::Index { base, .. }
        | rumoca_core::Expression::FieldAccess { base, .. } => {
            expr_has_var_ref_indices(base, name, indices)
        }
        _ => false,
    }
}

fn expr_has_unsubscripted_var_ref(expr: &rumoca_core::Expression, name: &str) -> bool {
    match expr {
        rumoca_core::Expression::VarRef {
            name: var_name,
            subscripts,
            ..
        } => var_name.as_str() == name && subscripts.is_empty(),
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            expr_has_unsubscripted_var_ref(lhs, name) || expr_has_unsubscripted_var_ref(rhs, name)
        }
        rumoca_core::Expression::Unary { rhs, .. } => expr_has_unsubscripted_var_ref(rhs, name),
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. } => args
            .iter()
            .any(|arg| expr_has_unsubscripted_var_ref(arg, name)),
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => elements
            .iter()
            .any(|element| expr_has_unsubscripted_var_ref(element, name)),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(cond, value)| {
                expr_has_unsubscripted_var_ref(cond, name)
                    || expr_has_unsubscripted_var_ref(value, name)
            }) || expr_has_unsubscripted_var_ref(else_branch, name)
        }
        rumoca_core::Expression::Index { base, .. }
        | rumoca_core::Expression::FieldAccess { base, .. } => {
            expr_has_unsubscripted_var_ref(base, name)
        }
        _ => false,
    }
}

fn add_primitive_real(flat: &mut Model, name: &str) {
    flat.add_variable(
        VarName::new(name),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new(name),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
}

fn add_fixed_false_parameter(flat: &mut Model, name: &str) {
    add_parameter_with_fixed(flat, name, Some(false));
}

fn add_parameter_with_fixed(flat: &mut Model, name: &str, fixed: Option<bool>) {
    flat.add_variable(
        VarName::new(name),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new(name),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            fixed,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
}

fn add_discrete_valued(flat: &mut Model, name: &str) {
    flat.add_variable(
        VarName::new(name),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new(name),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
}

fn add_discrete_real(flat: &mut Model, name: &str) {
    flat.add_variable(
        VarName::new(name),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new(name),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
}

fn add_tick_z_y_discrete_fixture(flat: &mut Model) {
    add_discrete_valued(flat, "tick");
    flat.add_variable(
        VarName::new("z"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("z"),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
    flat.add_variable(
        VarName::new("y"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("y"),
            dims: vec![2],
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
}

fn two_iteration_for_index() -> rumoca_core::ForIndex {
    rumoca_core::ForIndex {
        ident: "i".to_string(),
        range: rumoca_core::Expression::Range {
            start: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: test_span(),
            }),
            step: None,
            end: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                span: test_span(),
            }),
            span: test_span(),
        },
    }
}

fn z_increment_assignment() -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: make_comp_ref("z"),
        value: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(make_var_ref("z")),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: test_span(),
            }),
            span: test_span(),
        },
        span: test_span(),
    }
}

fn y_loop_assignment_then_z_increment() -> Vec<rumoca_core::Statement> {
    vec![
        rumoca_core::Statement::Assignment {
            comp: make_subscripted_comp_ref("y", make_var_ref("i")),
            value: make_var_ref("z"),
            span: test_span(),
        },
        z_increment_assignment(),
    ]
}

fn add_scalar_ode_with_rhs_call(flat: &mut Model, state_name: &str, call_name: &str) {
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref(state_name)],
                span: test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::FunctionCall {
                name: VarName::new(call_name).into(),
                args: vec![make_var_ref(state_name)],
                is_constructor: false,
                span: test_span(),
            }),
            span: test_span(),
        },
        span: test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });
}

// SPEC_0021: Exception - cohesive test fixture builder for algorithm lowering regressions.
#[allow(clippy::too_many_lines)]
fn build_top_level_assignment_before_loop_model() -> Model {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("y"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("y"),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
    flat.add_variable(
        VarName::new("y0"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("y0"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
    flat.add_variable(
        VarName::new("x"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("x"),
            dims: vec![2],
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
    flat.add_variable(
        VarName::new("t"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("t"),
            dims: vec![2],
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
    flat.algorithms.push(flat::Algorithm::new(
        vec![
            rumoca_core::Statement::Assignment {
                comp: make_comp_ref("y"),
                value: make_var_ref("y0"),

                span: test_span(),
            },
            rumoca_core::Statement::For {
                indices: vec![rumoca_core::ForIndex {
                    ident: "i".to_string(),
                    range: rumoca_core::Expression::Range {
                        start: Box::new(rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Integer(1),
                            span: test_span(),
                        }),
                        step: None,
                        end: Box::new(rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Integer(2),
                            span: test_span(),
                        }),
                        span: test_span(),
                    },
                }],
                equations: vec![rumoca_core::Statement::If {
                    cond_blocks: vec![rumoca_core::StatementBlock {
                        cond: rumoca_core::Expression::Binary {
                            op: rumoca_core::OpBinary::Ge,
                            lhs: Box::new(make_var_ref("time")),
                            rhs: Box::new(rumoca_core::Expression::Index {
                                base: Box::new(make_var_ref("t")),
                                subscripts: vec![rumoca_core::Subscript::generated_expr(
                                    Box::new(make_var_ref("i")),
                                    test_span(),
                                )],
                                span: test_span(),
                            }),
                            span: test_span(),
                        },
                        stmts: vec![rumoca_core::Statement::Assignment {
                            comp: make_comp_ref("y"),
                            value: rumoca_core::Expression::Index {
                                base: Box::new(make_var_ref("x")),
                                subscripts: vec![rumoca_core::Subscript::generated_expr(
                                    Box::new(make_var_ref("i")),
                                    test_span(),
                                )],
                                span: test_span(),
                            },
                            span: test_span(),
                        }],
                    }],
                    else_block: None,
                    span: test_span(),
                }],

                span: test_span(),
            },
        ],
        test_span(),
        "top-level assignment before for loop".to_string(),
    ));
    flat
}

fn assert_top_level_assignment_before_loop_lowering(dae: &Dae) {
    let rhs = dae
        .discrete
        .valued_updates
        .iter()
        .find(|eq| eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == "y"))
        .map(|eq| format!("{:?}", eq.rhs))
        .expect("missing lowered y discrete-valued assignment");

    assert!(
        rhs.contains("VarName(\"y0\")"),
        "loop fallback should preserve the preceding y := y0 assignment, got {rhs}"
    );
    assert!(
        !rhs.contains("VarName(\"__pre__.y\")"),
        "top-level sequential algorithm lowering must not fall back to pre(y), got {rhs}"
    );
}

fn collect_edge_guard_names(expr: &rumoca_core::Expression, names: &mut Vec<String>) {
    match expr {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Or,
            lhs,
            rhs,
            ..
        } => {
            collect_edge_guard_names(lhs, names);
            collect_edge_guard_names(rhs, names);
        }
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Edge,
            args,
            ..
        } => {
            let [
                rumoca_core::Expression::VarRef {
                    name, subscripts, ..
                },
            ] = args.as_slice()
            else {
                return;
            };
            if subscripts.is_empty() {
                names.push(name.as_str().to_string());
            }
        }
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::And,
            lhs,
            rhs,
            ..
        } => {
            if let (
                rumoca_core::Expression::VarRef {
                    name, subscripts, ..
                },
                rumoca_core::Expression::Unary {
                    op: rumoca_core::OpUnary::Not,
                    rhs,
                    ..
                },
            ) = (&**lhs, &**rhs)
                && let rumoca_core::Expression::VarRef {
                    name: pre_name,
                    subscripts: pre_subscripts,
                    ..
                } = &**rhs
                && subscripts.is_empty()
                && pre_subscripts.is_empty()
                && pre_name.as_str() == format!("__pre__.{}", name.as_str())
            {
                names.push(name.as_str().to_string());
            }
        }
        _ => {}
    }
}

#[derive(Default)]
struct InitialCallFinder {
    found: bool,
}

impl rumoca_core::ExpressionVisitor for InitialCallFinder {
    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if *function == rumoca_core::BuiltinFunction::Initial && args.is_empty() {
            self.found = true;
            return;
        }
        self.walk_builtin_call(function, args);
    }
}

fn contains_initial_call(expr: &rumoca_core::Expression) -> bool {
    let mut finder = InitialCallFinder::default();
    rumoca_core::ExpressionVisitor::visit_expression(&mut finder, expr);
    finder.found
}

fn build_supported_algorithm_slice_row_model() -> Model {
    let mut flat = Model::new();
    let span = test_span();
    flat.add_variable(
        VarName::new("int_addr"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("int_addr"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                span,
            }),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
    flat.add_variable(
        VarName::new("n_data"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("n_data"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(3),
                span,
            }),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
    flat.add_variable(
        VarName::new("mem"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("mem"),
            dims: vec![2, 3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
    flat.add_variable(
        VarName::new("mem_word"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("mem_word"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
    flat.add_variable(
        VarName::new("nextstate"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("nextstate"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );

    let generated_subscript = |expr| rumoca_core::Subscript::generated_expr(Box::new(expr), span);
    let full_row = generated_subscript(rumoca_core::Expression::Range {
        start: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            span,
        }),
        step: None,
        end: Box::new(make_var_ref("n_data")),
        span,
    });

    flat.algorithms.push(flat::Algorithm::new(
        vec![
            rumoca_core::Statement::Assignment {
                comp: make_multi_subscripted_comp_ref(
                    "mem",
                    vec![
                        generated_subscript(make_var_ref("int_addr")),
                        full_row.clone(),
                    ],
                ),
                value: make_var_ref("mem_word"),

                span,
            },
            rumoca_core::Statement::Assignment {
                comp: make_comp_ref("nextstate"),
                value: rumoca_core::Expression::Index {
                    base: Box::new(make_var_ref("mem")),
                    subscripts: vec![generated_subscript(make_var_ref("int_addr")), full_row],
                    span,
                },

                span,
            },
        ],
        span,
        "algorithm slice row".to_string(),
    ));

    flat
}

fn assert_supported_algorithm_slice_row_lowering(dae: &Dae) {
    let lowered_rhs = dae
        .continuous
        .equations
        .iter()
        .map(|eq| &eq.rhs)
        .collect::<Vec<_>>();
    assert!(
        lowered_rhs.iter().any(|rhs| {
            expr_has_var_ref_indices(rhs, "mem", &[1, 1])
                && expr_has_var_ref_indices(rhs, "mem_word", &[1])
        }) && lowered_rhs.iter().any(|rhs| {
            expr_has_var_ref_indices(rhs, "mem", &[2, 2])
                && expr_has_var_ref_indices(rhs, "mem_word", &[2])
        }) && lowered_rhs.iter().any(|rhs| {
            expr_has_var_ref_indices(rhs, "mem", &[1, 3])
                && expr_has_var_ref_indices(rhs, "mem_word", &[3])
        }) && lowered_rhs
            .iter()
            .all(|rhs| !expr_has_unsubscripted_var_ref(rhs, "mem")),
        "expected scalarized row write residuals, got {lowered_rhs:?}"
    );

    let nextstate_rhs = dae
        .continuous
        .equations
        .iter()
        .map(|eq| &eq.rhs)
        .find(|rhs| expr_has_unsubscripted_var_ref(rhs, "nextstate"))
        .expect("missing lowered nextstate equation");
    assert!(
        expr_has_var_ref_indices(nextstate_rhs, "mem_word", &[1])
            && expr_has_var_ref_indices(nextstate_rhs, "mem_word", &[2])
            && expr_has_var_ref_indices(nextstate_rhs, "mem_word", &[3]),
        "expected sequential slice read to observe preceding mem_word write, got {nextstate_rhs:?}"
    );
}

#[test]
fn test_todae_preserves_function_algorithm_bodies_for_codegen_readability() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    let mut fn_def = rumoca_core::Function::new("f", test_span());
    fn_def.add_input(rumoca_core::FunctionParam::new(
        "u",
        "Real",
        crate::test_support::test_span(),
    ));
    fn_def.add_output(rumoca_core::FunctionParam::new(
        "y",
        "Real",
        crate::test_support::test_span(),
    ));
    fn_def.body.push(rumoca_core::Statement::Assignment {
        comp: make_comp_ref("y"),
        value: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(make_var_ref("u")),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span: test_span(),
            }),
            span: test_span(),
        },

        span: test_span(),
    });
    flat.add_function(fn_def);

    add_scalar_ode_with_rhs_call(&mut flat, "x", "f");

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should keep reachable function bodies");

    let lowered_fn = dae
        .symbols
        .functions
        .get(&rumoca_core::VarName::new("f"))
        .expect("function f should be preserved in DAE");
    assert_eq!(
        lowered_fn.body.len(),
        1,
        "function algorithm body must be preserved for downstream codegen readability"
    );
}

#[test]
fn test_todae_lowers_supported_model_algorithms_to_equations() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");
    add_primitive_real(&mut flat, "y");

    flat.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::Assignment {
            comp: make_comp_ref("y"),
            value: make_var_ref("x"),

            span: test_span(),
        }],
        test_span(),
        "model algorithm".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should lower simple model algorithms");

    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| eq.origin.contains("algorithm assignment")),
        "lowered model algorithm assignment should appear in f_x"
    );
}

#[test]
fn test_todae_routes_discrete_valued_algorithm_assignment_to_f_m() {
    let mut flat = Model::new();
    add_discrete_valued(&mut flat, "x");
    add_discrete_valued(&mut flat, "y");

    flat.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::Assignment {
            comp: make_comp_ref("y"),
            value: make_var_ref("x"),

            span: test_span(),
        }],
        test_span(),
        "discrete model algorithm".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("discrete-valued model algorithm assignments should lower");

    assert!(
        dae.discrete
            .valued_updates
            .iter()
            .any(|eq| eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == "y")),
        "MLS Appendix B discrete-valued algorithm assignment should appear in f_m"
    );
    assert!(
        !dae.continuous
            .equations
            .iter()
            .any(|eq| format!("{:?}", eq.rhs).contains("VarName(\"y\")")),
        "discrete-valued algorithm assignment should not remain in continuous f_x"
    );
}

#[test]
fn test_todae_lowers_model_algorithm_for_loop_with_static_range() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");
    add_primitive_real(&mut flat, "y");

    flat.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::For {
            indices: vec![rumoca_core::ForIndex {
                ident: "i".to_string(),
                range: rumoca_core::Expression::Range {
                    start: Box::new(rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(1),
                        span: test_span(),
                    }),
                    step: None,
                    end: Box::new(rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(3),
                        span: test_span(),
                    }),
                    span: test_span(),
                },
            }],
            equations: vec![rumoca_core::Statement::Assignment {
                comp: make_comp_ref("y"),
                value: make_var_ref("i"),
                span: test_span(),
            }],

            span: test_span(),
        }],
        test_span(),
        "model for".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("for-loop algorithms with static ranges should lower to equations");

    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| eq.origin.contains("algorithm for-assignment")),
        "expected lowered for-loop assignment equations in f_x"
    );
}

#[test]
fn test_todae_lowers_model_algorithm_for_loop_with_subscripted_targets() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("y"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("y"),
            dims: vec![2],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );

    flat.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::For {
            indices: vec![rumoca_core::ForIndex {
                ident: "i".to_string(),
                range: rumoca_core::Expression::Range {
                    start: Box::new(rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(1),
                        span: test_span(),
                    }),
                    step: None,
                    end: Box::new(rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(2),
                        span: test_span(),
                    }),
                    span: test_span(),
                },
            }],
            equations: vec![rumoca_core::Statement::Assignment {
                comp: make_subscripted_comp_ref("y", make_var_ref("i")),
                value: make_var_ref("i"),
                span: test_span(),
            }],

            span: test_span(),
        }],
        test_span(),
        "model for indexed target".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("for-loop algorithms with static indexed targets should lower to equations");

    let lowered_rhs = dae
        .continuous
        .equations
        .iter()
        .map(|eq| &eq.rhs)
        .collect::<Vec<_>>();
    assert!(
        lowered_rhs
            .iter()
            .any(|rhs| expr_has_var_ref_indices(rhs, "y", &[1])),
        "expected lowered residual for indexed target y[1], got {lowered_rhs:?}"
    );
    assert!(
        lowered_rhs
            .iter()
            .any(|rhs| expr_has_var_ref_indices(rhs, "y", &[2])),
        "expected lowered residual for indexed target y[2], got {lowered_rhs:?}"
    );
    assert!(
        !lowered_rhs
            .iter()
            .any(|rhs| expr_has_unsubscripted_var_ref(rhs, "y")),
        "indexed algorithm targets must not collapse to the whole array name"
    );
}

#[test]
fn test_todae_lowers_model_algorithm_dynamic_scalar_index_assignment() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "u");
    flat.add_variable(
        VarName::new("iTick"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("iTick"),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
    flat.add_variable(
        VarName::new("buf"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("buf"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );

    flat.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::Assignment {
            comp: make_subscripted_comp_ref("buf", make_var_ref("iTick")),
            value: make_var_ref("u"),
            span: test_span(),
        }],
        test_span(),
        "dynamic scalar index assignment".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("dynamic scalar index algorithm assignments should lower");

    let lowered_rhs = dae
        .continuous
        .equations
        .iter()
        .map(|eq| &eq.rhs)
        .collect::<Vec<_>>();
    for index in 1..=3 {
        assert!(
            lowered_rhs.iter().any(|rhs| {
                expr_has_var_ref_indices(rhs, "buf", &[index])
                    && expr_has_var_ref_indices(rhs, "c", &[index])
                    && expr_has_unsubscripted_var_ref(rhs, "u")
            }),
            "missing guarded assignment for buf[{index}], got {lowered_rhs:?}"
        );
    }
    assert!(
        !lowered_rhs
            .iter()
            .any(|rhs| expr_has_unsubscripted_var_ref(rhs, "buf")),
        "dynamic scalar assignment must lower to scalar targets, got {lowered_rhs:?}"
    );
}

#[test]
fn test_todae_lowers_when_algorithm_for_loop_with_subscripted_targets() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("tick"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("tick"),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
    flat.add_variable(
        VarName::new("y"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("y"),
            dims: vec![2],
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );

    flat.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: make_var_ref("tick"),
                stmts: vec![rumoca_core::Statement::For {
                    indices: vec![rumoca_core::ForIndex {
                        ident: "i".to_string(),
                        range: rumoca_core::Expression::Range {
                            start: Box::new(rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Integer(1),
                                span: test_span(),
                            }),
                            step: None,
                            end: Box::new(rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Integer(2),
                                span: test_span(),
                            }),
                            span: test_span(),
                        },
                    }],
                    equations: vec![rumoca_core::Statement::Assignment {
                        comp: make_subscripted_comp_ref("y", make_var_ref("i")),
                        value: make_var_ref("tick"),
                        span: test_span(),
                    }],
                    span: test_span(),
                }],
            }],
            span: test_span(),
        }],
        test_span(),
        "when for indexed target".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("when for-loop algorithms with static indexed targets should lower to event equations");

    let lowered_lhs: Vec<_> = dae
        .discrete
        .real_updates
        .iter()
        .chain(dae.discrete.valued_updates.iter())
        .filter_map(|eq| eq.lhs.as_ref().map(|lhs| lhs.as_str().to_string()))
        .collect();
    assert!(
        lowered_lhs.iter().any(|lhs| lhs == "y[1]"),
        "expected lowered indexed event target y[1], got {lowered_lhs:?}"
    );
    assert!(
        lowered_lhs.iter().any(|lhs| lhs == "y[2]"),
        "expected lowered indexed event target y[2], got {lowered_lhs:?}"
    );
    assert!(
        !lowered_lhs.iter().any(|lhs| lhs == "y"),
        "indexed when targets must not collapse to the whole array name"
    );
}

#[test]
fn test_todae_lowers_when_algorithm_record_assignment_to_fields() {
    let mut flat = Model::new();
    add_discrete_valued(&mut flat, "tick");
    add_discrete_real(&mut flat, "estimator.estimate.flightPathAngle");
    add_discrete_real(&mut flat, "estimator.estimate.speedChange");
    add_discrete_real(&mut flat, "guidance.estimate.flightPathAngle");
    add_discrete_real(&mut flat, "guidance.estimate.speedChange");

    flat.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: make_var_ref("tick"),
                stmts: vec![rumoca_core::Statement::Assignment {
                    comp: make_comp_ref_from_flat_name("guidance.estimate"),
                    value: make_var_ref("estimator.estimate"),
                    span: test_span(),
                }],
            }],
            span: test_span(),
        }],
        test_span(),
        "when record assignment".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("record-valued when assignment should lower to primitive field updates");

    let updates = dae
        .discrete
        .real_updates
        .iter()
        .filter_map(|eq| {
            Some((
                eq.lhs.as_ref()?.as_str().to_string(),
                format!("{:?}", eq.rhs),
            ))
        })
        .collect::<std::collections::HashMap<_, _>>();

    let flight_path = updates
        .get("guidance.estimate.flightPathAngle")
        .expect("missing lowered field update for guidance.estimate.flightPathAngle");
    assert!(
        flight_path.contains("estimator.estimate.flightPathAngle"),
        "field update should read the corresponding source field, got {flight_path}"
    );

    let speed_change = updates
        .get("guidance.estimate.speedChange")
        .expect("missing lowered field update for guidance.estimate.speedChange");
    assert!(
        speed_change.contains("estimator.estimate.speedChange"),
        "field update should read the corresponding source field, got {speed_change}"
    );
    assert!(
        !updates.contains_key("guidance.estimate"),
        "record assignment must not leave a non-primitive record target"
    );
}

#[test]
fn test_todae_lowers_when_algorithm_for_loop_with_sequential_cross_target_updates() {
    let mut flat = Model::new();
    add_tick_z_y_discrete_fixture(&mut flat);

    flat.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: make_var_ref("tick"),
                stmts: vec![rumoca_core::Statement::For {
                    indices: vec![two_iteration_for_index()],
                    equations: y_loop_assignment_then_z_increment(),
                    span: test_span(),
                }],
            }],
            span: test_span(),
        }],
        test_span(),
        "when for sequential indexed target".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("when for-loop algorithms with sequential updates should lower");

    let mut rhs_by_lhs = std::collections::HashMap::new();
    for eq in dae
        .discrete
        .real_updates
        .iter()
        .chain(dae.discrete.valued_updates.iter())
    {
        if let Some(lhs) = &eq.lhs {
            rhs_by_lhs.insert(lhs.as_str().to_string(), format!("{:?}", eq.rhs));
        }
    }

    let y1 = rhs_by_lhs
        .get("y[1]")
        .expect("missing lowered y[1] equation");
    let y2 = rhs_by_lhs
        .get("y[2]")
        .expect("missing lowered y[2] equation");
    let z = rhs_by_lhs.get("z").expect("missing lowered z equation");

    // SPEC_0007 Stage 3 Contract: pre() is eliminated in every partition.
    // Event-entry observations of `z` are __pre__.z parameter references.
    assert!(
        y1.contains("VarName(\"__pre__.z\")"),
        "first loop assignment should observe event-entry z via __pre__.z, got {y1}"
    );
    assert!(
        y2.contains("VarName(\"__pre__.z\")") && y2.contains("Integer(1)"),
        "second loop assignment should observe z after one increment, got {y2}"
    );
    assert!(
        z.matches("Integer(1)").count() >= 2 && z.contains("VarName(\"__pre__.z\")"),
        "final z assignment should compose both loop increments from __pre__.z, got {z}"
    );
}

#[test]
fn test_todae_lowers_when_algorithm_preceding_assignment_visible_inside_for_loop() {
    let mut flat = Model::new();
    add_tick_z_y_discrete_fixture(&mut flat);

    flat.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: make_var_ref("tick"),
                stmts: vec![
                    rumoca_core::Statement::Assignment {
                        comp: make_comp_ref("z"),
                        value: rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Integer(5),
                            span: test_span(),
                        },
                        span: test_span(),
                    },
                    rumoca_core::Statement::For {
                        indices: vec![two_iteration_for_index()],
                        equations: y_loop_assignment_then_z_increment(),
                        span: test_span(),
                    },
                ],
            }],
            span: test_span(),
        }],
        test_span(),
        "when assignment before for loop".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("when assignments before for-loop should stay visible inside the loop");

    let mut rhs_by_lhs = std::collections::HashMap::new();
    for eq in dae
        .discrete
        .real_updates
        .iter()
        .chain(dae.discrete.valued_updates.iter())
    {
        if let Some(lhs) = &eq.lhs {
            rhs_by_lhs.insert(lhs.as_str().to_string(), format!("{:?}", eq.rhs));
        }
    }

    let y1 = rhs_by_lhs
        .get("y[1]")
        .expect("missing lowered y[1] equation");
    let y2 = rhs_by_lhs
        .get("y[2]")
        .expect("missing lowered y[2] equation");

    assert!(
        y1.contains("Integer(5)"),
        "first loop read should observe the preceding z assignment, got {y1}"
    );
    assert!(
        y2.contains("Integer(5)") && y2.contains("Integer(1)"),
        "second loop read should observe the incremented z value, got {y2}"
    );
}

#[test]
fn test_todae_lowers_top_level_algorithm_assignment_before_for_loop_sequentially() {
    let flat = build_top_level_assignment_before_loop_model();
    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("top-level sequential algorithm state should stay visible inside following for-loop");
    assert_top_level_assignment_before_loop_lowering(&dae);
}

#[test]
fn test_todae_lowers_initial_algorithm_assignment_to_fixed_false_parameter() {
    let mut flat = Model::new();
    add_fixed_false_parameter(&mut flat, "k");

    flat.initial_algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::Assignment {
            comp: make_comp_ref("k"),
            value: rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.0),
                span: test_span(),
            },
            span: test_span(),
        }],
        test_span(),
        "initial algorithm from SimpleFriction".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("MLS fixed=false parameters may be solved by initial algorithms");

    assert!(
        dae.initialization
            .equations
            .iter()
            .any(|eq| eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == "k")),
        "missing initialization equation for fixed=false parameter assignment"
    );
    assert!(
        dae.continuous.equations.is_empty(),
        "initial parameter assignment must not become a runtime continuous equation"
    );
}

#[test]
fn test_todae_rejects_initial_algorithm_assignment_to_default_fixed_parameter() {
    let mut flat = Model::new();
    add_parameter_with_fixed(&mut flat, "k", None);

    flat.initial_algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::Assignment {
            comp: make_comp_ref("k"),
            value: rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.0),
                span: test_span(),
            },
            span: test_span(),
        }],
        test_span(),
        "initial fixed parameter assignment".to_string(),
    ));

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("parameter fixed=true by default must not be solved by initial algorithms");

    assert!(
        err.to_string().contains("fixed parameter"),
        "unexpected error: {err}"
    );
}

#[test]
fn test_todae_rejects_initial_algorithm_assignment_to_explicit_fixed_parameter() {
    let mut flat = Model::new();
    add_parameter_with_fixed(&mut flat, "k", Some(true));

    flat.initial_algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::Assignment {
            comp: make_comp_ref("k"),
            value: rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.0),
                span: test_span(),
            },
            span: test_span(),
        }],
        test_span(),
        "initial fixed parameter assignment".to_string(),
    ));

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("parameter fixed=true must not be solved by initial algorithms");

    assert!(
        err.to_string().contains("fixed parameter"),
        "unexpected error: {err}"
    );
}

#[test]
fn test_todae_rejects_runtime_algorithm_assignment_to_parameter() {
    let mut flat = Model::new();
    add_fixed_false_parameter(&mut flat, "k");

    flat.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::Assignment {
            comp: make_comp_ref("k"),
            value: rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.0),
                span: test_span(),
            },
            span: test_span(),
        }],
        test_span(),
        "runtime parameter assignment".to_string(),
    ));

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("regular algorithm sections must not assign parameters");

    assert!(
        matches!(err, ToDaeError::UnsupportedAlgorithm { ref section, .. } if section == "model"),
        "expected unsupported model algorithm diagnostic, got {err:?}"
    );
}

#[test]
fn test_todae_lowers_initial_when_algorithm_discrete_assignment_to_initial_equations() {
    let mut flat = Model::new();
    add_discrete_valued(&mut flat, "armed");

    flat.initial_algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: rumoca_core::Expression::BuiltinCall {
                    function: BuiltinFunction::Initial,
                    args: vec![],
                    span: test_span(),
                },
                stmts: vec![rumoca_core::Statement::Assignment {
                    comp: make_comp_ref("armed"),
                    value: rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Boolean(true),
                        span: test_span(),
                    },
                    span: test_span(),
                }],
            }],
            span: test_span(),
        }],
        test_span(),
        "initial when algorithm".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("MLS initial algorithms with when-statements initialize discrete targets");

    assert!(
        dae.discrete.valued_updates.is_empty(),
        "initial algorithm assignments must not populate runtime f_m"
    );
    assert!(
        dae.initialization
            .equations
            .iter()
            .any(|eq| eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == "armed")),
        "missing initial equation for initial when algorithm assignment"
    );
}

#[test]
fn test_todae_lowers_vector_when_algorithm_condition_to_scalar_event_guard() {
    let mut flat = Model::new();
    for name in ["tick_a", "tick_b", "y"] {
        add_discrete_valued(&mut flat, name);
    }

    flat.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                // MLS §8.3.5: a vector when-condition is active when any scalar
                // element becomes true; it is not an array-valued if guard.
                cond: rumoca_core::Expression::Array {
                    elements: vec![make_var_ref("tick_a"), make_var_ref("tick_b")],
                    is_matrix: false,
                    span: test_span(),
                },
                stmts: vec![rumoca_core::Statement::Assignment {
                    comp: make_comp_ref("y"),
                    value: rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Boolean(true),
                        span: test_span(),
                    },
                    span: test_span(),
                }],
            }],
            span: test_span(),
        }],
        test_span(),
        "vector when algorithm".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("vector when algorithm should lower");

    let eq = dae
        .discrete
        .valued_updates
        .iter()
        .find(|eq| eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == "y"))
        .expect("missing lowered y event equation");
    let rumoca_core::Expression::If { branches, .. } = &eq.rhs else {
        panic!("expected guarded event assignment, got {:?}", eq.rhs);
    };
    let [(guard, _)] = branches.as_slice() else {
        panic!("expected one merged vector condition guard, got {branches:?}");
    };

    let mut edge_names = Vec::new();
    collect_edge_guard_names(guard, &mut edge_names);
    edge_names.sort();
    assert_eq!(
        edge_names,
        vec!["tick_a".to_string(), "tick_b".to_string()],
        "vector when guard must OR scalar activation edges, got {guard:?}"
    );
    assert!(
        !matches!(guard, rumoca_core::Expression::Array { .. }),
        "vector when guard must be a scalar Boolean expression, got {guard:?}"
    );
}

#[test]
fn test_todae_lowers_vector_when_algorithm_initial_condition_directly() {
    let mut flat = Model::new();
    for name in ["tick", "y"] {
        add_discrete_valued(&mut flat, name);
    }

    flat.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: rumoca_core::Expression::Array {
                    elements: vec![
                        rumoca_core::Expression::BuiltinCall {
                            function: rumoca_core::BuiltinFunction::Initial,
                            args: vec![],
                            span: test_span(),
                        },
                        make_var_ref("tick"),
                    ],
                    is_matrix: false,
                    span: test_span(),
                },
                stmts: vec![rumoca_core::Statement::Assignment {
                    comp: make_comp_ref("y"),
                    value: rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Boolean(true),
                        span: test_span(),
                    },
                    span: test_span(),
                }],
            }],
            span: test_span(),
        }],
        test_span(),
        "vector initial when algorithm".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("vector initial when algorithm should lower");

    let eq = dae
        .discrete
        .valued_updates
        .iter()
        .find(|eq| eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == "y"))
        .expect("missing lowered y event equation");
    let rumoca_core::Expression::If { branches, .. } = &eq.rhs else {
        panic!("expected guarded event assignment, got {:?}", eq.rhs);
    };
    let [(guard, _)] = branches.as_slice() else {
        panic!("expected one merged vector condition guard, got {branches:?}");
    };

    let mut edge_names = Vec::new();
    collect_edge_guard_names(guard, &mut edge_names);
    assert_eq!(edge_names, vec!["tick".to_string()]);
    assert!(
        contains_initial_call(guard),
        "explicit vector initial() when guard must stay active during initialization, got {guard:?}"
    );
}

#[test]
fn test_todae_rewrites_current_algorithm_value_inside_index_subscript() {
    let mut flat = Model::new();
    add_discrete_valued(&mut flat, "nextstate");
    add_discrete_valued(&mut flat, "x");
    add_discrete_valued(&mut flat, "table");
    add_discrete_valued(&mut flat, "strength");

    flat.algorithms.push(flat::Algorithm::new(
        vec![
            rumoca_core::Statement::Assignment {
                comp: make_comp_ref("nextstate"),
                value: rumoca_core::Expression::Index {
                    base: Box::new(make_var_ref("table")),
                    subscripts: vec![rumoca_core::Subscript::generated_expr(
                        Box::new(make_var_ref("x")),
                        test_span(),
                    )],
                    span: test_span(),
                },

                span: test_span(),
            },
            rumoca_core::Statement::Assignment {
                comp: make_comp_ref("nextstate"),
                value: rumoca_core::Expression::Index {
                    base: Box::new(make_var_ref("strength")),
                    subscripts: vec![rumoca_core::Subscript::generated_expr(
                        Box::new(make_var_ref("nextstate")),
                        test_span(),
                    )],
                    span: test_span(),
                },

                span: test_span(),
            },
        ],
        test_span(),
        "sequential index subscript algorithm".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("MLS §11.1 sequential algorithm reads inside subscripts use current values");

    let rhs = dae
        .discrete
        .valued_updates
        .iter()
        .find(|eq| {
            eq.lhs
                .as_ref()
                .is_some_and(|lhs| lhs.as_str() == "nextstate")
        })
        .map(|eq| format!("{:?}", eq.rhs))
        .expect("missing nextstate discrete equation");

    assert!(
        !rhs.contains("VarName(\"nextstate\")"),
        "sequential subscript read must be rewritten before self-reference validation, got {rhs}"
    );
    assert!(
        rhs.contains("VarName(\"table\")") && rhs.contains("VarName(\"strength\")"),
        "expected second assignment to index strength by the previous table lookup, got {rhs}"
    );
}

#[test]
fn test_todae_rejects_model_algorithm_for_loop_with_non_constant_range() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "n");
    add_primitive_real(&mut flat, "y");

    flat.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::For {
            indices: vec![rumoca_core::ForIndex {
                ident: "i".to_string(),
                range: rumoca_core::Expression::Range {
                    start: Box::new(rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(1),
                        span: test_span(),
                    }),
                    step: None,
                    end: Box::new(make_var_ref("n")),
                    span: test_span(),
                },
            }],
            equations: vec![rumoca_core::Statement::Assignment {
                comp: make_comp_ref("y"),
                value: make_var_ref("i"),
                span: test_span(),
            }],

            span: test_span(),
        }],
        test_span(),
        "model for dynamic range".to_string(),
    ));

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("for-loop algorithms with non-constant ranges must fail");

    assert!(
        matches!(
            err,
            ToDaeError::UnsupportedAlgorithm {
                ref section,
                ref origin,
                ..
            } if section == "model"
                && (origin.contains("statement=ForRangeEndNotConstant")
                    || origin.contains("statement=ForRangeNotConstant"))
        ),
        "expected ED013 unsupported diagnostic for non-constant for range, got {err:?}"
    );
}

#[test]
fn test_todae_accepts_model_algorithm_for_loop_with_constant_array_binding_range() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "y");
    flat.add_variable(
        VarName::new("switched"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("switched"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(1),
                        span: test_span(),
                    },
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(3),
                        span: test_span(),
                    },
                ],
                is_matrix: false,
                span: test_span(),
            }),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );

    flat.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::For {
            indices: vec![rumoca_core::ForIndex {
                ident: "i".to_string(),
                range: make_var_ref("switched"),
            }],
            equations: vec![rumoca_core::Statement::Assignment {
                comp: make_comp_ref("y"),
                value: make_var_ref("i"),
                span: test_span(),
            }],
            span: test_span(),
        }],
        test_span(),
        "model for constant array binding range".to_string(),
    ));

    to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("for-loop algorithms with constant array binding ranges should lower");
}

#[test]
fn test_todae_lowers_supported_algorithm_slice_row_write_and_read() {
    let flat = build_supported_algorithm_slice_row_model();
    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("supported row slice algorithm should lower");
    assert_supported_algorithm_slice_row_lowering(&dae);
}

#[test]
fn test_todae_lowers_supported_algorithm_slice_row_read_to_scalar_refs() {
    let mut flat = Model::new();
    let span = test_span();
    flat.add_variable(
        VarName::new("int_addr"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("int_addr"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                span,
            }),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
    flat.add_variable(
        VarName::new("n_data"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("n_data"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(3),
                span,
            }),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
    flat.add_variable(
        VarName::new("mem"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("mem"),
            dims: vec![2, 3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );
    flat.add_variable(
        VarName::new("nextstate"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("nextstate"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(test_span())
        }),
    );

    flat.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::Assignment {
            comp: make_comp_ref("nextstate"),
            value: rumoca_core::Expression::Index {
                base: Box::new(make_var_ref("mem")),
                subscripts: vec![
                    rumoca_core::Subscript::generated_expr(
                        Box::new(make_var_ref("int_addr")),
                        span,
                    ),
                    rumoca_core::Subscript::generated_expr(
                        Box::new(rumoca_core::Expression::Range {
                            start: Box::new(rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Integer(1),
                                span,
                            }),
                            step: None,
                            end: Box::new(make_var_ref("n_data")),
                            span,
                        }),
                        span,
                    ),
                ],
                span,
            },

            span,
        }],
        span,
        "algorithm slice row read".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("supported row slice read should lower");

    let nextstate_rhs = dae
        .continuous
        .equations
        .iter()
        .map(|eq| format!("{:?}", eq.rhs))
        .find(|rhs| rhs.contains("VarName(\"nextstate\")"))
        .expect("missing lowered nextstate equation");

    assert!(
        nextstate_rhs.contains("VarName(\"mem\")")
            && nextstate_rhs.contains("VarName(\"int_addr\")")
            && nextstate_rhs.contains("Index { value: 1")
            && nextstate_rhs.contains("Index { value: 2")
            && nextstate_rhs.contains("Index { value: 3"),
        "expected lowered row read to expand to scalar mem refs, got {nextstate_rhs}"
    );
}

#[test]
fn test_todae_lowers_multi_output_algorithm_function_call_to_output_selections() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "u");
    add_primitive_real(&mut flat, "y1");
    add_primitive_real(&mut flat, "y2");

    let mut f = rumoca_core::Function::new("Pkg.multi", test_span());
    f.add_input(rumoca_core::FunctionParam::new(
        "u",
        "Real",
        crate::test_support::test_span(),
    ));
    f.add_output(rumoca_core::FunctionParam::new(
        "y1",
        "Real",
        crate::test_support::test_span(),
    ));
    f.add_output(rumoca_core::FunctionParam::new(
        "y2",
        "Real",
        crate::test_support::test_span(),
    ));
    f.body.push(rumoca_core::Statement::Assignment {
        comp: make_comp_ref("y1"),
        value: make_var_ref("u"),

        span: test_span(),
    });
    f.body.push(rumoca_core::Statement::Assignment {
        comp: make_comp_ref("y2"),
        value: rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(0.0),
            span: test_span(),
        },

        span: test_span(),
    });
    flat.add_function(f);

    flat.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::FunctionCall {
            comp: make_comp_ref("Pkg.multi"),
            args: vec![make_var_ref("u")],
            outputs: vec![make_comp_ref("y1"), make_comp_ref("y2")],

            span: test_span(),
        }],
        test_span(),
        "model multi-output call".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("multi-output model algorithm calls should lower to selection equations");

    let mut saw_y1 = false;
    let mut saw_y2 = false;
    for eq in &dae.continuous.equations {
        let rumoca_core::Expression::Binary { rhs, .. } = &eq.rhs else {
            continue;
        };
        let rumoca_core::Expression::FunctionCall { name, .. } = rhs.as_ref() else {
            continue;
        };
        if name.as_str() == "Pkg.multi.y1" {
            saw_y1 = true;
        } else if name.as_str() == "Pkg.multi.y2" {
            saw_y2 = true;
        }
    }
    assert!(saw_y1, "missing lowered selection call for first output");
    assert!(saw_y2, "missing lowered selection call for second output");
}
