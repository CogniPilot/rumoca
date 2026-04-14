use super::*;

fn make_comp_ref(name: &str) -> flat::ComponentReference {
    flat::ComponentReference {
        local: false,
        parts: vec![flat::ComponentRefPart {
            ident: name.to_string(),
            subs: vec![],
        }],
        def_id: None,
    }
}

fn make_subscripted_comp_ref(name: &str, index_expr: flat::Expression) -> flat::ComponentReference {
    flat::ComponentReference {
        local: false,
        parts: vec![flat::ComponentRefPart {
            ident: name.to_string(),
            subs: vec![flat::Subscript::Expr(Box::new(index_expr))],
        }],
        def_id: None,
    }
}

fn make_multi_subscripted_comp_ref(
    name: &str,
    subs: Vec<flat::Subscript>,
) -> flat::ComponentReference {
    flat::ComponentReference {
        local: false,
        parts: vec![flat::ComponentRefPart {
            ident: name.to_string(),
            subs,
        }],
        def_id: None,
    }
}

fn make_var_ref(name: &str) -> flat::Expression {
    flat::Expression::VarRef {
        name: VarName::new(name),
        subscripts: vec![],
    }
}

fn add_primitive_real(flat: &mut Model, name: &str) {
    flat.add_variable(
        VarName::new(name),
        flat::Variable {
            name: VarName::new(name),
            is_primitive: true,
            ..Default::default()
        },
    );
}

fn add_scalar_ode_with_rhs_call(flat: &mut Model, state_name: &str, call_name: &str) {
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(rumoca_ir_core::Token::default()),
            lhs: Box::new(flat::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref(state_name)],
            }),
            rhs: Box::new(flat::Expression::FunctionCall {
                name: VarName::new(call_name),
                args: vec![make_var_ref(state_name)],
                is_constructor: false,
            }),
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });
}

fn build_top_level_assignment_before_loop_model() -> Model {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("y"),
        flat::Variable {
            name: VarName::new("y"),
            variability: rumoca_ir_flat::Variability::Discrete(rumoca_ir_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("y0"),
        flat::Variable {
            name: VarName::new("y0"),
            variability: rumoca_ir_flat::Variability::Parameter(rumoca_ir_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("x"),
        flat::Variable {
            name: VarName::new("x"),
            dims: vec![2],
            variability: rumoca_ir_flat::Variability::Parameter(rumoca_ir_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("t"),
        flat::Variable {
            name: VarName::new("t"),
            dims: vec![2],
            variability: rumoca_ir_flat::Variability::Parameter(rumoca_ir_core::Token::default()),
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.algorithms.push(flat::Algorithm::new(
        vec![
            flat::Statement::Assignment {
                comp: make_comp_ref("y"),
                value: make_var_ref("y0"),
            },
            flat::Statement::For {
                indices: vec![flat::ForIndex {
                    ident: "i".to_string(),
                    range: flat::Expression::Range {
                        start: Box::new(flat::Expression::Literal(flat::Literal::Integer(1))),
                        step: None,
                        end: Box::new(flat::Expression::Literal(flat::Literal::Integer(2))),
                    },
                }],
                equations: vec![flat::Statement::If {
                    cond_blocks: vec![flat::StatementBlock {
                        cond: flat::Expression::Binary {
                            op: rumoca_ir_flat::OpBinary::Ge(rumoca_ir_core::Token::default()),
                            lhs: Box::new(make_var_ref("time")),
                            rhs: Box::new(flat::Expression::Index {
                                base: Box::new(make_var_ref("t")),
                                subscripts: vec![flat::Subscript::Expr(Box::new(make_var_ref(
                                    "i",
                                )))],
                            }),
                        },
                        stmts: vec![flat::Statement::Assignment {
                            comp: make_comp_ref("y"),
                            value: flat::Expression::Index {
                                base: Box::new(make_var_ref("x")),
                                subscripts: vec![flat::Subscript::Expr(Box::new(make_var_ref(
                                    "i",
                                )))],
                            },
                        }],
                    }],
                    else_block: None,
                }],
            },
        ],
        Span::DUMMY,
        "top-level assignment before for loop".to_string(),
    ));
    flat
}

fn assert_top_level_assignment_before_loop_lowering(dae: &Dae) {
    let rhs = dae
        .f_x
        .iter()
        .find_map(|eq| {
            let rhs = format!("{:?}", eq.rhs);
            rhs.contains("VarName(\"y\")").then_some(rhs)
        })
        .expect("missing lowered y residual");

    assert!(
        rhs.contains("VarName(\"y0\")"),
        "loop fallback should preserve the preceding y := y0 assignment, got {rhs}"
    );
    assert!(
        !rhs.contains("VarName(\"__pre__.y\")"),
        "top-level sequential algorithm lowering must not fall back to pre(y), got {rhs}"
    );
}

fn build_supported_algorithm_slice_row_model() -> Model {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("int_addr"),
        flat::Variable {
            name: VarName::new("int_addr"),
            variability: rumoca_ir_flat::Variability::Parameter(rumoca_ir_core::Token::default()),
            binding: Some(flat::Expression::Literal(flat::Literal::Integer(2))),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("n_data"),
        flat::Variable {
            name: VarName::new("n_data"),
            variability: rumoca_ir_flat::Variability::Parameter(rumoca_ir_core::Token::default()),
            binding: Some(flat::Expression::Literal(flat::Literal::Integer(3))),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("mem"),
        flat::Variable {
            name: VarName::new("mem"),
            dims: vec![2, 3],
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("mem_word"),
        flat::Variable {
            name: VarName::new("mem_word"),
            dims: vec![3],
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("nextstate"),
        flat::Variable {
            name: VarName::new("nextstate"),
            dims: vec![3],
            is_primitive: true,
            ..Default::default()
        },
    );

    let full_row = flat::Subscript::Expr(Box::new(flat::Expression::Range {
        start: Box::new(flat::Expression::Literal(flat::Literal::Integer(1))),
        step: None,
        end: Box::new(make_var_ref("n_data")),
    }));

    flat.algorithms.push(flat::Algorithm::new(
        vec![
            flat::Statement::Assignment {
                comp: make_multi_subscripted_comp_ref(
                    "mem",
                    vec![
                        flat::Subscript::Expr(Box::new(make_var_ref("int_addr"))),
                        full_row.clone(),
                    ],
                ),
                value: make_var_ref("mem_word"),
            },
            flat::Statement::Assignment {
                comp: make_comp_ref("nextstate"),
                value: flat::Expression::Index {
                    base: Box::new(make_var_ref("mem")),
                    subscripts: vec![
                        flat::Subscript::Expr(Box::new(make_var_ref("int_addr"))),
                        full_row,
                    ],
                },
            },
        ],
        Span::DUMMY,
        "algorithm slice row".to_string(),
    ));

    flat
}

fn assert_supported_algorithm_slice_row_lowering(dae: &Dae) {
    let lowered_rhs: Vec<_> = dae.f_x.iter().map(|eq| format!("{:?}", eq.rhs)).collect();
    assert!(
        lowered_rhs.iter().any(|rhs| rhs
            .contains("mem[VarRef { name: VarName(\\\"int_addr\\\"), subscripts: [] },1]")
            && rhs.contains("VarName(\"mem_word\"), subscripts: [Index(1)]"))
            && lowered_rhs.iter().any(|rhs| rhs
                .contains("mem[VarRef { name: VarName(\\\"int_addr\\\"), subscripts: [] },2]")
                && rhs.contains("VarName(\"mem_word\"), subscripts: [Index(2)]"))
            && lowered_rhs.iter().any(|rhs| rhs
                .contains("mem[VarRef { name: VarName(\\\"int_addr\\\"), subscripts: [] },3]")
                && rhs.contains("VarName(\"mem_word\"), subscripts: [Index(3)]")),
        "expected scalarized row write residuals, got {lowered_rhs:?}"
    );

    let nextstate_rhs = dae
        .f_x
        .iter()
        .map(|eq| format!("{:?}", eq.rhs))
        .find(|rhs| rhs.contains("VarName(\"nextstate\")"))
        .expect("missing lowered nextstate equation");
    assert!(
        nextstate_rhs.contains("VarName(\"mem_word\"), subscripts: [Index(1)]")
            && nextstate_rhs.contains("VarName(\"mem_word\"), subscripts: [Index(2)]")
            && nextstate_rhs.contains("VarName(\"mem_word\"), subscripts: [Index(3)]"),
        "expected sequential slice read to observe preceding mem_word write, got {nextstate_rhs}"
    );
}

#[test]
fn test_todae_preserves_function_algorithm_bodies_for_codegen_readability() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    let mut fn_def = rumoca_ir_flat::Function::new("f", Span::DUMMY);
    fn_def.add_input(flat::FunctionParam::new("u", "Real"));
    fn_def.add_output(flat::FunctionParam::new("y", "Real"));
    fn_def.body.push(flat::Statement::Assignment {
        comp: make_comp_ref("y"),
        value: flat::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(rumoca_ir_core::Token::default()),
            lhs: Box::new(make_var_ref("u")),
            rhs: Box::new(flat::Expression::Literal(rumoca_ir_flat::Literal::Real(
                1.0,
            ))),
        },
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
        .functions
        .get(&dae::VarName::new("f"))
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
        vec![flat::Statement::Assignment {
            comp: make_comp_ref("y"),
            value: make_var_ref("x"),
        }],
        Span::DUMMY,
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
        dae.f_x
            .iter()
            .any(|eq| eq.origin.contains("algorithm assignment")),
        "lowered model algorithm assignment should appear in f_x"
    );
}

#[test]
fn test_todae_lowers_model_algorithm_for_loop_with_static_range() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");
    add_primitive_real(&mut flat, "y");

    flat.algorithms.push(flat::Algorithm::new(
        vec![flat::Statement::For {
            indices: vec![flat::ForIndex {
                ident: "i".to_string(),
                range: flat::Expression::Range {
                    start: Box::new(flat::Expression::Literal(flat::Literal::Integer(1))),
                    step: None,
                    end: Box::new(flat::Expression::Literal(flat::Literal::Integer(3))),
                },
            }],
            equations: vec![flat::Statement::Assignment {
                comp: make_comp_ref("y"),
                value: make_var_ref("i"),
            }],
        }],
        Span::DUMMY,
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
        dae.f_x
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
        flat::Variable {
            name: VarName::new("y"),
            dims: vec![2],
            is_primitive: true,
            ..Default::default()
        },
    );

    flat.algorithms.push(flat::Algorithm::new(
        vec![flat::Statement::For {
            indices: vec![flat::ForIndex {
                ident: "i".to_string(),
                range: flat::Expression::Range {
                    start: Box::new(flat::Expression::Literal(flat::Literal::Integer(1))),
                    step: None,
                    end: Box::new(flat::Expression::Literal(flat::Literal::Integer(2))),
                },
            }],
            equations: vec![flat::Statement::Assignment {
                comp: make_subscripted_comp_ref("y", make_var_ref("i")),
                value: make_var_ref("i"),
            }],
        }],
        Span::DUMMY,
        "model for indexed target".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("for-loop algorithms with static indexed targets should lower to equations");

    let lowered_rhs: Vec<String> = dae.f_x.iter().map(|eq| format!("{:?}", eq.rhs)).collect();
    assert!(
        lowered_rhs
            .iter()
            .any(|rhs| rhs.contains("VarName(\"y[1]\")")),
        "expected lowered residual for indexed target y[1], got {lowered_rhs:?}"
    );
    assert!(
        lowered_rhs
            .iter()
            .any(|rhs| rhs.contains("VarName(\"y[2]\")")),
        "expected lowered residual for indexed target y[2], got {lowered_rhs:?}"
    );
    assert!(
        !lowered_rhs.iter().any(|rhs| rhs.contains("VarName(\"y\")")),
        "indexed algorithm targets must not collapse to the whole array name"
    );
}

#[test]
fn test_todae_lowers_when_algorithm_for_loop_with_subscripted_targets() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("tick"),
        flat::Variable {
            name: VarName::new("tick"),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("y"),
        flat::Variable {
            name: VarName::new("y"),
            dims: vec![2],
            variability: rumoca_ir_flat::Variability::Discrete(rumoca_ir_core::Token::default()),
            is_primitive: true,
            ..Default::default()
        },
    );

    flat.algorithms.push(flat::Algorithm::new(
        vec![flat::Statement::When(vec![flat::StatementBlock {
            cond: make_var_ref("tick"),
            stmts: vec![flat::Statement::For {
                indices: vec![flat::ForIndex {
                    ident: "i".to_string(),
                    range: flat::Expression::Range {
                        start: Box::new(flat::Expression::Literal(flat::Literal::Integer(1))),
                        step: None,
                        end: Box::new(flat::Expression::Literal(flat::Literal::Integer(2))),
                    },
                }],
                equations: vec![flat::Statement::Assignment {
                    comp: make_subscripted_comp_ref("y", make_var_ref("i")),
                    value: make_var_ref("tick"),
                }],
            }],
        }])],
        Span::DUMMY,
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
        .f_z
        .iter()
        .chain(dae.f_m.iter())
        .filter_map(|eq| eq.lhs.clone())
        .collect();
    assert!(
        lowered_lhs.contains(&dae::VarName::new("y[1]")),
        "expected lowered indexed event target y[1], got {lowered_lhs:?}"
    );
    assert!(
        lowered_lhs.contains(&dae::VarName::new("y[2]")),
        "expected lowered indexed event target y[2], got {lowered_lhs:?}"
    );
    assert!(
        !lowered_lhs.contains(&dae::VarName::new("y")),
        "indexed when targets must not collapse to the whole array name"
    );
}

#[test]
fn test_todae_lowers_when_algorithm_for_loop_with_sequential_cross_target_updates() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("tick"),
        flat::Variable {
            name: VarName::new("tick"),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("z"),
        flat::Variable {
            name: VarName::new("z"),
            variability: rumoca_ir_flat::Variability::Discrete(rumoca_ir_core::Token::default()),
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("y"),
        flat::Variable {
            name: VarName::new("y"),
            dims: vec![2],
            variability: rumoca_ir_flat::Variability::Discrete(rumoca_ir_core::Token::default()),
            is_primitive: true,
            ..Default::default()
        },
    );

    flat.algorithms.push(flat::Algorithm::new(
        vec![flat::Statement::When(vec![flat::StatementBlock {
            cond: make_var_ref("tick"),
            stmts: vec![flat::Statement::For {
                indices: vec![flat::ForIndex {
                    ident: "i".to_string(),
                    range: flat::Expression::Range {
                        start: Box::new(flat::Expression::Literal(flat::Literal::Integer(1))),
                        step: None,
                        end: Box::new(flat::Expression::Literal(flat::Literal::Integer(2))),
                    },
                }],
                equations: vec![
                    flat::Statement::Assignment {
                        comp: make_subscripted_comp_ref("y", make_var_ref("i")),
                        value: make_var_ref("z"),
                    },
                    flat::Statement::Assignment {
                        comp: make_comp_ref("z"),
                        value: flat::Expression::Binary {
                            op: rumoca_ir_flat::OpBinary::Add(rumoca_ir_core::Token::default()),
                            lhs: Box::new(make_var_ref("z")),
                            rhs: Box::new(flat::Expression::Literal(flat::Literal::Integer(1))),
                        },
                    },
                ],
            }],
        }])],
        Span::DUMMY,
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
    for eq in dae.f_z.iter().chain(dae.f_m.iter()) {
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

    assert!(
        y1.contains("BuiltinCall { function: Pre, args: [VarRef { name: VarName(\"z\")"),
        "first loop assignment should observe event-entry z, got {y1}"
    );
    assert!(
        y2.contains("BuiltinCall { function: Pre, args: [VarRef { name: VarName(\"z\")")
            && y2.contains("Literal(Integer(1))"),
        "second loop assignment should observe z after one increment, got {y2}"
    );
    assert!(
        z.matches("Literal(Integer(1))").count() >= 2
            && z.contains("BuiltinCall { function: Pre, args: [VarRef { name: VarName(\"z\")"),
        "final z assignment should compose both loop increments from pre(z), got {z}"
    );
}

#[test]
fn test_todae_lowers_when_algorithm_preceding_assignment_visible_inside_for_loop() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("tick"),
        flat::Variable {
            name: VarName::new("tick"),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("z"),
        flat::Variable {
            name: VarName::new("z"),
            variability: rumoca_ir_flat::Variability::Discrete(rumoca_ir_core::Token::default()),
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("y"),
        flat::Variable {
            name: VarName::new("y"),
            dims: vec![2],
            variability: rumoca_ir_flat::Variability::Discrete(rumoca_ir_core::Token::default()),
            is_primitive: true,
            ..Default::default()
        },
    );

    flat.algorithms.push(flat::Algorithm::new(
        vec![flat::Statement::When(vec![flat::StatementBlock {
            cond: make_var_ref("tick"),
            stmts: vec![
                flat::Statement::Assignment {
                    comp: make_comp_ref("z"),
                    value: flat::Expression::Literal(flat::Literal::Integer(5)),
                },
                flat::Statement::For {
                    indices: vec![flat::ForIndex {
                        ident: "i".to_string(),
                        range: flat::Expression::Range {
                            start: Box::new(flat::Expression::Literal(flat::Literal::Integer(1))),
                            step: None,
                            end: Box::new(flat::Expression::Literal(flat::Literal::Integer(2))),
                        },
                    }],
                    equations: vec![
                        flat::Statement::Assignment {
                            comp: make_subscripted_comp_ref("y", make_var_ref("i")),
                            value: make_var_ref("z"),
                        },
                        flat::Statement::Assignment {
                            comp: make_comp_ref("z"),
                            value: flat::Expression::Binary {
                                op: rumoca_ir_flat::OpBinary::Add(rumoca_ir_core::Token::default()),
                                lhs: Box::new(make_var_ref("z")),
                                rhs: Box::new(flat::Expression::Literal(flat::Literal::Integer(1))),
                            },
                        },
                    ],
                },
            ],
        }])],
        Span::DUMMY,
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
    for eq in dae.f_z.iter().chain(dae.f_m.iter()) {
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
        y1.contains("Literal(Integer(5))"),
        "first loop read should observe the preceding z assignment, got {y1}"
    );
    assert!(
        y2.contains("Literal(Integer(5))") && y2.contains("Literal(Integer(1))"),
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
fn test_todae_rejects_model_algorithm_for_loop_with_non_constant_range() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "n");
    add_primitive_real(&mut flat, "y");

    flat.algorithms.push(flat::Algorithm::new(
        vec![flat::Statement::For {
            indices: vec![flat::ForIndex {
                ident: "i".to_string(),
                range: flat::Expression::Range {
                    start: Box::new(flat::Expression::Literal(flat::Literal::Integer(1))),
                    step: None,
                    end: Box::new(make_var_ref("n")),
                },
            }],
            equations: vec![flat::Statement::Assignment {
                comp: make_comp_ref("y"),
                value: make_var_ref("i"),
            }],
        }],
        Span::DUMMY,
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
    flat.add_variable(
        VarName::new("int_addr"),
        flat::Variable {
            name: VarName::new("int_addr"),
            variability: rumoca_ir_flat::Variability::Parameter(rumoca_ir_core::Token::default()),
            binding: Some(flat::Expression::Literal(flat::Literal::Integer(2))),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("n_data"),
        flat::Variable {
            name: VarName::new("n_data"),
            variability: rumoca_ir_flat::Variability::Parameter(rumoca_ir_core::Token::default()),
            binding: Some(flat::Expression::Literal(flat::Literal::Integer(3))),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("mem"),
        flat::Variable {
            name: VarName::new("mem"),
            dims: vec![2, 3],
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("nextstate"),
        flat::Variable {
            name: VarName::new("nextstate"),
            dims: vec![3],
            is_primitive: true,
            ..Default::default()
        },
    );

    flat.algorithms.push(flat::Algorithm::new(
        vec![flat::Statement::Assignment {
            comp: make_comp_ref("nextstate"),
            value: flat::Expression::Index {
                base: Box::new(make_var_ref("mem")),
                subscripts: vec![
                    flat::Subscript::Expr(Box::new(make_var_ref("int_addr"))),
                    flat::Subscript::Expr(Box::new(flat::Expression::Range {
                        start: Box::new(flat::Expression::Literal(flat::Literal::Integer(1))),
                        step: None,
                        end: Box::new(make_var_ref("n_data")),
                    })),
                ],
            },
        }],
        Span::DUMMY,
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
        .f_x
        .iter()
        .map(|eq| format!("{:?}", eq.rhs))
        .find(|rhs| rhs.contains("VarName(\"nextstate\")"))
        .expect("missing lowered nextstate equation");

    assert!(
        nextstate_rhs.contains("VarName(\"mem\"), subscripts: [Expr(VarRef { name: VarName(\"int_addr\"), subscripts: [] }), Index(1)]")
            && nextstate_rhs.contains("VarName(\"mem\"), subscripts: [Expr(VarRef { name: VarName(\"int_addr\"), subscripts: [] }), Index(2)]")
            && nextstate_rhs.contains("VarName(\"mem\"), subscripts: [Expr(VarRef { name: VarName(\"int_addr\"), subscripts: [] }), Index(3)]"),
        "expected lowered row read to expand to scalar mem refs, got {nextstate_rhs}"
    );
}

#[test]
fn test_todae_lowers_multi_output_algorithm_function_call_to_output_projections() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "u");
    add_primitive_real(&mut flat, "y1");
    add_primitive_real(&mut flat, "y2");

    let mut f = rumoca_ir_flat::Function::new("Pkg.multi", Span::DUMMY);
    f.add_input(flat::FunctionParam::new("u", "Real"));
    f.add_output(flat::FunctionParam::new("y1", "Real"));
    f.add_output(flat::FunctionParam::new("y2", "Real"));
    f.body.push(flat::Statement::Assignment {
        comp: make_comp_ref("y1"),
        value: make_var_ref("u"),
    });
    f.body.push(flat::Statement::Assignment {
        comp: make_comp_ref("y2"),
        value: flat::Expression::Literal(flat::Literal::Real(0.0)),
    });
    flat.add_function(f);

    flat.algorithms.push(flat::Algorithm::new(
        vec![flat::Statement::FunctionCall {
            comp: make_comp_ref("Pkg.multi"),
            args: vec![make_var_ref("u")],
            outputs: vec![make_var_ref("y1"), make_var_ref("y2")],
        }],
        Span::DUMMY,
        "model multi-output call".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("multi-output model algorithm calls should lower to projection equations");

    let mut saw_y1 = false;
    let mut saw_y2 = false;
    for eq in &dae.f_x {
        let dae::Expression::Binary { rhs, .. } = &eq.rhs else {
            continue;
        };
        let dae::Expression::FunctionCall { name, .. } = rhs.as_ref() else {
            continue;
        };
        if name.as_str() == "Pkg.multi.y1" {
            saw_y1 = true;
        } else if name.as_str() == "Pkg.multi.y2" {
            saw_y2 = true;
        }
    }
    assert!(saw_y1, "missing lowered projection call for first output");
    assert!(saw_y2, "missing lowered projection call for second output");
}
