use super::*;
use std::collections::BTreeSet;

mod projection_plan_more;
fn array_var(name: &str, dims: &[i64]) -> dae::Variable {
    dae::Variable {
        name: rumoca_core::VarName::new(name),
        dims: dims.to_vec(),
        ..Default::default()
    }
}

fn scalar_var(name: &str) -> dae::Variable {
    dae::Variable {
        name: rumoca_core::VarName::new(name),
        ..Default::default()
    }
}

#[test]
fn algebraic_projection_plan_uses_blt_scalar_blocks() {
    let rows = vec![
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 1 },
            solve::LinearOp::LoadY { dst: 1, index: 2 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ],
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 2 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 3 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
    ];
    let row_targets = vec![None; rows.len()];
    let plan = super::lower_algebraic_projection_plan(&rows, &row_targets, 1, 4)
        .expect("projection plan should lower");

    assert_eq!(plan.blocks.len(), 3);
    let mut rows = plan
        .blocks
        .iter()
        .map(|block| block.rows.clone())
        .collect::<Vec<_>>();
    rows.sort();
    assert_eq!(rows, vec![vec![1], vec![2], vec![3]]);
    for block in &plan.blocks {
        assert_eq!(block.rows.len(), 1);
        assert_eq!(block.y_indices.len(), 1);
        assert!(block.causal_steps.is_empty());
    }
}

#[test]
fn projection_incidence_uses_store_output_slice() {
    let row = vec![
        solve::LinearOp::LoadY { dst: 0, index: 10 },
        solve::LinearOp::LoadY { dst: 1, index: 11 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::LoadY { dst: 3, index: 12 },
        solve::LinearOp::StoreOutput { src: 3 },
    ];
    let projection_set = BTreeSet::from([10, 11, 12]);

    let y_indices = super::collect_algebraic_y_indices_for_row(&row, &projection_set);

    assert_eq!(y_indices, BTreeSet::from([12]));
}

#[test]
fn projection_incidence_keeps_coupled_residual_variables() {
    let row = vec![
        solve::LinearOp::LoadY { dst: 0, index: 10 },
        solve::LinearOp::LoadY { dst: 1, index: 11 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ];
    let projection_set = BTreeSet::from([10, 11]);

    let y_indices = super::collect_algebraic_y_indices_for_row(&row, &projection_set);

    assert_eq!(y_indices, BTreeSet::from([10, 11]));
}

#[test]
fn algebraic_projection_loop_uses_blt_unknowns_not_dependency_inputs() {
    let rows = vec![
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::LoadY { dst: 1, index: 1 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::LoadY { dst: 3, index: 2 },
            solve::LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Add,
                lhs: 2,
                rhs: 3,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ],
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 1 },
            solve::LinearOp::LoadY { dst: 1, index: 2 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ],
    ];
    let row_targets = vec![None; rows.len()];
    let plan = super::lower_algebraic_projection_plan(&rows, &row_targets, 0, 3)
        .expect("projection plan should lower");

    let loop_block = plan
        .blocks
        .iter()
        .find(|block| block.rows.len() == 2)
        .expect("coupled rows should form an algebraic loop");

    assert_eq!(loop_block.y_indices, vec![1, 2]);
}

#[test]
fn algebraic_projection_loop_keeps_explicit_row_targets() {
    let projection_incidence = ProjectionIncidence {
        incidence: Incidence::new(
            vec![BTreeSet::from([0, 1, 2]).into_iter().collect()],
            vec![EquationRef(7)],
            vec![
                UnknownId::SolverY(10),
                UnknownId::SolverY(11),
                UnknownId::SolverY(12),
            ],
        ),
        unknown_y_indices: vec![10, 11, 12],
    };
    let row_targets = vec![
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        Some(solve::scalar_slot_y(12)),
    ];

    let block = super::lower_algebraic_loop_projection_block(
        &[EquationRef(7)],
        &[UnknownId::SolverY(10), UnknownId::SolverY(11)],
        &row_targets,
        &projection_incidence,
    )
    .expect("targeted loop block should lower");

    assert_eq!(block.rows, vec![7]);
    assert_eq!(block.y_indices, vec![10, 11, 12]);
}

#[test]
fn algebraic_projection_plan_merges_blocks_that_share_row_targets() {
    let blocks = super::merge_overlapping_projection_blocks(vec![
        solve::AlgebraicProjectionBlock {
            rows: vec![10],
            y_indices: vec![3],
            causal_steps: Vec::new(),
        },
        solve::AlgebraicProjectionBlock {
            rows: vec![20, 21],
            y_indices: vec![3, 4],
            causal_steps: Vec::new(),
        },
        solve::AlgebraicProjectionBlock {
            rows: vec![30],
            y_indices: vec![8],
            causal_steps: Vec::new(),
        },
    ]);

    assert_eq!(blocks.len(), 2);
    assert!(
        blocks
            .iter()
            .any(|block| { block.rows == vec![10, 20, 21] && block.y_indices == vec![3, 4] })
    );
    assert!(
        blocks
            .iter()
            .any(|block| { block.rows == vec![30] && block.y_indices == vec![8] })
    );
}

fn var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: Vec::new(),
        span: rumoca_core::Span::DUMMY,
    }
}

fn indexed_var(name: &str, subscripts: Vec<rumoca_core::Subscript>) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts,
        span: rumoca_core::Span::DUMMY,
    }
}

fn int_expr(value: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn range_expr(
    start: rumoca_core::Expression,
    end: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Range {
        start: Box::new(start),
        step: None,
        end: Box::new(end),
        span: rumoca_core::Span::DUMMY,
    }
}

fn stepped_range_expr(
    start: rumoca_core::Expression,
    step: rumoca_core::Expression,
    end: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Range {
        start: Box::new(start),
        step: Some(Box::new(step)),
        end: Box::new(end),
        span: rumoca_core::Span::DUMMY,
    }
}

fn pre_var(name: &str) -> rumoca_core::Expression {
    var(&format!("__pre__.{name}"))
}

fn internal_sample_call(args: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    function_call(rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME, args)
}

fn sample_event_indicator_expr(start: f64, interval: f64) -> rumoca_core::Expression {
    internal_sample_call(vec![
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(start),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(interval),
            span: rumoca_core::Span::DUMMY,
        },
    ])
}

fn insert_pre_parameter(dae_model: &mut dae::Dae, name: &str) {
    let pre_name = format!("__pre__.{name}");
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new(&pre_name), scalar_var(&pre_name));
}

fn der(expr: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: vec![expr],
        span: rumoca_core::Span::DUMMY,
    }
}

fn pre(expr: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Pre,
        args: vec![expr],
        span: rumoca_core::Span::DUMMY,
    }
}

fn function_call(name: &str, args: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(name).into(),
        args,
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn binary(
    op: rumoca_core::OpBinary,
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn unary(op: rumoca_core::OpUnary, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Unary {
        op,
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn change_lowered_expr(
    expr: rumoca_core::Expression,
    pre_expr: rumoca_core::Expression,
) -> rumoca_core::Expression {
    binary(rumoca_core::OpBinary::Neq, expr, pre_expr)
}

#[test]
fn solve_appendix_b_validation_rejects_surviving_pre_operator() {
    let mut dae_model = dae::Dae::default();
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: pre(var("x")),
        span: rumoca_core::Span::DUMMY,
        origin: "pre invariant regression".to_string(),
        scalar_count: 1,
    });

    let err = lower_solve_problem(&dae_model).expect_err("pre() must fail Solve validation");

    assert!(
        err.to_string()
            .contains("Solve Appendix-B validation failed")
            && err.to_string().contains("contains `pre`"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_unresolved_function_call() {
    let mut dae_model = dae::Dae::default();
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: function_call("Missing.f", vec![]),
        span: rumoca_core::Span::DUMMY,
        origin: "function target invariant regression".to_string(),
        scalar_count: 1,
    });

    let err = lower_solve_problem(&dae_model).expect_err("unresolved call must fail validation");

    assert!(
        err.to_string().contains("invalid function `Missing.f`")
            && err.to_string().contains("unresolved function call"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_undefined_linear_op_register() {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.residual =
        solve::ScalarProgramBlock::new(vec![vec![solve::LinearOp::StoreOutput { src: 7 }]]);

    let err = appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)
        .expect_err("undefined register read must fail validation");

    assert!(
        err.to_string().contains("reads undefined register 7"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_invalid_matmul_operand_range() {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::MatMul {
            lhs_ops: vec![solve::LinearOp::Const { dst: 0, value: 1.0 }],
            lhs_start: 0,
            rhs_ops: vec![solve::LinearOp::Const { dst: 2, value: 1.0 }],
            rhs_start: 2,
            m: 1,
            k: 2,
            n: 1,
            lhs_sparsity: solve::SparsityPattern::Dense,
            rhs_sparsity: solve::SparsityPattern::Dense,
            metadata: solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };

    let err = appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)
        .expect_err("short MatMul lhs register range must fail validation");

    assert!(
        err.to_string()
            .contains("lhs register range starting at 0 has undefined register 1"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_invalid_linsolve_operand_range() {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::LinSolve {
            setup_ops: vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::Const { dst: 1, value: 0.0 },
                solve::LinearOp::Const { dst: 4, value: 2.0 },
            ],
            matrix_start: 0,
            rhs_start: 4,
            n: 2,
            next_reg: 5,
            metadata: solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };

    let err = appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)
        .expect_err("short LinSolve matrix register range must fail validation");

    assert!(
        err.to_string()
            .contains("matrix register range starting at 0 has undefined register 2"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_load_seed_in_runtime_problem_rows() {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.residual = solve::ScalarProgramBlock::new(vec![vec![
        solve::LinearOp::LoadSeed { dst: 0, index: 0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]]);

    let err = appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)
        .expect_err("runtime SolveProblem rows must not read seed vectors");

    assert!(
        err.to_string()
            .contains("LoadSeed[0] is only valid in derivative/JVP artifact rows"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_allows_load_seed_in_jvp_artifact_rows() {
    let artifacts = solve::SolveArtifacts {
        continuous: solve::ContinuousSolveArtifacts {
            implicit_jacobian_v: solve::ComputeBlock::from_scalar_program_block(
                solve::ScalarProgramBlock::new(vec![vec![
                    solve::LinearOp::LoadSeed { dst: 0, index: 0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ]]),
            ),
            full_jacobian_v: solve::ScalarProgramBlock::new(vec![vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]]),
            ..solve::ContinuousSolveArtifacts::default()
        },
    };

    appendix_b_validation::validate_solve_artifacts_appendix_b_invariants(&artifacts)
        .expect("JVP artifacts may read seed vectors");
}

#[test]
fn solve_appendix_b_validation_rejects_invalid_artifact_registers() {
    let artifacts = solve::SolveArtifacts {
        continuous: solve::ContinuousSolveArtifacts {
            full_jacobian_v: solve::ScalarProgramBlock::new(vec![vec![
                solve::LinearOp::StoreOutput { src: 3 },
            ]]),
            ..solve::ContinuousSolveArtifacts::default()
        },
    };

    let err = appendix_b_validation::validate_solve_artifacts_appendix_b_invariants(&artifacts)
        .expect_err("malformed JVP artifact rows must fail validation");

    assert!(
        err.to_string().contains("reads undefined register 3"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_invalid_table_operand_register() {
    let mut problem = solve::SolveProblem::default();
    problem.events.root_conditions = solve::ScalarProgramBlock::new(vec![vec![
        solve::LinearOp::Const { dst: 0, value: 0.0 },
        solve::LinearOp::Const { dst: 2, value: 1.0 },
        solve::LinearOp::TableLookup {
            dst: 3,
            table_id: 0,
            column: 1,
            input: 2,
        },
        solve::LinearOp::StoreOutput { src: 3 },
    ]]);

    let err = appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)
        .expect_err("undefined table operand register must fail validation");

    assert!(
        err.to_string().contains("reads undefined register 1"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_invalid_random_state_index() {
    let mut problem = solve::SolveProblem::default();
    problem.discrete.rhs = solve::ScalarProgramBlock::new(vec![vec![
        solve::LinearOp::Const {
            dst: 0,
            value: 11.0,
        },
        solve::LinearOp::Const {
            dst: 1,
            value: 17.0,
        },
        solve::LinearOp::RandomInitialState {
            dst: 2,
            generator: solve::RandomGenerator::Xorshift64Star,
            local_seed: 0,
            global_seed: 1,
            state_index: 3,
            state_len: 3,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]]);

    let err = appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)
        .expect_err("out-of-range random state index must fail validation");

    assert!(
        err.to_string()
            .contains("RandomInitialState state_index 3 is outside state_len=3"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_invalid_random_state_register_range() {
    let mut problem = solve::SolveProblem::default();
    problem.discrete.rhs = solve::ScalarProgramBlock::new(vec![vec![
        solve::LinearOp::Const { dst: 0, value: 1.0 },
        solve::LinearOp::RandomResult {
            dst: 2,
            generator: solve::RandomGenerator::Xorshift64Star,
            state_start: 0,
            state_len: 2,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]]);

    let err = appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)
        .expect_err("short random state register range must fail validation");

    assert!(
        err.to_string().contains("reads undefined register 1"),
        "unexpected error: {err}"
    );
}

#[test]
fn solver_name_index_maps_include_matrix_aliases() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("M"), array_var("M", &[3, 4]));

    let maps = build_solver_name_index_maps(&dae_model, 12);

    assert_eq!(
        maps.names,
        vec![
            "M[1,1]", "M[1,2]", "M[1,3]", "M[1,4]", "M[2,1]", "M[2,2]", "M[2,3]", "M[2,4]",
            "M[3,1]", "M[3,2]", "M[3,3]", "M[3,4]",
        ]
    );
    assert_eq!(maps.name_to_idx.get("M"), Some(&0));
    assert_eq!(maps.name_to_idx.get("M[1]"), Some(&0));
    assert_eq!(maps.name_to_idx.get("M[1,1]"), Some(&0));
    assert_eq!(maps.name_to_idx.get("M[1,2]"), Some(&1));
    assert_eq!(maps.name_to_idx.get("M[5]"), Some(&4));
    assert_eq!(maps.name_to_idx.get("M[2,1]"), Some(&4));
    assert_eq!(maps.name_to_idx.get("M[3,4]"), Some(&11));
    assert_eq!(
        solve::solver_idx_for_target("M[2,1]", &maps.name_to_idx),
        Some(4)
    );
    assert_eq!(
        maps.base_to_indices.get("M").cloned().unwrap_or_default(),
        (0..12).collect::<Vec<_>>()
    );
}

#[test]
fn solver_name_index_aliases_respect_truncated_solver_len() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("M"), array_var("M", &[2, 2]));

    let maps = build_solver_name_index_maps(&dae_model, 3);

    assert_eq!(maps.names, vec!["M[1,1]", "M[1,2]", "M[2,1]"]);
    assert_eq!(maps.name_to_idx.get("M[1]"), Some(&0));
    assert_eq!(maps.name_to_idx.get("M[1,1]"), Some(&0));
    assert_eq!(maps.name_to_idx.get("M[2]"), Some(&1));
    assert_eq!(maps.name_to_idx.get("M[1,2]"), Some(&1));
    assert_eq!(maps.name_to_idx.get("M[3]"), Some(&2));
    assert_eq!(maps.name_to_idx.get("M[2,1]"), Some(&2));
    assert_eq!(maps.name_to_idx.get("M[2,2]"), None);
}

#[test]
fn solver_name_index_aliases_do_not_reintroduce_runtime_discrete_names() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("a"), scalar_var("a"));
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("dup"), scalar_var("dup"));
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("b"), scalar_var("b"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("dup"), scalar_var("dup"));

    let layout = layout::build_var_layout_with_solver_len(&dae_model, 2);
    let solve_layout = lower_solve_layout(&dae_model, 2);

    assert_eq!(solve_layout.solver_maps.names, vec!["a", "b"]);
    assert_eq!(solve_layout.solver_maps.name_to_idx.get("a"), Some(&0));
    assert_eq!(solve_layout.solver_maps.name_to_idx.get("b"), Some(&1));
    assert_eq!(solve_layout.solver_maps.name_to_idx.get("dup"), None);
    assert!(matches!(
        layout.binding("dup"),
        Some(solve::ScalarSlot::P { .. })
    ));
}

#[test]
fn solve_problem_lowers_appendix_b_condition_memory_as_initial_updates() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("p"), scalar_var("p"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("d"), scalar_var("d"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("c[1]"), scalar_var("c[1]"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("flag"), scalar_var("flag"));
    dae_model.conditions.equations.push(dae::Equation::explicit(
        rumoca_core::VarName::new("c[1]"),
        var("flag"),
        Default::default(),
        "condition memory",
    ));
    dae_model.conditions.equations.push(dae::Equation::explicit(
        rumoca_core::VarName::new("d"),
        var("p"),
        Default::default(),
        "non-condition discrete assignment in f_c",
    ));

    let problem = lower_solve_problem(&dae_model).expect("condition memory should lower");

    assert_eq!(problem.initialization.update_rhs.len(), 1);
    assert_eq!(
        problem.initialization.update_targets,
        vec![solve::scalar_slot_p(2)]
    );
}

#[test]
fn solve_problem_lowers_generated_condition_memory_initial_updates() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("c"), scalar_var("c"));
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("__rumoca_c[1]"),
        scalar_var("__rumoca_c[1]"),
    );
    dae_model.conditions.equations.push(dae::Equation::explicit(
        rumoca_core::VarName::new("__rumoca_c[1]"),
        var("c"),
        Default::default(),
        "generated condition memory",
    ));

    let problem = lower_solve_problem(&dae_model).expect("generated condition memory should lower");

    assert_eq!(problem.initialization.update_rhs.len(), 1);
    assert_eq!(
        problem.initialization.update_targets,
        vec![solve::scalar_slot_p(1)]
    );
}

#[test]
fn solve_problem_zero_fills_missing_non_state_implicit_rows_for_templates() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("v"), scalar_var("v"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("i"), scalar_var("i"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("R"), scalar_var("R"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Sub,
            var("v"),
            binary(rumoca_core::OpBinary::Mul, var("R"), var("i")),
        ),
        Default::default(),
        "underdetermined template metadata row",
    ));

    let problem = lower_solve_problem(&dae_model).expect("template solve-IR should lower");

    assert_eq!(problem.solve_layout.solver_scalar_count(), 2);
    let rhs = rumoca_eval_solve::to_scalar_program_block(&problem.continuous.implicit_rhs);
    assert_eq!(rhs.programs.len(), 2);
    assert_eq!(rhs.programs[1], zero_rhs_row());
}

#[test]
fn solve_problem_keeps_input_driven_algebraic_equation_implicit() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("T"), scalar_var("T"));
    dae_model
        .variables
        .inputs
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), var("T")),
        Default::default(),
        "state derivative",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, var("T"), var("u")),
        Default::default(),
        // MLS §4.4.4 and Appendix B: input variables are known external
        // quantities during continuous solving. The equation constrains
        // the algebraic unknown `T`; it must not be converted into a
        // runtime assignment that writes the input tail.
        "algebraic driven by input",
    ));

    let problem = lower_solve_problem(&dae_model).expect("input algebraic should lower");

    assert!(problem.discrete.runtime_assignment_rhs.is_empty());
    assert_eq!(problem.solve_layout.solver_scalar_count(), 2);
    let implicit_rhs = rumoca_eval_solve::to_scalar_program_block(&problem.continuous.implicit_rhs);
    assert_ne!(implicit_rhs.programs[1], zero_rhs_row());
    assert!(matches!(
        problem.continuous.implicit_row_targets[1],
        Some(solve::ScalarSlot::Y { index: 1, .. })
    ));
}

#[test]
fn solve_problem_partitions_derivative_rows_by_ir_not_position() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("a"), scalar_var("a"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("b"), scalar_var("b"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Sub,
            var("a"),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span: rumoca_core::Span::DUMMY,
            },
        ),
        Default::default(),
        "algebraic row before derivative row",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), var("a")),
        Default::default(),
        // MLS Appendix B B.1a: continuous equations are simultaneous; a
        // state derivative row is not required to be first in `f_x`.
        "state derivative row after algebraic row",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, var("b"), var("x")),
        Default::default(),
        "algebraic row after derivative row",
    ));

    let problem = lower_solve_problem(&dae_model).expect("unordered f_x should lower");

    let drhs = rumoca_eval_solve::to_scalar_program_block(&problem.continuous.derivative_rhs);
    let rhs = rumoca_eval_solve::to_scalar_program_block(&problem.continuous.implicit_rhs);
    assert_eq!(drhs.programs.len(), 1);
    assert_eq!(problem.continuous.residual.programs.len(), 2);
    assert_eq!(rhs.programs.len(), 3);
    assert_ne!(rhs.programs[0], zero_rhs_row());
    assert_ne!(rhs.programs[1], zero_rhs_row());
    assert_ne!(rhs.programs[2], zero_rhs_row());
    assert!(matches!(
        problem.continuous.implicit_row_targets[0],
        Some(solve::ScalarSlot::Y { index: 0, .. })
    ));
    assert!(matches!(
        problem.continuous.implicit_row_targets[1],
        Some(solve::ScalarSlot::Y { index: 1, .. })
    ));
    assert!(matches!(
        problem.continuous.implicit_row_targets[2],
        Some(solve::ScalarSlot::Y { index: 2, .. })
    ));
}

#[test]
fn solve_problem_expands_sliced_derivative_rows_with_structural_bounds() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), array_var("x", &[2]));
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("nx"),
        dae::Variable {
            name: rumoca_core::VarName::new("nx"),
            start: Some(int_expr(2)),
            fixed: Some(true),
            ..Default::default()
        },
    );
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Sub,
            der(indexed_var(
                "x",
                vec![rumoca_core::Subscript::generated_index(
                    1,
                    rumoca_core::Span::DUMMY,
                )],
            )),
            int_expr(0),
        ),
        Default::default(),
        "scalar first derivative",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Sub,
            der(indexed_var(
                "x",
                vec![rumoca_core::Subscript::generated_expr(Box::new(
                    range_expr(int_expr(2), var("nx")),
                ))],
            )),
            indexed_var(
                "x",
                vec![rumoca_core::Subscript::generated_expr(Box::new(
                    range_expr(
                        int_expr(1),
                        binary(rumoca_core::OpBinary::Sub, var("nx"), int_expr(1)),
                    ),
                ))],
            ),
        ),
        Default::default(),
        // MLS §10.5 permits vector subscripts in expressions. A vectorized
        // derivative equation still defines scalar derivative equations for
        // the selected state components after structural parameters resolve.
        "sliced derivative row",
    ));

    let problem = lower_solve_problem(&dae_model)
        .expect("sliced derivative equations with structural bounds should lower");

    let drhs = rumoca_eval_solve::to_scalar_program_block(&problem.continuous.derivative_rhs);
    assert_eq!(drhs.programs.len(), 2);
}

#[test]
fn solve_problem_expands_descending_sliced_derivative_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), array_var("x", &[3]));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Sub,
            der(indexed_var(
                "x",
                vec![rumoca_core::Subscript::generated_index(
                    1,
                    rumoca_core::Span::DUMMY,
                )],
            )),
            int_expr(0),
        ),
        Default::default(),
        "scalar first derivative",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Sub,
            der(indexed_var(
                "x",
                vec![rumoca_core::Subscript::generated_expr(Box::new(
                    stepped_range_expr(
                        int_expr(3),
                        rumoca_core::Expression::Unary {
                            op: rumoca_core::OpUnary::Minus,
                            rhs: Box::new(int_expr(1)),
                            span: rumoca_core::Span::DUMMY,
                        },
                        int_expr(2),
                    ),
                ))],
            )),
            indexed_var(
                "x",
                vec![rumoca_core::Subscript::generated_expr(Box::new(
                    stepped_range_expr(
                        int_expr(2),
                        rumoca_core::Expression::Unary {
                            op: rumoca_core::OpUnary::Minus,
                            rhs: Box::new(int_expr(1)),
                            span: rumoca_core::Span::DUMMY,
                        },
                        int_expr(1),
                    ),
                ))],
            ),
        ),
        Default::default(),
        // MLS §10.4 range expressions may use a negative step. Sliced
        // derivative extraction must preserve the selected component order.
        "descending sliced derivative row",
    ));

    let problem = lower_solve_problem(&dae_model)
        .expect("descending sliced derivative equations should lower");

    let drhs = rumoca_eval_solve::to_scalar_program_block(&problem.continuous.derivative_rhs);
    assert_eq!(drhs.programs.len(), 3);
}
