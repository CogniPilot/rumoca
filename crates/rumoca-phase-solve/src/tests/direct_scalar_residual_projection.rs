use super::*;

fn projected_pair_function() -> rumoca_core::Function {
    let span = solve_test_span();
    let mut function = rumoca_core::Function::new("Test.projectedPair", span);
    function
        .inputs
        .push(rumoca_core::FunctionParam::new("u", "Real", span));
    function
        .outputs
        .push(rumoca_core::FunctionParam::new("force", "Real", span).with_dims(vec![2]));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: test_component_ref_from_name("force"),
        value: rumoca_core::Expression::Array {
            elements: vec![
                var("u"),
                binary(rumoca_core::OpBinary::Add, var("u"), int_expr(1)),
            ],
            is_matrix: false,
            span,
        },
        span,
    });
    function
}

fn projected_pair_lane(index: i64) -> rumoca_core::Expression {
    let span = solve_test_span();
    rumoca_core::Expression::Index {
        base: Box::new(function_call("Test.projectedPair", vec![source_var("u")])),
        subscripts: vec![rumoca_core::Subscript::index(index, span)],
        span,
    }
}

fn projected_scalar_equation(owner: &str) -> dae::Equation {
    dae::Equation {
        lhs: Some(source_ref(owner)),
        rhs: binary(
            rumoca_core::OpBinary::Add,
            source_var("u"),
            projected_pair_lane(2),
        ),
        span: solve_test_span(),
        origin: format!("projected scalar owner {owner}"),
        scalar_count: 1,
    }
}

fn projected_scalar_dae(owner: &str) -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("y"),
        source_array_var("y", &[2, 2]),
    );
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("u"), source_scalar_var("u"));
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("Test.projectedPair"),
        projected_pair_function(),
    );
    dae_model
        .continuous
        .equations
        .push(projected_scalar_equation(owner));
    dae_model
}

#[test]
fn solve_problem_uses_direct_scalar_owner_for_projected_function_residual() {
    let dae_model = projected_scalar_dae("y[1,1]");

    let problem = lower_solve_problem_with_solver_len(&dae_model, 4)
        .expect("a rendered scalar owner with a direct layout binding should lower normally");
    let residual = scalar_program_block_fixture(&problem.continuous.residual);
    let [row] = residual.programs.as_slice() else {
        panic!("expected exactly one explicit residual row");
    };
    let owner = problem
        .layout
        .binding("y[1,1]")
        .expect("the finite layout should bind the rendered scalar owner");

    assert_eq!(owner, solve::scalar_slot_y(0));
    assert!(
        row.iter()
            .any(|op| matches!(op, solve::LinearOp::LoadY { index: 0, .. }))
    );
    assert!(
        row.iter()
            .filter(|op| matches!(
                op,
                solve::LinearOp::Binary {
                    op: solve::BinaryOp::Add,
                    ..
                }
            ))
            .count()
            >= 2,
        "the selected function-output computation and surrounding addition must remain present"
    );
    assert_eq!(problem.continuous.implicit_row_targets[0], Some(owner));
}

#[test]
fn solve_problem_keeps_aggregate_projected_function_residual_projection() {
    let mut dae_model = projected_scalar_dae("y[1,1]");
    dae_model.variables.algebraics.clear();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("y"), source_array_var("y", &[2]));
    dae_model.continuous.equations.clear();
    dae_model
        .continuous
        .equations
        .push(dae::Equation::explicit_with_scalar_count(
            "y",
            projected_pair_call(),
            solve_test_span(),
            "aggregate projected function residual",
            2,
        ));

    let problem = lower_solve_problem_with_solver_len(&dae_model, 2)
        .expect("aggregate array owners must continue through function projection");
    let residual = scalar_program_block_fixture(&problem.continuous.residual);

    assert_eq!(residual.programs.len(), 2);
    assert_eq!(
        problem.continuous.implicit_row_targets,
        vec![
            problem.layout.binding("y[1]"),
            problem.layout.binding("y[2]")
        ]
    );
}

fn projected_pair_call() -> rumoca_core::Expression {
    function_call("Test.projectedPair", vec![source_var("u")])
}

#[test]
fn solve_problem_rejects_unbound_projected_scalar_owners() {
    for (owner, solver_len) in [("missing", 4), ("y[2,2]", 3)] {
        let dae_model = projected_scalar_dae(owner);
        let error = lower_solve_problem_with_solver_len(&dae_model, solver_len)
            .expect_err("missing and truncated owners must fail closed");

        assert!(
            error.is_missing_binding(),
            "unexpected error for {owner}: {error}"
        );
        assert!(error.reason().contains(owner), "unexpected error: {error}");
    }

    let dae_model = projected_scalar_dae("y[3,1]");
    let error = lower_solve_problem_with_solver_len(&dae_model, 4)
        .expect_err("an out-of-range owner must fail closed");

    assert_eq!(error.source_span(), Some(solve_test_span()));
    assert!(
        error.reason().contains("outside dimension bounds"),
        "unexpected error: {error}"
    );
}
