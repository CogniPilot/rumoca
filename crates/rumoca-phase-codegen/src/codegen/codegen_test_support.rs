use rumoca_ir_solve as solve;

pub(super) fn builtin_template(target: &str, template: &str) -> &'static str {
    crate::templates::builtin_target(target)
        .and_then(|target| target.template_source(template))
        .expect("built-in target template must exist")
}

fn fixture_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("codegen_solve_fixture.mo"),
        1,
        2,
    )
}

fn full_pattern(size: usize) -> solve::StructuralPattern {
    let span = fixture_span();
    let provenance =
        solve::PatternProvenance::derived(solve::PatternDerivation::TensorOperand, span)
            .expect("fixture provenance");
    let dependencies = (0..size).map(|_| (0..size).collect()).collect::<Vec<_>>();
    solve::StructuralPattern::from_row_dependencies(size, size, &dependencies, provenance)
        .expect("fixture pattern")
}

pub(super) fn solve_problem_with_two_by_two_linsolve_derivative() -> solve::SolveProblem {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::LinSolve {
            setup_ops: vec![
                solve::LinearOp::Const { dst: 0, value: 2.0 },
                solve::LinearOp::Const { dst: 1, value: 0.0 },
                solve::LinearOp::Const { dst: 2, value: 0.0 },
                solve::LinearOp::Const { dst: 3, value: 4.0 },
                solve::LinearOp::Const { dst: 4, value: 8.0 },
                solve::LinearOp::Const {
                    dst: 5,
                    value: 20.0,
                },
            ],
            matrix_start: 0,
            rhs_start: 4,
            n: 2,
            next_reg: 6,
            matrix_pattern: full_pattern(2),
            metadata: Default::default(),
            span: fixture_span(),
        }],
    };
    problem
}
