use super::*;
use rumoca_ir_solve as solve;

fn solve_problem_with_stencil_and_scalar_derivative() -> solve::SolveProblem {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs = solve::ComputeBlock {
        nodes: vec![
            solve::ComputeNode::AffineStencil {
                count: 2,
                domain: solve::AffineStencilDomain {
                    index_names: vec!["i".to_string()],
                    iterations: vec![
                        solve::AffineStencilIteration {
                            index_values: vec![1],
                        },
                        solve::AffineStencilIteration {
                            index_values: vec![2],
                        },
                    ],
                },
                base_ops: vec![
                    solve::LinearOp::Const { dst: 0, value: 1.0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
                load_strides: Vec::new(),
                const_strides: vec![solve::AffineStencilConstStride {
                    op_position: 0,
                    stride: 1.0,
                }],
                metadata: Default::default(),
                span: Default::default(),
            },
            solve::ComputeNode::ScalarPrograms(solve::ScalarProgramBlock::new(vec![vec![
                solve::LinearOp::Const { dst: 0, value: 3.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]])),
        ],
    };
    problem
}

#[test]
fn test_solve_derivative_nodes_scalarize_affine_stencils_for_c_templates() {
    let problem = solve_problem_with_stencil_and_scalar_derivative();
    let artifacts = solve::SolveArtifacts::default();

    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        r#"
{% set cfg = {"time": "m->time", "y": "Y({})", "p": "P({})"} %}
{% set ns = namespace(offset=0) %}
{% for node in solve_derivative_nodes %}
{% if node.ScalarPrograms is defined %}
{% for row in node.ScalarPrograms.programs %}
{{ ns.offset }}={{ render_solve_row_c(row, cfg) }};
{% set ns.offset = ns.offset + 1 %}
{% endfor %}
{% endif %}
{% endfor %}
"#,
        "StencilDemo",
    )
    .expect("solve template should scalarize AffineStencil derivative nodes");

    assert!(rendered.contains("0=1.0;"), "got:\n{rendered}");
    assert!(rendered.contains("1=2.0;"), "got:\n{rendered}");
    assert!(rendered.contains("2=3.0;"), "got:\n{rendered}");
}
