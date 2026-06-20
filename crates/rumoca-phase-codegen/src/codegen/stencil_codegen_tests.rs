use super::*;
use rumoca_ir_solve as solve;

fn tensor_domain(count: usize) -> rumoca_core::StructuredIndexDomain {
    rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: count as i64,
            step: 1,
        }],
    }
}

fn fixture_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("stencil_codegen_fixture.mo"),
        1,
        2,
    )
}

fn solve_problem_with_stencil_and_scalar_derivative() -> solve::SolveProblem {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs = solve::ComputeBlock {
        nodes: vec![
            solve::ComputeNode::AffineStencil {
                domain: tensor_domain(2),
                output_map: solve::TensorOutputMap::dense_contiguous(0, &tensor_domain(2))
                    .expect("valid dense output map"),
                base_ops: vec![
                    solve::LinearOp::Const { dst: 0, value: 1.0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
                load_strides: Vec::new(),
                const_strides: vec![solve::AffineStencilConstStride {
                    op_position: 0,
                    terms: vec![solve::AffineStencilConstStrideTerm {
                        dimension: 0,
                        stride: 1.0,
                    }],
                }],
                metadata: Default::default(),
                span: fixture_span(),
            },
            solve::ComputeNode::ScalarPrograms(solve::ScalarProgramBlock::with_source_span(
                vec![vec![
                    solve::LinearOp::Const { dst: 0, value: 3.0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ]],
                fixture_span(),
            )),
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
