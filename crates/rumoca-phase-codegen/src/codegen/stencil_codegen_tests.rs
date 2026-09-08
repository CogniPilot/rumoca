use super::codegen_test_support::{
    continuous_system_with_derivative,
    render_solve_fixture_template as render_solve_template_with_name, solve_artifacts,
    solve_layout_for_y,
};
use rumoca_ir_solve as solve;

fn tensor_domain(count: usize) -> rumoca_core::StructuredIndexDomain {
    rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::new(0),
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
    let solve_layout = solve_layout_for_y(3);
    let continuous = continuous_system_with_derivative(
        &solve_layout,
        solve::ComputeBlock {
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
                solve::ComputeNode::ScalarPrograms(
                    solve::ScalarProgramBlock::with_source_span(
                        vec![vec![
                            solve::LinearOp::Const { dst: 0, value: 3.0 },
                            solve::LinearOp::StoreOutput { src: 0 },
                        ]],
                        fixture_span()
                            .require_provenance("stencil codegen scalar fixture")
                            .expect("fixture span is source-backed"),
                    )
                    .expect("fixture program is computable"),
                ),
            ],
        },
    );
    let layout = solve::VarLayout::from_parts(indexmap::IndexMap::new(), 3, 0);
    let initialization = solve::InitializationSolveSystem::empty();
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    solve::SolveProblem::construct(
        layout,
        solve_layout,
        continuous,
        initialization,
        discrete,
        events,
        clocks,
    )
    .expect("Solve fixture aggregates satisfy the checked root contract")
}

#[test]
fn scalar_program_plan_exposes_scalarized_stencils_without_render_helpers() {
    let problem = solve_problem_with_stencil_and_scalar_derivative();
    let artifacts = solve_artifacts(&problem);

    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        r#"{% for program in solve_blocks.continuous.derivative_rhs.scalar_plan.programs %}
{% for op in program.ops %}
{% if op.kind == "Const" %}value={{ op.value }}{% elif op.kind == "StoreOutput" %}output={{ op.output_index }}{% endif %}
{% endfor %}
{% endfor %}
"#,
        "StencilDemo",
    )
    .expect("solve template should scalarize AffineStencil derivative nodes");

    assert!(rendered.contains("value=1.0"), "got:\n{rendered}");
    assert!(rendered.contains("value=2.0"), "got:\n{rendered}");
    assert!(rendered.contains("value=3.0"), "got:\n{rendered}");
    assert!(rendered.contains("output=0"), "got:\n{rendered}");
    assert!(rendered.contains("output=1"), "got:\n{rendered}");
    assert!(rendered.contains("output=2"), "got:\n{rendered}");
}
