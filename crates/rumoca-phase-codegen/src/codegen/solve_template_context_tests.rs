use super::codegen_test_support::{
    ContinuousSystemFixture, checked_continuous_system, continuous_system_with_derivative,
    render_solve_fixture_template as render_solve_template_with_name, solve_artifacts,
    solve_layout_for_y, solve_layout_with_names,
};
use rumoca_ir_solve as solve;

fn builtin_template(target: &str, template: &str) -> &'static str {
    crate::templates::builtin_target(target)
        .and_then(|target| target.template_source(template))
        .expect("built-in target template must exist")
}

#[test]
fn exact_once_derivative_templates_have_no_semantic_zero_prefill() {
    let mlir = builtin_template("mlir", "mlir.mlir.jinja");
    assert!(!mlir.contains("%drv_zero"));
    assert!(!mlir.contains("scf.for %drv_zero_i"));

    let cuda = builtin_template("cuda-ode", "model_ode.cu.jinja");
    assert!(!cuda.contains("batch_out[i] = 0.0"));

    let c_fixture = include_str!("test_fixtures/solve_c_spelling.c.jinja");
    assert!(!c_fixture.contains("__out[i] = 0.0"));

    let rust_fixed = builtin_template("rust-fixed-ode", "model_fixed_ode.rs.jinja");
    assert!(!rust_fixed.contains("out.fill(0.0)"));
}

fn single_slot_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn fixture_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("solve_template_context_fixture.mo"),
        1,
        2,
    )
}

fn scalar_block(rows: Vec<Vec<solve::LinearOp>>) -> solve::ScalarProgramBlock {
    solve::ScalarProgramBlock::with_source_span(
        rows,
        fixture_span()
            .require_provenance("solve-template fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("fixture program is computable")
}

fn scalar_block_with_output_indices(
    rows: Vec<Vec<solve::LinearOp>>,
    output_indices: Vec<usize>,
) -> solve::ScalarProgramBlock {
    let row_count = rows.len();
    solve::ScalarProgramBlock::with_output_indices(
        rows,
        vec![fixture_span(); row_count],
        output_indices,
    )
    .expect("scalar fixture metadata should match output indices")
}

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

fn implicit_problem_with_artifacts() -> (solve::SolveProblem, solve::SolveArtifactInputs) {
    let row = single_slot_row();
    let residual = solve::ComputeBlock::from_scalar_program_block(scalar_block(vec![row]));
    let solve_layout = solve_layout_with_names(
        solve::SolveLayout {
            algebraic_scalar_count: 1,
            ..solve::SolveLayout::default()
        },
        ["x[1]".to_string()],
    );
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = checked_continuous_system(
        &solve_layout,
        &discrete,
        &events,
        &clocks,
        ContinuousSystemFixture {
            implicit_rhs: residual.clone(),
            implicit_row_targets: vec![Some(solve::ScalarSlot::Y { index: 0 })],
            algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: vec![0],
                    y_indices: vec![0],
                    tearing: None,
                }],
            },
            residual,
            manifold: (
                solve::ComputeBlock::default(),
                solve::AlgebraicProjectionPlan::default(),
            ),
            derivative_rhs: solve::ComputeBlock::default(),
        },
    );
    let layout = solve::VarLayout::from_parts(indexmap::IndexMap::new(), 1, 0);
    let initialization = solve::InitializationSolveSystem::empty();
    let problem = solve::SolveProblem::construct(
        layout,
        solve_layout,
        continuous,
        initialization,
        discrete,
        events,
        clocks,
    )
    .expect("Solve fixture aggregates satisfy the checked root contract");

    let artifacts = solve_artifacts(&problem);
    (problem, artifacts)
}

fn implicit_problem_with_native_residual_map() -> solve::SolveProblem {
    let domain = tensor_domain(3);
    let solve_layout = solve_layout_with_names(
        solve::SolveLayout {
            algebraic_scalar_count: 7,
            ..solve::SolveLayout::default()
        },
        (0..7).map(|index| format!("x[{index}]")),
    );
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let implicit_rhs = solve::ComputeBlock {
        nodes: vec![
            solve::ComputeNode::ScalarPrograms(scalar_block(vec![single_slot_row()])),
            solve::ComputeNode::Map {
                domain: domain.clone(),
                output_map: solve::TensorOutputMap::dense_contiguous(1, &domain)
                    .expect("valid dense output map"),
                base_ops: vec![
                    solve::LinearOp::LoadY { dst: 0, index: 1 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
                load_strides: vec![solve::AffineStencilLoadStride {
                    op_position: 0,
                    terms: vec![solve::AffineStencilIndexStrideTerm {
                        dimension: 0,
                        stride: 1,
                    }],
                }],
                const_strides: Vec::new(),
                metadata: solve::TensorNodeMetadata::default(),
                span: fixture_span(),
            },
            solve::ComputeNode::ScalarPrograms(scalar_block_with_output_indices(
                vec![
                    vec![
                        solve::LinearOp::Const { dst: 0, value: 0.0 },
                        solve::LinearOp::StoreOutput { src: 0 },
                    ],
                    vec![
                        solve::LinearOp::Const { dst: 0, value: 0.0 },
                        solve::LinearOp::StoreOutput { src: 0 },
                    ],
                    vec![
                        solve::LinearOp::Const { dst: 0, value: 0.0 },
                        solve::LinearOp::StoreOutput { src: 0 },
                    ],
                ],
                vec![4, 5, 6],
            )),
        ],
    };
    let continuous = checked_continuous_system(
        &solve_layout,
        &discrete,
        &events,
        &clocks,
        ContinuousSystemFixture {
            implicit_rhs: implicit_rhs.clone(),
            implicit_row_targets: (0..7)
                .map(|index| Some(solve::ScalarSlot::Y { index }))
                .collect(),
            algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: (0..7).collect(),
                    y_indices: (0..7).collect(),
                    tearing: None,
                }],
            },
            residual: implicit_rhs,
            manifold: (
                solve::ComputeBlock::default(),
                solve::AlgebraicProjectionPlan::default(),
            ),
            derivative_rhs: solve::ComputeBlock::default(),
        },
    );
    let layout = solve::VarLayout::from_parts(indexmap::IndexMap::new(), 7, 0);
    let initialization = solve::InitializationSolveSystem::empty();
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

fn explicit_problem() -> solve::SolveProblem {
    let solve_layout = solve_layout_for_y(1);
    let continuous = continuous_system_with_derivative(
        &solve_layout,
        solve::ComputeBlock::from_scalar_program_block(scalar_block(vec![single_slot_row()])),
    );
    let layout = solve::VarLayout::from_parts(indexmap::IndexMap::new(), 1, 0);
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
fn test_solve_template_context_exposes_optional_rows_as_sequences() {
    let (problem, artifacts) = implicit_problem_with_artifacts();

    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        "{{ solve_implicit_rows | length }} {{ solve_jacobian_rows | length }} {{ solve_full_jacobian_rows | length }}",
        "ImplicitDemo",
    )
    .expect("solve template should render direct optional row sequences");
    assert_eq!(rendered, "1 1 0");

    let mlir = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("mlir", "mlir.mlir.jinja"),
        "ImplicitDemo",
    )
    .expect("mlir template should render optional functions");
    assert!(mlir.contains("func.func @eval_implicit_rhs"));
    assert!(mlir.contains("func.func @eval_jacobian_v"));

    let explicit_problem = explicit_problem();
    let explicit_artifacts = solve_artifacts(&explicit_problem);
    let rendered = render_solve_template_with_name(
        &explicit_problem,
        &explicit_artifacts,
        "{{ solve_implicit_rows | length }} {{ solve_jacobian_rows | length }} {{ solve_full_jacobian_rows | length }}",
        "ExplicitOnlyDemo",
    )
    .expect("implicit JVP rows should be empty without an implicit residual");
    assert_eq!(rendered, "0 0 1");
}

#[test]
fn test_solve_template_context_exposes_native_implicit_rhs_families() {
    let problem = implicit_problem_with_native_residual_map();
    let artifacts = solve_artifacts(&problem);
    let template = r#"
{%- set block = solve_blocks.continuous.implicit_rhs -%}
{%- set st = block.native_families[0] -%}
{{ block.nodes | length }}
{{ block.tensor_node_count }}
{{ block.map_family_count }}
{{ block.scalar_fallback_rows | length }}
{{ block.scalar_programs.output_indices | join(",") }}
{{ st.kind }} {{ st.output_offset }} {{ st.count }}
{{ render_solve_native_family_output_index_wgsl(st) }}
{{ render_solve_native_family_wgsl(st, {"time": "t", "y": "y[{}]", "p": "p[{}]"}) }}
"#;

    let rendered = render_solve_template_with_name(&problem, &artifacts, template, "ImplicitMap")
        .expect("implicit rhs native-family context should render");

    assert!(rendered.contains("3\n1\n1\n4\n0,1,2,3,4,5,6"));
    assert!(rendered.contains("map 1 3"));
    assert!(rendered.contains("u32(i32(1u) + i32((r) % 3u) * 1)"));
    assert!(rendered.contains("y[u32(i32(1u) + i32((r) % 3u) * 1)]"));
}
