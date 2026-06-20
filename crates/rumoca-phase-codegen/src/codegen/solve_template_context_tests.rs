use super::*;
use rumoca_ir_solve as solve;

fn builtin_template(target: &str, template: &str) -> &'static str {
    crate::templates::builtin_target(target)
        .and_then(|target| target.template_source(template))
        .expect("built-in target template must exist")
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
    solve::ScalarProgramBlock::with_source_span(rows, fixture_span())
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
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: count as i64,
            step: 1,
        }],
    }
}

fn implicit_problem_with_artifacts() -> (solve::SolveProblem, solve::SolveArtifacts) {
    let row = single_slot_row();
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs =
        solve::ComputeBlock::from_scalar_program_block(scalar_block(vec![row.clone()]));
    problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(scalar_block(vec![row.clone()]));

    let mut artifacts = solve::SolveArtifacts::default();
    artifacts.continuous.implicit_jacobian_v_scalar = scalar_block(vec![row.clone()]);
    artifacts.continuous.full_jacobian_v = scalar_block(vec![row]);
    (problem, artifacts)
}

fn implicit_problem_with_native_residual_map() -> solve::SolveProblem {
    let domain = tensor_domain(3);
    let mut problem = solve::SolveProblem::default();
    problem.continuous.implicit_rhs = solve::ComputeBlock {
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
    problem
}

fn explicit_problem() -> solve::SolveProblem {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs =
        solve::ComputeBlock::from_scalar_program_block(scalar_block(vec![single_slot_row()]));
    problem
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
    assert_eq!(rendered, "1 1 1");

    let mlir = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("mlir", "mlir.mlir.jinja"),
        "ImplicitDemo",
    )
    .expect("mlir template should render optional functions");
    assert!(mlir.contains("func.func @eval_implicit_rhs"));
    assert!(mlir.contains("func.func @eval_jacobian_v"));

    let rendered = render_solve_template_with_name(
        &explicit_problem(),
        &artifacts,
        "{{ solve_implicit_rows | length }} {{ solve_jacobian_rows | length }} {{ solve_full_jacobian_rows | length }}",
        "ExplicitOnlyDemo",
    )
    .expect("implicit JVP rows should be empty without an implicit residual");
    assert_eq!(rendered, "0 0 1");
}

#[test]
fn test_solve_template_context_exposes_native_implicit_rhs_families() {
    let problem = implicit_problem_with_native_residual_map();
    let artifacts = solve::SolveArtifacts::default();
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

#[test]
fn test_serialized_solve_template_context_exposes_optional_rows_as_sequences() {
    let (problem, artifacts) = implicit_problem_with_artifacts();

    let mut solve_json = serde_json::to_value(&problem).expect("serialize solve problem");
    solve_json["artifacts"] = serde_json::to_value(&artifacts).expect("serialize artifacts");
    let dae_json = serde_json::json!({
        "__ir_kind": "solve",
        "solve": solve_json,
    });
    let rendered = render_template_with_dae_json(
        &dae_json,
        "{{ solve_implicit_rows | length }} {{ solve_jacobian_rows | length }} {{ solve_full_jacobian_rows | length }}",
    )
    .expect("serialized solve context should render direct optional row sequences");
    assert_eq!(rendered, "1 1 1");

    let mut solve_json = serde_json::to_value(explicit_problem()).expect("serialize solve problem");
    solve_json["artifacts"] = serde_json::to_value(&artifacts).expect("serialize artifacts");
    let dae_json = serde_json::json!({
        "__ir_kind": "solve",
        "solve": solve_json,
    });
    let rendered = render_template_with_dae_json(
        &dae_json,
        "{{ solve_implicit_rows | length }} {{ solve_jacobian_rows | length }} {{ solve_full_jacobian_rows | length }}",
    )
    .expect("serialized implicit JVP rows should be empty without implicit residual");
    assert_eq!(rendered, "0 0 1");
}
