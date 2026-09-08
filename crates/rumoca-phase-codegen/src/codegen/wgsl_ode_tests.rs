//! WGSL ODE-target render tests (split from `codegen_tests` to stay
//! under the SPEC_0021 file-size limit).

use super::codegen_test_support::{
    ContinuousSystemFixture, builtin_template, checked_continuous_system,
    continuous_system_with_derivative, explicit_ode_layout,
    render_solve_fixture_template as render_solve_template_with_name, solve_artifacts,
    solve_layout_for_y, solve_layout_with_names, solve_problem_with_two_by_two_linsolve_derivative,
};
use rumoca_ir_solve as solve;

fn validate_wgsl(source: &str) {
    let module = naga::front::wgsl::parse_str(source)
        .unwrap_or_else(|error| panic!("generated WGSL must parse: {error}\n{source}"));
    naga::valid::Validator::new(
        naga::valid::ValidationFlags::all(),
        naga::valid::Capabilities::empty(),
    )
    .validate(&module)
    .unwrap_or_else(|error| panic!("generated WGSL must validate: {error}\n{source}"));
}

fn fixture_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("wgsl_ode_fixture.mo"),
        1,
        2,
    )
}

fn scalar_block(rows: Vec<Vec<solve::LinearOp>>) -> solve::ScalarProgramBlock {
    solve::ScalarProgramBlock::with_source_span(
        rows,
        fixture_span()
            .require_provenance("WGSL ODE fixture")
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

fn solve_problem_with_scalar_derivative_rows() -> solve::SolveProblem {
    let solve_layout = explicit_ode_layout(2, 1);
    let continuous = continuous_system_with_derivative(
        &solve_layout,
        solve::ComputeBlock {
            nodes: vec![solve::ComputeNode::ScalarPrograms(scalar_block(vec![
                // der0 = -p[0] * y[0]
                vec![
                    solve::LinearOp::LoadP { dst: 0, index: 0 },
                    solve::LinearOp::LoadY { dst: 1, index: 0 },
                    solve::LinearOp::Binary {
                        dst: 2,
                        op: solve::BinaryOp::Mul,
                        lhs: 0,
                        rhs: 1,
                    },
                    solve::LinearOp::Unary {
                        dst: 3,
                        op: solve::UnaryOp::Neg,
                        arg: 2,
                    },
                    solve::LinearOp::StoreOutput { src: 3 },
                ],
                // der1 = if y[0] > 2 then 1 else pow(y[0], 3)
                vec![
                    solve::LinearOp::LoadY { dst: 0, index: 0 },
                    solve::LinearOp::Const { dst: 1, value: 2.0 },
                    solve::LinearOp::Compare {
                        dst: 2,
                        op: solve::CompareOp::Gt,
                        lhs: 0,
                        rhs: 1,
                    },
                    solve::LinearOp::Const { dst: 3, value: 1.0 },
                    solve::LinearOp::Const { dst: 4, value: 3.0 },
                    solve::LinearOp::Binary {
                        dst: 5,
                        op: solve::BinaryOp::Pow,
                        lhs: 0,
                        rhs: 4,
                    },
                    solve::LinearOp::Select {
                        dst: 6,
                        cond: 2,
                        if_true: 3,
                        if_false: 5,
                    },
                    solve::LinearOp::StoreOutput { src: 6 },
                ],
            ]))],
        },
    );
    let layout = solve::VarLayout::from_parts(indexmap::IndexMap::new(), 2, 1);
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

fn solve_problem_with_affine_stencil_derivative() -> solve::SolveProblem {
    let solve_layout = explicit_ode_layout(8, 1);
    let continuous = continuous_system_with_derivative(
        &solve_layout,
        solve::ComputeBlock {
            nodes: vec![solve::ComputeNode::AffineStencil {
                domain: tensor_domain(8),
                output_map: solve::TensorOutputMap::dense_contiguous(0, &tensor_domain(8))
                    .expect("valid dense output map"),
                base_ops: vec![
                    solve::LinearOp::LoadP { dst: 0, index: 0 },
                    solve::LinearOp::LoadY { dst: 1, index: 0 },
                    solve::LinearOp::Binary {
                        dst: 2,
                        op: solve::BinaryOp::Mul,
                        lhs: 0,
                        rhs: 1,
                    },
                    solve::LinearOp::Unary {
                        dst: 3,
                        op: solve::UnaryOp::Neg,
                        arg: 2,
                    },
                    solve::LinearOp::StoreOutput { src: 3 },
                ],
                load_strides: vec![solve::AffineStencilLoadStride {
                    op_position: 1,
                    terms: vec![solve::AffineStencilIndexStrideTerm {
                        dimension: 0,
                        stride: 1,
                    }],
                }],
                const_strides: Vec::new(),
                metadata: solve::TensorNodeMetadata::default(),
                span: fixture_span(),
            }],
        },
    );
    let layout = solve::VarLayout::from_parts(indexmap::IndexMap::new(), 8, 1);
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

fn solve_problem_with_map_derivative() -> solve::SolveProblem {
    solve_problem_with_map_derivative_at(1)
}

fn solve_problem_with_map_derivative_at(op_position: usize) -> solve::SolveProblem {
    try_solve_problem_with_map_derivative_at(op_position)
        .expect("valid map fixture satisfies the checked root contract")
}

type MapFixtureResult = Result<solve::SolveProblem, Box<solve::SolveProblemShapeContractError>>;

fn try_solve_problem_with_map_derivative_at(op_position: usize) -> MapFixtureResult {
    let solve_layout = explicit_ode_layout(8, 1);
    let continuous = continuous_system_with_derivative(
        &solve_layout,
        solve::ComputeBlock {
            nodes: vec![solve::ComputeNode::Map {
                domain: tensor_domain(8),
                output_map: solve::TensorOutputMap::dense_contiguous(0, &tensor_domain(8))
                    .expect("valid dense output map"),
                base_ops: vec![
                    solve::LinearOp::LoadP { dst: 0, index: 0 },
                    solve::LinearOp::LoadY { dst: 1, index: 0 },
                    solve::LinearOp::Binary {
                        dst: 2,
                        op: solve::BinaryOp::Mul,
                        lhs: 0,
                        rhs: 1,
                    },
                    solve::LinearOp::StoreOutput { src: 2 },
                ],
                load_strides: vec![solve::AffineStencilLoadStride {
                    op_position,
                    terms: vec![solve::AffineStencilIndexStrideTerm {
                        dimension: 0,
                        stride: 1,
                    }],
                }],
                const_strides: Vec::new(),
                metadata: solve::TensorNodeMetadata::default(),
                span: fixture_span(),
            }],
        },
    );
    let layout = solve::VarLayout::from_parts(indexmap::IndexMap::new(), 8, 1);
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
    .map_err(Box::new)
}

fn solve_problem_with_mixed_native_and_scalar_derivative() -> solve::SolveProblem {
    let domain = tensor_domain(5);
    let solve_layout = explicit_ode_layout(6, 0);
    let continuous = continuous_system_with_derivative(
        &solve_layout,
        solve::ComputeBlock {
            nodes: vec![
                solve::ComputeNode::Map {
                    domain: domain.clone(),
                    output_map: solve::TensorOutputMap::dense_contiguous(0, &domain)
                        .expect("valid dense output map"),
                    base_ops: vec![
                        solve::LinearOp::LoadY { dst: 0, index: 0 },
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
                    vec![vec![
                        solve::LinearOp::Const { dst: 0, value: 9.0 },
                        solve::LinearOp::StoreOutput { src: 0 },
                    ]],
                    vec![5],
                )),
            ],
        },
    );
    let layout = solve::VarLayout::from_parts(indexmap::IndexMap::new(), 6, 0);
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
fn test_wgsl_ode_bubbles_invalid_tensor_output_map_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("bad_tensor_output.mo"),
        11,
        29,
    );
    let solve_layout = solve_layout_for_y(2);
    let continuous = continuous_system_with_derivative(
        &solve_layout,
        solve::ComputeBlock {
            nodes: vec![solve::ComputeNode::Map {
                domain: tensor_domain(2),
                output_map: solve::TensorOutputMap {
                    start: 0,
                    strides: vec![solve::AffineStencilIndexStrideTerm {
                        dimension: 0,
                        stride: -1,
                    }],
                },
                base_ops: vec![
                    solve::LinearOp::Const { dst: 0, value: 1.0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
                load_strides: Vec::new(),
                const_strides: Vec::new(),
                metadata: solve::TensorNodeMetadata::default(),
                span,
            }],
        },
    );
    let layout = solve::VarLayout::from_parts(indexmap::IndexMap::new(), 2, 0);
    let initialization = solve::InitializationSolveSystem::empty();
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let error = solve::SolveProblem::construct(
        layout,
        solve_layout,
        continuous,
        initialization,
        discrete,
        events,
        clocks,
    )
    .expect_err("invalid tensor output map should fail during root construction");
    assert_eq!(error.source_span(), Some(span));
    assert!(
        error
            .to_string()
            .contains("output map produced negative output index -1"),
        "error should explain the invalid output map: {error}"
    );
}

fn solve_problem_with_native_implicit_rhs_map() -> solve::SolveProblem {
    let domain = tensor_domain(3);
    let derivative_rhs = solve::ComputeBlock::from_scalar_program_block(scalar_block(vec![vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]]));
    let solve_layout = solve_layout_with_names(
        solve::SolveLayout {
            state_scalar_count: 1,
            algebraic_scalar_count: 3,
            ..solve::SolveLayout::default()
        },
        (0..4).map(|index| format!("x[{index}]")),
    );
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let implicit_rhs = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::Map {
            domain: domain.clone(),
            output_map: solve::TensorOutputMap::dense_contiguous(0, &domain)
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
        }],
    };
    let continuous = checked_continuous_system(
        &solve_layout,
        &discrete,
        &events,
        &clocks,
        ContinuousSystemFixture {
            implicit_rhs: implicit_rhs.clone(),
            implicit_row_targets: (0..3)
                .map(|index| Some(solve::ScalarSlot::Y { index: index + 1 }))
                .collect(),
            algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: (0..3).collect(),
                    y_indices: (1..4).collect(),
                    tearing: None,
                }],
            },
            residual: implicit_rhs,
            manifold: (
                solve::ComputeBlock::default(),
                solve::AlgebraicProjectionPlan::default(),
            ),
            derivative_rhs,
        },
    );
    let layout = solve::VarLayout::from_parts(indexmap::IndexMap::new(), 4, 1);
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

fn assert_kernel_entry_kind(
    kernel: &serde_json::Value,
    entry: &str,
    output_kind: WgslKernelOutputKind,
) {
    assert_eq!(kernel["entry"], entry);
    match output_kind {
        WgslKernelOutputKind::Native => {
            assert!(
                entry.contains("_map") || entry.contains("_stencil"),
                "native kernel entry should be map/stencil, got {entry}"
            );
            assert!(
                kernel.get("output_map").is_some(),
                "native kernel {entry} should expose output_map"
            );
            assert!(
                kernel.get("start_slot").is_none(),
                "native kernel {entry} should not expose scalar start_slot"
            );
            assert!(
                kernel.get("output_indices").is_none(),
                "native kernel {entry} should not expose scalar output_indices"
            );
        }
        WgslKernelOutputKind::ScalarChunk => {
            assert!(
                entry.contains("_chunk"),
                "scalar kernel entry should be chunk, got {entry}"
            );
            assert!(
                kernel.get("output_map").is_none(),
                "scalar kernel {entry} should not expose tensor output_map"
            );
            assert!(
                kernel.get("start_slot").is_some(),
                "scalar kernel {entry} should expose start_slot"
            );
            assert!(
                kernel.get("output_indices").is_some(),
                "scalar kernel {entry} should expose output_indices"
            );
        }
    }
}

#[derive(Clone, Copy)]
enum WgslKernelOutputKind {
    Native,
    ScalarChunk,
}

#[test]
fn test_wgsl_ode_builtin_target_renders_row_parallel_kernel() {
    let problem = solve_problem_with_scalar_derivative_rows();
    let artifacts = solve_artifacts(&problem);
    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("wgsl-ode", "model_ode.wgsl.jinja"),
        "WgslDemo",
    )
    .expect("wgsl-ode template should render scalar rows");

    assert!(rendered.contains("fn derivative_rhs_chunk0"));
    assert!(rendered.contains("const ROWS: u32 = 2u;"));
    assert!(rendered.contains("// scalar_chunks=1"));
    assert_eq!(rendered.matches("// workgroup_size=64").count(), 1);
    assert_eq!(rendered.matches("// chunk_size=64").count(), 1);
    assert!(rendered.contains("// kernel_count=1"));
    // der0: negated product of p[0] and y[0]
    assert!(rendered.contains("out[0] = (-(((p[0]) * (y[0]))));"));
    // der1: select-based conditional with float-forced constants and pow
    assert!(rendered.contains("select(0.0, 1.0, (y[0]) > (2.0))"));
    assert!(rendered.contains("pow(y[0], 3.0)"));
    assert!(
        !rendered.contains(" ? "),
        "WGSL output must not contain C ternaries: {rendered}"
    );
    validate_wgsl(&rendered);
}

#[test]
fn test_wgsl_ode_builtin_target_renders_native_stencil_kernel() {
    let problem = solve_problem_with_affine_stencil_derivative();
    let artifacts = solve_artifacts(&problem);
    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("wgsl-ode", "model_ode.wgsl.jinja"),
        "StencilDemo",
    )
    .expect("wgsl-ode template should render native stencil node");

    assert!(rendered.contains("fn derivative_rhs_stencil0"));
    assert!(rendered.contains("const ROWS: u32 = 8u;"));
    assert!(rendered.contains(
        "out[u32(i32(0u) + i32((r) % 8u) * 1)] = (-(((p[0]) * (y[u32(i32(0u) + i32((r) % 8u) * 1)]))));"
    ));
    assert!(
        !rendered.contains("fn derivative_rhs_chunk0"),
        "pure stencil block should not emit residual chunk: {rendered}"
    );
    validate_wgsl(&rendered);
}

#[test]
fn test_wgsl_ode_builtin_target_renders_native_map_kernel() {
    let problem = solve_problem_with_map_derivative();
    let artifacts = solve_artifacts(&problem);
    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("wgsl-ode", "model_ode.wgsl.jinja"),
        "MapDemo",
    )
    .expect("wgsl-ode template should render native map node");

    assert!(rendered.contains("fn derivative_rhs_map0"));
    assert!(rendered.contains("const ROWS: u32 = 8u;"));
    assert!(rendered.contains(
        "out[u32(i32(0u) + i32((r) % 8u) * 1)] = ((p[0]) * (y[u32(i32(0u) + i32((r) % 8u) * 1)]));"
    ));
    assert!(
        !rendered.contains("fn derivative_rhs_chunk0"),
        "pure map block should not emit residual chunk: {rendered}"
    );
    validate_wgsl(&rendered);
}

#[test]
fn test_wgsl_ode_builtin_target_reports_invalid_native_stride_position() {
    let error = try_solve_problem_with_map_derivative_at(99)
        .expect_err("invalid native stride metadata should fail during root construction");

    assert!(
        error
            .to_string()
            .contains("load stride at op 99 of 4 targets missing op"),
        "error should explain the invalid native-family stride position: {error}"
    );
}

#[test]
fn test_wgsl_ode_builtin_target_rejects_implicit_rhs() {
    let problem = solve_problem_with_native_implicit_rhs_map();
    let artifacts = solve_artifacts(&problem);
    let error = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("wgsl-ode", "model_ode.wgsl.jinja"),
        "ImplicitMapDemo",
    )
    .expect_err("wgsl-ode must reject implicit equation rows");
    assert!(
        error.to_string().contains("residual_equations"),
        "rejection must name the unsupported semantic capability: {error}"
    );
}

#[test]
fn test_wgsl_ode_builtin_target_declines_linear_solve_component() {
    let problem = solve_problem_with_two_by_two_linsolve_derivative();
    let artifacts = solve_artifacts(&problem);
    let error = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("wgsl-ode", "model_ode.wgsl.jinja"),
        "TensorDemo",
    )
    .expect_err("wgsl-ode must decline linear-solve operations");

    assert!(
        error.to_string().contains("LinearSolveComponent"),
        "decline should name the unsupported op: {error}"
    );
}

#[test]
fn test_wgsl_ode_layout_manifest_renders_bindings() {
    let problem = solve_problem_with_scalar_derivative_rows();
    let artifacts = solve_artifacts(&problem);
    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("wgsl-ode", "model_layout.json.jinja"),
        "WgslDemo",
    )
    .expect("wgsl-ode layout manifest should render");

    let parsed: serde_json::Value =
        serde_json::from_str(&rendered).expect("layout manifest must be valid JSON");
    assert_eq!(parsed["model"], "WgslDemo");
    assert!(
        parsed.get("kernel_prefix").is_none(),
        "layout manifest should not expose stale scalar-only kernel_prefix metadata"
    );
    assert_eq!(
        parsed["entry_prefixes"],
        serde_json::json!({
            "native": ["derivative_rhs_map", "derivative_rhs_stencil"],
            "scalar": "derivative_rhs_chunk",
        })
    );
    assert_eq!(parsed["chunk_size"], parsed["workgroup_size"]);
    assert_eq!(parsed["chunks"], 1);
    assert_eq!(parsed["kernel_count"], 1);
    assert_eq!(parsed["workgroup_total"], 1);
    assert_eq!(parsed["rows"], 2);
    assert_eq!(parsed["tensor_nodes"], 0);
    assert_eq!(parsed["tensor_families"], 0);
    assert_eq!(parsed["map_families"], 0);
    assert_eq!(parsed["stencil_families"], 0);
    assert_eq!(parsed["scalar_fallback_rows"], 2);
    assert_kernel_entry_kind(
        &parsed["kernels"][0],
        "derivative_rhs_chunk0",
        WgslKernelOutputKind::ScalarChunk,
    );
    assert_eq!(parsed["kernels"][0]["workgroup_size"], 64);
    assert_eq!(parsed["kernels"][0]["workgroups"], 1);
    assert_eq!(parsed["kernels"][0]["start_slot"], 0);
    assert_eq!(
        parsed["kernels"][0]["output_indices"],
        serde_json::json!([0, 1])
    );
    assert!(parsed.get("implicit_rhs").is_none());
}

#[test]
fn test_wgsl_ode_layout_manifest_derives_chunks_from_workgroup_size() {
    let problem = solve_problem_with_scalar_derivative_rows();
    let artifacts = solve_artifacts(&problem);
    let template = builtin_template("wgsl-ode", "model_layout.json.jinja").replace(
        "{%- set workgroup_size = 64 %}",
        "{%- set workgroup_size = 1 %}",
    );
    let rendered =
        render_solve_template_with_name(&problem, &artifacts, &template, "WgslChunkDemo")
            .expect("wgsl-ode layout manifest should render with alternate workgroup size");

    let parsed: serde_json::Value =
        serde_json::from_str(&rendered).expect("layout manifest must be valid JSON");
    assert_eq!(parsed["workgroup_size"], 1);
    assert_eq!(parsed["chunk_size"], 1);
    assert_eq!(parsed["chunks"], 2);
    assert_eq!(parsed["kernel_count"], 2);
    assert_eq!(parsed["workgroup_total"], 2);
    assert_eq!(parsed["kernels"][0]["rows"], 1);
    assert_eq!(parsed["kernels"][0]["workgroups"], 1);
    assert_kernel_entry_kind(
        &parsed["kernels"][0],
        "derivative_rhs_chunk0",
        WgslKernelOutputKind::ScalarChunk,
    );
    assert_eq!(parsed["kernels"][0]["start_slot"], 0);
    assert_eq!(
        parsed["kernels"][0]["output_indices"],
        serde_json::json!([0])
    );
    assert_eq!(parsed["kernels"][1]["rows"], 1);
    assert_eq!(parsed["kernels"][1]["workgroups"], 1);
    assert_kernel_entry_kind(
        &parsed["kernels"][1],
        "derivative_rhs_chunk1",
        WgslKernelOutputKind::ScalarChunk,
    );
    assert_eq!(parsed["kernels"][1]["start_slot"], 1);
    assert_eq!(
        parsed["kernels"][1]["output_indices"],
        serde_json::json!([1])
    );
}

#[test]
fn test_wgsl_ode_layout_manifest_reports_native_stencil_kernel() {
    let problem = solve_problem_with_affine_stencil_derivative();
    let artifacts = solve_artifacts(&problem);
    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("wgsl-ode", "model_layout.json.jinja"),
        "StencilDemo",
    )
    .expect("wgsl-ode layout manifest should render native stencil node");

    let parsed: serde_json::Value =
        serde_json::from_str(&rendered).expect("layout manifest must be valid JSON");
    assert_eq!(parsed["rows"], 8);
    assert_eq!(parsed["tensor_nodes"], 1);
    assert_eq!(parsed["tensor_families"], 1);
    assert_eq!(parsed["map_families"], 0);
    assert_eq!(parsed["stencil_families"], 1);
    assert_eq!(parsed["scalar_fallback_rows"], 0);
    assert_eq!(parsed["chunks"], 0);
    assert_eq!(parsed["kernel_count"], 1);
    assert_eq!(parsed["workgroup_total"], 1);
    assert_kernel_entry_kind(
        &parsed["kernels"][0],
        "derivative_rhs_stencil0",
        WgslKernelOutputKind::Native,
    );
    assert_eq!(parsed["kernels"][0]["rows"], 8);
    assert_eq!(parsed["kernels"][0]["output_map"]["start"], 0);
    assert_eq!(
        parsed["kernels"][0]["output_map"]["strides"],
        serde_json::json!([{ "dimension": 0, "stride": 1 }])
    );
    assert_eq!(parsed["kernels"][0]["workgroup_size"], 64);
    assert_eq!(parsed["kernels"][0]["workgroups"], 1);
    assert_eq!(parsed["native_families"][0]["kind"], "stencil");
    assert_eq!(parsed["native_families"][0]["output_map"]["start"], 0);
    assert_eq!(
        parsed["native_families"][0]["output_map"]["strides"],
        serde_json::json!([{ "dimension": 0, "stride": 1 }])
    );
    assert_eq!(parsed["native_families"][0]["rows"], 8);
    assert_eq!(
        parsed["native_families"][0]["domain_shape"],
        serde_json::json!([8])
    );
}

#[test]
fn test_wgsl_ode_layout_manifest_reports_mixed_native_and_scalar_schedule() {
    let problem = solve_problem_with_mixed_native_and_scalar_derivative();
    let artifacts = solve_artifacts(&problem);
    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("wgsl-ode", "model_layout.json.jinja"),
        "MixedScheduleDemo",
    )
    .expect("wgsl-ode layout manifest should render mixed native/scalar schedule");

    let parsed: serde_json::Value =
        serde_json::from_str(&rendered).expect("layout manifest must be valid JSON");
    assert_eq!(parsed["rows"], 6);
    assert_eq!(parsed["tensor_families"], 1);
    assert_eq!(parsed["scalar_fallback_rows"], 1);
    assert_eq!(parsed["chunks"], 1);
    assert_eq!(parsed["kernel_count"], 2);
    assert_eq!(parsed["workgroup_total"], 2);
    assert_kernel_entry_kind(
        &parsed["kernels"][0],
        "derivative_rhs_map0",
        WgslKernelOutputKind::Native,
    );
    assert_eq!(parsed["kernels"][0]["workgroups"], 1);
    assert_kernel_entry_kind(
        &parsed["kernels"][1],
        "derivative_rhs_chunk0",
        WgslKernelOutputKind::ScalarChunk,
    );
    assert_eq!(parsed["kernels"][1]["workgroups"], 1);
    assert_eq!(parsed["kernels"][1]["start_slot"], 0);
    assert_eq!(
        parsed["kernels"][1]["output_indices"],
        serde_json::json!([5])
    );
}

#[test]
fn test_wgsl_ode_layout_manifest_rejects_zero_workgroup_size() {
    let problem = solve_problem_with_map_derivative();
    let artifacts = solve_artifacts(&problem);
    let template = builtin_template("wgsl-ode", "model_layout.json.jinja").replace(
        "{%- set workgroup_size = 64 %}",
        "{%- set workgroup_size = 0 %}",
    );
    let error = render_solve_template_with_name(&problem, &artifacts, &template, "BadWorkgroup")
        .expect_err("zero workgroup size should fail before manifest emission");

    assert!(
        error
            .to_string()
            .contains("workgroup_size must be greater than zero"),
        "error should explain invalid workgroup size: {error}"
    );
}

#[test]
fn test_wgsl_ode_layout_manifest_reports_native_map_kernel() {
    let problem = solve_problem_with_map_derivative();
    let artifacts = solve_artifacts(&problem);
    let rendered = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("wgsl-ode", "model_layout.json.jinja"),
        "MapDemo",
    )
    .expect("wgsl-ode layout manifest should render native map node");

    let parsed: serde_json::Value =
        serde_json::from_str(&rendered).expect("layout manifest must be valid JSON");
    assert_eq!(parsed["rows"], 8);
    assert_eq!(parsed["tensor_nodes"], 1);
    assert_eq!(parsed["tensor_families"], 1);
    assert_eq!(parsed["map_families"], 1);
    assert_eq!(parsed["stencil_families"], 0);
    assert_eq!(parsed["scalar_fallback_rows"], 0);
    assert_eq!(parsed["chunks"], 0);
    assert_eq!(parsed["kernel_count"], 1);
    assert_kernel_entry_kind(
        &parsed["kernels"][0],
        "derivative_rhs_map0",
        WgslKernelOutputKind::Native,
    );
    assert_eq!(parsed["kernels"][0]["rows"], 8);
    assert_eq!(parsed["kernels"][0]["output_map"]["start"], 0);
    assert_eq!(
        parsed["kernels"][0]["output_map"]["strides"],
        serde_json::json!([{ "dimension": 0, "stride": 1 }])
    );
    assert_eq!(parsed["kernels"][0]["workgroup_size"], 64);
    assert_eq!(parsed["native_families"][0]["kind"], "map");
    assert_eq!(parsed["native_families"][0]["output_map"]["start"], 0);
    assert_eq!(
        parsed["native_families"][0]["output_map"]["strides"],
        serde_json::json!([{ "dimension": 0, "stride": 1 }])
    );
    assert_eq!(parsed["native_families"][0]["rows"], 8);
    assert_eq!(
        parsed["native_families"][0]["domain_shape"],
        serde_json::json!([8])
    );
}
