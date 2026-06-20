use super::*;
use rumoca_ir_solve as solve;

use super::codegen_tests::builtin_template;

fn fixture_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("sparse_solve_fixture.mo"),
        1,
        2,
    )
}

fn solve_problem_with_sparse_output_map_derivative() -> solve::SolveProblem {
    let mut problem = solve::SolveProblem::default();
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: 3,
            step: 1,
        }],
    };
    problem.continuous.derivative_rhs = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::Map {
            domain,
            output_map: solve::TensorOutputMap {
                start: 2,
                strides: vec![solve::AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 2,
                }],
            },
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
        }],
    };
    problem
}

#[test]
// SPEC_0021: Exception - this regression verifies sparse output handling
// across C, Rust, and MLIR renderers from one shared fixture.
#[allow(clippy::too_many_lines)]
fn scalar_solve_targets_write_sparse_output_indices() {
    let problem = solve_problem_with_sparse_output_map_derivative();
    let artifacts = solve::SolveArtifacts::default();

    let c = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("c-solve", "model_solve.c.jinja"),
        "SparseDemo",
    )
    .expect("c-solve template should render");
    assert!(c.contains("out[i] = 0.0;"));
    assert!(c.contains("out[2] ="));
    assert!(c.contains("out[4] ="));
    assert!(c.contains("out[6] ="));
    assert!(!c.contains("out[0] ="), "C ignored output_indices:\n{c}");

    let rust = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("rust-solve", "model_solve.rs.jinja"),
        "SparseDemo",
    )
    .expect("rust-solve template should render");
    assert!(rust.contains("out[..DERIVATIVE_LEN].fill(0.0);"));
    assert!(rust.contains("out[2] ="));
    assert!(rust.contains("out[4] ="));
    assert!(rust.contains("out[6] ="));
    assert!(
        !rust.contains("out[0] ="),
        "Rust ignored output_indices:\n{rust}"
    );

    let cuda = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("cuda-c", "model_solve.cu.jinja"),
        "SparseDemo",
    )
    .expect("cuda-c template should render");
    assert!(cuda.contains("batch_out[i] = 0.0;"));
    assert!(cuda.contains("batch_out[2] ="));
    assert!(cuda.contains("batch_out[4] ="));
    assert!(cuda.contains("batch_out[6] ="));
    assert!(
        !cuda.contains("batch_out[0] ="),
        "CUDA ignored output_indices:\n{cuda}"
    );

    let embedded = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("embedded-c", "model.c.jinja"),
        "SparseDemo",
    )
    .expect("embedded-c template should render");
    assert!(embedded.contains("out[i] = 0.0;"));
    assert!(
        embedded
            .contains("real_t dx[SPARSEDEMO_DERIVATIVE_LEN > 0 ? SPARSEDEMO_DERIVATIVE_LEN : 1]")
    );
    assert!(embedded.contains("for (int i = 0; i < SPARSEDEMO_STATE_LEN; ++i)"));
    assert!(embedded.contains("out[2] ="));
    assert!(embedded.contains("out[4] ="));
    assert!(embedded.contains("out[6] ="));
    assert!(
        !embedded.contains("out[0] ="),
        "embedded C ignored output_indices:\n{embedded}"
    );

    let mlir = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("mlir", "mlir.mlir.jinja"),
        "SparseDemo",
    )
    .expect("mlir template should render");
    assert!(mlir.contains("%drv_zero = arith.constant 0.0 : f64"));
    assert!(mlir.contains("%drv_zero_i6 = arith.constant 6 : index"));
    assert!(mlir.contains("%outi0 = arith.constant 2 : index"));
    assert!(mlir.contains("%outi1 = arith.constant 4 : index"));
    assert!(mlir.contains("%outi2 = arith.constant 6 : index"));

    let layout = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("wgsl-solve", "model_layout.json.jinja"),
        "SparseDemo",
    )
    .expect("wgsl-solve layout manifest should render");
    let layout: serde_json::Value =
        serde_json::from_str(&layout).expect("wgsl-solve layout manifest should be valid JSON");
    let expected_strides = serde_json::json!([{ "dimension": 0, "stride": 2 }]);
    assert_eq!(
        layout["kernels"][0]["output_map"]["strides"], expected_strides,
        "native kernel manifest should expose sparse tensor output strides"
    );
    assert_eq!(
        layout["native_families"][0]["output_map"]["strides"], expected_strides,
        "native family manifest should expose sparse tensor output strides"
    );

    let wgsl = render_solve_template_with_name(
        &problem,
        &artifacts,
        builtin_template("wgsl-solve", "model_solve.wgsl.jinja"),
        "SparseDemo",
    )
    .expect("wgsl-solve source should render");
    assert!(
        wgsl.contains("Native map family: affine output map start 2, rows 3"),
        "native WGSL source should describe sparse output maps without implying contiguity:\n{wgsl}"
    );
    assert!(
        !wgsl.contains("output slots 2..5"),
        "native WGSL source should not describe sparse output maps as contiguous ranges:\n{wgsl}"
    );
}
