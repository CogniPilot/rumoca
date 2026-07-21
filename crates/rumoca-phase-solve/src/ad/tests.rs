use super::*;
use crate::layout::build_var_layout;
use rumoca_core::OpBinary;
use rumoca_ir_dae as dae;

fn scalar_program_block_fixture(
    block: &rumoca_ir_solve::ComputeBlock,
) -> rumoca_ir_solve::ScalarProgramBlock {
    rumoca_eval_solve::to_scalar_program_block(block).expect("valid AD fixture should scalarize")
}

fn scalar_var(name: &str) -> dae::Variable {
    dae::Variable::new(
        rumoca_core::VarName::new(name),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    )
}

fn ad_test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("ad_test_fixture.mo"),
        0,
        1,
    )
}

fn unspanned_ad_test_span() -> rumoca_core::Span {
    rumoca_core::Span::DUMMY
}

#[test]
fn matmul_jvp_lowering_rejects_non_matmul_node_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_ad_tests_source_21.mo"),
        4,
        12,
    );
    let node = ComputeNode::ScalarPrograms(ScalarProgramBlock::with_source_span(
        vec![vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        span,
    ));

    let err = lower_matmul_jvp_node(&node).expect_err("MatMul JVP helper should reject non-MatMul");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.reason()
            .contains("MatMul JVP lowering received ScalarPrograms node"),
        "{err:?}"
    );
}

#[test]
fn matmul_jvp_lowering_rejects_empty_scalar_program_node() {
    let node = ComputeNode::ScalarPrograms(ScalarProgramBlock::with_source_span(
        Vec::new(),
        rumoca_core::Span::source_free_serde_default(),
    ));

    let err = lower_matmul_jvp_node(&node).expect_err("empty ScalarPrograms node is malformed IR");

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(
        err.reason()
            .contains("ScalarPrograms node has no program span"),
        "{err:?}"
    );
}

#[test]
fn spanned_scalar_program_ad_rejects_mismatched_span_count() -> Result<(), LowerError> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_ad_tests_source_84.mo"),
        6,
        11,
    );
    let rows = vec![
        vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        vec![
            LinearOp::Const { dst: 0, value: 2.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
    ];

    let err = match lower_scalar_program_block_ad_with_spans(&rows, &[span]) {
        Ok(_) => {
            return Err(LowerError::ContractViolation {
                reason: "mismatched AD row span metadata must fail".to_string(),
                span,
            });
        }
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.reason().contains("AD scalar row span count 1"),
        "unexpected error: {err}"
    );
    Ok(())
}

#[test]
fn unspanned_scalar_program_ad_rejects_mismatched_span_count() {
    let rows = vec![
        vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        vec![
            LinearOp::Const { dst: 0, value: 2.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
    ];

    let err = lower_scalar_program_block_ad_with_spans(&rows, &[unspanned_ad_test_span()])
        .expect_err("unspanned mismatched AD row metadata must fail");

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(
        err.reason().contains("AD scalar row span count 1"),
        "unexpected error: {err}"
    );
}

#[test]
fn constant_matmul_jvp_rejects_output_count_overflow() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_ad_tests_source_71.mo"),
        3,
        9,
    );
    let block = ComputeBlock {
        nodes: vec![ComputeNode::MatMul {
            lhs_ops: vec![LinearOp::Const { dst: 0, value: 1.0 }],
            lhs_start: 0,
            rhs_ops: vec![LinearOp::Const { dst: 1, value: 1.0 }],
            rhs_start: 1,
            m: usize::MAX,
            k: 1,
            n: 2,
            lhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
            rhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span,
        }],
    };

    let err = lower_compute_block_jvp(&block)
        .expect_err("constant MatMul JVP output count overflow must fail");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
    assert!(
        err.reason().contains("MatMul JVP lhs value count")
            || err.reason().contains("MatMul JVP operand value count"),
        "unexpected error: {err}"
    );
}

#[test]
fn linsolve_jvp_rejects_matrix_range_overflow() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_ad_tests_source_72.mo"),
        5,
        17,
    );
    let block = ComputeBlock {
        nodes: vec![ComputeNode::LinSolve {
            setup_ops: Vec::new(),
            matrix_start: 0,
            rhs_start: 0,
            n: usize::MAX,
            next_reg: 0,
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span,
        }],
    };

    let err =
        lower_compute_block_jvp(&block).expect_err("LinSolve JVP matrix range overflow must fail");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
    assert!(
        err.reason().contains("LinSolve JVP matrix range"),
        "unexpected error: {err}"
    );
}

#[test]
fn scalar_linear_solve_component_ad_rejects_matrix_range_overflow() {
    let row = vec![
        LinearOp::LinearSolveComponent {
            dst: 0,
            matrix_start: 0,
            rhs_start: 0,
            n: usize::MAX,
            component: 0,
        },
        LinearOp::StoreOutput { src: 0 },
    ];

    let err = lower_scalar_program_block_ad(&[row])
        .expect_err("scalar LinearSolveComponent AD range overflow must fail");

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(
        err.reason()
            .contains("LinearSolveComponent AD matrix range"),
        "unexpected error: {err}"
    );
}

#[test]
fn scalar_ad_rejects_register_allocation_overflow() {
    let mut builder = AdBuilder {
        next_reg: u32::MAX,
        ..AdBuilder::new_with_span(SeedMode::SolverYOnly, unspanned_ad_test_span())
    };

    let err = builder
        .emit_const(1.0)
        .expect_err("scalar AD register allocation overflow must fail");

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(
        err.reason().contains("AD register allocation overflow"),
        "unexpected error: {err}"
    );
}

#[test]
fn spanned_ad_builder_rejects_register_allocation_overflow() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_ad_tests_source_73.mo"),
        8,
        19,
    );
    let mut builder = AdBuilder {
        next_reg: u32::MAX,
        ..AdBuilder::new_with_span(SeedMode::SolverYOnly, span)
    };

    let err = builder
        .emit_const(1.0)
        .expect_err("spanned AD register allocation overflow must fail");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
    assert!(
        err.reason().contains("AD register allocation overflow"),
        "unexpected error: {err}"
    );
}

fn decay_dae() -> dae::Dae {
    let span = ad_test_span();
    let mut dae = dae::Dae::new();
    dae.variables.states.insert("x".into(), scalar_var("x"));
    dae.variables.parameters.insert("k".into(), scalar_var("k"));
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::Binary {
            op: OpBinary::Mul,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: "k".into(),
                subscripts: Vec::new(),
                span,
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: "x".into(),
                subscripts: Vec::new(),
                span,
            }),
            span,
        },
        span,
        origin: "test".into(),
        scalar_count: 1,
    });
    dae
}

#[test]
fn full_ad_seeds_parameter_slots_after_solver_y_slots() {
    let dae = decay_dae();
    let layout = build_var_layout(&dae).expect("test DAE layout should build");
    let rows = lower_residual_full_ad(&dae, &layout).expect("lower full AD");

    assert!(
        rows[0]
            .iter()
            .any(|op| matches!(op, LinearOp::LoadSeed { index: 1, .. })),
        "parameter k should use seed index after the single solver-y state: {:?}",
        rows[0]
    );
}

#[test]
fn solver_ad_keeps_parameters_unseeded_for_diffsol_jacobian() {
    let dae = decay_dae();
    let layout = build_var_layout(&dae).expect("test DAE layout should build");
    let rows = lower_residual_ad(&dae, &layout).expect("lower solver AD");

    assert!(
        !rows[0]
            .iter()
            .any(|op| matches!(op, LinearOp::LoadSeed { index: 1, .. })),
        "solver Jacobian should remain with respect to solver-y slots only: {:?}",
        rows[0]
    );
}

/// Forward-AD of a runtime-indexed parameter load: under solver-y-only AD
/// the tangent is zero (parameters are constant w.r.t. y, and the index is
/// discrete), and under parameter-seed AD it becomes a `LoadIndexedSeed`
/// over the same run shifted into the seed region by `p_seed_offset`, reusing
/// the same runtime index register.
#[test]
fn full_ad_of_indexed_param_load_emits_shifted_indexed_seed() {
    let primal = vec![
        LinearOp::Const { dst: 0, value: 2.0 }, // 0-based flat offset
        LinearOp::LoadIndexedP {
            dst: 1,
            base: 3,
            count: 5,
            index: 0,
        },
        LinearOp::StoreOutput { src: 1 },
    ];

    // Solver-y-only: zero tangent, no indexed-seed load.
    let solver = lower_row_ad_with_span(&primal, SeedMode::SolverYOnly, None).expect("solver AD");
    assert!(
        !solver
            .iter()
            .any(|op| matches!(op, LinearOp::LoadIndexedSeed { .. })),
        "solver-y AD of an indexed parameter load must have a zero tangent: {solver:?}"
    );

    // Parameter-seed AD: the real part keeps the `LoadIndexedP`; the tangent
    // is a `LoadIndexedSeed` over the same run shifted by `p_seed_offset`.
    let full = lower_row_ad_with_span(&primal, SeedMode::SolverYAndP { p_seed_offset: 2 }, None)
        .expect("parameter-seed AD");
    assert!(
        full.iter().any(|op| matches!(
            op,
            LinearOp::LoadIndexedP {
                base: 3,
                count: 5,
                ..
            }
        )),
        "parameter-seed AD must keep the real indexed parameter load: {full:?}"
    );
    assert!(
        full.iter().any(|op| matches!(
            op,
            LinearOp::LoadIndexedSeed {
                base: 5, // p_seed_offset (2) + base (3)
                count: 5,
                ..
            }
        )),
        "parameter-seed AD tangent must be a LoadIndexedSeed shifted by p_seed_offset: {full:?}"
    );
}

/// Numeric parity: evaluating the parameter-seed AD of an indexed parameter
/// load yields the seed at exactly the selected slot, matching the analytic
/// tangent d(p[base + clamp(round(idx))])/d(seed) = seed[p_seed_index(slot)].
#[test]
fn indexed_param_load_ad_tangent_reads_seed_at_selected_slot() {
    use rumoca_ir_solve::ScalarProgramBlock;

    let p_seed_offset = 2usize;
    let base = 3usize;
    let count = 5usize;
    let offset = 2usize; // clamp(round(2.0))
    let primal = vec![
        LinearOp::Const {
            dst: 0,
            value: offset as f64,
        },
        LinearOp::LoadIndexedP {
            dst: 1,
            base,
            count,
            index: 0,
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    let ad_row = lower_row_ad_with_span(&primal, SeedMode::SolverYAndP { p_seed_offset }, None)
        .expect("parameter-seed AD");
    let block = ScalarProgramBlock::with_source_span(vec![ad_row], ad_test_span());

    // Seed a single 1.0 at the slot the tangent should select; the stored
    // output (the dual) must read exactly that value.
    let selected = p_seed_offset + base + offset; // 7
    let mut seed = vec![0.0; 16];
    seed[selected] = 4.0;
    let p = vec![0.0; 16];
    let mut out = [0.0];
    rumoca_eval_solve::eval_scalar_program_block(&block, &[], &p, 0.0, Some(&seed), &mut out)
        .expect("evaluate parameter-seed AD row");
    assert!(
        (out[0] - 4.0).abs() < 1e-12,
        "indexed-load tangent must read seed[{selected}] = 4.0, got {}",
        out[0]
    );

    // Seeding a neighbouring slot must NOT leak into the tangent.
    let mut seed2 = vec![0.0; 16];
    seed2[selected + 1] = 9.0;
    let mut out2 = [0.0];
    rumoca_eval_solve::eval_scalar_program_block(&block, &[], &p, 0.0, Some(&seed2), &mut out2)
        .expect("evaluate parameter-seed AD row");
    assert!(
        out2[0].abs() < 1e-12,
        "tangent must isolate the selected slot, got {}",
        out2[0]
    );
}

#[test]
fn compute_block_jvp_scalar_programs_applies_standard_ad() {
    // A ComputeBlock with only ScalarPrograms should produce the same result as
    // the existing scalar AD pass.
    use rumoca_ir_solve::ScalarProgramBlock;
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let block = ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(
            ScalarProgramBlock::with_source_span(vec![row], ad_test_span()),
        )],
    };
    let jvp = lower_compute_block_jvp(&block).expect("JVP of scalar row");
    assert_eq!(jvp.nodes.len(), 1);
    let jvp_rows = scalar_program_block_fixture(&jvp);
    assert_eq!(jvp_rows.programs.len(), 1, "one JVP row for one primal row");
    assert!(
        jvp_rows.programs[0]
            .iter()
            .any(|op| matches!(op, LinearOp::LoadSeed { index: 0, .. })),
        "JVP row should load seed[0] for LoadY[0]: {:?}",
        jvp_rows.programs[0]
    );
}

#[test]
fn compute_block_jvp_matmul_constant_lhs_emits_matmul_jvp() {
    // For MatMul(A_param, x_state) where A has only LoadP ops,
    // the JVP node should also be MatMul with rhs_ops using LoadSeed.
    let lhs_ops = vec![
        LinearOp::LoadP { dst: 0, index: 0 }, // A[0,0]
        LinearOp::LoadP { dst: 1, index: 1 }, // A[0,1]
        LinearOp::Move { dst: 2, src: 0 },
        LinearOp::Move { dst: 3, src: 1 },
    ];
    let rhs_ops = vec![
        LinearOp::LoadY { dst: 4, index: 0 }, // x[0]
        LinearOp::LoadY { dst: 5, index: 1 }, // x[1]
        LinearOp::Move { dst: 6, src: 4 },
        LinearOp::Move { dst: 7, src: 5 },
    ];
    let block = ComputeBlock {
        nodes: vec![ComputeNode::MatMul {
            lhs_ops,
            lhs_start: 2,
            rhs_ops,
            rhs_start: 6,
            m: 1,
            k: 2,
            n: 1,
            lhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
            rhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span: ad_test_span(),
        }],
    };

    let jvp = lower_compute_block_jvp(&block).expect("JVP of MatMul with P-lhs");

    // The JVP node should also be a MatMul (not ScalarPrograms).
    assert_eq!(jvp.nodes.len(), 1, "one JVP node for one primal node");
    assert!(
        matches!(
            jvp.nodes[0],
            ComputeNode::MatMul {
                m: 1,
                k: 2,
                n: 1,
                ..
            }
        ),
        "JVP of MatMul(P-lhs, Y-rhs) should be MatMul, got {:?}",
        jvp.nodes[0]
    );

    // Exact operand AD retains primal loads needed by nonlinear expressions and
    // packs the tangent values consumed by the MatMul kernel.
    if let ComputeNode::MatMul { rhs_ops, .. } = &jvp.nodes[0] {
        assert!(
            rhs_ops
                .iter()
                .any(|op| matches!(op, LinearOp::LoadSeed { .. })),
            "JVP rhs_ops should contain LoadSeed: {:?}",
            rhs_ops
        );
    }
}

#[test]
fn compute_block_jvp_matmul_variable_lhs_emits_matmul_jvp() {
    // For MatMul(A_state, x_param), the JVP should also be MatMul with
    // lhs_ops using LoadSeed.
    let lhs_ops = vec![
        LinearOp::LoadY { dst: 0, index: 0 }, // A depends on states
        LinearOp::Move { dst: 1, src: 0 },
    ];
    let rhs_ops = vec![
        LinearOp::LoadP { dst: 2, index: 0 },
        LinearOp::Move { dst: 3, src: 2 },
    ];
    let block = ComputeBlock {
        nodes: vec![ComputeNode::MatMul {
            lhs_ops,
            lhs_start: 1,
            rhs_ops,
            rhs_start: 3,
            m: 1,
            k: 1,
            n: 1,
            lhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
            rhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span: ad_test_span(),
        }],
    };

    let jvp = lower_compute_block_jvp(&block).expect("JVP of MatMul with Y-lhs");

    assert!(
        matches!(
            jvp.nodes[0],
            ComputeNode::MatMul {
                m: 1,
                k: 1,
                n: 1,
                ..
            }
        ),
        "JVP of MatMul(Y-lhs, P-rhs) should remain MatMul: {:?}",
        jvp.nodes[0]
    );
    if let ComputeNode::MatMul {
        lhs_ops, rhs_ops, ..
    } = &jvp.nodes[0]
    {
        assert!(
            lhs_ops
                .iter()
                .any(|op| matches!(op, LinearOp::LoadSeed { .. })),
            "JVP lhs_ops should contain LoadSeed: {:?}",
            lhs_ops
        );
        assert!(
            !rhs_ops
                .iter()
                .any(|op| matches!(op, LinearOp::LoadSeed { .. })),
            "constant rhs_ops should not contain LoadSeed: {:?}",
            rhs_ops
        );
    }
}

#[test]
fn compute_block_jvp_matmul_constant_operands_remains_tensor_native() {
    let block = ComputeBlock {
        nodes: vec![ComputeNode::MatMul {
            lhs_ops: vec![
                LinearOp::LoadP { dst: 0, index: 0 },
                LinearOp::Move { dst: 1, src: 0 },
            ],
            lhs_start: 1,
            rhs_ops: vec![
                LinearOp::LoadP { dst: 2, index: 1 },
                LinearOp::Move { dst: 3, src: 2 },
            ],
            rhs_start: 3,
            m: 1,
            k: 1,
            n: 1,
            lhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
            rhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span: ad_test_span(),
        }],
    };

    let jvp = lower_compute_block_jvp(&block).expect("constant MatMul JVP should lower");
    assert!(
        matches!(jvp.nodes[0], ComputeNode::MatMul { .. }),
        "constant MatMul JVP should remain tensor-native"
    );

    let mut out = [f64::NAN];
    rumoca_eval_solve::PreparedComputeBlock::new(&jvp)
        .expect("constant MatMul JVP should prepare")
        .eval_with_context(
            &[],
            &[2.0, 3.0],
            0.0,
            rumoca_eval_solve::RowEvalContext {
                seed: Some(&[]),
                ..Default::default()
            },
            &mut out,
        )
        .expect("constant MatMul JVP should evaluate");
    assert_eq!(out, [0.0]);
}

fn eval_jvp(block: &ComputeBlock, y: &[f64], p: &[f64], seed: &[f64]) -> Vec<f64> {
    let jvp = lower_compute_block_jvp(block).expect("JVP fixture should lower");
    let mut out = vec![0.0; jvp.len().expect("JVP fixture output count")];
    rumoca_eval_solve::PreparedComputeBlock::new(&jvp)
        .expect("JVP fixture should prepare")
        .eval_with_context(
            y,
            p,
            0.0,
            rumoca_eval_solve::RowEvalContext {
                seed: Some(seed),
                ..Default::default()
            },
            &mut out,
        )
        .expect("JVP fixture should evaluate");
    out
}

fn eval_scalar_reference_jvp(block: &ComputeBlock, y: &[f64], p: &[f64], seed: &[f64]) -> Vec<f64> {
    let scalar = scalar_program_block_fixture(block);
    let rows = lower_scalar_program_block_ad(&scalar.programs).expect("scalar reference AD");
    let reference = ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_output_indices(rows, scalar.program_spans, scalar.output_indices)
            .expect("scalar reference output mapping"),
    );
    let mut out = vec![0.0; reference.len().expect("scalar reference output count")];
    rumoca_eval_solve::PreparedComputeBlock::new(&reference)
        .expect("scalar reference should prepare")
        .eval_with_context(
            y,
            p,
            0.0,
            rumoca_eval_solve::RowEvalContext {
                seed: Some(seed),
                ..Default::default()
            },
            &mut out,
        )
        .expect("scalar reference should evaluate");
    out
}

#[test]
fn compute_block_jvp_matmul_differentiates_nonlinear_operand_program() {
    let block = ComputeBlock {
        nodes: vec![ComputeNode::MatMul {
            lhs_ops: vec![LinearOp::LoadP { dst: 0, index: 0 }],
            lhs_start: 0,
            rhs_ops: vec![
                LinearOp::LoadY { dst: 1, index: 0 },
                LinearOp::Binary {
                    dst: 2,
                    op: BinaryOp::Mul,
                    lhs: 1,
                    rhs: 1,
                },
            ],
            rhs_start: 2,
            m: 1,
            k: 1,
            n: 1,
            lhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
            rhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span: ad_test_span(),
        }],
    };

    assert_eq!(eval_jvp(&block, &[3.0], &[2.0], &[0.5]), vec![6.0]);
}

#[test]
fn compute_block_jvp_matmul_uses_native_block_product_rule() {
    let block = ComputeBlock {
        nodes: vec![ComputeNode::MatMul {
            lhs_ops: vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::Binary {
                    dst: 1,
                    op: BinaryOp::Mul,
                    lhs: 0,
                    rhs: 0,
                },
            ],
            lhs_start: 1,
            rhs_ops: vec![
                LinearOp::LoadY { dst: 2, index: 1 },
                LinearOp::Unary {
                    dst: 3,
                    op: UnaryOp::Sin,
                    arg: 2,
                },
            ],
            rhs_start: 3,
            m: 1,
            k: 1,
            n: 1,
            lhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
            rhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span: ad_test_span(),
        }],
    };

    let jvp = lower_compute_block_jvp(&block).expect("two-variable MatMul JVP should lower");
    assert!(
        matches!(jvp.nodes[0], ComputeNode::MatMul { k: 2, .. }),
        "two-variable product rule should remain one block MatMul"
    );
    let native = eval_jvp(&block, &[1.7, 0.4], &[], &[0.2, -0.3]);
    let scalar = eval_scalar_reference_jvp(&block, &[1.7, 0.4], &[], &[0.2, -0.3]);
    assert_eq!(native.len(), scalar.len());
    assert!((native[0] - scalar[0]).abs() <= 1.0e-12);
}

fn affine_jvp_fixture(is_map: bool) -> ComputeBlock {
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 0,
            upper: 2,
            step: 1,
        }],
    };
    let output_map = rumoca_ir_solve::TensorOutputMap::dense_contiguous(0, &domain)
        .expect("valid affine fixture output map");
    let base_ops = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Add,
            lhs: 2,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 3 },
    ];
    let load_strides = vec![rumoca_ir_solve::AffineStencilLoadStride {
        op_position: 0,
        terms: vec![rumoca_ir_solve::AffineStencilIndexStrideTerm {
            dimension: 0,
            stride: 1,
        }],
    }];
    let const_strides = vec![rumoca_ir_solve::AffineStencilConstStride {
        op_position: 1,
        terms: vec![rumoca_ir_solve::AffineStencilConstStrideTerm {
            dimension: 0,
            stride: 10.0,
        }],
    }];
    let metadata = rumoca_ir_solve::TensorNodeMetadata::default();
    let span = ad_test_span();
    let node = if is_map {
        ComputeNode::Map {
            domain,
            output_map,
            base_ops,
            load_strides,
            const_strides,
            metadata,
            span,
        }
    } else {
        ComputeNode::AffineStencil {
            domain,
            output_map,
            base_ops,
            load_strides,
            const_strides,
            metadata,
            span,
        }
    };
    ComputeBlock { nodes: vec![node] }
}

#[test]
fn compute_block_jvp_preserves_compact_affine_nodes_and_strides() {
    for is_map in [true, false] {
        let block = affine_jvp_fixture(is_map);
        let jvp = lower_compute_block_jvp(&block).expect("affine JVP should lower");
        assert!(
            matches!(
                (&jvp.nodes[0], is_map),
                (ComputeNode::Map { .. }, true) | (ComputeNode::AffineStencil { .. }, false)
            ),
            "affine JVP must preserve its tensor node kind"
        );
        assert_eq!(
            eval_jvp(&block, &[2.0, 3.0, 4.0], &[], &[0.5, 1.0, 1.5]),
            vec![2.0, 6.0, 12.0]
        );
        assert_eq!(
            eval_jvp(&block, &[2.0, 3.0, 4.0], &[], &[0.5, 1.0, 1.5]),
            eval_scalar_reference_jvp(&block, &[2.0, 3.0, 4.0], &[], &[0.5, 1.0, 1.5])
        );
    }
}

#[test]
fn compute_block_jvp_linsolve_preserves_tensor_node_and_matches_scalar_fallback() {
    let block = ComputeBlock {
        nodes: vec![ComputeNode::LinSolve {
            setup_ops: vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::Const { dst: 1, value: 0.0 },
                LinearOp::Const { dst: 2, value: 0.0 },
                LinearOp::Const { dst: 3, value: 2.0 },
                LinearOp::LoadP { dst: 4, index: 0 },
                LinearOp::LoadY { dst: 5, index: 1 },
            ],
            matrix_start: 0,
            rhs_start: 4,
            n: 2,
            next_reg: 6,
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span: ad_test_span(),
        }],
    };

    let tensor_jvp = lower_compute_block_jvp(&block).expect("tensor LinSolve JVP should lower");
    assert!(
        matches!(tensor_jvp.nodes[0], ComputeNode::LinSolve { n: 2, .. }),
        "LinSolve JVP should remain a tensor LinSolve node: {:?}",
        tensor_jvp.nodes[0]
    );

    let scalar_programs = scalar_program_block_fixture(&block);
    let scalar_jvp = ComputeBlock::from_scalar_program_block(ScalarProgramBlock::with_source_span(
        lower_scalar_program_block_ad(&scalar_programs.programs)
            .expect("scalar LinSolve JVP should lower"),
        ad_test_span(),
    ));

    let y = [3.0, 8.0];
    let p = [6.0];
    let seed = [0.5, 1.0];
    let tensor_len = tensor_jvp
        .len()
        .expect("valid tensor JVP should report output length");
    let scalar_len = scalar_jvp
        .len()
        .expect("valid scalar JVP should report output length");
    let mut tensor_out = vec![0.0; tensor_len];
    let mut scalar_out = vec![0.0; scalar_len];
    let context = rumoca_eval_solve::RowEvalContext {
        seed: Some(&seed),
        ..Default::default()
    };

    rumoca_eval_solve::PreparedComputeBlock::new(&tensor_jvp)
        .expect("valid tensor JVP should prepare")
        .eval_with_context(&y, &p, 0.0, context, &mut tensor_out)
        .expect("tensor JVP should evaluate");
    rumoca_eval_solve::PreparedComputeBlock::new(&scalar_jvp)
        .expect("valid scalar JVP should prepare")
        .eval_with_context(&y, &p, 0.0, context, &mut scalar_out)
        .expect("scalar fallback JVP should evaluate");

    assert_eq!(tensor_out.len(), scalar_out.len());
    for (tensor, scalar) in tensor_out.iter().zip(scalar_out.iter()) {
        assert!(
            (tensor - scalar).abs() <= 1.0e-12,
            "tensor LinSolve JVP {tensor} should match scalar fallback {scalar}"
        );
    }
    assert!((tensor_out[0] + 1.0 / 3.0).abs() <= 1.0e-12);
    assert!((tensor_out[1] - 0.5).abs() <= 1.0e-12);
}
