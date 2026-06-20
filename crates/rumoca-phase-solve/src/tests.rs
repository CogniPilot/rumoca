use super::*;
use std::collections::BTreeSet;

mod derivative_row_tests;
mod projection_plan_more;

fn solve_test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("solve_test_fixture.mo"),
        0,
        1,
    )
}

fn scalar_program_block_fixture(
    block: &rumoca_ir_solve::ComputeBlock,
) -> rumoca_ir_solve::ScalarProgramBlock {
    rumoca_eval_solve::to_scalar_program_block(block)
        .expect("valid solve-lowering fixture should scalarize")
}

fn array_var(name: &str, dims: &[i64]) -> dae::Variable {
    dae::Variable {
        name: rumoca_core::VarName::new(name),
        component_ref: Some(source_component_ref_from_name(name)),
        source_span: solve_test_span(),
        dims: dims.to_vec(),
        ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    }
}

fn scalar_var(name: &str) -> dae::Variable {
    dae::Variable {
        name: rumoca_core::VarName::new(name),
        component_ref: Some(source_component_ref_from_name(name)),
        source_span: solve_test_span(),
        ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    }
}

fn source_array_var(name: &str, dims: &[i64]) -> dae::Variable {
    dae::Variable {
        name: rumoca_core::VarName::new(name),
        component_ref: Some(source_component_ref_from_name(name)),
        source_span: solve_test_span(),
        dims: dims.to_vec(),
        ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    }
}

fn source_scalar_var(name: &str) -> dae::Variable {
    dae::Variable {
        name: rumoca_core::VarName::new(name),
        component_ref: Some(source_component_ref_from_name(name)),
        source_span: solve_test_span(),
        ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    }
}

fn test_component_ref_from_name(name: &str) -> rumoca_core::ComponentReference {
    crate::test_support::component_ref_from_name(name)
}

fn source_component_ref_from_name(name: &str) -> rumoca_core::ComponentReference {
    let span = solve_test_span();
    let mut component_ref = test_component_ref_from_name(name);
    component_ref.span = span;
    component_ref.def_id = Some(source_fixture_def_id(name));
    for part in &mut component_ref.parts {
        part.span = span;
        for subscript in &mut part.subs {
            match subscript {
                rumoca_core::Subscript::Index { span: sub_span, .. }
                | rumoca_core::Subscript::Colon { span: sub_span }
                | rumoca_core::Subscript::Expr { span: sub_span, .. } => *sub_span = span,
            }
        }
    }
    component_ref
}

fn source_fixture_def_id(name: &str) -> rumoca_core::DefId {
    let hash = name.bytes().fold(2_166_136_261_u32, |hash, byte| {
        hash.wrapping_mul(16_777_619) ^ u32::from(byte)
    });
    rumoca_core::DefId::new(hash.max(1))
}

#[test]
fn lower_vec_with_capacity_reports_capacity_overflow_with_span() -> Result<(), LowerError> {
    let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(93), 1, 5);

    let err = match lower_vec_with_capacity::<u8>(usize::MAX, "test vector", span) {
        Ok(_) => {
            return Err(LowerError::ContractViolation {
                reason: "oversized test vector should fail before allocating".to_string(),
                span,
            });
        }
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.reason()
            .contains("test vector capacity exceeds host memory limits"),
        "unexpected error: {err}"
    );
    Ok(())
}

#[test]
fn lower_vec_with_capacity_reports_capacity_overflow_without_dummy_span() {
    let err = lower_vec_with_capacity::<u8>(usize::MAX, "test vector", rumoca_core::Span::DUMMY)
        .expect_err("unspanned oversized test vector should fail before allocating");

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(
        err.reason()
            .contains("test vector capacity exceeds host memory limits"),
        "unexpected error: {err}"
    );
}

#[test]
fn lower_hash_set_with_capacity_reports_capacity_overflow_without_dummy_span() {
    let err = lower_hash_set_with_capacity::<usize>(
        usize::MAX,
        "test hash set",
        rumoca_core::Span::DUMMY,
    )
    .expect_err("unspanned oversized hash set should fail before allocating");

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(
        err.reason()
            .contains("test hash set capacity exceeds host memory limits"),
        "unexpected error: {err}"
    );
}

#[test]
fn reserve_lower_index_map_capacity_reports_capacity_overflow_without_dummy_span() {
    let mut values = IndexMap::<usize, usize>::new();

    let err = reserve_lower_index_map_capacity(
        &mut values,
        usize::MAX,
        "test index map",
        rumoca_core::Span::DUMMY,
    )
    .expect_err("unspanned oversized index map should fail before allocating");

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(
        err.reason()
            .contains("test index map capacity exceeds host memory limits"),
        "unexpected error: {err}"
    );
}

#[test]
fn checked_layout_remainder_reports_underflow_without_dummy_span() {
    let err = checked_layout_remainder(2, 3, "test layout", rumoca_core::Span::DUMMY)
        .expect_err("unspanned layout underflow should fail");

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(
        err.reason()
            .contains("test layout consumes 3 entries from only 2 available"),
        "unexpected error: {err}"
    );
}

#[test]
fn algebraic_projection_plan_reports_range_underflow_without_dummy_span() {
    let err = lower_algebraic_projection_plan(&[], &[], 2, 1, rumoca_core::Span::DUMMY)
        .expect_err("invalid projection range should fail");

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(
        err.reason()
            .contains("algebraic projection range starts after solver scalar count"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_identity_mass_matrix_reports_capacity_overflow_with_span() -> Result<(), LowerError> {
    let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(97), 2, 6);
    let mut problem = solve::SolveProblem::default();
    problem.solve_layout.state_scalar_count = usize::MAX;
    problem.continuous.derivative_rhs = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::with_source_span(vec![Vec::new()], span),
        )],
    };

    let err = match solve_identity_mass_matrix(&problem) {
        Ok(_) => {
            return Err(LowerError::ContractViolation {
                reason: "oversized identity mass matrix should fail before allocating".to_string(),
                span,
            });
        }
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.reason()
            .contains("identity mass matrix rows capacity exceeds host memory limits"),
        "unexpected error: {err}"
    );
    Ok(())
}

#[test]
fn checked_literal_positive_indices_preserves_literal_indices() -> Result<(), LowerError> {
    let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(94), 3, 8);
    let subscripts = vec![
        rumoca_core::Subscript::index(1, span),
        rumoca_core::Subscript::index(3, span),
    ];

    assert_eq!(
        checked_literal_positive_indices(&subscripts, None)?,
        Some(vec![1, 3])
    );
    Ok(())
}

#[test]
fn checked_literal_positive_indices_declines_nonliteral_subscripts() -> Result<(), LowerError> {
    let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(95), 5, 12);
    let subscripts = vec![rumoca_core::Subscript::colon(span)];

    assert_eq!(checked_literal_positive_indices(&subscripts, None)?, None);
    Ok(())
}

#[test]
fn checked_literal_positive_indices_rejects_missing_source_span() {
    let subscripts = vec![rumoca_core::Subscript::index(1, rumoca_core::Span::DUMMY)];

    let err = checked_literal_positive_indices(&subscripts, None)
        .expect_err("source-free literal subscripts should fail fast");

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(err.reason().contains("literal positive subscript"));
    assert!(err.reason().contains("source provenance"));
}

#[cfg(target_pointer_width = "32")]
#[test]
fn checked_literal_positive_indices_rejects_host_index_overflow_with_span() {
    let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(96), 7, 14);
    let subscripts = vec![rumoca_core::Subscript::index(i64::MAX, span)];

    let err = checked_literal_positive_indices(&subscripts, None)
        .expect_err("oversized literal subscript should fail on 32-bit hosts");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
    assert!(err.reason().contains("literal subscript index"));
    assert!(err.reason().contains("exceeds host index range"));
}

#[test]
fn checked_solver_scalar_index_rejects_overflow_with_span() {
    let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(91), 4, 10);
    let var = dae::Variable {
        name: rumoca_core::VarName::new("x"),
        source_span: span,
        ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    };

    let err = checked_solver_scalar_index(usize::MAX, 1, "x[2]", &var)
        .expect_err("solver scalar index overflow must fail");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "invalid IR contract: solver scalar index for `x[2]` overflows host index range"
    );
}

#[test]
fn checked_solver_scalar_index_rejects_overflow_without_dummy_span() {
    let var = dae::Variable {
        name: rumoca_core::VarName::new("x"),
        source_span: rumoca_core::Span::DUMMY,
        ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    };

    let err = checked_solver_scalar_index(usize::MAX, 1, "x[2]", &var)
        .expect_err("solver scalar index overflow must fail");

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert_eq!(
        err.reason(),
        "invalid IR contract: solver scalar index for `x[2]` overflows host index range"
    );
}

#[test]
fn checked_solver_scalar_offset_rejects_overflow_with_span() {
    let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(92), 6, 12);
    let var = dae::Variable {
        name: rumoca_core::VarName::new("x"),
        source_span: span,
        ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    };

    let err = checked_solver_scalar_offset(usize::MAX, 1, "x", &var)
        .expect_err("solver scalar offset overflow must fail");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "invalid IR contract: solver scalar offset after `x` overflows host index range"
    );
}

#[test]
fn checked_solver_scalar_offset_rejects_overflow_without_dummy_span() {
    let var = dae::Variable {
        name: rumoca_core::VarName::new("x"),
        source_span: rumoca_core::Span::DUMMY,
        ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    };

    let err = checked_solver_scalar_offset(usize::MAX, 1, "x", &var)
        .expect_err("solver scalar offset overflow must fail");

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert_eq!(
        err.reason(),
        "invalid IR contract: solver scalar offset after `x` overflows host index range"
    );
}

#[test]
fn continuous_equation_scalar_name_reports_missing_array_lhs_with_span() {
    let dae_model = dae::Dae::new();
    let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(9), 13, 21);

    let err = super::continuous_equation_scalar_name(
        &dae_model,
        &rumoca_core::VarName::new("missingArray"),
        0,
        2,
        span,
    )
    .expect_err("array continuous equation target must resolve to a DAE variable");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
    assert!(
        err.reason()
            .contains("continuous equation array LHS `missingArray`")
    );
}

#[test]
fn continuous_equation_scalar_name_reports_missing_array_lhs_without_dummy_span() {
    let dae_model = dae::Dae::new();

    let err = super::continuous_equation_scalar_name(
        &dae_model,
        &rumoca_core::VarName::new("missingArray"),
        0,
        2,
        rumoca_core::Span::DUMMY,
    )
    .expect_err("array continuous equation target must resolve to a DAE variable");

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(
        err.reason()
            .contains("continuous equation array LHS `missingArray`")
    );
}

#[test]
fn discrete_update_scalar_name_reports_missing_array_lhs_with_span() {
    let dae_model = dae::Dae::new();
    let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(10), 23, 31);

    let err = super::discrete_update_scalar_name(
        &dae_model,
        &rumoca_core::VarName::new("missingDiscreteArray"),
        0,
        2,
        span,
    )
    .expect_err("array discrete update target must resolve to a DAE variable");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
    assert!(
        err.reason()
            .contains("discrete update array LHS `missingDiscreteArray`")
    );
}

#[test]
fn discrete_update_scalar_name_reports_missing_array_lhs_without_dummy_span() {
    let dae_model = dae::Dae::new();

    let err = super::discrete_update_scalar_name(
        &dae_model,
        &rumoca_core::VarName::new("missingDiscreteArray"),
        0,
        2,
        rumoca_core::Span::DUMMY,
    )
    .expect_err("array discrete update target must resolve to a DAE variable");

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(
        err.reason()
            .contains("discrete update array LHS `missingDiscreteArray`")
    );
}

#[test]
fn target_expr_scalar_name_accepts_spanned_index_base_ref() -> Result<(), LowerError> {
    let dae_model = dae::Dae::new();
    let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(11), 5, 12);
    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("tail"),
            subscripts: Vec::new(),
            span,
        }),
        subscripts: vec![rumoca_core::Subscript::index(2, span)],
        span,
    };

    let name = target_expr_scalar_name(&dae_model, &expr, 0, 1)?;

    assert_eq!(name.as_deref(), Some("tail[2]"));
    Ok(())
}

#[test]
fn algebraic_projection_plan_uses_blt_scalar_blocks() {
    let rows = vec![
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 1 },
            solve::LinearOp::LoadY { dst: 1, index: 2 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ],
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 2 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 3 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
    ];
    let row_targets = vec![None; rows.len()];
    let plan = super::lower_algebraic_projection_plan(&rows, &row_targets, 1, 4, solve_test_span())
        .expect("projection plan should lower");

    assert_eq!(plan.blocks.len(), 3);
    let mut rows = plan
        .blocks
        .iter()
        .map(|block| block.rows.clone())
        .collect::<Vec<_>>();
    rows.sort();
    assert_eq!(rows, vec![vec![1], vec![2], vec![3]]);
    for block in &plan.blocks {
        assert_eq!(block.rows.len(), 1);
        assert_eq!(block.y_indices.len(), 1);
        assert!(block.causal_steps.is_empty());
    }
}

#[test]
fn projection_incidence_uses_store_output_slice() {
    let row = vec![
        solve::LinearOp::LoadY { dst: 0, index: 10 },
        solve::LinearOp::LoadY { dst: 1, index: 11 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::LoadY { dst: 3, index: 12 },
        solve::LinearOp::StoreOutput { src: 3 },
    ];
    let projection_set = BTreeSet::from([10, 11, 12]);

    let y_indices = super::collect_algebraic_y_indices_for_row(&row, &projection_set);

    assert_eq!(y_indices, BTreeSet::from([12]));
}

#[test]
fn implicit_rhs_records_residual_row_placement() {
    let derivative = vec![constant_row(10.0)];
    let residual = vec![constant_row(30.0), constant_row(11.0), constant_row(12.0)];
    let residual_targets = vec![
        Some(solve::scalar_slot_y(3)),
        None,
        Some(solve::scalar_slot_y(0)),
    ];

    let implicit = super::build_implicit_rhs_rows(
        &derivative,
        &residual,
        &residual_targets,
        1,
        4,
        rumoca_core::Span::DUMMY,
    )
    .expect("implicit rows should build");

    assert_eq!(implicit.rows[0], derivative[0]);
    assert_eq!(implicit.rows[1], residual[1]);
    assert_eq!(implicit.rows[2], residual[2]);
    assert_eq!(implicit.rows[3], residual[0]);
    assert_eq!(
        implicit.residual_to_implicit_rows,
        vec![Some(3), Some(1), Some(2)]
    );
    assert_eq!(implicit.row_targets[3], Some(solve::scalar_slot_y(3)));
    assert_eq!(implicit.row_targets[2], Some(solve::scalar_slot_y(0)));
}

#[test]
fn implicit_rhs_reports_solver_sized_buffer_overflow() -> Result<(), super::LowerError> {
    let err = match super::build_implicit_rhs_rows(
        &[],
        &[],
        &[],
        0,
        usize::MAX,
        rumoca_core::Span::DUMMY,
    ) {
        Ok(_) => {
            return Err(super::LowerError::ContractViolation {
                reason: "oversized implicit RHS buffers should fail before allocating".to_string(),
                span: rumoca_core::Span::DUMMY,
            });
        }
        Err(err) => err,
    };

    assert_eq!(err.source_span(), None);
    assert!(matches!(
        err,
        super::LowerError::UnspannedContractViolation { .. }
    ));
    assert!(
        err.reason()
            .contains("implicit RHS rows capacity exceeds host memory limits"),
        "unexpected error: {err}"
    );
    Ok(())
}

#[test]
fn implicit_rhs_reports_solver_sized_buffer_overflow_with_context_span()
-> Result<(), super::LowerError> {
    let span = solve_test_span();
    let err = match super::build_implicit_rhs_rows(&[], &[], &[], 0, usize::MAX, span) {
        Ok(_) => {
            return Err(super::LowerError::ContractViolation {
                reason: "oversized implicit RHS buffers should fail before allocating".to_string(),
                span,
            });
        }
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        super::LowerError::ContractViolation { span: err_span, .. } if err_span == span
    ));
    Ok(())
}

#[test]
fn projection_incidence_keeps_coupled_residual_variables() {
    let row = vec![
        solve::LinearOp::LoadY { dst: 0, index: 10 },
        solve::LinearOp::LoadY { dst: 1, index: 11 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ];
    let projection_set = BTreeSet::from([10, 11]);

    let y_indices = super::collect_algebraic_y_indices_for_row(&row, &projection_set);

    assert_eq!(y_indices, BTreeSet::from([10, 11]));
}

#[test]
fn algebraic_projection_loop_uses_blt_unknowns_not_dependency_inputs() {
    let rows = vec![
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::LoadY { dst: 1, index: 1 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::LoadY { dst: 3, index: 2 },
            solve::LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Add,
                lhs: 2,
                rhs: 3,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ],
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 1 },
            solve::LinearOp::LoadY { dst: 1, index: 2 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ],
    ];
    let row_targets = vec![None; rows.len()];
    let plan = super::lower_algebraic_projection_plan(&rows, &row_targets, 0, 3, solve_test_span())
        .expect("projection plan should lower");

    let loop_block = plan
        .blocks
        .iter()
        .find(|block| block.rows.len() == 2)
        .expect("coupled rows should form an algebraic loop");

    assert_eq!(loop_block.y_indices, vec![1, 2]);
}

#[test]
fn algebraic_projection_loop_keeps_explicit_row_targets() -> Result<(), LowerError> {
    let projection_incidence = ProjectionIncidence {
        incidence: Incidence::new(
            vec![BTreeSet::from([0, 1, 2]).into_iter().collect()],
            vec![EquationRef(7)],
            vec![
                UnknownId::SolverY(10),
                UnknownId::SolverY(11),
                UnknownId::SolverY(12),
            ],
        ),
        unknown_y_indices: vec![10, 11, 12],
    };
    let row_targets = vec![
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        Some(solve::scalar_slot_y(12)),
    ];

    let block = super::lower_algebraic_loop_projection_block(
        &[EquationRef(7)],
        &[UnknownId::SolverY(10), UnknownId::SolverY(11)],
        &row_targets,
        &projection_incidence,
        solve_test_span(),
    )?;
    let Some(block) = block else {
        return Err(LowerError::ContractViolation {
            reason: "targeted loop block should lower".to_string(),
            span: rumoca_core::Span::DUMMY,
        });
    };

    assert_eq!(block.rows, vec![7]);
    assert_eq!(block.y_indices, vec![10, 11, 12]);
    Ok(())
}

#[test]
fn algebraic_projection_plan_merges_blocks_that_share_row_targets() -> Result<(), LowerError> {
    let blocks = super::merge_overlapping_projection_blocks(
        vec![
            solve::AlgebraicProjectionBlock {
                rows: vec![10],
                y_indices: vec![3],
                causal_steps: Vec::new(),
            },
            solve::AlgebraicProjectionBlock {
                rows: vec![20, 21],
                y_indices: vec![3, 4],
                causal_steps: Vec::new(),
            },
            solve::AlgebraicProjectionBlock {
                rows: vec![30],
                y_indices: vec![8],
                causal_steps: Vec::new(),
            },
        ],
        solve_test_span(),
    )?;

    assert_eq!(blocks.len(), 2);
    assert!(
        blocks
            .iter()
            .any(|block| { block.rows == vec![10, 20, 21] && block.y_indices == vec![3, 4] })
    );
    assert!(
        blocks
            .iter()
            .any(|block| { block.rows == vec![30] && block.y_indices == vec![8] })
    );
    Ok(())
}

fn var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(source_component_ref_from_name(
            name,
        )),
        subscripts: Vec::new(),
        span: solve_test_span(),
    }
}

fn source_var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(source_component_ref_from_name(
            name,
        )),
        subscripts: Vec::new(),
        span: solve_test_span(),
    }
}

fn source_indexed_var(
    name: &str,
    subscripts: Vec<rumoca_core::Subscript>,
) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(source_component_ref_from_name(
            name,
        )),
        subscripts,
        span: solve_test_span(),
    }
}

fn int_expr(value: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: solve_test_span(),
    }
}

fn real_expr_with_span(value: f64, span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span,
    }
}

fn constant_row(value: f64) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::Const { dst: 0, value },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn range_expr(
    start: rumoca_core::Expression,
    end: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Range {
        start: Box::new(start),
        step: None,
        end: Box::new(end),
        span: rumoca_core::Span::DUMMY,
    }
}

fn stepped_range_expr(
    start: rumoca_core::Expression,
    step: rumoca_core::Expression,
    end: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Range {
        start: Box::new(start),
        step: Some(Box::new(step)),
        end: Box::new(end),
        span: rumoca_core::Span::DUMMY,
    }
}

fn pre_var(name: &str) -> rumoca_core::Expression {
    var(&format!("__pre__.{name}"))
}

fn internal_sample_call(args: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME).into(),
        args,
        is_constructor: false,
        span: solve_test_span(),
    }
}

fn sample_event_indicator_expr(start: f64, interval: f64) -> rumoca_core::Expression {
    internal_sample_call(vec![
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(start),
            span: solve_test_span(),
        },
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(interval),
            span: solve_test_span(),
        },
    ])
}

fn insert_pre_parameter(dae_model: &mut dae::Dae, name: &str) {
    let pre_name = format!("__pre__.{name}");
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new(&pre_name), scalar_var(&pre_name));
}

fn der(expr: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: vec![expr],
        span: rumoca_core::Span::DUMMY,
    }
}

fn pre(expr: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Pre,
        args: vec![expr],
        span: rumoca_core::Span::DUMMY,
    }
}

fn builtin_call(
    function: rumoca_core::BuiltinFunction,
    args: Vec<rumoca_core::Expression>,
) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function,
        args,
        span: rumoca_core::Span::DUMMY,
    }
}

fn function_call(name: &str, args: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(name).into(),
        args,
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn binary(
    op: rumoca_core::OpBinary,
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn unary(op: rumoca_core::OpUnary, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Unary {
        op,
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn change_lowered_expr(
    expr: rumoca_core::Expression,
    pre_expr: rumoca_core::Expression,
) -> rumoca_core::Expression {
    binary(rumoca_core::OpBinary::Neq, expr, pre_expr)
}

#[test]
fn solve_appendix_b_validation_rejects_surviving_pre_operator() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: pre(var("x")),
        span,
        origin: "pre invariant regression".to_string(),
        scalar_count: 1,
    });

    let err = lower_solve_problem(&dae_model).expect_err("pre() must fail Solve validation");

    assert!(matches!(
        err,
        LowerError::ContractViolation {
            span: err_span,
            ..
        } if err_span == span
    ));
    assert!(
        err.to_string()
            .contains("Solve Appendix-B validation failed")
            && err.to_string().contains("contains `pre`"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_unresolved_function_call() {
    let mut dae_model = dae::Dae::default();
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: function_call("Missing.f", vec![]),
        span: rumoca_core::Span::DUMMY,
        origin: "function target invariant regression".to_string(),
        scalar_count: 1,
    });

    let err = lower_solve_problem(&dae_model).expect_err("unresolved call must fail validation");

    assert!(
        err.to_string().contains("invalid function `Missing.f`")
            && err.to_string().contains("unresolved function call"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_undefined_linear_op_register() {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.residual = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![solve::LinearOp::StoreOutput { src: 7 }]],
            solve_test_span(),
        ),
    );

    let err = appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)
        .expect_err("undefined register read must fail validation");

    assert!(
        err.to_string().contains("reads undefined register 7"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_invalid_matmul_operand_range() {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::MatMul {
            lhs_ops: vec![solve::LinearOp::Const { dst: 0, value: 1.0 }],
            lhs_start: 0,
            rhs_ops: vec![solve::LinearOp::Const { dst: 2, value: 1.0 }],
            rhs_start: 2,
            m: 1,
            k: 2,
            n: 1,
            lhs_sparsity: solve::SparsityPattern::Dense,
            rhs_sparsity: solve::SparsityPattern::Dense,
            metadata: solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };

    let err = appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)
        .expect_err("short MatMul lhs register range must fail validation");

    assert!(
        err.to_string()
            .contains("lhs register range starting at 0 has undefined register 1"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_invalid_linsolve_operand_range() {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::LinSolve {
            setup_ops: vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::Const { dst: 1, value: 0.0 },
                solve::LinearOp::Const { dst: 4, value: 2.0 },
            ],
            matrix_start: 0,
            rhs_start: 4,
            n: 2,
            next_reg: 5,
            metadata: solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };

    let err = appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)
        .expect_err("short LinSolve matrix register range must fail validation");

    assert!(
        err.to_string()
            .contains("matrix register range starting at 0 has undefined register 2"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_matmul_operand_range_overflow() {
    let span = solve_test_span();
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::MatMul {
            lhs_ops: vec![solve::LinearOp::Const { dst: 0, value: 1.0 }],
            lhs_start: 0,
            rhs_ops: vec![solve::LinearOp::Const { dst: 1, value: 1.0 }],
            rhs_start: 1,
            m: usize::MAX,
            k: 2,
            n: 1,
            lhs_sparsity: solve::SparsityPattern::Dense,
            rhs_sparsity: solve::SparsityPattern::Dense,
            metadata: solve::TensorNodeMetadata::default(),
            span,
        }],
    };

    let err = appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)
        .expect_err("overflowed MatMul lhs range must fail validation");

    assert!(matches!(
        err,
        LowerError::ContractViolation {
            span: err_span,
            ..
        } if err_span == span
    ));
    assert!(
        err.to_string().contains("MatMul lhs range length overflow"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_random_state_register_range_overflow() {
    let span = solve_test_span();
    let mut problem = solve::SolveProblem::default();
    problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::Const {
                dst: u32::MAX,
                value: 1.0,
            },
            solve::LinearOp::RandomResult {
                dst: 0,
                generator: solve::RandomGenerator::Xorshift64Star,
                state_start: u32::MAX,
                state_len: 2,
            },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        span,
    );

    let err = appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)
        .expect_err("overflowed random state range must fail validation");

    assert!(matches!(
        err,
        LowerError::ContractViolation {
            span: err_span,
            ..
        } if err_span == span
    ));
    assert!(
        err.to_string()
            .contains("op input register range starting at 4294967295 overflows"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_invalid_map_stride_target() {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::Map {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 2,
                    step: 1,
                }],
            },
            output_map: solve::TensorOutputMap {
                start: 0,
                strides: vec![solve::AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 1,
                }],
            },
            base_ops: vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
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
            span: rumoca_core::Span::DUMMY,
        }],
    };

    let err = appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)
        .expect_err("Map load stride must point at a load op");

    assert!(
        err.to_string()
            .contains("Map stride targets non-load op Const"),
        "unexpected error: {err}"
    );
    assert!(
        !err.to_string().contains("Const {"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_invalid_map_const_stride_target() {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.derivative_rhs = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::Map {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 2,
                    step: 1,
                }],
            },
            output_map: solve::TensorOutputMap {
                start: 0,
                strides: vec![solve::AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 1,
                }],
            },
            base_ops: vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
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
            metadata: solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };

    let err = appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)
        .expect_err("Map const stride must point at a const op");

    assert!(
        err.to_string()
            .contains("Map const stride targets non-const op LoadY"),
        "unexpected error: {err}"
    );
    assert!(
        !err.to_string().contains("LoadY {"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_load_seed_in_runtime_problem_rows() {
    let mut problem = solve::SolveProblem::default();
    problem.continuous.residual = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            solve_test_span(),
        ),
    );

    let err = appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)
        .expect_err("runtime SolveProblem rows must not read seed vectors");

    assert!(
        err.to_string()
            .contains("LoadSeed[0] is only valid in derivative/JVP artifact rows"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_allows_load_seed_in_jvp_artifact_rows() {
    let artifacts = solve::SolveArtifacts {
        continuous: solve::ContinuousSolveArtifacts {
            implicit_jacobian_v: solve::ComputeBlock::from_scalar_program_block(
                solve::ScalarProgramBlock::with_source_span(
                    vec![vec![
                        solve::LinearOp::LoadSeed { dst: 0, index: 0 },
                        solve::LinearOp::StoreOutput { src: 0 },
                    ]],
                    solve_test_span(),
                ),
            ),
            full_jacobian_v: solve::ScalarProgramBlock::with_source_span(
                vec![vec![
                    solve::LinearOp::LoadSeed { dst: 0, index: 0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ]],
                solve_test_span(),
            ),
            ..solve::ContinuousSolveArtifacts::default()
        },
    };

    appendix_b_validation::validate_solve_artifacts_appendix_b_invariants(&artifacts)
        .expect("JVP artifacts may read seed vectors");
}

#[test]
fn solve_appendix_b_validation_rejects_invalid_artifact_registers() {
    let artifacts = solve::SolveArtifacts {
        continuous: solve::ContinuousSolveArtifacts {
            full_jacobian_v: solve::ScalarProgramBlock::with_source_span(
                vec![vec![solve::LinearOp::StoreOutput { src: 3 }]],
                solve_test_span(),
            ),
            ..solve::ContinuousSolveArtifacts::default()
        },
    };

    let err = appendix_b_validation::validate_solve_artifacts_appendix_b_invariants(&artifacts)
        .expect_err("malformed JVP artifact rows must fail validation");

    assert!(
        err.to_string().contains("reads undefined register 3"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_invalid_table_operand_register() {
    let mut problem = solve::SolveProblem::default();
    problem.events.root_conditions = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 0.0 },
            solve::LinearOp::Const { dst: 2, value: 1.0 },
            solve::LinearOp::TableLookup {
                dst: 3,
                table_id: 0,
                column: 1,
                input: 2,
            },
            solve::LinearOp::StoreOutput { src: 3 },
        ]],
        solve_test_span(),
    );

    let err = appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)
        .expect_err("undefined table operand register must fail validation");

    assert!(
        err.to_string().contains("reads undefined register 1"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_invalid_random_state_index() {
    let mut problem = solve::SolveProblem::default();
    problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::Const {
                dst: 0,
                value: 11.0,
            },
            solve::LinearOp::Const {
                dst: 1,
                value: 17.0,
            },
            solve::LinearOp::RandomInitialState {
                dst: 2,
                generator: solve::RandomGenerator::Xorshift64Star,
                local_seed: 0,
                global_seed: 1,
                state_index: 3,
                state_len: 3,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]],
        solve_test_span(),
    );

    let err = appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)
        .expect_err("out-of-range random state index must fail validation");

    assert!(
        err.to_string()
            .contains("RandomInitialState state_index 3 is outside state_len=3"),
        "unexpected error: {err}"
    );
}

#[test]
fn solve_appendix_b_validation_rejects_invalid_random_state_register_range() {
    let mut problem = solve::SolveProblem::default();
    problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 1.0 },
            solve::LinearOp::RandomResult {
                dst: 2,
                generator: solve::RandomGenerator::Xorshift64Star,
                state_start: 0,
                state_len: 2,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]],
        solve_test_span(),
    );

    let err = appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)
        .expect_err("short random state register range must fail validation");

    assert!(
        err.to_string().contains("reads undefined register 1"),
        "unexpected error: {err}"
    );
}

#[test]
fn solver_name_index_maps_use_canonical_matrix_subscripts() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("M"), array_var("M", &[3, 4]));

    let maps = build_solver_name_index_maps(&dae_model, 12).expect("solver name maps should build");

    assert_eq!(
        maps.names,
        vec![
            "M[1,1]", "M[1,2]", "M[1,3]", "M[1,4]", "M[2,1]", "M[2,2]", "M[2,3]", "M[2,4]",
            "M[3,1]", "M[3,2]", "M[3,3]", "M[3,4]",
        ]
    );
    assert_eq!(maps.name_to_idx.get("M"), Some(&0));
    assert_eq!(maps.name_to_idx.get("M[1]"), None);
    assert_eq!(maps.name_to_idx.get("M[1,1]"), Some(&0));
    assert_eq!(maps.name_to_idx.get("M[1,2]"), Some(&1));
    assert_eq!(maps.name_to_idx.get("M[5]"), None);
    assert_eq!(maps.name_to_idx.get("M[2,1]"), Some(&4));
    assert_eq!(maps.name_to_idx.get("M[3,4]"), Some(&11));
    assert_eq!(
        solve::solver_idx_for_target("M[2,1]", &maps.name_to_idx),
        Some(4)
    );
    assert_eq!(
        maps.base_to_indices.get("M").cloned().unwrap_or_default(),
        (0..12).collect::<Vec<_>>()
    );
}

#[test]
fn solver_name_index_canonical_names_respect_truncated_solver_len() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("M"), array_var("M", &[2, 2]));

    let maps = build_solver_name_index_maps(&dae_model, 3).expect("solver name maps should build");

    assert_eq!(maps.names, vec!["M[1,1]", "M[1,2]", "M[2,1]"]);
    assert_eq!(maps.name_to_idx.get("M[1]"), None);
    assert_eq!(maps.name_to_idx.get("M[1,1]"), Some(&0));
    assert_eq!(maps.name_to_idx.get("M[2]"), None);
    assert_eq!(maps.name_to_idx.get("M[1,2]"), Some(&1));
    assert_eq!(maps.name_to_idx.get("M[3]"), None);
    assert_eq!(maps.name_to_idx.get("M[2,1]"), Some(&2));
    assert_eq!(maps.name_to_idx.get("M[2,2]"), None);
}

#[test]
fn solver_name_index_maps_report_invalid_variable_shape_span() {
    let mut dae_model = dae::Dae::default();
    let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(42), 5, 18);
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("bad"),
        dae::Variable {
            source_span: span,
            ..array_var("bad", &[2, -1])
        },
    );

    let err = build_solver_name_index_maps(&dae_model, 2)
        .expect_err("invalid DAE variable shape should bubble as a lowering error");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
    assert!(
        err.reason()
            .contains("DAE variable `bad` has negative dimension -1"),
        "unexpected error: {err}"
    );
}

#[test]
fn solver_name_index_aliases_do_not_reintroduce_runtime_discrete_names() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("a"), scalar_var("a"));
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("dup"), scalar_var("dup"));
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("b"), scalar_var("b"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("dup"), scalar_var("dup"));

    let layout = layout::build_var_layout_with_solver_len(&dae_model, 2)
        .expect("test DAE layout should build");
    let solve_layout =
        lower_solve_layout(&dae_model, 2).expect("test DAE solve layout should build");

    assert_eq!(solve_layout.solver_maps.names, vec!["a", "b"]);
    assert_eq!(solve_layout.solver_maps.name_to_idx.get("a"), Some(&0));
    assert_eq!(solve_layout.solver_maps.name_to_idx.get("b"), Some(&1));
    assert_eq!(solve_layout.solver_maps.name_to_idx.get("dup"), None);
    assert!(matches!(
        layout.binding("dup"),
        Some(solve::ScalarSlot::P { .. })
    ));
}

#[test]
fn solve_problem_lowers_appendix_b_condition_memory_as_initial_updates() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("p"), scalar_var("p"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("d"), scalar_var("d"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("c[1]"), scalar_var("c[1]"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("flag"), scalar_var("flag"));
    dae_model.conditions.equations.push(dae::Equation::explicit(
        rumoca_core::VarName::new("c[1]"),
        var("flag"),
        rumoca_core::Span::DUMMY,
        "condition memory",
    ));
    dae_model.conditions.equations.push(dae::Equation::explicit(
        rumoca_core::VarName::new("d"),
        var("p"),
        rumoca_core::Span::DUMMY,
        "non-condition discrete assignment in f_c",
    ));

    let problem = lower_solve_problem(&dae_model).expect("condition memory should lower");

    assert_eq!(problem.initialization.update_rhs.len(), 1);
    assert_eq!(
        problem.initialization.update_targets,
        vec![solve::scalar_slot_p(2)]
    );
}

#[test]
fn solve_problem_lowers_generated_condition_memory_initial_updates() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("c"), scalar_var("c"));
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("__rumoca_c[1]"),
        scalar_var("__rumoca_c[1]"),
    );
    dae_model.conditions.equations.push(dae::Equation::explicit(
        rumoca_core::VarName::new("__rumoca_c[1]"),
        var("c"),
        rumoca_core::Span::DUMMY,
        "generated condition memory",
    ));

    let problem = lower_solve_problem(&dae_model).expect("generated condition memory should lower");

    assert_eq!(problem.initialization.update_rhs.len(), 1);
    assert_eq!(
        problem.initialization.update_targets,
        vec![solve::scalar_slot_p(1)]
    );
}

#[test]
fn solve_problem_preserves_scalar_program_source_spans() {
    let discrete_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("discrete_span.mo"),
        3,
        14,
    );
    let initial_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("initial_span.mo"),
        17,
        29,
    );
    let relation_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("relation_span.mo"),
        31,
        42,
    );
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("p"), scalar_var("p"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("d"), scalar_var("d"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("c"), scalar_var("c"));
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("__rumoca_c[1]"),
        scalar_var("__rumoca_c[1]"),
    );
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("d"),
            var("p"),
            discrete_span,
            "discrete assignment",
        ));
    dae_model.conditions.equations.push(dae::Equation::explicit(
        rumoca_core::VarName::new("__rumoca_c[1]"),
        var("c"),
        initial_span,
        "condition memory",
    ));
    dae_model
        .conditions
        .relations
        .push(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Gt,
            lhs: Box::new(real_expr_with_span(1.0, relation_span)),
            rhs: Box::new(real_expr_with_span(0.0, relation_span)),
            span: relation_span,
        });

    let problem = lower_solve_problem(&dae_model).expect("spanned scalar rows should lower");

    assert_eq!(problem.discrete.rhs.program_span(0), Some(discrete_span));
    assert_eq!(
        problem.initialization.update_rhs.program_span(0),
        Some(initial_span)
    );
    assert_eq!(
        problem.events.root_conditions.program_span(0),
        Some(relation_span)
    );
}

#[test]
fn solve_problem_zero_fills_missing_non_state_implicit_rows_for_templates() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("v"), scalar_var("v"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("i"), scalar_var("i"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("R"), scalar_var("R"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Sub,
            var("v"),
            binary(rumoca_core::OpBinary::Mul, var("R"), var("i")),
        ),
        solve_test_span(),
        "underdetermined template metadata row",
    ));

    let problem = lower_solve_problem(&dae_model).expect("template solve-IR should lower");

    assert_eq!(problem.solve_layout.solver_scalar_count(), 2);
    let rhs = scalar_program_block_fixture(&problem.continuous.implicit_rhs);
    assert_eq!(rhs.programs.len(), 2);
    assert_eq!(rhs.programs[1], zero_rhs_row());
}

#[test]
fn solve_problem_keeps_input_driven_algebraic_equation_implicit() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("T"), scalar_var("T"));
    dae_model
        .variables
        .inputs
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), var("T")),
        solve_test_span(),
        "state derivative",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, var("T"), var("u")),
        solve_test_span(),
        // MLS §4.4.4 and Appendix B: input variables are known external
        // quantities during continuous solving. The equation constrains
        // the algebraic unknown `T`; it must not be converted into a
        // runtime assignment that writes the input tail.
        "algebraic driven by input",
    ));

    let problem = lower_solve_problem(&dae_model).expect("input algebraic should lower");

    assert!(problem.discrete.runtime_assignment_rhs.is_empty());
    assert_eq!(problem.solve_layout.solver_scalar_count(), 2);
    let implicit_rhs = scalar_program_block_fixture(&problem.continuous.implicit_rhs);
    assert_ne!(implicit_rhs.programs[1], zero_rhs_row());
    assert!(matches!(
        problem.continuous.implicit_row_targets[1],
        Some(solve::ScalarSlot::Y { index: 1, .. })
    ));
}

#[test]
fn solve_problem_lowers_structured_continuous_residual_to_map() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), source_scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("z"), source_array_var("z", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("w"), source_array_var("w", &[3]));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), int_expr(0)),
        span,
        "state derivative",
    ));
    for index in 1..=3 {
        dae_model.continuous.equations.push(dae::Equation::residual(
            binary(
                rumoca_core::OpBinary::Sub,
                source_indexed_var(
                    "z",
                    vec![rumoca_core::Subscript::generated_index(index, span)],
                ),
                source_indexed_var(
                    "w",
                    vec![rumoca_core::Subscript::generated_index(index, span)],
                ),
            ),
            span,
            "structured z=w residual",
        ));
    }
    dae_model
        .continuous
        .structured_equations
        .push(dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 3,
                    step: 1,
                }],
            },
            first_equation_index: 1,
            equation_counts: vec![1, 1, 1],
            span,
            origin: "structured z=w residual".to_string(),
        });

    let problem = lower_solve_problem(&dae_model).expect("structured residual should lower");

    assert_eq!(problem.continuous.residual.len(), Ok(3));
    assert!(matches!(
        problem.continuous.residual.nodes.as_slice(),
        [solve::ComputeNode::Map { .. }]
    ));
    let residual_rows = scalar_program_block_fixture(&problem.continuous.residual);
    assert_eq!(residual_rows.programs.len(), 3);
    assert_eq!(residual_rows.output_indices, vec![0, 1, 2]);
    assert!(matches!(
        problem.continuous.implicit_rhs.nodes.as_slice(),
        [
            solve::ComputeNode::ScalarPrograms(_),
            solve::ComputeNode::Map {
                output_map: solve::TensorOutputMap { start: 1, .. },
                ..
            },
            solve::ComputeNode::ScalarPrograms(_)
        ]
    ));
    let implicit_rows = scalar_program_block_fixture(&problem.continuous.implicit_rhs);
    assert_eq!(implicit_rows.output_indices, vec![0, 1, 2, 3, 4, 5, 6]);
}

#[test]
fn solve_problem_lowers_structured_continuous_residual_with_scalar_math_to_map() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), source_scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("z"), source_array_var("z", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("w"), source_array_var("w", &[3]));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("theta"), scalar_var("theta"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), int_expr(0)),
        span,
        "state derivative",
    ));
    for index in 1..=3 {
        let subscript = rumoca_core::Subscript::generated_index(index, span);
        let trig_scaled_source = binary(
            rumoca_core::OpBinary::Add,
            binary(
                rumoca_core::OpBinary::Mul,
                builtin_call(rumoca_core::BuiltinFunction::Sin, vec![var("theta")]),
                source_indexed_var("w", vec![subscript.clone()]),
            ),
            builtin_call(rumoca_core::BuiltinFunction::Cos, vec![var("theta")]),
        );
        dae_model.continuous.equations.push(dae::Equation::residual(
            binary(
                rumoca_core::OpBinary::Sub,
                source_indexed_var("z", vec![subscript]),
                trig_scaled_source,
            ),
            span,
            "structured trig residual",
        ));
    }
    dae_model
        .continuous
        .structured_equations
        .push(dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 3,
                    step: 1,
                }],
            },
            first_equation_index: 1,
            equation_counts: vec![1, 1, 1],
            span,
            origin: "structured trig residual".to_string(),
        });

    let problem = lower_solve_problem(&dae_model).expect("structured trig residual should lower");

    assert_eq!(problem.continuous.residual.len(), Ok(3));
    assert!(matches!(
        problem.continuous.residual.nodes.as_slice(),
        [solve::ComputeNode::Map { .. }]
    ));
}

#[test]
fn solve_problem_lowers_structured_continuous_residual_with_guard_to_map() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), source_scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("z"), source_array_var("z", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("w"), source_array_var("w", &[3]));
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("mask"),
        source_array_var("mask", &[3]),
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("fallback"),
        scalar_var("fallback"),
    );
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), int_expr(0)),
        span,
        "state derivative",
    ));
    for index in 1..=3 {
        let subscript = rumoca_core::Subscript::generated_index(index, span);
        let guarded_source = rumoca_core::Expression::If {
            branches: vec![(
                binary(
                    rumoca_core::OpBinary::Gt,
                    source_indexed_var("mask", vec![subscript.clone()]),
                    int_expr(0),
                ),
                source_indexed_var("w", vec![subscript.clone()]),
            )],
            else_branch: Box::new(var("fallback")),
            span,
        };
        dae_model.continuous.equations.push(dae::Equation::residual(
            binary(
                rumoca_core::OpBinary::Sub,
                source_indexed_var("z", vec![subscript]),
                guarded_source,
            ),
            span,
            "structured guarded residual",
        ));
    }
    dae_model
        .continuous
        .structured_equations
        .push(dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 3,
                    step: 1,
                }],
            },
            first_equation_index: 1,
            equation_counts: vec![1, 1, 1],
            span,
            origin: "structured guarded residual".to_string(),
        });

    let problem =
        lower_solve_problem(&dae_model).expect("structured guarded residual should lower");

    assert_eq!(problem.continuous.residual.len(), Ok(3));
    assert!(matches!(
        problem.continuous.residual.nodes.as_slice(),
        [solve::ComputeNode::Map { .. }]
    ));
}
