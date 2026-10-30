use super::*;

struct RefusingTargetBackend;

impl SolveExecutionBackend for RefusingTargetBackend {
    fn compile_expression(
        &self,
        _: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        Err("private target tuple is unsupported".into())
    }
}

#[test]
fn issued_target_query_refusal_is_reported_during_preparation_with_its_source() {
    let block = spanned_block(
        vec![shifted_variable_residual_row(0, 1.0)],
        "unsupported_target.mo",
    );
    let span = block.program_span(0).unwrap();
    let source = PreparedScalarProgramBlock::new(block).unwrap();
    let backend = super::super::test_kernels::recording::RecordingBackend::new(Rc::new(
        RefusingTargetBackend,
    ));
    let error = super::super::compiled_targets::TargetQueries::prepare(
        &source,
        [(0, 0)],
        &backend,
        RowEvalContext::default(),
    )
    .err()
    .expect("required query code must exist before a runnable query is issued");
    assert!(matches!(
        error,
        EvalSolveError::CompiledExecution { span: Some(owner), .. } if owner == span
    ));
    assert_eq!(
        backend.requests(),
        (0, 0, 0),
        "target preparation must not request unrelated products"
    );
}

#[test]
fn eager_target_query_preserves_tiny_values_without_mutating_solver_state() {
    let source = PreparedScalarProgramBlock::new(spanned_block(
        vec![shifted_variable_residual_row(0, 1e-17)],
        "tiny_eager_target.mo",
    ))
    .unwrap();
    let backend = super::super::test_kernels::ReferenceBackend;
    let queries = super::super::compiled_targets::TargetQueries::prepare(
        &source,
        [(0, 0)],
        &backend,
        RowEvalContext::default(),
    )
    .unwrap();
    let y = [1e20];
    assert_eq!(
        queries
            .call((0, 0, 0), (&y, &[], 0.0), RowEvalContext::default())
            .unwrap(),
        Some(1e-17)
    );
    assert_eq!(y, [1e20]);
    assert_eq!(
        queries
            .call((0, 0, 1), (&y, &[], 0.0), RowEvalContext::default())
            .unwrap(),
        None
    );
}

#[test]
fn invalid_issued_target_row_is_a_preparation_error() {
    let source = PreparedScalarProgramBlock::new(spanned_block(
        vec![shifted_variable_residual_row(0, 1.0)],
        "bad_target_row.mo",
    ))
    .unwrap();
    let backend = super::super::test_kernels::ReferenceBackend;
    assert!(matches!(
        super::super::compiled_targets::TargetQueries::prepare(
            &source,
            [(2, 0)],
            &backend,
            RowEvalContext::default()
        ),
        Err(EvalSolveError::InvalidRow { .. })
    ));
}

#[test]
fn eager_affine_target_query_preserves_exact_values_and_singular_coefficients() {
    let source = PreparedScalarProgramBlock::new(spanned_block(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 0 },
            solve::LinearOp::LoadY { dst: 1, index: 0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::Const { dst: 3, value: 4.0 },
            solve::LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Sub,
                lhs: 2,
                rhs: 3,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ]],
        "affine_eager_target.mo",
    ))
    .unwrap();
    let backend = super::super::test_kernels::ReferenceBackend;
    let queries = super::super::compiled_targets::TargetQueries::prepare(
        &source,
        [(0, 0)],
        &backend,
        RowEvalContext::default(),
    )
    .unwrap();
    for coefficient in [2.0, -4.0, 1e-20] {
        let y = [1e30];
        assert_eq!(
            queries
                .call(
                    (0, 0, 0),
                    (&y, &[coefficient], 0.0),
                    RowEvalContext::default()
                )
                .unwrap(),
            Some(4.0 / coefficient)
        );
        assert_eq!(y, [1e30]);
    }
    for coefficient in [0.0, f64::INFINITY, f64::NAN] {
        assert!(matches!(
            queries.call(
                (0, 0, 0),
                (&[1.0], &[coefficient], 0.0),
                RowEvalContext::default()
            ),
            Err(RuntimeSolveError::RefreshTargetSingular {
                row: 0,
                target_y_index: 0,
                span: Some(_),
                ..
            })
        ));
    }
}

#[test]
fn a_shared_target_family_checks_only_the_selected_coefficient() {
    let load = |dst_start, input| solve::LinearOp::TensorLoad {
        dst_start,
        input,
        input_start: 0,
        count: 2,
        seed_start: None,
        lanes: 1,
    };
    let source = PreparedScalarProgramBlock::new(spanned_block(
        vec![vec![
            load(0, solve::TensorInputKind::Y),
            load(2, solve::TensorInputKind::P),
            solve::LinearOp::TensorBinary {
                dst_start: 4,
                op: solve::BinaryOp::Mul,
                lhs_start: 0,
                rhs_start: 2,
                count: 2,
                lhs_stride: 1,
                rhs_stride: 1,
                lanes: 1,
            },
            solve::LinearOp::Const { dst: 6, value: 4.0 },
            solve::LinearOp::TensorBinary {
                dst_start: 7,
                op: solve::BinaryOp::Sub,
                lhs_start: 4,
                rhs_start: 6,
                count: 2,
                lhs_stride: 1,
                rhs_stride: 0,
                lanes: 1,
            },
            solve::LinearOp::StoreOutputRange {
                start: 7,
                count: 2,
                stride: 1,
            },
        ]],
        "shared_coefficient_family.mo",
    ))
    .unwrap();
    let first = source.portable_target_value_plan(0, 0, 0).unwrap().unwrap();
    let second = source.portable_target_value_plan(0, 1, 1).unwrap().unwrap();
    assert_eq!(first.canonical_prefix_len(), second.canonical_prefix_len());
    assert_ne!(
        first.private_result_offset(),
        second.private_result_offset()
    );
    let queries = super::super::compiled_targets::TargetQueries::prepare(
        &source,
        [(0, 0), (1, 1)],
        &super::super::test_kernels::ReferenceBackend,
        RowEvalContext::default(),
    )
    .unwrap();
    for coefficient in [0.0, f64::NAN] {
        let y = [1e30, -1e30];
        let inputs = (&y[..], &[2.0, coefficient][..], 0.0);
        assert_eq!(
            queries
                .call((0, 0, 0), inputs, RowEvalContext::default())
                .unwrap(),
            Some(2.0)
        );
        assert!(matches!(
            queries.call((0, 1, 1), inputs, RowEvalContext::default()),
            Err(RuntimeSolveError::RefreshTargetSingular {
                row: 0,
                target_y_index: 1,
                ..
            })
        ));
        assert_eq!(y, [1e30, -1e30]);
    }
}

struct FamilyPolicyBackend {
    enabled: bool,
    wrong_count: bool,
    family_calls: std::cell::Cell<usize>,
    fallback_calls: std::cell::Cell<usize>,
}

impl SolveExecutionBackend for FamilyPolicyBackend {
    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        super::super::test_kernels::ReferenceBackend.compile_expression(block)
    }
    fn compile_target_values(
        &self,
        _: &solve_eval::PreparedTargetValuePlan,
        _: RowEvalContext<'_>,
    ) -> Result<Rc<dyn CompiledSolveTargetValues>, String> {
        self.fallback_calls.set(self.fallback_calls.get() + 1);
        Err("custom target policy refuses".into())
    }
    fn compile_target_expression_family(
        &self,
        blocks: &[&solve::ScalarProgramBlock],
    ) -> Result<Option<CompiledSolveExpressionFamily>, SolveFamilyCompileError> {
        self.family_calls.set(self.family_calls.get() + 1);
        if !self.enabled {
            return Ok(None);
        }
        if self.wrong_count {
            return Ok(Some(Vec::new()));
        }
        blocks
            .iter()
            .map(|block| self.compile_expression(block))
            .collect::<Result<Vec<_>, _>>()
            .map(Some)
            .map_err(|error| (None, error))
    }
}

#[test]
fn target_family_opt_in_preserves_values_and_decline_preserves_custom_policy() {
    for enabled in [false, true] {
        let source = PreparedScalarProgramBlock::new(spanned_block(
            vec![
                shifted_variable_residual_row(0, 7.0),
                shifted_variable_residual_row(1, 13.0),
            ],
            "target_family_policy.mo",
        ))
        .unwrap();
        let backend = FamilyPolicyBackend {
            enabled,
            wrong_count: false,
            family_calls: Default::default(),
            fallback_calls: Default::default(),
        };
        let result = super::super::compiled_targets::TargetQueries::prepare(
            &source,
            [(0, 0), (1, 1)],
            &backend,
            RowEvalContext::default(),
        );
        assert_eq!(backend.family_calls.get(), 1);
        if enabled {
            let queries = result.unwrap();
            assert_eq!(backend.fallback_calls.get(), 0);
            for (row, value) in [(0, 7.0), (1, 13.0)] {
                assert_eq!(
                    queries
                        .call(
                            (row, 0, row),
                            (&[99.0, 101.0], &[], 0.0),
                            RowEvalContext::default()
                        )
                        .unwrap(),
                    Some(value)
                );
            }
        } else {
            assert!(result.is_err());
            assert_eq!(backend.fallback_calls.get(), 1);
        }
    }
}

#[test]
fn target_family_wrong_entry_count_is_fatal_without_fallback() {
    let source = PreparedScalarProgramBlock::new(spanned_block(
        vec![shifted_variable_residual_row(0, 7.0)],
        "target_family_count.mo",
    ))
    .unwrap();
    let backend = FamilyPolicyBackend {
        enabled: true,
        wrong_count: true,
        family_calls: Default::default(),
        fallback_calls: Default::default(),
    };
    assert!(
        super::super::compiled_targets::TargetQueries::prepare(
            &source,
            [(0, 0)],
            &backend,
            RowEvalContext::default()
        )
        .is_err()
    );
    assert_eq!(backend.family_calls.get(), 1);
    assert_eq!(backend.fallback_calls.get(), 0);
}

#[test]
fn target_family_rejects_directional_seed_before_backend_preparation() {
    let source = PreparedScalarProgramBlock::new(spanned_block(
        vec![shifted_variable_residual_row(0, 7.0)],
        "target_family_seed.mo",
    ))
    .unwrap();
    let backend = FamilyPolicyBackend {
        enabled: true,
        wrong_count: false,
        family_calls: Default::default(),
        fallback_calls: Default::default(),
    };
    let seed = [1.0];
    assert!(
        super::super::compiled_targets::TargetQueries::prepare(
            &source,
            [(0, 0)],
            &backend,
            RowEvalContext {
                seed: Some(&seed),
                ..Default::default()
            }
        )
        .is_err()
    );
    assert_eq!(backend.family_calls.get(), 0);
    assert_eq!(backend.fallback_calls.get(), 0);
}

struct LateFamilyFailure {
    entry: usize,
}

impl SolveExecutionBackend for LateFamilyFailure {
    fn compile_expression(
        &self,
        _: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        Err("per-entry fallback must not run after family failure".into())
    }
    fn compile_target_expression_family(
        &self,
        blocks: &[&solve::ScalarProgramBlock],
    ) -> Result<Option<CompiledSolveExpressionFamily>, SolveFamilyCompileError> {
        assert_eq!(blocks.len(), 2);
        Err((Some(self.entry), "second family entry refused".into()))
    }
}

#[test]
fn target_family_failure_is_attributed_to_the_failing_entry_source() {
    let spans = [
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("first_family.mo"),
            1,
            2,
        ),
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("second_family.mo"),
            3,
            4,
        ),
    ];
    let source = PreparedScalarProgramBlock::new(
        solve::ScalarProgramBlock::with_program_spans(
            vec![
                shifted_variable_residual_row(0, 7.0),
                shifted_variable_residual_row(1, 13.0),
            ],
            spans.to_vec(),
        )
        .unwrap(),
    )
    .unwrap();
    let error = super::super::compiled_targets::TargetQueries::prepare(
        &source,
        [(0, 0), (1, 1)],
        &LateFamilyFailure { entry: 1 },
        RowEvalContext::default(),
    )
    .err()
    .unwrap();
    assert!(
        matches!(error, EvalSolveError::CompiledExecution { span: Some(span), .. } if span == spans[1]),
        "{error:?}"
    );
}

#[test]
fn target_family_invalid_failure_ordinal_does_not_fabricate_a_source_owner() {
    let source = PreparedScalarProgramBlock::new(spanned_block(
        vec![
            shifted_variable_residual_row(0, 7.0),
            shifted_variable_residual_row(1, 13.0),
        ],
        "invalid_family_failure.mo",
    ))
    .unwrap();
    let error = super::super::compiled_targets::TargetQueries::prepare(
        &source,
        [(0, 0), (1, 1)],
        &LateFamilyFailure { entry: 9 },
        RowEvalContext::default(),
    )
    .err()
    .unwrap();
    assert!(
        matches!(error, EvalSolveError::CompiledExecution { ref message, span: None } if message.contains("invalid family failure entry 9")),
        "{error:?}"
    );
}
