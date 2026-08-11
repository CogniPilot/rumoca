use super::*;

fn source_span(source: &'static str, start: usize, end: usize) -> Span {
    Span::from_offsets(SourceId::from_source_name(source), start, end)
}

#[test]
fn scalar_program_construction_rejects_missing_output_at_its_source() {
    let span = source_span("MissingOutput.mo", 23, 34);
    let programs = vec![vec![LinearOp::Const { dst: 0, value: 4.0 }]];

    let error = ScalarProgramBlock::with_program_spans(programs, vec![span])
        .expect_err("a complete scalar program must produce a value");

    assert_eq!(error.source_span(), Some(span));
    assert!(matches!(
        error,
        SolveProblemShapeContractError::ScalarProgramMissingOutput {
            node_index: 0,
            program_index: 0,
            ..
        }
    ));
}

#[test]
fn scalar_program_construction_accepts_one_program_with_several_outputs() {
    let span = source_span("VectorExpression.mo", 12, 27);
    let program = vec![
        LinearOp::Const { dst: 0, value: 2.0 },
        LinearOp::Const { dst: 1, value: 3.0 },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::StoreOutput { src: 1 },
    ];

    let block = ScalarProgramBlock::with_program_spans(vec![program], vec![span])
        .expect("a tensor scalar fallback may store several explicit outputs");

    assert_eq!(block.row_count(), 1);
    assert_eq!(block.stored_output_count(), 2);
    assert_eq!(block.program_span(0), Some(span));
}

#[test]
fn scalar_program_construction_accepts_one_compact_strided_output_range() {
    let span = source_span("StridedOutput.mo", 12, 27);
    let program = vec![
        LinearOp::Const { dst: 0, value: 2.0 },
        LinearOp::Const {
            dst: 1,
            value: 99.0,
        },
        LinearOp::Const { dst: 2, value: 3.0 },
        LinearOp::StoreOutputRange {
            start: 0,
            count: 2,
            stride: 2,
        },
    ];

    let block = ScalarProgramBlock::with_program_spans(vec![program], vec![span])
        .expect("a checked affine output range is one compact program boundary");

    assert_eq!(block.stored_output_count(), 2);
    assert_eq!(block.output_indices(), [0, 1]);
}

#[test]
fn scalar_program_construction_rejects_empty_compact_output_range() {
    let span = source_span("EmptyOutputRange.mo", 12, 27);
    let program = vec![
        LinearOp::Const { dst: 0, value: 2.0 },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::StoreOutputRange {
            start: 0,
            count: 0,
            stride: 1,
        },
    ];

    let error = ScalarProgramBlock::with_program_spans(vec![program], vec![span])
        .expect_err("an empty output range cannot construct");

    assert!(matches!(
        error,
        SolveProblemShapeContractError::ScalarProgramRegisterFlow {
            error: ScalarProgramRegisterError::EmptyRegisterRange {
                operation: "StoreOutputRange",
                ..
            },
            ..
        }
    ));
}

#[test]
fn scalar_program_construction_rejects_undefined_register_read_at_its_source() {
    let span = source_span("UndefinedRegister.mo", 41, 52);
    let program = vec![
        LinearOp::Move { dst: 0, src: 3 },
        LinearOp::StoreOutput { src: 0 },
    ];

    let error = ScalarProgramBlock::with_program_spans(vec![program], vec![span])
        .expect_err("a read must be dominated by an earlier register write");

    assert_eq!(error.source_span(), Some(span));
    assert!(matches!(
        error,
        SolveProblemShapeContractError::ScalarProgramRegisterFlow {
            program_index: 0,
            error: ScalarProgramRegisterError::UndefinedRegister {
                op_index: 0,
                operation: "Move",
                register: 3,
            },
            ..
        }
    ));
}

#[test]
fn scalar_program_construction_rejects_store_from_undefined_register() {
    let span = source_span("UndefinedOutput.mo", 8, 19);

    let error = ScalarProgramBlock::with_program_spans(
        vec![vec![LinearOp::StoreOutput { src: 7 }]],
        vec![span],
    )
    .expect_err("StoreOutput must consume a computed value");

    assert_eq!(error.source_span(), Some(span));
    assert!(matches!(
        error,
        SolveProblemShapeContractError::ScalarProgramRegisterFlow {
            error: ScalarProgramRegisterError::UndefinedRegister {
                op_index: 0,
                operation: "StoreOutput",
                register: 7,
            },
            ..
        }
    ));
}

#[test]
fn scalar_program_construction_rejects_dummy_provenance() {
    let error = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        vec![Span::DUMMY],
    )
    .expect_err("every scalar program must carry exact source provenance");

    assert!(matches!(
        error,
        SolveProblemShapeContractError::ScalarProgramMissingProvenance {
            node_index: 0,
            program_index: 0,
            ..
        }
    ));
}

#[test]
fn scalar_program_register_proof_returns_exact_register_count() {
    let program = [
        LinearOp::Const { dst: 4, value: 2.0 },
        LinearOp::Move { dst: 1, src: 4 },
        LinearOp::StoreOutput { src: 1 },
    ];

    let proof = ScalarProgramRegisterFlow::derive(&program)
        .expect("every source register has an earlier definition");

    assert_eq!(proof.register_count(), 5);

    let block = ScalarProgramBlock::with_program_spans(
        vec![program.to_vec()],
        vec![source_span("RegisterCertificate.mo", 0, 12)],
    )
    .expect("checked block retains its construction-owned execution capacity");
    assert_eq!(block.program_register_count(0), Some(5));
    assert_eq!(block.program_register_count(1), None);
}

#[test]
fn guarded_function_fold_construction_requires_defined_activation() {
    let fold = FunctionFoldProgram::checked(
        StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        1,
        0,
        vec![
            LinearOp::LoadFoldCarried { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .expect("construct compact identity fold");
    let program = [
        LinearOp::Const { dst: 0, value: 4.0 },
        LinearOp::GuardedFunctionFold {
            dst_start: 1,
            initial_start: 0,
            capture_start: 0,
            activation: 7,
            program: std::sync::Arc::new(fold),
        },
        LinearOp::StoreOutput { src: 1 },
    ];

    let error = ScalarProgramRegisterFlow::derive(&program)
        .expect_err("a guarded fold activation must be dominated by a definition");

    assert!(matches!(
        error,
        ScalarProgramRegisterError::UndefinedRegister {
            op_index: 1,
            operation: "GuardedFunctionFold",
            register: 7,
        }
    ));
}

#[test]
fn scalar_program_register_proof_accepts_unused_dot_product_stride_gaps() {
    let program = [
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::Const { dst: 4, value: 3.0 },
        LinearOp::Const { dst: 5, value: 4.0 },
        LinearOp::DotProduct {
            dst: 6,
            lhs_start: 0,
            rhs_start: 1,
            count: 2,
            lhs_stride: 4,
            rhs_stride: 4,
        },
        LinearOp::StoreOutput { src: 6 },
    ];

    let proof = ScalarProgramRegisterFlow::derive(&program)
        .expect("only registers selected by each stride are dot-product inputs");

    assert_eq!(proof.register_count(), 7);
}

#[test]
fn runtime_tensor_projection_is_one_rank_sized_checked_operation() {
    let program = [
        LinearOp::Const {
            dst: 0,
            value: 10.0,
        },
        LinearOp::Const {
            dst: 1,
            value: 20.0,
        },
        LinearOp::Const {
            dst: 2,
            value: 30.0,
        },
        LinearOp::Const {
            dst: 3,
            value: 40.0,
        },
        LinearOp::Const { dst: 4, value: 2.0 },
        LinearOp::LoadIndexedRegister {
            dst: 5,
            base: 0,
            stride: 1,
            dimensions: Box::new([2, 2]),
            indices: Box::new([TensorIndex::Runtime(4), TensorIndex::Constant(0)]),
        },
        LinearOp::StoreOutput { src: 5 },
    ];

    let proof = ScalarProgramRegisterFlow::derive(&program)
        .expect("rank-sized runtime projection has a complete register proof");

    assert_eq!(proof.register_count(), 6);
    assert_eq!(
        program
            .iter()
            .filter(|op| matches!(op, LinearOp::LoadIndexedRegister { .. }))
            .count(),
        1
    );
    assert!(
        !program
            .iter()
            .any(|op| matches!(op, LinearOp::Select { .. }))
    );
}

#[test]
fn scalar_program_register_proof_rejects_undefined_strided_dot_product_input() {
    let program = [
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::Const { dst: 4, value: 3.0 },
        LinearOp::DotProduct {
            dst: 6,
            lhs_start: 0,
            rhs_start: 1,
            count: 2,
            lhs_stride: 4,
            rhs_stride: 4,
        },
        LinearOp::StoreOutput { src: 6 },
    ];

    let error = ScalarProgramRegisterFlow::derive(&program)
        .expect_err("every register selected by a dot-product stride must be defined");

    assert!(matches!(
        error,
        ScalarProgramRegisterError::UndefinedRegister {
            op_index: 3,
            operation: "DotProduct",
            register: 5,
        }
    ));
}

#[test]
fn scalar_program_construction_rejects_a_hole_in_register_range() {
    let span = source_span("IncompleteRandomState.mo", 17, 38);
    let program = vec![
        LinearOp::Const { dst: 5, value: 1.0 },
        LinearOp::Const { dst: 7, value: 3.0 },
        LinearOp::RandomResult {
            dst: 8,
            generator: RandomGenerator::Xorshift64Star,
            state_start: 5,
            state_len: 3,
        },
        LinearOp::StoreOutput { src: 8 },
    ];

    let error = ScalarProgramBlock::with_program_spans(vec![program], vec![span])
        .expect_err("every register in a source range must be defined");

    assert_eq!(error.source_span(), Some(span));
    assert!(matches!(
        error,
        SolveProblemShapeContractError::ScalarProgramRegisterFlow {
            error: ScalarProgramRegisterError::UndefinedRegister {
                op_index: 2,
                operation: "RandomResult",
                register: 6,
            },
            ..
        }
    ));
}

#[test]
fn scalar_program_construction_rejects_zero_transpose_element_width() {
    let span = source_span("InvalidTranspose.mo", 11, 33);
    let program = vec![
        LinearOp::TensorTranspose {
            dst_start: 0,
            src_start: 0,
            rows: 2,
            columns: 3,
            element_width: 0,
            lanes: 1,
        },
        LinearOp::StoreOutput { src: 0 },
    ];

    let error = ScalarProgramBlock::with_program_spans(vec![program], vec![span])
        .expect_err("a transpose must own a nonzero trailing element width");

    assert_eq!(error.source_span(), Some(span));
    assert!(
        matches!(
            &error,
            SolveProblemShapeContractError::ScalarProgramRegisterFlow {
                error: ScalarProgramRegisterError::InvalidTensorProjection {
                    op_index: 0,
                    reason: "tensor transpose has an invalid shape, element width, or lane count",
                },
                ..
            }
        ),
        "unexpected construction error: {error:?}"
    );
}

#[test]
fn scalar_program_construction_accepts_one_tensor_cross_owner() {
    let span = source_span("TensorCross.mo", 7, 22);
    let mut program = (0..6)
        .map(|dst| LinearOp::Const {
            dst,
            value: f64::from(dst),
        })
        .collect::<Vec<_>>();
    program.push(LinearOp::TensorCross {
        dst_start: 6,
        lhs_start: 0,
        rhs_start: 3,
        lanes: 1,
    });
    program.push(LinearOp::StoreOutputRange {
        start: 6,
        count: 3,
        stride: 1,
    });

    let block = ScalarProgramBlock::with_program_spans(vec![program], vec![span])
        .expect("a checked cross product owns two length-three input ranges");

    assert_eq!(block.stored_output_count(), 3);
}

#[test]
fn scalar_program_construction_rejects_invalid_tensor_cross_lanes() {
    let span = source_span("InvalidTensorCross.mo", 7, 29);
    let program = vec![
        LinearOp::TensorCross {
            dst_start: 0,
            lhs_start: 0,
            rhs_start: 0,
            lanes: 3,
        },
        LinearOp::StoreOutput { src: 0 },
    ];

    let error = ScalarProgramBlock::with_program_spans(vec![program], vec![span])
        .expect_err("a tensor cross product accepts only primal or interleaved dual lanes");

    assert_eq!(error.source_span(), Some(span));
    assert!(matches!(
        error,
        SolveProblemShapeContractError::ScalarProgramRegisterFlow {
            error: ScalarProgramRegisterError::InvalidTensorProjection {
                op_index: 0,
                reason: "tensor cross product has an invalid lane count",
            },
            ..
        }
    ));
}

fn conditional_test_region(values: &[f64]) -> Vec<LinearOp> {
    let mut program = values
        .iter()
        .copied()
        .enumerate()
        .map(|(dst, value)| LinearOp::Const {
            dst: dst as Reg,
            value,
        })
        .collect::<Vec<_>>();
    program.extend((0..values.len()).map(|src| LinearOp::StoreOutput { src: src as Reg }));
    program
}

fn conditional_capture_range_region(index_start: usize, count: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadFunctionConditionalCaptureRange {
            dst_start: 0,
            index_start,
            count,
        },
        LinearOp::StoreOutputRange {
            start: 0,
            count,
            stride: 1,
        },
    ]
}

#[test]
fn function_conditional_construction_accepts_one_compact_capture_range() {
    let program = FunctionConditionalProgram::checked(
        3,
        [3],
        [(
            vec![
                LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            conditional_capture_range_region(0, 3),
        )],
        conditional_capture_range_region(0, 3),
    )
    .expect("one checked capture range preserves the aggregate ABI");

    assert_eq!(program.capture_count, 3);
    assert_eq!(program.arms[0].result_register_count, 3);
    assert_eq!(program.fallback_register_count, 3);
    assert!(matches!(
        program.arms[0].result[0],
        LinearOp::LoadFunctionConditionalCaptureRange {
            dst_start: 0,
            index_start: 0,
            count: 3,
        }
    ));
}

#[test]
fn function_conditional_construction_rejects_empty_or_out_of_frame_capture_ranges() {
    for (index_start, count) in [(0, 0), (2, 2)] {
        let error = FunctionConditionalProgram::checked(
            3,
            [3],
            [(
                vec![
                    LinearOp::Const { dst: 0, value: 1.0 },
                    LinearOp::StoreOutput { src: 0 },
                ],
                conditional_capture_range_region(index_start, count),
            )],
            conditional_test_region(&[1.0, 2.0, 3.0]),
        )
        .expect_err("an invalid capture range cannot construct");

        assert!(matches!(
            error,
            ScalarProgramRegisterError::InvalidFunctionConditional {
                reason: "capture range load is empty, overflows, or exceeds the capture ABI",
                ..
            }
        ));
    }
}

#[test]
fn function_conditional_construction_preserves_one_correlated_result_tuple() {
    let span = source_span("CorrelatedConditional.mo", 4, 42);
    let program = std::sync::Arc::new(
        FunctionConditionalProgram::checked(
            0,
            [1, 1],
            [(
                conditional_test_region(&[1.0]),
                conditional_test_region(&[2.0, 3.0]),
            )],
            conditional_test_region(&[4.0, 5.0]),
        )
        .expect("one checked condition owns two correlated target values"),
    );
    let row = vec![
        LinearOp::FunctionConditional {
            dst_start: 0,
            capture_start: 0,
            program,
        },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::StoreOutput { src: 1 },
    ];

    let block = ScalarProgramBlock::with_program_spans(vec![row], vec![span])
        .expect("the correlated tuple dominates both projections");
    assert_eq!(block.programs()[0][0].dst_register_count(), 2);
}

#[test]
fn function_conditional_construction_rejects_a_partial_branch_tuple() {
    let error = FunctionConditionalProgram::checked(
        0,
        [1, 1],
        [(
            conditional_test_region(&[1.0]),
            conditional_test_region(&[2.0]),
        )],
        conditional_test_region(&[4.0, 5.0]),
    )
    .expect_err("every branch must define every correlated target");

    assert!(matches!(
        error,
        ScalarProgramRegisterError::InvalidFunctionConditional {
            reason: "branch region does not completely define the result tuple",
            ..
        }
    ));
}

#[test]
fn scalar_program_block_rejects_one_conditional_owner_with_two_bodies() {
    let span = source_span("ConditionalOwnerMismatch.mo", 0, 32);
    let owner = FunctionConditionalOwnerId::checked(1).expect("nonzero fixture owner");
    let make = |selected| {
        std::sync::Arc::new(
            FunctionConditionalProgram::checked_owned(
                owner,
                0,
                [1],
                [(
                    conditional_test_region(&[1.0]),
                    conditional_test_region(&[selected]),
                )],
                conditional_test_region(&[0.0]),
            )
            .expect("each fixture body is independently valid"),
        )
    };
    let rows = [make(2.0), make(3.0)]
        .into_iter()
        .map(|program| {
            vec![
                LinearOp::FunctionConditional {
                    dst_start: 0,
                    capture_start: 0,
                    program,
                },
                LinearOp::StoreOutput { src: 0 },
            ]
        })
        .collect();

    let error = ScalarProgramBlock::with_program_spans(rows, vec![span, span])
        .expect_err("one issued owner cannot identify two checked bodies");
    assert!(matches!(
        error,
        SolveProblemShapeContractError::FunctionConditionalOwnerMismatch { owner: 1, .. }
    ));
}

fn sparse_wire_fixture() -> ScalarProgramBlock {
    let span = source_span("SparseOutputs.mo", 11, 24);
    ScalarProgramBlock::with_output_indices(
        vec![
            vec![
                LinearOp::Const { dst: 0, value: 2.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 5.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
        ],
        vec![span, span],
        vec![9, 2],
    )
    .expect("fixture has exact spans, outputs, and register flow")
}

#[test]
fn scalar_program_wire_roundtrip_preserves_sparse_output_identity() {
    let block = sparse_wire_fixture();
    let value = serde_json::to_value(&block).expect("serialize current scalar-program wire");
    assert!(
        value.get("program_register_counts").is_none(),
        "the execution certificate is reconstructed, not serialized as a second IR"
    );
    let decoded: ScalarProgramBlock =
        serde_json::from_value(value).expect("decode through the checked constructor");

    assert_eq!(decoded.programs(), block.programs());
    assert_eq!(decoded.program_spans(), block.program_spans());
    assert_eq!(decoded.output_indices(), [9, 2]);
    assert_eq!(decoded.program_register_count(0), Some(1));
    assert_eq!(decoded.program_register_count(1), Some(1));
}

#[test]
fn scalar_program_wire_rejects_missing_provenance() {
    let mut value =
        serde_json::to_value(sparse_wire_fixture()).expect("serialize current scalar-program wire");
    value["program_spans"][0] =
        serde_json::to_value(Span::DUMMY).expect("serialize missing provenance marker");

    let error = serde_json::from_value::<ScalarProgramBlock>(value)
        .expect_err("wire decoding must not bypass exact per-program provenance");

    assert!(
        error
            .to_string()
            .contains("scalar program 0 has no source provenance"),
        "unexpected error: {error}"
    );
}

#[test]
fn scalar_program_wire_rejects_invalid_register_flow() {
    let mut value =
        serde_json::to_value(sparse_wire_fixture()).expect("serialize current scalar-program wire");
    value["programs"][0]
        .as_array_mut()
        .expect("serialized program is an array")
        .remove(0);

    let error = serde_json::from_value::<ScalarProgramBlock>(value)
        .expect_err("wire decoding must derive register flow through the checked constructor");

    assert!(
        error.to_string().contains(
            "scalar program 0 has invalid register flow: StoreOutput op 0 reads undefined register r0"
        ),
        "unexpected error: {error}"
    );
}

#[test]
fn scalar_program_wire_rejects_empty_compact_output_range() {
    let span = source_span("ForgedOutputRange.mo", 4, 20);
    let block = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::Const { dst: 0, value: 2.0 },
            LinearOp::StoreOutput { src: 0 },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 1,
                stride: 1,
            },
        ]],
        vec![span],
    )
    .expect("construct checked output-range fixture");
    let mut value = serde_json::to_value(block).expect("serialize checked output-range fixture");
    value["programs"][0][2]["StoreOutputRange"]["count"] = serde_json::json!(0);
    value["output_indices"]
        .as_array_mut()
        .expect("serialized output indices are an array")
        .pop();

    let error = serde_json::from_value::<ScalarProgramBlock>(value)
        .expect_err("wire replay must reject an empty compact output range");

    assert!(
        error
            .to_string()
            .contains("StoreOutputRange op 2 reads an empty register range"),
        "unexpected error: {error}"
    );
}

#[test]
fn scalar_program_wire_rejects_forged_conditional_capture_range() {
    let span = source_span("ForgedCaptureRange.mo", 4, 24);
    let conditional = FunctionConditionalProgram::checked(
        3,
        [3],
        [(
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            conditional_capture_range_region(0, 3),
        )],
        conditional_test_region(&[4.0, 5.0, 6.0]),
    )
    .expect("construct checked capture-range fixture");
    let block = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::Const { dst: 1, value: 2.0 },
            LinearOp::Const { dst: 2, value: 3.0 },
            LinearOp::FunctionConditional {
                dst_start: 3,
                capture_start: 0,
                program: std::sync::Arc::new(conditional),
            },
            LinearOp::StoreOutputRange {
                start: 3,
                count: 3,
                stride: 1,
            },
        ]],
        vec![span],
    )
    .expect("construct checked scalar-program fixture");
    let mut value = serde_json::to_value(block).expect("serialize capture-range fixture");
    value["programs"][0][3]["FunctionConditional"]["program"]["arms"][0]["result"][0]["LoadFunctionConditionalCaptureRange"]
        ["count"] = serde_json::json!(0);

    let error = serde_json::from_value::<ScalarProgramBlock>(value)
        .expect_err("wire replay must reject a forged conditional capture range");

    assert!(
        error
            .to_string()
            .contains("capture range load is empty, overflows, or exceeds the capture ABI"),
        "unexpected error: {error}"
    );
}

#[test]
fn scalar_program_wire_rejects_non_current_fields() {
    let mut value =
        serde_json::to_value(sparse_wire_fixture()).expect("serialize current scalar-program wire");
    value["removed_programs"] = serde_json::json!([]);

    let error = serde_json::from_value::<ScalarProgramBlock>(value)
        .expect_err("the current wire must reject removed fields");

    assert!(
        error
            .to_string()
            .contains("unknown field `removed_programs`"),
        "unexpected error: {error}"
    );
}
