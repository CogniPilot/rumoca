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
fn scalar_program_construction_and_wire_replay_refuse_duplicate_output_identity() {
    let span = source_span("DuplicateOutput.mo", 12, 27);
    let programs = vec![
        vec![
            LinearOp::Const { dst: 0, value: 2.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        vec![
            LinearOp::Const { dst: 0, value: 3.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
    ];
    let error =
        ScalarProgramBlock::with_output_indices(programs.clone(), vec![span, span], vec![0, 0])
            .expect_err("one logical output cannot have two scalar owners");
    assert_eq!(error.source_span(), Some(span));
    assert!(matches!(
        error,
        SolveProblemShapeContractError::DuplicateIndex {
            context: "ScalarProgramBlock.output_indices",
            index: 0,
            ..
        }
    ));

    let wire = serde_json::json!({
        "programs": programs,
        "program_spans": [span, span],
        "output_indices": [0, 0]
    });
    let replay = serde_json::from_value::<ScalarProgramBlock>(wire)
        .expect_err("wire replay cannot mint duplicate output identity");
    assert!(replay.to_string().contains("duplicate index 0"));
}

#[test]
fn scalar_program_construction_refuses_stored_output_count_overflow() {
    let span = source_span("OutputOverflow.mo", 12, 27);
    let program = vec![
        LinearOp::StoreOutputRange {
            start: 0,
            count: usize::MAX,
            stride: 0,
        },
        LinearOp::StoreOutput { src: 0 },
    ];
    let error = ScalarProgramBlock::with_output_indices(vec![program], vec![span], Vec::new())
        .expect_err("stored-output cardinality must use checked arithmetic");
    assert!(matches!(
        error,
        SolveProblemShapeContractError::OutputIndexOverflow { .. }
    ));
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
fn scalar_program_construction_rejects_a_second_definition() {
    let span = source_span("DuplicateDefinition.mo", 17, 29);
    let program = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::Const { dst: 0, value: 2.0 },
        LinearOp::StoreOutput { src: 0 },
    ];

    let error = ScalarProgramBlock::with_program_spans(vec![program], vec![span])
        .expect_err("a second destination write must not mint a checked program");

    assert_eq!(error.source_span(), Some(span));
    assert!(matches!(
        error,
        SolveProblemShapeContractError::ScalarProgramRegisterFlow {
            program_index: 0,
            error: ScalarProgramRegisterError::DestinationRegisterAlreadyDefined {
                op_index: 1,
                operation: "Const",
                register: 0,
            },
            ..
        }
    ));
}

#[test]
fn scalar_program_construction_rejects_a_partially_overlapping_destination_range() {
    let span = source_span("OverlappingDefinitionRange.mo", 8, 31);
    let program = vec![
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 3,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::StoreOutput { src: 0 },
    ];

    let error = ScalarProgramBlock::with_program_spans(vec![program], vec![span])
        .expect_err("an overlapping destination range must not mint a checked program");

    assert_eq!(error.source_span(), Some(span));
    assert!(matches!(
        error,
        SolveProblemShapeContractError::ScalarProgramRegisterFlow {
            program_index: 0,
            error: ScalarProgramRegisterError::DestinationRegisterAlreadyDefined {
                op_index: 1,
                operation: "TensorLoad",
                register: 1,
            },
            ..
        }
    ));
}

#[test]
fn scalar_program_construction_accepts_disjoint_destination_ranges() {
    let span = source_span("DisjointDefinitionRanges.mo", 2, 24);
    let program = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::TensorLoad {
            dst_start: 1,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 2,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];

    let block = ScalarProgramBlock::with_program_spans(vec![program], vec![span])
        .expect("disjoint destination ranges have one definition per register");

    assert_eq!(block.program_register_count(0), Some(3));
}

#[test]
fn scalar_program_wire_rejects_a_second_destination_definition() {
    let span = source_span("DuplicateDefinitionWire.mo", 3, 18);
    let block = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::Const { dst: 1, value: 2.0 },
            LinearOp::StoreOutput { src: 1 },
        ]],
        vec![span],
    )
    .expect("construct an SSA scalar-program wire fixture");
    let mut wire = serde_json::to_value(block).expect("serialize the SSA fixture");
    wire["programs"][0][1]["Const"]["dst"] = serde_json::json!(0);

    let error = serde_json::from_value::<ScalarProgramBlock>(wire)
        .expect_err("wire replay must not issue a block for a second destination definition");

    assert!(
        error
            .to_string()
            .contains("Const op 1 redefines destination register r0"),
        "unexpected duplicate-definition wire error: {error}"
    );
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
fn scalar_program_execution_iterator_keeps_each_programs_issued_evidence_correlated() {
    let span = source_span("ExecutionCertificate.mo", 0, 18);
    let block = ScalarProgramBlock::with_program_spans(
        vec![
            vec![
                LinearOp::Const { dst: 1, value: 2.0 },
                LinearOp::StoreOutput { src: 1 },
            ],
            vec![
                LinearOp::Const { dst: 3, value: 4.0 },
                LinearOp::StoreOutput { src: 3 },
            ],
        ],
        vec![span, span],
    )
    .expect("checked programs retain their own execution evidence");

    let executions = block.execution_programs().collect::<Vec<_>>();

    assert_eq!(executions.len(), 2);
    assert_eq!(executions[0].register_count(), 2);
    assert_eq!(executions[0].output_sources(), [1]);
    assert_eq!(executions[1].register_count(), 4);
    assert_eq!(executions[1].output_sources(), [3]);
}

#[test]
fn guarded_function_fold_construction_requires_defined_activation() {
    let fold = FunctionFoldProgram::checked(
        StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::new(0),
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
fn runtime_tensor_projection_rejects_unproved_or_out_of_domain_indices() {
    for (index, reason) in [
        (
            f64::NAN,
            "runtime tensor index lacks a construction-issued exact integer domain",
        ),
        (
            1.5,
            "runtime tensor index lacks a construction-issued exact integer domain",
        ),
        (0.0, "runtime tensor index domain escapes its selected axis"),
        (3.0, "runtime tensor index domain escapes its selected axis"),
    ] {
        let program = [
            LinearOp::Const { dst: 0, value: 7.0 },
            LinearOp::Const { dst: 1, value: 8.0 },
            LinearOp::Const {
                dst: 2,
                value: index,
            },
            LinearOp::LoadIndexedRegister {
                dst: 3,
                base: 0,
                stride: 1,
                dimensions: Box::new([2]),
                indices: Box::new([TensorIndex::Runtime(2)]),
            },
            LinearOp::StoreOutput { src: 3 },
        ];

        let error = ScalarProgramRegisterFlow::derive(&program)
            .expect_err("an invalid runtime index must fail program construction");
        assert!(matches!(
            error,
            ScalarProgramRegisterError::InvalidTensorProjection {
                op_index: 3,
                reason: actual,
            } if actual == reason
        ));
    }
}

#[test]
fn runtime_tensor_projection_accepts_both_proved_boundaries() {
    for index in [1.0, 2.0] {
        let program = [
            LinearOp::Const { dst: 0, value: 7.0 },
            LinearOp::Const { dst: 1, value: 8.0 },
            LinearOp::Const {
                dst: 2,
                value: index,
            },
            LinearOp::LoadIndexedRegister {
                dst: 3,
                base: 0,
                stride: 1,
                dimensions: Box::new([2]),
                indices: Box::new([TensorIndex::Runtime(2)]),
            },
            LinearOp::StoreOutput { src: 3 },
        ];

        ScalarProgramRegisterFlow::derive(&program)
            .expect("a proved one-based boundary index constructs");
    }
}

#[test]
fn runtime_tensor_projection_rejects_an_unproved_dynamic_input() {
    let program = [
        LinearOp::Const { dst: 0, value: 7.0 },
        LinearOp::Const { dst: 1, value: 8.0 },
        LinearOp::LoadY { dst: 2, index: 0 },
        LinearOp::LoadIndexedRegister {
            dst: 3,
            base: 0,
            stride: 1,
            dimensions: Box::new([2]),
            indices: Box::new([TensorIndex::Runtime(2)]),
        },
        LinearOp::StoreOutput { src: 3 },
    ];

    let error = ScalarProgramRegisterFlow::derive(&program)
        .expect_err("input-driven tensor indexing has no static domain proof");
    assert!(matches!(
        error,
        ScalarProgramRegisterError::InvalidTensorProjection {
            op_index: 3,
            reason: "runtime tensor index lacks a construction-issued exact integer domain",
        }
    ));
}

#[test]
fn comparison_cannot_reset_unproved_input_into_runtime_index_evidence() {
    let program = [
        LinearOp::Const { dst: 0, value: 7.0 },
        LinearOp::Const { dst: 1, value: 8.0 },
        LinearOp::LoadY { dst: 2, index: 0 },
        LinearOp::Const { dst: 3, value: 0.0 },
        LinearOp::Compare {
            dst: 4,
            op: CompareOp::Eq,
            lhs: 2,
            rhs: 3,
        },
        LinearOp::Const { dst: 5, value: 1.0 },
        LinearOp::Binary {
            dst: 6,
            op: BinaryOp::Add,
            lhs: 4,
            rhs: 5,
        },
        LinearOp::LoadIndexedRegister {
            dst: 7,
            base: 0,
            stride: 1,
            dimensions: Box::new([2]),
            indices: Box::new([TensorIndex::Runtime(6)]),
        },
        LinearOp::StoreOutput { src: 7 },
    ];

    let error = ScalarProgramRegisterFlow::derive(&program)
        .expect_err("comparison output must not manufacture exact-integer evidence");
    assert!(matches!(
        error,
        ScalarProgramRegisterError::InvalidTensorProjection {
            op_index: 7,
            reason: "runtime tensor index lacks a construction-issued exact integer domain",
        }
    ));
}

#[test]
fn deleted_indexed_input_wire_variants_are_unknown() {
    for suffix in ["P", "Seed"] {
        let kind = ["Load", "Indexed", suffix].concat();
        let mut tagged = serde_json::Map::new();
        tagged.insert(
            kind.clone(),
            serde_json::json!({
                "dst": 0,
                "base": 0,
                "count": 1,
                "index": 0
            }),
        );
        let error = serde_json::from_value::<LinearOp>(serde_json::Value::Object(tagged))
            .expect_err("deleted scalar indexed-input wire tags must not compat-decode");
        let message = error.to_string();
        assert!(
            message.contains("unknown variant") && message.contains(&kind),
            "unexpected deleted-tag rejection: {message}"
        );
    }
}

#[test]
fn function_fold_binder_domain_proves_affine_runtime_indexing() {
    let domain = StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::new(0),
            display_name: "i".to_string(),
            lower: 1,
            upper: 2,
            step: 1,
        }],
    };
    let program = FunctionFoldProgram::checked(
        domain,
        5,
        5,
        vec![
            LinearOp::LoadFoldIndex {
                dst: 0,
                dimension: 0,
            },
            LinearOp::Const { dst: 1, value: 3.0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::LoadIndexedFoldCarried {
                dst: 3,
                base: 0,
                stride: 1,
                dimensions: Box::new([5]),
                indices: Box::new([TensorIndex::Runtime(2)]),
            },
            LinearOp::LoadIndexedFoldCapture {
                dst: 4,
                base: 0,
                stride: 1,
                dimensions: Box::new([5]),
                indices: Box::new([TensorIndex::Runtime(2)]),
            },
            LinearOp::StoreOutput { src: 3 },
            LinearOp::StoreOutput { src: 4 },
            LinearOp::StoreOutput { src: 3 },
            LinearOp::StoreOutput { src: 3 },
            LinearOp::StoreOutput { src: 3 },
        ],
    )
    .expect("the fold domain proves every affine index is in 4..=5");

    assert_eq!(program.register_count(), 5);
}

#[test]
fn function_fold_rejects_a_binder_domain_outside_the_tensor_axis() {
    let domain = StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::new(0),
            display_name: "i".to_string(),
            lower: 0,
            upper: 1,
            step: 1,
        }],
    };
    let error = FunctionFoldProgram::checked(
        domain,
        2,
        0,
        vec![
            LinearOp::LoadFoldIndex {
                dst: 0,
                dimension: 0,
            },
            LinearOp::LoadIndexedFoldCarried {
                dst: 1,
                base: 0,
                stride: 1,
                dimensions: Box::new([2]),
                indices: Box::new([TensorIndex::Runtime(0)]),
            },
            LinearOp::StoreOutput { src: 1 },
            LinearOp::StoreOutput { src: 1 },
        ],
    )
    .expect_err("a fold binder whose domain includes zero cannot authorize indexing");

    assert!(matches!(
        error,
        ScalarProgramRegisterError::InvalidTensorProjection {
            op_index: 1,
            reason: "runtime tensor index domain escapes its selected axis",
        }
    ));
}

#[test]
fn function_fold_rejects_an_invalid_domain_before_minting_an_index_proof() {
    let error = FunctionFoldProgram::checked(
        StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::new(0),
                display_name: "i".to_string(),
                lower: 1,
                upper: 2,
                step: 0,
            }],
        },
        1,
        0,
        vec![
            LinearOp::LoadFoldIndex {
                dst: 0,
                dimension: 0,
            },
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .expect_err("an unchecked fold domain cannot authorize a runtime coordinate");

    assert!(matches!(
        error,
        ScalarProgramRegisterError::InvalidFunctionFold {
            op_index: 0,
            reason: "function fold has an invalid structured domain",
        }
    ));
}

#[test]
fn function_fold_wire_reissues_runtime_index_evidence() {
    let program = FunctionFoldProgram::checked(
        StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::new(0),
                display_name: "i".to_string(),
                lower: 1,
                upper: 2,
                step: 1,
            }],
        },
        2,
        0,
        vec![
            LinearOp::LoadFoldIndex {
                dst: 0,
                dimension: 0,
            },
            LinearOp::LoadIndexedFoldCarried {
                dst: 1,
                base: 0,
                stride: 1,
                dimensions: Box::new([2]),
                indices: Box::new([TensorIndex::Runtime(0)]),
            },
            LinearOp::StoreOutput { src: 1 },
            LinearOp::StoreOutput { src: 1 },
        ],
    )
    .expect("construct a fold with issued runtime-index evidence");
    let mut wire = serde_json::to_value(program).expect("serialize checked fold");
    wire["domain"]["binders"][0]["lower"] = serde_json::json!(0);

    let error = serde_json::from_value::<FunctionFoldProgram>(wire)
        .expect_err("wire replay must reissue the binder-domain index proof");

    assert!(
        error
            .to_string()
            .contains("runtime tensor index domain escapes its selected axis"),
        "unexpected forged fold rejection: {error}"
    );
}

#[test]
fn tensor_update_and_slice_require_proved_coordinate_domains() {
    let update = [
        LinearOp::Const { dst: 0, value: 7.0 },
        LinearOp::Const { dst: 1, value: 8.0 },
        LinearOp::Const { dst: 2, value: 9.0 },
        LinearOp::Const { dst: 3, value: 0.0 },
        LinearOp::TensorUpdate {
            dst_start: 4,
            base_start: 0,
            value_start: 2,
            dimensions: Box::new([2]),
            subscripts: Box::new([TensorUpdateSubscript::Index(TensorIndex::Runtime(3))]),
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 4,
            count: 2,
            stride: 1,
        },
    ];
    let error = ScalarProgramRegisterFlow::derive(&update)
        .expect_err("an invalid update index cannot silently leave its base unchanged");
    assert!(matches!(
        error,
        ScalarProgramRegisterError::InvalidTensorProjection {
            op_index: 4,
            reason: "runtime tensor index domain escapes its selected axis",
        }
    ));

    let slice = [
        LinearOp::Const { dst: 0, value: 7.0 },
        LinearOp::Const { dst: 1, value: 8.0 },
        LinearOp::Const { dst: 2, value: 9.0 },
        LinearOp::Const {
            dst: 3,
            value: 10.0,
        },
        LinearOp::Const { dst: 4, value: 1.0 },
        LinearOp::Const { dst: 5, value: 3.0 },
        LinearOp::TensorUpdate {
            dst_start: 6,
            base_start: 0,
            value_start: 2,
            dimensions: Box::new([2]),
            subscripts: Box::new([TensorUpdateSubscript::Slice {
                start: 4,
                dimensions: Box::new([2]),
            }]),
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 6,
            count: 2,
            stride: 1,
        },
    ];
    let error = ScalarProgramRegisterFlow::derive(&slice)
        .expect_err("every compact slice coordinate requires the base-axis proof");
    assert!(matches!(
        error,
        ScalarProgramRegisterError::InvalidTensorProjection {
            op_index: 6,
            reason: "runtime tensor index domain escapes its selected axis",
        }
    ));
}

#[test]
fn fold_tensor_update_rejects_an_empty_axis_without_panicking() {
    let error = FunctionFoldProgram::checked(
        StructuredIndexDomain { binders: vec![] },
        1,
        0,
        vec![LinearOp::StoreOutputFoldTensorUpdate {
            source_base: 0,
            source_stride: 1,
            dimensions: Box::new([0]),
            updates: Box::new([crate::FoldTensorUpdate {
                subscripts: Box::new([crate::TensorSubscript::Whole]),
                condition: None,
                value_start: 0,
                value_stride: 1,
            }]),
            nodes: Box::new([crate::FoldTensorNode::Update { base: 0, update: 0 }]),
            result: 1,
            lanes: 1,
        }],
    )
    .expect_err("a zero tensor axis must be rejected before extent arithmetic");

    assert!(matches!(
        error,
        ScalarProgramRegisterError::InvalidTensorProjection {
            op_index: 0,
            reason: "tensor update has no checked rank or patches",
        }
    ));
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

    assert_eq!(program.capture_count(), 3);
    assert_eq!(program.arms()[0].result_register_count(), 3);
    assert_eq!(program.fallback_register_count(), 3);
    assert!(matches!(
        program.arms()[0].result()[0],
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
fn function_conditional_wire_rederives_result_and_region_capacities() {
    let program = FunctionConditionalProgram::checked(
        0,
        [1],
        [(
            conditional_test_region(&[1.0]),
            conditional_test_region(&[2.0]),
        )],
        conditional_test_region(&[3.0]),
    )
    .expect("construct a complete conditional wire fixture");

    for (pointer, reason) in [
        (
            "/result_count",
            "stored result width does not match the target tuple",
        ),
        (
            "/arms/0/condition_register_count",
            "stored region register capacity does not match its body",
        ),
        (
            "/arms/0/result_register_count",
            "stored region register capacity does not match its body",
        ),
        (
            "/fallback_register_count",
            "stored region register capacity does not match its body",
        ),
    ] {
        let mut wire = serde_json::to_value(&program).expect("serialize checked conditional");
        *wire
            .pointer_mut(pointer)
            .expect("current conditional wire field") = serde_json::json!(2);
        let error = serde_json::from_value::<FunctionConditionalProgram>(wire)
            .expect_err("wire replay must rederive every stored conditional capacity");
        assert_eq!(
            error.to_string(),
            format!("FunctionConditional op 0 is invalid: {reason}"),
            "unexpected rejection for {pointer}"
        );
    }
}

#[test]
fn function_conditional_owner_wire_refuses_a_private_zero_identity() {
    let owner = FunctionConditionalOwnerId::checked(7).expect("nonzero fixture owner");
    let program = FunctionConditionalProgram::checked_owned(
        owner,
        0,
        [1],
        [(
            conditional_test_region(&[1.0]),
            conditional_test_region(&[2.0]),
        )],
        conditional_test_region(&[3.0]),
    )
    .expect("construct an owned conditional wire fixture");
    let issued = serde_json::to_value(&program).expect("serialize the owned conditional");

    assert_eq!(
        serde_json::from_value::<FunctionConditionalProgram>(issued.clone())
            .expect("an issued owner identity replays unchanged"),
        program
    );

    let mut forged = issued;
    forged["owner"] = serde_json::json!(0);
    let error = serde_json::from_value::<FunctionConditionalProgram>(forged)
        .expect_err("a private zero cannot decode into an issued owner identity");

    assert_eq!(
        error.to_string(),
        "FunctionConditional op 0 is invalid: owner identity is zero"
    );
    assert!(FunctionConditionalOwnerId::checked(0).is_none());
}

#[test]
fn function_conditional_wire_refuses_forged_capture_and_arm_counts() {
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
    .expect("construct a capture-bearing conditional wire fixture");
    let issued = serde_json::to_value(&program).expect("serialize the capture-bearing conditional");

    let mut narrowed = issued.clone();
    narrowed["capture_count"] = serde_json::json!(2);
    let error = serde_json::from_value::<FunctionConditionalProgram>(narrowed)
        .expect_err("a narrowed capture ABI cannot admit its own capture reads");
    assert_eq!(
        error.to_string(),
        "FunctionConditional op 0 is invalid: capture range load is empty, overflows, or exceeds the capture ABI"
    );

    let mut emptied = issued;
    emptied["arms"] = serde_json::json!([]);
    let error = serde_json::from_value::<FunctionConditionalProgram>(emptied)
        .expect_err("a conditional without an ordered condition cannot decode");
    assert_eq!(
        error.to_string(),
        "FunctionConditional op 0 is invalid: ordered condition list is empty"
    );
}

#[test]
fn function_fold_wire_refuses_forged_carried_capture_and_domain_counts() {
    let program = FunctionFoldProgram::checked(
        StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::new(0),
                display_name: "i".to_string(),
                lower: 1,
                upper: 3,
                step: 1,
            }],
        },
        1,
        1,
        vec![
            LinearOp::LoadFoldCarried { dst: 0, index: 0 },
            LinearOp::LoadFoldCapture { dst: 1, index: 0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ],
    )
    .expect("construct a complete fold wire fixture");
    assert_eq!(program.domain_scalar_count(), 3);
    let issued = serde_json::to_value(&program).expect("serialize the checked fold");

    assert_eq!(
        serde_json::from_value::<FunctionFoldProgram>(issued.clone())
            .expect("an issued fold replays unchanged"),
        program
    );

    for (field, forged, message) in [
        (
            "carried_count",
            2,
            "FunctionFold op 0 is invalid: update output count does not match carried tuple",
        ),
        (
            "capture_count",
            0,
            "LoadFoldCapture op 1 projects element 0 from range length 0",
        ),
        (
            "register_count",
            9,
            "FunctionFold op 0 is invalid: stored update register count does not match its body",
        ),
        (
            "domain_scalar_count",
            9,
            "FunctionFold op 0 is invalid: stored domain cardinality does not match its structured domain",
        ),
    ] {
        let mut wire = issued.clone();
        wire[field] = serde_json::json!(forged);
        let error = serde_json::from_value::<FunctionFoldProgram>(wire)
            .expect_err("wire replay must rederive every stored fold count");
        assert_eq!(
            error.to_string(),
            message,
            "unexpected rejection for {field}"
        );
    }
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

#[test]
fn scalar_program_wire_cannot_forge_a_runtime_tensor_index_domain() {
    let span = source_span("RuntimeIndexWire.mo", 4, 19);
    let block = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::Const { dst: 0, value: 7.0 },
            LinearOp::Const { dst: 1, value: 8.0 },
            LinearOp::Const { dst: 2, value: 1.0 },
            LinearOp::LoadIndexedRegister {
                dst: 3,
                base: 0,
                stride: 1,
                dimensions: Box::new([2]),
                indices: Box::new([TensorIndex::Runtime(2)]),
            },
            LinearOp::StoreOutput { src: 3 },
        ]],
        vec![span],
    )
    .expect("construct a proved runtime-index wire fixture");
    let mut value = serde_json::to_value(block).expect("serialize runtime-index fixture");
    value["programs"][0][2]["Const"]["value"] = serde_json::json!(0.0);

    let error = serde_json::from_value::<ScalarProgramBlock>(value)
        .expect_err("wire replay must reissue rather than trust the erased index proof");

    assert!(
        error
            .to_string()
            .contains("runtime tensor index domain escapes its selected axis"),
        "unexpected wire rejection: {error}"
    );
}
