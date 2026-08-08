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
    let decoded: ScalarProgramBlock =
        serde_json::from_value(value).expect("decode through the checked constructor");

    assert_eq!(decoded.programs(), block.programs());
    assert_eq!(decoded.program_spans(), block.program_spans());
    assert_eq!(decoded.output_indices(), [9, 2]);
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
