//! A scalar program block keeps the component occurrence lowering recorded for
//! each program, through the JSON and binary wires, and a block without one
//! keeps its JSON bytes unchanged.

use rumoca_core::{ClassEquation, ComponentOccurrence, InstanceId, ScopeId, SourceId, Span};
use rumoca_ir_solve::{LinearOp, ScalarProgramBlock};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("Unit.mo"), 1, 9)
}

fn occurrence(instance: u32) -> ComponentOccurrence {
    ComponentOccurrence {
        class_equation: ClassEquation {
            scope: ScopeId(3),
            span: span(),
        },
        instance: InstanceId(instance),
        view: 0,
    }
}

fn block() -> ScalarProgramBlock {
    let program = |value| {
        vec![
            LinearOp::Const { dst: 0, value },
            LinearOp::StoreOutput { src: 0 },
        ]
    };
    ScalarProgramBlock::with_program_spans(vec![program(1.0), program(2.0)], vec![span(); 2])
        .unwrap()
}

#[test]
fn programs_keep_their_occurrence_through_both_wires() {
    let tagged = block()
        .with_program_occurrences(vec![Some(occurrence(5)), None])
        .unwrap();
    let json = serde_json::to_string(&tagged).unwrap();
    assert!(json.contains("\"program_occurrences\":"));
    let from_json: ScalarProgramBlock = serde_json::from_str(&json).unwrap();
    let binary = bincode::serialize(&tagged).unwrap();
    let from_binary: ScalarProgramBlock = bincode::deserialize(&binary).unwrap();
    assert_eq!(bincode::serialize(&from_binary).unwrap(), binary);
    for decoded in [&tagged, &from_json, &from_binary] {
        assert_eq!(decoded.program_occurrence(0), Some(occurrence(5)));
        assert_eq!(decoded.program_occurrence(1), None);
    }
}

#[test]
fn a_block_without_occurrences_keeps_its_json_and_round_trips_in_binary() {
    let plain = block();
    let untagged = block().with_program_occurrences(vec![None, None]).unwrap();
    let json = serde_json::to_string(&plain).unwrap();
    assert!(!json.contains("program_occurrences"));
    assert_eq!(serde_json::to_string(&untagged).unwrap(), json);
    let binary = bincode::serialize(&plain).unwrap();
    let decoded: ScalarProgramBlock = bincode::deserialize(&binary).unwrap();
    assert_eq!(decoded.program_occurrence(0), None);
    assert_eq!(bincode::serialize(&decoded).unwrap(), binary);
}

#[test]
fn occurrences_must_align_with_programs() {
    assert!(
        block()
            .with_program_occurrences(vec![Some(occurrence(1))])
            .is_err()
    );
    let shared = block();
    let _owner = shared.clone();
    assert!(
        shared
            .with_program_occurrences(vec![Some(occurrence(1)), None])
            .is_err(),
        "a block another owner already holds cannot gain an identity"
    );
    let mut json = serde_json::to_string(&block()).unwrap();
    assert_eq!(json.pop(), Some('}'));
    json.push_str(&format!(
        ",\"program_occurrences\":[{}]}}",
        serde_json::to_string(&Some(occurrence(1))).unwrap()
    ));
    assert!(serde_json::from_str::<ScalarProgramBlock>(&json).is_err());
}
