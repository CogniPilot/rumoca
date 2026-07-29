use rumoca_core::{SourceId, Span};

use super::*;

fn provenance(start: usize) -> DaeProvenance {
    DaeProvenance::source(Span::from_offsets(
        SourceId::from_source_name("function_facts.mo"),
        start,
        start + 1,
    ))
    .expect("test provenance is source-backed")
}

fn conflicting_storage(found_witness: u32) -> (Storage, FunctionReadSet, FunctionReadSet) {
    let mut storage = Storage::default();
    let lhs = storage
        .function_read_sets
        .singleton(
            FunctionReadFact {
                value: 3,
                definition: 4,
                witness: 0,
            },
            provenance(1),
        )
        .expect("small proof arena");
    let rhs = storage
        .function_read_sets
        .singleton(
            FunctionReadFact {
                value: 3,
                definition: 5,
                witness: found_witness,
            },
            provenance(2),
        )
        .expect("small proof arena");
    (storage, lhs, rhs)
}

fn push_facts(
    storage: &mut Storage,
    scope: Option<u32>,
    illegal_coordinate: Option<u32>,
    read_set: FunctionReadSet,
    at: DaeProvenance,
) {
    storage.expressions.function_scopes.push(scope);
    storage
        .expressions
        .function_illegal_coordinates
        .push(illegal_coordinate);
    storage.expressions.function_read_sets.push(read_set);
    storage.expressions.function_latest_calls.push(None);
    storage.expressions.provenance.push(at);
}

fn binary() -> ExprNode {
    ExprNode::Binary {
        operator: BinaryOperator::Add,
        lhs: 0,
        rhs: 1,
    }
}

#[test]
fn conflicting_read_uses_the_exact_found_occurrence_span() {
    let (mut storage, lhs, rhs) = conflicting_storage(1);
    push_facts(&mut storage, Some(7), None, lhs, provenance(10));
    push_facts(&mut storage, Some(7), None, rhs, provenance(20));

    let error = node_function_facts(&mut storage, &binary(), provenance(30))
        .expect_err("different definitions conflict");
    assert!(matches!(
        error,
        DaeConstructionError::InvalidFunctionValueRead {
            value: 3,
            expected_definition: Some(4),
            found_definition: 5,
            span,
        } if span == provenance(20).span()
    ));
}

#[test]
fn invalid_proof_witness_is_rejected_without_a_fallback_span() {
    let (mut storage, lhs, rhs) = conflicting_storage(9);
    push_facts(&mut storage, Some(7), None, lhs, provenance(10));
    push_facts(&mut storage, Some(7), None, rhs, provenance(20));

    let error = node_function_facts(&mut storage, &binary(), provenance(30))
        .expect_err("invalid proof witness cannot be hidden");
    assert!(matches!(
        error,
        DaeConstructionError::UnknownId {
            kind: "expression provenance",
            index: 9,
            ..
        }
    ));
}

#[test]
fn scope_conflict_precedes_earlier_read_conflict() {
    let (mut storage, lhs, rhs) = conflicting_storage(1);
    push_facts(&mut storage, Some(7), None, lhs, provenance(10));
    push_facts(&mut storage, Some(8), None, rhs, provenance(20));

    let error = node_function_facts(&mut storage, &binary(), provenance(30))
        .expect_err("cross-function expressions are rejected first");
    assert!(matches!(
        error,
        DaeConstructionError::InvalidFunctionScope {
            expected_function: Some(7),
            found_function: 8,
            span,
        } if span == provenance(30).span()
    ));
}

#[test]
fn fold_keeps_leftmost_illegal_coordinate() {
    let mut storage = Storage::default();
    push_facts(
        &mut storage,
        Some(7),
        Some(11),
        FunctionReadSet::EMPTY,
        provenance(10),
    );
    push_facts(
        &mut storage,
        Some(7),
        Some(12),
        FunctionReadSet::EMPTY,
        provenance(20),
    );

    let facts =
        node_function_facts(&mut storage, &binary(), provenance(30)).expect("compatible facts");
    assert_eq!(facts.scope, Some(7));
    assert_eq!(facts.illegal_coordinate, Some(11));
    assert_eq!(facts.read_set, FunctionReadSet::EMPTY);
}
