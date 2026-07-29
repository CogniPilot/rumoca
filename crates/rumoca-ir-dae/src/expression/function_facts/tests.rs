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

#[test]
fn conflicting_read_uses_the_exact_found_occurrence_span() {
    let (mut storage, lhs, rhs) = conflicting_storage(1);
    storage.expressions.provenance = vec![provenance(10), provenance(20)];

    let error = merge_function_read_sets(&mut storage, lhs, rhs, provenance(30))
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
    storage.expressions.provenance = vec![provenance(10)];

    let error = merge_function_read_sets(&mut storage, lhs, rhs, provenance(30))
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
