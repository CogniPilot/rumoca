//! Wire tests for the single current [`Reference`] shape.
//!
//! The positive fixture is the exact current spelling; the negative fixture is
//! the list of payloads that must not decode — the deleted bare-name shape, a
//! record missing any one field (including `instance_id`, which the exact
//! identity cutover added and which the old record defaulted away), an unknown
//! field, and every identity-contract violation.

use super::*;
use crate::ir_primitives::{
    ComponentRefPart, DefId, FunctionInstanceId, SourceId, Span, Subscript,
};

const REFERENCE_WIRE_GOLDEN: &str = include_str!("../../../tests/golden/reference_wire.json");
const REFERENCE_WIRE_REJECTED: &str =
    include_str!("../../../tests/golden/reference_wire_rejected.json");

fn fixture_span() -> Span {
    Span::from_offsets(SourceId::from_source_name("reference_wire.mo"), 12, 21)
}

/// A reference with no structure, no occurrence identity and no resolved
/// function: the shape that used to serialize as a bare JSON string.
fn undecorated_reference() -> Reference {
    Reference::new("x")
}

/// A fully decorated reference: structure, occurrence identity, resolved
/// function target, and the generated flag all present.
fn decorated_reference() -> Reference {
    let span = fixture_span();
    let component_ref = ComponentReference::construct(
        false,
        span,
        vec![
            ComponentRefPart {
                ident: "body".to_string(),
                span,
                subs: vec![Subscript::Index { value: 2, span }],
                def_id: DefId::new(7),
            },
            ComponentRefPart {
                ident: "r".to_string(),
                span,
                subs: Vec::new(),
                def_id: DefId::new(42),
            },
        ],
    )
    .expect("fixture reference is nonempty and resolved");

    Reference::generated_component_reference(component_ref)
        .with_instance_id(InstanceId::new(9))
        .with_resolved_function(ResolvedFunctionReference {
            instance_id: FunctionInstanceId::new(3),
            base_part_count: 2,
            transitively_non_replaceable: true,
        })
}

fn fixture_references() -> Vec<Reference> {
    vec![undecorated_reference(), decorated_reference()]
}

#[test]
fn reference_wire_matches_the_current_golden_shape() {
    let actual = serde_json::to_value(fixture_references()).expect("serialize reference fixture");
    let golden: serde_json::Value =
        serde_json::from_str(REFERENCE_WIRE_GOLDEN).expect("golden fixture is valid JSON");

    assert_eq!(
        actual, golden,
        "reference serialization drifted from the current wire shape"
    );
    assert_eq!(
        serde_json::from_value::<Vec<Reference>>(golden).expect("golden fixture decodes"),
        fixture_references(),
        "the current wire shape must round-trip back to the same references"
    );
}

#[test]
fn every_reference_field_is_explicit_on_the_wire() {
    for reference in fixture_references() {
        let value = serde_json::to_value(&reference).expect("serialize reference");
        let record = value
            .as_object()
            .expect("a reference serializes as one record, never as a bare name");
        let mut keys = record.keys().map(String::as_str).collect::<Vec<_>>();
        keys.sort_unstable();
        assert_eq!(
            keys,
            [
                "component_ref",
                "generated",
                "instance_id",
                "name",
                "resolved_function"
            ],
            "every field is written explicitly, including the absent ones"
        );
    }
}

#[test]
fn reference_round_trips_through_the_compact_binary_wire() {
    for reference in fixture_references() {
        let binary = bincode::serialize(&reference).expect("serialize reference as bincode");
        let decoded: Reference = bincode::deserialize(&binary).expect("decode bincode reference");
        assert_eq!(decoded, reference);
        assert_eq!(
            bincode::serialize(&decoded).expect("re-serialize decoded reference"),
            binary
        );
    }
}

#[test]
fn deleted_and_incomplete_reference_shapes_are_rejected() {
    #[derive(serde::Deserialize)]
    struct RejectedCase {
        case: String,
        error_fragment: String,
        payload: serde_json::Value,
    }

    let cases: Vec<RejectedCase> =
        serde_json::from_str(REFERENCE_WIRE_REJECTED).expect("rejection fixture is valid JSON");
    assert!(
        cases.len() >= 10,
        "the rejection fixture must keep covering every deleted and incomplete shape"
    );

    for case in cases {
        let error = serde_json::from_value::<Reference>(case.payload)
            .map(|reference| reference.as_str().to_string())
            .expect_err(&format!("`{}` must not decode as a reference", case.case));
        assert!(
            error.to_string().contains(&case.error_fragment),
            "`{}` rejected with `{error}`, expected it to mention `{}`",
            case.case,
            case.error_fragment
        );
    }
}

#[test]
fn reference_construction_rejects_identity_free_records() {
    assert_eq!(
        Reference::construct(VarName::new(""), None, None, None, false),
        Err(ReferenceContractError::EmptyName)
    );
    assert_eq!(
        Reference::construct(
            VarName::new("x"),
            None,
            None,
            Some(InstanceId::UNSET),
            false
        ),
        Err(ReferenceContractError::UnsetInstanceIdentity)
    );
    assert_eq!(
        Reference::construct(
            VarName::new("x"),
            None,
            None,
            Some(InstanceId::new(1)),
            false
        )
        .expect("an allocated occurrence identity is admitted")
        .instance_id(),
        Some(InstanceId::new(1))
    );
}
