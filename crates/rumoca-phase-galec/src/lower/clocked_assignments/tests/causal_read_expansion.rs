//! Mutation witnesses for total causal-read expansion.

use super::*;

fn witness_span() -> Span {
    Span::from_offsets(SourceId::from_source_name("causal-read-expansion.mo"), 2, 7)
}

#[test]
fn a_foreign_read_identity_is_a_typed_error_instead_of_a_dropped_edge() {
    let variables = HashMap::from([(3, "local")]);
    let error = require_causal_variable(&variables, 9, witness_span())
        .expect_err("a foreign variable identity cannot be ignored");
    assert!(matches!(
        error,
        GalecTargetError::ForeignCausalRead {
            variable_index: 9,
            span,
        } if span == witness_span()
    ));
    assert_eq!(error.code(), "EGT024");
}

#[test]
fn a_complete_scalar_family_is_visited_once_in_canonical_order() {
    let mut visited = Vec::new();
    visit_complete_scalar_definition_family(
        "complete",
        4,
        witness_span(),
        |scalar| Some(scalar + 10),
        |definition| visited.push(definition),
    )
    .expect("an in-domain complete family is the accepted shape");
    assert_eq!(visited, [10, 11, 12, 13]);
}

#[test]
fn a_missing_scalar_definition_rejects_the_whole_family() {
    let mut visited = Vec::new();
    let error = visit_complete_scalar_definition_family(
        "partial",
        3,
        witness_span(),
        |scalar| (scalar != 1).then_some(scalar),
        |definition| visited.push(definition),
    )
    .expect_err("a claimed complete family cannot omit one definition");
    assert!(matches!(
        error,
        GalecTargetError::IncompleteCausalScalarDefinitions {
            ref variable,
            missing_scalar: 1,
            scalar_count: 3,
            span,
        } if variable == "partial" && span == witness_span()
    ));
    assert_eq!(error.code(), "EGT026");
    assert!(
        visited.is_empty(),
        "an incomplete family cannot expose a partially visited prefix"
    );
}

#[cfg(target_pointer_width = "64")]
#[test]
fn an_overflowing_scalar_count_rejects_before_any_definition_is_visited() {
    let mut definitions_queried = 0usize;
    let mut visited = Vec::<u32>::new();
    let scalar_count = usize::try_from(u64::from(u32::MAX) + 1)
        .expect("64-bit test target represents the overflow witness");
    let error = visit_complete_scalar_definition_family(
        "tooWide",
        scalar_count,
        witness_span(),
        |_| {
            definitions_queried += 1;
            Some(0)
        },
        |definition| visited.push(definition),
    )
    .expect_err("a family outside the DAE scalar identity domain must reject");
    assert!(matches!(
        error,
        GalecTargetError::CausalScalarDefinitionOverflow {
            ref variable,
            scalar_count: count,
            span,
        } if variable == "tooWide" && count == scalar_count && span == witness_span()
    ));
    assert_eq!(error.code(), "EGT025");
    assert_eq!(definitions_queried, 0);
    assert!(visited.is_empty());
}
