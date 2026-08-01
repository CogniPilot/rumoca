//! Stable `ES0xx` structural diagnostic codes (SPEC_0008 "Error Code Ranges").
//!
//! Warnings (`ES001`, `ES002`) and hard errors (`ES01x`) share this one table so
//! the whole structural surface is greppable from a single place. Codes are part
//! of the tool contract: once shipped, a code is never renumbered or reused for
//! a different defect class — [`STRUCTURAL_DIAGNOSTIC_CODES`] plus the unit
//! tests below pin that.

/// A maximum matching smaller than the system size was found (warning form).
pub const ES001_STRUCTURAL_SINGULARITY: &str = "ES001";
/// A strongly connected component of size > 1 requires simultaneous solution.
pub const ES002_ALGEBRAIC_LOOP: &str = "ES002";

/// [`crate::StructuralError::Singular`]: no perfect matching exists.
pub const ES010_SINGULAR_SYSTEM: &str = "ES010";
/// [`crate::StructuralError::EmptySystem`]: no equations and no unknowns.
pub const ES011_EMPTY_SYSTEM: &str = "ES011";
/// [`crate::StructuralError::DroppedStatedInitialValue`]: the only available
/// index reduction would discard an MLS 3.6 §8.6 `fixed = true` initial
/// equation.
///
/// SPEC_0008 acceptance contract (a rejection is only as good as the acceptance
/// it bounds):
///
/// * **rejects** a state demotion, or a holonomic reduction, after which no
///   coordinate the runtime answers states a `fixed = true` value the system it
///   came from stated — proved by re-reading the rebuilt system, never predicted;
/// * **accepts**, and must keep accepting: a demotion whose class carries the
///   value onto the surviving state (transferred as an MLS 3.6 §8.6 definition);
///   a class another seeded declaration already answers; a class whose asserted
///   value the stated one is not *proved* to differ from, including every
///   difference that still reads a parameter; and any coordinate the model never
///   pinned;
/// * **owner** `crate::dae_transform::constraints::discarded_stated_initial_value`
///   over `crate::dae_transform::initial_pins::represented_initial_values`;
/// * **evidence** `dae_transform::tests::initial_values` —
///   `a_pinned_state_is_demoted_when_the_class_carries_its_value_to_the_survivor`,
///   `an_invariant_class_that_asserts_the_stated_value_admits_the_demotion` and
///   `a_parameter_valued_asserted_value_is_left_to_the_initialization_instant`
///   for the accepted shapes;
///   `a_pinned_state_no_surviving_equation_states_is_still_refused` and
///   `an_invariant_class_that_asserts_another_value_refuses_the_demotion` for the
///   rejected one.
pub const ES012_DROPPED_STATED_INITIAL_VALUE: &str = "ES012";
/// [`crate::StructuralError::ConflictingStatedInitialValues`]: two coordinates
/// the system proves equal each pin an MLS 3.6 §8.6 `fixed = true` start, and
/// the two starts state different values.
///
/// SPEC_0008 acceptance contract:
///
/// * **rejects** two `fixed = true` declarations of one proved-equal class whose
///   stated values differ by a constant this phase evaluates to a nonzero number
///   — no parameter value can close that gap, so the initialization system MLS
///   3.6 §8.6 describes has no solution;
/// * **accepts**, and must keep accepting: two declarations whose difference
///   still reads a parameter (`a(start = 3)` and `b(start = 1)` under
///   `a = b + L` state one value exactly when `L = 2`), which is left to the
///   initialization instant as a checked residual;
/// * **owner** `crate::dae_transform::initial_pins::ValueClosure::reject_contradicted_pins`;
/// * **evidence** `dae_transform::tests::initial_values::two_pinned_members_that_disagree_report_the_inconsistent_initialization`
///   and `initial_value_alias_transfer::stated_values_that_agree_through_a_parameter_are_not_a_conflict`
///   for the accepted shape.
pub const ES013_CONFLICTING_STATED_INITIAL_VALUES: &str = "ES013";
/// [`crate::StructuralError::ContractViolation`] and
/// [`crate::StructuralError::UnspannedContractViolation`]: DAE IR metadata
/// required by structural analysis is missing or inconsistent.
///
/// Both variants deliberately share this one code: they are the same defect
/// class, and whether an honest source span exists is reported by
/// [`crate::StructuralError::source_span`], not by a second code.
pub const ES014_CONTRACT_VIOLATION: &str = "ES014";

/// Every structural diagnostic code, in numeric order.
///
/// The grep-discoverable registry: a new `ES0xx` constant must be added here,
/// and every code returned by [`crate::StructuralError::code`] must be present
/// (enforced by `structural_error_codes_are_registered`).
pub const STRUCTURAL_DIAGNOSTIC_CODES: &[&str] = &[
    ES001_STRUCTURAL_SINGULARITY,
    ES002_ALGEBRAIC_LOOP,
    ES010_SINGULAR_SYSTEM,
    ES011_EMPTY_SYSTEM,
    ES012_DROPPED_STATED_INITIAL_VALUE,
    ES013_CONFLICTING_STATED_INITIAL_VALUES,
    ES014_CONTRACT_VIOLATION,
];

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use super::*;
    use crate::StructuralError;
    use rumoca_core::PhaseError;

    fn structural_code_test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("structural_diagnostic_codes_source_1.mo"),
            3,
            9,
        )
    }

    /// One instance of every `StructuralError` variant, so adding a variant
    /// without a code forces a compile error in the exhaustive `match` inside
    /// [`StructuralError::code`] and a test update here.
    fn every_structural_error() -> Vec<StructuralError> {
        vec![
            StructuralError::Singular {
                n_equations: 2,
                n_unknowns: 3,
                n_matched: 2,
                unmatched_equations: Vec::new(),
                unmatched_unknowns: vec!["x".to_string()],
                unmatched_unknown_spans: vec![structural_code_test_span()],
                over_determined_block: Box::default(),
            },
            StructuralError::EmptySystem,
            StructuralError::DroppedStatedInitialValue {
                variable: "x".to_string(),
                span: structural_code_test_span(),
            },
            StructuralError::ConflictingStatedInitialValues {
                variable: "x".to_string(),
                other: "y".to_string(),
                span: structural_code_test_span(),
                other_span: structural_code_test_span(),
            },
            StructuralError::Projection {
                reason: "dynamic index".to_string(),
                span: structural_code_test_span(),
            },
            StructuralError::ContractViolation {
                reason: "missing metadata".to_string(),
                span: structural_code_test_span(),
            },
            StructuralError::UnspannedContractViolation {
                reason: "missing metadata".to_string(),
            },
        ]
    }

    #[test]
    fn structural_error_codes_are_registered() {
        for error in every_structural_error() {
            let code = error.code();
            assert!(
                STRUCTURAL_DIAGNOSTIC_CODES.contains(&code),
                "structural error {error:?} returned unregistered code {code}"
            );
        }
    }

    #[test]
    fn structural_diagnostic_codes_are_unique_and_well_formed() {
        let unique: BTreeSet<&&str> = STRUCTURAL_DIAGNOSTIC_CODES.iter().collect();
        assert_eq!(
            unique.len(),
            STRUCTURAL_DIAGNOSTIC_CODES.len(),
            "duplicate entry in STRUCTURAL_DIAGNOSTIC_CODES: {STRUCTURAL_DIAGNOSTIC_CODES:?}"
        );
        for code in STRUCTURAL_DIAGNOSTIC_CODES {
            assert!(
                code.len() == 5
                    && code.starts_with("ES")
                    && code[2..].chars().all(|c| c.is_ascii_digit()),
                "structural code {code} must match ES<three digits>"
            );
        }
    }

    #[test]
    fn contract_violation_variants_share_one_code() {
        let spanned = StructuralError::ContractViolation {
            reason: "missing metadata".to_string(),
            span: structural_code_test_span(),
        };
        let unspanned = StructuralError::UnspannedContractViolation {
            reason: "missing metadata".to_string(),
        };

        assert_eq!(spanned.code(), ES014_CONTRACT_VIOLATION);
        assert_eq!(unspanned.code(), ES014_CONTRACT_VIOLATION);
        assert!(spanned.source_span().is_some());
        assert!(unspanned.source_span().is_none());
    }

    #[test]
    fn distinct_structural_defects_have_distinct_codes() {
        let codes: Vec<&'static str> = every_structural_error()
            .iter()
            .map(StructuralError::code)
            .collect();
        let unique: BTreeSet<&&str> = codes.iter().collect();

        // Seven variants, five codes: all checked-contract failures share ES014.
        assert_eq!(codes.len(), 7);
        assert_eq!(unique.len(), 5, "unexpected code aliasing: {codes:?}");
    }

    #[test]
    fn structural_codes_are_pinned_literals() {
        assert_eq!(ES001_STRUCTURAL_SINGULARITY, "ES001");
        assert_eq!(ES002_ALGEBRAIC_LOOP, "ES002");
        assert_eq!(ES010_SINGULAR_SYSTEM, "ES010");
        assert_eq!(ES011_EMPTY_SYSTEM, "ES011");
        assert_eq!(ES012_DROPPED_STATED_INITIAL_VALUE, "ES012");
        assert_eq!(ES013_CONFLICTING_STATED_INITIAL_VALUES, "ES013");
        assert_eq!(ES014_CONTRACT_VIOLATION, "ES014");
    }

    #[test]
    fn phase_error_preserves_all_available_singularity_spans() {
        let primary = structural_code_test_span();
        let secondary = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("structural-secondary.mo"),
            10,
            16,
        );
        let error = StructuralError::Singular {
            n_equations: 1,
            n_unknowns: 2,
            n_matched: 1,
            unmatched_equations: Vec::new(),
            unmatched_unknowns: vec!["x".to_string(), "y".to_string()],
            unmatched_unknown_spans: vec![primary, secondary],
            over_determined_block: Box::default(),
        };

        let diagnostic = error.to_diagnostic();
        assert_eq!(diagnostic.code.as_deref(), Some(ES010_SINGULAR_SYSTEM));
        assert_eq!(diagnostic.labels[0].span, primary);
        assert!(diagnostic.labels[0].primary);
        assert!(
            diagnostic
                .labels
                .iter()
                .any(|label| label.span == secondary && !label.primary)
        );
    }
}
