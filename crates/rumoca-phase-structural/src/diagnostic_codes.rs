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
/// [`crate::StructuralError::InvalidIcPlanUnknown`]: an IC-plan unknown does not
/// map to a solver-vector slot.
pub const ES012_INVALID_IC_PLAN_UNKNOWN: &str = "ES012";
/// [`crate::StructuralError::InconsistentEquation`]: a source equation has a
/// compiler-known nonzero constant residual.
pub const ES013_INCONSISTENT_EQUATION: &str = "ES013";
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
    ES012_INVALID_IC_PLAN_UNKNOWN,
    ES013_INCONSISTENT_EQUATION,
    ES014_CONTRACT_VIOLATION,
];

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use super::*;
    use crate::StructuralError;

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
                unmatched_unknown_spans: vec![Some(structural_code_test_span())],
                over_determined_block: Box::default(),
            },
            StructuralError::EmptySystem,
            StructuralError::InvalidIcPlanUnknown {
                name: "x".to_string(),
            },
            StructuralError::InconsistentEquation {
                residual: 1.0,
                origin: "eq[0]".to_string(),
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

        // Six variants, five codes: only the contract-violation pair aliases.
        assert_eq!(codes.len(), 6);
        assert_eq!(unique.len(), 5, "unexpected code aliasing: {codes:?}");
    }

    #[test]
    fn structural_codes_are_pinned_literals() {
        assert_eq!(ES001_STRUCTURAL_SINGULARITY, "ES001");
        assert_eq!(ES002_ALGEBRAIC_LOOP, "ES002");
        assert_eq!(ES010_SINGULAR_SYSTEM, "ES010");
        assert_eq!(ES011_EMPTY_SYSTEM, "ES011");
        assert_eq!(ES012_INVALID_IC_PLAN_UNKNOWN, "ES012");
        assert_eq!(ES013_INCONSISTENT_EQUATION, "ES013");
        assert_eq!(ES014_CONTRACT_VIOLATION, "ES014");
    }
}
