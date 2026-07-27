//! Stable `EL0xx` Solve-lowering diagnostic codes (SPEC_0008 "Error Code
//! Ranges").
//!
//! `EL001`-`EL011` cover expression/row lowering declines ([`LowerError`]),
//! `EL02x` covers solve-model assembly failures ([`SolveModelLowerError`]) and
//! `EL03x` covers parameter-override failures ([`ParameterOverrideError`]).
//! Structural failures keep their own `ES0xx` code as they cross the crate
//! boundary — [`SolveModelLowerError::code`] delegates rather than re-labeling.
//!
//! Codes are part of the tool contract: once shipped, a code is never
//! renumbered or reused for a different defect class.
//! [`SOLVE_LOWER_DIAGNOSTIC_CODES`] plus the unit tests below pin that.

use crate::lower::LowerError;
use crate::solve_model::{ParameterOverrideError, SolveModelLowerError};

/// [`LowerError::Unsupported`] / [`LowerError::UnsupportedAt`]: a construct the
/// Solve lowering cannot represent. The spanned and span-free forms share one
/// code — span presence is reported by `source_span()`, not by the code.
pub const EL001_UNSUPPORTED: &str = "EL001";
/// [`LowerError::MissingBinding`]: a referenced variable has no layout slot.
pub const EL002_MISSING_BINDING: &str = "EL002";
/// [`LowerError::MissingFunction`]: a called function has no definition.
pub const EL003_MISSING_FUNCTION: &str = "EL003";
/// [`LowerError::InvalidFunction`]: a function definition cannot be lowered.
pub const EL004_INVALID_FUNCTION: &str = "EL004";
/// [`LowerError::ContractViolation`] / [`LowerError::UnspannedContractViolation`]:
/// Solve/DAE IR metadata required by lowering is missing or inconsistent.
pub const EL005_CONTRACT_VIOLATION: &str = "EL005";
/// [`LowerError::Scalarization`]: array/tensor scalarization failed.
pub const EL006_SCALARIZATION: &str = "EL006";
/// [`LowerError::ProjectionBudgetExceeded`]: an inlined function projection
/// exceeded the node budget (recoverable at the outermost projection boundary).
pub const EL007_PROJECTION_BUDGET_EXCEEDED: &str = "EL007";
/// [`LowerError::DynamicSubscript`]: a runtime-valued subscript in a position
/// requiring a compile-time index.
pub const EL008_DYNAMIC_SUBSCRIPT: &str = "EL008";
/// [`LowerError::ForRangeUnknownDimension`]: `size()` in a for-loop range over a
/// dimension with no structural binding.
pub const EL009_FOR_RANGE_UNKNOWN_DIMENSION: &str = "EL009";
/// [`LowerError::MissingActualArgument`]: a function/constructor input with
/// neither an actual argument nor a default binding.
pub const EL010_MISSING_ACTUAL_ARGUMENT: &str = "EL010";
/// [`LowerError::DynamicBindingBase`]: a dynamic-binding path rooted in an
/// expression form that has no binding key.
pub const EL011_DYNAMIC_BINDING_BASE: &str = "EL011";

/// [`SolveModelLowerError::Evaluation`]: a compile-time evaluation required by
/// solve-model assembly (start values, parameters, attributes) failed.
pub const EL020_EVALUATION: &str = "EL020";
/// [`SolveModelLowerError::MassMatrix`]: a mass-matrix row for a state could not
/// be derived.
pub const EL021_MASS_MATRIX: &str = "EL021";

/// [`ParameterOverrideError::UnpropagatableDependent`]: a dependent parameter's
/// binding references an overridden parameter but could not be re-evaluated.
pub const EL030_PARAMETER_OVERRIDE_UNPROPAGATABLE: &str = "EL030";

/// Every Solve-lowering diagnostic code, in numeric order.
///
/// The grep-discoverable registry: a new `EL0xx` constant must be added here,
/// and every code returned by [`LowerError::code`],
/// [`SolveModelLowerError::code`] or [`ParameterOverrideError::code`] must be
/// present (enforced by the unit tests below).
pub const SOLVE_LOWER_DIAGNOSTIC_CODES: &[&str] = &[
    EL001_UNSUPPORTED,
    EL002_MISSING_BINDING,
    EL003_MISSING_FUNCTION,
    EL004_INVALID_FUNCTION,
    EL005_CONTRACT_VIOLATION,
    EL006_SCALARIZATION,
    EL007_PROJECTION_BUDGET_EXCEEDED,
    EL008_DYNAMIC_SUBSCRIPT,
    EL009_FOR_RANGE_UNKNOWN_DIMENSION,
    EL010_MISSING_ACTUAL_ARGUMENT,
    EL011_DYNAMIC_BINDING_BASE,
    EL020_EVALUATION,
    EL021_MASS_MATRIX,
    EL030_PARAMETER_OVERRIDE_UNPROPAGATABLE,
];

impl LowerError {
    /// Stable diagnostic code (SPEC_0008 `EL0xx` range).
    ///
    /// The `Spanned` / `WithContext` wrappers delegate to the wrapped error, so
    /// attaching a fallback span or a context frame never changes the code.
    #[must_use]
    pub fn code(&self) -> &'static str {
        match self {
            Self::Unsupported { .. } | Self::UnsupportedAt { .. } => EL001_UNSUPPORTED,
            Self::MissingBinding { .. } => EL002_MISSING_BINDING,
            Self::MissingFunction { .. } => EL003_MISSING_FUNCTION,
            Self::InvalidFunction { .. } => EL004_INVALID_FUNCTION,
            Self::ContractViolation { .. } | Self::UnspannedContractViolation { .. } => {
                EL005_CONTRACT_VIOLATION
            }
            Self::Scalarization { .. } => EL006_SCALARIZATION,
            Self::ProjectionBudgetExceeded { .. } => EL007_PROJECTION_BUDGET_EXCEEDED,
            Self::DynamicSubscript => EL008_DYNAMIC_SUBSCRIPT,
            Self::ForRangeUnknownDimension { .. } => EL009_FOR_RANGE_UNKNOWN_DIMENSION,
            Self::MissingActualArgument { .. } => EL010_MISSING_ACTUAL_ARGUMENT,
            Self::DynamicBindingBase { .. } => EL011_DYNAMIC_BINDING_BASE,
            Self::Spanned { source, .. } | Self::WithContext { source, .. } => source.code(),
        }
    }
}

impl SolveModelLowerError {
    /// Stable diagnostic code (SPEC_0008).
    ///
    /// `Lower` delegates into the `EL0xx` range and `Structural` delegates into
    /// the structural `ES0xx` range: an error keeps the code of the phase that
    /// produced it as it crosses the crate boundary.
    #[must_use]
    pub fn code(&self) -> &'static str {
        match self {
            Self::Lower(error) => error.code(),
            Self::Structural { source } => source.code(),
            Self::Evaluation { .. } => EL020_EVALUATION,
            Self::MassMatrix { .. } => EL021_MASS_MATRIX,
        }
    }
}

impl ParameterOverrideError {
    /// Stable diagnostic code (SPEC_0008 `EL0xx` range).
    #[must_use]
    pub const fn code(&self) -> &'static str {
        match self {
            Self::UnpropagatableDependent { .. } => EL030_PARAMETER_OVERRIDE_UNPROPAGATABLE,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use super::*;

    fn solve_code_test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("solve_diagnostic_codes_source_1.mo"),
            5,
            13,
        )
    }

    /// One instance of every non-wrapper `LowerError` variant. Adding a variant
    /// without a code forces a compile error in the exhaustive `match` inside
    /// [`LowerError::code`] and a test update here.
    fn every_lower_error() -> Vec<LowerError> {
        vec![
            LowerError::Unsupported {
                reason: "unsupported".to_string(),
            },
            LowerError::UnsupportedAt {
                reason: "unsupported".to_string(),
                contexts: Vec::new(),
                span: solve_code_test_span(),
            },
            LowerError::MissingBinding {
                name: "x".to_string(),
            },
            LowerError::MissingFunction {
                name: "Pkg.f".to_string(),
            },
            LowerError::InvalidFunction {
                name: "Pkg.f".to_string(),
                reason: "no body".to_string(),
            },
            LowerError::ContractViolation {
                reason: "metadata mismatch".to_string(),
                span: solve_code_test_span(),
            },
            LowerError::UnspannedContractViolation {
                reason: "metadata mismatch".to_string(),
            },
            LowerError::Scalarization {
                message: "shape mismatch".to_string(),
                span: Some(solve_code_test_span()),
            },
            LowerError::ProjectionBudgetExceeded {
                function: "Pkg.f".to_string(),
                span: solve_code_test_span(),
            },
            LowerError::DynamicSubscript,
            LowerError::ForRangeUnknownDimension {
                name: "n".to_string(),
            },
            LowerError::MissingActualArgument {
                function: "Pkg.f".to_string(),
                what: "required input",
                input: "x".to_string(),
                span: solve_code_test_span(),
            },
            LowerError::DynamicBindingBase {
                tag: "call".to_string(),
            },
        ]
    }

    #[test]
    fn solve_lower_codes_are_registered() {
        for error in every_lower_error() {
            let code = error.code();
            assert!(
                SOLVE_LOWER_DIAGNOSTIC_CODES.contains(&code),
                "lower error {error:?} returned unregistered code {code}"
            );
        }
    }

    #[test]
    fn solve_lower_codes_cover_the_expression_range() {
        let reachable: BTreeSet<&'static str> =
            every_lower_error().iter().map(LowerError::code).collect();
        let expected: BTreeSet<&'static str> = SOLVE_LOWER_DIAGNOSTIC_CODES
            .iter()
            .copied()
            .filter(|code| {
                *code != EL020_EVALUATION
                    && *code != EL021_MASS_MATRIX
                    && *code != EL030_PARAMETER_OVERRIDE_UNPROPAGATABLE
            })
            .collect();

        assert_eq!(
            reachable, expected,
            "every EL001-EL011 code must be reachable from a LowerError variant"
        );
    }

    #[test]
    fn solve_diagnostic_codes_are_unique_and_well_formed() {
        let unique: BTreeSet<&&str> = SOLVE_LOWER_DIAGNOSTIC_CODES.iter().collect();
        assert_eq!(
            unique.len(),
            SOLVE_LOWER_DIAGNOSTIC_CODES.len(),
            "duplicate entry in SOLVE_LOWER_DIAGNOSTIC_CODES: {SOLVE_LOWER_DIAGNOSTIC_CODES:?}"
        );
        for code in SOLVE_LOWER_DIAGNOSTIC_CODES {
            assert!(
                code.len() == 5
                    && code.starts_with("EL")
                    && code[2..].chars().all(|c| c.is_ascii_digit()),
                "solve code {code} must match EL<three digits>"
            );
        }
    }

    #[test]
    fn wrapper_variants_delegate_code() {
        let spanned = LowerError::MissingBinding {
            name: "x".to_string(),
        }
        .with_fallback_span(solve_code_test_span());
        assert!(matches!(spanned, LowerError::Spanned { .. }));
        assert_eq!(spanned.code(), EL002_MISSING_BINDING);

        let contexted = LowerError::DynamicSubscript.with_context("row 3");
        assert!(matches!(contexted, LowerError::WithContext { .. }));
        assert_eq!(contexted.code(), EL008_DYNAMIC_SUBSCRIPT);

        let nested = LowerError::MissingFunction {
            name: "Pkg.f".to_string(),
        }
        .with_context("residual row 1")
        .with_fallback_span(solve_code_test_span());
        assert_eq!(nested.code(), EL003_MISSING_FUNCTION);
    }

    #[test]
    fn unsupported_span_pair_shares_one_code() {
        let unspanned = LowerError::Unsupported {
            reason: "der() of a parameter".to_string(),
        };
        let spanned = unspanned.clone().with_fallback_span(solve_code_test_span());

        assert!(matches!(spanned, LowerError::UnsupportedAt { .. }));
        assert_eq!(unspanned.code(), spanned.code());
        assert_eq!(spanned.code(), EL001_UNSUPPORTED);
    }

    #[test]
    fn contract_violation_variants_share_one_code() {
        let spanned = LowerError::contract_violation("bad metadata", solve_code_test_span());
        let unspanned = LowerError::UnspannedContractViolation {
            reason: "bad metadata".to_string(),
        };

        assert_eq!(spanned.code(), EL005_CONTRACT_VIOLATION);
        assert_eq!(unspanned.code(), EL005_CONTRACT_VIOLATION);
        assert!(spanned.source_span().is_some());
        assert!(unspanned.source_span().is_none());
    }

    #[test]
    fn solve_model_lower_error_codes() {
        assert_eq!(
            SolveModelLowerError::Lower(LowerError::DynamicSubscript).code(),
            EL008_DYNAMIC_SUBSCRIPT
        );
        assert_eq!(
            SolveModelLowerError::Evaluation {
                context: "start value for `x`".to_string(),
                source: rumoca_eval_dae::EvalError::MissingBinding {
                    name: "x".to_string(),
                },
                span: None,
            }
            .code(),
            EL020_EVALUATION
        );
        assert_eq!(
            SolveModelLowerError::MassMatrix {
                row: 0,
                state_name: "x".to_string(),
                reason: "non-constant coefficient".to_string(),
                span: None,
            }
            .code(),
            EL021_MASS_MATRIX
        );
    }

    #[test]
    fn structural_errors_keep_their_own_code_across_the_crate_boundary() {
        let error = SolveModelLowerError::Structural {
            source: rumoca_phase_structural::StructuralError::EmptySystem,
        };

        assert_eq!(error.code(), "ES011");
        assert!(
            rumoca_phase_structural::STRUCTURAL_DIAGNOSTIC_CODES.contains(&error.code()),
            "structural codes surfaced through solve must stay in the ES registry"
        );
    }

    #[test]
    fn parameter_override_error_code() {
        assert_eq!(
            ParameterOverrideError::UnpropagatableDependent {
                dependent: "b".to_string(),
            }
            .code(),
            EL030_PARAMETER_OVERRIDE_UNPROPAGATABLE
        );
    }

    #[test]
    fn solve_codes_are_pinned_literals() {
        assert_eq!(EL001_UNSUPPORTED, "EL001");
        assert_eq!(EL002_MISSING_BINDING, "EL002");
        assert_eq!(EL003_MISSING_FUNCTION, "EL003");
        assert_eq!(EL004_INVALID_FUNCTION, "EL004");
        assert_eq!(EL005_CONTRACT_VIOLATION, "EL005");
        assert_eq!(EL006_SCALARIZATION, "EL006");
        assert_eq!(EL007_PROJECTION_BUDGET_EXCEEDED, "EL007");
        assert_eq!(EL008_DYNAMIC_SUBSCRIPT, "EL008");
        assert_eq!(EL009_FOR_RANGE_UNKNOWN_DIMENSION, "EL009");
        assert_eq!(EL010_MISSING_ACTUAL_ARGUMENT, "EL010");
        assert_eq!(EL011_DYNAMIC_BINDING_BASE, "EL011");
        assert_eq!(EL020_EVALUATION, "EL020");
        assert_eq!(EL021_MASS_MATRIX, "EL021");
        assert_eq!(EL030_PARAMETER_OVERRIDE_UNPROPAGATABLE, "EL030");
    }
}
