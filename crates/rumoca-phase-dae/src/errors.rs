//! Phase-local error types for the ToDae phase.
//!
//! Error codes: ED0xx for todae phase (per SPEC_0008).
//!
//! Uses miette for rich diagnostic output with error codes and help text.

use miette::Diagnostic;
use rumoca_core::{BoxedResult, SourceSpan, Span, error_constructor, span_to_source_span};
use thiserror::Error;

/// Type alias for ToDae results with boxed errors.
pub type ToDaeResult<T> = BoxedResult<T, ToDaeError>;

/// Errors that can occur during DAE conversion.
#[derive(Debug, Clone, Error, Diagnostic)]
pub enum ToDaeError {
    /// The model is unbalanced (equations don't match unknowns).
    #[error("unbalanced model: {equations} equations, {unknowns} unknowns (balance = {balance})")]
    #[diagnostic(
        code(rumoca::todae::ED001),
        help("MLS §4.9: A balanced model has the same number of equations as unknowns")
    )]
    Unbalanced {
        equations: usize,
        unknowns: usize,
        balance: i64,
    },

    /// Internal error during DAE conversion.
    #[error("internal todae error: {0}")]
    #[diagnostic(code(rumoca::todae::ED003))]
    Internal(String),

    /// Reinit called on a non-state variable (EQN-016).
    #[error("reinit() can only be applied to state variables: {name} is not a state")]
    #[diagnostic(
        code(rumoca::todae::ED004),
        help(
            "MLS §8.3.5 (EQN-016): reinit(x, expr) requires x to be a state variable with der(x) in the equation system"
        )
    )]
    ReinitNonState {
        name: String,
        #[label("reinit applied to non-state variable here")]
        span: SourceSpan,
    },

    /// Function call could not be resolved to a builtin/intrinsic/user function.
    #[error("unresolved function call: {name}")]
    #[diagnostic(
        code(rumoca::todae::ED005),
        help("ensure the called function is imported/qualified and available in the model scope")
    )]
    UnresolvedFunctionCall {
        name: String,
        #[label("unresolved function call here")]
        span: SourceSpan,
    },

    /// Function definition exists but has no executable implementation.
    #[error("function '{name}' has no algorithm body and is not declared external")]
    #[diagnostic(
        code(rumoca::todae::ED006),
        help("provide an algorithm body, or declare the function as external")
    )]
    FunctionWithoutBody {
        name: String,
        #[label("invalid function definition referenced here")]
        span: SourceSpan,
    },

    /// Constructor field selection cannot be resolved from constructor signature.
    #[error("constructor field selection cannot be resolved: {selection}")]
    #[diagnostic(
        code(rumoca::todae::ED007),
        help(
            "ensure constructor field selections are fully resolved during flattening, with constructor signature fields available"
        )
    )]
    ConstructorFieldSelectionUnresolved {
        selection: String,
        #[label("unresolved constructor field selection here")]
        span: SourceSpan,
    },

    /// Variable/reference name could not be resolved in generated DAE expressions.
    #[error("unresolved reference: {name}")]
    #[diagnostic(
        code(rumoca::todae::ED008),
        help(
            "ensure references are flattened to known variables/constants before ToDae completes"
        )
    )]
    UnresolvedReference {
        name: String,
        #[label("unresolved reference appears here")]
        span: SourceSpan,
    },

    /// Clock constructor expression could not be lowered to a static schedule.
    #[error(
        "unresolved clock schedule: {unresolved}/{constructors} constructor(s) could not be lowered ({examples})"
    )]
    #[diagnostic(
        code(rumoca::todae::ED009),
        help(
            "supported static forms include Clock(period), Clock(intervalCounter, resolution), shiftSample(...), backSample(...), and aliases resolved to those forms; event Clock(condition) remains dynamic, and other unresolved constructors must fail before simulation"
        )
    )]
    UnresolvedClockSchedule {
        constructors: usize,
        unresolved: usize,
        examples: String,
    },

    /// Appendix B discrete-valued partition (`f_m`) is not in solved assignment form.
    #[error("invalid Appendix B discrete solved form: {detail}")]
    #[diagnostic(
        code(rumoca::todae::ED010),
        help(
            "f_m equations must be explicit discrete-valued assignments with an acyclic dependency order"
        )
    )]
    DiscreteSolvedFormViolation {
        detail: String,
        #[label("invalid discrete solved-form equation")]
        span: SourceSpan,
    },

    /// Canonical condition partition (`f_c(relation(v))`) is inconsistent.
    #[error("invalid Appendix B condition partition: {detail}")]
    #[diagnostic(
        code(rumoca::todae::ED011),
        help("f_c and relation must stay 1:1 and preserve canonical condition ordering")
    )]
    ConditionPartitionViolation { detail: String },

    /// Solver-facing runtime metadata failed internal consistency checks.
    #[error("invalid runtime metadata: {detail}")]
    #[diagnostic(
        code(rumoca::todae::ED012),
        help("runtime metadata must be complete and internally consistent before simulation")
    )]
    RuntimeMetadataViolation { detail: String },

    /// Model-level algorithms are not allowed in solver-facing DAE unless lowered.
    #[error("unsupported {section} algorithm in solver-facing DAE: {origin}")]
    #[diagnostic(
        code(rumoca::todae::ED013),
        help(
            "model and initial algorithms must lower to equations before ToDae completion; unsupported statements are not allowed"
        )
    )]
    UnsupportedAlgorithm {
        section: String,
        origin: String,
        #[label("unsupported algorithm statement here")]
        span: SourceSpan,
    },

    /// Solver-facing DAE contains high-level synchronous constructs that must be lowered.
    #[error("strict solver-facing DAE violation: {detail}")]
    #[diagnostic(
        code(rumoca::todae::ED014),
        help(
            "lower synchronous constructs (sample/hold/Clock/subSample/superSample/shiftSample/backSample/noClock/firstTick/previous) before solver-facing DAE emission"
        )
    )]
    StrictSolverDaeViolation {
        detail: String,
        #[label("unsupported synchronous construct appears here")]
        span: SourceSpan,
    },

    /// Solver-facing DAE failed required runtime contract checks.
    #[error("invalid runtime contract: {detail}")]
    #[diagnostic(
        code(rumoca::todae::ED015),
        help(
            "runtime DAE must provide coherent variable partitions and discrete update partitions before simulation"
        )
    )]
    RuntimeContractViolation {
        detail: String,
        #[label("invalid runtime contract originates here")]
        span: SourceSpan,
    },

    /// SPEC_0007 Stage 3 Contract: no source temporal operator may survive into solver-facing DAE-IR.
    #[error("source temporal operator survived DAE boundary: {detail}")]
    #[diagnostic(
        code(rumoca::todae::ED016),
        help(
            "SPEC_0007 Stage 3 Contract requires DAE temporal lowering to convert pre/edge/change/sample/previous into Appendix B variables, conditions, schedules, and ordinary equations before the DAE stage exits"
        )
    )]
    SourceTemporalOperatorSurvivedDaeBoundary {
        detail: String,
        #[label("source temporal operator survived here")]
        span: SourceSpan,
    },
}

impl ToDaeError {
    // Constructor methods using the error_constructor! macro
    error_constructor!(reinit_non_state, ReinitNonState { name: String });
    error_constructor!(
        unresolved_function_call,
        UnresolvedFunctionCall { name: String }
    );
    error_constructor!(function_without_body, FunctionWithoutBody { name: String });
    error_constructor!(
        constructor_field_selection_unresolved,
        ConstructorFieldSelectionUnresolved { selection: String }
    );
    error_constructor!(unresolved_reference, UnresolvedReference { name: String });
    error_constructor!(
        discrete_solved_form_violation,
        DiscreteSolvedFormViolation { detail: String }
    );
    error_constructor!(
        unsupported_algorithm,
        UnsupportedAlgorithm {
            section: String,
            origin: String
        }
    );
    error_constructor!(
        strict_solver_dae_violation,
        StrictSolverDaeViolation { detail: String }
    );
    error_constructor!(
        source_temporal_operator_survived_dae_boundary,
        SourceTemporalOperatorSurvivedDaeBoundary { detail: String }
    );

    /// Create an Unbalanced error (no span, computed balance).
    pub fn unbalanced(equations: usize, unknowns: usize) -> Self {
        let balance = equations as i64 - unknowns as i64;
        Self::Unbalanced {
            equations,
            unknowns,
            balance,
        }
    }

    /// Create an Internal error (no span).
    pub fn internal(message: impl Into<String>) -> Self {
        Self::Internal(message.into())
    }

    /// Create an unresolved clock schedule error.
    pub fn unresolved_clock_schedule(
        constructors: usize,
        unresolved: usize,
        examples: impl Into<String>,
    ) -> Self {
        Self::UnresolvedClockSchedule {
            constructors,
            unresolved,
            examples: examples.into(),
        }
    }

    /// Create a condition partition invariant error.
    pub fn condition_partition_violation(detail: impl Into<String>) -> Self {
        Self::ConditionPartitionViolation {
            detail: detail.into(),
        }
    }

    /// Create a runtime metadata invariant error.
    pub fn runtime_metadata_violation(detail: impl Into<String>) -> Self {
        Self::RuntimeMetadataViolation {
            detail: detail.into(),
        }
    }

    /// Create a runtime contract invariant error.
    pub fn runtime_contract_violation(detail: impl Into<String>) -> Self {
        Self::runtime_contract_violation_at(detail, Span::DUMMY)
    }

    /// Create a runtime contract invariant error with the best available IR span.
    pub fn runtime_contract_violation_at(detail: impl Into<String>, span: Span) -> Self {
        Self::RuntimeContractViolation {
            detail: detail.into(),
            span: span_to_source_span(span),
        }
    }
}

impl From<crate::balance::BalanceError> for ToDaeError {
    fn from(error: crate::balance::BalanceError) -> Self {
        let span = error.source_span().unwrap_or(Span::DUMMY);
        ToDaeError::runtime_contract_violation_at(error.to_string(), span)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{SourceId, Span};

    #[test]
    fn test_unbalanced_error() {
        let err = ToDaeError::unbalanced(5, 3);
        assert!(format!("{err}").contains("5 equations"));
        assert!(format!("{err}").contains("3 unknowns"));

        // Check that miette code is present
        use miette::Diagnostic;
        let code = err.code().map(|c| c.to_string());
        assert_eq!(code, Some("rumoca::todae::ED001".to_string()));
    }

    #[test]
    fn active_todae_errors_keep_stable_diagnostic_codes() {
        let span = Span::from_offsets(SourceId(0), 0, 10);
        use miette::Diagnostic;

        let cases = [
            (
                ToDaeError::unbalanced(5, 3),
                "rumoca::todae::ED001",
                Some("balanced model"),
            ),
            (
                ToDaeError::internal("broken invariant"),
                "rumoca::todae::ED003",
                None,
            ),
            (
                ToDaeError::reinit_non_state("x", span),
                "rumoca::todae::ED004",
                Some("reinit"),
            ),
            (
                ToDaeError::unresolved_function_call("missingFn", span),
                "rumoca::todae::ED005",
                Some("called function"),
            ),
            (
                ToDaeError::function_without_body("f", span),
                "rumoca::todae::ED006",
                Some("algorithm body"),
            ),
            (
                ToDaeError::constructor_field_selection_unresolved("C.x", span),
                "rumoca::todae::ED007",
                Some("constructor field selections"),
            ),
            (
                ToDaeError::unresolved_reference("x", span),
                "rumoca::todae::ED008",
                Some("flattened"),
            ),
            (
                ToDaeError::unresolved_clock_schedule(2, 1, "Clock(x)"),
                "rumoca::todae::ED009",
                Some("Clock(period)"),
            ),
            (
                ToDaeError::discrete_solved_form_violation("cycle", span),
                "rumoca::todae::ED010",
                Some("f_m"),
            ),
            (
                ToDaeError::condition_partition_violation("mismatch"),
                "rumoca::todae::ED011",
                Some("f_c"),
            ),
            (
                ToDaeError::runtime_metadata_violation("missing interval"),
                "rumoca::todae::ED012",
                Some("runtime metadata"),
            ),
            (
                ToDaeError::unsupported_algorithm("model", "while", span),
                "rumoca::todae::ED013",
                Some("algorithms must lower"),
            ),
            (
                ToDaeError::strict_solver_dae_violation("sample", span),
                "rumoca::todae::ED014",
                Some("lower synchronous constructs"),
            ),
            (
                ToDaeError::runtime_contract_violation("overlap"),
                "rumoca::todae::ED015",
                Some("runtime DAE"),
            ),
            (
                ToDaeError::source_temporal_operator_survived_dae_boundary("pre(x)", span),
                "rumoca::todae::ED016",
                Some("pre/edge/change/sample/previous"),
            ),
        ];

        for (err, expected_code, help_fragment) in cases {
            assert_eq!(
                err.code().map(|c| c.to_string()).as_deref(),
                Some(expected_code),
                "unexpected diagnostic code for {err:?}"
            );
            if let Some(fragment) = help_fragment {
                let help = err.help().map(|h| h.to_string()).unwrap_or_default();
                assert!(
                    help.contains(fragment),
                    "expected help for {err:?} to contain `{fragment}`, got `{help}`"
                );
            }
        }
    }
}
