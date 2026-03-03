//! Phase-local error types for the ToDae phase.
//!
//! Error codes: ED0xx for todae phase (per SPEC_0008).
//!
//! Uses miette for rich diagnostic output with error codes and help text.

use miette::Diagnostic;
use rumoca_core::{BoxedResult, SourceSpan, error_constructor};
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

    /// A state variable was expected but not found.
    #[error("expected state variable not found: {name}")]
    #[diagnostic(
        code(rumoca::todae::ED002),
        help("state variables must have der() calls in the equation system")
    )]
    MissingState {
        name: String,
        #[label("expected state here")]
        span: SourceSpan,
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

    /// Constructor field projection cannot be resolved from constructor signature.
    #[error("constructor field projection cannot be resolved: {projection}")]
    #[diagnostic(
        code(rumoca::todae::ED007),
        help(
            "ensure constructor field projections are fully resolved during flattening, with constructor signature fields available"
        )
    )]
    ConstructorFieldProjectionUnresolved {
        projection: String,
        #[label("unresolved constructor field projection here")]
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
            "clock constructor timing must be lowered during ToDae; dynamic/unsupported constructors should fail before simulation"
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
    RuntimeContractViolation { detail: String },
}

impl ToDaeError {
    // Constructor methods using the error_constructor! macro
    error_constructor!(missing_state, MissingState { name: String });
    error_constructor!(reinit_non_state, ReinitNonState { name: String });
    error_constructor!(
        unresolved_function_call,
        UnresolvedFunctionCall { name: String }
    );
    error_constructor!(function_without_body, FunctionWithoutBody { name: String });
    error_constructor!(
        constructor_field_projection_unresolved,
        ConstructorFieldProjectionUnresolved { projection: String }
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
        Self::RuntimeContractViolation {
            detail: detail.into(),
        }
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
    fn test_missing_state_with_help() {
        let span = Span::from_offsets(SourceId(0), 0, 10);
        let err = ToDaeError::missing_state("x", span);

        // Check that help text is present
        use miette::Diagnostic;
        let help = err.help().map(|h| h.to_string());
        assert!(help.is_some());
        assert!(help.unwrap().contains("der()"));
    }
}
