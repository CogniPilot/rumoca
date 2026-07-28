//! Phase-local error types for the ToDae phase.
//!
//! Error codes: ED0xx for todae phase (per SPEC_0008).
//!
//! Uses miette for rich diagnostic output with error codes and help text.

use miette::Diagnostic;
use rumoca_core::{
    BoxedResult, Diagnostic as CommonDiagnostic, PhaseError, Span, error_constructor,
    miette_phase_error_to_diagnostic,
};
use thiserror::Error;

/// Type alias for ToDae results with boxed errors.
pub type ToDaeResult<T> = BoxedResult<T, ToDaeError>;

/// Errors that can occur during DAE conversion.
#[derive(Debug, Clone, Error, Diagnostic)]
pub enum ToDaeError {
    /// The model is unbalanced (equations don't match unknowns).
    ///
    /// `detail` carries the full component breakdown so the failure can be
    /// root-caused without recompiling: which unknown/equation partition
    /// dominates, which balance clamps were exercised, and how many continuous
    /// equation rows were filtered out of `f_x` and why.
    #[error("unbalanced model: {equations} equations, {unknowns} unknowns (balance = {balance})")]
    #[diagnostic(
        code(rumoca::todae::ED001),
        help(
            "MLS §4.9: A balanced model has the same number of equations as unknowns; breakdown: {detail}"
        )
    )]
    Unbalanced {
        equations: usize,
        unknowns: usize,
        balance: i64,
        detail: crate::balance::BalanceBreakdown,
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
        span: Span,
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
        span: Span,
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
        span: Span,
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
        span: Span,
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
        span: Span,
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
        span: Span,
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

    /// Solver-facing runtime metadata failed internal consistency checks at a
    /// known IR/source owner.
    #[error("invalid runtime metadata: {detail}")]
    #[diagnostic(
        code(rumoca::todae::ED012),
        help("runtime metadata must be complete and internally consistent before simulation")
    )]
    RuntimeMetadataViolationAt {
        detail: String,
        ir_span: Span,
        #[label("invalid runtime metadata here")]
        span: Span,
    },

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
        span: Span,
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
        span: Span,
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
        ir_span: Span,
        #[label("invalid runtime contract originates here")]
        span: Span,
    },

    /// Solver-facing DAE failed required runtime contract checks without source provenance.
    #[error("invalid runtime contract: {detail}")]
    #[diagnostic(
        code(rumoca::todae::ED015),
        help(
            "runtime DAE must provide coherent variable partitions and discrete update partitions before simulation"
        )
    )]
    UnspannedRuntimeContractViolation { detail: String },

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
        span: Span,
    },

    /// The virtual connection graph (MLS §9.4) is invalid: required
    /// spanning-tree edges form a cycle, or two definite roots are connected.
    #[error("invalid connection graph: {detail}")]
    #[diagnostic(
        code(rumoca::todae::ED017),
        help(
            "MLS §9.4: Connections.branch() edges must form a forest and each spanning tree may contain at most one Connections.root()"
        )
    )]
    InvalidConnectionGraph {
        detail: String,
        #[label("connection graph constructed from these connectors")]
        span: Span,
    },

    /// The source uses an MLS runtime operator whose semantics are not yet
    /// implemented by the canonical DAE/runtime pipeline.
    #[error("unsupported runtime operator `{operator}`: {detail}")]
    #[diagnostic(
        code(rumoca::todae::ED018),
        help(
            "Rumoca rejects this operator until its runtime semantics are implemented; accepting it with a passthrough or constant fallback would produce incorrect simulation results"
        )
    )]
    UnsupportedRuntimeOperator {
        operator: String,
        detail: String,
        #[label("unsupported runtime operator used here")]
        span: Span,
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
        unsupported_runtime_operator,
        UnsupportedRuntimeOperator {
            operator: String,
            detail: String
        }
    );
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

    /// Create an Unbalanced error from the balance breakdown that produced it.
    ///
    /// `equations`/`unknowns`/`balance` are derived from `detail` so the error
    /// payload and the balance gate can never disagree.
    pub fn unbalanced_from_detail(detail: crate::balance::BalanceDetail) -> Self {
        let (equations, unknowns) = detail.equations_unknowns();
        let balance = detail.balance();
        Self::Unbalanced {
            equations,
            unknowns,
            balance,
            detail: crate::balance::BalanceBreakdown::from(detail),
        }
    }

    /// The balance breakdown carried by an [`ToDaeError::Unbalanced`] error.
    pub fn balance_detail(&self) -> Option<&crate::balance::BalanceDetail> {
        match self {
            Self::Unbalanced { detail, .. } => Some(detail),
            _ => None,
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

    /// Create a runtime metadata invariant error with the best available IR span.
    pub fn runtime_metadata_violation_at(detail: impl Into<String>, span: Span) -> Self {
        Self::RuntimeMetadataViolationAt {
            detail: detail.into(),
            ir_span: span,
            span,
        }
    }

    /// Create a runtime contract invariant error.
    pub fn runtime_contract_violation(detail: impl Into<String>) -> Self {
        Self::UnspannedRuntimeContractViolation {
            detail: detail.into(),
        }
    }

    /// Create a runtime contract invariant error with the best available IR span.
    pub fn runtime_contract_violation_at(detail: impl Into<String>, span: Span) -> Self {
        Self::RuntimeContractViolation {
            detail: detail.into(),
            ir_span: span,
            span,
        }
    }

    /// Create a runtime contract invariant error with an optional real IR span.
    pub fn runtime_contract_violation_with_span(detail: impl Into<String>, span: Span) -> Self {
        if span.is_dummy() {
            Self::runtime_contract_violation(detail)
        } else {
            Self::runtime_contract_violation_at(detail, span)
        }
    }

    pub fn source_span(&self) -> Option<Span> {
        self.diagnostic_source_spans()
            .first()
            .copied()
            .and_then(real_span)
    }

    fn diagnostic_source_spans(&self) -> &[Span] {
        match self {
            Self::ReinitNonState { span, .. }
            | Self::UnresolvedFunctionCall { span, .. }
            | Self::FunctionWithoutBody { span, .. }
            | Self::ConstructorFieldSelectionUnresolved { span, .. }
            | Self::UnresolvedReference { span, .. }
            | Self::DiscreteSolvedFormViolation { span, .. }
            | Self::RuntimeMetadataViolationAt { span, .. }
            | Self::UnsupportedAlgorithm { span, .. }
            | Self::StrictSolverDaeViolation { span, .. }
            | Self::RuntimeContractViolation { span, .. }
            | Self::SourceTemporalOperatorSurvivedDaeBoundary { span, .. }
            | Self::InvalidConnectionGraph { span, .. }
            | Self::UnsupportedRuntimeOperator { span, .. } => std::slice::from_ref(span),
            Self::Unbalanced { .. }
            | Self::Internal(_)
            | Self::UnresolvedClockSchedule { .. }
            | Self::ConditionPartitionViolation { .. }
            | Self::RuntimeMetadataViolation { .. }
            | Self::UnspannedRuntimeContractViolation { .. } => &[],
        }
    }
}

impl PhaseError for ToDaeError {
    fn to_diagnostic(&self) -> CommonDiagnostic {
        miette_phase_error_to_diagnostic(self, self.diagnostic_source_spans())
    }
}

fn real_span(span: Span) -> Option<Span> {
    (!span.is_dummy()).then_some(span)
}

impl From<crate::balance::BalanceError> for ToDaeError {
    fn from(error: crate::balance::BalanceError) -> Self {
        match error.source_span() {
            Some(span) => ToDaeError::runtime_contract_violation_at(error.to_string(), span),
            None => ToDaeError::runtime_contract_violation(error.to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::balance::BalanceDetail;
    use rumoca_core::{SourceId, Span};

    fn unbalanced_detail(f_x: usize, unknowns: usize) -> BalanceDetail {
        BalanceDetail {
            alg_unknowns: unknowns,
            f_x_scalar: f_x,
            ..BalanceDetail::default()
        }
    }

    #[test]
    fn test_unbalanced_error() {
        let err = ToDaeError::unbalanced_from_detail(unbalanced_detail(5, 3));
        assert!(format!("{err}").contains("5 equations"));
        assert!(format!("{err}").contains("3 unknowns"));

        // Check that miette code is present
        use miette::Diagnostic;
        let code = err.code().map(|c| c.to_string());
        assert_eq!(code, Some("rumoca::todae::ED001".to_string()));
    }

    #[test]
    fn unbalanced_error_carries_detail_and_ed001_code() {
        use miette::Diagnostic;
        let detail = BalanceDetail {
            interface_flow_count: 4,
            ..unbalanced_detail(5, 3)
        };
        let (expected_equations, expected_unknowns) = detail.equations_unknowns();
        let expected_balance = detail.balance();
        let err = ToDaeError::unbalanced_from_detail(detail);

        let ToDaeError::Unbalanced {
            equations,
            unknowns,
            balance,
            detail: carried,
        } = &err
        else {
            panic!("expected an Unbalanced error, got {err:?}");
        };
        assert_eq!(*equations, expected_equations);
        assert_eq!(*unknowns, expected_unknowns);
        assert_eq!(*balance, expected_balance);
        assert_eq!(carried.f_x_scalar, 5);
        assert_eq!(carried.interface_flow_count, 4);

        assert_eq!(
            err.code().map(|c| c.to_string()).as_deref(),
            Some("rumoca::todae::ED001")
        );
        let help = err
            .help()
            .map(|h| h.to_string())
            .expect("ED001 must carry help text");
        assert!(help.contains("f_x=5"), "{help}");
        assert!(help.contains("iflow=4"), "{help}");
        assert!(help.contains("clamps["), "{help}");
        assert!(!help.contains('\n'), "help must stay single-line: {help}");
        assert_eq!(
            err.balance_detail().map(|detail| detail.f_x_scalar),
            Some(5)
        );
    }

    #[test]
    fn active_todae_errors_keep_stable_diagnostic_codes() {
        let span = Span::from_offsets(SourceId::from_source_name("errors_fixture.mo"), 0, 10);
        use miette::Diagnostic;

        let cases = [
            (
                ToDaeError::unbalanced_from_detail(unbalanced_detail(5, 3)),
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

    #[test]
    fn phase_error_preserves_source_identity_and_help() {
        let span = Span::from_offsets(
            SourceId::from_source_name("phase_dae_phase_error.mo"),
            12,
            21,
        );
        let error =
            ToDaeError::unsupported_runtime_operator("spatialDistribution", "not lowered", span);
        let diagnostic = error.to_diagnostic();

        assert_eq!(diagnostic.code.as_deref(), Some("ED018"));
        assert_eq!(diagnostic.labels[0].span, span);
        assert!(
            diagnostic
                .notes
                .iter()
                .any(|note| note.contains("rejects this operator"))
        );
    }
}
