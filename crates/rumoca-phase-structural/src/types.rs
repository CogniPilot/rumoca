//! Branded structural-analysis products over one checked DAE.

use rumoca_core::{Diagnostic, Label, PhaseError, PrimaryLabel, Span};
use rumoca_ir_dae as dae;

pub use crate::blt::{StructuredFamilyContext, StructuredScalarBlock};

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct EquationRef(pub usize);

impl std::fmt::Display for EquationRef {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "f_x[{}]", self.0)
    }
}

/// One scalar unknown, carrying the branded declaration identity that owns it.
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum UnknownId<'dae> {
    Derivative {
        state: dae::StateId<'dae>,
        scalar: u32,
    },
    Algebraic {
        variable: dae::AlgebraicId<'dae>,
        scalar: u32,
    },
    /// Solver-only incidence has no DAE declaration identity.
    Solver(usize),
    Unmatched {
        equation: usize,
    },
}

#[derive(Debug, Clone)]
pub enum BltBlock<'dae> {
    Scalar {
        equation: EquationRef,
        unknown: UnknownId<'dae>,
    },
    AlgebraicLoop {
        equations: Vec<EquationRef>,
        unknowns: Vec<UnknownId<'dae>>,
        /// Greedy tearing of the loop, when one reduces the solve dimension.
        /// Local indices address `equations` and `unknowns` by position.
        tearing: Option<crate::tearing::TearingResult>,
    },
    StructuredScalar(StructuredScalarBlock<'dae>),
}

impl BltBlock<'_> {
    #[must_use]
    pub fn scalar_block_count(&self) -> usize {
        match self {
            Self::Scalar { .. } => 1,
            Self::AlgebraicLoop { .. } => 0,
            Self::StructuredScalar(block) => block.scalar_block_count(),
        }
    }
}

#[derive(Debug)]
pub struct SortedDae<'dae> {
    pub blocks: Vec<BltBlock<'dae>>,
    pub matching: Vec<(EquationRef, UnknownId<'dae>)>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SingularBlockWitness {
    pub equations: usize,
    pub unknowns: usize,
    pub sample: Vec<String>,
}

impl std::fmt::Display for SingularBlockWitness {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.equations == 0 {
            return Ok(());
        }
        write!(
            formatter,
            "; over-determined block: {} equations over {} unknowns",
            self.equations, self.unknowns
        )
    }
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum StructuralError {
    #[error(
        "structurally singular system: {n_matched} matched out of {n_equations} equations and {n_unknowns} unknowns"
    )]
    Singular {
        n_equations: usize,
        n_unknowns: usize,
        n_matched: usize,
        unmatched_equations: Vec<String>,
        unmatched_unknowns: Vec<String>,
        unmatched_unknown_spans: Vec<Span>,
        over_determined_block: Box<SingularBlockWitness>,
    },
    /// Span-free: an empty whole-model system has no equation or variable owner.
    #[error("empty system: no equations or unknowns")]
    EmptySystem,
    /// The only index reduction available demoted a state whose `fixed = true`
    /// start MLS 3.6 §8.6 turns into an initialization equation, and no equation
    /// the reduction keeps reproduces that equation. Reducing anyway would
    /// answer with a guess in place of a stated initial condition, so the phase
    /// reports the condition it would have had to drop instead.
    #[error(
        "index reduction would discard the stated initial value of `{variable}`: MLS 3.6 section 8.6 adds `{variable} = {variable}.start` to the initialization equations for a `fixed = true` variable, and demoting `{variable}` leaves no equation that states it"
    )]
    DroppedStatedInitialValue { variable: String, span: Span },
    /// Two coordinates the system proves equal — up to sign and a time-invariant
    /// displacement — each carry a `fixed = true` start, and the two starts do
    /// not state the same value. MLS 3.6 §8.6 adds both as initialization
    /// equations, so the initialization system they describe has no solution;
    /// picking one silently would answer with an initial condition the model
    /// never stated.
    #[error(
        "conflicting stated initial values: `{variable}` and `{other}` are the same quantity, and MLS 3.6 section 8.6 adds both `fixed = true` starts to the initialization equations"
    )]
    ConflictingStatedInitialValues {
        variable: String,
        other: String,
        span: Span,
        other_span: Span,
    },
    /// A constant pure call (SPEC_0043 §4) whose evaluation at construction
    /// fails: an assertion, or a non-finite or out-of-range value. The same
    /// evaluation would fail at every instant the model runs it.
    #[error("constant call of `{call}` cannot be evaluated: {reason}")]
    ConstantCallEvaluation {
        call: String,
        reason: String,
        span: Span,
    },
    #[error("checked DAE scalar projection failed: {reason}")]
    Projection { reason: String, span: Span },
    #[error("invalid structural IR contract: {reason}")]
    ContractViolation { reason: String, span: Span },
    /// Span-free: the violated aggregate contract has no single honest source owner.
    #[error("invalid structural IR contract without source span: {reason}")]
    UnspannedContractViolation { reason: String },
}

impl StructuralError {
    #[must_use]
    pub const fn code(&self) -> &'static str {
        use crate::diagnostic_codes as codes;
        match self {
            Self::Singular { .. } => codes::ES010_SINGULAR_SYSTEM,
            Self::EmptySystem => codes::ES011_EMPTY_SYSTEM,
            Self::DroppedStatedInitialValue { .. } => codes::ES012_DROPPED_STATED_INITIAL_VALUE,
            Self::ConflictingStatedInitialValues { .. } => {
                codes::ES013_CONFLICTING_STATED_INITIAL_VALUES
            }
            Self::ConstantCallEvaluation { .. } => codes::ES015_CONSTANT_CALL_EVALUATION,
            Self::Projection { .. }
            | Self::ContractViolation { .. }
            | Self::UnspannedContractViolation { .. } => codes::ES014_CONTRACT_VIOLATION,
        }
    }

    #[must_use]
    pub fn source_span(&self) -> Option<Span> {
        match self {
            Self::Singular {
                unmatched_unknown_spans,
                ..
            } => unmatched_unknown_spans.first().copied(),
            Self::DroppedStatedInitialValue { span, .. }
            | Self::ConflictingStatedInitialValues { span, .. }
            | Self::ConstantCallEvaluation { span, .. }
            | Self::Projection { span, .. }
            | Self::ContractViolation { span, .. }
                if !span.is_dummy() =>
            {
                Some(*span)
            }
            Self::EmptySystem
            | Self::DroppedStatedInitialValue { .. }
            | Self::ConflictingStatedInitialValues { .. }
            | Self::ConstantCallEvaluation { .. }
            | Self::Projection { .. }
            | Self::ContractViolation { .. }
            | Self::UnspannedContractViolation { .. } => None,
        }
    }
}

impl PhaseError for StructuralError {
    fn to_diagnostic(&self) -> Diagnostic {
        let label = match self {
            Self::DroppedStatedInitialValue { .. } => {
                "this stated initial value has no equation left after index reduction"
            }
            Self::ConflictingStatedInitialValues { .. } => {
                "this stated initial value contradicts the one below"
            }
            Self::ConstantCallEvaluation { .. } => "this constant call fails whenever it runs",
            _ => "structural analysis failed here",
        };
        let mut diagnostic = match self.source_span() {
            Some(span) => Diagnostic::error(
                self.code(),
                self.to_string(),
                PrimaryLabel::new(span).with_message(label),
            ),
            None => Diagnostic::global_error(self.code(), self.to_string()),
        };
        if let Self::Singular {
            unmatched_unknown_spans,
            ..
        } = self
        {
            for span in unmatched_unknown_spans.iter().copied().skip(1) {
                diagnostic = diagnostic.with_label(
                    Label::secondary(span).with_message("unmatched structural unknown"),
                );
            }
        }
        if let Self::ConflictingStatedInitialValues { other_span, .. } = self
            && !other_span.is_dummy()
        {
            diagnostic = diagnostic.with_label(
                Label::secondary(*other_span)
                    .with_message("the same quantity is pinned to a different value here"),
            );
        }
        diagnostic
    }
}
