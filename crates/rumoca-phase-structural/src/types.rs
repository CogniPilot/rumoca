//! Types for BLT-sorted DAE structure.

use rumoca_core::Diagnostic;
use rumoca_ir_dae as dae;

/// Reference to a continuous equation in the original DAE `f_x` list.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct EquationRef(pub usize);

impl std::fmt::Display for EquationRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "f_x[{}]", self.0)
    }
}

/// Unknown variable in the DAE system.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum UnknownId {
    /// Derivative of a state variable: `der(x_i)`.
    DerState(rumoca_core::VarName),
    /// Algebraic or output variable: `z_j` or `w_k`.
    Variable(rumoca_core::VarName),
    /// Solver-vector scalar used by structural consumers outside DAE source.
    SolverY(usize),
}

impl std::fmt::Display for UnknownId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DerState(name) => write!(f, "der({name})"),
            Self::Variable(name) => write!(f, "{name}"),
            Self::SolverY(index) => write!(f, "y[{index}]"),
        }
    }
}

/// A single block in the BLT (Block Lower Triangular) decomposition.
#[derive(Debug, Clone)]
pub enum BltBlock {
    /// A scalar block: one equation matched to one unknown.
    Scalar {
        equation: EquationRef,
        unknown: UnknownId,
    },
    /// An algebraic loop: a set of equations that must be solved simultaneously.
    AlgebraicLoop {
        equations: Vec<EquationRef>,
        unknowns: Vec<UnknownId>,
    },
}

/// A DAE sorted into BLT block form for sequential simulation.
#[derive(Debug)]
pub struct SortedDae<'a> {
    /// Reference to the original DAE.
    pub dae: &'a dae::Dae,
    /// BLT blocks in evaluation order.
    pub blocks: Vec<BltBlock>,
    /// Full matching: each pair `(equation, unknown)` from the maximum matching.
    pub matching: Vec<(EquationRef, UnknownId)>,
    /// Diagnostic warnings (e.g. algebraic loop notifications).
    pub diagnostics: Vec<Diagnostic>,
}

/// Errors from structural analysis that prevent simulation code generation.
#[derive(Debug, Clone, thiserror::Error)]
pub enum StructuralError {
    /// The system is structurally singular: no perfect matching exists.
    #[error(
        "structurally singular system: {n_matched} matched out of {n_equations} equations and {n_unknowns} unknowns; unmatched equations: {}; unmatched unknowns: {}",
        summarize_singular_names(unmatched_equations),
        summarize_singular_names(unmatched_unknowns)
    )]
    Singular {
        n_equations: usize,
        n_unknowns: usize,
        n_matched: usize,
        unmatched_equations: Vec<String>,
        unmatched_unknowns: Vec<String>,
        /// Source spans of the unmatched unknowns, parallel to
        /// `unmatched_unknowns`, so the failure is traceable back to source
        /// when the unknown has source provenance.
        unmatched_unknown_spans: Vec<Option<rumoca_core::Span>>,
    },
    /// The system has no equations or unknowns.
    #[error("empty system: no equations or unknowns")]
    EmptySystem,
    /// An IC plan referenced an unknown that cannot be mapped to the solver vector.
    #[error("invalid IC plan: unresolved unknown `{name}`")]
    InvalidIcPlanUnknown { name: String },
    /// A source equation has a compiler-known nonzero residual and therefore
    /// cannot hold for any value of the continuous unknowns.
    #[error("inconsistent equation `{origin}`: constant residual is {residual}")]
    InconsistentEquation {
        residual: f64,
        origin: String,
        span: rumoca_core::Span,
    },
    /// DAE IR metadata required by structural analysis is missing or inconsistent.
    #[error("invalid structural IR contract: {reason}")]
    ContractViolation {
        reason: String,
        span: rumoca_core::Span,
    },
    /// DAE IR metadata required by structural analysis is missing or
    /// inconsistent, and the malformed metadata has no honest source span.
    #[error("invalid structural IR contract without source span: {reason}")]
    UnspannedContractViolation { reason: String },
}

impl StructuralError {
    /// Source span of the first unmatched unknown carrying one, so a structural
    /// singularity can be reported against the offending model variable.
    #[must_use]
    pub fn source_span(&self) -> Option<rumoca_core::Span> {
        match self {
            Self::Singular {
                unmatched_unknown_spans,
                ..
            } => unmatched_unknown_spans
                .iter()
                .find_map(|span| span.and_then(|span| (!span.is_dummy()).then_some(span))),
            Self::ContractViolation { span, .. } if !span.is_dummy() => Some(*span),
            Self::InconsistentEquation { span, .. } if !span.is_dummy() => Some(*span),
            Self::EmptySystem
            | Self::InvalidIcPlanUnknown { .. }
            | Self::InconsistentEquation { .. }
            | Self::ContractViolation { .. }
            | Self::UnspannedContractViolation { .. } => None,
        }
    }
}

fn summarize_singular_names(names: &[String]) -> String {
    const LIMIT: usize = 12;
    if names.is_empty() {
        return "-".to_string();
    }
    let mut summary: Vec<String> = names.iter().take(LIMIT).cloned().collect();
    if names.len() > LIMIT {
        summary.push(format!("... +{}", names.len() - LIMIT));
    }
    summary.join(", ")
}
