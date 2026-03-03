//! Types for BLT-sorted DAE structure.

use rumoca_core::Diagnostic;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

/// Reference to an equation in the original [`dae::Dae`].
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum EquationRef {
    /// Continuous equation at index `i` in `dae.f_x` (MLS B.1a).
    Continuous(usize),
}

impl std::fmt::Display for EquationRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Continuous(i) => write!(f, "f_x[{i}]"),
        }
    }
}

/// Unknown variable in the DAE system.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum UnknownId {
    /// Derivative of a state variable: `der(x_i)`.
    DerState(flat::VarName),
    /// Algebraic or output variable: `z_j` or `w_k`.
    Variable(flat::VarName),
}

impl std::fmt::Display for UnknownId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DerState(name) => write!(f, "der({name})"),
            Self::Variable(name) => write!(f, "{name}"),
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
#[derive(Debug, thiserror::Error)]
pub enum StructuralError {
    /// The system is structurally singular: no perfect matching exists.
    #[error(
        "structurally singular system: {n_matched} matched out of {n_equations} equations and {n_unknowns} unknowns"
    )]
    Singular {
        n_equations: usize,
        n_unknowns: usize,
        n_matched: usize,
        unmatched_equations: Vec<String>,
        unmatched_unknowns: Vec<String>,
    },
    /// The system has no equations or unknowns.
    #[error("empty system: no equations or unknowns")]
    EmptySystem,
}
