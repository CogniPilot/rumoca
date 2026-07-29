//! Branded structural-analysis products over one checked DAE.

use rumoca_core::{Diagnostic, Label, PhaseError, PrimaryLabel, Span};
use rumoca_ir_dae as dae;

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
pub struct StructuredScalarBlock {
    pub span: Span,
    pub first_equation_index: usize,
    pub equations_per_point: usize,
    pub point_count: usize,
    pub extents: Vec<usize>,
    pub cell_strides: Vec<usize>,
    pub base_unknowns: Vec<usize>,
    pub unknown_steps: Vec<Vec<i64>>,
}

impl StructuredScalarBlock {
    #[must_use]
    pub fn scalar_block_count(&self) -> usize {
        self.point_count
            .checked_mul(self.equations_per_point)
            .expect("checked structured block row count is representable")
    }

    pub fn scalar_rows(
        &self,
    ) -> impl Iterator<Item = Result<(EquationRef, usize), StructuralError>> + '_ {
        (0..self.point_count).flat_map(move |point| {
            (0..self.equations_per_point).map(move |position| {
                self.scalar_row(point, position)
                    .ok_or_else(|| StructuralError::ContractViolation {
                        reason: format!(
                            "compact family at f_x[{}] cannot address point {point}, body {position}",
                            self.first_equation_index
                        ),
                        span: self.span,
                    })
            })
        })
    }

    fn scalar_row(&self, point: usize, position: usize) -> Option<(EquationRef, usize)> {
        let equation = point
            .checked_mul(self.equations_per_point)?
            .checked_add(position)?
            .checked_add(self.first_equation_index)?;
        let mut unknown = i64::try_from(*self.base_unknowns.get(position)?).ok()?;
        for (dimension, step) in self.unknown_steps.get(position)?.iter().enumerate() {
            let stride = *self.cell_strides.get(dimension)?;
            let extent = *self.extents.get(dimension)?;
            let coordinate = if stride == 0 || extent == 0 {
                0
            } else {
                (point / stride) % extent
            };
            unknown = unknown.checked_add(i64::try_from(coordinate).ok()?.checked_mul(*step)?)?;
        }
        Some((EquationRef(equation), usize::try_from(unknown).ok()?))
    }
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
    },
    StructuredScalar(StructuredScalarBlock),
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

    #[must_use]
    pub fn loop_size(&self) -> Option<usize> {
        match self {
            Self::AlgebraicLoop { equations, .. } => Some(equations.len()),
            Self::Scalar { .. } | Self::StructuredScalar(_) => None,
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
            Self::Projection { span, .. } | Self::ContractViolation { span, .. }
                if !span.is_dummy() =>
            {
                Some(*span)
            }
            Self::EmptySystem
            | Self::Projection { .. }
            | Self::ContractViolation { .. }
            | Self::UnspannedContractViolation { .. } => None,
        }
    }
}

impl PhaseError for StructuralError {
    fn to_diagnostic(&self) -> Diagnostic {
        let mut diagnostic = match self.source_span() {
            Some(span) => Diagnostic::error(
                self.code(),
                self.to_string(),
                PrimaryLabel::new(span).with_message("structural analysis failed here"),
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
        diagnostic
    }
}
