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
    /// An equation that carries no matched unknown.
    ///
    /// A BLT block is only built from a maximum matching, so this is reachable
    /// exactly when a caller decomposes a system that is not square. Naming the
    /// case in the type replaces the historical `"???"` string sentinel: an
    /// unmatched block is now distinguishable from a variable that happens to
    /// be called `???`, and every consumer is forced to decide what to do with
    /// it rather than reading a plausible-looking name
    /// (SPEC_0008 §"Fail-Fast Error Semantics").
    Unmatched { equation: usize },
}

impl std::fmt::Display for UnknownId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DerState(name) => write!(f, "der({name})"),
            Self::Variable(name) => write!(f, "{name}"),
            Self::SolverY(index) => write!(f, "y[{index}]"),
            Self::Unmatched { equation } => write!(f, "<unmatched f_x[{equation}]>"),
        }
    }
}

/// A whole regular equation family emitted as one BLT block.
///
/// Every scalar row of the family was *checked* — row by row, not extrapolated
/// from the base cell — to be its own singleton SCC: the row references exactly
/// one unknown, that unknown is the row's match, so the row has out-degree zero
/// in the dependency graph. Keeping the family compact here is what stops BLT
/// from materializing one block (and one cloned [`UnknownId`]) per array
/// element — see SPEC_0032 §2 for the domain ordering the expansion follows.
#[derive(Debug, Clone)]
pub struct StructuredScalarBlock {
    /// Span of the `for`-equation the family came from, so an expansion that
    /// cannot address one of its rows reports against real source.
    pub span: rumoca_core::Span,
    /// First scalar equation index of the family.
    pub first_equation_index: usize,
    /// Scalar equations emitted per domain point.
    pub equations_per_point: usize,
    /// Number of domain points.
    pub point_count: usize,
    /// Binder extents, in source binder declaration order.
    pub extents: Vec<usize>,
    /// Row-major cell strides, parallel to `extents`.
    pub cell_strides: Vec<usize>,
    /// Unknown index solved by each equation position in the base cell.
    pub base_unknowns: Vec<usize>,
    /// Per-(equation position, binder) unknown-index step.
    pub unknown_steps: Vec<Vec<i64>>,
}

impl StructuredScalarBlock {
    /// Number of scalar blocks this family stands in for.
    #[must_use]
    pub fn scalar_block_count(&self) -> usize {
        self.point_count.saturating_mul(self.equations_per_point)
    }

    /// Message for a consumer that cannot handle a compact family block.
    ///
    /// Two kinds of consumer use it. IC planning and Solve projection lowering
    /// build their incidence through [`crate::Incidence::new`], which carries no
    /// structured-matching descriptors, so the variant is genuinely unreachable
    /// there. BLT elimination *can* be reached by a compact block and expands it
    /// first; the message covers the case where a caller skipped that step.
    /// Either way the consumer reports instead of silently dropping the
    /// family's rows.
    #[must_use]
    pub fn unsupported_by(&self, consumer: &str) -> String {
        format!(
            "{consumer} received a compact family block covering {} scalar rows from f_x[{}]",
            self.scalar_block_count(),
            self.first_equation_index
        )
    }

    /// Expand to `(equation, unknown index)` pairs in domain order
    /// (`first + point * per_point + position`, innermost binder fastest).
    ///
    /// A row whose index arithmetic does not close (an overflow, or a descriptor
    /// whose per-position vectors are shorter than `equations_per_point`) yields
    /// an `Err` rather than being skipped: dropping it would silently shrink the
    /// decomposition below the equation set it claims to cover, which
    /// SPEC_0008 §"Fail-Fast Error Semantics" forbids.
    pub fn scalar_rows(
        &self,
    ) -> impl Iterator<Item = Result<(EquationRef, usize), StructuralError>> + '_ {
        (0..self.point_count).flat_map(move |point| {
            (0..self.equations_per_point).map(move |position| {
                self.scalar_row(point, position)
                    .ok_or_else(|| self.unaddressable_row(point, position))
            })
        })
    }

    fn unknown_out_of_range(
        &self,
        equation: &EquationRef,
        unknown_index: usize,
    ) -> StructuralError {
        StructuralError::ContractViolation {
            reason: format!(
                "compact family block row {equation} names unknown index {unknown_index}, outside the unknown table it is expanded against"
            ),
            span: self.span,
        }
    }

    fn unaddressable_row(&self, point: usize, position: usize) -> StructuralError {
        StructuralError::ContractViolation {
            reason: format!(
                "compact family block at f_x[{}] cannot address point {point} position {position} of its {}x{} domain",
                self.first_equation_index, self.point_count, self.equations_per_point
            ),
            span: self.span,
        }
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
    /// A regular equation family whose rows are each their own scalar block,
    /// kept compact instead of expanded.
    StructuredScalar(StructuredScalarBlock),
}

impl BltBlock {
    /// Number of scalar blocks the block stands for. `AlgebraicLoop` counts as
    /// zero scalar blocks; use [`Self::loop_size`] for its dimension.
    #[must_use]
    pub fn scalar_block_count(&self) -> usize {
        match self {
            Self::Scalar { .. } => 1,
            Self::AlgebraicLoop { .. } => 0,
            Self::StructuredScalar(block) => block.scalar_block_count(),
        }
    }

    /// Dimension of an algebraic loop, or `None` for causal blocks.
    #[must_use]
    pub fn loop_size(&self) -> Option<usize> {
        match self {
            Self::AlgebraicLoop { equations, .. } => Some(equations.len()),
            Self::Scalar { .. } | Self::StructuredScalar(..) => None,
        }
    }

    /// Expand a causal block into its `(equation, unknown)` pairs in evaluation
    /// order. Algebraic loops yield nothing.
    ///
    /// An index a compact family names but `unknown_names` does not hold is an
    /// `Err`. Reporting the unknown as [`UnknownId::Unmatched`] would be an
    /// invented value — the row *is* matched, the caller simply passed a name
    /// table the block was not built against — and SPEC_0008
    /// §"Fail-Fast Error Semantics" prohibits substituting one.
    pub fn expand_scalar_blocks<'a>(
        &'a self,
        unknown_names: &'a [UnknownId],
    ) -> Box<dyn Iterator<Item = Result<(EquationRef, UnknownId), StructuralError>> + 'a> {
        match self {
            Self::Scalar { equation, unknown } => {
                Box::new(std::iter::once(Ok((equation.clone(), unknown.clone()))))
            }
            Self::AlgebraicLoop { .. } => Box::new(std::iter::empty()),
            Self::StructuredScalar(block) => Box::new(block.scalar_rows().map(move |row| {
                let (equation, unknown_index) = row?;
                let unknown = unknown_names
                    .get(unknown_index)
                    .cloned()
                    .ok_or_else(|| block.unknown_out_of_range(&equation, unknown_index))?;
                Ok((equation, unknown))
            })),
        }
    }
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

/// The minimal structurally over-determined block around a singular matching.
///
/// Alternating-path reachability from the unmatched equation rows selects the
/// Dulmage-Mendelsohn over-determined part: every unknown it reaches is already
/// matched inside the block, so the block carries strictly more equations than
/// unknowns and *is* the redundant constraint set. Naming its equations turns
/// "something is singular" into "these equations over-constrain these
/// unknowns", which is the only form of the diagnostic a modeller can act on.
///
/// `equations == 0` means the block was not computed (the caller had no
/// incidence to walk), never that the block is empty.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SingularBlockWitness {
    /// Equations reachable from the unmatched rows by alternating paths.
    pub equations: usize,
    /// Unknowns reachable from the unmatched rows by alternating paths.
    pub unknowns: usize,
    /// Labels of the first few block equations, in equation order.
    pub sample: Vec<String>,
}

impl std::fmt::Display for SingularBlockWitness {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.equations == 0 {
            return Ok(());
        }
        write!(
            f,
            "; over-determined block: {} equations over {} unknowns: {}",
            self.equations,
            self.unknowns,
            summarize_names_with_limit(&self.sample, self.sample.len())
        )
    }
}

/// Errors from structural analysis that prevent simulation code generation.
#[derive(Debug, Clone, thiserror::Error)]
pub enum StructuralError {
    /// The system is structurally singular: no perfect matching exists.
    #[error(
        "structurally singular system: {n_matched} matched out of {n_equations} equations and {n_unknowns} unknowns; unmatched equations: {}; unmatched unknowns: {}{over_determined_block}",
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
        /// The Dulmage-Mendelsohn over-determined block around the unmatched
        /// rows: the equations an alternating path can reach from them.
        ///
        /// Boxed so the `Singular` payload stays under the `result_large_err`
        /// threshold: every `Result<_, StructuralError>` in this crate pays for
        /// the largest variant, and the witness is only ever read on the
        /// failure path.
        over_determined_block: Box<SingularBlockWitness>,
    },
    /// The system has no equations or unknowns.
    ///
    /// Span-free: emptiness is a whole-system property with no owning source
    /// construct — there is no equation or declaration to point at.
    #[error("empty system: no equations or unknowns")]
    EmptySystem,
    /// An IC plan referenced an unknown that cannot be mapped to the solver vector.
    ///
    /// Span-free: the unresolved unknown is a solver-vector identity produced by
    /// layout, not a source declaration, so it carries no honest span.
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
    ///
    /// Span-free: this variant exists precisely because the malformed metadata
    /// carries only `Span::DUMMY`; fabricating a span here would violate the
    /// SPEC_0008 rule that dummy spans are reserved for genuinely source-free
    /// constructs. `ContractViolation` is the spanned counterpart.
    #[error("invalid structural IR contract without source span: {reason}")]
    UnspannedContractViolation { reason: String },
}

impl StructuralError {
    /// Stable diagnostic code (SPEC_0008 `ES0xx` range).
    ///
    /// `ContractViolation` and `UnspannedContractViolation` intentionally share
    /// [`ES014_CONTRACT_VIOLATION`](crate::diagnostic_codes::ES014_CONTRACT_VIOLATION):
    /// same defect class, with span presence reported by [`Self::source_span`].
    #[must_use]
    pub const fn code(&self) -> &'static str {
        use crate::diagnostic_codes as codes;
        match self {
            Self::Singular { .. } => codes::ES010_SINGULAR_SYSTEM,
            Self::EmptySystem => codes::ES011_EMPTY_SYSTEM,
            Self::InvalidIcPlanUnknown { .. } => codes::ES012_INVALID_IC_PLAN_UNKNOWN,
            Self::InconsistentEquation { .. } => codes::ES013_INCONSISTENT_EQUATION,
            Self::ContractViolation { .. } | Self::UnspannedContractViolation { .. } => {
                codes::ES014_CONTRACT_VIOLATION
            }
        }
    }

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
    summarize_names_with_limit(names, 12)
}

fn summarize_names_with_limit(names: &[String], limit: usize) -> String {
    if names.is_empty() {
        return "-".to_string();
    }
    let mut summary: Vec<String> = names.iter().take(limit).cloned().collect();
    if names.len() > limit {
        summary.push(format!("... +{}", names.len() - limit));
    }
    summary.join(", ")
}
