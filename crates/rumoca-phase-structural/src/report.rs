//! Human-readable structural analysis report.
//!
//! Captures, with names, the result of structural analysis so a developer can
//! see *which equation determines which variable* (the matching), which
//! variables are coupled (the strongly-connected components / algebraic loops),
//! and how each coupled block is torn (iteration variables + residual equations
//! + the causal solve order). Backs the `rumoca sim --structure` debug dump.

use std::fmt;

/// One coupled-block tearing: the iteration (tear) variables, the residual
/// equations matched to them, and the causal solve order for the remaining
/// variables. All entries are named.
#[derive(Debug, Clone)]
pub struct TearingReport {
    /// Tear (iteration) variable names — the nonlinear solve dimension.
    pub tear_vars: Vec<String>,
    /// Residual equation labels, one per tear variable.
    pub residual_equations: Vec<String>,
    /// Causal solve order: `(equation, variable)` pairs solved in sequence once
    /// the tear variables are known.
    pub causal_sequence: Vec<(String, String)>,
}

/// One block in the BLT (block lower triangular) evaluation order.
#[derive(Debug, Clone)]
pub enum BlockReport {
    /// A scalar block: one equation determines one unknown directly.
    Scalar {
        /// Equation label (e.g. `f_x[3] (inductor)`).
        equation: String,
        /// Unknown the equation determines (e.g. `der(inductor.i)`).
        unknown: String,
    },
    /// A coupled strongly-connected component (algebraic loop): a set of
    /// equations that must be solved simultaneously for a set of unknowns.
    Coupled {
        /// Equation labels in the block.
        equations: Vec<String>,
        /// Unknown names coupled in the block.
        unknowns: Vec<String>,
        /// Tearing of the block, when one could be computed.
        tearing: Option<TearingReport>,
    },
}

impl BlockReport {
    /// Number of unknowns the block determines.
    #[must_use]
    pub fn size(&self) -> usize {
        match self {
            Self::Scalar { .. } => 1,
            Self::Coupled { unknowns, .. } => unknowns.len(),
        }
    }

    /// Whether this is a coupled (algebraic-loop) block.
    #[must_use]
    pub fn is_coupled(&self) -> bool {
        matches!(self, Self::Coupled { .. })
    }
}

/// Structural analysis of a DAE: the matching plus the BLT blocks (with coupled
/// SCCs and their tearing).
#[derive(Debug, Clone)]
pub struct StructuralReport {
    /// Number of equations in the analyzed system.
    pub n_equations: usize,
    /// Number of unknowns in the analyzed system.
    pub n_unknowns: usize,
    /// Maximum matching as `(equation, unknown)` pairs — which equation
    /// determines which variable.
    pub matching: Vec<(String, String)>,
    /// BLT blocks in evaluation order.
    pub blocks: Vec<BlockReport>,
}

impl StructuralReport {
    /// Number of coupled (algebraic-loop) blocks.
    #[must_use]
    pub fn coupled_block_count(&self) -> usize {
        self.blocks
            .iter()
            .filter(|block| block.is_coupled())
            .count()
    }

    /// Size of the largest coupled block (1 if the system is fully sequential).
    #[must_use]
    pub fn largest_coupled_block(&self) -> usize {
        self.blocks
            .iter()
            .filter(|block| block.is_coupled())
            .map(BlockReport::size)
            .max()
            .unwrap_or(1)
    }
}

impl fmt::Display for TearingReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "           tear vars ({}): {}",
            self.tear_vars.len(),
            self.tear_vars.join(", ")
        )?;
        writeln!(
            f,
            "           residual eqs:  {}",
            self.residual_equations.join(", ")
        )?;
        for (equation, variable) in &self.causal_sequence {
            writeln!(f, "             solve {variable} <- {equation}")?;
        }
        Ok(())
    }
}

impl BlockReport {
    fn fmt_block(&self, f: &mut fmt::Formatter<'_>, index: usize) -> fmt::Result {
        match self {
            Self::Scalar { equation, unknown } => {
                writeln!(f, "  [{index:>4}] scalar   {unknown} <- {equation}")
            }
            Self::Coupled {
                equations,
                unknowns,
                tearing,
            } => {
                writeln!(
                    f,
                    "  [{index:>4}] coupled  {}x{} SCC",
                    equations.len(),
                    unknowns.len()
                )?;
                writeln!(f, "           unknowns:  {}", unknowns.join(", "))?;
                writeln!(f, "           equations: {}", equations.join(", "))?;
                match tearing {
                    Some(tearing) => write!(f, "{tearing}"),
                    None => writeln!(f, "           tearing:   <none found> (dense solve)"),
                }
            }
        }
    }
}

impl fmt::Display for StructuralReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "structure: {} equations, {} unknowns | {} BLT block(s), {} coupled (largest {})",
            self.n_equations,
            self.n_unknowns,
            self.blocks.len(),
            self.coupled_block_count(),
            self.largest_coupled_block(),
        )?;

        writeln!(f, "\nBLT blocks (evaluation order):")?;
        for (index, block) in self.blocks.iter().enumerate() {
            block.fmt_block(f, index)?;
        }
        Ok(())
    }
}
