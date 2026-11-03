//! The workspace of one directional-derivative evaluation, issued from its
//! phase demands.
//!
//! A tangent evaluation keeps one seed column and one state column for its
//! whole run, then runs its phases one after another: the right-hand side,
//! each simultaneous assignment segment, an optional implicit solve and an
//! optional consistency check. Because no two phases are live at once, one
//! scratch region of the largest phase demand serves all of them, and the
//! implicit solve and the consistency check, the two phases that also need a
//! private region (each beginning with its own saved seed), share one region
//! of the larger demand. Every extent is checked; a printer or evaluator
//! uses the issued extents and derives none.

/// The implicit solve phase: `unknowns` simultaneous targets solved through a
/// dense `unknowns x unknowns` system, and its program's output count.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SolveImplicitTangentDemand {
    pub unknowns: usize,
    pub outputs: usize,
}

/// The consistency phase: `rows` checked rows and its program's output count.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SolveConsistencyTangentDemand {
    pub rows: usize,
    pub outputs: usize,
}

/// What each phase of one tangent evaluation needs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SolveTangentDemand<'a> {
    /// Elements of the seed column.
    pub seed: usize,
    /// Elements of the state column.
    pub states: usize,
    /// Outputs of the right-hand-side program.
    pub rhs_outputs: usize,
    /// Targets of each simultaneous assignment segment, in order.
    pub segment_targets: &'a [usize],
    pub implicit: Option<SolveImplicitTangentDemand>,
    pub consistency: Option<SolveConsistencyTangentDemand>,
}

/// The issued workspace extents.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SolveTangentWorkspace {
    implicit_matrix_len: usize,
    implicit_pivot_len: usize,
    scratch_len: usize,
    private_len: usize,
    work_len: usize,
}

/// Which extent of a tangent workspace overflows.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveTangentWorkspaceError {
    ImplicitMatrix,
    ImplicitPivot,
    ImplicitPrivate,
    ConsistencyPrivate,
    Workspace,
}

impl std::fmt::Display for SolveTangentWorkspaceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::ImplicitMatrix => "implicit matrix extent overflow",
            Self::ImplicitPivot => "implicit pivot extent overflow",
            Self::ImplicitPrivate => "implicit workspace extent overflow",
            Self::ConsistencyPrivate => "consistency workspace extent overflow",
            Self::Workspace => "tangent workspace extent overflow",
        })
    }
}

impl std::error::Error for SolveTangentWorkspaceError {}

impl SolveTangentWorkspace {
    /// Issue the workspace for `demand`.
    pub fn issue(demand: &SolveTangentDemand<'_>) -> Result<Self, SolveTangentWorkspaceError> {
        let mut scratch_len = demand
            .segment_targets
            .iter()
            .copied()
            .fold(demand.rhs_outputs, usize::max);
        let (mut private_len, mut implicit_matrix_len, mut implicit_pivot_len) = (0, 0, 0);
        if let Some(implicit) = demand.implicit {
            let n = implicit.unknowns;
            implicit_pivot_len = n
                .checked_mul(2)
                .ok_or(SolveTangentWorkspaceError::ImplicitPivot)?;
            implicit_matrix_len = n
                .checked_mul(n)
                .ok_or(SolveTangentWorkspaceError::ImplicitMatrix)?;
            private_len = demand
                .seed
                .checked_add(implicit_matrix_len)
                .and_then(|length| length.checked_add(n))
                .ok_or(SolveTangentWorkspaceError::ImplicitPrivate)?;
            scratch_len = scratch_len.max(implicit.outputs);
        }
        if let Some(consistency) = demand.consistency {
            let checks = demand
                .seed
                .checked_add(consistency.rows)
                .ok_or(SolveTangentWorkspaceError::ConsistencyPrivate)?;
            private_len = private_len.max(checks);
            scratch_len = scratch_len.max(consistency.outputs);
        }
        let work_len = demand
            .seed
            .checked_add(demand.states)
            .and_then(|length| length.checked_add(scratch_len))
            .and_then(|length| length.checked_add(private_len))
            .ok_or(SolveTangentWorkspaceError::Workspace)?;
        Ok(Self {
            implicit_matrix_len,
            implicit_pivot_len,
            scratch_len,
            private_len,
            work_len,
        })
    }

    /// Elements of the implicit system's dense matrix.
    #[must_use]
    pub const fn implicit_matrix_len(&self) -> usize {
        self.implicit_matrix_len
    }

    /// Pivot and row-size entries of the implicit solve.
    #[must_use]
    pub const fn implicit_pivot_len(&self) -> usize {
        self.implicit_pivot_len
    }

    /// The scratch region every phase reuses in turn.
    #[must_use]
    pub const fn scratch_len(&self) -> usize {
        self.scratch_len
    }

    /// The private region the implicit solve and the consistency check reuse
    /// in turn.
    #[must_use]
    pub const fn private_len(&self) -> usize {
        self.private_len
    }

    /// The whole workspace: seed, states, scratch and the private region.
    #[must_use]
    pub const fn work_len(&self) -> usize {
        self.work_len
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn demand(segments: &[usize]) -> SolveTangentDemand<'_> {
        SolveTangentDemand {
            seed: 4,
            states: 6,
            rhs_outputs: 3,
            segment_targets: segments,
            implicit: None,
            consistency: None,
        }
    }

    #[test]
    fn sequential_phases_share_one_scratch_region_of_the_largest_demand() {
        let workspace = SolveTangentWorkspace::issue(&demand(&[2, 7, 1])).unwrap();
        assert_eq!(workspace.scratch_len(), 7);
        assert_eq!(workspace.private_len(), 0);
        assert_eq!(workspace.work_len(), 4 + 6 + 7);
        assert_eq!(
            (
                workspace.implicit_matrix_len(),
                workspace.implicit_pivot_len()
            ),
            (0, 0)
        );
    }

    #[test]
    fn the_implicit_solve_and_consistency_reuse_one_private_region() {
        let mut both = demand(&[2]);
        both.implicit = Some(SolveImplicitTangentDemand {
            unknowns: 3,
            outputs: 5,
        });
        both.consistency = Some(SolveConsistencyTangentDemand {
            rows: 20,
            outputs: 9,
        });
        let workspace = SolveTangentWorkspace::issue(&both).unwrap();
        assert_eq!(workspace.implicit_matrix_len(), 9);
        assert_eq!(workspace.implicit_pivot_len(), 6);
        // Implicit needs 4 + 9 + 3 = 16, consistency 4 + 20 = 24: the
        // phases never overlap, so the larger region serves both, not 40.
        assert_eq!(workspace.private_len(), 24);
        assert_eq!(workspace.scratch_len(), 9);
        assert_eq!(workspace.work_len(), 4 + 6 + 9 + 24);

        both.consistency = None;
        let implicit = SolveTangentWorkspace::issue(&both).unwrap();
        assert_eq!(implicit.private_len(), 16);
        assert_eq!(implicit.scratch_len(), 5);
    }

    #[test]
    fn every_overflowing_extent_is_refused() {
        let huge = SolveImplicitTangentDemand {
            unknowns: usize::MAX / 2 + 1,
            outputs: 1,
        };
        let mut matrix = demand(&[]);
        matrix.implicit = Some(SolveImplicitTangentDemand {
            unknowns: 1usize << (usize::BITS / 2),
            outputs: 1,
        });
        assert_eq!(
            SolveTangentWorkspace::issue(&matrix),
            Err(SolveTangentWorkspaceError::ImplicitMatrix)
        );
        let mut pivot = demand(&[]);
        pivot.implicit = Some(huge);
        assert_eq!(
            SolveTangentWorkspace::issue(&pivot),
            Err(SolveTangentWorkspaceError::ImplicitPivot)
        );
        let mut private = demand(&[]);
        private.seed = usize::MAX;
        private.implicit = Some(SolveImplicitTangentDemand {
            unknowns: 1,
            outputs: 1,
        });
        assert_eq!(
            SolveTangentWorkspace::issue(&private),
            Err(SolveTangentWorkspaceError::ImplicitPrivate)
        );
        let mut checks = demand(&[]);
        checks.seed = usize::MAX;
        checks.consistency = Some(SolveConsistencyTangentDemand {
            rows: 1,
            outputs: 1,
        });
        assert_eq!(
            SolveTangentWorkspace::issue(&checks),
            Err(SolveTangentWorkspaceError::ConsistencyPrivate)
        );
        let mut total = demand(&[]);
        total.states = usize::MAX;
        assert_eq!(
            SolveTangentWorkspace::issue(&total),
            Err(SolveTangentWorkspaceError::Workspace)
        );
    }
}
