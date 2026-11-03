//! The workspace of one directional-derivative evaluation, issued from its
//! checked phase schedule.
//!
//! A tangent evaluation of a [`SolveProblem`] keeps one seed column, over
//! the problem's seed domain (its `y` and `p` scalars and time), and one
//! column of its state scalars for its whole run, then runs its phases one
//! after another: the right-hand side,
//! each simultaneous assignment segment, an optional implicit solve and an
//! optional consistency check. [`SolveTangentSchedule`] admits exactly that
//! order, built only from the checked program of each phase; each phase's
//! demand is read from its program. Because the schedule is sequential, one
//! scratch region of the largest phase demand serves every phase, and the
//! implicit solve and the consistency check, the two phases that also need a
//! private region (each beginning with its own saved seed), share one region
//! of the larger demand. The issued workspace keeps the schedule it was
//! issued for (the problem and each phase's program, by identity), so a
//! consumer refuses a workspace issued for another problem or programs.

use crate::{ScalarProgramBlock, SolveProblem};

/// One phase of a tangent schedule and the program that runs it.
#[derive(Debug, Clone)]
enum Phase {
    Rhs,
    Assignment { targets: usize },
    Implicit { unknowns: usize },
    Consistency { rows: usize },
}

/// The checked, sequential phase schedule of one tangent evaluation.
#[derive(Debug, Clone)]
pub struct SolveTangentSchedule<'problem> {
    problem: &'problem SolveProblem,
    seed: usize,
    states: usize,
    phases: Vec<(Phase, ScalarProgramBlock)>,
}

/// The issued workspace extents, with the schedule they were issued for.
#[derive(Debug, Clone)]
pub struct SolveTangentWorkspace<'problem> {
    implicit_matrix_len: usize,
    implicit_pivot_len: usize,
    scratch_len: usize,
    private_len: usize,
    work_len: usize,
    schedule: SolveTangentSchedule<'problem>,
}

/// Why a tangent schedule or workspace is refused.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveTangentWorkspaceError {
    /// The seed domain of the problem overflows.
    SeedDomain,
    /// An assignment segment's outputs are not exactly its targets in order.
    SegmentCoverage,
    /// A phase was added after a phase that must follow it, or twice.
    PhaseOrder,
    ImplicitMatrix,
    ImplicitPivot,
    ImplicitPrivate,
    ConsistencyPrivate,
    Workspace,
    /// The workspace was issued for another schedule.
    ForeignSchedule,
}

impl std::fmt::Display for SolveTangentWorkspaceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::SeedDomain => "tangent seed extent overflow",
            Self::SegmentCoverage => {
                "tangent segment does not cover its simultaneous target inventory"
            }
            Self::PhaseOrder => "tangent phases are not in their sequential order",
            Self::ImplicitMatrix => "implicit matrix extent overflow",
            Self::ImplicitPivot => "implicit pivot extent overflow",
            Self::ImplicitPrivate => "implicit workspace extent overflow",
            Self::ConsistencyPrivate => "consistency workspace extent overflow",
            Self::Workspace => "tangent workspace extent overflow",
            Self::ForeignSchedule => "tangent workspace was issued for another schedule",
        })
    }
}

impl std::error::Error for SolveTangentWorkspaceError {}

impl Phase {
    /// Phases run in this order; equal ranks may repeat only for segments.
    const fn rank(&self) -> u8 {
        match self {
            Self::Rhs => 0,
            Self::Assignment { .. } => 1,
            Self::Implicit { .. } => 2,
            Self::Consistency { .. } => 3,
        }
    }

    fn same(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Rhs, Self::Rhs) => true,
            (Self::Assignment { targets: a }, Self::Assignment { targets: b }) => a == b,
            (Self::Implicit { unknowns: a }, Self::Implicit { unknowns: b }) => a == b,
            (Self::Consistency { rows: a }, Self::Consistency { rows: b }) => a == b,
            _ => false,
        }
    }
}

impl<'problem> SolveTangentSchedule<'problem> {
    /// Elements of `problem`'s tangent seed domain: one direction per `y`
    /// and `p` scalar, and one for time.
    pub fn seed_len(problem: &SolveProblem) -> Result<usize, SolveTangentWorkspaceError> {
        problem
            .layout
            .y_scalars()
            .checked_add(problem.layout.p_scalars())
            .and_then(|length| length.checked_add(1))
            .ok_or(SolveTangentWorkspaceError::SeedDomain)
    }

    /// A schedule of `problem` whose first phase is the right-hand side
    /// `rhs`; its seed and state columns are the problem's own.
    pub fn new(
        problem: &'problem SolveProblem,
        rhs: &ScalarProgramBlock,
    ) -> Result<Self, SolveTangentWorkspaceError> {
        Ok(Self {
            problem,
            seed: Self::seed_len(problem)?,
            states: problem.solve_layout.state_scalar_count(),
            phases: vec![(Phase::Rhs, rhs.clone())],
        })
    }

    /// Add a simultaneous assignment segment of `targets` targets, whose
    /// program writes exactly its targets in order.
    pub fn assignment(
        self,
        program: &ScalarProgramBlock,
        targets: usize,
    ) -> Result<Self, SolveTangentWorkspaceError> {
        if program.output_indices().iter().copied().ne(0..targets) {
            return Err(SolveTangentWorkspaceError::SegmentCoverage);
        }
        self.push(Phase::Assignment { targets }, program)
    }

    /// Add the implicit solve of `unknowns` simultaneous targets.
    pub fn implicit(
        self,
        program: &ScalarProgramBlock,
        unknowns: usize,
    ) -> Result<Self, SolveTangentWorkspaceError> {
        self.push(Phase::Implicit { unknowns }, program)
    }

    /// Add the consistency check of `rows` checked rows.
    pub fn consistency(
        self,
        program: &ScalarProgramBlock,
        rows: usize,
    ) -> Result<Self, SolveTangentWorkspaceError> {
        self.push(Phase::Consistency { rows }, program)
    }

    fn push(
        mut self,
        phase: Phase,
        program: &ScalarProgramBlock,
    ) -> Result<Self, SolveTangentWorkspaceError> {
        let last = self.phases.last().map_or(0, |(last, _)| last.rank());
        let repeats = matches!(phase, Phase::Assignment { .. });
        if phase.rank() < last || (phase.rank() == last && !repeats) {
            return Err(SolveTangentWorkspaceError::PhaseOrder);
        }
        self.phases.push((phase, program.clone()));
        Ok(self)
    }

    /// Whether `other` schedules the same problem, phases and programs.
    fn same(&self, other: &Self) -> bool {
        std::ptr::eq(self.problem, other.problem)
            && self.phases.len() == other.phases.len()
            && self
                .phases
                .iter()
                .zip(&other.phases)
                .all(|((a, pa), (b, pb))| a.same(b) && pa.shares_program_owner(pb))
    }

    /// Issue the workspace this schedule needs.
    pub fn issue(self) -> Result<SolveTangentWorkspace<'problem>, SolveTangentWorkspaceError> {
        let (mut scratch_len, mut private_len) = (0usize, 0usize);
        let (mut implicit_matrix_len, mut implicit_pivot_len) = (0, 0);
        for (phase, program) in &self.phases {
            scratch_len = scratch_len.max(program.output_count());
            match *phase {
                Phase::Rhs => {}
                Phase::Assignment { targets } => scratch_len = scratch_len.max(targets),
                Phase::Implicit { unknowns: n } => {
                    implicit_pivot_len = n
                        .checked_mul(2)
                        .ok_or(SolveTangentWorkspaceError::ImplicitPivot)?;
                    implicit_matrix_len = n
                        .checked_mul(n)
                        .ok_or(SolveTangentWorkspaceError::ImplicitMatrix)?;
                    let own = self
                        .seed
                        .checked_add(implicit_matrix_len)
                        .and_then(|length| length.checked_add(n))
                        .ok_or(SolveTangentWorkspaceError::ImplicitPrivate)?;
                    private_len = private_len.max(own);
                }
                Phase::Consistency { rows } => {
                    let own = self
                        .seed
                        .checked_add(rows)
                        .ok_or(SolveTangentWorkspaceError::ConsistencyPrivate)?;
                    private_len = private_len.max(own);
                }
            }
        }
        let work_len = self
            .seed
            .checked_add(self.states)
            .and_then(|length| length.checked_add(scratch_len))
            .and_then(|length| length.checked_add(private_len))
            .ok_or(SolveTangentWorkspaceError::Workspace)?;
        Ok(SolveTangentWorkspace {
            implicit_matrix_len,
            implicit_pivot_len,
            scratch_len,
            private_len,
            work_len,
            schedule: self,
        })
    }
}

impl<'problem> SolveTangentWorkspace<'problem> {
    /// This workspace, when it was issued for exactly `schedule`.
    pub fn for_schedule(
        &self,
        schedule: &SolveTangentSchedule<'_>,
    ) -> Result<&Self, SolveTangentWorkspaceError> {
        if self.schedule.same(schedule) {
            Ok(self)
        } else {
            Err(SolveTangentWorkspaceError::ForeignSchedule)
        }
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
mod tests;
