//! Counts of the algebraic projection's fallback paths (SPEC_0044 ME-PROJ-003).
//!
//! Every path the projection takes when its preferred solve declines is
//! counted per canonical projection block on this thread: a torn solve that
//! declines to the dense block Newton, an affine elimination that declines to
//! the full-system solve, a projection-stage seed rescue, a staged refresh that
//! falls back to the complete simultaneous projection, a Newton step whose
//! Jacobian cannot serve it, and a sensitivity solve whose torn or sparse
//! factorization declines to the dense one. A run reports every block whose fallback rate
//! exceeds [`rumoca_eval_solve::projection_policy::PROJECTION_FALLBACK_REPORT_RATE`];
//! a fallback is never silent.

use std::cell::RefCell;
use std::collections::BTreeMap;

use rumoca_eval_solve::projection_policy::{
    PROJECTION_FALLBACK_REPORT_MIN_CALLS, PROJECTION_FALLBACK_REPORT_RATE,
};

/// A path the projection takes when its preferred solve declines.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ProjectionFallback {
    /// The torn solve declined and the dense block Newton ran.
    TornToDense,
    /// The affine elimination declined and the full-system solve ran.
    AffineFullSystem,
    /// A projection stage's seed was unavailable and its block projected from
    /// the incoming coordinate.
    SeedRescue,
    /// A staged refresh fell back to the complete simultaneous projection.
    CompletePlan,
    /// A Newton step's Jacobian could not serve it at the point.
    JacobianDeclined,
    /// A sensitivity solve's torn or sparse factorization declined and the
    /// block's dense factorization answered it.
    SeedDense,
}

impl ProjectionFallback {
    pub const ALL: [Self; 6] = [
        Self::TornToDense,
        Self::AffineFullSystem,
        Self::SeedRescue,
        Self::CompletePlan,
        Self::JacobianDeclined,
        Self::SeedDense,
    ];

    /// Stable name for reports and provenance.
    #[must_use]
    pub const fn label(self) -> &'static str {
        match self {
            Self::TornToDense => "torn_to_dense",
            Self::AffineFullSystem => "affine_full_system",
            Self::SeedRescue => "seed_rescue",
            Self::CompletePlan => "complete_plan",
            Self::JacobianDeclined => "jacobian_declined",
            Self::SeedDense => "seed_dense",
        }
    }

    const fn index(self) -> usize {
        self as usize
    }
}

/// Where a projection call or fallback happened: one canonical block, or the
/// complete plan of a staged refresh whose failing stage has no block.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ProjectionSite {
    Block(usize),
    CompletePlan,
}

/// Calls and fallbacks of one projection site.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ProjectionFallbackCounts {
    /// Rows of the block (zero for the complete plan).
    pub rows: usize,
    /// Projection calls (block solves, or staged refreshes for the plan).
    pub calls: u64,
    /// Calls that took at least one fallback; a fallback ahead of a call (a
    /// seed rescue before its block's projection) belongs to that call.
    pub fallback_calls: u64,
    /// Fallbacks by [`ProjectionFallback`] in declaration order.
    pub fallbacks: [u64; 6],
    /// Step halvings of the torn reduced Newton line search: trials at a
    /// step fraction below one that the sufficient-decrease test asked for.
    /// Not a fallback; it shows how often the full Newton step was refused.
    pub torn_step_halvings: u64,
}

impl ProjectionFallbackCounts {
    /// Fallbacks of every kind.
    #[must_use]
    pub fn total(&self) -> u64 {
        self.fallbacks.iter().sum()
    }

    /// The share of calls that fell back.
    #[must_use]
    pub fn rate(&self) -> f64 {
        if self.calls == 0 {
            return 0.0;
        }
        (self.fallback_calls as f64 / self.calls as f64).min(1.0)
    }

    /// Whether the rate exceeds the reporting threshold over at least the
    /// policy's minimum number of calls.
    #[must_use]
    pub fn over_threshold(&self) -> bool {
        self.calls >= PROJECTION_FALLBACK_REPORT_MIN_CALLS
            && self.rate() > PROJECTION_FALLBACK_REPORT_RATE
    }

    #[must_use]
    pub fn count(&self, fallback: ProjectionFallback) -> u64 {
        self.fallbacks[fallback.index()]
    }
}

/// The counts of every projection site on this thread, in site order.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ProjectionFallbackReport {
    pub sites: BTreeMap<ProjectionSite, ProjectionFallbackCounts>,
    /// Event iterations that ended on a relation surface (SPEC_0044
    /// ME-EVENT-008): each named root cycled between its sides, every side was
    /// a fixed point of the coordinate, and no joint mode of the cycling
    /// relations was consistent, so the current side was kept. Counted per
    /// root.
    pub relation_surfaces: BTreeMap<usize, u64>,
}

impl ProjectionFallbackReport {
    /// Relation settles kept on a coordinate surface, summed over roots.
    #[must_use]
    pub fn relation_surface_total(&self) -> u64 {
        self.relation_surfaces.values().sum()
    }

    /// The sites whose fallback rate exceeds the reporting threshold.
    pub fn over_threshold(
        &self,
    ) -> impl Iterator<Item = (ProjectionSite, &ProjectionFallbackCounts)> + '_ {
        self.sites
            .iter()
            .filter(|(_, counts)| counts.over_threshold())
            .map(|(site, counts)| (*site, counts))
    }

    /// One line per site over the threshold, for a run's warnings.
    #[must_use]
    pub fn warnings(&self) -> Vec<String> {
        let mut lines: Vec<String> = self
            .over_threshold()
            .map(|(site, counts)| {
                let breakdown = ProjectionFallback::ALL
                    .iter()
                    .filter(|fallback| counts.count(**fallback) > 0)
                    .map(|fallback| format!("{} {}", fallback.label(), counts.count(*fallback)))
                    .collect::<Vec<_>>()
                    .join(", ");
                let place = match site {
                    ProjectionSite::Block(block) => {
                        format!("projection block {block} ({} rows)", counts.rows)
                    }
                    ProjectionSite::CompletePlan => "the staged algebraic refresh".to_string(),
                };
                format!(
                    "{place} fell back on {:.1}% of {} calls ({breakdown})",
                    100.0 * counts.rate(),
                    counts.calls
                )
            })
            .collect();
        if !self.relation_surfaces.is_empty() {
            let roots = self
                .relation_surfaces
                .iter()
                .map(|(root, count)| format!("root {root} x{count}"))
                .collect::<Vec<_>>()
                .join(", ");
            lines.push(format!(
                "{} relation settle(s) kept a relation on its coordinate surface ({roots})",
                self.relation_surface_total()
            ));
        }
        lines
    }
}

thread_local! {
    static COUNTS: RefCell<BTreeMap<ProjectionSite, ProjectionFallbackCounts>> =
        const { RefCell::new(BTreeMap::new()) };
    /// The projection calls in progress, innermost last, each with whether it
    /// has fallen back.
    static ACTIVE: RefCell<Vec<(ProjectionSite, bool)>> = const { RefCell::new(Vec::new()) };
    /// Sites whose fallback came ahead of their next call (a seed rescue
    /// before its block's projection); that call has already fallen back.
    static PENDING: RefCell<Vec<ProjectionSite>> = const { RefCell::new(Vec::new()) };
    static SURFACES: RefCell<BTreeMap<usize, u64>> = const { RefCell::new(BTreeMap::new()) };
}

/// The projection fallback counts on this thread since the last
/// [`reset_projection_fallbacks`].
#[must_use]
pub fn projection_fallbacks() -> ProjectionFallbackReport {
    ProjectionFallbackReport {
        sites: COUNTS.with(|counts| counts.borrow().clone()),
        relation_surfaces: SURFACES.with(|surfaces| surfaces.borrow().clone()),
    }
}

pub fn reset_projection_fallbacks() {
    COUNTS.with(|counts| counts.borrow_mut().clear());
    ACTIVE.with(|active| active.borrow_mut().clear());
    PENDING.with(|pending| pending.borrow_mut().clear());
    SURFACES.with(|surfaces| surfaces.borrow_mut().clear());
}

/// One projection call at a site, counted when it begins; it ends when this
/// guard drops.
pub(crate) struct ProjectionCall(());

impl Drop for ProjectionCall {
    fn drop(&mut self) {
        ACTIVE.with(|active| active.borrow_mut().pop());
    }
}

/// Begin one projection call at `site`, a block of `rows` rows.
pub(crate) fn begin_call(site: ProjectionSite, rows: usize) -> ProjectionCall {
    COUNTS.with(|counts| {
        let mut counts = counts.borrow_mut();
        let entry = counts.entry(site).or_default();
        entry.rows = rows;
        entry.calls += 1;
    });
    let pending = PENDING.with(|pending| {
        let mut pending = pending.borrow_mut();
        let position = pending
            .iter()
            .position(|pending_site| *pending_site == site);
        position
            .map(|position| pending.swap_remove(position))
            .is_some()
    });
    ACTIVE.with(|active| active.borrow_mut().push((site, pending)));
    ProjectionCall(())
}

/// Count one fallback at `site`: once more for its kind, and once for the
/// call in progress at that site unless that call already fell back.
pub(crate) fn note_fallback(site: ProjectionSite, fallback: ProjectionFallback) {
    let new_call = ACTIVE.with(|active| {
        let mut active = active.borrow_mut();
        match active
            .iter_mut()
            .rev()
            .find(|(active_site, _)| *active_site == site)
        {
            Some((_, fell_back)) => !std::mem::replace(fell_back, true),
            None => PENDING.with(|pending| {
                let mut pending = pending.borrow_mut();
                let fresh = !pending.contains(&site);
                if fresh {
                    pending.push(site);
                }
                fresh
            }),
        }
    });
    COUNTS.with(|counts| {
        let mut counts = counts.borrow_mut();
        let entry = counts.entry(site).or_default();
        entry.fallbacks[fallback.index()] += 1;
        entry.fallback_calls += u64::from(new_call);
    });
}

/// Count one torn line-search step halving at the innermost call in progress.
pub(crate) fn note_torn_step_halving() {
    let Some(site) = ACTIVE.with(|active| active.borrow().last().map(|(site, _)| *site)) else {
        return;
    };
    COUNTS.with(|counts| {
        counts
            .borrow_mut()
            .entry(site)
            .or_default()
            .torn_step_halvings += 1;
    });
}

/// Count one event iteration kept on the relation surface of `roots`.
pub(crate) fn note_relation_surface(roots: &[usize]) {
    SURFACES.with(|surfaces| {
        let mut surfaces = surfaces.borrow_mut();
        for root in roots {
            *surfaces.entry(*root).or_default() += 1;
        }
    });
}

/// Single-program sharings whose shared-value proof failed in this process;
/// each ran its program unshared (SPEC_0043 shared-value segments).
#[must_use]
pub fn shared_value_proof_failures() -> u64 {
    rumoca_ir_solve::shared_value_proof_failures()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_shared_value_proof_failure_count_is_the_ir_solve_counter() {
        assert_eq!(
            shared_value_proof_failures(),
            rumoca_ir_solve::shared_value_proof_failures()
        );
    }

    #[test]
    fn relation_surface_settles_are_counted_per_root_and_reported() {
        reset_projection_fallbacks();
        note_relation_surface(&[4, 9]);
        note_relation_surface(&[4]);
        let report = projection_fallbacks();
        assert_eq!(report.relation_surfaces.get(&4), Some(&2));
        assert_eq!(report.relation_surface_total(), 3);
        assert!(
            report
                .warnings()
                .iter()
                .any(|line| line.contains("coordinate surface (root 4 x2, root 9 x1)"))
        );
        reset_projection_fallbacks();
        assert_eq!(projection_fallbacks().relation_surface_total(), 0);
    }

    #[test]
    fn a_call_counts_once_however_many_fallbacks_it_takes() {
        reset_projection_fallbacks();
        let site = ProjectionSite::Block(7);
        for call in 0..40 {
            let _call = begin_call(site, 3);
            if call == 0 {
                note_fallback(site, ProjectionFallback::TornToDense);
                note_fallback(site, ProjectionFallback::JacobianDeclined);
            }
        }
        let report = projection_fallbacks();
        let counts = report.sites[&site];
        assert_eq!(
            (counts.rows, counts.calls, counts.fallback_calls),
            (3, 40, 1)
        );
        assert_eq!(counts.count(ProjectionFallback::TornToDense), 1);
        assert_eq!(counts.count(ProjectionFallback::JacobianDeclined), 1);
        assert!(!counts.over_threshold(), "1 of 40 calls is below the rate");
        assert!(report.warnings().is_empty());
    }

    #[test]
    fn a_fallback_ahead_of_a_call_belongs_to_that_call() {
        reset_projection_fallbacks();
        let site = ProjectionSite::Block(2);
        note_fallback(site, ProjectionFallback::SeedRescue);
        {
            let _call = begin_call(site, 4);
            note_fallback(site, ProjectionFallback::CompletePlan);
        }
        let counts = projection_fallbacks().sites[&site];
        assert_eq!((counts.calls, counts.fallback_calls), (1, 1));
        assert_eq!(counts.total(), 2);
    }

    #[test]
    fn a_site_over_the_rate_is_warned_by_name() {
        reset_projection_fallbacks();
        let site = ProjectionSite::Block(11);
        for _ in 0..20 {
            let _call = begin_call(site, 5);
            note_fallback(site, ProjectionFallback::AffineFullSystem);
        }
        {
            let _plan = begin_call(ProjectionSite::CompletePlan, 0);
        }
        let report = projection_fallbacks();
        assert_eq!(report.over_threshold().count(), 1);
        assert_eq!(
            report.warnings(),
            [
                "projection block 11 (5 rows) fell back on 100.0% of 20 calls (affine_full_system 20)"
            ]
        );
        reset_projection_fallbacks();
        assert!(projection_fallbacks().sites.is_empty());
    }
}
