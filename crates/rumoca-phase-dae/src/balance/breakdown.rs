//! Diagnostic-only breakdown of the DAE balance arithmetic.
//!
//! Everything in this module is *observational*: it records how the clamps in
//! [`equations_unknowns_and_clamps`] behaved and which continuous equation rows
//! were filtered out of the `f_x` count, so an unbalanced model can be triaged
//! without re-deriving the arithmetic by hand. None of these values feed back
//! into the balance verdict — [`BalanceDetail::balance`] is byte-identical to
//! the pre-instrumentation formula.

use super::BalanceDetail;

/// How many rows each deficit/surplus clamp in the balance formula discarded.
///
/// A non-zero field means the raw component count was larger than the balance
/// arithmetic was willing to admit, i.e. the corresponding analysis
/// over-reported and is the first suspect when a model is unbalanced.
#[derive(
    Debug, Clone, Copy, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize, Hash,
)]
pub struct BalanceClamps {
    /// Index-erased aggregate candidate rows dropped by the deficit clamp.
    pub aggregate_candidates_dropped: usize,
    /// Interface flow equations dropped by the deficit clamp
    /// (suspect: `analysis::variable_analysis::count_interface_flows`).
    pub interface_flow_dropped: usize,
    /// Overconstrained-connector equations dropped by the deficit clamp
    /// (suspect: `analysis::variable_analysis::count_overconstrained_interface`).
    pub oc_interface_dropped: usize,
    /// Break-edge corrections dropped by the surplus clamp
    /// (suspect: `overconstrained_interface::break_edge` accounting).
    pub break_edge_dropped: usize,
}

impl BalanceClamps {
    /// True when no clamp discarded anything, i.e. every raw component count
    /// was admitted verbatim.
    pub fn is_inert(&self) -> bool {
        *self == Self::default()
    }

    /// Names of the clamps that discarded at least one row, in formula order.
    pub fn exercised(&self) -> Vec<&'static str> {
        let mut names = Vec::new();
        for (count, name) in [
            (self.aggregate_candidates_dropped, "aggregate_candidates"),
            (self.interface_flow_dropped, "interface_flow"),
            (self.oc_interface_dropped, "oc_interface"),
            (self.break_edge_dropped, "oc_break_edge"),
        ] {
            if count > 0 {
                names.push(name);
            }
        }
        names
    }
}

impl std::fmt::Display for BalanceClamps {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "agg_cand_dropped={} iflow_dropped={} oc_dropped={} brk_dropped={}",
            self.aggregate_candidates_dropped,
            self.interface_flow_dropped,
            self.oc_interface_dropped,
            self.break_edge_dropped
        )
    }
}

/// Why a continuous equation row was not counted toward `f_x`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BalanceExclusionReason {
    /// Connection alias whose target is already defined by a component equation.
    RedundantConnectionAlias,
    /// Connection equation that constrains no continuous unknown.
    ConnectionNoContinuousRef,
    /// `binding equation for ...` row that is an input-only alias.
    BindingInputAlias,
    /// Ordinary row that references neither a continuous unknown nor an input.
    NoContinuousOrInputRef,
}

/// Per-reason tally of continuous equation rows excluded from the strict `f_x`
/// count. Diagnostic only: never part of the balance arithmetic.
#[derive(
    Debug, Clone, Copy, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize, Hash,
)]
pub struct BalanceExclusionCounts {
    pub redundant_connection_alias: usize,
    pub connection_no_continuous_ref: usize,
    pub binding_input_alias: usize,
    pub no_continuous_or_input_ref: usize,
}

impl BalanceExclusionCounts {
    /// Total number of excluded equation objects (not scalars).
    pub fn total(&self) -> usize {
        self.redundant_connection_alias
            + self.connection_no_continuous_ref
            + self.binding_input_alias
            + self.no_continuous_or_input_ref
    }

    /// True when every continuous equation row was counted.
    pub fn is_inert(&self) -> bool {
        self.total() == 0
    }

    pub(crate) fn record(&mut self, reason: BalanceExclusionReason) {
        match reason {
            BalanceExclusionReason::RedundantConnectionAlias => {
                self.redundant_connection_alias += 1;
            }
            BalanceExclusionReason::ConnectionNoContinuousRef => {
                self.connection_no_continuous_ref += 1;
            }
            BalanceExclusionReason::BindingInputAlias => self.binding_input_alias += 1,
            BalanceExclusionReason::NoContinuousOrInputRef => {
                self.no_continuous_or_input_ref += 1;
            }
        }
    }
}

impl std::fmt::Display for BalanceExclusionCounts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "redundant_connection_alias={} connection_no_continuous_ref={} binding_input_alias={} no_continuous_or_input_ref={}",
            self.redundant_connection_alias,
            self.connection_no_continuous_ref,
            self.binding_input_alias,
            self.no_continuous_or_input_ref
        )
    }
}

/// Verdict for a single continuous equation row in the `f_x` count.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BalanceEquationVerdict {
    Counted,
    Excluded(BalanceExclusionReason),
}

/// A [`BalanceDetail`] wrapper with a single-line `Display`, suitable for
/// embedding in a diagnostic `help(...)` string.
///
/// `BalanceDetail`'s own `Display` is multi-line and is used by CLI reports;
/// error payloads need a one-line rendering.
#[derive(Debug, Clone)]
pub struct BalanceBreakdown(pub Box<BalanceDetail>);

impl BalanceBreakdown {
    pub fn into_detail(self) -> BalanceDetail {
        *self.0
    }
}

impl From<BalanceDetail> for BalanceBreakdown {
    fn from(detail: BalanceDetail) -> Self {
        Self(Box::new(detail))
    }
}

impl std::ops::Deref for BalanceBreakdown {
    type Target = BalanceDetail;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::fmt::Display for BalanceBreakdown {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let detail = &self.0;
        write!(
            f,
            "unknowns[states={} alg={} out={} discrete_real={} discrete_valued={}] \
             equations[f_x={} f_x_aggregate_candidate={} f_z={} f_m={} f_c={} algo={} when={} \
             iflow={} oc={} brk={}] clamps[{}] excluded[{}]",
            detail.state_unknowns,
            detail.alg_unknowns,
            detail.output_unknowns,
            detail.discrete_real_unknowns,
            detail.discrete_valued_unknowns,
            detail.f_x_scalar,
            detail.f_x_aggregate_candidate_scalar,
            detail.f_z_scalar,
            detail.f_m_scalar,
            detail.f_c_scalar,
            detail.algorithm_outputs,
            detail.when_eq_scalar,
            detail.interface_flow_count,
            detail.overconstrained_interface_count,
            detail.oc_break_edge_scalar_count,
            detail.clamps(),
            detail.excluded,
        )
    }
}

/// Balance arithmetic with clamp reporting.
///
/// This is the single implementation of the equation/unknown formula; the
/// public `equations_unknowns_from_detail`/`balance_from_detail` wrappers drop
/// the [`BalanceClamps`] so the verdict is unaffected by the instrumentation.
pub(crate) fn equations_unknowns_and_clamps(
    detail: &BalanceDetail,
) -> (usize, usize, BalanceClamps) {
    let unknowns = detail.raw_unknowns();
    let unknowns_i = unknowns as i64;
    let mut clamps = BalanceClamps::default();

    let base = (detail.f_x_scalar
        + detail.f_z_scalar
        + detail.f_m_scalar
        + detail.f_c_scalar
        + detail.algorithm_outputs
        + detail.when_eq_scalar) as i64;
    // Index-erased aggregate references identify a scalar family but do not
    // carry enough multiplicity information to prove an overdetermined
    // system. Admit those candidate rows only while they close a deficit;
    // exact and explicitly indexed references remain strict.
    let base = base
        + admit_up_to_deficit(
            detail.f_x_aggregate_candidate_scalar as i64,
            unknowns_i - base,
            &mut clamps.aggregate_candidates_dropped,
        );
    let base = base
        + admit_up_to_deficit(
            detail.interface_flow_count as i64,
            unknowns_i - base,
            &mut clamps.interface_flow_dropped,
        );
    let raw_equations = base
        + admit_up_to_deficit(
            detail.overconstrained_interface_count.max(0),
            unknowns_i - base,
            &mut clamps.oc_interface_dropped,
        );

    let brk = detail.oc_break_edge_scalar_count as i64;
    let effective_brk = brk.min((raw_equations - unknowns_i).max(0));
    clamps.break_edge_dropped = (brk - effective_brk).max(0) as usize;
    ((raw_equations - effective_brk) as usize, unknowns, clamps)
}

/// Admit at most `available` rows, and only while they close the remaining
/// `deficit`. Records the discarded remainder in `dropped`.
fn admit_up_to_deficit(available: i64, deficit: i64, dropped: &mut usize) -> i64 {
    let effective = available.min(deficit.max(0));
    *dropped = (available - effective).max(0) as usize;
    effective
}

/// Name the single largest contributor on the side of the balance gap.
///
/// A negative balance (under-determined) is dominated by the largest unknown
/// partition; a positive balance (over-determined) by the largest equation
/// partition. This is a triage hint that names the first place to look, not a
/// proof of the root cause.
pub(crate) fn dominant_balance_term(detail: &BalanceDetail) -> &'static str {
    let balance = detail.balance();
    if balance == 0 {
        return "balanced";
    }
    let candidates: &[(usize, &'static str)] = if balance < 0 {
        &[
            (detail.state_unknowns, "state_unknowns"),
            (detail.alg_unknowns, "alg_unknowns"),
            (detail.output_unknowns, "output_unknowns"),
            (detail.discrete_real_unknowns, "discrete_real_unknowns"),
            (detail.discrete_valued_unknowns, "discrete_valued_unknowns"),
        ]
    } else {
        &[
            (detail.f_x_scalar, "f_x"),
            (detail.f_z_scalar, "f_z"),
            (detail.f_m_scalar, "f_m"),
            (detail.f_c_scalar, "f_c"),
            (detail.interface_flow_count, "interface_flow"),
            (
                detail.overconstrained_interface_count.max(0) as usize,
                "oc_interface",
            ),
        ]
    };
    candidates
        .iter()
        .fold(("none", 0usize), |best, (count, name)| {
            if *count > best.1 {
                (*name, *count)
            } else {
                best
            }
        })
        .0
}

#[cfg(test)]
mod tests {
    use super::*;

    fn detail_with(f_x: usize, unknowns: usize) -> BalanceDetail {
        BalanceDetail {
            alg_unknowns: unknowns,
            f_x_scalar: f_x,
            ..BalanceDetail::default()
        }
    }

    #[test]
    fn clamps_are_inert_for_an_exactly_counted_system() {
        let detail = detail_with(4, 4);
        assert_eq!(detail.equations_unknowns(), (4, 4));
        assert_eq!(detail.balance(), 0);
        assert_eq!(detail.clamps(), BalanceClamps::default());
        assert!(detail.clamps().is_inert());
        assert!(detail.clamps().exercised().is_empty());
    }

    #[test]
    fn balance_clamps_report_interface_flow_deficit() {
        // 4 unknowns, 3 strict rows, 5 interface flow equations available:
        // only one may be admitted, the other four are surplus.
        let detail = BalanceDetail {
            interface_flow_count: 5,
            ..detail_with(3, 4)
        };
        assert_eq!(detail.equations_unknowns(), (4, 4));
        assert_eq!(detail.balance(), 0);
        assert_eq!(detail.clamps().interface_flow_dropped, 4);
        assert_eq!(detail.clamps().exercised(), vec!["interface_flow"]);
    }

    #[test]
    fn balance_clamps_report_aggregate_and_oc_surplus() {
        let detail = BalanceDetail {
            f_x_aggregate_candidate_scalar: 3,
            overconstrained_interface_count: 2,
            ..detail_with(2, 3)
        };
        // 2 strict rows + 1 aggregate candidate closes the deficit; the other
        // 2 aggregate rows and both oc rows are dropped.
        assert_eq!(detail.equations_unknowns(), (3, 3));
        assert_eq!(detail.clamps().aggregate_candidates_dropped, 2);
        assert_eq!(detail.clamps().oc_interface_dropped, 2);
        assert_eq!(
            detail.clamps().exercised(),
            vec!["aggregate_candidates", "oc_interface"]
        );
    }

    #[test]
    fn balance_clamps_report_break_edge_surplus() {
        // Overdetermined by 1; two break edges available but only one may be
        // applied against the surplus.
        let detail = BalanceDetail {
            oc_break_edge_scalar_count: 2,
            ..detail_with(4, 3)
        };
        assert_eq!(detail.equations_unknowns(), (3, 3));
        assert_eq!(detail.clamps().break_edge_dropped, 1);
        assert_eq!(detail.clamps().exercised(), vec!["oc_break_edge"]);
    }

    #[test]
    fn exclusion_counts_record_each_reason_once() {
        let mut counts = BalanceExclusionCounts::default();
        assert!(counts.is_inert());
        counts.record(BalanceExclusionReason::RedundantConnectionAlias);
        counts.record(BalanceExclusionReason::ConnectionNoContinuousRef);
        counts.record(BalanceExclusionReason::BindingInputAlias);
        counts.record(BalanceExclusionReason::NoContinuousOrInputRef);
        assert_eq!(counts.redundant_connection_alias, 1);
        assert_eq!(counts.connection_no_continuous_ref, 1);
        assert_eq!(counts.binding_input_alias, 1);
        assert_eq!(counts.no_continuous_or_input_ref, 1);
        assert_eq!(counts.total(), 4);
        assert!(!counts.is_inert());
    }

    #[test]
    fn dominant_term_names_the_larger_side_of_the_gap() {
        assert_eq!(detail_with(4, 4).dominant_term(), "balanced");
        // Under-determined: the largest unknown partition dominates.
        let under = BalanceDetail {
            state_unknowns: 2,
            ..detail_with(1, 5)
        };
        assert!(under.balance() < 0);
        assert_eq!(under.dominant_term(), "alg_unknowns");
        // Over-determined: the largest equation partition dominates.
        let over = BalanceDetail {
            f_z_scalar: 7,
            ..detail_with(2, 3)
        };
        assert!(over.balance() > 0);
        assert_eq!(over.dominant_term(), "f_z");
    }

    #[test]
    fn breakdown_display_is_single_line_and_names_components() {
        let breakdown = BalanceBreakdown::from(detail_with(3, 4));
        let rendered = breakdown.to_string();
        assert!(!rendered.contains('\n'), "help text must stay single-line");
        assert!(rendered.contains("f_x=3"), "{rendered}");
        assert!(rendered.contains("alg=4"), "{rendered}");
        assert!(rendered.contains("clamps["), "{rendered}");
        assert!(rendered.contains("excluded["), "{rendered}");
        assert_eq!(breakdown.balance(), -1);
    }
}
