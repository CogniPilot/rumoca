//! Stable counters for compact-family and tensor-preservation regression gates.

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::LowerError;
use crate::tensor_declines::{
    LoweredFamily, TensorDeclineJournal, TensorFallbackCount, TensorFallbackReason, fallback_counts,
};

/// Where the measured family list came from, and whether it is provably the one
/// Solve lowering consumed.
///
/// `family_index` attribution is positional, so a report built from a DAE other
/// than the lowered one is wrong in every index as well as in its totals. This
/// field is the report's own statement about which case it is.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum TensorReportProvenance {
    /// The families measured are the ones the decline journal recorded at the
    /// lowering site -- either taken from it directly, or a caller-supplied DAE
    /// verified positionally identical to it.
    LoweredDae,
    /// The caller supplied a DAE and no journal recorded what lowering actually
    /// consumed, so the report cannot prove the two agree. Totals and
    /// `family_index` values are only meaningful if the caller passed the DAE
    /// that produced this `SolveProblem`.
    #[default]
    UnverifiedDae,
}

impl TensorReportProvenance {
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::LoweredDae => "lowered-dae",
            Self::UnverifiedDae => "unverified-dae",
        }
    }

    /// Whether the measurement is provably about the lowered system.
    #[must_use]
    pub const fn is_verified(self) -> bool {
        matches!(self, Self::LoweredDae)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TensorFallback {
    pub family_index: usize,
    pub reason: TensorFallbackReason,
    pub span: rumoca_core::Span,
    pub compact_domain_points: usize,
    /// Canonical family bodies that were scalarized (`equations_per_point`
    /// minus the bodies a tensor node covered).
    pub scalarized_bodies: usize,
    pub scalarized_rows: usize,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct TensorPreservationReport {
    pub compact_family_count: usize,
    pub compact_domain_points: usize,
    pub structured_scalar_view_rows: usize,
    pub peak_family_scalar_view_rows: usize,
    pub structural_equation_rows: usize,
    pub solve_node_counts: solve::ComputeNodeCounts,
    pub preserved_family_bodies: usize,
    pub scalarized_family_bodies: usize,
    pub scalarized_family_rows: usize,
    pub fallbacks: Vec<TensorFallback>,
    /// Whether the measured families are provably the ones lowering consumed.
    pub provenance: TensorReportProvenance,
}

impl TensorPreservationReport {
    /// Percentage of canonical structured-family bodies retained as native
    /// tensor nodes. `None` means the model contains no structured families
    /// and therefore has no meaningful tensor-preservation denominator.
    #[must_use]
    pub fn preservation_percent(&self) -> Option<f64> {
        let total = self
            .preserved_family_bodies
            .checked_add(self.scalarized_family_bodies)?;
        (total != 0).then(|| 100.0 * self.preserved_family_bodies as f64 / total as f64)
    }

    /// Per-reason totals in stable code order. This is what turns "95% of
    /// eligible families scalarized" into a per-cause breakdown an aggregator
    /// can ratchet on.
    #[must_use]
    pub fn fallback_counts(&self) -> Vec<TensorFallbackCount> {
        fallback_counts(&self.fallbacks)
    }
}

/// Report compact ownership and tensor preservation without materializing a
/// scalar view. Tests can apply model-specific budgets to these stable counters.
///
/// This entry point has no decline journal, so it is weaker in two ways at
/// once, and the returned report says so via
/// [`TensorReportProvenance::UnverifiedDae`]:
///
/// * every scalarized family is reported as
///   [`TensorFallbackReason::IncompleteTensorCoverage`] -- the unattributable
///   residue, because no branch reason was carried out of lowering; and
/// * nothing proves `dae_model` is the DAE that produced `problem`. Family
///   attribution is positional and the denominator is `dae_model`'s family
///   count, so a caller that hands in a DAE the lowering funnel later rewrote
///   gets a preservation percentage measured against a different system.
///
/// Callers that lowered through
/// [`crate::lower_solve_problem_with_tensor_declines`] should use
/// [`tensor_preservation_report_from_lowering`], which takes no DAE and cannot
/// be given the wrong one.
pub fn tensor_preservation_report(
    dae_model: &dae::Dae,
    problem: &solve::SolveProblem,
) -> Result<TensorPreservationReport, LowerError> {
    let families = dae_model
        .continuous
        .structured_equations
        .iter()
        .map(MeasuredFamily::from_dae_family)
        .collect::<Vec<_>>();
    report_families(
        ReportInputs {
            families: &families,
            structural_equation_rows: dae_model.continuous.equations.len(),
            declines: None,
            provenance: TensorReportProvenance::UnverifiedDae,
        },
        problem,
    )
}

/// Same measurement as [`tensor_preservation_report`], but each scalarized
/// family is attributed to the lowering branch that declined it.
///
/// `dae_model` must be the DAE that produced `problem`. When `declines` came
/// from a full lowering entry point it carries the family list lowering
/// consumed, and a `dae_model` that disagrees with it is rejected with a
/// spanned contract violation rather than measured against the wrong system --
/// positional `family_index` attribution makes a silent mismatch wrong in every
/// row, not just in the totals. Prefer
/// [`tensor_preservation_report_from_lowering`], where the mismatch cannot be
/// expressed at all.
pub fn tensor_preservation_report_with_declines(
    dae_model: &dae::Dae,
    problem: &solve::SolveProblem,
    declines: &TensorDeclineJournal,
) -> Result<TensorPreservationReport, LowerError> {
    let provenance = verify_lowered_provenance(dae_model, declines)?;
    let families = dae_model
        .continuous
        .structured_equations
        .iter()
        .map(MeasuredFamily::from_dae_family)
        .collect::<Vec<_>>();
    report_families(
        ReportInputs {
            families: &families,
            structural_equation_rows: dae_model.continuous.equations.len(),
            declines: Some(declines),
            provenance,
        },
        problem,
    )
}

/// The attributed measurement, taken against the families the journal recorded
/// at the lowering site.
///
/// This entry point takes no DAE, so there is no second DAE to disagree with
/// the lowered one: the denominator and every `family_index` are the lowering's
/// own. `declines` must come from [`crate::lower_solve_problem_with_tensor_declines`]
/// (or another full lowering entry point); a journal with no recorded family
/// list cannot be measured and is rejected with a contract violation.
pub fn tensor_preservation_report_from_lowering(
    problem: &solve::SolveProblem,
    declines: &TensorDeclineJournal,
) -> Result<TensorPreservationReport, LowerError> {
    let Some(lowered) = declines.lowered_continuous() else {
        return Err(LowerError::UnspannedContractViolation {
            reason: "tensor preservation report requires a decline journal that recorded the \
                     lowered continuous families"
                .to_string(),
        });
    };
    let families = lowered
        .families()
        .iter()
        .map(MeasuredFamily::from_lowered_family)
        .collect::<Vec<_>>();
    report_families(
        ReportInputs {
            families: &families,
            structural_equation_rows: lowered.structural_equation_rows(),
            declines: Some(declines),
            provenance: TensorReportProvenance::LoweredDae,
        },
        problem,
    )
}

/// Reject a caller-supplied DAE that is not the one lowering consumed.
fn verify_lowered_provenance(
    dae_model: &dae::Dae,
    declines: &TensorDeclineJournal,
) -> Result<TensorReportProvenance, LowerError> {
    let families = &dae_model.continuous.structured_equations;
    let Some(lowered) = declines.lowered_continuous() else {
        return Ok(TensorReportProvenance::UnverifiedDae);
    };
    let Some(mismatch) = lowered.first_mismatch(dae_model) else {
        return Ok(TensorReportProvenance::LoweredDae);
    };
    let reason = format!(
        "tensor preservation report was given a DAE with {} structured families over {} \
         equation rows, but Solve lowering consumed {} over {} -- they first differ at family \
         index {mismatch}, so every positional family attribution would name the wrong family",
        families.len(),
        dae_model.continuous.equations.len(),
        lowered.families().len(),
        lowered.structural_equation_rows()
    );
    let span = families
        .get(mismatch)
        .map(|family| family.span)
        .or_else(|| lowered.families().get(mismatch).map(|family| family.span))
        .or_else(|| dae_model.continuous.equations.first().map(|eq| eq.span));
    Err(match span {
        Some(span) => tensor_report_contract_error(reason, span),
        None => LowerError::UnspannedContractViolation { reason },
    })
}

/// One family reduced to what the report measures, so the same code path serves
/// a caller-supplied DAE and the journal's recorded family list.
struct MeasuredFamily<'a> {
    span: rumoca_core::Span,
    domain: &'a rumoca_core::StructuredIndexDomain,
    equations_per_point: usize,
}

impl<'a> MeasuredFamily<'a> {
    fn from_dae_family(family: &'a dae::StructuredEquationFamily) -> Self {
        Self {
            span: family.span,
            domain: &family.domain,
            equations_per_point: family.equations_per_point,
        }
    }

    fn from_lowered_family(family: &'a LoweredFamily) -> Self {
        Self {
            span: family.span,
            domain: &family.domain,
            equations_per_point: family.equations_per_point,
        }
    }
}

struct ReportInputs<'a> {
    families: &'a [MeasuredFamily<'a>],
    structural_equation_rows: usize,
    declines: Option<&'a TensorDeclineJournal>,
    provenance: TensorReportProvenance,
}

fn report_families(
    inputs: ReportInputs<'_>,
    problem: &solve::SolveProblem,
) -> Result<TensorPreservationReport, LowerError> {
    let tensor_nodes = problem
        .continuous
        .derivative_rhs
        .nodes
        .iter()
        .chain(&problem.continuous.residual.nodes);
    let tensor_nodes = tensor_nodes.collect::<Vec<_>>();
    let mut report = TensorPreservationReport {
        compact_family_count: inputs.families.len(),
        structural_equation_rows: inputs.structural_equation_rows,
        solve_node_counts: problem.compute_node_counts(),
        provenance: inputs.provenance,
        ..TensorPreservationReport::default()
    };
    // A tensor node is the native implementation of one canonical family
    // body, over the whole family domain or over a sub-domain of it. Families
    // originating at the same source span can share an equal domain, so
    // coverage must consume nodes instead of counting the same node once for
    // every such family.
    let mut available_tensor_nodes = vec![true; tensor_nodes.len()];
    for (family_index, family) in inputs.families.iter().enumerate() {
        record_family(
            &mut report,
            FamilyCoverage {
                family_index,
                family,
                declines: inputs.declines,
            },
            &tensor_nodes,
            &mut available_tensor_nodes,
        )?;
    }
    Ok(report)
}

/// One family's identity plus the attribution source used when it scalarizes.
struct FamilyCoverage<'a> {
    family_index: usize,
    family: &'a MeasuredFamily<'a>,
    declines: Option<&'a TensorDeclineJournal>,
}

impl FamilyCoverage<'_> {
    /// The code-derived reason this family scalarized. Without a journal the
    /// only honest answer is the unattributable residue.
    fn reason(&self) -> TensorFallbackReason {
        self.declines
            .map_or(TensorFallbackReason::IncompleteTensorCoverage, |journal| {
                journal.reason_for_family(self.family_index)
            })
    }
}

fn record_family(
    report: &mut TensorPreservationReport,
    coverage: FamilyCoverage<'_>,
    tensor_nodes: &[&solve::ComputeNode],
    available_tensor_nodes: &mut [bool],
) -> Result<(), LowerError> {
    let family = coverage.family;
    let points = family.domain.scalar_count().map_err(|error| {
        tensor_report_contract_error(
            format!("structured index domain is invalid: {error}"),
            family.span,
        )
    })?;
    let rows = points
        .checked_mul(family.equations_per_point)
        .ok_or_else(|| {
            tensor_report_contract_error("structured family row count overflows", family.span)
        })?;
    report.compact_domain_points = report
        .compact_domain_points
        .checked_add(points)
        .ok_or_else(|| {
            tensor_report_contract_error("compact domain point count overflows", family.span)
        })?;
    report.structured_scalar_view_rows = report
        .structured_scalar_view_rows
        .checked_add(rows)
        .ok_or_else(|| {
            tensor_report_contract_error("structured scalar-view row count overflows", family.span)
        })?;
    report.peak_family_scalar_view_rows = report.peak_family_scalar_view_rows.max(rows);
    if points == 0 {
        // An empty index domain expands to no rows at all, so it neither
        // preserves nor scalarizes a body. Counting it either way would move
        // the KPI for a family that carries no work.
        return Ok(());
    }

    let preserved_bodies =
        preserved_family_bodies(family, points, tensor_nodes, available_tensor_nodes)?;
    report.preserved_family_bodies = report
        .preserved_family_bodies
        .checked_add(preserved_bodies)
        .ok_or_else(|| {
            tensor_report_contract_error("preserved family body count overflows", family.span)
        })?;
    let missing_bodies = family.equations_per_point - preserved_bodies;
    if missing_bodies == 0 {
        return Ok(());
    }
    let scalarized_rows = points.checked_mul(missing_bodies).ok_or_else(|| {
        tensor_report_contract_error("scalarized family row count overflows", family.span)
    })?;
    report.scalarized_family_bodies = report
        .scalarized_family_bodies
        .checked_add(missing_bodies)
        .ok_or_else(|| {
            tensor_report_contract_error("scalarized family body count overflows", family.span)
        })?;
    report.scalarized_family_rows = report
        .scalarized_family_rows
        .checked_add(scalarized_rows)
        .ok_or_else(|| {
            tensor_report_contract_error("total scalarized family row count overflows", family.span)
        })?;
    report.fallbacks.push(TensorFallback {
        family_index: coverage.family_index,
        reason: coverage.reason(),
        span: family.span,
        compact_domain_points: points,
        scalarized_bodies: missing_bodies,
        scalarized_rows,
    });
    Ok(())
}

/// Canonical family bodies this family kept as native tensor nodes.
///
/// A body is preserved when tensor nodes cover the family's whole index domain,
/// which the stencil can do with ONE node over the full domain or with several
/// over disjoint sub-domains (the shrinking-prefix search splits a family whose
/// boundary iteration is not affine). Counting only exact-domain nodes reported
/// a fully preserved split family as scalarized, so coverage is measured in
/// domain points and floored to whole bodies -- a partially covered body is
/// never claimed as preserved.
fn preserved_family_bodies(
    family: &MeasuredFamily<'_>,
    points: usize,
    tensor_nodes: &[&solve::ComputeNode],
    available_tensor_nodes: &mut [bool],
) -> Result<usize, LowerError> {
    let capacity = points
        .checked_mul(family.equations_per_point)
        .ok_or_else(|| {
            tensor_report_contract_error("structured family row count overflows", family.span)
        })?;
    let mut covered_points = 0usize;
    for (index, node) in tensor_nodes.iter().enumerate() {
        if covered_points >= capacity {
            break;
        }
        if !available_tensor_nodes[index] {
            continue;
        }
        let Some(node_points) = tensor_node_family_coverage(node, family)? else {
            continue;
        };
        available_tensor_nodes[index] = false;
        covered_points = covered_points.saturating_add(node_points);
    }
    Ok((covered_points / points).min(family.equations_per_point))
}

/// Domain points of `node` that belong to `family`, or `None` when the node is
/// not one of the family's bodies. Sub-domain nodes count: the stencil stamps
/// every node it builds for a family with that family's span, and the compact
/// sub-domains it derives are always contained in the family domain.
fn tensor_node_family_coverage(
    node: &solve::ComputeNode,
    family: &MeasuredFamily<'_>,
) -> Result<Option<usize>, LowerError> {
    let (domain, span) = match node {
        solve::ComputeNode::Map { domain, span, .. }
        | solve::ComputeNode::AffineStencil { domain, span, .. } => (domain, span),
        solve::ComputeNode::ScalarPrograms(_)
        | solve::ComputeNode::MatMul { .. }
        | solve::ComputeNode::LinSolve { .. } => return Ok(None),
    };
    if *span != family.span || !domain_within_family(domain, family.domain) {
        return Ok(None);
    }
    let points = domain.scalar_count().map_err(|error| {
        tensor_report_contract_error(
            format!("tensor node index domain is invalid: {error}"),
            *span,
        )
    })?;
    Ok(Some(points))
}

/// Whether every index tuple of `domain` is also an index tuple of `family`.
///
/// Both domains are cross products of per-binder arithmetic sequences, so
/// containment holds exactly when it holds dimension by dimension.
fn domain_within_family(
    domain: &rumoca_core::StructuredIndexDomain,
    family_domain: &rumoca_core::StructuredIndexDomain,
) -> bool {
    domain.binders.len() == family_domain.binders.len()
        && domain
            .binders
            .iter()
            .zip(&family_domain.binders)
            .all(|(binder, family_binder)| binder_within(binder, family_binder))
}

fn binder_within(
    binder: &rumoca_core::StructuredIndexBinder,
    family_binder: &rumoca_core::StructuredIndexBinder,
) -> bool {
    if binder.id != family_binder.id || binder.step == 0 || family_binder.step == 0 {
        return false;
    }
    let Some(remainder) = binder.step.checked_rem(family_binder.step) else {
        return false;
    };
    remainder == 0
        && binder_holds_value(family_binder, binder.lower)
        && binder_holds_value(family_binder, binder.upper)
}

fn binder_holds_value(binder: &rumoca_core::StructuredIndexBinder, value: i64) -> bool {
    let Some(offset) = value.checked_sub(binder.lower) else {
        return false;
    };
    let Some(remainder) = offset.checked_rem(binder.step) else {
        return false;
    };
    if remainder != 0 {
        return false;
    }
    if binder.step > 0 {
        binder.lower <= value && value <= binder.upper
    } else {
        binder.upper <= value && value <= binder.lower
    }
}

fn tensor_report_contract_error(reason: impl Into<String>, span: rumoca_core::Span) -> LowerError {
    if span.is_dummy() {
        LowerError::UnspannedContractViolation {
            reason: reason.into(),
        }
    } else {
        LowerError::ContractViolation {
            reason: reason.into(),
            span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn family(points: i64, equations_per_point: usize) -> dae::StructuredEquationFamily {
        dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: points,
                    step: 1,
                }],
            },
            first_equation_index: 0,
            equations_per_point,
            span: rumoca_core::Span::DUMMY,
            origin: "report test".to_string(),
            regular: None,
            template: None,
            interiors_materialized: true,
        }
    }

    #[test]
    fn report_counts_large_fallback_without_materializing_domain() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .continuous
            .structured_equations
            .push(family(1_000_000, 2));

        let report = tensor_preservation_report(&dae_model, &solve::SolveProblem::default())
            .expect("compact report should not enumerate the family");

        assert_eq!(report.compact_family_count, 1);
        assert_eq!(report.compact_domain_points, 1_000_000);
        assert_eq!(report.structured_scalar_view_rows, 2_000_000);
        assert_eq!(report.peak_family_scalar_view_rows, 2_000_000);
        assert_eq!(report.scalarized_family_rows, 2_000_000);
        assert_eq!(report.preservation_percent(), Some(0.0));
        assert_eq!(
            report.fallbacks[0].reason.code(),
            "solve:incomplete-tensor-coverage"
        );
        assert_eq!(report.fallbacks[0].scalarized_bodies, 2);
    }

    #[test]
    fn family_never_seen_by_structured_lowering_reports_absent_rows() {
        let mut dae_model = dae::Dae::default();
        dae_model.continuous.structured_equations.push(family(4, 1));
        // A journal that saw a DIFFERENT family: this one's rows never reached
        // the stencil, which is a different fact from declining there.
        let mut declines = TensorDeclineJournal::new();
        declines.observe_family(7);

        let report = tensor_preservation_report_with_declines(
            &dae_model,
            &solve::SolveProblem::default(),
            &declines,
        )
        .expect("attributed report should inspect compact metadata");

        assert_eq!(
            report.fallbacks[0].reason.code(),
            "solve:family-rows-absent"
        );
        let counts = report.fallback_counts();
        assert_eq!(counts[0].families, 1);
        assert_eq!(counts[0].scalarized_rows, 4);
    }

    /// A family carrying a real span, so a provenance rejection can be shown to
    /// be spanned rather than a bare contract string.
    fn spanned_family(points: i64) -> dae::StructuredEquationFamily {
        dae::StructuredEquationFamily {
            span: rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name("tensor_report_fixture.mo"),
                1,
                2,
            ),
            ..family(points, 1)
        }
    }

    #[test]
    fn observed_family_with_no_recorded_decline_is_not_reported_absent() {
        let mut dae_model = dae::Dae::default();
        dae_model.continuous.structured_equations.push(family(4, 1));
        // Lowering SAW this family's rows and declined nothing for it. The
        // report has no cause to name, and "lowering never saw a candidate"
        // would be the opposite of the recorded fact.
        let mut declines = TensorDeclineJournal::new();
        declines.observe_family(0);

        let report = tensor_preservation_report_with_declines(
            &dae_model,
            &solve::SolveProblem::default(),
            &declines,
        )
        .expect("attributed report should inspect compact metadata");

        assert_eq!(
            report.fallbacks[0].reason.code(),
            "solve:unattributed-family-decline"
        );
    }

    #[test]
    fn attributed_report_rejects_a_dae_other_than_the_one_lowered() {
        let mut lowered_dae = dae::Dae::default();
        lowered_dae
            .continuous
            .structured_equations
            .extend([spanned_family(3), spanned_family(5)]);
        let mut declines = TensorDeclineJournal::new();
        declines.record_lowered_continuous(&lowered_dae);
        // The caller kept a handle to a DAE whose first family did not survive
        // into lowering. Family index 0 now names a different family, so every
        // positional attribution -- and the denominator -- would be wrong.
        let mut caller_dae = dae::Dae::default();
        caller_dae
            .continuous
            .structured_equations
            .push(spanned_family(5));

        let error = tensor_preservation_report_with_declines(
            &caller_dae,
            &solve::SolveProblem::default(),
            &declines,
        )
        .expect_err("a DAE that is not the lowered one must not be measured");

        assert!(error.source_span().is_some(), "{error:?}");
        assert!(error.reason().contains("structured families"), "{error:?}");
        let verified = tensor_preservation_report_with_declines(
            &lowered_dae,
            &solve::SolveProblem::default(),
            &declines,
        )
        .expect("the DAE lowering consumed verifies");
        assert_eq!(verified.provenance, TensorReportProvenance::LoweredDae);
        assert_eq!(verified.compact_family_count, 2);

        // Same family list, different equation block: the row count lowering
        // recorded identifies the DAE too, and `structural_equation_rows` is
        // reported straight from it.
        let mut regrown_dae = lowered_dae.clone();
        regrown_dae.continuous.equations.push(fixture_equation());
        let error = tensor_preservation_report_with_declines(
            &regrown_dae,
            &solve::SolveProblem::default(),
            &declines,
        )
        .expect_err("a DAE with a different equation block must not be measured");
        assert!(error.reason().contains("equation rows"), "{error:?}");
    }

    fn fixture_equation() -> dae::Equation {
        let span = spanned_family(3).span;
        dae::Equation::residual(
            rumoca_core::Expression::Empty { span },
            span,
            "tensor report provenance fixture",
        )
    }

    #[test]
    fn report_from_lowering_needs_no_dae_and_marks_its_provenance() {
        let mut lowered_dae = dae::Dae::default();
        lowered_dae
            .continuous
            .structured_equations
            .extend([spanned_family(3), spanned_family(5)]);
        lowered_dae.continuous.equations.push(fixture_equation());
        let mut declines = TensorDeclineJournal::new();
        declines.record_lowered_continuous(&lowered_dae);

        let report =
            tensor_preservation_report_from_lowering(&solve::SolveProblem::default(), &declines)
                .expect("the journal carries everything the measurement needs");

        assert_eq!(report.provenance, TensorReportProvenance::LoweredDae);
        assert_eq!(report.compact_family_count, 2);
        assert_eq!(report.structural_equation_rows, 1);
        assert_eq!(report.scalarized_family_bodies, 2);
        // A journal that never lowered anything cannot be measured at all,
        // rather than being measured against an assumed-empty family list.
        let error = tensor_preservation_report_from_lowering(
            &solve::SolveProblem::default(),
            &TensorDeclineJournal::new(),
        )
        .expect_err("an unrecorded journal has no families to measure");
        assert!(error.reason().contains("lowered continuous families"));
    }

    #[test]
    fn sub_domain_nodes_together_preserve_a_family_body() {
        let mut dae_model = dae::Dae::default();
        dae_model.continuous.structured_equations.push(family(4, 1));
        let nodes = [(1, 2), (3, 4)]
            .into_iter()
            .map(|(lower, upper)| sub_domain_map_node(lower, upper))
            .collect::<Vec<_>>();
        let problem = solve::SolveProblem::with_derivative_rhs(solve::ComputeBlock { nodes });

        let report = tensor_preservation_report(&dae_model, &problem)
            .expect("sub-domain coverage should remain well-defined");

        assert_eq!(report.preserved_family_bodies, 1);
        assert_eq!(report.scalarized_family_bodies, 0);
        assert!(report.fallbacks.is_empty());
        assert_eq!(report.provenance, TensorReportProvenance::UnverifiedDae);
    }

    #[test]
    fn a_node_outside_the_family_domain_never_counts_as_coverage() {
        let mut dae_model = dae::Dae::default();
        dae_model.continuous.structured_equations.push(family(4, 1));
        let problem = solve::SolveProblem::with_derivative_rhs(solve::ComputeBlock {
            // Indices 5..6 are not points of the 1..4 family domain.
            nodes: vec![sub_domain_map_node(5, 6)],
        });

        let report = tensor_preservation_report(&dae_model, &problem)
            .expect("out-of-domain nodes should remain well-defined");

        assert_eq!(report.preserved_family_bodies, 0);
        assert_eq!(report.scalarized_family_bodies, 1);
    }

    fn sub_domain_map_node(lower: i64, upper: i64) -> solve::ComputeNode {
        let domain = rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower,
                upper,
                step: 1,
            }],
        };
        solve::ComputeNode::Map {
            output_map: solve::TensorOutputMap::dense_contiguous(0, &domain)
                .expect("test domain has valid dense output strides"),
            domain,
            base_ops: vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: Vec::new(),
            const_strides: Vec::new(),
            metadata: solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn per_reason_counts_group_fallbacks_by_code() {
        let mut dae_model = dae::Dae::default();
        dae_model.continuous.structured_equations.extend([
            family(3, 1),
            family(5, 1),
            family(2, 1),
        ]);
        let mut declines = TensorDeclineJournal::new();
        declines.record(0, TensorFallbackReason::MismatchedDaeBodyShape);
        declines.record(1, TensorFallbackReason::MismatchedDaeBodyShape);
        declines.record(2, TensorFallbackReason::NonAffineOutputMap);

        let report = tensor_preservation_report_with_declines(
            &dae_model,
            &solve::SolveProblem::default(),
            &declines,
        )
        .expect("attributed report should inspect compact metadata");

        let counts = report
            .fallback_counts()
            .into_iter()
            .map(|count| (count.code(), count.families, count.scalarized_rows))
            .collect::<Vec<_>>();
        assert_eq!(
            counts,
            vec![
                ("solve:mismatched-dae-body-shape", 2, 8),
                ("solve:non-affine-output-map", 1, 2),
            ]
        );
    }

    #[test]
    fn preservation_percentage_is_body_weighted_and_domain_size_independent() {
        let report = TensorPreservationReport {
            preserved_family_bodies: 3,
            scalarized_family_bodies: 1,
            compact_domain_points: 1_000_000,
            ..TensorPreservationReport::default()
        };

        assert_eq!(report.preservation_percent(), Some(75.0));
        assert_eq!(
            TensorPreservationReport::default().preservation_percent(),
            None
        );
    }

    #[test]
    fn one_tensor_node_cannot_cover_two_equal_source_families() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .continuous
            .structured_equations
            .extend([family(3, 1), family(3, 1)]);
        let domain = dae_model.continuous.structured_equations[0].domain.clone();
        let problem = solve::SolveProblem::with_derivative_rhs(solve::ComputeBlock {
            nodes: vec![solve::ComputeNode::Map {
                output_map: solve::TensorOutputMap::dense_contiguous(0, &domain)
                    .expect("test domain has valid dense output strides"),
                domain,
                base_ops: vec![
                    solve::LinearOp::Const { dst: 0, value: 1.0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
                load_strides: Vec::new(),
                const_strides: Vec::new(),
                metadata: solve::TensorNodeMetadata::default(),
                span: rumoca_core::Span::DUMMY,
            }],
        });

        let report = tensor_preservation_report(&dae_model, &problem)
            .expect("equal family coverage should remain well-defined");
        assert_eq!(report.preserved_family_bodies, 1);
        assert_eq!(report.scalarized_family_bodies, 1);
        assert_eq!(report.preservation_percent(), Some(50.0));
    }
}
