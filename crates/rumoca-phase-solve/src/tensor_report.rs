//! Stable counters for compact-family and tensor-preservation regression gates.

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::LowerError;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TensorFallbackReason {
    /// A family did not produce enough full-domain Map/AffineStencil nodes to
    /// cover each of its canonical body equations.
    IncompleteTensorCoverage,
}

impl TensorFallbackReason {
    #[must_use]
    pub const fn code(self) -> &'static str {
        match self {
            Self::IncompleteTensorCoverage => "solve:incomplete-tensor-coverage",
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TensorFallback {
    pub family_index: usize,
    pub reason: TensorFallbackReason,
    pub span: rumoca_core::Span,
    pub compact_domain_points: usize,
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
}

/// Report compact ownership and tensor preservation without materializing a
/// scalar view. Tests can apply model-specific budgets to these stable counters.
pub fn tensor_preservation_report(
    dae_model: &dae::Dae,
    problem: &solve::SolveProblem,
) -> Result<TensorPreservationReport, LowerError> {
    let families = &dae_model.continuous.structured_equations;
    let tensor_nodes = problem
        .continuous
        .derivative_rhs
        .nodes
        .iter()
        .chain(&problem.continuous.residual.nodes);
    let tensor_nodes = tensor_nodes.collect::<Vec<_>>();
    let mut report = TensorPreservationReport {
        compact_family_count: families.len(),
        structural_equation_rows: dae_model.continuous.equations.len(),
        solve_node_counts: problem.compute_node_counts(),
        ..TensorPreservationReport::default()
    };
    for (family_index, family) in families.iter().enumerate() {
        record_family(&mut report, family_index, family, &tensor_nodes)?;
    }
    Ok(report)
}

fn record_family(
    report: &mut TensorPreservationReport,
    family_index: usize,
    family: &dae::StructuredEquationFamily,
    tensor_nodes: &[&solve::ComputeNode],
) -> Result<(), LowerError> {
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

    let preserved_bodies = tensor_nodes
        .iter()
        .filter(|node| tensor_node_covers_family(node, family))
        .count()
        .min(family.equations_per_point);
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
        family_index,
        reason: TensorFallbackReason::IncompleteTensorCoverage,
        span: family.span,
        compact_domain_points: points,
        scalarized_rows,
    });
    Ok(())
}

fn tensor_node_covers_family(
    node: &&solve::ComputeNode,
    family: &dae::StructuredEquationFamily,
) -> bool {
    match node {
        solve::ComputeNode::Map { domain, span, .. }
        | solve::ComputeNode::AffineStencil { domain, span, .. } => {
            *span == family.span && domain == &family.domain
        }
        solve::ComputeNode::ScalarPrograms(_)
        | solve::ComputeNode::MatMul { .. }
        | solve::ComputeNode::LinSolve { .. } => false,
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
        assert_eq!(
            report.fallbacks[0].reason.code(),
            "solve:incomplete-tensor-coverage"
        );
    }
}
