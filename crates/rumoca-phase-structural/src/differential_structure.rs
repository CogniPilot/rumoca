//! Source-bound differential signatures and checked structural offsets.
//!
//! This analysis establishes the graph obligations in SPEC_0007 / STRUCT-T07. It does
//! not establish differentiability or numerical rank and cannot authorize an
//! executable reduced system by itself.

mod assignment;
mod offsets;
#[cfg(test)]
mod tests;

use std::collections::BTreeMap;

use rumoca_core::Span;
use rumoca_eval_dae::{ScalarCoordinateProjectionCache, for_each_scalar_coordinate_cached};
use rumoca_ir_dae as dae;

use crate::incidence::projection::{ScalarResidual, projection_error, visit_owner_rows};
use crate::{EquationRef, SingularBlockWitness, StructuralError};

/// A derived scalar view of an unchanged source declaration.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DifferentialCoordinate<'dae> {
    variable: dae::VariableId<'dae>,
    scalar: u32,
}

impl<'dae> DifferentialCoordinate<'dae> {
    pub fn variable(self) -> dae::VariableId<'dae> {
        self.variable
    }

    pub fn scalar(self) -> u32 {
        self.scalar
    }
}

#[derive(Debug, Clone, Copy)]
struct SignatureEntry {
    column: usize,
    order: u32,
}

/// An immutable analysis view, issued only after primal/dual certification.
///
/// Scalar rows are ephemeral views of the issuing DAE, not serialized owners.
/// The formal dimension is conditional on a regular differentiated Jacobian;
/// it is not a numerical rank or a simulation capability certificate.
#[derive(Debug)]
pub struct DifferentialStructure<'dae> {
    variables: Vec<DifferentialCoordinate<'dae>>,
    rows: Vec<Vec<SignatureEntry>>,
    matching: Vec<usize>,
    equation_orders: Vec<u32>,
    variable_orders: Vec<u32>,
    formal_dimension: usize,
}

impl<'dae> DifferentialStructure<'dae> {
    /// Source coordinates in canonical variable order, then tensor scalar order.
    pub fn variables(&self) -> &[DifferentialCoordinate<'dae>] {
        &self.variables
    }

    /// Required differentiation orders in canonical continuous scalar-row order.
    pub fn equation_orders(&self) -> &[u32] {
        &self.equation_orders
    }

    /// Highest needed derivative order, aligned with [`Self::variables`].
    pub fn variable_orders(&self) -> &[u32] {
        &self.variable_orders
    }

    /// Assignment dimension, conditional on a regular differentiated Jacobian.
    pub fn formal_dimension(&self) -> usize {
        self.formal_dimension
    }

    pub fn matched_variable(&self, row: EquationRef) -> Option<DifferentialCoordinate<'dae>> {
        self.matching
            .get(row.0)
            .map(|&column| self.variables[column])
    }

    pub fn signature_row(
        &self,
        row: EquationRef,
    ) -> Option<impl Iterator<Item = (DifferentialCoordinate<'dae>, u32)> + '_> {
        self.rows.get(row.0).map(|entries| {
            entries
                .iter()
                .map(|entry| (self.variables[entry.column], entry.order))
        })
    }
}

/// Analyze derivative orders without rewriting or numerically evaluating a DAE.
pub fn analyze_differential_structure<'dae>(
    view: dae::DaeView<'dae>,
) -> Result<DifferentialStructure<'dae>, StructuralError> {
    let (variables, bases) = variable_columns(view)?;
    let mut rows = Vec::new();
    let mut spans = Vec::new();
    let mut cache = ScalarCoordinateProjectionCache::default();
    for owner in view.continuous_owners() {
        visit_owner_rows(view, owner, |row| {
            spans.push(row.provenance.span());
            rows.push(project_signature(view, row, &bases, &mut cache)?);
            Ok(())
        })?;
    }
    if rows.is_empty() && variables.is_empty() {
        return Err(StructuralError::EmptySystem);
    }
    let matching = assignment::maximum_weight_matching(&rows, variables.len())
        .map_err(|reason| contract(&spans, reason))?;
    let matching = require_square_matching(view, &variables, &rows, matching)?;
    let (equation_orders, variable_orders) =
        offsets::least_offsets(&rows, &matching).map_err(|reason| contract(&spans, reason))?;
    let formal_dimension =
        offsets::certify(&rows, &matching, &equation_orders, &variable_orders)
            .ok_or_else(|| contract(&spans, "differential assignment certificate is invalid"))?;
    Ok(DifferentialStructure {
        variables,
        rows,
        matching,
        equation_orders,
        variable_orders,
        formal_dimension,
    })
}

fn project_signature<'dae>(
    view: dae::DaeView<'dae>,
    row: ScalarResidual<'dae, '_>,
    bases: &[Option<usize>],
    cache: &mut ScalarCoordinateProjectionCache<'dae>,
) -> Result<Vec<SignatureEntry>, StructuralError> {
    let mut entries = BTreeMap::<usize, u32>::new();
    for_each_scalar_coordinate_cached(
        view,
        row.expression,
        row.scalar,
        row.domain_point,
        cache,
        |coordinate, scalar| {
            if let Some((column, order)) = coordinate_column(bases, coordinate, scalar) {
                let prior = entries.entry(column).or_default();
                *prior = (*prior).max(order);
            }
        },
    )
    .map_err(projection_error)?;
    Ok(entries
        .into_iter()
        .map(|(column, order)| SignatureEntry { column, order })
        .collect())
}

type VariableColumns<'dae> = (Vec<DifferentialCoordinate<'dae>>, Vec<Option<usize>>);

fn variable_columns(view: dae::DaeView<'_>) -> Result<VariableColumns<'_>, StructuralError> {
    let mut columns = Vec::new();
    let mut bases = vec![None; view.variable_count()];
    for (id, variable) in view.variables().filter(|(_, variable)| {
        matches!(
            variable.role(),
            dae::VariableRole::State | dae::VariableRole::Algebraic | dae::VariableRole::Output
        )
    }) {
        let span = variable.declaration().span();
        let count = variable.value_type().scalar_count().ok_or_else(|| {
            contract(
                &[span],
                "differential coordinates require primitive scalar views",
            )
        })?;
        bases[id.index() as usize] = Some(columns.len());
        for scalar in 0..count {
            columns.push(DifferentialCoordinate {
                variable: id,
                scalar: u32::try_from(scalar)
                    .map_err(|_| contract(&[span], "differential coordinate exceeds u32"))?,
            });
        }
    }
    Ok((columns, bases))
}

fn coordinate_column(
    bases: &[Option<usize>],
    coordinate: dae::CoordinateView<'_>,
    scalar: usize,
) -> Option<(usize, u32)> {
    let (variable, order) = match coordinate {
        dae::CoordinateView::State(state) => (state.index(), 0),
        dae::CoordinateView::Derivative(state) => (state.index(), 1),
        dae::CoordinateView::Algebraic(algebraic) => (algebraic.index(), 0),
        _ => return None,
    };
    let base = bases[variable as usize].expect("continuous source coordinate has a column base");
    let column = base
        .checked_add(scalar)
        .expect("checked source scalar projection fits the allocated column view");
    Some((column, order))
}

fn require_square_matching<'dae>(
    view: dae::DaeView<'dae>,
    variables: &[DifferentialCoordinate<'dae>],
    rows: &[Vec<SignatureEntry>],
    matching: Vec<Option<usize>>,
) -> Result<Vec<usize>, StructuralError> {
    let count = matching.iter().flatten().count();
    if count == rows.len() && count == variables.len() {
        return Ok(matching.into_iter().map(Option::unwrap).collect());
    }
    let mut used = vec![false; variables.len()];
    for &column in matching.iter().flatten() {
        used[column] = true;
    }
    let mut unmatched_unknowns = Vec::new();
    let mut unmatched_unknown_spans = Vec::new();
    for (coordinate, used) in variables.iter().zip(used) {
        if used {
            continue;
        }
        let variable = view
            .variable(coordinate.variable)
            .expect("issuing source variable");
        unmatched_unknowns.push(format!(
            "{} (scalar {})",
            variable.name(),
            coordinate.scalar
        ));
        unmatched_unknown_spans.push(variable.declaration().span());
    }
    Err(StructuralError::Singular {
        n_equations: rows.len(),
        n_unknowns: variables.len(),
        n_matched: count,
        unmatched_equations: matching
            .iter()
            .enumerate()
            .filter(|(_, column)| column.is_none())
            .map(|(row, _)| EquationRef(row).to_string())
            .collect(),
        unmatched_unknowns,
        unmatched_unknown_spans,
        over_determined_block: Box::new(SingularBlockWitness::default()),
    })
}

fn contract(spans: &[Span], reason: &str) -> StructuralError {
    match spans.first() {
        Some(&span) => StructuralError::ContractViolation {
            reason: reason.to_owned(),
            span,
        },
        None => StructuralError::UnspannedContractViolation {
            reason: reason.to_owned(),
        },
    }
}
