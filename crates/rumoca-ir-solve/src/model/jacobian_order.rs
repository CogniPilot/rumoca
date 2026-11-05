//! Checked output axes for the explicitly stored-coordinate Jacobian product.
#[cfg(test)]
mod tests;

use crate::SolveModel;
use std::collections::{BTreeMap, BTreeSet};

/// Complete row/column bijections over one model's stored solver states.
/// This is not a tangent/retraction map or Jacobian-domain admission. Consuming
/// construction takes this object's model, never a second independently supplied
/// model. No detached index permutation, mutation or wire constructor is exposed.
///
/// ```compile_fail
/// use rumoca_ir_solve::{SolveModel, StoredStateJacobianOutputOrder};
/// fn immutable_source(model: &mut SolveModel) {
///     let order = StoredStateJacobianOutputOrder::canonical(model).unwrap();
///     model.problem = Default::default();
///     let _ = order.row_labels();
/// }
/// ```
#[derive(Debug)]
pub struct StoredStateJacobianOutputOrder<'source> {
    model: &'source SolveModel,
    input_labels: Vec<String>,
    row_labels: Vec<String>,
    column_labels: Vec<String>,
    rows: Vec<usize>,
    columns: Vec<usize>,
    cells: usize,
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum StoredStateJacobianOrderError {
    #[error("stored-state Jacobian extent overflows")]
    ExtentOverflow,
    #[error("canonical stored-state label is missing at coordinate {index}")]
    MissingCanonicalLabel { index: usize },
    #[error("canonical stored-state labels are ambiguous")]
    AmbiguousCanonicalLabels,
    #[error("Jacobian {axis} axis has {actual} labels; expected {expected}")]
    Length {
        axis: &'static str,
        expected: usize,
        actual: usize,
    },
    #[error("Jacobian {axis} axis has duplicate or foreign stored-state labels")]
    InvalidAxis { axis: &'static str },
}

fn canonical_labels(model: &SolveModel) -> Result<Vec<String>, StoredStateJacobianOrderError> {
    let n = model.state_scalar_count();
    n.checked_mul(n)
        .ok_or(StoredStateJacobianOrderError::ExtentOverflow)?;
    let labels = (0..n)
        .map(|index| {
            model
                .problem
                .solver_name(index)
                .ok_or(StoredStateJacobianOrderError::MissingCanonicalLabel { index })
        })
        .collect::<Result<Vec<_>, _>>()?;
    if labels.iter().collect::<BTreeSet<_>>().len() != labels.len() {
        return Err(StoredStateJacobianOrderError::AmbiguousCanonicalLabels);
    }
    Ok(labels)
}

fn axis(
    requested: &[String],
    canonical: &[String],
    name: &'static str,
) -> Result<Vec<usize>, StoredStateJacobianOrderError> {
    if requested.len() != canonical.len() {
        return Err(StoredStateJacobianOrderError::Length {
            axis: name,
            expected: canonical.len(),
            actual: requested.len(),
        });
    }
    let domain = canonical
        .iter()
        .enumerate()
        .map(|(index, label)| (label.as_str(), index))
        .collect::<BTreeMap<_, _>>();
    let mut seen = BTreeSet::new();
    requested
        .iter()
        .map(|label| {
            domain
                .get(label.as_str())
                .copied()
                .filter(|index| seen.insert(*index))
                .ok_or(StoredStateJacobianOrderError::InvalidAxis { axis: name })
        })
        .collect()
}

impl<'source> StoredStateJacobianOutputOrder<'source> {
    pub fn canonical(model: &'source SolveModel) -> Result<Self, StoredStateJacobianOrderError> {
        let labels = canonical_labels(model)?;
        Self::issue(model, labels.clone(), labels.clone(), labels)
    }

    /// Labels request placements; canonical source coordinates provide identity.
    /// Both axes must cover every stored solver-state coordinate exactly once.
    pub fn construct(
        model: &'source SolveModel,
        rows: &[String],
        columns: &[String],
    ) -> Result<Self, StoredStateJacobianOrderError> {
        Self::issue(
            model,
            canonical_labels(model)?,
            rows.to_vec(),
            columns.to_vec(),
        )
    }

    fn issue(
        model: &'source SolveModel,
        input_labels: Vec<String>,
        row_labels: Vec<String>,
        column_labels: Vec<String>,
    ) -> Result<Self, StoredStateJacobianOrderError> {
        let rows = axis(&row_labels, &input_labels, "row")?;
        let columns = axis(&column_labels, &input_labels, "column")?;
        let cells = rows
            .len()
            .checked_mul(columns.len())
            .ok_or(StoredStateJacobianOrderError::ExtentOverflow)?;
        Ok(Self {
            model,
            input_labels,
            row_labels,
            column_labels,
            rows,
            columns,
            cells,
        })
    }

    pub fn source_model(&self) -> &'source SolveModel {
        self.model
    }
    /// State inputs always retain canonical order, independently of output axes.
    pub fn input_labels(&self) -> &[String] {
        &self.input_labels
    }
    pub fn row_labels(&self) -> &[String] {
        &self.row_labels
    }
    pub fn column_labels(&self) -> &[String] {
        &self.column_labels
    }
    pub fn row_coordinates(&self) -> &[usize] {
        &self.rows
    }
    pub fn column_coordinates(&self) -> &[usize] {
        &self.columns
    }
    pub fn cells(&self) -> usize {
        self.cells
    }
}
