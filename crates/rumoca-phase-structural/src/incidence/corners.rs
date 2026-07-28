//! Corner-derived incidence for non-materialized regular equation families.
//!
//! A `regular` family whose interior cells were never materialized carries real
//! equation bodies only on its corner rows: the base cell plus one neighbor per
//! binder. Structural analysis reconstructs every remaining scalar row by
//! translating the base rows, so the compatibility graph is produced in
//! `O(corners)` body walks and `O(1)` arithmetic per emitted row instead of
//! walking (and hashing) every interior body.
//!
//! This is the structural-incidence analogue of the Solve-IR corner-row stride
//! derivation described by SPEC_0032 §2: the compact domain is the authority on
//! row identity, and interior rows are never read.

use rumoca_ir_dae as dae;

use crate::incidence::rows::IncidenceRowsBuilder;
use crate::types::StructuralError;

/// Supplies unknown occurrences in deterministic expression-walk order for one
/// scalar equation row.
///
/// Corner emission reads only base and neighbor rows through this, which is
/// what keeps interior bodies untouched. Occurrence order is required: sorted
/// incidence sets lose access identity when two affine accesses cross between
/// corner points. Tests substitute a fixed row table for the DAE walker.
pub(crate) trait RowSource {
    fn ordered_row(&self, row: usize) -> Vec<usize>;
}

/// The corner model of one regular family: the compact domain shape plus the
/// contiguous scalar row range it owns.
#[derive(Debug, Clone)]
pub(crate) struct CornerPlan {
    pub(crate) first_equation_index: usize,
    pub(crate) equations_per_point: usize,
    pub(crate) point_count: usize,
    pub(crate) extents: Vec<usize>,
    pub(crate) cell_strides: Vec<usize>,
    /// Span of the `for`-equation that owns the family. Every contract failure
    /// raised while deriving or emitting the family's rows is reported against
    /// it, so the diagnostic points at real source instead of being span-free.
    pub(crate) span: rumoca_core::Span,
}

impl CornerPlan {
    /// Derive the corner model of `family`, or `None` when the family's compact
    /// domain does not describe a contiguous uniform row block.
    pub(crate) fn from_family(family: &dae::StructuredEquationFamily) -> Option<Self> {
        let equations_per_point = family.equations_per_point;
        if equations_per_point == 0 {
            return None;
        }
        let point_count = family.point_count().ok()?;
        if point_count == 0 {
            return None;
        }
        let extents = family.domain.extents().ok()?;
        let cell_strides = family.domain.ordinal_strides().ok()?;
        if extents.len() != cell_strides.len() {
            return None;
        }
        Some(Self {
            first_equation_index: family.first_equation_index,
            equations_per_point,
            point_count,
            extents,
            cell_strides,
            span: family.span,
        })
    }

    /// Number of scalar rows the family owns.
    pub(crate) fn row_count(&self) -> Option<usize> {
        self.point_count.checked_mul(self.equations_per_point)
    }

    /// One past the family's last scalar row.
    pub(crate) fn end_row(&self) -> Option<usize> {
        self.first_equation_index.checked_add(self.row_count()?)
    }

    /// Row index of `equation_position` within the cell at `point`.
    pub(crate) fn row_of(&self, point: usize, equation_position: usize) -> Option<usize> {
        point
            .checked_mul(self.equations_per_point)?
            .checked_add(equation_position)?
            .checked_add(self.first_equation_index)
    }
}

/// The corner models of every non-materialized regular family in `dae`, sorted
/// by first row and pruned to disjoint, in-range blocks.
///
/// Materialized families are deliberately excluded: their walked incidence is
/// authoritative, so they keep the per-row body walk.
#[cfg(test)]
pub(crate) fn plan_regular_family_corners(dae: &dae::Dae, n_eq: usize) -> Vec<CornerPlan> {
    let mut plans: Vec<CornerPlan> = dae
        .continuous
        .structured_equations
        .iter()
        .filter(|family| family.regular.is_some() && !family.interiors_materialized)
        .filter_map(CornerPlan::from_family)
        .filter(|plan| plan.end_row().is_some_and(|end| end <= n_eq))
        .collect();
    plans.sort_by_key(|plan| plan.first_equation_index);
    retain_disjoint(plans)
}

/// Keep only plans whose row blocks do not overlap an already-kept block.
///
/// Family row ranges are disjoint by construction; this is a defensive filter
/// so a degraded IR cannot make one family overwrite another's rows.
#[cfg(test)]
fn retain_disjoint(plans: Vec<CornerPlan>) -> Vec<CornerPlan> {
    let mut kept: Vec<CornerPlan> = Vec::with_capacity(plans.len());
    let mut next_free_row = 0usize;
    for plan in plans {
        let Some(end) = plan.end_row() else {
            continue;
        };
        if plan.first_equation_index < next_free_row {
            continue;
        }
        next_free_row = end;
        kept.push(plan);
    }
    kept
}

/// Emit every scalar row of `plan` into `builder`, walking only the corner
/// rows.
///
/// Precondition: `builder.row_count() == plan.first_equation_index`, so the
/// builder's row indices coincide with DAE equation indices and translated rows
/// can name their base row directly.
///
/// Returns `Err` when the family's corner rows cannot establish an affine
/// per-access translation (i.e. the family is not actually regular in the
/// numeric unknown layout). Non-materialized interior bodies are placeholders,
/// so production callers propagate this error rather than walking those rows.
pub(crate) fn emit_family_rows(
    builder: &mut IncidenceRowsBuilder,
    plan: &CornerPlan,
    source: &dyn RowSource,
) -> Result<(), StructuralError> {
    if builder.row_count() != plan.first_equation_index {
        return Err(contract(
            plan,
            format!(
                "corner emission expected builder row {} but found {}",
                plan.first_equation_index,
                builder.row_count()
            ),
        ));
    }
    let base_rows: Vec<Vec<usize>> = (0..plan.equations_per_point)
        .map(|position| source.ordered_row(plan.first_equation_index + position))
        .collect();
    let units = derive_translation_units(plan, &base_rows, source)?;

    for base in &base_rows {
        builder.push_occurrences(base);
    }
    for point in 1..plan.point_count {
        emit_point_rows(builder, plan, &base_rows, &units, point)?;
    }
    Ok(())
}

fn emit_point_rows(
    builder: &mut IncidenceRowsBuilder,
    plan: &CornerPlan,
    base_rows: &[Vec<usize>],
    units: &[Vec<Vec<i64>>],
    point: usize,
) -> Result<(), StructuralError> {
    for (position, occurrence_units) in units.iter().enumerate() {
        let shifts: Vec<i64> = occurrence_units
            .iter()
            .map(|steps| {
                cell_shift(plan, steps, point).ok_or_else(|| {
                    contract(
                        plan,
                        format!(
                            "corner translation for family row {} point {point} overflows",
                            plan.first_equation_index
                        ),
                    )
                })
            })
            .collect::<Result<_, _>>()?;
        let source = base_rows.get(position).ok_or_else(|| {
            contract(
                plan,
                format!(
                    "corner translation has no source occurrences for family row position {position}"
                ),
            )
        })?;
        builder.push_affine_occurrences(source, &shifts)?;
    }
    Ok(())
}

/// Per-(row position, unknown occurrence, binder) translation unit, read from
/// each binder's neighbor cell.
///
/// Occurrences are independent because a regular body can capture a scalar while
/// indexing an array, e.g. `der(x[i]) = u`: `u` has zero stride and `der(x[i])`
/// advances. An extent-1 binder contributes no shift.
fn derive_translation_units(
    plan: &CornerPlan,
    base_rows: &[Vec<usize>],
    source: &dyn RowSource,
) -> Result<Vec<Vec<Vec<i64>>>, StructuralError> {
    let mut units: Vec<Vec<Vec<i64>>> = base_rows
        .iter()
        .map(|row| vec![vec![0i64; plan.extents.len()]; row.len()])
        .collect();
    for (dimension, &extent) in plan.extents.iter().enumerate() {
        if extent <= 1 {
            continue;
        }
        let neighbor_point = plan.cell_strides[dimension];
        for (position, base) in base_rows.iter().enumerate() {
            let row = plan.row_of(neighbor_point, position).ok_or_else(|| {
                contract(
                    plan,
                    format!(
                        "corner neighbor row for binder {dimension} position {position} overflows"
                    ),
                )
            })?;
            let neighbor = source.ordered_row(row);
            let column_units = column_translations(base, &neighbor).ok_or_else(|| {
                contract(
                    plan,
                    format!(
                        "family row {row} does not preserve its base-cell incidence cardinality"
                    ),
                )
            })?;
            for (column, unit) in column_units.into_iter().enumerate() {
                units[position][column][dimension] = unit;
            }
        }
    }
    Ok(units)
}

/// Total index shift of the cell at `point` for a row whose per-binder units
/// are `steps`.
pub(crate) fn cell_shift(plan: &CornerPlan, steps: &[i64], point: usize) -> Option<i64> {
    let mut shift = 0i128;
    for (dimension, &step) in steps.iter().enumerate() {
        let stride = *plan.cell_strides.get(dimension)?;
        let extent = *plan.extents.get(dimension)?;
        let coordinate = i128::try_from(cell_coordinate(point, stride, extent)).ok()?;
        shift = shift.checked_add(coordinate.checked_mul(i128::from(step))?)?;
    }
    i64::try_from(shift).ok()
}

/// The position-count of cell `cell` along one binder, given that binder's
/// row-major cell stride and extent: `(cell / stride) % extent`.
pub(crate) fn cell_coordinate(cell: usize, stride: usize, extent: usize) -> usize {
    if stride == 0 || extent == 0 {
        return 0;
    }
    (cell / stride) % extent
}

/// If `neighbor` is `base` shifted by a single constant offset, return that
/// delta. `None` means the two cells' incidence is not a pure translation. An
/// empty base translates trivially by 0.
///
/// Both arguments are canonical CSR runs (ascending, deduplicated), so the two
/// sequences are compared elementwise with no allocation and no re-sort.
pub(crate) fn uniform_translation(base: &[usize], neighbor: &[usize]) -> Option<i64> {
    if base.len() != neighbor.len() {
        return None;
    }
    let (Some(&base_first), Some(&neighbor_first)) = (base.first(), neighbor.first()) else {
        return Some(0);
    };
    let delta = i128::try_from(neighbor_first)
        .ok()?
        .checked_sub(i128::try_from(base_first).ok()?)?;
    let uniform = base.iter().zip(neighbor).all(|(&left, &right)| {
        match (i128::try_from(left), i128::try_from(right)) {
            (Ok(left), Ok(right)) => right.checked_sub(left) == Some(delta),
            _ => false,
        }
    });
    if !uniform {
        return None;
    }
    i64::try_from(delta).ok()
}

/// Per-occurrence affine translation from one corner cell to its neighbor.
///
/// Expression-walk order preserves source-access identity, so each occurrence
/// can have its own stride, including zero for a binder-invariant capture.
fn column_translations(base: &[usize], neighbor: &[usize]) -> Option<Vec<i64>> {
    if base.len() != neighbor.len() {
        return None;
    }
    base.iter()
        .zip(neighbor)
        .map(|(&left, &right)| {
            let delta = i128::try_from(right)
                .ok()?
                .checked_sub(i128::try_from(left).ok()?)?;
            i64::try_from(delta).ok()
        })
        .collect()
}

/// A family-level contract failure, reported against the owning
/// `for`-equation's span.
///
/// The span is attached so a failed compact proof reports against the source
/// `for` equation instead of becoming a span-free backend error.
fn contract(plan: &CornerPlan, reason: String) -> StructuralError {
    StructuralError::ContractViolation {
        reason,
        span: plan.span,
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::incidence::rows::IncidenceRows;

    /// A fixed per-row incidence table standing in for the DAE body walker.
    pub(crate) struct TableRows(pub(crate) Vec<Vec<usize>>);

    impl RowSource for TableRows {
        fn ordered_row(&self, row: usize) -> Vec<usize> {
            match self.0.get(row) {
                Some(columns) => columns.clone(),
                None => Vec::new(),
            }
        }
    }

    pub(crate) fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_structural_corner_fixture.mo"),
            1,
            2,
        )
    }

    /// A 1-D `regular` family of `cells` cells, one equation per cell.
    pub(crate) fn one_dim_family(cells: i64) -> dae::StructuredEquationFamily {
        use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
        dae::StructuredEquationFamily {
            domain: StructuredIndexDomain {
                binders: vec![StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: cells,
                    step: 1,
                }],
            },
            first_equation_index: 0,
            equations_per_point: 1,
            span: test_span(),
            origin: "corner_check_fixture".to_string(),
            regular: None,
            template: None,
            interiors_materialized: true,
        }
    }

    /// A 2-D `regular` family over a `rows x cols` row-major domain.
    pub(crate) fn two_dim_family(rows: i64, cols: i64) -> dae::StructuredEquationFamily {
        use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
        let binder = |id, name: &str, upper| StructuredIndexBinder {
            id,
            display_name: name.to_string(),
            lower: 1,
            upper,
            step: 1,
        };
        dae::StructuredEquationFamily {
            domain: StructuredIndexDomain {
                binders: vec![binder(0, "i", rows), binder(1, "j", cols)],
            },
            first_equation_index: 0,
            equations_per_point: 1,
            span: test_span(),
            origin: "corner_check_fixture_2d".to_string(),
            regular: None,
            template: None,
            interiors_materialized: true,
        }
    }

    /// Emit `plan` into a fresh builder, padding the rows before the family so
    /// builder indices line up with equation indices.
    pub(crate) fn synthesize(plan: &CornerPlan, source: &dyn RowSource) -> Option<IncidenceRows> {
        let mut builder = IncidenceRowsBuilder::default();
        for _ in 0..plan.first_equation_index {
            builder.push_sorted(&[]);
        }
        emit_family_rows(&mut builder, plan, source).ok()?;
        Some(builder.finish())
    }

    #[test]
    fn uniform_translation_detects_constant_shift() {
        // A stencil cell {der(X[i]), X[i-1], X[i], X[i+1]} stepped by one
        // position shifts every unknown index by the same array stride.
        assert_eq!(
            uniform_translation(&[10, 20, 21, 22], &[13, 23, 24, 25]),
            Some(3)
        );
    }

    #[test]
    fn uniform_translation_rejects_non_uniform_or_mismatched() {
        assert_eq!(uniform_translation(&[1, 2, 3], &[2, 3, 5]), None);
        assert_eq!(uniform_translation(&[1, 2], &[2, 3, 4]), None);
        assert_eq!(uniform_translation(&[], &[]), Some(0));
    }

    #[test]
    fn column_translations_preserve_invariant_captures() {
        assert_eq!(column_translations(&[10, 40], &[11, 40]), Some(vec![1, 0]));
        assert_eq!(column_translations(&[1], &[2, 3]), None);
    }

    #[test]
    fn cell_coordinate_decomposes_row_major_position() {
        // 3x4 grid, row-major: strides [4, 1], extents [3, 4]. Cell 6 = (1, 2).
        assert_eq!(cell_coordinate(6, 4, 3), 1);
        assert_eq!(cell_coordinate(6, 1, 4), 2);
        assert_eq!(cell_coordinate(11, 4, 3), 2);
        assert_eq!(cell_coordinate(11, 1, 4), 3);
        // Degenerate strides/extents stay at 0 rather than dividing by zero.
        assert_eq!(cell_coordinate(5, 0, 3), 0);
        assert_eq!(cell_coordinate(5, 2, 0), 0);
    }

    #[test]
    fn corner_emission_reconstructs_interiors_from_corner_rows_only() {
        // 4-cell 1-D family. Only the base (row 0) and its neighbor (row 1)
        // carry real incidence; rows 2 and 3 are EMPTY, standing in for
        // cheapened interiors.
        let plan = CornerPlan::from_family(&one_dim_family(4)).expect("1-D corner plan");
        let table = TableRows(vec![vec![5, 6], vec![7, 8], Vec::new(), Vec::new()]);
        let rows = synthesize(&plan, &table).expect("regular 1-D family is corner-derivable");
        assert_eq!(rows.row(0), &[5, 6]);
        assert_eq!(rows.row(1), &[7, 8]);
        assert_eq!(rows.row(2), &[9, 10]);
        assert_eq!(rows.row(3), &[11, 12]);
    }

    #[test]
    fn corner_emission_keeps_fixed_and_varying_columns() {
        let plan = CornerPlan::from_family(&one_dim_family(4)).expect("1-D corner plan");
        let table = TableRows(vec![vec![0, 4], vec![1, 4], Vec::new(), Vec::new()]);
        let rows = synthesize(&plan, &table).expect("captured scalar has an affine zero stride");
        assert_eq!(rows.row(0), &[0, 4]);
        assert_eq!(rows.row(1), &[1, 4]);
        assert_eq!(rows.row(2), &[2, 4]);
        assert_eq!(rows.row(3), &[3, 4]);
    }

    #[test]
    fn corner_emission_reconstructs_2d_interior_from_two_corner_neighbors() {
        // 2x2 row-major family. The j-unit is +2 (cell0 -> cell1) and the
        // i-unit is +10 (cell0 -> cell2), so cell3 must be the base shifted by
        // 12 -- reconstructed without reading cell3.
        let plan = CornerPlan::from_family(&two_dim_family(2, 2)).expect("2-D corner plan");
        let table = TableRows(vec![vec![10, 11], vec![12, 13], vec![20, 21], Vec::new()]);
        let rows = synthesize(&plan, &table).expect("regular 2-D family is corner-derivable");
        assert_eq!(rows.row(3), &[22, 23]);
    }

    #[test]
    fn corner_emission_supports_distinct_access_strides() {
        // The two accesses advance by different strides. This is still affine:
        // it represents two arrays with different scalar layouts.
        let plan = CornerPlan::from_family(&one_dim_family(3)).expect("1-D corner plan");
        let table = TableRows(vec![vec![0, 1], vec![2, 5], Vec::new()]);
        let rows = synthesize(&plan, &table).expect("each access has an affine stride");
        assert_eq!(rows.row(2), &[4, 9]);
    }

    #[test]
    fn corner_emission_preserves_identity_when_affine_accesses_cross() {
        // Expression order is [x[3*i], x[8-2*i]]. Numeric incidence order
        // crosses between i=1 and i=2, so sorting the corners before pairing
        // would infer the wrong strides for i=3.
        let plan = CornerPlan::from_family(&one_dim_family(3)).expect("1-D corner plan");
        let table = TableRows(vec![vec![3, 6], vec![6, 4], Vec::new()]);
        let rows = synthesize(&plan, &table).expect("occurrence identity establishes both strides");
        assert_eq!(rows.row(0), &[3, 6]);
        assert_eq!(rows.row(1), &[4, 6]);
        assert_eq!(rows.row(2), &[2, 9]);
    }

    #[test]
    fn corner_emission_overrides_a_materialized_interior_that_breaks_translation() {
        // Base {0,1}; neighbor {1,2} fixes a unit translation of +1, so the
        // emission predicts {2,3} for cell 2 even though the table carries
        // {2,9}. This is the mismatch the corner model treats as a decline at
        // the family level.
        let plan = CornerPlan::from_family(&one_dim_family(3)).expect("1-D corner plan");
        let table = TableRows(vec![vec![0, 1], vec![1, 2], vec![2, 9]]);
        let rows = synthesize(&plan, &table).expect("corners are derivable");
        assert_eq!(rows.row(2), &[2, 3]);
    }

    #[test]
    fn corner_emission_never_walks_an_interior_row() {
        // 100_000 cells with empty interior bodies: the emission must read only
        // rows 0 and 1 and still produce the translated interior.
        use std::cell::RefCell;

        struct CountingRows {
            table: TableRows,
            reads: RefCell<Vec<usize>>,
        }
        impl RowSource for CountingRows {
            fn ordered_row(&self, row: usize) -> Vec<usize> {
                self.reads.borrow_mut().push(row);
                self.table.ordered_row(row)
            }
        }

        let cells = 100_000usize;
        let plan = CornerPlan::from_family(&one_dim_family(
            i64::try_from(cells).expect("cell count fits i64"),
        ))
        .expect("1-D corner plan");
        let mut table = vec![Vec::new(); cells];
        table[0] = vec![0, 1];
        table[1] = vec![2, 3];
        let source = CountingRows {
            table: TableRows(table),
            reads: RefCell::new(Vec::new()),
        };
        let rows = synthesize(&plan, &source).expect("corner-derivable");
        assert_eq!(rows.len(), cells);
        assert_eq!(rows.row(cells - 1), &[199_998, 199_999]);
        assert_eq!(source.reads.into_inner(), vec![0, 1]);
    }

    #[test]
    fn plans_are_dropped_for_materialized_and_overlapping_families() {
        let mut materialized = one_dim_family(4);
        materialized.regular = Some(rumoca_core::RegularForFamily {
            binders: vec!["i".to_string()],
            accesses: Vec::new(),
        });
        let mut compact = materialized.clone();
        compact.interiors_materialized = false;
        let mut overlapping = compact.clone();
        overlapping.first_equation_index = 2;

        let dae = dae_with_families(vec![materialized, compact, overlapping]);
        let plans = plan_regular_family_corners(&dae, 8);
        assert_eq!(plans.len(), 1, "materialized and overlapping plans dropped");
        assert_eq!(plans[0].first_equation_index, 0);
        assert_eq!(plans[0].point_count, 4);
    }

    fn dae_with_families(families: Vec<dae::StructuredEquationFamily>) -> dae::Dae {
        let mut dae = dae::Dae::default();
        dae.continuous.structured_equations = families;
        dae
    }
}
