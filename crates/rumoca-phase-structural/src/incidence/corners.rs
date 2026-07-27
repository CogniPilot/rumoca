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

/// Supplies the walked, canonical (sorted, deduplicated) incidence of one
/// scalar equation row.
///
/// Corner emission reads only base and neighbor rows through this, which is
/// what keeps interior bodies untouched. Tests substitute a fixed row table for
/// the DAE walker.
pub(crate) trait RowSource {
    fn sorted_row(&self, row: usize) -> Vec<usize>;
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
/// Returns `Err` when the family's corner rows are not a uniform per-binder
/// translation (i.e. the family is not actually regular in the numeric unknown
/// layout). Callers roll the builder back and fall through to walking every
/// row, which is exactly the historical behaviour for a declined family.
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
        .map(|position| source.sorted_row(plan.first_equation_index + position))
        .collect();
    let units = derive_translation_units(plan, &base_rows, source)?;

    for base in &base_rows {
        builder.push_sorted(base);
    }
    for point in 1..plan.point_count {
        emit_point_rows(builder, plan, &units, point)?;
    }
    Ok(())
}

fn emit_point_rows(
    builder: &mut IncidenceRowsBuilder,
    plan: &CornerPlan,
    units: &[Vec<i64>],
    point: usize,
) -> Result<(), StructuralError> {
    for (position, steps) in units.iter().enumerate() {
        let shift = cell_shift(plan, steps, point).ok_or_else(|| {
            contract(
                plan,
                format!(
                    "corner translation for family row {} point {point} overflows",
                    plan.first_equation_index
                ),
            )
        })?;
        builder.push_translated(plan.first_equation_index + position, shift)?;
    }
    Ok(())
}

/// Per-(row position, binder) translation unit, read from each binder's
/// neighbor cell. A row may shift differently per binder because its accesses
/// can target distinct arrays; an extent-1 binder contributes no shift.
fn derive_translation_units(
    plan: &CornerPlan,
    base_rows: &[Vec<usize>],
    source: &dyn RowSource,
) -> Result<Vec<Vec<i64>>, StructuralError> {
    let mut units = vec![vec![0i64; plan.extents.len()]; plan.equations_per_point];
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
            let neighbor = source.sorted_row(row);
            let unit = uniform_translation(base, &neighbor).ok_or_else(|| {
                contract(
                    plan,
                    format!("family row {row} is not a uniform translation of its base cell"),
                )
            })?;
            units[position][dimension] = unit;
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

/// A family-level contract failure, reported against the owning
/// `for`-equation's span.
///
/// The caller of [`emit_family_rows`] treats the error as a *decline* and walks
/// the family's rows body by body instead. That is not silent recovery: the
/// walked incidence is the authoritative relation, and the corner model is only
/// the arithmetic shortcut to it. The span is attached anyway so that any future
/// caller that surfaces the failure -- rather than falling back -- reports it
/// against real source.
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
        fn sorted_row(&self, row: usize) -> Vec<usize> {
            let mut columns = match self.0.get(row) {
                Some(columns) => columns.clone(),
                None => Vec::new(),
            };
            columns.sort_unstable();
            columns.dedup();
            columns
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
    fn corner_emission_declines_when_the_neighbor_is_not_a_translation() {
        // Base {0,1}; neighbor {2,5} shifts its elements by 2 and 4 -- not a
        // single constant offset.
        let plan = CornerPlan::from_family(&one_dim_family(2)).expect("1-D corner plan");
        let table = TableRows(vec![vec![0, 1], vec![2, 5]]);
        assert!(synthesize(&plan, &table).is_none());
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
            fn sorted_row(&self, row: usize) -> Vec<usize> {
                self.reads.borrow_mut().push(row);
                self.table.sorted_row(row)
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
