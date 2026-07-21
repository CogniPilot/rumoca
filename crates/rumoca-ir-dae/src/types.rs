use rumoca_core::{ComprehensionTemplate, RegularForFamily, Span, StructuredIndexDomain};
use serde::{Deserialize, Serialize};

/// Return a component-path base name with all bracketed subscripts removed.
pub fn component_base_name(name: &str) -> Option<String> {
    rumoca_core::component_path_base_name(name)
}

/// Structured DAE equation family over a compact source domain.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StructuredEquationFamily {
    /// Compact index domain in source binder declaration order.
    pub domain: StructuredIndexDomain,
    /// First equation index in the corresponding DAE equation vector.
    #[serde(default)]
    pub first_equation_index: usize,
    /// Uniform scalar-view equation count emitted by each domain point.
    ///
    /// Keeping one body-row count instead of a per-point vector makes family
    /// metadata independent of compact-domain cardinality.
    pub equations_per_point: usize,
    /// Source span for diagnostics.
    pub span: Span,
    /// Human-readable origin description for traceability.
    pub origin: String,
    /// Family-native classification carried from the flat IR: present when this
    /// family is a regular elementwise stencil with affine subscripts, holding
    /// the per-access stride table the Solve-IR lowering uses to build a compact
    /// `AffineStencil` from one base row. `None` means materialized the old way.
    #[serde(default)]
    pub regular: Option<RegularForFamily>,
    /// The family's canonical comprehension body, carried from the flat IR. When
    /// present, downstream phases read this template directly (printing it as a
    /// `for`-comprehension, deriving strides, building a compact kernel) instead of
    /// reconstructing it from materialized corner cells. `None` for families lowered
    /// before this representation existed. See [`rumoca_core::ComprehensionTemplate`].
    #[serde(default)]
    pub template: Option<ComprehensionTemplate>,
    /// Whether the interior cells' scalar bodies are materialized. `true` (default)
    /// is the historical behavior. `false` means only the corner cells carry real
    /// bodies (a regular family lowered with materialization off), so downstream
    /// phases reconstruct interior incidence/strides from the corners rather than
    /// reading the placeholder interior bodies.
    #[serde(default = "crate::types::default_true")]
    pub interiors_materialized: bool,
}

/// Serde default for `interiors_materialized` (historical: all cells materialized).
pub(crate) fn default_true() -> bool {
    true
}

/// Position of a scalar-view DAE equation inside a structured equation family.
///
/// This is not a serialized owner of mathematical content. It is the stable
/// phase-local handle used by lowering/evaluation code when it needs to relate
/// a scalar-view equation back to the compact structured family that produced
/// it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructuredEquationSlot {
    pub family_index: usize,
    pub iteration_index: usize,
    pub equation_position: usize,
    pub equation_count: usize,
}

impl StructuredEquationFamily {
    pub fn point_count(&self) -> Result<usize, rumoca_core::StructuredIndexDomainError> {
        self.domain.scalar_count()
    }

    pub fn scalar_view_row_count(&self) -> Result<usize, rumoca_core::StructuredIndexDomainError> {
        self.point_count()?
            .checked_mul(self.equations_per_point)
            .ok_or(rumoca_core::StructuredIndexDomainError::ScalarCountOverflow)
    }

    pub fn slot_for_equation(
        &self,
        family_index: usize,
        equation_index: usize,
    ) -> Option<StructuredEquationSlot> {
        let equation_count = (self.equations_per_point > 0).then_some(self.equations_per_point)?;
        let relative_equation = equation_index.checked_sub(self.first_equation_index)?;
        let family_len = self.scalar_view_row_count().ok()?;
        if relative_equation >= family_len {
            return None;
        }
        Some(StructuredEquationSlot {
            family_index,
            iteration_index: relative_equation / equation_count,
            equation_position: relative_equation % equation_count,
            equation_count,
        })
    }
}

pub fn structured_equation_slot(
    structured_equations: &[StructuredEquationFamily],
    equation_index: usize,
) -> Option<StructuredEquationSlot> {
    structured_equations
        .iter()
        .enumerate()
        .find_map(|(family_index, family)| family.slot_for_equation(family_index, equation_index))
}

/// Re-point structured families at their row blocks after an equation-list pass
/// rebuilt the list, expanding some array equations into several scalar rows.
///
/// `spans[old_idx] == (new_start, new_len)` records where input equation `old_idx`
/// landed: an equation expanded into `new_len` rows starting at `new_start`, an
/// unchanged equation reports `new_len == 1`. A family's `first_equation_index`
/// and compact cardinality index into the equation vector, so expanding an earlier
/// equation shifts every later family and silently corrupts its row references
/// unless it is remapped here (e.g. a `pin[:].v` member slice expanded ahead of a
/// `for`-loop family).
///
/// Each family's scalar rows stay contiguous through expansion, so each family is
/// rebuilt by walking its covered input rows and re-deriving its first-row index
/// and uniform body-row count in the new row space. A family whose covered rows are
/// missing or no longer contiguous is dropped, degrading to safe scalar lowering
/// rather than indexing the wrong rows.
pub fn remap_structured_families_after_expansion(
    families: &mut Vec<StructuredEquationFamily>,
    spans: &[(usize, usize)],
) {
    families.retain_mut(|family| match remapped_family_block(family, spans) {
        Some((first, equations_per_point)) => {
            family.first_equation_index = first;
            family.equations_per_point = equations_per_point;
            true
        }
        None => false,
    });
}

/// Compute one family's `(first_equation_index, equations_per_point)` in the new row
/// space, or `None` if its covered rows are missing or no longer contiguous.
fn remapped_family_block(
    family: &StructuredEquationFamily,
    spans: &[(usize, usize)],
) -> Option<(usize, usize)> {
    let mut old_idx = family.first_equation_index;
    let point_count = family.point_count().ok()?;
    let mut new_rows = Vec::new();
    new_rows
        .try_reserve_exact(family.scalar_view_row_count().ok()?)
        .ok()?;
    let mut new_equations_per_point = None;
    for _ in 0..point_count {
        let before = new_rows.len();
        collect_expanded_rows(old_idx, family.equations_per_point, spans, &mut new_rows)?;
        let new_count = new_rows.len() - before;
        if new_count == 0 {
            return None;
        }
        if new_equations_per_point
            .replace(new_count)
            .is_some_and(|count| count != new_count)
        {
            return None;
        }
        old_idx += family.equations_per_point;
    }
    // The family's rows must remain one contiguous run after expansion, else it
    // can no longer describe a single array block and the caller drops it.
    let first = *new_rows.first()?;
    if new_rows
        .iter()
        .enumerate()
        .any(|(offset, &row)| row != first + offset)
    {
        return None;
    }
    Some((first, new_equations_per_point?))
}

/// Append the post-expansion row indices of input equations `old_idx..old_idx+count`
/// to `out`. `None` if any source index has no recorded span.
fn collect_expanded_rows(
    old_idx: usize,
    count: usize,
    spans: &[(usize, usize)],
    out: &mut Vec<usize>,
) -> Option<()> {
    for src in old_idx..old_idx + count {
        let &(new_start, new_len) = spans.get(src)?;
        out.extend(new_start..new_start + new_len);
    }
    Some(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn family(
        first_equation_index: usize,
        point_row_counts: Vec<usize>,
    ) -> StructuredEquationFamily {
        let equations_per_point = *point_row_counts.first().expect("test family has points");
        assert!(
            point_row_counts
                .iter()
                .all(|count| *count == equations_per_point)
        );
        StructuredEquationFamily {
            domain: StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: point_row_counts.len() as i64,
                    step: 1,
                }],
            },
            first_equation_index,
            equations_per_point,
            span: Span::DUMMY,
            origin: "test".to_string(),
            regular: None,
            template: None,
            interiors_materialized: true,
        }
    }

    #[test]
    fn maps_equation_index_to_structured_family_slot() {
        let families = vec![family(10, vec![2, 2, 2])];

        assert_eq!(
            structured_equation_slot(&families, 13),
            Some(StructuredEquationSlot {
                family_index: 0,
                iteration_index: 1,
                equation_position: 1,
                equation_count: 2,
            })
        );
    }

    #[test]
    fn rejects_zero_row_family_slots() {
        let mut zero_row_family = family(10, vec![1, 1, 1]);
        zero_row_family.equations_per_point = 0;
        let families = vec![zero_row_family];

        assert_eq!(structured_equation_slot(&families, 11), None);
    }

    #[test]
    fn remap_shifts_family_past_an_expanded_equation() {
        // Input equation 0 expands into 3 rows (a `v[:] = pin[:].v` member slice);
        // every later equation is unchanged. A family that started at input row 1
        // must move to new row 3.
        let mut families = vec![family(1, vec![1, 1, 1])];
        let spans = [(0, 3), (3, 1), (4, 1), (5, 1), (6, 1), (7, 1)];

        remap_structured_families_after_expansion(&mut families, &spans);

        assert_eq!(families.len(), 1);
        assert_eq!(families[0].first_equation_index, 3);
        assert_eq!(families[0].equations_per_point, 1);
    }

    #[test]
    fn remap_grows_counts_when_family_rows_themselves_expand() {
        // Each of the family's two cells expands 1 -> 2 rows; the block stays
        // contiguous, so counts double and the start tracks the new first row.
        let mut families = vec![family(1, vec![1, 1])];
        let spans = [(0, 1), (1, 2), (3, 2)];

        remap_structured_families_after_expansion(&mut families, &spans);

        assert_eq!(families.len(), 1);
        assert_eq!(families[0].first_equation_index, 1);
        assert_eq!(families[0].equations_per_point, 2);
    }

    #[test]
    fn remap_drops_family_with_missing_span() {
        // A covered row has no span (its equation vanished): the family can no
        // longer index a contiguous block and is dropped.
        let mut families = vec![family(0, vec![1, 1, 1])];
        let spans = [(0, 1), (1, 1)];

        remap_structured_families_after_expansion(&mut families, &spans);

        assert!(families.is_empty());
    }
}
