use rumoca_core::{Span, StructuredIndexDomain};
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
    /// Scalar-view equation count for each domain point in deterministic order.
    pub equation_counts: Vec<usize>,
    /// Source span for diagnostics.
    pub span: Span,
    /// Human-readable origin description for traceability.
    pub origin: String,
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
    pub fn common_iteration_equation_count(&self) -> Option<usize> {
        let first = *self.equation_counts.first()?;
        if first == 0 {
            return None;
        }
        self.equation_counts
            .iter()
            .all(|equation_count| *equation_count == first)
            .then_some(first)
    }

    pub fn slot_for_equation(
        &self,
        family_index: usize,
        equation_index: usize,
    ) -> Option<StructuredEquationSlot> {
        let equation_count = self.common_iteration_equation_count()?;
        let relative_equation = equation_index.checked_sub(self.first_equation_index)?;
        let family_len = equation_count.checked_mul(self.equation_counts.len())?;
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

#[cfg(test)]
mod tests {
    use super::*;

    fn family(
        first_equation_index: usize,
        equation_counts: Vec<usize>,
    ) -> StructuredEquationFamily {
        StructuredEquationFamily {
            domain: StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: equation_counts.len() as i64,
                    step: 1,
                }],
            },
            first_equation_index,
            equation_counts,
            span: Span::DUMMY,
            origin: "test".to_string(),
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
    fn rejects_non_uniform_family_slots() {
        let families = vec![family(10, vec![1, 2, 1])];

        assert_eq!(structured_equation_slot(&families, 11), None);
    }
}
