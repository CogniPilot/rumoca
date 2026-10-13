//! Demanded scalar views of the single checked declaration layout owner.

use super::{ScalarSlot, VarLayout, flat_index_to_subscripts, identity, offset_contiguous_slot};
use rumoca_core::VarName;

impl VarLayout {
    pub fn scalar_declaration_name(&self, binding: &VarName, ordinal: usize) -> Option<String> {
        self.scalar_binding(binding, *self.bindings.get(binding)?, ordinal)
            .map(|(name, _)| name)
    }

    /// Explicit scalar-consumer view; no scalar table is retained or serialized.
    pub fn scalar_bindings(&self) -> impl Iterator<Item = (String, ScalarSlot)> + '_ {
        self.bindings.iter().flat_map(move |(name, slot)| {
            let count = self
                .shapes
                .get(name)
                .map_or(1, |shape| shape.iter().product());
            (0..count).filter_map(move |ordinal| self.scalar_binding(name, *slot, ordinal))
        })
    }

    fn scalar_binding(
        &self,
        name: &VarName,
        base: ScalarSlot,
        ordinal: usize,
    ) -> Option<(String, ScalarSlot)> {
        let Some(shape) = self.shapes.get(name) else {
            return (ordinal == 0).then(|| (name.to_string(), base));
        };
        let indices = flat_index_to_subscripts(
            name.as_str(),
            shape,
            ordinal,
            self.shape_spans.get(name).copied(),
        )
        .ok()??;
        let scalar_name = rumoca_core::scalar_component_display_name(name.as_str(), &indices)?;
        let slot = if let Some(entries) = self
            .shape_indexed_keys
            .get(name)
            .and_then(|key| self.indexed_bindings.get(key))
        {
            entries.get(ordinal)?.slot
        } else {
            offset_contiguous_slot(base, ordinal)?
        };
        Some((scalar_name, slot))
    }

    /// Demand one scalar display name from the original declaration run.
    pub fn scalar_name_for_slot(&self, slot: ScalarSlot) -> Option<String> {
        for (name, base) in &self.bindings {
            let ordinal = match (*base, slot) {
                (ScalarSlot::Y { index: start, .. }, ScalarSlot::Y { index, .. })
                | (ScalarSlot::P { index: start, .. }, ScalarSlot::P { index, .. }) => {
                    index.checked_sub(start)
                }
                _ if identity::same_slot(base, &slot) => Some(0),
                _ => None,
            };
            if let Some((name, actual)) =
                ordinal.and_then(|ordinal| self.scalar_binding(name, *base, ordinal))
                && identity::same_slot(&actual, &slot)
            {
                return Some(name);
            }
        }
        None
    }
}

impl VarLayout {
    pub(crate) fn has_indexed_shape_binding(&self, name: &VarName) -> bool {
        self.shape_indexed_keys
            .get(name)
            .is_some_and(|key| self.indexed_bindings.contains_key(key))
    }

    pub(crate) fn y_declaration_ranges(
        &self,
    ) -> Result<Vec<std::ops::Range<usize>>, crate::SolveProblemShapeContractError> {
        let mut ranges = Vec::new();
        for (name, slot) in &self.bindings {
            let ScalarSlot::Y { index, .. } = *slot else {
                continue;
            };
            if self.has_indexed_shape_binding(name) {
                return Err(
                    crate::SolveProblemShapeContractError::DerivativeStateStorage {
                        detail: format!(
                            "Y declaration `{name}` has indexed storage, not a contiguous declaration range"
                        ),
                    },
                );
            }
            if !identity::same_slot(slot, &super::scalar_slot_y(index)) {
                return Err(
                    crate::SolveProblemShapeContractError::DerivativeStateStorage {
                        detail: format!("Y declaration `{name}` has a foreign byte offset"),
                    },
                );
            }
            let count = self
                .shapes
                .get(name)
                .map_or(Some(1), |shape| {
                    shape.iter().try_fold(1usize, |n, d| n.checked_mul(*d))
                })
                .ok_or_else(
                    || crate::SolveProblemShapeContractError::DerivativeStateStorage {
                        detail: "Y declaration shape overflow".into(),
                    },
                )?;
            let end = index.checked_add(count).ok_or_else(|| {
                crate::SolveProblemShapeContractError::DerivativeStateStorage {
                    detail: "Y declaration range overflow".into(),
                }
            })?;
            if count != 0 {
                ranges.push(index..end);
            }
        }
        ranges.sort_by_key(|range| range.start);
        Ok(ranges)
    }

    pub(crate) fn validate_named_y_coverage(
        &self,
        state_count: usize,
    ) -> Result<(), crate::SolveProblemShapeContractError> {
        let error = |detail: String| {
            crate::SolveProblemShapeContractError::DerivativeStateStorage { detail }
        };
        if self.y_scalars != state_count {
            return Err(error(format!(
                "{state_count} states need {state_count} Y scalars, the layout owns {}",
                self.y_scalars
            )));
        }
        let mut cursor = 0;
        for range in self.y_declaration_ranges()? {
            if range.start != cursor {
                return Err(error(format!(
                    "state slot Y[{cursor}] is not named by exactly one declaration"
                )));
            }
            cursor = range.end;
        }
        if cursor != state_count {
            return Err(error(format!("state slot Y[{cursor}] has no name")));
        }
        Ok(())
    }
}

#[cfg(test)]
mod compact_binding_tests {
    use super::super::{VarLayoutShapeContractError, scalar_slot_p};
    use super::*;

    #[test]
    fn compact_declaration_bindings_issue_only_demanded_scalar_views() {
        for count in [2, 65536] {
            let span = super::super::tests::layout_source_span(77, 8, 11);
            let layout = VarLayout::from_parts_with_shapes_and_spans(
                [("u".to_owned(), scalar_slot_p(3))].into(),
                [("u".to_owned(), vec![count])].into(),
                [("u".to_owned(), span)].into(),
                0,
                count + 3,
            )
            .unwrap();
            assert_eq!(layout.bindings().len(), 1);
            assert_eq!(layout.shape_span("u"), Some(span));
            let wire = serde_json::to_value(&layout).unwrap();
            assert_eq!(wire["bindings"].as_object().unwrap().len(), 1);
            for ordinal in [0, count / 2, count - 1] {
                let name = format!("u[{}]", ordinal + 1);
                let slot = scalar_slot_p(3 + ordinal);
                assert_eq!(layout.binding(&name), Some(slot));
                assert_eq!(layout.scalar_name_for_slot(slot), Some(name));
            }
            assert_eq!(
                layout.scalar_bindings().take(2).collect::<Vec<_>>(),
                vec![
                    ("u[1]".into(), scalar_slot_p(3)),
                    ("u[2]".into(), scalar_slot_p(4))
                ]
            );
            for name in ["u[0]", "u[:]", "u[1,1]", "foreign.u[1]"] {
                assert_eq!(layout.binding(name), None);
            }
            assert_eq!(
                layout.scalar_name_for_slot(ScalarSlot::P {
                    index: 3,
                    byte_offset: 999
                }),
                None
            );
            let mut foreign_shape = wire.clone();
            foreign_shape["shapes"]["u"] = serde_json::json!([count + 1]);
            let replay: VarLayout = serde_json::from_value(foreign_shape).unwrap();
            assert!(matches!(
                replay.validate_shape_contract(),
                Err(VarLayoutShapeContractError::ShapeOutOfBounds { .. })
            ));
            let mut foreign_span = wire;
            foreign_span["shape_spans"]["v"] = foreign_span["shape_spans"]["u"].clone();
            let replay: VarLayout = serde_json::from_value(foreign_span).unwrap();
            assert!(matches!(
                replay.validate_shape_contract(),
                Err(VarLayoutShapeContractError::ShapeSpanWithoutShape { .. })
            ));
        }
    }
}
