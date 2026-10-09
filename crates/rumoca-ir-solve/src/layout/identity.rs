//! Exact immutable layout comparison for retained execution hooks.

use super::{IndexedScalarSlot, ScalarSlot, VarLayout};

fn same_slot(left: &ScalarSlot, right: &ScalarSlot) -> bool {
    match (left, right) {
        (ScalarSlot::Constant(left), ScalarSlot::Constant(right)) => {
            left.to_bits() == right.to_bits()
        }
        (ScalarSlot::Constant(_), _) | (_, ScalarSlot::Constant(_)) => false,
        _ => left == right,
    }
}

fn same_indexed(left: &[IndexedScalarSlot], right: &[IndexedScalarSlot]) -> bool {
    left.len() == right.len()
        && left.iter().zip(right).all(|(left, right)| {
            left.indices == right.indices && same_slot(&left.slot, &right.slot)
        })
}

impl VarLayout {
    /// Whether both layouts retain the exact ordered address, shape and
    /// provenance context, including IEEE constant payloads and indexed views.
    /// This query neither derives scalar slots nor serializes the layout.
    pub fn same_execution_layout(&self, other: &Self) -> bool {
        self.y_scalars == other.y_scalars
            && self.p_scalars == other.p_scalars
            && self.bindings.len() == other.bindings.len()
            && self.bindings.iter().zip(&other.bindings).all(
                |((name, slot), (other_name, other_slot))| {
                    name == other_name && same_slot(slot, other_slot)
                },
            )
            && self.shapes.iter().eq(other.shapes.iter())
            && self.shape_spans.iter().eq(other.shape_spans.iter())
            && self
                .shape_indexed_keys
                .iter()
                .eq(other.shape_indexed_keys.iter())
            && self.indexed_bindings.len() == other.indexed_bindings.len()
            && self
                .indexed_bindings
                .iter()
                .zip(&other.indexed_bindings)
                .all(|((key, slots), (other_key, other_slots))| {
                    key == other_key && same_indexed(slots, other_slots)
                })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn constant(value: f64) -> VarLayout {
        VarLayout::from_parts(
            [(String::from("k"), ScalarSlot::Constant(value))].into(),
            0,
            0,
        )
    }

    #[test]
    fn exact_layout_keeps_signed_zero_and_nan_payloads() {
        let negative_zero = constant(-0.0);
        assert!(negative_zero.same_execution_layout(&negative_zero.clone()));
        assert!(!negative_zero.same_execution_layout(&constant(0.0)));
        let nan = f64::from_bits(0x7ff8_0000_0000_0001);
        assert!(constant(nan).same_execution_layout(&constant(nan)));
        assert!(!constant(nan).same_execution_layout(&constant(f64::from_bits(nan.to_bits() + 1))));
    }

    #[test]
    fn exact_layout_includes_authored_indexed_constant_slots() {
        let layout = |value| {
            VarLayout::from_parts_with_shapes_and_indexed_bindings(
                [(String::from("v"), ScalarSlot::Constant(0.0))].into(),
                [(String::from("v"), vec![2])].into(),
                [(
                    super::super::ComponentReferenceKey::generated("v"),
                    vec![
                        IndexedScalarSlot {
                            indices: vec![1],
                            slot: ScalarSlot::Constant(3.0),
                        },
                        IndexedScalarSlot {
                            indices: vec![2],
                            slot: ScalarSlot::Constant(value),
                        },
                    ],
                )]
                .into(),
                0,
                0,
            )
            .unwrap()
        };
        let source = layout(-0.0);
        let foreign = layout(0.0);
        assert!(source.same_execution_layout(&source.clone()));
        assert!(!source.same_execution_layout(&foreign));
        assert_eq!(
            serde_json::to_value(&source).unwrap(),
            serde_json::to_value(&foreign).unwrap(),
            "current serialization omits indexed caches and cannot prove this execution context"
        );
    }
}
