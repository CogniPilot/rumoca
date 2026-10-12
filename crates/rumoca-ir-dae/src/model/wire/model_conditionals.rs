//! Replay lexical model-arm operations through their checked conditional owner.

use super::*;

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct ModelConditionalInput {
    condition: u32,
    guards: [u32; 2],
    #[serde(deserialize_with = "deserialize_parent")]
    parent: Option<u32>,
    expression: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

pub(super) struct ModelConditionalsOutput<'a>(pub(super) &'a FrozenStorage);

impl Serialize for ModelConditionalsOutput<'_> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        #[derive(Serialize)]
        struct Operation {
            condition: u32,
            guards: [u32; 2],
            parent: Option<u32>,
            expression: u32,
            provenance: DaeProvenance,
        }
        use serde::ser::SerializeSeq;
        let mut sequence = serializer.serialize_seq(Some(self.0.model_conditionals.len()))?;
        for entry in &self.0.model_conditionals {
            let arms = entry.arms.map(|arm| &self.0.model_arms[arm as usize]);
            sequence.serialize_element(&Operation {
                condition: entry.condition,
                guards: arms.map(|arm| arm.guard),
                parent: arms[0].parent,
                expression: entry.expression,
                provenance: entry.provenance,
            })?;
        }
        sequence.end()
    }
}

/// Enter the context named by this operation, issuing only ready owner inputs.
pub(super) fn prepare<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    context: Option<u32>,
    closing: Option<usize>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let needed = context
        .map(|arm| arm as usize / 2)
        .into_iter()
        .chain(closing)
        .max();
    if let Some(needed) = needed {
        while ids.model_conditionals.len() <= needed {
            let ordinal = ids.model_conditionals.len();
            let entry = wire
                .model_conditionals
                .get(ordinal)
                .ok_or_else(|| malformed("model conditionals"))?;
            let parent = entry
                .parent
                .map(|arm| mapped_arm(ids, arm, entry.provenance))
                .transpose()?;
            dae.replay_model_arm_context(parent, entry.provenance)?;
            let condition = mapped(
                &ids.expressions,
                entry.condition,
                "model conditional condition",
                entry.provenance,
            )?;
            let guards = entry.guards.map(|guard| {
                mapped(
                    &ids.expressions,
                    guard,
                    "model conditional guard",
                    entry.provenance,
                )
            });
            let [then_guard, else_guard] = guards;
            let owner = dae.replay_model_conditional(
                condition,
                [then_guard?, else_guard?],
                entry.provenance,
            )?;
            ids.model_arms
                .extend([owner.arm(dae, true), owner.arm(dae, false)]);
            ids.model_conditionals.push(Some(owner));
        }
    }
    let context = context
        .map(|arm| mapped_arm(ids, arm, provenance))
        .transpose()?;
    dae.replay_model_arm_context(context, provenance)
}

fn mapped_arm<'dae>(
    ids: &WireIds<'dae>,
    raw: u32,
    at: DaeProvenance,
) -> Result<ModelArm<'dae>, DaeConstructionError> {
    ids.model_arms
        .get(raw as usize)
        .copied()
        .ok_or_else(|| unknown("model arm", raw, at))
}

pub(super) fn close<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    owner: usize,
    operand_count: u32,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    if operand_count != 3 {
        return Err(malformed("model conditional operands"));
    }
    let operands = wire_operands(wire, &mut ids.next_operand, operand_count)?;
    let entry = &wire.model_conditionals[owner];
    if operands[0] != entry.condition || entry.provenance != provenance {
        return Err(malformed("model conditional condition"));
    }
    let values = [
        mapped(
            &ids.expressions,
            operands[1],
            "model conditional arm",
            provenance,
        )?,
        mapped(
            &ids.expressions,
            operands[2],
            "model conditional arm",
            provenance,
        )?,
    ];
    let token = ids.model_conditionals[owner]
        .take()
        .ok_or_else(|| malformed("closed model conditional"))?;
    dae.finish_model_conditional(token, values)
}

pub(super) fn closing_owners(
    wire: &StorageWire,
) -> Result<rustc_hash::FxHashMap<u32, usize>, DaeConstructionError> {
    let mut closes = rustc_hash::FxHashMap::default();
    for (ordinal, entry) in wire.model_conditionals.iter().enumerate() {
        if closes.insert(entry.expression, ordinal).is_some() {
            return Err(malformed("duplicate model conditional close"));
        }
    }
    Ok(closes)
}

fn deserialize_parent<'de, D: serde::Deserializer<'de>>(
    deserializer: D,
) -> Result<Option<u32>, D::Error> {
    Option::<u32>::deserialize(deserializer)
}
