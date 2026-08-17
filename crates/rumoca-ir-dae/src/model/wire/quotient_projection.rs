//! Serializer-side projection of dynamic-quotient owners.
//!
//! The verifier proves every registry entry owns exactly its canonical
//! batch before anything is omitted, and the stream outputs replace each
//! owner-produced relation, activation definition, and root with a
//! positional marker carrying only the owner ordinal.

use super::*;

/// Prove every registry entry owns exactly its canonical batch before the
/// serializer omits anything.
///
/// A model owner must claim nodes `q+1..=q+6` in the fixed generated order —
/// ratio, pi, phase, indicator, zero, relation expression — with generated
/// RuntimeDiscontinuity provenance, the exact inline ratio/phase/relation
/// wiring, the contiguous packed operand slots (quotient two, indicator one),
/// and its recorded relation/activation/root cross-referencing those exact
/// facts. A function owner must claim a Builtin quotient inside its recorded
/// function scope. Anything else fails serialization: the wire never omits a
/// range the registry has not proven it owns.
pub(super) fn verify_owner_projection(storage: &FrozenStorage) -> Result<OwnerProjection, String> {
    use crate::model::runtime_quotients::QuotientOwnerKind;

    // The registry is canonical in quotient order on every recording path;
    // the marker ordinals below are exactly its indices. A violation here
    // is a broken construction invariant, never something to repair by
    // sorting at the boundary.
    let canonical_order = storage
        .runtime_quotient_owners
        .windows(2)
        .all(|pair| pair[0].quotient < pair[1].quotient);
    if !canonical_order {
        return Err("owner registry violates canonical quotient order".into());
    }
    let sorted = &storage.runtime_quotient_owners;
    let mut projection = OwnerProjection {
        replace: rustc_hash::FxHashMap::default(),
        skip_nodes: rustc_hash::FxHashSet::default(),
        skip_operands: rustc_hash::FxHashSet::default(),
        relation_markers: rustc_hash::FxHashMap::default(),
        activation_markers: rustc_hash::FxHashMap::default(),
        root_markers: rustc_hash::FxHashMap::default(),
    };
    for (ordinal, entry) in sorted.iter().enumerate() {
        let (quotient_range, [lhs, rhs]) = verified_quotient_operands(storage, entry)?;
        if projection.replace.contains_key(&entry.quotient) {
            return Err("duplicate registry quotient".into());
        }
        let kind = match &entry.kind {
            QuotientOwnerKind::FunctionBody { function } => {
                let scope = storage
                    .expressions
                    .function_scopes
                    .get(entry.quotient as usize)
                    .copied()
                    .flatten();
                if scope != Some(*function) {
                    return Err("function owner quotient is outside its recorded body".into());
                }
                OwnerNodeKind::Function {
                    function: *function,
                }
            }
            QuotientOwnerKind::ModelEvent {
                generated,
                relation,
                activation,
                root,
            } => {
                let indicator_range = verify_model_owner_batch(
                    storage,
                    entry.quotient,
                    quotient_range,
                    [lhs, rhs],
                    generated,
                )?;
                verify_model_owner_surface(
                    storage,
                    entry.quotient,
                    *generated,
                    *relation,
                    *activation,
                    *root,
                )?;
                projection.skip_nodes.extend(*generated);
                projection.skip_operands.extend(indicator_range.indices());
                let claimed = projection
                    .relation_markers
                    .insert(*relation, ordinal as u32)
                    .is_some()
                    || projection
                        .activation_markers
                        .insert(*activation, ordinal as u32)
                        .is_some()
                    || projection
                        .root_markers
                        .insert(*root, ordinal as u32)
                        .is_some();
                if claimed {
                    return Err("two model owners claim one event artifact".into());
                }
                OwnerNodeKind::Model {
                    activation: *activation,
                }
            }
        };
        projection.skip_operands.extend(quotient_range.indices());
        projection.replace.insert(
            entry.quotient,
            OwnerNodeProjection {
                kind,
                builtin: entry.builtin,
                lhs,
                rhs,
            },
        );
    }
    Ok(projection)
}

fn verified_quotient_operands(
    storage: &FrozenStorage,
    entry: &crate::model::runtime_quotients::RuntimeQuotientOwnerEntry,
) -> Result<(OperandRange, [u32; 2]), String> {
    let arena = &storage.expressions;
    let Some(ExprNode::Builtin { builtin, operands }) = arena.nodes.get(entry.quotient as usize)
    else {
        return Err("registry quotient is not a builtin expression".into());
    };
    if *builtin != entry.builtin || operands.len != 2 {
        return Err("registry quotient does not match its recorded builtin".into());
    }
    let slots = &arena.operands[operands.indices()];
    Ok((*operands, [slots[0], slots[1]]))
}

/// Prove one model owner's six generated nodes have exactly the canonical
/// indicator shape, wiring, packed slots, and generated provenance; return
/// the indicator's packed operand range.
fn verify_model_owner_batch(
    storage: &FrozenStorage,
    quotient: u32,
    quotient_range: OperandRange,
    [lhs, rhs]: [u32; 2],
    generated: &[u32; 6],
) -> Result<OperandRange, String> {
    let arena = &storage.expressions;
    let expected: [u32; 6] = std::array::from_fn(|offset| quotient + 1 + offset as u32);
    if *generated != expected {
        return Err("model owner batch is not contiguous after its quotient".into());
    }
    let node = |index: u32| arena.nodes.get(index as usize);
    let generated_ok = matches!(
        node(expected[0]),
        Some(ExprNode::Binary {
            operator: BinaryOperator::Divide,
            lhs: ratio_lhs,
            rhs: ratio_rhs,
        }) if *ratio_lhs == lhs && *ratio_rhs == rhs
    ) && matches!(
        node(expected[1]),
        Some(ExprNode::Literal(DaeLiteral::Real(value)))
            if value.to_bits() == std::f64::consts::PI.to_bits()
    ) && matches!(
        node(expected[2]),
        Some(ExprNode::Binary {
            operator: BinaryOperator::Multiply,
            lhs: phase_lhs,
            rhs: phase_rhs,
        }) if *phase_lhs == expected[1] && *phase_rhs == expected[0]
    ) && matches!(
        node(expected[4]),
        Some(ExprNode::Literal(DaeLiteral::Real(value)))
            if value.to_bits() == 0.0_f64.to_bits()
    ) && matches!(
        node(expected[5]),
        Some(ExprNode::Binary {
            operator: BinaryOperator::GreaterEqual,
            lhs: relation_lhs,
            rhs: relation_rhs,
        }) if *relation_lhs == expected[3] && *relation_rhs == expected[4]
    );
    if !generated_ok {
        return Err("model owner batch violates the canonical indicator shape".into());
    }
    let Some(ExprNode::Builtin {
        builtin: PureBuiltin::Sin,
        operands: indicator_range,
    }) = node(expected[3])
    else {
        return Err("model owner batch violates the canonical indicator shape".into());
    };
    if indicator_range.len != 1
        || arena.operands[indicator_range.indices()] != [expected[2]]
        || indicator_range.start != quotient_range.start + 2
    {
        return Err("model owner batch does not own its packed operand slots".into());
    }
    let canonical = canonical_generated_provenance(storage, quotient)?;
    for &index in &expected {
        let generated_provenance = arena
            .provenance
            .get(index as usize)
            .ok_or("model owner batch escapes the arena")?;
        if *generated_provenance != canonical {
            return Err("model owner batch lacks the canonical generated provenance".into());
        }
    }
    Ok(*indicator_range)
}

/// The one generated provenance replay derives from the quotient record:
/// every omitted node and event artifact must carry exactly it, because the
/// wire drops their provenance columns and reconstruction re-derives them
/// from the quotient record's span alone.
fn canonical_generated_provenance(
    storage: &FrozenStorage,
    quotient: u32,
) -> Result<DaeProvenance, String> {
    let quotient_provenance = storage
        .expressions
        .provenance
        .get(quotient as usize)
        .ok_or("registry quotient escapes the arena")?;
    DaeProvenance::generated(
        DaeGeneration::RuntimeDiscontinuity,
        quotient_provenance.span(),
    )
    .map_err(|_| "registry quotient span cannot anchor generated provenance".into())
}

fn verify_model_owner_surface(
    storage: &FrozenStorage,
    quotient: u32,
    generated: [u32; 6],
    relation: u32,
    activation: u32,
    root: u32,
) -> Result<(), String> {
    let canonical = canonical_generated_provenance(storage, quotient)?;
    let relation_ok = storage
        .relations
        .get(relation as usize)
        .is_some_and(|entry| entry.expression == generated[5] && entry.provenance == canonical);
    let activation_ok = storage
        .conditions
        .get(activation as usize)
        .is_some_and(|entry| {
            matches!(entry.node, Some(crate::conditions::ConditionNode::Always))
                && entry.provenance == canonical
        });
    let root_ok = storage.roots.get(root as usize).is_some_and(|entry| {
        entry.relation == relation
            && entry.activation == activation
            && entry.provenance == canonical
    });
    if !relation_ok || !activation_ok || !root_ok {
        return Err("model owner event surface does not match its registry".into());
    }
    Ok(())
}

pub(super) struct RelationsOutput<'storage> {
    pub(super) relations: &'storage [RelationEntry],
    pub(super) markers: &'storage rustc_hash::FxHashMap<u32, u32>,
}

impl Serialize for RelationsOutput<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeSeq;
        let mut sequence = serializer.serialize_seq(Some(self.relations.len()))?;
        for (index, entry) in self.relations.iter().enumerate() {
            if let Some(&owner) = self.markers.get(&(index as u32)) {
                sequence.serialize_element(&StreamMarkerOutput {
                    name: "RelationEntryWire",
                    variant: 1,
                    owner,
                })?;
            } else {
                sequence.serialize_element(&RelationRecordOutput(entry))?;
            }
        }
        sequence.end()
    }
}

struct RelationRecordOutput<'storage>(&'storage RelationEntry);

impl Serialize for RelationRecordOutput<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state =
            serializer.serialize_struct_variant("RelationEntryWire", 0, "relation", 2)?;
        state.serialize_field("expression", &self.0.expression)?;
        state.serialize_field("provenance", &self.0.provenance)?;
        state.end()
    }
}

/// One positional owner marker: it occupies exactly one source stream
/// ordinal and carries only the owner-operation ordinal, never an output id.
struct StreamMarkerOutput {
    name: &'static str,
    variant: u32,
    owner: u32,
}

impl Serialize for StreamMarkerOutput {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state =
            serializer.serialize_struct_variant(self.name, self.variant, "quotient_owner", 1)?;
        state.serialize_field("owner", &self.owner)?;
        state.end()
    }
}

pub(super) struct RootsOutput<'storage> {
    pub(super) roots: &'storage [RootEntry],
    pub(super) markers: &'storage rustc_hash::FxHashMap<u32, u32>,
}

impl Serialize for RootsOutput<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeSeq;
        let mut sequence = serializer.serialize_seq(Some(self.roots.len()))?;
        for (index, entry) in self.roots.iter().enumerate() {
            if let Some(&owner) = self.markers.get(&(index as u32)) {
                sequence.serialize_element(&StreamMarkerOutput {
                    name: "RootEntryWire",
                    variant: 1,
                    owner,
                })?;
            } else {
                sequence.serialize_element(&RootRecordOutput(entry))?;
            }
        }
        sequence.end()
    }
}

struct RootRecordOutput<'storage>(&'storage RootEntry);

impl Serialize for RootRecordOutput<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct_variant("RootEntryWire", 0, "root", 3)?;
        state.serialize_field("relation", &self.0.relation)?;
        state.serialize_field("activation", &self.0.activation)?;
        state.serialize_field("provenance", &self.0.provenance)?;
        state.end()
    }
}

pub(super) struct ConditionsOutput<'storage> {
    pub(super) conditions: &'storage [ConditionEntry],
    pub(super) markers: &'storage rustc_hash::FxHashMap<u32, u32>,
}

impl Serialize for ConditionsOutput<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeSeq;
        let mut sequence = serializer.serialize_seq(Some(self.conditions.len()))?;
        for (index, entry) in self.conditions.iter().enumerate() {
            sequence.serialize_element(&ConditionEntryOutput {
                entry,
                owner: self.markers.get(&(index as u32)).copied(),
            })?;
        }
        sequence.end()
    }
}

struct ConditionEntryOutput<'storage> {
    entry: &'storage ConditionEntry,
    owner: Option<u32>,
}

impl Serialize for ConditionEntryOutput<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("ConditionEntry", 2)?;
        match self.owner {
            Some(owner) => {
                state.serialize_field("node", &Some(ConditionMarkerNodeOutput(owner)))?
            }
            None => state.serialize_field("node", &self.entry.node)?,
        }
        state.serialize_field("provenance", &self.entry.provenance)?;
        state.end()
    }
}

/// The owner's activation definition marker, at the reserved ordinal's
/// definition position. Variant ordinal 9 follows `AnyRise` in
/// [`ConditionNodeWire`]'s declaration order.
struct ConditionMarkerNodeOutput(u32);

impl Serialize for ConditionMarkerNodeOutput {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_newtype_variant("ConditionNode", 9, "quotient_owner", &self.0)
    }
}
