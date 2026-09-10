use super::*;
use serde::ser::{SerializeSeq, SerializeStruct, SerializeStructVariant};
use serde::{Deserialize, Serialize};

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct ExpressionArenaWire {
    pub(super) nodes: Vec<ExprNodeWire>,
    #[serde(deserialize_with = "deserialize_provenance_vec")]
    pub(super) provenance: Vec<DaeProvenance>,
    pub(super) operands: Vec<u32>,
    pub(super) subscripts: Vec<PackedSubscriptWire>,
}

pub(super) struct WireExpression<'wire> {
    pub(super) node: &'wire ExprNodeWire,
    pub(super) provenance: DaeProvenance,
}

pub(super) fn wire_expression(
    wire: &StorageWire,
    index: usize,
) -> Result<WireExpression<'_>, DaeConstructionError> {
    Ok(WireExpression {
        node: wire
            .expressions
            .nodes
            .get(index)
            .ok_or_else(|| malformed("expressions.nodes"))?,
        provenance: *wire
            .expressions
            .provenance
            .get(index)
            .ok_or_else(|| malformed("expressions.provenance"))?,
    })
}

/// The serializer-side projection of one dynamic-quotient owner.
///
/// `verify_owner_batches` proves every registry entry owns exactly its
/// canonical node/provenance/operand ranges before anything is omitted; a
/// registry that does not own what it claims fails serialization.
pub(super) struct OwnerNodeProjection {
    pub(super) kind: OwnerNodeKind,
    pub(super) builtin: PureBuiltin,
    pub(super) lhs: u32,
    pub(super) rhs: u32,
}

pub(super) enum OwnerNodeKind {
    Model { activation: u32 },
    Function { function: u32 },
}

/// Per-arena omission plan, computed once from the verified registry.
pub(super) struct OwnerProjection {
    /// Quotient node index → owner record to emit in its place.
    pub(super) replace: rustc_hash::FxHashMap<u32, OwnerNodeProjection>,
    /// Generated node indices omitted from the wire entirely.
    pub(super) skip_nodes: rustc_hash::FxHashSet<u32>,
    /// Packed operand positions omitted from the wire entirely.
    pub(super) skip_operands: rustc_hash::FxHashSet<usize>,
    /// Relation ordinal → owner ordinal marker.
    pub(super) relation_markers: rustc_hash::FxHashMap<u32, u32>,
    /// Activation condition ordinal → owner ordinal marker.
    pub(super) activation_markers: rustc_hash::FxHashMap<u32, u32>,
    /// Root ordinal → owner ordinal marker.
    pub(super) root_markers: rustc_hash::FxHashMap<u32, u32>,
}

pub(super) struct ExpressionArenaOutput<'storage> {
    pub(super) arena: &'storage FrozenExpressionArenaStorage,
    pub(super) projection: &'storage OwnerProjection,
}

impl Serialize for ExpressionArenaOutput<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("ExpressionArena", 4)?;
        state.serialize_field(
            "nodes",
            &ExpressionNodesOutput {
                arena: self.arena,
                projection: self.projection,
            },
        )?;
        state.serialize_field(
            "provenance",
            &FilteredSeqOutput {
                values: &self.arena.provenance,
                skip: &self.projection.skip_nodes,
            },
        )?;
        state.serialize_field(
            "operands",
            &FilteredOperandsOutput {
                values: &self.arena.operands,
                skip: &self.projection.skip_operands,
            },
        )?;
        state.serialize_field("subscripts", &self.arena.subscripts)?;
        state.end()
    }
}

struct FilteredSeqOutput<'storage, T> {
    values: &'storage [T],
    skip: &'storage rustc_hash::FxHashSet<u32>,
}

impl<T: Serialize> Serialize for FilteredSeqOutput<'_, T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut sequence = serializer.serialize_seq(Some(self.values.len() - self.skip.len()))?;
        for (index, value) in self.values.iter().enumerate() {
            if self.skip.contains(&(index as u32)) {
                continue;
            }
            sequence.serialize_element(value)?;
        }
        sequence.end()
    }
}

struct FilteredOperandsOutput<'storage> {
    values: &'storage [u32],
    skip: &'storage rustc_hash::FxHashSet<usize>,
}

impl Serialize for FilteredOperandsOutput<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut sequence = serializer.serialize_seq(Some(self.values.len() - self.skip.len()))?;
        for (index, value) in self.values.iter().enumerate() {
            if self.skip.contains(&index) {
                continue;
            }
            sequence.serialize_element(value)?;
        }
        sequence.end()
    }
}

struct ExpressionNodesOutput<'storage> {
    arena: &'storage FrozenExpressionArenaStorage,
    projection: &'storage OwnerProjection,
}

impl Serialize for ExpressionNodesOutput<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let count = self.arena.nodes.len() - self.projection.skip_nodes.len();
        let mut sequence = serializer.serialize_seq(Some(count))?;
        for (index, (node, value_type)) in self
            .arena
            .nodes
            .iter()
            .zip(&self.arena.value_types)
            .enumerate()
        {
            let raw = index as u32;
            if self.projection.skip_nodes.contains(&raw) {
                continue;
            }
            if let Some(owner) = self.projection.replace.get(&raw) {
                sequence.serialize_element(&OwnerRecordOutput(owner))?;
                continue;
            }
            sequence.serialize_element(&ExpressionNodeOutput {
                index,
                node,
                value_type: *value_type,
            })?;
        }
        sequence.end()
    }
}

struct OwnerRecordOutput<'storage>(&'storage OwnerNodeProjection);

impl Serialize for OwnerRecordOutput<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct_variant(
            "ExprNode",
            RUNTIME_QUOTIENT_OWNER_VARIANT,
            "runtime_quotient_owner",
            4,
        )?;
        state.serialize_field("kind", &QuotientOwnerKindOutput(&self.0.kind))?;
        state.serialize_field("builtin", &self.0.builtin)?;
        state.serialize_field("lhs", &self.0.lhs)?;
        state.serialize_field("rhs", &self.0.rhs)?;
        state.end()
    }
}

struct QuotientOwnerKindOutput<'storage>(&'storage OwnerNodeKind);

impl Serialize for QuotientOwnerKindOutput<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self.0 {
            OwnerNodeKind::Model { activation } => {
                let mut state =
                    serializer.serialize_struct_variant("QuotientOwnerKindWire", 0, "model", 1)?;
                state.serialize_field("activation", activation)?;
                state.end()
            }
            OwnerNodeKind::Function { function } => {
                let mut state = serializer.serialize_struct_variant(
                    "QuotientOwnerKindWire",
                    1,
                    "function",
                    1,
                )?;
                state.serialize_field("function", function)?;
                state.end()
            }
        }
    }
}

/// Wire-local variant indices of the projected arena nodes.
///
/// Ordinal-tagged encodings identify a variant positionally, so a projected
/// node must carry the index its own arena enum declares and [`ExprNodeWire`]
/// decodes. The two enums declare the same variants in the same order, and the
/// binary round trips in `wire_omits_constructor_derived_facts_and_round_trips_canonically`
/// and `wire_omits_generated_fold_facts_and_replays_them_through_construction`
/// cover every projected variant, so drift fails loudly instead of silently.
const ARRAY_VARIANT: u32 = 5;
const RECORD_VARIANT: u32 = 6;
const CALL_VARIANT: u32 = 13;
const FUNCTION_FOLD_PARAMETER_VARIANT: u32 = 16;
const FUNCTION_FOLD_OUTPUT_VARIANT: u32 = 17;
const RUNTIME_QUOTIENT_OWNER_VARIANT: u32 = 19;

/// One arena node projected onto the operation that built it.
///
/// A record and an empty array cannot infer their own type from operands, so
/// the type they were constructed with travels as an operand of that node. A
/// generated fold parameter or output carries only its owning function: every
/// other fact those nodes hold is re-issued by the fold transition that
/// generates them, so replay recomputes it instead of reading it.
struct ExpressionNodeOutput<'storage> {
    index: usize,
    node: &'storage ExprNode,
    value_type: u32,
}

impl Serialize for ExpressionNodeOutput<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self.node {
            ExprNode::Array { operands } => {
                let anchor = (operands.len == 0).then_some(self.value_type);
                let mut state =
                    serializer.serialize_struct_variant("ExprNode", ARRAY_VARIANT, "array", 2)?;
                state.serialize_field("operand_count", operands)?;
                state.serialize_field("value_type", &anchor)?;
                state.end()
            }
            ExprNode::Record { operands } => {
                let mut state =
                    serializer.serialize_struct_variant("ExprNode", RECORD_VARIANT, "record", 2)?;
                state.serialize_field("operand_count", operands)?;
                state.serialize_field("value_type", &self.value_type)?;
                state.end()
            }
            ExprNode::Call {
                owner,
                function,
                output,
                operands,
            } => {
                let operand_count = if *owner as usize == self.index {
                    operands.len
                } else {
                    0
                };
                let mut state =
                    serializer.serialize_struct_variant("ExprNode", CALL_VARIANT, "call", 4)?;
                state.serialize_field("owner", owner)?;
                state.serialize_field("function", function)?;
                state.serialize_field("output", output)?;
                state.serialize_field("operand_count", &operand_count)?;
                state.end()
            }
            ExprNode::FunctionFoldParameter { function, .. } => {
                let mut state = serializer.serialize_struct_variant(
                    "ExprNode",
                    FUNCTION_FOLD_PARAMETER_VARIANT,
                    "function_fold_parameter",
                    1,
                )?;
                state.serialize_field("function", function)?;
                state.end()
            }
            ExprNode::FunctionFoldOutput { function, .. } => {
                let mut state = serializer.serialize_struct_variant(
                    "ExprNode",
                    FUNCTION_FOLD_OUTPUT_VARIANT,
                    "function_fold_output",
                    1,
                )?;
                state.serialize_field("function", function)?;
                state.end()
            }
            node => node.serialize(serializer),
        }
    }
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(super) enum ExprNodeWire {
    Literal(DaeLiteralWire),
    Coordinate(CoordinateWire),
    Unary {
        operator: UnaryOperator,
        operand: u32,
    },
    Binary {
        operator: BinaryOperator,
        lhs: u32,
        rhs: u32,
    },
    Conditional {
        operand_count: u32,
    },
    Array {
        operand_count: u32,
        /// The constructed element type an empty array cannot infer.
        #[serde(deserialize_with = "deserialize_required_option")]
        value_type: Option<u32>,
    },
    Record {
        operand_count: u32,
        value_type: u32,
    },
    Field {
        base: u32,
        field: u32,
    },
    Range {
        start_expression: u32,
        #[serde(deserialize_with = "deserialize_required_option")]
        explicit_step_expression: Option<u32>,
        stop_expression: u32,
    },
    Comprehension {
        domain: u32,
        body: u32,
    },
    Index {
        base: u32,
        subscript_count: u32,
    },
    ArrayUpdate {
        base: u32,
        value: u32,
        subscript_count: u32,
    },
    Builtin {
        builtin: PureBuiltin,
        operand_count: u32,
    },
    Call {
        owner: u32,
        function: u32,
        output: u32,
        operand_count: u32,
    },
    StringConversion {
        declaration: rumoca_core::DefId,
        value: u32,
        #[serde(deserialize_with = "deserialize_required_option")]
        minimum_length: Option<u32>,
        #[serde(deserialize_with = "deserialize_required_option")]
        left_justified: Option<u32>,
        #[serde(deserialize_with = "deserialize_required_option")]
        significant_digits: Option<u32>,
        #[serde(deserialize_with = "deserialize_required_option")]
        format: Option<u32>,
    },
    FunctionValue {
        function: u32,
        value: u32,
        definition_ordinal: u32,
    },
    FunctionFoldParameter {
        function: u32,
    },
    FunctionFoldOutput {
        function: u32,
    },
    ClockTransfer {
        kind: crate::ClockTransferKind,
        source: u32,
        source_clock: u32,
        target_clock: u32,
    },
    /// One owner-marked dynamic quotient operation.
    ///
    /// The record carries only semantic inputs; every owner-produced
    /// identity — the model kind's six generated indicator nodes, relation,
    /// activation definition, and root — is absent from the wire and
    /// regenerated by the staged replay. A model record advances the source
    /// expression ordinal by seven; a function record by one.
    RuntimeQuotientOwner {
        kind: QuotientOwnerKindWire,
        builtin: PureBuiltin,
        lhs: u32,
        rhs: u32,
    },
}

#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(super) enum QuotientOwnerKindWire {
    /// `activation` is the owner's pre-reserved condition ordinal — an
    /// input-side reservation identity, not an operation output.
    Model {
        activation: u32,
    },
    Function {
        function: u32,
    },
}

fn deserialize_required_option<'de, D>(deserializer: D) -> Result<Option<u32>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    Option::<u32>::deserialize(deserializer)
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(super) enum DaeLiteralWire {
    Real(f64),
    Integer(i64),
    Enumeration(i64),
    Boolean(bool),
    String(String),
}

impl DaeLiteralWire {
    pub(super) fn as_literal(&self) -> DaeLiteral {
        match self {
            Self::Real(value) => DaeLiteral::Real(*value),
            Self::Integer(value) => DaeLiteral::Integer(*value),
            Self::Enumeration(value) => DaeLiteral::Enumeration(*value),
            Self::Boolean(value) => DaeLiteral::Boolean(*value),
            Self::String(value) => DaeLiteral::String(value.clone()),
        }
    }
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(super) enum CoordinateWire {
    Parameter(u32),
    Input(u32),
    State(u32),
    Derivative(u32),
    Algebraic(u32),
    DiscreteReal(u32),
    DiscreteValue(u32),
    PreDiscreteReal(u32),
    PreDiscreteValue(u32),
    Time,
    ClockInterval(u32),
    Condition(u32),
    Delay(u32),
    Previous(u32),
    Terminal(u32),
    Binder { domain: u32, ordinal: u32 },
    FunctionParameter { function: u32, ordinal: u32 },
    // Declaration order mirrors `Coordinate` in `expression/nodes.rs`, which is
    // the encoded form; ordinal-tagged codecs pair them positionally.
    PreState(u32),
    PreAlgebraic(u32),
}

#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(super) enum PackedSubscriptKindWire {
    Index(u32),
    Whole,
    Slice(u32),
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct PackedSubscriptWire {
    pub(super) kind: PackedSubscriptKindWire,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}
