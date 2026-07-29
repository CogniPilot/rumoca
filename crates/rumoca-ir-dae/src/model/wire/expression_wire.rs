use super::*;
use serde::ser::SerializeStruct;
use serde::{Deserialize, Serialize};

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct ExpressionArenaWire {
    pub(super) nodes: Vec<ExprNodeWire>,
    #[serde(deserialize_with = "deserialize_provenance_vec")]
    pub(super) provenance: Vec<DaeProvenance>,
    pub(super) type_anchors: Vec<ExpressionTypeAnchorWire>,
    pub(super) operands: Vec<u32>,
    pub(super) subscripts: Vec<PackedSubscriptWire>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct ExpressionTypeAnchorWire {
    pub(super) expression: u32,
    pub(super) value_type: u32,
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

#[derive(Serialize)]
struct ExpressionTypeAnchorOutput {
    expression: u32,
    value_type: u32,
}

impl Serialize for FrozenExpressionArenaStorage {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut type_anchors = Vec::new();
        for (index, (node, value_type)) in self.nodes.iter().zip(&self.value_types).enumerate() {
            if expression_requires_type_anchor(node) {
                let expression = u32::try_from(index).map_err(serde::ser::Error::custom)?;
                type_anchors.push(ExpressionTypeAnchorOutput {
                    expression,
                    value_type: *value_type,
                });
            }
        }
        let mut state = serializer.serialize_struct("ExpressionArena", 5)?;
        state.serialize_field("nodes", &self.nodes)?;
        state.serialize_field("provenance", &self.provenance)?;
        state.serialize_field("type_anchors", &type_anchors)?;
        state.serialize_field("operands", &self.operands)?;
        state.serialize_field("subscripts", &self.subscripts)?;
        state.end()
    }
}

fn expression_requires_type_anchor(node: &ExprNode) -> bool {
    matches!(node, ExprNode::Record { .. })
        || matches!(node, ExprNode::Array { operands } if operands.len == 0)
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
    },
    Record {
        operand_count: u32,
    },
    Field {
        base: u32,
        field: u32,
    },
    Range {
        start: i64,
        step: i64,
        stop: i64,
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
        function: u32,
        output: u32,
        operand_count: u32,
    },
    FunctionValue {
        function: u32,
        value: u32,
        definition_ordinal: u32,
    },
    FunctionFoldParameter {
        function: u32,
        fold: u32,
        carried: u32,
        definition_ordinal: u32,
    },
    FunctionFoldOutput {
        function: u32,
        fold: u32,
        carried: u32,
        definition_ordinal: u32,
    },
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
    Condition(u32),
    Delay(u32),
    Previous(u32),
    Terminal(u32),
    Binder { domain: u32, ordinal: u32 },
    FunctionParameter { function: u32, ordinal: u32 },
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
