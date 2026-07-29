use super::*;

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct ExpressionArenaWire {
    pub(super) nodes: Vec<ExprNodeWire>,
    #[serde(deserialize_with = "deserialize_provenance_vec")]
    pub(super) provenance: Vec<DaeProvenance>,
    pub(super) value_types: Vec<u32>,
    pub(super) variability: Vec<ExpressionVariability>,
    pub(super) binder_domains: Vec<Option<u32>>,
    pub(super) function_scopes: Vec<Option<u32>>,
    pub(super) operands: Vec<u32>,
    pub(super) subscripts: Vec<PackedSubscriptWire>,
}

pub(super) struct WireExpression<'wire> {
    pub(super) node: &'wire ExprNodeWire,
    pub(super) provenance: DaeProvenance,
    pub(super) value_type: u32,
    pub(super) variability: ExpressionVariability,
    pub(super) binder_domain: Option<u32>,
    pub(super) function_scope: Option<u32>,
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
        value_type: *wire
            .expressions
            .value_types
            .get(index)
            .ok_or_else(|| malformed("expressions.value_types"))?,
        variability: *wire
            .expressions
            .variability
            .get(index)
            .ok_or_else(|| malformed("expressions.variability"))?,
        binder_domain: *wire
            .expressions
            .binder_domains
            .get(index)
            .ok_or_else(|| malformed("expressions.binder_domains"))?,
        function_scope: *wire
            .expressions
            .function_scopes
            .get(index)
            .ok_or_else(|| malformed("expressions.function_scopes"))?,
    })
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
        operands: OperandRangeWire,
    },
    Array {
        operands: OperandRangeWire,
    },
    Record {
        operands: OperandRangeWire,
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
        subscripts: OperandRangeWire,
    },
    ArrayUpdate {
        base: u32,
        value: u32,
        subscripts: OperandRangeWire,
    },
    Builtin {
        builtin: PureBuiltin,
        operands: OperandRangeWire,
    },
    Call {
        function: u32,
        output: u32,
        operands: OperandRangeWire,
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
#[serde(deny_unknown_fields)]
pub(super) struct OperandRangeWire {
    pub(super) start: u32,
    pub(super) len: u32,
}

impl OperandRangeWire {
    pub(super) fn indices(self) -> Option<std::ops::Range<usize>> {
        let start = self.start as usize;
        let end = start.checked_add(self.len as usize)?;
        Some(start..end)
    }
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
