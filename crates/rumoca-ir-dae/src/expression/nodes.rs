use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum UnaryOperator {
    Plus,
    Negate,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
    ElementwiseAdd,
    ElementwiseSubtract,
    ElementwiseMultiply,
    ElementwiseDivide,
    ElementwisePower,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PureBuiltin {
    Abs,
    Sign,
    Sqrt,
    Div,
    Mod,
    Rem,
    Floor,
    Ceil,
    Integer,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Atan2,
    Sinh,
    Cosh,
    Tanh,
    Exp,
    Log,
    Log10,
    Smooth,
    NoEvent,
    Homotopy,
    Min,
    Max,
    Sum,
    Product,
    Size,
    Zeros,
    Ones,
    Fill,
    Linspace,
    Cross,
}

impl PureBuiltin {
    pub(super) fn has_shaped_result(self) -> bool {
        matches!(
            self,
            Self::Zeros | Self::Ones | Self::Fill | Self::Linspace | Self::Cross
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum Coordinate {
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoordinateInput<'dae> {
    Parameter(ParameterId<'dae>),
    Input(InputId<'dae>),
    State(StateId<'dae>),
    Derivative(StateId<'dae>),
    Algebraic(AlgebraicId<'dae>),
    DiscreteReal(DiscreteRealId<'dae>),
    DiscreteValue(DiscreteValueId<'dae>),
    PreDiscreteReal(DiscreteRealId<'dae>),
    PreDiscreteValue(DiscreteValueId<'dae>),
    Time,
    ClockInterval(PeriodicClockId<'dae>),
    Condition(crate::ConditionId<'dae>),
    Previous(crate::PreviousId<'dae>),
    Terminal(crate::TerminalId<'dae>),
    FunctionParameter(FunctionParameterId<'dae>),
}

impl CoordinateInput<'_> {
    pub(super) fn erase(self) -> Coordinate {
        match self {
            Self::Parameter(id) => Coordinate::Parameter(id.index()),
            Self::Input(id) => Coordinate::Input(id.index()),
            Self::State(id) => Coordinate::State(id.index()),
            Self::Derivative(id) => Coordinate::Derivative(id.index()),
            Self::Algebraic(id) => Coordinate::Algebraic(id.index()),
            Self::DiscreteReal(id) => Coordinate::DiscreteReal(id.index()),
            Self::DiscreteValue(id) => Coordinate::DiscreteValue(id.index()),
            Self::PreDiscreteReal(id) => Coordinate::PreDiscreteReal(id.index()),
            Self::PreDiscreteValue(id) => Coordinate::PreDiscreteValue(id.index()),
            Self::Time => Coordinate::Time,
            Self::ClockInterval(id) => Coordinate::ClockInterval(id.index()),
            Self::Condition(id) => Coordinate::Condition(id.index()),
            Self::Previous(id) => Coordinate::Previous(id.index()),
            Self::Terminal(id) => Coordinate::Terminal(id.index()),
            Self::FunctionParameter(id) => Coordinate::FunctionParameter {
                function: id.function().index(),
                ordinal: id.ordinal(),
            },
        }
    }
}
#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ExprNode {
    Literal(DaeLiteral),
    Coordinate(Coordinate),
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
        #[serde(rename = "operand_count")]
        operands: OperandRange,
    },
    Array {
        #[serde(rename = "operand_count")]
        operands: OperandRange,
    },
    Record {
        #[serde(rename = "operand_count")]
        operands: OperandRange,
    },
    Field {
        base: u32,
        field: u32,
    },
    Range {
        #[serde(rename = "start_expression")]
        start: u32,
        #[serde(rename = "explicit_step_expression")]
        explicit_step: Option<u32>,
        #[serde(rename = "stop_expression")]
        stop: u32,
    },
    Comprehension {
        domain: u32,
        body: u32,
    },
    Index {
        base: u32,
        #[serde(rename = "subscript_count")]
        subscripts: OperandRange,
    },
    ArrayUpdate {
        base: u32,
        value: u32,
        #[serde(rename = "subscript_count")]
        subscripts: OperandRange,
    },
    Builtin {
        builtin: PureBuiltin,
        #[serde(rename = "operand_count")]
        operands: OperandRange,
    },
    Call {
        function: u32,
        output: u32,
        #[serde(rename = "operand_count")]
        operands: OperandRange,
    },
    StringConversion {
        declaration: rumoca_core::DefId,
        value: u32,
        minimum_length: Option<u32>,
        left_justified: Option<u32>,
        significant_digits: Option<u32>,
        format: Option<u32>,
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

impl ExprNode {
    pub(crate) fn for_each_child(
        &self,
        expressions: &ExpressionArenaStorage,
        mut visit: impl FnMut(u32),
    ) {
        match self {
            Self::Literal(_)
            | Self::Coordinate(_)
            | Self::FunctionValue { .. }
            | Self::FunctionFoldParameter { .. }
            | Self::FunctionFoldOutput { .. } => {}
            Self::Unary { operand, .. }
            | Self::Field { base: operand, .. }
            | Self::Comprehension { body: operand, .. } => visit(*operand),
            Self::Binary { lhs, rhs, .. } => {
                visit(*lhs);
                visit(*rhs);
            }
            Self::Range {
                start,
                explicit_step,
                stop,
            } => {
                visit(*start);
                if let Some(step) = explicit_step {
                    visit(*step);
                }
                visit(*stop);
            }
            Self::Conditional { operands }
            | Self::Array { operands }
            | Self::Record { operands }
            | Self::Builtin { operands, .. }
            | Self::Call { operands, .. } => {
                for &operand in &expressions.operands[operands.indices()] {
                    visit(operand);
                }
            }
            Self::Index { base, subscripts } => {
                visit(*base);
                visit_subscript_children(expressions, *subscripts, &mut visit);
            }
            Self::ArrayUpdate {
                base,
                value,
                subscripts,
            } => {
                visit(*base);
                visit(*value);
                visit_subscript_children(expressions, *subscripts, &mut visit);
            }
            Self::StringConversion {
                value,
                minimum_length,
                left_justified,
                significant_digits,
                format,
                ..
            } => {
                visit(*value);
                if let Some(minimum_length) = minimum_length {
                    visit(*minimum_length);
                }
                if let Some(left_justified) = left_justified {
                    visit(*left_justified);
                }
                if let Some(significant_digits) = significant_digits {
                    visit(*significant_digits);
                }
                if let Some(format) = format {
                    visit(*format);
                }
            }
        }
    }
}

fn visit_subscript_children(
    expressions: &ExpressionArenaStorage,
    subscripts: OperandRange,
    visit: &mut impl FnMut(u32),
) {
    for subscript in &expressions.subscripts[subscripts.indices()] {
        match subscript.kind {
            PackedSubscriptKind::Index(expression) | PackedSubscriptKind::Slice(expression) => {
                visit(expression);
            }
            PackedSubscriptKind::Whole => {}
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum PackedSubscriptKind {
    Index(u32),
    Whole,
    Slice(u32),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct PackedSubscript {
    pub(crate) kind: PackedSubscriptKind,
    pub(crate) provenance: DaeProvenance,
}
