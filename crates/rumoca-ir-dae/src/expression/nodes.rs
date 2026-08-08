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
    /// MLS §10.4.2.1 concatenation after promoting every operand to at
    /// least rank two, along the first dimension.
    PromotedCat1,
    /// MLS §10.4.2.1 concatenation after promoting every operand to at
    /// least rank two, along the second dimension.
    PromotedCat2,
    /// MLS §10.3.3 Integer identity matrix with its extent retained as the
    /// constructor operand and no materialized scalar payload.
    ///
    /// Appended because the wire encoding uses enum ordinals; see
    /// `DAE_SCHEMA_VERSION`.
    Identity,
    /// MLS §10.3.2 rank-one view of one compact operand. The checked
    /// constructor derives its sole extent from the operand dimensions.
    ///
    /// Appended because the wire encoding uses enum ordinals; see
    /// `DAE_SCHEMA_VERSION`.
    Vector,
    /// MLS §10.3.5 permutation of the first two axes of one compact primitive
    /// operand. Axes at ordinal two and above retain their original order.
    ///
    /// Appended because the wire encoding uses enum ordinals; see
    /// `DAE_SCHEMA_VERSION`.
    Transpose,
    /// MLS §10.3.5 square matrix formed from one compact numeric vector.
    /// The checked constructor derives both result extents from the operand.
    ///
    /// Appended because the wire encoding uses enum ordinals; see
    /// `DAE_SCHEMA_VERSION`.
    Diagonal,
    /// MLS §10.3.5 matrix formed from two compact numeric vectors. The
    /// checked constructor derives each result extent from its corresponding
    /// operand.
    ///
    /// Appended because the wire encoding uses enum ordinals; see
    /// `DAE_SCHEMA_VERSION`.
    OuterProduct,
    /// MLS §10.3.5 antisymmetric matrix formed from one compact Real
    /// 3-vector. The checked constructor derives the fixed `[3, 3]` result.
    ///
    /// Appended because the wire encoding uses enum ordinals; see
    /// `DAE_SCHEMA_VERSION`.
    Skew,
}

impl PureBuiltin {
    pub(super) fn has_shaped_result(self) -> bool {
        matches!(
            self,
            Self::Zeros
                | Self::Ones
                | Self::Fill
                | Self::Linspace
                | Self::Cross
                | Self::PromotedCat1
                | Self::PromotedCat2
                | Self::Identity
                | Self::Vector
                | Self::Transpose
                | Self::Diagonal
                | Self::OuterProduct
                | Self::Skew
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
    Binder {
        domain: u32,
        ordinal: u32,
    },
    FunctionParameter {
        function: u32,
        ordinal: u32,
    },
    // Appended, not grouped with the other `Pre*` variants: `bincode` tags a
    // variant by its declaration ordinal, and `Dae::deserialize` decodes the
    // whole payload before it checks `DAE_SCHEMA_VERSION`. Inserting mid-enum
    // would make a superseded blob misread `Time` as `PreState(u32)` and
    // consume four bytes that are not there, corrupting the stream before the
    // version check can reject it. Appending keeps every earlier ordinal fixed,
    // so a stale blob still decodes structurally and fails on the version.
    /// MLS §3.7.5 `pre(v)` where `v` is a continuous state coordinate.
    ///
    /// Legal only inside an unclocked when-clause body, where it denotes the
    /// left limit `v(t^pre)` at event entry — the value before any body
    /// definition or `reinit` of the same event takes effect.
    PreState(u32),
    /// MLS §3.7.5 `pre(v)` where `v` is a continuous algebraic (or output)
    /// coordinate, with the same event-entry left-limit meaning.
    PreAlgebraic(u32),
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
    PreState(StateId<'dae>),
    PreAlgebraic(AlgebraicId<'dae>),
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
            Self::PreState(id) => Coordinate::PreState(id.index()),
            Self::PreAlgebraic(id) => Coordinate::PreAlgebraic(id.index()),
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
    ClockTransfer {
        kind: crate::ClockTransferKind,
        source: u32,
        source_clock: u32,
        target_clock: u32,
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
            | Self::Comprehension { body: operand, .. }
            | Self::ClockTransfer {
                source: operand, ..
            } => visit(*operand),
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
