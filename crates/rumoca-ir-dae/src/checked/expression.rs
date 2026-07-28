use rumoca_core::Span;
use serde::{Deserialize, Serialize};

use super::model::Storage;
use super::{
    AlgebraicId, DaeConstructionError, DaeProvenance, DiscreteRealId, DiscreteValueId, DomainId,
    ExprId, FunctionId, InputId, ParameterId, StateId, ValueTypeId,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ScalarType {
    Real,
    Integer,
    Boolean,
    String,
}

impl ScalarType {
    fn is_numeric(self) -> bool {
        matches!(self, Self::Real | Self::Integer)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ValueType {
    scalar: ScalarType,
    dimensions: Box<[u32]>,
}

impl ValueType {
    pub fn scalar(scalar: ScalarType) -> Self {
        Self {
            scalar,
            dimensions: Box::new([]),
        }
    }

    pub fn array(scalar: ScalarType, dimensions: impl Into<Box<[u32]>>) -> Self {
        Self {
            scalar,
            dimensions: dimensions.into(),
        }
    }

    pub const fn scalar_type(&self) -> ScalarType {
        self.scalar
    }

    pub fn dimensions(&self) -> &[u32] {
        &self.dimensions
    }

    pub fn is_scalar(&self) -> bool {
        self.dimensions.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DaeLiteral {
    Real(f64),
    Integer(i64),
    Boolean(bool),
    String(String),
}

impl DaeLiteral {
    fn scalar_type(&self) -> ScalarType {
        match self {
            Self::Real(_) => ScalarType::Real,
            Self::Integer(_) => ScalarType::Integer,
            Self::Boolean(_) => ScalarType::Boolean,
            Self::String(_) => ScalarType::String,
        }
    }
}

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
    Sin,
    Cos,
    Tan,
    Exp,
    Log,
    Min,
    Max,
    Sum,
    Product,
    Size,
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
    Condition(u32),
    Delay(u32),
    Previous(u32),
    Terminal,
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
    Condition(super::ConditionId<'dae>),
    Delay(u32),
    Previous(DiscreteRealId<'dae>),
    Terminal,
}

impl CoordinateInput<'_> {
    fn erase(self) -> Coordinate {
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
            Self::Condition(id) => Coordinate::Condition(id.index()),
            Self::Delay(id) => Coordinate::Delay(id),
            Self::Previous(id) => Coordinate::Previous(id.index()),
            Self::Terminal => Coordinate::Terminal,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct OperandRange {
    pub(crate) start: u32,
    pub(crate) len: u32,
}

impl OperandRange {
    fn new(start: usize, len: usize, at: DaeProvenance) -> Result<Self, DaeConstructionError> {
        Ok(Self {
            start: checked_u32(start, "expression operand buffer", at)?,
            len: checked_u32(len, "expression operand buffer", at)?,
        })
    }

    pub(crate) fn indices(self) -> std::ops::Range<usize> {
        let start = self.start as usize;
        start..start + self.len as usize
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
        operands: OperandRange,
    },
    Array {
        operands: OperandRange,
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
        subscripts: OperandRange,
    },
    Builtin {
        builtin: PureBuiltin,
        operands: OperandRange,
    },
    Call {
        function: u32,
        output: u32,
        operands: OperandRange,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum PackedSubscriptKind {
    Index(u32),
    Whole,
    Slice(u32),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct PackedSubscript {
    pub(crate) kind: PackedSubscriptKind,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
#[serde(deny_unknown_fields)]
pub(crate) struct ExpressionArenaStorage {
    pub(crate) nodes: Vec<ExprNode>,
    pub(crate) provenance: Vec<DaeProvenance>,
    pub(crate) value_types: Vec<u32>,
    pub(crate) operands: Vec<u32>,
    pub(crate) subscripts: Vec<PackedSubscript>,
}

impl ExpressionArenaStorage {
    pub(crate) fn push(
        &mut self,
        node: ExprNode,
        ty: u32,
        provenance: DaeProvenance,
    ) -> Result<u32, DaeConstructionError> {
        let id = checked_u32(self.nodes.len(), "expression arena", provenance)?;
        self.nodes.push(node);
        self.provenance.push(provenance);
        self.value_types.push(ty);
        debug_assert_eq!(self.nodes.len(), self.provenance.len());
        debug_assert_eq!(self.nodes.len(), self.value_types.len());
        Ok(id)
    }

    fn push_operands(
        &mut self,
        operands: impl IntoIterator<Item = u32>,
        at: DaeProvenance,
    ) -> Result<OperandRange, DaeConstructionError> {
        let start = self.operands.len();
        self.operands.extend(operands);
        OperandRange::new(start, self.operands.len() - start, at)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Subscript<'dae> {
    Index {
        expression: ExprId<'dae>,
        provenance: DaeProvenance,
    },
    Whole {
        provenance: DaeProvenance,
    },
    Slice {
        expression: ExprId<'dae>,
        provenance: DaeProvenance,
    },
}

impl Subscript<'_> {
    fn provenance(self) -> DaeProvenance {
        match self {
            Self::Index { provenance, .. }
            | Self::Whole { provenance }
            | Self::Slice { provenance, .. } => provenance,
        }
    }
}

/// Non-owning access to the one DAE-wide expression arena.
pub struct Expressions<'storage, 'dae> {
    pub(crate) source_map: &'storage rumoca_core::SourceMap,
    pub(crate) storage: &'storage mut Storage,
    pub(crate) marker: std::marker::PhantomData<&'dae mut &'dae ()>,
}

impl<'storage, 'dae> Expressions<'storage, 'dae> {
    /// Select the exact provenance for the single node inserted next.
    pub fn at<'scope>(&'scope mut self, provenance: DaeProvenance) -> ExpressionAt<'scope, 'dae> {
        ExpressionAt {
            source_map: self.source_map,
            storage: self.storage,
            provenance,
            marker: std::marker::PhantomData,
        }
    }
}

/// Inline node-construction scope selected by [`Expressions::at`].
pub struct ExpressionAt<'storage, 'dae> {
    source_map: &'storage rumoca_core::SourceMap,
    storage: &'storage mut Storage,
    provenance: DaeProvenance,
    marker: std::marker::PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> ExpressionAt<'_, 'dae> {
    pub fn literal(self, value: DaeLiteral) -> Result<ExprId<'dae>, DaeConstructionError> {
        if matches!(value, DaeLiteral::Real(value) if !value.is_finite()) {
            return Err(DaeConstructionError::ExpectedNumeric {
                found: ScalarType::Real,
                span: self.provenance.span(),
            });
        }
        let ty = self
            .storage
            .intern_type(ValueType::scalar(value.scalar_type()), self.provenance)?;
        self.insert(ExprNode::Literal(value), ty)
    }

    pub fn coordinate(
        self,
        coordinate: CoordinateInput<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let ty = self.storage.coordinate_type(coordinate, self.provenance)?;
        self.insert(ExprNode::Coordinate(coordinate.erase()), ty)
    }

    pub fn unary(
        self,
        operator: UnaryOperator,
        operand: ExprId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let ty = self.storage.expr_type(operand, self.provenance)?.clone();
        match operator {
            UnaryOperator::Not if ty.scalar_type() != ScalarType::Boolean => {
                return Err(type_mismatch(
                    ScalarType::Boolean,
                    ty.scalar_type(),
                    self.provenance,
                ));
            }
            UnaryOperator::Plus | UnaryOperator::Negate if !ty.scalar_type().is_numeric() => {
                return Err(DaeConstructionError::ExpectedNumeric {
                    found: ty.scalar_type(),
                    span: self.provenance.span(),
                });
            }
            _ => {}
        }
        let ty = self.storage.intern_type(ty, self.provenance)?;
        self.insert(
            ExprNode::Unary {
                operator,
                operand: operand.index(),
            },
            ty,
        )
    }

    pub fn binary(
        self,
        operator: BinaryOperator,
        lhs: ExprId<'dae>,
        rhs: ExprId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let lhs_ty = self.storage.expr_type(lhs, self.provenance)?.clone();
        let rhs_ty = self.storage.expr_type(rhs, self.provenance)?.clone();
        if lhs_ty != rhs_ty {
            return Err(DaeConstructionError::ShapeMismatch {
                span: self.provenance.span(),
            });
        }
        let result = binary_result(operator, &lhs_ty, self.provenance)?;
        let ty = self.storage.intern_type(result, self.provenance)?;
        self.insert(
            ExprNode::Binary {
                operator,
                lhs: lhs.index(),
                rhs: rhs.index(),
            },
            ty,
        )
    }

    pub fn array(
        self,
        elements: impl IntoIterator<Item = ExprId<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let elements = elements.into_iter().collect::<Vec<_>>();
        let Some(first) = elements.first().copied() else {
            return Err(DaeConstructionError::EmptyArray {
                span: self.provenance.span(),
            });
        };
        let element_ty = self.storage.expr_type(first, self.provenance)?.clone();
        for element in &elements[1..] {
            if self.storage.expr_type(*element, self.provenance)? != &element_ty {
                return Err(DaeConstructionError::ShapeMismatch {
                    span: self.provenance.span(),
                });
            }
        }
        let mut dimensions = Vec::with_capacity(element_ty.dimensions().len() + 1);
        dimensions.push(checked_u32(
            elements.len(),
            "array extent",
            self.provenance,
        )?);
        dimensions.extend_from_slice(element_ty.dimensions());
        let ty = self.storage.intern_type(
            ValueType::array(element_ty.scalar_type(), dimensions),
            self.provenance,
        )?;
        let operands = self
            .storage
            .expressions
            .push_operands(elements.into_iter().map(ExprId::index), self.provenance)?;
        self.insert(ExprNode::Array { operands }, ty)
    }

    pub fn range(
        self,
        start: i64,
        step: i64,
        stop: i64,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        if step == 0 {
            return Err(DaeConstructionError::ZeroRangeStep {
                span: self.provenance.span(),
            });
        }
        let extent = range_extent(start, step, stop);
        let ty = self.storage.intern_type(
            ValueType::array(ScalarType::Integer, [extent]),
            self.provenance,
        )?;
        self.insert(ExprNode::Range { start, step, stop }, ty)
    }

    pub fn comprehension(
        self,
        domain: DomainId<'dae>,
        body: ExprId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let extent = self.storage.domain_extent(domain, self.provenance)?;
        let body_ty = self.storage.expr_type(body, self.provenance)?.clone();
        let mut dimensions = Vec::with_capacity(body_ty.dimensions().len() + 1);
        dimensions.push(extent);
        dimensions.extend_from_slice(body_ty.dimensions());
        let ty = self.storage.intern_type(
            ValueType::array(body_ty.scalar_type(), dimensions),
            self.provenance,
        )?;
        self.insert(
            ExprNode::Comprehension {
                domain: domain.index(),
                body: body.index(),
            },
            ty,
        )
    }

    pub fn index(
        self,
        base: ExprId<'dae>,
        subscripts: impl IntoIterator<Item = Subscript<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let base_ty = self.storage.expr_type(base, self.provenance)?.clone();
        let subscripts = subscripts.into_iter().collect::<Vec<_>>();
        if subscripts.len() > base_ty.dimensions().len() {
            return Err(DaeConstructionError::InvalidSubscript {
                span: self.provenance.span(),
            });
        }
        let start = self.storage.expressions.subscripts.len();
        let mut indexed_axes = 0;
        for subscript in subscripts {
            super::model::check_provenance(self.source_map, subscript.provenance())?;
            let kind = match subscript {
                Subscript::Index { expression, .. } => {
                    validate_subscript(self.storage, expression, true, self.provenance)?;
                    indexed_axes += 1;
                    PackedSubscriptKind::Index(expression.index())
                }
                Subscript::Whole { .. } => PackedSubscriptKind::Whole,
                Subscript::Slice { expression, .. } => {
                    validate_subscript(self.storage, expression, false, self.provenance)?;
                    PackedSubscriptKind::Slice(expression.index())
                }
            };
            self.storage.expressions.subscripts.push(PackedSubscript {
                kind,
                provenance: subscript.provenance(),
            });
        }
        let range = OperandRange::new(
            start,
            self.storage.expressions.subscripts.len() - start,
            self.provenance,
        )?;
        let dimensions = base_ty.dimensions()[indexed_axes..].to_vec();
        let ty = self.storage.intern_type(
            ValueType::array(base_ty.scalar_type(), dimensions),
            self.provenance,
        )?;
        self.insert(
            ExprNode::Index {
                base: base.index(),
                subscripts: range,
            },
            ty,
        )
    }

    pub fn builtin(
        self,
        builtin: PureBuiltin,
        arguments: impl IntoIterator<Item = ExprId<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let arguments = arguments.into_iter().collect::<Vec<_>>();
        let result = builtin_result(self.storage, builtin, &arguments, self.provenance)?;
        let ty = self.storage.intern_type(result, self.provenance)?;
        let operands = self
            .storage
            .expressions
            .push_operands(arguments.into_iter().map(ExprId::index), self.provenance)?;
        self.insert(ExprNode::Builtin { builtin, operands }, ty)
    }

    pub fn call(
        self,
        function: FunctionId<'dae>,
        output: usize,
        arguments: impl IntoIterator<Item = ExprId<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let arguments = arguments.into_iter().collect::<Vec<_>>();
        let (parameters, results) = self.storage.function_signature(function, self.provenance)?;
        if arguments.len() != parameters.len() {
            return Err(DaeConstructionError::InvalidArity {
                expected: parameters.len(),
                found: arguments.len(),
                span: self.provenance.span(),
            });
        }
        for (argument, expected) in arguments.iter().zip(parameters) {
            let found = self
                .storage
                .expressions
                .value_types
                .get(argument.index() as usize)
                .copied()
                .ok_or_else(|| DaeConstructionError::UnknownId {
                    kind: "expression",
                    index: argument.index(),
                    span: self.provenance.span(),
                })?;
            if found != *expected {
                return Err(DaeConstructionError::ShapeMismatch {
                    span: self.provenance.span(),
                });
            }
        }
        let Some(&ty) = results.get(output) else {
            return Err(DaeConstructionError::InvalidArity {
                expected: results.len(),
                found: output + 1,
                span: self.provenance.span(),
            });
        };
        let operands = self
            .storage
            .expressions
            .push_operands(arguments.into_iter().map(ExprId::index), self.provenance)?;
        let output = checked_u32(output, "function output", self.provenance)?;
        self.insert(
            ExprNode::Call {
                function: function.index(),
                output,
                operands,
            },
            ValueTypeId::from_raw(ty),
        )
    }

    pub fn conditional(
        self,
        branches: impl IntoIterator<Item = (ExprId<'dae>, ExprId<'dae>)>,
        fallback: ExprId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let branches = branches.into_iter().collect::<Vec<_>>();
        if branches.is_empty() {
            return Err(DaeConstructionError::InvalidArity {
                expected: 1,
                found: 0,
                span: self.provenance.span(),
            });
        }
        let result = self.storage.expr_type(fallback, self.provenance)?.clone();
        for (condition, value) in &branches {
            let condition_ty = self.storage.expr_type(*condition, self.provenance)?;
            if !condition_ty.is_scalar() || condition_ty.scalar_type() != ScalarType::Boolean {
                return Err(type_mismatch(
                    ScalarType::Boolean,
                    condition_ty.scalar_type(),
                    self.provenance,
                ));
            }
            if self.storage.expr_type(*value, self.provenance)? != &result {
                return Err(DaeConstructionError::ShapeMismatch {
                    span: self.provenance.span(),
                });
            }
        }
        let mut raw = Vec::with_capacity(branches.len() * 2 + 1);
        for (condition, value) in branches {
            raw.extend([condition.index(), value.index()]);
        }
        raw.push(fallback.index());
        let operands = self
            .storage
            .expressions
            .push_operands(raw, self.provenance)?;
        let ty = self.storage.intern_type(result, self.provenance)?;
        self.insert(ExprNode::Conditional { operands }, ty)
    }

    fn insert(
        self,
        node: ExprNode,
        ty: ValueTypeId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        super::model::check_provenance(self.source_map, self.provenance)?;
        self.storage
            .expressions
            .push(node, ty.index(), self.provenance)
            .map(ExprId::from_raw)
    }
}

fn binary_result(
    operator: BinaryOperator,
    operands: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    let scalar = operands.scalar_type();
    match operator {
        BinaryOperator::And | BinaryOperator::Or if scalar != ScalarType::Boolean => {
            Err(type_mismatch(ScalarType::Boolean, scalar, at))
        }
        BinaryOperator::Add
        | BinaryOperator::Subtract
        | BinaryOperator::Multiply
        | BinaryOperator::Divide
        | BinaryOperator::Power
            if !scalar.is_numeric() =>
        {
            Err(DaeConstructionError::ExpectedNumeric {
                found: scalar,
                span: at.span(),
            })
        }
        BinaryOperator::Equal
        | BinaryOperator::NotEqual
        | BinaryOperator::Less
        | BinaryOperator::LessEqual
        | BinaryOperator::Greater
        | BinaryOperator::GreaterEqual => Ok(ValueType::array(
            ScalarType::Boolean,
            operands.dimensions().to_vec(),
        )),
        _ => Ok(operands.clone()),
    }
}

fn builtin_result<'dae>(
    storage: &Storage,
    builtin: PureBuiltin,
    arguments: &[ExprId<'dae>],
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    let Some(first) = arguments.first().copied() else {
        return Err(DaeConstructionError::InvalidArity {
            expected: 1,
            found: 0,
            span: at.span(),
        });
    };
    let first = storage.expr_type(first, at)?.clone();
    if matches!(builtin, PureBuiltin::Size) {
        return Ok(ValueType::scalar(ScalarType::Integer));
    }
    if !first.scalar_type().is_numeric() {
        return Err(DaeConstructionError::ExpectedNumeric {
            found: first.scalar_type(),
            span: at.span(),
        });
    }
    for argument in &arguments[1..] {
        if storage.expr_type(*argument, at)? != &first {
            return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
        }
    }
    Ok(first)
}

fn range_extent(start: i64, step: i64, stop: i64) -> u32 {
    if (step > 0 && start > stop) || (step < 0 && start < stop) {
        return 0;
    }
    let distance = i128::from(stop) - i128::from(start);
    let steps = distance / i128::from(step);
    u32::try_from(steps + 1).unwrap_or(u32::MAX)
}

fn checked_u32(
    value: usize,
    arena: &'static str,
    at: DaeProvenance,
) -> Result<u32, DaeConstructionError> {
    u32::try_from(value).map_err(|_| DaeConstructionError::CapacityExceeded {
        arena,
        attempted_index: value,
        span: at.span(),
    })
}

fn type_mismatch(
    expected: ScalarType,
    found: ScalarType,
    at: DaeProvenance,
) -> DaeConstructionError {
    DaeConstructionError::TypeMismatch {
        expected,
        found,
        span: at.span(),
    }
}

fn validate_subscript<'dae>(
    storage: &Storage,
    expression: ExprId<'dae>,
    expect_scalar: bool,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let ty = storage.expr_type(expression, at)?;
    let scalar_matches = ty.is_scalar() == expect_scalar;
    if ty.scalar_type() == ScalarType::Integer && scalar_matches {
        return Ok(());
    }
    Err(DaeConstructionError::InvalidSubscript { span: at.span() })
}

pub(crate) fn source_text(
    source_map: &rumoca_core::SourceMap,
    provenance: DaeProvenance,
) -> Option<&str> {
    let span: Span = provenance.span();
    let (_, source) = source_map.get_source(span.source)?;
    source.get(span.start.0..span.end.0)
}
