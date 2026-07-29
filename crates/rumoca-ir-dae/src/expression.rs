use rumoca_core::Span;
use serde::{Deserialize, Serialize};

use crate::model::{Storage, invalid_arity};
use crate::{
    AlgebraicId, DaeConstructionError, DaeProvenance, DiscreteRealId, DiscreteValueId,
    DomainBinderId, DomainId, ExprId, FunctionFoldId, FunctionId, FunctionParameterId,
    FunctionValueId, InputId, ParameterId, StateId, ValueTypeId,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ScalarType {
    Real,
    Integer,
    Boolean,
    String,
    Record,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ExpressionVariability {
    Constant,
    Parameter,
    Discrete,
    Continuous,
}

impl ScalarType {
    pub(crate) fn is_numeric(self) -> bool {
        matches!(self, Self::Real | Self::Integer)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ValueType {
    scalar: ScalarType,
    dimensions: Box<[u32]>,
    record_name: Option<rumoca_core::VarName>,
    record_fields: Box<[RecordFieldType]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct RecordFieldType {
    name: rumoca_core::VarName,
    value_type: u32,
}

impl ValueType {
    pub fn scalar(scalar: ScalarType) -> Self {
        Self {
            scalar,
            dimensions: Box::new([]),
            record_name: None,
            record_fields: Box::new([]),
        }
    }

    pub fn array(scalar: ScalarType, dimensions: impl Into<Box<[u32]>>) -> Self {
        Self {
            scalar,
            dimensions: dimensions.into(),
            record_name: None,
            record_fields: Box::new([]),
        }
    }

    pub(crate) fn record(
        name: rumoca_core::VarName,
        fields: impl Into<Box<[RecordFieldType]>>,
    ) -> Self {
        Self {
            scalar: ScalarType::Record,
            dimensions: Box::new([]),
            record_name: Some(name),
            record_fields: fields.into(),
        }
    }

    pub const fn scalar_type(&self) -> ScalarType {
        self.scalar
    }

    pub fn dimensions(&self) -> &[u32] {
        &self.dimensions
    }

    pub fn is_scalar(&self) -> bool {
        self.scalar != ScalarType::Record && self.dimensions.is_empty()
    }

    pub fn scalar_count(&self) -> Option<usize> {
        if self.scalar == ScalarType::Record {
            return None;
        }
        self.dimensions
            .iter()
            .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
    }

    pub fn scalar_subscripts(&self, flat_index: usize) -> Option<Vec<u32>> {
        if self.scalar == ScalarType::Record {
            return None;
        }
        if self.dimensions.is_empty() {
            return (flat_index == 0).then(Vec::new);
        }
        let scalar_count = self.scalar_count()?;
        if flat_index >= scalar_count || self.dimensions.contains(&0) {
            return None;
        }
        let mut remainder = flat_index;
        let mut subscripts = Vec::with_capacity(self.dimensions.len());
        for extent in self.dimensions.iter().rev() {
            subscripts.push(u32::try_from(remainder % *extent as usize).ok()? + 1);
            remainder /= *extent as usize;
        }
        subscripts.reverse();
        Some(subscripts)
    }

    pub fn is_record(&self) -> bool {
        self.scalar == ScalarType::Record
    }

    pub fn record_name(&self) -> Option<&rumoca_core::VarName> {
        self.record_name.as_ref()
    }

    pub fn record_field_count(&self) -> usize {
        self.record_fields.len()
    }

    pub fn record_field_name(&self, ordinal: usize) -> Option<&rumoca_core::VarName> {
        self.record_fields.get(ordinal).map(|field| &field.name)
    }

    pub(crate) fn record_field_type(&self, ordinal: usize) -> Option<u32> {
        self.record_fields
            .get(ordinal)
            .map(|field| field.value_type)
    }
}

impl RecordFieldType {
    pub(crate) fn new(name: rumoca_core::VarName, value_type: u32) -> Self {
        Self { name, value_type }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum DaeLiteral {
    Real(f64),
    Integer(i64),
    Enumeration(i64),
    Boolean(bool),
    String(String),
}

impl DaeLiteral {
    fn scalar_type(&self) -> ScalarType {
        match self {
            Self::Real(_) => ScalarType::Real,
            Self::Integer(_) => ScalarType::Integer,
            Self::Enumeration(_) => ScalarType::Integer,
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
    fn has_shaped_result(self) -> bool {
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
    Condition(crate::ConditionId<'dae>),
    Previous(crate::PreviousId<'dae>),
    Terminal(crate::TerminalId<'dae>),
    FunctionParameter(FunctionParameterId<'dae>),
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
            Self::Previous(id) => Coordinate::Previous(id.index()),
            Self::Terminal(id) => Coordinate::Terminal(id.index()),
            Self::FunctionParameter(id) => Coordinate::FunctionParameter {
                function: id.function().index(),
                ordinal: id.ordinal(),
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
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
        operands: OperandRange,
    },
    Array {
        operands: OperandRange,
    },
    Record {
        operands: OperandRange,
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
        subscripts: OperandRange,
    },
    ArrayUpdate {
        base: u32,
        value: u32,
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
    FunctionValue {
        function: u32,
        value: u32,
        definition: u32,
    },
    FunctionFoldParameter {
        function: u32,
        fold: u32,
        carried: u32,
    },
    FunctionFoldOutput {
        function: u32,
        fold: u32,
        carried: u32,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize)]
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

#[derive(Debug, Clone, PartialEq, Serialize, Default)]
#[serde(deny_unknown_fields)]
pub(crate) struct ExpressionArenaStorage {
    pub(crate) nodes: Vec<ExprNode>,
    pub(crate) provenance: Vec<DaeProvenance>,
    pub(crate) value_types: Vec<u32>,
    pub(crate) variability: Vec<ExpressionVariability>,
    pub(crate) binder_domains: Vec<Option<u32>>,
    pub(crate) function_scopes: Vec<Option<u32>>,
    pub(crate) operands: Vec<u32>,
    pub(crate) subscripts: Vec<PackedSubscript>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct FrozenExpressionArenaStorage {
    pub(crate) nodes: Box<[ExprNode]>,
    pub(crate) provenance: Box<[DaeProvenance]>,
    pub(crate) value_types: Box<[u32]>,
    pub(crate) variability: Box<[ExpressionVariability]>,
    pub(crate) binder_domains: Box<[Option<u32>]>,
    pub(crate) function_scopes: Box<[Option<u32>]>,
    pub(crate) operands: Box<[u32]>,
    pub(crate) subscripts: Box<[PackedSubscript]>,
}

impl ExpressionArenaStorage {
    pub(crate) fn push(
        &mut self,
        node: ExprNode,
        ty: u32,
        variability: ExpressionVariability,
        binder_domain: Option<u32>,
        function_scope: Option<u32>,
        provenance: DaeProvenance,
    ) -> Result<u32, DaeConstructionError> {
        let id = checked_u32(self.nodes.len(), "expression arena", provenance)?;
        self.nodes.push(node);
        self.provenance.push(provenance);
        self.value_types.push(ty);
        self.variability.push(variability);
        self.binder_domains.push(binder_domain);
        self.function_scopes.push(function_scope);
        debug_assert_eq!(self.nodes.len(), self.provenance.len());
        debug_assert_eq!(self.nodes.len(), self.value_types.len());
        debug_assert_eq!(self.nodes.len(), self.variability.len());
        debug_assert_eq!(self.nodes.len(), self.binder_domains.len());
        debug_assert_eq!(self.nodes.len(), self.function_scopes.len());
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

    pub(crate) fn freeze(self) -> FrozenExpressionArenaStorage {
        FrozenExpressionArenaStorage {
            nodes: self.nodes.into_boxed_slice(),
            provenance: self.provenance.into_boxed_slice(),
            value_types: self.value_types.into_boxed_slice(),
            variability: self.variability.into_boxed_slice(),
            binder_domains: self.binder_domains.into_boxed_slice(),
            function_scopes: self.function_scopes.into_boxed_slice(),
            operands: self.operands.into_boxed_slice(),
            subscripts: self.subscripts.into_boxed_slice(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Subscript<'dae> {
    Value {
        expression: ExprId<'dae>,
        provenance: DaeProvenance,
    },
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
            Self::Value { provenance, .. }
            | Self::Index { provenance, .. }
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
        if matches!(value, DaeLiteral::Enumeration(_)) {
            return Err(DaeConstructionError::InvalidEnumerationOrdinal {
                ordinal: 0,
                span: self.provenance.span(),
            });
        }
        if matches!(value, DaeLiteral::Real(value) if !value.is_finite()) {
            return Err(DaeConstructionError::ExpectedNumeric {
                found: ScalarType::Real,
                span: self.provenance.span(),
            });
        }
        let ty = self
            .storage
            .intern_type(ValueType::scalar(value.scalar_type()), self.provenance)?;
        self.insert(
            ExprNode::Literal(value),
            ty,
            ExpressionVariability::Constant,
            None,
        )
    }

    pub fn enumeration_literal(self, ordinal: i64) -> Result<ExprId<'dae>, DaeConstructionError> {
        if ordinal < 1 {
            return Err(DaeConstructionError::InvalidEnumerationOrdinal {
                ordinal,
                span: self.provenance.span(),
            });
        }
        let value_type = self
            .storage
            .intern_type(ValueType::scalar(ScalarType::Integer), self.provenance)?;
        self.insert(
            ExprNode::Literal(DaeLiteral::Enumeration(ordinal)),
            value_type,
            ExpressionVariability::Constant,
            None,
        )
    }

    pub fn coordinate(
        self,
        coordinate: CoordinateInput<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let (ty, variability) = self.storage.coordinate_facts(coordinate, self.provenance)?;
        self.insert(
            ExprNode::Coordinate(coordinate.erase()),
            ty,
            variability,
            None,
        )
    }

    pub fn function_parameter(
        self,
        parameter: FunctionParameterId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let (ty, variability) = self
            .storage
            .function_parameter_facts(parameter, self.provenance)?;
        self.insert(
            ExprNode::Coordinate(Coordinate::FunctionParameter {
                function: parameter.function().index(),
                ordinal: parameter.ordinal(),
            }),
            ty,
            variability,
            None,
        )
    }

    fn function_value_use(
        self,
        value: FunctionValueId<'dae>,
        definition: ExprId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let ty = self.storage.function_value_facts(value, self.provenance)?;
        self.storage.expect_value_type_compatible(
            ty.index(),
            definition_type(self.storage, definition, self.provenance)?,
            self.provenance,
        )?;
        let variability = self.storage.expr_variability(definition, self.provenance)?;
        let binder_domain = self
            .storage
            .expr_binder_domain(definition, self.provenance)?;
        self.insert(
            ExprNode::FunctionValue {
                function: value.function().index(),
                value: value.ordinal(),
                definition: definition.index(),
            },
            ty,
            variability,
            binder_domain,
        )
    }

    pub fn binder(
        self,
        binder: DomainBinderId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        self.storage
            .domain_binder(binder.domain().index(), binder.ordinal(), self.provenance)?;
        let ty = self
            .storage
            .intern_type(ValueType::scalar(ScalarType::Integer), self.provenance)?;
        self.insert(
            ExprNode::Coordinate(Coordinate::Binder {
                domain: binder.domain().index(),
                ordinal: binder.ordinal(),
            }),
            ty,
            ExpressionVariability::Constant,
            Some(binder.domain().index()),
        )
    }

    pub fn unary(
        self,
        operator: UnaryOperator,
        operand: ExprId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let ty = self.storage.expr_type(operand, self.provenance)?.clone();
        let variability = self.storage.expr_variability(operand, self.provenance)?;
        let binder_domain = self.storage.expr_binder_domain(operand, self.provenance)?;
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
            variability,
            binder_domain,
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
        let variability = self
            .storage
            .expr_variability(lhs, self.provenance)?
            .max(self.storage.expr_variability(rhs, self.provenance)?);
        let binder_domain = merged_binder_domain(self.storage, [lhs, rhs], self.provenance)?;
        let result = binary_result(operator, &lhs_ty, &rhs_ty, self.provenance)?;
        let ty = self.storage.intern_type(result, self.provenance)?;
        self.insert(
            ExprNode::Binary {
                operator,
                lhs: lhs.index(),
                rhs: rhs.index(),
            },
            ty,
            variability,
            binder_domain,
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
        let mut element_ty = self.storage.expr_type(first, self.provenance)?.clone();
        let mut variability = self.storage.expr_variability(first, self.provenance)?;
        let binder_domain =
            merged_binder_domain(self.storage, elements.iter().copied(), self.provenance)?;
        for element in &elements[1..] {
            element_ty = common_value_type(
                &element_ty,
                self.storage.expr_type(*element, self.provenance)?,
                self.provenance,
            )?;
            variability =
                variability.max(self.storage.expr_variability(*element, self.provenance)?);
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
        self.insert(ExprNode::Array { operands }, ty, variability, binder_domain)
    }

    /// Construct an empty array using its context-proven value type.
    ///
    /// An empty literal has no element expression from which to derive its
    /// scalar type or trailing dimensions, so its semantic owner must supply
    /// an array type whose outer extent is zero.
    pub fn empty_array(
        self,
        value_type: ValueTypeId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let ty = self
            .storage
            .value_type_at(value_type.index(), self.provenance)?;
        if ty.is_record() || ty.dimensions().first() != Some(&0) {
            return Err(DaeConstructionError::ShapeMismatch {
                span: self.provenance.span(),
            });
        }
        let operands = self
            .storage
            .expressions
            .push_operands(std::iter::empty(), self.provenance)?;
        self.insert(
            ExprNode::Array { operands },
            value_type,
            ExpressionVariability::Constant,
            None,
        )
    }

    pub fn record(
        self,
        value_type: ValueTypeId<'dae>,
        fields: impl IntoIterator<Item = ExprId<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let fields = fields.into_iter().collect::<Vec<_>>();
        let record = self
            .storage
            .value_type_at(value_type.index(), self.provenance)?
            .clone();
        if !record.is_record() || fields.len() != record.record_field_count() {
            return Err(invalid_arity(
                record.record_field_count(),
                fields.len(),
                self.provenance,
            ));
        }
        let mut variability = ExpressionVariability::Constant;
        for (ordinal, field) in fields.iter().copied().enumerate() {
            self.storage.expect_value_type_compatible(
                record
                    .record_field_type(ordinal)
                    .expect("record field ordinal is in range"),
                definition_type(self.storage, field, self.provenance)?,
                self.provenance,
            )?;
            variability = variability.max(self.storage.expr_variability(field, self.provenance)?);
        }
        let binder_domain =
            merged_binder_domain(self.storage, fields.iter().copied(), self.provenance)?;
        let operands = self
            .storage
            .expressions
            .push_operands(fields.into_iter().map(ExprId::index), self.provenance)?;
        self.insert(
            ExprNode::Record { operands },
            value_type,
            variability,
            binder_domain,
        )
    }

    pub fn field(
        self,
        base: ExprId<'dae>,
        field: usize,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let record = self.storage.expr_type(base, self.provenance)?;
        let field_type = record.record_field_type(field).ok_or_else(|| {
            invalid_arity(record.record_field_count(), field + 1, self.provenance)
        })?;
        let variability = self.storage.expr_variability(base, self.provenance)?;
        let binder_domain = self.storage.expr_binder_domain(base, self.provenance)?;
        let field = checked_u32(field, "record field", self.provenance)?;
        self.insert(
            ExprNode::Field {
                base: base.index(),
                field,
            },
            ValueTypeId::from_raw(field_type),
            variability,
            binder_domain,
        )
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
        let extent = range_extent(start, step, stop, self.provenance)?;
        let ty = self.storage.intern_type(
            ValueType::array(ScalarType::Integer, [extent]),
            self.provenance,
        )?;
        self.insert(
            ExprNode::Range { start, step, stop },
            ty,
            ExpressionVariability::Constant,
            None,
        )
    }

    pub fn comprehension(
        self,
        domain: DomainId<'dae>,
        body: ExprId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let domain_extents = self
            .storage
            .domain_extents(domain, self.provenance)?
            .to_vec();
        let body_ty = self.storage.expr_type(body, self.provenance)?.clone();
        let variability = self.storage.expr_variability(body, self.provenance)?;
        self.storage
            .expect_domain_expression(body, domain, self.provenance)?;
        let mut dimensions = Vec::with_capacity(body_ty.dimensions().len() + domain_extents.len());
        dimensions.extend(domain_extents);
        dimensions.extend_from_slice(body_ty.dimensions());
        let ty = self.storage.intern_type(
            ValueType::array(body_ty.scalar_type(), dimensions),
            self.provenance,
        )?;
        let binder_domain = self.storage.domain_parent(domain, self.provenance)?;
        self.insert(
            ExprNode::Comprehension {
                domain: domain.index(),
                body: body.index(),
            },
            ty,
            variability,
            binder_domain,
        )
    }

    pub fn index(
        self,
        base: ExprId<'dae>,
        subscripts: impl IntoIterator<Item = Subscript<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let selection = pack_subscripts(
            self.source_map,
            self.storage,
            base,
            subscripts.into_iter().collect(),
            self.provenance,
        )?;
        let ty = self.storage.intern_type(
            ValueType::array(selection.scalar_type, selection.dimensions),
            self.provenance,
        )?;
        self.insert(
            ExprNode::Index {
                base: base.index(),
                subscripts: selection.range,
            },
            ty,
            selection.variability,
            selection.binder_domain,
        )
    }

    pub fn array_update(
        self,
        base: ExprId<'dae>,
        value: ExprId<'dae>,
        subscripts: impl IntoIterator<Item = Subscript<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let base_type = self
            .storage
            .expressions
            .value_types
            .get(base.index() as usize)
            .copied()
            .ok_or_else(|| crate::model::unknown("expression", base.index(), self.provenance))?;
        let selection = pack_subscripts(
            self.source_map,
            self.storage,
            base,
            subscripts.into_iter().collect(),
            self.provenance,
        )?;
        let selected_type = self.storage.intern_type(
            ValueType::array(selection.scalar_type, selection.dimensions),
            self.provenance,
        )?;
        let value_type = definition_type(self.storage, value, self.provenance)?;
        self.storage.expect_value_type_compatible(
            selected_type.index(),
            value_type,
            self.provenance,
        )?;
        let variability = selection
            .variability
            .max(self.storage.expr_variability(value, self.provenance)?);
        let binder_domain = merge_binder_domain(
            self.storage,
            selection.binder_domain,
            self.storage.expr_binder_domain(value, self.provenance)?,
            self.provenance,
        )?;
        self.insert(
            ExprNode::ArrayUpdate {
                base: base.index(),
                value: value.index(),
                subscripts: selection.range,
            },
            ValueTypeId::from_raw(base_type),
            variability,
            binder_domain,
        )
    }

    pub fn builtin(
        self,
        builtin: PureBuiltin,
        arguments: impl IntoIterator<Item = ExprId<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let arguments = arguments.into_iter().collect::<Vec<_>>();
        let variability = max_variability(self.storage, &arguments, self.provenance)?;
        let result = builtin_result(self.storage, builtin, &arguments, self.provenance)?;
        if matches!(
            builtin,
            PureBuiltin::Div | PureBuiltin::Mod | PureBuiltin::Rem
        ) {
            validate_static_quotient(self.storage, builtin, &arguments, self.provenance)?;
        }
        let binder_domain =
            merged_binder_domain(self.storage, arguments.iter().copied(), self.provenance)?;
        let ty = self.storage.intern_type(result, self.provenance)?;
        let operands = self
            .storage
            .expressions
            .push_operands(arguments.into_iter().map(ExprId::index), self.provenance)?;
        self.insert(
            ExprNode::Builtin { builtin, operands },
            ty,
            variability,
            binder_domain,
        )
    }

    pub fn call(
        self,
        function: FunctionId<'dae>,
        output: usize,
        arguments: impl IntoIterator<Item = ExprId<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let arguments = arguments.into_iter().collect::<Vec<_>>();
        let variability = max_variability(self.storage, &arguments, self.provenance)?;
        let binder_domain =
            merged_binder_domain(self.storage, arguments.iter().copied(), self.provenance)?;
        let (parameters, results) = self.storage.function_signature(function, self.provenance)?;
        if arguments.len() != parameters.len() {
            return Err(invalid_arity(
                parameters.len(),
                arguments.len(),
                self.provenance,
            ));
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
            self.storage
                .expect_value_type_compatible(*expected, found, self.provenance)?;
        }
        let Some(&ty) = results.get(output) else {
            return Err(invalid_arity(results.len(), output + 1, self.provenance));
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
            variability,
            binder_domain,
        )
    }

    pub fn conditional(
        self,
        branches: impl IntoIterator<Item = (ExprId<'dae>, ExprId<'dae>)>,
        fallback: ExprId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let branches = branches.into_iter().collect::<Vec<_>>();
        if branches.is_empty() {
            return Err(invalid_arity(1, 0, self.provenance));
        }
        let mut result = self.storage.expr_type(fallback, self.provenance)?.clone();
        let mut variability = self.storage.expr_variability(fallback, self.provenance)?;
        let mut expression_inputs = Vec::with_capacity(branches.len() * 2 + 1);
        expression_inputs.push(fallback);
        for (condition, value) in &branches {
            let condition_ty = self.storage.expr_type(*condition, self.provenance)?;
            if !condition_ty.is_scalar() || condition_ty.scalar_type() != ScalarType::Boolean {
                return Err(type_mismatch(
                    ScalarType::Boolean,
                    condition_ty.scalar_type(),
                    self.provenance,
                ));
            }
            result = common_value_type(
                &result,
                self.storage.expr_type(*value, self.provenance)?,
                self.provenance,
            )?;
            variability =
                variability.max(self.storage.expr_variability(*condition, self.provenance)?);
            variability = variability.max(self.storage.expr_variability(*value, self.provenance)?);
            expression_inputs.extend([*condition, *value]);
        }
        let binder_domain = merged_binder_domain(self.storage, expression_inputs, self.provenance)?;
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
        self.insert(
            ExprNode::Conditional { operands },
            ty,
            variability,
            binder_domain,
        )
    }

    fn insert(
        self,
        node: ExprNode,
        ty: ValueTypeId<'dae>,
        variability: ExpressionVariability,
        binder_domain: Option<u32>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        crate::model::check_provenance(self.source_map, self.provenance)?;
        let function_scope = node_function_scope(self.storage, &node, self.provenance)?;
        self.storage
            .expressions
            .push(
                node,
                ty.index(),
                variability,
                binder_domain,
                function_scope,
                self.provenance,
            )
            .map(ExprId::from_raw)
    }
}

struct PackedSelection {
    scalar_type: ScalarType,
    dimensions: Vec<u32>,
    range: OperandRange,
    variability: ExpressionVariability,
    binder_domain: Option<u32>,
}

fn pack_subscripts(
    source_map: &rumoca_core::SourceMap,
    storage: &mut Storage,
    base: ExprId<'_>,
    subscripts: Vec<Subscript<'_>>,
    provenance: DaeProvenance,
) -> Result<PackedSelection, DaeConstructionError> {
    let base_type = storage.expr_type(base, provenance)?.clone();
    if subscripts.len() > base_type.dimensions().len() {
        return Err(DaeConstructionError::InvalidSubscript {
            span: provenance.span(),
        });
    }
    let start = storage.expressions.subscripts.len();
    let mut dimensions = Vec::new();
    let mut variability = storage.expr_variability(base, provenance)?;
    let mut binder_domain = storage.expr_binder_domain(base, provenance)?;
    for (axis, subscript) in subscripts.into_iter().enumerate() {
        crate::model::check_provenance(source_map, subscript.provenance())?;
        let axis_extent = base_type.dimensions()[axis];
        let kind = match subscript {
            Subscript::Value { expression, .. } => {
                let ty = storage.expr_type(expression, provenance)?.clone();
                validate_subscript(storage, expression, ty.is_scalar(), provenance)?;
                variability = variability.max(storage.expr_variability(expression, provenance)?);
                binder_domain = merge_binder_domain(
                    storage,
                    binder_domain,
                    storage.expr_binder_domain(expression, provenance)?,
                    provenance,
                )?;
                packed_value_subscript(&ty, expression, &mut dimensions)
            }
            Subscript::Index { expression, .. } => {
                validate_subscript(storage, expression, true, provenance)?;
                variability = variability.max(storage.expr_variability(expression, provenance)?);
                binder_domain = merge_binder_domain(
                    storage,
                    binder_domain,
                    storage.expr_binder_domain(expression, provenance)?,
                    provenance,
                )?;
                PackedSubscriptKind::Index(expression.index())
            }
            Subscript::Whole { .. } => {
                dimensions.push(axis_extent);
                PackedSubscriptKind::Whole
            }
            Subscript::Slice { expression, .. } => {
                validate_subscript(storage, expression, false, provenance)?;
                dimensions
                    .extend_from_slice(storage.expr_type(expression, provenance)?.dimensions());
                variability = variability.max(storage.expr_variability(expression, provenance)?);
                binder_domain = merge_binder_domain(
                    storage,
                    binder_domain,
                    storage.expr_binder_domain(expression, provenance)?,
                    provenance,
                )?;
                PackedSubscriptKind::Slice(expression.index())
            }
        };
        storage.expressions.subscripts.push(PackedSubscript {
            kind,
            provenance: subscript.provenance(),
        });
    }
    let range = OperandRange::new(
        start,
        storage.expressions.subscripts.len() - start,
        provenance,
    )?;
    dimensions.extend_from_slice(&base_type.dimensions()[range.len as usize..]);
    Ok(PackedSelection {
        scalar_type: base_type.scalar_type(),
        dimensions,
        range,
        variability,
        binder_domain,
    })
}

fn node_function_scope(
    storage: &Storage,
    node: &ExprNode,
    at: DaeProvenance,
) -> Result<Option<u32>, DaeConstructionError> {
    let direct = match node {
        ExprNode::Coordinate(Coordinate::FunctionParameter { function, .. })
        | ExprNode::FunctionValue { function, .. }
        | ExprNode::FunctionFoldParameter { function, .. }
        | ExprNode::FunctionFoldOutput { function, .. } => {
            return Ok(Some(*function));
        }
        ExprNode::Literal(_) | ExprNode::Coordinate(_) | ExprNode::Range { .. } => Vec::new(),
        ExprNode::Unary { operand, .. } => vec![*operand],
        ExprNode::Binary { lhs, rhs, .. } => vec![*lhs, *rhs],
        ExprNode::Field { base, .. } => vec![*base],
        ExprNode::Comprehension { body, .. } => vec![*body],
        ExprNode::Index { base, subscripts } => {
            expression_with_subscripts(storage, vec![*base], *subscripts, at)?
        }
        ExprNode::ArrayUpdate {
            base,
            value,
            subscripts,
        } => expression_with_subscripts(storage, vec![*base, *value], *subscripts, at)?,
        ExprNode::Conditional { operands }
        | ExprNode::Array { operands }
        | ExprNode::Record { operands }
        | ExprNode::Builtin { operands, .. }
        | ExprNode::Call { operands, .. } => storage
            .expressions
            .operands
            .get(operands.indices())
            .ok_or_else(|| crate::model::unknown("operand range", operands.start, at))?
            .to_vec(),
    };
    direct.into_iter().try_fold(None, |scope, expression| {
        let found = storage
            .expressions
            .function_scopes
            .get(expression as usize)
            .copied()
            .ok_or_else(|| crate::model::unknown("expression", expression, at))?;
        merge_function_scope(scope, found, at)
    })
}

fn expression_with_subscripts(
    storage: &Storage,
    mut values: Vec<u32>,
    subscripts: OperandRange,
    at: DaeProvenance,
) -> Result<Vec<u32>, DaeConstructionError> {
    for subscript in storage
        .expressions
        .subscripts
        .get(subscripts.indices())
        .ok_or_else(|| crate::model::unknown("subscript range", subscripts.start, at))?
    {
        match subscript.kind {
            PackedSubscriptKind::Index(value) | PackedSubscriptKind::Slice(value) => {
                values.push(value);
            }
            PackedSubscriptKind::Whole => {}
        }
    }
    Ok(values)
}

pub(crate) fn insert_function_value_use<'dae>(
    source_map: &rumoca_core::SourceMap,
    storage: &mut Storage,
    value: FunctionValueId<'dae>,
    definition: ExprId<'dae>,
    domain: Option<DomainId<'dae>>,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    match domain {
        Some(domain) => storage.expect_domain_expression(definition, domain, provenance)?,
        None => {
            if let Some(found_domain) = storage.expr_binder_domain(definition, provenance)? {
                return Err(DaeConstructionError::InvalidBinderScope {
                    expected_domain: None,
                    found_domain,
                    span: provenance.span(),
                });
            }
        }
    }
    match storage.expr_function_scope(definition, provenance)? {
        None => {}
        Some(function) if function == value.function().index() => {}
        Some(function) => {
            return Err(DaeConstructionError::InvalidFunctionScope {
                expected_function: Some(value.function().index()),
                found_function: function,
                span: provenance.span(),
            });
        }
    }
    ExpressionAt {
        source_map,
        storage,
        provenance,
        marker: std::marker::PhantomData,
    }
    .function_value_use(value, definition)
}

pub(crate) fn insert_function_fold_parameter<'dae>(
    source_map: &rumoca_core::SourceMap,
    storage: &mut Storage,
    fold: FunctionFoldId<'dae>,
    carried: usize,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    let (value_type, domain) = function_fold_value_facts(storage, fold, carried, provenance)?;
    ExpressionAt {
        source_map,
        storage,
        provenance,
        marker: std::marker::PhantomData,
    }
    .insert(
        ExprNode::FunctionFoldParameter {
            function: fold.function().index(),
            fold: fold.ordinal(),
            carried: checked_u32(carried, "function fold parameter", provenance)?,
        },
        ValueTypeId::from_raw(value_type),
        ExpressionVariability::Parameter,
        Some(domain),
    )
}

pub(crate) fn insert_function_fold_output<'dae>(
    source_map: &rumoca_core::SourceMap,
    storage: &mut Storage,
    fold: FunctionFoldId<'dae>,
    carried: usize,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    let (value_type, _) = function_fold_value_facts(storage, fold, carried, provenance)?;
    let entry = function_fold_entry(storage, fold, provenance)?;
    let initial = ExprId::from_raw(entry.initial_values[carried]);
    let update = ExprId::from_raw(entry.update_values[carried]);
    let variability = storage
        .expr_variability(initial, provenance)?
        .max(storage.expr_variability(update, provenance)?);
    ExpressionAt {
        source_map,
        storage,
        provenance,
        marker: std::marker::PhantomData,
    }
    .insert(
        ExprNode::FunctionFoldOutput {
            function: fold.function().index(),
            fold: fold.ordinal(),
            carried: checked_u32(carried, "function fold output", provenance)?,
        },
        ValueTypeId::from_raw(value_type),
        variability,
        None,
    )
}

fn function_fold_value_facts(
    storage: &Storage,
    fold: FunctionFoldId<'_>,
    carried: usize,
    provenance: DaeProvenance,
) -> Result<(u32, u32), DaeConstructionError> {
    let entry = function_fold_entry(storage, fold, provenance)?;
    let value = entry
        .targets
        .get(carried)
        .copied()
        .ok_or_else(|| invalid_arity(entry.targets.len(), carried + 1, provenance))?;
    let value_type = storage
        .functions
        .get(fold.function().index() as usize)
        .and_then(|function| function.values.get(value as usize))
        .map(|value| value.value_type)
        .ok_or_else(|| crate::model::unknown("function fold target", value, provenance))?;
    Ok((value_type, entry.domain))
}

fn function_fold_entry<'storage>(
    storage: &'storage Storage,
    fold: FunctionFoldId<'_>,
    provenance: DaeProvenance,
) -> Result<&'storage crate::model::FunctionFoldEntry, DaeConstructionError> {
    let raw = storage
        .functions
        .get(fold.function().index() as usize)
        .and_then(|function| function.folds.get(fold.ordinal() as usize))
        .copied()
        .ok_or_else(|| crate::model::unknown("function fold", fold.ordinal(), provenance))?;
    storage
        .function_folds
        .get(raw as usize)
        .ok_or_else(|| crate::model::unknown("function fold", raw, provenance))
}

fn definition_type(
    storage: &Storage,
    definition: ExprId<'_>,
    at: DaeProvenance,
) -> Result<u32, DaeConstructionError> {
    storage
        .expressions
        .value_types
        .get(definition.index() as usize)
        .copied()
        .ok_or_else(|| crate::model::unknown("expression", definition.index(), at))
}

fn merge_function_scope(
    lhs: Option<u32>,
    rhs: Option<u32>,
    at: DaeProvenance,
) -> Result<Option<u32>, DaeConstructionError> {
    match (lhs, rhs) {
        (None, scope) | (scope, None) => Ok(scope),
        (Some(lhs), Some(rhs)) if lhs == rhs => Ok(Some(lhs)),
        (Some(expected), Some(found)) => Err(DaeConstructionError::InvalidFunctionScope {
            expected_function: Some(expected),
            found_function: found,
            span: at.span(),
        }),
    }
}

fn packed_value_subscript(
    ty: &ValueType,
    expression: ExprId<'_>,
    dimensions: &mut Vec<u32>,
) -> PackedSubscriptKind {
    if ty.is_scalar() {
        return PackedSubscriptKind::Index(expression.index());
    }
    dimensions.extend_from_slice(ty.dimensions());
    PackedSubscriptKind::Slice(expression.index())
}

fn merged_binder_domain<'dae>(
    storage: &Storage,
    expressions: impl IntoIterator<Item = ExprId<'dae>>,
    at: DaeProvenance,
) -> Result<Option<u32>, DaeConstructionError> {
    expressions
        .into_iter()
        .try_fold(None, |domain, expression| {
            merge_binder_domain(
                storage,
                domain,
                storage.expr_binder_domain(expression, at)?,
                at,
            )
        })
}

fn merge_binder_domain(
    storage: &Storage,
    lhs: Option<u32>,
    rhs: Option<u32>,
    at: DaeProvenance,
) -> Result<Option<u32>, DaeConstructionError> {
    match (lhs, rhs) {
        (None, domain) | (domain, None) => Ok(domain),
        (Some(lhs), Some(rhs)) if lhs == rhs => Ok(Some(lhs)),
        (Some(lhs), Some(rhs)) if storage.domain_is_ancestor_or_same(lhs, rhs, at)? => {
            Ok(Some(rhs))
        }
        (Some(lhs), Some(rhs)) if storage.domain_is_ancestor_or_same(rhs, lhs, at)? => {
            Ok(Some(lhs))
        }
        (Some(expected), Some(found)) => Err(DaeConstructionError::InvalidBinderScope {
            expected_domain: Some(expected),
            found_domain: found,
            span: at.span(),
        }),
    }
}

fn max_variability(
    storage: &Storage,
    expressions: &[ExprId<'_>],
    at: DaeProvenance,
) -> Result<ExpressionVariability, DaeConstructionError> {
    expressions
        .iter()
        .try_fold(ExpressionVariability::Constant, |maximum, expression| {
            Ok(maximum.max(storage.expr_variability(*expression, at)?))
        })
}

fn validate_static_quotient(
    storage: &Storage,
    builtin: PureBuiltin,
    arguments: &[ExprId<'_>],
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let [lhs, rhs] = arguments else {
        unreachable!("builtin result validation proves quotient arity")
    };
    let operator = quotient_name(builtin);
    let Some(lhs) = static_numeric_value(storage, lhs.index()) else {
        return Err(DaeConstructionError::NonStaticDiscontinuity {
            operator,
            span: at.span(),
        });
    };
    let Some(rhs) = static_numeric_value(storage, rhs.index()) else {
        return Err(DaeConstructionError::NonStaticDiscontinuity {
            operator,
            span: at.span(),
        });
    };
    let function = match builtin {
        PureBuiltin::Div => rumoca_core::BuiltinFunction::Div,
        PureBuiltin::Mod => rumoca_core::BuiltinFunction::Mod,
        PureBuiltin::Rem => rumoca_core::BuiltinFunction::Rem,
        _ => unreachable!("caller restricts static quotient validation"),
    };
    let result = rumoca_core::apply_scalar_binary_math(function, lhs, rhs);
    if result.is_some_and(f64::is_finite) {
        Ok(())
    } else {
        Err(DaeConstructionError::UndefinedBuiltinDomain {
            operator,
            span: at.span(),
        })
    }
}

fn quotient_name(builtin: PureBuiltin) -> &'static str {
    match builtin {
        PureBuiltin::Div => "div",
        PureBuiltin::Mod => "mod",
        PureBuiltin::Rem => "rem",
        _ => unreachable!("caller restricts quotient builtins"),
    }
}

fn static_numeric_value(storage: &Storage, expression: u32) -> Option<f64> {
    let node = storage.expressions.nodes.get(expression as usize)?;
    let value = match node {
        ExprNode::Literal(DaeLiteral::Real(value)) => *value,
        ExprNode::Literal(DaeLiteral::Integer(value) | DaeLiteral::Enumeration(value)) => {
            *value as f64
        }
        ExprNode::Unary { operator, operand } => {
            let operand = static_numeric_value(storage, *operand)?;
            match operator {
                UnaryOperator::Plus => operand,
                UnaryOperator::Negate => -operand,
                UnaryOperator::Not => return None,
            }
        }
        ExprNode::Binary { operator, lhs, rhs } => {
            let lhs = static_numeric_value(storage, *lhs)?;
            let rhs = static_numeric_value(storage, *rhs)?;
            match operator {
                BinaryOperator::Add | BinaryOperator::ElementwiseAdd => lhs + rhs,
                BinaryOperator::Subtract | BinaryOperator::ElementwiseSubtract => lhs - rhs,
                BinaryOperator::Multiply | BinaryOperator::ElementwiseMultiply => lhs * rhs,
                BinaryOperator::Divide | BinaryOperator::ElementwiseDivide if rhs != 0.0 => {
                    lhs / rhs
                }
                BinaryOperator::Power | BinaryOperator::ElementwisePower => lhs.powf(rhs),
                _ => return None,
            }
        }
        ExprNode::Builtin { builtin, operands }
            if matches!(
                builtin,
                PureBuiltin::Div | PureBuiltin::Mod | PureBuiltin::Rem
            ) =>
        {
            let mut operands = storage.expressions.operands[operands.indices()].iter();
            let lhs = static_numeric_value(storage, *operands.next()?)?;
            let rhs = static_numeric_value(storage, *operands.next()?)?;
            let function = match builtin {
                PureBuiltin::Div => rumoca_core::BuiltinFunction::Div,
                PureBuiltin::Mod => rumoca_core::BuiltinFunction::Mod,
                PureBuiltin::Rem => rumoca_core::BuiltinFunction::Rem,
                _ => unreachable!("guard restricts quotient builtins"),
            };
            rumoca_core::apply_scalar_binary_math(function, lhs, rhs)?
        }
        _ => return None,
    };
    value.is_finite().then_some(value)
}

fn binary_result(
    operator: BinaryOperator,
    lhs: &ValueType,
    rhs: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    let lhs_scalar = lhs.scalar_type();
    let rhs_scalar = rhs.scalar_type();
    match operator {
        BinaryOperator::And | BinaryOperator::Or => {
            expect_same_shape(lhs, rhs, at)?;
            if lhs_scalar != ScalarType::Boolean {
                return Err(type_mismatch(ScalarType::Boolean, lhs_scalar, at));
            }
            if rhs_scalar != ScalarType::Boolean {
                return Err(type_mismatch(ScalarType::Boolean, rhs_scalar, at));
            }
            Ok(lhs.clone())
        }
        BinaryOperator::Add
        | BinaryOperator::Subtract
        | BinaryOperator::ElementwiseAdd
        | BinaryOperator::ElementwiseSubtract => {
            expect_same_shape(lhs, rhs, at)?;
            expect_numeric(lhs_scalar, at)?;
            expect_numeric(rhs_scalar, at)?;
            let scalar = promoted_numeric_scalar(lhs_scalar, rhs_scalar, false);
            Ok(ValueType::array(scalar, lhs.dimensions().to_vec()))
        }
        BinaryOperator::Multiply => multiplication_result(lhs, rhs, at),
        BinaryOperator::Divide => division_result(lhs, rhs, at),
        BinaryOperator::Power => power_result(lhs, rhs, at),
        BinaryOperator::ElementwiseMultiply
        | BinaryOperator::ElementwiseDivide
        | BinaryOperator::ElementwisePower => elementwise_result(operator, lhs, rhs, at),
        BinaryOperator::Equal
        | BinaryOperator::NotEqual
        | BinaryOperator::Less
        | BinaryOperator::LessEqual
        | BinaryOperator::Greater
        | BinaryOperator::GreaterEqual => {
            expect_same_shape(lhs, rhs, at)?;
            if lhs_scalar != rhs_scalar && !(lhs_scalar.is_numeric() && rhs_scalar.is_numeric()) {
                return Err(type_mismatch(lhs_scalar, rhs_scalar, at));
            }
            Ok(ValueType::array(
                ScalarType::Boolean,
                lhs.dimensions().to_vec(),
            ))
        }
    }
}

fn expect_same_shape(
    lhs: &ValueType,
    rhs: &ValueType,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    if lhs.dimensions() == rhs.dimensions() {
        Ok(())
    } else {
        Err(DaeConstructionError::ShapeMismatch { span: at.span() })
    }
}

fn promoted_numeric_scalar(lhs: ScalarType, rhs: ScalarType, force_real: bool) -> ScalarType {
    if force_real || lhs == ScalarType::Real || rhs == ScalarType::Real {
        ScalarType::Real
    } else {
        ScalarType::Integer
    }
}

fn multiplication_result(
    lhs: &ValueType,
    rhs: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    expect_numeric(lhs.scalar_type(), at)?;
    expect_numeric(rhs.scalar_type(), at)?;
    let dimensions = match (lhs.dimensions(), rhs.dimensions()) {
        ([], rhs) => rhs.to_vec(),
        (lhs, []) => lhs.to_vec(),
        ([lhs_n], [rhs_n]) if lhs_n == rhs_n => Vec::new(),
        ([rows, inner], [rhs_inner]) if inner == rhs_inner => vec![*rows],
        ([lhs_inner], [rhs_inner, columns]) if lhs_inner == rhs_inner => vec![*columns],
        ([rows, inner], [rhs_inner, columns]) if inner == rhs_inner => {
            vec![*rows, *columns]
        }
        _ => return Err(DaeConstructionError::ShapeMismatch { span: at.span() }),
    };
    Ok(ValueType::array(
        promoted_numeric_scalar(lhs.scalar_type(), rhs.scalar_type(), false),
        dimensions,
    ))
}

fn division_result(
    lhs: &ValueType,
    rhs: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    expect_numeric(lhs.scalar_type(), at)?;
    expect_numeric(rhs.scalar_type(), at)?;
    if !rhs.is_scalar() {
        return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
    }
    Ok(ValueType::array(
        ScalarType::Real,
        lhs.dimensions().to_vec(),
    ))
}

fn power_result(
    lhs: &ValueType,
    rhs: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    expect_numeric(lhs.scalar_type(), at)?;
    expect_numeric(rhs.scalar_type(), at)?;
    if !rhs.is_scalar()
        || !(lhs.is_scalar()
            || matches!(lhs.dimensions(), [rows, columns] if rows == columns)
                && rhs.scalar_type() == ScalarType::Integer)
    {
        return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
    }
    Ok(ValueType::array(
        promoted_numeric_scalar(lhs.scalar_type(), rhs.scalar_type(), false),
        lhs.dimensions().to_vec(),
    ))
}

fn elementwise_result(
    operator: BinaryOperator,
    lhs: &ValueType,
    rhs: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    expect_numeric(lhs.scalar_type(), at)?;
    expect_numeric(rhs.scalar_type(), at)?;
    let dimensions = if lhs.is_scalar() {
        rhs.dimensions()
    } else if rhs.is_scalar() || lhs.dimensions() == rhs.dimensions() {
        lhs.dimensions()
    } else {
        return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
    };
    Ok(ValueType::array(
        promoted_numeric_scalar(
            lhs.scalar_type(),
            rhs.scalar_type(),
            matches!(operator, BinaryOperator::ElementwiseDivide),
        ),
        dimensions.to_vec(),
    ))
}

fn common_value_type(
    lhs: &ValueType,
    rhs: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    if lhs.dimensions() != rhs.dimensions() {
        return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
    }
    let scalar = if lhs.scalar_type() == rhs.scalar_type() {
        lhs.scalar_type()
    } else if lhs.scalar_type().is_numeric() && rhs.scalar_type().is_numeric() {
        ScalarType::Real
    } else {
        return Err(type_mismatch(lhs.scalar_type(), rhs.scalar_type(), at));
    };
    Ok(ValueType::array(scalar, lhs.dimensions().to_vec()))
}

fn expect_numeric(scalar: ScalarType, at: DaeProvenance) -> Result<(), DaeConstructionError> {
    if scalar.is_numeric() {
        Ok(())
    } else {
        Err(DaeConstructionError::ExpectedNumeric {
            found: scalar,
            span: at.span(),
        })
    }
}

fn builtin_result<'dae>(
    storage: &Storage,
    builtin: PureBuiltin,
    arguments: &[ExprId<'dae>],
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    let Some(first) = arguments.first().copied() else {
        return Err(invalid_arity(1, 0, at));
    };
    let first = storage.expr_type(first, at)?.clone();
    if builtin.has_shaped_result() {
        return shaped_builtin_result(storage, builtin, arguments, &first, at);
    }
    if builtin == PureBuiltin::NoEvent {
        expect_arity(arguments, 1, at)?;
        return Ok(first);
    }
    if builtin == PureBuiltin::Homotopy {
        return homotopy_result(storage, arguments, first, at);
    }
    if builtin == PureBuiltin::Size {
        return size_result(storage, arguments, first, at);
    }
    expect_numeric(first.scalar_type(), at)?;
    match builtin {
        PureBuiltin::Abs
        | PureBuiltin::Sign
        | PureBuiltin::Sqrt
        | PureBuiltin::Floor
        | PureBuiltin::Ceil
        | PureBuiltin::Sin
        | PureBuiltin::Cos
        | PureBuiltin::Tan
        | PureBuiltin::Asin
        | PureBuiltin::Acos
        | PureBuiltin::Atan
        | PureBuiltin::Sinh
        | PureBuiltin::Cosh
        | PureBuiltin::Tanh
        | PureBuiltin::Exp
        | PureBuiltin::Log
        | PureBuiltin::Log10 => {
            expect_arity(arguments, 1, at)?;
            Ok(first)
        }
        PureBuiltin::Integer => {
            expect_arity(arguments, 1, at)?;
            if !first.is_scalar() {
                return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
            }
            Ok(ValueType::scalar(ScalarType::Integer))
        }
        PureBuiltin::Atan2 | PureBuiltin::Div | PureBuiltin::Mod | PureBuiltin::Rem => {
            expect_arity(arguments, 2, at)?;
            common_value_type(&first, storage.expr_type(arguments[1], at)?, at)
        }
        PureBuiltin::Homotopy => unreachable!("homotopy returns after checking both branches"),
        PureBuiltin::Smooth => {
            expect_arity(arguments, 2, at)?;
            if !first.is_scalar() || first.scalar_type() != ScalarType::Integer {
                return Err(DaeConstructionError::InvalidSubscript { span: at.span() });
            }
            Ok(storage.expr_type(arguments[1], at)?.clone())
        }
        PureBuiltin::Sum | PureBuiltin::Product => {
            expect_arity(arguments, 1, at)?;
            Ok(ValueType::scalar(first.scalar_type()))
        }
        PureBuiltin::Min | PureBuiltin::Max if arguments.len() == 1 => {
            Ok(ValueType::scalar(first.scalar_type()))
        }
        PureBuiltin::Min | PureBuiltin::Max => {
            for argument in &arguments[1..] {
                if storage.expr_type(*argument, at)? != &first {
                    return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
                }
            }
            Ok(first)
        }
        PureBuiltin::Size => unreachable!("size returns after checking its dimension argument"),
        PureBuiltin::Zeros
        | PureBuiltin::Ones
        | PureBuiltin::Fill
        | PureBuiltin::Linspace
        | PureBuiltin::Cross => {
            unreachable!("array constructors return before numeric builtins")
        }
        PureBuiltin::NoEvent => {
            unreachable!("type-preserving noEvent returns before numeric builtin checks")
        }
    }
}

fn homotopy_result(
    storage: &Storage,
    arguments: &[ExprId<'_>],
    actual: ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    expect_arity(arguments, 2, at)?;
    let simplified = storage.expr_type(arguments[1], at)?;
    if !actual.is_scalar() || actual.scalar_type() != ScalarType::Real || simplified != &actual {
        return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
    }
    Ok(actual)
}

fn size_result(
    storage: &Storage,
    arguments: &[ExprId<'_>],
    array: ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    if arguments.len() == 1 {
        let rank = u32::try_from(array.dimensions().len()).map_err(|_| {
            DaeConstructionError::CapacityExceeded {
                arena: "value type rank",
                attempted_index: array.dimensions().len(),
                span: at.span(),
            }
        })?;
        return Ok(ValueType::array(ScalarType::Integer, [rank]));
    }
    expect_arity(arguments, 2, at)?;
    let dimension = storage.expr_type(arguments[1], at)?;
    if !dimension.is_scalar() || dimension.scalar_type() != ScalarType::Integer {
        return Err(DaeConstructionError::InvalidSubscript { span: at.span() });
    }
    Ok(ValueType::scalar(ScalarType::Integer))
}

fn shaped_builtin_result(
    storage: &Storage,
    builtin: PureBuiltin,
    arguments: &[ExprId<'_>],
    first: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    let (scalar, extents) = match builtin {
        PureBuiltin::Zeros | PureBuiltin::Ones => (ScalarType::Real, arguments),
        PureBuiltin::Fill if arguments.len() >= 2 && first.is_scalar() => {
            (first.scalar_type(), &arguments[1..])
        }
        PureBuiltin::Fill if arguments.len() < 2 => {
            return Err(invalid_arity(2, arguments.len(), at));
        }
        PureBuiltin::Fill => return Err(DaeConstructionError::ShapeMismatch { span: at.span() }),
        PureBuiltin::Linspace => {
            expect_arity(arguments, 3, at)?;
            let endpoints = common_value_type(first, storage.expr_type(arguments[1], at)?, at)?;
            let extent = literal_array_extent(storage, arguments[2], at)?;
            if !endpoints.is_scalar() {
                return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
            }
            if extent < 2 {
                return Err(DaeConstructionError::InvalidArrayExtent { span: at.span() });
            }
            return Ok(ValueType::array(ScalarType::Real, [extent]));
        }
        PureBuiltin::Cross => {
            expect_arity(arguments, 2, at)?;
            let result = common_value_type(first, storage.expr_type(arguments[1], at)?, at)?;
            if result.dimensions() != [3] {
                return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
            }
            return Ok(result);
        }
        _ => unreachable!("only compact shaped builtins use this validator"),
    };
    let dimensions = extents
        .iter()
        .copied()
        .map(|expression| literal_array_extent(storage, expression, at))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(ValueType::array(scalar, dimensions))
}

fn literal_array_extent(
    storage: &Storage,
    expression: ExprId<'_>,
    at: DaeProvenance,
) -> Result<u32, DaeConstructionError> {
    let value_type = storage.expr_type(expression, at)?;
    if !value_type.is_scalar() || value_type.scalar_type() != ScalarType::Integer {
        return Err(DaeConstructionError::InvalidArrayExtent { span: at.span() });
    }
    u32::try_from(
        storage
            .static_integer(expression)
            .ok_or(DaeConstructionError::InvalidArrayExtent { span: at.span() })?,
    )
    .map_err(|_| DaeConstructionError::InvalidArrayExtent { span: at.span() })
}

fn expect_arity(
    arguments: &[ExprId<'_>],
    expected: usize,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    if arguments.len() == expected {
        return Ok(());
    }
    Err(invalid_arity(expected, arguments.len(), at))
}

fn range_extent(
    start: i64,
    step: i64,
    stop: i64,
    at: DaeProvenance,
) -> Result<u32, DaeConstructionError> {
    if (step > 0 && start > stop) || (step < 0 && start < stop) {
        return Ok(0);
    }
    let distance = i128::from(stop) - i128::from(start);
    let steps = distance / i128::from(step);
    u32::try_from(steps + 1)
        .map_err(|_| DaeConstructionError::RangeExtentOverflow { span: at.span() })
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
