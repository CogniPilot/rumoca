mod directional;
mod tensor;
pub(in crate::typed_program) mod wire;

use std::marker::PhantomData;

use rumoca_core::{Span, StructuredIndexDomain};
use serde::{Deserialize, Serialize};

use super::call::{SolvePureCallInterface, SolvePureCallOwnerId};
use super::types::{SolveArithmeticProfile, SolveScalarType, SolveValue, SolveValueType};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SolveSlotId(u32);

impl SolveSlotId {
    #[must_use]
    pub const fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SolveRegisterId(u32);

impl SolveRegisterId {
    #[must_use]
    pub const fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ProgramSlot<'program> {
    id: SolveSlotId,
    marker: PhantomData<&'program mut &'program ()>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ProgramRegister<'program> {
    id: SolveRegisterId,
    marker: PhantomData<&'program mut &'program ()>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ProgramTensorViewAxis<'program> {
    /// One runtime, one-based Modelica index; this axis is absent from the result.
    Index(ProgramRegister<'program>),
    /// One zero-based contiguous interval retained in the result.
    Span { origin: u32, extent: u32 },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveTensorViewAxis {
    Index(SolveRegisterId),
    Span { origin: u32, extent: u32 },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveStorageClass {
    Input,
    Output,
    TunableParameter,
    CalculatedParameter,
    Constant,
    PersistentState,
    PreviousState,
    MethodLocal,
    SignalStatus,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveSlotAccess {
    ReadOnly,
    ReadWrite,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveSlot {
    id: SolveSlotId,
    value_type: SolveValueType,
    storage: SolveStorageClass,
    access: SolveSlotAccess,
    provenance: Span,
}

impl SolveSlot {
    #[must_use]
    pub const fn id(&self) -> SolveSlotId {
        self.id
    }

    #[must_use]
    pub const fn value_type(&self) -> &SolveValueType {
        &self.value_type
    }

    #[must_use]
    pub const fn storage(&self) -> SolveStorageClass {
        self.storage
    }

    #[must_use]
    pub const fn access(&self) -> SolveSlotAccess {
        self.access
    }

    #[must_use]
    pub const fn provenance(&self) -> Span {
        self.provenance
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveUnaryOperator {
    Negate,
    Not,
    Abs,
    Sign,
    Sqrt,
    Floor,
    Ceiling,
    Truncate,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Sinh,
    Cosh,
    Tanh,
    Exp,
    Log,
    Log10,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveBinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
    And,
    Or,
    Atan2,
    Min,
    Max,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveCompareOperator {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveConversionOperator {
    IntegerToReal,
    RealToIntegerTowardZero,
    RealToIntegerTowardNegativeInfinity,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveReductionOperator {
    Sum,
    Product,
    Minimum,
    Maximum,
    All,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "operation", rename_all = "snake_case")]
pub enum SolveOperation {
    Constant {
        destination: SolveRegisterId,
        value: SolveValue,
    },
    Load {
        destination: SolveRegisterId,
        slot: SolveSlotId,
    },
    Store {
        slot: SolveSlotId,
        source: SolveRegisterId,
    },
    Unary {
        destination: SolveRegisterId,
        operator: SolveUnaryOperator,
        operand: SolveRegisterId,
    },
    Binary {
        destination: SolveRegisterId,
        operator: SolveBinaryOperator,
        lhs: SolveRegisterId,
        rhs: SolveRegisterId,
    },
    Compare {
        destination: SolveRegisterId,
        operator: SolveCompareOperator,
        lhs: SolveRegisterId,
        rhs: SolveRegisterId,
    },
    Convert {
        destination: SolveRegisterId,
        operator: SolveConversionOperator,
        operand: SolveRegisterId,
    },
    Select {
        destination: SolveRegisterId,
        condition: SolveRegisterId,
        if_true: SolveRegisterId,
        if_false: SolveRegisterId,
    },
    /// One lazy structured conditional over exact compact captures/results.
    Conditional {
        condition: SolveRegisterId,
        captures: Box<[SolveRegisterId]>,
        destinations: Box<[SolveRegisterId]>,
        if_true: Box<SolveProgramRegion>,
        if_false: Box<SolveProgramRegion>,
    },
    /// One pointwise tensor map over a compact structured domain.
    Map {
        domain: StructuredIndexDomain,
        captures: Box<[SolveRegisterId]>,
        destination: SolveRegisterId,
        body: Box<SolveProgramRegion>,
    },
    /// One finite compact-domain loop with an explicit carried tuple.
    Fold {
        domain: StructuredIndexDomain,
        initial: Box<[SolveRegisterId]>,
        captures: Box<[SolveRegisterId]>,
        destinations: Box<[SolveRegisterId]>,
        transition: Box<SolveProgramRegion>,
    },
    /// Elementwise multiplication of one aggregate by one scalar.
    Scale {
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        scalar: SolveRegisterId,
    },
    /// One elementwise tensor/scalar binary operation with scalar broadcasting.
    BroadcastBinary {
        destination: SolveRegisterId,
        operator: SolveBinaryOperator,
        aggregate: SolveRegisterId,
        scalar: SolveRegisterId,
        scalar_on_lhs: bool,
    },
    /// MLS transpose of the first two axes, retaining every trailing axis.
    Transpose {
        destination: SolveRegisterId,
        operand: SolveRegisterId,
    },
    /// MLS vector/matrix product over one shared inner dimension.
    MatrixMultiply {
        destination: SolveRegisterId,
        lhs: SolveRegisterId,
        rhs: SolveRegisterId,
    },
    /// MLS cross product over two numeric three-vectors.
    Cross {
        destination: SolveRegisterId,
        lhs: SolveRegisterId,
        rhs: SolveRegisterId,
    },
    /// One aggregate reduction without an IR-level coordinate list.
    Reduce {
        destination: SolveRegisterId,
        operator: SolveReductionOperator,
        operand: SolveRegisterId,
    },
    /// One square Real identity tensor; shape is owned by the destination type.
    Identity { destination: SolveRegisterId },
    /// One vector-to-square-matrix diagonal construction.
    Diagonal {
        destination: SolveRegisterId,
        operand: SolveRegisterId,
    },
    /// One axis concatenation over compact same-rank tensor operands.
    Concatenate {
        destination: SolveRegisterId,
        axis: u32,
        operands: Box<[SolveRegisterId]>,
    },
    Fill {
        destination: SolveRegisterId,
        value: SolveRegisterId,
    },
    ConstructAggregate {
        destination: SolveRegisterId,
        elements: Box<[SolveRegisterId]>,
    },
    ProjectElement {
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        /// Zero-based checked IR coordinates.
        indices: Box<[u32]>,
    },
    /// One runtime-indexed projection with explicit bounds failure.
    ProjectElementDynamic {
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        /// Scalar Integer registers containing one-based Modelica coordinates.
        indices: Box<[SolveRegisterId]>,
    },
    ProjectSlice {
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        /// Zero-based checked IR origin.
        origin: Box<[u32]>,
    },
    /// One rank-preserving or rank-reducing affine tensor view.
    ProjectView {
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        axes: Box<[SolveTensorViewAxis]>,
    },
    SelectElement {
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        /// Scalar Integer registers containing one-based Modelica coordinates.
        indices: Box<[SolveRegisterId]>,
        out_of_range: SolveRegisterId,
    },
    /// One functional aggregate update at one-based Modelica coordinates.
    UpdateElement {
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        value: SolveRegisterId,
        indices: Box<[SolveRegisterId]>,
    },
    /// One functional compact slice update at a zero-based checked origin.
    UpdateSlice {
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        value: SolveRegisterId,
        origin: Box<[u32]>,
    },
    /// One functional affine tensor-view update.
    UpdateView {
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        value: SolveRegisterId,
        axes: Box<[SolveTensorViewAxis]>,
    },
    /// One atomic invocation of a compiler-issued pure-call owner.
    ///
    /// Aggregate arguments and results each retain one typed register. The
    /// destinations are ordered value results followed by assertion
    /// predicates exactly as declared by the owner interface.
    Call {
        owner: SolvePureCallOwnerId,
        arguments: Box<[SolveRegisterId]>,
        destinations: Box<[SolveRegisterId]>,
    },
}

impl SolveOperation {
    /// Visits every register read by this operation in semantic operand order.
    ///
    /// This is the closed-vocabulary source of truth for checked backend
    /// liveness. Region bodies own separate register namespaces, so structured
    /// operations visit only their captures and other enclosing-program
    /// operands here.
    pub fn visit_input_registers(&self, mut visit: impl FnMut(SolveRegisterId)) {
        match self {
            Self::Constant { .. } | Self::Load { .. } | Self::Identity { .. } => {}
            Self::Store { source, .. } => visit(*source),
            Self::Unary { operand, .. }
            | Self::Convert { operand, .. }
            | Self::Transpose { operand, .. }
            | Self::Reduce { operand, .. }
            | Self::Diagonal { operand, .. } => visit(*operand),
            Self::Binary { lhs, rhs, .. }
            | Self::Compare { lhs, rhs, .. }
            | Self::MatrixMultiply { lhs, rhs, .. }
            | Self::Cross { lhs, rhs, .. } => visit_register_pair(*lhs, *rhs, &mut visit),
            Self::Select {
                condition,
                if_true,
                if_false,
                ..
            } => {
                visit(*condition);
                visit(*if_true);
                visit(*if_false);
            }
            Self::Conditional {
                condition,
                captures,
                ..
            } => visit_register_then_list(*condition, captures, &mut visit),
            Self::Map { captures, .. } => captures.iter().copied().for_each(&mut visit),
            Self::Fold {
                initial, captures, ..
            } => {
                initial.iter().copied().for_each(&mut visit);
                captures.iter().copied().for_each(&mut visit);
            }
            Self::Scale {
                aggregate, scalar, ..
            }
            | Self::BroadcastBinary {
                aggregate, scalar, ..
            } => visit_register_pair(*aggregate, *scalar, &mut visit),
            Self::Concatenate { operands, .. }
            | Self::ConstructAggregate {
                elements: operands, ..
            } => operands.iter().copied().for_each(&mut visit),
            Self::Fill { value, .. } => visit(*value),
            Self::ProjectElement { aggregate, .. } | Self::ProjectSlice { aggregate, .. } => {
                visit(*aggregate)
            }
            Self::ProjectElementDynamic {
                aggregate, indices, ..
            } => visit_register_then_list(*aggregate, indices, &mut visit),
            Self::ProjectView {
                aggregate, axes, ..
            } => {
                visit(*aggregate);
                visit_view_axis_registers(axes, &mut visit);
            }
            Self::SelectElement {
                aggregate,
                indices,
                out_of_range,
                ..
            } => {
                visit_register_then_list(*aggregate, indices, &mut visit);
                visit(*out_of_range);
            }
            Self::UpdateElement {
                aggregate,
                value,
                indices,
                ..
            } => visit_register_pair_then_list(*aggregate, *value, indices, &mut visit),
            Self::UpdateSlice {
                aggregate, value, ..
            } => visit_register_pair(*aggregate, *value, &mut visit),
            Self::UpdateView {
                aggregate,
                value,
                axes,
                ..
            } => {
                visit(*aggregate);
                visit(*value);
                visit_view_axis_registers(axes, &mut visit);
            }
            Self::Call { arguments, .. } => arguments.iter().copied().for_each(&mut visit),
        }
    }

    /// Visits every register defined by this operation in result-tuple order.
    pub fn visit_output_registers(&self, mut visit: impl FnMut(SolveRegisterId)) {
        match self {
            Self::Store { .. } => {}
            Self::Constant { destination, .. }
            | Self::Load { destination, .. }
            | Self::Unary { destination, .. }
            | Self::Binary { destination, .. }
            | Self::Compare { destination, .. }
            | Self::Convert { destination, .. }
            | Self::Select { destination, .. }
            | Self::Map { destination, .. }
            | Self::MatrixMultiply { destination, .. }
            | Self::Scale { destination, .. }
            | Self::BroadcastBinary { destination, .. }
            | Self::Transpose { destination, .. }
            | Self::Cross { destination, .. }
            | Self::Reduce { destination, .. }
            | Self::Identity { destination }
            | Self::Diagonal { destination, .. }
            | Self::Concatenate { destination, .. }
            | Self::Fill { destination, .. }
            | Self::ConstructAggregate { destination, .. }
            | Self::ProjectElement { destination, .. }
            | Self::ProjectElementDynamic { destination, .. }
            | Self::ProjectSlice { destination, .. }
            | Self::ProjectView { destination, .. }
            | Self::SelectElement { destination, .. }
            | Self::UpdateElement { destination, .. }
            | Self::UpdateSlice { destination, .. }
            | Self::UpdateView { destination, .. } => visit(*destination),
            Self::Conditional { destinations, .. }
            | Self::Fold { destinations, .. }
            | Self::Call { destinations, .. } => {
                destinations.iter().copied().for_each(&mut visit);
            }
        }
    }
}

fn visit_view_axis_registers(
    axes: &[SolveTensorViewAxis],
    visit: &mut impl FnMut(SolveRegisterId),
) {
    for axis in axes {
        if let SolveTensorViewAxis::Index(register) = axis {
            visit(*register);
        }
    }
}

/// Visits two operand registers in semantic order.
fn visit_register_pair(
    first: SolveRegisterId,
    second: SolveRegisterId,
    visit: &mut impl FnMut(SolveRegisterId),
) {
    visit(first);
    visit(second);
}

/// Visits one leading operand register, then a compact operand list.
fn visit_register_then_list(
    first: SolveRegisterId,
    registers: &[SolveRegisterId],
    visit: &mut impl FnMut(SolveRegisterId),
) {
    visit(first);
    registers.iter().copied().for_each(visit);
}

/// Visits two leading operand registers, then a compact operand list.
fn visit_register_pair_then_list(
    first: SolveRegisterId,
    second: SolveRegisterId,
    registers: &[SolveRegisterId],
    visit: &mut impl FnMut(SolveRegisterId),
) {
    visit(first);
    visit(second);
    registers.iter().copied().for_each(visit);
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveProgramRegion {
    inputs: Box<[SolveValueType]>,
    outputs: Box<[SolveValueType]>,
    body: TypedProgram,
    provenance: Span,
}

impl SolveProgramRegion {
    #[must_use]
    pub const fn inputs(&self) -> &[SolveValueType] {
        &self.inputs
    }

    #[must_use]
    pub const fn outputs(&self) -> &[SolveValueType] {
        &self.outputs
    }

    #[must_use]
    pub const fn body(&self) -> &TypedProgram {
        &self.body
    }

    #[must_use]
    pub const fn provenance(&self) -> Span {
        self.provenance
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveSpannedOperation {
    operation: SolveOperation,
    provenance: Span,
}

impl SolveSpannedOperation {
    #[must_use]
    pub const fn operation(&self) -> &SolveOperation {
        &self.operation
    }

    #[must_use]
    pub const fn provenance(&self) -> Span {
        self.provenance
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TypedProgram {
    arithmetic: SolveArithmeticProfile,
    slots: Box<[SolveSlot]>,
    register_types: Box<[SolveValueType]>,
    operations: Box<[SolveSpannedOperation]>,
}

impl TypedProgram {
    pub fn construct(
        arithmetic: SolveArithmeticProfile,
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Self, SolveProgramConstructionError> {
        let mut builder = TypedProgramBuilder {
            arithmetic,
            slots: Vec::new(),
            slot_initialized: Vec::new(),
            register_types: Vec::new(),
            operations: Vec::new(),
            available_calls: Vec::new(),
            marker: PhantomData,
        };
        build(&mut builder)?;
        Ok(Self {
            arithmetic,
            slots: builder.slots.into_boxed_slice(),
            register_types: builder.register_types.into_boxed_slice(),
            operations: builder.operations.into_boxed_slice(),
        })
    }

    pub(super) fn construct_with_calls(
        arithmetic: SolveArithmeticProfile,
        available_calls: Vec<SolvePureCallInterface>,
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Self, SolveProgramConstructionError> {
        let mut builder = TypedProgramBuilder {
            arithmetic,
            slots: Vec::new(),
            slot_initialized: Vec::new(),
            register_types: Vec::new(),
            operations: Vec::new(),
            available_calls,
            marker: PhantomData,
        };
        build(&mut builder)?;
        Ok(Self {
            arithmetic,
            slots: builder.slots.into_boxed_slice(),
            register_types: builder.register_types.into_boxed_slice(),
            operations: builder.operations.into_boxed_slice(),
        })
    }

    #[must_use]
    pub const fn arithmetic(&self) -> SolveArithmeticProfile {
        self.arithmetic
    }

    #[must_use]
    pub fn slots(&self) -> &[SolveSlot] {
        &self.slots
    }

    #[must_use]
    pub fn register_types(&self) -> &[SolveValueType] {
        &self.register_types
    }

    #[must_use]
    pub fn operations(&self) -> &[SolveSpannedOperation] {
        &self.operations
    }
}

pub struct TypedProgramBuilder<'program> {
    arithmetic: SolveArithmeticProfile,
    slots: Vec<SolveSlot>,
    slot_initialized: Vec<bool>,
    register_types: Vec<SolveValueType>,
    operations: Vec<SolveSpannedOperation>,
    available_calls: Vec<SolvePureCallInterface>,
    marker: PhantomData<&'program mut &'program ()>,
}

impl<'program> TypedProgramBuilder<'program> {
    pub fn declare_slot(
        &mut self,
        value_type: SolveValueType,
        storage: SolveStorageClass,
        access: SolveSlotAccess,
        provenance: Span,
    ) -> Result<ProgramSlot<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        if !value_type.belongs_to(self.arithmetic) {
            return Err(SolveProgramConstructionError::ProfileMismatch { provenance });
        }
        if storage == SolveStorageClass::Constant && access != SolveSlotAccess::ReadOnly {
            return Err(SolveProgramConstructionError::WritableConstant { provenance });
        }
        let id = SolveSlotId(checked_ordinal(self.slots.len(), provenance)?);
        self.slots.push(SolveSlot {
            id,
            value_type,
            storage,
            access,
            provenance,
        });
        self.slot_initialized.push(!matches!(
            storage,
            SolveStorageClass::Output | SolveStorageClass::MethodLocal
        ));
        Ok(ProgramSlot {
            id,
            marker: PhantomData,
        })
    }

    pub fn constant(
        &mut self,
        value: SolveValue,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        if !value.value_type().belongs_to(self.arithmetic) {
            return Err(SolveProgramConstructionError::ProfileMismatch { provenance });
        }
        let destination = self.issue_register(value.value_type().clone(), provenance)?;
        self.push(
            SolveOperation::Constant {
                destination: destination.id,
                value,
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn load(
        &mut self,
        slot: ProgramSlot<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let owned_slot = self.slot(slot, provenance)?;
        if !self
            .slot_initialized
            .get(owned_slot.id.index())
            .copied()
            .unwrap_or(false)
        {
            return Err(SolveProgramConstructionError::UninitializedSlot { provenance });
        }
        let value_type = owned_slot.value_type.clone();
        let destination = self.issue_register(value_type, provenance)?;
        self.push(
            SolveOperation::Load {
                destination: destination.id,
                slot: slot.id,
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn store(
        &mut self,
        slot: ProgramSlot<'program>,
        source: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let slot = self.slot(slot, provenance)?;
        if slot.access != SolveSlotAccess::ReadWrite {
            return Err(SolveProgramConstructionError::ReadOnlyStore { provenance });
        }
        let source_type = self.register_type(source, provenance)?;
        if slot.value_type != *source_type {
            return Err(SolveProgramConstructionError::TypeMismatch { provenance });
        }
        let slot_id = slot.id;
        self.push(
            SolveOperation::Store {
                slot: slot_id,
                source: source.id,
            },
            provenance,
        );
        self.slot_initialized[slot_id.index()] = true;
        Ok(())
    }

    pub fn unary(
        &mut self,
        operator: SolveUnaryOperator,
        operand: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let value_type = self.register_type(operand, provenance)?.clone();
        let valid = match operator {
            SolveUnaryOperator::Negate | SolveUnaryOperator::Abs | SolveUnaryOperator::Sign => {
                value_type.element_type().is_numeric()
            }
            SolveUnaryOperator::Not => value_type.element_type() == SolveScalarType::Boolean,
            SolveUnaryOperator::Sqrt
            | SolveUnaryOperator::Floor
            | SolveUnaryOperator::Ceiling
            | SolveUnaryOperator::Truncate
            | SolveUnaryOperator::Sin
            | SolveUnaryOperator::Cos
            | SolveUnaryOperator::Tan
            | SolveUnaryOperator::Asin
            | SolveUnaryOperator::Acos
            | SolveUnaryOperator::Atan
            | SolveUnaryOperator::Sinh
            | SolveUnaryOperator::Cosh
            | SolveUnaryOperator::Tanh
            | SolveUnaryOperator::Exp
            | SolveUnaryOperator::Log
            | SolveUnaryOperator::Log10 => {
                matches!(value_type.element_type(), SolveScalarType::Real { .. })
            }
        };
        if !valid {
            return Err(SolveProgramConstructionError::TypeMismatch { provenance });
        }
        let destination = self.issue_register(value_type, provenance)?;
        self.push(
            SolveOperation::Unary {
                destination: destination.id,
                operator,
                operand: operand.id,
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn binary(
        &mut self,
        operator: SolveBinaryOperator,
        lhs: ProgramRegister<'program>,
        rhs: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let lhs_type = self.register_type(lhs, provenance)?.clone();
        let rhs_type = self.register_type(rhs, provenance)?;
        let valid_elements = binary_operator_accepts(operator, lhs_type.element_type());
        if lhs_type != *rhs_type || !valid_elements {
            return Err(SolveProgramConstructionError::TypeMismatch { provenance });
        }
        let destination = self.issue_register(lhs_type, provenance)?;
        self.push(
            SolveOperation::Binary {
                destination: destination.id,
                operator,
                lhs: lhs.id,
                rhs: rhs.id,
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn compare(
        &mut self,
        operator: SolveCompareOperator,
        lhs: ProgramRegister<'program>,
        rhs: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let lhs_type = self.register_type(lhs, provenance)?.clone();
        let rhs_type = self.register_type(rhs, provenance)?;
        let ordered = !matches!(
            operator,
            SolveCompareOperator::Equal | SolveCompareOperator::NotEqual
        );
        if lhs_type != *rhs_type || (ordered && !lhs_type.element_type().is_numeric()) {
            return Err(SolveProgramConstructionError::TypeMismatch { provenance });
        }
        let destination = self.issue_register(lhs_type.boolean_with_same_shape(), provenance)?;
        self.push(
            SolveOperation::Compare {
                destination: destination.id,
                operator,
                lhs: lhs.id,
                rhs: rhs.id,
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn convert(
        &mut self,
        operator: SolveConversionOperator,
        operand: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let operand_type = self.register_type(operand, provenance)?;
        let scalar = match (operator, operand_type.element_type()) {
            (SolveConversionOperator::IntegerToReal, SolveScalarType::Integer(_)) => {
                SolveScalarType::real(self.arithmetic)
            }
            (
                SolveConversionOperator::RealToIntegerTowardZero
                | SolveConversionOperator::RealToIntegerTowardNegativeInfinity,
                SolveScalarType::Real { .. },
            ) => SolveScalarType::integer(self.arithmetic),
            _ => return Err(SolveProgramConstructionError::TypeMismatch { provenance }),
        };
        let value_type = operand_type.with_element_type(scalar);
        let destination = self.issue_register(value_type, provenance)?;
        self.push(
            SolveOperation::Convert {
                destination: destination.id,
                operator,
                operand: operand.id,
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn select(
        &mut self,
        condition: ProgramRegister<'program>,
        if_true: ProgramRegister<'program>,
        if_false: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let condition_type = self.register_type(condition, provenance)?;
        let true_type = self.register_type(if_true, provenance)?.clone();
        let false_type = self.register_type(if_false, provenance)?;
        if condition_type.element_type() != SolveScalarType::Boolean
            || !condition_type.dimensions().is_empty()
            || true_type != *false_type
        {
            return Err(SolveProgramConstructionError::TypeMismatch { provenance });
        }
        let destination = self.issue_register(true_type, provenance)?;
        self.push(
            SolveOperation::Select {
                destination: destination.id,
                condition: condition.id,
                if_true: if_true.id,
                if_false: if_false.id,
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn conditional(
        &mut self,
        condition: ProgramRegister<'program>,
        captures: &[ProgramRegister<'program>],
        output_types: Vec<SolveValueType>,
        provenance: Span,
        build_true: impl for<'region> FnOnce(
            &mut TypedProgramBuilder<'region>,
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
        ) -> Result<(), SolveProgramConstructionError>,
        build_false: impl for<'region> FnOnce(
            &mut TypedProgramBuilder<'region>,
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let condition_type = self.register_type(condition, provenance)?;
        if condition_type.element_type() != SolveScalarType::Boolean
            || !condition_type.dimensions().is_empty()
            || output_types.is_empty()
            || output_types
                .iter()
                .any(|value_type| !value_type.belongs_to(self.arithmetic))
        {
            return Err(SolveProgramConstructionError::InvalidRegion { provenance });
        }
        let input_types = captures
            .iter()
            .map(|capture| self.register_type(*capture, provenance).cloned())
            .collect::<Result<Vec<_>, _>>()?;
        let if_true = self.build_region(
            input_types.clone(),
            output_types.clone(),
            provenance,
            build_true,
        )?;
        let if_false = self.build_region(input_types, output_types, provenance, build_false)?;
        self.conditional_from_regions(condition, captures, if_true, if_false, provenance)
    }

    pub fn fold(
        &mut self,
        domain: StructuredIndexDomain,
        initial: &[ProgramRegister<'program>],
        captures: &[ProgramRegister<'program>],
        provenance: Span,
        build: impl for<'region> FnOnce(
            &mut TypedProgramBuilder<'region>,
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        require_fold_domain(&domain, self.arithmetic, provenance)?;
        if initial.is_empty() {
            return Err(SolveProgramConstructionError::InvalidFold { provenance });
        }
        let carried_types = self.register_types_for(initial, provenance)?;
        let capture_types = self.register_types_for(captures, provenance)?;
        let binder_types = vec![
            SolveValueType::scalar(SolveScalarType::integer(self.arithmetic));
            domain.binders.len()
        ];
        let mut input_types = carried_types.clone();
        input_types.extend(capture_types.iter().cloned());
        input_types.extend(binder_types.iter().cloned());
        let carried_count = carried_types.len();
        let capture_count = capture_types.len();
        let transition = self.build_region(
            input_types,
            carried_types,
            provenance,
            |builder, inputs, outputs| {
                let (carried, remaining) = inputs.split_at(carried_count);
                let (captures, binders) = remaining.split_at(capture_count);
                build(builder, carried, captures, binders, outputs)
            },
        )?;
        self.fold_from_region(domain, initial, captures, transition, provenance)
    }

    pub fn map(
        &mut self,
        domain: StructuredIndexDomain,
        captures: &[ProgramRegister<'program>],
        body_type: SolveValueType,
        provenance: Span,
        build: impl for<'region> FnOnce(
            &mut TypedProgramBuilder<'region>,
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
            ProgramSlot<'region>,
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        require_map_domain(&domain, self.arithmetic, provenance)?;
        if !body_type.belongs_to(self.arithmetic) {
            return Err(SolveProgramConstructionError::InvalidMap { provenance });
        }
        let capture_types = self.register_types_for(captures, provenance)?;
        let binder_types = vec![
            SolveValueType::scalar(SolveScalarType::integer(self.arithmetic));
            domain.binders.len()
        ];
        let mut input_types = capture_types.clone();
        input_types.extend(binder_types);
        let capture_count = capture_types.len();
        let body = self.build_region(
            input_types,
            vec![body_type],
            provenance,
            |builder, inputs, outputs| {
                let (captures, binders) = inputs.split_at(capture_count);
                let [output] = outputs else {
                    unreachable!("checked map body owns one output")
                };
                build(builder, captures, binders, *output)
            },
        )?;
        self.map_from_region(domain, captures, body, provenance)
    }

    fn build_region(
        &self,
        inputs: Vec<SolveValueType>,
        outputs: Vec<SolveValueType>,
        provenance: Span,
        build: impl for<'region> FnOnce(
            &mut TypedProgramBuilder<'region>,
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<SolveProgramRegion, SolveProgramConstructionError> {
        let body = TypedProgram::construct_with_calls(
            self.arithmetic,
            self.available_calls.clone(),
            |builder| {
                let input_slots = declare_interface_slots(
                    builder,
                    &inputs,
                    SolveStorageClass::Input,
                    SolveSlotAccess::ReadOnly,
                    provenance,
                )?;
                let output_slots = declare_interface_slots(
                    builder,
                    &outputs,
                    SolveStorageClass::Output,
                    SolveSlotAccess::ReadWrite,
                    provenance,
                )?;
                build(builder, &input_slots, &output_slots)
            },
        )?;
        construct_region(inputs, outputs, body, provenance)
    }

    fn conditional_from_regions(
        &mut self,
        condition: ProgramRegister<'program>,
        captures: &[ProgramRegister<'program>],
        if_true: SolveProgramRegion,
        if_false: SolveProgramRegion,
        provenance: Span,
    ) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let condition_type = self.register_type(condition, provenance)?;
        let capture_types = captures
            .iter()
            .map(|capture| self.register_type(*capture, provenance).cloned())
            .collect::<Result<Vec<_>, _>>()?;
        if condition_type.element_type() != SolveScalarType::Boolean
            || !condition_type.dimensions().is_empty()
            || if_true.inputs != if_false.inputs
            || if_true.outputs != if_false.outputs
            || capture_types.as_slice() != if_true.inputs.as_ref()
            || if_true.body.arithmetic() != self.arithmetic
            || if_false.body.arithmetic() != self.arithmetic
            || if_true.provenance != provenance
            || if_false.provenance != provenance
        {
            return Err(SolveProgramConstructionError::InvalidRegion { provenance });
        }
        let destinations = if_true
            .outputs
            .iter()
            .cloned()
            .map(|value_type| self.issue_register(value_type, provenance))
            .collect::<Result<Vec<_>, _>>()?;
        self.push(
            SolveOperation::Conditional {
                condition: condition.id,
                captures: captures
                    .iter()
                    .map(|capture| capture.id)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                destinations: destinations
                    .iter()
                    .map(|destination| destination.id)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                if_true: Box::new(if_true),
                if_false: Box::new(if_false),
            },
            provenance,
        );
        Ok(destinations)
    }

    fn fold_from_region(
        &mut self,
        domain: StructuredIndexDomain,
        initial: &[ProgramRegister<'program>],
        captures: &[ProgramRegister<'program>],
        transition: SolveProgramRegion,
        provenance: Span,
    ) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        require_fold_domain(&domain, self.arithmetic, provenance)?;
        let carried_types = self.register_types_for(initial, provenance)?;
        let capture_types = self.register_types_for(captures, provenance)?;
        let binder_type = SolveValueType::scalar(SolveScalarType::integer(self.arithmetic));
        let expected_inputs = carried_types
            .iter()
            .chain(&capture_types)
            .cloned()
            .chain(std::iter::repeat_n(binder_type, domain.binders.len()))
            .collect::<Vec<_>>();
        if carried_types.is_empty()
            || transition.inputs.as_ref() != expected_inputs
            || transition.outputs.as_ref() != carried_types
            || transition.body.arithmetic() != self.arithmetic
            || transition.provenance != provenance
        {
            return Err(SolveProgramConstructionError::InvalidFold { provenance });
        }
        let destinations = carried_types
            .into_iter()
            .map(|value_type| self.issue_register(value_type, provenance))
            .collect::<Result<Vec<_>, _>>()?;
        self.push(
            SolveOperation::Fold {
                domain,
                initial: initial
                    .iter()
                    .map(|value| value.id)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                captures: captures
                    .iter()
                    .map(|value| value.id)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                destinations: destinations
                    .iter()
                    .map(|value| value.id)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                transition: Box::new(transition),
            },
            provenance,
        );
        Ok(destinations)
    }

    fn map_from_region(
        &mut self,
        domain: StructuredIndexDomain,
        captures: &[ProgramRegister<'program>],
        body: SolveProgramRegion,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        require_map_domain(&domain, self.arithmetic, provenance)?;
        let capture_types = self.register_types_for(captures, provenance)?;
        let binder_type = SolveValueType::scalar(SolveScalarType::integer(self.arithmetic));
        let expected_inputs = capture_types
            .iter()
            .cloned()
            .chain(std::iter::repeat_n(binder_type, domain.binders.len()))
            .collect::<Vec<_>>();
        let [body_type] = body.outputs.as_ref() else {
            return Err(SolveProgramConstructionError::InvalidMap { provenance });
        };
        if body.inputs.as_ref() != expected_inputs
            || body.body.arithmetic() != self.arithmetic
            || body.provenance != provenance
        {
            return Err(SolveProgramConstructionError::InvalidMap { provenance });
        }
        let mut dimensions = domain
            .extents()
            .map_err(|_| SolveProgramConstructionError::InvalidMap { provenance })?
            .into_iter()
            .map(|extent| {
                u32::try_from(extent)
                    .map_err(|_| SolveProgramConstructionError::InvalidMap { provenance })
            })
            .collect::<Result<Vec<_>, _>>()?;
        dimensions.extend_from_slice(body_type.dimensions());
        let result_type = SolveValueType::tensor(body_type.element_type(), dimensions)
            .map_err(|_| SolveProgramConstructionError::InvalidMap { provenance })?;
        let destination = self.issue_register(result_type, provenance)?;
        self.push(
            SolveOperation::Map {
                domain,
                captures: captures
                    .iter()
                    .map(|value| value.id)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                destination: destination.id,
                body: Box::new(body),
            },
            provenance,
        );
        Ok(destination)
    }

    fn register_types_for(
        &self,
        registers: &[ProgramRegister<'program>],
        provenance: Span,
    ) -> Result<Vec<SolveValueType>, SolveProgramConstructionError> {
        registers
            .iter()
            .map(|register| self.register_type(*register, provenance).cloned())
            .collect()
    }

    pub fn fill(
        &mut self,
        value: ProgramRegister<'program>,
        dimensions: Vec<u32>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let value_type = self.register_type(value, provenance)?;
        if !value_type.dimensions().is_empty() {
            return Err(SolveProgramConstructionError::TypeMismatch { provenance });
        }
        let aggregate_type = SolveValueType::tensor(value_type.element_type(), dimensions)
            .map_err(|_| SolveProgramConstructionError::InvalidAggregate { provenance })?;
        let destination = self.issue_register(aggregate_type, provenance)?;
        self.push(
            SolveOperation::Fill {
                destination: destination.id,
                value: value.id,
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn construct_aggregate(
        &mut self,
        elements: &[ProgramRegister<'program>],
        dimensions: Vec<u32>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let aggregate_type = self.aggregate_type(elements, dimensions, provenance)?;
        let destination = self.issue_register(aggregate_type, provenance)?;
        self.push(
            SolveOperation::ConstructAggregate {
                destination: destination.id,
                elements: elements
                    .iter()
                    .map(|element| element.id)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn project_element(
        &mut self,
        aggregate: ProgramRegister<'program>,
        indices: Vec<u32>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let aggregate_type = self.register_type(aggregate, provenance)?;
        if !indices_in_bounds(aggregate_type.dimensions(), &indices) {
            return Err(SolveProgramConstructionError::InvalidProjection { provenance });
        }
        let value_type = SolveValueType::scalar(aggregate_type.element_type());
        let destination = self.issue_register(value_type, provenance)?;
        self.push(
            SolveOperation::ProjectElement {
                destination: destination.id,
                aggregate: aggregate.id,
                indices: indices.into_boxed_slice(),
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn project_element_dynamic(
        &mut self,
        aggregate: ProgramRegister<'program>,
        indices: &[ProgramRegister<'program>],
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let aggregate_type = self.register_type(aggregate, provenance)?;
        let index_types_are_valid = indices.iter().all(|index| {
            self.register_type(*index, provenance)
                .is_ok_and(|value_type| {
                    value_type.dimensions().is_empty()
                        && matches!(value_type.element_type(), SolveScalarType::Integer(_))
                })
        });
        if aggregate_type.dimensions().len() != indices.len() || !index_types_are_valid {
            return Err(SolveProgramConstructionError::InvalidProjection { provenance });
        }
        let value_type = SolveValueType::scalar(aggregate_type.element_type());
        let destination = self.issue_register(value_type, provenance)?;
        self.push(
            SolveOperation::ProjectElementDynamic {
                destination: destination.id,
                aggregate: aggregate.id,
                indices: indices
                    .iter()
                    .map(|index| index.id)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn project_slice(
        &mut self,
        aggregate: ProgramRegister<'program>,
        origin: Vec<u32>,
        dimensions: Vec<u32>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let aggregate_type = self.register_type(aggregate, provenance)?;
        let slice_type = SolveValueType::tensor(aggregate_type.element_type(), dimensions)
            .map_err(|_| SolveProgramConstructionError::InvalidProjection { provenance })?;
        if !slice_in_bounds(
            aggregate_type.dimensions(),
            &origin,
            slice_type.dimensions(),
        ) {
            return Err(SolveProgramConstructionError::InvalidProjection { provenance });
        }
        let destination = self.issue_register(slice_type, provenance)?;
        self.push(
            SolveOperation::ProjectSlice {
                destination: destination.id,
                aggregate: aggregate.id,
                origin: origin.into_boxed_slice(),
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn select_element(
        &mut self,
        aggregate: ProgramRegister<'program>,
        indices: &[ProgramRegister<'program>],
        out_of_range: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let aggregate_type = self.register_type(aggregate, provenance)?.clone();
        let fallback_type = self.register_type(out_of_range, provenance)?;
        let element_type = SolveValueType::scalar(aggregate_type.element_type());
        let index_types_are_valid = indices.iter().all(|index| {
            self.register_type(*index, provenance)
                .is_ok_and(|value_type| {
                    value_type.dimensions().is_empty()
                        && matches!(value_type.element_type(), SolveScalarType::Integer(_))
                })
        });
        if aggregate_type.dimensions().len() != indices.len()
            || *fallback_type != element_type
            || !index_types_are_valid
        {
            return Err(SolveProgramConstructionError::InvalidProjection { provenance });
        }
        let destination = self.issue_register(element_type, provenance)?;
        self.push(
            SolveOperation::SelectElement {
                destination: destination.id,
                aggregate: aggregate.id,
                indices: indices
                    .iter()
                    .map(|index| index.id)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                out_of_range: out_of_range.id,
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn update_element(
        &mut self,
        aggregate: ProgramRegister<'program>,
        value: ProgramRegister<'program>,
        indices: &[ProgramRegister<'program>],
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let aggregate_type = self.register_type(aggregate, provenance)?.clone();
        let value_type = self.register_type(value, provenance)?;
        let element_type = SolveValueType::scalar(aggregate_type.element_type());
        let index_types_are_valid = indices.iter().all(|index| {
            self.register_type(*index, provenance)
                .is_ok_and(|value_type| {
                    value_type.dimensions().is_empty()
                        && matches!(value_type.element_type(), SolveScalarType::Integer(_))
                })
        });
        if aggregate_type.dimensions().len() != indices.len()
            || *value_type != element_type
            || !index_types_are_valid
        {
            return Err(SolveProgramConstructionError::InvalidProjection { provenance });
        }
        let destination = self.issue_register(aggregate_type, provenance)?;
        self.push(
            SolveOperation::UpdateElement {
                destination: destination.id,
                aggregate: aggregate.id,
                value: value.id,
                indices: indices
                    .iter()
                    .map(|index| index.id)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn update_slice(
        &mut self,
        aggregate: ProgramRegister<'program>,
        value: ProgramRegister<'program>,
        origin: Vec<u32>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let aggregate_type = self.register_type(aggregate, provenance)?.clone();
        let value_type = self.register_type(value, provenance)?;
        if aggregate_type.element_type() != value_type.element_type()
            || !slice_in_bounds(
                aggregate_type.dimensions(),
                &origin,
                value_type.dimensions(),
            )
        {
            return Err(SolveProgramConstructionError::InvalidProjection { provenance });
        }
        let destination = self.issue_register(aggregate_type, provenance)?;
        self.push(
            SolveOperation::UpdateSlice {
                destination: destination.id,
                aggregate: aggregate.id,
                value: value.id,
                origin: origin.into_boxed_slice(),
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn call(
        &mut self,
        owner: SolvePureCallOwnerId,
        arguments: &[ProgramRegister<'program>],
        provenance: Span,
    ) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let interface = self
            .available_calls
            .get(owner.index() as usize)
            .filter(|interface| interface.id == owner)
            .ok_or(SolveProgramConstructionError::UnknownCallOwner { provenance })?;
        if interface.inputs.len() != arguments.len()
            || interface
                .inputs
                .iter()
                .zip(arguments)
                .any(|(expected, argument)| {
                    self.register_type(*argument, provenance) != Ok(expected)
                })
        {
            return Err(SolveProgramConstructionError::InvalidCallInterface { provenance });
        }
        let output_types = interface
            .outputs
            .iter()
            .map(|output| output.value_type().clone())
            .collect::<Vec<_>>();
        let final_count = self
            .register_types
            .len()
            .checked_add(output_types.len())
            .ok_or(SolveProgramConstructionError::IdentityOverflow { provenance })?;
        if final_count > u32::MAX as usize {
            return Err(SolveProgramConstructionError::IdentityOverflow { provenance });
        }
        let destinations = output_types
            .into_iter()
            .map(|value_type| self.issue_register(value_type, provenance))
            .collect::<Result<Vec<_>, _>>()?;
        self.push(
            SolveOperation::Call {
                owner,
                arguments: arguments
                    .iter()
                    .map(|argument| argument.id)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                destinations: destinations
                    .iter()
                    .map(|destination| destination.id)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            },
            provenance,
        );
        Ok(destinations)
    }

    fn aggregate_type(
        &self,
        elements: &[ProgramRegister<'program>],
        dimensions: Vec<u32>,
        provenance: Span,
    ) -> Result<SolveValueType, SolveProgramConstructionError> {
        let Some(first) = elements.first().copied() else {
            return Err(SolveProgramConstructionError::InvalidAggregate { provenance });
        };
        let first_type = self.register_type(first, provenance)?;
        let aggregate_type = SolveValueType::tensor(first_type.element_type(), dimensions)
            .map_err(|_| SolveProgramConstructionError::InvalidAggregate { provenance })?;
        let element_count = u32::try_from(elements.len())
            .map_err(|_| SolveProgramConstructionError::IdentityOverflow { provenance })?;
        let shape_is_valid = if first_type.dimensions().is_empty() {
            aggregate_type.scalar_count() == element_count
        } else {
            aggregate_type.dimensions().first() == Some(&element_count)
                && aggregate_type.dimensions()[1..] == *first_type.dimensions()
        };
        if !shape_is_valid
            || !elements.iter().all(|element| {
                self.register_type(*element, provenance)
                    .is_ok_and(|candidate| candidate == first_type)
            })
        {
            return Err(SolveProgramConstructionError::InvalidAggregate { provenance });
        }
        Ok(aggregate_type)
    }

    fn slot(
        &self,
        slot: ProgramSlot<'program>,
        provenance: Span,
    ) -> Result<&SolveSlot, SolveProgramConstructionError> {
        self.slots
            .get(slot.id.0 as usize)
            .filter(|candidate| candidate.id == slot.id)
            .ok_or(SolveProgramConstructionError::UnknownSlot { provenance })
    }

    fn register_type(
        &self,
        register: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<&SolveValueType, SolveProgramConstructionError> {
        self.register_types
            .get(register.id.0 as usize)
            .ok_or(SolveProgramConstructionError::UnknownRegister { provenance })
    }

    fn issue_register(
        &mut self,
        value_type: SolveValueType,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        let id = SolveRegisterId(checked_ordinal(self.register_types.len(), provenance)?);
        self.register_types.push(value_type);
        Ok(ProgramRegister {
            id,
            marker: PhantomData,
        })
    }

    fn push(&mut self, operation: SolveOperation, provenance: Span) {
        self.operations.push(SolveSpannedOperation {
            operation,
            provenance,
        });
    }
}

fn declare_interface_slots<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    value_types: &[SolveValueType],
    storage: SolveStorageClass,
    access: SolveSlotAccess,
    provenance: Span,
) -> Result<Vec<ProgramSlot<'program>>, SolveProgramConstructionError> {
    value_types
        .iter()
        .cloned()
        .map(|value_type| builder.declare_slot(value_type, storage, access, provenance))
        .collect()
}

fn require_fold_domain(
    domain: &StructuredIndexDomain,
    arithmetic: SolveArithmeticProfile,
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    domain
        .validate()
        .map_err(|_| SolveProgramConstructionError::InvalidFold { provenance })?;
    let integers = arithmetic.integer_domain();
    if domain.binders.is_empty()
        || domain
            .binders
            .iter()
            .any(|binder| !integers.contains(binder.lower) || !integers.contains(binder.upper))
    {
        return Err(SolveProgramConstructionError::InvalidFold { provenance });
    }
    Ok(())
}

fn require_map_domain(
    domain: &StructuredIndexDomain,
    arithmetic: SolveArithmeticProfile,
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    domain
        .validate()
        .map_err(|_| SolveProgramConstructionError::InvalidMap { provenance })?;
    let integers = arithmetic.integer_domain();
    if domain.binders.is_empty()
        || domain
            .binders
            .iter()
            .any(|binder| !integers.contains(binder.lower) || !integers.contains(binder.upper))
    {
        return Err(SolveProgramConstructionError::InvalidMap { provenance });
    }
    Ok(())
}

pub(super) fn construct_region(
    inputs: Vec<SolveValueType>,
    outputs: Vec<SolveValueType>,
    body: TypedProgram,
    provenance: Span,
) -> Result<SolveProgramRegion, SolveProgramConstructionError> {
    if provenance.is_dummy()
        || outputs.is_empty()
        || inputs
            .iter()
            .chain(&outputs)
            .any(|value_type| !value_type.belongs_to(body.arithmetic()))
    {
        return Err(SolveProgramConstructionError::InvalidRegion { provenance });
    }
    validate_region_body(&body, &inputs, &outputs, provenance)?;
    Ok(SolveProgramRegion {
        inputs: inputs.into_boxed_slice(),
        outputs: outputs.into_boxed_slice(),
        body,
        provenance,
    })
}

fn validate_region_body(
    body: &TypedProgram,
    inputs: &[SolveValueType],
    outputs: &[SolveValueType],
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    let interface_count = inputs
        .len()
        .checked_add(outputs.len())
        .ok_or(SolveProgramConstructionError::IdentityOverflow { provenance })?;
    if body.slots().len() < interface_count
        || !slots_match(
            &body.slots()[..inputs.len()],
            inputs,
            SolveStorageClass::Input,
        )
        || !slots_match(
            &body.slots()[inputs.len()..interface_count],
            outputs,
            SolveStorageClass::Output,
        )
        || body.slots()[interface_count..]
            .iter()
            .any(|slot| slot.storage() != SolveStorageClass::MethodLocal)
    {
        return Err(SolveProgramConstructionError::InvalidRegion { provenance });
    }
    validate_region_outputs(body, inputs.len(), outputs.len(), provenance)
}

fn slots_match(
    slots: &[SolveSlot],
    expected: &[SolveValueType],
    storage: SolveStorageClass,
) -> bool {
    slots
        .iter()
        .zip(expected)
        .all(|(slot, expected)| slot.storage() == storage && slot.value_type() == expected)
}

fn validate_region_outputs(
    body: &TypedProgram,
    output_start: usize,
    output_count: usize,
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    let mut stores = vec![0usize; output_count];
    for operation in body.operations() {
        match operation.operation() {
            SolveOperation::Load { slot, .. }
                if (output_start..output_start + output_count).contains(&slot.index()) =>
            {
                return Err(SolveProgramConstructionError::InvalidRegion { provenance });
            }
            SolveOperation::Store { slot, .. } => {
                if let Some(output) = slot
                    .index()
                    .checked_sub(output_start)
                    .filter(|output| *output < output_count)
                {
                    stores[output] += 1;
                }
            }
            _ => {}
        }
    }
    if stores.iter().any(|count| *count != 1) {
        return Err(SolveProgramConstructionError::InvalidRegion { provenance });
    }
    Ok(())
}

fn checked_ordinal(value: usize, provenance: Span) -> Result<u32, SolveProgramConstructionError> {
    u32::try_from(value).map_err(|_| SolveProgramConstructionError::IdentityOverflow { provenance })
}

fn require_provenance(provenance: Span) -> Result<(), SolveProgramConstructionError> {
    if provenance.is_dummy() {
        return Err(SolveProgramConstructionError::MissingProvenance);
    }
    Ok(())
}

fn binary_operator_accepts(operator: SolveBinaryOperator, scalar: SolveScalarType) -> bool {
    match operator {
        SolveBinaryOperator::Add
        | SolveBinaryOperator::Subtract
        | SolveBinaryOperator::Multiply
        | SolveBinaryOperator::Min
        | SolveBinaryOperator::Max => scalar.is_numeric(),
        SolveBinaryOperator::Divide | SolveBinaryOperator::Power | SolveBinaryOperator::Atan2 => {
            matches!(scalar, SolveScalarType::Real { .. })
        }
        SolveBinaryOperator::And | SolveBinaryOperator::Or => scalar == SolveScalarType::Boolean,
    }
}

fn indices_in_bounds(dimensions: &[u32], indices: &[u32]) -> bool {
    dimensions.len() == indices.len()
        && dimensions
            .iter()
            .zip(indices)
            .all(|(extent, index)| *index < *extent)
}

fn slice_in_bounds(base: &[u32], origin: &[u32], dimensions: &[u32]) -> bool {
    base.len() == origin.len()
        && base.len() == dimensions.len()
        && base
            .iter()
            .zip(origin)
            .zip(dimensions)
            .all(|((base, origin), extent)| {
                origin.checked_add(*extent).is_some_and(|end| end <= *base)
            })
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SolveProgramConstructionError {
    MissingProvenance,
    WireMismatch,
    IdentityOverflow { provenance: Span },
    ProfileMismatch { provenance: Span },
    WritableConstant { provenance: Span },
    UnknownSlot { provenance: Span },
    UnknownRegister { provenance: Span },
    ReadOnlyStore { provenance: Span },
    TypeMismatch { provenance: Span },
    InvalidAggregate { provenance: Span },
    InvalidProjection { provenance: Span },
    UnknownCallOwner { provenance: Span },
    InvalidCallInterface { provenance: Span },
    EmptyCallOutput { provenance: Span },
    InvalidCallOutput { provenance: Span },
    InvalidRegion { provenance: Span },
    InvalidMap { provenance: Span },
    InvalidFold { provenance: Span },
    InvalidTensorAlgebra { provenance: Span },
    IncompleteCallOutput { provenance: Span },
    DuplicateCallIdentity { provenance: Span },
    UninitializedSlot { provenance: Span },
}

impl SolveProgramConstructionError {
    /// The owner span this rejection was raised at, when the rejected
    /// construct had one.
    ///
    /// SPEC_0008: the two provenance-free rejections are exactly the ones that
    /// fire *because* no owner span was recoverable, so they report `None`
    /// rather than manufacturing the dummy sentinel. Callers that render a
    /// diagnostic must handle the unspanned case explicitly.
    #[must_use]
    pub const fn provenance(&self) -> Option<Span> {
        match self {
            Self::MissingProvenance | Self::WireMismatch => None,
            Self::IdentityOverflow { provenance }
            | Self::ProfileMismatch { provenance }
            | Self::WritableConstant { provenance }
            | Self::UnknownSlot { provenance }
            | Self::UnknownRegister { provenance }
            | Self::ReadOnlyStore { provenance }
            | Self::TypeMismatch { provenance }
            | Self::InvalidAggregate { provenance }
            | Self::InvalidProjection { provenance }
            | Self::UnknownCallOwner { provenance }
            | Self::InvalidCallInterface { provenance }
            | Self::EmptyCallOutput { provenance }
            | Self::InvalidCallOutput { provenance }
            | Self::InvalidRegion { provenance }
            | Self::InvalidMap { provenance }
            | Self::InvalidFold { provenance }
            | Self::InvalidTensorAlgebra { provenance }
            | Self::IncompleteCallOutput { provenance }
            | Self::DuplicateCallIdentity { provenance }
            | Self::UninitializedSlot { provenance } => Some(*provenance),
        }
    }
}

impl std::fmt::Display for SolveProgramConstructionError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
            Self::MissingProvenance => "typed program owner is missing exact provenance",
            Self::WireMismatch => "typed program wire does not replay through checked construction",
            Self::IdentityOverflow { .. } => "typed program identity capacity exceeded",
            Self::ProfileMismatch { .. } => "value type does not belong to the program profile",
            Self::WritableConstant { .. } => "constant storage cannot be writable",
            Self::UnknownSlot { .. } => "slot is not owned by this program",
            Self::UnknownRegister { .. } => "register is not owned by this program",
            Self::ReadOnlyStore { .. } => "store targets read-only storage",
            Self::TypeMismatch { .. } => "typed program operand or result type mismatch",
            Self::InvalidAggregate { .. } => "typed aggregate shape or element contract is invalid",
            Self::InvalidProjection { .. } => "typed aggregate projection is invalid",
            Self::UnknownCallOwner { .. } => "pure-call owner was not issued by this table",
            Self::InvalidCallInterface { .. } => "pure-call argument or slot interface is invalid",
            Self::EmptyCallOutput { .. } => "pure-call owner has no value or assertion output",
            Self::InvalidCallOutput { .. } => "pure-call output kind does not match its type",
            Self::InvalidRegion { .. } => "typed structured region interface is invalid",
            Self::InvalidMap { .. } => "typed compact map interface or domain is invalid",
            Self::InvalidFold { .. } => "typed compact fold interface or domain is invalid",
            Self::InvalidTensorAlgebra { .. } => {
                "typed tensor-algebra rank, shape, or element contract is invalid"
            }
            Self::IncompleteCallOutput { .. } => {
                "pure-call body does not define every output exactly once"
            }
            Self::DuplicateCallIdentity { .. } => "pure-call semantic identity was already issued",
            Self::UninitializedSlot { .. } => {
                "slot load is not dominated by an external or stored definition"
            }
        };
        formatter.write_str(message)
    }
}

impl std::error::Error for SolveProgramConstructionError {}

#[cfg(test)]
mod tests;
