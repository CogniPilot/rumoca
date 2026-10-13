mod calls;
pub use calls::{ProgramCall, SolveAssertionForwarding};
mod construction_checks;
mod construction_error;
use construction_checks::{
    binary_operator_accepts, checked_ordinal, construct_region, declare_interface_slots,
    indices_in_bounds, require_fold_domain, require_map_domain, require_provenance,
    slice_in_bounds,
};
pub use construction_error::SolveProgramConstructionError;

mod assigned_state;
use assigned_state::{AssignedStateConstruction, AssignedStateLayout};
pub use assigned_state::{
    ProgramAssignedState, SolveAssignedConditional, SolveAssignedStateId,
    SolveAssignedStateOperation,
};
mod assertions;
mod directional;
pub use assertions::{ProgramAssertion, SolveAssertionMessage};
mod fault_flow;
mod linear_solve;
mod loop_flow;
mod native;
mod operation_visit;
mod pass_flow;
mod read_flow;
mod tensor;
mod value_numbering;
use value_numbering::ValueKey;

pub use loop_flow::{MAX_UNROLLED_OPERATIONS, MAX_UNROLLED_PASSES, invariant_guard};
pub use pass_flow::index_forms;
pub use tensor::promoted_concatenate_dimensions;
pub(in crate::typed_program) mod wire;

use std::marker::PhantomData;

use rumoca_core::native_body::NativeBody;
use rumoca_core::{Span, StructuredIndexDomain};
use serde::{Deserialize, Serialize};

use super::call::{SolvePureCallOutput, SolvePureCallOwnerId, SolvePureCallTableView};
use super::demand::{SolveLiveness, SolveOutputDemand};
use super::pass_independence::{IndependentPasses, LaneExactness, SolveIndexForm};
use super::types::{SolveArithmeticProfile, SolveScalarType, SolveValue, SolveValueType};
use crate::fault::FaultClass;

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
    /// The exact Integer quotient truncated toward zero (MLS §3.7.2 `div`).
    /// A zero divisor or an unrepresentable quotient is an evaluation
    /// failure, never a wrapped or defaulted value. `Divide` stays Real-only,
    /// so an Integer `/` cannot bypass its explicit conversion to Real.
    IntegerQuotient,
    /// MLS `mod`: exact Integer remainder with the divisor's sign.
    IntegerModulo,
    /// MLS `rem`: exact Integer remainder with the dividend's sign.
    IntegerRemainder,
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
    AssignedConditional {
        program: Box<SolveAssignedConditional>,
    },
    AssignedState {
        #[serde(rename = "state_operation")]
        operation: SolveAssignedStateOperation,
    },
    /// One source-ordered assertion with failed-only message captures.
    CheckAssertion {
        predicate_output: usize,
        message_outputs: Box<[usize]>,
        condition: SolveRegisterId,
        captures: Box<[SolveRegisterId]>,
        destinations: Box<[SolveRegisterId]>,
        message: SolveAssertionMessage,
    },
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
        state_captures: Box<[SolveAssignedStateId]>,
        destinations: Box<[SolveRegisterId]>,
        transition: Box<SolveProgramRegion>,
        /// An early-exit predicate over the carried tuple, the captures and
        /// the binder values of the pass about to run, evaluated before each
        /// pass; the first false value ends the loop with the current tuple
        /// (MLS §11.2.3). The producer proves that every skipped pass would
        /// leave the tuple unchanged, so the domain stays the static capacity
        /// while a run-time bound such as `i <= n` decides how many passes
        /// execute.
        continuation: Option<Box<SolveProgramRegion>>,
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
        assertion_forwarding: Box<[SolveAssertionForwarding]>,
        owner: SolvePureCallOwnerId,
        arguments: Box<[SolveRegisterId]>,
        destinations: Box<[SolveRegisterId]>,
    },
    /// A square Binary64 system over whole matrix/vector registers.
    LinearSolve {
        destination: SolveRegisterId,
        matrix: SolveRegisterId,
        rhs: SolveRegisterId,
    },
    /// One SPEC_0040 DAE-C30 compiler-defined foreign body, typed by its
    /// catalog interface: operands are its inputs and destinations its
    /// outputs, each in interface order.
    Native {
        body: NativeBody,
        operands: Box<[SolveRegisterId]>,
        destinations: Box<[SolveRegisterId]>,
    },
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
    assigned_states: AssignedStateLayout,
    #[serde(skip)]
    read_flow: read_flow::ReadFlow,
    #[serde(skip)]
    fault_flow: fault_flow::FaultFlow,
    #[serde(skip)]
    loop_flow: loop_flow::LoopFlow,
    #[serde(skip)]
    pass_flow: pass_flow::PassFlow,
    #[serde(skip)]
    call_demands: Box<[Option<SolveOutputDemand>]>,
}

impl TypedProgram {
    pub fn construct(
        arithmetic: SolveArithmeticProfile,
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Self, SolveProgramConstructionError> {
        Self::construct_with_calls(arithmetic, SolvePureCallTableView::default(), build)
    }

    pub(super) fn construct_with_calls(
        arithmetic: SolveArithmeticProfile,
        available_calls: SolvePureCallTableView<'_>,
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Self, SolveProgramConstructionError> {
        Self::construct_with_owner(arithmetic, available_calls, &[], build)
    }

    pub(super) fn construct_with_owner(
        arithmetic: SolveArithmeticProfile,
        available_calls: SolvePureCallTableView<'_>,
        assertion_outputs: &[SolvePureCallOutput],
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Self, SolveProgramConstructionError> {
        Self::construct_with_assigned_interface(
            arithmetic,
            available_calls,
            assertion_outputs,
            Vec::new(),
            Vec::new(),
            None,
            build,
        )
    }

    fn construct_with_assigned_interface(
        arithmetic: SolveArithmeticProfile,
        available_calls: SolvePureCallTableView<'_>,
        assertion_outputs: &[SolvePureCallOutput],
        state_inputs: Vec<SolveValueType>,
        state_outputs: Vec<SolveValueType>,
        region_provenance: Option<Span>,
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Self, SolveProgramConstructionError> {
        if state_inputs
            .iter()
            .chain(&state_outputs)
            .any(|value_type| !value_type.belongs_to(arithmetic))
        {
            return Err(SolveProgramConstructionError::WireMismatch);
        }
        let mut builder = TypedProgramBuilder {
            arithmetic,
            slots: Vec::new(),
            slot_initialized: Vec::new(),
            register_types: Vec::new(),
            operations: Vec::new(),
            numbering: std::collections::HashMap::new(),
            numbered_values: true,
            assigned_states: AssignedStateConstruction::interface(state_inputs, state_outputs),
            available_calls,
            assertion_outputs,
            marker: PhantomData,
        };
        build(&mut builder)?;
        builder.assigned_states.complete(
            builder
                .operations
                .last()
                .map(|op| op.provenance)
                .or(region_provenance),
        )?;
        Ok(Self::assemble(
            arithmetic,
            builder.slots,
            builder.register_types,
            builder.operations,
            builder.assigned_states.layout,
        ))
    }

    fn assemble(
        arithmetic: SolveArithmeticProfile,
        slots: Vec<SolveSlot>,
        register_types: Vec<SolveValueType>,
        operations: Vec<SolveSpannedOperation>,
        assigned_states: AssignedStateLayout,
    ) -> Self {
        let read_flow = read_flow::ReadFlow::of(&operations, register_types.len(), &slots);
        let fault_flow = fault_flow::FaultFlow::of(&operations, &register_types, &slots, &[]);
        let loop_flow = loop_flow::LoopFlow::of(&operations);
        let pass_flow = pass_flow::PassFlow::of(&operations, &fault_flow);
        let mut program = Self {
            arithmetic,
            slots: slots.into_boxed_slice(),
            register_types: register_types.into_boxed_slice(),
            operations: operations.into_boxed_slice(),
            assigned_states,
            read_flow,
            fault_flow,
            loop_flow,
            pass_flow,
            call_demands: Box::default(),
        };
        program.call_demands = SolveLiveness::call_site_demands(&program);
        program
    }

    /// The operations one pass of this program emits, counting each unrolled
    /// loop once per pass.
    #[must_use]
    pub fn emitted_weight(&self) -> usize {
        self.loop_flow.weight()
    }

    /// Whether a pass of this program can record an observation: an assertion
    /// check or a call anywhere in it.
    #[must_use]
    pub fn observes(&self) -> bool {
        self.loop_flow.observes()
    }

    /// The number of straight-line passes operation `operation` may be emitted
    /// as, when it is a loop with a proven constant trip count and a body
    /// within the cost bound, else `None`. A loop whose passes must not record
    /// an observation (`observed`) and whose body does has no unrolling.
    #[must_use]
    pub fn unrolled_passes(&self, operation: usize, observed: bool) -> Option<usize> {
        let passes = self.loop_flow.unrolled(operation)?;
        let body_observes = match self.operations.get(operation)?.operation() {
            SolveOperation::Fold { transition, .. } => transition.body().observes(),
            SolveOperation::Map { body, .. } => body.body().observes(),
            _ => return None,
        };
        (!(observed && body_observes)).then_some(passes)
    }

    /// The independent-passes property of loop operation `operation`, proven
    /// once when this program was constructed (SPEC_0040 SOLVE-C104): no pass
    /// reads a value another pass writes or carries a value to the next, and
    /// the exactness of the operations of one pass says whether passes may be
    /// evaluated in lanes. `None` for an operation that is not such a loop.
    #[must_use]
    pub fn independent_passes(&self, operation: usize) -> Option<IndependentPasses> {
        self.pass_flow.passes(operation)
    }

    /// The outputs of the callee of call operation `operation` that this
    /// program reads, in output order; `None` for an operation that is not a
    /// call. This is the demand a plan built for the call site serves.
    #[must_use]
    pub fn call_site_demand(&self, operation: usize) -> Option<&SolveOutputDemand> {
        self.call_demands.get(operation)?.as_ref()
    }

    /// How operation `operation` can fail, proven once when this program was
    /// constructed. Every pass that drops, reorders or speculates an operation
    /// reads this one classification (SPEC_0040 SOLVE-C102).
    #[must_use]
    pub fn fault_class(&self, operation: usize) -> FaultClass {
        self.fault_flow
            .classes()
            .get(operation)
            .copied()
            .unwrap_or(FaultClass::HasEffect)
    }

    /// Whether operation `operation` is pure and cannot fault, so it may be
    /// dropped when nothing reads its results.
    #[must_use]
    pub fn is_prunable(&self, operation: usize) -> bool {
        self.fault_class(operation).is_prunable()
    }

    /// The class of this program entered as a region: the join of its
    /// operations, where a store to its own Output slot is not an effect.
    #[must_use]
    pub fn region_fault_class(&self) -> FaultClass {
        self.fault_flow.region_class(&self.operations, &self.slots)
    }

    /// The last operation of this program that reads each register, by register
    /// index; `None` for a register nothing reads. Every executor derives its
    /// value lifetimes from this relation.
    #[must_use]
    pub fn register_last_reads(&self) -> &[Option<usize>] {
        self.read_flow.register_last_reads()
    }

    /// Whether `operation` is the last operation of this program that reads
    /// `register` and lists it once. The register's value is dead afterwards,
    /// so an executor may move it out instead of sharing it.
    #[must_use]
    pub fn register_moves_at(&self, register: SolveRegisterId, operation: usize) -> bool {
        self.read_flow
            .register_moves_at(register.index(), operation)
    }

    /// Whether `operation` is the last load of the read-only `slot`, whose
    /// value is dead afterwards.
    #[must_use]
    pub fn slot_last_load_at(&self, slot: SolveSlotId, operation: usize) -> bool {
        self.read_flow.slot_last_load_at(slot.index(), operation)
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

/// Registers belong to one construction scope, even when scopes share call owners.
///
/// ```compile_fail,E0521
/// use rumoca_ir_solve::{SolveArithmeticProfile, SolveUnaryOperator, SolveValue, TypedProgram};
/// fn cross_scope(profile: SolveArithmeticProfile, span: rumoca_core::Span) {
///     let _ = TypedProgram::construct(profile, |outer| {
///         let value = outer.constant(SolveValue::boolean(true), span)?;
///         TypedProgram::construct(profile, |inner| {
///             inner.unary(SolveUnaryOperator::Not, value, span)?;
///             Ok(())
///         })?;
///         Ok(())
///     });
/// }
/// # let _ = cross_scope;
/// ```
pub struct TypedProgramBuilder<'program> {
    arithmetic: SolveArithmeticProfile,
    slots: Vec<SolveSlot>,
    slot_initialized: Vec<bool>,
    register_types: Vec<SolveValueType>,
    operations: Vec<SolveSpannedOperation>,
    numbering: std::collections::HashMap<ValueKey, SolveRegisterId>,
    /// Whether repeated scalar computations share a register; a replayed wire
    /// keeps every recorded register.
    numbered_values: bool,
    assigned_states: AssignedStateConstruction,
    available_calls: SolvePureCallTableView<'program>,
    assertion_outputs: &'program [SolvePureCallOutput],
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
        let key = value
            .value_type()
            .dimensions()
            .is_empty()
            .then(|| ValueKey::Constant(value.clone()));
        if let Some(existing) = self.numbered(key.as_ref()) {
            return Ok(existing);
        }
        let destination = self.issue_register(value.value_type().clone(), provenance)?;
        self.push(
            SolveOperation::Constant {
                destination: destination.id,
                value,
            },
            provenance,
        );
        self.number(key, destination);
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
        let key = value_type
            .dimensions()
            .is_empty()
            .then_some(ValueKey::Unary(operator, operand.id));
        if let Some(existing) = self.numbered(key.as_ref()) {
            return Ok(existing);
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
        self.number(key, destination);
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
        let key = lhs_type
            .dimensions()
            .is_empty()
            .then_some(ValueKey::Binary(operator, lhs.id, rhs.id));
        if let Some(existing) = self.numbered(key.as_ref()) {
            return Ok(existing);
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
        self.number(key, destination);
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
        let key = lhs_type
            .dimensions()
            .is_empty()
            .then_some(ValueKey::Compare(operator, lhs.id, rhs.id));
        if let Some(existing) = self.numbered(key.as_ref()) {
            return Ok(existing);
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
        self.number(key, destination);
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
        let key = value_type
            .dimensions()
            .is_empty()
            .then_some(ValueKey::Convert(operator, operand.id));
        if let Some(existing) = self.numbered(key.as_ref()) {
            return Ok(existing);
        }
        let destination = self.issue_register(value_type, provenance)?;
        self.push(
            SolveOperation::Convert {
                destination: destination.id,
                operator,
                operand: operand.id,
            },
            provenance,
        );
        self.number(key, destination);
        Ok(destination)
    }

    /// Preserve an already matching value or emit the one implicit numeric
    /// widening admitted by checked DAE construction.
    pub fn coerce_to(
        &mut self,
        value: ProgramRegister<'program>,
        target: &SolveValueType,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let source = self.register_type(value, provenance)?;
        if source == target {
            return Ok(value);
        }
        let integer_to_real = source.dimensions() == target.dimensions()
            && matches!(source.element_type(), SolveScalarType::Integer(_))
            && matches!(target.element_type(), SolveScalarType::Real { .. });
        if !integer_to_real {
            return Err(SolveProgramConstructionError::TypeMismatch { provenance });
        }
        self.convert(SolveConversionOperator::IntegerToReal, value, provenance)
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
        self.fold_with_assigned(
            domain,
            initial,
            captures,
            &[],
            provenance,
            |builder, _, carried, captures, binders, outputs| {
                build(builder, carried, captures, binders, outputs)
            },
        )
    }

    pub fn fold_while(
        &mut self,
        domain: StructuredIndexDomain,
        initial: &[ProgramRegister<'program>],
        captures: &[ProgramRegister<'program>],
        provenance: Span,
        continuation: impl for<'region> FnOnce(
            &mut TypedProgramBuilder<'region>,
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
        ) -> Result<(), SolveProgramConstructionError>,
        build: impl for<'region> FnOnce(
            &mut TypedProgramBuilder<'region>,
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
        self.fold_while_assigned(
            domain,
            initial,
            (captures, &[]),
            provenance,
            |builder, _, carried, captures, binders, outputs| {
                continuation(builder, carried, captures, binders, outputs)
            },
            |builder, _, carried, captures, binders, outputs| {
                build(builder, carried, captures, binders, outputs)
            },
        )
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
                    return Err(SolveProgramConstructionError::InvalidMap { provenance });
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
        let body = TypedProgram::construct_with_owner(
            self.arithmetic,
            self.available_calls,
            self.assertion_outputs,
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

    fn check_conditional_regions(
        &self,
        condition: ProgramRegister<'program>,
        captures: &[ProgramRegister<'program>],
        if_true: &SolveProgramRegion,
        if_false: &SolveProgramRegion,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
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
        Ok(())
    }

    fn conditional_from_regions(
        &mut self,
        condition: ProgramRegister<'program>,
        captures: &[ProgramRegister<'program>],
        if_true: SolveProgramRegion,
        if_false: SolveProgramRegion,
        provenance: Span,
    ) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
        self.check_conditional_regions(condition, captures, &if_true, &if_false, provenance)?;
        if !if_true.body.assigned_state_inputs().is_empty()
            || !if_true.body.assigned_state_outputs().is_empty()
            || !if_false.body.assigned_state_inputs().is_empty()
            || !if_false.body.assigned_state_outputs().is_empty()
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
        (captures, state_captures): (
            &[ProgramRegister<'program>],
            &[ProgramAssignedState<'program>],
        ),
        transition: SolveProgramRegion,
        continuation: Option<SolveProgramRegion>,
        provenance: Span,
    ) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        require_fold_domain(&domain, self.arithmetic, provenance)?;
        let state_types = state_captures
            .iter()
            .map(|state| self.assigned_state_type(*state, provenance).cloned())
            .collect::<Result<Vec<_>, _>>()?;
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
            || transition.body.assigned_state_inputs() != state_types
            || !transition.body.assigned_state_outputs().is_empty()
            || transition.inputs.as_ref() != expected_inputs
            || transition.outputs.as_ref() != carried_types
            || transition.body.arithmetic() != self.arithmetic
            || transition.provenance != provenance
            || continuation.as_ref().is_some_and(|predicate| {
                predicate.body.assigned_state_inputs() != state_types
                    || !predicate.body.assigned_state_outputs().is_empty()
                    || predicate.inputs.as_ref() != expected_inputs
                    || predicate.outputs.as_ref()
                        != [SolveValueType::scalar(SolveScalarType::Boolean)]
                    || predicate.body.arithmetic() != self.arithmetic
                    || predicate.provenance != provenance
            })
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
                state_captures: state_captures
                    .iter()
                    .map(|state| state.id)
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
                continuation: continuation.map(Box::new),
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
        if !body.body.assigned_state_inputs().is_empty()
            || !body.body.assigned_state_outputs().is_empty()
            || body.inputs.as_ref() != expected_inputs
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
        let key = Some(ValueKey::Project(
            aggregate.id,
            indices.clone().into_boxed_slice(),
        ));
        if let Some(existing) = self.numbered(key.as_ref()) {
            return Ok(existing);
        }
        let destination = self.issue_register(value_type, provenance)?;
        self.push(
            SolveOperation::ProjectElement {
                destination: destination.id,
                aggregate: aggregate.id,
                indices: indices.into_boxed_slice(),
            },
            provenance,
        );
        self.number(key, destination);
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
        let key = Some(ValueKey::ProjectDynamic(
            aggregate.id,
            indices.iter().map(|index| index.id).collect(),
        ));
        if let Some(existing) = self.numbered(key.as_ref()) {
            return Ok(existing);
        }
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
        self.number(key, destination);
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

    /// Legacy value-only facade; all call emission delegates to `emit_call`.
    pub fn call(
        &mut self,
        owner: SolvePureCallOwnerId,
        arguments: &[ProgramRegister<'program>],
        provenance: Span,
    ) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
        self.emit_call(owner, arguments, provenance)
            .map(ProgramCall::into_registers)
    }

    pub fn emit_call(
        &mut self,
        owner: SolvePureCallOwnerId,
        arguments: &[ProgramRegister<'program>],
        provenance: Span,
    ) -> Result<ProgramCall<'program>, SolveProgramConstructionError> {
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
        let operation = self.operations.len();
        self.push(
            SolveOperation::Call {
                assertion_forwarding: Box::new([]),
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
        Ok(ProgramCall {
            operation,
            owner,
            registers: destinations,
            marker: PhantomData,
        })
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

    /// The checked type of a register this builder issued.
    pub fn value_type_of(
        &self,
        register: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<&SolveValueType, SolveProgramConstructionError> {
        self.register_type(register, provenance)
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

    /// The register of an earlier identical single-cell computation.
    fn numbered(&self, key: Option<&ValueKey>) -> Option<ProgramRegister<'program>> {
        key.filter(|_| self.numbered_values)
            .and_then(|key| self.numbering.get(key))
            .map(|&id| ProgramRegister {
                id,
                marker: PhantomData,
            })
    }

    fn number(&mut self, key: Option<ValueKey>, register: ProgramRegister<'program>) {
        if let Some(key) = key {
            self.numbering.insert(key, register.id);
        }
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

#[cfg(test)]
mod tests;
