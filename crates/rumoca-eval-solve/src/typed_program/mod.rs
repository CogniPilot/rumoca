mod number;
mod tensor;

use rumoca_core::Span;
use rumoca_core::StructuredIndexDomain;
use rumoca_ir_solve::{
    SolveBinaryOperator, SolveOperation, SolveProgramRegion, SolvePureCallOwner,
    SolvePureCallOwnerId, SolvePureCallTable, SolveRealFormat, SolveReductionOperator,
    SolveRegisterId, SolveScalarType, SolveSlotId, SolveTensorViewAxis, SolveValue, SolveValueKind,
    SolveValueType, TypedProgram,
};

use number::{
    eval_binary_element, eval_binary_typed, eval_compare_typed, eval_convert_typed,
    eval_unary_typed,
};

/// One runtime value for one typed register or slot.
///
/// A tensor remains one value with one compact shape. Its scalar payload is
/// materialized only at this evaluator boundary; no operation graph is
/// expanded or reconstructed.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypedValue {
    value_type: SolveValueType,
    elements: Box<[SolveValueKind]>,
}

impl TypedValue {
    pub fn construct(
        value_type: SolveValueType,
        elements: Vec<SolveValueKind>,
    ) -> Result<Self, TypedValueConstructionError> {
        if elements.len() != value_type.scalar_count() as usize {
            return Err(TypedValueConstructionError::ScalarCountMismatch);
        }
        if elements
            .iter()
            .copied()
            .any(|element| !element_matches_type(element, value_type.element_type()))
        {
            return Err(TypedValueConstructionError::ElementTypeMismatch);
        }
        Ok(Self {
            value_type,
            elements: elements.into_boxed_slice(),
        })
    }

    #[must_use]
    pub const fn value_type(&self) -> &SolveValueType {
        &self.value_type
    }

    #[must_use]
    pub const fn elements(&self) -> &[SolveValueKind] {
        &self.elements
    }

    fn scalar(value: &SolveValue) -> Self {
        Self {
            value_type: value.value_type().clone(),
            elements: Box::new([value.kind()]),
        }
    }

    fn checked(
        value_type: SolveValueType,
        elements: Vec<SolveValueKind>,
        provenance: Span,
    ) -> Result<Self, TypedProgramEvalError> {
        Self::construct(value_type, elements).map_err(|_| {
            TypedProgramEvalError::InvalidCheckedProgram {
                operation: "construct runtime value",
                provenance,
            }
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypedValueConstructionError {
    ScalarCountMismatch,
    ElementTypeMismatch,
}

impl std::fmt::Display for TypedValueConstructionError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            Self::ScalarCountMismatch => "runtime value scalar count does not match its type",
            Self::ElementTypeMismatch => "runtime value element does not match its type",
        })
    }
}

impl std::error::Error for TypedValueConstructionError {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypedProgramEvalError {
    UnknownOwner {
        owner: SolvePureCallOwnerId,
    },
    InvalidArgument {
        index: usize,
        provenance: Span,
    },
    InvalidCheckedProgram {
        operation: &'static str,
        provenance: Span,
    },
    IntegerArithmetic {
        operation: &'static str,
        provenance: Span,
    },
    InvalidIntegerConversion {
        provenance: Span,
    },
}

impl TypedProgramEvalError {
    #[must_use]
    pub const fn source_span(&self) -> Option<Span> {
        match self {
            Self::UnknownOwner { .. } => None,
            Self::InvalidArgument { provenance, .. }
            | Self::InvalidCheckedProgram { provenance, .. }
            | Self::IntegerArithmetic { provenance, .. }
            | Self::InvalidIntegerConversion { provenance } => Some(*provenance),
        }
    }
}

impl std::fmt::Display for TypedProgramEvalError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownOwner { owner } => {
                write!(formatter, "unknown pure-call owner {}", owner.index())
            }
            Self::InvalidArgument { index, .. } => {
                write!(
                    formatter,
                    "pure-call argument {index} does not match its interface"
                )
            }
            Self::InvalidCheckedProgram { operation, .. } => {
                write!(formatter, "checked typed program failed during {operation}")
            }
            Self::IntegerArithmetic { operation, .. } => {
                write!(
                    formatter,
                    "Integer {operation} left the checked target domain"
                )
            }
            Self::InvalidIntegerConversion { .. } => {
                formatter.write_str("Real-to-Integer conversion left the checked target domain")
            }
        }
    }
}

impl std::error::Error for TypedProgramEvalError {}

/// Evaluate one compiler-issued pure-call owner with compact typed arguments.
pub fn eval_pure_call(
    table: &SolvePureCallTable,
    owner: SolvePureCallOwnerId,
    arguments: &[TypedValue],
) -> Result<Vec<TypedValue>, TypedProgramEvalError> {
    let owner = table
        .owner(owner)
        .ok_or(TypedProgramEvalError::UnknownOwner { owner })?;
    eval_owner(table, owner, arguments)
}

/// Evaluate the construction-issued compact directional relation of one
/// typed owner. Arguments and results use the directional site's aggregate
/// primal/tangent ABI; tensor coordinates are materialized only as payload.
pub fn eval_pure_call_directional(
    table: &SolvePureCallTable,
    owner: SolvePureCallOwnerId,
    arguments: &[TypedValue],
) -> Result<Vec<TypedValue>, TypedProgramEvalError> {
    let owner = table
        .owner(owner)
        .ok_or(TypedProgramEvalError::UnknownOwner { owner })?;
    eval_directional_owner(table, owner, arguments)
}

#[derive(Clone, Copy)]
enum EvaluationMode {
    Primal,
    Directional,
}

fn eval_owner(
    table: &SolvePureCallTable,
    owner: &SolvePureCallOwner,
    arguments: &[TypedValue],
) -> Result<Vec<TypedValue>, TypedProgramEvalError> {
    let mut invocations = InvocationScope::new(table);
    eval_owner_in_scope(
        table,
        owner,
        arguments,
        &mut invocations,
        EvaluationMode::Primal,
    )
}

fn eval_directional_owner(
    table: &SolvePureCallTable,
    owner: &SolvePureCallOwner,
    arguments: &[TypedValue],
) -> Result<Vec<TypedValue>, TypedProgramEvalError> {
    let mut invocations = InvocationScope::new(table);
    eval_owner_in_scope(
        table,
        owner,
        arguments,
        &mut invocations,
        EvaluationMode::Directional,
    )
}

fn eval_owner_in_scope(
    table: &SolvePureCallTable,
    owner: &SolvePureCallOwner,
    arguments: &[TypedValue],
    invocations: &mut InvocationScope,
    mode: EvaluationMode,
) -> Result<Vec<TypedValue>, TypedProgramEvalError> {
    let (inputs, outputs, body) = match mode {
        EvaluationMode::Primal => (owner.inputs(), owner.outputs(), owner.body()),
        EvaluationMode::Directional => {
            let directional =
                owner
                    .directional()
                    .ok_or(TypedProgramEvalError::InvalidCheckedProgram {
                        operation: "evaluate unavailable directional owner",
                        provenance: owner.provenance(),
                    })?;
            (
                directional.inputs(),
                directional.outputs(),
                directional.body(),
            )
        }
    };
    validate_arguments(inputs, arguments, owner.provenance())?;
    let mut frame = EvalFrame::new(table, body, invocations, mode);
    for (slot, value) in frame.slots.iter_mut().zip(arguments) {
        *slot = Some(value.clone());
    }
    frame.run()?;
    let output_start = arguments.len();
    outputs
        .iter()
        .enumerate()
        .map(|(output, _)| {
            frame
                .slots
                .get(output_start + output)
                .and_then(Clone::clone)
                .ok_or(TypedProgramEvalError::InvalidCheckedProgram {
                    operation: "read pure-call output",
                    provenance: owner.provenance(),
                })
        })
        .collect()
}

#[cfg(test)]
fn eval_pure_call_with_invocation_counts(
    table: &SolvePureCallTable,
    owner: SolvePureCallOwnerId,
    arguments: &[TypedValue],
) -> Result<(Vec<TypedValue>, Vec<u32>), TypedProgramEvalError> {
    let owner = table
        .owner(owner)
        .ok_or(TypedProgramEvalError::UnknownOwner { owner })?;
    let mut invocations = InvocationScope::new(table);
    let outputs = eval_owner_in_scope(
        table,
        owner,
        arguments,
        &mut invocations,
        EvaluationMode::Primal,
    )?;
    Ok((outputs, invocations.misses))
}

fn validate_arguments(
    inputs: &[SolveValueType],
    arguments: &[TypedValue],
    provenance: Span,
) -> Result<(), TypedProgramEvalError> {
    let invalid = if inputs.len() != arguments.len() {
        Some(arguments.len().min(inputs.len()))
    } else {
        inputs
            .iter()
            .zip(arguments)
            .position(|(expected, actual)| expected != actual.value_type())
    };
    match invalid {
        Some(index) => Err(TypedProgramEvalError::InvalidArgument { index, provenance }),
        None => Ok(()),
    }
}

fn eval_region(
    table: &SolvePureCallTable,
    region: &SolveProgramRegion,
    arguments: &[TypedValue],
    invocations: &mut InvocationScope,
    mode: EvaluationMode,
) -> Result<Vec<TypedValue>, TypedProgramEvalError> {
    if region.inputs().len() != arguments.len()
        || region
            .inputs()
            .iter()
            .zip(arguments)
            .any(|(expected, actual)| expected != actual.value_type())
    {
        return Err(TypedProgramEvalError::InvalidCheckedProgram {
            operation: "bind structured region captures",
            provenance: region.provenance(),
        });
    }
    let mut frame = EvalFrame::new(table, region.body(), invocations, mode);
    for (slot, value) in frame.slots.iter_mut().zip(arguments) {
        *slot = Some(value.clone());
    }
    frame.run()?;
    let output_start = arguments.len();
    region
        .outputs()
        .iter()
        .enumerate()
        .map(|(output, _)| {
            frame
                .slots
                .get(output_start + output)
                .and_then(Clone::clone)
                .ok_or(TypedProgramEvalError::InvalidCheckedProgram {
                    operation: "read structured region output",
                    provenance: region.provenance(),
                })
        })
        .collect()
}

/// Results already issued in one exact function/domain invocation.
///
/// Conditional regions inherit this scope because they are lazy projections
/// of the same sequential function invocation. A compact `Map` or `Fold`
/// creates one fresh scope per domain point because its binder values are part
/// of the invocation coordinate.
struct InvocationScope {
    calls: Vec<Option<Vec<TypedValue>>>,
    #[cfg(test)]
    misses: Vec<u32>,
}

impl InvocationScope {
    fn new(table: &SolvePureCallTable) -> Self {
        Self {
            calls: vec![None; table.owners().len()],
            #[cfg(test)]
            misses: vec![0; table.owners().len()],
        }
    }

    fn get(&self, owner: SolvePureCallOwnerId) -> Option<&[TypedValue]> {
        self.calls
            .get(owner.index() as usize)
            .and_then(Option::as_deref)
    }

    fn insert(
        &mut self,
        owner: SolvePureCallOwnerId,
        values: Vec<TypedValue>,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let destination = self
            .calls
            .get_mut(owner.index() as usize)
            .ok_or(TypedProgramEvalError::UnknownOwner { owner })?;
        if destination.replace(values).is_some() {
            return invalid("redefine pure-call invocation", provenance);
        }
        #[cfg(test)]
        {
            self.misses[owner.index() as usize] += 1;
        }
        Ok(())
    }
}

struct EvalFrame<'model, 'scope> {
    table: &'model SolvePureCallTable,
    program: &'model TypedProgram,
    invocations: &'scope mut InvocationScope,
    mode: EvaluationMode,
    slots: Vec<Option<TypedValue>>,
    registers: Vec<Option<TypedValue>>,
}

impl<'model, 'scope> EvalFrame<'model, 'scope> {
    fn new(
        table: &'model SolvePureCallTable,
        program: &'model TypedProgram,
        invocations: &'scope mut InvocationScope,
        mode: EvaluationMode,
    ) -> Self {
        Self {
            table,
            program,
            invocations,
            mode,
            slots: vec![None; program.slots().len()],
            registers: vec![None; program.register_types().len()],
        }
    }

    fn run(&mut self) -> Result<(), TypedProgramEvalError> {
        for operation in self.program.operations() {
            self.eval_operation(operation.operation(), operation.provenance())?;
        }
        Ok(())
    }

    // SPEC_0021: exhaustive dispatch over the closed typed operation vocabulary.
    // SPEC_0021: Exception - cohesive exhaustive flow stays contiguous so ordering remains auditable.
    #[allow(clippy::too_many_lines)]
    fn eval_operation(
        &mut self,
        operation: &SolveOperation,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        match operation {
            SolveOperation::Constant { destination, value } => {
                self.write(*destination, TypedValue::scalar(value), provenance)
            }
            SolveOperation::Load { destination, slot } => {
                let value = self.read_slot(*slot, provenance)?.clone();
                self.write(*destination, value, provenance)
            }
            SolveOperation::Store { slot, source } => {
                let value = self.read(*source, provenance)?.clone();
                self.write_slot(*slot, value, provenance)
            }
            SolveOperation::Unary {
                destination,
                operator,
                operand,
            } => {
                let value =
                    eval_unary_typed(*operator, self.read(*operand, provenance)?, provenance)?;
                self.write(*destination, value, provenance)
            }
            SolveOperation::Binary {
                destination,
                operator,
                lhs,
                rhs,
            } => {
                let value = eval_binary_typed(
                    *operator,
                    self.read(*lhs, provenance)?,
                    self.read(*rhs, provenance)?,
                    provenance,
                )?;
                self.write(*destination, value, provenance)
            }
            SolveOperation::Compare {
                destination,
                operator,
                lhs,
                rhs,
            } => {
                let value = eval_compare_typed(
                    *operator,
                    self.read(*lhs, provenance)?,
                    self.read(*rhs, provenance)?,
                    provenance,
                )?;
                self.write(*destination, value, provenance)
            }
            SolveOperation::Convert {
                destination,
                operator,
                operand,
            } => {
                let destination_type = self.destination_type(*destination, provenance)?.clone();
                let value = eval_convert_typed(
                    *operator,
                    self.read(*operand, provenance)?,
                    destination_type,
                    provenance,
                )?;
                self.write(*destination, value, provenance)
            }
            SolveOperation::Select {
                destination,
                condition,
                if_true,
                if_false,
            } => {
                let selected = if scalar_boolean(self.read(*condition, provenance)?, provenance)? {
                    *if_true
                } else {
                    *if_false
                };
                let value = self.read(selected, provenance)?.clone();
                self.write(*destination, value, provenance)
            }
            SolveOperation::Conditional {
                condition,
                captures,
                destinations,
                if_true,
                if_false,
            } => self.eval_conditional(
                *condition,
                captures,
                destinations,
                if_true,
                if_false,
                provenance,
            ),
            SolveOperation::Map {
                domain,
                captures,
                destination,
                body,
            } => self.eval_map(domain, captures, *destination, body, provenance),
            SolveOperation::Fold {
                domain,
                initial,
                captures,
                destinations,
                transition,
            } => self.eval_fold(
                domain,
                initial,
                captures,
                destinations,
                transition,
                provenance,
            ),
            SolveOperation::Scale {
                destination,
                aggregate,
                scalar,
            } => self.eval_scale(*destination, *aggregate, *scalar, provenance),
            SolveOperation::BroadcastBinary {
                destination,
                operator,
                aggregate,
                scalar,
                scalar_on_lhs,
            } => self.eval_broadcast_binary(
                *destination,
                *operator,
                *aggregate,
                *scalar,
                *scalar_on_lhs,
                provenance,
            ),
            SolveOperation::Transpose {
                destination,
                operand,
            } => self.eval_transpose(*destination, *operand, provenance),
            SolveOperation::MatrixMultiply {
                destination,
                lhs,
                rhs,
            } => self.eval_matrix_multiply(*destination, *lhs, *rhs, provenance),
            SolveOperation::Cross {
                destination,
                lhs,
                rhs,
            } => self.eval_cross(*destination, *lhs, *rhs, provenance),
            SolveOperation::Reduce {
                destination,
                operator,
                operand,
            } => self.eval_reduce(*destination, *operator, *operand, provenance),
            SolveOperation::Identity { destination } => {
                self.eval_identity(*destination, provenance)
            }
            SolveOperation::Diagonal {
                destination,
                operand,
            } => self.eval_diagonal(*destination, *operand, provenance),
            SolveOperation::Concatenate {
                destination,
                axis,
                operands,
            } => self.eval_concatenate(*destination, *axis, operands, provenance),
            SolveOperation::Fill { destination, value } => {
                self.eval_fill(*destination, *value, provenance)
            }
            SolveOperation::ConstructAggregate {
                destination,
                elements,
            } => self.eval_construct(*destination, elements, provenance),
            SolveOperation::ProjectElement {
                destination,
                aggregate,
                indices,
            } => self.eval_project_element(*destination, *aggregate, indices, provenance),
            SolveOperation::ProjectElementDynamic {
                destination,
                aggregate,
                indices,
            } => self.eval_project_element_dynamic(*destination, *aggregate, indices, provenance),
            SolveOperation::ProjectSlice {
                destination,
                aggregate,
                origin,
            } => self.eval_project_slice(*destination, *aggregate, origin, provenance),
            SolveOperation::ProjectView {
                destination,
                aggregate,
                axes,
            } => self.eval_project_view(*destination, *aggregate, axes, provenance),
            SolveOperation::SelectElement {
                destination,
                aggregate,
                indices,
                out_of_range,
            } => self.eval_select_element(
                *destination,
                *aggregate,
                indices,
                *out_of_range,
                provenance,
            ),
            SolveOperation::UpdateElement {
                destination,
                aggregate,
                value,
                indices,
            } => self.eval_update_element(*destination, *aggregate, *value, indices, provenance),
            SolveOperation::UpdateSlice {
                destination,
                aggregate,
                value,
                origin,
            } => self.eval_update_slice(*destination, *aggregate, *value, origin, provenance),
            SolveOperation::UpdateView {
                destination,
                aggregate,
                value,
                axes,
            } => self.eval_update_view(*destination, *aggregate, *value, axes, provenance),
            SolveOperation::Call {
                owner,
                arguments,
                destinations,
            } => self.eval_call(*owner, arguments, destinations, provenance),
        }
    }

    fn eval_fill(
        &mut self,
        destination: SolveRegisterId,
        value: SolveRegisterId,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let element = scalar_element(self.read(value, provenance)?, provenance)?;
        let value_type = self.destination_type(destination, provenance)?.clone();
        let count = value_type.scalar_count() as usize;
        let value = TypedValue::checked(value_type, vec![element; count], provenance)?;
        self.write(destination, value, provenance)
    }

    fn eval_fold(
        &mut self,
        domain: &StructuredIndexDomain,
        initial: &[SolveRegisterId],
        captures: &[SolveRegisterId],
        destinations: &[SolveRegisterId],
        transition: &SolveProgramRegion,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let mut carried = initial
            .iter()
            .map(|value| self.read(*value, provenance).cloned())
            .collect::<Result<Vec<_>, _>>()?;
        let captures = captures
            .iter()
            .map(|value| self.read(*value, provenance).cloned())
            .collect::<Result<Vec<_>, _>>()?;
        let tuples = domain
            .index_tuple_iter()
            .map_err(|_| invalid_error("iterate compact fold domain", provenance))?;
        for tuple in tuples {
            let mut arguments = carried;
            arguments.extend(captures.iter().cloned());
            arguments.extend(
                tuple
                    .into_iter()
                    .map(|value| self.fold_binder(value, provenance))
                    .collect::<Result<Vec<_>, _>>()?,
            );
            let mut iteration = InvocationScope::new(self.table);
            carried = eval_region(
                self.table,
                transition,
                &arguments,
                &mut iteration,
                self.mode,
            )?;
        }
        if carried.len() != destinations.len() {
            return invalid("transfer compact fold outputs", provenance);
        }
        for (destination, value) in destinations.iter().zip(carried) {
            self.write(*destination, value, provenance)?;
        }
        Ok(())
    }

    fn eval_map(
        &mut self,
        domain: &StructuredIndexDomain,
        captures: &[SolveRegisterId],
        destination: SolveRegisterId,
        body: &SolveProgramRegion,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let captures = captures
            .iter()
            .map(|value| self.read(*value, provenance).cloned())
            .collect::<Result<Vec<_>, _>>()?;
        let value_type = self.destination_type(destination, provenance)?.clone();
        let mut elements = Vec::with_capacity(value_type.scalar_count() as usize);
        let tuples = domain
            .index_tuple_iter()
            .map_err(|_| invalid_error("iterate compact map domain", provenance))?;
        for tuple in tuples {
            let mut arguments = captures.clone();
            arguments.extend(
                tuple
                    .into_iter()
                    .map(|value| self.fold_binder(value, provenance))
                    .collect::<Result<Vec<_>, _>>()?,
            );
            let mut iteration = InvocationScope::new(self.table);
            let outputs = eval_region(self.table, body, &arguments, &mut iteration, self.mode)?;
            let [output] = outputs.as_slice() else {
                return invalid("collect compact map output", provenance);
            };
            elements.extend_from_slice(output.elements());
        }
        self.write(
            destination,
            TypedValue::checked(value_type, elements, provenance)?,
            provenance,
        )
    }

    fn fold_binder(
        &self,
        value: i64,
        provenance: Span,
    ) -> Result<TypedValue, TypedProgramEvalError> {
        let value_type = SolveValueType::scalar(SolveScalarType::integer(self.table.arithmetic()));
        TypedValue::checked(value_type, vec![SolveValueKind::Integer(value)], provenance)
    }

    fn eval_conditional(
        &mut self,
        condition: SolveRegisterId,
        captures: &[SolveRegisterId],
        destinations: &[SolveRegisterId],
        if_true: &SolveProgramRegion,
        if_false: &SolveProgramRegion,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let selected = if scalar_boolean(self.read(condition, provenance)?, provenance)? {
            if_true
        } else {
            if_false
        };
        let captures = captures
            .iter()
            .map(|capture| self.read(*capture, provenance).cloned())
            .collect::<Result<Vec<_>, _>>()?;
        let outputs = eval_region(self.table, selected, &captures, self.invocations, self.mode)?;
        if outputs.len() != destinations.len() {
            return invalid("transfer structured region outputs", provenance);
        }
        for (destination, value) in destinations.iter().zip(outputs) {
            self.write(*destination, value, provenance)?;
        }
        Ok(())
    }

    fn eval_construct(
        &mut self,
        destination: SolveRegisterId,
        elements: &[SolveRegisterId],
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let elements = elements
            .iter()
            .map(|element| {
                self.read(*element, provenance)
                    .map(|value| value.elements())
            })
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .copied()
            .collect();
        let value = TypedValue::checked(
            self.destination_type(destination, provenance)?.clone(),
            elements,
            provenance,
        )?;
        self.write(destination, value, provenance)
    }

    fn eval_project_element(
        &mut self,
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        indices: &[u32],
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let aggregate = self.read(aggregate, provenance)?;
        let offset = row_major_offset(aggregate.value_type.dimensions(), indices)
            .ok_or(invalid_error("project aggregate element", provenance))?;
        let element = aggregate
            .elements
            .get(offset)
            .copied()
            .ok_or(invalid_error("project aggregate element", provenance))?;
        let value = TypedValue::checked(
            self.destination_type(destination, provenance)?.clone(),
            vec![element],
            provenance,
        )?;
        self.write(destination, value, provenance)
    }

    fn eval_project_slice(
        &mut self,
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        origin: &[u32],
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let aggregate = self.read(aggregate, provenance)?;
        let value_type = self.destination_type(destination, provenance)?.clone();
        let elements = project_slice(aggregate, origin, &value_type, provenance)?;
        let value = TypedValue::checked(value_type, elements, provenance)?;
        self.write(destination, value, provenance)
    }

    fn eval_project_view(
        &mut self,
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        axes: &[SolveTensorViewAxis],
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let axes = self.resolve_tensor_view_axes(axes, provenance)?;
        let aggregate = self.read(aggregate, provenance)?;
        let value_type = self.destination_type(destination, provenance)?.clone();
        let elements = project_tensor_view(aggregate, &axes, &value_type, provenance)?;
        let value = TypedValue::checked(value_type, elements, provenance)?;
        self.write(destination, value, provenance)
    }

    fn eval_project_element_dynamic(
        &mut self,
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        indices: &[SolveRegisterId],
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let coordinates = indices
            .iter()
            .map(|index| scalar_integer(self.read(*index, provenance)?, provenance))
            .collect::<Result<Vec<_>, _>>()?;
        let element = select_modelica_element(self.read(aggregate, provenance)?, &coordinates)
            .ok_or(invalid_error("project aggregate element", provenance))?;
        let value = TypedValue::checked(
            self.destination_type(destination, provenance)?.clone(),
            vec![element],
            provenance,
        )?;
        self.write(destination, value, provenance)
    }

    fn eval_select_element(
        &mut self,
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        indices: &[SolveRegisterId],
        out_of_range: SolveRegisterId,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let coordinates = indices
            .iter()
            .map(|index| scalar_integer(self.read(*index, provenance)?, provenance))
            .collect::<Result<Vec<_>, _>>()?;
        let fallback = scalar_element(self.read(out_of_range, provenance)?, provenance)?;
        let element = select_modelica_element(self.read(aggregate, provenance)?, &coordinates)
            .unwrap_or(fallback);
        let value = TypedValue::checked(
            self.destination_type(destination, provenance)?.clone(),
            vec![element],
            provenance,
        )?;
        self.write(destination, value, provenance)
    }

    fn eval_update_element(
        &mut self,
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        value: SolveRegisterId,
        indices: &[SolveRegisterId],
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let coordinates = indices
            .iter()
            .map(|index| scalar_integer(self.read(*index, provenance)?, provenance))
            .collect::<Result<Vec<_>, _>>()?;
        let coordinates = coordinates
            .into_iter()
            .map(|index| {
                index
                    .checked_sub(1)
                    .and_then(|index| u32::try_from(index).ok())
            })
            .collect::<Option<Vec<_>>>()
            .ok_or(invalid_error("update aggregate element", provenance))?;
        let aggregate = self.read(aggregate, provenance)?;
        let offset = row_major_offset(aggregate.value_type.dimensions(), &coordinates)
            .ok_or(invalid_error("update aggregate element", provenance))?;
        let mut elements = aggregate.elements.to_vec();
        elements[offset] = scalar_element(self.read(value, provenance)?, provenance)?;
        let value = TypedValue::checked(
            self.destination_type(destination, provenance)?.clone(),
            elements,
            provenance,
        )?;
        self.write(destination, value, provenance)
    }

    fn eval_update_slice(
        &mut self,
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        value: SolveRegisterId,
        origin: &[u32],
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let aggregate = self.read(aggregate, provenance)?;
        let update = self.read(value, provenance)?;
        let mut elements = aggregate.elements.to_vec();
        let dimensions = update.value_type.dimensions();
        let mut coordinate = vec![0u32; dimensions.len()];
        for element in update.elements.iter().copied() {
            let destination_coordinate = coordinate
                .iter()
                .zip(origin)
                .map(|(coordinate, origin)| coordinate.checked_add(*origin))
                .collect::<Option<Vec<_>>>()
                .ok_or(invalid_error("update aggregate slice", provenance))?;
            let offset =
                row_major_offset(aggregate.value_type.dimensions(), &destination_coordinate)
                    .ok_or(invalid_error("update aggregate slice", provenance))?;
            elements[offset] = element;
            increment_coordinate(&mut coordinate, dimensions);
        }
        let value = TypedValue::checked(
            self.destination_type(destination, provenance)?.clone(),
            elements,
            provenance,
        )?;
        self.write(destination, value, provenance)
    }

    fn eval_update_view(
        &mut self,
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        value: SolveRegisterId,
        axes: &[SolveTensorViewAxis],
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let axes = self.resolve_tensor_view_axes(axes, provenance)?;
        let aggregate = self.read(aggregate, provenance)?;
        let update = self.read(value, provenance)?;
        let mut elements = aggregate.elements.to_vec();
        let dimensions = update.value_type.dimensions();
        let mut coordinate = vec![0u32; dimensions.len()];
        for element in update.elements.iter().copied() {
            let destination_coordinate = tensor_view_coordinate(&axes, &coordinate, provenance)?;
            let offset =
                row_major_offset(aggregate.value_type.dimensions(), &destination_coordinate)
                    .ok_or(invalid_error("update aggregate view", provenance))?;
            elements[offset] = element;
            increment_coordinate(&mut coordinate, dimensions);
        }
        let value = TypedValue::checked(
            self.destination_type(destination, provenance)?.clone(),
            elements,
            provenance,
        )?;
        self.write(destination, value, provenance)
    }

    fn resolve_tensor_view_axes(
        &self,
        axes: &[SolveTensorViewAxis],
        provenance: Span,
    ) -> Result<Vec<ResolvedTensorViewAxis>, TypedProgramEvalError> {
        axes.iter()
            .map(|axis| match *axis {
                SolveTensorViewAxis::Index(index) => {
                    scalar_integer(self.read(index, provenance)?, provenance)?
                        .checked_sub(1)
                        .and_then(|index| u32::try_from(index).ok())
                        .map(ResolvedTensorViewAxis::Index)
                        .ok_or(invalid_error("resolve aggregate view index", provenance))
                }
                SolveTensorViewAxis::Span { origin, .. } => {
                    Ok(ResolvedTensorViewAxis::Span { origin })
                }
            })
            .collect()
    }

    fn eval_call(
        &mut self,
        owner: SolvePureCallOwnerId,
        arguments: &[SolveRegisterId],
        destinations: &[SolveRegisterId],
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        if let Some(outputs) = self.invocations.get(owner).map(<[TypedValue]>::to_vec) {
            return self.transfer_call_outputs(outputs, destinations, provenance);
        }
        let arguments = arguments
            .iter()
            .map(|argument| self.read(*argument, provenance).cloned())
            .collect::<Result<Vec<_>, _>>()?;
        let called = self
            .table
            .owner(owner)
            .ok_or(TypedProgramEvalError::UnknownOwner { owner })?;
        let outputs = match self.mode {
            EvaluationMode::Primal => eval_owner(self.table, called, &arguments)?,
            EvaluationMode::Directional => eval_directional_owner(self.table, called, &arguments)?,
        };
        self.invocations
            .insert(owner, outputs.clone(), provenance)?;
        self.transfer_call_outputs(outputs, destinations, provenance)
    }

    fn transfer_call_outputs(
        &mut self,
        outputs: Vec<TypedValue>,
        destinations: &[SolveRegisterId],
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        if outputs.len() != destinations.len() {
            return invalid("transfer pure-call outputs", provenance);
        }
        for (destination, value) in destinations.iter().zip(outputs) {
            self.write(*destination, value, provenance)?;
        }
        Ok(())
    }

    fn read(
        &self,
        register: SolveRegisterId,
        provenance: Span,
    ) -> Result<&TypedValue, TypedProgramEvalError> {
        self.registers
            .get(register.index())
            .and_then(Option::as_ref)
            .ok_or(invalid_error("read register", provenance))
    }

    fn write(
        &mut self,
        register: SolveRegisterId,
        value: TypedValue,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        if self.destination_type(register, provenance)? != value.value_type() {
            return invalid("write register", provenance);
        }
        let destination = self
            .registers
            .get_mut(register.index())
            .ok_or(invalid_error("write register", provenance))?;
        if destination.replace(value).is_some() {
            return invalid("redefine register", provenance);
        }
        Ok(())
    }

    fn destination_type(
        &self,
        register: SolveRegisterId,
        provenance: Span,
    ) -> Result<&SolveValueType, TypedProgramEvalError> {
        self.program
            .register_types()
            .get(register.index())
            .ok_or(invalid_error("resolve register type", provenance))
    }

    fn read_slot(
        &self,
        slot: SolveSlotId,
        provenance: Span,
    ) -> Result<&TypedValue, TypedProgramEvalError> {
        self.slots
            .get(slot.index())
            .and_then(Option::as_ref)
            .ok_or(invalid_error("read slot", provenance))
    }

    fn write_slot(
        &mut self,
        slot: SolveSlotId,
        value: TypedValue,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let expected = self
            .program
            .slots()
            .get(slot.index())
            .ok_or(invalid_error("resolve slot type", provenance))?;
        if expected.value_type() != value.value_type() {
            return invalid("write slot", provenance);
        }
        let destination = self
            .slots
            .get_mut(slot.index())
            .ok_or(invalid_error("write slot", provenance))?;
        *destination = Some(value);
        Ok(())
    }
}

fn element_matches_type(value: SolveValueKind, scalar: SolveScalarType) -> bool {
    match (value, scalar) {
        (
            SolveValueKind::Real32(_),
            SolveScalarType::Real {
                format: SolveRealFormat::Binary32,
                ..
            },
        )
        | (
            SolveValueKind::Real64(_),
            SolveScalarType::Real {
                format: SolveRealFormat::Binary64,
                ..
            },
        )
        | (SolveValueKind::Boolean(_), SolveScalarType::Boolean) => true,
        (SolveValueKind::Integer(value), SolveScalarType::Integer(domain)) => {
            domain.contains(value)
        }
        _ => false,
    }
}

fn scalar_element(
    value: &TypedValue,
    provenance: Span,
) -> Result<SolveValueKind, TypedProgramEvalError> {
    if !value.value_type.dimensions().is_empty() || value.elements.len() != 1 {
        return invalid("read scalar value", provenance);
    }
    Ok(value.elements[0])
}

fn scalar_boolean(value: &TypedValue, provenance: Span) -> Result<bool, TypedProgramEvalError> {
    match scalar_element(value, provenance)? {
        SolveValueKind::Boolean(value) => Ok(value),
        _ => invalid("read Boolean condition", provenance),
    }
}

fn scalar_integer(value: &TypedValue, provenance: Span) -> Result<i64, TypedProgramEvalError> {
    match scalar_element(value, provenance)? {
        SolveValueKind::Integer(value) => Ok(value),
        _ => invalid("read Integer index", provenance),
    }
}

fn row_major_offset(dimensions: &[u32], indices: &[u32]) -> Option<usize> {
    if dimensions.len() != indices.len() {
        return None;
    }
    dimensions
        .iter()
        .copied()
        .zip(indices.iter().copied())
        .try_fold(0usize, |offset, (extent, index)| {
            if index >= extent {
                return None;
            }
            offset
                .checked_mul(extent as usize)?
                .checked_add(index as usize)
        })
}

fn select_modelica_element(value: &TypedValue, indices: &[i64]) -> Option<SolveValueKind> {
    let indices = indices
        .iter()
        .copied()
        .map(|index| {
            index
                .checked_sub(1)
                .and_then(|index| u32::try_from(index).ok())
        })
        .collect::<Option<Vec<_>>>()?;
    let offset = row_major_offset(value.value_type.dimensions(), &indices)?;
    value.elements.get(offset).copied()
}

fn project_slice(
    aggregate: &TypedValue,
    origin: &[u32],
    destination_type: &SolveValueType,
    provenance: Span,
) -> Result<Vec<SolveValueKind>, TypedProgramEvalError> {
    let dimensions = destination_type.dimensions();
    let mut coordinate = vec![0u32; dimensions.len()];
    let mut elements = Vec::with_capacity(destination_type.scalar_count() as usize);
    for _ in 0..destination_type.scalar_count() {
        let source = coordinate
            .iter()
            .zip(origin)
            .map(|(coordinate, origin)| coordinate.checked_add(*origin))
            .collect::<Option<Vec<_>>>()
            .ok_or(invalid_error("project aggregate slice", provenance))?;
        let offset = row_major_offset(aggregate.value_type.dimensions(), &source)
            .ok_or(invalid_error("project aggregate slice", provenance))?;
        elements.push(
            aggregate
                .elements
                .get(offset)
                .copied()
                .ok_or(invalid_error("project aggregate slice", provenance))?,
        );
        increment_coordinate(&mut coordinate, dimensions);
    }
    Ok(elements)
}

#[derive(Clone, Copy)]
enum ResolvedTensorViewAxis {
    Index(u32),
    Span { origin: u32 },
}

fn project_tensor_view(
    aggregate: &TypedValue,
    axes: &[ResolvedTensorViewAxis],
    destination_type: &SolveValueType,
    provenance: Span,
) -> Result<Vec<SolveValueKind>, TypedProgramEvalError> {
    let dimensions = destination_type.dimensions();
    let mut coordinate = vec![0u32; dimensions.len()];
    let mut elements = Vec::with_capacity(destination_type.scalar_count() as usize);
    for _ in 0..destination_type.scalar_count() {
        let source = tensor_view_coordinate(axes, &coordinate, provenance)?;
        let offset = row_major_offset(aggregate.value_type.dimensions(), &source)
            .ok_or(invalid_error("project aggregate view", provenance))?;
        elements.push(
            aggregate
                .elements
                .get(offset)
                .copied()
                .ok_or(invalid_error("project aggregate view", provenance))?,
        );
        increment_coordinate(&mut coordinate, dimensions);
    }
    Ok(elements)
}

fn tensor_view_coordinate(
    axes: &[ResolvedTensorViewAxis],
    coordinate: &[u32],
    provenance: Span,
) -> Result<Vec<u32>, TypedProgramEvalError> {
    let mut retained = coordinate.iter().copied();
    let result = axes
        .iter()
        .map(|axis| match *axis {
            ResolvedTensorViewAxis::Index(index) => Some(index),
            ResolvedTensorViewAxis::Span { origin } => origin.checked_add(retained.next()?),
        })
        .collect::<Option<Vec<_>>>()
        .ok_or(invalid_error(
            "resolve aggregate view coordinate",
            provenance,
        ))?;
    if retained.next().is_some() {
        return invalid("resolve aggregate view coordinate", provenance);
    }
    Ok(result)
}

fn increment_coordinate(coordinate: &mut [u32], dimensions: &[u32]) {
    for axis in (0..coordinate.len()).rev() {
        coordinate[axis] += 1;
        if coordinate[axis] < dimensions[axis] {
            return;
        }
        coordinate[axis] = 0;
    }
}

fn invalid<T>(operation: &'static str, provenance: Span) -> Result<T, TypedProgramEvalError> {
    Err(invalid_error(operation, provenance))
}

fn invalid_error(operation: &'static str, provenance: Span) -> TypedProgramEvalError {
    TypedProgramEvalError::InvalidCheckedProgram {
        operation,
        provenance,
    }
}

#[cfg(test)]
mod tests;
