//! Checked forward directional transformation for compact typed programs.
//!
//! The transform duplicates differentiable aggregate values, never their
//! coordinates. Structured regions and tensor operations remain structured;
//! an unsupported vocabulary item rejects the directional owner as a whole.

use super::*;
use crate::typed_program::call::{
    SolvePureCallDirectionalInterface, SolvePureCallDirectionalOwner, SolvePureCallOutput,
    SolvePureCallOutputKind,
};

#[derive(Clone, Copy)]
struct Directional<T> {
    primal: T,
    tangent: Option<T>,
}

impl TypedProgram {
    pub(in crate::typed_program) fn derive_directional_owner(
        &self,
        inputs: &[SolveValueType],
        outputs: &[SolvePureCallOutput],
        available: Vec<Option<SolvePureCallDirectionalInterface>>,
        provenance: Span,
    ) -> Result<Option<SolvePureCallDirectionalOwner>, SolveProgramConstructionError> {
        if !program_supports_directional(self, &available) {
            return Ok(None);
        }
        let directional_inputs = expand_types(inputs);
        let directional_outputs = expand_outputs(outputs);
        let body = derive_program(self, inputs.len(), outputs.len(), &available)?;
        if body.slots().len() < directional_inputs.len() + directional_outputs.len() {
            return Err(SolveProgramConstructionError::InvalidCallInterface { provenance });
        }
        Ok(Some(SolvePureCallDirectionalOwner::new(
            directional_inputs,
            directional_outputs,
            body,
        )))
    }
}

fn is_real(value_type: &SolveValueType) -> bool {
    matches!(value_type.element_type(), SolveScalarType::Real { .. })
}

fn expand_types(types: &[SolveValueType]) -> Vec<SolveValueType> {
    types
        .iter()
        .flat_map(|value_type| {
            std::iter::once(value_type.clone())
                .chain(is_real(value_type).then(|| value_type.clone()))
        })
        .collect()
}

fn expand_outputs(outputs: &[SolvePureCallOutput]) -> Vec<SolvePureCallOutput> {
    outputs
        .iter()
        .flat_map(|output| {
            let primal = match output.kind() {
                SolvePureCallOutputKind::Result => {
                    SolvePureCallOutput::result(output.value_type().clone())
                }
                SolvePureCallOutputKind::AssertionPredicate => {
                    SolvePureCallOutput::assertion_predicate()
                }
            };
            let tangent = (output.kind() == SolvePureCallOutputKind::Result
                && is_real(output.value_type()))
            .then(|| SolvePureCallOutput::result(output.value_type().clone()));
            std::iter::once(primal).chain(tangent)
        })
        .collect()
}

fn program_supports_directional(
    program: &TypedProgram,
    available: &[Option<SolvePureCallDirectionalInterface>],
) -> bool {
    program
        .operations()
        .iter()
        .all(|operation| match operation.operation() {
            SolveOperation::Map { .. } => false,
            SolveOperation::BroadcastBinary {
                operator:
                    SolveBinaryOperator::Divide
                    | SolveBinaryOperator::Power
                    | SolveBinaryOperator::Atan2
                    | SolveBinaryOperator::Min
                    | SolveBinaryOperator::Max
                    | SolveBinaryOperator::And
                    | SolveBinaryOperator::Or,
                ..
            } => false,
            SolveOperation::Reduce {
                operator:
                    SolveReductionOperator::Product
                    | SolveReductionOperator::Minimum
                    | SolveReductionOperator::Maximum,
                ..
            } => false,
            SolveOperation::Call { owner, .. } => available
                .get(owner.index() as usize)
                .is_some_and(Option::is_some),
            SolveOperation::Conditional {
                if_true, if_false, ..
            } => {
                program_supports_directional(if_true.body(), available)
                    && program_supports_directional(if_false.body(), available)
            }
            SolveOperation::Fold { transition, .. } => {
                program_supports_directional(transition.body(), available)
            }
            SolveOperation::Unary {
                destination,
                operator:
                    SolveUnaryOperator::Abs
                    | SolveUnaryOperator::Sqrt
                    | SolveUnaryOperator::Asin
                    | SolveUnaryOperator::Acos
                    | SolveUnaryOperator::Log
                    | SolveUnaryOperator::Log10,
                ..
            }
            | SolveOperation::Binary {
                destination,
                operator:
                    SolveBinaryOperator::Divide
                    | SolveBinaryOperator::Power
                    | SolveBinaryOperator::Min
                    | SolveBinaryOperator::Max
                    | SolveBinaryOperator::Atan2,
                ..
            } => program
                .register_types()
                .get(destination.index())
                .is_some_and(|value_type| value_type.dimensions().is_empty()),
            _ => true,
        })
}

fn directional_interfaces(
    available: &[Option<SolvePureCallDirectionalInterface>],
) -> Vec<SolvePureCallInterface> {
    available
        .iter()
        .enumerate()
        .map(|(index, interface)| match interface {
            Some(interface) => SolvePureCallInterface {
                id: interface.id,
                inputs: interface.inputs.clone(),
                outputs: interface.outputs.clone(),
            },
            None => SolvePureCallInterface {
                id: SolvePureCallOwnerId::from_index(index as u32),
                inputs: Box::new([]),
                outputs: Box::new([]),
            },
        })
        .collect()
}

fn derive_program(
    primal: &TypedProgram,
    input_count: usize,
    output_count: usize,
    available: &[Option<SolvePureCallDirectionalInterface>],
) -> Result<TypedProgram, SolveProgramConstructionError> {
    TypedProgram::construct_with_calls(
        primal.arithmetic(),
        directional_interfaces(available),
        |builder| {
            let mut slots = Vec::with_capacity(primal.slots().len());
            for slot in primal.slots() {
                let primal_slot = builder.declare_slot(
                    slot.value_type().clone(),
                    slot.storage(),
                    slot.access(),
                    slot.provenance(),
                )?;
                let tangent = (is_real(slot.value_type())
                    && slot.storage() != SolveStorageClass::Constant)
                    .then(|| {
                        builder.declare_slot(
                            slot.value_type().clone(),
                            slot.storage(),
                            slot.access(),
                            slot.provenance(),
                        )
                    })
                    .transpose()?;
                slots.push(Directional {
                    primal: primal_slot,
                    tangent,
                });
            }
            let expected_interface = expand_types(
                &primal.slots()[..input_count]
                    .iter()
                    .map(|slot| slot.value_type().clone())
                    .collect::<Vec<_>>(),
            )
            .len()
                + expand_types(
                    &primal.slots()[input_count..input_count + output_count]
                        .iter()
                        .map(|slot| slot.value_type().clone())
                        .collect::<Vec<_>>(),
                )
                .len();
            debug_assert_eq!(
                slots
                    .iter()
                    .take(input_count + output_count)
                    .map(|slot| 1 + usize::from(slot.tangent.is_some()))
                    .sum::<usize>(),
                expected_interface,
            );
            let mut directional = DirectionalBuilder {
                primal,
                builder,
                slots,
                registers: vec![None; primal.register_types().len()],
                available,
            };
            directional.lower_all()
        },
    )
}

struct DirectionalBuilder<'primal, 'program> {
    primal: &'primal TypedProgram,
    builder: &'primal mut TypedProgramBuilder<'program>,
    slots: Vec<Directional<ProgramSlot<'program>>>,
    registers: Vec<Option<Directional<ProgramRegister<'program>>>>,
    available: &'primal [Option<SolvePureCallDirectionalInterface>],
}

impl<'primal, 'program> DirectionalBuilder<'primal, 'program> {
    fn lower_all(&mut self) -> Result<(), SolveProgramConstructionError> {
        for operation in self.primal.operations() {
            self.lower(operation.operation(), operation.provenance())?;
        }
        Ok(())
    }

    fn bind(
        &mut self,
        destination: SolveRegisterId,
        value: Directional<ProgramRegister<'program>>,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let slot = self
            .registers
            .get_mut(destination.index())
            .ok_or(SolveProgramConstructionError::InvalidRegion { provenance })?;
        if slot.replace(value).is_some() {
            return Err(SolveProgramConstructionError::InvalidRegion { provenance });
        }
        Ok(())
    }

    fn get(
        &self,
        register: SolveRegisterId,
        provenance: Span,
    ) -> Result<Directional<ProgramRegister<'program>>, SolveProgramConstructionError> {
        self.registers
            .get(register.index())
            .copied()
            .flatten()
            .ok_or(SolveProgramConstructionError::InvalidRegion { provenance })
    }

    fn zero(
        &mut self,
        value_type: &SolveValueType,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        let scalar = self
            .builder
            .constant(SolveValue::real(self.primal.arithmetic(), 0.0), provenance)?;
        if value_type.dimensions().is_empty() {
            Ok(scalar)
        } else {
            self.builder
                .fill(scalar, value_type.dimensions().to_vec(), provenance)
        }
    }

    fn constant_like(
        &mut self,
        value_type: &SolveValueType,
        value: f64,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        let scalar = self.builder.constant(
            SolveValue::real(self.primal.arithmetic(), value),
            provenance,
        )?;
        if value_type.dimensions().is_empty() {
            Ok(scalar)
        } else {
            self.builder
                .fill(scalar, value_type.dimensions().to_vec(), provenance)
        }
    }

    fn tangent_or_zero(
        &mut self,
        value: Directional<ProgramRegister<'program>>,
        value_type: &SolveValueType,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        match value.tangent {
            Some(tangent) => Ok(tangent),
            None => self.zero(value_type, provenance),
        }
    }

    fn bind_nonreal(
        &mut self,
        destination: SolveRegisterId,
        primal: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        self.bind(
            destination,
            Directional {
                primal,
                tangent: None,
            },
            provenance,
        )
    }

    #[allow(clippy::too_many_lines)]
    fn lower(
        &mut self,
        operation: &SolveOperation,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        match operation {
            SolveOperation::Constant { destination, value } => {
                let primal = self.builder.constant(value.clone(), provenance)?;
                let tangent = is_real(value.value_type())
                    .then(|| self.zero(value.value_type(), provenance))
                    .transpose()?;
                self.bind(*destination, Directional { primal, tangent }, provenance)
            }
            SolveOperation::Load { destination, slot } => {
                let source = self.slots[slot.index()];
                let primal = self.builder.load(source.primal, provenance)?;
                let tangent = match source.tangent {
                    Some(tangent) => Some(self.builder.load(tangent, provenance)?),
                    None if is_real(self.primal.slots()[slot.index()].value_type()) => {
                        Some(self.zero(self.primal.slots()[slot.index()].value_type(), provenance)?)
                    }
                    None => None,
                };
                self.bind(*destination, Directional { primal, tangent }, provenance)
            }
            SolveOperation::Store { slot, source } => {
                let destination = self.slots[slot.index()];
                let source = self.get(*source, provenance)?;
                self.builder
                    .store(destination.primal, source.primal, provenance)?;
                if let Some(destination) = destination.tangent {
                    let value_type = self.primal.slots()[slot.index()].value_type().clone();
                    let tangent = self.tangent_or_zero(source, &value_type, provenance)?;
                    self.builder.store(destination, tangent, provenance)?;
                }
                Ok(())
            }
            SolveOperation::Unary {
                destination,
                operator,
                operand,
            } => {
                let operand = self.get(*operand, provenance)?;
                let value_type = self.primal.register_types()[destination.index()].clone();
                let result = self.lower_unary(*operator, operand, &value_type, provenance)?;
                self.bind(*destination, result, provenance)
            }
            SolveOperation::Binary {
                destination,
                operator,
                lhs,
                rhs,
            } => {
                let lhs = self.get(*lhs, provenance)?;
                let rhs = self.get(*rhs, provenance)?;
                let value_type = self.primal.register_types()[destination.index()].clone();
                let result = self.lower_binary(*operator, lhs, rhs, &value_type, provenance)?;
                self.bind(*destination, result, provenance)
            }
            SolveOperation::Compare {
                destination,
                operator,
                lhs,
                rhs,
            } => {
                let lhs = self.get(*lhs, provenance)?.primal;
                let rhs = self.get(*rhs, provenance)?.primal;
                let primal = self.builder.compare(*operator, lhs, rhs, provenance)?;
                self.bind_nonreal(*destination, primal, provenance)
            }
            SolveOperation::Convert {
                destination,
                operator,
                operand,
            } => {
                let operand = self.get(*operand, provenance)?;
                let primal = self
                    .builder
                    .convert(*operator, operand.primal, provenance)?;
                let tangent = is_real(&self.primal.register_types()[destination.index()])
                    .then(|| {
                        self.zero(
                            &self.primal.register_types()[destination.index()],
                            provenance,
                        )
                    })
                    .transpose()?;
                self.bind(*destination, Directional { primal, tangent }, provenance)
            }
            SolveOperation::Select {
                destination,
                condition,
                if_true,
                if_false,
            } => {
                let condition = self.get(*condition, provenance)?.primal;
                let if_true = self.get(*if_true, provenance)?;
                let if_false = self.get(*if_false, provenance)?;
                let primal =
                    self.builder
                        .select(condition, if_true.primal, if_false.primal, provenance)?;
                let tangent = if is_real(&self.primal.register_types()[destination.index()]) {
                    let value_type = self.primal.register_types()[destination.index()].clone();
                    let if_true = self.tangent_or_zero(if_true, &value_type, provenance)?;
                    let if_false = self.tangent_or_zero(if_false, &value_type, provenance)?;
                    Some(
                        self.builder
                            .select(condition, if_true, if_false, provenance)?,
                    )
                } else {
                    None
                };
                self.bind(*destination, Directional { primal, tangent }, provenance)
            }
            SolveOperation::Conditional {
                condition,
                captures,
                destinations,
                if_true,
                if_false,
            } => self.lower_conditional(
                *condition,
                captures,
                destinations,
                if_true,
                if_false,
                provenance,
            ),
            SolveOperation::Fold {
                domain,
                initial,
                captures,
                destinations,
                transition,
            } => self.lower_fold(
                domain,
                initial,
                captures,
                destinations,
                transition,
                provenance,
            ),
            SolveOperation::Map { .. } => {
                Err(SolveProgramConstructionError::InvalidMap { provenance })
            }
            SolveOperation::Scale {
                destination,
                aggregate,
                scalar,
            } => self.lower_scale(*destination, *aggregate, *scalar, provenance),
            SolveOperation::BroadcastBinary {
                destination,
                operator,
                aggregate,
                scalar,
                scalar_on_lhs,
            } => self.lower_broadcast(
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
            } => self.lower_linear_tensor_unary(
                *destination,
                *operand,
                provenance,
                |builder, value, provenance| builder.transpose(value, provenance),
            ),
            SolveOperation::MatrixMultiply {
                destination,
                lhs,
                rhs,
            } => self.lower_matrix_multiply(*destination, *lhs, *rhs, provenance),
            SolveOperation::Cross {
                destination,
                lhs,
                rhs,
            } => self.lower_cross(*destination, *lhs, *rhs, provenance),
            SolveOperation::Reduce {
                destination,
                operator,
                operand,
            } => self.lower_reduce(*destination, *operator, *operand, provenance),
            SolveOperation::Identity { destination } => {
                let value_type = self.primal.register_types()[destination.index()].clone();
                let [rows, columns] = value_type.dimensions() else {
                    return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
                };
                if rows != columns {
                    return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
                }
                let primal = self
                    .builder
                    .identity(value_type.element_type(), *rows, provenance)?;
                let tangent = is_real(&value_type)
                    .then(|| self.zero(&value_type, provenance))
                    .transpose()?;
                self.bind(*destination, Directional { primal, tangent }, provenance)
            }
            SolveOperation::Diagonal {
                destination,
                operand,
            } => self.lower_linear_tensor_unary(
                *destination,
                *operand,
                provenance,
                |builder, value, provenance| builder.diagonal(value, provenance),
            ),
            SolveOperation::Concatenate {
                destination,
                axis,
                operands,
            } => self.lower_concatenate(*destination, *axis, operands, provenance),
            SolveOperation::Fill { destination, value } => {
                let dimensions = self.primal.register_types()[destination.index()]
                    .dimensions()
                    .to_vec();
                let value = self.get(*value, provenance)?;
                let primal = self
                    .builder
                    .fill(value.primal, dimensions.clone(), provenance)?;
                let tangent = match value.tangent {
                    Some(tangent) => Some(self.builder.fill(tangent, dimensions, provenance)?),
                    None => None,
                };
                self.bind(*destination, Directional { primal, tangent }, provenance)
            }
            SolveOperation::ConstructAggregate {
                destination,
                elements,
            } => self.lower_construct(*destination, elements, provenance),
            SolveOperation::ProjectElement {
                destination,
                aggregate,
                indices,
            } => self.lower_project_element(*destination, *aggregate, indices.to_vec(), provenance),
            SolveOperation::ProjectElementDynamic {
                destination,
                aggregate,
                indices,
            } => self.lower_project_element_dynamic(*destination, *aggregate, indices, provenance),
            SolveOperation::ProjectSlice {
                destination,
                aggregate,
                origin,
            } => self.lower_project_slice(*destination, *aggregate, origin.to_vec(), provenance),
            SolveOperation::ProjectView {
                destination,
                aggregate,
                axes,
            } => self.lower_project_view(*destination, *aggregate, axes, provenance),
            SolveOperation::SelectElement {
                destination,
                aggregate,
                indices,
                out_of_range,
            } => self.lower_select_element(
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
            } => self.lower_update_element(*destination, *aggregate, *value, indices, provenance),
            SolveOperation::UpdateSlice {
                destination,
                aggregate,
                value,
                origin,
            } => self.lower_update_slice(
                *destination,
                *aggregate,
                *value,
                origin.to_vec(),
                provenance,
            ),
            SolveOperation::UpdateView {
                destination,
                aggregate,
                value,
                axes,
            } => self.lower_update_view(*destination, *aggregate, *value, axes, provenance),
            SolveOperation::Call {
                owner,
                arguments,
                destinations,
            } => self.lower_call(*owner, arguments, destinations, provenance),
        }
    }

    // SPEC_0021: Exception - exhaustive tangent relation over every checked
    // unary operator; keeping primal and tangent clauses adjacent makes the
    // construction proof reviewable as one total match.
    #[allow(clippy::too_many_lines)]
    fn lower_unary(
        &mut self,
        operator: SolveUnaryOperator,
        operand: Directional<ProgramRegister<'program>>,
        value_type: &SolveValueType,
        provenance: Span,
    ) -> Result<Directional<ProgramRegister<'program>>, SolveProgramConstructionError> {
        let primal = self.builder.unary(operator, operand.primal, provenance)?;
        if !is_real(value_type) {
            return Ok(Directional {
                primal,
                tangent: None,
            });
        }
        let tangent = self.tangent_or_zero(operand, value_type, provenance)?;
        let zero = self.zero(value_type, provenance)?;
        let derivative = match operator {
            SolveUnaryOperator::Negate => {
                self.builder
                    .unary(SolveUnaryOperator::Negate, tangent, provenance)?
            }
            SolveUnaryOperator::Not => zero,
            SolveUnaryOperator::Abs => {
                let negative =
                    self.builder
                        .unary(SolveUnaryOperator::Negate, tangent, provenance)?;
                let condition = self.builder.compare(
                    SolveCompareOperator::GreaterEqual,
                    operand.primal,
                    zero,
                    provenance,
                )?;
                self.builder
                    .select(condition, tangent, negative, provenance)?
            }
            SolveUnaryOperator::Sign
            | SolveUnaryOperator::Floor
            | SolveUnaryOperator::Ceiling
            | SolveUnaryOperator::Truncate => zero,
            SolveUnaryOperator::Sin => {
                let factor =
                    self.builder
                        .unary(SolveUnaryOperator::Cos, operand.primal, provenance)?;
                self.builder
                    .binary(SolveBinaryOperator::Multiply, tangent, factor, provenance)?
            }
            SolveUnaryOperator::Cos => {
                let factor =
                    self.builder
                        .unary(SolveUnaryOperator::Sin, operand.primal, provenance)?;
                let factor = self
                    .builder
                    .unary(SolveUnaryOperator::Negate, factor, provenance)?;
                self.builder
                    .binary(SolveBinaryOperator::Multiply, tangent, factor, provenance)?
            }
            SolveUnaryOperator::Tan => {
                let factor =
                    self.builder
                        .unary(SolveUnaryOperator::Cos, operand.primal, provenance)?;
                let denominator = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    factor,
                    factor,
                    provenance,
                )?;
                self.builder.binary(
                    SolveBinaryOperator::Divide,
                    tangent,
                    denominator,
                    provenance,
                )?
            }
            SolveUnaryOperator::Asin | SolveUnaryOperator::Acos => {
                let one = self.constant_like(value_type, 1.0, provenance)?;
                let square = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    operand.primal,
                    operand.primal,
                    provenance,
                )?;
                let denominator =
                    self.builder
                        .binary(SolveBinaryOperator::Subtract, one, square, provenance)?;
                let denominator =
                    self.builder
                        .unary(SolveUnaryOperator::Sqrt, denominator, provenance)?;
                let derivative = self.builder.binary(
                    SolveBinaryOperator::Divide,
                    tangent,
                    denominator,
                    provenance,
                )?;
                let signed = if operator == SolveUnaryOperator::Acos {
                    self.builder
                        .unary(SolveUnaryOperator::Negate, derivative, provenance)?
                } else {
                    derivative
                };
                let tangent_is_zero =
                    self.builder
                        .compare(SolveCompareOperator::Equal, tangent, zero, provenance)?;
                self.builder
                    .select(tangent_is_zero, zero, signed, provenance)?
            }
            SolveUnaryOperator::Atan => {
                let one = self.constant_like(value_type, 1.0, provenance)?;
                let square = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    operand.primal,
                    operand.primal,
                    provenance,
                )?;
                let denominator =
                    self.builder
                        .binary(SolveBinaryOperator::Add, one, square, provenance)?;
                self.builder.binary(
                    SolveBinaryOperator::Divide,
                    tangent,
                    denominator,
                    provenance,
                )?
            }
            SolveUnaryOperator::Sinh | SolveUnaryOperator::Cosh => {
                let derivative_operator = if operator == SolveUnaryOperator::Sinh {
                    SolveUnaryOperator::Cosh
                } else {
                    SolveUnaryOperator::Sinh
                };
                let factor = self
                    .builder
                    .unary(derivative_operator, operand.primal, provenance)?;
                self.builder
                    .binary(SolveBinaryOperator::Multiply, tangent, factor, provenance)?
            }
            SolveUnaryOperator::Tanh => {
                let factor =
                    self.builder
                        .unary(SolveUnaryOperator::Cosh, operand.primal, provenance)?;
                let denominator = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    factor,
                    factor,
                    provenance,
                )?;
                self.builder.binary(
                    SolveBinaryOperator::Divide,
                    tangent,
                    denominator,
                    provenance,
                )?
            }
            SolveUnaryOperator::Exp => {
                self.builder
                    .binary(SolveBinaryOperator::Multiply, tangent, primal, provenance)?
            }
            SolveUnaryOperator::Log | SolveUnaryOperator::Log10 => {
                let denominator = if operator == SolveUnaryOperator::Log10 {
                    let ln10 =
                        self.constant_like(value_type, std::f64::consts::LN_10, provenance)?;
                    self.builder.binary(
                        SolveBinaryOperator::Multiply,
                        operand.primal,
                        ln10,
                        provenance,
                    )?
                } else {
                    operand.primal
                };
                let derivative = self.builder.binary(
                    SolveBinaryOperator::Divide,
                    tangent,
                    denominator,
                    provenance,
                )?;
                let nonzero = self.builder.compare(
                    SolveCompareOperator::NotEqual,
                    operand.primal,
                    zero,
                    provenance,
                )?;
                self.builder.select(nonzero, derivative, zero, provenance)?
            }
            SolveUnaryOperator::Sqrt => {
                let two = self.constant_like(value_type, 2.0, provenance)?;
                let denominator =
                    self.builder
                        .binary(SolveBinaryOperator::Multiply, two, primal, provenance)?;
                let derivative = self.builder.binary(
                    SolveBinaryOperator::Divide,
                    tangent,
                    denominator,
                    provenance,
                )?;
                let nonzero = self.builder.compare(
                    SolveCompareOperator::NotEqual,
                    operand.primal,
                    zero,
                    provenance,
                )?;
                self.builder.select(nonzero, derivative, zero, provenance)?
            }
        };
        Ok(Directional {
            primal,
            tangent: Some(derivative),
        })
    }

    // SPEC_0021: Exception - exhaustive tangent relation over every checked
    // binary operator, including the scalar AD singular-value guards.
    #[allow(clippy::too_many_lines)]
    fn lower_binary(
        &mut self,
        operator: SolveBinaryOperator,
        lhs: Directional<ProgramRegister<'program>>,
        rhs: Directional<ProgramRegister<'program>>,
        value_type: &SolveValueType,
        provenance: Span,
    ) -> Result<Directional<ProgramRegister<'program>>, SolveProgramConstructionError> {
        let direct_primal = self
            .builder
            .binary(operator, lhs.primal, rhs.primal, provenance)?;
        if !is_real(value_type) {
            return Ok(Directional {
                primal: direct_primal,
                tangent: None,
            });
        }
        let zero = self.zero(value_type, provenance)?;
        let primal = if operator == SolveBinaryOperator::Divide {
            let denominator_is_zero =
                self.builder
                    .compare(SolveCompareOperator::Equal, rhs.primal, zero, provenance)?;
            let numerator_is_zero =
                self.builder
                    .compare(SolveCompareOperator::Equal, lhs.primal, zero, provenance)?;
            let zero_over_zero =
                self.builder
                    .select(numerator_is_zero, zero, direct_primal, provenance)?;
            self.builder.select(
                denominator_is_zero,
                zero_over_zero,
                direct_primal,
                provenance,
            )?
        } else {
            direct_primal
        };
        let lhs_tangent = self.tangent_or_zero(lhs, value_type, provenance)?;
        let rhs_tangent = self.tangent_or_zero(rhs, value_type, provenance)?;
        let tangent = match operator {
            SolveBinaryOperator::Add | SolveBinaryOperator::Subtract => {
                self.builder
                    .binary(operator, lhs_tangent, rhs_tangent, provenance)?
            }
            SolveBinaryOperator::Multiply => {
                let first = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    lhs_tangent,
                    rhs.primal,
                    provenance,
                )?;
                let second = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    lhs.primal,
                    rhs_tangent,
                    provenance,
                )?;
                self.builder
                    .binary(SolveBinaryOperator::Add, first, second, provenance)?
            }
            SolveBinaryOperator::Divide => {
                let first = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    lhs_tangent,
                    rhs.primal,
                    provenance,
                )?;
                let second = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    lhs.primal,
                    rhs_tangent,
                    provenance,
                )?;
                let numerator = self.builder.binary(
                    SolveBinaryOperator::Subtract,
                    first,
                    second,
                    provenance,
                )?;
                let denominator = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    rhs.primal,
                    rhs.primal,
                    provenance,
                )?;
                let derivative = self.builder.binary(
                    SolveBinaryOperator::Divide,
                    numerator,
                    denominator,
                    provenance,
                )?;
                let denominator_is_zero = self.builder.compare(
                    SolveCompareOperator::Equal,
                    rhs.primal,
                    zero,
                    provenance,
                )?;
                self.builder
                    .select(denominator_is_zero, zero, derivative, provenance)?
            }
            SolveBinaryOperator::Power => {
                let rhs_constant = self.builder.compare(
                    SolveCompareOperator::Equal,
                    rhs_tangent,
                    zero,
                    provenance,
                )?;
                let one = self.constant_like(value_type, 1.0, provenance)?;
                let exponent = self.builder.binary(
                    SolveBinaryOperator::Subtract,
                    rhs.primal,
                    one,
                    provenance,
                )?;
                let power = self.builder.binary(
                    SolveBinaryOperator::Power,
                    lhs.primal,
                    exponent,
                    provenance,
                )?;
                let constant = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    rhs.primal,
                    power,
                    provenance,
                )?;
                let constant_safe = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    constant,
                    lhs_tangent,
                    provenance,
                )?;
                let lhs_is_zero = self.builder.compare(
                    SolveCompareOperator::Equal,
                    lhs.primal,
                    zero,
                    provenance,
                )?;
                let rhs_is_one = self.builder.compare(
                    SolveCompareOperator::Equal,
                    rhs.primal,
                    one,
                    provenance,
                )?;
                let lhs_zero_derivative =
                    self.builder
                        .select(rhs_is_one, lhs_tangent, zero, provenance)?;
                let constant = self.builder.select(
                    lhs_is_zero,
                    lhs_zero_derivative,
                    constant_safe,
                    provenance,
                )?;
                let log = self
                    .builder
                    .unary(SolveUnaryOperator::Log, lhs.primal, provenance)?;
                let first = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    rhs_tangent,
                    log,
                    provenance,
                )?;
                let ratio = self.builder.binary(
                    SolveBinaryOperator::Divide,
                    lhs_tangent,
                    lhs.primal,
                    provenance,
                )?;
                let second = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    rhs.primal,
                    ratio,
                    provenance,
                )?;
                let variable =
                    self.builder
                        .binary(SolveBinaryOperator::Add, first, second, provenance)?;
                let variable_safe = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    primal,
                    variable,
                    provenance,
                )?;
                let lhs_positive = self.builder.compare(
                    SolveCompareOperator::Greater,
                    lhs.primal,
                    zero,
                    provenance,
                )?;
                let variable =
                    self.builder
                        .select(lhs_positive, variable_safe, zero, provenance)?;
                self.builder
                    .select(rhs_constant, constant, variable, provenance)?
            }
            SolveBinaryOperator::Atan2 => {
                let first = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    lhs_tangent,
                    rhs.primal,
                    provenance,
                )?;
                let second = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    lhs.primal,
                    rhs_tangent,
                    provenance,
                )?;
                let numerator = self.builder.binary(
                    SolveBinaryOperator::Subtract,
                    first,
                    second,
                    provenance,
                )?;
                let lhs_square = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    lhs.primal,
                    lhs.primal,
                    provenance,
                )?;
                let rhs_square = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    rhs.primal,
                    rhs.primal,
                    provenance,
                )?;
                let denominator = self.builder.binary(
                    SolveBinaryOperator::Add,
                    lhs_square,
                    rhs_square,
                    provenance,
                )?;
                self.builder.binary(
                    SolveBinaryOperator::Divide,
                    numerator,
                    denominator,
                    provenance,
                )?
            }
            SolveBinaryOperator::Min | SolveBinaryOperator::Max => {
                let comparison = if operator == SolveBinaryOperator::Max {
                    SolveCompareOperator::GreaterEqual
                } else {
                    SolveCompareOperator::LessEqual
                };
                let condition = self
                    .builder
                    .compare(comparison, lhs.primal, rhs.primal, provenance)?;
                self.builder
                    .select(condition, lhs_tangent, rhs_tangent, provenance)?
            }
            SolveBinaryOperator::And | SolveBinaryOperator::Or => {
                self.zero(value_type, provenance)?
            }
        };
        Ok(Directional {
            primal,
            tangent: Some(tangent),
        })
    }

    fn expanded_registers(
        &mut self,
        registers: &[SolveRegisterId],
        provenance: Span,
    ) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
        let mut expanded = Vec::new();
        for register in registers {
            let value = self.get(*register, provenance)?;
            expanded.push(value.primal);
            if let Some(tangent) = value.tangent {
                expanded.push(tangent);
            }
        }
        Ok(expanded)
    }

    fn bind_expanded(
        &mut self,
        destinations: &[SolveRegisterId],
        values: &[ProgramRegister<'program>],
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let mut value = 0usize;
        for destination in destinations {
            let tangent = is_real(&self.primal.register_types()[destination.index()]);
            let primal = *values
                .get(value)
                .ok_or(SolveProgramConstructionError::InvalidRegion { provenance })?;
            value += 1;
            let tangent = tangent.then(|| values.get(value).copied()).flatten();
            if tangent.is_some() {
                value += 1;
            }
            self.bind(*destination, Directional { primal, tangent }, provenance)?;
        }
        if value != values.len() {
            return Err(SolveProgramConstructionError::InvalidRegion { provenance });
        }
        Ok(())
    }

    fn lower_conditional(
        &mut self,
        condition: SolveRegisterId,
        captures: &[SolveRegisterId],
        destinations: &[SolveRegisterId],
        if_true: &SolveProgramRegion,
        if_false: &SolveProgramRegion,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let condition = self.get(condition, provenance)?.primal;
        let captures = self.expanded_registers(captures, provenance)?;
        let if_true = derive_region(if_true, self.available)?;
        let if_false = derive_region(if_false, self.available)?;
        let values = self
            .builder
            .conditional_from_regions(condition, &captures, if_true, if_false, provenance)?;
        self.bind_expanded(destinations, &values, provenance)
    }

    fn lower_fold(
        &mut self,
        domain: &StructuredIndexDomain,
        initial: &[SolveRegisterId],
        captures: &[SolveRegisterId],
        destinations: &[SolveRegisterId],
        transition: &SolveProgramRegion,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let initial = self.expanded_registers(initial, provenance)?;
        let captures = self.expanded_registers(captures, provenance)?;
        let transition = derive_region(transition, self.available)?;
        let values = self.builder.fold_from_region(
            domain.clone(),
            &initial,
            &captures,
            transition,
            provenance,
        )?;
        self.bind_expanded(destinations, &values, provenance)
    }

    fn lower_scale(
        &mut self,
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        scalar: SolveRegisterId,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let aggregate = self.get(aggregate, provenance)?;
        let scalar = self.get(scalar, provenance)?;
        let primal = self
            .builder
            .scale(aggregate.primal, scalar.primal, provenance)?;
        let value_type = self.primal.register_types()[destination.index()].clone();
        let aggregate_tangent = self.tangent_or_zero(aggregate, &value_type, provenance)?;
        let scalar_type = SolveValueType::scalar(value_type.element_type());
        let scalar_tangent = self.tangent_or_zero(scalar, &scalar_type, provenance)?;
        let first = self
            .builder
            .scale(aggregate_tangent, scalar.primal, provenance)?;
        let second = self
            .builder
            .scale(aggregate.primal, scalar_tangent, provenance)?;
        let tangent = self
            .builder
            .binary(SolveBinaryOperator::Add, first, second, provenance)?;
        self.bind(
            destination,
            Directional {
                primal,
                tangent: Some(tangent),
            },
            provenance,
        )
    }

    fn lower_broadcast(
        &mut self,
        destination: SolveRegisterId,
        operator: SolveBinaryOperator,
        aggregate: SolveRegisterId,
        scalar: SolveRegisterId,
        scalar_on_lhs: bool,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let aggregate = self.get(aggregate, provenance)?;
        let scalar = self.get(scalar, provenance)?;
        let primal = self.builder.broadcast_binary(
            operator,
            aggregate.primal,
            scalar.primal,
            scalar_on_lhs,
            provenance,
        )?;
        let value_type = self.primal.register_types()[destination.index()].clone();
        let aggregate_tangent = self.tangent_or_zero(aggregate, &value_type, provenance)?;
        let scalar_type = SolveValueType::scalar(value_type.element_type());
        let scalar_tangent = self.tangent_or_zero(scalar, &scalar_type, provenance)?;
        let tangent = match operator {
            SolveBinaryOperator::Add | SolveBinaryOperator::Subtract => {
                self.builder.broadcast_binary(
                    operator,
                    aggregate_tangent,
                    scalar_tangent,
                    scalar_on_lhs,
                    provenance,
                )?
            }
            SolveBinaryOperator::Multiply => {
                let first = self
                    .builder
                    .scale(aggregate_tangent, scalar.primal, provenance)?;
                let second = self
                    .builder
                    .scale(aggregate.primal, scalar_tangent, provenance)?;
                self.builder
                    .binary(SolveBinaryOperator::Add, first, second, provenance)?
            }
            _ => {
                return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
            }
        };
        self.bind(
            destination,
            Directional {
                primal,
                tangent: Some(tangent),
            },
            provenance,
        )
    }

    fn lower_linear_tensor_unary(
        &mut self,
        destination: SolveRegisterId,
        operand: SolveRegisterId,
        provenance: Span,
        operation: impl Fn(
            &mut TypedProgramBuilder<'program>,
            ProgramRegister<'program>,
            Span,
        ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError>,
    ) -> Result<(), SolveProgramConstructionError> {
        let operand = self.get(operand, provenance)?;
        let primal = operation(self.builder, operand.primal, provenance)?;
        let tangent = operand
            .tangent
            .map(|tangent| operation(self.builder, tangent, provenance))
            .transpose()?;
        self.bind(destination, Directional { primal, tangent }, provenance)
    }

    fn lower_matrix_multiply(
        &mut self,
        destination: SolveRegisterId,
        lhs: SolveRegisterId,
        rhs: SolveRegisterId,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let lhs_type = self.primal.register_types()[lhs.index()].clone();
        let rhs_type = self.primal.register_types()[rhs.index()].clone();
        let lhs = self.get(lhs, provenance)?;
        let rhs = self.get(rhs, provenance)?;
        let primal = self
            .builder
            .matrix_multiply(lhs.primal, rhs.primal, provenance)?;
        let lhs_tangent = self.tangent_or_zero(lhs, &lhs_type, provenance)?;
        let rhs_tangent = self.tangent_or_zero(rhs, &rhs_type, provenance)?;
        let first = self
            .builder
            .matrix_multiply(lhs_tangent, rhs.primal, provenance)?;
        let second = self
            .builder
            .matrix_multiply(lhs.primal, rhs_tangent, provenance)?;
        let tangent = self
            .builder
            .binary(SolveBinaryOperator::Add, first, second, provenance)?;
        self.bind(
            destination,
            Directional {
                primal,
                tangent: Some(tangent),
            },
            provenance,
        )
    }

    fn lower_cross(
        &mut self,
        destination: SolveRegisterId,
        lhs: SolveRegisterId,
        rhs: SolveRegisterId,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let lhs = self.get(lhs, provenance)?;
        let rhs = self.get(rhs, provenance)?;
        let primal = self.builder.cross(lhs.primal, rhs.primal, provenance)?;
        let value_type = self.primal.register_types()[destination.index()].clone();
        let lhs_tangent = self.tangent_or_zero(lhs, &value_type, provenance)?;
        let rhs_tangent = self.tangent_or_zero(rhs, &value_type, provenance)?;
        let first = self.builder.cross(lhs_tangent, rhs.primal, provenance)?;
        let second = self.builder.cross(lhs.primal, rhs_tangent, provenance)?;
        let tangent = self
            .builder
            .binary(SolveBinaryOperator::Add, first, second, provenance)?;
        self.bind(
            destination,
            Directional {
                primal,
                tangent: Some(tangent),
            },
            provenance,
        )
    }

    fn lower_reduce(
        &mut self,
        destination: SolveRegisterId,
        operator: SolveReductionOperator,
        operand: SolveRegisterId,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let operand = self.get(operand, provenance)?;
        let primal = self.builder.reduce(operator, operand.primal, provenance)?;
        let tangent = match operator {
            SolveReductionOperator::Sum => operand
                .tangent
                .map(|tangent| self.builder.reduce(operator, tangent, provenance))
                .transpose()?,
            SolveReductionOperator::All => None,
            _ => {
                return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
            }
        };
        self.bind(destination, Directional { primal, tangent }, provenance)
    }

    fn lower_concatenate(
        &mut self,
        destination: SolveRegisterId,
        axis: u32,
        operands: &[SolveRegisterId],
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let source_ids = operands;
        let operands = source_ids
            .iter()
            .map(|operand| self.get(*operand, provenance))
            .collect::<Result<Vec<_>, _>>()?;
        let primal_values = operands
            .iter()
            .map(|operand| operand.primal)
            .collect::<Vec<_>>();
        let primal = self.builder.concatenate(axis, &primal_values, provenance)?;
        let tangent = if is_real(&self.primal.register_types()[destination.index()]) {
            let mut tangents = Vec::with_capacity(operands.len());
            for (operand, source) in operands.into_iter().zip(source_ids.iter()) {
                let value_type = self.primal.register_types()[source.index()].clone();
                tangents.push(self.tangent_or_zero(operand, &value_type, provenance)?);
            }
            Some(self.builder.concatenate(axis, &tangents, provenance)?)
        } else {
            None
        };
        self.bind(destination, Directional { primal, tangent }, provenance)
    }

    fn lower_construct(
        &mut self,
        destination: SolveRegisterId,
        elements: &[SolveRegisterId],
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let elements = elements
            .iter()
            .map(|element| self.get(*element, provenance))
            .collect::<Result<Vec<_>, _>>()?;
        let primals = elements
            .iter()
            .map(|element| element.primal)
            .collect::<Vec<_>>();
        let dimensions = self.primal.register_types()[destination.index()]
            .dimensions()
            .to_vec();
        let primal = self
            .builder
            .construct_aggregate(&primals, dimensions.clone(), provenance)?;
        let tangent = if is_real(&self.primal.register_types()[destination.index()]) {
            let scalar_type = SolveValueType::scalar(
                self.primal.register_types()[destination.index()].element_type(),
            );
            let mut tangents = Vec::with_capacity(elements.len());
            for element in elements {
                tangents.push(self.tangent_or_zero(element, &scalar_type, provenance)?);
            }
            Some(
                self.builder
                    .construct_aggregate(&tangents, dimensions, provenance)?,
            )
        } else {
            None
        };
        self.bind(destination, Directional { primal, tangent }, provenance)
    }

    fn lower_project_element(
        &mut self,
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        indices: Vec<u32>,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let aggregate = self.get(aggregate, provenance)?;
        let primal = self
            .builder
            .project_element(aggregate.primal, indices.clone(), provenance)?;
        let tangent = aggregate
            .tangent
            .map(|tangent| self.builder.project_element(tangent, indices, provenance))
            .transpose()?;
        self.bind(destination, Directional { primal, tangent }, provenance)
    }

    fn index_registers(
        &self,
        indices: &[SolveRegisterId],
        provenance: Span,
    ) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
        indices
            .iter()
            .map(|index| self.get(*index, provenance).map(|value| value.primal))
            .collect()
    }

    fn lower_project_element_dynamic(
        &mut self,
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        indices: &[SolveRegisterId],
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let aggregate = self.get(aggregate, provenance)?;
        let indices = self.index_registers(indices, provenance)?;
        let primal =
            self.builder
                .project_element_dynamic(aggregate.primal, &indices, provenance)?;
        let tangent = aggregate
            .tangent
            .map(|tangent| {
                self.builder
                    .project_element_dynamic(tangent, &indices, provenance)
            })
            .transpose()?;
        self.bind(destination, Directional { primal, tangent }, provenance)
    }

    fn lower_project_slice(
        &mut self,
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        origin: Vec<u32>,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let aggregate = self.get(aggregate, provenance)?;
        let dimensions = self.primal.register_types()[destination.index()]
            .dimensions()
            .to_vec();
        let primal = self.builder.project_slice(
            aggregate.primal,
            origin.clone(),
            dimensions.clone(),
            provenance,
        )?;
        let tangent = aggregate
            .tangent
            .map(|tangent| {
                self.builder
                    .project_slice(tangent, origin, dimensions, provenance)
            })
            .transpose()?;
        self.bind(destination, Directional { primal, tangent }, provenance)
    }

    fn view_axes(
        &self,
        axes: &[SolveTensorViewAxis],
        provenance: Span,
    ) -> Result<Vec<ProgramTensorViewAxis<'program>>, SolveProgramConstructionError> {
        axes.iter()
            .map(|axis| match *axis {
                SolveTensorViewAxis::Index(index) => self
                    .get(index, provenance)
                    .map(|index| ProgramTensorViewAxis::Index(index.primal)),
                SolveTensorViewAxis::Span { origin, extent } => {
                    Ok(ProgramTensorViewAxis::Span { origin, extent })
                }
            })
            .collect()
    }

    fn lower_project_view(
        &mut self,
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        axes: &[SolveTensorViewAxis],
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let aggregate = self.get(aggregate, provenance)?;
        let axes = self.view_axes(axes, provenance)?;
        let primal = self
            .builder
            .project_view(aggregate.primal, &axes, provenance)?;
        let tangent = aggregate
            .tangent
            .map(|tangent| self.builder.project_view(tangent, &axes, provenance))
            .transpose()?;
        self.bind(destination, Directional { primal, tangent }, provenance)
    }

    fn lower_select_element(
        &mut self,
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        indices: &[SolveRegisterId],
        out_of_range: SolveRegisterId,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let aggregate = self.get(aggregate, provenance)?;
        let fallback = self.get(out_of_range, provenance)?;
        let indices = self.index_registers(indices, provenance)?;
        let primal =
            self.builder
                .select_element(aggregate.primal, &indices, fallback.primal, provenance)?;
        let tangent = if let Some(aggregate_tangent) = aggregate.tangent {
            let value_type = self.primal.register_types()[destination.index()].clone();
            let fallback = self.tangent_or_zero(fallback, &value_type, provenance)?;
            Some(
                self.builder
                    .select_element(aggregate_tangent, &indices, fallback, provenance)?,
            )
        } else {
            None
        };
        self.bind(destination, Directional { primal, tangent }, provenance)
    }

    fn lower_update_element(
        &mut self,
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        value: SolveRegisterId,
        indices: &[SolveRegisterId],
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let aggregate = self.get(aggregate, provenance)?;
        let value = self.get(value, provenance)?;
        let indices = self.index_registers(indices, provenance)?;
        let primal =
            self.builder
                .update_element(aggregate.primal, value.primal, &indices, provenance)?;
        let tangent = if is_real(&self.primal.register_types()[destination.index()]) {
            let aggregate_type = self.primal.register_types()[destination.index()].clone();
            let value_type = SolveValueType::scalar(aggregate_type.element_type());
            let aggregate = self.tangent_or_zero(aggregate, &aggregate_type, provenance)?;
            let value = self.tangent_or_zero(value, &value_type, provenance)?;
            Some(
                self.builder
                    .update_element(aggregate, value, &indices, provenance)?,
            )
        } else {
            None
        };
        self.bind(destination, Directional { primal, tangent }, provenance)
    }

    fn lower_update_slice(
        &mut self,
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        value: SolveRegisterId,
        origin: Vec<u32>,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let value_type = self.primal.register_types()[value.index()].clone();
        let aggregate = self.get(aggregate, provenance)?;
        let value = self.get(value, provenance)?;
        let primal = self.builder.update_slice(
            aggregate.primal,
            value.primal,
            origin.clone(),
            provenance,
        )?;
        let tangent = if is_real(&self.primal.register_types()[destination.index()]) {
            let aggregate_type = self.primal.register_types()[destination.index()].clone();
            let aggregate = self.tangent_or_zero(aggregate, &aggregate_type, provenance)?;
            let value = self.tangent_or_zero(value, &value_type, provenance)?;
            Some(
                self.builder
                    .update_slice(aggregate, value, origin, provenance)?,
            )
        } else {
            None
        };
        self.bind(destination, Directional { primal, tangent }, provenance)
    }

    fn lower_update_view(
        &mut self,
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        value: SolveRegisterId,
        axes: &[SolveTensorViewAxis],
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let value_type = self.primal.register_types()[value.index()].clone();
        let aggregate = self.get(aggregate, provenance)?;
        let value = self.get(value, provenance)?;
        let axes = self.view_axes(axes, provenance)?;
        let primal = self
            .builder
            .update_view(aggregate.primal, value.primal, &axes, provenance)?;
        let tangent = if is_real(&self.primal.register_types()[destination.index()]) {
            let aggregate_type = self.primal.register_types()[destination.index()].clone();
            let aggregate = self.tangent_or_zero(aggregate, &aggregate_type, provenance)?;
            let value = self.tangent_or_zero(value, &value_type, provenance)?;
            Some(
                self.builder
                    .update_view(aggregate, value, &axes, provenance)?,
            )
        } else {
            None
        };
        self.bind(destination, Directional { primal, tangent }, provenance)
    }

    fn lower_call(
        &mut self,
        owner: SolvePureCallOwnerId,
        arguments: &[SolveRegisterId],
        destinations: &[SolveRegisterId],
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        if self
            .available
            .get(owner.index() as usize)
            .is_none_or(Option::is_none)
        {
            return Err(SolveProgramConstructionError::UnknownCallOwner { provenance });
        }
        let arguments = self.expanded_registers(arguments, provenance)?;
        let values = self.builder.call(owner, &arguments, provenance)?;
        self.bind_expanded(destinations, &values, provenance)
    }
}

fn derive_region(
    primal: &SolveProgramRegion,
    available: &[Option<SolvePureCallDirectionalInterface>],
) -> Result<SolveProgramRegion, SolveProgramConstructionError> {
    let inputs = expand_types(primal.inputs());
    let outputs = expand_types(primal.outputs());
    let body = derive_program(
        primal.body(),
        primal.inputs().len(),
        primal.outputs().len(),
        available,
    )?;
    construct_region(inputs, outputs, body, primal.provenance())
}
