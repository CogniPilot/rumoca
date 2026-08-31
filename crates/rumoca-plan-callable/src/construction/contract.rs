use rumoca_core::{OperationContractKey, Span};
use rumoca_ir_dae::{DaeView, ExprId, ExpressionOperation, ScalarType};

use super::{PendingOwner, PlanConstructionError};
use crate::model::{
    CallableScalarType, CallableValueType, CompactBinder, CompactDomain, OwnerCallOccurrence,
};

pub(super) fn checked_id(index: usize, span: Span) -> Result<u32, PlanConstructionError> {
    u32::try_from(index).map_err(|_| PlanConstructionError::IdentityOverflow { span })
}

pub(super) fn authenticate_contract<'dae>(
    dae: DaeView<'dae>,
    expression: rumoca_ir_dae::ExpressionView<'dae>,
) -> Result<OperationContractKey, PlanConstructionError> {
    let span = expression.provenance().span();
    let real_result = expression.value_type().scalar_type() == ScalarType::Real;
    let contract = match expression.operation() {
        ExpressionOperation::Literal(
            rumoca_ir_dae::DaeLiteral::Boolean(_)
            | rumoca_ir_dae::DaeLiteral::Integer(_)
            | rumoca_ir_dae::DaeLiteral::Real(_),
        ) => OperationContractKey::Constant,
        ExpressionOperation::Coordinate(
            rumoca_ir_dae::CoordinateView::FunctionParameter(_)
            | rumoca_ir_dae::CoordinateView::Binder(_),
        )
        | ExpressionOperation::FunctionValue { .. }
        | ExpressionOperation::FunctionFoldParameter { .. }
        | ExpressionOperation::FunctionFoldOutput { .. } => OperationContractKey::Load,
        ExpressionOperation::Unary {
            operator: rumoca_ir_dae::UnaryOperator::Negate,
            ..
        } if real_result => OperationContractKey::UnaryNegateReal,
        ExpressionOperation::Binary { operator, lhs, rhs } => {
            authenticate_binary_contract(dae, operator, lhs, rhs, span)?
        }
        ExpressionOperation::Conditional(_) => OperationContractKey::Conditional,
        ExpressionOperation::Array(_) => OperationContractKey::ConstructAggregate,
        ExpressionOperation::Comprehension { .. } => OperationContractKey::Map,
        ExpressionOperation::Index { subscripts, .. } => {
            if subscripts
                .iter()
                .any(|subscript| !matches!(subscript, rumoca_ir_dae::SubscriptView::Index { .. }))
            {
                OperationContractKey::ProjectSlice
            } else {
                OperationContractKey::ProjectElement
            }
        }
        ExpressionOperation::ArrayUpdate { subscripts, .. }
            if subscripts.iter().all(|subscript| {
                matches!(subscript, rumoca_ir_dae::SubscriptView::Index { .. })
            }) =>
        {
            OperationContractKey::UpdateElement
        }
        ExpressionOperation::Builtin { builtin, .. } => match builtin {
            rumoca_ir_dae::PureBuiltin::Sqrt => OperationContractKey::UnarySqrtReal,
            rumoca_ir_dae::PureBuiltin::Sin => OperationContractKey::UnarySinReal,
            rumoca_ir_dae::PureBuiltin::Cos => OperationContractKey::UnaryCosReal,
            rumoca_ir_dae::PureBuiltin::Identity if real_result => {
                OperationContractKey::IdentityReal
            }
            rumoca_ir_dae::PureBuiltin::Identity => OperationContractKey::IdentityInteger,
            _ => return Err(PlanConstructionError::InvalidOperation { span }),
        },
        _ => return Err(PlanConstructionError::InvalidOperation { span }),
    };
    Ok(contract)
}

fn authenticate_binary_contract<'dae>(
    dae: DaeView<'dae>,
    operator: rumoca_ir_dae::BinaryOperator,
    lhs: ExprId<'dae>,
    rhs: ExprId<'dae>,
    span: Span,
) -> Result<OperationContractKey, PlanConstructionError> {
    let lhs = dae
        .expression(lhs)
        .ok_or(PlanConstructionError::InvalidSourceOccurrence { span })?
        .value_type();
    let rhs = dae
        .expression(rhs)
        .ok_or(PlanConstructionError::InvalidSourceOccurrence { span })?
        .value_type();
    if operator != rumoca_ir_dae::BinaryOperator::Multiply && lhs.dimensions() != rhs.dimensions() {
        return Err(PlanConstructionError::InvalidOperation { span });
    }
    let real = lhs.scalar_type() == ScalarType::Real || rhs.scalar_type() == ScalarType::Real;
    let contract = match operator {
        rumoca_ir_dae::BinaryOperator::Multiply if lhs.is_scalar() && !rhs.is_scalar() => {
            if real {
                OperationContractKey::ScaleReal
            } else {
                OperationContractKey::ScaleInteger
            }
        }
        rumoca_ir_dae::BinaryOperator::Multiply if !lhs.is_scalar() && rhs.is_scalar() => {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        rumoca_ir_dae::BinaryOperator::Multiply if !lhs.is_scalar() && !rhs.is_scalar() && real => {
            OperationContractKey::MatrixMultiplyReal
        }
        rumoca_ir_dae::BinaryOperator::Multiply if !lhs.is_scalar() && !rhs.is_scalar() => {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        rumoca_ir_dae::BinaryOperator::Add if real => OperationContractKey::BinaryAddReal,
        rumoca_ir_dae::BinaryOperator::Add => OperationContractKey::BinaryAddInteger,
        rumoca_ir_dae::BinaryOperator::Subtract if real => OperationContractKey::BinarySubtractReal,
        rumoca_ir_dae::BinaryOperator::Subtract => OperationContractKey::BinarySubtractInteger,
        rumoca_ir_dae::BinaryOperator::Multiply if real => OperationContractKey::BinaryMultiplyReal,
        rumoca_ir_dae::BinaryOperator::Multiply => OperationContractKey::BinaryMultiplyInteger,
        rumoca_ir_dae::BinaryOperator::Divide => OperationContractKey::BinaryDivideReal,
        rumoca_ir_dae::BinaryOperator::Power if real => OperationContractKey::BinaryPowerReal,
        rumoca_ir_dae::BinaryOperator::Power => {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        rumoca_ir_dae::BinaryOperator::Less if real => OperationContractKey::CompareLessReal,
        rumoca_ir_dae::BinaryOperator::Less => OperationContractKey::CompareLessInteger,
        _ => return Err(PlanConstructionError::InvalidOperation { span }),
    };
    Ok(contract)
}

/// Order every owner so each callee precedes its callers, and refuse the whole
/// plan when a call occurrence closes a cycle.
///
/// The refusal cites the exact call that closes the cycle, because that call
/// is the unsupported occurrence. A caller declaration on the cycle is not the
/// responsible source location and is not reported as one.
pub(super) fn topological_owner_order(
    owners: &[PendingOwner],
    call_occurrences: &[OwnerCallOccurrence],
) -> Result<Box<[u32]>, PlanConstructionError> {
    fn visit(
        owner: usize,
        owners: &[PendingOwner],
        outgoing: &[Vec<OwnerCallOccurrence>],
        states: &mut [u8],
        order: &mut Vec<u32>,
    ) -> Result<(), PlanConstructionError> {
        states[owner] = 1;
        for occurrence in &outgoing[owner] {
            let callee = occurrence.callee as usize;
            match states[callee] {
                2 => continue,
                1 => {
                    return Err(PlanConstructionError::UnsupportedRecursion {
                        span: occurrence.span,
                    });
                }
                _ => {}
            }
            visit(callee, owners, outgoing, states, order)?;
        }
        states[owner] = 2;
        order.push(checked_id(owner, owners[owner].declaration.span())?);
        Ok(())
    }

    let mut outgoing = vec![Vec::new(); owners.len()];
    for occurrence in call_occurrences {
        let caller = occurrence.caller as usize;
        if caller >= owners.len() || occurrence.callee as usize >= owners.len() {
            return Err(PlanConstructionError::InvalidOwner {
                span: occurrence.span,
            });
        }
        outgoing[caller].push(*occurrence);
    }

    let mut states = vec![0; owners.len()];
    let mut order = Vec::with_capacity(owners.len());
    for owner in 0..owners.len() {
        if states[owner] == 0 {
            visit(owner, owners, &outgoing, &mut states, &mut order)?;
        }
    }
    Ok(order.into_boxed_slice())
}

pub(super) fn callable_value_type<'dae>(
    dae: DaeView<'dae>,
    id: rumoca_ir_dae::ValueTypeId<'dae>,
    span: Span,
) -> Result<CallableValueType, PlanConstructionError> {
    let value_type = dae
        .value_type(id)
        .ok_or(PlanConstructionError::InvalidSourceOccurrence { span })?;
    let scalar = match value_type.scalar_type() {
        ScalarType::Boolean => CallableScalarType::Boolean,
        ScalarType::Integer => CallableScalarType::Integer,
        ScalarType::Real => CallableScalarType::Real,
        ScalarType::Enumeration | ScalarType::String | ScalarType::Record => {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
    };
    Ok(CallableValueType {
        scalar,
        dimensions: value_type.dimensions().into(),
    })
}

pub(super) fn expected_operands(
    operation: ExpressionOperation<'_>,
    contract: OperationContractKey,
) -> Box<dyn Iterator<Item = ExprId<'_>> + '_> {
    if matches!(
        contract,
        OperationContractKey::IdentityInteger | OperationContractKey::IdentityReal
    ) {
        return Box::new(std::iter::empty());
    }
    match operation {
        ExpressionOperation::Literal(_)
        | ExpressionOperation::Coordinate(_)
        | ExpressionOperation::Range(_)
        | ExpressionOperation::FunctionValue { .. }
        | ExpressionOperation::FunctionFoldParameter { .. }
        | ExpressionOperation::FunctionFoldOutput { .. } => Box::new(std::iter::empty()),
        ExpressionOperation::Unary { operand, .. } => Box::new(std::iter::once(operand)),
        ExpressionOperation::Binary { lhs, rhs, .. } => Box::new([lhs, rhs].into_iter()),
        ExpressionOperation::Conditional(values)
        | ExpressionOperation::Array(values)
        | ExpressionOperation::Record(values) => Box::new(values.iter()),
        ExpressionOperation::Field { base, .. } | ExpressionOperation::Index { base, .. } => {
            Box::new(std::iter::once(base))
        }
        ExpressionOperation::Comprehension { body, .. } => Box::new(std::iter::once(body)),
        ExpressionOperation::ArrayUpdate { base, value, .. } => Box::new([base, value].into_iter()),
        ExpressionOperation::Builtin { arguments, .. }
        | ExpressionOperation::Call { arguments, .. } => Box::new(arguments.iter()),
        ExpressionOperation::StringConversion { value, .. }
        | ExpressionOperation::ClockTransfer { source: value, .. } => {
            Box::new(std::iter::once(value))
        }
    }
}

pub(super) fn direct_value_operands(
    operation: ExpressionOperation<'_>,
) -> Box<dyn Iterator<Item = ExprId<'_>> + '_> {
    match operation {
        ExpressionOperation::Literal(_)
        | ExpressionOperation::Coordinate(_)
        | ExpressionOperation::Range(_)
        | ExpressionOperation::FunctionValue { .. }
        | ExpressionOperation::FunctionFoldParameter { .. }
        | ExpressionOperation::FunctionFoldOutput { .. } => Box::new(std::iter::empty()),
        ExpressionOperation::Unary { operand, .. } => Box::new(std::iter::once(operand)),
        ExpressionOperation::Binary { lhs, rhs, .. } => Box::new([lhs, rhs].into_iter()),
        ExpressionOperation::Conditional(values)
        | ExpressionOperation::Array(values)
        | ExpressionOperation::Record(values) => Box::new(values.iter()),
        ExpressionOperation::Field { base, .. } | ExpressionOperation::Index { base, .. } => {
            Box::new(std::iter::once(base))
        }
        ExpressionOperation::Comprehension { body, .. } => Box::new(std::iter::once(body)),
        ExpressionOperation::ArrayUpdate { base, value, .. } => Box::new([base, value].into_iter()),
        ExpressionOperation::Builtin {
            builtin: rumoca_ir_dae::PureBuiltin::Identity,
            arguments: _,
        } => Box::new(std::iter::empty()),
        ExpressionOperation::Builtin { arguments, .. }
        | ExpressionOperation::Call { arguments, .. } => Box::new(arguments.iter()),
        ExpressionOperation::StringConversion { value, .. }
        | ExpressionOperation::ClockTransfer { source: value, .. } => {
            Box::new(std::iter::once(value))
        }
    }
}

pub(super) fn conversion_target_type<'dae>(
    dae: DaeView<'dae>,
    consumer: rumoca_ir_dae::ExpressionView<'dae>,
    ordinal: usize,
    span: Span,
) -> Result<&'dae rumoca_ir_dae::ValueType, PlanConstructionError> {
    match consumer.operation() {
        ExpressionOperation::Call {
            function,
            arguments,
            ..
        } => {
            if ordinal >= arguments.len() {
                return Err(PlanConstructionError::InvalidOperation { span });
            }
            let parameter = dae
                .exact_function(function)
                .parameter_types()
                .get(ordinal)
                .ok_or(PlanConstructionError::InvalidOperation { span })?;
            dae.value_type(parameter)
                .ok_or(PlanConstructionError::InvalidOperation { span })
        }
        ExpressionOperation::Conditional(values)
            if ordinal.is_multiple_of(2) && ordinal + 1 < values.len() =>
        {
            let operand = values
                .get(ordinal)
                .ok_or(PlanConstructionError::InvalidOperation { span })?;
            Ok(dae.exact_expression(operand).value_type())
        }
        ExpressionOperation::Index { base, .. } | ExpressionOperation::Field { base, .. } => {
            if ordinal != 0 {
                return Err(PlanConstructionError::InvalidOperation { span });
            }
            Ok(dae.exact_expression(base).value_type())
        }
        _ => Ok(consumer.value_type()),
    }
}

pub(super) fn expected_operand_value_type<'dae>(
    dae: DaeView<'dae>,
    consumer: rumoca_ir_dae::ExpressionView<'dae>,
    ordinal: usize,
    operand: ExprId<'dae>,
    span: Span,
) -> Result<CallableValueType, PlanConstructionError> {
    let operand_view = dae.exact_expression(operand);
    let mut expected = callable_value_type(dae, operand_view.value_type_id(), span)?;
    let target_scalar = match consumer.operation() {
        ExpressionOperation::Call { function, .. } => {
            let parameter = dae
                .exact_function(function)
                .parameter_types()
                .get(ordinal)
                .ok_or(PlanConstructionError::InvalidOperation { span })?;
            return callable_value_type(dae, parameter, span);
        }
        ExpressionOperation::Unary { .. } => consumer.value_type().scalar_type(),
        ExpressionOperation::Binary { operator, lhs, rhs } => {
            let lhs = dae.exact_expression(lhs).value_type().scalar_type();
            let rhs = dae.exact_expression(rhs).value_type().scalar_type();
            if operator == rumoca_ir_dae::BinaryOperator::Divide
                || lhs == ScalarType::Real
                || rhs == ScalarType::Real
            {
                ScalarType::Real
            } else {
                operand_view.value_type().scalar_type()
            }
        }
        ExpressionOperation::Conditional(values) => {
            if ordinal.is_multiple_of(2) && ordinal + 1 < values.len() {
                operand_view.value_type().scalar_type()
            } else {
                consumer.value_type().scalar_type()
            }
        }
        ExpressionOperation::Array(_) => consumer.value_type().scalar_type(),
        ExpressionOperation::ArrayUpdate { .. } if ordinal != 0 => {
            consumer.value_type().scalar_type()
        }
        ExpressionOperation::Builtin { .. } => consumer.value_type().scalar_type(),
        _ => operand_view.value_type().scalar_type(),
    };
    expected.scalar = callable_scalar_type(target_scalar, span)?;
    Ok(expected)
}

fn callable_scalar_type(
    scalar: ScalarType,
    span: Span,
) -> Result<CallableScalarType, PlanConstructionError> {
    match scalar {
        ScalarType::Boolean => Ok(CallableScalarType::Boolean),
        ScalarType::Integer => Ok(CallableScalarType::Integer),
        ScalarType::Real => Ok(CallableScalarType::Real),
        ScalarType::Enumeration | ScalarType::String | ScalarType::Record => {
            Err(PlanConstructionError::InvalidOperation { span })
        }
    }
}

pub(super) fn compact_domain<'dae>(
    dae: DaeView<'dae>,
    id: rumoca_ir_dae::DomainId<'dae>,
    span: Span,
) -> Result<CompactDomain, PlanConstructionError> {
    let domain = dae
        .domain(id)
        .ok_or(PlanConstructionError::InvalidSourceOccurrence { span })?;
    Ok(CompactDomain {
        binders: domain
            .structured()
            .binders
            .iter()
            .map(|binder| CompactBinder {
                lower: binder.lower,
                upper: binder.upper,
                step: binder.step,
            })
            .collect(),
        scalar_count: domain.scalar_count(),
    })
}
