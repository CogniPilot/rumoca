//! Bounded exact-operation fact for scalar-first, body-angular Hamilton kinematics.
mod expected;
use super::{SolvePureCallOutput, SolvePureCallOutputKind};
use crate::{
    SolveBinaryOperator, SolveOperation, SolveRealFormat, SolveScalarType, SolveStorageClass,
    SolveUnaryOperator, SolveValueKind, SolveValueType, TypedProgram,
};
use expected::expected;

/// Source relation only. Unit length, basis conversion and numerical-domain
/// permission are separate. The original call must still execute completely.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BodyQuaternionKinematics(());

#[derive(Clone, Debug, PartialEq, Eq)]
enum Token {
    Input(u8, u8),
    Real(u64),
    Neg,
    Add,
    Sub,
    Mul,
}
#[derive(Clone)]
enum Value {
    Real(Vec<Token>),
    Array(Vec<Vec<Token>>),
    Integer(i64),
}

pub(super) fn derive(
    body: &TypedProgram,
    inputs: &[SolveValueType],
    outputs: &[SolvePureCallOutput],
) -> Option<BodyQuaternionKinematics> {
    if inputs.len() != 2
        || outputs.len() != 1
        || !real_shape(&inputs[0], &[4])
        || !real_shape(&inputs[1], &[3])
        || outputs[0].kind() != SolvePureCallOutputKind::Result
        || !real_shape(outputs[0].value_type(), &[4])
        || body.operations().len() > 128
    {
        return None;
    }
    let mut values = vec![None; body.register_types().len()];
    let mut result = None;
    for spanned in body.operations() {
        apply(spanned.operation(), body, &mut values, &mut result)?;
    }
    (result? == expected()).then_some(BodyQuaternionKinematics(()))
}

fn real_shape(ty: &SolveValueType, shape: &[u32]) -> bool {
    ty.element_type()
        == SolveScalarType::Real {
            format: SolveRealFormat::Binary64,
        }
        && ty.dimensions() == shape
}

fn apply(
    op: &SolveOperation,
    body: &TypedProgram,
    values: &mut [Option<Value>],
    result: &mut Option<Vec<Vec<Token>>>,
) -> Option<()> {
    use SolveOperation as O;
    let (destination, value) = match op {
        O::Load { destination, slot } => {
            let input = u8::try_from(slot.index()).ok()?;
            let slot = body.slots().get(slot.index())?;
            if slot.storage() != SolveStorageClass::Input || input > 1 {
                return None;
            }
            let count = slot.value_type().scalar_count();
            (
                *destination,
                Value::Array(
                    (0..count)
                        .map(|i| vec![Token::Input(input, i as u8)])
                        .collect(),
                ),
            )
        }
        O::Constant { destination, value } => (
            *destination,
            match value.kind() {
                SolveValueKind::Real64(bits) => Value::Real(vec![Token::Real(bits)]),
                SolveValueKind::Integer(value) => Value::Integer(value),
                _ => return None,
            },
        ),
        O::Store { slot, source } => {
            if body.slots().get(slot.index())?.storage() != SolveStorageClass::Output
                || result.is_some()
            {
                return None;
            }
            let Value::Array(array) = values.get(source.index())?.as_ref()? else {
                return None;
            };
            *result = Some(array.clone());
            return Some(());
        }
        O::CheckFinite { .. } => return Some(()),
        _ => expression(op, body, values)?,
    };
    *values.get_mut(destination.index())? = Some(value);
    Some(())
}

fn expression(
    op: &SolveOperation,
    body: &TypedProgram,
    values: &[Option<Value>],
) -> Option<(crate::SolveRegisterId, Value)> {
    use SolveOperation as O;
    match op {
        O::ProjectElement {
            destination,
            aggregate,
            indices,
        } if indices.len() == 1 => {
            let Value::Array(array) = values.get(aggregate.index())?.as_ref()? else {
                return None;
            };
            Some((
                *destination,
                Value::Real(array.get(indices[0] as usize)?.clone()),
            ))
        }
        O::Unary {
            destination,
            operator: SolveUnaryOperator::Negate,
            operand,
        } => {
            let mut tokens = real(values, operand.index())?.to_vec();
            tokens.push(Token::Neg);
            Some((*destination, Value::Real(tokens)))
        }
        O::Binary {
            destination,
            operator,
            lhs,
            rhs,
        } => {
            let token = match operator {
                SolveBinaryOperator::Add => Token::Add,
                SolveBinaryOperator::Subtract => Token::Sub,
                SolveBinaryOperator::Multiply => Token::Mul,
                _ => return None,
            };
            let mut tokens = real(values, lhs.index())?.to_vec();
            tokens.extend_from_slice(real(values, rhs.index())?);
            tokens.push(token);
            (tokens.len() <= 32).then_some((*destination, Value::Real(tokens)))
        }
        _ => aggregate(op, body, values),
    }
}

fn aggregate(
    op: &SolveOperation,
    body: &TypedProgram,
    values: &[Option<Value>],
) -> Option<(crate::SolveRegisterId, Value)> {
    use SolveOperation as O;
    match op {
        O::Fill { destination, value } => {
            let ty = body.register_types().get(destination.index())?;
            let count = ty.scalar_count() as usize;
            if count > 4 || ty.dimensions().len() != 1 {
                return None;
            }
            Some((
                *destination,
                Value::Array(vec![real(values, value.index())?.to_vec(); count]),
            ))
        }
        O::ConstructAggregate {
            destination,
            elements,
        } if elements.len() <= 4 => {
            let array = elements
                .iter()
                .map(|r| Some(real(values, r.index())?.to_vec()))
                .collect::<Option<Vec<_>>>()?;
            Some((*destination, Value::Array(array)))
        }
        O::UpdateElement {
            destination,
            aggregate,
            value,
            indices,
        } if indices.len() == 1 => {
            let Value::Array(array) = values.get(aggregate.index())?.as_ref()? else {
                return None;
            };
            let Value::Integer(index) = values.get(indices[0].index())?.as_ref()? else {
                return None;
            };
            let index = usize::try_from(index.checked_sub(1)?).ok()?;
            let mut array = array.clone();
            *array.get_mut(index)? = real(values, value.index())?.to_vec();
            Some((*destination, Value::Array(array)))
        }
        _ => None,
    }
}

fn real(values: &[Option<Value>], index: usize) -> Option<&[Token]> {
    match values.get(index)?.as_ref()? {
        Value::Real(tokens) => Some(tokens),
        _ => None,
    }
}
