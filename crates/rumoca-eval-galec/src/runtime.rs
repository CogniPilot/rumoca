use std::collections::{BTreeMap, BTreeSet};

use rumoca_ir_galec::ast;

use crate::interpreter::{EvaluationError, Evaluator};
use crate::{IntegerDomain, Value};

pub(super) fn flatten_dimension(
    elements: Vec<&Value>,
    expected: Option<usize>,
) -> Option<Vec<&Value>> {
    let mut nested = Vec::new();
    for element in elements {
        let Value::Array(values) = element else {
            return None;
        };
        if expected.is_some_and(|expected| values.len() != expected) || values.is_empty() {
            return None;
        }
        nested.extend(values);
    }
    Some(nested)
}

pub(super) fn record_field(value: Value, name: &str) -> Result<Value, EvaluationError> {
    let Value::Record(fields) = value else {
        return Err(EvaluationError::Type("record component reference"));
    };
    fields
        .get(name)
        .cloned()
        .ok_or_else(|| EvaluationError::UnknownName(name.to_owned()))
}

pub(super) fn declarations(
    block: &ast::Block,
    values: &[ast::VariableDeclaration],
) -> Result<BTreeMap<String, Value>, EvaluationError> {
    values
        .iter()
        .map(|declaration| {
            Ok((
                declaration.name.lexeme().to_owned(),
                uninitialized_declaration(block, declaration)?,
            ))
        })
        .collect()
}

pub(super) fn uninitialized_declaration(
    block: &ast::Block,
    declaration: &ast::VariableDeclaration,
) -> Result<Value, EvaluationError> {
    uninitialized_declaration_inner(block, declaration, &mut BTreeSet::new())
}

fn uninitialized_declaration_inner(
    block: &ast::Block,
    declaration: &ast::VariableDeclaration,
    compartment_stack: &mut BTreeSet<String>,
) -> Result<Value, EvaluationError> {
    let scalar = match &declaration.ty {
        ast::TypeRef::Primitive(_) => Value::Uninitialized,
        ast::TypeRef::Compartment(name) => {
            if !compartment_stack.insert(name.lexeme().to_owned()) {
                return Err(EvaluationError::MalformedCheckedBlock(format!(
                    "recursive compartment `{}`",
                    name.lexeme()
                )));
            }
            let compartment = block
                .compartments
                .iter()
                .find(|compartment| compartment.name.lexeme() == name.lexeme())
                .ok_or_else(|| {
                    EvaluationError::MalformedCheckedBlock(format!(
                        "unknown compartment `{}`",
                        name.lexeme()
                    ))
                })?;
            let fields = compartment
                .entities
                .iter()
                .map(|entity| {
                    Ok((
                        entity.decl.name.lexeme().to_owned(),
                        uninitialized_declaration_inner(block, &entity.decl, compartment_stack)?,
                    ))
                })
                .collect::<Result<BTreeMap<_, _>, EvaluationError>>()?;
            compartment_stack.remove(name.lexeme());
            Value::Record(fields)
        }
    };
    declaration
        .dimensions
        .iter()
        .rev()
        .try_fold(scalar, |value, dimension| {
            let ast::Dimension::Expr(ast::Expression::Integer(size)) = dimension else {
                return Err(EvaluationError::MalformedCheckedBlock(format!(
                    "non-literal runtime dimension on `{}`",
                    declaration.name.lexeme()
                )));
            };
            let size = usize::try_from(*size).map_err(|_| {
                EvaluationError::MalformedCheckedBlock(format!(
                    "invalid runtime dimension {size} on `{}`",
                    declaration.name.lexeme()
                ))
            })?;
            if size == 0 {
                return Err(EvaluationError::MalformedCheckedBlock(format!(
                    "zero runtime dimension on `{}`",
                    declaration.name.lexeme()
                )));
            }
            Ok(Value::Array(vec![value; size]))
        })
}

pub(super) fn value_matches_declaration(
    block: &ast::Block,
    declaration: &ast::VariableDeclaration,
    value: &Value,
) -> Result<bool, EvaluationError> {
    let mut element = value;
    for dimension in &declaration.dimensions {
        let ast::Dimension::Expr(ast::Expression::Integer(size)) = dimension else {
            return Err(EvaluationError::MalformedCheckedBlock(format!(
                "non-literal block dimension on `{}`",
                declaration.name.lexeme()
            )));
        };
        let expected = usize::try_from(*size).map_err(|_| {
            EvaluationError::MalformedCheckedBlock(format!(
                "invalid block dimension {size} on `{}`",
                declaration.name.lexeme()
            ))
        })?;
        let Value::Array(values) = element else {
            return Ok(false);
        };
        if values.len() != expected {
            return Ok(false);
        }
        let Some(first) = values.first() else {
            return Ok(false);
        };
        if !values.iter().all(|candidate| same_shape(first, candidate)) {
            return Ok(false);
        }
        element = first;
    }
    value_matches_type(block, &declaration.ty, element)
}

pub(super) fn value_matches_type(
    block: &ast::Block,
    ty: &ast::TypeRef,
    value: &Value,
) -> Result<bool, EvaluationError> {
    match (ty, value) {
        (ast::TypeRef::Primitive(ast::ScalarType::Boolean), Value::Boolean(_))
        | (ast::TypeRef::Primitive(ast::ScalarType::Integer), Value::Integer(_))
        | (ast::TypeRef::Primitive(ast::ScalarType::Real), Value::Real(_)) => Ok(true),
        (ast::TypeRef::Compartment(name), Value::Record(fields)) => {
            let compartment = block
                .compartments
                .iter()
                .find(|candidate| candidate.name.lexeme() == name.lexeme())
                .ok_or_else(|| {
                    EvaluationError::MalformedCheckedBlock(format!(
                        "unknown compartment `{}`",
                        name.lexeme()
                    ))
                })?;
            if fields.len() != compartment.entities.len() {
                return Ok(false);
            }
            for entity in &compartment.entities {
                let Some(field) = fields.get(entity.decl.name.lexeme()) else {
                    return Ok(false);
                };
                if !value_matches_declaration(block, &entity.decl, field)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        _ => Ok(false),
    }
}

fn same_shape(lhs: &Value, rhs: &Value) -> bool {
    match (lhs, rhs) {
        (Value::Array(lhs), Value::Array(rhs)) => {
            lhs.len() == rhs.len() && lhs.iter().zip(rhs).all(|(lhs, rhs)| same_shape(lhs, rhs))
        }
        (Value::Record(lhs), Value::Record(rhs)) => {
            lhs.len() == rhs.len()
                && lhs
                    .iter()
                    .all(|(name, lhs)| rhs.get(name).is_some_and(|rhs| same_shape(lhs, rhs)))
        }
        (Value::Boolean(_), Value::Boolean(_))
        | (Value::Integer(_), Value::Integer(_))
        | (Value::Real(_), Value::Real(_)) => true,
        _ => false,
    }
}

pub(super) fn require_arity(
    name: &str,
    expected: usize,
    found: usize,
) -> Result<(), EvaluationError> {
    if expected == found {
        return Ok(());
    }
    Err(EvaluationError::InputArity {
        name: name.to_owned(),
        expected,
        found,
    })
}

pub(super) fn not_value(value: Value) -> Result<Value, EvaluationError> {
    match value {
        Value::Boolean(value) => Ok(Value::Boolean(!value)),
        Value::Array(values) => values
            .into_iter()
            .map(not_value)
            .collect::<Result<Vec<_>, _>>()
            .map(Value::Array),
        Value::Integer(_) | Value::Real(_) | Value::Record(_) | Value::Uninitialized => {
            Err(EvaluationError::Type("not"))
        }
    }
}

pub(super) fn value_in_integer_domain(value: &Value, domain: IntegerDomain) -> bool {
    match value {
        Value::Integer(value) => domain.contains(*value),
        Value::Array(values) => values
            .iter()
            .all(|value| value_in_integer_domain(value, domain)),
        Value::Record(fields) => fields
            .values()
            .all(|value| value_in_integer_domain(value, domain)),
        Value::Boolean(_) | Value::Real(_) => true,
        Value::Uninitialized => false,
    }
}

pub(super) fn compare_ordered<T: PartialOrd + PartialEq>(
    op: ast::BinaryOp,
    a: T,
    b: T,
) -> Result<bool, EvaluationError> {
    Ok(match op {
        ast::BinaryOp::Lt => a < b,
        ast::BinaryOp::Gt => a > b,
        ast::BinaryOp::Le => a <= b,
        ast::BinaryOp::Ge => a >= b,
        ast::BinaryOp::Eq => a == b,
        ast::BinaryOp::Ne => a != b,
        _ => {
            return Err(EvaluationError::MalformedCheckedBlock(
                "non-comparison operator reached comparison evaluator".to_owned(),
            ));
        }
    })
}

pub(super) fn size_dimension(value: &Value, dimension: i64) -> Result<Value, EvaluationError> {
    if dimension < 1 {
        return Err(EvaluationError::Bounds {
            index: dimension,
            length: 0,
        });
    }
    let mut current = value;
    for _ in 1..dimension {
        let Value::Array(values) = current else {
            return Err(EvaluationError::Type("size array"));
        };
        current = values
            .first()
            .ok_or(EvaluationError::Type("empty checked array"))?;
    }
    let Value::Array(values) = current else {
        return Err(EvaluationError::Type("size array"));
    };
    Ok(Value::Integer(i64::try_from(values.len()).map_err(
        |_| EvaluationError::MalformedCheckedBlock("array length exceeds GALEC Integer".to_owned()),
    )?))
}

pub(super) fn indices(
    subscripts: &[ast::Expression],
    evaluator: &Evaluator<'_>,
    locals: &BTreeMap<String, Value>,
) -> Result<Vec<usize>, EvaluationError> {
    let mut shadow = Evaluator {
        block: evaluator.block,
        state: evaluator.state.clone(),
        active_signals: evaluator.active_signals.clone(),
        signal_closures: evaluator.signal_closures.clone(),
        declaration_scopes: evaluator.declaration_scopes.clone(),
        integer_domain: evaluator.integer_domain,
        lifecycle: evaluator.lifecycle,
    };
    subscripts
        .iter()
        .map(|subscript| {
            let index = shadow
                .expression(subscript, locals)?
                .integer()
                .ok_or(EvaluationError::Type("array subscript"))?;
            let zero_based = index
                .checked_sub(1)
                .ok_or(EvaluationError::Bounds { index, length: 0 })?;
            usize::try_from(zero_based).map_err(|_| EvaluationError::Bounds { index, length: 0 })
        })
        .collect()
}

pub(super) fn indexed(
    value: &Value,
    subscripts: &[ast::Expression],
    evaluator: &Evaluator<'_>,
    locals: &BTreeMap<String, Value>,
) -> Result<Value, EvaluationError> {
    indexed_owned(value.clone(), subscripts, evaluator, locals)
}

pub(super) fn indexed_owned(
    mut value: Value,
    subscripts: &[ast::Expression],
    evaluator: &Evaluator<'_>,
    locals: &BTreeMap<String, Value>,
) -> Result<Value, EvaluationError> {
    for index in indices(subscripts, evaluator, locals)? {
        let Value::Array(values) = value else {
            return Err(EvaluationError::Type("array reference"));
        };
        let length = values.len();
        value = values.get(index).cloned().ok_or(EvaluationError::Bounds {
            index: i64::try_from(index + 1).map_err(|_| {
                EvaluationError::MalformedCheckedBlock(
                    "array index exceeds GALEC Integer".to_owned(),
                )
            })?,
            length,
        })?;
    }
    Ok(value)
}

pub(super) fn assign_indices(
    slot: &mut Value,
    indices: &[usize],
    value: Value,
) -> Result<(), EvaluationError> {
    let mut current = slot;
    for &index in indices {
        let Value::Array(values) = current else {
            return Err(EvaluationError::Type("array assignment"));
        };
        let length = values.len();
        current = values.get_mut(index).ok_or(EvaluationError::Bounds {
            index: i64::try_from(index + 1).map_err(|_| {
                EvaluationError::MalformedCheckedBlock(
                    "array index exceeds GALEC Integer".to_owned(),
                )
            })?,
            length,
        })?;
    }
    *current = value;
    Ok(())
}

pub(super) fn assign_state_parts(
    root: &mut Value,
    parts: &[ast::RefPart],
    indices_by_part: &[Vec<usize>],
    value: Value,
) -> Result<(), EvaluationError> {
    *state_path_mut(root, parts, indices_by_part)? = value;
    Ok(())
}

pub(super) fn state_path_mut<'a>(
    root: &'a mut Value,
    parts: &[ast::RefPart],
    indices_by_part: &[Vec<usize>],
) -> Result<&'a mut Value, EvaluationError> {
    if parts.len() != indices_by_part.len() || parts.is_empty() {
        return Err(EvaluationError::MalformedCheckedBlock(
            "state reference path is inconsistent".to_owned(),
        ));
    }
    let mut current = indexed_mut(root, &indices_by_part[0])?;
    for (part, indices) in parts[1..].iter().zip(&indices_by_part[1..]) {
        let Value::Record(fields) = current else {
            return Err(EvaluationError::Type("record component reference"));
        };
        current = fields
            .get_mut(part.name.lexeme())
            .ok_or_else(|| EvaluationError::UnknownName(part.name.lexeme().to_owned()))?;
        current = indexed_mut(current, indices)?;
    }
    Ok(current)
}

pub(super) fn state_path_declaration<'a>(
    block: &'a ast::Block,
    parts: &[ast::RefPart],
) -> Result<&'a ast::VariableDeclaration, EvaluationError> {
    let first = parts.first().ok_or_else(|| {
        EvaluationError::MalformedCheckedBlock("empty checked state reference".to_owned())
    })?;
    let mut declaration = block
        .interface
        .iter()
        .map(|variable| &variable.decl)
        .chain(block.protected.iter().map(|entity| &entity.decl))
        .find(|candidate| candidate.name.lexeme() == first.name.lexeme())
        .ok_or_else(|| EvaluationError::UnknownName(first.name.lexeme().to_owned()))?;
    for part in &parts[1..] {
        let ast::TypeRef::Compartment(name) = &declaration.ty else {
            return Err(EvaluationError::Type("record component declaration"));
        };
        let compartment = block
            .compartments
            .iter()
            .find(|candidate| candidate.name.lexeme() == name.lexeme())
            .ok_or_else(|| {
                EvaluationError::MalformedCheckedBlock(format!(
                    "unknown compartment `{}`",
                    name.lexeme()
                ))
            })?;
        declaration = compartment
            .entities
            .iter()
            .map(|entity| &entity.decl)
            .find(|candidate| candidate.name.lexeme() == part.name.lexeme())
            .ok_or_else(|| EvaluationError::UnknownName(part.name.lexeme().to_owned()))?;
    }
    Ok(declaration)
}

pub(super) fn indexed_mut<'a>(
    mut value: &'a mut Value,
    indices: &[usize],
) -> Result<&'a mut Value, EvaluationError> {
    for &index in indices {
        let Value::Array(values) = value else {
            return Err(EvaluationError::Type("array assignment"));
        };
        let length = values.len();
        value = values.get_mut(index).ok_or(EvaluationError::Bounds {
            index: i64::try_from(index + 1).map_err(|_| {
                EvaluationError::MalformedCheckedBlock(
                    "array index exceeds GALEC Integer".to_owned(),
                )
            })?,
            length,
        })?;
    }
    Ok(value)
}

pub(super) fn limit_value(
    value: &mut Value,
    min: Option<&Value>,
    max: Option<&Value>,
) -> Result<(), EvaluationError> {
    match value {
        Value::Array(values) => {
            for value in values {
                limit_value(value, min, max)?;
            }
        }
        Value::Real(number) => {
            if number.is_nan() {
                return Ok(());
            }
            if let Some(minimum) = min {
                let minimum = minimum
                    .real()
                    .ok_or(EvaluationError::Type("Real minimum bound"))?;
                *number = number.max(minimum);
            }
            if let Some(maximum) = max {
                let maximum = maximum
                    .real()
                    .ok_or(EvaluationError::Type("Real maximum bound"))?;
                *number = number.min(maximum);
            }
        }
        Value::Integer(number) => {
            if let Some(minimum) = min {
                let minimum = minimum
                    .integer()
                    .ok_or(EvaluationError::Type("Integer minimum bound"))?;
                *number = (*number).max(minimum);
            }
            if let Some(maximum) = max {
                let maximum = maximum
                    .integer()
                    .ok_or(EvaluationError::Type("Integer maximum bound"))?;
                *number = (*number).min(maximum);
            }
        }
        Value::Boolean(_) | Value::Record(_) if min.is_some() || max.is_some() => {
            return Err(EvaluationError::Type("range bound"));
        }
        Value::Boolean(_) | Value::Record(_) => {}
        Value::Uninitialized => {
            return Err(EvaluationError::Uninitialized(
                "range-limited value".to_owned(),
            ));
        }
    }
    Ok(())
}

pub(super) fn initialized(value: &Value, name: &str) -> Result<(), EvaluationError> {
    if value.is_initialized() {
        Ok(())
    } else {
        Err(EvaluationError::Uninitialized(name.to_owned()))
    }
}
