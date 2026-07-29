#[cfg(test)]
mod tests;

use super::*;
use crate::model::{FunctionReadFact, FunctionReadMergeError};

pub(super) fn node_function_scope(
    storage: &Storage,
    node: &ExprNode,
    at: DaeProvenance,
) -> Result<Option<u32>, DaeConstructionError> {
    match node {
        ExprNode::Coordinate(Coordinate::FunctionParameter { function, .. })
        | ExprNode::FunctionValue { function, .. }
        | ExprNode::FunctionFoldParameter { function, .. }
        | ExprNode::FunctionFoldOutput { function, .. } => Ok(Some(*function)),
        ExprNode::Literal(_) | ExprNode::Coordinate(_) | ExprNode::Range { .. } => Ok(None),
        ExprNode::Unary { operand, .. }
        | ExprNode::Field { base: operand, .. }
        | ExprNode::Comprehension { body: operand, .. } => {
            merge_expression_function_scope(storage, None, *operand, at)
        }
        ExprNode::Binary { lhs, rhs, .. } => {
            let scope = merge_expression_function_scope(storage, None, *lhs, at)?;
            merge_expression_function_scope(storage, scope, *rhs, at)
        }
        ExprNode::Index { base, subscripts } => {
            let scope = merge_expression_function_scope(storage, None, *base, at)?;
            merge_subscript_function_scopes(storage, scope, *subscripts, at)
        }
        ExprNode::ArrayUpdate {
            base,
            value,
            subscripts,
        } => {
            let scope = merge_expression_function_scope(storage, None, *base, at)?;
            let scope = merge_expression_function_scope(storage, scope, *value, at)?;
            merge_subscript_function_scopes(storage, scope, *subscripts, at)
        }
        ExprNode::Conditional { operands }
        | ExprNode::Array { operands }
        | ExprNode::Record { operands }
        | ExprNode::Builtin { operands, .. }
        | ExprNode::Call { operands, .. } => merge_operand_function_scopes(storage, *operands, at),
    }
}

fn merge_operand_function_scopes(
    storage: &Storage,
    operands: OperandRange,
    at: DaeProvenance,
) -> Result<Option<u32>, DaeConstructionError> {
    storage
        .expressions
        .operands
        .get(operands.indices())
        .ok_or_else(|| crate::model::unknown("operand range", operands.start, at))?
        .iter()
        .try_fold(None, |scope, expression| {
            merge_expression_function_scope(storage, scope, *expression, at)
        })
}

fn merge_subscript_function_scopes(
    storage: &Storage,
    mut scope: Option<u32>,
    subscripts: OperandRange,
    at: DaeProvenance,
) -> Result<Option<u32>, DaeConstructionError> {
    for subscript in storage
        .expressions
        .subscripts
        .get(subscripts.indices())
        .ok_or_else(|| crate::model::unknown("subscript range", subscripts.start, at))?
    {
        let expression = match subscript.kind {
            PackedSubscriptKind::Index(expression) | PackedSubscriptKind::Slice(expression) => {
                expression
            }
            PackedSubscriptKind::Whole => continue,
        };
        scope = merge_expression_function_scope(storage, scope, expression, at)?;
    }
    Ok(scope)
}

fn merge_expression_function_scope(
    storage: &Storage,
    scope: Option<u32>,
    expression: u32,
    at: DaeProvenance,
) -> Result<Option<u32>, DaeConstructionError> {
    let found = storage
        .expressions
        .function_scopes
        .get(expression as usize)
        .copied()
        .ok_or_else(|| crate::model::unknown("expression", expression, at))?;
    merge_function_scope(scope, found, at)
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

pub(super) fn node_function_illegal_coordinate(
    storage: &Storage,
    node: &ExprNode,
    at: DaeProvenance,
) -> Result<Option<u32>, DaeConstructionError> {
    let direct = match node {
        ExprNode::Coordinate(Coordinate::FunctionParameter { .. } | Coordinate::Binder { .. })
        | ExprNode::Literal(_)
        | ExprNode::Range { .. }
        | ExprNode::FunctionValue { .. }
        | ExprNode::FunctionFoldParameter { .. }
        | ExprNode::FunctionFoldOutput { .. } => return Ok(None),
        ExprNode::Coordinate(_) => {
            return checked_u32(storage.expressions.nodes.len(), "expression arena", at).map(Some);
        }
        ExprNode::Unary { operand, .. }
        | ExprNode::Field { base: operand, .. }
        | ExprNode::Comprehension { body: operand, .. } => {
            return function_illegal_coordinate_of(storage, *operand, at);
        }
        ExprNode::Binary { lhs, rhs, .. } => {
            return function_illegal_coordinate_of(storage, *lhs, at)?.map_or_else(
                || function_illegal_coordinate_of(storage, *rhs, at),
                |illegal| Ok(Some(illegal)),
            );
        }
        ExprNode::Index { base, subscripts } => {
            if let Some(illegal) = function_illegal_coordinate_of(storage, *base, at)? {
                return Ok(Some(illegal));
            }
            return subscript_function_illegal_coordinate(storage, *subscripts, at);
        }
        ExprNode::ArrayUpdate {
            base,
            value,
            subscripts,
        } => {
            if let Some(illegal) = function_illegal_coordinate_of(storage, *base, at)?
                .or(function_illegal_coordinate_of(storage, *value, at)?)
            {
                return Ok(Some(illegal));
            }
            return subscript_function_illegal_coordinate(storage, *subscripts, at);
        }
        ExprNode::Conditional { operands }
        | ExprNode::Array { operands }
        | ExprNode::Record { operands }
        | ExprNode::Builtin { operands, .. }
        | ExprNode::Call { operands, .. } => *operands,
    };
    storage
        .expressions
        .operands
        .get(direct.indices())
        .ok_or_else(|| crate::model::unknown("operand range", direct.start, at))?
        .iter()
        .try_fold(None, |found, expression| {
            if found.is_some() {
                Ok(found)
            } else {
                function_illegal_coordinate_of(storage, *expression, at)
            }
        })
}

fn subscript_function_illegal_coordinate(
    storage: &Storage,
    subscripts: OperandRange,
    at: DaeProvenance,
) -> Result<Option<u32>, DaeConstructionError> {
    storage
        .expressions
        .subscripts
        .get(subscripts.indices())
        .ok_or_else(|| crate::model::unknown("subscript range", subscripts.start, at))?
        .iter()
        .try_fold(None, |found, subscript| {
            if found.is_some() {
                return Ok(found);
            }
            match subscript.kind {
                PackedSubscriptKind::Index(expression) | PackedSubscriptKind::Slice(expression) => {
                    function_illegal_coordinate_of(storage, expression, at)
                }
                PackedSubscriptKind::Whole => Ok(None),
            }
        })
}

fn function_illegal_coordinate_of(
    storage: &Storage,
    expression: u32,
    at: DaeProvenance,
) -> Result<Option<u32>, DaeConstructionError> {
    storage
        .expressions
        .function_illegal_coordinates
        .get(expression as usize)
        .copied()
        .ok_or_else(|| crate::model::unknown("expression", expression, at))
}

pub(super) fn node_function_read_set(
    storage: &mut Storage,
    node: &ExprNode,
    at: DaeProvenance,
) -> Result<FunctionReadSet, DaeConstructionError> {
    let witness = checked_u32(storage.expressions.nodes.len(), "expression arena", at)?;
    match node {
        ExprNode::FunctionValue {
            value,
            definition_ordinal,
            ..
        } => {
            return storage.function_read_sets.singleton(
                FunctionReadFact {
                    value: *value,
                    definition: *definition_ordinal,
                    witness,
                },
                at,
            );
        }
        ExprNode::FunctionFoldParameter {
            function,
            fold,
            carried,
            definition_ordinal,
        }
        | ExprNode::FunctionFoldOutput {
            function,
            fold,
            carried,
            definition_ordinal,
        } => {
            let fold = FunctionFoldId::from_raw(*function, *fold);
            let entry = function_fold_entry(storage, fold, at)?;
            let value = entry
                .targets
                .get(*carried as usize)
                .copied()
                .ok_or_else(|| invalid_arity(entry.targets.len(), *carried as usize + 1, at))?;
            return storage.function_read_sets.singleton(
                FunctionReadFact {
                    value,
                    definition: *definition_ordinal,
                    witness,
                },
                at,
            );
        }
        _ => {}
    }
    match node {
        ExprNode::Literal(_)
        | ExprNode::Coordinate(_)
        | ExprNode::Range { .. }
        | ExprNode::FunctionValue { .. }
        | ExprNode::FunctionFoldParameter { .. }
        | ExprNode::FunctionFoldOutput { .. } => Ok(FunctionReadSet::EMPTY),
        ExprNode::Unary { operand, .. }
        | ExprNode::Field { base: operand, .. }
        | ExprNode::Comprehension { body: operand, .. } => {
            function_read_set_of(storage, *operand, at)
        }
        ExprNode::Binary { lhs, rhs, .. } => {
            let lhs = function_read_set_of(storage, *lhs, at)?;
            let rhs = function_read_set_of(storage, *rhs, at)?;
            merge_function_read_sets(storage, lhs, rhs, at)
        }
        ExprNode::Index { base, subscripts } => {
            let base = function_read_set_of(storage, *base, at)?;
            merge_subscript_function_read_sets(storage, base, *subscripts, at)
        }
        ExprNode::ArrayUpdate {
            base,
            value,
            subscripts,
        } => {
            let base = function_read_set_of(storage, *base, at)?;
            let value = function_read_set_of(storage, *value, at)?;
            let merged = merge_function_read_sets(storage, base, value, at)?;
            merge_subscript_function_read_sets(storage, merged, *subscripts, at)
        }
        ExprNode::Conditional { operands }
        | ExprNode::Array { operands }
        | ExprNode::Record { operands }
        | ExprNode::Builtin { operands, .. }
        | ExprNode::Call { operands, .. } => {
            merge_operand_function_read_sets(storage, *operands, at)
        }
    }
}

fn function_read_set_of(
    storage: &Storage,
    expression: u32,
    at: DaeProvenance,
) -> Result<FunctionReadSet, DaeConstructionError> {
    storage
        .expressions
        .function_read_sets
        .get(expression as usize)
        .copied()
        .ok_or_else(|| crate::model::unknown("expression", expression, at))
}

fn merge_operand_function_read_sets(
    storage: &mut Storage,
    operands: OperandRange,
    at: DaeProvenance,
) -> Result<FunctionReadSet, DaeConstructionError> {
    let mut merged = FunctionReadSet::EMPTY;
    for index in operands.indices() {
        let expression = storage
            .expressions
            .operands
            .get(index)
            .copied()
            .ok_or_else(|| crate::model::unknown("operand", index as u32, at))?;
        let found = function_read_set_of(storage, expression, at)?;
        merged = merge_function_read_sets(storage, merged, found, at)?;
    }
    Ok(merged)
}

fn merge_subscript_function_read_sets(
    storage: &mut Storage,
    mut merged: FunctionReadSet,
    subscripts: OperandRange,
    at: DaeProvenance,
) -> Result<FunctionReadSet, DaeConstructionError> {
    for index in subscripts.indices() {
        let kind = storage
            .expressions
            .subscripts
            .get(index)
            .ok_or_else(|| crate::model::unknown("subscript", index as u32, at))?
            .kind;
        let expression = match kind {
            PackedSubscriptKind::Index(expression) | PackedSubscriptKind::Slice(expression) => {
                expression
            }
            PackedSubscriptKind::Whole => continue,
        };
        let found = function_read_set_of(storage, expression, at)?;
        merged = merge_function_read_sets(storage, merged, found, at)?;
    }
    Ok(merged)
}

fn merge_function_read_sets(
    storage: &mut Storage,
    lhs: FunctionReadSet,
    rhs: FunctionReadSet,
    at: DaeProvenance,
) -> Result<FunctionReadSet, DaeConstructionError> {
    match storage.function_read_sets.merge(lhs, rhs, at) {
        Ok(merged) => Ok(merged),
        Err(FunctionReadMergeError::Construction(error)) => Err(error),
        Err(FunctionReadMergeError::Conflict(conflict)) => {
            let provenance = storage
                .expressions
                .provenance
                .get(conflict.found.witness as usize)
                .copied()
                .ok_or_else(|| {
                    crate::model::unknown("expression provenance", conflict.found.witness, at)
                })?;
            Err(DaeConstructionError::InvalidFunctionValueRead {
                value: conflict.found.value,
                expected_definition: Some(conflict.expected.definition),
                found_definition: conflict.found.definition,
                span: provenance.span(),
            })
        }
    }
}
