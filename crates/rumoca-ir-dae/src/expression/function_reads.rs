use super::*;
use crate::model::{FunctionReadFact, FunctionReadMergeError};

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
            .kind
            .clone();
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
