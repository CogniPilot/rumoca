#[cfg(test)]
mod tests;

use super::*;
use crate::model::{FunctionReadFact, FunctionReadMergeError};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct FunctionInsertionFacts {
    pub(super) scope: Option<u32>,
    pub(super) illegal_coordinate: Option<u32>,
    pub(super) read_set: FunctionReadSet,
    pub(super) latest_call: Option<FunctionCallFact>,
}

impl FunctionInsertionFacts {
    const EMPTY: Self = Self {
        scope: None,
        illegal_coordinate: None,
        read_set: FunctionReadSet::EMPTY,
        latest_call: None,
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct FunctionCallFact {
    pub(crate) target: u32,
    pub(crate) witness: u32,
}

/// Derives all function-local facts while visiting each direct child once.
///
/// Scope errors take precedence over illegal-coordinate and read errors.
/// Lower-priority errors are deferred without changing their left-to-right
/// witness orientation.
pub(super) fn node_function_facts(
    storage: &mut Storage,
    node: &ExprNode,
    at: DaeProvenance,
) -> Result<FunctionInsertionFacts, DaeConstructionError> {
    if let Some(facts) = leaf_function_facts(storage, node, at)? {
        return Ok(facts);
    }

    let mut fold = FunctionFactsFold::new();
    match node {
        ExprNode::Unary { operand, .. }
        | ExprNode::Field { base: operand, .. }
        | ExprNode::Comprehension { body: operand, .. } => {
            fold.merge_expression(storage, *operand, at)?;
        }
        ExprNode::Binary { lhs, rhs, .. } => {
            fold.merge_expression(storage, *lhs, at)?;
            fold.merge_expression(storage, *rhs, at)?;
        }
        ExprNode::Index { base, subscripts } => {
            fold.merge_expression(storage, *base, at)?;
            fold.merge_subscripts(storage, *subscripts, at)?;
        }
        ExprNode::ArrayUpdate {
            base,
            value,
            subscripts,
        } => {
            fold.merge_expression(storage, *base, at)?;
            fold.merge_expression(storage, *value, at)?;
            fold.merge_subscripts(storage, *subscripts, at)?;
        }
        ExprNode::Conditional { operands }
        | ExprNode::Array { operands }
        | ExprNode::Record { operands }
        | ExprNode::Builtin { operands, .. } => fold.merge_operands(storage, *operands, at)?,
        ExprNode::Call {
            function, operands, ..
        } => {
            fold.merge_operands(storage, *operands, at)?;
            fold.record_call(storage, *function, at)?;
        }
        ExprNode::Literal(_)
        | ExprNode::Coordinate(_)
        | ExprNode::Range { .. }
        | ExprNode::FunctionValue { .. }
        | ExprNode::FunctionFoldParameter { .. }
        | ExprNode::FunctionFoldOutput { .. } => unreachable!("leaf expressions returned above"),
    }
    fold.finish(storage, at)
}

fn leaf_function_facts(
    storage: &mut Storage,
    node: &ExprNode,
    at: DaeProvenance,
) -> Result<Option<FunctionInsertionFacts>, DaeConstructionError> {
    match node {
        ExprNode::Coordinate(Coordinate::FunctionParameter { function, .. }) => {
            checked_u32(storage.expressions.nodes.len(), "expression arena", at)?;
            Ok(Some(FunctionInsertionFacts {
                scope: Some(*function),
                ..FunctionInsertionFacts::EMPTY
            }))
        }
        ExprNode::FunctionValue {
            function,
            value,
            definition_ordinal,
        } => {
            let witness = checked_u32(storage.expressions.nodes.len(), "expression arena", at)?;
            let read_set = storage.function_read_sets.singleton(
                FunctionReadFact {
                    value: *value,
                    definition: *definition_ordinal,
                    witness,
                },
                at,
            )?;
            Ok(Some(FunctionInsertionFacts {
                scope: Some(*function),
                read_set,
                ..FunctionInsertionFacts::EMPTY
            }))
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
            let witness = checked_u32(storage.expressions.nodes.len(), "expression arena", at)?;
            let fold = FunctionFoldId::from_raw(*function, *fold);
            let entry = function_fold_entry(storage, fold, at)?;
            let value = entry
                .targets
                .get(*carried as usize)
                .copied()
                .ok_or_else(|| invalid_arity(entry.targets.len(), *carried as usize + 1, at))?;
            let read_set = storage.function_read_sets.singleton(
                FunctionReadFact {
                    value,
                    definition: *definition_ordinal,
                    witness,
                },
                at,
            )?;
            Ok(Some(FunctionInsertionFacts {
                scope: Some(*function),
                read_set,
                ..FunctionInsertionFacts::EMPTY
            }))
        }
        ExprNode::Coordinate(Coordinate::Binder { .. })
        | ExprNode::Literal(_)
        | ExprNode::Range { .. } => {
            checked_u32(storage.expressions.nodes.len(), "expression arena", at)?;
            Ok(Some(FunctionInsertionFacts::EMPTY))
        }
        ExprNode::Coordinate(_) => {
            let witness = checked_u32(storage.expressions.nodes.len(), "expression arena", at)?;
            Ok(Some(FunctionInsertionFacts {
                illegal_coordinate: Some(witness),
                ..FunctionInsertionFacts::EMPTY
            }))
        }
        ExprNode::Unary { .. }
        | ExprNode::Binary { .. }
        | ExprNode::Conditional { .. }
        | ExprNode::Array { .. }
        | ExprNode::Record { .. }
        | ExprNode::Field { .. }
        | ExprNode::Comprehension { .. }
        | ExprNode::Index { .. }
        | ExprNode::ArrayUpdate { .. }
        | ExprNode::Builtin { .. }
        | ExprNode::Call { .. } => Ok(None),
    }
}

#[derive(Debug)]
struct FunctionFactsFold {
    facts: FunctionInsertionFacts,
    illegal_error: Option<DaeConstructionError>,
    read_error: Option<FunctionReadMergeError>,
}

impl FunctionFactsFold {
    const fn new() -> Self {
        Self {
            facts: FunctionInsertionFacts::EMPTY,
            illegal_error: None,
            read_error: None,
        }
    }

    fn merge_operands(
        &mut self,
        storage: &mut Storage,
        operands: OperandRange,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        if storage
            .expressions
            .operands
            .get(operands.indices())
            .is_none()
        {
            return Err(crate::model::unknown("operand range", operands.start, at));
        }
        for index in operands.indices() {
            let expression = storage.expressions.operands[index];
            self.merge_expression(storage, expression, at)?;
        }
        Ok(())
    }

    fn merge_subscripts(
        &mut self,
        storage: &mut Storage,
        subscripts: OperandRange,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        if storage
            .expressions
            .subscripts
            .get(subscripts.indices())
            .is_none()
        {
            return Err(crate::model::unknown(
                "subscript range",
                subscripts.start,
                at,
            ));
        }
        for index in subscripts.indices() {
            let expression = match storage.expressions.subscripts[index].kind {
                PackedSubscriptKind::Index(expression) | PackedSubscriptKind::Slice(expression) => {
                    expression
                }
                PackedSubscriptKind::Whole => continue,
            };
            self.merge_expression(storage, expression, at)?;
        }
        Ok(())
    }

    fn merge_expression(
        &mut self,
        storage: &mut Storage,
        expression: u32,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let index = expression as usize;
        let scope = storage
            .expressions
            .function_scopes
            .get(index)
            .copied()
            .ok_or_else(|| crate::model::unknown("expression", expression, at))?;
        self.facts.scope = merge_function_scope(self.facts.scope, scope, at)?;
        let latest_call = storage
            .expressions
            .function_latest_calls
            .get(index)
            .copied()
            .ok_or_else(|| crate::model::unknown("expression", expression, at))?;
        self.merge_call(latest_call);

        if self.facts.illegal_coordinate.is_none() && self.illegal_error.is_none() {
            match storage
                .expressions
                .function_illegal_coordinates
                .get(index)
                .copied()
            {
                Some(illegal) => self.facts.illegal_coordinate = illegal,
                None => {
                    self.illegal_error = Some(crate::model::unknown("expression", expression, at));
                }
            }
        }

        if self.illegal_error.is_none() && self.read_error.is_none() {
            match storage.expressions.function_read_sets.get(index).copied() {
                Some(read_set) => {
                    match storage
                        .function_read_sets
                        .merge(self.facts.read_set, read_set, at)
                    {
                        Ok(merged) => self.facts.read_set = merged,
                        Err(error) => self.read_error = Some(error),
                    }
                }
                None => {
                    self.read_error = Some(FunctionReadMergeError::Construction(
                        crate::model::unknown("expression", expression, at),
                    ));
                }
            }
        }
        Ok(())
    }

    fn record_call(
        &mut self,
        storage: &Storage,
        target: u32,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let witness = checked_u32(storage.expressions.nodes.len(), "expression arena", at)?;
        self.merge_call(Some(FunctionCallFact { target, witness }));
        Ok(())
    }

    fn merge_call(&mut self, candidate: Option<FunctionCallFact>) {
        if candidate.is_some_and(|candidate| {
            self.facts
                .latest_call
                .is_none_or(|current| candidate.target > current.target)
        }) {
            self.facts.latest_call = candidate;
        }
    }

    fn finish(
        self,
        storage: &Storage,
        at: DaeProvenance,
    ) -> Result<FunctionInsertionFacts, DaeConstructionError> {
        if let Some(error) = self.illegal_error {
            return Err(error);
        }
        checked_u32(storage.expressions.nodes.len(), "expression arena", at)?;
        if let Some(error) = self.read_error {
            return Err(function_read_error(storage, error, at));
        }
        Ok(self.facts)
    }
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

fn function_read_error(
    storage: &Storage,
    error: FunctionReadMergeError,
    at: DaeProvenance,
) -> DaeConstructionError {
    match error {
        FunctionReadMergeError::Construction(error) => error,
        FunctionReadMergeError::Conflict(conflict) => {
            let Some(provenance) = storage
                .expressions
                .provenance
                .get(conflict.found.witness as usize)
                .copied()
            else {
                return crate::model::unknown("expression provenance", conflict.found.witness, at);
            };
            DaeConstructionError::InvalidFunctionValueRead {
                value: conflict.found.value,
                expected_definition: Some(conflict.expected.definition),
                found_definition: conflict.found.definition,
                span: provenance.span(),
            }
        }
    }
}
