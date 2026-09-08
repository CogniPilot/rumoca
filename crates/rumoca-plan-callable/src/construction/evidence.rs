use std::{collections::BTreeSet, marker::PhantomData};

use rumoca_core::OperationContractKey;
use rumoca_ir_dae::{DaeLiteral, ExprId, ExpressionOperation, SubscriptView};

use super::contract::{authenticate_contract, callable_value_type, checked_id};
use super::plan::{
    CallableOperationDetail, OperationEntry, OperationSource, ValueEntry, ValueProducer,
};
use super::{
    CallableExpressionSource, CallablePlanConstruction, ConstructionScopeId, ConstructionValueId,
    PlanConstructionError,
};

impl<'plan, 'dae> CallablePlanConstruction<'plan, 'dae> {
    /// Construct one tensor projection while consuming its complete checked
    /// coordinate proof in the same poisoned transaction. Coordinate evidence
    /// is deliberately absent from the executable operation.
    pub fn add_index(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        source: CallableExpressionSource<'plan, 'dae>,
        base: ConstructionValueId<'plan>,
    ) -> Result<ConstructionValueId<'plan>, PlanConstructionError> {
        self.transaction(|construction| construction.try_add_index(scope, source, base))
    }

    fn try_add_index(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        source: CallableExpressionSource<'plan, 'dae>,
        base: ConstructionValueId<'plan>,
    ) -> Result<ConstructionValueId<'plan>, PlanConstructionError> {
        let span = source.provenance.span();
        let expression = self.checked_expression(scope, source)?;
        let ExpressionOperation::Index {
            base: expected_base,
            subscripts,
        } = expression.operation()
        else {
            return Err(PlanConstructionError::InvalidOperation { span });
        };
        self.check_operands(scope, &[base], expression, [expected_base], span)?;
        let contract = authenticate_contract(self.dae, expression)?;
        if !matches!(
            contract,
            OperationContractKey::ProjectElement | OperationContractKey::ProjectSlice
        ) {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        let evidence = self.checked_subscript_evidence(source, expected_base, subscripts.iter())?;
        self.finish_evidence_operation(scope, source, contract, &[base], &evidence)
    }

    /// Construct one element update from the exact Integer coordinate values
    /// named by its checked DAE subscripts. DAE construction has already
    /// discharged every statically decidable bound; dynamic coordinates remain
    /// explicit operands so a later representation-bound root can own Indexing
    /// status. Slice updates remain unsupported and refuse construction.
    pub fn add_array_update(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        source: CallableExpressionSource<'plan, 'dae>,
        base: ConstructionValueId<'plan>,
        value: ConstructionValueId<'plan>,
        indices: &[ConstructionValueId<'plan>],
    ) -> Result<ConstructionValueId<'plan>, PlanConstructionError> {
        self.transaction(|construction| {
            construction.try_add_array_update(scope, source, base, value, indices)
        })
    }

    fn try_add_array_update(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        source: CallableExpressionSource<'plan, 'dae>,
        base: ConstructionValueId<'plan>,
        value: ConstructionValueId<'plan>,
        indices: &[ConstructionValueId<'plan>],
    ) -> Result<ConstructionValueId<'plan>, PlanConstructionError> {
        let span = source.provenance.span();
        let expression = self.checked_expression(scope, source)?;
        let ExpressionOperation::ArrayUpdate {
            base: expected_base,
            value: expected_value,
            subscripts,
        } = expression.operation()
        else {
            return Err(PlanConstructionError::InvalidOperation { span });
        };
        self.check_operands(
            scope,
            &[base, value],
            expression,
            [expected_base, expected_value],
            span,
        )?;
        let contract = authenticate_contract(self.dae, expression)?;
        if contract != OperationContractKey::UpdateElement {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        let owner = self.scope_owner(scope, span)?;
        let dimensions = self
            .dae
            .exact_expression(expected_base)
            .value_type()
            .dimensions();
        if indices.len() != subscripts.len() || dimensions.len() != subscripts.len() {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        for (subscript, coordinate) in subscripts.iter().zip(indices.iter().copied()) {
            let SubscriptView::Index {
                expression: expected_coordinate,
                provenance,
            } = subscript
            else {
                return Err(PlanConstructionError::InvalidOperation { span });
            };
            let coordinate_entry = self
                .values
                .get(coordinate.raw as usize)
                .ok_or(PlanConstructionError::InvalidOperation { span })?;
            if coordinate_entry.owner != owner
                || !self.scope_dominates(coordinate_entry.scope, scope.raw)
                || coordinate_entry.source_expression != expected_coordinate.index()
                || coordinate_entry.value_type.scalar != crate::CallableScalarType::Integer
                || !coordinate_entry.value_type.dimensions.is_empty()
            {
                return Err(PlanConstructionError::InvalidOperation {
                    span: provenance.span(),
                });
            }
        }
        let operands = std::iter::once(base)
            .chain(std::iter::once(value))
            .chain(indices.iter().copied())
            .collect::<Vec<_>>();
        self.finish_evidence_operation(scope, source, contract, &operands, &BTreeSet::new())
    }

    fn checked_subscript_evidence(
        &self,
        source: CallableExpressionSource<'plan, 'dae>,
        base: ExprId<'dae>,
        subscripts: impl IntoIterator<Item = SubscriptView<'dae>>,
    ) -> Result<BTreeSet<ExprId<'dae>>, PlanConstructionError> {
        let mut evidence = BTreeSet::new();
        let dimensions = self.dae.exact_expression(base).value_type().dimensions();
        for (ordinal, subscript) in subscripts.into_iter().enumerate() {
            let dimension = dimensions.get(ordinal).copied().ok_or(
                PlanConstructionError::InvalidOperation {
                    span: source.provenance.span(),
                },
            )?;
            match subscript {
                SubscriptView::Index {
                    expression,
                    provenance,
                } => self.checked_index_evidence(
                    source,
                    expression,
                    provenance,
                    dimension,
                    &mut evidence,
                )?,
                SubscriptView::Whole { .. } => {}
                SubscriptView::Slice {
                    expression,
                    provenance,
                } => {
                    self.checked_slice_evidence(expression, provenance, dimension, &mut evidence)?
                }
            }
        }
        self.checked_outstanding_evidence(source.function, &evidence)?;
        Ok(evidence)
    }

    /// One literal coordinate must sit inside its exact declared extent.
    fn checked_index_evidence(
        &self,
        source: CallableExpressionSource<'plan, 'dae>,
        expression: ExprId<'dae>,
        provenance: rumoca_ir_dae::DaeProvenance,
        dimension: u32,
        evidence: &mut BTreeSet<ExprId<'dae>>,
    ) -> Result<(), PlanConstructionError> {
        let coordinate = self.checked_integer_literal(source.function, expression)?;
        if coordinate < 1 || coordinate > i64::from(dimension) {
            return Err(PlanConstructionError::InvalidOperation {
                span: provenance.span(),
            });
        }
        evidence.insert(expression);
        Ok(())
    }

    /// One slice must be an exact literal range whose nonempty extent stays
    /// inside its declared dimension; its bounds and step are its evidence.
    fn checked_slice_evidence(
        &self,
        expression: ExprId<'dae>,
        provenance: rumoca_ir_dae::DaeProvenance,
        dimension: u32,
        evidence: &mut BTreeSet<ExprId<'dae>>,
    ) -> Result<(), PlanConstructionError> {
        let range = self.dae.exact_expression(expression);
        let ExpressionOperation::Range(range) = range.operation() else {
            return Err(PlanConstructionError::InvalidOperation {
                span: range.provenance().span(),
            });
        };
        let start = range.start().value();
        let stop = range.stop().value();
        let step = range.effective_step();
        let nonempty = (step > 0 && start <= stop) || (step < 0 && start >= stop);
        let outside =
            start < 1 || start > i64::from(dimension) || stop < 1 || stop > i64::from(dimension);
        if step == 0 || (nonempty && outside) {
            return Err(PlanConstructionError::InvalidOperation {
                span: provenance.span(),
            });
        }
        evidence.insert(expression);
        evidence.insert(range.start().expression());
        if let Some(step) = range.explicit_step() {
            evidence.insert(step.expression());
        }
        evidence.insert(range.stop().expression());
        Ok(())
    }

    fn checked_integer_literal(
        &self,
        function: rumoca_ir_dae::FunctionId<'dae>,
        expression: ExprId<'dae>,
    ) -> Result<i64, PlanConstructionError> {
        let value = self.exact_integer_literal(expression)?;
        let expression_view = self.dae.exact_expression(expression);
        if !self
            .outstanding_expressions
            .contains(&(function, expression))
        {
            return Err(PlanConstructionError::InvalidOperation {
                span: expression_view.provenance().span(),
            });
        }
        Ok(value)
    }

    fn exact_integer_literal(
        &self,
        expression: ExprId<'dae>,
    ) -> Result<i64, PlanConstructionError> {
        let expression_view = self.dae.exact_expression(expression);
        let ExpressionOperation::Literal(DaeLiteral::Integer(value)) = expression_view.operation()
        else {
            return Err(PlanConstructionError::InvalidOperation {
                span: expression_view.provenance().span(),
            });
        };
        Ok(*value)
    }

    fn checked_outstanding_evidence(
        &self,
        function: rumoca_ir_dae::FunctionId<'dae>,
        evidence: &BTreeSet<ExprId<'dae>>,
    ) -> Result<(), PlanConstructionError> {
        for expression in evidence {
            if !self
                .outstanding_expressions
                .contains(&(function, *expression))
            {
                return Err(PlanConstructionError::DuplicateOccurrence {
                    span: self.dae.exact_expression(*expression).provenance().span(),
                });
            }
        }
        Ok(())
    }

    fn finish_evidence_operation(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        source: CallableExpressionSource<'plan, 'dae>,
        contract: OperationContractKey,
        operands: &[ConstructionValueId<'plan>],
        evidence: &BTreeSet<ExprId<'dae>>,
    ) -> Result<ConstructionValueId<'plan>, PlanConstructionError> {
        let span = source.provenance.span();
        let owner = self.scope_owner(scope, span)?;
        let expression = self.dae.exact_expression(source.expression);
        let operation = checked_id(self.operations.len(), span)?;
        let value = checked_id(self.values.len(), span)?;
        let value_type = callable_value_type(self.dae, expression.value_type_id(), span)?;
        self.claim_expression(source)?;
        for evidence_source in evidence {
            self.outstanding_expressions
                .remove(&(source.function, *evidence_source));
        }
        self.operations.push(OperationEntry {
            owner,
            scope: scope.raw,
            source: OperationSource::Expression(source.expression.index()),
            contract,
            operands: operands.iter().map(|operand| operand.raw).collect(),
            results: Box::new([value]),
            detail: CallableOperationDetail::Plain,
            provenance: source.provenance,
        });
        self.values.push(ValueEntry {
            owner,
            scope: scope.raw,
            source_expression: source.expression.index(),
            value_type,
            producer: ValueProducer::Operation(operation),
        });
        self.scopes[scope.raw as usize].operations.push(operation);
        self.owners[owner as usize].operations.push(operation);
        Ok(ConstructionValueId {
            raw: value,
            marker: PhantomData,
        })
    }
}
