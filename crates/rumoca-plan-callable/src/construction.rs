use std::{
    collections::{BTreeSet, HashSet},
    marker::PhantomData,
};

use rumoca_core::{OperationContractKey, Span};
use rumoca_ir_dae::{
    CallableSourceInventoryView, DaeProvenance, DaeView, ExprId, ExpressionOperation,
    FunctionAssertionId, FunctionCallId, FunctionConditionalId, FunctionDefinitionId,
    FunctionFoldId, FunctionId, ScalarType,
};
use thiserror::Error;

use self::plan::{
    AcyclicCallEdge, CallProjectionSourceEntry, CallableOperationDetail, EffectEntry,
    OperationEntry, OperationSource, ProjectionEntry, StructuredRegionDetail,
    StructuredRegionEntry, ValueEntry, ValueProducer,
};
use crate::model::{
    CallableIntegerSourceFact, CallableInterface, CallableScalarType, CallableValueType,
};

mod conditional_group;
mod contract;
mod evidence;
pub(crate) mod plan;
use contract::*;
pub use plan::CallablePlan;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum PlanConstructionError {
    #[error("callable plan identity capacity overflow")]
    IdentityOverflow { span: Span },
    #[error("callable plan references a foreign source occurrence")]
    InvalidSourceOccurrence { span: Span },
    #[error("callable plan references an unavailable owner")]
    InvalidOwner { span: Span },
    #[error("callable plan violates source structure, dominance, or ownership")]
    InvalidOperation { span: Span },
    #[error("callable plan repeats a DAE source occurrence")]
    DuplicateOccurrence { span: Span },
    #[error("callable plan left a DAE source occurrence unclaimed")]
    IncompleteCoverage { span: Span },
    /// A call occurrence closes a cycle in the owner call graph.
    ///
    /// The span is the recursive call itself, never the declaration of a
    /// caller that happens to sit on the cycle. Recursion is a named
    /// unsupported callable behavior, not a violated plan invariant.
    #[error("callable plan cannot represent a recursive call")]
    UnsupportedRecursion { span: Span },
}

impl PlanConstructionError {
    pub const fn span(&self) -> Span {
        match self {
            Self::IdentityOverflow { span }
            | Self::InvalidSourceOccurrence { span }
            | Self::InvalidOwner { span }
            | Self::InvalidOperation { span }
            | Self::DuplicateOccurrence { span }
            | Self::IncompleteCoverage { span }
            | Self::UnsupportedRecursion { span } => *span,
        }
    }
}

macro_rules! construction_id {
    ($name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub struct $name<'plan> {
            raw: u32,
            marker: PhantomData<&'plan mut &'plan ()>,
        }
    };
}

construction_id!(ConstructionOwnerId);
construction_id!(ConstructionScopeId);
construction_id!(ConstructionValueId);

/// Linear capability for closing one exact conditional region.
///
/// ```compile_fail
/// fn duplicate<'plan, 'dae>(
///     region: rumoca_plan_callable::ConditionalRegionConstruction<'plan, 'dae>,
/// ) {
///     let _forged_second_close = region.clone();
/// }
/// ```
#[must_use = "close this conditional region capability; dropping it abandons the open region"]
pub struct ConditionalRegionConstruction<'plan, 'dae> {
    source: CallableExpressionSource<'plan, 'dae>,
    parent: ConstructionScopeId<'plan>,
    branches: Box<[ConstructionScopeId<'plan>]>,
    fallback: ConstructionScopeId<'plan>,
}

/// Linear capability for one exact atomic conditional assignment group.
///
/// ```compile_fail
/// fn duplicate<'plan, 'dae>(
///     region: rumoca_plan_callable::ConditionalGroupConstruction<'plan, 'dae>,
/// ) {
///     let _forged_split_close = region.clone();
/// }
/// ```
#[must_use = "close this conditional group capability; dropping it abandons the open group"]
pub struct ConditionalGroupConstruction<'plan, 'dae> {
    source: CallableConditionalSource<'plan, 'dae>,
    parent: ConstructionScopeId<'plan>,
    branches: Box<[ConstructionScopeId<'plan>]>,
    fallback: ConstructionScopeId<'plan>,
}

impl<'plan> ConditionalGroupConstruction<'plan, '_> {
    pub fn branch_scopes(&self) -> impl ExactSizeIterator<Item = ConstructionScopeId<'plan>> + '_ {
        self.branches.iter().copied()
    }

    pub const fn fallback_scope(&self) -> ConstructionScopeId<'plan> {
        self.fallback
    }

    /// Every child scope this capability opened, branches first, fallback last.
    fn child_scopes(&self) -> impl Iterator<Item = ConstructionScopeId<'plan>> + '_ {
        self.branches
            .iter()
            .copied()
            .chain(std::iter::once(self.fallback))
    }
}

impl<'plan> ConditionalRegionConstruction<'plan, '_> {
    pub fn branch_scopes(&self) -> impl ExactSizeIterator<Item = ConstructionScopeId<'plan>> + '_ {
        self.branches.iter().copied()
    }

    pub const fn fallback_scope(&self) -> ConstructionScopeId<'plan> {
        self.fallback
    }

    /// Every child scope this capability opened, branches first, fallback last.
    fn child_scopes(&self) -> impl Iterator<Item = ConstructionScopeId<'plan>> + '_ {
        self.branches
            .iter()
            .copied()
            .chain(std::iter::once(self.fallback))
    }
}

/// Linear capability for closing one exact compact map region.
///
/// ```compile_fail
/// fn duplicate<'plan, 'dae>(
///     region: rumoca_plan_callable::MapRegionConstruction<'plan, 'dae>,
/// ) {
///     let _forged_second_close = region.clone();
/// }
/// ```
#[must_use = "close this map region capability; dropping it abandons the open region"]
pub struct MapRegionConstruction<'plan, 'dae> {
    source: CallableExpressionSource<'plan, 'dae>,
    parent: ConstructionScopeId<'plan>,
    body: ConstructionScopeId<'plan>,
}

impl<'plan> MapRegionConstruction<'plan, '_> {
    pub const fn body_scope(&self) -> ConstructionScopeId<'plan> {
        self.body
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallableOperandEdge<'plan, 'dae> {
    function: FunctionId<'dae>,
    consumer: ExprId<'dae>,
    operand: ExprId<'dae>,
    ordinal: usize,
    provenance: DaeProvenance,
    marker: PhantomData<&'plan mut &'plan ()>,
}

impl<'dae> CallableOperandEdge<'_, 'dae> {
    pub const fn consumer(self) -> ExprId<'dae> {
        self.consumer
    }

    pub const fn operand(self) -> ExprId<'dae> {
        self.operand
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

macro_rules! source_id {
    ($name:ident, $id:ident, $field:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub struct $name<'plan, 'dae> {
            function: FunctionId<'dae>,
            $field: $id<'dae>,
            provenance: DaeProvenance,
            marker: PhantomData<&'plan mut &'plan ()>,
        }

        impl $name<'_, '_> {
            pub const fn provenance(self) -> DaeProvenance {
                self.provenance
            }
        }
    };
}

source_id!(CallableExpressionSource, ExprId, expression);
source_id!(CallableDefinitionSource, FunctionDefinitionId, definition);
source_id!(CallableAssertionSource, FunctionAssertionId, assertion);
source_id!(CallableCallSource, FunctionCallId, call);
source_id!(
    CallableConditionalSource,
    FunctionConditionalId,
    conditional
);
source_id!(CallableFoldSource, FunctionFoldId, fold);

impl<'dae> CallableExpressionSource<'_, 'dae> {
    pub const fn function(self) -> FunctionId<'dae> {
        self.function
    }
    pub const fn expression(self) -> ExprId<'dae> {
        self.expression
    }
}

impl<'dae> CallableDefinitionSource<'_, 'dae> {
    pub const fn function(self) -> FunctionId<'dae> {
        self.function
    }
    pub const fn definition(self) -> FunctionDefinitionId<'dae> {
        self.definition
    }
}

impl<'dae> CallableAssertionSource<'_, 'dae> {
    pub const fn function(self) -> FunctionId<'dae> {
        self.function
    }
    pub const fn assertion(self) -> FunctionAssertionId<'dae> {
        self.assertion
    }
}

impl<'dae> CallableCallSource<'_, 'dae> {
    pub const fn function(self) -> FunctionId<'dae> {
        self.function
    }
    pub const fn call(self) -> FunctionCallId<'dae> {
        self.call
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallableCallProjectionSource<'plan, 'dae> {
    expression: ExprId<'dae>,
    output: u32,
    provenance: DaeProvenance,
    marker: PhantomData<&'plan mut &'plan ()>,
}

impl<'dae> CallableCallProjectionSource<'_, 'dae> {
    pub const fn expression(self) -> ExprId<'dae> {
        self.expression
    }
    pub const fn output(self) -> u32 {
        self.output
    }
    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

impl<'dae> CallableConditionalSource<'_, 'dae> {
    pub const fn function(self) -> FunctionId<'dae> {
        self.function
    }
    pub const fn conditional(self) -> FunctionConditionalId<'dae> {
        self.conditional
    }
}

impl<'dae> CallableFoldSource<'_, 'dae> {
    pub const fn function(self) -> FunctionId<'dae> {
        self.function
    }
    pub const fn fold(self) -> FunctionFoldId<'dae> {
        self.fold
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallableFunctionSource<'plan, 'dae> {
    function: FunctionId<'dae>,
    provenance: DaeProvenance,
    marker: PhantomData<&'plan mut &'plan ()>,
}

impl<'dae> CallableFunctionSource<'_, 'dae> {
    pub const fn function(self) -> FunctionId<'dae> {
        self.function
    }
    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

#[derive(Clone, Copy)]
struct FunctionSource<'dae> {
    id: FunctionId<'dae>,
    provenance: DaeProvenance,
}

#[derive(Clone, Copy)]
struct ExpressionSource<'dae> {
    function: FunctionId<'dae>,
    id: ExprId<'dae>,
    provenance: DaeProvenance,
}

#[derive(Clone, Copy)]
struct DefinitionSource<'dae> {
    id: FunctionDefinitionId<'dae>,
    provenance: DaeProvenance,
}

#[derive(Clone, Copy)]
struct AssertionSource<'dae> {
    id: FunctionAssertionId<'dae>,
    condition: ExprId<'dae>,
    message: ExprId<'dae>,
    provenance: DaeProvenance,
}

#[derive(Clone)]
struct ConditionalSource<'dae> {
    id: FunctionConditionalId<'dae>,
    definitions: Box<[FunctionDefinitionId<'dae>]>,
    conditions: Box<[ExprId<'dae>]>,
    branches: Box<[Box<[ExprId<'dae>]>]>,
    fallback: Box<[ExprId<'dae>]>,
    provenance: DaeProvenance,
}

#[derive(Clone)]
struct CallSource<'dae> {
    id: FunctionCallId<'dae>,
    owner: ExprId<'dae>,
    projections: Box<[CallProjectionSource<'dae>]>,
    provenance: DaeProvenance,
}

#[derive(Clone, Copy)]
struct CallProjectionSource<'dae> {
    expression: ExprId<'dae>,
    output: u32,
    provenance: DaeProvenance,
}

#[derive(Clone, Copy)]
struct FoldSource<'dae> {
    id: FunctionFoldId<'dae>,
    provenance: DaeProvenance,
}
/// One plan value a caller claims reproduces an exact DAE operand of a
/// structured region, and the scope rule that operand position requires.
#[derive(Clone, Copy)]
struct RegionValueClaim<'plan, 'dae> {
    value: ConstructionValueId<'plan>,
    owner: u32,
    scope: u32,
    consumer: rumoca_ir_dae::ExpressionView<'dae>,
    operand_ordinal: usize,
    expected: ExprId<'dae>,
    /// Branch and fallback results must live in their own child scope; a
    /// condition only has to dominate the region parent.
    exact_scope: bool,
    span: Span,
}

/// The complete result claim for closing one conditional region.
#[derive(Clone, Copy)]
struct ConditionalCloseClaim<'a, 'plan> {
    conditions: &'a [ConstructionValueId<'plan>],
    branch_results: &'a [ConstructionValueId<'plan>],
    fallback_result: ConstructionValueId<'plan>,
}

struct PendingOwner {
    source_function: u32,
    root_scope: u32,
    interface: CallableInterface,
    declaration: DaeProvenance,
    callees: Vec<u32>,
    operations: Vec<u32>,
}

struct PendingScope {
    owner: u32,
    parent: Option<u32>,
    operations: Vec<u32>,
    open_children: usize,
    closed: bool,
}

/// Affine assembly over the exact inventory born with the retained DAE.
pub struct CallablePlanConstruction<'plan, 'dae> {
    dae: DaeView<'dae>,
    functions: Box<[FunctionSource<'dae>]>,
    expressions: Box<[ExpressionSource<'dae>]>,
    definitions: Box<[DefinitionSource<'dae>]>,
    assertions: Box<[AssertionSource<'dae>]>,
    conditionals: Box<[ConditionalSource<'dae>]>,
    calls: Box<[CallSource<'dae>]>,
    folds: Box<[FoldSource<'dae>]>,
    outstanding_functions: HashSet<FunctionId<'dae>>,
    outstanding_expressions: HashSet<(FunctionId<'dae>, ExprId<'dae>)>,
    outstanding_definitions: HashSet<FunctionDefinitionId<'dae>>,
    outstanding_assertions: HashSet<FunctionAssertionId<'dae>>,
    outstanding_conditionals: HashSet<FunctionConditionalId<'dae>>,
    outstanding_calls: HashSet<FunctionCallId<'dae>>,
    outstanding_folds: HashSet<FunctionFoldId<'dae>>,
    owners: Vec<PendingOwner>,
    scopes: Vec<PendingScope>,
    values: Vec<ValueEntry>,
    operations: Vec<OperationEntry>,
    regions: Vec<StructuredRegionEntry>,
    projections: Vec<ProjectionEntry>,
    effects: Vec<EffectEntry>,
    call_edges: Vec<AcyclicCallEdge>,
    conversion_edges: HashSet<(FunctionId<'dae>, ExprId<'dae>, usize)>,
    open_regions: HashSet<(FunctionId<'dae>, ExprId<'dae>)>,
    open_conditional_groups: HashSet<FunctionConditionalId<'dae>>,
    poison: Option<PlanConstructionError>,
    marker: PhantomData<&'plan mut &'plan ()>,
}

impl<'plan, 'dae> CallablePlanConstruction<'plan, 'dae> {
    fn new(dae: DaeView<'dae>, inventory: &CallableSourceInventoryView<'plan, 'dae>) -> Self {
        let functions = inventory
            .functions()
            .map(|item| FunctionSource {
                id: item.function(),
                provenance: item.provenance(),
            })
            .collect::<Box<[_]>>();
        let expressions = inventory
            .expressions()
            .map(|item| ExpressionSource {
                function: item.function(),
                id: item.expression(),
                provenance: item.provenance(),
            })
            .collect::<Box<[_]>>();
        let definitions = inventory
            .definitions()
            .map(|item| DefinitionSource {
                id: item.definition(),
                provenance: item.provenance(),
            })
            .collect::<Box<[_]>>();
        let assertions = inventory
            .assertions()
            .map(|item| AssertionSource {
                id: item.assertion(),
                condition: item.condition(),
                message: item.message(),
                provenance: item.provenance(),
            })
            .collect::<Box<[_]>>();
        let conditionals = inventory
            .conditionals()
            .map(|item| ConditionalSource {
                id: item.conditional(),
                definitions: item.definitions().collect(),
                conditions: item.conditions().collect(),
                branches: item.branches().map(Iterator::collect).collect(),
                fallback: item.fallback().collect(),
                provenance: item.provenance(),
            })
            .collect::<Box<[_]>>();
        let calls = inventory
            .calls()
            .map(|item| CallSource {
                id: item.call(),
                owner: item.owner(),
                projections: item
                    .projections()
                    .map(|projection| CallProjectionSource {
                        expression: projection.expression(),
                        output: projection.output(),
                        provenance: projection.provenance(),
                    })
                    .collect(),
                provenance: item.provenance(),
            })
            .collect::<Box<[_]>>();
        let folds = inventory
            .folds()
            .map(|item| FoldSource {
                id: item.fold(),
                provenance: item.provenance(),
            })
            .collect::<Box<[_]>>();
        Self {
            dae,
            outstanding_functions: functions.iter().map(|item| item.id).collect(),
            outstanding_expressions: expressions
                .iter()
                .map(|item| (item.function, item.id))
                .collect(),
            outstanding_definitions: definitions.iter().map(|item| item.id).collect(),
            outstanding_assertions: assertions.iter().map(|item| item.id).collect(),
            outstanding_conditionals: conditionals.iter().map(|item| item.id).collect(),
            outstanding_calls: calls.iter().map(|item| item.id).collect(),
            outstanding_folds: folds.iter().map(|item| item.id).collect(),
            functions,
            expressions,
            definitions,
            assertions,
            conditionals,
            calls,
            folds,
            owners: Vec::new(),
            scopes: Vec::new(),
            values: Vec::new(),
            operations: Vec::new(),
            regions: Vec::new(),
            projections: Vec::new(),
            effects: Vec::new(),
            call_edges: Vec::new(),
            conversion_edges: HashSet::new(),
            open_regions: HashSet::new(),
            open_conditional_groups: HashSet::new(),
            poison: None,
            marker: PhantomData,
        }
    }

    pub fn functions(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableFunctionSource<'plan, 'dae>> + '_ {
        self.functions.iter().map(|item| CallableFunctionSource {
            function: item.id,
            provenance: item.provenance,
            marker: PhantomData,
        })
    }

    pub fn expressions(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableExpressionSource<'plan, 'dae>> + '_ {
        self.expressions
            .iter()
            .map(|item| CallableExpressionSource {
                function: item.function,
                expression: item.id,
                provenance: item.provenance,
                marker: PhantomData,
            })
    }

    /// Exact value-operand edges owned by one issued consumer occurrence.
    pub fn operand_edges(
        &self,
        consumer: CallableExpressionSource<'plan, 'dae>,
    ) -> Result<std::vec::IntoIter<CallableOperandEdge<'plan, 'dae>>, PlanConstructionError> {
        if !self
            .expressions
            .iter()
            .any(|item| item.function == consumer.function && item.id == consumer.expression)
        {
            return Err(PlanConstructionError::InvalidSourceOccurrence {
                span: consumer.provenance.span(),
            });
        }
        let expression = self.dae.exact_expression(consumer.expression);
        Ok(direct_value_operands(expression.operation())
            .enumerate()
            .map(|(ordinal, operand)| CallableOperandEdge {
                function: consumer.function,
                consumer: consumer.expression,
                operand,
                ordinal,
                provenance: consumer.provenance,
                marker: PhantomData,
            })
            .collect::<Vec<_>>()
            .into_iter())
    }

    pub fn definitions(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableDefinitionSource<'plan, 'dae>> + '_ {
        self.definitions
            .iter()
            .map(|item| CallableDefinitionSource {
                function: item.id.function(),
                definition: item.id,
                provenance: item.provenance,
                marker: PhantomData,
            })
    }

    pub fn assertions(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableAssertionSource<'plan, 'dae>> + '_ {
        self.assertions.iter().map(|item| CallableAssertionSource {
            function: item.id.function(),
            assertion: item.id,
            provenance: item.provenance,
            marker: PhantomData,
        })
    }

    pub fn conditionals(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableConditionalSource<'plan, 'dae>> + '_ {
        self.conditionals
            .iter()
            .map(|item| CallableConditionalSource {
                function: item.id.function(),
                conditional: item.id,
                provenance: item.provenance,
                marker: PhantomData,
            })
    }

    pub fn calls(&self) -> impl ExactSizeIterator<Item = CallableCallSource<'plan, 'dae>> + '_ {
        self.calls.iter().map(|item| CallableCallSource {
            function: item.id.function(),
            call: item.id,
            provenance: item.provenance,
            marker: PhantomData,
        })
    }

    pub fn call_projections(
        &self,
        source: CallableCallSource<'plan, 'dae>,
    ) -> Result<std::vec::IntoIter<CallableCallProjectionSource<'plan, 'dae>>, PlanConstructionError>
    {
        let call = self
            .calls
            .iter()
            .find(|item| item.id == source.call && item.provenance == source.provenance)
            .ok_or(PlanConstructionError::InvalidSourceOccurrence {
                span: source.provenance.span(),
            })?;
        Ok(call
            .projections
            .iter()
            .map(|projection| CallableCallProjectionSource {
                expression: projection.expression,
                output: projection.output,
                provenance: projection.provenance,
                marker: PhantomData,
            })
            .collect::<Vec<_>>()
            .into_iter())
    }

    pub fn folds(&self) -> impl ExactSizeIterator<Item = CallableFoldSource<'plan, 'dae>> + '_ {
        self.folds.iter().map(|item| CallableFoldSource {
            function: item.id.function(),
            fold: item.id,
            provenance: item.provenance,
            marker: PhantomData,
        })
    }

    pub fn add_owner(
        &mut self,
        source: CallableFunctionSource<'plan, 'dae>,
    ) -> Result<ConstructionOwnerId<'plan>, PlanConstructionError> {
        self.transaction(|construction| construction.try_add_owner(source))
    }

    fn try_add_owner(
        &mut self,
        source: CallableFunctionSource<'plan, 'dae>,
    ) -> Result<ConstructionOwnerId<'plan>, PlanConstructionError> {
        let span = source.provenance.span();
        if !self.outstanding_functions.remove(&source.function) {
            return Err(PlanConstructionError::DuplicateOccurrence { span });
        }
        let function = self
            .dae
            .function(source.function)
            .ok_or(PlanConstructionError::InvalidSourceOccurrence { span })?;
        let interface = CallableInterface {
            parameters: function
                .parameter_types()
                .iter()
                .map(|id| callable_value_type(self.dae, id, span))
                .collect::<Result<_, _>>()?,
            results: function
                .result_types()
                .iter()
                .map(|id| callable_value_type(self.dae, id, span))
                .collect::<Result<_, _>>()?,
        };
        let raw = checked_id(self.owners.len(), span)?;
        let root_scope = checked_id(self.scopes.len(), span)?;
        self.scopes.push(PendingScope {
            owner: raw,
            parent: None,
            operations: Vec::new(),
            open_children: 0,
            closed: false,
        });
        self.owners.push(PendingOwner {
            source_function: source.function.index(),
            root_scope,
            interface,
            declaration: function.declaration(),
            callees: Vec::new(),
            operations: Vec::new(),
        });
        Ok(ConstructionOwnerId {
            raw,
            marker: PhantomData,
        })
    }

    pub fn root_scope(&self, owner: ConstructionOwnerId<'plan>) -> ConstructionScopeId<'plan> {
        ConstructionScopeId {
            raw: self.owners[owner.raw as usize].root_scope,
            marker: PhantomData,
        }
    }

    pub fn add_value_operation(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        source: CallableExpressionSource<'plan, 'dae>,
        contract: OperationContractKey,
        operands: &[ConstructionValueId<'plan>],
    ) -> Result<ConstructionValueId<'plan>, PlanConstructionError> {
        self.transaction(|construction| {
            construction.try_add_value_operation(scope, source, contract, operands)
        })
    }

    fn try_add_value_operation(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        source: CallableExpressionSource<'plan, 'dae>,
        contract: OperationContractKey,
        operands: &[ConstructionValueId<'plan>],
    ) -> Result<ConstructionValueId<'plan>, PlanConstructionError> {
        let span = source.provenance.span();
        let owner = self.scope_owner(scope, span)?;
        let expression = self.checked_expression(scope, source)?;
        if matches!(
            expression.operation(),
            ExpressionOperation::Call { .. }
                | ExpressionOperation::Conditional(_)
                | ExpressionOperation::Comprehension { .. }
                | ExpressionOperation::Index { .. }
                | ExpressionOperation::ArrayUpdate { .. }
                | ExpressionOperation::Range(_)
                | ExpressionOperation::Builtin {
                    builtin: rumoca_ir_dae::PureBuiltin::Identity,
                    ..
                }
        ) {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        if authenticate_contract(self.dae, expression)? != contract {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        self.check_operands(
            scope,
            operands,
            expression,
            expected_operands(expression.operation(), contract),
            span,
        )?;
        self.claim_expression(source)?;
        let operation = checked_id(self.operations.len(), span)?;
        let value = checked_id(self.values.len(), span)?;
        self.operations.push(OperationEntry {
            owner,
            scope: scope.raw,
            source: OperationSource::Expression(source.expression.index()),
            contract,
            operands: operands.iter().map(|item| item.raw).collect(),
            results: Box::new([value]),
            detail: CallableOperationDetail::Plain,
            provenance: source.provenance,
        });
        self.values.push(ValueEntry {
            owner,
            scope: scope.raw,
            source_expression: source.expression.index(),
            value_type: callable_value_type(self.dae, expression.value_type_id(), span)?,
            producer: ValueProducer::Operation(operation),
        });
        self.record_operation(scope, operation, span)?;
        Ok(ConstructionValueId {
            raw: value,
            marker: PhantomData,
        })
    }

    pub fn open_conditional(
        &mut self,
        parent: ConstructionScopeId<'plan>,
        source: CallableExpressionSource<'plan, 'dae>,
    ) -> Result<ConditionalRegionConstruction<'plan, 'dae>, PlanConstructionError> {
        self.transaction(|construction| construction.try_open_conditional(parent, source))
    }

    fn try_open_conditional(
        &mut self,
        parent: ConstructionScopeId<'plan>,
        source: CallableExpressionSource<'plan, 'dae>,
    ) -> Result<ConditionalRegionConstruction<'plan, 'dae>, PlanConstructionError> {
        let span = source.provenance.span();
        let parent_owner = self.scope_owner(parent, span)?;
        let expression = self.checked_expression(parent, source)?;
        let ExpressionOperation::Conditional(operands) = expression.operation() else {
            return Err(PlanConstructionError::InvalidOperation { span });
        };
        if self.conditionals.iter().any(|group| {
            group.definitions.iter().any(|definition| {
                self.dae.exact_function_definition(*definition).rhs() == source.expression
            })
        }) {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        if operands.len() < 3 || operands.len() % 2 == 0 {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        if !self
            .open_regions
            .insert((source.function, source.expression))
        {
            return Err(PlanConstructionError::DuplicateOccurrence { span });
        }
        let branch_count = (operands.len() - 1) / 2;
        let child_count = branch_count
            .checked_add(1)
            .ok_or(PlanConstructionError::IdentityOverflow { span })?;
        let last = self
            .scopes
            .len()
            .checked_add(child_count - 1)
            .ok_or(PlanConstructionError::IdentityOverflow { span })?;
        checked_id(last, span)?;
        let first = self.scopes.len();
        let branches = (0..branch_count)
            .map(|ordinal| ConstructionScopeId {
                raw: (first + ordinal) as u32,
                marker: PhantomData,
            })
            .collect::<Box<[_]>>();
        let fallback = ConstructionScopeId {
            raw: (first + branch_count) as u32,
            marker: PhantomData,
        };
        let parent_open_children = self.scopes[parent.raw as usize]
            .open_children
            .checked_add(child_count)
            .ok_or(PlanConstructionError::IdentityOverflow { span })?;
        self.scopes.extend((0..child_count).map(|_| PendingScope {
            owner: parent_owner,
            parent: Some(parent.raw),
            operations: Vec::new(),
            open_children: 0,
            closed: false,
        }));
        self.scopes[parent.raw as usize].open_children = parent_open_children;
        Ok(ConditionalRegionConstruction {
            source,
            parent,
            branches,
            fallback,
        })
    }

    pub fn close_conditional(
        &mut self,
        region: ConditionalRegionConstruction<'plan, 'dae>,
        conditions: &[ConstructionValueId<'plan>],
        branch_results: &[ConstructionValueId<'plan>],
        fallback_result: ConstructionValueId<'plan>,
    ) -> Result<ConstructionValueId<'plan>, PlanConstructionError> {
        self.transaction(|construction| {
            construction.try_close_conditional(region, conditions, branch_results, fallback_result)
        })
    }

    fn try_close_conditional(
        &mut self,
        region: ConditionalRegionConstruction<'plan, 'dae>,
        conditions: &[ConstructionValueId<'plan>],
        branch_results: &[ConstructionValueId<'plan>],
        fallback_result: ConstructionValueId<'plan>,
    ) -> Result<ConstructionValueId<'plan>, PlanConstructionError> {
        let claim = ConditionalCloseClaim {
            conditions,
            branch_results,
            fallback_result,
        };
        let expression = self.authenticate_conditional_close(&region, claim)?;
        let captures = self.conditional_close_captures(&region, conditions)?;
        self.record_conditional_close(&region, claim, expression, captures)
    }

    /// Authenticate that the claimed values reproduce the exact operands of the
    /// open conditional expression, in its exact scopes.
    fn authenticate_conditional_close(
        &mut self,
        region: &ConditionalRegionConstruction<'plan, 'dae>,
        claim: ConditionalCloseClaim<'_, 'plan>,
    ) -> Result<rumoca_ir_dae::ExpressionView<'dae>, PlanConstructionError> {
        let span = region.source.provenance.span();
        let parent = self.scope_allow_open(region.parent, span)?;
        let owner = parent.owner;
        if !self
            .open_regions
            .contains(&(region.source.function, region.source.expression))
            || claim.conditions.len() != region.branches.len()
            || claim.branch_results.len() != region.branches.len()
        {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        let expression = self.checked_open_region_expression(owner, region.source)?;
        let ExpressionOperation::Conditional(operands) = expression.operation() else {
            return Err(PlanConstructionError::InvalidOperation { span });
        };
        let expected = operands.iter().collect::<Vec<_>>();
        if expected.len() != claim.conditions.len() * 2 + 1 {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        for (ordinal, condition) in claim.conditions.iter().enumerate() {
            self.check_region_value(RegionValueClaim {
                value: *condition,
                owner,
                scope: region.parent.raw,
                consumer: expression,
                operand_ordinal: ordinal * 2,
                expected: expected[ordinal * 2],
                exact_scope: false,
                span,
            })?;
            self.check_region_value(RegionValueClaim {
                value: claim.branch_results[ordinal],
                owner,
                scope: region.branches[ordinal].raw,
                consumer: expression,
                operand_ordinal: ordinal * 2 + 1,
                expected: expected[ordinal * 2 + 1],
                exact_scope: true,
                span,
            })?;
        }
        self.check_region_value(RegionValueClaim {
            value: claim.fallback_result,
            owner,
            scope: region.fallback.raw,
            consumer: expression,
            operand_ordinal: expected.len() - 1,
            expected: *expected
                .last()
                .ok_or(PlanConstructionError::InvalidOperation { span })?,
            exact_scope: true,
            span,
        })?;
        for child in region.child_scopes() {
            let child = self.scope_allow_open(child, span)?;
            if child.parent != Some(region.parent.raw) || child.open_children != 0 || child.closed {
                return Err(PlanConstructionError::InvalidOperation { span });
            }
        }
        Ok(expression)
    }

    /// Values the region body reads from outside itself, minus the conditions,
    /// which are already explicit operands of the region operation.
    fn conditional_close_captures(
        &self,
        region: &ConditionalRegionConstruction<'plan, 'dae>,
        conditions: &[ConstructionValueId<'plan>],
    ) -> Result<BTreeSet<u32>, PlanConstructionError> {
        let span = region.source.provenance.span();
        let mut captures = BTreeSet::new();
        for child in region.child_scopes() {
            self.collect_region_captures(child.raw, &mut captures, span)?;
        }
        for condition in conditions {
            captures.remove(&condition.raw);
        }
        Ok(captures)
    }

    fn record_conditional_close(
        &mut self,
        region: &ConditionalRegionConstruction<'plan, 'dae>,
        claim: ConditionalCloseClaim<'_, 'plan>,
        expression: rumoca_ir_dae::ExpressionView<'dae>,
        captures: BTreeSet<u32>,
    ) -> Result<ConstructionValueId<'plan>, PlanConstructionError> {
        let span = region.source.provenance.span();
        let owner = self.scope_allow_open(region.parent, span)?.owner;
        let child_count = region
            .branches
            .len()
            .checked_add(1)
            .ok_or(PlanConstructionError::IdentityOverflow { span })?;
        let remaining_open_children = self.scopes[region.parent.raw as usize]
            .open_children
            .checked_sub(child_count)
            .ok_or(PlanConstructionError::InvalidOperation { span })?;
        let operation = checked_id(self.operations.len(), span)?;
        let value = checked_id(self.values.len(), span)?;
        self.claim_expression(region.source)?;
        let operation_operands: Vec<u32> = claim
            .conditions
            .iter()
            .map(|value| value.raw)
            .chain(captures.iter().copied())
            .collect();
        self.operations.push(OperationEntry {
            owner,
            scope: region.parent.raw,
            source: OperationSource::Expression(region.source.expression.index()),
            contract: OperationContractKey::Conditional,
            operands: operation_operands.into_boxed_slice(),
            results: Box::new([value]),
            detail: CallableOperationDetail::Plain,
            provenance: region.source.provenance,
        });
        self.values.push(ValueEntry {
            owner,
            scope: region.parent.raw,
            source_expression: region.source.expression.index(),
            value_type: callable_value_type(self.dae, expression.value_type_id(), span)?,
            producer: ValueProducer::Operation(operation),
        });
        self.regions.push(StructuredRegionEntry {
            owner,
            operation,
            contract: OperationContractKey::Conditional,
            captures: captures.into_iter().collect(),
            results: Box::new([value]),
            detail: StructuredRegionDetail::Conditional {
                branch_scopes: region.branches.iter().map(|scope| scope.raw).collect(),
                fallback_scope: region.fallback.raw,
                conditions: claim.conditions.iter().map(|value| value.raw).collect(),
                branch_results: claim
                    .branch_results
                    .iter()
                    .map(|value| vec![value.raw].into_boxed_slice())
                    .collect(),
                fallback_results: Box::new([claim.fallback_result.raw]),
            },
            domain: None,
            provenance: region.source.provenance,
        });
        for child in region.child_scopes() {
            self.scopes[child.raw as usize].closed = true;
        }
        self.scopes[region.parent.raw as usize].open_children = remaining_open_children;
        self.scopes[region.parent.raw as usize]
            .operations
            .push(operation);
        self.owners[owner as usize].operations.push(operation);
        self.open_regions
            .remove(&(region.source.function, region.source.expression));
        Ok(ConstructionValueId {
            raw: value,
            marker: PhantomData,
        })
    }

    pub fn open_map(
        &mut self,
        parent: ConstructionScopeId<'plan>,
        source: CallableExpressionSource<'plan, 'dae>,
    ) -> Result<MapRegionConstruction<'plan, 'dae>, PlanConstructionError> {
        self.transaction(|construction| construction.try_open_map(parent, source))
    }

    fn try_open_map(
        &mut self,
        parent: ConstructionScopeId<'plan>,
        source: CallableExpressionSource<'plan, 'dae>,
    ) -> Result<MapRegionConstruction<'plan, 'dae>, PlanConstructionError> {
        let span = source.provenance.span();
        let owner = self.scope_owner(parent, span)?;
        let expression = self.checked_expression(parent, source)?;
        if !matches!(
            expression.operation(),
            ExpressionOperation::Comprehension { .. }
        ) || !self
            .open_regions
            .insert((source.function, source.expression))
        {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        let raw = checked_id(self.scopes.len(), span)?;
        self.scopes.push(PendingScope {
            owner,
            parent: Some(parent.raw),
            operations: Vec::new(),
            open_children: 0,
            closed: false,
        });
        self.scopes[parent.raw as usize].open_children = self.scopes[parent.raw as usize]
            .open_children
            .checked_add(1)
            .ok_or(PlanConstructionError::IdentityOverflow { span })?;
        Ok(MapRegionConstruction {
            source,
            parent,
            body: ConstructionScopeId {
                raw,
                marker: PhantomData,
            },
        })
    }

    pub fn close_map(
        &mut self,
        region: MapRegionConstruction<'plan, 'dae>,
        body_result: ConstructionValueId<'plan>,
    ) -> Result<ConstructionValueId<'plan>, PlanConstructionError> {
        self.transaction(|construction| construction.try_close_map(region, body_result))
    }

    fn try_close_map(
        &mut self,
        region: MapRegionConstruction<'plan, 'dae>,
        body_result: ConstructionValueId<'plan>,
    ) -> Result<ConstructionValueId<'plan>, PlanConstructionError> {
        let span = region.source.provenance.span();
        let parent = self.scope_allow_open(region.parent, span)?;
        let owner = parent.owner;
        let child = self.scope_allow_open(region.body, span)?;
        if child.parent != Some(region.parent.raw)
            || child.open_children != 0
            || child.closed
            || !self
                .open_regions
                .contains(&(region.source.function, region.source.expression))
        {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        let expression = self.checked_open_region_expression(owner, region.source)?;
        let ExpressionOperation::Comprehension { domain, body } = expression.operation() else {
            return Err(PlanConstructionError::InvalidOperation { span });
        };
        self.check_region_value(RegionValueClaim {
            value: body_result,
            owner,
            scope: region.body.raw,
            consumer: expression,
            operand_ordinal: 0,
            expected: body,
            exact_scope: true,
            span,
        })?;
        let mut captures = BTreeSet::new();
        self.collect_region_captures(region.body.raw, &mut captures, span)?;
        let remaining_open_children = self.scopes[region.parent.raw as usize]
            .open_children
            .checked_sub(1)
            .ok_or(PlanConstructionError::InvalidOperation { span })?;
        let compact = compact_domain(self.dae, domain, span)?;
        let operation = checked_id(self.operations.len(), span)?;
        let value = checked_id(self.values.len(), span)?;
        let value_type = callable_value_type(self.dae, expression.value_type_id(), span)?;
        self.claim_expression(region.source)?;
        self.operations.push(OperationEntry {
            owner,
            scope: region.parent.raw,
            source: OperationSource::Expression(region.source.expression.index()),
            contract: OperationContractKey::Map,
            operands: captures.iter().copied().collect(),
            results: Box::new([value]),
            detail: CallableOperationDetail::Map(compact.clone()),
            provenance: region.source.provenance,
        });
        self.values.push(ValueEntry {
            owner,
            scope: region.parent.raw,
            source_expression: region.source.expression.index(),
            value_type,
            producer: ValueProducer::Operation(operation),
        });
        self.regions.push(StructuredRegionEntry {
            owner,
            operation,
            contract: OperationContractKey::Map,
            captures: captures.into_iter().collect(),
            results: Box::new([value]),
            detail: StructuredRegionDetail::Map {
                body_scope: region.body.raw,
                body_result: body_result.raw,
            },
            domain: Some(compact),
            provenance: region.source.provenance,
        });
        self.scopes[region.body.raw as usize].closed = true;
        self.scopes[region.parent.raw as usize].open_children = remaining_open_children;
        self.scopes[region.parent.raw as usize]
            .operations
            .push(operation);
        self.owners[owner as usize].operations.push(operation);
        self.open_regions
            .remove(&(region.source.function, region.source.expression));
        Ok(ConstructionValueId {
            raw: value,
            marker: PhantomData,
        })
    }

    pub fn add_store(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        source: CallableDefinitionSource<'plan, 'dae>,
        value: ConstructionValueId<'plan>,
    ) -> Result<(), PlanConstructionError> {
        self.transaction(|construction| construction.try_add_store(scope, source, value))
    }

    fn try_add_store(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        source: CallableDefinitionSource<'plan, 'dae>,
        value: ConstructionValueId<'plan>,
    ) -> Result<(), PlanConstructionError> {
        let span = source.provenance.span();
        let definition = self
            .dae
            .function_definition(source.definition)
            .ok_or(PlanConstructionError::InvalidSourceOccurrence { span })?;
        let value_entry = self
            .values
            .get(value.raw as usize)
            .ok_or(PlanConstructionError::InvalidOperation { span })?;
        let owner = self.scope_owner(scope, span)?;
        if source.function != self.scope_owner_function(scope, span)?
            || value_entry.owner != owner
            || !self.scope_dominates(value_entry.scope, scope.raw)
            || value_entry.source_expression != definition.rhs().index()
        {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        if !self.outstanding_definitions.remove(&source.definition) {
            return Err(PlanConstructionError::DuplicateOccurrence { span });
        }
        let operation = checked_id(self.operations.len(), span)?;
        self.operations.push(OperationEntry {
            owner,
            scope: scope.raw,
            source: OperationSource::Definition(source.definition.ordinal()),
            contract: OperationContractKey::Store,
            operands: Box::new([value.raw]),
            results: Box::new([]),
            detail: CallableOperationDetail::Plain,
            provenance: source.provenance,
        });
        self.record_operation(scope, operation, span)?;
        Ok(())
    }

    /// Insert the one proof-directed numeric promotion required by a typed
    /// parent operation. The source literal/load occurrence remains consumed
    /// exactly once by its own producer; this derived operation cites that
    /// exact occurrence without reopening its obligation.
    pub fn promote_integer_to_real(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        edge: CallableOperandEdge<'plan, 'dae>,
        operand: ConstructionValueId<'plan>,
    ) -> Result<ConstructionValueId<'plan>, PlanConstructionError> {
        self.transaction(|construction| {
            construction.try_promote_integer_to_real(scope, edge, operand)
        })
    }

    fn try_promote_integer_to_real(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        edge: CallableOperandEdge<'plan, 'dae>,
        operand: ConstructionValueId<'plan>,
    ) -> Result<ConstructionValueId<'plan>, PlanConstructionError> {
        let span = edge.provenance.span();
        let owner = self.scope_owner(scope, span)?;
        if edge.function != self.scope_owner_function(scope, span)?
            || !self.expressions.iter().any(|item| {
                item.function == edge.function
                    && item.id == edge.consumer
                    && item.provenance == edge.provenance
            })
        {
            return Err(PlanConstructionError::InvalidSourceOccurrence { span });
        }
        let consumer = self.dae.exact_expression(edge.consumer);
        let expected = direct_value_operands(consumer.operation())
            .nth(edge.ordinal)
            .ok_or(PlanConstructionError::InvalidOperation { span })?;
        if expected != edge.operand
            || conversion_target_type(self.dae, consumer, edge.ordinal, span)?.scalar_type()
                != ScalarType::Real
            || !self
                .conversion_edges
                .insert((edge.function, edge.consumer, edge.ordinal))
        {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        let operand_entry = self
            .values
            .get(operand.raw as usize)
            .ok_or(PlanConstructionError::InvalidOperation { span })?;
        if operand_entry.owner != owner
            || !self.scope_dominates(operand_entry.scope, scope.raw)
            || operand_entry.source_expression != edge.operand.index()
            || operand_entry.value_type.scalar != CallableScalarType::Integer
        {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        let operation = checked_id(self.operations.len(), span)?;
        let value = checked_id(self.values.len(), span)?;
        let value_type = CallableValueType {
            scalar: CallableScalarType::Real,
            dimensions: operand_entry.value_type.dimensions.clone(),
        };
        let source_fact = match self.dae.exact_expression(edge.operand).operation() {
            ExpressionOperation::Literal(rumoca_ir_dae::DaeLiteral::Integer(value)) => {
                CallableIntegerSourceFact::ExactLiteral(*value)
            }
            _ => CallableIntegerSourceFact::FullDomain,
        };
        self.operations.push(OperationEntry {
            owner,
            scope: scope.raw,
            source: OperationSource::DerivedExpression(edge.consumer.index()),
            contract: OperationContractKey::ConvertIntegerToReal,
            operands: Box::new([operand.raw]),
            results: Box::new([value]),
            detail: CallableOperationDetail::IntegerToReal { source_fact },
            provenance: edge.provenance,
        });
        self.values.push(ValueEntry {
            owner,
            scope: scope.raw,
            source_expression: edge.operand.index(),
            value_type,
            producer: ValueProducer::Operation(operation),
        });
        self.record_operation(scope, operation, span)?;
        Ok(ConstructionValueId {
            raw: value,
            marker: PhantomData,
        })
    }

    pub fn add_call(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        source: CallableCallSource<'plan, 'dae>,
        arguments: &[ConstructionValueId<'plan>],
    ) -> Result<Box<[ConstructionValueId<'plan>]>, PlanConstructionError> {
        self.transaction(|construction| construction.try_add_call(scope, source, arguments))
    }

    fn try_add_call(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        source: CallableCallSource<'plan, 'dae>,
        arguments: &[ConstructionValueId<'plan>],
    ) -> Result<Box<[ConstructionValueId<'plan>]>, PlanConstructionError> {
        let call = self.authenticate_call_occurrence(scope, source, arguments)?;
        let callee = self.authenticated_callee(&call, source.provenance.span())?;
        self.record_call_occurrence(scope, source, arguments, &call, callee)
    }

    /// Authenticate that this call occurrence belongs to the scope's owner,
    /// is still outstanding, and that its owner expression and every requested
    /// output projection are the exact DAE call the inventory issued.
    fn authenticate_call_occurrence(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        source: CallableCallSource<'plan, 'dae>,
        arguments: &[ConstructionValueId<'plan>],
    ) -> Result<CallSource<'dae>, PlanConstructionError> {
        let span = source.provenance.span();
        if source.function != self.scope_owner_function(scope, span)?
            || !self.outstanding_calls.contains(&source.call)
        {
            return Err(PlanConstructionError::InvalidSourceOccurrence { span });
        }
        let call = self
            .calls
            .iter()
            .find(|item| item.id == source.call && item.provenance == source.provenance)
            .cloned()
            .ok_or(PlanConstructionError::InvalidSourceOccurrence { span })?;
        if call.projections.is_empty()
            || !self
                .outstanding_expressions
                .contains(&(source.function, call.owner))
        {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        let expression = self.dae.exact_expression(call.owner);
        let ExpressionOperation::Call {
            owner: call_owner,
            function,
            output: _,
            arguments: expected,
        } = expression.operation()
        else {
            return Err(PlanConstructionError::InvalidOperation { span });
        };
        if call_owner != call.owner {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        self.check_operands(scope, arguments, expression, expected.iter(), span)?;
        self.authenticate_call_projections(source, &call, function, expected)?;
        Ok(call)
    }

    /// Every requested output projection must be a distinct, strictly
    /// increasing, still-outstanding DAE expression that names the same
    /// callee, the same call owner, and the same argument list.
    fn authenticate_call_projections(
        &self,
        source: CallableCallSource<'plan, 'dae>,
        call: &CallSource<'dae>,
        function: FunctionId<'dae>,
        expected: rumoca_ir_dae::ExpressionOperands<'dae>,
    ) -> Result<(), PlanConstructionError> {
        let span = source.provenance.span();
        let mut previous_expression = None;
        for projection in &call.projections {
            let projection_span = projection.provenance.span();
            if previous_expression.is_some_and(|previous| previous >= projection.expression.index())
                || !self
                    .outstanding_expressions
                    .contains(&(source.function, projection.expression))
            {
                return Err(PlanConstructionError::InvalidOperation {
                    span: projection_span,
                });
            }
            let projection_expression = self.dae.exact_expression(projection.expression);
            let ExpressionOperation::Call {
                owner: projection_owner,
                function: projection_function,
                output: projection_output,
                arguments: projection_arguments,
            } = projection_expression.operation()
            else {
                return Err(PlanConstructionError::InvalidOperation {
                    span: projection_span,
                });
            };
            if projection_expression.provenance() != projection.provenance
                || projection_owner != call.owner
                || projection_function != function
                || projection_output != projection.output
                || !projection_arguments.iter().eq(expected.iter())
                || !self.expressions.iter().any(|item| {
                    item.function == source.function
                        && item.id == projection.expression
                        && item.provenance == projection.provenance
                })
            {
                return Err(PlanConstructionError::InvalidOperation {
                    span: projection_span,
                });
            }
            callable_value_type(self.dae, projection_expression.value_type_id(), span)?;
            previous_expression = Some(projection.expression.index());
        }
        Ok(())
    }

    /// The plan owner already added for the callee this occurrence names.
    fn authenticated_callee(
        &self,
        call: &CallSource<'dae>,
        span: Span,
    ) -> Result<u32, PlanConstructionError> {
        let ExpressionOperation::Call { function, .. } =
            self.dae.exact_expression(call.owner).operation()
        else {
            return Err(PlanConstructionError::InvalidOperation { span });
        };
        self.owners
            .iter()
            .position(|item| item.source_function == function.index())
            .and_then(|index| u32::try_from(index).ok())
            .ok_or(PlanConstructionError::InvalidOwner { span })
    }

    fn record_call_occurrence(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        source: CallableCallSource<'plan, 'dae>,
        arguments: &[ConstructionValueId<'plan>],
        call: &CallSource<'dae>,
        callee: u32,
    ) -> Result<Box<[ConstructionValueId<'plan>]>, PlanConstructionError> {
        let span = source.provenance.span();
        let owner = self.scope_owner(scope, span)?;
        let expression = self.dae.exact_expression(call.owner);
        let operation = checked_id(self.operations.len(), span)?;
        let last_projection = self
            .projections
            .len()
            .checked_add(call.projections.len() - 1)
            .ok_or(PlanConstructionError::IdentityOverflow { span })?;
        checked_id(last_projection, span)?;
        let last_value = self
            .values
            .len()
            .checked_add(call.projections.len() - 1)
            .ok_or(PlanConstructionError::IdentityOverflow { span })?;
        checked_id(last_value, span)?;
        if !self.outstanding_calls.remove(&source.call) {
            return Err(PlanConstructionError::DuplicateOccurrence { span });
        }
        let first = call
            .projections
            .first()
            .ok_or(PlanConstructionError::InvalidOperation { span })?;
        if first.expression != call.owner {
            self.claim_expression(CallableExpressionSource {
                function: source.function,
                expression: call.owner,
                provenance: expression.provenance(),
                marker: PhantomData,
            })?;
        }
        for projection in &call.projections {
            self.claim_expression(CallableExpressionSource {
                function: source.function,
                expression: projection.expression,
                provenance: projection.provenance,
                marker: PhantomData,
            })?;
        }
        self.operations.push(OperationEntry {
            owner,
            scope: scope.raw,
            source: OperationSource::CallOccurrence {
                call: call.id.ordinal(),
                owner: call.owner.index(),
                projections: call
                    .projections
                    .iter()
                    .map(|projection| CallProjectionSourceEntry {
                        expression: projection.expression.index(),
                        output: projection.output,
                    })
                    .collect(),
            },
            contract: OperationContractKey::Call,
            operands: arguments.iter().map(|item| item.raw).collect(),
            results: Box::new([]),
            detail: CallableOperationDetail::Call { callee },
            provenance: source.provenance,
        });
        self.record_operation(scope, operation, span)?;
        if !self.owners[owner as usize].callees.contains(&callee) {
            self.owners[owner as usize].callees.push(callee);
        }
        self.call_edges.push(AcyclicCallEdge {
            caller: owner,
            callee,
            call_operation: operation,
        });
        call.projections
            .iter()
            .map(|projection| {
                self.push_projection(
                    scope,
                    CallableExpressionSource {
                        function: source.function,
                        expression: projection.expression,
                        provenance: projection.provenance,
                        marker: PhantomData,
                    },
                    operation,
                    self.dae.exact_expression(projection.expression),
                )
            })
            .collect()
    }

    pub fn add_assertion(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        source: CallableAssertionSource<'plan, 'dae>,
        predicate: ConstructionValueId<'plan>,
        message: CallableExpressionSource<'plan, 'dae>,
    ) -> Result<(), PlanConstructionError> {
        self.transaction(|construction| {
            construction.try_add_assertion(scope, source, predicate, message)
        })
    }

    fn try_add_assertion(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        source: CallableAssertionSource<'plan, 'dae>,
        predicate: ConstructionValueId<'plan>,
        message: CallableExpressionSource<'plan, 'dae>,
    ) -> Result<(), PlanConstructionError> {
        let span = source.provenance.span();
        let data = self
            .assertions
            .iter()
            .find(|item| item.id == source.assertion)
            .copied()
            .ok_or(PlanConstructionError::InvalidSourceOccurrence { span })?;
        let predicate_entry = self
            .values
            .get(predicate.raw as usize)
            .ok_or(PlanConstructionError::InvalidOperation { span })?;
        let owner = self.scope_owner(scope, span)?;
        if source.function != self.scope_owner_function(scope, span)?
            || predicate_entry.owner != owner
            || !self.scope_dominates(predicate_entry.scope, scope.raw)
            || predicate_entry.source_expression != data.condition.index()
            || message.function != source.function
            || message.expression != data.message
        {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        if !self.outstanding_assertions.remove(&source.assertion) {
            return Err(PlanConstructionError::DuplicateOccurrence { span });
        }
        let message_expression = self.checked_expression(scope, message)?;
        if !matches!(
            message_expression.operation(),
            ExpressionOperation::Literal(rumoca_ir_dae::DaeLiteral::String(_))
        ) {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        self.claim_expression(message)?;
        self.effects.push(EffectEntry {
            owner,
            condition_expression: data.condition.index(),
            predicate: predicate.raw,
            message_expression: data.message.index(),
            provenance: data.provenance,
        });
        Ok(())
    }

    fn checked_expression(
        &self,
        scope: ConstructionScopeId<'plan>,
        source: CallableExpressionSource<'plan, 'dae>,
    ) -> Result<rumoca_ir_dae::ExpressionView<'dae>, PlanConstructionError> {
        let span = source.provenance.span();
        if source.function != self.scope_owner_function(scope, span)?
            || !self
                .outstanding_expressions
                .contains(&(source.function, source.expression))
        {
            return Err(PlanConstructionError::InvalidSourceOccurrence { span });
        }
        self.dae
            .expression(source.expression)
            .ok_or(PlanConstructionError::InvalidSourceOccurrence { span })
    }

    fn claim_expression(
        &mut self,
        source: CallableExpressionSource<'plan, 'dae>,
    ) -> Result<(), PlanConstructionError> {
        if !self
            .outstanding_expressions
            .remove(&(source.function, source.expression))
        {
            return Err(PlanConstructionError::DuplicateOccurrence {
                span: source.provenance.span(),
            });
        }
        Ok(())
    }

    fn check_operands(
        &self,
        scope: ConstructionScopeId<'plan>,
        operands: &[ConstructionValueId<'plan>],
        consumer: rumoca_ir_dae::ExpressionView<'dae>,
        expected: impl IntoIterator<Item = ExprId<'dae>>,
        span: Span,
    ) -> Result<(), PlanConstructionError> {
        let expected = expected.into_iter().collect::<Vec<_>>();
        if operands.len() != expected.len() {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        for (ordinal, (operand, expected)) in operands.iter().zip(expected).enumerate() {
            let Some(value) = self.values.get(operand.raw as usize) else {
                return Err(PlanConstructionError::InvalidOperation { span });
            };
            let expected_type =
                expected_operand_value_type(self.dae, consumer, ordinal, expected, span)?;
            if value.owner != self.scope_owner(scope, span)?
                || !self.scope_dominates(value.scope, scope.raw)
                || value.source_expression != expected.index()
                || value.value_type != expected_type
            {
                return Err(PlanConstructionError::InvalidOperation { span });
            }
        }
        Ok(())
    }

    fn push_projection(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        source: CallableExpressionSource<'plan, 'dae>,
        operation: u32,
        expression: rumoca_ir_dae::ExpressionView<'dae>,
    ) -> Result<ConstructionValueId<'plan>, PlanConstructionError> {
        let span = source.provenance.span();
        let owner = self.scope_owner(scope, span)?;
        let projection = checked_id(self.projections.len(), span)?;
        let value = checked_id(self.values.len(), span)?;
        self.projections.push(ProjectionEntry {
            owner,
            source_expression: source.expression.index(),
            source_operation: operation,
            value,
            provenance: source.provenance,
        });
        self.values.push(ValueEntry {
            owner,
            scope: scope.raw,
            source_expression: source.expression.index(),
            value_type: callable_value_type(self.dae, expression.value_type_id(), span)?,
            producer: ValueProducer::Projection(projection),
        });
        let call = self
            .operations
            .get_mut(operation as usize)
            .ok_or(PlanConstructionError::InvalidOperation { span })?;
        let mut results = call.results.to_vec();
        results.push(value);
        call.results = results.into_boxed_slice();
        Ok(ConstructionValueId {
            raw: value,
            marker: PhantomData,
        })
    }

    fn owner_function(
        &self,
        owner: ConstructionOwnerId<'plan>,
        span: Span,
    ) -> Result<FunctionId<'dae>, PlanConstructionError> {
        let raw = self
            .owners
            .get(owner.raw as usize)
            .ok_or(PlanConstructionError::InvalidOwner { span })?
            .source_function;
        self.functions
            .iter()
            .find(|item| item.id.index() == raw)
            .map(|item| item.id)
            .ok_or(PlanConstructionError::InvalidOwner { span })
    }

    fn scope(
        &self,
        scope: ConstructionScopeId<'plan>,
        span: Span,
    ) -> Result<&PendingScope, PlanConstructionError> {
        let scope = self
            .scopes
            .get(scope.raw as usize)
            .ok_or(PlanConstructionError::InvalidOwner { span })?;
        if scope.closed || scope.open_children != 0 {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        Ok(scope)
    }

    fn scope_allow_open(
        &self,
        scope: ConstructionScopeId<'plan>,
        span: Span,
    ) -> Result<&PendingScope, PlanConstructionError> {
        self.scopes
            .get(scope.raw as usize)
            .ok_or(PlanConstructionError::InvalidOwner { span })
    }

    fn checked_open_region_expression(
        &self,
        owner: u32,
        source: CallableExpressionSource<'plan, 'dae>,
    ) -> Result<rumoca_ir_dae::ExpressionView<'dae>, PlanConstructionError> {
        let span = source.provenance.span();
        let owner_function = self.owner_function(
            ConstructionOwnerId {
                raw: owner,
                marker: PhantomData,
            },
            span,
        )?;
        if source.function != owner_function
            || !self
                .outstanding_expressions
                .contains(&(source.function, source.expression))
        {
            return Err(PlanConstructionError::InvalidSourceOccurrence { span });
        }
        self.dae
            .expression(source.expression)
            .ok_or(PlanConstructionError::InvalidSourceOccurrence { span })
    }

    fn check_region_value(
        &self,
        claim: RegionValueClaim<'plan, 'dae>,
    ) -> Result<(), PlanConstructionError> {
        let span = claim.span;
        let value = self
            .values
            .get(claim.value.raw as usize)
            .ok_or(PlanConstructionError::InvalidOperation { span })?;
        let scope_matches = if claim.exact_scope {
            value.scope == claim.scope
        } else {
            self.scope_dominates(value.scope, claim.scope)
        };
        let expected_type = expected_operand_value_type(
            self.dae,
            claim.consumer,
            claim.operand_ordinal,
            claim.expected,
            span,
        )?;
        if value.owner != claim.owner
            || !scope_matches
            || value.source_expression != claim.expected.index()
            || value.value_type != expected_type
        {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        Ok(())
    }

    fn collect_region_captures(
        &self,
        region_scope: u32,
        captures: &mut BTreeSet<u32>,
        span: Span,
    ) -> Result<(), PlanConstructionError> {
        for (scope_raw, scope) in self.scopes.iter().enumerate() {
            let scope_raw = checked_id(scope_raw, span)?;
            if !self.scope_dominates(region_scope, scope_raw) {
                continue;
            }
            for operation in &scope.operations {
                let operation = self
                    .operations
                    .get(*operation as usize)
                    .ok_or(PlanConstructionError::InvalidOperation { span })?;
                self.collect_operand_captures(region_scope, scope_raw, operation, captures, span)?;
            }
        }
        Ok(())
    }

    /// Every operand one operation reads must be dominated by its own scope;
    /// the ones produced outside the region become that region's captures.
    fn collect_operand_captures(
        &self,
        region_scope: u32,
        scope_raw: u32,
        operation: &OperationEntry,
        captures: &mut BTreeSet<u32>,
        span: Span,
    ) -> Result<(), PlanConstructionError> {
        for operand in operation.operands() {
            let value = self
                .values
                .get(*operand as usize)
                .ok_or(PlanConstructionError::InvalidOperation { span })?;
            if !self.scope_dominates(value.scope, scope_raw) {
                return Err(PlanConstructionError::InvalidOperation { span });
            }
            if !self.scope_dominates(region_scope, value.scope) {
                captures.insert(*operand);
            }
        }
        Ok(())
    }

    fn scope_owner(
        &self,
        scope: ConstructionScopeId<'plan>,
        span: Span,
    ) -> Result<u32, PlanConstructionError> {
        Ok(self.scope(scope, span)?.owner)
    }

    fn scope_owner_function(
        &self,
        scope: ConstructionScopeId<'plan>,
        span: Span,
    ) -> Result<FunctionId<'dae>, PlanConstructionError> {
        let owner = self.scope_owner(scope, span)?;
        self.owner_function(
            ConstructionOwnerId {
                raw: owner,
                marker: PhantomData,
            },
            span,
        )
    }

    fn record_operation(
        &mut self,
        scope: ConstructionScopeId<'plan>,
        operation: u32,
        span: Span,
    ) -> Result<(), PlanConstructionError> {
        let owner = self.scope_owner(scope, span)?;
        self.scopes[scope.raw as usize].operations.push(operation);
        self.owners[owner as usize].operations.push(operation);
        Ok(())
    }

    fn scope_dominates(&self, ancestor: u32, mut scope: u32) -> bool {
        loop {
            if ancestor == scope {
                return true;
            }
            let Some(parent) = self.scopes[scope as usize].parent else {
                return false;
            };
            scope = parent;
        }
    }

    fn transaction<T>(
        &mut self,
        operation: impl FnOnce(&mut Self) -> Result<T, PlanConstructionError>,
    ) -> Result<T, PlanConstructionError> {
        if let Some(error) = &self.poison {
            return Err(error.clone());
        }
        match operation(self) {
            Ok(value) => Ok(value),
            Err(error) => {
                self.poison = Some(error.clone());
                Err(error)
            }
        }
    }
}
