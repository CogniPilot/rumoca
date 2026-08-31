use std::marker::PhantomData;

use rumoca_core::OperationContractKey;
use rumoca_ir_dae::{
    DaeProvenance, DaeView, ExprId, ExpressionView, FunctionCallId, FunctionDefinitionId,
    FunctionDefinitionView, FunctionId, FunctionView,
};

use crate::construction::plan::{
    AcyclicCallEdge, CallProjectionSourceEntry, CallableOperationDetail, EffectEntry,
    OperationEntry, OperationSource, OwnerEntry, ProjectionEntry, ScopeEntry,
    StructuredRegionDetail, StructuredRegionEntry, ValueEntry, ValueProducer,
};
use crate::{
    CallableIntegerSourceFact, CallableInterface, CallablePlan, CallableValueType, CompactDomain,
};

macro_rules! plan_id {
    ($name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub struct $name<'plan> {
            raw: u32,
            marker: PhantomData<&'plan mut &'plan ()>,
        }
    };
}

plan_id!(CallableOwnerId);
plan_id!(CallableScopeId);
plan_id!(CallableValueId);
plan_id!(CallableOperationId);
plan_id!(CallableProjectionId);
plan_id!(CallableEffectId);

#[derive(Clone, Copy)]
pub struct CallablePlanView<'plan, 'dae> {
    plan: &'plan CallablePlan,
    dae: DaeView<'dae>,
    functions: &'plan [FunctionId<'dae>],
    expressions: &'plan [ExprId<'dae>],
    definitions: &'plan [Box<[FunctionDefinitionId<'dae>]>],
    calls: &'plan [Box<[FunctionCallId<'dae>]>],
}

#[derive(Clone, Copy)]
pub struct CallableOwnerView<'plan, 'dae> {
    view: CallablePlanView<'plan, 'dae>,
    id: CallableOwnerId<'plan>,
}

#[derive(Clone, Copy)]
pub struct CallableValueView<'plan, 'dae> {
    view: CallablePlanView<'plan, 'dae>,
    id: CallableValueId<'plan>,
}

#[derive(Clone, Copy)]
pub struct CallableScopeView<'plan, 'dae> {
    view: CallablePlanView<'plan, 'dae>,
    id: CallableScopeId<'plan>,
}

#[derive(Clone, Copy)]
pub struct CallableOperationView<'plan, 'dae> {
    view: CallablePlanView<'plan, 'dae>,
    id: CallableOperationId<'plan>,
}

#[derive(Clone, Copy)]
pub struct CallableRegionView<'plan, 'dae> {
    view: CallablePlanView<'plan, 'dae>,
    raw: u32,
}

#[derive(Clone, Copy)]
pub struct CallableProjectionView<'plan, 'dae> {
    view: CallablePlanView<'plan, 'dae>,
    raw: u32,
}

#[derive(Clone, Copy)]
pub struct CallableEffectView<'plan, 'dae> {
    view: CallablePlanView<'plan, 'dae>,
    id: CallableEffectId<'plan>,
}

#[derive(Clone, Copy)]
pub struct CallableCallEdgeView<'plan, 'dae> {
    view: CallablePlanView<'plan, 'dae>,
    raw: u32,
}

#[derive(Clone, Copy)]
pub enum CallableValueProducer<'plan> {
    Operation(CallableOperationId<'plan>),
    Projection(CallableProjectionId<'plan>),
}

#[derive(Clone, Copy)]
pub enum CallableOperationSourceView<'plan, 'dae> {
    Expression(ExpressionView<'dae>),
    Definition(FunctionDefinitionView<'dae>),
    CallOccurrence(CallableCallOccurrenceSourceView<'plan, 'dae>),
    ConditionalGroup(CallableConditionalGroupSourceView<'plan, 'dae>),
}

#[derive(Clone, Copy)]
pub struct CallableCallOccurrenceSourceView<'plan, 'dae> {
    view: CallablePlanView<'plan, 'dae>,
    function: u32,
    call: u32,
    owner: u32,
    projections: &'plan [CallProjectionSourceEntry],
}

#[derive(Clone, Copy)]
pub struct CallableCallProjectionSourceView<'dae> {
    expression: ExpressionView<'dae>,
    output: u32,
}

#[derive(Clone, Copy)]
pub struct CallableConditionalGroupSourceView<'plan, 'dae> {
    view: CallablePlanView<'plan, 'dae>,
    function: u32,
    expressions: &'plan [u32],
    definitions: &'plan [u32],
}

#[derive(Clone, Copy)]
pub enum CallableRegionDetailView<'plan, 'dae> {
    Conditional(CallableConditionalRegionView<'plan, 'dae>),
    Map(CallableMapRegionView<'plan, 'dae>),
}

#[derive(Clone, Copy)]
pub struct CallableConditionalRegionView<'plan, 'dae> {
    branch_scopes: &'plan [u32],
    fallback_scope: u32,
    conditions: &'plan [u32],
    branch_results: &'plan [Box<[u32]>],
    fallback_results: &'plan [u32],
    marker: PhantomData<&'dae mut &'dae ()>,
}

#[derive(Clone, Copy)]
pub struct CallableMapRegionView<'plan, 'dae> {
    body_scope: u32,
    body_result: u32,
    marker: PhantomData<&'plan mut &'dae ()>,
}

#[derive(Clone, Copy)]
pub enum CallableOperationDetailView<'plan> {
    Plain,
    Map(&'plan CompactDomain),
    IntegerToReal {
        source_fact: CallableIntegerSourceFact,
    },
    Call {
        callee: CallableOwnerId<'plan>,
    },
}

#[derive(Clone, Copy)]
pub struct CallableAssertionRelationView<'dae> {
    condition: ExpressionView<'dae>,
    message: ExpressionView<'dae>,
    provenance: DaeProvenance,
}

impl CallablePlan {
    pub fn inspect<R>(
        &self,
        inspect: impl for<'plan, 'dae> FnOnce(CallablePlanView<'plan, 'dae>) -> R,
    ) -> R {
        self.inspect_retained_dae(|dae| {
            let functions = correlated_function_ids(dae);
            let expressions = correlated_expression_ids(dae);
            let definitions = correlated_definition_ids(dae, &functions);
            let calls = correlated_call_ids(dae);
            inspect(CallablePlanView {
                plan: self,
                dae,
                functions: &functions,
                expressions: &expressions,
                definitions: &definitions,
                calls: &calls,
            })
        })
    }

    pub fn consume<R>(
        self,
        consume: impl for<'plan, 'dae> FnOnce(CallablePlanView<'plan, 'dae>) -> R,
    ) -> R {
        self.inspect_retained_dae(|dae| {
            let functions = correlated_function_ids(dae);
            let expressions = correlated_expression_ids(dae);
            let definitions = correlated_definition_ids(dae, &functions);
            let calls = correlated_call_ids(dae);
            consume(CallablePlanView {
                plan: &self,
                dae,
                functions: &functions,
                expressions: &expressions,
                definitions: &definitions,
                calls: &calls,
            })
        })
    }
}

impl<'plan, 'dae> CallablePlanView<'plan, 'dae> {
    pub fn owners(self) -> impl ExactSizeIterator<Item = CallableOwnerView<'plan, 'dae>> {
        (0..self.plan.owner_entries().len()).map(move |index| CallableOwnerView {
            view: self,
            id: CallableOwnerId {
                raw: index as u32,
                marker: PhantomData,
            },
        })
    }

    pub fn values(self) -> impl ExactSizeIterator<Item = CallableValueView<'plan, 'dae>> {
        (0..self.plan.value_entries().len()).map(move |index| CallableValueView {
            view: self,
            id: CallableValueId {
                raw: index as u32,
                marker: PhantomData,
            },
        })
    }

    pub fn scopes(self) -> impl ExactSizeIterator<Item = CallableScopeView<'plan, 'dae>> {
        (0..self.plan.scope_entries().len()).map(move |index| CallableScopeView {
            view: self,
            id: CallableScopeId {
                raw: index as u32,
                marker: PhantomData,
            },
        })
    }

    pub fn operations(self) -> impl ExactSizeIterator<Item = CallableOperationView<'plan, 'dae>> {
        (0..self.plan.operation_entries().len()).map(move |index| CallableOperationView {
            view: self,
            id: CallableOperationId {
                raw: index as u32,
                marker: PhantomData,
            },
        })
    }

    pub fn regions(self) -> impl ExactSizeIterator<Item = CallableRegionView<'plan, 'dae>> {
        (0..self.plan.region_entries().len()).map(move |index| CallableRegionView {
            view: self,
            raw: index as u32,
        })
    }

    pub fn projections(self) -> impl ExactSizeIterator<Item = CallableProjectionView<'plan, 'dae>> {
        (0..self.plan.projection_entries().len()).map(move |index| CallableProjectionView {
            view: self,
            raw: index as u32,
        })
    }

    pub fn effects(self) -> impl ExactSizeIterator<Item = CallableEffectView<'plan, 'dae>> {
        (0..self.plan.effect_entries().len()).map(move |index| CallableEffectView {
            view: self,
            id: CallableEffectId {
                raw: index as u32,
                marker: PhantomData,
            },
        })
    }

    pub fn acyclic_owner_order(
        self,
    ) -> impl ExactSizeIterator<Item = CallableOwnerId<'plan>> + 'plan {
        self.plan
            .acyclic_owner_order()
            .iter()
            .copied()
            .map(|raw| CallableOwnerId {
                raw,
                marker: PhantomData,
            })
    }

    pub fn call_edges(self) -> impl ExactSizeIterator<Item = CallableCallEdgeView<'plan, 'dae>> {
        (0..self.plan.acyclic_call_edges().len()).map(move |index| CallableCallEdgeView {
            view: self,
            raw: index as u32,
        })
    }
}

impl<'plan, 'dae> CallableOwnerView<'plan, 'dae> {
    pub const fn id(self) -> CallableOwnerId<'plan> {
        self.id
    }

    pub fn source_function(self) -> FunctionView<'dae> {
        let source = self.entry().source_function();
        self.view
            .dae
            .exact_function(self.view.functions[source as usize])
    }

    pub fn interface(self) -> &'plan CallableInterface {
        self.entry().interface()
    }

    pub fn root_scope(self) -> CallableScopeId<'plan> {
        CallableScopeId {
            raw: self.entry().root_scope(),
            marker: PhantomData,
        }
    }

    pub fn declaration(self) -> DaeProvenance {
        self.entry().declaration()
    }

    pub fn callees(self) -> impl ExactSizeIterator<Item = CallableOwnerId<'plan>> + 'plan {
        self.entry()
            .callees()
            .iter()
            .copied()
            .map(|raw| CallableOwnerId {
                raw,
                marker: PhantomData,
            })
    }

    pub fn operations(self) -> impl ExactSizeIterator<Item = CallableOperationId<'plan>> + 'plan {
        self.entry()
            .operations()
            .iter()
            .copied()
            .map(|raw| CallableOperationId {
                raw,
                marker: PhantomData,
            })
    }

    fn entry(self) -> &'plan OwnerEntry {
        &self.view.plan.owner_entries()[self.id.raw as usize]
    }
}

impl<'plan, 'dae> CallableScopeView<'plan, 'dae> {
    pub const fn id(self) -> CallableScopeId<'plan> {
        self.id
    }

    pub fn owner(self) -> CallableOwnerId<'plan> {
        CallableOwnerId {
            raw: self.entry().owner(),
            marker: PhantomData,
        }
    }

    pub fn parent(self) -> Option<CallableScopeId<'plan>> {
        self.entry().parent().map(|raw| CallableScopeId {
            raw,
            marker: PhantomData,
        })
    }

    pub fn operations(self) -> impl ExactSizeIterator<Item = CallableOperationId<'plan>> + 'plan {
        self.entry()
            .operations()
            .iter()
            .copied()
            .map(|raw| CallableOperationId {
                raw,
                marker: PhantomData,
            })
    }

    fn entry(self) -> &'plan ScopeEntry {
        &self.view.plan.scope_entries()[self.id.raw as usize]
    }
}

impl<'plan, 'dae> CallableValueView<'plan, 'dae> {
    pub const fn id(self) -> CallableValueId<'plan> {
        self.id
    }

    pub fn owner(self) -> CallableOwnerId<'plan> {
        CallableOwnerId {
            raw: self.entry().owner(),
            marker: PhantomData,
        }
    }

    pub fn source_expression(self) -> ExpressionView<'dae> {
        let raw = self.entry().source_expression();
        self.view
            .dae
            .exact_expression(self.view.expressions[raw as usize])
    }

    pub fn value_type(self) -> &'plan CallableValueType {
        self.entry().value_type()
    }

    pub fn scope(self) -> CallableScopeId<'plan> {
        CallableScopeId {
            raw: self.entry().scope(),
            marker: PhantomData,
        }
    }

    pub fn producer(self) -> CallableValueProducer<'plan> {
        match self.entry().producer() {
            ValueProducer::Operation(raw) => {
                CallableValueProducer::Operation(CallableOperationId {
                    raw,
                    marker: PhantomData,
                })
            }
            ValueProducer::Projection(raw) => {
                CallableValueProducer::Projection(CallableProjectionId {
                    raw,
                    marker: PhantomData,
                })
            }
        }
    }

    fn entry(self) -> &'plan ValueEntry {
        &self.view.plan.value_entries()[self.id.raw as usize]
    }
}

impl<'plan, 'dae> CallableOperationView<'plan, 'dae> {
    pub const fn id(self) -> CallableOperationId<'plan> {
        self.id
    }

    pub fn owner(self) -> CallableOwnerId<'plan> {
        CallableOwnerId {
            raw: self.entry().owner(),
            marker: PhantomData,
        }
    }

    pub fn source(self) -> CallableOperationSourceView<'plan, 'dae> {
        let function =
            self.view.plan.owner_entries()[self.entry().owner() as usize].source_function();
        match self.entry().source() {
            OperationSource::Expression(raw) | OperationSource::DerivedExpression(raw) => {
                CallableOperationSourceView::Expression(
                    self.view
                        .dae
                        .exact_expression(self.view.expressions[*raw as usize]),
                )
            }
            OperationSource::Definition(ordinal) => {
                CallableOperationSourceView::Definition(self.view.dae.exact_function_definition(
                    self.view.definitions[function as usize][*ordinal as usize],
                ))
            }
            OperationSource::CallOccurrence {
                call,
                owner,
                projections,
            } => CallableOperationSourceView::CallOccurrence(CallableCallOccurrenceSourceView {
                view: self.view,
                function,
                call: *call,
                owner: *owner,
                projections,
            }),
            OperationSource::ConditionalGroup {
                expressions,
                definitions,
            } => {
                CallableOperationSourceView::ConditionalGroup(CallableConditionalGroupSourceView {
                    view: self.view,
                    function,
                    expressions,
                    definitions,
                })
            }
        }
    }

    pub fn contract(self) -> OperationContractKey {
        self.entry().contract()
    }

    pub fn scope(self) -> CallableScopeId<'plan> {
        CallableScopeId {
            raw: self.entry().scope(),
            marker: PhantomData,
        }
    }

    pub fn operands(self) -> impl ExactSizeIterator<Item = CallableValueId<'plan>> + 'plan {
        self.entry()
            .operands()
            .iter()
            .copied()
            .map(|raw| CallableValueId {
                raw,
                marker: PhantomData,
            })
    }

    pub fn results(self) -> impl ExactSizeIterator<Item = CallableValueId<'plan>> + 'plan {
        self.entry()
            .results()
            .iter()
            .copied()
            .map(|raw| CallableValueId {
                raw,
                marker: PhantomData,
            })
    }

    pub fn detail(self) -> CallableOperationDetailView<'plan> {
        match self.entry().detail() {
            CallableOperationDetail::Plain => CallableOperationDetailView::Plain,
            CallableOperationDetail::Map(domain) => CallableOperationDetailView::Map(domain),
            CallableOperationDetail::IntegerToReal { source_fact } => {
                CallableOperationDetailView::IntegerToReal {
                    source_fact: *source_fact,
                }
            }
            CallableOperationDetail::Call { callee } => CallableOperationDetailView::Call {
                callee: CallableOwnerId {
                    raw: *callee,
                    marker: PhantomData,
                },
            },
        }
    }

    pub fn provenance(self) -> DaeProvenance {
        self.entry().provenance()
    }

    fn entry(self) -> &'plan OperationEntry {
        &self.view.plan.operation_entries()[self.id.raw as usize]
    }
}

impl<'plan, 'dae> CallableRegionView<'plan, 'dae> {
    pub fn owner(self) -> CallableOwnerId<'plan> {
        CallableOwnerId {
            raw: self.entry().owner(),
            marker: PhantomData,
        }
    }
    pub fn operation(self) -> CallableOperationId<'plan> {
        CallableOperationId {
            raw: self.entry().operation(),
            marker: PhantomData,
        }
    }
    pub fn contract(self) -> OperationContractKey {
        self.entry().contract()
    }
    pub fn captures(self) -> impl ExactSizeIterator<Item = CallableValueId<'plan>> + 'plan {
        self.entry()
            .captures()
            .iter()
            .copied()
            .map(|raw| CallableValueId {
                raw,
                marker: PhantomData,
            })
    }
    pub fn results(self) -> impl ExactSizeIterator<Item = CallableValueId<'plan>> + 'plan {
        self.entry()
            .results()
            .iter()
            .copied()
            .map(|raw| CallableValueId {
                raw,
                marker: PhantomData,
            })
    }
    pub fn domain(self) -> Option<&'plan CompactDomain> {
        self.entry().domain()
    }
    pub fn detail(self) -> CallableRegionDetailView<'plan, 'dae> {
        match self.entry().detail() {
            StructuredRegionDetail::Conditional {
                branch_scopes,
                fallback_scope,
                conditions,
                branch_results,
                fallback_results,
            } => CallableRegionDetailView::Conditional(CallableConditionalRegionView {
                branch_scopes,
                fallback_scope: *fallback_scope,
                conditions,
                branch_results,
                fallback_results,
                marker: PhantomData,
            }),
            StructuredRegionDetail::Map {
                body_scope,
                body_result,
            } => CallableRegionDetailView::Map(CallableMapRegionView {
                body_scope: *body_scope,
                body_result: *body_result,
                marker: PhantomData,
            }),
        }
    }
    pub fn provenance(self) -> DaeProvenance {
        self.entry().provenance()
    }
    fn entry(self) -> &'plan StructuredRegionEntry {
        &self.view.plan.region_entries()[self.raw as usize]
    }
}

impl<'plan, 'dae> CallableConditionalRegionView<'plan, 'dae> {
    pub fn branch_scopes(self) -> impl ExactSizeIterator<Item = CallableScopeId<'plan>> + 'plan {
        self.branch_scopes
            .iter()
            .copied()
            .map(|raw| CallableScopeId {
                raw,
                marker: PhantomData,
            })
    }

    pub fn fallback_scope(self) -> CallableScopeId<'plan> {
        CallableScopeId {
            raw: self.fallback_scope,
            marker: PhantomData,
        }
    }

    pub fn conditions(self) -> impl ExactSizeIterator<Item = CallableValueId<'plan>> + 'plan {
        self.conditions.iter().copied().map(|raw| CallableValueId {
            raw,
            marker: PhantomData,
        })
    }

    pub fn branch_count(self) -> usize {
        self.branch_results.len()
    }

    pub fn branch_results(
        self,
        ordinal: usize,
    ) -> Option<impl ExactSizeIterator<Item = CallableValueId<'plan>> + 'plan> {
        self.branch_results.get(ordinal).map(|results| {
            results.iter().copied().map(|raw| CallableValueId {
                raw,
                marker: PhantomData,
            })
        })
    }

    pub fn fallback_results(self) -> impl ExactSizeIterator<Item = CallableValueId<'plan>> + 'plan {
        self.fallback_results
            .iter()
            .copied()
            .map(|raw| CallableValueId {
                raw,
                marker: PhantomData,
            })
    }
}

impl<'plan, 'dae> CallableMapRegionView<'plan, 'dae> {
    pub fn body_scope(self) -> CallableScopeId<'plan> {
        CallableScopeId {
            raw: self.body_scope,
            marker: PhantomData,
        }
    }

    pub fn body_result(self) -> CallableValueId<'plan> {
        CallableValueId {
            raw: self.body_result,
            marker: PhantomData,
        }
    }
}

impl<'plan, 'dae> CallableProjectionView<'plan, 'dae> {
    pub fn id(self) -> CallableProjectionId<'plan> {
        CallableProjectionId {
            raw: self.raw,
            marker: PhantomData,
        }
    }
    pub fn owner(self) -> CallableOwnerId<'plan> {
        CallableOwnerId {
            raw: self.entry().owner(),
            marker: PhantomData,
        }
    }
    pub fn source_expression(self) -> ExpressionView<'dae> {
        let raw = self.entry().source_expression();
        self.view
            .dae
            .exact_expression(self.view.expressions[raw as usize])
    }
    pub fn source_operation(self) -> CallableOperationId<'plan> {
        CallableOperationId {
            raw: self.entry().source_operation(),
            marker: PhantomData,
        }
    }
    pub fn value(self) -> CallableValueId<'plan> {
        CallableValueId {
            raw: self.entry().value(),
            marker: PhantomData,
        }
    }
    pub fn provenance(self) -> DaeProvenance {
        self.entry().provenance()
    }
    fn entry(self) -> &'plan ProjectionEntry {
        &self.view.plan.projection_entries()[self.raw as usize]
    }
}

impl<'plan, 'dae> CallableEffectView<'plan, 'dae> {
    pub const fn id(self) -> CallableEffectId<'plan> {
        self.id
    }
    pub fn owner(self) -> CallableOwnerId<'plan> {
        CallableOwnerId {
            raw: self.entry().owner(),
            marker: PhantomData,
        }
    }
    pub fn predicate(self) -> CallableValueId<'plan> {
        CallableValueId {
            raw: self.entry().predicate(),
            marker: PhantomData,
        }
    }
    pub fn message(self) -> ExpressionView<'dae> {
        let raw = self.entry().message_expression();
        self.view
            .dae
            .exact_expression(self.view.expressions[raw as usize])
    }
    pub fn provenance(self) -> DaeProvenance {
        self.entry().provenance()
    }
    pub fn source(self) -> CallableAssertionRelationView<'dae> {
        CallableAssertionRelationView {
            condition: self.view.dae.exact_expression(
                self.view.expressions[self.entry().condition_expression() as usize],
            ),
            message: self.message(),
            provenance: self.entry().provenance(),
        }
    }
    fn entry(self) -> &'plan EffectEntry {
        &self.view.plan.effect_entries()[self.id.raw as usize]
    }
}

impl<'dae> CallableAssertionRelationView<'dae> {
    pub const fn condition(self) -> ExpressionView<'dae> {
        self.condition
    }

    pub const fn message(self) -> ExpressionView<'dae> {
        self.message
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

impl<'plan, 'dae> CallableConditionalGroupSourceView<'plan, 'dae> {
    pub fn expressions(self) -> impl ExactSizeIterator<Item = ExpressionView<'dae>> + 'plan {
        self.expressions.iter().map(move |raw| {
            self.view
                .dae
                .exact_expression(self.view.expressions[*raw as usize])
        })
    }

    pub fn definitions(
        self,
    ) -> impl ExactSizeIterator<Item = FunctionDefinitionView<'dae>> + 'plan {
        self.definitions.iter().map(move |ordinal| {
            self.view.dae.exact_function_definition(
                self.view.definitions[self.function as usize][*ordinal as usize],
            )
        })
    }
}

impl<'plan, 'dae> CallableCallOccurrenceSourceView<'plan, 'dae> {
    pub const fn call(self) -> FunctionCallId<'dae> {
        self.view.calls[self.function as usize][self.call as usize]
    }

    pub fn owner(self) -> ExpressionView<'dae> {
        self.view
            .dae
            .exact_expression(self.view.expressions[self.owner as usize])
    }

    pub fn projections(
        self,
    ) -> impl ExactSizeIterator<Item = CallableCallProjectionSourceView<'dae>> + 'plan {
        self.projections
            .iter()
            .map(move |projection| CallableCallProjectionSourceView {
                expression: self
                    .view
                    .dae
                    .exact_expression(self.view.expressions[projection.expression() as usize]),
                output: projection.output(),
            })
    }
}

impl<'dae> CallableCallProjectionSourceView<'dae> {
    pub const fn expression(self) -> ExpressionView<'dae> {
        self.expression
    }

    pub const fn output(self) -> u32 {
        self.output
    }
}

fn correlated_function_ids(dae: DaeView<'_>) -> Box<[FunctionId<'_>]> {
    dae.function_identities().collect()
}

fn correlated_expression_ids(dae: DaeView<'_>) -> Box<[ExprId<'_>]> {
    dae.expression_identities().collect()
}

fn correlated_definition_ids<'dae>(
    dae: DaeView<'dae>,
    functions: &[FunctionId<'dae>],
) -> Box<[Box<[FunctionDefinitionId<'dae>]>]> {
    functions
        .iter()
        .map(|function| {
            dae.exact_function(*function)
                .definition_identities()
                .collect()
        })
        .collect()
}

fn correlated_call_ids(dae: DaeView<'_>) -> Box<[Box<[FunctionCallId<'_>]>]> {
    dae.with_callable_source_inventory(|inventory| {
        inventory
            .functions()
            .map(|function| function.calls().map(|call| call.call()).collect())
            .collect()
    })
}

impl<'plan, 'dae> CallableCallEdgeView<'plan, 'dae> {
    pub fn caller(self) -> CallableOwnerId<'plan> {
        CallableOwnerId {
            raw: self.entry().caller(),
            marker: PhantomData,
        }
    }
    pub fn callee(self) -> CallableOwnerId<'plan> {
        CallableOwnerId {
            raw: self.entry().callee(),
            marker: PhantomData,
        }
    }
    pub fn call(self) -> CallableOperationId<'plan> {
        CallableOperationId {
            raw: self.entry().call_operation(),
            marker: PhantomData,
        }
    }
    fn entry(self) -> &'plan AcyclicCallEdge {
        &self.view.plan.acyclic_call_edges()[self.raw as usize]
    }
}
