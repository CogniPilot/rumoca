//! The callable plan root, its storage, its entries, and their sole mint.
//!
//! Every invariant-bearing field below is private to this module, and the only
//! code that may name those fields is the mint that also proves them:
//! [`CallablePlan::construct`] together with
//! [`CallablePlanConstruction::finish`]. A sibling module cannot write
//! `CallablePlan { .. }` or `PlanStorage { .. }`, so there is no route to a
//! plan that skipped coverage or the acyclic receipt.
//!
//! Entry fields are visible to the surrounding construction authority
//! (`pub(in crate::construction)`) because the checked operations that build
//! them live there. They are invisible to `crate::view`, `crate::model`, and
//! the crate root, which read the plan only through the immutable correlated
//! lends at the end of this module.

use std::collections::HashSet;

use rumoca_core::{OperationContractKey, Span};
use rumoca_ir_dae::{Dae, DaeProvenance};

use super::contract::topological_owner_order;
use super::{CallablePlanConstruction, PlanConstructionError};
use crate::model::{
    CallableCounters, CallableIntegerSourceFact, CallableInterface, CallableValueType,
    CompactDomain, OwnerCallOccurrence,
};

#[derive(Debug)]
pub(crate) enum CallableOperationDetail {
    Plain,
    Map(CompactDomain),
    IntegerToReal {
        source_fact: CallableIntegerSourceFact,
    },
    Call {
        callee: u32,
    },
}

#[derive(Debug)]
pub(crate) struct OwnerEntry {
    pub(in crate::construction) source_function: u32,
    pub(in crate::construction) root_scope: u32,
    pub(in crate::construction) interface: CallableInterface,
    pub(in crate::construction) declaration: DaeProvenance,
    pub(in crate::construction) callees: Box<[u32]>,
    pub(in crate::construction) operations: Box<[u32]>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ValueProducer {
    Operation(u32),
    Projection(u32),
}

#[derive(Debug)]
pub(crate) struct ValueEntry {
    pub(in crate::construction) owner: u32,
    pub(in crate::construction) scope: u32,
    pub(in crate::construction) source_expression: u32,
    pub(in crate::construction) value_type: CallableValueType,
    pub(in crate::construction) producer: ValueProducer,
}

#[derive(Debug)]
pub(crate) enum OperationSource {
    Expression(u32),
    DerivedExpression(u32),
    Definition(u32),
    CallOccurrence {
        call: u32,
        owner: u32,
        projections: Box<[CallProjectionSourceEntry]>,
    },
    ConditionalGroup {
        expressions: Box<[u32]>,
        definitions: Box<[u32]>,
    },
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct CallProjectionSourceEntry {
    pub(in crate::construction) expression: u32,
    pub(in crate::construction) output: u32,
}

#[derive(Debug)]
pub(crate) struct OperationEntry {
    pub(in crate::construction) owner: u32,
    pub(in crate::construction) scope: u32,
    pub(in crate::construction) source: OperationSource,
    pub(in crate::construction) contract: OperationContractKey,
    pub(in crate::construction) operands: Box<[u32]>,
    pub(in crate::construction) results: Box<[u32]>,
    pub(in crate::construction) detail: CallableOperationDetail,
    pub(in crate::construction) provenance: DaeProvenance,
}

#[derive(Debug)]
pub(crate) struct ScopeEntry {
    pub(in crate::construction) owner: u32,
    pub(in crate::construction) parent: Option<u32>,
    pub(in crate::construction) operations: Box<[u32]>,
}

#[derive(Debug)]
pub(crate) enum StructuredRegionDetail {
    Conditional {
        branch_scopes: Box<[u32]>,
        fallback_scope: u32,
        conditions: Box<[u32]>,
        branch_results: Box<[Box<[u32]>]>,
        fallback_results: Box<[u32]>,
    },
    Map {
        body_scope: u32,
        body_result: u32,
    },
}

#[derive(Debug)]
pub(crate) struct StructuredRegionEntry {
    pub(in crate::construction) owner: u32,
    pub(in crate::construction) operation: u32,
    pub(in crate::construction) contract: OperationContractKey,
    pub(in crate::construction) captures: Box<[u32]>,
    pub(in crate::construction) results: Box<[u32]>,
    pub(in crate::construction) detail: StructuredRegionDetail,
    pub(in crate::construction) domain: Option<CompactDomain>,
    pub(in crate::construction) provenance: DaeProvenance,
}

#[derive(Debug)]
pub(crate) struct ProjectionEntry {
    pub(in crate::construction) owner: u32,
    pub(in crate::construction) source_expression: u32,
    pub(in crate::construction) source_operation: u32,
    pub(in crate::construction) value: u32,
    pub(in crate::construction) provenance: DaeProvenance,
}

#[derive(Debug)]
pub(crate) struct EffectEntry {
    pub(in crate::construction) owner: u32,
    pub(in crate::construction) condition_expression: u32,
    pub(in crate::construction) predicate: u32,
    pub(in crate::construction) message_expression: u32,
    pub(in crate::construction) provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct AcyclicCallEdge {
    pub(in crate::construction) caller: u32,
    pub(in crate::construction) callee: u32,
    pub(in crate::construction) call_operation: u32,
}

#[derive(Debug)]
struct AcyclicReceipt {
    owner_order: Box<[u32]>,
    call_edges: Box<[AcyclicCallEdge]>,
}

/// Correlated storage for one finished plan.
///
/// Sole mint: [`CallablePlanConstruction::finish`] below. The fields are
/// private to this module, so no other module can assemble storage that never
/// discharged coverage.
#[derive(Debug)]
struct PlanStorage {
    owners: Box<[OwnerEntry]>,
    scopes: Box<[ScopeEntry]>,
    values: Box<[ValueEntry]>,
    operations: Box<[OperationEntry]>,
    regions: Box<[StructuredRegionEntry]>,
    projections: Box<[ProjectionEntry]>,
    effects: Box<[EffectEntry]>,
    receipt: AcyclicReceipt,
}

/// One affine callable proof plan retaining its exact checked DAE owner.
///
/// Sole mint: [`CallablePlan::construct`] below. Neither field is nameable
/// outside this module, so the only way to hold a `CallablePlan` is to have
/// run a construction that discharged the complete source inventory and minted
/// an acyclic receipt.
///
/// ```compile_fail
/// fn clone_plan(plan: rumoca_plan_callable::CallablePlan) {
///     let _second = plan.clone();
/// }
/// ```
///
/// ```compile_fail
/// fn default_plan() {
///     let _: rumoca_plan_callable::CallablePlan = Default::default();
/// }
/// ```
///
/// A struct literal cannot be written outside this module even when every
/// field value is supplied by a diverging expression, so no consumer can forge
/// a plan around an unchecked storage value:
///
/// ```compile_fail
/// fn never<T>() -> T {
///     loop {}
/// }
///
/// fn forge() -> rumoca_plan_callable::CallablePlan {
///     rumoca_plan_callable::CallablePlan {
///         dae: never(),
///         storage: never(),
///     }
/// }
/// ```
///
/// The storage type itself is not nameable either, so a consumer cannot even
/// declare the value the forged literal would need:
///
/// ```compile_fail
/// fn never<T>() -> T {
///     loop {}
/// }
///
/// fn forge_storage() -> rumoca_plan_callable::PlanStorage {
///     never()
/// }
/// ```
///
/// The retained DAE owner never escapes, so a consumer cannot recover a
/// mutable root from a plan it holds:
///
/// ```compile_fail
/// fn never<T>() -> T {
///     loop {}
/// }
///
/// fn escape() -> rumoca_ir_dae::Dae {
///     let plan: rumoca_plan_callable::CallablePlan = never();
///     plan.dae
/// }
/// ```
#[derive(Debug)]
pub struct CallablePlan {
    dae: Dae,
    storage: PlanStorage,
}

impl CallablePlan {
    /// Transfer one checked DAE into an affine construction and keep the plan
    /// only when that construction discharged every source occurrence.
    ///
    /// This is the sole mint of a `CallablePlan` value.
    pub fn construct<E, F>(dae: Dae, build: F) -> Result<Self, E>
    where
        E: From<PlanConstructionError>,
        F: for<'plan, 'dae> FnOnce(
            rumoca_ir_dae::DaeView<'dae>,
            &mut CallablePlanConstruction<'plan, 'dae>,
        ) -> Result<(), E>,
    {
        let storage = dae.inspect(|view| {
            view.with_callable_source_inventory(|inventory| {
                let mut construction = CallablePlanConstruction::new(view, &inventory);
                build(view, &mut construction)?;
                construction.finish().map_err(E::from)
            })
        })?;
        Ok(Self { dae, storage })
    }

    pub fn counters(&self) -> CallableCounters {
        self.dae.inspect(|dae| {
            dae.with_callable_source_inventory(|inventory| {
                let represented = self.represented_expression_sources();
                CallableCounters {
                    source_functions: inventory.functions().len(),
                    source_expressions: inventory.expressions().len(),
                    source_definitions: inventory.definitions().len(),
                    source_assertions: inventory.assertions().len(),
                    source_conditionals: inventory.conditionals().len(),
                    source_calls: inventory.calls().len(),
                    source_folds: inventory.folds().len(),
                    evidence_discharges: inventory.expressions().len() - represented.len(),
                    owners: self.storage.owners.len(),
                    scopes: self.storage.scopes.len(),
                    values: self.storage.values.len(),
                    operations: self.storage.operations.len(),
                    regions: self.storage.regions.len(),
                    projections: self.storage.projections.len(),
                    effects: self.storage.effects.len(),
                    call_edges: self.storage.receipt.call_edges.len(),
                }
            })
        })
    }

    fn represented_expression_sources(&self) -> HashSet<(u32, u32)> {
        let mut represented = HashSet::new();
        for operation in &self.storage.operations {
            let function = self.storage.owners[operation.owner as usize].source_function;
            match &operation.source {
                OperationSource::Expression(expression) => {
                    represented.insert((function, *expression));
                }
                OperationSource::ConditionalGroup { expressions, .. } => {
                    represented
                        .extend(expressions.iter().map(|expression| (function, *expression)));
                }
                OperationSource::CallOccurrence {
                    owner, projections, ..
                } => {
                    represented.insert((function, *owner));
                    represented.extend(
                        projections
                            .iter()
                            .map(|projection| (function, projection.expression)),
                    );
                }
                OperationSource::DerivedExpression(_) | OperationSource::Definition(_) => {}
            }
        }
        for projection in &self.storage.projections {
            represented.insert((
                self.storage.owners[projection.owner as usize].source_function,
                projection.source_expression,
            ));
        }
        represented
    }

    pub fn responsible_span(&self) -> Option<Span> {
        self.dae.inspect(|view| view.responsible_span())
    }

    /// Lend the retained DAE owner as a fresh branded inspection.
    ///
    /// The closure receives a borrowed view; the owner itself never leaves the
    /// plan.
    pub(crate) fn inspect_retained_dae<R>(
        &self,
        inspect: impl for<'dae> FnOnce(rumoca_ir_dae::DaeView<'dae>) -> R,
    ) -> R {
        self.dae.inspect(inspect)
    }

    pub(crate) fn owner_entries(&self) -> &[OwnerEntry] {
        &self.storage.owners
    }

    pub(crate) fn scope_entries(&self) -> &[ScopeEntry] {
        &self.storage.scopes
    }

    pub(crate) fn value_entries(&self) -> &[ValueEntry] {
        &self.storage.values
    }

    pub(crate) fn operation_entries(&self) -> &[OperationEntry] {
        &self.storage.operations
    }

    pub(crate) fn region_entries(&self) -> &[StructuredRegionEntry] {
        &self.storage.regions
    }

    pub(crate) fn projection_entries(&self) -> &[ProjectionEntry] {
        &self.storage.projections
    }

    pub(crate) fn effect_entries(&self) -> &[EffectEntry] {
        &self.storage.effects
    }

    pub(crate) fn acyclic_owner_order(&self) -> &[u32] {
        &self.storage.receipt.owner_order
    }

    pub(crate) fn acyclic_call_edges(&self) -> &[AcyclicCallEdge] {
        &self.storage.receipt.call_edges
    }
}

impl CallablePlanConstruction<'_, '_> {
    /// Mint the correlated storage of one finished plan.
    ///
    /// This is the sole mint of a `PlanStorage` value; it runs only after the
    /// complete source inventory is discharged, every linear capability is
    /// closed, and the call graph proves acyclic.
    fn finish(self) -> Result<PlanStorage, PlanConstructionError> {
        if let Some(error) = self.poison {
            return Err(error);
        }
        // An abandoned linear capability is checked first because it is the
        // cause, not the symptom: a region only claims its own source
        // occurrence when it closes, so leaving one open also leaves that
        // occurrence outstanding. Reporting the coverage gap first would name
        // an arbitrary unclaimed expression instead of the region the caller
        // actually abandoned, and would make this refusal unreachable.
        if let Some(span) = self.first_unclosed_capability_span() {
            return Err(PlanConstructionError::InvalidOperation { span });
        }
        if let Some(span) = self.first_outstanding_span() {
            return Err(PlanConstructionError::IncompleteCoverage { span });
        }
        let call_occurrences = self
            .call_edges
            .iter()
            .map(|edge| OwnerCallOccurrence {
                caller: edge.caller,
                callee: edge.callee,
                span: self.operations[edge.call_operation as usize]
                    .provenance
                    .span(),
            })
            .collect::<Box<[_]>>();
        let owner_order = topological_owner_order(&self.owners, &call_occurrences)?;
        Ok(PlanStorage {
            owners: self
                .owners
                .into_iter()
                .map(|owner| OwnerEntry {
                    source_function: owner.source_function,
                    root_scope: owner.root_scope,
                    interface: owner.interface,
                    declaration: owner.declaration,
                    callees: owner.callees.into_boxed_slice(),
                    operations: owner.operations.into_boxed_slice(),
                })
                .collect(),
            scopes: self
                .scopes
                .into_iter()
                .map(|scope| ScopeEntry {
                    owner: scope.owner,
                    parent: scope.parent,
                    operations: scope.operations.into_boxed_slice(),
                })
                .collect(),
            values: self.values.into_boxed_slice(),
            operations: self.operations.into_boxed_slice(),
            regions: self.regions.into_boxed_slice(),
            projections: self.projections.into_boxed_slice(),
            effects: self.effects.into_boxed_slice(),
            receipt: AcyclicReceipt {
                owner_order,
                call_edges: self.call_edges.into_boxed_slice(),
            },
        })
    }

    /// The exact retained span of the first capability the caller left open.
    ///
    /// Every open capability retains its own source provenance: an open
    /// structured region keeps its DAE expression occurrence, an open
    /// conditional group keeps its conditional occurrence, and an unclosed
    /// scope keeps the declaration of the owner that opened it. There is no
    /// span-free refusal.
    fn first_unclosed_capability_span(&self) -> Option<Span> {
        self.expressions
            .iter()
            .find(|item| self.open_regions.contains(&(item.function, item.id)))
            .map(|item| item.provenance.span())
            .or_else(|| {
                self.conditionals
                    .iter()
                    .find(|item| self.open_conditional_groups.contains(&item.id))
                    .map(|item| item.provenance.span())
            })
            .or_else(|| {
                self.scopes
                    .iter()
                    .find(|scope| {
                        scope.open_children != 0 || (scope.parent.is_some() && !scope.closed)
                    })
                    .map(|scope| self.owners[scope.owner as usize].declaration.span())
            })
    }

    fn first_outstanding_span(&self) -> Option<Span> {
        self.functions
            .iter()
            .find(|item| self.outstanding_functions.contains(&item.id))
            .map(|item| item.provenance.span())
            .or_else(|| {
                self.expressions
                    .iter()
                    .find(|item| {
                        self.outstanding_expressions
                            .contains(&(item.function, item.id))
                    })
                    .map(|item| item.provenance.span())
            })
            .or_else(|| {
                self.definitions
                    .iter()
                    .find(|item| self.outstanding_definitions.contains(&item.id))
                    .map(|item| item.provenance.span())
            })
            .or_else(|| {
                self.assertions
                    .iter()
                    .find(|item| self.outstanding_assertions.contains(&item.id))
                    .map(|item| item.provenance.span())
            })
            .or_else(|| {
                self.conditionals
                    .iter()
                    .find(|item| self.outstanding_conditionals.contains(&item.id))
                    .map(|item| item.provenance.span())
            })
            .or_else(|| {
                self.calls
                    .iter()
                    .find(|item| self.outstanding_calls.contains(&item.id))
                    .map(|item| item.provenance.span())
            })
            .or_else(|| {
                self.folds
                    .iter()
                    .find(|item| self.outstanding_folds.contains(&item.id))
                    .map(|item| item.provenance.span())
            })
    }
}

/// Immutable correlated lends.
///
/// `crate::view` reads a finished plan only through the accessors below. It
/// can observe every retained fact and can name no entry field, so a borrowed
/// view is a projection of the minted plan rather than a second construction
/// route.
impl OwnerEntry {
    pub(crate) fn source_function(&self) -> u32 {
        self.source_function
    }

    pub(crate) fn root_scope(&self) -> u32 {
        self.root_scope
    }

    pub(crate) fn interface(&self) -> &CallableInterface {
        &self.interface
    }

    pub(crate) fn declaration(&self) -> DaeProvenance {
        self.declaration
    }

    pub(crate) fn callees(&self) -> &[u32] {
        &self.callees
    }

    pub(crate) fn operations(&self) -> &[u32] {
        &self.operations
    }
}

impl ScopeEntry {
    pub(crate) fn owner(&self) -> u32 {
        self.owner
    }

    pub(crate) fn parent(&self) -> Option<u32> {
        self.parent
    }

    pub(crate) fn operations(&self) -> &[u32] {
        &self.operations
    }
}

impl ValueEntry {
    pub(crate) fn owner(&self) -> u32 {
        self.owner
    }

    pub(crate) fn scope(&self) -> u32 {
        self.scope
    }

    pub(crate) fn source_expression(&self) -> u32 {
        self.source_expression
    }

    pub(crate) fn value_type(&self) -> &CallableValueType {
        &self.value_type
    }

    pub(crate) fn producer(&self) -> ValueProducer {
        self.producer
    }
}

impl OperationEntry {
    pub(crate) fn owner(&self) -> u32 {
        self.owner
    }

    pub(crate) fn scope(&self) -> u32 {
        self.scope
    }

    pub(crate) fn source(&self) -> &OperationSource {
        &self.source
    }

    pub(crate) fn contract(&self) -> OperationContractKey {
        self.contract
    }

    pub(crate) fn operands(&self) -> &[u32] {
        &self.operands
    }

    pub(crate) fn results(&self) -> &[u32] {
        &self.results
    }

    pub(crate) fn detail(&self) -> &CallableOperationDetail {
        &self.detail
    }

    pub(crate) fn provenance(&self) -> DaeProvenance {
        self.provenance
    }
}

impl CallProjectionSourceEntry {
    pub(crate) fn expression(&self) -> u32 {
        self.expression
    }

    pub(crate) fn output(&self) -> u32 {
        self.output
    }
}

impl StructuredRegionEntry {
    pub(crate) fn owner(&self) -> u32 {
        self.owner
    }

    pub(crate) fn operation(&self) -> u32 {
        self.operation
    }

    pub(crate) fn contract(&self) -> OperationContractKey {
        self.contract
    }

    pub(crate) fn captures(&self) -> &[u32] {
        &self.captures
    }

    pub(crate) fn results(&self) -> &[u32] {
        &self.results
    }

    pub(crate) fn detail(&self) -> &StructuredRegionDetail {
        &self.detail
    }

    pub(crate) fn domain(&self) -> Option<&CompactDomain> {
        self.domain.as_ref()
    }

    pub(crate) fn provenance(&self) -> DaeProvenance {
        self.provenance
    }
}

impl ProjectionEntry {
    pub(crate) fn owner(&self) -> u32 {
        self.owner
    }

    pub(crate) fn source_expression(&self) -> u32 {
        self.source_expression
    }

    pub(crate) fn source_operation(&self) -> u32 {
        self.source_operation
    }

    pub(crate) fn value(&self) -> u32 {
        self.value
    }

    pub(crate) fn provenance(&self) -> DaeProvenance {
        self.provenance
    }
}

impl EffectEntry {
    pub(crate) fn owner(&self) -> u32 {
        self.owner
    }

    pub(crate) fn condition_expression(&self) -> u32 {
        self.condition_expression
    }

    pub(crate) fn predicate(&self) -> u32 {
        self.predicate
    }

    pub(crate) fn message_expression(&self) -> u32 {
        self.message_expression
    }

    pub(crate) fn provenance(&self) -> DaeProvenance {
        self.provenance
    }
}

impl AcyclicCallEdge {
    pub(crate) fn caller(&self) -> u32 {
        self.caller
    }

    pub(crate) fn callee(&self) -> u32 {
        self.callee
    }

    pub(crate) fn call_operation(&self) -> u32 {
        self.call_operation
    }
}
