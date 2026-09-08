//! Construction-issued source regions for the reachable callable graph.
//!
//! A callable consumer may classify an operation, but it may not decide where
//! a source occurrence executes. This inventory is derived once while the DAE
//! root closes. It gives every reachable expression use its exact source
//! region and represents lazy control flow as parent/activation edges. The
//! inventory is not serialized: current-wire replay rebuilds it by running the
//! same checked DAE construction.

use std::{collections::BTreeMap, marker::PhantomData};

use crate::expression::OperandRange;

use super::*;

#[derive(Debug, PartialEq, Eq)]
pub(super) struct CallableRegionInventory {
    regions: Box<[CallableRegionEntry]>,
    expression_uses: Box<[CallableExpressionUseEntry]>,
    call_uses: Box<[CallableCallUseEntry]>,
    captures: Box<[CallableCaptureEntry]>,
    external_bodies: Box<[CallableExternalBodyEntry]>,
    conditional_groups: Box<[CallableConditionalRegionsEntry]>,
    expression_conditionals: Box<[CallableExpressionConditionalRegionsEntry]>,
    fold_bodies: Box<[CallableFoldBodyEntry]>,
    map_bodies: Box<[CallableMapBodyEntry]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CallableRegionEntry {
    function: u32,
    parent: Option<u32>,
    activation: CallableRegionActivationEntry,
    kind: CallableRegionKindEntry,
    provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CallableRegionActivationEntry {
    Root,
    Enter,
    GuardTrue(u32),
    GuardFalse(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CallableRegionKindEntry {
    FunctionBody,
    ConditionalGuard {
        source: CallableConditionalRegionSourceEntry,
        ordinal: u32,
    },
    ConditionalResult {
        source: CallableConditionalRegionSourceEntry,
        ordinal: u32,
    },
    ConditionalFallback {
        source: CallableConditionalRegionSourceEntry,
    },
    FoldBody {
        fold: u32,
        domain: u32,
    },
    MapBody {
        expression_use: u32,
        domain: u32,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CallableConditionalRegionSourceEntry {
    AssignmentGroup(u32),
    ExpressionUse(u32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CallableExpressionUseEntry {
    function: u32,
    region: u32,
    expression: u32,
    operands: Box<[u32]>,
    provenance: DaeProvenance,
}

#[derive(Debug, PartialEq, Eq)]
struct CallableCallUseEntry {
    function: u32,
    region: u32,
    owner: u32,
    projections: Box<[CallableCallProjectionUseEntry]>,
    provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CallableCallProjectionUseEntry {
    expression_use: u32,
    output: u32,
    provenance: DaeProvenance,
}

struct PendingCallableCallUse {
    function: u32,
    region: u32,
    owner: u32,
    projections: Vec<CallableCallProjectionUseEntry>,
    provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum CallableCaptureSourceEntry {
    Parameter(u32),
    Definition(u32),
    Binder { domain: u32, ordinal: u32 },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CallableCaptureEntry {
    function: u32,
    source_region: u32,
    region: u32,
    expression_use: u32,
    source: CallableCaptureSourceEntry,
    provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CallableExternalBodyEntry {
    function: u32,
    region: u32,
    provenance: DaeProvenance,
}

#[derive(Debug, PartialEq, Eq)]
struct CallableConditionalRegionsEntry {
    function: u32,
    conditional: u32,
    parent: u32,
    guards: Box<[u32]>,
    results: Box<[u32]>,
    fallback: u32,
    provenance: DaeProvenance,
}

#[derive(Debug, PartialEq, Eq)]
struct CallableExpressionConditionalRegionsEntry {
    function: u32,
    expression_use: u32,
    parent: u32,
    guards: Box<[u32]>,
    results: Box<[u32]>,
    fallback: u32,
    provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CallableFoldBodyEntry {
    function: u32,
    fold: u32,
    parent: u32,
    body: u32,
    provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CallableMapBodyEntry {
    function: u32,
    expression_use: u32,
    domain: u32,
    parent: u32,
    body: u32,
    provenance: DaeProvenance,
}

/// Opaque identity of one source control region in a generative inventory loan.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CallableSourceRegionId<'inventory, 'dae> {
    raw: u32,
    marker: PhantomData<(&'inventory mut &'inventory (), &'dae ())>,
}

impl Copy for CallableSourceRegionId<'_, '_> {}

impl Clone for CallableSourceRegionId<'_, '_> {
    fn clone(&self) -> Self {
        *self
    }
}

/// Opaque identity of one expression use in one exact source region.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CallableExpressionUseId<'inventory, 'dae> {
    raw: u32,
    marker: PhantomData<(&'inventory mut &'inventory (), &'dae ())>,
}

/// Opaque identity of one call invocation in one exact source region.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CallableCallUseId<'inventory, 'dae> {
    raw: u32,
    marker: PhantomData<(&'inventory mut &'inventory (), &'dae ())>,
}

impl Copy for CallableCallUseId<'_, '_> {}

impl Clone for CallableCallUseId<'_, '_> {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for CallableExpressionUseId<'_, '_> {}

impl Clone for CallableExpressionUseId<'_, '_> {
    fn clone(&self) -> Self {
        *self
    }
}

/// Structural role of one DAE-issued callable source region.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallableSourceRegionKind<'inventory, 'dae> {
    FunctionBody,
    ConditionalGuard {
        source: CallableConditionalRegionSource<'inventory, 'dae>,
        ordinal: u32,
    },
    ConditionalResult {
        source: CallableConditionalRegionSource<'inventory, 'dae>,
        ordinal: u32,
    },
    ConditionalFallback {
        source: CallableConditionalRegionSource<'inventory, 'dae>,
    },
    FoldBody {
        fold: FunctionFoldId<'dae>,
        domain: DomainId<'dae>,
    },
    MapBody {
        expression_use: CallableExpressionUseId<'inventory, 'dae>,
        domain: DomainId<'dae>,
    },
}

/// Exact DAE source owner of one ordered conditional region family.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallableConditionalRegionSource<'inventory, 'dae> {
    AssignmentGroup(FunctionConditionalId<'dae>),
    ExpressionUse(CallableExpressionUseId<'inventory, 'dae>),
}

/// The one activation edge from a region's exact structural parent.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallableSourceRegionActivation<'inventory, 'dae> {
    Root,
    Enter,
    GuardTrue(CallableExpressionUseId<'inventory, 'dae>),
    GuardFalse(CallableExpressionUseId<'inventory, 'dae>),
}

/// One exact source control-region occurrence.
pub struct CallableSourceRegionOccurrence<'inventory, 'dae> {
    inventory: &'dae CallableRegionInventory,
    id: CallableSourceRegionId<'inventory, 'dae>,
}

/// One exact use of a DAE expression in a source control region.
pub struct CallableExpressionUseOccurrence<'inventory, 'dae> {
    inventory: &'dae CallableRegionInventory,
    id: CallableExpressionUseId<'inventory, 'dae>,
}

/// One exact callable invocation owned by one source control region.
pub struct CallableCallUseOccurrence<'inventory, 'dae> {
    id: CallableCallUseId<'inventory, 'dae>,
    entry: &'dae CallableCallUseEntry,
}

/// One exact result projection of a region-owned callable invocation.
#[derive(Clone, Copy)]
pub struct CallableCallProjectionUseOccurrence<'inventory, 'dae> {
    entry: &'dae CallableCallProjectionUseEntry,
    marker: PhantomData<&'inventory mut &'inventory ()>,
}

/// One exact cross-region source value entering a callable region.
#[derive(Clone, Copy)]
pub struct CallableCaptureOccurrence<'inventory, 'dae> {
    entry: &'dae CallableCaptureEntry,
    marker: PhantomData<&'inventory mut &'inventory ()>,
}

/// Source identity carried by one exact region capture edge.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallableCaptureSource<'dae> {
    FunctionParameter(FunctionParameterId<'dae>),
    FunctionDefinition(FunctionDefinitionId<'dae>),
    DomainBinder(DomainBinderId<'dae>),
}

/// Mandatory occurrence of one checked MLS external function body.
pub struct CallableExternalBodyOccurrence<'inventory, 'dae> {
    entry: &'dae CallableExternalBodyEntry,
    marker: PhantomData<&'inventory mut &'inventory ()>,
}

/// DAE-issued ordered lazy regions for one conditional assignment group.
pub struct CallableConditionalRegionsOccurrence<'inventory, 'dae> {
    entry: &'dae CallableConditionalRegionsEntry,
    marker: PhantomData<&'inventory mut &'inventory ()>,
}

/// DAE-issued ordered lazy regions for one scalar conditional expression use.
pub struct CallableExpressionConditionalRegionsOccurrence<'inventory, 'dae> {
    entry: &'dae CallableExpressionConditionalRegionsEntry,
    marker: PhantomData<&'inventory mut &'inventory ()>,
}

/// DAE-issued transition region for one compact function fold.
pub struct CallableFoldBodyOccurrence<'inventory, 'dae> {
    entry: &'dae CallableFoldBodyEntry,
    marker: PhantomData<&'inventory mut &'inventory ()>,
}

/// DAE-issued body region for one compact comprehension use.
pub struct CallableMapBodyOccurrence<'inventory, 'dae> {
    entry: &'dae CallableMapBodyEntry,
    marker: PhantomData<&'inventory mut &'inventory ()>,
}

impl<'inventory, 'dae> CallableSourceInventoryView<'inventory, 'dae> {
    pub fn source_regions(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableSourceRegionOccurrence<'inventory, 'dae>> + '_ {
        view::arena_walks::ArenaWalk::new(&self.inventory.regions.regions).map(|(raw, _)| {
            CallableSourceRegionOccurrence {
                inventory: &self.inventory.regions,
                id: region_id(raw),
            }
        })
    }

    pub fn expression_uses(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableExpressionUseOccurrence<'inventory, 'dae>> + '_ {
        view::arena_walks::ArenaWalk::new(&self.inventory.regions.expression_uses).map(
            |(raw, _)| CallableExpressionUseOccurrence {
                inventory: &self.inventory.regions,
                id: expression_use_id(raw),
            },
        )
    }

    pub fn call_uses(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableCallUseOccurrence<'inventory, 'dae>> + '_ {
        view::arena_walks::ArenaWalk::new(&self.inventory.regions.call_uses).map(|(raw, entry)| {
            CallableCallUseOccurrence {
                id: call_use_id(raw),
                entry,
            }
        })
    }

    pub fn captures(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableCaptureOccurrence<'inventory, 'dae>> + '_ {
        self.inventory
            .regions
            .captures
            .iter()
            .map(|entry| CallableCaptureOccurrence {
                entry,
                marker: PhantomData,
            })
    }

    pub fn external_bodies(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableExternalBodyOccurrence<'inventory, 'dae>> + '_ {
        self.inventory
            .regions
            .external_bodies
            .iter()
            .map(|entry| CallableExternalBodyOccurrence {
                entry,
                marker: PhantomData,
            })
    }

    pub fn conditional_regions(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableConditionalRegionsOccurrence<'inventory, 'dae>> + '_
    {
        self.inventory
            .regions
            .conditional_groups
            .iter()
            .map(|entry| CallableConditionalRegionsOccurrence {
                entry,
                marker: PhantomData,
            })
    }
}

impl<'inventory, 'dae> CallableSourceRegionOccurrence<'inventory, 'dae> {
    fn entry(&self) -> &'dae CallableRegionEntry {
        &self.inventory.regions[self.id.raw as usize]
    }

    pub const fn id(&self) -> CallableSourceRegionId<'inventory, 'dae> {
        self.id
    }

    pub fn function(&self) -> FunctionId<'dae> {
        FunctionId::from_raw(self.entry().function)
    }

    pub fn parent(&self) -> Option<CallableSourceRegionId<'inventory, 'dae>> {
        self.entry().parent.map(region_id)
    }

    pub fn activation(&self) -> CallableSourceRegionActivation<'inventory, 'dae> {
        match self.entry().activation {
            CallableRegionActivationEntry::Root => CallableSourceRegionActivation::Root,
            CallableRegionActivationEntry::Enter => CallableSourceRegionActivation::Enter,
            CallableRegionActivationEntry::GuardTrue(use_id) => {
                CallableSourceRegionActivation::GuardTrue(expression_use_id(use_id))
            }
            CallableRegionActivationEntry::GuardFalse(use_id) => {
                CallableSourceRegionActivation::GuardFalse(expression_use_id(use_id))
            }
        }
    }

    pub fn kind(&self) -> CallableSourceRegionKind<'inventory, 'dae> {
        match self.entry().kind {
            CallableRegionKindEntry::FunctionBody => CallableSourceRegionKind::FunctionBody,
            CallableRegionKindEntry::ConditionalGuard { source, ordinal } => {
                CallableSourceRegionKind::ConditionalGuard {
                    source: conditional_source(self.entry().function, source),
                    ordinal,
                }
            }
            CallableRegionKindEntry::ConditionalResult { source, ordinal } => {
                CallableSourceRegionKind::ConditionalResult {
                    source: conditional_source(self.entry().function, source),
                    ordinal,
                }
            }
            CallableRegionKindEntry::ConditionalFallback { source } => {
                CallableSourceRegionKind::ConditionalFallback {
                    source: conditional_source(self.entry().function, source),
                }
            }
            CallableRegionKindEntry::FoldBody { fold, domain } => {
                CallableSourceRegionKind::FoldBody {
                    fold: FunctionFoldId::from_raw(self.entry().function, fold),
                    domain: DomainId::from_raw(domain),
                }
            }
            CallableRegionKindEntry::MapBody {
                expression_use,
                domain,
            } => CallableSourceRegionKind::MapBody {
                expression_use: expression_use_id(expression_use),
                domain: DomainId::from_raw(domain),
            },
        }
    }

    pub fn provenance(&self) -> DaeProvenance {
        self.entry().provenance
    }
}

impl<'inventory, 'dae> CallableExpressionUseOccurrence<'inventory, 'dae> {
    fn entry(&self) -> &'dae CallableExpressionUseEntry {
        &self.inventory.expression_uses[self.id.raw as usize]
    }

    pub const fn id(&self) -> CallableExpressionUseId<'inventory, 'dae> {
        self.id
    }

    pub fn function(&self) -> FunctionId<'dae> {
        FunctionId::from_raw(self.entry().function)
    }

    pub fn region(&self) -> CallableSourceRegionId<'inventory, 'dae> {
        region_id(self.entry().region)
    }

    pub fn expression(&self) -> ExprId<'dae> {
        ExprId::from_raw(self.entry().expression)
    }

    pub fn operands(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableExpressionUseId<'inventory, 'dae>> + '_ {
        self.entry().operands.iter().copied().map(expression_use_id)
    }

    pub fn provenance(&self) -> DaeProvenance {
        self.entry().provenance
    }
}

impl<'inventory, 'dae> CallableCallUseOccurrence<'inventory, 'dae> {
    pub const fn id(&self) -> CallableCallUseId<'inventory, 'dae> {
        self.id
    }

    pub fn function(&self) -> FunctionId<'dae> {
        FunctionId::from_raw(self.entry.function)
    }

    pub fn region(&self) -> CallableSourceRegionId<'inventory, 'dae> {
        region_id(self.entry.region)
    }

    pub fn owner(&self) -> CallableExpressionUseId<'inventory, 'dae> {
        expression_use_id(self.entry.owner)
    }

    pub fn projections(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableCallProjectionUseOccurrence<'inventory, 'dae>> + '_
    {
        self.entry
            .projections
            .iter()
            .map(|entry| CallableCallProjectionUseOccurrence {
                entry,
                marker: PhantomData,
            })
    }

    pub fn provenance(&self) -> DaeProvenance {
        self.entry.provenance
    }
}

impl<'inventory, 'dae> CallableCallProjectionUseOccurrence<'inventory, 'dae> {
    pub fn expression_use(self) -> CallableExpressionUseId<'inventory, 'dae> {
        expression_use_id(self.entry.expression_use)
    }

    pub fn output(self) -> u32 {
        self.entry.output
    }

    pub fn provenance(self) -> DaeProvenance {
        self.entry.provenance
    }
}

impl<'inventory, 'dae> CallableCaptureOccurrence<'inventory, 'dae> {
    pub fn function(self) -> FunctionId<'dae> {
        FunctionId::from_raw(self.entry.function)
    }

    pub fn region(self) -> CallableSourceRegionId<'inventory, 'dae> {
        region_id(self.entry.region)
    }

    pub fn source_region(self) -> CallableSourceRegionId<'inventory, 'dae> {
        region_id(self.entry.source_region)
    }

    pub fn expression_use(self) -> CallableExpressionUseId<'inventory, 'dae> {
        expression_use_id(self.entry.expression_use)
    }

    pub fn source(self) -> CallableCaptureSource<'dae> {
        match self.entry.source {
            CallableCaptureSourceEntry::Parameter(ordinal) => {
                CallableCaptureSource::FunctionParameter(FunctionParameterId::from_raw(
                    self.entry.function,
                    ordinal,
                ))
            }
            CallableCaptureSourceEntry::Definition(ordinal) => {
                CallableCaptureSource::FunctionDefinition(FunctionDefinitionId::from_raw(
                    self.entry.function,
                    ordinal,
                ))
            }
            CallableCaptureSourceEntry::Binder { domain, ordinal } => {
                CallableCaptureSource::DomainBinder(DomainBinderId::from_raw(domain, ordinal))
            }
        }
    }

    pub fn provenance(self) -> DaeProvenance {
        self.entry.provenance
    }
}

impl<'inventory, 'dae> CallableExternalBodyOccurrence<'inventory, 'dae> {
    pub fn function(&self) -> FunctionId<'dae> {
        FunctionId::from_raw(self.entry.function)
    }

    pub fn region(&self) -> CallableSourceRegionId<'inventory, 'dae> {
        region_id(self.entry.region)
    }

    pub fn provenance(&self) -> DaeProvenance {
        self.entry.provenance
    }
}

macro_rules! conditional_region_accessors {
    ($type:ident, $source:expr) => {
        impl<'inventory, 'dae> $type<'inventory, 'dae> {
            pub fn parent(&self) -> CallableSourceRegionId<'inventory, 'dae> {
                region_id(self.entry.parent)
            }

            pub fn guards(
                &self,
            ) -> impl ExactSizeIterator<Item = CallableSourceRegionId<'inventory, 'dae>> + '_ {
                self.entry.guards.iter().copied().map(region_id)
            }

            pub fn results(
                &self,
            ) -> impl ExactSizeIterator<Item = CallableSourceRegionId<'inventory, 'dae>> + '_ {
                self.entry.results.iter().copied().map(region_id)
            }

            pub fn fallback(&self) -> CallableSourceRegionId<'inventory, 'dae> {
                region_id(self.entry.fallback)
            }

            pub fn provenance(&self) -> DaeProvenance {
                self.entry.provenance
            }

            pub fn source(&self) -> CallableConditionalRegionSource<'inventory, 'dae> {
                $source(self.entry)
            }
        }
    };
}

conditional_region_accessors!(
    CallableConditionalRegionsOccurrence,
    |entry: &CallableConditionalRegionsEntry| {
        CallableConditionalRegionSource::AssignmentGroup(FunctionConditionalId::from_raw(
            entry.function,
            entry.conditional,
        ))
    }
);

conditional_region_accessors!(
    CallableExpressionConditionalRegionsOccurrence,
    |entry: &CallableExpressionConditionalRegionsEntry| {
        CallableConditionalRegionSource::ExpressionUse(expression_use_id(entry.expression_use))
    }
);

impl<'inventory, 'dae> CallableFoldBodyOccurrence<'inventory, 'dae> {
    pub fn fold(&self) -> FunctionFoldId<'dae> {
        FunctionFoldId::from_raw(self.entry.function, self.entry.fold)
    }

    pub fn parent(&self) -> CallableSourceRegionId<'inventory, 'dae> {
        region_id(self.entry.parent)
    }

    pub fn body(&self) -> CallableSourceRegionId<'inventory, 'dae> {
        region_id(self.entry.body)
    }

    pub fn provenance(&self) -> DaeProvenance {
        self.entry.provenance
    }
}

impl<'inventory, 'dae> CallableMapBodyOccurrence<'inventory, 'dae> {
    pub fn expression_use(&self) -> CallableExpressionUseId<'inventory, 'dae> {
        expression_use_id(self.entry.expression_use)
    }

    pub fn parent(&self) -> CallableSourceRegionId<'inventory, 'dae> {
        region_id(self.entry.parent)
    }

    pub fn domain(&self) -> DomainId<'dae> {
        DomainId::from_raw(self.entry.domain)
    }

    pub fn body(&self) -> CallableSourceRegionId<'inventory, 'dae> {
        region_id(self.entry.body)
    }

    pub fn provenance(&self) -> DaeProvenance {
        self.entry.provenance
    }
}

fn conditional_source<'inventory, 'dae>(
    function: u32,
    source: CallableConditionalRegionSourceEntry,
) -> CallableConditionalRegionSource<'inventory, 'dae> {
    match source {
        CallableConditionalRegionSourceEntry::AssignmentGroup(conditional) => {
            CallableConditionalRegionSource::AssignmentGroup(FunctionConditionalId::from_raw(
                function,
                conditional,
            ))
        }
        CallableConditionalRegionSourceEntry::ExpressionUse(expression_use) => {
            CallableConditionalRegionSource::ExpressionUse(expression_use_id(expression_use))
        }
    }
}

fn region_id<'inventory, 'dae>(raw: u32) -> CallableSourceRegionId<'inventory, 'dae> {
    CallableSourceRegionId {
        raw,
        marker: PhantomData,
    }
}

fn expression_use_id<'inventory, 'dae>(raw: u32) -> CallableExpressionUseId<'inventory, 'dae> {
    CallableExpressionUseId {
        raw,
        marker: PhantomData,
    }
}

fn call_use_id<'inventory, 'dae>(raw: u32) -> CallableCallUseId<'inventory, 'dae> {
    CallableCallUseId {
        raw,
        marker: PhantomData,
    }
}

pub(super) fn issue_callable_region_inventory(
    storage: &Storage,
) -> Result<CallableRegionInventory, DaeConstructionError> {
    CallableRegionBuilder::new(storage).build()
}

struct CallableRegionBuilder<'storage> {
    storage: &'storage Storage,
    regions: Vec<CallableRegionEntry>,
    expression_uses: Vec<CallableExpressionUseEntry>,
    use_index: BTreeMap<(u32, u32, u32), u32>,
    call_uses: Vec<PendingCallableCallUse>,
    call_use_index: BTreeMap<(u32, u32, u32), u32>,
    captures: Vec<CallableCaptureEntry>,
    external_bodies: Vec<CallableExternalBodyEntry>,
    conditional_groups: Vec<CallableConditionalRegionsEntry>,
    expression_conditionals: Vec<CallableExpressionConditionalRegionsEntry>,
    fold_bodies: Vec<CallableFoldBodyEntry>,
    map_bodies: Vec<CallableMapBodyEntry>,
    roots: Vec<u32>,
    fold_regions: BTreeMap<(u32, u32), u32>,
}

struct ConditionalRegionInput<'expressions> {
    function: u32,
    parent: u32,
    source: CallableConditionalRegionSourceEntry,
    conditions: &'expressions [u32],
    branches: &'expressions [Vec<u32>],
    fallback: &'expressions [u32],
    provenance: DaeProvenance,
}

struct BuiltConditionalRegions {
    guards: Box<[u32]>,
    results: Box<[u32]>,
    fallback: u32,
    condition_uses: Box<[u32]>,
    branch_uses: Box<[Box<[u32]>]>,
    fallback_uses: Box<[u32]>,
}

struct ConditionalBranchInput<'expressions> {
    function: u32,
    source: CallableConditionalRegionSourceEntry,
    ordinal: u32,
    parent: u32,
    activation: CallableRegionActivationEntry,
    condition: u32,
    values: &'expressions [u32],
    provenance: DaeProvenance,
}

struct BuiltConditionalBranch {
    guard: u32,
    result: u32,
    condition_use: u32,
    value_uses: Box<[u32]>,
}

impl<'storage> CallableRegionBuilder<'storage> {
    fn new(storage: &'storage Storage) -> Self {
        Self {
            storage,
            regions: Vec::new(),
            expression_uses: Vec::new(),
            use_index: BTreeMap::new(),
            call_uses: Vec::new(),
            call_use_index: BTreeMap::new(),
            captures: Vec::new(),
            external_bodies: Vec::new(),
            conditional_groups: Vec::new(),
            expression_conditionals: Vec::new(),
            fold_bodies: Vec::new(),
            map_bodies: Vec::new(),
            roots: Vec::with_capacity(storage.functions.len()),
            fold_regions: BTreeMap::new(),
        }
    }

    fn build(mut self) -> Result<CallableRegionInventory, DaeConstructionError> {
        for function_index in 0..self.storage.functions.len() {
            self.build_function(function_index)?;
        }
        self.issue_captures()?;
        Ok(CallableRegionInventory {
            regions: self.regions.into_boxed_slice(),
            expression_uses: self.expression_uses.into_boxed_slice(),
            call_uses: self
                .call_uses
                .into_iter()
                .map(|entry| CallableCallUseEntry {
                    function: entry.function,
                    region: entry.region,
                    owner: entry.owner,
                    projections: entry.projections.into_boxed_slice(),
                    provenance: entry.provenance,
                })
                .collect(),
            captures: self.captures.into_boxed_slice(),
            external_bodies: self.external_bodies.into_boxed_slice(),
            conditional_groups: self.conditional_groups.into_boxed_slice(),
            expression_conditionals: self.expression_conditionals.into_boxed_slice(),
            fold_bodies: self.fold_bodies.into_boxed_slice(),
            map_bodies: self.map_bodies.into_boxed_slice(),
        })
    }

    fn build_function(&mut self, function_index: usize) -> Result<(), DaeConstructionError> {
        let function = &self.storage.functions[function_index];
        let function_raw = checked_u32(
            function_index,
            "callable source-region function",
            function.declaration,
        )?;
        let root = self.push_region(
            function_raw,
            None,
            CallableRegionActivationEntry::Root,
            CallableRegionKindEntry::FunctionBody,
            function.declaration,
        )?;
        self.roots.push(root);
        let body =
            function
                .definition
                .as_ref()
                .ok_or(DaeConstructionError::IncompleteDefinition {
                    kind: "function",
                    index: function_raw,
                    span: function.declaration.span(),
                })?;
        match body {
            FunctionBodyEntry::Modelica(body) => {
                let mut next_conditional = 0_u32;
                self.walk_statements(
                    function_raw,
                    root,
                    &body.statements,
                    &mut next_conditional,
                    function.declaration,
                )?;
            }
            FunctionBodyEntry::External(body) => {
                self.external_bodies.push(CallableExternalBodyEntry {
                    function: function_raw,
                    region: root,
                    provenance: body.provenance,
                });
                for expression in body.arguments.iter().filter_map(|argument| match argument {
                    ExternalArgumentEntry::Input(expression) => Some(*expression),
                    ExternalArgumentEntry::Output(_) => None,
                }) {
                    self.visit_expression(function_raw, root, expression, true, body.provenance)?;
                }
            }
        }
        Ok(())
    }

    fn walk_statements(
        &mut self,
        function: u32,
        region: u32,
        statements: &[FunctionStatementWire],
        next_conditional: &mut u32,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        for statement in statements {
            self.walk_statement(function, region, statement, next_conditional, at)?;
        }
        Ok(())
    }

    fn walk_statement(
        &mut self,
        function: u32,
        region: u32,
        statement: &FunctionStatementWire,
        next_conditional: &mut u32,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        match statement {
            FunctionStatementWire::Assignment { definition } => {
                self.visit_definition(function, region, *definition, at)?;
            }
            FunctionStatementWire::AssignmentGroup {
                definitions,
                conditional,
            } => self.walk_assignment_group(
                function,
                region,
                definitions,
                conditional.as_ref(),
                next_conditional,
                at,
            )?,
            FunctionStatementWire::Assertion {
                condition,
                message,
                provenance,
            } => {
                self.visit_expression(function, region, *condition, true, *provenance)?;
                self.visit_expression(function, region, *message, true, *provenance)?;
            }
            FunctionStatementWire::For {
                fold,
                statements,
                provenance,
            } => self.walk_fold(
                function,
                region,
                *fold,
                statements,
                next_conditional,
                *provenance,
            )?,
        }
        Ok(())
    }

    fn walk_assignment_group(
        &mut self,
        function: u32,
        region: u32,
        definitions: &[u32],
        conditional: Option<&FunctionConditionalWire>,
        next_conditional: &mut u32,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let Some(conditional) = conditional else {
            for definition in definitions {
                self.visit_definition(function, region, *definition, at)?;
            }
            return Ok(());
        };
        let provenance = self.definition_provenance(function, definitions, at)?;
        let source = CallableConditionalRegionSourceEntry::AssignmentGroup(*next_conditional);
        let built = self.build_conditional_regions(ConditionalRegionInput {
            function,
            parent: region,
            source,
            conditions: &conditional.conditions,
            branches: &conditional.branches,
            fallback: &conditional.fallback,
            provenance,
        })?;
        self.record_conditional_group(
            function,
            region,
            definitions,
            next_conditional,
            built,
            provenance,
        )
    }

    fn record_conditional_group(
        &mut self,
        function: u32,
        region: u32,
        definitions: &[u32],
        next_conditional: &mut u32,
        built: BuiltConditionalRegions,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        if built
            .branch_uses
            .iter()
            .any(|branch| branch.len() != definitions.len())
            || built.fallback_uses.len() != definitions.len()
        {
            return Err(DaeConstructionError::ShapeMismatch {
                span: provenance.span(),
            });
        }
        self.conditional_groups
            .push(CallableConditionalRegionsEntry {
                function,
                conditional: *next_conditional,
                parent: region,
                guards: built.guards,
                results: built.results,
                fallback: built.fallback,
                provenance,
            });
        *next_conditional =
            next_conditional
                .checked_add(1)
                .ok_or(DaeConstructionError::CapacityExceeded {
                    arena: "callable conditional region",
                    attempted_index: self.conditional_groups.len(),
                    span: provenance.span(),
                })?;
        for (result_ordinal, definition) in definitions.iter().copied().enumerate() {
            let entry = self.definition_entry(function, definition, provenance)?;
            let joined = self.push_expression_use(function, region, entry.rhs, entry.provenance)?;
            let operands = built
                .condition_uses
                .iter()
                .copied()
                .zip(
                    built
                        .branch_uses
                        .iter()
                        .map(|branch| branch[result_ordinal]),
                )
                .flat_map(|(condition, value)| [condition, value])
                .chain(std::iter::once(built.fallback_uses[result_ordinal]))
                .collect();
            self.expression_uses[joined as usize].operands = operands;
        }
        Ok(())
    }

    fn walk_fold(
        &mut self,
        function: u32,
        parent: u32,
        fold: u32,
        statements: &[FunctionStatementWire],
        next_conditional: &mut u32,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let (domain, parameter_definitions, output_definitions) = self
            .storage
            .functions
            .get(function as usize)
            .and_then(|entry| entry.folds.get(fold as usize))
            .and_then(|raw| self.storage.function_folds.get(*raw as usize))
            .map(|entry| {
                (
                    entry.domain,
                    entry.parameter_definitions.clone(),
                    entry.output_definitions.clone(),
                )
            })
            .ok_or_else(|| unknown("callable function fold", fold, provenance))?;
        let body = self.push_region(
            function,
            Some(parent),
            CallableRegionActivationEntry::Enter,
            CallableRegionKindEntry::FoldBody { fold, domain },
            provenance,
        )?;
        self.fold_regions.insert((function, fold), body);
        self.fold_bodies.push(CallableFoldBodyEntry {
            function,
            fold,
            parent,
            body,
            provenance,
        });
        for definition in parameter_definitions {
            self.visit_definition(function, body, definition, provenance)?;
        }
        self.walk_statements(function, body, statements, next_conditional, provenance)?;
        for definition in output_definitions {
            self.visit_definition(function, parent, definition, provenance)?;
        }
        Ok(())
    }

    fn build_conditional_regions(
        &mut self,
        input: ConditionalRegionInput<'_>,
    ) -> Result<BuiltConditionalRegions, DaeConstructionError> {
        if input.conditions.is_empty() || input.conditions.len() != input.branches.len() {
            return Err(invalid_arity(
                input.conditions.len(),
                input.branches.len(),
                input.provenance,
            ));
        }
        let mut guard_regions = Vec::with_capacity(input.conditions.len());
        let mut result_regions = Vec::with_capacity(input.conditions.len());
        let mut condition_uses = Vec::with_capacity(input.conditions.len());
        let mut branch_uses = Vec::with_capacity(input.branches.len());
        let mut preceding_guard = None;
        for (ordinal, (condition, branch)) in
            input.conditions.iter().zip(input.branches).enumerate()
        {
            let ordinal = checked_u32(ordinal, "callable conditional branch", input.provenance)?;
            let (parent, activation) = match preceding_guard {
                None => (input.parent, CallableRegionActivationEntry::Enter),
                Some((guard, condition_use)) => (
                    guard,
                    CallableRegionActivationEntry::GuardFalse(condition_use),
                ),
            };
            let built = self.build_conditional_branch(ConditionalBranchInput {
                function: input.function,
                source: input.source,
                ordinal,
                parent,
                activation,
                condition: *condition,
                values: branch,
                provenance: input.provenance,
            })?;
            guard_regions.push(built.guard);
            result_regions.push(built.result);
            condition_uses.push(built.condition_use);
            branch_uses.push(built.value_uses);
            preceding_guard = Some((built.guard, built.condition_use));
        }
        let Some((last_guard, last_condition)) = preceding_guard else {
            return Err(invalid_arity(0, input.branches.len(), input.provenance));
        };
        let fallback_region = self.push_region(
            input.function,
            Some(last_guard),
            CallableRegionActivationEntry::GuardFalse(last_condition),
            CallableRegionKindEntry::ConditionalFallback {
                source: input.source,
            },
            input.provenance,
        )?;
        let fallback_uses = input
            .fallback
            .iter()
            .map(|expression| {
                self.visit_expression(
                    input.function,
                    fallback_region,
                    *expression,
                    true,
                    input.provenance,
                )
            })
            .collect::<Result<Box<[_]>, _>>()?;
        Ok(BuiltConditionalRegions {
            guards: guard_regions.into_boxed_slice(),
            results: result_regions.into_boxed_slice(),
            fallback: fallback_region,
            condition_uses: condition_uses.into_boxed_slice(),
            branch_uses: branch_uses.into_boxed_slice(),
            fallback_uses,
        })
    }

    fn build_conditional_branch(
        &mut self,
        input: ConditionalBranchInput<'_>,
    ) -> Result<BuiltConditionalBranch, DaeConstructionError> {
        let guard = self.push_region(
            input.function,
            Some(input.parent),
            input.activation,
            CallableRegionKindEntry::ConditionalGuard {
                source: input.source,
                ordinal: input.ordinal,
            },
            input.provenance,
        )?;
        let condition_use = self.visit_expression(
            input.function,
            guard,
            input.condition,
            true,
            input.provenance,
        )?;
        let result = self.push_region(
            input.function,
            Some(guard),
            CallableRegionActivationEntry::GuardTrue(condition_use),
            CallableRegionKindEntry::ConditionalResult {
                source: input.source,
                ordinal: input.ordinal,
            },
            input.provenance,
        )?;
        let value_uses = input
            .values
            .iter()
            .map(|expression| {
                self.visit_expression(input.function, result, *expression, true, input.provenance)
            })
            .collect::<Result<Box<[_]>, _>>()?;
        Ok(BuiltConditionalBranch {
            guard,
            result,
            condition_use,
            value_uses,
        })
    }

    fn visit_definition(
        &mut self,
        function: u32,
        region: u32,
        definition: u32,
        at: DaeProvenance,
    ) -> Result<u32, DaeConstructionError> {
        let entry = self.definition_entry(function, definition, at)?;
        self.visit_expression(function, region, entry.rhs, true, entry.provenance)
    }

    fn definition_entry(
        &self,
        function: u32,
        definition: u32,
        at: DaeProvenance,
    ) -> Result<&FunctionDefinitionEntry, DaeConstructionError> {
        self.storage
            .functions
            .get(function as usize)
            .and_then(|entry| entry.definitions.get(definition as usize))
            .ok_or_else(|| unknown("callable function definition", definition, at))
    }

    fn definition_provenance(
        &self,
        function: u32,
        definitions: &[u32],
        at: DaeProvenance,
    ) -> Result<DaeProvenance, DaeConstructionError> {
        definitions
            .first()
            .copied()
            .map(|definition| self.definition_entry(function, definition, at))
            .transpose()?
            .map(|entry| entry.provenance)
            .ok_or(DaeConstructionError::IncompleteDefinition {
                kind: "callable conditional assignment group",
                index: 0,
                span: at.span(),
            })
    }

    fn visit_expression(
        &mut self,
        function: u32,
        region: u32,
        expression: u32,
        requested_call_projection: bool,
        at: DaeProvenance,
    ) -> Result<u32, DaeConstructionError> {
        if let Some(existing) = self.use_index.get(&(function, region, expression)).copied() {
            if requested_call_projection {
                self.issue_call_projection(function, region, expression, existing, at)?;
            }
            return Ok(existing);
        }
        let use_id = self.push_expression_use(function, region, expression, at)?;
        self.use_index
            .insert((function, region, expression), use_id);
        let node = self
            .storage
            .expressions
            .nodes
            .get(expression as usize)
            .cloned()
            .ok_or_else(|| unknown("function expression", expression, at))?;
        let operands =
            self.visit_expression_operands(function, region, expression, use_id, &node, at)?;
        self.expression_uses[use_id as usize].operands = operands;
        if requested_call_projection {
            self.issue_call_projection(function, region, expression, use_id, at)?;
        }
        Ok(use_id)
    }

    fn visit_expression_operands(
        &mut self,
        function: u32,
        region: u32,
        expression: u32,
        use_id: u32,
        node: &ExprNode,
        at: DaeProvenance,
    ) -> Result<Box<[u32]>, DaeConstructionError> {
        match node {
            ExprNode::Conditional { operands } => self
                .visit_conditional_expression(function, region, expression, use_id, *operands, at),
            ExprNode::Comprehension { domain, body } => self.visit_comprehension_expression(
                function, region, expression, use_id, *domain, *body,
            ),
            ExprNode::Call { owner, .. } => {
                if *owner != expression {
                    self.visit_expression(function, region, *owner, false, at)?;
                }
                self.visit_expression_children(function, region, node, at)
            }
            _ => self.visit_expression_children(function, region, node, at),
        }
    }

    fn visit_conditional_expression(
        &mut self,
        function: u32,
        region: u32,
        expression: u32,
        use_id: u32,
        operands: OperandRange,
        at: DaeProvenance,
    ) -> Result<Box<[u32]>, DaeConstructionError> {
        let raw = &self.storage.expressions.operands[operands.indices()];
        if raw.len() < 3 || raw.len().is_multiple_of(2) {
            return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
        }
        let guarded_operands = &raw[..raw.len() - 1];
        let conditions = guarded_operands
            .iter()
            .step_by(2)
            .copied()
            .collect::<Vec<_>>();
        let branches = guarded_operands
            .iter()
            .skip(1)
            .step_by(2)
            .copied()
            .map(|value| vec![value])
            .collect::<Vec<_>>();
        let fallback = raw
            .last()
            .copied()
            .ok_or(DaeConstructionError::ShapeMismatch { span: at.span() })?;
        let provenance = self.storage.expressions.provenance[expression as usize];
        let built = self.build_conditional_regions(ConditionalRegionInput {
            function,
            parent: region,
            source: CallableConditionalRegionSourceEntry::ExpressionUse(use_id),
            conditions: &conditions,
            branches: &branches,
            fallback: &[fallback],
            provenance,
        })?;
        self.record_expression_conditional(function, region, use_id, built, provenance)
    }

    fn record_expression_conditional(
        &mut self,
        function: u32,
        parent: u32,
        expression_use: u32,
        built: BuiltConditionalRegions,
        provenance: DaeProvenance,
    ) -> Result<Box<[u32]>, DaeConstructionError> {
        let branch_values = built
            .branch_uses
            .iter()
            .map(|branch| {
                branch
                    .first()
                    .copied()
                    .ok_or(DaeConstructionError::ShapeMismatch {
                        span: provenance.span(),
                    })
            })
            .collect::<Result<Vec<_>, _>>()?;
        self.expression_conditionals
            .push(CallableExpressionConditionalRegionsEntry {
                function,
                expression_use,
                parent,
                guards: built.guards,
                results: built.results,
                fallback: built.fallback,
                provenance,
            });
        Ok(built
            .condition_uses
            .iter()
            .copied()
            .zip(branch_values)
            .flat_map(|(condition, value)| [condition, value])
            .chain(built.fallback_uses)
            .collect())
    }

    fn visit_comprehension_expression(
        &mut self,
        function: u32,
        parent: u32,
        expression: u32,
        use_id: u32,
        domain: u32,
        body: u32,
    ) -> Result<Box<[u32]>, DaeConstructionError> {
        let provenance = self.storage.expressions.provenance[expression as usize];
        let body_region = self.push_region(
            function,
            Some(parent),
            CallableRegionActivationEntry::Enter,
            CallableRegionKindEntry::MapBody {
                expression_use: use_id,
                domain,
            },
            provenance,
        )?;
        self.map_bodies.push(CallableMapBodyEntry {
            function,
            expression_use: use_id,
            domain,
            parent,
            body: body_region,
            provenance,
        });
        let body_use = self.visit_expression(function, body_region, body, true, provenance)?;
        Ok(Box::new([body_use]))
    }

    fn visit_expression_children(
        &mut self,
        function: u32,
        region: u32,
        node: &ExprNode,
        at: DaeProvenance,
    ) -> Result<Box<[u32]>, DaeConstructionError> {
        let mut children = Vec::new();
        node.for_each_child(&self.storage.expressions, |child| children.push(child));
        children
            .into_iter()
            .map(|child| self.visit_expression(function, region, child, true, at))
            .collect()
    }

    fn issue_call_projection(
        &mut self,
        function: u32,
        region: u32,
        expression: u32,
        expression_use: u32,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let (owner_expression, output) =
            match self.storage.expressions.nodes.get(expression as usize) {
                Some(ExprNode::Call { owner, output, .. }) => (*owner, *output),
                Some(_) => return Ok(()),
                None => return Err(unknown("function expression", expression, at)),
            };
        let owner = self
            .use_index
            .get(&(function, region, owner_expression))
            .copied()
            .ok_or(DaeConstructionError::IncompleteDefinition {
                kind: "callable call owner use",
                index: expression_use,
                span: at.span(),
            })?;
        let projection = CallableCallProjectionUseEntry {
            expression_use,
            output,
            provenance: self.expression_uses[expression_use as usize].provenance,
        };
        let key = (function, region, owner_expression);
        if let Some(call_use) = self.call_use_index.get(&key).copied() {
            return self.insert_call_projection(call_use, projection);
        }
        let call_use = checked_u32(
            self.call_uses.len(),
            "callable call-use inventory",
            projection.provenance,
        )?;
        let provenance = self
            .storage
            .expressions
            .provenance
            .get(owner_expression as usize)
            .copied()
            .ok_or_else(|| unknown("callable call owner", owner_expression, at))?;
        self.call_use_index.insert(key, call_use);
        self.call_uses.push(PendingCallableCallUse {
            function,
            region,
            owner,
            projections: vec![projection],
            provenance,
        });
        Ok(())
    }

    fn insert_call_projection(
        &mut self,
        call_use: u32,
        projection: CallableCallProjectionUseEntry,
    ) -> Result<(), DaeConstructionError> {
        let expression_uses = &self.expression_uses;
        let entry = &mut self.call_uses[call_use as usize];
        if entry
            .projections
            .iter()
            .any(|existing| existing.expression_use == projection.expression_use)
        {
            return Ok(());
        }
        entry.projections.push(projection);
        entry
            .projections
            .sort_unstable_by_key(|item| expression_uses[item.expression_use as usize].expression);
        Ok(())
    }

    fn push_expression_use(
        &mut self,
        function: u32,
        region: u32,
        expression: u32,
        at: DaeProvenance,
    ) -> Result<u32, DaeConstructionError> {
        let provenance = self
            .storage
            .expressions
            .provenance
            .get(expression as usize)
            .copied()
            .ok_or_else(|| unknown("function expression", expression, at))?;
        let raw = checked_u32(
            self.expression_uses.len(),
            "callable expression-use inventory",
            provenance,
        )?;
        self.expression_uses.push(CallableExpressionUseEntry {
            function,
            region,
            expression,
            operands: Box::new([]),
            provenance,
        });
        Ok(raw)
    }

    fn push_region(
        &mut self,
        function: u32,
        parent: Option<u32>,
        activation: CallableRegionActivationEntry,
        kind: CallableRegionKindEntry,
        provenance: DaeProvenance,
    ) -> Result<u32, DaeConstructionError> {
        match (parent, activation) {
            (None, CallableRegionActivationEntry::Root) => {}
            (Some(_), CallableRegionActivationEntry::Enter)
            | (Some(_), CallableRegionActivationEntry::GuardTrue(_))
            | (Some(_), CallableRegionActivationEntry::GuardFalse(_)) => {}
            _ => {
                return Err(DaeConstructionError::ShapeMismatch {
                    span: provenance.span(),
                });
            }
        }
        let raw = checked_u32(
            self.regions.len(),
            "callable source-region inventory",
            provenance,
        )?;
        self.regions.push(CallableRegionEntry {
            function,
            parent,
            activation,
            kind,
            provenance,
        });
        Ok(raw)
    }

    fn issue_captures(&mut self) -> Result<(), DaeConstructionError> {
        for use_index in 0..self.expression_uses.len() {
            let expression_use = &self.expression_uses[use_index];
            let function = expression_use.function;
            let region = expression_use.region;
            let expression = expression_use.expression;
            let provenance = expression_use.provenance;
            let root = self
                .roots
                .get(function as usize)
                .copied()
                .ok_or_else(|| unknown("callable function", function, provenance))?;
            if region == root {
                continue;
            }
            let Some((source, source_region)) =
                self.capture_origin(function, region, expression, provenance)?
            else {
                continue;
            };
            if source_region == region {
                continue;
            }
            if !self.region_dominates(source_region, region, function, provenance)? {
                return Err(DaeConstructionError::InvalidCallableCaptureRegion {
                    source_region,
                    target_region: region,
                    span: provenance.span(),
                });
            }
            self.captures.push(CallableCaptureEntry {
                function,
                source_region,
                region,
                expression_use: checked_u32(
                    use_index,
                    "callable capture expression use",
                    provenance,
                )?,
                source,
                provenance,
            });
        }
        self.captures.sort_unstable_by_key(|capture| {
            (
                capture.function,
                capture.source_region,
                capture.region,
                capture.expression_use,
                capture.source,
            )
        });
        Ok(())
    }

    fn capture_origin(
        &self,
        function: u32,
        region: u32,
        expression: u32,
        provenance: DaeProvenance,
    ) -> Result<Option<(CallableCaptureSourceEntry, u32)>, DaeConstructionError> {
        let node = self
            .storage
            .expressions
            .nodes
            .get(expression as usize)
            .ok_or_else(|| unknown("function expression", expression, provenance))?;
        match node {
            ExprNode::Coordinate(Coordinate::FunctionParameter {
                function: owner,
                ordinal,
            }) => {
                self.expect_capture_function(function, *owner, provenance)?;
                let root = self
                    .roots
                    .get(function as usize)
                    .copied()
                    .ok_or_else(|| unknown("callable function", function, provenance))?;
                Ok(Some((
                    CallableCaptureSourceEntry::Parameter(*ordinal),
                    root,
                )))
            }
            ExprNode::Coordinate(Coordinate::Binder { domain, ordinal }) => Ok(Some((
                CallableCaptureSourceEntry::Binder {
                    domain: *domain,
                    ordinal: *ordinal,
                },
                self.binder_region(function, region, *domain, provenance)?,
            ))),
            ExprNode::FunctionValue {
                function: owner,
                definition_ordinal,
                ..
            }
            | ExprNode::FunctionFoldParameter {
                function: owner,
                definition_ordinal,
                ..
            }
            | ExprNode::FunctionFoldOutput {
                function: owner,
                definition_ordinal,
                ..
            } => {
                self.expect_capture_function(function, *owner, provenance)?;
                Ok(Some((
                    CallableCaptureSourceEntry::Definition(*definition_ordinal),
                    self.definition_region(function, *definition_ordinal, provenance)?,
                )))
            }
            _ => Ok(None),
        }
    }

    fn expect_capture_function(
        &self,
        expected: u32,
        found: u32,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        if expected == found {
            return Ok(());
        }
        Err(DaeConstructionError::InvalidFunctionScope {
            expected_function: Some(expected),
            found_function: found,
            span: provenance.span(),
        })
    }

    fn binder_region(
        &self,
        function: u32,
        mut region: u32,
        domain: u32,
        provenance: DaeProvenance,
    ) -> Result<u32, DaeConstructionError> {
        loop {
            let entry = self.region_entry(region, function, provenance)?;
            let binds_domain = match entry.kind {
                CallableRegionKindEntry::FoldBody { domain: owned, .. }
                | CallableRegionKindEntry::MapBody { domain: owned, .. } => owned == domain,
                _ => false,
            };
            if binds_domain {
                return Ok(region);
            }
            let Some(parent) = entry.parent else {
                return Err(DaeConstructionError::MissingCallableBinderRegion {
                    domain,
                    region,
                    span: provenance.span(),
                });
            };
            region = parent;
        }
    }

    fn region_dominates(
        &self,
        source: u32,
        mut target: u32,
        function: u32,
        provenance: DaeProvenance,
    ) -> Result<bool, DaeConstructionError> {
        loop {
            if source == target {
                return Ok(true);
            }
            let entry = self.region_entry(target, function, provenance)?;
            let Some(parent) = entry.parent else {
                return Ok(false);
            };
            target = parent;
        }
    }

    fn region_entry(
        &self,
        region: u32,
        function: u32,
        provenance: DaeProvenance,
    ) -> Result<&CallableRegionEntry, DaeConstructionError> {
        let entry = self
            .regions
            .get(region as usize)
            .ok_or_else(|| unknown("callable source region", region, provenance))?;
        if entry.function != function {
            return Err(DaeConstructionError::InvalidFunctionScope {
                expected_function: Some(function),
                found_function: entry.function,
                span: provenance.span(),
            });
        }
        Ok(entry)
    }

    fn definition_region(
        &self,
        function: u32,
        definition: u32,
        provenance: DaeProvenance,
    ) -> Result<u32, DaeConstructionError> {
        let entry = self
            .storage
            .functions
            .get(function as usize)
            .ok_or_else(|| unknown("callable function", function, provenance))?;
        match entry.definition_scopes.get(definition as usize).copied() {
            Some(Some(fold)) => self
                .fold_regions
                .get(&(function, fold))
                .copied()
                .ok_or_else(|| unknown("callable function fold", fold, provenance)),
            Some(None) => self
                .roots
                .get(function as usize)
                .copied()
                .ok_or_else(|| unknown("callable function", function, provenance)),
            None => Err(unknown(
                "callable function definition",
                definition,
                provenance,
            )),
        }
    }
}
