//! Target-neutral, affine proof plans for checked DAE callable graphs.
//!
//! A plan is an auxiliary proof aggregate, not another compiler IR root. It
//! retains the exact DAE transferred into construction and lends only exact
//! correlated source views inside a fresh branded inspection. There is
//! deliberately no raw DAE view, wire form, evaluator, target policy, or
//! owned-DAE extraction API.

mod construction;
mod model;
mod view;

pub use construction::{
    CallableAssertionSource, CallableCallProjectionSource, CallableCallSource,
    CallableConditionalSource, CallableDefinitionSource, CallableExpressionSource,
    CallableFoldSource, CallableFunctionSource, CallableOperandEdge, CallablePlan,
    CallablePlanConstruction, ConditionalGroupConstruction, ConditionalRegionConstruction,
    ConstructionOwnerId, ConstructionScopeId, ConstructionValueId, MapRegionConstruction,
    PlanConstructionError,
};
pub use model::{
    CallableCounters, CallableIntegerSourceFact, CallableInterface, CallableScalarType,
    CallableValueType, CompactBinder, CompactDomain,
};
pub use view::{
    CallableAssertionRelationView, CallableCallEdgeView, CallableCallOccurrenceSourceView,
    CallableCallProjectionSourceView, CallableConditionalGroupSourceView,
    CallableConditionalRegionView, CallableEffectId, CallableEffectView, CallableMapRegionView,
    CallableOperationDetailView, CallableOperationId, CallableOperationSourceView,
    CallableOperationView, CallableOwnerId, CallableOwnerView, CallablePlanView,
    CallableProjectionId, CallableProjectionView, CallableRegionDetailView, CallableRegionView,
    CallableScopeId, CallableScopeView, CallableValueId, CallableValueProducer, CallableValueView,
};
