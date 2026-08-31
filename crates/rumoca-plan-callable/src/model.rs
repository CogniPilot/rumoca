//! Leaf value vocabulary shared by the plan's construction authority and its
//! borrowed views.
//!
//! Nothing here carries a plan invariant. The invariant-bearing plan root,
//! storage, and entries live with their sole mint in
//! [`crate::construction::plan`] and are unreachable from this module.

use rumoca_core::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallableScalarType {
    Boolean,
    Integer,
    Real,
}

/// Target-neutral facts retained for one Integer-to-Real consumer edge.
/// Representation-bound preparation decides rounding and status later.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallableIntegerSourceFact {
    ExactLiteral(i64),
    FullDomain,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallableValueType {
    pub(crate) scalar: CallableScalarType,
    pub(crate) dimensions: Box<[u32]>,
}

impl CallableValueType {
    pub const fn scalar(&self) -> CallableScalarType {
        self.scalar
    }

    pub fn dimensions(&self) -> &[u32] {
        &self.dimensions
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallableInterface {
    pub(crate) parameters: Box<[CallableValueType]>,
    pub(crate) results: Box<[CallableValueType]>,
}

impl CallableInterface {
    pub fn parameters(&self) -> &[CallableValueType] {
        &self.parameters
    }

    pub fn results(&self) -> &[CallableValueType] {
        &self.results
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CompactBinder {
    pub(crate) lower: i64,
    pub(crate) upper: i64,
    pub(crate) step: i64,
}

impl CompactBinder {
    pub const fn lower(self) -> i64 {
        self.lower
    }

    pub const fn upper(self) -> i64 {
        self.upper
    }

    pub const fn step(self) -> i64 {
        self.step
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompactDomain {
    pub(crate) binders: Box<[CompactBinder]>,
    pub(crate) scalar_count: u32,
}

impl CompactDomain {
    pub fn binders(&self) -> &[CompactBinder] {
        &self.binders
    }

    pub const fn scalar_count(&self) -> u32 {
        self.scalar_count
    }
}

/// Exact source-versus-plan population counts lent by one finished plan.
///
/// The plan mints every value here from its private storage; the fields stay
/// crate-private so no sibling module can restate a count the plan did not
/// observe.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CallableCounters {
    pub(crate) source_functions: usize,
    pub(crate) source_expressions: usize,
    pub(crate) source_definitions: usize,
    pub(crate) source_assertions: usize,
    pub(crate) source_conditionals: usize,
    pub(crate) source_calls: usize,
    pub(crate) source_folds: usize,
    pub(crate) evidence_discharges: usize,
    pub(crate) owners: usize,
    pub(crate) scopes: usize,
    pub(crate) values: usize,
    pub(crate) operations: usize,
    pub(crate) regions: usize,
    pub(crate) projections: usize,
    pub(crate) effects: usize,
    pub(crate) call_edges: usize,
}

impl CallableCounters {
    pub const fn source_functions(self) -> usize {
        self.source_functions
    }
    pub const fn source_expressions(self) -> usize {
        self.source_expressions
    }
    pub const fn source_definitions(self) -> usize {
        self.source_definitions
    }
    pub const fn source_assertions(self) -> usize {
        self.source_assertions
    }
    pub const fn source_conditionals(self) -> usize {
        self.source_conditionals
    }
    pub const fn source_calls(self) -> usize {
        self.source_calls
    }
    pub const fn source_folds(self) -> usize {
        self.source_folds
    }
    pub const fn evidence_discharges(self) -> usize {
        self.evidence_discharges
    }
    pub const fn owners(self) -> usize {
        self.owners
    }
    pub const fn scopes(self) -> usize {
        self.scopes
    }
    pub const fn values(self) -> usize {
        self.values
    }
    pub const fn operations(self) -> usize {
        self.operations
    }
    pub const fn regions(self) -> usize {
        self.regions
    }
    pub const fn projections(self) -> usize {
        self.projections
    }
    pub const fn effects(self) -> usize {
        self.effects
    }
    pub const fn call_edges(self) -> usize {
        self.call_edges
    }
}

/// One caller-to-callee edge and the exact span of the call occurrence that
/// issued it, as retained by plan construction.
///
/// The span is the recursive-call witness: an owner cycle is reported at the
/// call that closes it, never at a caller declaration.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct OwnerCallOccurrence {
    pub(crate) caller: u32,
    pub(crate) callee: u32,
    pub(crate) span: Span,
}
