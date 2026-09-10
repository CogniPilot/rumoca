//! Construction-issued ownership of function SSA definitions by loop regions.
//!
//! A consumer that lowers one fold body has to separate two kinds of definition
//! it can reach from the loop's update expressions: the ones the loop itself
//! issues once per iteration, and the ones an enclosing scope already completed
//! and the body merely reads. Answering that from definition insertion order
//! would make the answer depend on an unstated arena invariant. Instead the DAE
//! records the open region at the instant each definition identity is issued
//! (see `FunctionEntry::definition_scopes`) and publishes the relation here as
//! a checked view, so the boundary is stated by construction and every consumer
//! reads the same fact.

use super::*;

impl<'dae> DaeView<'dae> {
    /// Construction-issued definition ownership of one function scope.
    ///
    /// `region` names the loop whose body the scope is, or `None` for the
    /// function's top-level body. The returned view answers, for every
    /// definition the function issued, whether that scope computes it or merely
    /// reads it - the boundary a fold-body lowering needs, stated by the
    /// construction that issued both identities.
    pub fn function_scope(
        self,
        function: FunctionId<'dae>,
        region: Option<FunctionFoldId<'dae>>,
    ) -> Option<FunctionScopeView<'dae>> {
        self.dae.storage.functions.get(function.index() as usize)?;
        if let Some(region) = region {
            if region.function() != function {
                return None;
            }
            self.function_fold(region)?;
        }
        Some(FunctionScopeView::new(self.dae, function, region))
    }
}

impl<'dae> FunctionFoldView<'dae> {
    /// Loop this one is lexically nested inside, if any.
    pub fn enclosing_fold(self) -> Option<FunctionFoldId<'dae>> {
        self.entry
            .parent
            .map(|parent| FunctionFoldId::from_raw(self.entry.function, parent))
    }

    /// Definition ownership this loop's body was issued.
    pub fn body_scope(self) -> FunctionScopeView<'dae> {
        FunctionScopeView::new(self.dae, self.id.function(), Some(self.id))
    }
}

/// Where one SSA definition sits relative to a loop region's body.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionScopeRelation {
    /// Issued while this region - or a loop nested inside it - was open.
    ///
    /// The region computes the definition itself, once per iteration.
    Region,
    /// Issued by a scope that strictly encloses this region.
    ///
    /// The value is complete before the region runs, so the region reads it as
    /// a loop-invariant capture rather than recomputing it.
    Enclosing,
    /// Issued by a region that neither contains nor is contained by this one.
    ///
    /// A sibling region's per-iteration definition is not reachable from a
    /// well-formed body: its binders are out of scope here. Consumers reject
    /// this rather than guessing which side of the boundary it belongs to.
    Disjoint,
}

/// Checked view of the definition ownership one region was issued.
///
/// Obtain one from [`DaeView::function_scope`](crate::DaeView::function_scope).
/// The view is bound to the exact function that owns the region, so a
/// definition of another function is never classified, only rejected.
#[derive(Clone, Copy)]
pub struct FunctionScopeView<'dae> {
    dae: &'dae Dae,
    function: FunctionId<'dae>,
    region: Option<FunctionFoldId<'dae>>,
}

impl<'dae> FunctionScopeView<'dae> {
    pub(super) fn new(
        dae: &'dae Dae,
        function: FunctionId<'dae>,
        region: Option<FunctionFoldId<'dae>>,
    ) -> Self {
        Self {
            dae,
            function,
            region,
        }
    }

    /// Function whose definitions this view classifies.
    pub const fn function(self) -> FunctionId<'dae> {
        self.function
    }

    /// Loop whose body this scope is, or `None` for the top-level body.
    pub const fn region(self) -> Option<FunctionFoldId<'dae>> {
        self.region
    }

    /// Classify one definition against this region's boundary.
    ///
    /// `None` means the identity is not one this function issued, which a
    /// branded identity can only be when it belongs to a sibling function.
    pub fn relation(self, definition: FunctionDefinitionId<'dae>) -> Option<FunctionScopeRelation> {
        if definition.function() != self.function {
            return None;
        }
        let entry = self
            .dae
            .storage
            .functions
            .get(self.function.index() as usize)?;
        let issued = *entry.definition_scopes.get(definition.ordinal() as usize)?;
        if self.contains_scope(entry, issued) {
            return Some(FunctionScopeRelation::Region);
        }
        if self.enclosed_by_scope(entry, issued) {
            return Some(FunctionScopeRelation::Enclosing);
        }
        Some(FunctionScopeRelation::Disjoint)
    }

    /// True when `scope` is this region or a loop nested inside it.
    fn contains_scope(self, entry: &'dae FunctionEntry, scope: Option<u32>) -> bool {
        let Some(region) = self.region else {
            // The top-level body encloses every region, so it owns every
            // definition the function issues.
            return true;
        };
        self.scope_chain(entry, scope)
            .any(|ancestor| ancestor == region.ordinal())
    }

    /// True when `scope` strictly encloses this region.
    fn enclosed_by_scope(self, entry: &'dae FunctionEntry, scope: Option<u32>) -> bool {
        let Some(region) = self.region else {
            return false;
        };
        let Some(scope) = scope else {
            // The top-level body strictly encloses every loop region.
            return true;
        };
        self.scope_chain(entry, self.parent_fold_ordinal(entry, region.ordinal()))
            .any(|ancestor| ancestor == scope)
    }

    /// The fold ordinal that structurally encloses `fold`, from the issued
    /// nesting forest. This walks IR fold identities, not rendered names.
    fn parent_fold_ordinal(self, entry: &'dae FunctionEntry, fold: u32) -> Option<u32> {
        self.fold_entry(entry, fold).and_then(|entry| entry.parent)
    }

    /// The loop ordinals enclosing `scope`, innermost first, `scope` included.
    ///
    /// Nesting is a forest whose parent ordinal is always strictly smaller than
    /// its child's, so the walk terminates at the top-level body.
    fn scope_chain(
        self,
        entry: &'dae FunctionEntry,
        scope: Option<u32>,
    ) -> impl Iterator<Item = u32> {
        let mut next = scope;
        std::iter::from_fn(move || {
            let fold = next?;
            next = self
                .parent_fold_ordinal(entry, fold)
                .filter(|parent| *parent < fold);
            Some(fold)
        })
    }

    fn fold_entry(self, entry: &'dae FunctionEntry, fold: u32) -> Option<&'dae FunctionFoldEntry> {
        let raw = *entry.folds.get(fold as usize)?;
        self.dae.storage.function_folds.get(raw as usize)
    }
}
