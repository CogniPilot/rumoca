//! Construction-issued region ownership of function SSA definitions.
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
        let scope = if let Some(region) = region {
            if region.function() != function {
                return None;
            }
            Some(self.function_fold(region)?.entry.scope)
        } else {
            None
        };
        Some(FunctionScopeView::new(self.dae, function, region, scope))
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
        FunctionScopeView::new(
            self.dae,
            self.id.function(),
            Some(self.id),
            Some(self.entry.scope),
        )
    }
}

/// Where one SSA definition sits relative to a region's body.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionScopeRelation {
    /// Issued while this region or a nested region was open.
    Region,
    /// Issued by a scope that strictly encloses this region.
    ///
    /// This proves ancestry. The constructor's current-definition admission
    /// separately proves which enclosing values are available as captures.
    Enclosing,
    /// Issued by a region that neither contains nor is contained by this one.
    ///
    /// A sibling region's definition is not reachable from a
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
    scope: Option<u32>,
}

impl<'dae> FunctionScopeView<'dae> {
    pub(super) fn new(
        dae: &'dae Dae,
        function: FunctionId<'dae>,
        region: Option<FunctionFoldId<'dae>>,
        scope: Option<u32>,
    ) -> Self {
        Self {
            dae,
            function,
            region,
            scope,
        }
    }

    /// Function whose definitions this view classifies.
    pub const fn function(self) -> FunctionId<'dae> {
        self.function
    }

    /// Loop directly associated with this scope view, if any.
    ///
    /// Absence of a loop identity does not itself certify a top-level scope.
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

    /// True when `scope` is this region or a region nested inside it.
    fn contains_scope(self, entry: &'dae FunctionEntry, scope: Option<u32>) -> bool {
        let Some(region) = self.scope else {
            // The top-level body encloses every region, so it owns every
            // definition the function issues.
            return true;
        };
        self.scope_chain(entry, scope)
            .any(|ancestor| ancestor == region)
    }

    /// True when `scope` strictly encloses this region.
    fn enclosed_by_scope(self, entry: &'dae FunctionEntry, scope: Option<u32>) -> bool {
        let Some(region) = self.scope else {
            return false;
        };
        let Some(scope) = scope else {
            // The top-level body strictly encloses every region.
            return true;
        };
        self.scope_chain(
            entry,
            entry
                .scopes
                .get(region as usize)
                .and_then(|scope| scope.parent),
        )
        .any(|ancestor| ancestor == scope)
    }

    /// The region ordinals enclosing `scope`, innermost first, `scope` included.
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
            let scope = next?;
            next = entry.scopes.get(scope as usize)?.parent;
            Some(scope)
        })
    }
}
