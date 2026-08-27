//! Connect endpoints checked against the declarations they name
//! (MLS §10.5 / §9.1).
//!
//! [`ConnectionEndpointIndex`] indexes every declared component occurrence of an
//! instance overlay, and judges connect endpoints against that index: the
//! `EF026` rejection of a subscript applied to a declaration proven to carry no
//! dimensions, and the expandable-connector ownership lookup that runs before
//! connection-set construction. The acceptance contract this check implements
//! is stated in the [parent module docs](super) (SPEC_0008); the scope of the
//! rejection is stated below.
//!
//! ## Scope of the `EF026` rejection, and what it does not cover
//!
//! The rejection is *narrow on purpose*: it fires only for a literal
//! integer subscript applied to a declaration this phase can prove is rank-zero.
//! Everything below is a shape MLS §9.1/§10.5 also governs that this check
//! structurally cannot see or deliberately does not judge. Each is named with
//! its current behaviour and its owner so the contract is not read as a promise
//! this module does not keep (SPEC_0008).
//!
//! - **Over-subscripting a declared array** (`connect(a[1,2], b)` for `C a[2]`).
//!   Not judged here: the check accepts any non-zero declared rank, because the
//!   flat representation of a collapsed connector-array member cannot be told
//!   apart from an over-subscripted one. Current behaviour: the endpoint matches
//!   nothing, the connection contributes no equations, a duplicate flow-zero
//!   equation keeps the ED001 balance count satisfied, and the model dies later
//!   as an `E011` structural singularity. Tracked as task #82; typecheck's
//!   `ET009` (MLS §10.5.1) already implements this rule for *expression*
//!   references and is the natural owner once it walks connect arguments.
//! - **Non-literal subscripts** — `end`, `:`, and any subscript that is not an
//!   evaluated integer. These are erased upstream: `ast::QualifiedName` carries
//!   `Vec<i64>` per segment, so by the time a connection reaches this module the
//!   distinction is gone, and resolve's `ER085` skips `Subscript::Range` and
//!   treats `end` as evaluable. Current behaviour: `connect(m.u[end], s)`
//!   fabricates members on a scalar and reports `ED001` with a wrong story;
//!   `connect(m.u[2,:], s)` drops the connection entirely and dies as `E011`.
//!   Tracked as task #87; the fix belongs upstream, where the subscript form
//!   still exists.
//! - **Slices of *composite* connector arrays** (a `C u[2,3]` whose `C` has
//!   members). `find_sub_variables_indexed` drops these connections entirely
//!   (zero equations generated). Tracked as task #87. The leaf-counting fix in
//!   point 1 of the [parent module docs](super) covers *primitive* connector
//!   arrays only.
//! - **Redeclared array dimensions** (MLS §7.3). A redeclaration may replace a
//!   component together with its dimensions, and reaches a component along four
//!   routes: an `extends` modification
//!   (`model M extends Base(redeclare C a[2])` over `replaceable C a;`); a
//!   modifier on an enclosing declaration
//!   (`Holder h(redeclare C a[2]); … connect(h.a[1], …)`); and the nested form
//!   of each, where the redeclaration sits inside an ordinary modification one
//!   or more levels down (`extends Wrap(h(redeclare C a[2]))` and
//!   `Wrap w(h(redeclare C a[2]))`), which is where the redeclare flag lives on
//!   the enclosing class-modification rather than on the modification a caller
//!   sees first. Instantiation keeps
//!   only the redeclared type in both cases, so the component arrives here
//!   wearing the *original* declaration's shape. `EF026` is suppressed for such
//!   a component and for everything instantiated beneath it (that is what
//!   `ConnectionEndpointIndex::rank_is_authoritative` and the
//!   `InstanceData::had_redeclare` marker it reads are for), but the lost
//!   dimensions are still lost: a rank-raising redeclare yields a scalar and an
//!   `ED001` imbalance, and a same-rank extent redeclare (`C a[1]` redeclared
//!   to `C a[2]`) keeps the old extent and silently drops `connect(a[2], …)`.
//!   Both are pre-existing instantiate gaps owned by task #92, not connection
//!   gaps. Inheritance on its own is *not* one of these cases: a component
//!   inherited without any redeclaration keeps an authoritative rank and is
//!   judged normally, and a redeclaration on one declaration does not touch its
//!   siblings (`Holder h; Holder h2(redeclare C a[2]);` leaves `h.a` judged).
//!
//!   The marker's granularity is the *declaration the redeclaration was written
//!   on*, not the member it names, so a redeclare of one member also suppresses
//!   the check for that declaration's other members
//!   (`Holder h(redeclare C a[2]); connect(h.bb[1], s)` abstains where OMC
//!   reports "Wrong number of subscripts in h.bb[1]"). Narrowing this needs the
//!   redeclared member's own identity to survive instantiation, which is the
//!   same #92 gap; abstaining is the safe direction while it does not.

use rumoca_core::Span;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;

use crate::errors::FlattenError;

pub(super) struct ConnectionEndpointIndex {
    declared: IndexMap<rumoca_core::ComponentPath, DeclaredEndpoint>,
    expandable_owners: IndexMap<rumoca_core::ComponentPath, rumoca_core::InstanceId>,
}

/// The part of a declaration a connect endpoint is checked against.
struct DeclaredEndpoint {
    /// Number of declared array dimensions (MLS §10.5 subscript budget).
    dimension_rank: usize,
    /// True when the declaration still carries dimension *expressions*.
    ///
    /// Typecheck fills `dims` but never clears `dims_expr`, so this stays true
    /// for every symbolically-shaped declaration even after its sizes are
    /// known. It is therefore only ever read to *widen* acceptance — a
    /// declaration that has any dimension expression at all is never treated as
    /// proven rank-zero.
    has_unevaluated_dimensions: bool,
    /// True when instantiation consumed a redeclare modification for this
    /// component, so its surviving `dims` are this compiler's residue of the
    /// *original* declaration rather than a statement about the source. See
    /// [`ConnectionEndpointIndex::rank_is_authoritative`].
    had_redeclare: bool,
    /// Span of the component declaration, when instantiation preserved one.
    declaration_span: Option<Span>,
}

impl ConnectionEndpointIndex {
    pub(super) fn new(overlay: &ast::InstanceOverlay) -> Self {
        let mut declared = IndexMap::default();
        let mut expandable_owners = IndexMap::default();
        for (instance_id, component) in &overlay.components {
            let path = component.qualified_name.to_component_path();
            declared.insert(
                path.clone(),
                DeclaredEndpoint {
                    dimension_rank: component.dims.len(),
                    has_unevaluated_dimensions: !component.dims_expr.is_empty(),
                    had_redeclare: component.had_redeclare,
                    declaration_span: component
                        .source_location
                        .has_source()
                        .then(|| component.source_location.span()),
                },
            );
            if component.is_expandable_connector_type {
                expandable_owners.insert(path, *instance_id);
            }
        }
        Self {
            declared,
            expandable_owners,
        }
    }

    /// Whether a component's declared rank is evidence about the *source*
    /// rather than an artifact of this compiler.
    ///
    /// MLS §7.3 lets a redeclaration replace a component *and its array
    /// dimensions*, either through an `extends` modification
    /// (`extends Base(redeclare C a[2])`) or through a modifier on an enclosing
    /// declaration (`Holder h(redeclare C a[2])`). Instantiation consumes only
    /// the redeclared *type*, so the redeclared dimensions never reach
    /// `InstanceData::dims` and the component arrives here still wearing the
    /// original declaration's shape. Reporting that shape as a user error would
    /// blame the model for this compiler's gap.
    ///
    /// Instantiation marks every component it consumed a redeclaration for
    /// (`InstanceData::had_redeclare`). A redeclaration on an *enclosing*
    /// declaration invalidates everything instantiated beneath it — `h.a`
    /// carries the dimensions lost by `Holder h(redeclare C a[2])` — so the
    /// rank is authoritative only when neither the component itself nor any
    /// ancestor on its path was redeclared.
    fn rank_is_authoritative(&self, path: &rumoca_core::ComponentPath) -> bool {
        let mut candidate = Some(path.clone());
        while let Some(current) = candidate {
            if self
                .declared
                .get(&current)
                .is_some_and(|declared| declared.had_redeclare)
            {
                return false;
            }
            candidate = current.parent();
        }
        true
    }

    /// Check both endpoints of every connection against
    /// [`Self::check_endpoint_subscripts`].
    pub(super) fn check_connection_endpoint_subscripts(
        &self,
        connections: &[&ast::InstanceConnection],
    ) -> Result<(), FlattenError> {
        for conn in connections {
            self.check_endpoint_subscripts(&conn.a, conn.span)?;
            self.check_endpoint_subscripts(&conn.b, conn.span)?;
        }
        Ok(())
    }

    /// MLS §10.5 / §9.1: reject one connect endpoint whose subscripts index a
    /// component whose declaration provably has no dimensions at all.
    ///
    /// Acceptance comes first (SPEC_0008; see the module docs). This predicate
    /// accepts, in the order tested:
    ///
    /// 1. the subscripted path itself names a declared component occurrence
    ///    (the scalarized element of a connector array);
    /// 2. no declaration for the unsubscripted base is in view (expandable-bus
    ///    members and any other path this phase does not own);
    /// 3. the declaration has **any** dimension at all (`dimension_rank > 0`) —
    ///    including the over-subscripted case `connect(a[1,2], b)` on a rank-1
    ///    `a`, which this check deliberately does not judge (see the module
    ///    docs for who currently catches it);
    /// 4. the declaration carries dimension expressions
    ///    ([`DeclaredEndpoint::has_unevaluated_dimensions`]);
    /// 5. the rank is not authoritative evidence about the source, because a
    ///    redeclare modification was consumed for the component or for an
    ///    enclosing one ([`Self::rank_is_authoritative`]).
    ///
    /// Only the remainder is rejected — a subscript applied to a declaration
    /// proven to have no dimensions — because dropping that subscript would
    /// silently connect the whole component it was meant to index.
    fn check_endpoint_subscripts(
        &self,
        endpoint: &ast::QualifiedName,
        span: Span,
    ) -> Result<(), FlattenError> {
        for (part_index, (_, subscripts)) in endpoint.parts.iter().enumerate() {
            if subscripts.is_empty() {
                continue;
            }
            let mut prefix = ast::QualifiedName {
                parts: endpoint.parts[..=part_index].to_vec(),
            };
            if self.declared.contains_key(&prefix.to_component_path()) {
                continue;
            }
            prefix.parts[part_index].1.clear();
            let base_path = prefix.to_component_path();
            let Some(declared) = self.declared.get(&base_path) else {
                continue;
            };
            if declared.dimension_rank > 0
                || declared.has_unevaluated_dimensions
                || !self.rank_is_authoritative(&base_path)
            {
                continue;
            }
            let Some(declaration_span) = declared.declaration_span else {
                return Err(FlattenError::missing_source_context(format!(
                    "declaration of `{base_path}` subscripted by connect endpoint `{}` has no source location",
                    endpoint.to_flat_string()
                )));
            };
            return Err(FlattenError::subscripted_dimensionless_connector(
                endpoint.to_flat_string(),
                base_path.to_string(),
                span,
                declaration_span,
            ));
        }
        Ok(())
    }

    pub(super) fn needs_expandable_augmentation(&self, endpoint: &ast::QualifiedName) -> bool {
        let endpoint_path = endpoint.to_component_path();
        if self.declared.contains_key(&endpoint_path) {
            return false;
        }
        // An indexed leaf of a declared array member is not a new expandable
        // member. Its declaration owns the array shape; the connection merely
        // selects one element (MLS §10.5). Keep the structured endpoint for
        // all parent lookup below so an actually absent member still requires
        // §9.1.3 augmentation.
        let mut declared_leaf = endpoint.clone();
        if let Some((_, subscripts)) = declared_leaf.parts.last_mut()
            && !subscripts.is_empty()
        {
            subscripts.clear();
            if self
                .declared
                .contains_key(&declared_leaf.to_component_path())
            {
                return false;
            }
        }
        let mut owner = endpoint_path.parent();
        while let Some(candidate) = owner {
            if self.expandable_owners.contains_key(&candidate) {
                return true;
            }
            owner = candidate.parent();
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn indexed_declared_expandable_member_needs_no_augmentation() {
        let mut overlay = ast::InstanceOverlay::new();
        overlay.add_component(ast::InstanceData {
            instance_id: rumoca_core::InstanceId(1),
            qualified_name: ast::QualifiedName::from_ident("telemetry"),
            is_expandable_connector_type: true,
            ..Default::default()
        });
        overlay.add_component(ast::InstanceData {
            instance_id: rumoca_core::InstanceId(2),
            owner_class_id: Some(rumoca_core::InstanceId(1)),
            qualified_name: ast::QualifiedName::from_dotted("telemetry.motor"),
            dims: vec![4],
            ..Default::default()
        });
        let index = ConnectionEndpointIndex::new(&overlay);
        let mut element = ast::QualifiedName::from_ident("telemetry");
        element.push("motor".to_owned(), vec![1]);

        assert!(!index.needs_expandable_augmentation(&element));

        let absent = ast::QualifiedName::from_dotted("telemetry.absent");
        assert!(index.needs_expandable_augmentation(&absent));
    }
}
