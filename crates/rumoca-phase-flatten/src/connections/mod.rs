//! Connection processing for the flatten phase (MLS §9).
//!
//! This module expands connect() statements into connection equations:
//! - Flow variables: sum to zero (Kirchhoff's current law)
//! - Non-flow (potential) variables: are equal
//!
//! ## MLS §9.2 Connection Semantics
//!
//! For each connection set:
//! - Potential (non-flow, non-stream) variables: equality equations
//!   `v1 = v2 = ... = vn` (n-1 equations)
//! - Flow variables: sum equation `f1 + f2 + ... + fn = 0` (1 equation)
//! - Stream variables: no ordinary equality equation for inside connectors;
//!   stream values are consumed through `inStream`/`actualStream` semantics
//!   (MLS §15).
//!
//! The sign convention for flow variables depends on whether the connector
//! is an inside or outside connector (MLS §9.2):
//! - Inside connector (component port): sign = +1
//! - Outside connector (model boundary): sign = -1
//!
//! ## Acceptance contract: subscripted connect endpoints (SPEC_0008)
//!
//! A `connect` argument may name one element or one slice of an array of
//! connectors (MLS §9.1.1: "the arguments of a connect-equation are component
//! references [...] the reference may include array subscripts"), and MLS §10.5
//! defines what such a reference denotes: a subscript consumes one *leading*
//! declared dimension, so a reference `a[i]` to a declaration `a[n, m]` denotes
//! a value of dimensions `[m]`, and `a[i, j]` denotes a scalar.
//!
//! Two consequences bind this module, and both are stated as acceptance
//! *before* any rejection:
//!
//! 1. **Accepted when representable — element/slice endpoints count their own leaves.** The number
//!    of scalar equations generated for a connection set is the number of
//!    scalar leaves of its members (MLS §9.2: one equality per matched
//!    potential leaf, one sum per flow leaf; MLS §4.8 counts those scalars when
//!    balancing the model). The leaf count of an endpoint is therefore the
//!    product of the dimensions the endpoint *denotes* (MLS §10.5), never a
//!    constant 1 chosen because the endpoint carries a subscript. Modelica
//!    permits `connect(a[i], b)` where both endpoints denote `Real[m]`; when
//!    the selection denotes the complete compact declaration (or Instance has
//!    already scalarized it), lowering produces `m` scalar equations. A strict
//!    subdomain of one compact Flat declaration lowers the same way: the
//!    member's owner carries its leading selection, the transaction marks
//!    exactly those elements in the declaration's checked
//!    [`flat::ConnectedDomain`], and the MLS §9.2 zero-flow planner emits one
//!    `= 0` row per untouched flow element. A strict subdomain of a compact
//!    *stream* declaration is still refused with its source span, because the
//!    MLS §15.2 mixing pairs a stream member with its connector's flow member
//!    per declaration rather than per element.
//!    Rejection as dimension-incompatible
//!    (CONN-008, MLS §9.2 "same named elements with the same dimensions") is
//!    admissible only when the denoted dimensions actually differ. This holds
//!    for endpoints whose base is one declared array in the flat model — a
//!    *primitive* connector array, or an array member of a connector. A slice of
//!    a *composite* connector array never reaches leaf counting at all; see the
//!    scope section below.
//!
//! 2. **Accepted syntax — subscripts that a declaration can carry.** Endpoint
//!    validation accepts a subscript whenever the subscripted path names a declared
//!    component occurrence, whenever no declaration for its base is in view
//!    here, whenever its declaration carries *any* dimension, whenever the
//!    declaration still carries dimension expressions, and whenever the rank
//!    this phase can see is not authoritative evidence about the source (see
//!    `declared_rank_is_authoritative`). Only the provably impossible remainder
//!    is rejected: a subscript applied to a component whose declaration is
//!    proven to have *no* dimensions at all selects along a dimension MLS §10.5
//!    does not give it, so it is reported against both the connect endpoint and
//!    the declaration site (`EF026`) instead of being dropped. Dropping it would
//!    silently connect the whole component the subscript was meant to index.
//!    This syntax acceptance is distinct from the later connected-subdomain
//!    capability check described above.
//!
//! The scope of that `EF026` rejection — every shape MLS §9.1/§10.5 also
//! governs that the check structurally cannot see or deliberately does not
//! judge, each named with its current behaviour and its owner — is stated in
//! the [`endpoint_subscripts`] module docs.
//!
//! ## Acceptance contract: matched primitive member pairs (SPEC_0008)
//!
//! Expanding a `connect` pairs the primitive members of the two connectors by
//! name, and MLS §9.3 then constrains what such a matched pair may be: "flow
//! variables may only connect to other flow variables, stream variables only to
//! other stream variables" (CONN-003 and CONN-030), and "the primitive
//! components may only connect parameter variables to parameter variables and
//! constant variables to constant variables" (CONN-028). The stream and
//! variability clauses are decided in one place,
//! [`member_pairing::classify_connection_member_pair`], which
//! [`connect_primitive_vars`] and [`connect_sub_variable`] consult before
//! joining any pair to a connection set; the flow clause stays with
//! [`validate_flow_consistency`]. That module's docs state the full acceptance
//! contract — what is connected, what deliberately generates no equation, what
//! is not judged for lack of evidence — and the scope of the `EF027`/`EF028`
//! rejections it raises.

use rumoca_core::{ProvenanceSpan, Span, TypeId};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use rumoca_ir_flat as flat;
use rustc_hash::FxHashMap;

use crate::errors::FlattenError;
use crate::path_utils::{segments as path_segments_of, strip_array_index};

mod endpoint_subscripts;
mod equality_projection;
mod equation_generation;
mod member_pairing;
mod path_index;
mod selection_evidence;
mod source_inventory;
mod stream_operators;
mod transaction;
mod validation;
use endpoint_subscripts::*;
use equality_projection::*;
use equation_generation::*;
pub(crate) use equation_generation::{connection_involves_disabled, process_connections};
use member_pairing::{
    MemberPairing, classify_connection_member_pair, is_flow_variable, is_stream_variable,
};
use path_index::*;
pub(crate) use selection_evidence::declared_array_element_evidence;
use selection_evidence::*;
use source_inventory::*;

pub(crate) fn stream_operator_identities(
    tree: &ast::ClassTree,
    span: Span,
) -> Result<stream_operators::StreamOperatorIdentities, FlattenError> {
    stream_operators::StreamOperatorIdentities::from_tree(tree, span)
}
use transaction::*;
use validation::*;

/// Refuse before any consumer enumerates a compact connection family whose
/// scalar compatibility view exceeds this phase's eager structural budget.
/// SPEC_0032 keeps the family authoritative; until connection-set construction
/// consumes that compact owner directly, materializing an unbounded view is an
/// explicit unsupported boundary rather than an allocation attempt.
pub(crate) fn ensure_connection_scalarization_budget(
    overlay: &ast::InstanceOverlay,
) -> Result<(), FlattenError> {
    let limit = crate::equations::MAX_EAGER_RANGE_ELEMENTS;
    let mut total = 0usize;
    for class in overlay.classes.values() {
        for connection in &class.connections {
            add_active_connection_scalarization_count(
                connection,
                &overlay.disabled_components,
                &mut total,
                limit,
            )?;
        }
    }
    Ok(())
}

fn add_active_connection_scalarization_count(
    connection: &ast::InstanceConnection,
    disabled_components: &indexmap::IndexSet<rumoca_core::ComponentPath>,
    total: &mut usize,
    limit: usize,
) -> Result<(), FlattenError> {
    match connection {
        ast::InstanceConnection::Scalar(connection) => {
            if !equation_generation::connection_involves_disabled(connection, disabled_components) {
                add_connection_scalarization_count(total, 1, limit, connection.span())?;
            }
            Ok(())
        }
        ast::InstanceConnection::Family(family) => add_active_family_scalarization_count(
            connection,
            family,
            disabled_components,
            total,
            limit,
        ),
    }
}

fn add_active_family_scalarization_count(
    connection: &ast::InstanceConnection,
    family: &ast::InstanceConnectionFamily,
    disabled_components: &indexmap::IndexSet<rumoca_core::ComponentPath>,
    total: &mut usize,
    limit: usize,
) -> Result<(), FlattenError> {
    if connection_family_is_wholly_disabled(family, disabled_components) {
        return Ok(());
    }
    let count = family
        .domain()
        .scalar_count()
        .map_err(|reason| crate::structured_connection_error(reason.to_string()))?;
    if count == 0 {
        return Ok(());
    }
    if disabled_components.is_empty() {
        return add_connection_scalarization_count(total, count, limit, family.span());
    }

    // A family may straddle individually disabled array occurrences. Count
    // the same active scalar view every downstream consumer sees, stopping as
    // soon as the active budget is exceeded. A parent-disabled family took the
    // constant-time branch above.
    for member in
        rumoca_eval_ast::connection::scalar_connection_view(std::slice::from_ref(connection))
    {
        let member = member.map_err(crate::structured_connection_error)?;
        if equation_generation::connection_involves_disabled(&member, disabled_components) {
            continue;
        }
        add_connection_scalarization_count(total, 1, limit, family.span())?;
    }
    Ok(())
}

fn add_connection_scalarization_count(
    total: &mut usize,
    count: usize,
    limit: usize,
    span: Span,
) -> Result<(), FlattenError> {
    *total = total
        .checked_add(count)
        .ok_or(FlattenError::RangeMaterializationLimit {
            element_count: u128::MAX,
            limit,
            span,
        })?;
    if *total > limit {
        return Err(FlattenError::RangeMaterializationLimit {
            element_count: *total as u128,
            limit,
            span,
        });
    }
    Ok(())
}

fn connection_family_is_wholly_disabled(
    family: &ast::InstanceConnectionFamily,
    disabled_components: &indexmap::IndexSet<rumoca_core::ComponentPath>,
) -> bool {
    [family.a(), family.b()].into_iter().any(|endpoint| {
        disabled_components
            .iter()
            .any(|disabled| connection_family_endpoint_starts_with(endpoint, disabled))
    })
}

fn connection_family_endpoint_starts_with(
    endpoint: &ast::InstanceConnectionEndpoint,
    disabled: &rumoca_core::ComponentPath,
) -> bool {
    if disabled.is_root() || disabled.len() > endpoint.parts().len() {
        return false;
    }
    endpoint
        .parts()
        .iter()
        .zip(disabled.parts())
        .all(|((name, forms), disabled_part)| {
            if disabled_part == name {
                return true;
            }
            let constants = forms
                .iter()
                .map(|form| {
                    form.coeffs
                        .iter()
                        .all(|coefficient| *coefficient == 0)
                        .then_some(form.constant)
                })
                .collect::<Option<Vec<_>>>();
            constants.is_some_and(|constants| {
                let rendered = constants
                    .iter()
                    .map(i64::to_string)
                    .collect::<Vec<_>>()
                    .join(",");
                disabled_part == &format!("{name}[{rendered}]")
            })
        })
}

fn qualified_connection_endpoint_starts_with(
    endpoint: &ast::QualifiedName,
    disabled: &rumoca_core::ComponentPath,
) -> bool {
    if disabled.is_root() || disabled.len() > endpoint.parts.len() {
        return false;
    }
    endpoint
        .parts
        .iter()
        .zip(disabled.parts())
        .all(|((name, subscripts), disabled_part)| {
            if disabled_part == name {
                return true;
            }
            let rendered = subscripts
                .iter()
                .map(i64::to_string)
                .collect::<Vec<_>>()
                .join(",");
            disabled_part == &format!("{name}[{rendered}]")
        })
}

/// Context for array output connection operations.
/// Groups related parameters to reduce function argument count.
struct ArrayConnCtx<'a> {
    path_a: &'a str,
    path_b: &'a str,
    var_a: &'a rumoca_core::VarName,
    var_b: &'a rumoca_core::VarName,
    a_is_primitive: bool,
    b_is_primitive: bool,
    span: Span,
}

struct ConnectionBuildCtx<'a> {
    flat: &'a flat::Model,
    var_index: &'a ConnectionVarIndex,
    flow_pairs: &'a mut Vec<(rumoca_core::VarName, rumoca_core::VarName)>,
    potential_uf: &'a mut UnionFind,
    stream_uf: &'a mut UnionFind,
    span: Span,
}

struct ConnectionProcessCtx<'a> {
    flat: &'a flat::Model,
    var_index: &'a ConnectionVarIndex,
    prefix_children: &'a FxHashMap<String, Vec<rumoca_core::VarName>>,
}

struct ArrayExpandedRouteCtx<'a> {
    array_var: &'a rumoca_core::VarName,
    expanded_path: &'a str,
    expanded_vars: &'a [rumoca_core::VarName],
    flat: &'a flat::Model,
    span: Span,
}

/// Precomputed lookup structures for connection path matching.
///
/// Built once per connection-processing pass to avoid repeated full scans and
/// repeated `path_segments_of` work in hot loops.
struct ConnectionVarIndex {
    /// Variables indexed by normalized base prefix (indices stripped), for
    /// connector-subvariable expansion lookups.
    subvars_by_base_prefix: FxHashMap<String, Vec<rumoca_core::VarName>>,
    /// Variables indexed by normalized full path (indices stripped), for exact
    /// path matching with array expansion.
    exact_by_base_path: FxHashMap<String, Vec<rumoca_core::VarName>>,
    /// Parsed path parts per variable name.
    parsed_parts_by_var: FxHashMap<rumoca_core::VarName, Vec<String>>,
}

impl ConnectionVarIndex {
    fn new(flat: &flat::Model) -> Self {
        Self::from_var_names(flat.variables.keys())
    }

    fn from_var_names<'a, I>(var_names: I) -> Self
    where
        I: IntoIterator<Item = &'a rumoca_core::VarName>,
    {
        let mut subvars_by_base_prefix: FxHashMap<String, Vec<rumoca_core::VarName>> =
            FxHashMap::default();
        let mut exact_by_base_path: FxHashMap<String, Vec<rumoca_core::VarName>> =
            FxHashMap::default();
        let mut parsed_parts_by_var: FxHashMap<rumoca_core::VarName, Vec<String>> =
            FxHashMap::default();

        for var_name in var_names {
            let parsed_parts: Vec<String> = path_segments_of(var_name.as_str())
                .into_iter()
                .map(std::borrow::ToOwned::to_owned)
                .collect();
            if parsed_parts.is_empty() {
                continue;
            }

            parsed_parts_by_var.insert(var_name.clone(), parsed_parts.clone());

            let exact_key = normalized_base_key_from_owned_parts(&parsed_parts);
            exact_by_base_path
                .entry(exact_key)
                .or_default()
                .push(var_name.clone());

            for prefix_len in 1..parsed_parts.len() {
                let key = normalized_base_key_from_owned_parts(&parsed_parts[..prefix_len]);
                subvars_by_base_prefix
                    .entry(key)
                    .or_default()
                    .push(var_name.clone());
            }
        }

        Self {
            subvars_by_base_prefix,
            exact_by_base_path,
            parsed_parts_by_var,
        }
    }

    fn parsed_parts(&self, var_name: &rumoca_core::VarName) -> Option<&[String]> {
        self.parsed_parts_by_var.get(var_name).map(Vec::as_slice)
    }

    fn subvar_candidates(&self, normalized_prefix: &str) -> Option<&[rumoca_core::VarName]> {
        self.subvars_by_base_prefix
            .get(normalized_prefix)
            .map(Vec::as_slice)
    }

    fn exact_candidates(&self, normalized_path: &str) -> Option<&[rumoca_core::VarName]> {
        self.exact_by_base_path
            .get(normalized_path)
            .map(Vec::as_slice)
    }
}

/// Per-connection lookup index for matching sub-variables on one connector side.
///
/// Built once for `(path_b, subs_b)` and reused for each sub-variable from the
/// opposite connector to avoid repeated scans in hot loops.
struct ConnectionSubMatchIndex {
    path_explicit_index_count: usize,
    exact_by_suffix: FxHashMap<String, rumoca_core::VarName>,
    by_suffix_and_indices: FxHashMap<String, rumoca_core::VarName>,
}

impl ConnectionSubMatchIndex {
    fn new(path: &str, subs: &[rumoca_core::VarName], var_index: &ConnectionVarIndex) -> Self {
        let path_segments = path_segments_of(path);
        let path_explicit_index_count = path_segments
            .iter()
            .filter(|segment| extract_array_index(segment).is_some())
            .count();

        let mut exact_by_suffix: FxHashMap<String, rumoca_core::VarName> = FxHashMap::default();
        let mut by_suffix_and_indices: FxHashMap<String, rumoca_core::VarName> =
            FxHashMap::default();

        for var in subs {
            if let Some(remainder) = var.as_str().strip_prefix(path)
                && let Some(suffix) = remainder.strip_prefix('.')
            {
                exact_by_suffix
                    .entry(suffix.to_string())
                    .or_insert_with(|| var.clone());
            }

            let fallback_parts;
            let b_parts = if let Some(parts) = var_index.parsed_parts(var) {
                parts
            } else {
                fallback_parts = path_segments_of(var.as_str())
                    .into_iter()
                    .map(std::borrow::ToOwned::to_owned)
                    .collect::<Vec<_>>();
                &fallback_parts
            };

            let Some((suffix, normalized_indices)) = extract_suffix_and_indices_for_path(
                b_parts,
                &path_segments,
                path_explicit_index_count,
            ) else {
                continue;
            };

            by_suffix_and_indices
                .entry(suffix_indices_key(&suffix, &normalized_indices))
                .or_insert_with(|| var.clone());
        }

        Self {
            path_explicit_index_count,
            exact_by_suffix,
            by_suffix_and_indices,
        }
    }

    fn find_match(&self, suffix: &str, normalized_indices_a: &str) -> Option<rumoca_core::VarName> {
        if let Some(var) = self.exact_by_suffix.get(suffix) {
            return Some(var.clone());
        }

        // If A has no indices and B path is also not explicitly indexed, there is
        // nothing else to match beyond the exact-name check above.
        if normalized_indices_a.is_empty() && self.path_explicit_index_count == 0 {
            return None;
        }

        self.by_suffix_and_indices
            .get(&suffix_indices_key(suffix, normalized_indices_a))
            .cloned()
    }
}

/// A set of variables that are connected together.
#[derive(Debug)]
struct ConnectionSet {
    /// All variables in this connection set.
    variables: Vec<rumoca_core::VarName>,
    /// Connection equation kind to generate for this set.
    kind: ConnectionKind,
    /// Scope where the connect() equation was declared.
    ///
    /// Empty string means root scope.
    scope: String,
    /// Representative source span for downstream diagnostics on generated
    /// connection equations. Points at the originating connect() statement
    /// (the first connection that contributed an endpoint to this set, in
    /// the order connections were processed). SPEC_0008: generated
    /// equations carry the originating connect() span, not Span::DUMMY.
    span: rumoca_core::Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConnectionKind {
    Flow,
    Potential,
    StructuralAssertion,
}

/// A semantic MLS §15.2 stream connection set.
///
/// Stream sets are kept per hierarchy level, exactly like the flow sets, because
/// MLS §9.1.2 defines the inside/outside role of a connector relative to the
/// class that declares the `connect`. A pass-through connector such as
/// `pipe.port_a` is the *outside* connector of the set declared inside `Pipe`
/// and the *inside* connector of the set declared in the enclosing model, and
/// MLS §15.2 weights the two roles with opposite flow signs. Merging the levels
/// into one global set would weight the same physical branch twice.
#[derive(Debug)]
struct StreamConnectionSet {
    /// Stream variables connected at this level.
    variables: Vec<rumoca_core::VarName>,
    /// Scope where the connect() equation was declared; empty means root scope.
    scope: String,
    /// Representative source span for diagnostics and generated stream owners.
    span: rumoca_core::Span,
}

/// Union-Find data structure for building connection sets.
///
/// Uses index-based internal representation to minimize allocations.
/// rumoca_core::VarName strings are stored once and referenced by index.
struct UnionFind {
    /// Maps rumoca_core::VarName to its index.
    var_to_idx: IndexMap<rumoca_core::VarName, usize>,
    /// Parent array using indices (self-referential = root).
    parent: Vec<usize>,
    /// Rank for union-by-rank optimization.
    rank: Vec<usize>,
}

impl UnionFind {
    fn new() -> Self {
        Self {
            var_to_idx: IndexMap::default(),
            parent: Vec::new(),
            rank: Vec::new(),
        }
    }

    /// Get or create the index for a variable.
    fn get_or_insert_idx(&mut self, var: &rumoca_core::VarName) -> usize {
        if let Some(&idx) = self.var_to_idx.get(var) {
            idx
        } else {
            let idx = self.parent.len();
            self.var_to_idx.insert(var.clone(), idx);
            self.parent.push(idx); // Self-referential = root
            self.rank.push(0);
            idx
        }
    }

    /// Find the representative (root) of a variable's set with path compression.
    fn find_idx(&mut self, mut idx: usize) -> usize {
        // Find root
        let mut root = idx;
        while self.parent[root] != root {
            root = self.parent[root];
        }
        // Path compression
        while self.parent[idx] != root {
            let next = self.parent[idx];
            self.parent[idx] = root;
            idx = next;
        }
        root
    }

    /// Find the root rumoca_core::VarName for a variable.
    #[cfg(test)]
    fn find(&mut self, var: &rumoca_core::VarName) -> rumoca_core::VarName {
        let idx = self.get_or_insert_idx(var);
        let root_idx = self.find_idx(idx);
        self.var_to_idx
            .get_index(root_idx)
            .map(|(name, _)| name.clone())
            .unwrap()
    }

    /// Union two variables into the same set using union-by-rank.
    fn union(&mut self, a: &rumoca_core::VarName, b: &rumoca_core::VarName) {
        let idx_a = self.get_or_insert_idx(a);
        let idx_b = self.get_or_insert_idx(b);
        let root_a = self.find_idx(idx_a);
        let root_b = self.find_idx(idx_b);

        if root_a != root_b {
            // Union by rank
            if self.rank[root_a] < self.rank[root_b] {
                self.parent[root_a] = root_b;
            } else if self.rank[root_a] > self.rank[root_b] {
                self.parent[root_b] = root_a;
            } else {
                self.parent[root_b] = root_a;
                self.rank[root_a] += 1;
            }
        }
    }

    /// Get all connection sets.
    fn get_sets(&mut self) -> IndexMap<rumoca_core::VarName, Vec<rumoca_core::VarName>> {
        let mut sets: IndexMap<rumoca_core::VarName, Vec<rumoca_core::VarName>> =
            IndexMap::default();

        // Group variables by their root index
        // idx iterates 0..n where n = parent.len() = var_to_idx.len(), so
        // get_index(idx) is always in-bounds. find_idx(idx) returns an index
        // within [0, n) by the union-find path-compression invariant.
        let n = self.parent.len();
        debug_assert_eq!(
            n,
            self.var_to_idx.len(),
            "parent and var_to_idx must be co-sized"
        );
        for idx in 0..n {
            let root_idx = self.find_idx(idx);
            debug_assert!(root_idx < n, "find_idx must stay within bounds");
            let var = self
                .var_to_idx
                .get_index(idx)
                .expect("index within var_to_idx bounds")
                .0
                .clone();
            let root = self
                .var_to_idx
                .get_index(root_idx)
                .expect("root index within var_to_idx bounds")
                .0
                .clone();
            sets.entry(root).or_default().push(var);
        }

        sets
    }
}

/// Find all primitive sub-variables under a connector path.
///
/// For example, if `prefix` is "r1.n" and the flat model has "r1.n.v" and "r1.n.i",
/// this returns those two variables.
///
/// This function also handles array connector expansion (MLS §10.1):
/// - For prefix "resistor.p" with flat vars "resistor[1].p.v", "resistor[2].p.v", etc.,
///   the function matches by allowing optional array indices after each path segment.
fn find_sub_variables_indexed(
    prefix: &str,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    var_index: &ConnectionVarIndex,
) -> Vec<rumoca_core::VarName> {
    // First try O(1) prefix index lookup
    if let Some(children) = prefix_children.get(prefix) {
        return children.clone();
    }

    // If no exact matches, try matching with array index expansion (O(n) fallback)
    // through precomputed normalized-prefix candidates.
    find_sub_variables_with_array_expansion_indexed(prefix, var_index)
}

/// Find variables that match a path pattern exactly (with array expansion).
///
/// Unlike `find_sub_variables`, this finds variables that ARE the pattern with array expansion,
/// not sub-variables of the pattern. Used for output-to-output connections like
/// `connect(voltageSensor.v, v)` where `voltageSensor.v` maps to `voltageSensor[i].v`.
///
/// For path "voltageSensor.v", finds `voltageSensor[1].v`, `voltageSensor[2].v`, etc.
fn find_exact_match_with_array_expansion(
    path: &str,
    var_index: &ConnectionVarIndex,
) -> Vec<rumoca_core::VarName> {
    let segments = path_segments_of(path);
    if segments.is_empty() {
        return Vec::new();
    }
    let normalized_path = normalized_base_key_from_segments(&segments);
    let Some(candidates) = var_index.exact_candidates(&normalized_path) else {
        return Vec::new();
    };

    candidates
        .iter()
        .filter(|name| {
            var_index
                .parsed_parts(name)
                .is_some_and(|parts| matches_exactly_with_array_indices_cached(parts, &segments))
        })
        .cloned()
        .collect()
}

fn matches_exactly_with_array_indices_cached(name_parts: &[String], segments: &[&str]) -> bool {
    if segments.is_empty() {
        return false;
    }

    if name_parts.len() != segments.len() {
        return false;
    }

    for (i, segment) in segments.iter().enumerate() {
        if !compare_path_part(name_parts[i].as_str(), segment) {
            return false;
        }
    }

    true
}

fn find_sub_variables_with_array_expansion_indexed(
    prefix: &str,
    var_index: &ConnectionVarIndex,
) -> Vec<rumoca_core::VarName> {
    let segments = path_segments_of(prefix);
    if segments.is_empty() {
        return Vec::new();
    }
    let normalized_prefix = normalized_base_key_from_segments(&segments);
    let Some(candidates) = var_index.subvar_candidates(&normalized_prefix) else {
        return Vec::new();
    };

    candidates
        .iter()
        .filter(|name| {
            var_index
                .parsed_parts(name)
                .is_some_and(|parts| matches_with_array_indices_cached(parts, &segments))
        })
        .cloned()
        .collect()
}

fn matches_with_array_indices_cached(name_parts: &[String], segments: &[&str]) -> bool {
    if segments.is_empty() {
        return false;
    }

    if name_parts.len() <= segments.len() {
        return false;
    }

    for (i, segment) in segments.iter().enumerate() {
        if i >= name_parts.len() {
            return false;
        }

        if !compare_path_part_with_mode(name_parts[i].as_str(), segment, true) {
            return false;
        }
    }

    true
}

/// Find matching variable in B given suffix and array indices from A.
///
/// For array connector connections, we need to match elements by their indices:
/// - A: "resistor[1].p.v" with prefix "resistor.p" -> suffix "v", indices "[1]"
/// - B: prefix "plug_p.pin" -> look for "plug_p.pin[1].v"
///
/// `normalized_indices_a` must already have path-level explicit indices removed
/// (e.g. "[1][2]" from path `s[1].n` becomes "[2]").
fn find_matching_var_b_indexed(
    suffix: &str,
    normalized_indices_a: &str,
    sub_match_index: &ConnectionSubMatchIndex,
) -> Option<rumoca_core::VarName> {
    sub_match_index.find_match(suffix, normalized_indices_a)
}

fn is_primitive_flat_var(flat: &flat::Model, var: &rumoca_core::VarName) -> bool {
    flat.variables
        .get(var)
        .is_some_and(|info| info.is_primitive)
}

/// Connect array output variables.
///
/// Handles the case where one side is an expanded array component pattern and
/// the other side is an array variable. For example:
/// - `connect(voltageSensor.v, v)` where `voltageSensor[i].v` maps to `v[i]`
///
/// This generates connection equations for array-to-array output connections.
fn connect_array_output_variables(
    ctx: &ArrayConnCtx,
    flat: &flat::Model,
    var_index: &ConnectionVarIndex,
    flow_pairs: &mut Vec<(rumoca_core::VarName, rumoca_core::VarName)>,
    potential_uf: &mut UnionFind,
    stream_uf: &mut UnionFind,
) -> Result<(), FlattenError> {
    // Case 0: Neither side is primitive - both expand to array element variables
    // E.g., connect(positiveThreshold.y, timerPositive.u) where both are on array components
    // Expands to positiveThreshold[i].y = timerPositive[i].u
    if !ctx.a_is_primitive && !ctx.b_is_primitive {
        let expanded_a = find_exact_match_with_array_expansion(ctx.path_a, var_index);
        let expanded_b = find_exact_match_with_array_expansion(ctx.path_b, var_index);
        if !expanded_a.is_empty() && !expanded_b.is_empty() {
            let planned = plan_expanded_exact_connections(
                ctx.path_a,
                ctx.path_b,
                &expanded_a,
                &expanded_b,
                flat,
                ctx.span,
            )?;
            for pair in planned {
                commit_planned_exact_connection(pair, flow_pairs, potential_uf, stream_uf);
            }
            return Ok(());
        }
    }

    // Case 1: A is an array variable, B expands to multiple scalar variables
    // E.g., connect(v, voltageSensor.v) - connects v[i] to voltageSensor[i].v
    if ctx.a_is_primitive {
        let expanded_b = find_exact_match_with_array_expansion(ctx.path_b, var_index);
        if !expanded_b.is_empty() {
            let matched = connect_array_to_expanded(
                &ArrayExpandedRouteCtx {
                    array_var: ctx.var_a,
                    expanded_path: ctx.path_b,
                    expanded_vars: &expanded_b,
                    flat,
                    span: ctx.span,
                },
                flow_pairs,
                potential_uf,
                stream_uf,
            )?;
            if matched != 0 {
                return Ok(());
            }
        }
    }

    // Case 2: B is an array variable, A expands to multiple scalar variables
    // E.g., connect(voltageSensor.v, v) - connects voltageSensor[i].v to v[i]
    if ctx.b_is_primitive {
        let expanded_a = find_exact_match_with_array_expansion(ctx.path_a, var_index);
        if !expanded_a.is_empty() {
            let matched = connect_array_to_expanded(
                &ArrayExpandedRouteCtx {
                    array_var: ctx.var_b,
                    expanded_path: ctx.path_a,
                    expanded_vars: &expanded_a,
                    flat,
                    span: ctx.span,
                },
                flow_pairs,
                potential_uf,
                stream_uf,
            )?;
            if matched != 0 {
                return Ok(());
            }
        }
    }

    Err(FlattenError::incompatible_connectors(
        ctx.path_a, ctx.path_b, ctx.span,
    ))
}

/// Extract the base array path from a subscripted path.
///
/// Connect an array variable to a set of expanded scalar variables.
///
/// For array variable `v` with dims=[3] and expanded vars [voltageSensor[1].v, voltageSensor[2].v, voltageSensor[3].v],
/// this creates connections representing:
/// - v[1] = voltageSensor[1].v
/// - v[2] = voltageSensor[2].v
/// - v[3] = voltageSensor[3].v
///
/// Since the array variable is a single variable with multiple scalars, we create
/// synthetic subscripted variable names for the connection sets.
fn connect_array_to_expanded(
    ctx: &ArrayExpandedRouteCtx<'_>,
    flow_pairs: &mut Vec<(rumoca_core::VarName, rumoca_core::VarName)>,
    potential_uf: &mut UnionFind,
    stream_uf: &mut UnionFind,
) -> Result<usize, FlattenError> {
    let planned = plan_array_to_expanded(
        ctx.array_var,
        ctx.expanded_path,
        ctx.expanded_vars,
        ctx.flat,
        ctx.span,
    )?;
    let matched = planned.len();
    for pair in planned {
        match pair.kind {
            PlannedConnectionKind::StructuralAssertion => {
                potential_uf.union(&pair.array_element, &pair.expanded);
            }
            PlannedConnectionKind::Flow => flow_pairs.push((pair.array_element, pair.expanded)),
            PlannedConnectionKind::Stream => {
                stream_uf.union(&pair.array_element, &pair.expanded);
            }
            PlannedConnectionKind::Potential => {
                potential_uf.union(&pair.array_element, &pair.expanded);
            }
        }
    }
    Ok(matched)
}

#[derive(Clone, Copy)]
enum PlannedConnectionKind {
    StructuralAssertion,
    Flow,
    Stream,
    Potential,
}

struct PlannedExactConnection {
    a: rumoca_core::VarName,
    b: rumoca_core::VarName,
    kind: PlannedConnectionKind,
}

fn plan_expanded_exact_connections(
    path_a: &str,
    path_b: &str,
    expanded_a: &[rumoca_core::VarName],
    expanded_b: &[rumoca_core::VarName],
    flat: &flat::Model,
    span: Span,
) -> Result<Vec<PlannedExactConnection>, FlattenError> {
    let by_coordinate_a = expanded_members_by_coordinate(path_a, expanded_a, span)?;
    let by_coordinate_b = expanded_members_by_coordinate(path_b, expanded_b, span)?;
    if by_coordinate_a.keys().ne(by_coordinate_b.keys()) {
        return Err(FlattenError::invalid_connection_evidence(
            format!(
                "expanded connection endpoints `{path_a}` and `{path_b}` do not cover the same coordinates"
            ),
            span,
        ));
    }
    by_coordinate_a
        .into_iter()
        .zip(by_coordinate_b)
        .map(|((_, a), (_, b))| {
            let kind = validate_connection_pair(flat, &a, &b, span)?;
            Ok(PlannedExactConnection { a, b, kind })
        })
        .collect()
}

fn expanded_members_by_coordinate(
    pattern: &str,
    members: &[rumoca_core::VarName],
    span: Span,
) -> Result<std::collections::BTreeMap<Vec<i64>, rumoca_core::VarName>, FlattenError> {
    let mut result = std::collections::BTreeMap::new();
    for member in members {
        let coordinate =
            expanded_coordinates_for_pattern(member.as_str(), pattern).map_err(|reason| {
                FlattenError::invalid_connection_evidence(
                    format!("expanded member `{member}` is invalid for `{pattern}`: {reason}"),
                    span,
                )
            })?;
        if result.insert(coordinate.clone(), member.clone()).is_some() {
            return Err(FlattenError::invalid_connection_evidence(
                format!(
                    "expanded endpoint `{pattern}` has more than one member at coordinate {coordinate:?}"
                ),
                span,
            ));
        }
    }
    Ok(result)
}

fn commit_planned_exact_connection(
    pair: PlannedExactConnection,
    flow_pairs: &mut Vec<(rumoca_core::VarName, rumoca_core::VarName)>,
    potential_uf: &mut UnionFind,
    stream_uf: &mut UnionFind,
) {
    match pair.kind {
        PlannedConnectionKind::StructuralAssertion | PlannedConnectionKind::Potential => {
            potential_uf.union(&pair.a, &pair.b);
        }
        PlannedConnectionKind::Flow => flow_pairs.push((pair.a, pair.b)),
        PlannedConnectionKind::Stream => stream_uf.union(&pair.a, &pair.b),
    }
}

struct PlannedArrayConnection {
    coordinates: Vec<i64>,
    array_element: rumoca_core::VarName,
    expanded: rumoca_core::VarName,
    kind: PlannedConnectionKind,
}

fn plan_array_to_expanded(
    array_var: &rumoca_core::VarName,
    expanded_path: &str,
    expanded_vars: &[rumoca_core::VarName],
    flat: &flat::Model,
    span: Span,
) -> Result<Vec<PlannedArrayConnection>, FlattenError> {
    let declaration = flat
        .variables
        .get(array_var)
        .ok_or_else(|| FlattenError::undefined_variable(array_var.as_str(), span))?;
    let expected = scalar_count_of_dims(&declaration.dims).map_err(|reason| {
        invalid_array_expansion(
            array_var,
            format!("its compact dimensions are invalid: {reason}"),
            span,
        )
    })?;
    let structural = matches!(
        declaration.variability,
        rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
    );
    if structural && expected != 0 {
        return Err(invalid_array_expansion(
            array_var,
            "a nonempty compact structural array requires a compact Flat assertion-family owner"
                .to_string(),
            span,
        ));
    }
    if expected != expanded_vars.len() {
        return Err(invalid_array_expansion(
            array_var,
            format!(
                "compact cardinality {expected} does not match {} expanded members",
                expanded_vars.len()
            ),
            span,
        ));
    }

    let mut seen = std::collections::BTreeSet::new();
    let mut planned = Vec::with_capacity(expanded_vars.len());
    for expanded in expanded_vars {
        let pair = plan_array_expanded_pair(
            array_var,
            &declaration.dims,
            expanded_path,
            expanded,
            flat,
            span,
        )?;
        if !seen.insert(pair.coordinates.clone()) {
            return Err(invalid_array_expansion(
                array_var,
                format!(
                    "more than one expanded member claims coordinate {:?}",
                    pair.coordinates
                ),
                span,
            ));
        }
        planned.push(pair);
    }
    planned.sort_by(|left, right| left.coordinates.cmp(&right.coordinates));
    Ok(planned)
}

fn plan_array_expanded_pair(
    array_var: &rumoca_core::VarName,
    dimensions: &[i64],
    expanded_path: &str,
    expanded: &rumoca_core::VarName,
    flat: &flat::Model,
    span: Span,
) -> Result<PlannedArrayConnection, FlattenError> {
    if !flat.variables.contains_key(expanded) {
        return Err(invalid_array_expansion(
            array_var,
            format!("expanded member `{expanded}` has no Flat declaration"),
            span,
        ));
    }
    let coordinates = expanded_coordinates_for_pattern(expanded.as_str(), expanded_path)
        .map_err(|reason| invalid_array_expansion(array_var, reason.to_string(), span))?;
    if coordinates.len() != dimensions.len() {
        return Err(invalid_array_expansion(
            array_var,
            format!(
                "expanded member `{expanded}` supplies {} coordinates for compact rank {}",
                coordinates.len(),
                dimensions.len()
            ),
            span,
        ));
    }
    if coordinates
        .iter()
        .zip(dimensions)
        .any(|(coordinate, extent)| *coordinate < 1 || *coordinate > *extent)
    {
        return Err(invalid_array_expansion(
            array_var,
            format!(
                "expanded member `{expanded}` coordinate {coordinates:?} is outside dimensions {dimensions:?}"
            ),
            span,
        ));
    }
    let rendered = coordinates
        .iter()
        .map(i64::to_string)
        .collect::<Vec<_>>()
        .join(",");
    let array_element = rumoca_core::VarName::new(format!("{}[{rendered}]", array_var.as_str()));
    let kind = validate_connection_pair(flat, &array_element, expanded, span)?;
    Ok(PlannedArrayConnection {
        coordinates,
        array_element,
        expanded: expanded.clone(),
        kind,
    })
}

fn validate_connection_pair(
    flat: &flat::Model,
    array_element: &rumoca_core::VarName,
    expanded: &rumoca_core::VarName,
    span: Span,
) -> Result<PlannedConnectionKind, FlattenError> {
    validate_flow_consistency(flat, array_element, expanded, span)?;
    validate_type_compatibility(flat, array_element, expanded, span)?;
    validate_dimension_compatibility(flat, array_element, expanded, span)?;
    validate_quantity_compatibility(flat, array_element, expanded, span)?;
    if classify_connection_member_pair(flat, array_element, expanded, span)?
        == MemberPairing::StructuralAssertion
    {
        return Ok(PlannedConnectionKind::StructuralAssertion);
    }
    if is_flow_variable(flat, array_element, span)? {
        Ok(PlannedConnectionKind::Flow)
    } else if is_stream_variable(flat, array_element, span)? {
        Ok(PlannedConnectionKind::Stream)
    } else {
        Ok(PlannedConnectionKind::Potential)
    }
}

fn invalid_array_expansion(
    array_var: &rumoca_core::VarName,
    reason: String,
    span: Span,
) -> FlattenError {
    FlattenError::invalid_connection_evidence(
        format!("array-to-expanded connection for `{array_var}` is invalid: {reason}"),
        span,
    )
}

/// Resolve the exact primitive pair that expanded validation and construction
/// both consume.
///
/// Collapsed connector arrays may retain their element axis on the Flat
/// declaration instead of the rendered connector path. Projecting that axis in
/// one helper prevents validation from judging a whole array while construction
/// silently connects one element.
fn resolved_expanded_member_pair(
    sub_a: &rumoca_core::VarName,
    path_a: &str,
    path_b: &str,
    var_b_match: &rumoca_core::VarName,
    indices_a: &str,
    flat: &flat::Model,
    span: Span,
) -> Result<(rumoca_core::VarName, rumoca_core::VarName), FlattenError> {
    let conn_a = scalarize_collapsed_connector_element(sub_a, path_a, flat);
    let mut conn_b = scalarize_collapsed_connector_element(var_b_match, path_b, flat);

    let path_b_has_index = path_has_explicit_index(path_b);
    if indices_a.is_empty() || path_b_has_index {
        return Ok((conn_a, conn_b));
    }

    // Matching expanded occurrences may distribute their coordinates across
    // several path segments (`b[1].sensor[2].x`). Compare the normalized
    // occurrence coordinate sequence, not one raw bracket group: the latter
    // mistakes nested expanded peers for compact members that still need a
    // projection (or vice versa).
    let target_occurrence_indices = extract_suffix(var_b_match.as_str(), path_b)
        .map(|(_, indices)| strip_explicit_path_indices(&indices, path_b))
        .unwrap_or_default();
    let source_occurrence_indices = strip_explicit_path_indices(indices_a, path_a);
    if !source_occurrence_indices.is_empty()
        && target_occurrence_indices == source_occurrence_indices
    {
        return Ok((conn_a, conn_b));
    }

    let b_dims = require_validation_var_info(flat, &conn_b, span)?.dims;
    if b_dims.is_empty() {
        // The unmatched source coordinate belongs to its already-expanded
        // component occurrence. A scalar target has no retained axis onto
        // which that occurrence coordinate could or should be projected.
        return Ok((conn_a, conn_b));
    }
    let coordinates = literal_coordinates(indices_a).ok_or_else(|| {
        FlattenError::invalid_connection_evidence(
            format!("expanded member `{sub_a}` has non-literal projection indices `{indices_a}`"),
            span,
        )
    })?;
    if coordinates.len() < b_dims.len() {
        return Err(FlattenError::invalid_connection_evidence(
            format!(
                "expanded member `{sub_a}` supplies {} projection coordinates for `{conn_b}` with retained rank {}",
                coordinates.len(),
                b_dims.len()
            ),
            span,
        ));
    }
    let projected = &coordinates[coordinates.len() - b_dims.len()..];
    if projected
        .iter()
        .zip(&b_dims)
        .any(|(index, extent)| *extent < 1 || *index < 1 || *index > *extent)
    {
        return Err(FlattenError::invalid_connection_evidence(
            format!(
                "expanded member `{sub_a}` projection {projected:?} is outside dimensions {b_dims:?} of `{conn_b}`"
            ),
            span,
        ));
    }
    let idx_suffix = projected
        .iter()
        .map(i64::to_string)
        .collect::<Vec<_>>()
        .join(",");
    conn_b = rumoca_core::VarName::new(format!("{}[{idx_suffix}]", conn_b.as_str()));
    Ok((conn_a, conn_b))
}

/// Connect a single sub-variable from connector A to matching sub-variable in connector B.
///
/// This helper reduces nesting in `build_connection_sets` by extracting the
/// inner loop logic for matching and connecting sub-variables.
///
/// Handles array connector expansion (MLS §10.1):
/// - For "resistor[1].p.v" with prefix "resistor.p", extracts suffix "v" and indices "[1]"
/// - Finds matching "plug_p.pin[1].v" in B's sub-variables
fn connect_sub_variable(
    sub_a: &rumoca_core::VarName,
    path_a: &str,
    path_b: &str,
    sub_match_index: &ConnectionSubMatchIndex,
    ctx: &mut ConnectionBuildCtx<'_>,
) -> Result<bool, FlattenError> {
    let Some((suffix_a, indices_a)) = extract_suffix(sub_a.as_str(), path_a) else {
        return Ok(false);
    };
    let normalized_indices_a = strip_explicit_path_indices(&indices_a, path_a);

    // Find matching variable in B with same suffix and array indices
    let Some(var_b_match) =
        find_matching_var_b_indexed(&suffix_a, &normalized_indices_a, sub_match_index)
    else {
        return Ok(false);
    };
    let (conn_a, conn_b) = resolved_expanded_member_pair(
        sub_a,
        path_a,
        path_b,
        &var_b_match,
        &indices_a,
        ctx.flat,
        ctx.span,
    )?;

    // MLS §9.3 pairing rules. A structural pair joins the assertion owner,
    // never an ordinary connection-equation set; a forbidden pair is rejected.
    if classify_connection_member_pair(ctx.flat, &conn_a, &conn_b, ctx.span)?
        == MemberPairing::StructuralAssertion
    {
        ctx.potential_uf.union(&conn_a, &conn_b);
        return Ok(true);
    }

    // Connect matching sub-variables based on flow/non-flow type
    if is_flow_variable(ctx.flat, &conn_a, ctx.span)? {
        ctx.flow_pairs.push((conn_a, conn_b));
    } else if is_stream_variable(ctx.flat, &conn_a, ctx.span)?
        && is_stream_variable(ctx.flat, &conn_b, ctx.span)?
    {
        // MLS §15.2 stream connectors are handled separately from flow/potential sets.
        ctx.stream_uf.union(&conn_a, &conn_b);
    } else {
        // Both sides agree on the stream prefix: a one-sided `stream` was
        // rejected above, so this branch can only be a potential pair.
        ctx.potential_uf.union(&conn_a, &conn_b);
    }
    Ok(true)
}

/// Process a single connection and update the connection structures.
fn process_connection(
    conn: &ast::InstanceScalarConnection,
    ctx: &ConnectionProcessCtx<'_>,
    flow_pairs: &mut Vec<(rumoca_core::VarName, rumoca_core::VarName)>,
    potential_uf: &mut UnionFind,
    stream_uf: &mut UnionFind,
) -> Result<(), FlattenError> {
    let path_a = conn.a().to_flat_string();
    let path_b = conn.b().to_flat_string();
    let var_a = rumoca_core::VarName::new(&path_a);
    let var_b = rumoca_core::VarName::new(&path_b);

    let a_is_primitive = is_primitive_flat_var(ctx.flat, &var_a);
    let b_is_primitive = is_primitive_flat_var(ctx.flat, &var_b);

    if a_is_primitive && b_is_primitive {
        return connect_primitive_vars(
            &var_a,
            &var_b,
            ctx.flat,
            flow_pairs,
            potential_uf,
            stream_uf,
            conn.span(),
        );
    }

    // Handle subscripted references to array variables: e.g., "comp.v[1]" where
    // flat.variables has "comp.v" as array[1]. The subscript comes from instantiation
    // resolving array dimension parameters. Treat as primitive since it refers to a
    // known variable's element.
    let a_subscript_prim = if a_is_primitive {
        false
    } else {
        has_proven_primitive_array_selection(&var_a, ctx.flat, conn.span())?
    };
    let b_subscript_prim = if b_is_primitive {
        false
    } else {
        has_proven_primitive_array_selection(&var_b, ctx.flat, conn.span())?
    };
    if (a_is_primitive || a_subscript_prim) && (b_is_primitive || b_subscript_prim) {
        return connect_primitive_vars(
            &var_a,
            &var_b,
            ctx.flat,
            flow_pairs,
            potential_uf,
            stream_uf,
            conn.span(),
        );
    }

    // At least one is a connector - try expansion
    let subs_a = find_sub_variables_indexed(&path_a, ctx.prefix_children, ctx.var_index);
    let subs_b = find_sub_variables_indexed(&path_b, ctx.prefix_children, ctx.var_index);

    if !subs_a.is_empty() && !subs_b.is_empty() {
        let mut build_ctx = ConnectionBuildCtx {
            flat: ctx.flat,
            var_index: ctx.var_index,
            flow_pairs,
            potential_uf,
            stream_uf,
            span: conn.span(),
        };
        return expand_connector_connection(&subs_a, &path_a, &path_b, &subs_b, &mut build_ctx);
    }

    let array_ctx = ArrayConnCtx {
        path_a: &path_a,
        path_b: &path_b,
        var_a: &var_a,
        var_b: &var_b,
        a_is_primitive,
        b_is_primitive,
        span: conn.span(),
    };
    connect_array_output_variables(
        &array_ctx,
        ctx.flat,
        ctx.var_index,
        flow_pairs,
        potential_uf,
        stream_uf,
    )
}

/// Connect two primitive variables directly based on flow type.
///
/// MLS §9.3 pairing rules are decided by [`classify_connection_member_pair`]
/// first: a structural pair is routed to the assertion owner, and a pair MLS
/// forbids is rejected here rather than dropped.
fn connect_primitive_vars(
    var_a: &rumoca_core::VarName,
    var_b: &rumoca_core::VarName,
    flat: &flat::Model,
    flow_pairs: &mut Vec<(rumoca_core::VarName, rumoca_core::VarName)>,
    potential_uf: &mut UnionFind,
    stream_uf: &mut UnionFind,
    span: Span,
) -> Result<(), FlattenError> {
    if classify_connection_member_pair(flat, var_a, var_b, span)?
        == MemberPairing::StructuralAssertion
    {
        potential_uf.union(var_a, var_b);
        return Ok(());
    }

    let is_flow_a = is_flow_variable(flat, var_a, span)?;
    let is_flow_b = is_flow_variable(flat, var_b, span)?;

    if is_flow_a && is_flow_b {
        flow_pairs.push((var_a.clone(), var_b.clone()));
    } else if is_stream_variable(flat, var_a, span)? && is_stream_variable(flat, var_b, span)? {
        stream_uf.union(var_a, var_b);
    } else if !is_flow_a && !is_flow_b {
        // Both sides agree on the stream prefix here: `classify_connection_member_pair`
        // has already rejected a one-sided `stream`, and a two-sided one took
        // the branch above.
        potential_uf.union(var_a, var_b);
    }
    // Mismatched flow/non-flow is caught by validation
    Ok(())
}

/// Expand a connector connection to its sub-variables.
fn expand_connector_connection(
    subs_a: &[rumoca_core::VarName],
    path_a: &str,
    path_b: &str,
    subs_b: &[rumoca_core::VarName],
    ctx: &mut ConnectionBuildCtx<'_>,
) -> Result<(), FlattenError> {
    let direction = complete_expanded_member_direction(
        path_a,
        subs_a,
        path_b,
        subs_b,
        ctx.var_index,
        ctx.span,
    )?;
    let (source_path, source_members, target_path, target_members) = match direction {
        ExpandedMemberDirection::Forward => (path_a, subs_a, path_b, subs_b),
        // Connector arrays can be represented asymmetrically: the expanded
        // side supplies indices for an indexless compact array member.
        ExpandedMemberDirection::Reverse => (path_b, subs_b, path_a, subs_a),
    };
    let match_index = ConnectionSubMatchIndex::new(target_path, target_members, ctx.var_index);
    for member in source_members {
        let matched = connect_sub_variable(member, source_path, target_path, &match_index, ctx)?;
        debug_assert!(
            matched,
            "coverage proof requires every source member to match"
        );
    }
    Ok(())
}

/// Build connection sets from individual connections.
///
/// Uses union-find to group connected variables transitively.
/// Separates flow and non-flow variables into different sets.
///
/// **Key**: Flow connection sets are built per hierarchical level (MLS §9.2).
/// When a boundary connector participates in both internal and external
/// connections, each level generates its own flow sum equation. This ensures
/// correct equation counts for hierarchical connector pass-through.
///
/// Flow connection sets are computed per-scope (hierarchy level where connect
/// was declared) because each scope generates its own flow conservation
/// equations. This is needed since we don't do alias elimination — intermediate
/// connector variables need their own flow sum equations at each level.
///
/// Potential (equality) connection sets use a global union-find since
/// N-1 equality equations give the same count whether split or merged.
///
/// Stream connection sets are computed per-scope for the same reason as the
/// flow sets: MLS §15.2 weights an inside connector with `max(-m_flow, 0)` and
/// an outside connector with `max(+m_flow, 0)`, and that role is only defined
/// relative to the level that declares the `connect` (MLS §9.1.2).
fn build_connection_sets(
    connections: &[ConnectionTopologyInput<'_>],
    flat: &flat::Model,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    var_index: &ConnectionVarIndex,
    consumption: &mut ConnectionSourceConsumption,
) -> Result<(Vec<ConnectionSet>, Vec<StreamConnectionSet>), FlattenError> {
    let mut potential_uf = UnionFind::new();
    let mut result = Vec::new();
    let mut stream_sets: Vec<StreamConnectionSet> = Vec::new();
    let process_ctx = ConnectionProcessCtx {
        flat,
        var_index,
        prefix_children,
    };

    // SPEC_0008: every generated connection equation carries real provenance.
    // Track direct connect() spans first; scalarized array members that do not
    // appear as direct endpoints use their owning flat variable span.
    let mut var_first_span: FxHashMap<rumoca_core::VarName, rumoca_core::Span> =
        FxHashMap::default();
    let record_var_span = |map: &mut FxHashMap<rumoca_core::VarName, rumoca_core::Span>,
                           var: rumoca_core::VarName,
                           span: rumoca_core::Span| {
        map.entry(var).or_insert(span);
    };
    for input in connections {
        let conn = input.connection;
        record_var_span(
            &mut var_first_span,
            rumoca_core::VarName::new(conn.a().to_flat_string()),
            conn.span(),
        );
        record_var_span(
            &mut var_first_span,
            rumoca_core::VarName::new(conn.b().to_flat_string()),
            conn.span(),
        );
    }

    // Group connections by scope (hierarchy level where connect was declared).
    let mut connections_by_scope: IndexMap<&str, Vec<&ConnectionTopologyInput<'_>>> =
        IndexMap::default();
    for input in connections {
        connections_by_scope
            .entry(input.connection.scope())
            .or_default()
            .push(input);
    }

    // Process each scope separately for flow and stream pairs, globally for
    // potential variables.
    for (scope, scope_conns) in &connections_by_scope {
        let mut flow_pairs: Vec<(rumoca_core::VarName, rumoca_core::VarName)> = Vec::new();
        let mut stream_uf = UnionFind::new();
        for input in scope_conns {
            let conn = input.connection;
            consumption.admit(input.source, conn.span())?;
            process_connection(
                conn,
                &process_ctx,
                &mut flow_pairs,
                &mut potential_uf,
                &mut stream_uf,
            )?;
        }

        let mut scope_uf = UnionFind::new();
        for (a, b) in flow_pairs {
            scope_uf.union(&a, &b);
        }
        for (_root, vars) in scope_uf.get_sets() {
            if vars.len() >= 2 {
                let span = representative_connection_span(&vars, &var_first_span, flat)?;
                result.push(ConnectionSet {
                    variables: vars,
                    kind: ConnectionKind::Flow,
                    scope: (*scope).to_string(),
                    span,
                });
            }
        }

        for (_root, vars) in stream_uf.get_sets() {
            // A semantic stream set must retain real connect provenance even
            // though equations are emitted only for its outside endpoints.
            let span = representative_connection_span(&vars, &var_first_span, flat)?;
            stream_sets.push(StreamConnectionSet {
                variables: vars,
                scope: (*scope).to_string(),
                span,
            });
        }
    }

    // Extract non-flow connection sets. Structural members share the same
    // union-find only to preserve transitive equality; their set kind routes
    // them to assertions rather than ordinary residual equations.
    for (_root, vars) in potential_uf.get_sets() {
        if vars.len() >= 2 {
            let span = representative_connection_span(&vars, &var_first_span, flat)?;
            let kind = non_flow_connection_set_kind(flat, &vars, span)?;
            result.push(ConnectionSet {
                variables: vars,
                kind,
                scope: String::new(),
                span,
            });
        }
    }

    Ok((result, stream_sets))
}

fn non_flow_connection_set_kind(
    flat: &flat::Model,
    variables: &[rumoca_core::VarName],
    span: Span,
) -> Result<ConnectionKind, FlattenError> {
    let mut structural = None;
    for variable in variables {
        let evidence = require_connection_declaration(flat, variable, span)?;
        let declaration = evidence.declaration();
        let current = matches!(
            &declaration.variability,
            rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
        );
        if structural.is_some_and(|expected| expected != current) {
            return Err(FlattenError::invalid_connection_evidence(
                "one non-flow connection set mixes structural and equation unknown members",
                span,
            ));
        }
        structural = Some(current);
    }
    Ok(if structural == Some(true) {
        ConnectionKind::StructuralAssertion
    } else {
        ConnectionKind::Potential
    })
}

fn representative_connection_span(
    vars: &[rumoca_core::VarName],
    var_first_span: &FxHashMap<rumoca_core::VarName, rumoca_core::Span>,
    flat: &flat::Model,
) -> Result<rumoca_core::Span, FlattenError> {
    let diagnostic_span = var_first_span
        .values()
        .copied()
        .chain(vars.iter().filter_map(|var| {
            flat.variables
                .get(var)
                .map(|declaration| declaration.source_span)
        }))
        .find(|span| !span.is_dummy())
        .ok_or_else(|| {
            FlattenError::missing_source_context(format!(
                "connection set `{}` has no source-backed declaration",
                vars.iter()
                    .map(rumoca_core::VarName::as_str)
                    .collect::<Vec<_>>()
                    .join(", ")
            ))
        })?;
    let mut spans = Vec::with_capacity(vars.len());
    for var in vars {
        if let Some(span) = var_first_span
            .get(var)
            .copied()
            .filter(|span| !span.is_dummy())
        {
            spans.push(span);
            continue;
        }
        let evidence = require_connection_declaration(flat, var, diagnostic_span)?;
        let declaration = evidence.declaration();
        if !declaration.source_span.is_dummy() {
            spans.push(declaration.source_span);
        }
    }
    spans
        .into_iter()
        .min_by_key(|span| (span.source.0, span.start.0, span.end.0))
        .ok_or_else(|| {
            FlattenError::missing_source_context(format!(
                "connection set `{}` has no source span",
                vars.iter()
                    .map(rumoca_core::VarName::as_str)
                    .collect::<Vec<_>>()
                    .join(", ")
            ))
        })
}

fn require_connection_provenance(
    span: Span,
    context: &'static str,
) -> Result<ProvenanceSpan, FlattenError> {
    span.require_provenance(context)
        .map_err(|err| FlattenError::missing_source_context(err.to_string()))
}

fn require_flat_variable_provenance(
    flat: &flat::Model,
    var: &rumoca_core::VarName,
    context: &'static str,
) -> Result<ProvenanceSpan, FlattenError> {
    let diagnostic_span = flat
        .variables
        .values()
        .map(|declaration| declaration.source_span)
        .find(|span| !span.is_dummy())
        .ok_or_else(|| {
            FlattenError::missing_source_context(format!(
                "{context} for `{}` has no source-backed Flat declaration catalog",
                var.as_str()
            ))
        })?;
    let evidence = require_connection_declaration(flat, var, diagnostic_span)?;
    let declaration = evidence.declaration();
    let span = (!declaration.source_span.is_dummy())
        .then_some(declaration.source_span)
        .ok_or_else(|| {
            FlattenError::missing_source_context(format!(
                "{context} for `{}` has no source span",
                var.as_str()
            ))
        })?;
    require_connection_provenance(span, context)
}

/// Create a residual expression: lhs - rhs (for equation lhs = rhs).
fn create_equality_residual(
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
    span: ProvenanceSpan,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: span.span(),
    }
}

/// Create a sum expression: a + b + c + ...
fn create_sum(
    exprs: Vec<rumoca_core::Expression>,
    span: ProvenanceSpan,
) -> rumoca_core::Expression {
    if exprs.is_empty() {
        return rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            span: span.span(),
        };
    }

    // SAFETY: is_empty() check above guarantees at least one element
    let mut iter = exprs.into_iter();
    let mut result = iter.next().unwrap();

    for expr in iter {
        result = rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(result),
            rhs: Box::new(expr),
            span: span.span(),
        };
    }

    result
}

#[cfg(test)]
const CONNECTION_TEST_SCALAR_TYPE: rumoca_core::TypeId = rumoca_core::TypeId(0x5f_0001);

/// Construct the current Flat test aggregate with one concrete scalar type.
#[cfg(test)]
fn connection_test_model() -> flat::Model {
    let mut flat = flat::Model::new();
    flat.effective_types.insert(
        CONNECTION_TEST_SCALAR_TYPE,
        rumoca_core::EffectiveType::new(
            CONNECTION_TEST_SCALAR_TYPE,
            CONNECTION_TEST_SCALAR_TYPE,
            [],
        )
        .expect("the fixture scalar type is concrete"),
    );
    flat.type_roots
        .insert(CONNECTION_TEST_SCALAR_TYPE, CONNECTION_TEST_SCALAR_TYPE);
    flat
}

/// Construct one primitive Flat occurrence carrying the fixture's concrete
/// scalar effective-type identity.
#[cfg(test)]
fn connection_test_variable(span: rumoca_core::Span) -> flat::Variable {
    flat::Variable {
        type_id: CONNECTION_TEST_SCALAR_TYPE,
        is_primitive: true,
        ..flat::Variable::empty_with_span(span)
    }
}

#[cfg(test)]
trait ConnectionTestFlatExt {
    fn add_test_variable(&mut self, name: rumoca_core::VarName, variable: flat::Variable);
}

#[cfg(test)]
impl ConnectionTestFlatExt for flat::Model {
    fn add_test_variable(&mut self, name: rumoca_core::VarName, mut variable: flat::Variable) {
        assert!(
            variable.instance_id.is_unset(),
            "the fixture constructor is the sole occurrence issuer"
        );
        variable.name = name.clone();
        variable.instance_id = self.materialize_instance(flat::InstanceRelation {
            owner: None,
            declaration: variable
                .component_ref
                .as_ref()
                .map(rumoca_core::ComponentReference::target_def_id),
            indices: Box::new([]),
            kind: flat::InstanceKind::Materialized,
        });
        self.add_variable(name, variable);
    }
}

/// Run the production one-shot Flat shape transition for a connection fixture.
///
/// The fixture itself must construct every occurrence and type identity before
/// calling this helper; this transition does not repair missing facts.
#[cfg(test)]
fn finalize_connection_test_flat(flat: &mut flat::Model) {
    flat.finalize_effective_type_shapes()
        .expect("connection test fixture must issue finalized effective shape identities");
}

#[cfg(test)]
mod connected_domain_tests;
#[cfg(test)]
mod endpoint_subscript_tests;
#[cfg(test)]
mod family_view_tests;
#[cfg(test)]
mod materialization_budget_tests;
#[cfg(test)]
mod scalar_count_tests;
#[cfg(test)]
mod stream_mixing_tests;
#[cfg(test)]
mod tests;
