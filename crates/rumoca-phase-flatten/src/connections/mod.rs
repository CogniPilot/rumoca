//! Connection processing for the flatten phase (MLS §9).
//!
//! This module expands connect() statements into connection equations:
//! - Flow variables: sum to zero (Kirchhoff's current law)
//! - Non-flow (potential) variables: are equal
//!
//! ## MLS §9.2 Connection Semantics
//!
//! For each connection set:
//! - Potential (non-flow) variables: equality equations `v1 = v2 = ... = vn` (n-1 equations)
//! - Flow variables: sum equation `f1 + f2 + ... + fn = 0` (1 equation)
//!
//! The sign convention for flow variables depends on whether the connector
//! is an inside or outside connector (MLS §9.2):
//! - Inside connector (component port): sign = +1
//! - Outside connector (model boundary): sign = -1

use indexmap::IndexMap;
use rumoca_core::{Span, TypeId};
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;
use rustc_hash::FxHashMap;

use crate::errors::FlattenError;
use crate::path_utils::{
    first_path_segment_without_index, has_top_level_dot, parse_path_with_indices, strip_array_index,
};

mod equation_generation;
mod path_index;
use equation_generation::*;
pub(crate) use equation_generation::{connection_involves_disabled, process_connections};
use path_index::*;

/// Context for array output connection operations.
/// Groups related parameters to reduce function argument count.
struct ArrayConnCtx<'a> {
    path_a: &'a str,
    path_b: &'a str,
    var_a: &'a flat::VarName,
    var_b: &'a flat::VarName,
    a_is_primitive: bool,
    b_is_primitive: bool,
}

struct ConnectionBuildCtx<'a> {
    flat: &'a flat::Model,
    var_index: &'a ConnectionVarIndex,
    flow_pairs: &'a mut Vec<(flat::VarName, flat::VarName)>,
    potential_uf: &'a mut UnionFind,
    stream_uf: &'a mut UnionFind,
}

/// Precomputed lookup structures for connection path matching.
///
/// Built once per connection-processing pass to avoid repeated full scans and
/// repeated `parse_path_with_indices` work in hot loops.
struct ConnectionVarIndex {
    /// Variables indexed by normalized base prefix (indices stripped), for
    /// connector-subvariable expansion lookups.
    subvars_by_base_prefix: FxHashMap<String, Vec<flat::VarName>>,
    /// Variables indexed by normalized full path (indices stripped), for exact
    /// path matching with array expansion.
    exact_by_base_path: FxHashMap<String, Vec<flat::VarName>>,
    /// Parsed path parts per variable name.
    parsed_parts_by_var: FxHashMap<flat::VarName, Vec<String>>,
}

impl ConnectionVarIndex {
    fn new(flat: &flat::Model) -> Self {
        Self::from_var_names(flat.variables.keys())
    }

    fn from_var_names<'a, I>(var_names: I) -> Self
    where
        I: IntoIterator<Item = &'a flat::VarName>,
    {
        let mut subvars_by_base_prefix: FxHashMap<String, Vec<flat::VarName>> =
            FxHashMap::default();
        let mut exact_by_base_path: FxHashMap<String, Vec<flat::VarName>> = FxHashMap::default();
        let mut parsed_parts_by_var: FxHashMap<flat::VarName, Vec<String>> = FxHashMap::default();

        for var_name in var_names {
            let parsed_parts: Vec<String> = parse_path_with_indices(var_name.as_str())
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

    fn parsed_parts(&self, var_name: &flat::VarName) -> Option<&[String]> {
        self.parsed_parts_by_var.get(var_name).map(Vec::as_slice)
    }

    fn subvar_candidates(&self, normalized_prefix: &str) -> Option<&[flat::VarName]> {
        self.subvars_by_base_prefix
            .get(normalized_prefix)
            .map(Vec::as_slice)
    }

    fn exact_candidates(&self, normalized_path: &str) -> Option<&[flat::VarName]> {
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
    exact_by_suffix: FxHashMap<String, flat::VarName>,
    by_suffix_and_indices: FxHashMap<String, flat::VarName>,
}

impl ConnectionSubMatchIndex {
    fn new(path: &str, subs: &[flat::VarName], var_index: &ConnectionVarIndex) -> Self {
        let path_segments = parse_path_with_indices(path);
        let path_explicit_index_count = path_segments
            .iter()
            .filter(|segment| extract_array_index(segment).is_some())
            .count();

        let mut exact_by_suffix: FxHashMap<String, flat::VarName> = FxHashMap::default();
        let mut by_suffix_and_indices: FxHashMap<String, flat::VarName> = FxHashMap::default();

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
                fallback_parts = parse_path_with_indices(var.as_str())
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

    fn find_match(&self, suffix: &str, normalized_indices_a: &str) -> Option<flat::VarName> {
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

// =============================================================================
// Task 2.1: Flow Variable Identification (CONN-003)
// =============================================================================

/// Check if a variable is a flow variable.
///
/// Per MLS §9.2 and CONN-003: Flow variables have the `flow` prefix
/// in their component declaration.
///
/// # Example
///
/// ```ignore
/// connector Pin
///     Real v;         // Potential variable (non-flow)
///     flow Real i;    // Flow variable
/// end Pin;
/// ```
pub fn is_flow_variable(flat: &flat::Model, var_name: &flat::VarName) -> bool {
    if let Some(v) = flat.variables.get(var_name) {
        return v.flow;
    }
    subscripted_base_var(var_name, flat)
        .and_then(|base| flat.variables.get(&base))
        .map(|v| v.flow)
        .unwrap_or(false)
}

/// Check if a variable is a stream variable.
///
/// Per MLS §15.2, stream connectors are handled by stream-specific equations
/// (`inStream`/`actualStream`) and must not be turned into direct potential
/// equality equations by `connect()`.
fn is_stream_variable(flat: &flat::Model, var_name: &flat::VarName) -> bool {
    if let Some(v) = flat.variables.get(var_name) {
        return v.stream;
    }
    subscripted_base_var(var_name, flat)
        .and_then(|base| flat.variables.get(&base))
        .map(|v| v.stream)
        .unwrap_or(false)
}

/// Check if a variable name is a subscripted reference to an existing array variable
/// with in-bounds subscript.
///
/// E.g., `"comp.v[1]"` is valid if `"comp.v"` exists in `flat.variables` with
/// dimension >= 1. Returns false for out-of-bounds subscripts like `"comp.v[2]"`
/// when `"comp.v"` is array[1].
///
/// This occurs when the instantiation phase resolves array dimension parameters
/// (e.g., `m=1`) and produces subscripted connection paths like `twoPulse.v[1]`.
fn is_subscripted_variable(var: &flat::VarName, flat: &flat::Model) -> bool {
    is_subscripted_variable_inner(var, flat).unwrap_or(false)
}

fn split_trailing_index_groups(path: &str) -> Option<(String, Vec<String>)> {
    if !path.ends_with(']') {
        return None;
    }

    let bytes = path.as_bytes();
    let mut end = bytes.len();
    let mut groups_rev: Vec<String> = Vec::new();

    while end > 0 && bytes[end - 1] == b']' {
        let start = find_trailing_group_start(bytes, end)?;
        groups_rev.push(path[start..end].to_string());
        end = start;
    }

    if groups_rev.is_empty() || end == 0 {
        return None;
    }

    let mut groups = groups_rev;
    groups.reverse();
    Some((path[..end].to_string(), groups))
}

fn find_trailing_group_start(bytes: &[u8], end: usize) -> Option<usize> {
    let mut depth = 0usize;
    for i in (0..end - 1).rev() {
        match bytes[i] {
            b']' => depth += 1,
            b'[' if depth == 0 => return Some(i),
            b'[' => depth -= 1,
            _ => {}
        }
    }
    None
}

fn parse_subscript_index(group: &str) -> Option<i64> {
    if group.len() < 3 || !group.starts_with('[') || !group.ends_with(']') {
        return None;
    }
    group[1..group.len() - 1].trim().parse().ok()
}

fn subscripted_base_var_with_rank(
    var: &flat::VarName,
    flat: &flat::Model,
) -> Option<(flat::VarName, usize)> {
    let (base_name, groups) = split_trailing_index_groups(var.as_str())?;
    let base = flat::VarName::new(&base_name);
    let base_var = flat.variables.get(&base)?;

    if base_var.dims.is_empty() {
        // Collapsed connector-array fields may lose explicit dimensions in flat::Variable.
        // Accept positive scalar indices and map to the base variable.
        if groups
            .iter()
            .all(|group| parse_subscript_index(group).is_some_and(|idx| idx >= 1))
        {
            return Some((base, groups.len()));
        }
        return None;
    }

    if groups.len() > base_var.dims.len() {
        return None;
    }

    let in_bounds = groups.iter().zip(base_var.dims.iter()).all(|(group, dim)| {
        parse_subscript_index(group).is_some_and(|idx| *dim >= 1 && idx >= 1 && idx <= *dim)
    });

    if in_bounds {
        Some((base, groups.len()))
    } else {
        None
    }
}

fn subscripted_base_var(var: &flat::VarName, flat: &flat::Model) -> Option<flat::VarName> {
    subscripted_base_var_with_rank(var, flat).map(|(base, _)| base)
}

fn is_subscripted_variable_inner(var: &flat::VarName, flat: &flat::Model) -> Option<bool> {
    subscripted_base_var(var, flat).map(|_| true)
}

// =============================================================================
// Connection Set Building
// =============================================================================

/// A set of variables that are connected together.
#[derive(Debug)]
struct ConnectionSet {
    /// All variables in this connection set.
    variables: Vec<flat::VarName>,
    /// Connection equation kind to generate for this set.
    kind: ConnectionKind,
    /// Scope where the connect() equation was declared.
    ///
    /// Empty string means root scope.
    scope: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConnectionKind {
    Flow,
    Potential,
    Stream,
}

/// Union-Find data structure for building connection sets.
///
/// Uses index-based internal representation to minimize allocations.
/// flat::VarName strings are stored once and referenced by index.
struct UnionFind {
    /// Maps flat::VarName to its index.
    var_to_idx: IndexMap<flat::VarName, usize>,
    /// Parent array using indices (self-referential = root).
    parent: Vec<usize>,
    /// Rank for union-by-rank optimization.
    rank: Vec<usize>,
}

impl UnionFind {
    fn new() -> Self {
        Self {
            var_to_idx: IndexMap::new(),
            parent: Vec::new(),
            rank: Vec::new(),
        }
    }

    /// Get or create the index for a variable.
    fn get_or_insert_idx(&mut self, var: &flat::VarName) -> usize {
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

    /// Find the root flat::VarName for a variable.
    #[cfg(test)]
    fn find(&mut self, var: &flat::VarName) -> flat::VarName {
        let idx = self.get_or_insert_idx(var);
        let root_idx = self.find_idx(idx);
        // Get the flat::VarName at root_idx
        self.var_to_idx
            .get_index(root_idx)
            .map(|(name, _)| name.clone())
            .unwrap()
    }

    /// Union two variables into the same set using union-by-rank.
    fn union(&mut self, a: &flat::VarName, b: &flat::VarName) {
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
    fn get_sets(&mut self) -> IndexMap<flat::VarName, Vec<flat::VarName>> {
        let mut sets: IndexMap<flat::VarName, Vec<flat::VarName>> = IndexMap::new();

        // Group variables by their root index
        // SAFETY: idx iterates 0..n where n = parent.len() = var_to_idx.len(),
        // and find_idx always returns an index within [0, n). get_index is safe.
        let n = self.parent.len();
        for idx in 0..n {
            let root_idx = self.find_idx(idx);
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

// =============================================================================
// Task 2.4: Connector Validation (CONN-001, CONN-003, CONN-008)
// =============================================================================

/// Validate all connections before processing.
///
/// Checks for:
/// - CONN-001/CONN-003: Flow/non-flow prefix consistency (homogeneity)
/// - CONN-002: Type compatibility (Real vs Integer vs Boolean)
/// - CONN-008: Array dimension compatibility
///
/// Note: CONN-005 (quantity matching) and other contracts are not yet implemented.
///
/// For connector-level connections (non-primitive paths), validation is
/// performed on the expanded sub-variables during connection set building.
fn validate_connections(
    connections: &[&ast::InstanceConnection],
    flat: &flat::Model,
    type_roots: &IndexMap<TypeId, TypeId>,
    prefix_children: &FxHashMap<String, Vec<flat::VarName>>,
    var_index: &ConnectionVarIndex,
) -> Result<(), FlattenError> {
    for conn in connections {
        let path_a = conn.a.to_flat_string();
        let path_b = conn.b.to_flat_string();
        let var_a = flat::VarName::new(&path_a);
        let var_b = flat::VarName::new(&path_b);
        let span = conn.span;

        // Only validate primitive-to-primitive connections directly
        // Connector-level connections are validated when expanded to sub-variables
        let a_is_primitive = flat.variables.contains_key(&var_a);
        let b_is_primitive = flat.variables.contains_key(&var_b);
        let a_subscript_prim = !a_is_primitive && is_subscripted_variable(&var_a, flat);
        let b_subscript_prim = !b_is_primitive && is_subscripted_variable(&var_b, flat);

        if (a_is_primitive || a_subscript_prim) && (b_is_primitive || b_subscript_prim) {
            // Validate flow prefix consistency (CONN-001/CONN-003)
            validate_flow_consistency(flat, &var_a, &var_b, span)?;

            // Validate type compatibility (CONN-002)
            validate_type_compatibility(flat, type_roots, &var_a, &var_b, span)?;

            // Validate array dimension compatibility (CONN-008)
            validate_dimension_compatibility(flat, &var_a, &var_b, span)?;
            continue;
        }

        // Connector-level connection: validate matched primitive members after expansion.
        let subs_a = find_sub_variables_indexed(&path_a, prefix_children, var_index);
        let subs_b = find_sub_variables_indexed(&path_b, prefix_children, var_index);
        if !subs_a.is_empty() && !subs_b.is_empty() {
            let ctx = ExpandedValidationCtx {
                path_a: &path_a,
                path_b: &path_b,
                flat,
                type_roots,
                span,
                var_index,
            };
            validate_expanded_connector_connection(&subs_a, &subs_b, &ctx)?;
        }
    }
    Ok(())
}

#[derive(Debug, Clone)]
struct ValidationVarInfo {
    flow: bool,
    type_id: TypeId,
    dims: Vec<i64>,
}

struct ExpandedValidationCtx<'a> {
    path_a: &'a str,
    path_b: &'a str,
    flat: &'a flat::Model,
    type_roots: &'a IndexMap<TypeId, TypeId>,
    span: Span,
    var_index: &'a ConnectionVarIndex,
}

fn get_validation_var_info(flat: &flat::Model, var: &flat::VarName) -> Option<ValidationVarInfo> {
    if let Some(v) = flat.variables.get(var) {
        return Some(ValidationVarInfo {
            flow: v.flow,
            type_id: v.type_id,
            dims: v.dims.clone(),
        });
    }

    // Subscripted references (e.g., "x[1]") refer to scalar elements of array vars.
    if !is_subscripted_variable(var, flat) {
        return None;
    }

    let (base_name, indexed_rank) = subscripted_base_var_with_rank(var, flat)?;
    let base_var = flat.variables.get(&base_name)?;
    let dims = if indexed_rank >= base_var.dims.len() {
        Vec::new()
    } else {
        base_var.dims[indexed_rank..].to_vec()
    };

    Some(ValidationVarInfo {
        flow: base_var.flow,
        type_id: base_var.type_id,
        // MLS array indexing semantics: indexing a subset of dimensions
        // preserves remaining dimensions (e.g., A[1] for A[2,3] yields [3]).
        dims,
    })
}

/// Validate that connected variables have consistent flow prefixes.
///
/// Per CONN-001 (Homogeneity) and CONN-003 (Flow-to-flow):
/// Both must be flow or both must be non-flow.
fn validate_flow_consistency(
    flat: &flat::Model,
    var_a: &flat::VarName,
    var_b: &flat::VarName,
    span: Span,
) -> Result<(), FlattenError> {
    let Some(info_a) = get_validation_var_info(flat, var_a) else {
        return Ok(());
    };
    let Some(info_b) = get_validation_var_info(flat, var_b) else {
        return Ok(());
    };
    let is_flow_a = info_a.flow;
    let is_flow_b = info_b.flow;

    if is_flow_a != is_flow_b {
        return Err(FlattenError::incompatible_connectors(
            format!(
                "{} ({})",
                var_a.as_str(),
                if is_flow_a { "flow" } else { "non-flow" }
            ),
            format!(
                "{} ({})",
                var_b.as_str(),
                if is_flow_b { "flow" } else { "non-flow" }
            ),
            span,
        ));
    }
    Ok(())
}

/// Validate that connected variables have compatible types.
///
/// Per CONN-002 (Type matching): Matched primitive components must have
/// the same primitive types (Real, Integer, Boolean, String).
fn validate_type_compatibility(
    flat: &flat::Model,
    type_roots: &IndexMap<TypeId, TypeId>,
    var_a: &flat::VarName,
    var_b: &flat::VarName,
    span: Span,
) -> Result<(), FlattenError> {
    let type_a =
        get_validation_var_info(flat, var_a).map(|v| canonical_type_id(v.type_id, type_roots));
    let type_b =
        get_validation_var_info(flat, var_b).map(|v| canonical_type_id(v.type_id, type_roots));

    // Only check if both types are known and different
    if let (Some(ta), Some(tb)) = (type_a, type_b)
        && !ta.is_unknown()
        && !tb.is_unknown()
        && ta != tb
    {
        return Err(FlattenError::incompatible_connectors(
            format!("{} (type_id: {:?})", var_a.as_str(), ta),
            format!("{} (type_id: {:?})", var_b.as_str(), tb),
            span,
        ));
    }
    Ok(())
}

fn canonical_type_id(type_id: TypeId, type_roots: &IndexMap<TypeId, TypeId>) -> TypeId {
    type_roots.get(&type_id).copied().unwrap_or(type_id)
}

/// Validate that connected variables have compatible array dimensions.
///
/// Per CONN-008 (MLS §9.2): Array dimensions must match for connection.
/// Per SPEC_0027: Dimension evaluation happens in typecheck phase before flatten.
///
/// Empty dimensions `[]` indicates a scalar variable (0-dimensional).
/// Scalars must connect to scalars; arrays must connect to same-dimension arrays.
fn validate_dimension_compatibility(
    flat: &flat::Model,
    var_a: &flat::VarName,
    var_b: &flat::VarName,
    span: Span,
) -> Result<(), FlattenError> {
    let Some(info_a) = get_validation_var_info(flat, var_a) else {
        return Ok(());
    };
    let Some(info_b) = get_validation_var_info(flat, var_b) else {
        return Ok(());
    };
    let dims_a = &info_a.dims;
    let dims_b = &info_b.dims;

    if dims_a != dims_b {
        return Err(FlattenError::incompatible_connectors(
            format!("{} (dims: {:?})", var_a.as_str(), dims_a),
            format!("{} (dims: {:?})", var_b.as_str(), dims_b),
            span,
        ));
    }
    Ok(())
}

fn validate_expanded_connector_connection(
    subs_a: &[flat::VarName],
    subs_b: &[flat::VarName],
    ctx: &ExpandedValidationCtx<'_>,
) -> Result<(), FlattenError> {
    let sub_match_index = ConnectionSubMatchIndex::new(ctx.path_b, subs_b, ctx.var_index);

    for sub_a in subs_a {
        let Some((suffix_a, indices_a)) = extract_suffix(sub_a.as_str(), ctx.path_a) else {
            continue;
        };
        let normalized_indices_a = strip_explicit_path_indices(&indices_a, ctx.path_a);

        let Some(var_b_match) =
            find_matching_var_b_indexed(&suffix_a, &normalized_indices_a, &sub_match_index)
        else {
            continue;
        };

        validate_flow_consistency(ctx.flat, sub_a, &var_b_match, ctx.span)?;
        validate_type_compatibility(ctx.flat, ctx.type_roots, sub_a, &var_b_match, ctx.span)?;
        validate_dimension_compatibility(ctx.flat, sub_a, &var_b_match, ctx.span)?;
    }
    Ok(())
}

// =============================================================================
// Connection Set Building
// =============================================================================

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
    prefix_children: &FxHashMap<String, Vec<flat::VarName>>,
    var_index: &ConnectionVarIndex,
) -> Vec<flat::VarName> {
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
) -> Vec<flat::VarName> {
    let segments = parse_path_with_indices(path);
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
) -> Vec<flat::VarName> {
    let segments = parse_path_with_indices(prefix);
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
) -> Option<flat::VarName> {
    sub_match_index.find_match(suffix, normalized_indices_a)
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
    flow_pairs: &mut Vec<(flat::VarName, flat::VarName)>,
    potential_uf: &mut UnionFind,
    stream_uf: &mut UnionFind,
) {
    // Case 0: Neither side is primitive - both expand to array element variables
    // E.g., connect(positiveThreshold.y, timerPositive.u) where both are on array components
    // Expands to positiveThreshold[i].y = timerPositive[i].u
    if !ctx.a_is_primitive && !ctx.b_is_primitive {
        let mut expanded_a = find_exact_match_with_array_expansion(ctx.path_a, var_index);
        let mut expanded_b = find_exact_match_with_array_expansion(ctx.path_b, var_index);
        if !expanded_a.is_empty() && expanded_a.len() == expanded_b.len() {
            expanded_a.sort_by(|a, b| a.as_str().cmp(b.as_str()));
            expanded_b.sort_by(|a, b| a.as_str().cmp(b.as_str()));
            for (va, vb) in expanded_a.iter().zip(expanded_b.iter()) {
                connect_primitive_vars(va, vb, flat, flow_pairs, potential_uf, stream_uf);
            }
            return;
        }
    }

    // Case 1: A is an array variable, B expands to multiple scalar variables
    // E.g., connect(v, voltageSensor.v) - connects v[i] to voltageSensor[i].v
    if ctx.a_is_primitive {
        let expanded_b = find_exact_match_with_array_expansion(ctx.path_b, var_index);
        if !expanded_b.is_empty() {
            connect_array_to_expanded(
                ctx.var_a,
                &expanded_b,
                flat,
                flow_pairs,
                potential_uf,
                stream_uf,
            );
            return;
        }
    }

    // Case 2: B is an array variable, A expands to multiple scalar variables
    // E.g., connect(voltageSensor.v, v) - connects voltageSensor[i].v to v[i]
    if ctx.b_is_primitive {
        let expanded_a = find_exact_match_with_array_expansion(ctx.path_a, var_index);
        if !expanded_a.is_empty() {
            connect_array_to_expanded(
                ctx.var_b,
                &expanded_a,
                flat,
                flow_pairs,
                potential_uf,
                stream_uf,
            );
            return;
        }
    }

    // Case 3: One side is a primitive output, the other is an array element reference
    // E.g., connect(inertialDelaySensitive[1].y, y[1]) where both are outputs
    // Only handle if the array variable is an output to avoid double-counting equations
    // (inputs don't need explicit connection equations as they're not unknowns)
    if let Some(set) = connect_output_to_array_element(ctx, flat) {
        let is_flow = set.iter().all(|v| is_flow_variable(flat, v));
        let is_stream = set.iter().all(|v| is_stream_variable(flat, v));
        if is_flow {
            for pair in set.windows(2) {
                flow_pairs.push((pair[0].clone(), pair[1].clone()));
            }
        } else if is_stream {
            for var in &set {
                stream_uf.union(&set[0], var);
            }
        } else {
            for var in &set {
                potential_uf.union(&set[0], var);
            }
        }
    }
}

/// Handle connection between a primitive output and an array element reference (MLS §9.2).
///
/// For connections like `connect(comp.y, arr[1])` where:
/// - `comp.y` is a primitive scalar output variable
/// - `arr[1]` is element 1 of array output variable `arr`
///
/// Per MLS §9.2, connection equations create equality constraints between connected
/// variables. This function handles the case where one side is a primitive and the
/// other is an array element reference (e.g., from a for-loop expanded connection).
///
/// Only handles connections where the array variable is an OUTPUT, since input
/// array connections don't need explicit equations (inputs aren't unknowns per MLS §4.4.2.2).
///
/// Returns the connection set if this pattern matches, None otherwise.
fn connect_output_to_array_element(
    ctx: &ArrayConnCtx,
    flat: &flat::Model,
) -> Option<Vec<flat::VarName>> {
    let a_array_info = parse_array_element_ref(ctx.path_a, flat);
    let b_array_info = parse_array_element_ref(ctx.path_b, flat);

    // Helper to check if base is an output array
    let is_output_array = |base: &flat::VarName| -> bool {
        flat.variables
            .get(base)
            .is_some_and(|v| matches!(v.causality, ast::Causality::Output(_)))
    };

    match (
        ctx.a_is_primitive,
        ctx.b_is_primitive,
        a_array_info,
        b_array_info,
    ) {
        // A is primitive, B is array[idx] where array is output
        (true, false, _, Some((base_b, idx_b))) if is_output_array(&base_b) => {
            let subscripted_b = flat::VarName::new(format!("{}[{}]", base_b.as_str(), idx_b));
            Some(vec![ctx.var_a.clone(), subscripted_b])
        }
        // B is primitive, A is array[idx] where array is output
        (false, true, Some((base_a, idx_a)), _) if is_output_array(&base_a) => {
            let subscripted_a = flat::VarName::new(format!("{}[{}]", base_a.as_str(), idx_a));
            Some(vec![subscripted_a, ctx.var_b.clone()])
        }
        // Both are array element references for output arrays
        (false, false, Some((base_a, idx_a)), Some((base_b, idx_b)))
            if is_output_array(&base_a) || is_output_array(&base_b) =>
        {
            let subscripted_a = flat::VarName::new(format!("{}[{}]", base_a.as_str(), idx_a));
            let subscripted_b = flat::VarName::new(format!("{}[{}]", base_b.as_str(), idx_b));
            Some(vec![subscripted_a, subscripted_b])
        }
        _ => None,
    }
}

/// Parse an array element reference like `x[1]` to extract base name and index.
///
/// Returns Some((base_var_name, index)) if path ends with [n] and the base is
/// an array variable in the flat model.
fn parse_array_element_ref(path: &str, flat: &flat::Model) -> Option<(flat::VarName, i64)> {
    let parts = parse_path_with_indices(path);
    let last = parts.last()?;
    let idx_group = extract_array_index(last)?;
    if !(idx_group.starts_with('[') && idx_group.ends_with(']')) {
        return None;
    }

    let idx_text = &idx_group[1..idx_group.len() - 1];
    if idx_text.contains(['[', ']', ',', ':']) {
        return None;
    }
    let idx: i64 = idx_text.parse().ok()?;

    let mut base_parts: Vec<String> = parts[..parts.len() - 1]
        .iter()
        .map(std::string::ToString::to_string)
        .collect();
    base_parts.push(strip_array_index(last).to_string());
    let base_var = flat::VarName::new(base_parts.join("."));

    let var = flat.variables.get(&base_var)?;
    if var.dims.is_empty() {
        return None; // Not an array
    }

    Some((base_var, idx))
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
    array_var: &flat::VarName,
    expanded_vars: &[flat::VarName],
    flat: &flat::Model,
    flow_pairs: &mut Vec<(flat::VarName, flat::VarName)>,
    potential_uf: &mut UnionFind,
    stream_uf: &mut UnionFind,
) {
    // Create synthetic subscripted variable names for the array
    // The array var "v" with expanded vars ["voltageSensor[1].v", "voltageSensor[2].v", "voltageSensor[3].v"]
    // creates connections: "v[1]" - "voltageSensor[1].v", etc.
    for expanded_var in expanded_vars {
        // Extract the index from the expanded variable name
        // e.g., "voltageSensor[1].v" -> extract "[1]"
        let Some(idx_str) = extract_first_array_index(expanded_var.as_str()) else {
            continue;
        };

        // Create synthetic subscripted name: "v" + "[1]" -> "v[1]"
        let subscripted_name = flat::VarName::new(format!("{}{}", array_var.as_str(), idx_str));

        // Determine flow/non-flow based on the array variable
        let is_flow = is_flow_variable(flat, array_var);
        let is_stream = is_stream_variable(flat, array_var);

        if is_flow {
            flow_pairs.push((subscripted_name, expanded_var.clone()));
        } else if is_stream {
            stream_uf.union(&subscripted_name, expanded_var);
        } else {
            potential_uf.union(&subscripted_name, expanded_var);
        }
    }
}

/// Extract the first array index from a variable path.
///
/// "voltageSensor[1].v" -> Some("[1]")
/// "plug_p.pin[2].i" -> Some("[2]") (from pin[2])
/// "r1.n.v" -> None
fn extract_first_array_index(path: &str) -> Option<String> {
    // Find the first '[' and its matching ']'
    let start = path.find('[')?;
    let end = path[start..].find(']')?;
    Some(path[start..start + end + 1].to_string())
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
    sub_a: &flat::VarName,
    path_a: &str,
    path_b: &str,
    sub_match_index: &ConnectionSubMatchIndex,
    ctx: &mut ConnectionBuildCtx<'_>,
) {
    // Skip parameters and constants — they are structural, not equation unknowns.
    // Connection equations for parameters (like connector.m = subcomponent.connector.m)
    // inflate the equation count without adding corresponding unknowns.
    if let Some(var_info) = ctx.flat.variables.get(sub_a)
        && matches!(
            var_info.variability,
            rumoca_ir_ast::Variability::Parameter(_) | rumoca_ir_ast::Variability::Constant(_)
        )
    {
        return;
    }

    let Some((suffix_a, indices_a)) = extract_suffix(sub_a.as_str(), path_a) else {
        return;
    };
    let normalized_indices_a = strip_explicit_path_indices(&indices_a, path_a);

    // Find matching variable in B with same suffix and array indices
    let Some(var_b_match) =
        find_matching_var_b_indexed(&suffix_a, &normalized_indices_a, sub_match_index)
    else {
        return;
    };
    let conn_a = scalarize_collapsed_connector_element(sub_a, path_a, ctx.flat);
    let mut conn_b = scalarize_collapsed_connector_element(&var_b_match, path_b, ctx.flat);

    // When B is an indexless collapsed connector-array member (e.g. `plugs_n.pin.i`)
    // matched against an indexed A sub-variable (e.g. `plug_p.pin[2].i`), preserve
    // element pairing by applying A's trailing element indices to B.
    //
    // This keeps per-element connection sets separate for array connect() expansions.
    let path_a_has_index = path_has_explicit_index(path_a);
    let path_b_has_index = path_has_explicit_index(path_b);
    let a_missing_last_seg_index = missing_index_on_last_prefix_segment(sub_a.as_str(), path_a);
    if !indices_a.is_empty() && !path_b_has_index {
        let b_dims = ctx
            .flat
            .variables
            .get(&conn_b)
            .map(|v| v.dims.clone())
            .unwrap_or_default();
        let dims_len = b_dims.len();
        let scalar_size = scalar_size_from_dims(&b_dims);

        // Some flattened connector-array members arrive with missing dims.
        // If A carries an index only on the last connector segment relative to
        // its prefix, project one trailing element index from A.
        //
        // Guardrails:
        // - Only project for actual multi-element arrays (scalar_size > 1).
        // - Validate projected indices are in-range for B's dimensions.
        // This prevents invalid projections like `starpoints.pin[2]` onto `pin[1]`.
        let projected_dims_len = if dims_len > 0 {
            if scalar_size > 1 { dims_len } else { 0 }
        } else if !path_a_has_index && a_missing_last_seg_index {
            1
        } else {
            0
        };
        if projected_dims_len > 0
            && let Some(idx_suffix) = select_indices_for_dims(&indices_a, projected_dims_len)
        {
            let projected_dims = if dims_len >= projected_dims_len {
                &b_dims[dims_len - projected_dims_len..]
            } else {
                &b_dims[..]
            };
            let index_in_bounds = projected_dims.is_empty()
                || projected_indices_within_dims(&idx_suffix, projected_dims);
            let idx_already_present = parse_path_with_indices(conn_b.as_str())
                .iter()
                .filter_map(|part| extract_array_index(part))
                .any(|idx| idx == idx_suffix);
            if index_in_bounds && !idx_already_present {
                conn_b = flat::VarName::new(format!("{}{}", conn_b.as_str(), idx_suffix));
            }
        }
    }

    // Connect matching sub-variables based on flow/non-flow type
    if is_flow_variable(ctx.flat, &conn_a) {
        ctx.flow_pairs.push((conn_a, conn_b));
    } else if is_stream_variable(ctx.flat, &conn_a) && is_stream_variable(ctx.flat, &conn_b) {
        // MLS §15.2 stream connectors are handled separately from flow/potential sets.
        ctx.stream_uf.union(&conn_a, &conn_b);
    } else {
        ctx.potential_uf.union(&conn_a, &conn_b);
    }
}

/// Process a single connection and update the connection structures.
fn process_connection(
    conn: &ast::InstanceConnection,
    flat: &flat::Model,
    var_index: &ConnectionVarIndex,
    flow_pairs: &mut Vec<(flat::VarName, flat::VarName)>,
    potential_uf: &mut UnionFind,
    stream_uf: &mut UnionFind,
    prefix_children: &FxHashMap<String, Vec<flat::VarName>>,
) {
    let path_a = conn.a.to_flat_string();
    let path_b = conn.b.to_flat_string();
    let var_a = flat::VarName::new(&path_a);
    let var_b = flat::VarName::new(&path_b);

    let a_is_primitive = flat.variables.contains_key(&var_a);
    let b_is_primitive = flat.variables.contains_key(&var_b);

    if a_is_primitive && b_is_primitive {
        connect_primitive_vars(&var_a, &var_b, flat, flow_pairs, potential_uf, stream_uf);
        return;
    }

    // Handle subscripted references to array variables: e.g., "comp.v[1]" where
    // flat.variables has "comp.v" as array[1]. The subscript comes from instantiation
    // resolving array dimension parameters. Treat as primitive since it refers to a
    // known variable's element.
    let a_subscript_prim = !a_is_primitive && is_subscripted_variable(&var_a, flat);
    let b_subscript_prim = !b_is_primitive && is_subscripted_variable(&var_b, flat);
    if (a_is_primitive || a_subscript_prim) && (b_is_primitive || b_subscript_prim) {
        connect_primitive_vars(&var_a, &var_b, flat, flow_pairs, potential_uf, stream_uf);
        return;
    }

    // At least one is a connector - try expansion
    let subs_a = find_sub_variables_indexed(&path_a, prefix_children, var_index);
    let subs_b = find_sub_variables_indexed(&path_b, prefix_children, var_index);

    if !subs_a.is_empty() && !subs_b.is_empty() {
        let mut ctx = ConnectionBuildCtx {
            flat,
            var_index,
            flow_pairs,
            potential_uf,
            stream_uf,
        };
        expand_connector_connection(&subs_a, &path_a, &path_b, &subs_b, &mut ctx);
        return;
    }

    // Handle output-to-output connections with array expansion
    let ctx = ArrayConnCtx {
        path_a: &path_a,
        path_b: &path_b,
        var_a: &var_a,
        var_b: &var_b,
        a_is_primitive,
        b_is_primitive,
    };
    connect_array_output_variables(&ctx, flat, var_index, flow_pairs, potential_uf, stream_uf);
}

/// Connect two primitive variables directly based on flow type.
/// Skips parameters and constants — they don't need connection equations.
fn connect_primitive_vars(
    var_a: &flat::VarName,
    var_b: &flat::VarName,
    flat: &flat::Model,
    flow_pairs: &mut Vec<(flat::VarName, flat::VarName)>,
    potential_uf: &mut UnionFind,
    stream_uf: &mut UnionFind,
) {
    // Skip parameters/constants — structural, not equation unknowns
    for var in [var_a, var_b] {
        if let Some(info) = flat.variables.get(var)
            && matches!(
                info.variability,
                rumoca_ir_ast::Variability::Parameter(_) | rumoca_ir_ast::Variability::Constant(_)
            )
        {
            return;
        }
    }

    let is_flow_a = is_flow_variable(flat, var_a);
    let is_flow_b = is_flow_variable(flat, var_b);
    let is_stream_a = is_stream_variable(flat, var_a);
    let is_stream_b = is_stream_variable(flat, var_b);

    if is_flow_a && is_flow_b {
        flow_pairs.push((var_a.clone(), var_b.clone()));
    } else if is_stream_a && is_stream_b {
        stream_uf.union(var_a, var_b);
    } else if !is_flow_a && !is_flow_b {
        if is_stream_a || is_stream_b {
            return;
        }
        potential_uf.union(var_a, var_b);
    }
    // Mismatched flow/non-flow is caught by validation
}

/// Expand a connector connection to its sub-variables.
fn expand_connector_connection(
    subs_a: &[flat::VarName],
    path_a: &str,
    path_b: &str,
    subs_b: &[flat::VarName],
    ctx: &mut ConnectionBuildCtx<'_>,
) {
    let sub_match_index = ConnectionSubMatchIndex::new(path_b, subs_b, ctx.var_index);
    for sub_a in subs_a {
        connect_sub_variable(sub_a, path_a, path_b, &sub_match_index, ctx);
    }
}

fn collect_existing_lhs_vars(flat: &flat::Model) -> std::collections::HashSet<flat::VarName> {
    let mut lhs_vars = std::collections::HashSet::new();
    for eq in &flat.equations {
        let flat::Expression::Binary { op, lhs, .. } = &eq.residual else {
            continue;
        };
        if !matches!(op, ast::OpBinary::Sub(_)) {
            continue;
        }
        if let flat::Expression::VarRef { name, .. } = lhs.as_ref() {
            lhs_vars.insert(name.clone());
        }
    }
    lhs_vars
}

fn collect_existing_var_refs(flat: &flat::Model) -> std::collections::HashSet<flat::VarName> {
    let mut refs = std::collections::HashSet::new();
    for eq in &flat.equations {
        eq.residual.collect_var_refs(&mut refs);
    }
    refs
}

fn stream_var_present_in_set(
    flat: &flat::Model,
    var: &flat::VarName,
    vars: &std::collections::HashSet<flat::VarName>,
) -> bool {
    if vars.contains(var) {
        return true;
    }
    if let Some(base) = subscripted_base_var(var, flat)
        && vars.contains(&base)
    {
        return true;
    }
    if let Some(stripped) = strip_embedded_array_indices(var.as_str())
        && vars.contains(&flat::VarName::new(stripped))
    {
        return true;
    }
    false
}

fn stream_set_touches_top_level_connector(flat: &flat::Model, vars: &[flat::VarName]) -> bool {
    vars.iter().any(|var| {
        first_path_segment_without_index(var.as_str())
            .is_some_and(|prefix| flat.top_level_connectors.contains(prefix))
    })
}

fn classify_stream_vars_by_presence(
    flat: &flat::Model,
    vars: Vec<flat::VarName>,
    present: &std::collections::HashSet<flat::VarName>,
) -> (Vec<flat::VarName>, Vec<flat::VarName>) {
    let mut defined = Vec::new();
    let mut undefined = Vec::new();
    for var in vars {
        if stream_var_present_in_set(flat, &var, present) {
            defined.push(var);
        } else {
            undefined.push(var);
        }
    }
    (defined, undefined)
}

fn append_stream_connection_sets_for_group(
    flat: &flat::Model,
    vars: Vec<flat::VarName>,
    existing_lhs_vars: &std::collections::HashSet<flat::VarName>,
    existing_var_refs: &mut Option<std::collections::HashSet<flat::VarName>>,
    result: &mut Vec<ConnectionSet>,
) {
    if vars.len() < 2 {
        return;
    }

    let touches_top_level = stream_set_touches_top_level_connector(flat, &vars);
    let (mut defined_streams, mut undefined_streams) =
        classify_stream_vars_by_presence(flat, vars, existing_lhs_vars);
    if undefined_streams.is_empty() {
        return;
    }

    undefined_streams.sort_by(|a, b| a.as_str().cmp(b.as_str()));
    defined_streams.sort_by(|a, b| a.as_str().cmp(b.as_str()));

    if touches_top_level {
        if undefined_streams.len() >= 2 {
            result.push(ConnectionSet {
                variables: undefined_streams,
                kind: ConnectionKind::Stream,
                scope: String::new(),
            });
        }
        return;
    }

    let refs = existing_var_refs.get_or_insert_with(|| collect_existing_var_refs(flat));
    let (referenced_streams, mut still_undefined) =
        classify_stream_vars_by_presence(flat, undefined_streams, refs);
    if still_undefined.is_empty() {
        return;
    }

    defined_streams.extend(referenced_streams);
    defined_streams.sort_by(|a, b| a.as_str().cmp(b.as_str()));
    still_undefined.sort_by(|a, b| a.as_str().cmp(b.as_str()));

    if let Some(anchor) = defined_streams.first().cloned() {
        for missing in still_undefined {
            result.push(ConnectionSet {
                variables: vec![missing, anchor.clone()],
                kind: ConnectionKind::Stream,
                scope: String::new(),
            });
        }
        return;
    }

    if still_undefined.len() >= 2 {
        result.push(ConnectionSet {
            variables: still_undefined,
            kind: ConnectionKind::Stream,
            scope: String::new(),
        });
    }
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
fn build_connection_sets(
    connections: &[&ast::InstanceConnection],
    flat: &flat::Model,
    prefix_children: &FxHashMap<String, Vec<flat::VarName>>,
    var_index: &ConnectionVarIndex,
) -> Vec<ConnectionSet> {
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    let mut result = Vec::new();

    // Group connections by scope (hierarchy level where connect was declared).
    let mut connections_by_scope: IndexMap<&str, Vec<&ast::InstanceConnection>> = IndexMap::new();
    for conn in connections {
        connections_by_scope
            .entry(&conn.scope)
            .or_default()
            .push(conn);
    }

    // Process each scope separately for flow pairs, globally for potential
    for (scope, scope_conns) in &connections_by_scope {
        let mut flow_pairs: Vec<(flat::VarName, flat::VarName)> = Vec::new();
        for conn in scope_conns {
            process_connection(
                conn,
                flat,
                var_index,
                &mut flow_pairs,
                &mut potential_uf,
                &mut stream_uf,
                prefix_children,
            );
        }

        // Build flow union-find for this scope
        let mut scope_uf = UnionFind::new();
        for (a, b) in flow_pairs {
            scope_uf.union(&a, &b);
        }
        for (_root, vars) in scope_uf.get_sets() {
            if vars.len() >= 2 {
                result.push(ConnectionSet {
                    variables: vars,
                    kind: ConnectionKind::Flow,
                    scope: (*scope).to_string(),
                });
            }
        }
    }

    // Extract potential connection sets (global — equality equations count
    // is the same whether merged or split: N-1 for N variables either way)
    for (_root, vars) in potential_uf.get_sets() {
        if vars.len() >= 2 {
            result.push(ConnectionSet {
                variables: vars,
                kind: ConnectionKind::Potential,
                scope: String::new(),
            });
        }
    }

    let stream_sets = stream_uf.get_sets();
    if !stream_sets.is_empty() {
        let existing_lhs_vars = collect_existing_lhs_vars(flat);
        let mut existing_var_refs: Option<std::collections::HashSet<flat::VarName>> = None;
        for (_root, vars) in stream_sets {
            append_stream_connection_sets_for_group(
                flat,
                vars,
                &existing_lhs_vars,
                &mut existing_var_refs,
                &mut result,
            );
        }
    }

    result
}

// =============================================================================
// flat::Equation Generation
// =============================================================================

/// Create a component reference expression for a variable name.
fn var_to_expr(var_name: &flat::VarName) -> flat::Expression {
    flat::Expression::VarRef {
        name: var_name.clone(),
        subscripts: Vec::new(),
    }
}

/// Create a residual expression: lhs - rhs (for equation lhs = rhs).
fn create_equality_residual(lhs: flat::Expression, rhs: flat::Expression) -> flat::Expression {
    flat::Expression::Binary {
        op: ast::OpBinary::Sub(ast::Token::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

/// Create a sum expression: a + b + c + ...
fn create_sum(exprs: Vec<flat::Expression>) -> flat::Expression {
    if exprs.is_empty() {
        return flat::Expression::Literal(flat::Literal::Integer(0));
    }

    // SAFETY: is_empty() check above guarantees at least one element
    let mut iter = exprs.into_iter();
    let mut result = iter.next().unwrap();

    for expr in iter {
        result = flat::Expression::Binary {
            op: ast::OpBinary::Add(ast::Token::default()),
            lhs: Box::new(result),
            rhs: Box::new(expr),
        };
    }

    result
}

// =============================================================================
// Task 2.2: Generate Effort (Potential) Equality Equations (CONN-001, CONN-026)
// =============================================================================

#[cfg(test)]
mod tests;
