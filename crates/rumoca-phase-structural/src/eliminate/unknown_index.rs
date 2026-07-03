//! Reverse index for boundary-unknown membership queries.
//!
//! `resolve_boundary_equations` repeatedly asks "which boundary unknowns does
//! this equation reference?". Answering by scanning every unknown per equation
//! is O(equations x unknowns); this module inverts the question by bucketing
//! unknowns under every key a variable reference can match, then confirming
//! candidates with the original presence predicate.

use std::collections::HashSet;

use indexmap::IndexMap;

use rumoca_ir_dae::{
    component_base_name, parse_embedded_subscripts, split_complex_field_suffix,
    var_ref_matches_unknown,
};

use super::{
    Dae, Expression, Reference, VarName, collect_exact_reference_expr_names_in_dae,
    collect_var_ref_nodes,
};
use crate::StructuralError;
use crate::variable_scope::DaeVariableScope;

/// Reverse index from variable references to the boundary unknowns they can
/// match under [`var_ref_mentions_unknown_for_presence`].
///
/// Candidate buckets over-approximate every `true` path of the matcher; each
/// candidate is confirmed with the original predicate, so the only correctness
/// obligation here is completeness of the buckets. This turns the boundary
/// pass from O(equations × unknowns) into O(equations × refs).
pub(super) struct BoundaryUnknownIndex<'a> {
    dae: &'a Dae,
    all_unknowns: &'a [VarName],
    /// Exact name text → position (id-equality and complex-alias targets).
    by_name: IndexMap<&'a str, usize>,
    /// (component base, trailing embedded indices) → positions, for refs that
    /// carry literal subscripts next to an unsubscripted or partially
    /// subscripted name.
    by_base_and_trailing: IndexMap<(String, Vec<i64>), Vec<usize>>,
    /// Component base → positions of unknowns whose embedded subscripts are
    /// all `1` (an unsubscripted aggregate ref matches its `[1,…,1]` scalar).
    all_one_embedded: IndexMap<String, Vec<usize>>,
    /// `.re`/`.im`-stripped full name → positions (complex field aliases).
    complex_stripped_name: IndexMap<String, Vec<usize>>,
    /// Component base → positions (target of base-level complex aliasing).
    by_base: IndexMap<String, Vec<usize>>,
    /// `.re`/`.im`-stripped component base → positions.
    complex_stripped_base: IndexMap<String, Vec<usize>>,
    /// Component-ref ident path → positions of aggregate (size > 1),
    /// non-index-component unknowns reachable via
    /// `reference_shares_component_base`.
    share_base: IndexMap<String, Vec<usize>>,
}

impl<'a> BoundaryUnknownIndex<'a> {
    pub(super) fn build(
        dae: &'a Dae,
        all_unknowns: &'a [VarName],
    ) -> Result<Self, StructuralError> {
        let scope = DaeVariableScope::new(dae);
        let mut index = Self {
            dae,
            all_unknowns,
            by_name: IndexMap::new(),
            by_base_and_trailing: IndexMap::new(),
            all_one_embedded: IndexMap::new(),
            complex_stripped_name: IndexMap::new(),
            by_base: IndexMap::new(),
            complex_stripped_base: IndexMap::new(),
            share_base: IndexMap::new(),
        };
        for (position, unknown) in all_unknowns.iter().enumerate() {
            let name = unknown.as_str();
            index.by_name.insert(name, position);
            if let Some((stripped, _)) = split_complex_field_suffix(name) {
                index
                    .complex_stripped_name
                    .entry(stripped.to_string())
                    .or_default()
                    .push(position);
            }
            if let Some(base) = component_base_name(name) {
                index.insert_base_keys(position, name, base);
            }
            if scope.size(unknown)? > 1
                && !scope.is_indexed_component_variable(unknown)
                && let Some(component_ref) =
                    scope.exact(unknown).and_then(|v| v.component_ref.as_ref())
            {
                index
                    .share_base
                    .entry(component_ref_ident_path(component_ref))
                    .or_default()
                    .push(position);
            }
        }
        Ok(index)
    }

    fn insert_base_keys(&mut self, position: usize, name: &str, base: String) {
        if let Some(indices) = parse_embedded_subscripts(name) {
            if indices.iter().all(|i| *i == 1) {
                self.all_one_embedded
                    .entry(base.clone())
                    .or_default()
                    .push(position);
            }
            self.by_base_and_trailing
                .entry((base.clone(), indices))
                .or_default()
                .push(position);
        }
        if let Some((stripped, _)) = split_complex_field_suffix(&base) {
            self.complex_stripped_base
                .entry(stripped.to_string())
                .or_default()
                .push(position);
        }
        self.by_base.entry(base).or_default().push(position);
    }

    /// Append every unknown position this var-ref could match. Over-generation
    /// is fine (candidates are re-checked); omission is not.
    fn append_candidate_positions(
        &self,
        name: &Reference,
        subscripts: &[rumoca_core::Subscript],
        out: &mut Vec<usize>,
    ) {
        let name_str = name.as_str();
        if let Some(&position) = self.by_name.get(name_str) {
            out.push(position);
        }
        if let Some((stripped, _)) = split_complex_field_suffix(name_str)
            && let Some(&position) = self.by_name.get(stripped)
        {
            out.push(position);
        }
        if let Some(bucket) = self.complex_stripped_name.get(name_str) {
            out.extend_from_slice(bucket);
        }
        if let Some(base) = component_base_name(name_str) {
            self.append_base_candidates(&base, subscripts, out);
        }
        if let Some(component_ref) = name.component_ref()
            && let Some(bucket) = self
                .share_base
                .get(&component_ref_ident_path(component_ref))
        {
            out.extend_from_slice(bucket);
        }
    }

    fn append_base_candidates(
        &self,
        base: &str,
        subscripts: &[rumoca_core::Subscript],
        out: &mut Vec<usize>,
    ) {
        if subscripts.is_empty() {
            if let Some(&position) = self.by_name.get(base) {
                out.push(position);
            }
            if let Some(bucket) = self.all_one_embedded.get(base) {
                out.extend_from_slice(bucket);
            }
        } else if let Some(indices) = literal_subscript_indices(subscripts)
            && let Some(bucket) = self.by_base_and_trailing.get(&(base.to_string(), indices))
        {
            out.extend_from_slice(bucket);
        }
        if let Some((stripped, _)) = split_complex_field_suffix(base)
            && let Some(bucket) = self.by_base.get(stripped)
        {
            out.extend_from_slice(bucket);
        }
        if let Some(bucket) = self.complex_stripped_base.get(base) {
            out.extend_from_slice(bucket);
        }
    }

    /// Positions (in `all_unknowns` order) of unresolved unknowns referenced by
    /// the expression, confirmed by the original presence predicate.
    fn live_unknown_positions(
        &self,
        expr: &Expression,
        resolved: &HashSet<VarName>,
    ) -> Result<Vec<usize>, StructuralError> {
        let mut var_refs = Vec::new();
        collect_var_ref_nodes(expr, &mut var_refs);
        let mut exact_names = Vec::new();
        collect_exact_reference_expr_names_in_dae(self.dae, expr, &mut exact_names);
        let mut candidates = Vec::new();
        for (name, subscripts) in &var_refs {
            self.append_candidate_positions(name, subscripts, &mut candidates);
        }
        for exact_name in &exact_names {
            if let Some(&position) = self.by_name.get(exact_name.as_str()) {
                candidates.push(position);
            }
        }
        candidates.sort_unstable();
        candidates.dedup();
        let mut live = Vec::new();
        for position in candidates {
            let unknown = &self.all_unknowns[position];
            if !resolved.contains(unknown)
                && refs_contain_unknown(&var_refs, &exact_names, unknown, self.dae)?
            {
                live.push(position);
            }
        }
        Ok(live)
    }
}

fn component_ref_ident_path(component_ref: &rumoca_core::ComponentReference) -> String {
    let mut path = String::new();
    for part in &component_ref.parts {
        if !path.is_empty() {
            path.push('.');
        }
        path.push_str(&part.ident);
    }
    path
}

/// Literal integer values of subscripts, mirroring the acceptance rules of
/// `subscripts_match_indices` (plain indices and literal integer expressions).
fn literal_subscript_indices(subscripts: &[rumoca_core::Subscript]) -> Option<Vec<i64>> {
    subscripts
        .iter()
        .map(|sub| match sub {
            rumoca_core::Subscript::Index { value, .. } => Some(*value),
            rumoca_core::Subscript::Expr { expr, .. } => match expr.as_ref() {
                Expression::Literal {
                    value: rumoca_core::Literal::Integer(i),
                    ..
                } => Some(*i),
                _ => None,
            },
            rumoca_core::Subscript::Colon { .. } => None,
        })
        .collect()
}

pub(super) fn checked_count_live_unknowns(
    expr: &Expression,
    unknown_index: &BoundaryUnknownIndex<'_>,
    resolved: &HashSet<VarName>,
) -> Result<usize, StructuralError> {
    Ok(unknown_index.live_unknown_positions(expr, resolved)?.len())
}

pub(super) fn has_any_live_unknown(
    expr: &Expression,
    unknown_index: &BoundaryUnknownIndex<'_>,
    resolved: &HashSet<VarName>,
) -> Result<bool, StructuralError> {
    Ok(!unknown_index
        .live_unknown_positions(expr, resolved)?
        .is_empty())
}

/// Find the live scalar unknowns referenced by an expression.
pub(super) fn find_live_scalar_unknowns(
    expr: &Expression,
    unknown_index: &BoundaryUnknownIndex<'_>,
    resolved: &HashSet<VarName>,
) -> Result<Vec<VarName>, StructuralError> {
    let scope = DaeVariableScope::new(unknown_index.dae);
    let mut live = Vec::new();
    for position in unknown_index.live_unknown_positions(expr, resolved)? {
        let unknown = &unknown_index.all_unknowns[position];
        if scope.size(unknown)? == 1 {
            live.push(unknown.clone());
        }
    }
    Ok(live)
}

fn refs_contain_unknown(
    refs: &[(Reference, Vec<rumoca_core::Subscript>)],
    exact_names: &[VarName],
    unknown: &VarName,
    dae: &Dae,
) -> Result<bool, StructuralError> {
    if exact_names.iter().any(|name| name == unknown) {
        return Ok(true);
    }
    for (name, subscripts) in refs {
        if var_ref_matches_unknown(name, subscripts.as_slice(), unknown) {
            return Ok(true);
        }
    }
    if exact_scalar_unknown_exists(dae, unknown)? {
        return Ok(false);
    }
    for (name, subscripts) in refs {
        if var_ref_mentions_unknown_for_presence(name, subscripts.as_slice(), unknown, dae)? {
            return Ok(true);
        }
    }
    Ok(false)
}

fn exact_scalar_unknown_exists(dae: &Dae, unknown: &VarName) -> Result<bool, StructuralError> {
    let scope = DaeVariableScope::new(dae);
    Ok(scope.exact(unknown).is_some() && scope.size(unknown)? == 1)
}

fn unknown_scalar_size(dae: &Dae, unknown: &VarName) -> Result<usize, StructuralError> {
    DaeVariableScope::new(dae).size(unknown)
}

fn var_ref_mentions_unknown_for_presence(
    name: &Reference,
    subscripts: &[rumoca_core::Subscript],
    unknown: &VarName,
    dae: &Dae,
) -> Result<bool, StructuralError> {
    if var_ref_matches_unknown(name, subscripts, unknown) {
        return Ok(true);
    }

    // MLS §10.6 / SPEC_0019: array equations stay aggregate before
    // scalarization. Any indexed or sliced reference to an aggregate unknown is
    // still a live reference to that unknown at this phase.
    if unknown_scalar_size(dae, unknown)? <= 1 {
        return Ok(false);
    }
    let scope = DaeVariableScope::new(dae);
    if scope.is_indexed_component_variable(unknown) {
        return Ok(false);
    }

    Ok(scope.reference_shares_component_base(name, unknown))
}
