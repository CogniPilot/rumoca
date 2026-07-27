//! Conservative homogeneity gate for compact component-array instantiation.
//!
//! SPEC_0032 §1: an array of structured components may keep a compact family
//! owner only when every domain point produces the *same* scalar element
//! declaration, so the remaining points can be derived from a single
//! instantiated template by reindexing paths.
//!
//! The question this module answers is deliberately scoped to
//! `prepare_element_declaration`, which is the only place a domain point can
//! change the element declaration. Its three inputs are the array component's
//! `start`, the resolved `original_binding` (which already folds in the
//! enclosing `ctx.mod_env()` entry for this component), and the component's own
//! non-`each` `modifications` — and the checks below are the negation of each,
//! in the same order. A modifier that never reaches
//! `prepare_element_declaration` cannot make the elements differ, so the gate
//! does not consult the modification environment beyond `original_binding`.
//!
//! The gate is one-sided: it refuses some arrays whose elements are provably
//! index-independent (any non-`each` nested class modification, for instance),
//! because it asks "would a per-element rewrite fire?" rather than "would the
//! rewritten declarations differ?". Refusing costs performance; accepting a
//! genuinely per-element array would produce a wrong model, so the bias is
//! intentional.

use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;

use super::{index_binding_for_element, index_nested_modification_for_element};
use crate::InstantiateResult;

/// Whether an array component's elements are index-independent.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum HomogeneityVerdict {
    /// Every domain point has the same declaration; one template suffices.
    Compact,
    /// At least one per-element rewrite applies; keep scalar expansion. The
    /// payload is a stable reason string for tracing.
    Scalar(&'static str),
}

/// Everything the gate needs to decide whether elements are index-independent.
pub(super) struct HomogeneityInput<'a> {
    pub(super) comp: &'a ast::Component,
    /// Non-`each` modifications already resolved to array literals; a non-empty
    /// list means `distribute_mods_for_element` would rewrite each element.
    pub(super) resolved_mods: &'a [(String, ast::Expression)],
    /// The array-level binding that `index_binding_for_element` would index.
    pub(super) original_binding: Option<&'a ast::Expression>,
    pub(super) tree: &'a ast::ClassTree,
    pub(super) parent_components: &'a IndexMap<String, ast::Component>,
    /// Domain points to probe. The caller supplies the first and last tuples so
    /// index-dependent rewrites are observed at both ends of the domain.
    pub(super) probe_tuples: &'a [Vec<i64>],
}

/// Decide whether an array component can be instantiated from one template.
pub(super) fn component_array_homogeneity(
    input: &HomogeneityInput<'_>,
) -> InstantiateResult<HomogeneityVerdict> {
    if !input.resolved_mods.is_empty() {
        return Ok(HomogeneityVerdict::Scalar("array-valued modifier"));
    }
    if input.original_binding.is_some() {
        return Ok(HomogeneityVerdict::Scalar("array binding"));
    }
    if !start_is_index_independent(input.comp) {
        return Ok(HomogeneityVerdict::Scalar("indexed start attribute"));
    }
    modifications_are_index_independent(input)
}

/// MLS §7.2.5: an `each` (or absent) start attribute applies unchanged to every
/// element; anything else is indexed per element.
fn start_is_index_independent(comp: &ast::Component) -> bool {
    comp.start_has_each || matches!(comp.start, ast::Expression::Empty { .. })
}

fn modifications_are_index_independent(
    input: &HomogeneityInput<'_>,
) -> InstantiateResult<HomogeneityVerdict> {
    for (name, expr) in &input.comp.modifications {
        if input.comp.each_modifications.contains(name) {
            continue;
        }
        for tuple in input.probe_tuples {
            if let Some(verdict) = modification_probe(input, expr, tuple)? {
                return Ok(verdict);
            }
        }
    }
    Ok(HomogeneityVerdict::Compact)
}

/// Run the two per-element modifier rewrites for one probe point and report the
/// first one that would actually change the declaration.
fn modification_probe(
    input: &HomogeneityInput<'_>,
    expr: &ast::Expression,
    tuple: &[i64],
) -> InstantiateResult<Option<HomogeneityVerdict>> {
    let nested =
        index_nested_modification_for_element(input.tree, input.parent_components, expr, tuple)?;
    if nested.is_some() {
        return Ok(Some(HomogeneityVerdict::Scalar("indexed nested modifier")));
    }
    let indexed = index_binding_for_element(input.tree, input.parent_components, expr, tuple)?;
    // `distribute_component_ref_mods_for_element` only overwrites the element
    // declaration when the indexed form is not an `ArrayIndex` wrapper, so an
    // unchanged result (or a preserved wrapper) leaves the element identical.
    if indexed != *expr && !matches!(indexed, ast::Expression::ArrayIndex { .. }) {
        return Ok(Some(HomogeneityVerdict::Scalar(
            "indexed component-reference modifier",
        )));
    }
    Ok(None)
}
