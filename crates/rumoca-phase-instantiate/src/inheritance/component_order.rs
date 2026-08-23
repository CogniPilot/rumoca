//! MLS §5.6.1 positional ordering of a class's effective components.
//!
//! The inheritance merge in the parent module decides *which* components and
//! values are effective; this module decides the *position* each occupies in
//! the flattened model. MLS §5.6.1 replaces an extends-clause with the
//! flattened base class at the position of the extends-clause, so an
//! `extends Base` must produce the same variable order as declaring the base's
//! members inline at that point.

use super::resolve_base_class;
use crate::errors::InstantiateResult;
use indexmap::IndexSet;
use rumoca_core::{DefId, is_builtin_type};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use std::sync::Arc;

/// Memoizes the positional component order per class definition.
///
/// The order only depends on a class's own lexical layout and its base classes'
/// orders, so it is stable per `DefId` and safe to reuse across the many
/// component instances of the same class.
pub(super) type ComponentOrderCache = IndexMap<DefId, Arc<Vec<String>>>;

/// One element of a class body that contributes effective components, tagged
/// with the byte offset where it appears in the class's own source text.
enum OrderedElement<'a> {
    /// A component declared directly in this class.
    Own(&'a str),
    /// An `extends` clause; its base class contents splice in at this position.
    Extends(&'a ast::Extend),
}

/// Effective component names of `class` in MLS §5.6.1 declaration order.
///
/// MLS §5.6.1: "an extends-clause [...] is replaced by the flattened base
/// class" *at the position of the extends-clause*. The base's contents are
/// therefore spliced in where the `extends` appears among the derived class's
/// own declarations, not hoisted ahead of them, and this splice is recursive so
/// a grandparent's members land before an intermediate base's own members when
/// the intermediate's `extends` precedes them.
///
/// Own declarations and extends-clauses are interleaved by their source byte
/// offset (both live in this class's single source file, so the offsets are
/// directly comparable). Diamond-shared and redeclared names keep the position
/// of their first occurrence.
pub(super) fn ordered_effective_component_names_with_cache(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    cache: &mut ComponentOrderCache,
) -> InstantiateResult<Arc<Vec<String>>> {
    if let Some(def_id) = class.def_id
        && let Some(cached) = cache.get(&def_id)
    {
        return Ok(Arc::clone(cached));
    }

    let mut elements: Vec<(u32, usize, OrderedElement<'_>)> = Vec::new();
    let mut sequence = 0usize;
    for (name, comp) in &class.components {
        elements.push((comp.location.start, sequence, OrderedElement::Own(name)));
        sequence += 1;
    }
    for extend in &class.extends {
        if is_builtin_type(&extend.base_name.to_string()) {
            continue;
        }
        elements.push((
            extend.location.start,
            sequence,
            OrderedElement::Extends(extend),
        ));
        sequence += 1;
    }
    // Stable order by source position; the sequence tie-breaker keeps a
    // deterministic fallback when two elements report the same (dummy) offset.
    elements.sort_by_key(|(position, sequence, _)| (*position, *sequence));

    let mut ordered: IndexSet<String> = IndexSet::default();
    for (_, _, element) in elements {
        match element {
            OrderedElement::Own(name) => {
                ordered.insert(name.to_string());
            }
            OrderedElement::Extends(extend) => {
                splice_base_component_names(tree, extend, cache, &mut ordered)?;
            }
        }
    }

    let result = Arc::new(ordered.into_iter().collect::<Vec<String>>());
    if let Some(def_id) = class.def_id {
        cache.insert(def_id, Arc::clone(&result));
    }
    Ok(result)
}

/// Splice one `extends` clause's base component names into `ordered` at the
/// clause's position, skipping any deselected by `break` (MLS §7.4).
fn splice_base_component_names(
    tree: &ast::ClassTree,
    extend: &ast::Extend,
    cache: &mut ComponentOrderCache,
    ordered: &mut IndexSet<String>,
) -> InstantiateResult<()> {
    let base_class = resolve_base_class(tree, extend)?;
    let base_names = ordered_effective_component_names_with_cache(tree, base_class, cache)?;
    for name in base_names.iter() {
        if extend.break_names.iter().any(|broken| broken == name) {
            continue;
        }
        ordered.insert(name.clone());
    }
    Ok(())
}

/// Reorder an effective-component map into MLS §5.6.1 declaration order.
///
/// The merge in `process_extends_with_cache` decides *which* components and
/// values are effective; this restores the *position* an equivalent inline
/// declaration would have produced. Names present in the map but absent from
/// `order` (defensive: a synthesis path this ordering does not model) keep their
/// prior relative position at the end.
pub(super) fn reorder_components_by_declaration(
    components: &mut IndexMap<String, ast::Component>,
    order: &[String],
) {
    let mut remaining = std::mem::take(components);
    let mut reordered = IndexMap::default();
    for name in order {
        if let Some(comp) = remaining.shift_remove(name) {
            reordered.insert(name.clone(), comp);
        }
    }
    for (name, comp) in remaining {
        reordered.insert(name, comp);
    }
    *components = reordered;
}
