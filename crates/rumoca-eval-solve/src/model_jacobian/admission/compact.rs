//! Admission of one compact derivative node (`Map` or `AffineStencil`) into
//! the shared state Jacobian, or its refusal with a named reason.
//!
//! Only integer facts are decided here, all over the ordinal domain: every
//! binder runs `0..extent` and a load reads `base + Σ stride·k`. The
//! extents come from the domain's own checked count. Index arithmetic is
//! bounded in `i128`, so no source bound can overflow the proof itself.
//! Admission issues nothing; the node's construction consumes these facts.

use super::*;
use rumoca_core::StructuredIndexDomain;
use rumoca_ir_solve::{AffineStencilIndexStrideTerm, AffineStencilLoadStride, TensorOutputMap};

/// One distinct state-load variable of a node. Every `LoadY` with this exact
/// affine key reads the same state scalar at every point and shares one seed.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::model_jacobian) struct LoadKey {
    pub(in crate::model_jacobian) base: i128,
    /// Stride per binder, in binder order (missing terms are zero).
    pub(in crate::model_jacobian) strides: Vec<i128>,
}

/// The checked integer facts of one admitted compact node.
#[derive(Debug)]
pub(in crate::model_jacobian) struct CompactNode<'source> {
    pub(in crate::model_jacobian) extents: Vec<usize>,
    pub(in crate::model_jacobian) base_ops: &'source [LinearOp],
    /// Distinct state loads; position is the node-local seed.
    pub(in crate::model_jacobian) seeds: Vec<LoadKey>,
    /// The seed each `LoadY` reads, by operation position.
    pub(in crate::model_jacobian) load_seeds: Vec<(usize, usize)>,
    pub(in crate::model_jacobian) output: LoadKey,
    pub(in crate::model_jacobian) span: Span,
}

pub(super) fn admit<'source>(
    domain: &StructuredIndexDomain,
    output_map: &TensorOutputMap,
    base_ops: &'source [LinearOp],
    load_strides: &[AffineStencilLoadStride],
    states: usize,
    span: Span,
) -> Result<CompactNode<'source>, ModelStateJacobianError> {
    let refuse = |reason| unsupported(reason, Some(span));
    let extents = domain
        .extents()
        .map_err(|_| refuse("compact domain extent is invalid"))?;
    if extents.contains(&0) {
        return Err(refuse("compact domain is empty"));
    }
    // A domain with no binders is one point with no ordinal to place it by;
    // it is not constructed as a compact node.
    if extents.is_empty() {
        return Err(refuse("compact domain has no binders"));
    }
    check_operations(base_ops, span)?;
    let stores = base_ops
        .iter()
        .filter(|op| matches!(op, LinearOp::StoreOutput { .. }))
        .count();
    if stores != 1 {
        return Err(refuse(
            "compact node must store exactly one output per point",
        ));
    }
    let output = LoadKey {
        base: output_map.start as i128,
        strides: strides(&output_map.strides, extents.len())
            .ok_or_else(|| refuse("compact output stride names no binder"))?,
    };
    prove_index(&output, &extents, states)
        .map_err(|_| refuse("compact output placement leaves the state domain"))?;
    if load_strides.iter().any(|stride| {
        !matches!(
            base_ops.get(stride.op_position),
            Some(LinearOp::LoadY { .. })
        )
    }) {
        return Err(refuse("compact strided load is not a state load"));
    }
    let (seeds, load_seeds) =
        state_load_keys(base_ops, load_strides, &extents, states).map_err(refuse)?;
    Ok(CompactNode {
        extents,
        base_ops,
        seeds,
        load_seeds,
        output,
        span,
    })
}

/// Whether `rows`, in row-major point order, is exactly the node's output key.
pub(super) fn rows_match(node: &CompactNode<'_>, rows: &[usize]) -> bool {
    let mut ordinals = vec![0usize; node.extents.len()];
    let mut expected = Vec::with_capacity(rows.len());
    loop {
        let row = node
            .output
            .strides
            .iter()
            .zip(&ordinals)
            .fold(node.output.base, |sum, (stride, k)| {
                sum + stride * *k as i128
            });
        expected.push(row);
        let mut axis = ordinals.len();
        loop {
            if axis == 0 {
                return expected.len() == rows.len()
                    && expected
                        .iter()
                        .zip(rows)
                        .all(|(want, row)| *want == *row as i128);
            }
            axis -= 1;
            ordinals[axis] += 1;
            if ordinals[axis] < node.extents[axis] {
                break;
            }
            ordinals[axis] = 0;
        }
        if expected.len() > rows.len() {
            return false;
        }
    }
}

fn strides(terms: &[AffineStencilIndexStrideTerm], rank: usize) -> Option<Vec<i128>> {
    let mut strides = vec![0i128; rank];
    for term in terms {
        *strides.get_mut(term.dimension)? += term.stride as i128;
    }
    Some(strides)
}

/// Exact `[min, max]` of `base + Σ_{e≤d} strides[e]·k_e` for every prefix `d`.
/// Each term is monotone in its own ordinal, so the extremes sit at box corners.
fn prefix_ranges(key: &LoadKey, extents: &[usize]) -> Vec<(i128, i128)> {
    let (mut low, mut high) = (key.base, key.base);
    let mut ranges = vec![(low, high)];
    for (stride, extent) in key.strides.iter().zip(extents) {
        let reach = stride * (*extent as i128 - 1);
        if reach < 0 {
            low += reach;
        } else {
            high += reach;
        }
        ranges.push((low, high));
    }
    ranges
}

/// Every product `stride·k` and every prefix sum of the issued index fits
/// `i64`, and the final index stays in `0..len`, so no index node can fault.
fn prove_index(key: &LoadKey, extents: &[usize], len: usize) -> Result<(), &'static str> {
    let fits = |value: i128| i64::try_from(value).is_ok();
    let products_fit = key
        .strides
        .iter()
        .zip(extents)
        .all(|(stride, extent)| fits(stride * (*extent as i128 - 1)));
    let ranges = prefix_ranges(key, extents);
    if !products_fit || ranges.iter().any(|(low, high)| !fits(*low) || !fits(*high)) {
        return Err("compact load index exceeds i64");
    }
    let (low, high) = ranges[ranges.len() - 1];
    if low < 0 || high >= len as i128 {
        return Err("compact load index leaves the state vector");
    }
    Ok(())
}

fn gcd(a: i128, b: i128) -> i128 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        (a, b) = (b, a % b);
    }
    a
}

/// Could two distinct keys read the same state scalar at some point? Exact for
/// one binder. For more it is conservative (range plus gcd): it may refuse an
/// alias-free node, which then keeps the scalar path, but never admits an
/// aliasing one.
fn may_alias(a: &LoadKey, b: &LoadKey, extents: &[usize]) -> bool {
    let target = b.base - a.base;
    let delta: Vec<i128> = a
        .strides
        .iter()
        .zip(&b.strides)
        .map(|(x, y)| x - y)
        .collect();
    if delta.iter().all(|d| *d == 0) {
        return target == 0;
    }
    if let ([d], [extent]) = (delta.as_slice(), extents) {
        return target % d == 0 && (0..*extent as i128).contains(&(target / d));
    }
    let g = delta.iter().fold(0, |acc, d| gcd(acc, *d));
    if target % g != 0 {
        return false;
    }
    let (mut low, mut high) = (0i128, 0i128);
    for (d, extent) in delta.iter().zip(extents) {
        let reach = d * (*extent as i128 - 1);
        if reach < 0 {
            low += reach;
        } else {
            high += reach;
        }
    }
    (low..=high).contains(&target)
}

/// Distinct state-load keys, and the seed of each `LoadY` by position.
type StateLoads = (Vec<LoadKey>, Vec<(usize, usize)>);

/// The node's distinct state loads in first-occurrence order, and the seed of
/// each `LoadY` by position. Equal keys share one seed; distinct keys that may
/// meet at a point refuse, because their contributions would otherwise
/// combine outside the issued arithmetic order.
fn state_load_keys(
    base_ops: &[LinearOp],
    load_strides: &[AffineStencilLoadStride],
    extents: &[usize],
    states: usize,
) -> Result<StateLoads, &'static str> {
    let mut keys: Vec<LoadKey> = Vec::new();
    let mut load_seeds = Vec::new();
    for (position, op) in base_ops.iter().enumerate() {
        let LinearOp::LoadY { index, .. } = op else {
            continue;
        };
        let terms: Vec<_> = load_strides
            .iter()
            .filter(|stride| stride.op_position == position)
            .flat_map(|stride| stride.terms.iter().cloned())
            .collect();
        let key = LoadKey {
            base: *index as i128,
            strides: strides(&terms, extents.len()).ok_or("compact load stride names no binder")?,
        };
        prove_index(&key, extents, states)?;
        let seed = match keys.iter().position(|existing| *existing == key) {
            Some(seed) => seed,
            None => {
                keys.push(key);
                keys.len() - 1
            }
        };
        load_seeds.push((position, seed));
    }
    for (i, a) in keys.iter().enumerate() {
        if keys[i + 1..].iter().any(|b| may_alias(a, b, extents)) {
            return Err("affine loads alias a state column");
        }
    }
    Ok((keys, load_seeds))
}

#[cfg(test)]
mod tests;
