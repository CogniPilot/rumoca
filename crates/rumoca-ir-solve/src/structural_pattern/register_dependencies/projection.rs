//! Iterative scalar/range projection of source-issued dependency families.
use std::collections::BTreeMap;

use super::{
    CallInput, ConcatenateInput, DependencyState, DependencyView, Family, FamilyKind, Term,
};
use crate::IndexIntervals;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct Selection {
    pub start: usize,
    pub count: usize,
    pub stride: usize,
}

#[derive(Clone, Copy)]
pub(super) struct Query<'a> {
    pub family: &'a Family,
    pub selection: Selection,
}

impl Query<'_> {
    fn key(self) -> (usize, Selection) {
        (std::ptr::from_ref(self.family) as usize, self.selection)
    }
}

enum Pending<'a> {
    Enter(Query<'a>),
    Finish(Query<'a>, Vec<Query<'a>>, DependencyState),
}

pub(super) fn project(roots: Vec<Query<'_>>) -> Option<DependencyState> {
    let mut results = BTreeMap::new();
    let mut pending: Vec<_> = roots.iter().copied().map(Pending::Enter).collect();
    while let Some(next) = pending.pop() {
        match next {
            Pending::Enter(query) if results.contains_key(&query.key()) => {}
            Pending::Enter(query) => enter(query, &mut pending, &mut results)?,
            Pending::Finish(query, children, base) => {
                let state = base.union(union_results(&children, &results)?);
                results.insert(query.key(), state);
            }
        }
    }
    union_results(&roots, &results)
}

fn union_results(
    queries: &[Query<'_>],
    results: &BTreeMap<(usize, Selection), DependencyState>,
) -> Option<DependencyState> {
    queries
        .iter()
        .try_fold(DependencyState::Empty, |state, query| {
            Some(state.union(results.get(&query.key())?.clone()))
        })
}

fn enter<'a>(
    query: Query<'a>,
    pending: &mut Vec<Pending<'a>>,
    results: &mut BTreeMap<(usize, Selection), DependencyState>,
) -> Option<()> {
    if query.selection.count == 0 || query.family.empty {
        results.insert(query.key(), DependencyState::Empty);
        return Some(());
    }
    match &query.family.kind {
        FamilyKind::Uniform(state) => {
            results.insert(query.key(), state.clone());
        }
        FamilyKind::Load {
            lanes,
            primal,
            tangent,
        } => {
            results.insert(
                query.key(),
                load(query.selection, *lanes, *primal, *tangent)?,
            );
        }
        FamilyKind::Union { lanes, terms } => {
            schedule(
                query,
                union_children(query.selection, *lanes, terms)?,
                DependencyState::Empty,
                pending,
            );
        }
        FamilyKind::Transpose {
            source,
            rows,
            columns,
            value_width,
        } => {
            schedule(
                query,
                transpose_children(query.selection, source, *rows, *columns, *value_width)?,
                DependencyState::Empty,
                pending,
            );
        }
        FamilyKind::Product { shape, lhs, rhs } => {
            schedule(
                query,
                product_children(query.selection, shape, lhs, rhs)?,
                DependencyState::Empty,
                pending,
            );
        }
        FamilyKind::Concatenate { inputs, count } => {
            schedule(
                query,
                concatenate_children(query.selection, inputs, *count)?,
                DependencyState::Empty,
                pending,
            );
        }
        FamilyKind::Overlay { source, extra } => {
            schedule(
                query,
                source.requests(query.selection)?,
                extra.clone(),
                pending,
            );
        }
        FamilyKind::Patch {
            projection,
            base,
            patch,
        } => {
            schedule(
                query,
                patch_children(query.selection, projection, base.as_ref(), patch.as_ref())?,
                DependencyState::Empty,
                pending,
            );
        }
        FamilyKind::Call { inputs } => {
            let (children, base) = call_children(query.selection, inputs)?;
            schedule(query, children, base, pending);
        }
    }
    Some(())
}

fn schedule<'a>(
    query: Query<'a>,
    children: Vec<Query<'a>>,
    base: DependencyState,
    pending: &mut Vec<Pending<'a>>,
) {
    pending.push(Pending::Finish(query, children.clone(), base));
    pending.extend(children.into_iter().rev().map(Pending::Enter));
}

fn union_children(selection: Selection, lanes: usize, terms: &[Term]) -> Option<Vec<Query<'_>>> {
    let mut children = Vec::new();
    for term in terms {
        for lane in 0..lanes {
            if term.output_lane.is_some_and(|selected| selected != lane) {
                continue;
            }
            let Some(selected) = select_lane(selection, lanes, lane)? else {
                continue;
            };
            let source = Selection {
                start: (selected.start / lanes)
                    .checked_mul(term.stride)?
                    .checked_mul(lanes)?
                    .checked_add(term.source_lane)?,
                count: selected.count,
                stride: selected.stride.checked_mul(term.stride)?,
            };
            children.extend(term.source.requests(source)?);
        }
    }
    Some(children)
}

fn transpose_children(
    selection: Selection,
    source: &DependencyView,
    rows: usize,
    columns: usize,
    value_width: usize,
) -> Option<Vec<Query<'_>>> {
    if source.source_empty()
        || (selection.start == 0
            && selection.stride == 1
            && selection.count == rows * columns * value_width)
    {
        return source.requests(Selection {
            start: 0,
            count: rows * columns * value_width,
            stride: 1,
        });
    }
    let mut children = Vec::new();
    for offset in 0..selection.count {
        let index = selection
            .start
            .checked_add(offset.checked_mul(selection.stride)?)?;
        let index = super::super::transpose_source_offset(index, rows, columns, value_width);
        children.extend(source.requests(Selection {
            start: index,
            count: 1,
            stride: 0,
        })?);
    }
    Some(children)
}

fn product_children<'a>(
    selection: Selection,
    shape: &super::super::MatrixMultiplyShape,
    lhs: &'a DependencyView,
    rhs: &'a DependencyView,
) -> Option<Vec<Query<'a>>> {
    if selection.start == 0
        && selection.stride == 1
        && selection.count == shape.rows * shape.columns * shape.lanes
    {
        let mut children = lhs.requests(Selection {
            start: 0,
            count: shape.rows * shape.inner * shape.lanes,
            stride: 1,
        })?;
        children.extend(rhs.requests(Selection {
            start: 0,
            count: shape.inner * shape.columns * shape.lanes,
            stride: 1,
        })?);
        return Some(children);
    }
    let mut children = Vec::new();
    for offset in 0..selection.count {
        let index = selection
            .start
            .checked_add(offset.checked_mul(selection.stride)?)?;
        let element = index / shape.lanes;
        let lane = index % shape.lanes;
        for source_lane in (0..=lane).filter(|source_lane| *source_lane == lane || shape.lanes == 2)
        {
            children.extend(lhs.requests(Selection {
                start: (element / shape.columns * shape.inner) * shape.lanes + source_lane,
                count: shape.inner,
                stride: shape.lanes,
            })?);
            children.extend(rhs.requests(Selection {
                start: (element % shape.columns) * shape.lanes + source_lane,
                count: shape.inner,
                stride: shape.columns * shape.lanes,
            })?);
        }
    }
    Some(children)
}

fn concatenate_children(
    selection: Selection,
    inputs: &[ConcatenateInput],
    count: usize,
) -> Option<Vec<Query<'_>>> {
    if selection.start == 0 && selection.count == count && selection.stride == 1 {
        return inputs.iter().try_fold(Vec::new(), |mut children, input| {
            children.extend(input.source.requests(Selection {
                start: 0,
                count: input.count,
                stride: 1,
            })?);
            Some(children)
        });
    }
    let mut children = Vec::new();
    for offset in 0..selection.count {
        let index = selection
            .start
            .checked_add(offset.checked_mul(selection.stride)?)?;
        let (input, source) = inputs
            .iter()
            .find_map(|input| Some((input, input.mapping.source(index)?)))?;
        children.extend(input.source.requests(Selection {
            start: source,
            count: 1,
            stride: 0,
        })?);
    }
    Some(children)
}

fn patch_children<'a>(
    selection: Selection,
    projection: &super::super::tensor_update::FixedUpdateProjection,
    base: Option<&'a DependencyView>,
    patch: Option<&'a DependencyView>,
) -> Option<Vec<Query<'a>>> {
    if projection.whole() {
        return patch?.requests(selection);
    }
    if patch.is_none() {
        return base?.requests(selection);
    }
    let mut children = Vec::new();
    for offset in 0..selection.count {
        let index = selection
            .start
            .checked_add(offset.checked_mul(selection.stride)?)?;
        let (source, index) = match projection.patch_offset(index) {
            Some(index) => (patch?, index),
            None => (base?, index),
        };
        children.extend(source.requests(Selection {
            start: index,
            count: 1,
            stride: 0,
        })?);
    }
    Some(children)
}

fn call_children(
    selection: Selection,
    inputs: &[CallInput],
) -> Option<(Vec<Query<'_>>, DependencyState)> {
    let mut children = Vec::new();
    let mut base = DependencyState::Empty;
    for input in inputs {
        match input {
            CallInput::Whole(state) => base = base.union(state.clone()),
            CallInput::Projected { source, projection } if !source.source_empty() => {
                children.extend(projected_call_children(selection, source, projection)?);
            }
            CallInput::Projected { .. } => {}
        }
    }
    Some((children, base))
}

fn projected_call_children<'a>(
    selection: Selection,
    source: &'a DependencyView,
    projection: &crate::typed_program::CheckedCallDependencyProjection,
) -> Option<Vec<Query<'a>>> {
    if projection.is_flat_identity() {
        return source.requests(selection);
    }
    let mut children = Vec::new();
    for offset in 0..selection.count {
        let index = selection
            .start
            .checked_add(offset.checked_mul(selection.stride)?)?;
        for input in projection.input_elements(index)? {
            children.extend(source.requests(Selection {
                start: input,
                count: 1,
                stride: 0,
            })?);
        }
    }
    Some(children)
}

fn load(
    selection: Selection,
    lanes: usize,
    primal: Option<usize>,
    tangent: Option<usize>,
) -> Option<DependencyState> {
    let mut result = DependencyState::Empty;
    for (lane, base) in [primal, tangent].into_iter().enumerate().take(lanes) {
        let Some(base) = base else {
            continue;
        };
        let Some(selected) = select_lane(selection, lanes, lane)? else {
            continue;
        };
        let first = base.checked_add(selected.start / lanes)?;
        let stride = selected.stride / lanes;
        let state = match stride {
            0 => DependencyState::singleton(first),
            1 => DependencyState::from_range(first, first.checked_add(selected.count)?),
            _ => DependencyState::from_intervals(IndexIntervals::of(
                (0..selected.count).map(|index| first + index * stride),
            )),
        };
        result = result.union(state);
    }
    Some(result)
}

/// Partition a checked primal/dual selection without enumerating its cells.
fn select_lane(selection: Selection, lanes: usize, lane: usize) -> Option<Option<Selection>> {
    if !(1..=2).contains(&lanes) {
        return None;
    }
    if selection.count == 0 {
        return Some(None);
    }
    if selection.stride.is_multiple_of(lanes) {
        return Some((selection.start % lanes == lane).then_some(selection));
    }
    // Only the interleaved dual case remains: an odd step alternates lanes.
    let first = usize::from(selection.start % lanes != lane);
    if first >= selection.count {
        return Some(None);
    }
    Some(Some(Selection {
        start: selection
            .start
            .checked_add(first.checked_mul(selection.stride)?)?,
        count: (selection.count - first - 1) / lanes + 1,
        stride: selection.stride.checked_mul(lanes)?,
    }))
}
