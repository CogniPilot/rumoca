//! Exact scalar incidence derived from checked DAE expression views.

mod causal;
pub(crate) mod projection;
pub mod rows;

use std::collections::{HashMap, HashSet};

use rumoca_eval_dae::{ScalarCoordinateProjectionCache, for_each_scalar_coordinate_cached};
use rumoca_ir_dae as dae;

use crate::incidence::rows::{IncidenceRows, IncidenceRowsBuilder};
use crate::types::{EquationRef, StructuralError, UnknownId};

/// Unit-coefficient admission issued only while projecting a complete DAE residual.
/// The row storage is private; graph-only incidence cannot manufacture this proof.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct CausalCandidates {
    rows: Box<IncidenceRows>,
}

impl CausalCandidates {
    pub(crate) fn row(&self, index: usize) -> &[usize] {
        self.rows.row(index)
    }

    fn get(&self, index: usize) -> Option<&[usize]> {
        self.rows.get(index)
    }
}

/// Incidence data branded to the inspected DAE.
#[derive(Debug)]
pub struct Incidence<'dae> {
    pub n_eq: usize,
    pub n_var: usize,
    pub eq_unknowns: IncidenceRows,
    pub(crate) causal_candidates: Option<CausalCandidates>,
    pub unknowns: Vec<UnknownId<'dae>>,
    pub unknown_spans: Vec<rumoca_core::Span>,
    pub equation_refs: Vec<EquationRef>,
    pub equation_spans: Vec<rumoca_core::Span>,
    pub(crate) structured_matching: Vec<StructuredMatchingFamily>,
    /// First scalar row of each continuous owner, terminated by `n_eq`, so
    /// owner `i` occupies rows `owner_first_row[i]..owner_first_row[i + 1]`.
    /// This lets the incremental reduction copy an untouched owner's rows
    /// verbatim from a prior round.
    pub(crate) owner_first_row: Vec<usize>,
}

/// The lifetime-free scalar rows of one round's incidence, detached from the
/// DAE they were branded to so the next round can reuse the runs of equations
/// its demotion does not touch.
///
/// A direct-state demotion substitutes only the demoted variable's
/// coordinates, so every continuous owner that does not reference that variable
/// keeps the exact incident-unknown set it had; the variable catalog is the
/// unified dense [`dae::VariableId`] order, which a role change leaves in
/// place, so unknown positions are stable across the rebuild. Reusing those
/// runs reproduces the from-scratch incidence bit for bit.
#[derive(Clone, Debug)]
pub(crate) struct ReusableIncidence {
    rows: IncidenceRows,
    causal_candidates: CausalCandidates,
    equation_spans: Vec<rumoca_core::Span>,
    owner_first_row: Vec<usize>,
}

impl ReusableIncidence {
    pub(crate) fn from_incidence(incidence: &Incidence<'_>) -> Self {
        Self {
            rows: incidence.eq_unknowns.clone(),
            causal_candidates: incidence
                .causal_candidates
                .clone()
                .expect("DAE incidence owns causal proofs"),
            equation_spans: incidence.equation_spans.clone(),
            owner_first_row: incidence.owner_first_row.clone(),
        }
    }

    fn owner_rows(&self, owner: usize) -> Option<std::ops::Range<usize>> {
        let start = *self.owner_first_row.get(owner)?;
        let end = *self.owner_first_row.get(owner.checked_add(1)?)?;
        Some(start..end)
    }
}

/// A prior round's incidence together with the owners a demotion touched.
///
/// `touched_owner[i]` is `true` when continuous owner `i` references the
/// demoted variable and must be projected afresh; every other owner's rows are
/// copied from `prev`.
pub(crate) struct IncidenceReuse<'a> {
    prev: &'a ReusableIncidence,
    touched_owner: &'a [bool],
}

impl<'a> IncidenceReuse<'a> {
    pub(crate) fn new(prev: &'a ReusableIncidence, touched_owner: &'a [bool]) -> Self {
        Self {
            prev,
            touched_owner,
        }
    }
}

/// Exact affine matching candidates for one checked structured family.
#[derive(Debug, Clone)]
pub(crate) struct StructuredMatchingFamily {
    pub(crate) first_equation_index: usize,
    pub(crate) equations_per_point: usize,
    pub(crate) point_count: usize,
    pub(crate) extents: Vec<usize>,
    pub(crate) cell_strides: Vec<usize>,
    pub(crate) base_unknowns: Vec<usize>,
    pub(crate) unknown_steps: Vec<Vec<i64>>,
    pub(crate) span: rumoca_core::Span,
}

impl StructuredMatchingFamily {
    pub(crate) fn candidate(
        &self,
        point: usize,
        equation_position: usize,
    ) -> Option<(usize, usize)> {
        if point >= self.point_count || equation_position >= self.equations_per_point {
            return None;
        }
        let equation = point
            .checked_mul(self.equations_per_point)?
            .checked_add(equation_position)?
            .checked_add(self.first_equation_index)?;
        let mut unknown = i64::try_from(*self.base_unknowns.get(equation_position)?).ok()?;
        for (dimension, step) in self
            .unknown_steps
            .get(equation_position)?
            .iter()
            .enumerate()
        {
            let coordinate = cell_coordinate(
                point,
                *self.cell_strides.get(dimension)?,
                *self.extents.get(dimension)?,
            );
            unknown = unknown.checked_add(i64::try_from(coordinate).ok()?.checked_mul(*step)?)?;
        }
        Some((equation, usize::try_from(unknown).ok()?))
    }

    pub(crate) fn row_count(&self) -> Option<usize> {
        self.point_count.checked_mul(self.equations_per_point)
    }

    pub(crate) fn row_range(&self) -> Option<std::ops::Range<usize>> {
        let end = self.first_equation_index.checked_add(self.row_count()?)?;
        Some(self.first_equation_index..end)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum UnknownKey {
    Derivative { variable: u32, scalar: u32 },
    Algebraic { variable: u32, scalar: u32 },
}

pub(crate) fn build_incidence<'dae>(
    view: dae::DaeView<'dae>,
) -> Result<Incidence<'dae>, StructuralError> {
    build_incidence_inner(view, None)
}

/// Build the incidence of a demoted system by reusing the rows of every owner
/// the demotion did not touch.
///
/// The result is asserted equal to a from-scratch [`build_incidence`] under
/// debug assertions (see [`assert_reuse_matches_fresh`]), so the reuse can only
/// ever reproduce the exact rows the full projection would have produced.
pub(crate) fn build_incidence_reusing<'dae>(
    view: dae::DaeView<'dae>,
    reuse: IncidenceReuse<'_>,
) -> Result<Incidence<'dae>, StructuralError> {
    let incidence = build_incidence_inner(view, Some(reuse))?;
    #[cfg(debug_assertions)]
    assert_reuse_matches_fresh(view, &incidence);
    Ok(incidence)
}

fn build_incidence_inner<'dae>(
    view: dae::DaeView<'dae>,
    reuse: Option<IncidenceReuse<'_>>,
) -> Result<Incidence<'dae>, StructuralError> {
    let unknowns = build_unknowns(view)?;
    let mut builder = IncidenceBuilder {
        view,
        unknown_map: &unknowns.map,
        rows: IncidenceRowsBuilder::default(),
        causal_candidates: IncidenceRowsBuilder::default(),
        equation_refs: Vec::new(),
        equation_spans: Vec::new(),
        structured_matching: Vec::new(),
        projection_cache: ScalarCoordinateProjectionCache::default(),
    };
    let mut owner_first_row = Vec::new();
    for (owner_index, owner) in view.continuous_owners().enumerate() {
        #[cfg(feature = "tracing")]
        let owner_start = std::time::Instant::now();
        let first_row = builder.rows.row_count();
        owner_first_row.push(first_row);
        let reused = reuse
            .as_ref()
            .filter(|reuse| {
                !reuse
                    .touched_owner
                    .get(owner_index)
                    .copied()
                    .unwrap_or(true)
            })
            .map(|reuse| reuse.prev)
            .and_then(|prev| builder.copy_owner_rows(prev, owner_index))
            .is_some();
        if !reused {
            projection::visit_owner_rows(view, owner, |row| {
                builder.push_expression(
                    row.expression,
                    row.scalar,
                    row.domain_point,
                    row.provenance,
                )
            })?;
        }
        if let dae::ContinuousOwnerView::Structured { family, .. } = owner {
            builder.record_family(family, first_row);
        }
        #[cfg(feature = "tracing")]
        tracing::debug!(
            target: "rumoca_phase_structural::timing",
            rows = builder.rows.row_count(),
            elapsed_seconds = owner_start.elapsed().as_secs_f64(),
            "projected continuous owner"
        );
    }
    let eq_unknowns = builder.rows.finish();
    let n_eq = eq_unknowns.len();
    owner_first_row.push(n_eq);
    Ok(Incidence {
        n_eq,
        n_var: unknowns.ids.len(),
        eq_unknowns,
        causal_candidates: Some(CausalCandidates {
            rows: Box::new(builder.causal_candidates.finish()),
        }),
        unknowns: unknowns.ids,
        unknown_spans: unknowns.spans,
        equation_refs: builder.equation_refs,
        equation_spans: builder.equation_spans,
        structured_matching: builder.structured_matching,
        owner_first_row,
    })
}

/// Fail loudly whenever a reused incidence diverges from the full projection.
///
/// The scalar rows and their provenance spans are the only data the reuse path
/// copies; the unknown catalog, its spans, and the structured-matching
/// descriptors are rederived from `view` on every build. Equal rows therefore
/// certify equal incidence.
#[cfg(debug_assertions)]
fn assert_reuse_matches_fresh(view: dae::DaeView<'_>, reused: &Incidence<'_>) {
    let fresh = build_incidence_inner(view, None)
        .expect("a system that reuses incidence rebuilt its full incidence once already");
    debug_assert!(
        reused.eq_unknowns == fresh.eq_unknowns,
        "incremental incidence rows diverged from the full projection"
    );
    debug_assert!(
        reused.causal_candidates == fresh.causal_candidates,
        "incremental causal proofs diverged from the full projection"
    );
    debug_assert!(
        reused.equation_spans == fresh.equation_spans,
        "incremental incidence spans diverged from the full projection"
    );
}

struct UnknownCatalog<'dae> {
    map: HashMap<UnknownKey, usize>,
    ids: Vec<UnknownId<'dae>>,
    spans: Vec<rumoca_core::Span>,
}

fn build_unknowns<'dae>(view: dae::DaeView<'dae>) -> Result<UnknownCatalog<'dae>, StructuralError> {
    let mut map = HashMap::new();
    let mut unknowns = Vec::new();
    let mut spans = Vec::new();
    for (_, variable) in view.variables() {
        let declaration = variable.declaration().span();
        match variable.identity() {
            dae::VariableIdentity::State(state) => {
                let count = structural_scalar_count(variable)?;
                for scalar in 0..count {
                    let scalar = checked_scalar_ordinal(scalar, declaration)?;
                    insert_unknown(
                        &mut map,
                        &mut unknowns,
                        &mut spans,
                        UnknownKey::Derivative {
                            variable: state.index(),
                            scalar,
                        },
                        UnknownId::Derivative { state, scalar },
                        declaration,
                    );
                }
            }
            dae::VariableIdentity::Algebraic(algebraic) => {
                let count = structural_scalar_count(variable)?;
                for scalar in 0..count {
                    let scalar = checked_scalar_ordinal(scalar, declaration)?;
                    insert_unknown(
                        &mut map,
                        &mut unknowns,
                        &mut spans,
                        UnknownKey::Algebraic {
                            variable: algebraic.index(),
                            scalar,
                        },
                        UnknownId::Algebraic {
                            variable: algebraic,
                            scalar,
                        },
                        declaration,
                    );
                }
            }
            dae::VariableIdentity::Parameter(_)
            | dae::VariableIdentity::Input(_)
            | dae::VariableIdentity::DiscreteReal(_)
            | dae::VariableIdentity::DiscreteValue(_) => {}
        }
    }
    Ok(UnknownCatalog {
        map,
        ids: unknowns,
        spans,
    })
}

fn structural_scalar_count(variable: dae::VariableView<'_>) -> Result<usize, StructuralError> {
    variable
        .value_type()
        .scalar_count()
        .ok_or_else(|| StructuralError::ContractViolation {
            reason: format!(
                "continuous variable `{}` must be projected to primitive coordinates before structural analysis",
                variable.name()
            ),
            span: variable.declaration().span(),
        })
}

fn insert_unknown<'dae>(
    map: &mut HashMap<UnknownKey, usize>,
    unknowns: &mut Vec<UnknownId<'dae>>,
    spans: &mut Vec<rumoca_core::Span>,
    key: UnknownKey,
    unknown: UnknownId<'dae>,
    span: rumoca_core::Span,
) {
    let index = unknowns.len();
    let replaced = map.insert(key, index);
    debug_assert!(
        replaced.is_none(),
        "checked variable scalar identity is unique"
    );
    unknowns.push(unknown);
    spans.push(span);
}

struct IncidenceBuilder<'map, 'dae> {
    view: dae::DaeView<'dae>,
    unknown_map: &'map HashMap<UnknownKey, usize>,
    rows: IncidenceRowsBuilder,
    causal_candidates: IncidenceRowsBuilder,
    equation_refs: Vec<EquationRef>,
    equation_spans: Vec<rumoca_core::Span>,
    structured_matching: Vec<StructuredMatchingFamily>,
    projection_cache: ScalarCoordinateProjectionCache<'dae>,
}

impl<'dae> IncidenceBuilder<'_, 'dae> {
    fn push_expression(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
        domain_point: Option<(dae::DomainId<'dae>, &[i64])>,
        owner: dae::DaeProvenance,
    ) -> Result<(), StructuralError> {
        let occurrences = self.unknown_dependencies(expression, scalar, domain_point)?;
        let candidates = self.unit_candidates(expression, scalar, domain_point)?;
        self.causal_candidates.push_occurrences(&candidates);
        self.rows.push_occurrences(&occurrences);
        self.equation_refs
            .push(EquationRef(self.equation_refs.len()));
        self.equation_spans.push(owner.span());
        Ok(())
    }

    /// Copy owner `owner_index`'s scalar rows verbatim from a prior round.
    ///
    /// Returns `None` (leaving the builder untouched) when the prior incidence
    /// has no matching owner range, so the caller falls back to a full
    /// projection.
    fn copy_owner_rows(&mut self, prev: &ReusableIncidence, owner_index: usize) -> Option<()> {
        let range = prev.owner_rows(owner_index)?;
        for row in range {
            let occurrences = prev.rows.get(row)?;
            let span = *prev.equation_spans.get(row)?;
            let candidates = prev.causal_candidates.get(row)?;
            self.rows.push_canonical_row(occurrences);
            self.causal_candidates.push_canonical_row(candidates);
            self.equation_refs
                .push(EquationRef(self.equation_refs.len()));
            self.equation_spans.push(span);
        }
        Some(())
    }

    fn record_family(&mut self, family: dae::StructuredFamilyView<'dae>, first_row: usize) {
        let domain = self
            .view
            .domain(family.domain())
            .expect("checked structured family domain resolves");
        let equations_per_point = family.bodies().len();
        if let Some(descriptor) = derive_structured_matching(
            &self.rows,
            first_row,
            equations_per_point,
            domain.extents(),
            family.provenance().span(),
        ) {
            self.structured_matching.push(descriptor);
        }
    }
}

fn resolve_coordinate(
    unknown_map: &HashMap<UnknownKey, usize>,
    coordinate: dae::CoordinateView<'_>,
    scalar: usize,
) -> Option<usize> {
    let scalar = u32::try_from(scalar).ok()?;
    let key = match coordinate {
        dae::CoordinateView::Derivative(state) => UnknownKey::Derivative {
            variable: state.index(),
            scalar,
        },
        dae::CoordinateView::Algebraic(variable) => UnknownKey::Algebraic {
            variable: variable.index(),
            scalar,
        },
        dae::CoordinateView::Parameter(_)
        | dae::CoordinateView::Input(_)
        | dae::CoordinateView::State(_)
        | dae::CoordinateView::DiscreteReal(_)
        | dae::CoordinateView::DiscreteValue(_)
        | dae::CoordinateView::PreDiscreteReal(_)
        | dae::CoordinateView::PreDiscreteValue(_)
        // A `pre()` read is the coordinate's settled left limit at event
        // entry, not the unknown itself, so it carries no incidence — the
        // Mean shape's `y_last = f*pre(x)` must not read as an equation
        // that solves for `x`.
        | dae::CoordinateView::PreState(_)
        | dae::CoordinateView::PreAlgebraic(_)
        | dae::CoordinateView::Time
        | dae::CoordinateView::ClockInterval(_)
        | dae::CoordinateView::Condition(_)
        | dae::CoordinateView::Delay(_)
        | dae::CoordinateView::Previous(_)
        | dae::CoordinateView::Terminal(_)
        | dae::CoordinateView::Binder(_)
        | dae::CoordinateView::FunctionParameter(_) => return None,
    };
    unknown_map.get(&key).copied()
}

fn derive_structured_matching(
    rows: &IncidenceRowsBuilder,
    first: usize,
    per_point: usize,
    extents: &[u32],
    span: rumoca_core::Span,
) -> Option<StructuredMatchingFamily> {
    if per_point == 0 || extents.contains(&0) {
        return None;
    }
    let point_count = extents
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))?;
    let cell_strides = row_major_strides(extents)?;
    let base_unknowns = (0..per_point)
        .map(|position| singleton_builder_row(rows, first + position))
        .collect::<Option<Vec<_>>>()?;
    let mut unknown_steps = vec![vec![0i64; extents.len()]; per_point];
    for position in 0..per_point {
        for (dimension, stride) in cell_strides.iter().copied().enumerate() {
            if extents[dimension] <= 1 {
                continue;
            }
            let row = first
                .checked_add(stride.checked_mul(per_point)?)?
                .checked_add(position)?;
            let corner = singleton_builder_row(rows, row)?;
            unknown_steps[position][dimension] = i64::try_from(corner)
                .ok()?
                .checked_sub(i64::try_from(base_unknowns[position]).ok()?)?;
        }
    }
    let descriptor = StructuredMatchingFamily {
        first_equation_index: first,
        equations_per_point: per_point,
        point_count,
        extents: extents.iter().map(|extent| *extent as usize).collect(),
        cell_strides,
        base_unknowns,
        unknown_steps,
        span,
    };
    for point in 0..point_count {
        for position in 0..per_point {
            let (row, predicted) = descriptor.candidate(point, position)?;
            if singleton_builder_row(rows, row)? != predicted {
                return None;
            }
        }
    }
    Some(descriptor)
}

fn singleton_builder_row(rows: &IncidenceRowsBuilder, row: usize) -> Option<usize> {
    let values = rows.row(row)?;
    (values.len() == 1).then(|| values[0])
}

fn row_major_strides(extents: &[u32]) -> Option<Vec<usize>> {
    let mut strides = vec![1usize; extents.len()];
    for index in (0..extents.len().saturating_sub(1)).rev() {
        strides[index] = strides[index + 1].checked_mul(extents[index + 1] as usize)?;
    }
    Some(strides)
}

fn cell_coordinate(point: usize, stride: usize, extent: usize) -> usize {
    if stride == 0 || extent == 0 {
        return 0;
    }
    (point / stride) % extent
}

fn checked_scalar_ordinal(scalar: usize, span: rumoca_core::Span) -> Result<u32, StructuralError> {
    u32::try_from(scalar).map_err(|_| StructuralError::ContractViolation {
        reason: "variable scalar ordinal exceeds u32 capacity".to_string(),
        span,
    })
}

pub(crate) fn build_dependency_graph(
    eq_unknowns: &IncidenceRows,
    match_var: &[Option<usize>],
    n_eq: usize,
) -> Vec<Vec<usize>> {
    let mut adjacency = vec![Vec::new(); n_eq];
    for (equation, edges) in adjacency.iter_mut().enumerate() {
        for &unknown in eq_unknowns.row(equation) {
            let Some(owner) = match_var.get(unknown).copied().flatten() else {
                continue;
            };
            if owner != equation {
                edges.push(owner);
            }
        }
        edges.sort_unstable();
        edges.dedup();
    }
    adjacency
}

/// Construct incidence for non-DAE solver projections.
pub fn solver_incidence(
    rows: Vec<HashSet<usize>>,
    unknown_count: usize,
) -> Result<Incidence<'static>, StructuralError> {
    let eq_unknowns = IncidenceRows::from_sets(rows);
    if eq_unknowns
        .iter()
        .any(|row| row.iter().any(|index| *index >= unknown_count))
    {
        return Err(StructuralError::UnspannedContractViolation {
            reason: "solver incidence row names an unknown outside its table".to_string(),
        });
    }
    let n_eq = eq_unknowns.len();
    Ok(Incidence {
        n_eq,
        n_var: unknown_count,
        eq_unknowns,
        // Graph-only callers supply no DAE expression or coefficient claim.
        causal_candidates: None,
        unknowns: (0..unknown_count).map(UnknownId::Solver).collect(),
        unknown_spans: Vec::new(),
        equation_refs: (0..n_eq).map(EquationRef).collect(),
        equation_spans: Vec::new(),
        structured_matching: Vec::new(),
        owner_first_row: (0..=n_eq).collect(),
    })
}

#[cfg(test)]
mod reuse_tests {
    use rumoca_core::{SourceMap, Span, TypeId, VarName};

    use super::*;

    fn at(source: rumoca_core::SourceId, start: usize, end: usize) -> dae::DaeProvenance {
        dae::DaeProvenance::source(Span::from_offsets(source, start, end)).unwrap()
    }

    /// Three residuals over two states and one algebraic, enough that owners
    /// carry distinct incident-unknown runs.
    fn coupled_model() -> dae::Dae {
        let mut sources = SourceMap::new();
        let source = sources.add(
            "reuse_incidence.mo",
            "Real x; Real v; Real a; equation der(x) - v = 0; der(v) - a = 0; a - x = 0;",
        );
        let x_at = at(source, 0, 6);
        let v_at = at(source, 8, 14);
        let a_at = at(source, 16, 22);
        let first_at = at(source, 33, 47);
        let second_at = at(source, 49, 63);
        let third_at = at(source, 65, 74);
        dae::Dae::construct(sources, |model| {
            let real = model.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    x_at,
                )
            })?;
            let (x, v, a) = model.variables(|variables| {
                Ok((
                    variables.state(
                        VarName::new("x"),
                        real,
                        x_at,
                        dae::VariableAttributes::default(),
                    )?,
                    variables.state(
                        VarName::new("v"),
                        real,
                        v_at,
                        dae::VariableAttributes::default(),
                    )?,
                    variables.algebraic(
                        VarName::new("a"),
                        real,
                        a_at,
                        dae::VariableAttributes::default(),
                    )?,
                ))
            })?;
            let (first, second, third) = model.expressions(|expressions| {
                let dx = expressions
                    .at(first_at)
                    .coordinate(dae::CoordinateInput::Derivative(x))?;
                let v_read = expressions
                    .at(first_at)
                    .coordinate(dae::CoordinateInput::State(v))?;
                let first =
                    expressions
                        .at(first_at)
                        .binary(dae::BinaryOperator::Subtract, dx, v_read)?;
                let dv = expressions
                    .at(second_at)
                    .coordinate(dae::CoordinateInput::Derivative(v))?;
                let a_read = expressions
                    .at(second_at)
                    .coordinate(dae::CoordinateInput::Algebraic(a))?;
                let second =
                    expressions
                        .at(second_at)
                        .binary(dae::BinaryOperator::Subtract, dv, a_read)?;
                let a_again = expressions
                    .at(third_at)
                    .coordinate(dae::CoordinateInput::Algebraic(a))?;
                let x_read = expressions
                    .at(third_at)
                    .coordinate(dae::CoordinateInput::State(x))?;
                let third = expressions.at(third_at).binary(
                    dae::BinaryOperator::Subtract,
                    a_again,
                    x_read,
                )?;
                Ok((first, second, third))
            })?;
            model.continuous(|continuous| {
                continuous.equation(first_at, |equation| equation.residual(first))?;
                continuous.equation(second_at, |equation| equation.residual(second))?;
                continuous.equation(third_at, |equation| equation.residual(third))?;
                Ok(())
            })
        })
        .unwrap()
    }

    /// Copying an untouched owner's rows and reprojecting a touched owner both
    /// reproduce the from-scratch incidence exactly, for every partition of the
    /// owners into touched and untouched. The rows and their provenance spans
    /// are the only reused data, so their equality certifies equal incidence.
    #[test]
    fn every_touched_mask_reproduces_the_full_incidence() {
        let model = coupled_model();
        model.inspect(|view| {
            let fresh = build_incidence(view).expect("coupled model has scalar incidence");
            let reusable = ReusableIncidence::from_incidence(&fresh);
            let owner_count = view.continuous_owners().count();
            assert_eq!(owner_count, 3);
            for pattern in 0..(1u32 << owner_count) {
                let touched: Vec<bool> = (0..owner_count)
                    .map(|owner| pattern & (1 << owner) != 0)
                    .collect();
                let reused =
                    build_incidence_reusing(view, IncidenceReuse::new(&reusable, &touched))
                        .expect("reuse keeps the incidence buildable");
                assert_eq!(
                    reused.eq_unknowns, fresh.eq_unknowns,
                    "rows diverged for touched pattern {pattern:b}"
                );
                assert_eq!(
                    reused.equation_spans, fresh.equation_spans,
                    "spans diverged for touched pattern {pattern:b}"
                );
                assert_eq!(reused.causal_candidates, fresh.causal_candidates);
                assert_eq!(reused.owner_first_row, fresh.owner_first_row);
            }
        });
    }
}
