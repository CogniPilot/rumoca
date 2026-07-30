//! Exact scalar incidence derived from checked DAE expression views.

pub mod rows;

use std::collections::{HashMap, HashSet};

use rumoca_eval_dae::for_each_scalar_coordinate;
use rumoca_ir_dae as dae;

use crate::incidence::rows::{IncidenceRows, IncidenceRowsBuilder};
use crate::types::{EquationRef, StructuralError, UnknownId};

/// Incidence data branded to the inspected DAE.
#[derive(Debug)]
pub struct Incidence<'dae> {
    pub n_eq: usize,
    pub n_var: usize,
    pub eq_unknowns: IncidenceRows,
    pub unknowns: Vec<UnknownId<'dae>>,
    pub unknown_spans: Vec<rumoca_core::Span>,
    pub equation_refs: Vec<EquationRef>,
    pub equation_spans: Vec<rumoca_core::Span>,
    pub(crate) structured_matching: Vec<StructuredMatchingFamily>,
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
    let unknowns = build_unknowns(view)?;
    let mut builder = IncidenceBuilder {
        view,
        unknown_map: &unknowns.map,
        rows: IncidenceRowsBuilder::default(),
        equation_refs: Vec::new(),
        equation_spans: Vec::new(),
        structured_matching: Vec::new(),
    };
    for owner in view.continuous_owners() {
        match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => {
                builder.push_expression(equation.residual(), 0, None, equation.provenance())?;
            }
            dae::ContinuousOwnerView::Structured { family, .. } => {
                builder.push_family(family)?;
            }
        }
    }
    let eq_unknowns = builder.rows.finish();
    let n_eq = eq_unknowns.len();
    Ok(Incidence {
        n_eq,
        n_var: unknowns.ids.len(),
        eq_unknowns,
        unknowns: unknowns.ids,
        unknown_spans: unknowns.spans,
        equation_refs: builder.equation_refs,
        equation_spans: builder.equation_spans,
        structured_matching: builder.structured_matching,
    })
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
    equation_refs: Vec<EquationRef>,
    equation_spans: Vec<rumoca_core::Span>,
    structured_matching: Vec<StructuredMatchingFamily>,
}

impl<'dae> IncidenceBuilder<'_, 'dae> {
    fn push_expression(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
        domain_point: Option<(dae::DomainId<'dae>, &[i64])>,
        owner: dae::DaeProvenance,
    ) -> Result<(), StructuralError> {
        let mut occurrences = Vec::new();
        for_each_scalar_coordinate(
            self.view,
            expression,
            scalar,
            domain_point,
            |coordinate, scalar| {
                if let Some(unknown) = self.resolve_coordinate(coordinate, scalar) {
                    occurrences.push(unknown);
                }
            },
        )
        .map_err(|source| StructuralError::Projection {
            reason: source.to_string(),
            span: projection_span(&source),
        })?;
        self.rows.push_occurrences(&occurrences);
        self.equation_refs
            .push(EquationRef(self.equation_refs.len()));
        self.equation_spans.push(owner.span());
        Ok(())
    }

    fn push_family(
        &mut self,
        family: dae::StructuredFamilyView<'dae>,
    ) -> Result<(), StructuralError> {
        let first_row = self.rows.row_count();
        let domain = self
            .view
            .domain(family.domain())
            .expect("checked structured family domain resolves");
        let point_count = domain.scalar_count() as usize;
        let equations_per_point = family.bodies().len();
        for point in 0..point_count {
            let values = domain
                .structured()
                .index_tuple_at(point)
                .expect("checked structured domain stays valid")
                .expect("point ordinal is inside checked domain");
            for body in family.bodies().iter() {
                let scalar = family
                    .scalar_view()
                    .body_scalar(point, domain.extents())
                    .expect("checked family view projects its domain point");
                self.push_expression(
                    body,
                    scalar,
                    Some((family.domain(), &values)),
                    family.provenance(),
                )?;
            }
        }
        if let Some(descriptor) = derive_structured_matching(
            &self.rows,
            first_row,
            equations_per_point,
            domain.extents(),
            family.provenance().span(),
        ) {
            self.structured_matching.push(descriptor);
        }
        Ok(())
    }

    fn resolve_coordinate(
        &self,
        coordinate: dae::CoordinateView<'dae>,
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
            | dae::CoordinateView::Time
            | dae::CoordinateView::ClockInterval(_)
            | dae::CoordinateView::Condition(_)
            | dae::CoordinateView::Delay(_)
            | dae::CoordinateView::Previous(_)
            | dae::CoordinateView::Terminal(_)
            | dae::CoordinateView::Binder(_)
            | dae::CoordinateView::FunctionParameter(_) => return None,
        };
        self.unknown_map.get(&key).copied()
    }
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
    (values.len() == 1).then_some(values[0])
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

fn projection_span(error: &rumoca_eval_dae::ProjectionError) -> rumoca_core::Span {
    match error {
        rumoca_eval_dae::ProjectionError::ScalarOutOfBounds { span, .. }
        | rumoca_eval_dae::ProjectionError::DynamicSubscript { span }
        | rumoca_eval_dae::ProjectionError::IndexOutOfBounds { span, .. }
        | rumoca_eval_dae::ProjectionError::IntegerOverflow { span }
        | rumoca_eval_dae::ProjectionError::FunctionRecursion { span }
        | rumoca_eval_dae::ProjectionError::UnsupportedRecordOperation { span }
        | rumoca_eval_dae::ProjectionError::ExternalFunction { span, .. } => *span,
    }
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
        unknowns: (0..unknown_count).map(UnknownId::Solver).collect(),
        unknown_spans: Vec::new(),
        equation_refs: (0..n_eq).map(EquationRef).collect(),
        equation_spans: Vec::new(),
        structured_matching: Vec::new(),
    })
}
