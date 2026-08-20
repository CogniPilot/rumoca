use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

use crate::errors::FlattenError;

pub(super) struct SourceStructuredIteration {
    pub(super) index_values: Vec<i64>,
    pub(super) equation_count: usize,
}

pub(super) fn compact_domain_from_iterations(
    indices: &[ast::ForIndex],
    iterations: &[SourceStructuredIteration],
    span: rumoca_core::Span,
) -> Result<rumoca_core::StructuredIndexDomain, FlattenError> {
    let binders = indices
        .iter()
        .enumerate()
        .map(|(dimension, index)| compact_binder(index, iterations, dimension))
        .collect::<Option<Vec<_>>>()
        .ok_or_else(|| {
            FlattenError::unsupported_equation("structured equation domain is not compact", span)
        })?;
    let domain = rumoca_core::StructuredIndexDomain { binders };
    let actual = domain.index_tuple_iter().map_err(|err| {
        FlattenError::unsupported_equation(
            format!("structured equation domain is invalid: {err}"),
            span,
        )
    })?;
    if actual.len() != iterations.len()
        || actual
            .zip(iterations)
            .any(|(actual, iteration)| actual != iteration.index_values)
    {
        return Err(FlattenError::unsupported_equation(
            "structured equation domain order is not compact",
            span,
        ));
    }
    Ok(domain)
}

fn compact_binder(
    index: &ast::ForIndex,
    iterations: &[SourceStructuredIteration],
    dimension: usize,
) -> Option<rumoca_core::StructuredIndexBinder> {
    let lower = iterations.first()?.index_values[dimension];
    let upper = iterations.last()?.index_values[dimension];
    let step = iterations
        .iter()
        .map(|iteration| iteration.index_values[dimension])
        .find(|value| *value != lower)
        .map_or(1, |value| value - lower);
    Some(rumoca_core::StructuredIndexBinder {
        id: dimension,
        display_name: index.ident.text.to_string(),
        lower,
        upper,
        step,
    })
}

pub(super) fn lift_full_iteration_child_family(
    families: &mut Vec<flat::StructuredEquationFamily>,
    parent_domain: &rumoca_core::StructuredIndexDomain,
    iterations: &[SourceStructuredIteration],
    regular: Option<rumoca_core::RegularForFamily>,
    template: Option<rumoca_core::ComprehensionTemplate>,
) -> bool {
    let Some(group) = complete_liftable_child_group(families, iterations) else {
        return false;
    };
    let remove_indices = group.remove_indices;
    let first_family = families[remove_indices[0]].clone();
    let child_view = family_scalar_view(&first_family);
    let mut binders = parent_domain.binders.clone();
    binders.extend(offset_child_binders(
        &first_family.domain.binders,
        parent_domain.binders.len(),
    ));
    let template = template.map(|mut template| {
        template.scalar_view = match child_view {
            rumoca_core::ComprehensionScalarView::BinderSubstitution => template.scalar_view,
            rumoca_core::ComprehensionScalarView::RowMajorProjection => {
                rumoca_core::ComprehensionScalarView::BinderPrefixProjection {
                    binder_count: u32::try_from(parent_domain.binders.len())
                        .expect("structured domain rank fits u32"),
                }
            }
            rumoca_core::ComprehensionScalarView::BinderPrefixProjection { binder_count } => {
                rumoca_core::ComprehensionScalarView::BinderPrefixProjection {
                    binder_count: u32::try_from(parent_domain.binders.len())
                        .expect("structured domain rank fits u32")
                        .checked_add(binder_count)
                        .expect("combined structured domain rank fits u32"),
                }
            }
        };
        template
    });
    let lifted = flat::StructuredEquationFamily {
        domain: rumoca_core::StructuredIndexDomain { binders },
        first_equation_index: first_family.first_equation_index,
        equations_per_point: first_family.equations_per_point,
        span: first_family.span,
        origin: first_family.origin,
        regular,
        // The lifted (multi-binder) family's comprehension body is captured in
        // `expand_for_equation` from the original un-substituted loop, so every binder
        // stays symbolic; the per-child templates this lift consumes only describe the
        // inner binder and are discarded in favor of `template`.
        template,
        // The lifted 2-D family is cheapened iff the child (inner-binder) families
        // were: nested cheapening happens in the inner `expand_for_equation`, whose
        // families this lift consumes.
        interiors_materialized: first_family.interiors_materialized,
    };
    let insert_at = remove_indices[0];
    for index in remove_indices.iter().rev() {
        families.remove(*index);
    }
    families.insert(insert_at, lifted);
    true
}

struct LiftableChildGroup {
    remove_indices: Vec<usize>,
}

fn complete_liftable_child_group(
    families: &[flat::StructuredEquationFamily],
    iterations: &[SourceStructuredIteration],
) -> Option<LiftableChildGroup> {
    let starts = iteration_starts(iterations);
    let mut entries = vec![None; iterations.len()];
    let mut signature = None;
    for (family_index, family) in families.iter().enumerate() {
        let Some(iteration_index) = full_iteration_child_index(family, iterations, &starts) else {
            continue;
        };
        let candidate = child_signature(family);
        if signature.get_or_insert_with(|| candidate.clone()) != &candidate {
            continue;
        }
        entries[iteration_index] = Some(family_index);
    }
    entries
        .into_iter()
        .collect::<Option<Vec<_>>>()
        .map(|remove_indices| LiftableChildGroup { remove_indices })
}

#[derive(Clone, PartialEq)]
struct ChildFamilySignature {
    domain: rumoca_core::StructuredIndexDomain,
    equations_per_point: usize,
    scalar_view: rumoca_core::ComprehensionScalarView,
}

fn child_signature(family: &flat::StructuredEquationFamily) -> ChildFamilySignature {
    ChildFamilySignature {
        domain: family.domain.clone(),
        equations_per_point: family.equations_per_point,
        scalar_view: family_scalar_view(family),
    }
}

fn full_iteration_child_index(
    family: &flat::StructuredEquationFamily,
    iterations: &[SourceStructuredIteration],
    starts: &[usize],
) -> Option<usize> {
    let extents = family.domain.extents().ok()?;
    let projected = match family_scalar_view(family) {
        rumoca_core::ComprehensionScalarView::BinderSubstitution => extents.len(),
        rumoca_core::ComprehensionScalarView::RowMajorProjection => 0,
        rumoca_core::ComprehensionScalarView::BinderPrefixProjection { binder_count } => {
            usize::try_from(binder_count).ok()?
        }
    };
    let total_count = extents
        .get(..projected)?
        .iter()
        .try_fold(family.equations_per_point, |count, extent| {
            count.checked_mul(*extent)
        })?;
    starts
        .iter()
        .zip(iterations)
        .position(|(start, iteration)| {
            family.first_equation_index == *start && total_count == iteration.equation_count
        })
}

fn family_scalar_view(
    family: &flat::StructuredEquationFamily,
) -> rumoca_core::ComprehensionScalarView {
    family.template.as_ref().map_or(
        rumoca_core::ComprehensionScalarView::RowMajorProjection,
        |template| template.scalar_view,
    )
}

fn iteration_starts(iterations: &[SourceStructuredIteration]) -> Vec<usize> {
    let mut next = 0usize;
    let mut starts = Vec::with_capacity(iterations.len());
    for iteration in iterations {
        starts.push(next);
        next += iteration.equation_count;
    }
    starts
}

fn offset_child_binders(
    binders: &[rumoca_core::StructuredIndexBinder],
    offset: usize,
) -> Vec<rumoca_core::StructuredIndexBinder> {
    binders
        .iter()
        .enumerate()
        .map(|(dimension, binder)| {
            let mut binder = binder.clone();
            binder.id = offset + dimension;
            binder
        })
        .collect()
}
