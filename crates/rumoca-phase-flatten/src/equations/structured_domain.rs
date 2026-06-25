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
    let expected = iterations
        .iter()
        .map(|iteration| iteration.index_values.clone())
        .collect::<Vec<_>>();
    let actual = domain.index_tuples().map_err(|err| {
        FlattenError::unsupported_equation(
            format!("structured equation domain is invalid: {err}"),
            span,
        )
    })?;
    if actual != expected {
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
    let mut values = Vec::new();
    for iteration in iterations {
        let value = iteration.index_values[dimension];
        if !values.contains(&value) {
            values.push(value);
        }
    }
    let step = match values.as_slice() {
        [] => return None,
        [_] => 1,
        [first, second, rest @ ..] => {
            let step = second - first;
            if step == 0 || rest.iter().any(|value| (value - first) % step != 0) {
                return None;
            }
            step
        }
    };
    Some(rumoca_core::StructuredIndexBinder {
        id: dimension,
        display_name: index.ident.text.to_string(),
        lower: values[0],
        upper: *values.last()?,
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
    let mut binders = parent_domain.binders.clone();
    binders.extend(offset_child_binders(
        &first_family.domain.binders,
        parent_domain.binders.len(),
    ));
    let mut equation_counts = Vec::new();
    for _ in iterations {
        equation_counts.extend(first_family.equation_counts.iter().copied());
    }
    let lifted = flat::StructuredEquationFamily {
        domain: rumoca_core::StructuredIndexDomain { binders },
        first_equation_index: first_family.first_equation_index,
        equation_counts,
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
    equation_counts: Vec<usize>,
}

fn child_signature(family: &flat::StructuredEquationFamily) -> ChildFamilySignature {
    ChildFamilySignature {
        domain: family.domain.clone(),
        equation_counts: family.equation_counts.clone(),
    }
}

fn full_iteration_child_index(
    family: &flat::StructuredEquationFamily,
    iterations: &[SourceStructuredIteration],
    starts: &[usize],
) -> Option<usize> {
    let total_count = family.equation_counts.iter().sum::<usize>();
    starts
        .iter()
        .zip(iterations)
        .position(|(start, iteration)| {
            family.first_equation_index == *start && total_count == iteration.equation_count
        })
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
