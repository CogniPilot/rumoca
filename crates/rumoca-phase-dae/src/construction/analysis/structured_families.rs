use super::*;

pub(super) fn validate_structured_families(
    families: &[flat::StructuredEquationFamily],
    equation_count: usize,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    record_array_fields: &HashMap<Span, RecordArrayFieldPlan>,
) -> Result<HashSet<usize>, ToDaeError> {
    let mut covered = HashSet::new();
    for family in families {
        require_span(family.span, "structured equation family")?;
        let domain_count = family.domain.scalar_count().map_err(|error| {
            ToDaeError::unsupported_flat(
                "structured equation domain",
                error.to_string(),
                family.span,
            )
        })?;
        if family.equations_per_point == 0 {
            return Err(ToDaeError::unsupported_flat(
                "structured equation family",
                "a family must own at least one residual per domain point",
                family.span,
            ));
        }
        if !family.interiors_materialized && family.template.is_none() {
            return Err(ToDaeError::unsupported_flat(
                "structured equation family",
                "a non-materialized family requires its canonical comprehension template",
                family.span,
            ));
        }
        if domain_count == 0 && family.template.is_none() {
            return Err(ToDaeError::unsupported_flat(
                "empty structured equation family",
                "an empty domain requires its canonical comprehension body because no materialized row can define the body",
                family.span,
            ));
        }
        let represented_rows = match &family.template {
            Some(template) => represented_template_rows(
                template,
                family,
                domain_count,
                roles,
                states,
                record_array_fields,
            )?,
            None => checked_materialized_rows(family, domain_count)?,
        };
        let end = family
            .first_equation_index
            .checked_add(represented_rows)
            .filter(|end| *end <= equation_count)
            .ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "structured equation family",
                    "the family row range is outside the owning Flat equation partition",
                    family.span,
                )
            })?;
        for index in family.first_equation_index..end {
            if !covered.insert(index) {
                return Err(ToDaeError::unsupported_flat(
                    "structured equation family",
                    "two semantic owners overlap the same Flat equation row",
                    family.span,
                ));
            }
        }
    }
    Ok(covered)
}

fn represented_template_rows(
    template: &rumoca_core::ComprehensionTemplate,
    family: &flat::StructuredEquationFamily,
    domain_count: usize,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    record_array_fields: &HashMap<Span, RecordArrayFieldPlan>,
) -> Result<usize, ToDaeError> {
    if template.body.len() != family.equations_per_point {
        return Err(ToDaeError::unsupported_flat(
            "structured equation family",
            "the comprehension body count differs from equations_per_point",
            family.span,
        ));
    }
    let binders = family
        .domain
        .binders
        .iter()
        .map(|binder| VarName::new(&binder.display_name))
        .collect::<HashSet<_>>();
    for body in &template.body {
        let validation_expression = expression_for_validation(body, record_array_fields);
        validate_expression_scoped(&validation_expression, roles, states, &binders)?;
    }
    match template.scalar_view {
        rumoca_core::ComprehensionScalarView::BinderSubstitution => {
            checked_materialized_rows(family, domain_count)
        }
        rumoca_core::ComprehensionScalarView::RowMajorProjection => Ok(family.equations_per_point),
        rumoca_core::ComprehensionScalarView::BinderPrefixProjection { binder_count } => {
            let extents = family.domain.extents().map_err(|error| {
                ToDaeError::unsupported_flat(
                    "structured equation domain",
                    error.to_string(),
                    family.span,
                )
            })?;
            extents
                .get(..usize::try_from(binder_count).unwrap_or(usize::MAX))
                .and_then(|prefix| {
                    prefix
                        .iter()
                        .try_fold(family.equations_per_point, |count, extent| {
                            count.checked_mul(*extent)
                        })
                })
                .ok_or_else(|| {
                    ToDaeError::unsupported_flat(
                        "structured equation family",
                        "the binder-prefix projection is outside its domain",
                        family.span,
                    )
                })
        }
    }
}

fn checked_materialized_rows(
    family: &flat::StructuredEquationFamily,
    domain_count: usize,
) -> Result<usize, ToDaeError> {
    domain_count
        .checked_mul(family.equations_per_point)
        .ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "structured equation family",
                "the materialized row range overflows usize",
                family.span,
            )
        })
}
