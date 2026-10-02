use super::*;

/// The structured families of one Flat equation partition.
pub(super) struct PartitionFamilies<'flat> {
    pub(super) families: &'flat [flat::StructuredEquationFamily],
    /// The rows of the owning Flat equation partition.
    pub(super) equations: &'flat [flat::Equation],
    /// Whether the partition is the initialization system. MLS 3.7 §8.6 solves
    /// initialization as one system of equations in which `pre(v)` of a
    /// discrete-time `v` is an unknown, so the Appendix B solved form of
    /// discrete-valued equations governs only the simulation partition.
    pub(super) initialization: bool,
    /// Families that are not owners of their rows (MLS 3.7 §10.6.1: an
    /// equation between arrays of records is owned by its record equalities).
    pub(super) excluded: &'flat HashSet<usize>,
}

pub(super) fn validate_structured_families(
    partition: PartitionFamilies<'_>,
    runtime_roles: &HashMap<VarName, PlannedRole>,
    expression_roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    record_array_fields: &RecordArrayFieldPlans,
    model_values: &ShapeEnvironment,
) -> Result<HashSet<usize>, ToDaeError> {
    let mut covered = HashSet::new();
    let equation_count = partition.equations.len();
    for (index, family) in partition.families.iter().enumerate() {
        if partition.excluded.contains(&index) {
            continue;
        }
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
        if materialized_discrete_real_family(family, runtime_roles)
            || (!partition.initialization
                && materialized_discrete_value_rows(family, partition.equations, runtime_roles))
        {
            // Its materialized rows are discrete definitions; each row keeps
            // its own ordinary owner.
            continue;
        }
        if let Some(template) = &family.template {
            // A materialized element-assignment family is validated together
            // with all other element rows for its target. The aggregate pass
            // derives exact declared-shape coverage and overlap evidence; the
            // template is only the compact second view of those same rows.
            // Materialized initialization rows are ordinary initial equations,
            // validated row by row like every scalar initial equation.
            let materialized_initialization =
                partition.initialization && family.interiors_materialized;
            if !materialized_initialization
                && (!family.interiors_materialized
                    || !structured_discrete_element_assignments(&template.body, runtime_roles))
            {
                structured_discrete_assignments(&template.body, runtime_roles, family.span)?;
            }
        }
        let represented_rows = match &family.template {
            Some(template) => represented_template_rows(
                template,
                family,
                domain_count,
                expression_roles,
                states,
                record_array_fields,
                model_values,
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

/// Whether a materialized family defines discrete Real coordinates: every body
/// is `v - expr` over a whole discrete Real `v`.
///
/// MLS Appendix B keeps a discrete Real definition out of the continuous
/// residual system, so such a family is not a continuous structured owner; its
/// materialized rows are the discrete Real equations, each lowered with its own
/// activation (for a clocked row, its partition's clock).
pub(in crate::construction) fn materialized_discrete_real_family(
    family: &flat::StructuredEquationFamily,
    roles: &HashMap<VarName, PlannedRole>,
) -> bool {
    let Some(template) = family.template.as_ref() else {
        return false;
    };
    family.interiors_materialized
        && !template.body.is_empty()
        && template.body.iter().all(|body| {
            matches!(
                body,
                Expression::Binary {
                    op: OpBinary::Sub,
                    lhs,
                    ..
                } if matches!(
                    lhs.as_ref(),
                    Expression::VarRef { name, subscripts, .. }
                        if subscripts.is_empty()
                            && matches!(roles.get(name.var_name()), Some(PlannedRole::DiscreteReal))
                )
            )
        })
}

/// Whether a materialized family without a compact template defines a
/// discrete-valued coordinate: some row is an Appendix B discrete-valued
/// assignment.
///
/// Flatten keeps no template when the loop body selects its equations, for
/// example through a parameter-conditioned `if`, so the family is only the
/// grouping of its rows and owns no body of its own. A discrete-valued row is
/// not a residual, so such a grouping is dissolved: each row is lowered with
/// its own ordinary owner, exactly as the same rows are when they stand
/// outside a loop.
pub(in crate::construction) fn materialized_discrete_value_rows(
    family: &flat::StructuredEquationFamily,
    equations: &[flat::Equation],
    roles: &HashMap<VarName, PlannedRole>,
) -> bool {
    // A component-array slice equation `split.set = fill(v, n)` is one
    // materialized row `{split[1].set, ...} = e` whose element equations
    // define each element (MLS 3.7 §10.6.1); its template is that same row.
    if family.interiors_materialized
        && family.template.as_ref().is_some_and(|template| {
            !template.body.is_empty()
                && template
                    .body
                    .iter()
                    .all(|body| discrete_element_array_body(body, roles))
        })
    {
        return true;
    }
    let Some(rows) = family
        .materialized_rows()
        .and_then(|rows| equations.get(rows))
    else {
        return false;
    };
    if !family.interiors_materialized {
        return false;
    }
    let Some(template) = &family.template else {
        return rows.iter().any(|equation| {
            structured_discrete_element_assignments(std::slice::from_ref(&equation.residual), roles)
                || matches!(
                    discrete_value_assignment(&equation.residual, roles, equation.span),
                    Ok(Some(_))
                )
        });
    };
    // A template over the fields of a component array
    // (`inPort[i].occupied = ...` in `Modelica.StateGraph.Step`) names no
    // declared coordinate, so it classifies as no discrete owner, while each
    // materialized row is the discrete-value assignment of one element field.
    // Those rows are the definitions; the template is only their grouping.
    !rows.is_empty()
        && matches!(
            structured_discrete_assignments(&template.body, roles, family.span),
            Ok(None)
        )
        && !structured_discrete_element_assignments(&template.body, roles)
        && rows.iter().all(|equation| {
            matches!(
                discrete_value_assignment(&equation.residual, roles, equation.span),
                Ok(Some(_))
            )
        })
}

fn represented_template_rows(
    template: &rumoca_core::ComprehensionTemplate,
    family: &flat::StructuredEquationFamily,
    domain_count: usize,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    record_array_fields: &RecordArrayFieldPlans,
    model_values: &ShapeEnvironment,
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
        validate_expression_scoped_with_record_array_fields(
            body,
            roles,
            states,
            &binders,
            record_array_fields,
            model_values,
        )?;
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

/// Indices of the materialized families every represented row of which is a
/// whole-record equality with a record-equation plan.
///
/// MLS §10.6.1 makes an equation between arrays of records the element-wise
/// whole-record equality of its element pairs. Such a family is only a second
/// view of rows whose leaf owners the record-equation plans already prove from
/// exact record and field identities, so it is not a structured owner: each
/// row keeps its record-equation owner.
pub(super) fn record_equality_families(
    families: &[flat::StructuredEquationFamily],
    records: &HashMap<usize, RecordEquationPlan>,
) -> HashSet<usize> {
    families
        .iter()
        .enumerate()
        .filter(|(_, family)| {
            materialized_family_rows(family).is_some_and(|rows| {
                !rows.is_empty() && rows.into_iter().all(|row| records.contains_key(&row))
            })
        })
        .map(|(index, _)| index)
        .collect()
}

fn materialized_family_rows(
    family: &flat::StructuredEquationFamily,
) -> Option<std::ops::Range<usize>> {
    let template = family.template.as_ref()?;
    if !family.interiors_materialized {
        return None;
    }
    let count = match template.scalar_view {
        rumoca_core::ComprehensionScalarView::RowMajorProjection => family.equations_per_point,
        rumoca_core::ComprehensionScalarView::BinderSubstitution => family
            .domain
            .scalar_count()
            .ok()?
            .checked_mul(family.equations_per_point)?,
        rumoca_core::ComprehensionScalarView::BinderPrefixProjection { .. } => return None,
    };
    Some(family.first_equation_index..family.first_equation_index.checked_add(count)?)
}
