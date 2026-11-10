use super::*;

#[cfg(test)]
mod selection_tests;
#[cfg(test)]
mod storage_tests;

/// The structured families of one Flat equation partition.
pub(super) struct PartitionFamilies<'flat> {
    pub(super) families: SelectedFamilies<'flat>,
    /// The rows of the owning Flat equation partition.
    pub(super) equations: &'flat [flat::Equation],
    /// Whether the partition is the initialization system. MLS 3.7 §8.6 solves
    /// initialization as one system of equations in which `pre(v)` of a
    /// discrete-time `v` is an unknown, so the Appendix B solved form of
    /// discrete-valued equations governs only the simulation partition.
    pub(super) initialization: bool,
    /// Families that are not owners of their rows: an equation between arrays
    /// of records is owned by its record equalities (MLS 3.7 §10.6.1), and a
    /// binder-selected family by its scalar rows ([`binder_selected_families`]).
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
        if !family.interiors_materialized() && family.template.is_none() {
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
        if materialized_discrete_real_family(&family, runtime_roles)
            || (!partition.initialization
                && materialized_discrete_value_rows(&family, partition.equations, runtime_roles))
        {
            // Its materialized rows are discrete definitions; each row keeps
            // its own ordinary owner.
            continue;
        }
        if let Some(template) = family.template {
            // A materialized element-assignment family is validated together
            // with all other element rows for its target. The aggregate pass
            // derives exact declared-shape coverage and overlap evidence; the
            // template is only the compact second view of those same rows.
            // Materialized initialization rows are ordinary initial equations,
            // validated row by row like every scalar initial equation.
            let materialized_initialization =
                partition.initialization && family.interiors_materialized();
            if !materialized_initialization
                && (!family.interiors_materialized()
                    || !structured_discrete_element_assignments(&template.body, runtime_roles))
            {
                structured_discrete_assignments(&template.body, runtime_roles, family.span)?;
            }
        }
        let represented_rows = match family.template {
            Some(template) => represented_template_rows(
                template,
                &family,
                domain_count,
                expression_roles,
                states,
                record_array_fields,
                model_values,
            )?,
            None => family
                .materialized_rows()
                .map(|rows| rows.len())
                .ok_or_else(|| {
                    ToDaeError::unsupported_flat(
                        "structured equation family",
                        "the materialized row range overflows usize",
                        family.span,
                    )
                })?,
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
    family.interiors_materialized()
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
    if family.interiors_materialized()
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
    if !family.interiors_materialized() {
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
    let scoped_values =
        super::super::affine_slices::scoped_shapes(model_values, &family.domain, family.span)?;
    for body in &template.body {
        validate_expression_scoped_with_record_array_fields(
            body,
            roles,
            states,
            &binders,
            record_array_fields,
            &scoped_values,
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

/// Materialized `for` families whose rows a domain binder selects a constant
/// branch of (MLS 3.7 §8.3.4, MSL PlugToPin: `plug.pin[j].i = if j == k then
/// -pin.i else 0`). Each point is an alias or connection balance or a constant
/// row fixed at translation; the structural phase reads such balances from
/// scalar rows only, so these rows keep their scalar owners, like a
/// whole-record equality family.
///
/// Matched: a component on one side of the residual and, on the other,
/// exactly `if j == e` (or `<>`) with `e` a binder-free parameter or constant
/// expression, between a literal and a binder-free arm. Any other binder
/// conditional (a boundary term in a sum, an arm reading the binder, a runtime
/// selector) keeps its compact owner (SPEC_0032).
pub(super) fn binder_selected_families(
    families: &[flat::StructuredEquationFamily],
    flat: &flat::Model,
) -> HashSet<usize> {
    let fixed = |expression: &Expression| fixed_at_translation(expression, flat);
    families
        .iter()
        .enumerate()
        .filter(|(_, family)| {
            let selection = PointSelection {
                domain: &family.domain,
                fixed: &fixed,
            };
            family.interiors_materialized()
                && family.template.as_ref().is_some_and(|template| {
                    template.scalar_view == rumoca_core::ComprehensionScalarView::BinderSubstitution
                        && template
                            .body
                            .iter()
                            .any(|body| selection.selects_alias_row(body))
                })
        })
        .map(|(index, _)| index)
        .collect()
}

/// Whether `expression` reads only parameters and constants.
fn fixed_at_translation(expression: &Expression, flat: &flat::Model) -> bool {
    let mut reads = Vec::new();
    expression.collect_var_refs(&mut reads);
    reads.iter().all(|name| {
        flat.variables.get(name).is_some_and(|variable| {
            matches!(
                variable.variability,
                rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
            )
        })
    })
}

/// One family's binders and the test that a selector is fixed at translation.
struct PointSelection<'a> {
    domain: &'a rumoca_core::StructuredIndexDomain,
    fixed: &'a dyn Fn(&Expression) -> bool,
}

impl PointSelection<'_> {
    /// Whether `residual` is `component - selection` (either order) with
    /// `selection` one fixed binder point between a literal and a binder-free
    /// arm.
    fn selects_alias_row(&self, residual: &Expression) -> bool {
        let Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            ..
        } = residual
        else {
            return false;
        };
        (is_component(lhs) && self.selects_constant_point(rhs))
            || (is_component(rhs) && self.selects_constant_point(lhs))
    }

    fn selects_constant_point(&self, expression: &Expression) -> bool {
        let Expression::If {
            branches,
            else_branch,
            ..
        } = expression
        else {
            return false;
        };
        let [(condition, selected)] = branches.as_slice() else {
            return false;
        };
        let fixed_pair = |literal: &Expression, other: &Expression| {
            super::guard_facts::numeric_literal(literal).is_some()
                && !reads_binder(other, self.domain)
        };
        self.selects_binder_point(condition)
            && (fixed_pair(selected, else_branch) || fixed_pair(else_branch, selected))
    }

    fn selects_binder_point(&self, condition: &Expression) -> bool {
        let Expression::Binary {
            op: rumoca_core::OpBinary::Eq | rumoca_core::OpBinary::Neq,
            lhs,
            rhs,
            ..
        } = condition
        else {
            return false;
        };
        let selector = |binder: &Expression, selector: &Expression| {
            is_binder(binder, self.domain)
                && !reads_binder(selector, self.domain)
                && (self.fixed)(selector)
        };
        selector(lhs, rhs) || selector(rhs, lhs)
    }
}

fn is_component(expression: &Expression) -> bool {
    matches!(
        expression,
        Expression::VarRef { .. } | Expression::FieldAccess { .. }
    )
}

fn is_binder(expression: &Expression, domain: &rumoca_core::StructuredIndexDomain) -> bool {
    matches!(
        expression,
        Expression::VarRef { name, subscripts, .. }
            if subscripts.is_empty()
                && domain
                    .binders
                    .iter()
                    .any(|binder| name.as_str() == binder.display_name)
    )
}

fn reads_binder(expression: &Expression, domain: &rumoca_core::StructuredIndexDomain) -> bool {
    expression.contains_subexpression(|expression| is_binder(expression, domain))
}

fn materialized_family_rows(
    family: &flat::StructuredEquationFamily,
) -> Option<std::ops::Range<usize>> {
    let template = family.template.as_ref()?;
    if !family.interiors_materialized() {
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
