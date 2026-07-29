use super::*;

#[derive(Clone)]
pub(in crate::construction) struct DerivedParameterPlan {
    pub(in crate::construction) domain: StructuredIndexDomain,
    pub(in crate::construction) body: Expression,
    pub(in crate::construction) owner: Span,
}

pub(super) struct DerivedParameterAnalysis {
    pub(super) plans: HashMap<VarName, DerivedParameterPlan>,
    pub(super) families: HashSet<usize>,
    pub(super) rows: HashSet<usize>,
}

pub(super) fn analyze_derived_parameters(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<DerivedParameterAnalysis, ToDaeError> {
    let mut plans = HashMap::new();
    let mut families = HashSet::new();
    let mut rows = HashSet::new();
    for (family_index, family) in flat.structured_equations.iter().enumerate() {
        if family.interiors_materialized {
            continue;
        }
        let Some(template) = &family.template else {
            continue;
        };
        let assignments = template
            .body
            .iter()
            .filter_map(direct_assignment)
            .collect::<Vec<_>>();
        if assignments.is_empty() {
            continue;
        }
        if assignments.len() != template.body.len() {
            return Err(ToDaeError::unsupported_flat(
                "derived parameter family",
                "a non-materialized family cannot mix parameter assignments with runtime equations",
                family.span,
            ));
        }
        if template.scalar_view != rumoca_core::ComprehensionScalarView::BinderSubstitution {
            return Err(ToDaeError::unsupported_flat(
                "derived parameter family",
                "parameter assignment families require binder-substitution scalar semantics",
                family.span,
            ));
        }
        validate_family_shape(flat, family, &assignments)?;
        for (target, body) in assignments {
            if !matches!(roles.get(&target), Some(PlannedRole::Algebraic)) {
                return Err(ToDaeError::unsupported_flat(
                    "derived parameter family",
                    format!("non-materialized target `{target}` is not a local algebraic array"),
                    family.span,
                ));
            }
            if plans
                .insert(
                    target.clone(),
                    DerivedParameterPlan {
                        domain: family.domain.clone(),
                        body: body.clone(),
                        owner: family.span,
                    },
                )
                .is_some()
            {
                return Err(ToDaeError::unsupported_flat(
                    "derived parameter family",
                    format!("`{target}` has more than one compact parameter definition"),
                    family.span,
                ));
            }
        }
        families.insert(family_index);
        rows.extend(family_rows(family, flat.equations.len())?);
    }
    validate_dependencies(&plans, roles)?;
    Ok(DerivedParameterAnalysis {
        plans,
        families,
        rows,
    })
}

fn direct_assignment(expression: &Expression) -> Option<(VarName, &Expression)> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = expression
    else {
        return None;
    };
    let (target, subscripts) = indexed_reference(lhs)?;
    if subscripts.is_empty() {
        return None;
    }
    Some((target.clone(), rhs))
}

fn indexed_reference(expression: &Expression) -> Option<(&VarName, &[Subscript])> {
    match expression {
        Expression::VarRef {
            name, subscripts, ..
        } => Some((name.var_name(), subscripts)),
        Expression::Index {
            base, subscripts, ..
        } => {
            let Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } = base.as_ref()
            else {
                return None;
            };
            base_subscripts
                .is_empty()
                .then_some((name.var_name(), subscripts.as_slice()))
        }
        _ => None,
    }
}

fn validate_family_shape(
    flat: &flat::Model,
    family: &flat::StructuredEquationFamily,
    assignments: &[(VarName, &Expression)],
) -> Result<(), ToDaeError> {
    let extents = family.domain.extents().map_err(|error| {
        ToDaeError::unsupported_flat("derived parameter domain", error.to_string(), family.span)
    })?;
    let extents = extents
        .into_iter()
        .map(i64::try_from)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_| {
            ToDaeError::unsupported_flat(
                "derived parameter domain",
                "a compact domain extent exceeds the Flat shape representation",
                family.span,
            )
        })?;
    let binder_names = family
        .domain
        .binders
        .iter()
        .map(|binder| VarName::new(&binder.display_name))
        .collect::<Vec<_>>();
    for (target, _) in assignments {
        let variable = &flat.variables[target];
        if variable.dims != extents {
            return Err(ToDaeError::unsupported_flat(
                "derived parameter shape",
                format!(
                    "compact domain {:?} does not define the full shape {:?} of `{target}`",
                    extents, variable.dims
                ),
                family.span,
            ));
        }
        let (found_target, subscripts) =
            indexed_reference(template_lhs(family, target).ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "derived parameter target",
                    format!("the template has no indexed assignment for `{target}`"),
                    family.span,
                )
            })?)
            .expect("the selected template lhs is an indexed reference");
        if found_target != target
            || subscripts.len() != binder_names.len()
            || !subscripts
                .iter()
                .zip(&binder_names)
                .all(|(subscript, binder)| subscript_is_binder(subscript, binder))
        {
            return Err(ToDaeError::unsupported_flat(
                "derived parameter target",
                format!("`{target}` is not assigned once in row-major binder order"),
                family.span,
            ));
        }
    }
    Ok(())
}

fn template_lhs<'family>(
    family: &'family flat::StructuredEquationFamily,
    target: &VarName,
) -> Option<&'family Expression> {
    family.template.as_ref()?.body.iter().find_map(|body| {
        let Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            ..
        } = body
        else {
            return None;
        };
        indexed_reference(lhs)
            .filter(|(found, _)| *found == target)
            .map(|_| lhs.as_ref())
    })
}

fn subscript_is_binder(subscript: &Subscript, binder: &VarName) -> bool {
    matches!(
        subscript,
        Subscript::Expr { expr, .. }
            if matches!(
                expr.as_ref(),
                Expression::VarRef {
                    name,
                    subscripts,
                    ..
                } if name.var_name() == binder && subscripts.is_empty()
            )
    )
}

fn family_rows(
    family: &flat::StructuredEquationFamily,
    equation_count: usize,
) -> Result<std::ops::Range<usize>, ToDaeError> {
    let count = family
        .domain
        .scalar_count()
        .ok()
        .and_then(|points| points.checked_mul(family.equations_per_point))
        .ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "derived parameter family",
                "the compact family row count overflows",
                family.span,
            )
        })?;
    let end = family
        .first_equation_index
        .checked_add(count)
        .filter(|end| *end <= equation_count)
        .ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "derived parameter family",
                "the compact family rows are outside the continuous equation partition",
                family.span,
            )
        })?;
    Ok(family.first_equation_index..end)
}

fn validate_dependencies(
    plans: &HashMap<VarName, DerivedParameterPlan>,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<(), ToDaeError> {
    for (target, plan) in plans {
        let binders = plan
            .domain
            .binders
            .iter()
            .map(|binder| VarName::new(&binder.display_name))
            .collect::<HashSet<_>>();
        validate_expression_dependencies(&plan.body, target, &binders, plans, roles, plan.owner)?;
    }
    Ok(())
}

fn validate_expression_dependencies(
    expression: &Expression,
    target: &VarName,
    binders: &HashSet<VarName>,
    plans: &HashMap<VarName, DerivedParameterPlan>,
    roles: &HashMap<VarName, PlannedRole>,
    owner: Span,
) -> Result<(), ToDaeError> {
    if let Expression::VarRef { name, .. } = expression {
        let referenced = name.var_name();
        if binders.contains(referenced) {
            return Ok(());
        }
        let valid = referenced != target
            && (matches!(
                roles.get(referenced),
                Some(PlannedRole::Parameter | PlannedRole::Constant)
            ) || plans.contains_key(referenced));
        if !valid {
            return Err(ToDaeError::unsupported_flat(
                "derived parameter dependency",
                format!(
                    "`{target}` depends on runtime or cyclic coordinate `{referenced}` and cannot be computed at parameter-set time"
                ),
                owner,
            ));
        }
    }
    for child in expression_children(expression) {
        validate_expression_dependencies(child, target, binders, plans, roles, owner)?;
    }
    Ok(())
}
