use super::*;

pub(in crate::construction) type StructuredEquationOwners<'flat> =
    flat::CheckedStructuredEquationOwners<'flat>;

pub(super) fn analyze_structured_equation_owners(
    flat: &flat::Model,
) -> Result<StructuredEquationOwners<'_>, ToDaeError> {
    flat.structured_equation_owners()
        .map_err(|error| structured_owner_error(flat, error))
}

fn structured_owner_error(
    flat: &flat::Model,
    error: flat::StructuredEquationOwnerError,
) -> ToDaeError {
    let partition = match error.partition() {
        flat::EquationPartitionKind::Continuous => (&flat.equations, &flat.structured_equations),
        flat::EquationPartitionKind::Initialization => {
            (&flat.initial_equations, &flat.initial_structured_equations)
        }
    };
    let span = match error.location() {
        flat::StructuredOwnerErrorLocation::Row(row) => {
            partition.0.get(row).map(|equation| equation.span)
        }
        flat::StructuredOwnerErrorLocation::Family(family) => {
            partition.1.get(family).map(|family| family.span)
        }
    };
    match span {
        Some(span) => {
            ToDaeError::unsupported_flat("structured equation ownership", error.to_string(), span)
        }
        None => ToDaeError::internal(error.to_string()),
    }
}

pub(super) fn validate_structured_templates(
    owners: &StructuredEquationOwners<'_>,
    runtime_roles: &HashMap<VarName, PlannedRole>,
    expression_roles: &HashMap<VarName, PlannedRole>,
    record_array_fields: &RecordArrayFieldPlans,
    model_values: &ShapeEnvironment,
) -> Result<(), ToDaeError> {
    for owner in owners
        .continuous()
        .owners()
        .iter()
        .chain(owners.initialization().owners())
    {
        let flat::CheckedEquationOwner::Template(owner) = owner else {
            continue;
        };
        let family = owner.family();
        let template = owner.template();
        require_span(family.span, "structured equation family")?;
        if !family.interiors_materialized
            || !structured_discrete_element_assignments(&template.body, runtime_roles)
        {
            structured_discrete_assignments(&template.body, runtime_roles, family.span)?;
        }
        validate_template_expressions(
            template,
            family,
            expression_roles,
            record_array_fields,
            model_values,
        )?;
    }
    Ok(())
}

fn validate_template_expressions(
    template: &rumoca_core::ComprehensionTemplate,
    family: &flat::StructuredEquationFamily,
    roles: &HashMap<VarName, PlannedRole>,
    record_array_fields: &RecordArrayFieldPlans,
    model_values: &ShapeEnvironment,
) -> Result<(), ToDaeError> {
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
            &binders,
            record_array_fields,
            model_values,
        )?;
    }
    Ok(())
}
