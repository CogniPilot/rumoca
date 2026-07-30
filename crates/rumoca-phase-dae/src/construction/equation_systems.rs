use super::*;

pub(super) fn lower_equation_systems<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    flat: &flat::Model,
    analysis: &Analysis,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let mut excluded_equation_rows = analysis.continuous_family_rows.clone();
    excluded_equation_rows.extend(&analysis.clock_equation_rows);
    excluded_equation_rows.extend(&analysis.derived_parameter_rows);
    lower_equations(
        construction,
        discrete_values,
        coordinates,
        functions,
        EquationRows {
            flat,
            equations: &flat.equations,
            excluded: &excluded_equation_rows,
            records: &analysis.record_equations,
            roles: &analysis.roles,
            topology: &analysis.discrete_value_topology,
            initialization: false,
        },
    )?;
    lower_structured_equations(
        construction,
        coordinates,
        functions,
        &flat.equations,
        &flat.structured_equations,
        &analysis.derived_parameter_families,
        false,
    )?;
    lower_equations(
        construction,
        discrete_values,
        coordinates,
        functions,
        EquationRows {
            flat,
            equations: &flat.initial_equations,
            excluded: &analysis.initialization_family_rows,
            records: &analysis.initial_record_equations,
            roles: &analysis.roles,
            topology: &analysis.discrete_value_topology,
            initialization: true,
        },
    )?;
    lower_structured_equations(
        construction,
        coordinates,
        functions,
        &flat.initial_equations,
        &flat.initial_structured_equations,
        &HashSet::new(),
        true,
    )?;
    Ok(())
}
