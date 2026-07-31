use super::*;

pub(super) fn lower_equation_systems<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    flat: &flat::Model,
    analysis: &Analysis,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    clocks: &LoweredClocks<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let mut excluded_equation_rows = analysis.continuous_family_rows.clone();
    excluded_equation_rows.extend(&analysis.clock_equation_rows);
    excluded_equation_rows.extend(&analysis.derived_parameter_rows);
    let no_clocked_owners = HashMap::new();
    let no_semi_linear_rules = SemiLinearRules::default();
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
            clocked_owners: &analysis.clocked_equation_owners,
            clocks,
            semi_linear: &analysis.semi_linear_rules,
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
            clocked_owners: &no_clocked_owners,
            clocks,
            // MLS §3.7.4.5's rules are stated for the model equations; an
            // initial equation is lowered exactly as written.
            semi_linear: &no_semi_linear_rules,
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

pub(super) fn lower_equation_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    owner_clock: Option<dae::PeriodicClockId<'dae>>,
    expression: &Expression,
    generated_root: Option<dae::DaeGeneration>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    match owner_clock {
        Some(clock) => lower_clocked_expression(
            construction,
            coordinates,
            functions,
            clock,
            expression,
            generated_root,
        ),
        None => lower_expression(
            construction,
            coordinates,
            functions,
            expression,
            generated_root,
        ),
    }
}
