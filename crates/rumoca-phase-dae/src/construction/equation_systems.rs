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
    let no_aggregate_connections = AggregateDiscreteConnections::default();
    let mut excluded_initial_rows = analysis.initialization_family_rows.clone();
    excluded_initial_rows.extend(&analysis.initial_discrete_equation_rows);
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
            multi_output: &analysis.multi_output_equations,
            roles: &analysis.roles,
            connection_ranks: &analysis.discrete_connection_ranks,
            aggregate_connections: &analysis.aggregate_discrete_connections,
            topology: &analysis.discrete_value_topology,
            clocked_owners: &analysis.clocked_equation_owners,
            clocks,
            semi_linear: &analysis.semi_linear_rules,
            initialization: false,
        },
    )?;
    lower_structured_equations(
        construction,
        discrete_values,
        coordinates,
        functions,
        StructuredEquationRows {
            equations: &flat.equations,
            families: &flat.structured_equations,
            excluded_families: &analysis.derived_parameter_families,
            environment: Some(StructuredEquationEnvironment {
                flat,
                roles: &analysis.roles,
                topology: &analysis.discrete_value_topology,
                connection_ranks: &analysis.discrete_connection_ranks,
                aggregate_connections: &analysis.aggregate_discrete_connections,
                clocked_owners: &analysis.clocked_equation_owners,
                clocks,
            }),
            initialization: false,
        },
    )?;
    lower_equations(
        construction,
        discrete_values,
        coordinates,
        functions,
        EquationRows {
            flat,
            equations: &flat.initial_equations,
            excluded: &excluded_initial_rows,
            records: &analysis.initial_record_equations,
            multi_output: &HashMap::new(),
            roles: &analysis.roles,
            connection_ranks: &analysis.discrete_connection_ranks,
            aggregate_connections: &no_aggregate_connections,
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
        discrete_values,
        coordinates,
        functions,
        StructuredEquationRows {
            equations: &flat.initial_equations,
            families: &flat.initial_structured_equations,
            excluded_families: &HashSet::new(),
            environment: None,
            initialization: true,
        },
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
