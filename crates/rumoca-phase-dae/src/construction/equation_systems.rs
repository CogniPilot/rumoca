use super::*;

/// The read-only inputs both equation phases lower against.
///
/// Five independent borrows: none is derived from another, so bundling them
/// cannot make two disagreeing values representable. `coordinates.by_instance()`
/// is taken at each use rather than stored, for the same reason.
pub(super) struct EquationSystemInputs<'borrow, 'scope, 'dae> {
    flat: &'borrow flat::Model,
    analysis: &'borrow Analysis<'scope>,
    coordinates: &'borrow ModelCoordinates<'dae>,
    functions: &'borrow FunctionRegistry<'scope, 'dae>,
    clocks: &'borrow LoweredClocks<'dae>,
}

impl<'borrow, 'scope, 'dae> EquationSystemInputs<'borrow, 'scope, 'dae> {
    pub(super) fn new(
        flat: &'borrow flat::Model,
        analysis: &'borrow Analysis<'scope>,
        coordinates: &'borrow ModelCoordinates<'dae>,
        functions: &'borrow FunctionRegistry<'scope, 'dae>,
        clocks: &'borrow LoweredClocks<'dae>,
    ) -> Self {
        Self {
            flat,
            analysis,
            coordinates,
            functions,
            clocks,
        }
    }
}

/// Lower the model equations, then the initial equations.
///
/// The two phases are separate functions because MLS 3.7.4.5's rules are stated
/// for the model equations: an initial equation is lowered exactly as written,
/// with no clocked owners, no semi-linear rules and no record equations. Keeping
/// that difference in the signatures makes it legible instead of leaving it as
/// three empty maps passed mid-function.
pub(super) fn lower_equation_systems<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    inputs: &EquationSystemInputs<'_, '_, 'dae>,
    model_equations: ModelEquationSequence<'_>,
) -> Result<(), dae::DaeConstructionError> {
    lower_model_equation_systems(construction, discrete_values, inputs, model_equations)?;
    lower_initial_equation_systems(construction, discrete_values, inputs)
}

fn lower_model_equation_systems<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    inputs: &EquationSystemInputs<'_, '_, 'dae>,
    model_equations: ModelEquationSequence<'_>,
) -> Result<(), dae::DaeConstructionError> {
    let EquationSystemInputs {
        flat,
        analysis,
        coordinates,
        functions,
        clocks,
    } = *inputs;
    let structured_equation_rows = analysis
        .structured_equation_owners
        .continuous()
        .structured_row_indices()
        .collect::<HashSet<_>>();
    let structured_family_rows = analysis
        .structured_equation_owners
        .continuous()
        .owners()
        .iter()
        .filter_map(|owner| match owner {
            flat::CheckedEquationOwner::Template(family) => {
                Some((family.family_index(), family.rows()))
            }
            flat::CheckedEquationOwner::Standalone(_) => None,
        })
        .collect::<HashMap<_, _>>();
    let mut excluded_equation_rows = structured_equation_rows.clone();
    excluded_equation_rows.extend(&analysis.clock_equation_rows);
    excluded_equation_rows.extend(&analysis.derived_parameter_rows);
    let mut deferred_equations = lower_equations(
        construction,
        discrete_values,
        coordinates,
        coordinates.by_instance(),
        functions,
        EquationRows {
            source: EquationSource::Model(model_equations),
            excluded: &excluded_equation_rows,
            records: &analysis.record_equations,
            multi_output: &analysis.multi_output_equations,
            topology: &analysis.discrete_value_topology,
            clocked_owners: &analysis.clocked_equation_owners,
            clocks,
            semi_linear: &analysis.semi_linear_rules,
        },
    )?;
    deferred_equations.consume_non_structured_claims(
        &structured_equation_rows,
        analysis.clock_equation_rows.iter().copied(),
    );
    deferred_equations.consume_non_structured_claims(
        &HashSet::new(),
        analysis.derived_parameter_rows.iter().copied(),
    );
    lower_structured_equations(
        construction,
        discrete_values,
        coordinates,
        functions,
        StructuredEquationRows {
            equations: &flat.equations,
            families: &flat.structured_equations,
            family_rows: &structured_family_rows,
            excluded_families: &analysis.derived_parameter_families,
            environment: Some(StructuredEquationEnvironment {
                roles: &analysis.roles,
                topology: &analysis.discrete_value_topology,
                clocked_owners: &analysis.clocked_equation_owners,
                clocks,
            }),
            initialization: false,
        },
        Some(&mut deferred_equations),
    )?;
    deferred_equations.finish()?;
    Ok(())
}

/// Lower the initial equations exactly as written.
///
/// No clocked owners, no semi-linear rules and no record equations: MLS 3.7.4.5
/// states those rules for the model equations only. The three empty inputs are
/// constructed here so their emptiness is a property of this phase rather than a
/// detail buried in a shared body.
fn lower_initial_equation_systems<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    inputs: &EquationSystemInputs<'_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let EquationSystemInputs {
        flat,
        analysis,
        coordinates,
        functions,
        clocks,
    } = *inputs;
    let no_clocked_owners = HashMap::new();
    let no_semi_linear_rules = SemiLinearRules::default();
    let no_record_equations = HashMap::new();
    let mut excluded_initial_rows = analysis
        .structured_equation_owners
        .initialization()
        .structured_row_indices()
        .collect::<HashSet<_>>();
    let initial_structured_family_rows = analysis
        .structured_equation_owners
        .initialization()
        .owners()
        .iter()
        .filter_map(|owner| match owner {
            flat::CheckedEquationOwner::Template(family) => {
                Some((family.family_index(), family.rows()))
            }
            flat::CheckedEquationOwner::Standalone(_) => None,
        })
        .collect::<HashMap<_, _>>();
    excluded_initial_rows.extend(&analysis.initial_discrete_equation_rows);
    lower_equations(
        construction,
        discrete_values,
        coordinates,
        coordinates.by_instance(),
        functions,
        EquationRows {
            source: EquationSource::Initialization(&flat.initial_equations),
            excluded: &excluded_initial_rows,
            records: &no_record_equations,
            multi_output: &analysis.initial_multi_output_equations,
            topology: &analysis.discrete_value_topology,
            clocked_owners: &no_clocked_owners,
            clocks,
            // MLS §3.7.4.5's rules are stated for the model equations; an
            // initial equation is lowered exactly as written.
            semi_linear: &no_semi_linear_rules,
        },
    )?
    .finish()?;
    lower_structured_equations(
        construction,
        discrete_values,
        coordinates,
        functions,
        StructuredEquationRows {
            equations: &flat.initial_equations,
            families: &flat.initial_structured_equations,
            family_rows: &initial_structured_family_rows,
            excluded_families: &HashSet::new(),
            environment: None,
            initialization: true,
        },
        None,
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
    let normalized = normalize_conditional_residual(expression);
    let expression = normalized.as_ref().unwrap_or(expression);
    let generated_root = normalized
        .is_some()
        .then_some(dae::DaeGeneration::ConditionLowering)
        .or(generated_root);
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
