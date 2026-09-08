//! Projection of exact causal output definitions into GALEC assignments.

use super::*;

/// Exact causal assignments appended after the clock schedule.
///
/// Construction selects whole-variable versus scalar definitions once. Actual
/// lowering then commits each selected root's statements and call actions in
/// one prepared emission group.
pub(super) struct CausalAssignmentsPlan<'dae> {
    assignments: Vec<PlannedCausalAssignment<'dae>>,
}

struct PlannedCausalAssignment<'dae> {
    variable: dae::VariableId<'dae>,
    class: VariableClass,
    definition: PlannedCausalDefinition<'dae>,
}

enum PlannedCausalDefinition<'dae> {
    Whole(dae::ExprId<'dae>),
    Scalars(Vec<dae::ExprId<'dae>>),
}

impl<'dae> CausalAssignmentsPlan<'dae> {
    pub(super) fn construct(
        definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
        classified: &[ClassifiedVariable<'dae>],
    ) -> Result<Self, GalecTargetError> {
        let mut assignments = Vec::new();
        // Complete scalar-definition sets cannot depend on an algebraic
        // variable (that is part of their structural admission proof), so
        // they are causal roots. Install them before the whole-definition
        // order: a whole definition may read one of these roots even though
        // it was not present when the whole-only topological order was built.
        for variable in classified.iter().filter(|variable| {
            matches!(variable.class, VariableClass::Local | VariableClass::Output)
                && definitions.definition_for_variable(variable.id).is_none()
                && definitions.fully_defines_variable(variable.id)
        }) {
            append_planned_assignment(&mut assignments, variable, definitions)?;
        }
        for algebraic in definitions.order() {
            let variable = dae::VariableId::from(*algebraic);
            let Some(classified) = classified.iter().find(|candidate| {
                candidate.id == variable
                    && matches!(
                        candidate.class,
                        VariableClass::Local | VariableClass::Output
                    )
            }) else {
                continue;
            };
            append_planned_assignment(&mut assignments, classified, definitions)?;
        }
        Ok(Self { assignments })
    }
}

fn append_planned_assignment<'dae>(
    assignments: &mut Vec<PlannedCausalAssignment<'dae>>,
    classified: &ClassifiedVariable<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
) -> Result<(), GalecTargetError> {
    if let Some(definition) = plan_definition(classified, definitions)? {
        assignments.push(PlannedCausalAssignment {
            variable: classified.id,
            class: classified.class,
            definition,
        });
    }
    Ok(())
}

fn plan_definition<'dae>(
    classified: &ClassifiedVariable<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
) -> Result<Option<PlannedCausalDefinition<'dae>>, GalecTargetError> {
    if let Some(definition) = definitions.definition_for_variable(classified.id) {
        return Ok(Some(PlannedCausalDefinition::Whole(definition)));
    }
    if !definitions.fully_defines_variable(classified.id) {
        return Ok(None);
    }
    let capacity_feature = match classified.class {
        VariableClass::Local => "local-definition-capacity",
        VariableClass::Output => "output-definition-capacity",
        _ => unreachable!("causal assignment plan admits only local/output variables"),
    };
    let mut expressions = Vec::with_capacity(classified.variable.scalar_count());
    for scalar in 0..classified.variable.scalar_count() {
        let scalar = u32::try_from(scalar).map_err(|_| {
            unsupported(
                capacity_feature,
                format!(
                    "{} `{}` has too many scalar definitions",
                    match classified.class {
                        VariableClass::Local => "local",
                        VariableClass::Output => "output",
                        _ => unreachable!("causal assignment class already checked"),
                    },
                    classified.variable.name()
                ),
                classified.variable.declaration().span(),
            )
        })?;
        expressions.push(
            definitions
                .scalar_definition_for_variable(classified.id, scalar)
                .expect("fully-defined causal variable has every scalar definition"),
        );
    }
    Ok(Some(PlannedCausalDefinition::Scalars(expressions)))
}

pub(super) struct PreparedCausalAssignments {
    pub(super) statements: Vec<gast::Spanned<gast::Statement>>,
    pub(super) locals: Vec<gast::VariableDeclaration>,
    pub(super) called_user_functions: HashSet<u32>,
    pub(super) call_actions: Vec<PreparedCallActions>,
}

pub(super) fn prepare_causal_assignments<'dae>(
    lowering: BlockLowering<'_, 'dae>,
    plan: &CausalAssignmentsPlan<'dae>,
    retained_calls: &mut RetainedCallResults,
) -> Result<PreparedCausalAssignments, GalecTargetError> {
    let BlockLowering {
        view,
        definitions,
        by_id,
        pre_names,
        arithmetic,
    } = lowering;
    let mut lowerer =
        ExpressionLowerer::with_do_step_effects(view, definitions, by_id, pre_names, arithmetic)
            .with_temporary_namespace(TemporaryNamespace::Causal);
    let mut statements = Vec::new();
    let mut call_actions = Vec::new();
    for (assignment_index, assignment) in plan.assignments.iter().enumerate() {
        let classified = by_id
            .get(&assignment.variable.index())
            .expect("planned causal variable remains classified");
        let targets = HashSet::from([assignment.variable.index()]);
        let prepared = lowerer.prepare_emission_group(
            EmissionRegion::CausalAssignment(assignment_index),
            &targets,
            CrossGroupCallRetention::Unguarded(&mut *retained_calls),
            |lowerer, statements| match assignment.class {
                VariableClass::Local => append_local_assignments(
                    classified,
                    &assignment.definition,
                    lowerer,
                    statements,
                ),
                VariableClass::Output => append_output_assignments(
                    classified,
                    &assignment.definition,
                    lowerer,
                    statements,
                ),
                _ => unreachable!("causal assignment plan carries only local/output variables"),
            },
        )?;
        prepared.commit_into(&mut statements, &mut call_actions);
    }
    Ok(PreparedCausalAssignments {
        statements,
        locals: lowerer.take_temporary_locals(),
        called_user_functions: lowerer.take_called_user_functions(),
        call_actions,
    })
}

fn append_local_assignments<'a, 'dae>(
    local: &ClassifiedVariable<'dae>,
    definition: &PlannedCausalDefinition<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    let span = local.variable.declaration().span();
    let mut assignments = Vec::new();
    for (scalar, indices) in row_major_indices(local.variable.value_type().dimensions())
        .into_iter()
        .enumerate()
    {
        let value = match definition {
            PlannedCausalDefinition::Whole(definition) => {
                lowerer.lower_element(*definition, &indices)?
            }
            PlannedCausalDefinition::Scalars(definitions) => lowerer.lower(
                *definitions
                    .get(scalar)
                    .expect("planned scalar definition matches target shape"),
            )?,
        };
        assignments.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: gast::Reference::Local(gast::RefPart {
                    name: local.name.clone(),
                    subscripts: indices
                        .iter()
                        .map(|index| gast::Expression::Integer(i64::from(*index)))
                        .collect(),
                    span,
                }),
                value: coerce(value, local.scalar_type, span)?,
            },
            span,
        ));
    }
    statements.extend(assignments);
    Ok(())
}

fn append_output_assignments<'a, 'dae>(
    output: &ClassifiedVariable<'dae>,
    definition: &PlannedCausalDefinition<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    let span = output.variable.declaration().span();
    let indices = row_major_indices(output.variable.value_type().dimensions());
    let mut output_statements = Vec::new();
    match definition {
        PlannedCausalDefinition::Whole(definition) => {
            for index in indices {
                let value = coerce(
                    lowerer.lower_element(*definition, &index)?,
                    output.scalar_type,
                    span,
                )?;
                push_assignment(output, index, value, span, &mut output_statements);
            }
        }
        PlannedCausalDefinition::Scalars(definitions) => {
            for (definition, index) in definitions.iter().zip(indices) {
                let value = coerce(lowerer.lower(*definition)?, output.scalar_type, span)?;
                push_assignment(output, index, value, span, &mut output_statements);
            }
        }
    }
    statements.extend(output_statements);
    Ok(())
}

fn push_assignment(
    output: &ClassifiedVariable<'_>,
    index: Vec<u32>,
    value: gast::Expression,
    span: Span,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) {
    statements.push(gast::Spanned::new(
        gast::Statement::Assignment {
            target: state_reference_indexed(output.name.clone(), &index, span),
            value,
        },
        span,
    ));
}
