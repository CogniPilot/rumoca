//! Projection of exact causal output definitions into GALEC assignments.

use super::*;

pub(super) fn append_causal_assignments<'dae>(
    view: dae::DaeView<'dae>,
    classified: &[ClassifiedVariable<'dae>],
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
    method_locals: &mut Vec<gast::VariableDeclaration>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<HashSet<u32>, GalecTargetError> {
    let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
    let mut lowerer = ExpressionLowerer::with_do_step_effects(view, by_id, pre_names)
        .with_temporary_namespace("causal");
    for algebraic in definitions.order() {
        let id = dae::VariableId::from(*algebraic);
        let Some(local) = classified
            .iter()
            .find(|variable| variable.id == id && variable.class == VariableClass::Local)
        else {
            continue;
        };
        append_local_assignments(local, &definitions, &mut lowerer, statements)?;
    }
    for output in classified
        .iter()
        .filter(|variable| variable.class == VariableClass::Output)
    {
        append_output_assignments(output, &definitions, &mut lowerer, statements)?;
    }
    method_locals.extend(lowerer.take_temporary_locals());
    Ok(lowerer.take_called_user_functions())
}

fn append_local_assignments<'a, 'dae>(
    local: &ClassifiedVariable<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    let span = local.variable.declaration().span();
    let mut assignments = Vec::new();
    for (scalar, indices) in row_major_indices(local.variable.value_type().dimensions())
        .into_iter()
        .enumerate()
    {
        let value = if let Some(definition) = definitions.definition_for_variable(local.id) {
            lowerer.lower_element(definition, &indices)?
        } else {
            let scalar = u32::try_from(scalar).map_err(|_| {
                unsupported(
                    "local-definition-capacity",
                    format!(
                        "local `{}` has too many scalar definitions",
                        local.variable.name()
                    ),
                    span,
                )
            })?;
            let definition = definitions
                .scalar_definition_for_variable(local.id, scalar)
                .expect("causal local has a checked scalar definition");
            lowerer.lower(definition)?
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
    statements.extend(lowerer.take_prefix_statements());
    statements.extend(assignments);
    Ok(())
}

fn append_output_assignments<'a, 'dae>(
    output: &ClassifiedVariable<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    let span = output.variable.declaration().span();
    let indices = row_major_indices(output.variable.value_type().dimensions());
    let mut output_statements = Vec::new();
    if let Some(definition) = definitions.definition_for_variable(output.id) {
        for index in indices {
            let value = coerce(
                lowerer.lower_element(definition, &index)?,
                output.scalar_type,
                span,
            )?;
            push_assignment(output, index, value, span, &mut output_statements);
        }
    } else if definitions.fully_defines_variable(output.id) {
        for (scalar, index) in indices.into_iter().enumerate() {
            let scalar = u32::try_from(scalar).map_err(|_| {
                unsupported(
                    "output-definition-capacity",
                    format!(
                        "output `{}` has too many scalar definitions",
                        output.variable.name()
                    ),
                    span,
                )
            })?;
            let definition = definitions
                .scalar_definition_for_variable(output.id, scalar)
                .expect("fully-defined output has one checked definition per scalar");
            let value = coerce(lowerer.lower(definition)?, output.scalar_type, span)?;
            push_assignment(output, index, value, span, &mut output_statements);
        }
    }
    statements.extend(lowerer.take_prefix_statements());
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
