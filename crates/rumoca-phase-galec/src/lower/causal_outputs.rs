//! Projection of exact causal output definitions into GALEC assignments.

use super::*;

pub(super) fn append_causal_output_assignments<'dae>(
    view: dae::DaeView<'dae>,
    classified: &[ClassifiedVariable<'dae>],
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
    let mut lowerer = ExpressionLowerer::new(view, by_id, pre_names);
    for output in classified
        .iter()
        .filter(|variable| variable.class == VariableClass::Output)
    {
        append_output_assignments(output, &definitions, &mut lowerer, statements)?;
    }
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
    if let Some(definition) = definitions.definition_for_variable(output.id) {
        for index in indices {
            let value = coerce(
                lowerer.lower_element(definition, &index)?,
                output.scalar_type,
                span,
            )?;
            push_assignment(output, index, value, span, statements);
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
            push_assignment(output, index, value, span, statements);
        }
    }
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
