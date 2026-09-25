//! Replay a formal system and append its proposed aggregate state coordinate map.

use super::super::formal_derivatives::SelectedCoordinate;
use super::super::variables::TargetVariable;
use super::*;
use rumoca_core::VarName;

type RebuiltCandidate = (dae::Dae, Vec<u32>, Option<u32>);

pub(in crate::dae_transform) fn rebuild_state_candidate(
    model: &dae::Dae,
    selected: &[SelectedCoordinate],
) -> Result<RebuiltCandidate, StructuralError> {
    let mut coordinates = Vec::new();
    let mut state = None;
    let rebuilt = model
        .inspect(|source| {
            let facts = DifferentiationFacts::collect(source);
            dae::Dae::construct(model.source_map().clone(), |target| {
                prepare_rebuild(
                    source,
                    target,
                    &facts,
                    RebuildRequest {
                        source_functions_only: true,
                        ..Default::default()
                    },
                    |prepared| {
                        let PreparedRebuild {
                            context,
                            target,
                            variables,
                            expressions,
                            quotients,
                            ..
                        } = prepared;
                        coordinates = variables
                            .iter()
                            .map(|v| v.identity.variable().index())
                            .collect();
                        define_variables(source, target, &expressions, variables)?;
                        rebuild_semantic_owners(
                            source,
                            target,
                            &expressions,
                            RebuiltOwnerIdentities {
                                variables,
                                domains: context.domains,
                                conditions: context.conditions,
                                clocks: context.clocks,
                            },
                            &[],
                            quotients,
                        )?;
                        state =
                            append_projection(source, target, variables, &expressions, selected)?;
                        Ok(())
                    },
                )
            })
        })
        .map_err(construction_failure)?;
    Ok((rebuilt, coordinates, state))
}

fn append_projection<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    variables: &[ReservedVariable<'target>],
    source_expressions: &[dae::ExprId<'target>],
    selected: &[SelectedCoordinate],
) -> Result<Option<u32>, dae::DaeConstructionError> {
    let Some(first) = selected.first() else {
        return Ok(None);
    };
    let first = source
        .variable(
            source
                .variable_id(first.value as usize)
                .expect("selected source value"),
        )
        .expect("selected source declaration");
    let at = dae::DaeProvenance::generated(
        dae::DaeGeneration::IndexReduction,
        first.declaration().span(),
    )?;
    let extent = u32::try_from(selected.len()).map_err(|_| invalid_projection(at))?;
    let value_type = target
        .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [extent]), at))?;
    let start = target.expressions(|expressions| {
        let values = selected
            .iter()
            .map(|coordinate| {
                project_start(source, expressions, source_expressions, *coordinate, at)
            })
            .collect::<Result<Vec<_>, _>>()?;
        expressions.at(at).array(values)
    })?;
    let state = target.variables(|variables| {
        variables.state(
            state_name(source),
            value_type,
            at,
            dae::VariableAttributes {
                start: Some(start),
                fixed: Some(vec![false]),
                origin: dae::VariableOrigin::Generated,
                ..Default::default()
            },
        )
    })?;
    let residuals = target.expressions(|expressions| {
        let mut values = Vec::with_capacity(selected.len());
        let mut derivatives = Vec::with_capacity(selected.len());
        for coordinate in selected {
            values.push(project(
                source,
                expressions,
                variables,
                coordinate.value,
                coordinate.scalar,
                at,
            )?);
            derivatives.push(project(
                source,
                expressions,
                variables,
                coordinate.successor,
                coordinate.scalar,
                at,
            )?);
        }
        let values = expressions.at(at).array(values)?;
        let derivatives = expressions.at(at).array(derivatives)?;
        let value = expressions
            .at(at)
            .coordinate(dae::CoordinateInput::State(state))?;
        let derivative = expressions
            .at(at)
            .coordinate(dae::CoordinateInput::Derivative(state))?;
        Ok([
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, value, values)?,
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, derivative, derivatives)?,
        ])
    })?;
    target.continuous(|equations| {
        for residual in residuals {
            equations.value_equation(at, residual)?;
        }
        Ok(())
    })?;
    Ok(Some(state.index()))
}

fn project<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::Expressions<'_, 'target>,
    variables: &[ReservedVariable<'target>],
    variable: u32,
    scalar: u32,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
    let TargetVariable::Algebraic(id) = variables[variable as usize].identity else {
        return Err(invalid_projection(at));
    };
    let value = target
        .at(at)
        .coordinate(dae::CoordinateInput::Algebraic(id))?;
    let declaration = source
        .variable(
            source
                .variable_id(variable as usize)
                .expect("selected coordinate exists"),
        )
        .expect("selected declaration exists");
    project_value(target, value, declaration.value_type(), scalar, at)
}

fn project_start<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::Expressions<'_, 'target>,
    source_expressions: &[dae::ExprId<'target>],
    coordinate: SelectedCoordinate,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
    let declaration = source
        .variable(
            source
                .variable_id(coordinate.value as usize)
                .expect("selected source value"),
        )
        .expect("selected source declaration");
    let Some(start) = declaration.start() else {
        return target.at(at).literal(dae::DaeLiteral::Real(0.0));
    };
    let value_type = source.expression(start).expect("source start").value_type();
    project_value(
        target,
        source_expressions[start.index() as usize],
        value_type,
        coordinate.scalar,
        at,
    )
}

fn project_value<'target>(
    target: &mut dae::Expressions<'_, 'target>,
    value: dae::ExprId<'target>,
    value_type: &dae::ValueType,
    scalar: u32,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
    let dimensions = value_type.dimensions();
    if dimensions.is_empty() {
        return Ok(value);
    }
    let indices = value_type
        .scalar_subscripts(scalar as usize)
        .ok_or_else(|| invalid_projection(at))?
        .into_iter()
        .map(|index| {
            Ok(dae::Subscript::Index {
                expression: target
                    .at(at)
                    .literal(dae::DaeLiteral::Integer(i64::from(index)))?,
                provenance: at,
            })
        })
        .collect::<Result<Vec<_>, dae::DaeConstructionError>>()?;
    target.at(at).index(value, indices)
}

fn state_name(source: dae::DaeView<'_>) -> VarName {
    let names = source
        .variables()
        .map(|(_, v)| v.name().clone())
        .collect::<std::collections::BTreeSet<_>>();
    let mut name = VarName::new("$state_coordinates");
    let mut suffix = 0;
    while names.contains(&name) {
        suffix += 1;
        name = VarName::new(format!("$state_coordinates.{suffix}"));
    }
    name
}

fn invalid_projection(at: dae::DaeProvenance) -> dae::DaeConstructionError {
    dae::DaeConstructionError::IncompleteDefinition {
        kind: "formal state coordinate projection",
        index: 0,
        span: at.span(),
    }
}
