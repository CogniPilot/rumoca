use super::*;

pub(super) struct RecordEquationLowering<'lower, 'dae> {
    pub(super) construction: &'lower mut dae::DaeConstruction<'dae>,
    pub(super) discrete_values: &'lower mut DiscreteValueStaging<'dae>,
    pub(super) coordinates: &'lower HashMap<VarName, Coordinate<'dae>>,
    pub(super) coordinate_instances: &'lower HashMap<rumoca_core::InstanceId, Coordinate<'dae>>,
    pub(super) functions: &'lower FunctionRegistry<'lower, 'dae>,
    pub(super) topology: &'lower DiscreteValueTopologyPlan,
}

pub(super) fn lower_record_equation<'dae>(
    lowering: &mut RecordEquationLowering<'_, 'dae>,
    equation: &flat::Equation,
    plan: &RecordEquationPlan,
    owner: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let aggregate = plan
        .aggregate
        .map(|side| aggregate_expression(equation, side))
        .transpose()?
        .map(|expression| {
            lower_expression(
                lowering.construction,
                lowering.coordinates,
                lowering.functions,
                expression,
                None,
            )
        })
        .transpose()?;
    let generated =
        dae::DaeProvenance::generated(dae::DaeGeneration::RecordEquationProjection, equation.span)?;
    lower_real_fields(lowering, plan, aggregate, owner, generated)?;
    lower_discrete_value_fields(lowering, plan, aggregate, owner, generated, equation.span)
}

fn lower_real_fields<'dae>(
    lowering: &mut RecordEquationLowering<'_, 'dae>,
    plan: &RecordEquationPlan,
    aggregate: Option<dae::ExprId<'dae>>,
    owner: dae::DaeProvenance,
    generated: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    for field in &plan.fields {
        let (target, value, discrete) = match field {
            RecordEquationFieldPlan::ContinuousRealResidual { target, value } => {
                (target, value, false)
            }
            RecordEquationFieldPlan::DiscreteRealResidual { target, value } => {
                (target, value, true)
            }
            RecordEquationFieldPlan::DiscreteValueDefinition { .. } => continue,
        };
        let target = coordinate_expression(lowering, target, generated)?;
        let value = field_value(lowering, value, aggregate, generated)?;
        let residual = lowering.construction.expressions(|expressions| {
            expressions
                .at(generated)
                .binary(dae::BinaryOperator::Subtract, target, value)
        })?;
        if discrete {
            lowering.construction.discrete(|system| {
                system.real_equation(owner, |equation| equation.residual(residual))
            })?;
        } else {
            lowering
                .construction
                .continuous(|system| system.value_equation(owner, residual))?;
        }
    }
    Ok(())
}

fn lower_discrete_value_fields<'dae>(
    lowering: &mut RecordEquationLowering<'_, 'dae>,
    plan: &RecordEquationPlan,
    aggregate: Option<dae::ExprId<'dae>>,
    owner: dae::DaeProvenance,
    generated: dae::DaeProvenance,
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let targets = plan
        .fields
        .iter()
        .filter_map(|field| match field {
            RecordEquationFieldPlan::DiscreteValueDefinition { target, .. } => {
                Some(target.name().clone())
            }
            RecordEquationFieldPlan::ContinuousRealResidual { .. }
            | RecordEquationFieldPlan::DiscreteRealResidual { .. } => None,
        })
        .collect::<Vec<_>>();
    if targets.is_empty() {
        return Ok(());
    }
    let first_target = plan
        .fields
        .iter()
        .find_map(|field| match field {
            RecordEquationFieldPlan::DiscreteValueDefinition { target, .. } => Some(target),
            RecordEquationFieldPlan::ContinuousRealResidual { .. }
            | RecordEquationFieldPlan::DiscreteRealResidual { .. } => None,
        })
        .ok_or(dae::DaeConstructionError::InvalidExpressionForm { span })?;
    let Coordinate::DiscreteValue(first_target_id) =
        checked_coordinate(lowering, first_target, generated)?
    else {
        return Err(dae::DaeConstructionError::InvalidVariableRole {
            name: first_target.name().clone(),
            span,
        });
    };
    let semantic_owner = lowering
        .discrete_values
        .owner(owner, targets, lowering.coordinates, lowering.topology)?
        .ok_or(dae::DaeConstructionError::InvalidDiscreteTopologyPlan {
            target: first_target_id.index(),
            span,
        })?;
    let action = dae::DaeProvenance::source(span)?;
    for field in &plan.fields {
        let RecordEquationFieldPlan::DiscreteValueDefinition { target, value, .. } = field else {
            continue;
        };
        let Coordinate::DiscreteValue(target_id) = checked_coordinate(lowering, target, action)?
        else {
            return Err(dae::DaeConstructionError::InvalidVariableRole {
                name: target.name().clone(),
                span,
            });
        };
        let value = field_value(lowering, value, aggregate, generated)?;
        lowering
            .discrete_values
            .always(semantic_owner, target_id, value, owner, action)?;
    }
    Ok(())
}

fn coordinate_expression<'dae>(
    lowering: &mut RecordEquationLowering<'_, 'dae>,
    coordinate: &RecordEquationCoordinate,
    generated: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let coordinate = checked_coordinate(lowering, coordinate, generated)?;
    lowering
        .construction
        .expressions(|expressions| expressions.at(generated).coordinate(coordinate.current()))
}

fn field_value<'dae>(
    lowering: &mut RecordEquationLowering<'_, 'dae>,
    value: &RecordEquationFieldValue,
    aggregate: Option<dae::ExprId<'dae>>,
    generated: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    match value {
        RecordEquationFieldValue::AggregateProjection(projection) => lower_record_projection(
            lowering.construction,
            aggregate.ok_or(dae::DaeConstructionError::InvalidExpressionForm {
                span: generated.span(),
            })?,
            projection,
            generated,
        ),
        RecordEquationFieldValue::Coordinate(source) => {
            coordinate_expression(lowering, source, generated)
        }
    }
}

fn checked_coordinate<'dae>(
    lowering: &RecordEquationLowering<'_, 'dae>,
    coordinate: &RecordEquationCoordinate,
    provenance: dae::DaeProvenance,
) -> Result<Coordinate<'dae>, dae::DaeConstructionError> {
    lowering
        .coordinate_instances
        .get(&coordinate.instance_id())
        .copied()
        .ok_or_else(|| dae::DaeConstructionError::InvalidVariableRole {
            name: coordinate.name().clone(),
            span: provenance.span(),
        })
}

fn aggregate_expression(
    equation: &flat::Equation,
    side: RecordEquationAggregateSide,
) -> Result<&Expression, dae::DaeConstructionError> {
    let Expression::Binary { lhs, rhs, .. } = &equation.residual else {
        return Err(dae::DaeConstructionError::InvalidExpressionForm {
            span: equation.span,
        });
    };
    Ok(match side {
        RecordEquationAggregateSide::Left => lhs,
        RecordEquationAggregateSide::Right => rhs,
    })
}

fn lower_record_projection<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    mut value: dae::ExprId<'dae>,
    projection: &[usize],
    generated: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    for ordinal in projection {
        value = construction
            .expressions(|expressions| expressions.at(generated).field(value, *ordinal))?;
    }
    Ok(value)
}
