use super::*;

/// The discrete owners a model multi-output equation may define.
pub(super) struct MultiOutputDiscreteOwners<'scope, 'dae> {
    pub(super) discrete_values: &'scope mut DiscreteValueStaging<'dae>,
    pub(super) topology: &'scope DiscreteValueTopologyPlan,
    /// The clock of the partition the equation belongs to, when it is clocked.
    pub(super) owner_clock: Option<dae::ClockId<'dae>>,
}

/// The receiving tuple and the called function of one multi-result equation.
pub(super) struct MultiOutputSource<'flat> {
    pub(super) receivers: &'flat [Expression],
    pub(super) name: &'flat rumoca_core::Reference,
    pub(super) arguments: &'flat [Expression],
    pub(super) provenance: dae::DaeProvenance,
}

/// The receiving tuple and call of `(a, b, ...) = f(...)`; any other residual
/// is not a multi-result equation.
fn multi_output_source(
    equation: &flat::Equation,
) -> Result<MultiOutputSource<'_>, dae::DaeConstructionError> {
    let invalid = || dae::DaeConstructionError::InvalidExpressionForm {
        span: equation.span,
    };
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &equation.residual
    else {
        return Err(invalid());
    };
    let (
        Expression::Tuple { elements, .. },
        Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            span,
        },
    ) = (lhs.as_ref(), rhs.as_ref())
    else {
        return Err(invalid());
    };
    Ok(MultiOutputSource {
        receivers: elements,
        name,
        arguments: args,
        provenance: dae::DaeProvenance::source(*span)?,
    })
}

/// One retained receiver of a multi-result equation: the coordinate it
/// defines, the result ordinal it reads and the field projection of that
/// result (empty for a variable that receives the whole result). A whole
/// record receives one result as one receiver per leaf coordinate.
struct Receiver<'flat> {
    ordinal: usize,
    target: &'flat VarName,
    projection: &'flat [RecordProjectionStep],
    /// The written receiving variable, for a variable that receives a result.
    written: Option<&'flat Expression>,
}

fn plan_receivers<'flat>(
    plan: &'flat MultiOutputEquationPlan,
    source: &MultiOutputSource<'flat>,
) -> Vec<Receiver<'flat>> {
    let variables = plan
        .outputs
        .iter()
        .enumerate()
        .filter_map(|(ordinal, target)| {
            target.as_ref().map(|target| Receiver {
                ordinal,
                target,
                projection: &[],
                written: Some(&source.receivers[ordinal]),
            })
        });
    let leaves = plan.records.iter().flat_map(|record| {
        record
            .plan
            .fields
            .iter()
            .filter_map(move |field| match &field.value {
                RecordEquationFieldValue::AggregateProjection(projection) => Some(Receiver {
                    ordinal: record.ordinal,
                    target: &field.target,
                    projection,
                    written: None,
                }),
                RecordEquationFieldValue::Coordinate(_) => None,
            })
    });
    let mut receivers = variables.chain(leaves).collect::<Vec<_>>();
    receivers.sort_by_key(|receiver| receiver.ordinal);
    receivers
}

/// The owner that defines the discrete-valued receivers of one equation.
struct DiscreteValueDefinition<'scope, 'dae> {
    staging: &'scope mut DiscreteValueStaging<'dae>,
    owner: DiscreteValueOwnerHandle,
}

pub(super) fn lower_multi_output_equation<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    equation: &flat::Equation,
    plan: &MultiOutputEquationPlan,
    owner: dae::DaeProvenance,
    discrete: Option<MultiOutputDiscreteOwners<'_, 'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    lower_multi_output_source(
        construction,
        coordinates,
        functions,
        multi_output_source(equation)?,
        plan,
        owner,
        discrete,
    )
}

pub(super) fn lower_multi_output_source<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    source: MultiOutputSource<'_>,
    plan: &MultiOutputEquationPlan,
    owner: dae::DaeProvenance,
    discrete: Option<MultiOutputDiscreteOwners<'_, 'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    let symbols = LoweringSymbols {
        coordinates,
        functions,
        shapes: functions.shapes.model_values(),
        function_body: None,
        values: None,
        owner_clock: discrete.as_ref().and_then(|discrete| discrete.owner_clock),
    };
    let receivers = plan_receivers(plan, &source);
    let generated =
        dae::DaeProvenance::generated(dae::DaeGeneration::RecordEquationProjection, owner.span())?;
    let values = receiver_values(construction, symbols, &source, &receivers, generated)?;
    let initialization = discrete.is_none();
    let selected = receivers
        .iter()
        .map(|receiver| (receiver.ordinal, receiver.target))
        .collect::<Vec<_>>();
    let mut definition = discrete_value_definition(coordinates, &selected, discrete, owner)?;
    for (receiver, value) in receivers.iter().zip(values) {
        match (coordinates[receiver.target], definition.as_mut()) {
            (Coordinate::DiscreteValue(target), Some(definition)) => {
                definition.staging.always(
                    definition.owner,
                    target,
                    value,
                    owner,
                    source.provenance,
                )?;
            }
            (coordinate, _) => {
                let lhs = match receiver.written {
                    Some(written) => {
                        lower_expression(construction, coordinates, functions, written, None)?
                    }
                    None => construction.expressions(|expressions| {
                        expressions.at(generated).coordinate(coordinate.current())
                    })?,
                };
                define_residual_receiver(
                    construction,
                    (lhs, value),
                    coordinate,
                    (owner, initialization),
                )?;
            }
        }
    }
    Ok(())
}

/// The value every receiver reads, in receiver order: its result ordinal's
/// call result, projected onto its field.
///
/// Every receiver retains its projection of the same source call occurrence.
fn receiver_values<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    source: &MultiOutputSource<'_>,
    receivers: &[Receiver<'_>],
    generated: dae::DaeProvenance,
) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let call = |construction: &mut dae::DaeConstruction<'dae>| {
        lower_call_operands(
            construction,
            symbols,
            &HashMap::new(),
            source.name,
            source.arguments,
            source.provenance,
        )
    };
    let mut ordinals = receivers
        .iter()
        .map(|receiver| receiver.ordinal)
        .collect::<Vec<_>>();
    ordinals.dedup();
    let shared =
        call(construction)?.results(construction, ordinals.iter().copied(), source.provenance)?;
    let mut values = Vec::with_capacity(receivers.len());
    for receiver in receivers {
        let result = ordinals
            .iter()
            .position(|ordinal| *ordinal == receiver.ordinal)
            .and_then(|position| shared.get(position).copied());
        let result = result.ok_or(dae::DaeConstructionError::InvalidExpressionForm {
            span: source.provenance.span(),
        })?;
        values.push(lower_record_projection(
            construction,
            result,
            receiver.projection,
            generated,
        )?);
    }
    Ok(values)
}

/// The planned B.1c owner of the discrete-valued receivers, if any. Analysis
/// admits discrete receivers only in model equations, so an initialization
/// equation has none.
fn discrete_value_definition<'scope, 'dae>(
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    selected: &[(usize, &VarName)],
    discrete: Option<MultiOutputDiscreteOwners<'scope, 'dae>>,
    owner: dae::DaeProvenance,
) -> Result<Option<DiscreteValueDefinition<'scope, 'dae>>, dae::DaeConstructionError> {
    let targets = selected
        .iter()
        .filter(|(_, target)| matches!(coordinates[*target], Coordinate::DiscreteValue(_)))
        .map(|(_, target)| (*target).clone())
        .collect::<Vec<_>>();
    let Some(discrete) = discrete.filter(|_| !targets.is_empty()) else {
        return Ok(None);
    };
    let handle = discrete
        .discrete_values
        .owner(owner, targets, coordinates, discrete.topology)?;
    Ok(handle.map(|owner| DiscreteValueDefinition {
        staging: discrete.discrete_values,
        owner,
    }))
}

/// A continuous, initialization, or discrete Real receiver is defined by the
/// residual `receiver - result`.
fn define_residual_receiver<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    (lhs, value): (dae::ExprId<'dae>, dae::ExprId<'dae>),
    coordinate: Coordinate<'dae>,
    (owner, initialization): (dae::DaeProvenance, bool),
) -> Result<(), dae::DaeConstructionError> {
    let residual = generated_residual(construction, owner, lhs, value)?;
    match coordinate {
        Coordinate::DiscreteReal(_) => {
            construction.discrete(|system| {
                system.real_equation(owner, |equation| equation.residual(residual))
            })?;
        }
        _ if initialization => {
            construction.initialization(|system| system.value_equation(owner, residual))?;
        }
        _ => {
            construction.continuous(|system| system.value_equation(owner, residual))?;
        }
    }
    Ok(())
}
