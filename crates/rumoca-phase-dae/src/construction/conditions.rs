use super::*;

pub(super) fn lower_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    expression: &Expression,
) -> Result<(dae::ConditionId<'dae>, Option<dae::PeriodicClockId<'dae>>), dae::DaeConstructionError>
{
    let (condition, relations, owner_clock) = lower_condition_tree(
        construction,
        coordinates,
        functions,
        sample_lattices,
        expression,
    )?;
    let provenance = dae::DaeProvenance::generated(
        dae::DaeGeneration::ConditionLowering,
        expression
            .span()
            .expect("analysis proves condition provenance"),
    )?;
    for relation in relations {
        construction.conditions(|conditions| conditions.root(relation, condition, provenance))?;
    }
    Ok((condition, owner_clock))
}

type LoweredCondition<'dae> = (
    dae::ConditionId<'dae>,
    Vec<dae::RelationId<'dae>>,
    Option<dae::PeriodicClockId<'dae>>,
);
type LoweredConditionNode<'dae> = (
    dae::ConditionInput<'dae>,
    Vec<dae::RelationId<'dae>>,
    Option<dae::PeriodicClockId<'dae>>,
);

fn lower_condition_tree<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    expression: &Expression,
) -> Result<LoweredCondition<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(
        expression
            .span()
            .expect("analysis proves condition provenance"),
    )?;
    let (input, relations, owner_clock) = match expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Initial,
            ..
        } => (dae::ConditionInput::Initial, Vec::new(), None),
        Expression::Unary {
            op: OpUnary::Not,
            rhs,
            ..
        } => {
            let (condition, relations, _) =
                lower_condition_tree(construction, coordinates, functions, sample_lattices, rhs)?;
            (dae::ConditionInput::Not(condition), relations, None)
        }
        Expression::Binary {
            op: OpBinary::And,
            lhs,
            rhs,
            ..
        }
        | Expression::Binary {
            op: OpBinary::Or,
            lhs,
            rhs,
            ..
        } => lower_binary_condition(
            construction,
            coordinates,
            functions,
            sample_lattices,
            (lhs, rhs),
            matches!(
                expression,
                Expression::Binary {
                    op: OpBinary::Or,
                    ..
                }
            ),
            provenance,
        )?,
        Expression::Array { elements, .. } => {
            return lower_vector_condition(
                construction,
                coordinates,
                functions,
                sample_lattices,
                elements,
                provenance.span(),
            );
        }
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            ..
        } => lower_sample_condition(construction, sample_lattices, provenance)?,
        Expression::BuiltinCall {
            function: BuiltinFunction::Change,
            args,
            ..
        } => {
            let expression = lower_change_expression(
                construction,
                coordinates,
                functions,
                args,
                provenance.span(),
            )?;
            (dae::ConditionInput::Discrete(expression), Vec::new(), None)
        }
        Expression::Binary {
            op:
                OpBinary::Eq | OpBinary::Neq | OpBinary::Lt | OpBinary::Le | OpBinary::Gt | OpBinary::Ge,
            lhs,
            rhs,
            ..
        } => {
            let lowered = lower_expression(construction, coordinates, functions, expression, None)?;
            let relation =
                construction.conditions(|conditions| conditions.relation(lowered, provenance))?;
            let roots =
                activation_relation_roots(functions, provenance.span(), (lhs, rhs), relation);
            (dae::ConditionInput::Relation(relation), roots, None)
        }
        _ => {
            let expression =
                lower_expression(construction, coordinates, functions, expression, None)?;
            (dae::ConditionInput::Discrete(expression), Vec::new(), None)
        }
    };
    let condition = construction.conditions(|conditions| conditions.reserve(provenance))?;
    construction.conditions(|conditions| conditions.define(condition, input, provenance))?;
    Ok((condition, relations, owner_clock))
}

/// The zero crossings a relational activation leaf owns.
///
/// MLS §8.5 gives a relation over `time` alone an exactly known instant, and
/// analysis already scheduled the time event that owns it. Searching for the
/// same instant with a zero crossing as well would not merely duplicate work:
/// §8.5 holds a relation at the value of its last event instant between events,
/// and a located crossing sits *on* the instant, where a strict `time > t` is
/// still false — the activation would be consumed at an instant at which it
/// never reads true.
///
/// The plan is addressed by the operands as well as the span, because flattening
/// gives every instance of a class the same span and each instance owns its own
/// instant.
fn activation_relation_roots<'dae>(
    functions: &FunctionRegistry<'_, 'dae>,
    span: Span,
    operands: (&Expression, &Expression),
    relation: dae::RelationId<'dae>,
) -> Vec<dae::RelationId<'dae>> {
    let (lhs, rhs) = operands;
    if matches!(
        functions.expression_events.plan(span, &[lhs, rhs]),
        Some(ExpressionEventPlan::TimeEvent(_))
    ) {
        return Vec::new();
    }
    vec![relation]
}

fn lower_sample_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    provenance: dae::DaeProvenance,
) -> Result<LoweredConditionNode<'dae>, dae::DaeConstructionError> {
    let lattice = *sample_lattices
        .iter()
        .find_map(|(span, lattice)| (*span == provenance.span()).then_some(lattice))
        .expect("analysis proves every sample condition has an exact clock lattice");
    let clock = construction.clocks(|clocks| clocks.periodic(lattice, provenance))?;
    Ok((
        dae::ConditionInput::Clock(clock.into()),
        Vec::new(),
        Some(clock),
    ))
}

fn lower_binary_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    operands: (&Expression, &Expression),
    disjunction: bool,
    provenance: dae::DaeProvenance,
) -> Result<LoweredConditionNode<'dae>, dae::DaeConstructionError> {
    let (lhs, rhs) = operands;
    let (lhs, mut relations, lhs_clock) =
        lower_condition_tree(construction, coordinates, functions, sample_lattices, lhs)?;
    let (rhs, rhs_relations, rhs_clock) =
        lower_condition_tree(construction, coordinates, functions, sample_lattices, rhs)?;
    relations.extend(rhs_relations);
    let input = if disjunction {
        dae::ConditionInput::Or(lhs, rhs)
    } else {
        dae::ConditionInput::And(lhs, rhs)
    };
    let owner_clock = merge_condition_clock(lhs_clock, rhs_clock, disjunction, provenance)?;
    Ok((input, relations, owner_clock))
}

fn lower_vector_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    elements: &[Expression],
    span: Span,
) -> Result<
    (
        dae::ConditionId<'dae>,
        Vec<dae::RelationId<'dae>>,
        Option<dae::PeriodicClockId<'dae>>,
    ),
    dae::DaeConstructionError,
> {
    let generated = dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, span)?;
    let Some(first) = elements.first() else {
        let expression = construction.expressions(|expressions| {
            expressions
                .at(generated)
                .literal(dae::DaeLiteral::Boolean(false))
        })?;
        let condition = construction.conditions(|conditions| conditions.reserve(generated))?;
        construction.conditions(|conditions| {
            conditions.define(
                condition,
                dae::ConditionInput::Discrete(expression),
                generated,
            )
        })?;
        return Ok((condition, Vec::new(), None));
    };
    let (mut condition, mut relations, mut owner_clock) =
        lower_condition_tree(construction, coordinates, functions, sample_lattices, first)?;
    for element in &elements[1..] {
        let (rhs, rhs_relations, rhs_clock) = lower_condition_tree(
            construction,
            coordinates,
            functions,
            sample_lattices,
            element,
        )?;
        condition = combine_conditions(construction, condition, rhs, true, span)?;
        relations.extend(rhs_relations);
        owner_clock = merge_condition_clock(owner_clock, rhs_clock, true, generated)?;
    }
    Ok((condition, relations, owner_clock))
}

fn lower_change_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    arguments: &[Expression],
    span: Span,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let (name, subscripts) =
        derivative_reference(&arguments[0]).expect("algorithm analysis proves change target");
    let current_provenance = dae::DaeProvenance::source(
        arguments[0]
            .span()
            .expect("algorithm analysis proves change operand provenance"),
    )?;
    let generated = dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, span)?;
    let symbols = LoweringSymbols {
        coordinates,
        functions,
        shapes: functions.shapes.model_values(),
        function_body: None,
        values: None,
        owner_clock: None,
    };
    let current = lower_coordinate_reference(
        construction,
        symbols,
        &HashMap::new(),
        coordinates[name.var_name()].current(),
        subscripts,
        current_provenance,
    )?;
    let previous_coordinate = coordinates[name.var_name()]
        .previous(span)
        .expect("algorithm analysis proves a discrete change target");
    let previous = lower_coordinate_reference(
        construction,
        symbols,
        &HashMap::new(),
        previous_coordinate,
        subscripts,
        generated,
    )?;
    construction.expressions(|expressions| {
        expressions
            .at(generated)
            .binary(dae::BinaryOperator::NotEqual, current, previous)
    })
}

fn merge_condition_clock<'dae>(
    lhs: Option<dae::PeriodicClockId<'dae>>,
    rhs: Option<dae::PeriodicClockId<'dae>>,
    disjunction: bool,
    provenance: dae::DaeProvenance,
) -> Result<Option<dae::PeriodicClockId<'dae>>, dae::DaeConstructionError> {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) if lhs != rhs => Err(dae::DaeConstructionError::DuplicateKey {
            kind: "condition clock owner",
            key: format!("{} and {}", lhs.index(), rhs.index()),
            span: provenance.span(),
        }),
        (Some(clock), Some(_)) => Ok(Some(clock)),
        (Some(clock), None) | (None, Some(clock)) if !disjunction => Ok(Some(clock)),
        _ => Ok(None),
    }
}

pub(super) fn negate_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    condition: dae::ConditionId<'dae>,
    span: Span,
) -> Result<dae::ConditionId<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, span)?;
    let negated = construction.conditions(|conditions| conditions.reserve(provenance))?;
    construction.conditions(|conditions| {
        conditions.define(negated, dae::ConditionInput::Not(condition), provenance)
    })?;
    Ok(negated)
}

pub(super) fn combine_conditions<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    lhs: dae::ConditionId<'dae>,
    rhs: dae::ConditionId<'dae>,
    disjunction: bool,
    span: Span,
) -> Result<dae::ConditionId<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, span)?;
    let combined = construction.conditions(|conditions| conditions.reserve(provenance))?;
    let input = if disjunction {
        dae::ConditionInput::Or(lhs, rhs)
    } else {
        dae::ConditionInput::And(lhs, rhs)
    };
    construction.conditions(|conditions| conditions.define(combined, input, provenance))?;
    Ok(combined)
}
