use super::*;

pub(super) fn lower_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, PeriodicClockSchedule)],
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

pub(super) fn algorithm_condition_owner_clock<'dae>(
    functions: &FunctionRegistry<'_, 'dae>,
    product: &AlgorithmConditionProduct<'_>,
) -> Result<Option<dae::PeriodicClockId<'dae>>, dae::DaeConstructionError> {
    let span = product.span();
    let provenance = dae::DaeProvenance::source(span)?;
    match product {
        AlgorithmConditionProduct::Sample { schedule, .. }
        | AlgorithmConditionProduct::SampleAlias { schedule, .. } => {
            functions.clocks.sample_id(*schedule, span).map(Some)
        }
        AlgorithmConditionProduct::Not { .. } => Ok(None),
        AlgorithmConditionProduct::Binary {
            lhs,
            rhs,
            disjunction,
            ..
        } => merge_condition_clock(
            algorithm_condition_owner_clock(functions, lhs)?,
            algorithm_condition_owner_clock(functions, rhs)?,
            *disjunction,
            provenance,
        ),
        AlgorithmConditionProduct::Vector { elements, .. } => {
            let mut owner = None;
            for element in elements {
                owner = merge_condition_clock(
                    owner,
                    algorithm_condition_owner_clock(functions, element)?,
                    true,
                    provenance,
                )?;
            }
            Ok(owner)
        }
        _ => Ok(None),
    }
}

pub(super) fn lower_algorithm_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    product: &AlgorithmConditionProduct<'_>,
) -> Result<(dae::ConditionId<'dae>, Option<dae::PeriodicClockId<'dae>>), dae::DaeConstructionError>
{
    let (condition, relations, owner_clock) =
        lower_algorithm_condition_tree(construction, coordinates, functions, product)?;
    let span = product.span();
    let provenance = dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, span)?;
    for relation in relations {
        construction.conditions(|conditions| conditions.root(relation, condition, provenance))?;
    }
    Ok((condition, owner_clock))
}

fn lower_algorithm_condition_tree<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    product: &AlgorithmConditionProduct<'_>,
) -> Result<LoweredCondition<'dae>, dae::DaeConstructionError> {
    let span = product.span();
    let provenance = dae::DaeProvenance::source(span)?;
    let (input, relations, owner_clock) = match product {
        AlgorithmConditionProduct::Initial { .. } => {
            (dae::ConditionInput::Initial, Vec::new(), None)
        }
        AlgorithmConditionProduct::Not { rhs, .. } => {
            let (condition, relations, _) =
                lower_algorithm_condition_tree(construction, coordinates, functions, rhs)?;
            (dae::ConditionInput::Not(condition), relations, None)
        }
        AlgorithmConditionProduct::Binary {
            lhs,
            rhs,
            disjunction,
            ..
        } => lower_algorithm_binary_condition(
            construction,
            coordinates,
            functions,
            lhs,
            rhs,
            *disjunction,
            provenance,
        )?,
        AlgorithmConditionProduct::Vector { elements, .. } => {
            return lower_algorithm_vector_condition(
                construction,
                coordinates,
                functions,
                elements,
                span,
            );
        }
        AlgorithmConditionProduct::Sample { schedule, .. }
        | AlgorithmConditionProduct::SampleAlias { schedule, .. } => {
            let clock = functions.clocks.sample_id(*schedule, span)?;
            (
                dae::ConditionInput::Clock(clock.into()),
                Vec::new(),
                Some(clock),
            )
        }
        AlgorithmConditionProduct::Relation {
            source,
            span,
            owner,
        } => lower_algorithm_relation(construction, coordinates, functions, source, *span, *owner)?,
        AlgorithmConditionProduct::Discrete { source, .. } => {
            let expression = lower_expression(construction, coordinates, functions, source, None)?;
            (dae::ConditionInput::Discrete(expression), Vec::new(), None)
        }
    };
    let condition = construction.conditions(|conditions| conditions.reserve(provenance))?;
    construction.conditions(|conditions| conditions.define(condition, input, provenance))?;
    Ok((condition, relations, owner_clock))
}

fn lower_algorithm_relation<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    source: &Expression,
    span: Span,
    owner: AlgorithmRelationOwner,
) -> Result<LoweredConditionNode<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(span)?;
    let lowered = lower_expression(construction, coordinates, functions, source, None)?;
    let relation =
        construction.conditions(|conditions| conditions.relation(lowered, provenance))?;
    let roots = match owner {
        AlgorithmRelationOwner::Root => vec![relation],
        AlgorithmRelationOwner::Scheduled(instant) => {
            construction.events(|events| events.time_event(instant, provenance))?;
            Vec::new()
        }
        AlgorithmRelationOwner::Dynamic(operand) => {
            let Expression::Binary { lhs, rhs, .. } = source else {
                return Err(dae::DaeConstructionError::InvalidExpressionForm { span });
            };
            let deadline = match operand {
                DynamicTimeEventOperand::Lhs => lhs,
                DynamicTimeEventOperand::Rhs => rhs,
            };
            let deadline = lower_expression(construction, coordinates, functions, deadline, None)?;
            let deadline = promote_algorithm_deadline_to_real(construction, deadline, provenance)?;
            construction.events(|events| events.dynamic_time_event(deadline, provenance))?;
            Vec::new()
        }
    };
    Ok((dae::ConditionInput::Relation(relation), roots, None))
}

fn promote_algorithm_deadline_to_real<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    deadline: dae::ExprId<'dae>,
    owner: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let value_type =
        construction.expressions(|expressions| expressions.value_type(deadline, owner))?;
    if !value_type.is_scalar() || value_type.scalar_type() != dae::ScalarType::Integer {
        return Ok(deadline);
    }
    let generated =
        dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, owner.span())?;
    let zero = construction.expressions(|expressions| {
        expressions
            .at(generated)
            .literal(dae::DaeLiteral::Real(0.0))
    })?;
    construction.expressions(|expressions| {
        expressions
            .at(generated)
            .binary(dae::BinaryOperator::Add, deadline, zero)
    })
}

fn lower_algorithm_binary_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    lhs: &AlgorithmConditionProduct<'_>,
    rhs: &AlgorithmConditionProduct<'_>,
    disjunction: bool,
    provenance: dae::DaeProvenance,
) -> Result<LoweredConditionNode<'dae>, dae::DaeConstructionError> {
    let (lhs, mut relations, lhs_clock) =
        lower_algorithm_condition_tree(construction, coordinates, functions, lhs)?;
    let (rhs, rhs_relations, rhs_clock) =
        lower_algorithm_condition_tree(construction, coordinates, functions, rhs)?;
    relations.extend(rhs_relations);
    let input = if disjunction {
        dae::ConditionInput::Or(lhs, rhs)
    } else {
        dae::ConditionInput::And(lhs, rhs)
    };
    let owner_clock = merge_condition_clock(lhs_clock, rhs_clock, disjunction, provenance)?;
    Ok((input, relations, owner_clock))
}

fn lower_algorithm_vector_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    elements: &[AlgorithmConditionProduct<'_>],
    span: Span,
) -> Result<LoweredCondition<'dae>, dae::DaeConstructionError> {
    let generated = dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, span)?;
    let Some((first, rest)) = elements.split_first() else {
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
        lower_algorithm_condition_tree(construction, coordinates, functions, first)?;
    for element in rest {
        let (rhs, rhs_relations, rhs_clock) =
            lower_algorithm_condition_tree(construction, coordinates, functions, element)?;
        condition = combine_element_activations(construction, condition, rhs, span)?;
        relations.extend(rhs_relations);
        owner_clock = merge_condition_clock(owner_clock, rhs_clock, true, generated)?;
    }
    Ok((condition, relations, owner_clock))
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
    sample_lattices: &[(Span, PeriodicClockSchedule)],
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
        Expression::Binary { op, lhs, rhs, .. } if matches!(op, OpBinary::And | OpBinary::Or) => {
            lower_binary_condition(
                construction,
                coordinates,
                functions,
                sample_lattices,
                (lhs, rhs),
                matches!(op, OpBinary::Or),
                provenance,
            )?
        }
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
            args,
            ..
        } => lower_sample_condition(functions, args, provenance)?,
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty()
            && functions
                .sample_alias_schedules
                .contains_key(name.var_name()) =>
        {
            lower_sample_alias_condition(functions, name.var_name(), provenance)?
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

fn lower_sample_alias_condition<'dae>(
    functions: &FunctionRegistry<'_, 'dae>,
    name: &VarName,
    provenance: dae::DaeProvenance,
) -> Result<LoweredConditionNode<'dae>, dae::DaeConstructionError> {
    let schedule = functions.sample_alias_schedules[name];
    let clock = functions.clocks.sample_id(schedule, provenance.span())?;
    Ok((
        dae::ConditionInput::Clock(clock.into()),
        Vec::new(),
        Some(clock),
    ))
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
        Some(ExpressionEventPlan::TimeEvent(_) | ExpressionEventPlan::DynamicTimeEvent(_))
    ) {
        return Vec::new();
    }
    vec![relation]
}

fn lower_sample_condition<'dae>(
    functions: &FunctionRegistry<'_, 'dae>,
    arguments: &[Expression],
    provenance: dae::DaeProvenance,
) -> Result<LoweredConditionNode<'dae>, dae::DaeConstructionError> {
    // Flattening replicates one source span across every component instance.
    // The evaluated operands are therefore part of a sample occurrence's
    // identity: two instances may share the source span while owning different
    // periods or phases. Analysis proved this exact occurrence and lowering
    // must consume that proof instead of selecting the first lattice by span.
    let operands: Vec<&Expression> = arguments.iter().collect();
    let Some(ExpressionEventPlan::SampleClock(schedule)) = functions
        .expression_events
        .plan(provenance.span(), &operands)
    else {
        return Err(dae::DaeConstructionError::InvalidExpressionForm {
            span: provenance.span(),
        });
    };
    let clock = functions.clocks.sample_id(schedule, provenance.span())?;
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
    sample_lattices: &[(Span, PeriodicClockSchedule)],
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

/// MLS §8.3.5 vector activation: `when {c1, …, cn}`.
///
/// §8.3.5 activates the body *"at the instant when … any of the elements of the
/// vector expression becomes true"*, and §8.3.5.1 realises that as one
/// `Boolean bi` per element with the activation `edge(b1) or … or edge(bn)`.
/// That is emphatically not the edge of the disjunction, and OpenModelica
/// distinguishes the two: `when {true, time > 0.5}` fires at `t = 0.5`, while
/// the scalar `when true or time > 0.5` never fires at all, because the
/// disjunction is true from the start and so never rises.
///
/// Folding the elements into one `Or` — one buffer for the whole vector —
/// therefore silently deletes every activation whose disjunction is a tautology.
/// Three MSL blocks are *written* in exactly that shape:
/// `Modelica.Blocks.Logical.TriggeredTrapezoid` and
/// `Modelica.Blocks.Logical.LogicalDelay` (`when {u, not u}`), and
/// `Modelica.Blocks.Math.ContinuousSignalExtrema`
/// (`when {u <= x, u >= x, terminal()}`).
///
/// None of the three compiles in rumoca today, for reasons that have nothing to
/// do with activation — `TriggeredTrapezoid` uses `initial()` inside an
/// expression (ED018), `ContinuousSignalExtrema` uses `terminal()` (ED018) and
/// `pre` of a continuous variable (ED019), and `LogicalDelay` fails its
/// initialization projection. So this is not a claim about their measured
/// behaviour: it is the shape they are written in, and it becomes live for them
/// the moment those operators are supported. What *is* measured is the shape
/// itself, against OpenModelica, on reductions of each block and on the
/// `{u, not u}` and `{true, time > 0.5}` probes.
///
/// The nodes nest to the left, so `{c1, c2, c3}` is
/// `AnyRise(AnyRise(c1, c2), c3)`; the Solve lowering flattens the tree back
/// into one edge per leaf.
fn lower_vector_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, PeriodicClockSchedule)],
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
        condition = combine_element_activations(construction, condition, rhs, span)?;
        relations.extend(rhs_relations);
        owner_clock = merge_condition_clock(owner_clock, rhs_clock, true, generated)?;
    }
    Ok((condition, relations, owner_clock))
}

/// Join two vector elements under [`dae::ConditionInput::AnyRise`].
fn combine_element_activations<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    lhs: dae::ConditionId<'dae>,
    rhs: dae::ConditionId<'dae>,
    span: Span,
) -> Result<dae::ConditionId<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, span)?;
    let combined = construction.conditions(|conditions| conditions.reserve(provenance))?;
    construction.conditions(|conditions| {
        conditions.define(combined, dae::ConditionInput::AnyRise(lhs, rhs), provenance)
    })?;
    Ok(combined)
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
