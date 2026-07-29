//! Constructor-only structural DAE-to-DAE lowering.
//!
//! Regular systems remain borrowed. A singular system is rebuilt only when a
//! scalar state is directly defined by a differentiable constraint. The
//! replacement DAE demotes that state and substitutes the exact symbolic
//! derivative of its definition at every derivative occurrence.

mod functions;
#[cfg(test)]
mod tests;

use rumoca_core::StateSelect;
use rumoca_ir_dae as dae;

use self::functions::{RebuiltFunction, rebuild_functions, reserve_functions};
use crate::{StructuralError, sort};

/// A finalized DAE ready for Solve lowering.
pub enum PreparedDae<'source> {
    Borrowed(&'source dae::Dae),
    Transformed {
        dae: Box<dae::Dae>,
        manifold: Box<[u32]>,
    },
}

impl PreparedDae<'_> {
    pub fn as_dae(&self) -> &dae::Dae {
        match self {
            Self::Borrowed(dae) => dae,
            Self::Transformed { dae, .. } => dae,
        }
    }

    pub fn inspect<R>(
        &self,
        inspect: impl for<'dae> FnOnce(dae::DaeView<'dae>, &[dae::ExprId<'dae>]) -> R,
    ) -> R {
        match self {
            Self::Borrowed(dae) => dae.inspect(|view| inspect(view, &[])),
            Self::Transformed { dae, manifold } => dae.inspect(|view| {
                let expressions = manifold
                    .iter()
                    .map(|index| {
                        view.expression_id(*index as usize)
                            .expect("prepared manifold expression resolves")
                    })
                    .collect::<Vec<_>>();
                inspect(view, &expressions)
            }),
        }
    }
}

#[derive(Clone, Copy)]
struct DirectStateConstraint {
    state: u32,
    rhs: u32,
    owner: dae::DaeProvenance,
}

#[derive(Clone, Copy)]
struct HolonomicConstraint {
    residual: u32,
    owner: dae::DaeProvenance,
}

/// Prepare a finalized DAE for Solve without admitting a weaker intermediate.
///
/// The currently accepted index-one subset is intentionally narrow: primitive
/// scalar expressions, direct state definitions, and unstructured continuous
/// and initialization residuals. A model outside that subset retains its
/// original structural error instead of receiving a guessed transformation.
pub fn prepare_for_solve(model: &dae::Dae) -> Result<PreparedDae<'_>, StructuralError> {
    let singular = match model.inspect(|view| sort(view).map(|_| ())) {
        Ok(_) => return Ok(PreparedDae::Borrowed(model)),
        Err(error @ StructuralError::Singular { .. }) => error,
        Err(StructuralError::EmptySystem) => return Ok(PreparedDae::Borrowed(model)),
        Err(error) => return Err(error),
    };
    let candidates = model.inspect(direct_state_constraints);
    for candidate in candidates {
        let rebuilt = rebuild_with_state_demotion(model, candidate)?;
        if rebuilt.inspect(|view| sort(view).map(|_| ())).is_ok() {
            return Ok(PreparedDae::Transformed {
                dae: Box::new(rebuilt),
                manifold: Box::new([]),
            });
        }
    }
    for constraint in model.inspect(holonomic_constraints) {
        let (rebuilt, manifold) = rebuild_holonomic_constraint(model, constraint)?;
        if rebuilt.inspect(|view| sort(view).map(|_| ())).is_ok() {
            return Ok(PreparedDae::Transformed {
                dae: Box::new(rebuilt),
                manifold: manifold.into_boxed_slice(),
            });
        }
    }
    Err(singular)
}

fn direct_state_constraints(view: dae::DaeView<'_>) -> Vec<DirectStateConstraint> {
    let mut constraints = view
        .continuous_owners()
        .filter_map(|owner| match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => {
                direct_state_constraint(view, equation)
            }
            dae::ContinuousOwnerView::Structured { .. } => None,
        })
        .collect::<Vec<_>>();
    constraints.sort_by_key(|candidate| {
        let selection = view
            .variable(
                view.variable_id(candidate.state as usize)
                    .expect("candidate state identity resolves"),
            )
            .expect("candidate state declaration resolves")
            .state_select();
        (state_demotion_priority(selection), candidate.state)
    });
    constraints
}

fn state_demotion_priority(selection: StateSelect) -> u8 {
    match selection {
        StateSelect::Never => 0,
        StateSelect::Avoid => 1,
        StateSelect::Default => 2,
        StateSelect::Prefer => 3,
        StateSelect::Always => 4,
    }
}

fn direct_state_constraint<'dae>(
    view: dae::DaeView<'dae>,
    equation: dae::ResidualEquationView<'dae>,
) -> Option<DirectStateConstraint> {
    let residual = view.expression(equation.residual())?;
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Subtract,
        lhs,
        rhs,
    } = residual.operation()
    else {
        return None;
    };
    let dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(state)) =
        view.expression(lhs)?.operation()
    else {
        return None;
    };
    let variable = view.variable(view.variable_id(state.index() as usize)?)?;
    if variable.state_select() == StateSelect::Always
        || !variable.value_type().is_scalar()
        || variable.value_type().scalar_type() != dae::ScalarType::Real
        || dae::expr_contains_var(view, rhs, variable.id())
        || !is_differentiable(view, rhs, state, &mut vec![false; view.expression_count()])
    {
        return None;
    }
    Some(DirectStateConstraint {
        state: state.index(),
        rhs: rhs.index(),
        owner: equation.provenance(),
    })
}

fn holonomic_constraints(view: dae::DaeView<'_>) -> Vec<HolonomicConstraint> {
    let definitions = explicit_derivative_definitions(view);
    view.continuous_owners()
        .filter_map(|owner| {
            let dae::ContinuousOwnerView::Residual { equation, .. } = owner else {
                return None;
            };
            let residual = equation.residual();
            let mut has_state = false;
            let mut forbidden = false;
            dae::for_each_expression(view, residual, |_, expression| {
                let dae::ExpressionOperation::Coordinate(coordinate) = expression.operation()
                else {
                    return;
                };
                match coordinate {
                    dae::CoordinateView::State(_) => has_state = true,
                    dae::CoordinateView::Derivative(_) | dae::CoordinateView::Algebraic(_) => {
                        forbidden = true;
                    }
                    _ => {}
                }
            });
            (has_state && !forbidden && can_differentiate_order(view, residual, 2, &definitions))
                .then_some(HolonomicConstraint {
                    residual: residual.index(),
                    owner: equation.provenance(),
                })
        })
        .collect()
}

fn can_differentiate_order<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    order: u8,
    definitions: &[Option<u32>],
) -> bool {
    let expression = view
        .expression(expression)
        .expect("checked differentiability expression resolves");
    match expression.operation() {
        dae::ExpressionOperation::Literal(_) => true,
        dae::ExpressionOperation::Coordinate(coordinate) => match coordinate {
            dae::CoordinateView::Parameter(_) | dae::CoordinateView::Time => true,
            dae::CoordinateView::State(state) => {
                definitions[state.index() as usize].is_some_and(|definition| {
                    order == 1
                        || can_differentiate_order(
                            view,
                            view.expression_id(definition as usize)
                                .expect("derivative definition resolves"),
                            order - 1,
                            definitions,
                        )
                })
            }
            _ => false,
        },
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => can_differentiate_order(view, operand, order, definitions),
        dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
            matches!(
                operator,
                dae::BinaryOperator::Add
                    | dae::BinaryOperator::Subtract
                    | dae::BinaryOperator::Multiply
            ) && can_differentiate_order(view, lhs, order, definitions)
                && can_differentiate_order(view, rhs, order, definitions)
        }
        _ => false,
    }
}

fn is_differentiable<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    demoted: dae::StateId<'dae>,
    visited: &mut [bool],
) -> bool {
    let index = expression.index() as usize;
    if visited[index] {
        return true;
    }
    visited[index] = true;
    let Some(expression) = view.expression(expression) else {
        return false;
    };
    match expression.operation() {
        dae::ExpressionOperation::Literal(_) => true,
        dae::ExpressionOperation::Coordinate(coordinate) => match coordinate {
            dae::CoordinateView::Parameter(_) | dae::CoordinateView::Time => true,
            dae::CoordinateView::State(state) => state != demoted,
            _ => false,
        },
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => is_differentiable(view, operand, demoted, visited),
        dae::ExpressionOperation::Binary {
            operator:
                dae::BinaryOperator::Add
                | dae::BinaryOperator::Subtract
                | dae::BinaryOperator::Multiply
                | dae::BinaryOperator::Divide,
            lhs,
            rhs,
        } => {
            is_differentiable(view, lhs, demoted, visited)
                && is_differentiable(view, rhs, demoted, visited)
        }
        _ => false,
    }
}

fn rebuild_holonomic_constraint(
    model: &dae::Dae,
    constraint: HolonomicConstraint,
) -> Result<(dae::Dae, Vec<u32>), StructuralError> {
    let mut manifold = Vec::with_capacity(2);
    let rebuilt = model.inspect(|source| {
        dae::Dae::construct(model.source_map().clone(), |target| {
            let types = rebuild_types(source, target)?;
            let domains = rebuild_domains(source, target)?;
            let (functions, mut function_reservations) = reserve_functions(source, target, &types)?;
            let mut variables = reserve_variables(source, target, &types, None)?;
            let conditions = reserve_conditions(source, target)?;
            let clocks = rebuild_clocks(source, target, &variables, &conditions)?;
            let temporal = rebuild_temporal_coordinates(source, target, &variables, &clocks)?;
            let derivative_definitions = explicit_derivative_definitions(source);
            let identities = RebuiltIdentities {
                types: &types,
                functions: &functions,
                variables: &variables,
                domains: &domains,
                conditions: &conditions,
                previous: &temporal.previous,
                terminals: &temporal.terminals,
            };
            let mut rebuilt_state = vec![None; source.expression_count()];
            rebuild_delay_coordinates(
                source,
                target,
                identities,
                &derivative_definitions,
                None,
                &mut rebuilt_state,
            )?;
            rebuild_functions(
                source,
                target,
                identities,
                &derivative_definitions,
                None,
                &mut rebuilt_state,
                &mut function_reservations,
            )?;
            let (expressions, replacement) = target.expressions(|expressions| {
                let mut rebuilder = ExpressionRebuilder::new(
                    source,
                    expressions,
                    identities,
                    &derivative_definitions,
                    None,
                    &mut rebuilt_state,
                );
                let rebuilt = rebuilder.rebuild_all()?;
                let source_residual = source
                    .expression_id(constraint.residual as usize)
                    .expect("holonomic residual resolves");
                let provenance = dae::DaeProvenance::generated(
                    dae::DaeGeneration::IndexReduction,
                    constraint.owner.span(),
                )?;
                let first = rebuilder.differentiate_order(source_residual, 1, provenance)?;
                let first = rebuilder.materialize_derivative(first, provenance)?;
                let second = rebuilder.differentiate_order(source_residual, 2, provenance)?;
                let second = rebuilder.materialize_derivative(second, provenance)?;
                manifold.extend([rebuilt[constraint.residual as usize].index(), first.index()]);
                Ok((rebuilt, second))
            })?;
            define_variables(source, target, &expressions, &mut variables)?;
            rebuild_semantic_owners(
                source,
                target,
                &expressions,
                RebuiltOwnerIdentities {
                    variables: &variables,
                    domains: &domains,
                    conditions: &conditions,
                    clocks: &clocks,
                },
                Some((constraint.residual, replacement)),
            )
        })
    });
    rebuilt
        .map(|dae| (dae, manifold))
        .map_err(|error| construction_failure(model, error))
}

fn rebuild_with_state_demotion(
    model: &dae::Dae,
    candidate: DirectStateConstraint,
) -> Result<dae::Dae, StructuralError> {
    let rebuilt = model.inspect(|source| {
        dae::Dae::construct(model.source_map().clone(), |target| {
            let types = rebuild_types(source, target)?;
            let domains = rebuild_domains(source, target)?;
            let (functions, mut function_reservations) = reserve_functions(source, target, &types)?;
            let mut variables = reserve_variables(source, target, &types, Some(candidate.state))?;
            let conditions = reserve_conditions(source, target)?;
            let clocks = rebuild_clocks(source, target, &variables, &conditions)?;
            let temporal = rebuild_temporal_coordinates(source, target, &variables, &clocks)?;
            let derivative_definitions = explicit_derivative_definitions(source);
            let identities = RebuiltIdentities {
                types: &types,
                functions: &functions,
                variables: &variables,
                domains: &domains,
                conditions: &conditions,
                previous: &temporal.previous,
                terminals: &temporal.terminals,
            };
            let mut rebuilt_state = vec![None; source.expression_count()];
            rebuild_delay_coordinates(
                source,
                target,
                identities,
                &derivative_definitions,
                Some(candidate),
                &mut rebuilt_state,
            )?;
            rebuild_functions(
                source,
                target,
                identities,
                &derivative_definitions,
                Some(candidate),
                &mut rebuilt_state,
                &mut function_reservations,
            )?;
            let expressions = target.expressions(|expressions| {
                let mut rebuilder = ExpressionRebuilder::new(
                    source,
                    expressions,
                    identities,
                    &derivative_definitions,
                    Some(candidate),
                    &mut rebuilt_state,
                );
                rebuilder.rebuild_all()
            })?;
            define_variables(source, target, &expressions, &mut variables)?;
            rebuild_semantic_owners(
                source,
                target,
                &expressions,
                RebuiltOwnerIdentities {
                    variables: &variables,
                    domains: &domains,
                    conditions: &conditions,
                    clocks: &clocks,
                },
                None,
            )
        })
    });
    rebuilt.map_err(|error| construction_failure(model, error))
}

fn explicit_derivative_definitions(view: dae::DaeView<'_>) -> Vec<Option<u32>> {
    let mut definitions = vec![None; view.variable_count()];
    let mut duplicate = vec![false; view.variable_count()];
    for owner in view.continuous_owners() {
        let dae::ContinuousOwnerView::Residual { equation, .. } = owner else {
            continue;
        };
        let Some(residual) = view.expression(equation.residual()) else {
            continue;
        };
        let dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Subtract,
            lhs,
            rhs,
        } = residual.operation()
        else {
            continue;
        };
        let Some(lhs) = view.expression(lhs) else {
            continue;
        };
        let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state)) =
            lhs.operation()
        else {
            continue;
        };
        let index = state.index() as usize;
        if definitions[index].replace(rhs.index()).is_some() {
            duplicate[index] = true;
        }
    }
    for (definition, duplicate) in definitions.iter_mut().zip(duplicate) {
        if duplicate {
            *definition = None;
        }
    }
    definitions
}

fn construction_failure(model: &dae::Dae, error: dae::DaeConstructionError) -> StructuralError {
    let span = error
        .source_span()
        .or_else(|| model.inspect(|view| view.responsible_span()));
    match span {
        Some(span) => StructuralError::ContractViolation {
            reason: format!("checked index-reduction reconstruction failed: {error}"),
            span,
        },
        None => StructuralError::UnspannedContractViolation {
            reason: format!("checked index-reduction reconstruction failed: {error}"),
        },
    }
}

fn rebuild_types<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
) -> Result<Vec<dae::ValueTypeId<'target>>, dae::DaeConstructionError> {
    target.types(|types| {
        let mut rebuilt = Vec::with_capacity(source.value_type_count());
        for index in 0..source.value_type_count() {
            let source_id = source
                .value_type_id(index)
                .expect("finalized value type ordinal resolves");
            let value_type = source
                .value_type(source_id)
                .expect("finalized value type identity resolves");
            let provenance = source
                .value_type_provenance(source_id)
                .expect("finalized value type has provenance");
            let rebuilt_type = if value_type.is_record() {
                rebuild_record_type(source, source_id, value_type, provenance, &rebuilt, types)?
            } else {
                let value_type = value_type.clone();
                match source.effective_flat_type(source_id) {
                    Some(flat_type) => types.intern(flat_type, value_type, provenance)?,
                    None => types.derived(value_type, provenance)?,
                }
            };
            rebuilt.push(rebuilt_type);
        }
        Ok(rebuilt)
    })
}

fn rebuild_record_type<'source, 'target>(
    source: dae::DaeView<'source>,
    source_id: dae::ValueTypeId<'source>,
    value_type: &dae::ValueType,
    provenance: dae::DaeProvenance,
    rebuilt: &[dae::ValueTypeId<'target>],
    types: &mut dae::ValueTypes<'_, 'target>,
) -> Result<dae::ValueTypeId<'target>, dae::DaeConstructionError> {
    let fields = (0..value_type.record_field_count()).map(|ordinal| {
        let (name, field_type) = source
            .record_field(source_id, ordinal)
            .expect("checked record field ordinal resolves");
        let field_type = rebuilt
            .get(field_type.index() as usize)
            .copied()
            .expect("checked record field type precedes its record owner");
        (name.clone(), field_type)
    });
    types.record(
        value_type
            .record_name()
            .expect("checked record has a canonical name")
            .clone(),
        fields,
        provenance,
    )
}

struct RebuiltDomain<'dae> {
    id: dae::DomainId<'dae>,
    binders: Vec<dae::DomainBinderId<'dae>>,
}

fn rebuild_domains<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
) -> Result<Vec<RebuiltDomain<'target>>, dae::DaeConstructionError> {
    let mut rebuilt: Vec<RebuiltDomain<'target>> = Vec::with_capacity(source.domain_count());
    for index in 0..source.domain_count() {
        let source_id = source
            .domain_id(index)
            .expect("finalized domain ordinal resolves");
        let domain = source
            .domain(source_id)
            .expect("finalized domain identity resolves");
        let provenance = domain.provenance();
        let id = target.domains(|domains| match domain.parent() {
            Some(parent) => domains.nested(
                rebuilt[parent.index() as usize].id,
                domain.structured().clone(),
                provenance,
            ),
            None => domains.structured(domain.structured().clone(), provenance),
        })?;
        let binders = target.domains(|domains| {
            (0..domain.structured().binders.len())
                .map(|ordinal| domains.binder(id, ordinal, provenance))
                .collect::<Result<Vec<_>, _>>()
        })?;
        rebuilt.push(RebuiltDomain { id, binders });
    }
    Ok(rebuilt)
}

fn reserve_conditions<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
) -> Result<Vec<dae::ConditionId<'target>>, dae::DaeConstructionError> {
    (0..source.condition_count())
        .map(|index| {
            let id = source
                .condition_id(index)
                .expect("finalized condition ordinal resolves");
            let condition = source
                .condition(id)
                .expect("finalized condition identity resolves");
            target.conditions(|conditions| conditions.reserve(condition.provenance()))
        })
        .collect()
}

fn rebuild_clocks<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    variables: &[ReservedVariable<'target>],
    conditions: &[dae::ConditionId<'target>],
) -> Result<Vec<dae::ClockId<'target>>, dae::DaeConstructionError> {
    let mut clocks = Vec::with_capacity(source.clock_count());
    for index in 0..source.clock_count() {
        let id = source
            .clock_id(index)
            .expect("finalized clock ordinal resolves");
        let clock = source.clock(id).expect("finalized clock identity resolves");
        let rebuilt = target.clocks(|target| match clock.operation() {
            dae::ClockOperation::Periodic(lattice) => target.periodic(*lattice, clock.provenance()),
            dae::ClockOperation::Triggered(condition) => {
                target.triggered(conditions[condition.index() as usize], clock.provenance())
            }
        })?;
        clocks.push(rebuilt);
    }
    for index in 0..source.clock_ownership_count() {
        rebuild_clock_ownership(source, target, variables, &clocks, index)?;
    }
    Ok(clocks)
}

fn rebuild_clock_ownership<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    variables: &[ReservedVariable<'target>],
    clocks: &[dae::ClockId<'target>],
    index: usize,
) -> Result<(), dae::DaeConstructionError> {
    let id = source
        .clock_ownership_id(index)
        .expect("finalized clock ownership ordinal resolves");
    let ownership = source
        .clock_ownership(id)
        .expect("finalized clock ownership identity resolves");
    let clock = clocks[ownership.clock().index() as usize];
    target.clocks(|target| match ownership.kind() {
        dae::ClockedVariableKind::DiscreteReal => {
            let TargetVariable::DiscreteReal(variable) =
                variables[ownership.variable().index() as usize].identity
            else {
                unreachable!("clock ownership retains its discrete-real role")
            };
            target.own_discrete_real(clock, variable, ownership.provenance())
        }
        dae::ClockedVariableKind::DiscreteValue => {
            let TargetVariable::DiscreteValue(variable) =
                variables[ownership.variable().index() as usize].identity
            else {
                unreachable!("clock ownership retains its discrete-value role")
            };
            target.own_discrete_value(clock, variable, ownership.provenance())
        }
    })?;
    Ok(())
}

struct RebuiltTemporal<'dae> {
    previous: Vec<dae::PreviousId<'dae>>,
    terminals: Vec<dae::TerminalId<'dae>>,
}

fn rebuild_temporal_coordinates<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    variables: &[ReservedVariable<'target>],
    clocks: &[dae::ClockId<'target>],
) -> Result<RebuiltTemporal<'target>, dae::DaeConstructionError> {
    let mut previous = Vec::with_capacity(source.previous_value_count());
    for index in 0..source.previous_value_count() {
        let id = source
            .previous_id(index)
            .expect("finalized previous-value ordinal resolves");
        let entry = source
            .previous(id)
            .expect("finalized previous-value identity resolves");
        let clock = clocks[entry.clock().index() as usize];
        let rebuilt = target.temporal(|target| {
            match variables[entry.variable().index() as usize].identity {
                TargetVariable::DiscreteReal(variable) => {
                    target.previous_discrete_real(clock, variable, entry.provenance())
                }
                TargetVariable::DiscreteValue(variable) => {
                    target.previous_discrete_value(clock, variable, entry.provenance())
                }
                _ => unreachable!("previous coordinate retains its discrete variable role"),
            }
        })?;
        previous.push(rebuilt);
    }
    let terminals = (0..source.terminal_count())
        .map(|index| {
            let id = source
                .terminal_id(index)
                .expect("finalized terminal ordinal resolves");
            let entry = source
                .terminal(id)
                .expect("finalized terminal identity resolves");
            target.temporal(|target| target.terminal(entry.provenance()))
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(RebuiltTemporal {
        previous,
        terminals,
    })
}

fn rebuild_delay_coordinates<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    identities: RebuiltIdentities<'_, 'target>,
    derivative_definitions: &[Option<u32>],
    candidate: Option<DirectStateConstraint>,
    rebuilt: &mut [Option<dae::ExprId<'target>>],
) -> Result<(), dae::DaeConstructionError> {
    let mut coordinate_indices = vec![None; source.delay_count()];
    for index in 0..source.expression_count() {
        let expression_id = source
            .expression_id(index)
            .expect("finalized expression ordinal resolves");
        let expression = source
            .expression(expression_id)
            .expect("finalized expression identity resolves");
        if let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Delay(delay)) =
            expression.operation()
        {
            coordinate_indices[delay.index() as usize] = Some(index);
        }
    }
    for (index, coordinate_index) in coordinate_indices.into_iter().enumerate() {
        let source_id = source
            .delay_id(index)
            .expect("finalized delay ordinal resolves");
        let delay = source
            .delay(source_id)
            .expect("finalized delay identity resolves");
        let coordinate_index =
            coordinate_index.expect("checked delay has exactly one coordinate expression");
        let coordinate_id = source
            .expression_id(coordinate_index)
            .expect("delay coordinate expression resolves");
        let coordinate_provenance = source
            .expression(coordinate_id)
            .expect("delay coordinate expression identity resolves")
            .provenance();
        let (rebuilt_source, rebuilt_time, rebuilt_maximum) =
            target.expressions(|expressions| {
                let mut rebuilder = ExpressionRebuilder::new(
                    source,
                    expressions,
                    identities,
                    derivative_definitions,
                    candidate,
                    rebuilt,
                );
                Ok((
                    rebuilder.rebuild(delay.source())?,
                    rebuilder.rebuild(delay.delay_time())?,
                    delay
                        .delay_max()
                        .map(|maximum| rebuilder.rebuild(maximum.expression()))
                        .transpose()?,
                ))
            })?;
        let coordinate =
            target.temporal(
                |temporal| match (delay.delay_time_evidence(), delay.delay_max()) {
                    (Some(evidence), None) => {
                        let positive = temporal.positive_parameter(
                            rebuilt_time,
                            evidence.value(),
                            evidence.provenance(),
                        )?;
                        temporal.delay(
                            rebuilt_source,
                            positive,
                            delay.provenance(),
                            coordinate_provenance,
                        )
                    }
                    (None, Some(maximum)) => {
                        let positive = temporal.positive_parameter(
                            rebuilt_maximum.expect("bounded delay has a rebuilt maximum"),
                            maximum.value(),
                            maximum.provenance(),
                        )?;
                        temporal.bounded_delay(
                            rebuilt_source,
                            rebuilt_time,
                            positive,
                            delay.provenance(),
                            coordinate_provenance,
                        )
                    }
                    _ => unreachable!("checked delay retains one timing evidence form"),
                },
            )?;
        if coordinate.id().index() as usize != index {
            return Err(dae::DaeConstructionError::ShapeMismatch {
                span: delay.provenance().span(),
            });
        }
        rebuilt[coordinate_index] = Some(coordinate.expression());
    }
    Ok(())
}

enum TargetVariable<'dae> {
    Parameter(dae::ParameterId<'dae>),
    Input(dae::InputId<'dae>),
    State(dae::StateId<'dae>),
    Algebraic(dae::AlgebraicId<'dae>),
    DiscreteReal(dae::DiscreteRealId<'dae>),
    DiscreteValue(dae::DiscreteValueId<'dae>),
}

impl Copy for TargetVariable<'_> {}

impl Clone for TargetVariable<'_> {
    fn clone(&self) -> Self {
        *self
    }
}

struct ReservedVariable<'dae> {
    identity: TargetVariable<'dae>,
    reservation: Option<dae::VariableReservation<'dae>>,
}

fn reserve_variables<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    types: &[dae::ValueTypeId<'target>],
    demoted: Option<u32>,
) -> Result<Vec<ReservedVariable<'target>>, dae::DaeConstructionError> {
    target.variables(|variables| {
        source
            .variables()
            .map(|(_, variable)| {
                reserve_variable(
                    variables,
                    variable,
                    types[variable.value_type_id().index() as usize],
                    demoted,
                )
            })
            .collect()
    })
}

fn reserve_variable<'target>(
    variables: &mut dae::Variables<'_, 'target>,
    variable: dae::VariableView<'_>,
    value_type: dae::ValueTypeId<'target>,
    demoted: Option<u32>,
) -> Result<ReservedVariable<'target>, dae::DaeConstructionError> {
    let name = variable.name().clone();
    let declaration = variable.declaration();
    let (identity, reservation) = match variable.role() {
        dae::VariableRole::Parameter => {
            let (id, reservation) = variables.reserve_parameter(name, value_type, declaration)?;
            (TargetVariable::Parameter(id), reservation)
        }
        dae::VariableRole::Constant => {
            let (id, reservation) = variables.reserve_constant(name, value_type, declaration)?;
            (TargetVariable::Parameter(id), reservation)
        }
        dae::VariableRole::Input => {
            let variability = input_variability(variable.variability());
            let (id, reservation) =
                variables.reserve_input(name, value_type, variability, declaration)?;
            (TargetVariable::Input(id), reservation)
        }
        dae::VariableRole::State if Some(variable.id().index()) == demoted => {
            let (id, reservation) = variables.reserve_algebraic(name, value_type, declaration)?;
            (TargetVariable::Algebraic(id), reservation)
        }
        dae::VariableRole::State => {
            let (id, reservation) = variables.reserve_state(name, value_type, declaration)?;
            (TargetVariable::State(id), reservation)
        }
        dae::VariableRole::Algebraic => {
            let (id, reservation) = variables.reserve_algebraic(name, value_type, declaration)?;
            (TargetVariable::Algebraic(id), reservation)
        }
        dae::VariableRole::Output => {
            let (id, reservation) = variables.reserve_output(name, value_type, declaration)?;
            (TargetVariable::Algebraic(id), reservation)
        }
        dae::VariableRole::DiscreteReal => {
            let (id, reservation) =
                variables.reserve_discrete_real(name, value_type, declaration)?;
            (TargetVariable::DiscreteReal(id), reservation)
        }
        dae::VariableRole::DiscreteValue => {
            let (id, reservation) =
                variables.reserve_discrete_value(name, value_type, declaration)?;
            (TargetVariable::DiscreteValue(id), reservation)
        }
    };
    Ok(ReservedVariable {
        identity,
        reservation: Some(reservation),
    })
}

fn input_variability(variability: dae::ExpressionVariability) -> dae::InputVariability {
    if variability == dae::ExpressionVariability::Continuous {
        dae::InputVariability::Continuous
    } else {
        dae::InputVariability::Discrete
    }
}

fn define_variables<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
    variables: &mut [ReservedVariable<'target>],
) -> Result<(), dae::DaeConstructionError> {
    target.variables(|target| {
        for ((_, source), reserved) in source.variables().zip(variables) {
            let expression = |id: dae::ExprId<'_>| expressions[id.index() as usize];
            let attributes = dae::VariableAttributes {
                component_ref: source.component_reference().cloned(),
                binding: source.binding().map(expression),
                start: source.start().map(expression),
                fixed: source.fixed(),
                min: source.minimum().map(expression),
                max: source.maximum().map(expression),
                nominal: source.nominal().map(expression),
                unit: source.unit().map(str::to_owned),
                state_select: source.state_select(),
                description: source.description().map(str::to_owned),
                causality: source.causality(),
                is_tunable: source.is_tunable(),
                is_held: source.is_held(),
                origin: source.origin(),
            };
            target.define(
                reserved
                    .reservation
                    .take()
                    .expect("each variable reservation is consumed exactly once"),
                attributes,
                source.declaration(),
            )?;
        }
        Ok(())
    })
}

struct RebuiltOwnerIdentities<'borrow, 'target> {
    variables: &'borrow [ReservedVariable<'target>],
    domains: &'borrow [RebuiltDomain<'target>],
    conditions: &'borrow [dae::ConditionId<'target>],
    clocks: &'borrow [dae::ClockId<'target>],
}

fn rebuild_semantic_owners<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
    identities: RebuiltOwnerIdentities<'_, 'target>,
    replacement: Option<(u32, dae::ExprId<'target>)>,
) -> Result<(), dae::DaeConstructionError> {
    rebuild_equations(
        source,
        target,
        expressions,
        identities.variables,
        identities.domains,
        replacement,
    )?;
    let relations = rebuild_relations(source, target, expressions)?;
    define_conditions(
        source,
        target,
        expressions,
        identities.conditions,
        &relations,
        identities.clocks,
    )?;
    rebuild_roots(source, target, identities.conditions, &relations)?;
    rebuild_events(
        source,
        target,
        expressions,
        identities.variables,
        identities.conditions,
    )
}

fn rebuild_relations<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
) -> Result<Vec<dae::RelationId<'target>>, dae::DaeConstructionError> {
    (0..source.relation_count())
        .map(|index| {
            let id = source
                .relation_id(index)
                .expect("finalized relation ordinal resolves");
            let relation = source
                .relation(id)
                .expect("finalized relation identity resolves");
            target.conditions(|conditions| {
                conditions.relation(
                    expressions[relation.expression().index() as usize],
                    relation.provenance(),
                )
            })
        })
        .collect()
}

fn define_conditions<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
    conditions: &[dae::ConditionId<'target>],
    relations: &[dae::RelationId<'target>],
    clocks: &[dae::ClockId<'target>],
) -> Result<(), dae::DaeConstructionError> {
    for (index, target_id) in conditions.iter().copied().enumerate() {
        let source_id = source
            .condition_id(index)
            .expect("finalized condition ordinal resolves");
        let condition = source
            .condition(source_id)
            .expect("finalized condition identity resolves");
        let input = match condition.operation() {
            dae::ConditionOperation::Initial => dae::ConditionInput::Initial,
            dae::ConditionOperation::Relation(id) => {
                dae::ConditionInput::Relation(relations[id.index() as usize])
            }
            dae::ConditionOperation::Discrete(expression) => {
                dae::ConditionInput::Discrete(expressions[expression.index() as usize])
            }
            dae::ConditionOperation::Not(id) => {
                dae::ConditionInput::Not(conditions[id.index() as usize])
            }
            dae::ConditionOperation::And(lhs, rhs) => dae::ConditionInput::And(
                conditions[lhs.index() as usize],
                conditions[rhs.index() as usize],
            ),
            dae::ConditionOperation::Or(lhs, rhs) => dae::ConditionInput::Or(
                conditions[lhs.index() as usize],
                conditions[rhs.index() as usize],
            ),
            dae::ConditionOperation::Clock(id) => {
                dae::ConditionInput::Clock(clocks[id.index() as usize])
            }
        };
        target
            .conditions(|conditions| conditions.define(target_id, input, condition.provenance()))?;
    }
    Ok(())
}

fn rebuild_roots<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    conditions: &[dae::ConditionId<'target>],
    relations: &[dae::RelationId<'target>],
) -> Result<(), dae::DaeConstructionError> {
    for index in 0..source.root_count() {
        let id = source
            .root_id(index)
            .expect("finalized root ordinal resolves");
        let root = source.root(id).expect("finalized root identity resolves");
        target.conditions(|target| {
            target.root(
                relations[root.relation().index() as usize],
                conditions[root.activation().index() as usize],
                root.provenance(),
            )
        })?;
    }
    Ok(())
}

fn rebuild_events<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
    variables: &[ReservedVariable<'target>],
    conditions: &[dae::ConditionId<'target>],
) -> Result<(), dae::DaeConstructionError> {
    for index in 0..source.time_event_count() {
        let id = source
            .time_event_id(index)
            .expect("finalized time-event ordinal resolves");
        let event = source
            .time_event(id)
            .expect("finalized time-event identity resolves");
        target.events(|events| events.time_event(*event.instant(), event.provenance()))?;
    }
    for index in 0..source.event_action_count() {
        rebuild_event_action(source, target, expressions, variables, conditions, index)?;
    }
    Ok(())
}

fn rebuild_event_action<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
    variables: &[ReservedVariable<'target>],
    conditions: &[dae::ConditionId<'target>],
    index: usize,
) -> Result<(), dae::DaeConstructionError> {
    let id = source
        .event_action_id(index)
        .expect("finalized event-action ordinal resolves");
    let action = source
        .event_action(id)
        .expect("finalized event-action identity resolves");
    let trigger = conditions[action.trigger().index() as usize];
    let guard = conditions[action.guard().index() as usize];
    target.events(|events| match action.operation() {
        dae::EventActionOperation::Assert { message, level } => events.assert_with_level(
            trigger,
            guard,
            expressions[message.index() as usize],
            level.map(|level| expressions[level.index() as usize]),
            action.provenance(),
        ),
        dae::EventActionOperation::Terminate { message } => events.terminate(
            trigger,
            guard,
            expressions[message.index() as usize],
            action.provenance(),
        ),
        dae::EventActionOperation::Reinitialize { state, value } => {
            let TargetVariable::State(state) = variables[state.index() as usize].identity else {
                unreachable!("event reinitialization target retains its state role")
            };
            events.reinitialize(
                trigger,
                guard,
                state,
                expressions[value.index() as usize],
                action.provenance(),
            )
        }
        dae::EventActionOperation::AssignDiscreteReal { target, value } => {
            let TargetVariable::DiscreteReal(target) = variables[target.index() as usize].identity
            else {
                unreachable!("event assignment retains its discrete-real role")
            };
            events.assign_discrete_real(
                trigger,
                guard,
                target,
                expressions[value.index() as usize],
                action.provenance(),
            )
        }
        dae::EventActionOperation::AssignDiscreteValue { target, value } => {
            let TargetVariable::DiscreteValue(target) = variables[target.index() as usize].identity
            else {
                unreachable!("event assignment retains its discrete-value role")
            };
            events.assign_discrete_value(
                trigger,
                guard,
                target,
                expressions[value.index() as usize],
                action.provenance(),
            )
        }
    })?;
    Ok(())
}

fn rebuild_equations<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
    variables: &[ReservedVariable<'target>],
    domains: &[RebuiltDomain<'target>],
    replacement: Option<(u32, dae::ExprId<'target>)>,
) -> Result<(), dae::DaeConstructionError> {
    target.continuous(|target| {
        for owner in source.continuous_owners() {
            match owner {
                dae::ContinuousOwnerView::Residual { equation, .. } => {
                    let residual = replacement
                        .filter(|(source, _)| *source == equation.residual().index())
                        .map_or(
                            expressions[equation.residual().index() as usize],
                            |(_, target)| target,
                        );
                    target.value_equation(equation.provenance(), residual)?;
                }
                dae::ContinuousOwnerView::Structured { family, .. } => {
                    rebuild_continuous_family(target, family, expressions, domains)?;
                }
            }
        }
        Ok(())
    })?;
    target.initialization(|target| {
        for owner in source.initialization_owners() {
            match owner {
                dae::InitializationOwnerView::Residual { equation, .. } => {
                    target.value_equation(
                        equation.provenance(),
                        expressions[equation.residual().index() as usize],
                    )?;
                }
                dae::InitializationOwnerView::Structured { family, .. } => {
                    rebuild_initialization_family(target, family, expressions, domains)?;
                }
            }
        }
        Ok(())
    })?;
    rebuild_discrete_equations(source, target, expressions, variables)
}

fn rebuild_discrete_equations<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
    variables: &[ReservedVariable<'target>],
) -> Result<(), dae::DaeConstructionError> {
    target.discrete(|target| {
        for index in 0..source.discrete_real_equation_count() {
            let equation = source
                .discrete_real_equation(index)
                .expect("finalized discrete-real equation resolves");
            target.real_equation(equation.provenance(), |target| {
                target.residual(expressions[equation.residual().index() as usize])
            })?;
        }
        for index in 0..source.discrete_assignment_count() {
            let id = source
                .discrete_assignment_id(index)
                .expect("finalized discrete assignment resolves");
            let assignment = source
                .discrete_assignment(id)
                .expect("finalized discrete assignment identity resolves");
            let TargetVariable::DiscreteValue(target_id) =
                variables[assignment.target().index() as usize].identity
            else {
                unreachable!("checked assignment target retains its discrete-value role")
            };
            target.assignment(
                assignment.provenance(),
                target_id,
                expressions[assignment.value().index() as usize],
            )?;
        }
        Ok(())
    })
}

fn rebuild_continuous_family<'target>(
    target: &mut dae::ContinuousEquations<'_, 'target>,
    family: dae::StructuredFamilyView<'_>,
    expressions: &[dae::ExprId<'target>],
    domains: &[RebuiltDomain<'target>],
) -> Result<(), dae::DaeConstructionError> {
    let domain = domains[family.domain().index() as usize].id;
    target.structured_family(
        family.provenance(),
        domain,
        family.scalar_view(),
        |target| rebuild_family_bodies(target, family, expressions),
    )?;
    Ok(())
}

fn rebuild_initialization_family<'target>(
    target: &mut dae::InitializationEquations<'_, 'target>,
    family: dae::StructuredFamilyView<'_>,
    expressions: &[dae::ExprId<'target>],
    domains: &[RebuiltDomain<'target>],
) -> Result<(), dae::DaeConstructionError> {
    let domain = domains[family.domain().index() as usize].id;
    target.structured_family(
        family.provenance(),
        domain,
        family.scalar_view(),
        |target| rebuild_family_bodies(target, family, expressions),
    )?;
    Ok(())
}

fn rebuild_family_bodies<'target>(
    target: &mut dae::StructuredResiduals<'_, 'target>,
    family: dae::StructuredFamilyView<'_>,
    expressions: &[dae::ExprId<'target>],
) -> Result<(), dae::DaeConstructionError> {
    for body in family.bodies().iter() {
        target.body(expressions[body.index() as usize])?;
    }
    Ok(())
}

struct ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    source: dae::DaeView<'source>,
    target: &'borrow mut dae::Expressions<'storage, 'target>,
    types: &'borrow [dae::ValueTypeId<'target>],
    functions: &'borrow [RebuiltFunction<'target>],
    variables: &'borrow [ReservedVariable<'target>],
    domains: &'borrow [RebuiltDomain<'target>],
    conditions: &'borrow [dae::ConditionId<'target>],
    previous: &'borrow [dae::PreviousId<'target>],
    terminals: &'borrow [dae::TerminalId<'target>],
    derivative_definitions: &'borrow [Option<u32>],
    candidate: Option<DirectStateConstraint>,
    rebuilt: &'borrow mut [Option<dae::ExprId<'target>>],
}

#[derive(Clone, Copy)]
struct RebuiltIdentities<'borrow, 'target> {
    types: &'borrow [dae::ValueTypeId<'target>],
    functions: &'borrow [RebuiltFunction<'target>],
    variables: &'borrow [ReservedVariable<'target>],
    domains: &'borrow [RebuiltDomain<'target>],
    conditions: &'borrow [dae::ConditionId<'target>],
    previous: &'borrow [dae::PreviousId<'target>],
    terminals: &'borrow [dae::TerminalId<'target>],
}

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    fn new(
        source: dae::DaeView<'source>,
        target: &'borrow mut dae::Expressions<'storage, 'target>,
        identities: RebuiltIdentities<'borrow, 'target>,
        derivative_definitions: &'borrow [Option<u32>],
        candidate: Option<DirectStateConstraint>,
        rebuilt: &'borrow mut [Option<dae::ExprId<'target>>],
    ) -> Self {
        Self {
            source,
            target,
            types: identities.types,
            functions: identities.functions,
            variables: identities.variables,
            domains: identities.domains,
            conditions: identities.conditions,
            previous: identities.previous,
            terminals: identities.terminals,
            derivative_definitions,
            candidate,
            rebuilt,
        }
    }

    fn rebuild_all(&mut self) -> Result<Vec<dae::ExprId<'target>>, dae::DaeConstructionError> {
        for index in 0..self.source.expression_count() {
            let source = self
                .source
                .expression_id(index)
                .expect("finalized expression ordinal resolves");
            self.rebuild(source)?;
        }
        Ok(self
            .rebuilt
            .iter()
            .map(|expression| expression.expect("every expression was rebuilt"))
            .collect())
    }

    fn rebuild(
        &mut self,
        source_id: dae::ExprId<'source>,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let index = source_id.index() as usize;
        if let Some(rebuilt) = self.rebuilt[index] {
            return Ok(rebuilt);
        }
        let source = self
            .source
            .expression(source_id)
            .expect("finalized expression identity resolves");
        let provenance = source.provenance();
        let value_type = self.types[source.value_type_id().index() as usize];
        let rebuilt = match source.operation() {
            dae::ExpressionOperation::Literal(literal) => {
                self.target.at(provenance).literal(literal.clone())?
            }
            dae::ExpressionOperation::Coordinate(coordinate) => {
                self.rebuild_coordinate(coordinate, provenance)?
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                let operand = self.rebuild(operand)?;
                self.target.at(provenance).unary(operator, operand)?
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                let lhs = self.rebuild(lhs)?;
                let rhs = self.rebuild(rhs)?;
                self.target.at(provenance).binary(operator, lhs, rhs)?
            }
            dae::ExpressionOperation::Conditional(operands) => {
                self.rebuild_conditional(operands, provenance)?
            }
            dae::ExpressionOperation::Array(operands) => {
                if operands.is_empty() {
                    self.target.at(provenance).empty_array(value_type)?
                } else {
                    let elements = self.rebuild_operands(operands)?;
                    self.target.at(provenance).array(elements)?
                }
            }
            dae::ExpressionOperation::Record(fields) => {
                let fields = self.rebuild_operands(fields)?;
                self.target.at(provenance).record(value_type, fields)?
            }
            dae::ExpressionOperation::Field { base, field } => {
                let base = self.rebuild(base)?;
                self.target.at(provenance).field(base, field as usize)?
            }
            dae::ExpressionOperation::Range { start, step, stop } => {
                self.target.at(provenance).range(start, step, stop)?
            }
            dae::ExpressionOperation::Comprehension { domain, body } => {
                let body = self.rebuild(body)?;
                self.target
                    .at(provenance)
                    .comprehension(self.domains[domain.index() as usize].id, body)?
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                let base = self.rebuild(base)?;
                let subscripts = self.rebuild_subscripts(subscripts)?;
                self.target.at(provenance).index(base, subscripts)?
            }
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => {
                let base = self.rebuild(base)?;
                let value = self.rebuild(value)?;
                let subscripts = self.rebuild_subscripts(subscripts)?;
                self.target
                    .at(provenance)
                    .array_update(base, value, subscripts)?
            }
            dae::ExpressionOperation::Builtin { builtin, arguments } => {
                let arguments = self.rebuild_operands(arguments)?;
                self.target.at(provenance).builtin(builtin, arguments)?
            }
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
            } => {
                let arguments = self.rebuild_operands(arguments)?;
                self.target.at(provenance).call(
                    self.functions[function.index() as usize].id,
                    output as usize,
                    arguments,
                )?
            }
            dae::ExpressionOperation::FunctionValue { .. }
            | dae::ExpressionOperation::FunctionFoldParameter { .. }
            | dae::ExpressionOperation::FunctionFoldOutput { .. } => {
                unreachable!("function-owner reconstruction seeds scoped expression identities")
            }
        };
        self.rebuilt[index] = Some(rebuilt);
        Ok(rebuilt)
    }

    fn rebuild_operands(
        &mut self,
        operands: dae::ExpressionOperands<'source>,
    ) -> Result<Vec<dae::ExprId<'target>>, dae::DaeConstructionError> {
        operands
            .iter()
            .map(|operand| self.rebuild(operand))
            .collect()
    }

    fn rebuild_conditional(
        &mut self,
        operands: dae::ExpressionOperands<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let fallback = self.rebuild(
            operands
                .get(operands.len() - 1)
                .expect("checked conditional has a fallback"),
        )?;
        let mut branches = Vec::with_capacity((operands.len() - 1) / 2);
        for index in (0..operands.len() - 1).step_by(2) {
            branches.push((
                self.rebuild(
                    operands
                        .get(index)
                        .expect("checked conditional branch has a condition"),
                )?,
                self.rebuild(
                    operands
                        .get(index + 1)
                        .expect("checked conditional branch has a value"),
                )?,
            ));
        }
        self.target.at(provenance).conditional(branches, fallback)
    }

    fn rebuild_subscripts(
        &mut self,
        subscripts: dae::SubscriptsView<'source>,
    ) -> Result<Vec<dae::Subscript<'target>>, dae::DaeConstructionError> {
        subscripts
            .iter()
            .map(|subscript| match subscript {
                dae::SubscriptView::Index {
                    expression,
                    provenance,
                } => Ok(dae::Subscript::Index {
                    expression: self.rebuild(expression)?,
                    provenance,
                }),
                dae::SubscriptView::Whole { provenance } => {
                    Ok(dae::Subscript::Whole { provenance })
                }
                dae::SubscriptView::Slice {
                    expression,
                    provenance,
                } => Ok(dae::Subscript::Slice {
                    expression: self.rebuild(expression)?,
                    provenance,
                }),
            })
            .collect()
    }

    fn rebuild_coordinate(
        &mut self,
        coordinate: dae::CoordinateView<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        if let (Some(candidate), dae::CoordinateView::Derivative(state)) =
            (self.candidate, coordinate)
            && state.index() == candidate.state
        {
            let generated = dae::DaeProvenance::generated(
                dae::DaeGeneration::IndexReduction,
                candidate.owner.span(),
            )?;
            return self
                .differentiate(
                    self.source
                        .expression_id(candidate.rhs as usize)
                        .expect("candidate RHS resolves"),
                    generated,
                )
                .and_then(|derivative| match derivative {
                    Derivative::Zero => self
                        .target
                        .at(generated)
                        .literal(dae::DaeLiteral::Real(0.0)),
                    Derivative::Expression(expression) => Ok(expression),
                });
        }
        let coordinate = match coordinate {
            dae::CoordinateView::Parameter(id) => {
                match self.variables[id.index() as usize].identity {
                    TargetVariable::Parameter(id) => dae::CoordinateInput::Parameter(id),
                    _ => unreachable!("parameter role is preserved"),
                }
            }
            dae::CoordinateView::Input(id) => match self.variables[id.index() as usize].identity {
                TargetVariable::Input(id) => dae::CoordinateInput::Input(id),
                _ => unreachable!("input role is preserved"),
            },
            dae::CoordinateView::State(id) => match self.variables[id.index() as usize].identity {
                TargetVariable::State(id) => dae::CoordinateInput::State(id),
                TargetVariable::Algebraic(id) => dae::CoordinateInput::Algebraic(id),
                _ => unreachable!("state becomes a state or algebraic"),
            },
            dae::CoordinateView::Derivative(id) => {
                match self.variables[id.index() as usize].identity {
                    TargetVariable::State(id) => dae::CoordinateInput::Derivative(id),
                    _ => unreachable!("only the selected state derivative is substituted"),
                }
            }
            dae::CoordinateView::Algebraic(id) => {
                match self.variables[id.index() as usize].identity {
                    TargetVariable::Algebraic(id) => dae::CoordinateInput::Algebraic(id),
                    _ => unreachable!("algebraic role is preserved"),
                }
            }
            dae::CoordinateView::Time => dae::CoordinateInput::Time,
            discrete @ (dae::CoordinateView::DiscreteReal(_)
            | dae::CoordinateView::DiscreteValue(_)
            | dae::CoordinateView::PreDiscreteReal(_)
            | dae::CoordinateView::PreDiscreteValue(_)) => {
                return self.rebuild_discrete_coordinate(discrete, provenance);
            }
            dae::CoordinateView::Condition(source) => {
                dae::CoordinateInput::Condition(self.conditions[source.index() as usize])
            }
            dae::CoordinateView::Previous(source) => {
                dae::CoordinateInput::Previous(self.previous[source.index() as usize])
            }
            dae::CoordinateView::Terminal(source) => {
                dae::CoordinateInput::Terminal(self.terminals[source.index() as usize])
            }
            dae::CoordinateView::Binder(binder) => {
                return self.target.at(provenance).binder(
                    self.domains[binder.domain().index() as usize].binders
                        [binder.ordinal() as usize],
                );
            }
            dae::CoordinateView::FunctionParameter(parameter) => {
                return self.target.at(provenance).function_parameter(
                    self.functions[parameter.function().index() as usize].parameters
                        [parameter.ordinal() as usize],
                );
            }
            dae::CoordinateView::Delay(_) => {
                unreachable!("delay-owner reconstruction seeds its coordinate identity")
            }
        };
        self.target.at(provenance).coordinate(coordinate)
    }

    fn rebuild_discrete_coordinate(
        &mut self,
        coordinate: dae::CoordinateView<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let coordinate = match coordinate {
            dae::CoordinateView::DiscreteReal(source) => {
                let TargetVariable::DiscreteReal(target) =
                    self.variables[source.index() as usize].identity
                else {
                    unreachable!("discrete-real coordinate retains its variable role")
                };
                dae::CoordinateInput::DiscreteReal(target)
            }
            dae::CoordinateView::DiscreteValue(source) => {
                let TargetVariable::DiscreteValue(target) =
                    self.variables[source.index() as usize].identity
                else {
                    unreachable!("discrete-value coordinate retains its variable role")
                };
                dae::CoordinateInput::DiscreteValue(target)
            }
            dae::CoordinateView::PreDiscreteReal(source) => {
                let TargetVariable::DiscreteReal(target) =
                    self.variables[source.index() as usize].identity
                else {
                    unreachable!("pre(discrete-real) retains its variable role")
                };
                dae::CoordinateInput::PreDiscreteReal(target)
            }
            dae::CoordinateView::PreDiscreteValue(source) => {
                let TargetVariable::DiscreteValue(target) =
                    self.variables[source.index() as usize].identity
                else {
                    unreachable!("pre(discrete-value) retains its variable role")
                };
                dae::CoordinateInput::PreDiscreteValue(target)
            }
            _ => unreachable!("caller passes a discrete coordinate"),
        };
        self.target.at(provenance).coordinate(coordinate)
    }

    fn differentiate(
        &mut self,
        source_id: dae::ExprId<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        self.differentiate_order(source_id, 1, provenance)
    }

    fn differentiate_order(
        &mut self,
        source_id: dae::ExprId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let source = self
            .source
            .expression(source_id)
            .expect("differentiable expression identity resolves");
        match source.operation() {
            dae::ExpressionOperation::Literal(_) => Ok(Derivative::Zero),
            dae::ExpressionOperation::Coordinate(coordinate) => match coordinate {
                dae::CoordinateView::Parameter(_) => Ok(Derivative::Zero),
                dae::CoordinateView::Time if order == 1 => self
                    .target
                    .at(provenance)
                    .literal(dae::DaeLiteral::Real(1.0))
                    .map(Derivative::Expression),
                dae::CoordinateView::Time => Ok(Derivative::Zero),
                dae::CoordinateView::State(state) => {
                    self.differentiate_state(state, order, provenance)
                }
                _ => unreachable!("differentiability preflight rejects this coordinate"),
            },
            dae::ExpressionOperation::Unary { operator, operand } => {
                let derivative = self.differentiate_order(operand, order, provenance)?;
                match (operator, derivative) {
                    (_, Derivative::Zero) => Ok(Derivative::Zero),
                    (dae::UnaryOperator::Plus, derivative) => Ok(derivative),
                    (dae::UnaryOperator::Negate, Derivative::Expression(operand)) => self
                        .target
                        .at(provenance)
                        .unary(dae::UnaryOperator::Negate, operand)
                        .map(Derivative::Expression),
                    (dae::UnaryOperator::Not, _) => {
                        unreachable!("differentiability preflight rejects Boolean negation")
                    }
                }
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                self.differentiate_binary(operator, lhs, rhs, order, provenance)
            }
            _ => unreachable!("differentiability preflight rejects this operation"),
        }
    }

    fn differentiate_state(
        &mut self,
        source_state: dae::StateId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        if let Some(definition) = self.derivative_definitions[source_state.index() as usize] {
            let definition = self
                .source
                .expression_id(definition as usize)
                .expect("explicit derivative definition resolves");
            if order > 1 {
                return self.differentiate_order(definition, order - 1, provenance);
            }
            let definition = self.rebuild(definition)?;
            return self
                .target
                .at(provenance)
                .unary(dae::UnaryOperator::Plus, definition)
                .map(Derivative::Expression);
        }
        let TargetVariable::State(state) = self.variables[source_state.index() as usize].identity
        else {
            unreachable!("candidate RHS cannot refer to the demoted state")
        };
        self.target
            .at(provenance)
            .coordinate(dae::CoordinateInput::Derivative(state))
            .map(Derivative::Expression)
    }

    fn differentiate_binary(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'source>,
        rhs: dae::ExprId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let lhs_derivative = self.differentiate_order(lhs, order, provenance)?;
        let rhs_derivative = self.differentiate_order(rhs, order, provenance)?;
        match operator {
            dae::BinaryOperator::Add | dae::BinaryOperator::Subtract => {
                self.combine_sum(operator, lhs_derivative, rhs_derivative, provenance)
            }
            dae::BinaryOperator::Multiply if order == 1 => {
                let lhs_value = self.rebuild(lhs)?;
                let rhs_value = self.rebuild(rhs)?;
                let left = self.multiply(lhs_derivative, rhs_value, provenance)?;
                let right = self.multiply(rhs_derivative, lhs_value, provenance)?;
                self.combine_sum(dae::BinaryOperator::Add, left, right, provenance)
            }
            dae::BinaryOperator::Multiply if order == 2 => self.differentiate_second_product(
                lhs,
                rhs,
                lhs_derivative,
                rhs_derivative,
                provenance,
            ),
            dae::BinaryOperator::Divide if order == 1 => {
                let lhs_value = self.rebuild(lhs)?;
                let rhs_value = self.rebuild(rhs)?;
                let left = self.multiply(lhs_derivative, rhs_value, provenance)?;
                let right = self.multiply(rhs_derivative, lhs_value, provenance)?;
                let numerator =
                    self.combine_sum(dae::BinaryOperator::Subtract, left, right, provenance)?;
                let Derivative::Expression(numerator) = numerator else {
                    return Ok(Derivative::Zero);
                };
                let denominator = self.target.at(provenance).binary(
                    dae::BinaryOperator::Multiply,
                    rhs_value,
                    rhs_value,
                )?;
                self.target
                    .at(provenance)
                    .binary(dae::BinaryOperator::Divide, numerator, denominator)
                    .map(Derivative::Expression)
            }
            _ => unreachable!("differentiability preflight rejects this binary operator"),
        }
    }

    fn differentiate_second_product(
        &mut self,
        lhs: dae::ExprId<'source>,
        rhs: dae::ExprId<'source>,
        lhs_second: Derivative<'target>,
        rhs_second: Derivative<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let lhs_value = self.rebuild(lhs)?;
        let rhs_value = self.rebuild(rhs)?;
        let lhs_first = self.differentiate_order(lhs, 1, provenance)?;
        let rhs_first = self.differentiate_order(rhs, 1, provenance)?;
        let left = self.multiply(lhs_second, rhs_value, provenance)?;
        let right = self.multiply(rhs_second, lhs_value, provenance)?;
        let middle = self.multiply_derivatives(lhs_first, rhs_first, provenance)?;
        let outer = self.combine_sum(dae::BinaryOperator::Add, left, right, provenance)?;
        self.combine_sum(dae::BinaryOperator::Add, outer, middle, provenance)
    }

    fn multiply_derivatives(
        &mut self,
        lhs: Derivative<'target>,
        rhs: Derivative<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let (Derivative::Expression(lhs), Derivative::Expression(rhs)) = (lhs, rhs) else {
            return Ok(Derivative::Zero);
        };
        let product = self
            .target
            .at(provenance)
            .binary(dae::BinaryOperator::Multiply, lhs, rhs)?;
        let two = self
            .target
            .at(provenance)
            .literal(dae::DaeLiteral::Real(2.0))?;
        self.target
            .at(provenance)
            .binary(dae::BinaryOperator::Multiply, two, product)
            .map(Derivative::Expression)
    }

    fn materialize_derivative(
        &mut self,
        derivative: Derivative<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        match derivative {
            Derivative::Zero => self
                .target
                .at(provenance)
                .literal(dae::DaeLiteral::Real(0.0)),
            Derivative::Expression(expression) => self
                .target
                .at(provenance)
                .unary(dae::UnaryOperator::Plus, expression),
        }
    }

    fn multiply(
        &mut self,
        derivative: Derivative<'target>,
        value: dae::ExprId<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let Derivative::Expression(derivative) = derivative else {
            return Ok(Derivative::Zero);
        };
        self.target
            .at(provenance)
            .binary(dae::BinaryOperator::Multiply, derivative, value)
            .map(Derivative::Expression)
    }

    fn combine_sum(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: Derivative<'target>,
        rhs: Derivative<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        match (lhs, rhs) {
            (Derivative::Zero, Derivative::Zero) => Ok(Derivative::Zero),
            (Derivative::Expression(expression), Derivative::Zero) => {
                Ok(Derivative::Expression(expression))
            }
            (Derivative::Zero, Derivative::Expression(expression))
                if operator == dae::BinaryOperator::Add =>
            {
                Ok(Derivative::Expression(expression))
            }
            (Derivative::Zero, Derivative::Expression(expression)) => self
                .target
                .at(provenance)
                .unary(dae::UnaryOperator::Negate, expression)
                .map(Derivative::Expression),
            (Derivative::Expression(lhs), Derivative::Expression(rhs)) => self
                .target
                .at(provenance)
                .binary(operator, lhs, rhs)
                .map(Derivative::Expression),
        }
    }
}

enum Derivative<'dae> {
    Zero,
    Expression(dae::ExprId<'dae>),
}
