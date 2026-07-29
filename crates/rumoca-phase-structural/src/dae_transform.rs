//! Constructor-only structural DAE-to-DAE lowering.
//!
//! Regular systems remain borrowed. A singular system is rebuilt only when a
//! scalar state is directly defined by a differentiable constraint. The
//! replacement DAE demotes that state and substitutes the exact symbolic
//! derivative of its definition at every derivative occurrence.

use rumoca_core::StateSelect;
use rumoca_ir_dae as dae;

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
        let Some(rebuilt) = rebuild_with_state_demotion(model, candidate)? else {
            continue;
        };
        if rebuilt.inspect(|view| sort(view).map(|_| ())).is_ok() {
            return Ok(PreparedDae::Transformed {
                dae: Box::new(rebuilt),
                manifold: Box::new([]),
            });
        }
    }
    for constraint in model.inspect(holonomic_constraints) {
        let Some((rebuilt, manifold)) = rebuild_holonomic_constraint(model, constraint)? else {
            continue;
        };
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
) -> Result<Option<(dae::Dae, Vec<u32>)>, StructuralError> {
    let supported = model.inspect(supports_common_reconstruction);
    if !supported {
        return Ok(None);
    }
    let mut manifold = Vec::with_capacity(2);
    let rebuilt = model.inspect(|source| {
        dae::Dae::construct(model.source_map().clone(), |target| {
            let types = rebuild_types(source, target)?;
            let domains = rebuild_domains(source, target)?;
            let mut variables = reserve_variables(source, target, &types, None)?;
            let derivative_definitions = explicit_derivative_definitions(source);
            let (expressions, replacement) = target.expressions(|expressions| {
                let mut rebuilder = ExpressionRebuilder::new(
                    source,
                    expressions,
                    &types,
                    &variables,
                    &domains,
                    &derivative_definitions,
                    None,
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
            rebuild_equations(
                source,
                target,
                &expressions,
                &variables,
                &domains,
                Some((constraint.residual, replacement)),
            )
        })
    });
    rebuilt
        .map(|dae| Some((dae, manifold)))
        .map_err(|error| construction_failure(model, error))
}

fn rebuild_with_state_demotion(
    model: &dae::Dae,
    candidate: DirectStateConstraint,
) -> Result<Option<dae::Dae>, StructuralError> {
    let supported = model.inspect(|view| supports_reconstruction(view, candidate));
    if !supported {
        return Ok(None);
    }
    let rebuilt = model.inspect(|source| {
        dae::Dae::construct(model.source_map().clone(), |target| {
            let types = rebuild_types(source, target)?;
            let domains = rebuild_domains(source, target)?;
            let mut variables = reserve_variables(source, target, &types, Some(candidate.state))?;
            let derivative_definitions = explicit_derivative_definitions(source);
            let expressions = target.expressions(|expressions| {
                let mut rebuilder = ExpressionRebuilder::new(
                    source,
                    expressions,
                    &types,
                    &variables,
                    &domains,
                    &derivative_definitions,
                    Some(candidate),
                );
                rebuilder.rebuild_all()
            })?;
            define_variables(source, target, &expressions, &mut variables)?;
            rebuild_equations(source, target, &expressions, &variables, &domains, None)
        })
    });
    rebuilt
        .map(Some)
        .map_err(|error| construction_failure(model, error))
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

fn supports_reconstruction(view: dae::DaeView<'_>, candidate: DirectStateConstraint) -> bool {
    if !supports_common_reconstruction(view) {
        return false;
    }
    (0..view.expression_count()).all(|index| {
        let id = view
            .expression_id(index)
            .expect("finalized expression ordinal resolves");
        match view
            .expression(id)
            .expect("finalized expression identity resolves")
            .operation()
        {
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state))
                if state.index() == candidate.state =>
            {
                is_differentiable(
                    view,
                    view.expression_id(candidate.rhs as usize)
                        .expect("candidate RHS resolves"),
                    state,
                    &mut vec![false; view.expression_count()],
                )
            }
            _ => true,
        }
    })
}

fn supports_common_reconstruction(view: dae::DaeView<'_>) -> bool {
    let unsupported_owner = view.function_count() != 0
        || view.relation_count() != 0
        || view.condition_count() != 0
        || view.root_count() != 0
        || view.time_event_count() != 0
        || view.event_action_count() != 0
        || view.clock_count() != 0
        || view.clock_ownership_count() != 0
        || view.previous_value_count() != 0
        || view.terminal_count() != 0
        || view.delay_count() != 0;
    if unsupported_owner {
        return false;
    }
    if view
        .variables()
        .any(|(_, variable)| variable.value_type().is_record())
    {
        return false;
    }
    if (0..view.value_type_count()).any(|index| {
        view.value_type_id(index)
            .and_then(|id| view.value_type(id))
            .is_none_or(dae::ValueType::is_record)
    }) {
        return false;
    }
    (0..view.expression_count()).all(|index| {
        let Some(id) = view.expression_id(index) else {
            return false;
        };
        let Some(expression) = view.expression(id) else {
            return false;
        };
        if expression.function_scope().is_some() {
            return false;
        }
        match expression.operation() {
            dae::ExpressionOperation::Literal(_)
            | dae::ExpressionOperation::Unary { .. }
            | dae::ExpressionOperation::Binary { .. }
            | dae::ExpressionOperation::Conditional(_)
            | dae::ExpressionOperation::Array(_)
            | dae::ExpressionOperation::Range { .. }
            | dae::ExpressionOperation::Comprehension { .. }
            | dae::ExpressionOperation::Index { .. }
            | dae::ExpressionOperation::ArrayUpdate { .. }
            | dae::ExpressionOperation::Builtin { .. } => true,
            dae::ExpressionOperation::Coordinate(coordinate) => matches!(
                coordinate,
                dae::CoordinateView::Parameter(_)
                    | dae::CoordinateView::Input(_)
                    | dae::CoordinateView::State(_)
                    | dae::CoordinateView::Derivative(_)
                    | dae::CoordinateView::Algebraic(_)
                    | dae::CoordinateView::DiscreteReal(_)
                    | dae::CoordinateView::DiscreteValue(_)
                    | dae::CoordinateView::PreDiscreteReal(_)
                    | dae::CoordinateView::PreDiscreteValue(_)
                    | dae::CoordinateView::Time
                    | dae::CoordinateView::Binder(_)
            ),
            _ => false,
        }
    })
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
        (0..source.value_type_count())
            .map(|index| {
                let source_id = source
                    .value_type_id(index)
                    .expect("finalized value type ordinal resolves");
                let value_type = source
                    .value_type(source_id)
                    .expect("finalized value type identity resolves")
                    .clone();
                let provenance = source
                    .value_type_provenance(source_id)
                    .expect("finalized value type has provenance");
                match source.effective_flat_type(source_id) {
                    Some(flat_type) => types.intern(flat_type, value_type, provenance),
                    None => types.derived(value_type, provenance),
                }
            })
            .collect()
    })
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
    variables: &'borrow [ReservedVariable<'target>],
    domains: &'borrow [RebuiltDomain<'target>],
    derivative_definitions: &'borrow [Option<u32>],
    candidate: Option<DirectStateConstraint>,
    rebuilt: Vec<Option<dae::ExprId<'target>>>,
    visiting: Vec<bool>,
}

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    fn new(
        source: dae::DaeView<'source>,
        target: &'borrow mut dae::Expressions<'storage, 'target>,
        types: &'borrow [dae::ValueTypeId<'target>],
        variables: &'borrow [ReservedVariable<'target>],
        domains: &'borrow [RebuiltDomain<'target>],
        derivative_definitions: &'borrow [Option<u32>],
        candidate: Option<DirectStateConstraint>,
    ) -> Self {
        Self {
            source,
            target,
            types,
            variables,
            domains,
            derivative_definitions,
            candidate,
            rebuilt: vec![None; source.expression_count()],
            visiting: vec![false; source.expression_count()],
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
        assert!(!self.visiting[index], "checked expression graph is acyclic");
        self.visiting[index] = true;
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
            _ => unreachable!("reconstruction preflight rejects this expression operation"),
        };
        self.visiting[index] = false;
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
            dae::CoordinateView::Binder(binder) => {
                return self.target.at(provenance).binder(
                    self.domains[binder.domain().index() as usize].binders
                        [binder.ordinal() as usize],
                );
            }
            _ => unreachable!("reconstruction preflight rejects this coordinate"),
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

#[cfg(test)]
mod tests {
    use rumoca_core::{SourceMap, Span, TypeId, VarName};

    use super::*;

    fn source_provenance(
        source: rumoca_core::SourceId,
        text: &str,
        needle: &str,
    ) -> dae::DaeProvenance {
        let start = text.find(needle).expect("fixture snippet exists");
        dae::DaeProvenance::source(Span::from_offsets(source, start, start + needle.len()))
            .expect("fixture span is source-backed")
    }

    #[derive(Clone, Copy)]
    struct FixtureVariables<'dae> {
        p: dae::ParameterId<'dae>,
        x: dae::StateId<'dae>,
        y: dae::StateId<'dae>,
        a: dae::AlgebraicId<'dae>,
        z: Option<dae::AlgebraicId<'dae>>,
        d: Option<dae::DiscreteRealId<'dae>>,
        b: Option<dae::DiscreteValueId<'dae>>,
    }

    #[derive(Clone, Copy, Default)]
    struct FixtureFeatures {
        family: bool,
        discrete: bool,
    }

    #[derive(Clone, Copy)]
    struct FixtureSpans {
        constraint: dae::DaeProvenance,
        derivative_y: dae::DaeProvenance,
        derivative_x: dae::DaeProvenance,
    }

    fn fixture_types<'dae>(
        model: &mut dae::DaeConstruction<'dae>,
        declaration: dae::DaeProvenance,
    ) -> Result<
        (
            dae::ValueTypeId<'dae>,
            dae::ValueTypeId<'dae>,
            dae::ValueTypeId<'dae>,
        ),
        dae::DaeConstructionError,
    > {
        model.types(|types| {
            Ok((
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    declaration,
                )?,
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::array(dae::ScalarType::Real, [2]),
                    declaration,
                )?,
                types.intern(
                    TypeId::new(2),
                    dae::ValueType::scalar(dae::ScalarType::Boolean),
                    declaration,
                )?,
            ))
        })
    }

    fn fixture_domain<'dae>(
        model: &mut dae::DaeConstruction<'dae>,
        owner: Option<dae::DaeProvenance>,
    ) -> Result<Option<dae::DomainId<'dae>>, dae::DaeConstructionError> {
        owner
            .map(|owner| {
                model.domains(|domains| {
                    domains.structured(
                        rumoca_core::StructuredIndexDomain {
                            binders: vec![rumoca_core::StructuredIndexBinder {
                                id: 0,
                                display_name: "i".to_owned(),
                                lower: 1,
                                upper: 2,
                                step: 1,
                            }],
                        },
                        owner,
                    )
                })
            })
            .transpose()
    }

    fn fixture_variables<'dae>(
        model: &mut dae::DaeConstruction<'dae>,
        real: dae::ValueTypeId<'dae>,
        vector: dae::ValueTypeId<'dae>,
        boolean: dae::ValueTypeId<'dae>,
        declaration: dae::DaeProvenance,
        features: FixtureFeatures,
    ) -> Result<FixtureVariables<'dae>, dae::DaeConstructionError> {
        model.variables(|variables| {
            let z = if features.family {
                Some(variables.algebraic(
                    VarName::new("z"),
                    vector,
                    declaration,
                    dae::VariableAttributes::default(),
                )?)
            } else {
                None
            };
            let d = if features.discrete {
                Some(variables.discrete_real(
                    VarName::new("d"),
                    real,
                    declaration,
                    dae::VariableAttributes::default(),
                )?)
            } else {
                None
            };
            let b = if features.discrete {
                Some(variables.discrete_value(
                    VarName::new("b"),
                    boolean,
                    declaration,
                    dae::VariableAttributes::default(),
                )?)
            } else {
                None
            };
            Ok(FixtureVariables {
                p: variables.parameter(
                    VarName::new("p"),
                    real,
                    declaration,
                    dae::VariableAttributes::default(),
                )?,
                x: variables.state(
                    VarName::new("x"),
                    real,
                    declaration,
                    dae::VariableAttributes::default(),
                )?,
                y: variables.state(
                    VarName::new("y"),
                    real,
                    declaration,
                    dae::VariableAttributes::default(),
                )?,
                a: variables.algebraic(
                    VarName::new("a"),
                    real,
                    declaration,
                    dae::VariableAttributes::default(),
                )?,
                z,
                d,
                b,
            })
        })
    }

    fn fixture_family_residual<'dae>(
        model: &mut dae::DaeConstruction<'dae>,
        domain: Option<dae::DomainId<'dae>>,
        owner: Option<dae::DaeProvenance>,
        z: Option<dae::AlgebraicId<'dae>>,
    ) -> Result<Option<dae::ExprId<'dae>>, dae::DaeConstructionError> {
        let (Some(domain), Some(owner), Some(z)) = (domain, owner, z) else {
            return Ok(None);
        };
        let binder = model.domains(|domains| domains.binder(domain, 0, owner))?;
        model
            .expressions(|expressions| {
                let aggregate = expressions
                    .at(owner)
                    .coordinate(dae::CoordinateInput::Algebraic(z))?;
                let index = expressions.at(owner).binder(binder)?;
                let value = expressions.at(owner).index(
                    aggregate,
                    [dae::Subscript::Index {
                        expression: index,
                        provenance: owner,
                    }],
                )?;
                expressions
                    .at(owner)
                    .binary(dae::BinaryOperator::Subtract, value, index)
            })
            .map(Some)
    }

    fn insert_fixture_equations<'dae>(
        model: &mut dae::DaeConstruction<'dae>,
        spans: FixtureSpans,
        residuals: [dae::ExprId<'dae>; 3],
        family: Option<(dae::DomainId<'dae>, dae::DaeProvenance, dae::ExprId<'dae>)>,
        discrete: Option<FixtureDiscrete<'dae>>,
    ) -> Result<(), dae::DaeConstructionError> {
        model.continuous(|continuous| {
            continuous.value_equation(spans.constraint, residuals[0])?;
            continuous.value_equation(spans.derivative_y, residuals[1])?;
            continuous.value_equation(spans.derivative_x, residuals[2])?;
            if let Some((domain, owner, residual)) = family {
                continuous.structured_family(
                    owner,
                    domain,
                    rumoca_core::ComprehensionScalarView::BinderSubstitution,
                    |family| family.body(residual),
                )?;
            }
            Ok(())
        })?;
        if let Some((domain, owner, residual)) = family {
            model.initialization(|initialization| {
                initialization.structured_family(
                    owner,
                    domain,
                    rumoca_core::ComprehensionScalarView::BinderSubstitution,
                    |family| family.body(residual),
                )?;
                Ok(())
            })?;
        }
        insert_fixture_discrete(model, discrete)?;
        Ok(())
    }

    fn insert_fixture_discrete<'dae>(
        model: &mut dae::DaeConstruction<'dae>,
        discrete: Option<FixtureDiscrete<'dae>>,
    ) -> Result<(), dae::DaeConstructionError> {
        let Some(discrete) = discrete else {
            return Ok(());
        };
        model.discrete(|equations| {
            equations.real_equation(discrete.real_owner, |equation| {
                equation.residual(discrete.real_residual)
            })?;
            equations.assignment(discrete.value_owner, discrete.value_target, discrete.value)?;
            Ok(())
        })
    }

    #[derive(Clone, Copy)]
    struct FixtureDiscrete<'dae> {
        real_owner: dae::DaeProvenance,
        real_residual: dae::ExprId<'dae>,
        value_owner: dae::DaeProvenance,
        value_target: dae::DiscreteValueId<'dae>,
        value: dae::ExprId<'dae>,
    }

    fn fixture_discrete<'dae>(
        model: &mut dae::DaeConstruction<'dae>,
        variables: FixtureVariables<'dae>,
        owners: Option<(dae::DaeProvenance, dae::DaeProvenance)>,
    ) -> Result<Option<FixtureDiscrete<'dae>>, dae::DaeConstructionError> {
        let (Some(d), Some(b), Some((real_owner, value_owner))) =
            (variables.d, variables.b, owners)
        else {
            return Ok(None);
        };
        model.expressions(|expressions| {
            let d = expressions
                .at(real_owner)
                .coordinate(dae::CoordinateInput::DiscreteReal(d))?;
            let two = expressions
                .at(real_owner)
                .literal(dae::DaeLiteral::Real(2.0))?;
            let real_residual =
                expressions
                    .at(real_owner)
                    .binary(dae::BinaryOperator::Subtract, d, two)?;
            let value = expressions
                .at(value_owner)
                .literal(dae::DaeLiteral::Boolean(true))?;
            Ok(Some(FixtureDiscrete {
                real_owner,
                real_residual,
                value_owner,
                value_target: b,
                value,
            }))
        })
    }

    fn constrained_state_model(
        nonlinear_constraint: bool,
        features: FixtureFeatures,
    ) -> (dae::Dae, dae::DaeProvenance) {
        let rhs = if nonlinear_constraint {
            "sin(y)"
        } else {
            "p*y"
        };
        let family_declaration = if features.family { " Real z[2];" } else { "" };
        let family_equation = if features.family {
            " for i in 1:2 loop z[i] = i; end for;"
        } else {
            ""
        };
        let discrete_declaration = if features.discrete {
            " discrete Real d; Boolean b;"
        } else {
            ""
        };
        let discrete_equations = if features.discrete {
            " d = 2; b = true;"
        } else {
            ""
        };
        let text = format!(
            "parameter Real p; Real x; Real y; Real a;{family_declaration}{discrete_declaration} equation x = {rhs}; der(y) = a; der(x) = 1;{family_equation}{discrete_equations}"
        );
        let mut sources = SourceMap::new();
        let source = sources.add("checked_index_reduction.mo", &text);
        let declaration = source_provenance(source, &text, "parameter Real p");
        let constraint = source_provenance(source, &text, &format!("x = {rhs}"));
        let derivative_y = source_provenance(source, &text, "der(y) = a");
        let derivative_x = source_provenance(source, &text, "der(x) = 1");
        let family_owner = features
            .family
            .then(|| source_provenance(source, &text, "for i in 1:2 loop z[i] = i"));
        let discrete_owners = features.discrete.then(|| {
            (
                source_provenance(source, &text, "d = 2"),
                source_provenance(source, &text, "b = true"),
            )
        });
        let model = dae::Dae::construct(sources, |model| {
            let (real, vector, boolean) = fixture_types(model, declaration)?;
            let domain = fixture_domain(model, family_owner)?;
            let variables = fixture_variables(model, real, vector, boolean, declaration, features)?;
            let spans = FixtureSpans {
                constraint,
                derivative_y,
                derivative_x,
            };
            let residuals = model.expressions(|expressions| {
                fixture_residuals(expressions, variables, spans, nonlinear_constraint)
            })?;
            let family_residual =
                fixture_family_residual(model, domain, family_owner, variables.z)?;
            let family = domain
                .zip(family_owner)
                .zip(family_residual)
                .map(|((domain, owner), residual)| (domain, owner, residual));
            let discrete = fixture_discrete(model, variables, discrete_owners)?;
            insert_fixture_equations(model, spans, residuals, family, discrete)
        })
        .expect("fixture DAE is valid");
        (model, constraint)
    }

    fn fixture_residuals<'dae>(
        expressions: &mut dae::Expressions<'_, 'dae>,
        variables: FixtureVariables<'dae>,
        spans: FixtureSpans,
        nonlinear_constraint: bool,
    ) -> Result<[dae::ExprId<'dae>; 3], dae::DaeConstructionError> {
        let x_value = expressions
            .at(spans.constraint)
            .coordinate(dae::CoordinateInput::State(variables.x))?;
        let y_value = expressions
            .at(spans.constraint)
            .coordinate(dae::CoordinateInput::State(variables.y))?;
        let constraint_rhs = fixture_constraint_rhs(
            expressions,
            variables.p,
            y_value,
            spans.constraint,
            nonlinear_constraint,
        )?;
        let constraint = expressions.at(spans.constraint).binary(
            dae::BinaryOperator::Subtract,
            x_value,
            constraint_rhs,
        )?;
        let y_derivative = expressions
            .at(spans.derivative_y)
            .coordinate(dae::CoordinateInput::Derivative(variables.y))?;
        let a_value = expressions
            .at(spans.derivative_y)
            .coordinate(dae::CoordinateInput::Algebraic(variables.a))?;
        let derivative_y = expressions.at(spans.derivative_y).binary(
            dae::BinaryOperator::Subtract,
            y_derivative,
            a_value,
        )?;
        let x_derivative = expressions
            .at(spans.derivative_x)
            .coordinate(dae::CoordinateInput::Derivative(variables.x))?;
        let one = expressions
            .at(spans.derivative_x)
            .literal(dae::DaeLiteral::Real(1.0))?;
        let derivative_x = expressions.at(spans.derivative_x).binary(
            dae::BinaryOperator::Subtract,
            x_derivative,
            one,
        )?;
        Ok([constraint, derivative_y, derivative_x])
    }

    fn fixture_constraint_rhs<'dae>(
        expressions: &mut dae::Expressions<'_, 'dae>,
        parameter: dae::ParameterId<'dae>,
        state: dae::ExprId<'dae>,
        provenance: dae::DaeProvenance,
        nonlinear: bool,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        if nonlinear {
            return expressions
                .at(provenance)
                .builtin(dae::PureBuiltin::Sin, [state]);
        }
        let parameter = expressions
            .at(provenance)
            .coordinate(dae::CoordinateInput::Parameter(parameter))?;
        expressions
            .at(provenance)
            .binary(dae::BinaryOperator::Multiply, parameter, state)
    }

    #[test]
    fn direct_state_demotion_reconstructs_a_finalized_dae_with_exact_provenance() {
        let (model, constraint) = constrained_state_model(false, FixtureFeatures::default());
        let prepared = prepare_for_solve(&model).expect("index-one constraint is reducible");
        assert!(matches!(prepared, PreparedDae::Transformed { .. }));

        prepared.as_dae().inspect(|view| {
            let x = view
                .variables()
                .find(|(_, variable)| variable.name().as_str() == "x")
                .map(|(_, variable)| variable)
                .expect("x declaration survives reconstruction");
            assert_eq!(x.role(), dae::VariableRole::Algebraic);
            assert!(sort(view).is_ok(), "replacement DAE has a perfect matching");

            let generated = (0..view.expression_count())
                .filter_map(|index| view.expression_id(index))
                .filter_map(|id| view.expression(id))
                .filter(|expression| {
                    expression.provenance().origin()
                        == dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::IndexReduction)
                })
                .collect::<Vec<_>>();
            assert!(!generated.is_empty());
            assert!(
                generated
                    .iter()
                    .all(|expression| expression.provenance().span() == constraint.span())
            );
            assert!((0..view.expression_count())
                .filter_map(|index| view.expression_id(index))
                .filter_map(|id| view.expression(id))
                .all(|expression| {
                    !matches!(
                        expression.operation(),
                        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state))
                            if view
                                .variable(state.into())
                                .is_some_and(|variable| variable.name().as_str() == "x")
                    )
                }));
        });
    }

    #[test]
    fn state_demotion_preserves_structured_families_and_binder_provenance() {
        let (model, _) = constrained_state_model(
            false,
            FixtureFeatures {
                family: true,
                ..FixtureFeatures::default()
            },
        );
        let prepared = prepare_for_solve(&model).expect("structured companion family is preserved");
        let transformed = match prepared {
            PreparedDae::Transformed { dae, .. } => dae,
            PreparedDae::Borrowed(_) => panic!("singular fixture requires state demotion"),
        };
        transformed.inspect(|view| {
            assert_eq!(view.domain_count(), 1);
            assert_eq!(view.continuous_family_count(), 1);
            assert_eq!(view.initialization_family_count(), 1);
            let family = view
                .continuous_family(0)
                .expect("structured family survives reconstruction");
            assert_eq!(family.scalar_rows(), 2);
            assert_eq!(
                view.source_text(family.provenance()),
                Some("for i in 1:2 loop z[i] = i")
            );
            let body = family
                .bodies()
                .get(0)
                .expect("structured family retains its body");
            let mut has_binder = false;
            dae::for_each_expression(view, body, |_, expression| {
                has_binder |= matches!(
                    expression.operation(),
                    dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder))
                        if binder.domain() == family.domain()
                );
            });
            assert!(has_binder);
            let initial = view
                .initialization_family(0)
                .expect("initialization family survives reconstruction");
            assert_eq!(initial.domain(), family.domain());
            assert_eq!(initial.bodies().len(), family.bodies().len());
            assert_eq!(initial.provenance(), family.provenance());
            assert!(
                sort(view).is_ok(),
                "replacement DAE remains structurally square"
            );
        });
    }

    #[test]
    fn state_demotion_preserves_discrete_equations_and_exact_provenance() {
        let (model, _) = constrained_state_model(
            false,
            FixtureFeatures {
                discrete: true,
                ..FixtureFeatures::default()
            },
        );
        let prepared = prepare_for_solve(&model).expect("discrete companion equations survive");
        let transformed = match prepared {
            PreparedDae::Transformed { dae, .. } => dae,
            PreparedDae::Borrowed(_) => panic!("singular fixture requires state demotion"),
        };
        transformed.inspect(|view| {
            assert_eq!(view.discrete_real_equation_count(), 1);
            assert_eq!(view.discrete_assignment_count(), 1);
            let real = view
                .discrete_real_equation(0)
                .expect("discrete-real equation survives reconstruction");
            assert_eq!(view.source_text(real.provenance()), Some("d = 2"));
            let assignment_id = view
                .discrete_assignment_id(0)
                .expect("discrete assignment identity resolves");
            let assignment = view
                .discrete_assignment(assignment_id)
                .expect("discrete assignment survives reconstruction");
            assert_eq!(view.source_text(assignment.provenance()), Some("b = true"));
            assert!(matches!(
                view.variable(assignment.target().into())
                    .expect("assignment target resolves")
                    .role(),
                dae::VariableRole::DiscreteValue
            ));
            assert!(
                sort(view).is_ok(),
                "replacement DAE remains structurally square"
            );
        });
    }

    #[test]
    fn unsupported_symbolic_derivative_preserves_the_original_singular_error() {
        let (model, _) = constrained_state_model(true, FixtureFeatures::default());
        let error = match prepare_for_solve(&model) {
            Ok(_) => panic!("unsupported differentiation must not guess a replacement"),
            Err(error) => error,
        };
        assert!(matches!(error, StructuralError::Singular { .. }));
    }
}
