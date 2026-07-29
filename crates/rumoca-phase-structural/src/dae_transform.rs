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
    Transformed(Box<dae::Dae>),
}

impl PreparedDae<'_> {
    pub fn as_dae(&self) -> &dae::Dae {
        match self {
            Self::Borrowed(dae) => dae,
            Self::Transformed(dae) => dae,
        }
    }
}

#[derive(Clone, Copy)]
struct DirectStateConstraint {
    state: u32,
    rhs: u32,
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
            return Ok(PreparedDae::Transformed(Box::new(rebuilt)));
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
            let mut variables = reserve_variables(source, target, &types, candidate.state)?;
            let derivative_definitions = explicit_derivative_definitions(source);
            let expressions = target.expressions(|expressions| {
                let mut rebuilder = ExpressionRebuilder::new(
                    source,
                    expressions,
                    &variables,
                    &derivative_definitions,
                    candidate,
                );
                rebuilder.rebuild_all()
            })?;
            define_variables(source, target, &expressions, &mut variables)?;
            rebuild_equations(source, target, &expressions)
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
    let unsupported_owner = view.function_count() != 0
        || view.domain_count() != 0
        || view.continuous_family_count() != 0
        || view.initialization_family_count() != 0
        || view.discrete_real_equation_count() != 0
        || view.discrete_assignment_count() != 0
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
    if view.variables().any(|(_, variable)| {
        variable.value_type().is_record()
            || matches!(
                variable.role(),
                dae::VariableRole::DiscreteReal | dae::VariableRole::DiscreteValue
            )
    }) {
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
        if expression.binder_domain().is_some() || expression.function_scope().is_some() {
            return false;
        }
        match expression.operation() {
            dae::ExpressionOperation::Literal(_)
            | dae::ExpressionOperation::Unary { .. }
            | dae::ExpressionOperation::Binary { .. } => true,
            dae::ExpressionOperation::Coordinate(coordinate) => match coordinate {
                dae::CoordinateView::Parameter(_)
                | dae::CoordinateView::Input(_)
                | dae::CoordinateView::State(_)
                | dae::CoordinateView::Algebraic(_)
                | dae::CoordinateView::Time => true,
                dae::CoordinateView::Derivative(state) => {
                    state.index() != candidate.state
                        || is_differentiable(
                            view,
                            view.expression_id(candidate.rhs as usize)
                                .expect("candidate RHS resolves"),
                            state,
                            &mut vec![false; view.expression_count()],
                        )
                }
                _ => false,
            },
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

enum TargetVariable<'dae> {
    Parameter(dae::ParameterId<'dae>),
    Input(dae::InputId<'dae>),
    State(dae::StateId<'dae>),
    Algebraic(dae::AlgebraicId<'dae>),
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
    demoted: u32,
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
    demoted: u32,
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
        dae::VariableRole::State if variable.id().index() == demoted => {
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
        dae::VariableRole::DiscreteReal | dae::VariableRole::DiscreteValue => {
            unreachable!("reconstruction preflight rejects discrete variables")
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
) -> Result<(), dae::DaeConstructionError> {
    target.continuous(|target| {
        for owner in source.continuous_owners() {
            let dae::ContinuousOwnerView::Residual { equation, .. } = owner else {
                unreachable!("reconstruction preflight rejects structured families")
            };
            target.value_equation(
                equation.provenance(),
                expressions[equation.residual().index() as usize],
            )?;
        }
        Ok(())
    })?;
    target.initialization(|target| {
        for owner in source.initialization_owners() {
            let dae::InitializationOwnerView::Residual { equation, .. } = owner else {
                unreachable!("reconstruction preflight rejects structured families")
            };
            target.value_equation(
                equation.provenance(),
                expressions[equation.residual().index() as usize],
            )?;
        }
        Ok(())
    })
}

struct ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    source: dae::DaeView<'source>,
    target: &'borrow mut dae::Expressions<'storage, 'target>,
    variables: &'borrow [ReservedVariable<'target>],
    derivative_definitions: &'borrow [Option<u32>],
    candidate: DirectStateConstraint,
    rebuilt: Vec<Option<dae::ExprId<'target>>>,
    visiting: Vec<bool>,
}

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    fn new(
        source: dae::DaeView<'source>,
        target: &'borrow mut dae::Expressions<'storage, 'target>,
        variables: &'borrow [ReservedVariable<'target>],
        derivative_definitions: &'borrow [Option<u32>],
        candidate: DirectStateConstraint,
    ) -> Self {
        Self {
            source,
            target,
            variables,
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
            _ => unreachable!("reconstruction preflight rejects this expression operation"),
        };
        self.visiting[index] = false;
        self.rebuilt[index] = Some(rebuilt);
        Ok(rebuilt)
    }

    fn rebuild_coordinate(
        &mut self,
        coordinate: dae::CoordinateView<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        if matches!(
            coordinate,
            dae::CoordinateView::Derivative(state) if state.index() == self.candidate.state
        ) {
            let generated = dae::DaeProvenance::generated(
                dae::DaeGeneration::IndexReduction,
                self.candidate.owner.span(),
            )?;
            return self
                .differentiate(
                    self.source
                        .expression_id(self.candidate.rhs as usize)
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
            _ => unreachable!("reconstruction preflight rejects this coordinate"),
        };
        self.target.at(provenance).coordinate(coordinate)
    }

    fn differentiate(
        &mut self,
        source_id: dae::ExprId<'source>,
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
                dae::CoordinateView::Time => self
                    .target
                    .at(provenance)
                    .literal(dae::DaeLiteral::Real(1.0))
                    .map(Derivative::Expression),
                dae::CoordinateView::State(state) => {
                    if let Some(definition) = self.derivative_definitions[state.index() as usize] {
                        let definition = self.rebuild(
                            self.source
                                .expression_id(definition as usize)
                                .expect("explicit derivative definition resolves"),
                        )?;
                        return self
                            .target
                            .at(provenance)
                            .unary(dae::UnaryOperator::Plus, definition)
                            .map(Derivative::Expression);
                    }
                    let TargetVariable::State(state) =
                        self.variables[state.index() as usize].identity
                    else {
                        unreachable!("candidate RHS cannot refer to the demoted state")
                    };
                    self.target
                        .at(provenance)
                        .coordinate(dae::CoordinateInput::Derivative(state))
                        .map(Derivative::Expression)
                }
                _ => unreachable!("differentiability preflight rejects this coordinate"),
            },
            dae::ExpressionOperation::Unary { operator, operand } => {
                let derivative = self.differentiate(operand, provenance)?;
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
                self.differentiate_binary(operator, lhs, rhs, provenance)
            }
            _ => unreachable!("differentiability preflight rejects this operation"),
        }
    }

    fn differentiate_binary(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'source>,
        rhs: dae::ExprId<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let lhs_derivative = self.differentiate(lhs, provenance)?;
        let rhs_derivative = self.differentiate(rhs, provenance)?;
        match operator {
            dae::BinaryOperator::Add | dae::BinaryOperator::Subtract => {
                self.combine_sum(operator, lhs_derivative, rhs_derivative, provenance)
            }
            dae::BinaryOperator::Multiply => {
                let lhs_value = self.rebuild(lhs)?;
                let rhs_value = self.rebuild(rhs)?;
                let left = self.multiply(lhs_derivative, rhs_value, provenance)?;
                let right = self.multiply(rhs_derivative, lhs_value, provenance)?;
                self.combine_sum(dae::BinaryOperator::Add, left, right, provenance)
            }
            dae::BinaryOperator::Divide => {
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
    }

    #[derive(Clone, Copy)]
    struct FixtureSpans {
        constraint: dae::DaeProvenance,
        derivative_y: dae::DaeProvenance,
        derivative_x: dae::DaeProvenance,
    }

    fn constrained_state_model(nonlinear_constraint: bool) -> (dae::Dae, dae::DaeProvenance) {
        let rhs = if nonlinear_constraint {
            "sin(y)"
        } else {
            "p*y"
        };
        let text = format!(
            "parameter Real p; Real x; Real y; Real a; equation x = {rhs}; der(y) = a; der(x) = 1;"
        );
        let mut sources = SourceMap::new();
        let source = sources.add("checked_index_reduction.mo", &text);
        let declaration = source_provenance(source, &text, "parameter Real p");
        let constraint = source_provenance(source, &text, &format!("x = {rhs}"));
        let derivative_y = source_provenance(source, &text, "der(y) = a");
        let derivative_x = source_provenance(source, &text, "der(x) = 1");
        let model = dae::Dae::construct(sources, |model| {
            let real = model.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    declaration,
                )
            })?;
            let variables = model.variables(|variables| {
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
                })
            })?;
            let spans = FixtureSpans {
                constraint,
                derivative_y,
                derivative_x,
            };
            let [constraint_residual, y_residual, x_residual] =
                model.expressions(|expressions| {
                    fixture_residuals(expressions, variables, spans, nonlinear_constraint)
                })?;
            model.continuous(|continuous| {
                continuous.value_equation(constraint, constraint_residual)?;
                continuous.value_equation(derivative_y, y_residual)?;
                continuous.value_equation(derivative_x, x_residual)
            })
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
        let (model, constraint) = constrained_state_model(false);
        let prepared = prepare_for_solve(&model).expect("index-one constraint is reducible");
        assert!(matches!(prepared, PreparedDae::Transformed(_)));

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
    fn unsupported_symbolic_derivative_preserves_the_original_singular_error() {
        let (model, _) = constrained_state_model(true);
        let error = match prepare_for_solve(&model) {
            Ok(_) => panic!("unsupported differentiation must not guess a replacement"),
            Err(error) => error,
        };
        assert!(matches!(error, StructuralError::Singular { .. }));
    }
}
