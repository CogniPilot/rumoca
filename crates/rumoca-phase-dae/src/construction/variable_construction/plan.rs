use super::*;
use rumoca_core::{ExpressionScope, ExpressionVisitor};

#[cfg(test)]
mod tests;

pub(in crate::construction) struct VariableConstructionPlan {
    reservations: ReservationSchedule,
}

impl VariableConstructionPlan {
    pub(super) fn into_reservation_issuer<T>(self) -> ReservationIssuer<T> {
        self.reservations.into_issuer()
    }
}

struct ReservationSchedule {
    terminal: ReservationTerminal,
    source_order: Box<[ReservationMember]>,
    definition_order: Box<[ReservationMember]>,
}

enum ReservationTerminal {
    Empty,
    Populated { source_count: usize, span: Span },
}

#[derive(Clone, Copy)]
struct ReservationMember {
    source_ordinal: usize,
    span: Span,
}

impl ReservationSchedule {
    fn checked(
        source_count: usize,
        source_order: Vec<ReservationMember>,
        definition_order: Vec<usize>,
        owner_span: Span,
    ) -> Result<Self, ToDaeError> {
        let mut expected = HashSet::with_capacity(source_order.len());
        let mut previous = None;
        for member in &source_order {
            if member.source_ordinal >= source_count
                || !expected.insert(member.source_ordinal)
                || previous.is_some_and(|ordinal| ordinal >= member.source_ordinal)
            {
                return Err(reservation_schedule_error(
                    "duplicate or out-of-order source member",
                    member.span,
                ));
            }
            previous = Some(member.source_ordinal);
        }
        let mut seen = HashSet::with_capacity(definition_order.len());
        let mut definitions = Vec::with_capacity(definition_order.len());
        for source_ordinal in &definition_order {
            if !expected.contains(source_ordinal) {
                return Err(reservation_schedule_error(
                    "foreign definition member",
                    owner_span,
                ));
            }
            if !seen.insert(*source_ordinal) {
                return Err(reservation_schedule_error(
                    "duplicate definition member",
                    owner_span,
                ));
            }
            let Some(member) = source_order
                .iter()
                .find(|member| member.source_ordinal == *source_ordinal)
                .copied()
            else {
                return Err(reservation_schedule_error(
                    "foreign definition member",
                    owner_span,
                ));
            };
            definitions.push(member);
        }
        if seen.len() != expected.len() {
            return Err(reservation_schedule_error(
                "missing definition member",
                owner_span,
            ));
        }
        let terminal = if source_count == 0 {
            ReservationTerminal::Empty
        } else {
            ReservationTerminal::Populated {
                source_count,
                span: owner_span,
            }
        };
        Ok(Self {
            terminal,
            source_order: source_order.into_boxed_slice(),
            definition_order: definitions.into_boxed_slice(),
        })
    }

    fn empty() -> Self {
        Self {
            terminal: ReservationTerminal::Empty,
            source_order: Box::new([]),
            definition_order: Box::new([]),
        }
    }

    fn into_issuer<T>(self) -> ReservationIssuer<T> {
        let issued = HashMap::with_capacity(self.source_order.len());
        ReservationIssuer {
            terminal: self.terminal,
            source_order: self.source_order,
            definition_order: self.definition_order,
            next_source_ordinal: 0,
            next_reservation: 0,
            pending: None,
            issued,
            failure_span: None,
        }
    }
}

fn reservation_schedule_error(detail: &str, span: Span) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "variable reservation plan",
        format!("{detail} in the issued variable reservation schedule"),
        span,
    )
}

pub(super) struct ReservationIssuer<T> {
    terminal: ReservationTerminal,
    source_order: Box<[ReservationMember]>,
    definition_order: Box<[ReservationMember]>,
    next_source_ordinal: usize,
    next_reservation: usize,
    pending: Option<ReservationMember>,
    issued: HashMap<usize, (T, Span)>,
    failure_span: Option<Span>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum ReservationRequirement {
    Complete,
    Reserve,
}

impl<T> ReservationIssuer<T> {
    pub(super) fn next_source(
        &mut self,
        source_ordinal: usize,
        span: Span,
    ) -> Result<ReservationRequirement, dae::DaeConstructionError> {
        if let Some(span) = self.failure_span {
            return Err(dae::DaeConstructionError::InvalidExpressionForm { span });
        }
        if self.pending.is_some()
            || source_ordinal != self.next_source_ordinal
            || !self.accepts_source(source_ordinal)
        {
            self.failure_span = Some(span);
            return Err(dae::DaeConstructionError::InvalidExpressionForm { span });
        }
        self.next_source_ordinal += 1;
        let Some(member) = self.source_order.get(self.next_reservation).copied() else {
            return Ok(ReservationRequirement::Complete);
        };
        if member.source_ordinal < source_ordinal {
            self.failure_span = Some(member.span);
            return Err(dae::DaeConstructionError::InvalidExpressionForm { span: member.span });
        }
        if member.source_ordinal == source_ordinal {
            self.next_reservation += 1;
            self.pending = Some(member);
            return Ok(ReservationRequirement::Reserve);
        }
        Ok(ReservationRequirement::Complete)
    }

    fn accepts_source(&self, source_ordinal: usize) -> bool {
        match self.terminal {
            ReservationTerminal::Empty => false,
            ReservationTerminal::Populated { source_count, .. } => source_ordinal < source_count,
        }
    }

    pub(super) fn issue(
        &mut self,
        reservation: T,
        span: Span,
    ) -> Result<(), dae::DaeConstructionError> {
        if let Some(span) = self.failure_span {
            return Err(dae::DaeConstructionError::InvalidExpressionForm { span });
        }
        let Some(member) = self.pending.take() else {
            self.failure_span = Some(span);
            return Err(dae::DaeConstructionError::InvalidExpressionForm { span });
        };
        if self
            .issued
            .insert(member.source_ordinal, (reservation, span))
            .is_some()
        {
            self.failure_span = Some(span);
            return Err(dae::DaeConstructionError::InvalidExpressionForm { span });
        }
        Ok(())
    }

    pub(super) fn finish(mut self) -> Result<IssuedReservations<T>, dae::DaeConstructionError> {
        if let Some(span) = self.failure_span {
            return Err(dae::DaeConstructionError::InvalidExpressionForm { span });
        }
        if let Some(pending) = self.pending {
            return Err(dae::DaeConstructionError::InvalidExpressionForm { span: pending.span });
        }
        let terminal_span = match self.terminal {
            ReservationTerminal::Empty if self.next_source_ordinal == 0 => None,
            ReservationTerminal::Empty => self.failure_span,
            ReservationTerminal::Populated { source_count, .. }
                if self.next_source_ordinal == source_count =>
            {
                None
            }
            ReservationTerminal::Populated { span, .. } => Some(span),
        };
        if let Some(span) = terminal_span {
            return Err(dae::DaeConstructionError::InvalidExpressionForm { span });
        }
        if let Some(missing) = self.source_order.get(self.next_reservation) {
            return Err(dae::DaeConstructionError::InvalidExpressionForm { span: missing.span });
        }
        let mut definitions = Vec::with_capacity(self.definition_order.len());
        for member in &self.definition_order {
            let Some((reservation, _)) = self.issued.remove(&member.source_ordinal) else {
                return Err(dae::DaeConstructionError::InvalidExpressionForm { span: member.span });
            };
            definitions.push(reservation);
        }
        if let Some((_, (_, span))) = self.issued.into_iter().next() {
            return Err(dae::DaeConstructionError::InvalidExpressionForm { span });
        }
        Ok(IssuedReservations {
            definitions: definitions.into_iter(),
        })
    }
}

pub(in crate::construction) struct IssuedReservations<T> {
    definitions: std::vec::IntoIter<T>,
}

impl<T> IssuedReservations<T> {
    pub(super) fn into_definitions(self) -> impl Iterator<Item = T> {
        self.definitions
    }
}

pub(super) struct VariablePlan {
    declaration: Span,
    dependencies: Box<[DependencyUse]>,
    binding_dependencies: Box<[DependencyUse]>,
    requires_function_ids: bool,
}

impl VariablePlan {
    /// A complete insertion is locally valid once every referenced coordinate
    /// identity has been issued. Definitions of those coordinates may still
    /// be open under their own linear forward-attribute capabilities.
    pub(super) fn requires_reservation(&self, source_ordinal: usize) -> bool {
        self.requires_function_ids
            || self
                .dependencies
                .iter()
                .any(|dependency| dependency.target >= source_ordinal)
    }
}

#[derive(Clone, Copy)]
struct DependencyUse {
    target: usize,
    span: Span,
}

pub(in crate::construction) fn plan_variable_construction(
    flat: &flat::Model,
    analysis: &Analysis<'_>,
) -> Result<VariableConstructionPlan, ToDaeError> {
    let source_ordinals = flat
        .variables
        .keys()
        .enumerate()
        .filter(|(_, name)| constructed_role(analysis.roles[*name]))
        .map(|(ordinal, name)| (name.clone(), ordinal))
        .collect::<HashMap<_, _>>();
    let mut variables = Vec::with_capacity(flat.variables.len());
    for (name, variable) in &flat.variables {
        let role = analysis.roles[name];
        variables.push(plan_variable(
            variable,
            role,
            analysis.derived_parameters.get(name),
            analysis.initial_parameters.get(name),
            &source_ordinals,
        ));
    }
    let Some(owner) = variables.first() else {
        return Ok(VariableConstructionPlan {
            reservations: ReservationSchedule::empty(),
        });
    };
    let owner_span = owner.declaration;
    let dependency_graph = variables
        .iter()
        .map(|variable| {
            variable
                .dependencies
                .iter()
                .map(|dependency| dependency.target)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let components = rumoca_core::dependency_first_sccs(&dependency_graph)
        .map_err(|error| variable_dependency_order_error(&variables, error, owner_span))?;
    let binding_dependency_graph = variables
        .iter()
        .map(|variable| {
            variable
                .binding_dependencies
                .iter()
                .map(|dependency| dependency.target)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let binding_components = rumoca_core::dependency_first_sccs(&binding_dependency_graph)
        .map_err(|error| variable_dependency_order_error(&variables, error, owner_span))?;
    let calculated_parameters = analysis
        .derived_parameters
        .keys()
        .chain(analysis.initial_parameters.keys())
        .cloned()
        .collect::<HashSet<_>>();
    reject_recursive_calculated_parameters(
        flat,
        &calculated_parameters,
        &variables,
        &binding_components,
        owner_span,
    )?;
    let reservations = reservation_schedule(&variables, &components, owner_span)?;
    Ok(VariableConstructionPlan { reservations })
}

fn variable_dependency_order_error(
    variables: &[VariablePlan],
    error: rumoca_core::DependencyGraphError,
    owner_span: Span,
) -> ToDaeError {
    let span = variables
        .get(error.source)
        .map_or(owner_span, |variable| variable.declaration);
    reservation_schedule_error(&format!("foreign dependency member: {error}"), span)
}

fn reservation_schedule(
    variables: &[VariablePlan],
    components: &[rumoca_core::DependencyScc],
    plan_span: Span,
) -> Result<ReservationSchedule, ToDaeError> {
    let source_order = variables
        .iter()
        .enumerate()
        .filter(|(source_ordinal, variable)| variable.requires_reservation(*source_ordinal))
        .map(|(source_ordinal, variable)| ReservationMember {
            source_ordinal,
            span: variable.declaration,
        })
        .collect::<Vec<_>>();
    let required = source_order
        .iter()
        .map(|member| member.source_ordinal)
        .collect::<HashSet<_>>();
    let definition_order = components
        .iter()
        .flat_map(|component| component.members.iter().copied())
        .filter(|source_ordinal| required.contains(source_ordinal))
        .collect::<Vec<_>>();
    let owner_span = match source_order.first() {
        Some(member) => member.span,
        None => plan_span,
    };
    ReservationSchedule::checked(variables.len(), source_order, definition_order, owner_span)
}

fn plan_variable(
    variable: &flat::Variable,
    role: PlannedRole,
    derived: Option<&DerivedParameterPlan>,
    initial_binding: Option<&Expression>,
    source_ordinals: &HashMap<VarName, usize>,
) -> VariablePlan {
    if !constructed_role(role) {
        return VariablePlan {
            declaration: variable.source_span,
            dependencies: Box::new([]),
            binding_dependencies: Box::new([]),
            requires_function_ids: false,
        };
    }
    let mut attributes = AttributeDependencyCollector::new(source_ordinals);
    for expression in [
        variable.start.as_ref(),
        variable.min.as_ref(),
        variable.max.as_ref(),
        variable.nominal.as_ref(),
    ]
    .into_iter()
    .flatten()
    {
        attributes.visit_expression(expression);
    }
    let mut binding_collector = AttributeDependencyCollector::new(source_ordinals);
    if let Some(derived) = derived {
        binding_collector.with_shadowed(
            derived
                .domain
                .binders
                .iter()
                .map(|binder| VarName::new(&binder.display_name)),
            |collector| collector.visit_expression(&derived.body),
        );
    } else if let Some(binding) = initial_binding {
        binding_collector.visit_expression(binding);
    } else if matches!(
        role,
        PlannedRole::Parameter | PlannedRole::Constant | PlannedRole::Input
    ) && let Some(binding) = &variable.binding
    {
        binding_collector.visit_expression(binding);
    }
    let requires_function_ids =
        attributes.requires_function_ids || binding_collector.requires_function_ids;
    let binding_dependencies = binding_collector.dependencies.into_boxed_slice();
    let dependencies = merge_dependencies(attributes.dependencies, &binding_dependencies);
    VariablePlan {
        declaration: variable.source_span,
        dependencies,
        binding_dependencies,
        requires_function_ids,
    }
}

fn merge_dependencies(
    mut attributes: Vec<DependencyUse>,
    binding: &[DependencyUse],
) -> Box<[DependencyUse]> {
    let mut seen = attributes
        .iter()
        .map(|dependency| dependency.target)
        .collect::<HashSet<_>>();
    attributes.extend(
        binding
            .iter()
            .copied()
            .filter(|dependency| seen.insert(dependency.target)),
    );
    attributes.into_boxed_slice()
}

fn constructed_role(role: PlannedRole) -> bool {
    !matches!(
        role,
        PlannedRole::UnusedExpandable
            | PlannedRole::Clock
            | PlannedRole::EnumerationLiteral
            | PlannedRole::Aggregate
    )
}

fn reject_recursive_calculated_parameters(
    flat: &flat::Model,
    calculated_parameters: &HashSet<VarName>,
    variables: &[VariablePlan],
    components: &[rumoca_core::DependencyScc],
    plan_span: Span,
) -> Result<(), ToDaeError> {
    for component in components.iter().filter(|component| component.recursive) {
        let has_calculated_parameter = component.members.iter().any(|&ordinal| {
            flat.variables
                .get_index(ordinal)
                .is_some_and(|(name, _)| calculated_parameters.contains(name))
        });
        if !has_calculated_parameter {
            continue;
        }
        let Some((owner, dependency, span)) =
            first_internal_binding_dependency(flat, variables, &component.members, plan_span)?
        else {
            let span = match component
                .members
                .iter()
                .find_map(|ordinal| flat.variables.get_index(*ordinal))
                .map(|(_, variable)| variable.source_span)
            {
                Some(span) => span,
                None => plan_span,
            };
            return Err(reservation_schedule_error(
                "recursive component without an internal dependency",
                span,
            ));
        };
        return Err(ToDaeError::unsupported_flat(
            "calculated parameter dependency",
            format!(
                "`{owner}` depends recursively on `{dependency}`; calculated parameter bindings require an acyclic dependency proof"
            ),
            span,
        ));
    }
    Ok(())
}

fn first_internal_binding_dependency(
    flat: &flat::Model,
    variables: &[VariablePlan],
    members: &[usize],
    plan_span: Span,
) -> Result<Option<(VarName, VarName, Span)>, ToDaeError> {
    for &owner in members {
        let Some(variable) = variables.get(owner) else {
            return Err(reservation_schedule_error(
                "foreign recursive component member",
                plan_span,
            ));
        };
        if let Some(dependency) = variable
            .binding_dependencies
            .iter()
            .find(|dependency| members.binary_search(&dependency.target).is_ok())
        {
            let Some((owner_name, _)) = flat.variables.get_index(owner) else {
                return Err(reservation_schedule_error(
                    "foreign recursive component member",
                    variable.declaration,
                ));
            };
            let Some((dependency_name, _)) = flat.variables.get_index(dependency.target) else {
                return Err(reservation_schedule_error(
                    "foreign recursive dependency member",
                    dependency.span,
                ));
            };
            return Ok(Some((
                owner_name.clone(),
                dependency_name.clone(),
                dependency.span,
            )));
        }
    }
    Ok(None)
}

struct AttributeDependencyCollector<'flat> {
    source_ordinals: &'flat HashMap<VarName, usize>,
    dependencies: Vec<DependencyUse>,
    seen_dependencies: HashSet<usize>,
    shadowed: HashMap<VarName, usize>,
    requires_function_ids: bool,
}

impl<'flat> AttributeDependencyCollector<'flat> {
    fn new(source_ordinals: &'flat HashMap<VarName, usize>) -> Self {
        Self {
            source_ordinals,
            dependencies: Vec::new(),
            seen_dependencies: HashSet::new(),
            shadowed: HashMap::new(),
            requires_function_ids: false,
        }
    }

    fn with_shadowed(
        &mut self,
        names: impl IntoIterator<Item = VarName>,
        visit: impl FnOnce(&mut Self),
    ) {
        let names = names.into_iter().collect::<Vec<_>>();
        let previous = names
            .iter()
            .map(|name| (name.clone(), self.shadowed.get(name).copied()))
            .collect::<Vec<_>>();
        self.push_shadowed(&names);
        visit(self);
        for (name, count) in previous {
            match count {
                Some(count) => {
                    self.shadowed.insert(name, count);
                }
                None => {
                    self.shadowed.remove(&name);
                }
            }
        }
    }

    fn push_shadowed(&mut self, names: &[VarName]) {
        for name in names {
            *self.shadowed.entry(name.clone()).or_default() += 1;
        }
    }

    fn pop_shadowed(&mut self, names: &[VarName]) {
        for name in names {
            let remove = match self.shadowed.get_mut(name) {
                Some(count) if *count > 1 => {
                    *count -= 1;
                    false
                }
                Some(_) => true,
                None => false,
            };
            if remove {
                self.shadowed.remove(name);
            }
        }
    }
}

impl ExpressionVisitor for AttributeDependencyCollector<'_> {
    fn visit_expression(&mut self, expression: &Expression) {
        if let Expression::VarRef { name, span, .. } = expression
            && !self.shadowed.contains_key(name.var_name())
            && let Some(&target) = self.source_ordinals.get(name.var_name())
            && self.seen_dependencies.insert(target)
        {
            self.dependencies.push(DependencyUse {
                target,
                span: *span,
            });
        }
        if let Expression::FunctionCall { is_constructor, .. } = expression {
            self.requires_function_ids |= matches!(
                classify_function_call(*is_constructor),
                FunctionCallLowering::Registry
            );
        }
        self.walk_expression(expression);
    }

    fn enter_scope(&mut self, scope: ExpressionScope<'_>) {
        let ExpressionScope::ArrayComprehension(indices) = scope;
        let names = indices
            .iter()
            .map(|index| VarName::new(&index.name))
            .collect::<Vec<_>>();
        self.push_shadowed(&names);
    }

    fn exit_scope(&mut self, scope: ExpressionScope<'_>) {
        let ExpressionScope::ArrayComprehension(indices) = scope;
        let names = indices
            .iter()
            .map(|index| VarName::new(&index.name))
            .collect::<Vec<_>>();
        self.pop_shadowed(&names);
    }
}
