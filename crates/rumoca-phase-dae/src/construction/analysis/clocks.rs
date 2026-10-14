mod conversions;

use super::*;
use conversions::{
    ClockConversionEdge, WhenConversionScope, clocked_binding_conversion_edges,
    clocked_when_conversion_edges, propagate_clock_conversion_owners,
    validate_inferred_conversions, value_clock_conversion_equation,
};
pub(in crate::construction) use conversions::{
    inferred_clock_transfer, when_conditional_selects_clock_structure,
};

/// What decides when a clock ticks.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(in crate::construction) enum ClockSchedule {
    /// MLS §16.3 Operators 16.2 and 16.3: a periodic clock on its exact
    /// lattice.
    Periodic(ClockLattice),
    /// MLS §16.3 Operator 16.4: an event clock, identified by the clock
    /// coordinate its `Clock(condition)` constructor defines, without its
    /// first `skip` ticks (MLS §16.5.2 `shiftSample`; 0 for the clock itself).
    Event { source: InstanceId, skip: u32 },
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(in crate::construction) struct ClockPlan {
    pub(in crate::construction) schedule: ClockSchedule,
    pub(in crate::construction) constructor_span: Span,
}

impl ClockPlan {
    pub(in crate::construction) const fn periodic(
        lattice: ClockLattice,
        constructor_span: Span,
    ) -> Self {
        Self {
            schedule: ClockSchedule::Periodic(lattice),
            constructor_span,
        }
    }

    /// The exact lattice of a periodic clock; an event clock has none.
    pub(in crate::construction) const fn lattice(&self) -> Option<ClockLattice> {
        match self.schedule {
            ClockSchedule::Periodic(lattice) => Some(lattice),
            ClockSchedule::Event { .. } => None,
        }
    }

    /// The clock coordinate of the event clock `self` and the number of its
    /// ticks it skips; `None` for a periodic clock.
    pub(in crate::construction) const fn event(&self) -> Option<(InstanceId, u32)> {
        match self.schedule {
            ClockSchedule::Event { source, skip } => Some((source, skip)),
            ClockSchedule::Periodic(_) => None,
        }
    }

    /// MLS §16.5.2 `shiftSample(u, counter, resolution)` of the event clock
    /// `self`: the same clock without its first `counter` more ticks. An
    /// event clock cannot be super-sampled, so the resolution must be 1
    /// (CLK-011); `None` for a periodic clock.
    pub(in crate::construction) fn shift_event(
        self,
        counter: i64,
        resolution: i64,
        span: Span,
    ) -> Option<Result<Self, ToDaeError>> {
        let (source, skip) = self.event()?;
        let Ok(counter) = u32::try_from(counter) else {
            return Some(Err(event_clock_conversion("shiftSample", span)));
        };
        if resolution != 1 {
            return Some(Err(event_clock_conversion("shiftSample", span)));
        }
        let Some(skip) = skip.checked_add(counter) else {
            return Some(Err(event_clock_conversion("shiftSample", span)));
        };
        Some(Ok(Self {
            schedule: ClockSchedule::Event { source, skip },
            constructor_span: self.constructor_span,
        }))
    }
}

/// The condition of one MLS §16.3 event clock `Clock(condition,
/// startInterval)`.
#[derive(Clone)]
pub(in crate::construction) struct EventClockPlan {
    /// The Boolean coordinate whose rising `pre` edge ticks the clock.
    pub(in crate::construction) condition: VarName,
    /// The value of `interval()` at the first tick (MLS §16.10 Operator
    /// 16.15); `None` is the default 0.
    pub(in crate::construction) start_interval: Option<Expression>,
    pub(in crate::construction) span: Span,
}

#[derive(Clone, Copy)]
pub(in crate::construction) struct ClockedValuePlan {
    pub(in crate::construction) clock: ClockPlan,
    pub(in crate::construction) ownership_span: Span,
    pub(in crate::construction) sampled: bool,
}

/// One `sample(u)` / `sample(u, c)` coordinate definition.
///
/// `clock` is populated only by the MLS §16.3 two-operand form, where the model
/// names the owning clock itself; the one-operand form leaves the owner to the
/// §16.5.1 inference in [`infer_sampled_clock_owners`].
#[derive(Clone, Copy)]
pub(super) struct SampledTarget {
    pub(super) span: Span,
    pub(super) clock: Option<ClockPlan>,
}

pub(super) struct ClockAnalysis {
    pub(super) plans: HashMap<InstanceId, ClockPlan>,
    /// Every event clock, keyed by the clock coordinate its constructor
    /// defines (the identity in [`ClockSchedule::Event`]).
    pub(super) event_clocks: HashMap<InstanceId, EventClockPlan>,
    pub(super) equation_rows: HashSet<usize>,
    pub(super) sampled_targets: HashMap<InstanceId, SampledTarget>,
}

pub(super) struct ClockDomainAnalysis {
    pub(super) equation_owners: HashMap<usize, ClockPlan>,
    pub(super) value_owners: HashMap<InstanceId, ClockedValuePlan>,
    /// Owning clock of every `when Clock()` branch, keyed by its position.
    pub(super) when_owners: HashMap<WhenBranchKey, ClockPlan>,
    /// Owning clock of every runtime coordinate that belongs to a clocked
    /// partition, whatever role the coordinate was planned with. Declaration
    /// bindings resolve their clock through this map, since a binding is not an
    /// equation row and therefore has no `equation_owners` entry.
    pub(super) coordinate_owners: HashMap<InstanceId, ClockPlan>,
}

/// The exact `Clock` coordinates of one Flat model.
///
/// A Flat expression occurrence names its coordinate through the model's
/// variable catalog: `Reference::instance_id` carries the *enclosing* class
/// occurrence, not the referenced declaration, so clock ownership resolves
/// through the same catalog every other construction path uses and then keys
/// its plans on the resolved variable's own occurrence identity.
struct ClockCoordinates<'flat> {
    flat: &'flat flat::Model,
    ordered: Vec<&'flat flat::Variable>,
    by_name: HashMap<&'flat VarName, &'flat flat::Variable>,
}

impl<'flat> ClockCoordinates<'flat> {
    fn resolve(&self, name: &VarName) -> Option<&'flat flat::Variable> {
        self.by_name.get(name).copied()
    }

    fn contains(&self, name: &VarName) -> bool {
        self.by_name.contains_key(name)
    }

    fn len(&self) -> usize {
        self.ordered.len()
    }
}

pub(super) fn analyze_clocks(
    flat: &flat::Model,
    constants: &EvalContext,
) -> Result<ClockAnalysis, ToDaeError> {
    let clocks = exact_clock_coordinates(flat)?;
    let mut plans = HashMap::new();
    let mut event_clocks = HashMap::new();
    let mut aliases = Vec::new();
    let mut derived = Vec::new();
    let mut equation_rows = HashSet::new();
    let mut element_hubs =
        std::collections::BTreeMap::<(u32, u32), Vec<(&flat::Variable, Span)>>::new();
    derive_bound_clock_plans(flat, constants, &clocks, &mut plans, &mut event_clocks)?;
    for (row, equation) in flat.equations.iter().enumerate() {
        let residual = static_clock_branch(&equation.residual, constants, &clocks)?;
        let Some((lhs, rhs)) = subtraction_operands(residual) else {
            if expression_mentions_clock(residual, &clocks) {
                return Err(unsupported_clock_equation(equation));
            }
            continue;
        };
        // `c = if p then c1 else c2` is the same parameter selection as the
        // branch-wise `if p then c = c1 else c = c2`.
        let rhs = static_clock_branch(rhs, constants, &clocks)?;
        let lhs_clock = whole_clock_reference(lhs, &clocks);
        let rhs_clock = whole_clock_reference(rhs, &clocks);
        // A connection to one element of a clock array (a `ClockVectorInput`)
        // aliases that element: every scalar clock connected to it shares its
        // clock.
        match (
            lhs_clock,
            rhs_clock,
            clock_element_reference(lhs, &clocks),
            clock_element_reference(rhs, &clocks),
        ) {
            (Some(member), None, None, Some(element))
            | (None, Some(member), Some(element), None) => {
                element_hubs
                    .entry(element)
                    .or_default()
                    .push((member, equation.span));
                equation_rows.insert(row);
                continue;
            }
            _ => {}
        }
        if let (Some(target), None) = (lhs_clock, rhs_clock)
            && let Some(event) = event_constructor(flat, rhs)?
        {
            let plan = ClockPlan {
                schedule: ClockSchedule::Event {
                    source: target.instance_id,
                    skip: 0,
                },
                constructor_span: event.span,
            };
            insert_plan(&mut plans, target, plan, equation.span)?;
            event_clocks.insert(target.instance_id, event);
            equation_rows.insert(row);
            continue;
        }
        match (lhs_clock, rhs_clock, periodic_constructor(rhs, constants)?) {
            (Some(target), None, Some(plan)) => {
                insert_plan(&mut plans, target, plan, equation.span)?;
                equation_rows.insert(row);
            }
            (Some(lhs), Some(rhs), None) => {
                aliases.push((lhs, rhs, equation.span));
                equation_rows.insert(row);
            }
            (Some(target), None, None) => {
                derived.push((target, rhs, equation.span));
                equation_rows.insert(row);
            }
            _ if expression_mentions_clock(residual, &clocks) => {
                return Err(unsupported_clock_equation(equation));
            }
            _ => {}
        }
    }

    let mut hub_arrays = HashSet::new();
    for ((array, _), members) in &element_hubs {
        hub_arrays.insert(*array);
        for pair in members.windows(2) {
            aliases.push((pair[0].0, pair[1].0, pair[1].1));
        }
    }
    resolve_clock_definitions(&mut plans, &derived, &aliases, constants, &clocks)?;
    for variable in &clocks.ordered {
        // A clock array whose elements are only connection hubs has no clock
        // of its own.
        if hub_arrays.contains(&variable.instance_id.index()) {
            continue;
        }
        if !plans.contains_key(&variable.instance_id) {
            return Err(ToDaeError::unresolved_clock_schedule(
                variable.name.as_str(),
                "no unique constructor reaches this clock coordinate through its aliases",
                variable.source_span,
            ));
        }
    }
    let sampled_targets = analyze_sampled_targets(flat, &clocks, &plans, &equation_rows)?;
    Ok(ClockAnalysis {
        plans,
        event_clocks,
        equation_rows,
        sampled_targets,
    })
}

/// The single branch a parameter `if` selects, or `None` when a condition is
/// not parameter-evaluable.
fn statically_selected_branch<'expression>(
    branches: &'expression [(Expression, Expression)],
    else_branch: &'expression Expression,
    constants: &EvalContext,
) -> Option<&'expression Expression> {
    for (condition, value) in branches {
        if eval_expr(condition, constants).ok()?.as_bool()? {
            return Some(value);
        }
    }
    Some(else_branch)
}

fn exact_clock_coordinates(flat: &flat::Model) -> Result<ClockCoordinates<'_>, ToDaeError> {
    let clock_type = flat.predefined_types.clock;
    let mut ordered = Vec::new();
    let mut by_name = HashMap::new();
    for (name, variable) in &flat.variables {
        if !is_predefined_clock_variable(flat, variable)? {
            continue;
        }
        ordered.push(variable);
        by_name.insert(name, variable);
    }
    debug_assert!(
        ordered
            .iter()
            .all(|variable| flat.effective_types[&variable.type_id].canonical_type() == clock_type)
    );
    Ok(ClockCoordinates {
        flat,
        ordered,
        by_name,
    })
}

fn derive_bound_clock_plans(
    flat: &flat::Model,
    constants: &EvalContext,
    clocks: &ClockCoordinates<'_>,
    plans: &mut HashMap<InstanceId, ClockPlan>,
    event_clocks: &mut HashMap<InstanceId, EventClockPlan>,
) -> Result<(), ToDaeError> {
    for _ in 0..clocks.len() {
        let mut progress = false;
        for variable in &clocks.ordered {
            if plans.contains_key(&variable.instance_id) {
                continue;
            }
            let Some(binding) = variable.binding.as_ref() else {
                continue;
            };
            if let Some(event) = event_constructor(flat, binding)? {
                let plan = ClockPlan {
                    schedule: ClockSchedule::Event {
                        source: variable.instance_id,
                        skip: 0,
                    },
                    constructor_span: event.span,
                };
                insert_plan(plans, variable, plan, event.span)?;
                event_clocks.insert(variable.instance_id, event);
                progress = true;
                continue;
            }
            let Some(plan) = bound_clock_plan(binding, constants, clocks, plans)? else {
                continue;
            };
            insert_plan(plans, variable, plan, expression_span(binding)?)?;
            progress = true;
        }
        if !progress {
            break;
        }
    }
    Ok(())
}

/// A `Clock` binding converting the event clock `source`: only
/// `shiftSample(u, counter)` with resolution 1 has an event clock as its
/// result (MLS §16.5.2).
fn shifted_event_binding(
    source: ClockPlan,
    function: BuiltinFunction,
    args: &[Expression],
    constants: &EvalContext,
    span: Span,
) -> Result<Option<ClockPlan>, ToDaeError> {
    let operator = function.name();
    let (counter, resolution) = match (function, args) {
        (BuiltinFunction::ShiftSample, [_, counter]) => {
            (clock_integer(counter, constants, operator, span)?, 1)
        }
        (BuiltinFunction::ShiftSample, [_, counter, resolution]) => (
            clock_integer(counter, constants, operator, span)?,
            clock_integer(resolution, constants, operator, span)?,
        ),
        _ => return Err(event_clock_conversion(operator, span)),
    };
    source.shift_event(counter, resolution, span).transpose()
}

fn bound_clock_plan(
    expression: &Expression,
    constants: &EvalContext,
    clocks: &ClockCoordinates<'_>,
    plans: &HashMap<InstanceId, ClockPlan>,
) -> Result<Option<ClockPlan>, ToDaeError> {
    // A top-level event constructor is planned by its caller, so one reached
    // here is the source of a clock conversion.
    if let Some(event) = event_constructor(clocks.flat, expression)? {
        return Err(event_clock_conversion("clock conversion", event.span));
    }
    if let Some(plan) = periodic_constructor(expression, constants)? {
        return Ok(Some(plan));
    }
    if let Some(source) = whole_clock_reference(expression, clocks) {
        return Ok(plans.get(&source.instance_id).copied());
    }
    let Expression::BuiltinCall {
        function,
        args,
        span,
    } = expression
    else {
        return Err(ToDaeError::unresolved_clock_schedule(
            "Clock binding",
            "a Clock binding must be a constructor, alias, or exact derived clock",
            expression_span(expression)?,
        ));
    };
    let operator = function.name();
    let Some(source) = args.first() else {
        return Err(invalid_clock_operator(
            operator,
            "requires a source clock",
            *span,
        ));
    };
    let Some(source_plan) = bound_clock_plan(source, constants, clocks, plans)? else {
        return Ok(None);
    };
    if source_plan.event().is_some() {
        return shifted_event_binding(source_plan, *function, args, constants, *span);
    }
    let Some(source_lattice) = source_plan.lattice() else {
        return Err(event_clock_conversion(operator, *span));
    };
    let lattice = match (function, args.as_slice()) {
        (BuiltinFunction::SubSample, [_, factor]) => {
            source_lattice.sub_sample(clock_integer(factor, constants, operator, *span)?)
        }
        (BuiltinFunction::SuperSample, [_, factor]) => {
            source_lattice.super_sample(clock_integer(factor, constants, operator, *span)?)
        }
        (BuiltinFunction::ShiftSample, [_, counter]) => {
            source_lattice.shift_sample(clock_integer(counter, constants, operator, *span)?, 1)
        }
        (BuiltinFunction::ShiftSample, [_, counter, resolution]) => source_lattice.shift_sample(
            clock_integer(counter, constants, operator, *span)?,
            clock_integer(resolution, constants, operator, *span)?,
        ),
        (BuiltinFunction::BackSample, [_, counter]) => {
            source_lattice.back_sample(clock_integer(counter, constants, operator, *span)?, 1)
        }
        (BuiltinFunction::BackSample, [_, counter, resolution]) => source_lattice.back_sample(
            clock_integer(counter, constants, operator, *span)?,
            clock_integer(resolution, constants, operator, *span)?,
        ),
        (BuiltinFunction::NoClock, [_]) => {
            return Err(invalid_clock_operator(
                operator,
                "has no exact periodic lattice for checked clock ownership",
                *span,
            ));
        }
        (
            BuiltinFunction::SubSample
            | BuiltinFunction::SuperSample
            | BuiltinFunction::ShiftSample
            | BuiltinFunction::BackSample
            | BuiltinFunction::NoClock,
            _,
        ) => {
            return Err(invalid_clock_operator(
                operator,
                "has invalid clock conversion arity",
                *span,
            ));
        }
        _ => {
            return Err(ToDaeError::unresolved_clock_schedule(
                "Clock binding",
                "a Clock binding must use an exact predefined clock conversion",
                *span,
            ));
        }
    }
    .map_err(|error| {
        ToDaeError::unsupported_runtime_operator(operator, error.to_string(), *span)
    })?;
    Ok(Some(ClockPlan::periodic(lattice, *span)))
}

fn clock_integer(
    expression: &Expression,
    constants: &EvalContext,
    operator: &str,
    span: Span,
) -> Result<i64, ToDaeError> {
    eval_expr(expression, constants)
        .ok()
        .and_then(|value| value.as_integer())
        .ok_or_else(|| {
            invalid_clock_operator(
                operator,
                "requires parameter-evaluable Integer conversion arguments",
                span,
            )
        })
}

fn invalid_clock_operator(operator: &str, detail: &str, span: Span) -> ToDaeError {
    ToDaeError::unsupported_runtime_operator(operator, detail, span)
}

fn named_sample_clock_plan(
    clock: &flat::Variable,
    plans: &HashMap<InstanceId, ClockPlan>,
    span: Span,
) -> Result<ClockPlan, ToDaeError> {
    plans.get(&clock.instance_id).copied().ok_or_else(|| {
        ToDaeError::unresolved_clock_schedule(
            clock.name.as_str(),
            "the clock operand of a value sample must resolve to a static schedule",
            span,
        )
    })
}

fn analyze_sampled_targets(
    flat: &flat::Model,
    clocks: &ClockCoordinates<'_>,
    plans: &HashMap<InstanceId, ClockPlan>,
    clock_equation_rows: &HashSet<usize>,
) -> Result<HashMap<InstanceId, SampledTarget>, ToDaeError> {
    let mut sampled = HashMap::new();
    for (row, equation) in flat.equations.iter().enumerate() {
        if clock_equation_rows.contains(&row) {
            continue;
        }
        if let Some((target, sample_span, clock)) =
            sampled_value_target(&equation.residual, flat, clocks)
        {
            let clock = clock
                .map(|clock| named_sample_clock_plan(clock, plans, sample_span))
                .transpose()?;
            if sampled
                .insert(
                    target.instance_id,
                    SampledTarget {
                        span: sample_span,
                        clock,
                    },
                )
                .is_some()
            {
                return Err(ToDaeError::unsupported_flat(
                    "clocked sample ownership proof",
                    format!(
                        "sampled coordinate `{}` has more than one definition",
                        target.name
                    ),
                    equation.span,
                ));
            }
        } else if expression_mentions_value_sample(&equation.residual, clocks) {
            return Err(ToDaeError::unsupported_flat(
                "clocked sample ownership proof",
                "sample(value) must be the complete right-hand side of one coordinate definition",
                equation.span,
            ));
        }
    }
    Ok(sampled)
}

/// An initial equation cannot own a clock-owned sample or previous value.
fn reject_initial_clock_ownership(flat: &flat::Model) -> Result<(), ToDaeError> {
    let no_sampled_targets = HashMap::new();
    for equation in &flat.initial_equations {
        if let Some(span) = required_clock_owner_span(&equation.residual, flat, &no_sampled_targets)
        {
            return Err(ToDaeError::unsupported_flat(
                "initial clocked equation ownership",
                "clock-owned sample/previous semantics are not valid in an initial equation",
                span,
            ));
        }
    }
    Ok(())
}

/// The model equation rows the clock analysis registers.
struct EquationRowScope<'a> {
    flat: &'a flat::Model,
    roles: &'a HashMap<VarName, PlannedRole>,
    clock_equation_rows: &'a HashSet<usize>,
    constants: &'a EvalContext,
    ordinals: &'a HashMap<InstanceId, usize>,
}

/// Registers the clock incidence of every non-clock equation row and the
/// conversion edge of every row that converts a value between clocks.
fn register_equation_rows(
    scope: EquationRowScope<'_>,
    occurrences: &mut [Option<Span>],
    domains: &mut DisjointDomains,
) -> Result<(Vec<Vec<usize>>, Vec<ClockConversionEdge>), ToDaeError> {
    let EquationRowScope {
        flat,
        roles,
        clock_equation_rows,
        constants,
        ordinals,
    } = scope;
    let mut equation_members = vec![Vec::new(); flat.equations.len()];
    let mut conversion_edges = Vec::new();
    for (row, equation) in flat.equations.iter().enumerate() {
        if clock_equation_rows.contains(&row) {
            continue;
        }
        let incidence = expression_clock_incidence(&equation.residual, flat, roles);
        equation_members[row] = register_incidence(&incidence, ordinals, occurrences, domains);
        if let Some(conversion) = value_clock_conversion_equation(equation, constants)? {
            let source_incidence = expression_clock_incidence(conversion.source, flat, roles);
            let source_members =
                register_incidence(&source_incidence, ordinals, occurrences, domains);
            conversion_edges.push(ClockConversionEdge::new(
                &equation_members[row],
                &source_members,
                conversion.kind,
                conversion.span,
            )?);
        }
    }
    Ok((equation_members, conversion_edges))
}

pub(super) fn analyze_clock_domains(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    plans: &HashMap<InstanceId, ClockPlan>,
    clock_equation_rows: &HashSet<usize>,
    sampled_targets: &HashMap<InstanceId, SampledTarget>,
    constants: &EvalContext,
) -> Result<ClockDomainAnalysis, ToDaeError> {
    reject_initial_clock_ownership(flat)?;
    let ordinals = flat
        .variables
        .iter()
        .filter(|(name, _)| {
            roles
                .get(*name)
                .is_some_and(|role| is_clock_runtime_role(*role))
        })
        .enumerate()
        .map(|(ordinal, (_, variable))| (variable.instance_id, ordinal))
        .collect::<HashMap<_, _>>();
    let mut domains = DisjointDomains::new(ordinals.len());
    let mut occurrences = vec![None; ordinals.len()];
    let (equation_members, mut conversion_edges) = register_equation_rows(
        EquationRowScope {
            flat,
            roles,
            clock_equation_rows,
            constants,
            ordinals: &ordinals,
        },
        &mut occurrences,
        &mut domains,
    )?;
    conversion_edges.extend(clocked_binding_conversion_edges(
        flat,
        roles,
        constants,
        &ordinals,
        &mut occurrences,
        &mut domains,
    )?);
    let WhenClockSeeds { seeds, inferred } = clocked_when_seeds(
        flat,
        roles,
        plans,
        &ordinals,
        &mut occurrences,
        &mut domains,
    );
    conversion_edges.extend(clocked_when_conversion_edges(
        &WhenConversionScope {
            flat,
            roles,
            constants,
            ordinals: &ordinals,
        },
        plans,
        &mut occurrences,
        &mut domains,
    )?);
    let mut owners = assign_domain_owners(&mut domains, seeds)?;
    own_named_sample_clocks(flat, sampled_targets, &ordinals, &mut domains, &mut owners)?;
    // MLS §16.5.1 infers the clock of a `sample(u)` partition from the clock
    // relations it takes part in, so every partition an exact conversion
    // reaches from an owned partition is owned first; only a partition no
    // relation reaches falls back to the model's unique clock.
    propagate_clock_conversion_owners(&conversion_edges, &mut domains, &mut owners)?;
    infer_sampled_clock_owners(
        flat,
        plans,
        sampled_targets,
        &ordinals,
        &mut domains,
        &mut owners,
    )?;
    propagate_clock_conversion_owners(&conversion_edges, &mut domains, &mut owners)?;
    validate_inferred_conversions(&conversion_edges, &mut domains, &owners)?;
    let equation_owners = assign_equation_owners(
        flat,
        &equation_members,
        &mut domains,
        &owners,
        sampled_targets,
    )?;
    let when_owners = resolve_inferred_when_owners(&inferred, &mut domains, &owners)?;
    let coordinate_owners = ordinals
        .iter()
        .filter_map(|(&instance, &ordinal)| {
            let root = domains.find(ordinal);
            owners.get(&root).map(|(clock, _)| (instance, *clock))
        })
        .collect::<HashMap<_, _>>();
    validate_clocked_value_samples(flat, plans, &equation_owners)?;
    let value_owners = assign_value_owners(
        flat,
        roles,
        &ordinals,
        &mut domains,
        &owners,
        &occurrences,
        sampled_targets,
    )?;
    Ok(ClockDomainAnalysis {
        equation_owners,
        value_owners,
        when_owners,
        coordinate_owners,
    })
}

/// MLS §16.5.1 `when Clock() then`: the branch declares that its equations form
/// a clocked partition whose clock is *inferred*, so the owner is whichever
/// clock the connected partition already proves. A branch that reaches no such
/// owner has no schedule and is reported rather than defaulted.
fn resolve_inferred_when_owners(
    inferred: &[InferredWhenBranch],
    domains: &mut DisjointDomains,
    owners: &HashMap<usize, (ClockPlan, Span)>,
) -> Result<HashMap<WhenBranchKey, ClockPlan>, ToDaeError> {
    let mut when_owners = HashMap::with_capacity(inferred.len());
    for branch in inferred {
        let owner = branch
            .member
            .map(|member| domains.find(member))
            .and_then(|root| owners.get(&root))
            .map(|(clock, _)| *clock)
            .ok_or_else(|| {
                ToDaeError::unresolved_clock_schedule(
                    "when Clock()",
                    "clock inference reaches no exact clock constructor from this partition",
                    branch.span,
                )
            })?;
        when_owners.insert(branch.key, owner);
    }
    Ok(when_owners)
}

fn is_clock_runtime_role(role: PlannedRole) -> bool {
    !matches!(
        role,
        PlannedRole::UnusedExpandable
            | PlannedRole::Parameter
            | PlannedRole::Constant
            | PlannedRole::Clock
            | PlannedRole::EnumerationLiteral
            | PlannedRole::Aggregate
    )
}

#[derive(Default)]
struct ClockIncidence {
    variables: Vec<(InstanceId, Span)>,
    seen: HashSet<InstanceId>,
}

impl ClockIncidence {
    fn insert(&mut self, instance: InstanceId, span: Span) {
        if self.seen.insert(instance) {
            self.variables.push((instance, span));
        }
    }
}

fn expression_clock_incidence(
    expression: &Expression,
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
) -> ClockIncidence {
    let mut incidence = ClockIncidence::default();
    collect_expression_clock_incidence(expression, flat, roles, &mut incidence);
    incidence
}

fn collect_expression_clock_incidence(
    expression: &Expression,
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    incidence: &mut ClockIncidence,
) {
    if let Expression::VarRef {
        name,
        subscripts,
        span,
    } = expression
    {
        register_runtime_coordinate(flat, name.var_name(), *span, roles, incidence);
        for subscript in subscripts {
            if let Subscript::Expr { expr, .. } = subscript {
                collect_expression_clock_incidence(expr, flat, roles, incidence);
            }
        }
        return;
    }
    match expression {
        Expression::BuiltinCall {
            function:
                BuiltinFunction::Sample
                | BuiltinFunction::Clock
                | BuiltinFunction::Hold
                | BuiltinFunction::SubSample
                | BuiltinFunction::SuperSample
                | BuiltinFunction::ShiftSample
                | BuiltinFunction::BackSample
                | BuiltinFunction::NoClock,
            args,
            ..
        } => {
            for argument in args.iter().skip(1) {
                collect_expression_clock_incidence(argument, flat, roles, incidence);
            }
        }
        _ => {
            for child in expression_children(expression) {
                collect_expression_clock_incidence(child, flat, roles, incidence);
            }
        }
    }
}

fn register_incidence(
    incidence: &ClockIncidence,
    ordinals: &HashMap<InstanceId, usize>,
    occurrences: &mut [Option<Span>],
    domains: &mut DisjointDomains,
) -> Vec<usize> {
    let members = incidence
        .variables
        .iter()
        .filter_map(|(instance, span)| {
            let ordinal = ordinals.get(instance).copied()?;
            occurrences[ordinal].get_or_insert(*span);
            Some(ordinal)
        })
        .collect::<Vec<_>>();
    if let Some((&first, rest)) = members.split_first() {
        for &member in rest {
            domains.union(first, member);
        }
    }
    members
}

#[derive(Clone, Copy)]
struct ClockDomainSeed {
    member: usize,
    clock: ClockPlan,
    span: Span,
}

/// The position of one `when`/`elsewhen` branch in a Flat model: branch
/// `branch` of `flat.when_chains[chain]`.
///
/// Every instance of one class shares the class's source spans, so a span
/// cannot tell two instances' branches apart; the position does.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(in crate::construction) struct WhenBranchKey {
    pub(in crate::construction) chain: usize,
    pub(in crate::construction) branch: usize,
}

/// One `when Clock() then` branch and the partition member it joins.
#[derive(Clone, Copy)]
struct InferredWhenBranch {
    member: Option<usize>,
    key: WhenBranchKey,
    span: Span,
}

struct WhenClockSeeds {
    seeds: Vec<ClockDomainSeed>,
    inferred: Vec<InferredWhenBranch>,
}

fn clocked_when_seeds(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    plans: &HashMap<InstanceId, ClockPlan>,
    ordinals: &HashMap<InstanceId, usize>,
    occurrences: &mut [Option<Span>],
    domains: &mut DisjointDomains,
) -> WhenClockSeeds {
    let mut seeds = Vec::new();
    let mut inferred = Vec::new();
    for (chain_index, chain) in flat.when_chains.iter().enumerate() {
        for (branch_index, branch) in chain.branches().enumerate() {
            let clock = clock_condition_plan(&branch.condition, flat, plans);
            if clock.is_none() && !is_inferred_clock_condition(&branch.condition) {
                continue;
            }
            let mut incidence = ClockIncidence::default();
            collect_when_clock_incidence(flat, &branch.equations, roles, &mut incidence);
            let members = register_incidence(&incidence, ordinals, occurrences, domains);
            let member = members.first().copied();
            match (clock, member) {
                (Some(clock), Some(member)) => seeds.push(ClockDomainSeed {
                    member,
                    clock,
                    span: branch.span,
                }),
                (Some(_), None) => {}
                (None, member) => inferred.push(InferredWhenBranch {
                    member,
                    key: WhenBranchKey {
                        chain: chain_index,
                        branch: branch_index,
                    },
                    span: branch.span,
                }),
            }
        }
    }
    WhenClockSeeds { seeds, inferred }
}

/// MLS §16.3 Operator 16.2 `Clock()`: the inferred-clock constructor, which in
/// a `when` condition marks a clocked partition whose clock the model does not
/// name.
pub(in crate::construction) fn is_inferred_clock_condition(condition: &Expression) -> bool {
    matches!(
        condition,
        Expression::BuiltinCall {
            function: BuiltinFunction::Clock,
            args,
            ..
        } if args.is_empty()
    )
}

fn clock_condition_plan(
    condition: &Expression,
    flat: &flat::Model,
    plans: &HashMap<InstanceId, ClockPlan>,
) -> Option<ClockPlan> {
    let Expression::VarRef {
        name, subscripts, ..
    } = condition
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let variable = flat.variables.get(name.var_name())?;
    plans.get(&variable.instance_id).copied()
}

fn collect_when_clock_incidence(
    flat: &flat::Model,
    equations: &[flat::WhenEquation],
    roles: &HashMap<VarName, PlannedRole>,
    incidence: &mut ClockIncidence,
) {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign {
                target,
                value,
                span,
                ..
            }
            | flat::WhenEquation::Reinit {
                state: target,
                value,
                span,
                ..
            } => {
                register_runtime_coordinate(flat, target, *span, roles, incidence);
                collect_expression_clock_incidence(value, flat, roles, incidence);
            }
            flat::WhenEquation::AssignElement {
                target,
                value,
                span,
                ..
            } => {
                register_runtime_coordinate(flat, target.var_name(), *span, roles, incidence);
                collect_expression_clock_incidence(value, flat, roles, incidence);
            }
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                collect_expression_clock_incidence(condition, flat, roles, incidence);
                collect_expression_clock_incidence(message, flat, roles, incidence);
                if let Some(level) = level {
                    collect_expression_clock_incidence(level, flat, roles, incidence);
                }
            }
            flat::WhenEquation::Terminate { message, .. } => {
                collect_expression_clock_incidence(message, flat, roles, incidence);
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, equations) in branches {
                    collect_expression_clock_incidence(condition, flat, roles, incidence);
                    collect_when_clock_incidence(flat, equations, roles, incidence);
                }
                if let Some(equations) = else_branch {
                    collect_when_clock_incidence(flat, equations, roles, incidence);
                }
            }
            flat::WhenEquation::FunctionCallOutputs {
                outputs,
                function,
                span,
                ..
            } => {
                for output in outputs {
                    register_runtime_coordinate(flat, output, *span, roles, incidence);
                }
                collect_expression_clock_incidence(function, flat, roles, incidence);
            }
        }
    }
}

fn register_runtime_coordinate(
    flat: &flat::Model,
    name: &VarName,
    span: Span,
    roles: &HashMap<VarName, PlannedRole>,
    incidence: &mut ClockIncidence,
) {
    if roles
        .get(name)
        .is_some_and(|role| is_clock_runtime_role(*role))
        && let Some(variable) = flat.variables.get(name)
    {
        incidence.insert(variable.instance_id, span);
    }
}

fn assign_domain_owners(
    domains: &mut DisjointDomains,
    seeds: Vec<ClockDomainSeed>,
) -> Result<HashMap<usize, (ClockPlan, Span)>, ToDaeError> {
    let mut owners = HashMap::new();
    for seed in seeds {
        let root = domains.find(seed.member);
        match owners.get(&root) {
            Some((clock, _)) if *clock != seed.clock => {
                return Err(ToDaeError::unsupported_flat(
                    "clocked equation ownership proof",
                    "one equation component is connected to distinct clock owners",
                    seed.span,
                ));
            }
            Some(_) => {}
            None => {
                owners.insert(root, (seed.clock, seed.span));
            }
        }
    }
    Ok(owners)
}

/// MLS §16.3 `sample(u, c)`: the model names the owning clock itself, so the
/// partition the sampled coordinate belongs to is proven, not inferred.
///
/// Two named clocks reaching one partition is the same conflict a clocked
/// `when` seed reports, so it is routed through [`assign_domain_owners`].
fn own_named_sample_clocks(
    flat: &flat::Model,
    sampled_targets: &HashMap<InstanceId, SampledTarget>,
    ordinals: &HashMap<InstanceId, usize>,
    domains: &mut DisjointDomains,
    owners: &mut HashMap<usize, (ClockPlan, Span)>,
) -> Result<(), ToDaeError> {
    let mut seeds = Vec::new();
    for variable in flat.variables.values() {
        let Some(target) = sampled_targets.get(&variable.instance_id) else {
            continue;
        };
        let Some(clock) = target.clock else {
            continue;
        };
        let Some(&ordinal) = ordinals.get(&variable.instance_id) else {
            continue;
        };
        seeds.push(ClockDomainSeed {
            member: ordinal,
            clock,
            span: target.span,
        });
    }
    seeds.sort_by_key(|seed| (seed.member, seed.span.start));
    for seed in seeds {
        let root = domains.find(seed.member);
        match owners.get(&root) {
            Some((clock, _)) if *clock != seed.clock => {
                return Err(ToDaeError::unsupported_flat(
                    "clocked sample ownership proof",
                    "sample(value, clock) names a clock that conflicts with its partition owner",
                    seed.span,
                ));
            }
            Some(_) => {}
            None => {
                owners.insert(root, (seed.clock, seed.span));
            }
        }
    }
    Ok(())
}

/// MLS §16.5.1 clock inference for `sample(u)`.
///
/// A value sample that is not connected to a clocked `when` partition still
/// belongs to exactly one clock partition. The partition is proven only when
/// the model owns exactly one clock constructor; two independent constructors
/// leave the sample owner ambiguous even when their lattices coincide, so the
/// inference fails instead of picking one.
fn infer_sampled_clock_owners(
    flat: &flat::Model,
    plans: &HashMap<InstanceId, ClockPlan>,
    sampled_targets: &HashMap<InstanceId, SampledTarget>,
    ordinals: &HashMap<InstanceId, usize>,
    domains: &mut DisjointDomains,
    owners: &mut HashMap<usize, (ClockPlan, Span)>,
) -> Result<(), ToDaeError> {
    for variable in flat.variables.values() {
        let Some(span) = sampled_targets
            .get(&variable.instance_id)
            .map(|target| target.span)
        else {
            continue;
        };
        let Some(&ordinal) = ordinals.get(&variable.instance_id) else {
            continue;
        };
        let root = domains.find(ordinal);
        if owners.contains_key(&root) {
            continue;
        }
        let plan = unique_clock_plan(plans, span)?;
        owners.insert(root, (plan, span));
    }
    Ok(())
}

fn unique_clock_plan(
    plans: &HashMap<InstanceId, ClockPlan>,
    span: Span,
) -> Result<ClockPlan, ToDaeError> {
    let mut unique: Option<ClockPlan> = None;
    for plan in plans.values() {
        match unique {
            Some(existing) if existing == *plan => {}
            Some(_) => {
                return Err(ToDaeError::unsupported_flat(
                    "clocked sample ownership proof",
                    "sample(value) has more than one possible inferred clock",
                    span,
                ));
            }
            None => unique = Some(*plan),
        }
    }
    unique.ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "clocked sample ownership proof",
            "sample(value) has no inferred clock constructor",
            span,
        )
    })
}

/// MLS §16.3 `sample(u, c)`: the explicit clock operand must be the clock that
/// owns the partition the sample occurs in.
///
/// Lowering keeps only the sampled value, so an operand naming a different
/// clock would silently change the sampling rate. Proving the match here is
/// what lets [`is_whole_clock_coordinate`] accept the two-operand form at the
/// lowering boundary.
fn validate_clocked_value_samples(
    flat: &flat::Model,
    plans: &HashMap<InstanceId, ClockPlan>,
    equation_owners: &HashMap<usize, ClockPlan>,
) -> Result<(), ToDaeError> {
    for (row, equation) in flat.equations.iter().enumerate() {
        validate_value_sample_operands(
            &equation.residual,
            flat,
            plans,
            equation_owners.get(&row).copied(),
        )?;
    }
    for equation in &flat.initial_equations {
        validate_value_sample_operands(&equation.residual, flat, plans, None)?;
    }
    for chain in &flat.when_chains {
        for branch in chain.branches() {
            let owner = clock_condition_plan(&branch.condition, flat, plans);
            validate_when_value_sample_operands(&branch.equations, flat, plans, owner)?;
        }
    }
    Ok(())
}

fn validate_value_sample_operands(
    expression: &Expression,
    flat: &flat::Model,
    plans: &HashMap<InstanceId, ClockPlan>,
    owner: Option<ClockPlan>,
) -> Result<(), ToDaeError> {
    if let Expression::BuiltinCall {
        function: BuiltinFunction::Sample,
        args,
        span,
    } = expression
        && let [value, clock] = args.as_slice()
        && let Some(named) = clock_condition_plan(clock, flat, plans)
    {
        if owner != Some(named) {
            return Err(ToDaeError::unsupported_flat(
                "clocked sample ownership proof",
                "sample(value, clock) must name the clock that owns its partition",
                *span,
            ));
        }
        return validate_value_sample_operands(value, flat, plans, owner);
    }
    for child in expression_children(expression) {
        validate_value_sample_operands(child, flat, plans, owner)?;
    }
    Ok(())
}

fn validate_when_value_sample_operands(
    equations: &[flat::WhenEquation],
    flat: &flat::Model,
    plans: &HashMap<InstanceId, ClockPlan>,
    owner: Option<ClockPlan>,
) -> Result<(), ToDaeError> {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. }
            | flat::WhenEquation::AssignElement { value, .. }
            | flat::WhenEquation::Reinit { value, .. } => {
                validate_value_sample_operands(value, flat, plans, owner)?;
            }
            flat::WhenEquation::Assert { message, level, .. } => {
                validate_value_sample_operands(message, flat, plans, owner)?;
                if let Some(level) = level {
                    validate_value_sample_operands(level, flat, plans, owner)?;
                }
            }
            flat::WhenEquation::Terminate { message, .. } => {
                validate_value_sample_operands(message, flat, plans, owner)?;
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (_, equations) in branches {
                    validate_when_value_sample_operands(equations, flat, plans, owner)?;
                }
                if let Some(equations) = else_branch {
                    validate_when_value_sample_operands(equations, flat, plans, owner)?;
                }
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                validate_value_sample_operands(function, flat, plans, owner)?;
            }
        }
    }
    Ok(())
}

/// True when `expression` names one whole predefined `Clock` coordinate.
pub(in crate::construction) fn is_whole_clock_coordinate(
    flat: &flat::Model,
    expression: &Expression,
) -> bool {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return false;
    };
    subscripts.is_empty()
        && flat.variables.get(name.var_name()).is_some_and(|variable| {
            flat.effective_types
                .get(&variable.type_id)
                .is_some_and(|effective| effective.canonical_type() == flat.predefined_types.clock)
        })
}

fn assign_equation_owners(
    flat: &flat::Model,
    equation_members: &[Vec<usize>],
    domains: &mut DisjointDomains,
    owners: &HashMap<usize, (ClockPlan, Span)>,
    sampled_targets: &HashMap<InstanceId, SampledTarget>,
) -> Result<HashMap<usize, ClockPlan>, ToDaeError> {
    let mut equation_owners = HashMap::new();
    for (row, members) in equation_members.iter().enumerate() {
        let owner = members
            .first()
            .map(|&member| domains.find(member))
            .and_then(|root| owners.get(&root))
            .map(|(clock, _)| *clock);
        let equation = &flat.equations[row];
        if let Some(owner) = owner {
            reject_discretized_partition(&equation.residual)?;
            equation_owners.insert(row, owner);
            continue;
        }
        if let Some(span) = required_clock_owner_span(&equation.residual, flat, sampled_targets) {
            return Err(ToDaeError::unsupported_flat(
                "clocked equation ownership proof",
                "clocked expression has no exact connected clock owner",
                span,
            ));
        }
    }
    Ok(equation_owners)
}

/// MLS §16.8.1: a clocked partition whose equations contain `der()` is a
/// discretized continuous-time partition, integrated between ticks by its
/// clock's `solverMethod`. Reading its derivative as an ordinary continuous
/// equation would integrate the state continuously and silently replace the
/// discretization, so such a partition is refused until it is discretized.
fn reject_discretized_partition(residual: &Expression) -> Result<(), ToDaeError> {
    let Some(span) = derivative_span(residual) else {
        return Ok(());
    };
    Err(ToDaeError::unsupported_flat(
        "discretized clocked partition",
        "an equation of a clocked partition contains der(); MLS §16.8.1 discretizes such a \
         partition with its solverMethod, which is not supported",
        span,
    ))
}

fn derivative_span(expression: &Expression) -> Option<Span> {
    if let Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        span,
        ..
    } = expression
    {
        return Some(*span);
    }
    expression_children(expression)
        .into_iter()
        .find_map(derivative_span)
}

fn required_clock_owner_span(
    expression: &Expression,
    flat: &flat::Model,
    sampled_targets: &HashMap<InstanceId, SampledTarget>,
) -> Option<Span> {
    match expression {
        Expression::BuiltinCall {
            function:
                BuiltinFunction::Previous | BuiltinFunction::Interval | BuiltinFunction::FirstTick,
            span,
            ..
        } => Some(*span),
        Expression::Binary { lhs, .. } => {
            if let Expression::VarRef {
                name, subscripts, ..
            } = lhs.as_ref()
                && subscripts.is_empty()
                && let Some(variable) = flat.variables.get(name.var_name())
                && let Some(target) = sampled_targets.get(&variable.instance_id)
            {
                return Some(target.span);
            }
            expression_children(expression)
                .into_iter()
                .find_map(|child| required_clock_owner_span(child, flat, sampled_targets))
        }
        _ => expression_children(expression)
            .into_iter()
            .find_map(|child| required_clock_owner_span(child, flat, sampled_targets)),
    }
}

fn assign_value_owners(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    ordinals: &HashMap<InstanceId, usize>,
    domains: &mut DisjointDomains,
    owners: &HashMap<usize, (ClockPlan, Span)>,
    occurrences: &[Option<Span>],
    sampled_targets: &HashMap<InstanceId, SampledTarget>,
) -> Result<HashMap<InstanceId, ClockedValuePlan>, ToDaeError> {
    let mut value_owners = HashMap::new();
    for (name, variable) in &flat.variables {
        let instance = variable.instance_id;
        let Some(&ordinal) = ordinals.get(&instance) else {
            continue;
        };
        if !roles.get(name).is_some_and(|role| {
            matches!(role, PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
        }) {
            continue;
        }
        let Some((clock, owner_span)) = owners.get(&domains.find(ordinal)).copied() else {
            continue;
        };
        let ownership_span = sampled_targets
            .get(&instance)
            .map(|target| target.span)
            .or(occurrences[ordinal])
            .ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "clocked value ownership proof",
                    format!(
                        "clocked coordinate `{name}` has an owner but no exact source occurrence"
                    ),
                    owner_span,
                )
            })?;
        require_span(
            ownership_span,
            format!("clock ownership occurrence for `{name}`"),
        )?;
        value_owners.insert(
            instance,
            ClockedValuePlan {
                clock,
                ownership_span,
                sampled: sampled_targets.contains_key(&instance),
            },
        );
    }
    for (name, variable) in &flat.variables {
        let Some(target) = sampled_targets.get(&variable.instance_id) else {
            continue;
        };
        if !value_owners.contains_key(&variable.instance_id) {
            return Err(ToDaeError::unsupported_flat(
                "clocked sample ownership proof",
                format!("sampled coordinate `{name}` has no exact connected clock owner"),
                target.span,
            ));
        }
    }
    Ok(value_owners)
}

struct DisjointDomains {
    parent: Vec<usize>,
}

impl DisjointDomains {
    fn new(len: usize) -> Self {
        Self {
            parent: (0..len).collect(),
        }
    }

    fn find(&mut self, member: usize) -> usize {
        let mut root = member;
        while self.parent[root] != root {
            root = self.parent[root];
        }
        let mut cursor = member;
        while self.parent[cursor] != cursor {
            let next = self.parent[cursor];
            self.parent[cursor] = root;
            cursor = next;
        }
        root
    }

    fn union(&mut self, lhs: usize, rhs: usize) {
        let lhs = self.find(lhs);
        let rhs = self.find(rhs);
        if lhs != rhs {
            self.parent[rhs] = lhs;
        }
    }
}

type SampledValueTarget<'flat> = (&'flat flat::Variable, Span, Option<&'flat flat::Variable>);

fn sampled_value_target<'flat>(
    expression: &Expression,
    flat: &'flat flat::Model,
    clocks: &ClockCoordinates<'flat>,
) -> Option<SampledValueTarget<'flat>> {
    let (lhs, rhs) = subtraction_operands(expression)?;
    let Expression::VarRef {
        name, subscripts, ..
    } = lhs
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let (_, clock) = value_sample_operands(rhs, clocks)?;
    let span = rhs.span()?;
    flat.variables
        .get(name.var_name())
        .map(|variable| (variable, span, clock))
}

fn expression_mentions_value_sample(
    expression: &Expression,
    clocks: &ClockCoordinates<'_>,
) -> bool {
    value_sample_operands(expression, clocks).is_some()
        || expression_children(expression)
            .into_iter()
            .any(|child| expression_mentions_value_sample(child, clocks))
}

fn subtraction_operands(expression: &Expression) -> Option<(&Expression, &Expression)> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = expression
    else {
        return None;
    };
    Some((lhs, rhs))
}

fn whole_clock_reference<'flat>(
    expression: &Expression,
    clocks: &ClockCoordinates<'flat>,
) -> Option<&'flat flat::Variable> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return None;
    };
    subscripts
        .is_empty()
        .then(|| clocks.resolve(name.var_name()))
        .flatten()
}

/// One element of a clock array named by literal one-based subscripts on
/// every dimension: the array's instance index and the row-major offset.
fn clock_element_reference(
    expression: &Expression,
    clocks: &ClockCoordinates<'_>,
) -> Option<(u32, u32)> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return None;
    };
    let variable = clocks.resolve(name.var_name())?;
    if variable.dims.is_empty() || variable.dims.len() != subscripts.len() {
        return None;
    }
    let mut offset = 0_u32;
    for (subscript, extent) in subscripts.iter().zip(&variable.dims) {
        let rumoca_core::Subscript::Index { value, .. } = subscript else {
            return None;
        };
        let extent = u32::try_from(*extent).ok()?;
        let index = u32::try_from(*value).ok()?.checked_sub(1)?;
        if index >= extent {
            return None;
        }
        offset = offset.checked_mul(extent)?.checked_add(index)?;
    }
    Some((variable.instance_id.index(), offset))
}

fn periodic_constructor(
    expression: &Expression,
    constants: &EvalContext,
) -> Result<Option<ClockPlan>, ToDaeError> {
    let Expression::BuiltinCall {
        function: BuiltinFunction::Clock,
        args,
        span,
    } = expression
    else {
        return Ok(None);
    };
    let lattice = match args.as_slice() {
        // MLS §16.3 `Clock(interval)`: a period given in seconds.
        [interval] => {
            let seconds = evaluate_clock_seconds(interval, constants, "Clock interval", *span)?;
            ClockRational::from_seconds(seconds)
                .and_then(|period| ClockLattice::new(period, ClockRational::ZERO))
        }
        // MLS §16.3 `Clock(intervalCounter, resolution)`: the exact rational
        // period `intervalCounter / resolution` seconds, which is the only form
        // that keeps sub-millisecond periods free of binary rounding.
        [interval_counter, resolution] => ClockLattice::from_interval_counter(
            clock_integer(interval_counter, constants, "Clock", *span)?,
            clock_integer(resolution, constants, "Clock", *span)?,
        ),
        _ => {
            return Err(ToDaeError::unsupported_runtime_operator(
                "Clock",
                "the canonical clock proof requires `Clock(interval)` or `Clock(intervalCounter, resolution)`",
                *span,
            ));
        }
    }
    .map_err(|error| ToDaeError::unsupported_runtime_operator("Clock", error.to_string(), *span))?;
    Ok(Some(ClockPlan::periodic(lattice, *span)))
}

/// MLS §16.3 Operator 16.4 `Clock(condition, startInterval)`: an event clock,
/// recognized by its Boolean first argument. The condition must name a
/// Boolean coordinate, whose rising `pre` edge ticks the clock.
fn event_constructor(
    flat: &flat::Model,
    expression: &Expression,
) -> Result<Option<EventClockPlan>, ToDaeError> {
    let Expression::BuiltinCall {
        function: BuiltinFunction::Clock,
        args,
        span,
    } = expression
    else {
        return Ok(None);
    };
    let (condition, start_interval) = match args.as_slice() {
        [condition] => (condition, None),
        [condition, start_interval] => (condition, Some(start_interval.clone())),
        _ => return Ok(None),
    };
    if let Expression::VarRef {
        name, subscripts, ..
    } = condition
        && let Some(variable) = flat.variables.get(name.var_name())
        && is_boolean_variable(flat, variable)
    {
        if !subscripts.is_empty() {
            return Err(event_clock_condition(*span));
        }
        return Ok(Some(EventClockPlan {
            condition: name.var_name().clone(),
            start_interval,
            span: *span,
        }));
    }
    if is_boolean_expression(condition) {
        return Err(event_clock_condition(*span));
    }
    Ok(None)
}

fn is_boolean_variable(flat: &flat::Model, variable: &flat::Variable) -> bool {
    flat.effective_types
        .get(&variable.type_id)
        .is_some_and(|effective| effective.canonical_type() == flat.predefined_types.boolean)
}

/// A Boolean-valued expression form that is not a coordinate reference.
fn is_boolean_expression(expression: &Expression) -> bool {
    match expression {
        Expression::Binary { op, .. } => {
            op.is_relational() || matches!(op, OpBinary::And | OpBinary::Or)
        }
        Expression::Unary { op, .. } => matches!(op, rumoca_core::OpUnary::Not),
        Expression::Literal {
            value: rumoca_core::Literal::Boolean(_),
            ..
        } => true,
        _ => false,
    }
}

fn event_clock_condition(span: Span) -> ToDaeError {
    ToDaeError::unsupported_runtime_operator(
        "Clock",
        "an event clock condition must name a scalar Boolean variable",
        span,
    )
}

/// An event clock has no lattice: its only exact conversion is a
/// `shiftSample` by whole ticks (MLS §16.5.2).
pub(in crate::construction) fn event_clock_conversion(operator: &str, span: Span) -> ToDaeError {
    ToDaeError::unsupported_runtime_operator(
        operator,
        "an event clock has no periodic lattice; only shiftSample by whole ticks converts it",
        span,
    )
}

/// MLS §16.7: clock partitioning is a static property of the model, so an
/// `if`-equation that defines a `Clock` coordinate must be decided by the
/// model's parameter values. Fold such an equation to the branch those values
/// select; a condition that is not parameter-evaluable has no static schedule.
fn static_clock_branch<'expression>(
    residual: &'expression Expression,
    constants: &EvalContext,
    clocks: &ClockCoordinates<'_>,
) -> Result<&'expression Expression, ToDaeError> {
    let mut current = residual;
    while let Expression::If {
        branches,
        else_branch,
        span,
    } = current
    {
        if !expression_mentions_clock(current, clocks) {
            return Ok(current);
        }
        current = statically_selected_branch(branches, else_branch, constants).ok_or_else(|| {
            ToDaeError::unresolved_clock_schedule(
                "clock equation",
                "an `if` equation that defines a Clock coordinate needs parameter-evaluable Boolean conditions",
                *span,
            )
        })?;
    }
    Ok(current)
}

fn insert_plan(
    plans: &mut HashMap<InstanceId, ClockPlan>,
    target: &flat::Variable,
    plan: ClockPlan,
    span: Span,
) -> Result<(), ToDaeError> {
    if let Some(existing) = plans.get(&target.instance_id)
        && existing.schedule != plan.schedule
    {
        return Err(ToDaeError::unresolved_clock_schedule(
            target.name.as_str(),
            "conflicting constructors bind this clock coordinate",
            span,
        ));
    }
    plans.insert(target.instance_id, plan);
    Ok(())
}

/// Close the clock-coordinate plans over the model's own definitions.
///
/// Equation order carries no meaning, so a derived clock (`c = subSample(base,
/// 2)`) and an alias (`c = y`) both have to wait for their source to acquire a
/// plan. Both are replayed until nothing new resolves; a coordinate that never
/// acquires a plan is reported by the caller against its own declaration.
fn resolve_clock_definitions(
    plans: &mut HashMap<InstanceId, ClockPlan>,
    derived: &[(&flat::Variable, &Expression, Span)],
    aliases: &[(&flat::Variable, &flat::Variable, Span)],
    constants: &EvalContext,
    clocks: &ClockCoordinates<'_>,
) -> Result<(), ToDaeError> {
    loop {
        let mut progress = false;
        for (target, expression, span) in derived {
            if plans.contains_key(&target.instance_id) {
                continue;
            }
            let Some(plan) = bound_clock_plan(expression, constants, clocks, plans)? else {
                continue;
            };
            insert_plan(plans, target, plan, *span)?;
            progress = true;
        }
        progress |= propagate_aliases(plans, aliases)?;
        if !progress {
            return Ok(());
        }
    }
}

fn propagate_aliases(
    plans: &mut HashMap<InstanceId, ClockPlan>,
    aliases: &[(&flat::Variable, &flat::Variable, Span)],
) -> Result<bool, ToDaeError> {
    let mut resolved = false;
    loop {
        let mut progress = false;
        for (lhs, rhs, span) in aliases {
            match (
                plans.get(&lhs.instance_id).copied(),
                plans.get(&rhs.instance_id).copied(),
            ) {
                (Some(lhs_plan), Some(rhs_plan)) if lhs_plan.schedule != rhs_plan.schedule => {
                    return Err(ToDaeError::unresolved_clock_schedule(
                        format!("{} = {}", lhs.name, rhs.name),
                        "this clock alias joins conflicting constructors",
                        *span,
                    ));
                }
                (Some(plan), None) => {
                    plans.insert(rhs.instance_id, plan);
                    progress = true;
                }
                (None, Some(plan)) => {
                    plans.insert(lhs.instance_id, plan);
                    progress = true;
                }
                (Some(_), Some(_)) | (None, None) => {}
            }
        }
        if !progress {
            return Ok(resolved);
        }
        resolved = true;
    }
}

fn expression_mentions_clock(expression: &Expression, clocks: &ClockCoordinates<'_>) -> bool {
    // MLS §16.3 `sample(u, c)` names its clock as an operand of a *value*
    // sample, not as a clock definition. That occurrence is proven separately by
    // `validate_clocked_value_samples`, so it must not make the surrounding
    // equation a clock equation.
    if let Some((value, _)) = value_sample_operands(expression, clocks) {
        return expression_mentions_clock(value, clocks);
    }
    matches!(
        expression,
        Expression::VarRef { name, .. } if clocks.contains(name.var_name())
    ) || matches!(
        expression,
        Expression::BuiltinCall {
            function: BuiltinFunction::Clock,
            ..
        }
    ) || expression_children(expression)
        .into_iter()
        .any(|child| expression_mentions_clock(child, clocks))
}

/// Split an MLS §16.3 value sample into `(sampled value, named clock)`.
///
/// The one-operand form leaves the clock to §16.5.1 inference; the two-operand
/// form names it, and only a whole `Clock` coordinate can be that operand —
/// `sample(start, interval)` (MLS §3.7.5) keeps its Real second operand and is
/// deliberately not matched here.
fn value_sample_operands<'expression, 'flat>(
    expression: &'expression Expression,
    clocks: &ClockCoordinates<'flat>,
) -> Option<(&'expression Expression, Option<&'flat flat::Variable>)> {
    let Expression::BuiltinCall {
        function: BuiltinFunction::Sample,
        args,
        ..
    } = expression
    else {
        return None;
    };
    match args.as_slice() {
        [value] => Some((value, None)),
        [value, clock] => whole_clock_reference(clock, clocks).map(|clock| (value, Some(clock))),
        _ => None,
    }
}

fn unsupported_clock_equation(equation: &flat::Equation) -> ToDaeError {
    ToDaeError::unresolved_clock_schedule(
        "clock equation",
        "a clock equation must be an exact whole-coordinate constructor or alias",
        equation.span,
    )
}
