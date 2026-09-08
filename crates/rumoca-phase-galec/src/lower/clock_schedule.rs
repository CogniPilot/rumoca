//! Exact fixed-clock scheduling inside one GALEC base-period `DoStep`.

use super::*;
use crate::admissibility::AdmittedClock;

use super::clocked_assignments::{
    ClockedAssignment, ClockedAssignments, ClockedCallPlan, lower_clocked_assignments_for_domain,
};

pub(super) struct ScheduledClockAssignments {
    pub(super) statements: Vec<gast::Spanned<gast::Statement>>,
    pub(super) locals: Vec<gast::VariableDeclaration>,
    pub(super) called_user_functions: HashSet<u32>,
    pub(super) call_actions: Vec<PreparedCallActions>,
}

struct ClockDomain<'dae> {
    clock: dae::ClockId<'dae>,
    divisor: u32,
    span: Span,
    counter: Option<gast::Name>,
    has_assignments: bool,
    assignments: ClockedAssignments,
}

struct ScheduledAssignment<'dae> {
    clock: dae::ClockId<'dae>,
    divisor: u32,
    assignment: ClockedAssignment,
}

struct CallScheduleFacts {
    dependencies: HashMap<EmissionRegion, HashSet<EmissionRegion>>,
    argument_reads: HashMap<EmissionRegion, HashSet<u32>>,
}

impl CallScheduleFacts {
    fn derive<'a>(actions: impl Iterator<Item = &'a PreparedCallActions>) -> Self {
        let mut dependencies = HashMap::new();
        let mut argument_reads = HashMap::new();
        for action in actions {
            dependencies
                .entry(action.region())
                .or_insert_with(HashSet::new)
                .extend(action.dependencies().iter().copied());
            argument_reads
                .entry(action.region())
                .or_insert_with(HashSet::new)
                .extend(action.argument_reads());
        }
        Self {
            dependencies,
            argument_reads,
        }
    }
}

pub(super) fn lower_clock_schedule<'dae>(
    lowering: BlockLowering<'_, 'dae>,
    schedule: &AdmittedClock,
    classified: &[ClassifiedVariable<'dae>],
    declarations: &mut ProtectedDeclarations<'_>,
    retained_calls: &mut RetainedCallResults,
) -> Result<ScheduledClockAssignments, GalecTargetError> {
    let BlockLowering {
        view,
        by_id,
        pre_names,
        ..
    } = lowering;
    let admitted_clocks = schedule
        .domains
        .iter()
        .map(|domain| domain.clock_index)
        .collect::<HashSet<_>>();
    let unclocked_owner_index = schedule
        .domains
        .iter()
        .filter(|domain| domain.divisor == 1)
        .map(|domain| domain.clock_index)
        .min()
        .expect("an admitted schedule has a base-period clock");
    let unclocked_owner = view
        .clock_id(usize::try_from(unclocked_owner_index).expect("clock index fits usize"))
        .expect("admitted base-period clock resolves");
    let call_plan = ClockedCallPlan::construct(lowering, &admitted_clocks, unclocked_owner)?;
    let mut domains = lower_clock_domains(lowering, schedule, &call_plan, retained_calls)?;
    let call_facts = CallScheduleFacts::derive(
        domains
            .iter()
            .flat_map(|domain| domain.assignments.call_actions.iter()),
    );
    let ordered = order_clocked_assignments(take_pending(&mut domains), by_id, &call_facts);
    let ordered = ordered?;
    let mut locals = Vec::new();
    let mut called_user_functions = HashSet::new();
    let mut call_actions = Vec::new();
    let mut generated_names = HashSet::new();
    for domain in &mut domains {
        locals.append(&mut domain.assignments.locals);
        called_user_functions.extend(std::mem::take(
            &mut domain.assignments.called_user_functions,
        ));
        call_actions.append(&mut domain.assignments.call_actions);
        if domain.divisor > 1 && domain.has_assignments {
            domain.counter = Some(append_divider_state(
                domain.clock,
                domain.divisor,
                domain.span,
                classified,
                pre_names,
                &mut generated_names,
                declarations,
            )?);
        }
    }
    let counters = domains
        .iter()
        .filter_map(|domain| {
            domain
                .counter
                .clone()
                .map(|counter| (domain.clock.index(), counter))
        })
        .collect::<HashMap<_, _>>();
    let mut statements = Vec::new();
    for scheduled in ordered {
        match counters.get(&scheduled.clock.index()) {
            Some(counter) => statements.push(guarded_domain(
                counter.clone(),
                scheduled.assignment.statements,
                scheduled.assignment.span,
            )),
            None => statements.extend(scheduled.assignment.statements),
        }
    }
    statements.extend(domains.into_iter().filter_map(|domain| {
        domain
            .counter
            .map(|counter| advance_counter(counter, domain.divisor, domain.span))
    }));
    Ok(ScheduledClockAssignments {
        statements,
        locals,
        called_user_functions,
        call_actions,
    })
}

fn lower_clock_domains<'refs, 'dae>(
    lowering: BlockLowering<'refs, 'dae>,
    schedule: &AdmittedClock,
    call_plan: &ClockedCallPlan<'refs, 'dae>,
    retained_calls: &mut RetainedCallResults,
) -> Result<Vec<ClockDomain<'dae>>, GalecTargetError> {
    let view = lowering.view;
    let mut domains = Vec::with_capacity(schedule.domains.len());
    for domain in &schedule.domains {
        let clock = view
            .clock_id(usize::try_from(domain.clock_index).expect("clock index fits usize"))
            .expect("admissibility retained a checked clock index");
        let clock_view = view.clock(clock).expect("admitted clock resolves");
        let assignments = lower_clocked_assignments_for_domain(
            lowering,
            clock,
            call_plan,
            &mut *retained_calls,
            domain.divisor == 1,
        )?;
        domains.push(ClockDomain {
            clock,
            divisor: domain.divisor,
            span: clock_view.provenance().span(),
            counter: None,
            has_assignments: !assignments.assignments.is_empty(),
            assignments,
        });
    }
    Ok(domains)
}

fn take_pending<'dae>(domains: &mut [ClockDomain<'dae>]) -> Vec<ScheduledAssignment<'dae>> {
    domains
        .iter_mut()
        .flat_map(|domain| {
            std::mem::take(&mut domain.assignments.assignments)
                .into_iter()
                .map(|assignment| ScheduledAssignment {
                    clock: domain.clock,
                    divisor: domain.divisor,
                    assignment,
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

fn order_clocked_assignments<'dae>(
    mut source: Vec<ScheduledAssignment<'dae>>,
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    call_facts: &CallScheduleFacts,
) -> Result<Vec<ScheduledAssignment<'dae>>, GalecTargetError> {
    source.sort_by_key(|assignment| {
        (
            std::cmp::Reverse(assignment.divisor),
            assignment.clock.index(),
        )
    });
    for scheduled in &mut source {
        for region in &scheduled.assignment.regions {
            if let Some(reads) = call_facts.argument_reads.get(region) {
                scheduled.assignment.reads.extend(reads.iter().copied());
            }
        }
    }
    let mut owners = HashMap::new();
    let mut region_owners = HashMap::new();
    for (index, scheduled) in source.iter().enumerate() {
        for target in &scheduled.assignment.targets {
            if let Some(previous) = owners.insert(*target, index)
                && previous != index
            {
                return Err(unsupported(
                    "multiple-clocked-owners",
                    format!("variable #{target} is assigned by more than one clocked atomic owner"),
                    scheduled.assignment.span,
                ));
            }
        }
        for region in &scheduled.assignment.regions {
            if let Some(previous) = region_owners.insert(*region, index)
                && previous != index
            {
                return Err(GalecTargetError::LoweringInternal {
                    detail: "one emission region escaped into two scheduled assignments".to_owned(),
                });
            }
        }
    }
    let mut emitted = vec![false; source.len()];
    let mut order = Vec::with_capacity(source.len());
    while let Some(index) = source.iter().enumerate().position(|(index, scheduled)| {
        assignment_is_ready(
            index,
            scheduled,
            &emitted,
            &owners,
            &region_owners,
            call_facts,
        )
    }) {
        emitted[index] = true;
        order.push(index);
    }
    if order.len() != source.len() {
        return Err(clock_domain_cycle(&source, &owners, &emitted, by_id));
    }
    let mut source = source.into_iter().map(Some).collect::<Vec<_>>();
    Ok(order
        .into_iter()
        .map(|index| {
            source[index]
                .take()
                .expect("domain is emitted exactly once")
        })
        .collect())
}

fn assignment_is_ready(
    index: usize,
    scheduled: &ScheduledAssignment<'_>,
    emitted: &[bool],
    owners: &HashMap<u32, usize>,
    region_owners: &HashMap<EmissionRegion, usize>,
    call_facts: &CallScheduleFacts,
) -> bool {
    !emitted[index]
        && scheduled.assignment.reads.iter().all(|read| {
            owners
                .get(read)
                .is_none_or(|dependency| *dependency == index || emitted[*dependency])
        })
        && scheduled.assignment.regions.iter().all(|region| {
            region_dependencies_are_ready(region, index, emitted, region_owners, call_facts)
        })
}

fn region_dependencies_are_ready(
    region: &EmissionRegion,
    index: usize,
    emitted: &[bool],
    region_owners: &HashMap<EmissionRegion, usize>,
    call_facts: &CallScheduleFacts,
) -> bool {
    call_facts
        .dependencies
        .get(region)
        .is_none_or(|dependencies| {
            dependencies.iter().all(|dependency| {
                dependency_owner_is_ready(dependency, index, emitted, region_owners)
            })
        })
}

fn dependency_owner_is_ready(
    dependency: &EmissionRegion,
    index: usize,
    emitted: &[bool],
    region_owners: &HashMap<EmissionRegion, usize>,
) -> bool {
    region_owners
        .get(dependency)
        .is_some_and(|owner| *owner == index || emitted[*owner])
}

/// Report the first assignment that no order can reach, naming the owners that
/// block it.
///
/// Every remaining assignment belongs to the same blocked component, so the
/// first one is a faithful witness; the named reads are the edges that could
/// not be satisfied.
fn clock_domain_cycle<'dae>(
    source: &[ScheduledAssignment<'dae>],
    owners: &HashMap<u32, usize>,
    emitted: &[bool],
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
) -> GalecTargetError {
    let (index, blocked) = source
        .iter()
        .enumerate()
        .find(|(index, _)| !emitted[*index])
        .expect("unfinished schedule has an unemitted assignment");
    let name = |variable: &u32| {
        by_id.get(variable).map_or_else(
            || format!("#{variable}"),
            |classified| format!("`{}`", classified.variable.name()),
        )
    };
    let mut targets = blocked
        .assignment
        .targets
        .iter()
        .map(name)
        .collect::<Vec<_>>();
    targets.sort();
    let mut dependencies = blocked
        .assignment
        .reads
        .iter()
        .filter_map(|read| owners.get(read).map(|owner| (*read, *owner)))
        .filter(|(_, owner)| !emitted[*owner] && *owner != index)
        .map(|(read, owner)| format!("{} on clock #{}", name(&read), source[owner].clock.index()))
        .collect::<Vec<_>>();
    dependencies.sort();
    dependencies.dedup();
    let blamed = if targets.is_empty() {
        "this clock domain's shared call preamble".to_owned()
    } else {
        targets.join(", ")
    };
    unsupported(
        "clock-domain-cycle",
        format!(
            "the atomic assignment of {blamed} on commensurate clock #{} is blocked by \
             coincident-tick reads of {}, whose own owners are blocked in turn; the remaining \
             assignment graph is cyclic",
            blocked.clock.index(),
            dependencies.join(", ")
        ),
        blocked.assignment.span,
    )
}

fn append_divider_state<'dae>(
    clock: dae::ClockId<'dae>,
    divisor: u32,
    span: Span,
    classified: &[ClassifiedVariable<'dae>],
    pre_names: &HashMap<u32, gast::Name>,
    generated_names: &mut HashSet<String>,
    declarations: &mut ProtectedDeclarations<'_>,
) -> Result<gast::Name, GalecTargetError> {
    let mut suffix = 0_u32;
    let name = loop {
        let candidate = format!("clockDivider{}_{suffix}", clock.index());
        suffix += 1;
        let occupied = classified
            .iter()
            .any(|variable| crate::mangle::name_lexeme(&variable.name) == candidate)
            || pre_names
                .values()
                .any(|name| crate::mangle::name_lexeme(name) == candidate)
            || generated_names.contains(&candidate)
            || rumoca_ir_galec::builtins::is_reserved_name(&candidate);
        if !occupied {
            generated_names.insert(candidate.clone());
            break with_span(crate::mangle::galec_variable_name(&candidate)?, span);
        }
    };
    let declaration = gast::VariableDeclaration {
        ty: gast::TypeRef::Primitive(gast::ScalarType::Integer),
        name: name.clone(),
        dimensions: Vec::new(),
        range: gast::RangeAttributes {
            min: Some(gast::Expression::Integer(0)),
            max: Some(gast::Expression::Integer(i64::from(divisor - 1))),
        },
        span,
    };
    declarations.protected.push(gast::ProtectedEntity {
        kind: gast::ProtectedKind::State,
        decl: declaration,
        start: Some(gast::Expression::Integer(0)),
    });
    declarations.startup.push(gast::Spanned::new(
        gast::Statement::Assignment {
            target: state_reference(name.clone(), span),
            value: gast::Expression::Integer(0),
        },
        span,
    ));
    declarations.nominals.push(None);
    Ok(name)
}

fn guarded_domain(
    counter: gast::Name,
    body: Vec<gast::Spanned<gast::Statement>>,
    span: Span,
) -> gast::Spanned<gast::Statement> {
    let condition = gast::Expression::binary(
        gast::BinaryOp::Eq,
        gast::Expression::Ref(state_reference(counter, span)),
        gast::Expression::Integer(0),
    );
    gast::Spanned::new(
        gast::Statement::If(gast::IfStatement {
            branches: vec![gast::IfBranch {
                condition: gast::Condition::Expression(condition),
                body,
                span,
            }],
            else_body: None,
        }),
        span,
    )
}

fn advance_counter(
    counter: gast::Name,
    divisor: u32,
    span: Span,
) -> gast::Spanned<gast::Statement> {
    let reference = || gast::Expression::Ref(state_reference(counter.clone(), span));
    let condition = gast::Expression::binary(
        gast::BinaryOp::Eq,
        reference(),
        gast::Expression::Integer(i64::from(divisor - 1)),
    );
    let increment = gast::Expression::binary(
        gast::BinaryOp::Add,
        reference(),
        gast::Expression::Integer(1),
    );
    let value = gast::Expression::If(gast::IfExpression::new(
        vec![(condition, gast::Expression::Integer(0))],
        increment,
    ));
    gast::Spanned::new(
        gast::Statement::Assignment {
            target: state_reference(counter, span),
            value,
        },
        span,
    )
}
