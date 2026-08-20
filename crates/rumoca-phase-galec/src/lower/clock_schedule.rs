//! Exact fixed-clock scheduling inside one GALEC base-period `DoStep`.

use super::*;
use crate::admissibility::AdmittedClock;

use super::clocked_assignments::{
    ClockedAssignment, ClockedAssignments, lower_clocked_assignments_for_domain,
};

pub(super) struct ScheduledClockAssignments {
    pub(super) statements: Vec<gast::Spanned<gast::Statement>>,
    pub(super) locals: Vec<gast::VariableDeclaration>,
    pub(super) called_user_functions: HashSet<u32>,
}

struct ClockDomain<'dae> {
    clock: dae::ClockId<'dae>,
    divisor: u32,
    span: Span,
    counter: Option<gast::Name>,
    assignments: ClockedAssignments,
}

struct ScheduledAssignment<'dae> {
    clock: dae::ClockId<'dae>,
    divisor: u32,
    counter: Option<gast::Name>,
    assignment: ClockedAssignment,
}

// SPEC_0021: Exception - validated boundary keeps proof-relevant inputs explicit.
#[allow(clippy::too_many_arguments)]
pub(super) fn lower_clock_schedule<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
    schedule: &AdmittedClock,
    classified: &[ClassifiedVariable<'dae>],
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
    nominals: &mut Vec<Option<f64>>,
    protected: &mut Vec<gast::ProtectedEntity>,
    startup: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<ScheduledClockAssignments, GalecTargetError> {
    let unclocked_owner = schedule
        .domains
        .iter()
        .filter(|domain| domain.divisor == 1)
        .map(|domain| domain.clock_index)
        .min()
        .expect("an admitted schedule has a base-period clock");
    let mut domains = schedule
        .domains
        .iter()
        .map(|domain| {
            let clock = view
                .clock_id(usize::try_from(domain.clock_index).expect("clock index fits usize"))
                .expect("admissibility retained a checked clock index");
            let clock_view = view.clock(clock).expect("admitted clock resolves");
            let assignments = lower_clocked_assignments_for_domain(
                view,
                definitions,
                clock,
                by_id,
                pre_names,
                domain.clock_index == unclocked_owner,
            )?;
            Ok(ClockDomain {
                clock,
                divisor: domain.divisor,
                span: clock_view.provenance().span(),
                counter: None,
                assignments,
            })
        })
        .collect::<Result<Vec<_>, GalecTargetError>>()?;
    let mut locals = Vec::new();
    let mut called_user_functions = HashSet::new();
    let mut generated_names = HashSet::new();
    for domain in &mut domains {
        locals.append(&mut domain.assignments.locals);
        called_user_functions.extend(std::mem::take(
            &mut domain.assignments.called_user_functions,
        ));
        if domain.divisor > 1 && !domain.assignments.assignments.is_empty() {
            domain.counter = Some(append_divider_state(
                domain.clock,
                domain.divisor,
                domain.span,
                classified,
                pre_names,
                &mut generated_names,
                nominals,
                protected,
                startup,
            )?);
        }
    }
    let pending = domains
        .iter_mut()
        .flat_map(|domain| {
            std::mem::take(&mut domain.assignments.assignments)
                .into_iter()
                .map(|assignment| ScheduledAssignment {
                    clock: domain.clock,
                    divisor: domain.divisor,
                    counter: domain.counter.clone(),
                    assignment,
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let mut statements = Vec::new();
    for scheduled in order_clocked_assignments(pending, by_id)? {
        match scheduled.counter {
            Some(counter) => statements.push(guarded_domain(
                counter,
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
    })
}

fn order_clocked_assignments<'dae>(
    mut source: Vec<ScheduledAssignment<'dae>>,
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
) -> Result<Vec<ScheduledAssignment<'dae>>, GalecTargetError> {
    source.sort_by_key(|assignment| {
        (
            std::cmp::Reverse(assignment.divisor),
            assignment.clock.index(),
        )
    });
    let mut owners = HashMap::new();
    let preambles = source
        .iter()
        .enumerate()
        .filter_map(|(index, scheduled)| {
            scheduled
                .assignment
                .is_preamble
                .then_some((scheduled.clock.index(), index))
        })
        .collect::<HashMap<_, _>>();
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
    }
    // Every owner target is a discrete Real or discrete-value coordinate, and a
    // clock-domain preamble is admitted only when its causally expanded reads
    // name none of those, so a preamble never carries an incoming edge and is
    // always emittable first. The barrier below is therefore an ordering
    // constraint, never a cycle: only the read edges can leave work unemitted.
    let mut emitted = vec![false; source.len()];
    let mut order = Vec::with_capacity(source.len());
    while let Some(index) = source.iter().enumerate().position(|(index, scheduled)| {
        !emitted[index]
            && (!scheduled.assignment.requires_preamble
                || preambles
                    .get(&scheduled.clock.index())
                    .is_none_or(|preamble| emitted[*preamble]))
            && scheduled.assignment.reads.iter().all(|read| {
                owners
                    .get(read)
                    .is_none_or(|dependency| *dependency == index || emitted[*dependency])
            })
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

// SPEC_0021: Exception - validated boundary keeps proof-relevant inputs explicit.
#[allow(clippy::too_many_arguments)]
fn append_divider_state<'dae>(
    clock: dae::ClockId<'dae>,
    divisor: u32,
    span: Span,
    classified: &[ClassifiedVariable<'dae>],
    pre_names: &HashMap<u32, gast::Name>,
    generated_names: &mut HashSet<String>,
    nominals: &mut Vec<Option<f64>>,
    protected: &mut Vec<gast::ProtectedEntity>,
    startup: &mut Vec<gast::Spanned<gast::Statement>>,
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
    protected.push(gast::ProtectedEntity {
        kind: gast::ProtectedKind::State,
        decl: declaration,
        start: Some(gast::Expression::Integer(0)),
    });
    startup.push(gast::Spanned::new(
        gast::Statement::Assignment {
            target: state_reference(name.clone(), span),
            value: gast::Expression::Integer(0),
        },
        span,
    ));
    nominals.push(None);
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
